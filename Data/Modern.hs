{-# LANGUAGE Rank2Types, StandaloneDeriving, DeriveDataTypeable #-}
module Data.Modern
  (ModernType(..),
   ModernData(..),
   ModernHash,
   ModernTypeName,
   ModernFieldName,
   ModernContext,
   ModernFailure(..),
   textualSchema,
   textualData,
   fromString,
   initialContext,
   computeTypeHash,
   runModernDeserializationFromByteString,
   runModernDeserializationFromFile,
   runModernSerializationToByteString,
   runModernSerializationToFile,
   serializeData,
   deserializeData,
   ensureTypeInContext,
   outputSynchronize)
  where

import BinaryFiles hiding (getContext)
import Control.Monad.State.Strict
import Data.Array
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import Data.Char
import qualified Data.Digest.Murmur3 as Hash
import Data.Int
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.String
import Data.Typeable
import Data.Word
import Numeric
import Prelude hiding (read)

import Debug.Trace


class (Monad m)
      => ModernMonad m
      where
  getContext :: m ModernContext
  putContext :: ModernContext -> m ()


data ModernDeserialization a =
  MakeModernDeserialization {
      modernDeserializationAction
        :: StateT ModernContext (ContextualDeserialization Endianness) a
    }
instance Monad ModernDeserialization where
  return a = MakeModernDeserialization $ return a
  x >>= f =
    MakeModernDeserialization $ do
      v <- modernDeserializationAction x
      modernDeserializationAction $ f v
instance ModernMonad ModernDeserialization where
  getContext = MakeModernDeserialization $ get
  putContext context = MakeModernDeserialization $ put context


data ModernSerialization a =
  MakeModernSerialization {
      modernSerializationAction
        :: StateT ModernContext (ContextualSerialization Endianness) a
    }
instance Monad ModernSerialization where
  return a = MakeModernSerialization $ return a
  x >>= f =
    MakeModernSerialization $ do
      v <- modernSerializationAction x
      modernSerializationAction $ f v
instance ModernMonad ModernSerialization where
  getContext = MakeModernSerialization $ get
  putContext context = MakeModernSerialization $ put context


data ModernType
  = ModernInt8Type
  | ModernInt16Type
  | ModernInt32Type
  | ModernInt64Type
  | ModernWord8Type
  | ModernWord16Type
  | ModernWord32Type
  | ModernWord64Type
  | ModernFloatType
  | ModernDoubleType
  | ModernUTF8Type
  | ModernBlobType
  | ModernListType ModernType
  | ModernTupleType (Maybe (Array Word64 ModernType))
  | ModernUnionType (Maybe (Word8, (Array Word64 ModernType)))
  | ModernStructureType (Maybe (Array Word64 (ModernFieldName, ModernType)))
  | ModernNamedType ModernTypeName ModernType
  deriving (Eq, Show)


data ModernData
  = ModernDataInt8 Int8
  | ModernDataInt16 Int16
  | ModernDataInt32 Int32
  | ModernDataInt64 Int64
  | ModernDataWord8 Word8
  | ModernDataWord16 Word16
  | ModernDataWord32 Word32
  | ModernDataWord64 Word64
  | ModernDataFloat Float
  | ModernDataDouble Double
  | ModernDataUTF8 ByteString
  | ModernDataBlob ByteString
  | ModernDataList ModernType (Array Int ModernData)
  | ModernDataTuple ModernType (Array Int ModernData)
  | ModernDataUnion ModernType Word64 ModernData
  | ModernDataStructure ModernType (Array Int ModernData)
  | ModernDataNamed ModernType ModernData


data ModernCommandType
  = ModernCommandTypeSynchronize
  | ModernCommandTypeDatum
  | ModernCommandTypeListType
  | ModernCommandTypeTupleType
  | ModernCommandTypeUnionType
  | ModernCommandTypeStructureType
  | ModernCommandTypeNamedType
  deriving (Eq, Ord)


data ModernHash
  = ModernHash ByteString
  deriving (Eq, Ord)


data ModernTypeName
  = ModernTypeName ByteString
  deriving (Eq, Ord)
instance IsString ModernTypeName where
  fromString = ModernTypeName . UTF8.fromString
instance Show ModernTypeName where
  show (ModernTypeName byteString) = show $ UTF8.toString byteString


data ModernFieldName
  = ModernFieldName ByteString
  deriving (Eq, Ord)
instance IsString ModernFieldName where
  fromString = ModernFieldName . UTF8.fromString
instance Show ModernFieldName where
  show (ModernFieldName byteString) = show $ UTF8.toString byteString


data ModernContext =
  ModernContext {
      modernContextTypes :: Map ModernHash ModernType,
      modernContextPendingCommandBitCount :: Word8,
      modernContextPendingCommandBitSource :: Word64,
      modernContextPendingCommandWords :: [Word64],
      modernContextPendingData :: [PendingData],
      modernContextStartingOffset :: Word64
    }


data PendingData
  = PendingBytes ByteString
  | PendingAlignmentMark Word64


data ModernFailure
  = ModernFailure String
  deriving (Typeable)
instance Show ModernFailure where
  show (ModernFailure string) = string
instance SerializationFailure ModernFailure


textualSchema :: [ModernType] -> String
textualSchema theTypes =
  let visitType _ ModernInt8Type = "Int8;"
      visitType _ ModernInt16Type = "Int16;"
      visitType _ ModernInt32Type = "Int32;"
      visitType _ ModernInt64Type = "Int64;"
      visitType _ ModernWord8Type = "Word8;"
      visitType _ ModernWord16Type = "Word16;"
      visitType _ ModernWord32Type = "Word32;"
      visitType _ ModernWord64Type = "Word64;"
      visitType _ ModernFloatType = "Float;"
      visitType _ ModernDoubleType = "Double;"
      visitType _ ModernUTF8Type = "UTF8;"
      visitType _ ModernBlobType = "Blob;"
      visitType depth (ModernListType contentType) =
        "List " ++ block depth [visitType (depth + 1) contentType]
      visitType depth (ModernTupleType maybeContentTypes) =
        "Tuple " ++ block depth (map (visitType (depth + 1))
                                     (case maybeContentTypes of
                                        Nothing -> []
                                        Just contentTypes ->
                                          elems contentTypes))
      visitType depth (ModernUnionType maybeContentTypes) =
        "Union "
        ++ block depth
                 (map (\contentType ->
                         visitType (depth + 1) contentType)
                      (case maybeContentTypes of
                         Nothing -> []
                         Just (_, contentTypes) -> elems contentTypes))
      visitType depth (ModernStructureType maybeFields) =
        "Structure "
        ++ block depth
                 (map (\(fieldName, contentType) ->
                          visitFieldName fieldName
                          ++ " "
                          ++ visitType (depth + 1) contentType)
                      (case maybeFields of
                         Nothing -> []
                         Just fields -> elems fields))
      visitType depth (ModernNamedType typeName contentType) =
        "Named "
        ++ visitTypeName typeName ++ " "
        ++ visitType depth contentType
        ++ ";"
      visitTypeName (ModernTypeName name) =
        UTF8.toString name
      visitFieldName (ModernFieldName name) =
        UTF8.toString name
      block _ [onlyItem] = "{ " ++ onlyItem ++ " }"
      block depth items =
        "{\n"
        ++ (concat
             $ map (\item ->
                      take ((depth + 1) * 2) (repeat ' ')
                      ++ item
                      ++ "\n")
                   items)
        ++ take (depth * 2) (repeat ' ')
        ++ "}"
  in intercalate "\n" $ map (visitType 0) theTypes


textualData :: [ModernData] -> String
textualData theData =
  let visitDatum (ModernDataInt8 value) = "int8:" ++ (showHexBytes 1 value)
      visitDatum (ModernDataInt16 value) = "int16:" ++ (showHexBytes 2 value)
      visitDatum (ModernDataInt32 value) = "int32:" ++ (showHexBytes 4 value)
      visitDatum (ModernDataInt64 value) = "int64:" ++ (showHexBytes 8 value)
      visitDatum (ModernDataWord8 value) = "word8:" ++ (showHexBytes 1 value) 
      visitDatum (ModernDataWord16 value) = "word16:" ++ (showHexBytes 2 value)
      visitDatum (ModernDataWord32 value) = "word32:" ++ (showHexBytes 4 value)
      visitDatum (ModernDataWord64 value) = "word64:" ++ (showHexBytes 8 value)
      visitDatum (ModernDataFloat value) = "float:" ++ (show value) -- TODO
      visitDatum (ModernDataDouble value) = "double:" ++ (show value) -- TODO
      visitDatum (ModernDataUTF8 value) = "utf8:" ++ (showString value)
      visitDatum (ModernDataBlob value) =
        "blob:" ++ (concat $ map (showHexBytes 1) (BS.unpack value))
      visitDatum (ModernDataList _ items) =
        "list:{"
        ++ (concat $ map (\item -> visitDatum item ++ ";") (elems items))
        ++ "}"
      visitDatum (ModernDataTuple _ items) =
        "tuple:{"
        ++ (concat $ map (\item -> visitDatum item ++ ";") (elems items))
        ++ "}"
      visitDatum (ModernDataUnion _ which datum) =
        "union:" ++ (show which) ++ "{" ++ visitDatum datum ++ "}"
      visitDatum (ModernDataStructure theType values) =
        "struct:{"
        ++ (concat $ map (\(name, value) ->
                            showString name ++ "=" ++ visitDatum value ++ ";")
                         (zip (case theType of
                                 ModernStructureType Nothing -> []
                                 ModernStructureType (Just fields) ->
                                   map (\(ModernFieldName bs, _) -> bs)
                                       (elems fields)
                                 _ -> repeat $ error "Unknown name.")
                              (elems values)))
        ++ "}"
      visitDatum (ModernDataNamed theType datum) =
        let bs = case theType of
                     ModernNamedType (ModernTypeName bs) _ -> bs
                     _ -> error "Unknown name."
        in "named:" ++ (showString bs) ++ "={" ++ visitDatum datum ++ "}"
      showString bs = "'" ++ (concat $ map (\c -> case c of
                                                    '\'' -> "''"
                                                    _ -> [c])
                                           $ UTF8.toString bs) ++ "'"
      showHexBytes w b =
        let h = map toUpper $ showHex b ""
            p = take (w * 2 - length h) (repeat '0')
        in p ++ h
  in concat $ map (\datum -> visitDatum datum ++ ";") theData


initialTypes :: Map ModernHash ModernType
initialTypes =
  Map.fromList
   $ map (\theType -> (computeTypeHash theType, theType))
         [ModernInt8Type,
          ModernInt16Type,
          ModernInt32Type,
          ModernInt64Type,
          ModernWord8Type,
          ModernWord16Type,
          ModernWord32Type,
          ModernWord64Type,
          ModernFloatType,
          ModernDoubleType,
          ModernUTF8Type,
          ModernBlobType]


standardCommandEncodingBitsize :: Word8
standardCommandEncodingBitsize = 3


standardCommandDecodings :: Map Word64 ModernCommandType
standardCommandDecodings =
  Map.fromList
    [(0x00, ModernCommandTypeSynchronize),
     (0x01, ModernCommandTypeDatum),
     (0x03, ModernCommandTypeListType),
     (0x04, ModernCommandTypeTupleType),
     (0x05, ModernCommandTypeUnionType),
     (0x06, ModernCommandTypeStructureType),
     (0x07, ModernCommandTypeNamedType)]


standardCommandEncodings :: Map ModernCommandType Word64
standardCommandEncodings =
  Map.fromList
   $ map (\(key, value) -> (value, key))
         $ Map.toList standardCommandDecodings


initialContext :: ModernContext
initialContext =
  ModernContext {
      modernContextTypes = initialTypes,
      modernContextPendingCommandBitCount = 0,
      modernContextPendingCommandBitSource = 0,
      modernContextPendingCommandWords = [],
      modernContextPendingData = [],
      modernContextStartingOffset = 0
    }


computeTypeHash :: ModernType -> ModernHash
computeTypeHash modernType =
  ModernHash
   $ Hash.asByteString
      $ Hash.hash
         $ computeTypeByteString modernType


computeTypeByteString :: ModernType -> ByteString
computeTypeByteString modernType =
  let typeHelper modernType =
        case modernType of
          ModernInt8Type -> UTF8.fromString "Int8"
          ModernInt16Type -> UTF8.fromString "Int16"
          ModernInt32Type -> UTF8.fromString "Int32"
          ModernInt64Type -> UTF8.fromString "Int64"
          ModernWord8Type -> UTF8.fromString "Word8"
          ModernWord16Type -> UTF8.fromString "Word16"
          ModernWord32Type -> UTF8.fromString "Word32"
          ModernWord64Type -> UTF8.fromString "Word64"
          ModernFloatType -> UTF8.fromString "Float"
          ModernDoubleType -> UTF8.fromString "Double"
          ModernUTF8Type -> UTF8.fromString "UTF8"
          ModernBlobType -> UTF8.fromString "Blob"
          ModernListType contentType ->
            BS.concat [UTF8.fromString "List(",
                       typeHelper contentType,
                       UTF8.fromString ")"]
          ModernTupleType maybeContentTypes ->
            BS.concat [UTF8.fromString "Tuple(",
                       BS.intercalate
                        (UTF8.fromString ",")
                        (case maybeContentTypes of
                          Nothing -> []
                          Just contentTypes ->
                            map typeHelper $ elems contentTypes),
                       UTF8.fromString ")"]
          ModernUnionType maybePossibilities ->
            BS.concat [UTF8.fromString "Union(",
                       BS.intercalate
                        (UTF8.fromString ",")
                        (case maybePossibilities of
                           Nothing -> []
                           Just (_, possibilities) ->
                             map typeHelper $ elems possibilities),
                       UTF8.fromString ")"]
          ModernStructureType maybeFields ->
            BS.concat [UTF8.fromString "Structure(",
                       BS.intercalate
                        (UTF8.fromString ",")
                        $ map (\(ModernFieldName fieldName, fieldType) ->
                                  BS.concat [fieldName,
                                             BS.pack [0x00],
                                             typeHelper fieldType])
                              (case maybeFields of
                                 Nothing -> []
                                 Just fields -> elems fields),
                       UTF8.fromString ")"]
          ModernNamedType (ModernTypeName name) contentType ->
            BS.concat [UTF8.fromString "Named(",
                       name,
                       BS.pack [0x00],
                       typeHelper contentType,
                       UTF8.fromString ")"]
  in BS.concat [UTF8.fromString "Type(",
                typeHelper modernType,
                UTF8.fromString ")"]


runModernDeserializationFromByteString
  :: ModernContext
  -> ByteString
  -> (ModernDeserialization a)
  -> Either (Int, [(Int, String)], SomeSerializationFailure)
            (ModernContext, a)
runModernDeserializationFromByteString context input action =
  runDeserializationFromByteString
    (do
      withContext LittleEndian $ do
        (result, context') <-
          runStateT (modernDeserializationAction action) context
        return (context', result))
    input


runModernDeserializationFromFile
  :: ModernContext
  -> FilePath
  -> (ModernDeserialization a)
  -> IO (Either (Int, [(Int, String)], SomeSerializationFailure)
                (ModernContext, a))
runModernDeserializationFromFile context filePath action = do
  runDeserializationFromFile
    (do
      withContext LittleEndian $ do
        (result, context') <-
          runStateT (modernDeserializationAction action) context
        return (context', result))
    filePath


runModernSerializationToByteString
  :: ModernContext
  -> (ModernSerialization a)
  -> Either (Int, [(Int, String)], SomeSerializationFailure)
            (ByteString, ModernContext, a)
runModernSerializationToByteString context action =
  case runSerializationToByteString $ do
         withContext LittleEndian $ do
           runStateT (modernSerializationAction
                       $ wrapModernSerialization action)
                     context of
    Left failure -> Left failure
    Right ((result, context'), output) -> Right (output, context', result)


runModernSerializationToFile
  :: ModernContext
  -> FilePath
  -> (ModernSerialization a)
  -> IO (Either (Int, [(Int, String)], SomeSerializationFailure)
                (ModernContext, a))
runModernSerializationToFile context filePath action = do
  eitherFailureResult <-
    flip runSerializationToFile filePath $ do
      withContext LittleEndian $ do
        runStateT (modernSerializationAction
                    $ wrapModernSerialization action) context
  return $ case eitherFailureResult of
             Left failure -> Left failure
             Right (result, context') -> Right (context', result)


serializeData :: [ModernData] -> ModernSerialization ()
serializeData items = do
  mapM_ ensureTypeInContext $ map dataType items
  mapM_ commandDatum items


deserializeData :: ModernDeserialization ([ModernType], [ModernData])
deserializeData = do
  let loop soFar = do
        maybeCommandType <- inputCommandType
        case maybeCommandType of
          Nothing -> return soFar
          Just ModernCommandTypeSynchronize -> return soFar
          Just commandType@ModernCommandTypeDatum
            -> return $ soFar ++ [commandType]
          Just commandType -> loop $ soFar ++ [commandType]
  commandTypes <- loop []
  maybeDatas <- mapM deserializeOneCommand commandTypes
  context <- getContext
  return (Map.elems $ modernContextTypes context,
          catMaybes maybeDatas)


deserializeOneCommand
  :: ModernCommandType
  -> ModernDeserialization (Maybe ModernData)
deserializeOneCommand commandType = do
  context <- getContext
  let knownTypes = modernContextTypes context
  case commandType of
    ModernCommandTypeDatum -> do
      typeHash <- inputDataHash
      case Map.lookup (ModernHash typeHash) knownTypes of
        Nothing -> undefined -- TODO
        Just theType -> do
          datum <- deserializeOneDatum theType
          return $ Just datum
    ModernCommandTypeListType -> do
      contentTypeHash <- inputDataHash
      let contentType =
            case Map.lookup (ModernHash contentTypeHash) knownTypes of
              Nothing -> undefined -- TODO
              Just knownType -> knownType
          theType = ModernListType contentType
      learnType theType
      return Nothing
    ModernCommandTypeTupleType -> do
      nItems <- inputDataWord64
      let loop soFar i = do
            if i == nItems
              then return soFar
              else do
                itemTypeHash <- inputDataHash
                let itemType =
                      case Map.lookup (ModernHash itemTypeHash) knownTypes of
                        Nothing -> undefined -- TODO
                        Just knownType -> knownType
                loop (soFar ++ [itemType]) (i + 1)
      items <- loop [] 0
      if nItems == 0
        then learnType $ ModernTupleType Nothing
        else do
          let itemKeyValuePairs = zip [0 .. nItems - 1] items
              itemBounds = (0, nItems - 1)
              itemArray = array itemBounds itemKeyValuePairs
              theType = ModernTupleType $ Just itemArray
          learnType theType
      return Nothing
    ModernCommandTypeUnionType -> do
      nItems <- inputDataWord64
      let loop soFar i = do
            if i == nItems
              then return soFar
              else do
                itemTypeHash <- inputDataHash
                let itemType =
                      case Map.lookup (ModernHash itemTypeHash) knownTypes of
                        Nothing -> undefined -- TODO
                        Just knownType -> knownType
                loop (soFar ++ [itemType]) (i + 1)
      items <- loop [] 0
      if nItems == 0
        then learnType $ ModernUnionType Nothing
        else do
          let itemKeyValuePairs = zip [0 .. nItems - 1] items
              itemBounds = (0, nItems - 1)
              itemArray = array itemBounds itemKeyValuePairs
              nBits = ceiling $ logBase 2 (fromIntegral nItems :: Double)
              theType = ModernUnionType $ Just (nBits, itemArray)
          learnType theType
      return Nothing
    ModernCommandTypeStructureType -> do
      nFields <- inputDataWord64
      let loop soFar i = do
            if i == nFields
              then return soFar
              else do
                inputAlign 8
                fieldName <- inputDataUTF8
                inputAlign 8
                fieldTypeHash <- inputDataHash
                let fieldType =
                      case Map.lookup (ModernHash fieldTypeHash) knownTypes of
                        Nothing -> undefined -- TODO
                        Just knownType -> knownType
                loop (soFar ++ [(ModernFieldName fieldName, fieldType)])
                     (i + 1)
      fields <- loop [] 0
      if nFields == 0
        then learnType $ ModernStructureType Nothing
        else do
          let fieldKeyValuePairs = zip [0 .. nFields - 1] fields
              fieldBounds = (0, nFields - 1)
              fieldArray = array fieldBounds fieldKeyValuePairs
              theType = ModernStructureType $ Just fieldArray
          learnType theType
      return Nothing
    ModernCommandTypeNamedType -> do
      inputAlign 8
      typeName <- inputDataUTF8
      inputAlign 8
      contentTypeHash <- inputDataHash
      let contentType =
            case Map.lookup (ModernHash contentTypeHash) knownTypes of
              Nothing -> undefined -- TODO
              Just knownType -> knownType
          theType = ModernNamedType (ModernTypeName typeName) contentType
      learnType theType
      return Nothing


deserializeOneDatum
  :: ModernType
  -> ModernDeserialization ModernData
deserializeOneDatum theType = do
  return $ ModernDataInt8 42
  {-
  case theType of
    ModernInt8Type -> do
      inputAlign 1
      undefined -- TODO
    ModernInt16Type -> do
      inputAlign 2
      undefined -- TODO
    ModernInt32Type -> do
      inputAlign 4
      undefined -- TODO
    ModernInt64Type -> do
      inputAlign 8
      undefined -- TODO
    ModernWord8Type -> do
      inputAlign 1
      undefined -- TODO
    ModernWord16Type -> do
      inputAlign 2
      undefined -- TODO
    ModernWord32Type -> do
      inputAlign 4
      undefined -- TODO
    ModernWord64Type -> do
      inputAlign 8
      undefined -- TODO
    ModernFloatType -> do
      inputAlign 4
      undefined -- TODO
    ModernDoubleType -> do
      inputAlign 8
      undefined -- TODO
    ModernUTF8Type -> do
      inputAlign 8
      result <- undefined -- TODO
      inputAlign 8
      return result
    ModernBlobType -> do
      inputAlign 8
      result <- undefined -- TODO
      inputAlign 8
      return result
    ModernListType contentType -> do
      undefined -- TODO
    ModernTupleType maybeContentTypes -> do
      undefined -- TODO
    ModernUnionType maybeContentTypes -> do
      undefined -- TODO
    ModernStructureType maybeContentTypes -> do
      undefined -- TODO
    ModernNamedType _ contentType -> do
      undefined -- TODO
      -}


dataType :: ModernData -> ModernType
dataType (ModernDataInt8 _) = ModernInt8Type
dataType (ModernDataInt16 _) = ModernInt16Type
dataType (ModernDataInt32 _) = ModernInt32Type
dataType (ModernDataInt64 _) = ModernInt64Type
dataType (ModernDataWord8 _) = ModernWord8Type
dataType (ModernDataWord16 _) = ModernWord16Type
dataType (ModernDataWord32 _) = ModernWord32Type
dataType (ModernDataWord64 _) = ModernWord64Type
dataType (ModernDataFloat _) = ModernFloatType
dataType (ModernDataDouble _) = ModernDoubleType
dataType (ModernDataUTF8 _) = ModernUTF8Type
dataType (ModernDataBlob _) = ModernBlobType
dataType (ModernDataList theType _) = theType
dataType (ModernDataTuple theType _) = theType
dataType (ModernDataUnion theType _ _) = theType
dataType (ModernDataStructure theType _) = theType
dataType (ModernDataNamed theType theValue) = theType


learnType
  :: ModernType
  -> ModernDeserialization ()
learnType theType = do
  oldContext <- getContext
  let oldKnownTypes = modernContextTypes oldContext
      theHash = computeTypeHash theType
      newKnownTypes = Map.insert theHash theType oldKnownTypes
      newContext = oldContext {
                       modernContextTypes = newKnownTypes
                     }
  putContext newContext


ensureTypeInContext
  :: ModernType
  -> ModernSerialization ()
ensureTypeInContext theType = do
  context <- getContext
  let hash = computeTypeHash theType
      knownTypes = modernContextTypes context
  if Map.member hash knownTypes
    then return ()
    else do
      let newKnownTypes = Map.insert hash theType knownTypes
          newContext = context {
                           modernContextTypes = newKnownTypes
                         }
      putContext newContext
      case theType of
        ModernListType contentType -> do
          ensureTypeInContext contentType
          let contentTypeHash = computeTypeHash contentType
          commandListType contentTypeHash
        ModernTupleType maybeContentTypes -> do
          contentTypeHashes <-
            case maybeContentTypes of
              Nothing -> return []
              Just contentTypes -> do
                mapM_ ensureTypeInContext $ elems contentTypes
                return $ map computeTypeHash $ elems contentTypes
          commandTupleType contentTypeHashes
        ModernUnionType maybeContentTypes -> do
          contentTypeHashes <-
            case maybeContentTypes of
              Nothing -> return []
              Just (_, contentTypes) -> do
                mapM_ ensureTypeInContext $ elems contentTypes
                return $ map computeTypeHash $ elems contentTypes
          commandUnionType contentTypeHashes
        ModernStructureType maybeFields -> do
          fieldTypeHashes <-
            case maybeFields of
              Nothing -> return []
              Just fields -> do
                mapM_ ensureTypeInContext $ map snd $ elems fields
                return $ map (\(fieldName, fieldType) ->
                                 (fieldName, computeTypeHash fieldType))
                             $ elems fields
          commandStructureType fieldTypeHashes
        ModernNamedType name contentType -> do
          ensureTypeInContext contentType
          let contentTypeHash = computeTypeHash contentType
          commandNamedType name contentTypeHash


commandSynchronize
  :: ModernSerialization ()
commandSynchronize = do
  outputCommandType ModernCommandTypeSynchronize
  outputSynchronize


commandDatum
  :: ModernData
  -> ModernSerialization ()
commandDatum datum = do
  let theType = dataType datum
      ModernHash theTypeHash = computeTypeHash theType
  ensureTypeInContext theType
  outputCommandType ModernCommandTypeDatum
  outputData theTypeHash
  outputSynchronize
  let helper datum = do
        case datum of
          ModernDataInt8 value -> do
            outputAlign 1
            outputDataWord (fromIntegral value :: Word8)
          ModernDataInt16 value -> do
            outputAlign 2
            outputDataWord (fromIntegral value :: Word16)
          ModernDataInt32 value -> do
            outputAlign 4
            outputDataWord (fromIntegral value :: Word32)
          ModernDataInt64 value -> do
            outputAlign 8
            outputDataWord (fromIntegral value :: Word64)
          ModernDataWord8 value -> do
            outputAlign 1
            outputDataWord value
          ModernDataWord16 value -> do
            outputAlign 2
            outputDataWord value
          ModernDataWord32 value -> do
            outputAlign 4
            outputDataWord value
          ModernDataWord64 value -> do
            outputAlign 8
            outputDataWord value
          ModernDataFloat value -> do
            outputAlign 4
            undefined
          ModernDataDouble value -> do
            outputAlign 8
            undefined
          ModernDataUTF8 value -> do
            outputAlign 8
            outputData $ BS.concat [value, BS.pack [0x00]]
            outputAlign 8
          ModernDataBlob value -> do
            outputAlign 8
            outputData value
            outputAlign 8
          ModernDataList _ values -> do
            outputAlign 8
            let (start, end) = bounds values
            outputDataWord $ end - start + 1
            mapM_ helper $ elems values
          ModernDataTuple _ values -> do
            mapM_ helper $ elems values
          ModernDataUnion theType index value -> do
            case theType of
              ModernUnionType Nothing -> do
                undefined -- TODO
              ModernUnionType (Just (bitCount, _)) -> do
                outputSynchronize
                outputCommandBits bitCount index
                helper value
              _ -> undefined -- TODO
          ModernDataStructure _ values -> do
            mapM_ helper $ elems values
          ModernDataNamed _ value -> do
            helper value
  helper datum


commandListType
  :: ModernHash
  -> ModernSerialization ()
commandListType (ModernHash contentTypeHash) = do
  outputCommandType ModernCommandTypeListType
  outputData contentTypeHash


commandTupleType
  :: [ModernHash]
  -> ModernSerialization ()
commandTupleType contentTypeHashes = do
  outputCommandType ModernCommandTypeTupleType
  outputDataWord (genericLength contentTypeHashes :: Word64)
  mapM_ (\(ModernHash contentTypeHash) -> do
           outputData contentTypeHash)
        contentTypeHashes


commandUnionType
  :: [ModernHash]
  -> ModernSerialization ()
commandUnionType possibilities = do
  outputCommandType ModernCommandTypeUnionType
  outputDataWord (genericLength possibilities :: Word64)
  mapM_ (\(ModernHash possibility) -> outputData possibility)
        possibilities


commandStructureType
  :: [(ModernFieldName, ModernHash)]
  -> ModernSerialization ()
commandStructureType fields = do
  outputCommandType ModernCommandTypeStructureType
  outputDataWord (genericLength fields :: Word64)
  mapM_ (\(ModernFieldName fieldName, ModernHash fieldTypeHash) -> do
           outputData $ BS.concat [fieldName, BS.pack [0x00]]
           outputAlign 8
           outputData fieldTypeHash)
        fields


commandNamedType
  :: ModernTypeName
  -> ModernHash
  -> ModernSerialization ()
commandNamedType (ModernTypeName name) (ModernHash contentTypeHash) = do
  outputCommandType ModernCommandTypeNamedType
  outputData $ BS.concat [name, BS.pack [0x00]]
  outputAlign 8
  outputData contentTypeHash


inputCommandBits
  :: Word8
  -> ModernDeserialization Word64
inputCommandBits inputCount = do
  oldContext <- getContext
  let oldCount = modernContextPendingCommandBitCount oldContext
      oldSource = modernContextPendingCommandBitSource oldContext
      oldStartingOffset = modernContextStartingOffset oldContext
  if inputCount <= oldCount
    then do
      let resultBits = oldSource .&. (shiftL 1 (fromIntegral inputCount) - 1)
          newCount = oldCount - inputCount
          newSource = shiftR oldSource (fromIntegral inputCount)
          newContext = oldContext {
                           modernContextPendingCommandBitCount = newCount,
                           modernContextPendingCommandBitSource = newSource
                         }
      putContext newContext
      return resultBits
    else do
      inputSource <- MakeModernDeserialization $ lift $ deserializeWord
      let newCount = oldCount + 64 - inputCount
          wrappedCount = inputCount - oldCount
          resultHighBits =
            inputSource .&. (shiftL 1 (fromIntegral wrappedCount) - 1)
          resultBits =
            oldSource .|. (shiftL resultHighBits (fromIntegral oldCount))
          newSource = shiftR inputSource (fromIntegral wrappedCount)
          newStartingOffset = oldStartingOffset + 8
          newContext = oldContext {
                           modernContextPendingCommandBitCount = newCount,
                           modernContextPendingCommandBitSource = newSource,
                           modernContextStartingOffset = newStartingOffset
                         }
      putContext newContext
      return resultBits


inputCommandType
  :: ModernDeserialization (Maybe ModernCommandType)
inputCommandType = do
  bits <- inputCommandBits standardCommandEncodingBitsize
  return $ Map.lookup bits standardCommandDecodings


inputDataHash
  :: ModernDeserialization ByteString
inputDataHash = do
  oldContext <- getContext
  let oldStartingOffset = modernContextStartingOffset oldContext
  result <- MakeModernDeserialization $ lift $ read 16
  let newStartingOffset = oldStartingOffset + 16
      newContext = oldContext {
                       modernContextStartingOffset = newStartingOffset
                     }
  putContext newContext
  return result


inputDataWord8
  :: ModernDeserialization Word8
inputDataWord8 = do
  oldContext <- getContext
  let oldStartingOffset = modernContextStartingOffset oldContext
  result <- MakeModernDeserialization $ lift $ deserializeWord
  let newStartingOffset = oldStartingOffset + 1
      newContext = oldContext {
                       modernContextStartingOffset = newStartingOffset
                     }
  putContext newContext
  return result


inputDataWord16
  :: ModernDeserialization Word16
inputDataWord16 = do
  oldContext <- getContext
  let oldStartingOffset = modernContextStartingOffset oldContext
  result <- MakeModernDeserialization $ lift $ deserializeWord
  let newStartingOffset = oldStartingOffset + 2
      newContext = oldContext {
                       modernContextStartingOffset = newStartingOffset
                     }
  putContext newContext
  return result


inputDataWord32
  :: ModernDeserialization Word32
inputDataWord32 = do
  oldContext <- getContext
  let oldStartingOffset = modernContextStartingOffset oldContext
  result <- MakeModernDeserialization $ lift $ deserializeWord
  let newStartingOffset = oldStartingOffset + 4
      newContext = oldContext {
                       modernContextStartingOffset = newStartingOffset
                     }
  putContext newContext
  return result


inputDataWord64
  :: ModernDeserialization Word64
inputDataWord64 = do
  oldContext <- getContext
  let oldStartingOffset = modernContextStartingOffset oldContext
  result <- MakeModernDeserialization $ lift $ deserializeWord
  let newStartingOffset = oldStartingOffset + 8
      newContext = oldContext {
                       modernContextStartingOffset = newStartingOffset
                     }
  putContext newContext
  return result


inputDataUTF8
  :: ModernDeserialization ByteString
inputDataUTF8 = do
  oldContext <- getContext
  let oldStartingOffset = modernContextStartingOffset oldContext
  result <- MakeModernDeserialization $ lift deserializeNullTerminatedText
  let newStartingOffset =
        oldStartingOffset + (fromIntegral $ BS.length result) + 1
      newContext = oldContext {
                       modernContextStartingOffset = newStartingOffset
                     }
  putContext newContext
  return result


inputAlign
  :: Word64
  -> ModernDeserialization ()
inputAlign alignment = do
  oldContext <- getContext
  let oldStartingOffset = modernContextStartingOffset oldContext
      misalignment = mod oldStartingOffset alignment
      padLength = if misalignment == 0
                    then 0
                    else alignment - misalignment
      newStartingOffset = oldStartingOffset + padLength
      newContext = oldContext {
                       modernContextStartingOffset = newStartingOffset
                     }
  byteString <-
    MakeModernDeserialization $ lift $ read $ fromIntegral padLength
  putContext newContext
  mapM_ (\byte ->
           if byte == 0x00
             then return ()
             else error (show byte) -- TODO
        )
        (BS.unpack byteString)


wrapModernSerialization
  :: ModernSerialization a
  -> ModernSerialization a
wrapModernSerialization action = do
  result <- action
  outputSynchronize
  return result


outputSynchronize
  :: ModernSerialization ()
outputSynchronize = do
  oldContext <- getContext
  let oldCount = modernContextPendingCommandBitCount oldContext
      oldSource = modernContextPendingCommandBitSource oldContext
      oldPendingCommandWords = modernContextPendingCommandWords oldContext
      oldPendingData = modernContextPendingData oldContext
      oldStartingOffset = modernContextStartingOffset oldContext
  mapM_ (\commandWord -> do
           MakeModernSerialization $ lift $ serializeWord commandWord)
        oldPendingCommandWords
  if oldCount > 0
    then MakeModernSerialization $ lift $ serializeWord oldSource
    else return ()
  newStartingOffset <-
    foldM (\startingOffset pendingDatum -> do
             case pendingDatum of
               PendingBytes byteString -> do
                 MakeModernSerialization $ lift $ write byteString
                 return $ startingOffset
                          + (fromIntegral $ BS.length byteString)
               PendingAlignmentMark alignment -> do
                 let misalignment = mod startingOffset alignment
                     padLength = if misalignment == 0
                                   then 0
                                   else alignment - misalignment
                 MakeModernSerialization
                  $ lift $ write $ BS.pack $ genericTake padLength $ repeat 0x00  
                 return $ startingOffset + padLength)
          oldStartingOffset
          oldPendingData
  let newContext = oldContext {
                       modernContextPendingCommandBitCount = 0,
                       modernContextPendingCommandBitSource = 0,
                       modernContextPendingCommandWords = [],
                       modernContextPendingData = [],
                       modernContextStartingOffset = newStartingOffset
                     }
  putContext newContext


outputCommandBits
  :: Word8
  -> Word64
  -> ModernSerialization ()
outputCommandBits outputCount outputSource = do
  oldContext <- getContext
  let oldCount = modernContextPendingCommandBitCount oldContext
      oldSource = modernContextPendingCommandBitSource oldContext
      combinedSources =
        oldSource .|. (shiftL outputSource $ fromIntegral oldCount)
      (newCount, newSource, immediateOutput) =
        if oldCount + outputCount >= 64
          then (oldCount + outputCount - 64,
                shiftR outputSource $ fromIntegral $ 64 - oldCount,
                Just combinedSources)
          else (oldCount + outputCount,
                combinedSources,
                Nothing)
      oldStartingOffset = modernContextStartingOffset oldContext
      newStartingOffset =
        case immediateOutput of
          Nothing -> oldStartingOffset
          Just _ -> oldStartingOffset + 8
      oldPendingCommandWords = modernContextPendingCommandWords oldContext
      newPendingCommandWords =
        case immediateOutput of
          Nothing -> oldPendingCommandWords
          Just word -> oldPendingCommandWords ++ [word]
      newContext = oldContext {
                       modernContextPendingCommandBitCount = newCount,
                       modernContextPendingCommandBitSource = newSource,
                       modernContextPendingCommandWords =
                         newPendingCommandWords,
                       modernContextStartingOffset = newStartingOffset
                     }
  putContext newContext


outputCommandType
  :: ModernCommandType
  -> ModernSerialization ()
outputCommandType commandType = do
  case Map.lookup commandType standardCommandEncodings of
    Just source -> outputCommandBits standardCommandEncodingBitsize source
    Nothing -> return ()


outputData
  :: ByteString
  -> ModernSerialization ()
outputData byteString = do
  oldContext <- getContext
  let oldPendingData = modernContextPendingData oldContext
      newPendingData = oldPendingData ++ [PendingBytes byteString]
      newContext = oldContext {
                       modernContextPendingData = newPendingData
                     }
  putContext newContext


outputDataWord
  :: (Bits word, Integral word, Num word)
  => word
  -> ModernSerialization ()
outputDataWord word = do
  let result = runSerializationToByteString $ withContext LittleEndian
                $ serializeWord word
  case result of
    Right ((), byteString) -> outputData byteString


outputAlign
  :: Word64
  -> ModernSerialization ()
outputAlign alignment = do
  oldContext <- getContext
  let oldPendingData = modernContextPendingData oldContext
      newPendingData = oldPendingData ++ [PendingAlignmentMark alignment]
      newContext = oldContext {
                       modernContextPendingData = newPendingData
                     }
  putContext newContext
