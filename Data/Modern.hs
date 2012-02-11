{-# LANGUAGE Rank2Types, StandaloneDeriving, DeriveDataTypeable,
             MultiParamTypeClasses, FunctionalDependencies #-}
module Data.Modern
  (ModernType(..),
   ModernData(..),
   ModernHash,
   ModernBitpath,
   ModernTypeName,
   ModernFieldName,
   ModernContext,
   ModernFailure(..),
   textualSchema,
   fromString,
   initialContext,
   computeTypeHash,
   runModernDeserializationFromByteString,
   runModernDeserializationFromFile,
   runModernSerializationToByteString,
   runModernSerializationToFile,
   serializeData,
   deserializeSchema,
   ensureTypeInContext)
  where

import BinaryFiles hiding (getContext)
import Control.Monad.State.Strict
import Data.Array
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.Digest.Murmur3 as Hash
import Data.Int
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.String
import Data.Typeable
import Data.Word
import Prelude hiding (read)


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
  | ModernTupleType [ModernType]
  | ModernUnionType [ModernType]
  | ModernStructureType [(ModernFieldName, ModernType)]
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
  | ModernDataList (Array Int ModernData)
  | ModernDataTuple [ModernData]
  | ModernDataStructure [(ModernFieldName, ModernData)]
  | ModernDataNamed ModernTypeName ModernData


data ModernCommandType
  = ModernCommandTypeListType
  | ModernCommandTypeTupleType
  | ModernCommandTypeUnionType
  | ModernCommandTypeStructureType
  | ModernCommandTypeNamedType
  deriving (Eq, Ord)


data ModernCommand
  = ModernCommandListType ModernHash
  | ModernCommandTupleType [ModernHash]
  | ModernCommandUnionType [ModernHash]
  | ModernCommandStructureType [(ModernFieldName, ModernHash)]
  | ModernCommandNamedType ModernTypeName ModernHash


data ModernHash
  = ModernHash ByteString
  deriving (Eq, Ord)


data ModernBitpath
  = ModernBitpath Word8 Word64


data Bit = Zero | One


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
      modernContextTypes :: Map ModernHash ModernType
    }


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
      visitType depth (ModernTupleType contentTypes) =
        "Tuple " ++ block depth (map (visitType (depth + 1)) contentTypes)
      visitType depth (ModernUnionType contentTypes) =
        "Union "
        ++ block depth
                 (map (\contentType ->
                         visitType (depth + 1) contentType)
                      contentTypes)
      visitType depth (ModernStructureType fields) =
        "Structure "
        ++ block depth
                 (map (\(fieldName, contentType) ->
                          visitFieldName fieldName
                          ++ " "
                          ++ visitType (depth + 1) contentType)
                      fields)
      visitType depth (ModernNamedType typeName contentType) =
        "Named "
        ++ visitTypeName typeName ++ " "
        ++ visitType depth contentType
        ++ ";"
      visitTypeName (ModernTypeName name) =
        UTF8.toString name
      visitFieldName (ModernFieldName name) =
        UTF8.toString name
      visitBitpath (ModernBitpath count source) =
        let single i =
              case shiftR source (fromIntegral $ count - i - 1) .&. 0x1 of
                0 -> "0"
                1 -> "1"
            loop soFar i =
              if i == count
                then soFar
                else loop (soFar ++ (single i)) (i + 1)
        in loop "" 0
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
    [(0x03, ModernCommandTypeListType),
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
      modernContextTypes = initialTypes
    }


nullBitpath :: ModernBitpath
nullBitpath = ModernBitpath 0 0


bitpathToList :: ModernBitpath -> [Bit]
bitpathToList (ModernBitpath count source) =
  unfoldr (\i ->
             if fromIntegral i == count
               then Nothing
               else Just (case fromIntegral $ shiftR source i .&. 1 of
                            0 -> Zero
                            _ -> One,
                          i + 1))
          0


bitpathAppend :: ModernBitpath -> Bit -> Maybe ModernBitpath
bitpathAppend (ModernBitpath count source) bit =
  if count == 64
    then Nothing
    else let bit' = case bit of
                      Zero -> 0
                      One -> 1
             shiftedBit = shiftL bit' (fromIntegral count)
             source' = source .|. shiftedBit
             count' = count + 1
         in Just $ ModernBitpath count' source'


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
          ModernTupleType contentTypes ->
            BS.concat [UTF8.fromString "Tuple(",
                       BS.intercalate
                        (UTF8.fromString ",")
                        $ map typeHelper contentTypes,
                       UTF8.fromString ")"]
          ModernUnionType possibilities ->
            BS.concat [UTF8.fromString "Union(",
                       BS.intercalate
                        (UTF8.fromString ",")
                        $ map typeHelper possibilities,
                       UTF8.fromString ")"]
          ModernStructureType fields ->
            BS.concat [UTF8.fromString "Structure(",
                       BS.intercalate
                        (UTF8.fromString ",")
                        $ map (\(ModernFieldName fieldName, fieldType) ->
                                  BS.concat [fieldName,
                                             BS.pack [0x00],
                                             typeHelper fieldType])
                              fields,
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
           runStateT (modernSerializationAction action) context of
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
        runStateT (modernSerializationAction action) context
  return $ case eitherFailureResult of
             Left failure -> Left failure
             Right (result, context') -> Right (context', result)


serializeData :: [ModernData] -> ModernSerialization ()
serializeData items = do
  mapM_ ensureTypeInContext $ map dataType items


deserializeSchema :: ModernDeserialization [ModernType]
deserializeSchema = do
  a <- deserializeOneCommand
  b <- deserializeOneCommand
  c <- deserializeOneCommand
  d <- deserializeOneCommand
  e <- deserializeOneCommand
  return [a, b, c, d, e]


deserializeOneCommand :: ModernDeserialization ModernType
deserializeOneCommand = do
  context <- getContext
  let knownTypes = modernContextTypes context
  maybeCommandType <- inputCommandType
  case maybeCommandType of
    Nothing -> return undefined -- TODO
    Just ModernCommandTypeListType -> do
      contentTypeHash <- inputDataHash
      let contentType =
            case Map.lookup (ModernHash contentTypeHash) knownTypes of
              Nothing -> undefined -- TODO
              Just knownType -> knownType
          theType = ModernListType contentType
      learnType theType
      return theType
    Just ModernCommandTypeTupleType -> do
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
      let theType = ModernTupleType items
      learnType theType
      return theType
    Just ModernCommandTypeUnionType -> do
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
      let theType = ModernUnionType items
      learnType theType
      return theType
    Just ModernCommandTypeStructureType -> do
      nFields <- inputDataWord64
      let loop soFar i = do
            if i == nFields
              then return soFar
              else do
                fieldName <- inputDataUTF8
                fieldTypeHash <- inputDataHash
                let fieldType =
                      case Map.lookup (ModernHash fieldTypeHash) knownTypes of
                        Nothing -> undefined -- TODO
                        Just knownType -> knownType
                loop (soFar ++ [(ModernFieldName fieldName, fieldType)])
                     (i + 1)
      fields <- loop [] 0
      let theType = ModernStructureType fields
      learnType theType
      return theType
    Just ModernCommandTypeNamedType -> do
      typeName <- inputDataUTF8
      contentTypeHash <- inputDataHash
      let contentType =
            case Map.lookup (ModernHash contentTypeHash) knownTypes of
              Nothing -> undefined -- TODO
              Just knownType -> knownType
          theType = ModernNamedType (ModernTypeName typeName) contentType
      learnType theType
      return theType


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
dataType (ModernDataList theArray) =
  ModernListType $ dataType $ theArray ! (fst $ bounds theArray)
dataType (ModernDataTuple theValues) =
  ModernTupleType $ map dataType theValues
dataType (ModernDataStructure fields) =
  ModernStructureType $ map (\(fieldName, fieldData) ->
                               (fieldName, dataType fieldData))
                            fields
dataType (ModernDataNamed typeName theValue) =
  ModernNamedType typeName $ dataType theValue


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
        ModernTupleType contentTypes -> do
          mapM ensureTypeInContext contentTypes
          let contentTypeHashes = map computeTypeHash contentTypes
          commandTupleType contentTypeHashes
        ModernUnionType contentTypes -> do
          mapM ensureTypeInContext contentTypes
          let contentTypeHashes = map computeTypeHash contentTypes
          commandUnionType contentTypeHashes
        ModernStructureType fields -> do
          mapM ensureTypeInContext $ map snd fields
          let fieldTypeHashes =
                map (\(fieldName, fieldType) ->
                        (fieldName, computeTypeHash fieldType))
                    fields
          commandStructureType fieldTypeHashes
        ModernNamedType name contentType -> do
          ensureTypeInContext contentType
          let contentTypeHash = computeTypeHash contentType
          commandNamedType name contentTypeHash


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
  outputDataWord64 $ genericLength contentTypeHashes
  mapM_ (\(ModernHash contentTypeHash) -> do
           outputData contentTypeHash)
        contentTypeHashes


commandUnionType
  :: [ModernHash]
  -> ModernSerialization ()
commandUnionType possibilities = do
  outputCommandType ModernCommandTypeUnionType
  outputDataWord64 $ genericLength possibilities
  mapM_ (\(ModernHash possibility) -> outputData possibility)
        possibilities


commandStructureType
  :: [(ModernFieldName, ModernHash)]
  -> ModernSerialization ()
commandStructureType fields = do
  outputCommandType ModernCommandTypeStructureType
  outputDataWord64 $ genericLength fields
  mapM_ (\(ModernFieldName fieldName, ModernHash fieldTypeHash) -> do
           outputDataUTF8 fieldName
           outputData fieldTypeHash)
        fields


commandNamedType
  :: ModernTypeName
  -> ModernHash
  -> ModernSerialization ()
commandNamedType (ModernTypeName name) (ModernHash contentTypeHash) = do
  outputCommandType ModernCommandTypeNamedType
  outputDataUTF8 name
  outputData contentTypeHash


inputCommandBits
  :: Word8
  -> Map Word64 object
  -> ModernDeserialization (Maybe object)
inputCommandBits count decodings = MakeModernDeserialization $ do
  source <- lift $ deserializeWord
    :: StateT ModernContext (ContextualDeserialization Endianness) Word64
  let lowBits = source .&. (shiftL 1 (fromIntegral count) - 1)
  return $ Map.lookup lowBits decodings


inputCommandType
  :: ModernDeserialization (Maybe ModernCommandType)
inputCommandType = do
  inputCommandBits standardCommandEncodingBitsize standardCommandDecodings


inputDataHash
  :: ModernDeserialization ByteString
inputDataHash = MakeModernDeserialization $ do
  lift $ read 16


inputDataWord64
  :: ModernDeserialization Word64
inputDataWord64 = MakeModernDeserialization $ do
  lift $ deserializeWord


inputDataUTF8
  :: ModernDeserialization ByteString
inputDataUTF8 = MakeModernDeserialization $ do
  let loop soFar = do
        next <- lift $ read 8
        if 0x00 == (BS.head $ BS.drop 7 next)
          then do
            let validNext =
                  (BS.pack . reverse . dropWhile (== 0x00) . reverse
                   . BS.unpack) next
            return $ BS.concat [soFar, validNext]
          else loop $ BS.concat [soFar, next]
  loop BS.empty


outputCommandBits
  :: Word8
  -> Word64
  -> ModernSerialization ()
outputCommandBits count source = MakeModernSerialization $ do
  lift $ serializeWord source


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
outputData byteString = MakeModernSerialization $ do
  lift $ write byteString


outputDataWord64
  :: Word64
  -> ModernSerialization ()
outputDataWord64 word64 = MakeModernSerialization $ do
  lift $ serializeWord word64


outputDataUTF8
  :: ByteString
  -> ModernSerialization ()
outputDataUTF8 byteString = do
  let payloadLength = BS.length byteString
      prospectivePadLength = 8 - mod payloadLength 8
      padLength = if prospectivePadLength == 0
                    then 8
                    else prospectivePadLength
  outputData byteString
  outputData $ BS.pack $ take padLength $ repeat 0x00

