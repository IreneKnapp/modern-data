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


class (Monad m, MonadSerial underlying)
      => ModernMonad m underlying
      | m -> underlying
      where
  makeModernAction
    :: StateT ModernContext (underlying Endianness) a
    -> m a
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
instance ModernMonad ModernDeserialization ContextualDeserialization where
  makeModernAction = MakeModernDeserialization
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
instance ModernMonad ModernSerialization ContextualSerialization where
  makeModernAction = MakeModernSerialization
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
  | ModernUnionType (ModernAttachments ModernType)
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
  = ModernCommandTypePad
  | ModernCommandTypeAssume
  | ModernCommandTypeAttach
  | ModernCommandTypeSplit
  | ModernCommandTypeDetach
  | ModernCommandTypeBeginConfiguration
  | ModernCommandTypeEndConfiguration
  | ModernCommandTypeListType
  | ModernCommandTypeTupleType
  | ModernCommandTypeUnionType
  | ModernCommandTypeStructureType
  | ModernCommandTypeNamedType
  | ModernCommandTypeData ModernType
  deriving (Eq, Show)


data ModernCommand
  = ModernCommandAssume ModernHash
  | ModernCommandAttach ModernBitpath ModernHash
  | ModernCommandSplit ModernBitpath
  | ModernCommandDetach ModernBitpath
  | ModernCommandBeginConfiguration ModernTypeName
  | ModernCommandEndConfiguration
  | ModernCommandListType ModernHash
  | ModernCommandTupleType [ModernHash]
  | ModernCommandUnionType (ModernAttachments ModernHash)
  | ModernCommandStructureType [(ModernFieldName, ModernHash)]
  | ModernCommandNamedType ModernTypeName ModernHash
  | ModernCommandData ModernData


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
      modernContextTypes :: Map ModernHash ModernType,
      modernContextCommands :: ModernAttachments ModernCommandType
    }


data ModernAttachments client
  = Attached client
  | Unattached
  | Split {
        splitZero :: ModernAttachments client,
        splitOne :: ModernAttachments client
      }
deriving instance (Eq client) => Eq (ModernAttachments client)
deriving instance (Show client) => Show (ModernAttachments client)


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
      visitType depth (ModernUnionType attachments) =
	"Union "
	++ block depth
		 (map (\(bitpath, contentType) ->
			 visitBitpath bitpath ++ " "
			 ++ visitType (depth + 1) contentType)
		      (attachmentsToList attachments))
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
	++ visitType (depth + 1) contentType
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
	     $ map (\item -> take (depth * 2) (repeat ' ') ++ item ++ "\n")
	           items)
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


{-
1000 -> Assume
1001 -> Detach
1010 -> Attach
1011 -> Split
1100 -> Pad
11010 -> BeginConfiguration
11011 -> EndConfiguration
11100 -> (Unattached)
11101 -> ListType
11110 -> UnionType
11111 -> StructureType
-}
initialCommandAttachments :: ModernAttachments ModernCommandType
initialCommandAttachments =
  Split {
      splitZero = Unattached,
      splitOne =
        Split {
            splitZero =
              Split {
                  splitZero =
                    Split {
                        splitZero = Attached ModernCommandTypeAssume,
                        splitOne = Attached ModernCommandTypeDetach
                      },
                  splitOne =
                    Split {
                        splitZero = Attached ModernCommandTypeAttach,
                        splitOne = Attached ModernCommandTypeSplit
                      }
                },
            splitOne =
              Split {
                  splitZero =
                    Split {
                        splitZero =
                          Split {
                              splitZero =
                                Attached ModernCommandTypeBeginConfiguration,
                              splitOne =
                                Attached ModernCommandTypeEndConfiguration
                            },
                        splitOne =
                          Split {
                              splitZero =
                                Attached ModernCommandTypePad,
                              splitOne =
                                Attached ModernCommandTypeNamedType
                            }
                      },
                  splitOne =
                    Split {
                        splitZero =
                          Split {
                              splitZero = Attached ModernCommandTypeListType,
                              splitOne = Attached ModernCommandTypeTupleType
                            },
                        splitOne =
                          Split {
                              splitZero =
                                Attached ModernCommandTypeUnionType,
                              splitOne =
                                Attached ModernCommandTypeStructureType
                            }
                      }
                }
          }
    }


initialContext :: ModernContext
initialContext =
  ModernContext {
      modernContextTypes = initialTypes,
      modernContextCommands = initialCommandAttachments
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


attachmentsToList :: ModernAttachments client -> [(ModernBitpath, client)]
attachmentsToList attachments =
  let loop bitpath (Attached client) = [(bitpath, client)]
      loop _ Unattached = []
      loop bitpath node@(Split { }) =
        (case bitpathAppend bitpath Zero of
          Nothing -> []
          Just bitpathZero -> loop bitpathZero (splitZero node))
        ++ (case bitpathAppend bitpath One of
              Nothing -> []
              Just bitpathOne -> loop bitpathOne (splitOne node))
  in loop nullBitpath attachments


attachmentsContents :: ModernAttachments client -> [client]
attachmentsContents attachments =
  let loop (Attached client) = [client]
      loop Unattached = []
      loop node@(Split { }) =
        (loop $ splitZero node)
        ++ (loop $ splitOne node)
  in loop attachments


mapAttachments
  :: (client -> client')
  -> ModernAttachments client
  -> ModernAttachments client'
mapAttachments function attachments =
  let loop (Attached client) = Attached $ function client
      loop Unattached = Unattached
      loop node@(Split { }) =
        Split {
            splitZero = loop $ splitZero node,
            splitOne = loop $ splitOne node
          }
  in loop attachments


attachmentsLookup
  :: (Eq client)
  => client
  -> ModernAttachments client
  -> Maybe ModernBitpath
attachmentsLookup client attachments =
  let loop bitpath (Attached foundClient) =
        if client == foundClient
          then Just bitpath
          else Nothing
      loop _ Unattached = Nothing
      loop bitpath node@(Split { }) =
        let zeroResult = case bitpathAppend bitpath Zero of
                           Nothing -> Nothing
                           Just bitpathZero -> loop bitpathZero (splitZero node)
            oneResult = case bitpathAppend bitpath One of
                          Nothing -> Nothing
                          Just bitpathOne -> loop bitpathOne (splitOne node)
        in case zeroResult of
             Just _ -> zeroResult
             Nothing -> oneResult
  in loop nullBitpath attachments


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
                        $ map (\(bitpath, possibilityType) ->
                                  BS.concat [bitpathHelper bitpath,
                                             typeHelper possibilityType])
                              $ attachmentsToList possibilities,
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
      bitpathHelper bitpath =
        UTF8.fromString
         $ map (\bit -> case bit of
                  Zero -> '0'
                  One -> '1')
               $ bitpathToList bitpath
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


ensureTypeInContext
  :: ModernType
  -> ModernSerialization ()
ensureTypeInContext theType = do
  context <- getContext
  let hash = computeTypeHash theType
      knownTypes = modernContextTypes context
  if Map.member hash knownTypes
    then return ()
    else case theType of
           ModernListType contentType -> do
             ensureTypeInContext contentType
             let contentTypeHash = computeTypeHash contentType
             commandListType contentTypeHash
           ModernTupleType contentTypes -> do
             mapM ensureTypeInContext contentTypes
             let contentTypeHashes = map computeTypeHash contentTypes
             commandTupleType contentTypeHashes
           ModernUnionType contentTypes -> do
             mapM ensureTypeInContext $ attachmentsContents contentTypes
             let contentTypeHashes =
                   mapAttachments computeTypeHash contentTypes
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
           _ -> return ()


commandListType
  :: ModernHash
  -> ModernSerialization ()
commandListType contentType = do
  return () -- TODO


commandTupleType
  :: [ModernHash]
  -> ModernSerialization ()
commandTupleType contentTypes = do
  return () -- TODO


commandUnionType
  :: ModernAttachments ModernHash
  -> ModernSerialization ()
commandUnionType attachments = do
  return () -- TODO


commandStructureType
  :: [(ModernFieldName, ModernHash)]
  -> ModernSerialization ()
commandStructureType fields = do
  bitpath <- getCommandBitpath ModernCommandTypeStructureType
  outputCommandBits bitpath
  outputDataInt $ length fields
  mapM_ (\(ModernFieldName fieldName, ModernHash fieldTypeHash) -> do
           outputDataUTF8 fieldName
           outputData fieldTypeHash)
        fields


commandNamedType
  :: ModernTypeName
  -> ModernHash
  -> ModernSerialization ()
commandNamedType (ModernTypeName name) contentType = do
  bitpath <- getCommandBitpath ModernCommandTypeNamedType
  outputCommandBits bitpath
  outputDataUTF8 name


getCommandBitpath
  :: (ModernMonad m underlying,
      Monad (underlying Endianness))
  => ModernCommandType
  -> m ModernBitpath
getCommandBitpath commandType = do
  context <- getContext
  let commands = modernContextCommands context
      maybeBits = attachmentsLookup commandType commands
  case maybeBits of
    Nothing ->
      makeModernAction
       $ lift $ throw $ ModernFailure $ show commandType ++ " not mapped."
    Just bits -> return bits


outputCommandBits
  :: ModernBitpath
  -> ModernSerialization ()
outputCommandBits (ModernBitpath count source) = MakeModernSerialization $ do
  lift $ serializeWord source


outputData
  :: ByteString
  -> ModernSerialization ()
outputData byteString = MakeModernSerialization $ do
  lift $ write byteString


outputDataInt
  :: Int
  -> ModernSerialization ()
outputDataInt int = MakeModernSerialization $ do
  lift $ serializeWord (fromIntegral int :: Word64)


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

