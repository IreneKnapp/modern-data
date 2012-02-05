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
  | ModernUnionType ModernTypeName (ModernAttachments ModernType)
  | ModernStructureType ModernTypeName [(ModernFieldName, ModernType)]
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
  | ModernDataStructure ModernTypeName [(ModernFieldName, ModernData)]


data ModernCommandType
  = ModernCommandTypePad
  | ModernCommandTypeAssume
  | ModernCommandTypeAttach
  | ModernCommandTypeSplit
  | ModernCommandTypeDetach
  | ModernCommandTypeBeginConfiguration
  | ModernCommandTypeEndConfiguration
  | ModernCommandTypeListType
  | ModernCommandTypeUnionType
  | ModernCommandTypeStructureType
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
  | ModernCommandUnionType ModernTypeName (ModernAttachments ModernHash)
  | ModernCommandStructureType ModernTypeName [(ModernFieldName, ModernHash)]
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
                        splitZero = Attached ModernCommandTypePad,
                        splitOne =
                          Split {
                              splitZero =
                                Attached ModernCommandTypeBeginConfiguration,
                              splitOne =
                                Attached ModernCommandTypeEndConfiguration
                            }
                      },
                  splitOne =
                    Split {
                        splitZero =
                          Split {
                              splitZero = Unattached,
                              splitOne = Attached ModernCommandTypeListType
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
      modernContextTypes = Map.empty,
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
          ModernUnionType (ModernTypeName name) possibilities ->
            BS.concat [UTF8.fromString "Union(",
                       name,
                       BS.pack [0x00],
                       BS.intercalate
                        (UTF8.fromString ",")
                        $ map (\(bitpath, possibilityType) ->
                                  BS.concat [bitpathHelper bitpath,
                                             typeHelper possibilityType])
                              $ attachmentsToList possibilities,
                       UTF8.fromString ")"]
          ModernStructureType (ModernTypeName name) fields ->
            BS.concat [UTF8.fromString "Structure(",
                       name,
                       BS.pack [0x00],
                       BS.intercalate
                        (UTF8.fromString ",")
                        $ map (\(ModernFieldName fieldName, fieldType) ->
                                  BS.concat [fieldName,
                                             BS.pack [0x00],
                                             typeHelper fieldType])
                              fields,
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
dataType (ModernDataStructure typeName fields) =
  ModernStructureType typeName
                      $ map (\(fieldName, fieldData) ->
                               (fieldName, dataType fieldData))
                            fields


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
             commandListType contentType
           ModernUnionType unionName contents -> do
             mapM ensureTypeInContext $ attachmentsContents contents
             let contentTypes = mapAttachments computeTypeHash contents
             commandUnionType unionName contentTypes
           ModernStructureType structureName fields -> do
             mapM ensureTypeInContext $ map snd fields
             let fieldTypes =
                   map (\(fieldName, fieldType) ->
                           (fieldName, computeTypeHash fieldType))
                       fields
             commandStructureType structureName fieldTypes
           _ -> return ()


commandListType :: ModernType -> ModernSerialization ()
commandListType contentType = MakeModernSerialization $ do
    lift $ throw $ ModernFailure "commandListType not implemented."


commandUnionType
  :: ModernTypeName
  -> ModernAttachments ModernHash
  -> ModernSerialization ()
commandUnionType contents attachments = MakeModernSerialization $ do
  lift $ throw $ ModernFailure "commandUnionType not implemented."


commandStructureType
  :: ModernTypeName
  -> [(ModernFieldName, ModernHash)]
  -> ModernSerialization ()
commandStructureType structureName fields = do
  bitpath <- getCommandBitpath ModernCommandTypeStructureType
  outputCommandBits bitpath


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
  lift $ withContext LittleEndian $ do
    serializeWord source
