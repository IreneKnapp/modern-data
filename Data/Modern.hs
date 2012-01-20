module Data.Modern
  (ModernType(..),
   ModernData(..),
   ModernHash,
   ModernBitpath,
   ModernTypeName,
   ModernFieldName,
   ModernContext,
   initialContext,
   computeTypeHash)
  where

import Control.Monad.State
import Data.Array
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import Data.Digest.Murmur3
import Data.Int
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.String
import Data.Word


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
  | ModernUnionType ModernTypeName [(ModernBitpath, ModernType)]
  | ModernStructureType ModernTypeName [(ModernFieldName, ModernType)]


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
  | ModernDataUnion ModernTypeName ModernData


data ModernCommandType
  = ModernCommandTypeAssume
  | ModernCommandTypeInquire
  | ModernCommandTypeAttach
  | ModernCommandTypeDetach
  | ModernCommandTypeBeginConfiguration
  | ModernCommandTypeEndConfiguration
  | ModernCommandTypeUnionType
  | ModernCommandTypeStructureType
  | ModernCommandTypeAtom
  | ModernCommandTypeUTF8
  | ModernCommandTypeShortBlob
  | ModernCommandTypeLongBlob
  | ModernCommandTypeShortList ModernType
  | ModernCommandTypeLongList ModernType
  | ModernCommandTypeUnion (ModernAttachments ModernType)
  | ModernCommandTypeStructure ModernType


data ModernCommand
  = ModernCommandAssume ModernHash
  | ModernCommandInquire ModernHash
  | ModernCommandAttach ModernBitpath ModernHash
  | ModernCommandDetach ModernBitpath
  | ModernCommandBeginConfiguration ModernTypeName
  | ModernCommandEndConfiguration ModernTypeName
  | ModernCommandUnionType ModernTypeName
                           (ModernAttachments ModernHash)
  | ModernCommandStructureType ModernTypeName
                               [(ModernFieldName, ModernHash)]
  | ModernCommandData ModernData


data ModernHash
  = ModernHash ByteString
  deriving (Eq, Ord)


data ModernBitpath
  = ModernBitpath Word8 Word64


data Bit = Zero | One


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


data ModernTypeName
  = ModernTypeName ByteString
  deriving (Eq, Ord)
instance IsString ModernTypeName where
  fromString = ModernTypeName . UTF8.fromString


data ModernFieldName
  = ModernFieldName ByteString
  deriving (Eq, Ord)
instance IsString ModernFieldName where
  fromString = ModernFieldName . UTF8.fromString


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


initialContext :: ModernContext
initialContext =
  ModernContext {
      modernContextTypes = undefined,
      modernContextCommands = undefined
    }


computeTypeHash :: ModernType -> ModernHash
computeTypeHash modernType =
  ModernHash $ asByteString $ hash $ computeTypeByteString modernType


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
                              possibilities,
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
