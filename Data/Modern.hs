module Data.Modern
  (ModernType(..),
   ModernData(..),
   ModernCommandType(..),
   ModernCommand(..),
   ModernHash(..),
   ModernBitpath(..),
   ModernTypeName(..),
   ModernFieldName(..),
   ModernContext(..))
  where

import Data.Array
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import Data.Int
import Data.Map (Map)
import qualified Data.Map as Map
import Data.String
import Data.Word


data ModernType
  = ModernTypeInt8
  | ModernTypeInt16
  | ModernTypeInt32
  | ModernTypeInt64
  | ModernTypeWord8
  | ModernTypeWord16
  | ModernTypeWord32
  | ModernTypeWord64
  | ModernTypeFloat
  | ModernTypeDouble
  | ModernTypeUTF8
  | ModernTypeBlob
  | ModernTypeList ModernType
  | ModernTypeUnion ModernTypeName [(ModernBitpath, ModernHash)]
  | ModernTypeStructure ModernTypeName [(ModernFieldName, ModernHash)]


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
  | ModernCommandTypeUnionType
  | ModernCommandTypeStructureType
  | ModernCommandTypeDataInt8
  | ModernCommandTypeDataInt16
  | ModernCommandTypeDataInt32
  | ModernCommandTypeDataInt64
  | ModernCommandTypeDataWord8
  | ModernCommandTypeDataWord16
  | ModernCommandTypeDataWord32
  | ModernCommandTypeDataWord64
  | ModernCommandTypeDataFloat
  | ModernCommandTypeDataDouble
  | ModernCommandTypeDataUTF8
  | ModernCommandTypeDataShortBlob
  | ModernCommandTypeDataLongBlob
  | ModernCommandTypeDataShortList
  | ModernCommandTypeDataLongList
  | ModernCommandTypeDataAttachedType ModernBitpath


data ModernCommand
  = ModernCommandAssume ModernHash
  | ModernCommandInquire ModernHash
  | ModernCommandAttach ModernBitpath ModernHash
  | ModernCommandDetach ModernBitpath
  | ModernCommandUnionType ModernTypeName [(ModernBitpath, ModernHash)]
  | ModernCommandStructureType ModernTypeName [(ModernFieldName, ModernHash)]
  | ModernCommandData ModernType ModernData


data ModernHash
  = ModernHash ByteString
  deriving (Eq, Ord)


data ModernBitpath
  = ModernBitpath Int Word64


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
      modernContextAttachments :: ModernContextAttachments
    }


data ModernContextAttachments
  = Attached ModernCommandType
  | Split {
        splitZero :: ModernContextAttachments,
        splitOne :: ModernContextAttachments
      }
