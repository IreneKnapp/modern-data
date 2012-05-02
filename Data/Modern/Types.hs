{-# LANGUAGE DeriveDataTypeable #-}
module Data.Modern.Types
  (ModernMonad(..),
   ModernDeserialization(..),
   ModernSerialization(..),
   ModernType(..),
   ModernData(..),
   ModernCommandType(..),
   ModernHash(..),
   ModernTypeName(..),
   ModernFieldName(..),
   ModernContext(..),
   PendingData(..),
   ModernFailure(..))
  where

import BinaryFiles hiding (getContext)
import Control.Monad.State.Strict
import Data.Array (Array)
import Data.ByteString (ByteString)
import qualified Data.ByteString.UTF8 as UTF8
import Data.Int
import Data.Map (Map)
import Data.String
import Data.Typeable
import Data.Word


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

