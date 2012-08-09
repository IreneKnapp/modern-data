{-# LANGUAGE DeriveDataTypeable, TypeFamilies #-}
module Data.Modern.Types
  (ModernMonad(..),
   ModernType(..),
   ModernData(..),
   dataType,
   ModernCommandType(..),
   ModernHash(..),
   ModernTypeName(..),
   ModernFieldName(..),
   ModernContext(..),
   PendingData(..),
   ModernFailure(..),
   fromString,
   ModernSerialization(..),
   getSerializationContext,
   putSerializationContext,
   ModernDeserialization(..),
   getDeserializationContext,
   putDeserializationContext,
   ModernFormat(..))
  where

import BinaryFiles hiding (getContext)
import Control.Monad.State.Strict
import Data.Array (Array)
import Data.Bits
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


data ModernType
  = ModernInt8Type
  | ModernInt16Type
  | ModernInt32Type
  | ModernInt64Type
  | ModernNat8Type
  | ModernNat16Type
  | ModernNat32Type
  | ModernNat64Type
  | ModernFloat32Type
  | ModernFloat64Type
  | ModernUTF8Type
  | ModernBlobType
  | ModernListType ModernType
  | ModernTupleType (Maybe (Array Word64 ModernType))
  | ModernUnionType (Maybe (Word8, (Array Word64 ModernType)))
  | ModernStructureType (Maybe (Array Word64 (ModernFieldName, ModernType)))
  | ModernNamedType ModernTypeName ModernType
  deriving (Eq, Ord, Show)


data ModernData
  = ModernDataInt8 Int8
  | ModernDataInt16 Int16
  | ModernDataInt32 Int32
  | ModernDataInt64 Int64
  | ModernDataNat8 Word8
  | ModernDataNat16 Word16
  | ModernDataNat32 Word32
  | ModernDataNat64 Word64
  | ModernDataFloat32 Float
  | ModernDataFloat64 Double
  | ModernDataUTF8 ByteString
  | ModernDataBlob ByteString
  | ModernDataList ModernType (Array Int ModernData)
  | ModernDataTuple ModernType (Array Int ModernData)
  | ModernDataUnion ModernType Word64 ModernData
  | ModernDataStructure ModernType (Array Int ModernData)
  | ModernDataNamed ModernType ModernData


dataType :: ModernData -> ModernType
dataType (ModernDataInt8 _) = ModernInt8Type
dataType (ModernDataInt16 _) = ModernInt16Type
dataType (ModernDataInt32 _) = ModernInt32Type
dataType (ModernDataInt64 _) = ModernInt64Type
dataType (ModernDataNat8 _) = ModernNat8Type
dataType (ModernDataNat16 _) = ModernNat16Type
dataType (ModernDataNat32 _) = ModernNat32Type
dataType (ModernDataNat64 _) = ModernNat64Type
dataType (ModernDataFloat32 _) = ModernFloat32Type
dataType (ModernDataFloat64 _) = ModernFloat64Type
dataType (ModernDataUTF8 _) = ModernUTF8Type
dataType (ModernDataBlob _) = ModernBlobType
dataType (ModernDataList theType _) = theType
dataType (ModernDataTuple theType _) = theType
dataType (ModernDataUnion theType _ _) = theType
dataType (ModernDataStructure theType _) = theType
dataType (ModernDataNamed theType theValue) = theType


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
      modernContextTypes :: Map ModernHash ModernType
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


data ModernSerialization format a =
  MakeModernSerialization {
      modernSerializationAction
        :: StateT (ModernContext, FormatSerializationContext format)
		  (ContextualSerialization Endianness) a
    }
instance (ModernFormat format)
         => Monad (ModernSerialization format)
         where
  return a = MakeModernSerialization $ return a
  x >>= f =
    MakeModernSerialization $ do
      v <- modernSerializationAction x
      modernSerializationAction $ f v
instance (ModernFormat format)
         => ModernMonad (ModernSerialization format)
         where
  getContext = MakeModernSerialization $ do
    (context, _) <- get
    return context
  putContext context = MakeModernSerialization $ do
    (_, serializationContext) <- get
    put (context, serializationContext)


getSerializationContext
  :: (ModernFormat format)
  => ModernSerialization format (FormatSerializationContext format)
getSerializationContext = MakeModernSerialization $ do
  (_, serializationContext) <- get
  return serializationContext


putSerializationContext
  :: (ModernFormat format)
  => FormatSerializationContext format -> ModernSerialization format ()
putSerializationContext serializationContext = MakeModernSerialization $ do
  (context, _) <- get
  put (context, serializationContext)


data ModernDeserialization format a =
  MakeModernDeserialization {
      modernDeserializationAction
        :: StateT (ModernContext, FormatDeserializationContext format)
		  (ContextualDeserialization Endianness) a
    }
instance (ModernFormat format)
         => Monad (ModernDeserialization format)
         where
  return a = MakeModernDeserialization $ return a
  x >>= f =
    MakeModernDeserialization $ do
      v <- modernDeserializationAction x
      modernDeserializationAction $ f v
instance (ModernFormat format)
         => ModernMonad (ModernDeserialization format)
	 where
  getContext = MakeModernDeserialization $ do
    (context, _) <- get
    return context
  putContext context = MakeModernDeserialization $ do
    (_, deserializationContext) <- get
    put (context, deserializationContext)


getDeserializationContext
  :: (ModernFormat format)
  => ModernDeserialization format (FormatDeserializationContext format)
getDeserializationContext = MakeModernDeserialization $ do
  (_, deserializationContext) <- get
  return deserializationContext


putDeserializationContext
  :: (ModernFormat format)
  => FormatDeserializationContext format
  -> ModernDeserialization format ()
putDeserializationContext deserializationContext =
  MakeModernDeserialization $ do
    (context, _) <- get
    put (context, deserializationContext)


class ModernFormat format where
  data FormatSerializationContext format
  initialSerializationContext :: FormatSerializationContext format
  outputCommandType :: ModernCommandType -> ModernSerialization format ()
  outputCommandBits
    :: Word8
    -> Word64
    -> ModernSerialization format ()
  outputDataHash :: ModernHash -> ModernSerialization format ()
  outputDataUTF8 :: ByteString -> ModernSerialization format ()
  outputDataBlob :: ByteString -> ModernSerialization format ()
  outputDataNat
    :: (Bits word, Integral word, Num word)
    => word
    -> ModernSerialization format ()
  outputSynchronize :: ModernSerialization format ()
  outputAlign :: Word64 -> ModernSerialization format ()
  
  data FormatDeserializationContext format
  initialDeserializationContext :: FormatDeserializationContext format
  inputCommandBits
    :: Word8
    -> ModernDeserialization format Word64
  inputCommandType
    :: ModernDeserialization format (Maybe ModernCommandType)
  inputDataHash
    :: ModernDeserialization format ModernHash
  inputDataNat8
    :: ModernDeserialization format Word8
  inputDataNat16
    :: ModernDeserialization format Word16
  inputDataNat32
    :: ModernDeserialization format Word32
  inputDataNat64
    :: ModernDeserialization format Word64
  inputDataUTF8
    :: ModernDeserialization format ByteString
  inputAlign
    :: Word64
    -> ModernDeserialization format ()

