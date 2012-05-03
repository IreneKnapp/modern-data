module Data.Modern.Initial
  (initialTypes,
   standardCommandEncodingBitsize,
   standardCommandDecodings,
   standardCommandEncodings,
   initialContext)
  where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Word

import Data.Modern.Hash
import Data.Modern.Types


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
      modernContextTypes = initialTypes
    }

