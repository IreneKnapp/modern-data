module Data.Modern.Initial
  (initialTypes,
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


initialContext :: ModernContext
initialContext =
  ModernContext {
      modernContextTypes = initialTypes
    }

