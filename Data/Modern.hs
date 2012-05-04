{-# LANGUAGE Rank2Types, StandaloneDeriving, DeriveDataTypeable #-}
module Data.Modern
  (-- From Data.Modern.Types
   ModernType(..),
   ModernData(..),
   ModernHash,
   ModernTypeName,
   ModernFieldName,
   ModernContext,
   ModernFailure(..),
   fromString,
   ModernFormat,
  
   -- From Data.Modern.Hash
   computeTypeHash,

   -- From Data.Modern.Initial
   initialContext,
   
   -- From Data.Modern.Serialization
   serializeData,
   ensureTypeInContext,
   runModernSerializationToByteString,
   runModernSerializationToFile,
   
   -- From Data.Modern.Deserialization
   -- deserializeData,
   -- runModernDeserializationFromByteString,
   -- runModernDeserializationFromFile,
   
   -- From Data.Modern.Binary
   FormatBinary(..),

   -- From Data.Modern.Explicatory
   FormatExplicatory(..))
  where

import Data.Modern.Binary
import Data.Modern.Explicatory
import Data.Modern.Deserialization
import Data.Modern.Hash
import Data.Modern.Initial
import Data.Modern.Serialization
import Data.Modern.Types

