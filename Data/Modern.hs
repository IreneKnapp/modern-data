{-# LANGUAGE Rank2Types, StandaloneDeriving, DeriveDataTypeable #-}
module Data.Modern
  (-- From Data.Modern.Types
   Internal.ModernType(..),
   Internal.ModernData(..),
   Internal.ModernHash,
   Internal.ModernTypeName,
   Internal.ModernFieldName,
   Internal.ModernContext,
   Internal.ModernFailure(..),
   Internal.fromString,
   
   -- From Data.Modern.Hash
   Internal.computeTypeHash,
   
   -- From Data.Modern.Initial
   Internal.initialContext,
   
   -- From Data.Modern.Serialization
   Internal.serializeData,
   Internal.ensureTypeInContext,
   runModernSerializationToByteString,
   runModernSerializationToFile,
   runModernSerializationToExplicatoryByteString,
   runModernSerializationToExplicatoryFile,
   
   -- From Data.Modern.Deserialization
   -- deserializeData,
   -- runModernDeserializationFromByteString,
   -- runModernDeserializationFromFile,
   -- runModernDeserializationFromExplicatoryByteString,
   -- runModernDeserializationFromExplicatoryFile,
   
   -- From Data.Modern.Binary
   Internal.FormatBinary(..),
   
   -- From Data.Modern.Explicatory,
   Internal.FormatExplicatory(..),
   
   -- From BinaryFiles
   SomeSerializationFailure(..),
   
   -- From Data.ByteString
   ByteString)
  where

import BinaryFiles
import Data.ByteString (ByteString)

import qualified Data.Modern.Binary as Internal
import qualified Data.Modern.Explicatory as Internal
import qualified Data.Modern.Deserialization as Internal
import qualified Data.Modern.Hash as Internal
import qualified Data.Modern.Initial as Internal
import qualified Data.Modern.Serialization as Internal
import qualified Data.Modern.Types as Internal


runModernSerializationToByteString
  :: Internal.ModernContext
  -> (Internal.ModernSerialization Internal.FormatBinary a)
  -> Either (Int, [(Int, String)], SomeSerializationFailure)
            (ByteString, Internal.ModernContext, a)
runModernSerializationToByteString =
  Internal.runModernSerializationToByteString


runModernSerializationToFile
  :: Internal.ModernContext
  -> FilePath
  -> (Internal.ModernSerialization Internal.FormatBinary a)
  -> IO (Either (Int, [(Int, String)], SomeSerializationFailure)
                (Internal.ModernContext, a))
runModernSerializationToFile =
  Internal.runModernSerializationToFile


runModernSerializationToExplicatoryByteString
  :: Internal.ModernContext
  -> (Internal.ModernSerialization Internal.FormatExplicatory a)
  -> Either (Int, [(Int, String)], SomeSerializationFailure)
            (ByteString, Internal.ModernContext, a)
runModernSerializationToExplicatoryByteString =
  Internal.runModernSerializationToByteString


runModernSerializationToExplicatoryFile
  :: Internal.ModernContext
  -> FilePath
  -> (Internal.ModernSerialization Internal.FormatExplicatory a)
  -> IO (Either (Int, [(Int, String)], SomeSerializationFailure)
                (Internal.ModernContext, a))
runModernSerializationToExplicatoryFile =
  Internal.runModernSerializationToFile

