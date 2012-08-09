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
   
   -- From Data.Modern.Context
   Internal.typeContentTypes,
   Internal.typeInContext,
   Internal.typesNotInContext,
   Internal.ensureTypesInContext,
   
   -- From here, wrapping around Data.Modern.Serialization
   serializeToByteString,
   serializeToFile,
   serializeToExplicatoryByteString,
   serializeToExplicatoryFile,
   
   -- From here, wrapping around Data.Modern.Deserialization
   deserializeFromByteString,
   deserializeFromFile,
   deserializeFromExplicatoryByteString,
   deserializeFromExplicatoryFile,
   
   -- From Data.Modern.Binary
   Internal.FormatBinary(..),
   
   -- From Data.Modern.Explicatory
   Internal.FormatExplicatory(..),
   
   -- From Data.Modern.Documentation
   Internal.documentSchema,
   
   -- From BinaryFiles
   SomeSerializationFailure(..),
   
   -- From Data.ByteString
   ByteString)
  where

import BinaryFiles
import Data.ByteString (ByteString)

import qualified Data.Modern.Binary as Internal
import qualified Data.Modern.Context as Internal
import qualified Data.Modern.Deserialization as Internal
import qualified Data.Modern.Documentation as Internal
import qualified Data.Modern.Explicatory as Internal
import qualified Data.Modern.Hash as Internal
import qualified Data.Modern.Initial as Internal
import qualified Data.Modern.Serialization as Internal
import qualified Data.Modern.Types as Internal


serializeToByteString
  :: Internal.ModernContext
  -> Internal.ModernData
  -> Either (Int, [(Int, String)], SomeSerializationFailure)
            (ByteString, Internal.ModernContext)
serializeToByteString context datum =
  case Internal.runModernSerializationToByteString
         context $ do
           (Internal.serializeData datum
              :: Internal.ModernSerialization Internal.FormatBinary ()) of 
    Left failure -> Left failure
    Right (byteString, newContext, ()) -> Right (byteString, newContext)


serializeToFile
  :: Internal.ModernContext
  -> Internal.ModernData
  -> FilePath
  -> IO (Either (Int, [(Int, String)], SomeSerializationFailure)
                Internal.ModernContext)
serializeToFile context datum filePath = do
  result <-
    Internal.runModernSerializationToFile
      context filePath $ do
        Internal.serializeData datum
	  :: Internal.ModernSerialization Internal.FormatBinary ()
  case result of
    Left failure -> return $ Left failure
    Right (newContext, ()) -> return $ Right newContext


serializeToExplicatoryByteString
  :: Internal.ModernContext
  -> Internal.ModernData
  -> Either (Int, [(Int, String)], SomeSerializationFailure)
            (ByteString, Internal.ModernContext)
serializeToExplicatoryByteString context datum =
  case Internal.runModernSerializationToByteString
         context $ do
           Internal.serializeData datum
	     :: Internal.ModernSerialization Internal.FormatExplicatory () of
    Left failure -> Left failure
    Right (byteString, newContext, ()) -> Right (byteString, newContext)


serializeToExplicatoryFile
  :: Internal.ModernContext
  -> Internal.ModernData
  -> FilePath
  -> IO (Either (Int, [(Int, String)], SomeSerializationFailure)
                Internal.ModernContext)
serializeToExplicatoryFile context datum filePath = do
  result <-
    Internal.runModernSerializationToFile
      context filePath $ do
        Internal.serializeData datum
	  :: Internal.ModernSerialization Internal.FormatExplicatory ()
  case result of
    Left failure -> return $ Left failure
    Right (newContext, ()) -> return $ Right newContext


deserializeFromByteString
  :: Internal.ModernContext
  -> ByteString
  -> Either (Int, [(Int, String)], SomeSerializationFailure)
            (Internal.ModernContext, Internal.ModernData)
deserializeFromByteString context byteString =
  Internal.runModernDeserializationFromByteString
    context byteString $ do
      (Internal.deserializeData
	 :: Internal.ModernDeserialization
	     Internal.FormatBinary
	     Internal.ModernData)


deserializeFromFile
  :: Internal.ModernContext
  -> FilePath
  -> IO (Either (Int, [(Int, String)], SomeSerializationFailure)
                (Internal.ModernContext, Internal.ModernData))
deserializeFromFile context filePath =
  Internal.runModernDeserializationFromFile
    context "input.txt" $ do
      (Internal.deserializeData
	 :: Internal.ModernDeserialization
	     Internal.FormatBinary
	     Internal.ModernData)


deserializeFromExplicatoryByteString
  :: Internal.ModernContext
  -> ByteString
  -> Either (Int, [(Int, String)], SomeSerializationFailure)
            (Internal.ModernContext, Internal.ModernData)
deserializeFromExplicatoryByteString context byteString =
  Internal.runModernDeserializationFromByteString
    context byteString $ do
      (Internal.deserializeData
	 :: Internal.ModernDeserialization
	     Internal.FormatExplicatory
	     Internal.ModernData)


deserializeFromExplicatoryFile
  :: Internal.ModernContext
  -> FilePath
  -> IO (Either (Int, [(Int, String)], SomeSerializationFailure)
                (Internal.ModernContext, Internal.ModernData))
deserializeFromExplicatoryFile context filePath =
  Internal.runModernDeserializationFromFile
    context "input.txt" $ do
      (Internal.deserializeData
	 :: Internal.ModernDeserialization
	     Internal.FormatExplicatory
	     Internal.ModernData)

