{-# LANGUAGE TypeFamilies #-}
module Data.Modern.Explicatory (FormatExplicatory(..)) where

import BinaryFiles
import Control.Monad.State.Strict
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import Data.Char
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Word
import Prelude hiding (read)

import Data.Modern.Types

import Debug.Trace


data FormatExplicatory
instance ModernFormat FormatExplicatory where
  data FormatSerializationContext FormatExplicatory =
    SerializationContext {
        serializationContextPendingData :: [String]
      }
  initialSerializationContext =
    SerializationContext {
        serializationContextPendingData = []
      }
  outputCommandType commandType = do
    case Map.lookup commandType standardCommandEncodings of
      Just encoding -> writeLine encoding
      Nothing -> return ()
  outputCommandBits outputCount outputSource = do
    let loop soFar index =
	  if index == outputCount
	    then soFar
	    else let c = if testBit outputSource
				    (fromIntegral outputCount
				     - fromIntegral index - 1)
			   then '1'
			   else '0'
		 in loop (soFar ++ [c]) (index + 1)
        output = loop "C" 0
    writeLine output
  outputData byteString = do
    oldContext <- getSerializationContext
    let oldPendingData = serializationContextPendingData oldContext
        newPendingData = oldPendingData ++ [showBytes byteString]
        newContext = oldContext {
                         serializationContextPendingData = newPendingData
                       }
    putSerializationContext newContext
  outputDataWord word = do
    let result = runSerializationToByteString $ withContext LittleEndian
                  $ serializeWord word
    case result of
      Right ((), byteString) -> outputData byteString
  outputSynchronize = do
    oldContext <- getSerializationContext
    let oldPendingData = serializationContextPendingData oldContext
    mapM_ writeLine oldPendingData
    let newContext = oldContext {
                         serializationContextPendingData = []
                       }
    putSerializationContext newContext
  outputAlign alignment = do
    oldContext <- getSerializationContext
    let oldPendingData = serializationContextPendingData oldContext
        newPendingData = oldPendingData ++ ["Align " ++ show alignment]
        newContext = oldContext {
                         serializationContextPendingData = newPendingData
                       }
    putSerializationContext newContext
  data FormatDeserializationContext FormatExplicatory =
    DeserializationContext {
        deserializationContextPendingCommandTypes :: [ModernCommandType]
      }
  initialDeserializationContext =
    DeserializationContext {
	deserializationContextPendingCommandTypes = []
      }
  inputCommandBits inputCount = do
    return undefined
  inputCommandType = do
    encoding <- readLine
    return $ Map.lookup encoding standardCommandDecodings
  inputDataHash = do
    return undefined
  inputDataWord8 = inputDataWord
  inputDataWord16 = inputDataWord
  inputDataWord32 = inputDataWord
  inputDataWord64 = inputDataWord
  inputDataUTF8 = do
    return undefined
  inputAlign alignment = do
    return undefined


inputDataWord
  :: (Bits word, Integral word, Num word, ModernFormat format)
  => ModernDeserialization format word
inputDataWord = do
  encoding <- readLine
  trace encoding $ return ()
  case parseBytes encoding of
    Just byteString -> do
      case runDeserializationFromByteString
             (withContext LittleEndian deserializeWord)
	     byteString of
        Right word -> return word
        Left _ -> error "Hm." -- TODO
    Nothing -> error "Hm." -- TODO


writeLine
  :: (ModernFormat format)
  => String
  -> ModernSerialization format ()
writeLine string = do
  MakeModernSerialization $ lift $ write $ UTF8.fromString $ string ++ "\n"


readLine
  :: (ModernFormat format)
  => ModernDeserialization format String
readLine = do
  let readChar = MakeModernDeserialization $ lift $ read 1
      loop soFar = do
	byteString <- readChar
	if (BS.null byteString)
	   || (byteString == UTF8.fromString "\n")
	  then return soFar
	  else loop (BS.concat [soFar, byteString])
  byteString <- loop BS.empty
  return $ UTF8.toString byteString


standardCommandDecodings :: Map String ModernCommandType
standardCommandDecodings =
  Map.fromList
    [("Synchronize", ModernCommandTypeSynchronize),
     ("Datum", ModernCommandTypeDatum),
     ("ListType", ModernCommandTypeListType),
     ("TupleType", ModernCommandTypeTupleType),
     ("UnionType", ModernCommandTypeUnionType),
     ("StructureType", ModernCommandTypeStructureType),
     ("NamedType", ModernCommandTypeNamedType)]


standardCommandEncodings :: Map ModernCommandType String
standardCommandEncodings =
  Map.fromList
   $ map (\(key, value) -> (value, key))
         $ Map.toList standardCommandDecodings


showBytes :: ByteString -> String
showBytes byteString =
  let loop soFar index =
	if index >= BS.length byteString
	  then soFar
	  else let byte = BS.head $ BS.drop index byteString
		   highNibble = showNibble (shiftR byte 4 .&. 0x0F)
		   lowNibble = showNibble (shiftR byte 0 .&. 0x0F)
	       in loop (soFar ++ [highNibble, lowNibble]) (index + 1)
      showNibble nibble = head $ drop (fromIntegral nibble) "0123456789abcdef"
  in loop "0x" 0


parseBytes :: String -> Maybe ByteString
parseBytes string =
  if (length string > 2) && (take 2 string == "0x")
    then let nibbles = drop 2 string
	     parseNibblePair nibblePair =
	       let highNibble = head nibblePair
		   lowNibble = head $ drop 1 nibblePair
	       in do
		 highBits <- parseNibble highNibble
		 lowBits <- parseNibble lowNibble
		 return (shiftL highBits 4 .|. lowBits)
	     parseNibble nibble =
	       fmap fromIntegral
		    $ elemIndex (toLower nibble) "0123456789abcdef"
	 in if even (length nibbles)
	      then Just
		    $ BS.pack
		    $ reverse
		    $ unfoldr (\restNibbles ->
				 if null restNibbles
				   then Nothing
				   else let (nibblePair, restNibbles') =
					      splitAt 2 restNibbles
					in case parseNibblePair nibblePair of
					     Just byte ->
					       Just (byte, restNibbles')
					     Nothing -> Nothing)
			      nibbles
	      else Nothing
    else Nothing

