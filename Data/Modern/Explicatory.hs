{-# LANGUAGE TypeFamilies #-}
module Data.Modern.Explicatory (FormatExplicatory(..)) where

import BinaryFiles
import Control.Monad.State.Strict
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Word

import Data.Modern.Types


data FormatExplicatory
  = FormatExplicatory
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
      Just encoding -> do
	MakeModernSerialization $ lift
	 $ write $ UTF8.fromString $ encoding ++ "\n"
      Nothing -> return ()
  outputCommandBits outputCount outputSource = do
    let loop soFar index =
	  if index == outputCount
	    then reverse soFar
	    else let c = if testBit outputSource
				    (fromIntegral outputCount
				     - fromIntegral index - 1)
			   then '1'
			   else '0'
		 in loop (soFar ++ [c]) (index + 1)
        output = loop "c" 0
    MakeModernSerialization $ lift
     $ write $ UTF8.fromString $ output ++ "\n"
  outputData byteString = do
    oldContext <- getSerializationContext
    let oldPendingData = serializationContextPendingData oldContext
        newPendingData = oldPendingData ++ [show byteString]
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
    mapM_ (\pendingDatum -> do
	     MakeModernSerialization $ lift
	      $ write $ UTF8.fromString $ pendingDatum ++ "\n")
          oldPendingData
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


commandSynchronize
  :: (ModernFormat format)
  => ModernSerialization format ()
commandSynchronize = do
  outputCommandType ModernCommandTypeSynchronize
  outputSynchronize


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
