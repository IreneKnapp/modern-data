{-# LANGUAGE TypeFamilies #-}
module Data.Modern.Binary (FormatBinary(..)) where

import BinaryFiles
import Control.Monad.State.Strict
import Data.Bits
import qualified Data.ByteString as BS
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Word

import Data.Modern.Types


data FormatBinary
  = FormatBinaryExperimental
instance ModernFormat FormatBinary where
  data FormatSerializationContext FormatBinary =
    SerializationContext {
        serializationContextPendingCommandBitCount :: Word8,
        serializationContextPendingCommandBitSource :: Word64,
        serializationContextPendingCommandWords :: [Word64],
        serializationContextPendingData :: [PendingData],
        serializationContextStartingOffset :: Word64
      }
  initialSerializationContext =
    SerializationContext {
        serializationContextPendingCommandBitCount = 0,
        serializationContextPendingCommandBitSource = 0,
        serializationContextPendingCommandWords = [],
        serializationContextPendingData = [],
        serializationContextStartingOffset = 0
      }
  outputCommandType commandType = do
    case Map.lookup commandType standardCommandEncodings of
      Just source -> outputCommandBits standardCommandEncodingBitsize source
      Nothing -> return ()
  outputCommandBits outputCount outputSource = do
    oldContext <- getSerializationContext
    let oldCount = serializationContextPendingCommandBitCount oldContext
        oldSource = serializationContextPendingCommandBitSource oldContext
        combinedSources =
          oldSource .|. (shiftL outputSource $ fromIntegral oldCount)
        (newCount, newSource, immediateOutput) =
          if oldCount + outputCount >= 64
            then (oldCount + outputCount - 64,
                  shiftR outputSource $ fromIntegral $ 64 - oldCount,
                  Just combinedSources)
            else (oldCount + outputCount,
                  combinedSources,
                  Nothing)
        oldStartingOffset = serializationContextStartingOffset oldContext
        newStartingOffset =
          case immediateOutput of
            Nothing -> oldStartingOffset
            Just _ -> oldStartingOffset + 8
        oldPendingCommandWords =
	  serializationContextPendingCommandWords oldContext
        newPendingCommandWords =
          case immediateOutput of
            Nothing -> oldPendingCommandWords
            Just word -> oldPendingCommandWords ++ [word]
        newContext = oldContext {
                         serializationContextPendingCommandBitCount =
			   newCount,
                         serializationContextPendingCommandBitSource =
			   newSource,
                         serializationContextPendingCommandWords =
                           newPendingCommandWords,
                         serializationContextStartingOffset =
			   newStartingOffset
                       }
    putSerializationContext newContext
  outputData byteString = do
    oldContext <- getSerializationContext
    let oldPendingData = serializationContextPendingData oldContext
        newPendingData = oldPendingData ++ [PendingBytes byteString]
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
    let oldCount = serializationContextPendingCommandBitCount oldContext
        oldSource = serializationContextPendingCommandBitSource oldContext
        oldPendingCommandWords =
	  serializationContextPendingCommandWords oldContext
        oldPendingData = serializationContextPendingData oldContext
        oldStartingOffset = serializationContextStartingOffset oldContext
    mapM_ (\commandWord -> do
             MakeModernSerialization $ lift $ serializeWord commandWord)
          oldPendingCommandWords
    if oldCount > 0
      then MakeModernSerialization $ lift $ serializeWord oldSource
      else return ()
    newStartingOffset <-
      foldM (\startingOffset pendingDatum -> do
               case pendingDatum of
                 PendingBytes byteString -> do
                   MakeModernSerialization $ lift $ write byteString
                   return $ startingOffset
                            + (fromIntegral $ BS.length byteString)
                 PendingAlignmentMark alignment -> do
                   let misalignment = mod startingOffset alignment
                       padLength = if misalignment == 0
                                     then 0
                                     else alignment - misalignment
                   MakeModernSerialization
                    $ lift $ write $ BS.pack $ genericTake padLength
		    $ repeat 0x00  
                   return $ startingOffset + padLength)
            oldStartingOffset
            oldPendingData
    let newContext = oldContext {
                         serializationContextPendingCommandBitCount = 0,
                         serializationContextPendingCommandBitSource = 0,
                         serializationContextPendingCommandWords = [],
                         serializationContextPendingData = [],
                         serializationContextStartingOffset = newStartingOffset
                       }
    putSerializationContext newContext
  outputAlign alignment = do
    oldContext <- getSerializationContext
    let oldPendingData = serializationContextPendingData oldContext
        newPendingData = oldPendingData ++ [PendingAlignmentMark alignment]
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
