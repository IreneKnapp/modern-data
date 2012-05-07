module Data.Modern.Deserialization
  (deserializeData,
   runModernDeserializationFromByteString,
   runModernDeserializationFromFile)
  where

import BinaryFiles hiding (getContext)
import Control.Monad.State.Strict
import qualified Data.Array as Array
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import Data.Maybe
import Data.Word
import Prelude hiding (read)

import Data.Modern.Hash
import Data.Modern.Initial
import Data.Modern.Types

import Debug.Trace


data ModernDeserializationContext =
  ModernDeserializationContext {
      modernDeserializationContextPendingCommandBitCount :: Word8,
      modernDeserializationContextPendingCommandBitSource :: Word64,
      modernDeserializationContextStartingOffset :: Word64
    }


deserializeData
  :: (ModernFormat format)
  => ModernDeserialization format ModernData
deserializeData = do
  let loop soFar = do
        maybeCommandType <- inputCommandType
        case maybeCommandType of
          Nothing -> return soFar
          Just ModernCommandTypeSynchronize -> return soFar
          Just commandType@ModernCommandTypeDatum
            -> return $ soFar ++ [commandType]
          Just commandType -> loop $ soFar ++ [commandType]
  commandTypes <- loop []
  maybeDatas <- mapM deserializeOneCommand commandTypes
  context <- getContext
  return $ head $ catMaybes $ maybeDatas


deserializeOneCommand
  :: (ModernFormat format)
  => ModernCommandType
  -> ModernDeserialization format (Maybe ModernData)
deserializeOneCommand commandType = do
  context <- getContext
  let knownTypes = modernContextTypes context
  case commandType of
    ModernCommandTypeDatum -> do
      typeHash <- inputDataHash
      case Map.lookup typeHash knownTypes of
        Nothing -> undefined -- TODO
        Just theType -> do
          datum <- deserializeOneDatum theType
          return $ Just datum
    ModernCommandTypeListType -> do
      contentTypeHash <- inputDataHash
      let contentType =
            case Map.lookup contentTypeHash knownTypes of
              Nothing -> undefined -- TODO
              Just knownType -> knownType
          theType = ModernListType contentType
      learnType theType
      return Nothing
    ModernCommandTypeTupleType -> do
      nItems <- inputDataWord64
      let loop soFar i = do
            if i == nItems
              then return soFar
              else do
                itemTypeHash <- inputDataHash
                let itemType =
                      case Map.lookup itemTypeHash knownTypes of
                        Nothing -> undefined -- TODO
                        Just knownType -> knownType
                loop (soFar ++ [itemType]) (i + 1)
      items <- loop [] 0
      if nItems == 0
        then learnType $ ModernTupleType Nothing
        else do
          let itemKeyValuePairs = zip [0 .. nItems - 1] items
              itemBounds = (0, nItems - 1)
              itemArray = Array.array itemBounds itemKeyValuePairs
              theType = ModernTupleType $ Just itemArray
          learnType theType
      return Nothing
    ModernCommandTypeUnionType -> do
      nItems <- inputDataWord64
      let loop soFar i = do
            if i == nItems
              then return soFar
              else do
                itemTypeHash <- inputDataHash
                let itemType =
                      case Map.lookup itemTypeHash knownTypes of
                        Nothing -> undefined -- TODO
                        Just knownType -> knownType
                loop (soFar ++ [itemType]) (i + 1)
      items <- loop [] 0
      if nItems == 0
        then learnType $ ModernUnionType Nothing
        else do
          let itemKeyValuePairs = zip [0 .. nItems - 1] items
              itemBounds = (0, nItems - 1)
              itemArray = Array.array itemBounds itemKeyValuePairs
              nBits = ceiling $ logBase 2 (fromIntegral nItems :: Double)
              theType = ModernUnionType $ Just (nBits, itemArray)
          learnType theType
      return Nothing
    ModernCommandTypeStructureType -> do
      nFields <- inputDataWord64
      let loop soFar i = do
            if i == nFields
              then return soFar
              else do
                fieldName <- inputDataUTF8
                inputAlign 8
                fieldTypeHash <- inputDataHash
                let fieldType =
                      case Map.lookup fieldTypeHash knownTypes of
                        Nothing -> undefined -- TODO
                        Just knownType -> knownType
                loop (soFar ++ [(ModernFieldName fieldName, fieldType)])
                     (i + 1)
      fields <- loop [] 0
      if nFields == 0
        then learnType $ ModernStructureType Nothing
        else do
          let fieldKeyValuePairs = zip [0 .. nFields - 1] fields
              fieldBounds = (0, nFields - 1)
              fieldArray = Array.array fieldBounds fieldKeyValuePairs
              theType = ModernStructureType $ Just fieldArray
          learnType theType
      return Nothing
    ModernCommandTypeNamedType -> do
      typeName <- inputDataUTF8
      inputAlign 8
      contentTypeHash <- inputDataHash
      let contentType =
            case Map.lookup contentTypeHash knownTypes of
              Nothing -> undefined -- TODO
              Just knownType -> knownType
          theType = ModernNamedType (ModernTypeName typeName) contentType
      learnType theType
      return Nothing


deserializeOneDatum
  :: (ModernFormat format)
  => ModernType
  -> ModernDeserialization format ModernData
deserializeOneDatum theType = do
  return $ ModernDataInt8 42
  {-
  case theType of
    ModernInt8Type -> do
      inputAlign 1
      undefined -- TODO
    ModernInt16Type -> do
      inputAlign 2
      undefined -- TODO
    ModernInt32Type -> do
      inputAlign 4
      undefined -- TODO
    ModernInt64Type -> do
      inputAlign 8
      undefined -- TODO
    ModernWord8Type -> do
      inputAlign 1
      undefined -- TODO
    ModernWord16Type -> do
      inputAlign 2
      undefined -- TODO
    ModernWord32Type -> do
      inputAlign 4
      undefined -- TODO
    ModernWord64Type -> do
      inputAlign 8
      undefined -- TODO
    ModernFloatType -> do
      inputAlign 4
      undefined -- TODO
    ModernDoubleType -> do
      inputAlign 8
      undefined -- TODO
    ModernUTF8Type -> do
      inputAlign 8
      result <- undefined -- TODO
      inputAlign 8
      return result
    ModernBlobType -> do
      inputAlign 8
      result <- undefined -- TODO
      inputAlign 8
      return result
    ModernListType contentType -> do
      undefined -- TODO
    ModernTupleType maybeContentTypes -> do
      undefined -- TODO
    ModernUnionType maybeContentTypes -> do
      undefined -- TODO
    ModernStructureType maybeContentTypes -> do
      undefined -- TODO
    ModernNamedType _ contentType -> do
      undefined -- TODO
      -}


learnType
  :: (ModernFormat format)
  => ModernType
  -> ModernDeserialization format ()
learnType theType = do
  oldContext <- getContext
  let oldKnownTypes = modernContextTypes oldContext
      theHash = computeTypeHash theType
      newKnownTypes = Map.insert theHash theType oldKnownTypes
      newContext = oldContext {
                       modernContextTypes = newKnownTypes
                     }
  putContext newContext


runModernDeserializationFromByteString
  :: (ModernFormat format)
  => ModernContext
  -> ByteString
  -> (ModernDeserialization format a)
  -> Either (Int, [(Int, String)], SomeSerializationFailure)
            (ModernContext, a)
runModernDeserializationFromByteString context input action =
  runDeserializationFromByteString
    (do
      withContext LittleEndian $ do
        (result, (context', _)) <-
          runStateT (modernDeserializationAction action)
		    (context, initialDeserializationContext)
        return (context', result))
    input


runModernDeserializationFromFile
  :: (ModernFormat format)
  => ModernContext
  -> FilePath
  -> (ModernDeserialization format a)
  -> IO (Either (Int, [(Int, String)], SomeSerializationFailure)
                (ModernContext, a))
runModernDeserializationFromFile context filePath action = do
  runDeserializationFromFile
    (do
      withContext LittleEndian $ do
        (result, (context', _)) <-
          runStateT (modernDeserializationAction action)
		    (context, initialDeserializationContext)
        return (context', result))
    filePath

