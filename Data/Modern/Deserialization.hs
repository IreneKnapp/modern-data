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
-- import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Data.Word
import Prelude hiding (read)

import Data.Modern.Hash
import Data.Modern.Initial
import Data.Modern.Types


data ModernDeserializationContext =
  ModernDeserializationContext {
      modernDeserializationContextPendingCommandBitCount :: Word8,
      modernDeserializationContextPendingCommandBitSource :: Word64,
      modernDeserializationContextStartingOffset :: Word64
    }


initialDeserializationContext :: ModernDeserializationContext
initialDeserializationContext =
  ModernDeserializationContext {
      modernDeserializationContextPendingCommandBitCount = 0,
      modernDeserializationContextPendingCommandBitSource = 0,
      modernDeserializationContextStartingOffset = 0
    }


data ModernDeserialization a =
  MakeModernDeserialization {
      modernDeserializationAction
        :: StateT (ModernContext, ModernDeserializationContext)
		  (ContextualDeserialization Endianness) a
    }
instance Monad ModernDeserialization where
  return a = MakeModernDeserialization $ return a
  x >>= f =
    MakeModernDeserialization $ do
      v <- modernDeserializationAction x
      modernDeserializationAction $ f v
instance ModernMonad ModernDeserialization where
  getContext = MakeModernDeserialization $ do
    (context, _) <- get
    return context
  putContext context = MakeModernDeserialization $ do
    (_, deserializationContext) <- get
    put (context, deserializationContext)


getDeserializationContext :: ModernDeserialization ModernDeserializationContext
getDeserializationContext = MakeModernDeserialization $ do
  (_, deserializationContext) <- get
  return deserializationContext


putDeserializationContext
  :: ModernDeserializationContext -> ModernDeserialization ()
putDeserializationContext deserializationContext =
  MakeModernDeserialization $ do
    (context, _) <- get
    put (context, deserializationContext)


deserializeData :: ModernDeserialization ([ModernType], [ModernData])
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
  return (Map.elems $ modernContextTypes context,
          catMaybes maybeDatas)


deserializeOneCommand
  :: ModernCommandType
  -> ModernDeserialization (Maybe ModernData)
deserializeOneCommand commandType = do
  context <- getContext
  let knownTypes = modernContextTypes context
  case commandType of
    ModernCommandTypeDatum -> do
      typeHash <- inputDataHash
      case Map.lookup (ModernHash typeHash) knownTypes of
        Nothing -> undefined -- TODO
        Just theType -> do
          datum <- deserializeOneDatum theType
          return $ Just datum
    ModernCommandTypeListType -> do
      contentTypeHash <- inputDataHash
      let contentType =
            case Map.lookup (ModernHash contentTypeHash) knownTypes of
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
                      case Map.lookup (ModernHash itemTypeHash) knownTypes of
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
                      case Map.lookup (ModernHash itemTypeHash) knownTypes of
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
                inputAlign 8
                fieldName <- inputDataUTF8
                inputAlign 8
                fieldTypeHash <- inputDataHash
                let fieldType =
                      case Map.lookup (ModernHash fieldTypeHash) knownTypes of
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
      inputAlign 8
      typeName <- inputDataUTF8
      inputAlign 8
      contentTypeHash <- inputDataHash
      let contentType =
            case Map.lookup (ModernHash contentTypeHash) knownTypes of
              Nothing -> undefined -- TODO
              Just knownType -> knownType
          theType = ModernNamedType (ModernTypeName typeName) contentType
      learnType theType
      return Nothing


deserializeOneDatum
  :: ModernType
  -> ModernDeserialization ModernData
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
  :: ModernType
  -> ModernDeserialization ()
learnType theType = do
  oldContext <- getContext
  let oldKnownTypes = modernContextTypes oldContext
      theHash = computeTypeHash theType
      newKnownTypes = Map.insert theHash theType oldKnownTypes
      newContext = oldContext {
                       modernContextTypes = newKnownTypes
                     }
  putContext newContext


inputCommandBits
  :: Word8
  -> ModernDeserialization Word64
inputCommandBits inputCount = do
  oldContext <- getDeserializationContext
  let oldCount = modernDeserializationContextPendingCommandBitCount oldContext
      oldSource =
	modernDeserializationContextPendingCommandBitSource oldContext
      oldStartingOffset = modernDeserializationContextStartingOffset oldContext
  if inputCount <= oldCount
    then do
      let resultBits = oldSource .&. (shiftL 1 (fromIntegral inputCount) - 1)
          newCount = oldCount - inputCount
          newSource = shiftR oldSource (fromIntegral inputCount)
          newContext =
	    oldContext {
                modernDeserializationContextPendingCommandBitCount = newCount,
                modernDeserializationContextPendingCommandBitSource = newSource
              }
      putDeserializationContext newContext
      return resultBits
    else do
      inputSource <- MakeModernDeserialization $ lift $ deserializeWord
      let newCount = oldCount + 64 - inputCount
          wrappedCount = inputCount - oldCount
          resultHighBits =
            inputSource .&. (shiftL 1 (fromIntegral wrappedCount) - 1)
          resultBits =
            oldSource .|. (shiftL resultHighBits (fromIntegral oldCount))
          newSource = shiftR inputSource (fromIntegral wrappedCount)
          newStartingOffset = oldStartingOffset + 8
          newContext =
	    oldContext {
                modernDeserializationContextPendingCommandBitCount = newCount,
                modernDeserializationContextPendingCommandBitSource =
		  newSource,
                modernDeserializationContextStartingOffset = newStartingOffset
              }
      putDeserializationContext newContext
      return resultBits


inputCommandType
  :: ModernDeserialization (Maybe ModernCommandType)
inputCommandType = do
  bits <- inputCommandBits standardCommandEncodingBitsize
  return $ Map.lookup bits standardCommandDecodings


inputDataHash
  :: ModernDeserialization ByteString
inputDataHash = do
  oldContext <- getDeserializationContext
  let oldStartingOffset = modernDeserializationContextStartingOffset oldContext
  result <- MakeModernDeserialization $ lift $ read 16
  let newStartingOffset = oldStartingOffset + 16
      newContext =
	oldContext {
            modernDeserializationContextStartingOffset = newStartingOffset
          }
  putDeserializationContext newContext
  return result


inputDataWord8
  :: ModernDeserialization Word8
inputDataWord8 = do
  oldContext <- getDeserializationContext
  let oldStartingOffset = modernDeserializationContextStartingOffset oldContext
  result <- MakeModernDeserialization $ lift $ deserializeWord
  let newStartingOffset = oldStartingOffset + 1
      newContext =
	oldContext {
            modernDeserializationContextStartingOffset = newStartingOffset
          }
  putDeserializationContext newContext
  return result


inputDataWord16
  :: ModernDeserialization Word16
inputDataWord16 = do
  oldContext <- getDeserializationContext
  let oldStartingOffset = modernDeserializationContextStartingOffset oldContext
  result <- MakeModernDeserialization $ lift $ deserializeWord
  let newStartingOffset = oldStartingOffset + 2
      newContext =
	oldContext {
            modernDeserializationContextStartingOffset = newStartingOffset
          }
  putDeserializationContext newContext
  return result


inputDataWord32
  :: ModernDeserialization Word32
inputDataWord32 = do
  oldContext <- getDeserializationContext
  let oldStartingOffset = modernDeserializationContextStartingOffset oldContext
  result <- MakeModernDeserialization $ lift $ deserializeWord
  let newStartingOffset = oldStartingOffset + 4
      newContext =
	oldContext {
            modernDeserializationContextStartingOffset = newStartingOffset
          }
  putDeserializationContext newContext
  return result


inputDataWord64
  :: ModernDeserialization Word64
inputDataWord64 = do
  oldContext <- getDeserializationContext
  let oldStartingOffset = modernDeserializationContextStartingOffset oldContext
  result <- MakeModernDeserialization $ lift $ deserializeWord
  let newStartingOffset = oldStartingOffset + 8
      newContext =
	oldContext {
            modernDeserializationContextStartingOffset = newStartingOffset
          }
  putDeserializationContext newContext
  return result


inputDataUTF8
  :: ModernDeserialization ByteString
inputDataUTF8 = do
  oldContext <- getDeserializationContext
  let oldStartingOffset = modernDeserializationContextStartingOffset oldContext
  result <- MakeModernDeserialization $ lift deserializeNullTerminatedText
  let newStartingOffset =
        oldStartingOffset + (fromIntegral $ BS.length result) + 1
      newContext =
	oldContext {
            modernDeserializationContextStartingOffset = newStartingOffset
          }
  putDeserializationContext newContext
  return result


inputAlign
  :: Word64
  -> ModernDeserialization ()
inputAlign alignment = do
  oldContext <- getDeserializationContext
  let oldStartingOffset = modernDeserializationContextStartingOffset oldContext
      misalignment = mod oldStartingOffset alignment
      padLength = if misalignment == 0
                    then 0
                    else alignment - misalignment
      newStartingOffset = oldStartingOffset + padLength
      newContext =
	oldContext {
            modernDeserializationContextStartingOffset = newStartingOffset
          }
  byteString <-
    MakeModernDeserialization $ lift $ read $ fromIntegral padLength
  putDeserializationContext newContext
  mapM_ (\byte ->
           if byte == 0x00
             then return ()
             else error (show byte) -- TODO
        )
        (BS.unpack byteString)


runModernDeserializationFromByteString
  :: ModernContext
  -> ByteString
  -> (ModernDeserialization a)
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
  :: ModernContext
  -> FilePath
  -> (ModernDeserialization a)
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

