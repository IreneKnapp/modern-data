module Data.Modern.Serialization
  (serializeData,
   ensureTypeInContext,
   runModernSerializationToByteString,
   runModernSerializationToFile)
  where

import BinaryFiles hiding (getContext)
import Control.Monad.State.Strict
import qualified Data.Array as Array
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List
import qualified Data.Map as Map
import Data.Word

import Data.Modern.Hash
import Data.Modern.Initial
import Data.Modern.Types


data ModernSerializationContext =
  ModernSerializationContext {
      modernSerializationContextPendingCommandBitCount :: Word8,
      modernSerializationContextPendingCommandBitSource :: Word64,
      modernSerializationContextPendingCommandWords :: [Word64],
      modernSerializationContextPendingData :: [PendingData],
      modernSerializationContextStartingOffset :: Word64
    }


initialSerializationContext :: ModernSerializationContext
initialSerializationContext =
  ModernSerializationContext {
      modernSerializationContextPendingCommandBitCount = 0,
      modernSerializationContextPendingCommandBitSource = 0,
      modernSerializationContextPendingCommandWords = [],
      modernSerializationContextPendingData = [],
      modernSerializationContextStartingOffset = 0
    }


data ModernSerialization a =
  MakeModernSerialization {
      modernSerializationAction
        :: StateT (ModernContext, ModernSerializationContext)
		  (ContextualSerialization Endianness) a
    }
instance Monad ModernSerialization where
  return a = MakeModernSerialization $ return a
  x >>= f =
    MakeModernSerialization $ do
      v <- modernSerializationAction x
      modernSerializationAction $ f v
instance ModernMonad ModernSerialization where
  getContext = MakeModernSerialization $ do
    (context, _) <- get
    return context
  putContext context = MakeModernSerialization $ do
    (_, serializationContext) <- get
    put (context, serializationContext)


getSerializationContext :: ModernSerialization ModernSerializationContext
getSerializationContext = MakeModernSerialization $ do
  (_, serializationContext) <- get
  return serializationContext


putSerializationContext :: ModernSerializationContext -> ModernSerialization ()
putSerializationContext serializationContext = MakeModernSerialization $ do
  (context, _) <- get
  put (context, serializationContext)


serializeData :: [ModernData] -> ModernSerialization ()
serializeData items = do
  mapM_ ensureTypeInContext $ map dataType items
  mapM_ commandDatum items


ensureTypeInContext
  :: ModernType
  -> ModernSerialization ()
ensureTypeInContext theType = do
  context <- getContext
  let hash = computeTypeHash theType
      knownTypes = modernContextTypes context
  if Map.member hash knownTypes
    then return ()
    else do
      let newKnownTypes = Map.insert hash theType knownTypes
          newContext = context {
                           modernContextTypes = newKnownTypes
                         }
      putContext newContext
      case theType of
        ModernListType contentType -> do
          ensureTypeInContext contentType
          let contentTypeHash = computeTypeHash contentType
          commandListType contentTypeHash
        ModernTupleType maybeContentTypes -> do
          contentTypeHashes <-
            case maybeContentTypes of
              Nothing -> return []
              Just contentTypes -> do
                mapM_ ensureTypeInContext $ Array.elems contentTypes
                return $ map computeTypeHash $ Array.elems contentTypes
          commandTupleType contentTypeHashes
        ModernUnionType maybeContentTypes -> do
          contentTypeHashes <-
            case maybeContentTypes of
              Nothing -> return []
              Just (_, contentTypes) -> do
                mapM_ ensureTypeInContext $ Array.elems contentTypes
                return $ map computeTypeHash $ Array.elems contentTypes
          commandUnionType contentTypeHashes
        ModernStructureType maybeFields -> do
          fieldTypeHashes <-
            case maybeFields of
              Nothing -> return []
              Just fields -> do
                mapM_ ensureTypeInContext $ map snd $ Array.elems fields
                return $ map (\(fieldName, fieldType) ->
                                 (fieldName, computeTypeHash fieldType))
                             $ Array.elems fields
          commandStructureType fieldTypeHashes
        ModernNamedType name contentType -> do
          ensureTypeInContext contentType
          let contentTypeHash = computeTypeHash contentType
          commandNamedType name contentTypeHash


commandSynchronize
  :: ModernSerialization ()
commandSynchronize = do
  outputCommandType ModernCommandTypeSynchronize
  outputSynchronize


commandDatum
  :: ModernData
  -> ModernSerialization ()
commandDatum datum = do
  let theType = dataType datum
      ModernHash theTypeHash = computeTypeHash theType
  ensureTypeInContext theType
  outputCommandType ModernCommandTypeDatum
  outputData theTypeHash
  outputSynchronize
  let helper datum = do
        case datum of
          ModernDataInt8 value -> do
            outputAlign 1
            outputDataWord (fromIntegral value :: Word8)
          ModernDataInt16 value -> do
            outputAlign 2
            outputDataWord (fromIntegral value :: Word16)
          ModernDataInt32 value -> do
            outputAlign 4
            outputDataWord (fromIntegral value :: Word32)
          ModernDataInt64 value -> do
            outputAlign 8
            outputDataWord (fromIntegral value :: Word64)
          ModernDataWord8 value -> do
            outputAlign 1
            outputDataWord value
          ModernDataWord16 value -> do
            outputAlign 2
            outputDataWord value
          ModernDataWord32 value -> do
            outputAlign 4
            outputDataWord value
          ModernDataWord64 value -> do
            outputAlign 8
            outputDataWord value
          ModernDataFloat value -> do
            outputAlign 4
            undefined
          ModernDataDouble value -> do
            outputAlign 8
            undefined
          ModernDataUTF8 value -> do
            outputAlign 8
            outputData $ BS.concat [value, BS.pack [0x00]]
            outputAlign 8
          ModernDataBlob value -> do
            outputAlign 8
            outputData value
            outputAlign 8
          ModernDataList _ values -> do
            outputAlign 8
            let (start, end) = Array.bounds values
            outputDataWord $ end - start + 1
            mapM_ helper $ Array.elems values
          ModernDataTuple _ values -> do
            mapM_ helper $ Array.elems values
          ModernDataUnion theType index value -> do
            case theType of
              ModernUnionType Nothing -> do
                undefined -- TODO
              ModernUnionType (Just (bitCount, _)) -> do
                outputSynchronize
                outputCommandBits bitCount index
                helper value
              _ -> undefined -- TODO
          ModernDataStructure _ values -> do
            mapM_ helper $ Array.elems values
          ModernDataNamed _ value -> do
            helper value
  helper datum


commandListType
  :: ModernHash
  -> ModernSerialization ()
commandListType (ModernHash contentTypeHash) = do
  outputCommandType ModernCommandTypeListType
  outputData contentTypeHash


commandTupleType
  :: [ModernHash]
  -> ModernSerialization ()
commandTupleType contentTypeHashes = do
  outputCommandType ModernCommandTypeTupleType
  outputDataWord (genericLength contentTypeHashes :: Word64)
  mapM_ (\(ModernHash contentTypeHash) -> do
           outputData contentTypeHash)
        contentTypeHashes


commandUnionType
  :: [ModernHash]
  -> ModernSerialization ()
commandUnionType possibilities = do
  outputCommandType ModernCommandTypeUnionType
  outputDataWord (genericLength possibilities :: Word64)
  mapM_ (\(ModernHash possibility) -> outputData possibility)
        possibilities


commandStructureType
  :: [(ModernFieldName, ModernHash)]
  -> ModernSerialization ()
commandStructureType fields = do
  outputCommandType ModernCommandTypeStructureType
  outputDataWord (genericLength fields :: Word64)
  mapM_ (\(ModernFieldName fieldName, ModernHash fieldTypeHash) -> do
           outputData $ BS.concat [fieldName, BS.pack [0x00]]
           outputAlign 8
           outputData fieldTypeHash)
        fields


commandNamedType
  :: ModernTypeName
  -> ModernHash
  -> ModernSerialization ()
commandNamedType (ModernTypeName name) (ModernHash contentTypeHash) = do
  outputCommandType ModernCommandTypeNamedType
  outputData $ BS.concat [name, BS.pack [0x00]]
  outputAlign 8
  outputData contentTypeHash


wrapModernSerialization
  :: ModernSerialization a
  -> ModernSerialization a
wrapModernSerialization action = do
  result <- action
  outputSynchronize
  return result


outputSynchronize
  :: ModernSerialization ()
outputSynchronize = do
  oldContext <- getSerializationContext
  let oldCount = modernSerializationContextPendingCommandBitCount oldContext
      oldSource = modernSerializationContextPendingCommandBitSource oldContext
      oldPendingCommandWords =
	modernSerializationContextPendingCommandWords oldContext
      oldPendingData = modernSerializationContextPendingData oldContext
      oldStartingOffset = modernSerializationContextStartingOffset oldContext
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
                  $ lift $ write $ BS.pack $ genericTake padLength $ repeat 0x00  
                 return $ startingOffset + padLength)
          oldStartingOffset
          oldPendingData
  let newContext = oldContext {
                       modernSerializationContextPendingCommandBitCount = 0,
                       modernSerializationContextPendingCommandBitSource = 0,
                       modernSerializationContextPendingCommandWords = [],
                       modernSerializationContextPendingData = [],
                       modernSerializationContextStartingOffset =
			 newStartingOffset
                     }
  putSerializationContext newContext


outputCommandBits
  :: Word8
  -> Word64
  -> ModernSerialization ()
outputCommandBits outputCount outputSource = do
  oldContext <- getSerializationContext
  let oldCount = modernSerializationContextPendingCommandBitCount oldContext
      oldSource = modernSerializationContextPendingCommandBitSource oldContext
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
      oldStartingOffset = modernSerializationContextStartingOffset oldContext
      newStartingOffset =
        case immediateOutput of
          Nothing -> oldStartingOffset
          Just _ -> oldStartingOffset + 8
      oldPendingCommandWords =
	modernSerializationContextPendingCommandWords oldContext
      newPendingCommandWords =
        case immediateOutput of
          Nothing -> oldPendingCommandWords
          Just word -> oldPendingCommandWords ++ [word]
      newContext = oldContext {
                       modernSerializationContextPendingCommandBitCount =
			 newCount,
                       modernSerializationContextPendingCommandBitSource =
			 newSource,
                       modernSerializationContextPendingCommandWords =
                         newPendingCommandWords,
                       modernSerializationContextStartingOffset =
			 newStartingOffset
                     }
  putSerializationContext newContext


outputCommandType
  :: ModernCommandType
  -> ModernSerialization ()
outputCommandType commandType = do
  case Map.lookup commandType standardCommandEncodings of
    Just source -> outputCommandBits standardCommandEncodingBitsize source
    Nothing -> return ()


outputData
  :: ByteString
  -> ModernSerialization ()
outputData byteString = do
  oldContext <- getSerializationContext
  let oldPendingData = modernSerializationContextPendingData oldContext
      newPendingData = oldPendingData ++ [PendingBytes byteString]
      newContext = oldContext {
                       modernSerializationContextPendingData = newPendingData
                     }
  putSerializationContext newContext


outputDataWord
  :: (Bits word, Integral word, Num word)
  => word
  -> ModernSerialization ()
outputDataWord word = do
  let result = runSerializationToByteString $ withContext LittleEndian
                $ serializeWord word
  case result of
    Right ((), byteString) -> outputData byteString


outputAlign
  :: Word64
  -> ModernSerialization ()
outputAlign alignment = do
  oldContext <- getSerializationContext
  let oldPendingData = modernSerializationContextPendingData oldContext
      newPendingData = oldPendingData ++ [PendingAlignmentMark alignment]
      newContext = oldContext {
                       modernSerializationContextPendingData = newPendingData
                     }
  putSerializationContext newContext


runModernSerializationToByteString
  :: ModernContext
  -> (ModernSerialization a)
  -> Either (Int, [(Int, String)], SomeSerializationFailure)
            (ByteString, ModernContext, a)
runModernSerializationToByteString context action =
  case runSerializationToByteString $ do
         withContext LittleEndian $ do
           runStateT (modernSerializationAction
                       $ wrapModernSerialization action)
                     (context, initialSerializationContext) of
    Left failure -> Left failure
    Right ((result, (context', _)), output) -> Right (output, context', result)


runModernSerializationToFile
  :: ModernContext
  -> FilePath
  -> (ModernSerialization a)
  -> IO (Either (Int, [(Int, String)], SomeSerializationFailure)
                (ModernContext, a))
runModernSerializationToFile context filePath action = do
  eitherFailureResult <-
    flip runSerializationToFile filePath $ do
      withContext LittleEndian $ do
        runStateT (modernSerializationAction
                    $ wrapModernSerialization action)
		  (context, initialSerializationContext)
  return $ case eitherFailureResult of
             Left failure -> Left failure
             Right (result, (context', _)) -> Right (context', result)

