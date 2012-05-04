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


serializeData
  :: (ModernFormat format)
  => [ModernData]
  -> ModernSerialization format ()
serializeData items = do
  mapM_ ensureTypeInContext $ map dataType items
  mapM_ commandDatum items


ensureTypeInContext
  :: (ModernFormat format)
  => ModernType
  -> ModernSerialization format ()
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


commandDatum
  :: (ModernFormat format)
  => ModernData
  -> ModernSerialization format ()
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
  :: (ModernFormat format)
  => ModernHash
  -> ModernSerialization format ()
commandListType (ModernHash contentTypeHash) = do
  outputCommandType ModernCommandTypeListType
  outputData contentTypeHash


commandTupleType
  :: (ModernFormat format)
  => [ModernHash]
  -> ModernSerialization format ()
commandTupleType contentTypeHashes = do
  outputCommandType ModernCommandTypeTupleType
  outputDataWord (genericLength contentTypeHashes :: Word64)
  mapM_ (\(ModernHash contentTypeHash) -> do
           outputData contentTypeHash)
        contentTypeHashes


commandUnionType
  :: (ModernFormat format)
  => [ModernHash]
  -> ModernSerialization format ()
commandUnionType possibilities = do
  outputCommandType ModernCommandTypeUnionType
  outputDataWord (genericLength possibilities :: Word64)
  mapM_ (\(ModernHash possibility) -> outputData possibility)
        possibilities


commandStructureType
  :: (ModernFormat format)
  => [(ModernFieldName, ModernHash)]
  -> ModernSerialization format ()
commandStructureType fields = do
  outputCommandType ModernCommandTypeStructureType
  outputDataWord (genericLength fields :: Word64)
  mapM_ (\(ModernFieldName fieldName, ModernHash fieldTypeHash) -> do
           outputData $ BS.concat [fieldName, BS.pack [0x00]]
           outputAlign 8
           outputData fieldTypeHash)
        fields


commandNamedType
  :: (ModernFormat format)
  => ModernTypeName
  -> ModernHash
  -> ModernSerialization format ()
commandNamedType (ModernTypeName name) (ModernHash contentTypeHash) = do
  outputCommandType ModernCommandTypeNamedType
  outputData $ BS.concat [name, BS.pack [0x00]]
  outputAlign 8
  outputData contentTypeHash


runModernSerializationToByteString
  :: (ModernFormat format)
  => ModernContext
  -> (ModernSerialization format a)
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
  :: (ModernFormat format)
  => ModernContext
  -> FilePath
  -> (ModernSerialization format a)
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


wrapModernSerialization
  :: (ModernFormat format)
  => ModernSerialization format a
  -> ModernSerialization format a
wrapModernSerialization action = do
  result <- action
  outputSynchronize
  return result

