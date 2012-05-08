module Data.Modern.Serialization
  (serializeData,
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
import qualified Data.Set as Set
import Data.Word

import Data.Modern.Context
import Data.Modern.Hash
import Data.Modern.Initial
import Data.Modern.Types


serializeData
  :: (ModernFormat format)
  => ModernData
  -> ModernSerialization format ()
serializeData item = do
  addTypeToContext $ dataType item
  commandDatum item


addTypeToContext
  :: (ModernFormat format)
  => ModernType
  -> ModernSerialization format ()
addTypeToContext theType = do
  context <- getContext
  if typeInContext theType context
    then return ()
    else do
      mapM_ addTypeToContext (Set.elems $ typeContentTypes theType)
      case theType of
        ModernListType contentType -> do
	  let contentTypeHash = computeTypeHash contentType
          outputCommandType ModernCommandTypeListType
          outputDataHash contentTypeHash
        ModernTupleType maybeContentTypes -> do
	  let contentTypes = maybe [] Array.elems maybeContentTypes
	      contentTypeHashes = map computeTypeHash contentTypes
          outputCommandType ModernCommandTypeTupleType
          outputDataNat (genericLength contentTypeHashes :: Word64)
          mapM_ outputDataHash contentTypeHashes
        ModernUnionType maybePossibilities -> do
          let possibilities = maybe [] (Array.elems . snd) maybePossibilities
          outputCommandType ModernCommandTypeUnionType
          outputDataNat (genericLength possibilities :: Word64)
          mapM_ (\possibilityType -> do
                   let possibilityTypeHash = computeTypeHash possibilityType
                   outputDataHash possibilityTypeHash)
                possibilities
        ModernStructureType maybeFields -> do
	  let fields = maybe [] Array.elems maybeFields
          outputCommandType ModernCommandTypeStructureType
          outputDataNat (genericLength fields :: Word64)
          mapM_ (\(ModernFieldName fieldName, fieldType) -> do
                   let fieldTypeHash = computeTypeHash fieldType
                   outputDataUTF8 fieldName
                   outputAlign 8
                   outputDataHash fieldTypeHash)
                fields
        ModernNamedType (ModernTypeName name) contentType -> do
	  let contentTypeHash = computeTypeHash contentType
          outputCommandType ModernCommandTypeNamedType
          outputDataUTF8 name
          outputAlign 8
          outputDataHash contentTypeHash
        _ -> error "Hm." -- TODO


commandDatum
  :: (ModernFormat format)
  => ModernData
  -> ModernSerialization format ()
commandDatum datum = do
  let theType = dataType datum
      theTypeHash = computeTypeHash theType
  outputCommandType ModernCommandTypeDatum
  outputDataHash theTypeHash
  outputSynchronize
  let helper datum = do
        case datum of
          ModernDataInt8 value -> do
            outputAlign 1
            outputDataNat (fromIntegral value :: Word8)
          ModernDataInt16 value -> do
            outputAlign 2
            outputDataNat (fromIntegral value :: Word16)
          ModernDataInt32 value -> do
            outputAlign 4
            outputDataNat (fromIntegral value :: Word32)
          ModernDataInt64 value -> do
            outputAlign 8
            outputDataNat (fromIntegral value :: Word64)
          ModernDataNat8 value -> do
            outputAlign 1
            outputDataNat value
          ModernDataNat16 value -> do
            outputAlign 2
            outputDataNat value
          ModernDataNat32 value -> do
            outputAlign 4
            outputDataNat value
          ModernDataNat64 value -> do
            outputAlign 8
            outputDataNat value
          ModernDataFloat32 value -> do
            outputAlign 4
            undefined
          ModernDataFloat64 value -> do
            outputAlign 8
            undefined
          ModernDataUTF8 value -> do
            outputAlign 8
	    outputDataUTF8 value
            outputAlign 8
          ModernDataBlob value -> do
            outputAlign 8
            outputDataBlob value
            outputAlign 8
          ModernDataList _ values -> do
            outputAlign 8
            let (start, end) = Array.bounds values
            outputDataNat $ end - start + 1
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

