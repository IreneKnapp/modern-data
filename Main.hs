module Main (main) where

import Data.Modern

import Data.Array
import qualified Data.ByteString.UTF8 as UTF8


main :: IO ()
main = do
  testDocumentation


testInput :: IO ()
testInput = do
  let context = initialContext
  result <- deserializeFromExplicatoryFile context "input.txt"
  context <- case result of
               Left failure -> do
                 putStrLn $ show failure
                 return context
               Right (context, value) -> do
                 return context
  return ()


testOutput :: IO ()
testOutput = do
  let itemType = ModernNamedType (fromString "Item") structureType
      structureType =
        ModernStructureType
         $ Just $ array (0, 1) [(0, (fromString "key", ModernUTF8Type)),
                                (1, (fromString "value", ModernWord64Type))]
      listOfStringsType = ModernListType ModernUTF8Type
      tupleType =
        ModernTupleType
         $ Just $ array (0, 3) [(0, ModernWord8Type),
                                (1, ModernWord16Type),
                                (2, ModernWord32Type),
                                (3, ModernWord64Type)]
      unionType =
        ModernUnionType
         $ Just (2, array (0, 2) [(0, itemType),
                                  (1, listOfStringsType),
                                  (2, tupleType)])
      schema = [itemType, listOfStringsType, tupleType, unionType]
      datum =
        ModernDataUnion unionType 0
         $ ModernDataNamed itemType
            $ ModernDataStructure structureType
               $ array (0, 1)
                       [(0, ModernDataUTF8 $ UTF8.fromString "Pi"),
                        (1, ModernDataWord64 0xC90FDAA22168C000)]
      context = initialContext
  result <- serializeToExplicatoryFile context datum "output.txt"
  context <- case result of
               Left failure -> do
                 putStrLn $ show failure
                 return context
               Right context -> return context
  return ()


testDocumentation = do
  let itemType = ModernNamedType (fromString "Item") structureType
      structureType =
        ModernStructureType
         $ Just $ array (0, 1) [(0, (fromString "key", ModernUTF8Type)),
                                (1, (fromString "value", ModernWord64Type))]
      listOfStringsType = ModernListType ModernUTF8Type
      tupleType =
        ModernTupleType
         $ Just $ array (0, 3) [(0, ModernWord8Type),
                                (1, ModernWord16Type),
                                (2, ModernWord32Type),
                                (3, ModernWord64Type)]
      unionType =
        ModernUnionType
         $ Just (2, array (0, 2) [(0, itemType),
                                  (1, listOfStringsType),
                                  (2, tupleType)])
      schema = [itemType, listOfStringsType, tupleType, unionType]
      datum =
        ModernDataUnion unionType 0
         $ ModernDataNamed itemType
            $ ModernDataStructure structureType
               $ array (0, 1)
                       [(0, ModernDataUTF8 $ UTF8.fromString "Pi"),
                        (1, ModernDataWord64 0xC90FDAA22168C000)]
      context = initialContext
  putStr $ documentSchema context

