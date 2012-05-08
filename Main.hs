module Main (main) where

import Data.Array
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.Set as Set

import Data.Modern


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
                                (1, (fromString "value", ModernNat64Type))]
      listOfStringsType = ModernListType ModernUTF8Type
      tupleType =
        ModernTupleType
         $ Just $ array (0, 3) [(0, ModernNat8Type),
                                (1, ModernNat16Type),
                                (2, ModernNat32Type),
                                (3, ModernNat64Type)]
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
                        (1, ModernDataNat64 0xC90FDAA22168C000)]
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
                                (1, (fromString "value", ModernNat64Type))]
      listOfStringsType = ModernListType ModernUTF8Type
      tupleType =
        ModernTupleType
         $ Just $ array (0, 3) [(0, ModernNat8Type),
                                (1, ModernNat16Type),
                                (2, ModernNat32Type),
                                (3, ModernNat64Type)]
      unionType =
        ModernUnionType
         $ Just (2, array (0, 2) [(0, itemType),
                                  (1, listOfStringsType),
                                  (2, tupleType)])
      schema = [itemType, listOfStringsType, tupleType, unionType]
      context = ensureTypesInContext (Set.singleton unionType) initialContext
  putStr $ documentSchema context

