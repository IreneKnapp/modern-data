module Main (main) where

import Data.Modern


main :: IO ()
main = do
  let itemType = ModernNamedType (fromString "Item") structureType
      structureType =
        ModernStructureType [(fromString "key", ModernUTF8Type),
			     (fromString "value", ModernWord64Type)]
      context = initialContext
  putStrLn $ textualSchema [itemType]
  result <- runModernSerializationToFile context "output.txt" $ do
	      ensureTypeInContext itemType
  context <- case result of
               Left failure -> do
		 putStrLn $ show failure
		 return context
               Right (context, ()) -> return context
  return ()

