{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import System.FilePath (FilePath)

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified System.Environment as IO
import qualified System.Exit as IO
import qualified System.FilePath as IO
import qualified System.Directory as IO
import qualified System.IO as IO
import qualified Text.XML as XML

import Control.Monad.Identity
import Data.List
import Data.Maybe
import Data.String
import Text.XML.Cursor


data Section =
  Section {
      sectionPath :: [(Int, T.Text)],
      sectionType :: SectionType,
      sectionChildren :: [Section]
    }


data SectionType
  = Numbered
  | Lettered
  | Anonymous Bool
  | InParent Bool


main :: IO ()
main = do
  arguments <- IO.getArgs
  case arguments of
    [inputWrapperPath, outputDirectoryPath] -> do
      inputExists <- IO.doesDirectoryExist inputWrapperPath
      case inputExists of
        True -> return ()
        False -> do
          putStrLn $ "Couldn't find " ++ show inputWrapperPath ++ "."
          IO.exitFailure
      outputExistsAsFile <- IO.doesFileExist outputDirectoryPath
      outputExistsAsDirectory <- IO.doesDirectoryExist outputDirectoryPath
      case outputExistsAsFile || outputExistsAsDirectory of
        True -> do
          putStrLn $ "Refusing to overwrite " ++ show outputDirectoryPath
                     ++ "."
          IO.exitFailure
        False -> return ()
      process inputWrapperPath outputDirectoryPath
    _ -> do
      putStrLn $ "Usage: docgen Input.scriv output/"
      IO.exitFailure


process :: FilePath -> FilePath -> IO ()
process inputWrapperPath outputDirectoryPath = do
  let inputBinderPath =
        fromString $ inputWrapperPath ++ "/"
                     ++ (IO.takeBaseName inputWrapperPath) ++ ".scrivx"
  inputBinder <- XML.readFile XML.def inputBinderPath
  let labels =
       Map.fromList
         $ fromDocument inputBinder
         $/ element "LabelSettings"
         &/ element "Labels"
         &/ element "Label"
         &| (\itemCursor ->
               let key = T.concat $ attribute "ID" itemCursor
                   name = T.concat $ itemCursor $/ content
               in (key, name))
      statuses =
       Map.fromList
         $ fromDocument inputBinder
         $/ element "StatusSettings"
         &/ element "StatusItems"
         &/ element "Status"
         &| (\itemCursor ->
               let key = attribute "ID" itemCursor
                   name = T.concat $ itemCursor $/ content
               in (key, name))
  binder <-
    mapM (\(index, binderItem) -> do
             collectBinderItem labels binderItem
                               [(index, binderItemTitle binderItem)])
         (zip [1 ..] $ (fromDocument inputBinder)
                     $/ element "Binder"
                     &/ element "BinderItem"
                     &| node)
  draft <- case computeDraft binder of
             Nothing -> do
               putStrLn $ "Draft not found by that name in the binder."
               IO.exitFailure
             Just draft -> return draft
  let processSection level section = do
        putStrLn $ (take (level * 4) (repeat ' '))
                   ++ (show $ sectionPath section)
        mapM_ (processSection (level + 1)) (sectionChildren section)
  processSection 0 draft


binderItemTitle :: XML.Node -> T.Text
binderItemTitle binderItem =
  T.concat $ (fromNode binderItem) $/ element "Title" &/ content


collectBinderItem
  :: Map.Map T.Text T.Text -> XML.Node -> [(Int, T.Text)] -> IO Section
collectBinderItem labels binderItem path = do
  children <-
    mapM (\(index, child) -> do
              collectBinderItem labels child
                                (path ++ [(index, binderItemTitle child)]))
         (zip [1 ..] ((fromNode binderItem)
                      $/ element "Children"
                      &/ element "BinderItem"
                      &| node))
  let labelID = T.concat ((fromNode binderItem)
                          $/ element "MetaData"
                          &/ element "LabelID"
                          &/ content)
      maybeLabel = Map.lookup labelID labels
      sectionType' =
        case maybeLabel of
          Just "Overview" -> InParent False
          Just "Synopsis" -> InParent True
          Just "Reference" -> Numbered
          Just "Tutorial" -> Numbered
          Just "Parameters" -> InParent True
          Just "Semantics" -> InParent True
          Just "Xref" -> InParent True
          Just "Manpage" -> Anonymous True
          Just "Appendix" -> Lettered
          _ -> Numbered
  return Section {
             sectionPath = path,
             sectionType = sectionType',
             sectionChildren = children
           }


visitSectionTree :: (Monad m) => ([a] -> Section -> m a) -> Section -> m a
visitSectionTree action section = do
  childResults <- mapM (visitSectionTree action) (sectionChildren section)
  action childResults section


findPreorder
  :: (Monad m) => (Section -> m Bool) -> Section -> m (Maybe Section)
findPreorder predicate section =
  visitSectionTree (\childResults section -> do
                      foundHere <- predicate section
                      case foundHere of
                        True -> return (Just section)
                        False -> return $ listToMaybe $ catMaybes childResults)
                   section


computeDraft :: [Section] -> Maybe Section
computeDraft binder =
  let maybeDraft =
        foldl' (\maybeResult section ->
                  case maybeResult of
                    Nothing -> runIdentity $ findPreorder isDraft section
                    Just _ -> maybeResult)
                Nothing
                binder
      isDraft section =
        let (_, title) = last (sectionPath section)
        in return $ title == "Draft"
      stripPathPrefix length section =
        Section {
            sectionPath = drop length (sectionPath section),
            sectionType = sectionType section,
            sectionChildren =
              map (stripPathPrefix length) (sectionChildren section)
          }
  in fmap (\draft -> stripPathPrefix (length $ sectionPath draft) draft)
          maybeDraft