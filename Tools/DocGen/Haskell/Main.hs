{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import System.FilePath (FilePath)

import qualified Data.Aeson as JSON
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Digest.Murmur3 as Hash
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified System.Environment as IO
import qualified System.Exit as IO
import qualified System.FilePath as IO
import qualified System.Directory as IO
import qualified System.IO as IO
import qualified Text.XML as XML

import Control.Monad.Identity
import Data.Char
import Data.List
import Data.Maybe
import Data.String
import Numeric
import Text.XML.Cursor

import Debug.Trace


data Section =
  Section {
      sectionPath :: [(Int, T.Text)],
      sectionType :: SectionType,
      sectionDocumentID :: T.Text,
      sectionChildren :: [Section]
    }


data SectionType
  = Numbered
  | Lettered
  | Anonymous
  | InParent Bool


data OutputSection =
  OutputSection {
      outputSectionID :: T.Text,
      outputSectionNumber :: Maybe T.Text,
      outputSectionTitle :: Maybe [(T.Text, T.Text)],
      outputSectionBody :: [BodyItem],
      outputSectionChildren :: [OutputSection]
    }


data FlattenedOutputSection =
  FlattenedOutputSection {
      flattenedOutputSectionTitle :: T.Text,
      flattenedOutputSectionNavigation :: [[TextItem]],
      flattenedOutputSectionBody :: [BodyItem]
    }
instance JSON.ToJSON FlattenedOutputSection where
  toJSON section =
    JSON.object
      ["title" JSON..= (JSON.toJSON $ flattenedOutputSectionTitle section),
       "navigation" JSON..= 
         (JSON.toJSON $ flattenedOutputSectionNavigation section),
       "body" JSON..= (JSON.toJSON $ flattenedOutputSectionBody section)]


data BodyItem
  = Header [TextItem]
  | Paragraph [TextItem]
instance JSON.ToJSON BodyItem where
  toJSON (Header items) =
    JSON.toJSON $ JSON.String "header" : map JSON.toJSON items
  toJSON (Paragraph items) =
    JSON.toJSON $ JSON.String "paragraph" : map JSON.toJSON items 

data TextItem
  = Link T.Text [TextItem]
  | Text T.Text
instance JSON.ToJSON TextItem where
  toJSON (Link identifier items) =
    JSON.toJSON $ JSON.String "link"
                  : JSON.String identifier
                  : map JSON.toJSON items
  toJSON (Text text) = JSON.String text


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
             Just draft -> getOutputDraft draft
           >>= return . computeFlattenedOutput Nothing
  IO.createDirectory outputDirectoryPath
  LBS.writeFile (outputDirectoryPath ++ "/content.json")
                (JSON.encode
                  $ JSON.object
                    ["toc" JSON..= (JSON.String $ fst $ head draft),
                     "sections" JSON..=
                       (JSON.object $ map (uncurry (JSON..=)) draft)])


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
          Just "Manpage" -> Anonymous
          Just "Appendix" -> Lettered
          _ -> Numbered
      documentID = T.concat ((fromNode binderItem)
                             $| attribute "ID")
  return Section {
             sectionPath = path,
             sectionType = sectionType',
             sectionDocumentID = documentID,
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
            sectionDocumentID = sectionDocumentID section,
            sectionChildren =
              map (stripPathPrefix length) (sectionChildren section)
          }
  in fmap (\draft -> stripPathPrefix (length $ sectionPath draft) draft)
          maybeDraft


getOutputDraft :: Section -> IO OutputSection
getOutputDraft draft = do
  (result, _) <- getOutputSection True Nothing 1 [] draft
  return result


getOutputSection
  :: Bool -> Maybe T.Text -> Int -> [(T.Text, T.Text)] -> Section
  -> IO (OutputSection, Bool)
getOutputSection isRoot numberSoFar nextNumberPart titleSoFar section = do
  let number =
        if isRoot
          then Nothing
          else case sectionType section of
                 Numbered ->
                   Just $ T.concat [fromMaybe "" numberSoFar,
                                    T.pack $ show nextNumberPart,
                                    "."]
                 Lettered ->
                   Just $ T.concat
                     [fromMaybe "" numberSoFar,
                      T.pack [chr (ord 'A' + nextNumberPart - 1)],
                      "."]
                 Anonymous -> Nothing
                 InParent _ -> Nothing
      identifier = computeIdentifierHash $ sectionDocumentID section
      finalTitleComponent =
        case sectionPath section of
          [] -> Nothing
          path -> Just (identifier, snd $ last path)
      initialTitle =
        case sectionType section of
          InParent False -> Nothing
          InParent True -> fmap (\item -> [item]) finalTitleComponent
          Anonymous -> fmap (\item -> [item]) finalTitleComponent
          _ -> case titleSoFar ++ maybeToList finalTitleComponent of
                 [] -> Nothing
                 result -> Just result
      inParent =
        case sectionType section of
          InParent _ -> True
          _ -> False
      childNumber =
        if inParent
          then numberSoFar
          else number
  (_, _, title, children, body) <-
        foldM (\(indexSoFar, letteredModeSoFar, titleSoFar, childrenSoFar,
                 bodySoFar)
                childSection -> do
                  let (indexHere, letteredMode) =
                        case sectionType section of
                          Lettered -> if letteredModeSoFar
                                        then (indexSoFar, True)
                                        else (1, True)
                          _ -> (indexSoFar, letteredModeSoFar)
                  (child, childInLine) <-
                    getOutputSection False
                                     childNumber
                                     indexSoFar
                                     (fromMaybe [] titleSoFar)
                                     childSection
                  let title =
                        case titleSoFar of
                          Just _ -> titleSoFar
                          Nothing -> if childInLine
                                       then outputSectionTitle child
                                       else Nothing
                      body =
                        if childInLine
                          then bodySoFar ++ outputSectionBody child
                          else
                            let header =
                                  case (outputSectionNumber child,
                                        outputSectionTitle child) of
                                    (Just number, Just title) ->
                                      [Header
                                        [Link (snd $ last title)
                                              [Text $ T.concat
                                                [number, " ",
                                                 T.intercalate titleSeparator
                                                   $ map snd title]]]]
                                    (Just number, Nothing) ->
                                      [Header [Text number]]
                                    (Nothing, Just title) ->
                                      [Header
                                        [Link (snd $ last title)
                                              [Text $ T.intercalate
                                                titleSeparator
                                                $ map snd title]]]
                                    (Nothing, Nothing) -> [Header [Text "?"]]
                            in bodySoFar ++ header
                      children =
                        if childInLine
                          then childrenSoFar
                          else childrenSoFar ++ [child]
                      index =
                        if childInLine
                          then indexHere
                          else indexHere + 1
                  return (index, letteredMode, title, children, body))
              (1, False, initialTitle, [], [])
              (sectionChildren section)
  let bodyPrefix =
        if inParent
          then case title of
                 Nothing -> []
                 Just title ->
                   [Header
                     $ concat [map (\(identifier, name) ->
                                      Link identifier [Text name])
                                   (init title),
                               [Text $ snd $ last title]]]
          else []
  return (OutputSection {
              outputSectionID = identifier,
              outputSectionNumber = number,
              outputSectionTitle = title,
              outputSectionBody = bodyPrefix ++ body,
              outputSectionChildren = children
           }, inParent)


computeIdentifierHash :: T.Text -> T.Text
computeIdentifierHash documentID =
  T.pack
    $ concatMap (\byte -> let hex = showHex byte ""
                              padding = take (2 - length hex) (repeat '0')
                          in hex ++ padding)
    $ BS.unpack $ BS.take 4
    $ Hash.asByteString $ Hash.hash
    $ T.encodeUtf8 documentID


computeFlattenedOutput
  :: Maybe [(T.Text, T.Text)]
  -> OutputSection
  -> [(T.Text, FlattenedOutputSection)]
computeFlattenedOutput maybeParentTitle section =
  [(outputSectionID section,
    FlattenedOutputSection {
        flattenedOutputSectionTitle =
          case (outputSectionNumber section, outputSectionTitle section) of
            (Just number, Just title) ->
              T.concat [number,
                        " ",
                        T.intercalate titleSeparator $ map snd title]
            (Just number, Nothing) -> number
            (Nothing, Just title) ->
              T.intercalate titleSeparator $ map snd title
            (Nothing, Nothing) -> "Table of Contents",
        flattenedOutputSectionNavigation =
          let titlePartsExceptLast title =
                titleParts (init title) ++ [Text $ snd $ last title]
              titleParts title =
                map (\(identifier, name) ->
                         Link identifier [Text name])
                    title
          in case (outputSectionNumber section, outputSectionTitle section) of
               (Just number, Just [(_, properTitle)]) ->
                 (case maybeParentTitle of
                    Nothing -> []
                    Just parentTitle ->
                      [intersperse (Text " ") (titleParts parentTitle)])
                 ++ [[Text number, Text " ", Text properTitle]]
               (Nothing, Just [(_, properTitle)]) ->
                 (case maybeParentTitle of
                    Nothing -> []
                    Just parentTitle ->
                      [intersperse (Text " ") (titleParts parentTitle)])
                 ++ [[Text properTitle]]
               (Just number, Just title) ->
                 [intersperse (Text " ")
                              ([Text number] ++ titlePartsExceptLast title)]
               (Just number, Nothing) -> [[Text number]]
               (Nothing, Just title) ->
                 [intersperse (Text " ")
                              (titlePartsExceptLast title)]
               (Nothing, Nothing) ->
                 [[Text "Table of Contents"]],
        flattenedOutputSectionBody = outputSectionBody section
      })]
  ++ concatMap (computeFlattenedOutput (outputSectionTitle section))
               (outputSectionChildren section)


titleSeparator :: T.Text
titleSeparator = " ∙ "