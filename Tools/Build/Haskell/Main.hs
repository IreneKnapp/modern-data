{-# LANGUAGE ExistentialQuantification, OverloadedStrings,
             DeriveDataTypeable, TemplateHaskell, LiberalTypeSynonyms,
             Rank2Types #-}
module Main (main) where

import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified System.Directory as IO

import Control.Lens
import Control.Monad
import Data.Function
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Typeable


class TextShow textShow where
  textShow :: textShow -> Text.Text

class HasName hasName where
  name :: Simple Lens hasName Text.Text
class HasLanguage hasLanguage where
  language :: Simple Lens hasLanguage Language

data Language
  = CLanguage
  | HaskellLanguage
  deriving (Eq, Ord)
instance TextShow Language where
  textShow CLanguage = "c-language"
  textShow HaskellLanguage = "haskell-language"

data Provenance
  = InputProvenance
  | BuiltProvenance
  | SystemProvenance
  deriving (Eq, Ord)
instance TextShow Provenance where
  textShow InputProvenance = "input-provenance"

data FileType
  = UnknownFileType
  | HeaderFileType Language
  | SourceFileType Language
  | ObjectFileType
  | ExecutableFileType
  | LibraryFileType
  deriving (Eq, Ord)

class (TextShow file, Eq file, Ord file, Typeable file) => File file where
  fromAnyFile :: AnyFile -> Maybe file
  fileType :: Simple Lens file FileType
  path :: Simple Lens file Text.Text
  provenance :: Simple Lens file Provenance

data AnyFile = forall file . File file => AnyFile file
  deriving (Typeable)
anyFile :: (File file) => Simple Lens AnyFile file
anyFile = to (\(AnyFile file) -> file)
instance Eq AnyFile where
  (==) a b =
    case a of
      AnyFile a' -> case fromAnyFile b of
                      Just b' -> (==) a' b'
                      Nothing -> False
instance Ord AnyFile where
  compare a b =
    case a of
      AnyFile a' -> case fromAnyFile b of
                      Just b' -> compare a' b'
                      Nothing -> on compare (view fileType) a b
instance File AnyFile where
  fromAnyFile (AnyFile file) = cast file
  fileType = anyFile . fileType
  path = anyFile . path
  provenance = anyFile . provenance
instance TextShow AnyFile where
  textShow (AnyFile file) = textShow file

data Task
  = AmalgamationTask
  | BinaryTask
  | TestTask
  | DebugTask
  | CleanTask
  deriving (Eq, Ord)

data HeaderFile =
  HeaderFile {
      _headerFileLanguage :: Language,
      _headerFilePath :: Text.Text,
      _headerFileProvenance :: Provenance
    }
  deriving (Typeable)
makeLenses ''HeaderFile
instance Eq HeaderFile where
  (==) a b =
    foldl1 (&&)
           [on (==) (view language) a b,
            on (==) (view path) a b,
            on (==) (view provenance) a b]
instance Ord HeaderFile where
  compare a b =
    mconcat [on compare (view language) a b,
             on compare (view path) a b,
             on compare (view provenance) a b]
instance File HeaderFile where
  fromAnyFile (AnyFile file) = cast file
  fileType = to (\file -> HeaderFileType $ file ^. language)
  path = headerFilePath
  provenance = headerFileProvenance
instance TextShow HeaderFile where
  textShow file =
    Text.concat $
      ["(header-file \"",
       file ^. path,
       "\" ",
       textShow $ file ^. provenance,
       " ",
       textShow $ file ^. language,
       ")"]

data SourceFile =
  SourceFile {
      _sourceFileLanguage :: Language,
      _sourceFilePath :: Text.Text,
      _sourceFileProvenance :: Provenance
    }
  deriving (Typeable)
makeLenses ''SourceFile
instance Eq SourceFile where
  (==) a b =
    foldl1 (&&)
           [on (==) (view language) a b,
            on (==) (view path) a b,
            on (==) (view provenance) a b]
instance Ord SourceFile where
  compare a b =
    mconcat [on compare (view language) a b,
             on compare (view path) a b,
             on compare (view provenance) a b]
instance File SourceFile where
  fromAnyFile (AnyFile file) = cast file
  fileType = to (\file -> SourceFileType $ file ^. language)
  path = sourceFilePath
  provenance = sourceFileProvenance
instance TextShow SourceFile where
  textShow file =
    Text.concat $
      ["(source-file \"",
       file ^. path,
       "\" ",
       textShow $ file ^. provenance,
       " ",
       textShow $ file ^. language,
       ")"]

data ObjectFile =
  ObjectFile {
      _objectFilePath :: Text.Text,
      _objectFileProvenance :: Provenance
    }
  deriving (Typeable)
makeLenses ''ObjectFile
instance Eq ObjectFile where
  (==) a b =
    foldl1 (&&)
           [on (==) (view path) a b,
            on (==) (view provenance) a b]
instance Ord ObjectFile where
  compare a b =
    mconcat [on compare (view path) a b,
             on compare (view provenance) a b]
instance File ObjectFile where
  fromAnyFile (AnyFile file) = cast file
  fileType = to (\_ -> ObjectFileType)
  path = objectFilePath
  provenance = objectFileProvenance
instance TextShow ObjectFile where
  textShow file =
    Text.concat $
      ["(object-file \"",
       file ^. path,
       "\" ",
       textShow $ file ^. provenance,
       ")"]

data ExecutableFile =
  ExecutableFile {
      _executableFilePath :: Text.Text,
      _executableFileProvenance :: Provenance
    }
  deriving (Typeable)
makeLenses ''ExecutableFile
instance Eq ExecutableFile where
  (==) a b =
    foldl1 (&&)
           [on (==) (view path) a b,
            on (==) (view provenance) a b]
instance Ord ExecutableFile where
  compare a b =
    mconcat [on compare (view path) a b,
             on compare (view provenance) a b]
instance File ExecutableFile where
  fromAnyFile (AnyFile file) = cast file
  fileType = to (\_ -> ExecutableFileType)
  path = executableFilePath
  provenance = executableFileProvenance
instance TextShow ExecutableFile where
  textShow file =
    Text.concat $
      ["(executable-file \"",
       file ^. path,
       "\" ",
       textShow $ file ^. provenance,
       ")"]

data LibraryFile =
  LibraryFile {
      _libraryFilePath :: Text.Text,
      _libraryFileProvenance :: Provenance
    }
  deriving (Typeable)
makeLenses ''LibraryFile
instance Eq LibraryFile where
  (==) a b =
    foldl1 (&&)
           [on (==) (view path) a b,
            on (==) (view provenance) a b]
instance Ord LibraryFile where
  compare a b =
    mconcat [on compare (view path) a b,
             on compare (view provenance) a b]
instance File LibraryFile where
  fromAnyFile (AnyFile file) = cast file
  fileType = to (\_ -> LibraryFileType)
  path = libraryFilePath
  provenance = libraryFileProvenance
instance TextShow LibraryFile where
  textShow file =
    Text.concat $
      ["(library-file \"",
       file ^. path,
       "\" ",
       textShow $ file ^. provenance,
       ")"]

class (TextShow buildStep) => BuildStep buildStep where
  buildStepInputs :: Simple Lens buildStep (Set.Set AnyFile)
  buildStepOutputs :: Simple Lens buildStep (Set.Set AnyFile)
  explainBuildStep :: buildStep -> Text.Text
  performBuildStep :: buildStep -> IO ()

data AnyBuildStep =
  forall buildStep . BuildStep buildStep => AnyBuildStep buildStep
anyBuildStep :: (BuildStep buildStep) => Getter AnyBuildStep (Maybe buildStep)
anyBuildStep = to (\(AnyBuildStep buildStep) -> cast buildStep)
instance BuildStep AnyBuildStep where
  buildStepInputs = anyBuildStep . buildStepInputs
  buildStepOutputs = anyBuildStep . buildStepOutputs
  explainBuildStep (AnyBuildStep buildStep) = explainBuildStep buildStep
  performBuildStep (AnyBuildStep buildStep) = performBuildStep buildStep
instance TextShow AnyBuildStep where
  textShow (AnyBuildStep buildStep) = textShow buildStep

class (HasName target, TextShow target, Typeable target) => Target target where
  targetBuildSteps :: Task -> target -> [AnyBuildStep]
  targetPrerequisites :: Getter target (Set.Set AnyTarget)
  targetProducts :: Getter target (Set.Set AnyFile)

data AnyTarget = forall target . Target target => AnyTarget target
  deriving (Typeable)
anyTarget
    :: forall f a . (Contravariant f, Functor f)
    => (forall target . Target target => Simple (LensLike f) target a)
    -> Simple (LensLike f) AnyTarget a
anyTarget underlying =
  lens (\(AnyTarget target) -> view underlying target)
       (\(AnyTarget target) value -> AnyTarget $ set underlying value target)
instance Eq AnyTarget where
  (==) a b = on (==) (view name) a b
instance Ord AnyTarget where
  compare a b = on compare (view name) a b
instance HasName AnyTarget where
  name = anyTarget name
instance Target AnyTarget where
  targetBuildSteps task (AnyTarget target) = targetBuildSteps task target
  targetPrerequisites = anyTarget targetPrerequisites
  targetProducts = anyTarget targetProducts
instance TextShow AnyTarget where
  textShow (AnyTarget target) = textShow target

data ExecutableTarget =
  ExecutableTarget {
      _executableTargetName :: Text.Text,
      _executableTargetPrerequisites :: Set.Set AnyTarget,
      _executableTargetPrivateHeaders :: Set.Set HeaderFile,
      _executableTargetSources :: Set.Set SourceFile
    }
  deriving (Typeable)
makeLenses ''ExecutableTarget
instance HasName ExecutableTarget where
  name = executableTargetName
instance Target ExecutableTarget where
  targetBuildSteps AmalgamationTask executable = []
  targetBuildSteps BinaryTask executable = []
  targetBuildSteps TestTask executable = []
  targetBuildSteps DebugTask executable = []
  targetBuildSteps CleanTask executable = []
  targetPrerequisites = executableTargetPrerequisites
  targetProducts =
    to (\executable ->
          Set.fromList
            [AnyFile $ ExecutableFile {
                 _executableFilePath =
                   Text.concat ["dist/",
                                executable ^. name,
                                "/binary/products/",
                                executable ^. name],
                 _executableFileProvenance = BuiltProvenance
               }])
instance TextShow ExecutableTarget where
  textShow target =
    Text.concat $
      ["(executable-target ",
       target ^. name,
       " (",
       Text.intercalate " " $ map textShow $ Set.toList $
         target ^. executableTargetPrerequisites,
       ") (",
       Text.intercalate " " $ map textShow $ Set.toList $
         target ^. executableTargetPrivateHeaders,
       ") (",
       Text.intercalate " " $ map textShow $ Set.toList $
         target ^. executableTargetSources,
       "))"]

data LibraryTarget =
  LibraryTarget {
      _libraryTargetName :: Text.Text,
      _libraryTargetPrerequisites :: Set.Set AnyTarget,
      _libraryTargetPublicHeaders :: Set.Set HeaderFile,
      _libraryTargetPrivateHeaders :: Set.Set HeaderFile,
      _libraryTargetSources :: Set.Set SourceFile
    }
  deriving (Typeable)
makeLenses ''LibraryTarget
instance HasName LibraryTarget where
  name = libraryTargetName
instance Target LibraryTarget where
  targetBuildSteps AmalgamationTask library = []
  targetBuildSteps BinaryTask library = []
  targetBuildSteps TestTask library = []
  targetBuildSteps DebugTask library = []
  targetBuildSteps CleanTask library = []
  targetPrerequisites = libraryTargetPrerequisites
  targetProducts =
    to (\library ->
          Set.fromList
            [AnyFile $ LibraryFile {
                 _libraryFilePath =
                   Text.concat ["dist/",
                                library ^. name,
                                "/binary/products/lib",
                                library ^. name,
                                ".a"],
                 _libraryFileProvenance = BuiltProvenance
               }])
instance TextShow LibraryTarget where
  textShow target =
    Text.concat $
      ["(library-target ",
       target ^. name,
       " (",
       Text.intercalate " " $ map textShow $ Set.toList
        $ target ^. libraryTargetPrerequisites,
       ") (",
       Text.intercalate " " $ map textShow $ Set.toList
        $ target ^. libraryTargetPublicHeaders,
       ") (",
       Text.intercalate " " $ map textShow $ Set.toList
        $ target ^. libraryTargetPrivateHeaders,
       ") (",
       Text.intercalate " " $ map textShow $ Set.toList
        $ target ^. libraryTargetSources,
       "))"]

data Invocation =
  Invocation {
      _invocationExecutable :: ExecutableFile,
      _invocationParameters :: [Text.Text],
      _invocationInputs :: Set.Set AnyFile,
      _invocationOutputs :: Set.Set AnyFile
    }
makeLenses ''Invocation
instance BuildStep Invocation where
  buildStepInputs = invocationInputs
  buildStepOutputs = invocationOutputs
  explainBuildStep invocation =
    Text.intercalate " "
      ((invocation ^. invocationExecutable . path)
       : invocation ^. invocationParameters)
  performBuildStep invocation = do
    putStrLn $ Text.unpack $ explainBuildStep invocation
instance TextShow Invocation where
  textShow invocation =
    Text.concat $
      ["(invocation ",
       textShow $ invocation ^. invocationExecutable,
       " (",
       Text.intercalate " " $ invocation ^. invocationParameters,
       ") (",
       Text.intercalate " " $ map textShow $ Set.toList
        $ invocation ^. invocationInputs,
       ") (",
       Text.intercalate " " $ map textShow $ Set.toList
        $ invocation ^. invocationOutputs,
       "))"]

data Mode
  = HelpMode
  | TaskMode Task AnyTarget
  deriving (Eq, Ord)

data Project =
  Project {
      _projectName :: Text.Text,
      _projectDefaultTarget :: Maybe AnyTarget,
      _projectTargets :: Set.Set AnyTarget
    }
makeLenses ''Project
instance Eq Project where
  (==) a b = on (==) _projectName a b
instance Ord Project where
  compare a b = on compare _projectName a b
instance HasName Project where
  name = projectName


main :: IO ()
main = do
  project <- makeProject "Modern Data"
  mainLibrary <- makeLibrary "modern" "../../C/library/"
  makeKeywordsExecutable <-
    makeExecutable "make-keywords" "../../C/tools/make-keywords/"
  mainLibrary <-
    libraryPrerequisiteAdd mainLibrary (AnyTarget makeKeywordsExecutable)
  project <- projectLibraryAdd project mainLibrary
  let buildSteps = targetBuildSteps BinaryTask mainLibrary
      explanation = map explainBuildStep buildSteps
  mapM_ (putStrLn . Text.unpack) explanation


makeProject :: Text.Text -> IO Project
makeProject name = do
  return $ Project {
               _projectName = name,
               _projectDefaultTarget = Nothing,
               _projectTargets = Set.empty
             }


projectLibraryAdd :: Project -> LibraryTarget -> IO Project
projectLibraryAdd project library = do
  return $ set projectTargets
    (Set.union (Set.singleton $ AnyTarget library) (project ^. projectTargets))
    project


makeExecutable :: Text.Text -> Text.Text -> IO ExecutableTarget
makeExecutable name directory = do
  files <- scanDirectory directory
  let headers =
        Set.fromList $ catMaybes $ map fromAnyFile $ Set.toList files
      sources =
        Set.fromList $ catMaybes $ map fromAnyFile $ Set.toList files
  return $ ExecutableTarget {
               _executableTargetName = name,
               _executableTargetPrerequisites = Set.empty,
               _executableTargetPrivateHeaders = headers,
               _executableTargetSources = sources
             }


makeLibrary :: Text.Text -> Text.Text -> IO LibraryTarget
makeLibrary name directory = do
  return $ LibraryTarget {
               _libraryTargetName = name,
               _libraryTargetPrerequisites = Set.empty,
               _libraryTargetPublicHeaders = Set.empty,
               _libraryTargetPrivateHeaders = Set.empty,
               _libraryTargetSources = Set.empty
             }


libraryPrerequisiteAdd :: LibraryTarget -> AnyTarget -> IO LibraryTarget
libraryPrerequisiteAdd library prerequisite = do
  return library


scanDirectory :: Text.Text -> IO (Set.Set AnyFile)
scanDirectory path = do
   contents <- IO.getDirectoryContents $ Text.unpack path
   foldM (\soFar filename -> do
            if elem filename [".", ".."]
              then return soFar
              else do
                let filePath = Text.concat [path, filename]
                isFile <- IO.doesFileExist $ Text.unpack filePath
                if isFile
                  then do
                    let maybeFileType = foldl'
                          (\soFar (extension, fileType) ->
                             case soFar of
                               Just _ -> soFar
                               Nothing ->
                                 let filenameLength = Text.length filename
                                     extensionLength = Text.length extension
                                     baseLength =
                                       filenameLength - extensionLength
                                 in if (baseLength >= 1)
                                       && (Text.drop baseLength filename
                                           == extension)
                                      then Just fileType
                                      else Nothing)
                          Nothing
                          [(".h", HeaderFileType CLanguage),
                           (".c", SourceFileType CLanguage),
                           (".hs", SourceFileType HaskellLanguage)]
                    case maybeFileType of
                      Nothing -> return soFar
                      Just (HeaderFileType language) -> do
                        let file = HeaderFile {
                                       _headerFileLanguage = language,
                                       _headerFilePath = filePath,
                                       _headerFileProvenance = InputProvenance
                                     }
                        return $ Set.insert (AnyFile file) soFar
                      Just (SourceFileType language) -> do
                        let file = SourceFile {
                                       _sourceFileLanguage = language,
                                       _sourceFilePath = filePath,
                                       _sourceFileProvenance = InputProvenance
                                     }
                        return $ Set.insert (AnyFile file) soFar
                  else do
                    isDirectory <- IO.doesDirectoryExist $ Text.unpack filePath
                    if isDirectory
                      then do
                        subresults <- scanDirectory filePath
                        return $ Set.union soFar subresults
                      else return soFar)
         Set.empty
         (map Text.pack contents)

