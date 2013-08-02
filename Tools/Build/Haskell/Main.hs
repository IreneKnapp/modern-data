{-# LANGUAGE ExistentialQuantification, OverloadedStrings,
             DeriveDataTypeable, TemplateHaskell #-}
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
makeLenses ''AnyFile
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
  fileType (AnyFile file) = fileType . anyFile
  path (AnyFile file) = path . anyFile
  provenance (AnyFile file) = provenance . anyFile
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
  fileType file = HeaderFileType $ file^.language
  filePath = view path
  fileProvenance = view provenance
instance TextShow HeaderFile where
  textShow file =
    Text.concat $
      ["(header-file \"",
       file^.path,
       "\" ",
       textShow $ file^.provenance,
       " ",
       textShow $ file^.language,
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
           [on (==) sourceFileLanguage a b,
            on (==) sourceFilePath a b,
            on (==) sourceFileProvenance a b]
instance Ord SourceFile where
  compare a b =
    mconcat [on compare sourceFileLanguage a b,
             on compare sourceFilePath a b,
             on compare sourceFileProvenance a b]
instance File SourceFile where
  fromAnyFile (AnyFile file) = cast file
  fileType file = SourceFileType $ sourceFileLanguage file
  filePath = sourceFilePath
  fileProvenance = sourceFileProvenance
instance TextShow SourceFile where
  textShow file =
    Text.concat $
      ["(source-file \"",
       sourceFilePath file,
       "\" ",
       textShow $ sourceFileProvenance file,
       " ",
       textShow $ sourceFileLanguage file,
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
           [on (==) objectFilePath a b,
            on (==) objectFileProvenance a b]
instance Ord ObjectFile where
  compare a b =
    mconcat [on compare objectFilePath a b,
             on compare objectFileProvenance a b]
instance File ObjectFile where
  fromAnyFile (AnyFile file) = cast file
  fileType _ = ObjectFileType
  filePath = objectFilePath
  fileProvenance = objectFileProvenance
instance TextShow ObjectFile where
  textShow file =
    Text.concat $
      ["(object-file \"",
       objectFilePath file,
       "\" ",
       textShow $ objectFileProvenance file,
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
           [on (==) executableFilePath a b,
            on (==) executableFileProvenance a b]
instance Ord ExecutableFile where
  compare a b =
    mconcat [on compare executableFilePath a b,
             on compare executableFileProvenance a b]
instance File ExecutableFile where
  fromAnyFile (AnyFile file) = cast file
  fileType _ = ExecutableFileType
  filePath = executableFilePath
  fileProvenance = executableFileProvenance
instance TextShow ExecutableFile where
  textShow file =
    Text.concat $
      ["(executable-file \"",
       executableFilePath file,
       "\" ",
       textShow $ executableFileProvenance file,
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
           [on (==) libraryFilePath a b,
            on (==) libraryFileProvenance a b]
instance Ord LibraryFile where
  compare a b =
    mconcat [on compare libraryFilePath a b,
             on compare libraryFileProvenance a b]
instance File LibraryFile where
  fromAnyFile (AnyFile file) = cast file
  fileType _ = LibraryFileType
  filePath = libraryFilePath
  fileProvenance = libraryFileProvenance
instance TextShow LibraryFile where
  textShow file =
    Text.concat $
      ["(library-file \"",
       libraryFilePath file,
       "\" ",
       textShow $ libraryFileProvenance file,
       ")"]

class (TextShow buildStep) => BuildStep buildStep where
  buildStepInputs :: buildStep -> Set.Set AnyFile
  buildStepOutputs :: buildStep -> Set.Set AnyFile
  explainBuildStep :: buildStep -> Text.Text
  performBuildStep :: buildStep -> IO ()

data AnyBuildStep =
  forall buildStep . BuildStep buildStep => AnyBuildStep buildStep
instance BuildStep AnyBuildStep where
  buildStepInputs (AnyBuildStep buildStep) = buildStepInputs buildStep
  buildStepOutputs (AnyBuildStep buildStep) = buildStepOutputs buildStep
  explainBuildStep (AnyBuildStep buildStep) = explainBuildStep buildStep
  performBuildStep (AnyBuildStep buildStep) = performBuildStep buildStep
instance TextShow AnyBuildStep where
  textShow (AnyBuildStep buildStep) = textShow buildStep

class (HasName target, TextShow target) => Target target where
  targetBuildSteps :: Task -> target -> [AnyBuildStep]
  targetPrerequisites :: target -> Set.Set AnyTarget
  targetProducts :: target -> Set.Set AnyFile

data AnyTarget = forall target . Target target => AnyTarget target
makeLenses ''AnyTarget
instance Eq AnyTarget where
  (==) a b = on (==) (view name) a b
instance Ord AnyTarget where
  compare a b = on compare (view name) a b
instance HasName AnyTarget where
  name (AnyTarget target) = name . target
instance Target AnyTarget where
  targetBuildSteps task (AnyTarget target) = targetBuildSteps task target
  targetPrerequisites (AnyTarget target) = targetPrerequisites target
  targetProducts (AnyTarget target) = targetProducts target
instance TextShow AnyTarget where
  textShow (AnyTarget target) = textShow target

data ExecutableTarget =
  ExecutableTarget {
      _executableTargetName :: Text.Text,
      _executableTargetPrerequisites :: Set.Set AnyTarget,
      _executableTargetPrivateHeaders :: Set.Set HeaderFile,
      _executableTargetSources :: Set.Set SourceFile
    }
makeLenses ''ExecutableTarget
instance HasName ExecutableTarget where
  nameOf = executableTargetName
instance Target ExecutableTarget where
  targetBuildSteps = computeExecutableBuildSteps
  targetPrerequisites = executableTargetPrerequisites
  targetProducts executable =
    Set.fromList
      [AnyFile $ ExecutableFile {
           executableFilePath =
             Text.concat ["dist/",
                          executableTargetName executable,
                          "/binary/products/",
                          executableTargetName executable],
           executableFileProvenance = BuiltProvenance
         }]
instance TextShow ExecutableTarget where
  textShow target =
    Text.concat $
      ["(executable-target ",
       executableTargetName target,
       " (",
       Text.intercalate " " $ map textShow $ Set.toList
        $ executableTargetPrerequisites target,
       ") (",
       Text.intercalate " " $ map textShow $ Set.toList
        $ executableTargetPrivateHeaders target,
       ") (",
       Text.intercalate " " $ map textShow $ Set.toList
        $ executableTargetSources target,
       "))"]

data LibraryTarget =
  LibraryTarget {
      _libraryTargetName :: Text.Text,
      _libraryTargetPrerequisites :: Set.Set AnyTarget,
      _libraryTargetPublicHeaders :: Set.Set HeaderFile,
      _libraryTargetPrivateHeaders :: Set.Set HeaderFile,
      _libraryTargetSources :: Set.Set SourceFile
    }
makeLenses ''LibraryTarget
instance HasName LibraryTarget where
  nameOf = libraryTargetName
instance Target LibraryTarget where
  targetBuildSteps = computeLibraryBuildSteps
  targetPrerequisites = libraryTargetPrerequisites
  targetProducts library =
    Set.fromList
      [AnyFile $ LibraryFile {
           libraryFilePath =
             Text.concat ["dist/",
                          libraryTargetName library,
                          "/binary/products/lib",
                          libraryTargetName library,
                          ".a"],
           libraryFileProvenance = BuiltProvenance
         }]
instance TextShow LibraryTarget where
  textShow target =
    Text.concat $
      ["(library-target ",
       libraryTargetName target,
       " (",
       Text.intercalate " " $ map textShow $ Set.toList
        $ libraryTargetPrerequisites target,
       ") (",
       Text.intercalate " " $ map textShow $ Set.toList
        $ libraryTargetPublicHeaders target,
       ") (",
       Text.intercalate " " $ map textShow $ Set.toList
        $ libraryTargetPrivateHeaders target,
       ") (",
       Text.intercalate " " $ map textShow $ Set.toList
        $ libraryTargetSources target,
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
  explainBuildStep = computeInvocationExplanation
  performBuildStep = performInvocation
instance TextShow Invocation where
  textShow invocation =
    Text.concat $
      ["(invocation ",
       textShow $ invocationExecutable invocation,
       " (",
       Text.intercalate " " $ invocationParameters invocation,
       ") (",
       Text.intercalate " " $ map textShow $ Set.toList
        $ invocationInputs invocation,
       ") (",
       Text.intercalate " " $ map textShow $ Set.toList
        $ invocationOutputs invocation,
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
               projectName = name,
               projectDefaultTarget = Nothing,
               projectTargets = Set.empty
             }


projectLibraryAdd :: Project -> LibraryTarget -> IO Project
projectLibraryAdd project library = do
  return $ project {
               projectTargets =
                 Set.union (Set.singleton $ AnyTarget library)
                           (projectTargets project)
             }


makeExecutable :: Text.Text -> Text.Text -> IO ExecutableTarget
makeExecutable name directory = do
  files <- scanDirectory directory
  let headers =
        Set.fromList $ catMaybes $ map fromAnyFile $ Set.toList files
      sources =
        Set.fromList $ catMaybes $ map fromAnyFile $ Set.toList files
  return $ ExecutableTarget {
               executableTargetName = name,
               executableTargetPrerequisites = Set.empty,
               executableTargetPrivateHeaders = headers,
               executableTargetSources = sources
             }


makeLibrary :: Text.Text -> Text.Text -> IO LibraryTarget
makeLibrary name directory = do
  return $ LibraryTarget {
               libraryTargetName = name,
               libraryTargetPrerequisites = Set.empty,
               libraryTargetPublicHeaders = Set.empty,
               libraryTargetPrivateHeaders = Set.empty,
               libraryTargetSources = Set.empty
             }


libraryPrerequisiteAdd :: LibraryTarget -> AnyTarget -> IO LibraryTarget
libraryPrerequisiteAdd library prerequisite = do
  return library


computeExecutableBuildSteps :: Task -> ExecutableTarget -> [AnyBuildStep]
computeExecutableBuildSteps AmalgamationTask executable = []
computeExecutableBuildSteps BinaryTask executable = []
computeExecutableBuildSteps TestTask executable = []
computeExecutableBuildSteps DebugTask executable = []
computeExecutableBuildSteps CleanTask executable = []


computeLibraryBuildSteps :: Task -> LibraryTarget -> [AnyBuildStep]
computeLibraryBuildSteps AmalgamationTask library = []
computeLibraryBuildSteps BinaryTask library = []
computeLibraryBuildSteps TestTask library = []
computeLibraryBuildSteps DebugTask library = []
computeLibraryBuildSteps CleanTask library = []


computeInvocationExplanation :: Invocation -> Text.Text
computeInvocationExplanation invocation =
  Text.intercalate " "
    ((filePath $ invocationExecutable invocation)
     : invocationParameters invocation)


performInvocation :: Invocation -> IO ()
performInvocation invocation = do
  putStrLn $ Text.unpack $ computeInvocationExplanation invocation


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
                                       headerFileLanguage = language,
                                       headerFilePath = filePath,
                                       headerFileProvenance = InputProvenance
                                     }
                        return $ Set.insert (AnyFile file) soFar
                      Just (SourceFileType language) -> do
                        let file = SourceFile {
                                       sourceFileLanguage = language,
                                       sourceFilePath = filePath,
                                       sourceFileProvenance = InputProvenance
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

