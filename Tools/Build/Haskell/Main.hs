{-# LANGUAGE ExistentialQuantification, OverloadedStrings #-}
module Main (main) where

import qualified Data.Set as Set
import qualified Data.Text as Text

import Data.Function


class TextShow textShow where
  textShow :: textShow -> Text.Text

class Named named where
  namedName :: named -> Text.Text

class BuildStep buildStep where
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

class (Named target) => Target target where
  targetBuildSteps :: Task -> target -> [AnyBuildStep]
  targetPrerequisites :: target -> Set.Set AnyTarget
  targetProducts :: target -> Set.Set AnyFile

data AnyTarget = forall target . Target target => AnyTarget target
instance Eq AnyTarget where
  (==) a b = on (==) namedName a b
instance Ord AnyTarget where
  compare a b = on compare namedName a b
instance Target AnyTarget where
  targetPrerequisites (AnyTarget target) = targetPrerequisites target
  targetProducts (AnyTarget target) = targetProducts target

data FileType
  = UnknownFileType
  | HeaderFileType
  | SourceFileType
  | ObjectFileType
  | ExecutableFileType
  | LibraryFileType
  deriving (Eq, Ord)

class File file where
  fileType :: file -> FileType
  filePath :: file -> Text.Text
  fileProvenance :: file -> Provenance

data AnyFile = forall file . File file => AnyFile file
instance Eq AnyFile where
  (==) (AnyFile a) (AnyFile b) = (==) a b
instance Ord AnyFile where
  compare (AnyFile a) (AnyFile b) = compare a b
instance File AnyFile where
  filePath (AnyFile file) = filePath file
  fileProvenance (AnyFile file) = fileProvenance file

data Mode
  = HelpMode
  | TaskMode Task AnyTarget
  deriving (Eq, Ord)

data Task
  = AmalgamationTask
  | BinaryTask
  | TestTask
  | DebugTask
  | CleanTask
  deriving (Eq, Ord)

data Project =
  Project {
      projectName :: Text.Text,
      projectDefaultTarget :: Maybe AnyTarget,
      projectTargets :: Set.Set AnyTarget
    }
instance Eq Project where
  (==) a b = on (==) projectName a b
instance Ord Project where
  compare a b = on compare projectName a b
instance Named Project where
  namedName = projectName

data ExecutableTarget =
  ExecutableTarget {
      executableTargetName :: Text.Text,
      executableTargetPrerequisites :: Set.Set AnyFile,
      executableTargetPrivateHeaders :: Set.Set HeaderFile,
      executableTargetSources :: Set.Set SourceFile
    }
instance Named ExecutableTarget where
  namedName = executableTargetName
instance Target ExecutableTarget where
  targetBuildSteps = computeExecutableBuildSteps
  targetPrerequisites = executableTargetPrerequisites
  targetProducts executable =
    Set.fromList
      [ExecutableFile {
           executableFilePath =
             Text.concat ["dist/",
                          executableTargetName executable,
                          "/binary/products/",
                          executableTargetName executable],
           executableFileProvenance = BuiltProvenance
         }]

data LibraryTarget =
  LibraryTarget {
      libraryTargetName :: Text.Text,
      libraryTargetPrerequisites :: Set.Set AnyFile,
      libraryTargetPublicHeaders :: Set.Set HeaderFile,
      libraryTargetPrivateHeaders :: Set.Set HeaderFile,
      libraryTargetSources :: Set.Set SourceFile
    }
instance Named LibraryTarget where
  namedName = libraryTargetName
instance Target LibraryTarget where
  targetBuildSteps = computeLibraryBuildSteps
  targetPrerequisites = libraryTargetPrerequisites
  targetProducts library =
    Set.fromList
      [LibraryFile {
           libraryFilePath =
             Text.concat ["dist/",
                          libraryTargetName library,
                          "/binary/products/lib",
                          libraryTargetName library,
                          ".a"],
           libraryFileProvenance = BuiltProvenance
         }]

data Provenance
  = InputProvenance
  | BuiltProvenance
  | SystemProvenance
  deriving (Eq, Ord)

data HeaderFile =
  HeaderFile {
      headerFileLanguage :: Language,
      headerFilePath :: Text.Text,
      headerFileProvenance :: Provenance
    }
instance File HeaderFile where
  filePath = headerFilePath
  fileProvenance = headerFileProvenance

data SourceFile =
  SourceFile {
      sourceFileLanguage :: Language,
      sourceFilePath :: Text.Text,
      sourceFileProvenance :: Provenance
    }
instance File SourceFile where
  filePath = sourceFilePath
  fileProvenance = sourceFileProvenance

data ObjectFile =
  ObjectFile {
      objectFilePath :: Text.Text,
      objectFileProvenance :: Provenance
    }
instance File ObjectFile where
  filePath = objectFilePath
  fileProvenance = objectFileProvenance

data ExecutableFile =
  ExecutableFile {
      executableFilePath :: Text.Text,
      executableFileProvenance :: Provenance
    }
instance File ExecutableFile where
  filePath = executableFilePath
  fileProvenance = executableFileProvenance

data LibraryFile =
  LibraryFile {
      libraryFilePath :: Text.Text,
      libraryFileProvenance :: Provenance
    }
instance File LibraryFile where
  filePath = libraryFilePath
  fileProvenance = libraryFileProvenance

data Invocation =
  Invocation {
      invocationExecutable :: ExecutableFile,
      invocationParameters :: [Text.Text],
      invocationInputs :: Set.Set AnyFile,
      invocationOutputs :: Set.Set AnyFile
    }
instance BuildStep Invocation where
  buildStepInputs = invocationInputs
  buildStepOutputs = invocationOutputs
  explainBuildStep = computeInvocationExplanation
  performBuildStep = performInvocation

data Language
  = CLanguage
  | HaskellLanguage
  deriving (Eq, Ord)


main :: IO ()
main = do
  project <- makeProject "Modern Data"
  mainLibrary <- makeLibrary "modern" "library/"
  makeKeywordsExecutable <-
    makeExecutable "make-keywords" "tools/make-keywords/"
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
  return $ ExecutableTarget {
               executableTargetName = name,
               executableTargetPrerequisites = Set.empty,
               executableTargetPrivateHeaders = Set.empty,
               executableTargetSources = Set.empty
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

