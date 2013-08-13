{-# LANGUAGE ExistentialQuantification, TemplateHaskell #-}
module Build.Types
  (TextShow(..),
   HasName(..),
   HasLanguage(..),
   File(..),
   BuildStep(..),
   Target(..),
   Language(..),
   Provenance(..),
   FileType(..),
   AnyFile(..),
   Task(..),
   HeaderFile(..),
   headerFileLanguage,
   headerFilePath,
   headerFileProvenance,
   SourceFile(..),
   sourceFileLanguage,
   sourceFilePath,
   sourceFileProvenance,
   ObjectFile(..),
   objectFilePath,
   objectFileProvenance,
   ExecutableFile(..),
   executableFilePath,
   executableFileProvenance,
   LibraryFile(..),
   libraryFilePath,
   libraryFileProvenance,
   AnyBuildStep(..),
   AnyTarget(..),
   ExecutableTarget(..),
   executableTargetName,
   executableTargetPrerequisites,
   executableTargetPrivateHeaders,
   executableTargetSources,
   LibraryTarget(..),
   libraryTargetName,
   libraryTargetPrerequisites,
   libraryTargetPublicHeaders,
   libraryTargetPrivateHeaders,
   libraryTargetSources,
   Invocation(..),
   invocationExecutable,
   invocationParameters,
   invocationInputs,
   invocationOutputs,
   CopyFile(..),
   copyFileInput,
   copyFileOutputPath,
   MakeDirectory(..),
   makeDirectoryPath,
   Conditional(..),
   conditionalCondition,
   conditionalWhenTrue,
   conditionalWhenFalse,
   AnyCondition(..),
   PathExists(..),
   pathExistsPath,
   FileExists(..),
   fileExistsPath,
   DirectoryExists(..),
   directoryExistsPath,
   Mode(..),
   Project(..),
   projectName,
   projectDefaultTarget,
   projectTargets)
  where


import qualified Data.Set as Set
import qualified Data.Text as Text

import Control.Lens
import Data.Function
import Data.Typeable


class TextShow textShow where
  textShow :: textShow -> Text.Text


class HasName hasName where
  name :: Simple Lens hasName Text.Text


class HasLanguage hasLanguage where
  language :: Simple Lens hasLanguage Language


data AnyFile = forall file . File file => AnyFile file


class (TextShow file, Eq file, Ord file, Typeable file) => File file where
  fromAnyFile :: AnyFile -> Maybe file
  fileType :: Getter file FileType
  path :: Simple Lens file Text.Text
  provenance :: Simple Lens file Provenance


data AnyBuildStep =
  forall buildStep . BuildStep buildStep => AnyBuildStep buildStep


class (TextShow buildStep) => BuildStep buildStep where
  buildStepInputs :: Getter buildStep (Set.Set AnyFile)
  buildStepOutputs :: Getter buildStep (Set.Set AnyFile)
  explainBuildStep :: buildStep -> Text.Text
  performBuildStep :: buildStep -> IO ()


data AnyTarget = forall target . Target target => AnyTarget target


class (HasName target, TextShow target, Typeable target) => Target target where
  targetBuildSteps :: Task -> target -> [AnyBuildStep]
  targetPrerequisites :: Simple Lens target (Set.Set AnyTarget)
  targetProducts :: Getter target (Set.Set AnyFile)


data AnyCondition =
  forall condition . Condition condition => AnyCondition condition


class (TextShow condition, Typeable condition) => Condition condition where
  explainCondition :: condition -> Text.Text
  testCondition :: condition -> IO Bool


data Language
  = CLanguage
  | HaskellLanguage


data Provenance
  = InputProvenance
  | BuiltProvenance
  | SystemProvenance


data FileType
  = UnknownFileType
  | HeaderFileType Language
  | SourceFileType Language
  | ObjectFileType
  | ExecutableFileType
  | LibraryFileType


data Task
  = AmalgamationTask
  | BinaryTask
  | TestTask
  | DebugTask
  | CleanTask


data HeaderFile =
  HeaderFile {
      _headerFileLanguage :: Language,
      _headerFilePath :: Text.Text,
      _headerFileProvenance :: Provenance
    }
makeLenses ''HeaderFile


data SourceFile =
  SourceFile {
      _sourceFileLanguage :: Language,
      _sourceFilePath :: Text.Text,
      _sourceFileProvenance :: Provenance
    }
makeLenses ''SourceFile


data ObjectFile =
  ObjectFile {
      _objectFilePath :: Text.Text,
      _objectFileProvenance :: Provenance
    }
makeLenses ''ObjectFile


data ExecutableFile =
  ExecutableFile {
      _executableFilePath :: Text.Text,
      _executableFileProvenance :: Provenance
    }
makeLenses ''ExecutableFile


data LibraryFile =
  LibraryFile {
      _libraryFilePath :: Text.Text,
      _libraryFileProvenance :: Provenance
    }
makeLenses ''LibraryFile


data ExecutableTarget =
  ExecutableTarget {
      _executableTargetName :: Text.Text,
      _executableTargetPrerequisites :: Set.Set AnyTarget,
      _executableTargetPrivateHeaders :: Set.Set HeaderFile,
      _executableTargetSources :: Set.Set SourceFile
    }
makeLenses ''ExecutableTarget


data LibraryTarget =
  LibraryTarget {
      _libraryTargetName :: Text.Text,
      _libraryTargetPrerequisites :: Set.Set AnyTarget,
      _libraryTargetPublicHeaders :: Set.Set HeaderFile,
      _libraryTargetPrivateHeaders :: Set.Set HeaderFile,
      _libraryTargetSources :: Set.Set SourceFile
    }
makeLenses ''LibraryTarget


data Invocation =
  Invocation {
      _invocationExecutable :: ExecutableFile,
      _invocationParameters :: [Text.Text],
      _invocationInputs :: Set.Set AnyFile,
      _invocationOutputs :: Set.Set AnyFile
    }
makeLenses ''Invocation


data CopyFile =
  CopyFile {
      _copyFileInput :: AnyFile,
      _copyFileOutputPath :: Text.Text
    }
makeLenses ''CopyFile


data MakeDirectory =
  MakeDirectory {
      _makeDirectoryPath :: Text.Text
    }
makeLenses ''MakeDirectory


data Conditional =
  Conditional {
      _conditionalCondition :: AnyCondition,
      _conditionalWhenTrue :: [AnyBuildStep],
      _conditionalWhenFalse :: [AnyBuildStep]
    }
makeLenses ''Conditional


data PathExists =
  PathExists {
      _pathExistsPath :: Text.Text
    }
makeLenses ''PathExists


data FileExists =
  FileExists {
      _fileExistsPath :: Text.Text
    }
makeLenses ''FileExists


data DirectoryExists =
  DirectoryExists {
      _directoryExistsPath :: Text.Text
    }
makeLenses ''DirectoryExists


data Mode
  = HelpMode
  | TaskMode Task AnyTarget


data Project =
  Project {
      _projectName :: Text.Text,
      _projectDefaultTarget :: Maybe AnyTarget,
      _projectTargets :: Set.Set AnyTarget
    }
makeLenses ''Project

