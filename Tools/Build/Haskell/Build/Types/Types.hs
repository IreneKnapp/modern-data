{-# LANGUAGE ExistentialQuantification #-}
module Build.Types.Types
  (TextShow(..),
   HasName(..),
   HasLanguage(..),
   File(..),
   BuildStep(..),
   Target(..),
   Condition(..),
   Language(..),
   Provenance(..),
   FileType(..),
   AnyFile(..),
   Task(..),
   HeaderFile(..),
   SourceFile(..),
   ObjectFile(..),
   ExecutableFile(..),
   LibraryFile(..),
   BuildStepType(..),
   AnyBuildStep(..),
   AnyTarget(..),
   ExecutableTarget(..),
   LibraryTarget(..),
   InvocationBuildStep(..),
   AmalgamateFilesBuildStep(..),
   CopyFileBuildStep(..),
   MakeDirectoryBuildStep(..),
   ConditionalBuildStep(..),
   ConditionType(..),
   AnyCondition(..),
   AndCondition(..),
   OrCondition(..),
   NotCondition(..),
   PathExistsCondition(..),
   FileExistsCondition(..),
   DirectoryExistsCondition(..),
   Mode(..),
   Project(..),
   Defaults(..),
   ProjectSpecification(..),
   SubprojectSpecification(..),
   AnyTargetSpecification(..),
   TargetSpecification(..),
   ExecutableSpecification(..),
   LibrarySpecification(..),
   InvocationSpecification(..),
   Buildfile(..))
  where


import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import Control.Lens
import Data.Typeable


class TextShow textShow where
  textShow :: textShow -> Text.Text


class HasName hasName where
  name :: Simple Lens hasName Text.Text


class HasLanguage hasLanguage where
  language :: Simple Lens hasLanguage Language


class (TextShow file, Eq file, Ord file, Typeable file) => File file where
  fromAnyFile :: AnyFile -> Maybe file
  fileType :: Getter file FileType
  path :: Simple Lens file Text.Text
  provenance :: Simple Lens file Provenance


class (HasName target, TextShow target, Typeable target) => Target target where
  targetBuildSteps :: Project -> Task -> target -> [AnyBuildStep]
  targetPrerequisites :: Simple Lens target (Set.Set Text.Text)
  targetProducts :: Getter target (Set.Set AnyFile)


class (TextShow buildStep, Eq buildStep, Ord buildStep, Typeable buildStep)
      => BuildStep buildStep where
  fromAnyBuildStep :: AnyBuildStep -> Maybe buildStep
  buildStepType :: Getter buildStep BuildStepType
  buildStepInputs :: Getter buildStep (Set.Set AnyFile)
  buildStepOutputs :: Getter buildStep (Set.Set AnyFile)
  performBuildStep :: buildStep -> IO Bool


class (TextShow condition, Eq condition, Ord condition, Typeable condition)
      => Condition condition where
  fromAnyCondition :: AnyCondition -> Maybe condition
  conditionType :: Getter condition ConditionType
  explainCondition :: condition -> Text.Text
  testCondition :: condition -> IO Bool


class (HasName target, TextShow target, Typeable target)
      => TargetSpecification target where
  fromAnyTargetSpecification :: AnyTargetSpecification -> Maybe target


data AnyFile = forall file . File file => AnyFile file


data AnyTarget = forall target . Target target => AnyTarget target


data AnyBuildStep =
  forall buildStep . BuildStep buildStep => AnyBuildStep buildStep


data AnyCondition =
  forall condition . Condition condition => AnyCondition condition


data BuildStepType
  = InvocationBuildStepType
  | AmalgamateFilesBuildStepType
  | CopyFileBuildStepType
  | MakeDirectoryBuildStepType
  | ConditionalBuildStepType


data ConditionType
  = AndConditionType
  | OrConditionType
  | NotConditionType
  | PathExistsConditionType
  | FileExistsConditionType
  | DirectoryExistsConditionType


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


data SourceFile =
  SourceFile {
      _sourceFileLanguage :: Language,
      _sourceFilePath :: Text.Text,
      _sourceFileProvenance :: Provenance
    }


data ObjectFile =
  ObjectFile {
      _objectFilePath :: Text.Text,
      _objectFileProvenance :: Provenance
    }


data ExecutableFile =
  ExecutableFile {
      _executableFilePath :: Text.Text,
      _executableFileProvenance :: Provenance
    }


data LibraryFile =
  LibraryFile {
      _libraryFilePath :: Text.Text,
      _libraryFileProvenance :: Provenance
    }


data InvocationBuildStep =
  InvocationBuildStep {
      _invocationBuildStepExecutable :: ExecutableFile,
      _invocationBuildStepParameters :: [Text.Text],
      _invocationBuildStepInputs :: Set.Set AnyFile,
      _invocationBuildStepOutputs :: Set.Set AnyFile
    }


data AmalgamateFilesBuildStep =
  AmalgamateFilesBuildStep {
     _amalgamateFilesBuildStepOutput :: SourceFile,
     _amalgamateFilesBuildStepInputs :: [SourceFile]
   }


data CopyFileBuildStep =
  CopyFileBuildStep {
      _copyFileBuildStepInput :: AnyFile,
      _copyFileBuildStepOutputPath :: Text.Text
    }


data MakeDirectoryBuildStep =
  MakeDirectoryBuildStep {
      _makeDirectoryBuildStepPath :: Text.Text
    }


data ConditionalBuildStep =
  ConditionalBuildStep {
      _conditionalBuildStepCondition :: AnyCondition,
      _conditionalBuildStepWhenTrue :: [AnyBuildStep],
      _conditionalBuildStepWhenFalse :: [AnyBuildStep]
    }


data AndCondition =
  AndCondition {
      _andConditionItems :: [AnyCondition]
    }


data OrCondition =
  OrCondition {
      _orConditionItems :: [AnyCondition]
    }


data NotCondition =
  NotCondition {
      _notConditionItem :: AnyCondition
    }


data PathExistsCondition =
  PathExistsCondition {
      _pathExistsConditionPath :: Text.Text
    }


data FileExistsCondition =
  FileExistsCondition {
      _fileExistsConditionPath :: Text.Text
    }


data DirectoryExistsCondition =
  DirectoryExistsCondition {
      _directoryExistsConditionPath :: Text.Text
    }


data ExecutableTarget =
  ExecutableTarget {
      _executableTargetName :: Text.Text,
      _executableTargetPrerequisites :: Set.Set Text.Text,
      _executableTargetPrivateHeaders :: Set.Set HeaderFile,
      _executableTargetSources :: Set.Set SourceFile,
      _executableTargetExtraInvocations :: [InvocationBuildStep]
    }


data LibraryTarget =
  LibraryTarget {
      _libraryTargetName :: Text.Text,
      _libraryTargetPrerequisites :: Set.Set Text.Text,
      _libraryTargetPublicHeaders :: Set.Set HeaderFile,
      _libraryTargetPrivateHeaders :: Set.Set HeaderFile,
      _libraryTargetSources :: Set.Set SourceFile,
      _libraryTargetExtraInvocations :: [InvocationBuildStep]
    }


data Mode
  = HelpMode
  | TaskMode Task (Maybe Text.Text)


data Project =
  Project {
      _projectName :: Text.Text,
      _projectTargets :: Map.Map Text.Text AnyTarget
    }


data Defaults =
  Defaults {
      _defaultsTarget :: Maybe Text.Text
    }


data InvocationSpecification =
  InvocationSpecification {
      _invocationSpecificationExecutable :: Text.Text,
      _invocationSpecificationParameters :: [Text.Text],
      _invocationSpecificationInputs :: [Text.Text],
      _invocationSpecificationOutputs :: [Text.Text]
    }


data ExecutableSpecification =
  ExecutableSpecification {
      _executableSpecificationName :: Text.Text,
      _executableSpecificationPrerequisites :: Set.Set Text.Text,
      _executableSpecificationPrivateHeaders :: Set.Set Text.Text,
      _executableSpecificationSources :: Set.Set Text.Text,
      _executableSpecificationExtraInvocations :: [InvocationSpecification]
    }


data LibrarySpecification =
  LibrarySpecification {
      _librarySpecificationName :: Text.Text,
      _librarySpecificationPrerequisites :: Set.Set Text.Text,
      _librarySpecificationPublicHeaders :: Set.Set Text.Text,
      _librarySpecificationPrivateHeaders :: Set.Set Text.Text,
      _librarySpecificationSources :: Set.Set Text.Text,
      _librarySpecificationExtraInvocations :: [InvocationSpecification]
    }


data AnyTargetSpecification =
  forall target . TargetSpecification target => AnyTargetSpecification target


data ProjectSpecification =
  ProjectSpecification {
      _projectSpecificationName :: Text.Text,
      _projectSpecificationDefaultTarget :: Maybe Text.Text,
      _projectSpecificationTargets :: [AnyTargetSpecification],
      _projectSpecificationSubprojects :: Set.Set Text.Text
    }


data SubprojectSpecification =
  SubprojectSpecification {
      _subprojectSpecificationDefaultTarget :: Maybe Text.Text,
      _subprojectSpecificationTargets :: [AnyTargetSpecification],
      _subprojectSpecificationSubprojects :: Set.Set Text.Text
    }


data Buildfile
  = ProjectBuildfile ProjectSpecification
  | SubprojectBuildfile SubprojectSpecification
