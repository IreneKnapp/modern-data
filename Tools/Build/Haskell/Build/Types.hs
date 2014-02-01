{-# LANGUAGE ExistentialQuantification, TemplateHaskell #-}
module Build.Types
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
   BuildStepType(..),
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
   InvocationBuildStep(..),
     invocationBuildStepExecutable,
     invocationBuildStepParameters,
     invocationBuildStepInputs,
     invocationBuildStepOutputs,
   AmalgamateFilesBuildStep(..),
     amalgamateFilesBuildStepOutput,
     amalgamateFilesBuildStepInputs,
   CopyFileBuildStep(..),
     copyFileBuildStepInput,
     copyFileBuildStepOutputPath,
   MakeDirectoryBuildStep(..),
     makeDirectoryBuildStepPath,
   ConditionalBuildStep(..),
     conditionalBuildStepCondition,
     conditionalBuildStepWhenTrue,
     conditionalBuildStepWhenFalse,
   ConditionType(..),
   AnyCondition(..),
   AndCondition(..),
     andConditionItems,
   OrCondition(..),
     orConditionItems,
   NotCondition(..),
     notConditionItem,
   PathExistsCondition(..),
     pathExistsConditionPath,
   FileExistsCondition(..),
     fileExistsConditionPath,
   DirectoryExistsCondition(..),
     directoryExistsConditionPath,
   Mode(..),
   Project(..),
     projectName,
     projectDefaultTarget,
     projectTargets,
   ProjectSpecification(..),
     projectSpecificationName,
     projectSpecificationDefaultTarget,
     projectSpecificationTargets,
     projectSpecificationSubprojects,
   SubprojectSpecification(..),
     subprojectSpecificationParent,
     subprojectSpecificationDefaultTarget,
     subprojectSpecificationTargets,
     subprojectSpecificationSubprojects,
   AnyTargetSpecification(..),
   TargetSpecification(..),
   ExecutableSpecification(..),
     executableSpecificationName,
     executableSpecificationPrerequisites,
     executableSpecificationSources,
     executableSpecificationExtraInvocations,
   LibrarySpecification(..),
     librarySpecificationName,
     librarySpecificationPrerequisites,
     librarySpecificationSources,
     librarySpecificationExtraInvocations,
   InvocationSpecification(..),
     invocationSpecificationExecutable,
     invocationSpecificationParameters,
     invocationSpecificationInputs,
     invocationSpecificationOutputs,
   Buildfile(..))
  where


import qualified Data.Map as Map
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


data BuildStepType
  = InvocationBuildStepType
  | AmalgamateFilesBuildStepType
  | CopyFileBuildStepType
  | MakeDirectoryBuildStepType
  | ConditionalBuildStepType


data AnyBuildStep =
  forall buildStep . BuildStep buildStep => AnyBuildStep buildStep


class (TextShow buildStep, Eq buildStep, Ord buildStep, Typeable buildStep)
      => BuildStep buildStep where
  fromAnyBuildStep :: AnyBuildStep -> Maybe buildStep
  buildStepType :: Getter buildStep BuildStepType
  buildStepInputs :: Getter buildStep (Set.Set AnyFile)
  buildStepOutputs :: Getter buildStep (Set.Set AnyFile)
  performBuildStep :: buildStep -> IO Bool


data AnyTarget = forall target . Target target => AnyTarget target


class (HasName target, TextShow target, Typeable target) => Target target where
  targetBuildSteps :: Task -> target -> [AnyBuildStep]
  targetPrerequisites :: Simple Lens target (Set.Set AnyTarget)
  targetProducts :: Getter target (Set.Set AnyFile)


data ConditionType
  = AndConditionType
  | OrConditionType
  | NotConditionType
  | PathExistsConditionType
  | FileExistsConditionType
  | DirectoryExistsConditionType


data AnyCondition =
  forall condition . Condition condition => AnyCondition condition


class (TextShow condition, Eq condition, Ord condition, Typeable condition)
      => Condition condition where
  fromAnyCondition :: AnyCondition -> Maybe condition
  conditionType :: Getter condition ConditionType
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


data InvocationBuildStep =
  InvocationBuildStep {
      _invocationBuildStepExecutable :: ExecutableFile,
      _invocationBuildStepParameters :: [Text.Text],
      _invocationBuildStepInputs :: Set.Set AnyFile,
      _invocationBuildStepOutputs :: Set.Set AnyFile
    }
makeLenses ''InvocationBuildStep


data AmalgamateFilesBuildStep =
  AmalgamateFilesBuildStep {
     _amalgamateFilesBuildStepOutput :: SourceFile,
     _amalgamateFilesBuildStepInputs :: [SourceFile]
   }
makeLenses ''AmalgamateFilesBuildStep


data CopyFileBuildStep =
  CopyFileBuildStep {
      _copyFileBuildStepInput :: AnyFile,
      _copyFileBuildStepOutputPath :: Text.Text
    }
makeLenses ''CopyFileBuildStep


data MakeDirectoryBuildStep =
  MakeDirectoryBuildStep {
      _makeDirectoryBuildStepPath :: Text.Text
    }
makeLenses ''MakeDirectoryBuildStep


data ConditionalBuildStep =
  ConditionalBuildStep {
      _conditionalBuildStepCondition :: AnyCondition,
      _conditionalBuildStepWhenTrue :: [AnyBuildStep],
      _conditionalBuildStepWhenFalse :: [AnyBuildStep]
    }
makeLenses ''ConditionalBuildStep


data AndCondition =
  AndCondition {
      _andConditionItems :: [AnyCondition]
    }
makeLenses ''AndCondition


data OrCondition =
  OrCondition {
      _orConditionItems :: [AnyCondition]
    }
makeLenses ''OrCondition


data NotCondition =
  NotCondition {
      _notConditionItem :: AnyCondition
    }
makeLenses ''NotCondition


data PathExistsCondition =
  PathExistsCondition {
      _pathExistsConditionPath :: Text.Text
    }
makeLenses ''PathExistsCondition


data FileExistsCondition =
  FileExistsCondition {
      _fileExistsConditionPath :: Text.Text
    }
makeLenses ''FileExistsCondition


data DirectoryExistsCondition =
  DirectoryExistsCondition {
      _directoryExistsConditionPath :: Text.Text
    }
makeLenses ''DirectoryExistsCondition


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


data InvocationSpecification =
  InvocationSpecification {
      _invocationSpecificationExecutable :: Text.Text,
      _invocationSpecificationParameters :: [Text.Text],
      _invocationSpecificationInputs :: [Text.Text],
      _invocationSpecificationOutputs :: [Text.Text]
    }
makeLenses ''InvocationSpecification


data ExecutableSpecification =
  ExecutableSpecification {
      _executableSpecificationName :: Text.Text,
      _executableSpecificationPrerequisites :: Set.Set Text.Text,
      _executableSpecificationSources :: Set.Set Text.Text,
      _executableSpecificationExtraInvocations :: [InvocationSpecification]
    }
makeLenses ''ExecutableSpecification


data LibrarySpecification =
  LibrarySpecification {
      _librarySpecificationName :: Text.Text,
      _librarySpecificationPrerequisites :: Set.Set Text.Text,
      _librarySpecificationSources :: Set.Set Text.Text,
      _librarySpecificationExtraInvocations :: [InvocationSpecification]
    }
makeLenses ''LibrarySpecification


data AnyTargetSpecification =
  forall target . TargetSpecification target => AnyTargetSpecification target


class (HasName target, TextShow target, Typeable target)
      => TargetSpecification target where
  fromAnyTargetSpecification :: AnyTargetSpecification -> Maybe target
  targetSpecificationPrerequisites :: Simple Lens target (Set.Set Text.Text)
  targetSpecificationSources :: Simple Lens target (Set.Set Text.Text)
  targetSpecificationExtraInvocations
    :: Simple Lens target [InvocationSpecification]


data ProjectSpecification =
  ProjectSpecification {
      _projectSpecificationName :: Text.Text,
      _projectSpecificationDefaultTarget :: Maybe Text.Text,
      _projectSpecificationTargets :: [AnyTargetSpecification],
      _projectSpecificationSubprojects :: Set.Set Text.Text
    }
makeLenses ''ProjectSpecification


data SubprojectSpecification =
  SubprojectSpecification {
      _subprojectSpecificationParent :: Text.Text,
      _subprojectSpecificationDefaultTarget :: Maybe Text.Text,
      _subprojectSpecificationTargets :: [AnyTargetSpecification],
      _subprojectSpecificationSubprojects :: Set.Set Text.Text
    }
makeLenses ''SubprojectSpecification


data Buildfile
  = ProjectBuildfile ProjectSpecification
  | SubprojectBuildfile SubprojectSpecification
