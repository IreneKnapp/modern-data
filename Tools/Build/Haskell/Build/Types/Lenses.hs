{-# LANGUAGE TemplateHaskell #-}
module Build.Types.Lenses
  (headerFileLanguage,
   headerFilePath,
   headerFileProvenance,
   sourceFileLanguage,
   sourceFilePath,
   sourceFileProvenance,
   objectFilePath,
   objectFileProvenance,
   executableFilePath,
   executableFileProvenance,
   libraryFilePath,
   libraryFileProvenance,
   executableTargetName,
   executableTargetPrerequisites,
   executableTargetPrivateHeaders,
   executableTargetSources,
   libraryTargetName,
   libraryTargetPrerequisites,
   libraryTargetPublicHeaders,
   libraryTargetPrivateHeaders,
   libraryTargetSources,
   invocationBuildStepExecutable,
   invocationBuildStepParameters,
   invocationBuildStepInputs,
   invocationBuildStepOutputs,
   amalgamateFilesBuildStepOutput,
   amalgamateFilesBuildStepInputs,
   copyFileBuildStepInput,
   copyFileBuildStepOutputPath,
   makeDirectoryBuildStepPath,
   conditionalBuildStepCondition,
   conditionalBuildStepWhenTrue,
   conditionalBuildStepWhenFalse,
   andConditionItems,
   orConditionItems,
   notConditionItem,
   pathExistsConditionPath,
   fileExistsConditionPath,
   directoryExistsConditionPath,
   projectName,
   projectTargets,
   defaultsTarget,
   projectSpecificationName,
   projectSpecificationDefaultTarget,
   projectSpecificationTargets,
   projectSpecificationSubprojects,
   subprojectSpecificationDefaultTarget,
   subprojectSpecificationTargets,
   subprojectSpecificationSubprojects,
   executableSpecificationName,
   executableSpecificationPrerequisites,
   executableSpecificationPrivateHeaders,
   executableSpecificationSources,
   executableSpecificationExtraInvocations,
   librarySpecificationName,
   librarySpecificationPrerequisites,
   librarySpecificationPublicHeaders,
   librarySpecificationPrivateHeaders,
   librarySpecificationSources,
   librarySpecificationExtraInvocations,
   invocationSpecificationExecutable,
   invocationSpecificationParameters,
   invocationSpecificationInputs,
   invocationSpecificationOutputs) where

import Control.Lens
import Data.Function

import Build.Types.Types


makeLenses ''HeaderFile
makeLenses ''SourceFile
makeLenses ''ObjectFile
makeLenses ''ExecutableFile
makeLenses ''LibraryFile
makeLenses ''ExecutableTarget
makeLenses ''LibraryTarget
makeLenses ''InvocationBuildStep
makeLenses ''AmalgamateFilesBuildStep
makeLenses ''CopyFileBuildStep
makeLenses ''MakeDirectoryBuildStep
makeLenses ''ConditionalBuildStep
makeLenses ''AndCondition
makeLenses ''OrCondition
makeLenses ''NotCondition
makeLenses ''PathExistsCondition
makeLenses ''FileExistsCondition
makeLenses ''DirectoryExistsCondition
makeLenses ''Project
makeLenses ''Defaults
makeLenses ''ProjectSpecification
makeLenses ''SubprojectSpecification
makeLenses ''ExecutableSpecification
makeLenses ''LibrarySpecification
makeLenses ''InvocationSpecification
