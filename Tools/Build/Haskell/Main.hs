{-# LANGUAGE ExistentialQuantification, OverloadedStrings,
             DeriveDataTypeable, LiberalTypeSynonyms, Rank2Types,
             StandaloneDeriving #-}
module Main (main) where

import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified System.Directory as IO
import qualified System.Exit as IO
import qualified System.FilePath.Posix as IO
import qualified System.Process as IO

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Function
import Data.Functor
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Typeable

import Build.Types


deriving instance Eq Language
deriving instance Eq FileType
deriving instance Eq Task
deriving instance Eq Mode
deriving instance Eq Provenance
deriving instance Eq BuildStepType
deriving instance Eq ConditionType

deriving instance Ord Language
deriving instance Ord FileType
deriving instance Ord Task
deriving instance Ord Mode
deriving instance Ord Provenance
deriving instance Ord BuildStepType
deriving instance Ord ConditionType

deriving instance Typeable AnyFile
deriving instance Typeable HeaderFile
deriving instance Typeable SourceFile
deriving instance Typeable ObjectFile
deriving instance Typeable ExecutableFile
deriving instance Typeable LibraryFile
deriving instance Typeable AnyTarget
deriving instance Typeable ExecutableTarget
deriving instance Typeable LibraryTarget
deriving instance Typeable AnyBuildStep
deriving instance Typeable InvocationBuildStep
deriving instance Typeable AmalgamateFilesBuildStep
deriving instance Typeable CopyFileBuildStep
deriving instance Typeable MakeDirectoryBuildStep
deriving instance Typeable ConditionalBuildStep
deriving instance Typeable AnyCondition
deriving instance Typeable AndCondition
deriving instance Typeable OrCondition
deriving instance Typeable NotCondition
deriving instance Typeable PathExistsCondition
deriving instance Typeable FileExistsCondition
deriving instance Typeable DirectoryExistsCondition
deriving instance Typeable AnyTargetSpecification
deriving instance Typeable ExecutableSpecification
deriving instance Typeable LibrarySpecification


instance TextShow Language where
  textShow CLanguage = "c-language"


instance TextShow Provenance where
  textShow InputProvenance = "input-provenance"


anyFile
    :: forall a f . (Functor f)
    => (forall file . (File file)
        => (a -> f a) -> file -> f file)
    -> ((a -> f a) -> AnyFile -> f AnyFile)
anyFile underlying f (AnyFile file) =
  AnyFile <$> underlying f file
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
  fileType = anyFile fileType
  path = anyFile path
  provenance = anyFile provenance
instance TextShow AnyFile where
  textShow (AnyFile file) = textShow file

instance HasLanguage HeaderFile where
  language = headerFileLanguage
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

instance HasLanguage SourceFile where
  language = sourceFileLanguage
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
    Text.concat
      ["(object-file \"",
       file ^. path,
       "\" ",
       textShow $ file ^. provenance,
       ")"]

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

anyBuildStep
    :: forall a f . (Functor f)
    => (forall buildStep . (BuildStep buildStep)
        => (a -> f a) -> buildStep -> f buildStep)
    -> ((a -> f a) -> AnyBuildStep -> f AnyBuildStep)
anyBuildStep underlying f (AnyBuildStep buildStep) =
  AnyBuildStep <$> underlying f buildStep
instance Eq AnyBuildStep where
  (==) a b =
    case a of
      AnyBuildStep a' -> case fromAnyBuildStep b of
                           Just b' -> (==) a' b'
                           Nothing -> False
instance Ord AnyBuildStep where
  compare a b =
    case a of
      AnyBuildStep a' -> case fromAnyBuildStep b of
                           Just b' -> compare a' b'
                           Nothing -> on compare (view buildStepType) a b
instance BuildStep AnyBuildStep where
  fromAnyBuildStep (AnyBuildStep buildStep) = cast buildStep
  buildStepType = anyBuildStep buildStepType
  buildStepInputs = anyBuildStep buildStepInputs
  buildStepOutputs = anyBuildStep buildStepOutputs
  performBuildStep (AnyBuildStep buildStep) = performBuildStep buildStep
instance TextShow AnyBuildStep where
  textShow (AnyBuildStep buildStep) = textShow buildStep

anyTarget
    :: forall a f . (Functor f)
    => (forall target . (Target target)
        => (a -> f a) -> target -> f target)
    -> ((a -> f a) -> AnyTarget -> f AnyTarget)
anyTarget underlying f (AnyTarget target) =
  AnyTarget <$> underlying f target
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


instance HasName ExecutableTarget where
  name = executableTargetName
instance Target ExecutableTarget where
  targetBuildSteps AmalgamationTask executable = []
  targetBuildSteps BinaryTask executable =
    let compilationSteps =
          (map (compileFileBuildStep $ AnyTarget executable)
               (Set.toList $ view executableTargetSources executable))
    in concat (map (targetBuildSteps BinaryTask)
                   (Set.toList $ view targetPrerequisites executable))
              ++ compilationSteps
              ++ [linkExecutableFileBuildStep
                   (AnyTarget executable)
                   (concatMap
                     (view $ anyBuildStep buildStepOutputs . to Set.toList
                             . to (map fromAnyFile) . to catMaybes)
                     compilationSteps)]
  targetBuildSteps TestTask executable = []
  targetBuildSteps DebugTask executable = []
  targetBuildSteps CleanTask executable = []
  targetPrerequisites = executableTargetPrerequisites
  targetProducts =
    to (\executable ->
          Set.fromList
            [AnyFile $ ExecutableFile {
                 _executableFilePath =
                   Text.concat ["_build/",
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

instance HasName LibraryTarget where
  name = libraryTargetName
instance Target LibraryTarget where
  targetBuildSteps AmalgamationTask library =
    let amalgamationFile = SourceFile {
            _sourceFileLanguage = CLanguage,
            _sourceFilePath =
              Text.concat ["_build/",
                           library ^. name,
                           "/amalgamation/",
                           library ^. name,
                           ".c"],
            _sourceFileProvenance = BuiltProvenance
          }
    in concat
      (map (targetBuildSteps BinaryTask)
           (Set.toList $ view targetPrerequisites library))
      ++ [amalgamateFilesBuildStep amalgamationFile
            (Set.toList $ view libraryTargetSources library)]
  targetBuildSteps BinaryTask library =
    concat (map (targetBuildSteps BinaryTask)
                (Set.toList $ view targetPrerequisites library))
           ++ (map (compileFileBuildStep $ AnyTarget library)
                   (Set.toList $ view libraryTargetSources library))
           ++ (map (installFileBuildStep "include/")
                   (map AnyFile $ Set.toList $
                     view libraryTargetPublicHeaders library))
  targetBuildSteps TestTask library = []
  targetBuildSteps DebugTask library = []
  targetBuildSteps CleanTask library = []
  targetPrerequisites = libraryTargetPrerequisites
  targetProducts =
    to (\library ->
          Set.fromList
            [AnyFile $ LibraryFile {
                 _libraryFilePath =
                   Text.concat ["_build/",
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

instance Eq InvocationBuildStep where
  (==) a b =
    foldl1 (&&)
           [on (==) (view invocationBuildStepExecutable) a b,
            on (==) (view invocationBuildStepParameters) a b,
            on (==) (view invocationBuildStepInputs) a b,
            on (==) (view invocationBuildStepOutputs) a b]
instance Ord InvocationBuildStep where
  compare a b =
    mconcat [on compare (view invocationBuildStepExecutable) a b,
             on compare (view invocationBuildStepParameters) a b,
             on compare (view invocationBuildStepInputs) a b,
             on compare (view invocationBuildStepOutputs) a b]
instance BuildStep InvocationBuildStep where
  fromAnyBuildStep (AnyBuildStep buildStep) = cast buildStep
  buildStepType = to (\_ -> InvocationBuildStepType)
  buildStepInputs = invocationBuildStepInputs
  buildStepOutputs = invocationBuildStepOutputs
  performBuildStep invocation = do
    putStrLn $ Text.unpack $ Text.intercalate " "
      ((invocation ^. invocationBuildStepExecutable . path)
       : invocation ^. invocationBuildStepParameters)
    exitCode <- IO.rawSystem
      (Text.unpack $ invocation ^. invocationBuildStepExecutable . path)
      (map Text.unpack $ invocation ^. invocationBuildStepParameters)
    return $ exitCode == IO.ExitSuccess
instance TextShow InvocationBuildStep where
  textShow invocation =
    Text.concat $
      ["(invocation ",
       textShow $ invocation ^. invocationBuildStepExecutable,
       " (",
       Text.intercalate " " $ invocation ^. invocationBuildStepParameters,
       ") (",
       Text.intercalate " " $ map textShow $ Set.toList
        $ invocation ^. invocationBuildStepInputs,
       ") (",
       Text.intercalate " " $ map textShow $ Set.toList
        $ invocation ^. invocationBuildStepOutputs,
       "))"]


instance Eq AmalgamateFilesBuildStep where
  (==) a b =
    foldl1 (&&)
           [on (==) (view amalgamateFilesBuildStepOutput) a b,
            on (==) (view amalgamateFilesBuildStepInputs) a b]
instance Ord AmalgamateFilesBuildStep where
  compare a b =
    mconcat [on compare (view amalgamateFilesBuildStepOutput) a b,
             on compare (view amalgamateFilesBuildStepInputs) a b]
instance BuildStep AmalgamateFilesBuildStep where
  fromAnyBuildStep (AnyBuildStep buildStep) = cast buildStep
  buildStepType = to (\_ -> AmalgamateFilesBuildStepType)
  buildStepInputs =
    to (\amalgamation -> Set.fromList $ map AnyFile $
          amalgamation ^. amalgamateFilesBuildStepInputs)
  buildStepOutputs =
    to (\amalgamation -> Set.singleton $ AnyFile $
          amalgamation ^. amalgamateFilesBuildStepOutput)
  performBuildStep amalgamation = do
    putStrLn $ Text.unpack $ Text.intercalate " "
      ("amalgamate"
       : amalgamation ^. amalgamateFilesBuildStepOutput . path
       : amalgamation ^. amalgamateFilesBuildStepInputs . to (map (view path)))
    return True
instance TextShow AmalgamateFilesBuildStep where
  textShow amalgamation =
    Text.concat $
      ["(amalgamation ",
       textShow $ amalgamation ^. amalgamateFilesBuildStepOutput,
       " (",
       Text.intercalate " " $ map textShow
        $ amalgamation ^. amalgamateFilesBuildStepInputs,
       "))"]


instance Eq CopyFileBuildStep where
  (==) a b =
    foldl1 (&&)
           [on (==) (view copyFileBuildStepInput) a b,
            on (==) (view copyFileBuildStepOutputPath) a b]
instance Ord CopyFileBuildStep where
  compare a b =
    mconcat [on compare (view copyFileBuildStepInput) a b,
             on compare (view copyFileBuildStepOutputPath) a b]
instance BuildStep CopyFileBuildStep where
  fromAnyBuildStep (AnyBuildStep buildStep) = cast buildStep
  buildStepType = to (\_ -> CopyFileBuildStepType)
  buildStepInputs =
    to (\copy -> Set.fromList [copy ^. copyFileBuildStepInput])
  buildStepOutputs =
    to (\copy ->
          Set.fromList
            [set path
                 (Text.concat
                   [copy ^. copyFileBuildStepOutputPath,
                    Text.pack $ IO.takeFileName $ Text.unpack $
                      copy ^. copyFileBuildStepInput . path])
                 (copy ^. copyFileBuildStepInput)])
  performBuildStep copy = do
    putStrLn $ Text.unpack $ Text.intercalate " "
      ["cp",
       copy ^. copyFileBuildStepInput . path,
       copy ^. copyFileBuildStepOutputPath]
    return True
instance TextShow CopyFileBuildStep where
  textShow copy =
    Text.concat $
      ["(copy-file ",
       textShow $ copy ^. copyFileBuildStepInput,
       " \"",
       copy ^. copyFileBuildStepOutputPath,
       "\")"]


instance Eq MakeDirectoryBuildStep where
  (==) a b =
    foldl1 (&&)
           [on (==) (view makeDirectoryBuildStepPath) a b]
instance Ord MakeDirectoryBuildStep where
  compare a b =
    mconcat [on compare (view makeDirectoryBuildStepPath) a b]
instance BuildStep MakeDirectoryBuildStep where
  fromAnyBuildStep (AnyBuildStep buildStep) = cast buildStep
  buildStepType = to (\_ -> MakeDirectoryBuildStepType)
  buildStepInputs = to (\_ -> Set.empty)
  buildStepOutputs = to (\_ -> Set.empty)
  performBuildStep make = do
    let path = Text.unpack $ make ^. makeDirectoryBuildStepPath
    exists <- IO.doesDirectoryExist path
    if exists
      then return True
      else do
        IO.createDirectory path
        return True
instance TextShow MakeDirectoryBuildStep where
  textShow make =
    Text.concat $
      ["(make-directory ",
       " \"",
       make ^. makeDirectoryBuildStepPath,
       "\")"]


instance Eq ConditionalBuildStep where
  (==) a b =
    foldl1 (&&)
           [on (==) (view conditionalBuildStepCondition) a b,
            on (==) (view conditionalBuildStepWhenTrue) a b,
            on (==) (view conditionalBuildStepWhenFalse) a b]
instance Ord ConditionalBuildStep where
  compare a b =
    mconcat [on compare (view conditionalBuildStepCondition) a b,
             on compare (view conditionalBuildStepWhenTrue) a b,
             on compare (view conditionalBuildStepWhenFalse) a b]
instance BuildStep ConditionalBuildStep where
  fromAnyBuildStep (AnyBuildStep buildStep) = cast buildStep
  buildStepType = to (\_ -> ConditionalBuildStepType)
  buildStepInputs =
    to (\conditional ->
          foldl' (\soFar field ->
                    foldl' Set.union
                           soFar
                           (map (view buildStepInputs)
                                (conditional ^. field)))
                 Set.empty
                 [conditionalBuildStepWhenTrue, conditionalBuildStepWhenFalse])
  buildStepOutputs =
    to (\conditional ->
          foldl' (\soFar field ->
                    foldl' Set.union
                           soFar
                           (map (view buildStepOutputs)
                                (conditional ^. field)))
                 Set.empty
                 [conditionalBuildStepWhenTrue, conditionalBuildStepWhenFalse])
  performBuildStep conditional = do
    value <- testCondition $ conditional ^. conditionalBuildStepCondition
    let field =
          if value
            then conditionalBuildStepWhenTrue
            else conditionalBuildStepWhenFalse
    foldM (\result step -> do
             if result
               then performBuildStep step
               else return False)
          True
          (conditional ^. field)
instance TextShow ConditionalBuildStep where
  textShow conditional =
    Text.concat $
      ["(if ",
       explainCondition $ conditional ^. conditionalBuildStepCondition,
       " ("]
      ++ (map textShow $ conditional ^. conditionalBuildStepWhenTrue)
      ++ [") ("]
      ++ (map textShow $ conditional ^. conditionalBuildStepWhenFalse)
      ++ ["))"]


anyCondition
    :: forall a f . (Functor f)
    => (forall condition . (Condition condition)
        => (a -> f a) -> condition -> f condition)
    -> ((a -> f a) -> AnyCondition -> f AnyCondition)
anyCondition underlying f (AnyCondition condition) =
  AnyCondition <$> underlying f condition
instance Eq AnyCondition where
  (==) a b =
    case a of
      AnyCondition a' -> case fromAnyCondition b of
                           Just b' -> (==) a' b'
                           Nothing -> False
instance Ord AnyCondition where
  compare a b =
    case a of
      AnyCondition a' -> case fromAnyCondition b of
                           Just b' -> compare a' b'
                           Nothing -> on compare (view conditionType) a b
instance Condition AnyCondition where
  fromAnyCondition (AnyCondition condition) = cast condition
  conditionType = anyCondition conditionType
  explainCondition (AnyCondition condition) = explainCondition condition
  testCondition (AnyCondition condition) = testCondition condition
instance TextShow AnyCondition where
  textShow (AnyCondition condition) = textShow condition


instance Eq AndCondition where
  (==) a b =
    foldl1 (&&)
           [on (==) (view andConditionItems) a b]
instance Ord AndCondition where
  compare a b =
    mconcat [on compare (view andConditionItems) a b]
instance Condition AndCondition where
  fromAnyCondition (AnyCondition condition) = cast condition
  conditionType = to (\_ -> AndConditionType)
  explainCondition condition =
    Text.concat
      ["(and ",
       Text.intercalate " "
        $ map explainCondition (condition ^. andConditionItems),
       ")"]
  testCondition condition = do
    foldM (\result item -> do
             case result of
               True -> testCondition item
               False -> return False)
          True
          (condition ^. andConditionItems)
instance TextShow AndCondition where
  textShow condition =
    Text.concat
      ["(and ",
       Text.intercalate " "
        $ map explainCondition (condition ^. andConditionItems),
       ")"]


instance Eq OrCondition where
  (==) a b =
    foldl1 (&&)
           [on (==) (view orConditionItems) a b]
instance Ord OrCondition where
  compare a b =
    mconcat [on compare (view orConditionItems) a b]
instance Condition OrCondition where
  fromAnyCondition (AnyCondition condition) = cast condition
  conditionType = to (\_ -> OrConditionType)
  explainCondition condition =
    Text.concat
      ["(or ",
       Text.intercalate " "
        $ map explainCondition (condition ^. orConditionItems),
       ")"]
  testCondition condition = do
    foldM (\result item -> do
             case result of
               True -> return True
               False -> testCondition item)
          False
          (condition ^. orConditionItems)
instance TextShow OrCondition where
  textShow condition =
    Text.concat
      ["(or ",
       Text.intercalate " "
        $ map explainCondition (condition ^. orConditionItems),
       ")"]


instance Eq NotCondition where
  (==) a b =
    foldl1 (&&)
           [on (==) (view notConditionItem) a b]
instance Ord NotCondition where
  compare a b =
    mconcat [on compare (view notConditionItem) a b]
instance Condition NotCondition where
  fromAnyCondition (AnyCondition condition) = cast condition
  conditionType = to (\_ -> NotConditionType)
  explainCondition condition =
    Text.concat
      ["(not ", explainCondition $ condition ^. notConditionItem, ")"]
  testCondition condition = do
    result <- testCondition $ condition ^. notConditionItem
    return $ not result
instance TextShow NotCondition where
  textShow condition =
    Text.concat
      ["(not ", explainCondition $ condition ^. notConditionItem, ")"]


instance Eq PathExistsCondition where
  (==) a b =
    foldl1 (&&)
           [on (==) (view pathExistsConditionPath) a b]
instance Ord PathExistsCondition where
  compare a b =
    mconcat [on compare (view pathExistsConditionPath) a b]
instance Condition PathExistsCondition where
  fromAnyCondition (AnyCondition condition) = cast condition
  conditionType = to (\_ -> PathExistsConditionType)
  explainCondition condition =
    Text.concat
      ["(path-exists ", condition ^. pathExistsConditionPath, ")"]
  testCondition condition = do
    return False
instance TextShow PathExistsCondition where
  textShow condition =
    Text.concat
      ["(path-exists ", condition ^. pathExistsConditionPath, ")"]


instance Eq FileExistsCondition where
  (==) a b =
    foldl1 (&&)
           [on (==) (view fileExistsConditionPath) a b]
instance Ord FileExistsCondition where
  compare a b =
    mconcat [on compare (view fileExistsConditionPath) a b]
instance Condition FileExistsCondition where
  fromAnyCondition (AnyCondition condition) = cast condition
  conditionType = to (\_ -> FileExistsConditionType)
  explainCondition condition =
    Text.concat
      ["(file-exists ", condition ^. fileExistsConditionPath, ")"]
  testCondition condition = do
    return False
instance TextShow FileExistsCondition where
  textShow condition =
    Text.concat
      ["(file-exists ", condition ^. fileExistsConditionPath, ")"]


instance Eq DirectoryExistsCondition where
  (==) a b =
    foldl1 (&&)
           [on (==) (view directoryExistsConditionPath) a b]
instance Ord DirectoryExistsCondition where
  compare a b =
    mconcat [on compare (view directoryExistsConditionPath) a b]
instance Condition DirectoryExistsCondition where
  fromAnyCondition (AnyCondition condition) = cast condition
  conditionType = to (\_ -> DirectoryExistsConditionType)
  explainCondition condition =
    Text.concat
      ["(directory-exists ", condition ^. directoryExistsConditionPath, ")"]
  testCondition condition = do
    return False
instance TextShow DirectoryExistsCondition where
  textShow condition =
    Text.concat
      ["(directory-exists ", condition ^. directoryExistsConditionPath, ")"]


instance Eq Project where
  (==) a b = on (==) _projectName a b
instance Ord Project where
  compare a b = on compare _projectName a b
instance HasName Project where
  name = projectName


parseObject
    :: Set.Set Text.Text
    -> Set.Set Text.Text
    -> (HashMap.HashMap Text.Text JSON.Value -> JSON.Parser a)
    -> JSON.Value
    -> JSON.Parser a
parseObject mandatoryKeys optionalKeys subparser (JSON.Object object) = do
  let presentKeys = Set.fromList $ HashMap.keys object
      allowedKeys = Set.union mandatoryKeys optionalKeys
      missingKeys = Set.difference mandatoryKeys presentKeys
      extraKeys = Set.difference presentKeys allowedKeys
      missingKeysMessage =
        if Set.null missingKeys
          then Nothing
          else Just $ Text.concat
                 ["Missing keys ",
                  Text.intercalate " " $ Set.toList missingKeys]
      extraKeysMessage =
        if Set.null extraKeys
          then Nothing
          else Just $ Text.concat
                 ["Extra keys ",
                  Text.intercalate " " $ Set.toList extraKeys]
      messages = catMaybes [missingKeysMessage, extraKeysMessage]
  if messages == []
    then subparser object
    else fail $ Text.unpack $ Text.intercalate "; " messages
parseObject _ _ _ _ = mzero


instance JSON.FromJSON Buildfile where
  parseJSON (JSON.Object object) = do
    type' <- object JSON..: "type" :: JSON.Parser Text.Text
    let subobject = JSON.Object $ HashMap.delete "type" object
    case type' of
      "project" -> ProjectBuildfile <$> JSON.parseJSON subobject
      "subproject" -> SubprojectBuildfile <$> JSON.parseJSON subobject
      _ -> mzero
  parseJSON _ = mzero


instance JSON.FromJSON ProjectSpecification where
  parseJSON =
    parseObject (Set.fromList ["name"])
                (Set.fromList ["default-target", "targets", "subprojects"])
                (\object ->
                   ProjectSpecification
                     <$> object JSON..: "name"
                     <*> object JSON..:? "default-target"
                     <*> object JSON..:? "targets" JSON..!= []
                     <*> object JSON..:? "subprojects" JSON..!= Set.empty)


instance JSON.FromJSON SubprojectSpecification where
  parseJSON =
    parseObject (Set.fromList [])
                (Set.fromList ["default-target", "targets", "subprojects"])
                (\object ->
                   SubprojectSpecification
                     <$> object JSON..:? "default-target"
                     <*> object JSON..:? "targets" JSON..!= []
                     <*> object JSON..:? "subprojects" JSON..!= Set.empty)


instance TextShow InvocationSpecification where
  textShow specification =
    Text.concat $
      ["(invocation-specification \"",
       specification ^. invocationSpecificationExecutable,
       "\" (",
       Text.intercalate " "
         (specification ^. invocationSpecificationParameters),
       ") (",
       Text.intercalate " "
         (specification ^. invocationSpecificationInputs),
       ") (",
       Text.intercalate " "
         (specification ^. invocationSpecificationOutputs),
       "))"]
instance JSON.FromJSON InvocationSpecification where
  parseJSON =
    parseObject (Set.fromList ["executable"])
                (Set.fromList ["parameters", "inputs", "outputs"])
                (\object ->
                   InvocationSpecification
                     <$> object JSON..: "executable"
                     <*> object JSON..:? "parameters" JSON..!= []
                     <*> object JSON..:? "inputs" JSON..!= []
                     <*> object JSON..:? "outputs" JSON..!= [])


anyTargetSpecification
    :: forall a f . (Functor f)
    => (forall specification . (TargetSpecification specification)
        => (a -> f a) -> specification -> f specification)
    -> ((a -> f a) -> AnyTargetSpecification -> f AnyTargetSpecification)
anyTargetSpecification underlying f (AnyTargetSpecification file) =
  AnyTargetSpecification <$> underlying f file
instance JSON.FromJSON AnyTargetSpecification where
  parseJSON (JSON.Object object) = do
    type' <- object JSON..: "type" :: JSON.Parser Text.Text
    let subobject = JSON.Object $ HashMap.delete "type" object
    case type' of
      "executable" -> AnyTargetSpecification
        <$> (JSON.parseJSON subobject :: JSON.Parser ExecutableSpecification)
      "library" -> AnyTargetSpecification
        <$> (JSON.parseJSON subobject :: JSON.Parser LibrarySpecification)
      _ -> mzero
  parseJSON _ = mzero
instance HasName AnyTargetSpecification where
  name = anyTargetSpecification name
instance TargetSpecification AnyTargetSpecification where
  fromAnyTargetSpecification (AnyTargetSpecification specification) =
    cast specification
  targetSpecificationPrerequisites =
    anyTargetSpecification targetSpecificationPrerequisites
  targetSpecificationSources =
    anyTargetSpecification targetSpecificationSources
  targetSpecificationExtraInvocations =
    anyTargetSpecification targetSpecificationExtraInvocations
instance TextShow AnyTargetSpecification where
  textShow (AnyTargetSpecification specification) = textShow specification


instance HasName ExecutableSpecification where
  name = executableSpecificationName
instance TextShow ExecutableSpecification where
  textShow specification =
    Text.concat $
      ["(library-specification \"",
       specification ^. name,
       "\" (",
       Text.intercalate " "
         (Set.elems $ specification ^. executableSpecificationPrerequisites),
       ") (",
       Text.intercalate " "
         (Set.elems $ specification ^. executableSpecificationSources),
       ") (",
       Text.intercalate " " $ map textShow
         (specification ^. executableSpecificationExtraInvocations),
       "))"]
instance TargetSpecification ExecutableSpecification where
  fromAnyTargetSpecification (AnyTargetSpecification specification) =
    cast specification
  targetSpecificationPrerequisites = executableSpecificationPrerequisites
  targetSpecificationSources = executableSpecificationSources
  targetSpecificationExtraInvocations = executableSpecificationExtraInvocations
instance JSON.FromJSON ExecutableSpecification where
  parseJSON =
    parseObject (Set.fromList ["name", "sources"])
                (Set.fromList ["prerequisites", "extra-invocations"])
                (\object ->
                   ExecutableSpecification
                     <$> object JSON..: "name"
                     <*> object JSON..:? "prerequisites" JSON..!= Set.empty
                     <*> object JSON..: "sources"
                     <*> object JSON..:? "extra-invocations" JSON..!= [])


instance HasName LibrarySpecification where
  name = librarySpecificationName
instance TextShow LibrarySpecification where
  textShow specification =
    Text.concat $
      ["(library-specification \"",
       specification ^. name,
       "\" (",
       Text.intercalate " "
         (Set.elems $ specification ^. librarySpecificationPrerequisites),
       ") (",
       Text.intercalate " "
         (Set.elems $ specification ^. librarySpecificationSources),
       ") (",
       Text.intercalate " " $ map textShow
         (specification ^. librarySpecificationExtraInvocations),
       "))"]
instance TargetSpecification LibrarySpecification where
  fromAnyTargetSpecification (AnyTargetSpecification specification) =
    cast specification
  targetSpecificationPrerequisites = librarySpecificationPrerequisites
  targetSpecificationSources = librarySpecificationSources
  targetSpecificationExtraInvocations = librarySpecificationExtraInvocations
instance JSON.FromJSON LibrarySpecification where
  parseJSON =
    parseObject (Set.fromList ["name", "sources"])
                (Set.fromList ["prerequisites", "extra-invocations"])
                (\object ->
                   LibrarySpecification
                     <$> object JSON..: "name"
                     <*> object JSON..:? "prerequisites" JSON..!= Set.empty
                     <*> object JSON..: "sources"
                     <*> object JSON..:? "extra-invocations" JSON..!= [])


main :: IO ()
main = do
  maybeProjectAndTarget <- loadProject Nothing
  result <- case maybeProjectAndTarget of
              Nothing -> return False
              Just (project, target) -> do
                foldM (\result step -> do
                         if result
                           then performBuildStep step
                           else return False)
                      True
                      (outputtingBuildSteps $
                        targetBuildSteps AmalgamationTask target)
  if result
    then putStrLn "\nSuccess"
    else putStrLn "\nFailure"


loadProject
  :: Maybe Text.Text
  -> IO (Maybe (Project, AnyTarget))
loadProject maybeSpecifiedTargetName = do
  let buildfileFileName :: IO.FilePath
      buildfileFileName = "Buildfile"
      ensureBuildfileCached
        :: IO.FilePath
        -> Map.Map IO.FilePath Buildfile
        -> IO.FilePath
        -> IO (Map.Map IO.FilePath Buildfile)
      ensureBuildfileCached projectRootPath buildfileCache filePath = do
        case Map.lookup filePath buildfileCache of
          Just buildfile -> return buildfileCache
          Nothing -> do
            maybeBuildfile <- loadBuildfile (projectRootPath IO.</> filePath)
            case maybeBuildfile of
              Nothing -> do
                putStrLn $ concat
                  ["Unable to load the buildfile \"",
                   filePath,
                   "\"."]
                return (buildfileCache)
              Just buildfile -> do
                return $ Map.insert filePath buildfile buildfileCache
      prefixCachePaths
        :: IO.FilePath
        -> Map.Map IO.FilePath Buildfile
        -> Map.Map IO.FilePath Buildfile
      prefixCachePaths prefixPath buildfileCache =
        Map.mapKeys (\path -> prefixPath IO.</> path) buildfileCache
      loadAllBuildfilesUpward
        :: IO.FilePath
        -> Map.Map IO.FilePath Buildfile
        -> IO (IO.FilePath, Map.Map IO.FilePath Buildfile, Maybe IO.FilePath)
      loadAllBuildfilesUpward projectRootPath buildfileCache = do
        buildfileCache <-
          ensureBuildfileCached projectRootPath buildfileCache
            buildfileFileName
        case Map.lookup buildfileFileName buildfileCache of
          Just (ProjectBuildfile _) ->
            return (projectRootPath, buildfileCache, Just buildfileFileName)
          Just (SubprojectBuildfile subprojectSpecification) -> do
            loadAllBuildfilesUpward
              (IO.dropTrailingPathSeparator $ IO.dropFileName projectRootPath)
              (prefixCachePaths
                (IO.addTrailingPathSeparator $ IO.takeFileName projectRootPath)
                buildfileCache)
          Nothing -> return (projectRootPath, buildfileCache, Nothing)
      loadAllBuildfilesDownward
        :: IO.FilePath
        -> Map.Map IO.FilePath Buildfile
        -> IO.FilePath
        -> Bool
        -> IO (Map.Map IO.FilePath Buildfile, Bool)
      loadAllBuildfilesDownward
          projectRootPath buildfileCache filePath atTopLevel = do
        buildfileCache <-
          ensureBuildfileCached projectRootPath buildfileCache filePath
        let loadImmediatelyDownward
              :: Set.Set Text.Text
              -> IO (Map.Map IO.FilePath Buildfile, Bool)
            loadImmediatelyDownward subprojectPathsSet = do
              foldM (\(buildfileCache, resultSoFar) subprojectPath -> do
                       let localBaseDirectoryPath = IO.takeDirectory filePath
                           subprojectDirectoryPath =
                             IO.addTrailingPathSeparator
                               (Text.unpack subprojectPath)
                           subprojectBuildfilePath =
                             if localBaseDirectoryPath == "."
                               then subprojectDirectoryPath
                                    IO.</> buildfileFileName
                               else localBaseDirectoryPath
                                    IO.</> subprojectDirectoryPath
                                    IO.</> buildfileFileName
                       (buildfileCache, resultHere) <-
                        loadAllBuildfilesDownward
                          projectRootPath
                          buildfileCache
                          subprojectBuildfilePath
                          False
                       return (buildfileCache, resultSoFar && resultHere))
                    (buildfileCache, True)
                    (Set.elems subprojectPathsSet)
        case Map.lookup filePath buildfileCache of
          Just (ProjectBuildfile projectSpecification) -> do
            if atTopLevel
              then loadImmediatelyDownward
                     (projectSpecification
                      ^. projectSpecificationSubprojects)
              else do
                putStrLn $ concat
                  ["Expected a subproject buildfile, ",
                   "but found a whole-project one in \"",
                   filePath,
                   "\"."]
                return (buildfileCache, False)
          Just (SubprojectBuildfile subprojectSpecification) -> do
            if atTopLevel
              then do
                putStrLn $ concat
                  ["Expected a whole-project buildfile, ",
                   "but found a subproject one in \"",
                   filePath,
                   "\"."]
                return (buildfileCache, False)
              else loadImmediatelyDownward
                     (subprojectSpecification
                      ^. subprojectSpecificationSubprojects)
          Nothing -> return (buildfileCache, False)
  projectRootPath <- IO.getCurrentDirectory
  (projectRootPath, buildfileCache, maybeRootBuildfilePath) <-
    loadAllBuildfilesUpward projectRootPath Map.empty
  case maybeRootBuildfilePath of
    Just rootBuildfilePath -> do
      (buildfileCache, result) <-
        loadAllBuildfilesDownward
          projectRootPath buildfileCache rootBuildfilePath True
      if result
        then do
          putStrLn $ concat
            ["The project root directory is \"",
             IO.addTrailingPathSeparator projectRootPath,
             "\"."]
          IO.exitSuccess -- TODO
        else do
          putStrLn $ concat
            ["Stopping before doing anything, ",
             "as there were buildfile problems."]
          return Nothing
    Nothing -> return Nothing


loadBuildfile :: IO.FilePath -> IO (Maybe Buildfile)
loadBuildfile filePath = do
  exists <- IO.doesFileExist filePath
  if exists
    then do
      byteString <- LazyByteString.readFile filePath
      case JSON.eitherDecode' byteString of
        Left message -> do
          putStrLn $ concat
            ["Failure while parsing JSON from \"",
             filePath,
             "\": ",
             message]
          return Nothing
        Right buildfile -> return $ Just buildfile
    else do
      putStrLn $ concat ["No buildfile exists at ", filePath]
      return Nothing


makeProject :: Text.Text -> IO Project
makeProject name = do
  return $ Project {
               _projectName = name,
               _projectDefaultTarget = Nothing,
               _projectTargets = Set.empty
             }


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


makeLibrary :: Text.Text -> Text.Text -> Set.Set Text.Text -> IO LibraryTarget
makeLibrary name directory publicHeadersIn = do
  files <- scanDirectory directory
  let allHeaders =
        Set.fromList $ catMaybes $ map fromAnyFile $ Set.toList files
      sources =
        Set.fromList $ catMaybes $ map fromAnyFile $ Set.toList files
      publicHeaders =
        Set.filter (\header -> Set.member (view path header) publicHeadersIn)
                   allHeaders
      privateHeaders = Set.difference allHeaders publicHeaders
      missingHeaders =
        Set.difference publicHeadersIn $ Set.map (view path) allHeaders
  if Set.null missingHeaders
    then return $ LibraryTarget {
                      _libraryTargetName = name,
                      _libraryTargetPrerequisites = Set.empty,
                      _libraryTargetPublicHeaders = Set.empty,
                      _libraryTargetPrivateHeaders = privateHeaders,
                      _libraryTargetSources = sources
                    }
    else fail $ Text.unpack $
      Text.concat ["Headers not found: ",
                   Text.intercalate ", " $ Set.toList $ missingHeaders,
                   "."]


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
                           (".c", SourceFileType CLanguage)]
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


buildStepOutputDirectories
    :: (BuildStep buildStep) => Getter buildStep (Set.Set Text.Text)
buildStepOutputDirectories = to $ \buildStep ->
  let innermostDirectories =
        Set.map (Text.pack . IO.dropFileName . Text.unpack . view path)
                (buildStep ^. buildStepOutputs)
      andParents soFar directory =
        let parentOf directory =
              Text.pack $ IO.takeDirectory $ Text.unpack directory
            parent = parentOf directory
        in if parentOf parent == parent
          then soFar
          else andParents (Set.insert parent soFar) parent
  in Set.foldl (\soFar directory ->
                  Set.union soFar (andParents Set.empty directory))
               Set.empty
               innermostDirectories


outputtingBuildSteps :: [AnyBuildStep] -> [AnyBuildStep]
outputtingBuildSteps steps =
  let ensureDirectory path =
        [AnyBuildStep $ ConditionalBuildStep {
             _conditionalBuildStepCondition =
               AnyCondition $ NotCondition {
                   _notConditionItem =
                     AnyCondition $ DirectoryExistsCondition {
                         _directoryExistsConditionPath = path
                       }
                 },
             _conditionalBuildStepWhenTrue =
               [AnyBuildStep $ MakeDirectoryBuildStep {
                    _makeDirectoryBuildStepPath = path
                  }],
             _conditionalBuildStepWhenFalse = []
           }]
      (result, _) =
        foldl' (\(soFar, directoriesSoFar) step ->
                  let directoriesHere =
                        Set.difference (step ^. buildStepOutputDirectories)
                                       directoriesSoFar
                  in (soFar
                      ++ (concatMap ensureDirectory
                                    (Set.toList directoriesHere))
                      ++ [step],
                      Set.union directoriesSoFar directoriesHere))
               ([], Set.empty)
               steps
  in result


compileInvocationBuildStep
    :: [Text.Text] -> [Text.Text] -> [AnyFile] -> AnyFile -> AnyBuildStep
compileInvocationBuildStep includeDirectories phaseArguments inputs output =
  AnyBuildStep $ InvocationBuildStep {
      _invocationBuildStepExecutable = ExecutableFile {
          _executableFilePath = "clang",
          _executableFileProvenance = SystemProvenance
        },
      _invocationBuildStepParameters =
        ["-O3"]
        ++ phaseArguments
        ++ ["-o", output ^. path]
        ++ (concatMap (\directory -> ["-I", directory]) includeDirectories)
        ++ (map (view path) inputs),
      _invocationBuildStepInputs = Set.fromList inputs,
      _invocationBuildStepOutputs = Set.singleton output
    }


compileFileBuildStep :: AnyTarget -> SourceFile -> AnyBuildStep
compileFileBuildStep target input =
  let output = ObjectFile {
          _objectFilePath =
            Text.concat ["_build/",
                         target ^. name,
                         "/binary/objects/",
                         Text.pack $ IO.takeBaseName $ Text.unpack $
                           input ^. path,
                         ".o"],
          _objectFileProvenance = BuiltProvenance
        }
      includeDirectories =
        [Text.pack $ IO.dropFileName $ Text.unpack $ input ^. path]
  in compileInvocationBuildStep
       includeDirectories ["-c"] [AnyFile input] (AnyFile output)


linkExecutableFileBuildStep :: AnyTarget -> [ObjectFile] -> AnyBuildStep
linkExecutableFileBuildStep target inputs =
  let output = ExecutableFile {
          _executableFilePath =
            Text.concat ["_build/",
                         target ^. name,
                         "/binary/bin/",
                         target ^. name],
          _executableFileProvenance = BuiltProvenance
        }
  in compileInvocationBuildStep [] [] (map AnyFile inputs) (AnyFile output)


installFileBuildStep :: Text.Text -> AnyFile -> AnyBuildStep
installFileBuildStep path input =
  AnyBuildStep $ CopyFileBuildStep {
      _copyFileBuildStepInput = input,
      _copyFileBuildStepOutputPath = path
    }


amalgamateFilesBuildStep :: SourceFile -> [SourceFile] -> AnyBuildStep
amalgamateFilesBuildStep output inputs =
  AnyBuildStep $ AmalgamateFilesBuildStep {
      _amalgamateFilesBuildStepOutput = output,
      _amalgamateFilesBuildStepInputs = inputs
    }
