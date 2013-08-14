{-# LANGUAGE ExistentialQuantification, OverloadedStrings,
             DeriveDataTypeable, LiberalTypeSynonyms, Rank2Types,
             StandaloneDeriving #-}
module Main (main) where

import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified System.Directory as IO
import qualified System.FilePath.Posix as IO

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

deriving instance Ord Language
deriving instance Ord FileType
deriving instance Ord Task
deriving instance Ord Mode
deriving instance Ord Provenance

deriving instance Typeable AnyFile
deriving instance Typeable HeaderFile
deriving instance Typeable SourceFile
deriving instance Typeable ObjectFile
deriving instance Typeable ExecutableFile
deriving instance Typeable LibraryFile
deriving instance Typeable AnyTarget
deriving instance Typeable ExecutableTarget
deriving instance Typeable LibraryTarget
deriving instance Typeable AnyCondition
deriving instance Typeable AndCondition
deriving instance Typeable OrCondition
deriving instance Typeable NotCondition
deriving instance Typeable PathExistsCondition
deriving instance Typeable FileExistsCondition
deriving instance Typeable DirectoryExistsCondition


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
instance BuildStep AnyBuildStep where
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

instance HasName LibraryTarget where
  name = libraryTargetName
instance Target LibraryTarget where
  targetBuildSteps AmalgamationTask library = []
  targetBuildSteps BinaryTask library =
    outputtingBuildSteps $
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

instance BuildStep InvocationBuildStep where
  buildStepInputs = invocationBuildStepInputs
  buildStepOutputs = invocationBuildStepOutputs
  performBuildStep invocation = do
    putStrLn $ Text.unpack $ Text.intercalate " "
      ((invocation ^. invocationBuildStepExecutable . path)
       : invocation ^. invocationBuildStepParameters)
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


instance BuildStep CopyFileBuildStep where
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
instance TextShow CopyFileBuildStep where
  textShow copy =
    Text.concat $
      ["(copy-file ",
       textShow $ copy ^. copyFileBuildStepInput,
       " \"",
       copy ^. copyFileBuildStepOutputPath,
       "\")"]


instance BuildStep MakeDirectoryBuildStep where
  buildStepInputs = to (\_ -> Set.empty)
  buildStepOutputs = to (\_ -> Set.empty)
  performBuildStep make = do
    putStrLn $ Text.unpack $ Text.intercalate " "
      ["mkdir", make ^. makeDirectoryBuildStepPath]
instance TextShow MakeDirectoryBuildStep where
  textShow make =
    Text.concat $
      ["(make-directory ",
       " \"",
       make ^. makeDirectoryBuildStepPath,
       "\")"]


instance BuildStep ConditionalBuildStep where
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
    mapM_ performBuildStep (conditional ^. field)
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


instance Condition AnyCondition where
  explainCondition (AnyCondition condition) = explainCondition condition
  testCondition (AnyCondition condition) = testCondition condition
instance TextShow AnyCondition where
  textShow (AnyCondition condition) = textShow condition


instance Condition AndCondition where
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


instance Condition OrCondition where
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


instance Condition NotCondition where
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


instance Condition PathExistsCondition where
  explainCondition condition =
    Text.concat
      ["(path-exists ", condition ^. pathExistsConditionPath, ")"]
  testCondition condition = do
    return False
instance TextShow PathExistsCondition where
  textShow condition =
    Text.concat
      ["(path-exists ", condition ^. pathExistsConditionPath, ")"]


instance Condition FileExistsCondition where
  explainCondition condition =
    Text.concat
      ["(file-exists ", condition ^. fileExistsConditionPath, ")"]
  testCondition condition = do
    return False
instance TextShow FileExistsCondition where
  textShow condition =
    Text.concat
      ["(file-exists ", condition ^. fileExistsConditionPath, ")"]


instance Condition DirectoryExistsCondition where
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


main :: IO ()
main = do
  project <- makeProject "Modern Data"
  mainLibrary <- makeLibrary "modern" "../../C/library/" $
    Set.fromList ["../../C/library/modern.h"]
  makeKeywordsExecutable <-
    makeExecutable "make-keywords" "../../C/tools/make-keywords/"
  mainLibrary <- return $
    over targetPrerequisites
         (Set.insert $ AnyTarget makeKeywordsExecutable)
         mainLibrary
  project <- return $
    over projectTargets
         (Set.insert $ AnyTarget mainLibrary)
         project
  mapM_ performBuildStep (targetBuildSteps BinaryTask mainLibrary)


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
  Set.map (Text.pack . IO.dropFileName . Text.unpack . view path)
          (buildStep ^. buildStepOutputs)


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


compileFileBuildStep :: AnyTarget -> SourceFile -> AnyBuildStep
compileFileBuildStep target input =
  let output = ObjectFile {
          _objectFilePath =
            Text.concat ["dist/",
                         target ^. name,
                         "/binary/objects/",
                         Text.pack $ IO.takeFileName $ Text.unpack $
                           input ^. path],
          _objectFileProvenance = BuiltProvenance
        }
  in AnyBuildStep $ InvocationBuildStep {
         _invocationBuildStepExecutable = ExecutableFile {
             _executableFilePath = "clang",
             _executableFileProvenance = SystemProvenance
           },
         _invocationBuildStepParameters =
           ["-O3", "-o", output ^. path, input ^. path],
         _invocationBuildStepInputs = Set.singleton $ AnyFile input,
         _invocationBuildStepOutputs = Set.singleton $ AnyFile output
       }


installFileBuildStep :: Text.Text -> AnyFile -> AnyBuildStep
installFileBuildStep path input =
  AnyBuildStep $ CopyFileBuildStep {
      _copyFileBuildStepInput = input,
      _copyFileBuildStepOutputPath = path
    }

