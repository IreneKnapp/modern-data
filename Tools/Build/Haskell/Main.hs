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


instance TextShow Language where
  textShow CLanguage = "c-language"
  textShow HaskellLanguage = "haskell-language"


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
  explainBuildStep (AnyBuildStep buildStep) = explainBuildStep buildStep
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


instance BuildStep CopyFile where
  buildStepInputs =
    to (\copy -> Set.fromList [copy ^. copyFileInput])
  buildStepOutputs =
    to (\copy ->
          Set.fromList
            [set path
                 (Text.concat
                   [copy ^. copyFileOutputPath,
                    Text.pack $ IO.takeFileName $ Text.unpack $
                      copy ^. copyFileInput . path])
                 (copy ^. copyFileInput)])
  explainBuildStep copy =
    Text.intercalate " "
      ["cp", copy ^. copyFileInput . path, copy ^. copyFileOutputPath]
  performBuildStep copy = do
    putStrLn $ Text.unpack $ explainBuildStep copy
instance TextShow CopyFile where
  textShow copy =
    Text.concat $
      ["(copy-file ",
       textShow $ copy ^. copyFileInput,
       " \"",
       copy ^. copyFileOutputPath,
       "\")"]


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
  let buildSteps = targetBuildSteps BinaryTask mainLibrary
      explanation = map explainBuildStep buildSteps
  putStrLn $ Text.unpack $ textShow mainLibrary
  mapM_ (putStrLn . Text.unpack) explanation


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
  in AnyBuildStep $ Invocation {
         _invocationExecutable = ExecutableFile {
             _executableFilePath = "clang",
             _executableFileProvenance = SystemProvenance
           },
         _invocationParameters = ["-O3", "-o", output ^. path, input ^. path],
         _invocationInputs = Set.singleton $ AnyFile input,
         _invocationOutputs = Set.singleton $ AnyFile output
       }


installFileBuildStep :: Text.Text -> AnyFile -> AnyBuildStep
installFileBuildStep path input =
  AnyBuildStep $ CopyFile {
      _copyFileInput = input,
      _copyFileOutputPath = path
    }

