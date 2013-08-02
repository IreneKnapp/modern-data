{-# LANGUAGE ExistentialQuantification, OverloadedStrings,
             DeriveDataTypeable #-}
module Main (main) where

import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified System.Directory as IO

import Control.Monad
import Data.Function
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Typeable


class TextShow textShow where
  textShow :: textShow -> Text.Text

class Named named where
  namedName :: named -> Text.Text

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

class (Named target, TextShow target) => Target target where
  targetBuildSteps :: Task -> target -> [AnyBuildStep]
  targetPrerequisites :: target -> Set.Set AnyTarget
  targetProducts :: target -> Set.Set AnyFile

data AnyTarget = forall target . Target target => AnyTarget target
instance Eq AnyTarget where
  (==) a b = on (==) namedName a b
instance Ord AnyTarget where
  compare a b = on compare namedName a b
instance Named AnyTarget where
  namedName (AnyTarget target) = namedName target
instance Target AnyTarget where
  targetBuildSteps task (AnyTarget target) = targetBuildSteps task target
  targetPrerequisites (AnyTarget target) = targetPrerequisites target
  targetProducts (AnyTarget target) = targetProducts target
instance TextShow AnyTarget where
  textShow (AnyTarget target) = textShow target

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
  fileType :: file -> FileType
  filePath :: file -> Text.Text
  fileProvenance :: file -> Provenance

data AnyFile = forall file . File file => AnyFile file
  deriving (Typeable)
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
                      Nothing -> on compare fileType a b
instance File AnyFile where
  fromAnyFile (AnyFile file) = cast file
  fileType (AnyFile file) = fileType file
  filePath (AnyFile file) = filePath file
  fileProvenance (AnyFile file) = fileProvenance file
instance TextShow AnyFile where
  textShow (AnyFile file) = textShow file

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
      executableTargetPrerequisites :: Set.Set AnyTarget,
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
      libraryTargetName :: Text.Text,
      libraryTargetPrerequisites :: Set.Set AnyTarget,
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

data Provenance
  = InputProvenance
  | BuiltProvenance
  | SystemProvenance
  deriving (Eq, Ord)
instance TextShow Provenance where
  textShow InputProvenance = "input-provenance"
  textShow BuiltProvenance = "build-provenance"
  textShow SystemProvenance = "system-provenance"

data HeaderFile =
  HeaderFile {
      headerFileLanguage :: Language,
      headerFilePath :: Text.Text,
      headerFileProvenance :: Provenance
    }
  deriving (Typeable)
instance Eq HeaderFile where
  (==) a b =
    foldl1 (&&)
           [on (==) headerFileLanguage a b,
            on (==) headerFilePath a b,
            on (==) headerFileProvenance a b]
instance Ord HeaderFile where
  compare a b =
    mconcat [on compare headerFileLanguage a b,
             on compare headerFilePath a b,
             on compare headerFileProvenance a b]
instance File HeaderFile where
  fromAnyFile (AnyFile file) = cast file
  fileType file = HeaderFileType $ headerFileLanguage file
  filePath = headerFilePath
  fileProvenance = headerFileProvenance
instance TextShow HeaderFile where
  textShow file =
    Text.concat $
      ["(header-file \"",
       headerFilePath file,
       "\" ",
       textShow $ headerFileProvenance file,
       " ",
       textShow $ headerFileLanguage file,
       ")"]

data SourceFile =
  SourceFile {
      sourceFileLanguage :: Language,
      sourceFilePath :: Text.Text,
      sourceFileProvenance :: Provenance
    }
  deriving (Typeable)
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
      objectFilePath :: Text.Text,
      objectFileProvenance :: Provenance
    }
  deriving (Typeable)
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
      executableFilePath :: Text.Text,
      executableFileProvenance :: Provenance
    }
  deriving (Typeable)
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
      libraryFilePath :: Text.Text,
      libraryFileProvenance :: Provenance
    }
  deriving (Typeable)
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

data Language
  = CLanguage
  | HaskellLanguage
  deriving (Eq, Ord)
instance TextShow Language where
  textShow CLanguage = "c-language"
  textShow HaskellLanguage = "haskell-language"


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

