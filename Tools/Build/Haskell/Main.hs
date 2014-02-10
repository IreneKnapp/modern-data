{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified System.Directory as IO
import qualified System.Environment as IO
import qualified System.FilePath.Posix as IO

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Functor
import Data.List
import Data.Maybe

import Build.Types


main :: IO ()
main = do
  arguments <- IO.getArgs
  let mode = case arguments of
               ["amalgamation"] ->
                 TaskMode AmalgamationTask Nothing
               ["amalgamation", targetName] ->
                 TaskMode AmalgamationTask (Just $ Text.pack targetName)
               ["binary"] ->
                 TaskMode BinaryTask Nothing
               ["binary", targetName] ->
                 TaskMode BinaryTask (Just $ Text.pack targetName)
               ["test"] ->
                 TaskMode TestTask Nothing
               ["test", targetName] ->
                 TaskMode TestTask (Just $ Text.pack targetName)
               ["debug"] ->
                 TaskMode DebugTask Nothing
               ["debug", targetName] ->
                 TaskMode DebugTask (Just $ Text.pack targetName)
               ["clean"] ->
                 TaskMode CleanTask Nothing
               ["clean", targetName] ->
                 TaskMode CleanTask (Just $ Text.pack targetName)
               [] -> TaskMode BinaryTask Nothing
               [targetName] ->
                 TaskMode BinaryTask (Just $ Text.pack targetName)
               _ -> HelpMode
  case mode of
    HelpMode -> do
      putStrLn "Usage: build [<task>] [<target>]"
      putStrLn $ concat
        ["<task> is one of \"amalgamation\", \"binary\", \"test\", ",
         "\"debug\", and \"clean\".  If omitted, it defaults to \"binary\"."]
      putStrLn $ concat
        ["<target> is (at most one) target name as specified in any ",
         "buildfile in this project.  If omitted, it defaults to the target ",
         "specified as the default in the current directory's buildfile; if ",
         "there is none specified there, building is not attempted."]
    TaskMode task maybeExplicitTargetName -> do
      result <- do
        maybeProjectAndDefaults <- loadProject
        case maybeProjectAndDefaults of
          Nothing -> do
            putStrLn "Project wasn't able to load."
            return False
          Just (project, defaults) -> do
            case defaults ^. defaultsTarget of
              Nothing -> do
                putStrLn
                  "No default target specified in this directory's Buildfile."
                return False
              Just targetName -> do
                case Map.lookup targetName (project ^. projectTargets) of
                  Nothing -> do
                    putStrLn $ concat
                      ["The specified target, \"",
                       Text.unpack targetName,
                       "\", is not defined by any Buildfile in this project."]
                    return False
                  Just target -> do
                    IO.setCurrentDirectory $ Text.unpack $
                      project ^. projectRootPath
                    foldM (\result step -> do
                             if result
                               then performBuildStep step
                               else return False)
                          True
                          (outputtingBuildSteps project $
                            targetBuildSteps project task target)
      if result
        then putStrLn "\nBuild succeeded."
        else putStrLn "\nBuild FAILED."
  

loadProject
  :: IO (Maybe (Project, Defaults))
loadProject = do
  let buildfileFileName :: IO.FilePath
      buildfileFileName = "Buildfile"
      ensureBuildfileCached
        :: IO.FilePath
        -> Map.Map IO.FilePath Buildfile
        -> IO.FilePath
        -> IO (Map.Map IO.FilePath Buildfile)
      ensureBuildfileCached theProjectRootPath buildfileCache filePath = do
        case Map.lookup filePath buildfileCache of
          Just buildfile -> return buildfileCache
          Nothing -> do
            maybeBuildfile <- loadBuildfile (theProjectRootPath IO.</> filePath)
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
        -> IO.FilePath
        -> Map.Map IO.FilePath Buildfile
        -> IO (IO.FilePath,
               Map.Map IO.FilePath Buildfile,
               Maybe (IO.FilePath, IO.FilePath))
      loadAllBuildfilesUpward
          theProjectRootPath defaultBuildfilePath buildfileCache = do
        buildfileCache <-
          ensureBuildfileCached theProjectRootPath buildfileCache
            buildfileFileName
        case Map.lookup buildfileFileName buildfileCache of
          Just (ProjectBuildfile _) ->
            return (theProjectRootPath,
                    buildfileCache,
                    Just (buildfileFileName, defaultBuildfilePath))
          Just (SubprojectBuildfile subprojectSpecification) -> do
            loadAllBuildfilesUpward
              (IO.dropTrailingPathSeparator $ IO.dropFileName
                 theProjectRootPath)
              ((IO.addTrailingPathSeparator $ IO.takeFileName
                 theProjectRootPath)
               IO.</> defaultBuildfilePath)
              (prefixCachePaths
                (IO.addTrailingPathSeparator $ IO.takeFileName
                   theProjectRootPath)
                buildfileCache)
          Nothing -> return (theProjectRootPath, buildfileCache, Nothing)
      loadAllBuildfilesDownward
        :: IO.FilePath
        -> Map.Map IO.FilePath Buildfile
        -> IO.FilePath
        -> Bool
        -> IO (Map.Map IO.FilePath Buildfile, Bool)
      loadAllBuildfilesDownward
          theProjectRootPath buildfileCache filePath atTopLevel = do
        buildfileCache <-
          ensureBuildfileCached theProjectRootPath buildfileCache filePath
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
                          theProjectRootPath
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
      translateProjectSpecification
        :: Map.Map Text.Text AnyFile
        -> Map.Map IO.FilePath Buildfile
        -> IO.FilePath
        -> IO.FilePath
        -> Maybe (Project, Defaults)
      translateProjectSpecification
          availableFiles buildfileCache theProjectRootPath defaultFilePath =
        case (Map.lookup buildfileFileName buildfileCache,
              Map.lookup defaultFilePath buildfileCache) of
          (Just (ProjectBuildfile project), Just defaults) -> Just
            (Project (Text.pack theProjectRootPath)
                     (project ^. name)
                     (foldl' Map.union
                             (Map.fromList $ catMaybes $ map
                                (\target ->
                                   fmap (\result -> (target ^. name, result))
                                        (translateTargetSpecification
                                          availableFiles
                                          ""
                                          target))
                                (project ^. projectSpecificationTargets))
                             (map (\subdirectoryName ->
                                     translateSubprojectSpecification
                                       availableFiles
                                       buildfileCache
                                       (subdirectoryName
                                        IO.</> buildfileFileName))
                                  (map Text.unpack $ Set.toList $ project ^.
                                     projectSpecificationSubprojects))),
             case defaults of
               ProjectBuildfile defaultsProject ->
                 Defaults
                   (defaultsProject ^. projectSpecificationDefaultTarget)
               SubprojectBuildfile defaultsSubproject ->
                 Defaults
                   (defaultsSubproject ^.
                      subprojectSpecificationDefaultTarget))
          _ -> Nothing
      translateSubprojectSpecification
        :: Map.Map Text.Text AnyFile
        -> Map.Map IO.FilePath Buildfile
        -> IO.FilePath
        -> Map.Map Text.Text AnyTarget
      translateSubprojectSpecification
          availableFiles buildfileCache filePath =
        case Map.lookup filePath buildfileCache of
          Just (SubprojectBuildfile subproject) ->
            (foldl' Map.union
                    (Map.fromList $ catMaybes $ map
                       (\target ->
                          fmap (\result -> (target ^. name, result))
                               (translateTargetSpecification
                                 availableFiles
                                 (IO.dropFileName filePath)
                                 target))
                       (subproject ^. subprojectSpecificationTargets))
                    (map (\subdirectoryName ->
                            translateSubprojectSpecification
                              availableFiles
                              buildfileCache
                              (IO.dropFileName filePath
                               IO.</> subdirectoryName
                               IO.</> buildfileFileName))
                         (map Text.unpack $ Set.toList $
                            subproject ^. subprojectSpecificationSubprojects)))
          _ -> Map.empty
      translateTargetSpecification
        :: Map.Map Text.Text AnyFile
        -> IO.FilePath
        -> AnyTargetSpecification
        -> Maybe AnyTarget
      translateTargetSpecification availableFiles basePath target =
        case fromAnyTargetSpecification target of
          Just actualTarget ->
            fmap AnyTarget $ translateExecutableSpecification
              availableFiles basePath actualTarget
          Nothing -> case fromAnyTargetSpecification target of
                 Just actualTarget ->
                   fmap AnyTarget $ translateLibrarySpecification
                     availableFiles basePath actualTarget
                 Nothing -> Nothing
      translateExecutableSpecification
        :: Map.Map Text.Text AnyFile
        -> IO.FilePath
        -> ExecutableSpecification
        -> Maybe ExecutableTarget
      translateExecutableSpecification availableFiles basePath target =
        let privateHeaders =
              translateFiles (translateHeaderFile availableFiles basePath)
                             (target ^. executableSpecificationPrivateHeaders)
            sources =
              translateFiles (translateSourceFile availableFiles basePath)
                             (target ^. executableSpecificationSources)
            allFiles = foldl' Map.union Map.empty
              [availableFiles,
               Map.map AnyFile privateHeaders,
               Map.map AnyFile sources]
        in Just $ ExecutableTarget
             (target ^. name)
             (target ^. executableSpecificationPrerequisites)
             (Set.fromList $ Map.elems privateHeaders)
             (Set.fromList $ Map.elems sources)
             (catMaybes $ map (translateInvocation allFiles) $ target ^.
                executableSpecificationExtraInvocations)
      translateLibrarySpecification
        :: Map.Map Text.Text AnyFile
        -> IO.FilePath
        -> LibrarySpecification
        -> Maybe LibraryTarget
      translateLibrarySpecification availableFiles basePath target =
        let publicHeaders =
              translateFiles (translateHeaderFile availableFiles basePath)
                             (target ^. librarySpecificationPublicHeaders)
            privateHeaders =
              translateFiles (translateHeaderFile availableFiles basePath)
                             (target ^. librarySpecificationPrivateHeaders)
            sources =
              translateFiles (translateSourceFile availableFiles basePath)
                             (target ^. librarySpecificationSources)
            allFiles = foldl' Map.union Map.empty
              [availableFiles,
               Map.map AnyFile publicHeaders,
               Map.map AnyFile privateHeaders,
               Map.map AnyFile sources]
        in Just $ LibraryTarget
             (target ^. name)
             (target ^. librarySpecificationPrerequisites)
             (Set.fromList $ Map.elems publicHeaders)
             (Set.fromList $ Map.elems privateHeaders)
             (Set.fromList $ Map.elems sources)
             (catMaybes $ map (translateInvocation allFiles) $ target ^.
                librarySpecificationExtraInvocations)
      translateFiles
        :: (File file)
        => (Text.Text -> Maybe file)
        -> Set.Set Text.Text
        -> Map.Map Text.Text file
      translateFiles translateFile files =
        Map.fromList $ catMaybes $ map
          (\filePath ->
             let maybeFile = translateFile filePath
             in fmap (\file ->
                        (Text.pack $ IO.takeFileName $ Text.unpack $
                           file ^. path,
                         file))
                     maybeFile)
          (Set.toList files)
      translateHeaderFile
        :: Map.Map Text.Text AnyFile
        -> IO.FilePath
        -> Text.Text
        -> Maybe HeaderFile
      translateHeaderFile availableFiles basePath fileName =
        listToMaybe $ catMaybes $ map fromAnyFile $ filter
          (\anyFile -> case fromAnyFile anyFile of
                         Just file ->
                           (Text.pack $ IO.takeFileName $ Text.unpack $
                             (file :: HeaderFile) ^. path)
                           == fileName
                         Nothing -> False)
          (Map.elems availableFiles)
      translateSourceFile
        :: Map.Map Text.Text AnyFile
        -> IO.FilePath
        -> Text.Text
        -> Maybe SourceFile
      translateSourceFile availableFiles basePath fileName =
        listToMaybe $ catMaybes $ map fromAnyFile $ filter
          (\anyFile -> case fromAnyFile anyFile of
                         Just file ->
                           (Text.pack $ IO.takeFileName $ Text.unpack $
                             (file :: SourceFile) ^. path)
                           == fileName
                         Nothing -> False)
          (Map.elems availableFiles)
      translateInvocation
        :: Map.Map Text.Text AnyFile
        -> InvocationSpecification
        -> Maybe InvocationBuildStep
      translateInvocation allFiles invocation = do
        InvocationBuildStep
          <$> (Map.lookup (invocation ^. invocationSpecificationExecutable)
                          allFiles
               >>= fromAnyFile)
          <*> Just (invocation ^. invocationSpecificationParameters)
          <*> (mapM (\input -> Map.lookup input allFiles)
                    (invocation ^. invocationSpecificationInputs)
               >>= return . Set.fromList)
          <*> (mapM (\output -> Map.lookup output allFiles)
                    (invocation ^. invocationSpecificationOutputs)
               >>= return . Set.fromList)
  theProjectRootPath <- IO.getCurrentDirectory
  (theProjectRootPath, buildfileCache, maybeBuildfilePaths) <-
    loadAllBuildfilesUpward theProjectRootPath buildfileFileName Map.empty
  case maybeBuildfilePaths of
    Just (rootBuildfilePath, defaultBuildfilePath) -> do
      (buildfileCache, result) <-
        loadAllBuildfilesDownward
          theProjectRootPath buildfileCache rootBuildfilePath True
      if result
        then do
          putStrLn $ concat
            ["The project root directory is \"",
             IO.addTrailingPathSeparator theProjectRootPath,
             "\"."]
          let availableFiles =
                foldl' Map.union Map.empty $ map
                  (\(basePath, buildfile) ->
                     let availableTargets =
                           case buildfile of
                             ProjectBuildfile availableProject ->
                               availableProject ^. projectSpecificationTargets
                             SubprojectBuildfile availableSubproject ->
                               availableSubproject ^.
                                 subprojectSpecificationTargets
                         availableFilesFromTarget
                           :: AnyTargetSpecification
                           -> Map.Map Text.Text AnyFile
                         availableFilesFromTarget target =
                           case fromAnyTargetSpecification target of
                             Just executable ->
                               availableFilesFromExecutableSpecification
                                 executable
                             Nothing -> Map.empty
                         availableFilesFromExecutableSpecification
                           :: ExecutableSpecification
                           -> Map.Map Text.Text AnyFile
                         availableFilesFromExecutableSpecification executable =
                           let theName = executable ^.
                                 executableSpecificationName
                           in Map.fromList
                               [(theName,
                                 AnyFile $ ExecutableFile
                                   (Text.pack $
                                      IO.addTrailingPathSeparator basePath
                                      IO.</> Text.unpack theName)
                                   BuiltProvenance)]
                     in foldl' Map.union Map.empty
                          (map availableFilesFromTarget availableTargets))
                  (Map.toList buildfileCache)
          return $ translateProjectSpecification availableFiles
                                                 buildfileCache
                                                 theProjectRootPath
                                                 defaultBuildfilePath
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


outputtingBuildSteps :: Project -> [AnyBuildStep] -> [AnyBuildStep]
outputtingBuildSteps project steps =
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
      directoriesOutsideProject =
        Set.fromList $ unfoldr
          (\path ->
             let pathComponents =
                   tail $ IO.splitPath $ IO.dropTrailingPathSeparator path
             in if (length pathComponents) < 1
                  then Nothing
                  else Just (Text.pack $ IO.dropTrailingPathSeparator path,
                             IO.joinPath ("/" : init pathComponents)))
          (Text.unpack $ project ^. projectRootPath)
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
               ([], directoriesOutsideProject)
               steps
  in result
