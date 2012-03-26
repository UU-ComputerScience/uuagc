{-# LANGUAGE CPP #-}
module Distribution.Simple.UUAGC.UUAGC(uuagcUserHook,
                                       uuagcUserHook',
                                       uuagc,
                                       uuagcLibUserHook
                                      ) where

import Distribution.Simple.BuildPaths (autogenModulesDir)
import Debug.Trace
import Distribution.Simple
import Distribution.Simple.PreProcess
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Utils
import Distribution.Simple.Setup
import Distribution.PackageDescription hiding (Flag)
import Distribution.Simple.UUAGC.AbsSyn( AGFileOption(..)
                                         , AGFileOptions
                                         , AGOptionsClass(..)
                                         , lookupFileOptions
                                         , fileClasses
                                         )
import Distribution.Simple.UUAGC.Parser
import Options hiding (verbose)
import Distribution.Verbosity
import System.Process( CreateProcess(..), createProcess, CmdSpec(..)
                     , StdStream(..), runProcess, waitForProcess
                     , proc)
import System.Directory(getModificationTime
                       ,doesFileExist
                       ,removeFile)
import System.FilePath(pathSeparators,
                       (</>),
                       takeFileName,
                       normalise,
                       joinPath,
                       dropFileName,
                       addExtension,
                       dropExtension,
                       splitDirectories)

import System.Exit (ExitCode(..))
import System.IO( openFile, IOMode(..),
                  hFileSize,
                  hSetFileSize,
                  hClose,
                  hGetContents,
                  hFlush,
                  Handle(..), stderr, hPutStr, hPutStrLn)
import System.Exit(exitFailure)
import Control.Exception (throwIO)
import Control.Monad (liftM, when, guard, forM_, forM)
import Control.Arrow ((&&&), second)
import Data.Maybe (maybeToList)
import Data.Either (partitionEithers)
import Data.List (nub)

{-# DEPRECATED uuagcUserHook, uuagcUserHook', uuagc "Use uuagcLibUserHook instead" #-}

-- | 'uuagc' returns the name of the uuagc compiler
uuagcn = "uuagc"

-- | 'defUUAGCOptions' returns the default names of the uuagc options
defUUAGCOptions :: String
defUUAGCOptions = "uuagc_options"

-- | File used to store de classes defined in the cabal file.
agClassesFile :: String
agClassesFile = "ag_file_options"

-- | The prefix used for the cabal file optionsw
agModule :: String
agModule = "x-agmodule"

-- | The prefix used for the cabal file options used for defining classes
agClass :: String
agClass  = "x-agclass"

-- | Deprecated userhook
uuagcUserHook :: UserHooks
uuagcUserHook = uuagcUserHook' uuagcn

-- | Deprecated userhook
uuagcUserHook' :: String -> UserHooks
uuagcUserHook' uuagcPath = uuagcLibUserHook (uuagcFromString uuagcPath)

-- | Create uuagc function using shell (old method)
uuagcFromString :: String -> [String] -> FilePath -> IO (ExitCode, [FilePath])
uuagcFromString uuagcPath args file = do  
  (_, Just ppOutput, Just ppError, ph) <- createProcess $ (proc uuagcPath (args ++ [file]))
                                    { std_in  = Inherit
                                    , std_out = CreatePipe
                                    , std_err = CreatePipe
                                    }
  ec <- waitForProcess ph
  case ec of
    ExitSuccess ->
      do fls <- processContent ppOutput
         return (ExitSuccess, fls)
    (ExitFailure exc) ->
      do hPutStrLn stderr (show exc)
         putErrorInfo ppOutput
         putErrorInfo ppError
         return (ExitFailure exc, [])

-- | Main hook, argument should be uuagc function
uuagcLibUserHook :: ([String] -> FilePath -> IO (ExitCode, [FilePath])) -> UserHooks
uuagcLibUserHook uuagc = hooks where
  hooks = simpleUserHooks { hookedPreProcessors = ("ag", ag):("lag",ag):knownSuffixHandlers
                          , buildHook = uuagcBuildHook uuagc
                          , sDistHook = uuagcSDistHook uuagc
                          }
  ag = uuagc' uuagc

originalPreBuild  = preBuild simpleUserHooks
originalBuildHook = buildHook simpleUserHooks
originalSDistHook = sDistHook simpleUserHooks

processContent :: Handle -> IO [String]
processContent = liftM words . hGetContents

putErrorInfo :: Handle -> IO ()
putErrorInfo h = hGetContents h >>= hPutStr stderr

addSearch :: String -> [String] -> [String]
addSearch sp fl = let sf = [head pathSeparators]
                      path = if sp == ""
                             then '.' : sf
                             else sp ++ sf
                  in [normalise (joinPath [sp,f]) | f  <- fl]

throwFailure :: IO ()
throwFailure = throwIO $ ExitFailure 1

-- The tmp build directory really depends on the type of project.
-- In the case executables it uses the name of the generated file for
-- the output directory.
withBuildTmpDir
  :: PackageDescription
     -> LocalBuildInfo
     -> (FilePath -> IO ())
     -> IO ()
withBuildTmpDir pkgDescr lbi f = do
#if MIN_VERSION_Cabal(1,8,0)
            withLib pkgDescr $ \ _ -> f $ buildDir lbi
#else
            withLib pkgDescr () $ \ _ -> f $ buildDir lbi
#endif
            withExe pkgDescr $ \ theExe ->
                    f $ buildDir lbi </> exeName theExe </> exeName theExe ++ "-tmp"

-- | 'updateAGFile' search into the uuagc options file for a list of all
-- AG Files and theirs file dependencies in order to see if the latters
-- are more updated that the formers, and if this is the case to
-- update the AG File
updateAGFile :: ([String] -> FilePath -> IO (ExitCode, [FilePath]))
             -> FilePath
             -> PackageDescription
             -> LocalBuildInfo
             -> (FilePath, String)
             -> IO ()
updateAGFile uuagc classesPath pkgDescr lbi (f, sp) = do
  fileOpts <- readFileOptions classesPath
  let opts = case lookup f fileOpts of
               Nothing -> noOptions
               Just x -> x
  (ec, fls) <- uuagc (optionsToString $ opts { genFileDeps = True, searchPath = sp : (searchPath opts) }) f
  case ec of
    ExitSuccess ->
      do let flsC = addSearch sp fls
         when ((not.null) flsC) $ do
            flsmt <- mapM getModificationTime flsC
            let maxModified = maximum flsmt
                removeTmpFile f buildTmp =
                  do
                    -- For src/a/b/c.ag and build, this creates ["build/src/a/b/c.hs","build/a/b/c.hs","build/b/c.hs","build/c.hs"]
                    -- Problem is we don't know what prefix of the filename is src directory and what part is in the classname
                    -- There must be a better solution for this...
                    let files = map (buildTmp </>) . scanr1 (</>) . splitDirectories . (`addExtension` "hs") . dropExtension $ f
                    forM_ files $ \f -> do
                      exists <- doesFileExist f
                      when exists $ do fmt <- getModificationTime f
                                       when (maxModified > fmt) $ removeFile f
            withBuildTmpDir pkgDescr lbi $ removeTmpFile f
    (ExitFailure exc) ->
      do hPutStrLn stderr (show exc)
         throwFailure

getAGFileOptions :: [(String, String)] -> IO AGFileOptions
getAGFileOptions extra = do
  usesOptionsFile <- doesFileExist defUUAGCOptions
  if usesOptionsFile
       then do r <- parserAG' defUUAGCOptions
               case r of
                 Left e -> print e >> exitFailure
                 Right a -> return a
       else mapM (parseOptionAG . snd)
            $ filter ((== agModule) . fst) extra

getAGClasses :: [(String, String)] -> IO [AGOptionsClass]
getAGClasses = mapM (parseClassAG . snd) . filter ((== agClass) . fst)

writeFileOptions :: FilePath -> [(String, Options)] -> IO ()
writeFileOptions classesPath opts  = do
  hClasses <- openFile classesPath WriteMode
  hPutStr hClasses $ show [(s,optionsToString opt) | (s,opt) <- opts]
  hFlush  hClasses
  hClose  hClasses

readFileOptions :: FilePath -> IO [(String, Options)]
readFileOptions classesPath = do
  hClasses <- openFile classesPath ReadMode
  sClasses <- hGetContents hClasses
  classes <- readIO sClasses :: IO [(String, [String])]
  hClose hClasses
  return $ [(s,opt) | (s,str) <- classes, let (opt,_,_) = getOptions str]

getOptionsFromClass :: [(String, Options)] -> AGFileOption -> ([String], Options)
getOptionsFromClass classes fOpt =
    second (foldl combineOptions (opts fOpt))
    . partitionEithers $ do
                       fClass <- fileClasses fOpt
                       case fClass `lookup` classes of
                         Just x  -> return $ Right x
                         Nothing -> return $ Left $ "Warning: The class "
                                                   ++ show fClass
                                                   ++ " is not defined."

uuagcSDistHook :: ([String] -> FilePath -> IO (ExitCode, [FilePath]))
     -> PackageDescription
     -> Maybe LocalBuildInfo
     -> UserHooks
     -> SDistFlags
     -> IO ()
uuagcSDistHook uuagc pd mbLbi uh df = do
  {-
  case mbLbi of
    Nothing -> warn normal "sdist: the local buildinfo was not present. Skipping AG initialization. Dist may fail."
    Just lbi -> let classesPath = buildDir lbi </> agClassesFile
                in commonHook uuagc classesPath pd lbi (sDistVerbosity df)
  originalSDistHook pd mbLbi uh df
  -}
  originalSDistHook pd mbLbi (uh { hookedPreProcessors = ("ag", nouuagc):("lag",nouuagc):knownSuffixHandlers }) df  -- bypass preprocessors

uuagcBuildHook
  :: ([String] -> FilePath -> IO (ExitCode, [FilePath]))
     -> PackageDescription
     -> LocalBuildInfo
     -> UserHooks
     -> BuildFlags
     -> IO ()
uuagcBuildHook uuagc pd lbi uh bf = do
  let classesPath = buildDir lbi </> agClassesFile
  commonHook uuagc classesPath pd lbi (buildVerbosity bf)
  originalBuildHook pd lbi uh bf

commonHook :: ([String] -> FilePath -> IO (ExitCode, [FilePath]))
     -> FilePath
     -> PackageDescription
     -> LocalBuildInfo
     -> Flag Verbosity
     -> IO ()
commonHook uuagc classesPath pd lbi fl = do
  let verbosity = fromFlagOrDefault normal fl
  when (verbosity >= verbose) $ putStrLn ("commonHook: Assuming AG classesPath: " ++ classesPath)
  createDirectoryIfMissingVerbose verbosity True (buildDir lbi)
  let lib    = library pd
      exes   = executables pd
      bis    = map libBuildInfo (maybeToList lib) ++ map buildInfo exes
  classes <- map (className &&& opts') `fmap` (getAGClasses . customFieldsPD $ pd)
  options <- getAGFileOptions (bis >>= customFieldsBI)
  fileOptions <- forM options (\ opt ->
      let (notFound, opts) = getOptionsFromClass classes $ opt
      in do when (verbosity >= verbose) $ putStrLn ("options for " ++ filename opt ++ ": " ++ unwords (optionsToString opts))
            forM_ notFound (hPutStrLn stderr) >> return (normalise . filename $ opt, opts))
  writeFileOptions classesPath fileOptions
  let agflSP = map (id &&& dropFileName) $ nub $ getAGFileList options
  mapM_ (updateAGFile uuagc classesPath pd lbi) agflSP

getAGFileList :: AGFileOptions -> [FilePath]
getAGFileList = map (normalise . filename)

uuagc :: BuildInfo -> LocalBuildInfo -> PreProcessor
uuagc = uuagc' (uuagcFromString uuagcn)

uuagc' :: ([String] -> FilePath -> IO (ExitCode, [FilePath]))
        -> BuildInfo
        -> LocalBuildInfo
        -> PreProcessor
uuagc' uuagc build lbi  =
   PreProcessor {
     platformIndependent = True,
     runPreProcessor = mkSimplePreProcessor $ \ inFile outFile verbosity ->
                       do info verbosity $ concat [inFile, " has been preprocessed into ", outFile]
                          print $ "processing: " ++ inFile ++ " generating: " ++ outFile
--                          opts <- getAGFileOptions $ customFieldsBI build
                          let classesPath = buildDir lbi </> agClassesFile
                          when (verbosity >= verbose) $ putStrLn ("uuagc-preprocessor: Assuming AG classesPath: " ++ classesPath)
                          fileOpts <- readFileOptions classesPath
                          let opts = case lookup inFile fileOpts of
                                       Nothing -> noOptions
                                       Just x -> x
                              search  = dropFileName inFile
                              options = opts { searchPath = search : (searchPath opts) 
                                             , outputFiles = outFile : (outputFiles opts) }
                          (eCode,_) <- uuagc (optionsToString options) inFile
                          case eCode of
                            ExitSuccess   -> return ()
                            ExitFailure _ -> throwFailure
                }

nouuagc :: BuildInfo -> LocalBuildInfo -> PreProcessor
nouuagc build lbi =
  PreProcessor {
    platformIndependent = True,
    runPreProcessor = mkSimplePreProcessor $ \inFile outFile verbosity -> do
      info verbosity ("skipping: " ++ outFile)
  }
