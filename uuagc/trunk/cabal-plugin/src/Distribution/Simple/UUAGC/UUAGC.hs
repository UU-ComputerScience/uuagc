{-# LANGUAGE CPP #-}
module Distribution.Simple.UUAGC.UUAGC(uuagcUserHook,
                                       uuagcUserHook',
                                       uuagc,
                                       uuagcLibUserHook,
                                       uuagcFromString
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
                     , shell)
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
                       replaceExtension,
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
import Data.List (nub,intersperse)
import Data.Map (Map)
import qualified Data.Map as Map

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
  let argline = uuagcPath ++ concatMap (' ':) (args ++ [file])
  (_, Just ppOutput, Just ppError, ph) <- createProcess (shell argline)
                                    { std_in  = Inherit
                                    , std_out = CreatePipe
                                    , std_err = CreatePipe
                                    }
  ec <- waitForProcess ph
  case ec of
    ExitSuccess ->
      do putErrorInfo ppError
         fls <- processContent ppOutput
         return (ExitSuccess, fls)
    (ExitFailure exc) ->
      do hPutStrLn stderr (uuagcPath ++ ": " ++ show exc)
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

-- | 'updateAGFile' search into the uuagc options file for a list of all
-- AG Files and theirs file dependencies in order to see if the latters
-- are more updated that the formers, and if this is the case to
-- update the AG File
updateAGFile :: ([String] -> FilePath -> IO (ExitCode, [FilePath]))
             -> Map FilePath (Options, Maybe (FilePath, [String]))
             -> (FilePath, (Options, Maybe (FilePath, [String])))
             -> IO ()
updateAGFile _ _ (_,(_,Nothing)) = return ()
updateAGFile uuagc newOptions (file,(opts,Just (gen,sp))) = do
  (ec, fls) <- uuagc (optionsToString $ opts { genFileDeps = True, searchPath = sp }) file
  case ec of
    ExitSuccess ->
      do files <- mapM (resolveFile opts sp) fls
         when ((not.null) files) $ do
            flsmt <- mapM getModificationTime files
            let maxModified = maximum flsmt
            fmt <- getModificationTime gen
            let newOpts :: Options 
                newOpts = maybe noOptions fst $ Map.lookup file newOptions
            -- When some dependency is newer or options have changed, we should regenerate
            when (maxModified > fmt || optionsToString newOpts /= optionsToString opts) $ removeFile gen
    ex@(ExitFailure _) -> throwIO ex

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

writeFileOptions :: FilePath -> Map FilePath (Options, Maybe (FilePath,[String])) -> IO ()
writeFileOptions classesPath opts  = do
  hClasses <- openFile classesPath WriteMode
  hPutStr hClasses $ show $ Map.map (\(opt,gen) -> (optionsToString opt, gen)) opts
  hFlush  hClasses
  hClose  hClasses

readFileOptions :: FilePath -> IO (Map FilePath (Options, Maybe (FilePath,[String])))
readFileOptions classesPath = do
  isFile <- doesFileExist classesPath
  if isFile
    then do hClasses <- openFile classesPath ReadMode
            sClasses <- hGetContents hClasses
            classes <- readIO sClasses :: IO (Map FilePath ([String], Maybe (FilePath,[String])))
            hClose hClasses
            return $ Map.map (\(opt,gen) -> let (opt',_,_) = getOptions opt in (opt', gen)) classes
    else    return Map.empty

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
  info verbosity $ "commonHook: Assuming AG classesPath: " ++ classesPath
  createDirectoryIfMissingVerbose verbosity True (buildDir lbi)
  -- Read already existing options
  -- Map FilePath (Options, Maybe (FilePath,[String]))
  oldOptions <- readFileOptions classesPath
  -- Read options from cabal and settings file
  let lib    = library pd
      exes   = executables pd
      bis    = map libBuildInfo (maybeToList lib) ++ map buildInfo exes
  classes <- map (className &&& opts') `fmap` (getAGClasses . customFieldsPD $ pd)
  configOptions <- getAGFileOptions (bis >>= customFieldsBI)
  -- Construct new options map
  newOptionsL <- forM configOptions (\ opt ->
      let (notFound, opts) = getOptionsFromClass classes $ opt
          file = normalise $ filename opt
          gen = maybe Nothing snd $ Map.lookup file oldOptions
      in do info verbosity $ "options for " ++ file ++ ": " ++ unwords (optionsToString opts)
            forM_ notFound (hPutStrLn stderr)
            return (file, (opts, gen)))
  let newOptions = Map.fromList newOptionsL
  writeFileOptions classesPath newOptions
  -- Check if files should be regenerated
  mapM_ (updateAGFile uuagc newOptions) $ Map.toList oldOptions

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
                       do putStrLn $ "[UUAGC] processing: " ++ inFile ++ " generating: " ++ outFile
                          let classesPath = buildDir lbi </> agClassesFile
                          info verbosity $ "uuagc-preprocessor: Assuming AG classesPath: " ++ classesPath
                          fileOpts <- readFileOptions classesPath
                          let opts = case Map.lookup inFile fileOpts of
                                       Nothing        -> noOptions
                                       Just (opt,gen) -> opt
                              search  = dropFileName inFile
                              options = opts { searchPath = search : hsSourceDirs build ++ searchPath opts
                                             , outputFiles = outFile : (outputFiles opts) }
                          (eCode,_) <- uuagc (optionsToString options) inFile
                          case eCode of
                            ExitSuccess   -> writeFileOptions classesPath (Map.insert inFile (opts, Just (outFile, searchPath options)) fileOpts)
                            ex@(ExitFailure _) -> throwIO ex
                }

nouuagc :: BuildInfo -> LocalBuildInfo -> PreProcessor
nouuagc build lbi =
  PreProcessor {
    platformIndependent = True,
    runPreProcessor = mkSimplePreProcessor $ \inFile outFile verbosity -> do
      info verbosity ("skipping: " ++ outFile)
  }

-- From UUAGC/src/Parser.hs, should be shared between UUAGC and cabal-plugin
resolveFile :: Options -> [FilePath] -> FilePath -> IO FilePath
resolveFile opts path fname = search (path ++ ["."])
 where search (p:ps) = do let filename = joinPath [p, fname]
                          fExists <- doesFileExist filename
                          if fExists
                            then return filename
                            else do let filename' = joinPath [p, replaceExtension fname "ag"]
                                    fExists' <- doesFileExist filename'
                                    if fExists'
                                      then return filename'
                                      else search ps
       search []     = do
         outputStr opts ("File: " ++ show fname ++ " not found in search path: " ++ show (concat (intersperse ";" (path ++ ["."]))) ++ "\n")
         failWithCode opts 1
         return (error "resolveFile: file not found")
