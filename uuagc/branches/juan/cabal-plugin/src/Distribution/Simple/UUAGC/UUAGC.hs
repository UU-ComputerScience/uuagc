{-# LANGUAGE CPP #-}

module Distribution.Simple.UUAGC.UUAGC(uuagcUserHook,
                                       uuagc
                                      ) where
import Distribution.Simple.BuildPaths (autogenModulesDir)
import Debug.Trace
import Distribution.Simple
import Distribution.Simple.PreProcess
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Utils
import Distribution.Simple.Setup
import Distribution.PackageDescription
import Distribution.Verbosity(silent)
import Distribution.Simple.UUAGC.AbsSyn( AGFileOption(..)
                                       , AGFileOptions
                                       , AGOptionsClass(..)
                                       , UUAGCOption(..)
                                       , UUAGCOptions
                                       , defaultUUAGCOptions
                                       , fromUUAGCOtoArgs
                                       , fromUUAGCOstoArgs
                                       , lookupFileOptions
                                       , fileClasses
                                       )
import Distribution.Simple.UUAGC.Parser
import System.Process( CreateProcess(..)
                     , createProcess, CmdSpec(..)
                     , StdStream(..)
                     , runProcess
                     , waitForProcess
                     , proc)

import System.Directory
import System.FilePath(pathSeparators,
                       (</>),
                       takeFileName,
                       normalise,
                       dropFileName,
                       addExtension,
                       dropExtension)

import System.Exit (ExitCode(..))
import System.IO( openFile
                , IOMode(..)
                , hFileSize
                , hSetFileSize
                , hClose
                , hGetContents
                , hFlush
                , Handle(..)
                , stderr
                , hPutStr
                , hPutStrLn)

import Control.Exception (throwIO)
import Control.Monad (liftM, when, guard, forM_, forM)
import Control.Arrow ((&&&), second)
import Data.Maybe (maybeToList)
import Data.Either (partitionEithers)
import Data.List (nub)

-- | 'uuagc' returns the name of the uuagc compiler
uuagcn = "uuagc"

-- | 'defUUAGCOptions' returns the default names of the uuagc options
defUUAGCOptions = "uuagc_options"

-- | File used to store de classes defined in the cabal file.
agClassesFile = ".ag_file_options"

-- | The prefix used for the cabal file optionsw
agModule = "x-agmodule"

-- | The prefix used for the cabal file options used for defining classes
agClass  = "x-agclass"

uuagcUserHook :: UserHooks
uuagcUserHook = simpleUserHooks {hookedPreProcessors = ("ag", uuagc):knownSuffixHandlers,
                                 buildHook           = uuagcBuildHook,
                                 postBuild           = uuagcPostBuild,
                                 sDistHook           = uuagcSDistHook,
                                 postSDist           = uuagcPostSDist
                                }

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
                  in [normalise (sp ++ f) | f  <- fl]

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

-- Creates the output file given the main preprocessed file and the buildtmp folder
tmpFile :: FilePath -> FilePath -> FilePath
tmpFile buildTmp = (buildTmp </>)
                   . (`addExtension` "hs")
                   . dropExtension
                   . takeFileName

-- | 'updateAGFile' search into the uuagc options file for a list of all
-- AG Files and theirs file dependencies in order to see if the latters
-- are more updated that the formers, and if this is the case to
-- update the AG File
updateAGFile :: UUAGCOptions -> FilePath -> [String] -> IO ()
updateAGFile opts f sp = do
  let modeOpts = filter isModeOption opts
      isModeOption UHaskellSyntax = True
      isModeOption ULCKeyWords    = True
      isModeOption UDoubleColons  = True
      isModeOption _              = False
  
      args = fromUUAGCOstoArgs modeOpts ++
             [ "--genfiledeps"
             , "--="++(intercalate ":" sp)
             , f
             ]
  -- putStrLn ("Generating deps: " ++ unwords args)
  (_,(Just ppOutput), (Just ppError),ph) <- createProcess
                                            $ (proc uuagcn args)
                                                  { std_in  = Inherit
                                                  , std_out = CreatePipe
                                                  , std_err = CreatePipe
                                                  }
  ec <- waitForProcess ph
  case ec of
    ExitSuccess ->
      do fls <- processContent ppOutput
         let flsC = addSearch sp fls
         flsmt <- mapM getModificationTime flsC
         let maxModified = maximum flsmt
             removeTmpFile f = do
                 exists <- doesFileExist f
                 when exists $ do
                     fmt <- getModificationTime f
                     when ((not.null) flsmt && 
                           maxModified > fmt) $ removeFile f
         withBuildTmpDir pkgDescr lbi $ removeTmpFile . (`tmpFile` f)
    (ExitFailure exc) ->
      do putErrorInfo ppOutput
         putErrorInfo ppError
         throwFailure
  where newProcess mopts = createProcess $ (proc uuagcn (fromUUAGCOstoArgs mopts ++ ["--genfiledeps"
                                                                                    ,"--=" ++ intercalate ":" [sp]
                                                                                    ,f
                                                                                    ]
                                                        )
                                           )
                                    { std_in  = Inherit
                                    , std_out = CreatePipe
                                    , std_err = CreatePipe
                                    }

uuagcPreBuild :: Args -> BuildFlags -> IO HookedBuildInfo
uuagcPreBuild args buildF = do
  uuagcOpts <- parserAG defUUAGCOptions
  let agfls  = getAGFileList uuagcOpts
      agflSP = map (\(f,opts) -> (f,opts,[searchPath f])) agfls
  mapM_ (\(f,opts,s) -> updateAGFile opts f s) agflSP
  originalPreBuild args buildF

getAGFileList :: AGFileOptions -> [(FilePath, UUAGCOptions)]
getAGFileList = map (\(AGFileOption s opts) -> (normalise s, opts))

writeFileOptions :: [(String, [UUAGCOption])] -> IO ()
writeFileOptions opts  = do
  hClasses <- openFile agClassesFile WriteMode
  hPutStr hClasses $ show opts
  hFlush  hClasses
  hClose  hClasses

readFileOptions :: IO [(String, [UUAGCOption])]
readFileOptions = do
  hClasses <- openFile agClassesFile ReadMode
  sClasses <- hGetContents hClasses
  classes <- readIO sClasses :: IO [(String, [UUAGCOption])]
  hClose hClasses
  return $ classes

getOptionsFromClass :: [(String, [UUAGCOption])] -> AGFileOption -> ([String], [UUAGCOption])
getOptionsFromClass classes fOpt =
    second (nub . concat . ((opts fOpt):))
    . partitionEithers $ do
                       fClass <- fileClasses fOpt
                       case fClass `lookup` classes of
                         Just x  -> return $ Right x
                         Nothing -> return $ Left $ "Warning: The class "
                                                   ++ show fClass
                                                   ++ " is not defined."

uuagcBuildHook
  :: PackageDescription
     -> LocalBuildInfo
     -> UserHooks
     -> BuildFlags
     -> IO ()
uuagcBuildHook pd lbi uh bf = do
  let lib    = library pd
      exes   = executables pd
      bis    = map libBuildInfo (maybeToList lib) ++ map buildInfo exes
  classes <- map (className &&& opts') `fmap` (getAGClasses . customFieldsPD $ pd)
  options <- getAGFileOptions (bis >>= customFieldsBI)
  fileOptions <- forM options (\ opt ->
      let (notFound, opts) = getOptionsFromClass classes $ opt
      in forM_ notFound (hPutStrLn stderr) >> return (normalise . filename $ opt, opts))
  writeFileOptions fileOptions
  let agflSP = map (id &&& dropFileName) $ nub $ getAGFileList options
  mapM_ (updateAGFile pd lbi) agflSP
  originalBuildHook pd lbi uh bf

uuagcPostBuild _ _ _ _ = do
               exists <- doesFileExist agClassesFile
               when exists $ removeFile agClassesFile

getAGFileList :: AGFileOptions -> [FilePath]
getAGFileList = map (normalise . filename)

uuagc :: BuildInfo
        -> LocalBuildInfo
        -> PreProcessor
uuagc build local  =
   PreProcessor 
   {
     platformIndependent = True,
     runPreProcessor = mkSimplePreProcessor 
                       $ \ inFile outFile verbosity ->
                       do 
                         print "Starting uuagc"
                         print "version: 0.9.18.2"
                         info verbosity $ concat [inFile, " has been preprocessed into ", outFile]
                         print $ "processing: " ++ inFile
                       --                          opts <- getAGFileOptions $ customFieldsBI build
                         fileOpts <- readFileOptions
                         let opts = case lookup inFile fileOpts of
                                      Nothing -> []
                                      Just x -> x
                             -- search  = dropFileName inFile
                             search = map ("-P"++) $ hsSourceDirs build
                             options = fromUUAGCOstoArgs opts
                                       ++ search ++ ["--output="++outFile, inFile] -- ["-P" ++ search, "--output=" ++ outFile, inFile]
                         (_,_,_,ph) <- createProcess (proc uuagcn options)
                         eCode <- waitForProcess ph
                         case eCode of
                           ExitSuccess   -> return ()
                           ExitFailure _ -> throwFailure
   }


uuagcSDistHook :: PackageDescription -> Maybe LocalBuildInfo 
               -> UserHooks -> SDistFlags -> IO ()
uuagcSDistHook pd (Just lbi) uh sdf = do
  let bf = fromSDFToBF sdf 
  print "SdistHook"
  uuagcBuildHook pd lbi uh bf
  originalSDistHook pd (Just lbi) uh sdf 
uuagcSDistHook pd Nothing    uh sdf = do 
  hPutStrLn stderr "Error"
  return ()

uuagcPostSDist ::  Args -> SDistFlags -> PackageDescription 
               -> Maybe LocalBuildInfo -> IO ()
uuagcPostSDist _ _ _ _ = do
               exists <- doesFileExist agClassesFile
               when exists $ removeFile agClassesFile

fromSDFToBF :: SDistFlags -> BuildFlags
fromSDFToBF _  = BuildFlags { 
                   buildProgramPaths = []
                 , buildProgramArgs  = []
                 , buildDistPref     = toFlag ""
                 , buildVerbosity    = toFlag silent
                 }