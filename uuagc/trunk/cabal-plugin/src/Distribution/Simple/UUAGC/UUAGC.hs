module Distribution.Simple.UUAGC.UUAGC(uuagcUserHook,
                                       uuagc
                                      ) where

import Distribution.Simple
import Distribution.Simple.PreProcess
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Utils
import Distribution.Simple.Setup
import Distribution.PackageDescription
import Distribution.Simple.UUAGC.AbsSyn( AGFileOption(..)
                                         , AGFileOptions
                                         , UUAGCOption(..)
                                         , UUAGCOptions
                                         , defaultUUAGCOptions
                                         , fromUUAGCOtoArgs
                                         , fromUUAGCOstoArgs
                                         , lookupFileOptions
                                         )
import Distribution.Simple.UUAGC.Parser
import System.Process( CreateProcess(..), createProcess, CmdSpec(..)
                     , StdStream(..), runProcess, waitForProcess
                     , proc
                     )
import System.Directory
import System.FilePath(pathSeparators,normalise)
import System.Exit (ExitCode(..))
import System.IO( openFile, IOMode(..), hFileSize
                , hSetFileSize, hClose, hGetContents
                , Handle(..), stderr, hPutStr
                )
import Control.Exception (throwIO)

-- | 'uuagc' returns the name of the uuagc compiler
uuagcn = "uuagc"

-- | 'defUUAGCOptions' returns the default names of the uuagc options
defUUAGCOptions = "uuagc_options"

uuagcUserHook :: UserHooks
uuagcUserHook = simpleUserHooks { hookedPreProcessors = ("ag", uuagc):knownSuffixHandlers
                                , preBuild = uuagcPreBuild
                                }

originalPreBuild = preBuild simpleUserHooks

processContent :: Handle -> IO [String]
processContent h = do s <- hGetContents h
                      return $ words s

putErrorInfo :: Handle -> IO ()
putErrorInfo h = do s <- hGetContents h
                    hPutStr stderr s

addSearch :: [String] -> [String] -> [String]
addSearch spl fl = let sf = [head pathSeparators]
                   in [ normalise (sp ++ sf ++ f)  | sp <- spl, f  <- fl]

throwFailure :: IO ()
throwFailure = do throwIO $ ExitFailure 1
                  return ()

-- This manages to change the file modification time.
updateFile :: FilePath -> IO ()
updateFile f = do h <- openFile f AppendMode
                  i <- hFileSize h
                  hSetFileSize h (i+1)
                  hSetFileSize h i
                  hClose h

-- | 'updateAGFile' search into the uuagc options file for a list of all
-- AG Files and theirs file dependencies in order to see if the latters
-- are more updated that the formers, and if this is the case to
-- update the AG File
updateAGFile :: FilePath -> [String] -> IO ()
updateAGFile f sp = do
  (_,(Just ppOutput), (Just ppError),ph) <- createProcess
                                            $ (proc uuagcn ["--genfiledeps"
                                                           ,"--="++(intercalate ":" sp)
                                                           ,f
                                                           ])
                                                  { std_in  = Inherit
                                                  , std_out = CreatePipe
                                                  , std_err = CreatePipe
                                                  }
  ec <- waitForProcess ph
  case ec of
    ExitSuccess       -> do fls <- processContent ppOutput
                            let flsC = addSearch sp fls
                            fmt   <- getModificationTime f
                            flsmt <- mapM getModificationTime flsC
                            if any (fmt < ) flsmt
                             then updateFile f
                             else return ()
    (ExitFailure exc) -> do putErrorInfo ppOutput
                            putErrorInfo ppError
                            throwFailure

uuagcPreBuild :: Args -> BuildFlags -> IO HookedBuildInfo
uuagcPreBuild args buildF = do
  uuagcOpts <- parserAG defUUAGCOptions
  let agfls  = getAGFileList uuagcOpts
      agflSP = map (\f -> (f,[searchPath f])) agfls
  mapM_ (uncurry updateAGFile) agflSP
  originalPreBuild args buildF

getAGFileList :: AGFileOptions -> [FilePath]
getAGFileList = map (\(AGFileOption s _) -> (normalise s))


searchPath :: FilePath -> FilePath
searchPath fp = let pf = reverse fp
                    sp = head pathSeparators
                    rl = searchPath' sp  pf
                in (reverse rl)
    where searchPath' y []     = []
          searchPath' y sp@(x:xs)
              | x == y         = sp
              | otherwise      = searchPath' y xs

uuagc :: BuildInfo
        -> LocalBuildInfo
        -> PreProcessor
uuagc build local  =
   PreProcessor {
     platformIndependent = True,
     runPreProcessor = mkSimplePreProcessor $ \inFile outFile verbosity ->
                       do info verbosity (inFile++" has been preprocessed into "++outFile)
                          print $ "processing: " ++ inFile
                          opts <- parserAG defUUAGCOptions
                          let search  = searchPath inFile
                              options = (fromUUAGCOstoArgs (lookupFileOptions inFile opts))
                                        ++ ["-P"++search,"--output="++outFile,inFile]
                          (_,_,_,ph) <- createProcess (proc uuagcn options)
                          eCode <- waitForProcess ph
                          case eCode of
                            ExitSuccess   -> return ()
                            ExitFailure _ -> throwFailure
                }

