module UUAGCTests(uuagcPreTest
                 ,uuagcTestHook
                 ,uuagcPostTest
                 ) where

import Distribution.Simple(UserHooks(..), Args(..))
import Distribution.PackageDescription(PackageDescription(..)
                                      ,HookedBuildInfo(..)
                                      ,emptyHookedBuildInfo
                                      ,Executable(..))
import Distribution.Simple.LocalBuildInfo(LocalBuildInfo(..))
import Distribution.Simple.Setup(TestFlags(..))
import System.FilePath((</>))
import System.Process 
import System.Exit

data Test = ExpectOK   FilePath String String 
          | ExpectFail FilePath String String

exec :: FilePath -> String -> FilePath -> IO ExitCode
exec uuagc opts file = do
  hp <- runProcess uuagc (words opts ++ [file]) Nothing Nothing Nothing Nothing Nothing
  waitForProcess hp

execFail :: FilePath -> String -> FilePath -> String -> IO ()
execFail uuagc file opts msg = do
  code <- exec uuagc opts file
  case code of
    ExitSuccess       -> putStrLn $ "File: " ++ file ++ " fail"  ++ msg 
    (ExitFailure val) -> putStrLn $ "File: " ++ file ++ " pass "


execOK :: FilePath -> String -> FilePath -> String -> IO ()
execOK uuagc opts file msg = do
  code <- exec uuagc opts file
  case code of
    ExitSuccess       -> putStrLn $ "File: " ++ file ++ " OK"
    (ExitFailure val) -> putStrLn $ "File: " ++ file ++ " fail. " ++ msg ++ " Because: " ++ (show val)

execTest :: FilePath -> [Test] -> IO ()
execTest uuagc tests = do mapM_ f tests
    where f (ExpectOK fp   opts msg)  = execOK   uuagc opts fp msg
          f (ExpectFail fp opts msg)  = execFail uuagc opts fp msg

uuagcPreTest :: Args -> TestFlags -> IO HookedBuildInfo
uuagcPreTest args tflags = do return emptyHookedBuildInfo


test = [ExpectOK "test/TestOkFP.ag"        "-a" "Not pass"
       ,ExpectOK "test/TestOkFirstLast.ag" "-a" "Not pass"]


uuagcTestHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> TestFlags -> IO ()
uuagcTestHook pd lbi uh tflags = do
    let uuagcExeName = exeName.head.executables $ pd
        bDir         = buildDir lbi
        uuagcFPExec  = bDir </> uuagcExeName </> uuagcExeName -- Dir base of an executable starts with the same name
    execTest uuagcFPExec test 


uuagcPostTest :: Args -> TestFlags -> PackageDescription -> LocalBuildInfo -> IO ()
uuagcPostTest args bool pd lbi = return ()

