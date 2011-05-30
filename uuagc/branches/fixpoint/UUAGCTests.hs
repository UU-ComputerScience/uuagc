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

data Test = ExpectOK   FilePath String 
          | ExpectFail FilePath String

exec :: FilePath -> FilePath -> IO ExitCode
exec uuagc file = do
  hp <- runProcess uuagc [file] Nothing Nothing Nothing Nothing Nothing
  waitForProcess hp

execFail :: FilePath -> FilePath -> String -> IO ()
execFail uuagc file msg = do
  code <- exec uuagc file
  case code of
    ExitSuccess       -> putStrLn $ "File: " ++ file ++ " fail"  ++ msg 
    (ExitFailure val) -> putStrLn $ "File: " ++ file ++ " pass "


execOK :: FilePath -> FilePath -> String -> IO ()
execOK uuagc file msg = do
  code <- exec uuagc file
  case code of
    ExitSuccess       -> putStrLn $ "File: " ++ file ++ " OK"
    (ExitFailure val) -> putStrLn $ "File: " ++ file ++ " fail. " ++ msg ++ " Because: " ++ (show val)

execTest :: FilePath -> [Test] -> IO ()
execTest uuagc tests = do mapM_ f tests
    where f (ExpectOK fp msg)   = execOK uuagc fp msg
          f (ExpectFail fp msg) = execFail uuagc fp msg

uuagcPreTest :: Args -> TestFlags -> IO HookedBuildInfo
uuagcPreTest args tflags = do return emptyHookedBuildInfo


test = [ExpectOK "test/TestOkFP.ag" "Not pass"
       ,ExpectOK "test/TestOkFirstLast.ag" "Not pass"]


uuagcTestHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> TestFlags -> IO ()
uuagcTestHook pd lbi uh tflags = do
    let uuagcExeName = exeName.head.executables $ pd
        bDir         = buildDir lbi
        uuagcFPExec  = bDir </> uuagcExeName </> uuagcExeName -- Dir base of an executable starts with the same name
    execTest uuagcFPExec test 


uuagcPostTest :: Args -> TestFlags -> PackageDescription -> LocalBuildInfo -> IO ()
uuagcPostTest args bool pd lbi = return ()

