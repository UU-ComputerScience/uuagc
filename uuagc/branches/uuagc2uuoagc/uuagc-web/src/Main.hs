{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
module Main where

import System.Directory
import System.Exit
import System.Environment
import System.Console.GetOpt
import System.IO
import Data.ByteString.UTF8(fromString)
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as U
import Happstack.Server
import Control.Monad.Trans
import UU.UUAGC
import qualified Text.Blaze as H
import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as H hiding (title)
import Control.Exception
import Data.IORef
import qualified Data.Sequence as Seq
import Data.Foldable(toList)
import Control.Monad
import Data.Typeable


main :: IO ()
main = do
  opts <- getOpts
  tmp <- getTemporaryDirectory
  let config = nullConf { port = optPortNr opts }
      policy = defaultBodyPolicy tmp (200 * 1024) 4096 4096
  simpleHTTP config $ do
    decodeBody policy
    msum [ dir "compile" uuagcCompile
         , uuagcForm
         ]

data Opts = Opts { optPortNr :: !Int }

defaultOpts :: Opts
defaultOpts = Opts { optPortNr = port nullConf }

optsSpec :: [OptDescr (Opts -> Opts)]
optsSpec =
  [ Option "p" ["port"] (ReqArg oPort "port nr") "specifies the port nr"
  ]

oPort :: String -> Opts -> Opts
oPort n o = o { optPortNr = read n }

getOpts :: IO Opts
getOpts = do
  args <- getArgs
  let usage = usageInfo "Usage: uuagc-web <OPTION ...>" optsSpec
  case getOpt Permute optsSpec args of
    (actions, _, []) -> return $ foldr ($) defaultOpts actions
    (_, _, errs)     -> do
      hPutStrLn stderr (unlines errs ++ "\n" ++ usage)
      exitFailure

uuagcForm :: ServerPart Response
uuagcForm = ok $ toResponse $ H.html $ do
  H.head $ H.title "uuagc web compilation"
  H.body $ do
    H.p $ "This is the web interface to UUAGC."
    H.form H.! H.enctype "multipart/form-data" H.! H.method "POST" H.! H.action "/compile" $ do
      H.table $ H.tbody $ do
        H.tr $ do
          H.td "options"
          H.td $ H.input H.! H.type_ "text" H.! H.name "opts" H.! H.size "80"
        H.tr $ do
          H.td "source"
          H.td $ H.input H.! H.type_ "file" H.! H.name "src" H.! H.size "40"
        H.tr $ do
          H.td ""
          H.td $ H.input H.! H.type_ "submit" H.! H.value "compile"

uuagcCompile :: ServerPart Response
uuagcCompile = do
  optStr <- look "opts"
  (tmpFile, uploadName, _) <- lookFile "src"

  let (opts, _, errs) = getOptions $ words optStr
  if null errs
   then do
     let opts' = opts { mainFilename = Just uploadName }
     outcome <- liftIO $ doCompile opts' tmpFile
     case outcome of
       Left errs' -> doneErrs errs'
       Right str  -> done False str
   else doneErrs errs

doneErrs :: [String] -> ServerPart Response
doneErrs errs = done True str where
  str  = B.intercalate (fromString "\r\n") strs
  strs = map fromString errs

done :: Bool -> B.ByteString -> ServerPart Response
done hasErrs str = ok $ augment $ toResponse str where
  augment | hasErrs   = setHeader "warning" "uuagc error"
          | otherwise = id

data AgException = AgException !Int
  deriving (Show,Typeable)
instance Exception AgException

doCompile :: Options -> FilePath -> IO (Either [String] B.ByteString)
doCompile opts inputfile = do
  msgsRef <- newIORef Seq.empty
  tmp <- getTemporaryDirectory
  bracket (openTempFile tmp "uuag")
    (\(outputfile,_) -> removeFile outputfile) $
    \(outputfile,h) -> do
      hClose h
      let opts' = setSafeOpts opts
            { outputStr = \msg -> modifyIORef msgsRef (Seq.|> msg)
            , failWithCode = throwIO . AgException
            }
      res <- try $ compile opts' inputfile outputfile
      case res of
        Left (AgException _) -> do
          msgs <- readIORef msgsRef
          return $ Left $ toList msgs
        Right () -> do
          str <- B.readFile outputfile
          return $ Right str

setSafeOpts :: Options -> Options
setSafeOpts opts = opts
  { werrors = True           -- warnings will otherwise not be printed
  , wignore = False
  , wmaxerrs = 15
  , kennedyWarren = True     -- other backends not allowed
  , sepSemMods = False       -- we cannot output those files
  , allowSepSemMods = False  -- prevent turning it on
  , noIncludes = True        -- require complete files as input
  , beQuiet = True
  }
