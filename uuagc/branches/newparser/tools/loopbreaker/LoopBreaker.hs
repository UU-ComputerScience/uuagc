module Main where

--
-- AG Loop Breaker
--

import System.Process
import System.Exit
import System.Cmd
import Control.Concurrent
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
import Data.IntSet(IntSet)
import qualified Data.IntSet as IntSet
import System.Random
import Control.Monad.Trans
import Control.Monad.Writer.Strict
import Control.Monad.State.Strict
import Data.List
import Data.Array.Unboxed


main :: IO ()
main = checkModels defaultOptions

defaultOptions :: Options
defaultOptions
  = Options { attrFileInitial      = "initial.attrs"
            , attrFileIntermediate = "intermediate.attrs"
            , builderScript        = "./builder.sh"
            , runnerScript         = "./tester.sh"
            , maxWaitTime          = 3
            , logFile              = "execution.log"
            , resultFile           = "results/result"
            , maxFailures          = 10
            }


--
-- AG Model checking on lazy attribute definitions
--

data Options = Options { attrFileInitial      :: String
                       , attrFileIntermediate :: String
                       , builderScript        :: String
                       , runnerScript         :: String
                       , maxWaitTime          :: Int
                       , logFile              :: String
                       , resultFile           :: String
                       , maxFailures          :: Int
                       }

checkModels :: Options -> IO ()
checkModels options
  = do initial <- readAttrConfiguration (attrFileInitial options)
       gen <- getStdGen
       cfg <- runSearch (agTest options) gen (maxFailures options) initial
       writeAttrConfiguration (resultFile options ++ ".attrs") cfg

modelLength :: AttrConfiguration -> Int
modelLength cfg = length [ a | (a,ChangeableOn) <- cfg ]

agTest :: Options -> Test Attr IO
agTest options cfg
  = do let modelSize = modelLength cfg
       writeAttrConfiguration (attrFileIntermediate options) cfg
       system (builderScript options)
       finishedOk <- runProcessLimited (maxWaitTime options) (runnerScript options)
       case finishedOk of
         True  -> do putStrLn ("Run finished OK (improving to model of size: " ++ show modelSize ++ ")")
                     appendFile (logFile options) ("Succeeding model of size " ++ show modelSize ++ ":\n")
                     appendAttrConfiguration (logFile options) cfg
                     appendFile (logFile options) ("\n\n")
                     return Succeeded
         False -> do putStrLn ("Run failed: backtracking (model of size: " ++ show modelSize ++ ")")
                     return Failed


--
-- Attribute configuration
--

type Attr = (String, String, String, String)
type AttrConfiguration = Config Attr

readAttrConfiguration :: String -> IO AttrConfiguration
readAttrConfiguration file
  = do s <- readFile file
       seq (length s) (return ())
       let lists :: [(String,[(String,[(String, String)])])]
           lists = read s
           
           attrs :: [Attr]
           attrs = concat [ concat [ [ (n,c,fld,attr) | (fld,attr) <- ss ] | (c,ss) <- cs ] | (n,cs) <- lists ]

       return (zip attrs (repeat ChangeableOn))

appendAttrConfiguration :: String -> AttrConfiguration -> IO ()
appendAttrConfiguration filename
  = writeAttrCfg (appendFile filename)

writeAttrConfiguration :: String -> AttrConfiguration -> IO ()
writeAttrConfiguration filename
  = writeAttrCfg (writeFile filename)
  
writeAttrCfg :: (String -> IO ()) -> AttrConfiguration -> IO ()
writeAttrCfg writer cfg
  = let attrs = [ a | (a,ChangeableOn) <- cfg ]
        mps = map (\(n,c,f,a) -> Map.singleton n $ Map.singleton c $ Set.singleton (f,a)) attrs
        mp = foldr (Map.unionWith $ Map.unionWith $ Set.union) Map.empty mps
        s = show $ Map.toList $ Map.map (Map.toList . Map.map Set.toList) $ mp
    in writer s


--
-- Run a process with a maximum runtime in seconds
--

runProcessLimited :: Int -> String -> IO Bool
runProcessLimited maxTime cmdline
  = do handle <- runCommand cmdline
       waitMax maxTime handle
  where
    waitMax 0 h
      = do terminateProcess h
           return False
    waitMax n h
      = do mbCode <- getProcessExitCode h
           case mbCode of
             Just code -> case code of
                            ExitSuccess   -> return True
                            ExitFailure _ -> return False
             Nothing   -> do threadDelay 1000000
                             waitMax (n-1) h


--
-- Random search for a configuration that fails
--

type Config t = [(t, Fixity)]

data Fixity
  = ChangeableOn
  | FixedOff
  deriving (Eq, Ord, Show)

cfgSize :: Config t -> Int
cfgSize cfg
  = length [ () | (_, ChangeableOn) <- cfg ]


data Result
  = Succeeded
  | Failed
  deriving (Eq, Ord, Show)

type Test t m = Config t -> m Result

data TestState t
  = TestState { configuration :: Config t  -- invariant: always succeeding configuration
              , generator     :: [Int]
              , nfailures     :: !Int
              }

runSearch :: Monad m => Test t m -> StdGen -> Int -> Config t -> m (Config t)
runSearch test gen maxFailures initCfg
  = do final <- execStateT it initial
       return (configuration final)
  where
    initial = TestState { configuration = initCfg
                        , generator     = randoms gen
                        , nfailures     = 0
                        }

    it = do cfg <- gets configuration
            g   <- gets generator
            n   <- gets nfailures
            let m = max 1 ((cfgSize initCfg - n) `div` 4)
                (cfg', g') = transform g m cfg
            res <- lift $ test cfg'
            case res of
              Succeeded -> do modify (\s -> s { configuration = cfg', generator = g', nfailures = 0 })
                              case cfgSize cfg' <= 0 of
                                True  -> return ()
                                False -> it
              Failed    -> do modify (\s -> s { generator = g', nfailures = n + 1 })
                              case n >= maxFailures of
                                True  -> return ()
                                False -> it

transform :: [Int] -> Int -> Config t -> (Config t, [Int])
transform gen n cfg
  = let rnds    = take n gen
        gen'    = drop n gen
        inds    = [ n | (n, (_, ChangeableOn)) <- zip [0..] cfg ]
        s       = length inds
        ainds :: UArray Int Int
        ainds   = listArray (0, s-1) inds
        winners = IntSet.fromList $ map (\i -> ainds ! (i `mod` s)) rnds
        cfg'    = zipWith (\i x@(e,_) -> if i `IntSet.member` winners then (e, FixedOff) else x) [0..] cfg
    in (cfg', gen')

--
-- Search for the smallest configuration that fails
--

data Range = Range { rngFrom :: Int -- includes rngFrom
                   , rngTo   :: Int -- excludes rngTo
                   } deriving (Eq, Ord, Show)

rngSize :: Range -> Int
rngSize rng
  = (rngTo rng) - (rngFrom rng)

subdivisions :: Range -> Int -> [Range]
subdivisions rng n
  = divisions (rngFrom rng) (rngTo rng)
  where
    size = max 1 (rngSize rng `div` n)
  
    divisions start end
      | start < end = Range start (min (start + size) end) : divisions (start+size) end
      | otherwise   = []

type Configuration a = [(a, Mode)]

data Mode
  = On
  | Off
  deriving (Eq, Ord, Show)

type Runner m a = Configuration a -> m Bool

runTester :: Monad m => Runner m a -> Configuration a -> m [Configuration a]
runTester runner initCfg
  = run initRange initCfg
  where
    initRange = Range { rngFrom = 0, rngTo = length initCfg }

    run rng cfg
      = let divs = map (subdivisions rng) [2 .. rngSize rng]
        in processDivs divs
      where
        processDivs []
          = return []
        processDivs (rngs : others)
          = do counterModels <- processRanges rngs
               if null counterModels
                then processDivs others
                else return counterModels

        processRanges []
          = return []
        processRanges (rng : others)
          = do models <- processRange rng
               modelsOther <- processRanges others
               return (models ++ modelsOther)
    
        processRange rng
          = do let cfg' = setModeRange On rng cfg
               res <- runner cfg'
               case res of
                 True  -> do cfgs <- run rng cfg'
                             return (cfg' : cfgs)
                 False -> return []


setModeRange :: Mode -> Range -> Configuration a -> Configuration a
setModeRange mode rng cfg
  = zipWith upd [0..] cfg
  where
    upd ind entry@(item,_)
      | left <= ind && ind < right = (item, mode)
      | otherwise                  = entry
    left  = rngFrom rng
    right = rngTo rng

