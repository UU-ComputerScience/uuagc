
module LOAG.Optimise where

import LOAG.Common
import LOAG.Graphs
import LOAG.Solver.MiniSat

import              Control.Arrow ((&&&))
import              Control.Monad (forM, forM_, when, foldM)
import              Control.Monad.ST
import              Data.Array.MArray
import              Data.Array.IO
import              Data.Function (on)
import qualified    Data.IntMap as IM
import qualified    Data.IntSet as IS
import qualified    Data.Map    as M
import              Data.Maybe  (fromJust, isJust)
import              Data.List   (intercalate, sort, sortBy)

type Opts = [Opt]
data Opt= Smaller    (Vertex) (Vertex) -- x < y 
        | Greater    (Vertex) (Vertex) -- x > y
        | AllSmaller (Vertex) Direction          -- _ < x
        | AllGreater (Vertex) Direction          -- _ > x
        | MinVisits
  deriving (Ord, Eq)

instance Show Opt where
    show (Smaller f t)      = show f ++ " --> " ++ show t
    show (Greater f t)      = show f ++ " <-- " ++ show t
    show (AllSmaller f _)   = " _ --> " ++ show f
    show (AllGreater f _)   = " _ <-- " ++ show f
    show MinVisits          = " Minimising #visits "


optimise :: Sat -> VarMap -> Opts -> (Int,Int) -> [Nt] -> InterfaceRes -> IO ()
optimise sat varMap opts nbounds nts interfaces = do 
    let scheduler = newSchedule sat varMap nbounds
    mapM_ (singleOpt sat varMap scheduler nts interfaces) (sort opts)
    b <- satsolve sat []
    return ()

-- | Given a non-terminal and a current best schedule
--     return a new best schedule and whether the new schedule is truely new
type SchedulerNt = Nt -> IM.IntMap [Vertex] -> IO (Bool, IM.IntMap [Vertex])
type Scheduler   = IM.IntMap [Vertex] -> IO (Bool, IM.IntMap [Vertex])

singleOpt :: Sat -> VarMap -> SchedulerNt -> [Nt] -> InterfaceRes -> Opt -> 
                IO ()
singleOpt sat varMap scheduler nts interfaces opt = do
    success <- 
      case opt of 
        Smaller f t     -> tryPair sat $ varOf f t 
        Greater f t     -> tryPair sat $ varOf t f
        AllSmaller f d  -> trySingle sat False f d varMap
        AllGreater f d  -> trySingle sat True f d varMap
        MinVisits -> do mapM_ (minPaths sat varMap scheduler interfaces) $ 
                          sortNfilter weigh nts
                        return True
    return ()
 where weigh (Nt _ _ _ is ss _) = size
        where size = length is + length ss
       varOf f t = case M.lookup (f,t) varMap of
                    Just v  -> v
                    Nothing -> case M.lookup (t,f) varMap of
                                Just v  -> varnot v
                                Nothing -> error "invalid optimisation.."

sortNfilter :: (a -> Int) -> [a] -> [a]
sortNfilter f = 
    map fst . sortBy (on compare snd) . 
                  filter (((/=) 0) . snd) . map (id &&& f)

minPaths :: Sat -> VarMap -> SchedulerNt -> InterfaceRes -> Nt -> IO Bool 
minPaths sat varMap scheduler interfaces tp@(Nt nt _ _ is ss _) = do
    forM_ neckCs $ attemptGroup sat (scheduler tp) mym . map varnot
    return True 
 where  neckCs= map (\(_,es,b) -> map (uncurry (mvar b)) es) $ 
                    bottlenecks mx mym
        mvar b f t | b       = varMap M.! (f,t)
                   | not b   = varnot $ varMap M.! (t,f)
        mym   = interfaces M.! nt
        mx    | IM.null mym = 0
              | otherwise   = fst $ IM.findMax mym

bottlenecks :: Int -> IM.IntMap [Vertex] -> 
                [(((Int,[Vertex]),(Int,[Vertex])),[Edge],Bool)]
bottlenecks x shd = 
    sortNfilter cost $ pairs x
 where  pairs x | x <= 1 = []
                | x >  1 = let pair = ((x,maybe [] id $ IM.lookup x shd)
                                      ,(x-1,maybe [] id$IM.lookup (x-1) shd))
                           in (pair,edges pair,even x) : pairs (x-1)
        cost (((p1,p1s),(p2,p2s)),es,_) = size 
         where size = length es 
        edges ((p1,p1s),(p2,p2s)) = [ (f,t) | f <- p1s, t <- p2s ] 

-- | get all combinations of partitions from different `directions'
--      ordered by distance (shortest paths first)
edgeCombs :: Int -> IM.IntMap [Vertex] -> [(Int,Int)]
edgeCombs x shd = 
    sortBy (on compare cost) $
        concat $
        takeWhile (not . null) $
        map (\(n,es) -> filter ((>0) . snd) 
            (map (\(f,t) -> (f+n,t+n)) es)) $
        zip [-0,-1..] $
        repeat (map ((,) x) [x-1,x-3..1])
 where cost (x1,x2) = length $ findK (x1 -1)
        where findK n | n < x2  = []
                      | n >= x2 = (shd IM.! n) ++ findK (n-2)

tryPair :: Sat -> MyVar -> IO Bool 
tryPair sat p = do
    b  <- satsolve sat [p]
    if b then addClause sat [p]
         else return False

attemptGroup :: Sat -> Scheduler -> IM.IntMap [Vertex] -> [MyVar] -> 
                        IO (IM.IntMap [Vertex])
attemptGroup sat scheduler interfaces = 
    tryGroup sat scheduler interfaces . filter notSet 
 where  notSet (Var _)      = True
        notSet (VarNot v)   = notSet v
        notSet _            = False

tryGroup :: Sat -> Scheduler -> IM.IntMap [Vertex] -> [MyVar] -> 
                IO (IM.IntMap [Vertex])
tryGroup _ _ interfaces []           = return interfaces
tryGroup sat scheduler interfaces ps = do
    xs  <- mapM (const (newLit sat)) ps
    mapM (\(a,r) -> addClause sat [varnot a, r]) $ zip xs ps
    b  <- satsolve sat xs
    let success = assertVars sat xs
        fail    = assertVars sat (map varnot (xs++ps)) >> satsolve sat []
    if b then do    (improved, sched) <- scheduler interfaces
                    if improved 
                        then success >> return sched
                        else fail    >> return interfaces 
         else fail >> return interfaces

assertVars :: Sat -> [MyVar] -> IO ()
assertVars sat vars = do    bs <- mapM (addClause sat . (:[])) vars
                            return ()
 
trySingle :: Sat -> Bool -> Vertex -> Direction -> VarMap -> IO Bool
trySingle sat des f dir varMap = do
        vars <- tryExtreme sat des literals
        assertVars sat vars
        return True
 where  literals = M.foldrWithKey select [] varMap
        select k a b | fst k == f = a : b --inh
                     | snd k == f = varnot a : b --syn
                     | otherwise  = b

tryExtreme :: Sat -> Bool -> [MyVar] -> IO [MyVar]
tryExtreme sat des xs =
  do a <- newLit sat
     switch <- newLit sat
     let try xs =
           do --putStrLn ("currently, " ++ show (length xs) ++ " literals")
              let assertOne | des       = map varnot (a : xs)
                            | otherwise = varnot a : xs
              addClause sat assertOne --"if a, then one of xs must be ~des"
              b <- satsolve sat [a]
              if b then
                do xbs <- sequence [ do v <- value sat x
                                        return (x,v)
                                   | x <- xs
                                   ]
                   sequence_ [ do let desx | des        = x
                                           | otherwise  = varnot x
                                  addClause sat [varnot a, desx] 
                                  addClause sat [varnot switch, desx]
                             | (x,b) <- xbs, b /= Just des ]
                   try [ x | (x,Just des) <- xbs ]
               else do  addClause sat [varnot a]
                        return [switch]
      in try xs

-- | Recalculate interface based on SAT and compare with a given one
newSchedule :: Sat -> VarMap -> (Int,Int) -> Nt -> IM.IntMap [Vertex] ->
                IO (Bool, IM.IntMap [Vertex])
newSchedule sat varMap nbounds tp@(Nt nt _ _ inhs outs _ ) sched = do
    idsf <- newArray nbounds IS.empty :: IO (IOArray Vertex Vertices)
    idst <- newArray nbounds IS.empty :: IO (IOArray Vertex Vertices)
    let ids = (idsf,idst)
    sequence_ [ do  v <- value sat pred
                    case v of
                      Nothing -> error "no val"
                      Just True -> addEdges (i,s) ids 
                      Just False-> addEdges (s,i) ids
            | (i,ios,_) <- inhs
            , (s,sos,_) <- outs
            , let pred = varMap M.! (i,s)
            ]
    f_idsf <- freeze idsf
    f_idst <- freeze idst
    let (_,newinterface) = runST $ do   schedA <- newArray nbounds Nothing
                                        completingN (f_idsf,f_idst) schedA tp
        newmx | IM.null newinterface = 0
              | otherwise            = fst $ IM.findMax newinterface
        oldmx | IM.null sched        = 0
              | otherwise            = fst $ IM.findMax sched 
        newsched | newmx < oldmx = newinterface
                 | otherwise     = sched
    return $ (newmx < oldmx, newsched)
 where  addEdges
          :: (Vertex, Vertex)
          -> (IOArray Vertex Vertices, IOArray Vertex Vertices)
          -> IO ()
        addEdges (f,t) (idsf,idst) = do
            modifyArray idsf f (t `IS.insert`)
            modifyArray idst t (f `IS.insert`)
 
-- | count the (max, avg, total) number of visits
getVisCount :: [Nt] -> InterfaceRes -> VisCount
getVisCount nts interfaces = 
    (mx, tot, (fromIntegral tot) / (fromIntegral $ length nts))
 where  count (mx,tot) (Nt nt _ _ _ _ _) = (max mx k,tot + k)
         where m = interfaces M.! nt
               k | IM.null m = 0
                 | otherwise = ((fst $ IM.findMax m) + 1) `div` 2
        (mx, tot) = foldl count (0,0) nts
--- minimisation functions
-------------------------------------------------------------------------------

globalMinimum :: Sat -> Bool -> [MyVar] -> IO [MyVar]
globalMinimum sat des xs =
  do ys <- sort sat xs
     let mini (i,j) | i >= j = return []
         mini (i,j) = do    putStrLn ("trying " ++ show (i,j))
                            b <- satsolve sat [varnot (ys !! k)]
                            if b then mini (k+1,j)
                                 else mini (i,k)
          where
           k = (i+j) `div` 2
      in mini (0,length ys)
     xbs <- sequence [ do v <- value sat x
                          return (x,v)
                     | x <- xs
                     ]
     return [ x | (x,Just True) <- xbs ]
 where
  sort sat []  = do return []
  sort sat [x] = do return [x]
  sort sat xs  = do as <- sort sat (take k xs)
                    bs <- sort sat (drop k xs)
                    map fromJust `fmap` merge (map Just as) (map Just bs)
   where
    k = length xs `div` 2

  merge2 Nothing b = return (b, Nothing)
  merge2 a Nothing = return (a, Nothing)
  merge2 (Just x) (Just y) =
    do a <- newLit sat
       b <- newLit sat
       addClause sat [varnot x, b]         -- x => b
       addClause sat [varnot y, b]         -- y => b
       addClause sat [varnot x, varnot y, a]  -- x => ~y || a
       addClause sat [x, varnot a]         -- ~x => ~a
       addClause sat [y, varnot a]         -- ~y => ~a
       addClause sat [x, y, varnot b]      -- ~x => y || ~b
       return (Just a,Just b)
  
  merge []  bs  = return bs
  merge as  []  = return as
  merge [a] [b] = (\(a,b) -> [a,b]) `fmap` merge2 a b
  merge as  bs  = take (a+b) `fmap` merge' (as ++ xas) (bs ++ xbs)
   where
    a   = length as
    b   = length bs
    m   = a `max` b
    n   = if even m then m else m+1
    xas = replicate (n-a) Nothing
    xbs = replicate (n-b) Nothing
  
  -- pre: as and bs have the same, even length
  merge' as bs =
    do xs <- merge eas ebs
       ys <- merge oas obs
       let x:xys = weave xs ys
       xys' <- sequence [ merge2 a b | (a,b) <- pairs xys ]
       return (x : unpairs xys' ++ [last xys])
   where
    (eas,oas) = evenOdds as
    (ebs,obs) = evenOdds bs

  evenOdds []       = ([], [])
  evenOdds [x]      = ([x], [])
  evenOdds (x:y:xs) = (x:es,y:os)
   where
    (es,os) = evenOdds xs

  pairs (x:y:xs) = (x,y) : pairs xs
  pairs _        = []
  
  unpairs ((x,y):xys) = x : y : unpairs xys
  unpairs []          = []
  
  weave (x:xs) (y:ys) = x : y : weave xs ys
  weave xs     ys     = xs ++ ys

------------------------------------------------------------------------------
