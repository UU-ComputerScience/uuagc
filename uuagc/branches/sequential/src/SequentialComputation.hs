module SequentialComputation (
    computeSequential, 
    Info(Info), 
    SequentialResult(SequentialResult,DirectCycle,InducedCycle), 
    tdpToTds, tdsToTdp, tdpNt, lmh, cyclesOnly,
    Interface
) where

import Debug.Trace

import SequentialTypes
import CommonTypes

import Data.Graph hiding (path)
import qualified Data.Graph (path)
import Control.Monad.ST
import Data.Array
import Data.Array.MArray
import Data.Array.ST
import Data.Maybe(fromJust)
import Control.Monad
import List(partition,sort,transpose,nub,(\\),intersect,minimumBy)

-- The updatable graph of attribute occurrences
type STGraph s = STArray s Vertex [Vertex]
type Tdp s = (STGraph s, STGraph s)
type Tds s = STGraph s 
type Comp s = (Tds s, Tdp s)

type Interfaces = [Interface]
type Interface = [([Vertex],[Vertex])]

type VisitSS = [Vertex]
type LMH = (Vertex,Vertex,Vertex)

-- Information about the vertices
data Info = Info { tdpToTds :: Table Vertex   -- Mapping attribute occurrences to attributes
                 , tdsToTdp :: Table [Vertex] -- Mapping attributes to all attribute occurrences
                 , tdpNt    :: Table (Name,Name,Name,Name) -- Mapping attribute occurrences to their rhs nt, lhs nt, constructor, and fieldnames
                 , lmh      :: [LMH] -- Division of tds. l<=inherited<m, m<=synthesized<=h
                 , cyclesOnly :: Bool -- Check for cycles only
                 }

-- Result of sequential computation
data SequentialResult = SequentialResult [[[VisitSS]]] [(Vertex,ChildVisit)] [Interface] -- Succeeded, with visit sub-sequences, a table of childvisits, and interfaces
                      | DirectCycle [(Edge,[Vertex])]  -- Failed because of a cycle in the direct dependencies (type-2)
                      | InducedCycle [(Edge,Interface)] -- No direct cycle, but the computed interface generated a cycle (type-3)


-- Compute the Tds and Tdp graphs, given Info and the direct dependencies between attribute occurrences
computeSequential :: Info -> [Edge] -> SequentialResult
computeSequential info dpr
  = runST (do (comp@(tds,tdp),s2i) <- tdsTdp (info{cyclesOnly = True}) dpr
              cycles2 <- cycles2 tds s2i
              if (not (null cycles2))
               then let ddp = directGraph info dpr
                        cyclePaths = map (fromJust . cyclePath info ddp) cycles2
                    in return (DirectCycle (zip cycles2 cyclePaths))
               else do mapM_ (insertTds (info{cyclesOnly = False}) comp) s2i
                       tds' <- freeze tds
                       tdsT <- thaw (transposeG tds') 
                       inters <- makeInterfaces tdsT (lmh info)
                       let idp = concatMap (concatMap (uncurry carthesian)) inters
                       mapM_ (insertTds info comp) idp
                       cycles3 <- cycles3 info tds inters
                       if (not (null cycles3))
                        then return (InducedCycle cycles3)
                        else do let dprgraph = buildG (Data.Array.bounds (tdpToTds info)) dpr
                                    newindex = snd (Data.Array.bounds (tdpToTds info)) + 1
                                    (vsgraph,table)  = visitssGraph info dprgraph newindex inters
                                    vs = visitss info table vsgraph inters
                                return (SequentialResult vs table inters)
          )

-- Initialise computation, and add all direct dependencies. This will trigger the whole of the computation
tdsTdp :: Info -> [Edge] -> ST s (Comp s,[Edge])
tdsTdp info dpr = do tds  <- newArray (Data.Array.bounds (tdsToTdp info)) []
                     tdpN <- newArray (Data.Array.bounds (tdpToTds info)) []
                     tdpT <- newArray (Data.Array.bounds (tdpToTds info)) []
                     let comp = (tds,(tdpN,tdpT))
                     es <- concatMapM (insertTdp info comp) dpr
                     return (comp,es)

-- Induces dependencies: Given a Tdp edge, add the corresponding Tds edge when applicable
-- Applicable for non-local attributes with equal field names
induce :: Info -> Comp s -> Edge -> ST s [Edge]
induce info comp (v1,v2)
  = let v1' = tdpToTds info ! v1
        v2' = tdpToTds info ! v2
        nonlocal = v1' /= -1 && v2' /= -1
        equalfield = tdpNt info ! v1 == tdpNt info ! v2
    in if nonlocal && equalfield
        then insertTds info comp (v1',v2')
        else return []

-- Add an egde to Tds. This induces dependencies on Tdp.
insertTds :: Info -> Comp s -> Edge -> ST s [Edge]
insertTds info (tds,tdp) (v1,v2)
  = if cyclesOnly info && v1 > v2
     then return [(v1,v2)]
     else do e1 <- readArray tds v1
             if v2 `elem` e1
              then return []
              else do writeArray tds v1 (v2:e1)
                      occur info (tds,tdp) (v1,v2)

-- Adds all induced dependencies of an Tds-edge to the corresponding Tdp-graphs
occur :: Info -> Comp s -> Edge -> ST s [Edge]
occur info comp (v1,v2)
  = let v1s = tdsToTdp info ! v1
        v2s = tdsToTdp info ! v2
    in liftM concat $ sequence [ insertTdp info comp (v1,v2) | v1 <- v1s, v2 <-  v2s, tdpNt info ! v1 == tdpNt info ! v2 ]

-- Add an edge to Tdp and transitively re-close it.
insertTdp :: Info -> Comp s -> Edge -> ST s [Edge]
insertTdp info comp@(_,(tdpN,tdpT)) (v1,v2)
  = do e1 <- readArray tdpN v1
       if v2 `elem` e1
        then return []
        else do inc <- readArray tdpT v1
                out <- readArray tdpN v2
                let edges = carthesian (v1:inc) (v2:out)
                concatMapM (addTdpEdge info comp) edges

-- Add an edge to Tdp. This induces dependencies on Tds
addTdpEdge :: Info -> Comp s -> Edge -> ST s [Edge]
addTdpEdge info comp@(_,(tdpN,tdpT)) (v1,v2)
  = do e <- readArray tdpN v1
       if v2 `elem` e
        then return []
        else do writeArray tdpN v1 (v2:e)
                e' <- readArray tdpT v2
                writeArray tdpT v2 (v1:e')
                induce info comp (v1,v2)

-------------------------------------------------------------------------------
-- Interfaces
-------------------------------------------------------------------------------
makeInterfaces :: Tds s -> [LMH] -> ST s Interfaces
makeInterfaces tds = mapM (makeInterface tds)

makeInterface :: Tds s -> LMH -> ST s Interface
makeInterface tds lmh
  = do (iwork,swork) <- sinks tds lmh []
       if null iwork && null swork 
         then return [([],[])]
         else makeInter tds lmh iwork swork

makeInter :: Tds s -> LMH -> [Vertex] -> [Vertex] -> ST s Interface
makeInter tds lmh []    [] = return []
makeInter tds lmh iwork swork
  = do (ipart,swork') <- predsI tds lmh iwork
       (spart,iwork') <- predsS tds lmh (swork++swork')
       rest <- makeInter tds lmh iwork' []
       return ((ipart,spart):rest)

-- Finds the sinks-components, given initial sinks
predsI,predsS :: Tds s -> LMH -> [Vertex] -> ST s ([Vertex],[Vertex])
-- The inherited sinks-component and the resulting synthesized sinks
predsI tds lmh []   = return ([],[])
predsI tds lmh work = do (inh,syn) <- sinks tds lmh work
                         (ipart,swork) <- predsI tds lmh inh
                         return (work ++ ipart,syn ++ swork)
-- The synthesized sinks-component and the resulting inherited sinks
predsS tds lmh []   = return ([],[])
predsS tds lmh work = do (inh,syn) <- sinks tds lmh work
                         (spart,iwork) <- predsS tds lmh syn
                         return (work ++ spart,inh ++ iwork)

-- Removes edges from vs, and returns resulting sinks, partitioned into inherited and synthesized
sinks :: Tds s -> LMH -> [Vertex] -> ST s ([Vertex],[Vertex])
sinks tds (l,m,h) vs
  = liftM (partition (<m) . concat) $ mapM f' [l..h]
     where f' i = do e <- readArray tds i
                     if null e
                      then if null vs then return [i] else return []
                      else do let e' = e \\ vs
                              writeArray tds i e'
                              if null e' && not (i `elem` vs) then return [i] else return []

-------------------------------------------------------------------------------
-- Cycles
-------------------------------------------------------------------------------
cycles2 :: Tds s -> [Edge] -> ST s [Edge]
cycles2 tds s2i = concatMapM (cycles2' tds) s2i

cycles2' :: Tds s -> Edge -> ST s [Edge]
cycles2' tds (v1,v2) = do e <- readArray tds v2
                          return (if v1 `elem` e then [(v1,v2)] else [])

cycles3 :: Info -> Tds s -> [Interface] -> ST s [(Edge,Interface)]
cycles3 info tds inters = concatMapM (cycles3' tds) (zip (lmh info) inters)

cycles3' :: Tds s -> (LMH,Interface) -> ST s [(Edge,Interface)]
cycles3' tds ((l,m,h),inter)
  = concatMapM (cyc tds) [m..h]
      where cyc :: Tds s -> Vertex -> ST s [(Edge,Interface)]
            cyc tds i = do e <- readArray tds i
                           concatMapM (toSelf tds i) e
            toSelf :: Tds s -> Vertex -> Vertex -> ST s [(Edge,Interface)]
            toSelf tds i j | j < m     = do e' <- readArray tds j
                                            if i `elem` e' then return [((i,j),inter)] else return []
                           | otherwise = return []

-- The graph of direct depencencies,
--     (Having edges between Rhs attribute occurrences and their corresponding Lhs counterparts)
directGraph :: Info -> [Edge] -> Graph
directGraph info dpr 
  = buildG (l,h) (dpr ++ edges)
      where (l,h) = Data.Array.bounds (tdpToTds info)
            edges = [if isInh s then (s,t) else (t,s) | s <- [l..h], isRhs s, t <- tdsToTdp info ! (tdpToTds info ! s), isLhs t]
            isRhs s = let (rnt,_,_,_) = tdpNt info ! s
                      in rnt /= nullIdent
            isLhs s = let (_,_,_,f) = tdpNt info ! s
                      in f == _LHS
            isInh s = lmhInhFind (lmh info) (tdpToTds info ! s)
            lmhInhFind [] s = False
            lmhInhFind ((l,m,h):lmh) s = (l <= s && s < m) || lmhInhFind lmh s

-- The path in the direct graph that gave the cycle.
cyclePath :: Info -> Graph -> Edge -> Maybe [Vertex]        
cyclePath info graph (v1,v2)
  = shortestMaybe $ paths
      where paths = [(liftM reverse) (path graph t s) | s <- tdsToTdp info ! v1, t <- tdsToTdp info ! v2, isSameLhs s t]
            isSameLhs s t = let (_,rnt,c,f) = tdpNt info ! s
                                (_,rnt',c',f') = tdpNt info ! t
                            in rnt == rnt' && c == c' && f == _LHS && f' == _LHS

shortestMaybe :: [Maybe [a]] -> Maybe [a]
shortestMaybe [] = Nothing
shortestMaybe lst = minimumBy shortest lst
                    where shortest Nothing ml2 = GT
                          shortest ml1 Nothing = LT
                          shortest (Just l1) (Just l2) = length l1 `compare` length l2
                      
path :: Graph -> Vertex -> Vertex -> Maybe [Vertex]
path graph from to 
  = path' [] from to
      where path' prev from to 
              | from == to       = Just [from]
              | from `elem` prev = Nothing
              | otherwise        = liftM (from:) $ shortestMaybe $ map (\x -> path' (from:prev) x to) (graph ! from)

-------------------------------------------------------------------------------
-- Visit sub-sequences - Graph
-------------------------------------------------------------------------------
visitssGraph :: Info -> Graph -> Vertex -> [Interface] -> (Graph,[(Vertex,ChildVisit)])
visitssGraph info tdp v inters
  = let (rhsedges,fs,v') = rhsEdges info v inters
        tdp' = buildG (0,v'-1) (rhsedges ++ edges tdp)
        visitedges = visitEdges info tdp' v (v'-1)
        tdp'' = transposeG $ buildG (0,v'-1) (visitedges ++ edges tdp')
    in (tdp'',fs)

rhsEdges :: Info -> Vertex -> [Interface] -> ([Edge],[(Vertex,ChildVisit)],Vertex)
rhsEdges info v [] = ([],[],v)
rhsEdges info v (inter:inters) 
  = let (edges,fs,v') = rhsEdge info 0 v inter 
        (rest,fs',v'') = rhsEdges info v' inters
    in (edges++rest,fs++fs',v'')

rhsEdge :: Info -> Int -> Vertex -> Interface -> ([Edge],[(Vertex,ChildVisit)],Vertex)
rhsEdge info n v [] = ([],[],v)
rhsEdge info n v ((inh,syn):inter)
  = let rhsinh = map (\x -> (x,True))  $ concatMap (tdsToTdp info !) inh
        rhssyn = map (\x -> (x,False)) $ concatMap (tdsToTdp info !) syn
        classes = eqClasses comp $ sort $ filter lhs $ map (\(v,b) -> (tdpNt info ! v,v,b)) $ rhsinh ++ rhssyn
        comp (a,_,_) (a',_,_) = a == a'
        lhs ((_,_,_,field),_,_) | field == _LHS = False
                                | otherwise = True
        islast = null inter
        childvisits = zip [v..] $ map ((\((_,_,_,field),_,_) -> ChildVisit field n islast) . head' "childvisits") classes
        edges = makeEdges v classes
        l = length classes
        (rest,fs,v') = rhsEdge info (n+1) (v+l) inter
    in (edges ++ rest,childvisits ++ fs,v')
        
makeEdges :: Int -> [[(a,Vertex,Bool)]] -> [Edge]
makeEdges n [] = []
makeEdges n (x:xs) 
  = map (makeEdge n) x ++ makeEdges (n+1) xs
      where makeEdge :: Int -> (a,Vertex,Bool) -> Edge
            makeEdge n (_,v,True) = (v,n)
            makeEdge n (_,v,False) = (n,v)

-- The edges between visits: Visit n+1 depends on visit n
visitEdges :: Info -> Graph -> Int -> Int -> [Edge]
visitEdges info tdp l h 
  = concatMap list2edges $ map (sort . map snd) $ eqClasses comp $ sort $ map (\x -> (tdpNt info ! head' (show x) (tdp ! x),x)) [l..h]
      where comp (a,_) (a',_) = a == a'
            list2edges []        = []
            list2edges [a]       = []
            list2edges (a:b:abs) = (a,b):list2edges (b:abs)

-------------------------------------------------------------------------------
-- Visit sub-sequences
-------------------------------------------------------------------------------
-- For each Nt, for each prod, for each visit, a subsequence
visitss :: Info -> [(Vertex,ChildVisit)] -> Graph -> [Interface] -> [[[VisitSS]]]
visitss info table vsgraph inters = map (transpose . visitss' info vsgraph []) inters

visitss' :: Info -> Graph -> [Vertex] -> Interface -> [[VisitSS]]
visitss' info vsgraph prev [] = []
visitss' info vsgraph prev (inter:inters) 
  = let (ss,prev') = visitss'' info vsgraph prev inter
    in ss:visitss' info vsgraph prev' inters

-- prev: Attributes computed in previous visits
-- (inh,syn): Attributes in this visit
visitss'' :: Info -> Graph -> [Vertex] -> ([Vertex],[Vertex]) -> ([VisitSS],[Vertex])
visitss'' info vsgraph prev (inh,syn) 
  = let sortFrom = map (map snd) $ eqClasses comp $ sort $ filter (lhs . fst) $ map (\x -> (tdpNt info ! x,x)) $ concatMap (tdsToTdp info !) syn
        inh' = filter (\x -> lhs  (tdpNt info ! x)) $ concatMap (tdsToTdp info !) inh
        lhs (_,_,_,x) = x == _LHS
        comp (a,_) (a',_) = a == a'
        prev' = inh' ++ prev
        trans vs = vs \\ prev'
        vss = map (trans . topSort' vsgraph) sortFrom
    in (vss, concat vss ++ prev')

-------------------------------------------------------------------------------
-- Graph-like functions
-------------------------------------------------------------------------------
postorder (Node a ts) = postorderF ts ++ [a]
postorderF = concatMap postorder
postOrd g = postorderF . dfs g
topSort' g = postOrd g

-------------------------------------------------------------------------------
-- Prelude-like functions
-------------------------------------------------------------------------------
-- Gives equivalence classes, given an ORDERED list
eqClasses :: (a -> a -> Bool) -> [a] -> [[a]]
eqClasses p [] = []
eqClasses p (a:as) 
  = eqC [a] as
     where eqC as [] = [as]
           eqC as (a:as') | p a (head' "eqClasses" as) = eqC (a:as) as'
                          | otherwise     = reverse as : eqC [a] as'

concatMapM f xs = liftM concat $ mapM f xs
carthesian xs ys = [(x,y) | x <- xs, y <- ys]

--DEBUG
head' a []  = error a
head' _ (x:xs) = x

