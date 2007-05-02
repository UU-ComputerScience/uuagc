\begin{code}
module SequentialComputation {- (
    computeSequential, 
) -} where
import SequentialTypes
import CommonTypes
import Interfaces
import InterfacesRules
import CodeSyntax
import GrammarInfo

import Debug.Trace
import System.Time
import System.IO.Unsafe
import Control.Monad(liftM,when)
import Control.Monad.ST(ST, runST)
import Data.Graph(Edge, Graph, Vertex, buildG)
import Data.Array(Array,(!),bounds)
import Data.Array.ST(STArray, newArray, readArray, writeArray, freeze)
import Data.Maybe(listToMaybe,mapMaybe,isJust,fromJust)
import Data.List(partition,nub,(\\),delete,minimumBy)
import qualified Data.Set as Set

\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section{Collecting information}

In the Data.Graph library,
a graph is represented as |Array Vertex [Vertex]|,
mapping each vertex to a list of adjacent vertices.
A |Vertex| is simply encoded by an |Int|.
So to test whether an edge |(x,y)| belongs to |g|
we can evaluate |y `elem` g!x|

Our algorithm will mostly be adding edges to the graph,
so we use a mutable array.
For even more efficiency, we use Sets instead of lists.
If we want to use any of the library
functions, we can convert our representation by |fmap Set.tList . freeze|.

\begin{code}
type STGraph s = STArray s Vertex (Set.Set Vertex)
type SGraph    = Array     Vertex (Set.Set Vertex)
\end{code}

We can add an edge to a graph, or remove it. These functions return
whether they did something (resp. addition or removal) or not. hasEdge
only checks whether a graph contains an edge or not.

\begin{code}
addEdge, removeEdge, hasEdge :: STGraph s -> Edge -> ST s Bool
addEdge graph (u,v) = do  e <- readArray graph u
                          if  Set.member v e
                              then return False
                              else do  writeArray graph u (Set.insert v e)
                                       return True

removeEdge graph (u,v) = do  e <- readArray graph u
                             if  Set.member v e
                                 then do  (writeArray graph u (Set.delete v e))
                                          return True
                                 else return False

hasEdge graph (u,v) = do  e <- readArray graph u
                          return (Set.member v e)
\end{code}

The first step is to assign a number to all attributes, and a
different one to all attribute occurrences. We create an array mapping
the numbers to the information about the attribute occurrences
(|ruleTable|), so we can look up this information in $O(1)$ time. We
also build mappings from attributes to their occurrences (|tdsToTdp|)
and vice versa (|tdpToTds|). |LMH| indicates the division of the
attributes - an element |(l,m,h) `elem` LMH| means that vertices |i, l
<= i <= h| are attributes of the same nonterminal, with vertices |j, l
<= j < m| being inherited and |k, m <= k <= h| being synthesized
attributes.

See the |SequentialTypes.Info| and |SequentialTypes.LMH|

Then we collect the direct dependencies, using the integer
representations. This list of tuples (edges in the dependency graph)
all information that is collected is passed to a function that will
compute the interfaces and visit sub-sequences. We cannot do this
computation in AG, because mutable arrays require the ST monad, which
cannot be used inside AG.

Now we can build a graph for attributes, and a graph for ao's, and add
the direct dependencies to the ao graph. Like Pennings we will call
the attribte graph Tds (transitive dependencies of symbols), and the
ao-graph Tdp (transitive dependencies of productions). Unlike him, we
will have only one Tds and one Tdp graph. In |STGraph|, we can lookup
outgoing edges in |O(1)| time, but looking up incoming edges will take
|O(e)| time, where |e| is the number of edges in the graph. As we will
be doing this quite often it is worthwhile to keep both Tdp and its
transposed version. The computation will involve both Tds and Tdp. It
treats specially. TODO elaborate on that.

\begin{code}
type Tdp s = (STGraph s, STGraph s)
type Tds s = STGraph s 
type Comp s = (Tds s, Tdp s)
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section{Generating IDS}

As we insert edges into Tdp we keep it transitively closed, so every
time we add the edge $(s,t)$ to V, we also add the edges 
$\{ (r,t) || (r,s) \in V \}$ and 
$\{ (s,u) || (t,u) \in V \}$.

\begin{code}
insertTdp :: Info -> Comp s -> Edge -> ST s ()
insertTdp info comp@(_,(tdpN,tdpT)) e@(s,t)                -- how to insert an edge (s,t):
  = ifM  (hasEdge tdpN e)                                  -- if it's already there...
         (return ())                                       -- ...we're done; otherwise:
         (do  rs <- readArray tdpT s                       -- find all sources r for an edge to s
              us <- readArray tdpN t                       -- find all targets u for an edge from t
              let edges = cartesian (s:Set.toList rs) (t:Set.toList us) -- construct (s,t) but also (r,t) and (s,u) and even (r,u)
              mapM_ (addTdpEdge info comp) edges           -- and add all of them, without having to bother about transitive closure anymore
         )

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM b t e = do  b' <- b
                if b' then t else e

cartesian :: [a] -> [b] -> [(a,b)]
cartesian as bs = [(a,b) | a <- as, b <- bs]

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = liftM concat (mapM f xs)
\end{code}

Edges in |Tdp| can induce edges in |Tds|, so whenever we add
an edge, we also add the induced edge if necessary

\begin{code}
addTdpEdge :: Info -> Comp s -> Edge -> ST s ()        -- how to add an edge (s,t) without bothering about the transitive closure:
addTdpEdge info comp@(_,(tdpN,tdpT)) e@(s,t)
  = ifM  (addEdge tdpN e)                                  -- add it to the normal graph, and if it was not already there:
         (do  addEdge tdpT (t,s)                           --   also add it to the transposed graph
              let  u = tdpToTds info ! s                   -- find the corresponding attributes...
                   v = tdpToTds info ! t
                   nonlocal = u /= -1 && v /= -1
                   equalfield = isEqualField (ruleTable info ! s) (ruleTable info ! t)
              when (nonlocal && equalfield)                -- ...and when necessary...
                   (insertTds info comp (u,v))             -- ...insert it to the Tds graph
         )
         (return ())
\end{code}

Inserting edges into |Tds| will insert edges between the occurrences
of the attributes into |Tdp|.
\end{notes}

\begin{code}
insertTds :: Info -> Comp s -> Edge -> ST s ()
insertTds info comp@(tds,_) e@(u,v)
  =  do ifM  (addEdge tds e) 
             (mapM_ (insertTdp info comp) [ (s,t)
                                          | s <- tdsToTdp info ! u
                                          , t <- tdsToTdp info ! v
                                          , isEqualField (ruleTable info ! s) (ruleTable info ! t)
                                          ]
             ) 
             (return ())
\end{code}

If we add the direct dependencies to the Tdp graph in this way, the
Tds graph is filled with IDS.

\begin{code}
simpleInsert :: Info -> Comp s -> Edge -> ST s [Vertex]
simpleInsert info comp@(_,(tdpN,tdpT)) e@(s,t)
  = ifM  (hasEdge tdpN e)
         (return [])
         (do  rs <- readArray tdpT s
              us <- readArray tdpN t
              let edges = cartesian (s:Set.toList rs) (t:Set.toList us)
              concatMapM (addSimpleEdge info comp) edges)

addSimpleEdge :: Info -> Comp s -> Edge -> ST s [Vertex]
addSimpleEdge info comp@(_,(tdpN,tdpT)) e@(s,t)
  = ifM  (hasEdge tdpN (t,s)) 
         (return [s])
         (do addEdge tdpN e
             addEdge tdpT (t,s)
             return [])

\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section{Cycle detection}

\begin{code}
-- dummy definition because the real definition hangs in some situations
cyclePath :: Info -> Graph -> Graph -> Edge -> Maybe [Vertex]
cyclePath info tds dp (u,v) = Just []

cyclePathHangsInSomeSituations :: Info -> Graph -> Graph -> Edge -> Maybe [Vertex]
cyclePathHangsInSomeSituations info tds dp (u,v)
  =  let  is = [ init (fromJust lower) ++ fromJust top
               | s <- tdsToTdp info ! u
               , t <- tdsToTdp info ! v
               , isRhsOfSameCon (ruleTable info ! s) (ruleTable info ! t) 
               , let top = directpath dp s t, isJust top
               , let lower = lowerpath info tds dp [] False t s, isJust lower
               ]
     in if null is then Nothing else Just (head is)
          

-- Returns a path between vertices
directpath :: Graph -> Vertex -> Vertex -> Maybe [Vertex]
directpath g s t 
  =  path [] s
     where  path prev s
              | s == t = Just [t]
              | s `elem` prev = Nothing
              | otherwise =  listToMaybe . map (s:) . mapMaybe (path (s:prev)) $ (g ! s)

{-
p1      X t
        |  
      s Y fs

TDS  s' Y fs'
-}

-- Returns a path between an inherited and a synthesized rhs vertex
lowerpath :: Info -> Graph -> Graph -> [Vertex] -> Bool -> Vertex -> Vertex -> Maybe [Vertex]
lowerpath info tds dp prev allowlhs s t
  =  path prev s
     where  path prev s
              | s == t = Just [t]
              | s `elem` prev = Nothing
              | getIsIn srule || isLocal srule = listToMaybe . map (s:) . mapMaybe (path (s:prev)) $ (dp ! s)
              | isLhs srule = Nothing
              | otherwise = let  s' = tdpToTds info ! s
                                 fs' = filter (isSynAttr . (attrTable info !)) (tds ! s')
                                 fs = filter eq (concatMap (tdsToTdp info !) fs')
                                 rest = mapMaybe (path (s:prev)) fs
                                 result = [ s:(fromJust bot ++ es)
                                          | es@(f:_) <-  rest
                                          , let  f' = tdpToTds info ! f
                                                 bot = i2spath info tds dp (es ++ prev) s' f' 
                                          , isJust bot ]
                            in if null result then Nothing else Just (head result)
              where srule = ruleTable info ! s
                    eq t = isEqualField srule (ruleTable info ! t)

-- Assuming a inh to syn dependency in TDS, return a path
i2spath :: Info -> Graph -> Graph -> [Vertex] -> Vertex -> Vertex -> Maybe [Vertex]
i2spath info tds dp prev u v 
  =  let  es = [ (s,t)
               | s <- tdsToTdp info ! u, let s' = ruleTable info ! s
               , t <- tdsToTdp info ! v, let t' = ruleTable info ! t
               , not (s `elem` prev)
               , isLhs s'
               , isLhs t'
               , isEqualField s' t' 
               ]
          paths = mapMaybe (\(s,t) -> lowerpath info tds dp prev True s t) es
     in if null paths then Nothing else Just (head paths)
\end{code}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section{Interfaces}

In absence of cycles we can find the interfaces. We only take
attributes that are used.

When an attribute has no incoming edges it can be computed. As the
emphasis is on incoming edges, we will work with the transposed Tds
graph. The funtion |used| indicates which vertices are included in the
interfaces.

See modules Interfaces and InterfacesRules for more information.

%format sem_IRoot_IRoot = "sem_{IRoot}"
%format sem_Interface_Interface = "sem_{Interface}"
%format sem_Interfaces_Cons = ":_{Interfaces}"
%format sem_Interfaces_Nil = "[]_{Interfaces}"
%format sem_Segments_Cons = ":_{Segments}"
%format sem_Segments_Nil = "[]_{Segments}"

\begin{code}
makeInterfaces :: Info -> Graph -> T_IRoot
makeInterfaces info tds
  =  let interslist = reverse . makeInterface tds []
         mkSegments = foldr (sem_Segments_Cons . uncurry sem_Segment_Segment) sem_Segments_Nil . interslist
         mkInter ((nt,cons),lmh) = sem_Interface_Interface nt cons (mkSegments lmh)
         inters = foldr (sem_Interfaces_Cons . mkInter) sem_Interfaces_Nil (zip (prods info) (lmh info))
     in sem_IRoot_IRoot inters
\end{code}

The sinks of a graph are those vertices that have no outgoing
edges. We define a function that determines whether a vertex is a sink
if a set |del| of vertices had been removed from the graph. This means
that the attribute can be computed if all attributes in |del| have
been computed.

\begin{code}
isSink :: Graph -> [Vertex] -> Vertex -> Bool
isSink graph del v = null (graph ! v \\ del)
\end{code}

Now we can make interfaces by taking inherited sinks and synthesized
sinks alternatively. If there are no synthesized attributes at all,
generate an interface with one visit computing nothing.

\begin{code}
makeInterface :: Graph -> [Vertex] -> LMH -> [([Vertex],[Vertex])]
makeInterface tds del (l,m,h)
  | m > h = [([],[])]
  | otherwise = let  syn = filter (isSink tds del) ([m..h] \\ del)
                     del' = del ++ syn
                     inh = filter (isSink tds del') ([l..(m-1)] \\ del')
                     del'' = del' ++ inh
                     rest = makeInterface tds del'' (l,m,h)
                in if  null inh && null syn
                       then []
                       else (inh,syn) : rest
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section{Detecting of cycles}

We only want to return s2i edges.

\begin{code}
findCycles :: Info -> SGraph -> [Edge]
findCycles info tds 
  = [ (v,u)  
    | (l,m,h) <- lmh info        -- for every nonterminal: [l..m-1] are inherited, [m..h] are synthesized
    , v <- [m..h]                -- for every synthesized attribute
    , u <- Set.toList (tds ! v)  -- find dependent attributes...
    , l <= u, u < m              -- ...that are inherited...
    , Set.member v (tds ! u)     -- ...and have a cycle back
    ]
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section{Tying it together}

\begin{code}
getResult :: Info -> Graph -> Graph -> [Edge] -> (CInterfaceMap, CVisitsMap, [Edge])
getResult info tds tdp dpr
  = let  inters = makeInterfaces info tds
         inhs = Inh_IRoot  {  info_Inh_IRoot = info
                           ,  tdp_Inh_IRoot = tdp
                           ,  dpr_Inh_IRoot = dpr}
         iroot = wrap_IRoot inters inhs
    in (inters_Syn_IRoot iroot, visits_Syn_IRoot iroot, edp_Syn_IRoot iroot)

reportLocalCycle :: Graph -> [Vertex] -> [(Vertex,[Vertex])]
reportLocalCycle dp ss
  = let  ds = [(s,fromJust mes)  
              | s <- ss
              , let mes = spath dp s s
              , isJust mes 
              ]
    in if  null ds 
           then error "'Local to Local'-cycle found, but no paths" 
           else ds


reportDirectCycle info tds2 dp cyc2
  = [ (e, fromJust mes) 
    | e <- cyc2
    , let mes = cyclePath info tds2 dp e
    , isJust mes 
    ]



spath :: Graph -> Vertex -> Vertex -> Maybe [Vertex]
spath graph from to 
  =  path [[v,from] | v <- graph ! from] []
     where path [] _ = Nothing
           path (l@(v:p):wl) prev 
             | v == to = Just (reverse l)
             | v `elem` prev = path wl prev
             | otherwise = path (wl ++ map (:l) (graph ! v)) (v:prev)


isLocLoc rt (s,t) = isLocal (rt ! s) && isLocal (rt ! t)


computeSequential :: Info -> [Edge] -> SeqResult
computeSequential info dpr
  = runST
    (do let bigBounds   = bounds (tdpToTds info)
            smallBounds = bounds (tdsToTdp info)
            (ll,es) = partition (isLocLoc (ruleTable info)) dpr
            graph = buildG bigBounds dpr
        tds  <- newArray smallBounds Set.empty
        tdp  <- newArray bigBounds   Set.empty
        tdpT <- newArray bigBounds   Set.empty
        --let tm1 = unsafePerformIO getClockTime
        --zzz <- trace (show tm1 ++ "\nll=" ++ show ll) (return ())
        --zzz <- trace ("es=" ++ show es) (return ())
        let comp = (tds,(tdp,tdpT))
        cyc1 <- concatMapM (simpleInsert info comp) ll                                          -- insert the local dependencies
        if  not (null cyc1)                                                                     -- are they cyclic?
            then return (LocalCycle (reportLocalCycle graph cyc1))                              -- then report an error.
            else do  mapM_ (insertTdp info comp) es                                             -- insert the other dependencies
                     tds2 <- freeze tds
                     let tds2list = fmap Set.toList tds2
                     --let tm2 = unsafePerformIO getClockTime
                     --    tf2 = show (normalizeTimeDiff (diffClockTimes tm2 tm1))
                     --zzz <- trace (tf2 ++ " tds2=" ++ show tds2) (return ())
                     let cyc2 = findCycles info tds2
                     --zzz <- trace ("cyc2" ++ show cyc2) (return ())
                     if  not (null cyc2)                                                        -- are they cyclic?
                         then return (DirectCycle (reportDirectCycle info tds2list graph cyc2)) -- then report an error.
                         else do  tdp2 <- freeze tdp
                                  let tdp2list = fmap Set.toList tdp2
                                  --let tm3 = unsafePerformIO getClockTime
                                  --    tf3 = show (normalizeTimeDiff (diffClockTimes tm3 tm2))
                                  --zzz <- trace (tf3 ++ " tdp2=" ++ show tdp2) (return ())
                                  --zzz <- trace ("starting getResult\n") (return ())
                                  let  (cim,cvm,edp) = getResult info tds2list tdp2list dpr
                                  --let tm4 = unsafePerformIO getClockTime
                                  --    tf4 = show (normalizeTimeDiff (diffClockTimes tm4 tm3))
                                  --zzz <- trace (tf4 ++ " edp=" ++ show edp) (return ())
                                  mapM_ (insertTds info comp) edp                               -- insert dependencies resulting from visit scheduling
                                  tds3 <- freeze tds
                                  --let tm5 = unsafePerformIO getClockTime
                                  --    tf5 = show (normalizeTimeDiff (diffClockTimes tm5 tm4))
                                  --zzz <- trace (tf5 ++ " tds3=" ++ show tds3) (return ())
                                  let cyc3 = findCycles info tds3
                                  if  not (null cyc3)                                           -- are they cyclic?
                                      then return (InducedCycle cim cyc3)                       -- then report an error.
                                      else return (CycleFree cim cvm)                           -- otherwise we succeed.
    )
\end{code}

\end{document}
