\begin{code}
module SequentialComputation {- (
    computeSequential, 
) -} where
import Debug.Trace
import SequentialTypes
import CommonTypes
import Interfaces
import InterfacesRules
import CodeSyntax
import GrammarInfo

import Control.Monad(liftM)
import Control.Monad.ST(ST, runST)
import Data.Graph(Edge, Graph, Vertex, buildG)
import Data.Array((!),bounds)
import Data.Array.ST(STArray, newArray, readArray, writeArray, freeze)
import Data.Maybe(listToMaybe,mapMaybe,isJust,fromJust)
import Data.List(partition,nub,(\\),delete,minimumBy)
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section{Collecting information}

In the GHC representation of graphs |Vertex| is an |Int|, and the
edges are represented as an Array, mapping nodes to a list of
nodes. An edge |(x,y)| of graph |a| is represented as |y `elem` a!x|
(|!| looks up a value in an array). This has the advantage of using
less memory. Our algorithm will mostly be adding edges to the graph,
so we use a mutable array. If we want to use any of the library
functions, we can freeze the mutable array.

\begin{code}
type STGraph s = STArray s Vertex [Vertex]
\end{code}

We can add an edge to a graph, or remove it. These functions return
whether they did something (resp. addition or removal) or not. hasEdge
only checks whether a graph contains an edge or not.

\begin{code}
addEdge, removeEdge, hasEdge :: STGraph s -> Edge -> ST s Bool
addEdge graph (u,v) = do  e <- readArray graph u
                          if  v `elem` e
                              then return False
                              else do  writeArray graph u (v:e)
                                       return True

removeEdge graph (u,v) = do  e <- readArray graph u
                             if  (v `elem` e)
                                 then do  (writeArray graph u (delete v e))
                                          return True
                                 else return False

hasEdge graph (u,v) = do  e <- readArray graph u
                          return (v `elem` e)
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
time we add the edge $(s,t)$ to V, we also add the edges $\{ (r,t) ||
(t,s) \in V \}$ and $\{ (s,u) || (t,u) \in V \}$.

\begin{code}
insertTdp :: Info -> Comp s -> Edge -> ST s [Edge]
insertTdp info comp@(_,(tdpN,tdpT)) (s,t)
  = ifM  (hasEdge tdpN (s,t))
         (return [])
         (do  rs <- readArray tdpT s
              us <- readArray tdpN t
              let edges = carthesian (s:rs) (t:us)
              concatMapM (addTdpEdge info comp) edges)

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM b t e = do  b' <- b
                if b' then t else e

carthesian :: [a] -> [b] -> [(a,b)]
carthesian as bs = [(a,b) | a <- as, b <- bs]

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = liftM concat (mapM f xs)
\end{code}

Edges in |Tdp| can induce edges in |Tds|, so we check whether we add
an edge, and call induce:

\begin{code}
addTdpEdge :: Info -> Comp s -> Edge -> ST s [Edge]
addTdpEdge info comp@(_,(tdpN,tdpT)) (s,t)
  = ifM  (addEdge tdpN (s,t))
         (do  addEdge tdpT (t,s)
              induce info comp (s,t))
         (return [])
\end{code}

An edge in |Tdp| only induces an edge in |Tds| if they are both
left-hand-side attributes or attributes of the same child.

\begin{code}
induce :: Info -> Comp s -> Edge -> ST s [Edge]
induce info comp (s,t)
  =  let  u = tdpToTds info ! s
          v = tdpToTds info ! t
          nonlocal = u /= -1 && v /= -1
          equalfield = isEqualField (ruleTable info ! s) (ruleTable info ! t)
     in if  nonlocal && equalfield
            then insertTds info comp (u,v)
            else return []
\end{code}

Inserting edges into |Tds| will insert edges between the occurrances
of the attributes into |Tdp|. If the computation is only done to find
cycles, we do not add s2i-edges. In this case |u > v| for the edge
|(u,v)| indicates an s2i-edge. \begin{notes} TODO: Is it clear why? If
not, we can explain: There are only edges between attributes of the
same nonterminal.
\end{notes}

\begin{code}
insertTds :: Info -> Comp s -> Edge -> ST s [Edge]
insertTds info comp@(tds,_) (u,v)
  =  do vs <- ifM  (addEdge tds (u,v)) 
                   (occur info comp (u,v)) 
                   (return [])
        if  cyclesOnly info && isSynAttr (attrTable info ! u) && isInhAttr (attrTable info ! v)
            then return ((u,v):vs)
            else return vs
\end{code}

\begin{code}
occur :: Info -> Comp s -> Edge -> ST s [Edge]
occur info comp (u,v)
  =  let  ss = tdsToTdp info ! u
          ts = tdsToTdp info ! v
          eqField (s,t) = isEqualField (ruleTable info ! s) (ruleTable info ! t)
          es = filter eqField (carthesian ss ts)
     in concatMapM (insertTdp info comp) es
\end{code}

If we add the direct dependencies to the Tdp graph in this way, the
Tds graph is filled with IDS.

\begin{code}
simpleInsert :: Info -> Comp s -> Edge -> ST s [Vertex]
simpleInsert info comp@(_,(tdpN,tdpT)) (s,t)
  = ifM  (hasEdge tdpN (s,t))
         (return [])
         (do  rs <- readArray tdpT s
              us <- readArray tdpN t
              let edges = carthesian (s:rs) (t:us)
              concatMapM (addSimpleEdge info comp) edges)

addSimpleEdge :: Info -> Comp s -> Edge -> ST s [Vertex]
addSimpleEdge info comp@(_,(tdpN,tdpT)) (s,t)
  = ifM  (hasEdge tdpN (t,s)) 
         (return [s])
         (do addEdge tdpN (s,t)
             addEdge tdpT (t,s)
             return [])

data IDP s =  IDP (Comp s) [Edge]
           |  LLCycle [Vertex]

tdsTdp :: Info -> [Edge] -> ST s (IDP s)
tdsTdp info dpr 
   = let  (ll,es) = partition isLocLoc dpr
          isLocLoc (s,t) = isLoc s && isLoc t
          isLoc s = isLocal (ruleTable info ! s)
     in do  tds  <- newArray (bounds (tdsToTdp info)) []
            tdpN <- newArray (bounds (tdpToTds info)) []
            tdpT <- newArray (bounds (tdpToTds info)) []
            let comp = (tds,(tdpN,tdpT))
            llcycles <- concatMapM (simpleInsert info comp) ll
            if  null llcycles
                then do  es <- concatMapM (insertTdp info comp) es
                         return (IDP comp (nub es))
                else return (LLCycle llcycles)
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section{Cycle detection}

The computation returns s2i edges. If for any s2i edge $(u,v)$ there
is an edge $(v,u)$ then we have found a cycle.

\begin{code}
cycles2 :: Graph -> [Edge] -> [Edge]
cycles2 tds s2i = [(u,v) | (u,v) <- s2i, u `elem` tds ! v ]
\end{code}

\begin{code}
cyclePath :: Info -> Graph -> Graph -> Edge -> Maybe [Vertex]
cyclePath info tds dp (u,v)
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
               , isLhs s', isLhs t', isEqualField s' t' ]
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
\section{Detecting type-3 cycles}

After the generation of interfaces we need to check Tds for induced
(type-3) cycles. We only want to return s2i edges. There is no need to
take into account removed vertices, as they can never form a cycle.

\begin{code}
cycles3 :: Info -> Graph -> [Edge]
cycles3 info tds = [ (v,u)  | (l,m,h) <- lmh info
                            , v <- [m..h]
                            , u <- tds ! v
                            , l <= u, u < m
                            , v `elem` tds ! u ]
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

reportCycle :: Graph -> [Vertex] -> [(Vertex,[Vertex])]
reportCycle dp ss
  = let  ds = [(s,fromJust mes)  
              | s <- ss
              , let mes = spath dp s s, isJust mes ]
    in if  null ds 
           then error "'Local to Local'-cycle found, but no paths" 
           else ds

spath :: Graph -> Vertex -> Vertex -> Maybe [Vertex]
spath graph from to 
  =  path' [[v,from] | v <- graph ! from] []
     where path' [] _ = Nothing
           path' (l@(v:p):wl) prev 
             | v == to = Just (reverse l)
             | v `elem` prev = path' wl prev
             | otherwise = path' (wl ++ map (:l) (graph ! v)) (v:prev)

computeSequential :: Info -> [Edge] -> SeqResult
computeSequential info dpr
  = runST  (do  let dp = buildG (bounds (tdpToTds info)) dpr
                init <- tdsTdp (info{cyclesOnly = True}) dpr
                case init of
                  LLCycle ss -> return (LocLocCycle (reportCycle dp ss))
                  IDP (comp@(tds,(tdp,_))) s2i  ->  do  tds' <- freeze tds
                                                        let cyc2 = cycles2 tds' s2i
                                                        if  not (null cyc2) 
                                                            then let errs = [(e,fromJust mes) 
                                                                            | e <- cyc2
                                                                            , let mes = cyclePath info tds' dp e, isJust mes ]
                                                                 in return (DirectCycle errs)
                                                            else do  tdp' <- freeze tdp
                                                                     let  (cim,cvm,edp) = getResult info tds' tdp' dpr
                                                                     mapM_ (insertTds (info{cyclesOnly = False}) comp) edp
                                                                     tds' <- freeze tds
                                                                     let cyc3 = (cycles3 info tds')
                                                                     if  (not (null cyc3))
                                                                         then return (InducedCycle cim cyc3)
                                                                         else return (SeqResult cim cvm)
           )
\end{code}

\end{document}
