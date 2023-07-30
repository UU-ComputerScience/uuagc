module LOAG.Graphs where

import Control.Monad (forM, forM_, when)
import Control.Monad.ST
import Control.Monad.State
import CommonTypes
import Data.STRef
import Data.Maybe (catMaybes, isNothing, fromJust)
import Data.Tuple (swap)
import qualified Data.Array as A
import           Data.Array.IArray (amap)
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Array.MArray (mapArray)
import Data.Array.ST

type Schedule   = (A.Array Vertex (Maybe Int), A.Array Int [Vertex])
type Vertex     = Int
type Cycle      = IS.IntSet
type Vertices   = IS.IntSet
type Edge       = (Vertex, Vertex)
type Edges      = S.Set Edge
-- Maps that are suitable for Graphs (from 1 node to a set of nodes)
type Graph s    = (DirGraphRef s, DirGraphRef s)
-- | Frozen version of a graph
type FrGraph    = (DirGraph, DirGraph)
type DirGraph   = A.Array Vertex Vertices
type DirGraphRef s = STArray s Vertex Vertices

-- |----------------------------------------------------------------------
-- | Functions for changing the state within AOAG
-- |  possibly catching errors from creating cycles

addEDs :: Graph s -> [Edge] -> (ST s) (Maybe (Edge, Cycle))
addEDs _ [] = return Nothing
addEDs edp (e:es) = do
    res <- e `inserT` edp
    case res of
        Right _ -> addEDs edp es
        Left c  -> return $ Just (e,c)
       
-- | Draws an edge from one node to another, by adding the latter to the
--    node set of the first
insErt :: Edge -> Graph s -> (ST s) ()
insErt (f, t) g@(ft,tf) = do 
    ts <- readArray ft f
    fs <- readArray tf t
    writeArray ft f (t `IS.insert` ts)
    writeArray tf t (f `IS.insert` fs)

removE :: Edge -> Graph s -> (ST s) ()
removE e@(f,t) g@(ft,tf) = do 
    ts <- readArray ft f
    fs <- readArray tf t
    writeArray ft f (t `IS.delete` ts)
    writeArray tf t (f `IS.delete` fs)


-- | Revert an edge in the graph
revErt :: Edge -> Graph s -> (ST s) ()
revErt e g = do
    present <- member e g
    when present $ removE e g >> insErt (swap e) g

-- | Assuming the given graph is already transitively closed, and
-- |    not cyclic, insert an 
-- |    edge such that the graph maintains transitively closed.
-- |    returns the cycle if this results in a cycle or returns a pair
-- |    (graph, edges) if not. Where graph is the new Graph and 
-- |    edges represent the edges that were required for transitively
-- |    closing the graph.
inserT :: Edge -> Graph s -> (ST s) (Either Cycle [Edge])
inserT e@(f, t) g@(gft,gtf)
    | f == t     = return $ Left $ IS.singleton f
    | otherwise  = do
        present <- member e g
        if present 
         then (return $ Right [])
         else do
          rs <- readArray gtf f
          us <- readArray gft t
          pointsToF <- readArray gtf f
          pointsToT <- readArray gtf t
          tPointsTo <- readArray gft t
          let new2t = pointsToF IS.\\ pointsToT
          -- extras from f connects all new nodes pointing to f with t
          let extraF = IS.foldl' (\acc tf -> (tf,t) : acc) [] new2t
          -- extras of t connects all nodes that will be pointing to t
          -- in the new graph, with all the nodes t points to in the
          -- current graph
          all2tPointsTo <- newSTRef []
          forM_ (IS.toList tPointsTo) $ \ft -> do
            current  <- readSTRef all2tPointsTo
            existing <- readArray gtf ft
            let new4ft = map (flip (,) ft) $ IS.toList $ 
                            -- removing existing here matters a lot
                            (f `IS.insert` pointsToF) IS.\\ existing
            writeSTRef all2tPointsTo $ current ++ new4ft
                  
          extraT <- readSTRef all2tPointsTo
        -- the extras consists of extras from f and extras from t
        -- both these extra sets dont contain edges if they are already 
        -- present in the old graph
          let extra  = extraF ++ extraT
          mapM_ (`insErt` g) (e : extra) 
        -- the new graph contains a cycle if there is a self-edge
        -- this cycle will contain both f and t
          cyclic <- member (f,f) g
          if cyclic
           then do
            cycle <- getCycle gft
            return $ Left cycle
           else return $ Right extra
        
       where
        -- given that there is a cycle,all elements of this cycle are being
        -- pointed at by f. However, not all elements that f points to are 
        -- part of the cycle. Only those that point back to f.
        getCycle :: STArray s Vertex Vertices -> (ST s) Cycle
        getCycle gft = do
            ts <- readArray gft f
            mnodes <- forM (IS.toList ts) $ \t' -> do
                fs' <- readArray gft t'
                if f `IS.member` fs'
                 then return $ Just t'
                 else return $ Nothing
            return $ IS.fromList $ catMaybes mnodes

-- | Check if a certain edge is part of a graph which means that,
-- |  the receiving node must be in the node set of the sending
member :: Edge -> Graph s -> (ST s) Bool
member (f, t) (ft, tf) = do
    ts <- readArray ft f
    return $ IS.member t ts

-- | Check whether an edge is part of a frozen graph
fr_member :: FrGraph -> Edge -> Bool
fr_member (ft, tf) (f, t) = IS.member t (ft A.! f)

-- | Flatten a graph, meaning that we transform this graph to 
-- |  a set of Edges by combining a sending node with all the
-- |  receiving nodes in its node set
flatten :: Graph s -> (ST s) Edges 
flatten (gft, _) = do
    list <- getAssocs gft
    return $ S.fromList $ concatMap 
                (\(f, ts) -> map ((,) f) $ IS.toList ts) list

freeze_graph :: Graph s -> (ST s) FrGraph
freeze_graph (mf, mt) = do
    fr_f <- freeze mf
    fr_t <- freeze mt
    return (fr_f, fr_t)
