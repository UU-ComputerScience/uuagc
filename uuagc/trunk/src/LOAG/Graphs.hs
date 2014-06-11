module LOAG.Graphs where

import Control.Monad.Trans (lift, MonadTrans(..))
import Control.Monad (forM, forM_)
import Control.Monad.ST
import Control.Monad.State
import CommonTypes
import Data.STRef
import Data.Maybe (catMaybes, isNothing, fromJust)
import Data.Tuple (swap)
import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Map as M
import qualified Data.Array as A
import           Data.Array.IArray (amap)
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

addEDs :: (MonadTrans m, MonadState s (m (ST s))) => Graph s -> 
            [Edge] -> (m (ST s)) (Maybe (Edge, Cycle))
addEDs _ [] = return Nothing
addEDs edp (e:es) = do
    res <- e `inserT` edp
    case res of
        Right _ -> addEDs edp es
        Left c  -> return $ Just (e,c)
       
-- | Draws an edge from one node to another, by adding the latter to the
--    node set of the first
insErt :: (MonadTrans m, MonadState s (m (ST s))) => Edge -> Graph s -> 
            (m (ST s)) ()
insErt (f, t) g@(ft,tf) = do 
    ts <- lift (readArray ft f)
    fs <- lift (readArray tf t)
    lift (writeArray ft f (t `IS.insert` ts))
    lift (writeArray tf t (f `IS.insert` fs))

removE :: (MonadTrans m, MonadState s (m (ST s))) => Edge -> Graph s -> 
            (m (ST s)) ()
removE e@(f,t) g@(ft,tf) = do 
    ts <- lift (readArray ft f)
    fs <- lift (readArray tf t)
    lift (writeArray ft f (t `IS.delete` ts))
    lift (writeArray tf t (f `IS.delete` fs))


-- | Revert an edge in the graph
revErt :: (MonadTrans m, MonadState s (m (ST s))) => Edge -> Graph s -> 
            (m (ST s)) ()
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
inserT :: (MonadTrans m, MonadState s (m (ST s))) => Edge -> Graph s -> 
            (m (ST s)) (Either Cycle [Edge])
inserT e@(f, t) g@(gft,gtf)
    | f == t     = return $ Left $ IS.singleton f
    | otherwise  = do
        present <- member e g
        if present 
         then (return $ Right [])
         else do
          pointsToF <- lift (readArray gtf f)
          pointsToT <- lift (readArray gtf t)
          tPointsTo <- lift (readArray gft t)
          let new2t = pointsToF IS.\\ pointsToT
          -- extras from f connects all new nodes pointing to f with t
          let extraF = IS.foldl' (\acc tf -> (tf,t) : acc) [] new2t
          -- extras of t connects all nodes that will be pointing to t
          -- in the new graph, with all the nodes t points to in the
          -- current graph
          all2tPointsTo <- lift (newSTRef [])
          forM_ (IS.toList tPointsTo) $ \ft -> do
            current  <- lift (readSTRef all2tPointsTo)
            existing <- lift (readArray gtf ft)
            let new4ft = map (flip (,) ft) $ IS.toList $ 
                            (f `IS.insert` pointsToF) IS.\\ existing
            lift (writeSTRef all2tPointsTo $ current ++ new4ft)
                  
          extraT <- lift (readSTRef all2tPointsTo)            
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
        getCycle :: (MonadTrans m, MonadState s (m (ST s))) => 
                        STArray s Vertex Vertices -> (m (ST s)) Cycle
        getCycle gft = do
            ts <- lift (readArray gft f)
            mnodes <- forM (IS.toList ts) $ \t' -> do
                fs' <- lift (readArray gft t')
                if f `IS.member` fs'
                 then return $ Just t'
                 else return $ Nothing
            return $ IS.fromList $ catMaybes mnodes

-- | Check if a certain edge is part of a graph which means that,
-- |  the receiving node must be in the node set of the sending
member :: (MonadTrans m, MonadState s (m (ST s))) => Edge -> Graph s -> 
            (m (ST s)) Bool
member (f, t) (ft, tf) = do
    ts <- lift (readArray ft f)
    return $ IS.member t ts

-- | Check whether an edge is part of a frozen graph
fr_member :: FrGraph -> Edge -> Bool
fr_member (ft, tf) (f, t) = IS.member t (ft A.! f)

-- | Flatten a graph, meaning that we transform this graph to 
-- |  a set of Edges by combining a sending node with all the
-- |  receiving nodes in its node set
flatten :: (MonadTrans m, MonadState s (m (ST s))) => Graph s -> (m (ST s)) Edges 
flatten (gft, _) = do
    list <- lift (getAssocs gft)
    return $ S.fromList $ concatMap 
                (\(f, ts) -> map ((,) f) $ IS.toList ts) list

freeze_graph :: (MonadTrans m, MonadState s (m (ST s))) => 
                    Graph s -> (m (ST s)) FrGraph
freeze_graph (mf, mt) = do
    fr_f <- lift (freeze mf)
    fr_t <- lift (freeze mt)
    return (fr_f, fr_t)

mapToArray :: (Int, Int) -> IM.IntMap Vertices -> A.Array Vertex [Vertex] 
mapToArray bounds tdp = A.accumArray (++) [] bounds flatg
 where  flatg = foldr split [] $ IM.assocs tdp
        split (f,ts) xs = (map ((,) f . (:[])) (IS.toList ts)) ++ xs

--maxSCCs = catMaybes . map cToList . SCC.sccList 
-- where cToList  (G.AcyclicSCC _) = Nothing
--       cToList  (G.CyclicSCC vs) = Just vs

findCycles :: (MonadTrans m, MonadState s (m (ST s))) => 
                       Graph s -> Vertex -> (m (ST s)) [[Vertex]]
findCycles (gf,gt) v = do
    f_gf <- lift (freeze gf)
    f_gt <- lift (freeze gt)
    let findPath :: (M.Map Vertex Int) -> Vertex -> [[Vertex]]
        findPath rMap v = 
            case M.lookup v rMap of
                Nothing     -> otherres
                Just r      -> [returnPath r rMap v [v]]
         where  
                otherres    = concatMap (findPath rMap') explore
                explore     = IS.toList $ f_gf A.! v
                rMap'       = M.insert v idx rMap
                idx         = M.size rMap
        returnPath :: Int -> M.Map Vertex Int -> Vertex -> [Vertex] -> [Vertex]
        returnPath r rMap v acc = 
                case filter select explore of -- assumes exactly 1 path back
                    []  -> acc 
                    [x] -> returnPath r rMap x (x:acc)
         where  select :: Vertex -> Bool
                select x    = maybe False validate $ M.lookup x rMap
                 where validate :: Int -> Bool
                       validate r' = r' >= r
                explore     = IS.toList $ f_gt A.! v
    return $ findPath M.empty v

cyclicPaths :: (MonadTrans m, MonadState s (m (ST s))) => 
                       Graph s -> Vertex -> (m (ST s)) [[Vertex]]
cyclicPaths (gf,_) v = do
    f_gf <- lift (freeze gf)
    let paths :: [Vertex] -> IS.IntSet -> Vertex -> [[Vertex]]
        paths acc accS v 
            | v `IS.member` accS = (takeWhile (/= v) acc) : (proceed nexts)
            | otherwise = proceed succs
         where  succs = IS.toList $ f_gf A.! v
                proceed = concatMap (paths (v:acc) (v `IS.insert` accS))
                nexts = IS.toList $ f_gf A.! v IS.\\ accS
    return $ paths [] IS.empty v

contCycles :: (MonadTrans m, MonadState s (m (ST s))) => 
                       Graph s -> Vertex -> (m (ST s)) [[Vertex]]
contCycles (gf,_) v = do
    f_gf <- lift (freeze gf)
    let findPath :: (Vertex -> [Vertex], IS.IntSet) -> Vertex -> [[Vertex]]
        findPath (f,s) v = 
            if IS.member v s
                then [f v]
                else otherres
         where  
                otherres    = concatMap (findPath (f', v`IS.insert` s)) explore
                explore     = IS.toList $ f_gf A.! v
                f' x        | x == v    = [v]
                            | otherwise = v : (f x)
    return $ findPath (const [], IS.empty) v
