module Knuth1 where

import Pretty
import ExecutionPlan
import CommonTypes
import Control.Monad
import Control.Monad.ST
import Data.Maybe
import Data.List
import Data.STRef
import Debug.Trace

import qualified Data.Array as Array
import qualified Data.Map as Map
import qualified Data.Set as Set

-- | Trace a message in the ST monad
traceST :: String -> ST s ()
traceST s = trace s (return ())

-------------------------------------------------------------------------------
--         Dependency graph representation
-------------------------------------------------------------------------------

-- Vertices
data AttrType = Inh | Syn | Loc deriving (Eq, Ord, Show)
data Vertex = VAttr  AttrType Identifier Identifier
     	    | VChild Identifier
	    | VRule  Identifier deriving (Eq, Ord)

instance Show Vertex where
  show (VAttr ty ch at) = show ty ++ " @" ++ show ch ++ "." ++ show at
  show (VChild ch)      = "Child " ++ show ch
  show (VRule ru)       = "Rule " ++ show ru

-- | Check if a vertex is an attribute
isVertexAttr :: Vertex -> Bool
isVertexAttr (VAttr _ _ _) = True
isVertexAttr _        	   = False

-- | Get the child name of an attribute
getAttrChildName :: Vertex -> Identifier
getAttrChildName (VAttr _ n _) = n

-- | Set the child name of an attribute
setAttrChildName :: Vertex -> Identifier -> Vertex
setAttrChildName (VAttr t _ a) n = VAttr t n a

-- | Get the type of an attribute
getAttrType :: Vertex -> AttrType
getAttrType (VAttr t _ _) = t

-- | Get the name of an attribute
getAttrName :: Vertex -> Identifier
getAttrName (VAttr _ _ a) = a

-- Edges
type Edge = (Vertex, Vertex)

-- Internal representation of a vertex
type IVertex = Int
type IEdge = (IVertex, IVertex)

-- Representation of the graph
data DependencyGraph s = DependencyGraph { vertexIMap   :: Map.Map     Vertex  IVertex
                                         , vertexOMap   :: Array.Array IVertex Vertex 
                                         , successors   :: Array.Array IVertex (STRef s (Set.Set IVertex))
                                         , predecessors :: Array.Array IVertex (STRef s (Set.Set IVertex)) }

-------------------------------------------------------------------------------
--         Dependency graph fuctions
-------------------------------------------------------------------------------

-- | Construct a dependency graph
graphConstruct :: [Vertex] -> ST s (DependencyGraph s)
graphConstruct vs = do let nv    = length vs
                       let ivs   = [0..nv-1]
                       let ivb   = (0,nv-1)
                       let vimap = Map.fromList (zip vs ivs)
                       let vomap = Array.array ivb (zip ivs vs)
                       succs <- replicateM nv (newSTRef Set.empty)
                       preds <- replicateM nv (newSTRef Set.empty)
                       let su    = Array.array ivb (zip ivs succs)
                       let pr    = Array.array ivb (zip ivs preds)
                       let graph = DependencyGraph { vertexIMap   = vimap
                                                   , vertexOMap   = vomap
                                                   , successors   = su
                                                   , predecessors = pr }
                       return graph

-- | Construct a transitivelly closed graph
graphConstructTRC :: [Vertex] -> [Edge] -> ST s (DependencyGraph s)
graphConstructTRC vs es = do g <- graphConstruct vs
                             -- Insert all initial edges
                             graphInsertEdgesTRC g es
                             return g

-- | Return all successors of a vertex
graphSuccessors :: DependencyGraph s -> Vertex -> ST s (Set.Set Vertex)
graphSuccessors g v = do sucs <- readSTRef $ (successors g) Array.! (graphGetIVertex g v)
                         return $ Set.map (graphGetVertex g) sucs

-- | Return all predecessors of a vertex
graphPredecessors :: DependencyGraph s -> Vertex -> ST s (Set.Set Vertex)
graphPredecessors g v = do sucs <- readSTRef $ (predecessors g) Array.! (graphGetIVertex g v)
                           return $ Set.map (graphGetVertex g) sucs

-- | Check if the graph contains an edge
graphContainsEdge :: DependencyGraph s -> Edge -> ST s Bool
graphContainsEdge g (v1,v2) = do let iv1  = graphGetIVertex g v1
                                 let iv2  = graphGetIVertex g v2
                                 sucs <- readSTRef $ (successors g) Array.! iv1
                                 return $ iv2 `Set.member` sucs

-- | Insert an edge in the graph
graphInsert :: DependencyGraph s -> Edge -> ST s ()
graphInsert g (v1,v2) = do let iv1  = graphGetIVertex g v1
                           let iv2  = graphGetIVertex g v2
                           -- Add v2 to the successors of v1 and v1 to predecessors of v2
                           modifySTRef ((successors g) Array.! iv1) $ Set.insert iv2
                           modifySTRef ((predecessors g) Array.! iv2) $ Set.insert iv1

-- | Insert an edge in a transtive closed graph and return all other edges that were
--   added due to transtivity
graphInsertTRC :: DependencyGraph s -> Edge -> ST s [(IVertex, Set.Set IVertex)]
graphInsertTRC g (v1,v2) = do let iv1  = graphGetIVertex g v1
                              let iv2  = graphGetIVertex g v2
                              -- Read predecessors of v1 and successors of v2
                              pred1 <- readSTRef $ (predecessors g) Array.! iv1
                              succ2 <- readSTRef $ (successors g) Array.! iv2
                              -- First insert all edges from v1
                              let rsucc1 = (successors g) Array.! iv1
                              succ1 <- readSTRef rsucc1
                              let add1 = succ2 `Set.difference` succ1
                              modifySTRef rsucc1 (Set.union add1 . Set.insert iv2)
                              -- All edges to v2
                              let rpred2 = (predecessors g) Array.! iv2
                              modifySTRef rpred2 (Set.union pred1 . Set.insert iv1)
                              -- Connect every predecessor of v1 to every successor of v2
                              sucl <- forM (Set.toList pred1) $ \pred -> do
                                -- Connect pred to v2 and all successors of v2
                                let rsucc = (successors g) Array.! pred
                                csucc <- readSTRef rsucc
                                let cadd = (Set.insert iv2 succ2) `Set.difference` csucc
                                modifySTRef rsucc (Set.union cadd)
                                return (pred, cadd)
                              -- Connect every successor of v2 to every predecessor of v1
                              forM_ (Set.toList succ2) $ \succ -> do
                                -- Connect succ to v1 and all predecessors of v1
                                let rpred = (predecessors g) Array.! succ
                                cpred <- readSTRef rpred
                                let cadd = (Set.insert iv1 pred1) `Set.difference` cpred
                                modifySTRef rpred (Set.union cadd)
                              -- Create return
                              return $ (iv1,add1) : sucl

-- | Return all vertices of the graph
graphVertices :: DependencyGraph s -> ST s [Vertex]
graphVertices = return . Array.elems . vertexOMap

-- | Return all edges of the graph
graphEdges :: DependencyGraph s -> ST s [Edge]
graphEdges g = do let vs = Array.indices $ vertexOMap g
                  perv <- forM vs $ \v -> do
                    let rv = graphGetVertex g v
                    sucs <- readSTRef $ (successors g) Array.! v
                    let sucl = Set.toList sucs
                    return $ map ((,) rv . graphGetVertex g) sucl
                  return $ concat perv

-- | Insert a list of edges in the graph
graphInsertEdges :: DependencyGraph s -> [Edge] -> ST s ()
graphInsertEdges g ed = mapM_ (graphInsert g) ed

-- | Insert a list of edges in the graph and return all other edges that
--   were added due to transitivity
graphInsertEdgesTRC :: DependencyGraph s -> [Edge] -> ST s [Edge]
graphInsertEdgesTRC g ed = do -- rets :: [[(IVertex, Set.Set IVertex)]]
                              rets <- mapM (graphInsertTRC g) ed
                              -- Combine all successor sets
                              let f    :: (IVertex, (Set.Set IVertex)) -> [(IVertex, IVertex)]
                                  f (v,s) = map ((,) v) (Set.toList s)
                              let comb :: [(IVertex, IVertex)]
                                  comb = concatMap (concatMap f) rets
                              -- Construct edges from this
                              return $ map (graphGetEdge g) $ comb

-- | Check whether the graph is cyclic
graphIsCyclic :: DependencyGraph s -> ST s Bool
graphIsCyclic g = do vs <- return $ Array.indices $ vertexOMap g
                     selfcyc <- forM vs $ \v -> do
                       sucs <- readSTRef $ (successors g) Array.! v
                       return $ v `Set.member` sucs
                     return $ or selfcyc

-- | Get internal representation of a vertex
graphGetIVertex :: DependencyGraph s -> Vertex -> IVertex
graphGetIVertex g v = vertexIMap g Map.! v

-- | Get external representation of a vertex
graphGetVertex :: DependencyGraph s -> IVertex -> Vertex
graphGetVertex g v = vertexOMap g Array.! v

-- | Get external representation of an edge
graphGetEdge :: DependencyGraph s -> IEdge -> Edge
graphGetEdge g (v1,v2) = (graphGetVertex g v1, graphGetVertex g v2)

-- | Check if the graph is transitively closed
graphIsTRC :: DependencyGraph s -> ST s Bool
graphIsTRC g = do let vs = Array.indices $ vertexOMap g
                  bs <- forM vs $ \v -> do
                    succs <- readSTRef $ (successors g) Array.! v
                    bs2 <- forM (Set.toList succs) $ \v2 -> do
                      succs2 <- readSTRef $ (successors g) Array.! v2
                      return $ succs2 `Set.isSubsetOf` succs
                    return $ and bs2
                  return $ and bs

-- | Check consistency of the graph (successor and predecessor sets)
graphCheckConsistency :: DependencyGraph s -> ST s Bool
graphCheckConsistency g = do let vs = Array.indices $ vertexOMap g
                             ret <- forM vs $ \v -> do
                               -- V must appear in every predecessor set of its successors
                               succs <- readSTRef $ (successors g) Array.! v
                               r1 <- forM (Set.toList succs) $ \succ -> do
                                 preds2 <- readSTRef $ (predecessors g) Array.! succ
                                 return (v `Set.member` preds2)
                               -- V must appear in every successor set of its predecessors
                               preds <- readSTRef $ (predecessors g) Array.! v
                               r2 <- forM (Set.toList preds) $ \pred -> do
                                 succs2 <- readSTRef $ (successors g) Array.! pred
                                 return (v `Set.member` succs2)
                               return $ and $ r1 ++ r2
                             return $ and $ ret

-- | Add edges to the graph so that it is topologically sorted (this will not work if graph is cyclic)
graphTopSort :: DependencyGraph s -> ST s [Edge]
graphTopSort g = do let vs = Array.indices $ vertexOMap g
                    order <- foldM (graphTopSort' g) [] vs
                    mb <- forM (zip order (tail order)) $ \(v1,v2) -> do
                      let edg = graphGetEdge g (v2,v1) -- order is actually reverse order
                      ce <- graphContainsEdge g edg
                      if ce
                        then return Nothing                
                        else do graphInsert g edg
                                return $ Just edg
                    return $ catMaybes mb

-- | Helper function for graphTopSort
graphTopSort' :: DependencyGraph s -> [IVertex] -> IVertex -> ST s [IVertex]
graphTopSort' g prev cur | cur `elem` prev = return prev
                         | otherwise       = do pred <- readSTRef $ (predecessors g) Array.! cur
                                                order <- foldM  (graphTopSort' g) prev $ Set.toList pred
                                                return $ cur : order

-------------------------------------------------------------------------------
--         Dependency graph information wrappers
-------------------------------------------------------------------------------

-- | Special wrapper for nonterminal dependency graphs (so that we can easily add other meta-information)
data NontDependencyGraph = NontDependencyGraph { ndgVertices    :: [Vertex] 
                                               , ndgEdges       :: [Edge] }

-- | Special wrapper for production dependency graphs, including mapping between child names and nonterminals
data ProdDependencyGraph = ProdDependencyGraph { pdgVertices    :: [Vertex]
                                               , pdgEdges       :: [Edge]
                                               , pdgRules       :: ERules
                                               , pdgChilds      :: EChildren
                                               , pdgProduction  :: Identifier
                                               , pdgChildMap    :: [(Identifier, Identifier)] }


-- | Represent all information from the dependency graphs for a nonterminal
data NontDependencyInformation = NontDependencyInformation { ndiNonterminal :: Identifier
                                                           , ndiParams      :: [Identifier]
                                                           , ndiInh         :: [Identifier]
                                                           , ndiSyn         :: [Identifier]
                                                           , ndiDepGraph    :: NontDependencyGraph
                                                           , ndiProds       :: [ProdDependencyGraph] }

--- Monadic versions of these records, for use with the ST monad

-- | Monadic wrapper of NontDependencyGraph
data NontDependencyGraphM s = NontDependencyGraphM { ndgmDepGraph :: DependencyGraph s 
                                                   , ndgmOrig     :: NontDependencyGraph }

-- | Monadic wrapper of ProdDependencyGraph
data ProdDependencyGraphM s = ProdDependencyGraphM { pdgmDepGraph   :: DependencyGraph s
                                                   , pdgmOrig       :: ProdDependencyGraph }


-- | Monadic wrapper of NontDependencyInformation
data NontDependencyInformationM s = NontDependencyInformationM { ndimOrig        :: NontDependencyInformation
                                                               , ndimDepGraph    :: NontDependencyGraphM s
                                                               , ndimProds       :: [ProdDependencyGraphM s] }


-- | Convert a NontDependencyGraph to the corresponding monadic version
mkNontDependencyGraphM :: NontDependencyGraph -> ST s (NontDependencyGraphM s)
mkNontDependencyGraphM ndg = do g <- graphConstructTRC (ndgVertices ndg) (ndgEdges ndg)
                                return $ NontDependencyGraphM { ndgmDepGraph = g 
                                                              , ndgmOrig     = ndg }


-- | Convert a ProdDependencyGraph to the corresponding monadic version
mkProdDependencyGraphM :: Bool -> ProdDependencyGraph -> ST s (ProdDependencyGraphM s)
mkProdDependencyGraphM trc pdg = do g <- if trc
                                         then graphConstructTRC (pdgVertices pdg) (pdgEdges pdg)
                                         else do g <- graphConstruct (pdgVertices pdg)
                                                 mapM_ (graphInsert g) (pdgEdges pdg)
                                                 return g
                                    return $ ProdDependencyGraphM { pdgmDepGraph   = g
                                                                  , pdgmOrig       = pdg }

-- | Convert a NontDependencyInformation to the corresponding monadic version
mkNontDependencyInformationM :: NontDependencyInformation -> ST s (NontDependencyInformationM s)
mkNontDependencyInformationM ndi = do dg <- mkNontDependencyGraphM (ndiDepGraph ndi)
                                      prods <- mapM (mkProdDependencyGraphM True) (ndiProds ndi)
                                      return $ NontDependencyInformationM { ndimOrig     = ndi
                                                                          , ndimDepGraph = dg
                                                                          , ndimProds    = prods }

-- | Construct the production graphs from the transitivelly closed graphs
undoTransitiveClosure :: [NontDependencyInformationM s] -> ST s [NontDependencyInformationM s]
undoTransitiveClosure ndis = do edgesl <- mapM (\ndi -> graphEdges (ndgmDepGraph $ ndimDepGraph ndi)) ndis
                                let edges = concat edgesl
                                forM ndis $ \ndi -> do
                                  prods <- mapM (mkProdDependencyGraphM False) (ndiProds $ ndimOrig ndi)
                                  forM_ (zip prods (ndimProds ndi)) $ \(nprod,oprod) -> do
                                    -- All possible edges
                                    let possa = do (v1,v2) <- edges
                                                   -- Take a child of this nonterminal type
                                                   guard $ isVertexAttr v1
                                                   guard $ isVertexAttr v2
                                                   let tp = getAttrChildName v1
                                                   (ch,chtp) <- pdgChildMap $ pdgmOrig nprod
                                                   guard $ tp == chtp
                                                   -- Construct edge as it should be in the production graph
                                                   let nv1 = setAttrChildName v1 ch
                                                   let nv2 = setAttrChildName v2 ch
                                                   return (nv1, nv2)
                                    toadd <- filterM (graphContainsEdge (pdgmDepGraph oprod)) possa
                                    graphInsertEdges (pdgmDepGraph nprod) toadd
                                  return $ NontDependencyInformationM { ndimOrig     = ndimOrig ndi
                                                                      , ndimDepGraph = ndimDepGraph ndi
                                                                      , ndimProds    = prods }


-------------------------------------------------------------------------------
--         Knuth-1 algorithm
-------------------------------------------------------------------------------

-- | Combine the dependency and nonterminal graphs using Knuth-1
--   this function assumes that the nonterminal graphs initially contains no edges
knuth1 :: [NontDependencyInformationM s] -> ST s ()
knuth1 ndis = do -- Create initial list of pending edges for each ndi per production (initially all prod edges)
                 let ipending :: NontDependencyInformationM s -> ST s [[Edge]]
                     ipending = mapM (graphEdges . pdgmDepGraph) . ndimProds
--               pndis :: [([[Edge]], NontDependencyInformation)]
                 pndis <- mapM (\ndi -> liftM2 (,) (ipending ndi) (return ndi)) ndis
                 knuth1' pndis

-- | Helper function for |knuth1| which repeats the process until we are done
knuth1' :: [([[Edge]], NontDependencyInformationM s)] -> ST s ()
knuth1' ndis = do -- Add edges from the production graphs to the nonterminal graph
--                ndis' :: [Maybe [Edge]]
                  ndis' <- mapM addProdNont ndis
                  -- List of all newly added edges
--                ntedge :: [Edge]
                  let pntedge = concatMap (\x -> maybe [] id x) ndis'
                  -- Add backedges
                  bedges <- addBackEdges ndis
                  -- All added nonterminal edges
                  let ntedge = pntedge ++ bedges
                  if null ntedge
                    -- When no new edges have been added we are done
                    then return ()
                    else do -- Otherwise, the next step is to add edges from nonterminal to production graphs
--                          ndis'' :: [Maybe [[Edge]]]
                            ndis'' <- mapM (\(_,x) -> addNontProd True (ntedge, x)) ndis
                            -- List of new states (production edges + dependency graphs)
--                          nndis' :: [([[Edge]], NontDependencyInformation)]
                            nndis' <- zipWithM (\(_,ndi) me -> return (maybe [] id me, ndi)) ndis ndis''
                            if any isJust ndis''
                               -- We have added some edges, so continue the process
                              then knuth1' nndis'
                              -- No new edges added, we are done
                              else return ()

-- | Add pending edges from the production graphs to the nonterminal graph, return Nothing if none were added
--   otherwise, return the list of newly added nonterminal edges
addProdNont :: ([[Edge]], NontDependencyInformationM s) -> ST s (Maybe [Edge])
addProdNont (pending, ndi) = do -- Unwrapping of the records
                                let nontDepGraph = ndimDepGraph ndi
                                let nontGraph = ndgmDepGraph nontDepGraph
                                -- nub the list because multiple productions can result in the same new edges
                                let possa = nub $ do (v1,v2) <- concat pending
                                                     -- Take only edges from syn.lhs to inh.lhs
                                                     guard $ isVertexAttr v1
                                                     guard $ getAttrChildName v1 == _LHS
                                                     guard $ getAttrType      v1 ==  Syn
                                                     guard $ isVertexAttr v2
                                                     guard $ getAttrChildName v2 == _LHS
                                                     guard $ getAttrType      v2 ==  Inh
                                                     -- Construct edge as it should be in nonterminal graph
                                                     let nv1 = setAttrChildName v1 (ndiNonterminal $ ndimOrig ndi)
                                                     let nv2 = setAttrChildName v2 (ndiNonterminal $ ndimOrig ndi)
                                                     return (nv1, nv2)
                                -- Edges that are not in the nonterminal graph yet
                                toadd <- filterM (\e -> return not `ap` graphContainsEdge nontGraph e) possa
                                -- Check whether new edges are to be added and return the added edges
                                if null toadd
                                   then return Nothing
                                   else do graphInsertEdgesTRC nontGraph toadd
                                           -- Debug output
                                           --mapM_ (\edge -> traceST $ "Adding nonterminal edge " ++ show edge) toadd
                                           return $ Just toadd

-- | Add edges from the nonterminal graphs to the production graphs, return Nothing if none were added
--   otherwise, return the list of newly added production edges and the updated graph
addNontProd :: Bool -> ([Edge], NontDependencyInformationM s) -> ST s (Maybe [[Edge]])
addNontProd trc (pending, ndi) = do -- Call the helper function for each nonterminal
                                    prods' <- mapM (addNontProd' trc pending) (ndimProds ndi)
                                    -- Check if any edges were added
                                    if any isJust prods'
                                      then -- Return list of newly created edges
                                           return $ Just $ map (maybe [] id) prods'
                                      else return Nothing

-- | Helper function for |addNontProd| for a single production
addNontProd' :: Bool -> [Edge] -> ProdDependencyGraphM s -> ST s (Maybe [Edge])
addNontProd' trc pend pdg = do -- Unwrapping of the records
                               prodGraph <- return $ pdgmDepGraph pdg
                               -- Construct all possible new edges
                               let possa = do (v1,v2) <- pend
                                              -- Take a child of this nonterminal type
                                              guard $ isVertexAttr v1
                                              guard $ isVertexAttr v2
                                              let tp = getAttrChildName v1
                                              (ch,chtp) <- pdgChildMap $ pdgmOrig pdg
                                              guard $ tp == chtp
                                              -- Construct edge as it should be in the production graph
                                              let nv1 = setAttrChildName v1 ch
                                              let nv2 = setAttrChildName v2 ch
                                              return (nv1, nv2)
                               -- Edges that are not in the production graph yet
                               toadd <- filterM (\e -> return not `ap` graphContainsEdge prodGraph e) possa
                               -- Check whether new edges are to be added and return the result
                               if null toadd
                                 then return Nothing
                                 else do -- Insert all edges and return transitive edges that are added in this process
                                         ret <- if trc
                                                then graphInsertEdgesTRC prodGraph toadd
                                                else do mapM_ (graphInsert prodGraph) toadd
                                                        return []
                                         -- Debug output
                                         --mapM_ (\edge -> traceST $ "Adding production edge " ++ show edge) toadd
                                         return $ Just ret

-- | Add the "back edges" to the nonterminal graphs for creating a global ordering
addBackEdges :: [([[Edge]], NontDependencyInformationM s)] -> ST s [Edge]
addBackEdges ndis = do -- gather all backedges
                       lBackEdges <- forM ndis $ \(aedg,ndi) -> do
                         -- For every production
                         bs <- forM (zip aedg (ndimProds ndi)) $ \(edg,prod) -> do
                           -- Filter out the backedges
                           return $ do (v1,v2) <- edg
                                       -- Backedges are from inh.ch to syn.ch
                                       guard $ isVertexAttr v1
                                       guard $ getAttrChildName v1 /= _LHS
                                       guard $ getAttrType      v1 ==  Inh
                                       guard $ isVertexAttr v2
                                       guard $ getAttrChildName v2 /= _LHS
                                       guard $ getAttrType      v2 ==  Syn
                                       guard $ getAttrChildName v1 == getAttrChildName v2
                                       -- Find the correct child name
                                       (ch,chtp) <- pdgChildMap $ pdgmOrig prod
                                       let tp = getAttrChildName v1
                                       guard $ tp == ch
                                       -- Construct the edge as it should be in the nonterminal graph
                                       let nv1 = setAttrChildName v1 chtp
                                       let nv2 = setAttrChildName v2 chtp
                                       return (nv1, nv2)
                         return $ foldl' union [] bs
                       -- Concatenate all lists of backedges
                       let backedges = foldl' union [] lBackEdges
                       -- Add backedges to every nonterminal graph
                       ret <- forM ndis $ \(_,ndi) -> do
                         -- Find the backedges for this nonterminal
                         let nont = ndiNonterminal . ndimOrig $ ndi
                         let thisbe = filter ((==) nont . getAttrChildName . fst) backedges
                         -- Add them to the graph
                         graphInsertEdgesTRC (ndgmDepGraph . ndimDepGraph $ ndi) thisbe
                       return $ backedges ++ concat ret


-- | Add all resulting edges from a topsort on the nonterminal graph to the production graph
--   this will ignore edges that will make the graph cyclic
addTopSortEdges :: [Edge] -> ProdDependencyGraphM s -> ST s ()
addTopSortEdges pend pdg = do -- Unwrapping of the records
                              prodGraph <- return $ pdgmDepGraph pdg
                              -- Construct all possible new edges
                              let possa = do (v1,v2) <- pend
                                             -- Take a child of this nonterminal type
                                             guard $ isVertexAttr v1
                                             guard $ isVertexAttr v2
                                             let tp = getAttrChildName v1
                                             (ch,chtp) <- pdgChildMap $ pdgmOrig pdg
                                             guard $ tp == chtp
                                             -- Construct edge as it should be in the production graph
                                             let nv1 = setAttrChildName v1 ch
                                             let nv2 = setAttrChildName v2 ch
                                             return (nv1, nv2)
                              -- Edges that are not in the production graph yet
                              forM_ possa $ \(v1,v2) -> do e1 <- graphContainsEdge prodGraph (v1,v2)
                                                           e2 <- graphContainsEdge prodGraph (v2,v1)
                                                           when (not $ e1 || e2) $ do
                                                             graphInsertTRC prodGraph (v1,v2)
                                                             return ()
