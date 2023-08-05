module KennedyWarren where

import Prelude hiding (init, succ)
import CommonTypes
import Pretty
import Knuth1
import ExecutionPlan
import Debug.Trace
import Control.Monad.ST
import Control.Monad.State
import Control.Monad.Except (ExceptT, runExceptT, MonadError(..))
import Control.Monad (guard, liftM, when, forM_, foldM, forM)
import Data.STRef
import Data.Maybe
import Data.List (intersperse, groupBy, partition, sortBy)
import Data.Ord
import qualified ErrorMessages as Err
import PrintErrorMessages ()

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Map.Strict as MapStrict
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

-- lazy version (does not return errors)
-- FIXME: construct map from nonterminal to intial visit (or state?) and use it in the generation of invokes
kennedyWarrenLazy :: Options -> Set NontermIdent -> [NontDependencyInformation] -> TypeSyns -> Derivings -> ExecutionPlan
kennedyWarrenLazy _ wr ndis typesyns derivings = plan where
  plan  = ExecutionPlan nonts typesyns wr derivings
  nonts = zipWith mkNont ndis nontIds
  nontIds = enumFromThen 1 4
  initvMap = Map.fromList $ zipWith (\ndi initv -> (ndiNonterminal ndi, initv)) ndis nontIds

  mkNont ndi initv = nont where
    nont = ENonterminal
                 (ndiNonterminal ndi)
                 (ndiParams ndi)
                 (ndiClassCtxs ndi)
                 initst
                 [initv]
                 nextMap
                 prevMap
                 prods
                 (ndiRecursive ndi)
                 (ndiHoInfo ndi)
    initst  = initv + 1
    finals  = initv + 2
    nextMap = Map.fromList [(initst, OneVis initv), (finals, NoneVis)]
    prevMap = Map.fromList [(initst, NoneVis), (finals, OneVis initv)]
    prods   = map mkProd (ndiProds ndi)

    mkProd pdi = prod where
      prod = EProduction
               (pdgProduction pdi)
               (pdgParams pdi)
               (pdgConstraints pdi)
               (pdgRules pdi)
               (pdgChilds pdi)
               visits
      visits = [vis]
      vis    = Visit initv initst finals inh syn steps kind
      inh    = Set.fromList $ ndiInh ndi
      syn    = Set.fromList $ ndiSyn ndi
      kind   = VisitPure False
      steps  = childSteps ++ invokeSteps ++ ruleSteps

      childSteps  = [ ChildIntro nm | EChild nm _ _ _ _ _ <- pdgChilds pdi ]
      invokeSteps = [ ChildVisit nm nt v
                    | EChild nm tp _ _ _ _ <- pdgChilds pdi
                    , let nt = extractNonterminal tp
                          v  = Map.findWithDefault (error "child not in initv-map") nt initvMap
                    ]
      ruleSteps   = [ Sem nm | (ERule nm _ _ _ _ _ _ _) <- pdgRules pdi ]


-- ordered version (may return errors)
kennedyWarrenOrder :: Options -> Set NontermIdent -> [NontDependencyInformation] -> TypeSyns -> Derivings -> Either Err.Error (ExecutionPlan, PP_Doc, PP_Doc)
kennedyWarrenOrder opts wr ndis typesyns derivings = runST $ runExceptT $ do
  indi <- lift $ mapM mkNontDependencyInformationM ndis
  lift $ knuth1 indi
  -- Check all graphs for cyclicity, transitive closure and consistency
  -- traceST $ "Checking graphs..."
  forM_ indi $ \ndi -> do
    let nont = ndiNonterminal . ndimOrig $ ndi
    let g = ndgmDepGraph . ndimDepGraph $ ndi
    -- Topological sort
    --tsedg <- graphTopSort g
    -- Cyclicity check
    ntCycVerts <- lift $ graphCyclicVerticesExt g
    when (not $ null ntCycVerts) $ do
      throwError $ Err.Cyclic nont Nothing (map show ntCycVerts)
--      let msg = "Nonterminal graph " ++ show nont ++ " is cylic!"
--      fail msg
    -- Transtive closure check
    trc <- lift $ graphIsTRC g
    when (not trc) $ do
      let msg = "Nonterminal graph " ++ show nont ++ " is not transitively closed!"
      errorWithoutStackTrace msg
    -- Consistency check
    cons <- lift $ graphCheckConsistency g
    when (not cons) $ do
      let msg = "Nonterminal graph " ++ show nont ++ " is not consistent!"
      errorWithoutStackTrace msg

    -- Loop trough all productions
    forM_ (ndimProds ndi) $ \prod -> do
      let pr = pdgProduction $ pdgmOrig prod
      let g' = pdgmDepGraph $ prod
      -- Topsort
      --addTopSortEdges tsedg prod
      -- Check for cyclicity
      pdCycVerts <- lift $ graphCyclicVerticesExt g'
      when (not $ null pdCycVerts) $ do
        throwError $ Err.Cyclic nont (Just pr) (map show pdCycVerts)
        -- let msg = "Production graph " ++ show pr ++ " of nonterminal "
        --                               ++ show nont ++ " is cylic!"
        -- fail msg
      -- Transtive closure check
      trc' <- lift $ graphIsTRC g'
      when (not trc') $ do
        lift $ traceST $ "Production graph " ++ show pr ++ " of nonterminal "
                                             ++ show nont ++ " is not transitively closed!"
        errorWithoutStackTrace "Production graph is not transitively closed."
      -- Check consistency
      consistent <- lift $ graphCheckConsistency g'
      when (not consistent) $ do
        let msg =  "Production graph " ++ show pr ++ " of nonterminal "
                                       ++ show nont ++ " is not consistent!"
        errorWithoutStackTrace msg
  -- reachable when everything is ok
  lift $ do
        -- Create non-transitive closed graph for efficiency
        indi' <- undoTransitiveClosure indi
        -- Graphviz output of dependency graphs
        gvs <- mapM toGVNontDependencyInfo indi'
        -- Doing kennedywarren
        (ret, visitg) <- runVG $ do
         -- traceVG $ "Running kennedy-warren..."
         initvs <- kennedyWarrenVisitM wr indi'
         -- Print some debug info
         nodes <- gets vgNodeNum
         edges <- gets vgEdgeNum
         when (not $ beQuiet opts) $ do
           traceVG $ "Number of nodes = " ++ show nodes
           traceVG $ "Number of edges = " ++ show edges
         -- Generate execution plan
         ex <- kennedyWarrenExecutionPlan opts indi' initvs wr typesyns derivings
         -- Get visit graph
         visitg <- toGVVisitGraph
         return (ex,visitg)
        -- Return the result
        return (ret, vlist gvs, visitg)

-------------------------------------------------------------------------------
--         Debugging functionality
-------------------------------------------------------------------------------

-- | Pretty print a vertex in GraphViz format
toGVVertex :: Bool -> Vertex -> ST s PP_Doc
toGVVertex l (VAttr t a b) = return $ (text $ "attr_" ++ show t ++ "_" ++ show a ++ "_" ++ show b) >#< if l
                        then text ("[shape=box,label=\"" ++ show t ++ " @" ++ show a ++ "." ++ show b ++ "\"]") else empty
toGVVertex l (VChild c)    = return $ (text $ "child_" ++ show c) >#< if l
                        then text ("[shape=ellipse,label=\"Child " ++ show c ++ "\"]") else empty
toGVVertex l (VRule r)   = return $ (text $ "rule_"  ++ show r) >#< if l
                        then text ("[shape=diamond,label=\"" ++ show r ++ "\"]") else empty

-- | Pretty print an edge in GraphViz format
toGVEdge :: Edge -> ST s PP_Doc
toGVEdge (v1, v2) = do r1 <- toGVVertex False v1
                       r2 <- toGVVertex False v2
                       return $ r1 >|< text "->" >#< r2

-- | Pretty print a NontDependencyInformation in GraphViz format
toGVNontDependencyInfo :: NontDependencyInformationM s -> ST s PP_Doc
toGVNontDependencyInfo ndi = do dg <- return $ ndgmDepGraph . ndimDepGraph $ ndi
                                verts <- graphVertices dg
                                edges <- graphEdges dg
                                vtexts <- mapM (toGVVertex True) verts
                                etexts <- mapM toGVEdge edges
                                ptexts <- mapM toGVProdDependencyGraph (ndimProds ndi)
                                return $ (text ("digraph ndg_" ++ show (ndiNonterminal $ ndimOrig ndi) ++ " {")
                                          >-<
                                          vlist vtexts
                                          >-<
                                          vlist etexts
                                          >-<
                                          text "}"
                                          >-<
                                          text "" -- empty line
                                          >-<
                                          vlist ptexts)

-- | Pretty print a ProdDependencyGraph in GraphViz format
toGVProdDependencyGraph :: ProdDependencyGraphM s -> ST s PP_Doc
toGVProdDependencyGraph pdg = do dg <- return $ pdgmDepGraph pdg
                                 verts <- graphVertices dg
                                 edges <- graphEdges dg
                                 vtexts <- mapM (toGVVertex True) verts
                                 etexts <- mapM toGVEdge edges
                                 return $ (text ("digraph pdg_" ++ show (pdgProduction $ pdgmOrig pdg) ++ " {")
                                           >-<
                                           (vlist vtexts)
                                           >-<
                                           (vlist etexts)
                                           >-<
                                           text ("info [shape=box,label=\"" ++ show (pdgChildMap $ pdgmOrig pdg) ++ "\"];")
                                           >-<
                                           text "}"
                                           >-<
                                           text "")

toGVVisitGraph :: VG s PP_Doc
toGVVisitGraph = do
  ndis <- gets vgNDI
  noded <- forM (IntMap.toList ndis) $ \(n,rndi) -> do
    ndi <- vgInST $ readSTRef rndi
    return $ "node_" >|< n >#< "[label=\"" >|< ndiNonterminal (ndimOrig ndi) >|< "_" >|< n >|< "\"];"
  edges <- gets vgEdges
  edged <- forM (IntMap.toList edges) $ \(edg,(VGNode from,VGNode to)) -> do
    inh <- getInherited (VGEdge edg)
    syn <- getSynthesized (VGEdge edg)
    return $ "node_" >|< from >#< "-> node_" >|< to >#< "[label=\"visit v" >|< edg
      >|< "\\ninh:" >#< (concat $ intersperse ", " $ map show $ Set.toList inh) >|< "\\nsyn: " >|< (concat $ intersperse ", " $ map show $ Set.toList syn) >|< "\"];"
  return $ "digraph visitgraph { " >-< vlist noded >-< vlist edged >-< "}"

-------------------------------------------------------------------------------
--         Kennedy-Warren in monadic style
-------------------------------------------------------------------------------
{-
runVG                    :: VG s a -> ST s a
insertInitialNode        :: NontDependencyInformationM s -> VG s VGNode
createPending            :: VGNode -> Set Identifier -> Set Identifier -> VG s VGEdge
selectPending            :: VG s VGEdge
getInherited             :: VGEdge -> VG s (Set Identifier)
getSynthesized           :: VGEdge -> VG s (Set Identifier)
markFinal                :: VGEdge -> VG s ()
getProductions           :: VGEdge -> VG s [VGProd]
onMarkedDepGraph         :: (ProdDependencyGraphM s -> ST s a) -> VGProd -> VG s a
isDepGraphVertexFinal    :: VGProd -> Vertex -> VG s Bool
setDepGraphVerticesFinal :: VGProd -> [Vertex] -> VG s ()
getChildState            :: VGProd -> Identifier -> VG s VGNode
addChildVisit            :: VGProd -> Identifier -> VGEdge -> VG s VisitStep
addVisitStep             :: VGProd -> VisitStep -> VG s ()
repeatM                  :: VG s () -> VG s ()
-}

newtype VGNode = VGNode Int deriving (Show,Eq,Ord)
newtype VGEdge = VGEdge Int deriving (Show,Eq,Ord)
newtype VGProd = VGProd (VGEdge,Int) deriving (Show,Eq,Ord)

data VGState s = VGState { vgNodeNum       :: Int
                         , vgEdgeNum       :: Int
                           -- Node maps
                         , vgOutgoing      :: IntMap (STRef s (Set VGEdge))
                         , vgIncoming      :: IntMap (Maybe VGEdge)
                         , vgNDI           :: IntMap (STRef s (NontDependencyInformationM s))
                         , vgInhSynNode    :: Map (Identifier, Set Identifier, Set Identifier) VGNode
                         , vgNodeInhSyn    :: IntMap (Set Identifier, Set Identifier)
                         , vgInitial       :: Map Identifier VGNode
                           -- Edge maps
                         , vgEdges         :: IntMap (VGNode, VGNode)
                         , vgEdgesR        :: Map (VGNode,VGNode) VGEdge
                         , vgInherited     :: IntMap (Set Identifier)
                         , vgSynthesized   :: IntMap (Set Identifier)
                         , vgPending       :: IntSet
                         , vgChildVisits   :: IntMap (STRef s (Map (Identifier,Int) [VGNode]))
                           -- Final vertices in production graphs
                         , vgFinalVertices :: IntMap (STRef s (Set (Vertex,Int)))
                           -- Construction of execution plan (Nonterminal,Production,Visit)
                         , vgProdVisits    :: Map (Identifier,Identifier,VGEdge) (STRef s [VisitStep])
                         }

type VG s a = ExceptT String (StateT (VGState s) (ST s)) a

------------------------------------------------------------
---              Public functions                        ---
------------------------------------------------------------
-- | Run the VG monad in the ST monad
runVG :: VG s a -> ST s a
runVG vg = do result <- runStateT (runExceptT vg) vgEmptyState
              let (Right a,_) = result
              return a

-- | Insert an initial node for this nonterminal into the visit graph
insertInitialNode :: NontDependencyInformationM s -> VG s VGNode
insertInitialNode ndi = do
  rndi          <- vgInST $ newSTRef ndi
  (VGNode node) <- vgCreateNode rndi Set.empty Set.empty
  initial       <- gets vgInitial
  incoming      <- gets vgIncoming
  modify $ \st -> st { vgInitial  = Map.insert (ndiNonterminal $ ndimOrig ndi) (VGNode node) initial
                     , vgIncoming = IntMap.insert node Nothing incoming }
  return (VGNode node)

-- | Create a pending edge from this node with a set of inherited and synthesized attributes
createPending :: VGNode -> Set Identifier -> Set Identifier -> VG s VGEdge
createPending vgn@(VGNode n) inh syn = do
  -- Check if target node already exists
  ninhsyn <- gets vgNodeInhSyn
  let (pinh,psyn) = imLookup n ninhsyn
  let ninh        = Set.union pinh inh
  let nsyn        = Set.union psyn syn
  mndi    <- gets vgNDI
  let rndi = imLookup n mndi
  ndi     <- vgInST $ readSTRef rndi
  inhsynn <- gets vgInhSynNode
  case Map.lookup (ndiNonterminal $ ndimOrig ndi, ninh, nsyn) inhsynn of
    Just tn -> do
      when (tn == vgn) $ do traceVG $ "Source and target nodes are the same!"
                            traceVG $ "Maybe there is a wrapper with no inherited or synthesized attributes."
                            traceVG $ "Inh: " ++ show inh
                            traceVG $ "Syn: " ++ show syn
                            traceVG $ "PInh: " ++ show pinh
                            traceVG $ "PSyn: " ++ show psyn
      -- tn is target node, now check if edge exists and create if not
      edgesr <- gets vgEdgesR
      case Map.lookup (vgn,tn) edgesr of
        Just e  -> return e
        Nothing -> vgCreatePendingEdge vgn tn inh syn
    Nothing -> do
      -- target node does not exist, create it and then create the new edge
      tn <- vgCreateNode rndi ninh nsyn
      vgCreatePendingEdge vgn tn inh syn

-- | Return an arbitrary pending edge of which the from node is ready
selectPending :: VG s VGEdge
selectPending = do
  pending  <- gets vgPending
  incoming <- gets vgIncoming
  edges    <- gets vgEdges
  let readyPend = filter (\p -> let (VGNode fr,_) = imLookup p edges
                                in  isJust $ IntMap.lookup fr incoming) $ IntSet.toList pending
  guard $ not $ null readyPend
  return $ VGEdge $ head $ readyPend

-- | Get the inherited attributes of an edge
getInherited :: VGEdge -> VG s (Set Identifier)
getInherited (VGEdge edg) = do
  inhs <- gets vgInherited
  return $ imLookup edg inhs

-- | Get the synthesized attributes of an edge
getSynthesized :: VGEdge -> VG s (Set Identifier)
getSynthesized (VGEdge edg) = do
  syns <- gets vgSynthesized
  return $ imLookup edg syns

-- | Mark an edge as final
markFinal :: VGEdge -> VG s ()
markFinal vgedg@(VGEdge edg) = do
  incoming <- gets vgIncoming
  edges    <- gets vgEdges
  pending  <- gets vgPending
  let (_,VGNode to) = imLookup edg edges
  modify $ \st -> st { vgIncoming = IntMap.insert to (Just vgedg) incoming
                     , vgPending  = IntSet.delete edg pending }

-- | Get all productions for an edge
getProductions :: VGEdge -> VG s [VGProd]
getProductions vedg@(VGEdge edg) = do
  edges <- gets vgEdges
  let (VGNode fr,_) = imLookup edg edges
  ndis <- gets vgNDI
  let rndi = imLookup fr ndis
  ndi <- vgInST $ readSTRef rndi
  return $ map (\x -> VGProd (vedg,x)) [0..(length $ ndimProds ndi)-1]

-- | Execute a function on the dependency graph for this production
onMarkedDepGraph :: (ProdDependencyGraphM s -> ST s a) -> VGProd -> VG s a
onMarkedDepGraph f (VGProd (VGEdge edg, n)) = do
  edges <- gets vgEdges
  let (VGNode fr,_) = imLookup edg edges
  ndis <- gets vgNDI
  let rndi = imLookup fr ndis
  ndi <- vgInST $ readSTRef rndi
  vgInST $ f $ (ndimProds ndi) !! n -- not efficient, but lists are usually short

-- | Check whether this vertex has been marked as final
isDepGraphVertexFinal :: VGProd -> Vertex -> VG s Bool
isDepGraphVertexFinal (VGProd (VGEdge edg, p)) v = do
  edges <- gets vgEdges
  let (from,_) = imLookup edg edges
  vgDepGraphVertexFinal from p v

-- | Mark these vertices final in this production
setDepGraphVerticesFinal :: VGProd -> [Vertex] -> VG s ()
setDepGraphVerticesFinal (VGProd (VGEdge edg, p)) vs = do
  edges   <- gets vgEdges
  let (_,VGNode to) = imLookup edg edges
  finalv <- gets vgFinalVertices
  let rfinalv       = imLookup to finalv
  vgInST $ modifySTRef rfinalv $ Set.union (Set.fromList $ map (\v -> (v,p)) vs)

-- | Add a child visit to this production and return the step for the execution plan
addChildVisit :: VGProd -> Identifier -> VGEdge -> VG s VisitStep
addChildVisit (VGProd (VGEdge edg, p)) ide (VGEdge vs) = do
  edges   <- gets vgEdges
  let (VGNode from,vgto) = imLookup vs edges -- from must be equal to the current state
  childvs <- gets vgChildVisits
  let rchildv = imLookup edg childvs
  vgInST $ modifySTRef rchildv $ MapStrict.insertWith (++) (ide,p) [vgto]
  ndis <- gets vgNDI
  let rndi = imLookup from ndis
  ndi <- vgInST $ readSTRef rndi
  let nt = ndiNonterminal $ ndimOrig ndi
  return $ ChildVisit ide nt vs

-- | Add a step to the execution plan of this visit
addVisitStep :: VGProd -> VisitStep -> VG s ()
addVisitStep (VGProd (VGEdge edg, p)) st = do
  edges <- gets vgEdges
  let (VGNode fr,_) = imLookup edg edges
  ndis <- gets vgNDI
  let rndi = imLookup fr ndis
  ndi <- vgInST $ readSTRef rndi
  prodvs <- gets vgProdVisits
  let nont = ndiNonterminal $ ndimOrig ndi
  let prod = pdgProduction $ pdgmOrig $ ndimProds ndi !! p
  let Just rprodv = Map.lookup (nont, prod, VGEdge edg) prodvs
  vgInST $ modifySTRef rprodv (++ [st])

-- | Get the state of a child in a certain production
getChildState :: VGProd -> Identifier -> VG s VGNode
getChildState (VGProd (VGEdge edg,p)) ide = do
  childvs <- gets vgChildVisits
  let rchildv = imLookup edg childvs
  childv  <- vgInST $ readSTRef rchildv
  case Map.lookup (ide,p) childv of
    Just (n:_) -> return n
    _          -> do
      -- Look for previous edge
      edges <- gets vgEdges
      let (VGNode from,_) = imLookup edg edges
      incoming <- gets vgIncoming
      case IntMap.lookup from incoming of
        Just (Just iedg) -> getChildState (VGProd (iedg,p)) ide
        Just Nothing     -> do
          -- Lookup initial state
          ndis <- gets vgNDI
          let rndi = imLookup from ndis
          ndi  <- vgInST $ readSTRef rndi
          let Just nt = lookup ide $ pdgChildMap $ pdgmOrig $ (ndimProds ndi) !! p
          vgFindInitial nt
        Nothing          -> error "getChildState"

-- | Repeat action untill mzero is encountered
repeatM :: VG s () -> VG s ()
repeatM m = catchError (m >> repeatM m) (const $ return ())

------------------------------------------------------------
---              Internal functions                      ---
------------------------------------------------------------
-- | Execute a ST action inside the VG monad
vgInST :: ST s a -> VG s a
vgInST = lift . lift

vgEmptyState :: VGState s
vgEmptyState = VGState { vgNodeNum       = 0
                       , vgEdgeNum       = 0
                       , vgOutgoing      = IntMap.empty
                       , vgIncoming      = IntMap.empty
                       , vgNDI           = IntMap.empty
                       , vgInhSynNode    = Map.empty
                       , vgNodeInhSyn    = IntMap.empty
                       , vgInitial       = Map.empty
                       , vgEdges         = IntMap.empty
                       , vgEdgesR        = Map.empty
                       , vgInherited     = IntMap.empty
                       , vgSynthesized   = IntMap.empty
                       , vgPending       = IntSet.empty
                       , vgChildVisits   = IntMap.empty
                       , vgFinalVertices = IntMap.empty
                       , vgProdVisits    = Map.empty
                       }
-- | Create a new node
vgCreateNode :: STRef s (NontDependencyInformationM s) -> Set Identifier -> Set Identifier -> VG s VGNode
vgCreateNode rndi inh syn = do
  num      <- gets vgNodeNum
  outgoing <- gets vgOutgoing
  inhsyn   <- gets vgInhSynNode
  ninhsyn  <- gets vgNodeInhSyn
  ndi      <- gets vgNDI
  finalv   <- gets vgFinalVertices
  rout     <- vgInST $ newSTRef Set.empty
  rfinalv  <- vgInST $ newSTRef Set.empty
  nndi     <- vgInST $ readSTRef rndi
  modify $ \st -> st { vgNodeNum       = num + 1
                     , vgOutgoing      = IntMap.insert num rout outgoing
                     , vgInhSynNode    = Map.insert (ndiNonterminal $ ndimOrig nndi,inh,syn) (VGNode num) inhsyn
                     , vgNodeInhSyn    = IntMap.insert num (inh,syn) ninhsyn
                     , vgNDI           = IntMap.insert num rndi ndi
                     , vgFinalVertices = IntMap.insert num rfinalv finalv }
  return $ VGNode num

-- | Create a new pending edge
vgCreatePendingEdge :: VGNode -> VGNode -> Set Identifier -> Set Identifier -> VG s VGEdge
vgCreatePendingEdge vgn1@(VGNode n1) vgn2 inh syn = do
  num      <- gets vgEdgeNum
  edges    <- gets vgEdges
  edgesr   <- gets vgEdgesR
  inhs     <- gets vgInherited
  syns     <- gets vgSynthesized
  outgoing <- gets vgOutgoing
  pend     <- gets vgPending
  childv   <- gets vgChildVisits
  rchildv  <- vgInST $ newSTRef Map.empty
  let outr    = imLookup n1 outgoing
  let ret     = VGEdge num
  vgInST $ modifySTRef outr (Set.insert ret)
  modify $ \st -> st { vgEdgeNum     = num + 1
                     , vgEdges       = IntMap.insert num (vgn1,vgn2) edges
                     , vgEdgesR      = Map.insert (vgn1,vgn2) ret edgesr
                     , vgPending     = IntSet.insert num pend
                     , vgInherited   = IntMap.insert num inh inhs
                     , vgSynthesized = IntMap.insert num syn syns
                     , vgChildVisits = IntMap.insert num rchildv childv }
  -- Add prod visits (for constructing an execution plan)
  ndis <- gets vgNDI
  let rndi = imLookup n1 ndis
  ndi <- vgInST $ readSTRef rndi
  prodv  <- gets vgProdVisits
  refs   <- forM (ndimProds ndi) $ \prod -> do
    rprod <- vgInST $ newSTRef []
    return ((ndiNonterminal $ ndimOrig ndi, pdgProduction $ pdgmOrig prod, ret),rprod)
  modify $ \st -> st { vgProdVisits = Map.union (Map.fromList refs) prodv }
  return $ ret

-- | Check whether a vertex is marked final on this node in this production
vgDepGraphVertexFinal :: VGNode -> Int -> Vertex -> VG s Bool
vgDepGraphVertexFinal (VGNode n) p v = do
  finalv <- gets vgFinalVertices
  let rfinalv = imLookup n finalv
  curset <- vgInST $ readSTRef rfinalv
  if Set.member (v,p) curset
    then return True
    else do
      incoming <- gets vgIncoming
      case IntMap.lookup n incoming of
        Just (Just (VGEdge edg)) -> do
          edges <- gets vgEdges
          let (fr,_) = imLookup edg edges
          vgDepGraphVertexFinal fr p v
        Just Nothing -> return False
        Nothing      -> error "This can never happen"

-- | Find the initial node for a nonterminal
vgFindInitial :: Identifier -> VG s VGNode
vgFindInitial nt = do
  initial <- gets vgInitial
  let Just r = Map.lookup nt initial
  return r

-- | Always succeeding IntMap lookup
imLookup :: Int -> IntMap a -> a
imLookup k m = let Just r = IntMap.lookup k m in r

-- | Trace inside the vg monad
traceVG :: String -> VG s ()
traceVG s = trace s (return ())

------------------------------------------------------------
---         The kennedy warren algorithm                 ---
------------------------------------------------------------
{-
runVG                    :: VG s a -> ST s a
insertInitialNode        :: NontDependencyInformationM s -> VG s VGNode
createPending            :: VGNode -> Set Identifier -> Set Identifier -> VG s VGEdge
selectPending            :: VG s VGEdge
getInherited             :: VGEdge -> VG s (Set Identifier)
getSynthesized           :: VGEdge -> VG s (Set Identifier)
markFinal                :: VGEdge -> VG s ()
getProductions           :: VGEdge -> VG s [VGProd]
onMarkedDepGraph         :: (ProdDependencyGraphM s -> ST s a) -> VGProd -> VG s a
isDepGraphVertexFinal    :: VGProd -> Vertex -> VG s Bool
setDepGraphVerticesFinal :: VGProd -> [Vertex] -> VG s ()
getChildState            :: VGProd -> Identifier -> VG s VGNode
addChildVisit            :: VGProd -> Identifier -> VGEdge -> VG s VisitStep
addVisitStep             :: VGProd -> VisitStep -> VG s ()
repeatM                  :: VG s () -> VG s ()
-}

kennedyWarrenVisitM :: Set NontermIdent -> [NontDependencyInformationM s] -> VG s [[VisitIdentifier]]
kennedyWarrenVisitM wr ndis = do
  -- Create initial nodes and edges (edges only for wrapper nodes)
  initvs <- forM ndis $ \ndi -> do
    nd <- insertInitialNode ndi
    let inh = Set.fromList $ ndiInh $ ndimOrig ndi
    let syn = Set.fromList $ ndiSyn $ ndimOrig ndi
    if (Set.member (ndiNonterminal $ ndimOrig $ ndi) wr) && (not (Set.null inh) || not (Set.null syn))
      then do
        VGEdge initv <- createPending nd inh syn
        return [initv]
      else return []
  -- Handle all pending edges while there are any
  repeatM $ do
    pend  <- selectPending
    prods <- getProductions pend
    inhs  <- getInherited pend
    syns  <- getSynthesized pend
    -- Handle each production for this edge
    forM_ prods $ \prod -> do
      -- Mark all inherited attributes as final
      setDepGraphVerticesFinal prod (map createLhsInh . Set.toList $ inhs)
      -- Find depth of all synthesized child visits
      (vis,_) <- foldM (foldChildVisits prod) ([],0) (map createLhsSyn . Set.toList $ syns)
      -- Mark them as final
      setDepGraphVerticesFinal prod (map fst vis)
      -- Change the inherited child visits
      vis2 <- correctInhChilds prod vis
      -- Add all synthesized attributes that are also ready but are not needed
      extravis <- extraChildSyn prod vis2
      setDepGraphVerticesFinal prod (map fst extravis)
      -- Group by visit number and do visit for every num
      let gvis = groupSortBy (comparing snd) $ vis2 ++ extravis
      forM_ gvis $ \vis3 -> do
        -- Split child visits from rules
        let (chattrs, rules) = partition isChildAttr $ map fst vis3
        -- Evaluate all rules
        forM_ (reverse $ rules) $ \rule ->
          case rule of
            VRule r  -> addVisitStep prod $ Sem r
            VChild c -> addVisitStep prod $ ChildIntro c
            _        -> return ()
        -- Now group by child, and do a visit for each child
        let chs = groupSortBy (comparing getAttrChildName) $ chattrs
        chvs <- forM chs $ \childvs -> do -- childs :: [Vertex]
          let cinhs = map getAttrName $ filter isChildInh childvs
          let csyns = map getAttrName $ filter isChildSyn childvs
          let cname = getAttrChildName $ head childvs
          -- Insert a new pending edge for this visit
          curstate <- getChildState prod cname
          target   <- createPending curstate (Set.fromList cinhs) (Set.fromList csyns)
          addChildVisit prod cname target
        -- Add child visits as simultanuous step
        when (not $ null chvs) $
          if (length chvs == 1)
          then addVisitStep prod $ head chvs
          else addVisitStep prod $ Sim chvs

    -- Mark this edge as final
    markFinal pend
  -- We are done
  -- traceVG "Done."
  return initvs

-- | groupBy that groups all equal (according to the function) elements instead of consequtive equal elements
groupSortBy :: (a -> a -> Ordering) -> [a] -> [[a]]
groupSortBy f = groupBy (\x y -> f x y == EQ) . sortBy f

type ChildVisits = [(Vertex,Int)]

-- | Helper function for folding over child visits
foldChildVisits :: VGProd -> (ChildVisits, Int) -> Vertex -> VG s (ChildVisits, Int)
foldChildVisits prod (vis,i) v = do
  (nvis,ni) <- findChildVisits prod v vis
  return (nvis, ni `max` i)

-- | Recursively find all visits to childs
findChildVisits :: VGProd -> Vertex -> ChildVisits -> VG s (ChildVisits, Int)
findChildVisits prod v vis = do
  case lookup v vis of
    Just i  -> return (vis,i)
    Nothing -> do
      final <- isDepGraphVertexFinal prod v
      if final
        then return (vis,0)
        else do
          succs <- onMarkedDepGraph (liftM Set.toList . flip graphSuccessors v . pdgmDepGraph) prod
          (nvis,ni)  <- foldM (foldChildVisits prod) (vis,0) succs
          if isChildSyn v
            then return ((v,ni + 1) : nvis, ni + 1)
            else return ((v,ni) : nvis, ni)

-- | Correct inherited child visits after foldChildVisits
correctInhChilds :: VGProd -> ChildVisits -> VG s ChildVisits
correctInhChilds prod vis =
  forM vis $ \(v,i) -> do
    if isChildInh v
     then do
      preds <- onMarkedDepGraph (liftM Set.toList . flip graphPredecessors v . pdgmDepGraph) prod
      let ni = foldl min 99999999 $ mapMaybe (`lookup` vis) preds
      return (v,ni)
     else if not $ isChildSyn v
           then do
            succs <- onMarkedDepGraph (liftM Set.toList . flip graphSuccessors v . pdgmDepGraph) prod
            let ni = foldl max (-1) $ mapMaybe (`lookup` vis) succs
            return (v,ni+1)
           else return (v,i)

-- | Synthesized attributes that can also be evaluated
extraChildSyn :: VGProd -> ChildVisits -> VG s ChildVisits
extraChildSyn prod vis = do
  allpreds <- forM vis $ \(v,_) -> do
    if isChildInh v
     then do
      preds <- onMarkedDepGraph (liftM Set.toList . flip graphPredecessors v . pdgmDepGraph) prod
      return $ Set.fromList $ filter isChildSyn preds
     else return Set.empty
  lextravis <- forM (Set.toList $ Set.unions allpreds) $ \v -> do
    ready <- isReadyVertex prod vis v
    return $ maybe Nothing (\i -> Just (v,i)) ready
  return $ catMaybes lextravis

-- | Check if a vertex can be marked final in this step (and is not final yet) and return the visit num
isReadyVertex :: VGProd -> ChildVisits -> Vertex -> VG s (Maybe Int)
isReadyVertex prod vis v = do
  final <- isDepGraphVertexFinal prod v
  if v `elem` (map fst vis) || final
    then return Nothing
    else do
      succ <- onMarkedDepGraph (flip graphSuccessors v . pdgmDepGraph) prod
      rd <- mapM (\x -> do case lookup x vis of
                             Just i  -> return $ Just i
                             Nothing -> do fin <- isDepGraphVertexFinal prod x
                                           return $ if fin then Just 1 else Nothing) (Set.toList succ)
      if all isJust rd
        then return $ Just $ foldl1 max $ catMaybes rd
        else return $ Nothing

-- | Check if this vertex is a synthesized attribute of a child
isChildSyn :: Vertex -> Bool
isChildSyn v = isChildAttr v && getAttrType v == Syn

-- | Check if this vertex is an inherited attribute of a child
isChildInh :: Vertex -> Bool
isChildInh v = isChildAttr v && getAttrType v == Inh

-- | Check if this vertex is an attribute of a child
isChildAttr :: Vertex -> Bool
isChildAttr v = isVertexAttr v && getAttrChildName v /= _LHS && getAttrType v /= Loc

-- | Create lhs.inh vertex
createLhsInh :: Identifier -> Vertex
createLhsInh = VAttr Inh _LHS

-- | Create lhs.inh vertex
createLhsSyn :: Identifier -> Vertex
createLhsSyn = VAttr Syn _LHS

------------------------------------------------------------
---         Construction of the execution plan           ---
------------------------------------------------------------
kennedyWarrenExecutionPlan :: Options -> [NontDependencyInformationM s] -> [[VisitIdentifier]] ->
                              Set NontermIdent -> TypeSyns -> Derivings -> VG s ExecutionPlan
kennedyWarrenExecutionPlan opts ndis initvs wr typesyns derivings = do
  -- Loop over all nonterminals
  nonts <- forM (zip ndis initvs) $ \(ndi, initv) -> do
    -- Loop over all productions of this nonterminal
    prods <- forM (ndimProds ndi) $ \prod -> do
      -- Construct the visits for this production
      let inont = ndiNonterminal $ ndimOrig ndi
      let iprod = pdgProduction $ pdgmOrig prod
      prodvs <- gets vgProdVisits
      let thisvisits = filter (\((int,ipr,_),_) -> int == inont && ipr == iprod) $ Map.toList prodvs
      visits <- forM thisvisits $ \((_,_,vgedg@(VGEdge edg)),rprodvs) -> do
        edges <- gets vgEdges
        let (VGNode fr, VGNode to) = imLookup edg edges
        steps <- vgInST $ readSTRef rprodvs
        inh   <- getInherited vgedg
        syn   <- getSynthesized vgedg
        let kind | monadic opts = VisitMonadic
                 | otherwise    = VisitPure True
        return $ Visit edg fr to inh syn steps kind
      -- Return execution plan for this production
      return $ EProduction (pdgProduction $ pdgmOrig prod)
                           (pdgParams     $ pdgmOrig prod)
               (pdgConstraints $ pdgmOrig prod)
                           (pdgRules      $ pdgmOrig prod)
                           (pdgChilds     $ pdgmOrig prod)
               visits
    -- Find initial state for this nonterminal
    VGNode init <- vgFindInitial $ ndiNonterminal $ ndimOrig ndi
    -- Construct an environment that specifies the next visit of the states that have exactly one
    nextMap <- mkNextMap init
    prevMap <- mkPrevMap init
    -- Return execution plan for this nonterminal
    return $  ENonterminal (ndiNonterminal $ ndimOrig ndi)
                           (ndiParams      $ ndimOrig ndi)
                           (ndiClassCtxs $ ndimOrig ndi)
                           init
                           initv
                           nextMap
                           prevMap
                           prods
                           (ndiRecursive $ ndimOrig ndi)
                           (ndiHoInfo    $ ndimOrig ndi)

  -- Return complete execution plan
  return $ ExecutionPlan nonts typesyns wr derivings

------------------------------------------------------------
---         Construction of the single-exit states map   ---
------------------------------------------------------------

-- depth-first traversal over the graph that starts at 'init' and maintains a state 'a'
-- the function 'f' can inspect the prev/next edges per state
exploreGraph :: (VGNode -> Set VGEdge -> Set VGEdge -> a -> VG s a) -> VGNode -> a -> VG s a
exploreGraph f (VGNode init) a0 = do
  exploredRef <- vgInST $ newSTRef IntSet.empty
  pendingRef  <- vgInST $ newSTRef [init]
  resRef      <- vgInST $ newSTRef a0
  outgoingMap <- gets vgOutgoing
  edgesInfo   <- gets vgEdges
  let explore = do
        pending <- vgInST $ readSTRef pendingRef
        case pending of
          []     -> return ()
          (p:ps) -> do
            vgInST $ writeSTRef pendingRef ps
            explored <- vgInST $ readSTRef exploredRef
            if IntSet.member p explored
              then return ()
              else do
                vgInST $ writeSTRef exploredRef (IntSet.insert p explored)
                case IntMap.lookup p outgoingMap of
                  Nothing -> return ()
                  Just outRef -> case IntMap.lookup p outgoingMap of
                    Nothing -> return ()
                    Just inRef -> do
                            outSet  <- vgInST $ readSTRef outRef
                            inSet   <- vgInST $ readSTRef inRef
                            sol0    <- vgInST $ readSTRef resRef
                            sol1    <- f (VGNode p) inSet outSet sol0
                            vgInST $ writeSTRef resRef sol1
                            forM_ (Set.elems outSet) $ \(VGEdge edge) ->
                              case IntMap.lookup edge edgesInfo of
                                Nothing            -> return ()
                                Just (_,VGNode to) -> vgInST $ modifySTRef pendingRef (to :)
            explore
  explore
  vgInST $ readSTRef resRef

mkNextMap :: Int -> VG s (Map Int StateCtx)
mkNextMap start = exploreGraph f (VGNode start) Map.empty where
  f (VGNode nd) _ edges = updateCountMap nd edges

mkPrevMap :: Int -> VG s (Map Int StateCtx)
mkPrevMap start = exploreGraph f (VGNode start) Map.empty where
  f (VGNode nd) edges _ = updateCountMap nd edges

updateCountMap :: Int -> Set VGEdge -> Map Int StateCtx -> VG s (Map Int StateCtx)
updateCountMap nd edges mp = return $ Map.insert nd v mp where
    s = Set.size edges
    v | s == 0    = NoneVis
      | s == 1    = let [VGEdge v'] = Set.elems edges
                    in OneVis v'
      | otherwise = ManyVis
