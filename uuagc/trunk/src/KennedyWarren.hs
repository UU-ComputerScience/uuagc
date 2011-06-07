module KennedyWarren where

import CommonTypes
import Pretty
import Knuth1
import Debug.Trace
import Control.Monad.ST
import Control.Monad.State
import Control.Monad.Error
import Data.STRef
import Data.Maybe
import Data.List

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet

kennedyWarrenIO :: [NontDependencyInformation] -> PP_Doc
kennedyWarrenIO ndis = runST $ do 
  indi <- mapM mkNontDependencyInformationM ndis
  knuth1 indi
  -- Check all graphs for cyclicity, transitive closure and consistency
  traceST $ "Checking graphs..."
  forM_ indi $ \ndi -> do
    let nont = ndiNonterminal . ndimOrig $ ndi
    let g = ndgmDepGraph . ndimDepGraph $ ndi
    -- Cyclicity check
    c1 <- graphIsCyclic g
    when c1 $ traceST $ "Nonterminal graph " ++ show nont ++ " is cylic!"
    -- Transtive closure check
    trc <- graphIsTRC g
    when (not trc) $ traceST $ "Nonterminal graph " ++ show nont ++ " is not transitively closed!"
    -- Consistency check
    cons <- graphCheckConsistency g
    when (not cons) $ traceST $ "Nonterminal graph " ++ show nont ++ " is not consistent!"
    -- Loop trough all productions
    forM_ (ndimProds ndi) $ \prod -> do
      let pr = pdgProduction $ pdgmOrig prod
      let g = pdgmDepGraph $ prod
      -- Check for cyclicity
      c2 <- graphIsCyclic g 
      when c2 $ traceST $ "Production graph " ++ show pr ++ " of nonterminal "
                          ++ show nont ++ " is cylic!"
      -- Transtive closure check
      trc <- graphIsTRC g
      when (not trc) $ traceST $ "Production graph " ++ show pr ++ " of nonterminal " 
                                 ++ show nont ++ " is not transitively closed!"
      -- Check consistency
      cons <- graphCheckConsistency g
      when (not cons) $ traceST $ "Production graph " ++ show pr ++ " of nonterminal " 
                                 ++ show nont ++ " is not consistent!"
  -- Recreate non-transitive closed graph for efficiency
  indi <- undoTransitiveClosure indi
  -- Debug output
  traceST $ "Converting to string..."
  rtexts <- mapM toGVNontDependencyInfo indi
  traceST $ "Done."
  return (vlist rtexts)

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
