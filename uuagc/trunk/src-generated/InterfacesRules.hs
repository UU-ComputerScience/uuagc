{-# LANGUAGE Rank2Types, GADTs #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module InterfacesRules where
{-# LINE 2 "./src-ag/Interfaces.ag" #-}

import CommonTypes
import SequentialTypes
{-# LINE 11 "dist/build/InterfacesRules.hs" #-}

{-# LINE 10 "./src-ag/InterfacesRules.lag" #-}

import Interfaces
import CodeSyntax
import GrammarInfo

import qualified Data.Sequence as Seq
import Data.Sequence(Seq)
import qualified Data.Map as Map
import Data.Map(Map)
import Data.Tree(Tree(Node), Forest)
import Data.Graph(Graph, dfs, edges, buildG, transposeG)
import Data.Maybe (fromJust)
import Data.List (partition,transpose,(\\),nub,findIndex)
import Data.Array ((!),inRange,bounds,assocs)
import Data.Foldable(toList)
{-# LINE 29 "dist/build/InterfacesRules.hs" #-}
import Control.Monad.Identity (Identity)
import qualified Control.Monad.Identity
{-# LINE 53 "./src-ag/InterfacesRules.lag" #-}

type VisitSS = [Vertex]
{-# LINE 35 "dist/build/InterfacesRules.hs" #-}

{-# LINE 88 "./src-ag/InterfacesRules.lag" #-}

gather :: Info -> [Vertex] -> [[Vertex]]
gather info =  eqClasses comp
               where comp a b = isEqualField (ruleTable info ! a) (ruleTable info ! b)
{-# LINE 42 "dist/build/InterfacesRules.hs" #-}

{-# LINE 129 "./src-ag/InterfacesRules.lag" #-}

-- Only non-empty syn will ever be forced, because visits with empty syn are never performed
-- Right hand side synthesized attributes always have a field
cv :: (Vertex -> CRule) -> Int -> Vertex -> ([Vertex],[Vertex]) -> (Vertex,ChildVisit)
cv look n v (inh,syn) =  let  fld = getField (look (head syn))
                              rnt = fromJust (getRhsNt (look (head syn)))
                              d = ChildVisit fld rnt n inh syn
                         in (v,d)
{-# LINE 53 "dist/build/InterfacesRules.hs" #-}

{-# LINE 152 "./src-ag/InterfacesRules.lag" #-}

ed :: Vertex -> ([Vertex], [Vertex]) -> [(Vertex, Vertex)]
ed v (inh,syn) = map (\i -> (i,v)) inh ++ map (\s -> (v,s)) syn
{-# LINE 59 "dist/build/InterfacesRules.hs" #-}

{-# LINE 240 "./src-ag/InterfacesRules.lag" #-}

postorder :: Tree a -> [a]
postorder (Node a ts) = postorderF ts ++ [a]
postorderF :: Forest a -> [a]
postorderF = concatMap postorder
postOrd :: Graph -> [Vertex] -> [Vertex]
postOrd g = postorderF . dfs g
topSort' :: Graph -> [Vertex] -> [Vertex]
topSort' g = postOrd g
{-# LINE 71 "dist/build/InterfacesRules.hs" #-}

{-# LINE 323 "./src-ag/InterfacesRules.lag" #-}

type IntraVisit = [Vertex]
{-# LINE 76 "dist/build/InterfacesRules.hs" #-}

{-# LINE 345 "./src-ag/InterfacesRules.lag" #-}

swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)
{-# LINE 82 "dist/build/InterfacesRules.hs" #-}

{-# LINE 420 "./src-ag/InterfacesRules.lag" #-}

ccv :: Identifier -> NontermIdent -> Int -> CInterfaceMap -> CRule
ccv name nt n table
  =  CChildVisit name nt n inh syn lst
     where  CInterface segs = Map.findWithDefault (error ("InterfacesRules::ccv::interfaces not in table for nt: " ++ show nt)) nt table
            (seg:remain) = drop n segs
            CSegment inh syn = seg
            lst = null remain
{-# LINE 93 "dist/build/InterfacesRules.hs" #-}
-- IRoot -------------------------------------------------------
-- wrapper
data Inh_IRoot  = Inh_IRoot { dpr_Inh_IRoot :: !([Edge]), info_Inh_IRoot :: !(Info), tdp_Inh_IRoot :: !(Graph) }
data Syn_IRoot  = Syn_IRoot { edp_Syn_IRoot :: !([Edge]), inters_Syn_IRoot :: !(CInterfaceMap), visits_Syn_IRoot :: !(CVisitsMap) }
{-# INLINABLE wrap_IRoot #-}
wrap_IRoot :: T_IRoot  -> Inh_IRoot  -> (Syn_IRoot )
wrap_IRoot !(T_IRoot act) !(Inh_IRoot _lhsIdpr _lhsIinfo _lhsItdp) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_IRoot_vIn1 _lhsIdpr _lhsIinfo _lhsItdp
        !(T_IRoot_vOut1 _lhsOedp _lhsOinters _lhsOvisits) <- return (inv_IRoot_s2 sem arg)
        return (Syn_IRoot _lhsOedp _lhsOinters _lhsOvisits)
   )

-- cata
{-# INLINE sem_IRoot #-}
sem_IRoot :: IRoot  -> T_IRoot 
sem_IRoot ( IRoot inters_ ) = sem_IRoot_IRoot ( sem_Interfaces inters_ )

-- semantic domain
newtype T_IRoot  = T_IRoot {
                           attach_T_IRoot :: Identity (T_IRoot_s2 )
                           }
newtype T_IRoot_s2  = C_IRoot_s2 {
                                 inv_IRoot_s2 :: (T_IRoot_v1 )
                                 }
data T_IRoot_s3  = C_IRoot_s3
type T_IRoot_v1  = (T_IRoot_vIn1 ) -> (T_IRoot_vOut1 )
data T_IRoot_vIn1  = T_IRoot_vIn1 ([Edge]) (Info) (Graph)
data T_IRoot_vOut1  = T_IRoot_vOut1 ([Edge]) (CInterfaceMap) (CVisitsMap)
{-# NOINLINE sem_IRoot_IRoot #-}
sem_IRoot_IRoot :: T_Interfaces  -> T_IRoot 
sem_IRoot_IRoot arg_inters_ = T_IRoot (return st2) where
   {-# NOINLINE st2 #-}
   !st2 = let
      v1 :: T_IRoot_v1 
      v1 = \ !(T_IRoot_vIn1 _lhsIdpr _lhsIinfo _lhsItdp) -> ( let
         _intersX8 = Control.Monad.Identity.runIdentity (attach_T_Interfaces (arg_inters_))
         (T_Interfaces_vOut7 _intersIdescr _intersIedp _intersIfirstvisitvertices _intersIinters _intersInewedges _intersIv _intersIvisits) = inv_Interfaces_s8 _intersX8 (T_Interfaces_vIn7 _intersOallInters _intersOddp _intersOinfo _intersOprev _intersOv _intersOvisitDescr _intersOvssGraph)
         _newedges = rule0 _intersInewedges
         _visitssGraph = rule1 _intersIv _lhsItdp _newedges
         _intersOv = rule2 _lhsItdp
         _intersOvisitDescr = rule3 _descr
         _descr = rule4 _intersIdescr
         _intersOvssGraph = rule5 _visitssGraph
         _intersOprev = rule6 _intersIfirstvisitvertices _lhsIinfo
         _intersOddp = rule7 _intersIv _lhsIdpr _newedges
         _intersOallInters = rule8 _intersIinters
         _lhsOedp :: [Edge]
         _lhsOedp = rule9 _intersIedp
         _lhsOinters :: CInterfaceMap
         _lhsOinters = rule10 _intersIinters
         _lhsOvisits :: CVisitsMap
         _lhsOvisits = rule11 _intersIvisits
         _intersOinfo = rule12 _lhsIinfo
         !__result_ = T_IRoot_vOut1 _lhsOedp _lhsOinters _lhsOvisits
         in __result_ )
     in C_IRoot_s2 v1
   {-# INLINE rule0 #-}
   {-# LINE 66 "./src-ag/InterfacesRules.lag" #-}
   rule0 = \ ((_intersInewedges) :: Seq Edge ) ->
                            {-# LINE 66 "./src-ag/InterfacesRules.lag" #-}
                            toList _intersInewedges
                            {-# LINE 157 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule1 #-}
   {-# LINE 67 "./src-ag/InterfacesRules.lag" #-}
   rule1 = \ ((_intersIv) :: Vertex) ((_lhsItdp) :: Graph) _newedges ->
                                 {-# LINE 67 "./src-ag/InterfacesRules.lag" #-}
                                 let graph = buildG (0,_intersIv-1) es
                                     es = _newedges ++ edges _lhsItdp
                                 in transposeG graph
                                 {-# LINE 165 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule2 #-}
   {-# LINE 80 "./src-ag/InterfacesRules.lag" #-}
   rule2 = \ ((_lhsItdp) :: Graph) ->
                        {-# LINE 80 "./src-ag/InterfacesRules.lag" #-}
                        snd (bounds _lhsItdp) + 1
                        {-# LINE 171 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule3 #-}
   {-# LINE 122 "./src-ag/InterfacesRules.lag" #-}
   rule3 = \ _descr ->
                                  {-# LINE 122 "./src-ag/InterfacesRules.lag" #-}
                                  Map.fromList _descr
                                  {-# LINE 177 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule4 #-}
   {-# LINE 142 "./src-ag/InterfacesRules.lag" #-}
   rule4 = \ ((_intersIdescr) :: Seq (Vertex,ChildVisit)) ->
                         {-# LINE 142 "./src-ag/InterfacesRules.lag" #-}
                         toList _intersIdescr
                         {-# LINE 183 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule5 #-}
   {-# LINE 214 "./src-ag/InterfacesRules.lag" #-}
   rule5 = \ _visitssGraph ->
                               {-# LINE 214 "./src-ag/InterfacesRules.lag" #-}
                               _visitssGraph
                               {-# LINE 189 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule6 #-}
   {-# LINE 260 "./src-ag/InterfacesRules.lag" #-}
   rule6 = \ ((_intersIfirstvisitvertices) :: [Vertex]) ((_lhsIinfo) :: Info) ->
                            {-# LINE 260 "./src-ag/InterfacesRules.lag" #-}
                            let terminals = [ v | (v,cr) <- assocs (ruleTable _lhsIinfo), not (getHasCode cr), isLocal cr ]
                            in _intersIfirstvisitvertices ++ terminals
                            {-# LINE 196 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule7 #-}
   {-# LINE 343 "./src-ag/InterfacesRules.lag" #-}
   rule7 = \ ((_intersIv) :: Vertex) ((_lhsIdpr) :: [Edge]) _newedges ->
                          {-# LINE 343 "./src-ag/InterfacesRules.lag" #-}
                          buildG (0,_intersIv-1) (map swap (_lhsIdpr ++ _newedges))
                          {-# LINE 202 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule8 #-}
   {-# LINE 381 "./src-ag/InterfacesRules.lag" #-}
   rule8 = \ ((_intersIinters) :: CInterfaceMap) ->
                                 {-# LINE 381 "./src-ag/InterfacesRules.lag" #-}
                                 _intersIinters
                                 {-# LINE 208 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule9 #-}
   {-# LINE 443 "./src-ag/InterfacesRules.lag" #-}
   rule9 = \ ((_intersIedp) :: Seq Edge) ->
                        {-# LINE 443 "./src-ag/InterfacesRules.lag" #-}
                        toList _intersIedp
                        {-# LINE 214 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule10 #-}
   rule10 = \ ((_intersIinters) :: CInterfaceMap) ->
     _intersIinters
   {-# INLINE rule11 #-}
   rule11 = \ ((_intersIvisits) :: CVisitsMap) ->
     _intersIvisits
   {-# INLINE rule12 #-}
   rule12 = \ ((_lhsIinfo) :: Info) ->
     _lhsIinfo

-- Interface ---------------------------------------------------
-- wrapper
data Inh_Interface  = Inh_Interface { allInters_Inh_Interface :: !(CInterfaceMap), ddp_Inh_Interface :: !(Graph), info_Inh_Interface :: !(Info), prev_Inh_Interface :: !([Vertex]), v_Inh_Interface :: !(Vertex), visitDescr_Inh_Interface :: !(Map Vertex ChildVisit), vssGraph_Inh_Interface :: !(Graph) }
data Syn_Interface  = Syn_Interface { descr_Syn_Interface :: !(Seq (Vertex,ChildVisit)), edp_Syn_Interface :: !(Seq Edge), firstvisitvertices_Syn_Interface :: !([Vertex]), inter_Syn_Interface :: !(CInterface), newedges_Syn_Interface :: !(Seq Edge ), nt_Syn_Interface :: !(NontermIdent), v_Syn_Interface :: !(Vertex), visits_Syn_Interface :: !(Map ConstructorIdent CVisits) }
{-# INLINABLE wrap_Interface #-}
wrap_Interface :: T_Interface  -> Inh_Interface  -> (Syn_Interface )
wrap_Interface !(T_Interface act) !(Inh_Interface _lhsIallInters _lhsIddp _lhsIinfo _lhsIprev _lhsIv _lhsIvisitDescr _lhsIvssGraph) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_Interface_vIn4 _lhsIallInters _lhsIddp _lhsIinfo _lhsIprev _lhsIv _lhsIvisitDescr _lhsIvssGraph
        !(T_Interface_vOut4 _lhsOdescr _lhsOedp _lhsOfirstvisitvertices _lhsOinter _lhsOnewedges _lhsOnt _lhsOv _lhsOvisits) <- return (inv_Interface_s5 sem arg)
        return (Syn_Interface _lhsOdescr _lhsOedp _lhsOfirstvisitvertices _lhsOinter _lhsOnewedges _lhsOnt _lhsOv _lhsOvisits)
   )

-- cata
{-# INLINE sem_Interface #-}
sem_Interface :: Interface  -> T_Interface 
sem_Interface ( Interface !nt_ !cons_ seg_ ) = sem_Interface_Interface nt_ cons_ ( sem_Segments seg_ )

-- semantic domain
newtype T_Interface  = T_Interface {
                                   attach_T_Interface :: Identity (T_Interface_s5 )
                                   }
newtype T_Interface_s5  = C_Interface_s5 {
                                         inv_Interface_s5 :: (T_Interface_v4 )
                                         }
data T_Interface_s6  = C_Interface_s6
type T_Interface_v4  = (T_Interface_vIn4 ) -> (T_Interface_vOut4 )
data T_Interface_vIn4  = T_Interface_vIn4 (CInterfaceMap) (Graph) (Info) ([Vertex]) (Vertex) (Map Vertex ChildVisit) (Graph)
data T_Interface_vOut4  = T_Interface_vOut4 (Seq (Vertex,ChildVisit)) (Seq Edge) ([Vertex]) (CInterface) (Seq Edge ) (NontermIdent) (Vertex) (Map ConstructorIdent CVisits)
{-# NOINLINE sem_Interface_Interface #-}
sem_Interface_Interface :: (NontermIdent) -> ([ConstructorIdent]) -> T_Segments  -> T_Interface 
sem_Interface_Interface !arg_nt_ !arg_cons_ arg_seg_ = T_Interface (return st5) where
   {-# NOINLINE st5 #-}
   !st5 = let
      v4 :: T_Interface_v4 
      v4 = \ !(T_Interface_vIn4 _lhsIallInters _lhsIddp _lhsIinfo _lhsIprev _lhsIv _lhsIvisitDescr _lhsIvssGraph) -> ( let
         _segX14 = Control.Monad.Identity.runIdentity (attach_T_Segments (arg_seg_))
         (T_Segments_vOut13 _segIcvisits _segIdescr _segIedp _segIfirstInh _segIgroups _segIhdIntravisits _segInewedges _segInewvertices _segIprev _segIsegs _segIv) = inv_Segments_s14 _segX14 (T_Segments_vIn13 _segOallInters _segOcons _segOddp _segOfromLhs _segOinfo _segOisFirst _segOn _segOprev _segOv _segOvisitDescr _segOvssGraph)
         _segOv = rule13 _lhsIv
         _v = rule14 _segInewvertices _segIv
         _lhsOv :: Vertex
         _lhsOv = rule15 _v
         _firstvisitvertices = rule16 _segIv _v
         _newedges = rule17 _firstvisitvertices _segInewvertices
         _lhsOnewedges :: Seq Edge 
         _lhsOnewedges = rule18 _newedges _segInewedges
         _look :: Vertex -> CRule
         _look = rule19 _lhsIinfo
         _descr = rule20 _firstvisitvertices _look _segIgroups
         _lhsOdescr :: Seq (Vertex,ChildVisit)
         _lhsOdescr = rule21 _descr _segIdescr
         _segOn = rule22  ()
         _segOcons = rule23 arg_cons_
         _segOisFirst = rule24  ()
         _segOfromLhs = rule25 _lhsIprev
         _lhsOnt :: NontermIdent
         _lhsOnt = rule26 arg_nt_
         _lhsOinter :: CInterface
         _lhsOinter = rule27 _segIsegs
         _lhsOvisits :: Map ConstructorIdent CVisits
         _lhsOvisits = rule28 _segIcvisits arg_cons_
         _lhsOedp :: Seq Edge
         _lhsOedp = rule29 _segIedp
         _lhsOfirstvisitvertices :: [Vertex]
         _lhsOfirstvisitvertices = rule30 _firstvisitvertices
         _segOallInters = rule31 _lhsIallInters
         _segOddp = rule32 _lhsIddp
         _segOinfo = rule33 _lhsIinfo
         _segOprev = rule34 _lhsIprev
         _segOvisitDescr = rule35 _lhsIvisitDescr
         _segOvssGraph = rule36 _lhsIvssGraph
         !__result_ = T_Interface_vOut4 _lhsOdescr _lhsOedp _lhsOfirstvisitvertices _lhsOinter _lhsOnewedges _lhsOnt _lhsOv _lhsOvisits
         in __result_ )
     in C_Interface_s5 v4
   {-# INLINE rule13 #-}
   {-# LINE 183 "./src-ag/InterfacesRules.lag" #-}
   rule13 = \ ((_lhsIv) :: Vertex) ->
                          {-# LINE 183 "./src-ag/InterfacesRules.lag" #-}
                          _lhsIv
                          {-# LINE 305 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule14 #-}
   {-# LINE 184 "./src-ag/InterfacesRules.lag" #-}
   rule14 = \ ((_segInewvertices) :: [Vertex]) ((_segIv) :: Vertex) ->
                          {-# LINE 184 "./src-ag/InterfacesRules.lag" #-}
                          _segIv + length _segInewvertices
                          {-# LINE 311 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule15 #-}
   {-# LINE 185 "./src-ag/InterfacesRules.lag" #-}
   rule15 = \ _v ->
                          {-# LINE 185 "./src-ag/InterfacesRules.lag" #-}
                          _v
                          {-# LINE 317 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule16 #-}
   {-# LINE 186 "./src-ag/InterfacesRules.lag" #-}
   rule16 = \ ((_segIv) :: Vertex) _v ->
                                           {-# LINE 186 "./src-ag/InterfacesRules.lag" #-}
                                           [_segIv .. _v-1]
                                           {-# LINE 323 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule17 #-}
   {-# LINE 187 "./src-ag/InterfacesRules.lag" #-}
   rule17 = \ _firstvisitvertices ((_segInewvertices) :: [Vertex]) ->
                                 {-# LINE 187 "./src-ag/InterfacesRules.lag" #-}
                                 zip _firstvisitvertices _segInewvertices
                                 {-# LINE 329 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule18 #-}
   {-# LINE 188 "./src-ag/InterfacesRules.lag" #-}
   rule18 = \ _newedges ((_segInewedges) :: Seq Edge ) ->
                                 {-# LINE 188 "./src-ag/InterfacesRules.lag" #-}
                                 _segInewedges Seq.>< Seq.fromList _newedges
                                 {-# LINE 335 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule19 #-}
   {-# LINE 191 "./src-ag/InterfacesRules.lag" #-}
   rule19 = \ ((_lhsIinfo) :: Info) ->
                             {-# LINE 191 "./src-ag/InterfacesRules.lag" #-}
                             \a -> ruleTable _lhsIinfo ! a
                             {-# LINE 341 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule20 #-}
   {-# LINE 192 "./src-ag/InterfacesRules.lag" #-}
   rule20 = \ _firstvisitvertices ((_look) :: Vertex -> CRule) ((_segIgroups) :: [([Vertex],[Vertex])]) ->
                              {-# LINE 192 "./src-ag/InterfacesRules.lag" #-}
                              zipWith (cv _look (-1)) _firstvisitvertices _segIgroups
                              {-# LINE 347 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule21 #-}
   {-# LINE 193 "./src-ag/InterfacesRules.lag" #-}
   rule21 = \ _descr ((_segIdescr) :: Seq (Vertex,ChildVisit)) ->
                              {-# LINE 193 "./src-ag/InterfacesRules.lag" #-}
                              _segIdescr Seq.>< Seq.fromList _descr
                              {-# LINE 353 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule22 #-}
   {-# LINE 201 "./src-ag/InterfacesRules.lag" #-}
   rule22 = \  (_ :: ()) ->
                         {-# LINE 201 "./src-ag/InterfacesRules.lag" #-}
                         0
                         {-# LINE 359 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule23 #-}
   {-# LINE 233 "./src-ag/InterfacesRules.lag" #-}
   rule23 = \ cons_ ->
                            {-# LINE 233 "./src-ag/InterfacesRules.lag" #-}
                            cons_
                            {-# LINE 365 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule24 #-}
   {-# LINE 314 "./src-ag/InterfacesRules.lag" #-}
   rule24 = \  (_ :: ()) ->
                               {-# LINE 314 "./src-ag/InterfacesRules.lag" #-}
                               True
                               {-# LINE 371 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule25 #-}
   {-# LINE 352 "./src-ag/InterfacesRules.lag" #-}
   rule25 = \ ((_lhsIprev) :: [Vertex]) ->
                               {-# LINE 352 "./src-ag/InterfacesRules.lag" #-}
                               _lhsIprev
                               {-# LINE 377 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule26 #-}
   {-# LINE 392 "./src-ag/InterfacesRules.lag" #-}
   rule26 = \ nt_ ->
                          {-# LINE 392 "./src-ag/InterfacesRules.lag" #-}
                          nt_
                          {-# LINE 383 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule27 #-}
   {-# LINE 396 "./src-ag/InterfacesRules.lag" #-}
   rule27 = \ ((_segIsegs) :: CSegments) ->
                              {-# LINE 396 "./src-ag/InterfacesRules.lag" #-}
                              CInterface _segIsegs
                              {-# LINE 389 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule28 #-}
   {-# LINE 397 "./src-ag/InterfacesRules.lag" #-}
   rule28 = \ ((_segIcvisits) :: [[CVisit]]) cons_ ->
                               {-# LINE 397 "./src-ag/InterfacesRules.lag" #-}
                               Map.fromList (zip cons_ (transpose _segIcvisits))
                               {-# LINE 395 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule29 #-}
   rule29 = \ ((_segIedp) :: Seq Edge) ->
     _segIedp
   {-# INLINE rule30 #-}
   rule30 = \ _firstvisitvertices ->
     _firstvisitvertices
   {-# INLINE rule31 #-}
   rule31 = \ ((_lhsIallInters) :: CInterfaceMap) ->
     _lhsIallInters
   {-# INLINE rule32 #-}
   rule32 = \ ((_lhsIddp) :: Graph) ->
     _lhsIddp
   {-# INLINE rule33 #-}
   rule33 = \ ((_lhsIinfo) :: Info) ->
     _lhsIinfo
   {-# INLINE rule34 #-}
   rule34 = \ ((_lhsIprev) :: [Vertex]) ->
     _lhsIprev
   {-# INLINE rule35 #-}
   rule35 = \ ((_lhsIvisitDescr) :: Map Vertex ChildVisit) ->
     _lhsIvisitDescr
   {-# INLINE rule36 #-}
   rule36 = \ ((_lhsIvssGraph) :: Graph) ->
     _lhsIvssGraph

-- Interfaces --------------------------------------------------
-- wrapper
data Inh_Interfaces  = Inh_Interfaces { allInters_Inh_Interfaces :: !(CInterfaceMap), ddp_Inh_Interfaces :: !(Graph), info_Inh_Interfaces :: !(Info), prev_Inh_Interfaces :: !([Vertex]), v_Inh_Interfaces :: !(Vertex), visitDescr_Inh_Interfaces :: !(Map Vertex ChildVisit), vssGraph_Inh_Interfaces :: !(Graph) }
data Syn_Interfaces  = Syn_Interfaces { descr_Syn_Interfaces :: !(Seq (Vertex,ChildVisit)), edp_Syn_Interfaces :: !(Seq Edge), firstvisitvertices_Syn_Interfaces :: !([Vertex]), inters_Syn_Interfaces :: !(CInterfaceMap), newedges_Syn_Interfaces :: !(Seq Edge ), v_Syn_Interfaces :: !(Vertex), visits_Syn_Interfaces :: !(CVisitsMap) }
{-# INLINABLE wrap_Interfaces #-}
wrap_Interfaces :: T_Interfaces  -> Inh_Interfaces  -> (Syn_Interfaces )
wrap_Interfaces !(T_Interfaces act) !(Inh_Interfaces _lhsIallInters _lhsIddp _lhsIinfo _lhsIprev _lhsIv _lhsIvisitDescr _lhsIvssGraph) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_Interfaces_vIn7 _lhsIallInters _lhsIddp _lhsIinfo _lhsIprev _lhsIv _lhsIvisitDescr _lhsIvssGraph
        !(T_Interfaces_vOut7 _lhsOdescr _lhsOedp _lhsOfirstvisitvertices _lhsOinters _lhsOnewedges _lhsOv _lhsOvisits) <- return (inv_Interfaces_s8 sem arg)
        return (Syn_Interfaces _lhsOdescr _lhsOedp _lhsOfirstvisitvertices _lhsOinters _lhsOnewedges _lhsOv _lhsOvisits)
   )

-- cata
{-# NOINLINE sem_Interfaces #-}
sem_Interfaces :: Interfaces  -> T_Interfaces 
sem_Interfaces list = Prelude.foldr sem_Interfaces_Cons sem_Interfaces_Nil (Prelude.map sem_Interface list)

-- semantic domain
newtype T_Interfaces  = T_Interfaces {
                                     attach_T_Interfaces :: Identity (T_Interfaces_s8 )
                                     }
newtype T_Interfaces_s8  = C_Interfaces_s8 {
                                           inv_Interfaces_s8 :: (T_Interfaces_v7 )
                                           }
data T_Interfaces_s9  = C_Interfaces_s9
type T_Interfaces_v7  = (T_Interfaces_vIn7 ) -> (T_Interfaces_vOut7 )
data T_Interfaces_vIn7  = T_Interfaces_vIn7 (CInterfaceMap) (Graph) (Info) ([Vertex]) (Vertex) (Map Vertex ChildVisit) (Graph)
data T_Interfaces_vOut7  = T_Interfaces_vOut7 (Seq (Vertex,ChildVisit)) (Seq Edge) ([Vertex]) (CInterfaceMap) (Seq Edge ) (Vertex) (CVisitsMap)
{-# NOINLINE sem_Interfaces_Cons #-}
sem_Interfaces_Cons :: T_Interface  -> T_Interfaces  -> T_Interfaces 
sem_Interfaces_Cons arg_hd_ arg_tl_ = T_Interfaces (return st8) where
   {-# NOINLINE st8 #-}
   !st8 = let
      v7 :: T_Interfaces_v7 
      v7 = \ !(T_Interfaces_vIn7 _lhsIallInters _lhsIddp _lhsIinfo _lhsIprev _lhsIv _lhsIvisitDescr _lhsIvssGraph) -> ( let
         _hdX5 = Control.Monad.Identity.runIdentity (attach_T_Interface (arg_hd_))
         _tlX8 = Control.Monad.Identity.runIdentity (attach_T_Interfaces (arg_tl_))
         (T_Interface_vOut4 _hdIdescr _hdIedp _hdIfirstvisitvertices _hdIinter _hdInewedges _hdInt _hdIv _hdIvisits) = inv_Interface_s5 _hdX5 (T_Interface_vIn4 _hdOallInters _hdOddp _hdOinfo _hdOprev _hdOv _hdOvisitDescr _hdOvssGraph)
         (T_Interfaces_vOut7 _tlIdescr _tlIedp _tlIfirstvisitvertices _tlIinters _tlInewedges _tlIv _tlIvisits) = inv_Interfaces_s8 _tlX8 (T_Interfaces_vIn7 _tlOallInters _tlOddp _tlOinfo _tlOprev _tlOv _tlOvisitDescr _tlOvssGraph)
         _lhsOinters :: CInterfaceMap
         _lhsOinters = rule37 _hdIinter _hdInt _tlIinters
         _lhsOvisits :: CVisitsMap
         _lhsOvisits = rule38 _hdInt _hdIvisits _tlIvisits
         _lhsOdescr :: Seq (Vertex,ChildVisit)
         _lhsOdescr = rule39 _hdIdescr _tlIdescr
         _lhsOedp :: Seq Edge
         _lhsOedp = rule40 _hdIedp _tlIedp
         _lhsOfirstvisitvertices :: [Vertex]
         _lhsOfirstvisitvertices = rule41 _hdIfirstvisitvertices _tlIfirstvisitvertices
         _lhsOnewedges :: Seq Edge 
         _lhsOnewedges = rule42 _hdInewedges _tlInewedges
         _lhsOv :: Vertex
         _lhsOv = rule43 _tlIv
         _hdOallInters = rule44 _lhsIallInters
         _hdOddp = rule45 _lhsIddp
         _hdOinfo = rule46 _lhsIinfo
         _hdOprev = rule47 _lhsIprev
         _hdOv = rule48 _lhsIv
         _hdOvisitDescr = rule49 _lhsIvisitDescr
         _hdOvssGraph = rule50 _lhsIvssGraph
         _tlOallInters = rule51 _lhsIallInters
         _tlOddp = rule52 _lhsIddp
         _tlOinfo = rule53 _lhsIinfo
         _tlOprev = rule54 _lhsIprev
         _tlOv = rule55 _hdIv
         _tlOvisitDescr = rule56 _lhsIvisitDescr
         _tlOvssGraph = rule57 _lhsIvssGraph
         !__result_ = T_Interfaces_vOut7 _lhsOdescr _lhsOedp _lhsOfirstvisitvertices _lhsOinters _lhsOnewedges _lhsOv _lhsOvisits
         in __result_ )
     in C_Interfaces_s8 v7
   {-# INLINE rule37 #-}
   {-# LINE 386 "./src-ag/InterfacesRules.lag" #-}
   rule37 = \ ((_hdIinter) :: CInterface) ((_hdInt) :: NontermIdent) ((_tlIinters) :: CInterfaceMap) ->
                          {-# LINE 386 "./src-ag/InterfacesRules.lag" #-}
                          Map.insert _hdInt _hdIinter _tlIinters
                          {-# LINE 498 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule38 #-}
   {-# LINE 387 "./src-ag/InterfacesRules.lag" #-}
   rule38 = \ ((_hdInt) :: NontermIdent) ((_hdIvisits) :: Map ConstructorIdent CVisits) ((_tlIvisits) :: CVisitsMap) ->
                          {-# LINE 387 "./src-ag/InterfacesRules.lag" #-}
                          Map.insert _hdInt _hdIvisits _tlIvisits
                          {-# LINE 504 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule39 #-}
   rule39 = \ ((_hdIdescr) :: Seq (Vertex,ChildVisit)) ((_tlIdescr) :: Seq (Vertex,ChildVisit)) ->
     _hdIdescr Seq.>< _tlIdescr
   {-# INLINE rule40 #-}
   rule40 = \ ((_hdIedp) :: Seq Edge) ((_tlIedp) :: Seq Edge) ->
     _hdIedp Seq.>< _tlIedp
   {-# INLINE rule41 #-}
   rule41 = \ ((_hdIfirstvisitvertices) :: [Vertex]) ((_tlIfirstvisitvertices) :: [Vertex]) ->
     _hdIfirstvisitvertices ++ _tlIfirstvisitvertices
   {-# INLINE rule42 #-}
   rule42 = \ ((_hdInewedges) :: Seq Edge ) ((_tlInewedges) :: Seq Edge ) ->
     _hdInewedges Seq.>< _tlInewedges
   {-# INLINE rule43 #-}
   rule43 = \ ((_tlIv) :: Vertex) ->
     _tlIv
   {-# INLINE rule44 #-}
   rule44 = \ ((_lhsIallInters) :: CInterfaceMap) ->
     _lhsIallInters
   {-# INLINE rule45 #-}
   rule45 = \ ((_lhsIddp) :: Graph) ->
     _lhsIddp
   {-# INLINE rule46 #-}
   rule46 = \ ((_lhsIinfo) :: Info) ->
     _lhsIinfo
   {-# INLINE rule47 #-}
   rule47 = \ ((_lhsIprev) :: [Vertex]) ->
     _lhsIprev
   {-# INLINE rule48 #-}
   rule48 = \ ((_lhsIv) :: Vertex) ->
     _lhsIv
   {-# INLINE rule49 #-}
   rule49 = \ ((_lhsIvisitDescr) :: Map Vertex ChildVisit) ->
     _lhsIvisitDescr
   {-# INLINE rule50 #-}
   rule50 = \ ((_lhsIvssGraph) :: Graph) ->
     _lhsIvssGraph
   {-# INLINE rule51 #-}
   rule51 = \ ((_lhsIallInters) :: CInterfaceMap) ->
     _lhsIallInters
   {-# INLINE rule52 #-}
   rule52 = \ ((_lhsIddp) :: Graph) ->
     _lhsIddp
   {-# INLINE rule53 #-}
   rule53 = \ ((_lhsIinfo) :: Info) ->
     _lhsIinfo
   {-# INLINE rule54 #-}
   rule54 = \ ((_lhsIprev) :: [Vertex]) ->
     _lhsIprev
   {-# INLINE rule55 #-}
   rule55 = \ ((_hdIv) :: Vertex) ->
     _hdIv
   {-# INLINE rule56 #-}
   rule56 = \ ((_lhsIvisitDescr) :: Map Vertex ChildVisit) ->
     _lhsIvisitDescr
   {-# INLINE rule57 #-}
   rule57 = \ ((_lhsIvssGraph) :: Graph) ->
     _lhsIvssGraph
{-# NOINLINE sem_Interfaces_Nil #-}
sem_Interfaces_Nil ::  T_Interfaces 
sem_Interfaces_Nil  = T_Interfaces (return st8) where
   {-# NOINLINE st8 #-}
   !st8 = let
      v7 :: T_Interfaces_v7 
      v7 = \ !(T_Interfaces_vIn7 _lhsIallInters _lhsIddp _lhsIinfo _lhsIprev _lhsIv _lhsIvisitDescr _lhsIvssGraph) -> ( let
         _lhsOinters :: CInterfaceMap
         _lhsOinters = rule58  ()
         _lhsOvisits :: CVisitsMap
         _lhsOvisits = rule59  ()
         _lhsOdescr :: Seq (Vertex,ChildVisit)
         _lhsOdescr = rule60  ()
         _lhsOedp :: Seq Edge
         _lhsOedp = rule61  ()
         _lhsOfirstvisitvertices :: [Vertex]
         _lhsOfirstvisitvertices = rule62  ()
         _lhsOnewedges :: Seq Edge 
         _lhsOnewedges = rule63  ()
         _lhsOv :: Vertex
         _lhsOv = rule64 _lhsIv
         !__result_ = T_Interfaces_vOut7 _lhsOdescr _lhsOedp _lhsOfirstvisitvertices _lhsOinters _lhsOnewedges _lhsOv _lhsOvisits
         in __result_ )
     in C_Interfaces_s8 v7
   {-# INLINE rule58 #-}
   {-# LINE 388 "./src-ag/InterfacesRules.lag" #-}
   rule58 = \  (_ :: ()) ->
                          {-# LINE 388 "./src-ag/InterfacesRules.lag" #-}
                          Map.empty
                          {-# LINE 591 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule59 #-}
   {-# LINE 389 "./src-ag/InterfacesRules.lag" #-}
   rule59 = \  (_ :: ()) ->
                          {-# LINE 389 "./src-ag/InterfacesRules.lag" #-}
                          Map.empty
                          {-# LINE 597 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule60 #-}
   rule60 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule61 #-}
   rule61 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule62 #-}
   rule62 = \  (_ :: ()) ->
     []
   {-# INLINE rule63 #-}
   rule63 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule64 #-}
   rule64 = \ ((_lhsIv) :: Vertex) ->
     _lhsIv

-- Segment -----------------------------------------------------
-- wrapper
data Inh_Segment  = Inh_Segment { allInters_Inh_Segment :: !(CInterfaceMap), cons_Inh_Segment :: !([ConstructorIdent]), ddp_Inh_Segment :: !(Graph), fromLhs_Inh_Segment :: !([Vertex]), info_Inh_Segment :: !(Info), isFirst_Inh_Segment :: !(Bool), n_Inh_Segment :: !(Int), nextInh_Inh_Segment :: !([Vertex]), nextIntravisits_Inh_Segment :: !([IntraVisit]), nextNewvertices_Inh_Segment :: !([Vertex]), prev_Inh_Segment :: !([Vertex]), v_Inh_Segment :: !(Vertex), visitDescr_Inh_Segment :: !(Map Vertex ChildVisit), vssGraph_Inh_Segment :: !(Graph) }
data Syn_Segment  = Syn_Segment { cvisits_Syn_Segment :: !([CVisit]), descr_Syn_Segment :: !(Seq (Vertex,ChildVisit)), edp_Syn_Segment :: !(Seq Edge), groups_Syn_Segment :: !([([Vertex],[Vertex])]), inh_Syn_Segment :: !([Vertex]), intravisits_Syn_Segment :: !([IntraVisit]), newedges_Syn_Segment :: !(Seq Edge ), newvertices_Syn_Segment :: !([Vertex]), prev_Syn_Segment :: !([Vertex]), seg_Syn_Segment :: !(CSegment), v_Syn_Segment :: !(Vertex), visitss_Syn_Segment :: !([VisitSS]) }
{-# INLINABLE wrap_Segment #-}
wrap_Segment :: T_Segment  -> Inh_Segment  -> (Syn_Segment )
wrap_Segment !(T_Segment act) !(Inh_Segment _lhsIallInters _lhsIcons _lhsIddp _lhsIfromLhs _lhsIinfo _lhsIisFirst _lhsIn _lhsInextInh _lhsInextIntravisits _lhsInextNewvertices _lhsIprev _lhsIv _lhsIvisitDescr _lhsIvssGraph) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_Segment_vIn10 _lhsIallInters _lhsIcons _lhsIddp _lhsIfromLhs _lhsIinfo _lhsIisFirst _lhsIn _lhsInextInh _lhsInextIntravisits _lhsInextNewvertices _lhsIprev _lhsIv _lhsIvisitDescr _lhsIvssGraph
        !(T_Segment_vOut10 _lhsOcvisits _lhsOdescr _lhsOedp _lhsOgroups _lhsOinh _lhsOintravisits _lhsOnewedges _lhsOnewvertices _lhsOprev _lhsOseg _lhsOv _lhsOvisitss) <- return (inv_Segment_s11 sem arg)
        return (Syn_Segment _lhsOcvisits _lhsOdescr _lhsOedp _lhsOgroups _lhsOinh _lhsOintravisits _lhsOnewedges _lhsOnewvertices _lhsOprev _lhsOseg _lhsOv _lhsOvisitss)
   )

-- cata
{-# INLINE sem_Segment #-}
sem_Segment :: Segment  -> T_Segment 
sem_Segment ( Segment !inh_ !syn_ ) = sem_Segment_Segment inh_ syn_

-- semantic domain
newtype T_Segment  = T_Segment {
                               attach_T_Segment :: Identity (T_Segment_s11 )
                               }
newtype T_Segment_s11  = C_Segment_s11 {
                                       inv_Segment_s11 :: (T_Segment_v10 )
                                       }
data T_Segment_s12  = C_Segment_s12
type T_Segment_v10  = (T_Segment_vIn10 ) -> (T_Segment_vOut10 )
data T_Segment_vIn10  = T_Segment_vIn10 (CInterfaceMap) ([ConstructorIdent]) (Graph) ([Vertex]) (Info) (Bool) (Int) ([Vertex]) ([IntraVisit]) ([Vertex]) ([Vertex]) (Vertex) (Map Vertex ChildVisit) (Graph)
data T_Segment_vOut10  = T_Segment_vOut10 ([CVisit]) (Seq (Vertex,ChildVisit)) (Seq Edge) ([([Vertex],[Vertex])]) ([Vertex]) ([IntraVisit]) (Seq Edge ) ([Vertex]) ([Vertex]) (CSegment) (Vertex) ([VisitSS])
{-# NOINLINE sem_Segment_Segment #-}
sem_Segment_Segment :: ([Vertex]) -> ([Vertex]) -> T_Segment 
sem_Segment_Segment !arg_inh_ !arg_syn_ = T_Segment (return st11) where
   {-# NOINLINE st11 #-}
   !st11 = let
      v10 :: T_Segment_v10 
      v10 = \ !(T_Segment_vIn10 _lhsIallInters _lhsIcons _lhsIddp _lhsIfromLhs _lhsIinfo _lhsIisFirst _lhsIn _lhsInextInh _lhsInextIntravisits _lhsInextNewvertices _lhsIprev _lhsIv _lhsIvisitDescr _lhsIvssGraph) -> ( let
         _look :: Vertex -> CRule
         _look = rule65 _lhsIinfo
         _occurAs :: (CRule -> Bool) -> [Vertex] -> [Vertex]
         _occurAs = rule66 _lhsIinfo _look
         _groups :: [([Vertex],[Vertex])]
         _groups = rule67 _lhsIinfo _look _occurAs arg_inh_ arg_syn_
         _v :: Int
         _v = rule68 _groups _lhsIv
         _newvertices = rule69 _lhsIv _v
         _lhsOdescr :: Seq (Vertex,ChildVisit)
         _lhsOdescr = rule70 _groups _lhsIn _look _newvertices
         _attredges = rule71 _groups _newvertices
         _visitedges = rule72 _lhsInextNewvertices _newvertices
         _lhsOnewedges :: Seq Edge 
         _lhsOnewedges = rule73 _attredges _visitedges
         _synOccur = rule74 _lhsIinfo _occurAs arg_syn_
         _vss = rule75 _lhsIcons _lhsIinfo _lhsIvssGraph _synOccur arg_syn_
         _visitss' = rule76 _lhsIprev _vss
         _defined = rule77 _lhsIvisitDescr _visitss
         _lhsOprev :: [Vertex]
         _lhsOprev = rule78 _defined _lhsIprev
         _visitss :: [[Vertex]]
         _visitss = rule79 _lhsIinfo _visitss'
         _fromLhs = rule80 _lhsIfromLhs _occurAs arg_inh_
         _computed = rule81 _lhsIinfo _lhsIvisitDescr _visitss
         _intravisits = rule82 _iv _lhsInextIntravisits _visitss
         _iv = rule83 _computed _fromLhs _lhsIddp
         _lhsOseg :: CSegment
         _lhsOseg = rule84 _inhmap _lhsIprev _lhsIvisitDescr _lhsIvssGraph _synmap
         _inhmap :: Map Identifier Type
         _synmap :: Map Identifier Type
         (_inhmap,_synmap) = rule85 _lhsIinfo arg_inh_ arg_syn_
         _lhsOcvisits :: [CVisit]
         _lhsOcvisits = rule86 _inhmap _intravisits _lhsIallInters _lhsIinfo _lhsIvisitDescr _synmap _visitss
         _lhsOedp :: Seq Edge
         _lhsOedp = rule87 _lhsInextInh arg_inh_ arg_syn_
         _lhsOinh :: [Vertex]
         _lhsOinh = rule88 arg_inh_
         _lhsOgroups :: [([Vertex],[Vertex])]
         _lhsOgroups = rule89 _groups
         _lhsOintravisits :: [IntraVisit]
         _lhsOintravisits = rule90 _intravisits
         _lhsOnewvertices :: [Vertex]
         _lhsOnewvertices = rule91 _newvertices
         _lhsOv :: Vertex
         _lhsOv = rule92 _v
         _lhsOvisitss :: [VisitSS]
         _lhsOvisitss = rule93 _visitss
         !__result_ = T_Segment_vOut10 _lhsOcvisits _lhsOdescr _lhsOedp _lhsOgroups _lhsOinh _lhsOintravisits _lhsOnewedges _lhsOnewvertices _lhsOprev _lhsOseg _lhsOv _lhsOvisitss
         in __result_ )
     in C_Segment_s11 v10
   {-# INLINE rule65 #-}
   {-# LINE 101 "./src-ag/InterfacesRules.lag" #-}
   rule65 = \ ((_lhsIinfo) :: Info) ->
                           {-# LINE 101 "./src-ag/InterfacesRules.lag" #-}
                           \a -> ruleTable _lhsIinfo ! a
                           {-# LINE 707 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule66 #-}
   {-# LINE 104 "./src-ag/InterfacesRules.lag" #-}
   rule66 = \ ((_lhsIinfo) :: Info) ((_look) :: Vertex -> CRule) ->
                              {-# LINE 104 "./src-ag/InterfacesRules.lag" #-}
                              \p us -> [ a  |  u <- us
                                            ,  a <- tdsToTdp _lhsIinfo ! u
                                            ,  p (_look a)]
                              {-# LINE 715 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule67 #-}
   {-# LINE 108 "./src-ag/InterfacesRules.lag" #-}
   rule67 = \ ((_lhsIinfo) :: Info) ((_look) :: Vertex -> CRule) ((_occurAs) :: (CRule -> Bool) -> [Vertex] -> [Vertex]) inh_ syn_ ->
                              {-# LINE 108 "./src-ag/InterfacesRules.lag" #-}
                              let group as = gather _lhsIinfo (_occurAs isRhs as)
                              in map (partition (isInh . _look)) (group (inh_ ++ syn_))
                              {-# LINE 722 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule68 #-}
   {-# LINE 111 "./src-ag/InterfacesRules.lag" #-}
   rule68 = \ ((_groups) :: [([Vertex],[Vertex])]) ((_lhsIv) :: Vertex) ->
                        {-# LINE 111 "./src-ag/InterfacesRules.lag" #-}
                        _lhsIv + length _groups
                        {-# LINE 728 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule69 #-}
   {-# LINE 112 "./src-ag/InterfacesRules.lag" #-}
   rule69 = \ ((_lhsIv) :: Vertex) ((_v) :: Int) ->
                                  {-# LINE 112 "./src-ag/InterfacesRules.lag" #-}
                                  [_lhsIv .. _v    -1]
                                  {-# LINE 734 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule70 #-}
   {-# LINE 127 "./src-ag/InterfacesRules.lag" #-}
   rule70 = \ ((_groups) :: [([Vertex],[Vertex])]) ((_lhsIn) :: Int) ((_look) :: Vertex -> CRule) _newvertices ->
                            {-# LINE 127 "./src-ag/InterfacesRules.lag" #-}
                            Seq.fromList $ zipWith (cv _look _lhsIn) _newvertices _groups
                            {-# LINE 740 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule71 #-}
   {-# LINE 150 "./src-ag/InterfacesRules.lag" #-}
   rule71 = \ ((_groups) :: [([Vertex],[Vertex])]) _newvertices ->
                               {-# LINE 150 "./src-ag/InterfacesRules.lag" #-}
                               concat (zipWith ed _newvertices _groups)
                               {-# LINE 746 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule72 #-}
   {-# LINE 170 "./src-ag/InterfacesRules.lag" #-}
   rule72 = \ ((_lhsInextNewvertices) :: [Vertex]) _newvertices ->
                                 {-# LINE 170 "./src-ag/InterfacesRules.lag" #-}
                                 zip _newvertices _lhsInextNewvertices
                                 {-# LINE 752 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule73 #-}
   {-# LINE 171 "./src-ag/InterfacesRules.lag" #-}
   rule73 = \ _attredges _visitedges ->
                               {-# LINE 171 "./src-ag/InterfacesRules.lag" #-}
                               Seq.fromList _attredges Seq.>< Seq.fromList _visitedges
                               {-# LINE 758 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule74 #-}
   {-# LINE 225 "./src-ag/InterfacesRules.lag" #-}
   rule74 = \ ((_lhsIinfo) :: Info) ((_occurAs) :: (CRule -> Bool) -> [Vertex] -> [Vertex]) syn_ ->
                               {-# LINE 225 "./src-ag/InterfacesRules.lag" #-}
                               gather _lhsIinfo (_occurAs isLhs syn_)
                               {-# LINE 764 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule75 #-}
   {-# LINE 226 "./src-ag/InterfacesRules.lag" #-}
   rule75 = \ ((_lhsIcons) :: [ConstructorIdent]) ((_lhsIinfo) :: Info) ((_lhsIvssGraph) :: Graph) _synOccur syn_ ->
                           {-# LINE 226 "./src-ag/InterfacesRules.lag" #-}
                           let hasCode' v | inRange (bounds (ruleTable _lhsIinfo)) v =  getHasCode (ruleTable _lhsIinfo ! v)
                                          | otherwise = True
                           in if  null syn_
                                  then replicate (length _lhsIcons) []
                                  else map (filter hasCode' . topSort' _lhsIvssGraph) _synOccur
                           {-# LINE 774 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule76 #-}
   {-# LINE 270 "./src-ag/InterfacesRules.lag" #-}
   rule76 = \ ((_lhsIprev) :: [Vertex]) _vss ->
                               {-# LINE 270 "./src-ag/InterfacesRules.lag" #-}
                               map (\\ _lhsIprev) _vss
                               {-# LINE 780 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule77 #-}
   {-# LINE 271 "./src-ag/InterfacesRules.lag" #-}
   rule77 = \ ((_lhsIvisitDescr) :: Map Vertex ChildVisit) ((_visitss) :: [[Vertex]]) ->
                               {-# LINE 271 "./src-ag/InterfacesRules.lag" #-}
                               let defines v = case Map.lookup v _lhsIvisitDescr of
                                                 Nothing -> [v]
                                                 Just (ChildVisit _ _ _ inh _) -> v:inh
                               in concatMap (concatMap defines) _visitss
                               {-# LINE 789 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule78 #-}
   {-# LINE 275 "./src-ag/InterfacesRules.lag" #-}
   rule78 = \ _defined ((_lhsIprev) :: [Vertex]) ->
                           {-# LINE 275 "./src-ag/InterfacesRules.lag" #-}
                           _lhsIprev ++ _defined
                           {-# LINE 795 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule79 #-}
   {-# LINE 284 "./src-ag/InterfacesRules.lag" #-}
   rule79 = \ ((_lhsIinfo) :: Info) _visitss' ->
                              {-# LINE 284 "./src-ag/InterfacesRules.lag" #-}
                              let  rem' :: [(Identifier,Identifier,Maybe Type)] -> [Vertex] -> [Vertex]
                                   rem' _ [] = []
                                   rem' prev (v:vs)
                                     | inRange (bounds table) v
                                         = let  cr = table ! v
                                                addV = case findIndex cmp prev of
                                                         Just _ -> id
                                                         _      -> (v:)
                                                cmp (fld,attr,tp) = getField cr == fld && getAttr cr == attr && sameNT (getType cr) tp
                                                sameNT (Just (NT ntA _ _)) (Just (NT ntB _ _)) = ntA == ntB
                                                sameNT _          _                            = False
                                                def = Map.elems (getDefines cr)
                                           in addV (rem' (def ++ prev) vs)
                                     | otherwise = v:rem' prev vs
                                   table = ruleTable _lhsIinfo
                              in map (rem' []) _visitss'
                              {-# LINE 816 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule80 #-}
   {-# LINE 357 "./src-ag/InterfacesRules.lag" #-}
   rule80 = \ ((_lhsIfromLhs) :: [Vertex]) ((_occurAs) :: (CRule -> Bool) -> [Vertex] -> [Vertex]) inh_ ->
                              {-# LINE 357 "./src-ag/InterfacesRules.lag" #-}
                              _occurAs isLhs inh_ ++ _lhsIfromLhs
                              {-# LINE 822 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule81 #-}
   {-# LINE 358 "./src-ag/InterfacesRules.lag" #-}
   rule81 = \ ((_lhsIinfo) :: Info) ((_lhsIvisitDescr) :: Map Vertex ChildVisit) ((_visitss) :: [[Vertex]]) ->
                                {-# LINE 358 "./src-ag/InterfacesRules.lag" #-}
                                let computes v = case Map.lookup v _lhsIvisitDescr of
                                                   Nothing -> Map.keys (getDefines (ruleTable _lhsIinfo ! v))
                                                   Just (ChildVisit _ _ _ _ syn) -> v:syn
                                in concatMap (concatMap computes) _visitss
                                {-# LINE 831 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule82 #-}
   {-# LINE 362 "./src-ag/InterfacesRules.lag" #-}
   rule82 = \ _iv ((_lhsInextIntravisits) :: [IntraVisit]) ((_visitss) :: [[Vertex]]) ->
                                  {-# LINE 362 "./src-ag/InterfacesRules.lag" #-}
                                  zipWith _iv _visitss _lhsInextIntravisits
                                  {-# LINE 837 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule83 #-}
   {-# LINE 363 "./src-ag/InterfacesRules.lag" #-}
   rule83 = \ _computed _fromLhs ((_lhsIddp) :: Graph) ->
                          {-# LINE 363 "./src-ag/InterfacesRules.lag" #-}
                          \vs next ->
                            let needed = concatMap (_lhsIddp !) vs
                            in nub (needed ++ next) \\ (_fromLhs ++ _computed)
                          {-# LINE 845 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule84 #-}
   {-# LINE 406 "./src-ag/InterfacesRules.lag" #-}
   rule84 = \ ((_inhmap) :: Map Identifier Type) ((_lhsIprev) :: [Vertex]) ((_lhsIvisitDescr) :: Map Vertex ChildVisit) ((_lhsIvssGraph) :: Graph) ((_synmap) :: Map Identifier Type) ->
                          {-# LINE 406 "./src-ag/InterfacesRules.lag" #-}
                          if False then undefined _lhsIvssGraph _lhsIvisitDescr _lhsIprev else CSegment _inhmap _synmap
                          {-# LINE 851 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule85 #-}
   {-# LINE 410 "./src-ag/InterfacesRules.lag" #-}
   rule85 = \ ((_lhsIinfo) :: Info) inh_ syn_ ->
                                      {-# LINE 410 "./src-ag/InterfacesRules.lag" #-}
                                      let makemap = Map.fromList . map findType
                                          findType v = getNtaNameType (attrTable _lhsIinfo ! v)
                                      in (makemap inh_,makemap syn_)
                                      {-# LINE 859 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule86 #-}
   {-# LINE 413 "./src-ag/InterfacesRules.lag" #-}
   rule86 = \ ((_inhmap) :: Map Identifier Type) _intravisits ((_lhsIallInters) :: CInterfaceMap) ((_lhsIinfo) :: Info) ((_lhsIvisitDescr) :: Map Vertex ChildVisit) ((_synmap) :: Map Identifier Type) ((_visitss) :: [[Vertex]]) ->
                              {-# LINE 413 "./src-ag/InterfacesRules.lag" #-}
                              let  mkVisit vss intra = CVisit _inhmap _synmap (mkSequence vss) (mkSequence intra) True
                                   mkSequence = map mkRule
                                   mkRule v = case Map.lookup v _lhsIvisitDescr of
                                                Nothing -> ruleTable _lhsIinfo ! v
                                                Just (ChildVisit name nt n _ _) -> ccv name nt n _lhsIallInters
                              in zipWith mkVisit _visitss _intravisits
                              {-# LINE 870 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule87 #-}
   {-# LINE 440 "./src-ag/InterfacesRules.lag" #-}
   rule87 = \ ((_lhsInextInh) :: [Vertex]) inh_ syn_ ->
                          {-# LINE 440 "./src-ag/InterfacesRules.lag" #-}
                          Seq.fromList [(i,s) | i <- inh_, s <- syn_]
                          Seq.>< Seq.fromList [(s,i) | s <- syn_, i <- _lhsInextInh ]
                          {-# LINE 877 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule88 #-}
   {-# LINE 445 "./src-ag/InterfacesRules.lag" #-}
   rule88 = \ inh_ ->
                         {-# LINE 445 "./src-ag/InterfacesRules.lag" #-}
                         inh_
                         {-# LINE 883 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule89 #-}
   rule89 = \ ((_groups) :: [([Vertex],[Vertex])]) ->
     _groups
   {-# INLINE rule90 #-}
   rule90 = \ _intravisits ->
     _intravisits
   {-# INLINE rule91 #-}
   rule91 = \ _newvertices ->
     _newvertices
   {-# INLINE rule92 #-}
   rule92 = \ ((_v) :: Int) ->
     _v
   {-# INLINE rule93 #-}
   rule93 = \ ((_visitss) :: [[Vertex]]) ->
     _visitss

-- Segments ----------------------------------------------------
-- wrapper
data Inh_Segments  = Inh_Segments { allInters_Inh_Segments :: !(CInterfaceMap), cons_Inh_Segments :: !([ConstructorIdent]), ddp_Inh_Segments :: !(Graph), fromLhs_Inh_Segments :: !([Vertex]), info_Inh_Segments :: !(Info), isFirst_Inh_Segments :: !(Bool), n_Inh_Segments :: !(Int), prev_Inh_Segments :: !([Vertex]), v_Inh_Segments :: !(Vertex), visitDescr_Inh_Segments :: !(Map Vertex ChildVisit), vssGraph_Inh_Segments :: !(Graph) }
data Syn_Segments  = Syn_Segments { cvisits_Syn_Segments :: !([[CVisit]]), descr_Syn_Segments :: !(Seq (Vertex,ChildVisit)), edp_Syn_Segments :: !(Seq Edge), firstInh_Syn_Segments :: !([Vertex]), groups_Syn_Segments :: !([([Vertex],[Vertex])]), hdIntravisits_Syn_Segments :: !([IntraVisit]), newedges_Syn_Segments :: !(Seq Edge ), newvertices_Syn_Segments :: !([Vertex]), prev_Syn_Segments :: !([Vertex]), segs_Syn_Segments :: !(CSegments), v_Syn_Segments :: !(Vertex) }
{-# INLINABLE wrap_Segments #-}
wrap_Segments :: T_Segments  -> Inh_Segments  -> (Syn_Segments )
wrap_Segments !(T_Segments act) !(Inh_Segments _lhsIallInters _lhsIcons _lhsIddp _lhsIfromLhs _lhsIinfo _lhsIisFirst _lhsIn _lhsIprev _lhsIv _lhsIvisitDescr _lhsIvssGraph) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_Segments_vIn13 _lhsIallInters _lhsIcons _lhsIddp _lhsIfromLhs _lhsIinfo _lhsIisFirst _lhsIn _lhsIprev _lhsIv _lhsIvisitDescr _lhsIvssGraph
        !(T_Segments_vOut13 _lhsOcvisits _lhsOdescr _lhsOedp _lhsOfirstInh _lhsOgroups _lhsOhdIntravisits _lhsOnewedges _lhsOnewvertices _lhsOprev _lhsOsegs _lhsOv) <- return (inv_Segments_s14 sem arg)
        return (Syn_Segments _lhsOcvisits _lhsOdescr _lhsOedp _lhsOfirstInh _lhsOgroups _lhsOhdIntravisits _lhsOnewedges _lhsOnewvertices _lhsOprev _lhsOsegs _lhsOv)
   )

-- cata
{-# NOINLINE sem_Segments #-}
sem_Segments :: Segments  -> T_Segments 
sem_Segments list = Prelude.foldr sem_Segments_Cons sem_Segments_Nil (Prelude.map sem_Segment list)

-- semantic domain
newtype T_Segments  = T_Segments {
                                 attach_T_Segments :: Identity (T_Segments_s14 )
                                 }
newtype T_Segments_s14  = C_Segments_s14 {
                                         inv_Segments_s14 :: (T_Segments_v13 )
                                         }
data T_Segments_s15  = C_Segments_s15
type T_Segments_v13  = (T_Segments_vIn13 ) -> (T_Segments_vOut13 )
data T_Segments_vIn13  = T_Segments_vIn13 (CInterfaceMap) ([ConstructorIdent]) (Graph) ([Vertex]) (Info) (Bool) (Int) ([Vertex]) (Vertex) (Map Vertex ChildVisit) (Graph)
data T_Segments_vOut13  = T_Segments_vOut13 ([[CVisit]]) (Seq (Vertex,ChildVisit)) (Seq Edge) ([Vertex]) ([([Vertex],[Vertex])]) ([IntraVisit]) (Seq Edge ) ([Vertex]) ([Vertex]) (CSegments) (Vertex)
{-# NOINLINE sem_Segments_Cons #-}
sem_Segments_Cons :: T_Segment  -> T_Segments  -> T_Segments 
sem_Segments_Cons arg_hd_ arg_tl_ = T_Segments (return st14) where
   {-# NOINLINE st14 #-}
   !st14 = let
      v13 :: T_Segments_v13 
      v13 = \ !(T_Segments_vIn13 _lhsIallInters _lhsIcons _lhsIddp _lhsIfromLhs _lhsIinfo _lhsIisFirst _lhsIn _lhsIprev _lhsIv _lhsIvisitDescr _lhsIvssGraph) -> ( let
         _hdX11 = Control.Monad.Identity.runIdentity (attach_T_Segment (arg_hd_))
         _tlX14 = Control.Monad.Identity.runIdentity (attach_T_Segments (arg_tl_))
         (T_Segment_vOut10 _hdIcvisits _hdIdescr _hdIedp _hdIgroups _hdIinh _hdIintravisits _hdInewedges _hdInewvertices _hdIprev _hdIseg _hdIv _hdIvisitss) = inv_Segment_s11 _hdX11 (T_Segment_vIn10 _hdOallInters _hdOcons _hdOddp _hdOfromLhs _hdOinfo _hdOisFirst _hdOn _hdOnextInh _hdOnextIntravisits _hdOnextNewvertices _hdOprev _hdOv _hdOvisitDescr _hdOvssGraph)
         (T_Segments_vOut13 _tlIcvisits _tlIdescr _tlIedp _tlIfirstInh _tlIgroups _tlIhdIntravisits _tlInewedges _tlInewvertices _tlIprev _tlIsegs _tlIv) = inv_Segments_s14 _tlX14 (T_Segments_vIn13 _tlOallInters _tlOcons _tlOddp _tlOfromLhs _tlOinfo _tlOisFirst _tlOn _tlOprev _tlOv _tlOvisitDescr _tlOvssGraph)
         _hdOnextNewvertices = rule94 _tlInewvertices
         _lhsOnewvertices :: [Vertex]
         _lhsOnewvertices = rule95 _hdInewvertices
         _lhsOgroups :: [([Vertex],[Vertex])]
         _lhsOgroups = rule96 _hdIgroups
         _tlOn = rule97 _lhsIn
         _tlOisFirst = rule98  ()
         _hdOnextIntravisits = rule99 _tlIhdIntravisits
         _lhsOhdIntravisits :: [IntraVisit]
         _lhsOhdIntravisits = rule100 _hdIintravisits
         _hdOfromLhs = rule101 _lhsIfromLhs
         _tlOfromLhs = rule102  ()
         _lhsOsegs :: CSegments
         _lhsOsegs = rule103 _hdIseg _tlIsegs
         _hdOnextInh = rule104 _tlIfirstInh
         _lhsOfirstInh :: [Vertex]
         _lhsOfirstInh = rule105 _hdIinh
         _lhsOcvisits :: [[CVisit]]
         _lhsOcvisits = rule106 _hdIcvisits _tlIcvisits
         _lhsOdescr :: Seq (Vertex,ChildVisit)
         _lhsOdescr = rule107 _hdIdescr _tlIdescr
         _lhsOedp :: Seq Edge
         _lhsOedp = rule108 _hdIedp _tlIedp
         _lhsOnewedges :: Seq Edge 
         _lhsOnewedges = rule109 _hdInewedges _tlInewedges
         _lhsOprev :: [Vertex]
         _lhsOprev = rule110 _tlIprev
         _lhsOv :: Vertex
         _lhsOv = rule111 _tlIv
         _hdOallInters = rule112 _lhsIallInters
         _hdOcons = rule113 _lhsIcons
         _hdOddp = rule114 _lhsIddp
         _hdOinfo = rule115 _lhsIinfo
         _hdOisFirst = rule116 _lhsIisFirst
         _hdOn = rule117 _lhsIn
         _hdOprev = rule118 _lhsIprev
         _hdOv = rule119 _lhsIv
         _hdOvisitDescr = rule120 _lhsIvisitDescr
         _hdOvssGraph = rule121 _lhsIvssGraph
         _tlOallInters = rule122 _lhsIallInters
         _tlOcons = rule123 _lhsIcons
         _tlOddp = rule124 _lhsIddp
         _tlOinfo = rule125 _lhsIinfo
         _tlOprev = rule126 _hdIprev
         _tlOv = rule127 _hdIv
         _tlOvisitDescr = rule128 _lhsIvisitDescr
         _tlOvssGraph = rule129 _lhsIvssGraph
         !__result_ = T_Segments_vOut13 _lhsOcvisits _lhsOdescr _lhsOedp _lhsOfirstInh _lhsOgroups _lhsOhdIntravisits _lhsOnewedges _lhsOnewvertices _lhsOprev _lhsOsegs _lhsOv
         in __result_ )
     in C_Segments_s14 v13
   {-# INLINE rule94 #-}
   {-# LINE 165 "./src-ag/InterfacesRules.lag" #-}
   rule94 = \ ((_tlInewvertices) :: [Vertex]) ->
                                  {-# LINE 165 "./src-ag/InterfacesRules.lag" #-}
                                  _tlInewvertices
                                  {-# LINE 996 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule95 #-}
   {-# LINE 166 "./src-ag/InterfacesRules.lag" #-}
   rule95 = \ ((_hdInewvertices) :: [Vertex]) ->
                               {-# LINE 166 "./src-ag/InterfacesRules.lag" #-}
                               _hdInewvertices
                               {-# LINE 1002 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule96 #-}
   {-# LINE 180 "./src-ag/InterfacesRules.lag" #-}
   rule96 = \ ((_hdIgroups) :: [([Vertex],[Vertex])]) ->
                         {-# LINE 180 "./src-ag/InterfacesRules.lag" #-}
                         _hdIgroups
                         {-# LINE 1008 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule97 #-}
   {-# LINE 203 "./src-ag/InterfacesRules.lag" #-}
   rule97 = \ ((_lhsIn) :: Int) ->
                   {-# LINE 203 "./src-ag/InterfacesRules.lag" #-}
                   _lhsIn + 1
                   {-# LINE 1014 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule98 #-}
   {-# LINE 316 "./src-ag/InterfacesRules.lag" #-}
   rule98 = \  (_ :: ()) ->
                         {-# LINE 316 "./src-ag/InterfacesRules.lag" #-}
                         False
                         {-# LINE 1020 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule99 #-}
   {-# LINE 329 "./src-ag/InterfacesRules.lag" #-}
   rule99 = \ ((_tlIhdIntravisits) :: [IntraVisit]) ->
                                  {-# LINE 329 "./src-ag/InterfacesRules.lag" #-}
                                  _tlIhdIntravisits
                                  {-# LINE 1026 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule100 #-}
   {-# LINE 330 "./src-ag/InterfacesRules.lag" #-}
   rule100 = \ ((_hdIintravisits) :: [IntraVisit]) ->
                                 {-# LINE 330 "./src-ag/InterfacesRules.lag" #-}
                                 _hdIintravisits
                                 {-# LINE 1032 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule101 #-}
   {-# LINE 354 "./src-ag/InterfacesRules.lag" #-}
   rule101 = \ ((_lhsIfromLhs) :: [Vertex]) ->
                          {-# LINE 354 "./src-ag/InterfacesRules.lag" #-}
                          _lhsIfromLhs
                          {-# LINE 1038 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule102 #-}
   {-# LINE 355 "./src-ag/InterfacesRules.lag" #-}
   rule102 = \  (_ :: ()) ->
                          {-# LINE 355 "./src-ag/InterfacesRules.lag" #-}
                          []
                          {-# LINE 1044 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule103 #-}
   {-# LINE 401 "./src-ag/InterfacesRules.lag" #-}
   rule103 = \ ((_hdIseg) :: CSegment) ((_tlIsegs) :: CSegments) ->
                        {-# LINE 401 "./src-ag/InterfacesRules.lag" #-}
                        _hdIseg : _tlIsegs
                        {-# LINE 1050 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule104 #-}
   {-# LINE 447 "./src-ag/InterfacesRules.lag" #-}
   rule104 = \ ((_tlIfirstInh) :: [Vertex]) ->
                          {-# LINE 447 "./src-ag/InterfacesRules.lag" #-}
                          _tlIfirstInh
                          {-# LINE 1056 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule105 #-}
   {-# LINE 448 "./src-ag/InterfacesRules.lag" #-}
   rule105 = \ ((_hdIinh) :: [Vertex]) ->
                            {-# LINE 448 "./src-ag/InterfacesRules.lag" #-}
                            _hdIinh
                            {-# LINE 1062 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule106 #-}
   rule106 = \ ((_hdIcvisits) :: [CVisit]) ((_tlIcvisits) :: [[CVisit]]) ->
     _hdIcvisits : _tlIcvisits
   {-# INLINE rule107 #-}
   rule107 = \ ((_hdIdescr) :: Seq (Vertex,ChildVisit)) ((_tlIdescr) :: Seq (Vertex,ChildVisit)) ->
     _hdIdescr Seq.>< _tlIdescr
   {-# INLINE rule108 #-}
   rule108 = \ ((_hdIedp) :: Seq Edge) ((_tlIedp) :: Seq Edge) ->
     _hdIedp Seq.>< _tlIedp
   {-# INLINE rule109 #-}
   rule109 = \ ((_hdInewedges) :: Seq Edge ) ((_tlInewedges) :: Seq Edge ) ->
     _hdInewedges Seq.>< _tlInewedges
   {-# INLINE rule110 #-}
   rule110 = \ ((_tlIprev) :: [Vertex]) ->
     _tlIprev
   {-# INLINE rule111 #-}
   rule111 = \ ((_tlIv) :: Vertex) ->
     _tlIv
   {-# INLINE rule112 #-}
   rule112 = \ ((_lhsIallInters) :: CInterfaceMap) ->
     _lhsIallInters
   {-# INLINE rule113 #-}
   rule113 = \ ((_lhsIcons) :: [ConstructorIdent]) ->
     _lhsIcons
   {-# INLINE rule114 #-}
   rule114 = \ ((_lhsIddp) :: Graph) ->
     _lhsIddp
   {-# INLINE rule115 #-}
   rule115 = \ ((_lhsIinfo) :: Info) ->
     _lhsIinfo
   {-# INLINE rule116 #-}
   rule116 = \ ((_lhsIisFirst) :: Bool) ->
     _lhsIisFirst
   {-# INLINE rule117 #-}
   rule117 = \ ((_lhsIn) :: Int) ->
     _lhsIn
   {-# INLINE rule118 #-}
   rule118 = \ ((_lhsIprev) :: [Vertex]) ->
     _lhsIprev
   {-# INLINE rule119 #-}
   rule119 = \ ((_lhsIv) :: Vertex) ->
     _lhsIv
   {-# INLINE rule120 #-}
   rule120 = \ ((_lhsIvisitDescr) :: Map Vertex ChildVisit) ->
     _lhsIvisitDescr
   {-# INLINE rule121 #-}
   rule121 = \ ((_lhsIvssGraph) :: Graph) ->
     _lhsIvssGraph
   {-# INLINE rule122 #-}
   rule122 = \ ((_lhsIallInters) :: CInterfaceMap) ->
     _lhsIallInters
   {-# INLINE rule123 #-}
   rule123 = \ ((_lhsIcons) :: [ConstructorIdent]) ->
     _lhsIcons
   {-# INLINE rule124 #-}
   rule124 = \ ((_lhsIddp) :: Graph) ->
     _lhsIddp
   {-# INLINE rule125 #-}
   rule125 = \ ((_lhsIinfo) :: Info) ->
     _lhsIinfo
   {-# INLINE rule126 #-}
   rule126 = \ ((_hdIprev) :: [Vertex]) ->
     _hdIprev
   {-# INLINE rule127 #-}
   rule127 = \ ((_hdIv) :: Vertex) ->
     _hdIv
   {-# INLINE rule128 #-}
   rule128 = \ ((_lhsIvisitDescr) :: Map Vertex ChildVisit) ->
     _lhsIvisitDescr
   {-# INLINE rule129 #-}
   rule129 = \ ((_lhsIvssGraph) :: Graph) ->
     _lhsIvssGraph
{-# NOINLINE sem_Segments_Nil #-}
sem_Segments_Nil ::  T_Segments 
sem_Segments_Nil  = T_Segments (return st14) where
   {-# NOINLINE st14 #-}
   !st14 = let
      v13 :: T_Segments_v13 
      v13 = \ !(T_Segments_vIn13 _lhsIallInters _lhsIcons _lhsIddp _lhsIfromLhs _lhsIinfo _lhsIisFirst _lhsIn _lhsIprev _lhsIv _lhsIvisitDescr _lhsIvssGraph) -> ( let
         _lhsOnewvertices :: [Vertex]
         _lhsOnewvertices = rule130  ()
         _lhsOgroups :: [([Vertex],[Vertex])]
         _lhsOgroups = rule131  ()
         _lhsOhdIntravisits :: [IntraVisit]
         _lhsOhdIntravisits = rule132  ()
         _lhsOsegs :: CSegments
         _lhsOsegs = rule133  ()
         _lhsOfirstInh :: [Vertex]
         _lhsOfirstInh = rule134  ()
         _lhsOcvisits :: [[CVisit]]
         _lhsOcvisits = rule135  ()
         _lhsOdescr :: Seq (Vertex,ChildVisit)
         _lhsOdescr = rule136  ()
         _lhsOedp :: Seq Edge
         _lhsOedp = rule137  ()
         _lhsOnewedges :: Seq Edge 
         _lhsOnewedges = rule138  ()
         _lhsOprev :: [Vertex]
         _lhsOprev = rule139 _lhsIprev
         _lhsOv :: Vertex
         _lhsOv = rule140 _lhsIv
         !__result_ = T_Segments_vOut13 _lhsOcvisits _lhsOdescr _lhsOedp _lhsOfirstInh _lhsOgroups _lhsOhdIntravisits _lhsOnewedges _lhsOnewvertices _lhsOprev _lhsOsegs _lhsOv
         in __result_ )
     in C_Segments_s14 v13
   {-# INLINE rule130 #-}
   {-# LINE 167 "./src-ag/InterfacesRules.lag" #-}
   rule130 = \  (_ :: ()) ->
                               {-# LINE 167 "./src-ag/InterfacesRules.lag" #-}
                               []
                               {-# LINE 1172 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule131 #-}
   {-# LINE 181 "./src-ag/InterfacesRules.lag" #-}
   rule131 = \  (_ :: ()) ->
                         {-# LINE 181 "./src-ag/InterfacesRules.lag" #-}
                         []
                         {-# LINE 1178 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule132 #-}
   {-# LINE 331 "./src-ag/InterfacesRules.lag" #-}
   rule132 = \  (_ :: ()) ->
                               {-# LINE 331 "./src-ag/InterfacesRules.lag" #-}
                               repeat []
                               {-# LINE 1184 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule133 #-}
   {-# LINE 402 "./src-ag/InterfacesRules.lag" #-}
   rule133 = \  (_ :: ()) ->
                        {-# LINE 402 "./src-ag/InterfacesRules.lag" #-}
                        []
                        {-# LINE 1190 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule134 #-}
   {-# LINE 449 "./src-ag/InterfacesRules.lag" #-}
   rule134 = \  (_ :: ()) ->
                           {-# LINE 449 "./src-ag/InterfacesRules.lag" #-}
                           []
                           {-# LINE 1196 "dist/build/InterfacesRules.hs"#-}
   {-# INLINE rule135 #-}
   rule135 = \  (_ :: ()) ->
     []
   {-# INLINE rule136 #-}
   rule136 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule137 #-}
   rule137 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule138 #-}
   rule138 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule139 #-}
   rule139 = \ ((_lhsIprev) :: [Vertex]) ->
     _lhsIprev
   {-# INLINE rule140 #-}
   rule140 = \ ((_lhsIv) :: Vertex) ->
     _lhsIv
