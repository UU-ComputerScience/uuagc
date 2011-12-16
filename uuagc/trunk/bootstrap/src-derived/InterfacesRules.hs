{-# OPTIONS_GHC -XBangPatterns #-}

-- UUAGC 0.9.40.1 (src-ag/InterfacesRules.lag)
module InterfacesRules where
{-# LINE 10 "src-ag/InterfacesRules.lag" #-}

import Interfaces
import SequentialTypes
import CodeSyntax
import GrammarInfo

import qualified Data.Sequence as Seq
import Data.Sequence(Seq)
import qualified Data.Map as Map
import Data.Map(Map)
import qualified Data.Set as Set
import Data.Set(Set)
import Data.Tree(Tree(Node))
import Data.Graph(Graph, dfs, edges, buildG, transposeG)
import Data.Maybe (fromJust)
import Data.List (partition,transpose,(\\),nub,intersect, findIndex)
import Data.Array ((!),inRange,bounds,assocs)
import Data.Foldable(toList)

import Debug.Trace(trace)
{-# LINE 27 "dist/build/InterfacesRules.hs" #-}

{-# LINE 2 "src-ag/Interfaces.ag" #-}

import CommonTypes
import SequentialTypes
{-# LINE 33 "dist/build/InterfacesRules.hs" #-}
{-# LINE 58 "src-ag/InterfacesRules.lag" #-}

type VisitSS = [Vertex]
{-# LINE 37 "dist/build/InterfacesRules.hs" #-}

{-# LINE 93 "src-ag/InterfacesRules.lag" #-}

gather :: Info -> [Vertex] -> [[Vertex]]
gather info =  eqClasses comp
               where comp a b = isEqualField (ruleTable info ! a) (ruleTable info ! b)
{-# LINE 44 "dist/build/InterfacesRules.hs" #-}

{-# LINE 134 "src-ag/InterfacesRules.lag" #-}

-- Only non-empty syn will ever be forced, because visits with empty syn are never performed
-- Right hand side synthesized attributes always have a field
cv :: (Vertex -> CRule) -> Int -> Vertex -> ([Vertex],[Vertex]) -> (Vertex,ChildVisit)
cv look n v (inh,syn) =  let  fld = getField (look (head syn))
                              rnt = fromJust (getRhsNt (look (head syn)))
                              d = ChildVisit fld rnt n inh syn
                         in (v,d)
{-# LINE 55 "dist/build/InterfacesRules.hs" #-}

{-# LINE 157 "src-ag/InterfacesRules.lag" #-}

ed v (inh,syn) = map (\i -> (i,v)) inh ++ map (\s -> (v,s)) syn
{-# LINE 60 "dist/build/InterfacesRules.hs" #-}

{-# LINE 244 "src-ag/InterfacesRules.lag" #-}

postorder (Node a ts) = postorderF ts ++ [a]
postorderF = concatMap postorder
postOrd g = postorderF . dfs g
topSort' g = postOrd g
{-# LINE 68 "dist/build/InterfacesRules.hs" #-}

{-# LINE 323 "src-ag/InterfacesRules.lag" #-}

type IntraVisit = [Vertex]
{-# LINE 73 "dist/build/InterfacesRules.hs" #-}

{-# LINE 345 "src-ag/InterfacesRules.lag" #-}

swap (a,b) = (b,a)
{-# LINE 78 "dist/build/InterfacesRules.hs" #-}

{-# LINE 419 "src-ag/InterfacesRules.lag" #-}

ccv :: Identifier -> NontermIdent -> Int -> CInterfaceMap -> CRule
ccv name nt n table
  =  CChildVisit name nt n inh syn last
     where  CInterface segs = Map.findWithDefault (error ("InterfacesRules::ccv::interfaces not in table for nt: " ++ show nt)) nt table
            (seg:remain) = drop n segs
            CSegment inh syn = seg
            last = null remain
{-# LINE 89 "dist/build/InterfacesRules.hs" #-}
-- IRoot -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         dpr                  : [Edge]
         info                 : Info
         tdp                  : Graph
      synthesized attributes:
         edp                  : [Edge]
         inters               : CInterfaceMap
         visits               : CVisitsMap
   alternatives:
      alternative IRoot:
         child inters         : Interfaces 
         visit 0:
            local newedges    : _
            local visitssGraph : _
            local descr       : _
-}
-- cata
sem_IRoot :: IRoot ->
             T_IRoot
sem_IRoot !(IRoot _inters) =
    (sem_IRoot_IRoot (sem_Interfaces _inters))
-- semantic domain
newtype T_IRoot = T_IRoot (([Edge]) ->
                           Info ->
                           Graph ->
                           ( ([Edge]),CInterfaceMap,CVisitsMap))
data Inh_IRoot = Inh_IRoot {dpr_Inh_IRoot :: !(([Edge])),info_Inh_IRoot :: !(Info),tdp_Inh_IRoot :: !(Graph)}
data Syn_IRoot = Syn_IRoot {edp_Syn_IRoot :: !(([Edge])),inters_Syn_IRoot :: !(CInterfaceMap),visits_Syn_IRoot :: !(CVisitsMap)}
wrap_IRoot :: T_IRoot ->
              Inh_IRoot ->
              Syn_IRoot
wrap_IRoot !(T_IRoot sem) !(Inh_IRoot _lhsIdpr _lhsIinfo _lhsItdp) =
    (let ( !_lhsOedp,!_lhsOinters,!_lhsOvisits) = sem _lhsIdpr _lhsIinfo _lhsItdp
     in  (Syn_IRoot _lhsOedp _lhsOinters _lhsOvisits))
sem_IRoot_IRoot :: T_Interfaces ->
                   T_IRoot
sem_IRoot_IRoot !(T_Interfaces inters_) =
    (T_IRoot (\ (!_lhsIdpr)
                (!_lhsIinfo)
                (!_lhsItdp) ->
                  (case (({-# LINE 83 "src-ag/InterfacesRules.lag" #-}
                          _lhsIinfo
                          {-# LINE 135 "dist/build/InterfacesRules" #-}
                          )) of
                   { !_intersOinfo ->
                   (case (({-# LINE 85 "src-ag/InterfacesRules.lag" #-}
                           snd (bounds _lhsItdp) + 1
                           {-# LINE 140 "dist/build/InterfacesRules" #-}
                           )) of
                    { !_intersOv ->
                    (case (inters_ _intersOinfo _intersOv) of
                     { ( !_intersIdescr,!_intersIfirstvisitvertices,!_intersInewedges,!_intersIv,!T_Interfaces_1 inters_1) ->
                         (case (({-# LINE 260 "src-ag/InterfacesRules.lag" #-}
                                 let terminals = [ v | (v,cr) <- assocs (ruleTable _lhsIinfo), not (getHasCode cr), isLocal cr ]
                                 in _intersIfirstvisitvertices ++ terminals
                                 {-# LINE 148 "dist/build/InterfacesRules" #-}
                                 )) of
                          { !_intersOprev ->
                          (case (({-# LINE 71 "src-ag/InterfacesRules.lag" #-}
                                  toList _intersInewedges
                                  {-# LINE 153 "dist/build/InterfacesRules" #-}
                                  )) of
                           { !_newedges ->
                           (case (({-# LINE 72 "src-ag/InterfacesRules.lag" #-}
                                   let graph = buildG (0,_intersIv-1) es
                                       es = _newedges ++ edges _lhsItdp
                                   in transposeG graph
                                   {-# LINE 160 "dist/build/InterfacesRules" #-}
                                   )) of
                            { !_visitssGraph ->
                            (case (({-# LINE 218 "src-ag/InterfacesRules.lag" #-}
                                    _visitssGraph
                                    {-# LINE 165 "dist/build/InterfacesRules" #-}
                                    )) of
                             { !_intersOvssGraph ->
                             (case (({-# LINE 147 "src-ag/InterfacesRules.lag" #-}
                                     toList _intersIdescr
                                     {-# LINE 170 "dist/build/InterfacesRules" #-}
                                     )) of
                              { !_descr ->
                              (case (({-# LINE 127 "src-ag/InterfacesRules.lag" #-}
                                      Map.fromList _descr
                                      {-# LINE 175 "dist/build/InterfacesRules" #-}
                                      )) of
                               { !_intersOvisitDescr ->
                               (case (inters_1 _intersOprev _intersOvisitDescr _intersOvssGraph) of
                                { ( !_intersIinters,!T_Interfaces_2 inters_2) ->
                                    (case (({-# LINE 380 "src-ag/InterfacesRules.lag" #-}
                                            _intersIinters
                                            {-# LINE 182 "dist/build/InterfacesRules" #-}
                                            )) of
                                     { !_intersOallInters ->
                                     (case (({-# LINE 343 "src-ag/InterfacesRules.lag" #-}
                                             buildG (0,_intersIv-1) (map swap (_lhsIdpr ++ _newedges))
                                             {-# LINE 187 "dist/build/InterfacesRules" #-}
                                             )) of
                                      { !_intersOddp ->
                                      (case (inters_2 _intersOallInters _intersOddp) of
                                       { ( !_intersIedp,!_intersIvisits) ->
                                           (case (({-# LINE 442 "src-ag/InterfacesRules.lag" #-}
                                                   toList _intersIedp
                                                   {-# LINE 194 "dist/build/InterfacesRules" #-}
                                                   )) of
                                            { !_lhsOedp ->
                                            (case (({-# LINE 382 "src-ag/InterfacesRules.lag" #-}
                                                    _intersIinters
                                                    {-# LINE 199 "dist/build/InterfacesRules" #-}
                                                    )) of
                                             { !_lhsOinters ->
                                             (case (({-# LINE 383 "src-ag/InterfacesRules.lag" #-}
                                                     _intersIvisits
                                                     {-# LINE 204 "dist/build/InterfacesRules" #-}
                                                     )) of
                                              { !_lhsOvisits ->
                                              ( _lhsOedp,_lhsOinters,_lhsOvisits) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })))
-- Interface ---------------------------------------------------
{-
   visit 0:
      inherited attribute:
         info                 : Info
      chained attribute:
         v                    : Vertex
      synthesized attributes:
         descr                : Seq (Vertex,ChildVisit)
         firstvisitvertices   : [Vertex]
         newedges             : Seq Edge 
   visit 1:
      inherited attributes:
         prev                 : [Vertex]
         visitDescr           : Map Vertex ChildVisit
         vssGraph             : Graph
      synthesized attributes:
         inter                : CInterface
         nt                   : NontermIdent
   visit 2:
      inherited attributes:
         allInters            : CInterfaceMap
         ddp                  : Graph
      synthesized attributes:
         edp                  : Seq Edge
         visits               : Map ConstructorIdent CVisits
   alternatives:
      alternative Interface:
         child nt             : {NontermIdent}
         child cons           : {[ConstructorIdent]}
         child seg            : Segments 
         visit 0:
            local look        : {Vertex -> CRule}
            local v           : _
            local firstvisitvertices : _
            local descr       : _
            local newedges    : _
-}
-- cata
sem_Interface :: Interface ->
                 T_Interface
sem_Interface !(Interface _nt _cons _seg) =
    (sem_Interface_Interface _nt _cons (sem_Segments _seg))
-- semantic domain
newtype T_Interface = T_Interface (Info ->
                                   Vertex ->
                                   ( (Seq (Vertex,ChildVisit)),([Vertex]),(Seq Edge ),Vertex,T_Interface_1))
newtype T_Interface_1 = T_Interface_1 (([Vertex]) ->
                                       (Map Vertex ChildVisit) ->
                                       Graph ->
                                       ( CInterface,NontermIdent,T_Interface_2))
newtype T_Interface_2 = T_Interface_2 (CInterfaceMap ->
                                       Graph ->
                                       ( (Seq Edge),(Map ConstructorIdent CVisits)))
data Inh_Interface = Inh_Interface {allInters_Inh_Interface :: !(CInterfaceMap),ddp_Inh_Interface :: !(Graph),info_Inh_Interface :: !(Info),prev_Inh_Interface :: !(([Vertex])),v_Inh_Interface :: !(Vertex),visitDescr_Inh_Interface :: !((Map Vertex ChildVisit)),vssGraph_Inh_Interface :: !(Graph)}
data Syn_Interface = Syn_Interface {descr_Syn_Interface :: !((Seq (Vertex,ChildVisit))),edp_Syn_Interface :: !((Seq Edge)),firstvisitvertices_Syn_Interface :: !(([Vertex])),inter_Syn_Interface :: !(CInterface),newedges_Syn_Interface :: !((Seq Edge )),nt_Syn_Interface :: !(NontermIdent),v_Syn_Interface :: !(Vertex),visits_Syn_Interface :: !((Map ConstructorIdent CVisits))}
wrap_Interface :: T_Interface ->
                  Inh_Interface ->
                  Syn_Interface
wrap_Interface !(T_Interface sem) !(Inh_Interface _lhsIallInters _lhsIddp _lhsIinfo _lhsIprev _lhsIv _lhsIvisitDescr _lhsIvssGraph) =
    (let ( !_lhsOdescr,!_lhsOfirstvisitvertices,!_lhsOnewedges,!_lhsOv,!T_Interface_1 sem_1) = sem _lhsIinfo _lhsIv
         ( !_lhsOinter,!_lhsOnt,!T_Interface_2 sem_2) = sem_1 _lhsIprev _lhsIvisitDescr _lhsIvssGraph
         ( !_lhsOedp,!_lhsOvisits) = sem_2 _lhsIallInters _lhsIddp
     in  (Syn_Interface _lhsOdescr _lhsOedp _lhsOfirstvisitvertices _lhsOinter _lhsOnewedges _lhsOnt _lhsOv _lhsOvisits))
sem_Interface_Interface :: NontermIdent ->
                           ([ConstructorIdent]) ->
                           T_Segments ->
                           T_Interface
sem_Interface_Interface !nt_ !cons_ !(T_Segments seg_) =
    (T_Interface (\ (!_lhsIinfo)
                    (!_lhsIv) ->
                      (case (({-# LINE 83 "src-ag/InterfacesRules.lag" #-}
                              _lhsIinfo
                              {-# LINE 281 "dist/build/InterfacesRules" #-}
                              )) of
                       { !_segOinfo ->
                       (case (({-# LINE 205 "src-ag/InterfacesRules.lag" #-}
                               0
                               {-# LINE 286 "dist/build/InterfacesRules" #-}
                               )) of
                        { !_segOn ->
                        (case (({-# LINE 195 "src-ag/InterfacesRules.lag" #-}
                                \a -> ruleTable _lhsIinfo ! a
                                {-# LINE 291 "dist/build/InterfacesRules" #-}
                                )) of
                         { !_look ->
                         (case (({-# LINE 187 "src-ag/InterfacesRules.lag" #-}
                                 _lhsIv
                                 {-# LINE 296 "dist/build/InterfacesRules" #-}
                                 )) of
                          { !_segOv ->
                          (case (seg_ _segOinfo _segOn _segOv) of
                           { ( !_segIdescr,!_segIgroups,!_segInewedges,!_segInewvertices,!_segIv,!T_Segments_1 seg_1) ->
                               (case (({-# LINE 188 "src-ag/InterfacesRules.lag" #-}
                                       _segIv + length _segInewvertices
                                       {-# LINE 303 "dist/build/InterfacesRules" #-}
                                       )) of
                                { !_v ->
                                (case (({-# LINE 190 "src-ag/InterfacesRules.lag" #-}
                                        [_segIv .. _v-1]
                                        {-# LINE 308 "dist/build/InterfacesRules" #-}
                                        )) of
                                 { !_firstvisitvertices ->
                                 (case (({-# LINE 196 "src-ag/InterfacesRules.lag" #-}
                                         zipWith (cv _look (-1)) _firstvisitvertices _segIgroups
                                         {-# LINE 313 "dist/build/InterfacesRules" #-}
                                         )) of
                                  { !_descr ->
                                  (case (({-# LINE 197 "src-ag/InterfacesRules.lag" #-}
                                          _segIdescr Seq.>< Seq.fromList _descr
                                          {-# LINE 318 "dist/build/InterfacesRules" #-}
                                          )) of
                                   { !_lhsOdescr ->
                                   (case (({-# LINE 258 "src-ag/InterfacesRules.lag" #-}
                                           _firstvisitvertices
                                           {-# LINE 323 "dist/build/InterfacesRules" #-}
                                           )) of
                                    { !_lhsOfirstvisitvertices ->
                                    (case (({-# LINE 191 "src-ag/InterfacesRules.lag" #-}
                                            zip _firstvisitvertices _segInewvertices
                                            {-# LINE 328 "dist/build/InterfacesRules" #-}
                                            )) of
                                     { !_newedges ->
                                     (case (({-# LINE 192 "src-ag/InterfacesRules.lag" #-}
                                             _segInewedges Seq.>< Seq.fromList _newedges
                                             {-# LINE 333 "dist/build/InterfacesRules" #-}
                                             )) of
                                      { !_lhsOnewedges ->
                                      (case (({-# LINE 189 "src-ag/InterfacesRules.lag" #-}
                                              _v
                                              {-# LINE 338 "dist/build/InterfacesRules" #-}
                                              )) of
                                       { !_lhsOv ->
                                       (case ((let sem_Interface_Interface_1 :: T_Interface_1
                                                   sem_Interface_Interface_1 =
                                                       (T_Interface_1 (\ (!_lhsIprev)
                                                                         (!_lhsIvisitDescr)
                                                                         (!_lhsIvssGraph) ->
                                                                           (case (({-# LINE 216 "src-ag/InterfacesRules.lag" #-}
                                                                                   _lhsIvssGraph
                                                                                   {-# LINE 348 "dist/build/InterfacesRules" #-}
                                                                                   )) of
                                                                            { !_segOvssGraph ->
                                                                            (case (({-# LINE 125 "src-ag/InterfacesRules.lag" #-}
                                                                                    _lhsIvisitDescr
                                                                                    {-# LINE 353 "dist/build/InterfacesRules" #-}
                                                                                    )) of
                                                                             { !_segOvisitDescr ->
                                                                             (case (({-# LINE 263 "src-ag/InterfacesRules.lag" #-}
                                                                                     _lhsIprev
                                                                                     {-# LINE 358 "dist/build/InterfacesRules" #-}
                                                                                     )) of
                                                                              { !_segOprev ->
                                                                              (case (({-# LINE 237 "src-ag/InterfacesRules.lag" #-}
                                                                                      cons_
                                                                                      {-# LINE 363 "dist/build/InterfacesRules" #-}
                                                                                      )) of
                                                                               { !_segOcons ->
                                                                               (case (seg_1 _segOcons _segOprev _segOvisitDescr _segOvssGraph) of
                                                                                { ( !_segIsegs,!T_Segments_2 seg_2) ->
                                                                                    (case (({-# LINE 395 "src-ag/InterfacesRules.lag" #-}
                                                                                            CInterface _segIsegs
                                                                                            {-# LINE 370 "dist/build/InterfacesRules" #-}
                                                                                            )) of
                                                                                     { !_lhsOinter ->
                                                                                     (case (({-# LINE 391 "src-ag/InterfacesRules.lag" #-}
                                                                                             nt_
                                                                                             {-# LINE 375 "dist/build/InterfacesRules" #-}
                                                                                             )) of
                                                                                      { !_lhsOnt ->
                                                                                      (case ((let sem_Interface_Interface_2 :: T_Interface_2
                                                                                                  sem_Interface_Interface_2 =
                                                                                                      (T_Interface_2 (\ (!_lhsIallInters)
                                                                                                                        (!_lhsIddp) ->
                                                                                                                          (case (({-# LINE 341 "src-ag/InterfacesRules.lag" #-}
                                                                                                                                  _lhsIddp
                                                                                                                                  {-# LINE 384 "dist/build/InterfacesRules" #-}
                                                                                                                                  )) of
                                                                                                                           { !_segOddp ->
                                                                                                                           (case (({-# LINE 378 "src-ag/InterfacesRules.lag" #-}
                                                                                                                                   _lhsIallInters
                                                                                                                                   {-# LINE 389 "dist/build/InterfacesRules" #-}
                                                                                                                                   )) of
                                                                                                                            { !_segOallInters ->
                                                                                                                            (case (({-# LINE 351 "src-ag/InterfacesRules.lag" #-}
                                                                                                                                    _lhsIprev
                                                                                                                                    {-# LINE 394 "dist/build/InterfacesRules" #-}
                                                                                                                                    )) of
                                                                                                                             { !_segOfromLhs ->
                                                                                                                             (case (({-# LINE 314 "src-ag/InterfacesRules.lag" #-}
                                                                                                                                     True
                                                                                                                                     {-# LINE 399 "dist/build/InterfacesRules" #-}
                                                                                                                                     )) of
                                                                                                                              { !_segOisFirst ->
                                                                                                                              (case (seg_2 _segOallInters _segOddp _segOfromLhs _segOisFirst) of
                                                                                                                               { ( !_segIcvisits,!_segIedp,!_segIfirstInh,!_segIhdIntravisits,!_segIprev) ->
                                                                                                                                   (case (({-# LINE 437 "src-ag/InterfacesRules.lag" #-}
                                                                                                                                           _segIedp
                                                                                                                                           {-# LINE 406 "dist/build/InterfacesRules" #-}
                                                                                                                                           )) of
                                                                                                                                    { !_lhsOedp ->
                                                                                                                                    (case (({-# LINE 396 "src-ag/InterfacesRules.lag" #-}
                                                                                                                                            Map.fromList (zip cons_ (transpose _segIcvisits))
                                                                                                                                            {-# LINE 411 "dist/build/InterfacesRules" #-}
                                                                                                                                            )) of
                                                                                                                                     { !_lhsOvisits ->
                                                                                                                                     ( _lhsOedp,_lhsOvisits) }) }) }) }) }) }) })))
                                                                                              in  sem_Interface_Interface_2)) of
                                                                                       { ( !sem_Interface_2) ->
                                                                                       ( _lhsOinter,_lhsOnt,sem_Interface_2) }) }) }) }) }) }) }) })))
                                               in  sem_Interface_Interface_1)) of
                                        { ( !sem_Interface_1) ->
                                        ( _lhsOdescr,_lhsOfirstvisitvertices,_lhsOnewedges,_lhsOv,sem_Interface_1) }) }) }) }) }) }) }) }) }) }) }) }) }) })))
-- Interfaces --------------------------------------------------
{-
   visit 0:
      inherited attribute:
         info                 : Info
      chained attribute:
         v                    : Vertex
      synthesized attributes:
         descr                : Seq (Vertex,ChildVisit)
         firstvisitvertices   : [Vertex]
         newedges             : Seq Edge 
   visit 1:
      inherited attributes:
         prev                 : [Vertex]
         visitDescr           : Map Vertex ChildVisit
         vssGraph             : Graph
      synthesized attribute:
         inters               : CInterfaceMap
   visit 2:
      inherited attributes:
         allInters            : CInterfaceMap
         ddp                  : Graph
      synthesized attributes:
         edp                  : Seq Edge
         visits               : CVisitsMap
   alternatives:
      alternative Cons:
         child hd             : Interface 
         child tl             : Interfaces 
      alternative Nil:
-}
-- cata
sem_Interfaces :: Interfaces ->
                  T_Interfaces
sem_Interfaces !list =
    (Prelude.foldr sem_Interfaces_Cons sem_Interfaces_Nil (Prelude.map sem_Interface list))
-- semantic domain
newtype T_Interfaces = T_Interfaces (Info ->
                                     Vertex ->
                                     ( (Seq (Vertex,ChildVisit)),([Vertex]),(Seq Edge ),Vertex,T_Interfaces_1))
newtype T_Interfaces_1 = T_Interfaces_1 (([Vertex]) ->
                                         (Map Vertex ChildVisit) ->
                                         Graph ->
                                         ( CInterfaceMap,T_Interfaces_2))
newtype T_Interfaces_2 = T_Interfaces_2 (CInterfaceMap ->
                                         Graph ->
                                         ( (Seq Edge),CVisitsMap))
data Inh_Interfaces = Inh_Interfaces {allInters_Inh_Interfaces :: !(CInterfaceMap),ddp_Inh_Interfaces :: !(Graph),info_Inh_Interfaces :: !(Info),prev_Inh_Interfaces :: !(([Vertex])),v_Inh_Interfaces :: !(Vertex),visitDescr_Inh_Interfaces :: !((Map Vertex ChildVisit)),vssGraph_Inh_Interfaces :: !(Graph)}
data Syn_Interfaces = Syn_Interfaces {descr_Syn_Interfaces :: !((Seq (Vertex,ChildVisit))),edp_Syn_Interfaces :: !((Seq Edge)),firstvisitvertices_Syn_Interfaces :: !(([Vertex])),inters_Syn_Interfaces :: !(CInterfaceMap),newedges_Syn_Interfaces :: !((Seq Edge )),v_Syn_Interfaces :: !(Vertex),visits_Syn_Interfaces :: !(CVisitsMap)}
wrap_Interfaces :: T_Interfaces ->
                   Inh_Interfaces ->
                   Syn_Interfaces
wrap_Interfaces !(T_Interfaces sem) !(Inh_Interfaces _lhsIallInters _lhsIddp _lhsIinfo _lhsIprev _lhsIv _lhsIvisitDescr _lhsIvssGraph) =
    (let ( !_lhsOdescr,!_lhsOfirstvisitvertices,!_lhsOnewedges,!_lhsOv,!T_Interfaces_1 sem_1) = sem _lhsIinfo _lhsIv
         ( !_lhsOinters,!T_Interfaces_2 sem_2) = sem_1 _lhsIprev _lhsIvisitDescr _lhsIvssGraph
         ( !_lhsOedp,!_lhsOvisits) = sem_2 _lhsIallInters _lhsIddp
     in  (Syn_Interfaces _lhsOdescr _lhsOedp _lhsOfirstvisitvertices _lhsOinters _lhsOnewedges _lhsOv _lhsOvisits))
sem_Interfaces_Cons :: T_Interface ->
                       T_Interfaces ->
                       T_Interfaces
sem_Interfaces_Cons !(T_Interface hd_) !(T_Interfaces tl_) =
    (T_Interfaces (\ (!_lhsIinfo)
                     (!_lhsIv) ->
                       (case (({-# LINE 82 "src-ag/InterfacesRules.lag" #-}
                               _lhsIv
                               {-# LINE 486 "dist/build/InterfacesRules" #-}
                               )) of
                        { !_hdOv ->
                        (case (({-# LINE 83 "src-ag/InterfacesRules.lag" #-}
                                _lhsIinfo
                                {-# LINE 491 "dist/build/InterfacesRules" #-}
                                )) of
                         { !_hdOinfo ->
                         (case (hd_ _hdOinfo _hdOv) of
                          { ( !_hdIdescr,!_hdIfirstvisitvertices,!_hdInewedges,!_hdIv,!T_Interface_1 hd_1) ->
                              (case (({-# LINE 82 "src-ag/InterfacesRules.lag" #-}
                                      _hdIv
                                      {-# LINE 498 "dist/build/InterfacesRules" #-}
                                      )) of
                               { !_tlOv ->
                               (case (({-# LINE 83 "src-ag/InterfacesRules.lag" #-}
                                       _lhsIinfo
                                       {-# LINE 503 "dist/build/InterfacesRules" #-}
                                       )) of
                                { !_tlOinfo ->
                                (case (tl_ _tlOinfo _tlOv) of
                                 { ( !_tlIdescr,!_tlIfirstvisitvertices,!_tlInewedges,!_tlIv,!T_Interfaces_1 tl_1) ->
                                     (case (({-# LINE 130 "src-ag/InterfacesRules.lag" #-}
                                             _hdIdescr Seq.>< _tlIdescr
                                             {-# LINE 510 "dist/build/InterfacesRules" #-}
                                             )) of
                                      { !_lhsOdescr ->
                                      (case (({-# LINE 258 "src-ag/InterfacesRules.lag" #-}
                                              _hdIfirstvisitvertices ++ _tlIfirstvisitvertices
                                              {-# LINE 515 "dist/build/InterfacesRules" #-}
                                              )) of
                                       { !_lhsOfirstvisitvertices ->
                                       (case (({-# LINE 129 "src-ag/InterfacesRules.lag" #-}
                                               _hdInewedges Seq.>< _tlInewedges
                                               {-# LINE 520 "dist/build/InterfacesRules" #-}
                                               )) of
                                        { !_lhsOnewedges ->
                                        (case (({-# LINE 82 "src-ag/InterfacesRules.lag" #-}
                                                _tlIv
                                                {-# LINE 525 "dist/build/InterfacesRules" #-}
                                                )) of
                                         { !_lhsOv ->
                                         (case ((let sem_Interfaces_Cons_1 :: T_Interfaces_1
                                                     sem_Interfaces_Cons_1 =
                                                         (T_Interfaces_1 (\ (!_lhsIprev)
                                                                            (!_lhsIvisitDescr)
                                                                            (!_lhsIvssGraph) ->
                                                                              (case (({-# LINE 216 "src-ag/InterfacesRules.lag" #-}
                                                                                      _lhsIvssGraph
                                                                                      {-# LINE 535 "dist/build/InterfacesRules" #-}
                                                                                      )) of
                                                                               { !_tlOvssGraph ->
                                                                               (case (({-# LINE 125 "src-ag/InterfacesRules.lag" #-}
                                                                                       _lhsIvisitDescr
                                                                                       {-# LINE 540 "dist/build/InterfacesRules" #-}
                                                                                       )) of
                                                                                { !_tlOvisitDescr ->
                                                                                (case (({-# LINE 258 "src-ag/InterfacesRules.lag" #-}
                                                                                        _lhsIprev
                                                                                        {-# LINE 545 "dist/build/InterfacesRules" #-}
                                                                                        )) of
                                                                                 { !_tlOprev ->
                                                                                 (case (({-# LINE 216 "src-ag/InterfacesRules.lag" #-}
                                                                                         _lhsIvssGraph
                                                                                         {-# LINE 550 "dist/build/InterfacesRules" #-}
                                                                                         )) of
                                                                                  { !_hdOvssGraph ->
                                                                                  (case (({-# LINE 125 "src-ag/InterfacesRules.lag" #-}
                                                                                          _lhsIvisitDescr
                                                                                          {-# LINE 555 "dist/build/InterfacesRules" #-}
                                                                                          )) of
                                                                                   { !_hdOvisitDescr ->
                                                                                   (case (({-# LINE 258 "src-ag/InterfacesRules.lag" #-}
                                                                                           _lhsIprev
                                                                                           {-# LINE 560 "dist/build/InterfacesRules" #-}
                                                                                           )) of
                                                                                    { !_hdOprev ->
                                                                                    (case (tl_1 _tlOprev _tlOvisitDescr _tlOvssGraph) of
                                                                                     { ( !_tlIinters,!T_Interfaces_2 tl_2) ->
                                                                                         (case (hd_1 _hdOprev _hdOvisitDescr _hdOvssGraph) of
                                                                                          { ( !_hdIinter,!_hdInt,!T_Interface_2 hd_2) ->
                                                                                              (case (({-# LINE 385 "src-ag/InterfacesRules.lag" #-}
                                                                                                      Map.insert _hdInt _hdIinter _tlIinters
                                                                                                      {-# LINE 569 "dist/build/InterfacesRules" #-}
                                                                                                      )) of
                                                                                               { !_lhsOinters ->
                                                                                               (case ((let sem_Interfaces_Cons_2 :: T_Interfaces_2
                                                                                                           sem_Interfaces_Cons_2 =
                                                                                                               (T_Interfaces_2 (\ (!_lhsIallInters)
                                                                                                                                  (!_lhsIddp) ->
                                                                                                                                    (case (({-# LINE 341 "src-ag/InterfacesRules.lag" #-}
                                                                                                                                            _lhsIddp
                                                                                                                                            {-# LINE 578 "dist/build/InterfacesRules" #-}
                                                                                                                                            )) of
                                                                                                                                     { !_tlOddp ->
                                                                                                                                     (case (({-# LINE 378 "src-ag/InterfacesRules.lag" #-}
                                                                                                                                             _lhsIallInters
                                                                                                                                             {-# LINE 583 "dist/build/InterfacesRules" #-}
                                                                                                                                             )) of
                                                                                                                                      { !_tlOallInters ->
                                                                                                                                      (case (tl_2 _tlOallInters _tlOddp) of
                                                                                                                                       { ( !_tlIedp,!_tlIvisits) ->
                                                                                                                                           (case (({-# LINE 341 "src-ag/InterfacesRules.lag" #-}
                                                                                                                                                   _lhsIddp
                                                                                                                                                   {-# LINE 590 "dist/build/InterfacesRules" #-}
                                                                                                                                                   )) of
                                                                                                                                            { !_hdOddp ->
                                                                                                                                            (case (({-# LINE 378 "src-ag/InterfacesRules.lag" #-}
                                                                                                                                                    _lhsIallInters
                                                                                                                                                    {-# LINE 595 "dist/build/InterfacesRules" #-}
                                                                                                                                                    )) of
                                                                                                                                             { !_hdOallInters ->
                                                                                                                                             (case (hd_2 _hdOallInters _hdOddp) of
                                                                                                                                              { ( !_hdIedp,!_hdIvisits) ->
                                                                                                                                                  (case (({-# LINE 437 "src-ag/InterfacesRules.lag" #-}
                                                                                                                                                          _hdIedp Seq.>< _tlIedp
                                                                                                                                                          {-# LINE 602 "dist/build/InterfacesRules" #-}
                                                                                                                                                          )) of
                                                                                                                                                   { !_lhsOedp ->
                                                                                                                                                   (case (({-# LINE 386 "src-ag/InterfacesRules.lag" #-}
                                                                                                                                                           Map.insert _hdInt _hdIvisits _tlIvisits
                                                                                                                                                           {-# LINE 607 "dist/build/InterfacesRules" #-}
                                                                                                                                                           )) of
                                                                                                                                                    { !_lhsOvisits ->
                                                                                                                                                    ( _lhsOedp,_lhsOvisits) }) }) }) }) }) }) }) })))
                                                                                                       in  sem_Interfaces_Cons_2)) of
                                                                                                { ( !sem_Interfaces_2) ->
                                                                                                ( _lhsOinters,sem_Interfaces_2) }) }) }) }) }) }) }) }) }) })))
                                                 in  sem_Interfaces_Cons_1)) of
                                          { ( !sem_Interfaces_1) ->
                                          ( _lhsOdescr,_lhsOfirstvisitvertices,_lhsOnewedges,_lhsOv,sem_Interfaces_1) }) }) }) }) }) }) }) }) }) }) })))
sem_Interfaces_Nil :: T_Interfaces
sem_Interfaces_Nil =
    (T_Interfaces (\ (!_lhsIinfo)
                     (!_lhsIv) ->
                       (case (({-# LINE 130 "src-ag/InterfacesRules.lag" #-}
                               Seq.empty
                               {-# LINE 623 "dist/build/InterfacesRules" #-}
                               )) of
                        { !_lhsOdescr ->
                        (case (({-# LINE 258 "src-ag/InterfacesRules.lag" #-}
                                []
                                {-# LINE 628 "dist/build/InterfacesRules" #-}
                                )) of
                         { !_lhsOfirstvisitvertices ->
                         (case (({-# LINE 129 "src-ag/InterfacesRules.lag" #-}
                                 Seq.empty
                                 {-# LINE 633 "dist/build/InterfacesRules" #-}
                                 )) of
                          { !_lhsOnewedges ->
                          (case (({-# LINE 82 "src-ag/InterfacesRules.lag" #-}
                                  _lhsIv
                                  {-# LINE 638 "dist/build/InterfacesRules" #-}
                                  )) of
                           { !_lhsOv ->
                           (case ((let sem_Interfaces_Nil_1 :: T_Interfaces_1
                                       sem_Interfaces_Nil_1 =
                                           (T_Interfaces_1 (\ (!_lhsIprev)
                                                              (!_lhsIvisitDescr)
                                                              (!_lhsIvssGraph) ->
                                                                (case (({-# LINE 387 "src-ag/InterfacesRules.lag" #-}
                                                                        Map.empty
                                                                        {-# LINE 648 "dist/build/InterfacesRules" #-}
                                                                        )) of
                                                                 { !_lhsOinters ->
                                                                 (case ((let sem_Interfaces_Nil_2 :: T_Interfaces_2
                                                                             sem_Interfaces_Nil_2 =
                                                                                 (T_Interfaces_2 (\ (!_lhsIallInters)
                                                                                                    (!_lhsIddp) ->
                                                                                                      (case (({-# LINE 437 "src-ag/InterfacesRules.lag" #-}
                                                                                                              Seq.empty
                                                                                                              {-# LINE 657 "dist/build/InterfacesRules" #-}
                                                                                                              )) of
                                                                                                       { !_lhsOedp ->
                                                                                                       (case (({-# LINE 388 "src-ag/InterfacesRules.lag" #-}
                                                                                                               Map.empty
                                                                                                               {-# LINE 662 "dist/build/InterfacesRules" #-}
                                                                                                               )) of
                                                                                                        { !_lhsOvisits ->
                                                                                                        ( _lhsOedp,_lhsOvisits) }) })))
                                                                         in  sem_Interfaces_Nil_2)) of
                                                                  { ( !sem_Interfaces_2) ->
                                                                  ( _lhsOinters,sem_Interfaces_2) }) })))
                                   in  sem_Interfaces_Nil_1)) of
                            { ( !sem_Interfaces_1) ->
                            ( _lhsOdescr,_lhsOfirstvisitvertices,_lhsOnewedges,_lhsOv,sem_Interfaces_1) }) }) }) }) })))
-- Segment -----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         info                 : Info
      chained attribute:
         v                    : Vertex
   visit 1:
      inherited attributes:
         n                    : Int
         nextNewvertices      : [Vertex]
      synthesized attributes:
         descr                : Seq (Vertex,ChildVisit)
         groups               : [([Vertex],[Vertex])]
         newedges             : Seq Edge 
         newvertices          : [Vertex]
   visit 2:
      inherited attributes:
         cons                 : [ConstructorIdent]
         visitDescr           : Map Vertex ChildVisit
         vssGraph             : Graph
      chained attribute:
         prev                 : [Vertex]
      synthesized attribute:
         seg                  : CSegment
   visit 3:
      inherited attributes:
         allInters            : CInterfaceMap
         ddp                  : Graph
         fromLhs              : [Vertex]
         isFirst              : Bool
         nextInh              : [Vertex]
         nextIntravisits      : [IntraVisit]
      synthesized attributes:
         cvisits              : [CVisit]
         edp                  : Seq Edge
         inh                  : [Vertex]
         intravisits          : [IntraVisit]
         visitss              : [VisitSS]
   alternatives:
      alternative Segment:
         child inh            : {[Vertex]}
         child syn            : {[Vertex]}
         visit 0:
            local look        : {Vertex -> CRule}
            local occurAs     : {(CRule -> Bool) -> [Vertex] -> [Vertex]}
            local groups      : {[([Vertex],[Vertex])]}
            local v           : {Int}
         visit 1:
            local newvertices : _
            local visitedges  : _
            local attredges   : _
            intra v           : {Int}
            intra groups      : {[([Vertex],[Vertex])]}
            intra look        : {Vertex -> CRule}
            intra occurAs     : {(CRule -> Bool) -> [Vertex] -> [Vertex]}
         visit 2:
            local synOccur    : _
            local vss         : _
            local visitss'    : _
            local visitss     : {[[Vertex]]}
            local defined     : _
            local _tup1       : _
            local synmap      : {Map Identifier Type}
            local inhmap      : {Map Identifier Type}
            intra occurAs     : {(CRule -> Bool) -> [Vertex] -> [Vertex]}
         visit 3:
            local computed    : _
            local fromLhs     : _
            local iv          : _
            local intravisits : _
            intra visitss     : {[[Vertex]]}
            intra occurAs     : {(CRule -> Bool) -> [Vertex] -> [Vertex]}
            intra synmap      : {Map Identifier Type}
            intra inhmap      : {Map Identifier Type}
-}
-- cata
sem_Segment :: Segment ->
               T_Segment
sem_Segment !(Segment _inh _syn) =
    (sem_Segment_Segment _inh _syn)
-- semantic domain
newtype T_Segment = T_Segment (Info ->
                               Vertex ->
                               ( Vertex,T_Segment_1))
newtype T_Segment_1 = T_Segment_1 (Int ->
                                   ([Vertex]) ->
                                   ( (Seq (Vertex,ChildVisit)),([([Vertex],[Vertex])]),(Seq Edge ),([Vertex]),T_Segment_2))
newtype T_Segment_2 = T_Segment_2 (([ConstructorIdent]) ->
                                   ([Vertex]) ->
                                   (Map Vertex ChildVisit) ->
                                   Graph ->
                                   ( ([Vertex]),CSegment,T_Segment_3))
newtype T_Segment_3 = T_Segment_3 (CInterfaceMap ->
                                   Graph ->
                                   ([Vertex]) ->
                                   Bool ->
                                   ([Vertex]) ->
                                   ([IntraVisit]) ->
                                   ( ([CVisit]),(Seq Edge),([Vertex]),([IntraVisit]),([VisitSS])))
data Inh_Segment = Inh_Segment {allInters_Inh_Segment :: !(CInterfaceMap),cons_Inh_Segment :: !(([ConstructorIdent])),ddp_Inh_Segment :: !(Graph),fromLhs_Inh_Segment :: !(([Vertex])),info_Inh_Segment :: !(Info),isFirst_Inh_Segment :: !(Bool),n_Inh_Segment :: !(Int),nextInh_Inh_Segment :: !(([Vertex])),nextIntravisits_Inh_Segment :: !(([IntraVisit])),nextNewvertices_Inh_Segment :: !(([Vertex])),prev_Inh_Segment :: !(([Vertex])),v_Inh_Segment :: !(Vertex),visitDescr_Inh_Segment :: !((Map Vertex ChildVisit)),vssGraph_Inh_Segment :: !(Graph)}
data Syn_Segment = Syn_Segment {cvisits_Syn_Segment :: !(([CVisit])),descr_Syn_Segment :: !((Seq (Vertex,ChildVisit))),edp_Syn_Segment :: !((Seq Edge)),groups_Syn_Segment :: !(([([Vertex],[Vertex])])),inh_Syn_Segment :: !(([Vertex])),intravisits_Syn_Segment :: !(([IntraVisit])),newedges_Syn_Segment :: !((Seq Edge )),newvertices_Syn_Segment :: !(([Vertex])),prev_Syn_Segment :: !(([Vertex])),seg_Syn_Segment :: !(CSegment),v_Syn_Segment :: !(Vertex),visitss_Syn_Segment :: !(([VisitSS]))}
wrap_Segment :: T_Segment ->
                Inh_Segment ->
                Syn_Segment
wrap_Segment !(T_Segment sem) !(Inh_Segment _lhsIallInters _lhsIcons _lhsIddp _lhsIfromLhs _lhsIinfo _lhsIisFirst _lhsIn _lhsInextInh _lhsInextIntravisits _lhsInextNewvertices _lhsIprev _lhsIv _lhsIvisitDescr _lhsIvssGraph) =
    (let ( !_lhsOv,!T_Segment_1 sem_1) = sem _lhsIinfo _lhsIv
         ( !_lhsOdescr,!_lhsOgroups,!_lhsOnewedges,!_lhsOnewvertices,!T_Segment_2 sem_2) = sem_1 _lhsIn _lhsInextNewvertices
         ( !_lhsOprev,!_lhsOseg,!T_Segment_3 sem_3) = sem_2 _lhsIcons _lhsIprev _lhsIvisitDescr _lhsIvssGraph
         ( !_lhsOcvisits,!_lhsOedp,!_lhsOinh,!_lhsOintravisits,!_lhsOvisitss) = sem_3 _lhsIallInters _lhsIddp _lhsIfromLhs _lhsIisFirst _lhsInextInh _lhsInextIntravisits
     in  (Syn_Segment _lhsOcvisits _lhsOdescr _lhsOedp _lhsOgroups _lhsOinh _lhsOintravisits _lhsOnewedges _lhsOnewvertices _lhsOprev _lhsOseg _lhsOv _lhsOvisitss))
sem_Segment_Segment :: ([Vertex]) ->
                       ([Vertex]) ->
                       T_Segment
sem_Segment_Segment !inh_ !syn_ =
    (T_Segment (\ (!_lhsIinfo)
                  (!_lhsIv) ->
                    (case (({-# LINE 106 "src-ag/InterfacesRules.lag" #-}
                            \a -> ruleTable _lhsIinfo ! a
                            {-# LINE 791 "dist/build/InterfacesRules" #-}
                            )) of
                     { !_look ->
                     (case (({-# LINE 109 "src-ag/InterfacesRules.lag" #-}
                             \p us -> [ a  |  u <- us
                                           ,  a <- tdsToTdp _lhsIinfo ! u
                                           ,  p (_look a)]
                             {-# LINE 798 "dist/build/InterfacesRules" #-}
                             )) of
                      { !_occurAs ->
                      (case (({-# LINE 113 "src-ag/InterfacesRules.lag" #-}
                              let group as = gather _lhsIinfo (_occurAs isRhs as)
                              in map (partition (isInh . _look)) (group (inh_ ++ syn_))
                              {-# LINE 804 "dist/build/InterfacesRules" #-}
                              )) of
                       { !_groups ->
                       (case (({-# LINE 116 "src-ag/InterfacesRules.lag" #-}
                               _lhsIv + length _groups
                               {-# LINE 809 "dist/build/InterfacesRules" #-}
                               )) of
                        { !_v ->
                        (case (({-# LINE 82 "src-ag/InterfacesRules.lag" #-}
                                _v
                                {-# LINE 814 "dist/build/InterfacesRules" #-}
                                )) of
                         { !_lhsOv ->
                         (case ((let sem_Segment_Segment_1 :: T_Segment_1
                                     sem_Segment_Segment_1 =
                                         (T_Segment_1 (\ (!_lhsIn)
                                                         (!_lhsInextNewvertices) ->
                                                           (case (({-# LINE 117 "src-ag/InterfacesRules.lag" #-}
                                                                   [_lhsIv .. _v    -1]
                                                                   {-# LINE 823 "dist/build/InterfacesRules" #-}
                                                                   )) of
                                                            { !_newvertices ->
                                                            (case (({-# LINE 132 "src-ag/InterfacesRules.lag" #-}
                                                                    Seq.fromList $ zipWith (cv _look _lhsIn) _newvertices _groups
                                                                    {-# LINE 828 "dist/build/InterfacesRules" #-}
                                                                    )) of
                                                             { !_lhsOdescr ->
                                                             (case (({-# LINE 182 "src-ag/InterfacesRules.lag" #-}
                                                                     _groups
                                                                     {-# LINE 833 "dist/build/InterfacesRules" #-}
                                                                     )) of
                                                              { !_lhsOgroups ->
                                                              (case (({-# LINE 174 "src-ag/InterfacesRules.lag" #-}
                                                                      zip _newvertices _lhsInextNewvertices
                                                                      {-# LINE 838 "dist/build/InterfacesRules" #-}
                                                                      )) of
                                                               { !_visitedges ->
                                                               (case (({-# LINE 155 "src-ag/InterfacesRules.lag" #-}
                                                                       concat (zipWith ed _newvertices _groups)
                                                                       {-# LINE 843 "dist/build/InterfacesRules" #-}
                                                                       )) of
                                                                { !_attredges ->
                                                                (case (({-# LINE 175 "src-ag/InterfacesRules.lag" #-}
                                                                        Seq.fromList _attredges Seq.>< Seq.fromList _visitedges
                                                                        {-# LINE 848 "dist/build/InterfacesRules" #-}
                                                                        )) of
                                                                 { !_lhsOnewedges ->
                                                                 (case (({-# LINE 166 "src-ag/InterfacesRules.lag" #-}
                                                                         _newvertices
                                                                         {-# LINE 853 "dist/build/InterfacesRules" #-}
                                                                         )) of
                                                                  { !_lhsOnewvertices ->
                                                                  (case ((let sem_Segment_Segment_2 :: T_Segment_2
                                                                              sem_Segment_Segment_2 =
                                                                                  (T_Segment_2 (\ (!_lhsIcons)
                                                                                                  (!_lhsIprev)
                                                                                                  (!_lhsIvisitDescr)
                                                                                                  (!_lhsIvssGraph) ->
                                                                                                    (case (({-# LINE 229 "src-ag/InterfacesRules.lag" #-}
                                                                                                            gather _lhsIinfo (_occurAs isLhs syn_)
                                                                                                            {-# LINE 864 "dist/build/InterfacesRules" #-}
                                                                                                            )) of
                                                                                                     { !_synOccur ->
                                                                                                     (case (({-# LINE 230 "src-ag/InterfacesRules.lag" #-}
                                                                                                             let hasCode v | inRange (bounds (ruleTable _lhsIinfo)) v =  getHasCode (ruleTable _lhsIinfo ! v)
                                                                                                                           | otherwise = True
                                                                                                             in if  null syn_
                                                                                                                    then replicate (length _lhsIcons) []
                                                                                                                    else map (filter hasCode . topSort' _lhsIvssGraph) _synOccur
                                                                                                             {-# LINE 873 "dist/build/InterfacesRules" #-}
                                                                                                             )) of
                                                                                                      { !_vss ->
                                                                                                      (case (({-# LINE 270 "src-ag/InterfacesRules.lag" #-}
                                                                                                              map (\\ _lhsIprev) _vss
                                                                                                              {-# LINE 878 "dist/build/InterfacesRules" #-}
                                                                                                              )) of
                                                                                                       { !_visitss' ->
                                                                                                       (case (({-# LINE 284 "src-ag/InterfacesRules.lag" #-}
                                                                                                               let  rem :: [(Identifier,Identifier,Maybe Type)] -> [Vertex] -> [Vertex]
                                                                                                                    rem prev [] = []
                                                                                                                    rem prev (v:vs)
                                                                                                                      | inRange (bounds table) v
                                                                                                                          = let  cr = table ! v
                                                                                                                                 addV = case findIndex cmp prev of
                                                                                                                                          Just _ -> id
                                                                                                                                          _      -> (v:)
                                                                                                                                 cmp (fld,attr,tp) = getField cr == fld && getAttr cr == attr && sameNT (getType cr) tp
                                                                                                                                 sameNT (Just (NT ntA _ _)) (Just (NT ntB _ _)) = ntA == ntB
                                                                                                                                 sameNT _          _                            = False
                                                                                                                                 def = Map.elems (getDefines cr)
                                                                                                                            in addV (rem (def ++ prev) vs)
                                                                                                                      | otherwise = v:rem prev vs
                                                                                                                    table = ruleTable _lhsIinfo
                                                                                                               in map (rem []) _visitss'
                                                                                                               {-# LINE 898 "dist/build/InterfacesRules" #-}
                                                                                                               )) of
                                                                                                        { !_visitss ->
                                                                                                        (case (({-# LINE 271 "src-ag/InterfacesRules.lag" #-}
                                                                                                                let defines v = case Map.lookup v _lhsIvisitDescr of
                                                                                                                                  Nothing -> [v]
                                                                                                                                  Just (ChildVisit _ _ _ inh _) -> v:inh
                                                                                                                in concatMap (concatMap defines) _visitss
                                                                                                                {-# LINE 906 "dist/build/InterfacesRules" #-}
                                                                                                                )) of
                                                                                                         { !_defined ->
                                                                                                         (case (({-# LINE 275 "src-ag/InterfacesRules.lag" #-}
                                                                                                                 _lhsIprev ++ _defined
                                                                                                                 {-# LINE 911 "dist/build/InterfacesRules" #-}
                                                                                                                 )) of
                                                                                                          { !_lhsOprev ->
                                                                                                          (case (({-# LINE 409 "src-ag/InterfacesRules.lag" #-}
                                                                                                                  let makemap = Map.fromList . map findType
                                                                                                                      findType v = getNtaNameType (attrTable _lhsIinfo ! v)
                                                                                                                  in (makemap inh_,makemap syn_)
                                                                                                                  {-# LINE 918 "dist/build/InterfacesRules" #-}
                                                                                                                  )) of
                                                                                                           { !__tup1 ->
                                                                                                           (case (({-# LINE 409 "src-ag/InterfacesRules.lag" #-}
                                                                                                                   __tup1
                                                                                                                   {-# LINE 923 "dist/build/InterfacesRules" #-}
                                                                                                                   )) of
                                                                                                            { !(_,!_synmap) ->
                                                                                                            (case (({-# LINE 409 "src-ag/InterfacesRules.lag" #-}
                                                                                                                    __tup1
                                                                                                                    {-# LINE 928 "dist/build/InterfacesRules" #-}
                                                                                                                    )) of
                                                                                                             { !(!_inhmap,_) ->
                                                                                                             (case (({-# LINE 405 "src-ag/InterfacesRules.lag" #-}
                                                                                                                     if False then undefined _lhsIvssGraph _lhsIvisitDescr _lhsIprev else CSegment _inhmap _synmap
                                                                                                                     {-# LINE 933 "dist/build/InterfacesRules" #-}
                                                                                                                     )) of
                                                                                                              { !_lhsOseg ->
                                                                                                              (case ((let sem_Segment_Segment_3 :: T_Segment_3
                                                                                                                          sem_Segment_Segment_3 =
                                                                                                                              (T_Segment_3 (\ (!_lhsIallInters)
                                                                                                                                              (!_lhsIddp)
                                                                                                                                              (!_lhsIfromLhs)
                                                                                                                                              (!_lhsIisFirst)
                                                                                                                                              (!_lhsInextInh)
                                                                                                                                              (!_lhsInextIntravisits) ->
                                                                                                                                                (case (({-# LINE 357 "src-ag/InterfacesRules.lag" #-}
                                                                                                                                                        let computes v = case Map.lookup v _lhsIvisitDescr of
                                                                                                                                                                           Nothing -> Map.keys (getDefines (ruleTable _lhsIinfo ! v))
                                                                                                                                                                           Just (ChildVisit _ _ _ _ syn) -> v:syn
                                                                                                                                                        in concatMap (concatMap computes) _visitss
                                                                                                                                                        {-# LINE 949 "dist/build/InterfacesRules" #-}
                                                                                                                                                        )) of
                                                                                                                                                 { !_computed ->
                                                                                                                                                 (case (({-# LINE 356 "src-ag/InterfacesRules.lag" #-}
                                                                                                                                                         _occurAs isLhs inh_ ++ _lhsIfromLhs
                                                                                                                                                         {-# LINE 954 "dist/build/InterfacesRules" #-}
                                                                                                                                                         )) of
                                                                                                                                                  { !_fromLhs ->
                                                                                                                                                  (case (({-# LINE 362 "src-ag/InterfacesRules.lag" #-}
                                                                                                                                                          \vs next ->
                                                                                                                                                            let needed = concatMap (_lhsIddp !) vs
                                                                                                                                                            in nub (needed ++ next) \\ (_fromLhs ++ _computed)
                                                                                                                                                          {-# LINE 961 "dist/build/InterfacesRules" #-}
                                                                                                                                                          )) of
                                                                                                                                                   { !_iv ->
                                                                                                                                                   (case (({-# LINE 361 "src-ag/InterfacesRules.lag" #-}
                                                                                                                                                           zipWith _iv _visitss _lhsInextIntravisits
                                                                                                                                                           {-# LINE 966 "dist/build/InterfacesRules" #-}
                                                                                                                                                           )) of
                                                                                                                                                    { !_intravisits ->
                                                                                                                                                    (case (({-# LINE 412 "src-ag/InterfacesRules.lag" #-}
                                                                                                                                                            let  mkVisit vss intra = CVisit _inhmap _synmap (mkSequence vss) (mkSequence intra) True
                                                                                                                                                                 mkSequence = map mkRule
                                                                                                                                                                 mkRule v = case Map.lookup v _lhsIvisitDescr of
                                                                                                                                                                              Nothing -> ruleTable _lhsIinfo ! v
                                                                                                                                                                              Just (ChildVisit name nt n _ _) -> ccv name nt n _lhsIallInters
                                                                                                                                                            in zipWith mkVisit _visitss _intravisits
                                                                                                                                                            {-# LINE 976 "dist/build/InterfacesRules" #-}
                                                                                                                                                            )) of
                                                                                                                                                     { !_lhsOcvisits ->
                                                                                                                                                     (case (({-# LINE 439 "src-ag/InterfacesRules.lag" #-}
                                                                                                                                                             Seq.fromList [(i,s) | i <- inh_, s <- syn_]
                                                                                                                                                             Seq.>< Seq.fromList [(s,i) | s <- syn_, i <- _lhsInextInh ]
                                                                                                                                                             {-# LINE 982 "dist/build/InterfacesRules" #-}
                                                                                                                                                             )) of
                                                                                                                                                      { !_lhsOedp ->
                                                                                                                                                      (case (({-# LINE 444 "src-ag/InterfacesRules.lag" #-}
                                                                                                                                                              inh_
                                                                                                                                                              {-# LINE 987 "dist/build/InterfacesRules" #-}
                                                                                                                                                              )) of
                                                                                                                                                       { !_lhsOinh ->
                                                                                                                                                       (case (({-# LINE 327 "src-ag/InterfacesRules.lag" #-}
                                                                                                                                                               _intravisits
                                                                                                                                                               {-# LINE 992 "dist/build/InterfacesRules" #-}
                                                                                                                                                               )) of
                                                                                                                                                        { !_lhsOintravisits ->
                                                                                                                                                        (case (({-# LINE 269 "src-ag/InterfacesRules.lag" #-}
                                                                                                                                                                _visitss
                                                                                                                                                                {-# LINE 997 "dist/build/InterfacesRules" #-}
                                                                                                                                                                )) of
                                                                                                                                                         { !_lhsOvisitss ->
                                                                                                                                                         ( _lhsOcvisits,_lhsOedp,_lhsOinh,_lhsOintravisits,_lhsOvisitss) }) }) }) }) }) }) }) }) })))
                                                                                                                      in  sem_Segment_Segment_3)) of
                                                                                                               { ( !sem_Segment_3) ->
                                                                                                               ( _lhsOprev,_lhsOseg,sem_Segment_3) }) }) }) }) }) }) }) }) }) }) })))
                                                                          in  sem_Segment_Segment_2)) of
                                                                   { ( !sem_Segment_2) ->
                                                                   ( _lhsOdescr,_lhsOgroups,_lhsOnewedges,_lhsOnewvertices,sem_Segment_2) }) }) }) }) }) }) }) })))
                                 in  sem_Segment_Segment_1)) of
                          { ( !sem_Segment_1) ->
                          ( _lhsOv,sem_Segment_1) }) }) }) }) }) })))
-- Segments ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         info                 : Info
         n                    : Int
      chained attribute:
         v                    : Vertex
      synthesized attributes:
         descr                : Seq (Vertex,ChildVisit)
         groups               : [([Vertex],[Vertex])]
         newedges             : Seq Edge 
         newvertices          : [Vertex]
   visit 1:
      inherited attributes:
         cons                 : [ConstructorIdent]
         prev                 : [Vertex]
         visitDescr           : Map Vertex ChildVisit
         vssGraph             : Graph
      synthesized attribute:
         segs                 : CSegments
   visit 2:
      inherited attributes:
         allInters            : CInterfaceMap
         ddp                  : Graph
         fromLhs              : [Vertex]
         isFirst              : Bool
      synthesized attributes:
         cvisits              : [[CVisit]]
         edp                  : Seq Edge
         firstInh             : [Vertex]
         hdIntravisits        : [IntraVisit]
         prev                 : [Vertex]
   alternatives:
      alternative Cons:
         child hd             : Segment 
         child tl             : Segments 
      alternative Nil:
-}
-- cata
sem_Segments :: Segments ->
                T_Segments
sem_Segments !list =
    (Prelude.foldr sem_Segments_Cons sem_Segments_Nil (Prelude.map sem_Segment list))
-- semantic domain
newtype T_Segments = T_Segments (Info ->
                                 Int ->
                                 Vertex ->
                                 ( (Seq (Vertex,ChildVisit)),([([Vertex],[Vertex])]),(Seq Edge ),([Vertex]),Vertex,T_Segments_1))
newtype T_Segments_1 = T_Segments_1 (([ConstructorIdent]) ->
                                     ([Vertex]) ->
                                     (Map Vertex ChildVisit) ->
                                     Graph ->
                                     ( CSegments,T_Segments_2))
newtype T_Segments_2 = T_Segments_2 (CInterfaceMap ->
                                     Graph ->
                                     ([Vertex]) ->
                                     Bool ->
                                     ( ([[CVisit]]),(Seq Edge),([Vertex]),([IntraVisit]),([Vertex])))
data Inh_Segments = Inh_Segments {allInters_Inh_Segments :: !(CInterfaceMap),cons_Inh_Segments :: !(([ConstructorIdent])),ddp_Inh_Segments :: !(Graph),fromLhs_Inh_Segments :: !(([Vertex])),info_Inh_Segments :: !(Info),isFirst_Inh_Segments :: !(Bool),n_Inh_Segments :: !(Int),prev_Inh_Segments :: !(([Vertex])),v_Inh_Segments :: !(Vertex),visitDescr_Inh_Segments :: !((Map Vertex ChildVisit)),vssGraph_Inh_Segments :: !(Graph)}
data Syn_Segments = Syn_Segments {cvisits_Syn_Segments :: !(([[CVisit]])),descr_Syn_Segments :: !((Seq (Vertex,ChildVisit))),edp_Syn_Segments :: !((Seq Edge)),firstInh_Syn_Segments :: !(([Vertex])),groups_Syn_Segments :: !(([([Vertex],[Vertex])])),hdIntravisits_Syn_Segments :: !(([IntraVisit])),newedges_Syn_Segments :: !((Seq Edge )),newvertices_Syn_Segments :: !(([Vertex])),prev_Syn_Segments :: !(([Vertex])),segs_Syn_Segments :: !(CSegments),v_Syn_Segments :: !(Vertex)}
wrap_Segments :: T_Segments ->
                 Inh_Segments ->
                 Syn_Segments
wrap_Segments !(T_Segments sem) !(Inh_Segments _lhsIallInters _lhsIcons _lhsIddp _lhsIfromLhs _lhsIinfo _lhsIisFirst _lhsIn _lhsIprev _lhsIv _lhsIvisitDescr _lhsIvssGraph) =
    (let ( !_lhsOdescr,!_lhsOgroups,!_lhsOnewedges,!_lhsOnewvertices,!_lhsOv,!T_Segments_1 sem_1) = sem _lhsIinfo _lhsIn _lhsIv
         ( !_lhsOsegs,!T_Segments_2 sem_2) = sem_1 _lhsIcons _lhsIprev _lhsIvisitDescr _lhsIvssGraph
         ( !_lhsOcvisits,!_lhsOedp,!_lhsOfirstInh,!_lhsOhdIntravisits,!_lhsOprev) = sem_2 _lhsIallInters _lhsIddp _lhsIfromLhs _lhsIisFirst
     in  (Syn_Segments _lhsOcvisits _lhsOdescr _lhsOedp _lhsOfirstInh _lhsOgroups _lhsOhdIntravisits _lhsOnewedges _lhsOnewvertices _lhsOprev _lhsOsegs _lhsOv))
sem_Segments_Cons :: T_Segment ->
                     T_Segments ->
                     T_Segments
sem_Segments_Cons !(T_Segment hd_) !(T_Segments tl_) =
    (T_Segments (\ (!_lhsIinfo)
                   (!_lhsIn)
                   (!_lhsIv) ->
                     (case (({-# LINE 82 "src-ag/InterfacesRules.lag" #-}
                             _lhsIv
                             {-# LINE 1088 "dist/build/InterfacesRules" #-}
                             )) of
                      { !_hdOv ->
                      (case (({-# LINE 83 "src-ag/InterfacesRules.lag" #-}
                              _lhsIinfo
                              {-# LINE 1093 "dist/build/InterfacesRules" #-}
                              )) of
                       { !_hdOinfo ->
                       (case (hd_ _hdOinfo _hdOv) of
                        { ( !_hdIv,!T_Segment_1 hd_1) ->
                            (case (({-# LINE 82 "src-ag/InterfacesRules.lag" #-}
                                    _hdIv
                                    {-# LINE 1100 "dist/build/InterfacesRules" #-}
                                    )) of
                             { !_tlOv ->
                             (case (({-# LINE 83 "src-ag/InterfacesRules.lag" #-}
                                     _lhsIinfo
                                     {-# LINE 1105 "dist/build/InterfacesRules" #-}
                                     )) of
                              { !_tlOinfo ->
                              (case (({-# LINE 203 "src-ag/InterfacesRules.lag" #-}
                                      _lhsIn
                                      {-# LINE 1110 "dist/build/InterfacesRules" #-}
                                      )) of
                               { !_hdOn ->
                               (case (({-# LINE 207 "src-ag/InterfacesRules.lag" #-}
                                       _lhsIn + 1
                                       {-# LINE 1115 "dist/build/InterfacesRules" #-}
                                       )) of
                                { !_tlOn ->
                                (case (tl_ _tlOinfo _tlOn _tlOv) of
                                 { ( !_tlIdescr,!_tlIgroups,!_tlInewedges,!_tlInewvertices,!_tlIv,!T_Segments_1 tl_1) ->
                                     (case (({-# LINE 169 "src-ag/InterfacesRules.lag" #-}
                                             _tlInewvertices
                                             {-# LINE 1122 "dist/build/InterfacesRules" #-}
                                             )) of
                                      { !_hdOnextNewvertices ->
                                      (case (hd_1 _hdOn _hdOnextNewvertices) of
                                       { ( !_hdIdescr,!_hdIgroups,!_hdInewedges,!_hdInewvertices,!T_Segment_2 hd_2) ->
                                           (case (({-# LINE 130 "src-ag/InterfacesRules.lag" #-}
                                                   _hdIdescr Seq.>< _tlIdescr
                                                   {-# LINE 1129 "dist/build/InterfacesRules" #-}
                                                   )) of
                                            { !_lhsOdescr ->
                                            (case (({-# LINE 184 "src-ag/InterfacesRules.lag" #-}
                                                    _hdIgroups
                                                    {-# LINE 1134 "dist/build/InterfacesRules" #-}
                                                    )) of
                                             { !_lhsOgroups ->
                                             (case (({-# LINE 129 "src-ag/InterfacesRules.lag" #-}
                                                     _hdInewedges Seq.>< _tlInewedges
                                                     {-# LINE 1139 "dist/build/InterfacesRules" #-}
                                                     )) of
                                              { !_lhsOnewedges ->
                                              (case (({-# LINE 170 "src-ag/InterfacesRules.lag" #-}
                                                      _hdInewvertices
                                                      {-# LINE 1144 "dist/build/InterfacesRules" #-}
                                                      )) of
                                               { !_lhsOnewvertices ->
                                               (case (({-# LINE 82 "src-ag/InterfacesRules.lag" #-}
                                                       _tlIv
                                                       {-# LINE 1149 "dist/build/InterfacesRules" #-}
                                                       )) of
                                                { !_lhsOv ->
                                                (case ((let sem_Segments_Cons_1 :: T_Segments_1
                                                            sem_Segments_Cons_1 =
                                                                (T_Segments_1 (\ (!_lhsIcons)
                                                                                 (!_lhsIprev)
                                                                                 (!_lhsIvisitDescr)
                                                                                 (!_lhsIvssGraph) ->
                                                                                   (case (({-# LINE 216 "src-ag/InterfacesRules.lag" #-}
                                                                                           _lhsIvssGraph
                                                                                           {-# LINE 1160 "dist/build/InterfacesRules" #-}
                                                                                           )) of
                                                                                    { !_tlOvssGraph ->
                                                                                    (case (({-# LINE 125 "src-ag/InterfacesRules.lag" #-}
                                                                                            _lhsIvisitDescr
                                                                                            {-# LINE 1165 "dist/build/InterfacesRules" #-}
                                                                                            )) of
                                                                                     { !_tlOvisitDescr ->
                                                                                     (case (({-# LINE 216 "src-ag/InterfacesRules.lag" #-}
                                                                                             _lhsIvssGraph
                                                                                             {-# LINE 1170 "dist/build/InterfacesRules" #-}
                                                                                             )) of
                                                                                      { !_hdOvssGraph ->
                                                                                      (case (({-# LINE 125 "src-ag/InterfacesRules.lag" #-}
                                                                                              _lhsIvisitDescr
                                                                                              {-# LINE 1175 "dist/build/InterfacesRules" #-}
                                                                                              )) of
                                                                                       { !_hdOvisitDescr ->
                                                                                       (case (({-# LINE 263 "src-ag/InterfacesRules.lag" #-}
                                                                                               _lhsIprev
                                                                                               {-# LINE 1180 "dist/build/InterfacesRules" #-}
                                                                                               )) of
                                                                                        { !_hdOprev ->
                                                                                        (case (({-# LINE 235 "src-ag/InterfacesRules.lag" #-}
                                                                                                _lhsIcons
                                                                                                {-# LINE 1185 "dist/build/InterfacesRules" #-}
                                                                                                )) of
                                                                                         { !_hdOcons ->
                                                                                         (case (hd_2 _hdOcons _hdOprev _hdOvisitDescr _hdOvssGraph) of
                                                                                          { ( !_hdIprev,!_hdIseg,!T_Segment_3 hd_3) ->
                                                                                              (case (({-# LINE 263 "src-ag/InterfacesRules.lag" #-}
                                                                                                      _hdIprev
                                                                                                      {-# LINE 1192 "dist/build/InterfacesRules" #-}
                                                                                                      )) of
                                                                                               { !_tlOprev ->
                                                                                               (case (({-# LINE 235 "src-ag/InterfacesRules.lag" #-}
                                                                                                       _lhsIcons
                                                                                                       {-# LINE 1197 "dist/build/InterfacesRules" #-}
                                                                                                       )) of
                                                                                                { !_tlOcons ->
                                                                                                (case (tl_1 _tlOcons _tlOprev _tlOvisitDescr _tlOvssGraph) of
                                                                                                 { ( !_tlIsegs,!T_Segments_2 tl_2) ->
                                                                                                     (case (({-# LINE 400 "src-ag/InterfacesRules.lag" #-}
                                                                                                             _hdIseg : _tlIsegs
                                                                                                             {-# LINE 1204 "dist/build/InterfacesRules" #-}
                                                                                                             )) of
                                                                                                      { !_lhsOsegs ->
                                                                                                      (case ((let sem_Segments_Cons_2 :: T_Segments_2
                                                                                                                  sem_Segments_Cons_2 =
                                                                                                                      (T_Segments_2 (\ (!_lhsIallInters)
                                                                                                                                       (!_lhsIddp)
                                                                                                                                       (!_lhsIfromLhs)
                                                                                                                                       (!_lhsIisFirst) ->
                                                                                                                                         (case (({-# LINE 341 "src-ag/InterfacesRules.lag" #-}
                                                                                                                                                 _lhsIddp
                                                                                                                                                 {-# LINE 1215 "dist/build/InterfacesRules" #-}
                                                                                                                                                 )) of
                                                                                                                                          { !_tlOddp ->
                                                                                                                                          (case (({-# LINE 378 "src-ag/InterfacesRules.lag" #-}
                                                                                                                                                  _lhsIallInters
                                                                                                                                                  {-# LINE 1220 "dist/build/InterfacesRules" #-}
                                                                                                                                                  )) of
                                                                                                                                           { !_tlOallInters ->
                                                                                                                                           (case (({-# LINE 341 "src-ag/InterfacesRules.lag" #-}
                                                                                                                                                   _lhsIddp
                                                                                                                                                   {-# LINE 1225 "dist/build/InterfacesRules" #-}
                                                                                                                                                   )) of
                                                                                                                                            { !_hdOddp ->
                                                                                                                                            (case (({-# LINE 378 "src-ag/InterfacesRules.lag" #-}
                                                                                                                                                    _lhsIallInters
                                                                                                                                                    {-# LINE 1230 "dist/build/InterfacesRules" #-}
                                                                                                                                                    )) of
                                                                                                                                             { !_hdOallInters ->
                                                                                                                                             (case (({-# LINE 354 "src-ag/InterfacesRules.lag" #-}
                                                                                                                                                     []
                                                                                                                                                     {-# LINE 1235 "dist/build/InterfacesRules" #-}
                                                                                                                                                     )) of
                                                                                                                                              { !_tlOfromLhs ->
                                                                                                                                              (case (({-# LINE 353 "src-ag/InterfacesRules.lag" #-}
                                                                                                                                                      _lhsIfromLhs
                                                                                                                                                      {-# LINE 1240 "dist/build/InterfacesRules" #-}
                                                                                                                                                      )) of
                                                                                                                                               { !_hdOfromLhs ->
                                                                                                                                               (case (({-# LINE 316 "src-ag/InterfacesRules.lag" #-}
                                                                                                                                                       False
                                                                                                                                                       {-# LINE 1245 "dist/build/InterfacesRules" #-}
                                                                                                                                                       )) of
                                                                                                                                                { !_tlOisFirst ->
                                                                                                                                                (case (tl_2 _tlOallInters _tlOddp _tlOfromLhs _tlOisFirst) of
                                                                                                                                                 { ( !_tlIcvisits,!_tlIedp,!_tlIfirstInh,!_tlIhdIntravisits,!_tlIprev) ->
                                                                                                                                                     (case (({-# LINE 329 "src-ag/InterfacesRules.lag" #-}
                                                                                                                                                             _tlIhdIntravisits
                                                                                                                                                             {-# LINE 1252 "dist/build/InterfacesRules" #-}
                                                                                                                                                             )) of
                                                                                                                                                      { !_hdOnextIntravisits ->
                                                                                                                                                      (case (({-# LINE 312 "src-ag/InterfacesRules.lag" #-}
                                                                                                                                                              _lhsIisFirst
                                                                                                                                                              {-# LINE 1257 "dist/build/InterfacesRules" #-}
                                                                                                                                                              )) of
                                                                                                                                                       { !_hdOisFirst ->
                                                                                                                                                       (case (({-# LINE 446 "src-ag/InterfacesRules.lag" #-}
                                                                                                                                                               _tlIfirstInh
                                                                                                                                                               {-# LINE 1262 "dist/build/InterfacesRules" #-}
                                                                                                                                                               )) of
                                                                                                                                                        { !_hdOnextInh ->
                                                                                                                                                        (case (hd_3 _hdOallInters _hdOddp _hdOfromLhs _hdOisFirst _hdOnextInh _hdOnextIntravisits) of
                                                                                                                                                         { ( !_hdIcvisits,!_hdIedp,!_hdIinh,!_hdIintravisits,!_hdIvisitss) ->
                                                                                                                                                             (case (({-# LINE 399 "src-ag/InterfacesRules.lag" #-}
                                                                                                                                                                     _hdIcvisits : _tlIcvisits
                                                                                                                                                                     {-# LINE 1269 "dist/build/InterfacesRules" #-}
                                                                                                                                                                     )) of
                                                                                                                                                              { !_lhsOcvisits ->
                                                                                                                                                              (case (({-# LINE 437 "src-ag/InterfacesRules.lag" #-}
                                                                                                                                                                      _hdIedp Seq.>< _tlIedp
                                                                                                                                                                      {-# LINE 1274 "dist/build/InterfacesRules" #-}
                                                                                                                                                                      )) of
                                                                                                                                                               { !_lhsOedp ->
                                                                                                                                                               (case (({-# LINE 447 "src-ag/InterfacesRules.lag" #-}
                                                                                                                                                                       _hdIinh
                                                                                                                                                                       {-# LINE 1279 "dist/build/InterfacesRules" #-}
                                                                                                                                                                       )) of
                                                                                                                                                                { !_lhsOfirstInh ->
                                                                                                                                                                (case (({-# LINE 330 "src-ag/InterfacesRules.lag" #-}
                                                                                                                                                                        _hdIintravisits
                                                                                                                                                                        {-# LINE 1284 "dist/build/InterfacesRules" #-}
                                                                                                                                                                        )) of
                                                                                                                                                                 { !_lhsOhdIntravisits ->
                                                                                                                                                                 (case (({-# LINE 263 "src-ag/InterfacesRules.lag" #-}
                                                                                                                                                                         _tlIprev
                                                                                                                                                                         {-# LINE 1289 "dist/build/InterfacesRules" #-}
                                                                                                                                                                         )) of
                                                                                                                                                                  { !_lhsOprev ->
                                                                                                                                                                  ( _lhsOcvisits,_lhsOedp,_lhsOfirstInh,_lhsOhdIntravisits,_lhsOprev) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })))
                                                                                                              in  sem_Segments_Cons_2)) of
                                                                                                       { ( !sem_Segments_2) ->
                                                                                                       ( _lhsOsegs,sem_Segments_2) }) }) }) }) }) }) }) }) }) }) }) })))
                                                        in  sem_Segments_Cons_1)) of
                                                 { ( !sem_Segments_1) ->
                                                 ( _lhsOdescr,_lhsOgroups,_lhsOnewedges,_lhsOnewvertices,_lhsOv,sem_Segments_1) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })))
sem_Segments_Nil :: T_Segments
sem_Segments_Nil =
    (T_Segments (\ (!_lhsIinfo)
                   (!_lhsIn)
                   (!_lhsIv) ->
                     (case (({-# LINE 130 "src-ag/InterfacesRules.lag" #-}
                             Seq.empty
                             {-# LINE 1306 "dist/build/InterfacesRules" #-}
                             )) of
                      { !_lhsOdescr ->
                      (case (({-# LINE 185 "src-ag/InterfacesRules.lag" #-}
                              []
                              {-# LINE 1311 "dist/build/InterfacesRules" #-}
                              )) of
                       { !_lhsOgroups ->
                       (case (({-# LINE 129 "src-ag/InterfacesRules.lag" #-}
                               Seq.empty
                               {-# LINE 1316 "dist/build/InterfacesRules" #-}
                               )) of
                        { !_lhsOnewedges ->
                        (case (({-# LINE 171 "src-ag/InterfacesRules.lag" #-}
                                []
                                {-# LINE 1321 "dist/build/InterfacesRules" #-}
                                )) of
                         { !_lhsOnewvertices ->
                         (case (({-# LINE 82 "src-ag/InterfacesRules.lag" #-}
                                 _lhsIv
                                 {-# LINE 1326 "dist/build/InterfacesRules" #-}
                                 )) of
                          { !_lhsOv ->
                          (case ((let sem_Segments_Nil_1 :: T_Segments_1
                                      sem_Segments_Nil_1 =
                                          (T_Segments_1 (\ (!_lhsIcons)
                                                           (!_lhsIprev)
                                                           (!_lhsIvisitDescr)
                                                           (!_lhsIvssGraph) ->
                                                             (case (({-# LINE 401 "src-ag/InterfacesRules.lag" #-}
                                                                     []
                                                                     {-# LINE 1337 "dist/build/InterfacesRules" #-}
                                                                     )) of
                                                              { !_lhsOsegs ->
                                                              (case ((let sem_Segments_Nil_2 :: T_Segments_2
                                                                          sem_Segments_Nil_2 =
                                                                              (T_Segments_2 (\ (!_lhsIallInters)
                                                                                               (!_lhsIddp)
                                                                                               (!_lhsIfromLhs)
                                                                                               (!_lhsIisFirst) ->
                                                                                                 (case (({-# LINE 399 "src-ag/InterfacesRules.lag" #-}
                                                                                                         []
                                                                                                         {-# LINE 1348 "dist/build/InterfacesRules" #-}
                                                                                                         )) of
                                                                                                  { !_lhsOcvisits ->
                                                                                                  (case (({-# LINE 437 "src-ag/InterfacesRules.lag" #-}
                                                                                                          Seq.empty
                                                                                                          {-# LINE 1353 "dist/build/InterfacesRules" #-}
                                                                                                          )) of
                                                                                                   { !_lhsOedp ->
                                                                                                   (case (({-# LINE 448 "src-ag/InterfacesRules.lag" #-}
                                                                                                           []
                                                                                                           {-# LINE 1358 "dist/build/InterfacesRules" #-}
                                                                                                           )) of
                                                                                                    { !_lhsOfirstInh ->
                                                                                                    (case (({-# LINE 331 "src-ag/InterfacesRules.lag" #-}
                                                                                                            repeat []
                                                                                                            {-# LINE 1363 "dist/build/InterfacesRules" #-}
                                                                                                            )) of
                                                                                                     { !_lhsOhdIntravisits ->
                                                                                                     (case (({-# LINE 263 "src-ag/InterfacesRules.lag" #-}
                                                                                                             _lhsIprev
                                                                                                             {-# LINE 1368 "dist/build/InterfacesRules" #-}
                                                                                                             )) of
                                                                                                      { !_lhsOprev ->
                                                                                                      ( _lhsOcvisits,_lhsOedp,_lhsOfirstInh,_lhsOhdIntravisits,_lhsOprev) }) }) }) }) })))
                                                                      in  sem_Segments_Nil_2)) of
                                                               { ( !sem_Segments_2) ->
                                                               ( _lhsOsegs,sem_Segments_2) }) })))
                                  in  sem_Segments_Nil_1)) of
                           { ( !sem_Segments_1) ->
                           ( _lhsOdescr,_lhsOgroups,_lhsOnewedges,_lhsOnewvertices,_lhsOv,sem_Segments_1) }) }) }) }) }) })))