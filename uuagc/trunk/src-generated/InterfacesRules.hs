{-# OPTIONS_GHC -XBangPatterns #-}

-- UUAGC 0.9.42.2 (src-ag/InterfacesRules.lag)
module InterfacesRules where
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
{-# LINE 22 "dist/build/InterfacesRules.hs" #-}

{-# LINE 2 "./src-ag/Interfaces.ag" #-}

import CommonTypes
import SequentialTypes
{-# LINE 28 "dist/build/InterfacesRules.hs" #-}
{-# LINE 53 "./src-ag/InterfacesRules.lag" #-}

type VisitSS = [Vertex]
{-# LINE 32 "dist/build/InterfacesRules.hs" #-}

{-# LINE 88 "./src-ag/InterfacesRules.lag" #-}

gather :: Info -> [Vertex] -> [[Vertex]]
gather info =  eqClasses comp
               where comp a b = isEqualField (ruleTable info ! a) (ruleTable info ! b)
{-# LINE 39 "dist/build/InterfacesRules.hs" #-}

{-# LINE 129 "./src-ag/InterfacesRules.lag" #-}

-- Only non-empty syn will ever be forced, because visits with empty syn are never performed
-- Right hand side synthesized attributes always have a field
cv :: (Vertex -> CRule) -> Int -> Vertex -> ([Vertex],[Vertex]) -> (Vertex,ChildVisit)
cv look n v (inh,syn) =  let  fld = getField (look (head syn))
                              rnt = fromJust (getRhsNt (look (head syn)))
                              d = ChildVisit fld rnt n inh syn
                         in (v,d)
{-# LINE 50 "dist/build/InterfacesRules.hs" #-}

{-# LINE 152 "./src-ag/InterfacesRules.lag" #-}

ed :: Vertex -> ([Vertex], [Vertex]) -> [(Vertex, Vertex)]
ed v (inh,syn) = map (\i -> (i,v)) inh ++ map (\s -> (v,s)) syn
{-# LINE 56 "dist/build/InterfacesRules.hs" #-}

{-# LINE 240 "./src-ag/InterfacesRules.lag" #-}

postorder :: Tree a -> [a]
postorder (Node a ts) = postorderF ts ++ [a]
postorderF :: Forest a -> [a]
postorderF = concatMap postorder
postOrd :: Graph -> [Vertex] -> [Vertex]
postOrd g = postorderF . dfs g
topSort' :: Graph -> [Vertex] -> [Vertex]
topSort' g = postOrd g
{-# LINE 68 "dist/build/InterfacesRules.hs" #-}

{-# LINE 323 "./src-ag/InterfacesRules.lag" #-}

type IntraVisit = [Vertex]
{-# LINE 73 "dist/build/InterfacesRules.hs" #-}

{-# LINE 345 "./src-ag/InterfacesRules.lag" #-}

swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)
{-# LINE 79 "dist/build/InterfacesRules.hs" #-}

{-# LINE 420 "./src-ag/InterfacesRules.lag" #-}

ccv :: Identifier -> NontermIdent -> Int -> CInterfaceMap -> CRule
ccv name nt n table
  =  CChildVisit name nt n inh syn lst
     where  CInterface segs = Map.findWithDefault (error ("InterfacesRules::ccv::interfaces not in table for nt: " ++ show nt)) nt table
            (seg:remain) = drop n segs
            CSegment inh syn = seg
            lst = null remain
{-# LINE 90 "dist/build/InterfacesRules.hs" #-}
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
                  (case (({-# LINE 78 "./src-ag/InterfacesRules.lag" #-}
                          _lhsIinfo
                          {-# LINE 136 "dist/build/InterfacesRules.hs" #-}
                          )) of
                   { !_intersOinfo ->
                   (case (({-# LINE 80 "./src-ag/InterfacesRules.lag" #-}
                           snd (bounds _lhsItdp) + 1
                           {-# LINE 141 "dist/build/InterfacesRules.hs" #-}
                           )) of
                    { !_intersOv ->
                    (case (inters_ _intersOinfo _intersOv) of
                     { ( !_intersIdescr,!_intersIfirstvisitvertices,!_intersInewedges,!_intersIv,!T_Interfaces_1 inters_1) ->
                         (case (({-# LINE 260 "./src-ag/InterfacesRules.lag" #-}
                                 let terminals = [ v | (v,cr) <- assocs (ruleTable _lhsIinfo), not (getHasCode cr), isLocal cr ]
                                 in _intersIfirstvisitvertices ++ terminals
                                 {-# LINE 149 "dist/build/InterfacesRules.hs" #-}
                                 )) of
                          { !_intersOprev ->
                          (case (({-# LINE 66 "./src-ag/InterfacesRules.lag" #-}
                                  toList _intersInewedges
                                  {-# LINE 154 "dist/build/InterfacesRules.hs" #-}
                                  )) of
                           { !_newedges ->
                           (case (({-# LINE 67 "./src-ag/InterfacesRules.lag" #-}
                                   let graph = buildG (0,_intersIv-1) es
                                       es = _newedges ++ edges _lhsItdp
                                   in transposeG graph
                                   {-# LINE 161 "dist/build/InterfacesRules.hs" #-}
                                   )) of
                            { !_visitssGraph ->
                            (case (({-# LINE 214 "./src-ag/InterfacesRules.lag" #-}
                                    _visitssGraph
                                    {-# LINE 166 "dist/build/InterfacesRules.hs" #-}
                                    )) of
                             { !_intersOvssGraph ->
                             (case (({-# LINE 142 "./src-ag/InterfacesRules.lag" #-}
                                     toList _intersIdescr
                                     {-# LINE 171 "dist/build/InterfacesRules.hs" #-}
                                     )) of
                              { !_descr ->
                              (case (({-# LINE 122 "./src-ag/InterfacesRules.lag" #-}
                                      Map.fromList _descr
                                      {-# LINE 176 "dist/build/InterfacesRules.hs" #-}
                                      )) of
                               { !_intersOvisitDescr ->
                               (case (inters_1 _intersOprev _intersOvisitDescr _intersOvssGraph) of
                                { ( !_intersIinters,!T_Interfaces_2 inters_2) ->
                                    (case (({-# LINE 381 "./src-ag/InterfacesRules.lag" #-}
                                            _intersIinters
                                            {-# LINE 183 "dist/build/InterfacesRules.hs" #-}
                                            )) of
                                     { !_intersOallInters ->
                                     (case (({-# LINE 343 "./src-ag/InterfacesRules.lag" #-}
                                             buildG (0,_intersIv-1) (map swap (_lhsIdpr ++ _newedges))
                                             {-# LINE 188 "dist/build/InterfacesRules.hs" #-}
                                             )) of
                                      { !_intersOddp ->
                                      (case (inters_2 _intersOallInters _intersOddp) of
                                       { ( !_intersIedp,!_intersIvisits) ->
                                           (case (({-# LINE 443 "./src-ag/InterfacesRules.lag" #-}
                                                   toList _intersIedp
                                                   {-# LINE 195 "dist/build/InterfacesRules.hs" #-}
                                                   )) of
                                            { !_lhsOedp ->
                                            (case (({-# LINE 383 "./src-ag/InterfacesRules.lag" #-}
                                                    _intersIinters
                                                    {-# LINE 200 "dist/build/InterfacesRules.hs" #-}
                                                    )) of
                                             { !_lhsOinters ->
                                             (case (({-# LINE 384 "./src-ag/InterfacesRules.lag" #-}
                                                     _intersIvisits
                                                     {-# LINE 205 "dist/build/InterfacesRules.hs" #-}
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
                      (case (({-# LINE 78 "./src-ag/InterfacesRules.lag" #-}
                              _lhsIinfo
                              {-# LINE 282 "dist/build/InterfacesRules.hs" #-}
                              )) of
                       { !_segOinfo ->
                       (case (({-# LINE 201 "./src-ag/InterfacesRules.lag" #-}
                               0
                               {-# LINE 287 "dist/build/InterfacesRules.hs" #-}
                               )) of
                        { !_segOn ->
                        (case (({-# LINE 191 "./src-ag/InterfacesRules.lag" #-}
                                \a -> ruleTable _lhsIinfo ! a
                                {-# LINE 292 "dist/build/InterfacesRules.hs" #-}
                                )) of
                         { !_look ->
                         (case (({-# LINE 183 "./src-ag/InterfacesRules.lag" #-}
                                 _lhsIv
                                 {-# LINE 297 "dist/build/InterfacesRules.hs" #-}
                                 )) of
                          { !_segOv ->
                          (case (seg_ _segOinfo _segOn _segOv) of
                           { ( !_segIdescr,!_segIgroups,!_segInewedges,!_segInewvertices,!_segIv,!T_Segments_1 seg_1) ->
                               (case (({-# LINE 184 "./src-ag/InterfacesRules.lag" #-}
                                       _segIv + length _segInewvertices
                                       {-# LINE 304 "dist/build/InterfacesRules.hs" #-}
                                       )) of
                                { !_v ->
                                (case (({-# LINE 186 "./src-ag/InterfacesRules.lag" #-}
                                        [_segIv .. _v-1]
                                        {-# LINE 309 "dist/build/InterfacesRules.hs" #-}
                                        )) of
                                 { !_firstvisitvertices ->
                                 (case (({-# LINE 192 "./src-ag/InterfacesRules.lag" #-}
                                         zipWith (cv _look (-1)) _firstvisitvertices _segIgroups
                                         {-# LINE 314 "dist/build/InterfacesRules.hs" #-}
                                         )) of
                                  { !_descr ->
                                  (case (({-# LINE 193 "./src-ag/InterfacesRules.lag" #-}
                                          _segIdescr Seq.>< Seq.fromList _descr
                                          {-# LINE 319 "dist/build/InterfacesRules.hs" #-}
                                          )) of
                                   { !_lhsOdescr ->
                                   (case (({-# LINE 258 "./src-ag/InterfacesRules.lag" #-}
                                           _firstvisitvertices
                                           {-# LINE 324 "dist/build/InterfacesRules.hs" #-}
                                           )) of
                                    { !_lhsOfirstvisitvertices ->
                                    (case (({-# LINE 187 "./src-ag/InterfacesRules.lag" #-}
                                            zip _firstvisitvertices _segInewvertices
                                            {-# LINE 329 "dist/build/InterfacesRules.hs" #-}
                                            )) of
                                     { !_newedges ->
                                     (case (({-# LINE 188 "./src-ag/InterfacesRules.lag" #-}
                                             _segInewedges Seq.>< Seq.fromList _newedges
                                             {-# LINE 334 "dist/build/InterfacesRules.hs" #-}
                                             )) of
                                      { !_lhsOnewedges ->
                                      (case (({-# LINE 185 "./src-ag/InterfacesRules.lag" #-}
                                              _v
                                              {-# LINE 339 "dist/build/InterfacesRules.hs" #-}
                                              )) of
                                       { !_lhsOv ->
                                       (case ((let sem_Interface_Interface_1 :: T_Interface_1
                                                   sem_Interface_Interface_1 =
                                                       (T_Interface_1 (\ (!_lhsIprev)
                                                                         (!_lhsIvisitDescr)
                                                                         (!_lhsIvssGraph) ->
                                                                           (case (({-# LINE 212 "./src-ag/InterfacesRules.lag" #-}
                                                                                   _lhsIvssGraph
                                                                                   {-# LINE 349 "dist/build/InterfacesRules.hs" #-}
                                                                                   )) of
                                                                            { !_segOvssGraph ->
                                                                            (case (({-# LINE 120 "./src-ag/InterfacesRules.lag" #-}
                                                                                    _lhsIvisitDescr
                                                                                    {-# LINE 354 "dist/build/InterfacesRules.hs" #-}
                                                                                    )) of
                                                                             { !_segOvisitDescr ->
                                                                             (case (({-# LINE 263 "./src-ag/InterfacesRules.lag" #-}
                                                                                     _lhsIprev
                                                                                     {-# LINE 359 "dist/build/InterfacesRules.hs" #-}
                                                                                     )) of
                                                                              { !_segOprev ->
                                                                              (case (({-# LINE 233 "./src-ag/InterfacesRules.lag" #-}
                                                                                      cons_
                                                                                      {-# LINE 364 "dist/build/InterfacesRules.hs" #-}
                                                                                      )) of
                                                                               { !_segOcons ->
                                                                               (case (seg_1 _segOcons _segOprev _segOvisitDescr _segOvssGraph) of
                                                                                { ( !_segIsegs,!T_Segments_2 seg_2) ->
                                                                                    (case (({-# LINE 396 "./src-ag/InterfacesRules.lag" #-}
                                                                                            CInterface _segIsegs
                                                                                            {-# LINE 371 "dist/build/InterfacesRules.hs" #-}
                                                                                            )) of
                                                                                     { !_lhsOinter ->
                                                                                     (case (({-# LINE 392 "./src-ag/InterfacesRules.lag" #-}
                                                                                             nt_
                                                                                             {-# LINE 376 "dist/build/InterfacesRules.hs" #-}
                                                                                             )) of
                                                                                      { !_lhsOnt ->
                                                                                      (case ((let sem_Interface_Interface_2 :: T_Interface_2
                                                                                                  sem_Interface_Interface_2 =
                                                                                                      (T_Interface_2 (\ (!_lhsIallInters)
                                                                                                                        (!_lhsIddp) ->
                                                                                                                          (case (({-# LINE 341 "./src-ag/InterfacesRules.lag" #-}
                                                                                                                                  _lhsIddp
                                                                                                                                  {-# LINE 385 "dist/build/InterfacesRules.hs" #-}
                                                                                                                                  )) of
                                                                                                                           { !_segOddp ->
                                                                                                                           (case (({-# LINE 379 "./src-ag/InterfacesRules.lag" #-}
                                                                                                                                   _lhsIallInters
                                                                                                                                   {-# LINE 390 "dist/build/InterfacesRules.hs" #-}
                                                                                                                                   )) of
                                                                                                                            { !_segOallInters ->
                                                                                                                            (case (({-# LINE 352 "./src-ag/InterfacesRules.lag" #-}
                                                                                                                                    _lhsIprev
                                                                                                                                    {-# LINE 395 "dist/build/InterfacesRules.hs" #-}
                                                                                                                                    )) of
                                                                                                                             { !_segOfromLhs ->
                                                                                                                             (case (({-# LINE 314 "./src-ag/InterfacesRules.lag" #-}
                                                                                                                                     True
                                                                                                                                     {-# LINE 400 "dist/build/InterfacesRules.hs" #-}
                                                                                                                                     )) of
                                                                                                                              { !_segOisFirst ->
                                                                                                                              (case (seg_2 _segOallInters _segOddp _segOfromLhs _segOisFirst) of
                                                                                                                               { ( !_segIcvisits,!_segIedp,!_segIfirstInh,!_segIhdIntravisits,!_segIprev) ->
                                                                                                                                   (case (({-# LINE 438 "./src-ag/InterfacesRules.lag" #-}
                                                                                                                                           _segIedp
                                                                                                                                           {-# LINE 407 "dist/build/InterfacesRules.hs" #-}
                                                                                                                                           )) of
                                                                                                                                    { !_lhsOedp ->
                                                                                                                                    (case (({-# LINE 397 "./src-ag/InterfacesRules.lag" #-}
                                                                                                                                            Map.fromList (zip cons_ (transpose _segIcvisits))
                                                                                                                                            {-# LINE 412 "dist/build/InterfacesRules.hs" #-}
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
                       (case (({-# LINE 77 "./src-ag/InterfacesRules.lag" #-}
                               _lhsIv
                               {-# LINE 487 "dist/build/InterfacesRules.hs" #-}
                               )) of
                        { !_hdOv ->
                        (case (({-# LINE 78 "./src-ag/InterfacesRules.lag" #-}
                                _lhsIinfo
                                {-# LINE 492 "dist/build/InterfacesRules.hs" #-}
                                )) of
                         { !_hdOinfo ->
                         (case (hd_ _hdOinfo _hdOv) of
                          { ( !_hdIdescr,!_hdIfirstvisitvertices,!_hdInewedges,!_hdIv,!T_Interface_1 hd_1) ->
                              (case (({-# LINE 77 "./src-ag/InterfacesRules.lag" #-}
                                      _hdIv
                                      {-# LINE 499 "dist/build/InterfacesRules.hs" #-}
                                      )) of
                               { !_tlOv ->
                               (case (({-# LINE 78 "./src-ag/InterfacesRules.lag" #-}
                                       _lhsIinfo
                                       {-# LINE 504 "dist/build/InterfacesRules.hs" #-}
                                       )) of
                                { !_tlOinfo ->
                                (case (tl_ _tlOinfo _tlOv) of
                                 { ( !_tlIdescr,!_tlIfirstvisitvertices,!_tlInewedges,!_tlIv,!T_Interfaces_1 tl_1) ->
                                     (case (({-# LINE 125 "./src-ag/InterfacesRules.lag" #-}
                                             _hdIdescr Seq.>< _tlIdescr
                                             {-# LINE 511 "dist/build/InterfacesRules.hs" #-}
                                             )) of
                                      { !_lhsOdescr ->
                                      (case (({-# LINE 258 "./src-ag/InterfacesRules.lag" #-}
                                              _hdIfirstvisitvertices ++ _tlIfirstvisitvertices
                                              {-# LINE 516 "dist/build/InterfacesRules.hs" #-}
                                              )) of
                                       { !_lhsOfirstvisitvertices ->
                                       (case (({-# LINE 124 "./src-ag/InterfacesRules.lag" #-}
                                               _hdInewedges Seq.>< _tlInewedges
                                               {-# LINE 521 "dist/build/InterfacesRules.hs" #-}
                                               )) of
                                        { !_lhsOnewedges ->
                                        (case (({-# LINE 77 "./src-ag/InterfacesRules.lag" #-}
                                                _tlIv
                                                {-# LINE 526 "dist/build/InterfacesRules.hs" #-}
                                                )) of
                                         { !_lhsOv ->
                                         (case ((let sem_Interfaces_Cons_1 :: T_Interfaces_1
                                                     sem_Interfaces_Cons_1 =
                                                         (T_Interfaces_1 (\ (!_lhsIprev)
                                                                            (!_lhsIvisitDescr)
                                                                            (!_lhsIvssGraph) ->
                                                                              (case (({-# LINE 212 "./src-ag/InterfacesRules.lag" #-}
                                                                                      _lhsIvssGraph
                                                                                      {-# LINE 536 "dist/build/InterfacesRules.hs" #-}
                                                                                      )) of
                                                                               { !_tlOvssGraph ->
                                                                               (case (({-# LINE 120 "./src-ag/InterfacesRules.lag" #-}
                                                                                       _lhsIvisitDescr
                                                                                       {-# LINE 541 "dist/build/InterfacesRules.hs" #-}
                                                                                       )) of
                                                                                { !_tlOvisitDescr ->
                                                                                (case (({-# LINE 258 "./src-ag/InterfacesRules.lag" #-}
                                                                                        _lhsIprev
                                                                                        {-# LINE 546 "dist/build/InterfacesRules.hs" #-}
                                                                                        )) of
                                                                                 { !_tlOprev ->
                                                                                 (case (({-# LINE 212 "./src-ag/InterfacesRules.lag" #-}
                                                                                         _lhsIvssGraph
                                                                                         {-# LINE 551 "dist/build/InterfacesRules.hs" #-}
                                                                                         )) of
                                                                                  { !_hdOvssGraph ->
                                                                                  (case (({-# LINE 120 "./src-ag/InterfacesRules.lag" #-}
                                                                                          _lhsIvisitDescr
                                                                                          {-# LINE 556 "dist/build/InterfacesRules.hs" #-}
                                                                                          )) of
                                                                                   { !_hdOvisitDescr ->
                                                                                   (case (({-# LINE 258 "./src-ag/InterfacesRules.lag" #-}
                                                                                           _lhsIprev
                                                                                           {-# LINE 561 "dist/build/InterfacesRules.hs" #-}
                                                                                           )) of
                                                                                    { !_hdOprev ->
                                                                                    (case (tl_1 _tlOprev _tlOvisitDescr _tlOvssGraph) of
                                                                                     { ( !_tlIinters,!T_Interfaces_2 tl_2) ->
                                                                                         (case (hd_1 _hdOprev _hdOvisitDescr _hdOvssGraph) of
                                                                                          { ( !_hdIinter,!_hdInt,!T_Interface_2 hd_2) ->
                                                                                              (case (({-# LINE 386 "./src-ag/InterfacesRules.lag" #-}
                                                                                                      Map.insert _hdInt _hdIinter _tlIinters
                                                                                                      {-# LINE 570 "dist/build/InterfacesRules.hs" #-}
                                                                                                      )) of
                                                                                               { !_lhsOinters ->
                                                                                               (case ((let sem_Interfaces_Cons_2 :: T_Interfaces_2
                                                                                                           sem_Interfaces_Cons_2 =
                                                                                                               (T_Interfaces_2 (\ (!_lhsIallInters)
                                                                                                                                  (!_lhsIddp) ->
                                                                                                                                    (case (({-# LINE 341 "./src-ag/InterfacesRules.lag" #-}
                                                                                                                                            _lhsIddp
                                                                                                                                            {-# LINE 579 "dist/build/InterfacesRules.hs" #-}
                                                                                                                                            )) of
                                                                                                                                     { !_tlOddp ->
                                                                                                                                     (case (({-# LINE 379 "./src-ag/InterfacesRules.lag" #-}
                                                                                                                                             _lhsIallInters
                                                                                                                                             {-# LINE 584 "dist/build/InterfacesRules.hs" #-}
                                                                                                                                             )) of
                                                                                                                                      { !_tlOallInters ->
                                                                                                                                      (case (tl_2 _tlOallInters _tlOddp) of
                                                                                                                                       { ( !_tlIedp,!_tlIvisits) ->
                                                                                                                                           (case (({-# LINE 341 "./src-ag/InterfacesRules.lag" #-}
                                                                                                                                                   _lhsIddp
                                                                                                                                                   {-# LINE 591 "dist/build/InterfacesRules.hs" #-}
                                                                                                                                                   )) of
                                                                                                                                            { !_hdOddp ->
                                                                                                                                            (case (({-# LINE 379 "./src-ag/InterfacesRules.lag" #-}
                                                                                                                                                    _lhsIallInters
                                                                                                                                                    {-# LINE 596 "dist/build/InterfacesRules.hs" #-}
                                                                                                                                                    )) of
                                                                                                                                             { !_hdOallInters ->
                                                                                                                                             (case (hd_2 _hdOallInters _hdOddp) of
                                                                                                                                              { ( !_hdIedp,!_hdIvisits) ->
                                                                                                                                                  (case (({-# LINE 438 "./src-ag/InterfacesRules.lag" #-}
                                                                                                                                                          _hdIedp Seq.>< _tlIedp
                                                                                                                                                          {-# LINE 603 "dist/build/InterfacesRules.hs" #-}
                                                                                                                                                          )) of
                                                                                                                                                   { !_lhsOedp ->
                                                                                                                                                   (case (({-# LINE 387 "./src-ag/InterfacesRules.lag" #-}
                                                                                                                                                           Map.insert _hdInt _hdIvisits _tlIvisits
                                                                                                                                                           {-# LINE 608 "dist/build/InterfacesRules.hs" #-}
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
                       (case (({-# LINE 125 "./src-ag/InterfacesRules.lag" #-}
                               Seq.empty
                               {-# LINE 624 "dist/build/InterfacesRules.hs" #-}
                               )) of
                        { !_lhsOdescr ->
                        (case (({-# LINE 258 "./src-ag/InterfacesRules.lag" #-}
                                []
                                {-# LINE 629 "dist/build/InterfacesRules.hs" #-}
                                )) of
                         { !_lhsOfirstvisitvertices ->
                         (case (({-# LINE 124 "./src-ag/InterfacesRules.lag" #-}
                                 Seq.empty
                                 {-# LINE 634 "dist/build/InterfacesRules.hs" #-}
                                 )) of
                          { !_lhsOnewedges ->
                          (case (({-# LINE 77 "./src-ag/InterfacesRules.lag" #-}
                                  _lhsIv
                                  {-# LINE 639 "dist/build/InterfacesRules.hs" #-}
                                  )) of
                           { !_lhsOv ->
                           (case ((let sem_Interfaces_Nil_1 :: T_Interfaces_1
                                       sem_Interfaces_Nil_1 =
                                           (T_Interfaces_1 (\ (!_lhsIprev)
                                                              (!_lhsIvisitDescr)
                                                              (!_lhsIvssGraph) ->
                                                                (case (({-# LINE 388 "./src-ag/InterfacesRules.lag" #-}
                                                                        Map.empty
                                                                        {-# LINE 649 "dist/build/InterfacesRules.hs" #-}
                                                                        )) of
                                                                 { !_lhsOinters ->
                                                                 (case ((let sem_Interfaces_Nil_2 :: T_Interfaces_2
                                                                             sem_Interfaces_Nil_2 =
                                                                                 (T_Interfaces_2 (\ (!_lhsIallInters)
                                                                                                    (!_lhsIddp) ->
                                                                                                      (case (({-# LINE 438 "./src-ag/InterfacesRules.lag" #-}
                                                                                                              Seq.empty
                                                                                                              {-# LINE 658 "dist/build/InterfacesRules.hs" #-}
                                                                                                              )) of
                                                                                                       { !_lhsOedp ->
                                                                                                       (case (({-# LINE 389 "./src-ag/InterfacesRules.lag" #-}
                                                                                                               Map.empty
                                                                                                               {-# LINE 663 "dist/build/InterfacesRules.hs" #-}
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
                    (case (({-# LINE 101 "./src-ag/InterfacesRules.lag" #-}
                            \a -> ruleTable _lhsIinfo ! a
                            {-# LINE 792 "dist/build/InterfacesRules.hs" #-}
                            )) of
                     { !_look ->
                     (case (({-# LINE 104 "./src-ag/InterfacesRules.lag" #-}
                             \p us -> [ a  |  u <- us
                                           ,  a <- tdsToTdp _lhsIinfo ! u
                                           ,  p (_look a)]
                             {-# LINE 799 "dist/build/InterfacesRules.hs" #-}
                             )) of
                      { !_occurAs ->
                      (case (({-# LINE 108 "./src-ag/InterfacesRules.lag" #-}
                              let group as = gather _lhsIinfo (_occurAs isRhs as)
                              in map (partition (isInh . _look)) (group (inh_ ++ syn_))
                              {-# LINE 805 "dist/build/InterfacesRules.hs" #-}
                              )) of
                       { !_groups ->
                       (case (({-# LINE 111 "./src-ag/InterfacesRules.lag" #-}
                               _lhsIv + length _groups
                               {-# LINE 810 "dist/build/InterfacesRules.hs" #-}
                               )) of
                        { !_v ->
                        (case (({-# LINE 77 "./src-ag/InterfacesRules.lag" #-}
                                _v
                                {-# LINE 815 "dist/build/InterfacesRules.hs" #-}
                                )) of
                         { !_lhsOv ->
                         (case ((let sem_Segment_Segment_1 :: T_Segment_1
                                     sem_Segment_Segment_1 =
                                         (T_Segment_1 (\ (!_lhsIn)
                                                         (!_lhsInextNewvertices) ->
                                                           (case (({-# LINE 112 "./src-ag/InterfacesRules.lag" #-}
                                                                   [_lhsIv .. _v    -1]
                                                                   {-# LINE 824 "dist/build/InterfacesRules.hs" #-}
                                                                   )) of
                                                            { !_newvertices ->
                                                            (case (({-# LINE 127 "./src-ag/InterfacesRules.lag" #-}
                                                                    Seq.fromList $ zipWith (cv _look _lhsIn) _newvertices _groups
                                                                    {-# LINE 829 "dist/build/InterfacesRules.hs" #-}
                                                                    )) of
                                                             { !_lhsOdescr ->
                                                             (case (({-# LINE 178 "./src-ag/InterfacesRules.lag" #-}
                                                                     _groups
                                                                     {-# LINE 834 "dist/build/InterfacesRules.hs" #-}
                                                                     )) of
                                                              { !_lhsOgroups ->
                                                              (case (({-# LINE 170 "./src-ag/InterfacesRules.lag" #-}
                                                                      zip _newvertices _lhsInextNewvertices
                                                                      {-# LINE 839 "dist/build/InterfacesRules.hs" #-}
                                                                      )) of
                                                               { !_visitedges ->
                                                               (case (({-# LINE 150 "./src-ag/InterfacesRules.lag" #-}
                                                                       concat (zipWith ed _newvertices _groups)
                                                                       {-# LINE 844 "dist/build/InterfacesRules.hs" #-}
                                                                       )) of
                                                                { !_attredges ->
                                                                (case (({-# LINE 171 "./src-ag/InterfacesRules.lag" #-}
                                                                        Seq.fromList _attredges Seq.>< Seq.fromList _visitedges
                                                                        {-# LINE 849 "dist/build/InterfacesRules.hs" #-}
                                                                        )) of
                                                                 { !_lhsOnewedges ->
                                                                 (case (({-# LINE 162 "./src-ag/InterfacesRules.lag" #-}
                                                                         _newvertices
                                                                         {-# LINE 854 "dist/build/InterfacesRules.hs" #-}
                                                                         )) of
                                                                  { !_lhsOnewvertices ->
                                                                  (case ((let sem_Segment_Segment_2 :: T_Segment_2
                                                                              sem_Segment_Segment_2 =
                                                                                  (T_Segment_2 (\ (!_lhsIcons)
                                                                                                  (!_lhsIprev)
                                                                                                  (!_lhsIvisitDescr)
                                                                                                  (!_lhsIvssGraph) ->
                                                                                                    (case (({-# LINE 225 "./src-ag/InterfacesRules.lag" #-}
                                                                                                            gather _lhsIinfo (_occurAs isLhs syn_)
                                                                                                            {-# LINE 865 "dist/build/InterfacesRules.hs" #-}
                                                                                                            )) of
                                                                                                     { !_synOccur ->
                                                                                                     (case (({-# LINE 226 "./src-ag/InterfacesRules.lag" #-}
                                                                                                             let hasCode' v | inRange (bounds (ruleTable _lhsIinfo)) v =  getHasCode (ruleTable _lhsIinfo ! v)
                                                                                                                            | otherwise = True
                                                                                                             in if  null syn_
                                                                                                                    then replicate (length _lhsIcons) []
                                                                                                                    else map (filter hasCode' . topSort' _lhsIvssGraph) _synOccur
                                                                                                             {-# LINE 874 "dist/build/InterfacesRules.hs" #-}
                                                                                                             )) of
                                                                                                      { !_vss ->
                                                                                                      (case (({-# LINE 270 "./src-ag/InterfacesRules.lag" #-}
                                                                                                              map (\\ _lhsIprev) _vss
                                                                                                              {-# LINE 879 "dist/build/InterfacesRules.hs" #-}
                                                                                                              )) of
                                                                                                       { !_visitss' ->
                                                                                                       (case (({-# LINE 284 "./src-ag/InterfacesRules.lag" #-}
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
                                                                                                               {-# LINE 899 "dist/build/InterfacesRules.hs" #-}
                                                                                                               )) of
                                                                                                        { !_visitss ->
                                                                                                        (case (({-# LINE 271 "./src-ag/InterfacesRules.lag" #-}
                                                                                                                let defines v = case Map.lookup v _lhsIvisitDescr of
                                                                                                                                  Nothing -> [v]
                                                                                                                                  Just (ChildVisit _ _ _ inh _) -> v:inh
                                                                                                                in concatMap (concatMap defines) _visitss
                                                                                                                {-# LINE 907 "dist/build/InterfacesRules.hs" #-}
                                                                                                                )) of
                                                                                                         { !_defined ->
                                                                                                         (case (({-# LINE 275 "./src-ag/InterfacesRules.lag" #-}
                                                                                                                 _lhsIprev ++ _defined
                                                                                                                 {-# LINE 912 "dist/build/InterfacesRules.hs" #-}
                                                                                                                 )) of
                                                                                                          { !_lhsOprev ->
                                                                                                          (case (({-# LINE 410 "./src-ag/InterfacesRules.lag" #-}
                                                                                                                  let makemap = Map.fromList . map findType
                                                                                                                      findType v = getNtaNameType (attrTable _lhsIinfo ! v)
                                                                                                                  in (makemap inh_,makemap syn_)
                                                                                                                  {-# LINE 919 "dist/build/InterfacesRules.hs" #-}
                                                                                                                  )) of
                                                                                                           { !__tup1 ->
                                                                                                           (case (({-# LINE 410 "./src-ag/InterfacesRules.lag" #-}
                                                                                                                   __tup1
                                                                                                                   {-# LINE 924 "dist/build/InterfacesRules.hs" #-}
                                                                                                                   )) of
                                                                                                            { !(_,!_synmap) ->
                                                                                                            (case (({-# LINE 410 "./src-ag/InterfacesRules.lag" #-}
                                                                                                                    __tup1
                                                                                                                    {-# LINE 929 "dist/build/InterfacesRules.hs" #-}
                                                                                                                    )) of
                                                                                                             { !(!_inhmap,_) ->
                                                                                                             (case (({-# LINE 406 "./src-ag/InterfacesRules.lag" #-}
                                                                                                                     if False then undefined _lhsIvssGraph _lhsIvisitDescr _lhsIprev else CSegment _inhmap _synmap
                                                                                                                     {-# LINE 934 "dist/build/InterfacesRules.hs" #-}
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
                                                                                                                                                (case (({-# LINE 358 "./src-ag/InterfacesRules.lag" #-}
                                                                                                                                                        let computes v = case Map.lookup v _lhsIvisitDescr of
                                                                                                                                                                           Nothing -> Map.keys (getDefines (ruleTable _lhsIinfo ! v))
                                                                                                                                                                           Just (ChildVisit _ _ _ _ syn) -> v:syn
                                                                                                                                                        in concatMap (concatMap computes) _visitss
                                                                                                                                                        {-# LINE 950 "dist/build/InterfacesRules.hs" #-}
                                                                                                                                                        )) of
                                                                                                                                                 { !_computed ->
                                                                                                                                                 (case (({-# LINE 357 "./src-ag/InterfacesRules.lag" #-}
                                                                                                                                                         _occurAs isLhs inh_ ++ _lhsIfromLhs
                                                                                                                                                         {-# LINE 955 "dist/build/InterfacesRules.hs" #-}
                                                                                                                                                         )) of
                                                                                                                                                  { !_fromLhs ->
                                                                                                                                                  (case (({-# LINE 363 "./src-ag/InterfacesRules.lag" #-}
                                                                                                                                                          \vs next ->
                                                                                                                                                            let needed = concatMap (_lhsIddp !) vs
                                                                                                                                                            in nub (needed ++ next) \\ (_fromLhs ++ _computed)
                                                                                                                                                          {-# LINE 962 "dist/build/InterfacesRules.hs" #-}
                                                                                                                                                          )) of
                                                                                                                                                   { !_iv ->
                                                                                                                                                   (case (({-# LINE 362 "./src-ag/InterfacesRules.lag" #-}
                                                                                                                                                           zipWith _iv _visitss _lhsInextIntravisits
                                                                                                                                                           {-# LINE 967 "dist/build/InterfacesRules.hs" #-}
                                                                                                                                                           )) of
                                                                                                                                                    { !_intravisits ->
                                                                                                                                                    (case (({-# LINE 413 "./src-ag/InterfacesRules.lag" #-}
                                                                                                                                                            let  mkVisit vss intra = CVisit _inhmap _synmap (mkSequence vss) (mkSequence intra) True
                                                                                                                                                                 mkSequence = map mkRule
                                                                                                                                                                 mkRule v = case Map.lookup v _lhsIvisitDescr of
                                                                                                                                                                              Nothing -> ruleTable _lhsIinfo ! v
                                                                                                                                                                              Just (ChildVisit name nt n _ _) -> ccv name nt n _lhsIallInters
                                                                                                                                                            in zipWith mkVisit _visitss _intravisits
                                                                                                                                                            {-# LINE 977 "dist/build/InterfacesRules.hs" #-}
                                                                                                                                                            )) of
                                                                                                                                                     { !_lhsOcvisits ->
                                                                                                                                                     (case (({-# LINE 440 "./src-ag/InterfacesRules.lag" #-}
                                                                                                                                                             Seq.fromList [(i,s) | i <- inh_, s <- syn_]
                                                                                                                                                             Seq.>< Seq.fromList [(s,i) | s <- syn_, i <- _lhsInextInh ]
                                                                                                                                                             {-# LINE 983 "dist/build/InterfacesRules.hs" #-}
                                                                                                                                                             )) of
                                                                                                                                                      { !_lhsOedp ->
                                                                                                                                                      (case (({-# LINE 445 "./src-ag/InterfacesRules.lag" #-}
                                                                                                                                                              inh_
                                                                                                                                                              {-# LINE 988 "dist/build/InterfacesRules.hs" #-}
                                                                                                                                                              )) of
                                                                                                                                                       { !_lhsOinh ->
                                                                                                                                                       (case (({-# LINE 327 "./src-ag/InterfacesRules.lag" #-}
                                                                                                                                                               _intravisits
                                                                                                                                                               {-# LINE 993 "dist/build/InterfacesRules.hs" #-}
                                                                                                                                                               )) of
                                                                                                                                                        { !_lhsOintravisits ->
                                                                                                                                                        (case (({-# LINE 269 "./src-ag/InterfacesRules.lag" #-}
                                                                                                                                                                _visitss
                                                                                                                                                                {-# LINE 998 "dist/build/InterfacesRules.hs" #-}
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
                     (case (({-# LINE 77 "./src-ag/InterfacesRules.lag" #-}
                             _lhsIv
                             {-# LINE 1089 "dist/build/InterfacesRules.hs" #-}
                             )) of
                      { !_hdOv ->
                      (case (({-# LINE 78 "./src-ag/InterfacesRules.lag" #-}
                              _lhsIinfo
                              {-# LINE 1094 "dist/build/InterfacesRules.hs" #-}
                              )) of
                       { !_hdOinfo ->
                       (case (hd_ _hdOinfo _hdOv) of
                        { ( !_hdIv,!T_Segment_1 hd_1) ->
                            (case (({-# LINE 77 "./src-ag/InterfacesRules.lag" #-}
                                    _hdIv
                                    {-# LINE 1101 "dist/build/InterfacesRules.hs" #-}
                                    )) of
                             { !_tlOv ->
                             (case (({-# LINE 78 "./src-ag/InterfacesRules.lag" #-}
                                     _lhsIinfo
                                     {-# LINE 1106 "dist/build/InterfacesRules.hs" #-}
                                     )) of
                              { !_tlOinfo ->
                              (case (({-# LINE 199 "./src-ag/InterfacesRules.lag" #-}
                                      _lhsIn
                                      {-# LINE 1111 "dist/build/InterfacesRules.hs" #-}
                                      )) of
                               { !_hdOn ->
                               (case (({-# LINE 203 "./src-ag/InterfacesRules.lag" #-}
                                       _lhsIn + 1
                                       {-# LINE 1116 "dist/build/InterfacesRules.hs" #-}
                                       )) of
                                { !_tlOn ->
                                (case (tl_ _tlOinfo _tlOn _tlOv) of
                                 { ( !_tlIdescr,!_tlIgroups,!_tlInewedges,!_tlInewvertices,!_tlIv,!T_Segments_1 tl_1) ->
                                     (case (({-# LINE 165 "./src-ag/InterfacesRules.lag" #-}
                                             _tlInewvertices
                                             {-# LINE 1123 "dist/build/InterfacesRules.hs" #-}
                                             )) of
                                      { !_hdOnextNewvertices ->
                                      (case (hd_1 _hdOn _hdOnextNewvertices) of
                                       { ( !_hdIdescr,!_hdIgroups,!_hdInewedges,!_hdInewvertices,!T_Segment_2 hd_2) ->
                                           (case (({-# LINE 125 "./src-ag/InterfacesRules.lag" #-}
                                                   _hdIdescr Seq.>< _tlIdescr
                                                   {-# LINE 1130 "dist/build/InterfacesRules.hs" #-}
                                                   )) of
                                            { !_lhsOdescr ->
                                            (case (({-# LINE 180 "./src-ag/InterfacesRules.lag" #-}
                                                    _hdIgroups
                                                    {-# LINE 1135 "dist/build/InterfacesRules.hs" #-}
                                                    )) of
                                             { !_lhsOgroups ->
                                             (case (({-# LINE 124 "./src-ag/InterfacesRules.lag" #-}
                                                     _hdInewedges Seq.>< _tlInewedges
                                                     {-# LINE 1140 "dist/build/InterfacesRules.hs" #-}
                                                     )) of
                                              { !_lhsOnewedges ->
                                              (case (({-# LINE 166 "./src-ag/InterfacesRules.lag" #-}
                                                      _hdInewvertices
                                                      {-# LINE 1145 "dist/build/InterfacesRules.hs" #-}
                                                      )) of
                                               { !_lhsOnewvertices ->
                                               (case (({-# LINE 77 "./src-ag/InterfacesRules.lag" #-}
                                                       _tlIv
                                                       {-# LINE 1150 "dist/build/InterfacesRules.hs" #-}
                                                       )) of
                                                { !_lhsOv ->
                                                (case ((let sem_Segments_Cons_1 :: T_Segments_1
                                                            sem_Segments_Cons_1 =
                                                                (T_Segments_1 (\ (!_lhsIcons)
                                                                                 (!_lhsIprev)
                                                                                 (!_lhsIvisitDescr)
                                                                                 (!_lhsIvssGraph) ->
                                                                                   (case (({-# LINE 212 "./src-ag/InterfacesRules.lag" #-}
                                                                                           _lhsIvssGraph
                                                                                           {-# LINE 1161 "dist/build/InterfacesRules.hs" #-}
                                                                                           )) of
                                                                                    { !_tlOvssGraph ->
                                                                                    (case (({-# LINE 120 "./src-ag/InterfacesRules.lag" #-}
                                                                                            _lhsIvisitDescr
                                                                                            {-# LINE 1166 "dist/build/InterfacesRules.hs" #-}
                                                                                            )) of
                                                                                     { !_tlOvisitDescr ->
                                                                                     (case (({-# LINE 212 "./src-ag/InterfacesRules.lag" #-}
                                                                                             _lhsIvssGraph
                                                                                             {-# LINE 1171 "dist/build/InterfacesRules.hs" #-}
                                                                                             )) of
                                                                                      { !_hdOvssGraph ->
                                                                                      (case (({-# LINE 120 "./src-ag/InterfacesRules.lag" #-}
                                                                                              _lhsIvisitDescr
                                                                                              {-# LINE 1176 "dist/build/InterfacesRules.hs" #-}
                                                                                              )) of
                                                                                       { !_hdOvisitDescr ->
                                                                                       (case (({-# LINE 263 "./src-ag/InterfacesRules.lag" #-}
                                                                                               _lhsIprev
                                                                                               {-# LINE 1181 "dist/build/InterfacesRules.hs" #-}
                                                                                               )) of
                                                                                        { !_hdOprev ->
                                                                                        (case (({-# LINE 231 "./src-ag/InterfacesRules.lag" #-}
                                                                                                _lhsIcons
                                                                                                {-# LINE 1186 "dist/build/InterfacesRules.hs" #-}
                                                                                                )) of
                                                                                         { !_hdOcons ->
                                                                                         (case (hd_2 _hdOcons _hdOprev _hdOvisitDescr _hdOvssGraph) of
                                                                                          { ( !_hdIprev,!_hdIseg,!T_Segment_3 hd_3) ->
                                                                                              (case (({-# LINE 263 "./src-ag/InterfacesRules.lag" #-}
                                                                                                      _hdIprev
                                                                                                      {-# LINE 1193 "dist/build/InterfacesRules.hs" #-}
                                                                                                      )) of
                                                                                               { !_tlOprev ->
                                                                                               (case (({-# LINE 231 "./src-ag/InterfacesRules.lag" #-}
                                                                                                       _lhsIcons
                                                                                                       {-# LINE 1198 "dist/build/InterfacesRules.hs" #-}
                                                                                                       )) of
                                                                                                { !_tlOcons ->
                                                                                                (case (tl_1 _tlOcons _tlOprev _tlOvisitDescr _tlOvssGraph) of
                                                                                                 { ( !_tlIsegs,!T_Segments_2 tl_2) ->
                                                                                                     (case (({-# LINE 401 "./src-ag/InterfacesRules.lag" #-}
                                                                                                             _hdIseg : _tlIsegs
                                                                                                             {-# LINE 1205 "dist/build/InterfacesRules.hs" #-}
                                                                                                             )) of
                                                                                                      { !_lhsOsegs ->
                                                                                                      (case ((let sem_Segments_Cons_2 :: T_Segments_2
                                                                                                                  sem_Segments_Cons_2 =
                                                                                                                      (T_Segments_2 (\ (!_lhsIallInters)
                                                                                                                                       (!_lhsIddp)
                                                                                                                                       (!_lhsIfromLhs)
                                                                                                                                       (!_lhsIisFirst) ->
                                                                                                                                         (case (({-# LINE 341 "./src-ag/InterfacesRules.lag" #-}
                                                                                                                                                 _lhsIddp
                                                                                                                                                 {-# LINE 1216 "dist/build/InterfacesRules.hs" #-}
                                                                                                                                                 )) of
                                                                                                                                          { !_tlOddp ->
                                                                                                                                          (case (({-# LINE 379 "./src-ag/InterfacesRules.lag" #-}
                                                                                                                                                  _lhsIallInters
                                                                                                                                                  {-# LINE 1221 "dist/build/InterfacesRules.hs" #-}
                                                                                                                                                  )) of
                                                                                                                                           { !_tlOallInters ->
                                                                                                                                           (case (({-# LINE 341 "./src-ag/InterfacesRules.lag" #-}
                                                                                                                                                   _lhsIddp
                                                                                                                                                   {-# LINE 1226 "dist/build/InterfacesRules.hs" #-}
                                                                                                                                                   )) of
                                                                                                                                            { !_hdOddp ->
                                                                                                                                            (case (({-# LINE 379 "./src-ag/InterfacesRules.lag" #-}
                                                                                                                                                    _lhsIallInters
                                                                                                                                                    {-# LINE 1231 "dist/build/InterfacesRules.hs" #-}
                                                                                                                                                    )) of
                                                                                                                                             { !_hdOallInters ->
                                                                                                                                             (case (({-# LINE 355 "./src-ag/InterfacesRules.lag" #-}
                                                                                                                                                     []
                                                                                                                                                     {-# LINE 1236 "dist/build/InterfacesRules.hs" #-}
                                                                                                                                                     )) of
                                                                                                                                              { !_tlOfromLhs ->
                                                                                                                                              (case (({-# LINE 354 "./src-ag/InterfacesRules.lag" #-}
                                                                                                                                                      _lhsIfromLhs
                                                                                                                                                      {-# LINE 1241 "dist/build/InterfacesRules.hs" #-}
                                                                                                                                                      )) of
                                                                                                                                               { !_hdOfromLhs ->
                                                                                                                                               (case (({-# LINE 316 "./src-ag/InterfacesRules.lag" #-}
                                                                                                                                                       False
                                                                                                                                                       {-# LINE 1246 "dist/build/InterfacesRules.hs" #-}
                                                                                                                                                       )) of
                                                                                                                                                { !_tlOisFirst ->
                                                                                                                                                (case (tl_2 _tlOallInters _tlOddp _tlOfromLhs _tlOisFirst) of
                                                                                                                                                 { ( !_tlIcvisits,!_tlIedp,!_tlIfirstInh,!_tlIhdIntravisits,!_tlIprev) ->
                                                                                                                                                     (case (({-# LINE 329 "./src-ag/InterfacesRules.lag" #-}
                                                                                                                                                             _tlIhdIntravisits
                                                                                                                                                             {-# LINE 1253 "dist/build/InterfacesRules.hs" #-}
                                                                                                                                                             )) of
                                                                                                                                                      { !_hdOnextIntravisits ->
                                                                                                                                                      (case (({-# LINE 312 "./src-ag/InterfacesRules.lag" #-}
                                                                                                                                                              _lhsIisFirst
                                                                                                                                                              {-# LINE 1258 "dist/build/InterfacesRules.hs" #-}
                                                                                                                                                              )) of
                                                                                                                                                       { !_hdOisFirst ->
                                                                                                                                                       (case (({-# LINE 447 "./src-ag/InterfacesRules.lag" #-}
                                                                                                                                                               _tlIfirstInh
                                                                                                                                                               {-# LINE 1263 "dist/build/InterfacesRules.hs" #-}
                                                                                                                                                               )) of
                                                                                                                                                        { !_hdOnextInh ->
                                                                                                                                                        (case (hd_3 _hdOallInters _hdOddp _hdOfromLhs _hdOisFirst _hdOnextInh _hdOnextIntravisits) of
                                                                                                                                                         { ( !_hdIcvisits,!_hdIedp,!_hdIinh,!_hdIintravisits,!_hdIvisitss) ->
                                                                                                                                                             (case (({-# LINE 400 "./src-ag/InterfacesRules.lag" #-}
                                                                                                                                                                     _hdIcvisits : _tlIcvisits
                                                                                                                                                                     {-# LINE 1270 "dist/build/InterfacesRules.hs" #-}
                                                                                                                                                                     )) of
                                                                                                                                                              { !_lhsOcvisits ->
                                                                                                                                                              (case (({-# LINE 438 "./src-ag/InterfacesRules.lag" #-}
                                                                                                                                                                      _hdIedp Seq.>< _tlIedp
                                                                                                                                                                      {-# LINE 1275 "dist/build/InterfacesRules.hs" #-}
                                                                                                                                                                      )) of
                                                                                                                                                               { !_lhsOedp ->
                                                                                                                                                               (case (({-# LINE 448 "./src-ag/InterfacesRules.lag" #-}
                                                                                                                                                                       _hdIinh
                                                                                                                                                                       {-# LINE 1280 "dist/build/InterfacesRules.hs" #-}
                                                                                                                                                                       )) of
                                                                                                                                                                { !_lhsOfirstInh ->
                                                                                                                                                                (case (({-# LINE 330 "./src-ag/InterfacesRules.lag" #-}
                                                                                                                                                                        _hdIintravisits
                                                                                                                                                                        {-# LINE 1285 "dist/build/InterfacesRules.hs" #-}
                                                                                                                                                                        )) of
                                                                                                                                                                 { !_lhsOhdIntravisits ->
                                                                                                                                                                 (case (({-# LINE 263 "./src-ag/InterfacesRules.lag" #-}
                                                                                                                                                                         _tlIprev
                                                                                                                                                                         {-# LINE 1290 "dist/build/InterfacesRules.hs" #-}
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
                     (case (({-# LINE 125 "./src-ag/InterfacesRules.lag" #-}
                             Seq.empty
                             {-# LINE 1307 "dist/build/InterfacesRules.hs" #-}
                             )) of
                      { !_lhsOdescr ->
                      (case (({-# LINE 181 "./src-ag/InterfacesRules.lag" #-}
                              []
                              {-# LINE 1312 "dist/build/InterfacesRules.hs" #-}
                              )) of
                       { !_lhsOgroups ->
                       (case (({-# LINE 124 "./src-ag/InterfacesRules.lag" #-}
                               Seq.empty
                               {-# LINE 1317 "dist/build/InterfacesRules.hs" #-}
                               )) of
                        { !_lhsOnewedges ->
                        (case (({-# LINE 167 "./src-ag/InterfacesRules.lag" #-}
                                []
                                {-# LINE 1322 "dist/build/InterfacesRules.hs" #-}
                                )) of
                         { !_lhsOnewvertices ->
                         (case (({-# LINE 77 "./src-ag/InterfacesRules.lag" #-}
                                 _lhsIv
                                 {-# LINE 1327 "dist/build/InterfacesRules.hs" #-}
                                 )) of
                          { !_lhsOv ->
                          (case ((let sem_Segments_Nil_1 :: T_Segments_1
                                      sem_Segments_Nil_1 =
                                          (T_Segments_1 (\ (!_lhsIcons)
                                                           (!_lhsIprev)
                                                           (!_lhsIvisitDescr)
                                                           (!_lhsIvssGraph) ->
                                                             (case (({-# LINE 402 "./src-ag/InterfacesRules.lag" #-}
                                                                     []
                                                                     {-# LINE 1338 "dist/build/InterfacesRules.hs" #-}
                                                                     )) of
                                                              { !_lhsOsegs ->
                                                              (case ((let sem_Segments_Nil_2 :: T_Segments_2
                                                                          sem_Segments_Nil_2 =
                                                                              (T_Segments_2 (\ (!_lhsIallInters)
                                                                                               (!_lhsIddp)
                                                                                               (!_lhsIfromLhs)
                                                                                               (!_lhsIisFirst) ->
                                                                                                 (case (({-# LINE 400 "./src-ag/InterfacesRules.lag" #-}
                                                                                                         []
                                                                                                         {-# LINE 1349 "dist/build/InterfacesRules.hs" #-}
                                                                                                         )) of
                                                                                                  { !_lhsOcvisits ->
                                                                                                  (case (({-# LINE 438 "./src-ag/InterfacesRules.lag" #-}
                                                                                                          Seq.empty
                                                                                                          {-# LINE 1354 "dist/build/InterfacesRules.hs" #-}
                                                                                                          )) of
                                                                                                   { !_lhsOedp ->
                                                                                                   (case (({-# LINE 449 "./src-ag/InterfacesRules.lag" #-}
                                                                                                           []
                                                                                                           {-# LINE 1359 "dist/build/InterfacesRules.hs" #-}
                                                                                                           )) of
                                                                                                    { !_lhsOfirstInh ->
                                                                                                    (case (({-# LINE 331 "./src-ag/InterfacesRules.lag" #-}
                                                                                                            repeat []
                                                                                                            {-# LINE 1364 "dist/build/InterfacesRules.hs" #-}
                                                                                                            )) of
                                                                                                     { !_lhsOhdIntravisits ->
                                                                                                     (case (({-# LINE 263 "./src-ag/InterfacesRules.lag" #-}
                                                                                                             _lhsIprev
                                                                                                             {-# LINE 1369 "dist/build/InterfacesRules.hs" #-}
                                                                                                             )) of
                                                                                                      { !_lhsOprev ->
                                                                                                      ( _lhsOcvisits,_lhsOedp,_lhsOfirstInh,_lhsOhdIntravisits,_lhsOprev) }) }) }) }) })))
                                                                      in  sem_Segments_Nil_2)) of
                                                               { ( !sem_Segments_2) ->
                                                               ( _lhsOsegs,sem_Segments_2) }) })))
                                  in  sem_Segments_Nil_1)) of
                           { ( !sem_Segments_1) ->
                           ( _lhsOdescr,_lhsOgroups,_lhsOnewedges,_lhsOnewvertices,_lhsOv,sem_Segments_1) }) }) }) }) }) })))