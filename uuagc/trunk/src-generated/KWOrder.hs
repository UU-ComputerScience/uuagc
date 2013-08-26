{-# LANGUAGE Rank2Types, GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module KWOrder where
{-# LINE 2 "./src-ag/Patterns.ag" #-}

-- Patterns.ag imports
import UU.Scanner.Position(Pos)
import CommonTypes (ConstructorIdent,Identifier)
{-# LINE 11 "dist/build/KWOrder.hs" #-}

{-# LINE 2 "./src-ag/Expression.ag" #-}

import UU.Scanner.Position(Pos)
import HsToken
{-# LINE 17 "dist/build/KWOrder.hs" #-}

{-# LINE 2 "./src-ag/HsToken.ag" #-}

import CommonTypes
import UU.Scanner.Position(Pos)
{-# LINE 23 "dist/build/KWOrder.hs" #-}

{-# LINE 2 "./src-ag/AbstractSyntax.ag" #-}

-- AbstractSyntax.ag imports
import Data.Set(Set)
import Data.Map(Map)
import Patterns    (Pattern(..),Patterns)
import Expression  (Expression(..))
import Macro --marcos
import CommonTypes
import ErrorMessages
{-# LINE 35 "dist/build/KWOrder.hs" #-}

{-# LINE 8 "./src-ag/KWOrder.ag" #-}

import AbstractSyntax
import HsToken
import Expression
import Patterns
import Options
import PPUtil
import Pretty
import Knuth1
import KennedyWarren
import ExecutionPlan
import Data.Maybe
import Debug.Trace
import Data.Set(Set)
import Data.Map(Map)
import Data.Sequence(Seq)
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Data.Monoid(mappend,mempty)
{-# LINE 58 "dist/build/KWOrder.hs" #-}
import Control.Monad.Identity (Identity)
import qualified Control.Monad.Identity
{-# LINE 275 "./src-ag/KWOrder.ag" #-}

-- a depends on b, thus a is a successor of b
depToEdge :: Dependency -> Edge
depToEdge (Dependency a b) = (occToVertex False b, occToVertex True a)

occToVertex :: Bool -> Occurrence -> Vertex
occToVertex _ (OccRule nm) = VRule nm
occToVertex isDependency (OccAttr c a)
  | c == _LOC  = VAttr Syn c a   -- local attributes are treated as synthesized attrs of 'loc'
  | c == _INST = VChild a        -- higher-order attributes are treated as children
  | otherwise  = VAttr kind c a where
      kind | isDependency && c == _LHS     = Inh     -- these dependencies have the property that
           | isDependency && c /= _LHS     = Syn     -- they can all be faked by writing a 'const' rule
           | not isDependency && c == _LHS = Syn     -- Perhaps we should also allow other forms of dependencies
           | not isDependency && c /= _LHS = Inh     -- as well, such as two inherited attributes, which would
                                                     -- force them in different visits
{-# LINE 78 "dist/build/KWOrder.hs" #-}
-- Child -------------------------------------------------------
-- wrapper
data Inh_Child  = Inh_Child { aroundMap_Inh_Child :: (Map Identifier [Expression]), inhMap_Inh_Child :: (Map Identifier Attributes), mergeMap_Inh_Child :: (Map Identifier (Identifier, [Identifier], Expression)), mergedChildren_Inh_Child :: (Set Identifier), options_Inh_Child :: (Options), synMap_Inh_Child :: (Map Identifier Attributes) }
data Syn_Child  = Syn_Child { echilds_Syn_Child :: (EChild), edges_Syn_Child :: (Set.Set Edge), nontnames_Syn_Child :: ([(Identifier, Identifier)]), refHoNts_Syn_Child :: (Set NontermIdent), refNts_Syn_Child :: (Set NontermIdent), vertices_Syn_Child :: (Set.Set Vertex) }
{-# INLINABLE wrap_Child #-}
wrap_Child :: T_Child  -> Inh_Child  -> (Syn_Child )
wrap_Child (T_Child act) (Inh_Child _lhsIaroundMap _lhsIinhMap _lhsImergeMap _lhsImergedChildren _lhsIoptions _lhsIsynMap) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Child_vIn1 _lhsIaroundMap _lhsIinhMap _lhsImergeMap _lhsImergedChildren _lhsIoptions _lhsIsynMap
        (T_Child_vOut1 _lhsOechilds _lhsOedges _lhsOnontnames _lhsOrefHoNts _lhsOrefNts _lhsOvertices) <- return (inv_Child_s2 sem arg)
        return (Syn_Child _lhsOechilds _lhsOedges _lhsOnontnames _lhsOrefHoNts _lhsOrefNts _lhsOvertices)
   )

-- cata
{-# INLINE sem_Child #-}
sem_Child :: Child  -> T_Child 
sem_Child ( Child name_ tp_ kind_ ) = sem_Child_Child name_ tp_ kind_

-- semantic domain
newtype T_Child  = T_Child {
                           attach_T_Child :: Identity (T_Child_s2 )
                           }
newtype T_Child_s2  = C_Child_s2 {
                                 inv_Child_s2 :: (T_Child_v1 )
                                 }
data T_Child_s3  = C_Child_s3
type T_Child_v1  = (T_Child_vIn1 ) -> (T_Child_vOut1 )
data T_Child_vIn1  = T_Child_vIn1 (Map Identifier [Expression]) (Map Identifier Attributes) (Map Identifier (Identifier, [Identifier], Expression)) (Set Identifier) (Options) (Map Identifier Attributes)
data T_Child_vOut1  = T_Child_vOut1 (EChild) (Set.Set Edge) ([(Identifier, Identifier)]) (Set NontermIdent) (Set NontermIdent) (Set.Set Vertex)
{-# NOINLINE sem_Child_Child #-}
sem_Child_Child :: (Identifier) -> (Type) -> (ChildKind) -> T_Child 
sem_Child_Child arg_name_ arg_tp_ arg_kind_ = T_Child (return st2) where
   {-# NOINLINE st2 #-}
   st2 = let
      v1 :: T_Child_v1 
      v1 = \ (T_Child_vIn1 _lhsIaroundMap _lhsIinhMap _lhsImergeMap _lhsImergedChildren _lhsIoptions _lhsIsynMap) -> ( let
         _chnt = rule0 arg_name_ arg_tp_
         _inh = rule1 _chnt _lhsIinhMap
         _syn = rule2 _chnt _lhsIsynMap
         _refNts = rule3 arg_tp_
         _refHoNts = rule4 _isHigherOrder _refNts
         _isHigherOrder = rule5 arg_kind_
         _hasArounds = rule6 _lhsIaroundMap arg_name_
         _merges = rule7 _lhsImergeMap arg_name_
         _isMerged = rule8 _lhsImergedChildren arg_name_
         _lhsOechilds :: EChild
         _lhsOechilds = rule9 _hasArounds _isMerged _merges arg_kind_ arg_name_ arg_tp_
         _vertex = rule10 arg_name_
         _synvertices = rule11 _syn arg_name_
         _inhvertices = rule12 _inh arg_name_
         _lhsOvertices :: Set.Set Vertex
         _lhsOvertices = rule13 _inhvertices _synvertices _vertex arg_tp_
         _childIsDeforested = rule14 arg_tp_
         _higherOrderEdges = rule15 _childIsDeforested _lhsIoptions _vertex arg_kind_
         _aroundEdges = rule16 _hasArounds _vertex arg_name_
         _edgesout = rule17 _higherOrderEdges
         _edgesin = rule18 _synvertices _vertex
         _lhsOedges :: Set.Set Edge
         _lhsOedges = rule19 _edgesin _edgesout
         _lhsOnontnames :: [(Identifier, Identifier)]
         _lhsOnontnames = rule20 arg_name_ arg_tp_
         _lhsOrefHoNts :: Set NontermIdent
         _lhsOrefHoNts = rule21 _refHoNts
         _lhsOrefNts :: Set NontermIdent
         _lhsOrefNts = rule22 _refNts
         __result_ = T_Child_vOut1 _lhsOechilds _lhsOedges _lhsOnontnames _lhsOrefHoNts _lhsOrefNts _lhsOvertices
         in __result_ )
     in C_Child_s2 v1
   {-# INLINE rule0 #-}
   {-# LINE 19 "./src-ag/DistChildAttr.ag" #-}
   rule0 = \ name_ tp_ ->
                       {-# LINE 19 "./src-ag/DistChildAttr.ag" #-}
                       case tp_ of
                         NT nt _ _ -> nt
                         Self      -> error ("The type of child " ++ show name_ ++ " should not be a Self type.")
                         Haskell t -> identifier ""
                       {-# LINE 156 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule1 #-}
   {-# LINE 23 "./src-ag/DistChildAttr.ag" #-}
   rule1 = \ _chnt ((_lhsIinhMap) :: Map Identifier Attributes) ->
                      {-# LINE 23 "./src-ag/DistChildAttr.ag" #-}
                      Map.findWithDefault Map.empty _chnt     _lhsIinhMap
                      {-# LINE 162 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule2 #-}
   {-# LINE 24 "./src-ag/DistChildAttr.ag" #-}
   rule2 = \ _chnt ((_lhsIsynMap) :: Map Identifier Attributes) ->
                      {-# LINE 24 "./src-ag/DistChildAttr.ag" #-}
                      Map.findWithDefault Map.empty _chnt     _lhsIsynMap
                      {-# LINE 168 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule3 #-}
   {-# LINE 74 "./src-ag/KWOrder.ag" #-}
   rule3 = \ tp_ ->
                 {-# LINE 74 "./src-ag/KWOrder.ag" #-}
                 case tp_ of
                   NT nt _ _ -> Set.singleton nt
                   _         -> mempty
                 {-# LINE 176 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule4 #-}
   {-# LINE 77 "./src-ag/KWOrder.ag" #-}
   rule4 = \ _isHigherOrder _refNts ->
                   {-# LINE 77 "./src-ag/KWOrder.ag" #-}
                   if _isHigherOrder     then _refNts     else mempty
                   {-# LINE 182 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule5 #-}
   {-# LINE 78 "./src-ag/KWOrder.ag" #-}
   rule5 = \ kind_ ->
                        {-# LINE 78 "./src-ag/KWOrder.ag" #-}
                        case kind_ of
                          ChildSyntax -> False
                          _           -> True
                        {-# LINE 190 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule6 #-}
   {-# LINE 108 "./src-ag/KWOrder.ag" #-}
   rule6 = \ ((_lhsIaroundMap) :: Map Identifier [Expression]) name_ ->
                     {-# LINE 108 "./src-ag/KWOrder.ag" #-}
                     case Map.lookup name_ _lhsIaroundMap of
                       Nothing -> False
                       Just as -> not (null as)
                     {-# LINE 198 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule7 #-}
   {-# LINE 136 "./src-ag/KWOrder.ag" #-}
   rule7 = \ ((_lhsImergeMap) :: Map Identifier (Identifier, [Identifier], Expression)) name_ ->
                   {-# LINE 136 "./src-ag/KWOrder.ag" #-}
                   maybe Nothing (\(_,ms,_) -> Just ms) $ Map.lookup name_ _lhsImergeMap
                   {-# LINE 204 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule8 #-}
   {-# LINE 137 "./src-ag/KWOrder.ag" #-}
   rule8 = \ ((_lhsImergedChildren) :: Set Identifier) name_ ->
                   {-# LINE 137 "./src-ag/KWOrder.ag" #-}
                   name_ `Set.member` _lhsImergedChildren
                   {-# LINE 210 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule9 #-}
   {-# LINE 178 "./src-ag/KWOrder.ag" #-}
   rule9 = \ _hasArounds _isMerged _merges kind_ name_ tp_ ->
                          {-# LINE 178 "./src-ag/KWOrder.ag" #-}
                          case tp_ of
                            NT _ _ _ -> EChild name_ tp_ kind_ _hasArounds     _merges     _isMerged
                            _        -> ETerm name_ tp_
                          {-# LINE 218 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule10 #-}
   {-# LINE 215 "./src-ag/KWOrder.ag" #-}
   rule10 = \ name_ ->
                               {-# LINE 215 "./src-ag/KWOrder.ag" #-}
                               VChild name_
                               {-# LINE 224 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule11 #-}
   {-# LINE 216 "./src-ag/KWOrder.ag" #-}
   rule11 = \ _syn name_ ->
                               {-# LINE 216 "./src-ag/KWOrder.ag" #-}
                               map (VAttr Syn name_) . Map.keys $ _syn
                               {-# LINE 230 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule12 #-}
   {-# LINE 217 "./src-ag/KWOrder.ag" #-}
   rule12 = \ _inh name_ ->
                               {-# LINE 217 "./src-ag/KWOrder.ag" #-}
                               map (VAttr Inh name_) . Map.keys $ _inh
                               {-# LINE 236 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule13 #-}
   {-# LINE 218 "./src-ag/KWOrder.ag" #-}
   rule13 = \ _inhvertices _synvertices _vertex tp_ ->
                               {-# LINE 218 "./src-ag/KWOrder.ag" #-}
                               case tp_ of
                                  NT _ _ _ -> Set.insert _vertex     $ Set.fromList (_synvertices     ++ _inhvertices    )
                                  _        -> Set.empty
                               {-# LINE 244 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule14 #-}
   {-# LINE 248 "./src-ag/KWOrder.ag" #-}
   rule14 = \ tp_ ->
                            {-# LINE 248 "./src-ag/KWOrder.ag" #-}
                            case tp_ of
                              NT _ _ defor -> defor
                              _            -> False
                            {-# LINE 252 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule15 #-}
   {-# LINE 251 "./src-ag/KWOrder.ag" #-}
   rule15 = \ _childIsDeforested ((_lhsIoptions) :: Options) _vertex kind_ ->
                           {-# LINE 251 "./src-ag/KWOrder.ag" #-}
                           case kind_ of
                             ChildAttr | lateHigherOrderBinding _lhsIoptions && not _childIsDeforested
                                          -> [(_vertex    , VAttr Inh _LHS idLateBindingAttr)]
                             _            -> []
                           {-# LINE 261 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule16 #-}
   {-# LINE 255 "./src-ag/KWOrder.ag" #-}
   rule16 = \ _hasArounds _vertex name_ ->
                           {-# LINE 255 "./src-ag/KWOrder.ag" #-}
                           if _hasArounds
                           then [(_vertex    , VAttr Syn _LOC (Ident (getName name_ ++ "_around") (getPos name_)))]
                           else []
                           {-# LINE 269 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule17 #-}
   {-# LINE 261 "./src-ag/KWOrder.ag" #-}
   rule17 = \ _higherOrderEdges ->
                            {-# LINE 261 "./src-ag/KWOrder.ag" #-}
                            _higherOrderEdges
                            {-# LINE 275 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule18 #-}
   {-# LINE 262 "./src-ag/KWOrder.ag" #-}
   rule18 = \ _synvertices _vertex ->
                            {-# LINE 262 "./src-ag/KWOrder.ag" #-}
                            map (flip (,) _vertex    ) _synvertices
                            {-# LINE 281 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule19 #-}
   {-# LINE 263 "./src-ag/KWOrder.ag" #-}
   rule19 = \ _edgesin _edgesout ->
                            {-# LINE 263 "./src-ag/KWOrder.ag" #-}
                            Set.fromList (_edgesout     ++ _edgesin    )
                            {-# LINE 287 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule20 #-}
   {-# LINE 301 "./src-ag/KWOrder.ag" #-}
   rule20 = \ name_ tp_ ->
                             {-# LINE 301 "./src-ag/KWOrder.ag" #-}
                             case tp_ of
                               NT nont _ _ -> [(name_, nont)]
                               _           -> []
                             {-# LINE 295 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule21 #-}
   rule21 = \ _refHoNts ->
     _refHoNts
   {-# INLINE rule22 #-}
   rule22 = \ _refNts ->
     _refNts

-- Children ----------------------------------------------------
-- wrapper
data Inh_Children  = Inh_Children { aroundMap_Inh_Children :: (Map Identifier [Expression]), inhMap_Inh_Children :: (Map Identifier Attributes), mergeMap_Inh_Children :: (Map Identifier (Identifier, [Identifier], Expression)), mergedChildren_Inh_Children :: (Set Identifier), options_Inh_Children :: (Options), synMap_Inh_Children :: (Map Identifier Attributes) }
data Syn_Children  = Syn_Children { echilds_Syn_Children :: (EChildren), edges_Syn_Children :: (Set.Set Edge), nontnames_Syn_Children :: ([(Identifier, Identifier)]), refHoNts_Syn_Children :: (Set NontermIdent), refNts_Syn_Children :: (Set NontermIdent), vertices_Syn_Children :: (Set.Set Vertex) }
{-# INLINABLE wrap_Children #-}
wrap_Children :: T_Children  -> Inh_Children  -> (Syn_Children )
wrap_Children (T_Children act) (Inh_Children _lhsIaroundMap _lhsIinhMap _lhsImergeMap _lhsImergedChildren _lhsIoptions _lhsIsynMap) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Children_vIn4 _lhsIaroundMap _lhsIinhMap _lhsImergeMap _lhsImergedChildren _lhsIoptions _lhsIsynMap
        (T_Children_vOut4 _lhsOechilds _lhsOedges _lhsOnontnames _lhsOrefHoNts _lhsOrefNts _lhsOvertices) <- return (inv_Children_s5 sem arg)
        return (Syn_Children _lhsOechilds _lhsOedges _lhsOnontnames _lhsOrefHoNts _lhsOrefNts _lhsOvertices)
   )

-- cata
{-# NOINLINE sem_Children #-}
sem_Children :: Children  -> T_Children 
sem_Children list = Prelude.foldr sem_Children_Cons sem_Children_Nil (Prelude.map sem_Child list)

-- semantic domain
newtype T_Children  = T_Children {
                                 attach_T_Children :: Identity (T_Children_s5 )
                                 }
newtype T_Children_s5  = C_Children_s5 {
                                       inv_Children_s5 :: (T_Children_v4 )
                                       }
data T_Children_s6  = C_Children_s6
type T_Children_v4  = (T_Children_vIn4 ) -> (T_Children_vOut4 )
data T_Children_vIn4  = T_Children_vIn4 (Map Identifier [Expression]) (Map Identifier Attributes) (Map Identifier (Identifier, [Identifier], Expression)) (Set Identifier) (Options) (Map Identifier Attributes)
data T_Children_vOut4  = T_Children_vOut4 (EChildren) (Set.Set Edge) ([(Identifier, Identifier)]) (Set NontermIdent) (Set NontermIdent) (Set.Set Vertex)
{-# NOINLINE sem_Children_Cons #-}
sem_Children_Cons :: T_Child  -> T_Children  -> T_Children 
sem_Children_Cons arg_hd_ arg_tl_ = T_Children (return st5) where
   {-# NOINLINE st5 #-}
   st5 = let
      v4 :: T_Children_v4 
      v4 = \ (T_Children_vIn4 _lhsIaroundMap _lhsIinhMap _lhsImergeMap _lhsImergedChildren _lhsIoptions _lhsIsynMap) -> ( let
         _hdX2 = Control.Monad.Identity.runIdentity (attach_T_Child (arg_hd_))
         _tlX5 = Control.Monad.Identity.runIdentity (attach_T_Children (arg_tl_))
         (T_Child_vOut1 _hdIechilds _hdIedges _hdInontnames _hdIrefHoNts _hdIrefNts _hdIvertices) = inv_Child_s2 _hdX2 (T_Child_vIn1 _hdOaroundMap _hdOinhMap _hdOmergeMap _hdOmergedChildren _hdOoptions _hdOsynMap)
         (T_Children_vOut4 _tlIechilds _tlIedges _tlInontnames _tlIrefHoNts _tlIrefNts _tlIvertices) = inv_Children_s5 _tlX5 (T_Children_vIn4 _tlOaroundMap _tlOinhMap _tlOmergeMap _tlOmergedChildren _tlOoptions _tlOsynMap)
         _lhsOechilds :: EChildren
         _lhsOechilds = rule23 _hdIechilds _tlIechilds
         _lhsOedges :: Set.Set Edge
         _lhsOedges = rule24 _hdIedges _tlIedges
         _lhsOnontnames :: [(Identifier, Identifier)]
         _lhsOnontnames = rule25 _hdInontnames _tlInontnames
         _lhsOrefHoNts :: Set NontermIdent
         _lhsOrefHoNts = rule26 _hdIrefHoNts _tlIrefHoNts
         _lhsOrefNts :: Set NontermIdent
         _lhsOrefNts = rule27 _hdIrefNts _tlIrefNts
         _lhsOvertices :: Set.Set Vertex
         _lhsOvertices = rule28 _hdIvertices _tlIvertices
         _hdOaroundMap = rule29 _lhsIaroundMap
         _hdOinhMap = rule30 _lhsIinhMap
         _hdOmergeMap = rule31 _lhsImergeMap
         _hdOmergedChildren = rule32 _lhsImergedChildren
         _hdOoptions = rule33 _lhsIoptions
         _hdOsynMap = rule34 _lhsIsynMap
         _tlOaroundMap = rule35 _lhsIaroundMap
         _tlOinhMap = rule36 _lhsIinhMap
         _tlOmergeMap = rule37 _lhsImergeMap
         _tlOmergedChildren = rule38 _lhsImergedChildren
         _tlOoptions = rule39 _lhsIoptions
         _tlOsynMap = rule40 _lhsIsynMap
         __result_ = T_Children_vOut4 _lhsOechilds _lhsOedges _lhsOnontnames _lhsOrefHoNts _lhsOrefNts _lhsOvertices
         in __result_ )
     in C_Children_s5 v4
   {-# INLINE rule23 #-}
   rule23 = \ ((_hdIechilds) :: EChild) ((_tlIechilds) :: EChildren) ->
     _hdIechilds : _tlIechilds
   {-# INLINE rule24 #-}
   rule24 = \ ((_hdIedges) :: Set.Set Edge) ((_tlIedges) :: Set.Set Edge) ->
     _hdIedges `Set.union` _tlIedges
   {-# INLINE rule25 #-}
   rule25 = \ ((_hdInontnames) :: [(Identifier, Identifier)]) ((_tlInontnames) :: [(Identifier, Identifier)]) ->
     _hdInontnames ++ _tlInontnames
   {-# INLINE rule26 #-}
   rule26 = \ ((_hdIrefHoNts) :: Set NontermIdent) ((_tlIrefHoNts) :: Set NontermIdent) ->
     _hdIrefHoNts `mappend` _tlIrefHoNts
   {-# INLINE rule27 #-}
   rule27 = \ ((_hdIrefNts) :: Set NontermIdent) ((_tlIrefNts) :: Set NontermIdent) ->
     _hdIrefNts `mappend` _tlIrefNts
   {-# INLINE rule28 #-}
   rule28 = \ ((_hdIvertices) :: Set.Set Vertex) ((_tlIvertices) :: Set.Set Vertex) ->
     _hdIvertices `Set.union` _tlIvertices
   {-# INLINE rule29 #-}
   rule29 = \ ((_lhsIaroundMap) :: Map Identifier [Expression]) ->
     _lhsIaroundMap
   {-# INLINE rule30 #-}
   rule30 = \ ((_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# INLINE rule31 #-}
   rule31 = \ ((_lhsImergeMap) :: Map Identifier (Identifier, [Identifier], Expression)) ->
     _lhsImergeMap
   {-# INLINE rule32 #-}
   rule32 = \ ((_lhsImergedChildren) :: Set Identifier) ->
     _lhsImergedChildren
   {-# INLINE rule33 #-}
   rule33 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule34 #-}
   rule34 = \ ((_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
   {-# INLINE rule35 #-}
   rule35 = \ ((_lhsIaroundMap) :: Map Identifier [Expression]) ->
     _lhsIaroundMap
   {-# INLINE rule36 #-}
   rule36 = \ ((_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# INLINE rule37 #-}
   rule37 = \ ((_lhsImergeMap) :: Map Identifier (Identifier, [Identifier], Expression)) ->
     _lhsImergeMap
   {-# INLINE rule38 #-}
   rule38 = \ ((_lhsImergedChildren) :: Set Identifier) ->
     _lhsImergedChildren
   {-# INLINE rule39 #-}
   rule39 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule40 #-}
   rule40 = \ ((_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
{-# NOINLINE sem_Children_Nil #-}
sem_Children_Nil ::  T_Children 
sem_Children_Nil  = T_Children (return st5) where
   {-# NOINLINE st5 #-}
   st5 = let
      v4 :: T_Children_v4 
      v4 = \ (T_Children_vIn4 _lhsIaroundMap _lhsIinhMap _lhsImergeMap _lhsImergedChildren _lhsIoptions _lhsIsynMap) -> ( let
         _lhsOechilds :: EChildren
         _lhsOechilds = rule41  ()
         _lhsOedges :: Set.Set Edge
         _lhsOedges = rule42  ()
         _lhsOnontnames :: [(Identifier, Identifier)]
         _lhsOnontnames = rule43  ()
         _lhsOrefHoNts :: Set NontermIdent
         _lhsOrefHoNts = rule44  ()
         _lhsOrefNts :: Set NontermIdent
         _lhsOrefNts = rule45  ()
         _lhsOvertices :: Set.Set Vertex
         _lhsOvertices = rule46  ()
         __result_ = T_Children_vOut4 _lhsOechilds _lhsOedges _lhsOnontnames _lhsOrefHoNts _lhsOrefNts _lhsOvertices
         in __result_ )
     in C_Children_s5 v4
   {-# INLINE rule41 #-}
   rule41 = \  (_ :: ()) ->
     []
   {-# INLINE rule42 #-}
   rule42 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule43 #-}
   rule43 = \  (_ :: ()) ->
     []
   {-# INLINE rule44 #-}
   rule44 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule45 #-}
   rule45 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule46 #-}
   rule46 = \  (_ :: ()) ->
     Set.empty

-- Expression --------------------------------------------------
-- wrapper
data Inh_Expression  = Inh_Expression {  }
data Syn_Expression  = Syn_Expression { copy_Syn_Expression :: (Expression), vertices_Syn_Expression :: (Set.Set Vertex) }
{-# INLINABLE wrap_Expression #-}
wrap_Expression :: T_Expression  -> Inh_Expression  -> (Syn_Expression )
wrap_Expression (T_Expression act) (Inh_Expression ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Expression_vIn7 
        (T_Expression_vOut7 _lhsOcopy _lhsOvertices) <- return (inv_Expression_s8 sem arg)
        return (Syn_Expression _lhsOcopy _lhsOvertices)
   )

-- cata
{-# INLINE sem_Expression #-}
sem_Expression :: Expression  -> T_Expression 
sem_Expression ( Expression pos_ tks_ ) = sem_Expression_Expression pos_ tks_

-- semantic domain
newtype T_Expression  = T_Expression {
                                     attach_T_Expression :: Identity (T_Expression_s8 )
                                     }
newtype T_Expression_s8  = C_Expression_s8 {
                                           inv_Expression_s8 :: (T_Expression_v7 )
                                           }
data T_Expression_s9  = C_Expression_s9
type T_Expression_v7  = (T_Expression_vIn7 ) -> (T_Expression_vOut7 )
data T_Expression_vIn7  = T_Expression_vIn7 
data T_Expression_vOut7  = T_Expression_vOut7 (Expression) (Set.Set Vertex)
{-# NOINLINE sem_Expression_Expression #-}
sem_Expression_Expression :: (Pos) -> ([HsToken]) -> T_Expression 
sem_Expression_Expression arg_pos_ arg_tks_ = T_Expression (return st8) where
   {-# NOINLINE st8 #-}
   st8 = let
      v7 :: T_Expression_v7 
      v7 = \ (T_Expression_vIn7 ) -> ( let
         _lhsOvertices :: Set.Set Vertex
         _lhsOvertices = rule47 arg_tks_
         _copy = rule48 arg_pos_ arg_tks_
         _lhsOcopy :: Expression
         _lhsOcopy = rule49 _copy
         __result_ = T_Expression_vOut7 _lhsOcopy _lhsOvertices
         in __result_ )
     in C_Expression_s8 v7
   {-# INLINE rule47 #-}
   {-# LINE 200 "./src-ag/KWOrder.ag" #-}
   rule47 = \ tks_ ->
                                 {-# LINE 200 "./src-ag/KWOrder.ag" #-}
                                 Set.unions $ map (\tok -> vertices_Syn_HsToken
                                              (wrap_HsToken (sem_HsToken tok) Inh_HsToken)) tks_
                                 {-# LINE 517 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule48 #-}
   rule48 = \ pos_ tks_ ->
     Expression pos_ tks_
   {-# INLINE rule49 #-}
   rule49 = \ _copy ->
     _copy

-- Grammar -----------------------------------------------------
-- wrapper
data Inh_Grammar  = Inh_Grammar { options_Inh_Grammar :: (Options) }
data Syn_Grammar  = Syn_Grammar { depgraphs_Syn_Grammar :: (PP_Doc), errors_Syn_Grammar :: (Seq Error), inhmap_Syn_Grammar :: (Map.Map NontermIdent Attributes), localSigMap_Syn_Grammar :: (Map.Map NontermIdent (Map.Map ConstructorIdent (Map.Map Identifier Type))), output_Syn_Grammar :: (ExecutionPlan), synmap_Syn_Grammar :: (Map.Map NontermIdent Attributes), visitgraph_Syn_Grammar :: (PP_Doc) }
{-# INLINABLE wrap_Grammar #-}
wrap_Grammar :: T_Grammar  -> Inh_Grammar  -> (Syn_Grammar )
wrap_Grammar (T_Grammar act) (Inh_Grammar _lhsIoptions) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Grammar_vIn10 _lhsIoptions
        (T_Grammar_vOut10 _lhsOdepgraphs _lhsOerrors _lhsOinhmap _lhsOlocalSigMap _lhsOoutput _lhsOsynmap _lhsOvisitgraph) <- return (inv_Grammar_s11 sem arg)
        return (Syn_Grammar _lhsOdepgraphs _lhsOerrors _lhsOinhmap _lhsOlocalSigMap _lhsOoutput _lhsOsynmap _lhsOvisitgraph)
   )

-- cata
{-# INLINE sem_Grammar #-}
sem_Grammar :: Grammar  -> T_Grammar 
sem_Grammar ( Grammar typeSyns_ useMap_ derivings_ wrappers_ nonts_ pragmas_ manualAttrOrderMap_ paramMap_ contextMap_ quantMap_ uniqueMap_ augmentsMap_ aroundsMap_ mergeMap_ ) = sem_Grammar_Grammar typeSyns_ useMap_ derivings_ wrappers_ ( sem_Nonterminals nonts_ ) pragmas_ manualAttrOrderMap_ paramMap_ contextMap_ quantMap_ uniqueMap_ augmentsMap_ aroundsMap_ mergeMap_

-- semantic domain
newtype T_Grammar  = T_Grammar {
                               attach_T_Grammar :: Identity (T_Grammar_s11 )
                               }
newtype T_Grammar_s11  = C_Grammar_s11 {
                                       inv_Grammar_s11 :: (T_Grammar_v10 )
                                       }
data T_Grammar_s12  = C_Grammar_s12
type T_Grammar_v10  = (T_Grammar_vIn10 ) -> (T_Grammar_vOut10 )
data T_Grammar_vIn10  = T_Grammar_vIn10 (Options)
data T_Grammar_vOut10  = T_Grammar_vOut10 (PP_Doc) (Seq Error) (Map.Map NontermIdent Attributes) (Map.Map NontermIdent (Map.Map ConstructorIdent (Map.Map Identifier Type))) (ExecutionPlan) (Map.Map NontermIdent Attributes) (PP_Doc)
{-# NOINLINE sem_Grammar_Grammar #-}
sem_Grammar_Grammar :: (TypeSyns) -> (UseMap) -> (Derivings) -> (Set NontermIdent) -> T_Nonterminals  -> (PragmaMap) -> (AttrOrderMap) -> (ParamMap) -> (ContextMap) -> (QuantMap) -> (UniqueMap) -> (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) -> (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) -> (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier, [Identifier], Expression)))) -> T_Grammar 
sem_Grammar_Grammar arg_typeSyns_ _ arg_derivings_ arg_wrappers_ arg_nonts_ _ arg_manualAttrOrderMap_ _ arg_contextMap_ _ _ _ arg_aroundsMap_ arg_mergeMap_ = T_Grammar (return st11) where
   {-# NOINLINE st11 #-}
   st11 = let
      v10 :: T_Grammar_v10 
      v10 = \ (T_Grammar_vIn10 _lhsIoptions) -> ( let
         _nontsX26 = Control.Monad.Identity.runIdentity (attach_T_Nonterminals (arg_nonts_))
         (T_Nonterminals_vOut25 _nontsIdepinfo _nontsIinhMap' _nontsIinhmap _nontsIlocalSigMap _nontsIntDeps _nontsIntHoDeps _nontsIrulenumber _nontsIsynMap' _nontsIsynmap) = inv_Nonterminals_s26 _nontsX26 (T_Nonterminals_vIn25 _nontsOaroundMap _nontsOclassContexts _nontsOclosedHoNtDeps _nontsOclosedHoNtRevDeps _nontsOclosedNtDeps _nontsOinhMap _nontsOmanualDeps _nontsOmergeMap _nontsOoptions _nontsOrulenumber _nontsOsynMap)
         _nontsOinhMap = rule50 _nontsIinhMap'
         _nontsOsynMap = rule51 _nontsIsynMap'
         _nontsOrulenumber = rule52  ()
         _closedNtDeps = rule53 _nontsIntDeps
         _closedHoNtDeps = rule54 _nontsIntHoDeps
         _closedHoNtRevDeps = rule55 _closedHoNtDeps
         _nontsOaroundMap = rule56 arg_aroundsMap_
         _nontsOmergeMap = rule57 arg_mergeMap_
         _nontsOclassContexts = rule58 arg_contextMap_
         _nontsOmanualDeps = rule59 arg_manualAttrOrderMap_
         _lhsOoutput :: ExecutionPlan
         _lhsOdepgraphs :: PP_Doc
         _lhsOvisitgraph :: PP_Doc
         _lhsOerrors :: Seq Error
         (_lhsOoutput,_lhsOdepgraphs,_lhsOvisitgraph,_lhsOerrors) = rule60 _lhsIoptions _nontsIdepinfo arg_derivings_ arg_typeSyns_ arg_wrappers_
         _lhsOinhmap :: Map.Map NontermIdent Attributes
         _lhsOinhmap = rule61 _nontsIinhmap
         _lhsOlocalSigMap :: Map.Map NontermIdent (Map.Map ConstructorIdent (Map.Map Identifier Type))
         _lhsOlocalSigMap = rule62 _nontsIlocalSigMap
         _lhsOsynmap :: Map.Map NontermIdent Attributes
         _lhsOsynmap = rule63 _nontsIsynmap
         _nontsOclosedHoNtDeps = rule64 _closedHoNtDeps
         _nontsOclosedHoNtRevDeps = rule65 _closedHoNtRevDeps
         _nontsOclosedNtDeps = rule66 _closedNtDeps
         _nontsOoptions = rule67 _lhsIoptions
         __result_ = T_Grammar_vOut10 _lhsOdepgraphs _lhsOerrors _lhsOinhmap _lhsOlocalSigMap _lhsOoutput _lhsOsynmap _lhsOvisitgraph
         in __result_ )
     in C_Grammar_s11 v10
   {-# INLINE rule50 #-}
   {-# LINE 15 "./src-ag/DistChildAttr.ag" #-}
   rule50 = \ ((_nontsIinhMap') :: Map Identifier Attributes) ->
                             {-# LINE 15 "./src-ag/DistChildAttr.ag" #-}
                             _nontsIinhMap'
                             {-# LINE 597 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule51 #-}
   {-# LINE 16 "./src-ag/DistChildAttr.ag" #-}
   rule51 = \ ((_nontsIsynMap') :: Map Identifier Attributes) ->
                             {-# LINE 16 "./src-ag/DistChildAttr.ag" #-}
                             _nontsIsynMap'
                             {-# LINE 603 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule52 #-}
   {-# LINE 44 "./src-ag/KWOrder.ag" #-}
   rule52 = \  (_ :: ()) ->
                                  {-# LINE 44 "./src-ag/KWOrder.ag" #-}
                                  0
                                  {-# LINE 609 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule53 #-}
   {-# LINE 83 "./src-ag/KWOrder.ag" #-}
   rule53 = \ ((_nontsIntDeps) :: Map NontermIdent (Set NontermIdent)) ->
                            {-# LINE 83 "./src-ag/KWOrder.ag" #-}
                            closeMap _nontsIntDeps
                            {-# LINE 615 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule54 #-}
   {-# LINE 84 "./src-ag/KWOrder.ag" #-}
   rule54 = \ ((_nontsIntHoDeps) :: Map NontermIdent (Set NontermIdent)) ->
                            {-# LINE 84 "./src-ag/KWOrder.ag" #-}
                            closeMap _nontsIntHoDeps
                            {-# LINE 621 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule55 #-}
   {-# LINE 85 "./src-ag/KWOrder.ag" #-}
   rule55 = \ _closedHoNtDeps ->
                            {-# LINE 85 "./src-ag/KWOrder.ag" #-}
                            revDeps _closedHoNtDeps
                            {-# LINE 627 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule56 #-}
   {-# LINE 105 "./src-ag/KWOrder.ag" #-}
   rule56 = \ aroundsMap_ ->
                      {-# LINE 105 "./src-ag/KWOrder.ag" #-}
                      aroundsMap_
                      {-# LINE 633 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule57 #-}
   {-# LINE 130 "./src-ag/KWOrder.ag" #-}
   rule57 = \ mergeMap_ ->
                     {-# LINE 130 "./src-ag/KWOrder.ag" #-}
                     mergeMap_
                     {-# LINE 639 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule58 #-}
   {-# LINE 146 "./src-ag/KWOrder.ag" #-}
   rule58 = \ contextMap_ ->
                          {-# LINE 146 "./src-ag/KWOrder.ag" #-}
                          contextMap_
                          {-# LINE 645 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule59 #-}
   {-# LINE 269 "./src-ag/KWOrder.ag" #-}
   rule59 = \ manualAttrOrderMap_ ->
                                                   {-# LINE 269 "./src-ag/KWOrder.ag" #-}
                                                   manualAttrOrderMap_
                                                   {-# LINE 651 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule60 #-}
   {-# LINE 360 "./src-ag/KWOrder.ag" #-}
   rule60 = \ ((_lhsIoptions) :: Options) ((_nontsIdepinfo) :: [NontDependencyInformation]) derivings_ typeSyns_ wrappers_ ->
                    {-# LINE 360 "./src-ag/KWOrder.ag" #-}
                    let lazyPlan = kennedyWarrenLazy _lhsIoptions wrappers_ _nontsIdepinfo typeSyns_ derivings_
                    in if visit _lhsIoptions && withCycle _lhsIoptions
                       then case kennedyWarrenOrder _lhsIoptions wrappers_ _nontsIdepinfo typeSyns_ derivings_ of
                              Left e        -> (lazyPlan,empty,empty,Seq.singleton e)
                              Right (o,d,v) -> (o,d,v,Seq.empty)
                       else (lazyPlan,empty,empty,Seq.empty)
                    {-# LINE 662 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule61 #-}
   rule61 = \ ((_nontsIinhmap) :: Map.Map NontermIdent Attributes) ->
     _nontsIinhmap
   {-# INLINE rule62 #-}
   rule62 = \ ((_nontsIlocalSigMap) :: Map.Map NontermIdent (Map.Map ConstructorIdent (Map.Map Identifier Type))) ->
     _nontsIlocalSigMap
   {-# INLINE rule63 #-}
   rule63 = \ ((_nontsIsynmap) :: Map.Map NontermIdent Attributes) ->
     _nontsIsynmap
   {-# INLINE rule64 #-}
   rule64 = \ _closedHoNtDeps ->
     _closedHoNtDeps
   {-# INLINE rule65 #-}
   rule65 = \ _closedHoNtRevDeps ->
     _closedHoNtRevDeps
   {-# INLINE rule66 #-}
   rule66 = \ _closedNtDeps ->
     _closedNtDeps
   {-# INLINE rule67 #-}
   rule67 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions

-- HsToken -----------------------------------------------------
-- wrapper
data Inh_HsToken  = Inh_HsToken {  }
data Syn_HsToken  = Syn_HsToken { vertices_Syn_HsToken :: (Set.Set Vertex) }
{-# INLINABLE wrap_HsToken #-}
wrap_HsToken :: T_HsToken  -> Inh_HsToken  -> (Syn_HsToken )
wrap_HsToken (T_HsToken act) (Inh_HsToken ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_HsToken_vIn13 
        (T_HsToken_vOut13 _lhsOvertices) <- return (inv_HsToken_s14 sem arg)
        return (Syn_HsToken _lhsOvertices)
   )

-- cata
{-# NOINLINE sem_HsToken #-}
sem_HsToken :: HsToken  -> T_HsToken 
sem_HsToken ( AGLocal var_ pos_ rdesc_ ) = sem_HsToken_AGLocal var_ pos_ rdesc_
sem_HsToken ( AGField field_ attr_ pos_ rdesc_ ) = sem_HsToken_AGField field_ attr_ pos_ rdesc_
sem_HsToken ( HsToken value_ pos_ ) = sem_HsToken_HsToken value_ pos_
sem_HsToken ( CharToken value_ pos_ ) = sem_HsToken_CharToken value_ pos_
sem_HsToken ( StrToken value_ pos_ ) = sem_HsToken_StrToken value_ pos_
sem_HsToken ( Err mesg_ pos_ ) = sem_HsToken_Err mesg_ pos_

-- semantic domain
newtype T_HsToken  = T_HsToken {
                               attach_T_HsToken :: Identity (T_HsToken_s14 )
                               }
newtype T_HsToken_s14  = C_HsToken_s14 {
                                       inv_HsToken_s14 :: (T_HsToken_v13 )
                                       }
data T_HsToken_s15  = C_HsToken_s15
type T_HsToken_v13  = (T_HsToken_vIn13 ) -> (T_HsToken_vOut13 )
data T_HsToken_vIn13  = T_HsToken_vIn13 
data T_HsToken_vOut13  = T_HsToken_vOut13 (Set.Set Vertex)
{-# NOINLINE sem_HsToken_AGLocal #-}
sem_HsToken_AGLocal :: (Identifier) -> (Pos) -> (Maybe String) -> T_HsToken 
sem_HsToken_AGLocal arg_var_ _ _ = T_HsToken (return st14) where
   {-# NOINLINE st14 #-}
   st14 = let
      v13 :: T_HsToken_v13 
      v13 = \ (T_HsToken_vIn13 ) -> ( let
         _lhsOvertices :: Set.Set Vertex
         _lhsOvertices = rule68 arg_var_
         __result_ = T_HsToken_vOut13 _lhsOvertices
         in __result_ )
     in C_HsToken_s14 v13
   {-# INLINE rule68 #-}
   {-# LINE 193 "./src-ag/KWOrder.ag" #-}
   rule68 = \ var_ ->
                              {-# LINE 193 "./src-ag/KWOrder.ag" #-}
                              Set.singleton $ VChild var_
                              {-# LINE 737 "dist/build/KWOrder.hs"#-}
{-# NOINLINE sem_HsToken_AGField #-}
sem_HsToken_AGField :: (Identifier) -> (Identifier) -> (Pos) -> (Maybe String) -> T_HsToken 
sem_HsToken_AGField arg_field_ arg_attr_ _ _ = T_HsToken (return st14) where
   {-# NOINLINE st14 #-}
   st14 = let
      v13 :: T_HsToken_v13 
      v13 = \ (T_HsToken_vIn13 ) -> ( let
         _lhsOvertices :: Set.Set Vertex
         _lhsOvertices = rule69 arg_attr_ arg_field_
         __result_ = T_HsToken_vOut13 _lhsOvertices
         in __result_ )
     in C_HsToken_s14 v13
   {-# INLINE rule69 #-}
   {-# LINE 194 "./src-ag/KWOrder.ag" #-}
   rule69 = \ attr_ field_ ->
                              {-# LINE 194 "./src-ag/KWOrder.ag" #-}
                              Set.singleton $ VAttr (if      field_ == _LHS then Inh
                                                     else if field_ == _LOC then Loc
                                                     else                        Syn) field_ attr_
                              {-# LINE 757 "dist/build/KWOrder.hs"#-}
{-# NOINLINE sem_HsToken_HsToken #-}
sem_HsToken_HsToken :: (String) -> (Pos) -> T_HsToken 
sem_HsToken_HsToken _ _ = T_HsToken (return st14) where
   {-# NOINLINE st14 #-}
   st14 = let
      v13 :: T_HsToken_v13 
      v13 = \ (T_HsToken_vIn13 ) -> ( let
         _lhsOvertices :: Set.Set Vertex
         _lhsOvertices = rule70  ()
         __result_ = T_HsToken_vOut13 _lhsOvertices
         in __result_ )
     in C_HsToken_s14 v13
   {-# INLINE rule70 #-}
   rule70 = \  (_ :: ()) ->
     Set.empty
{-# NOINLINE sem_HsToken_CharToken #-}
sem_HsToken_CharToken :: (String) -> (Pos) -> T_HsToken 
sem_HsToken_CharToken _ _ = T_HsToken (return st14) where
   {-# NOINLINE st14 #-}
   st14 = let
      v13 :: T_HsToken_v13 
      v13 = \ (T_HsToken_vIn13 ) -> ( let
         _lhsOvertices :: Set.Set Vertex
         _lhsOvertices = rule71  ()
         __result_ = T_HsToken_vOut13 _lhsOvertices
         in __result_ )
     in C_HsToken_s14 v13
   {-# INLINE rule71 #-}
   rule71 = \  (_ :: ()) ->
     Set.empty
{-# NOINLINE sem_HsToken_StrToken #-}
sem_HsToken_StrToken :: (String) -> (Pos) -> T_HsToken 
sem_HsToken_StrToken _ _ = T_HsToken (return st14) where
   {-# NOINLINE st14 #-}
   st14 = let
      v13 :: T_HsToken_v13 
      v13 = \ (T_HsToken_vIn13 ) -> ( let
         _lhsOvertices :: Set.Set Vertex
         _lhsOvertices = rule72  ()
         __result_ = T_HsToken_vOut13 _lhsOvertices
         in __result_ )
     in C_HsToken_s14 v13
   {-# INLINE rule72 #-}
   rule72 = \  (_ :: ()) ->
     Set.empty
{-# NOINLINE sem_HsToken_Err #-}
sem_HsToken_Err :: (String) -> (Pos) -> T_HsToken 
sem_HsToken_Err _ _ = T_HsToken (return st14) where
   {-# NOINLINE st14 #-}
   st14 = let
      v13 :: T_HsToken_v13 
      v13 = \ (T_HsToken_vIn13 ) -> ( let
         _lhsOvertices :: Set.Set Vertex
         _lhsOvertices = rule73  ()
         __result_ = T_HsToken_vOut13 _lhsOvertices
         in __result_ )
     in C_HsToken_s14 v13
   {-# INLINE rule73 #-}
   rule73 = \  (_ :: ()) ->
     Set.empty

-- HsTokens ----------------------------------------------------
-- wrapper
data Inh_HsTokens  = Inh_HsTokens {  }
data Syn_HsTokens  = Syn_HsTokens {  }
{-# INLINABLE wrap_HsTokens #-}
wrap_HsTokens :: T_HsTokens  -> Inh_HsTokens  -> (Syn_HsTokens )
wrap_HsTokens (T_HsTokens act) (Inh_HsTokens ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_HsTokens_vIn16 
        (T_HsTokens_vOut16 ) <- return (inv_HsTokens_s17 sem arg)
        return (Syn_HsTokens )
   )

-- cata
{-# NOINLINE sem_HsTokens #-}
sem_HsTokens :: HsTokens  -> T_HsTokens 
sem_HsTokens list = Prelude.foldr sem_HsTokens_Cons sem_HsTokens_Nil (Prelude.map sem_HsToken list)

-- semantic domain
newtype T_HsTokens  = T_HsTokens {
                                 attach_T_HsTokens :: Identity (T_HsTokens_s17 )
                                 }
newtype T_HsTokens_s17  = C_HsTokens_s17 {
                                         inv_HsTokens_s17 :: (T_HsTokens_v16 )
                                         }
data T_HsTokens_s18  = C_HsTokens_s18
type T_HsTokens_v16  = (T_HsTokens_vIn16 ) -> (T_HsTokens_vOut16 )
data T_HsTokens_vIn16  = T_HsTokens_vIn16 
data T_HsTokens_vOut16  = T_HsTokens_vOut16 
{-# NOINLINE sem_HsTokens_Cons #-}
sem_HsTokens_Cons :: T_HsToken  -> T_HsTokens  -> T_HsTokens 
sem_HsTokens_Cons arg_hd_ arg_tl_ = T_HsTokens (return st17) where
   {-# NOINLINE st17 #-}
   st17 = let
      v16 :: T_HsTokens_v16 
      v16 = \ (T_HsTokens_vIn16 ) -> ( let
         _hdX14 = Control.Monad.Identity.runIdentity (attach_T_HsToken (arg_hd_))
         _tlX17 = Control.Monad.Identity.runIdentity (attach_T_HsTokens (arg_tl_))
         (T_HsToken_vOut13 _hdIvertices) = inv_HsToken_s14 _hdX14 (T_HsToken_vIn13 )
         (T_HsTokens_vOut16 ) = inv_HsTokens_s17 _tlX17 (T_HsTokens_vIn16 )
         __result_ = T_HsTokens_vOut16 
         in __result_ )
     in C_HsTokens_s17 v16
{-# NOINLINE sem_HsTokens_Nil #-}
sem_HsTokens_Nil ::  T_HsTokens 
sem_HsTokens_Nil  = T_HsTokens (return st17) where
   {-# NOINLINE st17 #-}
   st17 = let
      v16 :: T_HsTokens_v16 
      v16 = \ (T_HsTokens_vIn16 ) -> ( let
         __result_ = T_HsTokens_vOut16 
         in __result_ )
     in C_HsTokens_s17 v16

-- HsTokensRoot ------------------------------------------------
-- wrapper
data Inh_HsTokensRoot  = Inh_HsTokensRoot {  }
data Syn_HsTokensRoot  = Syn_HsTokensRoot {  }
{-# INLINABLE wrap_HsTokensRoot #-}
wrap_HsTokensRoot :: T_HsTokensRoot  -> Inh_HsTokensRoot  -> (Syn_HsTokensRoot )
wrap_HsTokensRoot (T_HsTokensRoot act) (Inh_HsTokensRoot ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_HsTokensRoot_vIn19 
        (T_HsTokensRoot_vOut19 ) <- return (inv_HsTokensRoot_s20 sem arg)
        return (Syn_HsTokensRoot )
   )

-- cata
{-# INLINE sem_HsTokensRoot #-}
sem_HsTokensRoot :: HsTokensRoot  -> T_HsTokensRoot 
sem_HsTokensRoot ( HsTokensRoot tokens_ ) = sem_HsTokensRoot_HsTokensRoot ( sem_HsTokens tokens_ )

-- semantic domain
newtype T_HsTokensRoot  = T_HsTokensRoot {
                                         attach_T_HsTokensRoot :: Identity (T_HsTokensRoot_s20 )
                                         }
newtype T_HsTokensRoot_s20  = C_HsTokensRoot_s20 {
                                                 inv_HsTokensRoot_s20 :: (T_HsTokensRoot_v19 )
                                                 }
data T_HsTokensRoot_s21  = C_HsTokensRoot_s21
type T_HsTokensRoot_v19  = (T_HsTokensRoot_vIn19 ) -> (T_HsTokensRoot_vOut19 )
data T_HsTokensRoot_vIn19  = T_HsTokensRoot_vIn19 
data T_HsTokensRoot_vOut19  = T_HsTokensRoot_vOut19 
{-# NOINLINE sem_HsTokensRoot_HsTokensRoot #-}
sem_HsTokensRoot_HsTokensRoot :: T_HsTokens  -> T_HsTokensRoot 
sem_HsTokensRoot_HsTokensRoot arg_tokens_ = T_HsTokensRoot (return st20) where
   {-# NOINLINE st20 #-}
   st20 = let
      v19 :: T_HsTokensRoot_v19 
      v19 = \ (T_HsTokensRoot_vIn19 ) -> ( let
         _tokensX17 = Control.Monad.Identity.runIdentity (attach_T_HsTokens (arg_tokens_))
         (T_HsTokens_vOut16 ) = inv_HsTokens_s17 _tokensX17 (T_HsTokens_vIn16 )
         __result_ = T_HsTokensRoot_vOut19 
         in __result_ )
     in C_HsTokensRoot_s20 v19

-- Nonterminal -------------------------------------------------
-- wrapper
data Inh_Nonterminal  = Inh_Nonterminal { aroundMap_Inh_Nonterminal :: (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))), classContexts_Inh_Nonterminal :: (ContextMap), closedHoNtDeps_Inh_Nonterminal :: (Map NontermIdent (Set NontermIdent)), closedHoNtRevDeps_Inh_Nonterminal :: (Map NontermIdent (Set NontermIdent)), closedNtDeps_Inh_Nonterminal :: (Map NontermIdent (Set NontermIdent)), inhMap_Inh_Nonterminal :: (Map Identifier Attributes), manualDeps_Inh_Nonterminal :: (AttrOrderMap), mergeMap_Inh_Nonterminal :: (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier, [Identifier], Expression)))), options_Inh_Nonterminal :: (Options), rulenumber_Inh_Nonterminal :: (Int), synMap_Inh_Nonterminal :: (Map Identifier Attributes) }
data Syn_Nonterminal  = Syn_Nonterminal { depinfo_Syn_Nonterminal :: (NontDependencyInformation), inhMap'_Syn_Nonterminal :: (Map Identifier Attributes), inhmap_Syn_Nonterminal :: (Map.Map NontermIdent Attributes), localSigMap_Syn_Nonterminal :: (Map.Map NontermIdent (Map.Map ConstructorIdent (Map.Map Identifier Type))), ntDeps_Syn_Nonterminal :: (Map NontermIdent (Set NontermIdent)), ntHoDeps_Syn_Nonterminal :: (Map NontermIdent (Set NontermIdent)), rulenumber_Syn_Nonterminal :: (Int), synMap'_Syn_Nonterminal :: (Map Identifier Attributes), synmap_Syn_Nonterminal :: (Map.Map NontermIdent Attributes) }
{-# INLINABLE wrap_Nonterminal #-}
wrap_Nonterminal :: T_Nonterminal  -> Inh_Nonterminal  -> (Syn_Nonterminal )
wrap_Nonterminal (T_Nonterminal act) (Inh_Nonterminal _lhsIaroundMap _lhsIclassContexts _lhsIclosedHoNtDeps _lhsIclosedHoNtRevDeps _lhsIclosedNtDeps _lhsIinhMap _lhsImanualDeps _lhsImergeMap _lhsIoptions _lhsIrulenumber _lhsIsynMap) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Nonterminal_vIn22 _lhsIaroundMap _lhsIclassContexts _lhsIclosedHoNtDeps _lhsIclosedHoNtRevDeps _lhsIclosedNtDeps _lhsIinhMap _lhsImanualDeps _lhsImergeMap _lhsIoptions _lhsIrulenumber _lhsIsynMap
        (T_Nonterminal_vOut22 _lhsOdepinfo _lhsOinhMap' _lhsOinhmap _lhsOlocalSigMap _lhsOntDeps _lhsOntHoDeps _lhsOrulenumber _lhsOsynMap' _lhsOsynmap) <- return (inv_Nonterminal_s23 sem arg)
        return (Syn_Nonterminal _lhsOdepinfo _lhsOinhMap' _lhsOinhmap _lhsOlocalSigMap _lhsOntDeps _lhsOntHoDeps _lhsOrulenumber _lhsOsynMap' _lhsOsynmap)
   )

-- cata
{-# INLINE sem_Nonterminal #-}
sem_Nonterminal :: Nonterminal  -> T_Nonterminal 
sem_Nonterminal ( Nonterminal nt_ params_ inh_ syn_ prods_ ) = sem_Nonterminal_Nonterminal nt_ params_ inh_ syn_ ( sem_Productions prods_ )

-- semantic domain
newtype T_Nonterminal  = T_Nonterminal {
                                       attach_T_Nonterminal :: Identity (T_Nonterminal_s23 )
                                       }
newtype T_Nonterminal_s23  = C_Nonterminal_s23 {
                                               inv_Nonterminal_s23 :: (T_Nonterminal_v22 )
                                               }
data T_Nonterminal_s24  = C_Nonterminal_s24
type T_Nonterminal_v22  = (T_Nonterminal_vIn22 ) -> (T_Nonterminal_vOut22 )
data T_Nonterminal_vIn22  = T_Nonterminal_vIn22 (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) (ContextMap) (Map NontermIdent (Set NontermIdent)) (Map NontermIdent (Set NontermIdent)) (Map NontermIdent (Set NontermIdent)) (Map Identifier Attributes) (AttrOrderMap) (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier, [Identifier], Expression)))) (Options) (Int) (Map Identifier Attributes)
data T_Nonterminal_vOut22  = T_Nonterminal_vOut22 (NontDependencyInformation) (Map Identifier Attributes) (Map.Map NontermIdent Attributes) (Map.Map NontermIdent (Map.Map ConstructorIdent (Map.Map Identifier Type))) (Map NontermIdent (Set NontermIdent)) (Map NontermIdent (Set NontermIdent)) (Int) (Map Identifier Attributes) (Map.Map NontermIdent Attributes)
{-# NOINLINE sem_Nonterminal_Nonterminal #-}
sem_Nonterminal_Nonterminal :: (NontermIdent) -> ([Identifier]) -> (Attributes) -> (Attributes) -> T_Productions  -> T_Nonterminal 
sem_Nonterminal_Nonterminal arg_nt_ arg_params_ arg_inh_ arg_syn_ arg_prods_ = T_Nonterminal (return st23) where
   {-# NOINLINE st23 #-}
   st23 = let
      v22 :: T_Nonterminal_v22 
      v22 = \ (T_Nonterminal_vIn22 _lhsIaroundMap _lhsIclassContexts _lhsIclosedHoNtDeps _lhsIclosedHoNtRevDeps _lhsIclosedNtDeps _lhsIinhMap _lhsImanualDeps _lhsImergeMap _lhsIoptions _lhsIrulenumber _lhsIsynMap) -> ( let
         _prodsX38 = Control.Monad.Identity.runIdentity (attach_T_Productions (arg_prods_))
         (T_Productions_vOut37 _prodsIdepgraph _prodsIlocalSigMap _prodsIrefHoNts _prodsIrefNts _prodsIrulenumber) = inv_Productions_s38 _prodsX38 (T_Productions_vIn37 _prodsOaroundMap _prodsOinhMap _prodsOmanualDeps _prodsOmergeMap _prodsOoptions _prodsOrulenumber _prodsOsynMap)
         _lhsOinhMap' :: Map Identifier Attributes
         _lhsOinhMap' = rule74 arg_inh_ arg_nt_
         _lhsOsynMap' :: Map Identifier Attributes
         _lhsOsynMap' = rule75 arg_nt_ arg_syn_
         _lhsOntDeps :: Map NontermIdent (Set NontermIdent)
         _lhsOntDeps = rule76 _prodsIrefNts arg_nt_
         _lhsOntHoDeps :: Map NontermIdent (Set NontermIdent)
         _lhsOntHoDeps = rule77 _prodsIrefHoNts arg_nt_
         _closedNtDeps = rule78 _lhsIclosedNtDeps arg_nt_
         _closedHoNtDeps = rule79 _lhsIclosedHoNtDeps arg_nt_
         _closedHoNtRevDeps = rule80 _lhsIclosedHoNtRevDeps arg_nt_
         _recursive = rule81 _closedNtDeps arg_nt_
         _nontrivAcyc = rule82 _closedHoNtDeps arg_nt_
         _hoInfo = rule83 _closedHoNtDeps _closedHoNtRevDeps _nontrivAcyc
         _aroundMap = rule84 _lhsIaroundMap arg_nt_
         _mergeMap = rule85 _lhsImergeMap arg_nt_
         _classContexts = rule86 _lhsIclassContexts arg_nt_
         _prodsOmanualDeps = rule87 _lhsImanualDeps arg_nt_
         _synvertices = rule88 arg_nt_ arg_syn_
         _inhvertices = rule89 arg_inh_ arg_nt_
         _vertices = rule90 _inhvertices _synvertices
         _nontgraph = rule91 _vertices
         _lhsOdepinfo :: NontDependencyInformation
         _lhsOdepinfo = rule92 _classContexts _hoInfo _nontgraph _prodsIdepgraph _recursive arg_inh_ arg_nt_ arg_params_ arg_syn_
         _lhsOinhmap :: Map.Map NontermIdent Attributes
         _lhsOinhmap = rule93 arg_inh_ arg_nt_
         _lhsOsynmap :: Map.Map NontermIdent Attributes
         _lhsOsynmap = rule94 arg_nt_ arg_syn_
         _lhsOlocalSigMap :: Map.Map NontermIdent (Map.Map ConstructorIdent (Map.Map Identifier Type))
         _lhsOlocalSigMap = rule95 _prodsIlocalSigMap arg_nt_
         _lhsOrulenumber :: Int
         _lhsOrulenumber = rule96 _prodsIrulenumber
         _prodsOaroundMap = rule97 _aroundMap
         _prodsOinhMap = rule98 _lhsIinhMap
         _prodsOmergeMap = rule99 _mergeMap
         _prodsOoptions = rule100 _lhsIoptions
         _prodsOrulenumber = rule101 _lhsIrulenumber
         _prodsOsynMap = rule102 _lhsIsynMap
         __result_ = T_Nonterminal_vOut22 _lhsOdepinfo _lhsOinhMap' _lhsOinhmap _lhsOlocalSigMap _lhsOntDeps _lhsOntHoDeps _lhsOrulenumber _lhsOsynMap' _lhsOsynmap
         in __result_ )
     in C_Nonterminal_s23 v22
   {-# INLINE rule74 #-}
   {-# LINE 7 "./src-ag/DistChildAttr.ag" #-}
   rule74 = \ inh_ nt_ ->
                                 {-# LINE 7 "./src-ag/DistChildAttr.ag" #-}
                                 Map.singleton nt_ inh_
                                 {-# LINE 1002 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule75 #-}
   {-# LINE 8 "./src-ag/DistChildAttr.ag" #-}
   rule75 = \ nt_ syn_ ->
                                 {-# LINE 8 "./src-ag/DistChildAttr.ag" #-}
                                 Map.singleton nt_ syn_
                                 {-# LINE 1008 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule76 #-}
   {-# LINE 59 "./src-ag/KWOrder.ag" #-}
   rule76 = \ ((_prodsIrefNts) :: Set NontermIdent) nt_ ->
                            {-# LINE 59 "./src-ag/KWOrder.ag" #-}
                            Map.singleton nt_ _prodsIrefNts
                            {-# LINE 1014 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule77 #-}
   {-# LINE 60 "./src-ag/KWOrder.ag" #-}
   rule77 = \ ((_prodsIrefHoNts) :: Set NontermIdent) nt_ ->
                            {-# LINE 60 "./src-ag/KWOrder.ag" #-}
                            Map.singleton nt_ _prodsIrefHoNts
                            {-# LINE 1020 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule78 #-}
   {-# LINE 62 "./src-ag/KWOrder.ag" #-}
   rule78 = \ ((_lhsIclosedNtDeps) :: Map NontermIdent (Set NontermIdent)) nt_ ->
                            {-# LINE 62 "./src-ag/KWOrder.ag" #-}
                            Map.findWithDefault Set.empty nt_ _lhsIclosedNtDeps
                            {-# LINE 1026 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule79 #-}
   {-# LINE 63 "./src-ag/KWOrder.ag" #-}
   rule79 = \ ((_lhsIclosedHoNtDeps) :: Map NontermIdent (Set NontermIdent)) nt_ ->
                            {-# LINE 63 "./src-ag/KWOrder.ag" #-}
                            Map.findWithDefault Set.empty nt_ _lhsIclosedHoNtDeps
                            {-# LINE 1032 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule80 #-}
   {-# LINE 64 "./src-ag/KWOrder.ag" #-}
   rule80 = \ ((_lhsIclosedHoNtRevDeps) :: Map NontermIdent (Set NontermIdent)) nt_ ->
                            {-# LINE 64 "./src-ag/KWOrder.ag" #-}
                            Map.findWithDefault Set.empty nt_ _lhsIclosedHoNtRevDeps
                            {-# LINE 1038 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule81 #-}
   {-# LINE 66 "./src-ag/KWOrder.ag" #-}
   rule81 = \ _closedNtDeps nt_ ->
                            {-# LINE 66 "./src-ag/KWOrder.ag" #-}
                            nt_ `Set.member` _closedNtDeps
                            {-# LINE 1044 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule82 #-}
   {-# LINE 67 "./src-ag/KWOrder.ag" #-}
   rule82 = \ _closedHoNtDeps nt_ ->
                            {-# LINE 67 "./src-ag/KWOrder.ag" #-}
                            nt_ `Set.member` _closedHoNtDeps
                            {-# LINE 1050 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule83 #-}
   {-# LINE 68 "./src-ag/KWOrder.ag" #-}
   rule83 = \ _closedHoNtDeps _closedHoNtRevDeps _nontrivAcyc ->
                            {-# LINE 68 "./src-ag/KWOrder.ag" #-}
                            HigherOrderInfo { hoNtDeps            = _closedHoNtDeps
                                            , hoNtRevDeps         = _closedHoNtRevDeps
                                            , hoAcyclic           = _nontrivAcyc
                                            }
                            {-# LINE 1059 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule84 #-}
   {-# LINE 101 "./src-ag/KWOrder.ag" #-}
   rule84 = \ ((_lhsIaroundMap) :: Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) nt_ ->
                                                 {-# LINE 101 "./src-ag/KWOrder.ag" #-}
                                                 Map.findWithDefault Map.empty nt_ _lhsIaroundMap
                                                 {-# LINE 1065 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule85 #-}
   {-# LINE 126 "./src-ag/KWOrder.ag" #-}
   rule85 = \ ((_lhsImergeMap) :: Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier, [Identifier], Expression)))) nt_ ->
                                                {-# LINE 126 "./src-ag/KWOrder.ag" #-}
                                                Map.findWithDefault Map.empty nt_ _lhsImergeMap
                                                {-# LINE 1071 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule86 #-}
   {-# LINE 149 "./src-ag/KWOrder.ag" #-}
   rule86 = \ ((_lhsIclassContexts) :: ContextMap) nt_ ->
                        {-# LINE 149 "./src-ag/KWOrder.ag" #-}
                        Map.findWithDefault [] nt_ _lhsIclassContexts
                        {-# LINE 1077 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule87 #-}
   {-# LINE 270 "./src-ag/KWOrder.ag" #-}
   rule87 = \ ((_lhsImanualDeps) :: AttrOrderMap) nt_ ->
                                                   {-# LINE 270 "./src-ag/KWOrder.ag" #-}
                                                   Map.findWithDefault Map.empty nt_ _lhsImanualDeps
                                                   {-# LINE 1083 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule88 #-}
   {-# LINE 325 "./src-ag/KWOrder.ag" #-}
   rule88 = \ nt_ syn_ ->
                                     {-# LINE 325 "./src-ag/KWOrder.ag" #-}
                                     map (VAttr Syn nt_) . Map.keys $ syn_
                                     {-# LINE 1089 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule89 #-}
   {-# LINE 326 "./src-ag/KWOrder.ag" #-}
   rule89 = \ inh_ nt_ ->
                                     {-# LINE 326 "./src-ag/KWOrder.ag" #-}
                                     map (VAttr Inh nt_) . Map.keys $ inh_
                                     {-# LINE 1095 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule90 #-}
   {-# LINE 327 "./src-ag/KWOrder.ag" #-}
   rule90 = \ _inhvertices _synvertices ->
                                     {-# LINE 327 "./src-ag/KWOrder.ag" #-}
                                     _synvertices     ++ _inhvertices
                                     {-# LINE 1101 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule91 #-}
   {-# LINE 331 "./src-ag/KWOrder.ag" #-}
   rule91 = \ _vertices ->
                                   {-# LINE 331 "./src-ag/KWOrder.ag" #-}
                                   NontDependencyGraph { ndgVertices = _vertices
                                                       , ndgEdges    = [] }
                                   {-# LINE 1108 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule92 #-}
   {-# LINE 339 "./src-ag/KWOrder.ag" #-}
   rule92 = \ _classContexts _hoInfo _nontgraph ((_prodsIdepgraph) :: [ProdDependencyGraph]) _recursive inh_ nt_ params_ syn_ ->
                                 {-# LINE 339 "./src-ag/KWOrder.ag" #-}
                                 NontDependencyInformation { ndiNonterminal = nt_
                                                           , ndiParams      = params_
                                                           , ndiInh         = Map.keys inh_
                                                           , ndiSyn         = Map.keys syn_
                                                           , ndiDepGraph    = _nontgraph
                                                           , ndiProds       = _prodsIdepgraph
                                                           , ndiRecursive   = _recursive
                                                           , ndiHoInfo      = _hoInfo
                                                           , ndiClassCtxs   = _classContexts
                                                           }
                                 {-# LINE 1123 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule93 #-}
   {-# LINE 377 "./src-ag/KWOrder.ag" #-}
   rule93 = \ inh_ nt_ ->
                               {-# LINE 377 "./src-ag/KWOrder.ag" #-}
                               Map.singleton nt_ inh_
                               {-# LINE 1129 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule94 #-}
   {-# LINE 378 "./src-ag/KWOrder.ag" #-}
   rule94 = \ nt_ syn_ ->
                               {-# LINE 378 "./src-ag/KWOrder.ag" #-}
                               Map.singleton nt_ syn_
                               {-# LINE 1135 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule95 #-}
   {-# LINE 387 "./src-ag/KWOrder.ag" #-}
   rule95 = \ ((_prodsIlocalSigMap) :: Map.Map ConstructorIdent (Map.Map Identifier Type)) nt_ ->
                                                   {-# LINE 387 "./src-ag/KWOrder.ag" #-}
                                                   Map.singleton nt_ _prodsIlocalSigMap
                                                   {-# LINE 1141 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule96 #-}
   rule96 = \ ((_prodsIrulenumber) :: Int) ->
     _prodsIrulenumber
   {-# INLINE rule97 #-}
   rule97 = \ _aroundMap ->
     _aroundMap
   {-# INLINE rule98 #-}
   rule98 = \ ((_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# INLINE rule99 #-}
   rule99 = \ _mergeMap ->
     _mergeMap
   {-# INLINE rule100 #-}
   rule100 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule101 #-}
   rule101 = \ ((_lhsIrulenumber) :: Int) ->
     _lhsIrulenumber
   {-# INLINE rule102 #-}
   rule102 = \ ((_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap

-- Nonterminals ------------------------------------------------
-- wrapper
data Inh_Nonterminals  = Inh_Nonterminals { aroundMap_Inh_Nonterminals :: (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))), classContexts_Inh_Nonterminals :: (ContextMap), closedHoNtDeps_Inh_Nonterminals :: (Map NontermIdent (Set NontermIdent)), closedHoNtRevDeps_Inh_Nonterminals :: (Map NontermIdent (Set NontermIdent)), closedNtDeps_Inh_Nonterminals :: (Map NontermIdent (Set NontermIdent)), inhMap_Inh_Nonterminals :: (Map Identifier Attributes), manualDeps_Inh_Nonterminals :: (AttrOrderMap), mergeMap_Inh_Nonterminals :: (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier, [Identifier], Expression)))), options_Inh_Nonterminals :: (Options), rulenumber_Inh_Nonterminals :: (Int), synMap_Inh_Nonterminals :: (Map Identifier Attributes) }
data Syn_Nonterminals  = Syn_Nonterminals { depinfo_Syn_Nonterminals :: ([NontDependencyInformation]), inhMap'_Syn_Nonterminals :: (Map Identifier Attributes), inhmap_Syn_Nonterminals :: (Map.Map NontermIdent Attributes), localSigMap_Syn_Nonterminals :: (Map.Map NontermIdent (Map.Map ConstructorIdent (Map.Map Identifier Type))), ntDeps_Syn_Nonterminals :: (Map NontermIdent (Set NontermIdent)), ntHoDeps_Syn_Nonterminals :: (Map NontermIdent (Set NontermIdent)), rulenumber_Syn_Nonterminals :: (Int), synMap'_Syn_Nonterminals :: (Map Identifier Attributes), synmap_Syn_Nonterminals :: (Map.Map NontermIdent Attributes) }
{-# INLINABLE wrap_Nonterminals #-}
wrap_Nonterminals :: T_Nonterminals  -> Inh_Nonterminals  -> (Syn_Nonterminals )
wrap_Nonterminals (T_Nonterminals act) (Inh_Nonterminals _lhsIaroundMap _lhsIclassContexts _lhsIclosedHoNtDeps _lhsIclosedHoNtRevDeps _lhsIclosedNtDeps _lhsIinhMap _lhsImanualDeps _lhsImergeMap _lhsIoptions _lhsIrulenumber _lhsIsynMap) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Nonterminals_vIn25 _lhsIaroundMap _lhsIclassContexts _lhsIclosedHoNtDeps _lhsIclosedHoNtRevDeps _lhsIclosedNtDeps _lhsIinhMap _lhsImanualDeps _lhsImergeMap _lhsIoptions _lhsIrulenumber _lhsIsynMap
        (T_Nonterminals_vOut25 _lhsOdepinfo _lhsOinhMap' _lhsOinhmap _lhsOlocalSigMap _lhsOntDeps _lhsOntHoDeps _lhsOrulenumber _lhsOsynMap' _lhsOsynmap) <- return (inv_Nonterminals_s26 sem arg)
        return (Syn_Nonterminals _lhsOdepinfo _lhsOinhMap' _lhsOinhmap _lhsOlocalSigMap _lhsOntDeps _lhsOntHoDeps _lhsOrulenumber _lhsOsynMap' _lhsOsynmap)
   )

-- cata
{-# NOINLINE sem_Nonterminals #-}
sem_Nonterminals :: Nonterminals  -> T_Nonterminals 
sem_Nonterminals list = Prelude.foldr sem_Nonterminals_Cons sem_Nonterminals_Nil (Prelude.map sem_Nonterminal list)

-- semantic domain
newtype T_Nonterminals  = T_Nonterminals {
                                         attach_T_Nonterminals :: Identity (T_Nonterminals_s26 )
                                         }
newtype T_Nonterminals_s26  = C_Nonterminals_s26 {
                                                 inv_Nonterminals_s26 :: (T_Nonterminals_v25 )
                                                 }
data T_Nonterminals_s27  = C_Nonterminals_s27
type T_Nonterminals_v25  = (T_Nonterminals_vIn25 ) -> (T_Nonterminals_vOut25 )
data T_Nonterminals_vIn25  = T_Nonterminals_vIn25 (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) (ContextMap) (Map NontermIdent (Set NontermIdent)) (Map NontermIdent (Set NontermIdent)) (Map NontermIdent (Set NontermIdent)) (Map Identifier Attributes) (AttrOrderMap) (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier, [Identifier], Expression)))) (Options) (Int) (Map Identifier Attributes)
data T_Nonterminals_vOut25  = T_Nonterminals_vOut25 ([NontDependencyInformation]) (Map Identifier Attributes) (Map.Map NontermIdent Attributes) (Map.Map NontermIdent (Map.Map ConstructorIdent (Map.Map Identifier Type))) (Map NontermIdent (Set NontermIdent)) (Map NontermIdent (Set NontermIdent)) (Int) (Map Identifier Attributes) (Map.Map NontermIdent Attributes)
{-# NOINLINE sem_Nonterminals_Cons #-}
sem_Nonterminals_Cons :: T_Nonterminal  -> T_Nonterminals  -> T_Nonterminals 
sem_Nonterminals_Cons arg_hd_ arg_tl_ = T_Nonterminals (return st26) where
   {-# NOINLINE st26 #-}
   st26 = let
      v25 :: T_Nonterminals_v25 
      v25 = \ (T_Nonterminals_vIn25 _lhsIaroundMap _lhsIclassContexts _lhsIclosedHoNtDeps _lhsIclosedHoNtRevDeps _lhsIclosedNtDeps _lhsIinhMap _lhsImanualDeps _lhsImergeMap _lhsIoptions _lhsIrulenumber _lhsIsynMap) -> ( let
         _hdX23 = Control.Monad.Identity.runIdentity (attach_T_Nonterminal (arg_hd_))
         _tlX26 = Control.Monad.Identity.runIdentity (attach_T_Nonterminals (arg_tl_))
         (T_Nonterminal_vOut22 _hdIdepinfo _hdIinhMap' _hdIinhmap _hdIlocalSigMap _hdIntDeps _hdIntHoDeps _hdIrulenumber _hdIsynMap' _hdIsynmap) = inv_Nonterminal_s23 _hdX23 (T_Nonterminal_vIn22 _hdOaroundMap _hdOclassContexts _hdOclosedHoNtDeps _hdOclosedHoNtRevDeps _hdOclosedNtDeps _hdOinhMap _hdOmanualDeps _hdOmergeMap _hdOoptions _hdOrulenumber _hdOsynMap)
         (T_Nonterminals_vOut25 _tlIdepinfo _tlIinhMap' _tlIinhmap _tlIlocalSigMap _tlIntDeps _tlIntHoDeps _tlIrulenumber _tlIsynMap' _tlIsynmap) = inv_Nonterminals_s26 _tlX26 (T_Nonterminals_vIn25 _tlOaroundMap _tlOclassContexts _tlOclosedHoNtDeps _tlOclosedHoNtRevDeps _tlOclosedNtDeps _tlOinhMap _tlOmanualDeps _tlOmergeMap _tlOoptions _tlOrulenumber _tlOsynMap)
         _lhsOdepinfo :: [NontDependencyInformation]
         _lhsOdepinfo = rule103 _hdIdepinfo _tlIdepinfo
         _lhsOinhMap' :: Map Identifier Attributes
         _lhsOinhMap' = rule104 _hdIinhMap' _tlIinhMap'
         _lhsOinhmap :: Map.Map NontermIdent Attributes
         _lhsOinhmap = rule105 _hdIinhmap _tlIinhmap
         _lhsOlocalSigMap :: Map.Map NontermIdent (Map.Map ConstructorIdent (Map.Map Identifier Type))
         _lhsOlocalSigMap = rule106 _hdIlocalSigMap _tlIlocalSigMap
         _lhsOntDeps :: Map NontermIdent (Set NontermIdent)
         _lhsOntDeps = rule107 _hdIntDeps _tlIntDeps
         _lhsOntHoDeps :: Map NontermIdent (Set NontermIdent)
         _lhsOntHoDeps = rule108 _hdIntHoDeps _tlIntHoDeps
         _lhsOsynMap' :: Map Identifier Attributes
         _lhsOsynMap' = rule109 _hdIsynMap' _tlIsynMap'
         _lhsOsynmap :: Map.Map NontermIdent Attributes
         _lhsOsynmap = rule110 _hdIsynmap _tlIsynmap
         _lhsOrulenumber :: Int
         _lhsOrulenumber = rule111 _tlIrulenumber
         _hdOaroundMap = rule112 _lhsIaroundMap
         _hdOclassContexts = rule113 _lhsIclassContexts
         _hdOclosedHoNtDeps = rule114 _lhsIclosedHoNtDeps
         _hdOclosedHoNtRevDeps = rule115 _lhsIclosedHoNtRevDeps
         _hdOclosedNtDeps = rule116 _lhsIclosedNtDeps
         _hdOinhMap = rule117 _lhsIinhMap
         _hdOmanualDeps = rule118 _lhsImanualDeps
         _hdOmergeMap = rule119 _lhsImergeMap
         _hdOoptions = rule120 _lhsIoptions
         _hdOrulenumber = rule121 _lhsIrulenumber
         _hdOsynMap = rule122 _lhsIsynMap
         _tlOaroundMap = rule123 _lhsIaroundMap
         _tlOclassContexts = rule124 _lhsIclassContexts
         _tlOclosedHoNtDeps = rule125 _lhsIclosedHoNtDeps
         _tlOclosedHoNtRevDeps = rule126 _lhsIclosedHoNtRevDeps
         _tlOclosedNtDeps = rule127 _lhsIclosedNtDeps
         _tlOinhMap = rule128 _lhsIinhMap
         _tlOmanualDeps = rule129 _lhsImanualDeps
         _tlOmergeMap = rule130 _lhsImergeMap
         _tlOoptions = rule131 _lhsIoptions
         _tlOrulenumber = rule132 _hdIrulenumber
         _tlOsynMap = rule133 _lhsIsynMap
         __result_ = T_Nonterminals_vOut25 _lhsOdepinfo _lhsOinhMap' _lhsOinhmap _lhsOlocalSigMap _lhsOntDeps _lhsOntHoDeps _lhsOrulenumber _lhsOsynMap' _lhsOsynmap
         in __result_ )
     in C_Nonterminals_s26 v25
   {-# INLINE rule103 #-}
   rule103 = \ ((_hdIdepinfo) :: NontDependencyInformation) ((_tlIdepinfo) :: [NontDependencyInformation]) ->
     _hdIdepinfo : _tlIdepinfo
   {-# INLINE rule104 #-}
   rule104 = \ ((_hdIinhMap') :: Map Identifier Attributes) ((_tlIinhMap') :: Map Identifier Attributes) ->
     _hdIinhMap' `Map.union` _tlIinhMap'
   {-# INLINE rule105 #-}
   rule105 = \ ((_hdIinhmap) :: Map.Map NontermIdent Attributes) ((_tlIinhmap) :: Map.Map NontermIdent Attributes) ->
     _hdIinhmap `Map.union` _tlIinhmap
   {-# INLINE rule106 #-}
   rule106 = \ ((_hdIlocalSigMap) :: Map.Map NontermIdent (Map.Map ConstructorIdent (Map.Map Identifier Type))) ((_tlIlocalSigMap) :: Map.Map NontermIdent (Map.Map ConstructorIdent (Map.Map Identifier Type))) ->
     _hdIlocalSigMap `Map.union` _tlIlocalSigMap
   {-# INLINE rule107 #-}
   rule107 = \ ((_hdIntDeps) :: Map NontermIdent (Set NontermIdent)) ((_tlIntDeps) :: Map NontermIdent (Set NontermIdent)) ->
     _hdIntDeps `mappend` _tlIntDeps
   {-# INLINE rule108 #-}
   rule108 = \ ((_hdIntHoDeps) :: Map NontermIdent (Set NontermIdent)) ((_tlIntHoDeps) :: Map NontermIdent (Set NontermIdent)) ->
     _hdIntHoDeps `mappend` _tlIntHoDeps
   {-# INLINE rule109 #-}
   rule109 = \ ((_hdIsynMap') :: Map Identifier Attributes) ((_tlIsynMap') :: Map Identifier Attributes) ->
     _hdIsynMap' `Map.union` _tlIsynMap'
   {-# INLINE rule110 #-}
   rule110 = \ ((_hdIsynmap) :: Map.Map NontermIdent Attributes) ((_tlIsynmap) :: Map.Map NontermIdent Attributes) ->
     _hdIsynmap `Map.union` _tlIsynmap
   {-# INLINE rule111 #-}
   rule111 = \ ((_tlIrulenumber) :: Int) ->
     _tlIrulenumber
   {-# INLINE rule112 #-}
   rule112 = \ ((_lhsIaroundMap) :: Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) ->
     _lhsIaroundMap
   {-# INLINE rule113 #-}
   rule113 = \ ((_lhsIclassContexts) :: ContextMap) ->
     _lhsIclassContexts
   {-# INLINE rule114 #-}
   rule114 = \ ((_lhsIclosedHoNtDeps) :: Map NontermIdent (Set NontermIdent)) ->
     _lhsIclosedHoNtDeps
   {-# INLINE rule115 #-}
   rule115 = \ ((_lhsIclosedHoNtRevDeps) :: Map NontermIdent (Set NontermIdent)) ->
     _lhsIclosedHoNtRevDeps
   {-# INLINE rule116 #-}
   rule116 = \ ((_lhsIclosedNtDeps) :: Map NontermIdent (Set NontermIdent)) ->
     _lhsIclosedNtDeps
   {-# INLINE rule117 #-}
   rule117 = \ ((_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# INLINE rule118 #-}
   rule118 = \ ((_lhsImanualDeps) :: AttrOrderMap) ->
     _lhsImanualDeps
   {-# INLINE rule119 #-}
   rule119 = \ ((_lhsImergeMap) :: Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier, [Identifier], Expression)))) ->
     _lhsImergeMap
   {-# INLINE rule120 #-}
   rule120 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule121 #-}
   rule121 = \ ((_lhsIrulenumber) :: Int) ->
     _lhsIrulenumber
   {-# INLINE rule122 #-}
   rule122 = \ ((_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
   {-# INLINE rule123 #-}
   rule123 = \ ((_lhsIaroundMap) :: Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) ->
     _lhsIaroundMap
   {-# INLINE rule124 #-}
   rule124 = \ ((_lhsIclassContexts) :: ContextMap) ->
     _lhsIclassContexts
   {-# INLINE rule125 #-}
   rule125 = \ ((_lhsIclosedHoNtDeps) :: Map NontermIdent (Set NontermIdent)) ->
     _lhsIclosedHoNtDeps
   {-# INLINE rule126 #-}
   rule126 = \ ((_lhsIclosedHoNtRevDeps) :: Map NontermIdent (Set NontermIdent)) ->
     _lhsIclosedHoNtRevDeps
   {-# INLINE rule127 #-}
   rule127 = \ ((_lhsIclosedNtDeps) :: Map NontermIdent (Set NontermIdent)) ->
     _lhsIclosedNtDeps
   {-# INLINE rule128 #-}
   rule128 = \ ((_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# INLINE rule129 #-}
   rule129 = \ ((_lhsImanualDeps) :: AttrOrderMap) ->
     _lhsImanualDeps
   {-# INLINE rule130 #-}
   rule130 = \ ((_lhsImergeMap) :: Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier, [Identifier], Expression)))) ->
     _lhsImergeMap
   {-# INLINE rule131 #-}
   rule131 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule132 #-}
   rule132 = \ ((_hdIrulenumber) :: Int) ->
     _hdIrulenumber
   {-# INLINE rule133 #-}
   rule133 = \ ((_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
{-# NOINLINE sem_Nonterminals_Nil #-}
sem_Nonterminals_Nil ::  T_Nonterminals 
sem_Nonterminals_Nil  = T_Nonterminals (return st26) where
   {-# NOINLINE st26 #-}
   st26 = let
      v25 :: T_Nonterminals_v25 
      v25 = \ (T_Nonterminals_vIn25 _lhsIaroundMap _lhsIclassContexts _lhsIclosedHoNtDeps _lhsIclosedHoNtRevDeps _lhsIclosedNtDeps _lhsIinhMap _lhsImanualDeps _lhsImergeMap _lhsIoptions _lhsIrulenumber _lhsIsynMap) -> ( let
         _lhsOdepinfo :: [NontDependencyInformation]
         _lhsOdepinfo = rule134  ()
         _lhsOinhMap' :: Map Identifier Attributes
         _lhsOinhMap' = rule135  ()
         _lhsOinhmap :: Map.Map NontermIdent Attributes
         _lhsOinhmap = rule136  ()
         _lhsOlocalSigMap :: Map.Map NontermIdent (Map.Map ConstructorIdent (Map.Map Identifier Type))
         _lhsOlocalSigMap = rule137  ()
         _lhsOntDeps :: Map NontermIdent (Set NontermIdent)
         _lhsOntDeps = rule138  ()
         _lhsOntHoDeps :: Map NontermIdent (Set NontermIdent)
         _lhsOntHoDeps = rule139  ()
         _lhsOsynMap' :: Map Identifier Attributes
         _lhsOsynMap' = rule140  ()
         _lhsOsynmap :: Map.Map NontermIdent Attributes
         _lhsOsynmap = rule141  ()
         _lhsOrulenumber :: Int
         _lhsOrulenumber = rule142 _lhsIrulenumber
         __result_ = T_Nonterminals_vOut25 _lhsOdepinfo _lhsOinhMap' _lhsOinhmap _lhsOlocalSigMap _lhsOntDeps _lhsOntHoDeps _lhsOrulenumber _lhsOsynMap' _lhsOsynmap
         in __result_ )
     in C_Nonterminals_s26 v25
   {-# INLINE rule134 #-}
   rule134 = \  (_ :: ()) ->
     []
   {-# INLINE rule135 #-}
   rule135 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule136 #-}
   rule136 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule137 #-}
   rule137 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule138 #-}
   rule138 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule139 #-}
   rule139 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule140 #-}
   rule140 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule141 #-}
   rule141 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule142 #-}
   rule142 = \ ((_lhsIrulenumber) :: Int) ->
     _lhsIrulenumber

-- Pattern -----------------------------------------------------
-- wrapper
data Inh_Pattern  = Inh_Pattern {  }
data Syn_Pattern  = Syn_Pattern { copy_Syn_Pattern :: (Pattern), vertices_Syn_Pattern :: (Set.Set Vertex) }
{-# INLINABLE wrap_Pattern #-}
wrap_Pattern :: T_Pattern  -> Inh_Pattern  -> (Syn_Pattern )
wrap_Pattern (T_Pattern act) (Inh_Pattern ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Pattern_vIn28 
        (T_Pattern_vOut28 _lhsOcopy _lhsOvertices) <- return (inv_Pattern_s29 sem arg)
        return (Syn_Pattern _lhsOcopy _lhsOvertices)
   )

-- cata
{-# NOINLINE sem_Pattern #-}
sem_Pattern :: Pattern  -> T_Pattern 
sem_Pattern ( Constr name_ pats_ ) = sem_Pattern_Constr name_ ( sem_Patterns pats_ )
sem_Pattern ( Product pos_ pats_ ) = sem_Pattern_Product pos_ ( sem_Patterns pats_ )
sem_Pattern ( Alias field_ attr_ pat_ ) = sem_Pattern_Alias field_ attr_ ( sem_Pattern pat_ )
sem_Pattern ( Irrefutable pat_ ) = sem_Pattern_Irrefutable ( sem_Pattern pat_ )
sem_Pattern ( Underscore pos_ ) = sem_Pattern_Underscore pos_

-- semantic domain
newtype T_Pattern  = T_Pattern {
                               attach_T_Pattern :: Identity (T_Pattern_s29 )
                               }
newtype T_Pattern_s29  = C_Pattern_s29 {
                                       inv_Pattern_s29 :: (T_Pattern_v28 )
                                       }
data T_Pattern_s30  = C_Pattern_s30
type T_Pattern_v28  = (T_Pattern_vIn28 ) -> (T_Pattern_vOut28 )
data T_Pattern_vIn28  = T_Pattern_vIn28 
data T_Pattern_vOut28  = T_Pattern_vOut28 (Pattern) (Set.Set Vertex)
{-# NOINLINE sem_Pattern_Constr #-}
sem_Pattern_Constr :: (ConstructorIdent) -> T_Patterns  -> T_Pattern 
sem_Pattern_Constr arg_name_ arg_pats_ = T_Pattern (return st29) where
   {-# NOINLINE st29 #-}
   st29 = let
      v28 :: T_Pattern_v28 
      v28 = \ (T_Pattern_vIn28 ) -> ( let
         _patsX32 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_pats_))
         (T_Patterns_vOut31 _patsIcopy _patsIvertices) = inv_Patterns_s32 _patsX32 (T_Patterns_vIn31 )
         _lhsOvertices :: Set.Set Vertex
         _lhsOvertices = rule143 _patsIvertices
         _copy = rule144 _patsIcopy arg_name_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule145 _copy
         __result_ = T_Pattern_vOut28 _lhsOcopy _lhsOvertices
         in __result_ )
     in C_Pattern_s29 v28
   {-# INLINE rule143 #-}
   rule143 = \ ((_patsIvertices) :: Set.Set Vertex) ->
     _patsIvertices
   {-# INLINE rule144 #-}
   rule144 = \ ((_patsIcopy) :: Patterns) name_ ->
     Constr name_ _patsIcopy
   {-# INLINE rule145 #-}
   rule145 = \ _copy ->
     _copy
{-# NOINLINE sem_Pattern_Product #-}
sem_Pattern_Product :: (Pos) -> T_Patterns  -> T_Pattern 
sem_Pattern_Product arg_pos_ arg_pats_ = T_Pattern (return st29) where
   {-# NOINLINE st29 #-}
   st29 = let
      v28 :: T_Pattern_v28 
      v28 = \ (T_Pattern_vIn28 ) -> ( let
         _patsX32 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_pats_))
         (T_Patterns_vOut31 _patsIcopy _patsIvertices) = inv_Patterns_s32 _patsX32 (T_Patterns_vIn31 )
         _lhsOvertices :: Set.Set Vertex
         _lhsOvertices = rule146 _patsIvertices
         _copy = rule147 _patsIcopy arg_pos_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule148 _copy
         __result_ = T_Pattern_vOut28 _lhsOcopy _lhsOvertices
         in __result_ )
     in C_Pattern_s29 v28
   {-# INLINE rule146 #-}
   rule146 = \ ((_patsIvertices) :: Set.Set Vertex) ->
     _patsIvertices
   {-# INLINE rule147 #-}
   rule147 = \ ((_patsIcopy) :: Patterns) pos_ ->
     Product pos_ _patsIcopy
   {-# INLINE rule148 #-}
   rule148 = \ _copy ->
     _copy
{-# NOINLINE sem_Pattern_Alias #-}
sem_Pattern_Alias :: (Identifier) -> (Identifier) -> T_Pattern  -> T_Pattern 
sem_Pattern_Alias arg_field_ arg_attr_ arg_pat_ = T_Pattern (return st29) where
   {-# NOINLINE st29 #-}
   st29 = let
      v28 :: T_Pattern_v28 
      v28 = \ (T_Pattern_vIn28 ) -> ( let
         _patX29 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pat_))
         (T_Pattern_vOut28 _patIcopy _patIvertices) = inv_Pattern_s29 _patX29 (T_Pattern_vIn28 )
         _vertex = rule149 arg_attr_ arg_field_
         _lhsOvertices :: Set.Set Vertex
         _lhsOvertices = rule150 _patIvertices _vertex
         _copy = rule151 _patIcopy arg_attr_ arg_field_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule152 _copy
         __result_ = T_Pattern_vOut28 _lhsOcopy _lhsOvertices
         in __result_ )
     in C_Pattern_s29 v28
   {-# INLINE rule149 #-}
   {-# LINE 205 "./src-ag/KWOrder.ag" #-}
   rule149 = \ attr_ field_ ->
                            {-# LINE 205 "./src-ag/KWOrder.ag" #-}
                            if                  field_ == _INST then VChild attr_
                            else VAttr (if      field_ == _LHS  then Syn
                                        else if field_ == _LOC  then Loc
                                        else                         Inh) field_ attr_
                            {-# LINE 1509 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule150 #-}
   {-# LINE 209 "./src-ag/KWOrder.ag" #-}
   rule150 = \ ((_patIvertices) :: Set.Set Vertex) _vertex ->
                            {-# LINE 209 "./src-ag/KWOrder.ag" #-}
                            Set.insert _vertex     _patIvertices
                            {-# LINE 1515 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule151 #-}
   rule151 = \ ((_patIcopy) :: Pattern) attr_ field_ ->
     Alias field_ attr_ _patIcopy
   {-# INLINE rule152 #-}
   rule152 = \ _copy ->
     _copy
{-# NOINLINE sem_Pattern_Irrefutable #-}
sem_Pattern_Irrefutable :: T_Pattern  -> T_Pattern 
sem_Pattern_Irrefutable arg_pat_ = T_Pattern (return st29) where
   {-# NOINLINE st29 #-}
   st29 = let
      v28 :: T_Pattern_v28 
      v28 = \ (T_Pattern_vIn28 ) -> ( let
         _patX29 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pat_))
         (T_Pattern_vOut28 _patIcopy _patIvertices) = inv_Pattern_s29 _patX29 (T_Pattern_vIn28 )
         _lhsOvertices :: Set.Set Vertex
         _lhsOvertices = rule153 _patIvertices
         _copy = rule154 _patIcopy
         _lhsOcopy :: Pattern
         _lhsOcopy = rule155 _copy
         __result_ = T_Pattern_vOut28 _lhsOcopy _lhsOvertices
         in __result_ )
     in C_Pattern_s29 v28
   {-# INLINE rule153 #-}
   rule153 = \ ((_patIvertices) :: Set.Set Vertex) ->
     _patIvertices
   {-# INLINE rule154 #-}
   rule154 = \ ((_patIcopy) :: Pattern) ->
     Irrefutable _patIcopy
   {-# INLINE rule155 #-}
   rule155 = \ _copy ->
     _copy
{-# NOINLINE sem_Pattern_Underscore #-}
sem_Pattern_Underscore :: (Pos) -> T_Pattern 
sem_Pattern_Underscore arg_pos_ = T_Pattern (return st29) where
   {-# NOINLINE st29 #-}
   st29 = let
      v28 :: T_Pattern_v28 
      v28 = \ (T_Pattern_vIn28 ) -> ( let
         _lhsOvertices :: Set.Set Vertex
         _lhsOvertices = rule156  ()
         _copy = rule157 arg_pos_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule158 _copy
         __result_ = T_Pattern_vOut28 _lhsOcopy _lhsOvertices
         in __result_ )
     in C_Pattern_s29 v28
   {-# INLINE rule156 #-}
   rule156 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule157 #-}
   rule157 = \ pos_ ->
     Underscore pos_
   {-# INLINE rule158 #-}
   rule158 = \ _copy ->
     _copy

-- Patterns ----------------------------------------------------
-- wrapper
data Inh_Patterns  = Inh_Patterns {  }
data Syn_Patterns  = Syn_Patterns { copy_Syn_Patterns :: (Patterns), vertices_Syn_Patterns :: (Set.Set Vertex) }
{-# INLINABLE wrap_Patterns #-}
wrap_Patterns :: T_Patterns  -> Inh_Patterns  -> (Syn_Patterns )
wrap_Patterns (T_Patterns act) (Inh_Patterns ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Patterns_vIn31 
        (T_Patterns_vOut31 _lhsOcopy _lhsOvertices) <- return (inv_Patterns_s32 sem arg)
        return (Syn_Patterns _lhsOcopy _lhsOvertices)
   )

-- cata
{-# NOINLINE sem_Patterns #-}
sem_Patterns :: Patterns  -> T_Patterns 
sem_Patterns list = Prelude.foldr sem_Patterns_Cons sem_Patterns_Nil (Prelude.map sem_Pattern list)

-- semantic domain
newtype T_Patterns  = T_Patterns {
                                 attach_T_Patterns :: Identity (T_Patterns_s32 )
                                 }
newtype T_Patterns_s32  = C_Patterns_s32 {
                                         inv_Patterns_s32 :: (T_Patterns_v31 )
                                         }
data T_Patterns_s33  = C_Patterns_s33
type T_Patterns_v31  = (T_Patterns_vIn31 ) -> (T_Patterns_vOut31 )
data T_Patterns_vIn31  = T_Patterns_vIn31 
data T_Patterns_vOut31  = T_Patterns_vOut31 (Patterns) (Set.Set Vertex)
{-# NOINLINE sem_Patterns_Cons #-}
sem_Patterns_Cons :: T_Pattern  -> T_Patterns  -> T_Patterns 
sem_Patterns_Cons arg_hd_ arg_tl_ = T_Patterns (return st32) where
   {-# NOINLINE st32 #-}
   st32 = let
      v31 :: T_Patterns_v31 
      v31 = \ (T_Patterns_vIn31 ) -> ( let
         _hdX29 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_hd_))
         _tlX32 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_tl_))
         (T_Pattern_vOut28 _hdIcopy _hdIvertices) = inv_Pattern_s29 _hdX29 (T_Pattern_vIn28 )
         (T_Patterns_vOut31 _tlIcopy _tlIvertices) = inv_Patterns_s32 _tlX32 (T_Patterns_vIn31 )
         _lhsOvertices :: Set.Set Vertex
         _lhsOvertices = rule159 _hdIvertices _tlIvertices
         _copy = rule160 _hdIcopy _tlIcopy
         _lhsOcopy :: Patterns
         _lhsOcopy = rule161 _copy
         __result_ = T_Patterns_vOut31 _lhsOcopy _lhsOvertices
         in __result_ )
     in C_Patterns_s32 v31
   {-# INLINE rule159 #-}
   rule159 = \ ((_hdIvertices) :: Set.Set Vertex) ((_tlIvertices) :: Set.Set Vertex) ->
     _hdIvertices `Set.union` _tlIvertices
   {-# INLINE rule160 #-}
   rule160 = \ ((_hdIcopy) :: Pattern) ((_tlIcopy) :: Patterns) ->
     (:) _hdIcopy _tlIcopy
   {-# INLINE rule161 #-}
   rule161 = \ _copy ->
     _copy
{-# NOINLINE sem_Patterns_Nil #-}
sem_Patterns_Nil ::  T_Patterns 
sem_Patterns_Nil  = T_Patterns (return st32) where
   {-# NOINLINE st32 #-}
   st32 = let
      v31 :: T_Patterns_v31 
      v31 = \ (T_Patterns_vIn31 ) -> ( let
         _lhsOvertices :: Set.Set Vertex
         _lhsOvertices = rule162  ()
         _copy = rule163  ()
         _lhsOcopy :: Patterns
         _lhsOcopy = rule164 _copy
         __result_ = T_Patterns_vOut31 _lhsOcopy _lhsOvertices
         in __result_ )
     in C_Patterns_s32 v31
   {-# INLINE rule162 #-}
   rule162 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule163 #-}
   rule163 = \  (_ :: ()) ->
     []
   {-# INLINE rule164 #-}
   rule164 = \ _copy ->
     _copy

-- Production --------------------------------------------------
-- wrapper
data Inh_Production  = Inh_Production { aroundMap_Inh_Production :: (Map ConstructorIdent (Map Identifier [Expression])), inhMap_Inh_Production :: (Map Identifier Attributes), manualDeps_Inh_Production :: (Map ConstructorIdent (Set Dependency)), mergeMap_Inh_Production :: (Map ConstructorIdent (Map Identifier (Identifier, [Identifier], Expression))), options_Inh_Production :: (Options), rulenumber_Inh_Production :: (Int), synMap_Inh_Production :: (Map Identifier Attributes) }
data Syn_Production  = Syn_Production { depgraph_Syn_Production :: (ProdDependencyGraph), localSigMap_Syn_Production :: (Map.Map ConstructorIdent (Map.Map Identifier Type)), refHoNts_Syn_Production :: (Set NontermIdent), refNts_Syn_Production :: (Set NontermIdent), rulenumber_Syn_Production :: (Int) }
{-# INLINABLE wrap_Production #-}
wrap_Production :: T_Production  -> Inh_Production  -> (Syn_Production )
wrap_Production (T_Production act) (Inh_Production _lhsIaroundMap _lhsIinhMap _lhsImanualDeps _lhsImergeMap _lhsIoptions _lhsIrulenumber _lhsIsynMap) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Production_vIn34 _lhsIaroundMap _lhsIinhMap _lhsImanualDeps _lhsImergeMap _lhsIoptions _lhsIrulenumber _lhsIsynMap
        (T_Production_vOut34 _lhsOdepgraph _lhsOlocalSigMap _lhsOrefHoNts _lhsOrefNts _lhsOrulenumber) <- return (inv_Production_s35 sem arg)
        return (Syn_Production _lhsOdepgraph _lhsOlocalSigMap _lhsOrefHoNts _lhsOrefNts _lhsOrulenumber)
   )

-- cata
{-# INLINE sem_Production #-}
sem_Production :: Production  -> T_Production 
sem_Production ( Production con_ params_ constraints_ children_ rules_ typeSigs_ macro_ ) = sem_Production_Production con_ params_ constraints_ ( sem_Children children_ ) ( sem_Rules rules_ ) ( sem_TypeSigs typeSigs_ ) macro_

-- semantic domain
newtype T_Production  = T_Production {
                                     attach_T_Production :: Identity (T_Production_s35 )
                                     }
newtype T_Production_s35  = C_Production_s35 {
                                             inv_Production_s35 :: (T_Production_v34 )
                                             }
data T_Production_s36  = C_Production_s36
type T_Production_v34  = (T_Production_vIn34 ) -> (T_Production_vOut34 )
data T_Production_vIn34  = T_Production_vIn34 (Map ConstructorIdent (Map Identifier [Expression])) (Map Identifier Attributes) (Map ConstructorIdent (Set Dependency)) (Map ConstructorIdent (Map Identifier (Identifier, [Identifier], Expression))) (Options) (Int) (Map Identifier Attributes)
data T_Production_vOut34  = T_Production_vOut34 (ProdDependencyGraph) (Map.Map ConstructorIdent (Map.Map Identifier Type)) (Set NontermIdent) (Set NontermIdent) (Int)
{-# NOINLINE sem_Production_Production #-}
sem_Production_Production :: (ConstructorIdent) -> ([Identifier]) -> ([Type]) -> T_Children  -> T_Rules  -> T_TypeSigs  -> (MaybeMacro) -> T_Production 
sem_Production_Production arg_con_ arg_params_ arg_constraints_ arg_children_ arg_rules_ arg_typeSigs_ _ = T_Production (return st35) where
   {-# NOINLINE st35 #-}
   st35 = let
      v34 :: T_Production_v34 
      v34 = \ (T_Production_vIn34 _lhsIaroundMap _lhsIinhMap _lhsImanualDeps _lhsImergeMap _lhsIoptions _lhsIrulenumber _lhsIsynMap) -> ( let
         _childrenX5 = Control.Monad.Identity.runIdentity (attach_T_Children (arg_children_))
         _rulesX44 = Control.Monad.Identity.runIdentity (attach_T_Rules (arg_rules_))
         _typeSigsX50 = Control.Monad.Identity.runIdentity (attach_T_TypeSigs (arg_typeSigs_))
         (T_Children_vOut4 _childrenIechilds _childrenIedges _childrenInontnames _childrenIrefHoNts _childrenIrefNts _childrenIvertices) = inv_Children_s5 _childrenX5 (T_Children_vIn4 _childrenOaroundMap _childrenOinhMap _childrenOmergeMap _childrenOmergedChildren _childrenOoptions _childrenOsynMap)
         (T_Rules_vOut43 _rulesIedges _rulesIerules _rulesIrulenumber _rulesIvertices) = inv_Rules_s44 _rulesX44 (T_Rules_vIn43 _rulesOrulenumber)
         (T_TypeSigs_vOut49 _typeSigsIlocalSigMap) = inv_TypeSigs_s50 _typeSigsX50 (T_TypeSigs_vIn49 )
         _aroundMap = rule165 _lhsIaroundMap arg_con_
         _mergeMap = rule166 _lhsImergeMap arg_con_
         _mergedChildren = rule167 _mergeMap
         _vertices = rule168 _childrenIvertices _rulesIvertices
         _manualDeps = rule169 _lhsImanualDeps arg_con_
         _manualEdges = rule170 _manualDeps
         _edges = rule171 _childrenIedges _rulesIedges
         _lhsOdepgraph :: ProdDependencyGraph
         _lhsOdepgraph = rule172 _childrenIechilds _childrenInontnames _edges _rulesIerules _vertices arg_con_ arg_constraints_ arg_params_
         _lhsOlocalSigMap :: Map.Map ConstructorIdent (Map.Map Identifier Type)
         _lhsOlocalSigMap = rule173 _typeSigsIlocalSigMap arg_con_
         _lhsOrefHoNts :: Set NontermIdent
         _lhsOrefHoNts = rule174 _childrenIrefHoNts
         _lhsOrefNts :: Set NontermIdent
         _lhsOrefNts = rule175 _childrenIrefNts
         _lhsOrulenumber :: Int
         _lhsOrulenumber = rule176 _rulesIrulenumber
         _childrenOaroundMap = rule177 _aroundMap
         _childrenOinhMap = rule178 _lhsIinhMap
         _childrenOmergeMap = rule179 _mergeMap
         _childrenOmergedChildren = rule180 _mergedChildren
         _childrenOoptions = rule181 _lhsIoptions
         _childrenOsynMap = rule182 _lhsIsynMap
         _rulesOrulenumber = rule183 _lhsIrulenumber
         __result_ = T_Production_vOut34 _lhsOdepgraph _lhsOlocalSigMap _lhsOrefHoNts _lhsOrefNts _lhsOrulenumber
         in __result_ )
     in C_Production_s35 v34
   {-# INLINE rule165 #-}
   {-# LINE 102 "./src-ag/KWOrder.ag" #-}
   rule165 = \ ((_lhsIaroundMap) :: Map ConstructorIdent (Map Identifier [Expression])) con_ ->
                                                 {-# LINE 102 "./src-ag/KWOrder.ag" #-}
                                                 Map.findWithDefault Map.empty con_ _lhsIaroundMap
                                                 {-# LINE 1731 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule166 #-}
   {-# LINE 127 "./src-ag/KWOrder.ag" #-}
   rule166 = \ ((_lhsImergeMap) :: Map ConstructorIdent (Map Identifier (Identifier, [Identifier], Expression))) con_ ->
                                                {-# LINE 127 "./src-ag/KWOrder.ag" #-}
                                                Map.findWithDefault Map.empty con_ _lhsImergeMap
                                                {-# LINE 1737 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule167 #-}
   {-# LINE 133 "./src-ag/KWOrder.ag" #-}
   rule167 = \ _mergeMap ->
                         {-# LINE 133 "./src-ag/KWOrder.ag" #-}
                         Set.unions [ Set.fromList ms | (_,ms,_) <- Map.elems _mergeMap     ]
                         {-# LINE 1743 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule168 #-}
   {-# LINE 229 "./src-ag/KWOrder.ag" #-}
   rule168 = \ ((_childrenIvertices) :: Set.Set Vertex) ((_rulesIvertices) :: Set.Set Vertex) ->
                                 {-# LINE 229 "./src-ag/KWOrder.ag" #-}
                                 _rulesIvertices `Set.union` _childrenIvertices
                                 {-# LINE 1749 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule169 #-}
   {-# LINE 272 "./src-ag/KWOrder.ag" #-}
   rule169 = \ ((_lhsImanualDeps) :: Map ConstructorIdent (Set Dependency)) con_ ->
                       {-# LINE 272 "./src-ag/KWOrder.ag" #-}
                       Map.findWithDefault Set.empty con_ _lhsImanualDeps
                       {-# LINE 1755 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule170 #-}
   {-# LINE 273 "./src-ag/KWOrder.ag" #-}
   rule170 = \ _manualDeps ->
                       {-# LINE 273 "./src-ag/KWOrder.ag" #-}
                       Set.map depToEdge _manualDeps
                       {-# LINE 1761 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule171 #-}
   {-# LINE 295 "./src-ag/KWOrder.ag" #-}
   rule171 = \ ((_childrenIedges) :: Set.Set Edge) ((_rulesIedges) :: Set.Set Edge) ->
                              {-# LINE 295 "./src-ag/KWOrder.ag" #-}
                              _rulesIedges `Set.union` _childrenIedges
                              {-# LINE 1767 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule172 #-}
   {-# LINE 310 "./src-ag/KWOrder.ag" #-}
   rule172 = \ ((_childrenIechilds) :: EChildren) ((_childrenInontnames) :: [(Identifier, Identifier)]) _edges ((_rulesIerules) :: ERules) _vertices con_ constraints_ params_ ->
                                  {-# LINE 310 "./src-ag/KWOrder.ag" #-}
                                  ProdDependencyGraph { pdgVertices    = Set.toList _vertices
                                                      , pdgEdges       = Set.toList _edges
                                                      , pdgRules       = _rulesIerules
                                                      , pdgChilds      = _childrenIechilds
                                                      , pdgProduction  = con_
                                                      , pdgChildMap    = _childrenInontnames
                                                      , pdgConstraints = constraints_
                                                      , pdgParams      = params_ }
                                  {-# LINE 1780 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule173 #-}
   {-# LINE 388 "./src-ag/KWOrder.ag" #-}
   rule173 = \ ((_typeSigsIlocalSigMap) :: Map Identifier Type) con_ ->
                                                   {-# LINE 388 "./src-ag/KWOrder.ag" #-}
                                                   Map.singleton con_ _typeSigsIlocalSigMap
                                                   {-# LINE 1786 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule174 #-}
   rule174 = \ ((_childrenIrefHoNts) :: Set NontermIdent) ->
     _childrenIrefHoNts
   {-# INLINE rule175 #-}
   rule175 = \ ((_childrenIrefNts) :: Set NontermIdent) ->
     _childrenIrefNts
   {-# INLINE rule176 #-}
   rule176 = \ ((_rulesIrulenumber) :: Int) ->
     _rulesIrulenumber
   {-# INLINE rule177 #-}
   rule177 = \ _aroundMap ->
     _aroundMap
   {-# INLINE rule178 #-}
   rule178 = \ ((_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# INLINE rule179 #-}
   rule179 = \ _mergeMap ->
     _mergeMap
   {-# INLINE rule180 #-}
   rule180 = \ _mergedChildren ->
     _mergedChildren
   {-# INLINE rule181 #-}
   rule181 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule182 #-}
   rule182 = \ ((_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
   {-# INLINE rule183 #-}
   rule183 = \ ((_lhsIrulenumber) :: Int) ->
     _lhsIrulenumber

-- Productions -------------------------------------------------
-- wrapper
data Inh_Productions  = Inh_Productions { aroundMap_Inh_Productions :: (Map ConstructorIdent (Map Identifier [Expression])), inhMap_Inh_Productions :: (Map Identifier Attributes), manualDeps_Inh_Productions :: (Map ConstructorIdent (Set Dependency)), mergeMap_Inh_Productions :: (Map ConstructorIdent (Map Identifier (Identifier, [Identifier], Expression))), options_Inh_Productions :: (Options), rulenumber_Inh_Productions :: (Int), synMap_Inh_Productions :: (Map Identifier Attributes) }
data Syn_Productions  = Syn_Productions { depgraph_Syn_Productions :: ([ProdDependencyGraph]), localSigMap_Syn_Productions :: (Map.Map ConstructorIdent (Map.Map Identifier Type)), refHoNts_Syn_Productions :: (Set NontermIdent), refNts_Syn_Productions :: (Set NontermIdent), rulenumber_Syn_Productions :: (Int) }
{-# INLINABLE wrap_Productions #-}
wrap_Productions :: T_Productions  -> Inh_Productions  -> (Syn_Productions )
wrap_Productions (T_Productions act) (Inh_Productions _lhsIaroundMap _lhsIinhMap _lhsImanualDeps _lhsImergeMap _lhsIoptions _lhsIrulenumber _lhsIsynMap) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Productions_vIn37 _lhsIaroundMap _lhsIinhMap _lhsImanualDeps _lhsImergeMap _lhsIoptions _lhsIrulenumber _lhsIsynMap
        (T_Productions_vOut37 _lhsOdepgraph _lhsOlocalSigMap _lhsOrefHoNts _lhsOrefNts _lhsOrulenumber) <- return (inv_Productions_s38 sem arg)
        return (Syn_Productions _lhsOdepgraph _lhsOlocalSigMap _lhsOrefHoNts _lhsOrefNts _lhsOrulenumber)
   )

-- cata
{-# NOINLINE sem_Productions #-}
sem_Productions :: Productions  -> T_Productions 
sem_Productions list = Prelude.foldr sem_Productions_Cons sem_Productions_Nil (Prelude.map sem_Production list)

-- semantic domain
newtype T_Productions  = T_Productions {
                                       attach_T_Productions :: Identity (T_Productions_s38 )
                                       }
newtype T_Productions_s38  = C_Productions_s38 {
                                               inv_Productions_s38 :: (T_Productions_v37 )
                                               }
data T_Productions_s39  = C_Productions_s39
type T_Productions_v37  = (T_Productions_vIn37 ) -> (T_Productions_vOut37 )
data T_Productions_vIn37  = T_Productions_vIn37 (Map ConstructorIdent (Map Identifier [Expression])) (Map Identifier Attributes) (Map ConstructorIdent (Set Dependency)) (Map ConstructorIdent (Map Identifier (Identifier, [Identifier], Expression))) (Options) (Int) (Map Identifier Attributes)
data T_Productions_vOut37  = T_Productions_vOut37 ([ProdDependencyGraph]) (Map.Map ConstructorIdent (Map.Map Identifier Type)) (Set NontermIdent) (Set NontermIdent) (Int)
{-# NOINLINE sem_Productions_Cons #-}
sem_Productions_Cons :: T_Production  -> T_Productions  -> T_Productions 
sem_Productions_Cons arg_hd_ arg_tl_ = T_Productions (return st38) where
   {-# NOINLINE st38 #-}
   st38 = let
      v37 :: T_Productions_v37 
      v37 = \ (T_Productions_vIn37 _lhsIaroundMap _lhsIinhMap _lhsImanualDeps _lhsImergeMap _lhsIoptions _lhsIrulenumber _lhsIsynMap) -> ( let
         _hdX35 = Control.Monad.Identity.runIdentity (attach_T_Production (arg_hd_))
         _tlX38 = Control.Monad.Identity.runIdentity (attach_T_Productions (arg_tl_))
         (T_Production_vOut34 _hdIdepgraph _hdIlocalSigMap _hdIrefHoNts _hdIrefNts _hdIrulenumber) = inv_Production_s35 _hdX35 (T_Production_vIn34 _hdOaroundMap _hdOinhMap _hdOmanualDeps _hdOmergeMap _hdOoptions _hdOrulenumber _hdOsynMap)
         (T_Productions_vOut37 _tlIdepgraph _tlIlocalSigMap _tlIrefHoNts _tlIrefNts _tlIrulenumber) = inv_Productions_s38 _tlX38 (T_Productions_vIn37 _tlOaroundMap _tlOinhMap _tlOmanualDeps _tlOmergeMap _tlOoptions _tlOrulenumber _tlOsynMap)
         _lhsOdepgraph :: [ProdDependencyGraph]
         _lhsOdepgraph = rule184 _hdIdepgraph _tlIdepgraph
         _lhsOlocalSigMap :: Map.Map ConstructorIdent (Map.Map Identifier Type)
         _lhsOlocalSigMap = rule185 _hdIlocalSigMap _tlIlocalSigMap
         _lhsOrefHoNts :: Set NontermIdent
         _lhsOrefHoNts = rule186 _hdIrefHoNts _tlIrefHoNts
         _lhsOrefNts :: Set NontermIdent
         _lhsOrefNts = rule187 _hdIrefNts _tlIrefNts
         _lhsOrulenumber :: Int
         _lhsOrulenumber = rule188 _tlIrulenumber
         _hdOaroundMap = rule189 _lhsIaroundMap
         _hdOinhMap = rule190 _lhsIinhMap
         _hdOmanualDeps = rule191 _lhsImanualDeps
         _hdOmergeMap = rule192 _lhsImergeMap
         _hdOoptions = rule193 _lhsIoptions
         _hdOrulenumber = rule194 _lhsIrulenumber
         _hdOsynMap = rule195 _lhsIsynMap
         _tlOaroundMap = rule196 _lhsIaroundMap
         _tlOinhMap = rule197 _lhsIinhMap
         _tlOmanualDeps = rule198 _lhsImanualDeps
         _tlOmergeMap = rule199 _lhsImergeMap
         _tlOoptions = rule200 _lhsIoptions
         _tlOrulenumber = rule201 _hdIrulenumber
         _tlOsynMap = rule202 _lhsIsynMap
         __result_ = T_Productions_vOut37 _lhsOdepgraph _lhsOlocalSigMap _lhsOrefHoNts _lhsOrefNts _lhsOrulenumber
         in __result_ )
     in C_Productions_s38 v37
   {-# INLINE rule184 #-}
   rule184 = \ ((_hdIdepgraph) :: ProdDependencyGraph) ((_tlIdepgraph) :: [ProdDependencyGraph]) ->
     _hdIdepgraph : _tlIdepgraph
   {-# INLINE rule185 #-}
   rule185 = \ ((_hdIlocalSigMap) :: Map.Map ConstructorIdent (Map.Map Identifier Type)) ((_tlIlocalSigMap) :: Map.Map ConstructorIdent (Map.Map Identifier Type)) ->
     _hdIlocalSigMap `Map.union` _tlIlocalSigMap
   {-# INLINE rule186 #-}
   rule186 = \ ((_hdIrefHoNts) :: Set NontermIdent) ((_tlIrefHoNts) :: Set NontermIdent) ->
     _hdIrefHoNts `mappend` _tlIrefHoNts
   {-# INLINE rule187 #-}
   rule187 = \ ((_hdIrefNts) :: Set NontermIdent) ((_tlIrefNts) :: Set NontermIdent) ->
     _hdIrefNts `mappend` _tlIrefNts
   {-# INLINE rule188 #-}
   rule188 = \ ((_tlIrulenumber) :: Int) ->
     _tlIrulenumber
   {-# INLINE rule189 #-}
   rule189 = \ ((_lhsIaroundMap) :: Map ConstructorIdent (Map Identifier [Expression])) ->
     _lhsIaroundMap
   {-# INLINE rule190 #-}
   rule190 = \ ((_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# INLINE rule191 #-}
   rule191 = \ ((_lhsImanualDeps) :: Map ConstructorIdent (Set Dependency)) ->
     _lhsImanualDeps
   {-# INLINE rule192 #-}
   rule192 = \ ((_lhsImergeMap) :: Map ConstructorIdent (Map Identifier (Identifier, [Identifier], Expression))) ->
     _lhsImergeMap
   {-# INLINE rule193 #-}
   rule193 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule194 #-}
   rule194 = \ ((_lhsIrulenumber) :: Int) ->
     _lhsIrulenumber
   {-# INLINE rule195 #-}
   rule195 = \ ((_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
   {-# INLINE rule196 #-}
   rule196 = \ ((_lhsIaroundMap) :: Map ConstructorIdent (Map Identifier [Expression])) ->
     _lhsIaroundMap
   {-# INLINE rule197 #-}
   rule197 = \ ((_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# INLINE rule198 #-}
   rule198 = \ ((_lhsImanualDeps) :: Map ConstructorIdent (Set Dependency)) ->
     _lhsImanualDeps
   {-# INLINE rule199 #-}
   rule199 = \ ((_lhsImergeMap) :: Map ConstructorIdent (Map Identifier (Identifier, [Identifier], Expression))) ->
     _lhsImergeMap
   {-# INLINE rule200 #-}
   rule200 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule201 #-}
   rule201 = \ ((_hdIrulenumber) :: Int) ->
     _hdIrulenumber
   {-# INLINE rule202 #-}
   rule202 = \ ((_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
{-# NOINLINE sem_Productions_Nil #-}
sem_Productions_Nil ::  T_Productions 
sem_Productions_Nil  = T_Productions (return st38) where
   {-# NOINLINE st38 #-}
   st38 = let
      v37 :: T_Productions_v37 
      v37 = \ (T_Productions_vIn37 _lhsIaroundMap _lhsIinhMap _lhsImanualDeps _lhsImergeMap _lhsIoptions _lhsIrulenumber _lhsIsynMap) -> ( let
         _lhsOdepgraph :: [ProdDependencyGraph]
         _lhsOdepgraph = rule203  ()
         _lhsOlocalSigMap :: Map.Map ConstructorIdent (Map.Map Identifier Type)
         _lhsOlocalSigMap = rule204  ()
         _lhsOrefHoNts :: Set NontermIdent
         _lhsOrefHoNts = rule205  ()
         _lhsOrefNts :: Set NontermIdent
         _lhsOrefNts = rule206  ()
         _lhsOrulenumber :: Int
         _lhsOrulenumber = rule207 _lhsIrulenumber
         __result_ = T_Productions_vOut37 _lhsOdepgraph _lhsOlocalSigMap _lhsOrefHoNts _lhsOrefNts _lhsOrulenumber
         in __result_ )
     in C_Productions_s38 v37
   {-# INLINE rule203 #-}
   rule203 = \  (_ :: ()) ->
     []
   {-# INLINE rule204 #-}
   rule204 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule205 #-}
   rule205 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule206 #-}
   rule206 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule207 #-}
   rule207 = \ ((_lhsIrulenumber) :: Int) ->
     _lhsIrulenumber

-- Rule --------------------------------------------------------
-- wrapper
data Inh_Rule  = Inh_Rule { rulenumber_Inh_Rule :: (Int) }
data Syn_Rule  = Syn_Rule { edges_Syn_Rule :: (Set.Set Edge), erules_Syn_Rule :: (ERule), rulenumber_Syn_Rule :: (Int), vertices_Syn_Rule :: (Set.Set Vertex) }
{-# INLINABLE wrap_Rule #-}
wrap_Rule :: T_Rule  -> Inh_Rule  -> (Syn_Rule )
wrap_Rule (T_Rule act) (Inh_Rule _lhsIrulenumber) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Rule_vIn40 _lhsIrulenumber
        (T_Rule_vOut40 _lhsOedges _lhsOerules _lhsOrulenumber _lhsOvertices) <- return (inv_Rule_s41 sem arg)
        return (Syn_Rule _lhsOedges _lhsOerules _lhsOrulenumber _lhsOvertices)
   )

-- cata
{-# INLINE sem_Rule #-}
sem_Rule :: Rule  -> T_Rule 
sem_Rule ( Rule mbName_ pattern_ rhs_ owrt_ origin_ explicit_ pure_ identity_ mbError_ eager_ ) = sem_Rule_Rule mbName_ ( sem_Pattern pattern_ ) ( sem_Expression rhs_ ) owrt_ origin_ explicit_ pure_ identity_ mbError_ eager_

-- semantic domain
newtype T_Rule  = T_Rule {
                         attach_T_Rule :: Identity (T_Rule_s41 )
                         }
newtype T_Rule_s41  = C_Rule_s41 {
                                 inv_Rule_s41 :: (T_Rule_v40 )
                                 }
data T_Rule_s42  = C_Rule_s42
type T_Rule_v40  = (T_Rule_vIn40 ) -> (T_Rule_vOut40 )
data T_Rule_vIn40  = T_Rule_vIn40 (Int)
data T_Rule_vOut40  = T_Rule_vOut40 (Set.Set Edge) (ERule) (Int) (Set.Set Vertex)
{-# NOINLINE sem_Rule_Rule #-}
sem_Rule_Rule :: (Maybe Identifier) -> T_Pattern  -> T_Expression  -> (Bool) -> (String) -> (Bool) -> (Bool) -> (Bool) -> (Maybe Error) -> (Bool) -> T_Rule 
sem_Rule_Rule arg_mbName_ arg_pattern_ arg_rhs_ arg_owrt_ arg_origin_ arg_explicit_ arg_pure_ _ arg_mbError_ _ = T_Rule (return st41) where
   {-# NOINLINE st41 #-}
   st41 = let
      v40 :: T_Rule_v40 
      v40 = \ (T_Rule_vIn40 _lhsIrulenumber) -> ( let
         _patternX29 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pattern_))
         _rhsX8 = Control.Monad.Identity.runIdentity (attach_T_Expression (arg_rhs_))
         (T_Pattern_vOut28 _patternIcopy _patternIvertices) = inv_Pattern_s29 _patternX29 (T_Pattern_vIn28 )
         (T_Expression_vOut7 _rhsIcopy _rhsIvertices) = inv_Expression_s8 _rhsX8 (T_Expression_vIn7 )
         _lhsOrulenumber :: Int
         _lhsOrulenumber = rule208 _lhsIrulenumber
         _rulename = rule209 _lhsIrulenumber arg_mbName_
         _lhsOerules :: ERule
         _lhsOerules = rule210 _patternIcopy _rhsIcopy _rulename arg_explicit_ arg_mbError_ arg_origin_ arg_owrt_ arg_pure_
         _vertex = rule211 _rulename
         _lhsOvertices :: Set.Set Vertex
         _lhsOvertices = rule212 _patternIvertices _rhsIvertices _vertex
         _edgesout = rule213 _rhsIvertices _vertex
         _edgesin = rule214 _patternIvertices _vertex
         _lhsOedges :: Set.Set Edge
         _lhsOedges = rule215 _edgesin _edgesout
         __result_ = T_Rule_vOut40 _lhsOedges _lhsOerules _lhsOrulenumber _lhsOvertices
         in __result_ )
     in C_Rule_s41 v40
   {-# INLINE rule208 #-}
   {-# LINE 47 "./src-ag/KWOrder.ag" #-}
   rule208 = \ ((_lhsIrulenumber) :: Int) ->
                             {-# LINE 47 "./src-ag/KWOrder.ag" #-}
                             _lhsIrulenumber + 1
                             {-# LINE 2040 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule209 #-}
   {-# LINE 48 "./src-ag/KWOrder.ag" #-}
   rule209 = \ ((_lhsIrulenumber) :: Int) mbName_ ->
                             {-# LINE 48 "./src-ag/KWOrder.ag" #-}
                             maybe (identifier $ "rule" ++ show _lhsIrulenumber) id mbName_
                             {-# LINE 2046 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule210 #-}
   {-# LINE 160 "./src-ag/KWOrder.ag" #-}
   rule210 = \ ((_patternIcopy) :: Pattern) ((_rhsIcopy) :: Expression) _rulename explicit_ mbError_ origin_ owrt_ pure_ ->
                        {-# LINE 160 "./src-ag/KWOrder.ag" #-}
                        ERule _rulename
                              _patternIcopy
                              _rhsIcopy
                              owrt_
                              origin_
                              explicit_
                              pure_
                              mbError_
                        {-# LINE 2059 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule211 #-}
   {-# LINE 224 "./src-ag/KWOrder.ag" #-}
   rule211 = \ _rulename ->
                           {-# LINE 224 "./src-ag/KWOrder.ag" #-}
                           VRule _rulename
                           {-# LINE 2065 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule212 #-}
   {-# LINE 225 "./src-ag/KWOrder.ag" #-}
   rule212 = \ ((_patternIvertices) :: Set.Set Vertex) ((_rhsIvertices) :: Set.Set Vertex) _vertex ->
                           {-# LINE 225 "./src-ag/KWOrder.ag" #-}
                           Set.insert _vertex     $ _patternIvertices `Set.union` _rhsIvertices
                           {-# LINE 2071 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule213 #-}
   {-# LINE 237 "./src-ag/KWOrder.ag" #-}
   rule213 = \ ((_rhsIvertices) :: Set.Set Vertex) _vertex ->
                           {-# LINE 237 "./src-ag/KWOrder.ag" #-}
                           map ((,) _vertex    ) (Set.toList _rhsIvertices)
                           {-# LINE 2077 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule214 #-}
   {-# LINE 238 "./src-ag/KWOrder.ag" #-}
   rule214 = \ ((_patternIvertices) :: Set.Set Vertex) _vertex ->
                           {-# LINE 238 "./src-ag/KWOrder.ag" #-}
                           map (flip (,) _vertex    ) (Set.toList _patternIvertices)
                           {-# LINE 2083 "dist/build/KWOrder.hs"#-}
   {-# INLINE rule215 #-}
   {-# LINE 239 "./src-ag/KWOrder.ag" #-}
   rule215 = \ _edgesin _edgesout ->
                           {-# LINE 239 "./src-ag/KWOrder.ag" #-}
                           Set.fromList $ _edgesout     ++ _edgesin
                           {-# LINE 2089 "dist/build/KWOrder.hs"#-}

-- Rules -------------------------------------------------------
-- wrapper
data Inh_Rules  = Inh_Rules { rulenumber_Inh_Rules :: (Int) }
data Syn_Rules  = Syn_Rules { edges_Syn_Rules :: (Set.Set Edge), erules_Syn_Rules :: (ERules), rulenumber_Syn_Rules :: (Int), vertices_Syn_Rules :: (Set.Set Vertex) }
{-# INLINABLE wrap_Rules #-}
wrap_Rules :: T_Rules  -> Inh_Rules  -> (Syn_Rules )
wrap_Rules (T_Rules act) (Inh_Rules _lhsIrulenumber) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Rules_vIn43 _lhsIrulenumber
        (T_Rules_vOut43 _lhsOedges _lhsOerules _lhsOrulenumber _lhsOvertices) <- return (inv_Rules_s44 sem arg)
        return (Syn_Rules _lhsOedges _lhsOerules _lhsOrulenumber _lhsOvertices)
   )

-- cata
{-# NOINLINE sem_Rules #-}
sem_Rules :: Rules  -> T_Rules 
sem_Rules list = Prelude.foldr sem_Rules_Cons sem_Rules_Nil (Prelude.map sem_Rule list)

-- semantic domain
newtype T_Rules  = T_Rules {
                           attach_T_Rules :: Identity (T_Rules_s44 )
                           }
newtype T_Rules_s44  = C_Rules_s44 {
                                   inv_Rules_s44 :: (T_Rules_v43 )
                                   }
data T_Rules_s45  = C_Rules_s45
type T_Rules_v43  = (T_Rules_vIn43 ) -> (T_Rules_vOut43 )
data T_Rules_vIn43  = T_Rules_vIn43 (Int)
data T_Rules_vOut43  = T_Rules_vOut43 (Set.Set Edge) (ERules) (Int) (Set.Set Vertex)
{-# NOINLINE sem_Rules_Cons #-}
sem_Rules_Cons :: T_Rule  -> T_Rules  -> T_Rules 
sem_Rules_Cons arg_hd_ arg_tl_ = T_Rules (return st44) where
   {-# NOINLINE st44 #-}
   st44 = let
      v43 :: T_Rules_v43 
      v43 = \ (T_Rules_vIn43 _lhsIrulenumber) -> ( let
         _hdX41 = Control.Monad.Identity.runIdentity (attach_T_Rule (arg_hd_))
         _tlX44 = Control.Monad.Identity.runIdentity (attach_T_Rules (arg_tl_))
         (T_Rule_vOut40 _hdIedges _hdIerules _hdIrulenumber _hdIvertices) = inv_Rule_s41 _hdX41 (T_Rule_vIn40 _hdOrulenumber)
         (T_Rules_vOut43 _tlIedges _tlIerules _tlIrulenumber _tlIvertices) = inv_Rules_s44 _tlX44 (T_Rules_vIn43 _tlOrulenumber)
         _lhsOedges :: Set.Set Edge
         _lhsOedges = rule216 _hdIedges _tlIedges
         _lhsOerules :: ERules
         _lhsOerules = rule217 _hdIerules _tlIerules
         _lhsOvertices :: Set.Set Vertex
         _lhsOvertices = rule218 _hdIvertices _tlIvertices
         _lhsOrulenumber :: Int
         _lhsOrulenumber = rule219 _tlIrulenumber
         _hdOrulenumber = rule220 _lhsIrulenumber
         _tlOrulenumber = rule221 _hdIrulenumber
         __result_ = T_Rules_vOut43 _lhsOedges _lhsOerules _lhsOrulenumber _lhsOvertices
         in __result_ )
     in C_Rules_s44 v43
   {-# INLINE rule216 #-}
   rule216 = \ ((_hdIedges) :: Set.Set Edge) ((_tlIedges) :: Set.Set Edge) ->
     _hdIedges `Set.union` _tlIedges
   {-# INLINE rule217 #-}
   rule217 = \ ((_hdIerules) :: ERule) ((_tlIerules) :: ERules) ->
     _hdIerules : _tlIerules
   {-# INLINE rule218 #-}
   rule218 = \ ((_hdIvertices) :: Set.Set Vertex) ((_tlIvertices) :: Set.Set Vertex) ->
     _hdIvertices `Set.union` _tlIvertices
   {-# INLINE rule219 #-}
   rule219 = \ ((_tlIrulenumber) :: Int) ->
     _tlIrulenumber
   {-# INLINE rule220 #-}
   rule220 = \ ((_lhsIrulenumber) :: Int) ->
     _lhsIrulenumber
   {-# INLINE rule221 #-}
   rule221 = \ ((_hdIrulenumber) :: Int) ->
     _hdIrulenumber
{-# NOINLINE sem_Rules_Nil #-}
sem_Rules_Nil ::  T_Rules 
sem_Rules_Nil  = T_Rules (return st44) where
   {-# NOINLINE st44 #-}
   st44 = let
      v43 :: T_Rules_v43 
      v43 = \ (T_Rules_vIn43 _lhsIrulenumber) -> ( let
         _lhsOedges :: Set.Set Edge
         _lhsOedges = rule222  ()
         _lhsOerules :: ERules
         _lhsOerules = rule223  ()
         _lhsOvertices :: Set.Set Vertex
         _lhsOvertices = rule224  ()
         _lhsOrulenumber :: Int
         _lhsOrulenumber = rule225 _lhsIrulenumber
         __result_ = T_Rules_vOut43 _lhsOedges _lhsOerules _lhsOrulenumber _lhsOvertices
         in __result_ )
     in C_Rules_s44 v43
   {-# INLINE rule222 #-}
   rule222 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule223 #-}
   rule223 = \  (_ :: ()) ->
     []
   {-# INLINE rule224 #-}
   rule224 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule225 #-}
   rule225 = \ ((_lhsIrulenumber) :: Int) ->
     _lhsIrulenumber

-- TypeSig -----------------------------------------------------
-- wrapper
data Inh_TypeSig  = Inh_TypeSig {  }
data Syn_TypeSig  = Syn_TypeSig { localSigMap_Syn_TypeSig :: (Map Identifier Type) }
{-# INLINABLE wrap_TypeSig #-}
wrap_TypeSig :: T_TypeSig  -> Inh_TypeSig  -> (Syn_TypeSig )
wrap_TypeSig (T_TypeSig act) (Inh_TypeSig ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_TypeSig_vIn46 
        (T_TypeSig_vOut46 _lhsOlocalSigMap) <- return (inv_TypeSig_s47 sem arg)
        return (Syn_TypeSig _lhsOlocalSigMap)
   )

-- cata
{-# INLINE sem_TypeSig #-}
sem_TypeSig :: TypeSig  -> T_TypeSig 
sem_TypeSig ( TypeSig name_ tp_ ) = sem_TypeSig_TypeSig name_ tp_

-- semantic domain
newtype T_TypeSig  = T_TypeSig {
                               attach_T_TypeSig :: Identity (T_TypeSig_s47 )
                               }
newtype T_TypeSig_s47  = C_TypeSig_s47 {
                                       inv_TypeSig_s47 :: (T_TypeSig_v46 )
                                       }
data T_TypeSig_s48  = C_TypeSig_s48
type T_TypeSig_v46  = (T_TypeSig_vIn46 ) -> (T_TypeSig_vOut46 )
data T_TypeSig_vIn46  = T_TypeSig_vIn46 
data T_TypeSig_vOut46  = T_TypeSig_vOut46 (Map Identifier Type)
{-# NOINLINE sem_TypeSig_TypeSig #-}
sem_TypeSig_TypeSig :: (Identifier) -> (Type) -> T_TypeSig 
sem_TypeSig_TypeSig arg_name_ arg_tp_ = T_TypeSig (return st47) where
   {-# NOINLINE st47 #-}
   st47 = let
      v46 :: T_TypeSig_v46 
      v46 = \ (T_TypeSig_vIn46 ) -> ( let
         _lhsOlocalSigMap :: Map Identifier Type
         _lhsOlocalSigMap = rule226 arg_name_ arg_tp_
         __result_ = T_TypeSig_vOut46 _lhsOlocalSigMap
         in __result_ )
     in C_TypeSig_s47 v46
   {-# INLINE rule226 #-}
   {-# LINE 389 "./src-ag/KWOrder.ag" #-}
   rule226 = \ name_ tp_ ->
                                                   {-# LINE 389 "./src-ag/KWOrder.ag" #-}
                                                   Map.singleton name_ tp_
                                                   {-# LINE 2241 "dist/build/KWOrder.hs"#-}

-- TypeSigs ----------------------------------------------------
-- wrapper
data Inh_TypeSigs  = Inh_TypeSigs {  }
data Syn_TypeSigs  = Syn_TypeSigs { localSigMap_Syn_TypeSigs :: (Map Identifier Type) }
{-# INLINABLE wrap_TypeSigs #-}
wrap_TypeSigs :: T_TypeSigs  -> Inh_TypeSigs  -> (Syn_TypeSigs )
wrap_TypeSigs (T_TypeSigs act) (Inh_TypeSigs ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_TypeSigs_vIn49 
        (T_TypeSigs_vOut49 _lhsOlocalSigMap) <- return (inv_TypeSigs_s50 sem arg)
        return (Syn_TypeSigs _lhsOlocalSigMap)
   )

-- cata
{-# NOINLINE sem_TypeSigs #-}
sem_TypeSigs :: TypeSigs  -> T_TypeSigs 
sem_TypeSigs list = Prelude.foldr sem_TypeSigs_Cons sem_TypeSigs_Nil (Prelude.map sem_TypeSig list)

-- semantic domain
newtype T_TypeSigs  = T_TypeSigs {
                                 attach_T_TypeSigs :: Identity (T_TypeSigs_s50 )
                                 }
newtype T_TypeSigs_s50  = C_TypeSigs_s50 {
                                         inv_TypeSigs_s50 :: (T_TypeSigs_v49 )
                                         }
data T_TypeSigs_s51  = C_TypeSigs_s51
type T_TypeSigs_v49  = (T_TypeSigs_vIn49 ) -> (T_TypeSigs_vOut49 )
data T_TypeSigs_vIn49  = T_TypeSigs_vIn49 
data T_TypeSigs_vOut49  = T_TypeSigs_vOut49 (Map Identifier Type)
{-# NOINLINE sem_TypeSigs_Cons #-}
sem_TypeSigs_Cons :: T_TypeSig  -> T_TypeSigs  -> T_TypeSigs 
sem_TypeSigs_Cons arg_hd_ arg_tl_ = T_TypeSigs (return st50) where
   {-# NOINLINE st50 #-}
   st50 = let
      v49 :: T_TypeSigs_v49 
      v49 = \ (T_TypeSigs_vIn49 ) -> ( let
         _hdX47 = Control.Monad.Identity.runIdentity (attach_T_TypeSig (arg_hd_))
         _tlX50 = Control.Monad.Identity.runIdentity (attach_T_TypeSigs (arg_tl_))
         (T_TypeSig_vOut46 _hdIlocalSigMap) = inv_TypeSig_s47 _hdX47 (T_TypeSig_vIn46 )
         (T_TypeSigs_vOut49 _tlIlocalSigMap) = inv_TypeSigs_s50 _tlX50 (T_TypeSigs_vIn49 )
         _lhsOlocalSigMap :: Map Identifier Type
         _lhsOlocalSigMap = rule227 _hdIlocalSigMap _tlIlocalSigMap
         __result_ = T_TypeSigs_vOut49 _lhsOlocalSigMap
         in __result_ )
     in C_TypeSigs_s50 v49
   {-# INLINE rule227 #-}
   rule227 = \ ((_hdIlocalSigMap) :: Map Identifier Type) ((_tlIlocalSigMap) :: Map Identifier Type) ->
     _hdIlocalSigMap `Map.union` _tlIlocalSigMap
{-# NOINLINE sem_TypeSigs_Nil #-}
sem_TypeSigs_Nil ::  T_TypeSigs 
sem_TypeSigs_Nil  = T_TypeSigs (return st50) where
   {-# NOINLINE st50 #-}
   st50 = let
      v49 :: T_TypeSigs_v49 
      v49 = \ (T_TypeSigs_vIn49 ) -> ( let
         _lhsOlocalSigMap :: Map Identifier Type
         _lhsOlocalSigMap = rule228  ()
         __result_ = T_TypeSigs_vOut49 _lhsOlocalSigMap
         in __result_ )
     in C_TypeSigs_s50 v49
   {-# INLINE rule228 #-}
   rule228 = \  (_ :: ()) ->
     Map.empty
