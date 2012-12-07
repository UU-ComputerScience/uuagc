{-# LANGUAGE Rank2Types, GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TfmToVisage where
{-# LINE 9 "./src-ag/TfmToVisage.ag" #-}

import AbstractSyntax
import VisagePatterns
import VisageSyntax
import qualified Data.Map as Map
import Data.Map (Map)
{-# LINE 13 "dist/build/TfmToVisage.hs" #-}

{-# LINE 2 "./src-ag/AbstractSyntax.ag" #-}

-- AbstractSyntax.ag imports
import Data.Set(Set)
import Data.Map(Map)
import Patterns    (Pattern(..),Patterns)
import Expression  (Expression(..))
import Macro --marcos
import CommonTypes
import ErrorMessages
{-# LINE 25 "dist/build/TfmToVisage.hs" #-}

{-# LINE 2 "./src-ag/Patterns.ag" #-}

-- Patterns.ag imports
import UU.Scanner.Position(Pos)
import CommonTypes (ConstructorIdent,Identifier)
{-# LINE 32 "dist/build/TfmToVisage.hs" #-}

{-# LINE 2 "./src-ag/Expression.ag" #-}

import UU.Scanner.Position(Pos)
import HsToken
{-# LINE 38 "dist/build/TfmToVisage.hs" #-}
import Control.Monad.Identity (Identity)
import qualified Control.Monad.Identity
{-# LINE 17 "./src-ag/TfmToVisage.ag" #-}

-- Maps a rule to a pair
-- Later, I expect to map to a list of rules, because we might need to unfold.


-- Checks that a certain alias is in fact a Var in the old representation of the AG system
isVar (Alias _ _ (Underscore _)) = True
isVar _ = False

type VisageRuleMap = [(String, VisageRule)]

splitVRules :: [VisageRule] -> VisageRuleMap
splitVRules vrs = concat (map unfoldvrs vrs)

unfoldvrs :: VisageRule -> VisageRuleMap
unfoldvrs vr@(VRule attrfields _ _ _ _) = zip (map (getName . fst) attrfields) (map (copyRule vr) attrfields)

copyRule :: VisageRule -> (Identifier,Identifier) -> VisageRule
copyRule (VRule attrfields _ pat expr owrt) (field,attr) = VRule attrfields attr pat expr owrt

getForField :: String -> VisageRuleMap -> [VisageRule]
getForField field xs = map snd (filter ((field ==) . fst) xs)

{-
   Delivers a map from fieldname to VisageRule with all references to others underscored.
   So, (lhs.x, rt.y, loc.z) = (0,1,2) becomes something like
   [("lhs", (lhs.x,_,_) = (0,1,2)

   At this point, we do not use this anymore.

allways :: VisageRule -> VisageRuleMap
allways vr@(VRule vrfields _ _ _ _) = zip vrfields (map (underScoreRule vr) (nub vrfields))

splitVRules :: [VisageRule] -> VisageRuleMap
splitVRules vrs = concat (map allways vrs)

underScoreRule :: VisageRule -> String -> VisageRule
underScoreRule (VRule fields pat expr owrt rule) s = VRule fields (underScore s pat) expr owrt rule

underScore :: String -> VisagePattern -> VisagePattern
underScore field (VConstr name pats) = VConstr name (map (underScore field) pats)
underScore field (VProduct pos pats) = VProduct pos (map (underScore field) pats)
underScore field vp@(VVar vfield attr)  =
   if (field == getName vfield)
   then vp
   else (VUnderscore (getPos vfield))
-- Should I recurse into the pat of VAlias?
underScore field vp@(VAlias afield attr pat) =
   if (field == getName afield)
   then vp
   else (VUnderscore (getPos afield))
underScore field vp@(VUnderscore pos) = vp

-}
{-# LINE 96 "dist/build/TfmToVisage.hs" #-}
-- Child -------------------------------------------------------
-- wrapper
data Inh_Child  = Inh_Child { inhMap_Inh_Child :: (Map Identifier Attributes), rulemap_Inh_Child :: (VisageRuleMap), synMap_Inh_Child :: (Map Identifier Attributes) }
data Syn_Child  = Syn_Child { vchild_Syn_Child :: (VisageChild) }
{-# INLINABLE wrap_Child #-}
wrap_Child :: T_Child  -> Inh_Child  -> (Syn_Child )
wrap_Child (T_Child act) (Inh_Child _lhsIinhMap _lhsIrulemap _lhsIsynMap) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Child_vIn1 _lhsIinhMap _lhsIrulemap _lhsIsynMap
        (T_Child_vOut1 _lhsOvchild) <- return (inv_Child_s2 sem arg)
        return (Syn_Child _lhsOvchild)
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
data T_Child_vIn1  = T_Child_vIn1 (Map Identifier Attributes) (VisageRuleMap) (Map Identifier Attributes)
data T_Child_vOut1  = T_Child_vOut1 (VisageChild)
{-# NOINLINE sem_Child_Child #-}
sem_Child_Child :: (Identifier) -> (Type) -> (ChildKind) -> T_Child 
sem_Child_Child arg_name_ arg_tp_ _ = T_Child (return st2) where
   {-# NOINLINE st2 #-}
   st2 = let
      v1 :: T_Child_v1 
      v1 = \ (T_Child_vIn1 _lhsIinhMap _lhsIrulemap _lhsIsynMap) -> ( let
         _lhsOvchild :: VisageChild
         _lhsOvchild = rule0 _inh _lhsIrulemap _syn arg_name_ arg_tp_
         _chnt = rule1 arg_name_ arg_tp_
         _inh = rule2 _chnt _lhsIinhMap
         _syn = rule3 _chnt _lhsIsynMap
         __result_ = T_Child_vOut1 _lhsOvchild
         in __result_ )
     in C_Child_s2 v1
   {-# INLINE rule0 #-}
   {-# LINE 121 "./src-ag/TfmToVisage.ag" #-}
   rule0 = \ _inh ((_lhsIrulemap) :: VisageRuleMap) _syn name_ tp_ ->
                         {-# LINE 121 "./src-ag/TfmToVisage.ag" #-}
                         VChild name_ tp_ _inh     _syn     (getForField (getName name_) _lhsIrulemap)
                         {-# LINE 147 "dist/build/TfmToVisage.hs"#-}
   {-# INLINE rule1 #-}
   {-# LINE 19 "./src-ag/DistChildAttr.ag" #-}
   rule1 = \ name_ tp_ ->
                       {-# LINE 19 "./src-ag/DistChildAttr.ag" #-}
                       case tp_ of
                         NT nt _ _ -> nt
                         Self      -> error ("The type of child " ++ show name_ ++ " should not be a Self type.")
                         Haskell t -> identifier ""
                       {-# LINE 156 "dist/build/TfmToVisage.hs"#-}
   {-# INLINE rule2 #-}
   {-# LINE 23 "./src-ag/DistChildAttr.ag" #-}
   rule2 = \ _chnt ((_lhsIinhMap) :: Map Identifier Attributes) ->
                      {-# LINE 23 "./src-ag/DistChildAttr.ag" #-}
                      Map.findWithDefault Map.empty _chnt     _lhsIinhMap
                      {-# LINE 162 "dist/build/TfmToVisage.hs"#-}
   {-# INLINE rule3 #-}
   {-# LINE 24 "./src-ag/DistChildAttr.ag" #-}
   rule3 = \ _chnt ((_lhsIsynMap) :: Map Identifier Attributes) ->
                      {-# LINE 24 "./src-ag/DistChildAttr.ag" #-}
                      Map.findWithDefault Map.empty _chnt     _lhsIsynMap
                      {-# LINE 168 "dist/build/TfmToVisage.hs"#-}

-- Children ----------------------------------------------------
-- wrapper
data Inh_Children  = Inh_Children { inhMap_Inh_Children :: (Map Identifier Attributes), rulemap_Inh_Children :: (VisageRuleMap), synMap_Inh_Children :: (Map Identifier Attributes) }
data Syn_Children  = Syn_Children { vchildren_Syn_Children :: ([VisageChild]) }
{-# INLINABLE wrap_Children #-}
wrap_Children :: T_Children  -> Inh_Children  -> (Syn_Children )
wrap_Children (T_Children act) (Inh_Children _lhsIinhMap _lhsIrulemap _lhsIsynMap) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Children_vIn4 _lhsIinhMap _lhsIrulemap _lhsIsynMap
        (T_Children_vOut4 _lhsOvchildren) <- return (inv_Children_s5 sem arg)
        return (Syn_Children _lhsOvchildren)
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
data T_Children_vIn4  = T_Children_vIn4 (Map Identifier Attributes) (VisageRuleMap) (Map Identifier Attributes)
data T_Children_vOut4  = T_Children_vOut4 ([VisageChild])
{-# NOINLINE sem_Children_Cons #-}
sem_Children_Cons :: T_Child  -> T_Children  -> T_Children 
sem_Children_Cons arg_hd_ arg_tl_ = T_Children (return st5) where
   {-# NOINLINE st5 #-}
   st5 = let
      v4 :: T_Children_v4 
      v4 = \ (T_Children_vIn4 _lhsIinhMap _lhsIrulemap _lhsIsynMap) -> ( let
         _hdX2 = Control.Monad.Identity.runIdentity (attach_T_Child (arg_hd_))
         _tlX5 = Control.Monad.Identity.runIdentity (attach_T_Children (arg_tl_))
         (T_Child_vOut1 _hdIvchild) = inv_Child_s2 _hdX2 (T_Child_vIn1 _hdOinhMap _hdOrulemap _hdOsynMap)
         (T_Children_vOut4 _tlIvchildren) = inv_Children_s5 _tlX5 (T_Children_vIn4 _tlOinhMap _tlOrulemap _tlOsynMap)
         _lhsOvchildren :: [VisageChild]
         _lhsOvchildren = rule4 _hdIvchild _tlIvchildren
         _hdOinhMap = rule5 _lhsIinhMap
         _hdOrulemap = rule6 _lhsIrulemap
         _hdOsynMap = rule7 _lhsIsynMap
         _tlOinhMap = rule8 _lhsIinhMap
         _tlOrulemap = rule9 _lhsIrulemap
         _tlOsynMap = rule10 _lhsIsynMap
         __result_ = T_Children_vOut4 _lhsOvchildren
         in __result_ )
     in C_Children_s5 v4
   {-# INLINE rule4 #-}
   {-# LINE 117 "./src-ag/TfmToVisage.ag" #-}
   rule4 = \ ((_hdIvchild) :: VisageChild) ((_tlIvchildren) :: [VisageChild]) ->
                                  {-# LINE 117 "./src-ag/TfmToVisage.ag" #-}
                                  _hdIvchild : _tlIvchildren
                                  {-# LINE 227 "dist/build/TfmToVisage.hs"#-}
   {-# INLINE rule5 #-}
   rule5 = \ ((_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# INLINE rule6 #-}
   rule6 = \ ((_lhsIrulemap) :: VisageRuleMap) ->
     _lhsIrulemap
   {-# INLINE rule7 #-}
   rule7 = \ ((_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
   {-# INLINE rule8 #-}
   rule8 = \ ((_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# INLINE rule9 #-}
   rule9 = \ ((_lhsIrulemap) :: VisageRuleMap) ->
     _lhsIrulemap
   {-# INLINE rule10 #-}
   rule10 = \ ((_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
{-# NOINLINE sem_Children_Nil #-}
sem_Children_Nil ::  T_Children 
sem_Children_Nil  = T_Children (return st5) where
   {-# NOINLINE st5 #-}
   st5 = let
      v4 :: T_Children_v4 
      v4 = \ (T_Children_vIn4 _lhsIinhMap _lhsIrulemap _lhsIsynMap) -> ( let
         _lhsOvchildren :: [VisageChild]
         _lhsOvchildren = rule11  ()
         __result_ = T_Children_vOut4 _lhsOvchildren
         in __result_ )
     in C_Children_s5 v4
   {-# INLINE rule11 #-}
   {-# LINE 118 "./src-ag/TfmToVisage.ag" #-}
   rule11 = \  (_ :: ()) ->
                                  {-# LINE 118 "./src-ag/TfmToVisage.ag" #-}
                                  []
                                  {-# LINE 263 "dist/build/TfmToVisage.hs"#-}

-- Expression --------------------------------------------------
-- wrapper
data Inh_Expression  = Inh_Expression {  }
data Syn_Expression  = Syn_Expression { self_Syn_Expression :: (Expression) }
{-# INLINABLE wrap_Expression #-}
wrap_Expression :: T_Expression  -> Inh_Expression  -> (Syn_Expression )
wrap_Expression (T_Expression act) (Inh_Expression ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Expression_vIn7 
        (T_Expression_vOut7 _lhsOself) <- return (inv_Expression_s8 sem arg)
        return (Syn_Expression _lhsOself)
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
data T_Expression_vOut7  = T_Expression_vOut7 (Expression)
{-# NOINLINE sem_Expression_Expression #-}
sem_Expression_Expression :: (Pos) -> ([HsToken]) -> T_Expression 
sem_Expression_Expression arg_pos_ arg_tks_ = T_Expression (return st8) where
   {-# NOINLINE st8 #-}
   st8 = let
      v7 :: T_Expression_v7 
      v7 = \ (T_Expression_vIn7 ) -> ( let
         _self = rule12 arg_pos_ arg_tks_
         _lhsOself :: Expression
         _lhsOself = rule13 _self
         __result_ = T_Expression_vOut7 _lhsOself
         in __result_ )
     in C_Expression_s8 v7
   {-# INLINE rule12 #-}
   rule12 = \ pos_ tks_ ->
     Expression pos_ tks_
   {-# INLINE rule13 #-}
   rule13 = \ _self ->
     _self

-- Grammar -----------------------------------------------------
-- wrapper
data Inh_Grammar  = Inh_Grammar {  }
data Syn_Grammar  = Syn_Grammar { visage_Syn_Grammar :: (VisageGrammar) }
{-# INLINABLE wrap_Grammar #-}
wrap_Grammar :: T_Grammar  -> Inh_Grammar  -> (Syn_Grammar )
wrap_Grammar (T_Grammar act) (Inh_Grammar ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Grammar_vIn10 
        (T_Grammar_vOut10 _lhsOvisage) <- return (inv_Grammar_s11 sem arg)
        return (Syn_Grammar _lhsOvisage)
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
data T_Grammar_vIn10  = T_Grammar_vIn10 
data T_Grammar_vOut10  = T_Grammar_vOut10 (VisageGrammar)
{-# NOINLINE sem_Grammar_Grammar #-}
sem_Grammar_Grammar :: (TypeSyns) -> (UseMap) -> (Derivings) -> (Set NontermIdent) -> T_Nonterminals  -> (PragmaMap) -> (AttrOrderMap) -> (ParamMap) -> (ContextMap) -> (QuantMap) -> (UniqueMap) -> (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) -> (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) -> (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier, [Identifier], Expression)))) -> T_Grammar 
sem_Grammar_Grammar _ _ _ _ arg_nonts_ _ _ _ _ _ _ _ _ _ = T_Grammar (return st11) where
   {-# NOINLINE st11 #-}
   st11 = let
      v10 :: T_Grammar_v10 
      v10 = \ (T_Grammar_vIn10 ) -> ( let
         _nontsX17 = Control.Monad.Identity.runIdentity (attach_T_Nonterminals (arg_nonts_))
         (T_Nonterminals_vOut16 _nontsIinhMap' _nontsIsynMap' _nontsIvnonts) = inv_Nonterminals_s17 _nontsX17 (T_Nonterminals_vIn16 _nontsOinhMap _nontsOsynMap)
         _lhsOvisage :: VisageGrammar
         _lhsOvisage = rule14 _nontsIvnonts
         _nontsOinhMap = rule15 _nontsIinhMap'
         _nontsOsynMap = rule16 _nontsIsynMap'
         __result_ = T_Grammar_vOut10 _lhsOvisage
         in __result_ )
     in C_Grammar_s11 v10
   {-# INLINE rule14 #-}
   {-# LINE 90 "./src-ag/TfmToVisage.ag" #-}
   rule14 = \ ((_nontsIvnonts) :: [VisageNonterminal]) ->
                     {-# LINE 90 "./src-ag/TfmToVisage.ag" #-}
                     VGrammar _nontsIvnonts
                     {-# LINE 366 "dist/build/TfmToVisage.hs"#-}
   {-# INLINE rule15 #-}
   {-# LINE 15 "./src-ag/DistChildAttr.ag" #-}
   rule15 = \ ((_nontsIinhMap') :: Map Identifier Attributes) ->
                             {-# LINE 15 "./src-ag/DistChildAttr.ag" #-}
                             _nontsIinhMap'
                             {-# LINE 372 "dist/build/TfmToVisage.hs"#-}
   {-# INLINE rule16 #-}
   {-# LINE 16 "./src-ag/DistChildAttr.ag" #-}
   rule16 = \ ((_nontsIsynMap') :: Map Identifier Attributes) ->
                             {-# LINE 16 "./src-ag/DistChildAttr.ag" #-}
                             _nontsIsynMap'
                             {-# LINE 378 "dist/build/TfmToVisage.hs"#-}

-- Nonterminal -------------------------------------------------
-- wrapper
data Inh_Nonterminal  = Inh_Nonterminal { inhMap_Inh_Nonterminal :: (Map Identifier Attributes), synMap_Inh_Nonterminal :: (Map Identifier Attributes) }
data Syn_Nonterminal  = Syn_Nonterminal { inhMap'_Syn_Nonterminal :: (Map Identifier Attributes), synMap'_Syn_Nonterminal :: (Map Identifier Attributes), vnont_Syn_Nonterminal :: (VisageNonterminal) }
{-# INLINABLE wrap_Nonterminal #-}
wrap_Nonterminal :: T_Nonterminal  -> Inh_Nonterminal  -> (Syn_Nonterminal )
wrap_Nonterminal (T_Nonterminal act) (Inh_Nonterminal _lhsIinhMap _lhsIsynMap) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Nonterminal_vIn13 _lhsIinhMap _lhsIsynMap
        (T_Nonterminal_vOut13 _lhsOinhMap' _lhsOsynMap' _lhsOvnont) <- return (inv_Nonterminal_s14 sem arg)
        return (Syn_Nonterminal _lhsOinhMap' _lhsOsynMap' _lhsOvnont)
   )

-- cata
{-# INLINE sem_Nonterminal #-}
sem_Nonterminal :: Nonterminal  -> T_Nonterminal 
sem_Nonterminal ( Nonterminal nt_ params_ inh_ syn_ prods_ ) = sem_Nonterminal_Nonterminal nt_ params_ inh_ syn_ ( sem_Productions prods_ )

-- semantic domain
newtype T_Nonterminal  = T_Nonterminal {
                                       attach_T_Nonterminal :: Identity (T_Nonterminal_s14 )
                                       }
newtype T_Nonterminal_s14  = C_Nonterminal_s14 {
                                               inv_Nonterminal_s14 :: (T_Nonterminal_v13 )
                                               }
data T_Nonterminal_s15  = C_Nonterminal_s15
type T_Nonterminal_v13  = (T_Nonterminal_vIn13 ) -> (T_Nonterminal_vOut13 )
data T_Nonterminal_vIn13  = T_Nonterminal_vIn13 (Map Identifier Attributes) (Map Identifier Attributes)
data T_Nonterminal_vOut13  = T_Nonterminal_vOut13 (Map Identifier Attributes) (Map Identifier Attributes) (VisageNonterminal)
{-# NOINLINE sem_Nonterminal_Nonterminal #-}
sem_Nonterminal_Nonterminal :: (NontermIdent) -> ([Identifier]) -> (Attributes) -> (Attributes) -> T_Productions  -> T_Nonterminal 
sem_Nonterminal_Nonterminal arg_nt_ _ arg_inh_ arg_syn_ arg_prods_ = T_Nonterminal (return st14) where
   {-# NOINLINE st14 #-}
   st14 = let
      v13 :: T_Nonterminal_v13 
      v13 = \ (T_Nonterminal_vIn13 _lhsIinhMap _lhsIsynMap) -> ( let
         _prodsX29 = Control.Monad.Identity.runIdentity (attach_T_Productions (arg_prods_))
         (T_Productions_vOut28 _prodsIvprods) = inv_Productions_s29 _prodsX29 (T_Productions_vIn28 _prodsOinhMap _prodsOsynMap)
         _lhsOvnont :: VisageNonterminal
         _lhsOvnont = rule17 _prodsIvprods arg_inh_ arg_nt_ arg_syn_
         _lhsOinhMap' :: Map Identifier Attributes
         _lhsOinhMap' = rule18 arg_inh_ arg_nt_
         _lhsOsynMap' :: Map Identifier Attributes
         _lhsOsynMap' = rule19 arg_nt_ arg_syn_
         _prodsOinhMap = rule20 _lhsIinhMap
         _prodsOsynMap = rule21 _lhsIsynMap
         __result_ = T_Nonterminal_vOut13 _lhsOinhMap' _lhsOsynMap' _lhsOvnont
         in __result_ )
     in C_Nonterminal_s14 v13
   {-# INLINE rule17 #-}
   {-# LINE 100 "./src-ag/TfmToVisage.ag" #-}
   rule17 = \ ((_prodsIvprods) :: [VisageProduction]) inh_ nt_ syn_ ->
                    {-# LINE 100 "./src-ag/TfmToVisage.ag" #-}
                    VNonterminal nt_ inh_ syn_ _prodsIvprods
                    {-# LINE 435 "dist/build/TfmToVisage.hs"#-}
   {-# INLINE rule18 #-}
   {-# LINE 7 "./src-ag/DistChildAttr.ag" #-}
   rule18 = \ inh_ nt_ ->
                                 {-# LINE 7 "./src-ag/DistChildAttr.ag" #-}
                                 Map.singleton nt_ inh_
                                 {-# LINE 441 "dist/build/TfmToVisage.hs"#-}
   {-# INLINE rule19 #-}
   {-# LINE 8 "./src-ag/DistChildAttr.ag" #-}
   rule19 = \ nt_ syn_ ->
                                 {-# LINE 8 "./src-ag/DistChildAttr.ag" #-}
                                 Map.singleton nt_ syn_
                                 {-# LINE 447 "dist/build/TfmToVisage.hs"#-}
   {-# INLINE rule20 #-}
   rule20 = \ ((_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# INLINE rule21 #-}
   rule21 = \ ((_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap

-- Nonterminals ------------------------------------------------
-- wrapper
data Inh_Nonterminals  = Inh_Nonterminals { inhMap_Inh_Nonterminals :: (Map Identifier Attributes), synMap_Inh_Nonterminals :: (Map Identifier Attributes) }
data Syn_Nonterminals  = Syn_Nonterminals { inhMap'_Syn_Nonterminals :: (Map Identifier Attributes), synMap'_Syn_Nonterminals :: (Map Identifier Attributes), vnonts_Syn_Nonterminals :: ([VisageNonterminal]) }
{-# INLINABLE wrap_Nonterminals #-}
wrap_Nonterminals :: T_Nonterminals  -> Inh_Nonterminals  -> (Syn_Nonterminals )
wrap_Nonterminals (T_Nonterminals act) (Inh_Nonterminals _lhsIinhMap _lhsIsynMap) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Nonterminals_vIn16 _lhsIinhMap _lhsIsynMap
        (T_Nonterminals_vOut16 _lhsOinhMap' _lhsOsynMap' _lhsOvnonts) <- return (inv_Nonterminals_s17 sem arg)
        return (Syn_Nonterminals _lhsOinhMap' _lhsOsynMap' _lhsOvnonts)
   )

-- cata
{-# NOINLINE sem_Nonterminals #-}
sem_Nonterminals :: Nonterminals  -> T_Nonterminals 
sem_Nonterminals list = Prelude.foldr sem_Nonterminals_Cons sem_Nonterminals_Nil (Prelude.map sem_Nonterminal list)

-- semantic domain
newtype T_Nonterminals  = T_Nonterminals {
                                         attach_T_Nonterminals :: Identity (T_Nonterminals_s17 )
                                         }
newtype T_Nonterminals_s17  = C_Nonterminals_s17 {
                                                 inv_Nonterminals_s17 :: (T_Nonterminals_v16 )
                                                 }
data T_Nonterminals_s18  = C_Nonterminals_s18
type T_Nonterminals_v16  = (T_Nonterminals_vIn16 ) -> (T_Nonterminals_vOut16 )
data T_Nonterminals_vIn16  = T_Nonterminals_vIn16 (Map Identifier Attributes) (Map Identifier Attributes)
data T_Nonterminals_vOut16  = T_Nonterminals_vOut16 (Map Identifier Attributes) (Map Identifier Attributes) ([VisageNonterminal])
{-# NOINLINE sem_Nonterminals_Cons #-}
sem_Nonterminals_Cons :: T_Nonterminal  -> T_Nonterminals  -> T_Nonterminals 
sem_Nonterminals_Cons arg_hd_ arg_tl_ = T_Nonterminals (return st17) where
   {-# NOINLINE st17 #-}
   st17 = let
      v16 :: T_Nonterminals_v16 
      v16 = \ (T_Nonterminals_vIn16 _lhsIinhMap _lhsIsynMap) -> ( let
         _hdX14 = Control.Monad.Identity.runIdentity (attach_T_Nonterminal (arg_hd_))
         _tlX17 = Control.Monad.Identity.runIdentity (attach_T_Nonterminals (arg_tl_))
         (T_Nonterminal_vOut13 _hdIinhMap' _hdIsynMap' _hdIvnont) = inv_Nonterminal_s14 _hdX14 (T_Nonterminal_vIn13 _hdOinhMap _hdOsynMap)
         (T_Nonterminals_vOut16 _tlIinhMap' _tlIsynMap' _tlIvnonts) = inv_Nonterminals_s17 _tlX17 (T_Nonterminals_vIn16 _tlOinhMap _tlOsynMap)
         _lhsOvnonts :: [VisageNonterminal]
         _lhsOvnonts = rule22 _hdIvnont _tlIvnonts
         _lhsOinhMap' :: Map Identifier Attributes
         _lhsOinhMap' = rule23 _hdIinhMap' _tlIinhMap'
         _lhsOsynMap' :: Map Identifier Attributes
         _lhsOsynMap' = rule24 _hdIsynMap' _tlIsynMap'
         _hdOinhMap = rule25 _lhsIinhMap
         _hdOsynMap = rule26 _lhsIsynMap
         _tlOinhMap = rule27 _lhsIinhMap
         _tlOsynMap = rule28 _lhsIsynMap
         __result_ = T_Nonterminals_vOut16 _lhsOinhMap' _lhsOsynMap' _lhsOvnonts
         in __result_ )
     in C_Nonterminals_s17 v16
   {-# INLINE rule22 #-}
   {-# LINE 94 "./src-ag/TfmToVisage.ag" #-}
   rule22 = \ ((_hdIvnont) :: VisageNonterminal) ((_tlIvnonts) :: [VisageNonterminal]) ->
                     {-# LINE 94 "./src-ag/TfmToVisage.ag" #-}
                     _hdIvnont : _tlIvnonts
                     {-# LINE 514 "dist/build/TfmToVisage.hs"#-}
   {-# INLINE rule23 #-}
   rule23 = \ ((_hdIinhMap') :: Map Identifier Attributes) ((_tlIinhMap') :: Map Identifier Attributes) ->
     _hdIinhMap' `Map.union` _tlIinhMap'
   {-# INLINE rule24 #-}
   rule24 = \ ((_hdIsynMap') :: Map Identifier Attributes) ((_tlIsynMap') :: Map Identifier Attributes) ->
     _hdIsynMap' `Map.union` _tlIsynMap'
   {-# INLINE rule25 #-}
   rule25 = \ ((_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# INLINE rule26 #-}
   rule26 = \ ((_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
   {-# INLINE rule27 #-}
   rule27 = \ ((_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# INLINE rule28 #-}
   rule28 = \ ((_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
{-# NOINLINE sem_Nonterminals_Nil #-}
sem_Nonterminals_Nil ::  T_Nonterminals 
sem_Nonterminals_Nil  = T_Nonterminals (return st17) where
   {-# NOINLINE st17 #-}
   st17 = let
      v16 :: T_Nonterminals_v16 
      v16 = \ (T_Nonterminals_vIn16 _lhsIinhMap _lhsIsynMap) -> ( let
         _lhsOvnonts :: [VisageNonterminal]
         _lhsOvnonts = rule29  ()
         _lhsOinhMap' :: Map Identifier Attributes
         _lhsOinhMap' = rule30  ()
         _lhsOsynMap' :: Map Identifier Attributes
         _lhsOsynMap' = rule31  ()
         __result_ = T_Nonterminals_vOut16 _lhsOinhMap' _lhsOsynMap' _lhsOvnonts
         in __result_ )
     in C_Nonterminals_s17 v16
   {-# INLINE rule29 #-}
   {-# LINE 96 "./src-ag/TfmToVisage.ag" #-}
   rule29 = \  (_ :: ()) ->
                     {-# LINE 96 "./src-ag/TfmToVisage.ag" #-}
                     []
                     {-# LINE 554 "dist/build/TfmToVisage.hs"#-}
   {-# INLINE rule30 #-}
   rule30 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule31 #-}
   rule31 = \  (_ :: ()) ->
     Map.empty

-- Pattern -----------------------------------------------------
-- wrapper
data Inh_Pattern  = Inh_Pattern {  }
data Syn_Pattern  = Syn_Pattern { copy_Syn_Pattern :: (Pattern), fieldattrs_Syn_Pattern :: ( [(Identifier,Identifier)] ), self_Syn_Pattern :: (Pattern), vpat_Syn_Pattern :: (VisagePattern) }
{-# INLINABLE wrap_Pattern #-}
wrap_Pattern :: T_Pattern  -> Inh_Pattern  -> (Syn_Pattern )
wrap_Pattern (T_Pattern act) (Inh_Pattern ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Pattern_vIn19 
        (T_Pattern_vOut19 _lhsOcopy _lhsOfieldattrs _lhsOself _lhsOvpat) <- return (inv_Pattern_s20 sem arg)
        return (Syn_Pattern _lhsOcopy _lhsOfieldattrs _lhsOself _lhsOvpat)
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
                               attach_T_Pattern :: Identity (T_Pattern_s20 )
                               }
newtype T_Pattern_s20  = C_Pattern_s20 {
                                       inv_Pattern_s20 :: (T_Pattern_v19 )
                                       }
data T_Pattern_s21  = C_Pattern_s21
type T_Pattern_v19  = (T_Pattern_vIn19 ) -> (T_Pattern_vOut19 )
data T_Pattern_vIn19  = T_Pattern_vIn19 
data T_Pattern_vOut19  = T_Pattern_vOut19 (Pattern) ( [(Identifier,Identifier)] ) (Pattern) (VisagePattern)
{-# NOINLINE sem_Pattern_Constr #-}
sem_Pattern_Constr :: (ConstructorIdent) -> T_Patterns  -> T_Pattern 
sem_Pattern_Constr arg_name_ arg_pats_ = T_Pattern (return st20) where
   {-# NOINLINE st20 #-}
   st20 = let
      v19 :: T_Pattern_v19 
      v19 = \ (T_Pattern_vIn19 ) -> ( let
         _patsX23 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_pats_))
         (T_Patterns_vOut22 _patsIcopy _patsIfieldattrs _patsIself _patsIvpats) = inv_Patterns_s23 _patsX23 (T_Patterns_vIn22 )
         _lhsOvpat :: VisagePattern
         _lhsOvpat = rule32 _patsIvpats arg_name_
         _lhsOfieldattrs ::  [(Identifier,Identifier)] 
         _lhsOfieldattrs = rule33 _patsIfieldattrs
         _copy = rule34 _patsIcopy arg_name_
         _self = rule35 _patsIself arg_name_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule36 _copy
         _lhsOself :: Pattern
         _lhsOself = rule37 _self
         __result_ = T_Pattern_vOut19 _lhsOcopy _lhsOfieldattrs _lhsOself _lhsOvpat
         in __result_ )
     in C_Pattern_s20 v19
   {-# INLINE rule32 #-}
   {-# LINE 136 "./src-ag/TfmToVisage.ag" #-}
   rule32 = \ ((_patsIvpats) :: [VisagePattern]) name_ ->
                             {-# LINE 136 "./src-ag/TfmToVisage.ag" #-}
                             VConstr name_ _patsIvpats
                             {-# LINE 623 "dist/build/TfmToVisage.hs"#-}
   {-# INLINE rule33 #-}
   rule33 = \ ((_patsIfieldattrs) ::  [(Identifier,Identifier)] ) ->
     _patsIfieldattrs
   {-# INLINE rule34 #-}
   rule34 = \ ((_patsIcopy) :: Patterns) name_ ->
     Constr name_ _patsIcopy
   {-# INLINE rule35 #-}
   rule35 = \ ((_patsIself) :: Patterns) name_ ->
     Constr name_ _patsIself
   {-# INLINE rule36 #-}
   rule36 = \ _copy ->
     _copy
   {-# INLINE rule37 #-}
   rule37 = \ _self ->
     _self
{-# NOINLINE sem_Pattern_Product #-}
sem_Pattern_Product :: (Pos) -> T_Patterns  -> T_Pattern 
sem_Pattern_Product arg_pos_ arg_pats_ = T_Pattern (return st20) where
   {-# NOINLINE st20 #-}
   st20 = let
      v19 :: T_Pattern_v19 
      v19 = \ (T_Pattern_vIn19 ) -> ( let
         _patsX23 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_pats_))
         (T_Patterns_vOut22 _patsIcopy _patsIfieldattrs _patsIself _patsIvpats) = inv_Patterns_s23 _patsX23 (T_Patterns_vIn22 )
         _lhsOvpat :: VisagePattern
         _lhsOvpat = rule38 _patsIvpats arg_pos_
         _lhsOfieldattrs ::  [(Identifier,Identifier)] 
         _lhsOfieldattrs = rule39 _patsIfieldattrs
         _copy = rule40 _patsIcopy arg_pos_
         _self = rule41 _patsIself arg_pos_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule42 _copy
         _lhsOself :: Pattern
         _lhsOself = rule43 _self
         __result_ = T_Pattern_vOut19 _lhsOcopy _lhsOfieldattrs _lhsOself _lhsOvpat
         in __result_ )
     in C_Pattern_s20 v19
   {-# INLINE rule38 #-}
   {-# LINE 137 "./src-ag/TfmToVisage.ag" #-}
   rule38 = \ ((_patsIvpats) :: [VisagePattern]) pos_ ->
                             {-# LINE 137 "./src-ag/TfmToVisage.ag" #-}
                             VProduct pos_ _patsIvpats
                             {-# LINE 666 "dist/build/TfmToVisage.hs"#-}
   {-# INLINE rule39 #-}
   rule39 = \ ((_patsIfieldattrs) ::  [(Identifier,Identifier)] ) ->
     _patsIfieldattrs
   {-# INLINE rule40 #-}
   rule40 = \ ((_patsIcopy) :: Patterns) pos_ ->
     Product pos_ _patsIcopy
   {-# INLINE rule41 #-}
   rule41 = \ ((_patsIself) :: Patterns) pos_ ->
     Product pos_ _patsIself
   {-# INLINE rule42 #-}
   rule42 = \ _copy ->
     _copy
   {-# INLINE rule43 #-}
   rule43 = \ _self ->
     _self
{-# NOINLINE sem_Pattern_Alias #-}
sem_Pattern_Alias :: (Identifier) -> (Identifier) -> T_Pattern  -> T_Pattern 
sem_Pattern_Alias arg_field_ arg_attr_ arg_pat_ = T_Pattern (return st20) where
   {-# NOINLINE st20 #-}
   st20 = let
      v19 :: T_Pattern_v19 
      v19 = \ (T_Pattern_vIn19 ) -> ( let
         _patX20 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pat_))
         (T_Pattern_vOut19 _patIcopy _patIfieldattrs _patIself _patIvpat) = inv_Pattern_s20 _patX20 (T_Pattern_vIn19 )
         _lhsOvpat :: VisagePattern
         _lhsOvpat = rule44 _patIvpat _self arg_attr_ arg_field_
         _lhsOfieldattrs ::  [(Identifier,Identifier)] 
         _lhsOfieldattrs = rule45 arg_attr_ arg_field_
         _copy = rule46 _patIcopy arg_attr_ arg_field_
         _self = rule47 _patIself arg_attr_ arg_field_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule48 _copy
         _lhsOself :: Pattern
         _lhsOself = rule49 _self
         __result_ = T_Pattern_vOut19 _lhsOcopy _lhsOfieldattrs _lhsOself _lhsOvpat
         in __result_ )
     in C_Pattern_s20 v19
   {-# INLINE rule44 #-}
   {-# LINE 138 "./src-ag/TfmToVisage.ag" #-}
   rule44 = \ ((_patIvpat) :: VisagePattern) _self attr_ field_ ->
                             {-# LINE 138 "./src-ag/TfmToVisage.ag" #-}
                             if (isVar _self)
                             then VVar field_ attr_
                             else VAlias field_ attr_ _patIvpat
                             {-# LINE 711 "dist/build/TfmToVisage.hs"#-}
   {-# INLINE rule45 #-}
   {-# LINE 147 "./src-ag/TfmToVisage.ag" #-}
   rule45 = \ attr_ field_ ->
                                   {-# LINE 147 "./src-ag/TfmToVisage.ag" #-}
                                   [(field_, attr_)]
                                   {-# LINE 717 "dist/build/TfmToVisage.hs"#-}
   {-# INLINE rule46 #-}
   rule46 = \ ((_patIcopy) :: Pattern) attr_ field_ ->
     Alias field_ attr_ _patIcopy
   {-# INLINE rule47 #-}
   rule47 = \ ((_patIself) :: Pattern) attr_ field_ ->
     Alias field_ attr_ _patIself
   {-# INLINE rule48 #-}
   rule48 = \ _copy ->
     _copy
   {-# INLINE rule49 #-}
   rule49 = \ _self ->
     _self
{-# NOINLINE sem_Pattern_Irrefutable #-}
sem_Pattern_Irrefutable :: T_Pattern  -> T_Pattern 
sem_Pattern_Irrefutable arg_pat_ = T_Pattern (return st20) where
   {-# NOINLINE st20 #-}
   st20 = let
      v19 :: T_Pattern_v19 
      v19 = \ (T_Pattern_vIn19 ) -> ( let
         _patX20 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pat_))
         (T_Pattern_vOut19 _patIcopy _patIfieldattrs _patIself _patIvpat) = inv_Pattern_s20 _patX20 (T_Pattern_vIn19 )
         _lhsOfieldattrs ::  [(Identifier,Identifier)] 
         _lhsOfieldattrs = rule50 _patIfieldattrs
         _copy = rule51 _patIcopy
         _self = rule52 _patIself
         _lhsOcopy :: Pattern
         _lhsOcopy = rule53 _copy
         _lhsOself :: Pattern
         _lhsOself = rule54 _self
         _lhsOvpat :: VisagePattern
         _lhsOvpat = rule55 _patIvpat
         __result_ = T_Pattern_vOut19 _lhsOcopy _lhsOfieldattrs _lhsOself _lhsOvpat
         in __result_ )
     in C_Pattern_s20 v19
   {-# INLINE rule50 #-}
   rule50 = \ ((_patIfieldattrs) ::  [(Identifier,Identifier)] ) ->
     _patIfieldattrs
   {-# INLINE rule51 #-}
   rule51 = \ ((_patIcopy) :: Pattern) ->
     Irrefutable _patIcopy
   {-# INLINE rule52 #-}
   rule52 = \ ((_patIself) :: Pattern) ->
     Irrefutable _patIself
   {-# INLINE rule53 #-}
   rule53 = \ _copy ->
     _copy
   {-# INLINE rule54 #-}
   rule54 = \ _self ->
     _self
   {-# INLINE rule55 #-}
   rule55 = \ ((_patIvpat) :: VisagePattern) ->
     _patIvpat
{-# NOINLINE sem_Pattern_Underscore #-}
sem_Pattern_Underscore :: (Pos) -> T_Pattern 
sem_Pattern_Underscore arg_pos_ = T_Pattern (return st20) where
   {-# NOINLINE st20 #-}
   st20 = let
      v19 :: T_Pattern_v19 
      v19 = \ (T_Pattern_vIn19 ) -> ( let
         _lhsOvpat :: VisagePattern
         _lhsOvpat = rule56 arg_pos_
         _lhsOfieldattrs ::  [(Identifier,Identifier)] 
         _lhsOfieldattrs = rule57  ()
         _copy = rule58 arg_pos_
         _self = rule59 arg_pos_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule60 _copy
         _lhsOself :: Pattern
         _lhsOself = rule61 _self
         __result_ = T_Pattern_vOut19 _lhsOcopy _lhsOfieldattrs _lhsOself _lhsOvpat
         in __result_ )
     in C_Pattern_s20 v19
   {-# INLINE rule56 #-}
   {-# LINE 141 "./src-ag/TfmToVisage.ag" #-}
   rule56 = \ pos_ ->
                             {-# LINE 141 "./src-ag/TfmToVisage.ag" #-}
                             VUnderscore pos_
                             {-# LINE 795 "dist/build/TfmToVisage.hs"#-}
   {-# INLINE rule57 #-}
   rule57 = \  (_ :: ()) ->
     []
   {-# INLINE rule58 #-}
   rule58 = \ pos_ ->
     Underscore pos_
   {-# INLINE rule59 #-}
   rule59 = \ pos_ ->
     Underscore pos_
   {-# INLINE rule60 #-}
   rule60 = \ _copy ->
     _copy
   {-# INLINE rule61 #-}
   rule61 = \ _self ->
     _self

-- Patterns ----------------------------------------------------
-- wrapper
data Inh_Patterns  = Inh_Patterns {  }
data Syn_Patterns  = Syn_Patterns { copy_Syn_Patterns :: (Patterns), fieldattrs_Syn_Patterns :: ( [(Identifier,Identifier)] ), self_Syn_Patterns :: (Patterns), vpats_Syn_Patterns :: ([VisagePattern]) }
{-# INLINABLE wrap_Patterns #-}
wrap_Patterns :: T_Patterns  -> Inh_Patterns  -> (Syn_Patterns )
wrap_Patterns (T_Patterns act) (Inh_Patterns ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Patterns_vIn22 
        (T_Patterns_vOut22 _lhsOcopy _lhsOfieldattrs _lhsOself _lhsOvpats) <- return (inv_Patterns_s23 sem arg)
        return (Syn_Patterns _lhsOcopy _lhsOfieldattrs _lhsOself _lhsOvpats)
   )

-- cata
{-# NOINLINE sem_Patterns #-}
sem_Patterns :: Patterns  -> T_Patterns 
sem_Patterns list = Prelude.foldr sem_Patterns_Cons sem_Patterns_Nil (Prelude.map sem_Pattern list)

-- semantic domain
newtype T_Patterns  = T_Patterns {
                                 attach_T_Patterns :: Identity (T_Patterns_s23 )
                                 }
newtype T_Patterns_s23  = C_Patterns_s23 {
                                         inv_Patterns_s23 :: (T_Patterns_v22 )
                                         }
data T_Patterns_s24  = C_Patterns_s24
type T_Patterns_v22  = (T_Patterns_vIn22 ) -> (T_Patterns_vOut22 )
data T_Patterns_vIn22  = T_Patterns_vIn22 
data T_Patterns_vOut22  = T_Patterns_vOut22 (Patterns) ( [(Identifier,Identifier)] ) (Patterns) ([VisagePattern])
{-# NOINLINE sem_Patterns_Cons #-}
sem_Patterns_Cons :: T_Pattern  -> T_Patterns  -> T_Patterns 
sem_Patterns_Cons arg_hd_ arg_tl_ = T_Patterns (return st23) where
   {-# NOINLINE st23 #-}
   st23 = let
      v22 :: T_Patterns_v22 
      v22 = \ (T_Patterns_vIn22 ) -> ( let
         _hdX20 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_hd_))
         _tlX23 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_tl_))
         (T_Pattern_vOut19 _hdIcopy _hdIfieldattrs _hdIself _hdIvpat) = inv_Pattern_s20 _hdX20 (T_Pattern_vIn19 )
         (T_Patterns_vOut22 _tlIcopy _tlIfieldattrs _tlIself _tlIvpats) = inv_Patterns_s23 _tlX23 (T_Patterns_vIn22 )
         _lhsOvpats :: [VisagePattern]
         _lhsOvpats = rule62 _hdIvpat _tlIvpats
         _lhsOfieldattrs ::  [(Identifier,Identifier)] 
         _lhsOfieldattrs = rule63 _hdIfieldattrs _tlIfieldattrs
         _copy = rule64 _hdIcopy _tlIcopy
         _self = rule65 _hdIself _tlIself
         _lhsOcopy :: Patterns
         _lhsOcopy = rule66 _copy
         _lhsOself :: Patterns
         _lhsOself = rule67 _self
         __result_ = T_Patterns_vOut22 _lhsOcopy _lhsOfieldattrs _lhsOself _lhsOvpats
         in __result_ )
     in C_Patterns_s23 v22
   {-# INLINE rule62 #-}
   {-# LINE 132 "./src-ag/TfmToVisage.ag" #-}
   rule62 = \ ((_hdIvpat) :: VisagePattern) ((_tlIvpats) :: [VisagePattern]) ->
                              {-# LINE 132 "./src-ag/TfmToVisage.ag" #-}
                              _hdIvpat : _tlIvpats
                              {-# LINE 871 "dist/build/TfmToVisage.hs"#-}
   {-# INLINE rule63 #-}
   rule63 = \ ((_hdIfieldattrs) ::  [(Identifier,Identifier)] ) ((_tlIfieldattrs) ::  [(Identifier,Identifier)] ) ->
     _hdIfieldattrs  ++  _tlIfieldattrs
   {-# INLINE rule64 #-}
   rule64 = \ ((_hdIcopy) :: Pattern) ((_tlIcopy) :: Patterns) ->
     (:) _hdIcopy _tlIcopy
   {-# INLINE rule65 #-}
   rule65 = \ ((_hdIself) :: Pattern) ((_tlIself) :: Patterns) ->
     (:) _hdIself _tlIself
   {-# INLINE rule66 #-}
   rule66 = \ _copy ->
     _copy
   {-# INLINE rule67 #-}
   rule67 = \ _self ->
     _self
{-# NOINLINE sem_Patterns_Nil #-}
sem_Patterns_Nil ::  T_Patterns 
sem_Patterns_Nil  = T_Patterns (return st23) where
   {-# NOINLINE st23 #-}
   st23 = let
      v22 :: T_Patterns_v22 
      v22 = \ (T_Patterns_vIn22 ) -> ( let
         _lhsOvpats :: [VisagePattern]
         _lhsOvpats = rule68  ()
         _lhsOfieldattrs ::  [(Identifier,Identifier)] 
         _lhsOfieldattrs = rule69  ()
         _copy = rule70  ()
         _self = rule71  ()
         _lhsOcopy :: Patterns
         _lhsOcopy = rule72 _copy
         _lhsOself :: Patterns
         _lhsOself = rule73 _self
         __result_ = T_Patterns_vOut22 _lhsOcopy _lhsOfieldattrs _lhsOself _lhsOvpats
         in __result_ )
     in C_Patterns_s23 v22
   {-# INLINE rule68 #-}
   {-# LINE 133 "./src-ag/TfmToVisage.ag" #-}
   rule68 = \  (_ :: ()) ->
                              {-# LINE 133 "./src-ag/TfmToVisage.ag" #-}
                              []
                              {-# LINE 912 "dist/build/TfmToVisage.hs"#-}
   {-# INLINE rule69 #-}
   rule69 = \  (_ :: ()) ->
     []
   {-# INLINE rule70 #-}
   rule70 = \  (_ :: ()) ->
     []
   {-# INLINE rule71 #-}
   rule71 = \  (_ :: ()) ->
     []
   {-# INLINE rule72 #-}
   rule72 = \ _copy ->
     _copy
   {-# INLINE rule73 #-}
   rule73 = \ _self ->
     _self

-- Production --------------------------------------------------
-- wrapper
data Inh_Production  = Inh_Production { inhMap_Inh_Production :: (Map Identifier Attributes), synMap_Inh_Production :: (Map Identifier Attributes) }
data Syn_Production  = Syn_Production { vprod_Syn_Production :: (VisageProduction) }
{-# INLINABLE wrap_Production #-}
wrap_Production :: T_Production  -> Inh_Production  -> (Syn_Production )
wrap_Production (T_Production act) (Inh_Production _lhsIinhMap _lhsIsynMap) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Production_vIn25 _lhsIinhMap _lhsIsynMap
        (T_Production_vOut25 _lhsOvprod) <- return (inv_Production_s26 sem arg)
        return (Syn_Production _lhsOvprod)
   )

-- cata
{-# INLINE sem_Production #-}
sem_Production :: Production  -> T_Production 
sem_Production ( Production con_ params_ constraints_ children_ rules_ typeSigs_ macro_ ) = sem_Production_Production con_ params_ constraints_ ( sem_Children children_ ) ( sem_Rules rules_ ) ( sem_TypeSigs typeSigs_ ) macro_

-- semantic domain
newtype T_Production  = T_Production {
                                     attach_T_Production :: Identity (T_Production_s26 )
                                     }
newtype T_Production_s26  = C_Production_s26 {
                                             inv_Production_s26 :: (T_Production_v25 )
                                             }
data T_Production_s27  = C_Production_s27
type T_Production_v25  = (T_Production_vIn25 ) -> (T_Production_vOut25 )
data T_Production_vIn25  = T_Production_vIn25 (Map Identifier Attributes) (Map Identifier Attributes)
data T_Production_vOut25  = T_Production_vOut25 (VisageProduction)
{-# NOINLINE sem_Production_Production #-}
sem_Production_Production :: (ConstructorIdent) -> ([Identifier]) -> ([Type]) -> T_Children  -> T_Rules  -> T_TypeSigs  -> (MaybeMacro) -> T_Production 
sem_Production_Production arg_con_ _ _ arg_children_ arg_rules_ arg_typeSigs_ _ = T_Production (return st26) where
   {-# NOINLINE st26 #-}
   st26 = let
      v25 :: T_Production_v25 
      v25 = \ (T_Production_vIn25 _lhsIinhMap _lhsIsynMap) -> ( let
         _childrenX5 = Control.Monad.Identity.runIdentity (attach_T_Children (arg_children_))
         _rulesX35 = Control.Monad.Identity.runIdentity (attach_T_Rules (arg_rules_))
         _typeSigsX41 = Control.Monad.Identity.runIdentity (attach_T_TypeSigs (arg_typeSigs_))
         (T_Children_vOut4 _childrenIvchildren) = inv_Children_s5 _childrenX5 (T_Children_vIn4 _childrenOinhMap _childrenOrulemap _childrenOsynMap)
         (T_Rules_vOut34 _rulesIvrules) = inv_Rules_s35 _rulesX35 (T_Rules_vIn34 )
         (T_TypeSigs_vOut40 ) = inv_TypeSigs_s41 _typeSigsX41 (T_TypeSigs_vIn40 )
         _lhsOvprod :: VisageProduction
         _lhsOvprod = rule74 _childrenIvchildren _lhsrules _locrules arg_con_
         _splitVRules = rule75 _rulesIvrules
         _locrules = rule76 _splitVRules
         _lhsrules = rule77 _splitVRules
         _childrenOrulemap = rule78 _splitVRules
         _childrenOinhMap = rule79 _lhsIinhMap
         _childrenOsynMap = rule80 _lhsIsynMap
         __result_ = T_Production_vOut25 _lhsOvprod
         in __result_ )
     in C_Production_s26 v25
   {-# INLINE rule74 #-}
   {-# LINE 110 "./src-ag/TfmToVisage.ag" #-}
   rule74 = \ ((_childrenIvchildren) :: [VisageChild]) _lhsrules _locrules con_ ->
                           {-# LINE 110 "./src-ag/TfmToVisage.ag" #-}
                           VProduction con_ _childrenIvchildren _lhsrules _locrules
                           {-# LINE 988 "dist/build/TfmToVisage.hs"#-}
   {-# INLINE rule75 #-}
   {-# LINE 111 "./src-ag/TfmToVisage.ag" #-}
   rule75 = \ ((_rulesIvrules) :: [VisageRule]) ->
                           {-# LINE 111 "./src-ag/TfmToVisage.ag" #-}
                           splitVRules _rulesIvrules
                           {-# LINE 994 "dist/build/TfmToVisage.hs"#-}
   {-# INLINE rule76 #-}
   {-# LINE 112 "./src-ag/TfmToVisage.ag" #-}
   rule76 = \ _splitVRules ->
                           {-# LINE 112 "./src-ag/TfmToVisage.ag" #-}
                           getForField "loc" _splitVRules
                           {-# LINE 1000 "dist/build/TfmToVisage.hs"#-}
   {-# INLINE rule77 #-}
   {-# LINE 113 "./src-ag/TfmToVisage.ag" #-}
   rule77 = \ _splitVRules ->
                           {-# LINE 113 "./src-ag/TfmToVisage.ag" #-}
                           getForField "lhs" _splitVRules
                           {-# LINE 1006 "dist/build/TfmToVisage.hs"#-}
   {-# INLINE rule78 #-}
   {-# LINE 114 "./src-ag/TfmToVisage.ag" #-}
   rule78 = \ _splitVRules ->
                           {-# LINE 114 "./src-ag/TfmToVisage.ag" #-}
                           _splitVRules
                           {-# LINE 1012 "dist/build/TfmToVisage.hs"#-}
   {-# INLINE rule79 #-}
   rule79 = \ ((_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# INLINE rule80 #-}
   rule80 = \ ((_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap

-- Productions -------------------------------------------------
-- wrapper
data Inh_Productions  = Inh_Productions { inhMap_Inh_Productions :: (Map Identifier Attributes), synMap_Inh_Productions :: (Map Identifier Attributes) }
data Syn_Productions  = Syn_Productions { vprods_Syn_Productions :: ([VisageProduction]) }
{-# INLINABLE wrap_Productions #-}
wrap_Productions :: T_Productions  -> Inh_Productions  -> (Syn_Productions )
wrap_Productions (T_Productions act) (Inh_Productions _lhsIinhMap _lhsIsynMap) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Productions_vIn28 _lhsIinhMap _lhsIsynMap
        (T_Productions_vOut28 _lhsOvprods) <- return (inv_Productions_s29 sem arg)
        return (Syn_Productions _lhsOvprods)
   )

-- cata
{-# NOINLINE sem_Productions #-}
sem_Productions :: Productions  -> T_Productions 
sem_Productions list = Prelude.foldr sem_Productions_Cons sem_Productions_Nil (Prelude.map sem_Production list)

-- semantic domain
newtype T_Productions  = T_Productions {
                                       attach_T_Productions :: Identity (T_Productions_s29 )
                                       }
newtype T_Productions_s29  = C_Productions_s29 {
                                               inv_Productions_s29 :: (T_Productions_v28 )
                                               }
data T_Productions_s30  = C_Productions_s30
type T_Productions_v28  = (T_Productions_vIn28 ) -> (T_Productions_vOut28 )
data T_Productions_vIn28  = T_Productions_vIn28 (Map Identifier Attributes) (Map Identifier Attributes)
data T_Productions_vOut28  = T_Productions_vOut28 ([VisageProduction])
{-# NOINLINE sem_Productions_Cons #-}
sem_Productions_Cons :: T_Production  -> T_Productions  -> T_Productions 
sem_Productions_Cons arg_hd_ arg_tl_ = T_Productions (return st29) where
   {-# NOINLINE st29 #-}
   st29 = let
      v28 :: T_Productions_v28 
      v28 = \ (T_Productions_vIn28 _lhsIinhMap _lhsIsynMap) -> ( let
         _hdX26 = Control.Monad.Identity.runIdentity (attach_T_Production (arg_hd_))
         _tlX29 = Control.Monad.Identity.runIdentity (attach_T_Productions (arg_tl_))
         (T_Production_vOut25 _hdIvprod) = inv_Production_s26 _hdX26 (T_Production_vIn25 _hdOinhMap _hdOsynMap)
         (T_Productions_vOut28 _tlIvprods) = inv_Productions_s29 _tlX29 (T_Productions_vIn28 _tlOinhMap _tlOsynMap)
         _lhsOvprods :: [VisageProduction]
         _lhsOvprods = rule81 _hdIvprod _tlIvprods
         _hdOinhMap = rule82 _lhsIinhMap
         _hdOsynMap = rule83 _lhsIsynMap
         _tlOinhMap = rule84 _lhsIinhMap
         _tlOsynMap = rule85 _lhsIsynMap
         __result_ = T_Productions_vOut28 _lhsOvprods
         in __result_ )
     in C_Productions_s29 v28
   {-# INLINE rule81 #-}
   {-# LINE 104 "./src-ag/TfmToVisage.ag" #-}
   rule81 = \ ((_hdIvprod) :: VisageProduction) ((_tlIvprods) :: [VisageProduction]) ->
                     {-# LINE 104 "./src-ag/TfmToVisage.ag" #-}
                     _hdIvprod : _tlIvprods
                     {-# LINE 1075 "dist/build/TfmToVisage.hs"#-}
   {-# INLINE rule82 #-}
   rule82 = \ ((_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# INLINE rule83 #-}
   rule83 = \ ((_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
   {-# INLINE rule84 #-}
   rule84 = \ ((_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# INLINE rule85 #-}
   rule85 = \ ((_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
{-# NOINLINE sem_Productions_Nil #-}
sem_Productions_Nil ::  T_Productions 
sem_Productions_Nil  = T_Productions (return st29) where
   {-# NOINLINE st29 #-}
   st29 = let
      v28 :: T_Productions_v28 
      v28 = \ (T_Productions_vIn28 _lhsIinhMap _lhsIsynMap) -> ( let
         _lhsOvprods :: [VisageProduction]
         _lhsOvprods = rule86  ()
         __result_ = T_Productions_vOut28 _lhsOvprods
         in __result_ )
     in C_Productions_s29 v28
   {-# INLINE rule86 #-}
   {-# LINE 106 "./src-ag/TfmToVisage.ag" #-}
   rule86 = \  (_ :: ()) ->
                     {-# LINE 106 "./src-ag/TfmToVisage.ag" #-}
                     []
                     {-# LINE 1105 "dist/build/TfmToVisage.hs"#-}

-- Rule --------------------------------------------------------
-- wrapper
data Inh_Rule  = Inh_Rule {  }
data Syn_Rule  = Syn_Rule { vrule_Syn_Rule :: (VisageRule) }
{-# INLINABLE wrap_Rule #-}
wrap_Rule :: T_Rule  -> Inh_Rule  -> (Syn_Rule )
wrap_Rule (T_Rule act) (Inh_Rule ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Rule_vIn31 
        (T_Rule_vOut31 _lhsOvrule) <- return (inv_Rule_s32 sem arg)
        return (Syn_Rule _lhsOvrule)
   )

-- cata
{-# INLINE sem_Rule #-}
sem_Rule :: Rule  -> T_Rule 
sem_Rule ( Rule mbName_ pattern_ rhs_ owrt_ origin_ explicit_ pure_ identity_ mbError_ eager_ ) = sem_Rule_Rule mbName_ ( sem_Pattern pattern_ ) ( sem_Expression rhs_ ) owrt_ origin_ explicit_ pure_ identity_ mbError_ eager_

-- semantic domain
newtype T_Rule  = T_Rule {
                         attach_T_Rule :: Identity (T_Rule_s32 )
                         }
newtype T_Rule_s32  = C_Rule_s32 {
                                 inv_Rule_s32 :: (T_Rule_v31 )
                                 }
data T_Rule_s33  = C_Rule_s33
type T_Rule_v31  = (T_Rule_vIn31 ) -> (T_Rule_vOut31 )
data T_Rule_vIn31  = T_Rule_vIn31 
data T_Rule_vOut31  = T_Rule_vOut31 (VisageRule)
{-# NOINLINE sem_Rule_Rule #-}
sem_Rule_Rule :: (Maybe Identifier) -> T_Pattern  -> T_Expression  -> (Bool) -> (String) -> (Bool) -> (Bool) -> (Bool) -> (Maybe Error) -> (Bool) -> T_Rule 
sem_Rule_Rule _ arg_pattern_ arg_rhs_ arg_owrt_ _ _ _ _ _ _ = T_Rule (return st32) where
   {-# NOINLINE st32 #-}
   st32 = let
      v31 :: T_Rule_v31 
      v31 = \ (T_Rule_vIn31 ) -> ( let
         _patternX20 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pattern_))
         _rhsX8 = Control.Monad.Identity.runIdentity (attach_T_Expression (arg_rhs_))
         (T_Pattern_vOut19 _patternIcopy _patternIfieldattrs _patternIself _patternIvpat) = inv_Pattern_s20 _patternX20 (T_Pattern_vIn19 )
         (T_Expression_vOut7 _rhsIself) = inv_Expression_s8 _rhsX8 (T_Expression_vIn7 )
         _lhsOvrule :: VisageRule
         _lhsOvrule = rule87 _patternIfieldattrs _patternIvpat _rhsIself arg_owrt_
         __result_ = T_Rule_vOut31 _lhsOvrule
         in __result_ )
     in C_Rule_s32 v31
   {-# INLINE rule87 #-}
   {-# LINE 129 "./src-ag/TfmToVisage.ag" #-}
   rule87 = \ ((_patternIfieldattrs) ::  [(Identifier,Identifier)] ) ((_patternIvpat) :: VisagePattern) ((_rhsIself) :: Expression) owrt_ ->
                         {-# LINE 129 "./src-ag/TfmToVisage.ag" #-}
                         VRule _patternIfieldattrs undefined _patternIvpat _rhsIself owrt_
                         {-# LINE 1158 "dist/build/TfmToVisage.hs"#-}

-- Rules -------------------------------------------------------
-- wrapper
data Inh_Rules  = Inh_Rules {  }
data Syn_Rules  = Syn_Rules { vrules_Syn_Rules :: ([VisageRule]) }
{-# INLINABLE wrap_Rules #-}
wrap_Rules :: T_Rules  -> Inh_Rules  -> (Syn_Rules )
wrap_Rules (T_Rules act) (Inh_Rules ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Rules_vIn34 
        (T_Rules_vOut34 _lhsOvrules) <- return (inv_Rules_s35 sem arg)
        return (Syn_Rules _lhsOvrules)
   )

-- cata
{-# NOINLINE sem_Rules #-}
sem_Rules :: Rules  -> T_Rules 
sem_Rules list = Prelude.foldr sem_Rules_Cons sem_Rules_Nil (Prelude.map sem_Rule list)

-- semantic domain
newtype T_Rules  = T_Rules {
                           attach_T_Rules :: Identity (T_Rules_s35 )
                           }
newtype T_Rules_s35  = C_Rules_s35 {
                                   inv_Rules_s35 :: (T_Rules_v34 )
                                   }
data T_Rules_s36  = C_Rules_s36
type T_Rules_v34  = (T_Rules_vIn34 ) -> (T_Rules_vOut34 )
data T_Rules_vIn34  = T_Rules_vIn34 
data T_Rules_vOut34  = T_Rules_vOut34 ([VisageRule])
{-# NOINLINE sem_Rules_Cons #-}
sem_Rules_Cons :: T_Rule  -> T_Rules  -> T_Rules 
sem_Rules_Cons arg_hd_ arg_tl_ = T_Rules (return st35) where
   {-# NOINLINE st35 #-}
   st35 = let
      v34 :: T_Rules_v34 
      v34 = \ (T_Rules_vIn34 ) -> ( let
         _hdX32 = Control.Monad.Identity.runIdentity (attach_T_Rule (arg_hd_))
         _tlX35 = Control.Monad.Identity.runIdentity (attach_T_Rules (arg_tl_))
         (T_Rule_vOut31 _hdIvrule) = inv_Rule_s32 _hdX32 (T_Rule_vIn31 )
         (T_Rules_vOut34 _tlIvrules) = inv_Rules_s35 _tlX35 (T_Rules_vIn34 )
         _lhsOvrules :: [VisageRule]
         _lhsOvrules = rule88 _hdIvrule _tlIvrules
         __result_ = T_Rules_vOut34 _lhsOvrules
         in __result_ )
     in C_Rules_s35 v34
   {-# INLINE rule88 #-}
   {-# LINE 124 "./src-ag/TfmToVisage.ag" #-}
   rule88 = \ ((_hdIvrule) :: VisageRule) ((_tlIvrules) :: [VisageRule]) ->
                               {-# LINE 124 "./src-ag/TfmToVisage.ag" #-}
                               _hdIvrule : _tlIvrules
                               {-# LINE 1211 "dist/build/TfmToVisage.hs"#-}
{-# NOINLINE sem_Rules_Nil #-}
sem_Rules_Nil ::  T_Rules 
sem_Rules_Nil  = T_Rules (return st35) where
   {-# NOINLINE st35 #-}
   st35 = let
      v34 :: T_Rules_v34 
      v34 = \ (T_Rules_vIn34 ) -> ( let
         _lhsOvrules :: [VisageRule]
         _lhsOvrules = rule89  ()
         __result_ = T_Rules_vOut34 _lhsOvrules
         in __result_ )
     in C_Rules_s35 v34
   {-# INLINE rule89 #-}
   {-# LINE 125 "./src-ag/TfmToVisage.ag" #-}
   rule89 = \  (_ :: ()) ->
                               {-# LINE 125 "./src-ag/TfmToVisage.ag" #-}
                               []
                               {-# LINE 1229 "dist/build/TfmToVisage.hs"#-}

-- TypeSig -----------------------------------------------------
-- wrapper
data Inh_TypeSig  = Inh_TypeSig {  }
data Syn_TypeSig  = Syn_TypeSig {  }
{-# INLINABLE wrap_TypeSig #-}
wrap_TypeSig :: T_TypeSig  -> Inh_TypeSig  -> (Syn_TypeSig )
wrap_TypeSig (T_TypeSig act) (Inh_TypeSig ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_TypeSig_vIn37 
        (T_TypeSig_vOut37 ) <- return (inv_TypeSig_s38 sem arg)
        return (Syn_TypeSig )
   )

-- cata
{-# INLINE sem_TypeSig #-}
sem_TypeSig :: TypeSig  -> T_TypeSig 
sem_TypeSig ( TypeSig name_ tp_ ) = sem_TypeSig_TypeSig name_ tp_

-- semantic domain
newtype T_TypeSig  = T_TypeSig {
                               attach_T_TypeSig :: Identity (T_TypeSig_s38 )
                               }
newtype T_TypeSig_s38  = C_TypeSig_s38 {
                                       inv_TypeSig_s38 :: (T_TypeSig_v37 )
                                       }
data T_TypeSig_s39  = C_TypeSig_s39
type T_TypeSig_v37  = (T_TypeSig_vIn37 ) -> (T_TypeSig_vOut37 )
data T_TypeSig_vIn37  = T_TypeSig_vIn37 
data T_TypeSig_vOut37  = T_TypeSig_vOut37 
{-# NOINLINE sem_TypeSig_TypeSig #-}
sem_TypeSig_TypeSig :: (Identifier) -> (Type) -> T_TypeSig 
sem_TypeSig_TypeSig _ _ = T_TypeSig (return st38) where
   {-# NOINLINE st38 #-}
   st38 = let
      v37 :: T_TypeSig_v37 
      v37 = \ (T_TypeSig_vIn37 ) -> ( let
         __result_ = T_TypeSig_vOut37 
         in __result_ )
     in C_TypeSig_s38 v37

-- TypeSigs ----------------------------------------------------
-- wrapper
data Inh_TypeSigs  = Inh_TypeSigs {  }
data Syn_TypeSigs  = Syn_TypeSigs {  }
{-# INLINABLE wrap_TypeSigs #-}
wrap_TypeSigs :: T_TypeSigs  -> Inh_TypeSigs  -> (Syn_TypeSigs )
wrap_TypeSigs (T_TypeSigs act) (Inh_TypeSigs ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_TypeSigs_vIn40 
        (T_TypeSigs_vOut40 ) <- return (inv_TypeSigs_s41 sem arg)
        return (Syn_TypeSigs )
   )

-- cata
{-# NOINLINE sem_TypeSigs #-}
sem_TypeSigs :: TypeSigs  -> T_TypeSigs 
sem_TypeSigs list = Prelude.foldr sem_TypeSigs_Cons sem_TypeSigs_Nil (Prelude.map sem_TypeSig list)

-- semantic domain
newtype T_TypeSigs  = T_TypeSigs {
                                 attach_T_TypeSigs :: Identity (T_TypeSigs_s41 )
                                 }
newtype T_TypeSigs_s41  = C_TypeSigs_s41 {
                                         inv_TypeSigs_s41 :: (T_TypeSigs_v40 )
                                         }
data T_TypeSigs_s42  = C_TypeSigs_s42
type T_TypeSigs_v40  = (T_TypeSigs_vIn40 ) -> (T_TypeSigs_vOut40 )
data T_TypeSigs_vIn40  = T_TypeSigs_vIn40 
data T_TypeSigs_vOut40  = T_TypeSigs_vOut40 
{-# NOINLINE sem_TypeSigs_Cons #-}
sem_TypeSigs_Cons :: T_TypeSig  -> T_TypeSigs  -> T_TypeSigs 
sem_TypeSigs_Cons arg_hd_ arg_tl_ = T_TypeSigs (return st41) where
   {-# NOINLINE st41 #-}
   st41 = let
      v40 :: T_TypeSigs_v40 
      v40 = \ (T_TypeSigs_vIn40 ) -> ( let
         _hdX38 = Control.Monad.Identity.runIdentity (attach_T_TypeSig (arg_hd_))
         _tlX41 = Control.Monad.Identity.runIdentity (attach_T_TypeSigs (arg_tl_))
         (T_TypeSig_vOut37 ) = inv_TypeSig_s38 _hdX38 (T_TypeSig_vIn37 )
         (T_TypeSigs_vOut40 ) = inv_TypeSigs_s41 _tlX41 (T_TypeSigs_vIn40 )
         __result_ = T_TypeSigs_vOut40 
         in __result_ )
     in C_TypeSigs_s41 v40
{-# NOINLINE sem_TypeSigs_Nil #-}
sem_TypeSigs_Nil ::  T_TypeSigs 
sem_TypeSigs_Nil  = T_TypeSigs (return st41) where
   {-# NOINLINE st41 #-}
   st41 = let
      v40 :: T_TypeSigs_v40 
      v40 = \ (T_TypeSigs_vIn40 ) -> ( let
         __result_ = T_TypeSigs_vOut40 
         in __result_ )
     in C_TypeSigs_s41 v40
