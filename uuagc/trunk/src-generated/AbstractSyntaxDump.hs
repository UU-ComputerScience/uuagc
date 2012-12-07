{-# LANGUAGE Rank2Types, GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AbstractSyntaxDump where
{-# LINE 6 "./src-ag/AbstractSyntaxDump.ag" #-}

import Data.List
import qualified Data.Map as Map

import Pretty
import PPUtil

import AbstractSyntax
import TokenDef
{-# LINE 16 "dist/build/AbstractSyntaxDump.hs" #-}

{-# LINE 2 "./src-ag/AbstractSyntax.ag" #-}

-- AbstractSyntax.ag imports
import Data.Set(Set)
import Data.Map(Map)
import Patterns    (Pattern(..),Patterns)
import Expression  (Expression(..))
import Macro --marcos
import CommonTypes
import ErrorMessages
{-# LINE 28 "dist/build/AbstractSyntaxDump.hs" #-}

{-# LINE 2 "./src-ag/Patterns.ag" #-}

-- Patterns.ag imports
import UU.Scanner.Position(Pos)
import CommonTypes (ConstructorIdent,Identifier)
{-# LINE 35 "dist/build/AbstractSyntaxDump.hs" #-}

{-# LINE 2 "./src-ag/Expression.ag" #-}

import UU.Scanner.Position(Pos)
import HsToken
{-# LINE 41 "dist/build/AbstractSyntaxDump.hs" #-}
import Control.Monad.Identity (Identity)
import qualified Control.Monad.Identity
-- Child -------------------------------------------------------
-- wrapper
data Inh_Child  = Inh_Child {  }
data Syn_Child  = Syn_Child { pp_Syn_Child :: (PP_Doc) }
{-# INLINABLE wrap_Child #-}
wrap_Child :: T_Child  -> Inh_Child  -> (Syn_Child )
wrap_Child (T_Child act) (Inh_Child ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Child_vIn1 
        (T_Child_vOut1 _lhsOpp) <- return (inv_Child_s2 sem arg)
        return (Syn_Child _lhsOpp)
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
data T_Child_vIn1  = T_Child_vIn1 
data T_Child_vOut1  = T_Child_vOut1 (PP_Doc)
{-# NOINLINE sem_Child_Child #-}
sem_Child_Child :: (Identifier) -> (Type) -> (ChildKind) -> T_Child 
sem_Child_Child arg_name_ arg_tp_ arg_kind_ = T_Child (return st2) where
   {-# NOINLINE st2 #-}
   st2 = let
      v1 :: T_Child_v1 
      v1 = \ (T_Child_vIn1 ) -> ( let
         _lhsOpp :: PP_Doc
         _lhsOpp = rule0 arg_kind_ arg_name_ arg_tp_
         __result_ = T_Child_vOut1 _lhsOpp
         in __result_ )
     in C_Child_s2 v1
   {-# INLINE rule0 #-}
   {-# LINE 35 "./src-ag/AbstractSyntaxDump.ag" #-}
   rule0 = \ kind_ name_ tp_ ->
                                                              {-# LINE 35 "./src-ag/AbstractSyntaxDump.ag" #-}
                                                              ppNestInfo ["Child","Child"] [pp name_, ppShow tp_] [ppF "kind" $ ppShow kind_] []
                                                              {-# LINE 91 "dist/build/AbstractSyntaxDump.hs"#-}

-- Children ----------------------------------------------------
-- wrapper
data Inh_Children  = Inh_Children {  }
data Syn_Children  = Syn_Children { pp_Syn_Children :: (PP_Doc), ppL_Syn_Children :: ([PP_Doc]) }
{-# INLINABLE wrap_Children #-}
wrap_Children :: T_Children  -> Inh_Children  -> (Syn_Children )
wrap_Children (T_Children act) (Inh_Children ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Children_vIn4 
        (T_Children_vOut4 _lhsOpp _lhsOppL) <- return (inv_Children_s5 sem arg)
        return (Syn_Children _lhsOpp _lhsOppL)
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
data T_Children_vIn4  = T_Children_vIn4 
data T_Children_vOut4  = T_Children_vOut4 (PP_Doc) ([PP_Doc])
{-# NOINLINE sem_Children_Cons #-}
sem_Children_Cons :: T_Child  -> T_Children  -> T_Children 
sem_Children_Cons arg_hd_ arg_tl_ = T_Children (return st5) where
   {-# NOINLINE st5 #-}
   st5 = let
      v4 :: T_Children_v4 
      v4 = \ (T_Children_vIn4 ) -> ( let
         _hdX2 = Control.Monad.Identity.runIdentity (attach_T_Child (arg_hd_))
         _tlX5 = Control.Monad.Identity.runIdentity (attach_T_Children (arg_tl_))
         (T_Child_vOut1 _hdIpp) = inv_Child_s2 _hdX2 (T_Child_vIn1 )
         (T_Children_vOut4 _tlIpp _tlIppL) = inv_Children_s5 _tlX5 (T_Children_vIn4 )
         _lhsOppL :: [PP_Doc]
         _lhsOppL = rule1 _hdIpp _tlIppL
         _lhsOpp :: PP_Doc
         _lhsOpp = rule2 _hdIpp _tlIpp
         __result_ = T_Children_vOut4 _lhsOpp _lhsOppL
         in __result_ )
     in C_Children_s5 v4
   {-# INLINE rule1 #-}
   {-# LINE 67 "./src-ag/AbstractSyntaxDump.ag" #-}
   rule1 = \ ((_hdIpp) :: PP_Doc) ((_tlIppL) :: [PP_Doc]) ->
                                                                                  {-# LINE 67 "./src-ag/AbstractSyntaxDump.ag" #-}
                                                                                  _hdIpp : _tlIppL
                                                                                  {-# LINE 146 "dist/build/AbstractSyntaxDump.hs"#-}
   {-# INLINE rule2 #-}
   rule2 = \ ((_hdIpp) :: PP_Doc) ((_tlIpp) :: PP_Doc) ->
     _hdIpp >-< _tlIpp
{-# NOINLINE sem_Children_Nil #-}
sem_Children_Nil ::  T_Children 
sem_Children_Nil  = T_Children (return st5) where
   {-# NOINLINE st5 #-}
   st5 = let
      v4 :: T_Children_v4 
      v4 = \ (T_Children_vIn4 ) -> ( let
         _lhsOppL :: [PP_Doc]
         _lhsOppL = rule3  ()
         _lhsOpp :: PP_Doc
         _lhsOpp = rule4  ()
         __result_ = T_Children_vOut4 _lhsOpp _lhsOppL
         in __result_ )
     in C_Children_s5 v4
   {-# INLINE rule3 #-}
   {-# LINE 68 "./src-ag/AbstractSyntaxDump.ag" #-}
   rule3 = \  (_ :: ()) ->
                                                                                  {-# LINE 68 "./src-ag/AbstractSyntaxDump.ag" #-}
                                                                                  []
                                                                                  {-# LINE 169 "dist/build/AbstractSyntaxDump.hs"#-}
   {-# INLINE rule4 #-}
   rule4 = \  (_ :: ()) ->
     empty

-- Expression --------------------------------------------------
-- wrapper
data Inh_Expression  = Inh_Expression {  }
data Syn_Expression  = Syn_Expression { pp_Syn_Expression :: (PP_Doc) }
{-# INLINABLE wrap_Expression #-}
wrap_Expression :: T_Expression  -> Inh_Expression  -> (Syn_Expression )
wrap_Expression (T_Expression act) (Inh_Expression ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Expression_vIn7 
        (T_Expression_vOut7 _lhsOpp) <- return (inv_Expression_s8 sem arg)
        return (Syn_Expression _lhsOpp)
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
data T_Expression_vOut7  = T_Expression_vOut7 (PP_Doc)
{-# NOINLINE sem_Expression_Expression #-}
sem_Expression_Expression :: (Pos) -> ([HsToken]) -> T_Expression 
sem_Expression_Expression arg_pos_ arg_tks_ = T_Expression (return st8) where
   {-# NOINLINE st8 #-}
   st8 = let
      v7 :: T_Expression_v7 
      v7 = \ (T_Expression_vIn7 ) -> ( let
         _lhsOpp :: PP_Doc
         _lhsOpp = rule5 arg_pos_ arg_tks_
         __result_ = T_Expression_vOut7 _lhsOpp
         in __result_ )
     in C_Expression_s8 v7
   {-# INLINE rule5 #-}
   {-# LINE 50 "./src-ag/AbstractSyntaxDump.ag" #-}
   rule5 = \ pos_ tks_ ->
                                                      {-# LINE 50 "./src-ag/AbstractSyntaxDump.ag" #-}
                                                      ppNestInfo ["Expression","Expression"] [ppShow pos_] [ppF "txt" $ vlist . showTokens . tokensToStrings $ tks_] []
                                                      {-# LINE 221 "dist/build/AbstractSyntaxDump.hs"#-}

-- Grammar -----------------------------------------------------
-- wrapper
data Inh_Grammar  = Inh_Grammar {  }
data Syn_Grammar  = Syn_Grammar { pp_Syn_Grammar :: (PP_Doc) }
{-# INLINABLE wrap_Grammar #-}
wrap_Grammar :: T_Grammar  -> Inh_Grammar  -> (Syn_Grammar )
wrap_Grammar (T_Grammar act) (Inh_Grammar ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Grammar_vIn10 
        (T_Grammar_vOut10 _lhsOpp) <- return (inv_Grammar_s11 sem arg)
        return (Syn_Grammar _lhsOpp)
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
data T_Grammar_vOut10  = T_Grammar_vOut10 (PP_Doc)
{-# NOINLINE sem_Grammar_Grammar #-}
sem_Grammar_Grammar :: (TypeSyns) -> (UseMap) -> (Derivings) -> (Set NontermIdent) -> T_Nonterminals  -> (PragmaMap) -> (AttrOrderMap) -> (ParamMap) -> (ContextMap) -> (QuantMap) -> (UniqueMap) -> (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) -> (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) -> (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier, [Identifier], Expression)))) -> T_Grammar 
sem_Grammar_Grammar arg_typeSyns_ arg_useMap_ arg_derivings_ arg_wrappers_ arg_nonts_ _ _ _ _ _ _ _ _ _ = T_Grammar (return st11) where
   {-# NOINLINE st11 #-}
   st11 = let
      v10 :: T_Grammar_v10 
      v10 = \ (T_Grammar_vIn10 ) -> ( let
         _nontsX17 = Control.Monad.Identity.runIdentity (attach_T_Nonterminals (arg_nonts_))
         (T_Nonterminals_vOut16 _nontsIpp _nontsIppL) = inv_Nonterminals_s17 _nontsX17 (T_Nonterminals_vIn16 )
         _lhsOpp :: PP_Doc
         _lhsOpp = rule6 _nontsIppL arg_derivings_ arg_typeSyns_ arg_useMap_ arg_wrappers_
         __result_ = T_Grammar_vOut10 _lhsOpp
         in __result_ )
     in C_Grammar_s11 v10
   {-# INLINE rule6 #-}
   {-# LINE 20 "./src-ag/AbstractSyntaxDump.ag" #-}
   rule6 = \ ((_nontsIppL) :: [PP_Doc]) derivings_ typeSyns_ useMap_ wrappers_ ->
                                                      {-# LINE 20 "./src-ag/AbstractSyntaxDump.ag" #-}
                                                      ppNestInfo ["Grammar","Grammar"] []
                                                         [ ppF "typeSyns" $ ppAssocL typeSyns_
                                                         , ppF "useMap" $ ppMap $ Map.map ppMap $ useMap_
                                                         , ppF "derivings" $ ppMap $ derivings_
                                                         , ppF "wrappers" $ ppShow $ wrappers_
                                                         , ppF "nonts" $ ppVList _nontsIppL
                                                         ] []
                                                      {-# LINE 278 "dist/build/AbstractSyntaxDump.hs"#-}

-- Nonterminal -------------------------------------------------
-- wrapper
data Inh_Nonterminal  = Inh_Nonterminal {  }
data Syn_Nonterminal  = Syn_Nonterminal { pp_Syn_Nonterminal :: (PP_Doc) }
{-# INLINABLE wrap_Nonterminal #-}
wrap_Nonterminal :: T_Nonterminal  -> Inh_Nonterminal  -> (Syn_Nonterminal )
wrap_Nonterminal (T_Nonterminal act) (Inh_Nonterminal ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Nonterminal_vIn13 
        (T_Nonterminal_vOut13 _lhsOpp) <- return (inv_Nonterminal_s14 sem arg)
        return (Syn_Nonterminal _lhsOpp)
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
data T_Nonterminal_vIn13  = T_Nonterminal_vIn13 
data T_Nonterminal_vOut13  = T_Nonterminal_vOut13 (PP_Doc)
{-# NOINLINE sem_Nonterminal_Nonterminal #-}
sem_Nonterminal_Nonterminal :: (NontermIdent) -> ([Identifier]) -> (Attributes) -> (Attributes) -> T_Productions  -> T_Nonterminal 
sem_Nonterminal_Nonterminal arg_nt_ arg_params_ arg_inh_ arg_syn_ arg_prods_ = T_Nonterminal (return st14) where
   {-# NOINLINE st14 #-}
   st14 = let
      v13 :: T_Nonterminal_v13 
      v13 = \ (T_Nonterminal_vIn13 ) -> ( let
         _prodsX29 = Control.Monad.Identity.runIdentity (attach_T_Productions (arg_prods_))
         (T_Productions_vOut28 _prodsIpp _prodsIppL) = inv_Productions_s29 _prodsX29 (T_Productions_vIn28 )
         _lhsOpp :: PP_Doc
         _lhsOpp = rule7 _prodsIppL arg_inh_ arg_nt_ arg_params_ arg_syn_
         __result_ = T_Nonterminal_vOut13 _lhsOpp
         in __result_ )
     in C_Nonterminal_s14 v13
   {-# INLINE rule7 #-}
   {-# LINE 29 "./src-ag/AbstractSyntaxDump.ag" #-}
   rule7 = \ ((_prodsIppL) :: [PP_Doc]) inh_ nt_ params_ syn_ ->
                                                      {-# LINE 29 "./src-ag/AbstractSyntaxDump.ag" #-}
                                                      ppNestInfo ["Nonterminal","Nonterminal"] (pp nt_ : map pp params_) [ppF "inh" $ ppMap inh_, ppF "syn" $ ppMap syn_, ppF "prods" $ ppVList _prodsIppL] []
                                                      {-# LINE 329 "dist/build/AbstractSyntaxDump.hs"#-}

-- Nonterminals ------------------------------------------------
-- wrapper
data Inh_Nonterminals  = Inh_Nonterminals {  }
data Syn_Nonterminals  = Syn_Nonterminals { pp_Syn_Nonterminals :: (PP_Doc), ppL_Syn_Nonterminals :: ([PP_Doc]) }
{-# INLINABLE wrap_Nonterminals #-}
wrap_Nonterminals :: T_Nonterminals  -> Inh_Nonterminals  -> (Syn_Nonterminals )
wrap_Nonterminals (T_Nonterminals act) (Inh_Nonterminals ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Nonterminals_vIn16 
        (T_Nonterminals_vOut16 _lhsOpp _lhsOppL) <- return (inv_Nonterminals_s17 sem arg)
        return (Syn_Nonterminals _lhsOpp _lhsOppL)
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
data T_Nonterminals_vIn16  = T_Nonterminals_vIn16 
data T_Nonterminals_vOut16  = T_Nonterminals_vOut16 (PP_Doc) ([PP_Doc])
{-# NOINLINE sem_Nonterminals_Cons #-}
sem_Nonterminals_Cons :: T_Nonterminal  -> T_Nonterminals  -> T_Nonterminals 
sem_Nonterminals_Cons arg_hd_ arg_tl_ = T_Nonterminals (return st17) where
   {-# NOINLINE st17 #-}
   st17 = let
      v16 :: T_Nonterminals_v16 
      v16 = \ (T_Nonterminals_vIn16 ) -> ( let
         _hdX14 = Control.Monad.Identity.runIdentity (attach_T_Nonterminal (arg_hd_))
         _tlX17 = Control.Monad.Identity.runIdentity (attach_T_Nonterminals (arg_tl_))
         (T_Nonterminal_vOut13 _hdIpp) = inv_Nonterminal_s14 _hdX14 (T_Nonterminal_vIn13 )
         (T_Nonterminals_vOut16 _tlIpp _tlIppL) = inv_Nonterminals_s17 _tlX17 (T_Nonterminals_vIn16 )
         _lhsOppL :: [PP_Doc]
         _lhsOppL = rule8 _hdIpp _tlIppL
         _lhsOpp :: PP_Doc
         _lhsOpp = rule9 _hdIpp _tlIpp
         __result_ = T_Nonterminals_vOut16 _lhsOpp _lhsOppL
         in __result_ )
     in C_Nonterminals_s17 v16
   {-# INLINE rule8 #-}
   {-# LINE 75 "./src-ag/AbstractSyntaxDump.ag" #-}
   rule8 = \ ((_hdIpp) :: PP_Doc) ((_tlIppL) :: [PP_Doc]) ->
                                                                                  {-# LINE 75 "./src-ag/AbstractSyntaxDump.ag" #-}
                                                                                  _hdIpp : _tlIppL
                                                                                  {-# LINE 384 "dist/build/AbstractSyntaxDump.hs"#-}
   {-# INLINE rule9 #-}
   rule9 = \ ((_hdIpp) :: PP_Doc) ((_tlIpp) :: PP_Doc) ->
     _hdIpp >-< _tlIpp
{-# NOINLINE sem_Nonterminals_Nil #-}
sem_Nonterminals_Nil ::  T_Nonterminals 
sem_Nonterminals_Nil  = T_Nonterminals (return st17) where
   {-# NOINLINE st17 #-}
   st17 = let
      v16 :: T_Nonterminals_v16 
      v16 = \ (T_Nonterminals_vIn16 ) -> ( let
         _lhsOppL :: [PP_Doc]
         _lhsOppL = rule10  ()
         _lhsOpp :: PP_Doc
         _lhsOpp = rule11  ()
         __result_ = T_Nonterminals_vOut16 _lhsOpp _lhsOppL
         in __result_ )
     in C_Nonterminals_s17 v16
   {-# INLINE rule10 #-}
   {-# LINE 76 "./src-ag/AbstractSyntaxDump.ag" #-}
   rule10 = \  (_ :: ()) ->
                                                                                  {-# LINE 76 "./src-ag/AbstractSyntaxDump.ag" #-}
                                                                                  []
                                                                                  {-# LINE 407 "dist/build/AbstractSyntaxDump.hs"#-}
   {-# INLINE rule11 #-}
   rule11 = \  (_ :: ()) ->
     empty

-- Pattern -----------------------------------------------------
-- wrapper
data Inh_Pattern  = Inh_Pattern {  }
data Syn_Pattern  = Syn_Pattern { copy_Syn_Pattern :: (Pattern), pp_Syn_Pattern :: (PP_Doc) }
{-# INLINABLE wrap_Pattern #-}
wrap_Pattern :: T_Pattern  -> Inh_Pattern  -> (Syn_Pattern )
wrap_Pattern (T_Pattern act) (Inh_Pattern ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Pattern_vIn19 
        (T_Pattern_vOut19 _lhsOcopy _lhsOpp) <- return (inv_Pattern_s20 sem arg)
        return (Syn_Pattern _lhsOcopy _lhsOpp)
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
data T_Pattern_vOut19  = T_Pattern_vOut19 (Pattern) (PP_Doc)
{-# NOINLINE sem_Pattern_Constr #-}
sem_Pattern_Constr :: (ConstructorIdent) -> T_Patterns  -> T_Pattern 
sem_Pattern_Constr arg_name_ arg_pats_ = T_Pattern (return st20) where
   {-# NOINLINE st20 #-}
   st20 = let
      v19 :: T_Pattern_v19 
      v19 = \ (T_Pattern_vIn19 ) -> ( let
         _patsX23 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_pats_))
         (T_Patterns_vOut22 _patsIcopy _patsIpp _patsIppL) = inv_Patterns_s23 _patsX23 (T_Patterns_vIn22 )
         _lhsOpp :: PP_Doc
         _lhsOpp = rule12 _patsIppL arg_name_
         _copy = rule13 _patsIcopy arg_name_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule14 _copy
         __result_ = T_Pattern_vOut19 _lhsOcopy _lhsOpp
         in __result_ )
     in C_Pattern_s20 v19
   {-# INLINE rule12 #-}
   {-# LINE 44 "./src-ag/AbstractSyntaxDump.ag" #-}
   rule12 = \ ((_patsIppL) :: [PP_Doc]) name_ ->
                                                              {-# LINE 44 "./src-ag/AbstractSyntaxDump.ag" #-}
                                                              ppNestInfo ["Pattern","Constr"] [pp name_] [ppF "pats" $ ppVList _patsIppL] []
                                                              {-# LINE 468 "dist/build/AbstractSyntaxDump.hs"#-}
   {-# INLINE rule13 #-}
   rule13 = \ ((_patsIcopy) :: Patterns) name_ ->
     Constr name_ _patsIcopy
   {-# INLINE rule14 #-}
   rule14 = \ _copy ->
     _copy
{-# NOINLINE sem_Pattern_Product #-}
sem_Pattern_Product :: (Pos) -> T_Patterns  -> T_Pattern 
sem_Pattern_Product arg_pos_ arg_pats_ = T_Pattern (return st20) where
   {-# NOINLINE st20 #-}
   st20 = let
      v19 :: T_Pattern_v19 
      v19 = \ (T_Pattern_vIn19 ) -> ( let
         _patsX23 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_pats_))
         (T_Patterns_vOut22 _patsIcopy _patsIpp _patsIppL) = inv_Patterns_s23 _patsX23 (T_Patterns_vIn22 )
         _lhsOpp :: PP_Doc
         _lhsOpp = rule15 _patsIppL arg_pos_
         _copy = rule16 _patsIcopy arg_pos_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule17 _copy
         __result_ = T_Pattern_vOut19 _lhsOcopy _lhsOpp
         in __result_ )
     in C_Pattern_s20 v19
   {-# INLINE rule15 #-}
   {-# LINE 45 "./src-ag/AbstractSyntaxDump.ag" #-}
   rule15 = \ ((_patsIppL) :: [PP_Doc]) pos_ ->
                                                              {-# LINE 45 "./src-ag/AbstractSyntaxDump.ag" #-}
                                                              ppNestInfo ["Pattern","Product"] [ppShow pos_] [ppF "pats" $ ppVList _patsIppL] []
                                                              {-# LINE 497 "dist/build/AbstractSyntaxDump.hs"#-}
   {-# INLINE rule16 #-}
   rule16 = \ ((_patsIcopy) :: Patterns) pos_ ->
     Product pos_ _patsIcopy
   {-# INLINE rule17 #-}
   rule17 = \ _copy ->
     _copy
{-# NOINLINE sem_Pattern_Alias #-}
sem_Pattern_Alias :: (Identifier) -> (Identifier) -> T_Pattern  -> T_Pattern 
sem_Pattern_Alias arg_field_ arg_attr_ arg_pat_ = T_Pattern (return st20) where
   {-# NOINLINE st20 #-}
   st20 = let
      v19 :: T_Pattern_v19 
      v19 = \ (T_Pattern_vIn19 ) -> ( let
         _patX20 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pat_))
         (T_Pattern_vOut19 _patIcopy _patIpp) = inv_Pattern_s20 _patX20 (T_Pattern_vIn19 )
         _lhsOpp :: PP_Doc
         _lhsOpp = rule18 _patIpp arg_attr_ arg_field_
         _copy = rule19 _patIcopy arg_attr_ arg_field_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule20 _copy
         __result_ = T_Pattern_vOut19 _lhsOcopy _lhsOpp
         in __result_ )
     in C_Pattern_s20 v19
   {-# INLINE rule18 #-}
   {-# LINE 46 "./src-ag/AbstractSyntaxDump.ag" #-}
   rule18 = \ ((_patIpp) :: PP_Doc) attr_ field_ ->
                                                              {-# LINE 46 "./src-ag/AbstractSyntaxDump.ag" #-}
                                                              ppNestInfo ["Pattern","Alias"] [pp field_, pp attr_] [ppF "pat" $ _patIpp] []
                                                              {-# LINE 526 "dist/build/AbstractSyntaxDump.hs"#-}
   {-# INLINE rule19 #-}
   rule19 = \ ((_patIcopy) :: Pattern) attr_ field_ ->
     Alias field_ attr_ _patIcopy
   {-# INLINE rule20 #-}
   rule20 = \ _copy ->
     _copy
{-# NOINLINE sem_Pattern_Irrefutable #-}
sem_Pattern_Irrefutable :: T_Pattern  -> T_Pattern 
sem_Pattern_Irrefutable arg_pat_ = T_Pattern (return st20) where
   {-# NOINLINE st20 #-}
   st20 = let
      v19 :: T_Pattern_v19 
      v19 = \ (T_Pattern_vIn19 ) -> ( let
         _patX20 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pat_))
         (T_Pattern_vOut19 _patIcopy _patIpp) = inv_Pattern_s20 _patX20 (T_Pattern_vIn19 )
         _lhsOpp :: PP_Doc
         _lhsOpp = rule21 _patIpp
         _copy = rule22 _patIcopy
         _lhsOcopy :: Pattern
         _lhsOcopy = rule23 _copy
         __result_ = T_Pattern_vOut19 _lhsOcopy _lhsOpp
         in __result_ )
     in C_Pattern_s20 v19
   {-# INLINE rule21 #-}
   rule21 = \ ((_patIpp) :: PP_Doc) ->
     _patIpp
   {-# INLINE rule22 #-}
   rule22 = \ ((_patIcopy) :: Pattern) ->
     Irrefutable _patIcopy
   {-# INLINE rule23 #-}
   rule23 = \ _copy ->
     _copy
{-# NOINLINE sem_Pattern_Underscore #-}
sem_Pattern_Underscore :: (Pos) -> T_Pattern 
sem_Pattern_Underscore arg_pos_ = T_Pattern (return st20) where
   {-# NOINLINE st20 #-}
   st20 = let
      v19 :: T_Pattern_v19 
      v19 = \ (T_Pattern_vIn19 ) -> ( let
         _lhsOpp :: PP_Doc
         _lhsOpp = rule24 arg_pos_
         _copy = rule25 arg_pos_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule26 _copy
         __result_ = T_Pattern_vOut19 _lhsOcopy _lhsOpp
         in __result_ )
     in C_Pattern_s20 v19
   {-# INLINE rule24 #-}
   {-# LINE 47 "./src-ag/AbstractSyntaxDump.ag" #-}
   rule24 = \ pos_ ->
                                                      {-# LINE 47 "./src-ag/AbstractSyntaxDump.ag" #-}
                                                      ppNestInfo ["Pattern","Underscore"] [ppShow pos_] [] []
                                                      {-# LINE 579 "dist/build/AbstractSyntaxDump.hs"#-}
   {-# INLINE rule25 #-}
   rule25 = \ pos_ ->
     Underscore pos_
   {-# INLINE rule26 #-}
   rule26 = \ _copy ->
     _copy

-- Patterns ----------------------------------------------------
-- wrapper
data Inh_Patterns  = Inh_Patterns {  }
data Syn_Patterns  = Syn_Patterns { copy_Syn_Patterns :: (Patterns), pp_Syn_Patterns :: (PP_Doc), ppL_Syn_Patterns :: ([PP_Doc]) }
{-# INLINABLE wrap_Patterns #-}
wrap_Patterns :: T_Patterns  -> Inh_Patterns  -> (Syn_Patterns )
wrap_Patterns (T_Patterns act) (Inh_Patterns ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Patterns_vIn22 
        (T_Patterns_vOut22 _lhsOcopy _lhsOpp _lhsOppL) <- return (inv_Patterns_s23 sem arg)
        return (Syn_Patterns _lhsOcopy _lhsOpp _lhsOppL)
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
data T_Patterns_vOut22  = T_Patterns_vOut22 (Patterns) (PP_Doc) ([PP_Doc])
{-# NOINLINE sem_Patterns_Cons #-}
sem_Patterns_Cons :: T_Pattern  -> T_Patterns  -> T_Patterns 
sem_Patterns_Cons arg_hd_ arg_tl_ = T_Patterns (return st23) where
   {-# NOINLINE st23 #-}
   st23 = let
      v22 :: T_Patterns_v22 
      v22 = \ (T_Patterns_vIn22 ) -> ( let
         _hdX20 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_hd_))
         _tlX23 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_tl_))
         (T_Pattern_vOut19 _hdIcopy _hdIpp) = inv_Pattern_s20 _hdX20 (T_Pattern_vIn19 )
         (T_Patterns_vOut22 _tlIcopy _tlIpp _tlIppL) = inv_Patterns_s23 _tlX23 (T_Patterns_vIn22 )
         _lhsOppL :: [PP_Doc]
         _lhsOppL = rule27 _hdIpp _tlIppL
         _lhsOpp :: PP_Doc
         _lhsOpp = rule28 _hdIpp _tlIpp
         _copy = rule29 _hdIcopy _tlIcopy
         _lhsOcopy :: Patterns
         _lhsOcopy = rule30 _copy
         __result_ = T_Patterns_vOut22 _lhsOcopy _lhsOpp _lhsOppL
         in __result_ )
     in C_Patterns_s23 v22
   {-# INLINE rule27 #-}
   {-# LINE 55 "./src-ag/AbstractSyntaxDump.ag" #-}
   rule27 = \ ((_hdIpp) :: PP_Doc) ((_tlIppL) :: [PP_Doc]) ->
                                                                                  {-# LINE 55 "./src-ag/AbstractSyntaxDump.ag" #-}
                                                                                  _hdIpp : _tlIppL
                                                                                  {-# LINE 643 "dist/build/AbstractSyntaxDump.hs"#-}
   {-# INLINE rule28 #-}
   rule28 = \ ((_hdIpp) :: PP_Doc) ((_tlIpp) :: PP_Doc) ->
     _hdIpp >-< _tlIpp
   {-# INLINE rule29 #-}
   rule29 = \ ((_hdIcopy) :: Pattern) ((_tlIcopy) :: Patterns) ->
     (:) _hdIcopy _tlIcopy
   {-# INLINE rule30 #-}
   rule30 = \ _copy ->
     _copy
{-# NOINLINE sem_Patterns_Nil #-}
sem_Patterns_Nil ::  T_Patterns 
sem_Patterns_Nil  = T_Patterns (return st23) where
   {-# NOINLINE st23 #-}
   st23 = let
      v22 :: T_Patterns_v22 
      v22 = \ (T_Patterns_vIn22 ) -> ( let
         _lhsOppL :: [PP_Doc]
         _lhsOppL = rule31  ()
         _lhsOpp :: PP_Doc
         _lhsOpp = rule32  ()
         _copy = rule33  ()
         _lhsOcopy :: Patterns
         _lhsOcopy = rule34 _copy
         __result_ = T_Patterns_vOut22 _lhsOcopy _lhsOpp _lhsOppL
         in __result_ )
     in C_Patterns_s23 v22
   {-# INLINE rule31 #-}
   {-# LINE 56 "./src-ag/AbstractSyntaxDump.ag" #-}
   rule31 = \  (_ :: ()) ->
                                                                                  {-# LINE 56 "./src-ag/AbstractSyntaxDump.ag" #-}
                                                                                  []
                                                                                  {-# LINE 675 "dist/build/AbstractSyntaxDump.hs"#-}
   {-# INLINE rule32 #-}
   rule32 = \  (_ :: ()) ->
     empty
   {-# INLINE rule33 #-}
   rule33 = \  (_ :: ()) ->
     []
   {-# INLINE rule34 #-}
   rule34 = \ _copy ->
     _copy

-- Production --------------------------------------------------
-- wrapper
data Inh_Production  = Inh_Production {  }
data Syn_Production  = Syn_Production { pp_Syn_Production :: (PP_Doc) }
{-# INLINABLE wrap_Production #-}
wrap_Production :: T_Production  -> Inh_Production  -> (Syn_Production )
wrap_Production (T_Production act) (Inh_Production ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Production_vIn25 
        (T_Production_vOut25 _lhsOpp) <- return (inv_Production_s26 sem arg)
        return (Syn_Production _lhsOpp)
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
data T_Production_vIn25  = T_Production_vIn25 
data T_Production_vOut25  = T_Production_vOut25 (PP_Doc)
{-# NOINLINE sem_Production_Production #-}
sem_Production_Production :: (ConstructorIdent) -> ([Identifier]) -> ([Type]) -> T_Children  -> T_Rules  -> T_TypeSigs  -> (MaybeMacro) -> T_Production 
sem_Production_Production arg_con_ _ _ arg_children_ arg_rules_ arg_typeSigs_ _ = T_Production (return st26) where
   {-# NOINLINE st26 #-}
   st26 = let
      v25 :: T_Production_v25 
      v25 = \ (T_Production_vIn25 ) -> ( let
         _childrenX5 = Control.Monad.Identity.runIdentity (attach_T_Children (arg_children_))
         _rulesX35 = Control.Monad.Identity.runIdentity (attach_T_Rules (arg_rules_))
         _typeSigsX41 = Control.Monad.Identity.runIdentity (attach_T_TypeSigs (arg_typeSigs_))
         (T_Children_vOut4 _childrenIpp _childrenIppL) = inv_Children_s5 _childrenX5 (T_Children_vIn4 )
         (T_Rules_vOut34 _rulesIpp _rulesIppL) = inv_Rules_s35 _rulesX35 (T_Rules_vIn34 )
         (T_TypeSigs_vOut40 _typeSigsIpp _typeSigsIppL) = inv_TypeSigs_s41 _typeSigsX41 (T_TypeSigs_vIn40 )
         _lhsOpp :: PP_Doc
         _lhsOpp = rule35 _childrenIppL _rulesIppL _typeSigsIppL arg_con_
         __result_ = T_Production_vOut25 _lhsOpp
         in __result_ )
     in C_Production_s26 v25
   {-# INLINE rule35 #-}
   {-# LINE 32 "./src-ag/AbstractSyntaxDump.ag" #-}
   rule35 = \ ((_childrenIppL) :: [PP_Doc]) ((_rulesIppL) :: [PP_Doc]) ((_typeSigsIppL) :: [PP_Doc]) con_ ->
                                                      {-# LINE 32 "./src-ag/AbstractSyntaxDump.ag" #-}
                                                      ppNestInfo ["Production","Production"] [pp con_] [ppF "children" $ ppVList _childrenIppL,ppF "rules" $ ppVList _rulesIppL,ppF "typeSigs" $ ppVList _typeSigsIppL] []
                                                      {-# LINE 739 "dist/build/AbstractSyntaxDump.hs"#-}

-- Productions -------------------------------------------------
-- wrapper
data Inh_Productions  = Inh_Productions {  }
data Syn_Productions  = Syn_Productions { pp_Syn_Productions :: (PP_Doc), ppL_Syn_Productions :: ([PP_Doc]) }
{-# INLINABLE wrap_Productions #-}
wrap_Productions :: T_Productions  -> Inh_Productions  -> (Syn_Productions )
wrap_Productions (T_Productions act) (Inh_Productions ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Productions_vIn28 
        (T_Productions_vOut28 _lhsOpp _lhsOppL) <- return (inv_Productions_s29 sem arg)
        return (Syn_Productions _lhsOpp _lhsOppL)
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
data T_Productions_vIn28  = T_Productions_vIn28 
data T_Productions_vOut28  = T_Productions_vOut28 (PP_Doc) ([PP_Doc])
{-# NOINLINE sem_Productions_Cons #-}
sem_Productions_Cons :: T_Production  -> T_Productions  -> T_Productions 
sem_Productions_Cons arg_hd_ arg_tl_ = T_Productions (return st29) where
   {-# NOINLINE st29 #-}
   st29 = let
      v28 :: T_Productions_v28 
      v28 = \ (T_Productions_vIn28 ) -> ( let
         _hdX26 = Control.Monad.Identity.runIdentity (attach_T_Production (arg_hd_))
         _tlX29 = Control.Monad.Identity.runIdentity (attach_T_Productions (arg_tl_))
         (T_Production_vOut25 _hdIpp) = inv_Production_s26 _hdX26 (T_Production_vIn25 )
         (T_Productions_vOut28 _tlIpp _tlIppL) = inv_Productions_s29 _tlX29 (T_Productions_vIn28 )
         _lhsOppL :: [PP_Doc]
         _lhsOppL = rule36 _hdIpp _tlIppL
         _lhsOpp :: PP_Doc
         _lhsOpp = rule37 _hdIpp _tlIpp
         __result_ = T_Productions_vOut28 _lhsOpp _lhsOppL
         in __result_ )
     in C_Productions_s29 v28
   {-# INLINE rule36 #-}
   {-# LINE 71 "./src-ag/AbstractSyntaxDump.ag" #-}
   rule36 = \ ((_hdIpp) :: PP_Doc) ((_tlIppL) :: [PP_Doc]) ->
                                                                                  {-# LINE 71 "./src-ag/AbstractSyntaxDump.ag" #-}
                                                                                  _hdIpp : _tlIppL
                                                                                  {-# LINE 794 "dist/build/AbstractSyntaxDump.hs"#-}
   {-# INLINE rule37 #-}
   rule37 = \ ((_hdIpp) :: PP_Doc) ((_tlIpp) :: PP_Doc) ->
     _hdIpp >-< _tlIpp
{-# NOINLINE sem_Productions_Nil #-}
sem_Productions_Nil ::  T_Productions 
sem_Productions_Nil  = T_Productions (return st29) where
   {-# NOINLINE st29 #-}
   st29 = let
      v28 :: T_Productions_v28 
      v28 = \ (T_Productions_vIn28 ) -> ( let
         _lhsOppL :: [PP_Doc]
         _lhsOppL = rule38  ()
         _lhsOpp :: PP_Doc
         _lhsOpp = rule39  ()
         __result_ = T_Productions_vOut28 _lhsOpp _lhsOppL
         in __result_ )
     in C_Productions_s29 v28
   {-# INLINE rule38 #-}
   {-# LINE 72 "./src-ag/AbstractSyntaxDump.ag" #-}
   rule38 = \  (_ :: ()) ->
                                                                                  {-# LINE 72 "./src-ag/AbstractSyntaxDump.ag" #-}
                                                                                  []
                                                                                  {-# LINE 817 "dist/build/AbstractSyntaxDump.hs"#-}
   {-# INLINE rule39 #-}
   rule39 = \  (_ :: ()) ->
     empty

-- Rule --------------------------------------------------------
-- wrapper
data Inh_Rule  = Inh_Rule {  }
data Syn_Rule  = Syn_Rule { pp_Syn_Rule :: (PP_Doc) }
{-# INLINABLE wrap_Rule #-}
wrap_Rule :: T_Rule  -> Inh_Rule  -> (Syn_Rule )
wrap_Rule (T_Rule act) (Inh_Rule ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Rule_vIn31 
        (T_Rule_vOut31 _lhsOpp) <- return (inv_Rule_s32 sem arg)
        return (Syn_Rule _lhsOpp)
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
data T_Rule_vOut31  = T_Rule_vOut31 (PP_Doc)
{-# NOINLINE sem_Rule_Rule #-}
sem_Rule_Rule :: (Maybe Identifier) -> T_Pattern  -> T_Expression  -> (Bool) -> (String) -> (Bool) -> (Bool) -> (Bool) -> (Maybe Error) -> (Bool) -> T_Rule 
sem_Rule_Rule _ arg_pattern_ arg_rhs_ arg_owrt_ arg_origin_ _ _ _ _ _ = T_Rule (return st32) where
   {-# NOINLINE st32 #-}
   st32 = let
      v31 :: T_Rule_v31 
      v31 = \ (T_Rule_vIn31 ) -> ( let
         _patternX20 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pattern_))
         _rhsX8 = Control.Monad.Identity.runIdentity (attach_T_Expression (arg_rhs_))
         (T_Pattern_vOut19 _patternIcopy _patternIpp) = inv_Pattern_s20 _patternX20 (T_Pattern_vIn19 )
         (T_Expression_vOut7 _rhsIpp) = inv_Expression_s8 _rhsX8 (T_Expression_vIn7 )
         _lhsOpp :: PP_Doc
         _lhsOpp = rule40 _patternIpp _rhsIpp arg_origin_ arg_owrt_
         __result_ = T_Rule_vOut31 _lhsOpp
         in __result_ )
     in C_Rule_s32 v31
   {-# INLINE rule40 #-}
   {-# LINE 38 "./src-ag/AbstractSyntaxDump.ag" #-}
   rule40 = \ ((_patternIpp) :: PP_Doc) ((_rhsIpp) :: PP_Doc) origin_ owrt_ ->
                                                              {-# LINE 38 "./src-ag/AbstractSyntaxDump.ag" #-}
                                                              ppNestInfo ["Rule","Rule"] [ppShow owrt_, pp origin_] [ppF "pattern" $ _patternIpp, ppF "rhs" $ _rhsIpp] []
                                                              {-# LINE 873 "dist/build/AbstractSyntaxDump.hs"#-}

-- Rules -------------------------------------------------------
-- wrapper
data Inh_Rules  = Inh_Rules {  }
data Syn_Rules  = Syn_Rules { pp_Syn_Rules :: (PP_Doc), ppL_Syn_Rules :: ([PP_Doc]) }
{-# INLINABLE wrap_Rules #-}
wrap_Rules :: T_Rules  -> Inh_Rules  -> (Syn_Rules )
wrap_Rules (T_Rules act) (Inh_Rules ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Rules_vIn34 
        (T_Rules_vOut34 _lhsOpp _lhsOppL) <- return (inv_Rules_s35 sem arg)
        return (Syn_Rules _lhsOpp _lhsOppL)
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
data T_Rules_vOut34  = T_Rules_vOut34 (PP_Doc) ([PP_Doc])
{-# NOINLINE sem_Rules_Cons #-}
sem_Rules_Cons :: T_Rule  -> T_Rules  -> T_Rules 
sem_Rules_Cons arg_hd_ arg_tl_ = T_Rules (return st35) where
   {-# NOINLINE st35 #-}
   st35 = let
      v34 :: T_Rules_v34 
      v34 = \ (T_Rules_vIn34 ) -> ( let
         _hdX32 = Control.Monad.Identity.runIdentity (attach_T_Rule (arg_hd_))
         _tlX35 = Control.Monad.Identity.runIdentity (attach_T_Rules (arg_tl_))
         (T_Rule_vOut31 _hdIpp) = inv_Rule_s32 _hdX32 (T_Rule_vIn31 )
         (T_Rules_vOut34 _tlIpp _tlIppL) = inv_Rules_s35 _tlX35 (T_Rules_vIn34 )
         _lhsOppL :: [PP_Doc]
         _lhsOppL = rule41 _hdIpp _tlIppL
         _lhsOpp :: PP_Doc
         _lhsOpp = rule42 _hdIpp _tlIpp
         __result_ = T_Rules_vOut34 _lhsOpp _lhsOppL
         in __result_ )
     in C_Rules_s35 v34
   {-# INLINE rule41 #-}
   {-# LINE 63 "./src-ag/AbstractSyntaxDump.ag" #-}
   rule41 = \ ((_hdIpp) :: PP_Doc) ((_tlIppL) :: [PP_Doc]) ->
                                                                                  {-# LINE 63 "./src-ag/AbstractSyntaxDump.ag" #-}
                                                                                  _hdIpp : _tlIppL
                                                                                  {-# LINE 928 "dist/build/AbstractSyntaxDump.hs"#-}
   {-# INLINE rule42 #-}
   rule42 = \ ((_hdIpp) :: PP_Doc) ((_tlIpp) :: PP_Doc) ->
     _hdIpp >-< _tlIpp
{-# NOINLINE sem_Rules_Nil #-}
sem_Rules_Nil ::  T_Rules 
sem_Rules_Nil  = T_Rules (return st35) where
   {-# NOINLINE st35 #-}
   st35 = let
      v34 :: T_Rules_v34 
      v34 = \ (T_Rules_vIn34 ) -> ( let
         _lhsOppL :: [PP_Doc]
         _lhsOppL = rule43  ()
         _lhsOpp :: PP_Doc
         _lhsOpp = rule44  ()
         __result_ = T_Rules_vOut34 _lhsOpp _lhsOppL
         in __result_ )
     in C_Rules_s35 v34
   {-# INLINE rule43 #-}
   {-# LINE 64 "./src-ag/AbstractSyntaxDump.ag" #-}
   rule43 = \  (_ :: ()) ->
                                                                                  {-# LINE 64 "./src-ag/AbstractSyntaxDump.ag" #-}
                                                                                  []
                                                                                  {-# LINE 951 "dist/build/AbstractSyntaxDump.hs"#-}
   {-# INLINE rule44 #-}
   rule44 = \  (_ :: ()) ->
     empty

-- TypeSig -----------------------------------------------------
-- wrapper
data Inh_TypeSig  = Inh_TypeSig {  }
data Syn_TypeSig  = Syn_TypeSig { pp_Syn_TypeSig :: (PP_Doc) }
{-# INLINABLE wrap_TypeSig #-}
wrap_TypeSig :: T_TypeSig  -> Inh_TypeSig  -> (Syn_TypeSig )
wrap_TypeSig (T_TypeSig act) (Inh_TypeSig ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_TypeSig_vIn37 
        (T_TypeSig_vOut37 _lhsOpp) <- return (inv_TypeSig_s38 sem arg)
        return (Syn_TypeSig _lhsOpp)
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
data T_TypeSig_vOut37  = T_TypeSig_vOut37 (PP_Doc)
{-# NOINLINE sem_TypeSig_TypeSig #-}
sem_TypeSig_TypeSig :: (Identifier) -> (Type) -> T_TypeSig 
sem_TypeSig_TypeSig arg_name_ arg_tp_ = T_TypeSig (return st38) where
   {-# NOINLINE st38 #-}
   st38 = let
      v37 :: T_TypeSig_v37 
      v37 = \ (T_TypeSig_vIn37 ) -> ( let
         _lhsOpp :: PP_Doc
         _lhsOpp = rule45 arg_name_ arg_tp_
         __result_ = T_TypeSig_vOut37 _lhsOpp
         in __result_ )
     in C_TypeSig_s38 v37
   {-# INLINE rule45 #-}
   {-# LINE 41 "./src-ag/AbstractSyntaxDump.ag" #-}
   rule45 = \ name_ tp_ ->
                                                              {-# LINE 41 "./src-ag/AbstractSyntaxDump.ag" #-}
                                                              ppNestInfo ["TypeSig","TypeSig"] [pp name_, ppShow tp_] [] []
                                                              {-# LINE 1003 "dist/build/AbstractSyntaxDump.hs"#-}

-- TypeSigs ----------------------------------------------------
-- wrapper
data Inh_TypeSigs  = Inh_TypeSigs {  }
data Syn_TypeSigs  = Syn_TypeSigs { pp_Syn_TypeSigs :: (PP_Doc), ppL_Syn_TypeSigs :: ([PP_Doc]) }
{-# INLINABLE wrap_TypeSigs #-}
wrap_TypeSigs :: T_TypeSigs  -> Inh_TypeSigs  -> (Syn_TypeSigs )
wrap_TypeSigs (T_TypeSigs act) (Inh_TypeSigs ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_TypeSigs_vIn40 
        (T_TypeSigs_vOut40 _lhsOpp _lhsOppL) <- return (inv_TypeSigs_s41 sem arg)
        return (Syn_TypeSigs _lhsOpp _lhsOppL)
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
data T_TypeSigs_vOut40  = T_TypeSigs_vOut40 (PP_Doc) ([PP_Doc])
{-# NOINLINE sem_TypeSigs_Cons #-}
sem_TypeSigs_Cons :: T_TypeSig  -> T_TypeSigs  -> T_TypeSigs 
sem_TypeSigs_Cons arg_hd_ arg_tl_ = T_TypeSigs (return st41) where
   {-# NOINLINE st41 #-}
   st41 = let
      v40 :: T_TypeSigs_v40 
      v40 = \ (T_TypeSigs_vIn40 ) -> ( let
         _hdX38 = Control.Monad.Identity.runIdentity (attach_T_TypeSig (arg_hd_))
         _tlX41 = Control.Monad.Identity.runIdentity (attach_T_TypeSigs (arg_tl_))
         (T_TypeSig_vOut37 _hdIpp) = inv_TypeSig_s38 _hdX38 (T_TypeSig_vIn37 )
         (T_TypeSigs_vOut40 _tlIpp _tlIppL) = inv_TypeSigs_s41 _tlX41 (T_TypeSigs_vIn40 )
         _lhsOppL :: [PP_Doc]
         _lhsOppL = rule46 _hdIpp _tlIppL
         _lhsOpp :: PP_Doc
         _lhsOpp = rule47 _hdIpp _tlIpp
         __result_ = T_TypeSigs_vOut40 _lhsOpp _lhsOppL
         in __result_ )
     in C_TypeSigs_s41 v40
   {-# INLINE rule46 #-}
   {-# LINE 59 "./src-ag/AbstractSyntaxDump.ag" #-}
   rule46 = \ ((_hdIpp) :: PP_Doc) ((_tlIppL) :: [PP_Doc]) ->
                                                                                  {-# LINE 59 "./src-ag/AbstractSyntaxDump.ag" #-}
                                                                                  _hdIpp : _tlIppL
                                                                                  {-# LINE 1058 "dist/build/AbstractSyntaxDump.hs"#-}
   {-# INLINE rule47 #-}
   rule47 = \ ((_hdIpp) :: PP_Doc) ((_tlIpp) :: PP_Doc) ->
     _hdIpp >-< _tlIpp
{-# NOINLINE sem_TypeSigs_Nil #-}
sem_TypeSigs_Nil ::  T_TypeSigs 
sem_TypeSigs_Nil  = T_TypeSigs (return st41) where
   {-# NOINLINE st41 #-}
   st41 = let
      v40 :: T_TypeSigs_v40 
      v40 = \ (T_TypeSigs_vIn40 ) -> ( let
         _lhsOppL :: [PP_Doc]
         _lhsOppL = rule48  ()
         _lhsOpp :: PP_Doc
         _lhsOpp = rule49  ()
         __result_ = T_TypeSigs_vOut40 _lhsOpp _lhsOppL
         in __result_ )
     in C_TypeSigs_s41 v40
   {-# INLINE rule48 #-}
   {-# LINE 60 "./src-ag/AbstractSyntaxDump.ag" #-}
   rule48 = \  (_ :: ()) ->
                                                                                  {-# LINE 60 "./src-ag/AbstractSyntaxDump.ag" #-}
                                                                                  []
                                                                                  {-# LINE 1081 "dist/build/AbstractSyntaxDump.hs"#-}
   {-# INLINE rule49 #-}
   rule49 = \  (_ :: ()) ->
     empty
