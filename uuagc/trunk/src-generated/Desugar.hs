{-# LANGUAGE Rank2Types, GADTs #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Desugar where
{-# LINE 14 "./src-ag/Desugar.ag" #-}

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Map(Map)
import qualified Data.Sequence as Seq
import Data.Sequence(Seq,(><))
import UU.Scanner.Position(Pos(..))
import Data.Maybe
import Data.List(intersperse)

import AbstractSyntax
import ErrorMessages
import Options
import HsToken
import HsTokenScanner
import TokenDef
import CommonTypes
{-# LINE 25 "dist/build/Desugar.hs" #-}

{-# LINE 2 "./src-ag/AbstractSyntax.ag" #-}

-- AbstractSyntax.ag imports
import Data.Set(Set)
import Data.Map(Map)
import Patterns    (Pattern(..),Patterns)
import Expression  (Expression(..))
import Macro --marcos
import CommonTypes
import ErrorMessages
{-# LINE 37 "dist/build/Desugar.hs" #-}

{-# LINE 2 "./src-ag/Patterns.ag" #-}

-- Patterns.ag imports
import UU.Scanner.Position(Pos)
import CommonTypes (ConstructorIdent,Identifier)
{-# LINE 44 "dist/build/Desugar.hs" #-}

{-# LINE 2 "./src-ag/Expression.ag" #-}

import UU.Scanner.Position(Pos)
import HsToken
{-# LINE 50 "dist/build/Desugar.hs" #-}

{-# LINE 2 "./src-ag/HsToken.ag" #-}

import CommonTypes
import UU.Scanner.Position(Pos)
{-# LINE 56 "dist/build/Desugar.hs" #-}
import Control.Monad.Identity (Identity)
import qualified Control.Monad.Identity
{-# LINE 98 "./src-ag/Desugar.ag" #-}

addl :: Int -> Pos -> Pos
addl n (Pos l c f) = Pos (l+n) c f
{-# LINE 63 "dist/build/Desugar.hs" #-}

{-# LINE 133 "./src-ag/Desugar.ag" #-}

maybeError :: a -> Error -> Maybe a -> (a, Seq Error)
maybeError def err mb
  = maybe (def, Seq.singleton err) (\r -> (r, Seq.empty)) mb

findField :: Identifier -> Identifier -> [(Identifier,Identifier)] -> Maybe Identifier
findField fld attr list
  | fld == _FIRST = f list
  | fld == _LAST  = f (reverse list)
  | otherwise     = Just fld
  where
    f = lookup attr
{-# LINE 78 "dist/build/Desugar.hs" #-}

{-# LINE 204 "./src-ag/Desugar.ag" #-}

mergeAttributes :: AttrMap -> AttrMap -> AttrMap
mergeAttributes = Map.unionWith $ Map.unionWith $ Set.union
{-# LINE 84 "dist/build/Desugar.hs" #-}

{-# LINE 251 "./src-ag/Desugar.ag" #-}

desugarExprs :: Options -> NontermIdent -> ConstructorIdent ->
                [(Identifier, Identifier)] -> [(Identifier, Identifier)] ->
                Seq Error -> [Expression] -> (Seq Error, [Expression])
desugarExprs options nt con childInhs childSyns
  = mapAccum (desugarExpr options nt con childInhs childSyns)
  where mapAccum f e = foldr (\x (e0,xs) -> let (e1,x') = f e0 x in (e1, x:xs)) (e, [])

desugarExpr :: Options -> NontermIdent -> ConstructorIdent ->
               [(Identifier, Identifier)] -> [(Identifier, Identifier)] ->
               Seq Error -> Expression -> (Seq Error, Expression)
desugarExpr options nt con childInhs childSyns errs expr
  = (errs Seq.>< errors_Syn_Expression syn, output_Syn_Expression syn)
  where
    inh = Inh_Expression { childInhs_Inh_Expression = childInhs
                         , childSyns_Inh_Expression = childSyns
                         , con_Inh_Expression       = con
                         , nt_Inh_Expression        = nt
                         , options_Inh_Expression   = options
                         , ruleDescr_Inh_Expression = "augment-rule"
                         }
    sem = sem_Expression expr
    syn = wrap_Expression sem inh
{-# LINE 110 "dist/build/Desugar.hs" #-}

{-# LINE 294 "./src-ag/Desugar.ag" #-}

addLateAttr :: Options -> String -> Attributes
addLateAttr options mainName
  | kennedyWarren options && lateHigherOrderBinding options =
      let tp = lateBindingType mainName
      in Map.singleton idLateBindingAttr tp
  | otherwise = Map.empty
{-# LINE 120 "dist/build/Desugar.hs" #-}
-- Child -------------------------------------------------------
-- wrapper
data Inh_Child  = Inh_Child { inhMap_Inh_Child :: !(Map Identifier Attributes), mainName_Inh_Child :: !(String), options_Inh_Child :: !(Options), synMap_Inh_Child :: !(Map Identifier Attributes) }
data Syn_Child  = Syn_Child { childInhs_Syn_Child :: !([(Identifier, Identifier)]), childSyns_Syn_Child :: !([(Identifier, Identifier)]), output_Syn_Child :: !(Child) }
{-# INLINABLE wrap_Child #-}
wrap_Child :: T_Child  -> Inh_Child  -> (Syn_Child )
wrap_Child !(T_Child act) !(Inh_Child _lhsIinhMap _lhsImainName _lhsIoptions _lhsIsynMap) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_Child_vIn1 _lhsIinhMap _lhsImainName _lhsIoptions _lhsIsynMap
        !(T_Child_vOut1 _lhsOchildInhs _lhsOchildSyns _lhsOoutput) <- return (inv_Child_s2 sem arg)
        return (Syn_Child _lhsOchildInhs _lhsOchildSyns _lhsOoutput)
   )

-- cata
{-# INLINE sem_Child #-}
sem_Child :: Child  -> T_Child 
sem_Child ( Child !name_ !tp_ !kind_ ) = sem_Child_Child name_ tp_ kind_

-- semantic domain
newtype T_Child  = T_Child {
                           attach_T_Child :: Identity (T_Child_s2 )
                           }
newtype T_Child_s2  = C_Child_s2 {
                                 inv_Child_s2 :: (T_Child_v1 )
                                 }
data T_Child_s3  = C_Child_s3
type T_Child_v1  = (T_Child_vIn1 ) -> (T_Child_vOut1 )
data T_Child_vIn1  = T_Child_vIn1 (Map Identifier Attributes) (String) (Options) (Map Identifier Attributes)
data T_Child_vOut1  = T_Child_vOut1 ([(Identifier, Identifier)]) ([(Identifier, Identifier)]) (Child)
{-# NOINLINE sem_Child_Child #-}
sem_Child_Child :: (Identifier) -> (Type) -> (ChildKind) -> T_Child 
sem_Child_Child !arg_name_ !arg_tp_ !arg_kind_ = T_Child (return st2) where
   {-# NOINLINE st2 #-}
   !st2 = let
      v1 :: T_Child_v1 
      v1 = \ !(T_Child_vIn1 _lhsIinhMap _lhsImainName _lhsIoptions _lhsIsynMap) -> ( let
         _lhsOchildInhs :: [(Identifier, Identifier)]
         _lhsOchildInhs = rule0 _inh arg_name_
         _lhsOchildSyns :: [(Identifier, Identifier)]
         _lhsOchildSyns = rule1 _syn arg_name_
         _lhsOoutput :: Child
         _lhsOoutput = rule2 arg_kind_ arg_name_ arg_tp_
         _chnt = rule3 arg_name_ arg_tp_
         _inh = rule4 _chnt _lhsIinhMap
         _syn = rule5 _chnt _lhsIsynMap
         _output = rule6 arg_kind_ arg_name_ arg_tp_
         !__result_ = T_Child_vOut1 _lhsOchildInhs _lhsOchildSyns _lhsOoutput
         in __result_ )
     in C_Child_s2 v1
   {-# INLINE rule0 #-}
   {-# LINE 130 "./src-ag/Desugar.ag" #-}
   rule0 = \ _inh name_ ->
                        {-# LINE 130 "./src-ag/Desugar.ag" #-}
                        [(i, name_) | i <- Map.keys _inh     ]
                        {-# LINE 176 "dist/build/Desugar.hs"#-}
   {-# INLINE rule1 #-}
   {-# LINE 131 "./src-ag/Desugar.ag" #-}
   rule1 = \ _syn name_ ->
                        {-# LINE 131 "./src-ag/Desugar.ag" #-}
                        [(s, name_) | s <- Map.keys _syn     ]
                        {-# LINE 182 "dist/build/Desugar.hs"#-}
   {-# INLINE rule2 #-}
   {-# LINE 315 "./src-ag/Desugar.ag" #-}
   rule2 = \ kind_ name_ tp_ ->
                 {-# LINE 315 "./src-ag/Desugar.ag" #-}
                 Child name_ tp_ kind_
                 {-# LINE 188 "dist/build/Desugar.hs"#-}
   {-# INLINE rule3 #-}
   {-# LINE 19 "./src-ag/DistChildAttr.ag" #-}
   rule3 = \ name_ tp_ ->
                       {-# LINE 19 "./src-ag/DistChildAttr.ag" #-}
                       case tp_ of
                         NT nt _ _ -> nt
                         Self      -> error ("The type of child " ++ show name_ ++ " should not be a Self type.")
                         Haskell t -> identifier ""
                       {-# LINE 197 "dist/build/Desugar.hs"#-}
   {-# INLINE rule4 #-}
   {-# LINE 23 "./src-ag/DistChildAttr.ag" #-}
   rule4 = \ _chnt ((_lhsIinhMap) :: Map Identifier Attributes) ->
                      {-# LINE 23 "./src-ag/DistChildAttr.ag" #-}
                      Map.findWithDefault Map.empty _chnt     _lhsIinhMap
                      {-# LINE 203 "dist/build/Desugar.hs"#-}
   {-# INLINE rule5 #-}
   {-# LINE 24 "./src-ag/DistChildAttr.ag" #-}
   rule5 = \ _chnt ((_lhsIsynMap) :: Map Identifier Attributes) ->
                      {-# LINE 24 "./src-ag/DistChildAttr.ag" #-}
                      Map.findWithDefault Map.empty _chnt     _lhsIsynMap
                      {-# LINE 209 "dist/build/Desugar.hs"#-}
   {-# INLINE rule6 #-}
   rule6 = \ kind_ name_ tp_ ->
     Child name_ tp_ kind_

-- Children ----------------------------------------------------
-- wrapper
data Inh_Children  = Inh_Children { inhMap_Inh_Children :: !(Map Identifier Attributes), mainName_Inh_Children :: !(String), options_Inh_Children :: !(Options), synMap_Inh_Children :: !(Map Identifier Attributes) }
data Syn_Children  = Syn_Children { childInhs_Syn_Children :: !([(Identifier, Identifier)]), childSyns_Syn_Children :: !([(Identifier, Identifier)]), output_Syn_Children :: !(Children) }
{-# INLINABLE wrap_Children #-}
wrap_Children :: T_Children  -> Inh_Children  -> (Syn_Children )
wrap_Children !(T_Children act) !(Inh_Children _lhsIinhMap _lhsImainName _lhsIoptions _lhsIsynMap) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_Children_vIn4 _lhsIinhMap _lhsImainName _lhsIoptions _lhsIsynMap
        !(T_Children_vOut4 _lhsOchildInhs _lhsOchildSyns _lhsOoutput) <- return (inv_Children_s5 sem arg)
        return (Syn_Children _lhsOchildInhs _lhsOchildSyns _lhsOoutput)
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
data T_Children_vIn4  = T_Children_vIn4 (Map Identifier Attributes) (String) (Options) (Map Identifier Attributes)
data T_Children_vOut4  = T_Children_vOut4 ([(Identifier, Identifier)]) ([(Identifier, Identifier)]) (Children)
{-# NOINLINE sem_Children_Cons #-}
sem_Children_Cons :: T_Child  -> T_Children  -> T_Children 
sem_Children_Cons arg_hd_ arg_tl_ = T_Children (return st5) where
   {-# NOINLINE st5 #-}
   !st5 = let
      v4 :: T_Children_v4 
      v4 = \ !(T_Children_vIn4 _lhsIinhMap _lhsImainName _lhsIoptions _lhsIsynMap) -> ( let
         _hdX2 = Control.Monad.Identity.runIdentity (attach_T_Child (arg_hd_))
         _tlX5 = Control.Monad.Identity.runIdentity (attach_T_Children (arg_tl_))
         (T_Child_vOut1 _hdIchildInhs _hdIchildSyns _hdIoutput) = inv_Child_s2 _hdX2 (T_Child_vIn1 _hdOinhMap _hdOmainName _hdOoptions _hdOsynMap)
         (T_Children_vOut4 _tlIchildInhs _tlIchildSyns _tlIoutput) = inv_Children_s5 _tlX5 (T_Children_vIn4 _tlOinhMap _tlOmainName _tlOoptions _tlOsynMap)
         _lhsOchildInhs :: [(Identifier, Identifier)]
         _lhsOchildInhs = rule7 _hdIchildInhs _tlIchildInhs
         _lhsOchildSyns :: [(Identifier, Identifier)]
         _lhsOchildSyns = rule8 _hdIchildSyns _tlIchildSyns
         _output = rule9 _hdIoutput _tlIoutput
         _lhsOoutput :: Children
         _lhsOoutput = rule10 _output
         _hdOinhMap = rule11 _lhsIinhMap
         _hdOmainName = rule12 _lhsImainName
         _hdOoptions = rule13 _lhsIoptions
         _hdOsynMap = rule14 _lhsIsynMap
         _tlOinhMap = rule15 _lhsIinhMap
         _tlOmainName = rule16 _lhsImainName
         _tlOoptions = rule17 _lhsIoptions
         _tlOsynMap = rule18 _lhsIsynMap
         !__result_ = T_Children_vOut4 _lhsOchildInhs _lhsOchildSyns _lhsOoutput
         in __result_ )
     in C_Children_s5 v4
   {-# INLINE rule7 #-}
   rule7 = \ ((_hdIchildInhs) :: [(Identifier, Identifier)]) ((_tlIchildInhs) :: [(Identifier, Identifier)]) ->
     _hdIchildInhs ++ _tlIchildInhs
   {-# INLINE rule8 #-}
   rule8 = \ ((_hdIchildSyns) :: [(Identifier, Identifier)]) ((_tlIchildSyns) :: [(Identifier, Identifier)]) ->
     _hdIchildSyns ++ _tlIchildSyns
   {-# INLINE rule9 #-}
   rule9 = \ ((_hdIoutput) :: Child) ((_tlIoutput) :: Children) ->
     (:) _hdIoutput _tlIoutput
   {-# INLINE rule10 #-}
   rule10 = \ _output ->
     _output
   {-# INLINE rule11 #-}
   rule11 = \ ((_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# INLINE rule12 #-}
   rule12 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule13 #-}
   rule13 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule14 #-}
   rule14 = \ ((_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
   {-# INLINE rule15 #-}
   rule15 = \ ((_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# INLINE rule16 #-}
   rule16 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule17 #-}
   rule17 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule18 #-}
   rule18 = \ ((_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
{-# NOINLINE sem_Children_Nil #-}
sem_Children_Nil ::  T_Children 
sem_Children_Nil  = T_Children (return st5) where
   {-# NOINLINE st5 #-}
   !st5 = let
      v4 :: T_Children_v4 
      v4 = \ !(T_Children_vIn4 _lhsIinhMap _lhsImainName _lhsIoptions _lhsIsynMap) -> ( let
         _lhsOchildInhs :: [(Identifier, Identifier)]
         _lhsOchildInhs = rule19  ()
         _lhsOchildSyns :: [(Identifier, Identifier)]
         _lhsOchildSyns = rule20  ()
         _output = rule21  ()
         _lhsOoutput :: Children
         _lhsOoutput = rule22 _output
         !__result_ = T_Children_vOut4 _lhsOchildInhs _lhsOchildSyns _lhsOoutput
         in __result_ )
     in C_Children_s5 v4
   {-# INLINE rule19 #-}
   rule19 = \  (_ :: ()) ->
     []
   {-# INLINE rule20 #-}
   rule20 = \  (_ :: ()) ->
     []
   {-# INLINE rule21 #-}
   rule21 = \  (_ :: ()) ->
     []
   {-# INLINE rule22 #-}
   rule22 = \ _output ->
     _output

-- Expression --------------------------------------------------
-- wrapper
data Inh_Expression  = Inh_Expression { childInhs_Inh_Expression :: !([(Identifier, Identifier)]), childSyns_Inh_Expression :: !([(Identifier, Identifier)]), con_Inh_Expression :: !(ConstructorIdent), nt_Inh_Expression :: !(NontermIdent), options_Inh_Expression :: !(Options), ruleDescr_Inh_Expression :: !(String) }
data Syn_Expression  = Syn_Expression { errors_Syn_Expression :: !(Seq Error), output_Syn_Expression :: !(Expression) }
{-# INLINABLE wrap_Expression #-}
wrap_Expression :: T_Expression  -> Inh_Expression  -> (Syn_Expression )
wrap_Expression !(T_Expression act) !(Inh_Expression _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsInt _lhsIoptions _lhsIruleDescr) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_Expression_vIn7 _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsInt _lhsIoptions _lhsIruleDescr
        !(T_Expression_vOut7 _lhsOerrors _lhsOoutput) <- return (inv_Expression_s8 sem arg)
        return (Syn_Expression _lhsOerrors _lhsOoutput)
   )

-- cata
{-# INLINE sem_Expression #-}
sem_Expression :: Expression  -> T_Expression 
sem_Expression ( Expression !pos_ !tks_ ) = sem_Expression_Expression pos_ tks_

-- semantic domain
newtype T_Expression  = T_Expression {
                                     attach_T_Expression :: Identity (T_Expression_s8 )
                                     }
newtype T_Expression_s8  = C_Expression_s8 {
                                           inv_Expression_s8 :: (T_Expression_v7 )
                                           }
data T_Expression_s9  = C_Expression_s9
type T_Expression_v7  = (T_Expression_vIn7 ) -> (T_Expression_vOut7 )
data T_Expression_vIn7  = T_Expression_vIn7 ([(Identifier, Identifier)]) ([(Identifier, Identifier)]) (ConstructorIdent) (NontermIdent) (Options) (String)
data T_Expression_vOut7  = T_Expression_vOut7 (Seq Error) (Expression)
{-# NOINLINE sem_Expression_Expression #-}
sem_Expression_Expression :: (Pos) -> ([HsToken]) -> T_Expression 
sem_Expression_Expression !arg_pos_ !arg_tks_ = T_Expression (return st8) where
   {-# NOINLINE st8 #-}
   !st8 = let
      v7 :: T_Expression_v7 
      v7 = \ !(T_Expression_vIn7 _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsInt _lhsIoptions _lhsIruleDescr) -> ( let
         _lhsOerrors :: Seq Error
         (_tks',_lhsOerrors) = rule23 _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsInt _lhsIoptions _lhsIruleDescr arg_tks_
         _lhsOoutput :: Expression
         _lhsOoutput = rule24 _tks' arg_pos_
         _output = rule25 arg_pos_ arg_tks_
         !__result_ = T_Expression_vOut7 _lhsOerrors _lhsOoutput
         in __result_ )
     in C_Expression_s8 v7
   {-# INLINE rule23 #-}
   {-# LINE 49 "./src-ag/Desugar.ag" #-}
   rule23 = \ ((_lhsIchildInhs) :: [(Identifier, Identifier)]) ((_lhsIchildSyns) :: [(Identifier, Identifier)]) ((_lhsIcon) :: ConstructorIdent) ((_lhsInt) :: NontermIdent) ((_lhsIoptions) :: Options) ((_lhsIruleDescr) :: String) tks_ ->
                                 {-# LINE 49 "./src-ag/Desugar.ag" #-}
                                 let inh = Inh_HsTokensRoot { childInhs_Inh_HsTokensRoot     = _lhsIchildInhs
                                                            , childSyns_Inh_HsTokensRoot     = _lhsIchildSyns
                                                            , nt_Inh_HsTokensRoot            = _lhsInt
                                                            , con_Inh_HsTokensRoot           = _lhsIcon
                                                            , ruleDescr_Inh_HsTokensRoot     = _lhsIruleDescr
                                                            , useFieldIdent_Inh_HsTokensRoot = genUseTraces _lhsIoptions
                                                            }
                                     sem = sem_HsTokensRoot (HsTokensRoot tks_)
                                     syn = wrap_HsTokensRoot sem inh
                                 in (tks_Syn_HsTokensRoot syn, errors_Syn_HsTokensRoot syn)
                                 {-# LINE 398 "dist/build/Desugar.hs"#-}
   {-# INLINE rule24 #-}
   {-# LINE 59 "./src-ag/Desugar.ag" #-}
   rule24 = \ _tks' pos_ ->
                     {-# LINE 59 "./src-ag/Desugar.ag" #-}
                     Expression pos_ _tks'
                     {-# LINE 404 "dist/build/Desugar.hs"#-}
   {-# INLINE rule25 #-}
   rule25 = \ pos_ tks_ ->
     Expression pos_ tks_

-- Grammar -----------------------------------------------------
-- wrapper
data Inh_Grammar  = Inh_Grammar { forcedIrrefutables_Inh_Grammar :: !(AttrMap), mainName_Inh_Grammar :: !(String), options_Inh_Grammar :: !(Options) }
data Syn_Grammar  = Syn_Grammar { allAttributes_Syn_Grammar :: !(AttrMap), errors_Syn_Grammar :: !(Seq Error), output_Syn_Grammar :: !(Grammar) }
{-# INLINABLE wrap_Grammar #-}
wrap_Grammar :: T_Grammar  -> Inh_Grammar  -> (Syn_Grammar )
wrap_Grammar !(T_Grammar act) !(Inh_Grammar _lhsIforcedIrrefutables _lhsImainName _lhsIoptions) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_Grammar_vIn10 _lhsIforcedIrrefutables _lhsImainName _lhsIoptions
        !(T_Grammar_vOut10 _lhsOallAttributes _lhsOerrors _lhsOoutput) <- return (inv_Grammar_s11 sem arg)
        return (Syn_Grammar _lhsOallAttributes _lhsOerrors _lhsOoutput)
   )

-- cata
{-# INLINE sem_Grammar #-}
sem_Grammar :: Grammar  -> T_Grammar 
sem_Grammar ( Grammar !typeSyns_ !useMap_ !derivings_ !wrappers_ nonts_ !pragmas_ !manualAttrOrderMap_ !paramMap_ !contextMap_ !quantMap_ !uniqueMap_ !augmentsMap_ !aroundsMap_ !mergeMap_ ) = sem_Grammar_Grammar typeSyns_ useMap_ derivings_ wrappers_ ( sem_Nonterminals nonts_ ) pragmas_ manualAttrOrderMap_ paramMap_ contextMap_ quantMap_ uniqueMap_ augmentsMap_ aroundsMap_ mergeMap_

-- semantic domain
newtype T_Grammar  = T_Grammar {
                               attach_T_Grammar :: Identity (T_Grammar_s11 )
                               }
newtype T_Grammar_s11  = C_Grammar_s11 {
                                       inv_Grammar_s11 :: (T_Grammar_v10 )
                                       }
data T_Grammar_s12  = C_Grammar_s12
type T_Grammar_v10  = (T_Grammar_vIn10 ) -> (T_Grammar_vOut10 )
data T_Grammar_vIn10  = T_Grammar_vIn10 (AttrMap) (String) (Options)
data T_Grammar_vOut10  = T_Grammar_vOut10 (AttrMap) (Seq Error) (Grammar)
{-# NOINLINE sem_Grammar_Grammar #-}
sem_Grammar_Grammar :: (TypeSyns) -> (UseMap) -> (Derivings) -> (Set NontermIdent) -> T_Nonterminals  -> (PragmaMap) -> (AttrOrderMap) -> (ParamMap) -> (ContextMap) -> (QuantMap) -> (UniqueMap) -> (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) -> (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) -> (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier, [Identifier], Expression)))) -> T_Grammar 
sem_Grammar_Grammar !arg_typeSyns_ !arg_useMap_ !arg_derivings_ !arg_wrappers_ arg_nonts_ !arg_pragmas_ !arg_manualAttrOrderMap_ !arg_paramMap_ !arg_contextMap_ !arg_quantMap_ !arg_uniqueMap_ !arg_augmentsMap_ !arg_aroundsMap_ !arg_mergeMap_ = T_Grammar (return st11) where
   {-# NOINLINE st11 #-}
   !st11 = let
      v10 :: T_Grammar_v10 
      v10 = \ !(T_Grammar_vIn10 _lhsIforcedIrrefutables _lhsImainName _lhsIoptions) -> ( let
         _nontsX26 = Control.Monad.Identity.runIdentity (attach_T_Nonterminals (arg_nonts_))
         (T_Nonterminals_vOut25 _nontsIallAttributes _nontsIaugmentsOut _nontsIerrors _nontsIinhMap' _nontsIoutput _nontsIsynMap') = inv_Nonterminals_s26 _nontsX26 (T_Nonterminals_vIn25 _nontsOaugmentsIn _nontsOforcedIrrefutables _nontsOinhMap _nontsOmainName _nontsOoptions _nontsOsynMap)
         _nontsOaugmentsIn = rule26 arg_augmentsMap_
         _lhsOoutput :: Grammar
         _lhsOoutput = rule27 _nontsIaugmentsOut _nontsIoutput arg_aroundsMap_ arg_contextMap_ arg_derivings_ arg_manualAttrOrderMap_ arg_mergeMap_ arg_paramMap_ arg_pragmas_ arg_quantMap_ arg_typeSyns_ arg_uniqueMap_ arg_useMap_ arg_wrappers_
         _nontsOinhMap = rule28 _nontsIinhMap'
         _nontsOsynMap = rule29 _nontsIsynMap'
         _lhsOallAttributes :: AttrMap
         _lhsOallAttributes = rule30 _nontsIallAttributes
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule31 _nontsIerrors
         _output = rule32 _nontsIoutput arg_aroundsMap_ arg_augmentsMap_ arg_contextMap_ arg_derivings_ arg_manualAttrOrderMap_ arg_mergeMap_ arg_paramMap_ arg_pragmas_ arg_quantMap_ arg_typeSyns_ arg_uniqueMap_ arg_useMap_ arg_wrappers_
         _nontsOforcedIrrefutables = rule33 _lhsIforcedIrrefutables
         _nontsOmainName = rule34 _lhsImainName
         _nontsOoptions = rule35 _lhsIoptions
         !__result_ = T_Grammar_vOut10 _lhsOallAttributes _lhsOerrors _lhsOoutput
         in __result_ )
     in C_Grammar_s11 v10
   {-# INLINE rule26 #-}
   {-# LINE 235 "./src-ag/Desugar.ag" #-}
   rule26 = \ augmentsMap_ ->
                           {-# LINE 235 "./src-ag/Desugar.ag" #-}
                           augmentsMap_
                           {-# LINE 469 "dist/build/Desugar.hs"#-}
   {-# INLINE rule27 #-}
   {-# LINE 319 "./src-ag/Desugar.ag" #-}
   rule27 = \ ((_nontsIaugmentsOut) :: Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) ((_nontsIoutput) :: Nonterminals) aroundsMap_ contextMap_ derivings_ manualAttrOrderMap_ mergeMap_ paramMap_ pragmas_ quantMap_ typeSyns_ uniqueMap_ useMap_ wrappers_ ->
                     {-# LINE 319 "./src-ag/Desugar.ag" #-}
                     Grammar typeSyns_
                             useMap_
                             derivings_
                             wrappers_
                             _nontsIoutput
                             pragmas_
                             manualAttrOrderMap_
                             paramMap_
                             contextMap_
                             quantMap_
                             uniqueMap_
                             _nontsIaugmentsOut
                             aroundsMap_
                             mergeMap_
                     {-# LINE 488 "dist/build/Desugar.hs"#-}
   {-# INLINE rule28 #-}
   {-# LINE 15 "./src-ag/DistChildAttr.ag" #-}
   rule28 = \ ((_nontsIinhMap') :: Map Identifier Attributes) ->
                             {-# LINE 15 "./src-ag/DistChildAttr.ag" #-}
                             _nontsIinhMap'
                             {-# LINE 494 "dist/build/Desugar.hs"#-}
   {-# INLINE rule29 #-}
   {-# LINE 16 "./src-ag/DistChildAttr.ag" #-}
   rule29 = \ ((_nontsIsynMap') :: Map Identifier Attributes) ->
                             {-# LINE 16 "./src-ag/DistChildAttr.ag" #-}
                             _nontsIsynMap'
                             {-# LINE 500 "dist/build/Desugar.hs"#-}
   {-# INLINE rule30 #-}
   rule30 = \ ((_nontsIallAttributes) :: AttrMap) ->
     _nontsIallAttributes
   {-# INLINE rule31 #-}
   rule31 = \ ((_nontsIerrors) :: Seq Error) ->
     _nontsIerrors
   {-# INLINE rule32 #-}
   rule32 = \ ((_nontsIoutput) :: Nonterminals) aroundsMap_ augmentsMap_ contextMap_ derivings_ manualAttrOrderMap_ mergeMap_ paramMap_ pragmas_ quantMap_ typeSyns_ uniqueMap_ useMap_ wrappers_ ->
     Grammar typeSyns_ useMap_ derivings_ wrappers_ _nontsIoutput pragmas_ manualAttrOrderMap_ paramMap_ contextMap_ quantMap_ uniqueMap_ augmentsMap_ aroundsMap_ mergeMap_
   {-# INLINE rule33 #-}
   rule33 = \ ((_lhsIforcedIrrefutables) :: AttrMap) ->
     _lhsIforcedIrrefutables
   {-# INLINE rule34 #-}
   rule34 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule35 #-}
   rule35 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions

-- HsToken -----------------------------------------------------
-- wrapper
data Inh_HsToken  = Inh_HsToken { addLines_Inh_HsToken :: !(Int), childInhs_Inh_HsToken :: !([(Identifier, Identifier)]), childSyns_Inh_HsToken :: !([(Identifier, Identifier)]), con_Inh_HsToken :: !(ConstructorIdent), nt_Inh_HsToken :: !(NontermIdent), ruleDescr_Inh_HsToken :: !(String), useFieldIdent_Inh_HsToken :: !(Bool) }
data Syn_HsToken  = Syn_HsToken { addLines_Syn_HsToken :: !(Int), errors_Syn_HsToken :: !(Seq Error), tks_Syn_HsToken :: !(HsToken) }
{-# INLINABLE wrap_HsToken #-}
wrap_HsToken :: T_HsToken  -> Inh_HsToken  -> (Syn_HsToken )
wrap_HsToken !(T_HsToken act) !(Inh_HsToken _lhsIaddLines _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsInt _lhsIruleDescr _lhsIuseFieldIdent) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_HsToken_vIn13 _lhsIaddLines _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsInt _lhsIruleDescr _lhsIuseFieldIdent
        !(T_HsToken_vOut13 _lhsOaddLines _lhsOerrors _lhsOtks) <- return (inv_HsToken_s14 sem arg)
        return (Syn_HsToken _lhsOaddLines _lhsOerrors _lhsOtks)
   )

-- cata
{-# NOINLINE sem_HsToken #-}
sem_HsToken :: HsToken  -> T_HsToken 
sem_HsToken ( AGLocal !var_ !pos_ !rdesc_ ) = sem_HsToken_AGLocal var_ pos_ rdesc_
sem_HsToken ( AGField !field_ !attr_ !pos_ !rdesc_ ) = sem_HsToken_AGField field_ attr_ pos_ rdesc_
sem_HsToken ( HsToken !value_ !pos_ ) = sem_HsToken_HsToken value_ pos_
sem_HsToken ( CharToken !value_ !pos_ ) = sem_HsToken_CharToken value_ pos_
sem_HsToken ( StrToken !value_ !pos_ ) = sem_HsToken_StrToken value_ pos_
sem_HsToken ( Err !mesg_ !pos_ ) = sem_HsToken_Err mesg_ pos_

-- semantic domain
newtype T_HsToken  = T_HsToken {
                               attach_T_HsToken :: Identity (T_HsToken_s14 )
                               }
newtype T_HsToken_s14  = C_HsToken_s14 {
                                       inv_HsToken_s14 :: (T_HsToken_v13 )
                                       }
data T_HsToken_s15  = C_HsToken_s15
type T_HsToken_v13  = (T_HsToken_vIn13 ) -> (T_HsToken_vOut13 )
data T_HsToken_vIn13  = T_HsToken_vIn13 (Int) ([(Identifier, Identifier)]) ([(Identifier, Identifier)]) (ConstructorIdent) (NontermIdent) (String) (Bool)
data T_HsToken_vOut13  = T_HsToken_vOut13 (Int) (Seq Error) (HsToken)
{-# NOINLINE sem_HsToken_AGLocal #-}
sem_HsToken_AGLocal :: (Identifier) -> (Pos) -> (Maybe String) -> T_HsToken 
sem_HsToken_AGLocal !arg_var_ !arg_pos_ _ = T_HsToken (return st14) where
   {-# NOINLINE st14 #-}
   !st14 = let
      v13 :: T_HsToken_v13 
      v13 = \ !(T_HsToken_vIn13 _lhsIaddLines _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsInt _lhsIruleDescr _lhsIuseFieldIdent) -> ( let
         _lhsOaddLines :: Int
         _lhsOaddLines = rule36 _lhsIaddLines _lhsIuseFieldIdent
         _tks = rule37 _lhsIaddLines _lhsIruleDescr _lhsIuseFieldIdent arg_pos_ arg_var_
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule38  ()
         _lhsOtks :: HsToken
         _lhsOtks = rule39 _tks
         !__result_ = T_HsToken_vOut13 _lhsOaddLines _lhsOerrors _lhsOtks
         in __result_ )
     in C_HsToken_s14 v13
   {-# INLINE rule36 #-}
   {-# LINE 74 "./src-ag/Desugar.ag" #-}
   rule36 = \ ((_lhsIaddLines) :: Int) ((_lhsIuseFieldIdent) :: Bool) ->
                       {-# LINE 74 "./src-ag/Desugar.ag" #-}
                       if _lhsIuseFieldIdent
                       then _lhsIaddLines + 1
                       else _lhsIaddLines
                       {-# LINE 579 "dist/build/Desugar.hs"#-}
   {-# INLINE rule37 #-}
   {-# LINE 77 "./src-ag/Desugar.ag" #-}
   rule37 = \ ((_lhsIaddLines) :: Int) ((_lhsIruleDescr) :: String) ((_lhsIuseFieldIdent) :: Bool) pos_ var_ ->
                  {-# LINE 77 "./src-ag/Desugar.ag" #-}
                  AGLocal var_ (addl _lhsIaddLines pos_) (if _lhsIuseFieldIdent then Just _lhsIruleDescr else Nothing)
                  {-# LINE 585 "dist/build/Desugar.hs"#-}
   {-# INLINE rule38 #-}
   rule38 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule39 #-}
   rule39 = \ _tks ->
     _tks
{-# NOINLINE sem_HsToken_AGField #-}
sem_HsToken_AGField :: (Identifier) -> (Identifier) -> (Pos) -> (Maybe String) -> T_HsToken 
sem_HsToken_AGField !arg_field_ !arg_attr_ !arg_pos_ _ = T_HsToken (return st14) where
   {-# NOINLINE st14 #-}
   !st14 = let
      v13 :: T_HsToken_v13 
      v13 = \ !(T_HsToken_vIn13 _lhsIaddLines _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsInt _lhsIruleDescr _lhsIuseFieldIdent) -> ( let
         _mField = rule40 _lhsIchildSyns arg_attr_ arg_field_
         _field' = rule41 _mField arg_field_
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule42 _lhsIcon _lhsInt _mField arg_field_
         _lhsOaddLines :: Int
         _lhsOaddLines = rule43 _field' _lhsIaddLines _lhsIuseFieldIdent arg_field_
         _tks = rule44 _field' _lhsIaddLines _lhsIruleDescr _lhsIuseFieldIdent arg_attr_ arg_pos_
         _lhsOtks :: HsToken
         _lhsOtks = rule45 _tks
         !__result_ = T_HsToken_vOut13 _lhsOaddLines _lhsOerrors _lhsOtks
         in __result_ )
     in C_HsToken_s14 v13
   {-# INLINE rule40 #-}
   {-# LINE 79 "./src-ag/Desugar.ag" #-}
   rule40 = \ ((_lhsIchildSyns) :: [(Identifier, Identifier)]) attr_ field_ ->
                     {-# LINE 79 "./src-ag/Desugar.ag" #-}
                     findField field_ attr_ _lhsIchildSyns
                     {-# LINE 616 "dist/build/Desugar.hs"#-}
   {-# INLINE rule41 #-}
   {-# LINE 81 "./src-ag/Desugar.ag" #-}
   rule41 = \ _mField field_ ->
                     {-# LINE 81 "./src-ag/Desugar.ag" #-}
                     maybe field_ id _mField
                     {-# LINE 622 "dist/build/Desugar.hs"#-}
   {-# INLINE rule42 #-}
   {-# LINE 82 "./src-ag/Desugar.ag" #-}
   rule42 = \ ((_lhsIcon) :: ConstructorIdent) ((_lhsInt) :: NontermIdent) _mField field_ ->
                     {-# LINE 82 "./src-ag/Desugar.ag" #-}
                     maybe (Seq.singleton (UndefAttr _lhsInt _lhsIcon field_ (Ident "<ANY>" (getPos field_)) False)) (const Seq.empty) _mField
                     {-# LINE 628 "dist/build/Desugar.hs"#-}
   {-# INLINE rule43 #-}
   {-# LINE 84 "./src-ag/Desugar.ag" #-}
   rule43 = \ _field' ((_lhsIaddLines) :: Int) ((_lhsIuseFieldIdent) :: Bool) field_ ->
                       {-# LINE 84 "./src-ag/Desugar.ag" #-}
                       if _lhsIuseFieldIdent || length (getName field_) < length (getName _field'    )
                       then _lhsIaddLines + 1
                       else _lhsIaddLines
                       {-# LINE 636 "dist/build/Desugar.hs"#-}
   {-# INLINE rule44 #-}
   {-# LINE 88 "./src-ag/Desugar.ag" #-}
   rule44 = \ _field' ((_lhsIaddLines) :: Int) ((_lhsIruleDescr) :: String) ((_lhsIuseFieldIdent) :: Bool) attr_ pos_ ->
                  {-# LINE 88 "./src-ag/Desugar.ag" #-}
                  AGField _field'     attr_ (addl _lhsIaddLines pos_) (if _lhsIuseFieldIdent then Just _lhsIruleDescr else Nothing)
                  {-# LINE 642 "dist/build/Desugar.hs"#-}
   {-# INLINE rule45 #-}
   rule45 = \ _tks ->
     _tks
{-# NOINLINE sem_HsToken_HsToken #-}
sem_HsToken_HsToken :: (String) -> (Pos) -> T_HsToken 
sem_HsToken_HsToken !arg_value_ !arg_pos_ = T_HsToken (return st14) where
   {-# NOINLINE st14 #-}
   !st14 = let
      v13 :: T_HsToken_v13 
      v13 = \ !(T_HsToken_vIn13 _lhsIaddLines _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsInt _lhsIruleDescr _lhsIuseFieldIdent) -> ( let
         _tks = rule46 _lhsIaddLines arg_pos_ arg_value_
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule47  ()
         _lhsOtks :: HsToken
         _lhsOtks = rule48 _tks
         _lhsOaddLines :: Int
         _lhsOaddLines = rule49 _lhsIaddLines
         !__result_ = T_HsToken_vOut13 _lhsOaddLines _lhsOerrors _lhsOtks
         in __result_ )
     in C_HsToken_s14 v13
   {-# INLINE rule46 #-}
   {-# LINE 90 "./src-ag/Desugar.ag" #-}
   rule46 = \ ((_lhsIaddLines) :: Int) pos_ value_ ->
                  {-# LINE 90 "./src-ag/Desugar.ag" #-}
                  HsToken value_ (addl _lhsIaddLines pos_)
                  {-# LINE 668 "dist/build/Desugar.hs"#-}
   {-# INLINE rule47 #-}
   rule47 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule48 #-}
   rule48 = \ _tks ->
     _tks
   {-# INLINE rule49 #-}
   rule49 = \ ((_lhsIaddLines) :: Int) ->
     _lhsIaddLines
{-# NOINLINE sem_HsToken_CharToken #-}
sem_HsToken_CharToken :: (String) -> (Pos) -> T_HsToken 
sem_HsToken_CharToken !arg_value_ !arg_pos_ = T_HsToken (return st14) where
   {-# NOINLINE st14 #-}
   !st14 = let
      v13 :: T_HsToken_v13 
      v13 = \ !(T_HsToken_vIn13 _lhsIaddLines _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsInt _lhsIruleDescr _lhsIuseFieldIdent) -> ( let
         _tks = rule50 _lhsIaddLines arg_pos_ arg_value_
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule51  ()
         _lhsOtks :: HsToken
         _lhsOtks = rule52 _tks
         _lhsOaddLines :: Int
         _lhsOaddLines = rule53 _lhsIaddLines
         !__result_ = T_HsToken_vOut13 _lhsOaddLines _lhsOerrors _lhsOtks
         in __result_ )
     in C_HsToken_s14 v13
   {-# INLINE rule50 #-}
   {-# LINE 92 "./src-ag/Desugar.ag" #-}
   rule50 = \ ((_lhsIaddLines) :: Int) pos_ value_ ->
                  {-# LINE 92 "./src-ag/Desugar.ag" #-}
                  CharToken value_ (addl _lhsIaddLines pos_)
                  {-# LINE 700 "dist/build/Desugar.hs"#-}
   {-# INLINE rule51 #-}
   rule51 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule52 #-}
   rule52 = \ _tks ->
     _tks
   {-# INLINE rule53 #-}
   rule53 = \ ((_lhsIaddLines) :: Int) ->
     _lhsIaddLines
{-# NOINLINE sem_HsToken_StrToken #-}
sem_HsToken_StrToken :: (String) -> (Pos) -> T_HsToken 
sem_HsToken_StrToken !arg_value_ !arg_pos_ = T_HsToken (return st14) where
   {-# NOINLINE st14 #-}
   !st14 = let
      v13 :: T_HsToken_v13 
      v13 = \ !(T_HsToken_vIn13 _lhsIaddLines _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsInt _lhsIruleDescr _lhsIuseFieldIdent) -> ( let
         _tks = rule54 _lhsIaddLines arg_pos_ arg_value_
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule55  ()
         _lhsOtks :: HsToken
         _lhsOtks = rule56 _tks
         _lhsOaddLines :: Int
         _lhsOaddLines = rule57 _lhsIaddLines
         !__result_ = T_HsToken_vOut13 _lhsOaddLines _lhsOerrors _lhsOtks
         in __result_ )
     in C_HsToken_s14 v13
   {-# INLINE rule54 #-}
   {-# LINE 94 "./src-ag/Desugar.ag" #-}
   rule54 = \ ((_lhsIaddLines) :: Int) pos_ value_ ->
                  {-# LINE 94 "./src-ag/Desugar.ag" #-}
                  StrToken value_ (addl _lhsIaddLines pos_)
                  {-# LINE 732 "dist/build/Desugar.hs"#-}
   {-# INLINE rule55 #-}
   rule55 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule56 #-}
   rule56 = \ _tks ->
     _tks
   {-# INLINE rule57 #-}
   rule57 = \ ((_lhsIaddLines) :: Int) ->
     _lhsIaddLines
{-# NOINLINE sem_HsToken_Err #-}
sem_HsToken_Err :: (String) -> (Pos) -> T_HsToken 
sem_HsToken_Err !arg_mesg_ !arg_pos_ = T_HsToken (return st14) where
   {-# NOINLINE st14 #-}
   !st14 = let
      v13 :: T_HsToken_v13 
      v13 = \ !(T_HsToken_vIn13 _lhsIaddLines _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsInt _lhsIruleDescr _lhsIuseFieldIdent) -> ( let
         _tks = rule58 _lhsIaddLines arg_mesg_ arg_pos_
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule59  ()
         _lhsOtks :: HsToken
         _lhsOtks = rule60 _tks
         _lhsOaddLines :: Int
         _lhsOaddLines = rule61 _lhsIaddLines
         !__result_ = T_HsToken_vOut13 _lhsOaddLines _lhsOerrors _lhsOtks
         in __result_ )
     in C_HsToken_s14 v13
   {-# INLINE rule58 #-}
   {-# LINE 96 "./src-ag/Desugar.ag" #-}
   rule58 = \ ((_lhsIaddLines) :: Int) mesg_ pos_ ->
                  {-# LINE 96 "./src-ag/Desugar.ag" #-}
                  Err mesg_ (addl _lhsIaddLines pos_)
                  {-# LINE 764 "dist/build/Desugar.hs"#-}
   {-# INLINE rule59 #-}
   rule59 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule60 #-}
   rule60 = \ _tks ->
     _tks
   {-# INLINE rule61 #-}
   rule61 = \ ((_lhsIaddLines) :: Int) ->
     _lhsIaddLines

-- HsTokens ----------------------------------------------------
-- wrapper
data Inh_HsTokens  = Inh_HsTokens { addLines_Inh_HsTokens :: !(Int), childInhs_Inh_HsTokens :: !([(Identifier, Identifier)]), childSyns_Inh_HsTokens :: !([(Identifier, Identifier)]), con_Inh_HsTokens :: !(ConstructorIdent), nt_Inh_HsTokens :: !(NontermIdent), ruleDescr_Inh_HsTokens :: !(String), useFieldIdent_Inh_HsTokens :: !(Bool) }
data Syn_HsTokens  = Syn_HsTokens { addLines_Syn_HsTokens :: !(Int), errors_Syn_HsTokens :: !(Seq Error), tks_Syn_HsTokens :: !(HsTokens) }
{-# INLINABLE wrap_HsTokens #-}
wrap_HsTokens :: T_HsTokens  -> Inh_HsTokens  -> (Syn_HsTokens )
wrap_HsTokens !(T_HsTokens act) !(Inh_HsTokens _lhsIaddLines _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsInt _lhsIruleDescr _lhsIuseFieldIdent) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_HsTokens_vIn16 _lhsIaddLines _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsInt _lhsIruleDescr _lhsIuseFieldIdent
        !(T_HsTokens_vOut16 _lhsOaddLines _lhsOerrors _lhsOtks) <- return (inv_HsTokens_s17 sem arg)
        return (Syn_HsTokens _lhsOaddLines _lhsOerrors _lhsOtks)
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
data T_HsTokens_vIn16  = T_HsTokens_vIn16 (Int) ([(Identifier, Identifier)]) ([(Identifier, Identifier)]) (ConstructorIdent) (NontermIdent) (String) (Bool)
data T_HsTokens_vOut16  = T_HsTokens_vOut16 (Int) (Seq Error) (HsTokens)
{-# NOINLINE sem_HsTokens_Cons #-}
sem_HsTokens_Cons :: T_HsToken  -> T_HsTokens  -> T_HsTokens 
sem_HsTokens_Cons arg_hd_ arg_tl_ = T_HsTokens (return st17) where
   {-# NOINLINE st17 #-}
   !st17 = let
      v16 :: T_HsTokens_v16 
      v16 = \ !(T_HsTokens_vIn16 _lhsIaddLines _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsInt _lhsIruleDescr _lhsIuseFieldIdent) -> ( let
         _hdX14 = Control.Monad.Identity.runIdentity (attach_T_HsToken (arg_hd_))
         _tlX17 = Control.Monad.Identity.runIdentity (attach_T_HsTokens (arg_tl_))
         (T_HsToken_vOut13 _hdIaddLines _hdIerrors _hdItks) = inv_HsToken_s14 _hdX14 (T_HsToken_vIn13 _hdOaddLines _hdOchildInhs _hdOchildSyns _hdOcon _hdOnt _hdOruleDescr _hdOuseFieldIdent)
         (T_HsTokens_vOut16 _tlIaddLines _tlIerrors _tlItks) = inv_HsTokens_s17 _tlX17 (T_HsTokens_vIn16 _tlOaddLines _tlOchildInhs _tlOchildSyns _tlOcon _tlOnt _tlOruleDescr _tlOuseFieldIdent)
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule62 _hdIerrors _tlIerrors
         _tks = rule63 _hdItks _tlItks
         _lhsOtks :: HsTokens
         _lhsOtks = rule64 _tks
         _lhsOaddLines :: Int
         _lhsOaddLines = rule65 _tlIaddLines
         _hdOaddLines = rule66 _lhsIaddLines
         _hdOchildInhs = rule67 _lhsIchildInhs
         _hdOchildSyns = rule68 _lhsIchildSyns
         _hdOcon = rule69 _lhsIcon
         _hdOnt = rule70 _lhsInt
         _hdOruleDescr = rule71 _lhsIruleDescr
         _hdOuseFieldIdent = rule72 _lhsIuseFieldIdent
         _tlOaddLines = rule73 _hdIaddLines
         _tlOchildInhs = rule74 _lhsIchildInhs
         _tlOchildSyns = rule75 _lhsIchildSyns
         _tlOcon = rule76 _lhsIcon
         _tlOnt = rule77 _lhsInt
         _tlOruleDescr = rule78 _lhsIruleDescr
         _tlOuseFieldIdent = rule79 _lhsIuseFieldIdent
         !__result_ = T_HsTokens_vOut16 _lhsOaddLines _lhsOerrors _lhsOtks
         in __result_ )
     in C_HsTokens_s17 v16
   {-# INLINE rule62 #-}
   rule62 = \ ((_hdIerrors) :: Seq Error) ((_tlIerrors) :: Seq Error) ->
     _hdIerrors Seq.>< _tlIerrors
   {-# INLINE rule63 #-}
   rule63 = \ ((_hdItks) :: HsToken) ((_tlItks) :: HsTokens) ->
     (:) _hdItks _tlItks
   {-# INLINE rule64 #-}
   rule64 = \ _tks ->
     _tks
   {-# INLINE rule65 #-}
   rule65 = \ ((_tlIaddLines) :: Int) ->
     _tlIaddLines
   {-# INLINE rule66 #-}
   rule66 = \ ((_lhsIaddLines) :: Int) ->
     _lhsIaddLines
   {-# INLINE rule67 #-}
   rule67 = \ ((_lhsIchildInhs) :: [(Identifier, Identifier)]) ->
     _lhsIchildInhs
   {-# INLINE rule68 #-}
   rule68 = \ ((_lhsIchildSyns) :: [(Identifier, Identifier)]) ->
     _lhsIchildSyns
   {-# INLINE rule69 #-}
   rule69 = \ ((_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# INLINE rule70 #-}
   rule70 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule71 #-}
   rule71 = \ ((_lhsIruleDescr) :: String) ->
     _lhsIruleDescr
   {-# INLINE rule72 #-}
   rule72 = \ ((_lhsIuseFieldIdent) :: Bool) ->
     _lhsIuseFieldIdent
   {-# INLINE rule73 #-}
   rule73 = \ ((_hdIaddLines) :: Int) ->
     _hdIaddLines
   {-# INLINE rule74 #-}
   rule74 = \ ((_lhsIchildInhs) :: [(Identifier, Identifier)]) ->
     _lhsIchildInhs
   {-# INLINE rule75 #-}
   rule75 = \ ((_lhsIchildSyns) :: [(Identifier, Identifier)]) ->
     _lhsIchildSyns
   {-# INLINE rule76 #-}
   rule76 = \ ((_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# INLINE rule77 #-}
   rule77 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule78 #-}
   rule78 = \ ((_lhsIruleDescr) :: String) ->
     _lhsIruleDescr
   {-# INLINE rule79 #-}
   rule79 = \ ((_lhsIuseFieldIdent) :: Bool) ->
     _lhsIuseFieldIdent
{-# NOINLINE sem_HsTokens_Nil #-}
sem_HsTokens_Nil ::  T_HsTokens 
sem_HsTokens_Nil  = T_HsTokens (return st17) where
   {-# NOINLINE st17 #-}
   !st17 = let
      v16 :: T_HsTokens_v16 
      v16 = \ !(T_HsTokens_vIn16 _lhsIaddLines _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsInt _lhsIruleDescr _lhsIuseFieldIdent) -> ( let
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule80  ()
         _tks = rule81  ()
         _lhsOtks :: HsTokens
         _lhsOtks = rule82 _tks
         _lhsOaddLines :: Int
         _lhsOaddLines = rule83 _lhsIaddLines
         !__result_ = T_HsTokens_vOut16 _lhsOaddLines _lhsOerrors _lhsOtks
         in __result_ )
     in C_HsTokens_s17 v16
   {-# INLINE rule80 #-}
   rule80 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule81 #-}
   rule81 = \  (_ :: ()) ->
     []
   {-# INLINE rule82 #-}
   rule82 = \ _tks ->
     _tks
   {-# INLINE rule83 #-}
   rule83 = \ ((_lhsIaddLines) :: Int) ->
     _lhsIaddLines

-- HsTokensRoot ------------------------------------------------
-- wrapper
data Inh_HsTokensRoot  = Inh_HsTokensRoot { childInhs_Inh_HsTokensRoot :: !([(Identifier, Identifier)]), childSyns_Inh_HsTokensRoot :: !([(Identifier, Identifier)]), con_Inh_HsTokensRoot :: !(ConstructorIdent), nt_Inh_HsTokensRoot :: !(NontermIdent), ruleDescr_Inh_HsTokensRoot :: !(String), useFieldIdent_Inh_HsTokensRoot :: !(Bool) }
data Syn_HsTokensRoot  = Syn_HsTokensRoot { errors_Syn_HsTokensRoot :: !(Seq Error), tks_Syn_HsTokensRoot :: !([HsToken]) }
{-# INLINABLE wrap_HsTokensRoot #-}
wrap_HsTokensRoot :: T_HsTokensRoot  -> Inh_HsTokensRoot  -> (Syn_HsTokensRoot )
wrap_HsTokensRoot !(T_HsTokensRoot act) !(Inh_HsTokensRoot _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsInt _lhsIruleDescr _lhsIuseFieldIdent) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_HsTokensRoot_vIn19 _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsInt _lhsIruleDescr _lhsIuseFieldIdent
        !(T_HsTokensRoot_vOut19 _lhsOerrors _lhsOtks) <- return (inv_HsTokensRoot_s20 sem arg)
        return (Syn_HsTokensRoot _lhsOerrors _lhsOtks)
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
data T_HsTokensRoot_vIn19  = T_HsTokensRoot_vIn19 ([(Identifier, Identifier)]) ([(Identifier, Identifier)]) (ConstructorIdent) (NontermIdent) (String) (Bool)
data T_HsTokensRoot_vOut19  = T_HsTokensRoot_vOut19 (Seq Error) ([HsToken])
{-# NOINLINE sem_HsTokensRoot_HsTokensRoot #-}
sem_HsTokensRoot_HsTokensRoot :: T_HsTokens  -> T_HsTokensRoot 
sem_HsTokensRoot_HsTokensRoot arg_tokens_ = T_HsTokensRoot (return st20) where
   {-# NOINLINE st20 #-}
   !st20 = let
      v19 :: T_HsTokensRoot_v19 
      v19 = \ !(T_HsTokensRoot_vIn19 _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsInt _lhsIruleDescr _lhsIuseFieldIdent) -> ( let
         _tokensX17 = Control.Monad.Identity.runIdentity (attach_T_HsTokens (arg_tokens_))
         (T_HsTokens_vOut16 _tokensIaddLines _tokensIerrors _tokensItks) = inv_HsTokens_s17 _tokensX17 (T_HsTokens_vIn16 _tokensOaddLines _tokensOchildInhs _tokensOchildSyns _tokensOcon _tokensOnt _tokensOruleDescr _tokensOuseFieldIdent)
         _tokensOaddLines = rule84  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule85 _tokensIerrors
         _lhsOtks :: [HsToken]
         _lhsOtks = rule86 _tokensItks
         _tokensOchildInhs = rule87 _lhsIchildInhs
         _tokensOchildSyns = rule88 _lhsIchildSyns
         _tokensOcon = rule89 _lhsIcon
         _tokensOnt = rule90 _lhsInt
         _tokensOruleDescr = rule91 _lhsIruleDescr
         _tokensOuseFieldIdent = rule92 _lhsIuseFieldIdent
         !__result_ = T_HsTokensRoot_vOut19 _lhsOerrors _lhsOtks
         in __result_ )
     in C_HsTokensRoot_s20 v19
   {-# INLINE rule84 #-}
   {-# LINE 67 "./src-ag/Desugar.ag" #-}
   rule84 = \  (_ :: ()) ->
                          {-# LINE 67 "./src-ag/Desugar.ag" #-}
                          0
                          {-# LINE 982 "dist/build/Desugar.hs"#-}
   {-# INLINE rule85 #-}
   rule85 = \ ((_tokensIerrors) :: Seq Error) ->
     _tokensIerrors
   {-# INLINE rule86 #-}
   rule86 = \ ((_tokensItks) :: HsTokens) ->
     _tokensItks
   {-# INLINE rule87 #-}
   rule87 = \ ((_lhsIchildInhs) :: [(Identifier, Identifier)]) ->
     _lhsIchildInhs
   {-# INLINE rule88 #-}
   rule88 = \ ((_lhsIchildSyns) :: [(Identifier, Identifier)]) ->
     _lhsIchildSyns
   {-# INLINE rule89 #-}
   rule89 = \ ((_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# INLINE rule90 #-}
   rule90 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule91 #-}
   rule91 = \ ((_lhsIruleDescr) :: String) ->
     _lhsIruleDescr
   {-# INLINE rule92 #-}
   rule92 = \ ((_lhsIuseFieldIdent) :: Bool) ->
     _lhsIuseFieldIdent

-- Nonterminal -------------------------------------------------
-- wrapper
data Inh_Nonterminal  = Inh_Nonterminal { augmentsIn_Inh_Nonterminal :: !(Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))), forcedIrrefutables_Inh_Nonterminal :: !(AttrMap), inhMap_Inh_Nonterminal :: !(Map Identifier Attributes), mainName_Inh_Nonterminal :: !(String), options_Inh_Nonterminal :: !(Options), synMap_Inh_Nonterminal :: !(Map Identifier Attributes) }
data Syn_Nonterminal  = Syn_Nonterminal { allAttributes_Syn_Nonterminal :: !(AttrMap), augmentsOut_Syn_Nonterminal :: !(Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))), errors_Syn_Nonterminal :: !(Seq Error), inhMap'_Syn_Nonterminal :: !(Map Identifier Attributes), output_Syn_Nonterminal :: !(Nonterminal), synMap'_Syn_Nonterminal :: !(Map Identifier Attributes) }
{-# INLINABLE wrap_Nonterminal #-}
wrap_Nonterminal :: T_Nonterminal  -> Inh_Nonterminal  -> (Syn_Nonterminal )
wrap_Nonterminal !(T_Nonterminal act) !(Inh_Nonterminal _lhsIaugmentsIn _lhsIforcedIrrefutables _lhsIinhMap _lhsImainName _lhsIoptions _lhsIsynMap) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_Nonterminal_vIn22 _lhsIaugmentsIn _lhsIforcedIrrefutables _lhsIinhMap _lhsImainName _lhsIoptions _lhsIsynMap
        !(T_Nonterminal_vOut22 _lhsOallAttributes _lhsOaugmentsOut _lhsOerrors _lhsOinhMap' _lhsOoutput _lhsOsynMap') <- return (inv_Nonterminal_s23 sem arg)
        return (Syn_Nonterminal _lhsOallAttributes _lhsOaugmentsOut _lhsOerrors _lhsOinhMap' _lhsOoutput _lhsOsynMap')
   )

-- cata
{-# INLINE sem_Nonterminal #-}
sem_Nonterminal :: Nonterminal  -> T_Nonterminal 
sem_Nonterminal ( Nonterminal !nt_ !params_ !inh_ !syn_ prods_ ) = sem_Nonterminal_Nonterminal nt_ params_ inh_ syn_ ( sem_Productions prods_ )

-- semantic domain
newtype T_Nonterminal  = T_Nonterminal {
                                       attach_T_Nonterminal :: Identity (T_Nonterminal_s23 )
                                       }
newtype T_Nonterminal_s23  = C_Nonterminal_s23 {
                                               inv_Nonterminal_s23 :: (T_Nonterminal_v22 )
                                               }
data T_Nonterminal_s24  = C_Nonterminal_s24
type T_Nonterminal_v22  = (T_Nonterminal_vIn22 ) -> (T_Nonterminal_vOut22 )
data T_Nonterminal_vIn22  = T_Nonterminal_vIn22 (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) (AttrMap) (Map Identifier Attributes) (String) (Options) (Map Identifier Attributes)
data T_Nonterminal_vOut22  = T_Nonterminal_vOut22 (AttrMap) (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) (Seq Error) (Map Identifier Attributes) (Nonterminal) (Map Identifier Attributes)
{-# NOINLINE sem_Nonterminal_Nonterminal #-}
sem_Nonterminal_Nonterminal :: (NontermIdent) -> ([Identifier]) -> (Attributes) -> (Attributes) -> T_Productions  -> T_Nonterminal 
sem_Nonterminal_Nonterminal !arg_nt_ !arg_params_ !arg_inh_ !arg_syn_ arg_prods_ = T_Nonterminal (return st23) where
   {-# NOINLINE st23 #-}
   !st23 = let
      v22 :: T_Nonterminal_v22 
      v22 = \ !(T_Nonterminal_vIn22 _lhsIaugmentsIn _lhsIforcedIrrefutables _lhsIinhMap _lhsImainName _lhsIoptions _lhsIsynMap) -> ( let
         _prodsX38 = Control.Monad.Identity.runIdentity (attach_T_Productions (arg_prods_))
         (T_Productions_vOut37 _prodsIallAttributes _prodsIaugmentsOut _prodsIerrors _prodsIoutput) = inv_Productions_s38 _prodsX38 (T_Productions_vIn37 _prodsOaugmentsIn _prodsOforcedIrrefutables _prodsOinhMap _prodsOmainName _prodsOnt _prodsOoptions _prodsOsynMap)
         _prodsOnt = rule93 arg_nt_
         _augmentsIn = rule94 _lhsIaugmentsIn arg_nt_
         _augmentsOut = rule95 _prodsIaugmentsOut arg_nt_
         _extraInh = rule96 _lhsImainName _lhsIoptions
         _lhsOoutput :: Nonterminal
         _lhsOoutput = rule97 _extraInh _prodsIoutput arg_inh_ arg_nt_ arg_params_ arg_syn_
         _lhsOinhMap' :: Map Identifier Attributes
         _lhsOinhMap' = rule98 arg_inh_ arg_nt_
         _lhsOsynMap' :: Map Identifier Attributes
         _lhsOsynMap' = rule99 arg_nt_ arg_syn_
         _lhsOallAttributes :: AttrMap
         _lhsOallAttributes = rule100 _prodsIallAttributes
         _lhsOaugmentsOut :: Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))
         _lhsOaugmentsOut = rule101 _augmentsOut
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule102 _prodsIerrors
         _output = rule103 _prodsIoutput arg_inh_ arg_nt_ arg_params_ arg_syn_
         _prodsOaugmentsIn = rule104 _augmentsIn
         _prodsOforcedIrrefutables = rule105 _lhsIforcedIrrefutables
         _prodsOinhMap = rule106 _lhsIinhMap
         _prodsOmainName = rule107 _lhsImainName
         _prodsOoptions = rule108 _lhsIoptions
         _prodsOsynMap = rule109 _lhsIsynMap
         !__result_ = T_Nonterminal_vOut22 _lhsOallAttributes _lhsOaugmentsOut _lhsOerrors _lhsOinhMap' _lhsOoutput _lhsOsynMap'
         in __result_ )
     in C_Nonterminal_s23 v22
   {-# INLINE rule93 #-}
   {-# LINE 157 "./src-ag/Desugar.ag" #-}
   rule93 = \ nt_ ->
                   {-# LINE 157 "./src-ag/Desugar.ag" #-}
                   nt_
                   {-# LINE 1078 "dist/build/Desugar.hs"#-}
   {-# INLINE rule94 #-}
   {-# LINE 239 "./src-ag/Desugar.ag" #-}
   rule94 = \ ((_lhsIaugmentsIn) :: Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) nt_ ->
                         {-# LINE 239 "./src-ag/Desugar.ag" #-}
                         Map.findWithDefault Map.empty nt_ _lhsIaugmentsIn
                         {-# LINE 1084 "dist/build/Desugar.hs"#-}
   {-# INLINE rule95 #-}
   {-# LINE 240 "./src-ag/Desugar.ag" #-}
   rule95 = \ ((_prodsIaugmentsOut) :: Map ConstructorIdent (Map Identifier [Expression])) nt_ ->
                          {-# LINE 240 "./src-ag/Desugar.ag" #-}
                          Map.singleton nt_ _prodsIaugmentsOut
                          {-# LINE 1090 "dist/build/Desugar.hs"#-}
   {-# INLINE rule96 #-}
   {-# LINE 292 "./src-ag/Desugar.ag" #-}
   rule96 = \ ((_lhsImainName) :: String) ((_lhsIoptions) :: Options) ->
                   {-# LINE 292 "./src-ag/Desugar.ag" #-}
                   addLateAttr _lhsIoptions _lhsImainName
                   {-# LINE 1096 "dist/build/Desugar.hs"#-}
   {-# INLINE rule97 #-}
   {-# LINE 308 "./src-ag/Desugar.ag" #-}
   rule97 = \ _extraInh ((_prodsIoutput) :: Productions) inh_ nt_ params_ syn_ ->
                 {-# LINE 308 "./src-ag/Desugar.ag" #-}
                 Nonterminal
                   nt_ params_
                   (_extraInh     `Map.union` inh_)
                   syn_
                   _prodsIoutput
                 {-# LINE 1106 "dist/build/Desugar.hs"#-}
   {-# INLINE rule98 #-}
   {-# LINE 7 "./src-ag/DistChildAttr.ag" #-}
   rule98 = \ inh_ nt_ ->
                                 {-# LINE 7 "./src-ag/DistChildAttr.ag" #-}
                                 Map.singleton nt_ inh_
                                 {-# LINE 1112 "dist/build/Desugar.hs"#-}
   {-# INLINE rule99 #-}
   {-# LINE 8 "./src-ag/DistChildAttr.ag" #-}
   rule99 = \ nt_ syn_ ->
                                 {-# LINE 8 "./src-ag/DistChildAttr.ag" #-}
                                 Map.singleton nt_ syn_
                                 {-# LINE 1118 "dist/build/Desugar.hs"#-}
   {-# INLINE rule100 #-}
   rule100 = \ ((_prodsIallAttributes) :: AttrMap) ->
     _prodsIallAttributes
   {-# INLINE rule101 #-}
   rule101 = \ _augmentsOut ->
     _augmentsOut
   {-# INLINE rule102 #-}
   rule102 = \ ((_prodsIerrors) :: Seq Error) ->
     _prodsIerrors
   {-# INLINE rule103 #-}
   rule103 = \ ((_prodsIoutput) :: Productions) inh_ nt_ params_ syn_ ->
     Nonterminal nt_ params_ inh_ syn_ _prodsIoutput
   {-# INLINE rule104 #-}
   rule104 = \ _augmentsIn ->
     _augmentsIn
   {-# INLINE rule105 #-}
   rule105 = \ ((_lhsIforcedIrrefutables) :: AttrMap) ->
     _lhsIforcedIrrefutables
   {-# INLINE rule106 #-}
   rule106 = \ ((_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# INLINE rule107 #-}
   rule107 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule108 #-}
   rule108 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule109 #-}
   rule109 = \ ((_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap

-- Nonterminals ------------------------------------------------
-- wrapper
data Inh_Nonterminals  = Inh_Nonterminals { augmentsIn_Inh_Nonterminals :: !(Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))), forcedIrrefutables_Inh_Nonterminals :: !(AttrMap), inhMap_Inh_Nonterminals :: !(Map Identifier Attributes), mainName_Inh_Nonterminals :: !(String), options_Inh_Nonterminals :: !(Options), synMap_Inh_Nonterminals :: !(Map Identifier Attributes) }
data Syn_Nonterminals  = Syn_Nonterminals { allAttributes_Syn_Nonterminals :: !(AttrMap), augmentsOut_Syn_Nonterminals :: !(Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))), errors_Syn_Nonterminals :: !(Seq Error), inhMap'_Syn_Nonterminals :: !(Map Identifier Attributes), output_Syn_Nonterminals :: !(Nonterminals), synMap'_Syn_Nonterminals :: !(Map Identifier Attributes) }
{-# INLINABLE wrap_Nonterminals #-}
wrap_Nonterminals :: T_Nonterminals  -> Inh_Nonterminals  -> (Syn_Nonterminals )
wrap_Nonterminals !(T_Nonterminals act) !(Inh_Nonterminals _lhsIaugmentsIn _lhsIforcedIrrefutables _lhsIinhMap _lhsImainName _lhsIoptions _lhsIsynMap) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_Nonterminals_vIn25 _lhsIaugmentsIn _lhsIforcedIrrefutables _lhsIinhMap _lhsImainName _lhsIoptions _lhsIsynMap
        !(T_Nonterminals_vOut25 _lhsOallAttributes _lhsOaugmentsOut _lhsOerrors _lhsOinhMap' _lhsOoutput _lhsOsynMap') <- return (inv_Nonterminals_s26 sem arg)
        return (Syn_Nonterminals _lhsOallAttributes _lhsOaugmentsOut _lhsOerrors _lhsOinhMap' _lhsOoutput _lhsOsynMap')
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
data T_Nonterminals_vIn25  = T_Nonterminals_vIn25 (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) (AttrMap) (Map Identifier Attributes) (String) (Options) (Map Identifier Attributes)
data T_Nonterminals_vOut25  = T_Nonterminals_vOut25 (AttrMap) (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) (Seq Error) (Map Identifier Attributes) (Nonterminals) (Map Identifier Attributes)
{-# NOINLINE sem_Nonterminals_Cons #-}
sem_Nonterminals_Cons :: T_Nonterminal  -> T_Nonterminals  -> T_Nonterminals 
sem_Nonterminals_Cons arg_hd_ arg_tl_ = T_Nonterminals (return st26) where
   {-# NOINLINE st26 #-}
   !st26 = let
      v25 :: T_Nonterminals_v25 
      v25 = \ !(T_Nonterminals_vIn25 _lhsIaugmentsIn _lhsIforcedIrrefutables _lhsIinhMap _lhsImainName _lhsIoptions _lhsIsynMap) -> ( let
         _hdX23 = Control.Monad.Identity.runIdentity (attach_T_Nonterminal (arg_hd_))
         _tlX26 = Control.Monad.Identity.runIdentity (attach_T_Nonterminals (arg_tl_))
         (T_Nonterminal_vOut22 _hdIallAttributes _hdIaugmentsOut _hdIerrors _hdIinhMap' _hdIoutput _hdIsynMap') = inv_Nonterminal_s23 _hdX23 (T_Nonterminal_vIn22 _hdOaugmentsIn _hdOforcedIrrefutables _hdOinhMap _hdOmainName _hdOoptions _hdOsynMap)
         (T_Nonterminals_vOut25 _tlIallAttributes _tlIaugmentsOut _tlIerrors _tlIinhMap' _tlIoutput _tlIsynMap') = inv_Nonterminals_s26 _tlX26 (T_Nonterminals_vIn25 _tlOaugmentsIn _tlOforcedIrrefutables _tlOinhMap _tlOmainName _tlOoptions _tlOsynMap)
         _lhsOallAttributes :: AttrMap
         _lhsOallAttributes = rule110 _hdIallAttributes _tlIallAttributes
         _lhsOaugmentsOut :: Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))
         _lhsOaugmentsOut = rule111 _hdIaugmentsOut _tlIaugmentsOut
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule112 _hdIerrors _tlIerrors
         _lhsOinhMap' :: Map Identifier Attributes
         _lhsOinhMap' = rule113 _hdIinhMap' _tlIinhMap'
         _lhsOsynMap' :: Map Identifier Attributes
         _lhsOsynMap' = rule114 _hdIsynMap' _tlIsynMap'
         _output = rule115 _hdIoutput _tlIoutput
         _lhsOoutput :: Nonterminals
         _lhsOoutput = rule116 _output
         _hdOaugmentsIn = rule117 _lhsIaugmentsIn
         _hdOforcedIrrefutables = rule118 _lhsIforcedIrrefutables
         _hdOinhMap = rule119 _lhsIinhMap
         _hdOmainName = rule120 _lhsImainName
         _hdOoptions = rule121 _lhsIoptions
         _hdOsynMap = rule122 _lhsIsynMap
         _tlOaugmentsIn = rule123 _lhsIaugmentsIn
         _tlOforcedIrrefutables = rule124 _lhsIforcedIrrefutables
         _tlOinhMap = rule125 _lhsIinhMap
         _tlOmainName = rule126 _lhsImainName
         _tlOoptions = rule127 _lhsIoptions
         _tlOsynMap = rule128 _lhsIsynMap
         !__result_ = T_Nonterminals_vOut25 _lhsOallAttributes _lhsOaugmentsOut _lhsOerrors _lhsOinhMap' _lhsOoutput _lhsOsynMap'
         in __result_ )
     in C_Nonterminals_s26 v25
   {-# INLINE rule110 #-}
   rule110 = \ ((_hdIallAttributes) :: AttrMap) ((_tlIallAttributes) :: AttrMap) ->
     _hdIallAttributes `mergeAttributes` _tlIallAttributes
   {-# INLINE rule111 #-}
   rule111 = \ ((_hdIaugmentsOut) :: Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) ((_tlIaugmentsOut) :: Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) ->
     _hdIaugmentsOut `Map.union` _tlIaugmentsOut
   {-# INLINE rule112 #-}
   rule112 = \ ((_hdIerrors) :: Seq Error) ((_tlIerrors) :: Seq Error) ->
     _hdIerrors Seq.>< _tlIerrors
   {-# INLINE rule113 #-}
   rule113 = \ ((_hdIinhMap') :: Map Identifier Attributes) ((_tlIinhMap') :: Map Identifier Attributes) ->
     _hdIinhMap' `Map.union` _tlIinhMap'
   {-# INLINE rule114 #-}
   rule114 = \ ((_hdIsynMap') :: Map Identifier Attributes) ((_tlIsynMap') :: Map Identifier Attributes) ->
     _hdIsynMap' `Map.union` _tlIsynMap'
   {-# INLINE rule115 #-}
   rule115 = \ ((_hdIoutput) :: Nonterminal) ((_tlIoutput) :: Nonterminals) ->
     (:) _hdIoutput _tlIoutput
   {-# INLINE rule116 #-}
   rule116 = \ _output ->
     _output
   {-# INLINE rule117 #-}
   rule117 = \ ((_lhsIaugmentsIn) :: Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) ->
     _lhsIaugmentsIn
   {-# INLINE rule118 #-}
   rule118 = \ ((_lhsIforcedIrrefutables) :: AttrMap) ->
     _lhsIforcedIrrefutables
   {-# INLINE rule119 #-}
   rule119 = \ ((_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# INLINE rule120 #-}
   rule120 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule121 #-}
   rule121 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule122 #-}
   rule122 = \ ((_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
   {-# INLINE rule123 #-}
   rule123 = \ ((_lhsIaugmentsIn) :: Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) ->
     _lhsIaugmentsIn
   {-# INLINE rule124 #-}
   rule124 = \ ((_lhsIforcedIrrefutables) :: AttrMap) ->
     _lhsIforcedIrrefutables
   {-# INLINE rule125 #-}
   rule125 = \ ((_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# INLINE rule126 #-}
   rule126 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule127 #-}
   rule127 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule128 #-}
   rule128 = \ ((_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
{-# NOINLINE sem_Nonterminals_Nil #-}
sem_Nonterminals_Nil ::  T_Nonterminals 
sem_Nonterminals_Nil  = T_Nonterminals (return st26) where
   {-# NOINLINE st26 #-}
   !st26 = let
      v25 :: T_Nonterminals_v25 
      v25 = \ !(T_Nonterminals_vIn25 _lhsIaugmentsIn _lhsIforcedIrrefutables _lhsIinhMap _lhsImainName _lhsIoptions _lhsIsynMap) -> ( let
         _lhsOallAttributes :: AttrMap
         _lhsOallAttributes = rule129  ()
         _lhsOaugmentsOut :: Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))
         _lhsOaugmentsOut = rule130  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule131  ()
         _lhsOinhMap' :: Map Identifier Attributes
         _lhsOinhMap' = rule132  ()
         _lhsOsynMap' :: Map Identifier Attributes
         _lhsOsynMap' = rule133  ()
         _output = rule134  ()
         _lhsOoutput :: Nonterminals
         _lhsOoutput = rule135 _output
         !__result_ = T_Nonterminals_vOut25 _lhsOallAttributes _lhsOaugmentsOut _lhsOerrors _lhsOinhMap' _lhsOoutput _lhsOsynMap'
         in __result_ )
     in C_Nonterminals_s26 v25
   {-# INLINE rule129 #-}
   rule129 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule130 #-}
   rule130 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule131 #-}
   rule131 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule132 #-}
   rule132 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule133 #-}
   rule133 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule134 #-}
   rule134 = \  (_ :: ()) ->
     []
   {-# INLINE rule135 #-}
   rule135 = \ _output ->
     _output

-- Pattern -----------------------------------------------------
-- wrapper
data Inh_Pattern  = Inh_Pattern { childInhs_Inh_Pattern :: !([(Identifier, Identifier)]), childSyns_Inh_Pattern :: !([(Identifier, Identifier)]), con_Inh_Pattern :: !(ConstructorIdent), defs_Inh_Pattern :: !(Set (Identifier, Identifier)), forcedIrrefutables_Inh_Pattern :: !(AttrMap), nt_Inh_Pattern :: !(NontermIdent) }
data Syn_Pattern  = Syn_Pattern { allAttributes_Syn_Pattern :: !(AttrMap), copy_Syn_Pattern :: !(Pattern), defsCollect_Syn_Pattern :: !(Set (Identifier, Identifier)), errors_Syn_Pattern :: !(Seq Error), output_Syn_Pattern :: !(Pattern) }
{-# INLINABLE wrap_Pattern #-}
wrap_Pattern :: T_Pattern  -> Inh_Pattern  -> (Syn_Pattern )
wrap_Pattern !(T_Pattern act) !(Inh_Pattern _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_Pattern_vIn28 _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt
        !(T_Pattern_vOut28 _lhsOallAttributes _lhsOcopy _lhsOdefsCollect _lhsOerrors _lhsOoutput) <- return (inv_Pattern_s29 sem arg)
        return (Syn_Pattern _lhsOallAttributes _lhsOcopy _lhsOdefsCollect _lhsOerrors _lhsOoutput)
   )

-- cata
{-# NOINLINE sem_Pattern #-}
sem_Pattern :: Pattern  -> T_Pattern 
sem_Pattern ( Constr !name_ pats_ ) = sem_Pattern_Constr name_ ( sem_Patterns pats_ )
sem_Pattern ( Product !pos_ pats_ ) = sem_Pattern_Product pos_ ( sem_Patterns pats_ )
sem_Pattern ( Alias !field_ !attr_ pat_ ) = sem_Pattern_Alias field_ attr_ ( sem_Pattern pat_ )
sem_Pattern ( Irrefutable pat_ ) = sem_Pattern_Irrefutable ( sem_Pattern pat_ )
sem_Pattern ( Underscore !pos_ ) = sem_Pattern_Underscore pos_

-- semantic domain
newtype T_Pattern  = T_Pattern {
                               attach_T_Pattern :: Identity (T_Pattern_s29 )
                               }
newtype T_Pattern_s29  = C_Pattern_s29 {
                                       inv_Pattern_s29 :: (T_Pattern_v28 )
                                       }
data T_Pattern_s30  = C_Pattern_s30
type T_Pattern_v28  = (T_Pattern_vIn28 ) -> (T_Pattern_vOut28 )
data T_Pattern_vIn28  = T_Pattern_vIn28 ([(Identifier, Identifier)]) ([(Identifier, Identifier)]) (ConstructorIdent) (Set (Identifier, Identifier)) (AttrMap) (NontermIdent)
data T_Pattern_vOut28  = T_Pattern_vOut28 (AttrMap) (Pattern) (Set (Identifier, Identifier)) (Seq Error) (Pattern)
{-# NOINLINE sem_Pattern_Constr #-}
sem_Pattern_Constr :: (ConstructorIdent) -> T_Patterns  -> T_Pattern 
sem_Pattern_Constr !arg_name_ arg_pats_ = T_Pattern (return st29) where
   {-# NOINLINE st29 #-}
   !st29 = let
      v28 :: T_Pattern_v28 
      v28 = \ !(T_Pattern_vIn28 _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt) -> ( let
         _patsX32 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_pats_))
         (T_Patterns_vOut31 _patsIallAttributes _patsIcopy _patsIdefsCollect _patsIerrors _patsIoutput) = inv_Patterns_s32 _patsX32 (T_Patterns_vIn31 _patsOchildInhs _patsOchildSyns _patsOcon _patsOdefs _patsOforcedIrrefutables _patsOnt)
         _lhsOallAttributes :: AttrMap
         _lhsOallAttributes = rule136 _patsIallAttributes
         _lhsOdefsCollect :: Set (Identifier, Identifier)
         _lhsOdefsCollect = rule137 _patsIdefsCollect
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule138 _patsIerrors
         _copy = rule139 _patsIcopy arg_name_
         _output = rule140 _patsIoutput arg_name_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule141 _copy
         _lhsOoutput :: Pattern
         _lhsOoutput = rule142 _output
         _patsOchildInhs = rule143 _lhsIchildInhs
         _patsOchildSyns = rule144 _lhsIchildSyns
         _patsOcon = rule145 _lhsIcon
         _patsOdefs = rule146 _lhsIdefs
         _patsOforcedIrrefutables = rule147 _lhsIforcedIrrefutables
         _patsOnt = rule148 _lhsInt
         !__result_ = T_Pattern_vOut28 _lhsOallAttributes _lhsOcopy _lhsOdefsCollect _lhsOerrors _lhsOoutput
         in __result_ )
     in C_Pattern_s29 v28
   {-# INLINE rule136 #-}
   rule136 = \ ((_patsIallAttributes) :: AttrMap) ->
     _patsIallAttributes
   {-# INLINE rule137 #-}
   rule137 = \ ((_patsIdefsCollect) :: Set (Identifier, Identifier)) ->
     _patsIdefsCollect
   {-# INLINE rule138 #-}
   rule138 = \ ((_patsIerrors) :: Seq Error) ->
     _patsIerrors
   {-# INLINE rule139 #-}
   rule139 = \ ((_patsIcopy) :: Patterns) name_ ->
     Constr name_ _patsIcopy
   {-# INLINE rule140 #-}
   rule140 = \ ((_patsIoutput) :: Patterns) name_ ->
     Constr name_ _patsIoutput
   {-# INLINE rule141 #-}
   rule141 = \ _copy ->
     _copy
   {-# INLINE rule142 #-}
   rule142 = \ _output ->
     _output
   {-# INLINE rule143 #-}
   rule143 = \ ((_lhsIchildInhs) :: [(Identifier, Identifier)]) ->
     _lhsIchildInhs
   {-# INLINE rule144 #-}
   rule144 = \ ((_lhsIchildSyns) :: [(Identifier, Identifier)]) ->
     _lhsIchildSyns
   {-# INLINE rule145 #-}
   rule145 = \ ((_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# INLINE rule146 #-}
   rule146 = \ ((_lhsIdefs) :: Set (Identifier, Identifier)) ->
     _lhsIdefs
   {-# INLINE rule147 #-}
   rule147 = \ ((_lhsIforcedIrrefutables) :: AttrMap) ->
     _lhsIforcedIrrefutables
   {-# INLINE rule148 #-}
   rule148 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
{-# NOINLINE sem_Pattern_Product #-}
sem_Pattern_Product :: (Pos) -> T_Patterns  -> T_Pattern 
sem_Pattern_Product !arg_pos_ arg_pats_ = T_Pattern (return st29) where
   {-# NOINLINE st29 #-}
   !st29 = let
      v28 :: T_Pattern_v28 
      v28 = \ !(T_Pattern_vIn28 _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt) -> ( let
         _patsX32 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_pats_))
         (T_Patterns_vOut31 _patsIallAttributes _patsIcopy _patsIdefsCollect _patsIerrors _patsIoutput) = inv_Patterns_s32 _patsX32 (T_Patterns_vIn31 _patsOchildInhs _patsOchildSyns _patsOcon _patsOdefs _patsOforcedIrrefutables _patsOnt)
         _lhsOallAttributes :: AttrMap
         _lhsOallAttributes = rule149 _patsIallAttributes
         _lhsOdefsCollect :: Set (Identifier, Identifier)
         _lhsOdefsCollect = rule150 _patsIdefsCollect
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule151 _patsIerrors
         _copy = rule152 _patsIcopy arg_pos_
         _output = rule153 _patsIoutput arg_pos_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule154 _copy
         _lhsOoutput :: Pattern
         _lhsOoutput = rule155 _output
         _patsOchildInhs = rule156 _lhsIchildInhs
         _patsOchildSyns = rule157 _lhsIchildSyns
         _patsOcon = rule158 _lhsIcon
         _patsOdefs = rule159 _lhsIdefs
         _patsOforcedIrrefutables = rule160 _lhsIforcedIrrefutables
         _patsOnt = rule161 _lhsInt
         !__result_ = T_Pattern_vOut28 _lhsOallAttributes _lhsOcopy _lhsOdefsCollect _lhsOerrors _lhsOoutput
         in __result_ )
     in C_Pattern_s29 v28
   {-# INLINE rule149 #-}
   rule149 = \ ((_patsIallAttributes) :: AttrMap) ->
     _patsIallAttributes
   {-# INLINE rule150 #-}
   rule150 = \ ((_patsIdefsCollect) :: Set (Identifier, Identifier)) ->
     _patsIdefsCollect
   {-# INLINE rule151 #-}
   rule151 = \ ((_patsIerrors) :: Seq Error) ->
     _patsIerrors
   {-# INLINE rule152 #-}
   rule152 = \ ((_patsIcopy) :: Patterns) pos_ ->
     Product pos_ _patsIcopy
   {-# INLINE rule153 #-}
   rule153 = \ ((_patsIoutput) :: Patterns) pos_ ->
     Product pos_ _patsIoutput
   {-# INLINE rule154 #-}
   rule154 = \ _copy ->
     _copy
   {-# INLINE rule155 #-}
   rule155 = \ _output ->
     _output
   {-# INLINE rule156 #-}
   rule156 = \ ((_lhsIchildInhs) :: [(Identifier, Identifier)]) ->
     _lhsIchildInhs
   {-# INLINE rule157 #-}
   rule157 = \ ((_lhsIchildSyns) :: [(Identifier, Identifier)]) ->
     _lhsIchildSyns
   {-# INLINE rule158 #-}
   rule158 = \ ((_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# INLINE rule159 #-}
   rule159 = \ ((_lhsIdefs) :: Set (Identifier, Identifier)) ->
     _lhsIdefs
   {-# INLINE rule160 #-}
   rule160 = \ ((_lhsIforcedIrrefutables) :: AttrMap) ->
     _lhsIforcedIrrefutables
   {-# INLINE rule161 #-}
   rule161 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
{-# NOINLINE sem_Pattern_Alias #-}
sem_Pattern_Alias :: (Identifier) -> (Identifier) -> T_Pattern  -> T_Pattern 
sem_Pattern_Alias !arg_field_ !arg_attr_ arg_pat_ = T_Pattern (return st29) where
   {-# NOINLINE st29 #-}
   !st29 = let
      v28 :: T_Pattern_v28 
      v28 = \ !(T_Pattern_vIn28 _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt) -> ( let
         _patX29 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pat_))
         (T_Pattern_vOut28 _patIallAttributes _patIcopy _patIdefsCollect _patIerrors _patIoutput) = inv_Pattern_s29 _patX29 (T_Pattern_vIn28 _patOchildInhs _patOchildSyns _patOcon _patOdefs _patOforcedIrrefutables _patOnt)
         (_field',_err1) = rule162 _lhsIchildInhs _lhsIcon _lhsInt arg_attr_ arg_field_
         _err2 = rule163 _field' _lhsIcon _lhsIdefs _lhsInt arg_attr_ arg_field_
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule164 _err1 _err2 _patIerrors
         _output = rule165 _field' _patIoutput arg_attr_
         _def = rule166 arg_attr_ arg_field_
         _lhsOdefsCollect :: Set (Identifier, Identifier)
         _lhsOdefsCollect = rule167 _def _patIdefsCollect
         _lhsOallAttributes :: AttrMap
         _lhsOallAttributes = rule168 _lhsIcon _lhsInt _patIallAttributes arg_attr_ arg_field_
         _lhsOoutput :: Pattern
         _lhsOoutput = rule169 _lhsIcon _lhsIforcedIrrefutables _lhsInt _output arg_attr_ arg_field_
         _copy = rule170 _patIcopy arg_attr_ arg_field_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule171 _copy
         _patOchildInhs = rule172 _lhsIchildInhs
         _patOchildSyns = rule173 _lhsIchildSyns
         _patOcon = rule174 _lhsIcon
         _patOdefs = rule175 _lhsIdefs
         _patOforcedIrrefutables = rule176 _lhsIforcedIrrefutables
         _patOnt = rule177 _lhsInt
         !__result_ = T_Pattern_vOut28 _lhsOallAttributes _lhsOcopy _lhsOdefsCollect _lhsOerrors _lhsOoutput
         in __result_ )
     in C_Pattern_s29 v28
   {-# INLINE rule162 #-}
   {-# LINE 110 "./src-ag/Desugar.ag" #-}
   rule162 = \ ((_lhsIchildInhs) :: [(Identifier, Identifier)]) ((_lhsIcon) :: ConstructorIdent) ((_lhsInt) :: NontermIdent) attr_ field_ ->
                                 {-# LINE 110 "./src-ag/Desugar.ag" #-}
                                 maybeError field_ (UndefAttr _lhsInt _lhsIcon (Ident "<ANY>" (getPos field_)) attr_ True) $
                                   findField field_ attr_ _lhsIchildInhs
                                 {-# LINE 1532 "dist/build/Desugar.hs"#-}
   {-# INLINE rule163 #-}
   {-# LINE 112 "./src-ag/Desugar.ag" #-}
   rule163 = \ _field' ((_lhsIcon) :: ConstructorIdent) ((_lhsIdefs) :: Set (Identifier, Identifier)) ((_lhsInt) :: NontermIdent) attr_ field_ ->
                   {-# LINE 112 "./src-ag/Desugar.ag" #-}
                   if _field'     == field_
                   then Seq.empty
                   else if (_field'    , attr_) `Set.member` _lhsIdefs
                        then Seq.singleton $ DupRule _lhsInt _lhsIcon field_ attr_ _field'
                        else Seq.empty
                   {-# LINE 1542 "dist/build/Desugar.hs"#-}
   {-# INLINE rule164 #-}
   {-# LINE 117 "./src-ag/Desugar.ag" #-}
   rule164 = \ _err1 _err2 ((_patIerrors) :: Seq Error) ->
                     {-# LINE 117 "./src-ag/Desugar.ag" #-}
                     _err1     Seq.>< _err2     Seq.>< _patIerrors
                     {-# LINE 1548 "dist/build/Desugar.hs"#-}
   {-# INLINE rule165 #-}
   {-# LINE 118 "./src-ag/Desugar.ag" #-}
   rule165 = \ _field' ((_patIoutput) :: Pattern) attr_ ->
                     {-# LINE 118 "./src-ag/Desugar.ag" #-}
                     Alias _field'     attr_ _patIoutput
                     {-# LINE 1554 "dist/build/Desugar.hs"#-}
   {-# INLINE rule166 #-}
   {-# LINE 182 "./src-ag/Desugar.ag" #-}
   rule166 = \ attr_ field_ ->
                  {-# LINE 182 "./src-ag/Desugar.ag" #-}
                  Set.singleton (field_, attr_)
                  {-# LINE 1560 "dist/build/Desugar.hs"#-}
   {-# INLINE rule167 #-}
   {-# LINE 183 "./src-ag/Desugar.ag" #-}
   rule167 = \ _def ((_patIdefsCollect) :: Set (Identifier, Identifier)) ->
                          {-# LINE 183 "./src-ag/Desugar.ag" #-}
                          _def     `Set.union` _patIdefsCollect
                          {-# LINE 1566 "dist/build/Desugar.hs"#-}
   {-# INLINE rule168 #-}
   {-# LINE 200 "./src-ag/Desugar.ag" #-}
   rule168 = \ ((_lhsIcon) :: ConstructorIdent) ((_lhsInt) :: NontermIdent) ((_patIallAttributes) :: AttrMap) attr_ field_ ->
                            {-# LINE 200 "./src-ag/Desugar.ag" #-}
                            (Map.singleton _lhsInt $ Map.singleton _lhsIcon $ Set.singleton (field_, attr_)) `mergeAttributes` _patIallAttributes
                            {-# LINE 1572 "dist/build/Desugar.hs"#-}
   {-# INLINE rule169 #-}
   {-# LINE 219 "./src-ag/Desugar.ag" #-}
   rule169 = \ ((_lhsIcon) :: ConstructorIdent) ((_lhsIforcedIrrefutables) :: AttrMap) ((_lhsInt) :: NontermIdent) _output attr_ field_ ->
                     {-# LINE 219 "./src-ag/Desugar.ag" #-}
                     if Set.member (field_, attr_) $ Map.findWithDefault Set.empty _lhsIcon $ Map.findWithDefault Map.empty _lhsInt $ _lhsIforcedIrrefutables
                     then Irrefutable _output
                     else _output
                     {-# LINE 1580 "dist/build/Desugar.hs"#-}
   {-# INLINE rule170 #-}
   rule170 = \ ((_patIcopy) :: Pattern) attr_ field_ ->
     Alias field_ attr_ _patIcopy
   {-# INLINE rule171 #-}
   rule171 = \ _copy ->
     _copy
   {-# INLINE rule172 #-}
   rule172 = \ ((_lhsIchildInhs) :: [(Identifier, Identifier)]) ->
     _lhsIchildInhs
   {-# INLINE rule173 #-}
   rule173 = \ ((_lhsIchildSyns) :: [(Identifier, Identifier)]) ->
     _lhsIchildSyns
   {-# INLINE rule174 #-}
   rule174 = \ ((_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# INLINE rule175 #-}
   rule175 = \ ((_lhsIdefs) :: Set (Identifier, Identifier)) ->
     _lhsIdefs
   {-# INLINE rule176 #-}
   rule176 = \ ((_lhsIforcedIrrefutables) :: AttrMap) ->
     _lhsIforcedIrrefutables
   {-# INLINE rule177 #-}
   rule177 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
{-# NOINLINE sem_Pattern_Irrefutable #-}
sem_Pattern_Irrefutable :: T_Pattern  -> T_Pattern 
sem_Pattern_Irrefutable arg_pat_ = T_Pattern (return st29) where
   {-# NOINLINE st29 #-}
   !st29 = let
      v28 :: T_Pattern_v28 
      v28 = \ !(T_Pattern_vIn28 _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt) -> ( let
         _patX29 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pat_))
         (T_Pattern_vOut28 _patIallAttributes _patIcopy _patIdefsCollect _patIerrors _patIoutput) = inv_Pattern_s29 _patX29 (T_Pattern_vIn28 _patOchildInhs _patOchildSyns _patOcon _patOdefs _patOforcedIrrefutables _patOnt)
         _lhsOallAttributes :: AttrMap
         _lhsOallAttributes = rule178  ()
         _lhsOdefsCollect :: Set (Identifier, Identifier)
         _lhsOdefsCollect = rule179 _patIdefsCollect
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule180 _patIerrors
         _copy = rule181 _patIcopy
         _output = rule182 _patIoutput
         _lhsOcopy :: Pattern
         _lhsOcopy = rule183 _copy
         _lhsOoutput :: Pattern
         _lhsOoutput = rule184 _output
         _patOchildInhs = rule185 _lhsIchildInhs
         _patOchildSyns = rule186 _lhsIchildSyns
         _patOcon = rule187 _lhsIcon
         _patOdefs = rule188 _lhsIdefs
         _patOforcedIrrefutables = rule189 _lhsIforcedIrrefutables
         _patOnt = rule190 _lhsInt
         !__result_ = T_Pattern_vOut28 _lhsOallAttributes _lhsOcopy _lhsOdefsCollect _lhsOerrors _lhsOoutput
         in __result_ )
     in C_Pattern_s29 v28
   {-# INLINE rule178 #-}
   {-# LINE 202 "./src-ag/Desugar.ag" #-}
   rule178 = \  (_ :: ()) ->
                            {-# LINE 202 "./src-ag/Desugar.ag" #-}
                            Map.empty
                            {-# LINE 1640 "dist/build/Desugar.hs"#-}
   {-# INLINE rule179 #-}
   rule179 = \ ((_patIdefsCollect) :: Set (Identifier, Identifier)) ->
     _patIdefsCollect
   {-# INLINE rule180 #-}
   rule180 = \ ((_patIerrors) :: Seq Error) ->
     _patIerrors
   {-# INLINE rule181 #-}
   rule181 = \ ((_patIcopy) :: Pattern) ->
     Irrefutable _patIcopy
   {-# INLINE rule182 #-}
   rule182 = \ ((_patIoutput) :: Pattern) ->
     Irrefutable _patIoutput
   {-# INLINE rule183 #-}
   rule183 = \ _copy ->
     _copy
   {-# INLINE rule184 #-}
   rule184 = \ _output ->
     _output
   {-# INLINE rule185 #-}
   rule185 = \ ((_lhsIchildInhs) :: [(Identifier, Identifier)]) ->
     _lhsIchildInhs
   {-# INLINE rule186 #-}
   rule186 = \ ((_lhsIchildSyns) :: [(Identifier, Identifier)]) ->
     _lhsIchildSyns
   {-# INLINE rule187 #-}
   rule187 = \ ((_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# INLINE rule188 #-}
   rule188 = \ ((_lhsIdefs) :: Set (Identifier, Identifier)) ->
     _lhsIdefs
   {-# INLINE rule189 #-}
   rule189 = \ ((_lhsIforcedIrrefutables) :: AttrMap) ->
     _lhsIforcedIrrefutables
   {-# INLINE rule190 #-}
   rule190 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
{-# NOINLINE sem_Pattern_Underscore #-}
sem_Pattern_Underscore :: (Pos) -> T_Pattern 
sem_Pattern_Underscore !arg_pos_ = T_Pattern (return st29) where
   {-# NOINLINE st29 #-}
   !st29 = let
      v28 :: T_Pattern_v28 
      v28 = \ !(T_Pattern_vIn28 _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt) -> ( let
         _lhsOallAttributes :: AttrMap
         _lhsOallAttributes = rule191  ()
         _lhsOdefsCollect :: Set (Identifier, Identifier)
         _lhsOdefsCollect = rule192  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule193  ()
         _copy = rule194 arg_pos_
         _output = rule195 arg_pos_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule196 _copy
         _lhsOoutput :: Pattern
         _lhsOoutput = rule197 _output
         !__result_ = T_Pattern_vOut28 _lhsOallAttributes _lhsOcopy _lhsOdefsCollect _lhsOerrors _lhsOoutput
         in __result_ )
     in C_Pattern_s29 v28
   {-# INLINE rule191 #-}
   rule191 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule192 #-}
   rule192 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule193 #-}
   rule193 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule194 #-}
   rule194 = \ pos_ ->
     Underscore pos_
   {-# INLINE rule195 #-}
   rule195 = \ pos_ ->
     Underscore pos_
   {-# INLINE rule196 #-}
   rule196 = \ _copy ->
     _copy
   {-# INLINE rule197 #-}
   rule197 = \ _output ->
     _output

-- Patterns ----------------------------------------------------
-- wrapper
data Inh_Patterns  = Inh_Patterns { childInhs_Inh_Patterns :: !([(Identifier, Identifier)]), childSyns_Inh_Patterns :: !([(Identifier, Identifier)]), con_Inh_Patterns :: !(ConstructorIdent), defs_Inh_Patterns :: !(Set (Identifier, Identifier)), forcedIrrefutables_Inh_Patterns :: !(AttrMap), nt_Inh_Patterns :: !(NontermIdent) }
data Syn_Patterns  = Syn_Patterns { allAttributes_Syn_Patterns :: !(AttrMap), copy_Syn_Patterns :: !(Patterns), defsCollect_Syn_Patterns :: !(Set (Identifier, Identifier)), errors_Syn_Patterns :: !(Seq Error), output_Syn_Patterns :: !(Patterns) }
{-# INLINABLE wrap_Patterns #-}
wrap_Patterns :: T_Patterns  -> Inh_Patterns  -> (Syn_Patterns )
wrap_Patterns !(T_Patterns act) !(Inh_Patterns _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_Patterns_vIn31 _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt
        !(T_Patterns_vOut31 _lhsOallAttributes _lhsOcopy _lhsOdefsCollect _lhsOerrors _lhsOoutput) <- return (inv_Patterns_s32 sem arg)
        return (Syn_Patterns _lhsOallAttributes _lhsOcopy _lhsOdefsCollect _lhsOerrors _lhsOoutput)
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
data T_Patterns_vIn31  = T_Patterns_vIn31 ([(Identifier, Identifier)]) ([(Identifier, Identifier)]) (ConstructorIdent) (Set (Identifier, Identifier)) (AttrMap) (NontermIdent)
data T_Patterns_vOut31  = T_Patterns_vOut31 (AttrMap) (Patterns) (Set (Identifier, Identifier)) (Seq Error) (Patterns)
{-# NOINLINE sem_Patterns_Cons #-}
sem_Patterns_Cons :: T_Pattern  -> T_Patterns  -> T_Patterns 
sem_Patterns_Cons arg_hd_ arg_tl_ = T_Patterns (return st32) where
   {-# NOINLINE st32 #-}
   !st32 = let
      v31 :: T_Patterns_v31 
      v31 = \ !(T_Patterns_vIn31 _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt) -> ( let
         _hdX29 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_hd_))
         _tlX32 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_tl_))
         (T_Pattern_vOut28 _hdIallAttributes _hdIcopy _hdIdefsCollect _hdIerrors _hdIoutput) = inv_Pattern_s29 _hdX29 (T_Pattern_vIn28 _hdOchildInhs _hdOchildSyns _hdOcon _hdOdefs _hdOforcedIrrefutables _hdOnt)
         (T_Patterns_vOut31 _tlIallAttributes _tlIcopy _tlIdefsCollect _tlIerrors _tlIoutput) = inv_Patterns_s32 _tlX32 (T_Patterns_vIn31 _tlOchildInhs _tlOchildSyns _tlOcon _tlOdefs _tlOforcedIrrefutables _tlOnt)
         _lhsOallAttributes :: AttrMap
         _lhsOallAttributes = rule198 _hdIallAttributes _tlIallAttributes
         _lhsOdefsCollect :: Set (Identifier, Identifier)
         _lhsOdefsCollect = rule199 _hdIdefsCollect _tlIdefsCollect
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule200 _hdIerrors _tlIerrors
         _copy = rule201 _hdIcopy _tlIcopy
         _output = rule202 _hdIoutput _tlIoutput
         _lhsOcopy :: Patterns
         _lhsOcopy = rule203 _copy
         _lhsOoutput :: Patterns
         _lhsOoutput = rule204 _output
         _hdOchildInhs = rule205 _lhsIchildInhs
         _hdOchildSyns = rule206 _lhsIchildSyns
         _hdOcon = rule207 _lhsIcon
         _hdOdefs = rule208 _lhsIdefs
         _hdOforcedIrrefutables = rule209 _lhsIforcedIrrefutables
         _hdOnt = rule210 _lhsInt
         _tlOchildInhs = rule211 _lhsIchildInhs
         _tlOchildSyns = rule212 _lhsIchildSyns
         _tlOcon = rule213 _lhsIcon
         _tlOdefs = rule214 _lhsIdefs
         _tlOforcedIrrefutables = rule215 _lhsIforcedIrrefutables
         _tlOnt = rule216 _lhsInt
         !__result_ = T_Patterns_vOut31 _lhsOallAttributes _lhsOcopy _lhsOdefsCollect _lhsOerrors _lhsOoutput
         in __result_ )
     in C_Patterns_s32 v31
   {-# INLINE rule198 #-}
   rule198 = \ ((_hdIallAttributes) :: AttrMap) ((_tlIallAttributes) :: AttrMap) ->
     _hdIallAttributes `mergeAttributes` _tlIallAttributes
   {-# INLINE rule199 #-}
   rule199 = \ ((_hdIdefsCollect) :: Set (Identifier, Identifier)) ((_tlIdefsCollect) :: Set (Identifier, Identifier)) ->
     _hdIdefsCollect `Set.union` _tlIdefsCollect
   {-# INLINE rule200 #-}
   rule200 = \ ((_hdIerrors) :: Seq Error) ((_tlIerrors) :: Seq Error) ->
     _hdIerrors Seq.>< _tlIerrors
   {-# INLINE rule201 #-}
   rule201 = \ ((_hdIcopy) :: Pattern) ((_tlIcopy) :: Patterns) ->
     (:) _hdIcopy _tlIcopy
   {-# INLINE rule202 #-}
   rule202 = \ ((_hdIoutput) :: Pattern) ((_tlIoutput) :: Patterns) ->
     (:) _hdIoutput _tlIoutput
   {-# INLINE rule203 #-}
   rule203 = \ _copy ->
     _copy
   {-# INLINE rule204 #-}
   rule204 = \ _output ->
     _output
   {-# INLINE rule205 #-}
   rule205 = \ ((_lhsIchildInhs) :: [(Identifier, Identifier)]) ->
     _lhsIchildInhs
   {-# INLINE rule206 #-}
   rule206 = \ ((_lhsIchildSyns) :: [(Identifier, Identifier)]) ->
     _lhsIchildSyns
   {-# INLINE rule207 #-}
   rule207 = \ ((_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# INLINE rule208 #-}
   rule208 = \ ((_lhsIdefs) :: Set (Identifier, Identifier)) ->
     _lhsIdefs
   {-# INLINE rule209 #-}
   rule209 = \ ((_lhsIforcedIrrefutables) :: AttrMap) ->
     _lhsIforcedIrrefutables
   {-# INLINE rule210 #-}
   rule210 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule211 #-}
   rule211 = \ ((_lhsIchildInhs) :: [(Identifier, Identifier)]) ->
     _lhsIchildInhs
   {-# INLINE rule212 #-}
   rule212 = \ ((_lhsIchildSyns) :: [(Identifier, Identifier)]) ->
     _lhsIchildSyns
   {-# INLINE rule213 #-}
   rule213 = \ ((_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# INLINE rule214 #-}
   rule214 = \ ((_lhsIdefs) :: Set (Identifier, Identifier)) ->
     _lhsIdefs
   {-# INLINE rule215 #-}
   rule215 = \ ((_lhsIforcedIrrefutables) :: AttrMap) ->
     _lhsIforcedIrrefutables
   {-# INLINE rule216 #-}
   rule216 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
{-# NOINLINE sem_Patterns_Nil #-}
sem_Patterns_Nil ::  T_Patterns 
sem_Patterns_Nil  = T_Patterns (return st32) where
   {-# NOINLINE st32 #-}
   !st32 = let
      v31 :: T_Patterns_v31 
      v31 = \ !(T_Patterns_vIn31 _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt) -> ( let
         _lhsOallAttributes :: AttrMap
         _lhsOallAttributes = rule217  ()
         _lhsOdefsCollect :: Set (Identifier, Identifier)
         _lhsOdefsCollect = rule218  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule219  ()
         _copy = rule220  ()
         _output = rule221  ()
         _lhsOcopy :: Patterns
         _lhsOcopy = rule222 _copy
         _lhsOoutput :: Patterns
         _lhsOoutput = rule223 _output
         !__result_ = T_Patterns_vOut31 _lhsOallAttributes _lhsOcopy _lhsOdefsCollect _lhsOerrors _lhsOoutput
         in __result_ )
     in C_Patterns_s32 v31
   {-# INLINE rule217 #-}
   rule217 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule218 #-}
   rule218 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule219 #-}
   rule219 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule220 #-}
   rule220 = \  (_ :: ()) ->
     []
   {-# INLINE rule221 #-}
   rule221 = \  (_ :: ()) ->
     []
   {-# INLINE rule222 #-}
   rule222 = \ _copy ->
     _copy
   {-# INLINE rule223 #-}
   rule223 = \ _output ->
     _output

-- Production --------------------------------------------------
-- wrapper
data Inh_Production  = Inh_Production { augmentsIn_Inh_Production :: !(Map ConstructorIdent (Map Identifier [Expression])), forcedIrrefutables_Inh_Production :: !(AttrMap), inhMap_Inh_Production :: !(Map Identifier Attributes), mainName_Inh_Production :: !(String), nt_Inh_Production :: !(NontermIdent), options_Inh_Production :: !(Options), synMap_Inh_Production :: !(Map Identifier Attributes) }
data Syn_Production  = Syn_Production { allAttributes_Syn_Production :: !(AttrMap), augmentsOut_Syn_Production :: !(Map ConstructorIdent (Map Identifier [Expression])), errors_Syn_Production :: !(Seq Error), output_Syn_Production :: !(Production) }
{-# INLINABLE wrap_Production #-}
wrap_Production :: T_Production  -> Inh_Production  -> (Syn_Production )
wrap_Production !(T_Production act) !(Inh_Production _lhsIaugmentsIn _lhsIforcedIrrefutables _lhsIinhMap _lhsImainName _lhsInt _lhsIoptions _lhsIsynMap) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_Production_vIn34 _lhsIaugmentsIn _lhsIforcedIrrefutables _lhsIinhMap _lhsImainName _lhsInt _lhsIoptions _lhsIsynMap
        !(T_Production_vOut34 _lhsOallAttributes _lhsOaugmentsOut _lhsOerrors _lhsOoutput) <- return (inv_Production_s35 sem arg)
        return (Syn_Production _lhsOallAttributes _lhsOaugmentsOut _lhsOerrors _lhsOoutput)
   )

-- cata
{-# INLINE sem_Production #-}
sem_Production :: Production  -> T_Production 
sem_Production ( Production !con_ !params_ !constraints_ children_ rules_ typeSigs_ !macro_ ) = sem_Production_Production con_ params_ constraints_ ( sem_Children children_ ) ( sem_Rules rules_ ) ( sem_TypeSigs typeSigs_ ) macro_

-- semantic domain
newtype T_Production  = T_Production {
                                     attach_T_Production :: Identity (T_Production_s35 )
                                     }
newtype T_Production_s35  = C_Production_s35 {
                                             inv_Production_s35 :: (T_Production_v34 )
                                             }
data T_Production_s36  = C_Production_s36
type T_Production_v34  = (T_Production_vIn34 ) -> (T_Production_vOut34 )
data T_Production_vIn34  = T_Production_vIn34 (Map ConstructorIdent (Map Identifier [Expression])) (AttrMap) (Map Identifier Attributes) (String) (NontermIdent) (Options) (Map Identifier Attributes)
data T_Production_vOut34  = T_Production_vOut34 (AttrMap) (Map ConstructorIdent (Map Identifier [Expression])) (Seq Error) (Production)
{-# NOINLINE sem_Production_Production #-}
sem_Production_Production :: (ConstructorIdent) -> ([Identifier]) -> ([Type]) -> T_Children  -> T_Rules  -> T_TypeSigs  -> (MaybeMacro) -> T_Production 
sem_Production_Production !arg_con_ !arg_params_ !arg_constraints_ arg_children_ arg_rules_ arg_typeSigs_ !arg_macro_ = T_Production (return st35) where
   {-# NOINLINE st35 #-}
   !st35 = let
      v34 :: T_Production_v34 
      v34 = \ !(T_Production_vIn34 _lhsIaugmentsIn _lhsIforcedIrrefutables _lhsIinhMap _lhsImainName _lhsInt _lhsIoptions _lhsIsynMap) -> ( let
         _childrenX5 = Control.Monad.Identity.runIdentity (attach_T_Children (arg_children_))
         _rulesX44 = Control.Monad.Identity.runIdentity (attach_T_Rules (arg_rules_))
         _typeSigsX50 = Control.Monad.Identity.runIdentity (attach_T_TypeSigs (arg_typeSigs_))
         (T_Children_vOut4 _childrenIchildInhs _childrenIchildSyns _childrenIoutput) = inv_Children_s5 _childrenX5 (T_Children_vIn4 _childrenOinhMap _childrenOmainName _childrenOoptions _childrenOsynMap)
         (T_Rules_vOut43 _rulesIallAttributes _rulesIdefsCollect _rulesIerrors _rulesIoutput) = inv_Rules_s44 _rulesX44 (T_Rules_vIn43 _rulesOchildInhs _rulesOchildSyns _rulesOcon _rulesOdefs _rulesOforcedIrrefutables _rulesOnt _rulesOoptions)
         (T_TypeSigs_vOut49 _typeSigsIoutput) = inv_TypeSigs_s50 _typeSigsX50 (T_TypeSigs_vIn49 )
         _rulesOcon = rule224 arg_con_
         _rulesOdefs = rule225 _rulesIdefsCollect
         _augmentsIn = rule226 _lhsIaugmentsIn arg_con_
         _augmentsOut = rule227 _augmentsOut1 arg_con_
         (_augmentErrs,_augmentsOut1) = rule228 _augmentsIn _childrenIchildInhs _childrenIchildSyns _lhsInt _lhsIoptions arg_con_
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule229 _augmentErrs _rulesIerrors
         _lhsOallAttributes :: AttrMap
         _lhsOallAttributes = rule230 _rulesIallAttributes
         _lhsOaugmentsOut :: Map ConstructorIdent (Map Identifier [Expression])
         _lhsOaugmentsOut = rule231 _augmentsOut
         _output = rule232 _childrenIoutput _rulesIoutput _typeSigsIoutput arg_con_ arg_constraints_ arg_macro_ arg_params_
         _lhsOoutput :: Production
         _lhsOoutput = rule233 _output
         _childrenOinhMap = rule234 _lhsIinhMap
         _childrenOmainName = rule235 _lhsImainName
         _childrenOoptions = rule236 _lhsIoptions
         _childrenOsynMap = rule237 _lhsIsynMap
         _rulesOchildInhs = rule238 _childrenIchildInhs
         _rulesOchildSyns = rule239 _childrenIchildSyns
         _rulesOforcedIrrefutables = rule240 _lhsIforcedIrrefutables
         _rulesOnt = rule241 _lhsInt
         _rulesOoptions = rule242 _lhsIoptions
         !__result_ = T_Production_vOut34 _lhsOallAttributes _lhsOaugmentsOut _lhsOerrors _lhsOoutput
         in __result_ )
     in C_Production_s35 v34
   {-# INLINE rule224 #-}
   {-# LINE 161 "./src-ag/Desugar.ag" #-}
   rule224 = \ con_ ->
                    {-# LINE 161 "./src-ag/Desugar.ag" #-}
                    con_
                    {-# LINE 1964 "dist/build/Desugar.hs"#-}
   {-# INLINE rule225 #-}
   {-# LINE 188 "./src-ag/Desugar.ag" #-}
   rule225 = \ ((_rulesIdefsCollect) :: Set (Identifier, Identifier)) ->
                     {-# LINE 188 "./src-ag/Desugar.ag" #-}
                     _rulesIdefsCollect
                     {-# LINE 1970 "dist/build/Desugar.hs"#-}
   {-# INLINE rule226 #-}
   {-# LINE 244 "./src-ag/Desugar.ag" #-}
   rule226 = \ ((_lhsIaugmentsIn) :: Map ConstructorIdent (Map Identifier [Expression])) con_ ->
                         {-# LINE 244 "./src-ag/Desugar.ag" #-}
                         Map.findWithDefault Map.empty con_ _lhsIaugmentsIn
                         {-# LINE 1976 "dist/build/Desugar.hs"#-}
   {-# INLINE rule227 #-}
   {-# LINE 245 "./src-ag/Desugar.ag" #-}
   rule227 = \ _augmentsOut1 con_ ->
                          {-# LINE 245 "./src-ag/Desugar.ag" #-}
                          Map.singleton con_ _augmentsOut1
                          {-# LINE 1982 "dist/build/Desugar.hs"#-}
   {-# INLINE rule228 #-}
   {-# LINE 247 "./src-ag/Desugar.ag" #-}
   rule228 = \ _augmentsIn ((_childrenIchildInhs) :: [(Identifier, Identifier)]) ((_childrenIchildSyns) :: [(Identifier, Identifier)]) ((_lhsInt) :: NontermIdent) ((_lhsIoptions) :: Options) con_ ->
                                              {-# LINE 247 "./src-ag/Desugar.ag" #-}
                                              Map.mapAccum (desugarExprs _lhsIoptions _lhsInt con_ _childrenIchildInhs _childrenIchildSyns) Seq.empty _augmentsIn
                                              {-# LINE 1988 "dist/build/Desugar.hs"#-}
   {-# INLINE rule229 #-}
   {-# LINE 283 "./src-ag/Desugar.ag" #-}
   rule229 = \ _augmentErrs ((_rulesIerrors) :: Seq Error) ->
                     {-# LINE 283 "./src-ag/Desugar.ag" #-}
                     _rulesIerrors Seq.>< _augmentErrs
                     {-# LINE 1994 "dist/build/Desugar.hs"#-}
   {-# INLINE rule230 #-}
   rule230 = \ ((_rulesIallAttributes) :: AttrMap) ->
     _rulesIallAttributes
   {-# INLINE rule231 #-}
   rule231 = \ _augmentsOut ->
     _augmentsOut
   {-# INLINE rule232 #-}
   rule232 = \ ((_childrenIoutput) :: Children) ((_rulesIoutput) :: Rules) ((_typeSigsIoutput) :: TypeSigs) con_ constraints_ macro_ params_ ->
     Production con_ params_ constraints_ _childrenIoutput _rulesIoutput _typeSigsIoutput macro_
   {-# INLINE rule233 #-}
   rule233 = \ _output ->
     _output
   {-# INLINE rule234 #-}
   rule234 = \ ((_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# INLINE rule235 #-}
   rule235 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule236 #-}
   rule236 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule237 #-}
   rule237 = \ ((_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
   {-# INLINE rule238 #-}
   rule238 = \ ((_childrenIchildInhs) :: [(Identifier, Identifier)]) ->
     _childrenIchildInhs
   {-# INLINE rule239 #-}
   rule239 = \ ((_childrenIchildSyns) :: [(Identifier, Identifier)]) ->
     _childrenIchildSyns
   {-# INLINE rule240 #-}
   rule240 = \ ((_lhsIforcedIrrefutables) :: AttrMap) ->
     _lhsIforcedIrrefutables
   {-# INLINE rule241 #-}
   rule241 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule242 #-}
   rule242 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions

-- Productions -------------------------------------------------
-- wrapper
data Inh_Productions  = Inh_Productions { augmentsIn_Inh_Productions :: !(Map ConstructorIdent (Map Identifier [Expression])), forcedIrrefutables_Inh_Productions :: !(AttrMap), inhMap_Inh_Productions :: !(Map Identifier Attributes), mainName_Inh_Productions :: !(String), nt_Inh_Productions :: !(NontermIdent), options_Inh_Productions :: !(Options), synMap_Inh_Productions :: !(Map Identifier Attributes) }
data Syn_Productions  = Syn_Productions { allAttributes_Syn_Productions :: !(AttrMap), augmentsOut_Syn_Productions :: !(Map ConstructorIdent (Map Identifier [Expression])), errors_Syn_Productions :: !(Seq Error), output_Syn_Productions :: !(Productions) }
{-# INLINABLE wrap_Productions #-}
wrap_Productions :: T_Productions  -> Inh_Productions  -> (Syn_Productions )
wrap_Productions !(T_Productions act) !(Inh_Productions _lhsIaugmentsIn _lhsIforcedIrrefutables _lhsIinhMap _lhsImainName _lhsInt _lhsIoptions _lhsIsynMap) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_Productions_vIn37 _lhsIaugmentsIn _lhsIforcedIrrefutables _lhsIinhMap _lhsImainName _lhsInt _lhsIoptions _lhsIsynMap
        !(T_Productions_vOut37 _lhsOallAttributes _lhsOaugmentsOut _lhsOerrors _lhsOoutput) <- return (inv_Productions_s38 sem arg)
        return (Syn_Productions _lhsOallAttributes _lhsOaugmentsOut _lhsOerrors _lhsOoutput)
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
data T_Productions_vIn37  = T_Productions_vIn37 (Map ConstructorIdent (Map Identifier [Expression])) (AttrMap) (Map Identifier Attributes) (String) (NontermIdent) (Options) (Map Identifier Attributes)
data T_Productions_vOut37  = T_Productions_vOut37 (AttrMap) (Map ConstructorIdent (Map Identifier [Expression])) (Seq Error) (Productions)
{-# NOINLINE sem_Productions_Cons #-}
sem_Productions_Cons :: T_Production  -> T_Productions  -> T_Productions 
sem_Productions_Cons arg_hd_ arg_tl_ = T_Productions (return st38) where
   {-# NOINLINE st38 #-}
   !st38 = let
      v37 :: T_Productions_v37 
      v37 = \ !(T_Productions_vIn37 _lhsIaugmentsIn _lhsIforcedIrrefutables _lhsIinhMap _lhsImainName _lhsInt _lhsIoptions _lhsIsynMap) -> ( let
         _hdX35 = Control.Monad.Identity.runIdentity (attach_T_Production (arg_hd_))
         _tlX38 = Control.Monad.Identity.runIdentity (attach_T_Productions (arg_tl_))
         (T_Production_vOut34 _hdIallAttributes _hdIaugmentsOut _hdIerrors _hdIoutput) = inv_Production_s35 _hdX35 (T_Production_vIn34 _hdOaugmentsIn _hdOforcedIrrefutables _hdOinhMap _hdOmainName _hdOnt _hdOoptions _hdOsynMap)
         (T_Productions_vOut37 _tlIallAttributes _tlIaugmentsOut _tlIerrors _tlIoutput) = inv_Productions_s38 _tlX38 (T_Productions_vIn37 _tlOaugmentsIn _tlOforcedIrrefutables _tlOinhMap _tlOmainName _tlOnt _tlOoptions _tlOsynMap)
         _lhsOallAttributes :: AttrMap
         _lhsOallAttributes = rule243 _hdIallAttributes _tlIallAttributes
         _lhsOaugmentsOut :: Map ConstructorIdent (Map Identifier [Expression])
         _lhsOaugmentsOut = rule244 _hdIaugmentsOut _tlIaugmentsOut
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule245 _hdIerrors _tlIerrors
         _output = rule246 _hdIoutput _tlIoutput
         _lhsOoutput :: Productions
         _lhsOoutput = rule247 _output
         _hdOaugmentsIn = rule248 _lhsIaugmentsIn
         _hdOforcedIrrefutables = rule249 _lhsIforcedIrrefutables
         _hdOinhMap = rule250 _lhsIinhMap
         _hdOmainName = rule251 _lhsImainName
         _hdOnt = rule252 _lhsInt
         _hdOoptions = rule253 _lhsIoptions
         _hdOsynMap = rule254 _lhsIsynMap
         _tlOaugmentsIn = rule255 _lhsIaugmentsIn
         _tlOforcedIrrefutables = rule256 _lhsIforcedIrrefutables
         _tlOinhMap = rule257 _lhsIinhMap
         _tlOmainName = rule258 _lhsImainName
         _tlOnt = rule259 _lhsInt
         _tlOoptions = rule260 _lhsIoptions
         _tlOsynMap = rule261 _lhsIsynMap
         !__result_ = T_Productions_vOut37 _lhsOallAttributes _lhsOaugmentsOut _lhsOerrors _lhsOoutput
         in __result_ )
     in C_Productions_s38 v37
   {-# INLINE rule243 #-}
   rule243 = \ ((_hdIallAttributes) :: AttrMap) ((_tlIallAttributes) :: AttrMap) ->
     _hdIallAttributes `mergeAttributes` _tlIallAttributes
   {-# INLINE rule244 #-}
   rule244 = \ ((_hdIaugmentsOut) :: Map ConstructorIdent (Map Identifier [Expression])) ((_tlIaugmentsOut) :: Map ConstructorIdent (Map Identifier [Expression])) ->
     _hdIaugmentsOut `Map.union` _tlIaugmentsOut
   {-# INLINE rule245 #-}
   rule245 = \ ((_hdIerrors) :: Seq Error) ((_tlIerrors) :: Seq Error) ->
     _hdIerrors Seq.>< _tlIerrors
   {-# INLINE rule246 #-}
   rule246 = \ ((_hdIoutput) :: Production) ((_tlIoutput) :: Productions) ->
     (:) _hdIoutput _tlIoutput
   {-# INLINE rule247 #-}
   rule247 = \ _output ->
     _output
   {-# INLINE rule248 #-}
   rule248 = \ ((_lhsIaugmentsIn) :: Map ConstructorIdent (Map Identifier [Expression])) ->
     _lhsIaugmentsIn
   {-# INLINE rule249 #-}
   rule249 = \ ((_lhsIforcedIrrefutables) :: AttrMap) ->
     _lhsIforcedIrrefutables
   {-# INLINE rule250 #-}
   rule250 = \ ((_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# INLINE rule251 #-}
   rule251 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule252 #-}
   rule252 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule253 #-}
   rule253 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule254 #-}
   rule254 = \ ((_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
   {-# INLINE rule255 #-}
   rule255 = \ ((_lhsIaugmentsIn) :: Map ConstructorIdent (Map Identifier [Expression])) ->
     _lhsIaugmentsIn
   {-# INLINE rule256 #-}
   rule256 = \ ((_lhsIforcedIrrefutables) :: AttrMap) ->
     _lhsIforcedIrrefutables
   {-# INLINE rule257 #-}
   rule257 = \ ((_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# INLINE rule258 #-}
   rule258 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule259 #-}
   rule259 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule260 #-}
   rule260 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule261 #-}
   rule261 = \ ((_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
{-# NOINLINE sem_Productions_Nil #-}
sem_Productions_Nil ::  T_Productions 
sem_Productions_Nil  = T_Productions (return st38) where
   {-# NOINLINE st38 #-}
   !st38 = let
      v37 :: T_Productions_v37 
      v37 = \ !(T_Productions_vIn37 _lhsIaugmentsIn _lhsIforcedIrrefutables _lhsIinhMap _lhsImainName _lhsInt _lhsIoptions _lhsIsynMap) -> ( let
         _lhsOallAttributes :: AttrMap
         _lhsOallAttributes = rule262  ()
         _lhsOaugmentsOut :: Map ConstructorIdent (Map Identifier [Expression])
         _lhsOaugmentsOut = rule263  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule264  ()
         _output = rule265  ()
         _lhsOoutput :: Productions
         _lhsOoutput = rule266 _output
         !__result_ = T_Productions_vOut37 _lhsOallAttributes _lhsOaugmentsOut _lhsOerrors _lhsOoutput
         in __result_ )
     in C_Productions_s38 v37
   {-# INLINE rule262 #-}
   rule262 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule263 #-}
   rule263 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule264 #-}
   rule264 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule265 #-}
   rule265 = \  (_ :: ()) ->
     []
   {-# INLINE rule266 #-}
   rule266 = \ _output ->
     _output

-- Rule --------------------------------------------------------
-- wrapper
data Inh_Rule  = Inh_Rule { childInhs_Inh_Rule :: !([(Identifier, Identifier)]), childSyns_Inh_Rule :: !([(Identifier, Identifier)]), con_Inh_Rule :: !(ConstructorIdent), defs_Inh_Rule :: !(Set (Identifier, Identifier)), forcedIrrefutables_Inh_Rule :: !(AttrMap), nt_Inh_Rule :: !(NontermIdent), options_Inh_Rule :: !(Options) }
data Syn_Rule  = Syn_Rule { allAttributes_Syn_Rule :: !(AttrMap), defsCollect_Syn_Rule :: !(Set (Identifier, Identifier)), errors_Syn_Rule :: !(Seq Error), output_Syn_Rule :: !(Rule) }
{-# INLINABLE wrap_Rule #-}
wrap_Rule :: T_Rule  -> Inh_Rule  -> (Syn_Rule )
wrap_Rule !(T_Rule act) !(Inh_Rule _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt _lhsIoptions) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_Rule_vIn40 _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt _lhsIoptions
        !(T_Rule_vOut40 _lhsOallAttributes _lhsOdefsCollect _lhsOerrors _lhsOoutput) <- return (inv_Rule_s41 sem arg)
        return (Syn_Rule _lhsOallAttributes _lhsOdefsCollect _lhsOerrors _lhsOoutput)
   )

-- cata
{-# INLINE sem_Rule #-}
sem_Rule :: Rule  -> T_Rule 
sem_Rule ( Rule !mbName_ pattern_ rhs_ !owrt_ !origin_ !explicit_ !pure_ !identity_ !mbError_ !eager_ ) = sem_Rule_Rule mbName_ ( sem_Pattern pattern_ ) ( sem_Expression rhs_ ) owrt_ origin_ explicit_ pure_ identity_ mbError_ eager_

-- semantic domain
newtype T_Rule  = T_Rule {
                         attach_T_Rule :: Identity (T_Rule_s41 )
                         }
newtype T_Rule_s41  = C_Rule_s41 {
                                 inv_Rule_s41 :: (T_Rule_v40 )
                                 }
data T_Rule_s42  = C_Rule_s42
type T_Rule_v40  = (T_Rule_vIn40 ) -> (T_Rule_vOut40 )
data T_Rule_vIn40  = T_Rule_vIn40 ([(Identifier, Identifier)]) ([(Identifier, Identifier)]) (ConstructorIdent) (Set (Identifier, Identifier)) (AttrMap) (NontermIdent) (Options)
data T_Rule_vOut40  = T_Rule_vOut40 (AttrMap) (Set (Identifier, Identifier)) (Seq Error) (Rule)
{-# NOINLINE sem_Rule_Rule #-}
sem_Rule_Rule :: (Maybe Identifier) -> T_Pattern  -> T_Expression  -> (Bool) -> (String) -> (Bool) -> (Bool) -> (Bool) -> (Maybe Error) -> (Bool) -> T_Rule 
sem_Rule_Rule !arg_mbName_ arg_pattern_ arg_rhs_ !arg_owrt_ !arg_origin_ !arg_explicit_ !arg_pure_ !arg_identity_ !arg_mbError_ !arg_eager_ = T_Rule (return st41) where
   {-# NOINLINE st41 #-}
   !st41 = let
      v40 :: T_Rule_v40 
      v40 = \ !(T_Rule_vIn40 _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt _lhsIoptions) -> ( let
         _patternX29 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pattern_))
         _rhsX8 = Control.Monad.Identity.runIdentity (attach_T_Expression (arg_rhs_))
         (T_Pattern_vOut28 _patternIallAttributes _patternIcopy _patternIdefsCollect _patternIerrors _patternIoutput) = inv_Pattern_s29 _patternX29 (T_Pattern_vIn28 _patternOchildInhs _patternOchildSyns _patternOcon _patternOdefs _patternOforcedIrrefutables _patternOnt)
         (T_Expression_vOut7 _rhsIerrors _rhsIoutput) = inv_Expression_s8 _rhsX8 (T_Expression_vIn7 _rhsOchildInhs _rhsOchildSyns _rhsOcon _rhsOnt _rhsOoptions _rhsOruleDescr)
         _ruleDescr = rule267 _lhsIcon _lhsInt _patternIdefsCollect
         _lhsOallAttributes :: AttrMap
         _lhsOallAttributes = rule268 _patternIallAttributes
         _lhsOdefsCollect :: Set (Identifier, Identifier)
         _lhsOdefsCollect = rule269 _patternIdefsCollect
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule270 _patternIerrors _rhsIerrors
         _output = rule271 _patternIoutput _rhsIoutput arg_eager_ arg_explicit_ arg_identity_ arg_mbError_ arg_mbName_ arg_origin_ arg_owrt_ arg_pure_
         _lhsOoutput :: Rule
         _lhsOoutput = rule272 _output
         _patternOchildInhs = rule273 _lhsIchildInhs
         _patternOchildSyns = rule274 _lhsIchildSyns
         _patternOcon = rule275 _lhsIcon
         _patternOdefs = rule276 _lhsIdefs
         _patternOforcedIrrefutables = rule277 _lhsIforcedIrrefutables
         _patternOnt = rule278 _lhsInt
         _rhsOchildInhs = rule279 _lhsIchildInhs
         _rhsOchildSyns = rule280 _lhsIchildSyns
         _rhsOcon = rule281 _lhsIcon
         _rhsOnt = rule282 _lhsInt
         _rhsOoptions = rule283 _lhsIoptions
         _rhsOruleDescr = rule284 _ruleDescr
         !__result_ = T_Rule_vOut40 _lhsOallAttributes _lhsOdefsCollect _lhsOerrors _lhsOoutput
         in __result_ )
     in C_Rule_s41 v40
   {-# INLINE rule267 #-}
   {-# LINE 172 "./src-ag/Desugar.ag" #-}
   rule267 = \ ((_lhsIcon) :: ConstructorIdent) ((_lhsInt) :: NontermIdent) ((_patternIdefsCollect) :: Set (Identifier, Identifier)) ->
                        {-# LINE 172 "./src-ag/Desugar.ag" #-}
                        show _lhsInt ++ " :: " ++ show _lhsIcon ++ " :: " ++ (concat $ intersperse "," $ map (\(f,a) -> show f ++ "." ++ show a) $ Set.toList _patternIdefsCollect)
                        {-# LINE 2265 "dist/build/Desugar.hs"#-}
   {-# INLINE rule268 #-}
   rule268 = \ ((_patternIallAttributes) :: AttrMap) ->
     _patternIallAttributes
   {-# INLINE rule269 #-}
   rule269 = \ ((_patternIdefsCollect) :: Set (Identifier, Identifier)) ->
     _patternIdefsCollect
   {-# INLINE rule270 #-}
   rule270 = \ ((_patternIerrors) :: Seq Error) ((_rhsIerrors) :: Seq Error) ->
     _patternIerrors Seq.>< _rhsIerrors
   {-# INLINE rule271 #-}
   rule271 = \ ((_patternIoutput) :: Pattern) ((_rhsIoutput) :: Expression) eager_ explicit_ identity_ mbError_ mbName_ origin_ owrt_ pure_ ->
     Rule mbName_ _patternIoutput _rhsIoutput owrt_ origin_ explicit_ pure_ identity_ mbError_ eager_
   {-# INLINE rule272 #-}
   rule272 = \ _output ->
     _output
   {-# INLINE rule273 #-}
   rule273 = \ ((_lhsIchildInhs) :: [(Identifier, Identifier)]) ->
     _lhsIchildInhs
   {-# INLINE rule274 #-}
   rule274 = \ ((_lhsIchildSyns) :: [(Identifier, Identifier)]) ->
     _lhsIchildSyns
   {-# INLINE rule275 #-}
   rule275 = \ ((_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# INLINE rule276 #-}
   rule276 = \ ((_lhsIdefs) :: Set (Identifier, Identifier)) ->
     _lhsIdefs
   {-# INLINE rule277 #-}
   rule277 = \ ((_lhsIforcedIrrefutables) :: AttrMap) ->
     _lhsIforcedIrrefutables
   {-# INLINE rule278 #-}
   rule278 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule279 #-}
   rule279 = \ ((_lhsIchildInhs) :: [(Identifier, Identifier)]) ->
     _lhsIchildInhs
   {-# INLINE rule280 #-}
   rule280 = \ ((_lhsIchildSyns) :: [(Identifier, Identifier)]) ->
     _lhsIchildSyns
   {-# INLINE rule281 #-}
   rule281 = \ ((_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# INLINE rule282 #-}
   rule282 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule283 #-}
   rule283 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule284 #-}
   rule284 = \ _ruleDescr ->
     _ruleDescr

-- Rules -------------------------------------------------------
-- wrapper
data Inh_Rules  = Inh_Rules { childInhs_Inh_Rules :: !([(Identifier, Identifier)]), childSyns_Inh_Rules :: !([(Identifier, Identifier)]), con_Inh_Rules :: !(ConstructorIdent), defs_Inh_Rules :: !(Set (Identifier, Identifier)), forcedIrrefutables_Inh_Rules :: !(AttrMap), nt_Inh_Rules :: !(NontermIdent), options_Inh_Rules :: !(Options) }
data Syn_Rules  = Syn_Rules { allAttributes_Syn_Rules :: !(AttrMap), defsCollect_Syn_Rules :: !(Set (Identifier, Identifier)), errors_Syn_Rules :: !(Seq Error), output_Syn_Rules :: !(Rules) }
{-# INLINABLE wrap_Rules #-}
wrap_Rules :: T_Rules  -> Inh_Rules  -> (Syn_Rules )
wrap_Rules !(T_Rules act) !(Inh_Rules _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt _lhsIoptions) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_Rules_vIn43 _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt _lhsIoptions
        !(T_Rules_vOut43 _lhsOallAttributes _lhsOdefsCollect _lhsOerrors _lhsOoutput) <- return (inv_Rules_s44 sem arg)
        return (Syn_Rules _lhsOallAttributes _lhsOdefsCollect _lhsOerrors _lhsOoutput)
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
data T_Rules_vIn43  = T_Rules_vIn43 ([(Identifier, Identifier)]) ([(Identifier, Identifier)]) (ConstructorIdent) (Set (Identifier, Identifier)) (AttrMap) (NontermIdent) (Options)
data T_Rules_vOut43  = T_Rules_vOut43 (AttrMap) (Set (Identifier, Identifier)) (Seq Error) (Rules)
{-# NOINLINE sem_Rules_Cons #-}
sem_Rules_Cons :: T_Rule  -> T_Rules  -> T_Rules 
sem_Rules_Cons arg_hd_ arg_tl_ = T_Rules (return st44) where
   {-# NOINLINE st44 #-}
   !st44 = let
      v43 :: T_Rules_v43 
      v43 = \ !(T_Rules_vIn43 _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt _lhsIoptions) -> ( let
         _hdX41 = Control.Monad.Identity.runIdentity (attach_T_Rule (arg_hd_))
         _tlX44 = Control.Monad.Identity.runIdentity (attach_T_Rules (arg_tl_))
         (T_Rule_vOut40 _hdIallAttributes _hdIdefsCollect _hdIerrors _hdIoutput) = inv_Rule_s41 _hdX41 (T_Rule_vIn40 _hdOchildInhs _hdOchildSyns _hdOcon _hdOdefs _hdOforcedIrrefutables _hdOnt _hdOoptions)
         (T_Rules_vOut43 _tlIallAttributes _tlIdefsCollect _tlIerrors _tlIoutput) = inv_Rules_s44 _tlX44 (T_Rules_vIn43 _tlOchildInhs _tlOchildSyns _tlOcon _tlOdefs _tlOforcedIrrefutables _tlOnt _tlOoptions)
         _lhsOallAttributes :: AttrMap
         _lhsOallAttributes = rule285 _hdIallAttributes _tlIallAttributes
         _lhsOdefsCollect :: Set (Identifier, Identifier)
         _lhsOdefsCollect = rule286 _hdIdefsCollect _tlIdefsCollect
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule287 _hdIerrors _tlIerrors
         _output = rule288 _hdIoutput _tlIoutput
         _lhsOoutput :: Rules
         _lhsOoutput = rule289 _output
         _hdOchildInhs = rule290 _lhsIchildInhs
         _hdOchildSyns = rule291 _lhsIchildSyns
         _hdOcon = rule292 _lhsIcon
         _hdOdefs = rule293 _lhsIdefs
         _hdOforcedIrrefutables = rule294 _lhsIforcedIrrefutables
         _hdOnt = rule295 _lhsInt
         _hdOoptions = rule296 _lhsIoptions
         _tlOchildInhs = rule297 _lhsIchildInhs
         _tlOchildSyns = rule298 _lhsIchildSyns
         _tlOcon = rule299 _lhsIcon
         _tlOdefs = rule300 _lhsIdefs
         _tlOforcedIrrefutables = rule301 _lhsIforcedIrrefutables
         _tlOnt = rule302 _lhsInt
         _tlOoptions = rule303 _lhsIoptions
         !__result_ = T_Rules_vOut43 _lhsOallAttributes _lhsOdefsCollect _lhsOerrors _lhsOoutput
         in __result_ )
     in C_Rules_s44 v43
   {-# INLINE rule285 #-}
   rule285 = \ ((_hdIallAttributes) :: AttrMap) ((_tlIallAttributes) :: AttrMap) ->
     _hdIallAttributes `mergeAttributes` _tlIallAttributes
   {-# INLINE rule286 #-}
   rule286 = \ ((_hdIdefsCollect) :: Set (Identifier, Identifier)) ((_tlIdefsCollect) :: Set (Identifier, Identifier)) ->
     _hdIdefsCollect `Set.union` _tlIdefsCollect
   {-# INLINE rule287 #-}
   rule287 = \ ((_hdIerrors) :: Seq Error) ((_tlIerrors) :: Seq Error) ->
     _hdIerrors Seq.>< _tlIerrors
   {-# INLINE rule288 #-}
   rule288 = \ ((_hdIoutput) :: Rule) ((_tlIoutput) :: Rules) ->
     (:) _hdIoutput _tlIoutput
   {-# INLINE rule289 #-}
   rule289 = \ _output ->
     _output
   {-# INLINE rule290 #-}
   rule290 = \ ((_lhsIchildInhs) :: [(Identifier, Identifier)]) ->
     _lhsIchildInhs
   {-# INLINE rule291 #-}
   rule291 = \ ((_lhsIchildSyns) :: [(Identifier, Identifier)]) ->
     _lhsIchildSyns
   {-# INLINE rule292 #-}
   rule292 = \ ((_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# INLINE rule293 #-}
   rule293 = \ ((_lhsIdefs) :: Set (Identifier, Identifier)) ->
     _lhsIdefs
   {-# INLINE rule294 #-}
   rule294 = \ ((_lhsIforcedIrrefutables) :: AttrMap) ->
     _lhsIforcedIrrefutables
   {-# INLINE rule295 #-}
   rule295 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule296 #-}
   rule296 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule297 #-}
   rule297 = \ ((_lhsIchildInhs) :: [(Identifier, Identifier)]) ->
     _lhsIchildInhs
   {-# INLINE rule298 #-}
   rule298 = \ ((_lhsIchildSyns) :: [(Identifier, Identifier)]) ->
     _lhsIchildSyns
   {-# INLINE rule299 #-}
   rule299 = \ ((_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# INLINE rule300 #-}
   rule300 = \ ((_lhsIdefs) :: Set (Identifier, Identifier)) ->
     _lhsIdefs
   {-# INLINE rule301 #-}
   rule301 = \ ((_lhsIforcedIrrefutables) :: AttrMap) ->
     _lhsIforcedIrrefutables
   {-# INLINE rule302 #-}
   rule302 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule303 #-}
   rule303 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Rules_Nil #-}
sem_Rules_Nil ::  T_Rules 
sem_Rules_Nil  = T_Rules (return st44) where
   {-# NOINLINE st44 #-}
   !st44 = let
      v43 :: T_Rules_v43 
      v43 = \ !(T_Rules_vIn43 _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt _lhsIoptions) -> ( let
         _lhsOallAttributes :: AttrMap
         _lhsOallAttributes = rule304  ()
         _lhsOdefsCollect :: Set (Identifier, Identifier)
         _lhsOdefsCollect = rule305  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule306  ()
         _output = rule307  ()
         _lhsOoutput :: Rules
         _lhsOoutput = rule308 _output
         !__result_ = T_Rules_vOut43 _lhsOallAttributes _lhsOdefsCollect _lhsOerrors _lhsOoutput
         in __result_ )
     in C_Rules_s44 v43
   {-# INLINE rule304 #-}
   rule304 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule305 #-}
   rule305 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule306 #-}
   rule306 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule307 #-}
   rule307 = \  (_ :: ()) ->
     []
   {-# INLINE rule308 #-}
   rule308 = \ _output ->
     _output

-- TypeSig -----------------------------------------------------
-- wrapper
data Inh_TypeSig  = Inh_TypeSig {  }
data Syn_TypeSig  = Syn_TypeSig { output_Syn_TypeSig :: !(TypeSig) }
{-# INLINABLE wrap_TypeSig #-}
wrap_TypeSig :: T_TypeSig  -> Inh_TypeSig  -> (Syn_TypeSig )
wrap_TypeSig !(T_TypeSig act) !(Inh_TypeSig ) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_TypeSig_vIn46 
        !(T_TypeSig_vOut46 _lhsOoutput) <- return (inv_TypeSig_s47 sem arg)
        return (Syn_TypeSig _lhsOoutput)
   )

-- cata
{-# INLINE sem_TypeSig #-}
sem_TypeSig :: TypeSig  -> T_TypeSig 
sem_TypeSig ( TypeSig !name_ !tp_ ) = sem_TypeSig_TypeSig name_ tp_

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
data T_TypeSig_vOut46  = T_TypeSig_vOut46 (TypeSig)
{-# NOINLINE sem_TypeSig_TypeSig #-}
sem_TypeSig_TypeSig :: (Identifier) -> (Type) -> T_TypeSig 
sem_TypeSig_TypeSig !arg_name_ !arg_tp_ = T_TypeSig (return st47) where
   {-# NOINLINE st47 #-}
   !st47 = let
      v46 :: T_TypeSig_v46 
      v46 = \ !(T_TypeSig_vIn46 ) -> ( let
         _output = rule309 arg_name_ arg_tp_
         _lhsOoutput :: TypeSig
         _lhsOoutput = rule310 _output
         !__result_ = T_TypeSig_vOut46 _lhsOoutput
         in __result_ )
     in C_TypeSig_s47 v46
   {-# INLINE rule309 #-}
   rule309 = \ name_ tp_ ->
     TypeSig name_ tp_
   {-# INLINE rule310 #-}
   rule310 = \ _output ->
     _output

-- TypeSigs ----------------------------------------------------
-- wrapper
data Inh_TypeSigs  = Inh_TypeSigs {  }
data Syn_TypeSigs  = Syn_TypeSigs { output_Syn_TypeSigs :: !(TypeSigs) }
{-# INLINABLE wrap_TypeSigs #-}
wrap_TypeSigs :: T_TypeSigs  -> Inh_TypeSigs  -> (Syn_TypeSigs )
wrap_TypeSigs !(T_TypeSigs act) !(Inh_TypeSigs ) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_TypeSigs_vIn49 
        !(T_TypeSigs_vOut49 _lhsOoutput) <- return (inv_TypeSigs_s50 sem arg)
        return (Syn_TypeSigs _lhsOoutput)
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
data T_TypeSigs_vOut49  = T_TypeSigs_vOut49 (TypeSigs)
{-# NOINLINE sem_TypeSigs_Cons #-}
sem_TypeSigs_Cons :: T_TypeSig  -> T_TypeSigs  -> T_TypeSigs 
sem_TypeSigs_Cons arg_hd_ arg_tl_ = T_TypeSigs (return st50) where
   {-# NOINLINE st50 #-}
   !st50 = let
      v49 :: T_TypeSigs_v49 
      v49 = \ !(T_TypeSigs_vIn49 ) -> ( let
         _hdX47 = Control.Monad.Identity.runIdentity (attach_T_TypeSig (arg_hd_))
         _tlX50 = Control.Monad.Identity.runIdentity (attach_T_TypeSigs (arg_tl_))
         (T_TypeSig_vOut46 _hdIoutput) = inv_TypeSig_s47 _hdX47 (T_TypeSig_vIn46 )
         (T_TypeSigs_vOut49 _tlIoutput) = inv_TypeSigs_s50 _tlX50 (T_TypeSigs_vIn49 )
         _output = rule311 _hdIoutput _tlIoutput
         _lhsOoutput :: TypeSigs
         _lhsOoutput = rule312 _output
         !__result_ = T_TypeSigs_vOut49 _lhsOoutput
         in __result_ )
     in C_TypeSigs_s50 v49
   {-# INLINE rule311 #-}
   rule311 = \ ((_hdIoutput) :: TypeSig) ((_tlIoutput) :: TypeSigs) ->
     (:) _hdIoutput _tlIoutput
   {-# INLINE rule312 #-}
   rule312 = \ _output ->
     _output
{-# NOINLINE sem_TypeSigs_Nil #-}
sem_TypeSigs_Nil ::  T_TypeSigs 
sem_TypeSigs_Nil  = T_TypeSigs (return st50) where
   {-# NOINLINE st50 #-}
   !st50 = let
      v49 :: T_TypeSigs_v49 
      v49 = \ !(T_TypeSigs_vIn49 ) -> ( let
         _output = rule313  ()
         _lhsOoutput :: TypeSigs
         _lhsOoutput = rule314 _output
         !__result_ = T_TypeSigs_vOut49 _lhsOoutput
         in __result_ )
     in C_TypeSigs_s50 v49
   {-# INLINE rule313 #-}
   rule313 = \  (_ :: ()) ->
     []
   {-# INLINE rule314 #-}
   rule314 = \ _output ->
     _output
