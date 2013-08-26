{-# LANGUAGE Rank2Types, GADTs #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Desugar where
{-# LINE 2 "./src-ag/HsToken.ag" #-}

import CommonTypes
import UU.Scanner.Position(Pos)
{-# LINE 11 "dist/build/Desugar.hs" #-}

{-# LINE 2 "./src-ag/Expression.ag" #-}

import UU.Scanner.Position(Pos)
import HsToken
{-# LINE 17 "dist/build/Desugar.hs" #-}

{-# LINE 2 "./src-ag/Patterns.ag" #-}

-- Patterns.ag imports
import UU.Scanner.Position(Pos)
import CommonTypes (ConstructorIdent,Identifier)
{-# LINE 24 "dist/build/Desugar.hs" #-}

{-# LINE 2 "./src-ag/AbstractSyntax.ag" #-}

-- AbstractSyntax.ag imports
import Data.Set(Set)
import Data.Map(Map)
import Patterns    (Pattern(..),Patterns)
import Expression  (Expression(..))
import Macro --marcos
import CommonTypes
import ErrorMessages
{-# LINE 36 "dist/build/Desugar.hs" #-}

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
        let arg = T_Child_vIn0 _lhsIinhMap _lhsImainName _lhsIoptions _lhsIsynMap
        !(T_Child_vOut0 _lhsOchildInhs _lhsOchildSyns _lhsOoutput) <- return (inv_Child_s0 sem K_Child_v0 arg)
        return (Syn_Child _lhsOchildInhs _lhsOchildSyns _lhsOoutput)
   )

-- cata
{-# INLINE sem_Child #-}
sem_Child :: Child  -> T_Child 
sem_Child ( Child !name_ !tp_ !kind_ ) = sem_Child_Child name_ tp_ kind_

-- semantic domain
newtype T_Child  = T_Child {
                           attach_T_Child :: Identity (T_Child_s0 )
                           }
data T_Child_s0  where C_Child_s0 :: {
                                     inv_Child_s0 :: !(forall t. K_Child_s0  t -> t)
                                     } -> T_Child_s0 
data T_Child_s1  = C_Child_s1
data T_Child_s34  = C_Child_s34
data K_Child_s0 k  where
   K_Child_v0 :: K_Child_s0  (T_Child_v0 )
   K_Child_v17 :: K_Child_s0  (T_Child_v17 )
type T_Child_v0  = (T_Child_vIn0 ) -> (T_Child_vOut0 )
data T_Child_vIn0  = T_Child_vIn0 !(Map Identifier Attributes) !(String) !(Options) !(Map Identifier Attributes)
data T_Child_vOut0  = T_Child_vOut0 !([(Identifier, Identifier)]) !([(Identifier, Identifier)]) !(Child)
type T_Child_v17  = (T_Child_vIn17 ) -> (T_Child_vOut17 )
data T_Child_vIn17  = T_Child_vIn17 !(Map Identifier Attributes) !(Map Identifier Attributes)
data T_Child_vOut17  = T_Child_vOut17 !([(Identifier, Identifier)]) !([(Identifier, Identifier)]) !(Child)
{-# NOINLINE sem_Child_Child #-}
sem_Child_Child :: (Identifier) -> (Type) -> (ChildKind) -> T_Child 
sem_Child_Child !arg_name_ !arg_tp_ !arg_kind_ = T_Child (return st0) where
   {-# NOINLINE st0 #-}
   !st0 = let
      k0 :: K_Child_s0  t -> t
      k0 K_Child_v0 = v0
      k0 K_Child_v17 = v17
      v0 :: T_Child_v0 
      v0 = \ !(T_Child_vIn0 _lhsIinhMap _lhsImainName _lhsIoptions _lhsIsynMap) -> (
         let !_chnt = rule0 arg_name_ arg_tp_ in
         let !_inh = rule1 _chnt _lhsIinhMap in
         let _lhsOchildInhs :: [(Identifier, Identifier)]
             !_lhsOchildInhs = rule3 _inh arg_name_ in
         let !_syn = rule2 _chnt _lhsIsynMap in
         let _lhsOchildSyns :: [(Identifier, Identifier)]
             !_lhsOchildSyns = rule4 _syn arg_name_ in
         let _lhsOoutput :: Child
             !_lhsOoutput = rule5 arg_kind_ arg_name_ arg_tp_ in
         let !__result_ = T_Child_vOut0 _lhsOchildInhs _lhsOchildSyns _lhsOoutput
          in __result_ )
      v17 :: T_Child_v17 
      v17 = \ !(T_Child_vIn17 _lhsIinhMap _lhsIsynMap) -> (
         let !_chnt = rule0 arg_name_ arg_tp_ in
         let !_inh = rule1 _chnt _lhsIinhMap in
         let _lhsOchildInhs :: [(Identifier, Identifier)]
             !_lhsOchildInhs = rule3 _inh arg_name_ in
         let !_syn = rule2 _chnt _lhsIsynMap in
         let _lhsOchildSyns :: [(Identifier, Identifier)]
             !_lhsOchildSyns = rule4 _syn arg_name_ in
         let _lhsOoutput :: Child
             !_lhsOoutput = rule5 arg_kind_ arg_name_ arg_tp_ in
         let !__result_ = T_Child_vOut17 _lhsOchildInhs _lhsOchildSyns _lhsOoutput
          in __result_ )
     in C_Child_s0 k0
   {-# NOINLINE[1] rule0 #-}
   {-# LINE 19 "./src-ag/DistChildAttr.ag" #-}
   rule0 = \ !name_ !tp_ ->
                       {-# LINE 19 "./src-ag/DistChildAttr.ag" #-}
                       case tp_ of
                         NT nt _ _ -> nt
                         Self      -> error ("The type of child " ++ show name_ ++ " should not be a Self type.")
                         Haskell t -> identifier ""
                       {-# LINE 201 "dist/build/Desugar.hs"#-}
   {-# NOINLINE[1] rule1 #-}
   {-# LINE 23 "./src-ag/DistChildAttr.ag" #-}
   rule1 = \ !_chnt ((!_lhsIinhMap) :: Map Identifier Attributes) ->
                      {-# LINE 23 "./src-ag/DistChildAttr.ag" #-}
                      Map.findWithDefault Map.empty _chnt     _lhsIinhMap
                      {-# LINE 207 "dist/build/Desugar.hs"#-}
   {-# NOINLINE[1] rule2 #-}
   {-# LINE 24 "./src-ag/DistChildAttr.ag" #-}
   rule2 = \ !_chnt ((!_lhsIsynMap) :: Map Identifier Attributes) ->
                      {-# LINE 24 "./src-ag/DistChildAttr.ag" #-}
                      Map.findWithDefault Map.empty _chnt     _lhsIsynMap
                      {-# LINE 213 "dist/build/Desugar.hs"#-}
   {-# NOINLINE[1] rule3 #-}
   {-# LINE 130 "./src-ag/Desugar.ag" #-}
   rule3 = \ !_inh !name_ ->
                        {-# LINE 130 "./src-ag/Desugar.ag" #-}
                        [(i, name_) | i <- Map.keys _inh     ]
                        {-# LINE 219 "dist/build/Desugar.hs"#-}
   {-# NOINLINE[1] rule4 #-}
   {-# LINE 131 "./src-ag/Desugar.ag" #-}
   rule4 = \ !_syn !name_ ->
                        {-# LINE 131 "./src-ag/Desugar.ag" #-}
                        [(s, name_) | s <- Map.keys _syn     ]
                        {-# LINE 225 "dist/build/Desugar.hs"#-}
   {-# NOINLINE[1] rule5 #-}
   {-# LINE 315 "./src-ag/Desugar.ag" #-}
   rule5 = \ !kind_ !name_ !tp_ ->
                 {-# LINE 315 "./src-ag/Desugar.ag" #-}
                 Child name_ tp_ kind_
                 {-# LINE 231 "dist/build/Desugar.hs"#-}

-- Children ----------------------------------------------------
-- wrapper
data Inh_Children  = Inh_Children { inhMap_Inh_Children :: !(Map Identifier Attributes), mainName_Inh_Children :: !(String), options_Inh_Children :: !(Options), synMap_Inh_Children :: !(Map Identifier Attributes) }
data Syn_Children  = Syn_Children { childInhs_Syn_Children :: !([(Identifier, Identifier)]), childSyns_Syn_Children :: !([(Identifier, Identifier)]), output_Syn_Children :: !(Children) }
{-# INLINABLE wrap_Children #-}
wrap_Children :: T_Children  -> Inh_Children  -> (Syn_Children )
wrap_Children !(T_Children act) !(Inh_Children _lhsIinhMap _lhsImainName _lhsIoptions _lhsIsynMap) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_Children_vIn1 _lhsIinhMap _lhsImainName _lhsIoptions _lhsIsynMap
        !(T_Children_vOut1 _lhsOchildInhs _lhsOchildSyns _lhsOoutput) <- return (inv_Children_s2 sem K_Children_v1 arg)
        return (Syn_Children _lhsOchildInhs _lhsOchildSyns _lhsOoutput)
   )

-- cata
{-# NOINLINE sem_Children #-}
sem_Children :: Children  -> T_Children 
sem_Children list = Prelude.foldr sem_Children_Cons sem_Children_Nil (Prelude.map sem_Child list)

-- semantic domain
newtype T_Children  = T_Children {
                                 attach_T_Children :: Identity (T_Children_s2 )
                                 }
data T_Children_s2  where C_Children_s2 :: {
                                           inv_Children_s2 :: !(forall t. K_Children_s2  t -> t)
                                           } -> T_Children_s2 
data T_Children_s3  = C_Children_s3
data T_Children_s35  = C_Children_s35
data K_Children_s2 k  where
   K_Children_v1 :: K_Children_s2  (T_Children_v1 )
   K_Children_v18 :: K_Children_s2  (T_Children_v18 )
type T_Children_v1  = (T_Children_vIn1 ) -> (T_Children_vOut1 )
data T_Children_vIn1  = T_Children_vIn1 !(Map Identifier Attributes) !(String) !(Options) !(Map Identifier Attributes)
data T_Children_vOut1  = T_Children_vOut1 !([(Identifier, Identifier)]) !([(Identifier, Identifier)]) !(Children)
type T_Children_v18  = (T_Children_vIn18 ) -> (T_Children_vOut18 )
data T_Children_vIn18  = T_Children_vIn18 !(Map Identifier Attributes) !(Map Identifier Attributes)
data T_Children_vOut18  = T_Children_vOut18 !([(Identifier, Identifier)]) !([(Identifier, Identifier)]) !(Children)
{-# NOINLINE sem_Children_Cons #-}
sem_Children_Cons :: T_Child  -> T_Children  -> T_Children 
sem_Children_Cons arg_hd_ arg_tl_ = T_Children (return st2) where
   {-# NOINLINE st2 #-}
   !st2 = let
      k2 :: K_Children_s2  t -> t
      k2 K_Children_v1 = v1
      k2 K_Children_v18 = v18
      v1 :: T_Children_v1 
      v1 = \ !(T_Children_vIn1 _lhsIinhMap _lhsImainName _lhsIoptions _lhsIsynMap) -> (
         let !_hdX0 = Control.Monad.Identity.runIdentity (attach_T_Child (arg_hd_)) in
         let !_tlX2 = Control.Monad.Identity.runIdentity (attach_T_Children (arg_tl_)) in
         let !_hdOinhMap = rule11 _lhsIinhMap in
         let !_tlOinhMap = rule15 _lhsIinhMap in
         let !_hdOsynMap = rule14 _lhsIsynMap in
         let !_tlOsynMap = rule18 _lhsIsynMap in
         let !(T_Child_vOut17 _hdIchildInhs _hdIchildSyns _hdIoutput) = inv_Child_s0 _hdX0 K_Child_v17 (T_Child_vIn17 _hdOinhMap _hdOsynMap) in
         let !(T_Children_vOut18 _tlIchildInhs _tlIchildSyns _tlIoutput) = inv_Children_s2 _tlX2 K_Children_v18 (T_Children_vIn18 _tlOinhMap _tlOsynMap) in
         let _lhsOchildInhs :: [(Identifier, Identifier)]
             !_lhsOchildInhs = rule7 _hdIchildInhs _tlIchildInhs in
         let _lhsOchildSyns :: [(Identifier, Identifier)]
             !_lhsOchildSyns = rule8 _hdIchildSyns _tlIchildSyns in
         let !_output = rule9 _hdIoutput _tlIoutput in
         let _lhsOoutput :: Children
             !_lhsOoutput = rule10 _output in
         let !__result_ = T_Children_vOut1 _lhsOchildInhs _lhsOchildSyns _lhsOoutput
          in __result_ )
      v18 :: T_Children_v18 
      v18 = \ !(T_Children_vIn18 _lhsIinhMap _lhsIsynMap) -> (
         let !_hdX0 = Control.Monad.Identity.runIdentity (attach_T_Child (arg_hd_)) in
         let !_tlX2 = Control.Monad.Identity.runIdentity (attach_T_Children (arg_tl_)) in
         let !_hdOinhMap = rule11 _lhsIinhMap in
         let !_tlOinhMap = rule15 _lhsIinhMap in
         let !_hdOsynMap = rule14 _lhsIsynMap in
         let !_tlOsynMap = rule18 _lhsIsynMap in
         let !(T_Child_vOut17 _hdIchildInhs _hdIchildSyns _hdIoutput) = inv_Child_s0 _hdX0 K_Child_v17 (T_Child_vIn17 _hdOinhMap _hdOsynMap) in
         let !(T_Children_vOut18 _tlIchildInhs _tlIchildSyns _tlIoutput) = inv_Children_s2 _tlX2 K_Children_v18 (T_Children_vIn18 _tlOinhMap _tlOsynMap) in
         let _lhsOchildInhs :: [(Identifier, Identifier)]
             !_lhsOchildInhs = rule7 _hdIchildInhs _tlIchildInhs in
         let _lhsOchildSyns :: [(Identifier, Identifier)]
             !_lhsOchildSyns = rule8 _hdIchildSyns _tlIchildSyns in
         let !_output = rule9 _hdIoutput _tlIoutput in
         let _lhsOoutput :: Children
             !_lhsOoutput = rule10 _output in
         let !__result_ = T_Children_vOut18 _lhsOchildInhs _lhsOchildSyns _lhsOoutput
          in __result_ )
     in C_Children_s2 k2
   {-# NOINLINE[1] rule7 #-}
   rule7 = \ ((!_hdIchildInhs) :: [(Identifier, Identifier)]) ((!_tlIchildInhs) :: [(Identifier, Identifier)]) ->
     _hdIchildInhs ++ _tlIchildInhs
   {-# NOINLINE[1] rule8 #-}
   rule8 = \ ((!_hdIchildSyns) :: [(Identifier, Identifier)]) ((!_tlIchildSyns) :: [(Identifier, Identifier)]) ->
     _hdIchildSyns ++ _tlIchildSyns
   {-# NOINLINE[1] rule9 #-}
   rule9 = \ ((!_hdIoutput) :: Child) ((!_tlIoutput) :: Children) ->
     (:) _hdIoutput _tlIoutput
   {-# NOINLINE[1] rule10 #-}
   rule10 = \ !_output ->
     _output
   {-# NOINLINE[1] rule11 #-}
   rule11 = \ ((!_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# NOINLINE[1] rule14 #-}
   rule14 = \ ((!_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
   {-# NOINLINE[1] rule15 #-}
   rule15 = \ ((!_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# NOINLINE[1] rule18 #-}
   rule18 = \ ((!_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
{-# NOINLINE sem_Children_Nil #-}
sem_Children_Nil ::  T_Children 
sem_Children_Nil  = T_Children (return st2) where
   {-# NOINLINE st2 #-}
   !st2 = let
      k2 :: K_Children_s2  t -> t
      k2 K_Children_v1 = v1
      k2 K_Children_v18 = v18
      v1 :: T_Children_v1 
      v1 = \ !(T_Children_vIn1 _lhsIinhMap _lhsImainName _lhsIoptions _lhsIsynMap) -> (
         let _lhsOchildInhs :: [(Identifier, Identifier)]
             !_lhsOchildInhs = rule19  () in
         let _lhsOchildSyns :: [(Identifier, Identifier)]
             !_lhsOchildSyns = rule20  () in
         let !_output = rule21  () in
         let _lhsOoutput :: Children
             !_lhsOoutput = rule22 _output in
         let !__result_ = T_Children_vOut1 _lhsOchildInhs _lhsOchildSyns _lhsOoutput
          in __result_ )
      v18 :: T_Children_v18 
      v18 = \ !(T_Children_vIn18 _lhsIinhMap _lhsIsynMap) -> (
         let _lhsOchildInhs :: [(Identifier, Identifier)]
             !_lhsOchildInhs = rule19  () in
         let _lhsOchildSyns :: [(Identifier, Identifier)]
             !_lhsOchildSyns = rule20  () in
         let !_output = rule21  () in
         let _lhsOoutput :: Children
             !_lhsOoutput = rule22 _output in
         let !__result_ = T_Children_vOut18 _lhsOchildInhs _lhsOchildSyns _lhsOoutput
          in __result_ )
     in C_Children_s2 k2
   {-# NOINLINE[1] rule19 #-}
   rule19 = \  (_ :: ()) ->
     []
   {-# NOINLINE[1] rule20 #-}
   rule20 = \  (_ :: ()) ->
     []
   {-# NOINLINE[1] rule21 #-}
   rule21 = \  (_ :: ()) ->
     []
   {-# NOINLINE[1] rule22 #-}
   rule22 = \ !_output ->
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
        let arg = T_Expression_vIn2 _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsInt _lhsIoptions _lhsIruleDescr
        !(T_Expression_vOut2 _lhsOerrors _lhsOoutput) <- return (inv_Expression_s4 sem arg)
        return (Syn_Expression _lhsOerrors _lhsOoutput)
   )

-- cata
{-# INLINE sem_Expression #-}
sem_Expression :: Expression  -> T_Expression 
sem_Expression ( Expression !pos_ !tks_ ) = sem_Expression_Expression pos_ tks_

-- semantic domain
newtype T_Expression  = T_Expression {
                                     attach_T_Expression :: Identity (T_Expression_s4 )
                                     }
newtype T_Expression_s4  = C_Expression_s4 {
                                           inv_Expression_s4 :: (T_Expression_v2 )
                                           }
data T_Expression_s5  = C_Expression_s5
type T_Expression_v2  = (T_Expression_vIn2 ) -> (T_Expression_vOut2 )
data T_Expression_vIn2  = T_Expression_vIn2 !([(Identifier, Identifier)]) !([(Identifier, Identifier)]) !(ConstructorIdent) !(NontermIdent) !(Options) !(String)
data T_Expression_vOut2  = T_Expression_vOut2 !(Seq Error) !(Expression)
{-# NOINLINE sem_Expression_Expression #-}
sem_Expression_Expression :: (Pos) -> ([HsToken]) -> T_Expression 
sem_Expression_Expression !arg_pos_ !arg_tks_ = T_Expression (return st4) where
   {-# NOINLINE st4 #-}
   !st4 = let
      v2 :: T_Expression_v2 
      v2 = \ !(T_Expression_vIn2 _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsInt _lhsIoptions _lhsIruleDescr) -> (
         let _lhsOerrors :: Seq Error
             !(!_tks',!_lhsOerrors) = rule23 _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsInt _lhsIoptions _lhsIruleDescr arg_tks_ in
         let _lhsOoutput :: Expression
             !_lhsOoutput = rule24 _tks' arg_pos_ in
         let !__result_ = T_Expression_vOut2 _lhsOerrors _lhsOoutput
          in __result_ )
     in C_Expression_s4 v2
   {-# INLINE rule23 #-}
   {-# LINE 49 "./src-ag/Desugar.ag" #-}
   rule23 = \ ((!_lhsIchildInhs) :: [(Identifier, Identifier)]) ((!_lhsIchildSyns) :: [(Identifier, Identifier)]) ((!_lhsIcon) :: ConstructorIdent) ((!_lhsInt) :: NontermIdent) ((!_lhsIoptions) :: Options) ((!_lhsIruleDescr) :: String) !tks_ ->
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
                                 {-# LINE 443 "dist/build/Desugar.hs"#-}
   {-# INLINE rule24 #-}
   {-# LINE 59 "./src-ag/Desugar.ag" #-}
   rule24 = \ !_tks' !pos_ ->
                     {-# LINE 59 "./src-ag/Desugar.ag" #-}
                     Expression pos_ _tks'
                     {-# LINE 449 "dist/build/Desugar.hs"#-}

-- Grammar -----------------------------------------------------
-- wrapper
data Inh_Grammar  = Inh_Grammar { forcedIrrefutables_Inh_Grammar :: !(AttrMap), mainName_Inh_Grammar :: !(String), options_Inh_Grammar :: !(Options) }
data Syn_Grammar  = Syn_Grammar { allAttributes_Syn_Grammar :: !(AttrMap), errors_Syn_Grammar :: !(Seq Error), output_Syn_Grammar :: !(Grammar) }
{-# INLINABLE wrap_Grammar #-}
wrap_Grammar :: T_Grammar  -> Inh_Grammar  -> (Syn_Grammar )
wrap_Grammar !(T_Grammar act) !(Inh_Grammar _lhsIforcedIrrefutables _lhsImainName _lhsIoptions) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_Grammar_vIn3 _lhsIforcedIrrefutables _lhsImainName _lhsIoptions
        !(T_Grammar_vOut3 _lhsOallAttributes _lhsOerrors _lhsOoutput) <- return (inv_Grammar_s6 sem arg)
        return (Syn_Grammar _lhsOallAttributes _lhsOerrors _lhsOoutput)
   )

-- cata
{-# INLINE sem_Grammar #-}
sem_Grammar :: Grammar  -> T_Grammar 
sem_Grammar ( Grammar !typeSyns_ !useMap_ !derivings_ !wrappers_ nonts_ !pragmas_ !manualAttrOrderMap_ !paramMap_ !contextMap_ !quantMap_ !uniqueMap_ !augmentsMap_ !aroundsMap_ !mergeMap_ ) = sem_Grammar_Grammar typeSyns_ useMap_ derivings_ wrappers_ ( sem_Nonterminals nonts_ ) pragmas_ manualAttrOrderMap_ paramMap_ contextMap_ quantMap_ uniqueMap_ augmentsMap_ aroundsMap_ mergeMap_

-- semantic domain
newtype T_Grammar  = T_Grammar {
                               attach_T_Grammar :: Identity (T_Grammar_s6 )
                               }
newtype T_Grammar_s6  = C_Grammar_s6 {
                                     inv_Grammar_s6 :: (T_Grammar_v3 )
                                     }
data T_Grammar_s7  = C_Grammar_s7
type T_Grammar_v3  = (T_Grammar_vIn3 ) -> (T_Grammar_vOut3 )
data T_Grammar_vIn3  = T_Grammar_vIn3 !(AttrMap) !(String) !(Options)
data T_Grammar_vOut3  = T_Grammar_vOut3 !(AttrMap) !(Seq Error) !(Grammar)
{-# NOINLINE sem_Grammar_Grammar #-}
sem_Grammar_Grammar :: (TypeSyns) -> (UseMap) -> (Derivings) -> (Set NontermIdent) -> T_Nonterminals  -> (PragmaMap) -> (AttrOrderMap) -> (ParamMap) -> (ContextMap) -> (QuantMap) -> (UniqueMap) -> (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) -> (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) -> (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier, [Identifier], Expression)))) -> T_Grammar 
sem_Grammar_Grammar !arg_typeSyns_ !arg_useMap_ !arg_derivings_ !arg_wrappers_ arg_nonts_ !arg_pragmas_ !arg_manualAttrOrderMap_ !arg_paramMap_ !arg_contextMap_ !arg_quantMap_ !arg_uniqueMap_ !arg_augmentsMap_ !arg_aroundsMap_ !arg_mergeMap_ = T_Grammar (return st6) where
   {-# NOINLINE st6 #-}
   !st6 = let
      v3 :: T_Grammar_v3 
      v3 = \ !(T_Grammar_vIn3 _lhsIforcedIrrefutables _lhsImainName _lhsIoptions) -> (
         let !_nontsX16 = Control.Monad.Identity.runIdentity (attach_T_Nonterminals (arg_nonts_)) in
         let !_nontsOaugmentsIn = rule28 arg_augmentsMap_ in
         let !_nontsOoptions = rule35 _lhsIoptions in
         let !_nontsOforcedIrrefutables = rule33 _lhsIforcedIrrefutables in
         let !_nontsOmainName = rule34 _lhsImainName in
         let !(T_Nonterminals_vOut19 _nontsIallAttributes _nontsIinhMap' _nontsIsynMap' _nontsX36) = inv_Nonterminals_s16 _nontsX16 K_Nonterminals_v19 (T_Nonterminals_vIn19 ) in
         let _lhsOallAttributes :: AttrMap
             !_lhsOallAttributes = rule30 _nontsIallAttributes in
         let !_nontsOinhMap = rule26 _nontsIinhMap' in
         let !_nontsOsynMap = rule27 _nontsIsynMap' in
         let !(T_Nonterminals_vOut20 _nontsIaugmentsOut _nontsIerrors _nontsIoutput) = inv_Nonterminals_s36 _nontsX36 (T_Nonterminals_vIn20 _nontsOaugmentsIn _nontsOforcedIrrefutables _nontsOinhMap _nontsOmainName _nontsOoptions _nontsOsynMap) in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule31 _nontsIerrors in
         let _lhsOoutput :: Grammar
             !_lhsOoutput = rule29 _nontsIaugmentsOut _nontsIoutput arg_aroundsMap_ arg_contextMap_ arg_derivings_ arg_manualAttrOrderMap_ arg_mergeMap_ arg_paramMap_ arg_pragmas_ arg_quantMap_ arg_typeSyns_ arg_uniqueMap_ arg_useMap_ arg_wrappers_ in
         let !__result_ = T_Grammar_vOut3 _lhsOallAttributes _lhsOerrors _lhsOoutput
          in __result_ )
     in C_Grammar_s6 v3
   {-# INLINE rule26 #-}
   {-# LINE 15 "./src-ag/DistChildAttr.ag" #-}
   rule26 = \ ((!_nontsIinhMap') :: Map Identifier Attributes) ->
                             {-# LINE 15 "./src-ag/DistChildAttr.ag" #-}
                             _nontsIinhMap'
                             {-# LINE 511 "dist/build/Desugar.hs"#-}
   {-# INLINE rule27 #-}
   {-# LINE 16 "./src-ag/DistChildAttr.ag" #-}
   rule27 = \ ((!_nontsIsynMap') :: Map Identifier Attributes) ->
                             {-# LINE 16 "./src-ag/DistChildAttr.ag" #-}
                             _nontsIsynMap'
                             {-# LINE 517 "dist/build/Desugar.hs"#-}
   {-# INLINE rule28 #-}
   {-# LINE 235 "./src-ag/Desugar.ag" #-}
   rule28 = \ !augmentsMap_ ->
                           {-# LINE 235 "./src-ag/Desugar.ag" #-}
                           augmentsMap_
                           {-# LINE 523 "dist/build/Desugar.hs"#-}
   {-# INLINE rule29 #-}
   {-# LINE 319 "./src-ag/Desugar.ag" #-}
   rule29 = \ ((!_nontsIaugmentsOut) :: Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) ((!_nontsIoutput) :: Nonterminals) !aroundsMap_ !contextMap_ !derivings_ !manualAttrOrderMap_ !mergeMap_ !paramMap_ !pragmas_ !quantMap_ !typeSyns_ !uniqueMap_ !useMap_ !wrappers_ ->
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
                     {-# LINE 542 "dist/build/Desugar.hs"#-}
   {-# INLINE rule30 #-}
   rule30 = \ ((!_nontsIallAttributes) :: AttrMap) ->
     _nontsIallAttributes
   {-# INLINE rule31 #-}
   rule31 = \ ((!_nontsIerrors) :: Seq Error) ->
     _nontsIerrors
   {-# INLINE rule33 #-}
   rule33 = \ ((!_lhsIforcedIrrefutables) :: AttrMap) ->
     _lhsIforcedIrrefutables
   {-# INLINE rule34 #-}
   rule34 = \ ((!_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule35 #-}
   rule35 = \ ((!_lhsIoptions) :: Options) ->
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
        let arg = T_HsToken_vIn4 _lhsIaddLines _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsInt _lhsIruleDescr _lhsIuseFieldIdent
        !(T_HsToken_vOut4 _lhsOaddLines _lhsOerrors _lhsOtks) <- return (inv_HsToken_s8 sem K_HsToken_v4 arg)
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
                               attach_T_HsToken :: Identity (T_HsToken_s8 )
                               }
data T_HsToken_s8  where C_HsToken_s8 :: {
                                         inv_HsToken_s8 :: !(forall t. K_HsToken_s8  t -> t)
                                         } -> T_HsToken_s8 
data T_HsToken_s9  = C_HsToken_s9
data T_HsToken_s37  = C_HsToken_s37
newtype T_HsToken_s47  = C_HsToken_s47 {
                                       inv_HsToken_s47 :: (T_HsToken_v35 )
                                       }
data K_HsToken_s8 k  where
   K_HsToken_v4 :: K_HsToken_s8  (T_HsToken_v4 )
   K_HsToken_v21 :: K_HsToken_s8  (T_HsToken_v21 )
   K_HsToken_v34 :: K_HsToken_s8  (T_HsToken_v34 )
type T_HsToken_v4  = (T_HsToken_vIn4 ) -> (T_HsToken_vOut4 )
data T_HsToken_vIn4  = T_HsToken_vIn4 !(Int) !([(Identifier, Identifier)]) !([(Identifier, Identifier)]) !(ConstructorIdent) !(NontermIdent) !(String) !(Bool)
data T_HsToken_vOut4  = T_HsToken_vOut4 !(Int) !(Seq Error) !(HsToken)
type T_HsToken_v21  = (T_HsToken_vIn21 ) -> (T_HsToken_vOut21 )
data T_HsToken_vIn21  = T_HsToken_vIn21 !(Int) !([(Identifier, Identifier)]) !(ConstructorIdent) !(NontermIdent) !(String) !(Bool)
data T_HsToken_vOut21  = T_HsToken_vOut21 !(Int) !(Seq Error) !(HsToken)
type T_HsToken_v34  = (T_HsToken_vIn34 ) -> (T_HsToken_vOut34 )
data T_HsToken_vIn34  = T_HsToken_vIn34 !([(Identifier, Identifier)]) !(ConstructorIdent) !(NontermIdent)
data T_HsToken_vOut34  = T_HsToken_vOut34 !(Seq Error) !(T_HsToken_s47 )
type T_HsToken_v35  = (T_HsToken_vIn35 ) -> (T_HsToken_vOut35 )
data T_HsToken_vIn35  = T_HsToken_vIn35 !(Int) !(String) !(Bool)
data T_HsToken_vOut35  = T_HsToken_vOut35 !(Int) !(HsToken)
{-# NOINLINE sem_HsToken_AGLocal #-}
sem_HsToken_AGLocal :: (Identifier) -> (Pos) -> (Maybe String) -> T_HsToken 
sem_HsToken_AGLocal !arg_var_ !arg_pos_ _ = T_HsToken (return st8) where
   {-# NOINLINE st8 #-}
   !st8 = let
      k8 :: K_HsToken_s8  t -> t
      k8 K_HsToken_v4 = v4
      k8 K_HsToken_v21 = v21
      k8 K_HsToken_v34 = v34
      v4 :: T_HsToken_v4 
      v4 = \ !(T_HsToken_vIn4 _lhsIaddLines _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsInt _lhsIruleDescr _lhsIuseFieldIdent) -> (
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule38  () in
         let _lhsOaddLines :: Int
             !_lhsOaddLines = rule36 _lhsIaddLines _lhsIuseFieldIdent in
         let !_tks = rule37 _lhsIaddLines _lhsIruleDescr _lhsIuseFieldIdent arg_pos_ arg_var_ in
         let _lhsOtks :: HsToken
             !_lhsOtks = rule39 _tks in
         let !__result_ = T_HsToken_vOut4 _lhsOaddLines _lhsOerrors _lhsOtks
          in __result_ )
      v21 :: T_HsToken_v21 
      v21 = \ !(T_HsToken_vIn21 _lhsIaddLines _lhsIchildSyns _lhsIcon _lhsInt _lhsIruleDescr _lhsIuseFieldIdent) -> (
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule38  () in
         let _lhsOaddLines :: Int
             !_lhsOaddLines = rule36 _lhsIaddLines _lhsIuseFieldIdent in
         let !_tks = rule37 _lhsIaddLines _lhsIruleDescr _lhsIuseFieldIdent arg_pos_ arg_var_ in
         let _lhsOtks :: HsToken
             !_lhsOtks = rule39 _tks in
         let !__result_ = T_HsToken_vOut21 _lhsOaddLines _lhsOerrors _lhsOtks
          in __result_ )
      v34 :: T_HsToken_v34 
      v34 = \ !(T_HsToken_vIn34 _lhsIchildSyns _lhsIcon _lhsInt) -> (
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule38  () in
         let !__st_ = st47  ()
             !__result_ = T_HsToken_vOut34 _lhsOerrors __st_
          in __result_ )
     in C_HsToken_s8 k8
   {-# NOINLINE st47 #-}
   st47 = \  (_ :: ()) -> let
      v35 :: T_HsToken_v35 
      v35 = \ !(T_HsToken_vIn35 _lhsIaddLines _lhsIruleDescr _lhsIuseFieldIdent) -> (
         let _lhsOaddLines :: Int
             !_lhsOaddLines = rule36 _lhsIaddLines _lhsIuseFieldIdent in
         let !_tks = rule37 _lhsIaddLines _lhsIruleDescr _lhsIuseFieldIdent arg_pos_ arg_var_ in
         let _lhsOtks :: HsToken
             !_lhsOtks = rule39 _tks in
         let !__result_ = T_HsToken_vOut35 _lhsOaddLines _lhsOtks
          in __result_ )
     in C_HsToken_s47 v35
   {-# NOINLINE[1] rule36 #-}
   {-# LINE 74 "./src-ag/Desugar.ag" #-}
   rule36 = \ ((!_lhsIaddLines) :: Int) ((!_lhsIuseFieldIdent) :: Bool) ->
                       {-# LINE 74 "./src-ag/Desugar.ag" #-}
                       if _lhsIuseFieldIdent
                       then _lhsIaddLines + 1
                       else _lhsIaddLines
                       {-# LINE 669 "dist/build/Desugar.hs"#-}
   {-# NOINLINE[1] rule37 #-}
   {-# LINE 77 "./src-ag/Desugar.ag" #-}
   rule37 = \ ((!_lhsIaddLines) :: Int) ((!_lhsIruleDescr) :: String) ((!_lhsIuseFieldIdent) :: Bool) !pos_ !var_ ->
                  {-# LINE 77 "./src-ag/Desugar.ag" #-}
                  AGLocal var_ (addl _lhsIaddLines pos_) (if _lhsIuseFieldIdent then Just _lhsIruleDescr else Nothing)
                  {-# LINE 675 "dist/build/Desugar.hs"#-}
   {-# NOINLINE[1] rule38 #-}
   rule38 = \  (_ :: ()) ->
     Seq.empty
   {-# NOINLINE[1] rule39 #-}
   rule39 = \ !_tks ->
     _tks
{-# NOINLINE sem_HsToken_AGField #-}
sem_HsToken_AGField :: (Identifier) -> (Identifier) -> (Pos) -> (Maybe String) -> T_HsToken 
sem_HsToken_AGField !arg_field_ !arg_attr_ !arg_pos_ _ = T_HsToken (return st8) where
   {-# NOINLINE st8 #-}
   !st8 = let
      k8 :: K_HsToken_s8  t -> t
      k8 K_HsToken_v4 = v4
      k8 K_HsToken_v21 = v21
      k8 K_HsToken_v34 = v34
      v4 :: T_HsToken_v4 
      v4 = \ !(T_HsToken_vIn4 _lhsIaddLines _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsInt _lhsIruleDescr _lhsIuseFieldIdent) -> (
         let !_mField = rule40 _lhsIchildSyns arg_attr_ arg_field_ in
         let !_field' = rule41 _mField arg_field_ in
         let _lhsOaddLines :: Int
             !_lhsOaddLines = rule43 _field' _lhsIaddLines _lhsIuseFieldIdent arg_field_ in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule42 _lhsIcon _lhsInt _mField arg_field_ in
         let !_tks = rule44 _field' _lhsIaddLines _lhsIruleDescr _lhsIuseFieldIdent arg_attr_ arg_pos_ in
         let _lhsOtks :: HsToken
             !_lhsOtks = rule45 _tks in
         let !__result_ = T_HsToken_vOut4 _lhsOaddLines _lhsOerrors _lhsOtks
          in __result_ )
      v21 :: T_HsToken_v21 
      v21 = \ !(T_HsToken_vIn21 _lhsIaddLines _lhsIchildSyns _lhsIcon _lhsInt _lhsIruleDescr _lhsIuseFieldIdent) -> (
         let !_mField = rule40 _lhsIchildSyns arg_attr_ arg_field_ in
         let !_field' = rule41 _mField arg_field_ in
         let _lhsOaddLines :: Int
             !_lhsOaddLines = rule43 _field' _lhsIaddLines _lhsIuseFieldIdent arg_field_ in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule42 _lhsIcon _lhsInt _mField arg_field_ in
         let !_tks = rule44 _field' _lhsIaddLines _lhsIruleDescr _lhsIuseFieldIdent arg_attr_ arg_pos_ in
         let _lhsOtks :: HsToken
             !_lhsOtks = rule45 _tks in
         let !__result_ = T_HsToken_vOut21 _lhsOaddLines _lhsOerrors _lhsOtks
          in __result_ )
      v34 :: T_HsToken_v34 
      v34 = \ !(T_HsToken_vIn34 _lhsIchildSyns _lhsIcon _lhsInt) -> (
         let !_mField = rule40 _lhsIchildSyns arg_attr_ arg_field_ in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule42 _lhsIcon _lhsInt _mField arg_field_ in
         let !__st_ = st47 _mField
             !__result_ = T_HsToken_vOut34 _lhsOerrors __st_
          in __result_ )
     in C_HsToken_s8 k8
   {-# NOINLINE st47 #-}
   st47 = \ !_mField -> let
      v35 :: T_HsToken_v35 
      v35 = \ !(T_HsToken_vIn35 _lhsIaddLines _lhsIruleDescr _lhsIuseFieldIdent) -> (
         let !_field' = rule41 _mField arg_field_ in
         let _lhsOaddLines :: Int
             !_lhsOaddLines = rule43 _field' _lhsIaddLines _lhsIuseFieldIdent arg_field_ in
         let !_tks = rule44 _field' _lhsIaddLines _lhsIruleDescr _lhsIuseFieldIdent arg_attr_ arg_pos_ in
         let _lhsOtks :: HsToken
             !_lhsOtks = rule45 _tks in
         let !__result_ = T_HsToken_vOut35 _lhsOaddLines _lhsOtks
          in __result_ )
     in C_HsToken_s47 v35
   {-# NOINLINE[1] rule40 #-}
   {-# LINE 79 "./src-ag/Desugar.ag" #-}
   rule40 = \ ((!_lhsIchildSyns) :: [(Identifier, Identifier)]) !attr_ !field_ ->
                     {-# LINE 79 "./src-ag/Desugar.ag" #-}
                     findField field_ attr_ _lhsIchildSyns
                     {-# LINE 744 "dist/build/Desugar.hs"#-}
   {-# NOINLINE[1] rule41 #-}
   {-# LINE 81 "./src-ag/Desugar.ag" #-}
   rule41 = \ !_mField !field_ ->
                     {-# LINE 81 "./src-ag/Desugar.ag" #-}
                     maybe field_ id _mField
                     {-# LINE 750 "dist/build/Desugar.hs"#-}
   {-# NOINLINE[1] rule42 #-}
   {-# LINE 82 "./src-ag/Desugar.ag" #-}
   rule42 = \ ((!_lhsIcon) :: ConstructorIdent) ((!_lhsInt) :: NontermIdent) !_mField !field_ ->
                     {-# LINE 82 "./src-ag/Desugar.ag" #-}
                     maybe (Seq.singleton (UndefAttr _lhsInt _lhsIcon field_ (Ident "<ANY>" (getPos field_)) False)) (const Seq.empty) _mField
                     {-# LINE 756 "dist/build/Desugar.hs"#-}
   {-# NOINLINE[1] rule43 #-}
   {-# LINE 84 "./src-ag/Desugar.ag" #-}
   rule43 = \ !_field' ((!_lhsIaddLines) :: Int) ((!_lhsIuseFieldIdent) :: Bool) !field_ ->
                       {-# LINE 84 "./src-ag/Desugar.ag" #-}
                       if _lhsIuseFieldIdent || length (getName field_) < length (getName _field'    )
                       then _lhsIaddLines + 1
                       else _lhsIaddLines
                       {-# LINE 764 "dist/build/Desugar.hs"#-}
   {-# NOINLINE[1] rule44 #-}
   {-# LINE 88 "./src-ag/Desugar.ag" #-}
   rule44 = \ !_field' ((!_lhsIaddLines) :: Int) ((!_lhsIruleDescr) :: String) ((!_lhsIuseFieldIdent) :: Bool) !attr_ !pos_ ->
                  {-# LINE 88 "./src-ag/Desugar.ag" #-}
                  AGField _field'     attr_ (addl _lhsIaddLines pos_) (if _lhsIuseFieldIdent then Just _lhsIruleDescr else Nothing)
                  {-# LINE 770 "dist/build/Desugar.hs"#-}
   {-# NOINLINE[1] rule45 #-}
   rule45 = \ !_tks ->
     _tks
{-# NOINLINE sem_HsToken_HsToken #-}
sem_HsToken_HsToken :: (String) -> (Pos) -> T_HsToken 
sem_HsToken_HsToken !arg_value_ !arg_pos_ = T_HsToken (return st8) where
   {-# NOINLINE st8 #-}
   !st8 = let
      k8 :: K_HsToken_s8  t -> t
      k8 K_HsToken_v4 = v4
      k8 K_HsToken_v21 = v21
      k8 K_HsToken_v34 = v34
      v4 :: T_HsToken_v4 
      v4 = \ !(T_HsToken_vIn4 _lhsIaddLines _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsInt _lhsIruleDescr _lhsIuseFieldIdent) -> (
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule47  () in
         let _lhsOaddLines :: Int
             !_lhsOaddLines = rule49 _lhsIaddLines in
         let !_tks = rule46 _lhsIaddLines arg_pos_ arg_value_ in
         let _lhsOtks :: HsToken
             !_lhsOtks = rule48 _tks in
         let !__result_ = T_HsToken_vOut4 _lhsOaddLines _lhsOerrors _lhsOtks
          in __result_ )
      v21 :: T_HsToken_v21 
      v21 = \ !(T_HsToken_vIn21 _lhsIaddLines _lhsIchildSyns _lhsIcon _lhsInt _lhsIruleDescr _lhsIuseFieldIdent) -> (
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule47  () in
         let _lhsOaddLines :: Int
             !_lhsOaddLines = rule49 _lhsIaddLines in
         let !_tks = rule46 _lhsIaddLines arg_pos_ arg_value_ in
         let _lhsOtks :: HsToken
             !_lhsOtks = rule48 _tks in
         let !__result_ = T_HsToken_vOut21 _lhsOaddLines _lhsOerrors _lhsOtks
          in __result_ )
      v34 :: T_HsToken_v34 
      v34 = \ !(T_HsToken_vIn34 _lhsIchildSyns _lhsIcon _lhsInt) -> (
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule47  () in
         let !__st_ = st47  ()
             !__result_ = T_HsToken_vOut34 _lhsOerrors __st_
          in __result_ )
     in C_HsToken_s8 k8
   {-# NOINLINE st47 #-}
   st47 = \  (_ :: ()) -> let
      v35 :: T_HsToken_v35 
      v35 = \ !(T_HsToken_vIn35 _lhsIaddLines _lhsIruleDescr _lhsIuseFieldIdent) -> (
         let _lhsOaddLines :: Int
             !_lhsOaddLines = rule49 _lhsIaddLines in
         let !_tks = rule46 _lhsIaddLines arg_pos_ arg_value_ in
         let _lhsOtks :: HsToken
             !_lhsOtks = rule48 _tks in
         let !__result_ = T_HsToken_vOut35 _lhsOaddLines _lhsOtks
          in __result_ )
     in C_HsToken_s47 v35
   {-# NOINLINE[1] rule46 #-}
   {-# LINE 90 "./src-ag/Desugar.ag" #-}
   rule46 = \ ((!_lhsIaddLines) :: Int) !pos_ !value_ ->
                  {-# LINE 90 "./src-ag/Desugar.ag" #-}
                  HsToken value_ (addl _lhsIaddLines pos_)
                  {-# LINE 830 "dist/build/Desugar.hs"#-}
   {-# NOINLINE[1] rule47 #-}
   rule47 = \  (_ :: ()) ->
     Seq.empty
   {-# NOINLINE[1] rule48 #-}
   rule48 = \ !_tks ->
     _tks
   {-# NOINLINE[1] rule49 #-}
   rule49 = \ ((!_lhsIaddLines) :: Int) ->
     _lhsIaddLines
{-# NOINLINE sem_HsToken_CharToken #-}
sem_HsToken_CharToken :: (String) -> (Pos) -> T_HsToken 
sem_HsToken_CharToken !arg_value_ !arg_pos_ = T_HsToken (return st8) where
   {-# NOINLINE st8 #-}
   !st8 = let
      k8 :: K_HsToken_s8  t -> t
      k8 K_HsToken_v4 = v4
      k8 K_HsToken_v21 = v21
      k8 K_HsToken_v34 = v34
      v4 :: T_HsToken_v4 
      v4 = \ !(T_HsToken_vIn4 _lhsIaddLines _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsInt _lhsIruleDescr _lhsIuseFieldIdent) -> (
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule51  () in
         let _lhsOaddLines :: Int
             !_lhsOaddLines = rule53 _lhsIaddLines in
         let !_tks = rule50 _lhsIaddLines arg_pos_ arg_value_ in
         let _lhsOtks :: HsToken
             !_lhsOtks = rule52 _tks in
         let !__result_ = T_HsToken_vOut4 _lhsOaddLines _lhsOerrors _lhsOtks
          in __result_ )
      v21 :: T_HsToken_v21 
      v21 = \ !(T_HsToken_vIn21 _lhsIaddLines _lhsIchildSyns _lhsIcon _lhsInt _lhsIruleDescr _lhsIuseFieldIdent) -> (
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule51  () in
         let _lhsOaddLines :: Int
             !_lhsOaddLines = rule53 _lhsIaddLines in
         let !_tks = rule50 _lhsIaddLines arg_pos_ arg_value_ in
         let _lhsOtks :: HsToken
             !_lhsOtks = rule52 _tks in
         let !__result_ = T_HsToken_vOut21 _lhsOaddLines _lhsOerrors _lhsOtks
          in __result_ )
      v34 :: T_HsToken_v34 
      v34 = \ !(T_HsToken_vIn34 _lhsIchildSyns _lhsIcon _lhsInt) -> (
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule51  () in
         let !__st_ = st47  ()
             !__result_ = T_HsToken_vOut34 _lhsOerrors __st_
          in __result_ )
     in C_HsToken_s8 k8
   {-# NOINLINE st47 #-}
   st47 = \  (_ :: ()) -> let
      v35 :: T_HsToken_v35 
      v35 = \ !(T_HsToken_vIn35 _lhsIaddLines _lhsIruleDescr _lhsIuseFieldIdent) -> (
         let _lhsOaddLines :: Int
             !_lhsOaddLines = rule53 _lhsIaddLines in
         let !_tks = rule50 _lhsIaddLines arg_pos_ arg_value_ in
         let _lhsOtks :: HsToken
             !_lhsOtks = rule52 _tks in
         let !__result_ = T_HsToken_vOut35 _lhsOaddLines _lhsOtks
          in __result_ )
     in C_HsToken_s47 v35
   {-# NOINLINE[1] rule50 #-}
   {-# LINE 92 "./src-ag/Desugar.ag" #-}
   rule50 = \ ((!_lhsIaddLines) :: Int) !pos_ !value_ ->
                  {-# LINE 92 "./src-ag/Desugar.ag" #-}
                  CharToken value_ (addl _lhsIaddLines pos_)
                  {-# LINE 896 "dist/build/Desugar.hs"#-}
   {-# NOINLINE[1] rule51 #-}
   rule51 = \  (_ :: ()) ->
     Seq.empty
   {-# NOINLINE[1] rule52 #-}
   rule52 = \ !_tks ->
     _tks
   {-# NOINLINE[1] rule53 #-}
   rule53 = \ ((!_lhsIaddLines) :: Int) ->
     _lhsIaddLines
{-# NOINLINE sem_HsToken_StrToken #-}
sem_HsToken_StrToken :: (String) -> (Pos) -> T_HsToken 
sem_HsToken_StrToken !arg_value_ !arg_pos_ = T_HsToken (return st8) where
   {-# NOINLINE st8 #-}
   !st8 = let
      k8 :: K_HsToken_s8  t -> t
      k8 K_HsToken_v4 = v4
      k8 K_HsToken_v21 = v21
      k8 K_HsToken_v34 = v34
      v4 :: T_HsToken_v4 
      v4 = \ !(T_HsToken_vIn4 _lhsIaddLines _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsInt _lhsIruleDescr _lhsIuseFieldIdent) -> (
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule55  () in
         let _lhsOaddLines :: Int
             !_lhsOaddLines = rule57 _lhsIaddLines in
         let !_tks = rule54 _lhsIaddLines arg_pos_ arg_value_ in
         let _lhsOtks :: HsToken
             !_lhsOtks = rule56 _tks in
         let !__result_ = T_HsToken_vOut4 _lhsOaddLines _lhsOerrors _lhsOtks
          in __result_ )
      v21 :: T_HsToken_v21 
      v21 = \ !(T_HsToken_vIn21 _lhsIaddLines _lhsIchildSyns _lhsIcon _lhsInt _lhsIruleDescr _lhsIuseFieldIdent) -> (
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule55  () in
         let _lhsOaddLines :: Int
             !_lhsOaddLines = rule57 _lhsIaddLines in
         let !_tks = rule54 _lhsIaddLines arg_pos_ arg_value_ in
         let _lhsOtks :: HsToken
             !_lhsOtks = rule56 _tks in
         let !__result_ = T_HsToken_vOut21 _lhsOaddLines _lhsOerrors _lhsOtks
          in __result_ )
      v34 :: T_HsToken_v34 
      v34 = \ !(T_HsToken_vIn34 _lhsIchildSyns _lhsIcon _lhsInt) -> (
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule55  () in
         let !__st_ = st47  ()
             !__result_ = T_HsToken_vOut34 _lhsOerrors __st_
          in __result_ )
     in C_HsToken_s8 k8
   {-# NOINLINE st47 #-}
   st47 = \  (_ :: ()) -> let
      v35 :: T_HsToken_v35 
      v35 = \ !(T_HsToken_vIn35 _lhsIaddLines _lhsIruleDescr _lhsIuseFieldIdent) -> (
         let _lhsOaddLines :: Int
             !_lhsOaddLines = rule57 _lhsIaddLines in
         let !_tks = rule54 _lhsIaddLines arg_pos_ arg_value_ in
         let _lhsOtks :: HsToken
             !_lhsOtks = rule56 _tks in
         let !__result_ = T_HsToken_vOut35 _lhsOaddLines _lhsOtks
          in __result_ )
     in C_HsToken_s47 v35
   {-# NOINLINE[1] rule54 #-}
   {-# LINE 94 "./src-ag/Desugar.ag" #-}
   rule54 = \ ((!_lhsIaddLines) :: Int) !pos_ !value_ ->
                  {-# LINE 94 "./src-ag/Desugar.ag" #-}
                  StrToken value_ (addl _lhsIaddLines pos_)
                  {-# LINE 962 "dist/build/Desugar.hs"#-}
   {-# NOINLINE[1] rule55 #-}
   rule55 = \  (_ :: ()) ->
     Seq.empty
   {-# NOINLINE[1] rule56 #-}
   rule56 = \ !_tks ->
     _tks
   {-# NOINLINE[1] rule57 #-}
   rule57 = \ ((!_lhsIaddLines) :: Int) ->
     _lhsIaddLines
{-# NOINLINE sem_HsToken_Err #-}
sem_HsToken_Err :: (String) -> (Pos) -> T_HsToken 
sem_HsToken_Err !arg_mesg_ !arg_pos_ = T_HsToken (return st8) where
   {-# NOINLINE st8 #-}
   !st8 = let
      k8 :: K_HsToken_s8  t -> t
      k8 K_HsToken_v4 = v4
      k8 K_HsToken_v21 = v21
      k8 K_HsToken_v34 = v34
      v4 :: T_HsToken_v4 
      v4 = \ !(T_HsToken_vIn4 _lhsIaddLines _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsInt _lhsIruleDescr _lhsIuseFieldIdent) -> (
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule59  () in
         let _lhsOaddLines :: Int
             !_lhsOaddLines = rule61 _lhsIaddLines in
         let !_tks = rule58 _lhsIaddLines arg_mesg_ arg_pos_ in
         let _lhsOtks :: HsToken
             !_lhsOtks = rule60 _tks in
         let !__result_ = T_HsToken_vOut4 _lhsOaddLines _lhsOerrors _lhsOtks
          in __result_ )
      v21 :: T_HsToken_v21 
      v21 = \ !(T_HsToken_vIn21 _lhsIaddLines _lhsIchildSyns _lhsIcon _lhsInt _lhsIruleDescr _lhsIuseFieldIdent) -> (
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule59  () in
         let _lhsOaddLines :: Int
             !_lhsOaddLines = rule61 _lhsIaddLines in
         let !_tks = rule58 _lhsIaddLines arg_mesg_ arg_pos_ in
         let _lhsOtks :: HsToken
             !_lhsOtks = rule60 _tks in
         let !__result_ = T_HsToken_vOut21 _lhsOaddLines _lhsOerrors _lhsOtks
          in __result_ )
      v34 :: T_HsToken_v34 
      v34 = \ !(T_HsToken_vIn34 _lhsIchildSyns _lhsIcon _lhsInt) -> (
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule59  () in
         let !__st_ = st47  ()
             !__result_ = T_HsToken_vOut34 _lhsOerrors __st_
          in __result_ )
     in C_HsToken_s8 k8
   {-# NOINLINE st47 #-}
   st47 = \  (_ :: ()) -> let
      v35 :: T_HsToken_v35 
      v35 = \ !(T_HsToken_vIn35 _lhsIaddLines _lhsIruleDescr _lhsIuseFieldIdent) -> (
         let _lhsOaddLines :: Int
             !_lhsOaddLines = rule61 _lhsIaddLines in
         let !_tks = rule58 _lhsIaddLines arg_mesg_ arg_pos_ in
         let _lhsOtks :: HsToken
             !_lhsOtks = rule60 _tks in
         let !__result_ = T_HsToken_vOut35 _lhsOaddLines _lhsOtks
          in __result_ )
     in C_HsToken_s47 v35
   {-# NOINLINE[1] rule58 #-}
   {-# LINE 96 "./src-ag/Desugar.ag" #-}
   rule58 = \ ((!_lhsIaddLines) :: Int) !mesg_ !pos_ ->
                  {-# LINE 96 "./src-ag/Desugar.ag" #-}
                  Err mesg_ (addl _lhsIaddLines pos_)
                  {-# LINE 1028 "dist/build/Desugar.hs"#-}
   {-# NOINLINE[1] rule59 #-}
   rule59 = \  (_ :: ()) ->
     Seq.empty
   {-# NOINLINE[1] rule60 #-}
   rule60 = \ !_tks ->
     _tks
   {-# NOINLINE[1] rule61 #-}
   rule61 = \ ((!_lhsIaddLines) :: Int) ->
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
        let arg = T_HsTokens_vIn5 _lhsIaddLines _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsInt _lhsIruleDescr _lhsIuseFieldIdent
        !(T_HsTokens_vOut5 _lhsOaddLines _lhsOerrors _lhsOtks) <- return (inv_HsTokens_s10 sem K_HsTokens_v5 arg)
        return (Syn_HsTokens _lhsOaddLines _lhsOerrors _lhsOtks)
   )

-- cata
{-# NOINLINE sem_HsTokens #-}
sem_HsTokens :: HsTokens  -> T_HsTokens 
sem_HsTokens list = Prelude.foldr sem_HsTokens_Cons sem_HsTokens_Nil (Prelude.map sem_HsToken list)

-- semantic domain
newtype T_HsTokens  = T_HsTokens {
                                 attach_T_HsTokens :: Identity (T_HsTokens_s10 )
                                 }
data T_HsTokens_s10  where C_HsTokens_s10 :: {
                                             inv_HsTokens_s10 :: !(forall t. K_HsTokens_s10  t -> t)
                                             } -> T_HsTokens_s10 
data T_HsTokens_s11  = C_HsTokens_s11
newtype T_HsTokens_s38  = C_HsTokens_s38 {
                                         inv_HsTokens_s38 :: (T_HsTokens_v23 )
                                         }
data T_HsTokens_s39  = C_HsTokens_s39
data K_HsTokens_s10 k  where
   K_HsTokens_v5 :: K_HsTokens_s10  (T_HsTokens_v5 )
   K_HsTokens_v22 :: K_HsTokens_s10  (T_HsTokens_v22 )
   K_HsTokens_v24 :: K_HsTokens_s10  (T_HsTokens_v24 )
type T_HsTokens_v5  = (T_HsTokens_vIn5 ) -> (T_HsTokens_vOut5 )
data T_HsTokens_vIn5  = T_HsTokens_vIn5 !(Int) !([(Identifier, Identifier)]) !([(Identifier, Identifier)]) !(ConstructorIdent) !(NontermIdent) !(String) !(Bool)
data T_HsTokens_vOut5  = T_HsTokens_vOut5 !(Int) !(Seq Error) !(HsTokens)
type T_HsTokens_v22  = (T_HsTokens_vIn22 ) -> (T_HsTokens_vOut22 )
data T_HsTokens_vIn22  = T_HsTokens_vIn22 !([(Identifier, Identifier)]) !(ConstructorIdent) !(NontermIdent)
data T_HsTokens_vOut22  = T_HsTokens_vOut22 !(Seq Error) !(T_HsTokens_s38 )
type T_HsTokens_v23  = (T_HsTokens_vIn23 ) -> (T_HsTokens_vOut23 )
data T_HsTokens_vIn23  = T_HsTokens_vIn23 !(Int) !(String) !(Bool)
data T_HsTokens_vOut23  = T_HsTokens_vOut23 !(Int) !(HsTokens)
type T_HsTokens_v24  = (T_HsTokens_vIn24 ) -> (T_HsTokens_vOut24 )
data T_HsTokens_vIn24  = T_HsTokens_vIn24 !(Int) !([(Identifier, Identifier)]) !(ConstructorIdent) !(NontermIdent) !(String) !(Bool)
data T_HsTokens_vOut24  = T_HsTokens_vOut24 !(Int) !(Seq Error) !(HsTokens)
{-# NOINLINE sem_HsTokens_Cons #-}
sem_HsTokens_Cons :: T_HsToken  -> T_HsTokens  -> T_HsTokens 
sem_HsTokens_Cons arg_hd_ arg_tl_ = T_HsTokens (return st10) where
   {-# NOINLINE st10 #-}
   !st10 = let
      k10 :: K_HsTokens_s10  t -> t
      k10 K_HsTokens_v5 = v5
      k10 K_HsTokens_v22 = v22
      k10 K_HsTokens_v24 = v24
      v5 :: T_HsTokens_v5 
      v5 = \ !(T_HsTokens_vIn5 _lhsIaddLines _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsInt _lhsIruleDescr _lhsIuseFieldIdent) -> (
         let !_hdX8 = Control.Monad.Identity.runIdentity (attach_T_HsToken (arg_hd_)) in
         let !_tlX10 = Control.Monad.Identity.runIdentity (attach_T_HsTokens (arg_tl_)) in
         let !_hdOaddLines = rule66 _lhsIaddLines in
         let !_hdOchildSyns = rule68 _lhsIchildSyns in
         let !_hdOuseFieldIdent = rule72 _lhsIuseFieldIdent in
         let !_tlOchildSyns = rule75 _lhsIchildSyns in
         let !_tlOuseFieldIdent = rule79 _lhsIuseFieldIdent in
         let !_hdOcon = rule69 _lhsIcon in
         let !_hdOnt = rule70 _lhsInt in
         let !_tlOcon = rule76 _lhsIcon in
         let !_tlOnt = rule77 _lhsInt in
         let !_hdOruleDescr = rule71 _lhsIruleDescr in
         let !_tlOruleDescr = rule78 _lhsIruleDescr in
         let !(T_HsToken_vOut21 _hdIaddLines _hdIerrors _hdItks) = inv_HsToken_s8 _hdX8 K_HsToken_v21 (T_HsToken_vIn21 _hdOaddLines _hdOchildSyns _hdOcon _hdOnt _hdOruleDescr _hdOuseFieldIdent) in
         let !(T_HsTokens_vOut22 _tlIerrors _tlX38) = inv_HsTokens_s10 _tlX10 K_HsTokens_v22 (T_HsTokens_vIn22 _tlOchildSyns _tlOcon _tlOnt) in
         let !_tlOaddLines = rule73 _hdIaddLines in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule62 _hdIerrors _tlIerrors in
         let !(T_HsTokens_vOut23 _tlIaddLines _tlItks) = inv_HsTokens_s38 _tlX38 (T_HsTokens_vIn23 _tlOaddLines _tlOruleDescr _tlOuseFieldIdent) in
         let _lhsOaddLines :: Int
             !_lhsOaddLines = rule65 _tlIaddLines in
         let !_tks = rule63 _hdItks _tlItks in
         let _lhsOtks :: HsTokens
             !_lhsOtks = rule64 _tks in
         let !__result_ = T_HsTokens_vOut5 _lhsOaddLines _lhsOerrors _lhsOtks
          in __result_ )
      v22 :: T_HsTokens_v22 
      v22 = \ !(T_HsTokens_vIn22 _lhsIchildSyns _lhsIcon _lhsInt) -> (
         let !_hdX8 = Control.Monad.Identity.runIdentity (attach_T_HsToken (arg_hd_)) in
         let !_tlX10 = Control.Monad.Identity.runIdentity (attach_T_HsTokens (arg_tl_)) in
         let !_hdOchildSyns = rule68 _lhsIchildSyns in
         let !_hdOcon = rule69 _lhsIcon in
         let !_hdOnt = rule70 _lhsInt in
         let !_tlOchildSyns = rule75 _lhsIchildSyns in
         let !_tlOcon = rule76 _lhsIcon in
         let !_tlOnt = rule77 _lhsInt in
         let !(T_HsToken_vOut34 _hdIerrors _hdX47) = inv_HsToken_s8 _hdX8 K_HsToken_v34 (T_HsToken_vIn34 _hdOchildSyns _hdOcon _hdOnt) in
         let !(T_HsTokens_vOut22 _tlIerrors _tlX38) = inv_HsTokens_s10 _tlX10 K_HsTokens_v22 (T_HsTokens_vIn22 _tlOchildSyns _tlOcon _tlOnt) in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule62 _hdIerrors _tlIerrors in
         let !__st_ = st38 _hdX47 _tlX38
             !__result_ = T_HsTokens_vOut22 _lhsOerrors __st_
          in __result_ )
      v24 :: T_HsTokens_v24 
      v24 = \ !(T_HsTokens_vIn24 _lhsIaddLines _lhsIchildSyns _lhsIcon _lhsInt _lhsIruleDescr _lhsIuseFieldIdent) -> (
         let !_hdX8 = Control.Monad.Identity.runIdentity (attach_T_HsToken (arg_hd_)) in
         let !_tlX10 = Control.Monad.Identity.runIdentity (attach_T_HsTokens (arg_tl_)) in
         let !_hdOaddLines = rule66 _lhsIaddLines in
         let !_hdOchildSyns = rule68 _lhsIchildSyns in
         let !_hdOuseFieldIdent = rule72 _lhsIuseFieldIdent in
         let !_tlOchildSyns = rule75 _lhsIchildSyns in
         let !_tlOuseFieldIdent = rule79 _lhsIuseFieldIdent in
         let !_hdOcon = rule69 _lhsIcon in
         let !_hdOnt = rule70 _lhsInt in
         let !_tlOcon = rule76 _lhsIcon in
         let !_tlOnt = rule77 _lhsInt in
         let !_hdOruleDescr = rule71 _lhsIruleDescr in
         let !_tlOruleDescr = rule78 _lhsIruleDescr in
         let !(T_HsToken_vOut21 _hdIaddLines _hdIerrors _hdItks) = inv_HsToken_s8 _hdX8 K_HsToken_v21 (T_HsToken_vIn21 _hdOaddLines _hdOchildSyns _hdOcon _hdOnt _hdOruleDescr _hdOuseFieldIdent) in
         let !(T_HsTokens_vOut22 _tlIerrors _tlX38) = inv_HsTokens_s10 _tlX10 K_HsTokens_v22 (T_HsTokens_vIn22 _tlOchildSyns _tlOcon _tlOnt) in
         let !_tlOaddLines = rule73 _hdIaddLines in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule62 _hdIerrors _tlIerrors in
         let !(T_HsTokens_vOut23 _tlIaddLines _tlItks) = inv_HsTokens_s38 _tlX38 (T_HsTokens_vIn23 _tlOaddLines _tlOruleDescr _tlOuseFieldIdent) in
         let _lhsOaddLines :: Int
             !_lhsOaddLines = rule65 _tlIaddLines in
         let !_tks = rule63 _hdItks _tlItks in
         let _lhsOtks :: HsTokens
             !_lhsOtks = rule64 _tks in
         let !__result_ = T_HsTokens_vOut24 _lhsOaddLines _lhsOerrors _lhsOtks
          in __result_ )
     in C_HsTokens_s10 k10
   {-# NOINLINE st38 #-}
   st38 = \ !_hdX47 !_tlX38 -> let
      v23 :: T_HsTokens_v23 
      v23 = \ !(T_HsTokens_vIn23 _lhsIaddLines _lhsIruleDescr _lhsIuseFieldIdent) -> (
         let !_hdOaddLines = rule66 _lhsIaddLines in
         let !_hdOuseFieldIdent = rule72 _lhsIuseFieldIdent in
         let !_tlOuseFieldIdent = rule79 _lhsIuseFieldIdent in
         let !_hdOruleDescr = rule71 _lhsIruleDescr in
         let !_tlOruleDescr = rule78 _lhsIruleDescr in
         let !(T_HsToken_vOut35 _hdIaddLines _hdItks) = inv_HsToken_s47 _hdX47 (T_HsToken_vIn35 _hdOaddLines _hdOruleDescr _hdOuseFieldIdent) in
         let !_tlOaddLines = rule73 _hdIaddLines in
         let !(T_HsTokens_vOut23 _tlIaddLines _tlItks) = inv_HsTokens_s38 _tlX38 (T_HsTokens_vIn23 _tlOaddLines _tlOruleDescr _tlOuseFieldIdent) in
         let _lhsOaddLines :: Int
             !_lhsOaddLines = rule65 _tlIaddLines in
         let !_tks = rule63 _hdItks _tlItks in
         let _lhsOtks :: HsTokens
             !_lhsOtks = rule64 _tks in
         let !__result_ = T_HsTokens_vOut23 _lhsOaddLines _lhsOtks
          in __result_ )
     in C_HsTokens_s38 v23
   {-# NOINLINE[1] rule62 #-}
   rule62 = \ ((!_hdIerrors) :: Seq Error) ((!_tlIerrors) :: Seq Error) ->
     _hdIerrors Seq.>< _tlIerrors
   {-# NOINLINE[1] rule63 #-}
   rule63 = \ ((!_hdItks) :: HsToken) ((!_tlItks) :: HsTokens) ->
     (:) _hdItks _tlItks
   {-# NOINLINE[1] rule64 #-}
   rule64 = \ !_tks ->
     _tks
   {-# NOINLINE[1] rule65 #-}
   rule65 = \ ((!_tlIaddLines) :: Int) ->
     _tlIaddLines
   {-# NOINLINE[1] rule66 #-}
   rule66 = \ ((!_lhsIaddLines) :: Int) ->
     _lhsIaddLines
   {-# NOINLINE[1] rule68 #-}
   rule68 = \ ((!_lhsIchildSyns) :: [(Identifier, Identifier)]) ->
     _lhsIchildSyns
   {-# NOINLINE[1] rule69 #-}
   rule69 = \ ((!_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# NOINLINE[1] rule70 #-}
   rule70 = \ ((!_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# NOINLINE[1] rule71 #-}
   rule71 = \ ((!_lhsIruleDescr) :: String) ->
     _lhsIruleDescr
   {-# NOINLINE[1] rule72 #-}
   rule72 = \ ((!_lhsIuseFieldIdent) :: Bool) ->
     _lhsIuseFieldIdent
   {-# NOINLINE[1] rule73 #-}
   rule73 = \ ((!_hdIaddLines) :: Int) ->
     _hdIaddLines
   {-# NOINLINE[1] rule75 #-}
   rule75 = \ ((!_lhsIchildSyns) :: [(Identifier, Identifier)]) ->
     _lhsIchildSyns
   {-# NOINLINE[1] rule76 #-}
   rule76 = \ ((!_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# NOINLINE[1] rule77 #-}
   rule77 = \ ((!_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# NOINLINE[1] rule78 #-}
   rule78 = \ ((!_lhsIruleDescr) :: String) ->
     _lhsIruleDescr
   {-# NOINLINE[1] rule79 #-}
   rule79 = \ ((!_lhsIuseFieldIdent) :: Bool) ->
     _lhsIuseFieldIdent
{-# NOINLINE sem_HsTokens_Nil #-}
sem_HsTokens_Nil ::  T_HsTokens 
sem_HsTokens_Nil  = T_HsTokens (return st10) where
   {-# NOINLINE st10 #-}
   !st10 = let
      k10 :: K_HsTokens_s10  t -> t
      k10 K_HsTokens_v5 = v5
      k10 K_HsTokens_v22 = v22
      k10 K_HsTokens_v24 = v24
      v5 :: T_HsTokens_v5 
      v5 = \ !(T_HsTokens_vIn5 _lhsIaddLines _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsInt _lhsIruleDescr _lhsIuseFieldIdent) -> (
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule80  () in
         let !_tks = rule81  () in
         let _lhsOaddLines :: Int
             !_lhsOaddLines = rule83 _lhsIaddLines in
         let _lhsOtks :: HsTokens
             !_lhsOtks = rule82 _tks in
         let !__result_ = T_HsTokens_vOut5 _lhsOaddLines _lhsOerrors _lhsOtks
          in __result_ )
      v22 :: T_HsTokens_v22 
      v22 = \ !(T_HsTokens_vIn22 _lhsIchildSyns _lhsIcon _lhsInt) -> (
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule80  () in
         let !__st_ = st38  ()
             !__result_ = T_HsTokens_vOut22 _lhsOerrors __st_
          in __result_ )
      v24 :: T_HsTokens_v24 
      v24 = \ !(T_HsTokens_vIn24 _lhsIaddLines _lhsIchildSyns _lhsIcon _lhsInt _lhsIruleDescr _lhsIuseFieldIdent) -> (
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule80  () in
         let !_tks = rule81  () in
         let _lhsOaddLines :: Int
             !_lhsOaddLines = rule83 _lhsIaddLines in
         let _lhsOtks :: HsTokens
             !_lhsOtks = rule82 _tks in
         let !__result_ = T_HsTokens_vOut24 _lhsOaddLines _lhsOerrors _lhsOtks
          in __result_ )
     in C_HsTokens_s10 k10
   {-# NOINLINE st38 #-}
   st38 = \  (_ :: ()) -> let
      v23 :: T_HsTokens_v23 
      v23 = \ !(T_HsTokens_vIn23 _lhsIaddLines _lhsIruleDescr _lhsIuseFieldIdent) -> (
         let !_tks = rule81  () in
         let _lhsOaddLines :: Int
             !_lhsOaddLines = rule83 _lhsIaddLines in
         let _lhsOtks :: HsTokens
             !_lhsOtks = rule82 _tks in
         let !__result_ = T_HsTokens_vOut23 _lhsOaddLines _lhsOtks
          in __result_ )
     in C_HsTokens_s38 v23
   {-# NOINLINE[1] rule80 #-}
   rule80 = \  (_ :: ()) ->
     Seq.empty
   {-# NOINLINE[1] rule81 #-}
   rule81 = \  (_ :: ()) ->
     []
   {-# NOINLINE[1] rule82 #-}
   rule82 = \ !_tks ->
     _tks
   {-# NOINLINE[1] rule83 #-}
   rule83 = \ ((!_lhsIaddLines) :: Int) ->
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
        let arg = T_HsTokensRoot_vIn6 _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsInt _lhsIruleDescr _lhsIuseFieldIdent
        !(T_HsTokensRoot_vOut6 _lhsOerrors _lhsOtks) <- return (inv_HsTokensRoot_s12 sem arg)
        return (Syn_HsTokensRoot _lhsOerrors _lhsOtks)
   )

-- cata
{-# INLINE sem_HsTokensRoot #-}
sem_HsTokensRoot :: HsTokensRoot  -> T_HsTokensRoot 
sem_HsTokensRoot ( HsTokensRoot tokens_ ) = sem_HsTokensRoot_HsTokensRoot ( sem_HsTokens tokens_ )

-- semantic domain
newtype T_HsTokensRoot  = T_HsTokensRoot {
                                         attach_T_HsTokensRoot :: Identity (T_HsTokensRoot_s12 )
                                         }
newtype T_HsTokensRoot_s12  = C_HsTokensRoot_s12 {
                                                 inv_HsTokensRoot_s12 :: (T_HsTokensRoot_v6 )
                                                 }
data T_HsTokensRoot_s13  = C_HsTokensRoot_s13
type T_HsTokensRoot_v6  = (T_HsTokensRoot_vIn6 ) -> (T_HsTokensRoot_vOut6 )
data T_HsTokensRoot_vIn6  = T_HsTokensRoot_vIn6 !([(Identifier, Identifier)]) !([(Identifier, Identifier)]) !(ConstructorIdent) !(NontermIdent) !(String) !(Bool)
data T_HsTokensRoot_vOut6  = T_HsTokensRoot_vOut6 !(Seq Error) !([HsToken])
{-# NOINLINE sem_HsTokensRoot_HsTokensRoot #-}
sem_HsTokensRoot_HsTokensRoot :: T_HsTokens  -> T_HsTokensRoot 
sem_HsTokensRoot_HsTokensRoot arg_tokens_ = T_HsTokensRoot (return st12) where
   {-# NOINLINE st12 #-}
   !st12 = let
      v6 :: T_HsTokensRoot_v6 
      v6 = \ !(T_HsTokensRoot_vIn6 _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsInt _lhsIruleDescr _lhsIuseFieldIdent) -> (
         let !_tokensX10 = Control.Monad.Identity.runIdentity (attach_T_HsTokens (arg_tokens_)) in
         let !_tokensOaddLines = rule84  () in
         let !_tokensOchildSyns = rule88 _lhsIchildSyns in
         let !_tokensOcon = rule89 _lhsIcon in
         let !_tokensOnt = rule90 _lhsInt in
         let !_tokensOruleDescr = rule91 _lhsIruleDescr in
         let !_tokensOuseFieldIdent = rule92 _lhsIuseFieldIdent in
         let !(T_HsTokens_vOut24 _tokensIaddLines _tokensIerrors _tokensItks) = inv_HsTokens_s10 _tokensX10 K_HsTokens_v24 (T_HsTokens_vIn24 _tokensOaddLines _tokensOchildSyns _tokensOcon _tokensOnt _tokensOruleDescr _tokensOuseFieldIdent) in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule85 _tokensIerrors in
         let _lhsOtks :: [HsToken]
             !_lhsOtks = rule86 _tokensItks in
         let !__result_ = T_HsTokensRoot_vOut6 _lhsOerrors _lhsOtks
          in __result_ )
     in C_HsTokensRoot_s12 v6
   {-# INLINE rule84 #-}
   {-# LINE 67 "./src-ag/Desugar.ag" #-}
   rule84 = \  (_ :: ()) ->
                          {-# LINE 67 "./src-ag/Desugar.ag" #-}
                          0
                          {-# LINE 1358 "dist/build/Desugar.hs"#-}
   {-# INLINE rule85 #-}
   rule85 = \ ((!_tokensIerrors) :: Seq Error) ->
     _tokensIerrors
   {-# INLINE rule86 #-}
   rule86 = \ ((!_tokensItks) :: HsTokens) ->
     _tokensItks
   {-# INLINE rule88 #-}
   rule88 = \ ((!_lhsIchildSyns) :: [(Identifier, Identifier)]) ->
     _lhsIchildSyns
   {-# INLINE rule89 #-}
   rule89 = \ ((!_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# INLINE rule90 #-}
   rule90 = \ ((!_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule91 #-}
   rule91 = \ ((!_lhsIruleDescr) :: String) ->
     _lhsIruleDescr
   {-# INLINE rule92 #-}
   rule92 = \ ((!_lhsIuseFieldIdent) :: Bool) ->
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
        let arg = T_Nonterminal_vIn7 _lhsIaugmentsIn _lhsIforcedIrrefutables _lhsIinhMap _lhsImainName _lhsIoptions _lhsIsynMap
        !(T_Nonterminal_vOut7 _lhsOallAttributes _lhsOaugmentsOut _lhsOerrors _lhsOinhMap' _lhsOoutput _lhsOsynMap') <- return (inv_Nonterminal_s14 sem K_Nonterminal_v7 arg)
        return (Syn_Nonterminal _lhsOallAttributes _lhsOaugmentsOut _lhsOerrors _lhsOinhMap' _lhsOoutput _lhsOsynMap')
   )

-- cata
{-# INLINE sem_Nonterminal #-}
sem_Nonterminal :: Nonterminal  -> T_Nonterminal 
sem_Nonterminal ( Nonterminal !nt_ !params_ !inh_ !syn_ prods_ ) = sem_Nonterminal_Nonterminal nt_ params_ inh_ syn_ ( sem_Productions prods_ )

-- semantic domain
newtype T_Nonterminal  = T_Nonterminal {
                                       attach_T_Nonterminal :: Identity (T_Nonterminal_s14 )
                                       }
data T_Nonterminal_s14  where C_Nonterminal_s14 :: {
                                                   inv_Nonterminal_s14 :: !(forall t. K_Nonterminal_s14  t -> t)
                                                   } -> T_Nonterminal_s14 
data T_Nonterminal_s15  = C_Nonterminal_s15
newtype T_Nonterminal_s46  = C_Nonterminal_s46 {
                                               inv_Nonterminal_s46 :: (T_Nonterminal_v33 )
                                               }
data K_Nonterminal_s14 k  where
   K_Nonterminal_v7 :: K_Nonterminal_s14  (T_Nonterminal_v7 )
   K_Nonterminal_v32 :: K_Nonterminal_s14  (T_Nonterminal_v32 )
type T_Nonterminal_v7  = (T_Nonterminal_vIn7 ) -> (T_Nonterminal_vOut7 )
data T_Nonterminal_vIn7  = T_Nonterminal_vIn7 !(Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) !(AttrMap) !(Map Identifier Attributes) !(String) !(Options) !(Map Identifier Attributes)
data T_Nonterminal_vOut7  = T_Nonterminal_vOut7 !(AttrMap) !(Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) !(Seq Error) !(Map Identifier Attributes) !(Nonterminal) !(Map Identifier Attributes)
type T_Nonterminal_v32  = (T_Nonterminal_vIn32 ) -> (T_Nonterminal_vOut32 )
data T_Nonterminal_vIn32  = T_Nonterminal_vIn32 
data T_Nonterminal_vOut32  = T_Nonterminal_vOut32 !(AttrMap) !(Map Identifier Attributes) !(Map Identifier Attributes) !(T_Nonterminal_s46 )
type T_Nonterminal_v33  = (T_Nonterminal_vIn33 ) -> (T_Nonterminal_vOut33 )
data T_Nonterminal_vIn33  = T_Nonterminal_vIn33 !(Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) !(AttrMap) !(Map Identifier Attributes) !(String) !(Options) !(Map Identifier Attributes)
data T_Nonterminal_vOut33  = T_Nonterminal_vOut33 !(Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) !(Seq Error) !(Nonterminal)
{-# NOINLINE sem_Nonterminal_Nonterminal #-}
sem_Nonterminal_Nonterminal :: (NontermIdent) -> ([Identifier]) -> (Attributes) -> (Attributes) -> T_Productions  -> T_Nonterminal 
sem_Nonterminal_Nonterminal !arg_nt_ !arg_params_ !arg_inh_ !arg_syn_ arg_prods_ = T_Nonterminal (return st14) where
   {-# NOINLINE st14 #-}
   !st14 = let
      k14 :: K_Nonterminal_s14  t -> t
      k14 K_Nonterminal_v7 = v7
      k14 K_Nonterminal_v32 = v32
      v7 :: T_Nonterminal_v7 
      v7 = \ !(T_Nonterminal_vIn7 _lhsIaugmentsIn _lhsIforcedIrrefutables _lhsIinhMap _lhsImainName _lhsIoptions _lhsIsynMap) -> (
         let !_prodsX24 = Control.Monad.Identity.runIdentity (attach_T_Productions (arg_prods_)) in
         let !_prodsOnt = rule95 arg_nt_ in
         let !_augmentsIn = rule96 _lhsIaugmentsIn arg_nt_ in
         let !_prodsOaugmentsIn = rule104 _augmentsIn in
         let !_prodsOinhMap = rule106 _lhsIinhMap in
         let !_prodsOoptions = rule108 _lhsIoptions in
         let !_prodsOsynMap = rule109 _lhsIsynMap in
         let _lhsOinhMap' :: Map Identifier Attributes
             !_lhsOinhMap' = rule93 arg_inh_ arg_nt_ in
         let !_prodsOforcedIrrefutables = rule105 _lhsIforcedIrrefutables in
         let !_extraInh = rule98 _lhsImainName _lhsIoptions in
         let _lhsOsynMap' :: Map Identifier Attributes
             !_lhsOsynMap' = rule94 arg_nt_ arg_syn_ in
         let !(T_Productions_vOut25 _prodsIallAttributes _prodsIaugmentsOut _prodsIerrors _prodsIoutput) = inv_Productions_s24 _prodsX24 K_Productions_v25 (T_Productions_vIn25 _prodsOaugmentsIn _prodsOforcedIrrefutables _prodsOinhMap _prodsOnt _prodsOoptions _prodsOsynMap) in
         let _lhsOallAttributes :: AttrMap
             !_lhsOallAttributes = rule100 _prodsIallAttributes in
         let !_augmentsOut = rule97 _prodsIaugmentsOut arg_nt_ in
         let _lhsOaugmentsOut :: Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))
             !_lhsOaugmentsOut = rule101 _augmentsOut in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule102 _prodsIerrors in
         let _lhsOoutput :: Nonterminal
             !_lhsOoutput = rule99 _extraInh _prodsIoutput arg_inh_ arg_nt_ arg_params_ arg_syn_ in
         let !__result_ = T_Nonterminal_vOut7 _lhsOallAttributes _lhsOaugmentsOut _lhsOerrors _lhsOinhMap' _lhsOoutput _lhsOsynMap'
          in __result_ )
      v32 :: T_Nonterminal_v32 
      v32 = \ !(T_Nonterminal_vIn32 ) -> (
         let !_prodsX24 = Control.Monad.Identity.runIdentity (attach_T_Productions (arg_prods_)) in
         let !_prodsOnt = rule95 arg_nt_ in
         let _lhsOinhMap' :: Map Identifier Attributes
             !_lhsOinhMap' = rule93 arg_inh_ arg_nt_ in
         let _lhsOsynMap' :: Map Identifier Attributes
             !_lhsOsynMap' = rule94 arg_nt_ arg_syn_ in
         let !(T_Productions_vOut39 _prodsIallAttributes _prodsX50) = inv_Productions_s24 _prodsX24 K_Productions_v39 (T_Productions_vIn39 _prodsOnt) in
         let _lhsOallAttributes :: AttrMap
             !_lhsOallAttributes = rule100 _prodsIallAttributes in
         let !__st_ = st46 _prodsX50
             !__result_ = T_Nonterminal_vOut32 _lhsOallAttributes _lhsOinhMap' _lhsOsynMap' __st_
          in __result_ )
     in C_Nonterminal_s14 k14
   {-# NOINLINE st46 #-}
   st46 = \ !_prodsX50 -> let
      v33 :: T_Nonterminal_v33 
      v33 = \ !(T_Nonterminal_vIn33 _lhsIaugmentsIn _lhsIforcedIrrefutables _lhsIinhMap _lhsImainName _lhsIoptions _lhsIsynMap) -> (
         let !_augmentsIn = rule96 _lhsIaugmentsIn arg_nt_ in
         let !_prodsOaugmentsIn = rule104 _augmentsIn in
         let !_prodsOinhMap = rule106 _lhsIinhMap in
         let !_prodsOoptions = rule108 _lhsIoptions in
         let !_prodsOsynMap = rule109 _lhsIsynMap in
         let !_prodsOforcedIrrefutables = rule105 _lhsIforcedIrrefutables in
         let !_extraInh = rule98 _lhsImainName _lhsIoptions in
         let !(T_Productions_vOut40 _prodsIaugmentsOut _prodsIerrors _prodsIoutput) = inv_Productions_s50 _prodsX50 (T_Productions_vIn40 _prodsOaugmentsIn _prodsOforcedIrrefutables _prodsOinhMap _prodsOoptions _prodsOsynMap) in
         let !_augmentsOut = rule97 _prodsIaugmentsOut arg_nt_ in
         let _lhsOaugmentsOut :: Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))
             !_lhsOaugmentsOut = rule101 _augmentsOut in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule102 _prodsIerrors in
         let _lhsOoutput :: Nonterminal
             !_lhsOoutput = rule99 _extraInh _prodsIoutput arg_inh_ arg_nt_ arg_params_ arg_syn_ in
         let !__result_ = T_Nonterminal_vOut33 _lhsOaugmentsOut _lhsOerrors _lhsOoutput
          in __result_ )
     in C_Nonterminal_s46 v33
   {-# NOINLINE[1] rule93 #-}
   {-# LINE 7 "./src-ag/DistChildAttr.ag" #-}
   rule93 = \ !inh_ !nt_ ->
                                 {-# LINE 7 "./src-ag/DistChildAttr.ag" #-}
                                 Map.singleton nt_ inh_
                                 {-# LINE 1500 "dist/build/Desugar.hs"#-}
   {-# NOINLINE[1] rule94 #-}
   {-# LINE 8 "./src-ag/DistChildAttr.ag" #-}
   rule94 = \ !nt_ !syn_ ->
                                 {-# LINE 8 "./src-ag/DistChildAttr.ag" #-}
                                 Map.singleton nt_ syn_
                                 {-# LINE 1506 "dist/build/Desugar.hs"#-}
   {-# NOINLINE[1] rule95 #-}
   {-# LINE 157 "./src-ag/Desugar.ag" #-}
   rule95 = \ !nt_ ->
                   {-# LINE 157 "./src-ag/Desugar.ag" #-}
                   nt_
                   {-# LINE 1512 "dist/build/Desugar.hs"#-}
   {-# NOINLINE[1] rule96 #-}
   {-# LINE 239 "./src-ag/Desugar.ag" #-}
   rule96 = \ ((!_lhsIaugmentsIn) :: Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) !nt_ ->
                         {-# LINE 239 "./src-ag/Desugar.ag" #-}
                         Map.findWithDefault Map.empty nt_ _lhsIaugmentsIn
                         {-# LINE 1518 "dist/build/Desugar.hs"#-}
   {-# NOINLINE[1] rule97 #-}
   {-# LINE 240 "./src-ag/Desugar.ag" #-}
   rule97 = \ ((!_prodsIaugmentsOut) :: Map ConstructorIdent (Map Identifier [Expression])) !nt_ ->
                          {-# LINE 240 "./src-ag/Desugar.ag" #-}
                          Map.singleton nt_ _prodsIaugmentsOut
                          {-# LINE 1524 "dist/build/Desugar.hs"#-}
   {-# NOINLINE[1] rule98 #-}
   {-# LINE 292 "./src-ag/Desugar.ag" #-}
   rule98 = \ ((!_lhsImainName) :: String) ((!_lhsIoptions) :: Options) ->
                   {-# LINE 292 "./src-ag/Desugar.ag" #-}
                   addLateAttr _lhsIoptions _lhsImainName
                   {-# LINE 1530 "dist/build/Desugar.hs"#-}
   {-# NOINLINE[1] rule99 #-}
   {-# LINE 308 "./src-ag/Desugar.ag" #-}
   rule99 = \ !_extraInh ((!_prodsIoutput) :: Productions) !inh_ !nt_ !params_ !syn_ ->
                 {-# LINE 308 "./src-ag/Desugar.ag" #-}
                 Nonterminal
                   nt_ params_
                   (_extraInh     `Map.union` inh_)
                   syn_
                   _prodsIoutput
                 {-# LINE 1540 "dist/build/Desugar.hs"#-}
   {-# NOINLINE[1] rule100 #-}
   rule100 = \ ((!_prodsIallAttributes) :: AttrMap) ->
     _prodsIallAttributes
   {-# NOINLINE[1] rule101 #-}
   rule101 = \ !_augmentsOut ->
     _augmentsOut
   {-# NOINLINE[1] rule102 #-}
   rule102 = \ ((!_prodsIerrors) :: Seq Error) ->
     _prodsIerrors
   {-# NOINLINE[1] rule104 #-}
   rule104 = \ !_augmentsIn ->
     _augmentsIn
   {-# NOINLINE[1] rule105 #-}
   rule105 = \ ((!_lhsIforcedIrrefutables) :: AttrMap) ->
     _lhsIforcedIrrefutables
   {-# NOINLINE[1] rule106 #-}
   rule106 = \ ((!_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# NOINLINE[1] rule108 #-}
   rule108 = \ ((!_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# NOINLINE[1] rule109 #-}
   rule109 = \ ((!_lhsIsynMap) :: Map Identifier Attributes) ->
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
        let arg = T_Nonterminals_vIn8 _lhsIaugmentsIn _lhsIforcedIrrefutables _lhsIinhMap _lhsImainName _lhsIoptions _lhsIsynMap
        !(T_Nonterminals_vOut8 _lhsOallAttributes _lhsOaugmentsOut _lhsOerrors _lhsOinhMap' _lhsOoutput _lhsOsynMap') <- return (inv_Nonterminals_s16 sem K_Nonterminals_v8 arg)
        return (Syn_Nonterminals _lhsOallAttributes _lhsOaugmentsOut _lhsOerrors _lhsOinhMap' _lhsOoutput _lhsOsynMap')
   )

-- cata
{-# NOINLINE sem_Nonterminals #-}
sem_Nonterminals :: Nonterminals  -> T_Nonterminals 
sem_Nonterminals list = Prelude.foldr sem_Nonterminals_Cons sem_Nonterminals_Nil (Prelude.map sem_Nonterminal list)

-- semantic domain
newtype T_Nonterminals  = T_Nonterminals {
                                         attach_T_Nonterminals :: Identity (T_Nonterminals_s16 )
                                         }
data T_Nonterminals_s16  where C_Nonterminals_s16 :: {
                                                     inv_Nonterminals_s16 :: !(forall t. K_Nonterminals_s16  t -> t)
                                                     } -> T_Nonterminals_s16 
data T_Nonterminals_s17  = C_Nonterminals_s17
newtype T_Nonterminals_s36  = C_Nonterminals_s36 {
                                                 inv_Nonterminals_s36 :: (T_Nonterminals_v20 )
                                                 }
data K_Nonterminals_s16 k  where
   K_Nonterminals_v8 :: K_Nonterminals_s16  (T_Nonterminals_v8 )
   K_Nonterminals_v19 :: K_Nonterminals_s16  (T_Nonterminals_v19 )
type T_Nonterminals_v8  = (T_Nonterminals_vIn8 ) -> (T_Nonterminals_vOut8 )
data T_Nonterminals_vIn8  = T_Nonterminals_vIn8 !(Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) !(AttrMap) !(Map Identifier Attributes) !(String) !(Options) !(Map Identifier Attributes)
data T_Nonterminals_vOut8  = T_Nonterminals_vOut8 !(AttrMap) !(Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) !(Seq Error) !(Map Identifier Attributes) !(Nonterminals) !(Map Identifier Attributes)
type T_Nonterminals_v19  = (T_Nonterminals_vIn19 ) -> (T_Nonterminals_vOut19 )
data T_Nonterminals_vIn19  = T_Nonterminals_vIn19 
data T_Nonterminals_vOut19  = T_Nonterminals_vOut19 !(AttrMap) !(Map Identifier Attributes) !(Map Identifier Attributes) !(T_Nonterminals_s36 )
type T_Nonterminals_v20  = (T_Nonterminals_vIn20 ) -> (T_Nonterminals_vOut20 )
data T_Nonterminals_vIn20  = T_Nonterminals_vIn20 !(Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) !(AttrMap) !(Map Identifier Attributes) !(String) !(Options) !(Map Identifier Attributes)
data T_Nonterminals_vOut20  = T_Nonterminals_vOut20 !(Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) !(Seq Error) !(Nonterminals)
{-# NOINLINE sem_Nonterminals_Cons #-}
sem_Nonterminals_Cons :: T_Nonterminal  -> T_Nonterminals  -> T_Nonterminals 
sem_Nonterminals_Cons arg_hd_ arg_tl_ = T_Nonterminals (return st16) where
   {-# NOINLINE st16 #-}
   !st16 = let
      k16 :: K_Nonterminals_s16  t -> t
      k16 K_Nonterminals_v8 = v8
      k16 K_Nonterminals_v19 = v19
      v8 :: T_Nonterminals_v8 
      v8 = \ !(T_Nonterminals_vIn8 _lhsIaugmentsIn _lhsIforcedIrrefutables _lhsIinhMap _lhsImainName _lhsIoptions _lhsIsynMap) -> (
         let !_hdX14 = Control.Monad.Identity.runIdentity (attach_T_Nonterminal (arg_hd_)) in
         let !_tlX16 = Control.Monad.Identity.runIdentity (attach_T_Nonterminals (arg_tl_)) in
         let !_hdOaugmentsIn = rule117 _lhsIaugmentsIn in
         let !_hdOinhMap = rule119 _lhsIinhMap in
         let !_hdOoptions = rule121 _lhsIoptions in
         let !_hdOsynMap = rule122 _lhsIsynMap in
         let !_tlOaugmentsIn = rule123 _lhsIaugmentsIn in
         let !_tlOinhMap = rule125 _lhsIinhMap in
         let !_tlOoptions = rule127 _lhsIoptions in
         let !_tlOsynMap = rule128 _lhsIsynMap in
         let !_hdOforcedIrrefutables = rule118 _lhsIforcedIrrefutables in
         let !_hdOmainName = rule120 _lhsImainName in
         let !_tlOforcedIrrefutables = rule124 _lhsIforcedIrrefutables in
         let !_tlOmainName = rule126 _lhsImainName in
         let !(T_Nonterminal_vOut7 _hdIallAttributes _hdIaugmentsOut _hdIerrors _hdIinhMap' _hdIoutput _hdIsynMap') = inv_Nonterminal_s14 _hdX14 K_Nonterminal_v7 (T_Nonterminal_vIn7 _hdOaugmentsIn _hdOforcedIrrefutables _hdOinhMap _hdOmainName _hdOoptions _hdOsynMap) in
         let !(T_Nonterminals_vOut8 _tlIallAttributes _tlIaugmentsOut _tlIerrors _tlIinhMap' _tlIoutput _tlIsynMap') = inv_Nonterminals_s16 _tlX16 K_Nonterminals_v8 (T_Nonterminals_vIn8 _tlOaugmentsIn _tlOforcedIrrefutables _tlOinhMap _tlOmainName _tlOoptions _tlOsynMap) in
         let _lhsOallAttributes :: AttrMap
             !_lhsOallAttributes = rule110 _hdIallAttributes _tlIallAttributes in
         let _lhsOaugmentsOut :: Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))
             !_lhsOaugmentsOut = rule111 _hdIaugmentsOut _tlIaugmentsOut in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule112 _hdIerrors _tlIerrors in
         let _lhsOinhMap' :: Map Identifier Attributes
             !_lhsOinhMap' = rule113 _hdIinhMap' _tlIinhMap' in
         let !_output = rule115 _hdIoutput _tlIoutput in
         let _lhsOoutput :: Nonterminals
             !_lhsOoutput = rule116 _output in
         let _lhsOsynMap' :: Map Identifier Attributes
             !_lhsOsynMap' = rule114 _hdIsynMap' _tlIsynMap' in
         let !__result_ = T_Nonterminals_vOut8 _lhsOallAttributes _lhsOaugmentsOut _lhsOerrors _lhsOinhMap' _lhsOoutput _lhsOsynMap'
          in __result_ )
      v19 :: T_Nonterminals_v19 
      v19 = \ !(T_Nonterminals_vIn19 ) -> (
         let !_hdX14 = Control.Monad.Identity.runIdentity (attach_T_Nonterminal (arg_hd_)) in
         let !_tlX16 = Control.Monad.Identity.runIdentity (attach_T_Nonterminals (arg_tl_)) in
         let !(T_Nonterminal_vOut32 _hdIallAttributes _hdIinhMap' _hdIsynMap' _hdX46) = inv_Nonterminal_s14 _hdX14 K_Nonterminal_v32 (T_Nonterminal_vIn32 ) in
         let !(T_Nonterminals_vOut19 _tlIallAttributes _tlIinhMap' _tlIsynMap' _tlX36) = inv_Nonterminals_s16 _tlX16 K_Nonterminals_v19 (T_Nonterminals_vIn19 ) in
         let _lhsOallAttributes :: AttrMap
             !_lhsOallAttributes = rule110 _hdIallAttributes _tlIallAttributes in
         let _lhsOinhMap' :: Map Identifier Attributes
             !_lhsOinhMap' = rule113 _hdIinhMap' _tlIinhMap' in
         let _lhsOsynMap' :: Map Identifier Attributes
             !_lhsOsynMap' = rule114 _hdIsynMap' _tlIsynMap' in
         let !__st_ = st36 _hdX46 _tlX36
             !__result_ = T_Nonterminals_vOut19 _lhsOallAttributes _lhsOinhMap' _lhsOsynMap' __st_
          in __result_ )
     in C_Nonterminals_s16 k16
   {-# NOINLINE st36 #-}
   st36 = \ !_hdX46 !_tlX36 -> let
      v20 :: T_Nonterminals_v20 
      v20 = \ !(T_Nonterminals_vIn20 _lhsIaugmentsIn _lhsIforcedIrrefutables _lhsIinhMap _lhsImainName _lhsIoptions _lhsIsynMap) -> (
         let !_hdOaugmentsIn = rule117 _lhsIaugmentsIn in
         let !_hdOinhMap = rule119 _lhsIinhMap in
         let !_hdOoptions = rule121 _lhsIoptions in
         let !_hdOsynMap = rule122 _lhsIsynMap in
         let !_tlOaugmentsIn = rule123 _lhsIaugmentsIn in
         let !_tlOinhMap = rule125 _lhsIinhMap in
         let !_tlOoptions = rule127 _lhsIoptions in
         let !_tlOsynMap = rule128 _lhsIsynMap in
         let !_hdOforcedIrrefutables = rule118 _lhsIforcedIrrefutables in
         let !_hdOmainName = rule120 _lhsImainName in
         let !_tlOforcedIrrefutables = rule124 _lhsIforcedIrrefutables in
         let !_tlOmainName = rule126 _lhsImainName in
         let !(T_Nonterminal_vOut33 _hdIaugmentsOut _hdIerrors _hdIoutput) = inv_Nonterminal_s46 _hdX46 (T_Nonterminal_vIn33 _hdOaugmentsIn _hdOforcedIrrefutables _hdOinhMap _hdOmainName _hdOoptions _hdOsynMap) in
         let !(T_Nonterminals_vOut20 _tlIaugmentsOut _tlIerrors _tlIoutput) = inv_Nonterminals_s36 _tlX36 (T_Nonterminals_vIn20 _tlOaugmentsIn _tlOforcedIrrefutables _tlOinhMap _tlOmainName _tlOoptions _tlOsynMap) in
         let _lhsOaugmentsOut :: Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))
             !_lhsOaugmentsOut = rule111 _hdIaugmentsOut _tlIaugmentsOut in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule112 _hdIerrors _tlIerrors in
         let !_output = rule115 _hdIoutput _tlIoutput in
         let _lhsOoutput :: Nonterminals
             !_lhsOoutput = rule116 _output in
         let !__result_ = T_Nonterminals_vOut20 _lhsOaugmentsOut _lhsOerrors _lhsOoutput
          in __result_ )
     in C_Nonterminals_s36 v20
   {-# NOINLINE[1] rule110 #-}
   rule110 = \ ((!_hdIallAttributes) :: AttrMap) ((!_tlIallAttributes) :: AttrMap) ->
     _hdIallAttributes `mergeAttributes` _tlIallAttributes
   {-# NOINLINE[1] rule111 #-}
   rule111 = \ ((!_hdIaugmentsOut) :: Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) ((!_tlIaugmentsOut) :: Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) ->
     _hdIaugmentsOut `Map.union` _tlIaugmentsOut
   {-# NOINLINE[1] rule112 #-}
   rule112 = \ ((!_hdIerrors) :: Seq Error) ((!_tlIerrors) :: Seq Error) ->
     _hdIerrors Seq.>< _tlIerrors
   {-# NOINLINE[1] rule113 #-}
   rule113 = \ ((!_hdIinhMap') :: Map Identifier Attributes) ((!_tlIinhMap') :: Map Identifier Attributes) ->
     _hdIinhMap' `Map.union` _tlIinhMap'
   {-# NOINLINE[1] rule114 #-}
   rule114 = \ ((!_hdIsynMap') :: Map Identifier Attributes) ((!_tlIsynMap') :: Map Identifier Attributes) ->
     _hdIsynMap' `Map.union` _tlIsynMap'
   {-# NOINLINE[1] rule115 #-}
   rule115 = \ ((!_hdIoutput) :: Nonterminal) ((!_tlIoutput) :: Nonterminals) ->
     (:) _hdIoutput _tlIoutput
   {-# NOINLINE[1] rule116 #-}
   rule116 = \ !_output ->
     _output
   {-# NOINLINE[1] rule117 #-}
   rule117 = \ ((!_lhsIaugmentsIn) :: Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) ->
     _lhsIaugmentsIn
   {-# NOINLINE[1] rule118 #-}
   rule118 = \ ((!_lhsIforcedIrrefutables) :: AttrMap) ->
     _lhsIforcedIrrefutables
   {-# NOINLINE[1] rule119 #-}
   rule119 = \ ((!_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# NOINLINE[1] rule120 #-}
   rule120 = \ ((!_lhsImainName) :: String) ->
     _lhsImainName
   {-# NOINLINE[1] rule121 #-}
   rule121 = \ ((!_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# NOINLINE[1] rule122 #-}
   rule122 = \ ((!_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
   {-# NOINLINE[1] rule123 #-}
   rule123 = \ ((!_lhsIaugmentsIn) :: Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) ->
     _lhsIaugmentsIn
   {-# NOINLINE[1] rule124 #-}
   rule124 = \ ((!_lhsIforcedIrrefutables) :: AttrMap) ->
     _lhsIforcedIrrefutables
   {-# NOINLINE[1] rule125 #-}
   rule125 = \ ((!_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# NOINLINE[1] rule126 #-}
   rule126 = \ ((!_lhsImainName) :: String) ->
     _lhsImainName
   {-# NOINLINE[1] rule127 #-}
   rule127 = \ ((!_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# NOINLINE[1] rule128 #-}
   rule128 = \ ((!_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
{-# NOINLINE sem_Nonterminals_Nil #-}
sem_Nonterminals_Nil ::  T_Nonterminals 
sem_Nonterminals_Nil  = T_Nonterminals (return st16) where
   {-# NOINLINE st16 #-}
   !st16 = let
      k16 :: K_Nonterminals_s16  t -> t
      k16 K_Nonterminals_v8 = v8
      k16 K_Nonterminals_v19 = v19
      v8 :: T_Nonterminals_v8 
      v8 = \ !(T_Nonterminals_vIn8 _lhsIaugmentsIn _lhsIforcedIrrefutables _lhsIinhMap _lhsImainName _lhsIoptions _lhsIsynMap) -> (
         let _lhsOallAttributes :: AttrMap
             !_lhsOallAttributes = rule129  () in
         let _lhsOaugmentsOut :: Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))
             !_lhsOaugmentsOut = rule130  () in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule131  () in
         let _lhsOinhMap' :: Map Identifier Attributes
             !_lhsOinhMap' = rule132  () in
         let !_output = rule134  () in
         let _lhsOsynMap' :: Map Identifier Attributes
             !_lhsOsynMap' = rule133  () in
         let _lhsOoutput :: Nonterminals
             !_lhsOoutput = rule135 _output in
         let !__result_ = T_Nonterminals_vOut8 _lhsOallAttributes _lhsOaugmentsOut _lhsOerrors _lhsOinhMap' _lhsOoutput _lhsOsynMap'
          in __result_ )
      v19 :: T_Nonterminals_v19 
      v19 = \ !(T_Nonterminals_vIn19 ) -> (
         let _lhsOallAttributes :: AttrMap
             !_lhsOallAttributes = rule129  () in
         let _lhsOinhMap' :: Map Identifier Attributes
             !_lhsOinhMap' = rule132  () in
         let _lhsOsynMap' :: Map Identifier Attributes
             !_lhsOsynMap' = rule133  () in
         let !__st_ = st36  ()
             !__result_ = T_Nonterminals_vOut19 _lhsOallAttributes _lhsOinhMap' _lhsOsynMap' __st_
          in __result_ )
     in C_Nonterminals_s16 k16
   {-# NOINLINE st36 #-}
   st36 = \  (_ :: ()) -> let
      v20 :: T_Nonterminals_v20 
      v20 = \ !(T_Nonterminals_vIn20 _lhsIaugmentsIn _lhsIforcedIrrefutables _lhsIinhMap _lhsImainName _lhsIoptions _lhsIsynMap) -> (
         let _lhsOaugmentsOut :: Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))
             !_lhsOaugmentsOut = rule130  () in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule131  () in
         let !_output = rule134  () in
         let _lhsOoutput :: Nonterminals
             !_lhsOoutput = rule135 _output in
         let !__result_ = T_Nonterminals_vOut20 _lhsOaugmentsOut _lhsOerrors _lhsOoutput
          in __result_ )
     in C_Nonterminals_s36 v20
   {-# NOINLINE[1] rule129 #-}
   rule129 = \  (_ :: ()) ->
     Map.empty
   {-# NOINLINE[1] rule130 #-}
   rule130 = \  (_ :: ()) ->
     Map.empty
   {-# NOINLINE[1] rule131 #-}
   rule131 = \  (_ :: ()) ->
     Seq.empty
   {-# NOINLINE[1] rule132 #-}
   rule132 = \  (_ :: ()) ->
     Map.empty
   {-# NOINLINE[1] rule133 #-}
   rule133 = \  (_ :: ()) ->
     Map.empty
   {-# NOINLINE[1] rule134 #-}
   rule134 = \  (_ :: ()) ->
     []
   {-# NOINLINE[1] rule135 #-}
   rule135 = \ !_output ->
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
        let arg = T_Pattern_vIn9 _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt
        !(T_Pattern_vOut9 _lhsOallAttributes _lhsOcopy _lhsOdefsCollect _lhsOerrors _lhsOoutput) <- return (inv_Pattern_s18 sem K_Pattern_v9 arg)
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
                               attach_T_Pattern :: Identity (T_Pattern_s18 )
                               }
data T_Pattern_s18  where C_Pattern_s18 :: {
                                           inv_Pattern_s18 :: !(forall t. K_Pattern_s18  t -> t)
                                           } -> T_Pattern_s18 
data T_Pattern_s19  = C_Pattern_s19
data T_Pattern_s42  = C_Pattern_s42
data T_Pattern_s45  = C_Pattern_s45
newtype T_Pattern_s51  = C_Pattern_s51 {
                                       inv_Pattern_s51 :: (T_Pattern_v42 )
                                       }
newtype T_Pattern_s54  = C_Pattern_s54 {
                                       inv_Pattern_s54 :: (T_Pattern_v48 )
                                       }
newtype T_Pattern_s58  = C_Pattern_s58 {
                                       inv_Pattern_s58 :: (T_Pattern_v56 )
                                       }
data K_Pattern_s18 k  where
   K_Pattern_v9 :: K_Pattern_s18  (T_Pattern_v9 )
   K_Pattern_v27 :: K_Pattern_s18  (T_Pattern_v27 )
   K_Pattern_v31 :: K_Pattern_s18  (T_Pattern_v31 )
   K_Pattern_v41 :: K_Pattern_s18  (T_Pattern_v41 )
   K_Pattern_v46 :: K_Pattern_s18  (T_Pattern_v46 )
   K_Pattern_v55 :: K_Pattern_s18  (T_Pattern_v55 )
type T_Pattern_v9  = (T_Pattern_vIn9 ) -> (T_Pattern_vOut9 )
data T_Pattern_vIn9  = T_Pattern_vIn9 !([(Identifier, Identifier)]) !([(Identifier, Identifier)]) !(ConstructorIdent) !(Set (Identifier, Identifier)) !(AttrMap) !(NontermIdent)
data T_Pattern_vOut9  = T_Pattern_vOut9 !(AttrMap) !(Pattern) !(Set (Identifier, Identifier)) !(Seq Error) !(Pattern)
type T_Pattern_v27  = (T_Pattern_vIn27 ) -> (T_Pattern_vOut27 )
data T_Pattern_vIn27  = T_Pattern_vIn27 !([(Identifier, Identifier)]) !(ConstructorIdent) !(Set (Identifier, Identifier)) !(AttrMap) !(NontermIdent)
data T_Pattern_vOut27  = T_Pattern_vOut27 !(AttrMap) !(Pattern) !(Set (Identifier, Identifier)) !(Seq Error) !(Pattern)
type T_Pattern_v31  = (T_Pattern_vIn31 ) -> (T_Pattern_vOut31 )
data T_Pattern_vIn31  = T_Pattern_vIn31 !([(Identifier, Identifier)]) !(ConstructorIdent) !(Set (Identifier, Identifier)) !(AttrMap) !(NontermIdent)
data T_Pattern_vOut31  = T_Pattern_vOut31 !(AttrMap) !(Set (Identifier, Identifier)) !(Seq Error) !(Pattern)
type T_Pattern_v41  = (T_Pattern_vIn41 ) -> (T_Pattern_vOut41 )
data T_Pattern_vIn41  = T_Pattern_vIn41 !(ConstructorIdent) !(NontermIdent)
data T_Pattern_vOut41  = T_Pattern_vOut41 !(AttrMap) !(Set (Identifier, Identifier)) !(T_Pattern_s51 )
type T_Pattern_v42  = (T_Pattern_vIn42 ) -> (T_Pattern_vOut42 )
data T_Pattern_vIn42  = T_Pattern_vIn42 !([(Identifier, Identifier)]) !(Set (Identifier, Identifier)) !(AttrMap)
data T_Pattern_vOut42  = T_Pattern_vOut42 !(Seq Error) !(Pattern)
type T_Pattern_v46  = (T_Pattern_vIn46 ) -> (T_Pattern_vOut46 )
data T_Pattern_vIn46  = T_Pattern_vIn46 
data T_Pattern_vOut46  = T_Pattern_vOut46 !(Set (Identifier, Identifier)) !(T_Pattern_s54 )
type T_Pattern_v48  = (T_Pattern_vIn48 ) -> (T_Pattern_vOut48 )
data T_Pattern_vIn48  = T_Pattern_vIn48 !([(Identifier, Identifier)]) !(ConstructorIdent) !(Set (Identifier, Identifier)) !(AttrMap) !(NontermIdent)
data T_Pattern_vOut48  = T_Pattern_vOut48 !(AttrMap) !(Seq Error) !(Pattern)
type T_Pattern_v55  = (T_Pattern_vIn55 ) -> (T_Pattern_vOut55 )
data T_Pattern_vIn55  = T_Pattern_vIn55 !(ConstructorIdent) !(NontermIdent)
data T_Pattern_vOut55  = T_Pattern_vOut55 !(AttrMap) !(T_Pattern_s58 )
type T_Pattern_v56  = (T_Pattern_vIn56 ) -> (T_Pattern_vOut56 )
data T_Pattern_vIn56  = T_Pattern_vIn56 
data T_Pattern_vOut56  = T_Pattern_vOut56 !(Set (Identifier, Identifier)) !(T_Pattern_s51 )
{-# NOINLINE sem_Pattern_Constr #-}
sem_Pattern_Constr :: (ConstructorIdent) -> T_Patterns  -> T_Pattern 
sem_Pattern_Constr !arg_name_ arg_pats_ = T_Pattern (return st18) where
   {-# NOINLINE st18 #-}
   !st18 = let
      k18 :: K_Pattern_s18  t -> t
      k18 K_Pattern_v9 = v9
      k18 K_Pattern_v27 = v27
      k18 K_Pattern_v31 = v31
      k18 K_Pattern_v41 = v41
      k18 K_Pattern_v46 = v46
      k18 K_Pattern_v55 = v55
      v9 :: T_Pattern_v9 
      v9 = \ !(T_Pattern_vIn9 _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt) -> (
         let !_patsX20 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_pats_)) in
         let !_patsOcon = rule145 _lhsIcon in
         let !_patsOnt = rule148 _lhsInt in
         let !_patsOchildInhs = rule143 _lhsIchildInhs in
         let !_patsOdefs = rule146 _lhsIdefs in
         let !_patsOforcedIrrefutables = rule147 _lhsIforcedIrrefutables in
         let !(T_Patterns_vOut26 _patsIallAttributes _patsIcopy _patsIdefsCollect _patsIerrors _patsIoutput) = inv_Patterns_s20 _patsX20 K_Patterns_v26 (T_Patterns_vIn26 _patsOchildInhs _patsOcon _patsOdefs _patsOforcedIrrefutables _patsOnt) in
         let _lhsOallAttributes :: AttrMap
             !_lhsOallAttributes = rule136 _patsIallAttributes in
         let !_copy = rule139 _patsIcopy arg_name_ in
         let _lhsOcopy :: Pattern
             !_lhsOcopy = rule141 _copy in
         let _lhsOdefsCollect :: Set (Identifier, Identifier)
             !_lhsOdefsCollect = rule137 _patsIdefsCollect in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule138 _patsIerrors in
         let !_output = rule140 _patsIoutput arg_name_ in
         let _lhsOoutput :: Pattern
             !_lhsOoutput = rule142 _output in
         let !__result_ = T_Pattern_vOut9 _lhsOallAttributes _lhsOcopy _lhsOdefsCollect _lhsOerrors _lhsOoutput
          in __result_ )
      v27 :: T_Pattern_v27 
      v27 = \ !(T_Pattern_vIn27 _lhsIchildInhs _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt) -> (
         let !_patsX20 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_pats_)) in
         let !_patsOcon = rule145 _lhsIcon in
         let !_patsOnt = rule148 _lhsInt in
         let !_patsOchildInhs = rule143 _lhsIchildInhs in
         let !_patsOdefs = rule146 _lhsIdefs in
         let !_patsOforcedIrrefutables = rule147 _lhsIforcedIrrefutables in
         let !(T_Patterns_vOut26 _patsIallAttributes _patsIcopy _patsIdefsCollect _patsIerrors _patsIoutput) = inv_Patterns_s20 _patsX20 K_Patterns_v26 (T_Patterns_vIn26 _patsOchildInhs _patsOcon _patsOdefs _patsOforcedIrrefutables _patsOnt) in
         let _lhsOallAttributes :: AttrMap
             !_lhsOallAttributes = rule136 _patsIallAttributes in
         let !_copy = rule139 _patsIcopy arg_name_ in
         let _lhsOcopy :: Pattern
             !_lhsOcopy = rule141 _copy in
         let _lhsOdefsCollect :: Set (Identifier, Identifier)
             !_lhsOdefsCollect = rule137 _patsIdefsCollect in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule138 _patsIerrors in
         let !_output = rule140 _patsIoutput arg_name_ in
         let _lhsOoutput :: Pattern
             !_lhsOoutput = rule142 _output in
         let !__result_ = T_Pattern_vOut27 _lhsOallAttributes _lhsOcopy _lhsOdefsCollect _lhsOerrors _lhsOoutput
          in __result_ )
      v31 :: T_Pattern_v31 
      v31 = \ !(T_Pattern_vIn31 _lhsIchildInhs _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt) -> (
         let !_patsX20 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_pats_)) in
         let !_patsOcon = rule145 _lhsIcon in
         let !_patsOnt = rule148 _lhsInt in
         let !_patsOchildInhs = rule143 _lhsIchildInhs in
         let !_patsOdefs = rule146 _lhsIdefs in
         let !_patsOforcedIrrefutables = rule147 _lhsIforcedIrrefutables in
         let !(T_Patterns_vOut38 _patsIallAttributes _patsIdefsCollect _patsIerrors _patsIoutput) = inv_Patterns_s20 _patsX20 K_Patterns_v38 (T_Patterns_vIn38 _patsOchildInhs _patsOcon _patsOdefs _patsOforcedIrrefutables _patsOnt) in
         let _lhsOallAttributes :: AttrMap
             !_lhsOallAttributes = rule136 _patsIallAttributes in
         let _lhsOdefsCollect :: Set (Identifier, Identifier)
             !_lhsOdefsCollect = rule137 _patsIdefsCollect in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule138 _patsIerrors in
         let !_output = rule140 _patsIoutput arg_name_ in
         let _lhsOoutput :: Pattern
             !_lhsOoutput = rule142 _output in
         let !__result_ = T_Pattern_vOut31 _lhsOallAttributes _lhsOdefsCollect _lhsOerrors _lhsOoutput
          in __result_ )
      v41 :: T_Pattern_v41 
      v41 = \ !(T_Pattern_vIn41 _lhsIcon _lhsInt) -> (
         let !_patsX20 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_pats_)) in
         let !_patsOcon = rule145 _lhsIcon in
         let !_patsOnt = rule148 _lhsInt in
         let !(T_Patterns_vOut45 _patsIallAttributes _patsIdefsCollect _patsX53) = inv_Patterns_s20 _patsX20 K_Patterns_v45 (T_Patterns_vIn45 _patsOcon _patsOnt) in
         let _lhsOallAttributes :: AttrMap
             !_lhsOallAttributes = rule136 _patsIallAttributes in
         let _lhsOdefsCollect :: Set (Identifier, Identifier)
             !_lhsOdefsCollect = rule137 _patsIdefsCollect in
         let !__st_ = st51 _patsX53
             !__result_ = T_Pattern_vOut41 _lhsOallAttributes _lhsOdefsCollect __st_
          in __result_ )
      v46 :: T_Pattern_v46 
      v46 = \ !(T_Pattern_vIn46 ) -> (
         let !_patsX20 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_pats_)) in
         let !(T_Patterns_vOut51 _patsIdefsCollect _patsX56) = inv_Patterns_s20 _patsX20 K_Patterns_v51 (T_Patterns_vIn51 ) in
         let _lhsOdefsCollect :: Set (Identifier, Identifier)
             !_lhsOdefsCollect = rule137 _patsIdefsCollect in
         let !__st_ = st54 _patsX56
             !__result_ = T_Pattern_vOut46 _lhsOdefsCollect __st_
          in __result_ )
      v55 :: T_Pattern_v55 
      v55 = \ !(T_Pattern_vIn55 _lhsIcon _lhsInt) -> (
         let !_patsX20 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_pats_)) in
         let !_patsOcon = rule145 _lhsIcon in
         let !_patsOnt = rule148 _lhsInt in
         let !(T_Patterns_vOut57 _patsIallAttributes _patsX59) = inv_Patterns_s20 _patsX20 K_Patterns_v57 (T_Patterns_vIn57 _patsOcon _patsOnt) in
         let _lhsOallAttributes :: AttrMap
             !_lhsOallAttributes = rule136 _patsIallAttributes in
         let !__st_ = st58 _patsX59
             !__result_ = T_Pattern_vOut55 _lhsOallAttributes __st_
          in __result_ )
     in C_Pattern_s18 k18
   {-# NOINLINE st51 #-}
   st51 = \ !_patsX53 -> let
      v42 :: T_Pattern_v42 
      v42 = \ !(T_Pattern_vIn42 _lhsIchildInhs _lhsIdefs _lhsIforcedIrrefutables) -> (
         let !_patsOchildInhs = rule143 _lhsIchildInhs in
         let !_patsOdefs = rule146 _lhsIdefs in
         let !_patsOforcedIrrefutables = rule147 _lhsIforcedIrrefutables in
         let !(T_Patterns_vOut47 _patsIerrors _patsIoutput) = inv_Patterns_s53 _patsX53 (T_Patterns_vIn47 _patsOchildInhs _patsOdefs _patsOforcedIrrefutables) in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule138 _patsIerrors in
         let !_output = rule140 _patsIoutput arg_name_ in
         let _lhsOoutput :: Pattern
             !_lhsOoutput = rule142 _output in
         let !__result_ = T_Pattern_vOut42 _lhsOerrors _lhsOoutput
          in __result_ )
     in C_Pattern_s51 v42
   {-# NOINLINE st54 #-}
   st54 = \ !_patsX56 -> let
      v48 :: T_Pattern_v48 
      v48 = \ !(T_Pattern_vIn48 _lhsIchildInhs _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt) -> (
         let !_patsOcon = rule145 _lhsIcon in
         let !_patsOnt = rule148 _lhsInt in
         let !_patsOchildInhs = rule143 _lhsIchildInhs in
         let !_patsOdefs = rule146 _lhsIdefs in
         let !_patsOforcedIrrefutables = rule147 _lhsIforcedIrrefutables in
         let !(T_Patterns_vOut52 _patsIallAttributes _patsIerrors _patsIoutput) = inv_Patterns_s56 _patsX56 (T_Patterns_vIn52 _patsOchildInhs _patsOcon _patsOdefs _patsOforcedIrrefutables _patsOnt) in
         let _lhsOallAttributes :: AttrMap
             !_lhsOallAttributes = rule136 _patsIallAttributes in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule138 _patsIerrors in
         let !_output = rule140 _patsIoutput arg_name_ in
         let _lhsOoutput :: Pattern
             !_lhsOoutput = rule142 _output in
         let !__result_ = T_Pattern_vOut48 _lhsOallAttributes _lhsOerrors _lhsOoutput
          in __result_ )
     in C_Pattern_s54 v48
   {-# NOINLINE st58 #-}
   st58 = \ !_patsX59 -> let
      v56 :: T_Pattern_v56 
      v56 = \ !(T_Pattern_vIn56 ) -> (
         let !(T_Patterns_vOut58 _patsIdefsCollect _patsX53) = inv_Patterns_s59 _patsX59 (T_Patterns_vIn58 ) in
         let _lhsOdefsCollect :: Set (Identifier, Identifier)
             !_lhsOdefsCollect = rule137 _patsIdefsCollect in
         let !__st_ = st51 _patsX53
             !__result_ = T_Pattern_vOut56 _lhsOdefsCollect __st_
          in __result_ )
     in C_Pattern_s58 v56
   {-# NOINLINE[1] rule136 #-}
   rule136 = \ ((!_patsIallAttributes) :: AttrMap) ->
     _patsIallAttributes
   {-# NOINLINE[1] rule137 #-}
   rule137 = \ ((!_patsIdefsCollect) :: Set (Identifier, Identifier)) ->
     _patsIdefsCollect
   {-# NOINLINE[1] rule138 #-}
   rule138 = \ ((!_patsIerrors) :: Seq Error) ->
     _patsIerrors
   {-# NOINLINE[1] rule139 #-}
   rule139 = \ ((!_patsIcopy) :: Patterns) !name_ ->
     Constr name_ _patsIcopy
   {-# NOINLINE[1] rule140 #-}
   rule140 = \ ((!_patsIoutput) :: Patterns) !name_ ->
     Constr name_ _patsIoutput
   {-# NOINLINE[1] rule141 #-}
   rule141 = \ !_copy ->
     _copy
   {-# NOINLINE[1] rule142 #-}
   rule142 = \ !_output ->
     _output
   {-# NOINLINE[1] rule143 #-}
   rule143 = \ ((!_lhsIchildInhs) :: [(Identifier, Identifier)]) ->
     _lhsIchildInhs
   {-# NOINLINE[1] rule145 #-}
   rule145 = \ ((!_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# NOINLINE[1] rule146 #-}
   rule146 = \ ((!_lhsIdefs) :: Set (Identifier, Identifier)) ->
     _lhsIdefs
   {-# NOINLINE[1] rule147 #-}
   rule147 = \ ((!_lhsIforcedIrrefutables) :: AttrMap) ->
     _lhsIforcedIrrefutables
   {-# NOINLINE[1] rule148 #-}
   rule148 = \ ((!_lhsInt) :: NontermIdent) ->
     _lhsInt
{-# NOINLINE sem_Pattern_Product #-}
sem_Pattern_Product :: (Pos) -> T_Patterns  -> T_Pattern 
sem_Pattern_Product !arg_pos_ arg_pats_ = T_Pattern (return st18) where
   {-# NOINLINE st18 #-}
   !st18 = let
      k18 :: K_Pattern_s18  t -> t
      k18 K_Pattern_v9 = v9
      k18 K_Pattern_v27 = v27
      k18 K_Pattern_v31 = v31
      k18 K_Pattern_v41 = v41
      k18 K_Pattern_v46 = v46
      k18 K_Pattern_v55 = v55
      v9 :: T_Pattern_v9 
      v9 = \ !(T_Pattern_vIn9 _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt) -> (
         let !_patsX20 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_pats_)) in
         let !_patsOcon = rule158 _lhsIcon in
         let !_patsOnt = rule161 _lhsInt in
         let !_patsOchildInhs = rule156 _lhsIchildInhs in
         let !_patsOdefs = rule159 _lhsIdefs in
         let !_patsOforcedIrrefutables = rule160 _lhsIforcedIrrefutables in
         let !(T_Patterns_vOut26 _patsIallAttributes _patsIcopy _patsIdefsCollect _patsIerrors _patsIoutput) = inv_Patterns_s20 _patsX20 K_Patterns_v26 (T_Patterns_vIn26 _patsOchildInhs _patsOcon _patsOdefs _patsOforcedIrrefutables _patsOnt) in
         let _lhsOallAttributes :: AttrMap
             !_lhsOallAttributes = rule149 _patsIallAttributes in
         let !_copy = rule152 _patsIcopy arg_pos_ in
         let _lhsOcopy :: Pattern
             !_lhsOcopy = rule154 _copy in
         let _lhsOdefsCollect :: Set (Identifier, Identifier)
             !_lhsOdefsCollect = rule150 _patsIdefsCollect in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule151 _patsIerrors in
         let !_output = rule153 _patsIoutput arg_pos_ in
         let _lhsOoutput :: Pattern
             !_lhsOoutput = rule155 _output in
         let !__result_ = T_Pattern_vOut9 _lhsOallAttributes _lhsOcopy _lhsOdefsCollect _lhsOerrors _lhsOoutput
          in __result_ )
      v27 :: T_Pattern_v27 
      v27 = \ !(T_Pattern_vIn27 _lhsIchildInhs _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt) -> (
         let !_patsX20 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_pats_)) in
         let !_patsOcon = rule158 _lhsIcon in
         let !_patsOnt = rule161 _lhsInt in
         let !_patsOchildInhs = rule156 _lhsIchildInhs in
         let !_patsOdefs = rule159 _lhsIdefs in
         let !_patsOforcedIrrefutables = rule160 _lhsIforcedIrrefutables in
         let !(T_Patterns_vOut26 _patsIallAttributes _patsIcopy _patsIdefsCollect _patsIerrors _patsIoutput) = inv_Patterns_s20 _patsX20 K_Patterns_v26 (T_Patterns_vIn26 _patsOchildInhs _patsOcon _patsOdefs _patsOforcedIrrefutables _patsOnt) in
         let _lhsOallAttributes :: AttrMap
             !_lhsOallAttributes = rule149 _patsIallAttributes in
         let !_copy = rule152 _patsIcopy arg_pos_ in
         let _lhsOcopy :: Pattern
             !_lhsOcopy = rule154 _copy in
         let _lhsOdefsCollect :: Set (Identifier, Identifier)
             !_lhsOdefsCollect = rule150 _patsIdefsCollect in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule151 _patsIerrors in
         let !_output = rule153 _patsIoutput arg_pos_ in
         let _lhsOoutput :: Pattern
             !_lhsOoutput = rule155 _output in
         let !__result_ = T_Pattern_vOut27 _lhsOallAttributes _lhsOcopy _lhsOdefsCollect _lhsOerrors _lhsOoutput
          in __result_ )
      v31 :: T_Pattern_v31 
      v31 = \ !(T_Pattern_vIn31 _lhsIchildInhs _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt) -> (
         let !_patsX20 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_pats_)) in
         let !_patsOcon = rule158 _lhsIcon in
         let !_patsOnt = rule161 _lhsInt in
         let !_patsOchildInhs = rule156 _lhsIchildInhs in
         let !_patsOdefs = rule159 _lhsIdefs in
         let !_patsOforcedIrrefutables = rule160 _lhsIforcedIrrefutables in
         let !(T_Patterns_vOut38 _patsIallAttributes _patsIdefsCollect _patsIerrors _patsIoutput) = inv_Patterns_s20 _patsX20 K_Patterns_v38 (T_Patterns_vIn38 _patsOchildInhs _patsOcon _patsOdefs _patsOforcedIrrefutables _patsOnt) in
         let _lhsOallAttributes :: AttrMap
             !_lhsOallAttributes = rule149 _patsIallAttributes in
         let _lhsOdefsCollect :: Set (Identifier, Identifier)
             !_lhsOdefsCollect = rule150 _patsIdefsCollect in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule151 _patsIerrors in
         let !_output = rule153 _patsIoutput arg_pos_ in
         let _lhsOoutput :: Pattern
             !_lhsOoutput = rule155 _output in
         let !__result_ = T_Pattern_vOut31 _lhsOallAttributes _lhsOdefsCollect _lhsOerrors _lhsOoutput
          in __result_ )
      v41 :: T_Pattern_v41 
      v41 = \ !(T_Pattern_vIn41 _lhsIcon _lhsInt) -> (
         let !_patsX20 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_pats_)) in
         let !_patsOcon = rule158 _lhsIcon in
         let !_patsOnt = rule161 _lhsInt in
         let !(T_Patterns_vOut45 _patsIallAttributes _patsIdefsCollect _patsX53) = inv_Patterns_s20 _patsX20 K_Patterns_v45 (T_Patterns_vIn45 _patsOcon _patsOnt) in
         let _lhsOallAttributes :: AttrMap
             !_lhsOallAttributes = rule149 _patsIallAttributes in
         let _lhsOdefsCollect :: Set (Identifier, Identifier)
             !_lhsOdefsCollect = rule150 _patsIdefsCollect in
         let !__st_ = st51 _patsX53
             !__result_ = T_Pattern_vOut41 _lhsOallAttributes _lhsOdefsCollect __st_
          in __result_ )
      v46 :: T_Pattern_v46 
      v46 = \ !(T_Pattern_vIn46 ) -> (
         let !_patsX20 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_pats_)) in
         let !(T_Patterns_vOut51 _patsIdefsCollect _patsX56) = inv_Patterns_s20 _patsX20 K_Patterns_v51 (T_Patterns_vIn51 ) in
         let _lhsOdefsCollect :: Set (Identifier, Identifier)
             !_lhsOdefsCollect = rule150 _patsIdefsCollect in
         let !__st_ = st54 _patsX56
             !__result_ = T_Pattern_vOut46 _lhsOdefsCollect __st_
          in __result_ )
      v55 :: T_Pattern_v55 
      v55 = \ !(T_Pattern_vIn55 _lhsIcon _lhsInt) -> (
         let !_patsX20 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_pats_)) in
         let !_patsOcon = rule158 _lhsIcon in
         let !_patsOnt = rule161 _lhsInt in
         let !(T_Patterns_vOut57 _patsIallAttributes _patsX59) = inv_Patterns_s20 _patsX20 K_Patterns_v57 (T_Patterns_vIn57 _patsOcon _patsOnt) in
         let _lhsOallAttributes :: AttrMap
             !_lhsOallAttributes = rule149 _patsIallAttributes in
         let !__st_ = st58 _patsX59
             !__result_ = T_Pattern_vOut55 _lhsOallAttributes __st_
          in __result_ )
     in C_Pattern_s18 k18
   {-# NOINLINE st51 #-}
   st51 = \ !_patsX53 -> let
      v42 :: T_Pattern_v42 
      v42 = \ !(T_Pattern_vIn42 _lhsIchildInhs _lhsIdefs _lhsIforcedIrrefutables) -> (
         let !_patsOchildInhs = rule156 _lhsIchildInhs in
         let !_patsOdefs = rule159 _lhsIdefs in
         let !_patsOforcedIrrefutables = rule160 _lhsIforcedIrrefutables in
         let !(T_Patterns_vOut47 _patsIerrors _patsIoutput) = inv_Patterns_s53 _patsX53 (T_Patterns_vIn47 _patsOchildInhs _patsOdefs _patsOforcedIrrefutables) in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule151 _patsIerrors in
         let !_output = rule153 _patsIoutput arg_pos_ in
         let _lhsOoutput :: Pattern
             !_lhsOoutput = rule155 _output in
         let !__result_ = T_Pattern_vOut42 _lhsOerrors _lhsOoutput
          in __result_ )
     in C_Pattern_s51 v42
   {-# NOINLINE st54 #-}
   st54 = \ !_patsX56 -> let
      v48 :: T_Pattern_v48 
      v48 = \ !(T_Pattern_vIn48 _lhsIchildInhs _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt) -> (
         let !_patsOcon = rule158 _lhsIcon in
         let !_patsOnt = rule161 _lhsInt in
         let !_patsOchildInhs = rule156 _lhsIchildInhs in
         let !_patsOdefs = rule159 _lhsIdefs in
         let !_patsOforcedIrrefutables = rule160 _lhsIforcedIrrefutables in
         let !(T_Patterns_vOut52 _patsIallAttributes _patsIerrors _patsIoutput) = inv_Patterns_s56 _patsX56 (T_Patterns_vIn52 _patsOchildInhs _patsOcon _patsOdefs _patsOforcedIrrefutables _patsOnt) in
         let _lhsOallAttributes :: AttrMap
             !_lhsOallAttributes = rule149 _patsIallAttributes in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule151 _patsIerrors in
         let !_output = rule153 _patsIoutput arg_pos_ in
         let _lhsOoutput :: Pattern
             !_lhsOoutput = rule155 _output in
         let !__result_ = T_Pattern_vOut48 _lhsOallAttributes _lhsOerrors _lhsOoutput
          in __result_ )
     in C_Pattern_s54 v48
   {-# NOINLINE st58 #-}
   st58 = \ !_patsX59 -> let
      v56 :: T_Pattern_v56 
      v56 = \ !(T_Pattern_vIn56 ) -> (
         let !(T_Patterns_vOut58 _patsIdefsCollect _patsX53) = inv_Patterns_s59 _patsX59 (T_Patterns_vIn58 ) in
         let _lhsOdefsCollect :: Set (Identifier, Identifier)
             !_lhsOdefsCollect = rule150 _patsIdefsCollect in
         let !__st_ = st51 _patsX53
             !__result_ = T_Pattern_vOut56 _lhsOdefsCollect __st_
          in __result_ )
     in C_Pattern_s58 v56
   {-# NOINLINE[1] rule149 #-}
   rule149 = \ ((!_patsIallAttributes) :: AttrMap) ->
     _patsIallAttributes
   {-# NOINLINE[1] rule150 #-}
   rule150 = \ ((!_patsIdefsCollect) :: Set (Identifier, Identifier)) ->
     _patsIdefsCollect
   {-# NOINLINE[1] rule151 #-}
   rule151 = \ ((!_patsIerrors) :: Seq Error) ->
     _patsIerrors
   {-# NOINLINE[1] rule152 #-}
   rule152 = \ ((!_patsIcopy) :: Patterns) !pos_ ->
     Product pos_ _patsIcopy
   {-# NOINLINE[1] rule153 #-}
   rule153 = \ ((!_patsIoutput) :: Patterns) !pos_ ->
     Product pos_ _patsIoutput
   {-# NOINLINE[1] rule154 #-}
   rule154 = \ !_copy ->
     _copy
   {-# NOINLINE[1] rule155 #-}
   rule155 = \ !_output ->
     _output
   {-# NOINLINE[1] rule156 #-}
   rule156 = \ ((!_lhsIchildInhs) :: [(Identifier, Identifier)]) ->
     _lhsIchildInhs
   {-# NOINLINE[1] rule158 #-}
   rule158 = \ ((!_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# NOINLINE[1] rule159 #-}
   rule159 = \ ((!_lhsIdefs) :: Set (Identifier, Identifier)) ->
     _lhsIdefs
   {-# NOINLINE[1] rule160 #-}
   rule160 = \ ((!_lhsIforcedIrrefutables) :: AttrMap) ->
     _lhsIforcedIrrefutables
   {-# NOINLINE[1] rule161 #-}
   rule161 = \ ((!_lhsInt) :: NontermIdent) ->
     _lhsInt
{-# NOINLINE sem_Pattern_Alias #-}
sem_Pattern_Alias :: (Identifier) -> (Identifier) -> T_Pattern  -> T_Pattern 
sem_Pattern_Alias !arg_field_ !arg_attr_ arg_pat_ = T_Pattern (return st18) where
   {-# NOINLINE st18 #-}
   !st18 = let
      k18 :: K_Pattern_s18  t -> t
      k18 K_Pattern_v9 = v9
      k18 K_Pattern_v27 = v27
      k18 K_Pattern_v31 = v31
      k18 K_Pattern_v41 = v41
      k18 K_Pattern_v46 = v46
      k18 K_Pattern_v55 = v55
      v9 :: T_Pattern_v9 
      v9 = \ !(T_Pattern_vIn9 _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt) -> (
         let !_patX18 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pat_)) in
         let !_patOcon = rule174 _lhsIcon in
         let !_patOnt = rule177 _lhsInt in
         let !_def = rule166 arg_attr_ arg_field_ in
         let !_patOchildInhs = rule172 _lhsIchildInhs in
         let !_patOdefs = rule175 _lhsIdefs in
         let !(!_field',!_err1) = rule162 _lhsIchildInhs _lhsIcon _lhsInt arg_attr_ arg_field_ in
         let !_err2 = rule163 _field' _lhsIcon _lhsIdefs _lhsInt arg_attr_ arg_field_ in
         let !_patOforcedIrrefutables = rule176 _lhsIforcedIrrefutables in
         let !(T_Pattern_vOut27 _patIallAttributes _patIcopy _patIdefsCollect _patIerrors _patIoutput) = inv_Pattern_s18 _patX18 K_Pattern_v27 (T_Pattern_vIn27 _patOchildInhs _patOcon _patOdefs _patOforcedIrrefutables _patOnt) in
         let _lhsOallAttributes :: AttrMap
             !_lhsOallAttributes = rule168 _lhsIcon _lhsInt _patIallAttributes arg_attr_ arg_field_ in
         let !_copy = rule170 _patIcopy arg_attr_ arg_field_ in
         let _lhsOcopy :: Pattern
             !_lhsOcopy = rule171 _copy in
         let _lhsOdefsCollect :: Set (Identifier, Identifier)
             !_lhsOdefsCollect = rule167 _def _patIdefsCollect in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule164 _err1 _err2 _patIerrors in
         let !_output = rule165 _field' _patIoutput arg_attr_ in
         let _lhsOoutput :: Pattern
             !_lhsOoutput = rule169 _lhsIcon _lhsIforcedIrrefutables _lhsInt _output arg_attr_ arg_field_ in
         let !__result_ = T_Pattern_vOut9 _lhsOallAttributes _lhsOcopy _lhsOdefsCollect _lhsOerrors _lhsOoutput
          in __result_ )
      v27 :: T_Pattern_v27 
      v27 = \ !(T_Pattern_vIn27 _lhsIchildInhs _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt) -> (
         let !_patX18 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pat_)) in
         let !_patOcon = rule174 _lhsIcon in
         let !_patOnt = rule177 _lhsInt in
         let !_def = rule166 arg_attr_ arg_field_ in
         let !_patOchildInhs = rule172 _lhsIchildInhs in
         let !_patOdefs = rule175 _lhsIdefs in
         let !(!_field',!_err1) = rule162 _lhsIchildInhs _lhsIcon _lhsInt arg_attr_ arg_field_ in
         let !_err2 = rule163 _field' _lhsIcon _lhsIdefs _lhsInt arg_attr_ arg_field_ in
         let !_patOforcedIrrefutables = rule176 _lhsIforcedIrrefutables in
         let !(T_Pattern_vOut27 _patIallAttributes _patIcopy _patIdefsCollect _patIerrors _patIoutput) = inv_Pattern_s18 _patX18 K_Pattern_v27 (T_Pattern_vIn27 _patOchildInhs _patOcon _patOdefs _patOforcedIrrefutables _patOnt) in
         let _lhsOallAttributes :: AttrMap
             !_lhsOallAttributes = rule168 _lhsIcon _lhsInt _patIallAttributes arg_attr_ arg_field_ in
         let !_copy = rule170 _patIcopy arg_attr_ arg_field_ in
         let _lhsOcopy :: Pattern
             !_lhsOcopy = rule171 _copy in
         let _lhsOdefsCollect :: Set (Identifier, Identifier)
             !_lhsOdefsCollect = rule167 _def _patIdefsCollect in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule164 _err1 _err2 _patIerrors in
         let !_output = rule165 _field' _patIoutput arg_attr_ in
         let _lhsOoutput :: Pattern
             !_lhsOoutput = rule169 _lhsIcon _lhsIforcedIrrefutables _lhsInt _output arg_attr_ arg_field_ in
         let !__result_ = T_Pattern_vOut27 _lhsOallAttributes _lhsOcopy _lhsOdefsCollect _lhsOerrors _lhsOoutput
          in __result_ )
      v31 :: T_Pattern_v31 
      v31 = \ !(T_Pattern_vIn31 _lhsIchildInhs _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt) -> (
         let !_patX18 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pat_)) in
         let !_patOcon = rule174 _lhsIcon in
         let !_patOnt = rule177 _lhsInt in
         let !_def = rule166 arg_attr_ arg_field_ in
         let !_patOchildInhs = rule172 _lhsIchildInhs in
         let !_patOdefs = rule175 _lhsIdefs in
         let !(!_field',!_err1) = rule162 _lhsIchildInhs _lhsIcon _lhsInt arg_attr_ arg_field_ in
         let !_err2 = rule163 _field' _lhsIcon _lhsIdefs _lhsInt arg_attr_ arg_field_ in
         let !_patOforcedIrrefutables = rule176 _lhsIforcedIrrefutables in
         let !(T_Pattern_vOut31 _patIallAttributes _patIdefsCollect _patIerrors _patIoutput) = inv_Pattern_s18 _patX18 K_Pattern_v31 (T_Pattern_vIn31 _patOchildInhs _patOcon _patOdefs _patOforcedIrrefutables _patOnt) in
         let _lhsOallAttributes :: AttrMap
             !_lhsOallAttributes = rule168 _lhsIcon _lhsInt _patIallAttributes arg_attr_ arg_field_ in
         let _lhsOdefsCollect :: Set (Identifier, Identifier)
             !_lhsOdefsCollect = rule167 _def _patIdefsCollect in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule164 _err1 _err2 _patIerrors in
         let !_output = rule165 _field' _patIoutput arg_attr_ in
         let _lhsOoutput :: Pattern
             !_lhsOoutput = rule169 _lhsIcon _lhsIforcedIrrefutables _lhsInt _output arg_attr_ arg_field_ in
         let !__result_ = T_Pattern_vOut31 _lhsOallAttributes _lhsOdefsCollect _lhsOerrors _lhsOoutput
          in __result_ )
      v41 :: T_Pattern_v41 
      v41 = \ !(T_Pattern_vIn41 _lhsIcon _lhsInt) -> (
         let !_patX18 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pat_)) in
         let !_patOcon = rule174 _lhsIcon in
         let !_patOnt = rule177 _lhsInt in
         let !_def = rule166 arg_attr_ arg_field_ in
         let !(T_Pattern_vOut41 _patIallAttributes _patIdefsCollect _patX51) = inv_Pattern_s18 _patX18 K_Pattern_v41 (T_Pattern_vIn41 _patOcon _patOnt) in
         let _lhsOallAttributes :: AttrMap
             !_lhsOallAttributes = rule168 _lhsIcon _lhsInt _patIallAttributes arg_attr_ arg_field_ in
         let _lhsOdefsCollect :: Set (Identifier, Identifier)
             !_lhsOdefsCollect = rule167 _def _patIdefsCollect in
         let !__st_ = st51 _lhsIcon _lhsInt _patX51
             !__result_ = T_Pattern_vOut41 _lhsOallAttributes _lhsOdefsCollect __st_
          in __result_ )
      v46 :: T_Pattern_v46 
      v46 = \ !(T_Pattern_vIn46 ) -> (
         let !_patX18 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pat_)) in
         let !_def = rule166 arg_attr_ arg_field_ in
         let !(T_Pattern_vOut46 _patIdefsCollect _patX54) = inv_Pattern_s18 _patX18 K_Pattern_v46 (T_Pattern_vIn46 ) in
         let _lhsOdefsCollect :: Set (Identifier, Identifier)
             !_lhsOdefsCollect = rule167 _def _patIdefsCollect in
         let !__st_ = st54 _patX54
             !__result_ = T_Pattern_vOut46 _lhsOdefsCollect __st_
          in __result_ )
      v55 :: T_Pattern_v55 
      v55 = \ !(T_Pattern_vIn55 _lhsIcon _lhsInt) -> (
         let !_patX18 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pat_)) in
         let !_patOcon = rule174 _lhsIcon in
         let !_patOnt = rule177 _lhsInt in
         let !(T_Pattern_vOut55 _patIallAttributes _patX58) = inv_Pattern_s18 _patX18 K_Pattern_v55 (T_Pattern_vIn55 _patOcon _patOnt) in
         let _lhsOallAttributes :: AttrMap
             !_lhsOallAttributes = rule168 _lhsIcon _lhsInt _patIallAttributes arg_attr_ arg_field_ in
         let !__st_ = st58 _lhsIcon _lhsInt _patX58
             !__result_ = T_Pattern_vOut55 _lhsOallAttributes __st_
          in __result_ )
     in C_Pattern_s18 k18
   {-# NOINLINE st51 #-}
   st51 = \ ((!_lhsIcon) :: ConstructorIdent) ((!_lhsInt) :: NontermIdent) !_patX51 -> let
      v42 :: T_Pattern_v42 
      v42 = \ !(T_Pattern_vIn42 _lhsIchildInhs _lhsIdefs _lhsIforcedIrrefutables) -> (
         let !_patOchildInhs = rule172 _lhsIchildInhs in
         let !_patOdefs = rule175 _lhsIdefs in
         let !(!_field',!_err1) = rule162 _lhsIchildInhs _lhsIcon _lhsInt arg_attr_ arg_field_ in
         let !_err2 = rule163 _field' _lhsIcon _lhsIdefs _lhsInt arg_attr_ arg_field_ in
         let !_patOforcedIrrefutables = rule176 _lhsIforcedIrrefutables in
         let !(T_Pattern_vOut42 _patIerrors _patIoutput) = inv_Pattern_s51 _patX51 (T_Pattern_vIn42 _patOchildInhs _patOdefs _patOforcedIrrefutables) in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule164 _err1 _err2 _patIerrors in
         let !_output = rule165 _field' _patIoutput arg_attr_ in
         let _lhsOoutput :: Pattern
             !_lhsOoutput = rule169 _lhsIcon _lhsIforcedIrrefutables _lhsInt _output arg_attr_ arg_field_ in
         let !__result_ = T_Pattern_vOut42 _lhsOerrors _lhsOoutput
          in __result_ )
     in C_Pattern_s51 v42
   {-# NOINLINE st54 #-}
   st54 = \ !_patX54 -> let
      v48 :: T_Pattern_v48 
      v48 = \ !(T_Pattern_vIn48 _lhsIchildInhs _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt) -> (
         let !_patOcon = rule174 _lhsIcon in
         let !_patOnt = rule177 _lhsInt in
         let !_patOchildInhs = rule172 _lhsIchildInhs in
         let !_patOdefs = rule175 _lhsIdefs in
         let !(!_field',!_err1) = rule162 _lhsIchildInhs _lhsIcon _lhsInt arg_attr_ arg_field_ in
         let !_err2 = rule163 _field' _lhsIcon _lhsIdefs _lhsInt arg_attr_ arg_field_ in
         let !_patOforcedIrrefutables = rule176 _lhsIforcedIrrefutables in
         let !(T_Pattern_vOut48 _patIallAttributes _patIerrors _patIoutput) = inv_Pattern_s54 _patX54 (T_Pattern_vIn48 _patOchildInhs _patOcon _patOdefs _patOforcedIrrefutables _patOnt) in
         let _lhsOallAttributes :: AttrMap
             !_lhsOallAttributes = rule168 _lhsIcon _lhsInt _patIallAttributes arg_attr_ arg_field_ in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule164 _err1 _err2 _patIerrors in
         let !_output = rule165 _field' _patIoutput arg_attr_ in
         let _lhsOoutput :: Pattern
             !_lhsOoutput = rule169 _lhsIcon _lhsIforcedIrrefutables _lhsInt _output arg_attr_ arg_field_ in
         let !__result_ = T_Pattern_vOut48 _lhsOallAttributes _lhsOerrors _lhsOoutput
          in __result_ )
     in C_Pattern_s54 v48
   {-# NOINLINE st58 #-}
   st58 = \ ((!_lhsIcon) :: ConstructorIdent) ((!_lhsInt) :: NontermIdent) !_patX58 -> let
      v56 :: T_Pattern_v56 
      v56 = \ !(T_Pattern_vIn56 ) -> (
         let !_def = rule166 arg_attr_ arg_field_ in
         let !(T_Pattern_vOut56 _patIdefsCollect _patX51) = inv_Pattern_s58 _patX58 (T_Pattern_vIn56 ) in
         let _lhsOdefsCollect :: Set (Identifier, Identifier)
             !_lhsOdefsCollect = rule167 _def _patIdefsCollect in
         let !__st_ = st51 _lhsIcon _lhsInt _patX51
             !__result_ = T_Pattern_vOut56 _lhsOdefsCollect __st_
          in __result_ )
     in C_Pattern_s58 v56
   {-# NOINLINE rule162 #-}
   {-# LINE 110 "./src-ag/Desugar.ag" #-}
   rule162 = \ ((!_lhsIchildInhs) :: [(Identifier, Identifier)]) ((!_lhsIcon) :: ConstructorIdent) ((!_lhsInt) :: NontermIdent) !attr_ !field_ ->
                                 {-# LINE 110 "./src-ag/Desugar.ag" #-}
                                 maybeError field_ (UndefAttr _lhsInt _lhsIcon (Ident "<ANY>" (getPos field_)) attr_ True) $
                                   findField field_ attr_ _lhsIchildInhs
                                 {-# LINE 2470 "dist/build/Desugar.hs"#-}
   {-# NOINLINE rule163 #-}
   {-# LINE 112 "./src-ag/Desugar.ag" #-}
   rule163 = \ !_field' ((!_lhsIcon) :: ConstructorIdent) ((!_lhsIdefs) :: Set (Identifier, Identifier)) ((!_lhsInt) :: NontermIdent) !attr_ !field_ ->
                   {-# LINE 112 "./src-ag/Desugar.ag" #-}
                   if _field'     == field_
                   then Seq.empty
                   else if (_field'    , attr_) `Set.member` _lhsIdefs
                        then Seq.singleton $ DupRule _lhsInt _lhsIcon field_ attr_ _field'
                        else Seq.empty
                   {-# LINE 2480 "dist/build/Desugar.hs"#-}
   {-# NOINLINE rule164 #-}
   {-# LINE 117 "./src-ag/Desugar.ag" #-}
   rule164 = \ !_err1 !_err2 ((!_patIerrors) :: Seq Error) ->
                     {-# LINE 117 "./src-ag/Desugar.ag" #-}
                     _err1     Seq.>< _err2     Seq.>< _patIerrors
                     {-# LINE 2486 "dist/build/Desugar.hs"#-}
   {-# NOINLINE rule165 #-}
   {-# LINE 118 "./src-ag/Desugar.ag" #-}
   rule165 = \ !_field' ((!_patIoutput) :: Pattern) !attr_ ->
                     {-# LINE 118 "./src-ag/Desugar.ag" #-}
                     Alias _field'     attr_ _patIoutput
                     {-# LINE 2492 "dist/build/Desugar.hs"#-}
   {-# NOINLINE rule166 #-}
   {-# LINE 182 "./src-ag/Desugar.ag" #-}
   rule166 = \ !attr_ !field_ ->
                  {-# LINE 182 "./src-ag/Desugar.ag" #-}
                  Set.singleton (field_, attr_)
                  {-# LINE 2498 "dist/build/Desugar.hs"#-}
   {-# NOINLINE rule167 #-}
   {-# LINE 183 "./src-ag/Desugar.ag" #-}
   rule167 = \ !_def ((!_patIdefsCollect) :: Set (Identifier, Identifier)) ->
                          {-# LINE 183 "./src-ag/Desugar.ag" #-}
                          _def     `Set.union` _patIdefsCollect
                          {-# LINE 2504 "dist/build/Desugar.hs"#-}
   {-# NOINLINE rule168 #-}
   {-# LINE 200 "./src-ag/Desugar.ag" #-}
   rule168 = \ ((!_lhsIcon) :: ConstructorIdent) ((!_lhsInt) :: NontermIdent) ((!_patIallAttributes) :: AttrMap) !attr_ !field_ ->
                            {-# LINE 200 "./src-ag/Desugar.ag" #-}
                            (Map.singleton _lhsInt $ Map.singleton _lhsIcon $ Set.singleton (field_, attr_)) `mergeAttributes` _patIallAttributes
                            {-# LINE 2510 "dist/build/Desugar.hs"#-}
   {-# NOINLINE rule169 #-}
   {-# LINE 219 "./src-ag/Desugar.ag" #-}
   rule169 = \ ((!_lhsIcon) :: ConstructorIdent) ((!_lhsIforcedIrrefutables) :: AttrMap) ((!_lhsInt) :: NontermIdent) !_output !attr_ !field_ ->
                     {-# LINE 219 "./src-ag/Desugar.ag" #-}
                     if Set.member (field_, attr_) $ Map.findWithDefault Set.empty _lhsIcon $ Map.findWithDefault Map.empty _lhsInt $ _lhsIforcedIrrefutables
                     then Irrefutable _output
                     else _output
                     {-# LINE 2518 "dist/build/Desugar.hs"#-}
   {-# NOINLINE[1] rule170 #-}
   rule170 = \ ((!_patIcopy) :: Pattern) !attr_ !field_ ->
     Alias field_ attr_ _patIcopy
   {-# NOINLINE[1] rule171 #-}
   rule171 = \ !_copy ->
     _copy
   {-# NOINLINE[1] rule172 #-}
   rule172 = \ ((!_lhsIchildInhs) :: [(Identifier, Identifier)]) ->
     _lhsIchildInhs
   {-# NOINLINE[1] rule174 #-}
   rule174 = \ ((!_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# NOINLINE[1] rule175 #-}
   rule175 = \ ((!_lhsIdefs) :: Set (Identifier, Identifier)) ->
     _lhsIdefs
   {-# NOINLINE[1] rule176 #-}
   rule176 = \ ((!_lhsIforcedIrrefutables) :: AttrMap) ->
     _lhsIforcedIrrefutables
   {-# NOINLINE[1] rule177 #-}
   rule177 = \ ((!_lhsInt) :: NontermIdent) ->
     _lhsInt
{-# NOINLINE sem_Pattern_Irrefutable #-}
sem_Pattern_Irrefutable :: T_Pattern  -> T_Pattern 
sem_Pattern_Irrefutable arg_pat_ = T_Pattern (return st18) where
   {-# NOINLINE st18 #-}
   !st18 = let
      k18 :: K_Pattern_s18  t -> t
      k18 K_Pattern_v9 = v9
      k18 K_Pattern_v27 = v27
      k18 K_Pattern_v31 = v31
      k18 K_Pattern_v41 = v41
      k18 K_Pattern_v46 = v46
      k18 K_Pattern_v55 = v55
      v9 :: T_Pattern_v9 
      v9 = \ !(T_Pattern_vIn9 _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt) -> (
         let _lhsOallAttributes :: AttrMap
             !_lhsOallAttributes = rule178  () in
         let !_patX18 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pat_)) in
         let !_patOchildInhs = rule185 _lhsIchildInhs in
         let !_patOcon = rule187 _lhsIcon in
         let !_patOdefs = rule188 _lhsIdefs in
         let !_patOnt = rule190 _lhsInt in
         let !_patOforcedIrrefutables = rule189 _lhsIforcedIrrefutables in
         let !(T_Pattern_vOut27 _patIallAttributes _patIcopy _patIdefsCollect _patIerrors _patIoutput) = inv_Pattern_s18 _patX18 K_Pattern_v27 (T_Pattern_vIn27 _patOchildInhs _patOcon _patOdefs _patOforcedIrrefutables _patOnt) in
         let !_copy = rule181 _patIcopy in
         let _lhsOcopy :: Pattern
             !_lhsOcopy = rule183 _copy in
         let _lhsOdefsCollect :: Set (Identifier, Identifier)
             !_lhsOdefsCollect = rule179 _patIdefsCollect in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule180 _patIerrors in
         let !_output = rule182 _patIoutput in
         let _lhsOoutput :: Pattern
             !_lhsOoutput = rule184 _output in
         let !__result_ = T_Pattern_vOut9 _lhsOallAttributes _lhsOcopy _lhsOdefsCollect _lhsOerrors _lhsOoutput
          in __result_ )
      v27 :: T_Pattern_v27 
      v27 = \ !(T_Pattern_vIn27 _lhsIchildInhs _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt) -> (
         let _lhsOallAttributes :: AttrMap
             !_lhsOallAttributes = rule178  () in
         let !_patX18 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pat_)) in
         let !_patOchildInhs = rule185 _lhsIchildInhs in
         let !_patOcon = rule187 _lhsIcon in
         let !_patOdefs = rule188 _lhsIdefs in
         let !_patOnt = rule190 _lhsInt in
         let !_patOforcedIrrefutables = rule189 _lhsIforcedIrrefutables in
         let !(T_Pattern_vOut27 _patIallAttributes _patIcopy _patIdefsCollect _patIerrors _patIoutput) = inv_Pattern_s18 _patX18 K_Pattern_v27 (T_Pattern_vIn27 _patOchildInhs _patOcon _patOdefs _patOforcedIrrefutables _patOnt) in
         let !_copy = rule181 _patIcopy in
         let _lhsOcopy :: Pattern
             !_lhsOcopy = rule183 _copy in
         let _lhsOdefsCollect :: Set (Identifier, Identifier)
             !_lhsOdefsCollect = rule179 _patIdefsCollect in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule180 _patIerrors in
         let !_output = rule182 _patIoutput in
         let _lhsOoutput :: Pattern
             !_lhsOoutput = rule184 _output in
         let !__result_ = T_Pattern_vOut27 _lhsOallAttributes _lhsOcopy _lhsOdefsCollect _lhsOerrors _lhsOoutput
          in __result_ )
      v31 :: T_Pattern_v31 
      v31 = \ !(T_Pattern_vIn31 _lhsIchildInhs _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt) -> (
         let _lhsOallAttributes :: AttrMap
             !_lhsOallAttributes = rule178  () in
         let !_patX18 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pat_)) in
         let !_patOchildInhs = rule185 _lhsIchildInhs in
         let !_patOcon = rule187 _lhsIcon in
         let !_patOdefs = rule188 _lhsIdefs in
         let !_patOnt = rule190 _lhsInt in
         let !_patOforcedIrrefutables = rule189 _lhsIforcedIrrefutables in
         let !(T_Pattern_vOut31 _patIallAttributes _patIdefsCollect _patIerrors _patIoutput) = inv_Pattern_s18 _patX18 K_Pattern_v31 (T_Pattern_vIn31 _patOchildInhs _patOcon _patOdefs _patOforcedIrrefutables _patOnt) in
         let _lhsOdefsCollect :: Set (Identifier, Identifier)
             !_lhsOdefsCollect = rule179 _patIdefsCollect in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule180 _patIerrors in
         let !_output = rule182 _patIoutput in
         let _lhsOoutput :: Pattern
             !_lhsOoutput = rule184 _output in
         let !__result_ = T_Pattern_vOut31 _lhsOallAttributes _lhsOdefsCollect _lhsOerrors _lhsOoutput
          in __result_ )
      v41 :: T_Pattern_v41 
      v41 = \ !(T_Pattern_vIn41 _lhsIcon _lhsInt) -> (
         let _lhsOallAttributes :: AttrMap
             !_lhsOallAttributes = rule178  () in
         let !_patX18 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pat_)) in
         let !(T_Pattern_vOut46 _patIdefsCollect _patX54) = inv_Pattern_s18 _patX18 K_Pattern_v46 (T_Pattern_vIn46 ) in
         let _lhsOdefsCollect :: Set (Identifier, Identifier)
             !_lhsOdefsCollect = rule179 _patIdefsCollect in
         let !__st_ = st51 _lhsIcon _lhsInt _patX54
             !__result_ = T_Pattern_vOut41 _lhsOallAttributes _lhsOdefsCollect __st_
          in __result_ )
      v46 :: T_Pattern_v46 
      v46 = \ !(T_Pattern_vIn46 ) -> (
         let !_patX18 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pat_)) in
         let !(T_Pattern_vOut46 _patIdefsCollect _patX54) = inv_Pattern_s18 _patX18 K_Pattern_v46 (T_Pattern_vIn46 ) in
         let _lhsOdefsCollect :: Set (Identifier, Identifier)
             !_lhsOdefsCollect = rule179 _patIdefsCollect in
         let !__st_ = st54 _patX54
             !__result_ = T_Pattern_vOut46 _lhsOdefsCollect __st_
          in __result_ )
      v55 :: T_Pattern_v55 
      v55 = \ !(T_Pattern_vIn55 _lhsIcon _lhsInt) -> (
         let _lhsOallAttributes :: AttrMap
             !_lhsOallAttributes = rule178  () in
         let !__st_ = st58 _lhsIcon _lhsInt
             !__result_ = T_Pattern_vOut55 _lhsOallAttributes __st_
          in __result_ )
     in C_Pattern_s18 k18
   {-# NOINLINE st51 #-}
   st51 = \ ((!_lhsIcon) :: ConstructorIdent) ((!_lhsInt) :: NontermIdent) !_patX54 -> let
      v42 :: T_Pattern_v42 
      v42 = \ !(T_Pattern_vIn42 _lhsIchildInhs _lhsIdefs _lhsIforcedIrrefutables) -> (
         let !_patOcon = rule187 _lhsIcon in
         let !_patOnt = rule190 _lhsInt in
         let !_patOchildInhs = rule185 _lhsIchildInhs in
         let !_patOdefs = rule188 _lhsIdefs in
         let !_patOforcedIrrefutables = rule189 _lhsIforcedIrrefutables in
         let !(T_Pattern_vOut48 _patIallAttributes _patIerrors _patIoutput) = inv_Pattern_s54 _patX54 (T_Pattern_vIn48 _patOchildInhs _patOcon _patOdefs _patOforcedIrrefutables _patOnt) in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule180 _patIerrors in
         let !_output = rule182 _patIoutput in
         let _lhsOoutput :: Pattern
             !_lhsOoutput = rule184 _output in
         let !__result_ = T_Pattern_vOut42 _lhsOerrors _lhsOoutput
          in __result_ )
     in C_Pattern_s51 v42
   {-# NOINLINE st54 #-}
   st54 = \ !_patX54 -> let
      v48 :: T_Pattern_v48 
      v48 = \ !(T_Pattern_vIn48 _lhsIchildInhs _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt) -> (
         let _lhsOallAttributes :: AttrMap
             !_lhsOallAttributes = rule178  () in
         let !_patOchildInhs = rule185 _lhsIchildInhs in
         let !_patOcon = rule187 _lhsIcon in
         let !_patOdefs = rule188 _lhsIdefs in
         let !_patOnt = rule190 _lhsInt in
         let !_patOforcedIrrefutables = rule189 _lhsIforcedIrrefutables in
         let !(T_Pattern_vOut48 _patIallAttributes _patIerrors _patIoutput) = inv_Pattern_s54 _patX54 (T_Pattern_vIn48 _patOchildInhs _patOcon _patOdefs _patOforcedIrrefutables _patOnt) in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule180 _patIerrors in
         let !_output = rule182 _patIoutput in
         let _lhsOoutput :: Pattern
             !_lhsOoutput = rule184 _output in
         let !__result_ = T_Pattern_vOut48 _lhsOallAttributes _lhsOerrors _lhsOoutput
          in __result_ )
     in C_Pattern_s54 v48
   {-# NOINLINE st58 #-}
   st58 = \ ((!_lhsIcon) :: ConstructorIdent) ((!_lhsInt) :: NontermIdent) -> let
      v56 :: T_Pattern_v56 
      v56 = \ !(T_Pattern_vIn56 ) -> (
         let !_patX18 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pat_)) in
         let !(T_Pattern_vOut46 _patIdefsCollect _patX54) = inv_Pattern_s18 _patX18 K_Pattern_v46 (T_Pattern_vIn46 ) in
         let _lhsOdefsCollect :: Set (Identifier, Identifier)
             !_lhsOdefsCollect = rule179 _patIdefsCollect in
         let !__st_ = st51 _lhsIcon _lhsInt _patX54
             !__result_ = T_Pattern_vOut56 _lhsOdefsCollect __st_
          in __result_ )
     in C_Pattern_s58 v56
   {-# NOINLINE rule178 #-}
   {-# LINE 202 "./src-ag/Desugar.ag" #-}
   rule178 = \  (_ :: ()) ->
                            {-# LINE 202 "./src-ag/Desugar.ag" #-}
                            Map.empty
                            {-# LINE 2701 "dist/build/Desugar.hs"#-}
   {-# NOINLINE[1] rule179 #-}
   rule179 = \ ((!_patIdefsCollect) :: Set (Identifier, Identifier)) ->
     _patIdefsCollect
   {-# NOINLINE[1] rule180 #-}
   rule180 = \ ((!_patIerrors) :: Seq Error) ->
     _patIerrors
   {-# NOINLINE[1] rule181 #-}
   rule181 = \ ((!_patIcopy) :: Pattern) ->
     Irrefutable _patIcopy
   {-# NOINLINE[1] rule182 #-}
   rule182 = \ ((!_patIoutput) :: Pattern) ->
     Irrefutable _patIoutput
   {-# NOINLINE[1] rule183 #-}
   rule183 = \ !_copy ->
     _copy
   {-# NOINLINE[1] rule184 #-}
   rule184 = \ !_output ->
     _output
   {-# NOINLINE[1] rule185 #-}
   rule185 = \ ((!_lhsIchildInhs) :: [(Identifier, Identifier)]) ->
     _lhsIchildInhs
   {-# NOINLINE[1] rule187 #-}
   rule187 = \ ((!_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# NOINLINE[1] rule188 #-}
   rule188 = \ ((!_lhsIdefs) :: Set (Identifier, Identifier)) ->
     _lhsIdefs
   {-# NOINLINE[1] rule189 #-}
   rule189 = \ ((!_lhsIforcedIrrefutables) :: AttrMap) ->
     _lhsIforcedIrrefutables
   {-# NOINLINE[1] rule190 #-}
   rule190 = \ ((!_lhsInt) :: NontermIdent) ->
     _lhsInt
{-# NOINLINE sem_Pattern_Underscore #-}
sem_Pattern_Underscore :: (Pos) -> T_Pattern 
sem_Pattern_Underscore !arg_pos_ = T_Pattern (return st18) where
   {-# NOINLINE st18 #-}
   !st18 = let
      k18 :: K_Pattern_s18  t -> t
      k18 K_Pattern_v9 = v9
      k18 K_Pattern_v27 = v27
      k18 K_Pattern_v31 = v31
      k18 K_Pattern_v41 = v41
      k18 K_Pattern_v46 = v46
      k18 K_Pattern_v55 = v55
      v9 :: T_Pattern_v9 
      v9 = \ !(T_Pattern_vIn9 _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt) -> (
         let _lhsOallAttributes :: AttrMap
             !_lhsOallAttributes = rule191  () in
         let _lhsOdefsCollect :: Set (Identifier, Identifier)
             !_lhsOdefsCollect = rule192  () in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule193  () in
         let !_copy = rule194 arg_pos_ in
         let _lhsOcopy :: Pattern
             !_lhsOcopy = rule196 _copy in
         let !_output = rule195 arg_pos_ in
         let _lhsOoutput :: Pattern
             !_lhsOoutput = rule197 _output in
         let !__result_ = T_Pattern_vOut9 _lhsOallAttributes _lhsOcopy _lhsOdefsCollect _lhsOerrors _lhsOoutput
          in __result_ )
      v27 :: T_Pattern_v27 
      v27 = \ !(T_Pattern_vIn27 _lhsIchildInhs _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt) -> (
         let _lhsOallAttributes :: AttrMap
             !_lhsOallAttributes = rule191  () in
         let _lhsOdefsCollect :: Set (Identifier, Identifier)
             !_lhsOdefsCollect = rule192  () in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule193  () in
         let !_copy = rule194 arg_pos_ in
         let _lhsOcopy :: Pattern
             !_lhsOcopy = rule196 _copy in
         let !_output = rule195 arg_pos_ in
         let _lhsOoutput :: Pattern
             !_lhsOoutput = rule197 _output in
         let !__result_ = T_Pattern_vOut27 _lhsOallAttributes _lhsOcopy _lhsOdefsCollect _lhsOerrors _lhsOoutput
          in __result_ )
      v31 :: T_Pattern_v31 
      v31 = \ !(T_Pattern_vIn31 _lhsIchildInhs _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt) -> (
         let _lhsOallAttributes :: AttrMap
             !_lhsOallAttributes = rule191  () in
         let _lhsOdefsCollect :: Set (Identifier, Identifier)
             !_lhsOdefsCollect = rule192  () in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule193  () in
         let !_output = rule195 arg_pos_ in
         let _lhsOoutput :: Pattern
             !_lhsOoutput = rule197 _output in
         let !__result_ = T_Pattern_vOut31 _lhsOallAttributes _lhsOdefsCollect _lhsOerrors _lhsOoutput
          in __result_ )
      v41 :: T_Pattern_v41 
      v41 = \ !(T_Pattern_vIn41 _lhsIcon _lhsInt) -> (
         let _lhsOallAttributes :: AttrMap
             !_lhsOallAttributes = rule191  () in
         let _lhsOdefsCollect :: Set (Identifier, Identifier)
             !_lhsOdefsCollect = rule192  () in
         let !__st_ = st51  ()
             !__result_ = T_Pattern_vOut41 _lhsOallAttributes _lhsOdefsCollect __st_
          in __result_ )
      v46 :: T_Pattern_v46 
      v46 = \ !(T_Pattern_vIn46 ) -> (
         let _lhsOdefsCollect :: Set (Identifier, Identifier)
             !_lhsOdefsCollect = rule192  () in
         let !__st_ = st54  ()
             !__result_ = T_Pattern_vOut46 _lhsOdefsCollect __st_
          in __result_ )
      v55 :: T_Pattern_v55 
      v55 = \ !(T_Pattern_vIn55 _lhsIcon _lhsInt) -> (
         let _lhsOallAttributes :: AttrMap
             !_lhsOallAttributes = rule191  () in
         let !__st_ = st58  ()
             !__result_ = T_Pattern_vOut55 _lhsOallAttributes __st_
          in __result_ )
     in C_Pattern_s18 k18
   {-# NOINLINE st51 #-}
   st51 = \  (_ :: ()) -> let
      v42 :: T_Pattern_v42 
      v42 = \ !(T_Pattern_vIn42 _lhsIchildInhs _lhsIdefs _lhsIforcedIrrefutables) -> (
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule193  () in
         let !_output = rule195 arg_pos_ in
         let _lhsOoutput :: Pattern
             !_lhsOoutput = rule197 _output in
         let !__result_ = T_Pattern_vOut42 _lhsOerrors _lhsOoutput
          in __result_ )
     in C_Pattern_s51 v42
   {-# NOINLINE st54 #-}
   st54 = \  (_ :: ()) -> let
      v48 :: T_Pattern_v48 
      v48 = \ !(T_Pattern_vIn48 _lhsIchildInhs _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt) -> (
         let _lhsOallAttributes :: AttrMap
             !_lhsOallAttributes = rule191  () in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule193  () in
         let !_output = rule195 arg_pos_ in
         let _lhsOoutput :: Pattern
             !_lhsOoutput = rule197 _output in
         let !__result_ = T_Pattern_vOut48 _lhsOallAttributes _lhsOerrors _lhsOoutput
          in __result_ )
     in C_Pattern_s54 v48
   {-# NOINLINE st58 #-}
   st58 = \  (_ :: ()) -> let
      v56 :: T_Pattern_v56 
      v56 = \ !(T_Pattern_vIn56 ) -> (
         let _lhsOdefsCollect :: Set (Identifier, Identifier)
             !_lhsOdefsCollect = rule192  () in
         let !__st_ = st51  ()
             !__result_ = T_Pattern_vOut56 _lhsOdefsCollect __st_
          in __result_ )
     in C_Pattern_s58 v56
   {-# NOINLINE[1] rule191 #-}
   rule191 = \  (_ :: ()) ->
     Map.empty
   {-# NOINLINE[1] rule192 #-}
   rule192 = \  (_ :: ()) ->
     Set.empty
   {-# NOINLINE[1] rule193 #-}
   rule193 = \  (_ :: ()) ->
     Seq.empty
   {-# NOINLINE[1] rule194 #-}
   rule194 = \ !pos_ ->
     Underscore pos_
   {-# NOINLINE[1] rule195 #-}
   rule195 = \ !pos_ ->
     Underscore pos_
   {-# NOINLINE[1] rule196 #-}
   rule196 = \ !_copy ->
     _copy
   {-# NOINLINE[1] rule197 #-}
   rule197 = \ !_output ->
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
        let arg = T_Patterns_vIn10 _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt
        !(T_Patterns_vOut10 _lhsOallAttributes _lhsOcopy _lhsOdefsCollect _lhsOerrors _lhsOoutput) <- return (inv_Patterns_s20 sem K_Patterns_v10 arg)
        return (Syn_Patterns _lhsOallAttributes _lhsOcopy _lhsOdefsCollect _lhsOerrors _lhsOoutput)
   )

-- cata
{-# NOINLINE sem_Patterns #-}
sem_Patterns :: Patterns  -> T_Patterns 
sem_Patterns list = Prelude.foldr sem_Patterns_Cons sem_Patterns_Nil (Prelude.map sem_Pattern list)

-- semantic domain
newtype T_Patterns  = T_Patterns {
                                 attach_T_Patterns :: Identity (T_Patterns_s20 )
                                 }
data T_Patterns_s20  where C_Patterns_s20 :: {
                                             inv_Patterns_s20 :: !(forall t. K_Patterns_s20  t -> t)
                                             } -> T_Patterns_s20 
data T_Patterns_s21  = C_Patterns_s21
data T_Patterns_s41  = C_Patterns_s41
data T_Patterns_s49  = C_Patterns_s49
newtype T_Patterns_s53  = C_Patterns_s53 {
                                         inv_Patterns_s53 :: (T_Patterns_v47 )
                                         }
newtype T_Patterns_s56  = C_Patterns_s56 {
                                         inv_Patterns_s56 :: (T_Patterns_v52 )
                                         }
newtype T_Patterns_s59  = C_Patterns_s59 {
                                         inv_Patterns_s59 :: (T_Patterns_v58 )
                                         }
data K_Patterns_s20 k  where
   K_Patterns_v10 :: K_Patterns_s20  (T_Patterns_v10 )
   K_Patterns_v26 :: K_Patterns_s20  (T_Patterns_v26 )
   K_Patterns_v38 :: K_Patterns_s20  (T_Patterns_v38 )
   K_Patterns_v45 :: K_Patterns_s20  (T_Patterns_v45 )
   K_Patterns_v51 :: K_Patterns_s20  (T_Patterns_v51 )
   K_Patterns_v57 :: K_Patterns_s20  (T_Patterns_v57 )
type T_Patterns_v10  = (T_Patterns_vIn10 ) -> (T_Patterns_vOut10 )
data T_Patterns_vIn10  = T_Patterns_vIn10 !([(Identifier, Identifier)]) !([(Identifier, Identifier)]) !(ConstructorIdent) !(Set (Identifier, Identifier)) !(AttrMap) !(NontermIdent)
data T_Patterns_vOut10  = T_Patterns_vOut10 !(AttrMap) !(Patterns) !(Set (Identifier, Identifier)) !(Seq Error) !(Patterns)
type T_Patterns_v26  = (T_Patterns_vIn26 ) -> (T_Patterns_vOut26 )
data T_Patterns_vIn26  = T_Patterns_vIn26 !([(Identifier, Identifier)]) !(ConstructorIdent) !(Set (Identifier, Identifier)) !(AttrMap) !(NontermIdent)
data T_Patterns_vOut26  = T_Patterns_vOut26 !(AttrMap) !(Patterns) !(Set (Identifier, Identifier)) !(Seq Error) !(Patterns)
type T_Patterns_v38  = (T_Patterns_vIn38 ) -> (T_Patterns_vOut38 )
data T_Patterns_vIn38  = T_Patterns_vIn38 !([(Identifier, Identifier)]) !(ConstructorIdent) !(Set (Identifier, Identifier)) !(AttrMap) !(NontermIdent)
data T_Patterns_vOut38  = T_Patterns_vOut38 !(AttrMap) !(Set (Identifier, Identifier)) !(Seq Error) !(Patterns)
type T_Patterns_v45  = (T_Patterns_vIn45 ) -> (T_Patterns_vOut45 )
data T_Patterns_vIn45  = T_Patterns_vIn45 !(ConstructorIdent) !(NontermIdent)
data T_Patterns_vOut45  = T_Patterns_vOut45 !(AttrMap) !(Set (Identifier, Identifier)) !(T_Patterns_s53 )
type T_Patterns_v47  = (T_Patterns_vIn47 ) -> (T_Patterns_vOut47 )
data T_Patterns_vIn47  = T_Patterns_vIn47 !([(Identifier, Identifier)]) !(Set (Identifier, Identifier)) !(AttrMap)
data T_Patterns_vOut47  = T_Patterns_vOut47 !(Seq Error) !(Patterns)
type T_Patterns_v51  = (T_Patterns_vIn51 ) -> (T_Patterns_vOut51 )
data T_Patterns_vIn51  = T_Patterns_vIn51 
data T_Patterns_vOut51  = T_Patterns_vOut51 !(Set (Identifier, Identifier)) !(T_Patterns_s56 )
type T_Patterns_v52  = (T_Patterns_vIn52 ) -> (T_Patterns_vOut52 )
data T_Patterns_vIn52  = T_Patterns_vIn52 !([(Identifier, Identifier)]) !(ConstructorIdent) !(Set (Identifier, Identifier)) !(AttrMap) !(NontermIdent)
data T_Patterns_vOut52  = T_Patterns_vOut52 !(AttrMap) !(Seq Error) !(Patterns)
type T_Patterns_v57  = (T_Patterns_vIn57 ) -> (T_Patterns_vOut57 )
data T_Patterns_vIn57  = T_Patterns_vIn57 !(ConstructorIdent) !(NontermIdent)
data T_Patterns_vOut57  = T_Patterns_vOut57 !(AttrMap) !(T_Patterns_s59 )
type T_Patterns_v58  = (T_Patterns_vIn58 ) -> (T_Patterns_vOut58 )
data T_Patterns_vIn58  = T_Patterns_vIn58 
data T_Patterns_vOut58  = T_Patterns_vOut58 !(Set (Identifier, Identifier)) !(T_Patterns_s53 )
{-# NOINLINE sem_Patterns_Cons #-}
sem_Patterns_Cons :: T_Pattern  -> T_Patterns  -> T_Patterns 
sem_Patterns_Cons arg_hd_ arg_tl_ = T_Patterns (return st20) where
   {-# NOINLINE st20 #-}
   !st20 = let
      k20 :: K_Patterns_s20  t -> t
      k20 K_Patterns_v10 = v10
      k20 K_Patterns_v26 = v26
      k20 K_Patterns_v38 = v38
      k20 K_Patterns_v45 = v45
      k20 K_Patterns_v51 = v51
      k20 K_Patterns_v57 = v57
      v10 :: T_Patterns_v10 
      v10 = \ !(T_Patterns_vIn10 _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt) -> (
         let !_hdX18 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_hd_)) in
         let !_tlX20 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_tl_)) in
         let !_hdOcon = rule207 _lhsIcon in
         let !_hdOnt = rule210 _lhsInt in
         let !_tlOcon = rule213 _lhsIcon in
         let !_tlOnt = rule216 _lhsInt in
         let !_hdOchildInhs = rule205 _lhsIchildInhs in
         let !_hdOdefs = rule208 _lhsIdefs in
         let !_tlOchildInhs = rule211 _lhsIchildInhs in
         let !_tlOdefs = rule214 _lhsIdefs in
         let !_hdOforcedIrrefutables = rule209 _lhsIforcedIrrefutables in
         let !_tlOforcedIrrefutables = rule215 _lhsIforcedIrrefutables in
         let !(T_Pattern_vOut27 _hdIallAttributes _hdIcopy _hdIdefsCollect _hdIerrors _hdIoutput) = inv_Pattern_s18 _hdX18 K_Pattern_v27 (T_Pattern_vIn27 _hdOchildInhs _hdOcon _hdOdefs _hdOforcedIrrefutables _hdOnt) in
         let !(T_Patterns_vOut26 _tlIallAttributes _tlIcopy _tlIdefsCollect _tlIerrors _tlIoutput) = inv_Patterns_s20 _tlX20 K_Patterns_v26 (T_Patterns_vIn26 _tlOchildInhs _tlOcon _tlOdefs _tlOforcedIrrefutables _tlOnt) in
         let _lhsOallAttributes :: AttrMap
             !_lhsOallAttributes = rule198 _hdIallAttributes _tlIallAttributes in
         let !_copy = rule201 _hdIcopy _tlIcopy in
         let _lhsOcopy :: Patterns
             !_lhsOcopy = rule203 _copy in
         let _lhsOdefsCollect :: Set (Identifier, Identifier)
             !_lhsOdefsCollect = rule199 _hdIdefsCollect _tlIdefsCollect in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule200 _hdIerrors _tlIerrors in
         let !_output = rule202 _hdIoutput _tlIoutput in
         let _lhsOoutput :: Patterns
             !_lhsOoutput = rule204 _output in
         let !__result_ = T_Patterns_vOut10 _lhsOallAttributes _lhsOcopy _lhsOdefsCollect _lhsOerrors _lhsOoutput
          in __result_ )
      v26 :: T_Patterns_v26 
      v26 = \ !(T_Patterns_vIn26 _lhsIchildInhs _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt) -> (
         let !_hdX18 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_hd_)) in
         let !_tlX20 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_tl_)) in
         let !_hdOcon = rule207 _lhsIcon in
         let !_hdOnt = rule210 _lhsInt in
         let !_tlOcon = rule213 _lhsIcon in
         let !_tlOnt = rule216 _lhsInt in
         let !_hdOchildInhs = rule205 _lhsIchildInhs in
         let !_hdOdefs = rule208 _lhsIdefs in
         let !_tlOchildInhs = rule211 _lhsIchildInhs in
         let !_tlOdefs = rule214 _lhsIdefs in
         let !_hdOforcedIrrefutables = rule209 _lhsIforcedIrrefutables in
         let !_tlOforcedIrrefutables = rule215 _lhsIforcedIrrefutables in
         let !(T_Pattern_vOut27 _hdIallAttributes _hdIcopy _hdIdefsCollect _hdIerrors _hdIoutput) = inv_Pattern_s18 _hdX18 K_Pattern_v27 (T_Pattern_vIn27 _hdOchildInhs _hdOcon _hdOdefs _hdOforcedIrrefutables _hdOnt) in
         let !(T_Patterns_vOut26 _tlIallAttributes _tlIcopy _tlIdefsCollect _tlIerrors _tlIoutput) = inv_Patterns_s20 _tlX20 K_Patterns_v26 (T_Patterns_vIn26 _tlOchildInhs _tlOcon _tlOdefs _tlOforcedIrrefutables _tlOnt) in
         let _lhsOallAttributes :: AttrMap
             !_lhsOallAttributes = rule198 _hdIallAttributes _tlIallAttributes in
         let !_copy = rule201 _hdIcopy _tlIcopy in
         let _lhsOcopy :: Patterns
             !_lhsOcopy = rule203 _copy in
         let _lhsOdefsCollect :: Set (Identifier, Identifier)
             !_lhsOdefsCollect = rule199 _hdIdefsCollect _tlIdefsCollect in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule200 _hdIerrors _tlIerrors in
         let !_output = rule202 _hdIoutput _tlIoutput in
         let _lhsOoutput :: Patterns
             !_lhsOoutput = rule204 _output in
         let !__result_ = T_Patterns_vOut26 _lhsOallAttributes _lhsOcopy _lhsOdefsCollect _lhsOerrors _lhsOoutput
          in __result_ )
      v38 :: T_Patterns_v38 
      v38 = \ !(T_Patterns_vIn38 _lhsIchildInhs _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt) -> (
         let !_hdX18 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_hd_)) in
         let !_tlX20 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_tl_)) in
         let !_hdOcon = rule207 _lhsIcon in
         let !_hdOnt = rule210 _lhsInt in
         let !_tlOcon = rule213 _lhsIcon in
         let !_tlOnt = rule216 _lhsInt in
         let !_hdOchildInhs = rule205 _lhsIchildInhs in
         let !_hdOdefs = rule208 _lhsIdefs in
         let !_tlOchildInhs = rule211 _lhsIchildInhs in
         let !_tlOdefs = rule214 _lhsIdefs in
         let !_hdOforcedIrrefutables = rule209 _lhsIforcedIrrefutables in
         let !_tlOforcedIrrefutables = rule215 _lhsIforcedIrrefutables in
         let !(T_Pattern_vOut31 _hdIallAttributes _hdIdefsCollect _hdIerrors _hdIoutput) = inv_Pattern_s18 _hdX18 K_Pattern_v31 (T_Pattern_vIn31 _hdOchildInhs _hdOcon _hdOdefs _hdOforcedIrrefutables _hdOnt) in
         let !(T_Patterns_vOut38 _tlIallAttributes _tlIdefsCollect _tlIerrors _tlIoutput) = inv_Patterns_s20 _tlX20 K_Patterns_v38 (T_Patterns_vIn38 _tlOchildInhs _tlOcon _tlOdefs _tlOforcedIrrefutables _tlOnt) in
         let _lhsOallAttributes :: AttrMap
             !_lhsOallAttributes = rule198 _hdIallAttributes _tlIallAttributes in
         let _lhsOdefsCollect :: Set (Identifier, Identifier)
             !_lhsOdefsCollect = rule199 _hdIdefsCollect _tlIdefsCollect in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule200 _hdIerrors _tlIerrors in
         let !_output = rule202 _hdIoutput _tlIoutput in
         let _lhsOoutput :: Patterns
             !_lhsOoutput = rule204 _output in
         let !__result_ = T_Patterns_vOut38 _lhsOallAttributes _lhsOdefsCollect _lhsOerrors _lhsOoutput
          in __result_ )
      v45 :: T_Patterns_v45 
      v45 = \ !(T_Patterns_vIn45 _lhsIcon _lhsInt) -> (
         let !_hdX18 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_hd_)) in
         let !_tlX20 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_tl_)) in
         let !_hdOcon = rule207 _lhsIcon in
         let !_hdOnt = rule210 _lhsInt in
         let !_tlOcon = rule213 _lhsIcon in
         let !_tlOnt = rule216 _lhsInt in
         let !(T_Pattern_vOut41 _hdIallAttributes _hdIdefsCollect _hdX51) = inv_Pattern_s18 _hdX18 K_Pattern_v41 (T_Pattern_vIn41 _hdOcon _hdOnt) in
         let !(T_Patterns_vOut45 _tlIallAttributes _tlIdefsCollect _tlX53) = inv_Patterns_s20 _tlX20 K_Patterns_v45 (T_Patterns_vIn45 _tlOcon _tlOnt) in
         let _lhsOallAttributes :: AttrMap
             !_lhsOallAttributes = rule198 _hdIallAttributes _tlIallAttributes in
         let _lhsOdefsCollect :: Set (Identifier, Identifier)
             !_lhsOdefsCollect = rule199 _hdIdefsCollect _tlIdefsCollect in
         let !__st_ = st53 _hdX51 _tlX53
             !__result_ = T_Patterns_vOut45 _lhsOallAttributes _lhsOdefsCollect __st_
          in __result_ )
      v51 :: T_Patterns_v51 
      v51 = \ !(T_Patterns_vIn51 ) -> (
         let !_hdX18 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_hd_)) in
         let !_tlX20 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_tl_)) in
         let !(T_Pattern_vOut46 _hdIdefsCollect _hdX54) = inv_Pattern_s18 _hdX18 K_Pattern_v46 (T_Pattern_vIn46 ) in
         let !(T_Patterns_vOut51 _tlIdefsCollect _tlX56) = inv_Patterns_s20 _tlX20 K_Patterns_v51 (T_Patterns_vIn51 ) in
         let _lhsOdefsCollect :: Set (Identifier, Identifier)
             !_lhsOdefsCollect = rule199 _hdIdefsCollect _tlIdefsCollect in
         let !__st_ = st56 _hdX54 _tlX56
             !__result_ = T_Patterns_vOut51 _lhsOdefsCollect __st_
          in __result_ )
      v57 :: T_Patterns_v57 
      v57 = \ !(T_Patterns_vIn57 _lhsIcon _lhsInt) -> (
         let !_hdX18 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_hd_)) in
         let !_tlX20 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_tl_)) in
         let !_hdOcon = rule207 _lhsIcon in
         let !_hdOnt = rule210 _lhsInt in
         let !_tlOcon = rule213 _lhsIcon in
         let !_tlOnt = rule216 _lhsInt in
         let !(T_Pattern_vOut55 _hdIallAttributes _hdX58) = inv_Pattern_s18 _hdX18 K_Pattern_v55 (T_Pattern_vIn55 _hdOcon _hdOnt) in
         let !(T_Patterns_vOut57 _tlIallAttributes _tlX59) = inv_Patterns_s20 _tlX20 K_Patterns_v57 (T_Patterns_vIn57 _tlOcon _tlOnt) in
         let _lhsOallAttributes :: AttrMap
             !_lhsOallAttributes = rule198 _hdIallAttributes _tlIallAttributes in
         let !__st_ = st59 _hdX58 _tlX59
             !__result_ = T_Patterns_vOut57 _lhsOallAttributes __st_
          in __result_ )
     in C_Patterns_s20 k20
   {-# NOINLINE st53 #-}
   st53 = \ !_hdX51 !_tlX53 -> let
      v47 :: T_Patterns_v47 
      v47 = \ !(T_Patterns_vIn47 _lhsIchildInhs _lhsIdefs _lhsIforcedIrrefutables) -> (
         let !_hdOchildInhs = rule205 _lhsIchildInhs in
         let !_hdOdefs = rule208 _lhsIdefs in
         let !_tlOchildInhs = rule211 _lhsIchildInhs in
         let !_tlOdefs = rule214 _lhsIdefs in
         let !_hdOforcedIrrefutables = rule209 _lhsIforcedIrrefutables in
         let !_tlOforcedIrrefutables = rule215 _lhsIforcedIrrefutables in
         let !(T_Pattern_vOut42 _hdIerrors _hdIoutput) = inv_Pattern_s51 _hdX51 (T_Pattern_vIn42 _hdOchildInhs _hdOdefs _hdOforcedIrrefutables) in
         let !(T_Patterns_vOut47 _tlIerrors _tlIoutput) = inv_Patterns_s53 _tlX53 (T_Patterns_vIn47 _tlOchildInhs _tlOdefs _tlOforcedIrrefutables) in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule200 _hdIerrors _tlIerrors in
         let !_output = rule202 _hdIoutput _tlIoutput in
         let _lhsOoutput :: Patterns
             !_lhsOoutput = rule204 _output in
         let !__result_ = T_Patterns_vOut47 _lhsOerrors _lhsOoutput
          in __result_ )
     in C_Patterns_s53 v47
   {-# NOINLINE st56 #-}
   st56 = \ !_hdX54 !_tlX56 -> let
      v52 :: T_Patterns_v52 
      v52 = \ !(T_Patterns_vIn52 _lhsIchildInhs _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt) -> (
         let !_hdOcon = rule207 _lhsIcon in
         let !_hdOnt = rule210 _lhsInt in
         let !_tlOcon = rule213 _lhsIcon in
         let !_tlOnt = rule216 _lhsInt in
         let !_hdOchildInhs = rule205 _lhsIchildInhs in
         let !_hdOdefs = rule208 _lhsIdefs in
         let !_tlOchildInhs = rule211 _lhsIchildInhs in
         let !_tlOdefs = rule214 _lhsIdefs in
         let !_hdOforcedIrrefutables = rule209 _lhsIforcedIrrefutables in
         let !_tlOforcedIrrefutables = rule215 _lhsIforcedIrrefutables in
         let !(T_Pattern_vOut48 _hdIallAttributes _hdIerrors _hdIoutput) = inv_Pattern_s54 _hdX54 (T_Pattern_vIn48 _hdOchildInhs _hdOcon _hdOdefs _hdOforcedIrrefutables _hdOnt) in
         let !(T_Patterns_vOut52 _tlIallAttributes _tlIerrors _tlIoutput) = inv_Patterns_s56 _tlX56 (T_Patterns_vIn52 _tlOchildInhs _tlOcon _tlOdefs _tlOforcedIrrefutables _tlOnt) in
         let _lhsOallAttributes :: AttrMap
             !_lhsOallAttributes = rule198 _hdIallAttributes _tlIallAttributes in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule200 _hdIerrors _tlIerrors in
         let !_output = rule202 _hdIoutput _tlIoutput in
         let _lhsOoutput :: Patterns
             !_lhsOoutput = rule204 _output in
         let !__result_ = T_Patterns_vOut52 _lhsOallAttributes _lhsOerrors _lhsOoutput
          in __result_ )
     in C_Patterns_s56 v52
   {-# NOINLINE st59 #-}
   st59 = \ !_hdX58 !_tlX59 -> let
      v58 :: T_Patterns_v58 
      v58 = \ !(T_Patterns_vIn58 ) -> (
         let !(T_Pattern_vOut56 _hdIdefsCollect _hdX51) = inv_Pattern_s58 _hdX58 (T_Pattern_vIn56 ) in
         let !(T_Patterns_vOut58 _tlIdefsCollect _tlX53) = inv_Patterns_s59 _tlX59 (T_Patterns_vIn58 ) in
         let _lhsOdefsCollect :: Set (Identifier, Identifier)
             !_lhsOdefsCollect = rule199 _hdIdefsCollect _tlIdefsCollect in
         let !__st_ = st53 _hdX51 _tlX53
             !__result_ = T_Patterns_vOut58 _lhsOdefsCollect __st_
          in __result_ )
     in C_Patterns_s59 v58
   {-# NOINLINE[1] rule198 #-}
   rule198 = \ ((!_hdIallAttributes) :: AttrMap) ((!_tlIallAttributes) :: AttrMap) ->
     _hdIallAttributes `mergeAttributes` _tlIallAttributes
   {-# NOINLINE[1] rule199 #-}
   rule199 = \ ((!_hdIdefsCollect) :: Set (Identifier, Identifier)) ((!_tlIdefsCollect) :: Set (Identifier, Identifier)) ->
     _hdIdefsCollect `Set.union` _tlIdefsCollect
   {-# NOINLINE[1] rule200 #-}
   rule200 = \ ((!_hdIerrors) :: Seq Error) ((!_tlIerrors) :: Seq Error) ->
     _hdIerrors Seq.>< _tlIerrors
   {-# NOINLINE[1] rule201 #-}
   rule201 = \ ((!_hdIcopy) :: Pattern) ((!_tlIcopy) :: Patterns) ->
     (:) _hdIcopy _tlIcopy
   {-# NOINLINE[1] rule202 #-}
   rule202 = \ ((!_hdIoutput) :: Pattern) ((!_tlIoutput) :: Patterns) ->
     (:) _hdIoutput _tlIoutput
   {-# NOINLINE[1] rule203 #-}
   rule203 = \ !_copy ->
     _copy
   {-# NOINLINE[1] rule204 #-}
   rule204 = \ !_output ->
     _output
   {-# NOINLINE[1] rule205 #-}
   rule205 = \ ((!_lhsIchildInhs) :: [(Identifier, Identifier)]) ->
     _lhsIchildInhs
   {-# NOINLINE[1] rule207 #-}
   rule207 = \ ((!_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# NOINLINE[1] rule208 #-}
   rule208 = \ ((!_lhsIdefs) :: Set (Identifier, Identifier)) ->
     _lhsIdefs
   {-# NOINLINE[1] rule209 #-}
   rule209 = \ ((!_lhsIforcedIrrefutables) :: AttrMap) ->
     _lhsIforcedIrrefutables
   {-# NOINLINE[1] rule210 #-}
   rule210 = \ ((!_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# NOINLINE[1] rule211 #-}
   rule211 = \ ((!_lhsIchildInhs) :: [(Identifier, Identifier)]) ->
     _lhsIchildInhs
   {-# NOINLINE[1] rule213 #-}
   rule213 = \ ((!_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# NOINLINE[1] rule214 #-}
   rule214 = \ ((!_lhsIdefs) :: Set (Identifier, Identifier)) ->
     _lhsIdefs
   {-# NOINLINE[1] rule215 #-}
   rule215 = \ ((!_lhsIforcedIrrefutables) :: AttrMap) ->
     _lhsIforcedIrrefutables
   {-# NOINLINE[1] rule216 #-}
   rule216 = \ ((!_lhsInt) :: NontermIdent) ->
     _lhsInt
{-# NOINLINE sem_Patterns_Nil #-}
sem_Patterns_Nil ::  T_Patterns 
sem_Patterns_Nil  = T_Patterns (return st20) where
   {-# NOINLINE st20 #-}
   !st20 = let
      k20 :: K_Patterns_s20  t -> t
      k20 K_Patterns_v10 = v10
      k20 K_Patterns_v26 = v26
      k20 K_Patterns_v38 = v38
      k20 K_Patterns_v45 = v45
      k20 K_Patterns_v51 = v51
      k20 K_Patterns_v57 = v57
      v10 :: T_Patterns_v10 
      v10 = \ !(T_Patterns_vIn10 _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt) -> (
         let _lhsOallAttributes :: AttrMap
             !_lhsOallAttributes = rule217  () in
         let !_copy = rule220  () in
         let _lhsOdefsCollect :: Set (Identifier, Identifier)
             !_lhsOdefsCollect = rule218  () in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule219  () in
         let !_output = rule221  () in
         let _lhsOcopy :: Patterns
             !_lhsOcopy = rule222 _copy in
         let _lhsOoutput :: Patterns
             !_lhsOoutput = rule223 _output in
         let !__result_ = T_Patterns_vOut10 _lhsOallAttributes _lhsOcopy _lhsOdefsCollect _lhsOerrors _lhsOoutput
          in __result_ )
      v26 :: T_Patterns_v26 
      v26 = \ !(T_Patterns_vIn26 _lhsIchildInhs _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt) -> (
         let _lhsOallAttributes :: AttrMap
             !_lhsOallAttributes = rule217  () in
         let !_copy = rule220  () in
         let _lhsOdefsCollect :: Set (Identifier, Identifier)
             !_lhsOdefsCollect = rule218  () in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule219  () in
         let !_output = rule221  () in
         let _lhsOcopy :: Patterns
             !_lhsOcopy = rule222 _copy in
         let _lhsOoutput :: Patterns
             !_lhsOoutput = rule223 _output in
         let !__result_ = T_Patterns_vOut26 _lhsOallAttributes _lhsOcopy _lhsOdefsCollect _lhsOerrors _lhsOoutput
          in __result_ )
      v38 :: T_Patterns_v38 
      v38 = \ !(T_Patterns_vIn38 _lhsIchildInhs _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt) -> (
         let _lhsOallAttributes :: AttrMap
             !_lhsOallAttributes = rule217  () in
         let _lhsOdefsCollect :: Set (Identifier, Identifier)
             !_lhsOdefsCollect = rule218  () in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule219  () in
         let !_output = rule221  () in
         let _lhsOoutput :: Patterns
             !_lhsOoutput = rule223 _output in
         let !__result_ = T_Patterns_vOut38 _lhsOallAttributes _lhsOdefsCollect _lhsOerrors _lhsOoutput
          in __result_ )
      v45 :: T_Patterns_v45 
      v45 = \ !(T_Patterns_vIn45 _lhsIcon _lhsInt) -> (
         let _lhsOallAttributes :: AttrMap
             !_lhsOallAttributes = rule217  () in
         let _lhsOdefsCollect :: Set (Identifier, Identifier)
             !_lhsOdefsCollect = rule218  () in
         let !__st_ = st53  ()
             !__result_ = T_Patterns_vOut45 _lhsOallAttributes _lhsOdefsCollect __st_
          in __result_ )
      v51 :: T_Patterns_v51 
      v51 = \ !(T_Patterns_vIn51 ) -> (
         let _lhsOdefsCollect :: Set (Identifier, Identifier)
             !_lhsOdefsCollect = rule218  () in
         let !__st_ = st56  ()
             !__result_ = T_Patterns_vOut51 _lhsOdefsCollect __st_
          in __result_ )
      v57 :: T_Patterns_v57 
      v57 = \ !(T_Patterns_vIn57 _lhsIcon _lhsInt) -> (
         let _lhsOallAttributes :: AttrMap
             !_lhsOallAttributes = rule217  () in
         let !__st_ = st59  ()
             !__result_ = T_Patterns_vOut57 _lhsOallAttributes __st_
          in __result_ )
     in C_Patterns_s20 k20
   {-# NOINLINE st53 #-}
   st53 = \  (_ :: ()) -> let
      v47 :: T_Patterns_v47 
      v47 = \ !(T_Patterns_vIn47 _lhsIchildInhs _lhsIdefs _lhsIforcedIrrefutables) -> (
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule219  () in
         let !_output = rule221  () in
         let _lhsOoutput :: Patterns
             !_lhsOoutput = rule223 _output in
         let !__result_ = T_Patterns_vOut47 _lhsOerrors _lhsOoutput
          in __result_ )
     in C_Patterns_s53 v47
   {-# NOINLINE st56 #-}
   st56 = \  (_ :: ()) -> let
      v52 :: T_Patterns_v52 
      v52 = \ !(T_Patterns_vIn52 _lhsIchildInhs _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt) -> (
         let _lhsOallAttributes :: AttrMap
             !_lhsOallAttributes = rule217  () in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule219  () in
         let !_output = rule221  () in
         let _lhsOoutput :: Patterns
             !_lhsOoutput = rule223 _output in
         let !__result_ = T_Patterns_vOut52 _lhsOallAttributes _lhsOerrors _lhsOoutput
          in __result_ )
     in C_Patterns_s56 v52
   {-# NOINLINE st59 #-}
   st59 = \  (_ :: ()) -> let
      v58 :: T_Patterns_v58 
      v58 = \ !(T_Patterns_vIn58 ) -> (
         let _lhsOdefsCollect :: Set (Identifier, Identifier)
             !_lhsOdefsCollect = rule218  () in
         let !__st_ = st53  ()
             !__result_ = T_Patterns_vOut58 _lhsOdefsCollect __st_
          in __result_ )
     in C_Patterns_s59 v58
   {-# NOINLINE[1] rule217 #-}
   rule217 = \  (_ :: ()) ->
     Map.empty
   {-# NOINLINE[1] rule218 #-}
   rule218 = \  (_ :: ()) ->
     Set.empty
   {-# NOINLINE[1] rule219 #-}
   rule219 = \  (_ :: ()) ->
     Seq.empty
   {-# NOINLINE[1] rule220 #-}
   rule220 = \  (_ :: ()) ->
     []
   {-# NOINLINE[1] rule221 #-}
   rule221 = \  (_ :: ()) ->
     []
   {-# NOINLINE[1] rule222 #-}
   rule222 = \ !_copy ->
     _copy
   {-# NOINLINE[1] rule223 #-}
   rule223 = \ !_output ->
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
        let arg = T_Production_vIn11 _lhsIaugmentsIn _lhsIforcedIrrefutables _lhsIinhMap _lhsImainName _lhsInt _lhsIoptions _lhsIsynMap
        !(T_Production_vOut11 _lhsOallAttributes _lhsOaugmentsOut _lhsOerrors _lhsOoutput) <- return (inv_Production_s22 sem K_Production_v11 arg)
        return (Syn_Production _lhsOallAttributes _lhsOaugmentsOut _lhsOerrors _lhsOoutput)
   )

-- cata
{-# INLINE sem_Production #-}
sem_Production :: Production  -> T_Production 
sem_Production ( Production !con_ !params_ !constraints_ children_ rules_ typeSigs_ !macro_ ) = sem_Production_Production con_ params_ constraints_ ( sem_Children children_ ) ( sem_Rules rules_ ) ( sem_TypeSigs typeSigs_ ) macro_

-- semantic domain
newtype T_Production  = T_Production {
                                     attach_T_Production :: Identity (T_Production_s22 )
                                     }
data T_Production_s22  where C_Production_s22 :: {
                                                 inv_Production_s22 :: !(forall t. K_Production_s22  t -> t)
                                                 } -> T_Production_s22 
data T_Production_s23  = C_Production_s23
data T_Production_s44  = C_Production_s44
newtype T_Production_s52  = C_Production_s52 {
                                             inv_Production_s52 :: (T_Production_v44 )
                                             }
data K_Production_s22 k  where
   K_Production_v11 :: K_Production_s22  (T_Production_v11 )
   K_Production_v30 :: K_Production_s22  (T_Production_v30 )
   K_Production_v43 :: K_Production_s22  (T_Production_v43 )
type T_Production_v11  = (T_Production_vIn11 ) -> (T_Production_vOut11 )
data T_Production_vIn11  = T_Production_vIn11 !(Map ConstructorIdent (Map Identifier [Expression])) !(AttrMap) !(Map Identifier Attributes) !(String) !(NontermIdent) !(Options) !(Map Identifier Attributes)
data T_Production_vOut11  = T_Production_vOut11 !(AttrMap) !(Map ConstructorIdent (Map Identifier [Expression])) !(Seq Error) !(Production)
type T_Production_v30  = (T_Production_vIn30 ) -> (T_Production_vOut30 )
data T_Production_vIn30  = T_Production_vIn30 !(Map ConstructorIdent (Map Identifier [Expression])) !(AttrMap) !(Map Identifier Attributes) !(NontermIdent) !(Options) !(Map Identifier Attributes)
data T_Production_vOut30  = T_Production_vOut30 !(AttrMap) !(Map ConstructorIdent (Map Identifier [Expression])) !(Seq Error) !(Production)
type T_Production_v43  = (T_Production_vIn43 ) -> (T_Production_vOut43 )
data T_Production_vIn43  = T_Production_vIn43 !(NontermIdent)
data T_Production_vOut43  = T_Production_vOut43 !(AttrMap) !(T_Production_s52 )
type T_Production_v44  = (T_Production_vIn44 ) -> (T_Production_vOut44 )
data T_Production_vIn44  = T_Production_vIn44 !(Map ConstructorIdent (Map Identifier [Expression])) !(AttrMap) !(Map Identifier Attributes) !(Options) !(Map Identifier Attributes)
data T_Production_vOut44  = T_Production_vOut44 !(Map ConstructorIdent (Map Identifier [Expression])) !(Seq Error) !(Production)
{-# NOINLINE sem_Production_Production #-}
sem_Production_Production :: (ConstructorIdent) -> ([Identifier]) -> ([Type]) -> T_Children  -> T_Rules  -> T_TypeSigs  -> (MaybeMacro) -> T_Production 
sem_Production_Production !arg_con_ !arg_params_ !arg_constraints_ arg_children_ arg_rules_ arg_typeSigs_ !arg_macro_ = T_Production (return st22) where
   {-# NOINLINE st22 #-}
   !st22 = let
      k22 :: K_Production_s22  t -> t
      k22 K_Production_v11 = v11
      k22 K_Production_v30 = v30
      k22 K_Production_v43 = v43
      v11 :: T_Production_v11 
      v11 = \ !(T_Production_vIn11 _lhsIaugmentsIn _lhsIforcedIrrefutables _lhsIinhMap _lhsImainName _lhsInt _lhsIoptions _lhsIsynMap) -> (
         let !_rulesX28 = Control.Monad.Identity.runIdentity (attach_T_Rules (arg_rules_)) in
         let !_childrenX2 = Control.Monad.Identity.runIdentity (attach_T_Children (arg_children_)) in
         let !_typeSigsX32 = Control.Monad.Identity.runIdentity (attach_T_TypeSigs (arg_typeSigs_)) in
         let !_rulesOcon = rule224 arg_con_ in
         let !_rulesOnt = rule241 _lhsInt in
         let !_childrenOinhMap = rule234 _lhsIinhMap in
         let !_childrenOsynMap = rule237 _lhsIsynMap in
         let !_augmentsIn = rule226 _lhsIaugmentsIn arg_con_ in
         let !_rulesOoptions = rule242 _lhsIoptions in
         let !_rulesOforcedIrrefutables = rule240 _lhsIforcedIrrefutables in
         let !(T_Children_vOut18 _childrenIchildInhs _childrenIchildSyns _childrenIoutput) = inv_Children_s2 _childrenX2 K_Children_v18 (T_Children_vIn18 _childrenOinhMap _childrenOsynMap) in
         let !(T_Rules_vOut28 _rulesIallAttributes _rulesIdefsCollect _rulesX43) = inv_Rules_s28 _rulesX28 K_Rules_v28 (T_Rules_vIn28 _rulesOcon _rulesOnt) in
         let !(T_TypeSigs_vOut16 _typeSigsIoutput) = inv_TypeSigs_s32 _typeSigsX32 (T_TypeSigs_vIn16 ) in
         let _lhsOallAttributes :: AttrMap
             !_lhsOallAttributes = rule230 _rulesIallAttributes in
         let !(!_augmentErrs,!_augmentsOut1) = rule228 _augmentsIn _childrenIchildInhs _childrenIchildSyns _lhsInt _lhsIoptions arg_con_ in
         let !_augmentsOut = rule227 _augmentsOut1 arg_con_ in
         let _lhsOaugmentsOut :: Map ConstructorIdent (Map Identifier [Expression])
             !_lhsOaugmentsOut = rule231 _augmentsOut in
         let !_rulesOchildInhs = rule238 _childrenIchildInhs in
         let !_rulesOchildSyns = rule239 _childrenIchildSyns in
         let !_rulesOdefs = rule225 _rulesIdefsCollect in
         let !(T_Rules_vOut29 _rulesIerrors _rulesIoutput) = inv_Rules_s43 _rulesX43 (T_Rules_vIn29 _rulesOchildInhs _rulesOchildSyns _rulesOdefs _rulesOforcedIrrefutables _rulesOoptions) in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule229 _augmentErrs _rulesIerrors in
         let !_output = rule232 _childrenIoutput _rulesIoutput _typeSigsIoutput arg_con_ arg_constraints_ arg_macro_ arg_params_ in
         let _lhsOoutput :: Production
             !_lhsOoutput = rule233 _output in
         let !__result_ = T_Production_vOut11 _lhsOallAttributes _lhsOaugmentsOut _lhsOerrors _lhsOoutput
          in __result_ )
      v30 :: T_Production_v30 
      v30 = \ !(T_Production_vIn30 _lhsIaugmentsIn _lhsIforcedIrrefutables _lhsIinhMap _lhsInt _lhsIoptions _lhsIsynMap) -> (
         let !_rulesX28 = Control.Monad.Identity.runIdentity (attach_T_Rules (arg_rules_)) in
         let !_childrenX2 = Control.Monad.Identity.runIdentity (attach_T_Children (arg_children_)) in
         let !_typeSigsX32 = Control.Monad.Identity.runIdentity (attach_T_TypeSigs (arg_typeSigs_)) in
         let !_rulesOcon = rule224 arg_con_ in
         let !_rulesOnt = rule241 _lhsInt in
         let !_childrenOinhMap = rule234 _lhsIinhMap in
         let !_childrenOsynMap = rule237 _lhsIsynMap in
         let !_augmentsIn = rule226 _lhsIaugmentsIn arg_con_ in
         let !_rulesOoptions = rule242 _lhsIoptions in
         let !_rulesOforcedIrrefutables = rule240 _lhsIforcedIrrefutables in
         let !(T_Children_vOut18 _childrenIchildInhs _childrenIchildSyns _childrenIoutput) = inv_Children_s2 _childrenX2 K_Children_v18 (T_Children_vIn18 _childrenOinhMap _childrenOsynMap) in
         let !(T_Rules_vOut28 _rulesIallAttributes _rulesIdefsCollect _rulesX43) = inv_Rules_s28 _rulesX28 K_Rules_v28 (T_Rules_vIn28 _rulesOcon _rulesOnt) in
         let !(T_TypeSigs_vOut16 _typeSigsIoutput) = inv_TypeSigs_s32 _typeSigsX32 (T_TypeSigs_vIn16 ) in
         let _lhsOallAttributes :: AttrMap
             !_lhsOallAttributes = rule230 _rulesIallAttributes in
         let !(!_augmentErrs,!_augmentsOut1) = rule228 _augmentsIn _childrenIchildInhs _childrenIchildSyns _lhsInt _lhsIoptions arg_con_ in
         let !_augmentsOut = rule227 _augmentsOut1 arg_con_ in
         let _lhsOaugmentsOut :: Map ConstructorIdent (Map Identifier [Expression])
             !_lhsOaugmentsOut = rule231 _augmentsOut in
         let !_rulesOchildInhs = rule238 _childrenIchildInhs in
         let !_rulesOchildSyns = rule239 _childrenIchildSyns in
         let !_rulesOdefs = rule225 _rulesIdefsCollect in
         let !(T_Rules_vOut29 _rulesIerrors _rulesIoutput) = inv_Rules_s43 _rulesX43 (T_Rules_vIn29 _rulesOchildInhs _rulesOchildSyns _rulesOdefs _rulesOforcedIrrefutables _rulesOoptions) in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule229 _augmentErrs _rulesIerrors in
         let !_output = rule232 _childrenIoutput _rulesIoutput _typeSigsIoutput arg_con_ arg_constraints_ arg_macro_ arg_params_ in
         let _lhsOoutput :: Production
             !_lhsOoutput = rule233 _output in
         let !__result_ = T_Production_vOut30 _lhsOallAttributes _lhsOaugmentsOut _lhsOerrors _lhsOoutput
          in __result_ )
      v43 :: T_Production_v43 
      v43 = \ !(T_Production_vIn43 _lhsInt) -> (
         let !_rulesX28 = Control.Monad.Identity.runIdentity (attach_T_Rules (arg_rules_)) in
         let !_rulesOcon = rule224 arg_con_ in
         let !_rulesOnt = rule241 _lhsInt in
         let !(T_Rules_vOut49 _rulesIallAttributes _rulesX55) = inv_Rules_s28 _rulesX28 K_Rules_v49 (T_Rules_vIn49 _rulesOcon _rulesOnt) in
         let _lhsOallAttributes :: AttrMap
             !_lhsOallAttributes = rule230 _rulesIallAttributes in
         let !__st_ = st52 _lhsInt _rulesX55
             !__result_ = T_Production_vOut43 _lhsOallAttributes __st_
          in __result_ )
     in C_Production_s22 k22
   {-# NOINLINE st52 #-}
   st52 = \ ((!_lhsInt) :: NontermIdent) !_rulesX55 -> let
      v44 :: T_Production_v44 
      v44 = \ !(T_Production_vIn44 _lhsIaugmentsIn _lhsIforcedIrrefutables _lhsIinhMap _lhsIoptions _lhsIsynMap) -> (
         let !_childrenX2 = Control.Monad.Identity.runIdentity (attach_T_Children (arg_children_)) in
         let !_typeSigsX32 = Control.Monad.Identity.runIdentity (attach_T_TypeSigs (arg_typeSigs_)) in
         let !_childrenOinhMap = rule234 _lhsIinhMap in
         let !_childrenOsynMap = rule237 _lhsIsynMap in
         let !_augmentsIn = rule226 _lhsIaugmentsIn arg_con_ in
         let !_rulesOoptions = rule242 _lhsIoptions in
         let !_rulesOforcedIrrefutables = rule240 _lhsIforcedIrrefutables in
         let !(T_Children_vOut18 _childrenIchildInhs _childrenIchildSyns _childrenIoutput) = inv_Children_s2 _childrenX2 K_Children_v18 (T_Children_vIn18 _childrenOinhMap _childrenOsynMap) in
         let !(T_Rules_vOut50 _rulesIdefsCollect _rulesX43) = inv_Rules_s55 _rulesX55 (T_Rules_vIn50 ) in
         let !(T_TypeSigs_vOut16 _typeSigsIoutput) = inv_TypeSigs_s32 _typeSigsX32 (T_TypeSigs_vIn16 ) in
         let !(!_augmentErrs,!_augmentsOut1) = rule228 _augmentsIn _childrenIchildInhs _childrenIchildSyns _lhsInt _lhsIoptions arg_con_ in
         let !_augmentsOut = rule227 _augmentsOut1 arg_con_ in
         let _lhsOaugmentsOut :: Map ConstructorIdent (Map Identifier [Expression])
             !_lhsOaugmentsOut = rule231 _augmentsOut in
         let !_rulesOchildInhs = rule238 _childrenIchildInhs in
         let !_rulesOchildSyns = rule239 _childrenIchildSyns in
         let !_rulesOdefs = rule225 _rulesIdefsCollect in
         let !(T_Rules_vOut29 _rulesIerrors _rulesIoutput) = inv_Rules_s43 _rulesX43 (T_Rules_vIn29 _rulesOchildInhs _rulesOchildSyns _rulesOdefs _rulesOforcedIrrefutables _rulesOoptions) in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule229 _augmentErrs _rulesIerrors in
         let !_output = rule232 _childrenIoutput _rulesIoutput _typeSigsIoutput arg_con_ arg_constraints_ arg_macro_ arg_params_ in
         let _lhsOoutput :: Production
             !_lhsOoutput = rule233 _output in
         let !__result_ = T_Production_vOut44 _lhsOaugmentsOut _lhsOerrors _lhsOoutput
          in __result_ )
     in C_Production_s52 v44
   {-# NOINLINE[1] rule224 #-}
   {-# LINE 161 "./src-ag/Desugar.ag" #-}
   rule224 = \ !con_ ->
                    {-# LINE 161 "./src-ag/Desugar.ag" #-}
                    con_
                    {-# LINE 3504 "dist/build/Desugar.hs"#-}
   {-# NOINLINE[1] rule225 #-}
   {-# LINE 188 "./src-ag/Desugar.ag" #-}
   rule225 = \ ((!_rulesIdefsCollect) :: Set (Identifier, Identifier)) ->
                     {-# LINE 188 "./src-ag/Desugar.ag" #-}
                     _rulesIdefsCollect
                     {-# LINE 3510 "dist/build/Desugar.hs"#-}
   {-# NOINLINE[1] rule226 #-}
   {-# LINE 244 "./src-ag/Desugar.ag" #-}
   rule226 = \ ((!_lhsIaugmentsIn) :: Map ConstructorIdent (Map Identifier [Expression])) !con_ ->
                         {-# LINE 244 "./src-ag/Desugar.ag" #-}
                         Map.findWithDefault Map.empty con_ _lhsIaugmentsIn
                         {-# LINE 3516 "dist/build/Desugar.hs"#-}
   {-# NOINLINE[1] rule227 #-}
   {-# LINE 245 "./src-ag/Desugar.ag" #-}
   rule227 = \ !_augmentsOut1 !con_ ->
                          {-# LINE 245 "./src-ag/Desugar.ag" #-}
                          Map.singleton con_ _augmentsOut1
                          {-# LINE 3522 "dist/build/Desugar.hs"#-}
   {-# NOINLINE[1] rule228 #-}
   {-# LINE 247 "./src-ag/Desugar.ag" #-}
   rule228 = \ !_augmentsIn ((!_childrenIchildInhs) :: [(Identifier, Identifier)]) ((!_childrenIchildSyns) :: [(Identifier, Identifier)]) ((!_lhsInt) :: NontermIdent) ((!_lhsIoptions) :: Options) !con_ ->
                                              {-# LINE 247 "./src-ag/Desugar.ag" #-}
                                              Map.mapAccum (desugarExprs _lhsIoptions _lhsInt con_ _childrenIchildInhs _childrenIchildSyns) Seq.empty _augmentsIn
                                              {-# LINE 3528 "dist/build/Desugar.hs"#-}
   {-# NOINLINE[1] rule229 #-}
   {-# LINE 283 "./src-ag/Desugar.ag" #-}
   rule229 = \ !_augmentErrs ((!_rulesIerrors) :: Seq Error) ->
                     {-# LINE 283 "./src-ag/Desugar.ag" #-}
                     _rulesIerrors Seq.>< _augmentErrs
                     {-# LINE 3534 "dist/build/Desugar.hs"#-}
   {-# NOINLINE[1] rule230 #-}
   rule230 = \ ((!_rulesIallAttributes) :: AttrMap) ->
     _rulesIallAttributes
   {-# NOINLINE[1] rule231 #-}
   rule231 = \ !_augmentsOut ->
     _augmentsOut
   {-# NOINLINE[1] rule232 #-}
   rule232 = \ ((!_childrenIoutput) :: Children) ((!_rulesIoutput) :: Rules) ((!_typeSigsIoutput) :: TypeSigs) !con_ !constraints_ !macro_ !params_ ->
     Production con_ params_ constraints_ _childrenIoutput _rulesIoutput _typeSigsIoutput macro_
   {-# NOINLINE[1] rule233 #-}
   rule233 = \ !_output ->
     _output
   {-# NOINLINE[1] rule234 #-}
   rule234 = \ ((!_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# NOINLINE[1] rule237 #-}
   rule237 = \ ((!_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
   {-# NOINLINE[1] rule238 #-}
   rule238 = \ ((!_childrenIchildInhs) :: [(Identifier, Identifier)]) ->
     _childrenIchildInhs
   {-# NOINLINE[1] rule239 #-}
   rule239 = \ ((!_childrenIchildSyns) :: [(Identifier, Identifier)]) ->
     _childrenIchildSyns
   {-# NOINLINE[1] rule240 #-}
   rule240 = \ ((!_lhsIforcedIrrefutables) :: AttrMap) ->
     _lhsIforcedIrrefutables
   {-# NOINLINE[1] rule241 #-}
   rule241 = \ ((!_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# NOINLINE[1] rule242 #-}
   rule242 = \ ((!_lhsIoptions) :: Options) ->
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
        let arg = T_Productions_vIn12 _lhsIaugmentsIn _lhsIforcedIrrefutables _lhsIinhMap _lhsImainName _lhsInt _lhsIoptions _lhsIsynMap
        !(T_Productions_vOut12 _lhsOallAttributes _lhsOaugmentsOut _lhsOerrors _lhsOoutput) <- return (inv_Productions_s24 sem K_Productions_v12 arg)
        return (Syn_Productions _lhsOallAttributes _lhsOaugmentsOut _lhsOerrors _lhsOoutput)
   )

-- cata
{-# NOINLINE sem_Productions #-}
sem_Productions :: Productions  -> T_Productions 
sem_Productions list = Prelude.foldr sem_Productions_Cons sem_Productions_Nil (Prelude.map sem_Production list)

-- semantic domain
newtype T_Productions  = T_Productions {
                                       attach_T_Productions :: Identity (T_Productions_s24 )
                                       }
data T_Productions_s24  where C_Productions_s24 :: {
                                                   inv_Productions_s24 :: !(forall t. K_Productions_s24  t -> t)
                                                   } -> T_Productions_s24 
data T_Productions_s25  = C_Productions_s25
data T_Productions_s40  = C_Productions_s40
newtype T_Productions_s50  = C_Productions_s50 {
                                               inv_Productions_s50 :: (T_Productions_v40 )
                                               }
data K_Productions_s24 k  where
   K_Productions_v12 :: K_Productions_s24  (T_Productions_v12 )
   K_Productions_v25 :: K_Productions_s24  (T_Productions_v25 )
   K_Productions_v39 :: K_Productions_s24  (T_Productions_v39 )
type T_Productions_v12  = (T_Productions_vIn12 ) -> (T_Productions_vOut12 )
data T_Productions_vIn12  = T_Productions_vIn12 !(Map ConstructorIdent (Map Identifier [Expression])) !(AttrMap) !(Map Identifier Attributes) !(String) !(NontermIdent) !(Options) !(Map Identifier Attributes)
data T_Productions_vOut12  = T_Productions_vOut12 !(AttrMap) !(Map ConstructorIdent (Map Identifier [Expression])) !(Seq Error) !(Productions)
type T_Productions_v25  = (T_Productions_vIn25 ) -> (T_Productions_vOut25 )
data T_Productions_vIn25  = T_Productions_vIn25 !(Map ConstructorIdent (Map Identifier [Expression])) !(AttrMap) !(Map Identifier Attributes) !(NontermIdent) !(Options) !(Map Identifier Attributes)
data T_Productions_vOut25  = T_Productions_vOut25 !(AttrMap) !(Map ConstructorIdent (Map Identifier [Expression])) !(Seq Error) !(Productions)
type T_Productions_v39  = (T_Productions_vIn39 ) -> (T_Productions_vOut39 )
data T_Productions_vIn39  = T_Productions_vIn39 !(NontermIdent)
data T_Productions_vOut39  = T_Productions_vOut39 !(AttrMap) !(T_Productions_s50 )
type T_Productions_v40  = (T_Productions_vIn40 ) -> (T_Productions_vOut40 )
data T_Productions_vIn40  = T_Productions_vIn40 !(Map ConstructorIdent (Map Identifier [Expression])) !(AttrMap) !(Map Identifier Attributes) !(Options) !(Map Identifier Attributes)
data T_Productions_vOut40  = T_Productions_vOut40 !(Map ConstructorIdent (Map Identifier [Expression])) !(Seq Error) !(Productions)
{-# NOINLINE sem_Productions_Cons #-}
sem_Productions_Cons :: T_Production  -> T_Productions  -> T_Productions 
sem_Productions_Cons arg_hd_ arg_tl_ = T_Productions (return st24) where
   {-# NOINLINE st24 #-}
   !st24 = let
      k24 :: K_Productions_s24  t -> t
      k24 K_Productions_v12 = v12
      k24 K_Productions_v25 = v25
      k24 K_Productions_v39 = v39
      v12 :: T_Productions_v12 
      v12 = \ !(T_Productions_vIn12 _lhsIaugmentsIn _lhsIforcedIrrefutables _lhsIinhMap _lhsImainName _lhsInt _lhsIoptions _lhsIsynMap) -> (
         let !_hdX22 = Control.Monad.Identity.runIdentity (attach_T_Production (arg_hd_)) in
         let !_tlX24 = Control.Monad.Identity.runIdentity (attach_T_Productions (arg_tl_)) in
         let !_hdOnt = rule252 _lhsInt in
         let !_tlOnt = rule259 _lhsInt in
         let !_hdOaugmentsIn = rule248 _lhsIaugmentsIn in
         let !_hdOinhMap = rule250 _lhsIinhMap in
         let !_hdOoptions = rule253 _lhsIoptions in
         let !_hdOsynMap = rule254 _lhsIsynMap in
         let !_tlOaugmentsIn = rule255 _lhsIaugmentsIn in
         let !_tlOinhMap = rule257 _lhsIinhMap in
         let !_tlOoptions = rule260 _lhsIoptions in
         let !_tlOsynMap = rule261 _lhsIsynMap in
         let !_hdOforcedIrrefutables = rule249 _lhsIforcedIrrefutables in
         let !_tlOforcedIrrefutables = rule256 _lhsIforcedIrrefutables in
         let !(T_Production_vOut30 _hdIallAttributes _hdIaugmentsOut _hdIerrors _hdIoutput) = inv_Production_s22 _hdX22 K_Production_v30 (T_Production_vIn30 _hdOaugmentsIn _hdOforcedIrrefutables _hdOinhMap _hdOnt _hdOoptions _hdOsynMap) in
         let !(T_Productions_vOut25 _tlIallAttributes _tlIaugmentsOut _tlIerrors _tlIoutput) = inv_Productions_s24 _tlX24 K_Productions_v25 (T_Productions_vIn25 _tlOaugmentsIn _tlOforcedIrrefutables _tlOinhMap _tlOnt _tlOoptions _tlOsynMap) in
         let _lhsOallAttributes :: AttrMap
             !_lhsOallAttributes = rule243 _hdIallAttributes _tlIallAttributes in
         let _lhsOaugmentsOut :: Map ConstructorIdent (Map Identifier [Expression])
             !_lhsOaugmentsOut = rule244 _hdIaugmentsOut _tlIaugmentsOut in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule245 _hdIerrors _tlIerrors in
         let !_output = rule246 _hdIoutput _tlIoutput in
         let _lhsOoutput :: Productions
             !_lhsOoutput = rule247 _output in
         let !__result_ = T_Productions_vOut12 _lhsOallAttributes _lhsOaugmentsOut _lhsOerrors _lhsOoutput
          in __result_ )
      v25 :: T_Productions_v25 
      v25 = \ !(T_Productions_vIn25 _lhsIaugmentsIn _lhsIforcedIrrefutables _lhsIinhMap _lhsInt _lhsIoptions _lhsIsynMap) -> (
         let !_hdX22 = Control.Monad.Identity.runIdentity (attach_T_Production (arg_hd_)) in
         let !_tlX24 = Control.Monad.Identity.runIdentity (attach_T_Productions (arg_tl_)) in
         let !_hdOnt = rule252 _lhsInt in
         let !_tlOnt = rule259 _lhsInt in
         let !_hdOaugmentsIn = rule248 _lhsIaugmentsIn in
         let !_hdOinhMap = rule250 _lhsIinhMap in
         let !_hdOoptions = rule253 _lhsIoptions in
         let !_hdOsynMap = rule254 _lhsIsynMap in
         let !_tlOaugmentsIn = rule255 _lhsIaugmentsIn in
         let !_tlOinhMap = rule257 _lhsIinhMap in
         let !_tlOoptions = rule260 _lhsIoptions in
         let !_tlOsynMap = rule261 _lhsIsynMap in
         let !_hdOforcedIrrefutables = rule249 _lhsIforcedIrrefutables in
         let !_tlOforcedIrrefutables = rule256 _lhsIforcedIrrefutables in
         let !(T_Production_vOut30 _hdIallAttributes _hdIaugmentsOut _hdIerrors _hdIoutput) = inv_Production_s22 _hdX22 K_Production_v30 (T_Production_vIn30 _hdOaugmentsIn _hdOforcedIrrefutables _hdOinhMap _hdOnt _hdOoptions _hdOsynMap) in
         let !(T_Productions_vOut25 _tlIallAttributes _tlIaugmentsOut _tlIerrors _tlIoutput) = inv_Productions_s24 _tlX24 K_Productions_v25 (T_Productions_vIn25 _tlOaugmentsIn _tlOforcedIrrefutables _tlOinhMap _tlOnt _tlOoptions _tlOsynMap) in
         let _lhsOallAttributes :: AttrMap
             !_lhsOallAttributes = rule243 _hdIallAttributes _tlIallAttributes in
         let _lhsOaugmentsOut :: Map ConstructorIdent (Map Identifier [Expression])
             !_lhsOaugmentsOut = rule244 _hdIaugmentsOut _tlIaugmentsOut in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule245 _hdIerrors _tlIerrors in
         let !_output = rule246 _hdIoutput _tlIoutput in
         let _lhsOoutput :: Productions
             !_lhsOoutput = rule247 _output in
         let !__result_ = T_Productions_vOut25 _lhsOallAttributes _lhsOaugmentsOut _lhsOerrors _lhsOoutput
          in __result_ )
      v39 :: T_Productions_v39 
      v39 = \ !(T_Productions_vIn39 _lhsInt) -> (
         let !_hdX22 = Control.Monad.Identity.runIdentity (attach_T_Production (arg_hd_)) in
         let !_tlX24 = Control.Monad.Identity.runIdentity (attach_T_Productions (arg_tl_)) in
         let !_hdOnt = rule252 _lhsInt in
         let !_tlOnt = rule259 _lhsInt in
         let !(T_Production_vOut43 _hdIallAttributes _hdX52) = inv_Production_s22 _hdX22 K_Production_v43 (T_Production_vIn43 _hdOnt) in
         let !(T_Productions_vOut39 _tlIallAttributes _tlX50) = inv_Productions_s24 _tlX24 K_Productions_v39 (T_Productions_vIn39 _tlOnt) in
         let _lhsOallAttributes :: AttrMap
             !_lhsOallAttributes = rule243 _hdIallAttributes _tlIallAttributes in
         let !__st_ = st50 _hdX52 _tlX50
             !__result_ = T_Productions_vOut39 _lhsOallAttributes __st_
          in __result_ )
     in C_Productions_s24 k24
   {-# NOINLINE st50 #-}
   st50 = \ !_hdX52 !_tlX50 -> let
      v40 :: T_Productions_v40 
      v40 = \ !(T_Productions_vIn40 _lhsIaugmentsIn _lhsIforcedIrrefutables _lhsIinhMap _lhsIoptions _lhsIsynMap) -> (
         let !_hdOaugmentsIn = rule248 _lhsIaugmentsIn in
         let !_hdOinhMap = rule250 _lhsIinhMap in
         let !_hdOoptions = rule253 _lhsIoptions in
         let !_hdOsynMap = rule254 _lhsIsynMap in
         let !_tlOaugmentsIn = rule255 _lhsIaugmentsIn in
         let !_tlOinhMap = rule257 _lhsIinhMap in
         let !_tlOoptions = rule260 _lhsIoptions in
         let !_tlOsynMap = rule261 _lhsIsynMap in
         let !_hdOforcedIrrefutables = rule249 _lhsIforcedIrrefutables in
         let !_tlOforcedIrrefutables = rule256 _lhsIforcedIrrefutables in
         let !(T_Production_vOut44 _hdIaugmentsOut _hdIerrors _hdIoutput) = inv_Production_s52 _hdX52 (T_Production_vIn44 _hdOaugmentsIn _hdOforcedIrrefutables _hdOinhMap _hdOoptions _hdOsynMap) in
         let !(T_Productions_vOut40 _tlIaugmentsOut _tlIerrors _tlIoutput) = inv_Productions_s50 _tlX50 (T_Productions_vIn40 _tlOaugmentsIn _tlOforcedIrrefutables _tlOinhMap _tlOoptions _tlOsynMap) in
         let _lhsOaugmentsOut :: Map ConstructorIdent (Map Identifier [Expression])
             !_lhsOaugmentsOut = rule244 _hdIaugmentsOut _tlIaugmentsOut in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule245 _hdIerrors _tlIerrors in
         let !_output = rule246 _hdIoutput _tlIoutput in
         let _lhsOoutput :: Productions
             !_lhsOoutput = rule247 _output in
         let !__result_ = T_Productions_vOut40 _lhsOaugmentsOut _lhsOerrors _lhsOoutput
          in __result_ )
     in C_Productions_s50 v40
   {-# NOINLINE[1] rule243 #-}
   rule243 = \ ((!_hdIallAttributes) :: AttrMap) ((!_tlIallAttributes) :: AttrMap) ->
     _hdIallAttributes `mergeAttributes` _tlIallAttributes
   {-# NOINLINE[1] rule244 #-}
   rule244 = \ ((!_hdIaugmentsOut) :: Map ConstructorIdent (Map Identifier [Expression])) ((!_tlIaugmentsOut) :: Map ConstructorIdent (Map Identifier [Expression])) ->
     _hdIaugmentsOut `Map.union` _tlIaugmentsOut
   {-# NOINLINE[1] rule245 #-}
   rule245 = \ ((!_hdIerrors) :: Seq Error) ((!_tlIerrors) :: Seq Error) ->
     _hdIerrors Seq.>< _tlIerrors
   {-# NOINLINE[1] rule246 #-}
   rule246 = \ ((!_hdIoutput) :: Production) ((!_tlIoutput) :: Productions) ->
     (:) _hdIoutput _tlIoutput
   {-# NOINLINE[1] rule247 #-}
   rule247 = \ !_output ->
     _output
   {-# NOINLINE[1] rule248 #-}
   rule248 = \ ((!_lhsIaugmentsIn) :: Map ConstructorIdent (Map Identifier [Expression])) ->
     _lhsIaugmentsIn
   {-# NOINLINE[1] rule249 #-}
   rule249 = \ ((!_lhsIforcedIrrefutables) :: AttrMap) ->
     _lhsIforcedIrrefutables
   {-# NOINLINE[1] rule250 #-}
   rule250 = \ ((!_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# NOINLINE[1] rule252 #-}
   rule252 = \ ((!_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# NOINLINE[1] rule253 #-}
   rule253 = \ ((!_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# NOINLINE[1] rule254 #-}
   rule254 = \ ((!_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
   {-# NOINLINE[1] rule255 #-}
   rule255 = \ ((!_lhsIaugmentsIn) :: Map ConstructorIdent (Map Identifier [Expression])) ->
     _lhsIaugmentsIn
   {-# NOINLINE[1] rule256 #-}
   rule256 = \ ((!_lhsIforcedIrrefutables) :: AttrMap) ->
     _lhsIforcedIrrefutables
   {-# NOINLINE[1] rule257 #-}
   rule257 = \ ((!_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# NOINLINE[1] rule259 #-}
   rule259 = \ ((!_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# NOINLINE[1] rule260 #-}
   rule260 = \ ((!_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# NOINLINE[1] rule261 #-}
   rule261 = \ ((!_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
{-# NOINLINE sem_Productions_Nil #-}
sem_Productions_Nil ::  T_Productions 
sem_Productions_Nil  = T_Productions (return st24) where
   {-# NOINLINE st24 #-}
   !st24 = let
      k24 :: K_Productions_s24  t -> t
      k24 K_Productions_v12 = v12
      k24 K_Productions_v25 = v25
      k24 K_Productions_v39 = v39
      v12 :: T_Productions_v12 
      v12 = \ !(T_Productions_vIn12 _lhsIaugmentsIn _lhsIforcedIrrefutables _lhsIinhMap _lhsImainName _lhsInt _lhsIoptions _lhsIsynMap) -> (
         let _lhsOallAttributes :: AttrMap
             !_lhsOallAttributes = rule262  () in
         let _lhsOaugmentsOut :: Map ConstructorIdent (Map Identifier [Expression])
             !_lhsOaugmentsOut = rule263  () in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule264  () in
         let !_output = rule265  () in
         let _lhsOoutput :: Productions
             !_lhsOoutput = rule266 _output in
         let !__result_ = T_Productions_vOut12 _lhsOallAttributes _lhsOaugmentsOut _lhsOerrors _lhsOoutput
          in __result_ )
      v25 :: T_Productions_v25 
      v25 = \ !(T_Productions_vIn25 _lhsIaugmentsIn _lhsIforcedIrrefutables _lhsIinhMap _lhsInt _lhsIoptions _lhsIsynMap) -> (
         let _lhsOallAttributes :: AttrMap
             !_lhsOallAttributes = rule262  () in
         let _lhsOaugmentsOut :: Map ConstructorIdent (Map Identifier [Expression])
             !_lhsOaugmentsOut = rule263  () in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule264  () in
         let !_output = rule265  () in
         let _lhsOoutput :: Productions
             !_lhsOoutput = rule266 _output in
         let !__result_ = T_Productions_vOut25 _lhsOallAttributes _lhsOaugmentsOut _lhsOerrors _lhsOoutput
          in __result_ )
      v39 :: T_Productions_v39 
      v39 = \ !(T_Productions_vIn39 _lhsInt) -> (
         let _lhsOallAttributes :: AttrMap
             !_lhsOallAttributes = rule262  () in
         let !__st_ = st50  ()
             !__result_ = T_Productions_vOut39 _lhsOallAttributes __st_
          in __result_ )
     in C_Productions_s24 k24
   {-# NOINLINE st50 #-}
   st50 = \  (_ :: ()) -> let
      v40 :: T_Productions_v40 
      v40 = \ !(T_Productions_vIn40 _lhsIaugmentsIn _lhsIforcedIrrefutables _lhsIinhMap _lhsIoptions _lhsIsynMap) -> (
         let _lhsOaugmentsOut :: Map ConstructorIdent (Map Identifier [Expression])
             !_lhsOaugmentsOut = rule263  () in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule264  () in
         let !_output = rule265  () in
         let _lhsOoutput :: Productions
             !_lhsOoutput = rule266 _output in
         let !__result_ = T_Productions_vOut40 _lhsOaugmentsOut _lhsOerrors _lhsOoutput
          in __result_ )
     in C_Productions_s50 v40
   {-# NOINLINE[1] rule262 #-}
   rule262 = \  (_ :: ()) ->
     Map.empty
   {-# NOINLINE[1] rule263 #-}
   rule263 = \  (_ :: ()) ->
     Map.empty
   {-# NOINLINE[1] rule264 #-}
   rule264 = \  (_ :: ()) ->
     Seq.empty
   {-# NOINLINE[1] rule265 #-}
   rule265 = \  (_ :: ()) ->
     []
   {-# NOINLINE[1] rule266 #-}
   rule266 = \ !_output ->
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
        let arg = T_Rule_vIn13 _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt _lhsIoptions
        !(T_Rule_vOut13 _lhsOallAttributes _lhsOdefsCollect _lhsOerrors _lhsOoutput) <- return (inv_Rule_s26 sem K_Rule_v13 arg)
        return (Syn_Rule _lhsOallAttributes _lhsOdefsCollect _lhsOerrors _lhsOoutput)
   )

-- cata
{-# INLINE sem_Rule #-}
sem_Rule :: Rule  -> T_Rule 
sem_Rule ( Rule !mbName_ pattern_ rhs_ !owrt_ !origin_ !explicit_ !pure_ !identity_ !mbError_ !eager_ ) = sem_Rule_Rule mbName_ ( sem_Pattern pattern_ ) ( sem_Expression rhs_ ) owrt_ origin_ explicit_ pure_ identity_ mbError_ eager_

-- semantic domain
newtype T_Rule  = T_Rule {
                         attach_T_Rule :: Identity (T_Rule_s26 )
                         }
data T_Rule_s26  where C_Rule_s26 :: {
                                     inv_Rule_s26 :: !(forall t. K_Rule_s26  t -> t)
                                     } -> T_Rule_s26 
data T_Rule_s27  = C_Rule_s27
newtype T_Rule_s48  = C_Rule_s48 {
                                 inv_Rule_s48 :: (T_Rule_v37 )
                                 }
newtype T_Rule_s57  = C_Rule_s57 {
                                 inv_Rule_s57 :: (T_Rule_v54 )
                                 }
data K_Rule_s26 k  where
   K_Rule_v13 :: K_Rule_s26  (T_Rule_v13 )
   K_Rule_v36 :: K_Rule_s26  (T_Rule_v36 )
   K_Rule_v53 :: K_Rule_s26  (T_Rule_v53 )
type T_Rule_v13  = (T_Rule_vIn13 ) -> (T_Rule_vOut13 )
data T_Rule_vIn13  = T_Rule_vIn13 !([(Identifier, Identifier)]) !([(Identifier, Identifier)]) !(ConstructorIdent) !(Set (Identifier, Identifier)) !(AttrMap) !(NontermIdent) !(Options)
data T_Rule_vOut13  = T_Rule_vOut13 !(AttrMap) !(Set (Identifier, Identifier)) !(Seq Error) !(Rule)
type T_Rule_v36  = (T_Rule_vIn36 ) -> (T_Rule_vOut36 )
data T_Rule_vIn36  = T_Rule_vIn36 !(ConstructorIdent) !(NontermIdent)
data T_Rule_vOut36  = T_Rule_vOut36 !(AttrMap) !(Set (Identifier, Identifier)) !(T_Rule_s48 )
type T_Rule_v37  = (T_Rule_vIn37 ) -> (T_Rule_vOut37 )
data T_Rule_vIn37  = T_Rule_vIn37 !([(Identifier, Identifier)]) !([(Identifier, Identifier)]) !(Set (Identifier, Identifier)) !(AttrMap) !(Options)
data T_Rule_vOut37  = T_Rule_vOut37 !(Seq Error) !(Rule)
type T_Rule_v53  = (T_Rule_vIn53 ) -> (T_Rule_vOut53 )
data T_Rule_vIn53  = T_Rule_vIn53 !(ConstructorIdent) !(NontermIdent)
data T_Rule_vOut53  = T_Rule_vOut53 !(AttrMap) !(T_Rule_s57 )
type T_Rule_v54  = (T_Rule_vIn54 ) -> (T_Rule_vOut54 )
data T_Rule_vIn54  = T_Rule_vIn54 
data T_Rule_vOut54  = T_Rule_vOut54 !(Set (Identifier, Identifier)) !(T_Rule_s48 )
{-# NOINLINE sem_Rule_Rule #-}
sem_Rule_Rule :: (Maybe Identifier) -> T_Pattern  -> T_Expression  -> (Bool) -> (String) -> (Bool) -> (Bool) -> (Bool) -> (Maybe Error) -> (Bool) -> T_Rule 
sem_Rule_Rule !arg_mbName_ arg_pattern_ arg_rhs_ !arg_owrt_ !arg_origin_ !arg_explicit_ !arg_pure_ !arg_identity_ !arg_mbError_ !arg_eager_ = T_Rule (return st26) where
   {-# NOINLINE st26 #-}
   !st26 = let
      k26 :: K_Rule_s26  t -> t
      k26 K_Rule_v13 = v13
      k26 K_Rule_v36 = v36
      k26 K_Rule_v53 = v53
      v13 :: T_Rule_v13 
      v13 = \ !(T_Rule_vIn13 _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt _lhsIoptions) -> (
         let !_patternX18 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pattern_)) in
         let !_rhsX4 = Control.Monad.Identity.runIdentity (attach_T_Expression (arg_rhs_)) in
         let !_patternOcon = rule275 _lhsIcon in
         let !_patternOnt = rule278 _lhsInt in
         let !_patternOchildInhs = rule273 _lhsIchildInhs in
         let !_patternOdefs = rule276 _lhsIdefs in
         let !_rhsOchildInhs = rule279 _lhsIchildInhs in
         let !_rhsOchildSyns = rule280 _lhsIchildSyns in
         let !_rhsOcon = rule281 _lhsIcon in
         let !_rhsOnt = rule282 _lhsInt in
         let !_rhsOoptions = rule283 _lhsIoptions in
         let !_patternOforcedIrrefutables = rule277 _lhsIforcedIrrefutables in
         let !(T_Pattern_vOut31 _patternIallAttributes _patternIdefsCollect _patternIerrors _patternIoutput) = inv_Pattern_s18 _patternX18 K_Pattern_v31 (T_Pattern_vIn31 _patternOchildInhs _patternOcon _patternOdefs _patternOforcedIrrefutables _patternOnt) in
         let _lhsOallAttributes :: AttrMap
             !_lhsOallAttributes = rule268 _patternIallAttributes in
         let _lhsOdefsCollect :: Set (Identifier, Identifier)
             !_lhsOdefsCollect = rule269 _patternIdefsCollect in
         let !_ruleDescr = rule267 _lhsIcon _lhsInt _patternIdefsCollect in
         let !_rhsOruleDescr = rule284 _ruleDescr in
         let !(T_Expression_vOut2 _rhsIerrors _rhsIoutput) = inv_Expression_s4 _rhsX4 (T_Expression_vIn2 _rhsOchildInhs _rhsOchildSyns _rhsOcon _rhsOnt _rhsOoptions _rhsOruleDescr) in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule270 _patternIerrors _rhsIerrors in
         let !_output = rule271 _patternIoutput _rhsIoutput arg_eager_ arg_explicit_ arg_identity_ arg_mbError_ arg_mbName_ arg_origin_ arg_owrt_ arg_pure_ in
         let _lhsOoutput :: Rule
             !_lhsOoutput = rule272 _output in
         let !__result_ = T_Rule_vOut13 _lhsOallAttributes _lhsOdefsCollect _lhsOerrors _lhsOoutput
          in __result_ )
      v36 :: T_Rule_v36 
      v36 = \ !(T_Rule_vIn36 _lhsIcon _lhsInt) -> (
         let !_patternX18 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pattern_)) in
         let !_patternOcon = rule275 _lhsIcon in
         let !_patternOnt = rule278 _lhsInt in
         let !(T_Pattern_vOut41 _patternIallAttributes _patternIdefsCollect _patternX51) = inv_Pattern_s18 _patternX18 K_Pattern_v41 (T_Pattern_vIn41 _patternOcon _patternOnt) in
         let _lhsOallAttributes :: AttrMap
             !_lhsOallAttributes = rule268 _patternIallAttributes in
         let _lhsOdefsCollect :: Set (Identifier, Identifier)
             !_lhsOdefsCollect = rule269 _patternIdefsCollect in
         let !__st_ = st48 _lhsIcon _lhsInt _patternIdefsCollect _patternX51
             !__result_ = T_Rule_vOut36 _lhsOallAttributes _lhsOdefsCollect __st_
          in __result_ )
      v53 :: T_Rule_v53 
      v53 = \ !(T_Rule_vIn53 _lhsIcon _lhsInt) -> (
         let !_patternX18 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pattern_)) in
         let !_patternOcon = rule275 _lhsIcon in
         let !_patternOnt = rule278 _lhsInt in
         let !(T_Pattern_vOut55 _patternIallAttributes _patternX58) = inv_Pattern_s18 _patternX18 K_Pattern_v55 (T_Pattern_vIn55 _patternOcon _patternOnt) in
         let _lhsOallAttributes :: AttrMap
             !_lhsOallAttributes = rule268 _patternIallAttributes in
         let !__st_ = st57 _lhsIcon _lhsInt _patternX58
             !__result_ = T_Rule_vOut53 _lhsOallAttributes __st_
          in __result_ )
     in C_Rule_s26 k26
   {-# NOINLINE st48 #-}
   st48 = \ ((!_lhsIcon) :: ConstructorIdent) ((!_lhsInt) :: NontermIdent) ((!_patternIdefsCollect) :: Set (Identifier, Identifier)) !_patternX51 -> let
      v37 :: T_Rule_v37 
      v37 = \ !(T_Rule_vIn37 _lhsIchildInhs _lhsIchildSyns _lhsIdefs _lhsIforcedIrrefutables _lhsIoptions) -> (
         let !_rhsOcon = rule281 _lhsIcon in
         let !_rhsOnt = rule282 _lhsInt in
         let !_ruleDescr = rule267 _lhsIcon _lhsInt _patternIdefsCollect in
         let !_rhsX4 = Control.Monad.Identity.runIdentity (attach_T_Expression (arg_rhs_)) in
         let !_patternOchildInhs = rule273 _lhsIchildInhs in
         let !_patternOdefs = rule276 _lhsIdefs in
         let !_rhsOchildInhs = rule279 _lhsIchildInhs in
         let !_rhsOchildSyns = rule280 _lhsIchildSyns in
         let !_rhsOoptions = rule283 _lhsIoptions in
         let !_rhsOruleDescr = rule284 _ruleDescr in
         let !_patternOforcedIrrefutables = rule277 _lhsIforcedIrrefutables in
         let !(T_Pattern_vOut42 _patternIerrors _patternIoutput) = inv_Pattern_s51 _patternX51 (T_Pattern_vIn42 _patternOchildInhs _patternOdefs _patternOforcedIrrefutables) in
         let !(T_Expression_vOut2 _rhsIerrors _rhsIoutput) = inv_Expression_s4 _rhsX4 (T_Expression_vIn2 _rhsOchildInhs _rhsOchildSyns _rhsOcon _rhsOnt _rhsOoptions _rhsOruleDescr) in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule270 _patternIerrors _rhsIerrors in
         let !_output = rule271 _patternIoutput _rhsIoutput arg_eager_ arg_explicit_ arg_identity_ arg_mbError_ arg_mbName_ arg_origin_ arg_owrt_ arg_pure_ in
         let _lhsOoutput :: Rule
             !_lhsOoutput = rule272 _output in
         let !__result_ = T_Rule_vOut37 _lhsOerrors _lhsOoutput
          in __result_ )
     in C_Rule_s48 v37
   {-# NOINLINE st57 #-}
   st57 = \ ((!_lhsIcon) :: ConstructorIdent) ((!_lhsInt) :: NontermIdent) !_patternX58 -> let
      v54 :: T_Rule_v54 
      v54 = \ !(T_Rule_vIn54 ) -> (
         let !(T_Pattern_vOut56 _patternIdefsCollect _patternX51) = inv_Pattern_s58 _patternX58 (T_Pattern_vIn56 ) in
         let _lhsOdefsCollect :: Set (Identifier, Identifier)
             !_lhsOdefsCollect = rule269 _patternIdefsCollect in
         let !__st_ = st48 _lhsIcon _lhsInt _patternIdefsCollect _patternX51
             !__result_ = T_Rule_vOut54 _lhsOdefsCollect __st_
          in __result_ )
     in C_Rule_s57 v54
   {-# NOINLINE[1] rule267 #-}
   {-# LINE 172 "./src-ag/Desugar.ag" #-}
   rule267 = \ ((!_lhsIcon) :: ConstructorIdent) ((!_lhsInt) :: NontermIdent) ((!_patternIdefsCollect) :: Set (Identifier, Identifier)) ->
                        {-# LINE 172 "./src-ag/Desugar.ag" #-}
                        show _lhsInt ++ " :: " ++ show _lhsIcon ++ " :: " ++ (concat $ intersperse "," $ map (\(f,a) -> show f ++ "." ++ show a) $ Set.toList _patternIdefsCollect)
                        {-# LINE 4003 "dist/build/Desugar.hs"#-}
   {-# NOINLINE[1] rule268 #-}
   rule268 = \ ((!_patternIallAttributes) :: AttrMap) ->
     _patternIallAttributes
   {-# NOINLINE[1] rule269 #-}
   rule269 = \ ((!_patternIdefsCollect) :: Set (Identifier, Identifier)) ->
     _patternIdefsCollect
   {-# NOINLINE[1] rule270 #-}
   rule270 = \ ((!_patternIerrors) :: Seq Error) ((!_rhsIerrors) :: Seq Error) ->
     _patternIerrors Seq.>< _rhsIerrors
   {-# NOINLINE[1] rule271 #-}
   rule271 = \ ((!_patternIoutput) :: Pattern) ((!_rhsIoutput) :: Expression) !eager_ !explicit_ !identity_ !mbError_ !mbName_ !origin_ !owrt_ !pure_ ->
     Rule mbName_ _patternIoutput _rhsIoutput owrt_ origin_ explicit_ pure_ identity_ mbError_ eager_
   {-# NOINLINE[1] rule272 #-}
   rule272 = \ !_output ->
     _output
   {-# NOINLINE[1] rule273 #-}
   rule273 = \ ((!_lhsIchildInhs) :: [(Identifier, Identifier)]) ->
     _lhsIchildInhs
   {-# NOINLINE[1] rule275 #-}
   rule275 = \ ((!_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# NOINLINE[1] rule276 #-}
   rule276 = \ ((!_lhsIdefs) :: Set (Identifier, Identifier)) ->
     _lhsIdefs
   {-# NOINLINE[1] rule277 #-}
   rule277 = \ ((!_lhsIforcedIrrefutables) :: AttrMap) ->
     _lhsIforcedIrrefutables
   {-# NOINLINE[1] rule278 #-}
   rule278 = \ ((!_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# NOINLINE[1] rule279 #-}
   rule279 = \ ((!_lhsIchildInhs) :: [(Identifier, Identifier)]) ->
     _lhsIchildInhs
   {-# NOINLINE[1] rule280 #-}
   rule280 = \ ((!_lhsIchildSyns) :: [(Identifier, Identifier)]) ->
     _lhsIchildSyns
   {-# NOINLINE[1] rule281 #-}
   rule281 = \ ((!_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# NOINLINE[1] rule282 #-}
   rule282 = \ ((!_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# NOINLINE[1] rule283 #-}
   rule283 = \ ((!_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# NOINLINE[1] rule284 #-}
   rule284 = \ !_ruleDescr ->
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
        let arg = T_Rules_vIn14 _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt _lhsIoptions
        !(T_Rules_vOut14 _lhsOallAttributes _lhsOdefsCollect _lhsOerrors _lhsOoutput) <- return (inv_Rules_s28 sem K_Rules_v14 arg)
        return (Syn_Rules _lhsOallAttributes _lhsOdefsCollect _lhsOerrors _lhsOoutput)
   )

-- cata
{-# NOINLINE sem_Rules #-}
sem_Rules :: Rules  -> T_Rules 
sem_Rules list = Prelude.foldr sem_Rules_Cons sem_Rules_Nil (Prelude.map sem_Rule list)

-- semantic domain
newtype T_Rules  = T_Rules {
                           attach_T_Rules :: Identity (T_Rules_s28 )
                           }
data T_Rules_s28  where C_Rules_s28 :: {
                                       inv_Rules_s28 :: !(forall t. K_Rules_s28  t -> t)
                                       } -> T_Rules_s28 
data T_Rules_s29  = C_Rules_s29
newtype T_Rules_s43  = C_Rules_s43 {
                                   inv_Rules_s43 :: (T_Rules_v29 )
                                   }
newtype T_Rules_s55  = C_Rules_s55 {
                                   inv_Rules_s55 :: (T_Rules_v50 )
                                   }
data K_Rules_s28 k  where
   K_Rules_v14 :: K_Rules_s28  (T_Rules_v14 )
   K_Rules_v28 :: K_Rules_s28  (T_Rules_v28 )
   K_Rules_v49 :: K_Rules_s28  (T_Rules_v49 )
type T_Rules_v14  = (T_Rules_vIn14 ) -> (T_Rules_vOut14 )
data T_Rules_vIn14  = T_Rules_vIn14 !([(Identifier, Identifier)]) !([(Identifier, Identifier)]) !(ConstructorIdent) !(Set (Identifier, Identifier)) !(AttrMap) !(NontermIdent) !(Options)
data T_Rules_vOut14  = T_Rules_vOut14 !(AttrMap) !(Set (Identifier, Identifier)) !(Seq Error) !(Rules)
type T_Rules_v28  = (T_Rules_vIn28 ) -> (T_Rules_vOut28 )
data T_Rules_vIn28  = T_Rules_vIn28 !(ConstructorIdent) !(NontermIdent)
data T_Rules_vOut28  = T_Rules_vOut28 !(AttrMap) !(Set (Identifier, Identifier)) !(T_Rules_s43 )
type T_Rules_v29  = (T_Rules_vIn29 ) -> (T_Rules_vOut29 )
data T_Rules_vIn29  = T_Rules_vIn29 !([(Identifier, Identifier)]) !([(Identifier, Identifier)]) !(Set (Identifier, Identifier)) !(AttrMap) !(Options)
data T_Rules_vOut29  = T_Rules_vOut29 !(Seq Error) !(Rules)
type T_Rules_v49  = (T_Rules_vIn49 ) -> (T_Rules_vOut49 )
data T_Rules_vIn49  = T_Rules_vIn49 !(ConstructorIdent) !(NontermIdent)
data T_Rules_vOut49  = T_Rules_vOut49 !(AttrMap) !(T_Rules_s55 )
type T_Rules_v50  = (T_Rules_vIn50 ) -> (T_Rules_vOut50 )
data T_Rules_vIn50  = T_Rules_vIn50 
data T_Rules_vOut50  = T_Rules_vOut50 !(Set (Identifier, Identifier)) !(T_Rules_s43 )
{-# NOINLINE sem_Rules_Cons #-}
sem_Rules_Cons :: T_Rule  -> T_Rules  -> T_Rules 
sem_Rules_Cons arg_hd_ arg_tl_ = T_Rules (return st28) where
   {-# NOINLINE st28 #-}
   !st28 = let
      k28 :: K_Rules_s28  t -> t
      k28 K_Rules_v14 = v14
      k28 K_Rules_v28 = v28
      k28 K_Rules_v49 = v49
      v14 :: T_Rules_v14 
      v14 = \ !(T_Rules_vIn14 _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt _lhsIoptions) -> (
         let !_hdX26 = Control.Monad.Identity.runIdentity (attach_T_Rule (arg_hd_)) in
         let !_tlX28 = Control.Monad.Identity.runIdentity (attach_T_Rules (arg_tl_)) in
         let !_hdOcon = rule292 _lhsIcon in
         let !_hdOnt = rule295 _lhsInt in
         let !_tlOcon = rule299 _lhsIcon in
         let !_tlOnt = rule302 _lhsInt in
         let !_hdOchildInhs = rule290 _lhsIchildInhs in
         let !_hdOchildSyns = rule291 _lhsIchildSyns in
         let !_hdOdefs = rule293 _lhsIdefs in
         let !_hdOoptions = rule296 _lhsIoptions in
         let !_tlOchildInhs = rule297 _lhsIchildInhs in
         let !_tlOchildSyns = rule298 _lhsIchildSyns in
         let !_tlOdefs = rule300 _lhsIdefs in
         let !_tlOoptions = rule303 _lhsIoptions in
         let !_hdOforcedIrrefutables = rule294 _lhsIforcedIrrefutables in
         let !_tlOforcedIrrefutables = rule301 _lhsIforcedIrrefutables in
         let !(T_Rule_vOut13 _hdIallAttributes _hdIdefsCollect _hdIerrors _hdIoutput) = inv_Rule_s26 _hdX26 K_Rule_v13 (T_Rule_vIn13 _hdOchildInhs _hdOchildSyns _hdOcon _hdOdefs _hdOforcedIrrefutables _hdOnt _hdOoptions) in
         let !(T_Rules_vOut14 _tlIallAttributes _tlIdefsCollect _tlIerrors _tlIoutput) = inv_Rules_s28 _tlX28 K_Rules_v14 (T_Rules_vIn14 _tlOchildInhs _tlOchildSyns _tlOcon _tlOdefs _tlOforcedIrrefutables _tlOnt _tlOoptions) in
         let _lhsOallAttributes :: AttrMap
             !_lhsOallAttributes = rule285 _hdIallAttributes _tlIallAttributes in
         let _lhsOdefsCollect :: Set (Identifier, Identifier)
             !_lhsOdefsCollect = rule286 _hdIdefsCollect _tlIdefsCollect in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule287 _hdIerrors _tlIerrors in
         let !_output = rule288 _hdIoutput _tlIoutput in
         let _lhsOoutput :: Rules
             !_lhsOoutput = rule289 _output in
         let !__result_ = T_Rules_vOut14 _lhsOallAttributes _lhsOdefsCollect _lhsOerrors _lhsOoutput
          in __result_ )
      v28 :: T_Rules_v28 
      v28 = \ !(T_Rules_vIn28 _lhsIcon _lhsInt) -> (
         let !_hdX26 = Control.Monad.Identity.runIdentity (attach_T_Rule (arg_hd_)) in
         let !_tlX28 = Control.Monad.Identity.runIdentity (attach_T_Rules (arg_tl_)) in
         let !_hdOcon = rule292 _lhsIcon in
         let !_hdOnt = rule295 _lhsInt in
         let !_tlOcon = rule299 _lhsIcon in
         let !_tlOnt = rule302 _lhsInt in
         let !(T_Rule_vOut36 _hdIallAttributes _hdIdefsCollect _hdX48) = inv_Rule_s26 _hdX26 K_Rule_v36 (T_Rule_vIn36 _hdOcon _hdOnt) in
         let !(T_Rules_vOut28 _tlIallAttributes _tlIdefsCollect _tlX43) = inv_Rules_s28 _tlX28 K_Rules_v28 (T_Rules_vIn28 _tlOcon _tlOnt) in
         let _lhsOallAttributes :: AttrMap
             !_lhsOallAttributes = rule285 _hdIallAttributes _tlIallAttributes in
         let _lhsOdefsCollect :: Set (Identifier, Identifier)
             !_lhsOdefsCollect = rule286 _hdIdefsCollect _tlIdefsCollect in
         let !__st_ = st43 _hdX48 _tlX43
             !__result_ = T_Rules_vOut28 _lhsOallAttributes _lhsOdefsCollect __st_
          in __result_ )
      v49 :: T_Rules_v49 
      v49 = \ !(T_Rules_vIn49 _lhsIcon _lhsInt) -> (
         let !_hdX26 = Control.Monad.Identity.runIdentity (attach_T_Rule (arg_hd_)) in
         let !_tlX28 = Control.Monad.Identity.runIdentity (attach_T_Rules (arg_tl_)) in
         let !_hdOcon = rule292 _lhsIcon in
         let !_hdOnt = rule295 _lhsInt in
         let !_tlOcon = rule299 _lhsIcon in
         let !_tlOnt = rule302 _lhsInt in
         let !(T_Rule_vOut53 _hdIallAttributes _hdX57) = inv_Rule_s26 _hdX26 K_Rule_v53 (T_Rule_vIn53 _hdOcon _hdOnt) in
         let !(T_Rules_vOut49 _tlIallAttributes _tlX55) = inv_Rules_s28 _tlX28 K_Rules_v49 (T_Rules_vIn49 _tlOcon _tlOnt) in
         let _lhsOallAttributes :: AttrMap
             !_lhsOallAttributes = rule285 _hdIallAttributes _tlIallAttributes in
         let !__st_ = st55 _hdX57 _tlX55
             !__result_ = T_Rules_vOut49 _lhsOallAttributes __st_
          in __result_ )
     in C_Rules_s28 k28
   {-# NOINLINE st43 #-}
   st43 = \ !_hdX48 !_tlX43 -> let
      v29 :: T_Rules_v29 
      v29 = \ !(T_Rules_vIn29 _lhsIchildInhs _lhsIchildSyns _lhsIdefs _lhsIforcedIrrefutables _lhsIoptions) -> (
         let !_hdOchildInhs = rule290 _lhsIchildInhs in
         let !_hdOchildSyns = rule291 _lhsIchildSyns in
         let !_hdOdefs = rule293 _lhsIdefs in
         let !_hdOoptions = rule296 _lhsIoptions in
         let !_tlOchildInhs = rule297 _lhsIchildInhs in
         let !_tlOchildSyns = rule298 _lhsIchildSyns in
         let !_tlOdefs = rule300 _lhsIdefs in
         let !_tlOoptions = rule303 _lhsIoptions in
         let !_hdOforcedIrrefutables = rule294 _lhsIforcedIrrefutables in
         let !_tlOforcedIrrefutables = rule301 _lhsIforcedIrrefutables in
         let !(T_Rule_vOut37 _hdIerrors _hdIoutput) = inv_Rule_s48 _hdX48 (T_Rule_vIn37 _hdOchildInhs _hdOchildSyns _hdOdefs _hdOforcedIrrefutables _hdOoptions) in
         let !(T_Rules_vOut29 _tlIerrors _tlIoutput) = inv_Rules_s43 _tlX43 (T_Rules_vIn29 _tlOchildInhs _tlOchildSyns _tlOdefs _tlOforcedIrrefutables _tlOoptions) in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule287 _hdIerrors _tlIerrors in
         let !_output = rule288 _hdIoutput _tlIoutput in
         let _lhsOoutput :: Rules
             !_lhsOoutput = rule289 _output in
         let !__result_ = T_Rules_vOut29 _lhsOerrors _lhsOoutput
          in __result_ )
     in C_Rules_s43 v29
   {-# NOINLINE st55 #-}
   st55 = \ !_hdX57 !_tlX55 -> let
      v50 :: T_Rules_v50 
      v50 = \ !(T_Rules_vIn50 ) -> (
         let !(T_Rule_vOut54 _hdIdefsCollect _hdX48) = inv_Rule_s57 _hdX57 (T_Rule_vIn54 ) in
         let !(T_Rules_vOut50 _tlIdefsCollect _tlX43) = inv_Rules_s55 _tlX55 (T_Rules_vIn50 ) in
         let _lhsOdefsCollect :: Set (Identifier, Identifier)
             !_lhsOdefsCollect = rule286 _hdIdefsCollect _tlIdefsCollect in
         let !__st_ = st43 _hdX48 _tlX43
             !__result_ = T_Rules_vOut50 _lhsOdefsCollect __st_
          in __result_ )
     in C_Rules_s55 v50
   {-# NOINLINE[1] rule285 #-}
   rule285 = \ ((!_hdIallAttributes) :: AttrMap) ((!_tlIallAttributes) :: AttrMap) ->
     _hdIallAttributes `mergeAttributes` _tlIallAttributes
   {-# NOINLINE[1] rule286 #-}
   rule286 = \ ((!_hdIdefsCollect) :: Set (Identifier, Identifier)) ((!_tlIdefsCollect) :: Set (Identifier, Identifier)) ->
     _hdIdefsCollect `Set.union` _tlIdefsCollect
   {-# NOINLINE[1] rule287 #-}
   rule287 = \ ((!_hdIerrors) :: Seq Error) ((!_tlIerrors) :: Seq Error) ->
     _hdIerrors Seq.>< _tlIerrors
   {-# NOINLINE[1] rule288 #-}
   rule288 = \ ((!_hdIoutput) :: Rule) ((!_tlIoutput) :: Rules) ->
     (:) _hdIoutput _tlIoutput
   {-# NOINLINE[1] rule289 #-}
   rule289 = \ !_output ->
     _output
   {-# NOINLINE[1] rule290 #-}
   rule290 = \ ((!_lhsIchildInhs) :: [(Identifier, Identifier)]) ->
     _lhsIchildInhs
   {-# NOINLINE[1] rule291 #-}
   rule291 = \ ((!_lhsIchildSyns) :: [(Identifier, Identifier)]) ->
     _lhsIchildSyns
   {-# NOINLINE[1] rule292 #-}
   rule292 = \ ((!_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# NOINLINE[1] rule293 #-}
   rule293 = \ ((!_lhsIdefs) :: Set (Identifier, Identifier)) ->
     _lhsIdefs
   {-# NOINLINE[1] rule294 #-}
   rule294 = \ ((!_lhsIforcedIrrefutables) :: AttrMap) ->
     _lhsIforcedIrrefutables
   {-# NOINLINE[1] rule295 #-}
   rule295 = \ ((!_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# NOINLINE[1] rule296 #-}
   rule296 = \ ((!_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# NOINLINE[1] rule297 #-}
   rule297 = \ ((!_lhsIchildInhs) :: [(Identifier, Identifier)]) ->
     _lhsIchildInhs
   {-# NOINLINE[1] rule298 #-}
   rule298 = \ ((!_lhsIchildSyns) :: [(Identifier, Identifier)]) ->
     _lhsIchildSyns
   {-# NOINLINE[1] rule299 #-}
   rule299 = \ ((!_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# NOINLINE[1] rule300 #-}
   rule300 = \ ((!_lhsIdefs) :: Set (Identifier, Identifier)) ->
     _lhsIdefs
   {-# NOINLINE[1] rule301 #-}
   rule301 = \ ((!_lhsIforcedIrrefutables) :: AttrMap) ->
     _lhsIforcedIrrefutables
   {-# NOINLINE[1] rule302 #-}
   rule302 = \ ((!_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# NOINLINE[1] rule303 #-}
   rule303 = \ ((!_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Rules_Nil #-}
sem_Rules_Nil ::  T_Rules 
sem_Rules_Nil  = T_Rules (return st28) where
   {-# NOINLINE st28 #-}
   !st28 = let
      k28 :: K_Rules_s28  t -> t
      k28 K_Rules_v14 = v14
      k28 K_Rules_v28 = v28
      k28 K_Rules_v49 = v49
      v14 :: T_Rules_v14 
      v14 = \ !(T_Rules_vIn14 _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt _lhsIoptions) -> (
         let _lhsOallAttributes :: AttrMap
             !_lhsOallAttributes = rule304  () in
         let _lhsOdefsCollect :: Set (Identifier, Identifier)
             !_lhsOdefsCollect = rule305  () in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule306  () in
         let !_output = rule307  () in
         let _lhsOoutput :: Rules
             !_lhsOoutput = rule308 _output in
         let !__result_ = T_Rules_vOut14 _lhsOallAttributes _lhsOdefsCollect _lhsOerrors _lhsOoutput
          in __result_ )
      v28 :: T_Rules_v28 
      v28 = \ !(T_Rules_vIn28 _lhsIcon _lhsInt) -> (
         let _lhsOallAttributes :: AttrMap
             !_lhsOallAttributes = rule304  () in
         let _lhsOdefsCollect :: Set (Identifier, Identifier)
             !_lhsOdefsCollect = rule305  () in
         let !__st_ = st43  ()
             !__result_ = T_Rules_vOut28 _lhsOallAttributes _lhsOdefsCollect __st_
          in __result_ )
      v49 :: T_Rules_v49 
      v49 = \ !(T_Rules_vIn49 _lhsIcon _lhsInt) -> (
         let _lhsOallAttributes :: AttrMap
             !_lhsOallAttributes = rule304  () in
         let !__st_ = st55  ()
             !__result_ = T_Rules_vOut49 _lhsOallAttributes __st_
          in __result_ )
     in C_Rules_s28 k28
   {-# NOINLINE st43 #-}
   st43 = \  (_ :: ()) -> let
      v29 :: T_Rules_v29 
      v29 = \ !(T_Rules_vIn29 _lhsIchildInhs _lhsIchildSyns _lhsIdefs _lhsIforcedIrrefutables _lhsIoptions) -> (
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule306  () in
         let !_output = rule307  () in
         let _lhsOoutput :: Rules
             !_lhsOoutput = rule308 _output in
         let !__result_ = T_Rules_vOut29 _lhsOerrors _lhsOoutput
          in __result_ )
     in C_Rules_s43 v29
   {-# NOINLINE st55 #-}
   st55 = \  (_ :: ()) -> let
      v50 :: T_Rules_v50 
      v50 = \ !(T_Rules_vIn50 ) -> (
         let _lhsOdefsCollect :: Set (Identifier, Identifier)
             !_lhsOdefsCollect = rule305  () in
         let !__st_ = st43  ()
             !__result_ = T_Rules_vOut50 _lhsOdefsCollect __st_
          in __result_ )
     in C_Rules_s55 v50
   {-# NOINLINE[1] rule304 #-}
   rule304 = \  (_ :: ()) ->
     Map.empty
   {-# NOINLINE[1] rule305 #-}
   rule305 = \  (_ :: ()) ->
     Set.empty
   {-# NOINLINE[1] rule306 #-}
   rule306 = \  (_ :: ()) ->
     Seq.empty
   {-# NOINLINE[1] rule307 #-}
   rule307 = \  (_ :: ()) ->
     []
   {-# NOINLINE[1] rule308 #-}
   rule308 = \ !_output ->
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
        let arg = T_TypeSig_vIn15 
        !(T_TypeSig_vOut15 _lhsOoutput) <- return (inv_TypeSig_s30 sem arg)
        return (Syn_TypeSig _lhsOoutput)
   )

-- cata
{-# INLINE sem_TypeSig #-}
sem_TypeSig :: TypeSig  -> T_TypeSig 
sem_TypeSig ( TypeSig !name_ !tp_ ) = sem_TypeSig_TypeSig name_ tp_

-- semantic domain
newtype T_TypeSig  = T_TypeSig {
                               attach_T_TypeSig :: Identity (T_TypeSig_s30 )
                               }
newtype T_TypeSig_s30  = C_TypeSig_s30 {
                                       inv_TypeSig_s30 :: (T_TypeSig_v15 )
                                       }
data T_TypeSig_s31  = C_TypeSig_s31
type T_TypeSig_v15  = (T_TypeSig_vIn15 ) -> (T_TypeSig_vOut15 )
data T_TypeSig_vIn15  = T_TypeSig_vIn15 
data T_TypeSig_vOut15  = T_TypeSig_vOut15 !(TypeSig)
{-# NOINLINE sem_TypeSig_TypeSig #-}
sem_TypeSig_TypeSig :: (Identifier) -> (Type) -> T_TypeSig 
sem_TypeSig_TypeSig !arg_name_ !arg_tp_ = T_TypeSig (return st30) where
   {-# NOINLINE st30 #-}
   !st30 = let
      v15 :: T_TypeSig_v15 
      v15 = \ !(T_TypeSig_vIn15 ) -> (
         let !_output = rule309 arg_name_ arg_tp_ in
         let _lhsOoutput :: TypeSig
             !_lhsOoutput = rule310 _output in
         let !__result_ = T_TypeSig_vOut15 _lhsOoutput
          in __result_ )
     in C_TypeSig_s30 v15
   {-# INLINE rule309 #-}
   rule309 = \ !name_ !tp_ ->
     TypeSig name_ tp_
   {-# INLINE rule310 #-}
   rule310 = \ !_output ->
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
        let arg = T_TypeSigs_vIn16 
        !(T_TypeSigs_vOut16 _lhsOoutput) <- return (inv_TypeSigs_s32 sem arg)
        return (Syn_TypeSigs _lhsOoutput)
   )

-- cata
{-# NOINLINE sem_TypeSigs #-}
sem_TypeSigs :: TypeSigs  -> T_TypeSigs 
sem_TypeSigs list = Prelude.foldr sem_TypeSigs_Cons sem_TypeSigs_Nil (Prelude.map sem_TypeSig list)

-- semantic domain
newtype T_TypeSigs  = T_TypeSigs {
                                 attach_T_TypeSigs :: Identity (T_TypeSigs_s32 )
                                 }
newtype T_TypeSigs_s32  = C_TypeSigs_s32 {
                                         inv_TypeSigs_s32 :: (T_TypeSigs_v16 )
                                         }
data T_TypeSigs_s33  = C_TypeSigs_s33
type T_TypeSigs_v16  = (T_TypeSigs_vIn16 ) -> (T_TypeSigs_vOut16 )
data T_TypeSigs_vIn16  = T_TypeSigs_vIn16 
data T_TypeSigs_vOut16  = T_TypeSigs_vOut16 !(TypeSigs)
{-# NOINLINE sem_TypeSigs_Cons #-}
sem_TypeSigs_Cons :: T_TypeSig  -> T_TypeSigs  -> T_TypeSigs 
sem_TypeSigs_Cons arg_hd_ arg_tl_ = T_TypeSigs (return st32) where
   {-# NOINLINE st32 #-}
   !st32 = let
      v16 :: T_TypeSigs_v16 
      v16 = \ !(T_TypeSigs_vIn16 ) -> (
         let !_hdX30 = Control.Monad.Identity.runIdentity (attach_T_TypeSig (arg_hd_)) in
         let !_tlX32 = Control.Monad.Identity.runIdentity (attach_T_TypeSigs (arg_tl_)) in
         let !(T_TypeSig_vOut15 _hdIoutput) = inv_TypeSig_s30 _hdX30 (T_TypeSig_vIn15 ) in
         let !(T_TypeSigs_vOut16 _tlIoutput) = inv_TypeSigs_s32 _tlX32 (T_TypeSigs_vIn16 ) in
         let !_output = rule311 _hdIoutput _tlIoutput in
         let _lhsOoutput :: TypeSigs
             !_lhsOoutput = rule312 _output in
         let !__result_ = T_TypeSigs_vOut16 _lhsOoutput
          in __result_ )
     in C_TypeSigs_s32 v16
   {-# INLINE rule311 #-}
   rule311 = \ ((!_hdIoutput) :: TypeSig) ((!_tlIoutput) :: TypeSigs) ->
     (:) _hdIoutput _tlIoutput
   {-# INLINE rule312 #-}
   rule312 = \ !_output ->
     _output
{-# NOINLINE sem_TypeSigs_Nil #-}
sem_TypeSigs_Nil ::  T_TypeSigs 
sem_TypeSigs_Nil  = T_TypeSigs (return st32) where
   {-# NOINLINE st32 #-}
   !st32 = let
      v16 :: T_TypeSigs_v16 
      v16 = \ !(T_TypeSigs_vIn16 ) -> (
         let !_output = rule313  () in
         let _lhsOoutput :: TypeSigs
             !_lhsOoutput = rule314 _output in
         let !__result_ = T_TypeSigs_vOut16 _lhsOoutput
          in __result_ )
     in C_TypeSigs_s32 v16
   {-# INLINE rule313 #-}
   rule313 = \  (_ :: ()) ->
     []
   {-# INLINE rule314 #-}
   rule314 = \ !_output ->
     _output
