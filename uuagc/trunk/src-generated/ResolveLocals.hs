{-# LANGUAGE Rank2Types, GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ResolveLocals where
{-# LINE 2 "./src-ag/Expression.ag" #-}

import UU.Scanner.Position(Pos)
import HsToken
{-# LINE 10 "dist/build/ResolveLocals.hs" #-}

{-# LINE 2 "./src-ag/Patterns.ag" #-}

-- Patterns.ag imports
import UU.Scanner.Position(Pos)
import CommonTypes (ConstructorIdent,Identifier)
{-# LINE 17 "dist/build/ResolveLocals.hs" #-}

{-# LINE 2 "./src-ag/AbstractSyntax.ag" #-}

-- AbstractSyntax.ag imports
import Data.Set(Set)
import Data.Map(Map)
import Patterns    (Pattern(..),Patterns)
import Expression  (Expression(..))
import Macro --marcos
import CommonTypes
import ErrorMessages
{-# LINE 29 "dist/build/ResolveLocals.hs" #-}

{-# LINE 15 "./src-ag/ResolveLocals.ag" #-}

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Map(Map)
import qualified Data.Sequence as Seq
import Data.Sequence(Seq,(><))
import CommonTypes
import Patterns
import ErrorMessages
import AbstractSyntax
import Expression
import Options
import HsToken(HsTokensRoot(HsTokensRoot))
import SemHsTokens(sem_HsTokensRoot,wrap_HsTokensRoot, Syn_HsTokensRoot(..),Inh_HsTokensRoot(..))
import Data.Maybe
{-# LINE 47 "dist/build/ResolveLocals.hs" #-}
import Control.Monad.Identity (Identity)
import qualified Control.Monad.Identity
-- Child -------------------------------------------------------
-- wrapper
data Inh_Child  = Inh_Child { allfields_Inh_Child :: ([(Identifier,Type,ChildKind)]), allnts_Inh_Child :: ([Identifier]), attrs_Inh_Child :: ([(Identifier,Identifier)]), con_Inh_Child :: (Identifier), inh_Inh_Child :: (Attributes), inhMap_Inh_Child :: (Map Identifier Attributes), mergeMap_Inh_Child :: (Map Identifier (Identifier,[Identifier])), nt_Inh_Child :: (Identifier), syn_Inh_Child :: (Attributes), synMap_Inh_Child :: (Map Identifier Attributes) }
data Syn_Child  = Syn_Child { attributes_Syn_Child :: ([(Identifier,Attributes,Attributes)]), field_Syn_Child :: ((Identifier,Type,ChildKind)), output_Syn_Child :: (Child) }
{-# INLINABLE wrap_Child #-}
wrap_Child :: T_Child  -> Inh_Child  -> (Syn_Child )
wrap_Child (T_Child act) (Inh_Child _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsIinh _lhsIinhMap _lhsImergeMap _lhsInt _lhsIsyn _lhsIsynMap) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Child_vIn1 _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsIinh _lhsIinhMap _lhsImergeMap _lhsInt _lhsIsyn _lhsIsynMap
        (T_Child_vOut1 _lhsOattributes _lhsOfield _lhsOoutput) <- return (inv_Child_s2 sem arg)
        return (Syn_Child _lhsOattributes _lhsOfield _lhsOoutput)
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
data T_Child_vIn1  = T_Child_vIn1 ([(Identifier,Type,ChildKind)]) ([Identifier]) ([(Identifier,Identifier)]) (Identifier) (Attributes) (Map Identifier Attributes) (Map Identifier (Identifier,[Identifier])) (Identifier) (Attributes) (Map Identifier Attributes)
data T_Child_vOut1  = T_Child_vOut1 ([(Identifier,Attributes,Attributes)]) ((Identifier,Type,ChildKind)) (Child)
{-# NOINLINE sem_Child_Child #-}
sem_Child_Child :: (Identifier) -> (Type) -> (ChildKind) -> T_Child 
sem_Child_Child arg_name_ arg_tp_ arg_kind_ = T_Child (return st2) where
   {-# NOINLINE st2 #-}
   st2 = let
      v1 :: T_Child_v1 
      v1 = \ (T_Child_vIn1 _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsIinh _lhsIinhMap _lhsImergeMap _lhsInt _lhsIsyn _lhsIsynMap) -> ( let
         _chnt = rule0 arg_name_ arg_tp_
         _inh = rule1 _chnt _lhsIinhMap
         _syn = rule2 _chnt _lhsIsynMap
         _lhsOattributes :: [(Identifier,Attributes,Attributes)]
         _lhsOattributes = rule3 _inh _syn arg_name_
         _lhsOfield :: (Identifier,Type,ChildKind)
         _lhsOfield = rule4 arg_kind_ arg_name_ arg_tp_
         _output = rule5 arg_kind_ arg_name_ arg_tp_
         _lhsOoutput :: Child
         _lhsOoutput = rule6 _output
         __result_ = T_Child_vOut1 _lhsOattributes _lhsOfield _lhsOoutput
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
                       {-# LINE 108 "dist/build/ResolveLocals.hs"#-}
   {-# INLINE rule1 #-}
   {-# LINE 23 "./src-ag/DistChildAttr.ag" #-}
   rule1 = \ _chnt ((_lhsIinhMap) :: Map Identifier Attributes) ->
                      {-# LINE 23 "./src-ag/DistChildAttr.ag" #-}
                      Map.findWithDefault Map.empty _chnt     _lhsIinhMap
                      {-# LINE 114 "dist/build/ResolveLocals.hs"#-}
   {-# INLINE rule2 #-}
   {-# LINE 24 "./src-ag/DistChildAttr.ag" #-}
   rule2 = \ _chnt ((_lhsIsynMap) :: Map Identifier Attributes) ->
                      {-# LINE 24 "./src-ag/DistChildAttr.ag" #-}
                      Map.findWithDefault Map.empty _chnt     _lhsIsynMap
                      {-# LINE 120 "dist/build/ResolveLocals.hs"#-}
   {-# INLINE rule3 #-}
   {-# LINE 83 "./src-ag/ResolveLocals.ag" #-}
   rule3 = \ _inh _syn name_ ->
                             {-# LINE 83 "./src-ag/ResolveLocals.ag" #-}
                             [(name_, _inh    , _syn    )]
                             {-# LINE 126 "dist/build/ResolveLocals.hs"#-}
   {-# INLINE rule4 #-}
   {-# LINE 86 "./src-ag/ResolveLocals.ag" #-}
   rule4 = \ kind_ name_ tp_ ->
                        {-# LINE 86 "./src-ag/ResolveLocals.ag" #-}
                        (name_, tp_, kind_)
                        {-# LINE 132 "dist/build/ResolveLocals.hs"#-}
   {-# INLINE rule5 #-}
   rule5 = \ kind_ name_ tp_ ->
     Child name_ tp_ kind_
   {-# INLINE rule6 #-}
   rule6 = \ _output ->
     _output

-- Children ----------------------------------------------------
-- wrapper
data Inh_Children  = Inh_Children { allfields_Inh_Children :: ([(Identifier,Type,ChildKind)]), allnts_Inh_Children :: ([Identifier]), attrs_Inh_Children :: ([(Identifier,Identifier)]), con_Inh_Children :: (Identifier), inh_Inh_Children :: (Attributes), inhMap_Inh_Children :: (Map Identifier Attributes), mergeMap_Inh_Children :: (Map Identifier (Identifier,[Identifier])), nt_Inh_Children :: (Identifier), syn_Inh_Children :: (Attributes), synMap_Inh_Children :: (Map Identifier Attributes) }
data Syn_Children  = Syn_Children { attributes_Syn_Children :: ([(Identifier,Attributes,Attributes)]), fields_Syn_Children :: ([(Identifier,Type,ChildKind)]), output_Syn_Children :: (Children) }
{-# INLINABLE wrap_Children #-}
wrap_Children :: T_Children  -> Inh_Children  -> (Syn_Children )
wrap_Children (T_Children act) (Inh_Children _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsIinh _lhsIinhMap _lhsImergeMap _lhsInt _lhsIsyn _lhsIsynMap) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Children_vIn4 _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsIinh _lhsIinhMap _lhsImergeMap _lhsInt _lhsIsyn _lhsIsynMap
        (T_Children_vOut4 _lhsOattributes _lhsOfields _lhsOoutput) <- return (inv_Children_s5 sem arg)
        return (Syn_Children _lhsOattributes _lhsOfields _lhsOoutput)
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
data T_Children_vIn4  = T_Children_vIn4 ([(Identifier,Type,ChildKind)]) ([Identifier]) ([(Identifier,Identifier)]) (Identifier) (Attributes) (Map Identifier Attributes) (Map Identifier (Identifier,[Identifier])) (Identifier) (Attributes) (Map Identifier Attributes)
data T_Children_vOut4  = T_Children_vOut4 ([(Identifier,Attributes,Attributes)]) ([(Identifier,Type,ChildKind)]) (Children)
{-# NOINLINE sem_Children_Cons #-}
sem_Children_Cons :: T_Child  -> T_Children  -> T_Children 
sem_Children_Cons arg_hd_ arg_tl_ = T_Children (return st5) where
   {-# NOINLINE st5 #-}
   st5 = let
      v4 :: T_Children_v4 
      v4 = \ (T_Children_vIn4 _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsIinh _lhsIinhMap _lhsImergeMap _lhsInt _lhsIsyn _lhsIsynMap) -> ( let
         _hdX2 = Control.Monad.Identity.runIdentity (attach_T_Child (arg_hd_))
         _tlX5 = Control.Monad.Identity.runIdentity (attach_T_Children (arg_tl_))
         (T_Child_vOut1 _hdIattributes _hdIfield _hdIoutput) = inv_Child_s2 _hdX2 (T_Child_vIn1 _hdOallfields _hdOallnts _hdOattrs _hdOcon _hdOinh _hdOinhMap _hdOmergeMap _hdOnt _hdOsyn _hdOsynMap)
         (T_Children_vOut4 _tlIattributes _tlIfields _tlIoutput) = inv_Children_s5 _tlX5 (T_Children_vIn4 _tlOallfields _tlOallnts _tlOattrs _tlOcon _tlOinh _tlOinhMap _tlOmergeMap _tlOnt _tlOsyn _tlOsynMap)
         _lhsOfields :: [(Identifier,Type,ChildKind)]
         _lhsOfields = rule7 _hdIfield _tlIfields
         _lhsOattributes :: [(Identifier,Attributes,Attributes)]
         _lhsOattributes = rule8 _hdIattributes _tlIattributes
         _output = rule9 _hdIoutput _tlIoutput
         _lhsOoutput :: Children
         _lhsOoutput = rule10 _output
         _hdOallfields = rule11 _lhsIallfields
         _hdOallnts = rule12 _lhsIallnts
         _hdOattrs = rule13 _lhsIattrs
         _hdOcon = rule14 _lhsIcon
         _hdOinh = rule15 _lhsIinh
         _hdOinhMap = rule16 _lhsIinhMap
         _hdOmergeMap = rule17 _lhsImergeMap
         _hdOnt = rule18 _lhsInt
         _hdOsyn = rule19 _lhsIsyn
         _hdOsynMap = rule20 _lhsIsynMap
         _tlOallfields = rule21 _lhsIallfields
         _tlOallnts = rule22 _lhsIallnts
         _tlOattrs = rule23 _lhsIattrs
         _tlOcon = rule24 _lhsIcon
         _tlOinh = rule25 _lhsIinh
         _tlOinhMap = rule26 _lhsIinhMap
         _tlOmergeMap = rule27 _lhsImergeMap
         _tlOnt = rule28 _lhsInt
         _tlOsyn = rule29 _lhsIsyn
         _tlOsynMap = rule30 _lhsIsynMap
         __result_ = T_Children_vOut4 _lhsOattributes _lhsOfields _lhsOoutput
         in __result_ )
     in C_Children_s5 v4
   {-# INLINE rule7 #-}
   {-# LINE 89 "./src-ag/ResolveLocals.ag" #-}
   rule7 = \ ((_hdIfield) :: (Identifier,Type,ChildKind)) ((_tlIfields) :: [(Identifier,Type,ChildKind)]) ->
                         {-# LINE 89 "./src-ag/ResolveLocals.ag" #-}
                         _hdIfield : _tlIfields
                         {-# LINE 216 "dist/build/ResolveLocals.hs"#-}
   {-# INLINE rule8 #-}
   rule8 = \ ((_hdIattributes) :: [(Identifier,Attributes,Attributes)]) ((_tlIattributes) :: [(Identifier,Attributes,Attributes)]) ->
     _hdIattributes ++ _tlIattributes
   {-# INLINE rule9 #-}
   rule9 = \ ((_hdIoutput) :: Child) ((_tlIoutput) :: Children) ->
     (:) _hdIoutput _tlIoutput
   {-# INLINE rule10 #-}
   rule10 = \ _output ->
     _output
   {-# INLINE rule11 #-}
   rule11 = \ ((_lhsIallfields) :: [(Identifier,Type,ChildKind)]) ->
     _lhsIallfields
   {-# INLINE rule12 #-}
   rule12 = \ ((_lhsIallnts) :: [Identifier]) ->
     _lhsIallnts
   {-# INLINE rule13 #-}
   rule13 = \ ((_lhsIattrs) :: [(Identifier,Identifier)]) ->
     _lhsIattrs
   {-# INLINE rule14 #-}
   rule14 = \ ((_lhsIcon) :: Identifier) ->
     _lhsIcon
   {-# INLINE rule15 #-}
   rule15 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule16 #-}
   rule16 = \ ((_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# INLINE rule17 #-}
   rule17 = \ ((_lhsImergeMap) :: Map Identifier (Identifier,[Identifier])) ->
     _lhsImergeMap
   {-# INLINE rule18 #-}
   rule18 = \ ((_lhsInt) :: Identifier) ->
     _lhsInt
   {-# INLINE rule19 #-}
   rule19 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
   {-# INLINE rule20 #-}
   rule20 = \ ((_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
   {-# INLINE rule21 #-}
   rule21 = \ ((_lhsIallfields) :: [(Identifier,Type,ChildKind)]) ->
     _lhsIallfields
   {-# INLINE rule22 #-}
   rule22 = \ ((_lhsIallnts) :: [Identifier]) ->
     _lhsIallnts
   {-# INLINE rule23 #-}
   rule23 = \ ((_lhsIattrs) :: [(Identifier,Identifier)]) ->
     _lhsIattrs
   {-# INLINE rule24 #-}
   rule24 = \ ((_lhsIcon) :: Identifier) ->
     _lhsIcon
   {-# INLINE rule25 #-}
   rule25 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule26 #-}
   rule26 = \ ((_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# INLINE rule27 #-}
   rule27 = \ ((_lhsImergeMap) :: Map Identifier (Identifier,[Identifier])) ->
     _lhsImergeMap
   {-# INLINE rule28 #-}
   rule28 = \ ((_lhsInt) :: Identifier) ->
     _lhsInt
   {-# INLINE rule29 #-}
   rule29 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
   {-# INLINE rule30 #-}
   rule30 = \ ((_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
{-# NOINLINE sem_Children_Nil #-}
sem_Children_Nil ::  T_Children 
sem_Children_Nil  = T_Children (return st5) where
   {-# NOINLINE st5 #-}
   st5 = let
      v4 :: T_Children_v4 
      v4 = \ (T_Children_vIn4 _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsIinh _lhsIinhMap _lhsImergeMap _lhsInt _lhsIsyn _lhsIsynMap) -> ( let
         _lhsOfields :: [(Identifier,Type,ChildKind)]
         _lhsOfields = rule31  ()
         _lhsOattributes :: [(Identifier,Attributes,Attributes)]
         _lhsOattributes = rule32  ()
         _output = rule33  ()
         _lhsOoutput :: Children
         _lhsOoutput = rule34 _output
         __result_ = T_Children_vOut4 _lhsOattributes _lhsOfields _lhsOoutput
         in __result_ )
     in C_Children_s5 v4
   {-# INLINE rule31 #-}
   {-# LINE 90 "./src-ag/ResolveLocals.ag" #-}
   rule31 = \  (_ :: ()) ->
                         {-# LINE 90 "./src-ag/ResolveLocals.ag" #-}
                         []
                         {-# LINE 308 "dist/build/ResolveLocals.hs"#-}
   {-# INLINE rule32 #-}
   rule32 = \  (_ :: ()) ->
     []
   {-# INLINE rule33 #-}
   rule33 = \  (_ :: ()) ->
     []
   {-# INLINE rule34 #-}
   rule34 = \ _output ->
     _output

-- Expression --------------------------------------------------
-- wrapper
data Inh_Expression  = Inh_Expression { allfields_Inh_Expression :: ([(Identifier,Type,ChildKind)]), allnts_Inh_Expression :: ([Identifier]), attrs_Inh_Expression :: ([(Identifier,Identifier)]), con_Inh_Expression :: (Identifier), mergeMap_Inh_Expression :: (Map Identifier (Identifier,[Identifier])), nt_Inh_Expression :: (Identifier), options_Inh_Expression :: (Options) }
data Syn_Expression  = Syn_Expression { errors_Syn_Expression :: (Seq Error), output_Syn_Expression :: (Expression) }
{-# INLINABLE wrap_Expression #-}
wrap_Expression :: T_Expression  -> Inh_Expression  -> (Syn_Expression )
wrap_Expression (T_Expression act) (Inh_Expression _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsImergeMap _lhsInt _lhsIoptions) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Expression_vIn7 _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsImergeMap _lhsInt _lhsIoptions
        (T_Expression_vOut7 _lhsOerrors _lhsOoutput) <- return (inv_Expression_s8 sem arg)
        return (Syn_Expression _lhsOerrors _lhsOoutput)
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
data T_Expression_vIn7  = T_Expression_vIn7 ([(Identifier,Type,ChildKind)]) ([Identifier]) ([(Identifier,Identifier)]) (Identifier) (Map Identifier (Identifier,[Identifier])) (Identifier) (Options)
data T_Expression_vOut7  = T_Expression_vOut7 (Seq Error) (Expression)
{-# NOINLINE sem_Expression_Expression #-}
sem_Expression_Expression :: (Pos) -> ([HsToken]) -> T_Expression 
sem_Expression_Expression arg_pos_ arg_tks_ = T_Expression (return st8) where
   {-# NOINLINE st8 #-}
   st8 = let
      v7 :: T_Expression_v7 
      v7 = \ (T_Expression_vIn7 _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsImergeMap _lhsInt _lhsIoptions) -> ( let
         (_errors,_newTks) = rule35 _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsImergeMap _lhsInt _lhsIoptions arg_tks_
         _lhsOoutput :: Expression
         _lhsOoutput = rule36 _newTks arg_pos_
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule37 _errors
         _output = rule38 arg_pos_ arg_tks_
         __result_ = T_Expression_vOut7 _lhsOerrors _lhsOoutput
         in __result_ )
     in C_Expression_s8 v7
   {-# INLINE rule35 #-}
   {-# LINE 145 "./src-ag/ResolveLocals.ag" #-}
   rule35 = \ ((_lhsIallfields) :: [(Identifier,Type,ChildKind)]) ((_lhsIallnts) :: [Identifier]) ((_lhsIattrs) :: [(Identifier,Identifier)]) ((_lhsIcon) :: Identifier) ((_lhsImergeMap) :: Map Identifier (Identifier,[Identifier])) ((_lhsInt) :: Identifier) ((_lhsIoptions) :: Options) tks_ ->
                                {-# LINE 145 "./src-ag/ResolveLocals.ag" #-}
                                let mergedChildren = [ x | (_,xs) <- Map.elems _lhsImergeMap, x <- xs ]
                                    attrsIn = filter (\(fld,_) -> not (fld `elem` mergedChildren)) _lhsIattrs
                                    inherited = Inh_HsTokensRoot
                                                { attrs_Inh_HsTokensRoot      = attrsIn
                                                , con_Inh_HsTokensRoot        = _lhsIcon
                                                , allfields_Inh_HsTokensRoot  = _lhsIallfields
                                                , allnts_Inh_HsTokensRoot     = _lhsIallnts
                                                , nt_Inh_HsTokensRoot         = _lhsInt
                                                , options_Inh_HsTokensRoot    = _lhsIoptions
                                                }
                                    synthesized = wrap_HsTokensRoot (sem_HsTokensRoot (HsTokensRoot tks_)) inherited
                                in (errors_Syn_HsTokensRoot synthesized, output_Syn_HsTokensRoot synthesized)
                                {-# LINE 381 "dist/build/ResolveLocals.hs"#-}
   {-# INLINE rule36 #-}
   {-# LINE 157 "./src-ag/ResolveLocals.ag" #-}
   rule36 = \ _newTks pos_ ->
                               {-# LINE 157 "./src-ag/ResolveLocals.ag" #-}
                               Expression pos_ _newTks
                               {-# LINE 387 "dist/build/ResolveLocals.hs"#-}
   {-# INLINE rule37 #-}
   rule37 = \ _errors ->
     _errors
   {-# INLINE rule38 #-}
   rule38 = \ pos_ tks_ ->
     Expression pos_ tks_

-- Grammar -----------------------------------------------------
-- wrapper
data Inh_Grammar  = Inh_Grammar { options_Inh_Grammar :: (Options) }
data Syn_Grammar  = Syn_Grammar { errors_Syn_Grammar :: (Seq Error), output_Syn_Grammar :: (Grammar) }
{-# INLINABLE wrap_Grammar #-}
wrap_Grammar :: T_Grammar  -> Inh_Grammar  -> (Syn_Grammar )
wrap_Grammar (T_Grammar act) (Inh_Grammar _lhsIoptions) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Grammar_vIn10 _lhsIoptions
        (T_Grammar_vOut10 _lhsOerrors _lhsOoutput) <- return (inv_Grammar_s11 sem arg)
        return (Syn_Grammar _lhsOerrors _lhsOoutput)
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
data T_Grammar_vOut10  = T_Grammar_vOut10 (Seq Error) (Grammar)
{-# NOINLINE sem_Grammar_Grammar #-}
sem_Grammar_Grammar :: (TypeSyns) -> (UseMap) -> (Derivings) -> (Set NontermIdent) -> T_Nonterminals  -> (PragmaMap) -> (AttrOrderMap) -> (ParamMap) -> (ContextMap) -> (QuantMap) -> (UniqueMap) -> (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) -> (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) -> (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier, [Identifier], Expression)))) -> T_Grammar 
sem_Grammar_Grammar arg_typeSyns_ arg_useMap_ arg_derivings_ arg_wrappers_ arg_nonts_ arg_pragmas_ arg_manualAttrOrderMap_ arg_paramMap_ arg_contextMap_ arg_quantMap_ arg_uniqueMap_ arg_augmentsMap_ arg_aroundsMap_ arg_mergeMap_ = T_Grammar (return st11) where
   {-# NOINLINE st11 #-}
   st11 = let
      v10 :: T_Grammar_v10 
      v10 = \ (T_Grammar_vIn10 _lhsIoptions) -> ( let
         _nontsX17 = Control.Monad.Identity.runIdentity (attach_T_Nonterminals (arg_nonts_))
         (T_Nonterminals_vOut16 _nontsIerrors _nontsIinhMap' _nontsInonts _nontsIoutput _nontsIsynMap') = inv_Nonterminals_s17 _nontsX17 (T_Nonterminals_vIn16 _nontsOallnts _nontsOinhMap _nontsOmergeMap _nontsOoptions _nontsOsynMap)
         _nontsOinhMap = rule39 _nontsIinhMap'
         _nontsOsynMap = rule40 _nontsIsynMap'
         _nontsOallnts = rule41 _nontsInonts
         _nontsOmergeMap = rule42 arg_mergeMap_
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule43 _nontsIerrors
         _output = rule44 _nontsIoutput arg_aroundsMap_ arg_augmentsMap_ arg_contextMap_ arg_derivings_ arg_manualAttrOrderMap_ arg_mergeMap_ arg_paramMap_ arg_pragmas_ arg_quantMap_ arg_typeSyns_ arg_uniqueMap_ arg_useMap_ arg_wrappers_
         _lhsOoutput :: Grammar
         _lhsOoutput = rule45 _output
         _nontsOoptions = rule46 _lhsIoptions
         __result_ = T_Grammar_vOut10 _lhsOerrors _lhsOoutput
         in __result_ )
     in C_Grammar_s11 v10
   {-# INLINE rule39 #-}
   {-# LINE 15 "./src-ag/DistChildAttr.ag" #-}
   rule39 = \ ((_nontsIinhMap') :: Map Identifier Attributes) ->
                             {-# LINE 15 "./src-ag/DistChildAttr.ag" #-}
                             _nontsIinhMap'
                             {-# LINE 452 "dist/build/ResolveLocals.hs"#-}
   {-# INLINE rule40 #-}
   {-# LINE 16 "./src-ag/DistChildAttr.ag" #-}
   rule40 = \ ((_nontsIsynMap') :: Map Identifier Attributes) ->
                             {-# LINE 16 "./src-ag/DistChildAttr.ag" #-}
                             _nontsIsynMap'
                             {-# LINE 458 "dist/build/ResolveLocals.hs"#-}
   {-# INLINE rule41 #-}
   {-# LINE 59 "./src-ag/ResolveLocals.ag" #-}
   rule41 = \ ((_nontsInonts) :: [(NontermIdent,[ConstructorIdent])]) ->
                             {-# LINE 59 "./src-ag/ResolveLocals.ag" #-}
                             map fst (_nontsInonts)
                             {-# LINE 464 "dist/build/ResolveLocals.hs"#-}
   {-# INLINE rule42 #-}
   {-# LINE 119 "./src-ag/ResolveLocals.ag" #-}
   rule42 = \ mergeMap_ ->
                                 {-# LINE 119 "./src-ag/ResolveLocals.ag" #-}
                                 Map.map (Map.map (Map.map (\(nt,srcs,_) -> (nt,srcs)))) mergeMap_
                                 {-# LINE 470 "dist/build/ResolveLocals.hs"#-}
   {-# INLINE rule43 #-}
   rule43 = \ ((_nontsIerrors) :: Seq Error) ->
     _nontsIerrors
   {-# INLINE rule44 #-}
   rule44 = \ ((_nontsIoutput) :: Nonterminals) aroundsMap_ augmentsMap_ contextMap_ derivings_ manualAttrOrderMap_ mergeMap_ paramMap_ pragmas_ quantMap_ typeSyns_ uniqueMap_ useMap_ wrappers_ ->
     Grammar typeSyns_ useMap_ derivings_ wrappers_ _nontsIoutput pragmas_ manualAttrOrderMap_ paramMap_ contextMap_ quantMap_ uniqueMap_ augmentsMap_ aroundsMap_ mergeMap_
   {-# INLINE rule45 #-}
   rule45 = \ _output ->
     _output
   {-# INLINE rule46 #-}
   rule46 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions

-- Nonterminal -------------------------------------------------
-- wrapper
data Inh_Nonterminal  = Inh_Nonterminal { allnts_Inh_Nonterminal :: ([Identifier]), inhMap_Inh_Nonterminal :: (Map Identifier Attributes), mergeMap_Inh_Nonterminal :: (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier])))), options_Inh_Nonterminal :: (Options), synMap_Inh_Nonterminal :: (Map Identifier Attributes) }
data Syn_Nonterminal  = Syn_Nonterminal { errors_Syn_Nonterminal :: (Seq Error), inhMap'_Syn_Nonterminal :: (Map Identifier Attributes), nonts_Syn_Nonterminal :: ([(NontermIdent,[ConstructorIdent])]), output_Syn_Nonterminal :: (Nonterminal), synMap'_Syn_Nonterminal :: (Map Identifier Attributes) }
{-# INLINABLE wrap_Nonterminal #-}
wrap_Nonterminal :: T_Nonterminal  -> Inh_Nonterminal  -> (Syn_Nonterminal )
wrap_Nonterminal (T_Nonterminal act) (Inh_Nonterminal _lhsIallnts _lhsIinhMap _lhsImergeMap _lhsIoptions _lhsIsynMap) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Nonterminal_vIn13 _lhsIallnts _lhsIinhMap _lhsImergeMap _lhsIoptions _lhsIsynMap
        (T_Nonterminal_vOut13 _lhsOerrors _lhsOinhMap' _lhsOnonts _lhsOoutput _lhsOsynMap') <- return (inv_Nonterminal_s14 sem arg)
        return (Syn_Nonterminal _lhsOerrors _lhsOinhMap' _lhsOnonts _lhsOoutput _lhsOsynMap')
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
data T_Nonterminal_vIn13  = T_Nonterminal_vIn13 ([Identifier]) (Map Identifier Attributes) (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier])))) (Options) (Map Identifier Attributes)
data T_Nonterminal_vOut13  = T_Nonterminal_vOut13 (Seq Error) (Map Identifier Attributes) ([(NontermIdent,[ConstructorIdent])]) (Nonterminal) (Map Identifier Attributes)
{-# NOINLINE sem_Nonterminal_Nonterminal #-}
sem_Nonterminal_Nonterminal :: (NontermIdent) -> ([Identifier]) -> (Attributes) -> (Attributes) -> T_Productions  -> T_Nonterminal 
sem_Nonterminal_Nonterminal arg_nt_ arg_params_ arg_inh_ arg_syn_ arg_prods_ = T_Nonterminal (return st14) where
   {-# NOINLINE st14 #-}
   st14 = let
      v13 :: T_Nonterminal_v13 
      v13 = \ (T_Nonterminal_vIn13 _lhsIallnts _lhsIinhMap _lhsImergeMap _lhsIoptions _lhsIsynMap) -> ( let
         _prodsX29 = Control.Monad.Identity.runIdentity (attach_T_Productions (arg_prods_))
         (T_Productions_vOut28 _prodsIcons _prodsIerrors _prodsIoutput) = inv_Productions_s29 _prodsX29 (T_Productions_vIn28 _prodsOallnts _prodsOinh _prodsOinhMap _prodsOmergeMap _prodsOnt _prodsOoptions _prodsOsyn _prodsOsynMap)
         _lhsOinhMap' :: Map Identifier Attributes
         _lhsOinhMap' = rule47 arg_inh_ arg_nt_
         _lhsOsynMap' :: Map Identifier Attributes
         _lhsOsynMap' = rule48 arg_nt_ arg_syn_
         _lhsOnonts :: [(NontermIdent,[ConstructorIdent])]
         _lhsOnonts = rule49 _prodsIcons arg_nt_
         _prodsOnt = rule50 arg_nt_
         _prodsOinh = rule51 arg_inh_
         _prodsOsyn = rule52 arg_syn_
         _mergeMap = rule53 _lhsImergeMap arg_nt_
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule54 _prodsIerrors
         _output = rule55 _prodsIoutput arg_inh_ arg_nt_ arg_params_ arg_syn_
         _lhsOoutput :: Nonterminal
         _lhsOoutput = rule56 _output
         _prodsOallnts = rule57 _lhsIallnts
         _prodsOinhMap = rule58 _lhsIinhMap
         _prodsOmergeMap = rule59 _mergeMap
         _prodsOoptions = rule60 _lhsIoptions
         _prodsOsynMap = rule61 _lhsIsynMap
         __result_ = T_Nonterminal_vOut13 _lhsOerrors _lhsOinhMap' _lhsOnonts _lhsOoutput _lhsOsynMap'
         in __result_ )
     in C_Nonterminal_s14 v13
   {-# INLINE rule47 #-}
   {-# LINE 7 "./src-ag/DistChildAttr.ag" #-}
   rule47 = \ inh_ nt_ ->
                                 {-# LINE 7 "./src-ag/DistChildAttr.ag" #-}
                                 Map.singleton nt_ inh_
                                 {-# LINE 551 "dist/build/ResolveLocals.hs"#-}
   {-# INLINE rule48 #-}
   {-# LINE 8 "./src-ag/DistChildAttr.ag" #-}
   rule48 = \ nt_ syn_ ->
                                 {-# LINE 8 "./src-ag/DistChildAttr.ag" #-}
                                 Map.singleton nt_ syn_
                                 {-# LINE 557 "dist/build/ResolveLocals.hs"#-}
   {-# INLINE rule49 #-}
   {-# LINE 63 "./src-ag/ResolveLocals.ag" #-}
   rule49 = \ ((_prodsIcons) :: [ConstructorIdent]) nt_ ->
                                {-# LINE 63 "./src-ag/ResolveLocals.ag" #-}
                                [(nt_,_prodsIcons)]
                                {-# LINE 563 "dist/build/ResolveLocals.hs"#-}
   {-# INLINE rule50 #-}
   {-# LINE 111 "./src-ag/ResolveLocals.ag" #-}
   rule50 = \ nt_ ->
                               {-# LINE 111 "./src-ag/ResolveLocals.ag" #-}
                               nt_
                               {-# LINE 569 "dist/build/ResolveLocals.hs"#-}
   {-# INLINE rule51 #-}
   {-# LINE 114 "./src-ag/ResolveLocals.ag" #-}
   rule51 = \ inh_ ->
                               {-# LINE 114 "./src-ag/ResolveLocals.ag" #-}
                               inh_
                               {-# LINE 575 "dist/build/ResolveLocals.hs"#-}
   {-# INLINE rule52 #-}
   {-# LINE 115 "./src-ag/ResolveLocals.ag" #-}
   rule52 = \ syn_ ->
                               {-# LINE 115 "./src-ag/ResolveLocals.ag" #-}
                               syn_
                               {-# LINE 581 "dist/build/ResolveLocals.hs"#-}
   {-# INLINE rule53 #-}
   {-# LINE 127 "./src-ag/ResolveLocals.ag" #-}
   rule53 = \ ((_lhsImergeMap) :: Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier])))) nt_ ->
                                                {-# LINE 127 "./src-ag/ResolveLocals.ag" #-}
                                                Map.findWithDefault Map.empty nt_ _lhsImergeMap
                                                {-# LINE 587 "dist/build/ResolveLocals.hs"#-}
   {-# INLINE rule54 #-}
   rule54 = \ ((_prodsIerrors) :: Seq Error) ->
     _prodsIerrors
   {-# INLINE rule55 #-}
   rule55 = \ ((_prodsIoutput) :: Productions) inh_ nt_ params_ syn_ ->
     Nonterminal nt_ params_ inh_ syn_ _prodsIoutput
   {-# INLINE rule56 #-}
   rule56 = \ _output ->
     _output
   {-# INLINE rule57 #-}
   rule57 = \ ((_lhsIallnts) :: [Identifier]) ->
     _lhsIallnts
   {-# INLINE rule58 #-}
   rule58 = \ ((_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# INLINE rule59 #-}
   rule59 = \ _mergeMap ->
     _mergeMap
   {-# INLINE rule60 #-}
   rule60 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule61 #-}
   rule61 = \ ((_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap

-- Nonterminals ------------------------------------------------
-- wrapper
data Inh_Nonterminals  = Inh_Nonterminals { allnts_Inh_Nonterminals :: ([Identifier]), inhMap_Inh_Nonterminals :: (Map Identifier Attributes), mergeMap_Inh_Nonterminals :: (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier])))), options_Inh_Nonterminals :: (Options), synMap_Inh_Nonterminals :: (Map Identifier Attributes) }
data Syn_Nonterminals  = Syn_Nonterminals { errors_Syn_Nonterminals :: (Seq Error), inhMap'_Syn_Nonterminals :: (Map Identifier Attributes), nonts_Syn_Nonterminals :: ([(NontermIdent,[ConstructorIdent])]), output_Syn_Nonterminals :: (Nonterminals), synMap'_Syn_Nonterminals :: (Map Identifier Attributes) }
{-# INLINABLE wrap_Nonterminals #-}
wrap_Nonterminals :: T_Nonterminals  -> Inh_Nonterminals  -> (Syn_Nonterminals )
wrap_Nonterminals (T_Nonterminals act) (Inh_Nonterminals _lhsIallnts _lhsIinhMap _lhsImergeMap _lhsIoptions _lhsIsynMap) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Nonterminals_vIn16 _lhsIallnts _lhsIinhMap _lhsImergeMap _lhsIoptions _lhsIsynMap
        (T_Nonterminals_vOut16 _lhsOerrors _lhsOinhMap' _lhsOnonts _lhsOoutput _lhsOsynMap') <- return (inv_Nonterminals_s17 sem arg)
        return (Syn_Nonterminals _lhsOerrors _lhsOinhMap' _lhsOnonts _lhsOoutput _lhsOsynMap')
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
data T_Nonterminals_vIn16  = T_Nonterminals_vIn16 ([Identifier]) (Map Identifier Attributes) (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier])))) (Options) (Map Identifier Attributes)
data T_Nonterminals_vOut16  = T_Nonterminals_vOut16 (Seq Error) (Map Identifier Attributes) ([(NontermIdent,[ConstructorIdent])]) (Nonterminals) (Map Identifier Attributes)
{-# NOINLINE sem_Nonterminals_Cons #-}
sem_Nonterminals_Cons :: T_Nonterminal  -> T_Nonterminals  -> T_Nonterminals 
sem_Nonterminals_Cons arg_hd_ arg_tl_ = T_Nonterminals (return st17) where
   {-# NOINLINE st17 #-}
   st17 = let
      v16 :: T_Nonterminals_v16 
      v16 = \ (T_Nonterminals_vIn16 _lhsIallnts _lhsIinhMap _lhsImergeMap _lhsIoptions _lhsIsynMap) -> ( let
         _hdX14 = Control.Monad.Identity.runIdentity (attach_T_Nonterminal (arg_hd_))
         _tlX17 = Control.Monad.Identity.runIdentity (attach_T_Nonterminals (arg_tl_))
         (T_Nonterminal_vOut13 _hdIerrors _hdIinhMap' _hdInonts _hdIoutput _hdIsynMap') = inv_Nonterminal_s14 _hdX14 (T_Nonterminal_vIn13 _hdOallnts _hdOinhMap _hdOmergeMap _hdOoptions _hdOsynMap)
         (T_Nonterminals_vOut16 _tlIerrors _tlIinhMap' _tlInonts _tlIoutput _tlIsynMap') = inv_Nonterminals_s17 _tlX17 (T_Nonterminals_vIn16 _tlOallnts _tlOinhMap _tlOmergeMap _tlOoptions _tlOsynMap)
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule62 _hdIerrors _tlIerrors
         _lhsOinhMap' :: Map Identifier Attributes
         _lhsOinhMap' = rule63 _hdIinhMap' _tlIinhMap'
         _lhsOnonts :: [(NontermIdent,[ConstructorIdent])]
         _lhsOnonts = rule64 _hdInonts _tlInonts
         _lhsOsynMap' :: Map Identifier Attributes
         _lhsOsynMap' = rule65 _hdIsynMap' _tlIsynMap'
         _output = rule66 _hdIoutput _tlIoutput
         _lhsOoutput :: Nonterminals
         _lhsOoutput = rule67 _output
         _hdOallnts = rule68 _lhsIallnts
         _hdOinhMap = rule69 _lhsIinhMap
         _hdOmergeMap = rule70 _lhsImergeMap
         _hdOoptions = rule71 _lhsIoptions
         _hdOsynMap = rule72 _lhsIsynMap
         _tlOallnts = rule73 _lhsIallnts
         _tlOinhMap = rule74 _lhsIinhMap
         _tlOmergeMap = rule75 _lhsImergeMap
         _tlOoptions = rule76 _lhsIoptions
         _tlOsynMap = rule77 _lhsIsynMap
         __result_ = T_Nonterminals_vOut16 _lhsOerrors _lhsOinhMap' _lhsOnonts _lhsOoutput _lhsOsynMap'
         in __result_ )
     in C_Nonterminals_s17 v16
   {-# INLINE rule62 #-}
   rule62 = \ ((_hdIerrors) :: Seq Error) ((_tlIerrors) :: Seq Error) ->
     _hdIerrors Seq.>< _tlIerrors
   {-# INLINE rule63 #-}
   rule63 = \ ((_hdIinhMap') :: Map Identifier Attributes) ((_tlIinhMap') :: Map Identifier Attributes) ->
     _hdIinhMap' `Map.union` _tlIinhMap'
   {-# INLINE rule64 #-}
   rule64 = \ ((_hdInonts) :: [(NontermIdent,[ConstructorIdent])]) ((_tlInonts) :: [(NontermIdent,[ConstructorIdent])]) ->
     _hdInonts ++ _tlInonts
   {-# INLINE rule65 #-}
   rule65 = \ ((_hdIsynMap') :: Map Identifier Attributes) ((_tlIsynMap') :: Map Identifier Attributes) ->
     _hdIsynMap' `Map.union` _tlIsynMap'
   {-# INLINE rule66 #-}
   rule66 = \ ((_hdIoutput) :: Nonterminal) ((_tlIoutput) :: Nonterminals) ->
     (:) _hdIoutput _tlIoutput
   {-# INLINE rule67 #-}
   rule67 = \ _output ->
     _output
   {-# INLINE rule68 #-}
   rule68 = \ ((_lhsIallnts) :: [Identifier]) ->
     _lhsIallnts
   {-# INLINE rule69 #-}
   rule69 = \ ((_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# INLINE rule70 #-}
   rule70 = \ ((_lhsImergeMap) :: Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier])))) ->
     _lhsImergeMap
   {-# INLINE rule71 #-}
   rule71 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule72 #-}
   rule72 = \ ((_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
   {-# INLINE rule73 #-}
   rule73 = \ ((_lhsIallnts) :: [Identifier]) ->
     _lhsIallnts
   {-# INLINE rule74 #-}
   rule74 = \ ((_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# INLINE rule75 #-}
   rule75 = \ ((_lhsImergeMap) :: Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier])))) ->
     _lhsImergeMap
   {-# INLINE rule76 #-}
   rule76 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule77 #-}
   rule77 = \ ((_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
{-# NOINLINE sem_Nonterminals_Nil #-}
sem_Nonterminals_Nil ::  T_Nonterminals 
sem_Nonterminals_Nil  = T_Nonterminals (return st17) where
   {-# NOINLINE st17 #-}
   st17 = let
      v16 :: T_Nonterminals_v16 
      v16 = \ (T_Nonterminals_vIn16 _lhsIallnts _lhsIinhMap _lhsImergeMap _lhsIoptions _lhsIsynMap) -> ( let
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule78  ()
         _lhsOinhMap' :: Map Identifier Attributes
         _lhsOinhMap' = rule79  ()
         _lhsOnonts :: [(NontermIdent,[ConstructorIdent])]
         _lhsOnonts = rule80  ()
         _lhsOsynMap' :: Map Identifier Attributes
         _lhsOsynMap' = rule81  ()
         _output = rule82  ()
         _lhsOoutput :: Nonterminals
         _lhsOoutput = rule83 _output
         __result_ = T_Nonterminals_vOut16 _lhsOerrors _lhsOinhMap' _lhsOnonts _lhsOoutput _lhsOsynMap'
         in __result_ )
     in C_Nonterminals_s17 v16
   {-# INLINE rule78 #-}
   rule78 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule79 #-}
   rule79 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule80 #-}
   rule80 = \  (_ :: ()) ->
     []
   {-# INLINE rule81 #-}
   rule81 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule82 #-}
   rule82 = \  (_ :: ()) ->
     []
   {-# INLINE rule83 #-}
   rule83 = \ _output ->
     _output

-- Pattern -----------------------------------------------------
-- wrapper
data Inh_Pattern  = Inh_Pattern { con_Inh_Pattern :: (Identifier), inh_Inh_Pattern :: (Attributes), nt_Inh_Pattern :: (Identifier), syn_Inh_Pattern :: (Attributes) }
data Syn_Pattern  = Syn_Pattern { copy_Syn_Pattern :: (Pattern), errors_Syn_Pattern :: (Seq Error), instVars_Syn_Pattern :: ([Identifier]), locVars_Syn_Pattern :: ([Identifier]), output_Syn_Pattern :: (Pattern) }
{-# INLINABLE wrap_Pattern #-}
wrap_Pattern :: T_Pattern  -> Inh_Pattern  -> (Syn_Pattern )
wrap_Pattern (T_Pattern act) (Inh_Pattern _lhsIcon _lhsIinh _lhsInt _lhsIsyn) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Pattern_vIn19 _lhsIcon _lhsIinh _lhsInt _lhsIsyn
        (T_Pattern_vOut19 _lhsOcopy _lhsOerrors _lhsOinstVars _lhsOlocVars _lhsOoutput) <- return (inv_Pattern_s20 sem arg)
        return (Syn_Pattern _lhsOcopy _lhsOerrors _lhsOinstVars _lhsOlocVars _lhsOoutput)
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
data T_Pattern_vIn19  = T_Pattern_vIn19 (Identifier) (Attributes) (Identifier) (Attributes)
data T_Pattern_vOut19  = T_Pattern_vOut19 (Pattern) (Seq Error) ([Identifier]) ([Identifier]) (Pattern)
{-# NOINLINE sem_Pattern_Constr #-}
sem_Pattern_Constr :: (ConstructorIdent) -> T_Patterns  -> T_Pattern 
sem_Pattern_Constr arg_name_ arg_pats_ = T_Pattern (return st20) where
   {-# NOINLINE st20 #-}
   st20 = let
      v19 :: T_Pattern_v19 
      v19 = \ (T_Pattern_vIn19 _lhsIcon _lhsIinh _lhsInt _lhsIsyn) -> ( let
         _patsX23 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_pats_))
         (T_Patterns_vOut22 _patsIcopy _patsIerrors _patsIinstVars _patsIlocVars _patsIoutput) = inv_Patterns_s23 _patsX23 (T_Patterns_vIn22 _patsOcon _patsOinh _patsOnt _patsOsyn)
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule84 _patsIerrors
         _lhsOinstVars :: [Identifier]
         _lhsOinstVars = rule85 _patsIinstVars
         _lhsOlocVars :: [Identifier]
         _lhsOlocVars = rule86 _patsIlocVars
         _copy = rule87 _patsIcopy arg_name_
         _output = rule88 _patsIoutput arg_name_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule89 _copy
         _lhsOoutput :: Pattern
         _lhsOoutput = rule90 _output
         _patsOcon = rule91 _lhsIcon
         _patsOinh = rule92 _lhsIinh
         _patsOnt = rule93 _lhsInt
         _patsOsyn = rule94 _lhsIsyn
         __result_ = T_Pattern_vOut19 _lhsOcopy _lhsOerrors _lhsOinstVars _lhsOlocVars _lhsOoutput
         in __result_ )
     in C_Pattern_s20 v19
   {-# INLINE rule84 #-}
   rule84 = \ ((_patsIerrors) :: Seq Error) ->
     _patsIerrors
   {-# INLINE rule85 #-}
   rule85 = \ ((_patsIinstVars) :: [Identifier]) ->
     _patsIinstVars
   {-# INLINE rule86 #-}
   rule86 = \ ((_patsIlocVars) :: [Identifier]) ->
     _patsIlocVars
   {-# INLINE rule87 #-}
   rule87 = \ ((_patsIcopy) :: Patterns) name_ ->
     Constr name_ _patsIcopy
   {-# INLINE rule88 #-}
   rule88 = \ ((_patsIoutput) :: Patterns) name_ ->
     Constr name_ _patsIoutput
   {-# INLINE rule89 #-}
   rule89 = \ _copy ->
     _copy
   {-# INLINE rule90 #-}
   rule90 = \ _output ->
     _output
   {-# INLINE rule91 #-}
   rule91 = \ ((_lhsIcon) :: Identifier) ->
     _lhsIcon
   {-# INLINE rule92 #-}
   rule92 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule93 #-}
   rule93 = \ ((_lhsInt) :: Identifier) ->
     _lhsInt
   {-# INLINE rule94 #-}
   rule94 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
{-# NOINLINE sem_Pattern_Product #-}
sem_Pattern_Product :: (Pos) -> T_Patterns  -> T_Pattern 
sem_Pattern_Product arg_pos_ arg_pats_ = T_Pattern (return st20) where
   {-# NOINLINE st20 #-}
   st20 = let
      v19 :: T_Pattern_v19 
      v19 = \ (T_Pattern_vIn19 _lhsIcon _lhsIinh _lhsInt _lhsIsyn) -> ( let
         _patsX23 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_pats_))
         (T_Patterns_vOut22 _patsIcopy _patsIerrors _patsIinstVars _patsIlocVars _patsIoutput) = inv_Patterns_s23 _patsX23 (T_Patterns_vIn22 _patsOcon _patsOinh _patsOnt _patsOsyn)
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule95 _patsIerrors
         _lhsOinstVars :: [Identifier]
         _lhsOinstVars = rule96 _patsIinstVars
         _lhsOlocVars :: [Identifier]
         _lhsOlocVars = rule97 _patsIlocVars
         _copy = rule98 _patsIcopy arg_pos_
         _output = rule99 _patsIoutput arg_pos_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule100 _copy
         _lhsOoutput :: Pattern
         _lhsOoutput = rule101 _output
         _patsOcon = rule102 _lhsIcon
         _patsOinh = rule103 _lhsIinh
         _patsOnt = rule104 _lhsInt
         _patsOsyn = rule105 _lhsIsyn
         __result_ = T_Pattern_vOut19 _lhsOcopy _lhsOerrors _lhsOinstVars _lhsOlocVars _lhsOoutput
         in __result_ )
     in C_Pattern_s20 v19
   {-# INLINE rule95 #-}
   rule95 = \ ((_patsIerrors) :: Seq Error) ->
     _patsIerrors
   {-# INLINE rule96 #-}
   rule96 = \ ((_patsIinstVars) :: [Identifier]) ->
     _patsIinstVars
   {-# INLINE rule97 #-}
   rule97 = \ ((_patsIlocVars) :: [Identifier]) ->
     _patsIlocVars
   {-# INLINE rule98 #-}
   rule98 = \ ((_patsIcopy) :: Patterns) pos_ ->
     Product pos_ _patsIcopy
   {-# INLINE rule99 #-}
   rule99 = \ ((_patsIoutput) :: Patterns) pos_ ->
     Product pos_ _patsIoutput
   {-# INLINE rule100 #-}
   rule100 = \ _copy ->
     _copy
   {-# INLINE rule101 #-}
   rule101 = \ _output ->
     _output
   {-# INLINE rule102 #-}
   rule102 = \ ((_lhsIcon) :: Identifier) ->
     _lhsIcon
   {-# INLINE rule103 #-}
   rule103 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule104 #-}
   rule104 = \ ((_lhsInt) :: Identifier) ->
     _lhsInt
   {-# INLINE rule105 #-}
   rule105 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
{-# NOINLINE sem_Pattern_Alias #-}
sem_Pattern_Alias :: (Identifier) -> (Identifier) -> T_Pattern  -> T_Pattern 
sem_Pattern_Alias arg_field_ arg_attr_ arg_pat_ = T_Pattern (return st20) where
   {-# NOINLINE st20 #-}
   st20 = let
      v19 :: T_Pattern_v19 
      v19 = \ (T_Pattern_vIn19 _lhsIcon _lhsIinh _lhsInt _lhsIsyn) -> ( let
         _patX20 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pat_))
         (T_Pattern_vOut19 _patIcopy _patIerrors _patIinstVars _patIlocVars _patIoutput) = inv_Pattern_s20 _patX20 (T_Pattern_vIn19 _patOcon _patOinh _patOnt _patOsyn)
         _lhsOlocVars :: [Identifier]
         _lhsOlocVars = rule106 arg_attr_ arg_field_
         _lhsOinstVars :: [Identifier]
         _lhsOinstVars = rule107 arg_attr_ arg_field_
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule108 _patIerrors
         _copy = rule109 _patIcopy arg_attr_ arg_field_
         _output = rule110 _patIoutput arg_attr_ arg_field_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule111 _copy
         _lhsOoutput :: Pattern
         _lhsOoutput = rule112 _output
         _patOcon = rule113 _lhsIcon
         _patOinh = rule114 _lhsIinh
         _patOnt = rule115 _lhsInt
         _patOsyn = rule116 _lhsIsyn
         __result_ = T_Pattern_vOut19 _lhsOcopy _lhsOerrors _lhsOinstVars _lhsOlocVars _lhsOoutput
         in __result_ )
     in C_Pattern_s20 v19
   {-# INLINE rule106 #-}
   {-# LINE 95 "./src-ag/ResolveLocals.ag" #-}
   rule106 = \ attr_ field_ ->
                               {-# LINE 95 "./src-ag/ResolveLocals.ag" #-}
                               if field_ == _LOC
                                  then [attr_]
                                  else []
                               {-# LINE 957 "dist/build/ResolveLocals.hs"#-}
   {-# INLINE rule107 #-}
   {-# LINE 98 "./src-ag/ResolveLocals.ag" #-}
   rule107 = \ attr_ field_ ->
                               {-# LINE 98 "./src-ag/ResolveLocals.ag" #-}
                               if field_ == _INST
                                  then [attr_]
                                  else []
                               {-# LINE 965 "dist/build/ResolveLocals.hs"#-}
   {-# INLINE rule108 #-}
   rule108 = \ ((_patIerrors) :: Seq Error) ->
     _patIerrors
   {-# INLINE rule109 #-}
   rule109 = \ ((_patIcopy) :: Pattern) attr_ field_ ->
     Alias field_ attr_ _patIcopy
   {-# INLINE rule110 #-}
   rule110 = \ ((_patIoutput) :: Pattern) attr_ field_ ->
     Alias field_ attr_ _patIoutput
   {-# INLINE rule111 #-}
   rule111 = \ _copy ->
     _copy
   {-# INLINE rule112 #-}
   rule112 = \ _output ->
     _output
   {-# INLINE rule113 #-}
   rule113 = \ ((_lhsIcon) :: Identifier) ->
     _lhsIcon
   {-# INLINE rule114 #-}
   rule114 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule115 #-}
   rule115 = \ ((_lhsInt) :: Identifier) ->
     _lhsInt
   {-# INLINE rule116 #-}
   rule116 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
{-# NOINLINE sem_Pattern_Irrefutable #-}
sem_Pattern_Irrefutable :: T_Pattern  -> T_Pattern 
sem_Pattern_Irrefutable arg_pat_ = T_Pattern (return st20) where
   {-# NOINLINE st20 #-}
   st20 = let
      v19 :: T_Pattern_v19 
      v19 = \ (T_Pattern_vIn19 _lhsIcon _lhsIinh _lhsInt _lhsIsyn) -> ( let
         _patX20 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pat_))
         (T_Pattern_vOut19 _patIcopy _patIerrors _patIinstVars _patIlocVars _patIoutput) = inv_Pattern_s20 _patX20 (T_Pattern_vIn19 _patOcon _patOinh _patOnt _patOsyn)
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule117 _patIerrors
         _lhsOinstVars :: [Identifier]
         _lhsOinstVars = rule118 _patIinstVars
         _lhsOlocVars :: [Identifier]
         _lhsOlocVars = rule119 _patIlocVars
         _copy = rule120 _patIcopy
         _output = rule121 _patIoutput
         _lhsOcopy :: Pattern
         _lhsOcopy = rule122 _copy
         _lhsOoutput :: Pattern
         _lhsOoutput = rule123 _output
         _patOcon = rule124 _lhsIcon
         _patOinh = rule125 _lhsIinh
         _patOnt = rule126 _lhsInt
         _patOsyn = rule127 _lhsIsyn
         __result_ = T_Pattern_vOut19 _lhsOcopy _lhsOerrors _lhsOinstVars _lhsOlocVars _lhsOoutput
         in __result_ )
     in C_Pattern_s20 v19
   {-# INLINE rule117 #-}
   rule117 = \ ((_patIerrors) :: Seq Error) ->
     _patIerrors
   {-# INLINE rule118 #-}
   rule118 = \ ((_patIinstVars) :: [Identifier]) ->
     _patIinstVars
   {-# INLINE rule119 #-}
   rule119 = \ ((_patIlocVars) :: [Identifier]) ->
     _patIlocVars
   {-# INLINE rule120 #-}
   rule120 = \ ((_patIcopy) :: Pattern) ->
     Irrefutable _patIcopy
   {-# INLINE rule121 #-}
   rule121 = \ ((_patIoutput) :: Pattern) ->
     Irrefutable _patIoutput
   {-# INLINE rule122 #-}
   rule122 = \ _copy ->
     _copy
   {-# INLINE rule123 #-}
   rule123 = \ _output ->
     _output
   {-# INLINE rule124 #-}
   rule124 = \ ((_lhsIcon) :: Identifier) ->
     _lhsIcon
   {-# INLINE rule125 #-}
   rule125 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule126 #-}
   rule126 = \ ((_lhsInt) :: Identifier) ->
     _lhsInt
   {-# INLINE rule127 #-}
   rule127 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
{-# NOINLINE sem_Pattern_Underscore #-}
sem_Pattern_Underscore :: (Pos) -> T_Pattern 
sem_Pattern_Underscore arg_pos_ = T_Pattern (return st20) where
   {-# NOINLINE st20 #-}
   st20 = let
      v19 :: T_Pattern_v19 
      v19 = \ (T_Pattern_vIn19 _lhsIcon _lhsIinh _lhsInt _lhsIsyn) -> ( let
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule128  ()
         _lhsOinstVars :: [Identifier]
         _lhsOinstVars = rule129  ()
         _lhsOlocVars :: [Identifier]
         _lhsOlocVars = rule130  ()
         _copy = rule131 arg_pos_
         _output = rule132 arg_pos_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule133 _copy
         _lhsOoutput :: Pattern
         _lhsOoutput = rule134 _output
         __result_ = T_Pattern_vOut19 _lhsOcopy _lhsOerrors _lhsOinstVars _lhsOlocVars _lhsOoutput
         in __result_ )
     in C_Pattern_s20 v19
   {-# INLINE rule128 #-}
   rule128 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule129 #-}
   rule129 = \  (_ :: ()) ->
     []
   {-# INLINE rule130 #-}
   rule130 = \  (_ :: ()) ->
     []
   {-# INLINE rule131 #-}
   rule131 = \ pos_ ->
     Underscore pos_
   {-# INLINE rule132 #-}
   rule132 = \ pos_ ->
     Underscore pos_
   {-# INLINE rule133 #-}
   rule133 = \ _copy ->
     _copy
   {-# INLINE rule134 #-}
   rule134 = \ _output ->
     _output

-- Patterns ----------------------------------------------------
-- wrapper
data Inh_Patterns  = Inh_Patterns { con_Inh_Patterns :: (Identifier), inh_Inh_Patterns :: (Attributes), nt_Inh_Patterns :: (Identifier), syn_Inh_Patterns :: (Attributes) }
data Syn_Patterns  = Syn_Patterns { copy_Syn_Patterns :: (Patterns), errors_Syn_Patterns :: (Seq Error), instVars_Syn_Patterns :: ([Identifier]), locVars_Syn_Patterns :: ([Identifier]), output_Syn_Patterns :: (Patterns) }
{-# INLINABLE wrap_Patterns #-}
wrap_Patterns :: T_Patterns  -> Inh_Patterns  -> (Syn_Patterns )
wrap_Patterns (T_Patterns act) (Inh_Patterns _lhsIcon _lhsIinh _lhsInt _lhsIsyn) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Patterns_vIn22 _lhsIcon _lhsIinh _lhsInt _lhsIsyn
        (T_Patterns_vOut22 _lhsOcopy _lhsOerrors _lhsOinstVars _lhsOlocVars _lhsOoutput) <- return (inv_Patterns_s23 sem arg)
        return (Syn_Patterns _lhsOcopy _lhsOerrors _lhsOinstVars _lhsOlocVars _lhsOoutput)
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
data T_Patterns_vIn22  = T_Patterns_vIn22 (Identifier) (Attributes) (Identifier) (Attributes)
data T_Patterns_vOut22  = T_Patterns_vOut22 (Patterns) (Seq Error) ([Identifier]) ([Identifier]) (Patterns)
{-# NOINLINE sem_Patterns_Cons #-}
sem_Patterns_Cons :: T_Pattern  -> T_Patterns  -> T_Patterns 
sem_Patterns_Cons arg_hd_ arg_tl_ = T_Patterns (return st23) where
   {-# NOINLINE st23 #-}
   st23 = let
      v22 :: T_Patterns_v22 
      v22 = \ (T_Patterns_vIn22 _lhsIcon _lhsIinh _lhsInt _lhsIsyn) -> ( let
         _hdX20 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_hd_))
         _tlX23 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_tl_))
         (T_Pattern_vOut19 _hdIcopy _hdIerrors _hdIinstVars _hdIlocVars _hdIoutput) = inv_Pattern_s20 _hdX20 (T_Pattern_vIn19 _hdOcon _hdOinh _hdOnt _hdOsyn)
         (T_Patterns_vOut22 _tlIcopy _tlIerrors _tlIinstVars _tlIlocVars _tlIoutput) = inv_Patterns_s23 _tlX23 (T_Patterns_vIn22 _tlOcon _tlOinh _tlOnt _tlOsyn)
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule135 _hdIerrors _tlIerrors
         _lhsOinstVars :: [Identifier]
         _lhsOinstVars = rule136 _hdIinstVars _tlIinstVars
         _lhsOlocVars :: [Identifier]
         _lhsOlocVars = rule137 _hdIlocVars _tlIlocVars
         _copy = rule138 _hdIcopy _tlIcopy
         _output = rule139 _hdIoutput _tlIoutput
         _lhsOcopy :: Patterns
         _lhsOcopy = rule140 _copy
         _lhsOoutput :: Patterns
         _lhsOoutput = rule141 _output
         _hdOcon = rule142 _lhsIcon
         _hdOinh = rule143 _lhsIinh
         _hdOnt = rule144 _lhsInt
         _hdOsyn = rule145 _lhsIsyn
         _tlOcon = rule146 _lhsIcon
         _tlOinh = rule147 _lhsIinh
         _tlOnt = rule148 _lhsInt
         _tlOsyn = rule149 _lhsIsyn
         __result_ = T_Patterns_vOut22 _lhsOcopy _lhsOerrors _lhsOinstVars _lhsOlocVars _lhsOoutput
         in __result_ )
     in C_Patterns_s23 v22
   {-# INLINE rule135 #-}
   rule135 = \ ((_hdIerrors) :: Seq Error) ((_tlIerrors) :: Seq Error) ->
     _hdIerrors Seq.>< _tlIerrors
   {-# INLINE rule136 #-}
   rule136 = \ ((_hdIinstVars) :: [Identifier]) ((_tlIinstVars) :: [Identifier]) ->
     _hdIinstVars ++ _tlIinstVars
   {-# INLINE rule137 #-}
   rule137 = \ ((_hdIlocVars) :: [Identifier]) ((_tlIlocVars) :: [Identifier]) ->
     _hdIlocVars ++ _tlIlocVars
   {-# INLINE rule138 #-}
   rule138 = \ ((_hdIcopy) :: Pattern) ((_tlIcopy) :: Patterns) ->
     (:) _hdIcopy _tlIcopy
   {-# INLINE rule139 #-}
   rule139 = \ ((_hdIoutput) :: Pattern) ((_tlIoutput) :: Patterns) ->
     (:) _hdIoutput _tlIoutput
   {-# INLINE rule140 #-}
   rule140 = \ _copy ->
     _copy
   {-# INLINE rule141 #-}
   rule141 = \ _output ->
     _output
   {-# INLINE rule142 #-}
   rule142 = \ ((_lhsIcon) :: Identifier) ->
     _lhsIcon
   {-# INLINE rule143 #-}
   rule143 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule144 #-}
   rule144 = \ ((_lhsInt) :: Identifier) ->
     _lhsInt
   {-# INLINE rule145 #-}
   rule145 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
   {-# INLINE rule146 #-}
   rule146 = \ ((_lhsIcon) :: Identifier) ->
     _lhsIcon
   {-# INLINE rule147 #-}
   rule147 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule148 #-}
   rule148 = \ ((_lhsInt) :: Identifier) ->
     _lhsInt
   {-# INLINE rule149 #-}
   rule149 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
{-# NOINLINE sem_Patterns_Nil #-}
sem_Patterns_Nil ::  T_Patterns 
sem_Patterns_Nil  = T_Patterns (return st23) where
   {-# NOINLINE st23 #-}
   st23 = let
      v22 :: T_Patterns_v22 
      v22 = \ (T_Patterns_vIn22 _lhsIcon _lhsIinh _lhsInt _lhsIsyn) -> ( let
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule150  ()
         _lhsOinstVars :: [Identifier]
         _lhsOinstVars = rule151  ()
         _lhsOlocVars :: [Identifier]
         _lhsOlocVars = rule152  ()
         _copy = rule153  ()
         _output = rule154  ()
         _lhsOcopy :: Patterns
         _lhsOcopy = rule155 _copy
         _lhsOoutput :: Patterns
         _lhsOoutput = rule156 _output
         __result_ = T_Patterns_vOut22 _lhsOcopy _lhsOerrors _lhsOinstVars _lhsOlocVars _lhsOoutput
         in __result_ )
     in C_Patterns_s23 v22
   {-# INLINE rule150 #-}
   rule150 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule151 #-}
   rule151 = \  (_ :: ()) ->
     []
   {-# INLINE rule152 #-}
   rule152 = \  (_ :: ()) ->
     []
   {-# INLINE rule153 #-}
   rule153 = \  (_ :: ()) ->
     []
   {-# INLINE rule154 #-}
   rule154 = \  (_ :: ()) ->
     []
   {-# INLINE rule155 #-}
   rule155 = \ _copy ->
     _copy
   {-# INLINE rule156 #-}
   rule156 = \ _output ->
     _output

-- Production --------------------------------------------------
-- wrapper
data Inh_Production  = Inh_Production { allnts_Inh_Production :: ([Identifier]), inh_Inh_Production :: (Attributes), inhMap_Inh_Production :: (Map Identifier Attributes), mergeMap_Inh_Production :: (Map ConstructorIdent (Map Identifier (Identifier,[Identifier]))), nt_Inh_Production :: (Identifier), options_Inh_Production :: (Options), syn_Inh_Production :: (Attributes), synMap_Inh_Production :: (Map Identifier Attributes) }
data Syn_Production  = Syn_Production { cons_Syn_Production :: ([ConstructorIdent]), errors_Syn_Production :: (Seq Error), output_Syn_Production :: (Production) }
{-# INLINABLE wrap_Production #-}
wrap_Production :: T_Production  -> Inh_Production  -> (Syn_Production )
wrap_Production (T_Production act) (Inh_Production _lhsIallnts _lhsIinh _lhsIinhMap _lhsImergeMap _lhsInt _lhsIoptions _lhsIsyn _lhsIsynMap) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Production_vIn25 _lhsIallnts _lhsIinh _lhsIinhMap _lhsImergeMap _lhsInt _lhsIoptions _lhsIsyn _lhsIsynMap
        (T_Production_vOut25 _lhsOcons _lhsOerrors _lhsOoutput) <- return (inv_Production_s26 sem arg)
        return (Syn_Production _lhsOcons _lhsOerrors _lhsOoutput)
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
data T_Production_vIn25  = T_Production_vIn25 ([Identifier]) (Attributes) (Map Identifier Attributes) (Map ConstructorIdent (Map Identifier (Identifier,[Identifier]))) (Identifier) (Options) (Attributes) (Map Identifier Attributes)
data T_Production_vOut25  = T_Production_vOut25 ([ConstructorIdent]) (Seq Error) (Production)
{-# NOINLINE sem_Production_Production #-}
sem_Production_Production :: (ConstructorIdent) -> ([Identifier]) -> ([Type]) -> T_Children  -> T_Rules  -> T_TypeSigs  -> (MaybeMacro) -> T_Production 
sem_Production_Production arg_con_ arg_params_ arg_constraints_ arg_children_ arg_rules_ arg_typeSigs_ arg_macro_ = T_Production (return st26) where
   {-# NOINLINE st26 #-}
   st26 = let
      v25 :: T_Production_v25 
      v25 = \ (T_Production_vIn25 _lhsIallnts _lhsIinh _lhsIinhMap _lhsImergeMap _lhsInt _lhsIoptions _lhsIsyn _lhsIsynMap) -> ( let
         _childrenX5 = Control.Monad.Identity.runIdentity (attach_T_Children (arg_children_))
         _rulesX35 = Control.Monad.Identity.runIdentity (attach_T_Rules (arg_rules_))
         _typeSigsX41 = Control.Monad.Identity.runIdentity (attach_T_TypeSigs (arg_typeSigs_))
         (T_Children_vOut4 _childrenIattributes _childrenIfields _childrenIoutput) = inv_Children_s5 _childrenX5 (T_Children_vIn4 _childrenOallfields _childrenOallnts _childrenOattrs _childrenOcon _childrenOinh _childrenOinhMap _childrenOmergeMap _childrenOnt _childrenOsyn _childrenOsynMap)
         (T_Rules_vOut34 _rulesIerrors _rulesIinstVars _rulesIlocVars _rulesIoutput) = inv_Rules_s35 _rulesX35 (T_Rules_vIn34 _rulesOallfields _rulesOallnts _rulesOattrs _rulesOcon _rulesOinh _rulesOmergeMap _rulesOnt _rulesOoptions _rulesOsyn)
         (T_TypeSigs_vOut40 _typeSigsIoutput) = inv_TypeSigs_s41 _typeSigsX41 (T_TypeSigs_vIn40 )
         _lhsOcons :: [ConstructorIdent]
         _lhsOcons = rule157 arg_con_
         _allfields = rule158 _childrenIfields
         _attrs = rule159 _childrenIattributes _inhnames _rulesIinstVars _rulesIlocVars
         _inhnames = rule160 _lhsIinh
         _synnames = rule161 _lhsIsyn
         _childrenOcon = rule162 arg_con_
         _rulesOcon = rule163 arg_con_
         _mergeMap = rule164 _lhsImergeMap arg_con_
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule165 _rulesIerrors
         _output = rule166 _childrenIoutput _rulesIoutput _typeSigsIoutput arg_con_ arg_constraints_ arg_macro_ arg_params_
         _lhsOoutput :: Production
         _lhsOoutput = rule167 _output
         _childrenOallfields = rule168 _allfields
         _childrenOallnts = rule169 _lhsIallnts
         _childrenOattrs = rule170 _attrs
         _childrenOinh = rule171 _lhsIinh
         _childrenOinhMap = rule172 _lhsIinhMap
         _childrenOmergeMap = rule173 _mergeMap
         _childrenOnt = rule174 _lhsInt
         _childrenOsyn = rule175 _lhsIsyn
         _childrenOsynMap = rule176 _lhsIsynMap
         _rulesOallfields = rule177 _allfields
         _rulesOallnts = rule178 _lhsIallnts
         _rulesOattrs = rule179 _attrs
         _rulesOinh = rule180 _lhsIinh
         _rulesOmergeMap = rule181 _mergeMap
         _rulesOnt = rule182 _lhsInt
         _rulesOoptions = rule183 _lhsIoptions
         _rulesOsyn = rule184 _lhsIsyn
         __result_ = T_Production_vOut25 _lhsOcons _lhsOerrors _lhsOoutput
         in __result_ )
     in C_Production_s26 v25
   {-# INLINE rule157 #-}
   {-# LINE 66 "./src-ag/ResolveLocals.ag" #-}
   rule157 = \ con_ ->
                              {-# LINE 66 "./src-ag/ResolveLocals.ag" #-}
                              [con_]
                              {-# LINE 1333 "dist/build/ResolveLocals.hs"#-}
   {-# INLINE rule158 #-}
   {-# LINE 73 "./src-ag/ResolveLocals.ag" #-}
   rule158 = \ ((_childrenIfields) :: [(Identifier,Type,ChildKind)]) ->
                                  {-# LINE 73 "./src-ag/ResolveLocals.ag" #-}
                                  _childrenIfields
                                  {-# LINE 1339 "dist/build/ResolveLocals.hs"#-}
   {-# INLINE rule159 #-}
   {-# LINE 74 "./src-ag/ResolveLocals.ag" #-}
   rule159 = \ ((_childrenIattributes) :: [(Identifier,Attributes,Attributes)]) _inhnames ((_rulesIinstVars) :: [Identifier]) ((_rulesIlocVars) :: [Identifier]) ->
                                   {-# LINE 74 "./src-ag/ResolveLocals.ag" #-}
                                   map ((,) _LOC)  _rulesIlocVars ++
                                   map ((,) _INST) _rulesIinstVars ++
                                   map ((,) _LHS)  _inhnames ++
                                   concat [map ((,) nm) (Map.keys as) | (nm,_,as) <- _childrenIattributes]
                                   {-# LINE 1348 "dist/build/ResolveLocals.hs"#-}
   {-# INLINE rule160 #-}
   {-# LINE 78 "./src-ag/ResolveLocals.ag" #-}
   rule160 = \ ((_lhsIinh) :: Attributes) ->
                                   {-# LINE 78 "./src-ag/ResolveLocals.ag" #-}
                                   Map.keys _lhsIinh
                                   {-# LINE 1354 "dist/build/ResolveLocals.hs"#-}
   {-# INLINE rule161 #-}
   {-# LINE 79 "./src-ag/ResolveLocals.ag" #-}
   rule161 = \ ((_lhsIsyn) :: Attributes) ->
                                   {-# LINE 79 "./src-ag/ResolveLocals.ag" #-}
                                   Map.keys _lhsIsyn
                                   {-# LINE 1360 "dist/build/ResolveLocals.hs"#-}
   {-# INLINE rule162 #-}
   {-# LINE 107 "./src-ag/ResolveLocals.ag" #-}
   rule162 = \ con_ ->
                                  {-# LINE 107 "./src-ag/ResolveLocals.ag" #-}
                                  con_
                                  {-# LINE 1366 "dist/build/ResolveLocals.hs"#-}
   {-# INLINE rule163 #-}
   {-# LINE 109 "./src-ag/ResolveLocals.ag" #-}
   rule163 = \ con_ ->
                               {-# LINE 109 "./src-ag/ResolveLocals.ag" #-}
                               con_
                               {-# LINE 1372 "dist/build/ResolveLocals.hs"#-}
   {-# INLINE rule164 #-}
   {-# LINE 128 "./src-ag/ResolveLocals.ag" #-}
   rule164 = \ ((_lhsImergeMap) :: Map ConstructorIdent (Map Identifier (Identifier,[Identifier]))) con_ ->
                                                {-# LINE 128 "./src-ag/ResolveLocals.ag" #-}
                                                Map.findWithDefault Map.empty con_ _lhsImergeMap
                                                {-# LINE 1378 "dist/build/ResolveLocals.hs"#-}
   {-# INLINE rule165 #-}
   rule165 = \ ((_rulesIerrors) :: Seq Error) ->
     _rulesIerrors
   {-# INLINE rule166 #-}
   rule166 = \ ((_childrenIoutput) :: Children) ((_rulesIoutput) :: Rules) ((_typeSigsIoutput) :: TypeSigs) con_ constraints_ macro_ params_ ->
     Production con_ params_ constraints_ _childrenIoutput _rulesIoutput _typeSigsIoutput macro_
   {-# INLINE rule167 #-}
   rule167 = \ _output ->
     _output
   {-# INLINE rule168 #-}
   rule168 = \ _allfields ->
     _allfields
   {-# INLINE rule169 #-}
   rule169 = \ ((_lhsIallnts) :: [Identifier]) ->
     _lhsIallnts
   {-# INLINE rule170 #-}
   rule170 = \ _attrs ->
     _attrs
   {-# INLINE rule171 #-}
   rule171 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule172 #-}
   rule172 = \ ((_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# INLINE rule173 #-}
   rule173 = \ _mergeMap ->
     _mergeMap
   {-# INLINE rule174 #-}
   rule174 = \ ((_lhsInt) :: Identifier) ->
     _lhsInt
   {-# INLINE rule175 #-}
   rule175 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
   {-# INLINE rule176 #-}
   rule176 = \ ((_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
   {-# INLINE rule177 #-}
   rule177 = \ _allfields ->
     _allfields
   {-# INLINE rule178 #-}
   rule178 = \ ((_lhsIallnts) :: [Identifier]) ->
     _lhsIallnts
   {-# INLINE rule179 #-}
   rule179 = \ _attrs ->
     _attrs
   {-# INLINE rule180 #-}
   rule180 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule181 #-}
   rule181 = \ _mergeMap ->
     _mergeMap
   {-# INLINE rule182 #-}
   rule182 = \ ((_lhsInt) :: Identifier) ->
     _lhsInt
   {-# INLINE rule183 #-}
   rule183 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule184 #-}
   rule184 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn

-- Productions -------------------------------------------------
-- wrapper
data Inh_Productions  = Inh_Productions { allnts_Inh_Productions :: ([Identifier]), inh_Inh_Productions :: (Attributes), inhMap_Inh_Productions :: (Map Identifier Attributes), mergeMap_Inh_Productions :: (Map ConstructorIdent (Map Identifier (Identifier,[Identifier]))), nt_Inh_Productions :: (Identifier), options_Inh_Productions :: (Options), syn_Inh_Productions :: (Attributes), synMap_Inh_Productions :: (Map Identifier Attributes) }
data Syn_Productions  = Syn_Productions { cons_Syn_Productions :: ([ConstructorIdent]), errors_Syn_Productions :: (Seq Error), output_Syn_Productions :: (Productions) }
{-# INLINABLE wrap_Productions #-}
wrap_Productions :: T_Productions  -> Inh_Productions  -> (Syn_Productions )
wrap_Productions (T_Productions act) (Inh_Productions _lhsIallnts _lhsIinh _lhsIinhMap _lhsImergeMap _lhsInt _lhsIoptions _lhsIsyn _lhsIsynMap) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Productions_vIn28 _lhsIallnts _lhsIinh _lhsIinhMap _lhsImergeMap _lhsInt _lhsIoptions _lhsIsyn _lhsIsynMap
        (T_Productions_vOut28 _lhsOcons _lhsOerrors _lhsOoutput) <- return (inv_Productions_s29 sem arg)
        return (Syn_Productions _lhsOcons _lhsOerrors _lhsOoutput)
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
data T_Productions_vIn28  = T_Productions_vIn28 ([Identifier]) (Attributes) (Map Identifier Attributes) (Map ConstructorIdent (Map Identifier (Identifier,[Identifier]))) (Identifier) (Options) (Attributes) (Map Identifier Attributes)
data T_Productions_vOut28  = T_Productions_vOut28 ([ConstructorIdent]) (Seq Error) (Productions)
{-# NOINLINE sem_Productions_Cons #-}
sem_Productions_Cons :: T_Production  -> T_Productions  -> T_Productions 
sem_Productions_Cons arg_hd_ arg_tl_ = T_Productions (return st29) where
   {-# NOINLINE st29 #-}
   st29 = let
      v28 :: T_Productions_v28 
      v28 = \ (T_Productions_vIn28 _lhsIallnts _lhsIinh _lhsIinhMap _lhsImergeMap _lhsInt _lhsIoptions _lhsIsyn _lhsIsynMap) -> ( let
         _hdX26 = Control.Monad.Identity.runIdentity (attach_T_Production (arg_hd_))
         _tlX29 = Control.Monad.Identity.runIdentity (attach_T_Productions (arg_tl_))
         (T_Production_vOut25 _hdIcons _hdIerrors _hdIoutput) = inv_Production_s26 _hdX26 (T_Production_vIn25 _hdOallnts _hdOinh _hdOinhMap _hdOmergeMap _hdOnt _hdOoptions _hdOsyn _hdOsynMap)
         (T_Productions_vOut28 _tlIcons _tlIerrors _tlIoutput) = inv_Productions_s29 _tlX29 (T_Productions_vIn28 _tlOallnts _tlOinh _tlOinhMap _tlOmergeMap _tlOnt _tlOoptions _tlOsyn _tlOsynMap)
         _lhsOcons :: [ConstructorIdent]
         _lhsOcons = rule185 _hdIcons _tlIcons
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule186 _hdIerrors _tlIerrors
         _output = rule187 _hdIoutput _tlIoutput
         _lhsOoutput :: Productions
         _lhsOoutput = rule188 _output
         _hdOallnts = rule189 _lhsIallnts
         _hdOinh = rule190 _lhsIinh
         _hdOinhMap = rule191 _lhsIinhMap
         _hdOmergeMap = rule192 _lhsImergeMap
         _hdOnt = rule193 _lhsInt
         _hdOoptions = rule194 _lhsIoptions
         _hdOsyn = rule195 _lhsIsyn
         _hdOsynMap = rule196 _lhsIsynMap
         _tlOallnts = rule197 _lhsIallnts
         _tlOinh = rule198 _lhsIinh
         _tlOinhMap = rule199 _lhsIinhMap
         _tlOmergeMap = rule200 _lhsImergeMap
         _tlOnt = rule201 _lhsInt
         _tlOoptions = rule202 _lhsIoptions
         _tlOsyn = rule203 _lhsIsyn
         _tlOsynMap = rule204 _lhsIsynMap
         __result_ = T_Productions_vOut28 _lhsOcons _lhsOerrors _lhsOoutput
         in __result_ )
     in C_Productions_s29 v28
   {-# INLINE rule185 #-}
   rule185 = \ ((_hdIcons) :: [ConstructorIdent]) ((_tlIcons) :: [ConstructorIdent]) ->
     _hdIcons ++ _tlIcons
   {-# INLINE rule186 #-}
   rule186 = \ ((_hdIerrors) :: Seq Error) ((_tlIerrors) :: Seq Error) ->
     _hdIerrors Seq.>< _tlIerrors
   {-# INLINE rule187 #-}
   rule187 = \ ((_hdIoutput) :: Production) ((_tlIoutput) :: Productions) ->
     (:) _hdIoutput _tlIoutput
   {-# INLINE rule188 #-}
   rule188 = \ _output ->
     _output
   {-# INLINE rule189 #-}
   rule189 = \ ((_lhsIallnts) :: [Identifier]) ->
     _lhsIallnts
   {-# INLINE rule190 #-}
   rule190 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule191 #-}
   rule191 = \ ((_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# INLINE rule192 #-}
   rule192 = \ ((_lhsImergeMap) :: Map ConstructorIdent (Map Identifier (Identifier,[Identifier]))) ->
     _lhsImergeMap
   {-# INLINE rule193 #-}
   rule193 = \ ((_lhsInt) :: Identifier) ->
     _lhsInt
   {-# INLINE rule194 #-}
   rule194 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule195 #-}
   rule195 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
   {-# INLINE rule196 #-}
   rule196 = \ ((_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
   {-# INLINE rule197 #-}
   rule197 = \ ((_lhsIallnts) :: [Identifier]) ->
     _lhsIallnts
   {-# INLINE rule198 #-}
   rule198 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule199 #-}
   rule199 = \ ((_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# INLINE rule200 #-}
   rule200 = \ ((_lhsImergeMap) :: Map ConstructorIdent (Map Identifier (Identifier,[Identifier]))) ->
     _lhsImergeMap
   {-# INLINE rule201 #-}
   rule201 = \ ((_lhsInt) :: Identifier) ->
     _lhsInt
   {-# INLINE rule202 #-}
   rule202 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule203 #-}
   rule203 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
   {-# INLINE rule204 #-}
   rule204 = \ ((_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
{-# NOINLINE sem_Productions_Nil #-}
sem_Productions_Nil ::  T_Productions 
sem_Productions_Nil  = T_Productions (return st29) where
   {-# NOINLINE st29 #-}
   st29 = let
      v28 :: T_Productions_v28 
      v28 = \ (T_Productions_vIn28 _lhsIallnts _lhsIinh _lhsIinhMap _lhsImergeMap _lhsInt _lhsIoptions _lhsIsyn _lhsIsynMap) -> ( let
         _lhsOcons :: [ConstructorIdent]
         _lhsOcons = rule205  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule206  ()
         _output = rule207  ()
         _lhsOoutput :: Productions
         _lhsOoutput = rule208 _output
         __result_ = T_Productions_vOut28 _lhsOcons _lhsOerrors _lhsOoutput
         in __result_ )
     in C_Productions_s29 v28
   {-# INLINE rule205 #-}
   rule205 = \  (_ :: ()) ->
     []
   {-# INLINE rule206 #-}
   rule206 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule207 #-}
   rule207 = \  (_ :: ()) ->
     []
   {-# INLINE rule208 #-}
   rule208 = \ _output ->
     _output

-- Rule --------------------------------------------------------
-- wrapper
data Inh_Rule  = Inh_Rule { allfields_Inh_Rule :: ([(Identifier,Type,ChildKind)]), allnts_Inh_Rule :: ([Identifier]), attrs_Inh_Rule :: ([(Identifier,Identifier)]), con_Inh_Rule :: (Identifier), inh_Inh_Rule :: (Attributes), mergeMap_Inh_Rule :: (Map Identifier (Identifier,[Identifier])), nt_Inh_Rule :: (Identifier), options_Inh_Rule :: (Options), syn_Inh_Rule :: (Attributes) }
data Syn_Rule  = Syn_Rule { errors_Syn_Rule :: (Seq Error), instVars_Syn_Rule :: ([Identifier]), locVars_Syn_Rule :: ([Identifier]), output_Syn_Rule :: (Rule) }
{-# INLINABLE wrap_Rule #-}
wrap_Rule :: T_Rule  -> Inh_Rule  -> (Syn_Rule )
wrap_Rule (T_Rule act) (Inh_Rule _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsIinh _lhsImergeMap _lhsInt _lhsIoptions _lhsIsyn) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Rule_vIn31 _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsIinh _lhsImergeMap _lhsInt _lhsIoptions _lhsIsyn
        (T_Rule_vOut31 _lhsOerrors _lhsOinstVars _lhsOlocVars _lhsOoutput) <- return (inv_Rule_s32 sem arg)
        return (Syn_Rule _lhsOerrors _lhsOinstVars _lhsOlocVars _lhsOoutput)
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
data T_Rule_vIn31  = T_Rule_vIn31 ([(Identifier,Type,ChildKind)]) ([Identifier]) ([(Identifier,Identifier)]) (Identifier) (Attributes) (Map Identifier (Identifier,[Identifier])) (Identifier) (Options) (Attributes)
data T_Rule_vOut31  = T_Rule_vOut31 (Seq Error) ([Identifier]) ([Identifier]) (Rule)
{-# NOINLINE sem_Rule_Rule #-}
sem_Rule_Rule :: (Maybe Identifier) -> T_Pattern  -> T_Expression  -> (Bool) -> (String) -> (Bool) -> (Bool) -> (Bool) -> (Maybe Error) -> (Bool) -> T_Rule 
sem_Rule_Rule arg_mbName_ arg_pattern_ arg_rhs_ arg_owrt_ arg_origin_ arg_explicit_ arg_pure_ arg_identity_ arg_mbError_ arg_eager_ = T_Rule (return st32) where
   {-# NOINLINE st32 #-}
   st32 = let
      v31 :: T_Rule_v31 
      v31 = \ (T_Rule_vIn31 _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsIinh _lhsImergeMap _lhsInt _lhsIoptions _lhsIsyn) -> ( let
         _patternX20 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pattern_))
         _rhsX8 = Control.Monad.Identity.runIdentity (attach_T_Expression (arg_rhs_))
         (T_Pattern_vOut19 _patternIcopy _patternIerrors _patternIinstVars _patternIlocVars _patternIoutput) = inv_Pattern_s20 _patternX20 (T_Pattern_vIn19 _patternOcon _patternOinh _patternOnt _patternOsyn)
         (T_Expression_vOut7 _rhsIerrors _rhsIoutput) = inv_Expression_s8 _rhsX8 (T_Expression_vIn7 _rhsOallfields _rhsOallnts _rhsOattrs _rhsOcon _rhsOmergeMap _rhsOnt _rhsOoptions)
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule209 _patternIerrors _rhsIerrors
         _lhsOinstVars :: [Identifier]
         _lhsOinstVars = rule210 _patternIinstVars
         _lhsOlocVars :: [Identifier]
         _lhsOlocVars = rule211 _patternIlocVars
         _output = rule212 _patternIoutput _rhsIoutput arg_eager_ arg_explicit_ arg_identity_ arg_mbError_ arg_mbName_ arg_origin_ arg_owrt_ arg_pure_
         _lhsOoutput :: Rule
         _lhsOoutput = rule213 _output
         _patternOcon = rule214 _lhsIcon
         _patternOinh = rule215 _lhsIinh
         _patternOnt = rule216 _lhsInt
         _patternOsyn = rule217 _lhsIsyn
         _rhsOallfields = rule218 _lhsIallfields
         _rhsOallnts = rule219 _lhsIallnts
         _rhsOattrs = rule220 _lhsIattrs
         _rhsOcon = rule221 _lhsIcon
         _rhsOmergeMap = rule222 _lhsImergeMap
         _rhsOnt = rule223 _lhsInt
         _rhsOoptions = rule224 _lhsIoptions
         __result_ = T_Rule_vOut31 _lhsOerrors _lhsOinstVars _lhsOlocVars _lhsOoutput
         in __result_ )
     in C_Rule_s32 v31
   {-# INLINE rule209 #-}
   rule209 = \ ((_patternIerrors) :: Seq Error) ((_rhsIerrors) :: Seq Error) ->
     _patternIerrors Seq.>< _rhsIerrors
   {-# INLINE rule210 #-}
   rule210 = \ ((_patternIinstVars) :: [Identifier]) ->
     _patternIinstVars
   {-# INLINE rule211 #-}
   rule211 = \ ((_patternIlocVars) :: [Identifier]) ->
     _patternIlocVars
   {-# INLINE rule212 #-}
   rule212 = \ ((_patternIoutput) :: Pattern) ((_rhsIoutput) :: Expression) eager_ explicit_ identity_ mbError_ mbName_ origin_ owrt_ pure_ ->
     Rule mbName_ _patternIoutput _rhsIoutput owrt_ origin_ explicit_ pure_ identity_ mbError_ eager_
   {-# INLINE rule213 #-}
   rule213 = \ _output ->
     _output
   {-# INLINE rule214 #-}
   rule214 = \ ((_lhsIcon) :: Identifier) ->
     _lhsIcon
   {-# INLINE rule215 #-}
   rule215 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule216 #-}
   rule216 = \ ((_lhsInt) :: Identifier) ->
     _lhsInt
   {-# INLINE rule217 #-}
   rule217 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
   {-# INLINE rule218 #-}
   rule218 = \ ((_lhsIallfields) :: [(Identifier,Type,ChildKind)]) ->
     _lhsIallfields
   {-# INLINE rule219 #-}
   rule219 = \ ((_lhsIallnts) :: [Identifier]) ->
     _lhsIallnts
   {-# INLINE rule220 #-}
   rule220 = \ ((_lhsIattrs) :: [(Identifier,Identifier)]) ->
     _lhsIattrs
   {-# INLINE rule221 #-}
   rule221 = \ ((_lhsIcon) :: Identifier) ->
     _lhsIcon
   {-# INLINE rule222 #-}
   rule222 = \ ((_lhsImergeMap) :: Map Identifier (Identifier,[Identifier])) ->
     _lhsImergeMap
   {-# INLINE rule223 #-}
   rule223 = \ ((_lhsInt) :: Identifier) ->
     _lhsInt
   {-# INLINE rule224 #-}
   rule224 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions

-- Rules -------------------------------------------------------
-- wrapper
data Inh_Rules  = Inh_Rules { allfields_Inh_Rules :: ([(Identifier,Type,ChildKind)]), allnts_Inh_Rules :: ([Identifier]), attrs_Inh_Rules :: ([(Identifier,Identifier)]), con_Inh_Rules :: (Identifier), inh_Inh_Rules :: (Attributes), mergeMap_Inh_Rules :: (Map Identifier (Identifier,[Identifier])), nt_Inh_Rules :: (Identifier), options_Inh_Rules :: (Options), syn_Inh_Rules :: (Attributes) }
data Syn_Rules  = Syn_Rules { errors_Syn_Rules :: (Seq Error), instVars_Syn_Rules :: ([Identifier]), locVars_Syn_Rules :: ([Identifier]), output_Syn_Rules :: (Rules) }
{-# INLINABLE wrap_Rules #-}
wrap_Rules :: T_Rules  -> Inh_Rules  -> (Syn_Rules )
wrap_Rules (T_Rules act) (Inh_Rules _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsIinh _lhsImergeMap _lhsInt _lhsIoptions _lhsIsyn) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Rules_vIn34 _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsIinh _lhsImergeMap _lhsInt _lhsIoptions _lhsIsyn
        (T_Rules_vOut34 _lhsOerrors _lhsOinstVars _lhsOlocVars _lhsOoutput) <- return (inv_Rules_s35 sem arg)
        return (Syn_Rules _lhsOerrors _lhsOinstVars _lhsOlocVars _lhsOoutput)
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
data T_Rules_vIn34  = T_Rules_vIn34 ([(Identifier,Type,ChildKind)]) ([Identifier]) ([(Identifier,Identifier)]) (Identifier) (Attributes) (Map Identifier (Identifier,[Identifier])) (Identifier) (Options) (Attributes)
data T_Rules_vOut34  = T_Rules_vOut34 (Seq Error) ([Identifier]) ([Identifier]) (Rules)
{-# NOINLINE sem_Rules_Cons #-}
sem_Rules_Cons :: T_Rule  -> T_Rules  -> T_Rules 
sem_Rules_Cons arg_hd_ arg_tl_ = T_Rules (return st35) where
   {-# NOINLINE st35 #-}
   st35 = let
      v34 :: T_Rules_v34 
      v34 = \ (T_Rules_vIn34 _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsIinh _lhsImergeMap _lhsInt _lhsIoptions _lhsIsyn) -> ( let
         _hdX32 = Control.Monad.Identity.runIdentity (attach_T_Rule (arg_hd_))
         _tlX35 = Control.Monad.Identity.runIdentity (attach_T_Rules (arg_tl_))
         (T_Rule_vOut31 _hdIerrors _hdIinstVars _hdIlocVars _hdIoutput) = inv_Rule_s32 _hdX32 (T_Rule_vIn31 _hdOallfields _hdOallnts _hdOattrs _hdOcon _hdOinh _hdOmergeMap _hdOnt _hdOoptions _hdOsyn)
         (T_Rules_vOut34 _tlIerrors _tlIinstVars _tlIlocVars _tlIoutput) = inv_Rules_s35 _tlX35 (T_Rules_vIn34 _tlOallfields _tlOallnts _tlOattrs _tlOcon _tlOinh _tlOmergeMap _tlOnt _tlOoptions _tlOsyn)
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule225 _hdIerrors _tlIerrors
         _lhsOinstVars :: [Identifier]
         _lhsOinstVars = rule226 _hdIinstVars _tlIinstVars
         _lhsOlocVars :: [Identifier]
         _lhsOlocVars = rule227 _hdIlocVars _tlIlocVars
         _output = rule228 _hdIoutput _tlIoutput
         _lhsOoutput :: Rules
         _lhsOoutput = rule229 _output
         _hdOallfields = rule230 _lhsIallfields
         _hdOallnts = rule231 _lhsIallnts
         _hdOattrs = rule232 _lhsIattrs
         _hdOcon = rule233 _lhsIcon
         _hdOinh = rule234 _lhsIinh
         _hdOmergeMap = rule235 _lhsImergeMap
         _hdOnt = rule236 _lhsInt
         _hdOoptions = rule237 _lhsIoptions
         _hdOsyn = rule238 _lhsIsyn
         _tlOallfields = rule239 _lhsIallfields
         _tlOallnts = rule240 _lhsIallnts
         _tlOattrs = rule241 _lhsIattrs
         _tlOcon = rule242 _lhsIcon
         _tlOinh = rule243 _lhsIinh
         _tlOmergeMap = rule244 _lhsImergeMap
         _tlOnt = rule245 _lhsInt
         _tlOoptions = rule246 _lhsIoptions
         _tlOsyn = rule247 _lhsIsyn
         __result_ = T_Rules_vOut34 _lhsOerrors _lhsOinstVars _lhsOlocVars _lhsOoutput
         in __result_ )
     in C_Rules_s35 v34
   {-# INLINE rule225 #-}
   rule225 = \ ((_hdIerrors) :: Seq Error) ((_tlIerrors) :: Seq Error) ->
     _hdIerrors Seq.>< _tlIerrors
   {-# INLINE rule226 #-}
   rule226 = \ ((_hdIinstVars) :: [Identifier]) ((_tlIinstVars) :: [Identifier]) ->
     _hdIinstVars ++ _tlIinstVars
   {-# INLINE rule227 #-}
   rule227 = \ ((_hdIlocVars) :: [Identifier]) ((_tlIlocVars) :: [Identifier]) ->
     _hdIlocVars ++ _tlIlocVars
   {-# INLINE rule228 #-}
   rule228 = \ ((_hdIoutput) :: Rule) ((_tlIoutput) :: Rules) ->
     (:) _hdIoutput _tlIoutput
   {-# INLINE rule229 #-}
   rule229 = \ _output ->
     _output
   {-# INLINE rule230 #-}
   rule230 = \ ((_lhsIallfields) :: [(Identifier,Type,ChildKind)]) ->
     _lhsIallfields
   {-# INLINE rule231 #-}
   rule231 = \ ((_lhsIallnts) :: [Identifier]) ->
     _lhsIallnts
   {-# INLINE rule232 #-}
   rule232 = \ ((_lhsIattrs) :: [(Identifier,Identifier)]) ->
     _lhsIattrs
   {-# INLINE rule233 #-}
   rule233 = \ ((_lhsIcon) :: Identifier) ->
     _lhsIcon
   {-# INLINE rule234 #-}
   rule234 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule235 #-}
   rule235 = \ ((_lhsImergeMap) :: Map Identifier (Identifier,[Identifier])) ->
     _lhsImergeMap
   {-# INLINE rule236 #-}
   rule236 = \ ((_lhsInt) :: Identifier) ->
     _lhsInt
   {-# INLINE rule237 #-}
   rule237 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule238 #-}
   rule238 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
   {-# INLINE rule239 #-}
   rule239 = \ ((_lhsIallfields) :: [(Identifier,Type,ChildKind)]) ->
     _lhsIallfields
   {-# INLINE rule240 #-}
   rule240 = \ ((_lhsIallnts) :: [Identifier]) ->
     _lhsIallnts
   {-# INLINE rule241 #-}
   rule241 = \ ((_lhsIattrs) :: [(Identifier,Identifier)]) ->
     _lhsIattrs
   {-# INLINE rule242 #-}
   rule242 = \ ((_lhsIcon) :: Identifier) ->
     _lhsIcon
   {-# INLINE rule243 #-}
   rule243 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule244 #-}
   rule244 = \ ((_lhsImergeMap) :: Map Identifier (Identifier,[Identifier])) ->
     _lhsImergeMap
   {-# INLINE rule245 #-}
   rule245 = \ ((_lhsInt) :: Identifier) ->
     _lhsInt
   {-# INLINE rule246 #-}
   rule246 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule247 #-}
   rule247 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
{-# NOINLINE sem_Rules_Nil #-}
sem_Rules_Nil ::  T_Rules 
sem_Rules_Nil  = T_Rules (return st35) where
   {-# NOINLINE st35 #-}
   st35 = let
      v34 :: T_Rules_v34 
      v34 = \ (T_Rules_vIn34 _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsIinh _lhsImergeMap _lhsInt _lhsIoptions _lhsIsyn) -> ( let
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule248  ()
         _lhsOinstVars :: [Identifier]
         _lhsOinstVars = rule249  ()
         _lhsOlocVars :: [Identifier]
         _lhsOlocVars = rule250  ()
         _output = rule251  ()
         _lhsOoutput :: Rules
         _lhsOoutput = rule252 _output
         __result_ = T_Rules_vOut34 _lhsOerrors _lhsOinstVars _lhsOlocVars _lhsOoutput
         in __result_ )
     in C_Rules_s35 v34
   {-# INLINE rule248 #-}
   rule248 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule249 #-}
   rule249 = \  (_ :: ()) ->
     []
   {-# INLINE rule250 #-}
   rule250 = \  (_ :: ()) ->
     []
   {-# INLINE rule251 #-}
   rule251 = \  (_ :: ()) ->
     []
   {-# INLINE rule252 #-}
   rule252 = \ _output ->
     _output

-- TypeSig -----------------------------------------------------
-- wrapper
data Inh_TypeSig  = Inh_TypeSig {  }
data Syn_TypeSig  = Syn_TypeSig { output_Syn_TypeSig :: (TypeSig) }
{-# INLINABLE wrap_TypeSig #-}
wrap_TypeSig :: T_TypeSig  -> Inh_TypeSig  -> (Syn_TypeSig )
wrap_TypeSig (T_TypeSig act) (Inh_TypeSig ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_TypeSig_vIn37 
        (T_TypeSig_vOut37 _lhsOoutput) <- return (inv_TypeSig_s38 sem arg)
        return (Syn_TypeSig _lhsOoutput)
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
data T_TypeSig_vOut37  = T_TypeSig_vOut37 (TypeSig)
{-# NOINLINE sem_TypeSig_TypeSig #-}
sem_TypeSig_TypeSig :: (Identifier) -> (Type) -> T_TypeSig 
sem_TypeSig_TypeSig arg_name_ arg_tp_ = T_TypeSig (return st38) where
   {-# NOINLINE st38 #-}
   st38 = let
      v37 :: T_TypeSig_v37 
      v37 = \ (T_TypeSig_vIn37 ) -> ( let
         _output = rule253 arg_name_ arg_tp_
         _lhsOoutput :: TypeSig
         _lhsOoutput = rule254 _output
         __result_ = T_TypeSig_vOut37 _lhsOoutput
         in __result_ )
     in C_TypeSig_s38 v37
   {-# INLINE rule253 #-}
   rule253 = \ name_ tp_ ->
     TypeSig name_ tp_
   {-# INLINE rule254 #-}
   rule254 = \ _output ->
     _output

-- TypeSigs ----------------------------------------------------
-- wrapper
data Inh_TypeSigs  = Inh_TypeSigs {  }
data Syn_TypeSigs  = Syn_TypeSigs { output_Syn_TypeSigs :: (TypeSigs) }
{-# INLINABLE wrap_TypeSigs #-}
wrap_TypeSigs :: T_TypeSigs  -> Inh_TypeSigs  -> (Syn_TypeSigs )
wrap_TypeSigs (T_TypeSigs act) (Inh_TypeSigs ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_TypeSigs_vIn40 
        (T_TypeSigs_vOut40 _lhsOoutput) <- return (inv_TypeSigs_s41 sem arg)
        return (Syn_TypeSigs _lhsOoutput)
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
data T_TypeSigs_vOut40  = T_TypeSigs_vOut40 (TypeSigs)
{-# NOINLINE sem_TypeSigs_Cons #-}
sem_TypeSigs_Cons :: T_TypeSig  -> T_TypeSigs  -> T_TypeSigs 
sem_TypeSigs_Cons arg_hd_ arg_tl_ = T_TypeSigs (return st41) where
   {-# NOINLINE st41 #-}
   st41 = let
      v40 :: T_TypeSigs_v40 
      v40 = \ (T_TypeSigs_vIn40 ) -> ( let
         _hdX38 = Control.Monad.Identity.runIdentity (attach_T_TypeSig (arg_hd_))
         _tlX41 = Control.Monad.Identity.runIdentity (attach_T_TypeSigs (arg_tl_))
         (T_TypeSig_vOut37 _hdIoutput) = inv_TypeSig_s38 _hdX38 (T_TypeSig_vIn37 )
         (T_TypeSigs_vOut40 _tlIoutput) = inv_TypeSigs_s41 _tlX41 (T_TypeSigs_vIn40 )
         _output = rule255 _hdIoutput _tlIoutput
         _lhsOoutput :: TypeSigs
         _lhsOoutput = rule256 _output
         __result_ = T_TypeSigs_vOut40 _lhsOoutput
         in __result_ )
     in C_TypeSigs_s41 v40
   {-# INLINE rule255 #-}
   rule255 = \ ((_hdIoutput) :: TypeSig) ((_tlIoutput) :: TypeSigs) ->
     (:) _hdIoutput _tlIoutput
   {-# INLINE rule256 #-}
   rule256 = \ _output ->
     _output
{-# NOINLINE sem_TypeSigs_Nil #-}
sem_TypeSigs_Nil ::  T_TypeSigs 
sem_TypeSigs_Nil  = T_TypeSigs (return st41) where
   {-# NOINLINE st41 #-}
   st41 = let
      v40 :: T_TypeSigs_v40 
      v40 = \ (T_TypeSigs_vIn40 ) -> ( let
         _output = rule257  ()
         _lhsOoutput :: TypeSigs
         _lhsOoutput = rule258 _output
         __result_ = T_TypeSigs_vOut40 _lhsOoutput
         in __result_ )
     in C_TypeSigs_s41 v40
   {-# INLINE rule257 #-}
   rule257 = \  (_ :: ()) ->
     []
   {-# INLINE rule258 #-}
   rule258 = \ _output ->
     _output
