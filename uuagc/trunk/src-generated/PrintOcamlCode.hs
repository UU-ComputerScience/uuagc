{-# LANGUAGE Rank2Types, GADTs #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PrintOcamlCode where
{-# LINE 2 "./src-ag/Code.ag" #-}

import Patterns
import Data.Set(Set)
import qualified Data.Set as Set
import Data.Map(Map)
import qualified Data.Map as Map
{-# LINE 14 "dist/build/PrintOcamlCode.hs" #-}

{-# LINE 2 "./src-ag/Patterns.ag" #-}

-- Patterns.ag imports
import UU.Scanner.Position(Pos)
import CommonTypes (ConstructorIdent,Identifier)
{-# LINE 21 "dist/build/PrintOcamlCode.hs" #-}

{-# LINE 10 "./src-ag/PrintOcamlCode.ag" #-}

import Pretty
import Code
import Patterns
import Options
import CommonTypes hiding (List,Type,Map,Maybe,IntMap,Either)
import Data.List(intersperse,intercalate)
import Data.Char(toLower)
{-# LINE 32 "dist/build/PrintOcamlCode.hs" #-}
import Control.Monad.Identity (Identity)
import qualified Control.Monad.Identity
{-# LINE 144 "./src-ag/Code.ag" #-}

-- Unboxed tuples
--   unbox  Whether unboxed tuples are wanted or not
--   inh    The inherited attributes.
--          If there are none, no unboxing can take place,
--          because in that case the semantic function (a top-level identifier) would have an unboxed type.
-- Of course we can't have an unboxed 1-tuple
mkTupleExpr :: Bool -> Bool -> Exprs -> Expr
mkTupleExpr unbox' noInh exprs | not unbox' || noInh || length exprs == 1 = TupleExpr exprs
                               | otherwise                                = UnboxedTupleExpr exprs
mkTupleType :: Bool -> Bool -> Types -> Type
mkTupleType unbox' noInh tps | not unbox' || noInh || length tps == 1 = TupleType tps
                             | otherwise                              = UnboxedTupleType tps
mkTupleLhs :: Bool -> Bool -> [String] -> Lhs
mkTupleLhs  unbox' noInh comps | not unbox' || noInh || length comps == 1 = TupleLhs comps
                               | otherwise                                = UnboxedTupleLhs comps
{-# LINE 52 "dist/build/PrintOcamlCode.hs" #-}

{-# LINE 21 "./src-ag/PrintOcamlCode.ag" #-}

type PP_Docs = [PP_Doc]

ppMultiSeqH :: [PP_Doc] -> PP_Doc -> PP_Doc
ppMultiSeqH = ppMultiSeq' (>#<)

ppMultiSeqV :: [PP_Doc] -> PP_Doc -> PP_Doc
ppMultiSeqV = ppMultiSeq' (>-<)

ppMultiSeq' :: (PP_Doc -> PP_Doc -> PP_Doc) -> [PP_Doc] -> PP_Doc -> PP_Doc
ppMultiSeq' next strictArgs expr
  = foldr (\v r -> (v >#< "`seq`") `next` pp_parens r) expr strictArgs

ppTuple :: Bool -> [PP_Doc] -> PP_Doc
ppTuple True  pps = "(" >|< pp_block " " (replicate (length pps `max` 1) ')') ",(" pps
ppTuple False pps = "(" >|< pp_block " " ")" "," pps
{-# LINE 71 "dist/build/PrintOcamlCode.hs" #-}

{-# LINE 175 "./src-ag/PrintOcamlCode.ag" #-}

toOcamlTC :: String -> String
toOcamlTC (c:cs) = toLower c : cs
toOcamlTC xs = xs
{-# LINE 78 "dist/build/PrintOcamlCode.hs" #-}
-- CaseAlt -----------------------------------------------------
-- wrapper
data Inh_CaseAlt  = Inh_CaseAlt { options_Inh_CaseAlt :: !(Options) }
data Syn_CaseAlt  = Syn_CaseAlt { pp_Syn_CaseAlt :: !(PP_Doc) }
{-# INLINABLE wrap_CaseAlt #-}
wrap_CaseAlt :: T_CaseAlt  -> Inh_CaseAlt  -> (Syn_CaseAlt )
wrap_CaseAlt !(T_CaseAlt act) !(Inh_CaseAlt _lhsIoptions) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_CaseAlt_vIn1 _lhsIoptions
        !(T_CaseAlt_vOut1 _lhsOpp) <- return (inv_CaseAlt_s2 sem arg)
        return (Syn_CaseAlt _lhsOpp)
   )

-- cata
{-# NOINLINE sem_CaseAlt #-}
sem_CaseAlt :: CaseAlt  -> T_CaseAlt 
sem_CaseAlt ( CaseAlt left_ expr_ ) = sem_CaseAlt_CaseAlt ( sem_Lhs left_ ) ( sem_Expr expr_ )

-- semantic domain
newtype T_CaseAlt  = T_CaseAlt {
                               attach_T_CaseAlt :: Identity (T_CaseAlt_s2 )
                               }
newtype T_CaseAlt_s2  = C_CaseAlt_s2 {
                                     inv_CaseAlt_s2 :: (T_CaseAlt_v1 )
                                     }
data T_CaseAlt_s3  = C_CaseAlt_s3
type T_CaseAlt_v1  = (T_CaseAlt_vIn1 ) -> (T_CaseAlt_vOut1 )
data T_CaseAlt_vIn1  = T_CaseAlt_vIn1 (Options)
data T_CaseAlt_vOut1  = T_CaseAlt_vOut1 (PP_Doc)
{-# NOINLINE sem_CaseAlt_CaseAlt #-}
sem_CaseAlt_CaseAlt :: T_Lhs  -> T_Expr  -> T_CaseAlt 
sem_CaseAlt_CaseAlt arg_left_ arg_expr_ = T_CaseAlt (return st2) where
   {-# NOINLINE st2 #-}
   !st2 = let
      v1 :: T_CaseAlt_v1 
      v1 = \ !(T_CaseAlt_vIn1 _lhsIoptions) -> ( let
         _leftX32 = Control.Monad.Identity.runIdentity (attach_T_Lhs (arg_left_))
         _exprX26 = Control.Monad.Identity.runIdentity (attach_T_Expr (arg_expr_))
         (T_Lhs_vOut31 _leftIpp) = inv_Lhs_s32 _leftX32 (T_Lhs_vIn31 _leftOoptions)
         (T_Expr_vOut25 _exprIpp) = inv_Expr_s26 _exprX26 (T_Expr_vIn25 _exprOoptions)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule0 _exprIpp _leftIpp
         _leftOoptions = rule1 _lhsIoptions
         _exprOoptions = rule2 _lhsIoptions
         !__result_ = T_CaseAlt_vOut1 _lhsOpp
         in __result_ )
     in C_CaseAlt_s2 v1
   {-# INLINE rule0 #-}
   {-# LINE 182 "./src-ag/PrintOcamlCode.ag" #-}
   rule0 = \ ((_exprIpp) :: PP_Doc) ((_leftIpp) :: PP_Doc) ->
                               {-# LINE 182 "./src-ag/PrintOcamlCode.ag" #-}
                               _leftIpp >#< "->" >#< _exprIpp
                               {-# LINE 132 "dist/build/PrintOcamlCode.hs"#-}
   {-# INLINE rule1 #-}
   rule1 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule2 #-}
   rule2 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions

-- CaseAlts ----------------------------------------------------
-- wrapper
data Inh_CaseAlts  = Inh_CaseAlts { options_Inh_CaseAlts :: !(Options) }
data Syn_CaseAlts  = Syn_CaseAlts { pps_Syn_CaseAlts :: !(PP_Docs) }
{-# INLINABLE wrap_CaseAlts #-}
wrap_CaseAlts :: T_CaseAlts  -> Inh_CaseAlts  -> (Syn_CaseAlts )
wrap_CaseAlts !(T_CaseAlts act) !(Inh_CaseAlts _lhsIoptions) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_CaseAlts_vIn4 _lhsIoptions
        !(T_CaseAlts_vOut4 _lhsOpps) <- return (inv_CaseAlts_s5 sem arg)
        return (Syn_CaseAlts _lhsOpps)
   )

-- cata
{-# NOINLINE sem_CaseAlts #-}
sem_CaseAlts :: CaseAlts  -> T_CaseAlts 
sem_CaseAlts list = Prelude.foldr sem_CaseAlts_Cons sem_CaseAlts_Nil (Prelude.map sem_CaseAlt list)

-- semantic domain
newtype T_CaseAlts  = T_CaseAlts {
                                 attach_T_CaseAlts :: Identity (T_CaseAlts_s5 )
                                 }
newtype T_CaseAlts_s5  = C_CaseAlts_s5 {
                                       inv_CaseAlts_s5 :: (T_CaseAlts_v4 )
                                       }
data T_CaseAlts_s6  = C_CaseAlts_s6
type T_CaseAlts_v4  = (T_CaseAlts_vIn4 ) -> (T_CaseAlts_vOut4 )
data T_CaseAlts_vIn4  = T_CaseAlts_vIn4 (Options)
data T_CaseAlts_vOut4  = T_CaseAlts_vOut4 (PP_Docs)
{-# NOINLINE sem_CaseAlts_Cons #-}
sem_CaseAlts_Cons :: T_CaseAlt  -> T_CaseAlts  -> T_CaseAlts 
sem_CaseAlts_Cons arg_hd_ arg_tl_ = T_CaseAlts (return st5) where
   {-# NOINLINE st5 #-}
   !st5 = let
      v4 :: T_CaseAlts_v4 
      v4 = \ !(T_CaseAlts_vIn4 _lhsIoptions) -> ( let
         _hdX2 = Control.Monad.Identity.runIdentity (attach_T_CaseAlt (arg_hd_))
         _tlX5 = Control.Monad.Identity.runIdentity (attach_T_CaseAlts (arg_tl_))
         (T_CaseAlt_vOut1 _hdIpp) = inv_CaseAlt_s2 _hdX2 (T_CaseAlt_vIn1 _hdOoptions)
         (T_CaseAlts_vOut4 _tlIpps) = inv_CaseAlts_s5 _tlX5 (T_CaseAlts_vIn4 _tlOoptions)
         _lhsOpps :: PP_Docs
         _lhsOpps = rule3 _hdIpp _tlIpps
         _hdOoptions = rule4 _lhsIoptions
         _tlOoptions = rule5 _lhsIoptions
         !__result_ = T_CaseAlts_vOut4 _lhsOpps
         in __result_ )
     in C_CaseAlts_s5 v4
   {-# INLINE rule3 #-}
   {-# LINE 65 "./src-ag/PrintOcamlCode.ag" #-}
   rule3 = \ ((_hdIpp) :: PP_Doc) ((_tlIpps) :: PP_Docs) ->
                     {-# LINE 65 "./src-ag/PrintOcamlCode.ag" #-}
                     _hdIpp : _tlIpps
                     {-# LINE 193 "dist/build/PrintOcamlCode.hs"#-}
   {-# INLINE rule4 #-}
   rule4 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule5 #-}
   rule5 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_CaseAlts_Nil #-}
sem_CaseAlts_Nil ::  T_CaseAlts 
sem_CaseAlts_Nil  = T_CaseAlts (return st5) where
   {-# NOINLINE st5 #-}
   !st5 = let
      v4 :: T_CaseAlts_v4 
      v4 = \ !(T_CaseAlts_vIn4 _lhsIoptions) -> ( let
         _lhsOpps :: PP_Docs
         _lhsOpps = rule6  ()
         !__result_ = T_CaseAlts_vOut4 _lhsOpps
         in __result_ )
     in C_CaseAlts_s5 v4
   {-# INLINE rule6 #-}
   {-# LINE 66 "./src-ag/PrintOcamlCode.ag" #-}
   rule6 = \  (_ :: ()) ->
                     {-# LINE 66 "./src-ag/PrintOcamlCode.ag" #-}
                     []
                     {-# LINE 217 "dist/build/PrintOcamlCode.hs"#-}

-- Chunk -------------------------------------------------------
-- wrapper
data Inh_Chunk  = Inh_Chunk { isToplevel_Inh_Chunk :: !(Bool), options_Inh_Chunk :: !(Options), textBlockMap_Inh_Chunk :: !(Map BlockInfo PP_Doc) }
data Syn_Chunk  = Syn_Chunk { pps_Syn_Chunk :: !(PP_Docs) }
{-# INLINABLE wrap_Chunk #-}
wrap_Chunk :: T_Chunk  -> Inh_Chunk  -> (Syn_Chunk )
wrap_Chunk !(T_Chunk act) !(Inh_Chunk _lhsIisToplevel _lhsIoptions _lhsItextBlockMap) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_Chunk_vIn7 _lhsIisToplevel _lhsIoptions _lhsItextBlockMap
        !(T_Chunk_vOut7 _lhsOpps) <- return (inv_Chunk_s8 sem arg)
        return (Syn_Chunk _lhsOpps)
   )

-- cata
{-# INLINE sem_Chunk #-}
sem_Chunk :: Chunk  -> T_Chunk 
sem_Chunk ( Chunk !name_ comment_ info_ dataDef_ cataFun_ semDom_ semWrapper_ semFunctions_ !semNames_ ) = sem_Chunk_Chunk name_ ( sem_Decl comment_ ) ( sem_Decls info_ ) ( sem_Decls dataDef_ ) ( sem_Decls cataFun_ ) ( sem_Decls semDom_ ) ( sem_Decls semWrapper_ ) ( sem_Decls semFunctions_ ) semNames_

-- semantic domain
newtype T_Chunk  = T_Chunk {
                           attach_T_Chunk :: Identity (T_Chunk_s8 )
                           }
newtype T_Chunk_s8  = C_Chunk_s8 {
                                 inv_Chunk_s8 :: (T_Chunk_v7 )
                                 }
data T_Chunk_s9  = C_Chunk_s9
type T_Chunk_v7  = (T_Chunk_vIn7 ) -> (T_Chunk_vOut7 )
data T_Chunk_vIn7  = T_Chunk_vIn7 (Bool) (Options) (Map BlockInfo PP_Doc)
data T_Chunk_vOut7  = T_Chunk_vOut7 (PP_Docs)
{-# NOINLINE sem_Chunk_Chunk #-}
sem_Chunk_Chunk :: (String) -> T_Decl  -> T_Decls  -> T_Decls  -> T_Decls  -> T_Decls  -> T_Decls  -> T_Decls  -> ([String]) -> T_Chunk 
sem_Chunk_Chunk !arg_name_ arg_comment_ arg_info_ arg_dataDef_ arg_cataFun_ arg_semDom_ arg_semWrapper_ arg_semFunctions_ _ = T_Chunk (return st8) where
   {-# NOINLINE st8 #-}
   !st8 = let
      v7 :: T_Chunk_v7 
      v7 = \ !(T_Chunk_vIn7 _lhsIisToplevel _lhsIoptions _lhsItextBlockMap) -> ( let
         _commentX20 = Control.Monad.Identity.runIdentity (attach_T_Decl (arg_comment_))
         _infoX23 = Control.Monad.Identity.runIdentity (attach_T_Decls (arg_info_))
         _dataDefX23 = Control.Monad.Identity.runIdentity (attach_T_Decls (arg_dataDef_))
         _cataFunX23 = Control.Monad.Identity.runIdentity (attach_T_Decls (arg_cataFun_))
         _semDomX23 = Control.Monad.Identity.runIdentity (attach_T_Decls (arg_semDom_))
         _semWrapperX23 = Control.Monad.Identity.runIdentity (attach_T_Decls (arg_semWrapper_))
         _semFunctionsX23 = Control.Monad.Identity.runIdentity (attach_T_Decls (arg_semFunctions_))
         (T_Decl_vOut19 _commentIpp) = inv_Decl_s20 _commentX20 (T_Decl_vIn19 _commentOisToplevel _commentOoptions)
         (T_Decls_vOut22 _infoIpps) = inv_Decls_s23 _infoX23 (T_Decls_vIn22 _infoOisToplevel _infoOoptions)
         (T_Decls_vOut22 _dataDefIpps) = inv_Decls_s23 _dataDefX23 (T_Decls_vIn22 _dataDefOisToplevel _dataDefOoptions)
         (T_Decls_vOut22 _cataFunIpps) = inv_Decls_s23 _cataFunX23 (T_Decls_vIn22 _cataFunOisToplevel _cataFunOoptions)
         (T_Decls_vOut22 _semDomIpps) = inv_Decls_s23 _semDomX23 (T_Decls_vIn22 _semDomOisToplevel _semDomOoptions)
         (T_Decls_vOut22 _semWrapperIpps) = inv_Decls_s23 _semWrapperX23 (T_Decls_vIn22 _semWrapperOisToplevel _semWrapperOoptions)
         (T_Decls_vOut22 _semFunctionsIpps) = inv_Decls_s23 _semFunctionsX23 (T_Decls_vIn22 _semFunctionsOisToplevel _semFunctionsOoptions)
         _lhsOpps :: PP_Docs
         _lhsOpps = rule7 _cataFunIpps _commentIpp _dataDefIpps _infoIpps _lhsItextBlockMap _semDomIpps _semFunctionsIpps _semWrapperIpps arg_name_
         _commentOisToplevel = rule8 _lhsIisToplevel
         _commentOoptions = rule9 _lhsIoptions
         _infoOisToplevel = rule10 _lhsIisToplevel
         _infoOoptions = rule11 _lhsIoptions
         _dataDefOisToplevel = rule12 _lhsIisToplevel
         _dataDefOoptions = rule13 _lhsIoptions
         _cataFunOisToplevel = rule14 _lhsIisToplevel
         _cataFunOoptions = rule15 _lhsIoptions
         _semDomOisToplevel = rule16 _lhsIisToplevel
         _semDomOoptions = rule17 _lhsIoptions
         _semWrapperOisToplevel = rule18 _lhsIisToplevel
         _semWrapperOoptions = rule19 _lhsIoptions
         _semFunctionsOisToplevel = rule20 _lhsIisToplevel
         _semFunctionsOoptions = rule21 _lhsIoptions
         !__result_ = T_Chunk_vOut7 _lhsOpps
         in __result_ )
     in C_Chunk_s8 v7
   {-# INLINE rule7 #-}
   {-# LINE 97 "./src-ag/PrintOcamlCode.ag" #-}
   rule7 = \ ((_cataFunIpps) :: PP_Docs) ((_commentIpp) :: PP_Doc) ((_dataDefIpps) :: PP_Docs) ((_infoIpps) :: PP_Docs) ((_lhsItextBlockMap) :: Map BlockInfo PP_Doc) ((_semDomIpps) :: PP_Docs) ((_semFunctionsIpps) :: PP_Docs) ((_semWrapperIpps) :: PP_Docs) name_ ->
                                {-# LINE 97 "./src-ag/PrintOcamlCode.ag" #-}
                                _commentIpp
                                :  _infoIpps
                                ++ _dataDefIpps
                                ++ _semDomIpps
                                ++ _semFunctionsIpps
                                ++ _semWrapperIpps
                                ++ _cataFunIpps
                                ++ [Map.findWithDefault empty (BlockOther, Just $ identifier name_) _lhsItextBlockMap]
                                {-# LINE 301 "dist/build/PrintOcamlCode.hs"#-}
   {-# INLINE rule8 #-}
   rule8 = \ ((_lhsIisToplevel) :: Bool) ->
     _lhsIisToplevel
   {-# INLINE rule9 #-}
   rule9 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule10 #-}
   rule10 = \ ((_lhsIisToplevel) :: Bool) ->
     _lhsIisToplevel
   {-# INLINE rule11 #-}
   rule11 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule12 #-}
   rule12 = \ ((_lhsIisToplevel) :: Bool) ->
     _lhsIisToplevel
   {-# INLINE rule13 #-}
   rule13 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule14 #-}
   rule14 = \ ((_lhsIisToplevel) :: Bool) ->
     _lhsIisToplevel
   {-# INLINE rule15 #-}
   rule15 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule16 #-}
   rule16 = \ ((_lhsIisToplevel) :: Bool) ->
     _lhsIisToplevel
   {-# INLINE rule17 #-}
   rule17 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule18 #-}
   rule18 = \ ((_lhsIisToplevel) :: Bool) ->
     _lhsIisToplevel
   {-# INLINE rule19 #-}
   rule19 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule20 #-}
   rule20 = \ ((_lhsIisToplevel) :: Bool) ->
     _lhsIisToplevel
   {-# INLINE rule21 #-}
   rule21 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions

-- Chunks ------------------------------------------------------
-- wrapper
data Inh_Chunks  = Inh_Chunks { isToplevel_Inh_Chunks :: !(Bool), options_Inh_Chunks :: !(Options), textBlockMap_Inh_Chunks :: !(Map BlockInfo PP_Doc) }
data Syn_Chunks  = Syn_Chunks { pps_Syn_Chunks :: !(PP_Docs) }
{-# INLINABLE wrap_Chunks #-}
wrap_Chunks :: T_Chunks  -> Inh_Chunks  -> (Syn_Chunks )
wrap_Chunks !(T_Chunks act) !(Inh_Chunks _lhsIisToplevel _lhsIoptions _lhsItextBlockMap) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_Chunks_vIn10 _lhsIisToplevel _lhsIoptions _lhsItextBlockMap
        !(T_Chunks_vOut10 _lhsOpps) <- return (inv_Chunks_s11 sem arg)
        return (Syn_Chunks _lhsOpps)
   )

-- cata
{-# NOINLINE sem_Chunks #-}
sem_Chunks :: Chunks  -> T_Chunks 
sem_Chunks list = Prelude.foldr sem_Chunks_Cons sem_Chunks_Nil (Prelude.map sem_Chunk list)

-- semantic domain
newtype T_Chunks  = T_Chunks {
                             attach_T_Chunks :: Identity (T_Chunks_s11 )
                             }
newtype T_Chunks_s11  = C_Chunks_s11 {
                                     inv_Chunks_s11 :: (T_Chunks_v10 )
                                     }
data T_Chunks_s12  = C_Chunks_s12
type T_Chunks_v10  = (T_Chunks_vIn10 ) -> (T_Chunks_vOut10 )
data T_Chunks_vIn10  = T_Chunks_vIn10 (Bool) (Options) (Map BlockInfo PP_Doc)
data T_Chunks_vOut10  = T_Chunks_vOut10 (PP_Docs)
{-# NOINLINE sem_Chunks_Cons #-}
sem_Chunks_Cons :: T_Chunk  -> T_Chunks  -> T_Chunks 
sem_Chunks_Cons arg_hd_ arg_tl_ = T_Chunks (return st11) where
   {-# NOINLINE st11 #-}
   !st11 = let
      v10 :: T_Chunks_v10 
      v10 = \ !(T_Chunks_vIn10 _lhsIisToplevel _lhsIoptions _lhsItextBlockMap) -> ( let
         _hdX8 = Control.Monad.Identity.runIdentity (attach_T_Chunk (arg_hd_))
         _tlX11 = Control.Monad.Identity.runIdentity (attach_T_Chunks (arg_tl_))
         (T_Chunk_vOut7 _hdIpps) = inv_Chunk_s8 _hdX8 (T_Chunk_vIn7 _hdOisToplevel _hdOoptions _hdOtextBlockMap)
         (T_Chunks_vOut10 _tlIpps) = inv_Chunks_s11 _tlX11 (T_Chunks_vIn10 _tlOisToplevel _tlOoptions _tlOtextBlockMap)
         _lhsOpps :: PP_Docs
         _lhsOpps = rule22 _hdIpps _tlIpps
         _hdOisToplevel = rule23 _lhsIisToplevel
         _hdOoptions = rule24 _lhsIoptions
         _hdOtextBlockMap = rule25 _lhsItextBlockMap
         _tlOisToplevel = rule26 _lhsIisToplevel
         _tlOoptions = rule27 _lhsIoptions
         _tlOtextBlockMap = rule28 _lhsItextBlockMap
         !__result_ = T_Chunks_vOut10 _lhsOpps
         in __result_ )
     in C_Chunks_s11 v10
   {-# INLINE rule22 #-}
   {-# LINE 85 "./src-ag/PrintOcamlCode.ag" #-}
   rule22 = \ ((_hdIpps) :: PP_Docs) ((_tlIpps) :: PP_Docs) ->
                     {-# LINE 85 "./src-ag/PrintOcamlCode.ag" #-}
                     _hdIpps ++ _tlIpps
                     {-# LINE 402 "dist/build/PrintOcamlCode.hs"#-}
   {-# INLINE rule23 #-}
   rule23 = \ ((_lhsIisToplevel) :: Bool) ->
     _lhsIisToplevel
   {-# INLINE rule24 #-}
   rule24 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule25 #-}
   rule25 = \ ((_lhsItextBlockMap) :: Map BlockInfo PP_Doc) ->
     _lhsItextBlockMap
   {-# INLINE rule26 #-}
   rule26 = \ ((_lhsIisToplevel) :: Bool) ->
     _lhsIisToplevel
   {-# INLINE rule27 #-}
   rule27 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule28 #-}
   rule28 = \ ((_lhsItextBlockMap) :: Map BlockInfo PP_Doc) ->
     _lhsItextBlockMap
{-# NOINLINE sem_Chunks_Nil #-}
sem_Chunks_Nil ::  T_Chunks 
sem_Chunks_Nil  = T_Chunks (return st11) where
   {-# NOINLINE st11 #-}
   !st11 = let
      v10 :: T_Chunks_v10 
      v10 = \ !(T_Chunks_vIn10 _lhsIisToplevel _lhsIoptions _lhsItextBlockMap) -> ( let
         _lhsOpps :: PP_Docs
         _lhsOpps = rule29  ()
         !__result_ = T_Chunks_vOut10 _lhsOpps
         in __result_ )
     in C_Chunks_s11 v10
   {-# INLINE rule29 #-}
   {-# LINE 86 "./src-ag/PrintOcamlCode.ag" #-}
   rule29 = \  (_ :: ()) ->
                     {-# LINE 86 "./src-ag/PrintOcamlCode.ag" #-}
                     []
                     {-# LINE 438 "dist/build/PrintOcamlCode.hs"#-}

-- DataAlt -----------------------------------------------------
-- wrapper
data Inh_DataAlt  = Inh_DataAlt {  }
data Syn_DataAlt  = Syn_DataAlt { pp_Syn_DataAlt :: !(PP_Doc) }
{-# INLINABLE wrap_DataAlt #-}
wrap_DataAlt :: T_DataAlt  -> Inh_DataAlt  -> (Syn_DataAlt )
wrap_DataAlt !(T_DataAlt act) !(Inh_DataAlt ) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_DataAlt_vIn13 
        !(T_DataAlt_vOut13 _lhsOpp) <- return (inv_DataAlt_s14 sem arg)
        return (Syn_DataAlt _lhsOpp)
   )

-- cata
{-# NOINLINE sem_DataAlt #-}
sem_DataAlt :: DataAlt  -> T_DataAlt 
sem_DataAlt ( DataAlt !name_ args_ ) = sem_DataAlt_DataAlt name_ ( sem_Types args_ )
sem_DataAlt ( Record !name_ args_ ) = sem_DataAlt_Record name_ ( sem_NamedTypes args_ )

-- semantic domain
newtype T_DataAlt  = T_DataAlt {
                               attach_T_DataAlt :: Identity (T_DataAlt_s14 )
                               }
newtype T_DataAlt_s14  = C_DataAlt_s14 {
                                       inv_DataAlt_s14 :: (T_DataAlt_v13 )
                                       }
data T_DataAlt_s15  = C_DataAlt_s15
type T_DataAlt_v13  = (T_DataAlt_vIn13 ) -> (T_DataAlt_vOut13 )
data T_DataAlt_vIn13  = T_DataAlt_vIn13 
data T_DataAlt_vOut13  = T_DataAlt_vOut13 (PP_Doc)
{-# NOINLINE sem_DataAlt_DataAlt #-}
sem_DataAlt_DataAlt :: (String) -> T_Types  -> T_DataAlt 
sem_DataAlt_DataAlt !arg_name_ arg_args_ = T_DataAlt (return st14) where
   {-# NOINLINE st14 #-}
   !st14 = let
      v13 :: T_DataAlt_v13 
      v13 = \ !(T_DataAlt_vIn13 ) -> ( let
         _argsX53 = Control.Monad.Identity.runIdentity (attach_T_Types (arg_args_))
         (T_Types_vOut52 _argsIpps) = inv_Types_s53 _argsX53 (T_Types_vIn52 )
         _lhsOpp :: PP_Doc
         _lhsOpp = rule30 _argsIpps arg_name_
         !__result_ = T_DataAlt_vOut13 _lhsOpp
         in __result_ )
     in C_DataAlt_s14 v13
   {-# INLINE rule30 #-}
   {-# LINE 185 "./src-ag/PrintOcamlCode.ag" #-}
   rule30 = \ ((_argsIpps) :: PP_Docs) name_ ->
                               {-# LINE 185 "./src-ag/PrintOcamlCode.ag" #-}
                               name_ >#< "of" >#< pp_block "" "" " * " (map pp_parens _argsIpps)
                               {-# LINE 490 "dist/build/PrintOcamlCode.hs"#-}
{-# NOINLINE sem_DataAlt_Record #-}
sem_DataAlt_Record :: (String) -> T_NamedTypes  -> T_DataAlt 
sem_DataAlt_Record _ arg_args_ = T_DataAlt (return st14) where
   {-# NOINLINE st14 #-}
   !st14 = let
      v13 :: T_DataAlt_v13 
      v13 = \ !(T_DataAlt_vIn13 ) -> ( let
         _argsX38 = Control.Monad.Identity.runIdentity (attach_T_NamedTypes (arg_args_))
         (T_NamedTypes_vOut37 _argsIpps) = inv_NamedTypes_s38 _argsX38 (T_NamedTypes_vIn37 )
         _lhsOpp :: PP_Doc
         _lhsOpp = rule31 _argsIpps
         !__result_ = T_DataAlt_vOut13 _lhsOpp
         in __result_ )
     in C_DataAlt_s14 v13
   {-# INLINE rule31 #-}
   {-# LINE 186 "./src-ag/PrintOcamlCode.ag" #-}
   rule31 = \ ((_argsIpps) :: PP_Docs) ->
                               {-# LINE 186 "./src-ag/PrintOcamlCode.ag" #-}
                               pp_block "{" "}" ";" _argsIpps
                               {-# LINE 510 "dist/build/PrintOcamlCode.hs"#-}

-- DataAlts ----------------------------------------------------
-- wrapper
data Inh_DataAlts  = Inh_DataAlts {  }
data Syn_DataAlts  = Syn_DataAlts { pps_Syn_DataAlts :: !(PP_Docs) }
{-# INLINABLE wrap_DataAlts #-}
wrap_DataAlts :: T_DataAlts  -> Inh_DataAlts  -> (Syn_DataAlts )
wrap_DataAlts !(T_DataAlts act) !(Inh_DataAlts ) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_DataAlts_vIn16 
        !(T_DataAlts_vOut16 _lhsOpps) <- return (inv_DataAlts_s17 sem arg)
        return (Syn_DataAlts _lhsOpps)
   )

-- cata
{-# NOINLINE sem_DataAlts #-}
sem_DataAlts :: DataAlts  -> T_DataAlts 
sem_DataAlts list = Prelude.foldr sem_DataAlts_Cons sem_DataAlts_Nil (Prelude.map sem_DataAlt list)

-- semantic domain
newtype T_DataAlts  = T_DataAlts {
                                 attach_T_DataAlts :: Identity (T_DataAlts_s17 )
                                 }
newtype T_DataAlts_s17  = C_DataAlts_s17 {
                                         inv_DataAlts_s17 :: (T_DataAlts_v16 )
                                         }
data T_DataAlts_s18  = C_DataAlts_s18
type T_DataAlts_v16  = (T_DataAlts_vIn16 ) -> (T_DataAlts_vOut16 )
data T_DataAlts_vIn16  = T_DataAlts_vIn16 
data T_DataAlts_vOut16  = T_DataAlts_vOut16 (PP_Docs)
{-# NOINLINE sem_DataAlts_Cons #-}
sem_DataAlts_Cons :: T_DataAlt  -> T_DataAlts  -> T_DataAlts 
sem_DataAlts_Cons arg_hd_ arg_tl_ = T_DataAlts (return st17) where
   {-# NOINLINE st17 #-}
   !st17 = let
      v16 :: T_DataAlts_v16 
      v16 = \ !(T_DataAlts_vIn16 ) -> ( let
         _hdX14 = Control.Monad.Identity.runIdentity (attach_T_DataAlt (arg_hd_))
         _tlX17 = Control.Monad.Identity.runIdentity (attach_T_DataAlts (arg_tl_))
         (T_DataAlt_vOut13 _hdIpp) = inv_DataAlt_s14 _hdX14 (T_DataAlt_vIn13 )
         (T_DataAlts_vOut16 _tlIpps) = inv_DataAlts_s17 _tlX17 (T_DataAlts_vIn16 )
         _lhsOpps :: PP_Docs
         _lhsOpps = rule32 _hdIpp _tlIpps
         !__result_ = T_DataAlts_vOut16 _lhsOpps
         in __result_ )
     in C_DataAlts_s17 v16
   {-# INLINE rule32 #-}
   {-# LINE 69 "./src-ag/PrintOcamlCode.ag" #-}
   rule32 = \ ((_hdIpp) :: PP_Doc) ((_tlIpps) :: PP_Docs) ->
                     {-# LINE 69 "./src-ag/PrintOcamlCode.ag" #-}
                     _hdIpp : _tlIpps
                     {-# LINE 563 "dist/build/PrintOcamlCode.hs"#-}
{-# NOINLINE sem_DataAlts_Nil #-}
sem_DataAlts_Nil ::  T_DataAlts 
sem_DataAlts_Nil  = T_DataAlts (return st17) where
   {-# NOINLINE st17 #-}
   !st17 = let
      v16 :: T_DataAlts_v16 
      v16 = \ !(T_DataAlts_vIn16 ) -> ( let
         _lhsOpps :: PP_Docs
         _lhsOpps = rule33  ()
         !__result_ = T_DataAlts_vOut16 _lhsOpps
         in __result_ )
     in C_DataAlts_s17 v16
   {-# INLINE rule33 #-}
   {-# LINE 70 "./src-ag/PrintOcamlCode.ag" #-}
   rule33 = \  (_ :: ()) ->
                     {-# LINE 70 "./src-ag/PrintOcamlCode.ag" #-}
                     []
                     {-# LINE 581 "dist/build/PrintOcamlCode.hs"#-}

-- Decl --------------------------------------------------------
-- wrapper
data Inh_Decl  = Inh_Decl { isToplevel_Inh_Decl :: !(Bool), options_Inh_Decl :: !(Options) }
data Syn_Decl  = Syn_Decl { pp_Syn_Decl :: !(PP_Doc) }
{-# INLINABLE wrap_Decl #-}
wrap_Decl :: T_Decl  -> Inh_Decl  -> (Syn_Decl )
wrap_Decl !(T_Decl act) !(Inh_Decl _lhsIisToplevel _lhsIoptions) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_Decl_vIn19 _lhsIisToplevel _lhsIoptions
        !(T_Decl_vOut19 _lhsOpp) <- return (inv_Decl_s20 sem arg)
        return (Syn_Decl _lhsOpp)
   )

-- cata
{-# NOINLINE sem_Decl #-}
sem_Decl :: Decl  -> T_Decl 
sem_Decl ( Decl left_ rhs_ !binds_ !uses_ ) = sem_Decl_Decl ( sem_Lhs left_ ) ( sem_Expr rhs_ ) binds_ uses_
sem_Decl ( Bind left_ rhs_ ) = sem_Decl_Bind ( sem_Lhs left_ ) ( sem_Expr rhs_ )
sem_Decl ( BindLet left_ rhs_ ) = sem_Decl_BindLet ( sem_Lhs left_ ) ( sem_Expr rhs_ )
sem_Decl ( Data !name_ !params_ alts_ !strict_ !derivings_ ) = sem_Decl_Data name_ params_ ( sem_DataAlts alts_ ) strict_ derivings_
sem_Decl ( NewType !name_ !params_ !con_ tp_ ) = sem_Decl_NewType name_ params_ con_ ( sem_Type tp_ )
sem_Decl ( Type !name_ !params_ tp_ ) = sem_Decl_Type name_ params_ ( sem_Type tp_ )
sem_Decl ( TSig !name_ tp_ ) = sem_Decl_TSig name_ ( sem_Type tp_ )
sem_Decl ( Comment !txt_ ) = sem_Decl_Comment txt_
sem_Decl ( PragmaDecl !txt_ ) = sem_Decl_PragmaDecl txt_
sem_Decl ( Resume !monadic_ !nt_ left_ rhs_ ) = sem_Decl_Resume monadic_ nt_ ( sem_Lhs left_ ) ( sem_Expr rhs_ )
sem_Decl ( EvalDecl !nt_ left_ rhs_ ) = sem_Decl_EvalDecl nt_ ( sem_Lhs left_ ) ( sem_Expr rhs_ )

-- semantic domain
newtype T_Decl  = T_Decl {
                         attach_T_Decl :: Identity (T_Decl_s20 )
                         }
newtype T_Decl_s20  = C_Decl_s20 {
                                 inv_Decl_s20 :: (T_Decl_v19 )
                                 }
data T_Decl_s21  = C_Decl_s21
type T_Decl_v19  = (T_Decl_vIn19 ) -> (T_Decl_vOut19 )
data T_Decl_vIn19  = T_Decl_vIn19 (Bool) (Options)
data T_Decl_vOut19  = T_Decl_vOut19 (PP_Doc)
{-# NOINLINE sem_Decl_Decl #-}
sem_Decl_Decl :: T_Lhs  -> T_Expr  -> (Set String) -> (Set String) -> T_Decl 
sem_Decl_Decl arg_left_ arg_rhs_ _ _ = T_Decl (return st20) where
   {-# NOINLINE st20 #-}
   !st20 = let
      v19 :: T_Decl_v19 
      v19 = \ !(T_Decl_vIn19 _lhsIisToplevel _lhsIoptions) -> ( let
         _leftX32 = Control.Monad.Identity.runIdentity (attach_T_Lhs (arg_left_))
         _rhsX26 = Control.Monad.Identity.runIdentity (attach_T_Expr (arg_rhs_))
         (T_Lhs_vOut31 _leftIpp) = inv_Lhs_s32 _leftX32 (T_Lhs_vIn31 _leftOoptions)
         (T_Expr_vOut25 _rhsIpp) = inv_Expr_s26 _rhsX26 (T_Expr_vIn25 _rhsOoptions)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule34 _leftIpp _lhsIisToplevel _rhsIpp
         _leftOoptions = rule35 _lhsIoptions
         _rhsOoptions = rule36 _lhsIoptions
         !__result_ = T_Decl_vOut19 _lhsOpp
         in __result_ )
     in C_Decl_s20 v19
   {-# INLINE rule34 #-}
   {-# LINE 107 "./src-ag/PrintOcamlCode.ag" #-}
   rule34 = \ ((_leftIpp) :: PP_Doc) ((_lhsIisToplevel) :: Bool) ((_rhsIpp) :: PP_Doc) ->
                               {-# LINE 107 "./src-ag/PrintOcamlCode.ag" #-}
                               if _lhsIisToplevel
                               then "let" >#< _leftIpp >#< "="
                                    >-< indent 4 _rhsIpp >#< ";;"
                               else "let" >#< _leftIpp >#< "="
                                    >-< indent 4 _rhsIpp >#< "in"
                               {-# LINE 650 "dist/build/PrintOcamlCode.hs"#-}
   {-# INLINE rule35 #-}
   rule35 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule36 #-}
   rule36 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Decl_Bind #-}
sem_Decl_Bind :: T_Lhs  -> T_Expr  -> T_Decl 
sem_Decl_Bind arg_left_ arg_rhs_ = T_Decl (return st20) where
   {-# NOINLINE st20 #-}
   !st20 = let
      v19 :: T_Decl_v19 
      v19 = \ !(T_Decl_vIn19 _lhsIisToplevel _lhsIoptions) -> ( let
         _leftX32 = Control.Monad.Identity.runIdentity (attach_T_Lhs (arg_left_))
         _rhsX26 = Control.Monad.Identity.runIdentity (attach_T_Expr (arg_rhs_))
         (T_Lhs_vOut31 _leftIpp) = inv_Lhs_s32 _leftX32 (T_Lhs_vIn31 _leftOoptions)
         (T_Expr_vOut25 _rhsIpp) = inv_Expr_s26 _rhsX26 (T_Expr_vIn25 _rhsOoptions)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule37  ()
         _leftOoptions = rule38 _lhsIoptions
         _rhsOoptions = rule39 _lhsIoptions
         !__result_ = T_Decl_vOut19 _lhsOpp
         in __result_ )
     in C_Decl_s20 v19
   {-# INLINE rule37 #-}
   {-# LINE 112 "./src-ag/PrintOcamlCode.ag" #-}
   rule37 = \  (_ :: ()) ->
                               {-# LINE 112 "./src-ag/PrintOcamlCode.ag" #-}
                               error "pp of Decl.Bind not supported"
                               {-# LINE 680 "dist/build/PrintOcamlCode.hs"#-}
   {-# INLINE rule38 #-}
   rule38 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule39 #-}
   rule39 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Decl_BindLet #-}
sem_Decl_BindLet :: T_Lhs  -> T_Expr  -> T_Decl 
sem_Decl_BindLet arg_left_ arg_rhs_ = T_Decl (return st20) where
   {-# NOINLINE st20 #-}
   !st20 = let
      v19 :: T_Decl_v19 
      v19 = \ !(T_Decl_vIn19 _lhsIisToplevel _lhsIoptions) -> ( let
         _leftX32 = Control.Monad.Identity.runIdentity (attach_T_Lhs (arg_left_))
         _rhsX26 = Control.Monad.Identity.runIdentity (attach_T_Expr (arg_rhs_))
         (T_Lhs_vOut31 _leftIpp) = inv_Lhs_s32 _leftX32 (T_Lhs_vIn31 _leftOoptions)
         (T_Expr_vOut25 _rhsIpp) = inv_Expr_s26 _rhsX26 (T_Expr_vIn25 _rhsOoptions)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule40  ()
         _leftOoptions = rule41 _lhsIoptions
         _rhsOoptions = rule42 _lhsIoptions
         !__result_ = T_Decl_vOut19 _lhsOpp
         in __result_ )
     in C_Decl_s20 v19
   {-# INLINE rule40 #-}
   {-# LINE 113 "./src-ag/PrintOcamlCode.ag" #-}
   rule40 = \  (_ :: ()) ->
                               {-# LINE 113 "./src-ag/PrintOcamlCode.ag" #-}
                               error "pp of Decl.BindLet not supported"
                               {-# LINE 710 "dist/build/PrintOcamlCode.hs"#-}
   {-# INLINE rule41 #-}
   rule41 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule42 #-}
   rule42 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Decl_Data #-}
sem_Decl_Data :: (String) -> ([String]) -> T_DataAlts  -> (Bool) -> ([String]) -> T_Decl 
sem_Decl_Data !arg_name_ !arg_params_ arg_alts_ _ _ = T_Decl (return st20) where
   {-# NOINLINE st20 #-}
   !st20 = let
      v19 :: T_Decl_v19 
      v19 = \ !(T_Decl_vIn19 _lhsIisToplevel _lhsIoptions) -> ( let
         _altsX17 = Control.Monad.Identity.runIdentity (attach_T_DataAlts (arg_alts_))
         (T_DataAlts_vOut16 _altsIpps) = inv_DataAlts_s17 _altsX17 (T_DataAlts_vIn16 )
         _lhsOpp :: PP_Doc
         _lhsOpp = rule43 _altsIpps arg_name_ arg_params_
         !__result_ = T_Decl_vOut19 _lhsOpp
         in __result_ )
     in C_Decl_s20 v19
   {-# INLINE rule43 #-}
   {-# LINE 114 "./src-ag/PrintOcamlCode.ag" #-}
   rule43 = \ ((_altsIpps) :: PP_Docs) name_ params_ ->
                               {-# LINE 114 "./src-ag/PrintOcamlCode.ag" #-}
                               "type" >#< hv_sp (map (\p -> "'" >|< p) params_ ++ [text $ toOcamlTC name_])
                               >#<  ( case _altsIpps of
                                            [] -> empty
                                            (x:xs) ->              "=" >#<  x
                                                   >-< vlist (map ("|" >#<) xs)
                                    )
                               >#< ";;"
                               {-# LINE 742 "dist/build/PrintOcamlCode.hs"#-}
{-# NOINLINE sem_Decl_NewType #-}
sem_Decl_NewType :: (String) -> ([String]) -> (String) -> T_Type  -> T_Decl 
sem_Decl_NewType _ _ _ arg_tp_ = T_Decl (return st20) where
   {-# NOINLINE st20 #-}
   !st20 = let
      v19 :: T_Decl_v19 
      v19 = \ !(T_Decl_vIn19 _lhsIisToplevel _lhsIoptions) -> ( let
         _tpX50 = Control.Monad.Identity.runIdentity (attach_T_Type (arg_tp_))
         (T_Type_vOut49 _tpIpp) = inv_Type_s50 _tpX50 (T_Type_vIn49 )
         _lhsOpp :: PP_Doc
         _lhsOpp = rule44  ()
         !__result_ = T_Decl_vOut19 _lhsOpp
         in __result_ )
     in C_Decl_s20 v19
   {-# INLINE rule44 #-}
   {-# LINE 121 "./src-ag/PrintOcamlCode.ag" #-}
   rule44 = \  (_ :: ()) ->
                               {-# LINE 121 "./src-ag/PrintOcamlCode.ag" #-}
                               error "pp of Decl.NewType not supported"
                               {-# LINE 762 "dist/build/PrintOcamlCode.hs"#-}
{-# NOINLINE sem_Decl_Type #-}
sem_Decl_Type :: (String) -> ([String]) -> T_Type  -> T_Decl 
sem_Decl_Type !arg_name_ !arg_params_ arg_tp_ = T_Decl (return st20) where
   {-# NOINLINE st20 #-}
   !st20 = let
      v19 :: T_Decl_v19 
      v19 = \ !(T_Decl_vIn19 _lhsIisToplevel _lhsIoptions) -> ( let
         _tpX50 = Control.Monad.Identity.runIdentity (attach_T_Type (arg_tp_))
         (T_Type_vOut49 _tpIpp) = inv_Type_s50 _tpX50 (T_Type_vIn49 )
         _lhsOpp :: PP_Doc
         _lhsOpp = rule45 _tpIpp arg_name_ arg_params_
         !__result_ = T_Decl_vOut19 _lhsOpp
         in __result_ )
     in C_Decl_s20 v19
   {-# INLINE rule45 #-}
   {-# LINE 122 "./src-ag/PrintOcamlCode.ag" #-}
   rule45 = \ ((_tpIpp) :: PP_Doc) name_ params_ ->
                               {-# LINE 122 "./src-ag/PrintOcamlCode.ag" #-}
                               "type" >#< hv_sp (map (\p -> "'" >|< p) params_ ++ [text $ toOcamlTC name_]) >#< "=" >#<  _tpIpp >#< ";;"
                               {-# LINE 782 "dist/build/PrintOcamlCode.hs"#-}
{-# NOINLINE sem_Decl_TSig #-}
sem_Decl_TSig :: (String) -> T_Type  -> T_Decl 
sem_Decl_TSig !arg_name_ arg_tp_ = T_Decl (return st20) where
   {-# NOINLINE st20 #-}
   !st20 = let
      v19 :: T_Decl_v19 
      v19 = \ !(T_Decl_vIn19 _lhsIisToplevel _lhsIoptions) -> ( let
         _tpX50 = Control.Monad.Identity.runIdentity (attach_T_Type (arg_tp_))
         (T_Type_vOut49 _tpIpp) = inv_Type_s50 _tpX50 (T_Type_vIn49 )
         _lhsOpp :: PP_Doc
         _lhsOpp = rule46 _tpIpp arg_name_
         !__result_ = T_Decl_vOut19 _lhsOpp
         in __result_ )
     in C_Decl_s20 v19
   {-# INLINE rule46 #-}
   {-# LINE 123 "./src-ag/PrintOcamlCode.ag" #-}
   rule46 = \ ((_tpIpp) :: PP_Doc) name_ ->
                               {-# LINE 123 "./src-ag/PrintOcamlCode.ag" #-}
                               "(*" >#< name_ >#< ":" >#< _tpIpp >#< "*)"
                               {-# LINE 802 "dist/build/PrintOcamlCode.hs"#-}
{-# NOINLINE sem_Decl_Comment #-}
sem_Decl_Comment :: (String) -> T_Decl 
sem_Decl_Comment !arg_txt_ = T_Decl (return st20) where
   {-# NOINLINE st20 #-}
   !st20 = let
      v19 :: T_Decl_v19 
      v19 = \ !(T_Decl_vIn19 _lhsIisToplevel _lhsIoptions) -> ( let
         _lhsOpp :: PP_Doc
         _lhsOpp = rule47 arg_txt_
         !__result_ = T_Decl_vOut19 _lhsOpp
         in __result_ )
     in C_Decl_s20 v19
   {-# INLINE rule47 #-}
   {-# LINE 124 "./src-ag/PrintOcamlCode.ag" #-}
   rule47 = \ txt_ ->
                               {-# LINE 124 "./src-ag/PrintOcamlCode.ag" #-}
                               if '\n' `elem` txt_
                                 then "(* " >-< vlist (lines txt_) >-< "*)"
                                 else "(*" >#< txt_ >#< "*)"
                               {-# LINE 822 "dist/build/PrintOcamlCode.hs"#-}
{-# NOINLINE sem_Decl_PragmaDecl #-}
sem_Decl_PragmaDecl :: (String) -> T_Decl 
sem_Decl_PragmaDecl _ = T_Decl (return st20) where
   {-# NOINLINE st20 #-}
   !st20 = let
      v19 :: T_Decl_v19 
      v19 = \ !(T_Decl_vIn19 _lhsIisToplevel _lhsIoptions) -> ( let
         _lhsOpp :: PP_Doc
         _lhsOpp = rule48  ()
         !__result_ = T_Decl_vOut19 _lhsOpp
         in __result_ )
     in C_Decl_s20 v19
   {-# INLINE rule48 #-}
   {-# LINE 127 "./src-ag/PrintOcamlCode.ag" #-}
   rule48 = \  (_ :: ()) ->
                               {-# LINE 127 "./src-ag/PrintOcamlCode.ag" #-}
                               error "pp of Decl.PragmaDecl not supported"
                               {-# LINE 840 "dist/build/PrintOcamlCode.hs"#-}
{-# NOINLINE sem_Decl_Resume #-}
sem_Decl_Resume :: (Bool) -> (String) -> T_Lhs  -> T_Expr  -> T_Decl 
sem_Decl_Resume _ _ arg_left_ arg_rhs_ = T_Decl (return st20) where
   {-# NOINLINE st20 #-}
   !st20 = let
      v19 :: T_Decl_v19 
      v19 = \ !(T_Decl_vIn19 _lhsIisToplevel _lhsIoptions) -> ( let
         _leftX32 = Control.Monad.Identity.runIdentity (attach_T_Lhs (arg_left_))
         _rhsX26 = Control.Monad.Identity.runIdentity (attach_T_Expr (arg_rhs_))
         (T_Lhs_vOut31 _leftIpp) = inv_Lhs_s32 _leftX32 (T_Lhs_vIn31 _leftOoptions)
         (T_Expr_vOut25 _rhsIpp) = inv_Expr_s26 _rhsX26 (T_Expr_vIn25 _rhsOoptions)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule49 _rhsIpp
         _leftOoptions = rule50 _lhsIoptions
         _rhsOoptions = rule51 _lhsIoptions
         !__result_ = T_Decl_vOut19 _lhsOpp
         in __result_ )
     in C_Decl_s20 v19
   {-# INLINE rule49 #-}
   rule49 = \ ((_rhsIpp) :: PP_Doc) ->
     _rhsIpp
   {-# INLINE rule50 #-}
   rule50 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule51 #-}
   rule51 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Decl_EvalDecl #-}
sem_Decl_EvalDecl :: (String) -> T_Lhs  -> T_Expr  -> T_Decl 
sem_Decl_EvalDecl _ arg_left_ arg_rhs_ = T_Decl (return st20) where
   {-# NOINLINE st20 #-}
   !st20 = let
      v19 :: T_Decl_v19 
      v19 = \ !(T_Decl_vIn19 _lhsIisToplevel _lhsIoptions) -> ( let
         _leftX32 = Control.Monad.Identity.runIdentity (attach_T_Lhs (arg_left_))
         _rhsX26 = Control.Monad.Identity.runIdentity (attach_T_Expr (arg_rhs_))
         (T_Lhs_vOut31 _leftIpp) = inv_Lhs_s32 _leftX32 (T_Lhs_vIn31 _leftOoptions)
         (T_Expr_vOut25 _rhsIpp) = inv_Expr_s26 _rhsX26 (T_Expr_vIn25 _rhsOoptions)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule52 _rhsIpp
         _leftOoptions = rule53 _lhsIoptions
         _rhsOoptions = rule54 _lhsIoptions
         !__result_ = T_Decl_vOut19 _lhsOpp
         in __result_ )
     in C_Decl_s20 v19
   {-# INLINE rule52 #-}
   rule52 = \ ((_rhsIpp) :: PP_Doc) ->
     _rhsIpp
   {-# INLINE rule53 #-}
   rule53 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule54 #-}
   rule54 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions

-- Decls -------------------------------------------------------
-- wrapper
data Inh_Decls  = Inh_Decls { isToplevel_Inh_Decls :: !(Bool), options_Inh_Decls :: !(Options) }
data Syn_Decls  = Syn_Decls { pps_Syn_Decls :: !(PP_Docs) }
{-# INLINABLE wrap_Decls #-}
wrap_Decls :: T_Decls  -> Inh_Decls  -> (Syn_Decls )
wrap_Decls !(T_Decls act) !(Inh_Decls _lhsIisToplevel _lhsIoptions) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_Decls_vIn22 _lhsIisToplevel _lhsIoptions
        !(T_Decls_vOut22 _lhsOpps) <- return (inv_Decls_s23 sem arg)
        return (Syn_Decls _lhsOpps)
   )

-- cata
{-# NOINLINE sem_Decls #-}
sem_Decls :: Decls  -> T_Decls 
sem_Decls list = Prelude.foldr sem_Decls_Cons sem_Decls_Nil (Prelude.map sem_Decl list)

-- semantic domain
newtype T_Decls  = T_Decls {
                           attach_T_Decls :: Identity (T_Decls_s23 )
                           }
newtype T_Decls_s23  = C_Decls_s23 {
                                   inv_Decls_s23 :: (T_Decls_v22 )
                                   }
data T_Decls_s24  = C_Decls_s24
type T_Decls_v22  = (T_Decls_vIn22 ) -> (T_Decls_vOut22 )
data T_Decls_vIn22  = T_Decls_vIn22 (Bool) (Options)
data T_Decls_vOut22  = T_Decls_vOut22 (PP_Docs)
{-# NOINLINE sem_Decls_Cons #-}
sem_Decls_Cons :: T_Decl  -> T_Decls  -> T_Decls 
sem_Decls_Cons arg_hd_ arg_tl_ = T_Decls (return st23) where
   {-# NOINLINE st23 #-}
   !st23 = let
      v22 :: T_Decls_v22 
      v22 = \ !(T_Decls_vIn22 _lhsIisToplevel _lhsIoptions) -> ( let
         _hdX20 = Control.Monad.Identity.runIdentity (attach_T_Decl (arg_hd_))
         _tlX23 = Control.Monad.Identity.runIdentity (attach_T_Decls (arg_tl_))
         (T_Decl_vOut19 _hdIpp) = inv_Decl_s20 _hdX20 (T_Decl_vIn19 _hdOisToplevel _hdOoptions)
         (T_Decls_vOut22 _tlIpps) = inv_Decls_s23 _tlX23 (T_Decls_vIn22 _tlOisToplevel _tlOoptions)
         _lhsOpps :: PP_Docs
         _lhsOpps = rule55 _hdIpp _tlIpps
         _hdOisToplevel = rule56 _lhsIisToplevel
         _hdOoptions = rule57 _lhsIoptions
         _tlOisToplevel = rule58 _lhsIisToplevel
         _tlOoptions = rule59 _lhsIoptions
         !__result_ = T_Decls_vOut22 _lhsOpps
         in __result_ )
     in C_Decls_s23 v22
   {-# INLINE rule55 #-}
   {-# LINE 81 "./src-ag/PrintOcamlCode.ag" #-}
   rule55 = \ ((_hdIpp) :: PP_Doc) ((_tlIpps) :: PP_Docs) ->
                     {-# LINE 81 "./src-ag/PrintOcamlCode.ag" #-}
                     _hdIpp : _tlIpps
                     {-# LINE 951 "dist/build/PrintOcamlCode.hs"#-}
   {-# INLINE rule56 #-}
   rule56 = \ ((_lhsIisToplevel) :: Bool) ->
     _lhsIisToplevel
   {-# INLINE rule57 #-}
   rule57 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule58 #-}
   rule58 = \ ((_lhsIisToplevel) :: Bool) ->
     _lhsIisToplevel
   {-# INLINE rule59 #-}
   rule59 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Decls_Nil #-}
sem_Decls_Nil ::  T_Decls 
sem_Decls_Nil  = T_Decls (return st23) where
   {-# NOINLINE st23 #-}
   !st23 = let
      v22 :: T_Decls_v22 
      v22 = \ !(T_Decls_vIn22 _lhsIisToplevel _lhsIoptions) -> ( let
         _lhsOpps :: PP_Docs
         _lhsOpps = rule60  ()
         !__result_ = T_Decls_vOut22 _lhsOpps
         in __result_ )
     in C_Decls_s23 v22
   {-# INLINE rule60 #-}
   {-# LINE 82 "./src-ag/PrintOcamlCode.ag" #-}
   rule60 = \  (_ :: ()) ->
                     {-# LINE 82 "./src-ag/PrintOcamlCode.ag" #-}
                     []
                     {-# LINE 981 "dist/build/PrintOcamlCode.hs"#-}

-- Expr --------------------------------------------------------
-- wrapper
data Inh_Expr  = Inh_Expr { options_Inh_Expr :: !(Options) }
data Syn_Expr  = Syn_Expr { pp_Syn_Expr :: !(PP_Doc) }
{-# INLINABLE wrap_Expr #-}
wrap_Expr :: T_Expr  -> Inh_Expr  -> (Syn_Expr )
wrap_Expr !(T_Expr act) !(Inh_Expr _lhsIoptions) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_Expr_vIn25 _lhsIoptions
        !(T_Expr_vOut25 _lhsOpp) <- return (inv_Expr_s26 sem arg)
        return (Syn_Expr _lhsOpp)
   )

-- cata
{-# NOINLINE sem_Expr #-}
sem_Expr :: Expr  -> T_Expr 
sem_Expr ( Let decls_ body_ ) = sem_Expr_Let ( sem_Decls decls_ ) ( sem_Expr body_ )
sem_Expr ( Case expr_ alts_ ) = sem_Expr_Case ( sem_Expr expr_ ) ( sem_CaseAlts alts_ )
sem_Expr ( Do stmts_ body_ ) = sem_Expr_Do ( sem_Decls stmts_ ) ( sem_Expr body_ )
sem_Expr ( Lambda args_ body_ ) = sem_Expr_Lambda ( sem_Exprs args_ ) ( sem_Expr body_ )
sem_Expr ( TupleExpr exprs_ ) = sem_Expr_TupleExpr ( sem_Exprs exprs_ )
sem_Expr ( UnboxedTupleExpr exprs_ ) = sem_Expr_UnboxedTupleExpr ( sem_Exprs exprs_ )
sem_Expr ( App !name_ args_ ) = sem_Expr_App name_ ( sem_Exprs args_ )
sem_Expr ( SimpleExpr !txt_ ) = sem_Expr_SimpleExpr txt_
sem_Expr ( TextExpr !lns_ ) = sem_Expr_TextExpr lns_
sem_Expr ( Trace !txt_ expr_ ) = sem_Expr_Trace txt_ ( sem_Expr expr_ )
sem_Expr ( PragmaExpr !onLeftSide_ !onNewLine_ !txt_ expr_ ) = sem_Expr_PragmaExpr onLeftSide_ onNewLine_ txt_ ( sem_Expr expr_ )
sem_Expr ( LineExpr expr_ ) = sem_Expr_LineExpr ( sem_Expr expr_ )
sem_Expr ( TypedExpr expr_ tp_ ) = sem_Expr_TypedExpr ( sem_Expr expr_ ) ( sem_Type tp_ )
sem_Expr ( ResultExpr !nt_ expr_ ) = sem_Expr_ResultExpr nt_ ( sem_Expr expr_ )
sem_Expr ( InvokeExpr !nt_ expr_ args_ ) = sem_Expr_InvokeExpr nt_ ( sem_Expr expr_ ) ( sem_Exprs args_ )
sem_Expr ( ResumeExpr !nt_ expr_ left_ rhs_ ) = sem_Expr_ResumeExpr nt_ ( sem_Expr expr_ ) ( sem_Lhs left_ ) ( sem_Expr rhs_ )
sem_Expr ( SemFun !nt_ args_ body_ ) = sem_Expr_SemFun nt_ ( sem_Exprs args_ ) ( sem_Expr body_ )

-- semantic domain
newtype T_Expr  = T_Expr {
                         attach_T_Expr :: Identity (T_Expr_s26 )
                         }
newtype T_Expr_s26  = C_Expr_s26 {
                                 inv_Expr_s26 :: (T_Expr_v25 )
                                 }
data T_Expr_s27  = C_Expr_s27
type T_Expr_v25  = (T_Expr_vIn25 ) -> (T_Expr_vOut25 )
data T_Expr_vIn25  = T_Expr_vIn25 (Options)
data T_Expr_vOut25  = T_Expr_vOut25 (PP_Doc)
{-# NOINLINE sem_Expr_Let #-}
sem_Expr_Let :: T_Decls  -> T_Expr  -> T_Expr 
sem_Expr_Let arg_decls_ arg_body_ = T_Expr (return st26) where
   {-# NOINLINE st26 #-}
   !st26 = let
      v25 :: T_Expr_v25 
      v25 = \ !(T_Expr_vIn25 _lhsIoptions) -> ( let
         _declsX23 = Control.Monad.Identity.runIdentity (attach_T_Decls (arg_decls_))
         _bodyX26 = Control.Monad.Identity.runIdentity (attach_T_Expr (arg_body_))
         (T_Decls_vOut22 _declsIpps) = inv_Decls_s23 _declsX23 (T_Decls_vIn22 _declsOisToplevel _declsOoptions)
         (T_Expr_vOut25 _bodyIpp) = inv_Expr_s26 _bodyX26 (T_Expr_vIn25 _bodyOoptions)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule61 _bodyIpp _declsIpps
         _declsOisToplevel = rule62  ()
         _declsOoptions = rule63 _lhsIoptions
         _bodyOoptions = rule64 _lhsIoptions
         !__result_ = T_Expr_vOut25 _lhsOpp
         in __result_ )
     in C_Expr_s26 v25
   {-# INLINE rule61 #-}
   {-# LINE 131 "./src-ag/PrintOcamlCode.ag" #-}
   rule61 = \ ((_bodyIpp) :: PP_Doc) ((_declsIpps) :: PP_Docs) ->
                                 {-# LINE 131 "./src-ag/PrintOcamlCode.ag" #-}
                                 pp_parens $ vlist (_declsIpps ++ [_bodyIpp])
                                 {-# LINE 1053 "dist/build/PrintOcamlCode.hs"#-}
   {-# INLINE rule62 #-}
   {-# LINE 218 "./src-ag/PrintOcamlCode.ag" #-}
   rule62 = \  (_ :: ()) ->
                           {-# LINE 218 "./src-ag/PrintOcamlCode.ag" #-}
                           False
                           {-# LINE 1059 "dist/build/PrintOcamlCode.hs"#-}
   {-# INLINE rule63 #-}
   rule63 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule64 #-}
   rule64 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Expr_Case #-}
sem_Expr_Case :: T_Expr  -> T_CaseAlts  -> T_Expr 
sem_Expr_Case arg_expr_ arg_alts_ = T_Expr (return st26) where
   {-# NOINLINE st26 #-}
   !st26 = let
      v25 :: T_Expr_v25 
      v25 = \ !(T_Expr_vIn25 _lhsIoptions) -> ( let
         _exprX26 = Control.Monad.Identity.runIdentity (attach_T_Expr (arg_expr_))
         _altsX5 = Control.Monad.Identity.runIdentity (attach_T_CaseAlts (arg_alts_))
         (T_Expr_vOut25 _exprIpp) = inv_Expr_s26 _exprX26 (T_Expr_vIn25 _exprOoptions)
         (T_CaseAlts_vOut4 _altsIpps) = inv_CaseAlts_s5 _altsX5 (T_CaseAlts_vIn4 _altsOoptions)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule65 _altsIpps _exprIpp
         _exprOoptions = rule66 _lhsIoptions
         _altsOoptions = rule67 _lhsIoptions
         !__result_ = T_Expr_vOut25 _lhsOpp
         in __result_ )
     in C_Expr_s26 v25
   {-# INLINE rule65 #-}
   {-# LINE 132 "./src-ag/PrintOcamlCode.ag" #-}
   rule65 = \ ((_altsIpps) :: PP_Docs) ((_exprIpp) :: PP_Doc) ->
                                 {-# LINE 132 "./src-ag/PrintOcamlCode.ag" #-}
                                 pp_parens ( "match" >#< _exprIpp >#< "with"
                                           >-< indent 2 ( case _altsIpps of
                                                            [] -> empty
                                                            (x:xs) -> " " >#<  x
                                                                      >-< vlist (map ("|" >#<) xs)
                                                        )
                                           )
                                 {-# LINE 1095 "dist/build/PrintOcamlCode.hs"#-}
   {-# INLINE rule66 #-}
   rule66 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule67 #-}
   rule67 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Expr_Do #-}
sem_Expr_Do :: T_Decls  -> T_Expr  -> T_Expr 
sem_Expr_Do arg_stmts_ arg_body_ = T_Expr (return st26) where
   {-# NOINLINE st26 #-}
   !st26 = let
      v25 :: T_Expr_v25 
      v25 = \ !(T_Expr_vIn25 _lhsIoptions) -> ( let
         _stmtsX23 = Control.Monad.Identity.runIdentity (attach_T_Decls (arg_stmts_))
         _bodyX26 = Control.Monad.Identity.runIdentity (attach_T_Expr (arg_body_))
         (T_Decls_vOut22 _stmtsIpps) = inv_Decls_s23 _stmtsX23 (T_Decls_vIn22 _stmtsOisToplevel _stmtsOoptions)
         (T_Expr_vOut25 _bodyIpp) = inv_Expr_s26 _bodyX26 (T_Expr_vIn25 _bodyOoptions)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule68  ()
         _stmtsOisToplevel = rule69  ()
         _stmtsOoptions = rule70 _lhsIoptions
         _bodyOoptions = rule71 _lhsIoptions
         !__result_ = T_Expr_vOut25 _lhsOpp
         in __result_ )
     in C_Expr_s26 v25
   {-# INLINE rule68 #-}
   {-# LINE 139 "./src-ag/PrintOcamlCode.ag" #-}
   rule68 = \  (_ :: ()) ->
                                 {-# LINE 139 "./src-ag/PrintOcamlCode.ag" #-}
                                 error "pp of Expr.Do not supported"
                                 {-# LINE 1126 "dist/build/PrintOcamlCode.hs"#-}
   {-# INLINE rule69 #-}
   {-# LINE 220 "./src-ag/PrintOcamlCode.ag" #-}
   rule69 = \  (_ :: ()) ->
                           {-# LINE 220 "./src-ag/PrintOcamlCode.ag" #-}
                           False
                           {-# LINE 1132 "dist/build/PrintOcamlCode.hs"#-}
   {-# INLINE rule70 #-}
   rule70 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule71 #-}
   rule71 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Expr_Lambda #-}
sem_Expr_Lambda :: T_Exprs  -> T_Expr  -> T_Expr 
sem_Expr_Lambda arg_args_ arg_body_ = T_Expr (return st26) where
   {-# NOINLINE st26 #-}
   !st26 = let
      v25 :: T_Expr_v25 
      v25 = \ !(T_Expr_vIn25 _lhsIoptions) -> ( let
         _argsX29 = Control.Monad.Identity.runIdentity (attach_T_Exprs (arg_args_))
         _bodyX26 = Control.Monad.Identity.runIdentity (attach_T_Expr (arg_body_))
         (T_Exprs_vOut28 _argsIpps) = inv_Exprs_s29 _argsX29 (T_Exprs_vIn28 _argsOoptions)
         (T_Expr_vOut25 _bodyIpp) = inv_Expr_s26 _bodyX26 (T_Expr_vIn25 _bodyOoptions)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule72 _argsIpps _bodyIpp
         _argsOoptions = rule73 _lhsIoptions
         _bodyOoptions = rule74 _lhsIoptions
         !__result_ = T_Expr_vOut25 _lhsOpp
         in __result_ )
     in C_Expr_s26 v25
   {-# INLINE rule72 #-}
   {-# LINE 140 "./src-ag/PrintOcamlCode.ag" #-}
   rule72 = \ ((_argsIpps) :: PP_Docs) ((_bodyIpp) :: PP_Doc) ->
                                 {-# LINE 140 "./src-ag/PrintOcamlCode.ag" #-}
                                 pp_parens ( pp "fun" >#< hv_sp _argsIpps >#< "->"
                                           >-< indent 2 _bodyIpp )
                                 {-# LINE 1163 "dist/build/PrintOcamlCode.hs"#-}
   {-# INLINE rule73 #-}
   rule73 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule74 #-}
   rule74 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Expr_TupleExpr #-}
sem_Expr_TupleExpr :: T_Exprs  -> T_Expr 
sem_Expr_TupleExpr arg_exprs_ = T_Expr (return st26) where
   {-# NOINLINE st26 #-}
   !st26 = let
      v25 :: T_Expr_v25 
      v25 = \ !(T_Expr_vIn25 _lhsIoptions) -> ( let
         _exprsX29 = Control.Monad.Identity.runIdentity (attach_T_Exprs (arg_exprs_))
         (T_Exprs_vOut28 _exprsIpps) = inv_Exprs_s29 _exprsX29 (T_Exprs_vIn28 _exprsOoptions)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule75 _exprsIpps
         _exprsOoptions = rule76 _lhsIoptions
         !__result_ = T_Expr_vOut25 _lhsOpp
         in __result_ )
     in C_Expr_s26 v25
   {-# INLINE rule75 #-}
   {-# LINE 142 "./src-ag/PrintOcamlCode.ag" #-}
   rule75 = \ ((_exprsIpps) :: PP_Docs) ->
                                 {-# LINE 142 "./src-ag/PrintOcamlCode.ag" #-}
                                 ppTuple False _exprsIpps
                                 {-# LINE 1190 "dist/build/PrintOcamlCode.hs"#-}
   {-# INLINE rule76 #-}
   rule76 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Expr_UnboxedTupleExpr #-}
sem_Expr_UnboxedTupleExpr :: T_Exprs  -> T_Expr 
sem_Expr_UnboxedTupleExpr arg_exprs_ = T_Expr (return st26) where
   {-# NOINLINE st26 #-}
   !st26 = let
      v25 :: T_Expr_v25 
      v25 = \ !(T_Expr_vIn25 _lhsIoptions) -> ( let
         _exprsX29 = Control.Monad.Identity.runIdentity (attach_T_Exprs (arg_exprs_))
         (T_Exprs_vOut28 _exprsIpps) = inv_Exprs_s29 _exprsX29 (T_Exprs_vIn28 _exprsOoptions)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule77  ()
         _exprsOoptions = rule78 _lhsIoptions
         !__result_ = T_Expr_vOut25 _lhsOpp
         in __result_ )
     in C_Expr_s26 v25
   {-# INLINE rule77 #-}
   {-# LINE 143 "./src-ag/PrintOcamlCode.ag" #-}
   rule77 = \  (_ :: ()) ->
                                   {-# LINE 143 "./src-ag/PrintOcamlCode.ag" #-}
                                   error "pp of Expr.UnboxedTupleExpr not supported"
                                   {-# LINE 1214 "dist/build/PrintOcamlCode.hs"#-}
   {-# INLINE rule78 #-}
   rule78 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Expr_App #-}
sem_Expr_App :: (String) -> T_Exprs  -> T_Expr 
sem_Expr_App !arg_name_ arg_args_ = T_Expr (return st26) where
   {-# NOINLINE st26 #-}
   !st26 = let
      v25 :: T_Expr_v25 
      v25 = \ !(T_Expr_vIn25 _lhsIoptions) -> ( let
         _argsX29 = Control.Monad.Identity.runIdentity (attach_T_Exprs (arg_args_))
         (T_Exprs_vOut28 _argsIpps) = inv_Exprs_s29 _argsX29 (T_Exprs_vIn28 _argsOoptions)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule79 _argsIpps arg_name_
         _argsOoptions = rule80 _lhsIoptions
         !__result_ = T_Expr_vOut25 _lhsOpp
         in __result_ )
     in C_Expr_s26 v25
   {-# INLINE rule79 #-}
   {-# LINE 144 "./src-ag/PrintOcamlCode.ag" #-}
   rule79 = \ ((_argsIpps) :: PP_Docs) name_ ->
                                 {-# LINE 144 "./src-ag/PrintOcamlCode.ag" #-}
                                 pp_parens $ name_ >#< hv_sp _argsIpps
                                 {-# LINE 1238 "dist/build/PrintOcamlCode.hs"#-}
   {-# INLINE rule80 #-}
   rule80 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Expr_SimpleExpr #-}
sem_Expr_SimpleExpr :: (String) -> T_Expr 
sem_Expr_SimpleExpr !arg_txt_ = T_Expr (return st26) where
   {-# NOINLINE st26 #-}
   !st26 = let
      v25 :: T_Expr_v25 
      v25 = \ !(T_Expr_vIn25 _lhsIoptions) -> ( let
         _lhsOpp :: PP_Doc
         _lhsOpp = rule81 arg_txt_
         !__result_ = T_Expr_vOut25 _lhsOpp
         in __result_ )
     in C_Expr_s26 v25
   {-# INLINE rule81 #-}
   {-# LINE 145 "./src-ag/PrintOcamlCode.ag" #-}
   rule81 = \ txt_ ->
                                 {-# LINE 145 "./src-ag/PrintOcamlCode.ag" #-}
                                 text txt_
                                 {-# LINE 1259 "dist/build/PrintOcamlCode.hs"#-}
{-# NOINLINE sem_Expr_TextExpr #-}
sem_Expr_TextExpr :: ([String]) -> T_Expr 
sem_Expr_TextExpr !arg_lns_ = T_Expr (return st26) where
   {-# NOINLINE st26 #-}
   !st26 = let
      v25 :: T_Expr_v25 
      v25 = \ !(T_Expr_vIn25 _lhsIoptions) -> ( let
         _lhsOpp :: PP_Doc
         _lhsOpp = rule82 arg_lns_
         !__result_ = T_Expr_vOut25 _lhsOpp
         in __result_ )
     in C_Expr_s26 v25
   {-# INLINE rule82 #-}
   {-# LINE 146 "./src-ag/PrintOcamlCode.ag" #-}
   rule82 = \ lns_ ->
                                 {-# LINE 146 "./src-ag/PrintOcamlCode.ag" #-}
                                 vlist (map text lns_)
                                 {-# LINE 1277 "dist/build/PrintOcamlCode.hs"#-}
{-# NOINLINE sem_Expr_Trace #-}
sem_Expr_Trace :: (String) -> T_Expr  -> T_Expr 
sem_Expr_Trace _ arg_expr_ = T_Expr (return st26) where
   {-# NOINLINE st26 #-}
   !st26 = let
      v25 :: T_Expr_v25 
      v25 = \ !(T_Expr_vIn25 _lhsIoptions) -> ( let
         _exprX26 = Control.Monad.Identity.runIdentity (attach_T_Expr (arg_expr_))
         (T_Expr_vOut25 _exprIpp) = inv_Expr_s26 _exprX26 (T_Expr_vIn25 _exprOoptions)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule83 _exprIpp
         _exprOoptions = rule84 _lhsIoptions
         !__result_ = T_Expr_vOut25 _lhsOpp
         in __result_ )
     in C_Expr_s26 v25
   {-# INLINE rule83 #-}
   {-# LINE 147 "./src-ag/PrintOcamlCode.ag" #-}
   rule83 = \ ((_exprIpp) :: PP_Doc) ->
                                 {-# LINE 147 "./src-ag/PrintOcamlCode.ag" #-}
                                 _exprIpp
                                 {-# LINE 1298 "dist/build/PrintOcamlCode.hs"#-}
   {-# INLINE rule84 #-}
   rule84 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Expr_PragmaExpr #-}
sem_Expr_PragmaExpr :: (Bool) -> (Bool) -> (String) -> T_Expr  -> T_Expr 
sem_Expr_PragmaExpr _ _ _ arg_expr_ = T_Expr (return st26) where
   {-# NOINLINE st26 #-}
   !st26 = let
      v25 :: T_Expr_v25 
      v25 = \ !(T_Expr_vIn25 _lhsIoptions) -> ( let
         _exprX26 = Control.Monad.Identity.runIdentity (attach_T_Expr (arg_expr_))
         (T_Expr_vOut25 _exprIpp) = inv_Expr_s26 _exprX26 (T_Expr_vIn25 _exprOoptions)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule85 _exprIpp
         _exprOoptions = rule86 _lhsIoptions
         !__result_ = T_Expr_vOut25 _lhsOpp
         in __result_ )
     in C_Expr_s26 v25
   {-# INLINE rule85 #-}
   {-# LINE 148 "./src-ag/PrintOcamlCode.ag" #-}
   rule85 = \ ((_exprIpp) :: PP_Doc) ->
                                 {-# LINE 148 "./src-ag/PrintOcamlCode.ag" #-}
                                 _exprIpp
                                 {-# LINE 1322 "dist/build/PrintOcamlCode.hs"#-}
   {-# INLINE rule86 #-}
   rule86 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Expr_LineExpr #-}
sem_Expr_LineExpr :: T_Expr  -> T_Expr 
sem_Expr_LineExpr arg_expr_ = T_Expr (return st26) where
   {-# NOINLINE st26 #-}
   !st26 = let
      v25 :: T_Expr_v25 
      v25 = \ !(T_Expr_vIn25 _lhsIoptions) -> ( let
         _exprX26 = Control.Monad.Identity.runIdentity (attach_T_Expr (arg_expr_))
         (T_Expr_vOut25 _exprIpp) = inv_Expr_s26 _exprX26 (T_Expr_vIn25 _exprOoptions)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule87 _exprIpp
         _exprOoptions = rule88 _lhsIoptions
         !__result_ = T_Expr_vOut25 _lhsOpp
         in __result_ )
     in C_Expr_s26 v25
   {-# INLINE rule87 #-}
   {-# LINE 149 "./src-ag/PrintOcamlCode.ag" #-}
   rule87 = \ ((_exprIpp) :: PP_Doc) ->
                                 {-# LINE 149 "./src-ag/PrintOcamlCode.ag" #-}
                                 _exprIpp
                                 {-# LINE 1346 "dist/build/PrintOcamlCode.hs"#-}
   {-# INLINE rule88 #-}
   rule88 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Expr_TypedExpr #-}
sem_Expr_TypedExpr :: T_Expr  -> T_Type  -> T_Expr 
sem_Expr_TypedExpr arg_expr_ arg_tp_ = T_Expr (return st26) where
   {-# NOINLINE st26 #-}
   !st26 = let
      v25 :: T_Expr_v25 
      v25 = \ !(T_Expr_vIn25 _lhsIoptions) -> ( let
         _exprX26 = Control.Monad.Identity.runIdentity (attach_T_Expr (arg_expr_))
         _tpX50 = Control.Monad.Identity.runIdentity (attach_T_Type (arg_tp_))
         (T_Expr_vOut25 _exprIpp) = inv_Expr_s26 _exprX26 (T_Expr_vIn25 _exprOoptions)
         (T_Type_vOut49 _tpIpp) = inv_Type_s50 _tpX50 (T_Type_vIn49 )
         _lhsOpp :: PP_Doc
         _lhsOpp = rule89 _exprIpp
         _exprOoptions = rule90 _lhsIoptions
         !__result_ = T_Expr_vOut25 _lhsOpp
         in __result_ )
     in C_Expr_s26 v25
   {-# INLINE rule89 #-}
   {-# LINE 150 "./src-ag/PrintOcamlCode.ag" #-}
   rule89 = \ ((_exprIpp) :: PP_Doc) ->
                                 {-# LINE 150 "./src-ag/PrintOcamlCode.ag" #-}
                                 _exprIpp
                                 {-# LINE 1372 "dist/build/PrintOcamlCode.hs"#-}
   {-# INLINE rule90 #-}
   rule90 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Expr_ResultExpr #-}
sem_Expr_ResultExpr :: (String) -> T_Expr  -> T_Expr 
sem_Expr_ResultExpr _ arg_expr_ = T_Expr (return st26) where
   {-# NOINLINE st26 #-}
   !st26 = let
      v25 :: T_Expr_v25 
      v25 = \ !(T_Expr_vIn25 _lhsIoptions) -> ( let
         _exprX26 = Control.Monad.Identity.runIdentity (attach_T_Expr (arg_expr_))
         (T_Expr_vOut25 _exprIpp) = inv_Expr_s26 _exprX26 (T_Expr_vIn25 _exprOoptions)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule91 _exprIpp
         _exprOoptions = rule92 _lhsIoptions
         !__result_ = T_Expr_vOut25 _lhsOpp
         in __result_ )
     in C_Expr_s26 v25
   {-# INLINE rule91 #-}
   rule91 = \ ((_exprIpp) :: PP_Doc) ->
     _exprIpp
   {-# INLINE rule92 #-}
   rule92 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Expr_InvokeExpr #-}
sem_Expr_InvokeExpr :: (String) -> T_Expr  -> T_Exprs  -> T_Expr 
sem_Expr_InvokeExpr _ arg_expr_ arg_args_ = T_Expr (return st26) where
   {-# NOINLINE st26 #-}
   !st26 = let
      v25 :: T_Expr_v25 
      v25 = \ !(T_Expr_vIn25 _lhsIoptions) -> ( let
         _exprX26 = Control.Monad.Identity.runIdentity (attach_T_Expr (arg_expr_))
         _argsX29 = Control.Monad.Identity.runIdentity (attach_T_Exprs (arg_args_))
         (T_Expr_vOut25 _exprIpp) = inv_Expr_s26 _exprX26 (T_Expr_vIn25 _exprOoptions)
         (T_Exprs_vOut28 _argsIpps) = inv_Exprs_s29 _argsX29 (T_Exprs_vIn28 _argsOoptions)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule93 _exprIpp
         _exprOoptions = rule94 _lhsIoptions
         _argsOoptions = rule95 _lhsIoptions
         !__result_ = T_Expr_vOut25 _lhsOpp
         in __result_ )
     in C_Expr_s26 v25
   {-# INLINE rule93 #-}
   rule93 = \ ((_exprIpp) :: PP_Doc) ->
     _exprIpp
   {-# INLINE rule94 #-}
   rule94 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule95 #-}
   rule95 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Expr_ResumeExpr #-}
sem_Expr_ResumeExpr :: (String) -> T_Expr  -> T_Lhs  -> T_Expr  -> T_Expr 
sem_Expr_ResumeExpr _ arg_expr_ arg_left_ arg_rhs_ = T_Expr (return st26) where
   {-# NOINLINE st26 #-}
   !st26 = let
      v25 :: T_Expr_v25 
      v25 = \ !(T_Expr_vIn25 _lhsIoptions) -> ( let
         _exprX26 = Control.Monad.Identity.runIdentity (attach_T_Expr (arg_expr_))
         _leftX32 = Control.Monad.Identity.runIdentity (attach_T_Lhs (arg_left_))
         _rhsX26 = Control.Monad.Identity.runIdentity (attach_T_Expr (arg_rhs_))
         (T_Expr_vOut25 _exprIpp) = inv_Expr_s26 _exprX26 (T_Expr_vIn25 _exprOoptions)
         (T_Lhs_vOut31 _leftIpp) = inv_Lhs_s32 _leftX32 (T_Lhs_vIn31 _leftOoptions)
         (T_Expr_vOut25 _rhsIpp) = inv_Expr_s26 _rhsX26 (T_Expr_vIn25 _rhsOoptions)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule96 _rhsIpp
         _exprOoptions = rule97 _lhsIoptions
         _leftOoptions = rule98 _lhsIoptions
         _rhsOoptions = rule99 _lhsIoptions
         !__result_ = T_Expr_vOut25 _lhsOpp
         in __result_ )
     in C_Expr_s26 v25
   {-# INLINE rule96 #-}
   rule96 = \ ((_rhsIpp) :: PP_Doc) ->
     _rhsIpp
   {-# INLINE rule97 #-}
   rule97 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule98 #-}
   rule98 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule99 #-}
   rule99 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Expr_SemFun #-}
sem_Expr_SemFun :: (String) -> T_Exprs  -> T_Expr  -> T_Expr 
sem_Expr_SemFun _ arg_args_ arg_body_ = T_Expr (return st26) where
   {-# NOINLINE st26 #-}
   !st26 = let
      v25 :: T_Expr_v25 
      v25 = \ !(T_Expr_vIn25 _lhsIoptions) -> ( let
         _argsX29 = Control.Monad.Identity.runIdentity (attach_T_Exprs (arg_args_))
         _bodyX26 = Control.Monad.Identity.runIdentity (attach_T_Expr (arg_body_))
         (T_Exprs_vOut28 _argsIpps) = inv_Exprs_s29 _argsX29 (T_Exprs_vIn28 _argsOoptions)
         (T_Expr_vOut25 _bodyIpp) = inv_Expr_s26 _bodyX26 (T_Expr_vIn25 _bodyOoptions)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule100 _bodyIpp
         _argsOoptions = rule101 _lhsIoptions
         _bodyOoptions = rule102 _lhsIoptions
         !__result_ = T_Expr_vOut25 _lhsOpp
         in __result_ )
     in C_Expr_s26 v25
   {-# INLINE rule100 #-}
   rule100 = \ ((_bodyIpp) :: PP_Doc) ->
     _bodyIpp
   {-# INLINE rule101 #-}
   rule101 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule102 #-}
   rule102 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions

-- Exprs -------------------------------------------------------
-- wrapper
data Inh_Exprs  = Inh_Exprs { options_Inh_Exprs :: !(Options) }
data Syn_Exprs  = Syn_Exprs { pps_Syn_Exprs :: !(PP_Docs) }
{-# INLINABLE wrap_Exprs #-}
wrap_Exprs :: T_Exprs  -> Inh_Exprs  -> (Syn_Exprs )
wrap_Exprs !(T_Exprs act) !(Inh_Exprs _lhsIoptions) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_Exprs_vIn28 _lhsIoptions
        !(T_Exprs_vOut28 _lhsOpps) <- return (inv_Exprs_s29 sem arg)
        return (Syn_Exprs _lhsOpps)
   )

-- cata
{-# NOINLINE sem_Exprs #-}
sem_Exprs :: Exprs  -> T_Exprs 
sem_Exprs list = Prelude.foldr sem_Exprs_Cons sem_Exprs_Nil (Prelude.map sem_Expr list)

-- semantic domain
newtype T_Exprs  = T_Exprs {
                           attach_T_Exprs :: Identity (T_Exprs_s29 )
                           }
newtype T_Exprs_s29  = C_Exprs_s29 {
                                   inv_Exprs_s29 :: (T_Exprs_v28 )
                                   }
data T_Exprs_s30  = C_Exprs_s30
type T_Exprs_v28  = (T_Exprs_vIn28 ) -> (T_Exprs_vOut28 )
data T_Exprs_vIn28  = T_Exprs_vIn28 (Options)
data T_Exprs_vOut28  = T_Exprs_vOut28 (PP_Docs)
{-# NOINLINE sem_Exprs_Cons #-}
sem_Exprs_Cons :: T_Expr  -> T_Exprs  -> T_Exprs 
sem_Exprs_Cons arg_hd_ arg_tl_ = T_Exprs (return st29) where
   {-# NOINLINE st29 #-}
   !st29 = let
      v28 :: T_Exprs_v28 
      v28 = \ !(T_Exprs_vIn28 _lhsIoptions) -> ( let
         _hdX26 = Control.Monad.Identity.runIdentity (attach_T_Expr (arg_hd_))
         _tlX29 = Control.Monad.Identity.runIdentity (attach_T_Exprs (arg_tl_))
         (T_Expr_vOut25 _hdIpp) = inv_Expr_s26 _hdX26 (T_Expr_vIn25 _hdOoptions)
         (T_Exprs_vOut28 _tlIpps) = inv_Exprs_s29 _tlX29 (T_Exprs_vIn28 _tlOoptions)
         _lhsOpps :: PP_Docs
         _lhsOpps = rule103 _hdIpp _tlIpps
         _hdOoptions = rule104 _lhsIoptions
         _tlOoptions = rule105 _lhsIoptions
         !__result_ = T_Exprs_vOut28 _lhsOpps
         in __result_ )
     in C_Exprs_s29 v28
   {-# INLINE rule103 #-}
   {-# LINE 61 "./src-ag/PrintOcamlCode.ag" #-}
   rule103 = \ ((_hdIpp) :: PP_Doc) ((_tlIpps) :: PP_Docs) ->
                     {-# LINE 61 "./src-ag/PrintOcamlCode.ag" #-}
                     _hdIpp : _tlIpps
                     {-# LINE 1538 "dist/build/PrintOcamlCode.hs"#-}
   {-# INLINE rule104 #-}
   rule104 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule105 #-}
   rule105 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Exprs_Nil #-}
sem_Exprs_Nil ::  T_Exprs 
sem_Exprs_Nil  = T_Exprs (return st29) where
   {-# NOINLINE st29 #-}
   !st29 = let
      v28 :: T_Exprs_v28 
      v28 = \ !(T_Exprs_vIn28 _lhsIoptions) -> ( let
         _lhsOpps :: PP_Docs
         _lhsOpps = rule106  ()
         !__result_ = T_Exprs_vOut28 _lhsOpps
         in __result_ )
     in C_Exprs_s29 v28
   {-# INLINE rule106 #-}
   {-# LINE 62 "./src-ag/PrintOcamlCode.ag" #-}
   rule106 = \  (_ :: ()) ->
                     {-# LINE 62 "./src-ag/PrintOcamlCode.ag" #-}
                     []
                     {-# LINE 1562 "dist/build/PrintOcamlCode.hs"#-}

-- Lhs ---------------------------------------------------------
-- wrapper
data Inh_Lhs  = Inh_Lhs { options_Inh_Lhs :: !(Options) }
data Syn_Lhs  = Syn_Lhs { pp_Syn_Lhs :: !(PP_Doc) }
{-# INLINABLE wrap_Lhs #-}
wrap_Lhs :: T_Lhs  -> Inh_Lhs  -> (Syn_Lhs )
wrap_Lhs !(T_Lhs act) !(Inh_Lhs _lhsIoptions) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_Lhs_vIn31 _lhsIoptions
        !(T_Lhs_vOut31 _lhsOpp) <- return (inv_Lhs_s32 sem arg)
        return (Syn_Lhs _lhsOpp)
   )

-- cata
{-# NOINLINE sem_Lhs #-}
sem_Lhs :: Lhs  -> T_Lhs 
sem_Lhs ( Pattern3 pat3_ ) = sem_Lhs_Pattern3 ( sem_Pattern pat3_ )
sem_Lhs ( Pattern3SM pat3_ ) = sem_Lhs_Pattern3SM ( sem_Pattern pat3_ )
sem_Lhs ( TupleLhs !comps_ ) = sem_Lhs_TupleLhs comps_
sem_Lhs ( UnboxedTupleLhs !comps_ ) = sem_Lhs_UnboxedTupleLhs comps_
sem_Lhs ( Fun !name_ args_ ) = sem_Lhs_Fun name_ ( sem_Exprs args_ )
sem_Lhs ( Unwrap !name_ sub_ ) = sem_Lhs_Unwrap name_ ( sem_Lhs sub_ )

-- semantic domain
newtype T_Lhs  = T_Lhs {
                       attach_T_Lhs :: Identity (T_Lhs_s32 )
                       }
newtype T_Lhs_s32  = C_Lhs_s32 {
                               inv_Lhs_s32 :: (T_Lhs_v31 )
                               }
data T_Lhs_s33  = C_Lhs_s33
type T_Lhs_v31  = (T_Lhs_vIn31 ) -> (T_Lhs_vOut31 )
data T_Lhs_vIn31  = T_Lhs_vIn31 (Options)
data T_Lhs_vOut31  = T_Lhs_vOut31 (PP_Doc)
{-# NOINLINE sem_Lhs_Pattern3 #-}
sem_Lhs_Pattern3 :: T_Pattern  -> T_Lhs 
sem_Lhs_Pattern3 arg_pat3_ = T_Lhs (return st32) where
   {-# NOINLINE st32 #-}
   !st32 = let
      v31 :: T_Lhs_v31 
      v31 = \ !(T_Lhs_vIn31 _lhsIoptions) -> ( let
         _pat3X41 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pat3_))
         (T_Pattern_vOut40 _pat3Icopy _pat3IisUnderscore _pat3Ipp) = inv_Pattern_s41 _pat3X41 (T_Pattern_vIn40 _pat3Ooptions)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule107 _pat3Ipp
         _pat3Ooptions = rule108 _lhsIoptions
         !__result_ = T_Lhs_vOut31 _lhsOpp
         in __result_ )
     in C_Lhs_s32 v31
   {-# INLINE rule107 #-}
   {-# LINE 153 "./src-ag/PrintOcamlCode.ag" #-}
   rule107 = \ ((_pat3Ipp) :: PP_Doc) ->
                               {-# LINE 153 "./src-ag/PrintOcamlCode.ag" #-}
                               _pat3Ipp
                               {-# LINE 1619 "dist/build/PrintOcamlCode.hs"#-}
   {-# INLINE rule108 #-}
   rule108 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Lhs_Pattern3SM #-}
sem_Lhs_Pattern3SM :: T_Pattern  -> T_Lhs 
sem_Lhs_Pattern3SM arg_pat3_ = T_Lhs (return st32) where
   {-# NOINLINE st32 #-}
   !st32 = let
      v31 :: T_Lhs_v31 
      v31 = \ !(T_Lhs_vIn31 _lhsIoptions) -> ( let
         _pat3X41 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pat3_))
         (T_Pattern_vOut40 _pat3Icopy _pat3IisUnderscore _pat3Ipp) = inv_Pattern_s41 _pat3X41 (T_Pattern_vIn40 _pat3Ooptions)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule109  ()
         _pat3Ooptions = rule110 _lhsIoptions
         !__result_ = T_Lhs_vOut31 _lhsOpp
         in __result_ )
     in C_Lhs_s32 v31
   {-# INLINE rule109 #-}
   {-# LINE 154 "./src-ag/PrintOcamlCode.ag" #-}
   rule109 = \  (_ :: ()) ->
                               {-# LINE 154 "./src-ag/PrintOcamlCode.ag" #-}
                               error "pp of Lhs.Pattern3SM not supported"
                               {-# LINE 1643 "dist/build/PrintOcamlCode.hs"#-}
   {-# INLINE rule110 #-}
   rule110 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Lhs_TupleLhs #-}
sem_Lhs_TupleLhs :: ([String]) -> T_Lhs 
sem_Lhs_TupleLhs !arg_comps_ = T_Lhs (return st32) where
   {-# NOINLINE st32 #-}
   !st32 = let
      v31 :: T_Lhs_v31 
      v31 = \ !(T_Lhs_vIn31 _lhsIoptions) -> ( let
         _lhsOpp :: PP_Doc
         _lhsOpp = rule111 arg_comps_
         !__result_ = T_Lhs_vOut31 _lhsOpp
         in __result_ )
     in C_Lhs_s32 v31
   {-# INLINE rule111 #-}
   {-# LINE 155 "./src-ag/PrintOcamlCode.ag" #-}
   rule111 = \ comps_ ->
                               {-# LINE 155 "./src-ag/PrintOcamlCode.ag" #-}
                               ppTuple False (map text comps_)
                               {-# LINE 1664 "dist/build/PrintOcamlCode.hs"#-}
{-# NOINLINE sem_Lhs_UnboxedTupleLhs #-}
sem_Lhs_UnboxedTupleLhs :: ([String]) -> T_Lhs 
sem_Lhs_UnboxedTupleLhs _ = T_Lhs (return st32) where
   {-# NOINLINE st32 #-}
   !st32 = let
      v31 :: T_Lhs_v31 
      v31 = \ !(T_Lhs_vIn31 _lhsIoptions) -> ( let
         _lhsOpp :: PP_Doc
         _lhsOpp = rule112  ()
         !__result_ = T_Lhs_vOut31 _lhsOpp
         in __result_ )
     in C_Lhs_s32 v31
   {-# INLINE rule112 #-}
   {-# LINE 156 "./src-ag/PrintOcamlCode.ag" #-}
   rule112 = \  (_ :: ()) ->
                                      {-# LINE 156 "./src-ag/PrintOcamlCode.ag" #-}
                                      error "pp of Lhs.UnboxedTupleLhs not supported"
                                      {-# LINE 1682 "dist/build/PrintOcamlCode.hs"#-}
{-# NOINLINE sem_Lhs_Fun #-}
sem_Lhs_Fun :: (String) -> T_Exprs  -> T_Lhs 
sem_Lhs_Fun !arg_name_ arg_args_ = T_Lhs (return st32) where
   {-# NOINLINE st32 #-}
   !st32 = let
      v31 :: T_Lhs_v31 
      v31 = \ !(T_Lhs_vIn31 _lhsIoptions) -> ( let
         _argsX29 = Control.Monad.Identity.runIdentity (attach_T_Exprs (arg_args_))
         (T_Exprs_vOut28 _argsIpps) = inv_Exprs_s29 _argsX29 (T_Exprs_vIn28 _argsOoptions)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule113 _argsIpps arg_name_
         _argsOoptions = rule114 _lhsIoptions
         !__result_ = T_Lhs_vOut31 _lhsOpp
         in __result_ )
     in C_Lhs_s32 v31
   {-# INLINE rule113 #-}
   {-# LINE 157 "./src-ag/PrintOcamlCode.ag" #-}
   rule113 = \ ((_argsIpps) :: PP_Docs) name_ ->
                               {-# LINE 157 "./src-ag/PrintOcamlCode.ag" #-}
                               name_ >#< hv_sp _argsIpps
                               {-# LINE 1703 "dist/build/PrintOcamlCode.hs"#-}
   {-# INLINE rule114 #-}
   rule114 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Lhs_Unwrap #-}
sem_Lhs_Unwrap :: (String) -> T_Lhs  -> T_Lhs 
sem_Lhs_Unwrap !arg_name_ arg_sub_ = T_Lhs (return st32) where
   {-# NOINLINE st32 #-}
   !st32 = let
      v31 :: T_Lhs_v31 
      v31 = \ !(T_Lhs_vIn31 _lhsIoptions) -> ( let
         _subX32 = Control.Monad.Identity.runIdentity (attach_T_Lhs (arg_sub_))
         (T_Lhs_vOut31 _subIpp) = inv_Lhs_s32 _subX32 (T_Lhs_vIn31 _subOoptions)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule115 _subIpp arg_name_
         _subOoptions = rule116 _lhsIoptions
         !__result_ = T_Lhs_vOut31 _lhsOpp
         in __result_ )
     in C_Lhs_s32 v31
   {-# INLINE rule115 #-}
   {-# LINE 158 "./src-ag/PrintOcamlCode.ag" #-}
   rule115 = \ ((_subIpp) :: PP_Doc) name_ ->
                               {-# LINE 158 "./src-ag/PrintOcamlCode.ag" #-}
                               pp_parens (name_ >#< _subIpp)
                               {-# LINE 1727 "dist/build/PrintOcamlCode.hs"#-}
   {-# INLINE rule116 #-}
   rule116 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions

-- NamedType ---------------------------------------------------
-- wrapper
data Inh_NamedType  = Inh_NamedType {  }
data Syn_NamedType  = Syn_NamedType { pp_Syn_NamedType :: !(PP_Doc) }
{-# INLINABLE wrap_NamedType #-}
wrap_NamedType :: T_NamedType  -> Inh_NamedType  -> (Syn_NamedType )
wrap_NamedType !(T_NamedType act) !(Inh_NamedType ) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_NamedType_vIn34 
        !(T_NamedType_vOut34 _lhsOpp) <- return (inv_NamedType_s35 sem arg)
        return (Syn_NamedType _lhsOpp)
   )

-- cata
{-# INLINE sem_NamedType #-}
sem_NamedType :: NamedType  -> T_NamedType 
sem_NamedType ( Named !strict_ !name_ tp_ ) = sem_NamedType_Named strict_ name_ ( sem_Type tp_ )

-- semantic domain
newtype T_NamedType  = T_NamedType {
                                   attach_T_NamedType :: Identity (T_NamedType_s35 )
                                   }
newtype T_NamedType_s35  = C_NamedType_s35 {
                                           inv_NamedType_s35 :: (T_NamedType_v34 )
                                           }
data T_NamedType_s36  = C_NamedType_s36
type T_NamedType_v34  = (T_NamedType_vIn34 ) -> (T_NamedType_vOut34 )
data T_NamedType_vIn34  = T_NamedType_vIn34 
data T_NamedType_vOut34  = T_NamedType_vOut34 (PP_Doc)
{-# NOINLINE sem_NamedType_Named #-}
sem_NamedType_Named :: (Bool) -> (String) -> T_Type  -> T_NamedType 
sem_NamedType_Named _ !arg_name_ arg_tp_ = T_NamedType (return st35) where
   {-# NOINLINE st35 #-}
   !st35 = let
      v34 :: T_NamedType_v34 
      v34 = \ !(T_NamedType_vIn34 ) -> ( let
         _tpX50 = Control.Monad.Identity.runIdentity (attach_T_Type (arg_tp_))
         (T_Type_vOut49 _tpIpp) = inv_Type_s50 _tpX50 (T_Type_vIn49 )
         _lhsOpp :: PP_Doc
         _lhsOpp = rule117 _tpIpp arg_name_
         !__result_ = T_NamedType_vOut34 _lhsOpp
         in __result_ )
     in C_NamedType_s35 v34
   {-# INLINE rule117 #-}
   {-# LINE 189 "./src-ag/PrintOcamlCode.ag" #-}
   rule117 = \ ((_tpIpp) :: PP_Doc) name_ ->
                               {-# LINE 189 "./src-ag/PrintOcamlCode.ag" #-}
                               name_ >#< ":" >#< _tpIpp
                               {-# LINE 1781 "dist/build/PrintOcamlCode.hs"#-}

-- NamedTypes --------------------------------------------------
-- wrapper
data Inh_NamedTypes  = Inh_NamedTypes {  }
data Syn_NamedTypes  = Syn_NamedTypes { pps_Syn_NamedTypes :: !(PP_Docs) }
{-# INLINABLE wrap_NamedTypes #-}
wrap_NamedTypes :: T_NamedTypes  -> Inh_NamedTypes  -> (Syn_NamedTypes )
wrap_NamedTypes !(T_NamedTypes act) !(Inh_NamedTypes ) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_NamedTypes_vIn37 
        !(T_NamedTypes_vOut37 _lhsOpps) <- return (inv_NamedTypes_s38 sem arg)
        return (Syn_NamedTypes _lhsOpps)
   )

-- cata
{-# NOINLINE sem_NamedTypes #-}
sem_NamedTypes :: NamedTypes  -> T_NamedTypes 
sem_NamedTypes list = Prelude.foldr sem_NamedTypes_Cons sem_NamedTypes_Nil (Prelude.map sem_NamedType list)

-- semantic domain
newtype T_NamedTypes  = T_NamedTypes {
                                     attach_T_NamedTypes :: Identity (T_NamedTypes_s38 )
                                     }
newtype T_NamedTypes_s38  = C_NamedTypes_s38 {
                                             inv_NamedTypes_s38 :: (T_NamedTypes_v37 )
                                             }
data T_NamedTypes_s39  = C_NamedTypes_s39
type T_NamedTypes_v37  = (T_NamedTypes_vIn37 ) -> (T_NamedTypes_vOut37 )
data T_NamedTypes_vIn37  = T_NamedTypes_vIn37 
data T_NamedTypes_vOut37  = T_NamedTypes_vOut37 (PP_Docs)
{-# NOINLINE sem_NamedTypes_Cons #-}
sem_NamedTypes_Cons :: T_NamedType  -> T_NamedTypes  -> T_NamedTypes 
sem_NamedTypes_Cons arg_hd_ arg_tl_ = T_NamedTypes (return st38) where
   {-# NOINLINE st38 #-}
   !st38 = let
      v37 :: T_NamedTypes_v37 
      v37 = \ !(T_NamedTypes_vIn37 ) -> ( let
         _hdX35 = Control.Monad.Identity.runIdentity (attach_T_NamedType (arg_hd_))
         _tlX38 = Control.Monad.Identity.runIdentity (attach_T_NamedTypes (arg_tl_))
         (T_NamedType_vOut34 _hdIpp) = inv_NamedType_s35 _hdX35 (T_NamedType_vIn34 )
         (T_NamedTypes_vOut37 _tlIpps) = inv_NamedTypes_s38 _tlX38 (T_NamedTypes_vIn37 )
         _lhsOpps :: PP_Docs
         _lhsOpps = rule118 _hdIpp _tlIpps
         !__result_ = T_NamedTypes_vOut37 _lhsOpps
         in __result_ )
     in C_NamedTypes_s38 v37
   {-# INLINE rule118 #-}
   {-# LINE 77 "./src-ag/PrintOcamlCode.ag" #-}
   rule118 = \ ((_hdIpp) :: PP_Doc) ((_tlIpps) :: PP_Docs) ->
                     {-# LINE 77 "./src-ag/PrintOcamlCode.ag" #-}
                     _hdIpp : _tlIpps
                     {-# LINE 1834 "dist/build/PrintOcamlCode.hs"#-}
{-# NOINLINE sem_NamedTypes_Nil #-}
sem_NamedTypes_Nil ::  T_NamedTypes 
sem_NamedTypes_Nil  = T_NamedTypes (return st38) where
   {-# NOINLINE st38 #-}
   !st38 = let
      v37 :: T_NamedTypes_v37 
      v37 = \ !(T_NamedTypes_vIn37 ) -> ( let
         _lhsOpps :: PP_Docs
         _lhsOpps = rule119  ()
         !__result_ = T_NamedTypes_vOut37 _lhsOpps
         in __result_ )
     in C_NamedTypes_s38 v37
   {-# INLINE rule119 #-}
   {-# LINE 78 "./src-ag/PrintOcamlCode.ag" #-}
   rule119 = \  (_ :: ()) ->
                     {-# LINE 78 "./src-ag/PrintOcamlCode.ag" #-}
                     []
                     {-# LINE 1852 "dist/build/PrintOcamlCode.hs"#-}

-- Pattern -----------------------------------------------------
-- wrapper
data Inh_Pattern  = Inh_Pattern { options_Inh_Pattern :: !(Options) }
data Syn_Pattern  = Syn_Pattern { copy_Syn_Pattern :: !(Pattern), isUnderscore_Syn_Pattern :: !(Bool), pp_Syn_Pattern :: !(PP_Doc) }
{-# INLINABLE wrap_Pattern #-}
wrap_Pattern :: T_Pattern  -> Inh_Pattern  -> (Syn_Pattern )
wrap_Pattern !(T_Pattern act) !(Inh_Pattern _lhsIoptions) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_Pattern_vIn40 _lhsIoptions
        !(T_Pattern_vOut40 _lhsOcopy _lhsOisUnderscore _lhsOpp) <- return (inv_Pattern_s41 sem arg)
        return (Syn_Pattern _lhsOcopy _lhsOisUnderscore _lhsOpp)
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
                               attach_T_Pattern :: Identity (T_Pattern_s41 )
                               }
newtype T_Pattern_s41  = C_Pattern_s41 {
                                       inv_Pattern_s41 :: (T_Pattern_v40 )
                                       }
data T_Pattern_s42  = C_Pattern_s42
type T_Pattern_v40  = (T_Pattern_vIn40 ) -> (T_Pattern_vOut40 )
data T_Pattern_vIn40  = T_Pattern_vIn40 (Options)
data T_Pattern_vOut40  = T_Pattern_vOut40 (Pattern) (Bool) (PP_Doc)
{-# NOINLINE sem_Pattern_Constr #-}
sem_Pattern_Constr :: (ConstructorIdent) -> T_Patterns  -> T_Pattern 
sem_Pattern_Constr !arg_name_ arg_pats_ = T_Pattern (return st41) where
   {-# NOINLINE st41 #-}
   !st41 = let
      v40 :: T_Pattern_v40 
      v40 = \ !(T_Pattern_vIn40 _lhsIoptions) -> ( let
         _patsX44 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_pats_))
         (T_Patterns_vOut43 _patsIcopy _patsIpps) = inv_Patterns_s44 _patsX44 (T_Patterns_vIn43 _patsOoptions)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule120 _patsIpps arg_name_
         _lhsOisUnderscore :: Bool
         _lhsOisUnderscore = rule121  ()
         _copy = rule122 _patsIcopy arg_name_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule123 _copy
         _patsOoptions = rule124 _lhsIoptions
         !__result_ = T_Pattern_vOut40 _lhsOcopy _lhsOisUnderscore _lhsOpp
         in __result_ )
     in C_Pattern_s41 v40
   {-# INLINE rule120 #-}
   {-# LINE 192 "./src-ag/PrintOcamlCode.ag" #-}
   rule120 = \ ((_patsIpps) :: PP_Docs) name_ ->
                           {-# LINE 192 "./src-ag/PrintOcamlCode.ag" #-}
                           pp_parens $ name_ >#< hv_sp _patsIpps
                           {-# LINE 1913 "dist/build/PrintOcamlCode.hs"#-}
   {-# INLINE rule121 #-}
   {-# LINE 202 "./src-ag/PrintOcamlCode.ag" #-}
   rule121 = \  (_ :: ()) ->
                                    {-# LINE 202 "./src-ag/PrintOcamlCode.ag" #-}
                                    False
                                    {-# LINE 1919 "dist/build/PrintOcamlCode.hs"#-}
   {-# INLINE rule122 #-}
   rule122 = \ ((_patsIcopy) :: Patterns) name_ ->
     Constr name_ _patsIcopy
   {-# INLINE rule123 #-}
   rule123 = \ _copy ->
     _copy
   {-# INLINE rule124 #-}
   rule124 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Pattern_Product #-}
sem_Pattern_Product :: (Pos) -> T_Patterns  -> T_Pattern 
sem_Pattern_Product !arg_pos_ arg_pats_ = T_Pattern (return st41) where
   {-# NOINLINE st41 #-}
   !st41 = let
      v40 :: T_Pattern_v40 
      v40 = \ !(T_Pattern_vIn40 _lhsIoptions) -> ( let
         _patsX44 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_pats_))
         (T_Patterns_vOut43 _patsIcopy _patsIpps) = inv_Patterns_s44 _patsX44 (T_Patterns_vIn43 _patsOoptions)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule125 _patsIpps
         _lhsOisUnderscore :: Bool
         _lhsOisUnderscore = rule126  ()
         _copy = rule127 _patsIcopy arg_pos_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule128 _copy
         _patsOoptions = rule129 _lhsIoptions
         !__result_ = T_Pattern_vOut40 _lhsOcopy _lhsOisUnderscore _lhsOpp
         in __result_ )
     in C_Pattern_s41 v40
   {-# INLINE rule125 #-}
   {-# LINE 193 "./src-ag/PrintOcamlCode.ag" #-}
   rule125 = \ ((_patsIpps) :: PP_Docs) ->
                           {-# LINE 193 "./src-ag/PrintOcamlCode.ag" #-}
                           pp_block "(" ")" "," _patsIpps
                           {-# LINE 1954 "dist/build/PrintOcamlCode.hs"#-}
   {-# INLINE rule126 #-}
   {-# LINE 203 "./src-ag/PrintOcamlCode.ag" #-}
   rule126 = \  (_ :: ()) ->
                                    {-# LINE 203 "./src-ag/PrintOcamlCode.ag" #-}
                                    False
                                    {-# LINE 1960 "dist/build/PrintOcamlCode.hs"#-}
   {-# INLINE rule127 #-}
   rule127 = \ ((_patsIcopy) :: Patterns) pos_ ->
     Product pos_ _patsIcopy
   {-# INLINE rule128 #-}
   rule128 = \ _copy ->
     _copy
   {-# INLINE rule129 #-}
   rule129 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Pattern_Alias #-}
sem_Pattern_Alias :: (Identifier) -> (Identifier) -> T_Pattern  -> T_Pattern 
sem_Pattern_Alias !arg_field_ !arg_attr_ arg_pat_ = T_Pattern (return st41) where
   {-# NOINLINE st41 #-}
   !st41 = let
      v40 :: T_Pattern_v40 
      v40 = \ !(T_Pattern_vIn40 _lhsIoptions) -> ( let
         _patX41 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pat_))
         (T_Pattern_vOut40 _patIcopy _patIisUnderscore _patIpp) = inv_Pattern_s41 _patX41 (T_Pattern_vIn40 _patOoptions)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule130 _patIisUnderscore arg_attr_ arg_field_
         _lhsOisUnderscore :: Bool
         _lhsOisUnderscore = rule131  ()
         _copy = rule132 _patIcopy arg_attr_ arg_field_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule133 _copy
         _patOoptions = rule134 _lhsIoptions
         !__result_ = T_Pattern_vOut40 _lhsOcopy _lhsOisUnderscore _lhsOpp
         in __result_ )
     in C_Pattern_s41 v40
   {-# INLINE rule130 #-}
   {-# LINE 195 "./src-ag/PrintOcamlCode.ag" #-}
   rule130 = \ ((_patIisUnderscore) :: Bool) attr_ field_ ->
                           {-# LINE 195 "./src-ag/PrintOcamlCode.ag" #-}
                           if _patIisUnderscore
                            then pp (attrname False field_ attr_)
                            else error "pp of Pattern.Alias is only supported in the form (x@_)"
                           {-# LINE 1997 "dist/build/PrintOcamlCode.hs"#-}
   {-# INLINE rule131 #-}
   {-# LINE 204 "./src-ag/PrintOcamlCode.ag" #-}
   rule131 = \  (_ :: ()) ->
                                    {-# LINE 204 "./src-ag/PrintOcamlCode.ag" #-}
                                    False
                                    {-# LINE 2003 "dist/build/PrintOcamlCode.hs"#-}
   {-# INLINE rule132 #-}
   rule132 = \ ((_patIcopy) :: Pattern) attr_ field_ ->
     Alias field_ attr_ _patIcopy
   {-# INLINE rule133 #-}
   rule133 = \ _copy ->
     _copy
   {-# INLINE rule134 #-}
   rule134 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Pattern_Irrefutable #-}
sem_Pattern_Irrefutable :: T_Pattern  -> T_Pattern 
sem_Pattern_Irrefutable arg_pat_ = T_Pattern (return st41) where
   {-# NOINLINE st41 #-}
   !st41 = let
      v40 :: T_Pattern_v40 
      v40 = \ !(T_Pattern_vIn40 _lhsIoptions) -> ( let
         _patX41 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pat_))
         (T_Pattern_vOut40 _patIcopy _patIisUnderscore _patIpp) = inv_Pattern_s41 _patX41 (T_Pattern_vIn40 _patOoptions)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule135  ()
         _copy = rule136 _patIcopy
         _lhsOcopy :: Pattern
         _lhsOcopy = rule137 _copy
         _lhsOisUnderscore :: Bool
         _lhsOisUnderscore = rule138 _patIisUnderscore
         _patOoptions = rule139 _lhsIoptions
         !__result_ = T_Pattern_vOut40 _lhsOcopy _lhsOisUnderscore _lhsOpp
         in __result_ )
     in C_Pattern_s41 v40
   {-# INLINE rule135 #-}
   {-# LINE 198 "./src-ag/PrintOcamlCode.ag" #-}
   rule135 = \  (_ :: ()) ->
                           {-# LINE 198 "./src-ag/PrintOcamlCode.ag" #-}
                           error "pp of Pattern.Irrefutable not supported"
                           {-# LINE 2038 "dist/build/PrintOcamlCode.hs"#-}
   {-# INLINE rule136 #-}
   rule136 = \ ((_patIcopy) :: Pattern) ->
     Irrefutable _patIcopy
   {-# INLINE rule137 #-}
   rule137 = \ _copy ->
     _copy
   {-# INLINE rule138 #-}
   rule138 = \ ((_patIisUnderscore) :: Bool) ->
     _patIisUnderscore
   {-# INLINE rule139 #-}
   rule139 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Pattern_Underscore #-}
sem_Pattern_Underscore :: (Pos) -> T_Pattern 
sem_Pattern_Underscore !arg_pos_ = T_Pattern (return st41) where
   {-# NOINLINE st41 #-}
   !st41 = let
      v40 :: T_Pattern_v40 
      v40 = \ !(T_Pattern_vIn40 _lhsIoptions) -> ( let
         _lhsOpp :: PP_Doc
         _lhsOpp = rule140  ()
         _lhsOisUnderscore :: Bool
         _lhsOisUnderscore = rule141  ()
         _copy = rule142 arg_pos_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule143 _copy
         !__result_ = T_Pattern_vOut40 _lhsOcopy _lhsOisUnderscore _lhsOpp
         in __result_ )
     in C_Pattern_s41 v40
   {-# INLINE rule140 #-}
   {-# LINE 199 "./src-ag/PrintOcamlCode.ag" #-}
   rule140 = \  (_ :: ()) ->
                           {-# LINE 199 "./src-ag/PrintOcamlCode.ag" #-}
                           text "_"
                           {-# LINE 2073 "dist/build/PrintOcamlCode.hs"#-}
   {-# INLINE rule141 #-}
   {-# LINE 205 "./src-ag/PrintOcamlCode.ag" #-}
   rule141 = \  (_ :: ()) ->
                                    {-# LINE 205 "./src-ag/PrintOcamlCode.ag" #-}
                                    True
                                    {-# LINE 2079 "dist/build/PrintOcamlCode.hs"#-}
   {-# INLINE rule142 #-}
   rule142 = \ pos_ ->
     Underscore pos_
   {-# INLINE rule143 #-}
   rule143 = \ _copy ->
     _copy

-- Patterns ----------------------------------------------------
-- wrapper
data Inh_Patterns  = Inh_Patterns { options_Inh_Patterns :: !(Options) }
data Syn_Patterns  = Syn_Patterns { copy_Syn_Patterns :: !(Patterns), pps_Syn_Patterns :: !(PP_Docs) }
{-# INLINABLE wrap_Patterns #-}
wrap_Patterns :: T_Patterns  -> Inh_Patterns  -> (Syn_Patterns )
wrap_Patterns !(T_Patterns act) !(Inh_Patterns _lhsIoptions) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_Patterns_vIn43 _lhsIoptions
        !(T_Patterns_vOut43 _lhsOcopy _lhsOpps) <- return (inv_Patterns_s44 sem arg)
        return (Syn_Patterns _lhsOcopy _lhsOpps)
   )

-- cata
{-# NOINLINE sem_Patterns #-}
sem_Patterns :: Patterns  -> T_Patterns 
sem_Patterns list = Prelude.foldr sem_Patterns_Cons sem_Patterns_Nil (Prelude.map sem_Pattern list)

-- semantic domain
newtype T_Patterns  = T_Patterns {
                                 attach_T_Patterns :: Identity (T_Patterns_s44 )
                                 }
newtype T_Patterns_s44  = C_Patterns_s44 {
                                         inv_Patterns_s44 :: (T_Patterns_v43 )
                                         }
data T_Patterns_s45  = C_Patterns_s45
type T_Patterns_v43  = (T_Patterns_vIn43 ) -> (T_Patterns_vOut43 )
data T_Patterns_vIn43  = T_Patterns_vIn43 (Options)
data T_Patterns_vOut43  = T_Patterns_vOut43 (Patterns) (PP_Docs)
{-# NOINLINE sem_Patterns_Cons #-}
sem_Patterns_Cons :: T_Pattern  -> T_Patterns  -> T_Patterns 
sem_Patterns_Cons arg_hd_ arg_tl_ = T_Patterns (return st44) where
   {-# NOINLINE st44 #-}
   !st44 = let
      v43 :: T_Patterns_v43 
      v43 = \ !(T_Patterns_vIn43 _lhsIoptions) -> ( let
         _hdX41 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_hd_))
         _tlX44 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_tl_))
         (T_Pattern_vOut40 _hdIcopy _hdIisUnderscore _hdIpp) = inv_Pattern_s41 _hdX41 (T_Pattern_vIn40 _hdOoptions)
         (T_Patterns_vOut43 _tlIcopy _tlIpps) = inv_Patterns_s44 _tlX44 (T_Patterns_vIn43 _tlOoptions)
         _lhsOpps :: PP_Docs
         _lhsOpps = rule144 _hdIpp _tlIpps
         _copy = rule145 _hdIcopy _tlIcopy
         _lhsOcopy :: Patterns
         _lhsOcopy = rule146 _copy
         _hdOoptions = rule147 _lhsIoptions
         _tlOoptions = rule148 _lhsIoptions
         !__result_ = T_Patterns_vOut43 _lhsOcopy _lhsOpps
         in __result_ )
     in C_Patterns_s44 v43
   {-# INLINE rule144 #-}
   {-# LINE 89 "./src-ag/PrintOcamlCode.ag" #-}
   rule144 = \ ((_hdIpp) :: PP_Doc) ((_tlIpps) :: PP_Docs) ->
                     {-# LINE 89 "./src-ag/PrintOcamlCode.ag" #-}
                     _hdIpp : _tlIpps
                     {-# LINE 2143 "dist/build/PrintOcamlCode.hs"#-}
   {-# INLINE rule145 #-}
   rule145 = \ ((_hdIcopy) :: Pattern) ((_tlIcopy) :: Patterns) ->
     (:) _hdIcopy _tlIcopy
   {-# INLINE rule146 #-}
   rule146 = \ _copy ->
     _copy
   {-# INLINE rule147 #-}
   rule147 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule148 #-}
   rule148 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Patterns_Nil #-}
sem_Patterns_Nil ::  T_Patterns 
sem_Patterns_Nil  = T_Patterns (return st44) where
   {-# NOINLINE st44 #-}
   !st44 = let
      v43 :: T_Patterns_v43 
      v43 = \ !(T_Patterns_vIn43 _lhsIoptions) -> ( let
         _lhsOpps :: PP_Docs
         _lhsOpps = rule149  ()
         _copy = rule150  ()
         _lhsOcopy :: Patterns
         _lhsOcopy = rule151 _copy
         !__result_ = T_Patterns_vOut43 _lhsOcopy _lhsOpps
         in __result_ )
     in C_Patterns_s44 v43
   {-# INLINE rule149 #-}
   {-# LINE 90 "./src-ag/PrintOcamlCode.ag" #-}
   rule149 = \  (_ :: ()) ->
                     {-# LINE 90 "./src-ag/PrintOcamlCode.ag" #-}
                     []
                     {-# LINE 2176 "dist/build/PrintOcamlCode.hs"#-}
   {-# INLINE rule150 #-}
   rule150 = \  (_ :: ()) ->
     []
   {-# INLINE rule151 #-}
   rule151 = \ _copy ->
     _copy

-- Program -----------------------------------------------------
-- wrapper
data Inh_Program  = Inh_Program { options_Inh_Program :: !(Options), textBlockMap_Inh_Program :: !(Map BlockInfo PP_Doc) }
data Syn_Program  = Syn_Program { output_Syn_Program :: !(PP_Docs) }
{-# INLINABLE wrap_Program #-}
wrap_Program :: T_Program  -> Inh_Program  -> (Syn_Program )
wrap_Program !(T_Program act) !(Inh_Program _lhsIoptions _lhsItextBlockMap) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_Program_vIn46 _lhsIoptions _lhsItextBlockMap
        !(T_Program_vOut46 _lhsOoutput) <- return (inv_Program_s47 sem arg)
        return (Syn_Program _lhsOoutput)
   )

-- cata
{-# INLINE sem_Program #-}
sem_Program :: Program  -> T_Program 
sem_Program ( Program chunks_ !ordered_ ) = sem_Program_Program ( sem_Chunks chunks_ ) ordered_

-- semantic domain
newtype T_Program  = T_Program {
                               attach_T_Program :: Identity (T_Program_s47 )
                               }
newtype T_Program_s47  = C_Program_s47 {
                                       inv_Program_s47 :: (T_Program_v46 )
                                       }
data T_Program_s48  = C_Program_s48
type T_Program_v46  = (T_Program_vIn46 ) -> (T_Program_vOut46 )
data T_Program_vIn46  = T_Program_vIn46 (Options) (Map BlockInfo PP_Doc)
data T_Program_vOut46  = T_Program_vOut46 (PP_Docs)
{-# NOINLINE sem_Program_Program #-}
sem_Program_Program :: T_Chunks  -> (Bool) -> T_Program 
sem_Program_Program arg_chunks_ _ = T_Program (return st47) where
   {-# NOINLINE st47 #-}
   !st47 = let
      v46 :: T_Program_v46 
      v46 = \ !(T_Program_vIn46 _lhsIoptions _lhsItextBlockMap) -> ( let
         _chunksX11 = Control.Monad.Identity.runIdentity (attach_T_Chunks (arg_chunks_))
         (T_Chunks_vOut10 _chunksIpps) = inv_Chunks_s11 _chunksX11 (T_Chunks_vIn10 _chunksOisToplevel _chunksOoptions _chunksOtextBlockMap)
         _lhsOoutput :: PP_Docs
         _lhsOoutput = rule152 _chunksIpps
         _chunksOisToplevel = rule153  ()
         _chunksOoptions = rule154 _lhsIoptions
         _chunksOtextBlockMap = rule155 _lhsItextBlockMap
         !__result_ = T_Program_vOut46 _lhsOoutput
         in __result_ )
     in C_Program_s47 v46
   {-# INLINE rule152 #-}
   {-# LINE 58 "./src-ag/PrintOcamlCode.ag" #-}
   rule152 = \ ((_chunksIpps) :: PP_Docs) ->
                               {-# LINE 58 "./src-ag/PrintOcamlCode.ag" #-}
                               _chunksIpps
                               {-# LINE 2236 "dist/build/PrintOcamlCode.hs"#-}
   {-# INLINE rule153 #-}
   {-# LINE 214 "./src-ag/PrintOcamlCode.ag" #-}
   rule153 = \  (_ :: ()) ->
                            {-# LINE 214 "./src-ag/PrintOcamlCode.ag" #-}
                            True
                            {-# LINE 2242 "dist/build/PrintOcamlCode.hs"#-}
   {-# INLINE rule154 #-}
   rule154 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule155 #-}
   rule155 = \ ((_lhsItextBlockMap) :: Map BlockInfo PP_Doc) ->
     _lhsItextBlockMap

-- Type --------------------------------------------------------
-- wrapper
data Inh_Type  = Inh_Type {  }
data Syn_Type  = Syn_Type { pp_Syn_Type :: !(PP_Doc) }
{-# INLINABLE wrap_Type #-}
wrap_Type :: T_Type  -> Inh_Type  -> (Syn_Type )
wrap_Type !(T_Type act) !(Inh_Type ) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_Type_vIn49 
        !(T_Type_vOut49 _lhsOpp) <- return (inv_Type_s50 sem arg)
        return (Syn_Type _lhsOpp)
   )

-- cata
{-# NOINLINE sem_Type #-}
sem_Type :: Type  -> T_Type 
sem_Type ( Arr left_ right_ ) = sem_Type_Arr ( sem_Type left_ ) ( sem_Type right_ )
sem_Type ( CtxApp !left_ right_ ) = sem_Type_CtxApp left_ ( sem_Type right_ )
sem_Type ( QuantApp !left_ right_ ) = sem_Type_QuantApp left_ ( sem_Type right_ )
sem_Type ( TypeApp func_ args_ ) = sem_Type_TypeApp ( sem_Type func_ ) ( sem_Types args_ )
sem_Type ( TupleType tps_ ) = sem_Type_TupleType ( sem_Types tps_ )
sem_Type ( UnboxedTupleType tps_ ) = sem_Type_UnboxedTupleType ( sem_Types tps_ )
sem_Type ( List tp_ ) = sem_Type_List ( sem_Type tp_ )
sem_Type ( SimpleType !txt_ ) = sem_Type_SimpleType txt_
sem_Type ( NontermType !name_ !params_ !deforested_ ) = sem_Type_NontermType name_ params_ deforested_
sem_Type ( TMaybe tp_ ) = sem_Type_TMaybe ( sem_Type tp_ )
sem_Type ( TEither left_ right_ ) = sem_Type_TEither ( sem_Type left_ ) ( sem_Type right_ )
sem_Type ( TMap key_ value_ ) = sem_Type_TMap ( sem_Type key_ ) ( sem_Type value_ )
sem_Type ( TIntMap value_ ) = sem_Type_TIntMap ( sem_Type value_ )

-- semantic domain
newtype T_Type  = T_Type {
                         attach_T_Type :: Identity (T_Type_s50 )
                         }
newtype T_Type_s50  = C_Type_s50 {
                                 inv_Type_s50 :: (T_Type_v49 )
                                 }
data T_Type_s51  = C_Type_s51
type T_Type_v49  = (T_Type_vIn49 ) -> (T_Type_vOut49 )
data T_Type_vIn49  = T_Type_vIn49 
data T_Type_vOut49  = T_Type_vOut49 (PP_Doc)
{-# NOINLINE sem_Type_Arr #-}
sem_Type_Arr :: T_Type  -> T_Type  -> T_Type 
sem_Type_Arr arg_left_ arg_right_ = T_Type (return st50) where
   {-# NOINLINE st50 #-}
   !st50 = let
      v49 :: T_Type_v49 
      v49 = \ !(T_Type_vIn49 ) -> ( let
         _leftX50 = Control.Monad.Identity.runIdentity (attach_T_Type (arg_left_))
         _rightX50 = Control.Monad.Identity.runIdentity (attach_T_Type (arg_right_))
         (T_Type_vOut49 _leftIpp) = inv_Type_s50 _leftX50 (T_Type_vIn49 )
         (T_Type_vOut49 _rightIpp) = inv_Type_s50 _rightX50 (T_Type_vIn49 )
         _lhsOpp :: PP_Doc
         _lhsOpp = rule156 _leftIpp _rightIpp
         !__result_ = T_Type_vOut49 _lhsOpp
         in __result_ )
     in C_Type_s50 v49
   {-# INLINE rule156 #-}
   {-# LINE 161 "./src-ag/PrintOcamlCode.ag" #-}
   rule156 = \ ((_leftIpp) :: PP_Doc) ((_rightIpp) :: PP_Doc) ->
                          {-# LINE 161 "./src-ag/PrintOcamlCode.ag" #-}
                          pp_parens (_leftIpp >#< "->" >#< _rightIpp)
                          {-# LINE 2313 "dist/build/PrintOcamlCode.hs"#-}
{-# NOINLINE sem_Type_CtxApp #-}
sem_Type_CtxApp :: ([(String, [String])]) -> T_Type  -> T_Type 
sem_Type_CtxApp _ arg_right_ = T_Type (return st50) where
   {-# NOINLINE st50 #-}
   !st50 = let
      v49 :: T_Type_v49 
      v49 = \ !(T_Type_vIn49 ) -> ( let
         _rightX50 = Control.Monad.Identity.runIdentity (attach_T_Type (arg_right_))
         (T_Type_vOut49 _rightIpp) = inv_Type_s50 _rightX50 (T_Type_vIn49 )
         _lhsOpp :: PP_Doc
         _lhsOpp = rule157  ()
         !__result_ = T_Type_vOut49 _lhsOpp
         in __result_ )
     in C_Type_s50 v49
   {-# INLINE rule157 #-}
   {-# LINE 162 "./src-ag/PrintOcamlCode.ag" #-}
   rule157 = \  (_ :: ()) ->
                          {-# LINE 162 "./src-ag/PrintOcamlCode.ag" #-}
                          error "pp of Type.CtxApp not supported"
                          {-# LINE 2333 "dist/build/PrintOcamlCode.hs"#-}
{-# NOINLINE sem_Type_QuantApp #-}
sem_Type_QuantApp :: (String) -> T_Type  -> T_Type 
sem_Type_QuantApp _ arg_right_ = T_Type (return st50) where
   {-# NOINLINE st50 #-}
   !st50 = let
      v49 :: T_Type_v49 
      v49 = \ !(T_Type_vIn49 ) -> ( let
         _rightX50 = Control.Monad.Identity.runIdentity (attach_T_Type (arg_right_))
         (T_Type_vOut49 _rightIpp) = inv_Type_s50 _rightX50 (T_Type_vIn49 )
         _lhsOpp :: PP_Doc
         _lhsOpp = rule158 _rightIpp
         !__result_ = T_Type_vOut49 _lhsOpp
         in __result_ )
     in C_Type_s50 v49
   {-# INLINE rule158 #-}
   rule158 = \ ((_rightIpp) :: PP_Doc) ->
     _rightIpp
{-# NOINLINE sem_Type_TypeApp #-}
sem_Type_TypeApp :: T_Type  -> T_Types  -> T_Type 
sem_Type_TypeApp arg_func_ arg_args_ = T_Type (return st50) where
   {-# NOINLINE st50 #-}
   !st50 = let
      v49 :: T_Type_v49 
      v49 = \ !(T_Type_vIn49 ) -> ( let
         _funcX50 = Control.Monad.Identity.runIdentity (attach_T_Type (arg_func_))
         _argsX53 = Control.Monad.Identity.runIdentity (attach_T_Types (arg_args_))
         (T_Type_vOut49 _funcIpp) = inv_Type_s50 _funcX50 (T_Type_vIn49 )
         (T_Types_vOut52 _argsIpps) = inv_Types_s53 _argsX53 (T_Types_vIn52 )
         _lhsOpp :: PP_Doc
         _lhsOpp = rule159 _argsIpps _funcIpp
         !__result_ = T_Type_vOut49 _lhsOpp
         in __result_ )
     in C_Type_s50 v49
   {-# INLINE rule159 #-}
   {-# LINE 163 "./src-ag/PrintOcamlCode.ag" #-}
   rule159 = \ ((_argsIpps) :: PP_Docs) ((_funcIpp) :: PP_Doc) ->
                          {-# LINE 163 "./src-ag/PrintOcamlCode.ag" #-}
                          pp_parens (hv_sp (_argsIpps ++ [_funcIpp]))
                          {-# LINE 2372 "dist/build/PrintOcamlCode.hs"#-}
{-# NOINLINE sem_Type_TupleType #-}
sem_Type_TupleType :: T_Types  -> T_Type 
sem_Type_TupleType arg_tps_ = T_Type (return st50) where
   {-# NOINLINE st50 #-}
   !st50 = let
      v49 :: T_Type_v49 
      v49 = \ !(T_Type_vIn49 ) -> ( let
         _tpsX53 = Control.Monad.Identity.runIdentity (attach_T_Types (arg_tps_))
         (T_Types_vOut52 _tpsIpps) = inv_Types_s53 _tpsX53 (T_Types_vIn52 )
         _lhsOpp :: PP_Doc
         _lhsOpp = rule160 _tpsIpps
         !__result_ = T_Type_vOut49 _lhsOpp
         in __result_ )
     in C_Type_s50 v49
   {-# INLINE rule160 #-}
   {-# LINE 164 "./src-ag/PrintOcamlCode.ag" #-}
   rule160 = \ ((_tpsIpps) :: PP_Docs) ->
                          {-# LINE 164 "./src-ag/PrintOcamlCode.ag" #-}
                          pp_block "(" ")" "," _tpsIpps
                          {-# LINE 2392 "dist/build/PrintOcamlCode.hs"#-}
{-# NOINLINE sem_Type_UnboxedTupleType #-}
sem_Type_UnboxedTupleType :: T_Types  -> T_Type 
sem_Type_UnboxedTupleType arg_tps_ = T_Type (return st50) where
   {-# NOINLINE st50 #-}
   !st50 = let
      v49 :: T_Type_v49 
      v49 = \ !(T_Type_vIn49 ) -> ( let
         _tpsX53 = Control.Monad.Identity.runIdentity (attach_T_Types (arg_tps_))
         (T_Types_vOut52 _tpsIpps) = inv_Types_s53 _tpsX53 (T_Types_vIn52 )
         _lhsOpp :: PP_Doc
         _lhsOpp = rule161  ()
         !__result_ = T_Type_vOut49 _lhsOpp
         in __result_ )
     in C_Type_s50 v49
   {-# INLINE rule161 #-}
   {-# LINE 166 "./src-ag/PrintOcamlCode.ag" #-}
   rule161 = \  (_ :: ()) ->
                          {-# LINE 166 "./src-ag/PrintOcamlCode.ag" #-}
                          error "pp of Type.UnboxedTupleType is not supported"
                          {-# LINE 2412 "dist/build/PrintOcamlCode.hs"#-}
{-# NOINLINE sem_Type_List #-}
sem_Type_List :: T_Type  -> T_Type 
sem_Type_List arg_tp_ = T_Type (return st50) where
   {-# NOINLINE st50 #-}
   !st50 = let
      v49 :: T_Type_v49 
      v49 = \ !(T_Type_vIn49 ) -> ( let
         _tpX50 = Control.Monad.Identity.runIdentity (attach_T_Type (arg_tp_))
         (T_Type_vOut49 _tpIpp) = inv_Type_s50 _tpX50 (T_Type_vIn49 )
         _lhsOpp :: PP_Doc
         _lhsOpp = rule162 _tpIpp
         !__result_ = T_Type_vOut49 _lhsOpp
         in __result_ )
     in C_Type_s50 v49
   {-# INLINE rule162 #-}
   {-# LINE 167 "./src-ag/PrintOcamlCode.ag" #-}
   rule162 = \ ((_tpIpp) :: PP_Doc) ->
                          {-# LINE 167 "./src-ag/PrintOcamlCode.ag" #-}
                          _tpIpp >#< "list"
                          {-# LINE 2432 "dist/build/PrintOcamlCode.hs"#-}
{-# NOINLINE sem_Type_SimpleType #-}
sem_Type_SimpleType :: (String) -> T_Type 
sem_Type_SimpleType !arg_txt_ = T_Type (return st50) where
   {-# NOINLINE st50 #-}
   !st50 = let
      v49 :: T_Type_v49 
      v49 = \ !(T_Type_vIn49 ) -> ( let
         _lhsOpp :: PP_Doc
         _lhsOpp = rule163 arg_txt_
         !__result_ = T_Type_vOut49 _lhsOpp
         in __result_ )
     in C_Type_s50 v49
   {-# INLINE rule163 #-}
   {-# LINE 168 "./src-ag/PrintOcamlCode.ag" #-}
   rule163 = \ txt_ ->
                          {-# LINE 168 "./src-ag/PrintOcamlCode.ag" #-}
                          text txt_
                          {-# LINE 2450 "dist/build/PrintOcamlCode.hs"#-}
{-# NOINLINE sem_Type_NontermType #-}
sem_Type_NontermType :: (String) -> ([String]) -> (Bool) -> T_Type 
sem_Type_NontermType !arg_name_ !arg_params_ _ = T_Type (return st50) where
   {-# NOINLINE st50 #-}
   !st50 = let
      v49 :: T_Type_v49 
      v49 = \ !(T_Type_vIn49 ) -> ( let
         _lhsOpp :: PP_Doc
         _lhsOpp = rule164 arg_name_ arg_params_
         !__result_ = T_Type_vOut49 _lhsOpp
         in __result_ )
     in C_Type_s50 v49
   {-# INLINE rule164 #-}
   {-# LINE 169 "./src-ag/PrintOcamlCode.ag" #-}
   rule164 = \ name_ params_ ->
                           {-# LINE 169 "./src-ag/PrintOcamlCode.ag" #-}
                           pp_block "(" ")" " " (map text params_ ++ [text $ toOcamlTC name_])
                           {-# LINE 2468 "dist/build/PrintOcamlCode.hs"#-}
{-# NOINLINE sem_Type_TMaybe #-}
sem_Type_TMaybe :: T_Type  -> T_Type 
sem_Type_TMaybe arg_tp_ = T_Type (return st50) where
   {-# NOINLINE st50 #-}
   !st50 = let
      v49 :: T_Type_v49 
      v49 = \ !(T_Type_vIn49 ) -> ( let
         _tpX50 = Control.Monad.Identity.runIdentity (attach_T_Type (arg_tp_))
         (T_Type_vOut49 _tpIpp) = inv_Type_s50 _tpX50 (T_Type_vIn49 )
         _lhsOpp :: PP_Doc
         _lhsOpp = rule165 _tpIpp
         !__result_ = T_Type_vOut49 _lhsOpp
         in __result_ )
     in C_Type_s50 v49
   {-# INLINE rule165 #-}
   {-# LINE 170 "./src-ag/PrintOcamlCode.ag" #-}
   rule165 = \ ((_tpIpp) :: PP_Doc) ->
                          {-# LINE 170 "./src-ag/PrintOcamlCode.ag" #-}
                          _tpIpp >#< "opt"
                          {-# LINE 2488 "dist/build/PrintOcamlCode.hs"#-}
{-# NOINLINE sem_Type_TEither #-}
sem_Type_TEither :: T_Type  -> T_Type  -> T_Type 
sem_Type_TEither arg_left_ arg_right_ = T_Type (return st50) where
   {-# NOINLINE st50 #-}
   !st50 = let
      v49 :: T_Type_v49 
      v49 = \ !(T_Type_vIn49 ) -> ( let
         _leftX50 = Control.Monad.Identity.runIdentity (attach_T_Type (arg_left_))
         _rightX50 = Control.Monad.Identity.runIdentity (attach_T_Type (arg_right_))
         (T_Type_vOut49 _leftIpp) = inv_Type_s50 _leftX50 (T_Type_vIn49 )
         (T_Type_vOut49 _rightIpp) = inv_Type_s50 _rightX50 (T_Type_vIn49 )
         _lhsOpp :: PP_Doc
         _lhsOpp = rule166  ()
         !__result_ = T_Type_vOut49 _lhsOpp
         in __result_ )
     in C_Type_s50 v49
   {-# INLINE rule166 #-}
   {-# LINE 171 "./src-ag/PrintOcamlCode.ag" #-}
   rule166 = \  (_ :: ()) ->
                          {-# LINE 171 "./src-ag/PrintOcamlCode.ag" #-}
                          error "pp of Type.TEither is not supported"
                          {-# LINE 2510 "dist/build/PrintOcamlCode.hs"#-}
{-# NOINLINE sem_Type_TMap #-}
sem_Type_TMap :: T_Type  -> T_Type  -> T_Type 
sem_Type_TMap arg_key_ arg_value_ = T_Type (return st50) where
   {-# NOINLINE st50 #-}
   !st50 = let
      v49 :: T_Type_v49 
      v49 = \ !(T_Type_vIn49 ) -> ( let
         _keyX50 = Control.Monad.Identity.runIdentity (attach_T_Type (arg_key_))
         _valueX50 = Control.Monad.Identity.runIdentity (attach_T_Type (arg_value_))
         (T_Type_vOut49 _keyIpp) = inv_Type_s50 _keyX50 (T_Type_vIn49 )
         (T_Type_vOut49 _valueIpp) = inv_Type_s50 _valueX50 (T_Type_vIn49 )
         _lhsOpp :: PP_Doc
         _lhsOpp = rule167  ()
         !__result_ = T_Type_vOut49 _lhsOpp
         in __result_ )
     in C_Type_s50 v49
   {-# INLINE rule167 #-}
   {-# LINE 172 "./src-ag/PrintOcamlCode.ag" #-}
   rule167 = \  (_ :: ()) ->
                          {-# LINE 172 "./src-ag/PrintOcamlCode.ag" #-}
                          error "pp of Type.TMap is not supported"
                          {-# LINE 2532 "dist/build/PrintOcamlCode.hs"#-}
{-# NOINLINE sem_Type_TIntMap #-}
sem_Type_TIntMap :: T_Type  -> T_Type 
sem_Type_TIntMap arg_value_ = T_Type (return st50) where
   {-# NOINLINE st50 #-}
   !st50 = let
      v49 :: T_Type_v49 
      v49 = \ !(T_Type_vIn49 ) -> ( let
         _valueX50 = Control.Monad.Identity.runIdentity (attach_T_Type (arg_value_))
         (T_Type_vOut49 _valueIpp) = inv_Type_s50 _valueX50 (T_Type_vIn49 )
         _lhsOpp :: PP_Doc
         _lhsOpp = rule168  ()
         !__result_ = T_Type_vOut49 _lhsOpp
         in __result_ )
     in C_Type_s50 v49
   {-# INLINE rule168 #-}
   {-# LINE 173 "./src-ag/PrintOcamlCode.ag" #-}
   rule168 = \  (_ :: ()) ->
                          {-# LINE 173 "./src-ag/PrintOcamlCode.ag" #-}
                          error "pp of Type.TIntMap is not supported"
                          {-# LINE 2552 "dist/build/PrintOcamlCode.hs"#-}

-- Types -------------------------------------------------------
-- wrapper
data Inh_Types  = Inh_Types {  }
data Syn_Types  = Syn_Types { pps_Syn_Types :: !(PP_Docs) }
{-# INLINABLE wrap_Types #-}
wrap_Types :: T_Types  -> Inh_Types  -> (Syn_Types )
wrap_Types !(T_Types act) !(Inh_Types ) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_Types_vIn52 
        !(T_Types_vOut52 _lhsOpps) <- return (inv_Types_s53 sem arg)
        return (Syn_Types _lhsOpps)
   )

-- cata
{-# NOINLINE sem_Types #-}
sem_Types :: Types  -> T_Types 
sem_Types list = Prelude.foldr sem_Types_Cons sem_Types_Nil (Prelude.map sem_Type list)

-- semantic domain
newtype T_Types  = T_Types {
                           attach_T_Types :: Identity (T_Types_s53 )
                           }
newtype T_Types_s53  = C_Types_s53 {
                                   inv_Types_s53 :: (T_Types_v52 )
                                   }
data T_Types_s54  = C_Types_s54
type T_Types_v52  = (T_Types_vIn52 ) -> (T_Types_vOut52 )
data T_Types_vIn52  = T_Types_vIn52 
data T_Types_vOut52  = T_Types_vOut52 (PP_Docs)
{-# NOINLINE sem_Types_Cons #-}
sem_Types_Cons :: T_Type  -> T_Types  -> T_Types 
sem_Types_Cons arg_hd_ arg_tl_ = T_Types (return st53) where
   {-# NOINLINE st53 #-}
   !st53 = let
      v52 :: T_Types_v52 
      v52 = \ !(T_Types_vIn52 ) -> ( let
         _hdX50 = Control.Monad.Identity.runIdentity (attach_T_Type (arg_hd_))
         _tlX53 = Control.Monad.Identity.runIdentity (attach_T_Types (arg_tl_))
         (T_Type_vOut49 _hdIpp) = inv_Type_s50 _hdX50 (T_Type_vIn49 )
         (T_Types_vOut52 _tlIpps) = inv_Types_s53 _tlX53 (T_Types_vIn52 )
         _lhsOpps :: PP_Docs
         _lhsOpps = rule169 _hdIpp _tlIpps
         !__result_ = T_Types_vOut52 _lhsOpps
         in __result_ )
     in C_Types_s53 v52
   {-# INLINE rule169 #-}
   {-# LINE 73 "./src-ag/PrintOcamlCode.ag" #-}
   rule169 = \ ((_hdIpp) :: PP_Doc) ((_tlIpps) :: PP_Docs) ->
                     {-# LINE 73 "./src-ag/PrintOcamlCode.ag" #-}
                     _hdIpp : _tlIpps
                     {-# LINE 2605 "dist/build/PrintOcamlCode.hs"#-}
{-# NOINLINE sem_Types_Nil #-}
sem_Types_Nil ::  T_Types 
sem_Types_Nil  = T_Types (return st53) where
   {-# NOINLINE st53 #-}
   !st53 = let
      v52 :: T_Types_v52 
      v52 = \ !(T_Types_vIn52 ) -> ( let
         _lhsOpps :: PP_Docs
         _lhsOpps = rule170  ()
         !__result_ = T_Types_vOut52 _lhsOpps
         in __result_ )
     in C_Types_s53 v52
   {-# INLINE rule170 #-}
   {-# LINE 74 "./src-ag/PrintOcamlCode.ag" #-}
   rule170 = \  (_ :: ()) ->
                     {-# LINE 74 "./src-ag/PrintOcamlCode.ag" #-}
                     []
                     {-# LINE 2623 "dist/build/PrintOcamlCode.hs"#-}
