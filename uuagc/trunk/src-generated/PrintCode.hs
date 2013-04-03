{-# LANGUAGE Rank2Types, GADTs #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PrintCode where
{-# LINE 2 "./src-ag/Code.ag" #-}

import Patterns
import Data.Set(Set)
import qualified Data.Set as Set
import Data.Map(Map)
import qualified Data.Map as Map
{-# LINE 14 "dist/build/PrintCode.hs" #-}

{-# LINE 2 "./src-ag/Patterns.ag" #-}

-- Patterns.ag imports
import UU.Scanner.Position(Pos)
import CommonTypes (ConstructorIdent,Identifier)
{-# LINE 21 "dist/build/PrintCode.hs" #-}

{-# LINE 10 "./src-ag/PrintCode.ag" #-}

import Data.Char (isAlphaNum)
import Pretty
import Code
import Options
import CommonTypes (attrname, _LOC, nullIdent)
import Data.List(intersperse)
import System.IO
import System.Directory
import System.FilePath
import CommonTypes(BlockInfo, BlockKind(..))
{-# LINE 35 "dist/build/PrintCode.hs" #-}
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
{-# LINE 55 "dist/build/PrintCode.hs" #-}

{-# LINE 23 "./src-ag/PrintCode.ag" #-}

type PP_Docs = [PP_Doc]
{-# LINE 60 "dist/build/PrintCode.hs" #-}

{-# LINE 27 "./src-ag/PrintCode.ag" #-}

ppMultiSeqH :: [PP_Doc] -> PP_Doc -> PP_Doc
ppMultiSeqH = ppMultiSeq' (>#<)

ppMultiSeqV :: [PP_Doc] -> PP_Doc -> PP_Doc
ppMultiSeqV = ppMultiSeq' (>-<)

ppMultiSeq' :: (PP_Doc -> PP_Doc -> PP_Doc) -> [PP_Doc] -> PP_Doc -> PP_Doc
ppMultiSeq' next strictArgs expr
  = foldr (\v r -> (v >#< "`seq`") `next` pp_parens r) expr strictArgs
{-# LINE 73 "dist/build/PrintCode.hs" #-}

{-# LINE 299 "./src-ag/PrintCode.ag" #-}


reallySimple :: String -> Bool
reallySimple = and . map (\x -> isAlphaNum x || x=='_')

ppTuple :: Bool -> [PP_Doc] -> PP_Doc
ppTuple True  pps = "(" >|< pp_block " " (replicate (length pps `max` 1) ')') ",(" pps
ppTuple False pps = "(" >|< pp_block " " ")" "," pps
ppUnboxedTuple :: Bool -> [PP_Doc] -> PP_Doc
ppUnboxedTuple True pps  = "(# " >|< pp_block " " (concat $ replicate (length pps `max` 1) " #)") ",(# " pps
ppUnboxedTuple False pps = "(# " >|< pp_block " " " #)" "," pps

{-# LINE 88 "dist/build/PrintCode.hs" #-}

{-# LINE 400 "./src-ag/PrintCode.ag" #-}

locname' :: Identifier -> [Char]
locname' n = "_loc_" ++ getName n
{-# LINE 94 "dist/build/PrintCode.hs" #-}

{-# LINE 475 "./src-ag/PrintCode.ag" #-}

renderDocs :: [PP_Doc] -> String
renderDocs pps = foldr (.) id (map (\d -> (disp d 50000) . ( '\n':) ) pps) ""
{-# LINE 100 "dist/build/PrintCode.hs" #-}

{-# LINE 523 "./src-ag/PrintCode.ag" #-}

writeModule :: FilePath -> [PP_Doc] -> IO ()
writeModule path docs
  = do bExists <- doesFileExist path
       if bExists
        then do input <- readFile path
                seq (length input) (return ())
                if input /= output
                 then dumpIt
                 else return ()
        else dumpIt
  where
    output = renderDocs docs
    dumpIt = writeFile path output
{-# LINE 117 "dist/build/PrintCode.hs" #-}
-- CaseAlt -----------------------------------------------------
-- wrapper
data Inh_CaseAlt  = Inh_CaseAlt { nested_Inh_CaseAlt :: !(Bool), options_Inh_CaseAlt :: !(Options), outputfile_Inh_CaseAlt :: !(String) }
data Syn_CaseAlt  = Syn_CaseAlt { pps_Syn_CaseAlt :: !(PP_Docs) }
{-# INLINABLE wrap_CaseAlt #-}
wrap_CaseAlt :: T_CaseAlt  -> Inh_CaseAlt  -> (Syn_CaseAlt )
wrap_CaseAlt !(T_CaseAlt act) !(Inh_CaseAlt _lhsInested _lhsIoptions _lhsIoutputfile) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_CaseAlt_vIn1 _lhsInested _lhsIoptions _lhsIoutputfile
        !(T_CaseAlt_vOut1 _lhsOpps) <- return (inv_CaseAlt_s2 sem arg)
        return (Syn_CaseAlt _lhsOpps)
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
data T_CaseAlt_vIn1  = T_CaseAlt_vIn1 (Bool) (Options) (String)
data T_CaseAlt_vOut1  = T_CaseAlt_vOut1 (PP_Docs)
{-# NOINLINE sem_CaseAlt_CaseAlt #-}
sem_CaseAlt_CaseAlt :: T_Lhs  -> T_Expr  -> T_CaseAlt 
sem_CaseAlt_CaseAlt arg_left_ arg_expr_ = T_CaseAlt (return st2) where
   {-# NOINLINE st2 #-}
   !st2 = let
      v1 :: T_CaseAlt_v1 
      v1 = \ !(T_CaseAlt_vIn1 _lhsInested _lhsIoptions _lhsIoutputfile) -> ( let
         _leftX32 = Control.Monad.Identity.runIdentity (attach_T_Lhs (arg_left_))
         _exprX26 = Control.Monad.Identity.runIdentity (attach_T_Expr (arg_expr_))
         (T_Lhs_vOut31 _leftIpp) = inv_Lhs_s32 _leftX32 (T_Lhs_vIn31 _leftOisDeclOfLet _leftOnested _leftOoptions _leftOoutputfile)
         (T_Expr_vOut25 _exprIpp) = inv_Expr_s26 _exprX26 (T_Expr_vIn25 _exprOnested _exprOoptions _exprOoutputfile)
         _lhsOpps :: PP_Docs
         _lhsOpps = rule0 _exprIpp _leftIpp
         _leftOisDeclOfLet = rule1  ()
         _leftOnested = rule2 _lhsInested
         _leftOoptions = rule3 _lhsIoptions
         _leftOoutputfile = rule4 _lhsIoutputfile
         _exprOnested = rule5 _lhsInested
         _exprOoptions = rule6 _lhsIoptions
         _exprOoutputfile = rule7 _lhsIoutputfile
         !__result_ = T_CaseAlt_vOut1 _lhsOpps
         in __result_ )
     in C_CaseAlt_s2 v1
   {-# INLINE rule0 #-}
   {-# LINE 218 "./src-ag/PrintCode.ag" #-}
   rule0 = \ ((_exprIpp) :: PP_Doc) ((_leftIpp) :: PP_Doc) ->
                               {-# LINE 218 "./src-ag/PrintCode.ag" #-}
                               ["{" >#< _leftIpp >#< "->", _exprIpp >#< "}"]
                               {-# LINE 176 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule1 #-}
   {-# LINE 424 "./src-ag/PrintCode.ag" #-}
   rule1 = \  (_ :: ()) ->
                           {-# LINE 424 "./src-ag/PrintCode.ag" #-}
                           False
                           {-# LINE 182 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule2 #-}
   rule2 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule3 #-}
   rule3 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule4 #-}
   rule4 = \ ((_lhsIoutputfile) :: String) ->
     _lhsIoutputfile
   {-# INLINE rule5 #-}
   rule5 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule6 #-}
   rule6 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule7 #-}
   rule7 = \ ((_lhsIoutputfile) :: String) ->
     _lhsIoutputfile

-- CaseAlts ----------------------------------------------------
-- wrapper
data Inh_CaseAlts  = Inh_CaseAlts { nested_Inh_CaseAlts :: !(Bool), options_Inh_CaseAlts :: !(Options), outputfile_Inh_CaseAlts :: !(String) }
data Syn_CaseAlts  = Syn_CaseAlts { pps_Syn_CaseAlts :: !(PP_Docs) }
{-# INLINABLE wrap_CaseAlts #-}
wrap_CaseAlts :: T_CaseAlts  -> Inh_CaseAlts  -> (Syn_CaseAlts )
wrap_CaseAlts !(T_CaseAlts act) !(Inh_CaseAlts _lhsInested _lhsIoptions _lhsIoutputfile) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_CaseAlts_vIn4 _lhsInested _lhsIoptions _lhsIoutputfile
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
data T_CaseAlts_vIn4  = T_CaseAlts_vIn4 (Bool) (Options) (String)
data T_CaseAlts_vOut4  = T_CaseAlts_vOut4 (PP_Docs)
{-# NOINLINE sem_CaseAlts_Cons #-}
sem_CaseAlts_Cons :: T_CaseAlt  -> T_CaseAlts  -> T_CaseAlts 
sem_CaseAlts_Cons arg_hd_ arg_tl_ = T_CaseAlts (return st5) where
   {-# NOINLINE st5 #-}
   !st5 = let
      v4 :: T_CaseAlts_v4 
      v4 = \ !(T_CaseAlts_vIn4 _lhsInested _lhsIoptions _lhsIoutputfile) -> ( let
         _hdX2 = Control.Monad.Identity.runIdentity (attach_T_CaseAlt (arg_hd_))
         _tlX5 = Control.Monad.Identity.runIdentity (attach_T_CaseAlts (arg_tl_))
         (T_CaseAlt_vOut1 _hdIpps) = inv_CaseAlt_s2 _hdX2 (T_CaseAlt_vIn1 _hdOnested _hdOoptions _hdOoutputfile)
         (T_CaseAlts_vOut4 _tlIpps) = inv_CaseAlts_s5 _tlX5 (T_CaseAlts_vIn4 _tlOnested _tlOoptions _tlOoutputfile)
         _lhsOpps :: PP_Docs
         _lhsOpps = rule8 _hdIpps _tlIpps
         _hdOnested = rule9 _lhsInested
         _hdOoptions = rule10 _lhsIoptions
         _hdOoutputfile = rule11 _lhsIoutputfile
         _tlOnested = rule12 _lhsInested
         _tlOoptions = rule13 _lhsIoptions
         _tlOoutputfile = rule14 _lhsIoutputfile
         !__result_ = T_CaseAlts_vOut4 _lhsOpps
         in __result_ )
     in C_CaseAlts_s5 v4
   {-# INLINE rule8 #-}
   {-# LINE 68 "./src-ag/PrintCode.ag" #-}
   rule8 = \ ((_hdIpps) :: PP_Docs) ((_tlIpps) :: PP_Docs) ->
                     {-# LINE 68 "./src-ag/PrintCode.ag" #-}
                     _hdIpps ++ _tlIpps
                     {-# LINE 259 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule9 #-}
   rule9 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule10 #-}
   rule10 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule11 #-}
   rule11 = \ ((_lhsIoutputfile) :: String) ->
     _lhsIoutputfile
   {-# INLINE rule12 #-}
   rule12 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule13 #-}
   rule13 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule14 #-}
   rule14 = \ ((_lhsIoutputfile) :: String) ->
     _lhsIoutputfile
{-# NOINLINE sem_CaseAlts_Nil #-}
sem_CaseAlts_Nil ::  T_CaseAlts 
sem_CaseAlts_Nil  = T_CaseAlts (return st5) where
   {-# NOINLINE st5 #-}
   !st5 = let
      v4 :: T_CaseAlts_v4 
      v4 = \ !(T_CaseAlts_vIn4 _lhsInested _lhsIoptions _lhsIoutputfile) -> ( let
         _lhsOpps :: PP_Docs
         _lhsOpps = rule15  ()
         !__result_ = T_CaseAlts_vOut4 _lhsOpps
         in __result_ )
     in C_CaseAlts_s5 v4
   {-# INLINE rule15 #-}
   {-# LINE 69 "./src-ag/PrintCode.ag" #-}
   rule15 = \  (_ :: ()) ->
                     {-# LINE 69 "./src-ag/PrintCode.ag" #-}
                     []
                     {-# LINE 295 "dist/build/PrintCode.hs"#-}

-- Chunk -------------------------------------------------------
-- wrapper
data Inh_Chunk  = Inh_Chunk { importBlocks_Inh_Chunk :: !(PP_Doc), isDeclOfLet_Inh_Chunk :: !(Bool), mainFile_Inh_Chunk :: !(String), mainName_Inh_Chunk :: !(String), moduleHeader_Inh_Chunk :: !(String -> String -> String -> Bool -> String), nested_Inh_Chunk :: !(Bool), options_Inh_Chunk :: !(Options), optionsLine_Inh_Chunk :: !(String), pragmaBlocks_Inh_Chunk :: !(String), textBlockMap_Inh_Chunk :: !(Map BlockInfo PP_Doc), textBlocks_Inh_Chunk :: !(PP_Doc) }
data Syn_Chunk  = Syn_Chunk { appendCommon_Syn_Chunk :: !([[PP_Doc]]), appendMain_Syn_Chunk :: !([[PP_Doc]]), genSems_Syn_Chunk :: !(IO ()), imports_Syn_Chunk :: !([String]), pps_Syn_Chunk :: !(PP_Docs) }
{-# INLINABLE wrap_Chunk #-}
wrap_Chunk :: T_Chunk  -> Inh_Chunk  -> (Syn_Chunk )
wrap_Chunk !(T_Chunk act) !(Inh_Chunk _lhsIimportBlocks _lhsIisDeclOfLet _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInested _lhsIoptions _lhsIoptionsLine _lhsIpragmaBlocks _lhsItextBlockMap _lhsItextBlocks) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_Chunk_vIn7 _lhsIimportBlocks _lhsIisDeclOfLet _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInested _lhsIoptions _lhsIoptionsLine _lhsIpragmaBlocks _lhsItextBlockMap _lhsItextBlocks
        !(T_Chunk_vOut7 _lhsOappendCommon _lhsOappendMain _lhsOgenSems _lhsOimports _lhsOpps) <- return (inv_Chunk_s8 sem arg)
        return (Syn_Chunk _lhsOappendCommon _lhsOappendMain _lhsOgenSems _lhsOimports _lhsOpps)
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
data T_Chunk_vIn7  = T_Chunk_vIn7 (PP_Doc) (Bool) (String) (String) (String -> String -> String -> Bool -> String) (Bool) (Options) (String) (String) (Map BlockInfo PP_Doc) (PP_Doc)
data T_Chunk_vOut7  = T_Chunk_vOut7 ([[PP_Doc]]) ([[PP_Doc]]) (IO ()) ([String]) (PP_Docs)
{-# NOINLINE sem_Chunk_Chunk #-}
sem_Chunk_Chunk :: (String) -> T_Decl  -> T_Decls  -> T_Decls  -> T_Decls  -> T_Decls  -> T_Decls  -> T_Decls  -> ([String]) -> T_Chunk 
sem_Chunk_Chunk !arg_name_ arg_comment_ arg_info_ arg_dataDef_ arg_cataFun_ arg_semDom_ arg_semWrapper_ arg_semFunctions_ !arg_semNames_ = T_Chunk (return st8) where
   {-# NOINLINE st8 #-}
   !st8 = let
      v7 :: T_Chunk_v7 
      v7 = \ !(T_Chunk_vIn7 _lhsIimportBlocks _lhsIisDeclOfLet _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInested _lhsIoptions _lhsIoptionsLine _lhsIpragmaBlocks _lhsItextBlockMap _lhsItextBlocks) -> ( let
         _commentX20 = Control.Monad.Identity.runIdentity (attach_T_Decl (arg_comment_))
         _infoX23 = Control.Monad.Identity.runIdentity (attach_T_Decls (arg_info_))
         _dataDefX23 = Control.Monad.Identity.runIdentity (attach_T_Decls (arg_dataDef_))
         _cataFunX23 = Control.Monad.Identity.runIdentity (attach_T_Decls (arg_cataFun_))
         _semDomX23 = Control.Monad.Identity.runIdentity (attach_T_Decls (arg_semDom_))
         _semWrapperX23 = Control.Monad.Identity.runIdentity (attach_T_Decls (arg_semWrapper_))
         _semFunctionsX23 = Control.Monad.Identity.runIdentity (attach_T_Decls (arg_semFunctions_))
         (T_Decl_vOut19 _commentIpp) = inv_Decl_s20 _commentX20 (T_Decl_vIn19 _commentOisDeclOfLet _commentOnested _commentOoptions _commentOoutputfile)
         (T_Decls_vOut22 _infoIpps) = inv_Decls_s23 _infoX23 (T_Decls_vIn22 _infoOisDeclOfLet _infoOnested _infoOoptions _infoOoutputfile)
         (T_Decls_vOut22 _dataDefIpps) = inv_Decls_s23 _dataDefX23 (T_Decls_vIn22 _dataDefOisDeclOfLet _dataDefOnested _dataDefOoptions _dataDefOoutputfile)
         (T_Decls_vOut22 _cataFunIpps) = inv_Decls_s23 _cataFunX23 (T_Decls_vIn22 _cataFunOisDeclOfLet _cataFunOnested _cataFunOoptions _cataFunOoutputfile)
         (T_Decls_vOut22 _semDomIpps) = inv_Decls_s23 _semDomX23 (T_Decls_vIn22 _semDomOisDeclOfLet _semDomOnested _semDomOoptions _semDomOoutputfile)
         (T_Decls_vOut22 _semWrapperIpps) = inv_Decls_s23 _semWrapperX23 (T_Decls_vIn22 _semWrapperOisDeclOfLet _semWrapperOnested _semWrapperOoptions _semWrapperOoutputfile)
         (T_Decls_vOut22 _semFunctionsIpps) = inv_Decls_s23 _semFunctionsX23 (T_Decls_vIn22 _semFunctionsOisDeclOfLet _semFunctionsOnested _semFunctionsOoptions _semFunctionsOoutputfile)
         _outputfile = rule16 _lhsImainFile _lhsIoptions arg_name_
         _lhsOpps :: PP_Docs
         _lhsOpps = rule17 _cataFunIpps _commentIpp _dataDefIpps _infoIpps _lhsItextBlockMap _semDomIpps _semFunctionsIpps _semWrapperIpps arg_name_
         _lhsOimports :: [String]
         _lhsOimports = rule18 _lhsImainName arg_name_
         _lhsOappendCommon :: [[PP_Doc]]
         _lhsOappendCommon = rule19 _commentIpp _dataDefIpps _lhsIoptions _semDomIpps _semWrapperIpps
         _lhsOappendMain :: [[PP_Doc]]
         _lhsOappendMain = rule20 _cataFunIpps _commentIpp _lhsIoptions _semWrapperIpps
         _lhsOgenSems :: IO ()
         _lhsOgenSems = rule21 _commentIpp _exports _infoIpps _lhsImainName _lhsImoduleHeader _lhsIoptionsLine _lhsIpragmaBlocks _lhsItextBlockMap _outputfile _semFunctionsIpps arg_name_
         _exports = rule22 arg_semNames_
         _commentOisDeclOfLet = rule23 _lhsIisDeclOfLet
         _commentOnested = rule24 _lhsInested
         _commentOoptions = rule25 _lhsIoptions
         _commentOoutputfile = rule26 _outputfile
         _infoOisDeclOfLet = rule27 _lhsIisDeclOfLet
         _infoOnested = rule28 _lhsInested
         _infoOoptions = rule29 _lhsIoptions
         _infoOoutputfile = rule30 _outputfile
         _dataDefOisDeclOfLet = rule31 _lhsIisDeclOfLet
         _dataDefOnested = rule32 _lhsInested
         _dataDefOoptions = rule33 _lhsIoptions
         _dataDefOoutputfile = rule34 _outputfile
         _cataFunOisDeclOfLet = rule35 _lhsIisDeclOfLet
         _cataFunOnested = rule36 _lhsInested
         _cataFunOoptions = rule37 _lhsIoptions
         _cataFunOoutputfile = rule38 _outputfile
         _semDomOisDeclOfLet = rule39 _lhsIisDeclOfLet
         _semDomOnested = rule40 _lhsInested
         _semDomOoptions = rule41 _lhsIoptions
         _semDomOoutputfile = rule42 _outputfile
         _semWrapperOisDeclOfLet = rule43 _lhsIisDeclOfLet
         _semWrapperOnested = rule44 _lhsInested
         _semWrapperOoptions = rule45 _lhsIoptions
         _semWrapperOoutputfile = rule46 _outputfile
         _semFunctionsOisDeclOfLet = rule47 _lhsIisDeclOfLet
         _semFunctionsOnested = rule48 _lhsInested
         _semFunctionsOoptions = rule49 _lhsIoptions
         _semFunctionsOoutputfile = rule50 _outputfile
         !__result_ = T_Chunk_vOut7 _lhsOappendCommon _lhsOappendMain _lhsOgenSems _lhsOimports _lhsOpps
         in __result_ )
     in C_Chunk_s8 v7
   {-# INLINE rule16 #-}
   {-# LINE 43 "./src-ag/PrintCode.ag" #-}
   rule16 = \ ((_lhsImainFile) :: String) ((_lhsIoptions) :: Options) name_ ->
                         {-# LINE 43 "./src-ag/PrintCode.ag" #-}
                         if sepSemMods _lhsIoptions
                         then replaceBaseName _lhsImainFile (takeBaseName _lhsImainFile ++ "_" ++ name_)
                         else _lhsImainFile
                         {-# LINE 398 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule17 #-}
   {-# LINE 96 "./src-ag/PrintCode.ag" #-}
   rule17 = \ ((_cataFunIpps) :: PP_Docs) ((_commentIpp) :: PP_Doc) ((_dataDefIpps) :: PP_Docs) ((_infoIpps) :: PP_Docs) ((_lhsItextBlockMap) :: Map BlockInfo PP_Doc) ((_semDomIpps) :: PP_Docs) ((_semFunctionsIpps) :: PP_Docs) ((_semWrapperIpps) :: PP_Docs) name_ ->
                                {-# LINE 96 "./src-ag/PrintCode.ag" #-}
                                _commentIpp
                                :  _infoIpps
                                ++ _dataDefIpps
                                ++ _cataFunIpps
                                ++ _semDomIpps
                                ++ _semWrapperIpps
                                ++ _semFunctionsIpps
                                ++ [Map.findWithDefault empty (BlockOther, Just $ identifier name_) _lhsItextBlockMap]
                                {-# LINE 411 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule18 #-}
   {-# LINE 483 "./src-ag/PrintCode.ag" #-}
   rule18 = \ ((_lhsImainName) :: String) name_ ->
                      {-# LINE 483 "./src-ag/PrintCode.ag" #-}
                      ["import " ++ _lhsImainName ++ "_" ++ name_ ++ "\n"]
                      {-# LINE 417 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule19 #-}
   {-# LINE 490 "./src-ag/PrintCode.ag" #-}
   rule19 = \ ((_commentIpp) :: PP_Doc) ((_dataDefIpps) :: PP_Docs) ((_lhsIoptions) :: Options) ((_semDomIpps) :: PP_Docs) ((_semWrapperIpps) :: PP_Docs) ->
            {-# LINE 490 "./src-ag/PrintCode.ag" #-}
            [ [_commentIpp]
            , _dataDefIpps
            , _semDomIpps
            , if reference _lhsIoptions then _semWrapperIpps else []
            ]
            {-# LINE 427 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule20 #-}
   {-# LINE 496 "./src-ag/PrintCode.ag" #-}
   rule20 = \ ((_cataFunIpps) :: PP_Docs) ((_commentIpp) :: PP_Doc) ((_lhsIoptions) :: Options) ((_semWrapperIpps) :: PP_Docs) ->
            {-# LINE 496 "./src-ag/PrintCode.ag" #-}
            [ [_commentIpp]
            , _cataFunIpps
            , if reference _lhsIoptions then [] else _semWrapperIpps
            ]
            {-# LINE 436 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule21 #-}
   {-# LINE 506 "./src-ag/PrintCode.ag" #-}
   rule21 = \ ((_commentIpp) :: PP_Doc) _exports ((_infoIpps) :: PP_Docs) ((_lhsImainName) :: String) ((_lhsImoduleHeader) :: String -> String -> String -> Bool -> String) ((_lhsIoptionsLine) :: String) ((_lhsIpragmaBlocks) :: String) ((_lhsItextBlockMap) :: Map BlockInfo PP_Doc) _outputfile ((_semFunctionsIpps) :: PP_Docs) name_ ->
            {-# LINE 506 "./src-ag/PrintCode.ag" #-}
            writeModule _outputfile
              [ pp $ _lhsIpragmaBlocks
              , pp $ Map.findWithDefault empty (BlockPragma, Just $ identifier name_) _lhsItextBlockMap
              , pp $ _lhsIoptionsLine
              , pp $ _lhsImoduleHeader _lhsImainName ("_" ++ name_) _exports     True
              , pp $ ("import " ++ _lhsImainName ++ "_common\n")
              , pp $ Map.findWithDefault empty (BlockImport, Just $ identifier name_) _lhsItextBlockMap
              , _commentIpp
              , vlist_sep "" _infoIpps
              , vlist_sep "" _semFunctionsIpps
              , Map.findWithDefault empty (BlockOther, Just $ identifier name_) _lhsItextBlockMap
              ]
            {-# LINE 453 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule22 #-}
   {-# LINE 521 "./src-ag/PrintCode.ag" #-}
   rule22 = \ semNames_ ->
                      {-# LINE 521 "./src-ag/PrintCode.ag" #-}
                      concat $ intersperse "," semNames_
                      {-# LINE 459 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule23 #-}
   rule23 = \ ((_lhsIisDeclOfLet) :: Bool) ->
     _lhsIisDeclOfLet
   {-# INLINE rule24 #-}
   rule24 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule25 #-}
   rule25 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule26 #-}
   rule26 = \ _outputfile ->
     _outputfile
   {-# INLINE rule27 #-}
   rule27 = \ ((_lhsIisDeclOfLet) :: Bool) ->
     _lhsIisDeclOfLet
   {-# INLINE rule28 #-}
   rule28 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule29 #-}
   rule29 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule30 #-}
   rule30 = \ _outputfile ->
     _outputfile
   {-# INLINE rule31 #-}
   rule31 = \ ((_lhsIisDeclOfLet) :: Bool) ->
     _lhsIisDeclOfLet
   {-# INLINE rule32 #-}
   rule32 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule33 #-}
   rule33 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule34 #-}
   rule34 = \ _outputfile ->
     _outputfile
   {-# INLINE rule35 #-}
   rule35 = \ ((_lhsIisDeclOfLet) :: Bool) ->
     _lhsIisDeclOfLet
   {-# INLINE rule36 #-}
   rule36 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule37 #-}
   rule37 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule38 #-}
   rule38 = \ _outputfile ->
     _outputfile
   {-# INLINE rule39 #-}
   rule39 = \ ((_lhsIisDeclOfLet) :: Bool) ->
     _lhsIisDeclOfLet
   {-# INLINE rule40 #-}
   rule40 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule41 #-}
   rule41 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule42 #-}
   rule42 = \ _outputfile ->
     _outputfile
   {-# INLINE rule43 #-}
   rule43 = \ ((_lhsIisDeclOfLet) :: Bool) ->
     _lhsIisDeclOfLet
   {-# INLINE rule44 #-}
   rule44 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule45 #-}
   rule45 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule46 #-}
   rule46 = \ _outputfile ->
     _outputfile
   {-# INLINE rule47 #-}
   rule47 = \ ((_lhsIisDeclOfLet) :: Bool) ->
     _lhsIisDeclOfLet
   {-# INLINE rule48 #-}
   rule48 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule49 #-}
   rule49 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule50 #-}
   rule50 = \ _outputfile ->
     _outputfile

-- Chunks ------------------------------------------------------
-- wrapper
data Inh_Chunks  = Inh_Chunks { importBlocks_Inh_Chunks :: !(PP_Doc), isDeclOfLet_Inh_Chunks :: !(Bool), mainFile_Inh_Chunks :: !(String), mainName_Inh_Chunks :: !(String), moduleHeader_Inh_Chunks :: !(String -> String -> String -> Bool -> String), nested_Inh_Chunks :: !(Bool), options_Inh_Chunks :: !(Options), optionsLine_Inh_Chunks :: !(String), pragmaBlocks_Inh_Chunks :: !(String), textBlockMap_Inh_Chunks :: !(Map BlockInfo PP_Doc), textBlocks_Inh_Chunks :: !(PP_Doc) }
data Syn_Chunks  = Syn_Chunks { appendCommon_Syn_Chunks :: !([[PP_Doc]]), appendMain_Syn_Chunks :: !([[PP_Doc]]), genSems_Syn_Chunks :: !(IO ()), imports_Syn_Chunks :: !([String]), pps_Syn_Chunks :: !(PP_Docs) }
{-# INLINABLE wrap_Chunks #-}
wrap_Chunks :: T_Chunks  -> Inh_Chunks  -> (Syn_Chunks )
wrap_Chunks !(T_Chunks act) !(Inh_Chunks _lhsIimportBlocks _lhsIisDeclOfLet _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInested _lhsIoptions _lhsIoptionsLine _lhsIpragmaBlocks _lhsItextBlockMap _lhsItextBlocks) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_Chunks_vIn10 _lhsIimportBlocks _lhsIisDeclOfLet _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInested _lhsIoptions _lhsIoptionsLine _lhsIpragmaBlocks _lhsItextBlockMap _lhsItextBlocks
        !(T_Chunks_vOut10 _lhsOappendCommon _lhsOappendMain _lhsOgenSems _lhsOimports _lhsOpps) <- return (inv_Chunks_s11 sem arg)
        return (Syn_Chunks _lhsOappendCommon _lhsOappendMain _lhsOgenSems _lhsOimports _lhsOpps)
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
data T_Chunks_vIn10  = T_Chunks_vIn10 (PP_Doc) (Bool) (String) (String) (String -> String -> String -> Bool -> String) (Bool) (Options) (String) (String) (Map BlockInfo PP_Doc) (PP_Doc)
data T_Chunks_vOut10  = T_Chunks_vOut10 ([[PP_Doc]]) ([[PP_Doc]]) (IO ()) ([String]) (PP_Docs)
{-# NOINLINE sem_Chunks_Cons #-}
sem_Chunks_Cons :: T_Chunk  -> T_Chunks  -> T_Chunks 
sem_Chunks_Cons arg_hd_ arg_tl_ = T_Chunks (return st11) where
   {-# NOINLINE st11 #-}
   !st11 = let
      v10 :: T_Chunks_v10 
      v10 = \ !(T_Chunks_vIn10 _lhsIimportBlocks _lhsIisDeclOfLet _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInested _lhsIoptions _lhsIoptionsLine _lhsIpragmaBlocks _lhsItextBlockMap _lhsItextBlocks) -> ( let
         _hdX8 = Control.Monad.Identity.runIdentity (attach_T_Chunk (arg_hd_))
         _tlX11 = Control.Monad.Identity.runIdentity (attach_T_Chunks (arg_tl_))
         (T_Chunk_vOut7 _hdIappendCommon _hdIappendMain _hdIgenSems _hdIimports _hdIpps) = inv_Chunk_s8 _hdX8 (T_Chunk_vIn7 _hdOimportBlocks _hdOisDeclOfLet _hdOmainFile _hdOmainName _hdOmoduleHeader _hdOnested _hdOoptions _hdOoptionsLine _hdOpragmaBlocks _hdOtextBlockMap _hdOtextBlocks)
         (T_Chunks_vOut10 _tlIappendCommon _tlIappendMain _tlIgenSems _tlIimports _tlIpps) = inv_Chunks_s11 _tlX11 (T_Chunks_vIn10 _tlOimportBlocks _tlOisDeclOfLet _tlOmainFile _tlOmainName _tlOmoduleHeader _tlOnested _tlOoptions _tlOoptionsLine _tlOpragmaBlocks _tlOtextBlockMap _tlOtextBlocks)
         _lhsOpps :: PP_Docs
         _lhsOpps = rule51 _hdIpps _tlIpps
         _lhsOappendCommon :: [[PP_Doc]]
         _lhsOappendCommon = rule52 _hdIappendCommon _tlIappendCommon
         _lhsOappendMain :: [[PP_Doc]]
         _lhsOappendMain = rule53 _hdIappendMain _tlIappendMain
         _lhsOgenSems :: IO ()
         _lhsOgenSems = rule54 _hdIgenSems _tlIgenSems
         _lhsOimports :: [String]
         _lhsOimports = rule55 _hdIimports _tlIimports
         _hdOimportBlocks = rule56 _lhsIimportBlocks
         _hdOisDeclOfLet = rule57 _lhsIisDeclOfLet
         _hdOmainFile = rule58 _lhsImainFile
         _hdOmainName = rule59 _lhsImainName
         _hdOmoduleHeader = rule60 _lhsImoduleHeader
         _hdOnested = rule61 _lhsInested
         _hdOoptions = rule62 _lhsIoptions
         _hdOoptionsLine = rule63 _lhsIoptionsLine
         _hdOpragmaBlocks = rule64 _lhsIpragmaBlocks
         _hdOtextBlockMap = rule65 _lhsItextBlockMap
         _hdOtextBlocks = rule66 _lhsItextBlocks
         _tlOimportBlocks = rule67 _lhsIimportBlocks
         _tlOisDeclOfLet = rule68 _lhsIisDeclOfLet
         _tlOmainFile = rule69 _lhsImainFile
         _tlOmainName = rule70 _lhsImainName
         _tlOmoduleHeader = rule71 _lhsImoduleHeader
         _tlOnested = rule72 _lhsInested
         _tlOoptions = rule73 _lhsIoptions
         _tlOoptionsLine = rule74 _lhsIoptionsLine
         _tlOpragmaBlocks = rule75 _lhsIpragmaBlocks
         _tlOtextBlockMap = rule76 _lhsItextBlockMap
         _tlOtextBlocks = rule77 _lhsItextBlocks
         !__result_ = T_Chunks_vOut10 _lhsOappendCommon _lhsOappendMain _lhsOgenSems _lhsOimports _lhsOpps
         in __result_ )
     in C_Chunks_s11 v10
   {-# INLINE rule51 #-}
   {-# LINE 88 "./src-ag/PrintCode.ag" #-}
   rule51 = \ ((_hdIpps) :: PP_Docs) ((_tlIpps) :: PP_Docs) ->
                     {-# LINE 88 "./src-ag/PrintCode.ag" #-}
                     _hdIpps ++ _tlIpps
                     {-# LINE 626 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule52 #-}
   rule52 = \ ((_hdIappendCommon) :: [[PP_Doc]]) ((_tlIappendCommon) :: [[PP_Doc]]) ->
     _hdIappendCommon ++ _tlIappendCommon
   {-# INLINE rule53 #-}
   rule53 = \ ((_hdIappendMain) :: [[PP_Doc]]) ((_tlIappendMain) :: [[PP_Doc]]) ->
     _hdIappendMain ++ _tlIappendMain
   {-# INLINE rule54 #-}
   rule54 = \ ((_hdIgenSems) :: IO ()) ((_tlIgenSems) :: IO ()) ->
     _hdIgenSems >> _tlIgenSems
   {-# INLINE rule55 #-}
   rule55 = \ ((_hdIimports) :: [String]) ((_tlIimports) :: [String]) ->
     _hdIimports ++ _tlIimports
   {-# INLINE rule56 #-}
   rule56 = \ ((_lhsIimportBlocks) :: PP_Doc) ->
     _lhsIimportBlocks
   {-# INLINE rule57 #-}
   rule57 = \ ((_lhsIisDeclOfLet) :: Bool) ->
     _lhsIisDeclOfLet
   {-# INLINE rule58 #-}
   rule58 = \ ((_lhsImainFile) :: String) ->
     _lhsImainFile
   {-# INLINE rule59 #-}
   rule59 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule60 #-}
   rule60 = \ ((_lhsImoduleHeader) :: String -> String -> String -> Bool -> String) ->
     _lhsImoduleHeader
   {-# INLINE rule61 #-}
   rule61 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule62 #-}
   rule62 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule63 #-}
   rule63 = \ ((_lhsIoptionsLine) :: String) ->
     _lhsIoptionsLine
   {-# INLINE rule64 #-}
   rule64 = \ ((_lhsIpragmaBlocks) :: String) ->
     _lhsIpragmaBlocks
   {-# INLINE rule65 #-}
   rule65 = \ ((_lhsItextBlockMap) :: Map BlockInfo PP_Doc) ->
     _lhsItextBlockMap
   {-# INLINE rule66 #-}
   rule66 = \ ((_lhsItextBlocks) :: PP_Doc) ->
     _lhsItextBlocks
   {-# INLINE rule67 #-}
   rule67 = \ ((_lhsIimportBlocks) :: PP_Doc) ->
     _lhsIimportBlocks
   {-# INLINE rule68 #-}
   rule68 = \ ((_lhsIisDeclOfLet) :: Bool) ->
     _lhsIisDeclOfLet
   {-# INLINE rule69 #-}
   rule69 = \ ((_lhsImainFile) :: String) ->
     _lhsImainFile
   {-# INLINE rule70 #-}
   rule70 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule71 #-}
   rule71 = \ ((_lhsImoduleHeader) :: String -> String -> String -> Bool -> String) ->
     _lhsImoduleHeader
   {-# INLINE rule72 #-}
   rule72 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule73 #-}
   rule73 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule74 #-}
   rule74 = \ ((_lhsIoptionsLine) :: String) ->
     _lhsIoptionsLine
   {-# INLINE rule75 #-}
   rule75 = \ ((_lhsIpragmaBlocks) :: String) ->
     _lhsIpragmaBlocks
   {-# INLINE rule76 #-}
   rule76 = \ ((_lhsItextBlockMap) :: Map BlockInfo PP_Doc) ->
     _lhsItextBlockMap
   {-# INLINE rule77 #-}
   rule77 = \ ((_lhsItextBlocks) :: PP_Doc) ->
     _lhsItextBlocks
{-# NOINLINE sem_Chunks_Nil #-}
sem_Chunks_Nil ::  T_Chunks 
sem_Chunks_Nil  = T_Chunks (return st11) where
   {-# NOINLINE st11 #-}
   !st11 = let
      v10 :: T_Chunks_v10 
      v10 = \ !(T_Chunks_vIn10 _lhsIimportBlocks _lhsIisDeclOfLet _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInested _lhsIoptions _lhsIoptionsLine _lhsIpragmaBlocks _lhsItextBlockMap _lhsItextBlocks) -> ( let
         _lhsOpps :: PP_Docs
         _lhsOpps = rule78  ()
         _lhsOappendCommon :: [[PP_Doc]]
         _lhsOappendCommon = rule79  ()
         _lhsOappendMain :: [[PP_Doc]]
         _lhsOappendMain = rule80  ()
         _lhsOgenSems :: IO ()
         _lhsOgenSems = rule81  ()
         _lhsOimports :: [String]
         _lhsOimports = rule82  ()
         !__result_ = T_Chunks_vOut10 _lhsOappendCommon _lhsOappendMain _lhsOgenSems _lhsOimports _lhsOpps
         in __result_ )
     in C_Chunks_s11 v10
   {-# INLINE rule78 #-}
   {-# LINE 89 "./src-ag/PrintCode.ag" #-}
   rule78 = \  (_ :: ()) ->
                     {-# LINE 89 "./src-ag/PrintCode.ag" #-}
                     []
                     {-# LINE 730 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule79 #-}
   rule79 = \  (_ :: ()) ->
     []
   {-# INLINE rule80 #-}
   rule80 = \  (_ :: ()) ->
     []
   {-# INLINE rule81 #-}
   rule81 = \  (_ :: ()) ->
     return ()
   {-# INLINE rule82 #-}
   rule82 = \  (_ :: ()) ->
     []

-- DataAlt -----------------------------------------------------
-- wrapper
data Inh_DataAlt  = Inh_DataAlt { nested_Inh_DataAlt :: !(Bool), strictPre_Inh_DataAlt :: !(PP_Doc) }
data Syn_DataAlt  = Syn_DataAlt { pp_Syn_DataAlt :: !(PP_Doc) }
{-# INLINABLE wrap_DataAlt #-}
wrap_DataAlt :: T_DataAlt  -> Inh_DataAlt  -> (Syn_DataAlt )
wrap_DataAlt !(T_DataAlt act) !(Inh_DataAlt _lhsInested _lhsIstrictPre) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_DataAlt_vIn13 _lhsInested _lhsIstrictPre
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
data T_DataAlt_vIn13  = T_DataAlt_vIn13 (Bool) (PP_Doc)
data T_DataAlt_vOut13  = T_DataAlt_vOut13 (PP_Doc)
{-# NOINLINE sem_DataAlt_DataAlt #-}
sem_DataAlt_DataAlt :: (String) -> T_Types  -> T_DataAlt 
sem_DataAlt_DataAlt !arg_name_ arg_args_ = T_DataAlt (return st14) where
   {-# NOINLINE st14 #-}
   !st14 = let
      v13 :: T_DataAlt_v13 
      v13 = \ !(T_DataAlt_vIn13 _lhsInested _lhsIstrictPre) -> ( let
         _argsX53 = Control.Monad.Identity.runIdentity (attach_T_Types (arg_args_))
         (T_Types_vOut52 _argsIpps) = inv_Types_s53 _argsX53 (T_Types_vIn52 _argsOnested)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule83 _argsIpps _lhsIstrictPre arg_name_
         _argsOnested = rule84 _lhsInested
         !__result_ = T_DataAlt_vOut13 _lhsOpp
         in __result_ )
     in C_DataAlt_s14 v13
   {-# INLINE rule83 #-}
   {-# LINE 221 "./src-ag/PrintCode.ag" #-}
   rule83 = \ ((_argsIpps) :: PP_Docs) ((_lhsIstrictPre) :: PP_Doc) name_ ->
                               {-# LINE 221 "./src-ag/PrintCode.ag" #-}
                               name_ >#< hv_sp (map ((_lhsIstrictPre >|<) . pp_parens) _argsIpps)
                               {-# LINE 795 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule84 #-}
   rule84 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
{-# NOINLINE sem_DataAlt_Record #-}
sem_DataAlt_Record :: (String) -> T_NamedTypes  -> T_DataAlt 
sem_DataAlt_Record !arg_name_ arg_args_ = T_DataAlt (return st14) where
   {-# NOINLINE st14 #-}
   !st14 = let
      v13 :: T_DataAlt_v13 
      v13 = \ !(T_DataAlt_vIn13 _lhsInested _lhsIstrictPre) -> ( let
         _argsX38 = Control.Monad.Identity.runIdentity (attach_T_NamedTypes (arg_args_))
         (T_NamedTypes_vOut37 _argsIpps) = inv_NamedTypes_s38 _argsX38 (T_NamedTypes_vIn37 _argsOnested)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule85 _argsIpps arg_name_
         _argsOnested = rule86 _lhsInested
         !__result_ = T_DataAlt_vOut13 _lhsOpp
         in __result_ )
     in C_DataAlt_s14 v13
   {-# INLINE rule85 #-}
   {-# LINE 222 "./src-ag/PrintCode.ag" #-}
   rule85 = \ ((_argsIpps) :: PP_Docs) name_ ->
                               {-# LINE 222 "./src-ag/PrintCode.ag" #-}
                               name_ >#< pp_block "{" "}" "," _argsIpps
                               {-# LINE 819 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule86 #-}
   rule86 = \ ((_lhsInested) :: Bool) ->
     _lhsInested

-- DataAlts ----------------------------------------------------
-- wrapper
data Inh_DataAlts  = Inh_DataAlts { nested_Inh_DataAlts :: !(Bool), strictPre_Inh_DataAlts :: !(PP_Doc) }
data Syn_DataAlts  = Syn_DataAlts { pps_Syn_DataAlts :: !(PP_Docs) }
{-# INLINABLE wrap_DataAlts #-}
wrap_DataAlts :: T_DataAlts  -> Inh_DataAlts  -> (Syn_DataAlts )
wrap_DataAlts !(T_DataAlts act) !(Inh_DataAlts _lhsInested _lhsIstrictPre) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_DataAlts_vIn16 _lhsInested _lhsIstrictPre
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
data T_DataAlts_vIn16  = T_DataAlts_vIn16 (Bool) (PP_Doc)
data T_DataAlts_vOut16  = T_DataAlts_vOut16 (PP_Docs)
{-# NOINLINE sem_DataAlts_Cons #-}
sem_DataAlts_Cons :: T_DataAlt  -> T_DataAlts  -> T_DataAlts 
sem_DataAlts_Cons arg_hd_ arg_tl_ = T_DataAlts (return st17) where
   {-# NOINLINE st17 #-}
   !st17 = let
      v16 :: T_DataAlts_v16 
      v16 = \ !(T_DataAlts_vIn16 _lhsInested _lhsIstrictPre) -> ( let
         _hdX14 = Control.Monad.Identity.runIdentity (attach_T_DataAlt (arg_hd_))
         _tlX17 = Control.Monad.Identity.runIdentity (attach_T_DataAlts (arg_tl_))
         (T_DataAlt_vOut13 _hdIpp) = inv_DataAlt_s14 _hdX14 (T_DataAlt_vIn13 _hdOnested _hdOstrictPre)
         (T_DataAlts_vOut16 _tlIpps) = inv_DataAlts_s17 _tlX17 (T_DataAlts_vIn16 _tlOnested _tlOstrictPre)
         _lhsOpps :: PP_Docs
         _lhsOpps = rule87 _hdIpp _tlIpps
         _hdOnested = rule88 _lhsInested
         _hdOstrictPre = rule89 _lhsIstrictPre
         _tlOnested = rule90 _lhsInested
         _tlOstrictPre = rule91 _lhsIstrictPre
         !__result_ = T_DataAlts_vOut16 _lhsOpps
         in __result_ )
     in C_DataAlts_s17 v16
   {-# INLINE rule87 #-}
   {-# LINE 72 "./src-ag/PrintCode.ag" #-}
   rule87 = \ ((_hdIpp) :: PP_Doc) ((_tlIpps) :: PP_Docs) ->
                     {-# LINE 72 "./src-ag/PrintCode.ag" #-}
                     _hdIpp : _tlIpps
                     {-# LINE 879 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule88 #-}
   rule88 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule89 #-}
   rule89 = \ ((_lhsIstrictPre) :: PP_Doc) ->
     _lhsIstrictPre
   {-# INLINE rule90 #-}
   rule90 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule91 #-}
   rule91 = \ ((_lhsIstrictPre) :: PP_Doc) ->
     _lhsIstrictPre
{-# NOINLINE sem_DataAlts_Nil #-}
sem_DataAlts_Nil ::  T_DataAlts 
sem_DataAlts_Nil  = T_DataAlts (return st17) where
   {-# NOINLINE st17 #-}
   !st17 = let
      v16 :: T_DataAlts_v16 
      v16 = \ !(T_DataAlts_vIn16 _lhsInested _lhsIstrictPre) -> ( let
         _lhsOpps :: PP_Docs
         _lhsOpps = rule92  ()
         !__result_ = T_DataAlts_vOut16 _lhsOpps
         in __result_ )
     in C_DataAlts_s17 v16
   {-# INLINE rule92 #-}
   {-# LINE 73 "./src-ag/PrintCode.ag" #-}
   rule92 = \  (_ :: ()) ->
                     {-# LINE 73 "./src-ag/PrintCode.ag" #-}
                     []
                     {-# LINE 909 "dist/build/PrintCode.hs"#-}

-- Decl --------------------------------------------------------
-- wrapper
data Inh_Decl  = Inh_Decl { isDeclOfLet_Inh_Decl :: !(Bool), nested_Inh_Decl :: !(Bool), options_Inh_Decl :: !(Options), outputfile_Inh_Decl :: !(String) }
data Syn_Decl  = Syn_Decl { pp_Syn_Decl :: !(PP_Doc) }
{-# INLINABLE wrap_Decl #-}
wrap_Decl :: T_Decl  -> Inh_Decl  -> (Syn_Decl )
wrap_Decl !(T_Decl act) !(Inh_Decl _lhsIisDeclOfLet _lhsInested _lhsIoptions _lhsIoutputfile) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_Decl_vIn19 _lhsIisDeclOfLet _lhsInested _lhsIoptions _lhsIoutputfile
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
data T_Decl_vIn19  = T_Decl_vIn19 (Bool) (Bool) (Options) (String)
data T_Decl_vOut19  = T_Decl_vOut19 (PP_Doc)
{-# NOINLINE sem_Decl_Decl #-}
sem_Decl_Decl :: T_Lhs  -> T_Expr  -> (Set String) -> (Set String) -> T_Decl 
sem_Decl_Decl arg_left_ arg_rhs_ _ _ = T_Decl (return st20) where
   {-# NOINLINE st20 #-}
   !st20 = let
      v19 :: T_Decl_v19 
      v19 = \ !(T_Decl_vIn19 _lhsIisDeclOfLet _lhsInested _lhsIoptions _lhsIoutputfile) -> ( let
         _leftX32 = Control.Monad.Identity.runIdentity (attach_T_Lhs (arg_left_))
         _rhsX26 = Control.Monad.Identity.runIdentity (attach_T_Expr (arg_rhs_))
         (T_Lhs_vOut31 _leftIpp) = inv_Lhs_s32 _leftX32 (T_Lhs_vIn31 _leftOisDeclOfLet _leftOnested _leftOoptions _leftOoutputfile)
         (T_Expr_vOut25 _rhsIpp) = inv_Expr_s26 _rhsX26 (T_Expr_vIn25 _rhsOnested _rhsOoptions _rhsOoutputfile)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule93 _leftIpp _rhsIpp
         _leftOisDeclOfLet = rule94 _lhsIisDeclOfLet
         _leftOnested = rule95 _lhsInested
         _leftOoptions = rule96 _lhsIoptions
         _leftOoutputfile = rule97 _lhsIoutputfile
         _rhsOnested = rule98 _lhsInested
         _rhsOoptions = rule99 _lhsIoptions
         _rhsOoutputfile = rule100 _lhsIoutputfile
         !__result_ = T_Decl_vOut19 _lhsOpp
         in __result_ )
     in C_Decl_s20 v19
   {-# INLINE rule93 #-}
   {-# LINE 106 "./src-ag/PrintCode.ag" #-}
   rule93 = \ ((_leftIpp) :: PP_Doc) ((_rhsIpp) :: PP_Doc) ->
                               {-# LINE 106 "./src-ag/PrintCode.ag" #-}
                               _leftIpp >#< "="
                               >-< indent 4 _rhsIpp
                               {-# LINE 980 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule94 #-}
   rule94 = \ ((_lhsIisDeclOfLet) :: Bool) ->
     _lhsIisDeclOfLet
   {-# INLINE rule95 #-}
   rule95 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule96 #-}
   rule96 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule97 #-}
   rule97 = \ ((_lhsIoutputfile) :: String) ->
     _lhsIoutputfile
   {-# INLINE rule98 #-}
   rule98 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule99 #-}
   rule99 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule100 #-}
   rule100 = \ ((_lhsIoutputfile) :: String) ->
     _lhsIoutputfile
{-# NOINLINE sem_Decl_Bind #-}
sem_Decl_Bind :: T_Lhs  -> T_Expr  -> T_Decl 
sem_Decl_Bind arg_left_ arg_rhs_ = T_Decl (return st20) where
   {-# NOINLINE st20 #-}
   !st20 = let
      v19 :: T_Decl_v19 
      v19 = \ !(T_Decl_vIn19 _lhsIisDeclOfLet _lhsInested _lhsIoptions _lhsIoutputfile) -> ( let
         _leftX32 = Control.Monad.Identity.runIdentity (attach_T_Lhs (arg_left_))
         _rhsX26 = Control.Monad.Identity.runIdentity (attach_T_Expr (arg_rhs_))
         (T_Lhs_vOut31 _leftIpp) = inv_Lhs_s32 _leftX32 (T_Lhs_vIn31 _leftOisDeclOfLet _leftOnested _leftOoptions _leftOoutputfile)
         (T_Expr_vOut25 _rhsIpp) = inv_Expr_s26 _rhsX26 (T_Expr_vIn25 _rhsOnested _rhsOoptions _rhsOoutputfile)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule101 _leftIpp _rhsIpp
         _leftOisDeclOfLet = rule102 _lhsIisDeclOfLet
         _leftOnested = rule103 _lhsInested
         _leftOoptions = rule104 _lhsIoptions
         _leftOoutputfile = rule105 _lhsIoutputfile
         _rhsOnested = rule106 _lhsInested
         _rhsOoptions = rule107 _lhsIoptions
         _rhsOoutputfile = rule108 _lhsIoutputfile
         !__result_ = T_Decl_vOut19 _lhsOpp
         in __result_ )
     in C_Decl_s20 v19
   {-# INLINE rule101 #-}
   {-# LINE 108 "./src-ag/PrintCode.ag" #-}
   rule101 = \ ((_leftIpp) :: PP_Doc) ((_rhsIpp) :: PP_Doc) ->
                               {-# LINE 108 "./src-ag/PrintCode.ag" #-}
                               _leftIpp >#< "<-" >#< _rhsIpp
                               {-# LINE 1030 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule102 #-}
   rule102 = \ ((_lhsIisDeclOfLet) :: Bool) ->
     _lhsIisDeclOfLet
   {-# INLINE rule103 #-}
   rule103 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule104 #-}
   rule104 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule105 #-}
   rule105 = \ ((_lhsIoutputfile) :: String) ->
     _lhsIoutputfile
   {-# INLINE rule106 #-}
   rule106 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule107 #-}
   rule107 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule108 #-}
   rule108 = \ ((_lhsIoutputfile) :: String) ->
     _lhsIoutputfile
{-# NOINLINE sem_Decl_BindLet #-}
sem_Decl_BindLet :: T_Lhs  -> T_Expr  -> T_Decl 
sem_Decl_BindLet arg_left_ arg_rhs_ = T_Decl (return st20) where
   {-# NOINLINE st20 #-}
   !st20 = let
      v19 :: T_Decl_v19 
      v19 = \ !(T_Decl_vIn19 _lhsIisDeclOfLet _lhsInested _lhsIoptions _lhsIoutputfile) -> ( let
         _leftX32 = Control.Monad.Identity.runIdentity (attach_T_Lhs (arg_left_))
         _rhsX26 = Control.Monad.Identity.runIdentity (attach_T_Expr (arg_rhs_))
         (T_Lhs_vOut31 _leftIpp) = inv_Lhs_s32 _leftX32 (T_Lhs_vIn31 _leftOisDeclOfLet _leftOnested _leftOoptions _leftOoutputfile)
         (T_Expr_vOut25 _rhsIpp) = inv_Expr_s26 _rhsX26 (T_Expr_vIn25 _rhsOnested _rhsOoptions _rhsOoutputfile)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule109 _leftIpp _rhsIpp
         _leftOisDeclOfLet = rule110 _lhsIisDeclOfLet
         _leftOnested = rule111 _lhsInested
         _leftOoptions = rule112 _lhsIoptions
         _leftOoutputfile = rule113 _lhsIoutputfile
         _rhsOnested = rule114 _lhsInested
         _rhsOoptions = rule115 _lhsIoptions
         _rhsOoutputfile = rule116 _lhsIoutputfile
         !__result_ = T_Decl_vOut19 _lhsOpp
         in __result_ )
     in C_Decl_s20 v19
   {-# INLINE rule109 #-}
   {-# LINE 109 "./src-ag/PrintCode.ag" #-}
   rule109 = \ ((_leftIpp) :: PP_Doc) ((_rhsIpp) :: PP_Doc) ->
                               {-# LINE 109 "./src-ag/PrintCode.ag" #-}
                               "let" >#< _leftIpp >#< "=" >#< _rhsIpp
                               {-# LINE 1080 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule110 #-}
   rule110 = \ ((_lhsIisDeclOfLet) :: Bool) ->
     _lhsIisDeclOfLet
   {-# INLINE rule111 #-}
   rule111 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule112 #-}
   rule112 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule113 #-}
   rule113 = \ ((_lhsIoutputfile) :: String) ->
     _lhsIoutputfile
   {-# INLINE rule114 #-}
   rule114 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule115 #-}
   rule115 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule116 #-}
   rule116 = \ ((_lhsIoutputfile) :: String) ->
     _lhsIoutputfile
{-# NOINLINE sem_Decl_Data #-}
sem_Decl_Data :: (String) -> ([String]) -> T_DataAlts  -> (Bool) -> ([String]) -> T_Decl 
sem_Decl_Data !arg_name_ !arg_params_ arg_alts_ !arg_strict_ !arg_derivings_ = T_Decl (return st20) where
   {-# NOINLINE st20 #-}
   !st20 = let
      v19 :: T_Decl_v19 
      v19 = \ !(T_Decl_vIn19 _lhsIisDeclOfLet _lhsInested _lhsIoptions _lhsIoutputfile) -> ( let
         _altsX17 = Control.Monad.Identity.runIdentity (attach_T_DataAlts (arg_alts_))
         (T_DataAlts_vOut16 _altsIpps) = inv_DataAlts_s17 _altsX17 (T_DataAlts_vIn16 _altsOnested _altsOstrictPre)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule117 _altsIpps arg_derivings_ arg_name_ arg_params_
         _altsOstrictPre = rule118 arg_strict_
         _altsOnested = rule119 _lhsInested
         !__result_ = T_Decl_vOut19 _lhsOpp
         in __result_ )
     in C_Decl_s20 v19
   {-# INLINE rule117 #-}
   {-# LINE 110 "./src-ag/PrintCode.ag" #-}
   rule117 = \ ((_altsIpps) :: PP_Docs) derivings_ name_ params_ ->
                               {-# LINE 110 "./src-ag/PrintCode.ag" #-}
                               "data" >#< hv_sp (name_ : params_)
                               >#<  ( case _altsIpps of
                                            [] -> empty
                                            (x:xs) ->              "=" >#<  x
                                                   >-< vlist (map ("|" >#<) xs)
                                       >-< if null derivings_
                                              then empty
                                              else "deriving" >#< ppTuple False (map text derivings_)
                                    )
                               {-# LINE 1131 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule118 #-}
   {-# LINE 321 "./src-ag/PrintCode.ag" #-}
   rule118 = \ strict_ ->
                            {-# LINE 321 "./src-ag/PrintCode.ag" #-}
                            if strict_ then pp "!" else empty
                            {-# LINE 1137 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule119 #-}
   rule119 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
{-# NOINLINE sem_Decl_NewType #-}
sem_Decl_NewType :: (String) -> ([String]) -> (String) -> T_Type  -> T_Decl 
sem_Decl_NewType !arg_name_ !arg_params_ !arg_con_ arg_tp_ = T_Decl (return st20) where
   {-# NOINLINE st20 #-}
   !st20 = let
      v19 :: T_Decl_v19 
      v19 = \ !(T_Decl_vIn19 _lhsIisDeclOfLet _lhsInested _lhsIoptions _lhsIoutputfile) -> ( let
         _tpX50 = Control.Monad.Identity.runIdentity (attach_T_Type (arg_tp_))
         (T_Type_vOut49 _tpIpp _tpIprec) = inv_Type_s50 _tpX50 (T_Type_vIn49 _tpOnested)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule120 _tpIpp arg_con_ arg_name_ arg_params_
         _tpOnested = rule121 _lhsInested
         !__result_ = T_Decl_vOut19 _lhsOpp
         in __result_ )
     in C_Decl_s20 v19
   {-# INLINE rule120 #-}
   {-# LINE 119 "./src-ag/PrintCode.ag" #-}
   rule120 = \ ((_tpIpp) :: PP_Doc) con_ name_ params_ ->
                               {-# LINE 119 "./src-ag/PrintCode.ag" #-}
                               "newtype" >#< hv_sp (name_ : params_) >#< "=" >#< con_ >#< pp_parens _tpIpp
                               {-# LINE 1161 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule121 #-}
   rule121 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
{-# NOINLINE sem_Decl_Type #-}
sem_Decl_Type :: (String) -> ([String]) -> T_Type  -> T_Decl 
sem_Decl_Type !arg_name_ !arg_params_ arg_tp_ = T_Decl (return st20) where
   {-# NOINLINE st20 #-}
   !st20 = let
      v19 :: T_Decl_v19 
      v19 = \ !(T_Decl_vIn19 _lhsIisDeclOfLet _lhsInested _lhsIoptions _lhsIoutputfile) -> ( let
         _tpX50 = Control.Monad.Identity.runIdentity (attach_T_Type (arg_tp_))
         (T_Type_vOut49 _tpIpp _tpIprec) = inv_Type_s50 _tpX50 (T_Type_vIn49 _tpOnested)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule122 _tpIpp arg_name_ arg_params_
         _tpOnested = rule123 _lhsInested
         !__result_ = T_Decl_vOut19 _lhsOpp
         in __result_ )
     in C_Decl_s20 v19
   {-# INLINE rule122 #-}
   {-# LINE 120 "./src-ag/PrintCode.ag" #-}
   rule122 = \ ((_tpIpp) :: PP_Doc) name_ params_ ->
                               {-# LINE 120 "./src-ag/PrintCode.ag" #-}
                               "type" >#< hv_sp (name_ : params_) >#< "=" >#<  _tpIpp
                               {-# LINE 1185 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule123 #-}
   rule123 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
{-# NOINLINE sem_Decl_TSig #-}
sem_Decl_TSig :: (String) -> T_Type  -> T_Decl 
sem_Decl_TSig !arg_name_ arg_tp_ = T_Decl (return st20) where
   {-# NOINLINE st20 #-}
   !st20 = let
      v19 :: T_Decl_v19 
      v19 = \ !(T_Decl_vIn19 _lhsIisDeclOfLet _lhsInested _lhsIoptions _lhsIoutputfile) -> ( let
         _tpX50 = Control.Monad.Identity.runIdentity (attach_T_Type (arg_tp_))
         (T_Type_vOut49 _tpIpp _tpIprec) = inv_Type_s50 _tpX50 (T_Type_vIn49 _tpOnested)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule124 _tpIpp arg_name_
         _tpOnested = rule125 _lhsInested
         !__result_ = T_Decl_vOut19 _lhsOpp
         in __result_ )
     in C_Decl_s20 v19
   {-# INLINE rule124 #-}
   {-# LINE 121 "./src-ag/PrintCode.ag" #-}
   rule124 = \ ((_tpIpp) :: PP_Doc) name_ ->
                               {-# LINE 121 "./src-ag/PrintCode.ag" #-}
                               name_ >#< "::" >#< _tpIpp
                               {-# LINE 1209 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule125 #-}
   rule125 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
{-# NOINLINE sem_Decl_Comment #-}
sem_Decl_Comment :: (String) -> T_Decl 
sem_Decl_Comment !arg_txt_ = T_Decl (return st20) where
   {-# NOINLINE st20 #-}
   !st20 = let
      v19 :: T_Decl_v19 
      v19 = \ !(T_Decl_vIn19 _lhsIisDeclOfLet _lhsInested _lhsIoptions _lhsIoutputfile) -> ( let
         _lhsOpp :: PP_Doc
         _lhsOpp = rule126 arg_txt_
         !__result_ = T_Decl_vOut19 _lhsOpp
         in __result_ )
     in C_Decl_s20 v19
   {-# INLINE rule126 #-}
   {-# LINE 122 "./src-ag/PrintCode.ag" #-}
   rule126 = \ txt_ ->
                               {-# LINE 122 "./src-ag/PrintCode.ag" #-}
                               if '\n' `elem` txt_
                                 then "{-" >-< vlist (lines txt_) >-< "-}"
                                 else "--" >#< txt_
                               {-# LINE 1232 "dist/build/PrintCode.hs"#-}
{-# NOINLINE sem_Decl_PragmaDecl #-}
sem_Decl_PragmaDecl :: (String) -> T_Decl 
sem_Decl_PragmaDecl !arg_txt_ = T_Decl (return st20) where
   {-# NOINLINE st20 #-}
   !st20 = let
      v19 :: T_Decl_v19 
      v19 = \ !(T_Decl_vIn19 _lhsIisDeclOfLet _lhsInested _lhsIoptions _lhsIoutputfile) -> ( let
         _lhsOpp :: PP_Doc
         _lhsOpp = rule127 arg_txt_
         !__result_ = T_Decl_vOut19 _lhsOpp
         in __result_ )
     in C_Decl_s20 v19
   {-# INLINE rule127 #-}
   {-# LINE 125 "./src-ag/PrintCode.ag" #-}
   rule127 = \ txt_ ->
                               {-# LINE 125 "./src-ag/PrintCode.ag" #-}
                               "{-#" >#< text txt_ >#< "#-}"
                               {-# LINE 1250 "dist/build/PrintCode.hs"#-}
{-# NOINLINE sem_Decl_Resume #-}
sem_Decl_Resume :: (Bool) -> (String) -> T_Lhs  -> T_Expr  -> T_Decl 
sem_Decl_Resume !arg_monadic_ _ arg_left_ arg_rhs_ = T_Decl (return st20) where
   {-# NOINLINE st20 #-}
   !st20 = let
      v19 :: T_Decl_v19 
      v19 = \ !(T_Decl_vIn19 _lhsIisDeclOfLet _lhsInested _lhsIoptions _lhsIoutputfile) -> ( let
         _leftX32 = Control.Monad.Identity.runIdentity (attach_T_Lhs (arg_left_))
         _rhsX26 = Control.Monad.Identity.runIdentity (attach_T_Expr (arg_rhs_))
         (T_Lhs_vOut31 _leftIpp) = inv_Lhs_s32 _leftX32 (T_Lhs_vIn31 _leftOisDeclOfLet _leftOnested _leftOoptions _leftOoutputfile)
         (T_Expr_vOut25 _rhsIpp) = inv_Expr_s26 _rhsX26 (T_Expr_vIn25 _rhsOnested _rhsOoptions _rhsOoutputfile)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule128 _leftIpp _rhsIpp arg_monadic_
         _leftOisDeclOfLet = rule129 _lhsIisDeclOfLet
         _leftOnested = rule130 _lhsInested
         _leftOoptions = rule131 _lhsIoptions
         _leftOoutputfile = rule132 _lhsIoutputfile
         _rhsOnested = rule133 _lhsInested
         _rhsOoptions = rule134 _lhsIoptions
         _rhsOoutputfile = rule135 _lhsIoutputfile
         !__result_ = T_Decl_vOut19 _lhsOpp
         in __result_ )
     in C_Decl_s20 v19
   {-# INLINE rule128 #-}
   {-# LINE 126 "./src-ag/PrintCode.ag" #-}
   rule128 = \ ((_leftIpp) :: PP_Doc) ((_rhsIpp) :: PP_Doc) monadic_ ->
                               {-# LINE 126 "./src-ag/PrintCode.ag" #-}
                               if monadic_
                               then _leftIpp >#< "<-" >#< _rhsIpp
                               else _leftIpp >#< "=" >-< indent 4 _rhsIpp
                               {-# LINE 1281 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule129 #-}
   rule129 = \ ((_lhsIisDeclOfLet) :: Bool) ->
     _lhsIisDeclOfLet
   {-# INLINE rule130 #-}
   rule130 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule131 #-}
   rule131 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule132 #-}
   rule132 = \ ((_lhsIoutputfile) :: String) ->
     _lhsIoutputfile
   {-# INLINE rule133 #-}
   rule133 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule134 #-}
   rule134 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule135 #-}
   rule135 = \ ((_lhsIoutputfile) :: String) ->
     _lhsIoutputfile
{-# NOINLINE sem_Decl_EvalDecl #-}
sem_Decl_EvalDecl :: (String) -> T_Lhs  -> T_Expr  -> T_Decl 
sem_Decl_EvalDecl !arg_nt_ arg_left_ arg_rhs_ = T_Decl (return st20) where
   {-# NOINLINE st20 #-}
   !st20 = let
      v19 :: T_Decl_v19 
      v19 = \ !(T_Decl_vIn19 _lhsIisDeclOfLet _lhsInested _lhsIoptions _lhsIoutputfile) -> ( let
         _leftX32 = Control.Monad.Identity.runIdentity (attach_T_Lhs (arg_left_))
         _rhsX26 = Control.Monad.Identity.runIdentity (attach_T_Expr (arg_rhs_))
         (T_Lhs_vOut31 _leftIpp) = inv_Lhs_s32 _leftX32 (T_Lhs_vIn31 _leftOisDeclOfLet _leftOnested _leftOoptions _leftOoutputfile)
         (T_Expr_vOut25 _rhsIpp) = inv_Expr_s26 _rhsX26 (T_Expr_vIn25 _rhsOnested _rhsOoptions _rhsOoutputfile)
         _strat = rule136 _lhsIoptions
         _lhsOpp :: PP_Doc
         _lhsOpp = rule137 _leftIpp _lhsIoptions _rhsIpp _strat arg_nt_
         _leftOisDeclOfLet = rule138 _lhsIisDeclOfLet
         _leftOnested = rule139 _lhsInested
         _leftOoptions = rule140 _lhsIoptions
         _leftOoutputfile = rule141 _lhsIoutputfile
         _rhsOnested = rule142 _lhsInested
         _rhsOoptions = rule143 _lhsIoptions
         _rhsOoutputfile = rule144 _lhsIoutputfile
         !__result_ = T_Decl_vOut19 _lhsOpp
         in __result_ )
     in C_Decl_s20 v19
   {-# INLINE rule136 #-}
   {-# LINE 129 "./src-ag/PrintCode.ag" #-}
   rule136 = \ ((_lhsIoptions) :: Options) ->
                               {-# LINE 129 "./src-ag/PrintCode.ag" #-}
                               if breadthFirstStrict _lhsIoptions
                               then "stepwiseEval"
                               else "lazyEval"
                               {-# LINE 1334 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule137 #-}
   {-# LINE 132 "./src-ag/PrintCode.ag" #-}
   rule137 = \ ((_leftIpp) :: PP_Doc) ((_lhsIoptions) :: Options) ((_rhsIpp) :: PP_Doc) _strat nt_ ->
                               {-# LINE 132 "./src-ag/PrintCode.ag" #-}
                               if breadthFirst _lhsIoptions
                               then _leftIpp >#< "=" >#< "case" >#< _strat     >#< pp_parens _rhsIpp >#< "of"
                                    >-< indent 4 (
                                      pp_parens (nt_ >|< "_Syn" >#< "_val") >#< "-> _val"
                                    )
                               else _leftIpp >#< "=" >#< _rhsIpp
                               {-# LINE 1345 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule138 #-}
   rule138 = \ ((_lhsIisDeclOfLet) :: Bool) ->
     _lhsIisDeclOfLet
   {-# INLINE rule139 #-}
   rule139 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule140 #-}
   rule140 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule141 #-}
   rule141 = \ ((_lhsIoutputfile) :: String) ->
     _lhsIoutputfile
   {-# INLINE rule142 #-}
   rule142 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule143 #-}
   rule143 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule144 #-}
   rule144 = \ ((_lhsIoutputfile) :: String) ->
     _lhsIoutputfile

-- Decls -------------------------------------------------------
-- wrapper
data Inh_Decls  = Inh_Decls { isDeclOfLet_Inh_Decls :: !(Bool), nested_Inh_Decls :: !(Bool), options_Inh_Decls :: !(Options), outputfile_Inh_Decls :: !(String) }
data Syn_Decls  = Syn_Decls { pps_Syn_Decls :: !(PP_Docs) }
{-# INLINABLE wrap_Decls #-}
wrap_Decls :: T_Decls  -> Inh_Decls  -> (Syn_Decls )
wrap_Decls !(T_Decls act) !(Inh_Decls _lhsIisDeclOfLet _lhsInested _lhsIoptions _lhsIoutputfile) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_Decls_vIn22 _lhsIisDeclOfLet _lhsInested _lhsIoptions _lhsIoutputfile
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
data T_Decls_vIn22  = T_Decls_vIn22 (Bool) (Bool) (Options) (String)
data T_Decls_vOut22  = T_Decls_vOut22 (PP_Docs)
{-# NOINLINE sem_Decls_Cons #-}
sem_Decls_Cons :: T_Decl  -> T_Decls  -> T_Decls 
sem_Decls_Cons arg_hd_ arg_tl_ = T_Decls (return st23) where
   {-# NOINLINE st23 #-}
   !st23 = let
      v22 :: T_Decls_v22 
      v22 = \ !(T_Decls_vIn22 _lhsIisDeclOfLet _lhsInested _lhsIoptions _lhsIoutputfile) -> ( let
         _hdX20 = Control.Monad.Identity.runIdentity (attach_T_Decl (arg_hd_))
         _tlX23 = Control.Monad.Identity.runIdentity (attach_T_Decls (arg_tl_))
         (T_Decl_vOut19 _hdIpp) = inv_Decl_s20 _hdX20 (T_Decl_vIn19 _hdOisDeclOfLet _hdOnested _hdOoptions _hdOoutputfile)
         (T_Decls_vOut22 _tlIpps) = inv_Decls_s23 _tlX23 (T_Decls_vIn22 _tlOisDeclOfLet _tlOnested _tlOoptions _tlOoutputfile)
         _lhsOpps :: PP_Docs
         _lhsOpps = rule145 _hdIpp _tlIpps
         _hdOisDeclOfLet = rule146 _lhsIisDeclOfLet
         _hdOnested = rule147 _lhsInested
         _hdOoptions = rule148 _lhsIoptions
         _hdOoutputfile = rule149 _lhsIoutputfile
         _tlOisDeclOfLet = rule150 _lhsIisDeclOfLet
         _tlOnested = rule151 _lhsInested
         _tlOoptions = rule152 _lhsIoptions
         _tlOoutputfile = rule153 _lhsIoutputfile
         !__result_ = T_Decls_vOut22 _lhsOpps
         in __result_ )
     in C_Decls_s23 v22
   {-# INLINE rule145 #-}
   {-# LINE 84 "./src-ag/PrintCode.ag" #-}
   rule145 = \ ((_hdIpp) :: PP_Doc) ((_tlIpps) :: PP_Docs) ->
                     {-# LINE 84 "./src-ag/PrintCode.ag" #-}
                     _hdIpp : _tlIpps
                     {-# LINE 1427 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule146 #-}
   rule146 = \ ((_lhsIisDeclOfLet) :: Bool) ->
     _lhsIisDeclOfLet
   {-# INLINE rule147 #-}
   rule147 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule148 #-}
   rule148 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule149 #-}
   rule149 = \ ((_lhsIoutputfile) :: String) ->
     _lhsIoutputfile
   {-# INLINE rule150 #-}
   rule150 = \ ((_lhsIisDeclOfLet) :: Bool) ->
     _lhsIisDeclOfLet
   {-# INLINE rule151 #-}
   rule151 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule152 #-}
   rule152 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule153 #-}
   rule153 = \ ((_lhsIoutputfile) :: String) ->
     _lhsIoutputfile
{-# NOINLINE sem_Decls_Nil #-}
sem_Decls_Nil ::  T_Decls 
sem_Decls_Nil  = T_Decls (return st23) where
   {-# NOINLINE st23 #-}
   !st23 = let
      v22 :: T_Decls_v22 
      v22 = \ !(T_Decls_vIn22 _lhsIisDeclOfLet _lhsInested _lhsIoptions _lhsIoutputfile) -> ( let
         _lhsOpps :: PP_Docs
         _lhsOpps = rule154  ()
         !__result_ = T_Decls_vOut22 _lhsOpps
         in __result_ )
     in C_Decls_s23 v22
   {-# INLINE rule154 #-}
   {-# LINE 85 "./src-ag/PrintCode.ag" #-}
   rule154 = \  (_ :: ()) ->
                     {-# LINE 85 "./src-ag/PrintCode.ag" #-}
                     []
                     {-# LINE 1469 "dist/build/PrintCode.hs"#-}

-- Expr --------------------------------------------------------
-- wrapper
data Inh_Expr  = Inh_Expr { nested_Inh_Expr :: !(Bool), options_Inh_Expr :: !(Options), outputfile_Inh_Expr :: !(String) }
data Syn_Expr  = Syn_Expr { pp_Syn_Expr :: !(PP_Doc) }
{-# INLINABLE wrap_Expr #-}
wrap_Expr :: T_Expr  -> Inh_Expr  -> (Syn_Expr )
wrap_Expr !(T_Expr act) !(Inh_Expr _lhsInested _lhsIoptions _lhsIoutputfile) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_Expr_vIn25 _lhsInested _lhsIoptions _lhsIoutputfile
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
data T_Expr_vIn25  = T_Expr_vIn25 (Bool) (Options) (String)
data T_Expr_vOut25  = T_Expr_vOut25 (PP_Doc)
{-# NOINLINE sem_Expr_Let #-}
sem_Expr_Let :: T_Decls  -> T_Expr  -> T_Expr 
sem_Expr_Let arg_decls_ arg_body_ = T_Expr (return st26) where
   {-# NOINLINE st26 #-}
   !st26 = let
      v25 :: T_Expr_v25 
      v25 = \ !(T_Expr_vIn25 _lhsInested _lhsIoptions _lhsIoutputfile) -> ( let
         _declsX23 = Control.Monad.Identity.runIdentity (attach_T_Decls (arg_decls_))
         _bodyX26 = Control.Monad.Identity.runIdentity (attach_T_Expr (arg_body_))
         (T_Decls_vOut22 _declsIpps) = inv_Decls_s23 _declsX23 (T_Decls_vIn22 _declsOisDeclOfLet _declsOnested _declsOoptions _declsOoutputfile)
         (T_Expr_vOut25 _bodyIpp) = inv_Expr_s26 _bodyX26 (T_Expr_vIn25 _bodyOnested _bodyOoptions _bodyOoutputfile)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule155 _bodyIpp _declsIpps
         _declsOisDeclOfLet = rule156  ()
         _declsOnested = rule157 _lhsInested
         _declsOoptions = rule158 _lhsIoptions
         _declsOoutputfile = rule159 _lhsIoutputfile
         _bodyOnested = rule160 _lhsInested
         _bodyOoptions = rule161 _lhsIoptions
         _bodyOoutputfile = rule162 _lhsIoutputfile
         !__result_ = T_Expr_vOut25 _lhsOpp
         in __result_ )
     in C_Expr_s26 v25
   {-# INLINE rule155 #-}
   {-# LINE 140 "./src-ag/PrintCode.ag" #-}
   rule155 = \ ((_bodyIpp) :: PP_Doc) ((_declsIpps) :: PP_Docs) ->
                               {-# LINE 140 "./src-ag/PrintCode.ag" #-}
                               pp_parens (    "let" >#< (vlist _declsIpps)
                                         >-< "in " >#< _bodyIpp
                                         )
                               {-# LINE 1547 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule156 #-}
   {-# LINE 416 "./src-ag/PrintCode.ag" #-}
   rule156 = \  (_ :: ()) ->
                            {-# LINE 416 "./src-ag/PrintCode.ag" #-}
                            True
                            {-# LINE 1553 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule157 #-}
   rule157 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule158 #-}
   rule158 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule159 #-}
   rule159 = \ ((_lhsIoutputfile) :: String) ->
     _lhsIoutputfile
   {-# INLINE rule160 #-}
   rule160 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule161 #-}
   rule161 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule162 #-}
   rule162 = \ ((_lhsIoutputfile) :: String) ->
     _lhsIoutputfile
{-# NOINLINE sem_Expr_Case #-}
sem_Expr_Case :: T_Expr  -> T_CaseAlts  -> T_Expr 
sem_Expr_Case arg_expr_ arg_alts_ = T_Expr (return st26) where
   {-# NOINLINE st26 #-}
   !st26 = let
      v25 :: T_Expr_v25 
      v25 = \ !(T_Expr_vIn25 _lhsInested _lhsIoptions _lhsIoutputfile) -> ( let
         _exprX26 = Control.Monad.Identity.runIdentity (attach_T_Expr (arg_expr_))
         _altsX5 = Control.Monad.Identity.runIdentity (attach_T_CaseAlts (arg_alts_))
         (T_Expr_vOut25 _exprIpp) = inv_Expr_s26 _exprX26 (T_Expr_vIn25 _exprOnested _exprOoptions _exprOoutputfile)
         (T_CaseAlts_vOut4 _altsIpps) = inv_CaseAlts_s5 _altsX5 (T_CaseAlts_vIn4 _altsOnested _altsOoptions _altsOoutputfile)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule163 _altsIpps _exprIpp
         _exprOnested = rule164 _lhsInested
         _exprOoptions = rule165 _lhsIoptions
         _exprOoutputfile = rule166 _lhsIoutputfile
         _altsOnested = rule167 _lhsInested
         _altsOoptions = rule168 _lhsIoptions
         _altsOoutputfile = rule169 _lhsIoutputfile
         !__result_ = T_Expr_vOut25 _lhsOpp
         in __result_ )
     in C_Expr_s26 v25
   {-# INLINE rule163 #-}
   {-# LINE 143 "./src-ag/PrintCode.ag" #-}
   rule163 = \ ((_altsIpps) :: PP_Docs) ((_exprIpp) :: PP_Doc) ->
                               {-# LINE 143 "./src-ag/PrintCode.ag" #-}
                               pp_parens (    "case" >#< pp_parens _exprIpp >#< "of"
                                         >-< (vlist _altsIpps)
                                         )
                               {-# LINE 1601 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule164 #-}
   rule164 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule165 #-}
   rule165 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule166 #-}
   rule166 = \ ((_lhsIoutputfile) :: String) ->
     _lhsIoutputfile
   {-# INLINE rule167 #-}
   rule167 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule168 #-}
   rule168 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule169 #-}
   rule169 = \ ((_lhsIoutputfile) :: String) ->
     _lhsIoutputfile
{-# NOINLINE sem_Expr_Do #-}
sem_Expr_Do :: T_Decls  -> T_Expr  -> T_Expr 
sem_Expr_Do arg_stmts_ arg_body_ = T_Expr (return st26) where
   {-# NOINLINE st26 #-}
   !st26 = let
      v25 :: T_Expr_v25 
      v25 = \ !(T_Expr_vIn25 _lhsInested _lhsIoptions _lhsIoutputfile) -> ( let
         _stmtsX23 = Control.Monad.Identity.runIdentity (attach_T_Decls (arg_stmts_))
         _bodyX26 = Control.Monad.Identity.runIdentity (attach_T_Expr (arg_body_))
         (T_Decls_vOut22 _stmtsIpps) = inv_Decls_s23 _stmtsX23 (T_Decls_vIn22 _stmtsOisDeclOfLet _stmtsOnested _stmtsOoptions _stmtsOoutputfile)
         (T_Expr_vOut25 _bodyIpp) = inv_Expr_s26 _bodyX26 (T_Expr_vIn25 _bodyOnested _bodyOoptions _bodyOoutputfile)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule170 _bodyIpp _stmtsIpps
         _stmtsOisDeclOfLet = rule171  ()
         _stmtsOnested = rule172 _lhsInested
         _stmtsOoptions = rule173 _lhsIoptions
         _stmtsOoutputfile = rule174 _lhsIoutputfile
         _bodyOnested = rule175 _lhsInested
         _bodyOoptions = rule176 _lhsIoptions
         _bodyOoutputfile = rule177 _lhsIoutputfile
         !__result_ = T_Expr_vOut25 _lhsOpp
         in __result_ )
     in C_Expr_s26 v25
   {-# INLINE rule170 #-}
   {-# LINE 146 "./src-ag/PrintCode.ag" #-}
   rule170 = \ ((_bodyIpp) :: PP_Doc) ((_stmtsIpps) :: PP_Docs) ->
                               {-# LINE 146 "./src-ag/PrintCode.ag" #-}
                               pp_parens ( "do" >#< (   vlist _stmtsIpps
                                                    >-< ("return" >#< _bodyIpp))
                                         )
                               {-# LINE 1650 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule171 #-}
   {-# LINE 418 "./src-ag/PrintCode.ag" #-}
   rule171 = \  (_ :: ()) ->
                            {-# LINE 418 "./src-ag/PrintCode.ag" #-}
                            False
                            {-# LINE 1656 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule172 #-}
   rule172 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule173 #-}
   rule173 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule174 #-}
   rule174 = \ ((_lhsIoutputfile) :: String) ->
     _lhsIoutputfile
   {-# INLINE rule175 #-}
   rule175 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule176 #-}
   rule176 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule177 #-}
   rule177 = \ ((_lhsIoutputfile) :: String) ->
     _lhsIoutputfile
{-# NOINLINE sem_Expr_Lambda #-}
sem_Expr_Lambda :: T_Exprs  -> T_Expr  -> T_Expr 
sem_Expr_Lambda arg_args_ arg_body_ = T_Expr (return st26) where
   {-# NOINLINE st26 #-}
   !st26 = let
      v25 :: T_Expr_v25 
      v25 = \ !(T_Expr_vIn25 _lhsInested _lhsIoptions _lhsIoutputfile) -> ( let
         _argsX29 = Control.Monad.Identity.runIdentity (attach_T_Exprs (arg_args_))
         _bodyX26 = Control.Monad.Identity.runIdentity (attach_T_Expr (arg_body_))
         (T_Exprs_vOut28 _argsIpps) = inv_Exprs_s29 _argsX29 (T_Exprs_vIn28 _argsOnested _argsOoptions _argsOoutputfile)
         (T_Expr_vOut25 _bodyIpp) = inv_Expr_s26 _bodyX26 (T_Expr_vIn25 _bodyOnested _bodyOoptions _bodyOoutputfile)
         _strictParams = rule178 _argsIpps _lhsIoptions
         _addBang = rule179 _lhsIoptions
         _lhsOpp :: PP_Doc
         _lhsOpp = rule180 _addBang _argsIpps _bodyIpp _strictParams
         _argsOnested = rule181 _lhsInested
         _argsOoptions = rule182 _lhsIoptions
         _argsOoutputfile = rule183 _lhsIoutputfile
         _bodyOnested = rule184 _lhsInested
         _bodyOoptions = rule185 _lhsIoptions
         _bodyOoutputfile = rule186 _lhsIoutputfile
         !__result_ = T_Expr_vOut25 _lhsOpp
         in __result_ )
     in C_Expr_s26 v25
   {-# INLINE rule178 #-}
   {-# LINE 149 "./src-ag/PrintCode.ag" #-}
   rule178 = \ ((_argsIpps) :: PP_Docs) ((_lhsIoptions) :: Options) ->
                                    {-# LINE 149 "./src-ag/PrintCode.ag" #-}
                                    if strictSems _lhsIoptions
                                    then _argsIpps
                                    else []
                                    {-# LINE 1706 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule179 #-}
   {-# LINE 152 "./src-ag/PrintCode.ag" #-}
   rule179 = \ ((_lhsIoptions) :: Options) ->
                               {-# LINE 152 "./src-ag/PrintCode.ag" #-}
                               if bangpats _lhsIoptions
                               then \p -> pp_parens ("!" >|< p)
                               else id
                               {-# LINE 1714 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule180 #-}
   {-# LINE 155 "./src-ag/PrintCode.ag" #-}
   rule180 = \ _addBang ((_argsIpps) :: PP_Docs) ((_bodyIpp) :: PP_Doc) _strictParams ->
                               {-# LINE 155 "./src-ag/PrintCode.ag" #-}
                               pp_parens (    "\\" >#< (vlist (map _addBang     _argsIpps)) >#< "->"
                                         >-< indent 4 (_strictParams     `ppMultiSeqV` _bodyIpp)
                                         )
                               {-# LINE 1722 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule181 #-}
   rule181 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule182 #-}
   rule182 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule183 #-}
   rule183 = \ ((_lhsIoutputfile) :: String) ->
     _lhsIoutputfile
   {-# INLINE rule184 #-}
   rule184 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule185 #-}
   rule185 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule186 #-}
   rule186 = \ ((_lhsIoutputfile) :: String) ->
     _lhsIoutputfile
{-# NOINLINE sem_Expr_TupleExpr #-}
sem_Expr_TupleExpr :: T_Exprs  -> T_Expr 
sem_Expr_TupleExpr arg_exprs_ = T_Expr (return st26) where
   {-# NOINLINE st26 #-}
   !st26 = let
      v25 :: T_Expr_v25 
      v25 = \ !(T_Expr_vIn25 _lhsInested _lhsIoptions _lhsIoutputfile) -> ( let
         _exprsX29 = Control.Monad.Identity.runIdentity (attach_T_Exprs (arg_exprs_))
         (T_Exprs_vOut28 _exprsIpps) = inv_Exprs_s29 _exprsX29 (T_Exprs_vIn28 _exprsOnested _exprsOoptions _exprsOoutputfile)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule187 _exprsIpps _lhsInested
         _exprsOnested = rule188 _lhsInested
         _exprsOoptions = rule189 _lhsIoptions
         _exprsOoutputfile = rule190 _lhsIoutputfile
         !__result_ = T_Expr_vOut25 _lhsOpp
         in __result_ )
     in C_Expr_s26 v25
   {-# INLINE rule187 #-}
   {-# LINE 158 "./src-ag/PrintCode.ag" #-}
   rule187 = \ ((_exprsIpps) :: PP_Docs) ((_lhsInested) :: Bool) ->
                               {-# LINE 158 "./src-ag/PrintCode.ag" #-}
                               ppTuple _lhsInested _exprsIpps
                               {-# LINE 1763 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule188 #-}
   rule188 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule189 #-}
   rule189 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule190 #-}
   rule190 = \ ((_lhsIoutputfile) :: String) ->
     _lhsIoutputfile
{-# NOINLINE sem_Expr_UnboxedTupleExpr #-}
sem_Expr_UnboxedTupleExpr :: T_Exprs  -> T_Expr 
sem_Expr_UnboxedTupleExpr arg_exprs_ = T_Expr (return st26) where
   {-# NOINLINE st26 #-}
   !st26 = let
      v25 :: T_Expr_v25 
      v25 = \ !(T_Expr_vIn25 _lhsInested _lhsIoptions _lhsIoutputfile) -> ( let
         _exprsX29 = Control.Monad.Identity.runIdentity (attach_T_Exprs (arg_exprs_))
         (T_Exprs_vOut28 _exprsIpps) = inv_Exprs_s29 _exprsX29 (T_Exprs_vIn28 _exprsOnested _exprsOoptions _exprsOoutputfile)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule191 _exprsIpps _lhsInested
         _exprsOnested = rule192 _lhsInested
         _exprsOoptions = rule193 _lhsIoptions
         _exprsOoutputfile = rule194 _lhsIoutputfile
         !__result_ = T_Expr_vOut25 _lhsOpp
         in __result_ )
     in C_Expr_s26 v25
   {-# INLINE rule191 #-}
   {-# LINE 159 "./src-ag/PrintCode.ag" #-}
   rule191 = \ ((_exprsIpps) :: PP_Docs) ((_lhsInested) :: Bool) ->
                                      {-# LINE 159 "./src-ag/PrintCode.ag" #-}
                                      ppUnboxedTuple _lhsInested _exprsIpps
                                      {-# LINE 1795 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule192 #-}
   rule192 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule193 #-}
   rule193 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule194 #-}
   rule194 = \ ((_lhsIoutputfile) :: String) ->
     _lhsIoutputfile
{-# NOINLINE sem_Expr_App #-}
sem_Expr_App :: (String) -> T_Exprs  -> T_Expr 
sem_Expr_App !arg_name_ arg_args_ = T_Expr (return st26) where
   {-# NOINLINE st26 #-}
   !st26 = let
      v25 :: T_Expr_v25 
      v25 = \ !(T_Expr_vIn25 _lhsInested _lhsIoptions _lhsIoutputfile) -> ( let
         _argsX29 = Control.Monad.Identity.runIdentity (attach_T_Exprs (arg_args_))
         (T_Exprs_vOut28 _argsIpps) = inv_Exprs_s29 _argsX29 (T_Exprs_vIn28 _argsOnested _argsOoptions _argsOoutputfile)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule195 _argsIpps arg_name_
         _argsOnested = rule196 _lhsInested
         _argsOoptions = rule197 _lhsIoptions
         _argsOoutputfile = rule198 _lhsIoutputfile
         !__result_ = T_Expr_vOut25 _lhsOpp
         in __result_ )
     in C_Expr_s26 v25
   {-# INLINE rule195 #-}
   {-# LINE 160 "./src-ag/PrintCode.ag" #-}
   rule195 = \ ((_argsIpps) :: PP_Docs) name_ ->
                               {-# LINE 160 "./src-ag/PrintCode.ag" #-}
                               pp_parens $ name_ >#< hv_sp _argsIpps
                               {-# LINE 1827 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule196 #-}
   rule196 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule197 #-}
   rule197 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule198 #-}
   rule198 = \ ((_lhsIoutputfile) :: String) ->
     _lhsIoutputfile
{-# NOINLINE sem_Expr_SimpleExpr #-}
sem_Expr_SimpleExpr :: (String) -> T_Expr 
sem_Expr_SimpleExpr !arg_txt_ = T_Expr (return st26) where
   {-# NOINLINE st26 #-}
   !st26 = let
      v25 :: T_Expr_v25 
      v25 = \ !(T_Expr_vIn25 _lhsInested _lhsIoptions _lhsIoutputfile) -> ( let
         _lhsOpp :: PP_Doc
         _lhsOpp = rule199 arg_txt_
         !__result_ = T_Expr_vOut25 _lhsOpp
         in __result_ )
     in C_Expr_s26 v25
   {-# INLINE rule199 #-}
   {-# LINE 161 "./src-ag/PrintCode.ag" #-}
   rule199 = \ txt_ ->
                               {-# LINE 161 "./src-ag/PrintCode.ag" #-}
                               text txt_
                               {-# LINE 1854 "dist/build/PrintCode.hs"#-}
{-# NOINLINE sem_Expr_TextExpr #-}
sem_Expr_TextExpr :: ([String]) -> T_Expr 
sem_Expr_TextExpr !arg_lns_ = T_Expr (return st26) where
   {-# NOINLINE st26 #-}
   !st26 = let
      v25 :: T_Expr_v25 
      v25 = \ !(T_Expr_vIn25 _lhsInested _lhsIoptions _lhsIoutputfile) -> ( let
         _lhsOpp :: PP_Doc
         _lhsOpp = rule200 arg_lns_
         !__result_ = T_Expr_vOut25 _lhsOpp
         in __result_ )
     in C_Expr_s26 v25
   {-# INLINE rule200 #-}
   {-# LINE 162 "./src-ag/PrintCode.ag" #-}
   rule200 = \ lns_ ->
                               {-# LINE 162 "./src-ag/PrintCode.ag" #-}
                               vlist (map text lns_)
                               {-# LINE 1872 "dist/build/PrintCode.hs"#-}
{-# NOINLINE sem_Expr_Trace #-}
sem_Expr_Trace :: (String) -> T_Expr  -> T_Expr 
sem_Expr_Trace !arg_txt_ arg_expr_ = T_Expr (return st26) where
   {-# NOINLINE st26 #-}
   !st26 = let
      v25 :: T_Expr_v25 
      v25 = \ !(T_Expr_vIn25 _lhsInested _lhsIoptions _lhsIoutputfile) -> ( let
         _exprX26 = Control.Monad.Identity.runIdentity (attach_T_Expr (arg_expr_))
         (T_Expr_vOut25 _exprIpp) = inv_Expr_s26 _exprX26 (T_Expr_vIn25 _exprOnested _exprOoptions _exprOoutputfile)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule201 _exprIpp arg_txt_
         _exprOnested = rule202 _lhsInested
         _exprOoptions = rule203 _lhsIoptions
         _exprOoutputfile = rule204 _lhsIoutputfile
         !__result_ = T_Expr_vOut25 _lhsOpp
         in __result_ )
     in C_Expr_s26 v25
   {-# INLINE rule201 #-}
   {-# LINE 163 "./src-ag/PrintCode.ag" #-}
   rule201 = \ ((_exprIpp) :: PP_Doc) txt_ ->
                               {-# LINE 163 "./src-ag/PrintCode.ag" #-}
                               "trace" >#< (   pp_parens ("\"" >|< text txt_ >|< "\"")
                                           >-< pp_parens _exprIpp
                                           )
                               {-# LINE 1897 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule202 #-}
   rule202 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule203 #-}
   rule203 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule204 #-}
   rule204 = \ ((_lhsIoutputfile) :: String) ->
     _lhsIoutputfile
{-# NOINLINE sem_Expr_PragmaExpr #-}
sem_Expr_PragmaExpr :: (Bool) -> (Bool) -> (String) -> T_Expr  -> T_Expr 
sem_Expr_PragmaExpr !arg_onLeftSide_ !arg_onNewLine_ !arg_txt_ arg_expr_ = T_Expr (return st26) where
   {-# NOINLINE st26 #-}
   !st26 = let
      v25 :: T_Expr_v25 
      v25 = \ !(T_Expr_vIn25 _lhsInested _lhsIoptions _lhsIoutputfile) -> ( let
         _exprX26 = Control.Monad.Identity.runIdentity (attach_T_Expr (arg_expr_))
         (T_Expr_vOut25 _exprIpp) = inv_Expr_s26 _exprX26 (T_Expr_vIn25 _exprOnested _exprOoptions _exprOoutputfile)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule205 _exprIpp arg_onLeftSide_ arg_onNewLine_ arg_txt_
         _exprOnested = rule206 _lhsInested
         _exprOoptions = rule207 _lhsIoptions
         _exprOoutputfile = rule208 _lhsIoutputfile
         !__result_ = T_Expr_vOut25 _lhsOpp
         in __result_ )
     in C_Expr_s26 v25
   {-# INLINE rule205 #-}
   {-# LINE 166 "./src-ag/PrintCode.ag" #-}
   rule205 = \ ((_exprIpp) :: PP_Doc) onLeftSide_ onNewLine_ txt_ ->
                               {-# LINE 166 "./src-ag/PrintCode.ag" #-}
                               let pragmaDoc = "{-#" >#< txt_ >#< "#-}"
                                   op = if onNewLine_
                                        then (>-<)
                                        else (>#<)
                                   leftOp x y = if onLeftSide_
                                                then x `op` y
                                                else y
                                   rightOp x y = if onLeftSide_
                                                 then x
                                                 else x `op` y
                               in pp_parens (pragmaDoc `leftOp` _exprIpp `rightOp` pragmaDoc)
                               {-# LINE 1939 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule206 #-}
   rule206 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule207 #-}
   rule207 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule208 #-}
   rule208 = \ ((_lhsIoutputfile) :: String) ->
     _lhsIoutputfile
{-# NOINLINE sem_Expr_LineExpr #-}
sem_Expr_LineExpr :: T_Expr  -> T_Expr 
sem_Expr_LineExpr arg_expr_ = T_Expr (return st26) where
   {-# NOINLINE st26 #-}
   !st26 = let
      v25 :: T_Expr_v25 
      v25 = \ !(T_Expr_vIn25 _lhsInested _lhsIoptions _lhsIoutputfile) -> ( let
         _exprX26 = Control.Monad.Identity.runIdentity (attach_T_Expr (arg_expr_))
         (T_Expr_vOut25 _exprIpp) = inv_Expr_s26 _exprX26 (T_Expr_vIn25 _exprOnested _exprOoptions _exprOoutputfile)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule209 _exprIpp _lhsIoutputfile
         _exprOnested = rule210 _lhsInested
         _exprOoptions = rule211 _lhsIoptions
         _exprOoutputfile = rule212 _lhsIoutputfile
         !__result_ = T_Expr_vOut25 _lhsOpp
         in __result_ )
     in C_Expr_s26 v25
   {-# INLINE rule209 #-}
   {-# LINE 177 "./src-ag/PrintCode.ag" #-}
   rule209 = \ ((_exprIpp) :: PP_Doc) ((_lhsIoutputfile) :: String) ->
                               {-# LINE 177 "./src-ag/PrintCode.ag" #-}
                               _exprIpp >-< "{-# LINE" >#< ppWithLineNr (\n -> pp $ show $ n + 1) >#< show _lhsIoutputfile >#< "#-}"
                                        >-< ""
                               {-# LINE 1972 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule210 #-}
   rule210 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule211 #-}
   rule211 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule212 #-}
   rule212 = \ ((_lhsIoutputfile) :: String) ->
     _lhsIoutputfile
{-# NOINLINE sem_Expr_TypedExpr #-}
sem_Expr_TypedExpr :: T_Expr  -> T_Type  -> T_Expr 
sem_Expr_TypedExpr arg_expr_ arg_tp_ = T_Expr (return st26) where
   {-# NOINLINE st26 #-}
   !st26 = let
      v25 :: T_Expr_v25 
      v25 = \ !(T_Expr_vIn25 _lhsInested _lhsIoptions _lhsIoutputfile) -> ( let
         _exprX26 = Control.Monad.Identity.runIdentity (attach_T_Expr (arg_expr_))
         _tpX50 = Control.Monad.Identity.runIdentity (attach_T_Type (arg_tp_))
         (T_Expr_vOut25 _exprIpp) = inv_Expr_s26 _exprX26 (T_Expr_vIn25 _exprOnested _exprOoptions _exprOoutputfile)
         (T_Type_vOut49 _tpIpp _tpIprec) = inv_Type_s50 _tpX50 (T_Type_vIn49 _tpOnested)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule213 _exprIpp _tpIpp
         _exprOnested = rule214 _lhsInested
         _exprOoptions = rule215 _lhsIoptions
         _exprOoutputfile = rule216 _lhsIoutputfile
         _tpOnested = rule217 _lhsInested
         !__result_ = T_Expr_vOut25 _lhsOpp
         in __result_ )
     in C_Expr_s26 v25
   {-# INLINE rule213 #-}
   {-# LINE 179 "./src-ag/PrintCode.ag" #-}
   rule213 = \ ((_exprIpp) :: PP_Doc) ((_tpIpp) :: PP_Doc) ->
                               {-# LINE 179 "./src-ag/PrintCode.ag" #-}
                               pp_parens (_exprIpp >#< "::" >#< _tpIpp)
                               {-# LINE 2007 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule214 #-}
   rule214 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule215 #-}
   rule215 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule216 #-}
   rule216 = \ ((_lhsIoutputfile) :: String) ->
     _lhsIoutputfile
   {-# INLINE rule217 #-}
   rule217 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
{-# NOINLINE sem_Expr_ResultExpr #-}
sem_Expr_ResultExpr :: (String) -> T_Expr  -> T_Expr 
sem_Expr_ResultExpr !arg_nt_ arg_expr_ = T_Expr (return st26) where
   {-# NOINLINE st26 #-}
   !st26 = let
      v25 :: T_Expr_v25 
      v25 = \ !(T_Expr_vIn25 _lhsInested _lhsIoptions _lhsIoutputfile) -> ( let
         _exprX26 = Control.Monad.Identity.runIdentity (attach_T_Expr (arg_expr_))
         (T_Expr_vOut25 _exprIpp) = inv_Expr_s26 _exprX26 (T_Expr_vIn25 _exprOnested _exprOoptions _exprOoutputfile)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule218 _exprIpp _lhsIoptions arg_nt_
         _exprOnested = rule219 _lhsInested
         _exprOoptions = rule220 _lhsIoptions
         _exprOoutputfile = rule221 _lhsIoutputfile
         !__result_ = T_Expr_vOut25 _lhsOpp
         in __result_ )
     in C_Expr_s26 v25
   {-# INLINE rule218 #-}
   {-# LINE 180 "./src-ag/PrintCode.ag" #-}
   rule218 = \ ((_exprIpp) :: PP_Doc) ((_lhsIoptions) :: Options) nt_ ->
                               {-# LINE 180 "./src-ag/PrintCode.ag" #-}
                               if breadthFirst _lhsIoptions
                               then "final" >#<
                                    pp_parens (nt_ >|< "_Syn" >#< pp_parens _exprIpp)
                               else _exprIpp
                               {-# LINE 2045 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule219 #-}
   rule219 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule220 #-}
   rule220 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule221 #-}
   rule221 = \ ((_lhsIoutputfile) :: String) ->
     _lhsIoutputfile
{-# NOINLINE sem_Expr_InvokeExpr #-}
sem_Expr_InvokeExpr :: (String) -> T_Expr  -> T_Exprs  -> T_Expr 
sem_Expr_InvokeExpr !arg_nt_ arg_expr_ arg_args_ = T_Expr (return st26) where
   {-# NOINLINE st26 #-}
   !st26 = let
      v25 :: T_Expr_v25 
      v25 = \ !(T_Expr_vIn25 _lhsInested _lhsIoptions _lhsIoutputfile) -> ( let
         _exprX26 = Control.Monad.Identity.runIdentity (attach_T_Expr (arg_expr_))
         _argsX29 = Control.Monad.Identity.runIdentity (attach_T_Exprs (arg_args_))
         (T_Expr_vOut25 _exprIpp) = inv_Expr_s26 _exprX26 (T_Expr_vIn25 _exprOnested _exprOoptions _exprOoutputfile)
         (T_Exprs_vOut28 _argsIpps) = inv_Exprs_s29 _argsX29 (T_Exprs_vIn28 _argsOnested _argsOoptions _argsOoutputfile)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule222 _argsIpps _exprIpp _lhsIoptions arg_nt_
         _exprOnested = rule223 _lhsInested
         _exprOoptions = rule224 _lhsIoptions
         _exprOoutputfile = rule225 _lhsIoutputfile
         _argsOnested = rule226 _lhsInested
         _argsOoptions = rule227 _lhsIoptions
         _argsOoutputfile = rule228 _lhsIoutputfile
         !__result_ = T_Expr_vOut25 _lhsOpp
         in __result_ )
     in C_Expr_s26 v25
   {-# INLINE rule222 #-}
   {-# LINE 184 "./src-ag/PrintCode.ag" #-}
   rule222 = \ ((_argsIpps) :: PP_Docs) ((_exprIpp) :: PP_Doc) ((_lhsIoptions) :: Options) nt_ ->
                               {-# LINE 184 "./src-ag/PrintCode.ag" #-}
                               if breadthFirst _lhsIoptions
                               then "invoke" >#< pp_parens _exprIpp >#< pp_parens (
                                     nt_ >|< "_Inh" >#< pp_parens (ppTuple False _argsIpps))
                               else _exprIpp >#< hv_sp _argsIpps
                               {-# LINE 2085 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule223 #-}
   rule223 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule224 #-}
   rule224 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule225 #-}
   rule225 = \ ((_lhsIoutputfile) :: String) ->
     _lhsIoutputfile
   {-# INLINE rule226 #-}
   rule226 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule227 #-}
   rule227 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule228 #-}
   rule228 = \ ((_lhsIoutputfile) :: String) ->
     _lhsIoutputfile
{-# NOINLINE sem_Expr_ResumeExpr #-}
sem_Expr_ResumeExpr :: (String) -> T_Expr  -> T_Lhs  -> T_Expr  -> T_Expr 
sem_Expr_ResumeExpr !arg_nt_ arg_expr_ arg_left_ arg_rhs_ = T_Expr (return st26) where
   {-# NOINLINE st26 #-}
   !st26 = let
      v25 :: T_Expr_v25 
      v25 = \ !(T_Expr_vIn25 _lhsInested _lhsIoptions _lhsIoutputfile) -> ( let
         _exprX26 = Control.Monad.Identity.runIdentity (attach_T_Expr (arg_expr_))
         _leftX32 = Control.Monad.Identity.runIdentity (attach_T_Lhs (arg_left_))
         _rhsX26 = Control.Monad.Identity.runIdentity (attach_T_Expr (arg_rhs_))
         (T_Expr_vOut25 _exprIpp) = inv_Expr_s26 _exprX26 (T_Expr_vIn25 _exprOnested _exprOoptions _exprOoutputfile)
         (T_Lhs_vOut31 _leftIpp) = inv_Lhs_s32 _leftX32 (T_Lhs_vIn31 _leftOisDeclOfLet _leftOnested _leftOoptions _leftOoutputfile)
         (T_Expr_vOut25 _rhsIpp) = inv_Expr_s26 _rhsX26 (T_Expr_vIn25 _rhsOnested _rhsOoptions _rhsOoutputfile)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule229 _exprIpp _leftIpp _lhsIoptions _rhsIpp arg_nt_
         _leftOisDeclOfLet = rule230  ()
         _exprOnested = rule231 _lhsInested
         _exprOoptions = rule232 _lhsIoptions
         _exprOoutputfile = rule233 _lhsIoutputfile
         _leftOnested = rule234 _lhsInested
         _leftOoptions = rule235 _lhsIoptions
         _leftOoutputfile = rule236 _lhsIoutputfile
         _rhsOnested = rule237 _lhsInested
         _rhsOoptions = rule238 _lhsIoptions
         _rhsOoutputfile = rule239 _lhsIoutputfile
         !__result_ = T_Expr_vOut25 _lhsOpp
         in __result_ )
     in C_Expr_s26 v25
   {-# INLINE rule229 #-}
   {-# LINE 188 "./src-ag/PrintCode.ag" #-}
   rule229 = \ ((_exprIpp) :: PP_Doc) ((_leftIpp) :: PP_Doc) ((_lhsIoptions) :: Options) ((_rhsIpp) :: PP_Doc) nt_ ->
                               {-# LINE 188 "./src-ag/PrintCode.ag" #-}
                               if breadthFirst _lhsIoptions
                               then pp_parens ("resume" >#< pp_parens _exprIpp
                                              >-< indent 2 (pp_parens ( "\\" >|<
                                                    pp_parens ("~" >|< pp_parens (nt_ >|< "_Syn" >#< "_inh_arg"))
                                                      >#< "->"
                                              >-< indent 2 ( "let" >#< _leftIpp >#< "= _inh_arg"
                                              >-< indent 2 ("in" >#< _rhsIpp)
                                              ))))
                               else pp_parens ( "case" >#< pp_parens _exprIpp >#< "of"
                                              >-< ("{" >#< _leftIpp >#< "->")
                                              >-< indent 4 (_rhsIpp >#< "}")
                                              )
                               {-# LINE 2148 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule230 #-}
   {-# LINE 420 "./src-ag/PrintCode.ag" #-}
   rule230 = \  (_ :: ()) ->
                           {-# LINE 420 "./src-ag/PrintCode.ag" #-}
                           False
                           {-# LINE 2154 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule231 #-}
   rule231 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule232 #-}
   rule232 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule233 #-}
   rule233 = \ ((_lhsIoutputfile) :: String) ->
     _lhsIoutputfile
   {-# INLINE rule234 #-}
   rule234 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule235 #-}
   rule235 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule236 #-}
   rule236 = \ ((_lhsIoutputfile) :: String) ->
     _lhsIoutputfile
   {-# INLINE rule237 #-}
   rule237 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule238 #-}
   rule238 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule239 #-}
   rule239 = \ ((_lhsIoutputfile) :: String) ->
     _lhsIoutputfile
{-# NOINLINE sem_Expr_SemFun #-}
sem_Expr_SemFun :: (String) -> T_Exprs  -> T_Expr  -> T_Expr 
sem_Expr_SemFun !arg_nt_ arg_args_ arg_body_ = T_Expr (return st26) where
   {-# NOINLINE st26 #-}
   !st26 = let
      v25 :: T_Expr_v25 
      v25 = \ !(T_Expr_vIn25 _lhsInested _lhsIoptions _lhsIoutputfile) -> ( let
         _argsX29 = Control.Monad.Identity.runIdentity (attach_T_Exprs (arg_args_))
         _bodyX26 = Control.Monad.Identity.runIdentity (attach_T_Expr (arg_body_))
         (T_Exprs_vOut28 _argsIpps) = inv_Exprs_s29 _argsX29 (T_Exprs_vIn28 _argsOnested _argsOoptions _argsOoutputfile)
         (T_Expr_vOut25 _bodyIpp) = inv_Expr_s26 _bodyX26 (T_Expr_vIn25 _bodyOnested _bodyOoptions _bodyOoutputfile)
         _strictParams = rule240 _argsIpps _lhsIoptions
         _addBang = rule241 _lhsIoptions
         _lhsOpp :: PP_Doc
         _lhsOpp = rule242 _addBang _argsIpps _bodyIpp _lhsIoptions _strictParams arg_nt_
         _argsOnested = rule243 _lhsInested
         _argsOoptions = rule244 _lhsIoptions
         _argsOoutputfile = rule245 _lhsIoutputfile
         _bodyOnested = rule246 _lhsInested
         _bodyOoptions = rule247 _lhsIoptions
         _bodyOoutputfile = rule248 _lhsIoutputfile
         !__result_ = T_Expr_vOut25 _lhsOpp
         in __result_ )
     in C_Expr_s26 v25
   {-# INLINE rule240 #-}
   {-# LINE 200 "./src-ag/PrintCode.ag" #-}
   rule240 = \ ((_argsIpps) :: PP_Docs) ((_lhsIoptions) :: Options) ->
                                    {-# LINE 200 "./src-ag/PrintCode.ag" #-}
                                    if strictSems _lhsIoptions
                                    then _argsIpps
                                    else []
                                    {-# LINE 2213 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule241 #-}
   {-# LINE 203 "./src-ag/PrintCode.ag" #-}
   rule241 = \ ((_lhsIoptions) :: Options) ->
                               {-# LINE 203 "./src-ag/PrintCode.ag" #-}
                               if bangpats _lhsIoptions
                               then \p -> pp_parens ("!" >|< p)
                               else id
                               {-# LINE 2221 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule242 #-}
   {-# LINE 206 "./src-ag/PrintCode.ag" #-}
   rule242 = \ _addBang ((_argsIpps) :: PP_Docs) ((_bodyIpp) :: PP_Doc) ((_lhsIoptions) :: Options) _strictParams nt_ ->
                               {-# LINE 206 "./src-ag/PrintCode.ag" #-}
                               if breadthFirst _lhsIoptions
                               then "Child" >#< pp_parens ( "\\" >|<
                                        pp_parens (nt_ >|< "_Inh" >#<
                                          ppTuple False (map _addBang     _argsIpps)) >#< "->"
                                    >-< indent 2 (_strictParams     `ppMultiSeqV` _bodyIpp))
                               else if null _argsIpps
                                    then _bodyIpp
                                    else pp_parens (    "\\" >#< (vlist (map _addBang     _argsIpps)) >#< "->"
                                                   >-< indent 4 (_strictParams     `ppMultiSeqV` _bodyIpp)
                                                   )
                               {-# LINE 2236 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule243 #-}
   rule243 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule244 #-}
   rule244 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule245 #-}
   rule245 = \ ((_lhsIoutputfile) :: String) ->
     _lhsIoutputfile
   {-# INLINE rule246 #-}
   rule246 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule247 #-}
   rule247 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule248 #-}
   rule248 = \ ((_lhsIoutputfile) :: String) ->
     _lhsIoutputfile

-- Exprs -------------------------------------------------------
-- wrapper
data Inh_Exprs  = Inh_Exprs { nested_Inh_Exprs :: !(Bool), options_Inh_Exprs :: !(Options), outputfile_Inh_Exprs :: !(String) }
data Syn_Exprs  = Syn_Exprs { pps_Syn_Exprs :: !(PP_Docs) }
{-# INLINABLE wrap_Exprs #-}
wrap_Exprs :: T_Exprs  -> Inh_Exprs  -> (Syn_Exprs )
wrap_Exprs !(T_Exprs act) !(Inh_Exprs _lhsInested _lhsIoptions _lhsIoutputfile) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_Exprs_vIn28 _lhsInested _lhsIoptions _lhsIoutputfile
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
data T_Exprs_vIn28  = T_Exprs_vIn28 (Bool) (Options) (String)
data T_Exprs_vOut28  = T_Exprs_vOut28 (PP_Docs)
{-# NOINLINE sem_Exprs_Cons #-}
sem_Exprs_Cons :: T_Expr  -> T_Exprs  -> T_Exprs 
sem_Exprs_Cons arg_hd_ arg_tl_ = T_Exprs (return st29) where
   {-# NOINLINE st29 #-}
   !st29 = let
      v28 :: T_Exprs_v28 
      v28 = \ !(T_Exprs_vIn28 _lhsInested _lhsIoptions _lhsIoutputfile) -> ( let
         _hdX26 = Control.Monad.Identity.runIdentity (attach_T_Expr (arg_hd_))
         _tlX29 = Control.Monad.Identity.runIdentity (attach_T_Exprs (arg_tl_))
         (T_Expr_vOut25 _hdIpp) = inv_Expr_s26 _hdX26 (T_Expr_vIn25 _hdOnested _hdOoptions _hdOoutputfile)
         (T_Exprs_vOut28 _tlIpps) = inv_Exprs_s29 _tlX29 (T_Exprs_vIn28 _tlOnested _tlOoptions _tlOoutputfile)
         _lhsOpps :: PP_Docs
         _lhsOpps = rule249 _hdIpp _tlIpps
         _hdOnested = rule250 _lhsInested
         _hdOoptions = rule251 _lhsIoptions
         _hdOoutputfile = rule252 _lhsIoutputfile
         _tlOnested = rule253 _lhsInested
         _tlOoptions = rule254 _lhsIoptions
         _tlOoutputfile = rule255 _lhsIoutputfile
         !__result_ = T_Exprs_vOut28 _lhsOpps
         in __result_ )
     in C_Exprs_s29 v28
   {-# INLINE rule249 #-}
   {-# LINE 64 "./src-ag/PrintCode.ag" #-}
   rule249 = \ ((_hdIpp) :: PP_Doc) ((_tlIpps) :: PP_Docs) ->
                     {-# LINE 64 "./src-ag/PrintCode.ag" #-}
                     _hdIpp : _tlIpps
                     {-# LINE 2313 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule250 #-}
   rule250 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule251 #-}
   rule251 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule252 #-}
   rule252 = \ ((_lhsIoutputfile) :: String) ->
     _lhsIoutputfile
   {-# INLINE rule253 #-}
   rule253 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule254 #-}
   rule254 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule255 #-}
   rule255 = \ ((_lhsIoutputfile) :: String) ->
     _lhsIoutputfile
{-# NOINLINE sem_Exprs_Nil #-}
sem_Exprs_Nil ::  T_Exprs 
sem_Exprs_Nil  = T_Exprs (return st29) where
   {-# NOINLINE st29 #-}
   !st29 = let
      v28 :: T_Exprs_v28 
      v28 = \ !(T_Exprs_vIn28 _lhsInested _lhsIoptions _lhsIoutputfile) -> ( let
         _lhsOpps :: PP_Docs
         _lhsOpps = rule256  ()
         !__result_ = T_Exprs_vOut28 _lhsOpps
         in __result_ )
     in C_Exprs_s29 v28
   {-# INLINE rule256 #-}
   {-# LINE 65 "./src-ag/PrintCode.ag" #-}
   rule256 = \  (_ :: ()) ->
                     {-# LINE 65 "./src-ag/PrintCode.ag" #-}
                     []
                     {-# LINE 2349 "dist/build/PrintCode.hs"#-}

-- Lhs ---------------------------------------------------------
-- wrapper
data Inh_Lhs  = Inh_Lhs { isDeclOfLet_Inh_Lhs :: !(Bool), nested_Inh_Lhs :: !(Bool), options_Inh_Lhs :: !(Options), outputfile_Inh_Lhs :: !(String) }
data Syn_Lhs  = Syn_Lhs { pp_Syn_Lhs :: !(PP_Doc) }
{-# INLINABLE wrap_Lhs #-}
wrap_Lhs :: T_Lhs  -> Inh_Lhs  -> (Syn_Lhs )
wrap_Lhs !(T_Lhs act) !(Inh_Lhs _lhsIisDeclOfLet _lhsInested _lhsIoptions _lhsIoutputfile) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_Lhs_vIn31 _lhsIisDeclOfLet _lhsInested _lhsIoptions _lhsIoutputfile
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
data T_Lhs_vIn31  = T_Lhs_vIn31 (Bool) (Bool) (Options) (String)
data T_Lhs_vOut31  = T_Lhs_vOut31 (PP_Doc)
{-# NOINLINE sem_Lhs_Pattern3 #-}
sem_Lhs_Pattern3 :: T_Pattern  -> T_Lhs 
sem_Lhs_Pattern3 arg_pat3_ = T_Lhs (return st32) where
   {-# NOINLINE st32 #-}
   !st32 = let
      v31 :: T_Lhs_v31 
      v31 = \ !(T_Lhs_vIn31 _lhsIisDeclOfLet _lhsInested _lhsIoptions _lhsIoutputfile) -> ( let
         _pat3X41 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pat3_))
         (T_Pattern_vOut40 _pat3Icopy _pat3IisUnderscore _pat3Ipp _pat3Ipp' _pat3IstrictVars) = inv_Pattern_s41 _pat3X41 (T_Pattern_vIn40 _pat3ObelowIrrefutable _pat3OisDeclOfLet _pat3Ooptions)
         _addStrictGuard = rule257 _hasStrictVars _lhsIoptions _strictGuard
         _strictGuard = rule258 _pat3IstrictVars
         _hasStrictVars = rule259 _pat3IstrictVars
         _lhsOpp :: PP_Doc
         _lhsOpp = rule260 _addStrictGuard _pat3Ipp
         _pat3ObelowIrrefutable = rule261  ()
         _pat3OisDeclOfLet = rule262 _lhsIisDeclOfLet
         _pat3Ooptions = rule263 _lhsIoptions
         !__result_ = T_Lhs_vOut31 _lhsOpp
         in __result_ )
     in C_Lhs_s32 v31
   {-# INLINE rule257 #-}
   {-# LINE 231 "./src-ag/PrintCode.ag" #-}
   rule257 = \ _hasStrictVars ((_lhsIoptions) :: Options) _strictGuard ->
                             {-# LINE 231 "./src-ag/PrintCode.ag" #-}
                             if strictCases _lhsIoptions && _hasStrictVars     then \v -> v >#< "|" >#< _strictGuard     else id
                             {-# LINE 2411 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule258 #-}
   {-# LINE 233 "./src-ag/PrintCode.ag" #-}
   rule258 = \ ((_pat3IstrictVars) :: [PP_Doc]) ->
                          {-# LINE 233 "./src-ag/PrintCode.ag" #-}
                          _pat3IstrictVars `ppMultiSeqH` (pp "True")
                          {-# LINE 2417 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule259 #-}
   {-# LINE 234 "./src-ag/PrintCode.ag" #-}
   rule259 = \ ((_pat3IstrictVars) :: [PP_Doc]) ->
                            {-# LINE 234 "./src-ag/PrintCode.ag" #-}
                            not (null _pat3IstrictVars)
                            {-# LINE 2423 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule260 #-}
   {-# LINE 251 "./src-ag/PrintCode.ag" #-}
   rule260 = \ _addStrictGuard ((_pat3Ipp) :: PP_Doc) ->
                               {-# LINE 251 "./src-ag/PrintCode.ag" #-}
                               _addStrictGuard     _pat3Ipp
                               {-# LINE 2429 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule261 #-}
   {-# LINE 381 "./src-ag/PrintCode.ag" #-}
   rule261 = \  (_ :: ()) ->
                                {-# LINE 381 "./src-ag/PrintCode.ag" #-}
                                False
                                {-# LINE 2435 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule262 #-}
   rule262 = \ ((_lhsIisDeclOfLet) :: Bool) ->
     _lhsIisDeclOfLet
   {-# INLINE rule263 #-}
   rule263 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Lhs_Pattern3SM #-}
sem_Lhs_Pattern3SM :: T_Pattern  -> T_Lhs 
sem_Lhs_Pattern3SM arg_pat3_ = T_Lhs (return st32) where
   {-# NOINLINE st32 #-}
   !st32 = let
      v31 :: T_Lhs_v31 
      v31 = \ !(T_Lhs_vIn31 _lhsIisDeclOfLet _lhsInested _lhsIoptions _lhsIoutputfile) -> ( let
         _pat3X41 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pat3_))
         (T_Pattern_vOut40 _pat3Icopy _pat3IisUnderscore _pat3Ipp _pat3Ipp' _pat3IstrictVars) = inv_Pattern_s41 _pat3X41 (T_Pattern_vIn40 _pat3ObelowIrrefutable _pat3OisDeclOfLet _pat3Ooptions)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule264 _pat3Ipp'
         _pat3ObelowIrrefutable = rule265  ()
         _pat3OisDeclOfLet = rule266 _lhsIisDeclOfLet
         _pat3Ooptions = rule267 _lhsIoptions
         !__result_ = T_Lhs_vOut31 _lhsOpp
         in __result_ )
     in C_Lhs_s32 v31
   {-# INLINE rule264 #-}
   {-# LINE 252 "./src-ag/PrintCode.ag" #-}
   rule264 = \ ((_pat3Ipp') :: PP_Doc) ->
                               {-# LINE 252 "./src-ag/PrintCode.ag" #-}
                               _pat3Ipp'
                               {-# LINE 2464 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule265 #-}
   {-# LINE 381 "./src-ag/PrintCode.ag" #-}
   rule265 = \  (_ :: ()) ->
                                {-# LINE 381 "./src-ag/PrintCode.ag" #-}
                                False
                                {-# LINE 2470 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule266 #-}
   rule266 = \ ((_lhsIisDeclOfLet) :: Bool) ->
     _lhsIisDeclOfLet
   {-# INLINE rule267 #-}
   rule267 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Lhs_TupleLhs #-}
sem_Lhs_TupleLhs :: ([String]) -> T_Lhs 
sem_Lhs_TupleLhs !arg_comps_ = T_Lhs (return st32) where
   {-# NOINLINE st32 #-}
   !st32 = let
      v31 :: T_Lhs_v31 
      v31 = \ !(T_Lhs_vIn31 _lhsIisDeclOfLet _lhsInested _lhsIoptions _lhsIoutputfile) -> ( let
         _addStrictGuard = rule268 _hasStrictVars _lhsIoptions _strictGuard
         _strictGuard = rule269 _lhsIisDeclOfLet _lhsIoptions arg_comps_
         _hasStrictVars = rule270 arg_comps_
         _addBang = rule271 _lhsIoptions
         _lhsOpp :: PP_Doc
         _lhsOpp = rule272 _addBang _addStrictGuard _lhsInested arg_comps_
         !__result_ = T_Lhs_vOut31 _lhsOpp
         in __result_ )
     in C_Lhs_s32 v31
   {-# INLINE rule268 #-}
   {-# LINE 231 "./src-ag/PrintCode.ag" #-}
   rule268 = \ _hasStrictVars ((_lhsIoptions) :: Options) _strictGuard ->
                             {-# LINE 231 "./src-ag/PrintCode.ag" #-}
                             if strictCases _lhsIoptions && _hasStrictVars     then \v -> v >#< "|" >#< _strictGuard     else id
                             {-# LINE 2498 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule269 #-}
   {-# LINE 236 "./src-ag/PrintCode.ag" #-}
   rule269 = \ ((_lhsIisDeclOfLet) :: Bool) ((_lhsIoptions) :: Options) comps_ ->
                          {-# LINE 236 "./src-ag/PrintCode.ag" #-}
                          if stricterCases _lhsIoptions && not _lhsIisDeclOfLet
                          then map text comps_ `ppMultiSeqH` (pp "True")
                          else pp "True"
                          {-# LINE 2506 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule270 #-}
   {-# LINE 239 "./src-ag/PrintCode.ag" #-}
   rule270 = \ comps_ ->
                            {-# LINE 239 "./src-ag/PrintCode.ag" #-}
                            not (null comps_)
                            {-# LINE 2512 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule271 #-}
   {-# LINE 247 "./src-ag/PrintCode.ag" #-}
   rule271 = \ ((_lhsIoptions) :: Options) ->
                      {-# LINE 247 "./src-ag/PrintCode.ag" #-}
                      if bangpats _lhsIoptions
                               then \p -> "!" >|< p
                               else id
                      {-# LINE 2520 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule272 #-}
   {-# LINE 253 "./src-ag/PrintCode.ag" #-}
   rule272 = \ _addBang _addStrictGuard ((_lhsInested) :: Bool) comps_ ->
                               {-# LINE 253 "./src-ag/PrintCode.ag" #-}
                               _addStrictGuard     $ ppTuple _lhsInested (map (_addBang     . text) comps_)
                               {-# LINE 2526 "dist/build/PrintCode.hs"#-}
{-# NOINLINE sem_Lhs_UnboxedTupleLhs #-}
sem_Lhs_UnboxedTupleLhs :: ([String]) -> T_Lhs 
sem_Lhs_UnboxedTupleLhs !arg_comps_ = T_Lhs (return st32) where
   {-# NOINLINE st32 #-}
   !st32 = let
      v31 :: T_Lhs_v31 
      v31 = \ !(T_Lhs_vIn31 _lhsIisDeclOfLet _lhsInested _lhsIoptions _lhsIoutputfile) -> ( let
         _addStrictGuard = rule273 _hasStrictVars _lhsIoptions _strictGuard
         _strictGuard = rule274 _lhsIisDeclOfLet _lhsIoptions arg_comps_
         _hasStrictVars = rule275 arg_comps_
         _addBang = rule276 _lhsIoptions
         _lhsOpp :: PP_Doc
         _lhsOpp = rule277 _addBang _addStrictGuard _lhsInested arg_comps_
         !__result_ = T_Lhs_vOut31 _lhsOpp
         in __result_ )
     in C_Lhs_s32 v31
   {-# INLINE rule273 #-}
   {-# LINE 231 "./src-ag/PrintCode.ag" #-}
   rule273 = \ _hasStrictVars ((_lhsIoptions) :: Options) _strictGuard ->
                             {-# LINE 231 "./src-ag/PrintCode.ag" #-}
                             if strictCases _lhsIoptions && _hasStrictVars     then \v -> v >#< "|" >#< _strictGuard     else id
                             {-# LINE 2548 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule274 #-}
   {-# LINE 236 "./src-ag/PrintCode.ag" #-}
   rule274 = \ ((_lhsIisDeclOfLet) :: Bool) ((_lhsIoptions) :: Options) comps_ ->
                          {-# LINE 236 "./src-ag/PrintCode.ag" #-}
                          if stricterCases _lhsIoptions && not _lhsIisDeclOfLet
                          then map text comps_ `ppMultiSeqH` (pp "True")
                          else pp "True"
                          {-# LINE 2556 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule275 #-}
   {-# LINE 239 "./src-ag/PrintCode.ag" #-}
   rule275 = \ comps_ ->
                            {-# LINE 239 "./src-ag/PrintCode.ag" #-}
                            not (null comps_)
                            {-# LINE 2562 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule276 #-}
   {-# LINE 247 "./src-ag/PrintCode.ag" #-}
   rule276 = \ ((_lhsIoptions) :: Options) ->
                      {-# LINE 247 "./src-ag/PrintCode.ag" #-}
                      if bangpats _lhsIoptions
                               then \p -> "!" >|< p
                               else id
                      {-# LINE 2570 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule277 #-}
   {-# LINE 254 "./src-ag/PrintCode.ag" #-}
   rule277 = \ _addBang _addStrictGuard ((_lhsInested) :: Bool) comps_ ->
                                      {-# LINE 254 "./src-ag/PrintCode.ag" #-}
                                      _addStrictGuard     $ ppUnboxedTuple _lhsInested (map (_addBang     . text) comps_)
                                      {-# LINE 2576 "dist/build/PrintCode.hs"#-}
{-# NOINLINE sem_Lhs_Fun #-}
sem_Lhs_Fun :: (String) -> T_Exprs  -> T_Lhs 
sem_Lhs_Fun !arg_name_ arg_args_ = T_Lhs (return st32) where
   {-# NOINLINE st32 #-}
   !st32 = let
      v31 :: T_Lhs_v31 
      v31 = \ !(T_Lhs_vIn31 _lhsIisDeclOfLet _lhsInested _lhsIoptions _lhsIoutputfile) -> ( let
         _argsX29 = Control.Monad.Identity.runIdentity (attach_T_Exprs (arg_args_))
         (T_Exprs_vOut28 _argsIpps) = inv_Exprs_s29 _argsX29 (T_Exprs_vIn28 _argsOnested _argsOoptions _argsOoutputfile)
         _addStrictGuard = rule278 _hasStrictVars _lhsIoptions _strictGuard
         _hasStrictVars = rule279 _argsIpps
         _strictGuard = rule280 _argsIpps
         _addBang = rule281 _lhsIoptions
         _lhsOpp :: PP_Doc
         _lhsOpp = rule282 _addBang _addStrictGuard _argsIpps arg_name_
         _argsOnested = rule283 _lhsInested
         _argsOoptions = rule284 _lhsIoptions
         _argsOoutputfile = rule285 _lhsIoutputfile
         !__result_ = T_Lhs_vOut31 _lhsOpp
         in __result_ )
     in C_Lhs_s32 v31
   {-# INLINE rule278 #-}
   {-# LINE 242 "./src-ag/PrintCode.ag" #-}
   rule278 = \ _hasStrictVars ((_lhsIoptions) :: Options) _strictGuard ->
                             {-# LINE 242 "./src-ag/PrintCode.ag" #-}
                             if strictSems _lhsIoptions && _hasStrictVars     then \v -> v >#< "|" >#< _strictGuard     else id
                             {-# LINE 2603 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule279 #-}
   {-# LINE 243 "./src-ag/PrintCode.ag" #-}
   rule279 = \ ((_argsIpps) :: PP_Docs) ->
                             {-# LINE 243 "./src-ag/PrintCode.ag" #-}
                             not (null _argsIpps)
                             {-# LINE 2609 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule280 #-}
   {-# LINE 244 "./src-ag/PrintCode.ag" #-}
   rule280 = \ ((_argsIpps) :: PP_Docs) ->
                             {-# LINE 244 "./src-ag/PrintCode.ag" #-}
                             _argsIpps `ppMultiSeqH` (pp "True")
                             {-# LINE 2615 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule281 #-}
   {-# LINE 247 "./src-ag/PrintCode.ag" #-}
   rule281 = \ ((_lhsIoptions) :: Options) ->
                      {-# LINE 247 "./src-ag/PrintCode.ag" #-}
                      if bangpats _lhsIoptions
                               then \p -> "!" >|< p
                               else id
                      {-# LINE 2623 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule282 #-}
   {-# LINE 255 "./src-ag/PrintCode.ag" #-}
   rule282 = \ _addBang _addStrictGuard ((_argsIpps) :: PP_Docs) name_ ->
                               {-# LINE 255 "./src-ag/PrintCode.ag" #-}
                               _addStrictGuard     (name_ >#< hv_sp (map _addBang     _argsIpps))
                               {-# LINE 2629 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule283 #-}
   rule283 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule284 #-}
   rule284 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule285 #-}
   rule285 = \ ((_lhsIoutputfile) :: String) ->
     _lhsIoutputfile
{-# NOINLINE sem_Lhs_Unwrap #-}
sem_Lhs_Unwrap :: (String) -> T_Lhs  -> T_Lhs 
sem_Lhs_Unwrap !arg_name_ arg_sub_ = T_Lhs (return st32) where
   {-# NOINLINE st32 #-}
   !st32 = let
      v31 :: T_Lhs_v31 
      v31 = \ !(T_Lhs_vIn31 _lhsIisDeclOfLet _lhsInested _lhsIoptions _lhsIoutputfile) -> ( let
         _subX32 = Control.Monad.Identity.runIdentity (attach_T_Lhs (arg_sub_))
         (T_Lhs_vOut31 _subIpp) = inv_Lhs_s32 _subX32 (T_Lhs_vIn31 _subOisDeclOfLet _subOnested _subOoptions _subOoutputfile)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule286 _subIpp arg_name_
         _subOisDeclOfLet = rule287 _lhsIisDeclOfLet
         _subOnested = rule288 _lhsInested
         _subOoptions = rule289 _lhsIoptions
         _subOoutputfile = rule290 _lhsIoutputfile
         !__result_ = T_Lhs_vOut31 _lhsOpp
         in __result_ )
     in C_Lhs_s32 v31
   {-# INLINE rule286 #-}
   {-# LINE 256 "./src-ag/PrintCode.ag" #-}
   rule286 = \ ((_subIpp) :: PP_Doc) name_ ->
                               {-# LINE 256 "./src-ag/PrintCode.ag" #-}
                               pp_parens (name_ >#< _subIpp)
                               {-# LINE 2662 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule287 #-}
   rule287 = \ ((_lhsIisDeclOfLet) :: Bool) ->
     _lhsIisDeclOfLet
   {-# INLINE rule288 #-}
   rule288 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule289 #-}
   rule289 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule290 #-}
   rule290 = \ ((_lhsIoutputfile) :: String) ->
     _lhsIoutputfile

-- NamedType ---------------------------------------------------
-- wrapper
data Inh_NamedType  = Inh_NamedType { nested_Inh_NamedType :: !(Bool) }
data Syn_NamedType  = Syn_NamedType { pp_Syn_NamedType :: !(PP_Doc) }
{-# INLINABLE wrap_NamedType #-}
wrap_NamedType :: T_NamedType  -> Inh_NamedType  -> (Syn_NamedType )
wrap_NamedType !(T_NamedType act) !(Inh_NamedType _lhsInested) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_NamedType_vIn34 _lhsInested
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
data T_NamedType_vIn34  = T_NamedType_vIn34 (Bool)
data T_NamedType_vOut34  = T_NamedType_vOut34 (PP_Doc)
{-# NOINLINE sem_NamedType_Named #-}
sem_NamedType_Named :: (Bool) -> (String) -> T_Type  -> T_NamedType 
sem_NamedType_Named !arg_strict_ !arg_name_ arg_tp_ = T_NamedType (return st35) where
   {-# NOINLINE st35 #-}
   !st35 = let
      v34 :: T_NamedType_v34 
      v34 = \ !(T_NamedType_vIn34 _lhsInested) -> ( let
         _tpX50 = Control.Monad.Identity.runIdentity (attach_T_Type (arg_tp_))
         (T_Type_vOut49 _tpIpp _tpIprec) = inv_Type_s50 _tpX50 (T_Type_vIn49 _tpOnested)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule291 _tpIpp arg_name_ arg_strict_
         _tpOnested = rule292 _lhsInested
         !__result_ = T_NamedType_vOut34 _lhsOpp
         in __result_ )
     in C_NamedType_s35 v34
   {-# INLINE rule291 #-}
   {-# LINE 225 "./src-ag/PrintCode.ag" #-}
   rule291 = \ ((_tpIpp) :: PP_Doc) name_ strict_ ->
                               {-# LINE 225 "./src-ag/PrintCode.ag" #-}
                               if strict_
                               then name_ >#< "::" >#< "!" >|< pp_parens _tpIpp
                               else name_ >#< "::" >#< _tpIpp
                               {-# LINE 2728 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule292 #-}
   rule292 = \ ((_lhsInested) :: Bool) ->
     _lhsInested

-- NamedTypes --------------------------------------------------
-- wrapper
data Inh_NamedTypes  = Inh_NamedTypes { nested_Inh_NamedTypes :: !(Bool) }
data Syn_NamedTypes  = Syn_NamedTypes { pps_Syn_NamedTypes :: !(PP_Docs) }
{-# INLINABLE wrap_NamedTypes #-}
wrap_NamedTypes :: T_NamedTypes  -> Inh_NamedTypes  -> (Syn_NamedTypes )
wrap_NamedTypes !(T_NamedTypes act) !(Inh_NamedTypes _lhsInested) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_NamedTypes_vIn37 _lhsInested
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
data T_NamedTypes_vIn37  = T_NamedTypes_vIn37 (Bool)
data T_NamedTypes_vOut37  = T_NamedTypes_vOut37 (PP_Docs)
{-# NOINLINE sem_NamedTypes_Cons #-}
sem_NamedTypes_Cons :: T_NamedType  -> T_NamedTypes  -> T_NamedTypes 
sem_NamedTypes_Cons arg_hd_ arg_tl_ = T_NamedTypes (return st38) where
   {-# NOINLINE st38 #-}
   !st38 = let
      v37 :: T_NamedTypes_v37 
      v37 = \ !(T_NamedTypes_vIn37 _lhsInested) -> ( let
         _hdX35 = Control.Monad.Identity.runIdentity (attach_T_NamedType (arg_hd_))
         _tlX38 = Control.Monad.Identity.runIdentity (attach_T_NamedTypes (arg_tl_))
         (T_NamedType_vOut34 _hdIpp) = inv_NamedType_s35 _hdX35 (T_NamedType_vIn34 _hdOnested)
         (T_NamedTypes_vOut37 _tlIpps) = inv_NamedTypes_s38 _tlX38 (T_NamedTypes_vIn37 _tlOnested)
         _lhsOpps :: PP_Docs
         _lhsOpps = rule293 _hdIpp _tlIpps
         _hdOnested = rule294 _lhsInested
         _tlOnested = rule295 _lhsInested
         !__result_ = T_NamedTypes_vOut37 _lhsOpps
         in __result_ )
     in C_NamedTypes_s38 v37
   {-# INLINE rule293 #-}
   {-# LINE 80 "./src-ag/PrintCode.ag" #-}
   rule293 = \ ((_hdIpp) :: PP_Doc) ((_tlIpps) :: PP_Docs) ->
                     {-# LINE 80 "./src-ag/PrintCode.ag" #-}
                     _hdIpp : _tlIpps
                     {-# LINE 2786 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule294 #-}
   rule294 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule295 #-}
   rule295 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
{-# NOINLINE sem_NamedTypes_Nil #-}
sem_NamedTypes_Nil ::  T_NamedTypes 
sem_NamedTypes_Nil  = T_NamedTypes (return st38) where
   {-# NOINLINE st38 #-}
   !st38 = let
      v37 :: T_NamedTypes_v37 
      v37 = \ !(T_NamedTypes_vIn37 _lhsInested) -> ( let
         _lhsOpps :: PP_Docs
         _lhsOpps = rule296  ()
         !__result_ = T_NamedTypes_vOut37 _lhsOpps
         in __result_ )
     in C_NamedTypes_s38 v37
   {-# INLINE rule296 #-}
   {-# LINE 81 "./src-ag/PrintCode.ag" #-}
   rule296 = \  (_ :: ()) ->
                     {-# LINE 81 "./src-ag/PrintCode.ag" #-}
                     []
                     {-# LINE 2810 "dist/build/PrintCode.hs"#-}

-- Pattern -----------------------------------------------------
-- wrapper
data Inh_Pattern  = Inh_Pattern { belowIrrefutable_Inh_Pattern :: !(Bool), isDeclOfLet_Inh_Pattern :: !(Bool), options_Inh_Pattern :: !(Options) }
data Syn_Pattern  = Syn_Pattern { copy_Syn_Pattern :: !(Pattern), isUnderscore_Syn_Pattern :: !(Bool), pp_Syn_Pattern :: !(PP_Doc), pp'_Syn_Pattern :: !(PP_Doc), strictVars_Syn_Pattern :: !([PP_Doc]) }
{-# INLINABLE wrap_Pattern #-}
wrap_Pattern :: T_Pattern  -> Inh_Pattern  -> (Syn_Pattern )
wrap_Pattern !(T_Pattern act) !(Inh_Pattern _lhsIbelowIrrefutable _lhsIisDeclOfLet _lhsIoptions) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_Pattern_vIn40 _lhsIbelowIrrefutable _lhsIisDeclOfLet _lhsIoptions
        !(T_Pattern_vOut40 _lhsOcopy _lhsOisUnderscore _lhsOpp _lhsOpp' _lhsOstrictVars) <- return (inv_Pattern_s41 sem arg)
        return (Syn_Pattern _lhsOcopy _lhsOisUnderscore _lhsOpp _lhsOpp' _lhsOstrictVars)
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
data T_Pattern_vIn40  = T_Pattern_vIn40 (Bool) (Bool) (Options)
data T_Pattern_vOut40  = T_Pattern_vOut40 (Pattern) (Bool) (PP_Doc) (PP_Doc) ([PP_Doc])
{-# NOINLINE sem_Pattern_Constr #-}
sem_Pattern_Constr :: (ConstructorIdent) -> T_Patterns  -> T_Pattern 
sem_Pattern_Constr !arg_name_ arg_pats_ = T_Pattern (return st41) where
   {-# NOINLINE st41 #-}
   !st41 = let
      v40 :: T_Pattern_v40 
      v40 = \ !(T_Pattern_vIn40 _lhsIbelowIrrefutable _lhsIisDeclOfLet _lhsIoptions) -> ( let
         _patsX44 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_pats_))
         (T_Patterns_vOut43 _patsIcopy _patsIpps _patsIpps' _patsIstrictVars) = inv_Patterns_s44 _patsX44 (T_Patterns_vIn43 _patsObelowIrrefutable _patsOisDeclOfLet _patsOoptions)
         _addBang = rule297 _lhsIbelowIrrefutable _lhsIisDeclOfLet _lhsIoptions
         _lhsOpp :: PP_Doc
         _lhsOpp = rule298 _addBang _patsIpps arg_name_
         _lhsOisUnderscore :: Bool
         _lhsOisUnderscore = rule299  ()
         _lhsOpp' :: PP_Doc
         _lhsOpp' = rule300 _patsIpps' arg_name_
         _lhsOstrictVars :: [PP_Doc]
         _lhsOstrictVars = rule301 _patsIstrictVars
         _copy = rule302 _patsIcopy arg_name_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule303 _copy
         _patsObelowIrrefutable = rule304 _lhsIbelowIrrefutable
         _patsOisDeclOfLet = rule305 _lhsIisDeclOfLet
         _patsOoptions = rule306 _lhsIoptions
         !__result_ = T_Pattern_vOut40 _lhsOcopy _lhsOisUnderscore _lhsOpp _lhsOpp' _lhsOstrictVars
         in __result_ )
     in C_Pattern_s41 v40
   {-# INLINE rule297 #-}
   {-# LINE 353 "./src-ag/PrintCode.ag" #-}
   rule297 = \ ((_lhsIbelowIrrefutable) :: Bool) ((_lhsIisDeclOfLet) :: Bool) ((_lhsIoptions) :: Options) ->
                      {-# LINE 353 "./src-ag/PrintCode.ag" #-}
                      if bangpats _lhsIoptions && not _lhsIisDeclOfLet && not _lhsIbelowIrrefutable
                      then \p -> "!" >|< p
                      else id
                      {-# LINE 2880 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule298 #-}
   {-# LINE 358 "./src-ag/PrintCode.ag" #-}
   rule298 = \ _addBang ((_patsIpps) :: [PP_Doc]) name_ ->
                           {-# LINE 358 "./src-ag/PrintCode.ag" #-}
                           _addBang     $ pp_parens $ name_ >#< hv_sp _patsIpps
                           {-# LINE 2886 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule299 #-}
   {-# LINE 369 "./src-ag/PrintCode.ag" #-}
   rule299 = \  (_ :: ()) ->
                                    {-# LINE 369 "./src-ag/PrintCode.ag" #-}
                                    False
                                    {-# LINE 2892 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule300 #-}
   {-# LINE 392 "./src-ag/PrintCode.ag" #-}
   rule300 = \ ((_patsIpps') :: [PP_Doc]) name_ ->
                            {-# LINE 392 "./src-ag/PrintCode.ag" #-}
                            pp_parens $ name_ >#< hv_sp (map pp_parens _patsIpps')
                            {-# LINE 2898 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule301 #-}
   rule301 = \ ((_patsIstrictVars) :: [PP_Doc]) ->
     _patsIstrictVars
   {-# INLINE rule302 #-}
   rule302 = \ ((_patsIcopy) :: Patterns) name_ ->
     Constr name_ _patsIcopy
   {-# INLINE rule303 #-}
   rule303 = \ _copy ->
     _copy
   {-# INLINE rule304 #-}
   rule304 = \ ((_lhsIbelowIrrefutable) :: Bool) ->
     _lhsIbelowIrrefutable
   {-# INLINE rule305 #-}
   rule305 = \ ((_lhsIisDeclOfLet) :: Bool) ->
     _lhsIisDeclOfLet
   {-# INLINE rule306 #-}
   rule306 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Pattern_Product #-}
sem_Pattern_Product :: (Pos) -> T_Patterns  -> T_Pattern 
sem_Pattern_Product !arg_pos_ arg_pats_ = T_Pattern (return st41) where
   {-# NOINLINE st41 #-}
   !st41 = let
      v40 :: T_Pattern_v40 
      v40 = \ !(T_Pattern_vIn40 _lhsIbelowIrrefutable _lhsIisDeclOfLet _lhsIoptions) -> ( let
         _patsX44 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_pats_))
         (T_Patterns_vOut43 _patsIcopy _patsIpps _patsIpps' _patsIstrictVars) = inv_Patterns_s44 _patsX44 (T_Patterns_vIn43 _patsObelowIrrefutable _patsOisDeclOfLet _patsOoptions)
         _addBang = rule307 _lhsIbelowIrrefutable _lhsIisDeclOfLet _lhsIoptions
         _lhsOpp :: PP_Doc
         _lhsOpp = rule308 _addBang _patsIpps
         _lhsOisUnderscore :: Bool
         _lhsOisUnderscore = rule309  ()
         _lhsOpp' :: PP_Doc
         _lhsOpp' = rule310 _patsIpps'
         _lhsOstrictVars :: [PP_Doc]
         _lhsOstrictVars = rule311 _patsIstrictVars
         _copy = rule312 _patsIcopy arg_pos_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule313 _copy
         _patsObelowIrrefutable = rule314 _lhsIbelowIrrefutable
         _patsOisDeclOfLet = rule315 _lhsIisDeclOfLet
         _patsOoptions = rule316 _lhsIoptions
         !__result_ = T_Pattern_vOut40 _lhsOcopy _lhsOisUnderscore _lhsOpp _lhsOpp' _lhsOstrictVars
         in __result_ )
     in C_Pattern_s41 v40
   {-# INLINE rule307 #-}
   {-# LINE 353 "./src-ag/PrintCode.ag" #-}
   rule307 = \ ((_lhsIbelowIrrefutable) :: Bool) ((_lhsIisDeclOfLet) :: Bool) ((_lhsIoptions) :: Options) ->
                      {-# LINE 353 "./src-ag/PrintCode.ag" #-}
                      if bangpats _lhsIoptions && not _lhsIisDeclOfLet && not _lhsIbelowIrrefutable
                      then \p -> "!" >|< p
                      else id
                      {-# LINE 2951 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule308 #-}
   {-# LINE 359 "./src-ag/PrintCode.ag" #-}
   rule308 = \ _addBang ((_patsIpps) :: [PP_Doc]) ->
                           {-# LINE 359 "./src-ag/PrintCode.ag" #-}
                           _addBang     $ pp_block "(" ")" "," _patsIpps
                           {-# LINE 2957 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule309 #-}
   {-# LINE 370 "./src-ag/PrintCode.ag" #-}
   rule309 = \  (_ :: ()) ->
                                    {-# LINE 370 "./src-ag/PrintCode.ag" #-}
                                    False
                                    {-# LINE 2963 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule310 #-}
   {-# LINE 393 "./src-ag/PrintCode.ag" #-}
   rule310 = \ ((_patsIpps') :: [PP_Doc]) ->
                            {-# LINE 393 "./src-ag/PrintCode.ag" #-}
                            pp_block "(" ")" "," _patsIpps'
                            {-# LINE 2969 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule311 #-}
   rule311 = \ ((_patsIstrictVars) :: [PP_Doc]) ->
     _patsIstrictVars
   {-# INLINE rule312 #-}
   rule312 = \ ((_patsIcopy) :: Patterns) pos_ ->
     Product pos_ _patsIcopy
   {-# INLINE rule313 #-}
   rule313 = \ _copy ->
     _copy
   {-# INLINE rule314 #-}
   rule314 = \ ((_lhsIbelowIrrefutable) :: Bool) ->
     _lhsIbelowIrrefutable
   {-# INLINE rule315 #-}
   rule315 = \ ((_lhsIisDeclOfLet) :: Bool) ->
     _lhsIisDeclOfLet
   {-# INLINE rule316 #-}
   rule316 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Pattern_Alias #-}
sem_Pattern_Alias :: (Identifier) -> (Identifier) -> T_Pattern  -> T_Pattern 
sem_Pattern_Alias !arg_field_ !arg_attr_ arg_pat_ = T_Pattern (return st41) where
   {-# NOINLINE st41 #-}
   !st41 = let
      v40 :: T_Pattern_v40 
      v40 = \ !(T_Pattern_vIn40 _lhsIbelowIrrefutable _lhsIisDeclOfLet _lhsIoptions) -> ( let
         _patX41 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pat_))
         (T_Pattern_vOut40 _patIcopy _patIisUnderscore _patIpp _patIpp' _patIstrictVars) = inv_Pattern_s41 _patX41 (T_Pattern_vIn40 _patObelowIrrefutable _patOisDeclOfLet _patOoptions)
         _strictVar = rule317 _lhsIisDeclOfLet _lhsIoptions _ppVar
         _strictPatVars = rule318 _lhsIisDeclOfLet _lhsIoptions _patIstrictVars
         _lhsOstrictVars :: [PP_Doc]
         _lhsOstrictVars = rule319 _strictPatVars _strictVar
         _addBang = rule320 _lhsIbelowIrrefutable _lhsIisDeclOfLet _lhsIoptions
         _ppVar = rule321 arg_attr_ arg_field_
         _ppVarBang = rule322 _addBang _ppVar
         _lhsOpp :: PP_Doc
         _lhsOpp = rule323 _patIisUnderscore _patIpp _ppVarBang
         _lhsOisUnderscore :: Bool
         _lhsOisUnderscore = rule324  ()
         _lhsOpp' :: PP_Doc
         _lhsOpp' = rule325 _patIpp' arg_attr_ arg_field_
         _copy = rule326 _patIcopy arg_attr_ arg_field_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule327 _copy
         _patObelowIrrefutable = rule328 _lhsIbelowIrrefutable
         _patOisDeclOfLet = rule329 _lhsIisDeclOfLet
         _patOoptions = rule330 _lhsIoptions
         !__result_ = T_Pattern_vOut40 _lhsOcopy _lhsOisUnderscore _lhsOpp _lhsOpp' _lhsOstrictVars
         in __result_ )
     in C_Pattern_s41 v40
   {-# INLINE rule317 #-}
   {-# LINE 331 "./src-ag/PrintCode.ag" #-}
   rule317 = \ ((_lhsIisDeclOfLet) :: Bool) ((_lhsIoptions) :: Options) _ppVar ->
            {-# LINE 331 "./src-ag/PrintCode.ag" #-}
            if strictCases _lhsIoptions && not _lhsIisDeclOfLet
            then [_ppVar    ]
            else []
            {-# LINE 3026 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule318 #-}
   {-# LINE 335 "./src-ag/PrintCode.ag" #-}
   rule318 = \ ((_lhsIisDeclOfLet) :: Bool) ((_lhsIoptions) :: Options) ((_patIstrictVars) :: [PP_Doc]) ->
            {-# LINE 335 "./src-ag/PrintCode.ag" #-}
            if stricterCases _lhsIoptions && not _lhsIisDeclOfLet
            then _patIstrictVars
            else []
            {-# LINE 3034 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule319 #-}
   {-# LINE 339 "./src-ag/PrintCode.ag" #-}
   rule319 = \ _strictPatVars _strictVar ->
            {-# LINE 339 "./src-ag/PrintCode.ag" #-}
            _strictVar     ++ _strictPatVars
            {-# LINE 3040 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule320 #-}
   {-# LINE 353 "./src-ag/PrintCode.ag" #-}
   rule320 = \ ((_lhsIbelowIrrefutable) :: Bool) ((_lhsIisDeclOfLet) :: Bool) ((_lhsIoptions) :: Options) ->
                      {-# LINE 353 "./src-ag/PrintCode.ag" #-}
                      if bangpats _lhsIoptions && not _lhsIisDeclOfLet && not _lhsIbelowIrrefutable
                      then \p -> "!" >|< p
                      else id
                      {-# LINE 3048 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule321 #-}
   {-# LINE 360 "./src-ag/PrintCode.ag" #-}
   rule321 = \ attr_ field_ ->
                           {-# LINE 360 "./src-ag/PrintCode.ag" #-}
                           pp (attrname False field_ attr_)
                           {-# LINE 3054 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule322 #-}
   {-# LINE 361 "./src-ag/PrintCode.ag" #-}
   rule322 = \ _addBang _ppVar ->
                              {-# LINE 361 "./src-ag/PrintCode.ag" #-}
                              _addBang     $ _ppVar
                              {-# LINE 3060 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule323 #-}
   {-# LINE 362 "./src-ag/PrintCode.ag" #-}
   rule323 = \ ((_patIisUnderscore) :: Bool) ((_patIpp) :: PP_Doc) _ppVarBang ->
                           {-# LINE 362 "./src-ag/PrintCode.ag" #-}
                           if _patIisUnderscore
                            then _ppVarBang
                            else _ppVarBang     >|< "@" >|< _patIpp
                           {-# LINE 3068 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule324 #-}
   {-# LINE 371 "./src-ag/PrintCode.ag" #-}
   rule324 = \  (_ :: ()) ->
                                    {-# LINE 371 "./src-ag/PrintCode.ag" #-}
                                    False
                                    {-# LINE 3074 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule325 #-}
   {-# LINE 394 "./src-ag/PrintCode.ag" #-}
   rule325 = \ ((_patIpp') :: PP_Doc) attr_ field_ ->
                            {-# LINE 394 "./src-ag/PrintCode.ag" #-}
                            let attribute | field_ == _LOC || field_ == nullIdent = locname' attr_
                                          | otherwise                             = attrname False field_ attr_
                            in attribute >|< "@" >|< _patIpp'
                            {-# LINE 3082 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule326 #-}
   rule326 = \ ((_patIcopy) :: Pattern) attr_ field_ ->
     Alias field_ attr_ _patIcopy
   {-# INLINE rule327 #-}
   rule327 = \ _copy ->
     _copy
   {-# INLINE rule328 #-}
   rule328 = \ ((_lhsIbelowIrrefutable) :: Bool) ->
     _lhsIbelowIrrefutable
   {-# INLINE rule329 #-}
   rule329 = \ ((_lhsIisDeclOfLet) :: Bool) ->
     _lhsIisDeclOfLet
   {-# INLINE rule330 #-}
   rule330 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Pattern_Irrefutable #-}
sem_Pattern_Irrefutable :: T_Pattern  -> T_Pattern 
sem_Pattern_Irrefutable arg_pat_ = T_Pattern (return st41) where
   {-# NOINLINE st41 #-}
   !st41 = let
      v40 :: T_Pattern_v40 
      v40 = \ !(T_Pattern_vIn40 _lhsIbelowIrrefutable _lhsIisDeclOfLet _lhsIoptions) -> ( let
         _patX41 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pat_))
         (T_Pattern_vOut40 _patIcopy _patIisUnderscore _patIpp _patIpp' _patIstrictVars) = inv_Pattern_s41 _patX41 (T_Pattern_vIn40 _patObelowIrrefutable _patOisDeclOfLet _patOoptions)
         _lhsOstrictVars :: [PP_Doc]
         _lhsOstrictVars = rule331  ()
         _lhsOpp :: PP_Doc
         _lhsOpp = rule332 _patIpp
         _patObelowIrrefutable = rule333  ()
         _lhsOpp' :: PP_Doc
         _lhsOpp' = rule334 _patIpp
         _copy = rule335 _patIcopy
         _lhsOcopy :: Pattern
         _lhsOcopy = rule336 _copy
         _lhsOisUnderscore :: Bool
         _lhsOisUnderscore = rule337 _patIisUnderscore
         _patOisDeclOfLet = rule338 _lhsIisDeclOfLet
         _patOoptions = rule339 _lhsIoptions
         !__result_ = T_Pattern_vOut40 _lhsOcopy _lhsOisUnderscore _lhsOpp _lhsOpp' _lhsOstrictVars
         in __result_ )
     in C_Pattern_s41 v40
   {-# INLINE rule331 #-}
   {-# LINE 341 "./src-ag/PrintCode.ag" #-}
   rule331 = \  (_ :: ()) ->
                         {-# LINE 341 "./src-ag/PrintCode.ag" #-}
                         []
                         {-# LINE 3129 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule332 #-}
   {-# LINE 365 "./src-ag/PrintCode.ag" #-}
   rule332 = \ ((_patIpp) :: PP_Doc) ->
                           {-# LINE 365 "./src-ag/PrintCode.ag" #-}
                           text "~" >|< pp_parens _patIpp
                           {-# LINE 3135 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule333 #-}
   {-# LINE 377 "./src-ag/PrintCode.ag" #-}
   rule333 = \  (_ :: ()) ->
                               {-# LINE 377 "./src-ag/PrintCode.ag" #-}
                               True
                               {-# LINE 3141 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule334 #-}
   {-# LINE 397 "./src-ag/PrintCode.ag" #-}
   rule334 = \ ((_patIpp) :: PP_Doc) ->
                            {-# LINE 397 "./src-ag/PrintCode.ag" #-}
                            text "~" >|< pp_parens _patIpp
                            {-# LINE 3147 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule335 #-}
   rule335 = \ ((_patIcopy) :: Pattern) ->
     Irrefutable _patIcopy
   {-# INLINE rule336 #-}
   rule336 = \ _copy ->
     _copy
   {-# INLINE rule337 #-}
   rule337 = \ ((_patIisUnderscore) :: Bool) ->
     _patIisUnderscore
   {-# INLINE rule338 #-}
   rule338 = \ ((_lhsIisDeclOfLet) :: Bool) ->
     _lhsIisDeclOfLet
   {-# INLINE rule339 #-}
   rule339 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Pattern_Underscore #-}
sem_Pattern_Underscore :: (Pos) -> T_Pattern 
sem_Pattern_Underscore !arg_pos_ = T_Pattern (return st41) where
   {-# NOINLINE st41 #-}
   !st41 = let
      v40 :: T_Pattern_v40 
      v40 = \ !(T_Pattern_vIn40 _lhsIbelowIrrefutable _lhsIisDeclOfLet _lhsIoptions) -> ( let
         _lhsOpp :: PP_Doc
         _lhsOpp = rule340  ()
         _lhsOisUnderscore :: Bool
         _lhsOisUnderscore = rule341  ()
         _lhsOpp' :: PP_Doc
         _lhsOpp' = rule342  ()
         _lhsOstrictVars :: [PP_Doc]
         _lhsOstrictVars = rule343  ()
         _copy = rule344 arg_pos_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule345 _copy
         !__result_ = T_Pattern_vOut40 _lhsOcopy _lhsOisUnderscore _lhsOpp _lhsOpp' _lhsOstrictVars
         in __result_ )
     in C_Pattern_s41 v40
   {-# INLINE rule340 #-}
   {-# LINE 366 "./src-ag/PrintCode.ag" #-}
   rule340 = \  (_ :: ()) ->
                           {-# LINE 366 "./src-ag/PrintCode.ag" #-}
                           text "_"
                           {-# LINE 3189 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule341 #-}
   {-# LINE 372 "./src-ag/PrintCode.ag" #-}
   rule341 = \  (_ :: ()) ->
                                    {-# LINE 372 "./src-ag/PrintCode.ag" #-}
                                    True
                                    {-# LINE 3195 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule342 #-}
   {-# LINE 398 "./src-ag/PrintCode.ag" #-}
   rule342 = \  (_ :: ()) ->
                            {-# LINE 398 "./src-ag/PrintCode.ag" #-}
                            text "_"
                            {-# LINE 3201 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule343 #-}
   rule343 = \  (_ :: ()) ->
     []
   {-# INLINE rule344 #-}
   rule344 = \ pos_ ->
     Underscore pos_
   {-# INLINE rule345 #-}
   rule345 = \ _copy ->
     _copy

-- Patterns ----------------------------------------------------
-- wrapper
data Inh_Patterns  = Inh_Patterns { belowIrrefutable_Inh_Patterns :: !(Bool), isDeclOfLet_Inh_Patterns :: !(Bool), options_Inh_Patterns :: !(Options) }
data Syn_Patterns  = Syn_Patterns { copy_Syn_Patterns :: !(Patterns), pps_Syn_Patterns :: !([PP_Doc]), pps'_Syn_Patterns :: !([PP_Doc]), strictVars_Syn_Patterns :: !([PP_Doc]) }
{-# INLINABLE wrap_Patterns #-}
wrap_Patterns :: T_Patterns  -> Inh_Patterns  -> (Syn_Patterns )
wrap_Patterns !(T_Patterns act) !(Inh_Patterns _lhsIbelowIrrefutable _lhsIisDeclOfLet _lhsIoptions) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_Patterns_vIn43 _lhsIbelowIrrefutable _lhsIisDeclOfLet _lhsIoptions
        !(T_Patterns_vOut43 _lhsOcopy _lhsOpps _lhsOpps' _lhsOstrictVars) <- return (inv_Patterns_s44 sem arg)
        return (Syn_Patterns _lhsOcopy _lhsOpps _lhsOpps' _lhsOstrictVars)
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
data T_Patterns_vIn43  = T_Patterns_vIn43 (Bool) (Bool) (Options)
data T_Patterns_vOut43  = T_Patterns_vOut43 (Patterns) ([PP_Doc]) ([PP_Doc]) ([PP_Doc])
{-# NOINLINE sem_Patterns_Cons #-}
sem_Patterns_Cons :: T_Pattern  -> T_Patterns  -> T_Patterns 
sem_Patterns_Cons arg_hd_ arg_tl_ = T_Patterns (return st44) where
   {-# NOINLINE st44 #-}
   !st44 = let
      v43 :: T_Patterns_v43 
      v43 = \ !(T_Patterns_vIn43 _lhsIbelowIrrefutable _lhsIisDeclOfLet _lhsIoptions) -> ( let
         _hdX41 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_hd_))
         _tlX44 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_tl_))
         (T_Pattern_vOut40 _hdIcopy _hdIisUnderscore _hdIpp _hdIpp' _hdIstrictVars) = inv_Pattern_s41 _hdX41 (T_Pattern_vIn40 _hdObelowIrrefutable _hdOisDeclOfLet _hdOoptions)
         (T_Patterns_vOut43 _tlIcopy _tlIpps _tlIpps' _tlIstrictVars) = inv_Patterns_s44 _tlX44 (T_Patterns_vIn43 _tlObelowIrrefutable _tlOisDeclOfLet _tlOoptions)
         _lhsOpps :: [PP_Doc]
         _lhsOpps = rule346 _hdIpp _tlIpps
         _lhsOpps' :: [PP_Doc]
         _lhsOpps' = rule347 _hdIpp' _tlIpps'
         _lhsOstrictVars :: [PP_Doc]
         _lhsOstrictVars = rule348 _hdIstrictVars _tlIstrictVars
         _copy = rule349 _hdIcopy _tlIcopy
         _lhsOcopy :: Patterns
         _lhsOcopy = rule350 _copy
         _hdObelowIrrefutable = rule351 _lhsIbelowIrrefutable
         _hdOisDeclOfLet = rule352 _lhsIisDeclOfLet
         _hdOoptions = rule353 _lhsIoptions
         _tlObelowIrrefutable = rule354 _lhsIbelowIrrefutable
         _tlOisDeclOfLet = rule355 _lhsIisDeclOfLet
         _tlOoptions = rule356 _lhsIoptions
         !__result_ = T_Patterns_vOut43 _lhsOcopy _lhsOpps _lhsOpps' _lhsOstrictVars
         in __result_ )
     in C_Patterns_s44 v43
   {-# INLINE rule346 #-}
   {-# LINE 348 "./src-ag/PrintCode.ag" #-}
   rule346 = \ ((_hdIpp) :: PP_Doc) ((_tlIpps) :: [PP_Doc]) ->
                     {-# LINE 348 "./src-ag/PrintCode.ag" #-}
                     _hdIpp : _tlIpps
                     {-# LINE 3276 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule347 #-}
   {-# LINE 388 "./src-ag/PrintCode.ag" #-}
   rule347 = \ ((_hdIpp') :: PP_Doc) ((_tlIpps') :: [PP_Doc]) ->
                      {-# LINE 388 "./src-ag/PrintCode.ag" #-}
                      _hdIpp' : _tlIpps'
                      {-# LINE 3282 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule348 #-}
   rule348 = \ ((_hdIstrictVars) :: [PP_Doc]) ((_tlIstrictVars) :: [PP_Doc]) ->
     _hdIstrictVars ++ _tlIstrictVars
   {-# INLINE rule349 #-}
   rule349 = \ ((_hdIcopy) :: Pattern) ((_tlIcopy) :: Patterns) ->
     (:) _hdIcopy _tlIcopy
   {-# INLINE rule350 #-}
   rule350 = \ _copy ->
     _copy
   {-# INLINE rule351 #-}
   rule351 = \ ((_lhsIbelowIrrefutable) :: Bool) ->
     _lhsIbelowIrrefutable
   {-# INLINE rule352 #-}
   rule352 = \ ((_lhsIisDeclOfLet) :: Bool) ->
     _lhsIisDeclOfLet
   {-# INLINE rule353 #-}
   rule353 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule354 #-}
   rule354 = \ ((_lhsIbelowIrrefutable) :: Bool) ->
     _lhsIbelowIrrefutable
   {-# INLINE rule355 #-}
   rule355 = \ ((_lhsIisDeclOfLet) :: Bool) ->
     _lhsIisDeclOfLet
   {-# INLINE rule356 #-}
   rule356 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Patterns_Nil #-}
sem_Patterns_Nil ::  T_Patterns 
sem_Patterns_Nil  = T_Patterns (return st44) where
   {-# NOINLINE st44 #-}
   !st44 = let
      v43 :: T_Patterns_v43 
      v43 = \ !(T_Patterns_vIn43 _lhsIbelowIrrefutable _lhsIisDeclOfLet _lhsIoptions) -> ( let
         _lhsOpps :: [PP_Doc]
         _lhsOpps = rule357  ()
         _lhsOpps' :: [PP_Doc]
         _lhsOpps' = rule358  ()
         _lhsOstrictVars :: [PP_Doc]
         _lhsOstrictVars = rule359  ()
         _copy = rule360  ()
         _lhsOcopy :: Patterns
         _lhsOcopy = rule361 _copy
         !__result_ = T_Patterns_vOut43 _lhsOcopy _lhsOpps _lhsOpps' _lhsOstrictVars
         in __result_ )
     in C_Patterns_s44 v43
   {-# INLINE rule357 #-}
   {-# LINE 349 "./src-ag/PrintCode.ag" #-}
   rule357 = \  (_ :: ()) ->
                     {-# LINE 349 "./src-ag/PrintCode.ag" #-}
                     []
                     {-# LINE 3334 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule358 #-}
   {-# LINE 389 "./src-ag/PrintCode.ag" #-}
   rule358 = \  (_ :: ()) ->
                      {-# LINE 389 "./src-ag/PrintCode.ag" #-}
                      []
                      {-# LINE 3340 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule359 #-}
   rule359 = \  (_ :: ()) ->
     []
   {-# INLINE rule360 #-}
   rule360 = \  (_ :: ()) ->
     []
   {-# INLINE rule361 #-}
   rule361 = \ _copy ->
     _copy

-- Program -----------------------------------------------------
-- wrapper
data Inh_Program  = Inh_Program { importBlocks_Inh_Program :: !(PP_Doc), mainBlocksDoc_Inh_Program :: !(PP_Doc), mainFile_Inh_Program :: !(String), mainName_Inh_Program :: !(String), moduleHeader_Inh_Program :: !(String -> String -> String -> Bool -> String), options_Inh_Program :: !(Options), optionsLine_Inh_Program :: !(String), pragmaBlocks_Inh_Program :: !(String), textBlockMap_Inh_Program :: !(Map BlockInfo PP_Doc), textBlocks_Inh_Program :: !(PP_Doc) }
data Syn_Program  = Syn_Program { genIO_Syn_Program :: !(IO ()), output_Syn_Program :: !(PP_Docs) }
{-# INLINABLE wrap_Program #-}
wrap_Program :: T_Program  -> Inh_Program  -> (Syn_Program )
wrap_Program !(T_Program act) !(Inh_Program _lhsIimportBlocks _lhsImainBlocksDoc _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsIoptions _lhsIoptionsLine _lhsIpragmaBlocks _lhsItextBlockMap _lhsItextBlocks) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_Program_vIn46 _lhsIimportBlocks _lhsImainBlocksDoc _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsIoptions _lhsIoptionsLine _lhsIpragmaBlocks _lhsItextBlockMap _lhsItextBlocks
        !(T_Program_vOut46 _lhsOgenIO _lhsOoutput) <- return (inv_Program_s47 sem arg)
        return (Syn_Program _lhsOgenIO _lhsOoutput)
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
data T_Program_vIn46  = T_Program_vIn46 (PP_Doc) (PP_Doc) (String) (String) (String -> String -> String -> Bool -> String) (Options) (String) (String) (Map BlockInfo PP_Doc) (PP_Doc)
data T_Program_vOut46  = T_Program_vOut46 (IO ()) (PP_Docs)
{-# NOINLINE sem_Program_Program #-}
sem_Program_Program :: T_Chunks  -> (Bool) -> T_Program 
sem_Program_Program arg_chunks_ !arg_ordered_ = T_Program (return st47) where
   {-# NOINLINE st47 #-}
   !st47 = let
      v46 :: T_Program_v46 
      v46 = \ !(T_Program_vIn46 _lhsIimportBlocks _lhsImainBlocksDoc _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsIoptions _lhsIoptionsLine _lhsIpragmaBlocks _lhsItextBlockMap _lhsItextBlocks) -> ( let
         _chunksX11 = Control.Monad.Identity.runIdentity (attach_T_Chunks (arg_chunks_))
         (T_Chunks_vOut10 _chunksIappendCommon _chunksIappendMain _chunksIgenSems _chunksIimports _chunksIpps) = inv_Chunks_s11 _chunksX11 (T_Chunks_vIn10 _chunksOimportBlocks _chunksOisDeclOfLet _chunksOmainFile _chunksOmainName _chunksOmoduleHeader _chunksOnested _chunksOoptions _chunksOoptionsLine _chunksOpragmaBlocks _chunksOtextBlockMap _chunksOtextBlocks)
         _options = rule362 _lhsIoptions arg_ordered_
         _chunksOnested = rule363 _lhsIoptions
         _lhsOoutput :: PP_Docs
         _lhsOoutput = rule364 _chunksIpps
         _chunksOisDeclOfLet = rule365  ()
         _mainModuleFile = rule366 _lhsImainFile
         _genMainModule = rule367 _chunksIappendMain _chunksIimports _lhsImainBlocksDoc _lhsImainName _lhsImoduleHeader _lhsIoptionsLine _lhsIpragmaBlocks _mainModuleFile
         _commonFile = rule368 _lhsImainFile
         _genCommonModule = rule369 _chunksIappendCommon _commonFile _lhsIimportBlocks _lhsImainName _lhsImoduleHeader _lhsIoptionsLine _lhsIpragmaBlocks _lhsItextBlocks
         _lhsOgenIO :: IO ()
         _lhsOgenIO = rule370 _chunksIgenSems _genCommonModule _genMainModule
         _chunksOimportBlocks = rule371 _lhsIimportBlocks
         _chunksOmainFile = rule372 _lhsImainFile
         _chunksOmainName = rule373 _lhsImainName
         _chunksOmoduleHeader = rule374 _lhsImoduleHeader
         _chunksOoptions = rule375 _options
         _chunksOoptionsLine = rule376 _lhsIoptionsLine
         _chunksOpragmaBlocks = rule377 _lhsIpragmaBlocks
         _chunksOtextBlockMap = rule378 _lhsItextBlockMap
         _chunksOtextBlocks = rule379 _lhsItextBlocks
         !__result_ = T_Program_vOut46 _lhsOgenIO _lhsOoutput
         in __result_ )
     in C_Program_s47 v46
   {-# INLINE rule362 #-}
   {-# LINE 58 "./src-ag/PrintCode.ag" #-}
   rule362 = \ ((_lhsIoptions) :: Options) ordered_ ->
                  {-# LINE 58 "./src-ag/PrintCode.ag" #-}
                  _lhsIoptions { breadthFirst = breadthFirst _lhsIoptions && visit _lhsIoptions && cases _lhsIoptions && ordered_ }
                  {-# LINE 3418 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule363 #-}
   {-# LINE 61 "./src-ag/PrintCode.ag" #-}
   rule363 = \ ((_lhsIoptions) :: Options) ->
                              {-# LINE 61 "./src-ag/PrintCode.ag" #-}
                              nest _lhsIoptions
                              {-# LINE 3424 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule364 #-}
   {-# LINE 93 "./src-ag/PrintCode.ag" #-}
   rule364 = \ ((_chunksIpps) :: PP_Docs) ->
                               {-# LINE 93 "./src-ag/PrintCode.ag" #-}
                               _chunksIpps
                               {-# LINE 3430 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule365 #-}
   {-# LINE 412 "./src-ag/PrintCode.ag" #-}
   rule365 = \  (_ :: ()) ->
                             {-# LINE 412 "./src-ag/PrintCode.ag" #-}
                             False
                             {-# LINE 3436 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule366 #-}
   {-# LINE 446 "./src-ag/PrintCode.ag" #-}
   rule366 = \ ((_lhsImainFile) :: String) ->
                             {-# LINE 446 "./src-ag/PrintCode.ag" #-}
                             _lhsImainFile
                             {-# LINE 3442 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule367 #-}
   {-# LINE 448 "./src-ag/PrintCode.ag" #-}
   rule367 = \ ((_chunksIappendMain) :: [[PP_Doc]]) ((_chunksIimports) :: [String]) ((_lhsImainBlocksDoc) :: PP_Doc) ((_lhsImainName) :: String) ((_lhsImoduleHeader) :: String -> String -> String -> Bool -> String) ((_lhsIoptionsLine) :: String) ((_lhsIpragmaBlocks) :: String) _mainModuleFile ->
            {-# LINE 448 "./src-ag/PrintCode.ag" #-}
            writeModule _mainModuleFile
              ( [ pp $ _lhsIpragmaBlocks
                , pp $ _lhsIoptionsLine
                , pp $ _lhsImoduleHeader _lhsImainName "" "" False
                , pp $ ("import " ++ _lhsImainName ++ "_common\n")
                ]
                ++ map pp _chunksIimports
                ++ map vlist _chunksIappendMain
                ++ [_lhsImainBlocksDoc]
              )
            {-# LINE 3457 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule368 #-}
   {-# LINE 459 "./src-ag/PrintCode.ag" #-}
   rule368 = \ ((_lhsImainFile) :: String) ->
                         {-# LINE 459 "./src-ag/PrintCode.ag" #-}
                         replaceBaseName _lhsImainFile (takeBaseName _lhsImainFile ++ "_common")
                         {-# LINE 3463 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule369 #-}
   {-# LINE 461 "./src-ag/PrintCode.ag" #-}
   rule369 = \ ((_chunksIappendCommon) :: [[PP_Doc]]) _commonFile ((_lhsIimportBlocks) :: PP_Doc) ((_lhsImainName) :: String) ((_lhsImoduleHeader) :: String -> String -> String -> Bool -> String) ((_lhsIoptionsLine) :: String) ((_lhsIpragmaBlocks) :: String) ((_lhsItextBlocks) :: PP_Doc) ->
            {-# LINE 461 "./src-ag/PrintCode.ag" #-}
            writeModule _commonFile
                ( [ pp $ _lhsIpragmaBlocks
                  , pp $ _lhsIoptionsLine
                  , pp $ _lhsImoduleHeader _lhsImainName "_common" "" True
                  , _lhsIimportBlocks
                  , _lhsItextBlocks
                  ]
                  ++ map vlist _chunksIappendCommon
                )
            {-# LINE 3477 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule370 #-}
   {-# LINE 471 "./src-ag/PrintCode.ag" #-}
   rule370 = \ ((_chunksIgenSems) :: IO ()) _genCommonModule _genMainModule ->
                    {-# LINE 471 "./src-ag/PrintCode.ag" #-}
                    do _genMainModule
                       _genCommonModule
                       _chunksIgenSems
                    {-# LINE 3485 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule371 #-}
   rule371 = \ ((_lhsIimportBlocks) :: PP_Doc) ->
     _lhsIimportBlocks
   {-# INLINE rule372 #-}
   rule372 = \ ((_lhsImainFile) :: String) ->
     _lhsImainFile
   {-# INLINE rule373 #-}
   rule373 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule374 #-}
   rule374 = \ ((_lhsImoduleHeader) :: String -> String -> String -> Bool -> String) ->
     _lhsImoduleHeader
   {-# INLINE rule375 #-}
   rule375 = \ _options ->
     _options
   {-# INLINE rule376 #-}
   rule376 = \ ((_lhsIoptionsLine) :: String) ->
     _lhsIoptionsLine
   {-# INLINE rule377 #-}
   rule377 = \ ((_lhsIpragmaBlocks) :: String) ->
     _lhsIpragmaBlocks
   {-# INLINE rule378 #-}
   rule378 = \ ((_lhsItextBlockMap) :: Map BlockInfo PP_Doc) ->
     _lhsItextBlockMap
   {-# INLINE rule379 #-}
   rule379 = \ ((_lhsItextBlocks) :: PP_Doc) ->
     _lhsItextBlocks

-- Type --------------------------------------------------------
-- wrapper
data Inh_Type  = Inh_Type { nested_Inh_Type :: !(Bool) }
data Syn_Type  = Syn_Type { pp_Syn_Type :: !(PP_Doc), prec_Syn_Type :: !(Int) }
{-# INLINABLE wrap_Type #-}
wrap_Type :: T_Type  -> Inh_Type  -> (Syn_Type )
wrap_Type !(T_Type act) !(Inh_Type _lhsInested) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_Type_vIn49 _lhsInested
        !(T_Type_vOut49 _lhsOpp _lhsOprec) <- return (inv_Type_s50 sem arg)
        return (Syn_Type _lhsOpp _lhsOprec)
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
data T_Type_vIn49  = T_Type_vIn49 (Bool)
data T_Type_vOut49  = T_Type_vOut49 (PP_Doc) (Int)
{-# NOINLINE sem_Type_Arr #-}
sem_Type_Arr :: T_Type  -> T_Type  -> T_Type 
sem_Type_Arr arg_left_ arg_right_ = T_Type (return st50) where
   {-# NOINLINE st50 #-}
   !st50 = let
      v49 :: T_Type_v49 
      v49 = \ !(T_Type_vIn49 _lhsInested) -> ( let
         _leftX50 = Control.Monad.Identity.runIdentity (attach_T_Type (arg_left_))
         _rightX50 = Control.Monad.Identity.runIdentity (attach_T_Type (arg_right_))
         (T_Type_vOut49 _leftIpp _leftIprec) = inv_Type_s50 _leftX50 (T_Type_vIn49 _leftOnested)
         (T_Type_vOut49 _rightIpp _rightIprec) = inv_Type_s50 _rightX50 (T_Type_vIn49 _rightOnested)
         _lhsOprec :: Int
         _lhsOprec = rule380  ()
         _lhsOpp :: PP_Doc
         _lhsOpp = rule381 _l _r
         _l = rule382 _leftIpp _leftIprec
         _r = rule383 _rightIpp _rightIprec
         _leftOnested = rule384 _lhsInested
         _rightOnested = rule385 _lhsInested
         !__result_ = T_Type_vOut49 _lhsOpp _lhsOprec
         in __result_ )
     in C_Type_s50 v49
   {-# INLINE rule380 #-}
   {-# LINE 259 "./src-ag/PrintCode.ag" #-}
   rule380 = \  (_ :: ()) ->
                               {-# LINE 259 "./src-ag/PrintCode.ag" #-}
                               2
                               {-# LINE 3583 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule381 #-}
   {-# LINE 260 "./src-ag/PrintCode.ag" #-}
   rule381 = \ _l _r ->
                               {-# LINE 260 "./src-ag/PrintCode.ag" #-}
                               _l     >#< "->" >-< _r
                               {-# LINE 3589 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule382 #-}
   {-# LINE 261 "./src-ag/PrintCode.ag" #-}
   rule382 = \ ((_leftIpp) :: PP_Doc) ((_leftIprec) :: Int) ->
                               {-# LINE 261 "./src-ag/PrintCode.ag" #-}
                               if _leftIprec  <= 2 then pp_parens _leftIpp  else _leftIpp
                               {-# LINE 3595 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule383 #-}
   {-# LINE 262 "./src-ag/PrintCode.ag" #-}
   rule383 = \ ((_rightIpp) :: PP_Doc) ((_rightIprec) :: Int) ->
                               {-# LINE 262 "./src-ag/PrintCode.ag" #-}
                               if _rightIprec <  2 then pp_parens _rightIpp else _rightIpp
                               {-# LINE 3601 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule384 #-}
   rule384 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule385 #-}
   rule385 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
{-# NOINLINE sem_Type_CtxApp #-}
sem_Type_CtxApp :: ([(String, [String])]) -> T_Type  -> T_Type 
sem_Type_CtxApp !arg_left_ arg_right_ = T_Type (return st50) where
   {-# NOINLINE st50 #-}
   !st50 = let
      v49 :: T_Type_v49 
      v49 = \ !(T_Type_vIn49 _lhsInested) -> ( let
         _rightX50 = Control.Monad.Identity.runIdentity (attach_T_Type (arg_right_))
         (T_Type_vOut49 _rightIpp _rightIprec) = inv_Type_s50 _rightX50 (T_Type_vIn49 _rightOnested)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule386 _rightIpp arg_left_
         _lhsOprec :: Int
         _lhsOprec = rule387 _rightIprec
         _rightOnested = rule388 _lhsInested
         !__result_ = T_Type_vOut49 _lhsOpp _lhsOprec
         in __result_ )
     in C_Type_s50 v49
   {-# INLINE rule386 #-}
   {-# LINE 268 "./src-ag/PrintCode.ag" #-}
   rule386 = \ ((_rightIpp) :: PP_Doc) left_ ->
                 {-# LINE 268 "./src-ag/PrintCode.ag" #-}
                 (pp_block "(" ")" "," $ map (\(n,ns) -> hv_sp $ map pp (n:ns)) left_) >#< "=>" >#< _rightIpp
                 {-# LINE 3630 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule387 #-}
   rule387 = \ ((_rightIprec) :: Int) ->
     _rightIprec
   {-# INLINE rule388 #-}
   rule388 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
{-# NOINLINE sem_Type_QuantApp #-}
sem_Type_QuantApp :: (String) -> T_Type  -> T_Type 
sem_Type_QuantApp !arg_left_ arg_right_ = T_Type (return st50) where
   {-# NOINLINE st50 #-}
   !st50 = let
      v49 :: T_Type_v49 
      v49 = \ !(T_Type_vIn49 _lhsInested) -> ( let
         _rightX50 = Control.Monad.Identity.runIdentity (attach_T_Type (arg_right_))
         (T_Type_vOut49 _rightIpp _rightIprec) = inv_Type_s50 _rightX50 (T_Type_vIn49 _rightOnested)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule389 _rightIpp arg_left_
         _lhsOprec :: Int
         _lhsOprec = rule390 _rightIprec
         _rightOnested = rule391 _lhsInested
         !__result_ = T_Type_vOut49 _lhsOpp _lhsOprec
         in __result_ )
     in C_Type_s50 v49
   {-# INLINE rule389 #-}
   {-# LINE 270 "./src-ag/PrintCode.ag" #-}
   rule389 = \ ((_rightIpp) :: PP_Doc) left_ ->
                 {-# LINE 270 "./src-ag/PrintCode.ag" #-}
                 left_ >#< _rightIpp
                 {-# LINE 3659 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule390 #-}
   rule390 = \ ((_rightIprec) :: Int) ->
     _rightIprec
   {-# INLINE rule391 #-}
   rule391 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
{-# NOINLINE sem_Type_TypeApp #-}
sem_Type_TypeApp :: T_Type  -> T_Types  -> T_Type 
sem_Type_TypeApp arg_func_ arg_args_ = T_Type (return st50) where
   {-# NOINLINE st50 #-}
   !st50 = let
      v49 :: T_Type_v49 
      v49 = \ !(T_Type_vIn49 _lhsInested) -> ( let
         _funcX50 = Control.Monad.Identity.runIdentity (attach_T_Type (arg_func_))
         _argsX53 = Control.Monad.Identity.runIdentity (attach_T_Types (arg_args_))
         (T_Type_vOut49 _funcIpp _funcIprec) = inv_Type_s50 _funcX50 (T_Type_vIn49 _funcOnested)
         (T_Types_vOut52 _argsIpps) = inv_Types_s53 _argsX53 (T_Types_vIn52 _argsOnested)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule392 _argsIpps _funcIpp
         _lhsOprec :: Int
         _lhsOprec = rule393 _funcIprec
         _funcOnested = rule394 _lhsInested
         _argsOnested = rule395 _lhsInested
         !__result_ = T_Type_vOut49 _lhsOpp _lhsOprec
         in __result_ )
     in C_Type_s50 v49
   {-# INLINE rule392 #-}
   {-# LINE 265 "./src-ag/PrintCode.ag" #-}
   rule392 = \ ((_argsIpps) :: PP_Docs) ((_funcIpp) :: PP_Doc) ->
                 {-# LINE 265 "./src-ag/PrintCode.ag" #-}
                 hv_sp (_funcIpp : _argsIpps)
                 {-# LINE 3691 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule393 #-}
   rule393 = \ ((_funcIprec) :: Int) ->
     _funcIprec
   {-# INLINE rule394 #-}
   rule394 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule395 #-}
   rule395 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
{-# NOINLINE sem_Type_TupleType #-}
sem_Type_TupleType :: T_Types  -> T_Type 
sem_Type_TupleType arg_tps_ = T_Type (return st50) where
   {-# NOINLINE st50 #-}
   !st50 = let
      v49 :: T_Type_v49 
      v49 = \ !(T_Type_vIn49 _lhsInested) -> ( let
         _tpsX53 = Control.Monad.Identity.runIdentity (attach_T_Types (arg_tps_))
         (T_Types_vOut52 _tpsIpps) = inv_Types_s53 _tpsX53 (T_Types_vIn52 _tpsOnested)
         _lhsOprec :: Int
         _lhsOprec = rule396  ()
         _lhsOpp :: PP_Doc
         _lhsOpp = rule397 _lhsInested _tpsIpps
         _tpsOnested = rule398 _lhsInested
         !__result_ = T_Type_vOut49 _lhsOpp _lhsOprec
         in __result_ )
     in C_Type_s50 v49
   {-# INLINE rule396 #-}
   {-# LINE 272 "./src-ag/PrintCode.ag" #-}
   rule396 = \  (_ :: ()) ->
                               {-# LINE 272 "./src-ag/PrintCode.ag" #-}
                               5
                               {-# LINE 3723 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule397 #-}
   {-# LINE 273 "./src-ag/PrintCode.ag" #-}
   rule397 = \ ((_lhsInested) :: Bool) ((_tpsIpps) :: PP_Docs) ->
                               {-# LINE 273 "./src-ag/PrintCode.ag" #-}
                               ppTuple _lhsInested _tpsIpps
                               {-# LINE 3729 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule398 #-}
   rule398 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
{-# NOINLINE sem_Type_UnboxedTupleType #-}
sem_Type_UnboxedTupleType :: T_Types  -> T_Type 
sem_Type_UnboxedTupleType arg_tps_ = T_Type (return st50) where
   {-# NOINLINE st50 #-}
   !st50 = let
      v49 :: T_Type_v49 
      v49 = \ !(T_Type_vIn49 _lhsInested) -> ( let
         _tpsX53 = Control.Monad.Identity.runIdentity (attach_T_Types (arg_tps_))
         (T_Types_vOut52 _tpsIpps) = inv_Types_s53 _tpsX53 (T_Types_vIn52 _tpsOnested)
         _lhsOprec :: Int
         _lhsOprec = rule399  ()
         _lhsOpp :: PP_Doc
         _lhsOpp = rule400 _lhsInested _tpsIpps
         _tpsOnested = rule401 _lhsInested
         !__result_ = T_Type_vOut49 _lhsOpp _lhsOprec
         in __result_ )
     in C_Type_s50 v49
   {-# INLINE rule399 #-}
   {-# LINE 275 "./src-ag/PrintCode.ag" #-}
   rule399 = \  (_ :: ()) ->
                                      {-# LINE 275 "./src-ag/PrintCode.ag" #-}
                                      5
                                      {-# LINE 3755 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule400 #-}
   {-# LINE 276 "./src-ag/PrintCode.ag" #-}
   rule400 = \ ((_lhsInested) :: Bool) ((_tpsIpps) :: PP_Docs) ->
                                      {-# LINE 276 "./src-ag/PrintCode.ag" #-}
                                      ppUnboxedTuple _lhsInested _tpsIpps
                                      {-# LINE 3761 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule401 #-}
   rule401 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
{-# NOINLINE sem_Type_List #-}
sem_Type_List :: T_Type  -> T_Type 
sem_Type_List arg_tp_ = T_Type (return st50) where
   {-# NOINLINE st50 #-}
   !st50 = let
      v49 :: T_Type_v49 
      v49 = \ !(T_Type_vIn49 _lhsInested) -> ( let
         _tpX50 = Control.Monad.Identity.runIdentity (attach_T_Type (arg_tp_))
         (T_Type_vOut49 _tpIpp _tpIprec) = inv_Type_s50 _tpX50 (T_Type_vIn49 _tpOnested)
         _lhsOprec :: Int
         _lhsOprec = rule402  ()
         _lhsOpp :: PP_Doc
         _lhsOpp = rule403 _tpIpp
         _tpOnested = rule404 _lhsInested
         !__result_ = T_Type_vOut49 _lhsOpp _lhsOprec
         in __result_ )
     in C_Type_s50 v49
   {-# INLINE rule402 #-}
   {-# LINE 278 "./src-ag/PrintCode.ag" #-}
   rule402 = \  (_ :: ()) ->
                               {-# LINE 278 "./src-ag/PrintCode.ag" #-}
                               5
                               {-# LINE 3787 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule403 #-}
   {-# LINE 279 "./src-ag/PrintCode.ag" #-}
   rule403 = \ ((_tpIpp) :: PP_Doc) ->
                               {-# LINE 279 "./src-ag/PrintCode.ag" #-}
                               "[" >|< _tpIpp >|< "]"
                               {-# LINE 3793 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule404 #-}
   rule404 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
{-# NOINLINE sem_Type_SimpleType #-}
sem_Type_SimpleType :: (String) -> T_Type 
sem_Type_SimpleType !arg_txt_ = T_Type (return st50) where
   {-# NOINLINE st50 #-}
   !st50 = let
      v49 :: T_Type_v49 
      v49 = \ !(T_Type_vIn49 _lhsInested) -> ( let
         _lhsOprec :: Int
         _lhsOprec = rule405  ()
         _lhsOpp :: PP_Doc
         _lhsOpp = rule406 arg_txt_
         !__result_ = T_Type_vOut49 _lhsOpp _lhsOprec
         in __result_ )
     in C_Type_s50 v49
   {-# INLINE rule405 #-}
   {-# LINE 281 "./src-ag/PrintCode.ag" #-}
   rule405 = \  (_ :: ()) ->
                               {-# LINE 281 "./src-ag/PrintCode.ag" #-}
                               5
                               {-# LINE 3816 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule406 #-}
   {-# LINE 282 "./src-ag/PrintCode.ag" #-}
   rule406 = \ txt_ ->
                               {-# LINE 282 "./src-ag/PrintCode.ag" #-}
                               if reallySimple txt_ then text txt_ else pp_parens (text txt_)
                               {-# LINE 3822 "dist/build/PrintCode.hs"#-}
{-# NOINLINE sem_Type_NontermType #-}
sem_Type_NontermType :: (String) -> ([String]) -> (Bool) -> T_Type 
sem_Type_NontermType !arg_name_ !arg_params_ !arg_deforested_ = T_Type (return st50) where
   {-# NOINLINE st50 #-}
   !st50 = let
      v49 :: T_Type_v49 
      v49 = \ !(T_Type_vIn49 _lhsInested) -> ( let
         _lhsOprec :: Int
         _lhsOprec = rule407  ()
         _lhsOpp :: PP_Doc
         _lhsOpp = rule408 _prefix arg_name_ arg_params_
         _prefix = rule409 arg_deforested_
         !__result_ = T_Type_vOut49 _lhsOpp _lhsOprec
         in __result_ )
     in C_Type_s50 v49
   {-# INLINE rule407 #-}
   {-# LINE 284 "./src-ag/PrintCode.ag" #-}
   rule407 = \  (_ :: ()) ->
                               {-# LINE 284 "./src-ag/PrintCode.ag" #-}
                               5
                               {-# LINE 3843 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule408 #-}
   {-# LINE 285 "./src-ag/PrintCode.ag" #-}
   rule408 = \ _prefix name_ params_ ->
                               {-# LINE 285 "./src-ag/PrintCode.ag" #-}
                               _prefix     >|< text name_ >#< hv_sp params_
                               {-# LINE 3849 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule409 #-}
   {-# LINE 286 "./src-ag/PrintCode.ag" #-}
   rule409 = \ deforested_ ->
                                {-# LINE 286 "./src-ag/PrintCode.ag" #-}
                                if deforested_
                                then text "T_"
                                else empty
                                {-# LINE 3857 "dist/build/PrintCode.hs"#-}
{-# NOINLINE sem_Type_TMaybe #-}
sem_Type_TMaybe :: T_Type  -> T_Type 
sem_Type_TMaybe arg_tp_ = T_Type (return st50) where
   {-# NOINLINE st50 #-}
   !st50 = let
      v49 :: T_Type_v49 
      v49 = \ !(T_Type_vIn49 _lhsInested) -> ( let
         _tpX50 = Control.Monad.Identity.runIdentity (attach_T_Type (arg_tp_))
         (T_Type_vOut49 _tpIpp _tpIprec) = inv_Type_s50 _tpX50 (T_Type_vIn49 _tpOnested)
         _lhsOprec :: Int
         _lhsOprec = rule410  ()
         _lhsOpp :: PP_Doc
         _lhsOpp = rule411 _tpIpp
         _tpOnested = rule412 _lhsInested
         !__result_ = T_Type_vOut49 _lhsOpp _lhsOprec
         in __result_ )
     in C_Type_s50 v49
   {-# INLINE rule410 #-}
   {-# LINE 289 "./src-ag/PrintCode.ag" #-}
   rule410 = \  (_ :: ()) ->
                               {-# LINE 289 "./src-ag/PrintCode.ag" #-}
                               5
                               {-# LINE 3880 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule411 #-}
   {-# LINE 290 "./src-ag/PrintCode.ag" #-}
   rule411 = \ ((_tpIpp) :: PP_Doc) ->
                               {-# LINE 290 "./src-ag/PrintCode.ag" #-}
                               text "Maybe" >#< _tpIpp
                               {-# LINE 3886 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule412 #-}
   rule412 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
{-# NOINLINE sem_Type_TEither #-}
sem_Type_TEither :: T_Type  -> T_Type  -> T_Type 
sem_Type_TEither arg_left_ arg_right_ = T_Type (return st50) where
   {-# NOINLINE st50 #-}
   !st50 = let
      v49 :: T_Type_v49 
      v49 = \ !(T_Type_vIn49 _lhsInested) -> ( let
         _leftX50 = Control.Monad.Identity.runIdentity (attach_T_Type (arg_left_))
         _rightX50 = Control.Monad.Identity.runIdentity (attach_T_Type (arg_right_))
         (T_Type_vOut49 _leftIpp _leftIprec) = inv_Type_s50 _leftX50 (T_Type_vIn49 _leftOnested)
         (T_Type_vOut49 _rightIpp _rightIprec) = inv_Type_s50 _rightX50 (T_Type_vIn49 _rightOnested)
         _lhsOprec :: Int
         _lhsOprec = rule413  ()
         _lhsOpp :: PP_Doc
         _lhsOpp = rule414 _leftIpp _rightIpp
         _leftOnested = rule415 _lhsInested
         _rightOnested = rule416 _lhsInested
         !__result_ = T_Type_vOut49 _lhsOpp _lhsOprec
         in __result_ )
     in C_Type_s50 v49
   {-# INLINE rule413 #-}
   {-# LINE 291 "./src-ag/PrintCode.ag" #-}
   rule413 = \  (_ :: ()) ->
                               {-# LINE 291 "./src-ag/PrintCode.ag" #-}
                               5
                               {-# LINE 3915 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule414 #-}
   {-# LINE 292 "./src-ag/PrintCode.ag" #-}
   rule414 = \ ((_leftIpp) :: PP_Doc) ((_rightIpp) :: PP_Doc) ->
                               {-# LINE 292 "./src-ag/PrintCode.ag" #-}
                               text "Either" >#< pp_parens _leftIpp >#< pp_parens _rightIpp
                               {-# LINE 3921 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule415 #-}
   rule415 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule416 #-}
   rule416 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
{-# NOINLINE sem_Type_TMap #-}
sem_Type_TMap :: T_Type  -> T_Type  -> T_Type 
sem_Type_TMap arg_key_ arg_value_ = T_Type (return st50) where
   {-# NOINLINE st50 #-}
   !st50 = let
      v49 :: T_Type_v49 
      v49 = \ !(T_Type_vIn49 _lhsInested) -> ( let
         _keyX50 = Control.Monad.Identity.runIdentity (attach_T_Type (arg_key_))
         _valueX50 = Control.Monad.Identity.runIdentity (attach_T_Type (arg_value_))
         (T_Type_vOut49 _keyIpp _keyIprec) = inv_Type_s50 _keyX50 (T_Type_vIn49 _keyOnested)
         (T_Type_vOut49 _valueIpp _valueIprec) = inv_Type_s50 _valueX50 (T_Type_vIn49 _valueOnested)
         _lhsOprec :: Int
         _lhsOprec = rule417  ()
         _lhsOpp :: PP_Doc
         _lhsOpp = rule418 _keyIpp _valueIpp
         _keyOnested = rule419 _lhsInested
         _valueOnested = rule420 _lhsInested
         !__result_ = T_Type_vOut49 _lhsOpp _lhsOprec
         in __result_ )
     in C_Type_s50 v49
   {-# INLINE rule417 #-}
   {-# LINE 293 "./src-ag/PrintCode.ag" #-}
   rule417 = \  (_ :: ()) ->
                               {-# LINE 293 "./src-ag/PrintCode.ag" #-}
                               5
                               {-# LINE 3953 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule418 #-}
   {-# LINE 294 "./src-ag/PrintCode.ag" #-}
   rule418 = \ ((_keyIpp) :: PP_Doc) ((_valueIpp) :: PP_Doc) ->
                               {-# LINE 294 "./src-ag/PrintCode.ag" #-}
                               text "Data.Map.Map" >#< pp_parens _keyIpp >#< pp_parens _valueIpp
                               {-# LINE 3959 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule419 #-}
   rule419 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule420 #-}
   rule420 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
{-# NOINLINE sem_Type_TIntMap #-}
sem_Type_TIntMap :: T_Type  -> T_Type 
sem_Type_TIntMap arg_value_ = T_Type (return st50) where
   {-# NOINLINE st50 #-}
   !st50 = let
      v49 :: T_Type_v49 
      v49 = \ !(T_Type_vIn49 _lhsInested) -> ( let
         _valueX50 = Control.Monad.Identity.runIdentity (attach_T_Type (arg_value_))
         (T_Type_vOut49 _valueIpp _valueIprec) = inv_Type_s50 _valueX50 (T_Type_vIn49 _valueOnested)
         _lhsOprec :: Int
         _lhsOprec = rule421  ()
         _lhsOpp :: PP_Doc
         _lhsOpp = rule422 _valueIpp
         _valueOnested = rule423 _lhsInested
         !__result_ = T_Type_vOut49 _lhsOpp _lhsOprec
         in __result_ )
     in C_Type_s50 v49
   {-# INLINE rule421 #-}
   {-# LINE 295 "./src-ag/PrintCode.ag" #-}
   rule421 = \  (_ :: ()) ->
                               {-# LINE 295 "./src-ag/PrintCode.ag" #-}
                               5
                               {-# LINE 3988 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule422 #-}
   {-# LINE 296 "./src-ag/PrintCode.ag" #-}
   rule422 = \ ((_valueIpp) :: PP_Doc) ->
                               {-# LINE 296 "./src-ag/PrintCode.ag" #-}
                               text "Data.IntMap.IntMap" >#< pp_parens _valueIpp
                               {-# LINE 3994 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule423 #-}
   rule423 = \ ((_lhsInested) :: Bool) ->
     _lhsInested

-- Types -------------------------------------------------------
-- wrapper
data Inh_Types  = Inh_Types { nested_Inh_Types :: !(Bool) }
data Syn_Types  = Syn_Types { pps_Syn_Types :: !(PP_Docs) }
{-# INLINABLE wrap_Types #-}
wrap_Types :: T_Types  -> Inh_Types  -> (Syn_Types )
wrap_Types !(T_Types act) !(Inh_Types _lhsInested) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_Types_vIn52 _lhsInested
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
data T_Types_vIn52  = T_Types_vIn52 (Bool)
data T_Types_vOut52  = T_Types_vOut52 (PP_Docs)
{-# NOINLINE sem_Types_Cons #-}
sem_Types_Cons :: T_Type  -> T_Types  -> T_Types 
sem_Types_Cons arg_hd_ arg_tl_ = T_Types (return st53) where
   {-# NOINLINE st53 #-}
   !st53 = let
      v52 :: T_Types_v52 
      v52 = \ !(T_Types_vIn52 _lhsInested) -> ( let
         _hdX50 = Control.Monad.Identity.runIdentity (attach_T_Type (arg_hd_))
         _tlX53 = Control.Monad.Identity.runIdentity (attach_T_Types (arg_tl_))
         (T_Type_vOut49 _hdIpp _hdIprec) = inv_Type_s50 _hdX50 (T_Type_vIn49 _hdOnested)
         (T_Types_vOut52 _tlIpps) = inv_Types_s53 _tlX53 (T_Types_vIn52 _tlOnested)
         _lhsOpps :: PP_Docs
         _lhsOpps = rule424 _hdIpp _tlIpps
         _hdOnested = rule425 _lhsInested
         _tlOnested = rule426 _lhsInested
         !__result_ = T_Types_vOut52 _lhsOpps
         in __result_ )
     in C_Types_s53 v52
   {-# INLINE rule424 #-}
   {-# LINE 76 "./src-ag/PrintCode.ag" #-}
   rule424 = \ ((_hdIpp) :: PP_Doc) ((_tlIpps) :: PP_Docs) ->
                     {-# LINE 76 "./src-ag/PrintCode.ag" #-}
                     _hdIpp : _tlIpps
                     {-# LINE 4052 "dist/build/PrintCode.hs"#-}
   {-# INLINE rule425 #-}
   rule425 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule426 #-}
   rule426 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
{-# NOINLINE sem_Types_Nil #-}
sem_Types_Nil ::  T_Types 
sem_Types_Nil  = T_Types (return st53) where
   {-# NOINLINE st53 #-}
   !st53 = let
      v52 :: T_Types_v52 
      v52 = \ !(T_Types_vIn52 _lhsInested) -> ( let
         _lhsOpps :: PP_Docs
         _lhsOpps = rule427  ()
         !__result_ = T_Types_vOut52 _lhsOpps
         in __result_ )
     in C_Types_s53 v52
   {-# INLINE rule427 #-}
   {-# LINE 77 "./src-ag/PrintCode.ag" #-}
   rule427 = \  (_ :: ()) ->
                     {-# LINE 77 "./src-ag/PrintCode.ag" #-}
                     []
                     {-# LINE 4076 "dist/build/PrintCode.hs"#-}
