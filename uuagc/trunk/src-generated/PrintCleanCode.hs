{-# LANGUAGE Rank2Types, GADTs #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PrintCleanCode where
{-# LINE 2 "./src-ag/Patterns.ag" #-}

-- Patterns.ag imports
import UU.Scanner.Position(Pos)
import CommonTypes (ConstructorIdent,Identifier)
{-# LINE 12 "dist/build/PrintCleanCode.hs" #-}

{-# LINE 2 "./src-ag/Code.ag" #-}

import Patterns
import Data.Set(Set)
import qualified Data.Set as Set
import Data.Map(Map)
import qualified Data.Map as Map
{-# LINE 21 "dist/build/PrintCleanCode.hs" #-}

{-# LINE 10 "./src-ag/PrintCleanCode.ag" #-}

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
{-# LINE 35 "dist/build/PrintCleanCode.hs" #-}
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
{-# LINE 55 "dist/build/PrintCleanCode.hs" #-}

{-# LINE 23 "./src-ag/PrintCleanCode.ag" #-}

type PP_Docs = [PP_Doc]
{-# LINE 60 "dist/build/PrintCleanCode.hs" #-}

{-# LINE 27 "./src-ag/PrintCleanCode.ag" #-}

ppMultiSeqH :: [PP_Doc] -> PP_Doc -> PP_Doc
ppMultiSeqH = ppMultiSeq' (>#<)

ppMultiSeqV :: [PP_Doc] -> PP_Doc -> PP_Doc
ppMultiSeqV = ppMultiSeq' (>-<)

ppMultiSeq' :: (PP_Doc -> PP_Doc -> PP_Doc) -> [PP_Doc] -> PP_Doc -> PP_Doc
ppMultiSeq' next strictArgs expr
  = foldr (\v r -> (v >#< "`seq`") `next` pp_parens r) expr strictArgs
{-# LINE 73 "dist/build/PrintCleanCode.hs" #-}

{-# LINE 318 "./src-ag/PrintCleanCode.ag" #-}


reallySimple :: String -> Bool
reallySimple = and . map (\x -> isAlphaNum x || x=='_')

ppTuple :: Bool -> [PP_Doc] -> PP_Doc
ppTuple _     [x] = pp x
ppTuple True  pps = "(" >|< pp_block " " (replicate (length pps `max` 1) ')') ",(" pps
ppTuple False pps = "(" >|< pp_block " " ")" "," pps
ppUnboxedTuple :: Bool -> [PP_Doc] -> PP_Doc
ppUnboxedTuple = ppTuple
--ppUnboxedTuple True pps  = "(# " >|< pp_block " " (concat $ replicate (length pps `max` 1) " #)") ",(# " pps
--ppUnboxedTuple False pps = "(# " >|< pp_block " " " #)" "," pps

{-# LINE 90 "dist/build/PrintCleanCode.hs" #-}

{-# LINE 421 "./src-ag/PrintCleanCode.ag" #-}

locname' :: Identifier -> [Char]
locname' n = "_loc_" ++ getName n
{-# LINE 96 "dist/build/PrintCleanCode.hs" #-}

{-# LINE 496 "./src-ag/PrintCleanCode.ag" #-}

renderDocs :: [PP_Doc] -> String
renderDocs pps = foldr (.) id (map (\d -> (disp d 50000) . ( '\n':) ) pps) ""
{-# LINE 102 "dist/build/PrintCleanCode.hs" #-}

{-# LINE 544 "./src-ag/PrintCleanCode.ag" #-}

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
{-# LINE 119 "dist/build/PrintCleanCode.hs" #-}
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
   {-# LINE 228 "./src-ag/PrintCleanCode.ag" #-}
   rule0 = \ ((_exprIpp) :: PP_Doc) ((_leftIpp) :: PP_Doc) ->
                               {-# LINE 228 "./src-ag/PrintCleanCode.ag" #-}
                               ["{" >#< _leftIpp >#< "->", _exprIpp >#< "}"]
                               {-# LINE 178 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule1 #-}
   {-# LINE 445 "./src-ag/PrintCleanCode.ag" #-}
   rule1 = \  (_ :: ()) ->
                           {-# LINE 445 "./src-ag/PrintCleanCode.ag" #-}
                           False
                           {-# LINE 184 "dist/build/PrintCleanCode.hs"#-}
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
   {-# LINE 72 "./src-ag/PrintCleanCode.ag" #-}
   rule8 = \ ((_hdIpps) :: PP_Docs) ((_tlIpps) :: PP_Docs) ->
                     {-# LINE 72 "./src-ag/PrintCleanCode.ag" #-}
                     _hdIpps ++ _tlIpps
                     {-# LINE 261 "dist/build/PrintCleanCode.hs"#-}
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
   {-# LINE 73 "./src-ag/PrintCleanCode.ag" #-}
   rule15 = \  (_ :: ()) ->
                     {-# LINE 73 "./src-ag/PrintCleanCode.ag" #-}
                     []
                     {-# LINE 297 "dist/build/PrintCleanCode.hs"#-}

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
         (T_Decl_vOut19 _commentIpp _commentIppa) = inv_Decl_s20 _commentX20 (T_Decl_vIn19 _commentOisDeclOfLet _commentOnested _commentOoptions _commentOoutputfile)
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
   {-# LINE 43 "./src-ag/PrintCleanCode.ag" #-}
   rule16 = \ ((_lhsImainFile) :: String) ((_lhsIoptions) :: Options) name_ ->
                         {-# LINE 43 "./src-ag/PrintCleanCode.ag" #-}
                         if sepSemMods _lhsIoptions
                         then replaceBaseName _lhsImainFile (takeBaseName _lhsImainFile ++ "_" ++ name_)
                         else _lhsImainFile
                         {-# LINE 400 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule17 #-}
   {-# LINE 104 "./src-ag/PrintCleanCode.ag" #-}
   rule17 = \ ((_cataFunIpps) :: PP_Docs) ((_commentIpp) :: PP_Doc) ((_dataDefIpps) :: PP_Docs) ((_infoIpps) :: PP_Docs) ((_lhsItextBlockMap) :: Map BlockInfo PP_Doc) ((_semDomIpps) :: PP_Docs) ((_semFunctionsIpps) :: PP_Docs) ((_semWrapperIpps) :: PP_Docs) name_ ->
                                {-# LINE 104 "./src-ag/PrintCleanCode.ag" #-}
                                _commentIpp
                                :  _infoIpps
                                ++ _dataDefIpps
                                ++ _cataFunIpps
                                ++ _semDomIpps
                                ++ _semWrapperIpps
                                ++ _semFunctionsIpps
                                ++ [Map.findWithDefault empty (BlockOther, Just $ identifier name_) _lhsItextBlockMap]
                                {-# LINE 413 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule18 #-}
   {-# LINE 504 "./src-ag/PrintCleanCode.ag" #-}
   rule18 = \ ((_lhsImainName) :: String) name_ ->
                      {-# LINE 504 "./src-ag/PrintCleanCode.ag" #-}
                      ["import " ++ _lhsImainName ++ "_" ++ name_ ++ "\n"]
                      {-# LINE 419 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule19 #-}
   {-# LINE 511 "./src-ag/PrintCleanCode.ag" #-}
   rule19 = \ ((_commentIpp) :: PP_Doc) ((_dataDefIpps) :: PP_Docs) ((_lhsIoptions) :: Options) ((_semDomIpps) :: PP_Docs) ((_semWrapperIpps) :: PP_Docs) ->
            {-# LINE 511 "./src-ag/PrintCleanCode.ag" #-}
            [ [_commentIpp]
            , _dataDefIpps
            , _semDomIpps
            , if reference _lhsIoptions then _semWrapperIpps else []
            ]
            {-# LINE 429 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule20 #-}
   {-# LINE 517 "./src-ag/PrintCleanCode.ag" #-}
   rule20 = \ ((_cataFunIpps) :: PP_Docs) ((_commentIpp) :: PP_Doc) ((_lhsIoptions) :: Options) ((_semWrapperIpps) :: PP_Docs) ->
            {-# LINE 517 "./src-ag/PrintCleanCode.ag" #-}
            [ [_commentIpp]
            , _cataFunIpps
            , if reference _lhsIoptions then [] else _semWrapperIpps
            ]
            {-# LINE 438 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule21 #-}
   {-# LINE 527 "./src-ag/PrintCleanCode.ag" #-}
   rule21 = \ ((_commentIpp) :: PP_Doc) _exports ((_infoIpps) :: PP_Docs) ((_lhsImainName) :: String) ((_lhsImoduleHeader) :: String -> String -> String -> Bool -> String) ((_lhsIoptionsLine) :: String) ((_lhsIpragmaBlocks) :: String) ((_lhsItextBlockMap) :: Map BlockInfo PP_Doc) _outputfile ((_semFunctionsIpps) :: PP_Docs) name_ ->
            {-# LINE 527 "./src-ag/PrintCleanCode.ag" #-}
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
            {-# LINE 455 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule22 #-}
   {-# LINE 542 "./src-ag/PrintCleanCode.ag" #-}
   rule22 = \ semNames_ ->
                      {-# LINE 542 "./src-ag/PrintCleanCode.ag" #-}
                      concat $ intersperse "," semNames_
                      {-# LINE 461 "dist/build/PrintCleanCode.hs"#-}
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
   {-# LINE 96 "./src-ag/PrintCleanCode.ag" #-}
   rule51 = \ ((_hdIpps) :: PP_Docs) ((_tlIpps) :: PP_Docs) ->
                     {-# LINE 96 "./src-ag/PrintCleanCode.ag" #-}
                     _hdIpps ++ _tlIpps
                     {-# LINE 628 "dist/build/PrintCleanCode.hs"#-}
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
   {-# LINE 97 "./src-ag/PrintCleanCode.ag" #-}
   rule78 = \  (_ :: ()) ->
                     {-# LINE 97 "./src-ag/PrintCleanCode.ag" #-}
                     []
                     {-# LINE 732 "dist/build/PrintCleanCode.hs"#-}
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
data Syn_DataAlt  = Syn_DataAlt { pp_Syn_DataAlt :: !(PP_Doc), ppa_Syn_DataAlt :: !(PP_Doc) }
{-# INLINABLE wrap_DataAlt #-}
wrap_DataAlt :: T_DataAlt  -> Inh_DataAlt  -> (Syn_DataAlt )
wrap_DataAlt !(T_DataAlt act) !(Inh_DataAlt _lhsInested _lhsIstrictPre) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_DataAlt_vIn13 _lhsInested _lhsIstrictPre
        !(T_DataAlt_vOut13 _lhsOpp _lhsOppa) <- return (inv_DataAlt_s14 sem arg)
        return (Syn_DataAlt _lhsOpp _lhsOppa)
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
data T_DataAlt_vOut13  = T_DataAlt_vOut13 (PP_Doc) (PP_Doc)
{-# NOINLINE sem_DataAlt_DataAlt #-}
sem_DataAlt_DataAlt :: (String) -> T_Types  -> T_DataAlt 
sem_DataAlt_DataAlt !arg_name_ arg_args_ = T_DataAlt (return st14) where
   {-# NOINLINE st14 #-}
   !st14 = let
      v13 :: T_DataAlt_v13 
      v13 = \ !(T_DataAlt_vIn13 _lhsInested _lhsIstrictPre) -> ( let
         _argsX53 = Control.Monad.Identity.runIdentity (attach_T_Types (arg_args_))
         (T_Types_vOut52 _argsIcopy _argsIpps) = inv_Types_s53 _argsX53 (T_Types_vIn52 _argsOnested)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule83 _argsIpps _lhsIstrictPre arg_name_
         _lhsOppa :: PP_Doc
         _lhsOppa = rule84  ()
         _argsOnested = rule85 _lhsInested
         !__result_ = T_DataAlt_vOut13 _lhsOpp _lhsOppa
         in __result_ )
     in C_DataAlt_s14 v13
   {-# INLINE rule83 #-}
   {-# LINE 231 "./src-ag/PrintCleanCode.ag" #-}
   rule83 = \ ((_argsIpps) :: PP_Docs) ((_lhsIstrictPre) :: PP_Doc) name_ ->
                               {-# LINE 231 "./src-ag/PrintCleanCode.ag" #-}
                               name_ >#< hv_sp (map ((_lhsIstrictPre >|<) . pp_parens) _argsIpps)
                               {-# LINE 799 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule84 #-}
   {-# LINE 232 "./src-ag/PrintCleanCode.ag" #-}
   rule84 = \  (_ :: ()) ->
                               {-# LINE 232 "./src-ag/PrintCleanCode.ag" #-}
                               empty
                               {-# LINE 805 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule85 #-}
   rule85 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
{-# NOINLINE sem_DataAlt_Record #-}
sem_DataAlt_Record :: (String) -> T_NamedTypes  -> T_DataAlt 
sem_DataAlt_Record !arg_name_ arg_args_ = T_DataAlt (return st14) where
   {-# NOINLINE st14 #-}
   !st14 = let
      v13 :: T_DataAlt_v13 
      v13 = \ !(T_DataAlt_vIn13 _lhsInested _lhsIstrictPre) -> ( let
         _argsX38 = Control.Monad.Identity.runIdentity (attach_T_NamedTypes (arg_args_))
         (T_NamedTypes_vOut37 _argsIppas _argsIpps) = inv_NamedTypes_s38 _argsX38 (T_NamedTypes_vIn37 _argsOnested)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule86 _argsIpps _lhsIstrictPre arg_name_
         _lhsOppa :: PP_Doc
         _lhsOppa = rule87 _argsIppas arg_name_
         _argsOnested = rule88 _lhsInested
         !__result_ = T_DataAlt_vOut13 _lhsOpp _lhsOppa
         in __result_ )
     in C_DataAlt_s14 v13
   {-# INLINE rule86 #-}
   {-# LINE 233 "./src-ag/PrintCleanCode.ag" #-}
   rule86 = \ ((_argsIpps) :: PP_Docs) ((_lhsIstrictPre) :: PP_Doc) name_ ->
                               {-# LINE 233 "./src-ag/PrintCleanCode.ag" #-}
                               name_ >#< hv_sp (map ((_lhsIstrictPre >|<) . pp_parens) _argsIpps)
                               {-# LINE 831 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule87 #-}
   {-# LINE 234 "./src-ag/PrintCleanCode.ag" #-}
   rule87 = \ ((_argsIppas) :: PP_Docs) name_ ->
                               {-# LINE 234 "./src-ag/PrintCleanCode.ag" #-}
                               let f n d = d >#< (pp_block ("(" ++ name_) ")" "" $ map pp (ppat n))
                                           >#< pp "=" >#< pp "x"
                                   ppat n = replicate (length _argsIppas - n - 1) (pp " _") ++ [pp " x"] ++ replicate n (pp " _")
                               in  snd $ foldr (\x (n, xs) -> (n + 1, f n x >-< xs)) (0, empty) _argsIppas
                               {-# LINE 840 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule88 #-}
   rule88 = \ ((_lhsInested) :: Bool) ->
     _lhsInested

-- DataAlts ----------------------------------------------------
-- wrapper
data Inh_DataAlts  = Inh_DataAlts { nested_Inh_DataAlts :: !(Bool), strictPre_Inh_DataAlts :: !(PP_Doc) }
data Syn_DataAlts  = Syn_DataAlts { ppas_Syn_DataAlts :: !(PP_Docs), pps_Syn_DataAlts :: !(PP_Docs) }
{-# INLINABLE wrap_DataAlts #-}
wrap_DataAlts :: T_DataAlts  -> Inh_DataAlts  -> (Syn_DataAlts )
wrap_DataAlts !(T_DataAlts act) !(Inh_DataAlts _lhsInested _lhsIstrictPre) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_DataAlts_vIn16 _lhsInested _lhsIstrictPre
        !(T_DataAlts_vOut16 _lhsOppas _lhsOpps) <- return (inv_DataAlts_s17 sem arg)
        return (Syn_DataAlts _lhsOppas _lhsOpps)
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
data T_DataAlts_vOut16  = T_DataAlts_vOut16 (PP_Docs) (PP_Docs)
{-# NOINLINE sem_DataAlts_Cons #-}
sem_DataAlts_Cons :: T_DataAlt  -> T_DataAlts  -> T_DataAlts 
sem_DataAlts_Cons arg_hd_ arg_tl_ = T_DataAlts (return st17) where
   {-# NOINLINE st17 #-}
   !st17 = let
      v16 :: T_DataAlts_v16 
      v16 = \ !(T_DataAlts_vIn16 _lhsInested _lhsIstrictPre) -> ( let
         _hdX14 = Control.Monad.Identity.runIdentity (attach_T_DataAlt (arg_hd_))
         _tlX17 = Control.Monad.Identity.runIdentity (attach_T_DataAlts (arg_tl_))
         (T_DataAlt_vOut13 _hdIpp _hdIppa) = inv_DataAlt_s14 _hdX14 (T_DataAlt_vIn13 _hdOnested _hdOstrictPre)
         (T_DataAlts_vOut16 _tlIppas _tlIpps) = inv_DataAlts_s17 _tlX17 (T_DataAlts_vIn16 _tlOnested _tlOstrictPre)
         _lhsOpps :: PP_Docs
         _lhsOpps = rule89 _hdIpp _tlIpps
         _lhsOppas :: PP_Docs
         _lhsOppas = rule90 _hdIppa _tlIppas
         _hdOnested = rule91 _lhsInested
         _hdOstrictPre = rule92 _lhsIstrictPre
         _tlOnested = rule93 _lhsInested
         _tlOstrictPre = rule94 _lhsIstrictPre
         !__result_ = T_DataAlts_vOut16 _lhsOppas _lhsOpps
         in __result_ )
     in C_DataAlts_s17 v16
   {-# INLINE rule89 #-}
   {-# LINE 76 "./src-ag/PrintCleanCode.ag" #-}
   rule89 = \ ((_hdIpp) :: PP_Doc) ((_tlIpps) :: PP_Docs) ->
                      {-# LINE 76 "./src-ag/PrintCleanCode.ag" #-}
                      _hdIpp : _tlIpps
                      {-# LINE 902 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule90 #-}
   {-# LINE 77 "./src-ag/PrintCleanCode.ag" #-}
   rule90 = \ ((_hdIppa) :: PP_Doc) ((_tlIppas) :: PP_Docs) ->
                      {-# LINE 77 "./src-ag/PrintCleanCode.ag" #-}
                      _hdIppa : _tlIppas
                      {-# LINE 908 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule91 #-}
   rule91 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule92 #-}
   rule92 = \ ((_lhsIstrictPre) :: PP_Doc) ->
     _lhsIstrictPre
   {-# INLINE rule93 #-}
   rule93 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule94 #-}
   rule94 = \ ((_lhsIstrictPre) :: PP_Doc) ->
     _lhsIstrictPre
{-# NOINLINE sem_DataAlts_Nil #-}
sem_DataAlts_Nil ::  T_DataAlts 
sem_DataAlts_Nil  = T_DataAlts (return st17) where
   {-# NOINLINE st17 #-}
   !st17 = let
      v16 :: T_DataAlts_v16 
      v16 = \ !(T_DataAlts_vIn16 _lhsInested _lhsIstrictPre) -> ( let
         _lhsOpps :: PP_Docs
         _lhsOpps = rule95  ()
         _lhsOppas :: PP_Docs
         _lhsOppas = rule96  ()
         !__result_ = T_DataAlts_vOut16 _lhsOppas _lhsOpps
         in __result_ )
     in C_DataAlts_s17 v16
   {-# INLINE rule95 #-}
   {-# LINE 78 "./src-ag/PrintCleanCode.ag" #-}
   rule95 = \  (_ :: ()) ->
                      {-# LINE 78 "./src-ag/PrintCleanCode.ag" #-}
                      []
                      {-# LINE 940 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule96 #-}
   {-# LINE 79 "./src-ag/PrintCleanCode.ag" #-}
   rule96 = \  (_ :: ()) ->
                      {-# LINE 79 "./src-ag/PrintCleanCode.ag" #-}
                      []
                      {-# LINE 946 "dist/build/PrintCleanCode.hs"#-}

-- Decl --------------------------------------------------------
-- wrapper
data Inh_Decl  = Inh_Decl { isDeclOfLet_Inh_Decl :: !(Bool), nested_Inh_Decl :: !(Bool), options_Inh_Decl :: !(Options), outputfile_Inh_Decl :: !(String) }
data Syn_Decl  = Syn_Decl { pp_Syn_Decl :: !(PP_Doc), ppa_Syn_Decl :: !(PP_Doc) }
{-# INLINABLE wrap_Decl #-}
wrap_Decl :: T_Decl  -> Inh_Decl  -> (Syn_Decl )
wrap_Decl !(T_Decl act) !(Inh_Decl _lhsIisDeclOfLet _lhsInested _lhsIoptions _lhsIoutputfile) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_Decl_vIn19 _lhsIisDeclOfLet _lhsInested _lhsIoptions _lhsIoutputfile
        !(T_Decl_vOut19 _lhsOpp _lhsOppa) <- return (inv_Decl_s20 sem arg)
        return (Syn_Decl _lhsOpp _lhsOppa)
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
data T_Decl_vOut19  = T_Decl_vOut19 (PP_Doc) (PP_Doc)
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
         _lhsOpp = rule97 _leftIpp _rhsIpp
         _lhsOppa :: PP_Doc
         _lhsOppa = rule98  ()
         _leftOisDeclOfLet = rule99 _lhsIisDeclOfLet
         _leftOnested = rule100 _lhsInested
         _leftOoptions = rule101 _lhsIoptions
         _leftOoutputfile = rule102 _lhsIoutputfile
         _rhsOnested = rule103 _lhsInested
         _rhsOoptions = rule104 _lhsIoptions
         _rhsOoutputfile = rule105 _lhsIoutputfile
         !__result_ = T_Decl_vOut19 _lhsOpp _lhsOppa
         in __result_ )
     in C_Decl_s20 v19
   {-# INLINE rule97 #-}
   {-# LINE 114 "./src-ag/PrintCleanCode.ag" #-}
   rule97 = \ ((_leftIpp) :: PP_Doc) ((_rhsIpp) :: PP_Doc) ->
                               {-# LINE 114 "./src-ag/PrintCleanCode.ag" #-}
                               _leftIpp >#< "="
                               >-< indent 4 _rhsIpp
                               {-# LINE 1019 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule98 #-}
   rule98 = \  (_ :: ()) ->
     empty
   {-# INLINE rule99 #-}
   rule99 = \ ((_lhsIisDeclOfLet) :: Bool) ->
     _lhsIisDeclOfLet
   {-# INLINE rule100 #-}
   rule100 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule101 #-}
   rule101 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule102 #-}
   rule102 = \ ((_lhsIoutputfile) :: String) ->
     _lhsIoutputfile
   {-# INLINE rule103 #-}
   rule103 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule104 #-}
   rule104 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule105 #-}
   rule105 = \ ((_lhsIoutputfile) :: String) ->
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
         _lhsOpp = rule106 _leftIpp _rhsIpp
         _lhsOppa :: PP_Doc
         _lhsOppa = rule107  ()
         _leftOisDeclOfLet = rule108 _lhsIisDeclOfLet
         _leftOnested = rule109 _lhsInested
         _leftOoptions = rule110 _lhsIoptions
         _leftOoutputfile = rule111 _lhsIoutputfile
         _rhsOnested = rule112 _lhsInested
         _rhsOoptions = rule113 _lhsIoptions
         _rhsOoutputfile = rule114 _lhsIoutputfile
         !__result_ = T_Decl_vOut19 _lhsOpp _lhsOppa
         in __result_ )
     in C_Decl_s20 v19
   {-# INLINE rule106 #-}
   {-# LINE 116 "./src-ag/PrintCleanCode.ag" #-}
   rule106 = \ ((_leftIpp) :: PP_Doc) ((_rhsIpp) :: PP_Doc) ->
                               {-# LINE 116 "./src-ag/PrintCleanCode.ag" #-}
                               _leftIpp >#< "<-" >#< _rhsIpp
                               {-# LINE 1074 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule107 #-}
   rule107 = \  (_ :: ()) ->
     empty
   {-# INLINE rule108 #-}
   rule108 = \ ((_lhsIisDeclOfLet) :: Bool) ->
     _lhsIisDeclOfLet
   {-# INLINE rule109 #-}
   rule109 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule110 #-}
   rule110 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule111 #-}
   rule111 = \ ((_lhsIoutputfile) :: String) ->
     _lhsIoutputfile
   {-# INLINE rule112 #-}
   rule112 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule113 #-}
   rule113 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule114 #-}
   rule114 = \ ((_lhsIoutputfile) :: String) ->
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
         _lhsOpp = rule115 _leftIpp _rhsIpp
         _lhsOppa :: PP_Doc
         _lhsOppa = rule116  ()
         _leftOisDeclOfLet = rule117 _lhsIisDeclOfLet
         _leftOnested = rule118 _lhsInested
         _leftOoptions = rule119 _lhsIoptions
         _leftOoutputfile = rule120 _lhsIoutputfile
         _rhsOnested = rule121 _lhsInested
         _rhsOoptions = rule122 _lhsIoptions
         _rhsOoutputfile = rule123 _lhsIoutputfile
         !__result_ = T_Decl_vOut19 _lhsOpp _lhsOppa
         in __result_ )
     in C_Decl_s20 v19
   {-# INLINE rule115 #-}
   {-# LINE 117 "./src-ag/PrintCleanCode.ag" #-}
   rule115 = \ ((_leftIpp) :: PP_Doc) ((_rhsIpp) :: PP_Doc) ->
                               {-# LINE 117 "./src-ag/PrintCleanCode.ag" #-}
                               "let" >#< _leftIpp >#< "=" >#< _rhsIpp
                               {-# LINE 1129 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule116 #-}
   rule116 = \  (_ :: ()) ->
     empty
   {-# INLINE rule117 #-}
   rule117 = \ ((_lhsIisDeclOfLet) :: Bool) ->
     _lhsIisDeclOfLet
   {-# INLINE rule118 #-}
   rule118 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule119 #-}
   rule119 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule120 #-}
   rule120 = \ ((_lhsIoutputfile) :: String) ->
     _lhsIoutputfile
   {-# INLINE rule121 #-}
   rule121 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule122 #-}
   rule122 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule123 #-}
   rule123 = \ ((_lhsIoutputfile) :: String) ->
     _lhsIoutputfile
{-# NOINLINE sem_Decl_Data #-}
sem_Decl_Data :: (String) -> ([String]) -> T_DataAlts  -> (Bool) -> ([String]) -> T_Decl 
sem_Decl_Data !arg_name_ !arg_params_ arg_alts_ !arg_strict_ !arg_derivings_ = T_Decl (return st20) where
   {-# NOINLINE st20 #-}
   !st20 = let
      v19 :: T_Decl_v19 
      v19 = \ !(T_Decl_vIn19 _lhsIisDeclOfLet _lhsInested _lhsIoptions _lhsIoutputfile) -> ( let
         _altsX17 = Control.Monad.Identity.runIdentity (attach_T_DataAlts (arg_alts_))
         (T_DataAlts_vOut16 _altsIppas _altsIpps) = inv_DataAlts_s17 _altsX17 (T_DataAlts_vIn16 _altsOnested _altsOstrictPre)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule124 _altsIppas _altsIpps arg_derivings_ arg_name_ arg_params_
         _altsOstrictPre = rule125 arg_strict_
         _lhsOppa :: PP_Doc
         _lhsOppa = rule126  ()
         _altsOnested = rule127 _lhsInested
         !__result_ = T_Decl_vOut19 _lhsOpp _lhsOppa
         in __result_ )
     in C_Decl_s20 v19
   {-# INLINE rule124 #-}
   {-# LINE 118 "./src-ag/PrintCleanCode.ag" #-}
   rule124 = \ ((_altsIppas) :: PP_Docs) ((_altsIpps) :: PP_Docs) derivings_ name_ params_ ->
                               {-# LINE 118 "./src-ag/PrintCleanCode.ag" #-}
                               "::" >#< hv_sp (name_ : params_)
                               >#<  ( case _altsIpps of
                                            [] -> empty
                                            (x:xs) ->              "=" >#<  x
                                                   >-< vlist (map ("|" >#<) xs)
                                       >-< if null derivings_
                                              then empty
                                              else "deriving" >#< ppTuple False (map text derivings_)
                                    )
                               >-< foldr (>-<) empty _altsIppas
                               {-# LINE 1186 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule125 #-}
   {-# LINE 342 "./src-ag/PrintCleanCode.ag" #-}
   rule125 = \ strict_ ->
                            {-# LINE 342 "./src-ag/PrintCleanCode.ag" #-}
                            if strict_ then pp "!" else empty
                            {-# LINE 1192 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule126 #-}
   rule126 = \  (_ :: ()) ->
     empty
   {-# INLINE rule127 #-}
   rule127 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
{-# NOINLINE sem_Decl_NewType #-}
sem_Decl_NewType :: (String) -> ([String]) -> (String) -> T_Type  -> T_Decl 
sem_Decl_NewType !arg_name_ !arg_params_ !arg_con_ arg_tp_ = T_Decl (return st20) where
   {-# NOINLINE st20 #-}
   !st20 = let
      v19 :: T_Decl_v19 
      v19 = \ !(T_Decl_vIn19 _lhsIisDeclOfLet _lhsInested _lhsIoptions _lhsIoutputfile) -> ( let
         _tpX50 = Control.Monad.Identity.runIdentity (attach_T_Type (arg_tp_))
         (T_Type_vOut49 _tpIcopy _tpIpp _tpIprec) = inv_Type_s50 _tpX50 (T_Type_vIn49 _tpOnested)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule128 _tpIpp arg_con_ arg_name_ arg_params_
         _lhsOppa :: PP_Doc
         _lhsOppa = rule129  ()
         _tpOnested = rule130 _lhsInested
         !__result_ = T_Decl_vOut19 _lhsOpp _lhsOppa
         in __result_ )
     in C_Decl_s20 v19
   {-# INLINE rule128 #-}
   {-# LINE 129 "./src-ag/PrintCleanCode.ag" #-}
   rule128 = \ ((_tpIpp) :: PP_Doc) con_ name_ params_ ->
                               {-# LINE 129 "./src-ag/PrintCleanCode.ag" #-}
                               "::" >#< hv_sp (name_ : params_) >#< "=" >#< con_ >#< pp_parens _tpIpp
                               {-# LINE 1221 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule129 #-}
   rule129 = \  (_ :: ()) ->
     empty
   {-# INLINE rule130 #-}
   rule130 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
{-# NOINLINE sem_Decl_Type #-}
sem_Decl_Type :: (String) -> ([String]) -> T_Type  -> T_Decl 
sem_Decl_Type !arg_name_ !arg_params_ arg_tp_ = T_Decl (return st20) where
   {-# NOINLINE st20 #-}
   !st20 = let
      v19 :: T_Decl_v19 
      v19 = \ !(T_Decl_vIn19 _lhsIisDeclOfLet _lhsInested _lhsIoptions _lhsIoutputfile) -> ( let
         _tpX50 = Control.Monad.Identity.runIdentity (attach_T_Type (arg_tp_))
         (T_Type_vOut49 _tpIcopy _tpIpp _tpIprec) = inv_Type_s50 _tpX50 (T_Type_vIn49 _tpOnested)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule131 _tpIpp arg_name_ arg_params_
         _lhsOppa :: PP_Doc
         _lhsOppa = rule132  ()
         _tpOnested = rule133 _lhsInested
         !__result_ = T_Decl_vOut19 _lhsOpp _lhsOppa
         in __result_ )
     in C_Decl_s20 v19
   {-# INLINE rule131 #-}
   {-# LINE 130 "./src-ag/PrintCleanCode.ag" #-}
   rule131 = \ ((_tpIpp) :: PP_Doc) name_ params_ ->
                               {-# LINE 130 "./src-ag/PrintCleanCode.ag" #-}
                               "::" >#< hv_sp (name_ : params_) >#< ":==" >#<  _tpIpp
                               {-# LINE 1250 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule132 #-}
   rule132 = \  (_ :: ()) ->
     empty
   {-# INLINE rule133 #-}
   rule133 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
{-# NOINLINE sem_Decl_TSig #-}
sem_Decl_TSig :: (String) -> T_Type  -> T_Decl 
sem_Decl_TSig !arg_name_ arg_tp_ = T_Decl (return st20) where
   {-# NOINLINE st20 #-}
   !st20 = let
      v19 :: T_Decl_v19 
      v19 = \ !(T_Decl_vIn19 _lhsIisDeclOfLet _lhsInested _lhsIoptions _lhsIoutputfile) -> ( let
         _tpX50 = Control.Monad.Identity.runIdentity (attach_T_Type (arg_tp_))
         (T_Type_vOut49 _tpIcopy _tpIpp _tpIprec) = inv_Type_s50 _tpX50 (T_Type_vIn49 _tpOnested)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule134 _tpIpp arg_name_
         _lhsOppa :: PP_Doc
         _lhsOppa = rule135  ()
         _tpOnested = rule136 _lhsInested
         !__result_ = T_Decl_vOut19 _lhsOpp _lhsOppa
         in __result_ )
     in C_Decl_s20 v19
   {-# INLINE rule134 #-}
   {-# LINE 131 "./src-ag/PrintCleanCode.ag" #-}
   rule134 = \ ((_tpIpp) :: PP_Doc) name_ ->
                               {-# LINE 131 "./src-ag/PrintCleanCode.ag" #-}
                               name_ >#< "::" >#< _tpIpp
                               {-# LINE 1279 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule135 #-}
   rule135 = \  (_ :: ()) ->
     empty
   {-# INLINE rule136 #-}
   rule136 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
{-# NOINLINE sem_Decl_Comment #-}
sem_Decl_Comment :: (String) -> T_Decl 
sem_Decl_Comment !arg_txt_ = T_Decl (return st20) where
   {-# NOINLINE st20 #-}
   !st20 = let
      v19 :: T_Decl_v19 
      v19 = \ !(T_Decl_vIn19 _lhsIisDeclOfLet _lhsInested _lhsIoptions _lhsIoutputfile) -> ( let
         _lhsOpp :: PP_Doc
         _lhsOpp = rule137 arg_txt_
         _lhsOppa :: PP_Doc
         _lhsOppa = rule138  ()
         !__result_ = T_Decl_vOut19 _lhsOpp _lhsOppa
         in __result_ )
     in C_Decl_s20 v19
   {-# INLINE rule137 #-}
   {-# LINE 132 "./src-ag/PrintCleanCode.ag" #-}
   rule137 = \ txt_ ->
                               {-# LINE 132 "./src-ag/PrintCleanCode.ag" #-}
                               if '\n' `elem` txt_
                                 then "/*" >-< vlist (lines txt_) >-< "*/"
                                 else "//" >#< txt_
                               {-# LINE 1307 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule138 #-}
   rule138 = \  (_ :: ()) ->
     empty
{-# NOINLINE sem_Decl_PragmaDecl #-}
sem_Decl_PragmaDecl :: (String) -> T_Decl 
sem_Decl_PragmaDecl !arg_txt_ = T_Decl (return st20) where
   {-# NOINLINE st20 #-}
   !st20 = let
      v19 :: T_Decl_v19 
      v19 = \ !(T_Decl_vIn19 _lhsIisDeclOfLet _lhsInested _lhsIoptions _lhsIoutputfile) -> ( let
         _lhsOpp :: PP_Doc
         _lhsOpp = rule139 arg_txt_
         _lhsOppa :: PP_Doc
         _lhsOppa = rule140  ()
         !__result_ = T_Decl_vOut19 _lhsOpp _lhsOppa
         in __result_ )
     in C_Decl_s20 v19
   {-# INLINE rule139 #-}
   {-# LINE 135 "./src-ag/PrintCleanCode.ag" #-}
   rule139 = \ txt_ ->
                               {-# LINE 135 "./src-ag/PrintCleanCode.ag" #-}
                               "/*#" >#< text txt_ >#< "#*/"
                               {-# LINE 1330 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule140 #-}
   rule140 = \  (_ :: ()) ->
     empty
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
         _lhsOpp = rule141 _leftIpp _rhsIpp arg_monadic_
         _lhsOppa :: PP_Doc
         _lhsOppa = rule142  ()
         _leftOisDeclOfLet = rule143 _lhsIisDeclOfLet
         _leftOnested = rule144 _lhsInested
         _leftOoptions = rule145 _lhsIoptions
         _leftOoutputfile = rule146 _lhsIoutputfile
         _rhsOnested = rule147 _lhsInested
         _rhsOoptions = rule148 _lhsIoptions
         _rhsOoutputfile = rule149 _lhsIoutputfile
         !__result_ = T_Decl_vOut19 _lhsOpp _lhsOppa
         in __result_ )
     in C_Decl_s20 v19
   {-# INLINE rule141 #-}
   {-# LINE 136 "./src-ag/PrintCleanCode.ag" #-}
   rule141 = \ ((_leftIpp) :: PP_Doc) ((_rhsIpp) :: PP_Doc) monadic_ ->
                               {-# LINE 136 "./src-ag/PrintCleanCode.ag" #-}
                               if monadic_
                               then _leftIpp >#< "<-" >#< _rhsIpp
                               else _leftIpp >#< "=" >-< indent 4 _rhsIpp
                               {-# LINE 1366 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule142 #-}
   rule142 = \  (_ :: ()) ->
     empty
   {-# INLINE rule143 #-}
   rule143 = \ ((_lhsIisDeclOfLet) :: Bool) ->
     _lhsIisDeclOfLet
   {-# INLINE rule144 #-}
   rule144 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule145 #-}
   rule145 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule146 #-}
   rule146 = \ ((_lhsIoutputfile) :: String) ->
     _lhsIoutputfile
   {-# INLINE rule147 #-}
   rule147 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule148 #-}
   rule148 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule149 #-}
   rule149 = \ ((_lhsIoutputfile) :: String) ->
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
         _strat = rule150 _lhsIoptions
         _lhsOpp :: PP_Doc
         _lhsOpp = rule151 _leftIpp _lhsIoptions _rhsIpp _strat arg_nt_
         _lhsOppa :: PP_Doc
         _lhsOppa = rule152  ()
         _leftOisDeclOfLet = rule153 _lhsIisDeclOfLet
         _leftOnested = rule154 _lhsInested
         _leftOoptions = rule155 _lhsIoptions
         _leftOoutputfile = rule156 _lhsIoutputfile
         _rhsOnested = rule157 _lhsInested
         _rhsOoptions = rule158 _lhsIoptions
         _rhsOoutputfile = rule159 _lhsIoutputfile
         !__result_ = T_Decl_vOut19 _lhsOpp _lhsOppa
         in __result_ )
     in C_Decl_s20 v19
   {-# INLINE rule150 #-}
   {-# LINE 139 "./src-ag/PrintCleanCode.ag" #-}
   rule150 = \ ((_lhsIoptions) :: Options) ->
                               {-# LINE 139 "./src-ag/PrintCleanCode.ag" #-}
                               if breadthFirstStrict _lhsIoptions
                               then "stepwiseEval"
                               else "lazyEval"
                               {-# LINE 1424 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule151 #-}
   {-# LINE 142 "./src-ag/PrintCleanCode.ag" #-}
   rule151 = \ ((_leftIpp) :: PP_Doc) ((_lhsIoptions) :: Options) ((_rhsIpp) :: PP_Doc) _strat nt_ ->
                               {-# LINE 142 "./src-ag/PrintCleanCode.ag" #-}
                               if breadthFirst _lhsIoptions
                               then _leftIpp >#< "=" >#< "case" >#< _strat     >#< pp_parens _rhsIpp >#< "of"
                                    >-< indent 4 (
                                      pp_parens (nt_ >|< "_Syn" >#< "_val") >#< "-> _val"
                                    )
                               else _leftIpp >#< "=" >#< _rhsIpp
                               {-# LINE 1435 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule152 #-}
   rule152 = \  (_ :: ()) ->
     empty
   {-# INLINE rule153 #-}
   rule153 = \ ((_lhsIisDeclOfLet) :: Bool) ->
     _lhsIisDeclOfLet
   {-# INLINE rule154 #-}
   rule154 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule155 #-}
   rule155 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule156 #-}
   rule156 = \ ((_lhsIoutputfile) :: String) ->
     _lhsIoutputfile
   {-# INLINE rule157 #-}
   rule157 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule158 #-}
   rule158 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule159 #-}
   rule159 = \ ((_lhsIoutputfile) :: String) ->
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
         (T_Decl_vOut19 _hdIpp _hdIppa) = inv_Decl_s20 _hdX20 (T_Decl_vIn19 _hdOisDeclOfLet _hdOnested _hdOoptions _hdOoutputfile)
         (T_Decls_vOut22 _tlIpps) = inv_Decls_s23 _tlX23 (T_Decls_vIn22 _tlOisDeclOfLet _tlOnested _tlOoptions _tlOoutputfile)
         _lhsOpps :: PP_Docs
         _lhsOpps = rule160 _hdIpp _tlIpps
         _hdOisDeclOfLet = rule161 _lhsIisDeclOfLet
         _hdOnested = rule162 _lhsInested
         _hdOoptions = rule163 _lhsIoptions
         _hdOoutputfile = rule164 _lhsIoutputfile
         _tlOisDeclOfLet = rule165 _lhsIisDeclOfLet
         _tlOnested = rule166 _lhsInested
         _tlOoptions = rule167 _lhsIoptions
         _tlOoutputfile = rule168 _lhsIoutputfile
         !__result_ = T_Decls_vOut22 _lhsOpps
         in __result_ )
     in C_Decls_s23 v22
   {-# INLINE rule160 #-}
   {-# LINE 92 "./src-ag/PrintCleanCode.ag" #-}
   rule160 = \ ((_hdIpp) :: PP_Doc) ((_tlIpps) :: PP_Docs) ->
                     {-# LINE 92 "./src-ag/PrintCleanCode.ag" #-}
                     _hdIpp : _tlIpps
                     {-# LINE 1520 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule161 #-}
   rule161 = \ ((_lhsIisDeclOfLet) :: Bool) ->
     _lhsIisDeclOfLet
   {-# INLINE rule162 #-}
   rule162 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule163 #-}
   rule163 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule164 #-}
   rule164 = \ ((_lhsIoutputfile) :: String) ->
     _lhsIoutputfile
   {-# INLINE rule165 #-}
   rule165 = \ ((_lhsIisDeclOfLet) :: Bool) ->
     _lhsIisDeclOfLet
   {-# INLINE rule166 #-}
   rule166 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule167 #-}
   rule167 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule168 #-}
   rule168 = \ ((_lhsIoutputfile) :: String) ->
     _lhsIoutputfile
{-# NOINLINE sem_Decls_Nil #-}
sem_Decls_Nil ::  T_Decls 
sem_Decls_Nil  = T_Decls (return st23) where
   {-# NOINLINE st23 #-}
   !st23 = let
      v22 :: T_Decls_v22 
      v22 = \ !(T_Decls_vIn22 _lhsIisDeclOfLet _lhsInested _lhsIoptions _lhsIoutputfile) -> ( let
         _lhsOpps :: PP_Docs
         _lhsOpps = rule169  ()
         !__result_ = T_Decls_vOut22 _lhsOpps
         in __result_ )
     in C_Decls_s23 v22
   {-# INLINE rule169 #-}
   {-# LINE 93 "./src-ag/PrintCleanCode.ag" #-}
   rule169 = \  (_ :: ()) ->
                     {-# LINE 93 "./src-ag/PrintCleanCode.ag" #-}
                     []
                     {-# LINE 1562 "dist/build/PrintCleanCode.hs"#-}

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
         _lhsOpp = rule170 _bodyIpp _declsIpps
         _declsOisDeclOfLet = rule171  ()
         _declsOnested = rule172 _lhsInested
         _declsOoptions = rule173 _lhsIoptions
         _declsOoutputfile = rule174 _lhsIoutputfile
         _bodyOnested = rule175 _lhsInested
         _bodyOoptions = rule176 _lhsIoptions
         _bodyOoutputfile = rule177 _lhsIoutputfile
         !__result_ = T_Expr_vOut25 _lhsOpp
         in __result_ )
     in C_Expr_s26 v25
   {-# INLINE rule170 #-}
   {-# LINE 150 "./src-ag/PrintCleanCode.ag" #-}
   rule170 = \ ((_bodyIpp) :: PP_Doc) ((_declsIpps) :: PP_Docs) ->
                               {-# LINE 150 "./src-ag/PrintCleanCode.ag" #-}
                               pp_parens (    "let" >#< (vlist _declsIpps)
                                         >-< "in " >#< _bodyIpp
                                         )
                               {-# LINE 1640 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule171 #-}
   {-# LINE 437 "./src-ag/PrintCleanCode.ag" #-}
   rule171 = \  (_ :: ()) ->
                            {-# LINE 437 "./src-ag/PrintCleanCode.ag" #-}
                            True
                            {-# LINE 1646 "dist/build/PrintCleanCode.hs"#-}
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
         _lhsOpp = rule178 _altsIpps _exprIpp
         _exprOnested = rule179 _lhsInested
         _exprOoptions = rule180 _lhsIoptions
         _exprOoutputfile = rule181 _lhsIoutputfile
         _altsOnested = rule182 _lhsInested
         _altsOoptions = rule183 _lhsIoptions
         _altsOoutputfile = rule184 _lhsIoutputfile
         !__result_ = T_Expr_vOut25 _lhsOpp
         in __result_ )
     in C_Expr_s26 v25
   {-# INLINE rule178 #-}
   {-# LINE 153 "./src-ag/PrintCleanCode.ag" #-}
   rule178 = \ ((_altsIpps) :: PP_Docs) ((_exprIpp) :: PP_Doc) ->
                               {-# LINE 153 "./src-ag/PrintCleanCode.ag" #-}
                               pp_parens (    "case" >#< pp_parens _exprIpp >#< "of"
                                         >-< (vlist _altsIpps)
                                         )
                               {-# LINE 1694 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule179 #-}
   rule179 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule180 #-}
   rule180 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule181 #-}
   rule181 = \ ((_lhsIoutputfile) :: String) ->
     _lhsIoutputfile
   {-# INLINE rule182 #-}
   rule182 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule183 #-}
   rule183 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule184 #-}
   rule184 = \ ((_lhsIoutputfile) :: String) ->
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
         _lhsOpp = rule185 _bodyIpp _stmtsIpps
         _stmtsOisDeclOfLet = rule186  ()
         _stmtsOnested = rule187 _lhsInested
         _stmtsOoptions = rule188 _lhsIoptions
         _stmtsOoutputfile = rule189 _lhsIoutputfile
         _bodyOnested = rule190 _lhsInested
         _bodyOoptions = rule191 _lhsIoptions
         _bodyOoutputfile = rule192 _lhsIoutputfile
         !__result_ = T_Expr_vOut25 _lhsOpp
         in __result_ )
     in C_Expr_s26 v25
   {-# INLINE rule185 #-}
   {-# LINE 156 "./src-ag/PrintCleanCode.ag" #-}
   rule185 = \ ((_bodyIpp) :: PP_Doc) ((_stmtsIpps) :: PP_Docs) ->
                               {-# LINE 156 "./src-ag/PrintCleanCode.ag" #-}
                               pp_parens ( "do" >#< (   vlist _stmtsIpps
                                                    >-< ("return" >#< _bodyIpp))
                                         )
                               {-# LINE 1743 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule186 #-}
   {-# LINE 439 "./src-ag/PrintCleanCode.ag" #-}
   rule186 = \  (_ :: ()) ->
                            {-# LINE 439 "./src-ag/PrintCleanCode.ag" #-}
                            False
                            {-# LINE 1749 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule187 #-}
   rule187 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule188 #-}
   rule188 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule189 #-}
   rule189 = \ ((_lhsIoutputfile) :: String) ->
     _lhsIoutputfile
   {-# INLINE rule190 #-}
   rule190 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule191 #-}
   rule191 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule192 #-}
   rule192 = \ ((_lhsIoutputfile) :: String) ->
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
         _strictParams = rule193 _argsIpps _lhsIoptions
         _addBang = rule194 _lhsIoptions
         _lhsOpp :: PP_Doc
         _lhsOpp = rule195 _addBang _argsIpps _bodyIpp _strictParams
         _argsOnested = rule196 _lhsInested
         _argsOoptions = rule197 _lhsIoptions
         _argsOoutputfile = rule198 _lhsIoutputfile
         _bodyOnested = rule199 _lhsInested
         _bodyOoptions = rule200 _lhsIoptions
         _bodyOoutputfile = rule201 _lhsIoutputfile
         !__result_ = T_Expr_vOut25 _lhsOpp
         in __result_ )
     in C_Expr_s26 v25
   {-# INLINE rule193 #-}
   {-# LINE 159 "./src-ag/PrintCleanCode.ag" #-}
   rule193 = \ ((_argsIpps) :: PP_Docs) ((_lhsIoptions) :: Options) ->
                                    {-# LINE 159 "./src-ag/PrintCleanCode.ag" #-}
                                    if strictSems _lhsIoptions
                                    then _argsIpps
                                    else []
                                    {-# LINE 1799 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule194 #-}
   {-# LINE 162 "./src-ag/PrintCleanCode.ag" #-}
   rule194 = \ ((_lhsIoptions) :: Options) ->
                               {-# LINE 162 "./src-ag/PrintCleanCode.ag" #-}
                               if bangpats _lhsIoptions
                               then \p -> pp_parens ("!" >|< p)
                               else id
                               {-# LINE 1807 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule195 #-}
   {-# LINE 165 "./src-ag/PrintCleanCode.ag" #-}
   rule195 = \ _addBang ((_argsIpps) :: PP_Docs) ((_bodyIpp) :: PP_Doc) _strictParams ->
                               {-# LINE 165 "./src-ag/PrintCleanCode.ag" #-}
                               pp_parens (    "\\" >#< (vlist (map _addBang     _argsIpps)) >#< "->"
                                         >-< indent 4 (_strictParams     `ppMultiSeqV` _bodyIpp)
                                         )
                               {-# LINE 1815 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule196 #-}
   rule196 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule197 #-}
   rule197 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule198 #-}
   rule198 = \ ((_lhsIoutputfile) :: String) ->
     _lhsIoutputfile
   {-# INLINE rule199 #-}
   rule199 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule200 #-}
   rule200 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule201 #-}
   rule201 = \ ((_lhsIoutputfile) :: String) ->
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
         _lhsOpp = rule202 _exprsIpps _lhsInested
         _exprsOnested = rule203 _lhsInested
         _exprsOoptions = rule204 _lhsIoptions
         _exprsOoutputfile = rule205 _lhsIoutputfile
         !__result_ = T_Expr_vOut25 _lhsOpp
         in __result_ )
     in C_Expr_s26 v25
   {-# INLINE rule202 #-}
   {-# LINE 168 "./src-ag/PrintCleanCode.ag" #-}
   rule202 = \ ((_exprsIpps) :: PP_Docs) ((_lhsInested) :: Bool) ->
                               {-# LINE 168 "./src-ag/PrintCleanCode.ag" #-}
                               ppTuple _lhsInested _exprsIpps
                               {-# LINE 1856 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule203 #-}
   rule203 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule204 #-}
   rule204 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule205 #-}
   rule205 = \ ((_lhsIoutputfile) :: String) ->
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
         _lhsOpp = rule206 _exprsIpps _lhsInested
         _exprsOnested = rule207 _lhsInested
         _exprsOoptions = rule208 _lhsIoptions
         _exprsOoutputfile = rule209 _lhsIoutputfile
         !__result_ = T_Expr_vOut25 _lhsOpp
         in __result_ )
     in C_Expr_s26 v25
   {-# INLINE rule206 #-}
   {-# LINE 169 "./src-ag/PrintCleanCode.ag" #-}
   rule206 = \ ((_exprsIpps) :: PP_Docs) ((_lhsInested) :: Bool) ->
                                      {-# LINE 169 "./src-ag/PrintCleanCode.ag" #-}
                                      ppUnboxedTuple _lhsInested _exprsIpps
                                      {-# LINE 1888 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule207 #-}
   rule207 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule208 #-}
   rule208 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule209 #-}
   rule209 = \ ((_lhsIoutputfile) :: String) ->
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
         _lhsOpp = rule210 _argsIpps arg_name_
         _argsOnested = rule211 _lhsInested
         _argsOoptions = rule212 _lhsIoptions
         _argsOoutputfile = rule213 _lhsIoutputfile
         !__result_ = T_Expr_vOut25 _lhsOpp
         in __result_ )
     in C_Expr_s26 v25
   {-# INLINE rule210 #-}
   {-# LINE 170 "./src-ag/PrintCleanCode.ag" #-}
   rule210 = \ ((_argsIpps) :: PP_Docs) name_ ->
                               {-# LINE 170 "./src-ag/PrintCleanCode.ag" #-}
                               pp_parens $ name_ >#< hv_sp _argsIpps
                               {-# LINE 1920 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule211 #-}
   rule211 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule212 #-}
   rule212 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule213 #-}
   rule213 = \ ((_lhsIoutputfile) :: String) ->
     _lhsIoutputfile
{-# NOINLINE sem_Expr_SimpleExpr #-}
sem_Expr_SimpleExpr :: (String) -> T_Expr 
sem_Expr_SimpleExpr !arg_txt_ = T_Expr (return st26) where
   {-# NOINLINE st26 #-}
   !st26 = let
      v25 :: T_Expr_v25 
      v25 = \ !(T_Expr_vIn25 _lhsInested _lhsIoptions _lhsIoutputfile) -> ( let
         _lhsOpp :: PP_Doc
         _lhsOpp = rule214 arg_txt_
         !__result_ = T_Expr_vOut25 _lhsOpp
         in __result_ )
     in C_Expr_s26 v25
   {-# INLINE rule214 #-}
   {-# LINE 171 "./src-ag/PrintCleanCode.ag" #-}
   rule214 = \ txt_ ->
                               {-# LINE 171 "./src-ag/PrintCleanCode.ag" #-}
                               text txt_
                               {-# LINE 1947 "dist/build/PrintCleanCode.hs"#-}
{-# NOINLINE sem_Expr_TextExpr #-}
sem_Expr_TextExpr :: ([String]) -> T_Expr 
sem_Expr_TextExpr !arg_lns_ = T_Expr (return st26) where
   {-# NOINLINE st26 #-}
   !st26 = let
      v25 :: T_Expr_v25 
      v25 = \ !(T_Expr_vIn25 _lhsInested _lhsIoptions _lhsIoutputfile) -> ( let
         _lhsOpp :: PP_Doc
         _lhsOpp = rule215 arg_lns_
         !__result_ = T_Expr_vOut25 _lhsOpp
         in __result_ )
     in C_Expr_s26 v25
   {-# INLINE rule215 #-}
   {-# LINE 172 "./src-ag/PrintCleanCode.ag" #-}
   rule215 = \ lns_ ->
                               {-# LINE 172 "./src-ag/PrintCleanCode.ag" #-}
                               vlist (map text lns_)
                               {-# LINE 1965 "dist/build/PrintCleanCode.hs"#-}
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
         _lhsOpp = rule216 _exprIpp arg_txt_
         _exprOnested = rule217 _lhsInested
         _exprOoptions = rule218 _lhsIoptions
         _exprOoutputfile = rule219 _lhsIoutputfile
         !__result_ = T_Expr_vOut25 _lhsOpp
         in __result_ )
     in C_Expr_s26 v25
   {-# INLINE rule216 #-}
   {-# LINE 173 "./src-ag/PrintCleanCode.ag" #-}
   rule216 = \ ((_exprIpp) :: PP_Doc) txt_ ->
                               {-# LINE 173 "./src-ag/PrintCleanCode.ag" #-}
                               "trace" >#< (   pp_parens ("\"" >|< text txt_ >|< "\"")
                                           >-< pp_parens _exprIpp
                                           )
                               {-# LINE 1990 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule217 #-}
   rule217 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule218 #-}
   rule218 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule219 #-}
   rule219 = \ ((_lhsIoutputfile) :: String) ->
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
         _lhsOpp = rule220 _exprIpp arg_onLeftSide_ arg_onNewLine_ arg_txt_
         _exprOnested = rule221 _lhsInested
         _exprOoptions = rule222 _lhsIoptions
         _exprOoutputfile = rule223 _lhsIoutputfile
         !__result_ = T_Expr_vOut25 _lhsOpp
         in __result_ )
     in C_Expr_s26 v25
   {-# INLINE rule220 #-}
   {-# LINE 176 "./src-ag/PrintCleanCode.ag" #-}
   rule220 = \ ((_exprIpp) :: PP_Doc) onLeftSide_ onNewLine_ txt_ ->
                               {-# LINE 176 "./src-ag/PrintCleanCode.ag" #-}
                               let pragmaDoc = "/*#" >#< txt_ >#< "#*/"
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
                               {-# LINE 2032 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule221 #-}
   rule221 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule222 #-}
   rule222 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule223 #-}
   rule223 = \ ((_lhsIoutputfile) :: String) ->
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
         _lhsOpp = rule224 _exprIpp _lhsIoutputfile
         _exprOnested = rule225 _lhsInested
         _exprOoptions = rule226 _lhsIoptions
         _exprOoutputfile = rule227 _lhsIoutputfile
         !__result_ = T_Expr_vOut25 _lhsOpp
         in __result_ )
     in C_Expr_s26 v25
   {-# INLINE rule224 #-}
   {-# LINE 187 "./src-ag/PrintCleanCode.ag" #-}
   rule224 = \ ((_exprIpp) :: PP_Doc) ((_lhsIoutputfile) :: String) ->
                               {-# LINE 187 "./src-ag/PrintCleanCode.ag" #-}
                               _exprIpp >-< "/*# LINE" >#< ppWithLineNr (\n -> pp $ show $ n + 1) >#< show _lhsIoutputfile >#< "#*/"
                                        >-< ""
                               {-# LINE 2065 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule225 #-}
   rule225 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule226 #-}
   rule226 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule227 #-}
   rule227 = \ ((_lhsIoutputfile) :: String) ->
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
         (T_Type_vOut49 _tpIcopy _tpIpp _tpIprec) = inv_Type_s50 _tpX50 (T_Type_vIn49 _tpOnested)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule228 _exprIpp _tpIpp
         _exprOnested = rule229 _lhsInested
         _exprOoptions = rule230 _lhsIoptions
         _exprOoutputfile = rule231 _lhsIoutputfile
         _tpOnested = rule232 _lhsInested
         !__result_ = T_Expr_vOut25 _lhsOpp
         in __result_ )
     in C_Expr_s26 v25
   {-# INLINE rule228 #-}
   {-# LINE 189 "./src-ag/PrintCleanCode.ag" #-}
   rule228 = \ ((_exprIpp) :: PP_Doc) ((_tpIpp) :: PP_Doc) ->
                               {-# LINE 189 "./src-ag/PrintCleanCode.ag" #-}
                               pp_parens (_exprIpp >#< "::" >#< _tpIpp)
                               {-# LINE 2100 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule229 #-}
   rule229 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule230 #-}
   rule230 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule231 #-}
   rule231 = \ ((_lhsIoutputfile) :: String) ->
     _lhsIoutputfile
   {-# INLINE rule232 #-}
   rule232 = \ ((_lhsInested) :: Bool) ->
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
         _lhsOpp = rule233 _exprIpp _lhsIoptions arg_nt_
         _exprOnested = rule234 _lhsInested
         _exprOoptions = rule235 _lhsIoptions
         _exprOoutputfile = rule236 _lhsIoutputfile
         !__result_ = T_Expr_vOut25 _lhsOpp
         in __result_ )
     in C_Expr_s26 v25
   {-# INLINE rule233 #-}
   {-# LINE 190 "./src-ag/PrintCleanCode.ag" #-}
   rule233 = \ ((_exprIpp) :: PP_Doc) ((_lhsIoptions) :: Options) nt_ ->
                               {-# LINE 190 "./src-ag/PrintCleanCode.ag" #-}
                               if breadthFirst _lhsIoptions
                               then "final" >#<
                                    pp_parens (nt_ >|< "_Syn" >#< pp_parens _exprIpp)
                               else _exprIpp
                               {-# LINE 2138 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule234 #-}
   rule234 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule235 #-}
   rule235 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule236 #-}
   rule236 = \ ((_lhsIoutputfile) :: String) ->
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
         _lhsOpp = rule237 _argsIpps _exprIpp _lhsIoptions arg_nt_
         _exprOnested = rule238 _lhsInested
         _exprOoptions = rule239 _lhsIoptions
         _exprOoutputfile = rule240 _lhsIoutputfile
         _argsOnested = rule241 _lhsInested
         _argsOoptions = rule242 _lhsIoptions
         _argsOoutputfile = rule243 _lhsIoutputfile
         !__result_ = T_Expr_vOut25 _lhsOpp
         in __result_ )
     in C_Expr_s26 v25
   {-# INLINE rule237 #-}
   {-# LINE 194 "./src-ag/PrintCleanCode.ag" #-}
   rule237 = \ ((_argsIpps) :: PP_Docs) ((_exprIpp) :: PP_Doc) ((_lhsIoptions) :: Options) nt_ ->
                               {-# LINE 194 "./src-ag/PrintCleanCode.ag" #-}
                               if breadthFirst _lhsIoptions
                               then "invoke" >#< pp_parens _exprIpp >#< pp_parens (
                                     nt_ >|< "_Inh" >#< pp_parens (ppTuple False _argsIpps))
                               else _exprIpp >#< hv_sp _argsIpps
                               {-# LINE 2178 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule238 #-}
   rule238 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule239 #-}
   rule239 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule240 #-}
   rule240 = \ ((_lhsIoutputfile) :: String) ->
     _lhsIoutputfile
   {-# INLINE rule241 #-}
   rule241 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule242 #-}
   rule242 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule243 #-}
   rule243 = \ ((_lhsIoutputfile) :: String) ->
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
         _lhsOpp = rule244 _exprIpp _leftIpp _lhsIoptions _rhsIpp arg_nt_
         _leftOisDeclOfLet = rule245  ()
         _exprOnested = rule246 _lhsInested
         _exprOoptions = rule247 _lhsIoptions
         _exprOoutputfile = rule248 _lhsIoutputfile
         _leftOnested = rule249 _lhsInested
         _leftOoptions = rule250 _lhsIoptions
         _leftOoutputfile = rule251 _lhsIoutputfile
         _rhsOnested = rule252 _lhsInested
         _rhsOoptions = rule253 _lhsIoptions
         _rhsOoutputfile = rule254 _lhsIoutputfile
         !__result_ = T_Expr_vOut25 _lhsOpp
         in __result_ )
     in C_Expr_s26 v25
   {-# INLINE rule244 #-}
   {-# LINE 198 "./src-ag/PrintCleanCode.ag" #-}
   rule244 = \ ((_exprIpp) :: PP_Doc) ((_leftIpp) :: PP_Doc) ((_lhsIoptions) :: Options) ((_rhsIpp) :: PP_Doc) nt_ ->
                               {-# LINE 198 "./src-ag/PrintCleanCode.ag" #-}
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
                               {-# LINE 2241 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule245 #-}
   {-# LINE 441 "./src-ag/PrintCleanCode.ag" #-}
   rule245 = \  (_ :: ()) ->
                           {-# LINE 441 "./src-ag/PrintCleanCode.ag" #-}
                           False
                           {-# LINE 2247 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule246 #-}
   rule246 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule247 #-}
   rule247 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule248 #-}
   rule248 = \ ((_lhsIoutputfile) :: String) ->
     _lhsIoutputfile
   {-# INLINE rule249 #-}
   rule249 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule250 #-}
   rule250 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule251 #-}
   rule251 = \ ((_lhsIoutputfile) :: String) ->
     _lhsIoutputfile
   {-# INLINE rule252 #-}
   rule252 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule253 #-}
   rule253 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule254 #-}
   rule254 = \ ((_lhsIoutputfile) :: String) ->
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
         _strictParams = rule255 _argsIpps _lhsIoptions
         _addBang = rule256 _lhsIoptions
         _lhsOpp :: PP_Doc
         _lhsOpp = rule257 _addBang _argsIpps _bodyIpp _lhsIoptions _strictParams arg_nt_
         _argsOnested = rule258 _lhsInested
         _argsOoptions = rule259 _lhsIoptions
         _argsOoutputfile = rule260 _lhsIoutputfile
         _bodyOnested = rule261 _lhsInested
         _bodyOoptions = rule262 _lhsIoptions
         _bodyOoutputfile = rule263 _lhsIoutputfile
         !__result_ = T_Expr_vOut25 _lhsOpp
         in __result_ )
     in C_Expr_s26 v25
   {-# INLINE rule255 #-}
   {-# LINE 210 "./src-ag/PrintCleanCode.ag" #-}
   rule255 = \ ((_argsIpps) :: PP_Docs) ((_lhsIoptions) :: Options) ->
                                    {-# LINE 210 "./src-ag/PrintCleanCode.ag" #-}
                                    if strictSems _lhsIoptions
                                    then _argsIpps
                                    else []
                                    {-# LINE 2306 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule256 #-}
   {-# LINE 213 "./src-ag/PrintCleanCode.ag" #-}
   rule256 = \ ((_lhsIoptions) :: Options) ->
                               {-# LINE 213 "./src-ag/PrintCleanCode.ag" #-}
                               if bangpats _lhsIoptions
                               then \p -> pp_parens ("!" >|< p)
                               else id
                               {-# LINE 2314 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule257 #-}
   {-# LINE 216 "./src-ag/PrintCleanCode.ag" #-}
   rule257 = \ _addBang ((_argsIpps) :: PP_Docs) ((_bodyIpp) :: PP_Doc) ((_lhsIoptions) :: Options) _strictParams nt_ ->
                               {-# LINE 216 "./src-ag/PrintCleanCode.ag" #-}
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
                               {-# LINE 2329 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule258 #-}
   rule258 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule259 #-}
   rule259 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule260 #-}
   rule260 = \ ((_lhsIoutputfile) :: String) ->
     _lhsIoutputfile
   {-# INLINE rule261 #-}
   rule261 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule262 #-}
   rule262 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule263 #-}
   rule263 = \ ((_lhsIoutputfile) :: String) ->
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
         _lhsOpps = rule264 _hdIpp _tlIpps
         _hdOnested = rule265 _lhsInested
         _hdOoptions = rule266 _lhsIoptions
         _hdOoutputfile = rule267 _lhsIoutputfile
         _tlOnested = rule268 _lhsInested
         _tlOoptions = rule269 _lhsIoptions
         _tlOoutputfile = rule270 _lhsIoutputfile
         !__result_ = T_Exprs_vOut28 _lhsOpps
         in __result_ )
     in C_Exprs_s29 v28
   {-# INLINE rule264 #-}
   {-# LINE 68 "./src-ag/PrintCleanCode.ag" #-}
   rule264 = \ ((_hdIpp) :: PP_Doc) ((_tlIpps) :: PP_Docs) ->
                     {-# LINE 68 "./src-ag/PrintCleanCode.ag" #-}
                     _hdIpp : _tlIpps
                     {-# LINE 2406 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule265 #-}
   rule265 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule266 #-}
   rule266 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule267 #-}
   rule267 = \ ((_lhsIoutputfile) :: String) ->
     _lhsIoutputfile
   {-# INLINE rule268 #-}
   rule268 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule269 #-}
   rule269 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule270 #-}
   rule270 = \ ((_lhsIoutputfile) :: String) ->
     _lhsIoutputfile
{-# NOINLINE sem_Exprs_Nil #-}
sem_Exprs_Nil ::  T_Exprs 
sem_Exprs_Nil  = T_Exprs (return st29) where
   {-# NOINLINE st29 #-}
   !st29 = let
      v28 :: T_Exprs_v28 
      v28 = \ !(T_Exprs_vIn28 _lhsInested _lhsIoptions _lhsIoutputfile) -> ( let
         _lhsOpps :: PP_Docs
         _lhsOpps = rule271  ()
         !__result_ = T_Exprs_vOut28 _lhsOpps
         in __result_ )
     in C_Exprs_s29 v28
   {-# INLINE rule271 #-}
   {-# LINE 69 "./src-ag/PrintCleanCode.ag" #-}
   rule271 = \  (_ :: ()) ->
                     {-# LINE 69 "./src-ag/PrintCleanCode.ag" #-}
                     []
                     {-# LINE 2442 "dist/build/PrintCleanCode.hs"#-}

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
         _addStrictGuard = rule272 _hasStrictVars _lhsIoptions _strictGuard
         _strictGuard = rule273 _pat3IstrictVars
         _hasStrictVars = rule274 _pat3IstrictVars
         _lhsOpp :: PP_Doc
         _lhsOpp = rule275 _addStrictGuard _pat3Ipp
         _pat3ObelowIrrefutable = rule276  ()
         _pat3OisDeclOfLet = rule277 _lhsIisDeclOfLet
         _pat3Ooptions = rule278 _lhsIoptions
         !__result_ = T_Lhs_vOut31 _lhsOpp
         in __result_ )
     in C_Lhs_s32 v31
   {-# INLINE rule272 #-}
   {-# LINE 248 "./src-ag/PrintCleanCode.ag" #-}
   rule272 = \ _hasStrictVars ((_lhsIoptions) :: Options) _strictGuard ->
                             {-# LINE 248 "./src-ag/PrintCleanCode.ag" #-}
                             if strictCases _lhsIoptions && _hasStrictVars     then \v -> v >#< "|" >#< _strictGuard     else id
                             {-# LINE 2504 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule273 #-}
   {-# LINE 250 "./src-ag/PrintCleanCode.ag" #-}
   rule273 = \ ((_pat3IstrictVars) :: [PP_Doc]) ->
                          {-# LINE 250 "./src-ag/PrintCleanCode.ag" #-}
                          _pat3IstrictVars `ppMultiSeqH` (pp "True")
                          {-# LINE 2510 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule274 #-}
   {-# LINE 251 "./src-ag/PrintCleanCode.ag" #-}
   rule274 = \ ((_pat3IstrictVars) :: [PP_Doc]) ->
                            {-# LINE 251 "./src-ag/PrintCleanCode.ag" #-}
                            not (null _pat3IstrictVars)
                            {-# LINE 2516 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule275 #-}
   {-# LINE 268 "./src-ag/PrintCleanCode.ag" #-}
   rule275 = \ _addStrictGuard ((_pat3Ipp) :: PP_Doc) ->
                               {-# LINE 268 "./src-ag/PrintCleanCode.ag" #-}
                               _addStrictGuard     _pat3Ipp
                               {-# LINE 2522 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule276 #-}
   {-# LINE 402 "./src-ag/PrintCleanCode.ag" #-}
   rule276 = \  (_ :: ()) ->
                                {-# LINE 402 "./src-ag/PrintCleanCode.ag" #-}
                                False
                                {-# LINE 2528 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule277 #-}
   rule277 = \ ((_lhsIisDeclOfLet) :: Bool) ->
     _lhsIisDeclOfLet
   {-# INLINE rule278 #-}
   rule278 = \ ((_lhsIoptions) :: Options) ->
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
         _lhsOpp = rule279 _pat3Ipp'
         _pat3ObelowIrrefutable = rule280  ()
         _pat3OisDeclOfLet = rule281 _lhsIisDeclOfLet
         _pat3Ooptions = rule282 _lhsIoptions
         !__result_ = T_Lhs_vOut31 _lhsOpp
         in __result_ )
     in C_Lhs_s32 v31
   {-# INLINE rule279 #-}
   {-# LINE 269 "./src-ag/PrintCleanCode.ag" #-}
   rule279 = \ ((_pat3Ipp') :: PP_Doc) ->
                               {-# LINE 269 "./src-ag/PrintCleanCode.ag" #-}
                               _pat3Ipp'
                               {-# LINE 2557 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule280 #-}
   {-# LINE 402 "./src-ag/PrintCleanCode.ag" #-}
   rule280 = \  (_ :: ()) ->
                                {-# LINE 402 "./src-ag/PrintCleanCode.ag" #-}
                                False
                                {-# LINE 2563 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule281 #-}
   rule281 = \ ((_lhsIisDeclOfLet) :: Bool) ->
     _lhsIisDeclOfLet
   {-# INLINE rule282 #-}
   rule282 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Lhs_TupleLhs #-}
sem_Lhs_TupleLhs :: ([String]) -> T_Lhs 
sem_Lhs_TupleLhs !arg_comps_ = T_Lhs (return st32) where
   {-# NOINLINE st32 #-}
   !st32 = let
      v31 :: T_Lhs_v31 
      v31 = \ !(T_Lhs_vIn31 _lhsIisDeclOfLet _lhsInested _lhsIoptions _lhsIoutputfile) -> ( let
         _addStrictGuard = rule283 _hasStrictVars _lhsIoptions _strictGuard
         _strictGuard = rule284 _lhsIisDeclOfLet _lhsIoptions arg_comps_
         _hasStrictVars = rule285 arg_comps_
         _addBang = rule286 _lhsIoptions
         _lhsOpp :: PP_Doc
         _lhsOpp = rule287 _addBang _addStrictGuard _lhsInested arg_comps_
         !__result_ = T_Lhs_vOut31 _lhsOpp
         in __result_ )
     in C_Lhs_s32 v31
   {-# INLINE rule283 #-}
   {-# LINE 248 "./src-ag/PrintCleanCode.ag" #-}
   rule283 = \ _hasStrictVars ((_lhsIoptions) :: Options) _strictGuard ->
                             {-# LINE 248 "./src-ag/PrintCleanCode.ag" #-}
                             if strictCases _lhsIoptions && _hasStrictVars     then \v -> v >#< "|" >#< _strictGuard     else id
                             {-# LINE 2591 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule284 #-}
   {-# LINE 253 "./src-ag/PrintCleanCode.ag" #-}
   rule284 = \ ((_lhsIisDeclOfLet) :: Bool) ((_lhsIoptions) :: Options) comps_ ->
                          {-# LINE 253 "./src-ag/PrintCleanCode.ag" #-}
                          if stricterCases _lhsIoptions && not _lhsIisDeclOfLet
                          then map text comps_ `ppMultiSeqH` (pp "True")
                          else pp "True"
                          {-# LINE 2599 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule285 #-}
   {-# LINE 256 "./src-ag/PrintCleanCode.ag" #-}
   rule285 = \ comps_ ->
                            {-# LINE 256 "./src-ag/PrintCleanCode.ag" #-}
                            not (null comps_)
                            {-# LINE 2605 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule286 #-}
   {-# LINE 264 "./src-ag/PrintCleanCode.ag" #-}
   rule286 = \ ((_lhsIoptions) :: Options) ->
                      {-# LINE 264 "./src-ag/PrintCleanCode.ag" #-}
                      if bangpats _lhsIoptions
                               then \p -> "!" >|< p
                               else id
                      {-# LINE 2613 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule287 #-}
   {-# LINE 270 "./src-ag/PrintCleanCode.ag" #-}
   rule287 = \ _addBang _addStrictGuard ((_lhsInested) :: Bool) comps_ ->
                               {-# LINE 270 "./src-ag/PrintCleanCode.ag" #-}
                               _addStrictGuard     $ ppTuple _lhsInested (map (_addBang     . text) comps_)
                               {-# LINE 2619 "dist/build/PrintCleanCode.hs"#-}
{-# NOINLINE sem_Lhs_UnboxedTupleLhs #-}
sem_Lhs_UnboxedTupleLhs :: ([String]) -> T_Lhs 
sem_Lhs_UnboxedTupleLhs !arg_comps_ = T_Lhs (return st32) where
   {-# NOINLINE st32 #-}
   !st32 = let
      v31 :: T_Lhs_v31 
      v31 = \ !(T_Lhs_vIn31 _lhsIisDeclOfLet _lhsInested _lhsIoptions _lhsIoutputfile) -> ( let
         _addStrictGuard = rule288 _hasStrictVars _lhsIoptions _strictGuard
         _strictGuard = rule289 _lhsIisDeclOfLet _lhsIoptions arg_comps_
         _hasStrictVars = rule290 arg_comps_
         _addBang = rule291 _lhsIoptions
         _lhsOpp :: PP_Doc
         _lhsOpp = rule292 _addBang _addStrictGuard _lhsInested arg_comps_
         !__result_ = T_Lhs_vOut31 _lhsOpp
         in __result_ )
     in C_Lhs_s32 v31
   {-# INLINE rule288 #-}
   {-# LINE 248 "./src-ag/PrintCleanCode.ag" #-}
   rule288 = \ _hasStrictVars ((_lhsIoptions) :: Options) _strictGuard ->
                             {-# LINE 248 "./src-ag/PrintCleanCode.ag" #-}
                             if strictCases _lhsIoptions && _hasStrictVars     then \v -> v >#< "|" >#< _strictGuard     else id
                             {-# LINE 2641 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule289 #-}
   {-# LINE 253 "./src-ag/PrintCleanCode.ag" #-}
   rule289 = \ ((_lhsIisDeclOfLet) :: Bool) ((_lhsIoptions) :: Options) comps_ ->
                          {-# LINE 253 "./src-ag/PrintCleanCode.ag" #-}
                          if stricterCases _lhsIoptions && not _lhsIisDeclOfLet
                          then map text comps_ `ppMultiSeqH` (pp "True")
                          else pp "True"
                          {-# LINE 2649 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule290 #-}
   {-# LINE 256 "./src-ag/PrintCleanCode.ag" #-}
   rule290 = \ comps_ ->
                            {-# LINE 256 "./src-ag/PrintCleanCode.ag" #-}
                            not (null comps_)
                            {-# LINE 2655 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule291 #-}
   {-# LINE 264 "./src-ag/PrintCleanCode.ag" #-}
   rule291 = \ ((_lhsIoptions) :: Options) ->
                      {-# LINE 264 "./src-ag/PrintCleanCode.ag" #-}
                      if bangpats _lhsIoptions
                               then \p -> "!" >|< p
                               else id
                      {-# LINE 2663 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule292 #-}
   {-# LINE 271 "./src-ag/PrintCleanCode.ag" #-}
   rule292 = \ _addBang _addStrictGuard ((_lhsInested) :: Bool) comps_ ->
                                      {-# LINE 271 "./src-ag/PrintCleanCode.ag" #-}
                                      _addStrictGuard     $ ppUnboxedTuple _lhsInested (map (_addBang     . text) comps_)
                                      {-# LINE 2669 "dist/build/PrintCleanCode.hs"#-}
{-# NOINLINE sem_Lhs_Fun #-}
sem_Lhs_Fun :: (String) -> T_Exprs  -> T_Lhs 
sem_Lhs_Fun !arg_name_ arg_args_ = T_Lhs (return st32) where
   {-# NOINLINE st32 #-}
   !st32 = let
      v31 :: T_Lhs_v31 
      v31 = \ !(T_Lhs_vIn31 _lhsIisDeclOfLet _lhsInested _lhsIoptions _lhsIoutputfile) -> ( let
         _argsX29 = Control.Monad.Identity.runIdentity (attach_T_Exprs (arg_args_))
         (T_Exprs_vOut28 _argsIpps) = inv_Exprs_s29 _argsX29 (T_Exprs_vIn28 _argsOnested _argsOoptions _argsOoutputfile)
         _addStrictGuard = rule293 _hasStrictVars _lhsIoptions _strictGuard
         _hasStrictVars = rule294 _argsIpps
         _strictGuard = rule295 _argsIpps
         _addBang = rule296 _lhsIoptions
         _lhsOpp :: PP_Doc
         _lhsOpp = rule297 _addBang _addStrictGuard _argsIpps arg_name_
         _argsOnested = rule298 _lhsInested
         _argsOoptions = rule299 _lhsIoptions
         _argsOoutputfile = rule300 _lhsIoutputfile
         !__result_ = T_Lhs_vOut31 _lhsOpp
         in __result_ )
     in C_Lhs_s32 v31
   {-# INLINE rule293 #-}
   {-# LINE 259 "./src-ag/PrintCleanCode.ag" #-}
   rule293 = \ _hasStrictVars ((_lhsIoptions) :: Options) _strictGuard ->
                             {-# LINE 259 "./src-ag/PrintCleanCode.ag" #-}
                             if strictSems _lhsIoptions && _hasStrictVars     then \v -> v >#< "|" >#< _strictGuard     else id
                             {-# LINE 2696 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule294 #-}
   {-# LINE 260 "./src-ag/PrintCleanCode.ag" #-}
   rule294 = \ ((_argsIpps) :: PP_Docs) ->
                             {-# LINE 260 "./src-ag/PrintCleanCode.ag" #-}
                             not (null _argsIpps)
                             {-# LINE 2702 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule295 #-}
   {-# LINE 261 "./src-ag/PrintCleanCode.ag" #-}
   rule295 = \ ((_argsIpps) :: PP_Docs) ->
                             {-# LINE 261 "./src-ag/PrintCleanCode.ag" #-}
                             _argsIpps `ppMultiSeqH` (pp "True")
                             {-# LINE 2708 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule296 #-}
   {-# LINE 264 "./src-ag/PrintCleanCode.ag" #-}
   rule296 = \ ((_lhsIoptions) :: Options) ->
                      {-# LINE 264 "./src-ag/PrintCleanCode.ag" #-}
                      if bangpats _lhsIoptions
                               then \p -> "!" >|< p
                               else id
                      {-# LINE 2716 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule297 #-}
   {-# LINE 272 "./src-ag/PrintCleanCode.ag" #-}
   rule297 = \ _addBang _addStrictGuard ((_argsIpps) :: PP_Docs) name_ ->
                               {-# LINE 272 "./src-ag/PrintCleanCode.ag" #-}
                               _addStrictGuard     (name_ >#< hv_sp (map _addBang     _argsIpps))
                               {-# LINE 2722 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule298 #-}
   rule298 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule299 #-}
   rule299 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule300 #-}
   rule300 = \ ((_lhsIoutputfile) :: String) ->
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
         _lhsOpp = rule301 _subIpp arg_name_
         _subOisDeclOfLet = rule302 _lhsIisDeclOfLet
         _subOnested = rule303 _lhsInested
         _subOoptions = rule304 _lhsIoptions
         _subOoutputfile = rule305 _lhsIoutputfile
         !__result_ = T_Lhs_vOut31 _lhsOpp
         in __result_ )
     in C_Lhs_s32 v31
   {-# INLINE rule301 #-}
   {-# LINE 273 "./src-ag/PrintCleanCode.ag" #-}
   rule301 = \ ((_subIpp) :: PP_Doc) name_ ->
                               {-# LINE 273 "./src-ag/PrintCleanCode.ag" #-}
                               pp_parens (name_ >#< _subIpp)
                               {-# LINE 2755 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule302 #-}
   rule302 = \ ((_lhsIisDeclOfLet) :: Bool) ->
     _lhsIisDeclOfLet
   {-# INLINE rule303 #-}
   rule303 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule304 #-}
   rule304 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule305 #-}
   rule305 = \ ((_lhsIoutputfile) :: String) ->
     _lhsIoutputfile

-- NamedType ---------------------------------------------------
-- wrapper
data Inh_NamedType  = Inh_NamedType { nested_Inh_NamedType :: !(Bool) }
data Syn_NamedType  = Syn_NamedType { pp_Syn_NamedType :: !(PP_Doc), ppa_Syn_NamedType :: !(PP_Doc) }
{-# INLINABLE wrap_NamedType #-}
wrap_NamedType :: T_NamedType  -> Inh_NamedType  -> (Syn_NamedType )
wrap_NamedType !(T_NamedType act) !(Inh_NamedType _lhsInested) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_NamedType_vIn34 _lhsInested
        !(T_NamedType_vOut34 _lhsOpp _lhsOppa) <- return (inv_NamedType_s35 sem arg)
        return (Syn_NamedType _lhsOpp _lhsOppa)
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
data T_NamedType_vOut34  = T_NamedType_vOut34 (PP_Doc) (PP_Doc)
{-# NOINLINE sem_NamedType_Named #-}
sem_NamedType_Named :: (Bool) -> (String) -> T_Type  -> T_NamedType 
sem_NamedType_Named !arg_strict_ !arg_name_ arg_tp_ = T_NamedType (return st35) where
   {-# NOINLINE st35 #-}
   !st35 = let
      v34 :: T_NamedType_v34 
      v34 = \ !(T_NamedType_vIn34 _lhsInested) -> ( let
         _tpX50 = Control.Monad.Identity.runIdentity (attach_T_Type (arg_tp_))
         (T_Type_vOut49 _tpIcopy _tpIpp _tpIprec) = inv_Type_s50 _tpX50 (T_Type_vIn49 _tpOnested)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule306 _tpIpp arg_strict_
         _lhsOppa :: PP_Doc
         _lhsOppa = rule307 arg_name_
         _tpOnested = rule308 _lhsInested
         !__result_ = T_NamedType_vOut34 _lhsOpp _lhsOppa
         in __result_ )
     in C_NamedType_s35 v34
   {-# INLINE rule306 #-}
   {-# LINE 240 "./src-ag/PrintCleanCode.ag" #-}
   rule306 = \ ((_tpIpp) :: PP_Doc) strict_ ->
                               {-# LINE 240 "./src-ag/PrintCleanCode.ag" #-}
                               if strict_
                                 then "!" >|< pp_parens _tpIpp
                                 else _tpIpp
                               {-# LINE 2823 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule307 #-}
   {-# LINE 244 "./src-ag/PrintCleanCode.ag" #-}
   rule307 = \ name_ ->
                               {-# LINE 244 "./src-ag/PrintCleanCode.ag" #-}
                               pp name_
                               {-# LINE 2829 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule308 #-}
   rule308 = \ ((_lhsInested) :: Bool) ->
     _lhsInested

-- NamedTypes --------------------------------------------------
-- wrapper
data Inh_NamedTypes  = Inh_NamedTypes { nested_Inh_NamedTypes :: !(Bool) }
data Syn_NamedTypes  = Syn_NamedTypes { ppas_Syn_NamedTypes :: !(PP_Docs), pps_Syn_NamedTypes :: !(PP_Docs) }
{-# INLINABLE wrap_NamedTypes #-}
wrap_NamedTypes :: T_NamedTypes  -> Inh_NamedTypes  -> (Syn_NamedTypes )
wrap_NamedTypes !(T_NamedTypes act) !(Inh_NamedTypes _lhsInested) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_NamedTypes_vIn37 _lhsInested
        !(T_NamedTypes_vOut37 _lhsOppas _lhsOpps) <- return (inv_NamedTypes_s38 sem arg)
        return (Syn_NamedTypes _lhsOppas _lhsOpps)
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
data T_NamedTypes_vOut37  = T_NamedTypes_vOut37 (PP_Docs) (PP_Docs)
{-# NOINLINE sem_NamedTypes_Cons #-}
sem_NamedTypes_Cons :: T_NamedType  -> T_NamedTypes  -> T_NamedTypes 
sem_NamedTypes_Cons arg_hd_ arg_tl_ = T_NamedTypes (return st38) where
   {-# NOINLINE st38 #-}
   !st38 = let
      v37 :: T_NamedTypes_v37 
      v37 = \ !(T_NamedTypes_vIn37 _lhsInested) -> ( let
         _hdX35 = Control.Monad.Identity.runIdentity (attach_T_NamedType (arg_hd_))
         _tlX38 = Control.Monad.Identity.runIdentity (attach_T_NamedTypes (arg_tl_))
         (T_NamedType_vOut34 _hdIpp _hdIppa) = inv_NamedType_s35 _hdX35 (T_NamedType_vIn34 _hdOnested)
         (T_NamedTypes_vOut37 _tlIppas _tlIpps) = inv_NamedTypes_s38 _tlX38 (T_NamedTypes_vIn37 _tlOnested)
         _lhsOpps :: PP_Docs
         _lhsOpps = rule309 _hdIpp _tlIpps
         _lhsOppas :: PP_Docs
         _lhsOppas = rule310 _hdIppa _tlIppas
         _hdOnested = rule311 _lhsInested
         _tlOnested = rule312 _lhsInested
         !__result_ = T_NamedTypes_vOut37 _lhsOppas _lhsOpps
         in __result_ )
     in C_NamedTypes_s38 v37
   {-# INLINE rule309 #-}
   {-# LINE 86 "./src-ag/PrintCleanCode.ag" #-}
   rule309 = \ ((_hdIpp) :: PP_Doc) ((_tlIpps) :: PP_Docs) ->
                      {-# LINE 86 "./src-ag/PrintCleanCode.ag" #-}
                      _hdIpp : _tlIpps
                      {-# LINE 2889 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule310 #-}
   {-# LINE 87 "./src-ag/PrintCleanCode.ag" #-}
   rule310 = \ ((_hdIppa) :: PP_Doc) ((_tlIppas) :: PP_Docs) ->
                      {-# LINE 87 "./src-ag/PrintCleanCode.ag" #-}
                      _hdIppa : _tlIppas
                      {-# LINE 2895 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule311 #-}
   rule311 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule312 #-}
   rule312 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
{-# NOINLINE sem_NamedTypes_Nil #-}
sem_NamedTypes_Nil ::  T_NamedTypes 
sem_NamedTypes_Nil  = T_NamedTypes (return st38) where
   {-# NOINLINE st38 #-}
   !st38 = let
      v37 :: T_NamedTypes_v37 
      v37 = \ !(T_NamedTypes_vIn37 _lhsInested) -> ( let
         _lhsOpps :: PP_Docs
         _lhsOpps = rule313  ()
         _lhsOppas :: PP_Docs
         _lhsOppas = rule314  ()
         !__result_ = T_NamedTypes_vOut37 _lhsOppas _lhsOpps
         in __result_ )
     in C_NamedTypes_s38 v37
   {-# INLINE rule313 #-}
   {-# LINE 88 "./src-ag/PrintCleanCode.ag" #-}
   rule313 = \  (_ :: ()) ->
                      {-# LINE 88 "./src-ag/PrintCleanCode.ag" #-}
                      []
                      {-# LINE 2921 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule314 #-}
   {-# LINE 89 "./src-ag/PrintCleanCode.ag" #-}
   rule314 = \  (_ :: ()) ->
                      {-# LINE 89 "./src-ag/PrintCleanCode.ag" #-}
                      []
                      {-# LINE 2927 "dist/build/PrintCleanCode.hs"#-}

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
         _addBang = rule315 _lhsIbelowIrrefutable _lhsIisDeclOfLet _lhsIoptions
         _lhsOpp :: PP_Doc
         _lhsOpp = rule316 _addBang _patsIpps arg_name_
         _lhsOisUnderscore :: Bool
         _lhsOisUnderscore = rule317  ()
         _lhsOpp' :: PP_Doc
         _lhsOpp' = rule318 _patsIpps' arg_name_
         _lhsOstrictVars :: [PP_Doc]
         _lhsOstrictVars = rule319 _patsIstrictVars
         _copy = rule320 _patsIcopy arg_name_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule321 _copy
         _patsObelowIrrefutable = rule322 _lhsIbelowIrrefutable
         _patsOisDeclOfLet = rule323 _lhsIisDeclOfLet
         _patsOoptions = rule324 _lhsIoptions
         !__result_ = T_Pattern_vOut40 _lhsOcopy _lhsOisUnderscore _lhsOpp _lhsOpp' _lhsOstrictVars
         in __result_ )
     in C_Pattern_s41 v40
   {-# INLINE rule315 #-}
   {-# LINE 374 "./src-ag/PrintCleanCode.ag" #-}
   rule315 = \ ((_lhsIbelowIrrefutable) :: Bool) ((_lhsIisDeclOfLet) :: Bool) ((_lhsIoptions) :: Options) ->
                      {-# LINE 374 "./src-ag/PrintCleanCode.ag" #-}
                      if bangpats _lhsIoptions && not _lhsIisDeclOfLet && not _lhsIbelowIrrefutable
                      then \p -> "!" >|< p
                      else id
                      {-# LINE 2997 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule316 #-}
   {-# LINE 379 "./src-ag/PrintCleanCode.ag" #-}
   rule316 = \ _addBang ((_patsIpps) :: [PP_Doc]) name_ ->
                           {-# LINE 379 "./src-ag/PrintCleanCode.ag" #-}
                           _addBang     $ pp_parens $ name_ >#< hv_sp _patsIpps
                           {-# LINE 3003 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule317 #-}
   {-# LINE 390 "./src-ag/PrintCleanCode.ag" #-}
   rule317 = \  (_ :: ()) ->
                                    {-# LINE 390 "./src-ag/PrintCleanCode.ag" #-}
                                    False
                                    {-# LINE 3009 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule318 #-}
   {-# LINE 413 "./src-ag/PrintCleanCode.ag" #-}
   rule318 = \ ((_patsIpps') :: [PP_Doc]) name_ ->
                            {-# LINE 413 "./src-ag/PrintCleanCode.ag" #-}
                            pp_parens $ name_ >#< hv_sp (map pp_parens _patsIpps')
                            {-# LINE 3015 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule319 #-}
   rule319 = \ ((_patsIstrictVars) :: [PP_Doc]) ->
     _patsIstrictVars
   {-# INLINE rule320 #-}
   rule320 = \ ((_patsIcopy) :: Patterns) name_ ->
     Constr name_ _patsIcopy
   {-# INLINE rule321 #-}
   rule321 = \ _copy ->
     _copy
   {-# INLINE rule322 #-}
   rule322 = \ ((_lhsIbelowIrrefutable) :: Bool) ->
     _lhsIbelowIrrefutable
   {-# INLINE rule323 #-}
   rule323 = \ ((_lhsIisDeclOfLet) :: Bool) ->
     _lhsIisDeclOfLet
   {-# INLINE rule324 #-}
   rule324 = \ ((_lhsIoptions) :: Options) ->
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
         _addBang = rule325 _lhsIbelowIrrefutable _lhsIisDeclOfLet _lhsIoptions
         _lhsOpp :: PP_Doc
         _lhsOpp = rule326 _addBang _patsIpps
         _lhsOisUnderscore :: Bool
         _lhsOisUnderscore = rule327  ()
         _lhsOpp' :: PP_Doc
         _lhsOpp' = rule328 _patsIpps'
         _lhsOstrictVars :: [PP_Doc]
         _lhsOstrictVars = rule329 _patsIstrictVars
         _copy = rule330 _patsIcopy arg_pos_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule331 _copy
         _patsObelowIrrefutable = rule332 _lhsIbelowIrrefutable
         _patsOisDeclOfLet = rule333 _lhsIisDeclOfLet
         _patsOoptions = rule334 _lhsIoptions
         !__result_ = T_Pattern_vOut40 _lhsOcopy _lhsOisUnderscore _lhsOpp _lhsOpp' _lhsOstrictVars
         in __result_ )
     in C_Pattern_s41 v40
   {-# INLINE rule325 #-}
   {-# LINE 374 "./src-ag/PrintCleanCode.ag" #-}
   rule325 = \ ((_lhsIbelowIrrefutable) :: Bool) ((_lhsIisDeclOfLet) :: Bool) ((_lhsIoptions) :: Options) ->
                      {-# LINE 374 "./src-ag/PrintCleanCode.ag" #-}
                      if bangpats _lhsIoptions && not _lhsIisDeclOfLet && not _lhsIbelowIrrefutable
                      then \p -> "!" >|< p
                      else id
                      {-# LINE 3068 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule326 #-}
   {-# LINE 380 "./src-ag/PrintCleanCode.ag" #-}
   rule326 = \ _addBang ((_patsIpps) :: [PP_Doc]) ->
                           {-# LINE 380 "./src-ag/PrintCleanCode.ag" #-}
                           _addBang     $ pp_block "(" ")" "," _patsIpps
                           {-# LINE 3074 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule327 #-}
   {-# LINE 391 "./src-ag/PrintCleanCode.ag" #-}
   rule327 = \  (_ :: ()) ->
                                    {-# LINE 391 "./src-ag/PrintCleanCode.ag" #-}
                                    False
                                    {-# LINE 3080 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule328 #-}
   {-# LINE 414 "./src-ag/PrintCleanCode.ag" #-}
   rule328 = \ ((_patsIpps') :: [PP_Doc]) ->
                            {-# LINE 414 "./src-ag/PrintCleanCode.ag" #-}
                            pp_block "(" ")" "," _patsIpps'
                            {-# LINE 3086 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule329 #-}
   rule329 = \ ((_patsIstrictVars) :: [PP_Doc]) ->
     _patsIstrictVars
   {-# INLINE rule330 #-}
   rule330 = \ ((_patsIcopy) :: Patterns) pos_ ->
     Product pos_ _patsIcopy
   {-# INLINE rule331 #-}
   rule331 = \ _copy ->
     _copy
   {-# INLINE rule332 #-}
   rule332 = \ ((_lhsIbelowIrrefutable) :: Bool) ->
     _lhsIbelowIrrefutable
   {-# INLINE rule333 #-}
   rule333 = \ ((_lhsIisDeclOfLet) :: Bool) ->
     _lhsIisDeclOfLet
   {-# INLINE rule334 #-}
   rule334 = \ ((_lhsIoptions) :: Options) ->
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
         _strictVar = rule335 _lhsIisDeclOfLet _lhsIoptions _ppVar
         _strictPatVars = rule336 _lhsIisDeclOfLet _lhsIoptions _patIstrictVars
         _lhsOstrictVars :: [PP_Doc]
         _lhsOstrictVars = rule337 _strictPatVars _strictVar
         _addBang = rule338 _lhsIbelowIrrefutable _lhsIisDeclOfLet _lhsIoptions
         _ppVar = rule339 _lhsIoptions arg_attr_ arg_field_
         _ppVarBang = rule340 _addBang _ppVar
         _lhsOpp :: PP_Doc
         _lhsOpp = rule341 _patIisUnderscore _patIpp _ppVarBang
         _lhsOisUnderscore :: Bool
         _lhsOisUnderscore = rule342  ()
         _lhsOpp' :: PP_Doc
         _lhsOpp' = rule343 _lhsIoptions _patIpp' arg_attr_ arg_field_
         _copy = rule344 _patIcopy arg_attr_ arg_field_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule345 _copy
         _patObelowIrrefutable = rule346 _lhsIbelowIrrefutable
         _patOisDeclOfLet = rule347 _lhsIisDeclOfLet
         _patOoptions = rule348 _lhsIoptions
         !__result_ = T_Pattern_vOut40 _lhsOcopy _lhsOisUnderscore _lhsOpp _lhsOpp' _lhsOstrictVars
         in __result_ )
     in C_Pattern_s41 v40
   {-# INLINE rule335 #-}
   {-# LINE 352 "./src-ag/PrintCleanCode.ag" #-}
   rule335 = \ ((_lhsIisDeclOfLet) :: Bool) ((_lhsIoptions) :: Options) _ppVar ->
            {-# LINE 352 "./src-ag/PrintCleanCode.ag" #-}
            if strictCases _lhsIoptions && not _lhsIisDeclOfLet
            then [_ppVar    ]
            else []
            {-# LINE 3143 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule336 #-}
   {-# LINE 356 "./src-ag/PrintCleanCode.ag" #-}
   rule336 = \ ((_lhsIisDeclOfLet) :: Bool) ((_lhsIoptions) :: Options) ((_patIstrictVars) :: [PP_Doc]) ->
            {-# LINE 356 "./src-ag/PrintCleanCode.ag" #-}
            if stricterCases _lhsIoptions && not _lhsIisDeclOfLet
            then _patIstrictVars
            else []
            {-# LINE 3151 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule337 #-}
   {-# LINE 360 "./src-ag/PrintCleanCode.ag" #-}
   rule337 = \ _strictPatVars _strictVar ->
            {-# LINE 360 "./src-ag/PrintCleanCode.ag" #-}
            _strictVar     ++ _strictPatVars
            {-# LINE 3157 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule338 #-}
   {-# LINE 374 "./src-ag/PrintCleanCode.ag" #-}
   rule338 = \ ((_lhsIbelowIrrefutable) :: Bool) ((_lhsIisDeclOfLet) :: Bool) ((_lhsIoptions) :: Options) ->
                      {-# LINE 374 "./src-ag/PrintCleanCode.ag" #-}
                      if bangpats _lhsIoptions && not _lhsIisDeclOfLet && not _lhsIbelowIrrefutable
                      then \p -> "!" >|< p
                      else id
                      {-# LINE 3165 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule339 #-}
   {-# LINE 381 "./src-ag/PrintCleanCode.ag" #-}
   rule339 = \ ((_lhsIoptions) :: Options) attr_ field_ ->
                           {-# LINE 381 "./src-ag/PrintCleanCode.ag" #-}
                           pp (attrname _lhsIoptions False field_ attr_)
                           {-# LINE 3171 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule340 #-}
   {-# LINE 382 "./src-ag/PrintCleanCode.ag" #-}
   rule340 = \ _addBang _ppVar ->
                              {-# LINE 382 "./src-ag/PrintCleanCode.ag" #-}
                              _addBang     $ _ppVar
                              {-# LINE 3177 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule341 #-}
   {-# LINE 383 "./src-ag/PrintCleanCode.ag" #-}
   rule341 = \ ((_patIisUnderscore) :: Bool) ((_patIpp) :: PP_Doc) _ppVarBang ->
                           {-# LINE 383 "./src-ag/PrintCleanCode.ag" #-}
                           if _patIisUnderscore
                            then _ppVarBang
                            else _ppVarBang     >|< "@" >|< _patIpp
                           {-# LINE 3185 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule342 #-}
   {-# LINE 392 "./src-ag/PrintCleanCode.ag" #-}
   rule342 = \  (_ :: ()) ->
                                    {-# LINE 392 "./src-ag/PrintCleanCode.ag" #-}
                                    False
                                    {-# LINE 3191 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule343 #-}
   {-# LINE 415 "./src-ag/PrintCleanCode.ag" #-}
   rule343 = \ ((_lhsIoptions) :: Options) ((_patIpp') :: PP_Doc) attr_ field_ ->
                            {-# LINE 415 "./src-ag/PrintCleanCode.ag" #-}
                            let attribute | field_ == _LOC || field_ == nullIdent = locname' attr_
                                          | otherwise                             = attrname _lhsIoptions False field_ attr_
                            in attribute >|< "@" >|< _patIpp'
                            {-# LINE 3199 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule344 #-}
   rule344 = \ ((_patIcopy) :: Pattern) attr_ field_ ->
     Alias field_ attr_ _patIcopy
   {-# INLINE rule345 #-}
   rule345 = \ _copy ->
     _copy
   {-# INLINE rule346 #-}
   rule346 = \ ((_lhsIbelowIrrefutable) :: Bool) ->
     _lhsIbelowIrrefutable
   {-# INLINE rule347 #-}
   rule347 = \ ((_lhsIisDeclOfLet) :: Bool) ->
     _lhsIisDeclOfLet
   {-# INLINE rule348 #-}
   rule348 = \ ((_lhsIoptions) :: Options) ->
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
         _lhsOstrictVars = rule349  ()
         _lhsOpp :: PP_Doc
         _lhsOpp = rule350 _patIpp
         _patObelowIrrefutable = rule351  ()
         _lhsOpp' :: PP_Doc
         _lhsOpp' = rule352 _patIpp
         _copy = rule353 _patIcopy
         _lhsOcopy :: Pattern
         _lhsOcopy = rule354 _copy
         _lhsOisUnderscore :: Bool
         _lhsOisUnderscore = rule355 _patIisUnderscore
         _patOisDeclOfLet = rule356 _lhsIisDeclOfLet
         _patOoptions = rule357 _lhsIoptions
         !__result_ = T_Pattern_vOut40 _lhsOcopy _lhsOisUnderscore _lhsOpp _lhsOpp' _lhsOstrictVars
         in __result_ )
     in C_Pattern_s41 v40
   {-# INLINE rule349 #-}
   {-# LINE 362 "./src-ag/PrintCleanCode.ag" #-}
   rule349 = \  (_ :: ()) ->
                         {-# LINE 362 "./src-ag/PrintCleanCode.ag" #-}
                         []
                         {-# LINE 3246 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule350 #-}
   {-# LINE 386 "./src-ag/PrintCleanCode.ag" #-}
   rule350 = \ ((_patIpp) :: PP_Doc) ->
                           {-# LINE 386 "./src-ag/PrintCleanCode.ag" #-}
                           text "~" >|< pp_parens _patIpp
                           {-# LINE 3252 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule351 #-}
   {-# LINE 398 "./src-ag/PrintCleanCode.ag" #-}
   rule351 = \  (_ :: ()) ->
                               {-# LINE 398 "./src-ag/PrintCleanCode.ag" #-}
                               True
                               {-# LINE 3258 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule352 #-}
   {-# LINE 418 "./src-ag/PrintCleanCode.ag" #-}
   rule352 = \ ((_patIpp) :: PP_Doc) ->
                            {-# LINE 418 "./src-ag/PrintCleanCode.ag" #-}
                            text "~" >|< pp_parens _patIpp
                            {-# LINE 3264 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule353 #-}
   rule353 = \ ((_patIcopy) :: Pattern) ->
     Irrefutable _patIcopy
   {-# INLINE rule354 #-}
   rule354 = \ _copy ->
     _copy
   {-# INLINE rule355 #-}
   rule355 = \ ((_patIisUnderscore) :: Bool) ->
     _patIisUnderscore
   {-# INLINE rule356 #-}
   rule356 = \ ((_lhsIisDeclOfLet) :: Bool) ->
     _lhsIisDeclOfLet
   {-# INLINE rule357 #-}
   rule357 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Pattern_Underscore #-}
sem_Pattern_Underscore :: (Pos) -> T_Pattern 
sem_Pattern_Underscore !arg_pos_ = T_Pattern (return st41) where
   {-# NOINLINE st41 #-}
   !st41 = let
      v40 :: T_Pattern_v40 
      v40 = \ !(T_Pattern_vIn40 _lhsIbelowIrrefutable _lhsIisDeclOfLet _lhsIoptions) -> ( let
         _lhsOpp :: PP_Doc
         _lhsOpp = rule358  ()
         _lhsOisUnderscore :: Bool
         _lhsOisUnderscore = rule359  ()
         _lhsOpp' :: PP_Doc
         _lhsOpp' = rule360  ()
         _lhsOstrictVars :: [PP_Doc]
         _lhsOstrictVars = rule361  ()
         _copy = rule362 arg_pos_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule363 _copy
         !__result_ = T_Pattern_vOut40 _lhsOcopy _lhsOisUnderscore _lhsOpp _lhsOpp' _lhsOstrictVars
         in __result_ )
     in C_Pattern_s41 v40
   {-# INLINE rule358 #-}
   {-# LINE 387 "./src-ag/PrintCleanCode.ag" #-}
   rule358 = \  (_ :: ()) ->
                           {-# LINE 387 "./src-ag/PrintCleanCode.ag" #-}
                           text "_"
                           {-# LINE 3306 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule359 #-}
   {-# LINE 393 "./src-ag/PrintCleanCode.ag" #-}
   rule359 = \  (_ :: ()) ->
                                    {-# LINE 393 "./src-ag/PrintCleanCode.ag" #-}
                                    True
                                    {-# LINE 3312 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule360 #-}
   {-# LINE 419 "./src-ag/PrintCleanCode.ag" #-}
   rule360 = \  (_ :: ()) ->
                            {-# LINE 419 "./src-ag/PrintCleanCode.ag" #-}
                            text "_"
                            {-# LINE 3318 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule361 #-}
   rule361 = \  (_ :: ()) ->
     []
   {-# INLINE rule362 #-}
   rule362 = \ pos_ ->
     Underscore pos_
   {-# INLINE rule363 #-}
   rule363 = \ _copy ->
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
         _lhsOpps = rule364 _hdIpp _tlIpps
         _lhsOpps' :: [PP_Doc]
         _lhsOpps' = rule365 _hdIpp' _tlIpps'
         _lhsOstrictVars :: [PP_Doc]
         _lhsOstrictVars = rule366 _hdIstrictVars _tlIstrictVars
         _copy = rule367 _hdIcopy _tlIcopy
         _lhsOcopy :: Patterns
         _lhsOcopy = rule368 _copy
         _hdObelowIrrefutable = rule369 _lhsIbelowIrrefutable
         _hdOisDeclOfLet = rule370 _lhsIisDeclOfLet
         _hdOoptions = rule371 _lhsIoptions
         _tlObelowIrrefutable = rule372 _lhsIbelowIrrefutable
         _tlOisDeclOfLet = rule373 _lhsIisDeclOfLet
         _tlOoptions = rule374 _lhsIoptions
         !__result_ = T_Patterns_vOut43 _lhsOcopy _lhsOpps _lhsOpps' _lhsOstrictVars
         in __result_ )
     in C_Patterns_s44 v43
   {-# INLINE rule364 #-}
   {-# LINE 369 "./src-ag/PrintCleanCode.ag" #-}
   rule364 = \ ((_hdIpp) :: PP_Doc) ((_tlIpps) :: [PP_Doc]) ->
                     {-# LINE 369 "./src-ag/PrintCleanCode.ag" #-}
                     _hdIpp : _tlIpps
                     {-# LINE 3393 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule365 #-}
   {-# LINE 409 "./src-ag/PrintCleanCode.ag" #-}
   rule365 = \ ((_hdIpp') :: PP_Doc) ((_tlIpps') :: [PP_Doc]) ->
                      {-# LINE 409 "./src-ag/PrintCleanCode.ag" #-}
                      _hdIpp' : _tlIpps'
                      {-# LINE 3399 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule366 #-}
   rule366 = \ ((_hdIstrictVars) :: [PP_Doc]) ((_tlIstrictVars) :: [PP_Doc]) ->
     _hdIstrictVars ++ _tlIstrictVars
   {-# INLINE rule367 #-}
   rule367 = \ ((_hdIcopy) :: Pattern) ((_tlIcopy) :: Patterns) ->
     (:) _hdIcopy _tlIcopy
   {-# INLINE rule368 #-}
   rule368 = \ _copy ->
     _copy
   {-# INLINE rule369 #-}
   rule369 = \ ((_lhsIbelowIrrefutable) :: Bool) ->
     _lhsIbelowIrrefutable
   {-# INLINE rule370 #-}
   rule370 = \ ((_lhsIisDeclOfLet) :: Bool) ->
     _lhsIisDeclOfLet
   {-# INLINE rule371 #-}
   rule371 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule372 #-}
   rule372 = \ ((_lhsIbelowIrrefutable) :: Bool) ->
     _lhsIbelowIrrefutable
   {-# INLINE rule373 #-}
   rule373 = \ ((_lhsIisDeclOfLet) :: Bool) ->
     _lhsIisDeclOfLet
   {-# INLINE rule374 #-}
   rule374 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Patterns_Nil #-}
sem_Patterns_Nil ::  T_Patterns 
sem_Patterns_Nil  = T_Patterns (return st44) where
   {-# NOINLINE st44 #-}
   !st44 = let
      v43 :: T_Patterns_v43 
      v43 = \ !(T_Patterns_vIn43 _lhsIbelowIrrefutable _lhsIisDeclOfLet _lhsIoptions) -> ( let
         _lhsOpps :: [PP_Doc]
         _lhsOpps = rule375  ()
         _lhsOpps' :: [PP_Doc]
         _lhsOpps' = rule376  ()
         _lhsOstrictVars :: [PP_Doc]
         _lhsOstrictVars = rule377  ()
         _copy = rule378  ()
         _lhsOcopy :: Patterns
         _lhsOcopy = rule379 _copy
         !__result_ = T_Patterns_vOut43 _lhsOcopy _lhsOpps _lhsOpps' _lhsOstrictVars
         in __result_ )
     in C_Patterns_s44 v43
   {-# INLINE rule375 #-}
   {-# LINE 370 "./src-ag/PrintCleanCode.ag" #-}
   rule375 = \  (_ :: ()) ->
                     {-# LINE 370 "./src-ag/PrintCleanCode.ag" #-}
                     []
                     {-# LINE 3451 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule376 #-}
   {-# LINE 410 "./src-ag/PrintCleanCode.ag" #-}
   rule376 = \  (_ :: ()) ->
                      {-# LINE 410 "./src-ag/PrintCleanCode.ag" #-}
                      []
                      {-# LINE 3457 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule377 #-}
   rule377 = \  (_ :: ()) ->
     []
   {-# INLINE rule378 #-}
   rule378 = \  (_ :: ()) ->
     []
   {-# INLINE rule379 #-}
   rule379 = \ _copy ->
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
         _options = rule380 _lhsIoptions arg_ordered_
         _chunksOnested = rule381 _lhsIoptions
         _lhsOoutput :: PP_Docs
         _lhsOoutput = rule382 _chunksIpps
         _chunksOisDeclOfLet = rule383  ()
         _mainModuleFile = rule384 _lhsImainFile
         _genMainModule = rule385 _chunksIappendMain _chunksIimports _lhsImainBlocksDoc _lhsImainName _lhsImoduleHeader _lhsIoptionsLine _lhsIpragmaBlocks _mainModuleFile
         _commonFile = rule386 _lhsImainFile
         _genCommonModule = rule387 _chunksIappendCommon _commonFile _lhsIimportBlocks _lhsImainName _lhsImoduleHeader _lhsIoptionsLine _lhsIpragmaBlocks _lhsItextBlocks
         _lhsOgenIO :: IO ()
         _lhsOgenIO = rule388 _chunksIgenSems _genCommonModule _genMainModule
         _chunksOimportBlocks = rule389 _lhsIimportBlocks
         _chunksOmainFile = rule390 _lhsImainFile
         _chunksOmainName = rule391 _lhsImainName
         _chunksOmoduleHeader = rule392 _lhsImoduleHeader
         _chunksOoptions = rule393 _options
         _chunksOoptionsLine = rule394 _lhsIoptionsLine
         _chunksOpragmaBlocks = rule395 _lhsIpragmaBlocks
         _chunksOtextBlockMap = rule396 _lhsItextBlockMap
         _chunksOtextBlocks = rule397 _lhsItextBlocks
         !__result_ = T_Program_vOut46 _lhsOgenIO _lhsOoutput
         in __result_ )
     in C_Program_s47 v46
   {-# INLINE rule380 #-}
   {-# LINE 62 "./src-ag/PrintCleanCode.ag" #-}
   rule380 = \ ((_lhsIoptions) :: Options) ordered_ ->
                  {-# LINE 62 "./src-ag/PrintCleanCode.ag" #-}
                  _lhsIoptions { breadthFirst = breadthFirst _lhsIoptions && visit _lhsIoptions && cases _lhsIoptions && ordered_ }
                  {-# LINE 3535 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule381 #-}
   {-# LINE 65 "./src-ag/PrintCleanCode.ag" #-}
   rule381 = \ ((_lhsIoptions) :: Options) ->
                              {-# LINE 65 "./src-ag/PrintCleanCode.ag" #-}
                              nest _lhsIoptions
                              {-# LINE 3541 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule382 #-}
   {-# LINE 101 "./src-ag/PrintCleanCode.ag" #-}
   rule382 = \ ((_chunksIpps) :: PP_Docs) ->
                               {-# LINE 101 "./src-ag/PrintCleanCode.ag" #-}
                               _chunksIpps
                               {-# LINE 3547 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule383 #-}
   {-# LINE 433 "./src-ag/PrintCleanCode.ag" #-}
   rule383 = \  (_ :: ()) ->
                             {-# LINE 433 "./src-ag/PrintCleanCode.ag" #-}
                             False
                             {-# LINE 3553 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule384 #-}
   {-# LINE 467 "./src-ag/PrintCleanCode.ag" #-}
   rule384 = \ ((_lhsImainFile) :: String) ->
                             {-# LINE 467 "./src-ag/PrintCleanCode.ag" #-}
                             _lhsImainFile
                             {-# LINE 3559 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule385 #-}
   {-# LINE 469 "./src-ag/PrintCleanCode.ag" #-}
   rule385 = \ ((_chunksIappendMain) :: [[PP_Doc]]) ((_chunksIimports) :: [String]) ((_lhsImainBlocksDoc) :: PP_Doc) ((_lhsImainName) :: String) ((_lhsImoduleHeader) :: String -> String -> String -> Bool -> String) ((_lhsIoptionsLine) :: String) ((_lhsIpragmaBlocks) :: String) _mainModuleFile ->
            {-# LINE 469 "./src-ag/PrintCleanCode.ag" #-}
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
            {-# LINE 3574 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule386 #-}
   {-# LINE 480 "./src-ag/PrintCleanCode.ag" #-}
   rule386 = \ ((_lhsImainFile) :: String) ->
                         {-# LINE 480 "./src-ag/PrintCleanCode.ag" #-}
                         replaceBaseName _lhsImainFile (takeBaseName _lhsImainFile ++ "_common")
                         {-# LINE 3580 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule387 #-}
   {-# LINE 482 "./src-ag/PrintCleanCode.ag" #-}
   rule387 = \ ((_chunksIappendCommon) :: [[PP_Doc]]) _commonFile ((_lhsIimportBlocks) :: PP_Doc) ((_lhsImainName) :: String) ((_lhsImoduleHeader) :: String -> String -> String -> Bool -> String) ((_lhsIoptionsLine) :: String) ((_lhsIpragmaBlocks) :: String) ((_lhsItextBlocks) :: PP_Doc) ->
            {-# LINE 482 "./src-ag/PrintCleanCode.ag" #-}
            writeModule _commonFile
                ( [ pp $ _lhsIpragmaBlocks
                  , pp $ _lhsIoptionsLine
                  , pp $ _lhsImoduleHeader _lhsImainName "_common" "" True
                  , _lhsIimportBlocks
                  , _lhsItextBlocks
                  ]
                  ++ map vlist _chunksIappendCommon
                )
            {-# LINE 3594 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule388 #-}
   {-# LINE 492 "./src-ag/PrintCleanCode.ag" #-}
   rule388 = \ ((_chunksIgenSems) :: IO ()) _genCommonModule _genMainModule ->
                    {-# LINE 492 "./src-ag/PrintCleanCode.ag" #-}
                    do _genMainModule
                       _genCommonModule
                       _chunksIgenSems
                    {-# LINE 3602 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule389 #-}
   rule389 = \ ((_lhsIimportBlocks) :: PP_Doc) ->
     _lhsIimportBlocks
   {-# INLINE rule390 #-}
   rule390 = \ ((_lhsImainFile) :: String) ->
     _lhsImainFile
   {-# INLINE rule391 #-}
   rule391 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule392 #-}
   rule392 = \ ((_lhsImoduleHeader) :: String -> String -> String -> Bool -> String) ->
     _lhsImoduleHeader
   {-# INLINE rule393 #-}
   rule393 = \ _options ->
     _options
   {-# INLINE rule394 #-}
   rule394 = \ ((_lhsIoptionsLine) :: String) ->
     _lhsIoptionsLine
   {-# INLINE rule395 #-}
   rule395 = \ ((_lhsIpragmaBlocks) :: String) ->
     _lhsIpragmaBlocks
   {-# INLINE rule396 #-}
   rule396 = \ ((_lhsItextBlockMap) :: Map BlockInfo PP_Doc) ->
     _lhsItextBlockMap
   {-# INLINE rule397 #-}
   rule397 = \ ((_lhsItextBlocks) :: PP_Doc) ->
     _lhsItextBlocks

-- Type --------------------------------------------------------
-- wrapper
data Inh_Type  = Inh_Type { nested_Inh_Type :: !(Bool) }
data Syn_Type  = Syn_Type { copy_Syn_Type :: !(Type), pp_Syn_Type :: !(PP_Doc), prec_Syn_Type :: !(Int) }
{-# INLINABLE wrap_Type #-}
wrap_Type :: T_Type  -> Inh_Type  -> (Syn_Type )
wrap_Type !(T_Type act) !(Inh_Type _lhsInested) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_Type_vIn49 _lhsInested
        !(T_Type_vOut49 _lhsOcopy _lhsOpp _lhsOprec) <- return (inv_Type_s50 sem arg)
        return (Syn_Type _lhsOcopy _lhsOpp _lhsOprec)
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
data T_Type_vOut49  = T_Type_vOut49 (Type) (PP_Doc) (Int)
{-# NOINLINE sem_Type_Arr #-}
sem_Type_Arr :: T_Type  -> T_Type  -> T_Type 
sem_Type_Arr arg_left_ arg_right_ = T_Type (return st50) where
   {-# NOINLINE st50 #-}
   !st50 = let
      v49 :: T_Type_v49 
      v49 = \ !(T_Type_vIn49 _lhsInested) -> ( let
         _leftX50 = Control.Monad.Identity.runIdentity (attach_T_Type (arg_left_))
         _rightX50 = Control.Monad.Identity.runIdentity (attach_T_Type (arg_right_))
         (T_Type_vOut49 _leftIcopy _leftIpp _leftIprec) = inv_Type_s50 _leftX50 (T_Type_vIn49 _leftOnested)
         (T_Type_vOut49 _rightIcopy _rightIpp _rightIprec) = inv_Type_s50 _rightX50 (T_Type_vIn49 _rightOnested)
         _lhsOprec :: Int
         _lhsOprec = rule398  ()
         _lhsOpp :: PP_Doc
         _lhsOpp = rule399 _l _r _rightIcopy
         _l = rule400 _leftIpp _leftIprec
         _r = rule401 _rightIpp _rightIprec
         _copy = rule402 _leftIcopy _rightIcopy
         _lhsOcopy :: Type
         _lhsOcopy = rule403 _copy
         _leftOnested = rule404 _lhsInested
         _rightOnested = rule405 _lhsInested
         !__result_ = T_Type_vOut49 _lhsOcopy _lhsOpp _lhsOprec
         in __result_ )
     in C_Type_s50 v49
   {-# INLINE rule398 #-}
   {-# LINE 276 "./src-ag/PrintCleanCode.ag" #-}
   rule398 = \  (_ :: ()) ->
                               {-# LINE 276 "./src-ag/PrintCleanCode.ag" #-}
                               2
                               {-# LINE 3703 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule399 #-}
   {-# LINE 277 "./src-ag/PrintCleanCode.ag" #-}
   rule399 = \ _l _r ((_rightIcopy) :: Type) ->
                               {-# LINE 277 "./src-ag/PrintCleanCode.ag" #-}
                               case _rightIcopy of
                                 Arr{} -> _l     >-< _r
                                 _     -> _l     >#< "->" >-< _r
                               {-# LINE 3711 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule400 #-}
   {-# LINE 280 "./src-ag/PrintCleanCode.ag" #-}
   rule400 = \ ((_leftIpp) :: PP_Doc) ((_leftIprec) :: Int) ->
                               {-# LINE 280 "./src-ag/PrintCleanCode.ag" #-}
                               if _leftIprec  <= 2 then pp_parens _leftIpp  else _leftIpp
                               {-# LINE 3717 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule401 #-}
   {-# LINE 281 "./src-ag/PrintCleanCode.ag" #-}
   rule401 = \ ((_rightIpp) :: PP_Doc) ((_rightIprec) :: Int) ->
                               {-# LINE 281 "./src-ag/PrintCleanCode.ag" #-}
                               if _rightIprec <  2 then pp_parens _rightIpp else _rightIpp
                               {-# LINE 3723 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule402 #-}
   rule402 = \ ((_leftIcopy) :: Type) ((_rightIcopy) :: Type) ->
     Arr _leftIcopy _rightIcopy
   {-# INLINE rule403 #-}
   rule403 = \ _copy ->
     _copy
   {-# INLINE rule404 #-}
   rule404 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule405 #-}
   rule405 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
{-# NOINLINE sem_Type_CtxApp #-}
sem_Type_CtxApp :: ([(String, [String])]) -> T_Type  -> T_Type 
sem_Type_CtxApp !arg_left_ arg_right_ = T_Type (return st50) where
   {-# NOINLINE st50 #-}
   !st50 = let
      v49 :: T_Type_v49 
      v49 = \ !(T_Type_vIn49 _lhsInested) -> ( let
         _rightX50 = Control.Monad.Identity.runIdentity (attach_T_Type (arg_right_))
         (T_Type_vOut49 _rightIcopy _rightIpp _rightIprec) = inv_Type_s50 _rightX50 (T_Type_vIn49 _rightOnested)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule406 _rightIpp arg_left_
         _copy = rule407 _rightIcopy arg_left_
         _lhsOcopy :: Type
         _lhsOcopy = rule408 _copy
         _lhsOprec :: Int
         _lhsOprec = rule409 _rightIprec
         _rightOnested = rule410 _lhsInested
         !__result_ = T_Type_vOut49 _lhsOcopy _lhsOpp _lhsOprec
         in __result_ )
     in C_Type_s50 v49
   {-# INLINE rule406 #-}
   {-# LINE 287 "./src-ag/PrintCleanCode.ag" #-}
   rule406 = \ ((_rightIpp) :: PP_Doc) left_ ->
                 {-# LINE 287 "./src-ag/PrintCleanCode.ag" #-}
                 _rightIpp >#< " | " >#< (pp_block "" "" "&" $ map (\(n,ns) -> hv_sp $ map pp (n:ns)) left_)
                 {-# LINE 3761 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule407 #-}
   rule407 = \ ((_rightIcopy) :: Type) left_ ->
     CtxApp left_ _rightIcopy
   {-# INLINE rule408 #-}
   rule408 = \ _copy ->
     _copy
   {-# INLINE rule409 #-}
   rule409 = \ ((_rightIprec) :: Int) ->
     _rightIprec
   {-# INLINE rule410 #-}
   rule410 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
{-# NOINLINE sem_Type_QuantApp #-}
sem_Type_QuantApp :: (String) -> T_Type  -> T_Type 
sem_Type_QuantApp !arg_left_ arg_right_ = T_Type (return st50) where
   {-# NOINLINE st50 #-}
   !st50 = let
      v49 :: T_Type_v49 
      v49 = \ !(T_Type_vIn49 _lhsInested) -> ( let
         _rightX50 = Control.Monad.Identity.runIdentity (attach_T_Type (arg_right_))
         (T_Type_vOut49 _rightIcopy _rightIpp _rightIprec) = inv_Type_s50 _rightX50 (T_Type_vIn49 _rightOnested)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule411 _rightIpp arg_left_
         _copy = rule412 _rightIcopy arg_left_
         _lhsOcopy :: Type
         _lhsOcopy = rule413 _copy
         _lhsOprec :: Int
         _lhsOprec = rule414 _rightIprec
         _rightOnested = rule415 _lhsInested
         !__result_ = T_Type_vOut49 _lhsOcopy _lhsOpp _lhsOprec
         in __result_ )
     in C_Type_s50 v49
   {-# INLINE rule411 #-}
   {-# LINE 289 "./src-ag/PrintCleanCode.ag" #-}
   rule411 = \ ((_rightIpp) :: PP_Doc) left_ ->
                 {-# LINE 289 "./src-ag/PrintCleanCode.ag" #-}
                 left_ >#< _rightIpp
                 {-# LINE 3799 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule412 #-}
   rule412 = \ ((_rightIcopy) :: Type) left_ ->
     QuantApp left_ _rightIcopy
   {-# INLINE rule413 #-}
   rule413 = \ _copy ->
     _copy
   {-# INLINE rule414 #-}
   rule414 = \ ((_rightIprec) :: Int) ->
     _rightIprec
   {-# INLINE rule415 #-}
   rule415 = \ ((_lhsInested) :: Bool) ->
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
         (T_Type_vOut49 _funcIcopy _funcIpp _funcIprec) = inv_Type_s50 _funcX50 (T_Type_vIn49 _funcOnested)
         (T_Types_vOut52 _argsIcopy _argsIpps) = inv_Types_s53 _argsX53 (T_Types_vIn52 _argsOnested)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule416 _argsIpps _funcIpp
         _copy = rule417 _argsIcopy _funcIcopy
         _lhsOcopy :: Type
         _lhsOcopy = rule418 _copy
         _lhsOprec :: Int
         _lhsOprec = rule419 _funcIprec
         _funcOnested = rule420 _lhsInested
         _argsOnested = rule421 _lhsInested
         !__result_ = T_Type_vOut49 _lhsOcopy _lhsOpp _lhsOprec
         in __result_ )
     in C_Type_s50 v49
   {-# INLINE rule416 #-}
   {-# LINE 284 "./src-ag/PrintCleanCode.ag" #-}
   rule416 = \ ((_argsIpps) :: PP_Docs) ((_funcIpp) :: PP_Doc) ->
                 {-# LINE 284 "./src-ag/PrintCleanCode.ag" #-}
                 pp "(" >#< hv_sp (_funcIpp : _argsIpps) >#< pp ")"
                 {-# LINE 3840 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule417 #-}
   rule417 = \ ((_argsIcopy) :: Types) ((_funcIcopy) :: Type) ->
     TypeApp _funcIcopy _argsIcopy
   {-# INLINE rule418 #-}
   rule418 = \ _copy ->
     _copy
   {-# INLINE rule419 #-}
   rule419 = \ ((_funcIprec) :: Int) ->
     _funcIprec
   {-# INLINE rule420 #-}
   rule420 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule421 #-}
   rule421 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
{-# NOINLINE sem_Type_TupleType #-}
sem_Type_TupleType :: T_Types  -> T_Type 
sem_Type_TupleType arg_tps_ = T_Type (return st50) where
   {-# NOINLINE st50 #-}
   !st50 = let
      v49 :: T_Type_v49 
      v49 = \ !(T_Type_vIn49 _lhsInested) -> ( let
         _tpsX53 = Control.Monad.Identity.runIdentity (attach_T_Types (arg_tps_))
         (T_Types_vOut52 _tpsIcopy _tpsIpps) = inv_Types_s53 _tpsX53 (T_Types_vIn52 _tpsOnested)
         _lhsOprec :: Int
         _lhsOprec = rule422  ()
         _lhsOpp :: PP_Doc
         _lhsOpp = rule423 _lhsInested _tpsIpps
         _copy = rule424 _tpsIcopy
         _lhsOcopy :: Type
         _lhsOcopy = rule425 _copy
         _tpsOnested = rule426 _lhsInested
         !__result_ = T_Type_vOut49 _lhsOcopy _lhsOpp _lhsOprec
         in __result_ )
     in C_Type_s50 v49
   {-# INLINE rule422 #-}
   {-# LINE 291 "./src-ag/PrintCleanCode.ag" #-}
   rule422 = \  (_ :: ()) ->
                               {-# LINE 291 "./src-ag/PrintCleanCode.ag" #-}
                               5
                               {-# LINE 3881 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule423 #-}
   {-# LINE 292 "./src-ag/PrintCleanCode.ag" #-}
   rule423 = \ ((_lhsInested) :: Bool) ((_tpsIpps) :: PP_Docs) ->
                               {-# LINE 292 "./src-ag/PrintCleanCode.ag" #-}
                               ppTuple _lhsInested _tpsIpps
                               {-# LINE 3887 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule424 #-}
   rule424 = \ ((_tpsIcopy) :: Types) ->
     TupleType _tpsIcopy
   {-# INLINE rule425 #-}
   rule425 = \ _copy ->
     _copy
   {-# INLINE rule426 #-}
   rule426 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
{-# NOINLINE sem_Type_UnboxedTupleType #-}
sem_Type_UnboxedTupleType :: T_Types  -> T_Type 
sem_Type_UnboxedTupleType arg_tps_ = T_Type (return st50) where
   {-# NOINLINE st50 #-}
   !st50 = let
      v49 :: T_Type_v49 
      v49 = \ !(T_Type_vIn49 _lhsInested) -> ( let
         _tpsX53 = Control.Monad.Identity.runIdentity (attach_T_Types (arg_tps_))
         (T_Types_vOut52 _tpsIcopy _tpsIpps) = inv_Types_s53 _tpsX53 (T_Types_vIn52 _tpsOnested)
         _lhsOprec :: Int
         _lhsOprec = rule427  ()
         _lhsOpp :: PP_Doc
         _lhsOpp = rule428 _lhsInested _tpsIpps
         _copy = rule429 _tpsIcopy
         _lhsOcopy :: Type
         _lhsOcopy = rule430 _copy
         _tpsOnested = rule431 _lhsInested
         !__result_ = T_Type_vOut49 _lhsOcopy _lhsOpp _lhsOprec
         in __result_ )
     in C_Type_s50 v49
   {-# INLINE rule427 #-}
   {-# LINE 294 "./src-ag/PrintCleanCode.ag" #-}
   rule427 = \  (_ :: ()) ->
                                      {-# LINE 294 "./src-ag/PrintCleanCode.ag" #-}
                                      5
                                      {-# LINE 3922 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule428 #-}
   {-# LINE 295 "./src-ag/PrintCleanCode.ag" #-}
   rule428 = \ ((_lhsInested) :: Bool) ((_tpsIpps) :: PP_Docs) ->
                                      {-# LINE 295 "./src-ag/PrintCleanCode.ag" #-}
                                      ppUnboxedTuple _lhsInested _tpsIpps
                                      {-# LINE 3928 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule429 #-}
   rule429 = \ ((_tpsIcopy) :: Types) ->
     UnboxedTupleType _tpsIcopy
   {-# INLINE rule430 #-}
   rule430 = \ _copy ->
     _copy
   {-# INLINE rule431 #-}
   rule431 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
{-# NOINLINE sem_Type_List #-}
sem_Type_List :: T_Type  -> T_Type 
sem_Type_List arg_tp_ = T_Type (return st50) where
   {-# NOINLINE st50 #-}
   !st50 = let
      v49 :: T_Type_v49 
      v49 = \ !(T_Type_vIn49 _lhsInested) -> ( let
         _tpX50 = Control.Monad.Identity.runIdentity (attach_T_Type (arg_tp_))
         (T_Type_vOut49 _tpIcopy _tpIpp _tpIprec) = inv_Type_s50 _tpX50 (T_Type_vIn49 _tpOnested)
         _lhsOprec :: Int
         _lhsOprec = rule432  ()
         _lhsOpp :: PP_Doc
         _lhsOpp = rule433 _tpIpp
         _copy = rule434 _tpIcopy
         _lhsOcopy :: Type
         _lhsOcopy = rule435 _copy
         _tpOnested = rule436 _lhsInested
         !__result_ = T_Type_vOut49 _lhsOcopy _lhsOpp _lhsOprec
         in __result_ )
     in C_Type_s50 v49
   {-# INLINE rule432 #-}
   {-# LINE 297 "./src-ag/PrintCleanCode.ag" #-}
   rule432 = \  (_ :: ()) ->
                               {-# LINE 297 "./src-ag/PrintCleanCode.ag" #-}
                               5
                               {-# LINE 3963 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule433 #-}
   {-# LINE 298 "./src-ag/PrintCleanCode.ag" #-}
   rule433 = \ ((_tpIpp) :: PP_Doc) ->
                               {-# LINE 298 "./src-ag/PrintCleanCode.ag" #-}
                               "[" >|< _tpIpp >|< "]"
                               {-# LINE 3969 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule434 #-}
   rule434 = \ ((_tpIcopy) :: Type) ->
     List _tpIcopy
   {-# INLINE rule435 #-}
   rule435 = \ _copy ->
     _copy
   {-# INLINE rule436 #-}
   rule436 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
{-# NOINLINE sem_Type_SimpleType #-}
sem_Type_SimpleType :: (String) -> T_Type 
sem_Type_SimpleType !arg_txt_ = T_Type (return st50) where
   {-# NOINLINE st50 #-}
   !st50 = let
      v49 :: T_Type_v49 
      v49 = \ !(T_Type_vIn49 _lhsInested) -> ( let
         _lhsOprec :: Int
         _lhsOprec = rule437  ()
         _lhsOpp :: PP_Doc
         _lhsOpp = rule438 arg_txt_
         _copy = rule439 arg_txt_
         _lhsOcopy :: Type
         _lhsOcopy = rule440 _copy
         !__result_ = T_Type_vOut49 _lhsOcopy _lhsOpp _lhsOprec
         in __result_ )
     in C_Type_s50 v49
   {-# INLINE rule437 #-}
   {-# LINE 300 "./src-ag/PrintCleanCode.ag" #-}
   rule437 = \  (_ :: ()) ->
                               {-# LINE 300 "./src-ag/PrintCleanCode.ag" #-}
                               5
                               {-# LINE 4001 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule438 #-}
   {-# LINE 301 "./src-ag/PrintCleanCode.ag" #-}
   rule438 = \ txt_ ->
                               {-# LINE 301 "./src-ag/PrintCleanCode.ag" #-}
                               if reallySimple txt_ then text txt_ else pp_parens (text txt_)
                               {-# LINE 4007 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule439 #-}
   rule439 = \ txt_ ->
     SimpleType txt_
   {-# INLINE rule440 #-}
   rule440 = \ _copy ->
     _copy
{-# NOINLINE sem_Type_NontermType #-}
sem_Type_NontermType :: (String) -> ([String]) -> (Bool) -> T_Type 
sem_Type_NontermType !arg_name_ !arg_params_ !arg_deforested_ = T_Type (return st50) where
   {-# NOINLINE st50 #-}
   !st50 = let
      v49 :: T_Type_v49 
      v49 = \ !(T_Type_vIn49 _lhsInested) -> ( let
         _lhsOprec :: Int
         _lhsOprec = rule441  ()
         _lhsOpp :: PP_Doc
         _lhsOpp = rule442 _prefix arg_name_ arg_params_
         _prefix = rule443 arg_deforested_
         _copy = rule444 arg_deforested_ arg_name_ arg_params_
         _lhsOcopy :: Type
         _lhsOcopy = rule445 _copy
         !__result_ = T_Type_vOut49 _lhsOcopy _lhsOpp _lhsOprec
         in __result_ )
     in C_Type_s50 v49
   {-# INLINE rule441 #-}
   {-# LINE 303 "./src-ag/PrintCleanCode.ag" #-}
   rule441 = \  (_ :: ()) ->
                               {-# LINE 303 "./src-ag/PrintCleanCode.ag" #-}
                               5
                               {-# LINE 4037 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule442 #-}
   {-# LINE 304 "./src-ag/PrintCleanCode.ag" #-}
   rule442 = \ _prefix name_ params_ ->
                               {-# LINE 304 "./src-ag/PrintCleanCode.ag" #-}
                               _prefix     >|< text name_ >#< hv_sp params_
                               {-# LINE 4043 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule443 #-}
   {-# LINE 305 "./src-ag/PrintCleanCode.ag" #-}
   rule443 = \ deforested_ ->
                                {-# LINE 305 "./src-ag/PrintCleanCode.ag" #-}
                                if deforested_
                                then text "T_"
                                else empty
                                {-# LINE 4051 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule444 #-}
   rule444 = \ deforested_ name_ params_ ->
     NontermType name_ params_ deforested_
   {-# INLINE rule445 #-}
   rule445 = \ _copy ->
     _copy
{-# NOINLINE sem_Type_TMaybe #-}
sem_Type_TMaybe :: T_Type  -> T_Type 
sem_Type_TMaybe arg_tp_ = T_Type (return st50) where
   {-# NOINLINE st50 #-}
   !st50 = let
      v49 :: T_Type_v49 
      v49 = \ !(T_Type_vIn49 _lhsInested) -> ( let
         _tpX50 = Control.Monad.Identity.runIdentity (attach_T_Type (arg_tp_))
         (T_Type_vOut49 _tpIcopy _tpIpp _tpIprec) = inv_Type_s50 _tpX50 (T_Type_vIn49 _tpOnested)
         _lhsOprec :: Int
         _lhsOprec = rule446  ()
         _lhsOpp :: PP_Doc
         _lhsOpp = rule447 _tpIpp
         _copy = rule448 _tpIcopy
         _lhsOcopy :: Type
         _lhsOcopy = rule449 _copy
         _tpOnested = rule450 _lhsInested
         !__result_ = T_Type_vOut49 _lhsOcopy _lhsOpp _lhsOprec
         in __result_ )
     in C_Type_s50 v49
   {-# INLINE rule446 #-}
   {-# LINE 308 "./src-ag/PrintCleanCode.ag" #-}
   rule446 = \  (_ :: ()) ->
                               {-# LINE 308 "./src-ag/PrintCleanCode.ag" #-}
                               5
                               {-# LINE 4083 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule447 #-}
   {-# LINE 309 "./src-ag/PrintCleanCode.ag" #-}
   rule447 = \ ((_tpIpp) :: PP_Doc) ->
                               {-# LINE 309 "./src-ag/PrintCleanCode.ag" #-}
                               text "Maybe" >#< pp_parens _tpIpp
                               {-# LINE 4089 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule448 #-}
   rule448 = \ ((_tpIcopy) :: Type) ->
     TMaybe _tpIcopy
   {-# INLINE rule449 #-}
   rule449 = \ _copy ->
     _copy
   {-# INLINE rule450 #-}
   rule450 = \ ((_lhsInested) :: Bool) ->
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
         (T_Type_vOut49 _leftIcopy _leftIpp _leftIprec) = inv_Type_s50 _leftX50 (T_Type_vIn49 _leftOnested)
         (T_Type_vOut49 _rightIcopy _rightIpp _rightIprec) = inv_Type_s50 _rightX50 (T_Type_vIn49 _rightOnested)
         _lhsOprec :: Int
         _lhsOprec = rule451  ()
         _lhsOpp :: PP_Doc
         _lhsOpp = rule452 _leftIpp _rightIpp
         _copy = rule453 _leftIcopy _rightIcopy
         _lhsOcopy :: Type
         _lhsOcopy = rule454 _copy
         _leftOnested = rule455 _lhsInested
         _rightOnested = rule456 _lhsInested
         !__result_ = T_Type_vOut49 _lhsOcopy _lhsOpp _lhsOprec
         in __result_ )
     in C_Type_s50 v49
   {-# INLINE rule451 #-}
   {-# LINE 310 "./src-ag/PrintCleanCode.ag" #-}
   rule451 = \  (_ :: ()) ->
                               {-# LINE 310 "./src-ag/PrintCleanCode.ag" #-}
                               5
                               {-# LINE 4127 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule452 #-}
   {-# LINE 311 "./src-ag/PrintCleanCode.ag" #-}
   rule452 = \ ((_leftIpp) :: PP_Doc) ((_rightIpp) :: PP_Doc) ->
                               {-# LINE 311 "./src-ag/PrintCleanCode.ag" #-}
                               text "Either" >#< pp_parens _leftIpp >#< pp_parens _rightIpp
                               {-# LINE 4133 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule453 #-}
   rule453 = \ ((_leftIcopy) :: Type) ((_rightIcopy) :: Type) ->
     TEither _leftIcopy _rightIcopy
   {-# INLINE rule454 #-}
   rule454 = \ _copy ->
     _copy
   {-# INLINE rule455 #-}
   rule455 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule456 #-}
   rule456 = \ ((_lhsInested) :: Bool) ->
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
         (T_Type_vOut49 _keyIcopy _keyIpp _keyIprec) = inv_Type_s50 _keyX50 (T_Type_vIn49 _keyOnested)
         (T_Type_vOut49 _valueIcopy _valueIpp _valueIprec) = inv_Type_s50 _valueX50 (T_Type_vIn49 _valueOnested)
         _lhsOprec :: Int
         _lhsOprec = rule457  ()
         _lhsOpp :: PP_Doc
         _lhsOpp = rule458 _keyIpp _valueIpp
         _copy = rule459 _keyIcopy _valueIcopy
         _lhsOcopy :: Type
         _lhsOcopy = rule460 _copy
         _keyOnested = rule461 _lhsInested
         _valueOnested = rule462 _lhsInested
         !__result_ = T_Type_vOut49 _lhsOcopy _lhsOpp _lhsOprec
         in __result_ )
     in C_Type_s50 v49
   {-# INLINE rule457 #-}
   {-# LINE 312 "./src-ag/PrintCleanCode.ag" #-}
   rule457 = \  (_ :: ()) ->
                               {-# LINE 312 "./src-ag/PrintCleanCode.ag" #-}
                               5
                               {-# LINE 4174 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule458 #-}
   {-# LINE 313 "./src-ag/PrintCleanCode.ag" #-}
   rule458 = \ ((_keyIpp) :: PP_Doc) ((_valueIpp) :: PP_Doc) ->
                               {-# LINE 313 "./src-ag/PrintCleanCode.ag" #-}
                               text "'Data.Map'.Map" >#< pp_parens _keyIpp >#< pp_parens _valueIpp
                               {-# LINE 4180 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule459 #-}
   rule459 = \ ((_keyIcopy) :: Type) ((_valueIcopy) :: Type) ->
     TMap _keyIcopy _valueIcopy
   {-# INLINE rule460 #-}
   rule460 = \ _copy ->
     _copy
   {-# INLINE rule461 #-}
   rule461 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule462 #-}
   rule462 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
{-# NOINLINE sem_Type_TIntMap #-}
sem_Type_TIntMap :: T_Type  -> T_Type 
sem_Type_TIntMap arg_value_ = T_Type (return st50) where
   {-# NOINLINE st50 #-}
   !st50 = let
      v49 :: T_Type_v49 
      v49 = \ !(T_Type_vIn49 _lhsInested) -> ( let
         _valueX50 = Control.Monad.Identity.runIdentity (attach_T_Type (arg_value_))
         (T_Type_vOut49 _valueIcopy _valueIpp _valueIprec) = inv_Type_s50 _valueX50 (T_Type_vIn49 _valueOnested)
         _lhsOprec :: Int
         _lhsOprec = rule463  ()
         _lhsOpp :: PP_Doc
         _lhsOpp = rule464 _valueIpp
         _copy = rule465 _valueIcopy
         _lhsOcopy :: Type
         _lhsOcopy = rule466 _copy
         _valueOnested = rule467 _lhsInested
         !__result_ = T_Type_vOut49 _lhsOcopy _lhsOpp _lhsOprec
         in __result_ )
     in C_Type_s50 v49
   {-# INLINE rule463 #-}
   {-# LINE 314 "./src-ag/PrintCleanCode.ag" #-}
   rule463 = \  (_ :: ()) ->
                               {-# LINE 314 "./src-ag/PrintCleanCode.ag" #-}
                               5
                               {-# LINE 4218 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule464 #-}
   {-# LINE 315 "./src-ag/PrintCleanCode.ag" #-}
   rule464 = \ ((_valueIpp) :: PP_Doc) ->
                               {-# LINE 315 "./src-ag/PrintCleanCode.ag" #-}
                               text "'Data.IntMap'.IntMap" >#< pp_parens _valueIpp
                               {-# LINE 4224 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule465 #-}
   rule465 = \ ((_valueIcopy) :: Type) ->
     TIntMap _valueIcopy
   {-# INLINE rule466 #-}
   rule466 = \ _copy ->
     _copy
   {-# INLINE rule467 #-}
   rule467 = \ ((_lhsInested) :: Bool) ->
     _lhsInested

-- Types -------------------------------------------------------
-- wrapper
data Inh_Types  = Inh_Types { nested_Inh_Types :: !(Bool) }
data Syn_Types  = Syn_Types { copy_Syn_Types :: !(Types), pps_Syn_Types :: !(PP_Docs) }
{-# INLINABLE wrap_Types #-}
wrap_Types :: T_Types  -> Inh_Types  -> (Syn_Types )
wrap_Types !(T_Types act) !(Inh_Types _lhsInested) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_Types_vIn52 _lhsInested
        !(T_Types_vOut52 _lhsOcopy _lhsOpps) <- return (inv_Types_s53 sem arg)
        return (Syn_Types _lhsOcopy _lhsOpps)
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
data T_Types_vOut52  = T_Types_vOut52 (Types) (PP_Docs)
{-# NOINLINE sem_Types_Cons #-}
sem_Types_Cons :: T_Type  -> T_Types  -> T_Types 
sem_Types_Cons arg_hd_ arg_tl_ = T_Types (return st53) where
   {-# NOINLINE st53 #-}
   !st53 = let
      v52 :: T_Types_v52 
      v52 = \ !(T_Types_vIn52 _lhsInested) -> ( let
         _hdX50 = Control.Monad.Identity.runIdentity (attach_T_Type (arg_hd_))
         _tlX53 = Control.Monad.Identity.runIdentity (attach_T_Types (arg_tl_))
         (T_Type_vOut49 _hdIcopy _hdIpp _hdIprec) = inv_Type_s50 _hdX50 (T_Type_vIn49 _hdOnested)
         (T_Types_vOut52 _tlIcopy _tlIpps) = inv_Types_s53 _tlX53 (T_Types_vIn52 _tlOnested)
         _lhsOpps :: PP_Docs
         _lhsOpps = rule468 _hdIpp _tlIpps
         _copy = rule469 _hdIcopy _tlIcopy
         _lhsOcopy :: Types
         _lhsOcopy = rule470 _copy
         _hdOnested = rule471 _lhsInested
         _tlOnested = rule472 _lhsInested
         !__result_ = T_Types_vOut52 _lhsOcopy _lhsOpps
         in __result_ )
     in C_Types_s53 v52
   {-# INLINE rule468 #-}
   {-# LINE 82 "./src-ag/PrintCleanCode.ag" #-}
   rule468 = \ ((_hdIpp) :: PP_Doc) ((_tlIpps) :: PP_Docs) ->
                     {-# LINE 82 "./src-ag/PrintCleanCode.ag" #-}
                     _hdIpp : _tlIpps
                     {-# LINE 4291 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule469 #-}
   rule469 = \ ((_hdIcopy) :: Type) ((_tlIcopy) :: Types) ->
     (:) _hdIcopy _tlIcopy
   {-# INLINE rule470 #-}
   rule470 = \ _copy ->
     _copy
   {-# INLINE rule471 #-}
   rule471 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule472 #-}
   rule472 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
{-# NOINLINE sem_Types_Nil #-}
sem_Types_Nil ::  T_Types 
sem_Types_Nil  = T_Types (return st53) where
   {-# NOINLINE st53 #-}
   !st53 = let
      v52 :: T_Types_v52 
      v52 = \ !(T_Types_vIn52 _lhsInested) -> ( let
         _lhsOpps :: PP_Docs
         _lhsOpps = rule473  ()
         _copy = rule474  ()
         _lhsOcopy :: Types
         _lhsOcopy = rule475 _copy
         !__result_ = T_Types_vOut52 _lhsOcopy _lhsOpps
         in __result_ )
     in C_Types_s53 v52
   {-# INLINE rule473 #-}
   {-# LINE 83 "./src-ag/PrintCleanCode.ag" #-}
   rule473 = \  (_ :: ()) ->
                     {-# LINE 83 "./src-ag/PrintCleanCode.ag" #-}
                     []
                     {-# LINE 4324 "dist/build/PrintCleanCode.hs"#-}
   {-# INLINE rule474 #-}
   rule474 = \  (_ :: ()) ->
     []
   {-# INLINE rule475 #-}
   rule475 = \ _copy ->
     _copy
