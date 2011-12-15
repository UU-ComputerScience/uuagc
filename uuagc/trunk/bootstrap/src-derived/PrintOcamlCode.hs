{-# OPTIONS_GHC -XBangPatterns #-}

-- UUAGC 0.9.40.1 (src-ag/PrintOcamlCode.ag)
module PrintOcamlCode where
{-# LINE 10 "src-ag/PrintOcamlCode.ag" #-}

import Pretty
import Code
import Patterns
import Options
import CommonTypes hiding (List,Type,Map,Maybe,IntMap,Either)
import Data.List(intersperse,intercalate)
import Data.Char(toLower)
{-# LINE 15 "dist/build/PrintOcamlCode.hs" #-}

{-# LINE 2 "src-ag/Code.ag" #-}

import Pretty
import Patterns
import Data.List(partition)
import Data.Set(Set)
import qualified Data.Set as Set
import Data.Map(Map)
import qualified Data.Map as Map
{-# LINE 26 "dist/build/PrintOcamlCode.hs" #-}

{-# LINE 2 "src-ag/Patterns.ag" #-}

-- Patterns.ag imports
import UU.Scanner.Position(Pos)
import CommonTypes (ConstructorIdent,Identifier)
{-# LINE 33 "dist/build/PrintOcamlCode.hs" #-}
{-# LINE 21 "src-ag/PrintOcamlCode.ag" #-}

type PP_Docs = [PP_Doc]

ppMultiSeqH :: [PP_Doc] -> PP_Doc -> PP_Doc
ppMultiSeqH = ppMultiSeq' (>#<)

ppMultiSeqV :: [PP_Doc] -> PP_Doc -> PP_Doc
ppMultiSeqV = ppMultiSeq' (>-<)

ppMultiSeq' :: (PP_Doc -> PP_Doc -> PP_Doc) -> [PP_Doc] -> PP_Doc -> PP_Doc
ppMultiSeq' next strictArgs expr
  = foldr (\v r -> (v >#< "`seq`") `next` pp_parens r) expr strictArgs

ppTuple True  pps = "(" >|< pp_block " " (replicate (length pps `max` 1) ')') ",(" pps
ppTuple False pps = "(" >|< pp_block " " ")" "," pps
{-# LINE 50 "dist/build/PrintOcamlCode.hs" #-}

{-# LINE 174 "src-ag/PrintOcamlCode.ag" #-}

toOcamlTC (c:cs) = toLower c : cs
toOcamlTC xs = xs
{-# LINE 56 "dist/build/PrintOcamlCode.hs" #-}

{-# LINE 146 "src-ag/Code.ag" #-}

-- Unboxed tuples
--   unbox  Whether unboxed tuples are wanted or not
--   inh    The inherited attributes.
--          If there are none, no unboxing can take place,
--          because in that case the semantic function (a top-level identifier) would have an unboxed type.
-- Of course we can't have an unboxed 1-tuple
mkTupleExpr :: Bool -> Bool -> Exprs -> Expr
mkTupleExpr unbox noInh exprs | not unbox || noInh || length exprs == 1 = TupleExpr exprs
                              | otherwise                               = UnboxedTupleExpr exprs
mkTupleType :: Bool -> Bool -> Types -> Type
mkTupleType unbox noInh tps | not unbox || noInh || length tps == 1 = TupleType tps
                            | otherwise                             = UnboxedTupleType tps
mkTupleLhs :: Bool -> Bool -> [String] -> Lhs
mkTupleLhs  unbox noInh comps | not unbox || noInh || length comps == 1 = TupleLhs comps
                              | otherwise                               = UnboxedTupleLhs comps
{-# LINE 75 "dist/build/PrintOcamlCode.hs" #-}
-- CaseAlt -----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         options              : Options
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative CaseAlt:
         child left           : Lhs 
         child expr           : Expr 
-}
-- cata
sem_CaseAlt :: CaseAlt ->
               T_CaseAlt
sem_CaseAlt !(CaseAlt _left _expr) =
    (sem_CaseAlt_CaseAlt (sem_Lhs _left) (sem_Expr _expr))
-- semantic domain
newtype T_CaseAlt = T_CaseAlt (Options ->
                               ( PP_Doc))
data Inh_CaseAlt = Inh_CaseAlt {options_Inh_CaseAlt :: !(Options)}
data Syn_CaseAlt = Syn_CaseAlt {pp_Syn_CaseAlt :: !(PP_Doc)}
wrap_CaseAlt :: T_CaseAlt ->
                Inh_CaseAlt ->
                Syn_CaseAlt
wrap_CaseAlt !(T_CaseAlt sem) !(Inh_CaseAlt _lhsIoptions) =
    (let ( !_lhsOpp) = sem _lhsIoptions
     in  (Syn_CaseAlt _lhsOpp))
sem_CaseAlt_CaseAlt :: T_Lhs ->
                       T_Expr ->
                       T_CaseAlt
sem_CaseAlt_CaseAlt !(T_Lhs left_) !(T_Expr expr_) =
    (T_CaseAlt (\ (!_lhsIoptions) ->
                    (case (({-# LINE 43 "src-ag/PrintOcamlCode.ag" #-}
                            _lhsIoptions
                            {-# LINE 111 "dist/build/PrintOcamlCode" #-}
                            )) of
                     { !_exprOoptions ->
                     (case (expr_ _exprOoptions) of
                      { ( !_exprIpp) ->
                          (case (({-# LINE 43 "src-ag/PrintOcamlCode.ag" #-}
                                  _lhsIoptions
                                  {-# LINE 118 "dist/build/PrintOcamlCode" #-}
                                  )) of
                           { !_leftOoptions ->
                           (case (left_ _leftOoptions) of
                            { ( !_leftIpp) ->
                                (case (({-# LINE 180 "src-ag/PrintOcamlCode.ag" #-}
                                        _leftIpp >#< "->" >#< _exprIpp
                                        {-# LINE 125 "dist/build/PrintOcamlCode" #-}
                                        )) of
                                 { !_lhsOpp ->
                                 (case ((Syn_CaseAlt _lhsOpp)) of
                                  { ___node ->
                                  ( _lhsOpp) }) }) }) }) }) })))
-- CaseAlts ----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         options              : Options
      synthesized attribute:
         pps                  : PP_Docs
   alternatives:
      alternative Cons:
         child hd             : CaseAlt 
         child tl             : CaseAlts 
      alternative Nil:
-}
-- cata
sem_CaseAlts :: CaseAlts ->
                T_CaseAlts
sem_CaseAlts !list =
    (Prelude.foldr sem_CaseAlts_Cons sem_CaseAlts_Nil (Prelude.map sem_CaseAlt list))
-- semantic domain
newtype T_CaseAlts = T_CaseAlts (Options ->
                                 ( PP_Docs))
data Inh_CaseAlts = Inh_CaseAlts {options_Inh_CaseAlts :: !(Options)}
data Syn_CaseAlts = Syn_CaseAlts {pps_Syn_CaseAlts :: !(PP_Docs)}
wrap_CaseAlts :: T_CaseAlts ->
                 Inh_CaseAlts ->
                 Syn_CaseAlts
wrap_CaseAlts !(T_CaseAlts sem) !(Inh_CaseAlts _lhsIoptions) =
    (let ( !_lhsOpps) = sem _lhsIoptions
     in  (Syn_CaseAlts _lhsOpps))
sem_CaseAlts_Cons :: T_CaseAlt ->
                     T_CaseAlts ->
                     T_CaseAlts
sem_CaseAlts_Cons !(T_CaseAlt hd_) !(T_CaseAlts tl_) =
    (T_CaseAlts (\ (!_lhsIoptions) ->
                     (case (({-# LINE 43 "src-ag/PrintOcamlCode.ag" #-}
                             _lhsIoptions
                             {-# LINE 167 "dist/build/PrintOcamlCode" #-}
                             )) of
                      { !_tlOoptions ->
                      (case (tl_ _tlOoptions) of
                       { ( !_tlIpps) ->
                           (case (({-# LINE 43 "src-ag/PrintOcamlCode.ag" #-}
                                   _lhsIoptions
                                   {-# LINE 174 "dist/build/PrintOcamlCode" #-}
                                   )) of
                            { !_hdOoptions ->
                            (case (hd_ _hdOoptions) of
                             { ( !_hdIpp) ->
                                 (case (({-# LINE 64 "src-ag/PrintOcamlCode.ag" #-}
                                         _hdIpp : _tlIpps
                                         {-# LINE 181 "dist/build/PrintOcamlCode" #-}
                                         )) of
                                  { !_lhsOpps ->
                                  (case ((Syn_CaseAlts _lhsOpps)) of
                                   { ___node ->
                                   ( _lhsOpps) }) }) }) }) }) })))
sem_CaseAlts_Nil :: T_CaseAlts
sem_CaseAlts_Nil =
    (T_CaseAlts (\ (!_lhsIoptions) ->
                     (case (({-# LINE 65 "src-ag/PrintOcamlCode.ag" #-}
                             []
                             {-# LINE 192 "dist/build/PrintOcamlCode" #-}
                             )) of
                      { !_lhsOpps ->
                      (case ((Syn_CaseAlts _lhsOpps)) of
                       { ___node ->
                       ( _lhsOpps) }) })))
-- Chunk -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         isToplevel           : Bool
         options              : Options
         textBlockMap         : Map BlockInfo PP_Doc
      synthesized attribute:
         pps                  : PP_Docs
   alternatives:
      alternative Chunk:
         child name           : {String}
         child comment        : Decl 
         child info           : Decls 
         child dataDef        : Decls 
         child cataFun        : Decls 
         child semDom         : Decls 
         child semWrapper     : Decls 
         child semFunctions   : Decls 
         child semNames       : {[String]}
-}
-- cata
sem_Chunk :: Chunk ->
             T_Chunk
sem_Chunk !(Chunk _name _comment _info _dataDef _cataFun _semDom _semWrapper _semFunctions _semNames) =
    (sem_Chunk_Chunk _name (sem_Decl _comment) (sem_Decls _info) (sem_Decls _dataDef) (sem_Decls _cataFun) (sem_Decls _semDom) (sem_Decls _semWrapper) (sem_Decls _semFunctions) _semNames)
-- semantic domain
newtype T_Chunk = T_Chunk (Bool ->
                           Options ->
                           (Map BlockInfo PP_Doc) ->
                           ( PP_Docs))
data Inh_Chunk = Inh_Chunk {isToplevel_Inh_Chunk :: !(Bool),options_Inh_Chunk :: !(Options),textBlockMap_Inh_Chunk :: !((Map BlockInfo PP_Doc))}
data Syn_Chunk = Syn_Chunk {pps_Syn_Chunk :: !(PP_Docs)}
wrap_Chunk :: T_Chunk ->
              Inh_Chunk ->
              Syn_Chunk
wrap_Chunk !(T_Chunk sem) !(Inh_Chunk _lhsIisToplevel _lhsIoptions _lhsItextBlockMap) =
    (let ( !_lhsOpps) = sem _lhsIisToplevel _lhsIoptions _lhsItextBlockMap
     in  (Syn_Chunk _lhsOpps))
sem_Chunk_Chunk :: String ->
                   T_Decl ->
                   T_Decls ->
                   T_Decls ->
                   T_Decls ->
                   T_Decls ->
                   T_Decls ->
                   T_Decls ->
                   ([String]) ->
                   T_Chunk
sem_Chunk_Chunk !name_ !(T_Decl comment_) !(T_Decls info_) !(T_Decls dataDef_) !(T_Decls cataFun_) !(T_Decls semDom_) !(T_Decls semWrapper_) !(T_Decls semFunctions_) !semNames_ =
    (T_Chunk (\ (!_lhsIisToplevel)
                (!_lhsIoptions)
                (!_lhsItextBlockMap) ->
                  (case (({-# LINE 209 "src-ag/PrintOcamlCode.ag" #-}
                          _lhsIisToplevel
                          {-# LINE 253 "dist/build/PrintOcamlCode" #-}
                          )) of
                   { !_semFunctionsOisToplevel ->
                   (case (({-# LINE 209 "src-ag/PrintOcamlCode.ag" #-}
                           _lhsIisToplevel
                           {-# LINE 258 "dist/build/PrintOcamlCode" #-}
                           )) of
                    { !_semWrapperOisToplevel ->
                    (case (({-# LINE 209 "src-ag/PrintOcamlCode.ag" #-}
                            _lhsIisToplevel
                            {-# LINE 263 "dist/build/PrintOcamlCode" #-}
                            )) of
                     { !_semDomOisToplevel ->
                     (case (({-# LINE 209 "src-ag/PrintOcamlCode.ag" #-}
                             _lhsIisToplevel
                             {-# LINE 268 "dist/build/PrintOcamlCode" #-}
                             )) of
                      { !_cataFunOisToplevel ->
                      (case (({-# LINE 209 "src-ag/PrintOcamlCode.ag" #-}
                              _lhsIisToplevel
                              {-# LINE 273 "dist/build/PrintOcamlCode" #-}
                              )) of
                       { !_dataDefOisToplevel ->
                       (case (({-# LINE 209 "src-ag/PrintOcamlCode.ag" #-}
                               _lhsIisToplevel
                               {-# LINE 278 "dist/build/PrintOcamlCode" #-}
                               )) of
                        { !_infoOisToplevel ->
                        (case (({-# LINE 209 "src-ag/PrintOcamlCode.ag" #-}
                                _lhsIisToplevel
                                {-# LINE 283 "dist/build/PrintOcamlCode" #-}
                                )) of
                         { !_commentOisToplevel ->
                         (case (({-# LINE 43 "src-ag/PrintOcamlCode.ag" #-}
                                 _lhsIoptions
                                 {-# LINE 288 "dist/build/PrintOcamlCode" #-}
                                 )) of
                          { !_semFunctionsOoptions ->
                          (case (semFunctions_ _semFunctionsOisToplevel _semFunctionsOoptions) of
                           { ( !_semFunctionsIpps) ->
                               (case (({-# LINE 43 "src-ag/PrintOcamlCode.ag" #-}
                                       _lhsIoptions
                                       {-# LINE 295 "dist/build/PrintOcamlCode" #-}
                                       )) of
                                { !_semWrapperOoptions ->
                                (case (semWrapper_ _semWrapperOisToplevel _semWrapperOoptions) of
                                 { ( !_semWrapperIpps) ->
                                     (case (({-# LINE 43 "src-ag/PrintOcamlCode.ag" #-}
                                             _lhsIoptions
                                             {-# LINE 302 "dist/build/PrintOcamlCode" #-}
                                             )) of
                                      { !_semDomOoptions ->
                                      (case (semDom_ _semDomOisToplevel _semDomOoptions) of
                                       { ( !_semDomIpps) ->
                                           (case (({-# LINE 43 "src-ag/PrintOcamlCode.ag" #-}
                                                   _lhsIoptions
                                                   {-# LINE 309 "dist/build/PrintOcamlCode" #-}
                                                   )) of
                                            { !_cataFunOoptions ->
                                            (case (cataFun_ _cataFunOisToplevel _cataFunOoptions) of
                                             { ( !_cataFunIpps) ->
                                                 (case (({-# LINE 43 "src-ag/PrintOcamlCode.ag" #-}
                                                         _lhsIoptions
                                                         {-# LINE 316 "dist/build/PrintOcamlCode" #-}
                                                         )) of
                                                  { !_dataDefOoptions ->
                                                  (case (dataDef_ _dataDefOisToplevel _dataDefOoptions) of
                                                   { ( !_dataDefIpps) ->
                                                       (case (({-# LINE 43 "src-ag/PrintOcamlCode.ag" #-}
                                                               _lhsIoptions
                                                               {-# LINE 323 "dist/build/PrintOcamlCode" #-}
                                                               )) of
                                                        { !_infoOoptions ->
                                                        (case (info_ _infoOisToplevel _infoOoptions) of
                                                         { ( !_infoIpps) ->
                                                             (case (({-# LINE 43 "src-ag/PrintOcamlCode.ag" #-}
                                                                     _lhsIoptions
                                                                     {-# LINE 330 "dist/build/PrintOcamlCode" #-}
                                                                     )) of
                                                              { !_commentOoptions ->
                                                              (case (comment_ _commentOisToplevel _commentOoptions) of
                                                               { ( !_commentIpp) ->
                                                                   (case (({-# LINE 96 "src-ag/PrintOcamlCode.ag" #-}
                                                                           _commentIpp
                                                                           :  _infoIpps
                                                                           ++ _dataDefIpps
                                                                           ++ _semDomIpps
                                                                           ++ _semFunctionsIpps
                                                                           ++ _semWrapperIpps
                                                                           ++ _cataFunIpps
                                                                           ++ [Map.findWithDefault empty (BlockOther, Just $ identifier name_) _lhsItextBlockMap]
                                                                           {-# LINE 344 "dist/build/PrintOcamlCode" #-}
                                                                           )) of
                                                                    { !_lhsOpps ->
                                                                    (case ((Syn_Chunk _lhsOpps)) of
                                                                     { ___node ->
                                                                     ( _lhsOpps) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })))
-- Chunks ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         isToplevel           : Bool
         options              : Options
         textBlockMap         : Map BlockInfo PP_Doc
      synthesized attribute:
         pps                  : PP_Docs
   alternatives:
      alternative Cons:
         child hd             : Chunk 
         child tl             : Chunks 
      alternative Nil:
-}
-- cata
sem_Chunks :: Chunks ->
              T_Chunks
sem_Chunks !list =
    (Prelude.foldr sem_Chunks_Cons sem_Chunks_Nil (Prelude.map sem_Chunk list))
-- semantic domain
newtype T_Chunks = T_Chunks (Bool ->
                             Options ->
                             (Map BlockInfo PP_Doc) ->
                             ( PP_Docs))
data Inh_Chunks = Inh_Chunks {isToplevel_Inh_Chunks :: !(Bool),options_Inh_Chunks :: !(Options),textBlockMap_Inh_Chunks :: !((Map BlockInfo PP_Doc))}
data Syn_Chunks = Syn_Chunks {pps_Syn_Chunks :: !(PP_Docs)}
wrap_Chunks :: T_Chunks ->
               Inh_Chunks ->
               Syn_Chunks
wrap_Chunks !(T_Chunks sem) !(Inh_Chunks _lhsIisToplevel _lhsIoptions _lhsItextBlockMap) =
    (let ( !_lhsOpps) = sem _lhsIisToplevel _lhsIoptions _lhsItextBlockMap
     in  (Syn_Chunks _lhsOpps))
sem_Chunks_Cons :: T_Chunk ->
                   T_Chunks ->
                   T_Chunks
sem_Chunks_Cons !(T_Chunk hd_) !(T_Chunks tl_) =
    (T_Chunks (\ (!_lhsIisToplevel)
                 (!_lhsIoptions)
                 (!_lhsItextBlockMap) ->
                   (case (({-# LINE 45 "src-ag/PrintOcamlCode.ag" #-}
                           _lhsItextBlockMap
                           {-# LINE 392 "dist/build/PrintOcamlCode" #-}
                           )) of
                    { !_tlOtextBlockMap ->
                    (case (({-# LINE 209 "src-ag/PrintOcamlCode.ag" #-}
                            _lhsIisToplevel
                            {-# LINE 397 "dist/build/PrintOcamlCode" #-}
                            )) of
                     { !_tlOisToplevel ->
                     (case (({-# LINE 45 "src-ag/PrintOcamlCode.ag" #-}
                             _lhsItextBlockMap
                             {-# LINE 402 "dist/build/PrintOcamlCode" #-}
                             )) of
                      { !_hdOtextBlockMap ->
                      (case (({-# LINE 209 "src-ag/PrintOcamlCode.ag" #-}
                              _lhsIisToplevel
                              {-# LINE 407 "dist/build/PrintOcamlCode" #-}
                              )) of
                       { !_hdOisToplevel ->
                       (case (({-# LINE 43 "src-ag/PrintOcamlCode.ag" #-}
                               _lhsIoptions
                               {-# LINE 412 "dist/build/PrintOcamlCode" #-}
                               )) of
                        { !_tlOoptions ->
                        (case (tl_ _tlOisToplevel _tlOoptions _tlOtextBlockMap) of
                         { ( !_tlIpps) ->
                             (case (({-# LINE 43 "src-ag/PrintOcamlCode.ag" #-}
                                     _lhsIoptions
                                     {-# LINE 419 "dist/build/PrintOcamlCode" #-}
                                     )) of
                              { !_hdOoptions ->
                              (case (hd_ _hdOisToplevel _hdOoptions _hdOtextBlockMap) of
                               { ( !_hdIpps) ->
                                   (case (({-# LINE 84 "src-ag/PrintOcamlCode.ag" #-}
                                           _hdIpps ++ _tlIpps
                                           {-# LINE 426 "dist/build/PrintOcamlCode" #-}
                                           )) of
                                    { !_lhsOpps ->
                                    (case ((Syn_Chunks _lhsOpps)) of
                                     { ___node ->
                                     ( _lhsOpps) }) }) }) }) }) }) }) }) }) })))
sem_Chunks_Nil :: T_Chunks
sem_Chunks_Nil =
    (T_Chunks (\ (!_lhsIisToplevel)
                 (!_lhsIoptions)
                 (!_lhsItextBlockMap) ->
                   (case (({-# LINE 85 "src-ag/PrintOcamlCode.ag" #-}
                           []
                           {-# LINE 439 "dist/build/PrintOcamlCode" #-}
                           )) of
                    { !_lhsOpps ->
                    (case ((Syn_Chunks _lhsOpps)) of
                     { ___node ->
                     ( _lhsOpps) }) })))
-- DataAlt -----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative DataAlt:
         child name           : {String}
         child args           : Types 
      alternative Record:
         child name           : {String}
         child args           : NamedTypes 
-}
-- cata
sem_DataAlt :: DataAlt ->
               T_DataAlt
sem_DataAlt !(DataAlt _name _args) =
    (sem_DataAlt_DataAlt _name (sem_Types _args))
sem_DataAlt !(Record _name _args) =
    (sem_DataAlt_Record _name (sem_NamedTypes _args))
-- semantic domain
newtype T_DataAlt = T_DataAlt (( PP_Doc))
data Inh_DataAlt = Inh_DataAlt {}
data Syn_DataAlt = Syn_DataAlt {pp_Syn_DataAlt :: !(PP_Doc)}
wrap_DataAlt :: T_DataAlt ->
                Inh_DataAlt ->
                Syn_DataAlt
wrap_DataAlt !(T_DataAlt sem) !(Inh_DataAlt) =
    (let ( !_lhsOpp) = sem
     in  (Syn_DataAlt _lhsOpp))
sem_DataAlt_DataAlt :: String ->
                       T_Types ->
                       T_DataAlt
sem_DataAlt_DataAlt !name_ !(T_Types args_) =
    (T_DataAlt (case (args_) of
                { ( !_argsIpps) ->
                    (case (({-# LINE 183 "src-ag/PrintOcamlCode.ag" #-}
                            name_ >#< "of" >#< pp_block "" "" " * " (map pp_parens _argsIpps)
                            {-# LINE 483 "dist/build/PrintOcamlCode" #-}
                            )) of
                     { !_lhsOpp ->
                     (case ((Syn_DataAlt _lhsOpp)) of
                      { ___node ->
                      ( _lhsOpp) }) }) }))
sem_DataAlt_Record :: String ->
                      T_NamedTypes ->
                      T_DataAlt
sem_DataAlt_Record !name_ !(T_NamedTypes args_) =
    (T_DataAlt (case (args_) of
                { ( !_argsIpps) ->
                    (case (({-# LINE 184 "src-ag/PrintOcamlCode.ag" #-}
                            pp_block "{" "}" ";" _argsIpps
                            {-# LINE 497 "dist/build/PrintOcamlCode" #-}
                            )) of
                     { !_lhsOpp ->
                     (case ((Syn_DataAlt _lhsOpp)) of
                      { ___node ->
                      ( _lhsOpp) }) }) }))
-- DataAlts ----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         pps                  : PP_Docs
   alternatives:
      alternative Cons:
         child hd             : DataAlt 
         child tl             : DataAlts 
      alternative Nil:
-}
-- cata
sem_DataAlts :: DataAlts ->
                T_DataAlts
sem_DataAlts !list =
    (Prelude.foldr sem_DataAlts_Cons sem_DataAlts_Nil (Prelude.map sem_DataAlt list))
-- semantic domain
newtype T_DataAlts = T_DataAlts (( PP_Docs))
data Inh_DataAlts = Inh_DataAlts {}
data Syn_DataAlts = Syn_DataAlts {pps_Syn_DataAlts :: !(PP_Docs)}
wrap_DataAlts :: T_DataAlts ->
                 Inh_DataAlts ->
                 Syn_DataAlts
wrap_DataAlts !(T_DataAlts sem) !(Inh_DataAlts) =
    (let ( !_lhsOpps) = sem
     in  (Syn_DataAlts _lhsOpps))
sem_DataAlts_Cons :: T_DataAlt ->
                     T_DataAlts ->
                     T_DataAlts
sem_DataAlts_Cons !(T_DataAlt hd_) !(T_DataAlts tl_) =
    (T_DataAlts (case (tl_) of
                 { ( !_tlIpps) ->
                     (case (hd_) of
                      { ( !_hdIpp) ->
                          (case (({-# LINE 68 "src-ag/PrintOcamlCode.ag" #-}
                                  _hdIpp : _tlIpps
                                  {-# LINE 539 "dist/build/PrintOcamlCode" #-}
                                  )) of
                           { !_lhsOpps ->
                           (case ((Syn_DataAlts _lhsOpps)) of
                            { ___node ->
                            ( _lhsOpps) }) }) }) }))
sem_DataAlts_Nil :: T_DataAlts
sem_DataAlts_Nil =
    (T_DataAlts (case (({-# LINE 69 "src-ag/PrintOcamlCode.ag" #-}
                        []
                        {-# LINE 549 "dist/build/PrintOcamlCode" #-}
                        )) of
                 { !_lhsOpps ->
                 (case ((Syn_DataAlts _lhsOpps)) of
                  { ___node ->
                  ( _lhsOpps) }) }))
-- Decl --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         isToplevel           : Bool
         options              : Options
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative Bind:
         child left           : Lhs 
         child rhs            : Expr 
      alternative BindLet:
         child left           : Lhs 
         child rhs            : Expr 
      alternative Comment:
         child txt            : {String}
      alternative Data:
         child name           : {String}
         child params         : {[String]}
         child alts           : DataAlts 
         child strict         : {Bool}
         child derivings      : {[String]}
      alternative Decl:
         child left           : Lhs 
         child rhs            : Expr 
         child binds          : {Set String}
         child uses           : {Set String}
      alternative EvalDecl:
         child nt             : {String}
         child left           : Lhs 
         child rhs            : Expr 
      alternative NewType:
         child name           : {String}
         child params         : {[String]}
         child con            : {String}
         child tp             : Type 
      alternative PragmaDecl:
         child txt            : {String}
      alternative Resume:
         child monadic        : {Bool}
         child nt             : {String}
         child left           : Lhs 
         child rhs            : Expr 
      alternative TSig:
         child name           : {String}
         child tp             : Type 
      alternative Type:
         child name           : {String}
         child params         : {[String]}
         child tp             : Type 
-}
-- cata
sem_Decl :: Decl ->
            T_Decl
sem_Decl !(Bind _left _rhs) =
    (sem_Decl_Bind (sem_Lhs _left) (sem_Expr _rhs))
sem_Decl !(BindLet _left _rhs) =
    (sem_Decl_BindLet (sem_Lhs _left) (sem_Expr _rhs))
sem_Decl !(Comment _txt) =
    (sem_Decl_Comment _txt)
sem_Decl !(Data _name _params _alts _strict _derivings) =
    (sem_Decl_Data _name _params (sem_DataAlts _alts) _strict _derivings)
sem_Decl !(Decl _left _rhs _binds _uses) =
    (sem_Decl_Decl (sem_Lhs _left) (sem_Expr _rhs) _binds _uses)
sem_Decl !(EvalDecl _nt _left _rhs) =
    (sem_Decl_EvalDecl _nt (sem_Lhs _left) (sem_Expr _rhs))
sem_Decl !(NewType _name _params _con _tp) =
    (sem_Decl_NewType _name _params _con (sem_Type _tp))
sem_Decl !(PragmaDecl _txt) =
    (sem_Decl_PragmaDecl _txt)
sem_Decl !(Resume _monadic _nt _left _rhs) =
    (sem_Decl_Resume _monadic _nt (sem_Lhs _left) (sem_Expr _rhs))
sem_Decl !(TSig _name _tp) =
    (sem_Decl_TSig _name (sem_Type _tp))
sem_Decl !(Type _name _params _tp) =
    (sem_Decl_Type _name _params (sem_Type _tp))
-- semantic domain
newtype T_Decl = T_Decl (Bool ->
                         Options ->
                         ( PP_Doc))
data Inh_Decl = Inh_Decl {isToplevel_Inh_Decl :: !(Bool),options_Inh_Decl :: !(Options)}
data Syn_Decl = Syn_Decl {pp_Syn_Decl :: !(PP_Doc)}
wrap_Decl :: T_Decl ->
             Inh_Decl ->
             Syn_Decl
wrap_Decl !(T_Decl sem) !(Inh_Decl _lhsIisToplevel _lhsIoptions) =
    (let ( !_lhsOpp) = sem _lhsIisToplevel _lhsIoptions
     in  (Syn_Decl _lhsOpp))
sem_Decl_Bind :: T_Lhs ->
                 T_Expr ->
                 T_Decl
sem_Decl_Bind !(T_Lhs left_) !(T_Expr rhs_) =
    (T_Decl (\ (!_lhsIisToplevel)
               (!_lhsIoptions) ->
                 (case (({-# LINE 111 "src-ag/PrintOcamlCode.ag" #-}
                         error "pp of Decl.Bind not supported"
                         {-# LINE 652 "dist/build/PrintOcamlCode" #-}
                         )) of
                  { !_lhsOpp ->
                  (case ((Syn_Decl _lhsOpp)) of
                   { ___node ->
                   ( _lhsOpp) }) })))
sem_Decl_BindLet :: T_Lhs ->
                    T_Expr ->
                    T_Decl
sem_Decl_BindLet !(T_Lhs left_) !(T_Expr rhs_) =
    (T_Decl (\ (!_lhsIisToplevel)
               (!_lhsIoptions) ->
                 (case (({-# LINE 112 "src-ag/PrintOcamlCode.ag" #-}
                         error "pp of Decl.BindLet not supported"
                         {-# LINE 666 "dist/build/PrintOcamlCode" #-}
                         )) of
                  { !_lhsOpp ->
                  (case ((Syn_Decl _lhsOpp)) of
                   { ___node ->
                   ( _lhsOpp) }) })))
sem_Decl_Comment :: String ->
                    T_Decl
sem_Decl_Comment !txt_ =
    (T_Decl (\ (!_lhsIisToplevel)
               (!_lhsIoptions) ->
                 (case (({-# LINE 123 "src-ag/PrintOcamlCode.ag" #-}
                         if '\n' `elem` txt_
                           then "(* " >-< vlist (lines txt_) >-< "*)"
                           else "(*" >#< txt_ >#< "*)"
                         {-# LINE 681 "dist/build/PrintOcamlCode" #-}
                         )) of
                  { !_lhsOpp ->
                  (case ((Syn_Decl _lhsOpp)) of
                   { ___node ->
                   ( _lhsOpp) }) })))
sem_Decl_Data :: String ->
                 ([String]) ->
                 T_DataAlts ->
                 Bool ->
                 ([String]) ->
                 T_Decl
sem_Decl_Data !name_ !params_ !(T_DataAlts alts_) !strict_ !derivings_ =
    (T_Decl (\ (!_lhsIisToplevel)
               (!_lhsIoptions) ->
                 (case (alts_) of
                  { ( !_altsIpps) ->
                      (case (({-# LINE 113 "src-ag/PrintOcamlCode.ag" #-}
                              "type" >#< hv_sp (map (\p -> "'" >|< p) params_ ++ [text $ toOcamlTC name_])
                              >#<  ( case _altsIpps of
                                           [] -> empty
                                           (x:xs) ->              "=" >#<  x
                                                  >-< vlist (map ("|" >#<) xs)
                                   )
                              >#< ";;"
                              {-# LINE 706 "dist/build/PrintOcamlCode" #-}
                              )) of
                       { !_lhsOpp ->
                       (case ((Syn_Decl _lhsOpp)) of
                        { ___node ->
                        ( _lhsOpp) }) }) })))
sem_Decl_Decl :: T_Lhs ->
                 T_Expr ->
                 (Set String) ->
                 (Set String) ->
                 T_Decl
sem_Decl_Decl !(T_Lhs left_) !(T_Expr rhs_) !binds_ !uses_ =
    (T_Decl (\ (!_lhsIisToplevel)
               (!_lhsIoptions) ->
                 (case (({-# LINE 43 "src-ag/PrintOcamlCode.ag" #-}
                         _lhsIoptions
                         {-# LINE 722 "dist/build/PrintOcamlCode" #-}
                         )) of
                  { !_rhsOoptions ->
                  (case (rhs_ _rhsOoptions) of
                   { ( !_rhsIpp) ->
                       (case (({-# LINE 43 "src-ag/PrintOcamlCode.ag" #-}
                               _lhsIoptions
                               {-# LINE 729 "dist/build/PrintOcamlCode" #-}
                               )) of
                        { !_leftOoptions ->
                        (case (left_ _leftOoptions) of
                         { ( !_leftIpp) ->
                             (case (({-# LINE 106 "src-ag/PrintOcamlCode.ag" #-}
                                     if _lhsIisToplevel
                                     then "let" >#< _leftIpp >#< "="
                                          >-< indent 4 _rhsIpp >#< ";;"
                                     else "let" >#< _leftIpp >#< "="
                                          >-< indent 4 _rhsIpp >#< "in"
                                     {-# LINE 740 "dist/build/PrintOcamlCode" #-}
                                     )) of
                              { !_lhsOpp ->
                              (case ((Syn_Decl _lhsOpp)) of
                               { ___node ->
                               ( _lhsOpp) }) }) }) }) }) })))
sem_Decl_EvalDecl :: String ->
                     T_Lhs ->
                     T_Expr ->
                     T_Decl
sem_Decl_EvalDecl !nt_ !(T_Lhs left_) !(T_Expr rhs_) =
    (T_Decl (\ (!_lhsIisToplevel)
               (!_lhsIoptions) ->
                 (case (({-# LINE 43 "src-ag/PrintOcamlCode.ag" #-}
                         _lhsIoptions
                         {-# LINE 755 "dist/build/PrintOcamlCode" #-}
                         )) of
                  { !_rhsOoptions ->
                  (case (rhs_ _rhsOoptions) of
                   { ( !_rhsIpp) ->
                       (case (({-# LINE 53 "src-ag/PrintOcamlCode.ag" #-}
                               _rhsIpp
                               {-# LINE 762 "dist/build/PrintOcamlCode" #-}
                               )) of
                        { !_lhsOpp ->
                        (case ((Syn_Decl _lhsOpp)) of
                         { ___node ->
                         ( _lhsOpp) }) }) }) })))
sem_Decl_NewType :: String ->
                    ([String]) ->
                    String ->
                    T_Type ->
                    T_Decl
sem_Decl_NewType !name_ !params_ !con_ !(T_Type tp_) =
    (T_Decl (\ (!_lhsIisToplevel)
               (!_lhsIoptions) ->
                 (case (({-# LINE 120 "src-ag/PrintOcamlCode.ag" #-}
                         error "pp of Decl.NewType not supported"
                         {-# LINE 778 "dist/build/PrintOcamlCode" #-}
                         )) of
                  { !_lhsOpp ->
                  (case ((Syn_Decl _lhsOpp)) of
                   { ___node ->
                   ( _lhsOpp) }) })))
sem_Decl_PragmaDecl :: String ->
                       T_Decl
sem_Decl_PragmaDecl !txt_ =
    (T_Decl (\ (!_lhsIisToplevel)
               (!_lhsIoptions) ->
                 (case (({-# LINE 126 "src-ag/PrintOcamlCode.ag" #-}
                         error "pp of Decl.PragmaDecl not supported"
                         {-# LINE 791 "dist/build/PrintOcamlCode" #-}
                         )) of
                  { !_lhsOpp ->
                  (case ((Syn_Decl _lhsOpp)) of
                   { ___node ->
                   ( _lhsOpp) }) })))
sem_Decl_Resume :: Bool ->
                   String ->
                   T_Lhs ->
                   T_Expr ->
                   T_Decl
sem_Decl_Resume !monadic_ !nt_ !(T_Lhs left_) !(T_Expr rhs_) =
    (T_Decl (\ (!_lhsIisToplevel)
               (!_lhsIoptions) ->
                 (case (({-# LINE 43 "src-ag/PrintOcamlCode.ag" #-}
                         _lhsIoptions
                         {-# LINE 807 "dist/build/PrintOcamlCode" #-}
                         )) of
                  { !_rhsOoptions ->
                  (case (rhs_ _rhsOoptions) of
                   { ( !_rhsIpp) ->
                       (case (({-# LINE 53 "src-ag/PrintOcamlCode.ag" #-}
                               _rhsIpp
                               {-# LINE 814 "dist/build/PrintOcamlCode" #-}
                               )) of
                        { !_lhsOpp ->
                        (case ((Syn_Decl _lhsOpp)) of
                         { ___node ->
                         ( _lhsOpp) }) }) }) })))
sem_Decl_TSig :: String ->
                 T_Type ->
                 T_Decl
sem_Decl_TSig !name_ !(T_Type tp_) =
    (T_Decl (\ (!_lhsIisToplevel)
               (!_lhsIoptions) ->
                 (case (tp_) of
                  { ( !_tpIpp) ->
                      (case (({-# LINE 122 "src-ag/PrintOcamlCode.ag" #-}
                              "(*" >#< name_ >#< ":" >#< _tpIpp >#< "*)"
                              {-# LINE 830 "dist/build/PrintOcamlCode" #-}
                              )) of
                       { !_lhsOpp ->
                       (case ((Syn_Decl _lhsOpp)) of
                        { ___node ->
                        ( _lhsOpp) }) }) })))
sem_Decl_Type :: String ->
                 ([String]) ->
                 T_Type ->
                 T_Decl
sem_Decl_Type !name_ !params_ !(T_Type tp_) =
    (T_Decl (\ (!_lhsIisToplevel)
               (!_lhsIoptions) ->
                 (case (tp_) of
                  { ( !_tpIpp) ->
                      (case (({-# LINE 121 "src-ag/PrintOcamlCode.ag" #-}
                              "type" >#< hv_sp (map (\p -> "'" >|< p) params_ ++ [text $ toOcamlTC name_]) >#< "=" >#<  _tpIpp >#< ";;"
                              {-# LINE 847 "dist/build/PrintOcamlCode" #-}
                              )) of
                       { !_lhsOpp ->
                       (case ((Syn_Decl _lhsOpp)) of
                        { ___node ->
                        ( _lhsOpp) }) }) })))
-- Decls -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         isToplevel           : Bool
         options              : Options
      synthesized attribute:
         pps                  : PP_Docs
   alternatives:
      alternative Cons:
         child hd             : Decl 
         child tl             : Decls 
      alternative Nil:
-}
-- cata
sem_Decls :: Decls ->
             T_Decls
sem_Decls !list =
    (Prelude.foldr sem_Decls_Cons sem_Decls_Nil (Prelude.map sem_Decl list))
-- semantic domain
newtype T_Decls = T_Decls (Bool ->
                           Options ->
                           ( PP_Docs))
data Inh_Decls = Inh_Decls {isToplevel_Inh_Decls :: !(Bool),options_Inh_Decls :: !(Options)}
data Syn_Decls = Syn_Decls {pps_Syn_Decls :: !(PP_Docs)}
wrap_Decls :: T_Decls ->
              Inh_Decls ->
              Syn_Decls
wrap_Decls !(T_Decls sem) !(Inh_Decls _lhsIisToplevel _lhsIoptions) =
    (let ( !_lhsOpps) = sem _lhsIisToplevel _lhsIoptions
     in  (Syn_Decls _lhsOpps))
sem_Decls_Cons :: T_Decl ->
                  T_Decls ->
                  T_Decls
sem_Decls_Cons !(T_Decl hd_) !(T_Decls tl_) =
    (T_Decls (\ (!_lhsIisToplevel)
                (!_lhsIoptions) ->
                  (case (({-# LINE 209 "src-ag/PrintOcamlCode.ag" #-}
                          _lhsIisToplevel
                          {-# LINE 892 "dist/build/PrintOcamlCode" #-}
                          )) of
                   { !_tlOisToplevel ->
                   (case (({-# LINE 209 "src-ag/PrintOcamlCode.ag" #-}
                           _lhsIisToplevel
                           {-# LINE 897 "dist/build/PrintOcamlCode" #-}
                           )) of
                    { !_hdOisToplevel ->
                    (case (({-# LINE 43 "src-ag/PrintOcamlCode.ag" #-}
                            _lhsIoptions
                            {-# LINE 902 "dist/build/PrintOcamlCode" #-}
                            )) of
                     { !_tlOoptions ->
                     (case (tl_ _tlOisToplevel _tlOoptions) of
                      { ( !_tlIpps) ->
                          (case (({-# LINE 43 "src-ag/PrintOcamlCode.ag" #-}
                                  _lhsIoptions
                                  {-# LINE 909 "dist/build/PrintOcamlCode" #-}
                                  )) of
                           { !_hdOoptions ->
                           (case (hd_ _hdOisToplevel _hdOoptions) of
                            { ( !_hdIpp) ->
                                (case (({-# LINE 80 "src-ag/PrintOcamlCode.ag" #-}
                                        _hdIpp : _tlIpps
                                        {-# LINE 916 "dist/build/PrintOcamlCode" #-}
                                        )) of
                                 { !_lhsOpps ->
                                 (case ((Syn_Decls _lhsOpps)) of
                                  { ___node ->
                                  ( _lhsOpps) }) }) }) }) }) }) }) })))
sem_Decls_Nil :: T_Decls
sem_Decls_Nil =
    (T_Decls (\ (!_lhsIisToplevel)
                (!_lhsIoptions) ->
                  (case (({-# LINE 81 "src-ag/PrintOcamlCode.ag" #-}
                          []
                          {-# LINE 928 "dist/build/PrintOcamlCode" #-}
                          )) of
                   { !_lhsOpps ->
                   (case ((Syn_Decls _lhsOpps)) of
                    { ___node ->
                    ( _lhsOpps) }) })))
-- Expr --------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         options              : Options
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative App:
         child name           : {String}
         child args           : Exprs 
      alternative Case:
         child expr           : Expr 
         child alts           : CaseAlts 
      alternative Do:
         child stmts          : Decls 
         child body           : Expr 
      alternative InvokeExpr:
         child nt             : {String}
         child expr           : Expr 
         child args           : Exprs 
      alternative Lambda:
         child args           : Exprs 
         child body           : Expr 
      alternative Let:
         child decls          : Decls 
         child body           : Expr 
      alternative LineExpr:
         child expr           : Expr 
      alternative PragmaExpr:
         child onLeftSide     : {Bool}
         child onNewLine      : {Bool}
         child txt            : {String}
         child expr           : Expr 
      alternative ResultExpr:
         child nt             : {String}
         child expr           : Expr 
      alternative ResumeExpr:
         child nt             : {String}
         child expr           : Expr 
         child left           : Lhs 
         child rhs            : Expr 
      alternative SemFun:
         child nt             : {String}
         child args           : Exprs 
         child body           : Expr 
      alternative SimpleExpr:
         child txt            : {String}
      alternative TextExpr:
         child lns            : {[String]}
      alternative Trace:
         child txt            : {String}
         child expr           : Expr 
      alternative TupleExpr:
         child exprs          : Exprs 
      alternative TypedExpr:
         child expr           : Expr 
         child tp             : Type 
      alternative UnboxedTupleExpr:
         child exprs          : Exprs 
-}
-- cata
sem_Expr :: Expr ->
            T_Expr
sem_Expr !(App _name _args) =
    (sem_Expr_App _name (sem_Exprs _args))
sem_Expr !(Case _expr _alts) =
    (sem_Expr_Case (sem_Expr _expr) (sem_CaseAlts _alts))
sem_Expr !(Do _stmts _body) =
    (sem_Expr_Do (sem_Decls _stmts) (sem_Expr _body))
sem_Expr !(InvokeExpr _nt _expr _args) =
    (sem_Expr_InvokeExpr _nt (sem_Expr _expr) (sem_Exprs _args))
sem_Expr !(Lambda _args _body) =
    (sem_Expr_Lambda (sem_Exprs _args) (sem_Expr _body))
sem_Expr !(Let _decls _body) =
    (sem_Expr_Let (sem_Decls _decls) (sem_Expr _body))
sem_Expr !(LineExpr _expr) =
    (sem_Expr_LineExpr (sem_Expr _expr))
sem_Expr !(PragmaExpr _onLeftSide _onNewLine _txt _expr) =
    (sem_Expr_PragmaExpr _onLeftSide _onNewLine _txt (sem_Expr _expr))
sem_Expr !(ResultExpr _nt _expr) =
    (sem_Expr_ResultExpr _nt (sem_Expr _expr))
sem_Expr !(ResumeExpr _nt _expr _left _rhs) =
    (sem_Expr_ResumeExpr _nt (sem_Expr _expr) (sem_Lhs _left) (sem_Expr _rhs))
sem_Expr !(SemFun _nt _args _body) =
    (sem_Expr_SemFun _nt (sem_Exprs _args) (sem_Expr _body))
sem_Expr !(SimpleExpr _txt) =
    (sem_Expr_SimpleExpr _txt)
sem_Expr !(TextExpr _lns) =
    (sem_Expr_TextExpr _lns)
sem_Expr !(Trace _txt _expr) =
    (sem_Expr_Trace _txt (sem_Expr _expr))
sem_Expr !(TupleExpr _exprs) =
    (sem_Expr_TupleExpr (sem_Exprs _exprs))
sem_Expr !(TypedExpr _expr _tp) =
    (sem_Expr_TypedExpr (sem_Expr _expr) (sem_Type _tp))
sem_Expr !(UnboxedTupleExpr _exprs) =
    (sem_Expr_UnboxedTupleExpr (sem_Exprs _exprs))
-- semantic domain
newtype T_Expr = T_Expr (Options ->
                         ( PP_Doc))
data Inh_Expr = Inh_Expr {options_Inh_Expr :: !(Options)}
data Syn_Expr = Syn_Expr {pp_Syn_Expr :: !(PP_Doc)}
wrap_Expr :: T_Expr ->
             Inh_Expr ->
             Syn_Expr
wrap_Expr !(T_Expr sem) !(Inh_Expr _lhsIoptions) =
    (let ( !_lhsOpp) = sem _lhsIoptions
     in  (Syn_Expr _lhsOpp))
sem_Expr_App :: String ->
                T_Exprs ->
                T_Expr
sem_Expr_App !name_ !(T_Exprs args_) =
    (T_Expr (\ (!_lhsIoptions) ->
                 (case (({-# LINE 43 "src-ag/PrintOcamlCode.ag" #-}
                         _lhsIoptions
                         {-# LINE 1050 "dist/build/PrintOcamlCode" #-}
                         )) of
                  { !_argsOoptions ->
                  (case (args_ _argsOoptions) of
                   { ( !_argsIpps) ->
                       (case (({-# LINE 143 "src-ag/PrintOcamlCode.ag" #-}
                               pp_parens $ name_ >#< hv_sp _argsIpps
                               {-# LINE 1057 "dist/build/PrintOcamlCode" #-}
                               )) of
                        { !_lhsOpp ->
                        (case ((Syn_Expr _lhsOpp)) of
                         { ___node ->
                         ( _lhsOpp) }) }) }) })))
sem_Expr_Case :: T_Expr ->
                 T_CaseAlts ->
                 T_Expr
sem_Expr_Case !(T_Expr expr_) !(T_CaseAlts alts_) =
    (T_Expr (\ (!_lhsIoptions) ->
                 (case (({-# LINE 43 "src-ag/PrintOcamlCode.ag" #-}
                         _lhsIoptions
                         {-# LINE 1070 "dist/build/PrintOcamlCode" #-}
                         )) of
                  { !_altsOoptions ->
                  (case (alts_ _altsOoptions) of
                   { ( !_altsIpps) ->
                       (case (({-# LINE 43 "src-ag/PrintOcamlCode.ag" #-}
                               _lhsIoptions
                               {-# LINE 1077 "dist/build/PrintOcamlCode" #-}
                               )) of
                        { !_exprOoptions ->
                        (case (expr_ _exprOoptions) of
                         { ( !_exprIpp) ->
                             (case (({-# LINE 131 "src-ag/PrintOcamlCode.ag" #-}
                                     pp_parens ( "match" >#< _exprIpp >#< "with"
                                               >-< indent 2 ( case _altsIpps of
                                                                [] -> empty
                                                                (x:xs) -> " " >#<  x
                                                                          >-< vlist (map ("|" >#<) xs)
                                                            )
                                               )
                                     {-# LINE 1090 "dist/build/PrintOcamlCode" #-}
                                     )) of
                              { !_lhsOpp ->
                              (case ((Syn_Expr _lhsOpp)) of
                               { ___node ->
                               ( _lhsOpp) }) }) }) }) }) })))
sem_Expr_Do :: T_Decls ->
               T_Expr ->
               T_Expr
sem_Expr_Do !(T_Decls stmts_) !(T_Expr body_) =
    (T_Expr (\ (!_lhsIoptions) ->
                 (case (({-# LINE 138 "src-ag/PrintOcamlCode.ag" #-}
                         error "pp of Expr.Do not supported"
                         {-# LINE 1103 "dist/build/PrintOcamlCode" #-}
                         )) of
                  { !_lhsOpp ->
                  (case ((Syn_Expr _lhsOpp)) of
                   { ___node ->
                   ( _lhsOpp) }) })))
sem_Expr_InvokeExpr :: String ->
                       T_Expr ->
                       T_Exprs ->
                       T_Expr
sem_Expr_InvokeExpr !nt_ !(T_Expr expr_) !(T_Exprs args_) =
    (T_Expr (\ (!_lhsIoptions) ->
                 (case (({-# LINE 43 "src-ag/PrintOcamlCode.ag" #-}
                         _lhsIoptions
                         {-# LINE 1117 "dist/build/PrintOcamlCode" #-}
                         )) of
                  { !_exprOoptions ->
                  (case (expr_ _exprOoptions) of
                   { ( !_exprIpp) ->
                       (case (({-# LINE 53 "src-ag/PrintOcamlCode.ag" #-}
                               _exprIpp
                               {-# LINE 1124 "dist/build/PrintOcamlCode" #-}
                               )) of
                        { !_lhsOpp ->
                        (case ((Syn_Expr _lhsOpp)) of
                         { ___node ->
                         ( _lhsOpp) }) }) }) })))
sem_Expr_Lambda :: T_Exprs ->
                   T_Expr ->
                   T_Expr
sem_Expr_Lambda !(T_Exprs args_) !(T_Expr body_) =
    (T_Expr (\ (!_lhsIoptions) ->
                 (case (({-# LINE 43 "src-ag/PrintOcamlCode.ag" #-}
                         _lhsIoptions
                         {-# LINE 1137 "dist/build/PrintOcamlCode" #-}
                         )) of
                  { !_bodyOoptions ->
                  (case (body_ _bodyOoptions) of
                   { ( !_bodyIpp) ->
                       (case (({-# LINE 43 "src-ag/PrintOcamlCode.ag" #-}
                               _lhsIoptions
                               {-# LINE 1144 "dist/build/PrintOcamlCode" #-}
                               )) of
                        { !_argsOoptions ->
                        (case (args_ _argsOoptions) of
                         { ( !_argsIpps) ->
                             (case (({-# LINE 139 "src-ag/PrintOcamlCode.ag" #-}
                                     pp_parens ( pp "fun" >#< hv_sp _argsIpps >#< "->"
                                               >-< indent 2 _bodyIpp )
                                     {-# LINE 1152 "dist/build/PrintOcamlCode" #-}
                                     )) of
                              { !_lhsOpp ->
                              (case ((Syn_Expr _lhsOpp)) of
                               { ___node ->
                               ( _lhsOpp) }) }) }) }) }) })))
sem_Expr_Let :: T_Decls ->
                T_Expr ->
                T_Expr
sem_Expr_Let !(T_Decls decls_) !(T_Expr body_) =
    (T_Expr (\ (!_lhsIoptions) ->
                 (case (({-# LINE 216 "src-ag/PrintOcamlCode.ag" #-}
                         False
                         {-# LINE 1165 "dist/build/PrintOcamlCode" #-}
                         )) of
                  { !_declsOisToplevel ->
                  (case (({-# LINE 43 "src-ag/PrintOcamlCode.ag" #-}
                          _lhsIoptions
                          {-# LINE 1170 "dist/build/PrintOcamlCode" #-}
                          )) of
                   { !_bodyOoptions ->
                   (case (body_ _bodyOoptions) of
                    { ( !_bodyIpp) ->
                        (case (({-# LINE 43 "src-ag/PrintOcamlCode.ag" #-}
                                _lhsIoptions
                                {-# LINE 1177 "dist/build/PrintOcamlCode" #-}
                                )) of
                         { !_declsOoptions ->
                         (case (decls_ _declsOisToplevel _declsOoptions) of
                          { ( !_declsIpps) ->
                              (case (({-# LINE 130 "src-ag/PrintOcamlCode.ag" #-}
                                      pp_parens $ vlist (_declsIpps ++ [_bodyIpp])
                                      {-# LINE 1184 "dist/build/PrintOcamlCode" #-}
                                      )) of
                               { !_lhsOpp ->
                               (case ((Syn_Expr _lhsOpp)) of
                                { ___node ->
                                ( _lhsOpp) }) }) }) }) }) }) })))
sem_Expr_LineExpr :: T_Expr ->
                     T_Expr
sem_Expr_LineExpr !(T_Expr expr_) =
    (T_Expr (\ (!_lhsIoptions) ->
                 (case (({-# LINE 43 "src-ag/PrintOcamlCode.ag" #-}
                         _lhsIoptions
                         {-# LINE 1196 "dist/build/PrintOcamlCode" #-}
                         )) of
                  { !_exprOoptions ->
                  (case (expr_ _exprOoptions) of
                   { ( !_exprIpp) ->
                       (case (({-# LINE 148 "src-ag/PrintOcamlCode.ag" #-}
                               _exprIpp
                               {-# LINE 1203 "dist/build/PrintOcamlCode" #-}
                               )) of
                        { !_lhsOpp ->
                        (case ((Syn_Expr _lhsOpp)) of
                         { ___node ->
                         ( _lhsOpp) }) }) }) })))
sem_Expr_PragmaExpr :: Bool ->
                       Bool ->
                       String ->
                       T_Expr ->
                       T_Expr
sem_Expr_PragmaExpr !onLeftSide_ !onNewLine_ !txt_ !(T_Expr expr_) =
    (T_Expr (\ (!_lhsIoptions) ->
                 (case (({-# LINE 43 "src-ag/PrintOcamlCode.ag" #-}
                         _lhsIoptions
                         {-# LINE 1218 "dist/build/PrintOcamlCode" #-}
                         )) of
                  { !_exprOoptions ->
                  (case (expr_ _exprOoptions) of
                   { ( !_exprIpp) ->
                       (case (({-# LINE 147 "src-ag/PrintOcamlCode.ag" #-}
                               _exprIpp
                               {-# LINE 1225 "dist/build/PrintOcamlCode" #-}
                               )) of
                        { !_lhsOpp ->
                        (case ((Syn_Expr _lhsOpp)) of
                         { ___node ->
                         ( _lhsOpp) }) }) }) })))
sem_Expr_ResultExpr :: String ->
                       T_Expr ->
                       T_Expr
sem_Expr_ResultExpr !nt_ !(T_Expr expr_) =
    (T_Expr (\ (!_lhsIoptions) ->
                 (case (({-# LINE 43 "src-ag/PrintOcamlCode.ag" #-}
                         _lhsIoptions
                         {-# LINE 1238 "dist/build/PrintOcamlCode" #-}
                         )) of
                  { !_exprOoptions ->
                  (case (expr_ _exprOoptions) of
                   { ( !_exprIpp) ->
                       (case (({-# LINE 53 "src-ag/PrintOcamlCode.ag" #-}
                               _exprIpp
                               {-# LINE 1245 "dist/build/PrintOcamlCode" #-}
                               )) of
                        { !_lhsOpp ->
                        (case ((Syn_Expr _lhsOpp)) of
                         { ___node ->
                         ( _lhsOpp) }) }) }) })))
sem_Expr_ResumeExpr :: String ->
                       T_Expr ->
                       T_Lhs ->
                       T_Expr ->
                       T_Expr
sem_Expr_ResumeExpr !nt_ !(T_Expr expr_) !(T_Lhs left_) !(T_Expr rhs_) =
    (T_Expr (\ (!_lhsIoptions) ->
                 (case (({-# LINE 43 "src-ag/PrintOcamlCode.ag" #-}
                         _lhsIoptions
                         {-# LINE 1260 "dist/build/PrintOcamlCode" #-}
                         )) of
                  { !_rhsOoptions ->
                  (case (rhs_ _rhsOoptions) of
                   { ( !_rhsIpp) ->
                       (case (({-# LINE 53 "src-ag/PrintOcamlCode.ag" #-}
                               _rhsIpp
                               {-# LINE 1267 "dist/build/PrintOcamlCode" #-}
                               )) of
                        { !_lhsOpp ->
                        (case ((Syn_Expr _lhsOpp)) of
                         { ___node ->
                         ( _lhsOpp) }) }) }) })))
sem_Expr_SemFun :: String ->
                   T_Exprs ->
                   T_Expr ->
                   T_Expr
sem_Expr_SemFun !nt_ !(T_Exprs args_) !(T_Expr body_) =
    (T_Expr (\ (!_lhsIoptions) ->
                 (case (({-# LINE 43 "src-ag/PrintOcamlCode.ag" #-}
                         _lhsIoptions
                         {-# LINE 1281 "dist/build/PrintOcamlCode" #-}
                         )) of
                  { !_bodyOoptions ->
                  (case (body_ _bodyOoptions) of
                   { ( !_bodyIpp) ->
                       (case (({-# LINE 53 "src-ag/PrintOcamlCode.ag" #-}
                               _bodyIpp
                               {-# LINE 1288 "dist/build/PrintOcamlCode" #-}
                               )) of
                        { !_lhsOpp ->
                        (case ((Syn_Expr _lhsOpp)) of
                         { ___node ->
                         ( _lhsOpp) }) }) }) })))
sem_Expr_SimpleExpr :: String ->
                       T_Expr
sem_Expr_SimpleExpr !txt_ =
    (T_Expr (\ (!_lhsIoptions) ->
                 (case (({-# LINE 144 "src-ag/PrintOcamlCode.ag" #-}
                         text txt_
                         {-# LINE 1300 "dist/build/PrintOcamlCode" #-}
                         )) of
                  { !_lhsOpp ->
                  (case ((Syn_Expr _lhsOpp)) of
                   { ___node ->
                   ( _lhsOpp) }) })))
sem_Expr_TextExpr :: ([String]) ->
                     T_Expr
sem_Expr_TextExpr !lns_ =
    (T_Expr (\ (!_lhsIoptions) ->
                 (case (({-# LINE 145 "src-ag/PrintOcamlCode.ag" #-}
                         vlist (map text lns_)
                         {-# LINE 1312 "dist/build/PrintOcamlCode" #-}
                         )) of
                  { !_lhsOpp ->
                  (case ((Syn_Expr _lhsOpp)) of
                   { ___node ->
                   ( _lhsOpp) }) })))
sem_Expr_Trace :: String ->
                  T_Expr ->
                  T_Expr
sem_Expr_Trace !txt_ !(T_Expr expr_) =
    (T_Expr (\ (!_lhsIoptions) ->
                 (case (({-# LINE 43 "src-ag/PrintOcamlCode.ag" #-}
                         _lhsIoptions
                         {-# LINE 1325 "dist/build/PrintOcamlCode" #-}
                         )) of
                  { !_exprOoptions ->
                  (case (expr_ _exprOoptions) of
                   { ( !_exprIpp) ->
                       (case (({-# LINE 146 "src-ag/PrintOcamlCode.ag" #-}
                               _exprIpp
                               {-# LINE 1332 "dist/build/PrintOcamlCode" #-}
                               )) of
                        { !_lhsOpp ->
                        (case ((Syn_Expr _lhsOpp)) of
                         { ___node ->
                         ( _lhsOpp) }) }) }) })))
sem_Expr_TupleExpr :: T_Exprs ->
                      T_Expr
sem_Expr_TupleExpr !(T_Exprs exprs_) =
    (T_Expr (\ (!_lhsIoptions) ->
                 (case (({-# LINE 43 "src-ag/PrintOcamlCode.ag" #-}
                         _lhsIoptions
                         {-# LINE 1344 "dist/build/PrintOcamlCode" #-}
                         )) of
                  { !_exprsOoptions ->
                  (case (exprs_ _exprsOoptions) of
                   { ( !_exprsIpps) ->
                       (case (({-# LINE 141 "src-ag/PrintOcamlCode.ag" #-}
                               ppTuple False _exprsIpps
                               {-# LINE 1351 "dist/build/PrintOcamlCode" #-}
                               )) of
                        { !_lhsOpp ->
                        (case ((Syn_Expr _lhsOpp)) of
                         { ___node ->
                         ( _lhsOpp) }) }) }) })))
sem_Expr_TypedExpr :: T_Expr ->
                      T_Type ->
                      T_Expr
sem_Expr_TypedExpr !(T_Expr expr_) !(T_Type tp_) =
    (T_Expr (\ (!_lhsIoptions) ->
                 (case (({-# LINE 43 "src-ag/PrintOcamlCode.ag" #-}
                         _lhsIoptions
                         {-# LINE 1364 "dist/build/PrintOcamlCode" #-}
                         )) of
                  { !_exprOoptions ->
                  (case (expr_ _exprOoptions) of
                   { ( !_exprIpp) ->
                       (case (({-# LINE 149 "src-ag/PrintOcamlCode.ag" #-}
                               _exprIpp
                               {-# LINE 1371 "dist/build/PrintOcamlCode" #-}
                               )) of
                        { !_lhsOpp ->
                        (case ((Syn_Expr _lhsOpp)) of
                         { ___node ->
                         ( _lhsOpp) }) }) }) })))
sem_Expr_UnboxedTupleExpr :: T_Exprs ->
                             T_Expr
sem_Expr_UnboxedTupleExpr !(T_Exprs exprs_) =
    (T_Expr (\ (!_lhsIoptions) ->
                 (case (({-# LINE 142 "src-ag/PrintOcamlCode.ag" #-}
                         error "pp of Expr.UnboxedTupleExpr not supported"
                         {-# LINE 1383 "dist/build/PrintOcamlCode" #-}
                         )) of
                  { !_lhsOpp ->
                  (case ((Syn_Expr _lhsOpp)) of
                   { ___node ->
                   ( _lhsOpp) }) })))
-- Exprs -------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         options              : Options
      synthesized attribute:
         pps                  : PP_Docs
   alternatives:
      alternative Cons:
         child hd             : Expr 
         child tl             : Exprs 
      alternative Nil:
-}
-- cata
sem_Exprs :: Exprs ->
             T_Exprs
sem_Exprs !list =
    (Prelude.foldr sem_Exprs_Cons sem_Exprs_Nil (Prelude.map sem_Expr list))
-- semantic domain
newtype T_Exprs = T_Exprs (Options ->
                           ( PP_Docs))
data Inh_Exprs = Inh_Exprs {options_Inh_Exprs :: !(Options)}
data Syn_Exprs = Syn_Exprs {pps_Syn_Exprs :: !(PP_Docs)}
wrap_Exprs :: T_Exprs ->
              Inh_Exprs ->
              Syn_Exprs
wrap_Exprs !(T_Exprs sem) !(Inh_Exprs _lhsIoptions) =
    (let ( !_lhsOpps) = sem _lhsIoptions
     in  (Syn_Exprs _lhsOpps))
sem_Exprs_Cons :: T_Expr ->
                  T_Exprs ->
                  T_Exprs
sem_Exprs_Cons !(T_Expr hd_) !(T_Exprs tl_) =
    (T_Exprs (\ (!_lhsIoptions) ->
                  (case (({-# LINE 43 "src-ag/PrintOcamlCode.ag" #-}
                          _lhsIoptions
                          {-# LINE 1425 "dist/build/PrintOcamlCode" #-}
                          )) of
                   { !_tlOoptions ->
                   (case (tl_ _tlOoptions) of
                    { ( !_tlIpps) ->
                        (case (({-# LINE 43 "src-ag/PrintOcamlCode.ag" #-}
                                _lhsIoptions
                                {-# LINE 1432 "dist/build/PrintOcamlCode" #-}
                                )) of
                         { !_hdOoptions ->
                         (case (hd_ _hdOoptions) of
                          { ( !_hdIpp) ->
                              (case (({-# LINE 60 "src-ag/PrintOcamlCode.ag" #-}
                                      _hdIpp : _tlIpps
                                      {-# LINE 1439 "dist/build/PrintOcamlCode" #-}
                                      )) of
                               { !_lhsOpps ->
                               (case ((Syn_Exprs _lhsOpps)) of
                                { ___node ->
                                ( _lhsOpps) }) }) }) }) }) })))
sem_Exprs_Nil :: T_Exprs
sem_Exprs_Nil =
    (T_Exprs (\ (!_lhsIoptions) ->
                  (case (({-# LINE 61 "src-ag/PrintOcamlCode.ag" #-}
                          []
                          {-# LINE 1450 "dist/build/PrintOcamlCode" #-}
                          )) of
                   { !_lhsOpps ->
                   (case ((Syn_Exprs _lhsOpps)) of
                    { ___node ->
                    ( _lhsOpps) }) })))
-- Lhs ---------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         options              : Options
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative Fun:
         child name           : {String}
         child args           : Exprs 
      alternative Pattern3:
         child pat3           : Pattern 
      alternative Pattern3SM:
         child pat3           : Pattern 
      alternative TupleLhs:
         child comps          : {[String]}
      alternative UnboxedTupleLhs:
         child comps          : {[String]}
      alternative Unwrap:
         child name           : {String}
         child sub            : Lhs 
-}
-- cata
sem_Lhs :: Lhs ->
           T_Lhs
sem_Lhs !(Fun _name _args) =
    (sem_Lhs_Fun _name (sem_Exprs _args))
sem_Lhs !(Pattern3 _pat3) =
    (sem_Lhs_Pattern3 (sem_Pattern _pat3))
sem_Lhs !(Pattern3SM _pat3) =
    (sem_Lhs_Pattern3SM (sem_Pattern _pat3))
sem_Lhs !(TupleLhs _comps) =
    (sem_Lhs_TupleLhs _comps)
sem_Lhs !(UnboxedTupleLhs _comps) =
    (sem_Lhs_UnboxedTupleLhs _comps)
sem_Lhs !(Unwrap _name _sub) =
    (sem_Lhs_Unwrap _name (sem_Lhs _sub))
-- semantic domain
newtype T_Lhs = T_Lhs (Options ->
                       ( PP_Doc))
data Inh_Lhs = Inh_Lhs {options_Inh_Lhs :: !(Options)}
data Syn_Lhs = Syn_Lhs {pp_Syn_Lhs :: !(PP_Doc)}
wrap_Lhs :: T_Lhs ->
            Inh_Lhs ->
            Syn_Lhs
wrap_Lhs !(T_Lhs sem) !(Inh_Lhs _lhsIoptions) =
    (let ( !_lhsOpp) = sem _lhsIoptions
     in  (Syn_Lhs _lhsOpp))
sem_Lhs_Fun :: String ->
               T_Exprs ->
               T_Lhs
sem_Lhs_Fun !name_ !(T_Exprs args_) =
    (T_Lhs (\ (!_lhsIoptions) ->
                (case (({-# LINE 43 "src-ag/PrintOcamlCode.ag" #-}
                        _lhsIoptions
                        {-# LINE 1512 "dist/build/PrintOcamlCode" #-}
                        )) of
                 { !_argsOoptions ->
                 (case (args_ _argsOoptions) of
                  { ( !_argsIpps) ->
                      (case (({-# LINE 156 "src-ag/PrintOcamlCode.ag" #-}
                              name_ >#< hv_sp _argsIpps
                              {-# LINE 1519 "dist/build/PrintOcamlCode" #-}
                              )) of
                       { !_lhsOpp ->
                       (case ((Syn_Lhs _lhsOpp)) of
                        { ___node ->
                        ( _lhsOpp) }) }) }) })))
sem_Lhs_Pattern3 :: T_Pattern ->
                    T_Lhs
sem_Lhs_Pattern3 !(T_Pattern pat3_) =
    (T_Lhs (\ (!_lhsIoptions) ->
                (case (({-# LINE 43 "src-ag/PrintOcamlCode.ag" #-}
                        _lhsIoptions
                        {-# LINE 1531 "dist/build/PrintOcamlCode" #-}
                        )) of
                 { !_pat3Ooptions ->
                 (case (pat3_ _pat3Ooptions) of
                  { ( !_pat3Icopy,!_pat3IisUnderscore,!_pat3Ipp) ->
                      (case (({-# LINE 152 "src-ag/PrintOcamlCode.ag" #-}
                              _pat3Ipp
                              {-# LINE 1538 "dist/build/PrintOcamlCode" #-}
                              )) of
                       { !_lhsOpp ->
                       (case ((Syn_Lhs _lhsOpp)) of
                        { ___node ->
                        ( _lhsOpp) }) }) }) })))
sem_Lhs_Pattern3SM :: T_Pattern ->
                      T_Lhs
sem_Lhs_Pattern3SM !(T_Pattern pat3_) =
    (T_Lhs (\ (!_lhsIoptions) ->
                (case (({-# LINE 153 "src-ag/PrintOcamlCode.ag" #-}
                        error "pp of Lhs.Pattern3SM not supported"
                        {-# LINE 1550 "dist/build/PrintOcamlCode" #-}
                        )) of
                 { !_lhsOpp ->
                 (case ((Syn_Lhs _lhsOpp)) of
                  { ___node ->
                  ( _lhsOpp) }) })))
sem_Lhs_TupleLhs :: ([String]) ->
                    T_Lhs
sem_Lhs_TupleLhs !comps_ =
    (T_Lhs (\ (!_lhsIoptions) ->
                (case (({-# LINE 154 "src-ag/PrintOcamlCode.ag" #-}
                        ppTuple False (map text comps_)
                        {-# LINE 1562 "dist/build/PrintOcamlCode" #-}
                        )) of
                 { !_lhsOpp ->
                 (case ((Syn_Lhs _lhsOpp)) of
                  { ___node ->
                  ( _lhsOpp) }) })))
sem_Lhs_UnboxedTupleLhs :: ([String]) ->
                           T_Lhs
sem_Lhs_UnboxedTupleLhs !comps_ =
    (T_Lhs (\ (!_lhsIoptions) ->
                (case (({-# LINE 155 "src-ag/PrintOcamlCode.ag" #-}
                        error "pp of Lhs.UnboxedTupleLhs not supported"
                        {-# LINE 1574 "dist/build/PrintOcamlCode" #-}
                        )) of
                 { !_lhsOpp ->
                 (case ((Syn_Lhs _lhsOpp)) of
                  { ___node ->
                  ( _lhsOpp) }) })))
sem_Lhs_Unwrap :: String ->
                  T_Lhs ->
                  T_Lhs
sem_Lhs_Unwrap !name_ !(T_Lhs sub_) =
    (T_Lhs (\ (!_lhsIoptions) ->
                (case (({-# LINE 43 "src-ag/PrintOcamlCode.ag" #-}
                        _lhsIoptions
                        {-# LINE 1587 "dist/build/PrintOcamlCode" #-}
                        )) of
                 { !_subOoptions ->
                 (case (sub_ _subOoptions) of
                  { ( !_subIpp) ->
                      (case (({-# LINE 157 "src-ag/PrintOcamlCode.ag" #-}
                              pp_parens (name_ >#< _subIpp)
                              {-# LINE 1594 "dist/build/PrintOcamlCode" #-}
                              )) of
                       { !_lhsOpp ->
                       (case ((Syn_Lhs _lhsOpp)) of
                        { ___node ->
                        ( _lhsOpp) }) }) }) })))
-- NamedType ---------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative Named:
         child strict         : {Bool}
         child name           : {String}
         child tp             : Type 
-}
-- cata
sem_NamedType :: NamedType ->
                 T_NamedType
sem_NamedType !(Named _strict _name _tp) =
    (sem_NamedType_Named _strict _name (sem_Type _tp))
-- semantic domain
newtype T_NamedType = T_NamedType (( PP_Doc))
data Inh_NamedType = Inh_NamedType {}
data Syn_NamedType = Syn_NamedType {pp_Syn_NamedType :: !(PP_Doc)}
wrap_NamedType :: T_NamedType ->
                  Inh_NamedType ->
                  Syn_NamedType
wrap_NamedType !(T_NamedType sem) !(Inh_NamedType) =
    (let ( !_lhsOpp) = sem
     in  (Syn_NamedType _lhsOpp))
sem_NamedType_Named :: Bool ->
                       String ->
                       T_Type ->
                       T_NamedType
sem_NamedType_Named !strict_ !name_ !(T_Type tp_) =
    (T_NamedType (case (tp_) of
                  { ( !_tpIpp) ->
                      (case (({-# LINE 187 "src-ag/PrintOcamlCode.ag" #-}
                              name_ >#< ":" >#< _tpIpp
                              {-# LINE 1635 "dist/build/PrintOcamlCode" #-}
                              )) of
                       { !_lhsOpp ->
                       (case ((Syn_NamedType _lhsOpp)) of
                        { ___node ->
                        ( _lhsOpp) }) }) }))
-- NamedTypes --------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         pps                  : PP_Docs
   alternatives:
      alternative Cons:
         child hd             : NamedType 
         child tl             : NamedTypes 
      alternative Nil:
-}
-- cata
sem_NamedTypes :: NamedTypes ->
                  T_NamedTypes
sem_NamedTypes !list =
    (Prelude.foldr sem_NamedTypes_Cons sem_NamedTypes_Nil (Prelude.map sem_NamedType list))
-- semantic domain
newtype T_NamedTypes = T_NamedTypes (( PP_Docs))
data Inh_NamedTypes = Inh_NamedTypes {}
data Syn_NamedTypes = Syn_NamedTypes {pps_Syn_NamedTypes :: !(PP_Docs)}
wrap_NamedTypes :: T_NamedTypes ->
                   Inh_NamedTypes ->
                   Syn_NamedTypes
wrap_NamedTypes !(T_NamedTypes sem) !(Inh_NamedTypes) =
    (let ( !_lhsOpps) = sem
     in  (Syn_NamedTypes _lhsOpps))
sem_NamedTypes_Cons :: T_NamedType ->
                       T_NamedTypes ->
                       T_NamedTypes
sem_NamedTypes_Cons !(T_NamedType hd_) !(T_NamedTypes tl_) =
    (T_NamedTypes (case (tl_) of
                   { ( !_tlIpps) ->
                       (case (hd_) of
                        { ( !_hdIpp) ->
                            (case (({-# LINE 76 "src-ag/PrintOcamlCode.ag" #-}
                                    _hdIpp : _tlIpps
                                    {-# LINE 1677 "dist/build/PrintOcamlCode" #-}
                                    )) of
                             { !_lhsOpps ->
                             (case ((Syn_NamedTypes _lhsOpps)) of
                              { ___node ->
                              ( _lhsOpps) }) }) }) }))
sem_NamedTypes_Nil :: T_NamedTypes
sem_NamedTypes_Nil =
    (T_NamedTypes (case (({-# LINE 77 "src-ag/PrintOcamlCode.ag" #-}
                          []
                          {-# LINE 1687 "dist/build/PrintOcamlCode" #-}
                          )) of
                   { !_lhsOpps ->
                   (case ((Syn_NamedTypes _lhsOpps)) of
                    { ___node ->
                    ( _lhsOpps) }) }))
-- Pattern -----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         options              : Options
      synthesized attributes:
         copy                 : Pattern 
         isUnderscore         : Bool
         pp                   : PP_Doc
   alternatives:
      alternative Alias:
         child field          : {Identifier}
         child attr           : {Identifier}
         child pat            : Pattern 
         visit 0:
            local copy        : _
      alternative Constr:
         child name           : {ConstructorIdent}
         child pats           : Patterns 
         visit 0:
            local copy        : _
      alternative Irrefutable:
         child pat            : Pattern 
         visit 0:
            local copy        : _
      alternative Product:
         child pos            : {Pos}
         child pats           : Patterns 
         visit 0:
            local copy        : _
      alternative Underscore:
         child pos            : {Pos}
         visit 0:
            local copy        : _
-}
-- cata
sem_Pattern :: Pattern ->
               T_Pattern
sem_Pattern !(Alias _field _attr _pat) =
    (sem_Pattern_Alias _field _attr (sem_Pattern _pat))
sem_Pattern !(Constr _name _pats) =
    (sem_Pattern_Constr _name (sem_Patterns _pats))
sem_Pattern !(Irrefutable _pat) =
    (sem_Pattern_Irrefutable (sem_Pattern _pat))
sem_Pattern !(Product _pos _pats) =
    (sem_Pattern_Product _pos (sem_Patterns _pats))
sem_Pattern !(Underscore _pos) =
    (sem_Pattern_Underscore _pos)
-- semantic domain
newtype T_Pattern = T_Pattern (Options ->
                               ( Pattern,Bool,PP_Doc))
data Inh_Pattern = Inh_Pattern {options_Inh_Pattern :: !(Options)}
data Syn_Pattern = Syn_Pattern {copy_Syn_Pattern :: !(Pattern),isUnderscore_Syn_Pattern :: !(Bool),pp_Syn_Pattern :: !(PP_Doc)}
wrap_Pattern :: T_Pattern ->
                Inh_Pattern ->
                Syn_Pattern
wrap_Pattern !(T_Pattern sem) !(Inh_Pattern _lhsIoptions) =
    (let ( !_lhsOcopy,!_lhsOisUnderscore,!_lhsOpp) = sem _lhsIoptions
     in  (Syn_Pattern _lhsOcopy _lhsOisUnderscore _lhsOpp))
sem_Pattern_Alias :: Identifier ->
                     Identifier ->
                     T_Pattern ->
                     T_Pattern
sem_Pattern_Alias !field_ !attr_ !(T_Pattern pat_) =
    (T_Pattern (\ (!_lhsIoptions) ->
                    (case (({-# LINE 43 "src-ag/PrintOcamlCode.ag" #-}
                            _lhsIoptions
                            {-# LINE 1760 "dist/build/PrintOcamlCode" #-}
                            )) of
                     { !_patOoptions ->
                     (case (pat_ _patOoptions) of
                      { ( !_patIcopy,!_patIisUnderscore,!_patIpp) ->
                          (case (({-# LINE 22 "src-ag/Patterns.ag" #-}
                                  Alias field_ attr_ _patIcopy
                                  {-# LINE 1767 "dist/build/PrintOcamlCode" #-}
                                  )) of
                           { !_copy ->
                           (case (({-# LINE 22 "src-ag/Patterns.ag" #-}
                                   _copy
                                   {-# LINE 1772 "dist/build/PrintOcamlCode" #-}
                                   )) of
                            { !_lhsOcopy ->
                            (case (({-# LINE 202 "src-ag/PrintOcamlCode.ag" #-}
                                    False
                                    {-# LINE 1777 "dist/build/PrintOcamlCode" #-}
                                    )) of
                             { !_lhsOisUnderscore ->
                             (case (({-# LINE 193 "src-ag/PrintOcamlCode.ag" #-}
                                     if _patIisUnderscore
                                      then pp (attrname False field_ attr_)
                                      else error "pp of Pattern.Alias is only supported in the form (x@_)"
                                     {-# LINE 1784 "dist/build/PrintOcamlCode" #-}
                                     )) of
                              { !_lhsOpp ->
                              (case ((Syn_Pattern _lhsOcopy _lhsOisUnderscore _lhsOpp)) of
                               { ___node ->
                               ( _lhsOcopy,_lhsOisUnderscore,_lhsOpp) }) }) }) }) }) }) })))
sem_Pattern_Constr :: ConstructorIdent ->
                      T_Patterns ->
                      T_Pattern
sem_Pattern_Constr !name_ !(T_Patterns pats_) =
    (T_Pattern (\ (!_lhsIoptions) ->
                    (case (({-# LINE 43 "src-ag/PrintOcamlCode.ag" #-}
                            _lhsIoptions
                            {-# LINE 1797 "dist/build/PrintOcamlCode" #-}
                            )) of
                     { !_patsOoptions ->
                     (case (pats_ _patsOoptions) of
                      { ( !_patsIcopy,!_patsIpps) ->
                          (case (({-# LINE 22 "src-ag/Patterns.ag" #-}
                                  Constr name_ _patsIcopy
                                  {-# LINE 1804 "dist/build/PrintOcamlCode" #-}
                                  )) of
                           { !_copy ->
                           (case (({-# LINE 22 "src-ag/Patterns.ag" #-}
                                   _copy
                                   {-# LINE 1809 "dist/build/PrintOcamlCode" #-}
                                   )) of
                            { !_lhsOcopy ->
                            (case (({-# LINE 200 "src-ag/PrintOcamlCode.ag" #-}
                                    False
                                    {-# LINE 1814 "dist/build/PrintOcamlCode" #-}
                                    )) of
                             { !_lhsOisUnderscore ->
                             (case (({-# LINE 190 "src-ag/PrintOcamlCode.ag" #-}
                                     pp_parens $ name_ >#< hv_sp _patsIpps
                                     {-# LINE 1819 "dist/build/PrintOcamlCode" #-}
                                     )) of
                              { !_lhsOpp ->
                              (case ((Syn_Pattern _lhsOcopy _lhsOisUnderscore _lhsOpp)) of
                               { ___node ->
                               ( _lhsOcopy,_lhsOisUnderscore,_lhsOpp) }) }) }) }) }) }) })))
sem_Pattern_Irrefutable :: T_Pattern ->
                           T_Pattern
sem_Pattern_Irrefutable !(T_Pattern pat_) =
    (T_Pattern (\ (!_lhsIoptions) ->
                    (case (({-# LINE 43 "src-ag/PrintOcamlCode.ag" #-}
                            _lhsIoptions
                            {-# LINE 1831 "dist/build/PrintOcamlCode" #-}
                            )) of
                     { !_patOoptions ->
                     (case (pat_ _patOoptions) of
                      { ( !_patIcopy,!_patIisUnderscore,!_patIpp) ->
                          (case (({-# LINE 22 "src-ag/Patterns.ag" #-}
                                  Irrefutable _patIcopy
                                  {-# LINE 1838 "dist/build/PrintOcamlCode" #-}
                                  )) of
                           { !_copy ->
                           (case (({-# LINE 22 "src-ag/Patterns.ag" #-}
                                   _copy
                                   {-# LINE 1843 "dist/build/PrintOcamlCode" #-}
                                   )) of
                            { !_lhsOcopy ->
                            (case (({-# LINE 199 "src-ag/PrintOcamlCode.ag" #-}
                                    _patIisUnderscore
                                    {-# LINE 1848 "dist/build/PrintOcamlCode" #-}
                                    )) of
                             { !_lhsOisUnderscore ->
                             (case (({-# LINE 196 "src-ag/PrintOcamlCode.ag" #-}
                                     error "pp of Pattern.Irrefutable not supported"
                                     {-# LINE 1853 "dist/build/PrintOcamlCode" #-}
                                     )) of
                              { !_lhsOpp ->
                              (case ((Syn_Pattern _lhsOcopy _lhsOisUnderscore _lhsOpp)) of
                               { ___node ->
                               ( _lhsOcopy,_lhsOisUnderscore,_lhsOpp) }) }) }) }) }) }) })))
sem_Pattern_Product :: Pos ->
                       T_Patterns ->
                       T_Pattern
sem_Pattern_Product !pos_ !(T_Patterns pats_) =
    (T_Pattern (\ (!_lhsIoptions) ->
                    (case (({-# LINE 43 "src-ag/PrintOcamlCode.ag" #-}
                            _lhsIoptions
                            {-# LINE 1866 "dist/build/PrintOcamlCode" #-}
                            )) of
                     { !_patsOoptions ->
                     (case (pats_ _patsOoptions) of
                      { ( !_patsIcopy,!_patsIpps) ->
                          (case (({-# LINE 22 "src-ag/Patterns.ag" #-}
                                  Product pos_ _patsIcopy
                                  {-# LINE 1873 "dist/build/PrintOcamlCode" #-}
                                  )) of
                           { !_copy ->
                           (case (({-# LINE 22 "src-ag/Patterns.ag" #-}
                                   _copy
                                   {-# LINE 1878 "dist/build/PrintOcamlCode" #-}
                                   )) of
                            { !_lhsOcopy ->
                            (case (({-# LINE 201 "src-ag/PrintOcamlCode.ag" #-}
                                    False
                                    {-# LINE 1883 "dist/build/PrintOcamlCode" #-}
                                    )) of
                             { !_lhsOisUnderscore ->
                             (case (({-# LINE 191 "src-ag/PrintOcamlCode.ag" #-}
                                     pp_block "(" ")" "," _patsIpps
                                     {-# LINE 1888 "dist/build/PrintOcamlCode" #-}
                                     )) of
                              { !_lhsOpp ->
                              (case ((Syn_Pattern _lhsOcopy _lhsOisUnderscore _lhsOpp)) of
                               { ___node ->
                               ( _lhsOcopy,_lhsOisUnderscore,_lhsOpp) }) }) }) }) }) }) })))
sem_Pattern_Underscore :: Pos ->
                          T_Pattern
sem_Pattern_Underscore !pos_ =
    (T_Pattern (\ (!_lhsIoptions) ->
                    (case (({-# LINE 22 "src-ag/Patterns.ag" #-}
                            Underscore pos_
                            {-# LINE 1900 "dist/build/PrintOcamlCode" #-}
                            )) of
                     { !_copy ->
                     (case (({-# LINE 22 "src-ag/Patterns.ag" #-}
                             _copy
                             {-# LINE 1905 "dist/build/PrintOcamlCode" #-}
                             )) of
                      { !_lhsOcopy ->
                      (case (({-# LINE 203 "src-ag/PrintOcamlCode.ag" #-}
                              True
                              {-# LINE 1910 "dist/build/PrintOcamlCode" #-}
                              )) of
                       { !_lhsOisUnderscore ->
                       (case (({-# LINE 197 "src-ag/PrintOcamlCode.ag" #-}
                               text "_"
                               {-# LINE 1915 "dist/build/PrintOcamlCode" #-}
                               )) of
                        { !_lhsOpp ->
                        (case ((Syn_Pattern _lhsOcopy _lhsOisUnderscore _lhsOpp)) of
                         { ___node ->
                         ( _lhsOcopy,_lhsOisUnderscore,_lhsOpp) }) }) }) }) })))
-- Patterns ----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         options              : Options
      synthesized attributes:
         copy                 : Patterns 
         pps                  : PP_Docs
   alternatives:
      alternative Cons:
         child hd             : Pattern 
         child tl             : Patterns 
         visit 0:
            local copy        : _
      alternative Nil:
         visit 0:
            local copy        : _
-}
-- cata
sem_Patterns :: Patterns ->
                T_Patterns
sem_Patterns !list =
    (Prelude.foldr sem_Patterns_Cons sem_Patterns_Nil (Prelude.map sem_Pattern list))
-- semantic domain
newtype T_Patterns = T_Patterns (Options ->
                                 ( Patterns,PP_Docs))
data Inh_Patterns = Inh_Patterns {options_Inh_Patterns :: !(Options)}
data Syn_Patterns = Syn_Patterns {copy_Syn_Patterns :: !(Patterns),pps_Syn_Patterns :: !(PP_Docs)}
wrap_Patterns :: T_Patterns ->
                 Inh_Patterns ->
                 Syn_Patterns
wrap_Patterns !(T_Patterns sem) !(Inh_Patterns _lhsIoptions) =
    (let ( !_lhsOcopy,!_lhsOpps) = sem _lhsIoptions
     in  (Syn_Patterns _lhsOcopy _lhsOpps))
sem_Patterns_Cons :: T_Pattern ->
                     T_Patterns ->
                     T_Patterns
sem_Patterns_Cons !(T_Pattern hd_) !(T_Patterns tl_) =
    (T_Patterns (\ (!_lhsIoptions) ->
                     (case (({-# LINE 43 "src-ag/PrintOcamlCode.ag" #-}
                             _lhsIoptions
                             {-# LINE 1962 "dist/build/PrintOcamlCode" #-}
                             )) of
                      { !_tlOoptions ->
                      (case (tl_ _tlOoptions) of
                       { ( !_tlIcopy,!_tlIpps) ->
                           (case (({-# LINE 43 "src-ag/PrintOcamlCode.ag" #-}
                                   _lhsIoptions
                                   {-# LINE 1969 "dist/build/PrintOcamlCode" #-}
                                   )) of
                            { !_hdOoptions ->
                            (case (hd_ _hdOoptions) of
                             { ( !_hdIcopy,!_hdIisUnderscore,!_hdIpp) ->
                                 (case (({-# LINE 22 "src-ag/Patterns.ag" #-}
                                         (:) _hdIcopy _tlIcopy
                                         {-# LINE 1976 "dist/build/PrintOcamlCode" #-}
                                         )) of
                                  { !_copy ->
                                  (case (({-# LINE 22 "src-ag/Patterns.ag" #-}
                                          _copy
                                          {-# LINE 1981 "dist/build/PrintOcamlCode" #-}
                                          )) of
                                   { !_lhsOcopy ->
                                   (case (({-# LINE 88 "src-ag/PrintOcamlCode.ag" #-}
                                           _hdIpp : _tlIpps
                                           {-# LINE 1986 "dist/build/PrintOcamlCode" #-}
                                           )) of
                                    { !_lhsOpps ->
                                    (case ((Syn_Patterns _lhsOcopy _lhsOpps)) of
                                     { ___node ->
                                     ( _lhsOcopy,_lhsOpps) }) }) }) }) }) }) }) })))
sem_Patterns_Nil :: T_Patterns
sem_Patterns_Nil =
    (T_Patterns (\ (!_lhsIoptions) ->
                     (case (({-# LINE 22 "src-ag/Patterns.ag" #-}
                             []
                             {-# LINE 1997 "dist/build/PrintOcamlCode" #-}
                             )) of
                      { !_copy ->
                      (case (({-# LINE 22 "src-ag/Patterns.ag" #-}
                              _copy
                              {-# LINE 2002 "dist/build/PrintOcamlCode" #-}
                              )) of
                       { !_lhsOcopy ->
                       (case (({-# LINE 89 "src-ag/PrintOcamlCode.ag" #-}
                               []
                               {-# LINE 2007 "dist/build/PrintOcamlCode" #-}
                               )) of
                        { !_lhsOpps ->
                        (case ((Syn_Patterns _lhsOcopy _lhsOpps)) of
                         { ___node ->
                         ( _lhsOcopy,_lhsOpps) }) }) }) })))
-- Program -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         options              : Options
         textBlockMap         : Map BlockInfo PP_Doc
      synthesized attribute:
         output               : PP_Docs
   alternatives:
      alternative Program:
         child chunks         : Chunks 
         child ordered        : {Bool}
-}
-- cata
sem_Program :: Program ->
               T_Program
sem_Program !(Program _chunks _ordered) =
    (sem_Program_Program (sem_Chunks _chunks) _ordered)
-- semantic domain
newtype T_Program = T_Program (Options ->
                               (Map BlockInfo PP_Doc) ->
                               ( PP_Docs))
data Inh_Program = Inh_Program {options_Inh_Program :: !(Options),textBlockMap_Inh_Program :: !((Map BlockInfo PP_Doc))}
data Syn_Program = Syn_Program {output_Syn_Program :: !(PP_Docs)}
wrap_Program :: T_Program ->
                Inh_Program ->
                Syn_Program
wrap_Program !(T_Program sem) !(Inh_Program _lhsIoptions _lhsItextBlockMap) =
    (let ( !_lhsOoutput) = sem _lhsIoptions _lhsItextBlockMap
     in  (Syn_Program _lhsOoutput))
sem_Program_Program :: T_Chunks ->
                       Bool ->
                       T_Program
sem_Program_Program !(T_Chunks chunks_) !ordered_ =
    (T_Program (\ (!_lhsIoptions)
                  (!_lhsItextBlockMap) ->
                    (case (({-# LINE 45 "src-ag/PrintOcamlCode.ag" #-}
                            _lhsItextBlockMap
                            {-# LINE 2051 "dist/build/PrintOcamlCode" #-}
                            )) of
                     { !_chunksOtextBlockMap ->
                     (case (({-# LINE 212 "src-ag/PrintOcamlCode.ag" #-}
                             True
                             {-# LINE 2056 "dist/build/PrintOcamlCode" #-}
                             )) of
                      { !_chunksOisToplevel ->
                      (case (({-# LINE 43 "src-ag/PrintOcamlCode.ag" #-}
                              _lhsIoptions
                              {-# LINE 2061 "dist/build/PrintOcamlCode" #-}
                              )) of
                       { !_chunksOoptions ->
                       (case (chunks_ _chunksOisToplevel _chunksOoptions _chunksOtextBlockMap) of
                        { ( !_chunksIpps) ->
                            (case (({-# LINE 57 "src-ag/PrintOcamlCode.ag" #-}
                                    _chunksIpps
                                    {-# LINE 2068 "dist/build/PrintOcamlCode" #-}
                                    )) of
                             { !_lhsOoutput ->
                             (case ((Syn_Program _lhsOoutput)) of
                              { ___node ->
                              ( _lhsOoutput) }) }) }) }) }) })))
-- Type --------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative Arr:
         child left           : Type 
         child right          : Type 
      alternative CtxApp:
         child left           : {[(String, [String])]}
         child right          : Type 
      alternative List:
         child tp             : Type 
      alternative NontermType:
         child name           : {String}
         child params         : {[String]}
         child deforested     : {Bool}
      alternative QuantApp:
         child left           : {String}
         child right          : Type 
      alternative SimpleType:
         child txt            : {String}
      alternative TEither:
         child left           : Type 
         child right          : Type 
      alternative TIntMap:
         child value          : Type 
      alternative TMap:
         child key            : Type 
         child value          : Type 
      alternative TMaybe:
         child tp             : Type 
      alternative TupleType:
         child tps            : Types 
      alternative TypeApp:
         child func           : Type 
         child args           : Types 
      alternative UnboxedTupleType:
         child tps            : Types 
-}
-- cata
sem_Type :: Type ->
            T_Type
sem_Type !(Arr _left _right) =
    (sem_Type_Arr (sem_Type _left) (sem_Type _right))
sem_Type !(CtxApp _left _right) =
    (sem_Type_CtxApp _left (sem_Type _right))
sem_Type !(List _tp) =
    (sem_Type_List (sem_Type _tp))
sem_Type !(NontermType _name _params _deforested) =
    (sem_Type_NontermType _name _params _deforested)
sem_Type !(QuantApp _left _right) =
    (sem_Type_QuantApp _left (sem_Type _right))
sem_Type !(SimpleType _txt) =
    (sem_Type_SimpleType _txt)
sem_Type !(TEither _left _right) =
    (sem_Type_TEither (sem_Type _left) (sem_Type _right))
sem_Type !(TIntMap _value) =
    (sem_Type_TIntMap (sem_Type _value))
sem_Type !(TMap _key _value) =
    (sem_Type_TMap (sem_Type _key) (sem_Type _value))
sem_Type !(TMaybe _tp) =
    (sem_Type_TMaybe (sem_Type _tp))
sem_Type !(TupleType _tps) =
    (sem_Type_TupleType (sem_Types _tps))
sem_Type !(TypeApp _func _args) =
    (sem_Type_TypeApp (sem_Type _func) (sem_Types _args))
sem_Type !(UnboxedTupleType _tps) =
    (sem_Type_UnboxedTupleType (sem_Types _tps))
-- semantic domain
newtype T_Type = T_Type (( PP_Doc))
data Inh_Type = Inh_Type {}
data Syn_Type = Syn_Type {pp_Syn_Type :: !(PP_Doc)}
wrap_Type :: T_Type ->
             Inh_Type ->
             Syn_Type
wrap_Type !(T_Type sem) !(Inh_Type) =
    (let ( !_lhsOpp) = sem
     in  (Syn_Type _lhsOpp))
sem_Type_Arr :: T_Type ->
                T_Type ->
                T_Type
sem_Type_Arr !(T_Type left_) !(T_Type right_) =
    (T_Type (case (right_) of
             { ( !_rightIpp) ->
                 (case (left_) of
                  { ( !_leftIpp) ->
                      (case (({-# LINE 160 "src-ag/PrintOcamlCode.ag" #-}
                              pp_parens (_leftIpp >#< "->" >#< _rightIpp)
                              {-# LINE 2164 "dist/build/PrintOcamlCode" #-}
                              )) of
                       { !_lhsOpp ->
                       (case ((Syn_Type _lhsOpp)) of
                        { ___node ->
                        ( _lhsOpp) }) }) }) }))
sem_Type_CtxApp :: ([(String, [String])]) ->
                   T_Type ->
                   T_Type
sem_Type_CtxApp !left_ !(T_Type right_) =
    (T_Type (case (({-# LINE 161 "src-ag/PrintOcamlCode.ag" #-}
                    error "pp of Type.CtxApp not supported"
                    {-# LINE 2176 "dist/build/PrintOcamlCode" #-}
                    )) of
             { !_lhsOpp ->
             (case ((Syn_Type _lhsOpp)) of
              { ___node ->
              ( _lhsOpp) }) }))
sem_Type_List :: T_Type ->
                 T_Type
sem_Type_List !(T_Type tp_) =
    (T_Type (case (tp_) of
             { ( !_tpIpp) ->
                 (case (({-# LINE 166 "src-ag/PrintOcamlCode.ag" #-}
                         _tpIpp >#< "list"
                         {-# LINE 2189 "dist/build/PrintOcamlCode" #-}
                         )) of
                  { !_lhsOpp ->
                  (case ((Syn_Type _lhsOpp)) of
                   { ___node ->
                   ( _lhsOpp) }) }) }))
sem_Type_NontermType :: String ->
                        ([String]) ->
                        Bool ->
                        T_Type
sem_Type_NontermType !name_ !params_ !deforested_ =
    (T_Type (case (({-# LINE 168 "src-ag/PrintOcamlCode.ag" #-}
                    pp_block "(" ")" " " (map text params_ ++ [text $ toOcamlTC name_])
                    {-# LINE 2202 "dist/build/PrintOcamlCode" #-}
                    )) of
             { !_lhsOpp ->
             (case ((Syn_Type _lhsOpp)) of
              { ___node ->
              ( _lhsOpp) }) }))
sem_Type_QuantApp :: String ->
                     T_Type ->
                     T_Type
sem_Type_QuantApp !left_ !(T_Type right_) =
    (T_Type (case (right_) of
             { ( !_rightIpp) ->
                 (case (({-# LINE 53 "src-ag/PrintOcamlCode.ag" #-}
                         _rightIpp
                         {-# LINE 2216 "dist/build/PrintOcamlCode" #-}
                         )) of
                  { !_lhsOpp ->
                  (case ((Syn_Type _lhsOpp)) of
                   { ___node ->
                   ( _lhsOpp) }) }) }))
sem_Type_SimpleType :: String ->
                       T_Type
sem_Type_SimpleType !txt_ =
    (T_Type (case (({-# LINE 167 "src-ag/PrintOcamlCode.ag" #-}
                    text txt_
                    {-# LINE 2227 "dist/build/PrintOcamlCode" #-}
                    )) of
             { !_lhsOpp ->
             (case ((Syn_Type _lhsOpp)) of
              { ___node ->
              ( _lhsOpp) }) }))
sem_Type_TEither :: T_Type ->
                    T_Type ->
                    T_Type
sem_Type_TEither !(T_Type left_) !(T_Type right_) =
    (T_Type (case (({-# LINE 170 "src-ag/PrintOcamlCode.ag" #-}
                    error "pp of Type.TEither is not supported"
                    {-# LINE 2239 "dist/build/PrintOcamlCode" #-}
                    )) of
             { !_lhsOpp ->
             (case ((Syn_Type _lhsOpp)) of
              { ___node ->
              ( _lhsOpp) }) }))
sem_Type_TIntMap :: T_Type ->
                    T_Type
sem_Type_TIntMap !(T_Type value_) =
    (T_Type (case (({-# LINE 172 "src-ag/PrintOcamlCode.ag" #-}
                    error "pp of Type.TIntMap is not supported"
                    {-# LINE 2250 "dist/build/PrintOcamlCode" #-}
                    )) of
             { !_lhsOpp ->
             (case ((Syn_Type _lhsOpp)) of
              { ___node ->
              ( _lhsOpp) }) }))
sem_Type_TMap :: T_Type ->
                 T_Type ->
                 T_Type
sem_Type_TMap !(T_Type key_) !(T_Type value_) =
    (T_Type (case (({-# LINE 171 "src-ag/PrintOcamlCode.ag" #-}
                    error "pp of Type.TMap is not supported"
                    {-# LINE 2262 "dist/build/PrintOcamlCode" #-}
                    )) of
             { !_lhsOpp ->
             (case ((Syn_Type _lhsOpp)) of
              { ___node ->
              ( _lhsOpp) }) }))
sem_Type_TMaybe :: T_Type ->
                   T_Type
sem_Type_TMaybe !(T_Type tp_) =
    (T_Type (case (tp_) of
             { ( !_tpIpp) ->
                 (case (({-# LINE 169 "src-ag/PrintOcamlCode.ag" #-}
                         _tpIpp >#< "opt"
                         {-# LINE 2275 "dist/build/PrintOcamlCode" #-}
                         )) of
                  { !_lhsOpp ->
                  (case ((Syn_Type _lhsOpp)) of
                   { ___node ->
                   ( _lhsOpp) }) }) }))
sem_Type_TupleType :: T_Types ->
                      T_Type
sem_Type_TupleType !(T_Types tps_) =
    (T_Type (case (tps_) of
             { ( !_tpsIpps) ->
                 (case (({-# LINE 163 "src-ag/PrintOcamlCode.ag" #-}
                         pp_block "(" ")" "," _tpsIpps
                         {-# LINE 2288 "dist/build/PrintOcamlCode" #-}
                         )) of
                  { !_lhsOpp ->
                  (case ((Syn_Type _lhsOpp)) of
                   { ___node ->
                   ( _lhsOpp) }) }) }))
sem_Type_TypeApp :: T_Type ->
                    T_Types ->
                    T_Type
sem_Type_TypeApp !(T_Type func_) !(T_Types args_) =
    (T_Type (case (args_) of
             { ( !_argsIpps) ->
                 (case (func_) of
                  { ( !_funcIpp) ->
                      (case (({-# LINE 162 "src-ag/PrintOcamlCode.ag" #-}
                              pp_parens (hv_sp (_argsIpps ++ [_funcIpp]))
                              {-# LINE 2304 "dist/build/PrintOcamlCode" #-}
                              )) of
                       { !_lhsOpp ->
                       (case ((Syn_Type _lhsOpp)) of
                        { ___node ->
                        ( _lhsOpp) }) }) }) }))
sem_Type_UnboxedTupleType :: T_Types ->
                             T_Type
sem_Type_UnboxedTupleType !(T_Types tps_) =
    (T_Type (case (({-# LINE 165 "src-ag/PrintOcamlCode.ag" #-}
                    error "pp of Type.UnboxedTupleType is not supported"
                    {-# LINE 2315 "dist/build/PrintOcamlCode" #-}
                    )) of
             { !_lhsOpp ->
             (case ((Syn_Type _lhsOpp)) of
              { ___node ->
              ( _lhsOpp) }) }))
-- Types -------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         pps                  : PP_Docs
   alternatives:
      alternative Cons:
         child hd             : Type 
         child tl             : Types 
      alternative Nil:
-}
-- cata
sem_Types :: Types ->
             T_Types
sem_Types !list =
    (Prelude.foldr sem_Types_Cons sem_Types_Nil (Prelude.map sem_Type list))
-- semantic domain
newtype T_Types = T_Types (( PP_Docs))
data Inh_Types = Inh_Types {}
data Syn_Types = Syn_Types {pps_Syn_Types :: !(PP_Docs)}
wrap_Types :: T_Types ->
              Inh_Types ->
              Syn_Types
wrap_Types !(T_Types sem) !(Inh_Types) =
    (let ( !_lhsOpps) = sem
     in  (Syn_Types _lhsOpps))
sem_Types_Cons :: T_Type ->
                  T_Types ->
                  T_Types
sem_Types_Cons !(T_Type hd_) !(T_Types tl_) =
    (T_Types (case (tl_) of
              { ( !_tlIpps) ->
                  (case (hd_) of
                   { ( !_hdIpp) ->
                       (case (({-# LINE 72 "src-ag/PrintOcamlCode.ag" #-}
                               _hdIpp : _tlIpps
                               {-# LINE 2357 "dist/build/PrintOcamlCode" #-}
                               )) of
                        { !_lhsOpps ->
                        (case ((Syn_Types _lhsOpps)) of
                         { ___node ->
                         ( _lhsOpps) }) }) }) }))
sem_Types_Nil :: T_Types
sem_Types_Nil =
    (T_Types (case (({-# LINE 73 "src-ag/PrintOcamlCode.ag" #-}
                     []
                     {-# LINE 2367 "dist/build/PrintOcamlCode" #-}
                     )) of
              { !_lhsOpps ->
              (case ((Syn_Types _lhsOpps)) of
               { ___node ->
               ( _lhsOpps) }) }))