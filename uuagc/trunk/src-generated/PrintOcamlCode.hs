{-# OPTIONS_GHC -XBangPatterns #-}

-- UUAGC 0.9.42.2 (src-ag/PrintOcamlCode.ag)
module PrintOcamlCode where
{-# LINE 10 "./src-ag/PrintOcamlCode.ag" #-}

import Pretty
import Code
import Patterns
import Options
import CommonTypes hiding (List,Type,Map,Maybe,IntMap,Either)
import Data.List(intersperse,intercalate)
import Data.Char(toLower)
{-# LINE 15 "dist/build/PrintOcamlCode.hs" #-}

{-# LINE 2 "./src-ag/Code.ag" #-}

import Patterns
import Data.Set(Set)
import qualified Data.Set as Set
import Data.Map(Map)
import qualified Data.Map as Map
{-# LINE 24 "dist/build/PrintOcamlCode.hs" #-}

{-# LINE 2 "./src-ag/Patterns.ag" #-}

-- Patterns.ag imports
import UU.Scanner.Position(Pos)
import CommonTypes (ConstructorIdent,Identifier)
{-# LINE 31 "dist/build/PrintOcamlCode.hs" #-}
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
{-# LINE 49 "dist/build/PrintOcamlCode.hs" #-}

{-# LINE 175 "./src-ag/PrintOcamlCode.ag" #-}

toOcamlTC :: String -> String
toOcamlTC (c:cs) = toLower c : cs
toOcamlTC xs = xs
{-# LINE 56 "dist/build/PrintOcamlCode.hs" #-}

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
                    (case (({-# LINE 44 "./src-ag/PrintOcamlCode.ag" #-}
                            _lhsIoptions
                            {-# LINE 111 "dist/build/PrintOcamlCode.hs" #-}
                            )) of
                     { !_exprOoptions ->
                     (case (expr_ _exprOoptions) of
                      { ( !_exprIpp) ->
                          (case (({-# LINE 44 "./src-ag/PrintOcamlCode.ag" #-}
                                  _lhsIoptions
                                  {-# LINE 118 "dist/build/PrintOcamlCode.hs" #-}
                                  )) of
                           { !_leftOoptions ->
                           (case (left_ _leftOoptions) of
                            { ( !_leftIpp) ->
                                (case (({-# LINE 182 "./src-ag/PrintOcamlCode.ag" #-}
                                        _leftIpp >#< "->" >#< _exprIpp
                                        {-# LINE 125 "dist/build/PrintOcamlCode.hs" #-}
                                        )) of
                                 { !_lhsOpp ->
                                 ( _lhsOpp) }) }) }) }) })))
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
                     (case (({-# LINE 44 "./src-ag/PrintOcamlCode.ag" #-}
                             _lhsIoptions
                             {-# LINE 165 "dist/build/PrintOcamlCode.hs" #-}
                             )) of
                      { !_tlOoptions ->
                      (case (tl_ _tlOoptions) of
                       { ( !_tlIpps) ->
                           (case (({-# LINE 44 "./src-ag/PrintOcamlCode.ag" #-}
                                   _lhsIoptions
                                   {-# LINE 172 "dist/build/PrintOcamlCode.hs" #-}
                                   )) of
                            { !_hdOoptions ->
                            (case (hd_ _hdOoptions) of
                             { ( !_hdIpp) ->
                                 (case (({-# LINE 65 "./src-ag/PrintOcamlCode.ag" #-}
                                         _hdIpp : _tlIpps
                                         {-# LINE 179 "dist/build/PrintOcamlCode.hs" #-}
                                         )) of
                                  { !_lhsOpps ->
                                  ( _lhsOpps) }) }) }) }) })))
sem_CaseAlts_Nil :: T_CaseAlts
sem_CaseAlts_Nil =
    (T_CaseAlts (\ (!_lhsIoptions) ->
                     (case (({-# LINE 66 "./src-ag/PrintOcamlCode.ag" #-}
                             []
                             {-# LINE 188 "dist/build/PrintOcamlCode.hs" #-}
                             )) of
                      { !_lhsOpps ->
                      ( _lhsOpps) })))
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
                  (case (({-# LINE 211 "./src-ag/PrintOcamlCode.ag" #-}
                          _lhsIisToplevel
                          {-# LINE 247 "dist/build/PrintOcamlCode.hs" #-}
                          )) of
                   { !_semFunctionsOisToplevel ->
                   (case (({-# LINE 211 "./src-ag/PrintOcamlCode.ag" #-}
                           _lhsIisToplevel
                           {-# LINE 252 "dist/build/PrintOcamlCode.hs" #-}
                           )) of
                    { !_semWrapperOisToplevel ->
                    (case (({-# LINE 211 "./src-ag/PrintOcamlCode.ag" #-}
                            _lhsIisToplevel
                            {-# LINE 257 "dist/build/PrintOcamlCode.hs" #-}
                            )) of
                     { !_semDomOisToplevel ->
                     (case (({-# LINE 211 "./src-ag/PrintOcamlCode.ag" #-}
                             _lhsIisToplevel
                             {-# LINE 262 "dist/build/PrintOcamlCode.hs" #-}
                             )) of
                      { !_cataFunOisToplevel ->
                      (case (({-# LINE 211 "./src-ag/PrintOcamlCode.ag" #-}
                              _lhsIisToplevel
                              {-# LINE 267 "dist/build/PrintOcamlCode.hs" #-}
                              )) of
                       { !_dataDefOisToplevel ->
                       (case (({-# LINE 211 "./src-ag/PrintOcamlCode.ag" #-}
                               _lhsIisToplevel
                               {-# LINE 272 "dist/build/PrintOcamlCode.hs" #-}
                               )) of
                        { !_infoOisToplevel ->
                        (case (({-# LINE 211 "./src-ag/PrintOcamlCode.ag" #-}
                                _lhsIisToplevel
                                {-# LINE 277 "dist/build/PrintOcamlCode.hs" #-}
                                )) of
                         { !_commentOisToplevel ->
                         (case (({-# LINE 44 "./src-ag/PrintOcamlCode.ag" #-}
                                 _lhsIoptions
                                 {-# LINE 282 "dist/build/PrintOcamlCode.hs" #-}
                                 )) of
                          { !_semFunctionsOoptions ->
                          (case (semFunctions_ _semFunctionsOisToplevel _semFunctionsOoptions) of
                           { ( !_semFunctionsIpps) ->
                               (case (({-# LINE 44 "./src-ag/PrintOcamlCode.ag" #-}
                                       _lhsIoptions
                                       {-# LINE 289 "dist/build/PrintOcamlCode.hs" #-}
                                       )) of
                                { !_semWrapperOoptions ->
                                (case (semWrapper_ _semWrapperOisToplevel _semWrapperOoptions) of
                                 { ( !_semWrapperIpps) ->
                                     (case (({-# LINE 44 "./src-ag/PrintOcamlCode.ag" #-}
                                             _lhsIoptions
                                             {-# LINE 296 "dist/build/PrintOcamlCode.hs" #-}
                                             )) of
                                      { !_semDomOoptions ->
                                      (case (semDom_ _semDomOisToplevel _semDomOoptions) of
                                       { ( !_semDomIpps) ->
                                           (case (({-# LINE 44 "./src-ag/PrintOcamlCode.ag" #-}
                                                   _lhsIoptions
                                                   {-# LINE 303 "dist/build/PrintOcamlCode.hs" #-}
                                                   )) of
                                            { !_cataFunOoptions ->
                                            (case (cataFun_ _cataFunOisToplevel _cataFunOoptions) of
                                             { ( !_cataFunIpps) ->
                                                 (case (({-# LINE 44 "./src-ag/PrintOcamlCode.ag" #-}
                                                         _lhsIoptions
                                                         {-# LINE 310 "dist/build/PrintOcamlCode.hs" #-}
                                                         )) of
                                                  { !_dataDefOoptions ->
                                                  (case (dataDef_ _dataDefOisToplevel _dataDefOoptions) of
                                                   { ( !_dataDefIpps) ->
                                                       (case (({-# LINE 44 "./src-ag/PrintOcamlCode.ag" #-}
                                                               _lhsIoptions
                                                               {-# LINE 317 "dist/build/PrintOcamlCode.hs" #-}
                                                               )) of
                                                        { !_infoOoptions ->
                                                        (case (info_ _infoOisToplevel _infoOoptions) of
                                                         { ( !_infoIpps) ->
                                                             (case (({-# LINE 44 "./src-ag/PrintOcamlCode.ag" #-}
                                                                     _lhsIoptions
                                                                     {-# LINE 324 "dist/build/PrintOcamlCode.hs" #-}
                                                                     )) of
                                                              { !_commentOoptions ->
                                                              (case (comment_ _commentOisToplevel _commentOoptions) of
                                                               { ( !_commentIpp) ->
                                                                   (case (({-# LINE 97 "./src-ag/PrintOcamlCode.ag" #-}
                                                                           _commentIpp
                                                                           :  _infoIpps
                                                                           ++ _dataDefIpps
                                                                           ++ _semDomIpps
                                                                           ++ _semFunctionsIpps
                                                                           ++ _semWrapperIpps
                                                                           ++ _cataFunIpps
                                                                           ++ [Map.findWithDefault empty (BlockOther, Just $ identifier name_) _lhsItextBlockMap]
                                                                           {-# LINE 338 "dist/build/PrintOcamlCode.hs" #-}
                                                                           )) of
                                                                    { !_lhsOpps ->
                                                                    ( _lhsOpps) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })))
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
                   (case (({-# LINE 46 "./src-ag/PrintOcamlCode.ag" #-}
                           _lhsItextBlockMap
                           {-# LINE 384 "dist/build/PrintOcamlCode.hs" #-}
                           )) of
                    { !_tlOtextBlockMap ->
                    (case (({-# LINE 211 "./src-ag/PrintOcamlCode.ag" #-}
                            _lhsIisToplevel
                            {-# LINE 389 "dist/build/PrintOcamlCode.hs" #-}
                            )) of
                     { !_tlOisToplevel ->
                     (case (({-# LINE 46 "./src-ag/PrintOcamlCode.ag" #-}
                             _lhsItextBlockMap
                             {-# LINE 394 "dist/build/PrintOcamlCode.hs" #-}
                             )) of
                      { !_hdOtextBlockMap ->
                      (case (({-# LINE 211 "./src-ag/PrintOcamlCode.ag" #-}
                              _lhsIisToplevel
                              {-# LINE 399 "dist/build/PrintOcamlCode.hs" #-}
                              )) of
                       { !_hdOisToplevel ->
                       (case (({-# LINE 44 "./src-ag/PrintOcamlCode.ag" #-}
                               _lhsIoptions
                               {-# LINE 404 "dist/build/PrintOcamlCode.hs" #-}
                               )) of
                        { !_tlOoptions ->
                        (case (tl_ _tlOisToplevel _tlOoptions _tlOtextBlockMap) of
                         { ( !_tlIpps) ->
                             (case (({-# LINE 44 "./src-ag/PrintOcamlCode.ag" #-}
                                     _lhsIoptions
                                     {-# LINE 411 "dist/build/PrintOcamlCode.hs" #-}
                                     )) of
                              { !_hdOoptions ->
                              (case (hd_ _hdOisToplevel _hdOoptions _hdOtextBlockMap) of
                               { ( !_hdIpps) ->
                                   (case (({-# LINE 85 "./src-ag/PrintOcamlCode.ag" #-}
                                           _hdIpps ++ _tlIpps
                                           {-# LINE 418 "dist/build/PrintOcamlCode.hs" #-}
                                           )) of
                                    { !_lhsOpps ->
                                    ( _lhsOpps) }) }) }) }) }) }) }) }) })))
sem_Chunks_Nil :: T_Chunks
sem_Chunks_Nil =
    (T_Chunks (\ (!_lhsIisToplevel)
                 (!_lhsIoptions)
                 (!_lhsItextBlockMap) ->
                   (case (({-# LINE 86 "./src-ag/PrintOcamlCode.ag" #-}
                           []
                           {-# LINE 429 "dist/build/PrintOcamlCode.hs" #-}
                           )) of
                    { !_lhsOpps ->
                    ( _lhsOpps) })))
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
                    (case (({-# LINE 185 "./src-ag/PrintOcamlCode.ag" #-}
                            name_ >#< "of" >#< pp_block "" "" " * " (map pp_parens _argsIpps)
                            {-# LINE 471 "dist/build/PrintOcamlCode.hs" #-}
                            )) of
                     { !_lhsOpp ->
                     ( _lhsOpp) }) }))
sem_DataAlt_Record :: String ->
                      T_NamedTypes ->
                      T_DataAlt
sem_DataAlt_Record !name_ !(T_NamedTypes args_) =
    (T_DataAlt (case (args_) of
                { ( !_argsIpps) ->
                    (case (({-# LINE 186 "./src-ag/PrintOcamlCode.ag" #-}
                            pp_block "{" "}" ";" _argsIpps
                            {-# LINE 483 "dist/build/PrintOcamlCode.hs" #-}
                            )) of
                     { !_lhsOpp ->
                     ( _lhsOpp) }) }))
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
                          (case (({-# LINE 69 "./src-ag/PrintOcamlCode.ag" #-}
                                  _hdIpp : _tlIpps
                                  {-# LINE 523 "dist/build/PrintOcamlCode.hs" #-}
                                  )) of
                           { !_lhsOpps ->
                           ( _lhsOpps) }) }) }))
sem_DataAlts_Nil :: T_DataAlts
sem_DataAlts_Nil =
    (T_DataAlts (case (({-# LINE 70 "./src-ag/PrintOcamlCode.ag" #-}
                        []
                        {-# LINE 531 "dist/build/PrintOcamlCode.hs" #-}
                        )) of
                 { !_lhsOpps ->
                 ( _lhsOpps) }))
-- Decl --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         isToplevel           : Bool
         options              : Options
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative Decl:
         child left           : Lhs 
         child rhs            : Expr 
         child binds          : {Set String}
         child uses           : {Set String}
      alternative Bind:
         child left           : Lhs 
         child rhs            : Expr 
      alternative BindLet:
         child left           : Lhs 
         child rhs            : Expr 
      alternative Data:
         child name           : {String}
         child params         : {[String]}
         child alts           : DataAlts 
         child strict         : {Bool}
         child derivings      : {[String]}
      alternative NewType:
         child name           : {String}
         child params         : {[String]}
         child con            : {String}
         child tp             : Type 
      alternative Type:
         child name           : {String}
         child params         : {[String]}
         child tp             : Type 
      alternative TSig:
         child name           : {String}
         child tp             : Type 
      alternative Comment:
         child txt            : {String}
      alternative PragmaDecl:
         child txt            : {String}
      alternative Resume:
         child monadic        : {Bool}
         child nt             : {String}
         child left           : Lhs 
         child rhs            : Expr 
      alternative EvalDecl:
         child nt             : {String}
         child left           : Lhs 
         child rhs            : Expr 
-}
-- cata
sem_Decl :: Decl ->
            T_Decl
sem_Decl !(Decl _left _rhs _binds _uses) =
    (sem_Decl_Decl (sem_Lhs _left) (sem_Expr _rhs) _binds _uses)
sem_Decl !(Bind _left _rhs) =
    (sem_Decl_Bind (sem_Lhs _left) (sem_Expr _rhs))
sem_Decl !(BindLet _left _rhs) =
    (sem_Decl_BindLet (sem_Lhs _left) (sem_Expr _rhs))
sem_Decl !(Data _name _params _alts _strict _derivings) =
    (sem_Decl_Data _name _params (sem_DataAlts _alts) _strict _derivings)
sem_Decl !(NewType _name _params _con _tp) =
    (sem_Decl_NewType _name _params _con (sem_Type _tp))
sem_Decl !(Type _name _params _tp) =
    (sem_Decl_Type _name _params (sem_Type _tp))
sem_Decl !(TSig _name _tp) =
    (sem_Decl_TSig _name (sem_Type _tp))
sem_Decl !(Comment _txt) =
    (sem_Decl_Comment _txt)
sem_Decl !(PragmaDecl _txt) =
    (sem_Decl_PragmaDecl _txt)
sem_Decl !(Resume _monadic _nt _left _rhs) =
    (sem_Decl_Resume _monadic _nt (sem_Lhs _left) (sem_Expr _rhs))
sem_Decl !(EvalDecl _nt _left _rhs) =
    (sem_Decl_EvalDecl _nt (sem_Lhs _left) (sem_Expr _rhs))
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
sem_Decl_Decl :: T_Lhs ->
                 T_Expr ->
                 (Set String) ->
                 (Set String) ->
                 T_Decl
sem_Decl_Decl !(T_Lhs left_) !(T_Expr rhs_) !binds_ !uses_ =
    (T_Decl (\ (!_lhsIisToplevel)
               (!_lhsIoptions) ->
                 (case (({-# LINE 44 "./src-ag/PrintOcamlCode.ag" #-}
                         _lhsIoptions
                         {-# LINE 634 "dist/build/PrintOcamlCode.hs" #-}
                         )) of
                  { !_rhsOoptions ->
                  (case (rhs_ _rhsOoptions) of
                   { ( !_rhsIpp) ->
                       (case (({-# LINE 44 "./src-ag/PrintOcamlCode.ag" #-}
                               _lhsIoptions
                               {-# LINE 641 "dist/build/PrintOcamlCode.hs" #-}
                               )) of
                        { !_leftOoptions ->
                        (case (left_ _leftOoptions) of
                         { ( !_leftIpp) ->
                             (case (({-# LINE 107 "./src-ag/PrintOcamlCode.ag" #-}
                                     if _lhsIisToplevel
                                     then "let" >#< _leftIpp >#< "="
                                          >-< indent 4 _rhsIpp >#< ";;"
                                     else "let" >#< _leftIpp >#< "="
                                          >-< indent 4 _rhsIpp >#< "in"
                                     {-# LINE 652 "dist/build/PrintOcamlCode.hs" #-}
                                     )) of
                              { !_lhsOpp ->
                              ( _lhsOpp) }) }) }) }) })))
sem_Decl_Bind :: T_Lhs ->
                 T_Expr ->
                 T_Decl
sem_Decl_Bind !(T_Lhs left_) !(T_Expr rhs_) =
    (T_Decl (\ (!_lhsIisToplevel)
               (!_lhsIoptions) ->
                 (case (({-# LINE 112 "./src-ag/PrintOcamlCode.ag" #-}
                         error "pp of Decl.Bind not supported"
                         {-# LINE 664 "dist/build/PrintOcamlCode.hs" #-}
                         )) of
                  { !_lhsOpp ->
                  ( _lhsOpp) })))
sem_Decl_BindLet :: T_Lhs ->
                    T_Expr ->
                    T_Decl
sem_Decl_BindLet !(T_Lhs left_) !(T_Expr rhs_) =
    (T_Decl (\ (!_lhsIisToplevel)
               (!_lhsIoptions) ->
                 (case (({-# LINE 113 "./src-ag/PrintOcamlCode.ag" #-}
                         error "pp of Decl.BindLet not supported"
                         {-# LINE 676 "dist/build/PrintOcamlCode.hs" #-}
                         )) of
                  { !_lhsOpp ->
                  ( _lhsOpp) })))
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
                      (case (({-# LINE 114 "./src-ag/PrintOcamlCode.ag" #-}
                              "type" >#< hv_sp (map (\p -> "'" >|< p) params_ ++ [text $ toOcamlTC name_])
                              >#<  ( case _altsIpps of
                                           [] -> empty
                                           (x:xs) ->              "=" >#<  x
                                                  >-< vlist (map ("|" >#<) xs)
                                   )
                              >#< ";;"
                              {-# LINE 699 "dist/build/PrintOcamlCode.hs" #-}
                              )) of
                       { !_lhsOpp ->
                       ( _lhsOpp) }) })))
sem_Decl_NewType :: String ->
                    ([String]) ->
                    String ->
                    T_Type ->
                    T_Decl
sem_Decl_NewType !name_ !params_ !con_ !(T_Type tp_) =
    (T_Decl (\ (!_lhsIisToplevel)
               (!_lhsIoptions) ->
                 (case (({-# LINE 121 "./src-ag/PrintOcamlCode.ag" #-}
                         error "pp of Decl.NewType not supported"
                         {-# LINE 713 "dist/build/PrintOcamlCode.hs" #-}
                         )) of
                  { !_lhsOpp ->
                  ( _lhsOpp) })))
sem_Decl_Type :: String ->
                 ([String]) ->
                 T_Type ->
                 T_Decl
sem_Decl_Type !name_ !params_ !(T_Type tp_) =
    (T_Decl (\ (!_lhsIisToplevel)
               (!_lhsIoptions) ->
                 (case (tp_) of
                  { ( !_tpIpp) ->
                      (case (({-# LINE 122 "./src-ag/PrintOcamlCode.ag" #-}
                              "type" >#< hv_sp (map (\p -> "'" >|< p) params_ ++ [text $ toOcamlTC name_]) >#< "=" >#<  _tpIpp >#< ";;"
                              {-# LINE 728 "dist/build/PrintOcamlCode.hs" #-}
                              )) of
                       { !_lhsOpp ->
                       ( _lhsOpp) }) })))
sem_Decl_TSig :: String ->
                 T_Type ->
                 T_Decl
sem_Decl_TSig !name_ !(T_Type tp_) =
    (T_Decl (\ (!_lhsIisToplevel)
               (!_lhsIoptions) ->
                 (case (tp_) of
                  { ( !_tpIpp) ->
                      (case (({-# LINE 123 "./src-ag/PrintOcamlCode.ag" #-}
                              "(*" >#< name_ >#< ":" >#< _tpIpp >#< "*)"
                              {-# LINE 742 "dist/build/PrintOcamlCode.hs" #-}
                              )) of
                       { !_lhsOpp ->
                       ( _lhsOpp) }) })))
sem_Decl_Comment :: String ->
                    T_Decl
sem_Decl_Comment !txt_ =
    (T_Decl (\ (!_lhsIisToplevel)
               (!_lhsIoptions) ->
                 (case (({-# LINE 124 "./src-ag/PrintOcamlCode.ag" #-}
                         if '\n' `elem` txt_
                           then "(* " >-< vlist (lines txt_) >-< "*)"
                           else "(*" >#< txt_ >#< "*)"
                         {-# LINE 755 "dist/build/PrintOcamlCode.hs" #-}
                         )) of
                  { !_lhsOpp ->
                  ( _lhsOpp) })))
sem_Decl_PragmaDecl :: String ->
                       T_Decl
sem_Decl_PragmaDecl !txt_ =
    (T_Decl (\ (!_lhsIisToplevel)
               (!_lhsIoptions) ->
                 (case (({-# LINE 127 "./src-ag/PrintOcamlCode.ag" #-}
                         error "pp of Decl.PragmaDecl not supported"
                         {-# LINE 766 "dist/build/PrintOcamlCode.hs" #-}
                         )) of
                  { !_lhsOpp ->
                  ( _lhsOpp) })))
sem_Decl_Resume :: Bool ->
                   String ->
                   T_Lhs ->
                   T_Expr ->
                   T_Decl
sem_Decl_Resume !monadic_ !nt_ !(T_Lhs left_) !(T_Expr rhs_) =
    (T_Decl (\ (!_lhsIisToplevel)
               (!_lhsIoptions) ->
                 (case (({-# LINE 44 "./src-ag/PrintOcamlCode.ag" #-}
                         _lhsIoptions
                         {-# LINE 780 "dist/build/PrintOcamlCode.hs" #-}
                         )) of
                  { !_rhsOoptions ->
                  (case (rhs_ _rhsOoptions) of
                   { ( !_rhsIpp) ->
                       (case (({-# LINE 54 "./src-ag/PrintOcamlCode.ag" #-}
                               _rhsIpp
                               {-# LINE 787 "dist/build/PrintOcamlCode.hs" #-}
                               )) of
                        { !_lhsOpp ->
                        ( _lhsOpp) }) }) })))
sem_Decl_EvalDecl :: String ->
                     T_Lhs ->
                     T_Expr ->
                     T_Decl
sem_Decl_EvalDecl !nt_ !(T_Lhs left_) !(T_Expr rhs_) =
    (T_Decl (\ (!_lhsIisToplevel)
               (!_lhsIoptions) ->
                 (case (({-# LINE 44 "./src-ag/PrintOcamlCode.ag" #-}
                         _lhsIoptions
                         {-# LINE 800 "dist/build/PrintOcamlCode.hs" #-}
                         )) of
                  { !_rhsOoptions ->
                  (case (rhs_ _rhsOoptions) of
                   { ( !_rhsIpp) ->
                       (case (({-# LINE 54 "./src-ag/PrintOcamlCode.ag" #-}
                               _rhsIpp
                               {-# LINE 807 "dist/build/PrintOcamlCode.hs" #-}
                               )) of
                        { !_lhsOpp ->
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
                  (case (({-# LINE 211 "./src-ag/PrintOcamlCode.ag" #-}
                          _lhsIisToplevel
                          {-# LINE 850 "dist/build/PrintOcamlCode.hs" #-}
                          )) of
                   { !_tlOisToplevel ->
                   (case (({-# LINE 211 "./src-ag/PrintOcamlCode.ag" #-}
                           _lhsIisToplevel
                           {-# LINE 855 "dist/build/PrintOcamlCode.hs" #-}
                           )) of
                    { !_hdOisToplevel ->
                    (case (({-# LINE 44 "./src-ag/PrintOcamlCode.ag" #-}
                            _lhsIoptions
                            {-# LINE 860 "dist/build/PrintOcamlCode.hs" #-}
                            )) of
                     { !_tlOoptions ->
                     (case (tl_ _tlOisToplevel _tlOoptions) of
                      { ( !_tlIpps) ->
                          (case (({-# LINE 44 "./src-ag/PrintOcamlCode.ag" #-}
                                  _lhsIoptions
                                  {-# LINE 867 "dist/build/PrintOcamlCode.hs" #-}
                                  )) of
                           { !_hdOoptions ->
                           (case (hd_ _hdOisToplevel _hdOoptions) of
                            { ( !_hdIpp) ->
                                (case (({-# LINE 81 "./src-ag/PrintOcamlCode.ag" #-}
                                        _hdIpp : _tlIpps
                                        {-# LINE 874 "dist/build/PrintOcamlCode.hs" #-}
                                        )) of
                                 { !_lhsOpps ->
                                 ( _lhsOpps) }) }) }) }) }) }) })))
sem_Decls_Nil :: T_Decls
sem_Decls_Nil =
    (T_Decls (\ (!_lhsIisToplevel)
                (!_lhsIoptions) ->
                  (case (({-# LINE 82 "./src-ag/PrintOcamlCode.ag" #-}
                          []
                          {-# LINE 884 "dist/build/PrintOcamlCode.hs" #-}
                          )) of
                   { !_lhsOpps ->
                   ( _lhsOpps) })))
-- Expr --------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         options              : Options
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative Let:
         child decls          : Decls 
         child body           : Expr 
      alternative Case:
         child expr           : Expr 
         child alts           : CaseAlts 
      alternative Do:
         child stmts          : Decls 
         child body           : Expr 
      alternative Lambda:
         child args           : Exprs 
         child body           : Expr 
      alternative TupleExpr:
         child exprs          : Exprs 
      alternative UnboxedTupleExpr:
         child exprs          : Exprs 
      alternative App:
         child name           : {String}
         child args           : Exprs 
      alternative SimpleExpr:
         child txt            : {String}
      alternative TextExpr:
         child lns            : {[String]}
      alternative Trace:
         child txt            : {String}
         child expr           : Expr 
      alternative PragmaExpr:
         child onLeftSide     : {Bool}
         child onNewLine      : {Bool}
         child txt            : {String}
         child expr           : Expr 
      alternative LineExpr:
         child expr           : Expr 
      alternative TypedExpr:
         child expr           : Expr 
         child tp             : Type 
      alternative ResultExpr:
         child nt             : {String}
         child expr           : Expr 
      alternative InvokeExpr:
         child nt             : {String}
         child expr           : Expr 
         child args           : Exprs 
      alternative ResumeExpr:
         child nt             : {String}
         child expr           : Expr 
         child left           : Lhs 
         child rhs            : Expr 
      alternative SemFun:
         child nt             : {String}
         child args           : Exprs 
         child body           : Expr 
-}
-- cata
sem_Expr :: Expr ->
            T_Expr
sem_Expr !(Let _decls _body) =
    (sem_Expr_Let (sem_Decls _decls) (sem_Expr _body))
sem_Expr !(Case _expr _alts) =
    (sem_Expr_Case (sem_Expr _expr) (sem_CaseAlts _alts))
sem_Expr !(Do _stmts _body) =
    (sem_Expr_Do (sem_Decls _stmts) (sem_Expr _body))
sem_Expr !(Lambda _args _body) =
    (sem_Expr_Lambda (sem_Exprs _args) (sem_Expr _body))
sem_Expr !(TupleExpr _exprs) =
    (sem_Expr_TupleExpr (sem_Exprs _exprs))
sem_Expr !(UnboxedTupleExpr _exprs) =
    (sem_Expr_UnboxedTupleExpr (sem_Exprs _exprs))
sem_Expr !(App _name _args) =
    (sem_Expr_App _name (sem_Exprs _args))
sem_Expr !(SimpleExpr _txt) =
    (sem_Expr_SimpleExpr _txt)
sem_Expr !(TextExpr _lns) =
    (sem_Expr_TextExpr _lns)
sem_Expr !(Trace _txt _expr) =
    (sem_Expr_Trace _txt (sem_Expr _expr))
sem_Expr !(PragmaExpr _onLeftSide _onNewLine _txt _expr) =
    (sem_Expr_PragmaExpr _onLeftSide _onNewLine _txt (sem_Expr _expr))
sem_Expr !(LineExpr _expr) =
    (sem_Expr_LineExpr (sem_Expr _expr))
sem_Expr !(TypedExpr _expr _tp) =
    (sem_Expr_TypedExpr (sem_Expr _expr) (sem_Type _tp))
sem_Expr !(ResultExpr _nt _expr) =
    (sem_Expr_ResultExpr _nt (sem_Expr _expr))
sem_Expr !(InvokeExpr _nt _expr _args) =
    (sem_Expr_InvokeExpr _nt (sem_Expr _expr) (sem_Exprs _args))
sem_Expr !(ResumeExpr _nt _expr _left _rhs) =
    (sem_Expr_ResumeExpr _nt (sem_Expr _expr) (sem_Lhs _left) (sem_Expr _rhs))
sem_Expr !(SemFun _nt _args _body) =
    (sem_Expr_SemFun _nt (sem_Exprs _args) (sem_Expr _body))
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
sem_Expr_Let :: T_Decls ->
                T_Expr ->
                T_Expr
sem_Expr_Let !(T_Decls decls_) !(T_Expr body_) =
    (T_Expr (\ (!_lhsIoptions) ->
                 (case (({-# LINE 218 "./src-ag/PrintOcamlCode.ag" #-}
                         False
                         {-# LINE 1004 "dist/build/PrintOcamlCode.hs" #-}
                         )) of
                  { !_declsOisToplevel ->
                  (case (({-# LINE 44 "./src-ag/PrintOcamlCode.ag" #-}
                          _lhsIoptions
                          {-# LINE 1009 "dist/build/PrintOcamlCode.hs" #-}
                          )) of
                   { !_bodyOoptions ->
                   (case (body_ _bodyOoptions) of
                    { ( !_bodyIpp) ->
                        (case (({-# LINE 44 "./src-ag/PrintOcamlCode.ag" #-}
                                _lhsIoptions
                                {-# LINE 1016 "dist/build/PrintOcamlCode.hs" #-}
                                )) of
                         { !_declsOoptions ->
                         (case (decls_ _declsOisToplevel _declsOoptions) of
                          { ( !_declsIpps) ->
                              (case (({-# LINE 131 "./src-ag/PrintOcamlCode.ag" #-}
                                      pp_parens $ vlist (_declsIpps ++ [_bodyIpp])
                                      {-# LINE 1023 "dist/build/PrintOcamlCode.hs" #-}
                                      )) of
                               { !_lhsOpp ->
                               ( _lhsOpp) }) }) }) }) }) })))
sem_Expr_Case :: T_Expr ->
                 T_CaseAlts ->
                 T_Expr
sem_Expr_Case !(T_Expr expr_) !(T_CaseAlts alts_) =
    (T_Expr (\ (!_lhsIoptions) ->
                 (case (({-# LINE 44 "./src-ag/PrintOcamlCode.ag" #-}
                         _lhsIoptions
                         {-# LINE 1034 "dist/build/PrintOcamlCode.hs" #-}
                         )) of
                  { !_altsOoptions ->
                  (case (alts_ _altsOoptions) of
                   { ( !_altsIpps) ->
                       (case (({-# LINE 44 "./src-ag/PrintOcamlCode.ag" #-}
                               _lhsIoptions
                               {-# LINE 1041 "dist/build/PrintOcamlCode.hs" #-}
                               )) of
                        { !_exprOoptions ->
                        (case (expr_ _exprOoptions) of
                         { ( !_exprIpp) ->
                             (case (({-# LINE 132 "./src-ag/PrintOcamlCode.ag" #-}
                                     pp_parens ( "match" >#< _exprIpp >#< "with"
                                               >-< indent 2 ( case _altsIpps of
                                                                [] -> empty
                                                                (x:xs) -> " " >#<  x
                                                                          >-< vlist (map ("|" >#<) xs)
                                                            )
                                               )
                                     {-# LINE 1054 "dist/build/PrintOcamlCode.hs" #-}
                                     )) of
                              { !_lhsOpp ->
                              ( _lhsOpp) }) }) }) }) })))
sem_Expr_Do :: T_Decls ->
               T_Expr ->
               T_Expr
sem_Expr_Do !(T_Decls stmts_) !(T_Expr body_) =
    (T_Expr (\ (!_lhsIoptions) ->
                 (case (({-# LINE 139 "./src-ag/PrintOcamlCode.ag" #-}
                         error "pp of Expr.Do not supported"
                         {-# LINE 1065 "dist/build/PrintOcamlCode.hs" #-}
                         )) of
                  { !_lhsOpp ->
                  ( _lhsOpp) })))
sem_Expr_Lambda :: T_Exprs ->
                   T_Expr ->
                   T_Expr
sem_Expr_Lambda !(T_Exprs args_) !(T_Expr body_) =
    (T_Expr (\ (!_lhsIoptions) ->
                 (case (({-# LINE 44 "./src-ag/PrintOcamlCode.ag" #-}
                         _lhsIoptions
                         {-# LINE 1076 "dist/build/PrintOcamlCode.hs" #-}
                         )) of
                  { !_bodyOoptions ->
                  (case (body_ _bodyOoptions) of
                   { ( !_bodyIpp) ->
                       (case (({-# LINE 44 "./src-ag/PrintOcamlCode.ag" #-}
                               _lhsIoptions
                               {-# LINE 1083 "dist/build/PrintOcamlCode.hs" #-}
                               )) of
                        { !_argsOoptions ->
                        (case (args_ _argsOoptions) of
                         { ( !_argsIpps) ->
                             (case (({-# LINE 140 "./src-ag/PrintOcamlCode.ag" #-}
                                     pp_parens ( pp "fun" >#< hv_sp _argsIpps >#< "->"
                                               >-< indent 2 _bodyIpp )
                                     {-# LINE 1091 "dist/build/PrintOcamlCode.hs" #-}
                                     )) of
                              { !_lhsOpp ->
                              ( _lhsOpp) }) }) }) }) })))
sem_Expr_TupleExpr :: T_Exprs ->
                      T_Expr
sem_Expr_TupleExpr !(T_Exprs exprs_) =
    (T_Expr (\ (!_lhsIoptions) ->
                 (case (({-# LINE 44 "./src-ag/PrintOcamlCode.ag" #-}
                         _lhsIoptions
                         {-# LINE 1101 "dist/build/PrintOcamlCode.hs" #-}
                         )) of
                  { !_exprsOoptions ->
                  (case (exprs_ _exprsOoptions) of
                   { ( !_exprsIpps) ->
                       (case (({-# LINE 142 "./src-ag/PrintOcamlCode.ag" #-}
                               ppTuple False _exprsIpps
                               {-# LINE 1108 "dist/build/PrintOcamlCode.hs" #-}
                               )) of
                        { !_lhsOpp ->
                        ( _lhsOpp) }) }) })))
sem_Expr_UnboxedTupleExpr :: T_Exprs ->
                             T_Expr
sem_Expr_UnboxedTupleExpr !(T_Exprs exprs_) =
    (T_Expr (\ (!_lhsIoptions) ->
                 (case (({-# LINE 143 "./src-ag/PrintOcamlCode.ag" #-}
                         error "pp of Expr.UnboxedTupleExpr not supported"
                         {-# LINE 1118 "dist/build/PrintOcamlCode.hs" #-}
                         )) of
                  { !_lhsOpp ->
                  ( _lhsOpp) })))
sem_Expr_App :: String ->
                T_Exprs ->
                T_Expr
sem_Expr_App !name_ !(T_Exprs args_) =
    (T_Expr (\ (!_lhsIoptions) ->
                 (case (({-# LINE 44 "./src-ag/PrintOcamlCode.ag" #-}
                         _lhsIoptions
                         {-# LINE 1129 "dist/build/PrintOcamlCode.hs" #-}
                         )) of
                  { !_argsOoptions ->
                  (case (args_ _argsOoptions) of
                   { ( !_argsIpps) ->
                       (case (({-# LINE 144 "./src-ag/PrintOcamlCode.ag" #-}
                               pp_parens $ name_ >#< hv_sp _argsIpps
                               {-# LINE 1136 "dist/build/PrintOcamlCode.hs" #-}
                               )) of
                        { !_lhsOpp ->
                        ( _lhsOpp) }) }) })))
sem_Expr_SimpleExpr :: String ->
                       T_Expr
sem_Expr_SimpleExpr !txt_ =
    (T_Expr (\ (!_lhsIoptions) ->
                 (case (({-# LINE 145 "./src-ag/PrintOcamlCode.ag" #-}
                         text txt_
                         {-# LINE 1146 "dist/build/PrintOcamlCode.hs" #-}
                         )) of
                  { !_lhsOpp ->
                  ( _lhsOpp) })))
sem_Expr_TextExpr :: ([String]) ->
                     T_Expr
sem_Expr_TextExpr !lns_ =
    (T_Expr (\ (!_lhsIoptions) ->
                 (case (({-# LINE 146 "./src-ag/PrintOcamlCode.ag" #-}
                         vlist (map text lns_)
                         {-# LINE 1156 "dist/build/PrintOcamlCode.hs" #-}
                         )) of
                  { !_lhsOpp ->
                  ( _lhsOpp) })))
sem_Expr_Trace :: String ->
                  T_Expr ->
                  T_Expr
sem_Expr_Trace !txt_ !(T_Expr expr_) =
    (T_Expr (\ (!_lhsIoptions) ->
                 (case (({-# LINE 44 "./src-ag/PrintOcamlCode.ag" #-}
                         _lhsIoptions
                         {-# LINE 1167 "dist/build/PrintOcamlCode.hs" #-}
                         )) of
                  { !_exprOoptions ->
                  (case (expr_ _exprOoptions) of
                   { ( !_exprIpp) ->
                       (case (({-# LINE 147 "./src-ag/PrintOcamlCode.ag" #-}
                               _exprIpp
                               {-# LINE 1174 "dist/build/PrintOcamlCode.hs" #-}
                               )) of
                        { !_lhsOpp ->
                        ( _lhsOpp) }) }) })))
sem_Expr_PragmaExpr :: Bool ->
                       Bool ->
                       String ->
                       T_Expr ->
                       T_Expr
sem_Expr_PragmaExpr !onLeftSide_ !onNewLine_ !txt_ !(T_Expr expr_) =
    (T_Expr (\ (!_lhsIoptions) ->
                 (case (({-# LINE 44 "./src-ag/PrintOcamlCode.ag" #-}
                         _lhsIoptions
                         {-# LINE 1187 "dist/build/PrintOcamlCode.hs" #-}
                         )) of
                  { !_exprOoptions ->
                  (case (expr_ _exprOoptions) of
                   { ( !_exprIpp) ->
                       (case (({-# LINE 148 "./src-ag/PrintOcamlCode.ag" #-}
                               _exprIpp
                               {-# LINE 1194 "dist/build/PrintOcamlCode.hs" #-}
                               )) of
                        { !_lhsOpp ->
                        ( _lhsOpp) }) }) })))
sem_Expr_LineExpr :: T_Expr ->
                     T_Expr
sem_Expr_LineExpr !(T_Expr expr_) =
    (T_Expr (\ (!_lhsIoptions) ->
                 (case (({-# LINE 44 "./src-ag/PrintOcamlCode.ag" #-}
                         _lhsIoptions
                         {-# LINE 1204 "dist/build/PrintOcamlCode.hs" #-}
                         )) of
                  { !_exprOoptions ->
                  (case (expr_ _exprOoptions) of
                   { ( !_exprIpp) ->
                       (case (({-# LINE 149 "./src-ag/PrintOcamlCode.ag" #-}
                               _exprIpp
                               {-# LINE 1211 "dist/build/PrintOcamlCode.hs" #-}
                               )) of
                        { !_lhsOpp ->
                        ( _lhsOpp) }) }) })))
sem_Expr_TypedExpr :: T_Expr ->
                      T_Type ->
                      T_Expr
sem_Expr_TypedExpr !(T_Expr expr_) !(T_Type tp_) =
    (T_Expr (\ (!_lhsIoptions) ->
                 (case (({-# LINE 44 "./src-ag/PrintOcamlCode.ag" #-}
                         _lhsIoptions
                         {-# LINE 1222 "dist/build/PrintOcamlCode.hs" #-}
                         )) of
                  { !_exprOoptions ->
                  (case (expr_ _exprOoptions) of
                   { ( !_exprIpp) ->
                       (case (({-# LINE 150 "./src-ag/PrintOcamlCode.ag" #-}
                               _exprIpp
                               {-# LINE 1229 "dist/build/PrintOcamlCode.hs" #-}
                               )) of
                        { !_lhsOpp ->
                        ( _lhsOpp) }) }) })))
sem_Expr_ResultExpr :: String ->
                       T_Expr ->
                       T_Expr
sem_Expr_ResultExpr !nt_ !(T_Expr expr_) =
    (T_Expr (\ (!_lhsIoptions) ->
                 (case (({-# LINE 44 "./src-ag/PrintOcamlCode.ag" #-}
                         _lhsIoptions
                         {-# LINE 1240 "dist/build/PrintOcamlCode.hs" #-}
                         )) of
                  { !_exprOoptions ->
                  (case (expr_ _exprOoptions) of
                   { ( !_exprIpp) ->
                       (case (({-# LINE 54 "./src-ag/PrintOcamlCode.ag" #-}
                               _exprIpp
                               {-# LINE 1247 "dist/build/PrintOcamlCode.hs" #-}
                               )) of
                        { !_lhsOpp ->
                        ( _lhsOpp) }) }) })))
sem_Expr_InvokeExpr :: String ->
                       T_Expr ->
                       T_Exprs ->
                       T_Expr
sem_Expr_InvokeExpr !nt_ !(T_Expr expr_) !(T_Exprs args_) =
    (T_Expr (\ (!_lhsIoptions) ->
                 (case (({-# LINE 44 "./src-ag/PrintOcamlCode.ag" #-}
                         _lhsIoptions
                         {-# LINE 1259 "dist/build/PrintOcamlCode.hs" #-}
                         )) of
                  { !_exprOoptions ->
                  (case (expr_ _exprOoptions) of
                   { ( !_exprIpp) ->
                       (case (({-# LINE 54 "./src-ag/PrintOcamlCode.ag" #-}
                               _exprIpp
                               {-# LINE 1266 "dist/build/PrintOcamlCode.hs" #-}
                               )) of
                        { !_lhsOpp ->
                        ( _lhsOpp) }) }) })))
sem_Expr_ResumeExpr :: String ->
                       T_Expr ->
                       T_Lhs ->
                       T_Expr ->
                       T_Expr
sem_Expr_ResumeExpr !nt_ !(T_Expr expr_) !(T_Lhs left_) !(T_Expr rhs_) =
    (T_Expr (\ (!_lhsIoptions) ->
                 (case (({-# LINE 44 "./src-ag/PrintOcamlCode.ag" #-}
                         _lhsIoptions
                         {-# LINE 1279 "dist/build/PrintOcamlCode.hs" #-}
                         )) of
                  { !_rhsOoptions ->
                  (case (rhs_ _rhsOoptions) of
                   { ( !_rhsIpp) ->
                       (case (({-# LINE 54 "./src-ag/PrintOcamlCode.ag" #-}
                               _rhsIpp
                               {-# LINE 1286 "dist/build/PrintOcamlCode.hs" #-}
                               )) of
                        { !_lhsOpp ->
                        ( _lhsOpp) }) }) })))
sem_Expr_SemFun :: String ->
                   T_Exprs ->
                   T_Expr ->
                   T_Expr
sem_Expr_SemFun !nt_ !(T_Exprs args_) !(T_Expr body_) =
    (T_Expr (\ (!_lhsIoptions) ->
                 (case (({-# LINE 44 "./src-ag/PrintOcamlCode.ag" #-}
                         _lhsIoptions
                         {-# LINE 1298 "dist/build/PrintOcamlCode.hs" #-}
                         )) of
                  { !_bodyOoptions ->
                  (case (body_ _bodyOoptions) of
                   { ( !_bodyIpp) ->
                       (case (({-# LINE 54 "./src-ag/PrintOcamlCode.ag" #-}
                               _bodyIpp
                               {-# LINE 1305 "dist/build/PrintOcamlCode.hs" #-}
                               )) of
                        { !_lhsOpp ->
                        ( _lhsOpp) }) }) })))
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
                  (case (({-# LINE 44 "./src-ag/PrintOcamlCode.ag" #-}
                          _lhsIoptions
                          {-# LINE 1345 "dist/build/PrintOcamlCode.hs" #-}
                          )) of
                   { !_tlOoptions ->
                   (case (tl_ _tlOoptions) of
                    { ( !_tlIpps) ->
                        (case (({-# LINE 44 "./src-ag/PrintOcamlCode.ag" #-}
                                _lhsIoptions
                                {-# LINE 1352 "dist/build/PrintOcamlCode.hs" #-}
                                )) of
                         { !_hdOoptions ->
                         (case (hd_ _hdOoptions) of
                          { ( !_hdIpp) ->
                              (case (({-# LINE 61 "./src-ag/PrintOcamlCode.ag" #-}
                                      _hdIpp : _tlIpps
                                      {-# LINE 1359 "dist/build/PrintOcamlCode.hs" #-}
                                      )) of
                               { !_lhsOpps ->
                               ( _lhsOpps) }) }) }) }) })))
sem_Exprs_Nil :: T_Exprs
sem_Exprs_Nil =
    (T_Exprs (\ (!_lhsIoptions) ->
                  (case (({-# LINE 62 "./src-ag/PrintOcamlCode.ag" #-}
                          []
                          {-# LINE 1368 "dist/build/PrintOcamlCode.hs" #-}
                          )) of
                   { !_lhsOpps ->
                   ( _lhsOpps) })))
-- Lhs ---------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         options              : Options
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative Pattern3:
         child pat3           : Pattern 
      alternative Pattern3SM:
         child pat3           : Pattern 
      alternative TupleLhs:
         child comps          : {[String]}
      alternative UnboxedTupleLhs:
         child comps          : {[String]}
      alternative Fun:
         child name           : {String}
         child args           : Exprs 
      alternative Unwrap:
         child name           : {String}
         child sub            : Lhs 
-}
-- cata
sem_Lhs :: Lhs ->
           T_Lhs
sem_Lhs !(Pattern3 _pat3) =
    (sem_Lhs_Pattern3 (sem_Pattern _pat3))
sem_Lhs !(Pattern3SM _pat3) =
    (sem_Lhs_Pattern3SM (sem_Pattern _pat3))
sem_Lhs !(TupleLhs _comps) =
    (sem_Lhs_TupleLhs _comps)
sem_Lhs !(UnboxedTupleLhs _comps) =
    (sem_Lhs_UnboxedTupleLhs _comps)
sem_Lhs !(Fun _name _args) =
    (sem_Lhs_Fun _name (sem_Exprs _args))
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
sem_Lhs_Pattern3 :: T_Pattern ->
                    T_Lhs
sem_Lhs_Pattern3 !(T_Pattern pat3_) =
    (T_Lhs (\ (!_lhsIoptions) ->
                (case (({-# LINE 44 "./src-ag/PrintOcamlCode.ag" #-}
                        _lhsIoptions
                        {-# LINE 1427 "dist/build/PrintOcamlCode.hs" #-}
                        )) of
                 { !_pat3Ooptions ->
                 (case (pat3_ _pat3Ooptions) of
                  { ( !_pat3Icopy,!_pat3IisUnderscore,!_pat3Ipp) ->
                      (case (({-# LINE 153 "./src-ag/PrintOcamlCode.ag" #-}
                              _pat3Ipp
                              {-# LINE 1434 "dist/build/PrintOcamlCode.hs" #-}
                              )) of
                       { !_lhsOpp ->
                       ( _lhsOpp) }) }) })))
sem_Lhs_Pattern3SM :: T_Pattern ->
                      T_Lhs
sem_Lhs_Pattern3SM !(T_Pattern pat3_) =
    (T_Lhs (\ (!_lhsIoptions) ->
                (case (({-# LINE 154 "./src-ag/PrintOcamlCode.ag" #-}
                        error "pp of Lhs.Pattern3SM not supported"
                        {-# LINE 1444 "dist/build/PrintOcamlCode.hs" #-}
                        )) of
                 { !_lhsOpp ->
                 ( _lhsOpp) })))
sem_Lhs_TupleLhs :: ([String]) ->
                    T_Lhs
sem_Lhs_TupleLhs !comps_ =
    (T_Lhs (\ (!_lhsIoptions) ->
                (case (({-# LINE 155 "./src-ag/PrintOcamlCode.ag" #-}
                        ppTuple False (map text comps_)
                        {-# LINE 1454 "dist/build/PrintOcamlCode.hs" #-}
                        )) of
                 { !_lhsOpp ->
                 ( _lhsOpp) })))
sem_Lhs_UnboxedTupleLhs :: ([String]) ->
                           T_Lhs
sem_Lhs_UnboxedTupleLhs !comps_ =
    (T_Lhs (\ (!_lhsIoptions) ->
                (case (({-# LINE 156 "./src-ag/PrintOcamlCode.ag" #-}
                        error "pp of Lhs.UnboxedTupleLhs not supported"
                        {-# LINE 1464 "dist/build/PrintOcamlCode.hs" #-}
                        )) of
                 { !_lhsOpp ->
                 ( _lhsOpp) })))
sem_Lhs_Fun :: String ->
               T_Exprs ->
               T_Lhs
sem_Lhs_Fun !name_ !(T_Exprs args_) =
    (T_Lhs (\ (!_lhsIoptions) ->
                (case (({-# LINE 44 "./src-ag/PrintOcamlCode.ag" #-}
                        _lhsIoptions
                        {-# LINE 1475 "dist/build/PrintOcamlCode.hs" #-}
                        )) of
                 { !_argsOoptions ->
                 (case (args_ _argsOoptions) of
                  { ( !_argsIpps) ->
                      (case (({-# LINE 157 "./src-ag/PrintOcamlCode.ag" #-}
                              name_ >#< hv_sp _argsIpps
                              {-# LINE 1482 "dist/build/PrintOcamlCode.hs" #-}
                              )) of
                       { !_lhsOpp ->
                       ( _lhsOpp) }) }) })))
sem_Lhs_Unwrap :: String ->
                  T_Lhs ->
                  T_Lhs
sem_Lhs_Unwrap !name_ !(T_Lhs sub_) =
    (T_Lhs (\ (!_lhsIoptions) ->
                (case (({-# LINE 44 "./src-ag/PrintOcamlCode.ag" #-}
                        _lhsIoptions
                        {-# LINE 1493 "dist/build/PrintOcamlCode.hs" #-}
                        )) of
                 { !_subOoptions ->
                 (case (sub_ _subOoptions) of
                  { ( !_subIpp) ->
                      (case (({-# LINE 158 "./src-ag/PrintOcamlCode.ag" #-}
                              pp_parens (name_ >#< _subIpp)
                              {-# LINE 1500 "dist/build/PrintOcamlCode.hs" #-}
                              )) of
                       { !_lhsOpp ->
                       ( _lhsOpp) }) }) })))
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
                      (case (({-# LINE 189 "./src-ag/PrintOcamlCode.ag" #-}
                              name_ >#< ":" >#< _tpIpp
                              {-# LINE 1539 "dist/build/PrintOcamlCode.hs" #-}
                              )) of
                       { !_lhsOpp ->
                       ( _lhsOpp) }) }))
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
                            (case (({-# LINE 77 "./src-ag/PrintOcamlCode.ag" #-}
                                    _hdIpp : _tlIpps
                                    {-# LINE 1579 "dist/build/PrintOcamlCode.hs" #-}
                                    )) of
                             { !_lhsOpps ->
                             ( _lhsOpps) }) }) }))
sem_NamedTypes_Nil :: T_NamedTypes
sem_NamedTypes_Nil =
    (T_NamedTypes (case (({-# LINE 78 "./src-ag/PrintOcamlCode.ag" #-}
                          []
                          {-# LINE 1587 "dist/build/PrintOcamlCode.hs" #-}
                          )) of
                   { !_lhsOpps ->
                   ( _lhsOpps) }))
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
      alternative Constr:
         child name           : {ConstructorIdent}
         child pats           : Patterns 
         visit 0:
            local copy        : _
      alternative Product:
         child pos            : {Pos}
         child pats           : Patterns 
         visit 0:
            local copy        : _
      alternative Alias:
         child field          : {Identifier}
         child attr           : {Identifier}
         child pat            : Pattern 
         visit 0:
            local copy        : _
      alternative Irrefutable:
         child pat            : Pattern 
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
sem_Pattern !(Constr _name _pats) =
    (sem_Pattern_Constr _name (sem_Patterns _pats))
sem_Pattern !(Product _pos _pats) =
    (sem_Pattern_Product _pos (sem_Patterns _pats))
sem_Pattern !(Alias _field _attr _pat) =
    (sem_Pattern_Alias _field _attr (sem_Pattern _pat))
sem_Pattern !(Irrefutable _pat) =
    (sem_Pattern_Irrefutable (sem_Pattern _pat))
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
sem_Pattern_Constr :: ConstructorIdent ->
                      T_Patterns ->
                      T_Pattern
sem_Pattern_Constr !name_ !(T_Patterns pats_) =
    (T_Pattern (\ (!_lhsIoptions) ->
                    (case (({-# LINE 44 "./src-ag/PrintOcamlCode.ag" #-}
                            _lhsIoptions
                            {-# LINE 1657 "dist/build/PrintOcamlCode.hs" #-}
                            )) of
                     { !_patsOoptions ->
                     (case (pats_ _patsOoptions) of
                      { ( !_patsIcopy,!_patsIpps) ->
                          (case (({-# LINE 22 "./src-ag/Patterns.ag" #-}
                                  Constr name_ _patsIcopy
                                  {-# LINE 1664 "dist/build/PrintOcamlCode.hs" #-}
                                  )) of
                           { !_copy ->
                           (case (({-# LINE 22 "./src-ag/Patterns.ag" #-}
                                   _copy
                                   {-# LINE 1669 "dist/build/PrintOcamlCode.hs" #-}
                                   )) of
                            { !_lhsOcopy ->
                            (case (({-# LINE 202 "./src-ag/PrintOcamlCode.ag" #-}
                                    False
                                    {-# LINE 1674 "dist/build/PrintOcamlCode.hs" #-}
                                    )) of
                             { !_lhsOisUnderscore ->
                             (case (({-# LINE 192 "./src-ag/PrintOcamlCode.ag" #-}
                                     pp_parens $ name_ >#< hv_sp _patsIpps
                                     {-# LINE 1679 "dist/build/PrintOcamlCode.hs" #-}
                                     )) of
                              { !_lhsOpp ->
                              ( _lhsOcopy,_lhsOisUnderscore,_lhsOpp) }) }) }) }) }) })))
sem_Pattern_Product :: Pos ->
                       T_Patterns ->
                       T_Pattern
sem_Pattern_Product !pos_ !(T_Patterns pats_) =
    (T_Pattern (\ (!_lhsIoptions) ->
                    (case (({-# LINE 44 "./src-ag/PrintOcamlCode.ag" #-}
                            _lhsIoptions
                            {-# LINE 1690 "dist/build/PrintOcamlCode.hs" #-}
                            )) of
                     { !_patsOoptions ->
                     (case (pats_ _patsOoptions) of
                      { ( !_patsIcopy,!_patsIpps) ->
                          (case (({-# LINE 22 "./src-ag/Patterns.ag" #-}
                                  Product pos_ _patsIcopy
                                  {-# LINE 1697 "dist/build/PrintOcamlCode.hs" #-}
                                  )) of
                           { !_copy ->
                           (case (({-# LINE 22 "./src-ag/Patterns.ag" #-}
                                   _copy
                                   {-# LINE 1702 "dist/build/PrintOcamlCode.hs" #-}
                                   )) of
                            { !_lhsOcopy ->
                            (case (({-# LINE 203 "./src-ag/PrintOcamlCode.ag" #-}
                                    False
                                    {-# LINE 1707 "dist/build/PrintOcamlCode.hs" #-}
                                    )) of
                             { !_lhsOisUnderscore ->
                             (case (({-# LINE 193 "./src-ag/PrintOcamlCode.ag" #-}
                                     pp_block "(" ")" "," _patsIpps
                                     {-# LINE 1712 "dist/build/PrintOcamlCode.hs" #-}
                                     )) of
                              { !_lhsOpp ->
                              ( _lhsOcopy,_lhsOisUnderscore,_lhsOpp) }) }) }) }) }) })))
sem_Pattern_Alias :: Identifier ->
                     Identifier ->
                     T_Pattern ->
                     T_Pattern
sem_Pattern_Alias !field_ !attr_ !(T_Pattern pat_) =
    (T_Pattern (\ (!_lhsIoptions) ->
                    (case (({-# LINE 44 "./src-ag/PrintOcamlCode.ag" #-}
                            _lhsIoptions
                            {-# LINE 1724 "dist/build/PrintOcamlCode.hs" #-}
                            )) of
                     { !_patOoptions ->
                     (case (pat_ _patOoptions) of
                      { ( !_patIcopy,!_patIisUnderscore,!_patIpp) ->
                          (case (({-# LINE 22 "./src-ag/Patterns.ag" #-}
                                  Alias field_ attr_ _patIcopy
                                  {-# LINE 1731 "dist/build/PrintOcamlCode.hs" #-}
                                  )) of
                           { !_copy ->
                           (case (({-# LINE 22 "./src-ag/Patterns.ag" #-}
                                   _copy
                                   {-# LINE 1736 "dist/build/PrintOcamlCode.hs" #-}
                                   )) of
                            { !_lhsOcopy ->
                            (case (({-# LINE 204 "./src-ag/PrintOcamlCode.ag" #-}
                                    False
                                    {-# LINE 1741 "dist/build/PrintOcamlCode.hs" #-}
                                    )) of
                             { !_lhsOisUnderscore ->
                             (case (({-# LINE 195 "./src-ag/PrintOcamlCode.ag" #-}
                                     if _patIisUnderscore
                                      then pp (attrname False field_ attr_)
                                      else error "pp of Pattern.Alias is only supported in the form (x@_)"
                                     {-# LINE 1748 "dist/build/PrintOcamlCode.hs" #-}
                                     )) of
                              { !_lhsOpp ->
                              ( _lhsOcopy,_lhsOisUnderscore,_lhsOpp) }) }) }) }) }) })))
sem_Pattern_Irrefutable :: T_Pattern ->
                           T_Pattern
sem_Pattern_Irrefutable !(T_Pattern pat_) =
    (T_Pattern (\ (!_lhsIoptions) ->
                    (case (({-# LINE 44 "./src-ag/PrintOcamlCode.ag" #-}
                            _lhsIoptions
                            {-# LINE 1758 "dist/build/PrintOcamlCode.hs" #-}
                            )) of
                     { !_patOoptions ->
                     (case (pat_ _patOoptions) of
                      { ( !_patIcopy,!_patIisUnderscore,!_patIpp) ->
                          (case (({-# LINE 22 "./src-ag/Patterns.ag" #-}
                                  Irrefutable _patIcopy
                                  {-# LINE 1765 "dist/build/PrintOcamlCode.hs" #-}
                                  )) of
                           { !_copy ->
                           (case (({-# LINE 22 "./src-ag/Patterns.ag" #-}
                                   _copy
                                   {-# LINE 1770 "dist/build/PrintOcamlCode.hs" #-}
                                   )) of
                            { !_lhsOcopy ->
                            (case (({-# LINE 201 "./src-ag/PrintOcamlCode.ag" #-}
                                    _patIisUnderscore
                                    {-# LINE 1775 "dist/build/PrintOcamlCode.hs" #-}
                                    )) of
                             { !_lhsOisUnderscore ->
                             (case (({-# LINE 198 "./src-ag/PrintOcamlCode.ag" #-}
                                     error "pp of Pattern.Irrefutable not supported"
                                     {-# LINE 1780 "dist/build/PrintOcamlCode.hs" #-}
                                     )) of
                              { !_lhsOpp ->
                              ( _lhsOcopy,_lhsOisUnderscore,_lhsOpp) }) }) }) }) }) })))
sem_Pattern_Underscore :: Pos ->
                          T_Pattern
sem_Pattern_Underscore !pos_ =
    (T_Pattern (\ (!_lhsIoptions) ->
                    (case (({-# LINE 22 "./src-ag/Patterns.ag" #-}
                            Underscore pos_
                            {-# LINE 1790 "dist/build/PrintOcamlCode.hs" #-}
                            )) of
                     { !_copy ->
                     (case (({-# LINE 22 "./src-ag/Patterns.ag" #-}
                             _copy
                             {-# LINE 1795 "dist/build/PrintOcamlCode.hs" #-}
                             )) of
                      { !_lhsOcopy ->
                      (case (({-# LINE 205 "./src-ag/PrintOcamlCode.ag" #-}
                              True
                              {-# LINE 1800 "dist/build/PrintOcamlCode.hs" #-}
                              )) of
                       { !_lhsOisUnderscore ->
                       (case (({-# LINE 199 "./src-ag/PrintOcamlCode.ag" #-}
                               text "_"
                               {-# LINE 1805 "dist/build/PrintOcamlCode.hs" #-}
                               )) of
                        { !_lhsOpp ->
                        ( _lhsOcopy,_lhsOisUnderscore,_lhsOpp) }) }) }) })))
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
                     (case (({-# LINE 44 "./src-ag/PrintOcamlCode.ag" #-}
                             _lhsIoptions
                             {-# LINE 1850 "dist/build/PrintOcamlCode.hs" #-}
                             )) of
                      { !_tlOoptions ->
                      (case (tl_ _tlOoptions) of
                       { ( !_tlIcopy,!_tlIpps) ->
                           (case (({-# LINE 44 "./src-ag/PrintOcamlCode.ag" #-}
                                   _lhsIoptions
                                   {-# LINE 1857 "dist/build/PrintOcamlCode.hs" #-}
                                   )) of
                            { !_hdOoptions ->
                            (case (hd_ _hdOoptions) of
                             { ( !_hdIcopy,!_hdIisUnderscore,!_hdIpp) ->
                                 (case (({-# LINE 22 "./src-ag/Patterns.ag" #-}
                                         (:) _hdIcopy _tlIcopy
                                         {-# LINE 1864 "dist/build/PrintOcamlCode.hs" #-}
                                         )) of
                                  { !_copy ->
                                  (case (({-# LINE 22 "./src-ag/Patterns.ag" #-}
                                          _copy
                                          {-# LINE 1869 "dist/build/PrintOcamlCode.hs" #-}
                                          )) of
                                   { !_lhsOcopy ->
                                   (case (({-# LINE 89 "./src-ag/PrintOcamlCode.ag" #-}
                                           _hdIpp : _tlIpps
                                           {-# LINE 1874 "dist/build/PrintOcamlCode.hs" #-}
                                           )) of
                                    { !_lhsOpps ->
                                    ( _lhsOcopy,_lhsOpps) }) }) }) }) }) }) })))
sem_Patterns_Nil :: T_Patterns
sem_Patterns_Nil =
    (T_Patterns (\ (!_lhsIoptions) ->
                     (case (({-# LINE 22 "./src-ag/Patterns.ag" #-}
                             []
                             {-# LINE 1883 "dist/build/PrintOcamlCode.hs" #-}
                             )) of
                      { !_copy ->
                      (case (({-# LINE 22 "./src-ag/Patterns.ag" #-}
                              _copy
                              {-# LINE 1888 "dist/build/PrintOcamlCode.hs" #-}
                              )) of
                       { !_lhsOcopy ->
                       (case (({-# LINE 90 "./src-ag/PrintOcamlCode.ag" #-}
                               []
                               {-# LINE 1893 "dist/build/PrintOcamlCode.hs" #-}
                               )) of
                        { !_lhsOpps ->
                        ( _lhsOcopy,_lhsOpps) }) }) })))
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
                    (case (({-# LINE 46 "./src-ag/PrintOcamlCode.ag" #-}
                            _lhsItextBlockMap
                            {-# LINE 1935 "dist/build/PrintOcamlCode.hs" #-}
                            )) of
                     { !_chunksOtextBlockMap ->
                     (case (({-# LINE 214 "./src-ag/PrintOcamlCode.ag" #-}
                             True
                             {-# LINE 1940 "dist/build/PrintOcamlCode.hs" #-}
                             )) of
                      { !_chunksOisToplevel ->
                      (case (({-# LINE 44 "./src-ag/PrintOcamlCode.ag" #-}
                              _lhsIoptions
                              {-# LINE 1945 "dist/build/PrintOcamlCode.hs" #-}
                              )) of
                       { !_chunksOoptions ->
                       (case (chunks_ _chunksOisToplevel _chunksOoptions _chunksOtextBlockMap) of
                        { ( !_chunksIpps) ->
                            (case (({-# LINE 58 "./src-ag/PrintOcamlCode.ag" #-}
                                    _chunksIpps
                                    {-# LINE 1952 "dist/build/PrintOcamlCode.hs" #-}
                                    )) of
                             { !_lhsOoutput ->
                             ( _lhsOoutput) }) }) }) }) })))
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
      alternative QuantApp:
         child left           : {String}
         child right          : Type 
      alternative TypeApp:
         child func           : Type 
         child args           : Types 
      alternative TupleType:
         child tps            : Types 
      alternative UnboxedTupleType:
         child tps            : Types 
      alternative List:
         child tp             : Type 
      alternative SimpleType:
         child txt            : {String}
      alternative NontermType:
         child name           : {String}
         child params         : {[String]}
         child deforested     : {Bool}
      alternative TMaybe:
         child tp             : Type 
      alternative TEither:
         child left           : Type 
         child right          : Type 
      alternative TMap:
         child key            : Type 
         child value          : Type 
      alternative TIntMap:
         child value          : Type 
-}
-- cata
sem_Type :: Type ->
            T_Type
sem_Type !(Arr _left _right) =
    (sem_Type_Arr (sem_Type _left) (sem_Type _right))
sem_Type !(CtxApp _left _right) =
    (sem_Type_CtxApp _left (sem_Type _right))
sem_Type !(QuantApp _left _right) =
    (sem_Type_QuantApp _left (sem_Type _right))
sem_Type !(TypeApp _func _args) =
    (sem_Type_TypeApp (sem_Type _func) (sem_Types _args))
sem_Type !(TupleType _tps) =
    (sem_Type_TupleType (sem_Types _tps))
sem_Type !(UnboxedTupleType _tps) =
    (sem_Type_UnboxedTupleType (sem_Types _tps))
sem_Type !(List _tp) =
    (sem_Type_List (sem_Type _tp))
sem_Type !(SimpleType _txt) =
    (sem_Type_SimpleType _txt)
sem_Type !(NontermType _name _params _deforested) =
    (sem_Type_NontermType _name _params _deforested)
sem_Type !(TMaybe _tp) =
    (sem_Type_TMaybe (sem_Type _tp))
sem_Type !(TEither _left _right) =
    (sem_Type_TEither (sem_Type _left) (sem_Type _right))
sem_Type !(TMap _key _value) =
    (sem_Type_TMap (sem_Type _key) (sem_Type _value))
sem_Type !(TIntMap _value) =
    (sem_Type_TIntMap (sem_Type _value))
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
                      (case (({-# LINE 161 "./src-ag/PrintOcamlCode.ag" #-}
                              pp_parens (_leftIpp >#< "->" >#< _rightIpp)
                              {-# LINE 2046 "dist/build/PrintOcamlCode.hs" #-}
                              )) of
                       { !_lhsOpp ->
                       ( _lhsOpp) }) }) }))
sem_Type_CtxApp :: ([(String, [String])]) ->
                   T_Type ->
                   T_Type
sem_Type_CtxApp !left_ !(T_Type right_) =
    (T_Type (case (({-# LINE 162 "./src-ag/PrintOcamlCode.ag" #-}
                    error "pp of Type.CtxApp not supported"
                    {-# LINE 2056 "dist/build/PrintOcamlCode.hs" #-}
                    )) of
             { !_lhsOpp ->
             ( _lhsOpp) }))
sem_Type_QuantApp :: String ->
                     T_Type ->
                     T_Type
sem_Type_QuantApp !left_ !(T_Type right_) =
    (T_Type (case (right_) of
             { ( !_rightIpp) ->
                 (case (({-# LINE 54 "./src-ag/PrintOcamlCode.ag" #-}
                         _rightIpp
                         {-# LINE 2068 "dist/build/PrintOcamlCode.hs" #-}
                         )) of
                  { !_lhsOpp ->
                  ( _lhsOpp) }) }))
sem_Type_TypeApp :: T_Type ->
                    T_Types ->
                    T_Type
sem_Type_TypeApp !(T_Type func_) !(T_Types args_) =
    (T_Type (case (args_) of
             { ( !_argsIpps) ->
                 (case (func_) of
                  { ( !_funcIpp) ->
                      (case (({-# LINE 163 "./src-ag/PrintOcamlCode.ag" #-}
                              pp_parens (hv_sp (_argsIpps ++ [_funcIpp]))
                              {-# LINE 2082 "dist/build/PrintOcamlCode.hs" #-}
                              )) of
                       { !_lhsOpp ->
                       ( _lhsOpp) }) }) }))
sem_Type_TupleType :: T_Types ->
                      T_Type
sem_Type_TupleType !(T_Types tps_) =
    (T_Type (case (tps_) of
             { ( !_tpsIpps) ->
                 (case (({-# LINE 164 "./src-ag/PrintOcamlCode.ag" #-}
                         pp_block "(" ")" "," _tpsIpps
                         {-# LINE 2093 "dist/build/PrintOcamlCode.hs" #-}
                         )) of
                  { !_lhsOpp ->
                  ( _lhsOpp) }) }))
sem_Type_UnboxedTupleType :: T_Types ->
                             T_Type
sem_Type_UnboxedTupleType !(T_Types tps_) =
    (T_Type (case (({-# LINE 166 "./src-ag/PrintOcamlCode.ag" #-}
                    error "pp of Type.UnboxedTupleType is not supported"
                    {-# LINE 2102 "dist/build/PrintOcamlCode.hs" #-}
                    )) of
             { !_lhsOpp ->
             ( _lhsOpp) }))
sem_Type_List :: T_Type ->
                 T_Type
sem_Type_List !(T_Type tp_) =
    (T_Type (case (tp_) of
             { ( !_tpIpp) ->
                 (case (({-# LINE 167 "./src-ag/PrintOcamlCode.ag" #-}
                         _tpIpp >#< "list"
                         {-# LINE 2113 "dist/build/PrintOcamlCode.hs" #-}
                         )) of
                  { !_lhsOpp ->
                  ( _lhsOpp) }) }))
sem_Type_SimpleType :: String ->
                       T_Type
sem_Type_SimpleType !txt_ =
    (T_Type (case (({-# LINE 168 "./src-ag/PrintOcamlCode.ag" #-}
                    text txt_
                    {-# LINE 2122 "dist/build/PrintOcamlCode.hs" #-}
                    )) of
             { !_lhsOpp ->
             ( _lhsOpp) }))
sem_Type_NontermType :: String ->
                        ([String]) ->
                        Bool ->
                        T_Type
sem_Type_NontermType !name_ !params_ !deforested_ =
    (T_Type (case (({-# LINE 169 "./src-ag/PrintOcamlCode.ag" #-}
                    pp_block "(" ")" " " (map text params_ ++ [text $ toOcamlTC name_])
                    {-# LINE 2133 "dist/build/PrintOcamlCode.hs" #-}
                    )) of
             { !_lhsOpp ->
             ( _lhsOpp) }))
sem_Type_TMaybe :: T_Type ->
                   T_Type
sem_Type_TMaybe !(T_Type tp_) =
    (T_Type (case (tp_) of
             { ( !_tpIpp) ->
                 (case (({-# LINE 170 "./src-ag/PrintOcamlCode.ag" #-}
                         _tpIpp >#< "opt"
                         {-# LINE 2144 "dist/build/PrintOcamlCode.hs" #-}
                         )) of
                  { !_lhsOpp ->
                  ( _lhsOpp) }) }))
sem_Type_TEither :: T_Type ->
                    T_Type ->
                    T_Type
sem_Type_TEither !(T_Type left_) !(T_Type right_) =
    (T_Type (case (({-# LINE 171 "./src-ag/PrintOcamlCode.ag" #-}
                    error "pp of Type.TEither is not supported"
                    {-# LINE 2154 "dist/build/PrintOcamlCode.hs" #-}
                    )) of
             { !_lhsOpp ->
             ( _lhsOpp) }))
sem_Type_TMap :: T_Type ->
                 T_Type ->
                 T_Type
sem_Type_TMap !(T_Type key_) !(T_Type value_) =
    (T_Type (case (({-# LINE 172 "./src-ag/PrintOcamlCode.ag" #-}
                    error "pp of Type.TMap is not supported"
                    {-# LINE 2164 "dist/build/PrintOcamlCode.hs" #-}
                    )) of
             { !_lhsOpp ->
             ( _lhsOpp) }))
sem_Type_TIntMap :: T_Type ->
                    T_Type
sem_Type_TIntMap !(T_Type value_) =
    (T_Type (case (({-# LINE 173 "./src-ag/PrintOcamlCode.ag" #-}
                    error "pp of Type.TIntMap is not supported"
                    {-# LINE 2173 "dist/build/PrintOcamlCode.hs" #-}
                    )) of
             { !_lhsOpp ->
             ( _lhsOpp) }))
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
                       (case (({-# LINE 73 "./src-ag/PrintOcamlCode.ag" #-}
                               _hdIpp : _tlIpps
                               {-# LINE 2213 "dist/build/PrintOcamlCode.hs" #-}
                               )) of
                        { !_lhsOpps ->
                        ( _lhsOpps) }) }) }))
sem_Types_Nil :: T_Types
sem_Types_Nil =
    (T_Types (case (({-# LINE 74 "./src-ag/PrintOcamlCode.ag" #-}
                     []
                     {-# LINE 2221 "dist/build/PrintOcamlCode.hs" #-}
                     )) of
              { !_lhsOpps ->
              ( _lhsOpps) }))