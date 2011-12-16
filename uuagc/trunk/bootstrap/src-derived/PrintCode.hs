{-# OPTIONS_GHC -XBangPatterns #-}

-- UUAGC 0.9.40.1 (src-ag/PrintCode.ag)
module PrintCode where
{-# LINE 10 "src-ag/PrintCode.ag" #-}

import Data.Char (isAlphaNum)
import Pretty
import Code
import Patterns
import Options
import CommonTypes (attrname, _LOC, getName, nullIdent)
import Data.List(intersperse)
import System.IO
import System.Directory
import System.FilePath
import CommonTypes(BlockInfo, BlockKind(..), identifier)
{-# LINE 19 "dist/build/PrintCode.hs" #-}

{-# LINE 2 "src-ag/Code.ag" #-}

import Pretty
import Patterns
import Data.List(partition)
import Data.Set(Set)
import qualified Data.Set as Set
import Data.Map(Map)
import qualified Data.Map as Map
{-# LINE 30 "dist/build/PrintCode.hs" #-}

{-# LINE 2 "src-ag/Patterns.ag" #-}

-- Patterns.ag imports
import UU.Scanner.Position(Pos)
import CommonTypes (ConstructorIdent,Identifier)
{-# LINE 37 "dist/build/PrintCode.hs" #-}
{-# LINE 24 "src-ag/PrintCode.ag" #-}

type PP_Docs = [PP_Doc]
{-# LINE 41 "dist/build/PrintCode.hs" #-}

{-# LINE 28 "src-ag/PrintCode.ag" #-}

ppMultiSeqH :: [PP_Doc] -> PP_Doc -> PP_Doc
ppMultiSeqH = ppMultiSeq' (>#<)

ppMultiSeqV :: [PP_Doc] -> PP_Doc -> PP_Doc
ppMultiSeqV = ppMultiSeq' (>-<)

ppMultiSeq' :: (PP_Doc -> PP_Doc -> PP_Doc) -> [PP_Doc] -> PP_Doc -> PP_Doc
ppMultiSeq' next strictArgs expr
  = foldr (\v r -> (v >#< "`seq`") `next` pp_parens r) expr strictArgs
{-# LINE 54 "dist/build/PrintCode.hs" #-}

{-# LINE 300 "src-ag/PrintCode.ag" #-}


reallySimple :: String -> Bool
reallySimple = and . map (\x -> isAlphaNum x || x=='_')

ppTuple True  pps = "(" >|< pp_block " " (replicate (length pps `max` 1) ')') ",(" pps
ppTuple False pps = "(" >|< pp_block " " ")" "," pps
ppUnboxedTuple True pps  = "(# " >|< pp_block " " (concat $ replicate (length pps `max` 1) " #)") ",(# " pps
ppUnboxedTuple False pps = "(# " >|< pp_block " " " #)" "," pps

{-# LINE 67 "dist/build/PrintCode.hs" #-}

{-# LINE 399 "src-ag/PrintCode.ag" #-}

locname' n = "_loc_" ++ getName n
{-# LINE 72 "dist/build/PrintCode.hs" #-}

{-# LINE 473 "src-ag/PrintCode.ag" #-}

renderDocs :: [PP_Doc] -> String
renderDocs pps = foldr (.) id (map (\d -> (disp d 50000) . ( '\n':) ) pps) ""
{-# LINE 78 "dist/build/PrintCode.hs" #-}

{-# LINE 521 "src-ag/PrintCode.ag" #-}

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
{-# LINE 95 "dist/build/PrintCode.hs" #-}

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
{-# LINE 114 "dist/build/PrintCode.hs" #-}
-- CaseAlt -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         nested               : Bool
         options              : Options
         outputfile           : String
      synthesized attribute:
         pps                  : PP_Docs
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
newtype T_CaseAlt = T_CaseAlt (Bool ->
                               Options ->
                               String ->
                               ( PP_Docs))
data Inh_CaseAlt = Inh_CaseAlt {nested_Inh_CaseAlt :: !(Bool),options_Inh_CaseAlt :: !(Options),outputfile_Inh_CaseAlt :: !(String)}
data Syn_CaseAlt = Syn_CaseAlt {pps_Syn_CaseAlt :: !(PP_Docs)}
wrap_CaseAlt :: T_CaseAlt ->
                Inh_CaseAlt ->
                Syn_CaseAlt
wrap_CaseAlt !(T_CaseAlt sem) !(Inh_CaseAlt _lhsInested _lhsIoptions _lhsIoutputfile) =
    (let ( !_lhsOpps) = sem _lhsInested _lhsIoptions _lhsIoutputfile
     in  (Syn_CaseAlt _lhsOpps))
sem_CaseAlt_CaseAlt :: T_Lhs ->
                       T_Expr ->
                       T_CaseAlt
sem_CaseAlt_CaseAlt !(T_Lhs left_) !(T_Expr expr_) =
    (T_CaseAlt (\ (!_lhsInested)
                  (!_lhsIoptions)
                  (!_lhsIoutputfile) ->
                    (case (({-# LINE 40 "src-ag/PrintCode.ag" #-}
                            _lhsIoutputfile
                            {-# LINE 156 "dist/build/PrintCode" #-}
                            )) of
                     { !_exprOoutputfile ->
                     (case (({-# LINE 50 "src-ag/PrintCode.ag" #-}
                             _lhsIoptions
                             {-# LINE 161 "dist/build/PrintCode" #-}
                             )) of
                      { !_exprOoptions ->
                      (case (({-# LINE 52 "src-ag/PrintCode.ag" #-}
                              _lhsInested
                              {-# LINE 166 "dist/build/PrintCode" #-}
                              )) of
                       { !_exprOnested ->
                       (case (({-# LINE 40 "src-ag/PrintCode.ag" #-}
                               _lhsIoutputfile
                               {-# LINE 171 "dist/build/PrintCode" #-}
                               )) of
                        { !_leftOoutputfile ->
                        (case (({-# LINE 50 "src-ag/PrintCode.ag" #-}
                                _lhsIoptions
                                {-# LINE 176 "dist/build/PrintCode" #-}
                                )) of
                         { !_leftOoptions ->
                         (case (({-# LINE 52 "src-ag/PrintCode.ag" #-}
                                 _lhsInested
                                 {-# LINE 181 "dist/build/PrintCode" #-}
                                 )) of
                          { !_leftOnested ->
                          (case (({-# LINE 422 "src-ag/PrintCode.ag" #-}
                                  False
                                  {-# LINE 186 "dist/build/PrintCode" #-}
                                  )) of
                           { !_leftOisDeclOfLet ->
                           (case (expr_ _exprOnested _exprOoptions _exprOoutputfile) of
                            { ( !_exprIpp) ->
                                (case (left_ _leftOisDeclOfLet _leftOnested _leftOoptions _leftOoutputfile) of
                                 { ( !_leftIpp) ->
                                     (case (({-# LINE 219 "src-ag/PrintCode.ag" #-}
                                             ["{" >#< _leftIpp >#< "->", _exprIpp >#< "}"]
                                             {-# LINE 195 "dist/build/PrintCode" #-}
                                             )) of
                                      { !_lhsOpps ->
                                      ( _lhsOpps) }) }) }) }) }) }) }) }) }) })))
-- CaseAlts ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         nested               : Bool
         options              : Options
         outputfile           : String
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
newtype T_CaseAlts = T_CaseAlts (Bool ->
                                 Options ->
                                 String ->
                                 ( PP_Docs))
data Inh_CaseAlts = Inh_CaseAlts {nested_Inh_CaseAlts :: !(Bool),options_Inh_CaseAlts :: !(Options),outputfile_Inh_CaseAlts :: !(String)}
data Syn_CaseAlts = Syn_CaseAlts {pps_Syn_CaseAlts :: !(PP_Docs)}
wrap_CaseAlts :: T_CaseAlts ->
                 Inh_CaseAlts ->
                 Syn_CaseAlts
wrap_CaseAlts !(T_CaseAlts sem) !(Inh_CaseAlts _lhsInested _lhsIoptions _lhsIoutputfile) =
    (let ( !_lhsOpps) = sem _lhsInested _lhsIoptions _lhsIoutputfile
     in  (Syn_CaseAlts _lhsOpps))
sem_CaseAlts_Cons :: T_CaseAlt ->
                     T_CaseAlts ->
                     T_CaseAlts
sem_CaseAlts_Cons !(T_CaseAlt hd_) !(T_CaseAlts tl_) =
    (T_CaseAlts (\ (!_lhsInested)
                   (!_lhsIoptions)
                   (!_lhsIoutputfile) ->
                     (case (({-# LINE 40 "src-ag/PrintCode.ag" #-}
                             _lhsIoutputfile
                             {-# LINE 241 "dist/build/PrintCode" #-}
                             )) of
                      { !_tlOoutputfile ->
                      (case (({-# LINE 50 "src-ag/PrintCode.ag" #-}
                              _lhsIoptions
                              {-# LINE 246 "dist/build/PrintCode" #-}
                              )) of
                       { !_tlOoptions ->
                       (case (({-# LINE 56 "src-ag/PrintCode.ag" #-}
                               _lhsInested
                               {-# LINE 251 "dist/build/PrintCode" #-}
                               )) of
                        { !_tlOnested ->
                        (case (({-# LINE 40 "src-ag/PrintCode.ag" #-}
                                _lhsIoutputfile
                                {-# LINE 256 "dist/build/PrintCode" #-}
                                )) of
                         { !_hdOoutputfile ->
                         (case (({-# LINE 50 "src-ag/PrintCode.ag" #-}
                                 _lhsIoptions
                                 {-# LINE 261 "dist/build/PrintCode" #-}
                                 )) of
                          { !_hdOoptions ->
                          (case (({-# LINE 56 "src-ag/PrintCode.ag" #-}
                                  _lhsInested
                                  {-# LINE 266 "dist/build/PrintCode" #-}
                                  )) of
                           { !_hdOnested ->
                           (case (tl_ _tlOnested _tlOoptions _tlOoutputfile) of
                            { ( !_tlIpps) ->
                                (case (hd_ _hdOnested _hdOoptions _hdOoutputfile) of
                                 { ( !_hdIpps) ->
                                     (case (({-# LINE 69 "src-ag/PrintCode.ag" #-}
                                             _hdIpps ++ _tlIpps
                                             {-# LINE 275 "dist/build/PrintCode" #-}
                                             )) of
                                      { !_lhsOpps ->
                                      ( _lhsOpps) }) }) }) }) }) }) }) }) })))
sem_CaseAlts_Nil :: T_CaseAlts
sem_CaseAlts_Nil =
    (T_CaseAlts (\ (!_lhsInested)
                   (!_lhsIoptions)
                   (!_lhsIoutputfile) ->
                     (case (({-# LINE 70 "src-ag/PrintCode.ag" #-}
                             []
                             {-# LINE 286 "dist/build/PrintCode" #-}
                             )) of
                      { !_lhsOpps ->
                      ( _lhsOpps) })))
-- Chunk -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         importBlocks         : PP_Doc
         isDeclOfLet          : Bool
         mainFile             : String
         mainName             : String
         moduleHeader         : String -> String -> String -> Bool -> String
         nested               : Bool
         options              : Options
         optionsLine          : String
         pragmaBlocks         : String
         textBlockMap         : Map BlockInfo PP_Doc
         textBlocks           : PP_Doc
      synthesized attributes:
         appendCommon         : [[PP_Doc]]
         appendMain           : [[PP_Doc]]
         genSems              : IO ()
         imports              : [String]
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
         visit 0:
            local outputfile  : _
            local exports     : _
-}
-- cata
sem_Chunk :: Chunk ->
             T_Chunk
sem_Chunk !(Chunk _name _comment _info _dataDef _cataFun _semDom _semWrapper _semFunctions _semNames) =
    (sem_Chunk_Chunk _name (sem_Decl _comment) (sem_Decls _info) (sem_Decls _dataDef) (sem_Decls _cataFun) (sem_Decls _semDom) (sem_Decls _semWrapper) (sem_Decls _semFunctions) _semNames)
-- semantic domain
newtype T_Chunk = T_Chunk (PP_Doc ->
                           Bool ->
                           String ->
                           String ->
                           (String -> String -> String -> Bool -> String) ->
                           Bool ->
                           Options ->
                           String ->
                           String ->
                           (Map BlockInfo PP_Doc) ->
                           PP_Doc ->
                           ( ([[PP_Doc]]),([[PP_Doc]]),(IO ()),([String]),PP_Docs))
data Inh_Chunk = Inh_Chunk {importBlocks_Inh_Chunk :: !(PP_Doc),isDeclOfLet_Inh_Chunk :: !(Bool),mainFile_Inh_Chunk :: !(String),mainName_Inh_Chunk :: !(String),moduleHeader_Inh_Chunk :: !((String -> String -> String -> Bool -> String)),nested_Inh_Chunk :: !(Bool),options_Inh_Chunk :: !(Options),optionsLine_Inh_Chunk :: !(String),pragmaBlocks_Inh_Chunk :: !(String),textBlockMap_Inh_Chunk :: !((Map BlockInfo PP_Doc)),textBlocks_Inh_Chunk :: !(PP_Doc)}
data Syn_Chunk = Syn_Chunk {appendCommon_Syn_Chunk :: !(([[PP_Doc]])),appendMain_Syn_Chunk :: !(([[PP_Doc]])),genSems_Syn_Chunk :: !((IO ())),imports_Syn_Chunk :: !(([String])),pps_Syn_Chunk :: !(PP_Docs)}
wrap_Chunk :: T_Chunk ->
              Inh_Chunk ->
              Syn_Chunk
wrap_Chunk !(T_Chunk sem) !(Inh_Chunk _lhsIimportBlocks _lhsIisDeclOfLet _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInested _lhsIoptions _lhsIoptionsLine _lhsIpragmaBlocks _lhsItextBlockMap _lhsItextBlocks) =
    (let ( !_lhsOappendCommon,!_lhsOappendMain,!_lhsOgenSems,!_lhsOimports,!_lhsOpps) = sem _lhsIimportBlocks _lhsIisDeclOfLet _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInested _lhsIoptions _lhsIoptionsLine _lhsIpragmaBlocks _lhsItextBlockMap _lhsItextBlocks
     in  (Syn_Chunk _lhsOappendCommon _lhsOappendMain _lhsOgenSems _lhsOimports _lhsOpps))
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
    (T_Chunk (\ (!_lhsIimportBlocks)
                (!_lhsIisDeclOfLet)
                (!_lhsImainFile)
                (!_lhsImainName)
                (!_lhsImoduleHeader)
                (!_lhsInested)
                (!_lhsIoptions)
                (!_lhsIoptionsLine)
                (!_lhsIpragmaBlocks)
                (!_lhsItextBlockMap)
                (!_lhsItextBlocks) ->
                  (case (({-# LINE 44 "src-ag/PrintCode.ag" #-}
                          if sepSemMods _lhsIoptions
                          then replaceBaseName _lhsImainFile (takeBaseName _lhsImainFile ++ "_" ++ name_)
                          else _lhsImainFile
                          {-# LINE 378 "dist/build/PrintCode" #-}
                          )) of
                   { !_outputfile ->
                   (case (({-# LINE 40 "src-ag/PrintCode.ag" #-}
                           _outputfile
                           {-# LINE 383 "dist/build/PrintCode" #-}
                           )) of
                    { !_semWrapperOoutputfile ->
                    (case (({-# LINE 50 "src-ag/PrintCode.ag" #-}
                            _lhsIoptions
                            {-# LINE 388 "dist/build/PrintCode" #-}
                            )) of
                     { !_semWrapperOoptions ->
                     (case (({-# LINE 54 "src-ag/PrintCode.ag" #-}
                             _lhsInested
                             {-# LINE 393 "dist/build/PrintCode" #-}
                             )) of
                      { !_semWrapperOnested ->
                      (case (({-# LINE 407 "src-ag/PrintCode.ag" #-}
                              _lhsIisDeclOfLet
                              {-# LINE 398 "dist/build/PrintCode" #-}
                              )) of
                       { !_semWrapperOisDeclOfLet ->
                       (case (({-# LINE 40 "src-ag/PrintCode.ag" #-}
                               _outputfile
                               {-# LINE 403 "dist/build/PrintCode" #-}
                               )) of
                        { !_semDomOoutputfile ->
                        (case (({-# LINE 50 "src-ag/PrintCode.ag" #-}
                                _lhsIoptions
                                {-# LINE 408 "dist/build/PrintCode" #-}
                                )) of
                         { !_semDomOoptions ->
                         (case (({-# LINE 54 "src-ag/PrintCode.ag" #-}
                                 _lhsInested
                                 {-# LINE 413 "dist/build/PrintCode" #-}
                                 )) of
                          { !_semDomOnested ->
                          (case (({-# LINE 407 "src-ag/PrintCode.ag" #-}
                                  _lhsIisDeclOfLet
                                  {-# LINE 418 "dist/build/PrintCode" #-}
                                  )) of
                           { !_semDomOisDeclOfLet ->
                           (case (({-# LINE 40 "src-ag/PrintCode.ag" #-}
                                   _outputfile
                                   {-# LINE 423 "dist/build/PrintCode" #-}
                                   )) of
                            { !_dataDefOoutputfile ->
                            (case (({-# LINE 50 "src-ag/PrintCode.ag" #-}
                                    _lhsIoptions
                                    {-# LINE 428 "dist/build/PrintCode" #-}
                                    )) of
                             { !_dataDefOoptions ->
                             (case (({-# LINE 54 "src-ag/PrintCode.ag" #-}
                                     _lhsInested
                                     {-# LINE 433 "dist/build/PrintCode" #-}
                                     )) of
                              { !_dataDefOnested ->
                              (case (({-# LINE 407 "src-ag/PrintCode.ag" #-}
                                      _lhsIisDeclOfLet
                                      {-# LINE 438 "dist/build/PrintCode" #-}
                                      )) of
                               { !_dataDefOisDeclOfLet ->
                               (case (({-# LINE 40 "src-ag/PrintCode.ag" #-}
                                       _outputfile
                                       {-# LINE 443 "dist/build/PrintCode" #-}
                                       )) of
                                { !_commentOoutputfile ->
                                (case (({-# LINE 50 "src-ag/PrintCode.ag" #-}
                                        _lhsIoptions
                                        {-# LINE 448 "dist/build/PrintCode" #-}
                                        )) of
                                 { !_commentOoptions ->
                                 (case (({-# LINE 52 "src-ag/PrintCode.ag" #-}
                                         _lhsInested
                                         {-# LINE 453 "dist/build/PrintCode" #-}
                                         )) of
                                  { !_commentOnested ->
                                  (case (({-# LINE 407 "src-ag/PrintCode.ag" #-}
                                          _lhsIisDeclOfLet
                                          {-# LINE 458 "dist/build/PrintCode" #-}
                                          )) of
                                   { !_commentOisDeclOfLet ->
                                   (case (semWrapper_ _semWrapperOisDeclOfLet _semWrapperOnested _semWrapperOoptions _semWrapperOoutputfile) of
                                    { ( !_semWrapperIpps) ->
                                        (case (semDom_ _semDomOisDeclOfLet _semDomOnested _semDomOoptions _semDomOoutputfile) of
                                         { ( !_semDomIpps) ->
                                             (case (dataDef_ _dataDefOisDeclOfLet _dataDefOnested _dataDefOoptions _dataDefOoutputfile) of
                                              { ( !_dataDefIpps) ->
                                                  (case (comment_ _commentOisDeclOfLet _commentOnested _commentOoptions _commentOoutputfile) of
                                                   { ( !_commentIpp) ->
                                                       (case (({-# LINE 487 "src-ag/PrintCode.ag" #-}
                                                               [ [_commentIpp]
                                                               , _dataDefIpps
                                                               , _semDomIpps
                                                               , if reference _lhsIoptions then _semWrapperIpps else []
                                                               ]
                                                               {-# LINE 475 "dist/build/PrintCode" #-}
                                                               )) of
                                                        { !_lhsOappendCommon ->
                                                        (case (({-# LINE 40 "src-ag/PrintCode.ag" #-}
                                                                _outputfile
                                                                {-# LINE 480 "dist/build/PrintCode" #-}
                                                                )) of
                                                         { !_cataFunOoutputfile ->
                                                         (case (({-# LINE 50 "src-ag/PrintCode.ag" #-}
                                                                 _lhsIoptions
                                                                 {-# LINE 485 "dist/build/PrintCode" #-}
                                                                 )) of
                                                          { !_cataFunOoptions ->
                                                          (case (({-# LINE 54 "src-ag/PrintCode.ag" #-}
                                                                  _lhsInested
                                                                  {-# LINE 490 "dist/build/PrintCode" #-}
                                                                  )) of
                                                           { !_cataFunOnested ->
                                                           (case (({-# LINE 407 "src-ag/PrintCode.ag" #-}
                                                                   _lhsIisDeclOfLet
                                                                   {-# LINE 495 "dist/build/PrintCode" #-}
                                                                   )) of
                                                            { !_cataFunOisDeclOfLet ->
                                                            (case (cataFun_ _cataFunOisDeclOfLet _cataFunOnested _cataFunOoptions _cataFunOoutputfile) of
                                                             { ( !_cataFunIpps) ->
                                                                 (case (({-# LINE 493 "src-ag/PrintCode.ag" #-}
                                                                         [ [_commentIpp]
                                                                         , _cataFunIpps
                                                                         , if reference _lhsIoptions then [] else _semWrapperIpps
                                                                         ]
                                                                         {-# LINE 505 "dist/build/PrintCode" #-}
                                                                         )) of
                                                                  { !_lhsOappendMain ->
                                                                  (case (({-# LINE 40 "src-ag/PrintCode.ag" #-}
                                                                          _outputfile
                                                                          {-# LINE 510 "dist/build/PrintCode" #-}
                                                                          )) of
                                                                   { !_semFunctionsOoutputfile ->
                                                                   (case (({-# LINE 50 "src-ag/PrintCode.ag" #-}
                                                                           _lhsIoptions
                                                                           {-# LINE 515 "dist/build/PrintCode" #-}
                                                                           )) of
                                                                    { !_semFunctionsOoptions ->
                                                                    (case (({-# LINE 54 "src-ag/PrintCode.ag" #-}
                                                                            _lhsInested
                                                                            {-# LINE 520 "dist/build/PrintCode" #-}
                                                                            )) of
                                                                     { !_semFunctionsOnested ->
                                                                     (case (({-# LINE 407 "src-ag/PrintCode.ag" #-}
                                                                             _lhsIisDeclOfLet
                                                                             {-# LINE 525 "dist/build/PrintCode" #-}
                                                                             )) of
                                                                      { !_semFunctionsOisDeclOfLet ->
                                                                      (case (({-# LINE 40 "src-ag/PrintCode.ag" #-}
                                                                              _outputfile
                                                                              {-# LINE 530 "dist/build/PrintCode" #-}
                                                                              )) of
                                                                       { !_infoOoutputfile ->
                                                                       (case (({-# LINE 50 "src-ag/PrintCode.ag" #-}
                                                                               _lhsIoptions
                                                                               {-# LINE 535 "dist/build/PrintCode" #-}
                                                                               )) of
                                                                        { !_infoOoptions ->
                                                                        (case (({-# LINE 54 "src-ag/PrintCode.ag" #-}
                                                                                _lhsInested
                                                                                {-# LINE 540 "dist/build/PrintCode" #-}
                                                                                )) of
                                                                         { !_infoOnested ->
                                                                         (case (({-# LINE 407 "src-ag/PrintCode.ag" #-}
                                                                                 _lhsIisDeclOfLet
                                                                                 {-# LINE 545 "dist/build/PrintCode" #-}
                                                                                 )) of
                                                                          { !_infoOisDeclOfLet ->
                                                                          (case (({-# LINE 519 "src-ag/PrintCode.ag" #-}
                                                                                  concat $ intersperse "," semNames_
                                                                                  {-# LINE 550 "dist/build/PrintCode" #-}
                                                                                  )) of
                                                                           { !_exports ->
                                                                           (case (semFunctions_ _semFunctionsOisDeclOfLet _semFunctionsOnested _semFunctionsOoptions _semFunctionsOoutputfile) of
                                                                            { ( !_semFunctionsIpps) ->
                                                                                (case (info_ _infoOisDeclOfLet _infoOnested _infoOoptions _infoOoutputfile) of
                                                                                 { ( !_infoIpps) ->
                                                                                     (case (({-# LINE 503 "src-ag/PrintCode.ag" #-}
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
                                                                                             {-# LINE 570 "dist/build/PrintCode" #-}
                                                                                             )) of
                                                                                      { !_lhsOgenSems ->
                                                                                      (case (({-# LINE 481 "src-ag/PrintCode.ag" #-}
                                                                                              ["import " ++ _lhsImainName ++ "_" ++ name_ ++ "\n"]
                                                                                              {-# LINE 575 "dist/build/PrintCode" #-}
                                                                                              )) of
                                                                                       { !_lhsOimports ->
                                                                                       (case (({-# LINE 97 "src-ag/PrintCode.ag" #-}
                                                                                               _commentIpp
                                                                                               :  _infoIpps
                                                                                               ++ _dataDefIpps
                                                                                               ++ _cataFunIpps
                                                                                               ++ _semDomIpps
                                                                                               ++ _semWrapperIpps
                                                                                               ++ _semFunctionsIpps
                                                                                               ++ [Map.findWithDefault empty (BlockOther, Just $ identifier name_) _lhsItextBlockMap]
                                                                                               {-# LINE 587 "dist/build/PrintCode" #-}
                                                                                               )) of
                                                                                        { !_lhsOpps ->
                                                                                        ( _lhsOappendCommon,_lhsOappendMain,_lhsOgenSems,_lhsOimports,_lhsOpps) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })))
-- Chunks ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         importBlocks         : PP_Doc
         isDeclOfLet          : Bool
         mainFile             : String
         mainName             : String
         moduleHeader         : String -> String -> String -> Bool -> String
         nested               : Bool
         options              : Options
         optionsLine          : String
         pragmaBlocks         : String
         textBlockMap         : Map BlockInfo PP_Doc
         textBlocks           : PP_Doc
      synthesized attributes:
         appendCommon         : [[PP_Doc]]
         appendMain           : [[PP_Doc]]
         genSems              : IO ()
         imports              : [String]
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
newtype T_Chunks = T_Chunks (PP_Doc ->
                             Bool ->
                             String ->
                             String ->
                             (String -> String -> String -> Bool -> String) ->
                             Bool ->
                             Options ->
                             String ->
                             String ->
                             (Map BlockInfo PP_Doc) ->
                             PP_Doc ->
                             ( ([[PP_Doc]]),([[PP_Doc]]),(IO ()),([String]),PP_Docs))
data Inh_Chunks = Inh_Chunks {importBlocks_Inh_Chunks :: !(PP_Doc),isDeclOfLet_Inh_Chunks :: !(Bool),mainFile_Inh_Chunks :: !(String),mainName_Inh_Chunks :: !(String),moduleHeader_Inh_Chunks :: !((String -> String -> String -> Bool -> String)),nested_Inh_Chunks :: !(Bool),options_Inh_Chunks :: !(Options),optionsLine_Inh_Chunks :: !(String),pragmaBlocks_Inh_Chunks :: !(String),textBlockMap_Inh_Chunks :: !((Map BlockInfo PP_Doc)),textBlocks_Inh_Chunks :: !(PP_Doc)}
data Syn_Chunks = Syn_Chunks {appendCommon_Syn_Chunks :: !(([[PP_Doc]])),appendMain_Syn_Chunks :: !(([[PP_Doc]])),genSems_Syn_Chunks :: !((IO ())),imports_Syn_Chunks :: !(([String])),pps_Syn_Chunks :: !(PP_Docs)}
wrap_Chunks :: T_Chunks ->
               Inh_Chunks ->
               Syn_Chunks
wrap_Chunks !(T_Chunks sem) !(Inh_Chunks _lhsIimportBlocks _lhsIisDeclOfLet _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInested _lhsIoptions _lhsIoptionsLine _lhsIpragmaBlocks _lhsItextBlockMap _lhsItextBlocks) =
    (let ( !_lhsOappendCommon,!_lhsOappendMain,!_lhsOgenSems,!_lhsOimports,!_lhsOpps) = sem _lhsIimportBlocks _lhsIisDeclOfLet _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInested _lhsIoptions _lhsIoptionsLine _lhsIpragmaBlocks _lhsItextBlockMap _lhsItextBlocks
     in  (Syn_Chunks _lhsOappendCommon _lhsOappendMain _lhsOgenSems _lhsOimports _lhsOpps))
sem_Chunks_Cons :: T_Chunk ->
                   T_Chunks ->
                   T_Chunks
sem_Chunks_Cons !(T_Chunk hd_) !(T_Chunks tl_) =
    (T_Chunks (\ (!_lhsIimportBlocks)
                 (!_lhsIisDeclOfLet)
                 (!_lhsImainFile)
                 (!_lhsImainName)
                 (!_lhsImoduleHeader)
                 (!_lhsInested)
                 (!_lhsIoptions)
                 (!_lhsIoptionsLine)
                 (!_lhsIpragmaBlocks)
                 (!_lhsItextBlockMap)
                 (!_lhsItextBlocks) ->
                   (case (({-# LINE 50 "src-ag/PrintCode.ag" #-}
                           _lhsIoptions
                           {-# LINE 661 "dist/build/PrintCode" #-}
                           )) of
                    { !_tlOoptions ->
                    (case (({-# LINE 54 "src-ag/PrintCode.ag" #-}
                            _lhsInested
                            {-# LINE 666 "dist/build/PrintCode" #-}
                            )) of
                     { !_tlOnested ->
                     (case (({-# LINE 437 "src-ag/PrintCode.ag" #-}
                             _lhsImainFile
                             {-# LINE 671 "dist/build/PrintCode" #-}
                             )) of
                      { !_tlOmainFile ->
                      (case (({-# LINE 407 "src-ag/PrintCode.ag" #-}
                              _lhsIisDeclOfLet
                              {-# LINE 676 "dist/build/PrintCode" #-}
                              )) of
                       { !_tlOisDeclOfLet ->
                       (case (({-# LINE 50 "src-ag/PrintCode.ag" #-}
                               _lhsIoptions
                               {-# LINE 681 "dist/build/PrintCode" #-}
                               )) of
                        { !_hdOoptions ->
                        (case (({-# LINE 54 "src-ag/PrintCode.ag" #-}
                                _lhsInested
                                {-# LINE 686 "dist/build/PrintCode" #-}
                                )) of
                         { !_hdOnested ->
                         (case (({-# LINE 437 "src-ag/PrintCode.ag" #-}
                                 _lhsImainFile
                                 {-# LINE 691 "dist/build/PrintCode" #-}
                                 )) of
                          { !_hdOmainFile ->
                          (case (({-# LINE 407 "src-ag/PrintCode.ag" #-}
                                  _lhsIisDeclOfLet
                                  {-# LINE 696 "dist/build/PrintCode" #-}
                                  )) of
                           { !_hdOisDeclOfLet ->
                           (case (({-# LINE 434 "src-ag/PrintCode.ag" #-}
                                   _lhsItextBlocks
                                   {-# LINE 701 "dist/build/PrintCode" #-}
                                   )) of
                            { !_tlOtextBlocks ->
                            (case (({-# LINE 435 "src-ag/PrintCode.ag" #-}
                                    _lhsItextBlockMap
                                    {-# LINE 706 "dist/build/PrintCode" #-}
                                    )) of
                             { !_tlOtextBlockMap ->
                             (case (({-# LINE 433 "src-ag/PrintCode.ag" #-}
                                     _lhsIpragmaBlocks
                                     {-# LINE 711 "dist/build/PrintCode" #-}
                                     )) of
                              { !_tlOpragmaBlocks ->
                              (case (({-# LINE 436 "src-ag/PrintCode.ag" #-}
                                      _lhsIoptionsLine
                                      {-# LINE 716 "dist/build/PrintCode" #-}
                                      )) of
                               { !_tlOoptionsLine ->
                               (case (({-# LINE 439 "src-ag/PrintCode.ag" #-}
                                       _lhsImoduleHeader
                                       {-# LINE 721 "dist/build/PrintCode" #-}
                                       )) of
                                { !_tlOmoduleHeader ->
                                (case (({-# LINE 438 "src-ag/PrintCode.ag" #-}
                                        _lhsImainName
                                        {-# LINE 726 "dist/build/PrintCode" #-}
                                        )) of
                                 { !_tlOmainName ->
                                 (case (({-# LINE 432 "src-ag/PrintCode.ag" #-}
                                         _lhsIimportBlocks
                                         {-# LINE 731 "dist/build/PrintCode" #-}
                                         )) of
                                  { !_tlOimportBlocks ->
                                  (case (tl_ _tlOimportBlocks _tlOisDeclOfLet _tlOmainFile _tlOmainName _tlOmoduleHeader _tlOnested _tlOoptions _tlOoptionsLine _tlOpragmaBlocks _tlOtextBlockMap _tlOtextBlocks) of
                                   { ( !_tlIappendCommon,!_tlIappendMain,!_tlIgenSems,!_tlIimports,!_tlIpps) ->
                                       (case (({-# LINE 434 "src-ag/PrintCode.ag" #-}
                                               _lhsItextBlocks
                                               {-# LINE 738 "dist/build/PrintCode" #-}
                                               )) of
                                        { !_hdOtextBlocks ->
                                        (case (({-# LINE 435 "src-ag/PrintCode.ag" #-}
                                                _lhsItextBlockMap
                                                {-# LINE 743 "dist/build/PrintCode" #-}
                                                )) of
                                         { !_hdOtextBlockMap ->
                                         (case (({-# LINE 433 "src-ag/PrintCode.ag" #-}
                                                 _lhsIpragmaBlocks
                                                 {-# LINE 748 "dist/build/PrintCode" #-}
                                                 )) of
                                          { !_hdOpragmaBlocks ->
                                          (case (({-# LINE 436 "src-ag/PrintCode.ag" #-}
                                                  _lhsIoptionsLine
                                                  {-# LINE 753 "dist/build/PrintCode" #-}
                                                  )) of
                                           { !_hdOoptionsLine ->
                                           (case (({-# LINE 439 "src-ag/PrintCode.ag" #-}
                                                   _lhsImoduleHeader
                                                   {-# LINE 758 "dist/build/PrintCode" #-}
                                                   )) of
                                            { !_hdOmoduleHeader ->
                                            (case (({-# LINE 438 "src-ag/PrintCode.ag" #-}
                                                    _lhsImainName
                                                    {-# LINE 763 "dist/build/PrintCode" #-}
                                                    )) of
                                             { !_hdOmainName ->
                                             (case (({-# LINE 432 "src-ag/PrintCode.ag" #-}
                                                     _lhsIimportBlocks
                                                     {-# LINE 768 "dist/build/PrintCode" #-}
                                                     )) of
                                              { !_hdOimportBlocks ->
                                              (case (hd_ _hdOimportBlocks _hdOisDeclOfLet _hdOmainFile _hdOmainName _hdOmoduleHeader _hdOnested _hdOoptions _hdOoptionsLine _hdOpragmaBlocks _hdOtextBlockMap _hdOtextBlocks) of
                                               { ( !_hdIappendCommon,!_hdIappendMain,!_hdIgenSems,!_hdIimports,!_hdIpps) ->
                                                   (case (({-# LINE 483 "src-ag/PrintCode.ag" #-}
                                                           _hdIappendCommon ++ _tlIappendCommon
                                                           {-# LINE 775 "dist/build/PrintCode" #-}
                                                           )) of
                                                    { !_lhsOappendCommon ->
                                                    (case (({-# LINE 483 "src-ag/PrintCode.ag" #-}
                                                            _hdIappendMain ++ _tlIappendMain
                                                            {-# LINE 780 "dist/build/PrintCode" #-}
                                                            )) of
                                                     { !_lhsOappendMain ->
                                                     (case (({-# LINE 499 "src-ag/PrintCode.ag" #-}
                                                             _hdIgenSems >> _tlIgenSems
                                                             {-# LINE 785 "dist/build/PrintCode" #-}
                                                             )) of
                                                      { !_lhsOgenSems ->
                                                      (case (({-# LINE 478 "src-ag/PrintCode.ag" #-}
                                                              _hdIimports ++ _tlIimports
                                                              {-# LINE 790 "dist/build/PrintCode" #-}
                                                              )) of
                                                       { !_lhsOimports ->
                                                       (case (({-# LINE 89 "src-ag/PrintCode.ag" #-}
                                                               _hdIpps ++ _tlIpps
                                                               {-# LINE 795 "dist/build/PrintCode" #-}
                                                               )) of
                                                        { !_lhsOpps ->
                                                        ( _lhsOappendCommon,_lhsOappendMain,_lhsOgenSems,_lhsOimports,_lhsOpps) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })))
sem_Chunks_Nil :: T_Chunks
sem_Chunks_Nil =
    (T_Chunks (\ (!_lhsIimportBlocks)
                 (!_lhsIisDeclOfLet)
                 (!_lhsImainFile)
                 (!_lhsImainName)
                 (!_lhsImoduleHeader)
                 (!_lhsInested)
                 (!_lhsIoptions)
                 (!_lhsIoptionsLine)
                 (!_lhsIpragmaBlocks)
                 (!_lhsItextBlockMap)
                 (!_lhsItextBlocks) ->
                   (case (({-# LINE 483 "src-ag/PrintCode.ag" #-}
                           []
                           {-# LINE 814 "dist/build/PrintCode" #-}
                           )) of
                    { !_lhsOappendCommon ->
                    (case (({-# LINE 483 "src-ag/PrintCode.ag" #-}
                            []
                            {-# LINE 819 "dist/build/PrintCode" #-}
                            )) of
                     { !_lhsOappendMain ->
                     (case (({-# LINE 499 "src-ag/PrintCode.ag" #-}
                             return ()
                             {-# LINE 824 "dist/build/PrintCode" #-}
                             )) of
                      { !_lhsOgenSems ->
                      (case (({-# LINE 478 "src-ag/PrintCode.ag" #-}
                              []
                              {-# LINE 829 "dist/build/PrintCode" #-}
                              )) of
                       { !_lhsOimports ->
                       (case (({-# LINE 90 "src-ag/PrintCode.ag" #-}
                               []
                               {-# LINE 834 "dist/build/PrintCode" #-}
                               )) of
                        { !_lhsOpps ->
                        ( _lhsOappendCommon,_lhsOappendMain,_lhsOgenSems,_lhsOimports,_lhsOpps) }) }) }) }) })))
-- DataAlt -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         nested               : Bool
         strictPre            : PP_Doc
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
newtype T_DataAlt = T_DataAlt (Bool ->
                               PP_Doc ->
                               ( PP_Doc))
data Inh_DataAlt = Inh_DataAlt {nested_Inh_DataAlt :: !(Bool),strictPre_Inh_DataAlt :: !(PP_Doc)}
data Syn_DataAlt = Syn_DataAlt {pp_Syn_DataAlt :: !(PP_Doc)}
wrap_DataAlt :: T_DataAlt ->
                Inh_DataAlt ->
                Syn_DataAlt
wrap_DataAlt !(T_DataAlt sem) !(Inh_DataAlt _lhsInested _lhsIstrictPre) =
    (let ( !_lhsOpp) = sem _lhsInested _lhsIstrictPre
     in  (Syn_DataAlt _lhsOpp))
sem_DataAlt_DataAlt :: String ->
                       T_Types ->
                       T_DataAlt
sem_DataAlt_DataAlt !name_ !(T_Types args_) =
    (T_DataAlt (\ (!_lhsInested)
                  (!_lhsIstrictPre) ->
                    (case (({-# LINE 54 "src-ag/PrintCode.ag" #-}
                            _lhsInested
                            {-# LINE 881 "dist/build/PrintCode" #-}
                            )) of
                     { !_argsOnested ->
                     (case (args_ _argsOnested) of
                      { ( !_argsIpps) ->
                          (case (({-# LINE 222 "src-ag/PrintCode.ag" #-}
                                  name_ >#< hv_sp (map ((_lhsIstrictPre >|<) . pp_parens) _argsIpps)
                                  {-# LINE 888 "dist/build/PrintCode" #-}
                                  )) of
                           { !_lhsOpp ->
                           ( _lhsOpp) }) }) })))
sem_DataAlt_Record :: String ->
                      T_NamedTypes ->
                      T_DataAlt
sem_DataAlt_Record !name_ !(T_NamedTypes args_) =
    (T_DataAlt (\ (!_lhsInested)
                  (!_lhsIstrictPre) ->
                    (case (({-# LINE 54 "src-ag/PrintCode.ag" #-}
                            _lhsInested
                            {-# LINE 900 "dist/build/PrintCode" #-}
                            )) of
                     { !_argsOnested ->
                     (case (args_ _argsOnested) of
                      { ( !_argsIpps) ->
                          (case (({-# LINE 223 "src-ag/PrintCode.ag" #-}
                                  name_ >#< pp_block "{" "}" "," _argsIpps
                                  {-# LINE 907 "dist/build/PrintCode" #-}
                                  )) of
                           { !_lhsOpp ->
                           ( _lhsOpp) }) }) })))
-- DataAlts ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         nested               : Bool
         strictPre            : PP_Doc
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
newtype T_DataAlts = T_DataAlts (Bool ->
                                 PP_Doc ->
                                 ( PP_Docs))
data Inh_DataAlts = Inh_DataAlts {nested_Inh_DataAlts :: !(Bool),strictPre_Inh_DataAlts :: !(PP_Doc)}
data Syn_DataAlts = Syn_DataAlts {pps_Syn_DataAlts :: !(PP_Docs)}
wrap_DataAlts :: T_DataAlts ->
                 Inh_DataAlts ->
                 Syn_DataAlts
wrap_DataAlts !(T_DataAlts sem) !(Inh_DataAlts _lhsInested _lhsIstrictPre) =
    (let ( !_lhsOpps) = sem _lhsInested _lhsIstrictPre
     in  (Syn_DataAlts _lhsOpps))
sem_DataAlts_Cons :: T_DataAlt ->
                     T_DataAlts ->
                     T_DataAlts
sem_DataAlts_Cons !(T_DataAlt hd_) !(T_DataAlts tl_) =
    (T_DataAlts (\ (!_lhsInested)
                   (!_lhsIstrictPre) ->
                     (case (({-# LINE 317 "src-ag/PrintCode.ag" #-}
                             _lhsIstrictPre
                             {-# LINE 950 "dist/build/PrintCode" #-}
                             )) of
                      { !_tlOstrictPre ->
                      (case (({-# LINE 54 "src-ag/PrintCode.ag" #-}
                              _lhsInested
                              {-# LINE 955 "dist/build/PrintCode" #-}
                              )) of
                       { !_tlOnested ->
                       (case (({-# LINE 317 "src-ag/PrintCode.ag" #-}
                               _lhsIstrictPre
                               {-# LINE 960 "dist/build/PrintCode" #-}
                               )) of
                        { !_hdOstrictPre ->
                        (case (({-# LINE 52 "src-ag/PrintCode.ag" #-}
                                _lhsInested
                                {-# LINE 965 "dist/build/PrintCode" #-}
                                )) of
                         { !_hdOnested ->
                         (case (tl_ _tlOnested _tlOstrictPre) of
                          { ( !_tlIpps) ->
                              (case (hd_ _hdOnested _hdOstrictPre) of
                               { ( !_hdIpp) ->
                                   (case (({-# LINE 73 "src-ag/PrintCode.ag" #-}
                                           _hdIpp : _tlIpps
                                           {-# LINE 974 "dist/build/PrintCode" #-}
                                           )) of
                                    { !_lhsOpps ->
                                    ( _lhsOpps) }) }) }) }) }) }) })))
sem_DataAlts_Nil :: T_DataAlts
sem_DataAlts_Nil =
    (T_DataAlts (\ (!_lhsInested)
                   (!_lhsIstrictPre) ->
                     (case (({-# LINE 74 "src-ag/PrintCode.ag" #-}
                             []
                             {-# LINE 984 "dist/build/PrintCode" #-}
                             )) of
                      { !_lhsOpps ->
                      ( _lhsOpps) })))
-- Decl --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         isDeclOfLet          : Bool
         nested               : Bool
         options              : Options
         outputfile           : String
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
         visit 0:
            local strat       : _
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
                         Bool ->
                         Options ->
                         String ->
                         ( PP_Doc))
data Inh_Decl = Inh_Decl {isDeclOfLet_Inh_Decl :: !(Bool),nested_Inh_Decl :: !(Bool),options_Inh_Decl :: !(Options),outputfile_Inh_Decl :: !(String)}
data Syn_Decl = Syn_Decl {pp_Syn_Decl :: !(PP_Doc)}
wrap_Decl :: T_Decl ->
             Inh_Decl ->
             Syn_Decl
wrap_Decl !(T_Decl sem) !(Inh_Decl _lhsIisDeclOfLet _lhsInested _lhsIoptions _lhsIoutputfile) =
    (let ( !_lhsOpp) = sem _lhsIisDeclOfLet _lhsInested _lhsIoptions _lhsIoutputfile
     in  (Syn_Decl _lhsOpp))
sem_Decl_Bind :: T_Lhs ->
                 T_Expr ->
                 T_Decl
sem_Decl_Bind !(T_Lhs left_) !(T_Expr rhs_) =
    (T_Decl (\ (!_lhsIisDeclOfLet)
               (!_lhsInested)
               (!_lhsIoptions)
               (!_lhsIoutputfile) ->
                 (case (({-# LINE 40 "src-ag/PrintCode.ag" #-}
                         _lhsIoutputfile
                         {-# LINE 1093 "dist/build/PrintCode" #-}
                         )) of
                  { !_rhsOoutputfile ->
                  (case (({-# LINE 50 "src-ag/PrintCode.ag" #-}
                          _lhsIoptions
                          {-# LINE 1098 "dist/build/PrintCode" #-}
                          )) of
                   { !_rhsOoptions ->
                   (case (({-# LINE 52 "src-ag/PrintCode.ag" #-}
                           _lhsInested
                           {-# LINE 1103 "dist/build/PrintCode" #-}
                           )) of
                    { !_rhsOnested ->
                    (case (({-# LINE 40 "src-ag/PrintCode.ag" #-}
                            _lhsIoutputfile
                            {-# LINE 1108 "dist/build/PrintCode" #-}
                            )) of
                     { !_leftOoutputfile ->
                     (case (({-# LINE 50 "src-ag/PrintCode.ag" #-}
                             _lhsIoptions
                             {-# LINE 1113 "dist/build/PrintCode" #-}
                             )) of
                      { !_leftOoptions ->
                      (case (({-# LINE 52 "src-ag/PrintCode.ag" #-}
                              _lhsInested
                              {-# LINE 1118 "dist/build/PrintCode" #-}
                              )) of
                       { !_leftOnested ->
                       (case (({-# LINE 407 "src-ag/PrintCode.ag" #-}
                               _lhsIisDeclOfLet
                               {-# LINE 1123 "dist/build/PrintCode" #-}
                               )) of
                        { !_leftOisDeclOfLet ->
                        (case (rhs_ _rhsOnested _rhsOoptions _rhsOoutputfile) of
                         { ( !_rhsIpp) ->
                             (case (left_ _leftOisDeclOfLet _leftOnested _leftOoptions _leftOoutputfile) of
                              { ( !_leftIpp) ->
                                  (case (({-# LINE 109 "src-ag/PrintCode.ag" #-}
                                          _leftIpp >#< "<-" >#< _rhsIpp
                                          {-# LINE 1132 "dist/build/PrintCode" #-}
                                          )) of
                                   { !_lhsOpp ->
                                   ( _lhsOpp) }) }) }) }) }) }) }) }) }) })))
sem_Decl_BindLet :: T_Lhs ->
                    T_Expr ->
                    T_Decl
sem_Decl_BindLet !(T_Lhs left_) !(T_Expr rhs_) =
    (T_Decl (\ (!_lhsIisDeclOfLet)
               (!_lhsInested)
               (!_lhsIoptions)
               (!_lhsIoutputfile) ->
                 (case (({-# LINE 40 "src-ag/PrintCode.ag" #-}
                         _lhsIoutputfile
                         {-# LINE 1146 "dist/build/PrintCode" #-}
                         )) of
                  { !_rhsOoutputfile ->
                  (case (({-# LINE 50 "src-ag/PrintCode.ag" #-}
                          _lhsIoptions
                          {-# LINE 1151 "dist/build/PrintCode" #-}
                          )) of
                   { !_rhsOoptions ->
                   (case (({-# LINE 52 "src-ag/PrintCode.ag" #-}
                           _lhsInested
                           {-# LINE 1156 "dist/build/PrintCode" #-}
                           )) of
                    { !_rhsOnested ->
                    (case (({-# LINE 40 "src-ag/PrintCode.ag" #-}
                            _lhsIoutputfile
                            {-# LINE 1161 "dist/build/PrintCode" #-}
                            )) of
                     { !_leftOoutputfile ->
                     (case (({-# LINE 50 "src-ag/PrintCode.ag" #-}
                             _lhsIoptions
                             {-# LINE 1166 "dist/build/PrintCode" #-}
                             )) of
                      { !_leftOoptions ->
                      (case (({-# LINE 52 "src-ag/PrintCode.ag" #-}
                              _lhsInested
                              {-# LINE 1171 "dist/build/PrintCode" #-}
                              )) of
                       { !_leftOnested ->
                       (case (({-# LINE 407 "src-ag/PrintCode.ag" #-}
                               _lhsIisDeclOfLet
                               {-# LINE 1176 "dist/build/PrintCode" #-}
                               )) of
                        { !_leftOisDeclOfLet ->
                        (case (rhs_ _rhsOnested _rhsOoptions _rhsOoutputfile) of
                         { ( !_rhsIpp) ->
                             (case (left_ _leftOisDeclOfLet _leftOnested _leftOoptions _leftOoutputfile) of
                              { ( !_leftIpp) ->
                                  (case (({-# LINE 110 "src-ag/PrintCode.ag" #-}
                                          "let" >#< _leftIpp >#< "=" >#< _rhsIpp
                                          {-# LINE 1185 "dist/build/PrintCode" #-}
                                          )) of
                                   { !_lhsOpp ->
                                   ( _lhsOpp) }) }) }) }) }) }) }) }) }) })))
sem_Decl_Comment :: String ->
                    T_Decl
sem_Decl_Comment !txt_ =
    (T_Decl (\ (!_lhsIisDeclOfLet)
               (!_lhsInested)
               (!_lhsIoptions)
               (!_lhsIoutputfile) ->
                 (case (({-# LINE 123 "src-ag/PrintCode.ag" #-}
                         if '\n' `elem` txt_
                           then "{-" >-< vlist (lines txt_) >-< "-}"
                           else "--" >#< txt_
                         {-# LINE 1200 "dist/build/PrintCode" #-}
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
    (T_Decl (\ (!_lhsIisDeclOfLet)
               (!_lhsInested)
               (!_lhsIoptions)
               (!_lhsIoutputfile) ->
                 (case (({-# LINE 54 "src-ag/PrintCode.ag" #-}
                         _lhsInested
                         {-# LINE 1217 "dist/build/PrintCode" #-}
                         )) of
                  { !_altsOnested ->
                  (case (({-# LINE 320 "src-ag/PrintCode.ag" #-}
                          if strict_ then pp "!" else empty
                          {-# LINE 1222 "dist/build/PrintCode" #-}
                          )) of
                   { !_altsOstrictPre ->
                   (case (alts_ _altsOnested _altsOstrictPre) of
                    { ( !_altsIpps) ->
                        (case (({-# LINE 111 "src-ag/PrintCode.ag" #-}
                                "data" >#< hv_sp (name_ : params_)
                                >#<  ( case _altsIpps of
                                             [] -> empty
                                             (x:xs) ->              "=" >#<  x
                                                    >-< vlist (map ("|" >#<) xs)
                                        >-< if null derivings_
                                               then empty
                                               else "deriving" >#< ppTuple False (map text derivings_)
                                     )
                                {-# LINE 1237 "dist/build/PrintCode" #-}
                                )) of
                         { !_lhsOpp ->
                         ( _lhsOpp) }) }) }) })))
sem_Decl_Decl :: T_Lhs ->
                 T_Expr ->
                 (Set String) ->
                 (Set String) ->
                 T_Decl
sem_Decl_Decl !(T_Lhs left_) !(T_Expr rhs_) !binds_ !uses_ =
    (T_Decl (\ (!_lhsIisDeclOfLet)
               (!_lhsInested)
               (!_lhsIoptions)
               (!_lhsIoutputfile) ->
                 (case (({-# LINE 40 "src-ag/PrintCode.ag" #-}
                         _lhsIoutputfile
                         {-# LINE 1253 "dist/build/PrintCode" #-}
                         )) of
                  { !_rhsOoutputfile ->
                  (case (({-# LINE 50 "src-ag/PrintCode.ag" #-}
                          _lhsIoptions
                          {-# LINE 1258 "dist/build/PrintCode" #-}
                          )) of
                   { !_rhsOoptions ->
                   (case (({-# LINE 52 "src-ag/PrintCode.ag" #-}
                           _lhsInested
                           {-# LINE 1263 "dist/build/PrintCode" #-}
                           )) of
                    { !_rhsOnested ->
                    (case (({-# LINE 40 "src-ag/PrintCode.ag" #-}
                            _lhsIoutputfile
                            {-# LINE 1268 "dist/build/PrintCode" #-}
                            )) of
                     { !_leftOoutputfile ->
                     (case (({-# LINE 50 "src-ag/PrintCode.ag" #-}
                             _lhsIoptions
                             {-# LINE 1273 "dist/build/PrintCode" #-}
                             )) of
                      { !_leftOoptions ->
                      (case (({-# LINE 52 "src-ag/PrintCode.ag" #-}
                              _lhsInested
                              {-# LINE 1278 "dist/build/PrintCode" #-}
                              )) of
                       { !_leftOnested ->
                       (case (({-# LINE 407 "src-ag/PrintCode.ag" #-}
                               _lhsIisDeclOfLet
                               {-# LINE 1283 "dist/build/PrintCode" #-}
                               )) of
                        { !_leftOisDeclOfLet ->
                        (case (rhs_ _rhsOnested _rhsOoptions _rhsOoutputfile) of
                         { ( !_rhsIpp) ->
                             (case (left_ _leftOisDeclOfLet _leftOnested _leftOoptions _leftOoutputfile) of
                              { ( !_leftIpp) ->
                                  (case (({-# LINE 107 "src-ag/PrintCode.ag" #-}
                                          _leftIpp >#< "="
                                          >-< indent 4 _rhsIpp
                                          {-# LINE 1293 "dist/build/PrintCode" #-}
                                          )) of
                                   { !_lhsOpp ->
                                   ( _lhsOpp) }) }) }) }) }) }) }) }) }) })))
sem_Decl_EvalDecl :: String ->
                     T_Lhs ->
                     T_Expr ->
                     T_Decl
sem_Decl_EvalDecl !nt_ !(T_Lhs left_) !(T_Expr rhs_) =
    (T_Decl (\ (!_lhsIisDeclOfLet)
               (!_lhsInested)
               (!_lhsIoptions)
               (!_lhsIoutputfile) ->
                 (case (({-# LINE 40 "src-ag/PrintCode.ag" #-}
                         _lhsIoutputfile
                         {-# LINE 1308 "dist/build/PrintCode" #-}
                         )) of
                  { !_rhsOoutputfile ->
                  (case (({-# LINE 50 "src-ag/PrintCode.ag" #-}
                          _lhsIoptions
                          {-# LINE 1313 "dist/build/PrintCode" #-}
                          )) of
                   { !_rhsOoptions ->
                   (case (({-# LINE 52 "src-ag/PrintCode.ag" #-}
                           _lhsInested
                           {-# LINE 1318 "dist/build/PrintCode" #-}
                           )) of
                    { !_rhsOnested ->
                    (case (({-# LINE 40 "src-ag/PrintCode.ag" #-}
                            _lhsIoutputfile
                            {-# LINE 1323 "dist/build/PrintCode" #-}
                            )) of
                     { !_leftOoutputfile ->
                     (case (({-# LINE 50 "src-ag/PrintCode.ag" #-}
                             _lhsIoptions
                             {-# LINE 1328 "dist/build/PrintCode" #-}
                             )) of
                      { !_leftOoptions ->
                      (case (({-# LINE 52 "src-ag/PrintCode.ag" #-}
                              _lhsInested
                              {-# LINE 1333 "dist/build/PrintCode" #-}
                              )) of
                       { !_leftOnested ->
                       (case (({-# LINE 407 "src-ag/PrintCode.ag" #-}
                               _lhsIisDeclOfLet
                               {-# LINE 1338 "dist/build/PrintCode" #-}
                               )) of
                        { !_leftOisDeclOfLet ->
                        (case (({-# LINE 130 "src-ag/PrintCode.ag" #-}
                                if breadthFirstStrict _lhsIoptions
                                then "stepwiseEval"
                                else "lazyEval"
                                {-# LINE 1345 "dist/build/PrintCode" #-}
                                )) of
                         { !_strat ->
                         (case (rhs_ _rhsOnested _rhsOoptions _rhsOoutputfile) of
                          { ( !_rhsIpp) ->
                              (case (left_ _leftOisDeclOfLet _leftOnested _leftOoptions _leftOoutputfile) of
                               { ( !_leftIpp) ->
                                   (case (({-# LINE 133 "src-ag/PrintCode.ag" #-}
                                           if breadthFirst _lhsIoptions
                                           then _leftIpp >#< "=" >#< "case" >#< _strat     >#< pp_parens _rhsIpp >#< "of"
                                                >-< indent 4 (
                                                  pp_parens (nt_ >|< "_Syn" >#< "_val") >#< "-> _val"
                                                )
                                           else _leftIpp >#< "=" >#< _rhsIpp
                                           {-# LINE 1359 "dist/build/PrintCode" #-}
                                           )) of
                                    { !_lhsOpp ->
                                    ( _lhsOpp) }) }) }) }) }) }) }) }) }) }) })))
sem_Decl_NewType :: String ->
                    ([String]) ->
                    String ->
                    T_Type ->
                    T_Decl
sem_Decl_NewType !name_ !params_ !con_ !(T_Type tp_) =
    (T_Decl (\ (!_lhsIisDeclOfLet)
               (!_lhsInested)
               (!_lhsIoptions)
               (!_lhsIoutputfile) ->
                 (case (({-# LINE 52 "src-ag/PrintCode.ag" #-}
                         _lhsInested
                         {-# LINE 1375 "dist/build/PrintCode" #-}
                         )) of
                  { !_tpOnested ->
                  (case (tp_ _tpOnested) of
                   { ( !_tpIpp,!_tpIprec) ->
                       (case (({-# LINE 120 "src-ag/PrintCode.ag" #-}
                               "newtype" >#< hv_sp (name_ : params_) >#< "=" >#< con_ >#< pp_parens _tpIpp
                               {-# LINE 1382 "dist/build/PrintCode" #-}
                               )) of
                        { !_lhsOpp ->
                        ( _lhsOpp) }) }) })))
sem_Decl_PragmaDecl :: String ->
                       T_Decl
sem_Decl_PragmaDecl !txt_ =
    (T_Decl (\ (!_lhsIisDeclOfLet)
               (!_lhsInested)
               (!_lhsIoptions)
               (!_lhsIoutputfile) ->
                 (case (({-# LINE 126 "src-ag/PrintCode.ag" #-}
                         "{-#" >#< text txt_ >#< "#-}"
                         {-# LINE 1395 "dist/build/PrintCode" #-}
                         )) of
                  { !_lhsOpp ->
                  ( _lhsOpp) })))
sem_Decl_Resume :: Bool ->
                   String ->
                   T_Lhs ->
                   T_Expr ->
                   T_Decl
sem_Decl_Resume !monadic_ !nt_ !(T_Lhs left_) !(T_Expr rhs_) =
    (T_Decl (\ (!_lhsIisDeclOfLet)
               (!_lhsInested)
               (!_lhsIoptions)
               (!_lhsIoutputfile) ->
                 (case (({-# LINE 40 "src-ag/PrintCode.ag" #-}
                         _lhsIoutputfile
                         {-# LINE 1411 "dist/build/PrintCode" #-}
                         )) of
                  { !_rhsOoutputfile ->
                  (case (({-# LINE 50 "src-ag/PrintCode.ag" #-}
                          _lhsIoptions
                          {-# LINE 1416 "dist/build/PrintCode" #-}
                          )) of
                   { !_rhsOoptions ->
                   (case (({-# LINE 52 "src-ag/PrintCode.ag" #-}
                           _lhsInested
                           {-# LINE 1421 "dist/build/PrintCode" #-}
                           )) of
                    { !_rhsOnested ->
                    (case (({-# LINE 40 "src-ag/PrintCode.ag" #-}
                            _lhsIoutputfile
                            {-# LINE 1426 "dist/build/PrintCode" #-}
                            )) of
                     { !_leftOoutputfile ->
                     (case (({-# LINE 50 "src-ag/PrintCode.ag" #-}
                             _lhsIoptions
                             {-# LINE 1431 "dist/build/PrintCode" #-}
                             )) of
                      { !_leftOoptions ->
                      (case (({-# LINE 52 "src-ag/PrintCode.ag" #-}
                              _lhsInested
                              {-# LINE 1436 "dist/build/PrintCode" #-}
                              )) of
                       { !_leftOnested ->
                       (case (({-# LINE 407 "src-ag/PrintCode.ag" #-}
                               _lhsIisDeclOfLet
                               {-# LINE 1441 "dist/build/PrintCode" #-}
                               )) of
                        { !_leftOisDeclOfLet ->
                        (case (rhs_ _rhsOnested _rhsOoptions _rhsOoutputfile) of
                         { ( !_rhsIpp) ->
                             (case (left_ _leftOisDeclOfLet _leftOnested _leftOoptions _leftOoutputfile) of
                              { ( !_leftIpp) ->
                                  (case (({-# LINE 127 "src-ag/PrintCode.ag" #-}
                                          if monadic_
                                          then _leftIpp >#< "<-" >#< _rhsIpp
                                          else _leftIpp >#< "=" >-< indent 4 _rhsIpp
                                          {-# LINE 1452 "dist/build/PrintCode" #-}
                                          )) of
                                   { !_lhsOpp ->
                                   ( _lhsOpp) }) }) }) }) }) }) }) }) }) })))
sem_Decl_TSig :: String ->
                 T_Type ->
                 T_Decl
sem_Decl_TSig !name_ !(T_Type tp_) =
    (T_Decl (\ (!_lhsIisDeclOfLet)
               (!_lhsInested)
               (!_lhsIoptions)
               (!_lhsIoutputfile) ->
                 (case (({-# LINE 52 "src-ag/PrintCode.ag" #-}
                         _lhsInested
                         {-# LINE 1466 "dist/build/PrintCode" #-}
                         )) of
                  { !_tpOnested ->
                  (case (tp_ _tpOnested) of
                   { ( !_tpIpp,!_tpIprec) ->
                       (case (({-# LINE 122 "src-ag/PrintCode.ag" #-}
                               name_ >#< "::" >#< _tpIpp
                               {-# LINE 1473 "dist/build/PrintCode" #-}
                               )) of
                        { !_lhsOpp ->
                        ( _lhsOpp) }) }) })))
sem_Decl_Type :: String ->
                 ([String]) ->
                 T_Type ->
                 T_Decl
sem_Decl_Type !name_ !params_ !(T_Type tp_) =
    (T_Decl (\ (!_lhsIisDeclOfLet)
               (!_lhsInested)
               (!_lhsIoptions)
               (!_lhsIoutputfile) ->
                 (case (({-# LINE 52 "src-ag/PrintCode.ag" #-}
                         _lhsInested
                         {-# LINE 1488 "dist/build/PrintCode" #-}
                         )) of
                  { !_tpOnested ->
                  (case (tp_ _tpOnested) of
                   { ( !_tpIpp,!_tpIprec) ->
                       (case (({-# LINE 121 "src-ag/PrintCode.ag" #-}
                               "type" >#< hv_sp (name_ : params_) >#< "=" >#<  _tpIpp
                               {-# LINE 1495 "dist/build/PrintCode" #-}
                               )) of
                        { !_lhsOpp ->
                        ( _lhsOpp) }) }) })))
-- Decls -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         isDeclOfLet          : Bool
         nested               : Bool
         options              : Options
         outputfile           : String
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
                           Bool ->
                           Options ->
                           String ->
                           ( PP_Docs))
data Inh_Decls = Inh_Decls {isDeclOfLet_Inh_Decls :: !(Bool),nested_Inh_Decls :: !(Bool),options_Inh_Decls :: !(Options),outputfile_Inh_Decls :: !(String)}
data Syn_Decls = Syn_Decls {pps_Syn_Decls :: !(PP_Docs)}
wrap_Decls :: T_Decls ->
              Inh_Decls ->
              Syn_Decls
wrap_Decls !(T_Decls sem) !(Inh_Decls _lhsIisDeclOfLet _lhsInested _lhsIoptions _lhsIoutputfile) =
    (let ( !_lhsOpps) = sem _lhsIisDeclOfLet _lhsInested _lhsIoptions _lhsIoutputfile
     in  (Syn_Decls _lhsOpps))
sem_Decls_Cons :: T_Decl ->
                  T_Decls ->
                  T_Decls
sem_Decls_Cons !(T_Decl hd_) !(T_Decls tl_) =
    (T_Decls (\ (!_lhsIisDeclOfLet)
                (!_lhsInested)
                (!_lhsIoptions)
                (!_lhsIoutputfile) ->
                  (case (({-# LINE 40 "src-ag/PrintCode.ag" #-}
                          _lhsIoutputfile
                          {-# LINE 1544 "dist/build/PrintCode" #-}
                          )) of
                   { !_tlOoutputfile ->
                   (case (({-# LINE 50 "src-ag/PrintCode.ag" #-}
                           _lhsIoptions
                           {-# LINE 1549 "dist/build/PrintCode" #-}
                           )) of
                    { !_tlOoptions ->
                    (case (({-# LINE 54 "src-ag/PrintCode.ag" #-}
                            _lhsInested
                            {-# LINE 1554 "dist/build/PrintCode" #-}
                            )) of
                     { !_tlOnested ->
                     (case (({-# LINE 407 "src-ag/PrintCode.ag" #-}
                             _lhsIisDeclOfLet
                             {-# LINE 1559 "dist/build/PrintCode" #-}
                             )) of
                      { !_tlOisDeclOfLet ->
                      (case (({-# LINE 40 "src-ag/PrintCode.ag" #-}
                              _lhsIoutputfile
                              {-# LINE 1564 "dist/build/PrintCode" #-}
                              )) of
                       { !_hdOoutputfile ->
                       (case (({-# LINE 50 "src-ag/PrintCode.ag" #-}
                               _lhsIoptions
                               {-# LINE 1569 "dist/build/PrintCode" #-}
                               )) of
                        { !_hdOoptions ->
                        (case (({-# LINE 52 "src-ag/PrintCode.ag" #-}
                                _lhsInested
                                {-# LINE 1574 "dist/build/PrintCode" #-}
                                )) of
                         { !_hdOnested ->
                         (case (({-# LINE 407 "src-ag/PrintCode.ag" #-}
                                 _lhsIisDeclOfLet
                                 {-# LINE 1579 "dist/build/PrintCode" #-}
                                 )) of
                          { !_hdOisDeclOfLet ->
                          (case (tl_ _tlOisDeclOfLet _tlOnested _tlOoptions _tlOoutputfile) of
                           { ( !_tlIpps) ->
                               (case (hd_ _hdOisDeclOfLet _hdOnested _hdOoptions _hdOoutputfile) of
                                { ( !_hdIpp) ->
                                    (case (({-# LINE 85 "src-ag/PrintCode.ag" #-}
                                            _hdIpp : _tlIpps
                                            {-# LINE 1588 "dist/build/PrintCode" #-}
                                            )) of
                                     { !_lhsOpps ->
                                     ( _lhsOpps) }) }) }) }) }) }) }) }) }) }) })))
sem_Decls_Nil :: T_Decls
sem_Decls_Nil =
    (T_Decls (\ (!_lhsIisDeclOfLet)
                (!_lhsInested)
                (!_lhsIoptions)
                (!_lhsIoutputfile) ->
                  (case (({-# LINE 86 "src-ag/PrintCode.ag" #-}
                          []
                          {-# LINE 1600 "dist/build/PrintCode" #-}
                          )) of
                   { !_lhsOpps ->
                   ( _lhsOpps) })))
-- Expr --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         nested               : Bool
         options              : Options
         outputfile           : String
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
         visit 0:
            local addBang     : _
            local strictParams : _
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
         visit 0:
            local addBang     : _
            local strictParams : _
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
newtype T_Expr = T_Expr (Bool ->
                         Options ->
                         String ->
                         ( PP_Doc))
data Inh_Expr = Inh_Expr {nested_Inh_Expr :: !(Bool),options_Inh_Expr :: !(Options),outputfile_Inh_Expr :: !(String)}
data Syn_Expr = Syn_Expr {pp_Syn_Expr :: !(PP_Doc)}
wrap_Expr :: T_Expr ->
             Inh_Expr ->
             Syn_Expr
wrap_Expr !(T_Expr sem) !(Inh_Expr _lhsInested _lhsIoptions _lhsIoutputfile) =
    (let ( !_lhsOpp) = sem _lhsInested _lhsIoptions _lhsIoutputfile
     in  (Syn_Expr _lhsOpp))
sem_Expr_App :: String ->
                T_Exprs ->
                T_Expr
sem_Expr_App !name_ !(T_Exprs args_) =
    (T_Expr (\ (!_lhsInested)
               (!_lhsIoptions)
               (!_lhsIoutputfile) ->
                 (case (({-# LINE 40 "src-ag/PrintCode.ag" #-}
                         _lhsIoutputfile
                         {-# LINE 1732 "dist/build/PrintCode" #-}
                         )) of
                  { !_argsOoutputfile ->
                  (case (({-# LINE 50 "src-ag/PrintCode.ag" #-}
                          _lhsIoptions
                          {-# LINE 1737 "dist/build/PrintCode" #-}
                          )) of
                   { !_argsOoptions ->
                   (case (({-# LINE 54 "src-ag/PrintCode.ag" #-}
                           _lhsInested
                           {-# LINE 1742 "dist/build/PrintCode" #-}
                           )) of
                    { !_argsOnested ->
                    (case (args_ _argsOnested _argsOoptions _argsOoutputfile) of
                     { ( !_argsIpps) ->
                         (case (({-# LINE 161 "src-ag/PrintCode.ag" #-}
                                 pp_parens $ name_ >#< hv_sp _argsIpps
                                 {-# LINE 1749 "dist/build/PrintCode" #-}
                                 )) of
                          { !_lhsOpp ->
                          ( _lhsOpp) }) }) }) }) })))
sem_Expr_Case :: T_Expr ->
                 T_CaseAlts ->
                 T_Expr
sem_Expr_Case !(T_Expr expr_) !(T_CaseAlts alts_) =
    (T_Expr (\ (!_lhsInested)
               (!_lhsIoptions)
               (!_lhsIoutputfile) ->
                 (case (({-# LINE 40 "src-ag/PrintCode.ag" #-}
                         _lhsIoutputfile
                         {-# LINE 1762 "dist/build/PrintCode" #-}
                         )) of
                  { !_altsOoutputfile ->
                  (case (({-# LINE 50 "src-ag/PrintCode.ag" #-}
                          _lhsIoptions
                          {-# LINE 1767 "dist/build/PrintCode" #-}
                          )) of
                   { !_altsOoptions ->
                   (case (({-# LINE 56 "src-ag/PrintCode.ag" #-}
                           _lhsInested
                           {-# LINE 1772 "dist/build/PrintCode" #-}
                           )) of
                    { !_altsOnested ->
                    (case (({-# LINE 40 "src-ag/PrintCode.ag" #-}
                            _lhsIoutputfile
                            {-# LINE 1777 "dist/build/PrintCode" #-}
                            )) of
                     { !_exprOoutputfile ->
                     (case (({-# LINE 50 "src-ag/PrintCode.ag" #-}
                             _lhsIoptions
                             {-# LINE 1782 "dist/build/PrintCode" #-}
                             )) of
                      { !_exprOoptions ->
                      (case (({-# LINE 52 "src-ag/PrintCode.ag" #-}
                              _lhsInested
                              {-# LINE 1787 "dist/build/PrintCode" #-}
                              )) of
                       { !_exprOnested ->
                       (case (alts_ _altsOnested _altsOoptions _altsOoutputfile) of
                        { ( !_altsIpps) ->
                            (case (expr_ _exprOnested _exprOoptions _exprOoutputfile) of
                             { ( !_exprIpp) ->
                                 (case (({-# LINE 144 "src-ag/PrintCode.ag" #-}
                                         pp_parens (    "case" >#< pp_parens _exprIpp >#< "of"
                                                   >-< (vlist _altsIpps)
                                                   )
                                         {-# LINE 1798 "dist/build/PrintCode" #-}
                                         )) of
                                  { !_lhsOpp ->
                                  ( _lhsOpp) }) }) }) }) }) }) }) }) })))
sem_Expr_Do :: T_Decls ->
               T_Expr ->
               T_Expr
sem_Expr_Do !(T_Decls stmts_) !(T_Expr body_) =
    (T_Expr (\ (!_lhsInested)
               (!_lhsIoptions)
               (!_lhsIoutputfile) ->
                 (case (({-# LINE 40 "src-ag/PrintCode.ag" #-}
                         _lhsIoutputfile
                         {-# LINE 1811 "dist/build/PrintCode" #-}
                         )) of
                  { !_bodyOoutputfile ->
                  (case (({-# LINE 50 "src-ag/PrintCode.ag" #-}
                          _lhsIoptions
                          {-# LINE 1816 "dist/build/PrintCode" #-}
                          )) of
                   { !_bodyOoptions ->
                   (case (({-# LINE 52 "src-ag/PrintCode.ag" #-}
                           _lhsInested
                           {-# LINE 1821 "dist/build/PrintCode" #-}
                           )) of
                    { !_bodyOnested ->
                    (case (({-# LINE 40 "src-ag/PrintCode.ag" #-}
                            _lhsIoutputfile
                            {-# LINE 1826 "dist/build/PrintCode" #-}
                            )) of
                     { !_stmtsOoutputfile ->
                     (case (({-# LINE 50 "src-ag/PrintCode.ag" #-}
                             _lhsIoptions
                             {-# LINE 1831 "dist/build/PrintCode" #-}
                             )) of
                      { !_stmtsOoptions ->
                      (case (({-# LINE 54 "src-ag/PrintCode.ag" #-}
                              _lhsInested
                              {-# LINE 1836 "dist/build/PrintCode" #-}
                              )) of
                       { !_stmtsOnested ->
                       (case (({-# LINE 416 "src-ag/PrintCode.ag" #-}
                               False
                               {-# LINE 1841 "dist/build/PrintCode" #-}
                               )) of
                        { !_stmtsOisDeclOfLet ->
                        (case (body_ _bodyOnested _bodyOoptions _bodyOoutputfile) of
                         { ( !_bodyIpp) ->
                             (case (stmts_ _stmtsOisDeclOfLet _stmtsOnested _stmtsOoptions _stmtsOoutputfile) of
                              { ( !_stmtsIpps) ->
                                  (case (({-# LINE 147 "src-ag/PrintCode.ag" #-}
                                          pp_parens ( "do" >#< (   vlist _stmtsIpps
                                                               >-< ("return" >#< _bodyIpp))
                                                    )
                                          {-# LINE 1852 "dist/build/PrintCode" #-}
                                          )) of
                                   { !_lhsOpp ->
                                   ( _lhsOpp) }) }) }) }) }) }) }) }) }) })))
sem_Expr_InvokeExpr :: String ->
                       T_Expr ->
                       T_Exprs ->
                       T_Expr
sem_Expr_InvokeExpr !nt_ !(T_Expr expr_) !(T_Exprs args_) =
    (T_Expr (\ (!_lhsInested)
               (!_lhsIoptions)
               (!_lhsIoutputfile) ->
                 (case (({-# LINE 40 "src-ag/PrintCode.ag" #-}
                         _lhsIoutputfile
                         {-# LINE 1866 "dist/build/PrintCode" #-}
                         )) of
                  { !_argsOoutputfile ->
                  (case (({-# LINE 50 "src-ag/PrintCode.ag" #-}
                          _lhsIoptions
                          {-# LINE 1871 "dist/build/PrintCode" #-}
                          )) of
                   { !_argsOoptions ->
                   (case (({-# LINE 54 "src-ag/PrintCode.ag" #-}
                           _lhsInested
                           {-# LINE 1876 "dist/build/PrintCode" #-}
                           )) of
                    { !_argsOnested ->
                    (case (({-# LINE 40 "src-ag/PrintCode.ag" #-}
                            _lhsIoutputfile
                            {-# LINE 1881 "dist/build/PrintCode" #-}
                            )) of
                     { !_exprOoutputfile ->
                     (case (({-# LINE 50 "src-ag/PrintCode.ag" #-}
                             _lhsIoptions
                             {-# LINE 1886 "dist/build/PrintCode" #-}
                             )) of
                      { !_exprOoptions ->
                      (case (({-# LINE 52 "src-ag/PrintCode.ag" #-}
                              _lhsInested
                              {-# LINE 1891 "dist/build/PrintCode" #-}
                              )) of
                       { !_exprOnested ->
                       (case (args_ _argsOnested _argsOoptions _argsOoutputfile) of
                        { ( !_argsIpps) ->
                            (case (expr_ _exprOnested _exprOoptions _exprOoutputfile) of
                             { ( !_exprIpp) ->
                                 (case (({-# LINE 185 "src-ag/PrintCode.ag" #-}
                                         if breadthFirst _lhsIoptions
                                         then "invoke" >#< pp_parens _exprIpp >#< pp_parens (
                                               nt_ >|< "_Inh" >#< pp_parens (ppTuple False _argsIpps))
                                         else _exprIpp >#< hv_sp _argsIpps
                                         {-# LINE 1903 "dist/build/PrintCode" #-}
                                         )) of
                                  { !_lhsOpp ->
                                  ( _lhsOpp) }) }) }) }) }) }) }) }) })))
sem_Expr_Lambda :: T_Exprs ->
                   T_Expr ->
                   T_Expr
sem_Expr_Lambda !(T_Exprs args_) !(T_Expr body_) =
    (T_Expr (\ (!_lhsInested)
               (!_lhsIoptions)
               (!_lhsIoutputfile) ->
                 (case (({-# LINE 40 "src-ag/PrintCode.ag" #-}
                         _lhsIoutputfile
                         {-# LINE 1916 "dist/build/PrintCode" #-}
                         )) of
                  { !_bodyOoutputfile ->
                  (case (({-# LINE 50 "src-ag/PrintCode.ag" #-}
                          _lhsIoptions
                          {-# LINE 1921 "dist/build/PrintCode" #-}
                          )) of
                   { !_bodyOoptions ->
                   (case (({-# LINE 52 "src-ag/PrintCode.ag" #-}
                           _lhsInested
                           {-# LINE 1926 "dist/build/PrintCode" #-}
                           )) of
                    { !_bodyOnested ->
                    (case (({-# LINE 40 "src-ag/PrintCode.ag" #-}
                            _lhsIoutputfile
                            {-# LINE 1931 "dist/build/PrintCode" #-}
                            )) of
                     { !_argsOoutputfile ->
                     (case (({-# LINE 50 "src-ag/PrintCode.ag" #-}
                             _lhsIoptions
                             {-# LINE 1936 "dist/build/PrintCode" #-}
                             )) of
                      { !_argsOoptions ->
                      (case (({-# LINE 54 "src-ag/PrintCode.ag" #-}
                              _lhsInested
                              {-# LINE 1941 "dist/build/PrintCode" #-}
                              )) of
                       { !_argsOnested ->
                       (case (({-# LINE 153 "src-ag/PrintCode.ag" #-}
                               if bangpats _lhsIoptions
                               then \p -> pp_parens ("!" >|< p)
                               else id
                               {-# LINE 1948 "dist/build/PrintCode" #-}
                               )) of
                        { !_addBang ->
                        (case (args_ _argsOnested _argsOoptions _argsOoutputfile) of
                         { ( !_argsIpps) ->
                             (case (({-# LINE 150 "src-ag/PrintCode.ag" #-}
                                     if strictSems _lhsIoptions
                                     then _argsIpps
                                     else []
                                     {-# LINE 1957 "dist/build/PrintCode" #-}
                                     )) of
                              { !_strictParams ->
                              (case (body_ _bodyOnested _bodyOoptions _bodyOoutputfile) of
                               { ( !_bodyIpp) ->
                                   (case (({-# LINE 156 "src-ag/PrintCode.ag" #-}
                                           pp_parens (    "\\" >#< (vlist (map _addBang     _argsIpps)) >#< "->"
                                                     >-< indent 4 (_strictParams     `ppMultiSeqV` _bodyIpp)
                                                     )
                                           {-# LINE 1966 "dist/build/PrintCode" #-}
                                           )) of
                                    { !_lhsOpp ->
                                    ( _lhsOpp) }) }) }) }) }) }) }) }) }) }) })))
sem_Expr_Let :: T_Decls ->
                T_Expr ->
                T_Expr
sem_Expr_Let !(T_Decls decls_) !(T_Expr body_) =
    (T_Expr (\ (!_lhsInested)
               (!_lhsIoptions)
               (!_lhsIoutputfile) ->
                 (case (({-# LINE 40 "src-ag/PrintCode.ag" #-}
                         _lhsIoutputfile
                         {-# LINE 1979 "dist/build/PrintCode" #-}
                         )) of
                  { !_bodyOoutputfile ->
                  (case (({-# LINE 50 "src-ag/PrintCode.ag" #-}
                          _lhsIoptions
                          {-# LINE 1984 "dist/build/PrintCode" #-}
                          )) of
                   { !_bodyOoptions ->
                   (case (({-# LINE 52 "src-ag/PrintCode.ag" #-}
                           _lhsInested
                           {-# LINE 1989 "dist/build/PrintCode" #-}
                           )) of
                    { !_bodyOnested ->
                    (case (({-# LINE 40 "src-ag/PrintCode.ag" #-}
                            _lhsIoutputfile
                            {-# LINE 1994 "dist/build/PrintCode" #-}
                            )) of
                     { !_declsOoutputfile ->
                     (case (({-# LINE 50 "src-ag/PrintCode.ag" #-}
                             _lhsIoptions
                             {-# LINE 1999 "dist/build/PrintCode" #-}
                             )) of
                      { !_declsOoptions ->
                      (case (({-# LINE 54 "src-ag/PrintCode.ag" #-}
                              _lhsInested
                              {-# LINE 2004 "dist/build/PrintCode" #-}
                              )) of
                       { !_declsOnested ->
                       (case (({-# LINE 414 "src-ag/PrintCode.ag" #-}
                               True
                               {-# LINE 2009 "dist/build/PrintCode" #-}
                               )) of
                        { !_declsOisDeclOfLet ->
                        (case (body_ _bodyOnested _bodyOoptions _bodyOoutputfile) of
                         { ( !_bodyIpp) ->
                             (case (decls_ _declsOisDeclOfLet _declsOnested _declsOoptions _declsOoutputfile) of
                              { ( !_declsIpps) ->
                                  (case (({-# LINE 141 "src-ag/PrintCode.ag" #-}
                                          pp_parens (    "let" >#< (vlist _declsIpps)
                                                    >-< "in " >#< _bodyIpp
                                                    )
                                          {-# LINE 2020 "dist/build/PrintCode" #-}
                                          )) of
                                   { !_lhsOpp ->
                                   ( _lhsOpp) }) }) }) }) }) }) }) }) }) })))
sem_Expr_LineExpr :: T_Expr ->
                     T_Expr
sem_Expr_LineExpr !(T_Expr expr_) =
    (T_Expr (\ (!_lhsInested)
               (!_lhsIoptions)
               (!_lhsIoutputfile) ->
                 (case (({-# LINE 40 "src-ag/PrintCode.ag" #-}
                         _lhsIoutputfile
                         {-# LINE 2032 "dist/build/PrintCode" #-}
                         )) of
                  { !_exprOoutputfile ->
                  (case (({-# LINE 50 "src-ag/PrintCode.ag" #-}
                          _lhsIoptions
                          {-# LINE 2037 "dist/build/PrintCode" #-}
                          )) of
                   { !_exprOoptions ->
                   (case (({-# LINE 52 "src-ag/PrintCode.ag" #-}
                           _lhsInested
                           {-# LINE 2042 "dist/build/PrintCode" #-}
                           )) of
                    { !_exprOnested ->
                    (case (expr_ _exprOnested _exprOoptions _exprOoutputfile) of
                     { ( !_exprIpp) ->
                         (case (({-# LINE 178 "src-ag/PrintCode.ag" #-}
                                 _exprIpp >-< "{-# LINE" >#< ppWithLineNr (\n -> pp $ show $ n + 1) >#< show _lhsIoutputfile >#< "#-}"
                                          >-< ""
                                 {-# LINE 2050 "dist/build/PrintCode" #-}
                                 )) of
                          { !_lhsOpp ->
                          ( _lhsOpp) }) }) }) }) })))
sem_Expr_PragmaExpr :: Bool ->
                       Bool ->
                       String ->
                       T_Expr ->
                       T_Expr
sem_Expr_PragmaExpr !onLeftSide_ !onNewLine_ !txt_ !(T_Expr expr_) =
    (T_Expr (\ (!_lhsInested)
               (!_lhsIoptions)
               (!_lhsIoutputfile) ->
                 (case (({-# LINE 40 "src-ag/PrintCode.ag" #-}
                         _lhsIoutputfile
                         {-# LINE 2065 "dist/build/PrintCode" #-}
                         )) of
                  { !_exprOoutputfile ->
                  (case (({-# LINE 50 "src-ag/PrintCode.ag" #-}
                          _lhsIoptions
                          {-# LINE 2070 "dist/build/PrintCode" #-}
                          )) of
                   { !_exprOoptions ->
                   (case (({-# LINE 52 "src-ag/PrintCode.ag" #-}
                           _lhsInested
                           {-# LINE 2075 "dist/build/PrintCode" #-}
                           )) of
                    { !_exprOnested ->
                    (case (expr_ _exprOnested _exprOoptions _exprOoutputfile) of
                     { ( !_exprIpp) ->
                         (case (({-# LINE 167 "src-ag/PrintCode.ag" #-}
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
                                 {-# LINE 2092 "dist/build/PrintCode" #-}
                                 )) of
                          { !_lhsOpp ->
                          ( _lhsOpp) }) }) }) }) })))
sem_Expr_ResultExpr :: String ->
                       T_Expr ->
                       T_Expr
sem_Expr_ResultExpr !nt_ !(T_Expr expr_) =
    (T_Expr (\ (!_lhsInested)
               (!_lhsIoptions)
               (!_lhsIoutputfile) ->
                 (case (({-# LINE 40 "src-ag/PrintCode.ag" #-}
                         _lhsIoutputfile
                         {-# LINE 2105 "dist/build/PrintCode" #-}
                         )) of
                  { !_exprOoutputfile ->
                  (case (({-# LINE 50 "src-ag/PrintCode.ag" #-}
                          _lhsIoptions
                          {-# LINE 2110 "dist/build/PrintCode" #-}
                          )) of
                   { !_exprOoptions ->
                   (case (({-# LINE 52 "src-ag/PrintCode.ag" #-}
                           _lhsInested
                           {-# LINE 2115 "dist/build/PrintCode" #-}
                           )) of
                    { !_exprOnested ->
                    (case (expr_ _exprOnested _exprOoptions _exprOoutputfile) of
                     { ( !_exprIpp) ->
                         (case (({-# LINE 181 "src-ag/PrintCode.ag" #-}
                                 if breadthFirst _lhsIoptions
                                 then "final" >#<
                                      pp_parens (nt_ >|< "_Syn" >#< pp_parens _exprIpp)
                                 else _exprIpp
                                 {-# LINE 2125 "dist/build/PrintCode" #-}
                                 )) of
                          { !_lhsOpp ->
                          ( _lhsOpp) }) }) }) }) })))
sem_Expr_ResumeExpr :: String ->
                       T_Expr ->
                       T_Lhs ->
                       T_Expr ->
                       T_Expr
sem_Expr_ResumeExpr !nt_ !(T_Expr expr_) !(T_Lhs left_) !(T_Expr rhs_) =
    (T_Expr (\ (!_lhsInested)
               (!_lhsIoptions)
               (!_lhsIoutputfile) ->
                 (case (({-# LINE 40 "src-ag/PrintCode.ag" #-}
                         _lhsIoutputfile
                         {-# LINE 2140 "dist/build/PrintCode" #-}
                         )) of
                  { !_rhsOoutputfile ->
                  (case (({-# LINE 50 "src-ag/PrintCode.ag" #-}
                          _lhsIoptions
                          {-# LINE 2145 "dist/build/PrintCode" #-}
                          )) of
                   { !_rhsOoptions ->
                   (case (({-# LINE 52 "src-ag/PrintCode.ag" #-}
                           _lhsInested
                           {-# LINE 2150 "dist/build/PrintCode" #-}
                           )) of
                    { !_rhsOnested ->
                    (case (({-# LINE 40 "src-ag/PrintCode.ag" #-}
                            _lhsIoutputfile
                            {-# LINE 2155 "dist/build/PrintCode" #-}
                            )) of
                     { !_leftOoutputfile ->
                     (case (({-# LINE 50 "src-ag/PrintCode.ag" #-}
                             _lhsIoptions
                             {-# LINE 2160 "dist/build/PrintCode" #-}
                             )) of
                      { !_leftOoptions ->
                      (case (({-# LINE 52 "src-ag/PrintCode.ag" #-}
                              _lhsInested
                              {-# LINE 2165 "dist/build/PrintCode" #-}
                              )) of
                       { !_leftOnested ->
                       (case (({-# LINE 40 "src-ag/PrintCode.ag" #-}
                               _lhsIoutputfile
                               {-# LINE 2170 "dist/build/PrintCode" #-}
                               )) of
                        { !_exprOoutputfile ->
                        (case (({-# LINE 50 "src-ag/PrintCode.ag" #-}
                                _lhsIoptions
                                {-# LINE 2175 "dist/build/PrintCode" #-}
                                )) of
                         { !_exprOoptions ->
                         (case (({-# LINE 52 "src-ag/PrintCode.ag" #-}
                                 _lhsInested
                                 {-# LINE 2180 "dist/build/PrintCode" #-}
                                 )) of
                          { !_exprOnested ->
                          (case (({-# LINE 418 "src-ag/PrintCode.ag" #-}
                                  False
                                  {-# LINE 2185 "dist/build/PrintCode" #-}
                                  )) of
                           { !_leftOisDeclOfLet ->
                           (case (rhs_ _rhsOnested _rhsOoptions _rhsOoutputfile) of
                            { ( !_rhsIpp) ->
                                (case (left_ _leftOisDeclOfLet _leftOnested _leftOoptions _leftOoutputfile) of
                                 { ( !_leftIpp) ->
                                     (case (expr_ _exprOnested _exprOoptions _exprOoutputfile) of
                                      { ( !_exprIpp) ->
                                          (case (({-# LINE 189 "src-ag/PrintCode.ag" #-}
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
                                                  {-# LINE 2207 "dist/build/PrintCode" #-}
                                                  )) of
                                           { !_lhsOpp ->
                                           ( _lhsOpp) }) }) }) }) }) }) }) }) }) }) }) }) }) })))
sem_Expr_SemFun :: String ->
                   T_Exprs ->
                   T_Expr ->
                   T_Expr
sem_Expr_SemFun !nt_ !(T_Exprs args_) !(T_Expr body_) =
    (T_Expr (\ (!_lhsInested)
               (!_lhsIoptions)
               (!_lhsIoutputfile) ->
                 (case (({-# LINE 40 "src-ag/PrintCode.ag" #-}
                         _lhsIoutputfile
                         {-# LINE 2221 "dist/build/PrintCode" #-}
                         )) of
                  { !_bodyOoutputfile ->
                  (case (({-# LINE 50 "src-ag/PrintCode.ag" #-}
                          _lhsIoptions
                          {-# LINE 2226 "dist/build/PrintCode" #-}
                          )) of
                   { !_bodyOoptions ->
                   (case (({-# LINE 52 "src-ag/PrintCode.ag" #-}
                           _lhsInested
                           {-# LINE 2231 "dist/build/PrintCode" #-}
                           )) of
                    { !_bodyOnested ->
                    (case (({-# LINE 40 "src-ag/PrintCode.ag" #-}
                            _lhsIoutputfile
                            {-# LINE 2236 "dist/build/PrintCode" #-}
                            )) of
                     { !_argsOoutputfile ->
                     (case (({-# LINE 50 "src-ag/PrintCode.ag" #-}
                             _lhsIoptions
                             {-# LINE 2241 "dist/build/PrintCode" #-}
                             )) of
                      { !_argsOoptions ->
                      (case (({-# LINE 54 "src-ag/PrintCode.ag" #-}
                              _lhsInested
                              {-# LINE 2246 "dist/build/PrintCode" #-}
                              )) of
                       { !_argsOnested ->
                       (case (({-# LINE 204 "src-ag/PrintCode.ag" #-}
                               if bangpats _lhsIoptions
                               then \p -> pp_parens ("!" >|< p)
                               else id
                               {-# LINE 2253 "dist/build/PrintCode" #-}
                               )) of
                        { !_addBang ->
                        (case (args_ _argsOnested _argsOoptions _argsOoutputfile) of
                         { ( !_argsIpps) ->
                             (case (({-# LINE 201 "src-ag/PrintCode.ag" #-}
                                     if strictSems _lhsIoptions
                                     then _argsIpps
                                     else []
                                     {-# LINE 2262 "dist/build/PrintCode" #-}
                                     )) of
                              { !_strictParams ->
                              (case (body_ _bodyOnested _bodyOoptions _bodyOoutputfile) of
                               { ( !_bodyIpp) ->
                                   (case (({-# LINE 207 "src-ag/PrintCode.ag" #-}
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
                                           {-# LINE 2278 "dist/build/PrintCode" #-}
                                           )) of
                                    { !_lhsOpp ->
                                    ( _lhsOpp) }) }) }) }) }) }) }) }) }) }) })))
sem_Expr_SimpleExpr :: String ->
                       T_Expr
sem_Expr_SimpleExpr !txt_ =
    (T_Expr (\ (!_lhsInested)
               (!_lhsIoptions)
               (!_lhsIoutputfile) ->
                 (case (({-# LINE 162 "src-ag/PrintCode.ag" #-}
                         text txt_
                         {-# LINE 2290 "dist/build/PrintCode" #-}
                         )) of
                  { !_lhsOpp ->
                  ( _lhsOpp) })))
sem_Expr_TextExpr :: ([String]) ->
                     T_Expr
sem_Expr_TextExpr !lns_ =
    (T_Expr (\ (!_lhsInested)
               (!_lhsIoptions)
               (!_lhsIoutputfile) ->
                 (case (({-# LINE 163 "src-ag/PrintCode.ag" #-}
                         vlist (map text lns_)
                         {-# LINE 2302 "dist/build/PrintCode" #-}
                         )) of
                  { !_lhsOpp ->
                  ( _lhsOpp) })))
sem_Expr_Trace :: String ->
                  T_Expr ->
                  T_Expr
sem_Expr_Trace !txt_ !(T_Expr expr_) =
    (T_Expr (\ (!_lhsInested)
               (!_lhsIoptions)
               (!_lhsIoutputfile) ->
                 (case (({-# LINE 40 "src-ag/PrintCode.ag" #-}
                         _lhsIoutputfile
                         {-# LINE 2315 "dist/build/PrintCode" #-}
                         )) of
                  { !_exprOoutputfile ->
                  (case (({-# LINE 50 "src-ag/PrintCode.ag" #-}
                          _lhsIoptions
                          {-# LINE 2320 "dist/build/PrintCode" #-}
                          )) of
                   { !_exprOoptions ->
                   (case (({-# LINE 52 "src-ag/PrintCode.ag" #-}
                           _lhsInested
                           {-# LINE 2325 "dist/build/PrintCode" #-}
                           )) of
                    { !_exprOnested ->
                    (case (expr_ _exprOnested _exprOoptions _exprOoutputfile) of
                     { ( !_exprIpp) ->
                         (case (({-# LINE 164 "src-ag/PrintCode.ag" #-}
                                 "trace" >#< (   pp_parens ("\"" >|< text txt_ >|< "\"")
                                             >-< pp_parens _exprIpp
                                             )
                                 {-# LINE 2334 "dist/build/PrintCode" #-}
                                 )) of
                          { !_lhsOpp ->
                          ( _lhsOpp) }) }) }) }) })))
sem_Expr_TupleExpr :: T_Exprs ->
                      T_Expr
sem_Expr_TupleExpr !(T_Exprs exprs_) =
    (T_Expr (\ (!_lhsInested)
               (!_lhsIoptions)
               (!_lhsIoutputfile) ->
                 (case (({-# LINE 40 "src-ag/PrintCode.ag" #-}
                         _lhsIoutputfile
                         {-# LINE 2346 "dist/build/PrintCode" #-}
                         )) of
                  { !_exprsOoutputfile ->
                  (case (({-# LINE 50 "src-ag/PrintCode.ag" #-}
                          _lhsIoptions
                          {-# LINE 2351 "dist/build/PrintCode" #-}
                          )) of
                   { !_exprsOoptions ->
                   (case (({-# LINE 54 "src-ag/PrintCode.ag" #-}
                           _lhsInested
                           {-# LINE 2356 "dist/build/PrintCode" #-}
                           )) of
                    { !_exprsOnested ->
                    (case (exprs_ _exprsOnested _exprsOoptions _exprsOoutputfile) of
                     { ( !_exprsIpps) ->
                         (case (({-# LINE 159 "src-ag/PrintCode.ag" #-}
                                 ppTuple _lhsInested _exprsIpps
                                 {-# LINE 2363 "dist/build/PrintCode" #-}
                                 )) of
                          { !_lhsOpp ->
                          ( _lhsOpp) }) }) }) }) })))
sem_Expr_TypedExpr :: T_Expr ->
                      T_Type ->
                      T_Expr
sem_Expr_TypedExpr !(T_Expr expr_) !(T_Type tp_) =
    (T_Expr (\ (!_lhsInested)
               (!_lhsIoptions)
               (!_lhsIoutputfile) ->
                 (case (({-# LINE 52 "src-ag/PrintCode.ag" #-}
                         _lhsInested
                         {-# LINE 2376 "dist/build/PrintCode" #-}
                         )) of
                  { !_tpOnested ->
                  (case (({-# LINE 40 "src-ag/PrintCode.ag" #-}
                          _lhsIoutputfile
                          {-# LINE 2381 "dist/build/PrintCode" #-}
                          )) of
                   { !_exprOoutputfile ->
                   (case (({-# LINE 50 "src-ag/PrintCode.ag" #-}
                           _lhsIoptions
                           {-# LINE 2386 "dist/build/PrintCode" #-}
                           )) of
                    { !_exprOoptions ->
                    (case (({-# LINE 52 "src-ag/PrintCode.ag" #-}
                            _lhsInested
                            {-# LINE 2391 "dist/build/PrintCode" #-}
                            )) of
                     { !_exprOnested ->
                     (case (tp_ _tpOnested) of
                      { ( !_tpIpp,!_tpIprec) ->
                          (case (expr_ _exprOnested _exprOoptions _exprOoutputfile) of
                           { ( !_exprIpp) ->
                               (case (({-# LINE 180 "src-ag/PrintCode.ag" #-}
                                       pp_parens (_exprIpp >#< "::" >#< _tpIpp)
                                       {-# LINE 2400 "dist/build/PrintCode" #-}
                                       )) of
                                { !_lhsOpp ->
                                ( _lhsOpp) }) }) }) }) }) }) })))
sem_Expr_UnboxedTupleExpr :: T_Exprs ->
                             T_Expr
sem_Expr_UnboxedTupleExpr !(T_Exprs exprs_) =
    (T_Expr (\ (!_lhsInested)
               (!_lhsIoptions)
               (!_lhsIoutputfile) ->
                 (case (({-# LINE 40 "src-ag/PrintCode.ag" #-}
                         _lhsIoutputfile
                         {-# LINE 2412 "dist/build/PrintCode" #-}
                         )) of
                  { !_exprsOoutputfile ->
                  (case (({-# LINE 50 "src-ag/PrintCode.ag" #-}
                          _lhsIoptions
                          {-# LINE 2417 "dist/build/PrintCode" #-}
                          )) of
                   { !_exprsOoptions ->
                   (case (({-# LINE 54 "src-ag/PrintCode.ag" #-}
                           _lhsInested
                           {-# LINE 2422 "dist/build/PrintCode" #-}
                           )) of
                    { !_exprsOnested ->
                    (case (exprs_ _exprsOnested _exprsOoptions _exprsOoutputfile) of
                     { ( !_exprsIpps) ->
                         (case (({-# LINE 160 "src-ag/PrintCode.ag" #-}
                                 ppUnboxedTuple _lhsInested _exprsIpps
                                 {-# LINE 2429 "dist/build/PrintCode" #-}
                                 )) of
                          { !_lhsOpp ->
                          ( _lhsOpp) }) }) }) }) })))
-- Exprs -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         nested               : Bool
         options              : Options
         outputfile           : String
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
newtype T_Exprs = T_Exprs (Bool ->
                           Options ->
                           String ->
                           ( PP_Docs))
data Inh_Exprs = Inh_Exprs {nested_Inh_Exprs :: !(Bool),options_Inh_Exprs :: !(Options),outputfile_Inh_Exprs :: !(String)}
data Syn_Exprs = Syn_Exprs {pps_Syn_Exprs :: !(PP_Docs)}
wrap_Exprs :: T_Exprs ->
              Inh_Exprs ->
              Syn_Exprs
wrap_Exprs !(T_Exprs sem) !(Inh_Exprs _lhsInested _lhsIoptions _lhsIoutputfile) =
    (let ( !_lhsOpps) = sem _lhsInested _lhsIoptions _lhsIoutputfile
     in  (Syn_Exprs _lhsOpps))
sem_Exprs_Cons :: T_Expr ->
                  T_Exprs ->
                  T_Exprs
sem_Exprs_Cons !(T_Expr hd_) !(T_Exprs tl_) =
    (T_Exprs (\ (!_lhsInested)
                (!_lhsIoptions)
                (!_lhsIoutputfile) ->
                  (case (({-# LINE 40 "src-ag/PrintCode.ag" #-}
                          _lhsIoutputfile
                          {-# LINE 2475 "dist/build/PrintCode" #-}
                          )) of
                   { !_tlOoutputfile ->
                   (case (({-# LINE 50 "src-ag/PrintCode.ag" #-}
                           _lhsIoptions
                           {-# LINE 2480 "dist/build/PrintCode" #-}
                           )) of
                    { !_tlOoptions ->
                    (case (({-# LINE 54 "src-ag/PrintCode.ag" #-}
                            _lhsInested
                            {-# LINE 2485 "dist/build/PrintCode" #-}
                            )) of
                     { !_tlOnested ->
                     (case (({-# LINE 40 "src-ag/PrintCode.ag" #-}
                             _lhsIoutputfile
                             {-# LINE 2490 "dist/build/PrintCode" #-}
                             )) of
                      { !_hdOoutputfile ->
                      (case (({-# LINE 50 "src-ag/PrintCode.ag" #-}
                              _lhsIoptions
                              {-# LINE 2495 "dist/build/PrintCode" #-}
                              )) of
                       { !_hdOoptions ->
                       (case (({-# LINE 52 "src-ag/PrintCode.ag" #-}
                               _lhsInested
                               {-# LINE 2500 "dist/build/PrintCode" #-}
                               )) of
                        { !_hdOnested ->
                        (case (tl_ _tlOnested _tlOoptions _tlOoutputfile) of
                         { ( !_tlIpps) ->
                             (case (hd_ _hdOnested _hdOoptions _hdOoutputfile) of
                              { ( !_hdIpp) ->
                                  (case (({-# LINE 65 "src-ag/PrintCode.ag" #-}
                                          _hdIpp : _tlIpps
                                          {-# LINE 2509 "dist/build/PrintCode" #-}
                                          )) of
                                   { !_lhsOpps ->
                                   ( _lhsOpps) }) }) }) }) }) }) }) }) })))
sem_Exprs_Nil :: T_Exprs
sem_Exprs_Nil =
    (T_Exprs (\ (!_lhsInested)
                (!_lhsIoptions)
                (!_lhsIoutputfile) ->
                  (case (({-# LINE 66 "src-ag/PrintCode.ag" #-}
                          []
                          {-# LINE 2520 "dist/build/PrintCode" #-}
                          )) of
                   { !_lhsOpps ->
                   ( _lhsOpps) })))
-- Lhs ---------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         isDeclOfLet          : Bool
         nested               : Bool
         options              : Options
         outputfile           : String
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative Fun:
         child name           : {String}
         child args           : Exprs 
         visit 0:
            local addBang     : _
            local strictGuard : _
            local hasStrictVars : _
            local addStrictGuard : _
      alternative Pattern3:
         child pat3           : Pattern 
         visit 0:
            local hasStrictVars : _
            local strictGuard : _
            local addStrictGuard : _
      alternative Pattern3SM:
         child pat3           : Pattern 
      alternative TupleLhs:
         child comps          : {[String]}
         visit 0:
            local addBang     : _
            local hasStrictVars : _
            local strictGuard : _
            local addStrictGuard : _
      alternative UnboxedTupleLhs:
         child comps          : {[String]}
         visit 0:
            local addBang     : _
            local hasStrictVars : _
            local strictGuard : _
            local addStrictGuard : _
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
newtype T_Lhs = T_Lhs (Bool ->
                       Bool ->
                       Options ->
                       String ->
                       ( PP_Doc))
data Inh_Lhs = Inh_Lhs {isDeclOfLet_Inh_Lhs :: !(Bool),nested_Inh_Lhs :: !(Bool),options_Inh_Lhs :: !(Options),outputfile_Inh_Lhs :: !(String)}
data Syn_Lhs = Syn_Lhs {pp_Syn_Lhs :: !(PP_Doc)}
wrap_Lhs :: T_Lhs ->
            Inh_Lhs ->
            Syn_Lhs
wrap_Lhs !(T_Lhs sem) !(Inh_Lhs _lhsIisDeclOfLet _lhsInested _lhsIoptions _lhsIoutputfile) =
    (let ( !_lhsOpp) = sem _lhsIisDeclOfLet _lhsInested _lhsIoptions _lhsIoutputfile
     in  (Syn_Lhs _lhsOpp))
sem_Lhs_Fun :: String ->
               T_Exprs ->
               T_Lhs
sem_Lhs_Fun !name_ !(T_Exprs args_) =
    (T_Lhs (\ (!_lhsIisDeclOfLet)
              (!_lhsInested)
              (!_lhsIoptions)
              (!_lhsIoutputfile) ->
                (case (({-# LINE 40 "src-ag/PrintCode.ag" #-}
                        _lhsIoutputfile
                        {-# LINE 2608 "dist/build/PrintCode" #-}
                        )) of
                 { !_argsOoutputfile ->
                 (case (({-# LINE 50 "src-ag/PrintCode.ag" #-}
                         _lhsIoptions
                         {-# LINE 2613 "dist/build/PrintCode" #-}
                         )) of
                  { !_argsOoptions ->
                  (case (({-# LINE 54 "src-ag/PrintCode.ag" #-}
                          _lhsInested
                          {-# LINE 2618 "dist/build/PrintCode" #-}
                          )) of
                   { !_argsOnested ->
                   (case (({-# LINE 248 "src-ag/PrintCode.ag" #-}
                           if bangpats _lhsIoptions
                                    then \p -> "!" >|< p
                                    else id
                           {-# LINE 2625 "dist/build/PrintCode" #-}
                           )) of
                    { !_addBang ->
                    (case (args_ _argsOnested _argsOoptions _argsOoutputfile) of
                     { ( !_argsIpps) ->
                         (case (({-# LINE 245 "src-ag/PrintCode.ag" #-}
                                 _argsIpps `ppMultiSeqH` (pp "True")
                                 {-# LINE 2632 "dist/build/PrintCode" #-}
                                 )) of
                          { !_strictGuard ->
                          (case (({-# LINE 244 "src-ag/PrintCode.ag" #-}
                                  not (null _argsIpps)
                                  {-# LINE 2637 "dist/build/PrintCode" #-}
                                  )) of
                           { !_hasStrictVars ->
                           (case (({-# LINE 243 "src-ag/PrintCode.ag" #-}
                                   if strictSems _lhsIoptions && _hasStrictVars     then \v -> v >#< "|" >#< _strictGuard     else id
                                   {-# LINE 2642 "dist/build/PrintCode" #-}
                                   )) of
                            { !_addStrictGuard ->
                            (case (({-# LINE 256 "src-ag/PrintCode.ag" #-}
                                    _addStrictGuard     (name_ >#< hv_sp (map _addBang     _argsIpps))
                                    {-# LINE 2647 "dist/build/PrintCode" #-}
                                    )) of
                             { !_lhsOpp ->
                             ( _lhsOpp) }) }) }) }) }) }) }) }) })))
sem_Lhs_Pattern3 :: T_Pattern ->
                    T_Lhs
sem_Lhs_Pattern3 !(T_Pattern pat3_) =
    (T_Lhs (\ (!_lhsIisDeclOfLet)
              (!_lhsInested)
              (!_lhsIoptions)
              (!_lhsIoutputfile) ->
                (case (({-# LINE 50 "src-ag/PrintCode.ag" #-}
                        _lhsIoptions
                        {-# LINE 2660 "dist/build/PrintCode" #-}
                        )) of
                 { !_pat3Ooptions ->
                 (case (({-# LINE 407 "src-ag/PrintCode.ag" #-}
                         _lhsIisDeclOfLet
                         {-# LINE 2665 "dist/build/PrintCode" #-}
                         )) of
                  { !_pat3OisDeclOfLet ->
                  (case (({-# LINE 380 "src-ag/PrintCode.ag" #-}
                          False
                          {-# LINE 2670 "dist/build/PrintCode" #-}
                          )) of
                   { !_pat3ObelowIrrefutable ->
                   (case (pat3_ _pat3ObelowIrrefutable _pat3OisDeclOfLet _pat3Ooptions) of
                    { ( !_pat3Icopy,!_pat3IisUnderscore,!_pat3Ipp,!_pat3Ipp',!_pat3IstrictVars) ->
                        (case (({-# LINE 235 "src-ag/PrintCode.ag" #-}
                                not (null _pat3IstrictVars)
                                {-# LINE 2677 "dist/build/PrintCode" #-}
                                )) of
                         { !_hasStrictVars ->
                         (case (({-# LINE 234 "src-ag/PrintCode.ag" #-}
                                 _pat3IstrictVars `ppMultiSeqH` (pp "True")
                                 {-# LINE 2682 "dist/build/PrintCode" #-}
                                 )) of
                          { !_strictGuard ->
                          (case (({-# LINE 232 "src-ag/PrintCode.ag" #-}
                                  if strictCases _lhsIoptions && _hasStrictVars     then \v -> v >#< "|" >#< _strictGuard     else id
                                  {-# LINE 2687 "dist/build/PrintCode" #-}
                                  )) of
                           { !_addStrictGuard ->
                           (case (({-# LINE 252 "src-ag/PrintCode.ag" #-}
                                   _addStrictGuard     _pat3Ipp
                                   {-# LINE 2692 "dist/build/PrintCode" #-}
                                   )) of
                            { !_lhsOpp ->
                            ( _lhsOpp) }) }) }) }) }) }) }) })))
sem_Lhs_Pattern3SM :: T_Pattern ->
                      T_Lhs
sem_Lhs_Pattern3SM !(T_Pattern pat3_) =
    (T_Lhs (\ (!_lhsIisDeclOfLet)
              (!_lhsInested)
              (!_lhsIoptions)
              (!_lhsIoutputfile) ->
                (case (({-# LINE 50 "src-ag/PrintCode.ag" #-}
                        _lhsIoptions
                        {-# LINE 2705 "dist/build/PrintCode" #-}
                        )) of
                 { !_pat3Ooptions ->
                 (case (({-# LINE 407 "src-ag/PrintCode.ag" #-}
                         _lhsIisDeclOfLet
                         {-# LINE 2710 "dist/build/PrintCode" #-}
                         )) of
                  { !_pat3OisDeclOfLet ->
                  (case (({-# LINE 380 "src-ag/PrintCode.ag" #-}
                          False
                          {-# LINE 2715 "dist/build/PrintCode" #-}
                          )) of
                   { !_pat3ObelowIrrefutable ->
                   (case (pat3_ _pat3ObelowIrrefutable _pat3OisDeclOfLet _pat3Ooptions) of
                    { ( !_pat3Icopy,!_pat3IisUnderscore,!_pat3Ipp,!_pat3Ipp',!_pat3IstrictVars) ->
                        (case (({-# LINE 253 "src-ag/PrintCode.ag" #-}
                                _pat3Ipp'
                                {-# LINE 2722 "dist/build/PrintCode" #-}
                                )) of
                         { !_lhsOpp ->
                         ( _lhsOpp) }) }) }) }) })))
sem_Lhs_TupleLhs :: ([String]) ->
                    T_Lhs
sem_Lhs_TupleLhs !comps_ =
    (T_Lhs (\ (!_lhsIisDeclOfLet)
              (!_lhsInested)
              (!_lhsIoptions)
              (!_lhsIoutputfile) ->
                (case (({-# LINE 248 "src-ag/PrintCode.ag" #-}
                        if bangpats _lhsIoptions
                                 then \p -> "!" >|< p
                                 else id
                        {-# LINE 2737 "dist/build/PrintCode" #-}
                        )) of
                 { !_addBang ->
                 (case (({-# LINE 240 "src-ag/PrintCode.ag" #-}
                         not (null comps_)
                         {-# LINE 2742 "dist/build/PrintCode" #-}
                         )) of
                  { !_hasStrictVars ->
                  (case (({-# LINE 237 "src-ag/PrintCode.ag" #-}
                          if stricterCases _lhsIoptions && not _lhsIisDeclOfLet
                          then map text comps_ `ppMultiSeqH` (pp "True")
                          else pp "True"
                          {-# LINE 2749 "dist/build/PrintCode" #-}
                          )) of
                   { !_strictGuard ->
                   (case (({-# LINE 232 "src-ag/PrintCode.ag" #-}
                           if strictCases _lhsIoptions && _hasStrictVars     then \v -> v >#< "|" >#< _strictGuard     else id
                           {-# LINE 2754 "dist/build/PrintCode" #-}
                           )) of
                    { !_addStrictGuard ->
                    (case (({-# LINE 254 "src-ag/PrintCode.ag" #-}
                            _addStrictGuard     $ ppTuple _lhsInested (map (_addBang     . text) comps_)
                            {-# LINE 2759 "dist/build/PrintCode" #-}
                            )) of
                     { !_lhsOpp ->
                     ( _lhsOpp) }) }) }) }) })))
sem_Lhs_UnboxedTupleLhs :: ([String]) ->
                           T_Lhs
sem_Lhs_UnboxedTupleLhs !comps_ =
    (T_Lhs (\ (!_lhsIisDeclOfLet)
              (!_lhsInested)
              (!_lhsIoptions)
              (!_lhsIoutputfile) ->
                (case (({-# LINE 248 "src-ag/PrintCode.ag" #-}
                        if bangpats _lhsIoptions
                                 then \p -> "!" >|< p
                                 else id
                        {-# LINE 2774 "dist/build/PrintCode" #-}
                        )) of
                 { !_addBang ->
                 (case (({-# LINE 240 "src-ag/PrintCode.ag" #-}
                         not (null comps_)
                         {-# LINE 2779 "dist/build/PrintCode" #-}
                         )) of
                  { !_hasStrictVars ->
                  (case (({-# LINE 237 "src-ag/PrintCode.ag" #-}
                          if stricterCases _lhsIoptions && not _lhsIisDeclOfLet
                          then map text comps_ `ppMultiSeqH` (pp "True")
                          else pp "True"
                          {-# LINE 2786 "dist/build/PrintCode" #-}
                          )) of
                   { !_strictGuard ->
                   (case (({-# LINE 232 "src-ag/PrintCode.ag" #-}
                           if strictCases _lhsIoptions && _hasStrictVars     then \v -> v >#< "|" >#< _strictGuard     else id
                           {-# LINE 2791 "dist/build/PrintCode" #-}
                           )) of
                    { !_addStrictGuard ->
                    (case (({-# LINE 255 "src-ag/PrintCode.ag" #-}
                            _addStrictGuard     $ ppUnboxedTuple _lhsInested (map (_addBang     . text) comps_)
                            {-# LINE 2796 "dist/build/PrintCode" #-}
                            )) of
                     { !_lhsOpp ->
                     ( _lhsOpp) }) }) }) }) })))
sem_Lhs_Unwrap :: String ->
                  T_Lhs ->
                  T_Lhs
sem_Lhs_Unwrap !name_ !(T_Lhs sub_) =
    (T_Lhs (\ (!_lhsIisDeclOfLet)
              (!_lhsInested)
              (!_lhsIoptions)
              (!_lhsIoutputfile) ->
                (case (({-# LINE 40 "src-ag/PrintCode.ag" #-}
                        _lhsIoutputfile
                        {-# LINE 2810 "dist/build/PrintCode" #-}
                        )) of
                 { !_subOoutputfile ->
                 (case (({-# LINE 50 "src-ag/PrintCode.ag" #-}
                         _lhsIoptions
                         {-# LINE 2815 "dist/build/PrintCode" #-}
                         )) of
                  { !_subOoptions ->
                  (case (({-# LINE 52 "src-ag/PrintCode.ag" #-}
                          _lhsInested
                          {-# LINE 2820 "dist/build/PrintCode" #-}
                          )) of
                   { !_subOnested ->
                   (case (({-# LINE 407 "src-ag/PrintCode.ag" #-}
                           _lhsIisDeclOfLet
                           {-# LINE 2825 "dist/build/PrintCode" #-}
                           )) of
                    { !_subOisDeclOfLet ->
                    (case (sub_ _subOisDeclOfLet _subOnested _subOoptions _subOoutputfile) of
                     { ( !_subIpp) ->
                         (case (({-# LINE 257 "src-ag/PrintCode.ag" #-}
                                 pp_parens (name_ >#< _subIpp)
                                 {-# LINE 2832 "dist/build/PrintCode" #-}
                                 )) of
                          { !_lhsOpp ->
                          ( _lhsOpp) }) }) }) }) }) })))
-- NamedType ---------------------------------------------------
{-
   visit 0:
      inherited attribute:
         nested               : Bool
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
newtype T_NamedType = T_NamedType (Bool ->
                                   ( PP_Doc))
data Inh_NamedType = Inh_NamedType {nested_Inh_NamedType :: !(Bool)}
data Syn_NamedType = Syn_NamedType {pp_Syn_NamedType :: !(PP_Doc)}
wrap_NamedType :: T_NamedType ->
                  Inh_NamedType ->
                  Syn_NamedType
wrap_NamedType !(T_NamedType sem) !(Inh_NamedType _lhsInested) =
    (let ( !_lhsOpp) = sem _lhsInested
     in  (Syn_NamedType _lhsOpp))
sem_NamedType_Named :: Bool ->
                       String ->
                       T_Type ->
                       T_NamedType
sem_NamedType_Named !strict_ !name_ !(T_Type tp_) =
    (T_NamedType (\ (!_lhsInested) ->
                      (case (({-# LINE 52 "src-ag/PrintCode.ag" #-}
                              _lhsInested
                              {-# LINE 2873 "dist/build/PrintCode" #-}
                              )) of
                       { !_tpOnested ->
                       (case (tp_ _tpOnested) of
                        { ( !_tpIpp,!_tpIprec) ->
                            (case (({-# LINE 226 "src-ag/PrintCode.ag" #-}
                                    if strict_
                                    then name_ >#< "::" >#< "!" >|< pp_parens _tpIpp
                                    else name_ >#< "::" >#< _tpIpp
                                    {-# LINE 2882 "dist/build/PrintCode" #-}
                                    )) of
                             { !_lhsOpp ->
                             ( _lhsOpp) }) }) })))
-- NamedTypes --------------------------------------------------
{-
   visit 0:
      inherited attribute:
         nested               : Bool
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
newtype T_NamedTypes = T_NamedTypes (Bool ->
                                     ( PP_Docs))
data Inh_NamedTypes = Inh_NamedTypes {nested_Inh_NamedTypes :: !(Bool)}
data Syn_NamedTypes = Syn_NamedTypes {pps_Syn_NamedTypes :: !(PP_Docs)}
wrap_NamedTypes :: T_NamedTypes ->
                   Inh_NamedTypes ->
                   Syn_NamedTypes
wrap_NamedTypes !(T_NamedTypes sem) !(Inh_NamedTypes _lhsInested) =
    (let ( !_lhsOpps) = sem _lhsInested
     in  (Syn_NamedTypes _lhsOpps))
sem_NamedTypes_Cons :: T_NamedType ->
                       T_NamedTypes ->
                       T_NamedTypes
sem_NamedTypes_Cons !(T_NamedType hd_) !(T_NamedTypes tl_) =
    (T_NamedTypes (\ (!_lhsInested) ->
                       (case (({-# LINE 54 "src-ag/PrintCode.ag" #-}
                               _lhsInested
                               {-# LINE 2922 "dist/build/PrintCode" #-}
                               )) of
                        { !_tlOnested ->
                        (case (({-# LINE 52 "src-ag/PrintCode.ag" #-}
                                _lhsInested
                                {-# LINE 2927 "dist/build/PrintCode" #-}
                                )) of
                         { !_hdOnested ->
                         (case (tl_ _tlOnested) of
                          { ( !_tlIpps) ->
                              (case (hd_ _hdOnested) of
                               { ( !_hdIpp) ->
                                   (case (({-# LINE 81 "src-ag/PrintCode.ag" #-}
                                           _hdIpp : _tlIpps
                                           {-# LINE 2936 "dist/build/PrintCode" #-}
                                           )) of
                                    { !_lhsOpps ->
                                    ( _lhsOpps) }) }) }) }) })))
sem_NamedTypes_Nil :: T_NamedTypes
sem_NamedTypes_Nil =
    (T_NamedTypes (\ (!_lhsInested) ->
                       (case (({-# LINE 82 "src-ag/PrintCode.ag" #-}
                               []
                               {-# LINE 2945 "dist/build/PrintCode" #-}
                               )) of
                        { !_lhsOpps ->
                        ( _lhsOpps) })))
-- Pattern -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         belowIrrefutable     : Bool
         isDeclOfLet          : Bool
         options              : Options
      synthesized attributes:
         copy                 : Pattern 
         isUnderscore         : Bool
         pp                   : PP_Doc
         pp'                  : PP_Doc
         strictVars           : [PP_Doc]
   alternatives:
      alternative Alias:
         child field          : {Identifier}
         child attr           : {Identifier}
         child pat            : Pattern 
         visit 0:
            local copy        : _
            local ppVar       : _
            local addBang     : _
            local ppVarBang   : _
            local strictPatVars : _
            local strictVar   : _
      alternative Constr:
         child name           : {ConstructorIdent}
         child pats           : Patterns 
         visit 0:
            local copy        : _
            local addBang     : _
      alternative Irrefutable:
         child pat            : Pattern 
         visit 0:
            local copy        : _
      alternative Product:
         child pos            : {Pos}
         child pats           : Patterns 
         visit 0:
            local copy        : _
            local addBang     : _
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
newtype T_Pattern = T_Pattern (Bool ->
                               Bool ->
                               Options ->
                               ( Pattern,Bool,PP_Doc,PP_Doc,([PP_Doc])))
data Inh_Pattern = Inh_Pattern {belowIrrefutable_Inh_Pattern :: !(Bool),isDeclOfLet_Inh_Pattern :: !(Bool),options_Inh_Pattern :: !(Options)}
data Syn_Pattern = Syn_Pattern {copy_Syn_Pattern :: !(Pattern),isUnderscore_Syn_Pattern :: !(Bool),pp_Syn_Pattern :: !(PP_Doc),pp'_Syn_Pattern :: !(PP_Doc),strictVars_Syn_Pattern :: !(([PP_Doc]))}
wrap_Pattern :: T_Pattern ->
                Inh_Pattern ->
                Syn_Pattern
wrap_Pattern !(T_Pattern sem) !(Inh_Pattern _lhsIbelowIrrefutable _lhsIisDeclOfLet _lhsIoptions) =
    (let ( !_lhsOcopy,!_lhsOisUnderscore,!_lhsOpp,!_lhsOpp',!_lhsOstrictVars) = sem _lhsIbelowIrrefutable _lhsIisDeclOfLet _lhsIoptions
     in  (Syn_Pattern _lhsOcopy _lhsOisUnderscore _lhsOpp _lhsOpp' _lhsOstrictVars))
sem_Pattern_Alias :: Identifier ->
                     Identifier ->
                     T_Pattern ->
                     T_Pattern
sem_Pattern_Alias !field_ !attr_ !(T_Pattern pat_) =
    (T_Pattern (\ (!_lhsIbelowIrrefutable)
                  (!_lhsIisDeclOfLet)
                  (!_lhsIoptions) ->
                    (case (({-# LINE 50 "src-ag/PrintCode.ag" #-}
                            _lhsIoptions
                            {-# LINE 3031 "dist/build/PrintCode" #-}
                            )) of
                     { !_patOoptions ->
                     (case (({-# LINE 407 "src-ag/PrintCode.ag" #-}
                             _lhsIisDeclOfLet
                             {-# LINE 3036 "dist/build/PrintCode" #-}
                             )) of
                      { !_patOisDeclOfLet ->
                      (case (({-# LINE 373 "src-ag/PrintCode.ag" #-}
                              _lhsIbelowIrrefutable
                              {-# LINE 3041 "dist/build/PrintCode" #-}
                              )) of
                       { !_patObelowIrrefutable ->
                       (case (pat_ _patObelowIrrefutable _patOisDeclOfLet _patOoptions) of
                        { ( !_patIcopy,!_patIisUnderscore,!_patIpp,!_patIpp',!_patIstrictVars) ->
                            (case (({-# LINE 22 "src-ag/Patterns.ag" #-}
                                    Alias field_ attr_ _patIcopy
                                    {-# LINE 3048 "dist/build/PrintCode" #-}
                                    )) of
                             { !_copy ->
                             (case (({-# LINE 22 "src-ag/Patterns.ag" #-}
                                     _copy
                                     {-# LINE 3053 "dist/build/PrintCode" #-}
                                     )) of
                              { !_lhsOcopy ->
                              (case (({-# LINE 370 "src-ag/PrintCode.ag" #-}
                                      False
                                      {-# LINE 3058 "dist/build/PrintCode" #-}
                                      )) of
                               { !_lhsOisUnderscore ->
                               (case (({-# LINE 359 "src-ag/PrintCode.ag" #-}
                                       pp (attrname False field_ attr_)
                                       {-# LINE 3063 "dist/build/PrintCode" #-}
                                       )) of
                                { !_ppVar ->
                                (case (({-# LINE 352 "src-ag/PrintCode.ag" #-}
                                        if bangpats _lhsIoptions && not _lhsIisDeclOfLet && not _lhsIbelowIrrefutable
                                        then \p -> "!" >|< p
                                        else id
                                        {-# LINE 3070 "dist/build/PrintCode" #-}
                                        )) of
                                 { !_addBang ->
                                 (case (({-# LINE 360 "src-ag/PrintCode.ag" #-}
                                         _addBang     $ _ppVar
                                         {-# LINE 3075 "dist/build/PrintCode" #-}
                                         )) of
                                  { !_ppVarBang ->
                                  (case (({-# LINE 361 "src-ag/PrintCode.ag" #-}
                                          if _patIisUnderscore
                                           then _ppVarBang
                                           else _ppVarBang     >|< "@" >|< _patIpp
                                          {-# LINE 3082 "dist/build/PrintCode" #-}
                                          )) of
                                   { !_lhsOpp ->
                                   (case (({-# LINE 393 "src-ag/PrintCode.ag" #-}
                                           let attribute | field_ == _LOC || field_ == nullIdent = locname' attr_
                                                         | otherwise                             = attrname False field_ attr_
                                           in attribute >|< "@" >|< _patIpp'
                                           {-# LINE 3089 "dist/build/PrintCode" #-}
                                           )) of
                                    { !_lhsOpp' ->
                                    (case (({-# LINE 333 "src-ag/PrintCode.ag" #-}
                                            if stricterCases _lhsIoptions && not _lhsIisDeclOfLet
                                            then _patIstrictVars
                                            else []
                                            {-# LINE 3096 "dist/build/PrintCode" #-}
                                            )) of
                                     { !_strictPatVars ->
                                     (case (({-# LINE 329 "src-ag/PrintCode.ag" #-}
                                             if strictCases _lhsIoptions && not _lhsIisDeclOfLet
                                             then [_ppVar    ]
                                             else []
                                             {-# LINE 3103 "dist/build/PrintCode" #-}
                                             )) of
                                      { !_strictVar ->
                                      (case (({-# LINE 337 "src-ag/PrintCode.ag" #-}
                                              _strictVar     ++ _strictPatVars
                                              {-# LINE 3108 "dist/build/PrintCode" #-}
                                              )) of
                                       { !_lhsOstrictVars ->
                                       ( _lhsOcopy,_lhsOisUnderscore,_lhsOpp,_lhsOpp',_lhsOstrictVars) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })))
sem_Pattern_Constr :: ConstructorIdent ->
                      T_Patterns ->
                      T_Pattern
sem_Pattern_Constr !name_ !(T_Patterns pats_) =
    (T_Pattern (\ (!_lhsIbelowIrrefutable)
                  (!_lhsIisDeclOfLet)
                  (!_lhsIoptions) ->
                    (case (({-# LINE 50 "src-ag/PrintCode.ag" #-}
                            _lhsIoptions
                            {-# LINE 3121 "dist/build/PrintCode" #-}
                            )) of
                     { !_patsOoptions ->
                     (case (({-# LINE 407 "src-ag/PrintCode.ag" #-}
                             _lhsIisDeclOfLet
                             {-# LINE 3126 "dist/build/PrintCode" #-}
                             )) of
                      { !_patsOisDeclOfLet ->
                      (case (({-# LINE 373 "src-ag/PrintCode.ag" #-}
                              _lhsIbelowIrrefutable
                              {-# LINE 3131 "dist/build/PrintCode" #-}
                              )) of
                       { !_patsObelowIrrefutable ->
                       (case (pats_ _patsObelowIrrefutable _patsOisDeclOfLet _patsOoptions) of
                        { ( !_patsIcopy,!_patsIpps,!_patsIpps',!_patsIstrictVars) ->
                            (case (({-# LINE 22 "src-ag/Patterns.ag" #-}
                                    Constr name_ _patsIcopy
                                    {-# LINE 3138 "dist/build/PrintCode" #-}
                                    )) of
                             { !_copy ->
                             (case (({-# LINE 22 "src-ag/Patterns.ag" #-}
                                     _copy
                                     {-# LINE 3143 "dist/build/PrintCode" #-}
                                     )) of
                              { !_lhsOcopy ->
                              (case (({-# LINE 368 "src-ag/PrintCode.ag" #-}
                                      False
                                      {-# LINE 3148 "dist/build/PrintCode" #-}
                                      )) of
                               { !_lhsOisUnderscore ->
                               (case (({-# LINE 352 "src-ag/PrintCode.ag" #-}
                                       if bangpats _lhsIoptions && not _lhsIisDeclOfLet && not _lhsIbelowIrrefutable
                                       then \p -> "!" >|< p
                                       else id
                                       {-# LINE 3155 "dist/build/PrintCode" #-}
                                       )) of
                                { !_addBang ->
                                (case (({-# LINE 357 "src-ag/PrintCode.ag" #-}
                                        _addBang     $ pp_parens $ name_ >#< hv_sp _patsIpps
                                        {-# LINE 3160 "dist/build/PrintCode" #-}
                                        )) of
                                 { !_lhsOpp ->
                                 (case (({-# LINE 391 "src-ag/PrintCode.ag" #-}
                                         pp_parens $ name_ >#< hv_sp (map pp_parens _patsIpps')
                                         {-# LINE 3165 "dist/build/PrintCode" #-}
                                         )) of
                                  { !_lhsOpp' ->
                                  (case (({-# LINE 326 "src-ag/PrintCode.ag" #-}
                                          _patsIstrictVars
                                          {-# LINE 3170 "dist/build/PrintCode" #-}
                                          )) of
                                   { !_lhsOstrictVars ->
                                   ( _lhsOcopy,_lhsOisUnderscore,_lhsOpp,_lhsOpp',_lhsOstrictVars) }) }) }) }) }) }) }) }) }) }) })))
sem_Pattern_Irrefutable :: T_Pattern ->
                           T_Pattern
sem_Pattern_Irrefutable !(T_Pattern pat_) =
    (T_Pattern (\ (!_lhsIbelowIrrefutable)
                  (!_lhsIisDeclOfLet)
                  (!_lhsIoptions) ->
                    (case (({-# LINE 50 "src-ag/PrintCode.ag" #-}
                            _lhsIoptions
                            {-# LINE 3182 "dist/build/PrintCode" #-}
                            )) of
                     { !_patOoptions ->
                     (case (({-# LINE 407 "src-ag/PrintCode.ag" #-}
                             _lhsIisDeclOfLet
                             {-# LINE 3187 "dist/build/PrintCode" #-}
                             )) of
                      { !_patOisDeclOfLet ->
                      (case (({-# LINE 376 "src-ag/PrintCode.ag" #-}
                              True
                              {-# LINE 3192 "dist/build/PrintCode" #-}
                              )) of
                       { !_patObelowIrrefutable ->
                       (case (pat_ _patObelowIrrefutable _patOisDeclOfLet _patOoptions) of
                        { ( !_patIcopy,!_patIisUnderscore,!_patIpp,!_patIpp',!_patIstrictVars) ->
                            (case (({-# LINE 22 "src-ag/Patterns.ag" #-}
                                    Irrefutable _patIcopy
                                    {-# LINE 3199 "dist/build/PrintCode" #-}
                                    )) of
                             { !_copy ->
                             (case (({-# LINE 22 "src-ag/Patterns.ag" #-}
                                     _copy
                                     {-# LINE 3204 "dist/build/PrintCode" #-}
                                     )) of
                              { !_lhsOcopy ->
                              (case (({-# LINE 367 "src-ag/PrintCode.ag" #-}
                                      _patIisUnderscore
                                      {-# LINE 3209 "dist/build/PrintCode" #-}
                                      )) of
                               { !_lhsOisUnderscore ->
                               (case (({-# LINE 364 "src-ag/PrintCode.ag" #-}
                                       text "~" >|< pp_parens _patIpp
                                       {-# LINE 3214 "dist/build/PrintCode" #-}
                                       )) of
                                { !_lhsOpp ->
                                (case (({-# LINE 396 "src-ag/PrintCode.ag" #-}
                                        text "~" >|< pp_parens _patIpp
                                        {-# LINE 3219 "dist/build/PrintCode" #-}
                                        )) of
                                 { !_lhsOpp' ->
                                 (case (({-# LINE 340 "src-ag/PrintCode.ag" #-}
                                         []
                                         {-# LINE 3224 "dist/build/PrintCode" #-}
                                         )) of
                                  { !_lhsOstrictVars ->
                                  ( _lhsOcopy,_lhsOisUnderscore,_lhsOpp,_lhsOpp',_lhsOstrictVars) }) }) }) }) }) }) }) }) }) })))
sem_Pattern_Product :: Pos ->
                       T_Patterns ->
                       T_Pattern
sem_Pattern_Product !pos_ !(T_Patterns pats_) =
    (T_Pattern (\ (!_lhsIbelowIrrefutable)
                  (!_lhsIisDeclOfLet)
                  (!_lhsIoptions) ->
                    (case (({-# LINE 50 "src-ag/PrintCode.ag" #-}
                            _lhsIoptions
                            {-# LINE 3237 "dist/build/PrintCode" #-}
                            )) of
                     { !_patsOoptions ->
                     (case (({-# LINE 407 "src-ag/PrintCode.ag" #-}
                             _lhsIisDeclOfLet
                             {-# LINE 3242 "dist/build/PrintCode" #-}
                             )) of
                      { !_patsOisDeclOfLet ->
                      (case (({-# LINE 373 "src-ag/PrintCode.ag" #-}
                              _lhsIbelowIrrefutable
                              {-# LINE 3247 "dist/build/PrintCode" #-}
                              )) of
                       { !_patsObelowIrrefutable ->
                       (case (pats_ _patsObelowIrrefutable _patsOisDeclOfLet _patsOoptions) of
                        { ( !_patsIcopy,!_patsIpps,!_patsIpps',!_patsIstrictVars) ->
                            (case (({-# LINE 22 "src-ag/Patterns.ag" #-}
                                    Product pos_ _patsIcopy
                                    {-# LINE 3254 "dist/build/PrintCode" #-}
                                    )) of
                             { !_copy ->
                             (case (({-# LINE 22 "src-ag/Patterns.ag" #-}
                                     _copy
                                     {-# LINE 3259 "dist/build/PrintCode" #-}
                                     )) of
                              { !_lhsOcopy ->
                              (case (({-# LINE 369 "src-ag/PrintCode.ag" #-}
                                      False
                                      {-# LINE 3264 "dist/build/PrintCode" #-}
                                      )) of
                               { !_lhsOisUnderscore ->
                               (case (({-# LINE 352 "src-ag/PrintCode.ag" #-}
                                       if bangpats _lhsIoptions && not _lhsIisDeclOfLet && not _lhsIbelowIrrefutable
                                       then \p -> "!" >|< p
                                       else id
                                       {-# LINE 3271 "dist/build/PrintCode" #-}
                                       )) of
                                { !_addBang ->
                                (case (({-# LINE 358 "src-ag/PrintCode.ag" #-}
                                        _addBang     $ pp_block "(" ")" "," _patsIpps
                                        {-# LINE 3276 "dist/build/PrintCode" #-}
                                        )) of
                                 { !_lhsOpp ->
                                 (case (({-# LINE 392 "src-ag/PrintCode.ag" #-}
                                         pp_block "(" ")" "," _patsIpps'
                                         {-# LINE 3281 "dist/build/PrintCode" #-}
                                         )) of
                                  { !_lhsOpp' ->
                                  (case (({-# LINE 326 "src-ag/PrintCode.ag" #-}
                                          _patsIstrictVars
                                          {-# LINE 3286 "dist/build/PrintCode" #-}
                                          )) of
                                   { !_lhsOstrictVars ->
                                   ( _lhsOcopy,_lhsOisUnderscore,_lhsOpp,_lhsOpp',_lhsOstrictVars) }) }) }) }) }) }) }) }) }) }) })))
sem_Pattern_Underscore :: Pos ->
                          T_Pattern
sem_Pattern_Underscore !pos_ =
    (T_Pattern (\ (!_lhsIbelowIrrefutable)
                  (!_lhsIisDeclOfLet)
                  (!_lhsIoptions) ->
                    (case (({-# LINE 22 "src-ag/Patterns.ag" #-}
                            Underscore pos_
                            {-# LINE 3298 "dist/build/PrintCode" #-}
                            )) of
                     { !_copy ->
                     (case (({-# LINE 22 "src-ag/Patterns.ag" #-}
                             _copy
                             {-# LINE 3303 "dist/build/PrintCode" #-}
                             )) of
                      { !_lhsOcopy ->
                      (case (({-# LINE 371 "src-ag/PrintCode.ag" #-}
                              True
                              {-# LINE 3308 "dist/build/PrintCode" #-}
                              )) of
                       { !_lhsOisUnderscore ->
                       (case (({-# LINE 365 "src-ag/PrintCode.ag" #-}
                               text "_"
                               {-# LINE 3313 "dist/build/PrintCode" #-}
                               )) of
                        { !_lhsOpp ->
                        (case (({-# LINE 397 "src-ag/PrintCode.ag" #-}
                                text "_"
                                {-# LINE 3318 "dist/build/PrintCode" #-}
                                )) of
                         { !_lhsOpp' ->
                         (case (({-# LINE 326 "src-ag/PrintCode.ag" #-}
                                 []
                                 {-# LINE 3323 "dist/build/PrintCode" #-}
                                 )) of
                          { !_lhsOstrictVars ->
                          ( _lhsOcopy,_lhsOisUnderscore,_lhsOpp,_lhsOpp',_lhsOstrictVars) }) }) }) }) }) })))
-- Patterns ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         belowIrrefutable     : Bool
         isDeclOfLet          : Bool
         options              : Options
      synthesized attributes:
         copy                 : Patterns 
         pps                  : [PP_Doc]
         pps'                 : [PP_Doc]
         strictVars           : [PP_Doc]
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
newtype T_Patterns = T_Patterns (Bool ->
                                 Bool ->
                                 Options ->
                                 ( Patterns,([PP_Doc]),([PP_Doc]),([PP_Doc])))
data Inh_Patterns = Inh_Patterns {belowIrrefutable_Inh_Patterns :: !(Bool),isDeclOfLet_Inh_Patterns :: !(Bool),options_Inh_Patterns :: !(Options)}
data Syn_Patterns = Syn_Patterns {copy_Syn_Patterns :: !(Patterns),pps_Syn_Patterns :: !(([PP_Doc])),pps'_Syn_Patterns :: !(([PP_Doc])),strictVars_Syn_Patterns :: !(([PP_Doc]))}
wrap_Patterns :: T_Patterns ->
                 Inh_Patterns ->
                 Syn_Patterns
wrap_Patterns !(T_Patterns sem) !(Inh_Patterns _lhsIbelowIrrefutable _lhsIisDeclOfLet _lhsIoptions) =
    (let ( !_lhsOcopy,!_lhsOpps,!_lhsOpps',!_lhsOstrictVars) = sem _lhsIbelowIrrefutable _lhsIisDeclOfLet _lhsIoptions
     in  (Syn_Patterns _lhsOcopy _lhsOpps _lhsOpps' _lhsOstrictVars))
sem_Patterns_Cons :: T_Pattern ->
                     T_Patterns ->
                     T_Patterns
sem_Patterns_Cons !(T_Pattern hd_) !(T_Patterns tl_) =
    (T_Patterns (\ (!_lhsIbelowIrrefutable)
                   (!_lhsIisDeclOfLet)
                   (!_lhsIoptions) ->
                     (case (({-# LINE 50 "src-ag/PrintCode.ag" #-}
                             _lhsIoptions
                             {-# LINE 3376 "dist/build/PrintCode" #-}
                             )) of
                      { !_tlOoptions ->
                      (case (({-# LINE 407 "src-ag/PrintCode.ag" #-}
                              _lhsIisDeclOfLet
                              {-# LINE 3381 "dist/build/PrintCode" #-}
                              )) of
                       { !_tlOisDeclOfLet ->
                       (case (({-# LINE 373 "src-ag/PrintCode.ag" #-}
                               _lhsIbelowIrrefutable
                               {-# LINE 3386 "dist/build/PrintCode" #-}
                               )) of
                        { !_tlObelowIrrefutable ->
                        (case (tl_ _tlObelowIrrefutable _tlOisDeclOfLet _tlOoptions) of
                         { ( !_tlIcopy,!_tlIpps,!_tlIpps',!_tlIstrictVars) ->
                             (case (({-# LINE 50 "src-ag/PrintCode.ag" #-}
                                     _lhsIoptions
                                     {-# LINE 3393 "dist/build/PrintCode" #-}
                                     )) of
                              { !_hdOoptions ->
                              (case (({-# LINE 407 "src-ag/PrintCode.ag" #-}
                                      _lhsIisDeclOfLet
                                      {-# LINE 3398 "dist/build/PrintCode" #-}
                                      )) of
                               { !_hdOisDeclOfLet ->
                               (case (({-# LINE 373 "src-ag/PrintCode.ag" #-}
                                       _lhsIbelowIrrefutable
                                       {-# LINE 3403 "dist/build/PrintCode" #-}
                                       )) of
                                { !_hdObelowIrrefutable ->
                                (case (hd_ _hdObelowIrrefutable _hdOisDeclOfLet _hdOoptions) of
                                 { ( !_hdIcopy,!_hdIisUnderscore,!_hdIpp,!_hdIpp',!_hdIstrictVars) ->
                                     (case (({-# LINE 22 "src-ag/Patterns.ag" #-}
                                             (:) _hdIcopy _tlIcopy
                                             {-# LINE 3410 "dist/build/PrintCode" #-}
                                             )) of
                                      { !_copy ->
                                      (case (({-# LINE 22 "src-ag/Patterns.ag" #-}
                                              _copy
                                              {-# LINE 3415 "dist/build/PrintCode" #-}
                                              )) of
                                       { !_lhsOcopy ->
                                       (case (({-# LINE 347 "src-ag/PrintCode.ag" #-}
                                               _hdIpp : _tlIpps
                                               {-# LINE 3420 "dist/build/PrintCode" #-}
                                               )) of
                                        { !_lhsOpps ->
                                        (case (({-# LINE 387 "src-ag/PrintCode.ag" #-}
                                                _hdIpp' : _tlIpps'
                                                {-# LINE 3425 "dist/build/PrintCode" #-}
                                                )) of
                                         { !_lhsOpps' ->
                                         (case (({-# LINE 326 "src-ag/PrintCode.ag" #-}
                                                 _hdIstrictVars ++ _tlIstrictVars
                                                 {-# LINE 3430 "dist/build/PrintCode" #-}
                                                 )) of
                                          { !_lhsOstrictVars ->
                                          ( _lhsOcopy,_lhsOpps,_lhsOpps',_lhsOstrictVars) }) }) }) }) }) }) }) }) }) }) }) }) })))
sem_Patterns_Nil :: T_Patterns
sem_Patterns_Nil =
    (T_Patterns (\ (!_lhsIbelowIrrefutable)
                   (!_lhsIisDeclOfLet)
                   (!_lhsIoptions) ->
                     (case (({-# LINE 22 "src-ag/Patterns.ag" #-}
                             []
                             {-# LINE 3441 "dist/build/PrintCode" #-}
                             )) of
                      { !_copy ->
                      (case (({-# LINE 22 "src-ag/Patterns.ag" #-}
                              _copy
                              {-# LINE 3446 "dist/build/PrintCode" #-}
                              )) of
                       { !_lhsOcopy ->
                       (case (({-# LINE 348 "src-ag/PrintCode.ag" #-}
                               []
                               {-# LINE 3451 "dist/build/PrintCode" #-}
                               )) of
                        { !_lhsOpps ->
                        (case (({-# LINE 388 "src-ag/PrintCode.ag" #-}
                                []
                                {-# LINE 3456 "dist/build/PrintCode" #-}
                                )) of
                         { !_lhsOpps' ->
                         (case (({-# LINE 326 "src-ag/PrintCode.ag" #-}
                                 []
                                 {-# LINE 3461 "dist/build/PrintCode" #-}
                                 )) of
                          { !_lhsOstrictVars ->
                          ( _lhsOcopy,_lhsOpps,_lhsOpps',_lhsOstrictVars) }) }) }) }) })))
-- Program -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         importBlocks         : PP_Doc
         mainBlocksDoc        : PP_Doc
         mainFile             : String
         mainName             : String
         moduleHeader         : String -> String -> String -> Bool -> String
         options              : Options
         optionsLine          : String
         pragmaBlocks         : String
         textBlockMap         : Map BlockInfo PP_Doc
         textBlocks           : PP_Doc
      synthesized attributes:
         genIO                : IO ()
         output               : PP_Docs
   alternatives:
      alternative Program:
         child chunks         : Chunks 
         child ordered        : {Bool}
         visit 0:
            local options     : _
            local commonFile  : _
            local genCommonModule : _
            local mainModuleFile : _
            local genMainModule : _
-}
-- cata
sem_Program :: Program ->
               T_Program
sem_Program !(Program _chunks _ordered) =
    (sem_Program_Program (sem_Chunks _chunks) _ordered)
-- semantic domain
newtype T_Program = T_Program (PP_Doc ->
                               PP_Doc ->
                               String ->
                               String ->
                               (String -> String -> String -> Bool -> String) ->
                               Options ->
                               String ->
                               String ->
                               (Map BlockInfo PP_Doc) ->
                               PP_Doc ->
                               ( (IO ()),PP_Docs))
data Inh_Program = Inh_Program {importBlocks_Inh_Program :: !(PP_Doc),mainBlocksDoc_Inh_Program :: !(PP_Doc),mainFile_Inh_Program :: !(String),mainName_Inh_Program :: !(String),moduleHeader_Inh_Program :: !((String -> String -> String -> Bool -> String)),options_Inh_Program :: !(Options),optionsLine_Inh_Program :: !(String),pragmaBlocks_Inh_Program :: !(String),textBlockMap_Inh_Program :: !((Map BlockInfo PP_Doc)),textBlocks_Inh_Program :: !(PP_Doc)}
data Syn_Program = Syn_Program {genIO_Syn_Program :: !((IO ())),output_Syn_Program :: !(PP_Docs)}
wrap_Program :: T_Program ->
                Inh_Program ->
                Syn_Program
wrap_Program !(T_Program sem) !(Inh_Program _lhsIimportBlocks _lhsImainBlocksDoc _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsIoptions _lhsIoptionsLine _lhsIpragmaBlocks _lhsItextBlockMap _lhsItextBlocks) =
    (let ( !_lhsOgenIO,!_lhsOoutput) = sem _lhsIimportBlocks _lhsImainBlocksDoc _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsIoptions _lhsIoptionsLine _lhsIpragmaBlocks _lhsItextBlockMap _lhsItextBlocks
     in  (Syn_Program _lhsOgenIO _lhsOoutput))
sem_Program_Program :: T_Chunks ->
                       Bool ->
                       T_Program
sem_Program_Program !(T_Chunks chunks_) !ordered_ =
    (T_Program (\ (!_lhsIimportBlocks)
                  (!_lhsImainBlocksDoc)
                  (!_lhsImainFile)
                  (!_lhsImainName)
                  (!_lhsImoduleHeader)
                  (!_lhsIoptions)
                  (!_lhsIoptionsLine)
                  (!_lhsIpragmaBlocks)
                  (!_lhsItextBlockMap)
                  (!_lhsItextBlocks) ->
                    (case (({-# LINE 435 "src-ag/PrintCode.ag" #-}
                            _lhsItextBlockMap
                            {-# LINE 3534 "dist/build/PrintCode" #-}
                            )) of
                     { !_chunksOtextBlockMap ->
                     (case (({-# LINE 433 "src-ag/PrintCode.ag" #-}
                             _lhsIpragmaBlocks
                             {-# LINE 3539 "dist/build/PrintCode" #-}
                             )) of
                      { !_chunksOpragmaBlocks ->
                      (case (({-# LINE 436 "src-ag/PrintCode.ag" #-}
                              _lhsIoptionsLine
                              {-# LINE 3544 "dist/build/PrintCode" #-}
                              )) of
                       { !_chunksOoptionsLine ->
                       (case (({-# LINE 59 "src-ag/PrintCode.ag" #-}
                               _lhsIoptions { breadthFirst = breadthFirst _lhsIoptions && visit _lhsIoptions && cases _lhsIoptions && ordered_ }
                               {-# LINE 3549 "dist/build/PrintCode" #-}
                               )) of
                        { !_options ->
                        (case (({-# LINE 50 "src-ag/PrintCode.ag" #-}
                                _options
                                {-# LINE 3554 "dist/build/PrintCode" #-}
                                )) of
                         { !_chunksOoptions ->
                         (case (({-# LINE 439 "src-ag/PrintCode.ag" #-}
                                 _lhsImoduleHeader
                                 {-# LINE 3559 "dist/build/PrintCode" #-}
                                 )) of
                          { !_chunksOmoduleHeader ->
                          (case (({-# LINE 438 "src-ag/PrintCode.ag" #-}
                                  _lhsImainName
                                  {-# LINE 3564 "dist/build/PrintCode" #-}
                                  )) of
                           { !_chunksOmainName ->
                           (case (({-# LINE 437 "src-ag/PrintCode.ag" #-}
                                   _lhsImainFile
                                   {-# LINE 3569 "dist/build/PrintCode" #-}
                                   )) of
                            { !_chunksOmainFile ->
                            (case (({-# LINE 457 "src-ag/PrintCode.ag" #-}
                                    replaceBaseName _lhsImainFile (takeBaseName _lhsImainFile ++ "_common")
                                    {-# LINE 3574 "dist/build/PrintCode" #-}
                                    )) of
                             { !_commonFile ->
                             (case (({-# LINE 410 "src-ag/PrintCode.ag" #-}
                                     False
                                     {-# LINE 3579 "dist/build/PrintCode" #-}
                                     )) of
                              { !_chunksOisDeclOfLet ->
                              (case (({-# LINE 62 "src-ag/PrintCode.ag" #-}
                                      nest _lhsIoptions
                                      {-# LINE 3584 "dist/build/PrintCode" #-}
                                      )) of
                               { !_chunksOnested ->
                               (case (({-# LINE 434 "src-ag/PrintCode.ag" #-}
                                       _lhsItextBlocks
                                       {-# LINE 3589 "dist/build/PrintCode" #-}
                                       )) of
                                { !_chunksOtextBlocks ->
                                (case (({-# LINE 432 "src-ag/PrintCode.ag" #-}
                                        _lhsIimportBlocks
                                        {-# LINE 3594 "dist/build/PrintCode" #-}
                                        )) of
                                 { !_chunksOimportBlocks ->
                                 (case (chunks_ _chunksOimportBlocks _chunksOisDeclOfLet _chunksOmainFile _chunksOmainName _chunksOmoduleHeader _chunksOnested _chunksOoptions _chunksOoptionsLine _chunksOpragmaBlocks _chunksOtextBlockMap _chunksOtextBlocks) of
                                  { ( !_chunksIappendCommon,!_chunksIappendMain,!_chunksIgenSems,!_chunksIimports,!_chunksIpps) ->
                                      (case (({-# LINE 458 "src-ag/PrintCode.ag" #-}
                                              writeModule _commonFile
                                                  ( [ pp $ _lhsIpragmaBlocks
                                                    , pp $ _lhsIoptionsLine
                                                    , pp $ _lhsImoduleHeader _lhsImainName "_common" "" True
                                                    , _lhsIimportBlocks
                                                    , _lhsItextBlocks
                                                    ]
                                                    ++ map vlist _chunksIappendCommon
                                                  )
                                              {-# LINE 3609 "dist/build/PrintCode" #-}
                                              )) of
                                       { !_genCommonModule ->
                                       (case (({-# LINE 444 "src-ag/PrintCode.ag" #-}
                                               _lhsImainFile
                                               {-# LINE 3614 "dist/build/PrintCode" #-}
                                               )) of
                                        { !_mainModuleFile ->
                                        (case (({-# LINE 445 "src-ag/PrintCode.ag" #-}
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
                                                {-# LINE 3628 "dist/build/PrintCode" #-}
                                                )) of
                                         { !_genMainModule ->
                                         (case (({-# LINE 469 "src-ag/PrintCode.ag" #-}
                                                 do _genMainModule
                                                    _genCommonModule
                                                    _chunksIgenSems
                                                 {-# LINE 3635 "dist/build/PrintCode" #-}
                                                 )) of
                                          { !_lhsOgenIO ->
                                          (case (({-# LINE 94 "src-ag/PrintCode.ag" #-}
                                                  _chunksIpps
                                                  {-# LINE 3640 "dist/build/PrintCode" #-}
                                                  )) of
                                           { !_lhsOoutput ->
                                           ( _lhsOgenIO,_lhsOoutput) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })))
-- Type --------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         nested               : Bool
      synthesized attributes:
         pp                   : PP_Doc
         prec                 : Int
   alternatives:
      alternative Arr:
         child left           : Type 
         child right          : Type 
         visit 0:
            local r           : _
            local l           : _
      alternative CtxApp:
         child left           : {[(String, [String])]}
         child right          : Type 
      alternative List:
         child tp             : Type 
      alternative NontermType:
         child name           : {String}
         child params         : {[String]}
         child deforested     : {Bool}
         visit 0:
            local prefix      : _
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
newtype T_Type = T_Type (Bool ->
                         ( PP_Doc,Int))
data Inh_Type = Inh_Type {nested_Inh_Type :: !(Bool)}
data Syn_Type = Syn_Type {pp_Syn_Type :: !(PP_Doc),prec_Syn_Type :: !(Int)}
wrap_Type :: T_Type ->
             Inh_Type ->
             Syn_Type
wrap_Type !(T_Type sem) !(Inh_Type _lhsInested) =
    (let ( !_lhsOpp,!_lhsOprec) = sem _lhsInested
     in  (Syn_Type _lhsOpp _lhsOprec))
sem_Type_Arr :: T_Type ->
                T_Type ->
                T_Type
sem_Type_Arr !(T_Type left_) !(T_Type right_) =
    (T_Type (\ (!_lhsInested) ->
                 (case (({-# LINE 52 "src-ag/PrintCode.ag" #-}
                         _lhsInested
                         {-# LINE 3740 "dist/build/PrintCode" #-}
                         )) of
                  { !_rightOnested ->
                  (case (({-# LINE 52 "src-ag/PrintCode.ag" #-}
                          _lhsInested
                          {-# LINE 3745 "dist/build/PrintCode" #-}
                          )) of
                   { !_leftOnested ->
                   (case (right_ _rightOnested) of
                    { ( !_rightIpp,!_rightIprec) ->
                        (case (({-# LINE 263 "src-ag/PrintCode.ag" #-}
                                if _rightIprec <  2 then pp_parens _rightIpp else _rightIpp
                                {-# LINE 3752 "dist/build/PrintCode" #-}
                                )) of
                         { !_r ->
                         (case (left_ _leftOnested) of
                          { ( !_leftIpp,!_leftIprec) ->
                              (case (({-# LINE 262 "src-ag/PrintCode.ag" #-}
                                      if _leftIprec  <= 2 then pp_parens _leftIpp  else _leftIpp
                                      {-# LINE 3759 "dist/build/PrintCode" #-}
                                      )) of
                               { !_l ->
                               (case (({-# LINE 261 "src-ag/PrintCode.ag" #-}
                                       _l     >#< "->" >-< _r
                                       {-# LINE 3764 "dist/build/PrintCode" #-}
                                       )) of
                                { !_lhsOpp ->
                                (case (({-# LINE 260 "src-ag/PrintCode.ag" #-}
                                        2
                                        {-# LINE 3769 "dist/build/PrintCode" #-}
                                        )) of
                                 { !_lhsOprec ->
                                 ( _lhsOpp,_lhsOprec) }) }) }) }) }) }) }) })))
sem_Type_CtxApp :: ([(String, [String])]) ->
                   T_Type ->
                   T_Type
sem_Type_CtxApp !left_ !(T_Type right_) =
    (T_Type (\ (!_lhsInested) ->
                 (case (({-# LINE 52 "src-ag/PrintCode.ag" #-}
                         _lhsInested
                         {-# LINE 3780 "dist/build/PrintCode" #-}
                         )) of
                  { !_rightOnested ->
                  (case (right_ _rightOnested) of
                   { ( !_rightIpp,!_rightIprec) ->
                       (case (({-# LINE 269 "src-ag/PrintCode.ag" #-}
                               (pp_block "(" ")" "," $ map (\(n,ns) -> hv_sp $ map pp (n:ns)) left_) >#< "=>" >#< _rightIpp
                               {-# LINE 3787 "dist/build/PrintCode" #-}
                               )) of
                        { !_lhsOpp ->
                        (case (({-# LINE 259 "src-ag/PrintCode.ag" #-}
                                _rightIprec
                                {-# LINE 3792 "dist/build/PrintCode" #-}
                                )) of
                         { !_lhsOprec ->
                         ( _lhsOpp,_lhsOprec) }) }) }) })))
sem_Type_List :: T_Type ->
                 T_Type
sem_Type_List !(T_Type tp_) =
    (T_Type (\ (!_lhsInested) ->
                 (case (({-# LINE 52 "src-ag/PrintCode.ag" #-}
                         _lhsInested
                         {-# LINE 3802 "dist/build/PrintCode" #-}
                         )) of
                  { !_tpOnested ->
                  (case (tp_ _tpOnested) of
                   { ( !_tpIpp,!_tpIprec) ->
                       (case (({-# LINE 280 "src-ag/PrintCode.ag" #-}
                               "[" >|< _tpIpp >|< "]"
                               {-# LINE 3809 "dist/build/PrintCode" #-}
                               )) of
                        { !_lhsOpp ->
                        (case (({-# LINE 279 "src-ag/PrintCode.ag" #-}
                                5
                                {-# LINE 3814 "dist/build/PrintCode" #-}
                                )) of
                         { !_lhsOprec ->
                         ( _lhsOpp,_lhsOprec) }) }) }) })))
sem_Type_NontermType :: String ->
                        ([String]) ->
                        Bool ->
                        T_Type
sem_Type_NontermType !name_ !params_ !deforested_ =
    (T_Type (\ (!_lhsInested) ->
                 (case (({-# LINE 287 "src-ag/PrintCode.ag" #-}
                         if deforested_
                         then text "T_"
                         else empty
                         {-# LINE 3828 "dist/build/PrintCode" #-}
                         )) of
                  { !_prefix ->
                  (case (({-# LINE 286 "src-ag/PrintCode.ag" #-}
                          _prefix     >|< text name_ >#< hv_sp params_
                          {-# LINE 3833 "dist/build/PrintCode" #-}
                          )) of
                   { !_lhsOpp ->
                   (case (({-# LINE 285 "src-ag/PrintCode.ag" #-}
                           5
                           {-# LINE 3838 "dist/build/PrintCode" #-}
                           )) of
                    { !_lhsOprec ->
                    ( _lhsOpp,_lhsOprec) }) }) })))
sem_Type_QuantApp :: String ->
                     T_Type ->
                     T_Type
sem_Type_QuantApp !left_ !(T_Type right_) =
    (T_Type (\ (!_lhsInested) ->
                 (case (({-# LINE 52 "src-ag/PrintCode.ag" #-}
                         _lhsInested
                         {-# LINE 3849 "dist/build/PrintCode" #-}
                         )) of
                  { !_rightOnested ->
                  (case (right_ _rightOnested) of
                   { ( !_rightIpp,!_rightIprec) ->
                       (case (({-# LINE 271 "src-ag/PrintCode.ag" #-}
                               left_ >#< _rightIpp
                               {-# LINE 3856 "dist/build/PrintCode" #-}
                               )) of
                        { !_lhsOpp ->
                        (case (({-# LINE 259 "src-ag/PrintCode.ag" #-}
                                _rightIprec
                                {-# LINE 3861 "dist/build/PrintCode" #-}
                                )) of
                         { !_lhsOprec ->
                         ( _lhsOpp,_lhsOprec) }) }) }) })))
sem_Type_SimpleType :: String ->
                       T_Type
sem_Type_SimpleType !txt_ =
    (T_Type (\ (!_lhsInested) ->
                 (case (({-# LINE 283 "src-ag/PrintCode.ag" #-}
                         if reallySimple txt_ then text txt_ else pp_parens (text txt_)
                         {-# LINE 3871 "dist/build/PrintCode" #-}
                         )) of
                  { !_lhsOpp ->
                  (case (({-# LINE 282 "src-ag/PrintCode.ag" #-}
                          5
                          {-# LINE 3876 "dist/build/PrintCode" #-}
                          )) of
                   { !_lhsOprec ->
                   ( _lhsOpp,_lhsOprec) }) })))
sem_Type_TEither :: T_Type ->
                    T_Type ->
                    T_Type
sem_Type_TEither !(T_Type left_) !(T_Type right_) =
    (T_Type (\ (!_lhsInested) ->
                 (case (({-# LINE 52 "src-ag/PrintCode.ag" #-}
                         _lhsInested
                         {-# LINE 3887 "dist/build/PrintCode" #-}
                         )) of
                  { !_rightOnested ->
                  (case (({-# LINE 52 "src-ag/PrintCode.ag" #-}
                          _lhsInested
                          {-# LINE 3892 "dist/build/PrintCode" #-}
                          )) of
                   { !_leftOnested ->
                   (case (right_ _rightOnested) of
                    { ( !_rightIpp,!_rightIprec) ->
                        (case (left_ _leftOnested) of
                         { ( !_leftIpp,!_leftIprec) ->
                             (case (({-# LINE 293 "src-ag/PrintCode.ag" #-}
                                     text "Either" >#< pp_parens _leftIpp >#< pp_parens _rightIpp
                                     {-# LINE 3901 "dist/build/PrintCode" #-}
                                     )) of
                              { !_lhsOpp ->
                              (case (({-# LINE 292 "src-ag/PrintCode.ag" #-}
                                      5
                                      {-# LINE 3906 "dist/build/PrintCode" #-}
                                      )) of
                               { !_lhsOprec ->
                               ( _lhsOpp,_lhsOprec) }) }) }) }) }) })))
sem_Type_TIntMap :: T_Type ->
                    T_Type
sem_Type_TIntMap !(T_Type value_) =
    (T_Type (\ (!_lhsInested) ->
                 (case (({-# LINE 52 "src-ag/PrintCode.ag" #-}
                         _lhsInested
                         {-# LINE 3916 "dist/build/PrintCode" #-}
                         )) of
                  { !_valueOnested ->
                  (case (value_ _valueOnested) of
                   { ( !_valueIpp,!_valueIprec) ->
                       (case (({-# LINE 297 "src-ag/PrintCode.ag" #-}
                               text "Data.IntMap.IntMap" >#< pp_parens _valueIpp
                               {-# LINE 3923 "dist/build/PrintCode" #-}
                               )) of
                        { !_lhsOpp ->
                        (case (({-# LINE 296 "src-ag/PrintCode.ag" #-}
                                5
                                {-# LINE 3928 "dist/build/PrintCode" #-}
                                )) of
                         { !_lhsOprec ->
                         ( _lhsOpp,_lhsOprec) }) }) }) })))
sem_Type_TMap :: T_Type ->
                 T_Type ->
                 T_Type
sem_Type_TMap !(T_Type key_) !(T_Type value_) =
    (T_Type (\ (!_lhsInested) ->
                 (case (({-# LINE 52 "src-ag/PrintCode.ag" #-}
                         _lhsInested
                         {-# LINE 3939 "dist/build/PrintCode" #-}
                         )) of
                  { !_valueOnested ->
                  (case (({-# LINE 52 "src-ag/PrintCode.ag" #-}
                          _lhsInested
                          {-# LINE 3944 "dist/build/PrintCode" #-}
                          )) of
                   { !_keyOnested ->
                   (case (value_ _valueOnested) of
                    { ( !_valueIpp,!_valueIprec) ->
                        (case (key_ _keyOnested) of
                         { ( !_keyIpp,!_keyIprec) ->
                             (case (({-# LINE 295 "src-ag/PrintCode.ag" #-}
                                     text "Data.Map.Map" >#< pp_parens _keyIpp >#< pp_parens _valueIpp
                                     {-# LINE 3953 "dist/build/PrintCode" #-}
                                     )) of
                              { !_lhsOpp ->
                              (case (({-# LINE 294 "src-ag/PrintCode.ag" #-}
                                      5
                                      {-# LINE 3958 "dist/build/PrintCode" #-}
                                      )) of
                               { !_lhsOprec ->
                               ( _lhsOpp,_lhsOprec) }) }) }) }) }) })))
sem_Type_TMaybe :: T_Type ->
                   T_Type
sem_Type_TMaybe !(T_Type tp_) =
    (T_Type (\ (!_lhsInested) ->
                 (case (({-# LINE 52 "src-ag/PrintCode.ag" #-}
                         _lhsInested
                         {-# LINE 3968 "dist/build/PrintCode" #-}
                         )) of
                  { !_tpOnested ->
                  (case (tp_ _tpOnested) of
                   { ( !_tpIpp,!_tpIprec) ->
                       (case (({-# LINE 291 "src-ag/PrintCode.ag" #-}
                               text "Maybe" >#< _tpIpp
                               {-# LINE 3975 "dist/build/PrintCode" #-}
                               )) of
                        { !_lhsOpp ->
                        (case (({-# LINE 290 "src-ag/PrintCode.ag" #-}
                                5
                                {-# LINE 3980 "dist/build/PrintCode" #-}
                                )) of
                         { !_lhsOprec ->
                         ( _lhsOpp,_lhsOprec) }) }) }) })))
sem_Type_TupleType :: T_Types ->
                      T_Type
sem_Type_TupleType !(T_Types tps_) =
    (T_Type (\ (!_lhsInested) ->
                 (case (({-# LINE 54 "src-ag/PrintCode.ag" #-}
                         _lhsInested
                         {-# LINE 3990 "dist/build/PrintCode" #-}
                         )) of
                  { !_tpsOnested ->
                  (case (tps_ _tpsOnested) of
                   { ( !_tpsIpps) ->
                       (case (({-# LINE 274 "src-ag/PrintCode.ag" #-}
                               ppTuple _lhsInested _tpsIpps
                               {-# LINE 3997 "dist/build/PrintCode" #-}
                               )) of
                        { !_lhsOpp ->
                        (case (({-# LINE 273 "src-ag/PrintCode.ag" #-}
                                5
                                {-# LINE 4002 "dist/build/PrintCode" #-}
                                )) of
                         { !_lhsOprec ->
                         ( _lhsOpp,_lhsOprec) }) }) }) })))
sem_Type_TypeApp :: T_Type ->
                    T_Types ->
                    T_Type
sem_Type_TypeApp !(T_Type func_) !(T_Types args_) =
    (T_Type (\ (!_lhsInested) ->
                 (case (({-# LINE 54 "src-ag/PrintCode.ag" #-}
                         _lhsInested
                         {-# LINE 4013 "dist/build/PrintCode" #-}
                         )) of
                  { !_argsOnested ->
                  (case (({-# LINE 52 "src-ag/PrintCode.ag" #-}
                          _lhsInested
                          {-# LINE 4018 "dist/build/PrintCode" #-}
                          )) of
                   { !_funcOnested ->
                   (case (args_ _argsOnested) of
                    { ( !_argsIpps) ->
                        (case (func_ _funcOnested) of
                         { ( !_funcIpp,!_funcIprec) ->
                             (case (({-# LINE 266 "src-ag/PrintCode.ag" #-}
                                     hv_sp (_funcIpp : _argsIpps)
                                     {-# LINE 4027 "dist/build/PrintCode" #-}
                                     )) of
                              { !_lhsOpp ->
                              (case (({-# LINE 259 "src-ag/PrintCode.ag" #-}
                                      _funcIprec
                                      {-# LINE 4032 "dist/build/PrintCode" #-}
                                      )) of
                               { !_lhsOprec ->
                               ( _lhsOpp,_lhsOprec) }) }) }) }) }) })))
sem_Type_UnboxedTupleType :: T_Types ->
                             T_Type
sem_Type_UnboxedTupleType !(T_Types tps_) =
    (T_Type (\ (!_lhsInested) ->
                 (case (({-# LINE 54 "src-ag/PrintCode.ag" #-}
                         _lhsInested
                         {-# LINE 4042 "dist/build/PrintCode" #-}
                         )) of
                  { !_tpsOnested ->
                  (case (tps_ _tpsOnested) of
                   { ( !_tpsIpps) ->
                       (case (({-# LINE 277 "src-ag/PrintCode.ag" #-}
                               ppUnboxedTuple _lhsInested _tpsIpps
                               {-# LINE 4049 "dist/build/PrintCode" #-}
                               )) of
                        { !_lhsOpp ->
                        (case (({-# LINE 276 "src-ag/PrintCode.ag" #-}
                                5
                                {-# LINE 4054 "dist/build/PrintCode" #-}
                                )) of
                         { !_lhsOprec ->
                         ( _lhsOpp,_lhsOprec) }) }) }) })))
-- Types -------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         nested               : Bool
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
newtype T_Types = T_Types (Bool ->
                           ( PP_Docs))
data Inh_Types = Inh_Types {nested_Inh_Types :: !(Bool)}
data Syn_Types = Syn_Types {pps_Syn_Types :: !(PP_Docs)}
wrap_Types :: T_Types ->
              Inh_Types ->
              Syn_Types
wrap_Types !(T_Types sem) !(Inh_Types _lhsInested) =
    (let ( !_lhsOpps) = sem _lhsInested
     in  (Syn_Types _lhsOpps))
sem_Types_Cons :: T_Type ->
                  T_Types ->
                  T_Types
sem_Types_Cons !(T_Type hd_) !(T_Types tl_) =
    (T_Types (\ (!_lhsInested) ->
                  (case (({-# LINE 54 "src-ag/PrintCode.ag" #-}
                          _lhsInested
                          {-# LINE 4094 "dist/build/PrintCode" #-}
                          )) of
                   { !_tlOnested ->
                   (case (({-# LINE 52 "src-ag/PrintCode.ag" #-}
                           _lhsInested
                           {-# LINE 4099 "dist/build/PrintCode" #-}
                           )) of
                    { !_hdOnested ->
                    (case (tl_ _tlOnested) of
                     { ( !_tlIpps) ->
                         (case (hd_ _hdOnested) of
                          { ( !_hdIpp,!_hdIprec) ->
                              (case (({-# LINE 77 "src-ag/PrintCode.ag" #-}
                                      _hdIpp : _tlIpps
                                      {-# LINE 4108 "dist/build/PrintCode" #-}
                                      )) of
                               { !_lhsOpps ->
                               ( _lhsOpps) }) }) }) }) })))
sem_Types_Nil :: T_Types
sem_Types_Nil =
    (T_Types (\ (!_lhsInested) ->
                  (case (({-# LINE 78 "src-ag/PrintCode.ag" #-}
                          []
                          {-# LINE 4117 "dist/build/PrintCode" #-}
                          )) of
                   { !_lhsOpps ->
                   ( _lhsOpps) })))