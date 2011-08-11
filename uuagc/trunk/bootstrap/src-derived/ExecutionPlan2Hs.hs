

-- UUAGC 0.9.38.6.5 (src-ag/ExecutionPlan2Hs.ag)
module ExecutionPlan2Hs where
{-# LINE 7 "src-ag/ExecutionPlan2Hs.ag" #-}

import ExecutionPlan
import Pretty
import PPUtil
import Options
import Data.Maybe
import Debug.Trace
import System.IO
import System.Directory

import TokenDef
import HsToken

import qualified Data.Set as Set
import qualified Data.Map as Map
{-# LINE 22 "dist/build/uuagc/uuagc-tmp/ExecutionPlan2Hs.hs" #-}

{-# LINE 2 "src-ag/ExecutionPlan.ag" #-}

-- VisitSyntax.ag imports
import Patterns    (Pattern(..),Patterns)
import Expression  (Expression(..))
import CommonTypes

import qualified Data.Set as Set
{-# LINE 32 "dist/build/uuagc/uuagc-tmp/ExecutionPlan2Hs.hs" #-}

{-# LINE 2 "src-ag/Patterns.ag" #-}

-- Patterns.ag imports
import UU.Scanner.Position(Pos)
import CommonTypes (ConstructorIdent,Identifier)
{-# LINE 39 "dist/build/uuagc/uuagc-tmp/ExecutionPlan2Hs.hs" #-}

{-# LINE 2 "src-ag/Expression.ag" #-}

import UU.Scanner.Position(Pos)
import HsToken
{-# LINE 45 "dist/build/uuagc/uuagc-tmp/ExecutionPlan2Hs.hs" #-}

{-# LINE 2 "src-ag/HsToken.ag" #-}

import CommonTypes
import UU.Scanner.Position(Pos)
{-# LINE 51 "dist/build/uuagc/uuagc-tmp/ExecutionPlan2Hs.hs" #-}
{-# LINE 204 "src-ag/ExecutionPlan2Hs.ag" #-}
type VisitStateState = (VisitIdentifier,StateIdentifier, StateIdentifier)
{-# LINE 54 "dist/build/uuagc/uuagc-tmp/ExecutionPlan2Hs.hs" #-}

{-# LINE 475 "src-ag/ExecutionPlan2Hs.ag" #-}

uwSetUnion :: (Ord a, Ord b) => Map.Map a (Set.Set b) -> Map.Map a (Set.Set b) -> Map.Map a (Set.Set b)
uwSetUnion = Map.unionWith Set.union
{-# LINE 60 "dist/build/uuagc/uuagc-tmp/ExecutionPlan2Hs.hs" #-}

{-# LINE 644 "src-ag/ExecutionPlan2Hs.ag" #-}

renderDocs :: [PP_Doc] -> String
renderDocs pps = foldr (.) id (map (\d -> (disp d 50000) . ( '\n':) ) pps) ""

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
{-# LINE 80 "dist/build/uuagc/uuagc-tmp/ExecutionPlan2Hs.hs" #-}
-- EChild ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         nt                   : NontermIdent
         options              : Options
      synthesized attributes:
         argnames             :   PP_Doc  
         argnamesw            :  PP_Doc 
         argtps               :   PP_Doc  
         childintros          : Map.Map Identifier PP_Doc
         datatype             : PP_Doc
         terminaldefs         : Set.Set String
   alternatives:
      alternative EChild:
         child name           : {Identifier}
         child tp             : {Type}
         child virtual        : {Maybe (Maybe Type)}
         visit 0:
            local addStrict   : _
            local argnames    : _
-}
-- cata
sem_EChild :: EChild  ->
              T_EChild 
sem_EChild (EChild _name _tp _virtual )  =
    (sem_EChild_EChild _name _tp _virtual )
-- semantic domain
newtype T_EChild  = T_EChild (NontermIdent ->
                              Options ->
                              ( (  PP_Doc  ),( PP_Doc ),(  PP_Doc  ),(Map.Map Identifier PP_Doc),PP_Doc,(Set.Set String)))
data Inh_EChild  = Inh_EChild {nt_Inh_EChild :: NontermIdent,options_Inh_EChild :: Options}
data Syn_EChild  = Syn_EChild {argnames_Syn_EChild :: (  PP_Doc  ),argnamesw_Syn_EChild :: ( PP_Doc ),argtps_Syn_EChild :: (  PP_Doc  ),childintros_Syn_EChild :: (Map.Map Identifier PP_Doc),datatype_Syn_EChild :: PP_Doc,terminaldefs_Syn_EChild :: (Set.Set String)}
wrap_EChild :: T_EChild  ->
               Inh_EChild  ->
               Syn_EChild 
wrap_EChild (T_EChild sem ) (Inh_EChild _lhsInt _lhsIoptions )  =
    (let ( _lhsOargnames,_lhsOargnamesw,_lhsOargtps,_lhsOchildintros,_lhsOdatatype,_lhsOterminaldefs) = sem _lhsInt _lhsIoptions 
     in  (Syn_EChild _lhsOargnames _lhsOargnamesw _lhsOargtps _lhsOchildintros _lhsOdatatype _lhsOterminaldefs ))
sem_EChild_EChild :: Identifier ->
                     Type ->
                     (Maybe (Maybe Type)) ->
                     T_EChild 
sem_EChild_EChild name_ tp_ virtual_  =
    (T_EChild (\ _lhsInt
                 _lhsIoptions ->
                   (let _lhsOdatatype :: PP_Doc
                        _lhsOargnamesw :: ( PP_Doc )
                        _lhsOargtps :: (  PP_Doc  )
                        _lhsOchildintros :: (Map.Map Identifier PP_Doc)
                        _lhsOterminaldefs :: (Set.Set String)
                        _lhsOargnames :: (  PP_Doc  )
                        -- "src-ag/ExecutionPlan2Hs.ag"(line 127, column 12)
                        _lhsOdatatype =
                            ({-# LINE 127 "src-ag/ExecutionPlan2Hs.ag" #-}
                             if isJust virtual_
                             then empty
                             else _addStrict     $ pp_parens $ typeToHaskellString (Just _lhsInt) [] tp_
                             {-# LINE 139 "src-ag/ExecutionPlan2Hs.hs" #-}
                             )
                        -- "src-ag/ExecutionPlan2Hs.ag"(line 130, column 12)
                        _addStrict =
                            ({-# LINE 130 "src-ag/ExecutionPlan2Hs.ag" #-}
                             \x -> if strictData _lhsIoptions then "!" >|< x else x
                             {-# LINE 145 "src-ag/ExecutionPlan2Hs.hs" #-}
                             )
                        -- "src-ag/ExecutionPlan2Hs.ag"(line 177, column 12)
                        _lhsOargnamesw =
                            ({-# LINE 177 "src-ag/ExecutionPlan2Hs.ag" #-}
                             if isJust virtual_
                             then empty
                             else if isNonterminal tp_
                                  then "(" >#< "sem_" >|< extractNonterminal tp_ >#< "field_" >|< name_ >#< ")"
                                  else text $ locname name_
                             {-# LINE 155 "src-ag/ExecutionPlan2Hs.hs" #-}
                             )
                        -- "src-ag/ExecutionPlan2Hs.ag"(line 324, column 12)
                        _lhsOargtps =
                            ({-# LINE 324 "src-ag/ExecutionPlan2Hs.ag" #-}
                             if isJust virtual_
                             then empty
                             else if isNonterminal tp_
                                  then ("T_" >|< extractNonterminal tp_) >#< "->"
                                  else (text $ show tp_) >#< "->"
                             {-# LINE 165 "src-ag/ExecutionPlan2Hs.hs" #-}
                             )
                        -- "src-ag/ExecutionPlan2Hs.ag"(line 329, column 12)
                        _argnames =
                            ({-# LINE 329 "src-ag/ExecutionPlan2Hs.ag" #-}
                             if isJust virtual_
                             then empty
                             else if isNonterminal tp_
                                  then "field_" >|< name_
                                  else text $ locname name_
                             {-# LINE 175 "src-ag/ExecutionPlan2Hs.hs" #-}
                             )
                        -- "src-ag/ExecutionPlan2Hs.ag"(line 390, column 12)
                        _lhsOchildintros =
                            ({-# LINE 390 "src-ag/ExecutionPlan2Hs.ag" #-}
                             Map.singleton name_ $ locname name_ >#< "<-" >#< "return" >#< "$" >#<
                             if isJust virtual_
                             then "sem_" >|< extractNonterminal tp_ >#< instname name_
                             else "field_" >|< name_
                             {-# LINE 184 "src-ag/ExecutionPlan2Hs.hs" #-}
                             )
                        -- "src-ag/ExecutionPlan2Hs.ag"(line 486, column 12)
                        _lhsOterminaldefs =
                            ({-# LINE 486 "src-ag/ExecutionPlan2Hs.ag" #-}
                             if isJust virtual_ || isNonterminal tp_
                             then Set.empty
                             else Set.singleton $ locname name_
                             {-# LINE 192 "src-ag/ExecutionPlan2Hs.hs" #-}
                             )
                        -- copy rule (from local)
                        _lhsOargnames =
                            ({-# LINE 319 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _argnames
                             {-# LINE 198 "src-ag/ExecutionPlan2Hs.hs" #-}
                             )
                    in  ( _lhsOargnames,_lhsOargnamesw,_lhsOargtps,_lhsOchildintros,_lhsOdatatype,_lhsOterminaldefs))) )
-- EChildren ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         nt                   : NontermIdent
         options              : Options
      synthesized attributes:
         argnames             :  [PP_Doc] 
         argnamesw            : [PP_Doc]
         argtps               :  [PP_Doc] 
         childintros          : Map.Map Identifier PP_Doc
         datatype             : [PP_Doc]
         terminaldefs         : Set.Set String
   alternatives:
      alternative Cons:
         child hd             : EChild 
         child tl             : EChildren 
      alternative Nil:
-}
-- cata
sem_EChildren :: EChildren  ->
                 T_EChildren 
sem_EChildren list  =
    (Prelude.foldr sem_EChildren_Cons sem_EChildren_Nil (Prelude.map sem_EChild list) )
-- semantic domain
newtype T_EChildren  = T_EChildren (NontermIdent ->
                                    Options ->
                                    ( ( [PP_Doc] ),([PP_Doc]),( [PP_Doc] ),(Map.Map Identifier PP_Doc),([PP_Doc]),(Set.Set String)))
data Inh_EChildren  = Inh_EChildren {nt_Inh_EChildren :: NontermIdent,options_Inh_EChildren :: Options}
data Syn_EChildren  = Syn_EChildren {argnames_Syn_EChildren :: ( [PP_Doc] ),argnamesw_Syn_EChildren :: ([PP_Doc]),argtps_Syn_EChildren :: ( [PP_Doc] ),childintros_Syn_EChildren :: (Map.Map Identifier PP_Doc),datatype_Syn_EChildren :: ([PP_Doc]),terminaldefs_Syn_EChildren :: (Set.Set String)}
wrap_EChildren :: T_EChildren  ->
                  Inh_EChildren  ->
                  Syn_EChildren 
wrap_EChildren (T_EChildren sem ) (Inh_EChildren _lhsInt _lhsIoptions )  =
    (let ( _lhsOargnames,_lhsOargnamesw,_lhsOargtps,_lhsOchildintros,_lhsOdatatype,_lhsOterminaldefs) = sem _lhsInt _lhsIoptions 
     in  (Syn_EChildren _lhsOargnames _lhsOargnamesw _lhsOargtps _lhsOchildintros _lhsOdatatype _lhsOterminaldefs ))
sem_EChildren_Cons :: T_EChild  ->
                      T_EChildren  ->
                      T_EChildren 
sem_EChildren_Cons (T_EChild hd_ ) (T_EChildren tl_ )  =
    (T_EChildren (\ _lhsInt
                    _lhsIoptions ->
                      (let _lhsOargnames :: ( [PP_Doc] )
                           _lhsOargnamesw :: ([PP_Doc])
                           _lhsOargtps :: ( [PP_Doc] )
                           _lhsOchildintros :: (Map.Map Identifier PP_Doc)
                           _lhsOdatatype :: ([PP_Doc])
                           _lhsOterminaldefs :: (Set.Set String)
                           _hdOnt :: NontermIdent
                           _hdOoptions :: Options
                           _tlOnt :: NontermIdent
                           _tlOoptions :: Options
                           _hdIargnames :: (  PP_Doc  )
                           _hdIargnamesw :: ( PP_Doc )
                           _hdIargtps :: (  PP_Doc  )
                           _hdIchildintros :: (Map.Map Identifier PP_Doc)
                           _hdIdatatype :: PP_Doc
                           _hdIterminaldefs :: (Set.Set String)
                           _tlIargnames :: ( [PP_Doc] )
                           _tlIargnamesw :: ([PP_Doc])
                           _tlIargtps :: ( [PP_Doc] )
                           _tlIchildintros :: (Map.Map Identifier PP_Doc)
                           _tlIdatatype :: ([PP_Doc])
                           _tlIterminaldefs :: (Set.Set String)
                           -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 321, column 31)
                           _lhsOargnames =
                               ({-# LINE 321 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _hdIargnames : _tlIargnames
                                {-# LINE 269 "src-ag/ExecutionPlan2Hs.hs" #-}
                                )
                           -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 174, column 32)
                           _lhsOargnamesw =
                               ({-# LINE 174 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _hdIargnamesw : _tlIargnamesw
                                {-# LINE 275 "src-ag/ExecutionPlan2Hs.hs" #-}
                                )
                           -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 320, column 31)
                           _lhsOargtps =
                               ({-# LINE 320 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _hdIargtps : _tlIargtps
                                {-# LINE 281 "src-ag/ExecutionPlan2Hs.hs" #-}
                                )
                           -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 381, column 41)
                           _lhsOchildintros =
                               ({-# LINE 381 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _hdIchildintros `Map.union` _tlIchildintros
                                {-# LINE 287 "src-ag/ExecutionPlan2Hs.hs" #-}
                                )
                           -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 124, column 51)
                           _lhsOdatatype =
                               ({-# LINE 124 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _hdIdatatype : _tlIdatatype
                                {-# LINE 293 "src-ag/ExecutionPlan2Hs.hs" #-}
                                )
                           -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 483, column 42)
                           _lhsOterminaldefs =
                               ({-# LINE 483 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _hdIterminaldefs `Set.union` _tlIterminaldefs
                                {-# LINE 299 "src-ag/ExecutionPlan2Hs.hs" #-}
                                )
                           -- copy rule (down)
                           _hdOnt =
                               ({-# LINE 123 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsInt
                                {-# LINE 305 "src-ag/ExecutionPlan2Hs.hs" #-}
                                )
                           -- copy rule (down)
                           _hdOoptions =
                               ({-# LINE 41 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsIoptions
                                {-# LINE 311 "src-ag/ExecutionPlan2Hs.hs" #-}
                                )
                           -- copy rule (down)
                           _tlOnt =
                               ({-# LINE 124 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsInt
                                {-# LINE 317 "src-ag/ExecutionPlan2Hs.hs" #-}
                                )
                           -- copy rule (down)
                           _tlOoptions =
                               ({-# LINE 41 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsIoptions
                                {-# LINE 323 "src-ag/ExecutionPlan2Hs.hs" #-}
                                )
                           ( _hdIargnames,_hdIargnamesw,_hdIargtps,_hdIchildintros,_hdIdatatype,_hdIterminaldefs) =
                               hd_ _hdOnt _hdOoptions 
                           ( _tlIargnames,_tlIargnamesw,_tlIargtps,_tlIchildintros,_tlIdatatype,_tlIterminaldefs) =
                               tl_ _tlOnt _tlOoptions 
                       in  ( _lhsOargnames,_lhsOargnamesw,_lhsOargtps,_lhsOchildintros,_lhsOdatatype,_lhsOterminaldefs))) )
sem_EChildren_Nil :: T_EChildren 
sem_EChildren_Nil  =
    (T_EChildren (\ _lhsInt
                    _lhsIoptions ->
                      (let _lhsOargnames :: ( [PP_Doc] )
                           _lhsOargnamesw :: ([PP_Doc])
                           _lhsOargtps :: ( [PP_Doc] )
                           _lhsOchildintros :: (Map.Map Identifier PP_Doc)
                           _lhsOdatatype :: ([PP_Doc])
                           _lhsOterminaldefs :: (Set.Set String)
                           -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 321, column 31)
                           _lhsOargnames =
                               ({-# LINE 321 "src-ag/ExecutionPlan2Hs.ag" #-}
                                []
                                {-# LINE 344 "src-ag/ExecutionPlan2Hs.hs" #-}
                                )
                           -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 174, column 32)
                           _lhsOargnamesw =
                               ({-# LINE 174 "src-ag/ExecutionPlan2Hs.ag" #-}
                                []
                                {-# LINE 350 "src-ag/ExecutionPlan2Hs.hs" #-}
                                )
                           -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 320, column 31)
                           _lhsOargtps =
                               ({-# LINE 320 "src-ag/ExecutionPlan2Hs.ag" #-}
                                []
                                {-# LINE 356 "src-ag/ExecutionPlan2Hs.hs" #-}
                                )
                           -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 381, column 41)
                           _lhsOchildintros =
                               ({-# LINE 381 "src-ag/ExecutionPlan2Hs.ag" #-}
                                Map.empty
                                {-# LINE 362 "src-ag/ExecutionPlan2Hs.hs" #-}
                                )
                           -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 124, column 51)
                           _lhsOdatatype =
                               ({-# LINE 124 "src-ag/ExecutionPlan2Hs.ag" #-}
                                []
                                {-# LINE 368 "src-ag/ExecutionPlan2Hs.hs" #-}
                                )
                           -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 483, column 42)
                           _lhsOterminaldefs =
                               ({-# LINE 483 "src-ag/ExecutionPlan2Hs.ag" #-}
                                Set.empty
                                {-# LINE 374 "src-ag/ExecutionPlan2Hs.hs" #-}
                                )
                       in  ( _lhsOargnames,_lhsOargnamesw,_lhsOargtps,_lhsOchildintros,_lhsOdatatype,_lhsOterminaldefs))) )
-- ENonterminal ------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allchildvisit        : Map.Map VisitIdentifier (Identifier -> PP_Doc)
         avisitdefs           : Map.Map VisitIdentifier (Set.Set Identifier)
         avisituses           : Map.Map VisitIdentifier (Set.Set Identifier)
         derivings            : Derivings
         importBlocks         : PP_Doc
         inhmap               : Map.Map NontermIdent Attributes
         mainFile             : String
         mainName             : String
         moduleHeader         : String -> String -> String -> Bool -> String
         options              : Options
         optionsLine          : String
         pragmaBlocks         : String
         synmap               : Map.Map NontermIdent Attributes
         textBlocks           : PP_Doc
         typeSyns             : TypeSyns
         wrappers             : Set.Set NontermIdent
      synthesized attributes:
         appendCommon         :  PP_Doc 
         appendMain           :  PP_Doc 
         childvisit           : Map.Map VisitIdentifier (Identifier -> PP_Doc)
         genProdIO            : IO ()
         imports              : [PP_Doc]
         output               : PP_Doc
         visitdefs            : Map.Map VisitIdentifier (Set.Set Identifier)
         visituses            : Map.Map VisitIdentifier (Set.Set Identifier)
   alternatives:
      alternative ENonterminal:
         child nt             : {NontermIdent}
         child params         : {[Identifier]}
         child initial        : {StateIdentifier}
         child initialv       : {Maybe VisitIdentifier}
         child prods          : EProductions 
         visit 0:
            local datatype    : _
            local derivings   : _
            local fsemname    : _
            local semname     : _
            local frecarg     : _
            local sem_nt      : _
            local outedges    : _
            local inedges     : _
            local allstates   : _
            local t_type      : _
            local t_init      : _
            local t_states    : _
            local k_type      : _
            local k_states    : _
            local wr_inh      : _
            local wr_syn      : _
            local genwrap     : _
            local wr_inhs     : _
            local wr_syns     : _
            local inhlist     : _
            local synlist     : _
            local wrapname    : _
            local inhname     : _
            local synname     : _
            local wrapper     : _
-}
-- cata
sem_ENonterminal :: ENonterminal  ->
                    T_ENonterminal 
sem_ENonterminal (ENonterminal _nt _params _initial _initialv _prods )  =
    (sem_ENonterminal_ENonterminal _nt _params _initial _initialv (sem_EProductions _prods ) )
-- semantic domain
newtype T_ENonterminal  = T_ENonterminal ((Map.Map VisitIdentifier (Identifier -> PP_Doc)) ->
                                          (Map.Map VisitIdentifier (Set.Set Identifier)) ->
                                          (Map.Map VisitIdentifier (Set.Set Identifier)) ->
                                          Derivings ->
                                          PP_Doc ->
                                          (Map.Map NontermIdent Attributes) ->
                                          String ->
                                          String ->
                                          (String -> String -> String -> Bool -> String) ->
                                          Options ->
                                          String ->
                                          String ->
                                          (Map.Map NontermIdent Attributes) ->
                                          PP_Doc ->
                                          TypeSyns ->
                                          (Set.Set NontermIdent) ->
                                          ( ( PP_Doc ),( PP_Doc ),(Map.Map VisitIdentifier (Identifier -> PP_Doc)),(IO ()),([PP_Doc]),PP_Doc,(Map.Map VisitIdentifier (Set.Set Identifier)),(Map.Map VisitIdentifier (Set.Set Identifier))))
data Inh_ENonterminal  = Inh_ENonterminal {allchildvisit_Inh_ENonterminal :: (Map.Map VisitIdentifier (Identifier -> PP_Doc)),avisitdefs_Inh_ENonterminal :: (Map.Map VisitIdentifier (Set.Set Identifier)),avisituses_Inh_ENonterminal :: (Map.Map VisitIdentifier (Set.Set Identifier)),derivings_Inh_ENonterminal :: Derivings,importBlocks_Inh_ENonterminal :: PP_Doc,inhmap_Inh_ENonterminal :: (Map.Map NontermIdent Attributes),mainFile_Inh_ENonterminal :: String,mainName_Inh_ENonterminal :: String,moduleHeader_Inh_ENonterminal :: (String -> String -> String -> Bool -> String),options_Inh_ENonterminal :: Options,optionsLine_Inh_ENonterminal :: String,pragmaBlocks_Inh_ENonterminal :: String,synmap_Inh_ENonterminal :: (Map.Map NontermIdent Attributes),textBlocks_Inh_ENonterminal :: PP_Doc,typeSyns_Inh_ENonterminal :: TypeSyns,wrappers_Inh_ENonterminal :: (Set.Set NontermIdent)}
data Syn_ENonterminal  = Syn_ENonterminal {appendCommon_Syn_ENonterminal :: ( PP_Doc ),appendMain_Syn_ENonterminal :: ( PP_Doc ),childvisit_Syn_ENonterminal :: (Map.Map VisitIdentifier (Identifier -> PP_Doc)),genProdIO_Syn_ENonterminal :: (IO ()),imports_Syn_ENonterminal :: ([PP_Doc]),output_Syn_ENonterminal :: PP_Doc,visitdefs_Syn_ENonterminal :: (Map.Map VisitIdentifier (Set.Set Identifier)),visituses_Syn_ENonterminal :: (Map.Map VisitIdentifier (Set.Set Identifier))}
wrap_ENonterminal :: T_ENonterminal  ->
                     Inh_ENonterminal  ->
                     Syn_ENonterminal 
wrap_ENonterminal (T_ENonterminal sem ) (Inh_ENonterminal _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIderivings _lhsIimportBlocks _lhsIinhmap _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsIoptions _lhsIoptionsLine _lhsIpragmaBlocks _lhsIsynmap _lhsItextBlocks _lhsItypeSyns _lhsIwrappers )  =
    (let ( _lhsOappendCommon,_lhsOappendMain,_lhsOchildvisit,_lhsOgenProdIO,_lhsOimports,_lhsOoutput,_lhsOvisitdefs,_lhsOvisituses) = sem _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIderivings _lhsIimportBlocks _lhsIinhmap _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsIoptions _lhsIoptionsLine _lhsIpragmaBlocks _lhsIsynmap _lhsItextBlocks _lhsItypeSyns _lhsIwrappers 
     in  (Syn_ENonterminal _lhsOappendCommon _lhsOappendMain _lhsOchildvisit _lhsOgenProdIO _lhsOimports _lhsOoutput _lhsOvisitdefs _lhsOvisituses ))
sem_ENonterminal_ENonterminal :: NontermIdent ->
                                 ([Identifier]) ->
                                 StateIdentifier ->
                                 (Maybe VisitIdentifier) ->
                                 T_EProductions  ->
                                 T_ENonterminal 
sem_ENonterminal_ENonterminal nt_ params_ initial_ initialv_ (T_EProductions prods_ )  =
    (T_ENonterminal (\ _lhsIallchildvisit
                       _lhsIavisitdefs
                       _lhsIavisituses
                       _lhsIderivings
                       _lhsIimportBlocks
                       _lhsIinhmap
                       _lhsImainFile
                       _lhsImainName
                       _lhsImoduleHeader
                       _lhsIoptions
                       _lhsIoptionsLine
                       _lhsIpragmaBlocks
                       _lhsIsynmap
                       _lhsItextBlocks
                       _lhsItypeSyns
                       _lhsIwrappers ->
                         (let _prodsOrename :: Bool
                              _lhsOoutput :: PP_Doc
                              _prodsOinhmap :: Attributes
                              _prodsOsynmap :: Attributes
                              _prodsOnt :: NontermIdent
                              _prodsOinitial :: StateIdentifier
                              _prodsOallstates :: (Set.Set StateIdentifier)
                              _lhsOappendMain :: ( PP_Doc )
                              _lhsOappendCommon :: ( PP_Doc )
                              _lhsOchildvisit :: (Map.Map VisitIdentifier (Identifier -> PP_Doc))
                              _lhsOgenProdIO :: (IO ())
                              _lhsOimports :: ([PP_Doc])
                              _lhsOvisitdefs :: (Map.Map VisitIdentifier (Set.Set Identifier))
                              _lhsOvisituses :: (Map.Map VisitIdentifier (Set.Set Identifier))
                              _prodsOallchildvisit :: (Map.Map VisitIdentifier (Identifier -> PP_Doc))
                              _prodsOavisitdefs :: (Map.Map VisitIdentifier (Set.Set Identifier))
                              _prodsOavisituses :: (Map.Map VisitIdentifier (Set.Set Identifier))
                              _prodsOimportBlocks :: PP_Doc
                              _prodsOmainFile :: String
                              _prodsOmainName :: String
                              _prodsOmoduleHeader :: (String -> String -> String -> Bool -> String)
                              _prodsOoptions :: Options
                              _prodsOoptionsLine :: String
                              _prodsOpragmaBlocks :: String
                              _prodsOtextBlocks :: PP_Doc
                              _prodsIallvisits :: ([VisitStateState])
                              _prodsIchildvisit :: (Map.Map VisitIdentifier (Identifier -> PP_Doc))
                              _prodsIdatatype :: ([PP_Doc])
                              _prodsIgenProdIO :: (IO ())
                              _prodsIimports :: ([PP_Doc])
                              _prodsIsem_nt :: PP_Doc
                              _prodsIsem_prod :: PP_Doc
                              _prodsIt_visits :: PP_Doc
                              _prodsIvisitdefs :: (Map.Map VisitIdentifier (Set.Set Identifier))
                              _prodsIvisituses :: (Map.Map VisitIdentifier (Set.Set Identifier))
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 45, column 18)
                              _prodsOrename =
                                  ({-# LINE 45 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   rename _lhsIoptions
                                   {-# LINE 533 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 62, column 18)
                              _lhsOoutput =
                                  ({-# LINE 62 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   ("-- " ++ getName nt_ ++ " " ++ replicate (60 - length (getName nt_)) '-')
                                   >-< (if dataTypes _lhsIoptions
                                        then "-- data"
                                             >-< _datatype
                                             >-< ""
                                        else empty)
                                   >-< (if nt_ `Set.member` _lhsIwrappers
                                        then "-- wrapper"
                                             >-< _wr_inh
                                             >-< _wr_syn
                                             >-< _wrapper
                                             >-< ""
                                        else empty)
                                   >-< (if   folds _lhsIoptions
                                        then "-- cata"
                                             >-< _sem_nt
                                             >-< ""
                                        else empty)
                                   >-< (if   semfuns _lhsIoptions
                                        then "-- semantic domain"
                                             >-< _t_init
                                             >-< _t_states
                                             >-< _k_states
                                             >-< _prodsIt_visits
                                             >-< _prodsIsem_prod
                                             >-< ""
                                        else empty)
                                   {-# LINE 565 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 101, column 18)
                              _datatype =
                                  ({-# LINE 101 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   case lookup nt_ _lhsItypeSyns of
                                      Nothing -> "data" >#< nt_ >#< (vlist $ ("=" >#< head _prodsIdatatype)
                                                  : (map ("|" >#<) $ tail _prodsIdatatype)) >#< _derivings
                                      Just (List t) -> "type" >#< nt_ >#< "=" >#< "[" >#< show t >#< "]"
                                      Just (Maybe t) -> "type" >#< nt_ >#< "=" >#< "Maybe" >#< show t
                                      Just (Tuple ts) -> "type" >#< nt_ >#< "=" >#< pp_parens (ppCommas $ map (show . snd) ts)
                                      Just (Either l r) -> "type" >#< nt_ >#< "=" >#< "Either" >#< show l >#< show r
                                      Just (Map k v) -> "type" >#< nt_ >#< "=" >#< "Data.Map.Map" >#< pp_parens (show k) >#< show v
                                      Just (IntMap t) -> "type" >#< nt_ >#< "=" >#< "Data.IntMap.IntMap" >#< show t
                                   {-# LINE 579 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 111, column 18)
                              _derivings =
                                  ({-# LINE 111 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   case Map.lookup nt_ _lhsIderivings of
                                      Nothing -> empty
                                      Just s  -> if   Set.null s
                                                 then empty
                                                 else "deriving" >#< (pp_parens $ ppCommas $ map pp $ Set.toList s)
                                   {-# LINE 589 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 136, column 18)
                              _fsemname =
                                  ({-# LINE 136 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   \x -> "sem_" ++ show x
                                   {-# LINE 595 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 137, column 18)
                              _semname =
                                  ({-# LINE 137 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _fsemname     nt_
                                   {-# LINE 601 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 138, column 18)
                              _frecarg =
                                  ({-# LINE 138 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   \t x -> case t of
                                              NT nt _ -> pp_parens (_fsemname nt >#< x)
                                              _       -> pp x
                                   {-# LINE 609 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 141, column 18)
                              _sem_nt =
                                  ({-# LINE 141 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _semname     >#< "::" >#< nt_ >#< "->" >#< _t_type
                                   >-< case lookup nt_ _lhsItypeSyns of
                                          Nothing -> _prodsIsem_nt
                                          Just (List t) -> _semname     >#< "list" >#< "=" >#< "Prelude.foldr" >#< _semname     >|< "_Cons"
                                                           >#< _semname     >|< "_Nil"
                                                           >#< case t of
                                                                  NT nt _ -> pp_parens ("Prelude.map" >#< _fsemname nt >#< "list")
                                                                  _ -> pp "list"
                                          Just (Maybe t) -> _semname     >#< "Prelude.Nothing" >#< "=" >#< _semname     >|< "_Nothing"
                                                            >-< _semname     >#< pp_parens ("Prelude.Just just") >#< "="
                                                            >#< _semname     >|< "_Just" >#< _frecarg t "just"
                                          Just (Tuple ts) -> _semname     >#< pp_parens (ppCommas $ map fst ts) >#< "="
                                                             >#< _semname     >|< "_Tuple" >#< ppSpaced (map (\t -> _frecarg (snd t) (show $ fst t)) ts)
                                          Just (Either l r) -> _semname     >#< "(Prelude.Left left)" >#< "=" >#< _semname     >|< "_Left" >#< _frecarg l "left"
                                                               >-< _semname     >#< "(Prelude.Right right)" >#< "=" >#< _semname     >|< "_Right" >#< _frecarg r "right"
                                          Just (Map k v) -> _semname     >#< "m" >#< "=" >#< "Data.Map.foldrWithKey"
                                                            >#< _semname     >|< "_Entry" >#< _semname     >|< "_Nil"
                                                            >#< case v of
                                                                   NT nt _ -> pp_parens ("Data.Map.map" >#< _fsemname nt >#< "m")
                                                                   _ -> pp "m"
                                          Just (IntMap v) -> _semname     >#< "m" >#< "=" >#< "Data.IntMap.foldWithKey"
                                                             >#< _semname     >|< "_Entry" >#< _semname     >|< "_Nil"
                                                             >#< case v of
                                                                    NT nt _ -> pp_parens ("Data.IntMap.map" >#< _fsemname nt >#< "m")
                                                                    _ -> pp "m"
                                   {-# LINE 639 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 198, column 19)
                              (Just _prodsOinhmap ) =
                                  ({-# LINE 198 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   Map.lookup nt_ _lhsIinhmap
                                   {-# LINE 645 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 199, column 19)
                              (Just _prodsOsynmap ) =
                                  ({-# LINE 199 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   Map.lookup nt_ _lhsIsynmap
                                   {-# LINE 651 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 220, column 18)
                              _outedges =
                                  ({-# LINE 220 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   Set.fromList $ map (\(_,f,_) -> f) _prodsIallvisits
                                   {-# LINE 657 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 221, column 18)
                              _inedges =
                                  ({-# LINE 221 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   Set.fromList $ map (\(_,_,t) -> t) _prodsIallvisits
                                   {-# LINE 663 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 222, column 18)
                              _allstates =
                                  ({-# LINE 222 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   Set.insert initial_ $ _inedges     `Set.union` _outedges
                                   {-# LINE 669 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 223, column 18)
                              _t_type =
                                  ({-# LINE 223 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   "T_" ++ show nt_
                                   {-# LINE 675 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 224, column 18)
                              _t_init =
                                  ({-# LINE 224 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   "type" >#< _t_type     >#< "=" >#< _t_type     >|< "_s" >|< initial_
                                   {-# LINE 681 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 225, column 18)
                              _t_states =
                                  ({-# LINE 225 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   vlist $ map (\st ->
                                      let nt_st = nt_ >|< "_s" >|< st
                                          t_st  = "T_" >|< nt_st
                                          k_st  = "K_" >|< nt_st
                                          c_st  = "C_" >|< nt_st
                                          inv_st  = "inv_" >|< nt_st
                                      in  "data" >#< t_st >#< "where" >#< c_st >#< "::" >#< "{" >#< inv_st >#< "::"
                                                 >#< "!" >|< pp_parens ("forall t." >#< k_st >#< "t" >#< "->" >#< "t") >#< "}"
                                                 >#< "->" >#< t_st
                                          ) $ Set.toList _allstates
                                   {-# LINE 696 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 238, column 18)
                              _k_type =
                                  ({-# LINE 238 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   "K_" ++ show nt_
                                   {-# LINE 702 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 239, column 18)
                              _k_states =
                                  ({-# LINE 239 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   vlist $ map (\st ->
                                      let nt_st = nt_ >|< "_s" >|< st
                                          k_st  = "K_" >|< nt_st
                                          outg  = filter (\(v,f,t) -> f == st) _prodsIallvisits
                                          visitlist = vlist $ map (\(v,f,t) ->
                                              _k_type     >|< "_v" >|< v >#< "::" >#< k_st >#< _t_type     >|< "_v" >|< v
                                               ) outg
                                      in  "data" >#< k_st >#< "k" >#< "where"
                                          >-< indent 3 visitlist) $ Set.toList _allstates
                                   {-# LINE 716 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 259, column 18)
                              _prodsOnt =
                                  ({-# LINE 259 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   nt_
                                   {-# LINE 722 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 276, column 18)
                              _wr_inh =
                                  ({-# LINE 276 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _genwrap     "Inh" _wr_inhs
                                   {-# LINE 728 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 277, column 18)
                              _wr_syn =
                                  ({-# LINE 277 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _genwrap     "Syn" _wr_syns
                                   {-# LINE 734 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 278, column 18)
                              _genwrap =
                                  ({-# LINE 278 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   \nm attr -> "data" >#< nm >|< "_" >|< nt_ >#< "=" >#< nm >|< "_" >|< nt_ >#< "{"
                                               >#< (ppCommas $ map (\(i,t) -> i >|< "_" >|< nm >|< "_" >|< nt_ >#< "::"
                                               >#< typeToHaskellString (Just nt_) [] t) attr) >#< "}"
                                   {-# LINE 742 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 281, column 18)
                              _wr_inhs =
                                  ({-# LINE 281 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   Map.toList $ fromJust $ Map.lookup nt_ _lhsIinhmap
                                   {-# LINE 748 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 282, column 18)
                              _wr_syns =
                                  ({-# LINE 282 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   Map.toList $ fromJust $ Map.lookup nt_ _lhsIsynmap
                                   {-# LINE 754 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 283, column 18)
                              _inhlist =
                                  ({-# LINE 283 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   map (lhsname True . fst) _wr_inhs
                                   {-# LINE 760 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 284, column 18)
                              _synlist =
                                  ({-# LINE 284 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   map (lhsname False . fst) _wr_syns
                                   {-# LINE 766 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 285, column 18)
                              _wrapname =
                                  ({-# LINE 285 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   "wrap_" ++ show nt_
                                   {-# LINE 772 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 286, column 18)
                              _inhname =
                                  ({-# LINE 286 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   "Inh_" ++ show nt_
                                   {-# LINE 778 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 287, column 18)
                              _synname =
                                  ({-# LINE 287 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   "Syn_" ++ show nt_
                                   {-# LINE 784 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 288, column 18)
                              _wrapper =
                                  ({-# LINE 288 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   (_wrapname     >#< "::" >#< _t_type     >#< "->"
                                    >#< _inhname     >#< "->" >#< _synname    )
                                   >-<
                                   (_wrapname     >#< "sem" >#< "(" >#< _inhname
                                    >#< ppSpaced _inhlist     >#< ")" >#< "=")
                                   >-<
                                   indent 3 (case initialv_ of
                                     Nothing -> _synname     >#< " { }"
                                     Just initv ->
                                        "let" >#< "(" >#< ppCommas _synlist     >#< "," >#< "_" >#< ")" >#< "="
                                              >#< "Control.Monad.Identity.runIdentity"
                                              >#< pp_parens ("inv_" >|< nt_ >|< "_s" >|< initial_
                                              >#< "sem" >#< _k_type     >|< "_v" >|< initv
                                              >#< ppSpaced _inhlist    )
                                        >-<
                                        "in " >#< "(" >#< _synname     >#< ppSpaced _synlist     >#< ")")
                                   {-# LINE 805 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 315, column 18)
                              _prodsOinitial =
                                  ({-# LINE 315 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   initial_
                                   {-# LINE 811 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 316, column 18)
                              _prodsOallstates =
                                  ({-# LINE 316 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _allstates
                                   {-# LINE 817 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 613, column 18)
                              _lhsOappendMain =
                                  ({-# LINE 613 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   (if nt_ `Set.member` _lhsIwrappers
                                    then     _wr_inh
                                         >-< _wr_syn
                                         >-< _wrapper
                                    else empty)
                                   >-< _sem_nt
                                   {-# LINE 828 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 619, column 18)
                              _lhsOappendCommon =
                                  ({-# LINE 619 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   (if dataTypes _lhsIoptions then _datatype     else empty)
                                   >-< _t_init
                                   >-< _t_states
                                   >-< _k_states
                                   >-< _prodsIt_visits
                                   {-# LINE 838 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 458, column 37)
                              _lhsOchildvisit =
                                  ({-# LINE 458 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _prodsIchildvisit
                                   {-# LINE 844 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 627, column 49)
                              _lhsOgenProdIO =
                                  ({-# LINE 627 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _prodsIgenProdIO
                                   {-# LINE 850 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 626, column 47)
                              _lhsOimports =
                                  ({-# LINE 626 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _prodsIimports
                                   {-# LINE 856 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 518, column 36)
                              _lhsOvisitdefs =
                                  ({-# LINE 518 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _prodsIvisitdefs
                                   {-# LINE 862 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 519, column 36)
                              _lhsOvisituses =
                                  ({-# LINE 519 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _prodsIvisituses
                                   {-# LINE 868 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- copy rule (down)
                              _prodsOallchildvisit =
                                  ({-# LINE 457 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIallchildvisit
                                   {-# LINE 874 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- copy rule (down)
                              _prodsOavisitdefs =
                                  ({-# LINE 528 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIavisitdefs
                                   {-# LINE 880 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- copy rule (down)
                              _prodsOavisituses =
                                  ({-# LINE 529 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIavisituses
                                   {-# LINE 886 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- copy rule (down)
                              _prodsOimportBlocks =
                                  ({-# LINE 25 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIimportBlocks
                                   {-# LINE 892 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- copy rule (down)
                              _prodsOmainFile =
                                  ({-# LINE 29 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsImainFile
                                   {-# LINE 898 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- copy rule (down)
                              _prodsOmainName =
                                  ({-# LINE 31 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsImainName
                                   {-# LINE 904 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- copy rule (down)
                              _prodsOmoduleHeader =
                                  ({-# LINE 28 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsImoduleHeader
                                   {-# LINE 910 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- copy rule (down)
                              _prodsOoptions =
                                  ({-# LINE 41 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIoptions
                                   {-# LINE 916 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- copy rule (down)
                              _prodsOoptionsLine =
                                  ({-# LINE 30 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIoptionsLine
                                   {-# LINE 922 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- copy rule (down)
                              _prodsOpragmaBlocks =
                                  ({-# LINE 26 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIpragmaBlocks
                                   {-# LINE 928 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- copy rule (down)
                              _prodsOtextBlocks =
                                  ({-# LINE 27 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsItextBlocks
                                   {-# LINE 934 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              ( _prodsIallvisits,_prodsIchildvisit,_prodsIdatatype,_prodsIgenProdIO,_prodsIimports,_prodsIsem_nt,_prodsIsem_prod,_prodsIt_visits,_prodsIvisitdefs,_prodsIvisituses) =
                                  prods_ _prodsOallchildvisit _prodsOallstates _prodsOavisitdefs _prodsOavisituses _prodsOimportBlocks _prodsOinhmap _prodsOinitial _prodsOmainFile _prodsOmainName _prodsOmoduleHeader _prodsOnt _prodsOoptions _prodsOoptionsLine _prodsOpragmaBlocks _prodsOrename _prodsOsynmap _prodsOtextBlocks 
                          in  ( _lhsOappendCommon,_lhsOappendMain,_lhsOchildvisit,_lhsOgenProdIO,_lhsOimports,_lhsOoutput,_lhsOvisitdefs,_lhsOvisituses))) )
-- ENonterminals -----------------------------------------------
{-
   visit 0:
      inherited attributes:
         allchildvisit        : Map.Map VisitIdentifier (Identifier -> PP_Doc)
         avisitdefs           : Map.Map VisitIdentifier (Set.Set Identifier)
         avisituses           : Map.Map VisitIdentifier (Set.Set Identifier)
         derivings            : Derivings
         importBlocks         : PP_Doc
         inhmap               : Map.Map NontermIdent Attributes
         mainFile             : String
         mainName             : String
         moduleHeader         : String -> String -> String -> Bool -> String
         options              : Options
         optionsLine          : String
         pragmaBlocks         : String
         synmap               : Map.Map NontermIdent Attributes
         textBlocks           : PP_Doc
         typeSyns             : TypeSyns
         wrappers             : Set.Set NontermIdent
      synthesized attributes:
         appendCommon         : [PP_Doc]
         appendMain           : [PP_Doc]
         childvisit           : Map.Map VisitIdentifier (Identifier -> PP_Doc)
         genProdIO            : IO ()
         imports              : [PP_Doc]
         output               : PP_Doc
         visitdefs            : Map.Map VisitIdentifier (Set.Set Identifier)
         visituses            : Map.Map VisitIdentifier (Set.Set Identifier)
   alternatives:
      alternative Cons:
         child hd             : ENonterminal 
         child tl             : ENonterminals 
      alternative Nil:
-}
-- cata
sem_ENonterminals :: ENonterminals  ->
                     T_ENonterminals 
sem_ENonterminals list  =
    (Prelude.foldr sem_ENonterminals_Cons sem_ENonterminals_Nil (Prelude.map sem_ENonterminal list) )
-- semantic domain
newtype T_ENonterminals  = T_ENonterminals ((Map.Map VisitIdentifier (Identifier -> PP_Doc)) ->
                                            (Map.Map VisitIdentifier (Set.Set Identifier)) ->
                                            (Map.Map VisitIdentifier (Set.Set Identifier)) ->
                                            Derivings ->
                                            PP_Doc ->
                                            (Map.Map NontermIdent Attributes) ->
                                            String ->
                                            String ->
                                            (String -> String -> String -> Bool -> String) ->
                                            Options ->
                                            String ->
                                            String ->
                                            (Map.Map NontermIdent Attributes) ->
                                            PP_Doc ->
                                            TypeSyns ->
                                            (Set.Set NontermIdent) ->
                                            ( ([PP_Doc]),([PP_Doc]),(Map.Map VisitIdentifier (Identifier -> PP_Doc)),(IO ()),([PP_Doc]),PP_Doc,(Map.Map VisitIdentifier (Set.Set Identifier)),(Map.Map VisitIdentifier (Set.Set Identifier))))
data Inh_ENonterminals  = Inh_ENonterminals {allchildvisit_Inh_ENonterminals :: (Map.Map VisitIdentifier (Identifier -> PP_Doc)),avisitdefs_Inh_ENonterminals :: (Map.Map VisitIdentifier (Set.Set Identifier)),avisituses_Inh_ENonterminals :: (Map.Map VisitIdentifier (Set.Set Identifier)),derivings_Inh_ENonterminals :: Derivings,importBlocks_Inh_ENonterminals :: PP_Doc,inhmap_Inh_ENonterminals :: (Map.Map NontermIdent Attributes),mainFile_Inh_ENonterminals :: String,mainName_Inh_ENonterminals :: String,moduleHeader_Inh_ENonterminals :: (String -> String -> String -> Bool -> String),options_Inh_ENonterminals :: Options,optionsLine_Inh_ENonterminals :: String,pragmaBlocks_Inh_ENonterminals :: String,synmap_Inh_ENonterminals :: (Map.Map NontermIdent Attributes),textBlocks_Inh_ENonterminals :: PP_Doc,typeSyns_Inh_ENonterminals :: TypeSyns,wrappers_Inh_ENonterminals :: (Set.Set NontermIdent)}
data Syn_ENonterminals  = Syn_ENonterminals {appendCommon_Syn_ENonterminals :: ([PP_Doc]),appendMain_Syn_ENonterminals :: ([PP_Doc]),childvisit_Syn_ENonterminals :: (Map.Map VisitIdentifier (Identifier -> PP_Doc)),genProdIO_Syn_ENonterminals :: (IO ()),imports_Syn_ENonterminals :: ([PP_Doc]),output_Syn_ENonterminals :: PP_Doc,visitdefs_Syn_ENonterminals :: (Map.Map VisitIdentifier (Set.Set Identifier)),visituses_Syn_ENonterminals :: (Map.Map VisitIdentifier (Set.Set Identifier))}
wrap_ENonterminals :: T_ENonterminals  ->
                      Inh_ENonterminals  ->
                      Syn_ENonterminals 
wrap_ENonterminals (T_ENonterminals sem ) (Inh_ENonterminals _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIderivings _lhsIimportBlocks _lhsIinhmap _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsIoptions _lhsIoptionsLine _lhsIpragmaBlocks _lhsIsynmap _lhsItextBlocks _lhsItypeSyns _lhsIwrappers )  =
    (let ( _lhsOappendCommon,_lhsOappendMain,_lhsOchildvisit,_lhsOgenProdIO,_lhsOimports,_lhsOoutput,_lhsOvisitdefs,_lhsOvisituses) = sem _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIderivings _lhsIimportBlocks _lhsIinhmap _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsIoptions _lhsIoptionsLine _lhsIpragmaBlocks _lhsIsynmap _lhsItextBlocks _lhsItypeSyns _lhsIwrappers 
     in  (Syn_ENonterminals _lhsOappendCommon _lhsOappendMain _lhsOchildvisit _lhsOgenProdIO _lhsOimports _lhsOoutput _lhsOvisitdefs _lhsOvisituses ))
sem_ENonterminals_Cons :: T_ENonterminal  ->
                          T_ENonterminals  ->
                          T_ENonterminals 
sem_ENonterminals_Cons (T_ENonterminal hd_ ) (T_ENonterminals tl_ )  =
    (T_ENonterminals (\ _lhsIallchildvisit
                        _lhsIavisitdefs
                        _lhsIavisituses
                        _lhsIderivings
                        _lhsIimportBlocks
                        _lhsIinhmap
                        _lhsImainFile
                        _lhsImainName
                        _lhsImoduleHeader
                        _lhsIoptions
                        _lhsIoptionsLine
                        _lhsIpragmaBlocks
                        _lhsIsynmap
                        _lhsItextBlocks
                        _lhsItypeSyns
                        _lhsIwrappers ->
                          (let _lhsOappendCommon :: ([PP_Doc])
                               _lhsOappendMain :: ([PP_Doc])
                               _lhsOchildvisit :: (Map.Map VisitIdentifier (Identifier -> PP_Doc))
                               _lhsOgenProdIO :: (IO ())
                               _lhsOimports :: ([PP_Doc])
                               _lhsOoutput :: PP_Doc
                               _lhsOvisitdefs :: (Map.Map VisitIdentifier (Set.Set Identifier))
                               _lhsOvisituses :: (Map.Map VisitIdentifier (Set.Set Identifier))
                               _hdOallchildvisit :: (Map.Map VisitIdentifier (Identifier -> PP_Doc))
                               _hdOavisitdefs :: (Map.Map VisitIdentifier (Set.Set Identifier))
                               _hdOavisituses :: (Map.Map VisitIdentifier (Set.Set Identifier))
                               _hdOderivings :: Derivings
                               _hdOimportBlocks :: PP_Doc
                               _hdOinhmap :: (Map.Map NontermIdent Attributes)
                               _hdOmainFile :: String
                               _hdOmainName :: String
                               _hdOmoduleHeader :: (String -> String -> String -> Bool -> String)
                               _hdOoptions :: Options
                               _hdOoptionsLine :: String
                               _hdOpragmaBlocks :: String
                               _hdOsynmap :: (Map.Map NontermIdent Attributes)
                               _hdOtextBlocks :: PP_Doc
                               _hdOtypeSyns :: TypeSyns
                               _hdOwrappers :: (Set.Set NontermIdent)
                               _tlOallchildvisit :: (Map.Map VisitIdentifier (Identifier -> PP_Doc))
                               _tlOavisitdefs :: (Map.Map VisitIdentifier (Set.Set Identifier))
                               _tlOavisituses :: (Map.Map VisitIdentifier (Set.Set Identifier))
                               _tlOderivings :: Derivings
                               _tlOimportBlocks :: PP_Doc
                               _tlOinhmap :: (Map.Map NontermIdent Attributes)
                               _tlOmainFile :: String
                               _tlOmainName :: String
                               _tlOmoduleHeader :: (String -> String -> String -> Bool -> String)
                               _tlOoptions :: Options
                               _tlOoptionsLine :: String
                               _tlOpragmaBlocks :: String
                               _tlOsynmap :: (Map.Map NontermIdent Attributes)
                               _tlOtextBlocks :: PP_Doc
                               _tlOtypeSyns :: TypeSyns
                               _tlOwrappers :: (Set.Set NontermIdent)
                               _hdIappendCommon :: ( PP_Doc )
                               _hdIappendMain :: ( PP_Doc )
                               _hdIchildvisit :: (Map.Map VisitIdentifier (Identifier -> PP_Doc))
                               _hdIgenProdIO :: (IO ())
                               _hdIimports :: ([PP_Doc])
                               _hdIoutput :: PP_Doc
                               _hdIvisitdefs :: (Map.Map VisitIdentifier (Set.Set Identifier))
                               _hdIvisituses :: (Map.Map VisitIdentifier (Set.Set Identifier))
                               _tlIappendCommon :: ([PP_Doc])
                               _tlIappendMain :: ([PP_Doc])
                               _tlIchildvisit :: (Map.Map VisitIdentifier (Identifier -> PP_Doc))
                               _tlIgenProdIO :: (IO ())
                               _tlIimports :: ([PP_Doc])
                               _tlIoutput :: PP_Doc
                               _tlIvisitdefs :: (Map.Map VisitIdentifier (Set.Set Identifier))
                               _tlIvisituses :: (Map.Map VisitIdentifier (Set.Set Identifier))
                               -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 610, column 51)
                               _lhsOappendCommon =
                                   ({-# LINE 610 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _hdIappendCommon : _tlIappendCommon
                                    {-# LINE 1085 "src-ag/ExecutionPlan2Hs.hs" #-}
                                    )
                               -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 610, column 51)
                               _lhsOappendMain =
                                   ({-# LINE 610 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _hdIappendMain : _tlIappendMain
                                    {-# LINE 1091 "src-ag/ExecutionPlan2Hs.hs" #-}
                                    )
                               -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 458, column 37)
                               _lhsOchildvisit =
                                   ({-# LINE 458 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _hdIchildvisit `Map.union` _tlIchildvisit
                                    {-# LINE 1097 "src-ag/ExecutionPlan2Hs.hs" #-}
                                    )
                               -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 627, column 49)
                               _lhsOgenProdIO =
                                   ({-# LINE 627 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _hdIgenProdIO >> _tlIgenProdIO
                                    {-# LINE 1103 "src-ag/ExecutionPlan2Hs.hs" #-}
                                    )
                               -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 626, column 47)
                               _lhsOimports =
                                   ({-# LINE 626 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _hdIimports ++ _tlIimports
                                    {-# LINE 1109 "src-ag/ExecutionPlan2Hs.hs" #-}
                                    )
                               -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 56, column 45)
                               _lhsOoutput =
                                   ({-# LINE 56 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _hdIoutput >-< _tlIoutput
                                    {-# LINE 1115 "src-ag/ExecutionPlan2Hs.hs" #-}
                                    )
                               -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 518, column 36)
                               _lhsOvisitdefs =
                                   ({-# LINE 518 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _hdIvisitdefs `uwSetUnion` _tlIvisitdefs
                                    {-# LINE 1121 "src-ag/ExecutionPlan2Hs.hs" #-}
                                    )
                               -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 519, column 36)
                               _lhsOvisituses =
                                   ({-# LINE 519 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _hdIvisituses `uwSetUnion` _tlIvisituses
                                    {-# LINE 1127 "src-ag/ExecutionPlan2Hs.hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOallchildvisit =
                                   ({-# LINE 457 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsIallchildvisit
                                    {-# LINE 1133 "src-ag/ExecutionPlan2Hs.hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOavisitdefs =
                                   ({-# LINE 528 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsIavisitdefs
                                    {-# LINE 1139 "src-ag/ExecutionPlan2Hs.hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOavisituses =
                                   ({-# LINE 529 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsIavisituses
                                    {-# LINE 1145 "src-ag/ExecutionPlan2Hs.hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOderivings =
                                   ({-# LINE 94 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsIderivings
                                    {-# LINE 1151 "src-ag/ExecutionPlan2Hs.hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOimportBlocks =
                                   ({-# LINE 25 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsIimportBlocks
                                    {-# LINE 1157 "src-ag/ExecutionPlan2Hs.hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOinhmap =
                                   ({-# LINE 188 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsIinhmap
                                    {-# LINE 1163 "src-ag/ExecutionPlan2Hs.hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOmainFile =
                                   ({-# LINE 29 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsImainFile
                                    {-# LINE 1169 "src-ag/ExecutionPlan2Hs.hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOmainName =
                                   ({-# LINE 31 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsImainName
                                    {-# LINE 1175 "src-ag/ExecutionPlan2Hs.hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOmoduleHeader =
                                   ({-# LINE 28 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsImoduleHeader
                                    {-# LINE 1181 "src-ag/ExecutionPlan2Hs.hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOoptions =
                                   ({-# LINE 41 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsIoptions
                                    {-# LINE 1187 "src-ag/ExecutionPlan2Hs.hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOoptionsLine =
                                   ({-# LINE 30 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsIoptionsLine
                                    {-# LINE 1193 "src-ag/ExecutionPlan2Hs.hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOpragmaBlocks =
                                   ({-# LINE 26 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsIpragmaBlocks
                                    {-# LINE 1199 "src-ag/ExecutionPlan2Hs.hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOsynmap =
                                   ({-# LINE 189 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsIsynmap
                                    {-# LINE 1205 "src-ag/ExecutionPlan2Hs.hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOtextBlocks =
                                   ({-# LINE 27 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsItextBlocks
                                    {-# LINE 1211 "src-ag/ExecutionPlan2Hs.hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOtypeSyns =
                                   ({-# LINE 93 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsItypeSyns
                                    {-# LINE 1217 "src-ag/ExecutionPlan2Hs.hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOwrappers =
                                   ({-# LINE 55 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsIwrappers
                                    {-# LINE 1223 "src-ag/ExecutionPlan2Hs.hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOallchildvisit =
                                   ({-# LINE 457 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsIallchildvisit
                                    {-# LINE 1229 "src-ag/ExecutionPlan2Hs.hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOavisitdefs =
                                   ({-# LINE 528 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsIavisitdefs
                                    {-# LINE 1235 "src-ag/ExecutionPlan2Hs.hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOavisituses =
                                   ({-# LINE 529 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsIavisituses
                                    {-# LINE 1241 "src-ag/ExecutionPlan2Hs.hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOderivings =
                                   ({-# LINE 94 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsIderivings
                                    {-# LINE 1247 "src-ag/ExecutionPlan2Hs.hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOimportBlocks =
                                   ({-# LINE 25 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsIimportBlocks
                                    {-# LINE 1253 "src-ag/ExecutionPlan2Hs.hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOinhmap =
                                   ({-# LINE 188 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsIinhmap
                                    {-# LINE 1259 "src-ag/ExecutionPlan2Hs.hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOmainFile =
                                   ({-# LINE 29 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsImainFile
                                    {-# LINE 1265 "src-ag/ExecutionPlan2Hs.hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOmainName =
                                   ({-# LINE 31 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsImainName
                                    {-# LINE 1271 "src-ag/ExecutionPlan2Hs.hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOmoduleHeader =
                                   ({-# LINE 28 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsImoduleHeader
                                    {-# LINE 1277 "src-ag/ExecutionPlan2Hs.hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOoptions =
                                   ({-# LINE 41 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsIoptions
                                    {-# LINE 1283 "src-ag/ExecutionPlan2Hs.hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOoptionsLine =
                                   ({-# LINE 30 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsIoptionsLine
                                    {-# LINE 1289 "src-ag/ExecutionPlan2Hs.hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOpragmaBlocks =
                                   ({-# LINE 26 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsIpragmaBlocks
                                    {-# LINE 1295 "src-ag/ExecutionPlan2Hs.hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOsynmap =
                                   ({-# LINE 189 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsIsynmap
                                    {-# LINE 1301 "src-ag/ExecutionPlan2Hs.hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOtextBlocks =
                                   ({-# LINE 27 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsItextBlocks
                                    {-# LINE 1307 "src-ag/ExecutionPlan2Hs.hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOtypeSyns =
                                   ({-# LINE 93 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsItypeSyns
                                    {-# LINE 1313 "src-ag/ExecutionPlan2Hs.hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOwrappers =
                                   ({-# LINE 55 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsIwrappers
                                    {-# LINE 1319 "src-ag/ExecutionPlan2Hs.hs" #-}
                                    )
                               ( _hdIappendCommon,_hdIappendMain,_hdIchildvisit,_hdIgenProdIO,_hdIimports,_hdIoutput,_hdIvisitdefs,_hdIvisituses) =
                                   hd_ _hdOallchildvisit _hdOavisitdefs _hdOavisituses _hdOderivings _hdOimportBlocks _hdOinhmap _hdOmainFile _hdOmainName _hdOmoduleHeader _hdOoptions _hdOoptionsLine _hdOpragmaBlocks _hdOsynmap _hdOtextBlocks _hdOtypeSyns _hdOwrappers 
                               ( _tlIappendCommon,_tlIappendMain,_tlIchildvisit,_tlIgenProdIO,_tlIimports,_tlIoutput,_tlIvisitdefs,_tlIvisituses) =
                                   tl_ _tlOallchildvisit _tlOavisitdefs _tlOavisituses _tlOderivings _tlOimportBlocks _tlOinhmap _tlOmainFile _tlOmainName _tlOmoduleHeader _tlOoptions _tlOoptionsLine _tlOpragmaBlocks _tlOsynmap _tlOtextBlocks _tlOtypeSyns _tlOwrappers 
                           in  ( _lhsOappendCommon,_lhsOappendMain,_lhsOchildvisit,_lhsOgenProdIO,_lhsOimports,_lhsOoutput,_lhsOvisitdefs,_lhsOvisituses))) )
sem_ENonterminals_Nil :: T_ENonterminals 
sem_ENonterminals_Nil  =
    (T_ENonterminals (\ _lhsIallchildvisit
                        _lhsIavisitdefs
                        _lhsIavisituses
                        _lhsIderivings
                        _lhsIimportBlocks
                        _lhsIinhmap
                        _lhsImainFile
                        _lhsImainName
                        _lhsImoduleHeader
                        _lhsIoptions
                        _lhsIoptionsLine
                        _lhsIpragmaBlocks
                        _lhsIsynmap
                        _lhsItextBlocks
                        _lhsItypeSyns
                        _lhsIwrappers ->
                          (let _lhsOappendCommon :: ([PP_Doc])
                               _lhsOappendMain :: ([PP_Doc])
                               _lhsOchildvisit :: (Map.Map VisitIdentifier (Identifier -> PP_Doc))
                               _lhsOgenProdIO :: (IO ())
                               _lhsOimports :: ([PP_Doc])
                               _lhsOoutput :: PP_Doc
                               _lhsOvisitdefs :: (Map.Map VisitIdentifier (Set.Set Identifier))
                               _lhsOvisituses :: (Map.Map VisitIdentifier (Set.Set Identifier))
                               -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 610, column 51)
                               _lhsOappendCommon =
                                   ({-# LINE 610 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    []
                                    {-# LINE 1356 "src-ag/ExecutionPlan2Hs.hs" #-}
                                    )
                               -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 610, column 51)
                               _lhsOappendMain =
                                   ({-# LINE 610 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    []
                                    {-# LINE 1362 "src-ag/ExecutionPlan2Hs.hs" #-}
                                    )
                               -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 458, column 37)
                               _lhsOchildvisit =
                                   ({-# LINE 458 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    Map.empty
                                    {-# LINE 1368 "src-ag/ExecutionPlan2Hs.hs" #-}
                                    )
                               -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 627, column 49)
                               _lhsOgenProdIO =
                                   ({-# LINE 627 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    return ()
                                    {-# LINE 1374 "src-ag/ExecutionPlan2Hs.hs" #-}
                                    )
                               -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 626, column 47)
                               _lhsOimports =
                                   ({-# LINE 626 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    []
                                    {-# LINE 1380 "src-ag/ExecutionPlan2Hs.hs" #-}
                                    )
                               -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 56, column 45)
                               _lhsOoutput =
                                   ({-# LINE 56 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    empty
                                    {-# LINE 1386 "src-ag/ExecutionPlan2Hs.hs" #-}
                                    )
                               -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 518, column 36)
                               _lhsOvisitdefs =
                                   ({-# LINE 518 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    Map.empty
                                    {-# LINE 1392 "src-ag/ExecutionPlan2Hs.hs" #-}
                                    )
                               -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 519, column 36)
                               _lhsOvisituses =
                                   ({-# LINE 519 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    Map.empty
                                    {-# LINE 1398 "src-ag/ExecutionPlan2Hs.hs" #-}
                                    )
                           in  ( _lhsOappendCommon,_lhsOappendMain,_lhsOchildvisit,_lhsOgenProdIO,_lhsOimports,_lhsOoutput,_lhsOvisitdefs,_lhsOvisituses))) )
-- EProduction -------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allchildvisit        : Map.Map VisitIdentifier (Identifier -> PP_Doc)
         allstates            : Set.Set StateIdentifier
         avisitdefs           : Map.Map VisitIdentifier (Set.Set Identifier)
         avisituses           : Map.Map VisitIdentifier (Set.Set Identifier)
         importBlocks         : PP_Doc
         inhmap               : Attributes
         initial              : StateIdentifier
         mainFile             : String
         mainName             : String
         moduleHeader         : String -> String -> String -> Bool -> String
         nt                   : NontermIdent
         options              : Options
         optionsLine          : String
         pragmaBlocks         : String
         rename               : Bool
         synmap               : Attributes
         textBlocks           : PP_Doc
      synthesized attributes:
         allvisits            : [VisitStateState]
         childvisit           : Map.Map VisitIdentifier (Identifier -> PP_Doc)
         datatype             : PP_Doc
         genProdIO            : IO ()
         imports              : [PP_Doc]
         sem_nt               : PP_Doc
         sem_prod             : PP_Doc
         t_visits             : PP_Doc
         visitdefs            : Map.Map VisitIdentifier (Set.Set Identifier)
         visituses            : Map.Map VisitIdentifier (Set.Set Identifier)
   alternatives:
      alternative EProduction:
         child con            : {ConstructorIdent}
         child rules          : ERules 
         child children       : EChildren 
         child visits         : Visits 
         visit 0:
            local args        : _
            local semname     : _
            local sem_prod    : _
            local statefns    : _
            local stargs      : _
            local stks        : _
            local stvisits    : _
            local stvs        : _
            local moduleName  : _
            local suffix      : _
            local outputfile  : _
-}
-- cata
sem_EProduction :: EProduction  ->
                   T_EProduction 
sem_EProduction (EProduction _con _rules _children _visits )  =
    (sem_EProduction_EProduction _con (sem_ERules _rules ) (sem_EChildren _children ) (sem_Visits _visits ) )
-- semantic domain
newtype T_EProduction  = T_EProduction ((Map.Map VisitIdentifier (Identifier -> PP_Doc)) ->
                                        (Set.Set StateIdentifier) ->
                                        (Map.Map VisitIdentifier (Set.Set Identifier)) ->
                                        (Map.Map VisitIdentifier (Set.Set Identifier)) ->
                                        PP_Doc ->
                                        Attributes ->
                                        StateIdentifier ->
                                        String ->
                                        String ->
                                        (String -> String -> String -> Bool -> String) ->
                                        NontermIdent ->
                                        Options ->
                                        String ->
                                        String ->
                                        Bool ->
                                        Attributes ->
                                        PP_Doc ->
                                        ( ([VisitStateState]),(Map.Map VisitIdentifier (Identifier -> PP_Doc)),PP_Doc,(IO ()),([PP_Doc]),PP_Doc,PP_Doc,PP_Doc,(Map.Map VisitIdentifier (Set.Set Identifier)),(Map.Map VisitIdentifier (Set.Set Identifier))))
data Inh_EProduction  = Inh_EProduction {allchildvisit_Inh_EProduction :: (Map.Map VisitIdentifier (Identifier -> PP_Doc)),allstates_Inh_EProduction :: (Set.Set StateIdentifier),avisitdefs_Inh_EProduction :: (Map.Map VisitIdentifier (Set.Set Identifier)),avisituses_Inh_EProduction :: (Map.Map VisitIdentifier (Set.Set Identifier)),importBlocks_Inh_EProduction :: PP_Doc,inhmap_Inh_EProduction :: Attributes,initial_Inh_EProduction :: StateIdentifier,mainFile_Inh_EProduction :: String,mainName_Inh_EProduction :: String,moduleHeader_Inh_EProduction :: (String -> String -> String -> Bool -> String),nt_Inh_EProduction :: NontermIdent,options_Inh_EProduction :: Options,optionsLine_Inh_EProduction :: String,pragmaBlocks_Inh_EProduction :: String,rename_Inh_EProduction :: Bool,synmap_Inh_EProduction :: Attributes,textBlocks_Inh_EProduction :: PP_Doc}
data Syn_EProduction  = Syn_EProduction {allvisits_Syn_EProduction :: ([VisitStateState]),childvisit_Syn_EProduction :: (Map.Map VisitIdentifier (Identifier -> PP_Doc)),datatype_Syn_EProduction :: PP_Doc,genProdIO_Syn_EProduction :: (IO ()),imports_Syn_EProduction :: ([PP_Doc]),sem_nt_Syn_EProduction :: PP_Doc,sem_prod_Syn_EProduction :: PP_Doc,t_visits_Syn_EProduction :: PP_Doc,visitdefs_Syn_EProduction :: (Map.Map VisitIdentifier (Set.Set Identifier)),visituses_Syn_EProduction :: (Map.Map VisitIdentifier (Set.Set Identifier))}
wrap_EProduction :: T_EProduction  ->
                    Inh_EProduction  ->
                    Syn_EProduction 
wrap_EProduction (T_EProduction sem ) (Inh_EProduction _lhsIallchildvisit _lhsIallstates _lhsIavisitdefs _lhsIavisituses _lhsIimportBlocks _lhsIinhmap _lhsIinitial _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInt _lhsIoptions _lhsIoptionsLine _lhsIpragmaBlocks _lhsIrename _lhsIsynmap _lhsItextBlocks )  =
    (let ( _lhsOallvisits,_lhsOchildvisit,_lhsOdatatype,_lhsOgenProdIO,_lhsOimports,_lhsOsem_nt,_lhsOsem_prod,_lhsOt_visits,_lhsOvisitdefs,_lhsOvisituses) = sem _lhsIallchildvisit _lhsIallstates _lhsIavisitdefs _lhsIavisituses _lhsIimportBlocks _lhsIinhmap _lhsIinitial _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInt _lhsIoptions _lhsIoptionsLine _lhsIpragmaBlocks _lhsIrename _lhsIsynmap _lhsItextBlocks 
     in  (Syn_EProduction _lhsOallvisits _lhsOchildvisit _lhsOdatatype _lhsOgenProdIO _lhsOimports _lhsOsem_nt _lhsOsem_prod _lhsOt_visits _lhsOvisitdefs _lhsOvisituses ))
sem_EProduction_EProduction :: ConstructorIdent ->
                               T_ERules  ->
                               T_EChildren  ->
                               T_Visits  ->
                               T_EProduction 
sem_EProduction_EProduction con_ (T_ERules rules_ ) (T_EChildren children_ ) (T_Visits visits_ )  =
    (T_EProduction (\ _lhsIallchildvisit
                      _lhsIallstates
                      _lhsIavisitdefs
                      _lhsIavisituses
                      _lhsIimportBlocks
                      _lhsIinhmap
                      _lhsIinitial
                      _lhsImainFile
                      _lhsImainName
                      _lhsImoduleHeader
                      _lhsInt
                      _lhsIoptions
                      _lhsIoptionsLine
                      _lhsIpragmaBlocks
                      _lhsIrename
                      _lhsIsynmap
                      _lhsItextBlocks ->
                        (let _lhsOdatatype :: PP_Doc
                             _lhsOsem_nt :: PP_Doc
                             _visitsOmrules :: (Map.Map Identifier PP_Doc)
                             _visitsOchildintros :: (Map.Map Identifier PP_Doc)
                             _rulesOusedrules :: (Set.Set Identifier)
                             _visitsOallintramap :: (Map.Map StateIdentifier (Set.Set String))
                             _visitsOterminaldefs :: (Set.Set String)
                             _visitsOruledefs :: (Map.Map Identifier (Set.Set String))
                             _visitsOruleuses :: (Map.Map Identifier (Set.Set String))
                             _lhsOimports :: ([PP_Doc])
                             _lhsOgenProdIO :: (IO ())
                             _lhsOchildvisit :: (Map.Map VisitIdentifier (Identifier -> PP_Doc))
                             _lhsOt_visits :: PP_Doc
                             _lhsOvisitdefs :: (Map.Map VisitIdentifier (Set.Set Identifier))
                             _lhsOvisituses :: (Map.Map VisitIdentifier (Set.Set Identifier))
                             _lhsOallvisits :: ([VisitStateState])
                             _lhsOsem_prod :: PP_Doc
                             _rulesOoptions :: Options
                             _childrenOnt :: NontermIdent
                             _childrenOoptions :: Options
                             _visitsOallchildvisit :: (Map.Map VisitIdentifier (Identifier -> PP_Doc))
                             _visitsOavisitdefs :: (Map.Map VisitIdentifier (Set.Set Identifier))
                             _visitsOavisituses :: (Map.Map VisitIdentifier (Set.Set Identifier))
                             _visitsOinhmap :: Attributes
                             _visitsOnt :: NontermIdent
                             _visitsOoptions :: Options
                             _visitsOsynmap :: Attributes
                             _rulesImrules :: (Map.Map Identifier PP_Doc)
                             _rulesIruledefs :: (Map.Map Identifier (Set.Set String))
                             _rulesIruleuses :: (Map.Map Identifier (Set.Set String))
                             _rulesIsem_rules :: PP_Doc
                             _childrenIargnames :: ( [PP_Doc] )
                             _childrenIargnamesw :: ([PP_Doc])
                             _childrenIargtps :: ( [PP_Doc] )
                             _childrenIchildintros :: (Map.Map Identifier PP_Doc)
                             _childrenIdatatype :: ([PP_Doc])
                             _childrenIterminaldefs :: (Set.Set String)
                             _visitsIallvisits :: ([VisitStateState])
                             _visitsIchildvisit :: (Map.Map VisitIdentifier (Identifier -> PP_Doc))
                             _visitsIintramap :: (Map.Map StateIdentifier (Set.Set String))
                             _visitsIsem_visit :: ( [(StateIdentifier,PP_Doc)] )
                             _visitsIt_visits :: PP_Doc
                             _visitsIusedrules :: (Set.Set Identifier)
                             _visitsIvisitdefs :: (Map.Map VisitIdentifier (Set.Set Identifier))
                             _visitsIvisituses :: (Map.Map VisitIdentifier (Set.Set Identifier))
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 121, column 17)
                             _lhsOdatatype =
                                 ({-# LINE 121 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  conname _lhsIrename _lhsInt con_ >#< ppSpaced _childrenIdatatype
                                  {-# LINE 1556 "src-ag/ExecutionPlan2Hs.hs" #-}
                                  )
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 170, column 17)
                             _lhsOsem_nt =
                                 ({-# LINE 170 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  "sem_" >|< _lhsInt >#< "(" >#< conname _lhsIrename _lhsInt con_ >#< ppSpaced _childrenIargnames >#< ")"
                                  >#< "=" >#< "sem_" >|< _lhsInt >|< "_" >|< con_ >#< ppSpaced _childrenIargnamesw
                                  {-# LINE 1563 "src-ag/ExecutionPlan2Hs.hs" #-}
                                  )
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 336, column 17)
                             _args =
                                 ({-# LINE 336 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _childrenIargnames
                                  {-# LINE 1569 "src-ag/ExecutionPlan2Hs.hs" #-}
                                  )
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 337, column 17)
                             _semname =
                                 ({-# LINE 337 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  "sem_" ++ show _lhsInt ++ "_" ++ show con_
                                  {-# LINE 1575 "src-ag/ExecutionPlan2Hs.hs" #-}
                                  )
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 338, column 17)
                             _sem_prod =
                                 ({-# LINE 338 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _semname     >#< "::" >#< ppSpaced _childrenIargtps >#< "T_" >|< _lhsInt
                                  >-< _semname     >#< ppSpaced _args     >#< "="
                                  >#< "st" >|< _lhsIinitial >#< "where"
                                  >-< (indent 3 $ vlist _statefns     >-< _rulesIsem_rules)
                                  {-# LINE 1584 "src-ag/ExecutionPlan2Hs.hs" #-}
                                  )
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 342, column 17)
                             _statefns =
                                 ({-# LINE 342 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  map (\st -> "st" >|< st >#< _stargs     st >#< "=" >#<
                                              "C_" >|< _lhsInt >|< "_s" >|< st >#< "k" >|< st >#< "where"
                                              >-< indent 3 (_stks     st >-< _stvs     st)
                                      ) $ Set.toList _lhsIallstates
                                  {-# LINE 1593 "src-ag/ExecutionPlan2Hs.hs" #-}
                                  )
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 346, column 17)
                             _stargs =
                                 ({-# LINE 346 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  \st -> ppSpaced $ Set.toList $ maybe Set.empty id $ Map.lookup st _visitsIintramap
                                  {-# LINE 1599 "src-ag/ExecutionPlan2Hs.hs" #-}
                                  )
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 347, column 17)
                             _stks =
                                 ({-# LINE 347 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  \st -> "k" >|< st >#< "::" >#< "K_" >|< _lhsInt >|< "_s" >|< st >#< "t" >#< "->" >#< "t"
                                         >-< vlist (map (\(v,f,t) -> "k" >|< st >#< "K_" >|< _lhsInt >|< "_v" >|< v >#< "="
                                                                     >#< "v" >|< v) $ _stvisits     st)
                                     >-< if null (_stvisits     st)
                                         then "k" >|< st >#< "_" >#< "=" >#< "error \"unreachable\""
                                         else empty
                                  {-# LINE 1610 "src-ag/ExecutionPlan2Hs.hs" #-}
                                  )
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 353, column 17)
                             _stvisits =
                                 ({-# LINE 353 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  \st -> filter (\(v,f,t) -> f == st) _visitsIallvisits
                                  {-# LINE 1616 "src-ag/ExecutionPlan2Hs.hs" #-}
                                  )
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 354, column 17)
                             _stvs =
                                 ({-# LINE 354 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  \st -> vlist $ map snd $ filter (\(f,pp) -> f == st) _visitsIsem_visit
                                  {-# LINE 1622 "src-ag/ExecutionPlan2Hs.hs" #-}
                                  )
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 355, column 17)
                             _visitsOmrules =
                                 ({-# LINE 355 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _rulesImrules
                                  {-# LINE 1628 "src-ag/ExecutionPlan2Hs.hs" #-}
                                  )
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 387, column 17)
                             _visitsOchildintros =
                                 ({-# LINE 387 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _childrenIchildintros
                                  {-# LINE 1634 "src-ag/ExecutionPlan2Hs.hs" #-}
                                  )
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 403, column 17)
                             _rulesOusedrules =
                                 ({-# LINE 403 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _visitsIusedrules
                                  {-# LINE 1640 "src-ag/ExecutionPlan2Hs.hs" #-}
                                  )
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 491, column 17)
                             _visitsOallintramap =
                                 ({-# LINE 491 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _visitsIintramap
                                  {-# LINE 1646 "src-ag/ExecutionPlan2Hs.hs" #-}
                                  )
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 492, column 17)
                             _visitsOterminaldefs =
                                 ({-# LINE 492 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _childrenIterminaldefs
                                  {-# LINE 1652 "src-ag/ExecutionPlan2Hs.hs" #-}
                                  )
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 511, column 17)
                             _visitsOruledefs =
                                 ({-# LINE 511 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _rulesIruledefs
                                  {-# LINE 1658 "src-ag/ExecutionPlan2Hs.hs" #-}
                                  )
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 512, column 17)
                             _visitsOruleuses =
                                 ({-# LINE 512 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _rulesIruleuses
                                  {-# LINE 1664 "src-ag/ExecutionPlan2Hs.hs" #-}
                                  )
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 630, column 17)
                             _lhsOimports =
                                 ({-# LINE 630 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  [pp $ "import " ++ _moduleName    ]
                                  {-# LINE 1670 "src-ag/ExecutionPlan2Hs.hs" #-}
                                  )
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 631, column 17)
                             _moduleName =
                                 ({-# LINE 631 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _lhsImainName ++ _suffix
                                  {-# LINE 1676 "src-ag/ExecutionPlan2Hs.hs" #-}
                                  )
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 632, column 17)
                             _suffix =
                                 ({-# LINE 632 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  "_" ++ show _lhsInt ++ "_" ++ show con_
                                  {-# LINE 1682 "src-ag/ExecutionPlan2Hs.hs" #-}
                                  )
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 633, column 17)
                             _outputfile =
                                 ({-# LINE 633 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _lhsImainFile ++ _suffix     ++ ".hs"
                                  {-# LINE 1688 "src-ag/ExecutionPlan2Hs.hs" #-}
                                  )
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 634, column 17)
                             _lhsOgenProdIO =
                                 ({-# LINE 634 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  writeModule _outputfile
                                    [ pp "{-# LANGUAGE Rank2Types, GADTs, EmptyDataDecls #-}"
                                    , pp $ _lhsIpragmaBlocks
                                    , pp $ _lhsIoptionsLine
                                    , pp $ _lhsImoduleHeader _lhsImainName _suffix     _semname     True
                                    , pp $ "import Control.Monad.Identity"
                                    , pp $ "import " ++ _lhsImainName ++ "_common"
                                    , _sem_prod
                                    ]
                                  {-# LINE 1702 "src-ag/ExecutionPlan2Hs.hs" #-}
                                  )
                             -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 458, column 37)
                             _lhsOchildvisit =
                                 ({-# LINE 458 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _visitsIchildvisit
                                  {-# LINE 1708 "src-ag/ExecutionPlan2Hs.hs" #-}
                                  )
                             -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 253, column 54)
                             _lhsOt_visits =
                                 ({-# LINE 253 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _visitsIt_visits
                                  {-# LINE 1714 "src-ag/ExecutionPlan2Hs.hs" #-}
                                  )
                             -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 518, column 36)
                             _lhsOvisitdefs =
                                 ({-# LINE 518 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _visitsIvisitdefs
                                  {-# LINE 1720 "src-ag/ExecutionPlan2Hs.hs" #-}
                                  )
                             -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 519, column 36)
                             _lhsOvisituses =
                                 ({-# LINE 519 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _visitsIvisituses
                                  {-# LINE 1726 "src-ag/ExecutionPlan2Hs.hs" #-}
                                  )
                             -- copy rule (up)
                             _lhsOallvisits =
                                 ({-# LINE 209 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _visitsIallvisits
                                  {-# LINE 1732 "src-ag/ExecutionPlan2Hs.hs" #-}
                                  )
                             -- copy rule (from local)
                             _lhsOsem_prod =
                                 ({-# LINE 309 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _sem_prod
                                  {-# LINE 1738 "src-ag/ExecutionPlan2Hs.hs" #-}
                                  )
                             -- copy rule (down)
                             _rulesOoptions =
                                 ({-# LINE 41 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _lhsIoptions
                                  {-# LINE 1744 "src-ag/ExecutionPlan2Hs.hs" #-}
                                  )
                             -- copy rule (down)
                             _childrenOnt =
                                 ({-# LINE 124 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _lhsInt
                                  {-# LINE 1750 "src-ag/ExecutionPlan2Hs.hs" #-}
                                  )
                             -- copy rule (down)
                             _childrenOoptions =
                                 ({-# LINE 41 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _lhsIoptions
                                  {-# LINE 1756 "src-ag/ExecutionPlan2Hs.hs" #-}
                                  )
                             -- copy rule (down)
                             _visitsOallchildvisit =
                                 ({-# LINE 457 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _lhsIallchildvisit
                                  {-# LINE 1762 "src-ag/ExecutionPlan2Hs.hs" #-}
                                  )
                             -- copy rule (down)
                             _visitsOavisitdefs =
                                 ({-# LINE 528 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _lhsIavisitdefs
                                  {-# LINE 1768 "src-ag/ExecutionPlan2Hs.hs" #-}
                                  )
                             -- copy rule (down)
                             _visitsOavisituses =
                                 ({-# LINE 529 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _lhsIavisituses
                                  {-# LINE 1774 "src-ag/ExecutionPlan2Hs.hs" #-}
                                  )
                             -- copy rule (down)
                             _visitsOinhmap =
                                 ({-# LINE 194 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _lhsIinhmap
                                  {-# LINE 1780 "src-ag/ExecutionPlan2Hs.hs" #-}
                                  )
                             -- copy rule (down)
                             _visitsOnt =
                                 ({-# LINE 253 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _lhsInt
                                  {-# LINE 1786 "src-ag/ExecutionPlan2Hs.hs" #-}
                                  )
                             -- copy rule (down)
                             _visitsOoptions =
                                 ({-# LINE 41 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _lhsIoptions
                                  {-# LINE 1792 "src-ag/ExecutionPlan2Hs.hs" #-}
                                  )
                             -- copy rule (down)
                             _visitsOsynmap =
                                 ({-# LINE 195 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _lhsIsynmap
                                  {-# LINE 1798 "src-ag/ExecutionPlan2Hs.hs" #-}
                                  )
                             ( _rulesImrules,_rulesIruledefs,_rulesIruleuses,_rulesIsem_rules) =
                                 rules_ _rulesOoptions _rulesOusedrules 
                             ( _childrenIargnames,_childrenIargnamesw,_childrenIargtps,_childrenIchildintros,_childrenIdatatype,_childrenIterminaldefs) =
                                 children_ _childrenOnt _childrenOoptions 
                             ( _visitsIallvisits,_visitsIchildvisit,_visitsIintramap,_visitsIsem_visit,_visitsIt_visits,_visitsIusedrules,_visitsIvisitdefs,_visitsIvisituses) =
                                 visits_ _visitsOallchildvisit _visitsOallintramap _visitsOavisitdefs _visitsOavisituses _visitsOchildintros _visitsOinhmap _visitsOmrules _visitsOnt _visitsOoptions _visitsOruledefs _visitsOruleuses _visitsOsynmap _visitsOterminaldefs 
                         in  ( _lhsOallvisits,_lhsOchildvisit,_lhsOdatatype,_lhsOgenProdIO,_lhsOimports,_lhsOsem_nt,_lhsOsem_prod,_lhsOt_visits,_lhsOvisitdefs,_lhsOvisituses))) )
-- EProductions ------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allchildvisit        : Map.Map VisitIdentifier (Identifier -> PP_Doc)
         allstates            : Set.Set StateIdentifier
         avisitdefs           : Map.Map VisitIdentifier (Set.Set Identifier)
         avisituses           : Map.Map VisitIdentifier (Set.Set Identifier)
         importBlocks         : PP_Doc
         inhmap               : Attributes
         initial              : StateIdentifier
         mainFile             : String
         mainName             : String
         moduleHeader         : String -> String -> String -> Bool -> String
         nt                   : NontermIdent
         options              : Options
         optionsLine          : String
         pragmaBlocks         : String
         rename               : Bool
         synmap               : Attributes
         textBlocks           : PP_Doc
      synthesized attributes:
         allvisits            : [VisitStateState]
         childvisit           : Map.Map VisitIdentifier (Identifier -> PP_Doc)
         datatype             : [PP_Doc]
         genProdIO            : IO ()
         imports              : [PP_Doc]
         sem_nt               : PP_Doc
         sem_prod             : PP_Doc
         t_visits             : PP_Doc
         visitdefs            : Map.Map VisitIdentifier (Set.Set Identifier)
         visituses            : Map.Map VisitIdentifier (Set.Set Identifier)
   alternatives:
      alternative Cons:
         child hd             : EProduction 
         child tl             : EProductions 
      alternative Nil:
-}
-- cata
sem_EProductions :: EProductions  ->
                    T_EProductions 
sem_EProductions list  =
    (Prelude.foldr sem_EProductions_Cons sem_EProductions_Nil (Prelude.map sem_EProduction list) )
-- semantic domain
newtype T_EProductions  = T_EProductions ((Map.Map VisitIdentifier (Identifier -> PP_Doc)) ->
                                          (Set.Set StateIdentifier) ->
                                          (Map.Map VisitIdentifier (Set.Set Identifier)) ->
                                          (Map.Map VisitIdentifier (Set.Set Identifier)) ->
                                          PP_Doc ->
                                          Attributes ->
                                          StateIdentifier ->
                                          String ->
                                          String ->
                                          (String -> String -> String -> Bool -> String) ->
                                          NontermIdent ->
                                          Options ->
                                          String ->
                                          String ->
                                          Bool ->
                                          Attributes ->
                                          PP_Doc ->
                                          ( ([VisitStateState]),(Map.Map VisitIdentifier (Identifier -> PP_Doc)),([PP_Doc]),(IO ()),([PP_Doc]),PP_Doc,PP_Doc,PP_Doc,(Map.Map VisitIdentifier (Set.Set Identifier)),(Map.Map VisitIdentifier (Set.Set Identifier))))
data Inh_EProductions  = Inh_EProductions {allchildvisit_Inh_EProductions :: (Map.Map VisitIdentifier (Identifier -> PP_Doc)),allstates_Inh_EProductions :: (Set.Set StateIdentifier),avisitdefs_Inh_EProductions :: (Map.Map VisitIdentifier (Set.Set Identifier)),avisituses_Inh_EProductions :: (Map.Map VisitIdentifier (Set.Set Identifier)),importBlocks_Inh_EProductions :: PP_Doc,inhmap_Inh_EProductions :: Attributes,initial_Inh_EProductions :: StateIdentifier,mainFile_Inh_EProductions :: String,mainName_Inh_EProductions :: String,moduleHeader_Inh_EProductions :: (String -> String -> String -> Bool -> String),nt_Inh_EProductions :: NontermIdent,options_Inh_EProductions :: Options,optionsLine_Inh_EProductions :: String,pragmaBlocks_Inh_EProductions :: String,rename_Inh_EProductions :: Bool,synmap_Inh_EProductions :: Attributes,textBlocks_Inh_EProductions :: PP_Doc}
data Syn_EProductions  = Syn_EProductions {allvisits_Syn_EProductions :: ([VisitStateState]),childvisit_Syn_EProductions :: (Map.Map VisitIdentifier (Identifier -> PP_Doc)),datatype_Syn_EProductions :: ([PP_Doc]),genProdIO_Syn_EProductions :: (IO ()),imports_Syn_EProductions :: ([PP_Doc]),sem_nt_Syn_EProductions :: PP_Doc,sem_prod_Syn_EProductions :: PP_Doc,t_visits_Syn_EProductions :: PP_Doc,visitdefs_Syn_EProductions :: (Map.Map VisitIdentifier (Set.Set Identifier)),visituses_Syn_EProductions :: (Map.Map VisitIdentifier (Set.Set Identifier))}
wrap_EProductions :: T_EProductions  ->
                     Inh_EProductions  ->
                     Syn_EProductions 
wrap_EProductions (T_EProductions sem ) (Inh_EProductions _lhsIallchildvisit _lhsIallstates _lhsIavisitdefs _lhsIavisituses _lhsIimportBlocks _lhsIinhmap _lhsIinitial _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInt _lhsIoptions _lhsIoptionsLine _lhsIpragmaBlocks _lhsIrename _lhsIsynmap _lhsItextBlocks )  =
    (let ( _lhsOallvisits,_lhsOchildvisit,_lhsOdatatype,_lhsOgenProdIO,_lhsOimports,_lhsOsem_nt,_lhsOsem_prod,_lhsOt_visits,_lhsOvisitdefs,_lhsOvisituses) = sem _lhsIallchildvisit _lhsIallstates _lhsIavisitdefs _lhsIavisituses _lhsIimportBlocks _lhsIinhmap _lhsIinitial _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInt _lhsIoptions _lhsIoptionsLine _lhsIpragmaBlocks _lhsIrename _lhsIsynmap _lhsItextBlocks 
     in  (Syn_EProductions _lhsOallvisits _lhsOchildvisit _lhsOdatatype _lhsOgenProdIO _lhsOimports _lhsOsem_nt _lhsOsem_prod _lhsOt_visits _lhsOvisitdefs _lhsOvisituses ))
sem_EProductions_Cons :: T_EProduction  ->
                         T_EProductions  ->
                         T_EProductions 
sem_EProductions_Cons (T_EProduction hd_ ) (T_EProductions tl_ )  =
    (T_EProductions (\ _lhsIallchildvisit
                       _lhsIallstates
                       _lhsIavisitdefs
                       _lhsIavisituses
                       _lhsIimportBlocks
                       _lhsIinhmap
                       _lhsIinitial
                       _lhsImainFile
                       _lhsImainName
                       _lhsImoduleHeader
                       _lhsInt
                       _lhsIoptions
                       _lhsIoptionsLine
                       _lhsIpragmaBlocks
                       _lhsIrename
                       _lhsIsynmap
                       _lhsItextBlocks ->
                         (let _lhsOallvisits :: ([VisitStateState])
                              _lhsOt_visits :: PP_Doc
                              _lhsOchildvisit :: (Map.Map VisitIdentifier (Identifier -> PP_Doc))
                              _lhsOdatatype :: ([PP_Doc])
                              _lhsOgenProdIO :: (IO ())
                              _lhsOimports :: ([PP_Doc])
                              _lhsOsem_nt :: PP_Doc
                              _lhsOsem_prod :: PP_Doc
                              _lhsOvisitdefs :: (Map.Map VisitIdentifier (Set.Set Identifier))
                              _lhsOvisituses :: (Map.Map VisitIdentifier (Set.Set Identifier))
                              _hdOallchildvisit :: (Map.Map VisitIdentifier (Identifier -> PP_Doc))
                              _hdOallstates :: (Set.Set StateIdentifier)
                              _hdOavisitdefs :: (Map.Map VisitIdentifier (Set.Set Identifier))
                              _hdOavisituses :: (Map.Map VisitIdentifier (Set.Set Identifier))
                              _hdOimportBlocks :: PP_Doc
                              _hdOinhmap :: Attributes
                              _hdOinitial :: StateIdentifier
                              _hdOmainFile :: String
                              _hdOmainName :: String
                              _hdOmoduleHeader :: (String -> String -> String -> Bool -> String)
                              _hdOnt :: NontermIdent
                              _hdOoptions :: Options
                              _hdOoptionsLine :: String
                              _hdOpragmaBlocks :: String
                              _hdOrename :: Bool
                              _hdOsynmap :: Attributes
                              _hdOtextBlocks :: PP_Doc
                              _tlOallchildvisit :: (Map.Map VisitIdentifier (Identifier -> PP_Doc))
                              _tlOallstates :: (Set.Set StateIdentifier)
                              _tlOavisitdefs :: (Map.Map VisitIdentifier (Set.Set Identifier))
                              _tlOavisituses :: (Map.Map VisitIdentifier (Set.Set Identifier))
                              _tlOimportBlocks :: PP_Doc
                              _tlOinhmap :: Attributes
                              _tlOinitial :: StateIdentifier
                              _tlOmainFile :: String
                              _tlOmainName :: String
                              _tlOmoduleHeader :: (String -> String -> String -> Bool -> String)
                              _tlOnt :: NontermIdent
                              _tlOoptions :: Options
                              _tlOoptionsLine :: String
                              _tlOpragmaBlocks :: String
                              _tlOrename :: Bool
                              _tlOsynmap :: Attributes
                              _tlOtextBlocks :: PP_Doc
                              _hdIallvisits :: ([VisitStateState])
                              _hdIchildvisit :: (Map.Map VisitIdentifier (Identifier -> PP_Doc))
                              _hdIdatatype :: PP_Doc
                              _hdIgenProdIO :: (IO ())
                              _hdIimports :: ([PP_Doc])
                              _hdIsem_nt :: PP_Doc
                              _hdIsem_prod :: PP_Doc
                              _hdIt_visits :: PP_Doc
                              _hdIvisitdefs :: (Map.Map VisitIdentifier (Set.Set Identifier))
                              _hdIvisituses :: (Map.Map VisitIdentifier (Set.Set Identifier))
                              _tlIallvisits :: ([VisitStateState])
                              _tlIchildvisit :: (Map.Map VisitIdentifier (Identifier -> PP_Doc))
                              _tlIdatatype :: ([PP_Doc])
                              _tlIgenProdIO :: (IO ())
                              _tlIimports :: ([PP_Doc])
                              _tlIsem_nt :: PP_Doc
                              _tlIsem_prod :: PP_Doc
                              _tlIt_visits :: PP_Doc
                              _tlIvisitdefs :: (Map.Map VisitIdentifier (Set.Set Identifier))
                              _tlIvisituses :: (Map.Map VisitIdentifier (Set.Set Identifier))
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 215, column 10)
                              _lhsOallvisits =
                                  ({-# LINE 215 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _hdIallvisits
                                   {-# LINE 1966 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 256, column 10)
                              _lhsOt_visits =
                                  ({-# LINE 256 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _hdIt_visits
                                   {-# LINE 1972 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 458, column 37)
                              _lhsOchildvisit =
                                  ({-# LINE 458 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _hdIchildvisit `Map.union` _tlIchildvisit
                                   {-# LINE 1978 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 118, column 34)
                              _lhsOdatatype =
                                  ({-# LINE 118 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _hdIdatatype : _tlIdatatype
                                   {-# LINE 1984 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 627, column 49)
                              _lhsOgenProdIO =
                                  ({-# LINE 627 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _hdIgenProdIO >> _tlIgenProdIO
                                   {-# LINE 1990 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 626, column 47)
                              _lhsOimports =
                                  ({-# LINE 626 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _hdIimports ++ _tlIimports
                                   {-# LINE 1996 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 168, column 44)
                              _lhsOsem_nt =
                                  ({-# LINE 168 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _hdIsem_nt >-< _tlIsem_nt
                                   {-# LINE 2002 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 310, column 34)
                              _lhsOsem_prod =
                                  ({-# LINE 310 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _hdIsem_prod >-< _tlIsem_prod
                                   {-# LINE 2008 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 518, column 36)
                              _lhsOvisitdefs =
                                  ({-# LINE 518 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _hdIvisitdefs `uwSetUnion` _tlIvisitdefs
                                   {-# LINE 2014 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 519, column 36)
                              _lhsOvisituses =
                                  ({-# LINE 519 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _hdIvisituses `uwSetUnion` _tlIvisituses
                                   {-# LINE 2020 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOallchildvisit =
                                  ({-# LINE 457 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIallchildvisit
                                   {-# LINE 2026 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOallstates =
                                  ({-# LINE 313 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIallstates
                                   {-# LINE 2032 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOavisitdefs =
                                  ({-# LINE 528 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIavisitdefs
                                   {-# LINE 2038 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOavisituses =
                                  ({-# LINE 529 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIavisituses
                                   {-# LINE 2044 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOimportBlocks =
                                  ({-# LINE 25 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIimportBlocks
                                   {-# LINE 2050 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOinhmap =
                                  ({-# LINE 194 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIinhmap
                                   {-# LINE 2056 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOinitial =
                                  ({-# LINE 312 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIinitial
                                   {-# LINE 2062 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOmainFile =
                                  ({-# LINE 29 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsImainFile
                                   {-# LINE 2068 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOmainName =
                                  ({-# LINE 31 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsImainName
                                   {-# LINE 2074 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOmoduleHeader =
                                  ({-# LINE 28 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsImoduleHeader
                                   {-# LINE 2080 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOnt =
                                  ({-# LINE 253 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsInt
                                   {-# LINE 2086 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOoptions =
                                  ({-# LINE 41 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIoptions
                                   {-# LINE 2092 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOoptionsLine =
                                  ({-# LINE 30 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIoptionsLine
                                   {-# LINE 2098 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOpragmaBlocks =
                                  ({-# LINE 26 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIpragmaBlocks
                                   {-# LINE 2104 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOrename =
                                  ({-# LINE 42 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIrename
                                   {-# LINE 2110 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOsynmap =
                                  ({-# LINE 195 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIsynmap
                                   {-# LINE 2116 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOtextBlocks =
                                  ({-# LINE 27 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsItextBlocks
                                   {-# LINE 2122 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOallchildvisit =
                                  ({-# LINE 457 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIallchildvisit
                                   {-# LINE 2128 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOallstates =
                                  ({-# LINE 313 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIallstates
                                   {-# LINE 2134 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOavisitdefs =
                                  ({-# LINE 528 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIavisitdefs
                                   {-# LINE 2140 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOavisituses =
                                  ({-# LINE 529 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIavisituses
                                   {-# LINE 2146 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOimportBlocks =
                                  ({-# LINE 25 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIimportBlocks
                                   {-# LINE 2152 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOinhmap =
                                  ({-# LINE 194 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIinhmap
                                   {-# LINE 2158 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOinitial =
                                  ({-# LINE 312 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIinitial
                                   {-# LINE 2164 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOmainFile =
                                  ({-# LINE 29 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsImainFile
                                   {-# LINE 2170 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOmainName =
                                  ({-# LINE 31 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsImainName
                                   {-# LINE 2176 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOmoduleHeader =
                                  ({-# LINE 28 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsImoduleHeader
                                   {-# LINE 2182 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOnt =
                                  ({-# LINE 253 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsInt
                                   {-# LINE 2188 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOoptions =
                                  ({-# LINE 41 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIoptions
                                   {-# LINE 2194 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOoptionsLine =
                                  ({-# LINE 30 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIoptionsLine
                                   {-# LINE 2200 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOpragmaBlocks =
                                  ({-# LINE 26 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIpragmaBlocks
                                   {-# LINE 2206 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOrename =
                                  ({-# LINE 42 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIrename
                                   {-# LINE 2212 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOsynmap =
                                  ({-# LINE 195 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIsynmap
                                   {-# LINE 2218 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOtextBlocks =
                                  ({-# LINE 27 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsItextBlocks
                                   {-# LINE 2224 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              ( _hdIallvisits,_hdIchildvisit,_hdIdatatype,_hdIgenProdIO,_hdIimports,_hdIsem_nt,_hdIsem_prod,_hdIt_visits,_hdIvisitdefs,_hdIvisituses) =
                                  hd_ _hdOallchildvisit _hdOallstates _hdOavisitdefs _hdOavisituses _hdOimportBlocks _hdOinhmap _hdOinitial _hdOmainFile _hdOmainName _hdOmoduleHeader _hdOnt _hdOoptions _hdOoptionsLine _hdOpragmaBlocks _hdOrename _hdOsynmap _hdOtextBlocks 
                              ( _tlIallvisits,_tlIchildvisit,_tlIdatatype,_tlIgenProdIO,_tlIimports,_tlIsem_nt,_tlIsem_prod,_tlIt_visits,_tlIvisitdefs,_tlIvisituses) =
                                  tl_ _tlOallchildvisit _tlOallstates _tlOavisitdefs _tlOavisituses _tlOimportBlocks _tlOinhmap _tlOinitial _tlOmainFile _tlOmainName _tlOmoduleHeader _tlOnt _tlOoptions _tlOoptionsLine _tlOpragmaBlocks _tlOrename _tlOsynmap _tlOtextBlocks 
                          in  ( _lhsOallvisits,_lhsOchildvisit,_lhsOdatatype,_lhsOgenProdIO,_lhsOimports,_lhsOsem_nt,_lhsOsem_prod,_lhsOt_visits,_lhsOvisitdefs,_lhsOvisituses))) )
sem_EProductions_Nil :: T_EProductions 
sem_EProductions_Nil  =
    (T_EProductions (\ _lhsIallchildvisit
                       _lhsIallstates
                       _lhsIavisitdefs
                       _lhsIavisituses
                       _lhsIimportBlocks
                       _lhsIinhmap
                       _lhsIinitial
                       _lhsImainFile
                       _lhsImainName
                       _lhsImoduleHeader
                       _lhsInt
                       _lhsIoptions
                       _lhsIoptionsLine
                       _lhsIpragmaBlocks
                       _lhsIrename
                       _lhsIsynmap
                       _lhsItextBlocks ->
                         (let _lhsOallvisits :: ([VisitStateState])
                              _lhsOchildvisit :: (Map.Map VisitIdentifier (Identifier -> PP_Doc))
                              _lhsOdatatype :: ([PP_Doc])
                              _lhsOgenProdIO :: (IO ())
                              _lhsOimports :: ([PP_Doc])
                              _lhsOsem_nt :: PP_Doc
                              _lhsOsem_prod :: PP_Doc
                              _lhsOt_visits :: PP_Doc
                              _lhsOvisitdefs :: (Map.Map VisitIdentifier (Set.Set Identifier))
                              _lhsOvisituses :: (Map.Map VisitIdentifier (Set.Set Identifier))
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 216, column 10)
                              _lhsOallvisits =
                                  ({-# LINE 216 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   error "Every nonterminal should have at least 1 production"
                                   {-# LINE 2264 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 458, column 37)
                              _lhsOchildvisit =
                                  ({-# LINE 458 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   Map.empty
                                   {-# LINE 2270 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 118, column 34)
                              _lhsOdatatype =
                                  ({-# LINE 118 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   []
                                   {-# LINE 2276 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 627, column 49)
                              _lhsOgenProdIO =
                                  ({-# LINE 627 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   return ()
                                   {-# LINE 2282 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 626, column 47)
                              _lhsOimports =
                                  ({-# LINE 626 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   []
                                   {-# LINE 2288 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 168, column 44)
                              _lhsOsem_nt =
                                  ({-# LINE 168 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   empty
                                   {-# LINE 2294 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 310, column 34)
                              _lhsOsem_prod =
                                  ({-# LINE 310 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   empty
                                   {-# LINE 2300 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 253, column 54)
                              _lhsOt_visits =
                                  ({-# LINE 253 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   empty
                                   {-# LINE 2306 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 518, column 36)
                              _lhsOvisitdefs =
                                  ({-# LINE 518 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   Map.empty
                                   {-# LINE 2312 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                              -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 519, column 36)
                              _lhsOvisituses =
                                  ({-# LINE 519 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   Map.empty
                                   {-# LINE 2318 "src-ag/ExecutionPlan2Hs.hs" #-}
                                   )
                          in  ( _lhsOallvisits,_lhsOchildvisit,_lhsOdatatype,_lhsOgenProdIO,_lhsOimports,_lhsOsem_nt,_lhsOsem_prod,_lhsOt_visits,_lhsOvisitdefs,_lhsOvisituses))) )
-- ERule -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         options              : Options
         usedrules            : Set.Set Identifier
      synthesized attributes:
         mrules               : Map.Map Identifier PP_Doc
         ruledefs             : Map.Map Identifier (Set.Set String)
         ruleuses             : Map.Map Identifier (Set.Set String)
         sem_rules            : PP_Doc
   alternatives:
      alternative ERule:
         child name           : {Identifier}
         child pattern        : Pattern 
         child rhs            : Expression 
         child owrt           : {Bool}
         child origin         : {String}
         child explicit       : {Bool}
         visit 0:
            local attrlst     : _
            local addbang     : _
-}
-- cata
sem_ERule :: ERule  ->
             T_ERule 
sem_ERule (ERule _name _pattern _rhs _owrt _origin _explicit )  =
    (sem_ERule_ERule _name (sem_Pattern _pattern ) (sem_Expression _rhs ) _owrt _origin _explicit )
-- semantic domain
newtype T_ERule  = T_ERule (Options ->
                            (Set.Set Identifier) ->
                            ( (Map.Map Identifier PP_Doc),(Map.Map Identifier (Set.Set String)),(Map.Map Identifier (Set.Set String)),PP_Doc))
data Inh_ERule  = Inh_ERule {options_Inh_ERule :: Options,usedrules_Inh_ERule :: (Set.Set Identifier)}
data Syn_ERule  = Syn_ERule {mrules_Syn_ERule :: (Map.Map Identifier PP_Doc),ruledefs_Syn_ERule :: (Map.Map Identifier (Set.Set String)),ruleuses_Syn_ERule :: (Map.Map Identifier (Set.Set String)),sem_rules_Syn_ERule :: PP_Doc}
wrap_ERule :: T_ERule  ->
              Inh_ERule  ->
              Syn_ERule 
wrap_ERule (T_ERule sem ) (Inh_ERule _lhsIoptions _lhsIusedrules )  =
    (let ( _lhsOmrules,_lhsOruledefs,_lhsOruleuses,_lhsOsem_rules) = sem _lhsIoptions _lhsIusedrules 
     in  (Syn_ERule _lhsOmrules _lhsOruledefs _lhsOruleuses _lhsOsem_rules ))
sem_ERule_ERule :: Identifier ->
                   T_Pattern  ->
                   T_Expression  ->
                   Bool ->
                   String ->
                   Bool ->
                   T_ERule 
sem_ERule_ERule name_ (T_Pattern pattern_ ) (T_Expression rhs_ ) owrt_ origin_ explicit_  =
    (T_ERule (\ _lhsIoptions
                _lhsIusedrules ->
                  (let _lhsOsem_rules :: PP_Doc
                       _lhsOmrules :: (Map.Map Identifier PP_Doc)
                       _patternOaddtilde :: (PP_Doc -> PP_Doc)
                       _lhsOruledefs :: (Map.Map Identifier (Set.Set String))
                       _lhsOruleuses :: (Map.Map Identifier (Set.Set String))
                       _patternIattrs :: (Set.Set String)
                       _patternIcopy :: Pattern 
                       _patternIsem_lhs :: ( PP_Doc )
                       _rhsIattrs :: (Set.Set String)
                       _rhsIsemfunc :: PP_Doc
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 410, column 11)
                       _lhsOsem_rules =
                           ({-# LINE 410 "src-ag/ExecutionPlan2Hs.ag" #-}
                            if Set.member name_ _lhsIusedrules
                            then (name_ >#< "=" >#<
                                 (if Set.null _rhsIattrs
                                  then empty
                                  else "\\" >|< _attrlst     >#< "->")
                                 >#< _rhsIsemfunc)
                            else empty
                            {-# LINE 2391 "src-ag/ExecutionPlan2Hs.hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 417, column 11)
                       _attrlst =
                           ({-# LINE 417 "src-ag/ExecutionPlan2Hs.ag" #-}
                            ppSpaced $ Set.toList _rhsIattrs
                            {-# LINE 2397 "src-ag/ExecutionPlan2Hs.hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 418, column 11)
                       _lhsOmrules =
                           ({-# LINE 418 "src-ag/ExecutionPlan2Hs.ag" #-}
                            Map.singleton name_ $ _addbang     _patternIsem_lhs >#< "<-" >#< "return" >#< "$" >#< name_ >#< _attrlst
                            {-# LINE 2403 "src-ag/ExecutionPlan2Hs.hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 419, column 11)
                       _patternOaddtilde =
                           ({-# LINE 419 "src-ag/ExecutionPlan2Hs.ag" #-}
                            \x -> if cases _lhsIoptions then x else "~" >|< x
                            {-# LINE 2409 "src-ag/ExecutionPlan2Hs.hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 420, column 11)
                       _addbang =
                           ({-# LINE 420 "src-ag/ExecutionPlan2Hs.ag" #-}
                            \x -> if bangpats _lhsIoptions then "!" >|< x else x
                            {-# LINE 2415 "src-ag/ExecutionPlan2Hs.hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 507, column 11)
                       _lhsOruledefs =
                           ({-# LINE 507 "src-ag/ExecutionPlan2Hs.ag" #-}
                            Map.singleton name_ _patternIattrs
                            {-# LINE 2421 "src-ag/ExecutionPlan2Hs.hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 508, column 11)
                       _lhsOruleuses =
                           ({-# LINE 508 "src-ag/ExecutionPlan2Hs.ag" #-}
                            Map.singleton name_ _rhsIattrs
                            {-# LINE 2427 "src-ag/ExecutionPlan2Hs.hs" #-}
                            )
                       ( _patternIattrs,_patternIcopy,_patternIsem_lhs) =
                           pattern_ _patternOaddtilde 
                       ( _rhsIattrs,_rhsIsemfunc) =
                           rhs_ 
                   in  ( _lhsOmrules,_lhsOruledefs,_lhsOruleuses,_lhsOsem_rules))) )
-- ERules ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         options              : Options
         usedrules            : Set.Set Identifier
      synthesized attributes:
         mrules               : Map.Map Identifier PP_Doc
         ruledefs             : Map.Map Identifier (Set.Set String)
         ruleuses             : Map.Map Identifier (Set.Set String)
         sem_rules            : PP_Doc
   alternatives:
      alternative Cons:
         child hd             : ERule 
         child tl             : ERules 
      alternative Nil:
-}
-- cata
sem_ERules :: ERules  ->
              T_ERules 
sem_ERules list  =
    (Prelude.foldr sem_ERules_Cons sem_ERules_Nil (Prelude.map sem_ERule list) )
-- semantic domain
newtype T_ERules  = T_ERules (Options ->
                              (Set.Set Identifier) ->
                              ( (Map.Map Identifier PP_Doc),(Map.Map Identifier (Set.Set String)),(Map.Map Identifier (Set.Set String)),PP_Doc))
data Inh_ERules  = Inh_ERules {options_Inh_ERules :: Options,usedrules_Inh_ERules :: (Set.Set Identifier)}
data Syn_ERules  = Syn_ERules {mrules_Syn_ERules :: (Map.Map Identifier PP_Doc),ruledefs_Syn_ERules :: (Map.Map Identifier (Set.Set String)),ruleuses_Syn_ERules :: (Map.Map Identifier (Set.Set String)),sem_rules_Syn_ERules :: PP_Doc}
wrap_ERules :: T_ERules  ->
               Inh_ERules  ->
               Syn_ERules 
wrap_ERules (T_ERules sem ) (Inh_ERules _lhsIoptions _lhsIusedrules )  =
    (let ( _lhsOmrules,_lhsOruledefs,_lhsOruleuses,_lhsOsem_rules) = sem _lhsIoptions _lhsIusedrules 
     in  (Syn_ERules _lhsOmrules _lhsOruledefs _lhsOruleuses _lhsOsem_rules ))
sem_ERules_Cons :: T_ERule  ->
                   T_ERules  ->
                   T_ERules 
sem_ERules_Cons (T_ERule hd_ ) (T_ERules tl_ )  =
    (T_ERules (\ _lhsIoptions
                 _lhsIusedrules ->
                   (let _lhsOmrules :: (Map.Map Identifier PP_Doc)
                        _lhsOruledefs :: (Map.Map Identifier (Set.Set String))
                        _lhsOruleuses :: (Map.Map Identifier (Set.Set String))
                        _lhsOsem_rules :: PP_Doc
                        _hdOoptions :: Options
                        _hdOusedrules :: (Set.Set Identifier)
                        _tlOoptions :: Options
                        _tlOusedrules :: (Set.Set Identifier)
                        _hdImrules :: (Map.Map Identifier PP_Doc)
                        _hdIruledefs :: (Map.Map Identifier (Set.Set String))
                        _hdIruleuses :: (Map.Map Identifier (Set.Set String))
                        _hdIsem_rules :: PP_Doc
                        _tlImrules :: (Map.Map Identifier PP_Doc)
                        _tlIruledefs :: (Map.Map Identifier (Set.Set String))
                        _tlIruleuses :: (Map.Map Identifier (Set.Set String))
                        _tlIsem_rules :: PP_Doc
                        -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 407, column 32)
                        _lhsOmrules =
                            ({-# LINE 407 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _hdImrules `Map.union` _tlImrules
                             {-# LINE 2494 "src-ag/ExecutionPlan2Hs.hs" #-}
                             )
                        -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 500, column 34)
                        _lhsOruledefs =
                            ({-# LINE 500 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _hdIruledefs `uwSetUnion` _tlIruledefs
                             {-# LINE 2500 "src-ag/ExecutionPlan2Hs.hs" #-}
                             )
                        -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 501, column 34)
                        _lhsOruleuses =
                            ({-# LINE 501 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _hdIruleuses `uwSetUnion` _tlIruleuses
                             {-# LINE 2506 "src-ag/ExecutionPlan2Hs.hs" #-}
                             )
                        -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 406, column 35)
                        _lhsOsem_rules =
                            ({-# LINE 406 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _hdIsem_rules >-< _tlIsem_rules
                             {-# LINE 2512 "src-ag/ExecutionPlan2Hs.hs" #-}
                             )
                        -- copy rule (down)
                        _hdOoptions =
                            ({-# LINE 41 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIoptions
                             {-# LINE 2518 "src-ag/ExecutionPlan2Hs.hs" #-}
                             )
                        -- copy rule (down)
                        _hdOusedrules =
                            ({-# LINE 397 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIusedrules
                             {-# LINE 2524 "src-ag/ExecutionPlan2Hs.hs" #-}
                             )
                        -- copy rule (down)
                        _tlOoptions =
                            ({-# LINE 41 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIoptions
                             {-# LINE 2530 "src-ag/ExecutionPlan2Hs.hs" #-}
                             )
                        -- copy rule (down)
                        _tlOusedrules =
                            ({-# LINE 397 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIusedrules
                             {-# LINE 2536 "src-ag/ExecutionPlan2Hs.hs" #-}
                             )
                        ( _hdImrules,_hdIruledefs,_hdIruleuses,_hdIsem_rules) =
                            hd_ _hdOoptions _hdOusedrules 
                        ( _tlImrules,_tlIruledefs,_tlIruleuses,_tlIsem_rules) =
                            tl_ _tlOoptions _tlOusedrules 
                    in  ( _lhsOmrules,_lhsOruledefs,_lhsOruleuses,_lhsOsem_rules))) )
sem_ERules_Nil :: T_ERules 
sem_ERules_Nil  =
    (T_ERules (\ _lhsIoptions
                 _lhsIusedrules ->
                   (let _lhsOmrules :: (Map.Map Identifier PP_Doc)
                        _lhsOruledefs :: (Map.Map Identifier (Set.Set String))
                        _lhsOruleuses :: (Map.Map Identifier (Set.Set String))
                        _lhsOsem_rules :: PP_Doc
                        -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 407, column 32)
                        _lhsOmrules =
                            ({-# LINE 407 "src-ag/ExecutionPlan2Hs.ag" #-}
                             Map.empty
                             {-# LINE 2555 "src-ag/ExecutionPlan2Hs.hs" #-}
                             )
                        -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 500, column 34)
                        _lhsOruledefs =
                            ({-# LINE 500 "src-ag/ExecutionPlan2Hs.ag" #-}
                             Map.empty
                             {-# LINE 2561 "src-ag/ExecutionPlan2Hs.hs" #-}
                             )
                        -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 501, column 34)
                        _lhsOruleuses =
                            ({-# LINE 501 "src-ag/ExecutionPlan2Hs.ag" #-}
                             Map.empty
                             {-# LINE 2567 "src-ag/ExecutionPlan2Hs.hs" #-}
                             )
                        -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 406, column 35)
                        _lhsOsem_rules =
                            ({-# LINE 406 "src-ag/ExecutionPlan2Hs.ag" #-}
                             empty
                             {-# LINE 2573 "src-ag/ExecutionPlan2Hs.hs" #-}
                             )
                    in  ( _lhsOmrules,_lhsOruledefs,_lhsOruleuses,_lhsOsem_rules))) )
-- ExecutionPlan -----------------------------------------------
{-
   visit 0:
      inherited attributes:
         importBlocks         : PP_Doc
         inhmap               : Map.Map NontermIdent Attributes
         mainFile             : String
         mainName             : String
         moduleHeader         : String -> String -> String -> Bool -> String
         options              : Options
         optionsLine          : String
         pragmaBlocks         : String
         synmap               : Map.Map NontermIdent Attributes
         textBlocks           : PP_Doc
      synthesized attributes:
         genIO                : IO ()
         output               : PP_Doc
   alternatives:
      alternative ExecutionPlan:
         child nonts          : ENonterminals 
         child typeSyns       : {TypeSyns}
         child wrappers       : {Set.Set NontermIdent}
         child derivings      : {Derivings}
         visit 0:
            local mainModuleFile : _
            local genMainModule : _
            local commonFile  : _
            local genCommonModule : _
-}
-- cata
sem_ExecutionPlan :: ExecutionPlan  ->
                     T_ExecutionPlan 
sem_ExecutionPlan (ExecutionPlan _nonts _typeSyns _wrappers _derivings )  =
    (sem_ExecutionPlan_ExecutionPlan (sem_ENonterminals _nonts ) _typeSyns _wrappers _derivings )
-- semantic domain
newtype T_ExecutionPlan  = T_ExecutionPlan (PP_Doc ->
                                            (Map.Map NontermIdent Attributes) ->
                                            String ->
                                            String ->
                                            (String -> String -> String -> Bool -> String) ->
                                            Options ->
                                            String ->
                                            String ->
                                            (Map.Map NontermIdent Attributes) ->
                                            PP_Doc ->
                                            ( (IO ()),PP_Doc))
data Inh_ExecutionPlan  = Inh_ExecutionPlan {importBlocks_Inh_ExecutionPlan :: PP_Doc,inhmap_Inh_ExecutionPlan :: (Map.Map NontermIdent Attributes),mainFile_Inh_ExecutionPlan :: String,mainName_Inh_ExecutionPlan :: String,moduleHeader_Inh_ExecutionPlan :: (String -> String -> String -> Bool -> String),options_Inh_ExecutionPlan :: Options,optionsLine_Inh_ExecutionPlan :: String,pragmaBlocks_Inh_ExecutionPlan :: String,synmap_Inh_ExecutionPlan :: (Map.Map NontermIdent Attributes),textBlocks_Inh_ExecutionPlan :: PP_Doc}
data Syn_ExecutionPlan  = Syn_ExecutionPlan {genIO_Syn_ExecutionPlan :: (IO ()),output_Syn_ExecutionPlan :: PP_Doc}
wrap_ExecutionPlan :: T_ExecutionPlan  ->
                      Inh_ExecutionPlan  ->
                      Syn_ExecutionPlan 
wrap_ExecutionPlan (T_ExecutionPlan sem ) (Inh_ExecutionPlan _lhsIimportBlocks _lhsIinhmap _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsIoptions _lhsIoptionsLine _lhsIpragmaBlocks _lhsIsynmap _lhsItextBlocks )  =
    (let ( _lhsOgenIO,_lhsOoutput) = sem _lhsIimportBlocks _lhsIinhmap _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsIoptions _lhsIoptionsLine _lhsIpragmaBlocks _lhsIsynmap _lhsItextBlocks 
     in  (Syn_ExecutionPlan _lhsOgenIO _lhsOoutput ))
sem_ExecutionPlan_ExecutionPlan :: T_ENonterminals  ->
                                   TypeSyns ->
                                   (Set.Set NontermIdent) ->
                                   Derivings ->
                                   T_ExecutionPlan 
sem_ExecutionPlan_ExecutionPlan (T_ENonterminals nonts_ ) typeSyns_ wrappers_ derivings_  =
    (T_ExecutionPlan (\ _lhsIimportBlocks
                        _lhsIinhmap
                        _lhsImainFile
                        _lhsImainName
                        _lhsImoduleHeader
                        _lhsIoptions
                        _lhsIoptionsLine
                        _lhsIpragmaBlocks
                        _lhsIsynmap
                        _lhsItextBlocks ->
                          (let _lhsOoutput :: PP_Doc
                               _nontsOwrappers :: (Set.Set NontermIdent)
                               _nontsOtypeSyns :: TypeSyns
                               _nontsOderivings :: Derivings
                               _nontsOallchildvisit :: (Map.Map VisitIdentifier (Identifier -> PP_Doc))
                               _nontsOavisitdefs :: (Map.Map VisitIdentifier (Set.Set Identifier))
                               _nontsOavisituses :: (Map.Map VisitIdentifier (Set.Set Identifier))
                               _lhsOgenIO :: (IO ())
                               _nontsOimportBlocks :: PP_Doc
                               _nontsOinhmap :: (Map.Map NontermIdent Attributes)
                               _nontsOmainFile :: String
                               _nontsOmainName :: String
                               _nontsOmoduleHeader :: (String -> String -> String -> Bool -> String)
                               _nontsOoptions :: Options
                               _nontsOoptionsLine :: String
                               _nontsOpragmaBlocks :: String
                               _nontsOsynmap :: (Map.Map NontermIdent Attributes)
                               _nontsOtextBlocks :: PP_Doc
                               _nontsIappendCommon :: ([PP_Doc])
                               _nontsIappendMain :: ([PP_Doc])
                               _nontsIchildvisit :: (Map.Map VisitIdentifier (Identifier -> PP_Doc))
                               _nontsIgenProdIO :: (IO ())
                               _nontsIimports :: ([PP_Doc])
                               _nontsIoutput :: PP_Doc
                               _nontsIvisitdefs :: (Map.Map VisitIdentifier (Set.Set Identifier))
                               _nontsIvisituses :: (Map.Map VisitIdentifier (Set.Set Identifier))
                               -- "src-ag/ExecutionPlan2Hs.ag"(line 53, column 19)
                               _lhsOoutput =
                                   ({-# LINE 53 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _nontsIoutput
                                    {-# LINE 2676 "src-ag/ExecutionPlan2Hs.hs" #-}
                                    )
                               -- "src-ag/ExecutionPlan2Hs.ag"(line 59, column 19)
                               _nontsOwrappers =
                                   ({-# LINE 59 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    wrappers_
                                    {-# LINE 2682 "src-ag/ExecutionPlan2Hs.hs" #-}
                                    )
                               -- "src-ag/ExecutionPlan2Hs.ag"(line 97, column 19)
                               _nontsOtypeSyns =
                                   ({-# LINE 97 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    typeSyns_
                                    {-# LINE 2688 "src-ag/ExecutionPlan2Hs.hs" #-}
                                    )
                               -- "src-ag/ExecutionPlan2Hs.ag"(line 98, column 19)
                               _nontsOderivings =
                                   ({-# LINE 98 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    derivings_
                                    {-# LINE 2694 "src-ag/ExecutionPlan2Hs.hs" #-}
                                    )
                               -- "src-ag/ExecutionPlan2Hs.ag"(line 463, column 19)
                               _nontsOallchildvisit =
                                   ({-# LINE 463 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _nontsIchildvisit
                                    {-# LINE 2700 "src-ag/ExecutionPlan2Hs.hs" #-}
                                    )
                               -- "src-ag/ExecutionPlan2Hs.ag"(line 532, column 19)
                               _nontsOavisitdefs =
                                   ({-# LINE 532 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _nontsIvisitdefs
                                    {-# LINE 2706 "src-ag/ExecutionPlan2Hs.hs" #-}
                                    )
                               -- "src-ag/ExecutionPlan2Hs.ag"(line 533, column 19)
                               _nontsOavisituses =
                                   ({-# LINE 533 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _nontsIvisituses
                                    {-# LINE 2712 "src-ag/ExecutionPlan2Hs.hs" #-}
                                    )
                               -- "src-ag/ExecutionPlan2Hs.ag"(line 581, column 19)
                               _lhsOgenIO =
                                   ({-# LINE 581 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    do _genMainModule
                                       _genCommonModule
                                       _nontsIgenProdIO
                                    {-# LINE 2720 "src-ag/ExecutionPlan2Hs.hs" #-}
                                    )
                               -- "src-ag/ExecutionPlan2Hs.ag"(line 584, column 19)
                               _mainModuleFile =
                                   ({-# LINE 584 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsImainFile ++ ".hs"
                                    {-# LINE 2726 "src-ag/ExecutionPlan2Hs.hs" #-}
                                    )
                               -- "src-ag/ExecutionPlan2Hs.ag"(line 585, column 19)
                               _genMainModule =
                                   ({-# LINE 585 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    writeModule _mainModuleFile
                                      ( [ pp $ "{-# LANGUAGE Rank2Types, GADTs, EmptyDataDecls #-}"
                                        , pp $ _lhsIpragmaBlocks
                                        , pp $ _lhsIoptionsLine
                                        , pp $ _lhsImoduleHeader _lhsImainName "" "" False
                                        , pp $ "import Control.Monad.Identity"
                                        , pp $ "import " ++ _lhsImainName ++ "_common"
                                        ]
                                        ++ _nontsIimports
                                        ++ _nontsIappendMain
                                      )
                                    {-# LINE 2742 "src-ag/ExecutionPlan2Hs.hs" #-}
                                    )
                               -- "src-ag/ExecutionPlan2Hs.ag"(line 596, column 19)
                               _commonFile =
                                   ({-# LINE 596 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsImainFile ++ "_common.hs"
                                    {-# LINE 2748 "src-ag/ExecutionPlan2Hs.hs" #-}
                                    )
                               -- "src-ag/ExecutionPlan2Hs.ag"(line 597, column 19)
                               _genCommonModule =
                                   ({-# LINE 597 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    writeModule _commonFile
                                      ( [ pp $ "{-# LANGUAGE Rank2Types, GADTs, EmptyDataDecls #-}"
                                        , pp $ _lhsIpragmaBlocks
                                        , pp $ _lhsIoptionsLine
                                        , pp $ _lhsImoduleHeader _lhsImainName "_common" "" True
                                        , pp $ "import Control.Monad.Identity"
                                        , _lhsIimportBlocks
                                        , _lhsItextBlocks
                                        ]
                                        ++ _nontsIappendCommon
                                      )
                                    {-# LINE 2764 "src-ag/ExecutionPlan2Hs.hs" #-}
                                    )
                               -- copy rule (down)
                               _nontsOimportBlocks =
                                   ({-# LINE 25 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsIimportBlocks
                                    {-# LINE 2770 "src-ag/ExecutionPlan2Hs.hs" #-}
                                    )
                               -- copy rule (down)
                               _nontsOinhmap =
                                   ({-# LINE 188 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsIinhmap
                                    {-# LINE 2776 "src-ag/ExecutionPlan2Hs.hs" #-}
                                    )
                               -- copy rule (down)
                               _nontsOmainFile =
                                   ({-# LINE 29 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsImainFile
                                    {-# LINE 2782 "src-ag/ExecutionPlan2Hs.hs" #-}
                                    )
                               -- copy rule (down)
                               _nontsOmainName =
                                   ({-# LINE 31 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsImainName
                                    {-# LINE 2788 "src-ag/ExecutionPlan2Hs.hs" #-}
                                    )
                               -- copy rule (down)
                               _nontsOmoduleHeader =
                                   ({-# LINE 28 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsImoduleHeader
                                    {-# LINE 2794 "src-ag/ExecutionPlan2Hs.hs" #-}
                                    )
                               -- copy rule (down)
                               _nontsOoptions =
                                   ({-# LINE 41 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsIoptions
                                    {-# LINE 2800 "src-ag/ExecutionPlan2Hs.hs" #-}
                                    )
                               -- copy rule (down)
                               _nontsOoptionsLine =
                                   ({-# LINE 30 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsIoptionsLine
                                    {-# LINE 2806 "src-ag/ExecutionPlan2Hs.hs" #-}
                                    )
                               -- copy rule (down)
                               _nontsOpragmaBlocks =
                                   ({-# LINE 26 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsIpragmaBlocks
                                    {-# LINE 2812 "src-ag/ExecutionPlan2Hs.hs" #-}
                                    )
                               -- copy rule (down)
                               _nontsOsynmap =
                                   ({-# LINE 189 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsIsynmap
                                    {-# LINE 2818 "src-ag/ExecutionPlan2Hs.hs" #-}
                                    )
                               -- copy rule (down)
                               _nontsOtextBlocks =
                                   ({-# LINE 27 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsItextBlocks
                                    {-# LINE 2824 "src-ag/ExecutionPlan2Hs.hs" #-}
                                    )
                               ( _nontsIappendCommon,_nontsIappendMain,_nontsIchildvisit,_nontsIgenProdIO,_nontsIimports,_nontsIoutput,_nontsIvisitdefs,_nontsIvisituses) =
                                   nonts_ _nontsOallchildvisit _nontsOavisitdefs _nontsOavisituses _nontsOderivings _nontsOimportBlocks _nontsOinhmap _nontsOmainFile _nontsOmainName _nontsOmoduleHeader _nontsOoptions _nontsOoptionsLine _nontsOpragmaBlocks _nontsOsynmap _nontsOtextBlocks _nontsOtypeSyns _nontsOwrappers 
                           in  ( _lhsOgenIO,_lhsOoutput))) )
-- Expression --------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         attrs                : Set.Set String
         semfunc              : PP_Doc
   alternatives:
      alternative Expression:
         child pos            : {Pos}
         child tks            : {[HsToken]}
-}
-- cata
sem_Expression :: Expression  ->
                  T_Expression 
sem_Expression (Expression _pos _tks )  =
    (sem_Expression_Expression _pos _tks )
-- semantic domain
newtype T_Expression  = T_Expression (( (Set.Set String),PP_Doc))
data Inh_Expression  = Inh_Expression {}
data Syn_Expression  = Syn_Expression {attrs_Syn_Expression :: (Set.Set String),semfunc_Syn_Expression :: PP_Doc}
wrap_Expression :: T_Expression  ->
                   Inh_Expression  ->
                   Syn_Expression 
wrap_Expression (T_Expression sem ) (Inh_Expression )  =
    (let ( _lhsOattrs,_lhsOsemfunc) = sem 
     in  (Syn_Expression _lhsOattrs _lhsOsemfunc ))
sem_Expression_Expression :: Pos ->
                             ([HsToken]) ->
                             T_Expression 
sem_Expression_Expression pos_ tks_  =
    (T_Expression (let _lhsOattrs :: (Set.Set String)
                       _lhsOsemfunc :: PP_Doc
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 448, column 16)
                       _lhsOattrs =
                           ({-# LINE 448 "src-ag/ExecutionPlan2Hs.ag" #-}
                            Set.unions $ map (\tok -> attrs_Syn_HsToken (wrap_HsToken (sem_HsToken tok) Inh_HsToken)) tks_
                            {-# LINE 2865 "src-ag/ExecutionPlan2Hs.hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 449, column 16)
                       _lhsOsemfunc =
                           ({-# LINE 449 "src-ag/ExecutionPlan2Hs.ag" #-}
                            vlist $ showTokens $ map (\tok -> tok_Syn_HsToken (wrap_HsToken (sem_HsToken tok) Inh_HsToken)) tks_
                            {-# LINE 2871 "src-ag/ExecutionPlan2Hs.hs" #-}
                            )
                   in  ( _lhsOattrs,_lhsOsemfunc)) )
-- HsToken -----------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         attrs                : Set.Set String
         tok                  : (Pos,String)
   alternatives:
      alternative AGField:
         child field          : {Identifier}
         child attr           : {Identifier}
         child pos            : {Pos}
         child rdesc          : {Maybe String}
         visit 0:
            local addTrace    : _
      alternative AGLocal:
         child var            : {Identifier}
         child pos            : {Pos}
         child rdesc          : {Maybe String}
         visit 0:
            local tok         : _
      alternative CharToken:
         child value          : {String}
         child pos            : {Pos}
      alternative Err:
         child mesg           : {String}
         child pos            : {Pos}
      alternative HsToken:
         child value          : {String}
         child pos            : {Pos}
      alternative StrToken:
         child value          : {String}
         child pos            : {Pos}
-}
-- cata
sem_HsToken :: HsToken  ->
               T_HsToken 
sem_HsToken (AGField _field _attr _pos _rdesc )  =
    (sem_HsToken_AGField _field _attr _pos _rdesc )
sem_HsToken (AGLocal _var _pos _rdesc )  =
    (sem_HsToken_AGLocal _var _pos _rdesc )
sem_HsToken (CharToken _value _pos )  =
    (sem_HsToken_CharToken _value _pos )
sem_HsToken (Err _mesg _pos )  =
    (sem_HsToken_Err _mesg _pos )
sem_HsToken (HsToken _value _pos )  =
    (sem_HsToken_HsToken _value _pos )
sem_HsToken (StrToken _value _pos )  =
    (sem_HsToken_StrToken _value _pos )
-- semantic domain
newtype T_HsToken  = T_HsToken (( (Set.Set String),((Pos,String))))
data Inh_HsToken  = Inh_HsToken {}
data Syn_HsToken  = Syn_HsToken {attrs_Syn_HsToken :: (Set.Set String),tok_Syn_HsToken :: ((Pos,String))}
wrap_HsToken :: T_HsToken  ->
                Inh_HsToken  ->
                Syn_HsToken 
wrap_HsToken (T_HsToken sem ) (Inh_HsToken )  =
    (let ( _lhsOattrs,_lhsOtok) = sem 
     in  (Syn_HsToken _lhsOattrs _lhsOtok ))
sem_HsToken_AGField :: Identifier ->
                       Identifier ->
                       Pos ->
                       (Maybe String) ->
                       T_HsToken 
sem_HsToken_AGField field_ attr_ pos_ rdesc_  =
    (T_HsToken (let _lhsOattrs :: (Set.Set String)
                    _lhsOtok :: ((Pos,String))
                    -- "src-ag/ExecutionPlan2Hs.ag"(line 443, column 15)
                    _lhsOattrs =
                        ({-# LINE 443 "src-ag/ExecutionPlan2Hs.ag" #-}
                         Set.singleton $ attrname True field_ attr_
                         {-# LINE 2944 "src-ag/ExecutionPlan2Hs.hs" #-}
                         )
                    -- "src-ag/ExecutionPlan2Hs.ag"(line 559, column 8)
                    _addTrace =
                        ({-# LINE 559 "src-ag/ExecutionPlan2Hs.ag" #-}
                         case rdesc_ of
                           Just d  -> \x -> "(trace " ++ show (d ++ " -> " ++ show field_ ++ "." ++ show attr_) ++ " (" ++ x ++ "))"
                           Nothing -> id
                         {-# LINE 2952 "src-ag/ExecutionPlan2Hs.hs" #-}
                         )
                    -- "src-ag/ExecutionPlan2Hs.ag"(line 562, column 8)
                    _lhsOtok =
                        ({-# LINE 562 "src-ag/ExecutionPlan2Hs.ag" #-}
                         (pos_, _addTrace     $ attrname True field_ attr_)
                         {-# LINE 2958 "src-ag/ExecutionPlan2Hs.hs" #-}
                         )
                in  ( _lhsOattrs,_lhsOtok)) )
sem_HsToken_AGLocal :: Identifier ->
                       Pos ->
                       (Maybe String) ->
                       T_HsToken 
sem_HsToken_AGLocal var_ pos_ rdesc_  =
    (T_HsToken (let _lhsOattrs :: (Set.Set String)
                    _lhsOtok :: ((Pos,String))
                    -- "src-ag/ExecutionPlan2Hs.ag"(line 442, column 15)
                    _lhsOattrs =
                        ({-# LINE 442 "src-ag/ExecutionPlan2Hs.ag" #-}
                         Set.singleton $ locname var_
                         {-# LINE 2972 "src-ag/ExecutionPlan2Hs.hs" #-}
                         )
                    -- "src-ag/ExecutionPlan2Hs.ag"(line 555, column 15)
                    _tok =
                        ({-# LINE 555 "src-ag/ExecutionPlan2Hs.ag" #-}
                         (pos_,locname var_)
                         {-# LINE 2978 "src-ag/ExecutionPlan2Hs.hs" #-}
                         )
                    -- copy rule (from local)
                    _lhsOtok =
                        ({-# LINE 557 "src-ag/ExecutionPlan2Hs.ag" #-}
                         _tok
                         {-# LINE 2984 "src-ag/ExecutionPlan2Hs.hs" #-}
                         )
                in  ( _lhsOattrs,_lhsOtok)) )
sem_HsToken_CharToken :: String ->
                         Pos ->
                         T_HsToken 
sem_HsToken_CharToken value_ pos_  =
    (T_HsToken (let _lhsOtok :: ((Pos,String))
                    _lhsOattrs :: (Set.Set String)
                    -- "src-ag/ExecutionPlan2Hs.ag"(line 566, column 16)
                    _lhsOtok =
                        ({-# LINE 566 "src-ag/ExecutionPlan2Hs.ag" #-}
                         (pos_, if null value_
                                   then ""
                                   else showCharShort (head value_)
                         )
                         {-# LINE 3000 "src-ag/ExecutionPlan2Hs.hs" #-}
                         )
                    -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 440, column 37)
                    _lhsOattrs =
                        ({-# LINE 440 "src-ag/ExecutionPlan2Hs.ag" #-}
                         Set.empty
                         {-# LINE 3006 "src-ag/ExecutionPlan2Hs.hs" #-}
                         )
                in  ( _lhsOattrs,_lhsOtok)) )
sem_HsToken_Err :: String ->
                   Pos ->
                   T_HsToken 
sem_HsToken_Err mesg_ pos_  =
    (T_HsToken (let _lhsOtok :: ((Pos,String))
                    _lhsOattrs :: (Set.Set String)
                    -- "src-ag/ExecutionPlan2Hs.ag"(line 572, column 16)
                    _lhsOtok =
                        ({-# LINE 572 "src-ag/ExecutionPlan2Hs.ag" #-}
                         (pos_, "")
                         {-# LINE 3019 "src-ag/ExecutionPlan2Hs.hs" #-}
                         )
                    -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 440, column 37)
                    _lhsOattrs =
                        ({-# LINE 440 "src-ag/ExecutionPlan2Hs.ag" #-}
                         Set.empty
                         {-# LINE 3025 "src-ag/ExecutionPlan2Hs.hs" #-}
                         )
                in  ( _lhsOattrs,_lhsOtok)) )
sem_HsToken_HsToken :: String ->
                       Pos ->
                       T_HsToken 
sem_HsToken_HsToken value_ pos_  =
    (T_HsToken (let _lhsOtok :: ((Pos,String))
                    _lhsOattrs :: (Set.Set String)
                    -- "src-ag/ExecutionPlan2Hs.ag"(line 564, column 14)
                    _lhsOtok =
                        ({-# LINE 564 "src-ag/ExecutionPlan2Hs.ag" #-}
                         (pos_, value_)
                         {-# LINE 3038 "src-ag/ExecutionPlan2Hs.hs" #-}
                         )
                    -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 440, column 37)
                    _lhsOattrs =
                        ({-# LINE 440 "src-ag/ExecutionPlan2Hs.ag" #-}
                         Set.empty
                         {-# LINE 3044 "src-ag/ExecutionPlan2Hs.hs" #-}
                         )
                in  ( _lhsOattrs,_lhsOtok)) )
sem_HsToken_StrToken :: String ->
                        Pos ->
                        T_HsToken 
sem_HsToken_StrToken value_ pos_  =
    (T_HsToken (let _lhsOtok :: ((Pos,String))
                    _lhsOattrs :: (Set.Set String)
                    -- "src-ag/ExecutionPlan2Hs.ag"(line 571, column 16)
                    _lhsOtok =
                        ({-# LINE 571 "src-ag/ExecutionPlan2Hs.ag" #-}
                         (pos_, showStrShort value_)
                         {-# LINE 3057 "src-ag/ExecutionPlan2Hs.hs" #-}
                         )
                    -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 440, column 37)
                    _lhsOattrs =
                        ({-# LINE 440 "src-ag/ExecutionPlan2Hs.ag" #-}
                         Set.empty
                         {-# LINE 3063 "src-ag/ExecutionPlan2Hs.hs" #-}
                         )
                in  ( _lhsOattrs,_lhsOtok)) )
-- HsTokens ----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         tks                  : [(Pos,String)]
   alternatives:
      alternative Cons:
         child hd             : HsToken 
         child tl             : HsTokens 
      alternative Nil:
-}
-- cata
sem_HsTokens :: HsTokens  ->
                T_HsTokens 
sem_HsTokens list  =
    (Prelude.foldr sem_HsTokens_Cons sem_HsTokens_Nil (Prelude.map sem_HsToken list) )
-- semantic domain
newtype T_HsTokens  = T_HsTokens (( ([(Pos,String)])))
data Inh_HsTokens  = Inh_HsTokens {}
data Syn_HsTokens  = Syn_HsTokens {tks_Syn_HsTokens :: ([(Pos,String)])}
wrap_HsTokens :: T_HsTokens  ->
                 Inh_HsTokens  ->
                 Syn_HsTokens 
wrap_HsTokens (T_HsTokens sem ) (Inh_HsTokens )  =
    (let ( _lhsOtks) = sem 
     in  (Syn_HsTokens _lhsOtks ))
sem_HsTokens_Cons :: T_HsToken  ->
                     T_HsTokens  ->
                     T_HsTokens 
sem_HsTokens_Cons (T_HsToken hd_ ) (T_HsTokens tl_ )  =
    (T_HsTokens (let _lhsOtks :: ([(Pos,String)])
                     _hdIattrs :: (Set.Set String)
                     _hdItok :: ((Pos,String))
                     _tlItks :: ([(Pos,String)])
                     -- "src-ag/ExecutionPlan2Hs.ag"(line 551, column 10)
                     _lhsOtks =
                         ({-# LINE 551 "src-ag/ExecutionPlan2Hs.ag" #-}
                          _hdItok : _tlItks
                          {-# LINE 3104 "src-ag/ExecutionPlan2Hs.hs" #-}
                          )
                     ( _hdIattrs,_hdItok) =
                         hd_ 
                     ( _tlItks) =
                         tl_ 
                 in  ( _lhsOtks)) )
sem_HsTokens_Nil :: T_HsTokens 
sem_HsTokens_Nil  =
    (T_HsTokens (let _lhsOtks :: ([(Pos,String)])
                     -- "src-ag/ExecutionPlan2Hs.ag"(line 552, column 10)
                     _lhsOtks =
                         ({-# LINE 552 "src-ag/ExecutionPlan2Hs.ag" #-}
                          []
                          {-# LINE 3118 "src-ag/ExecutionPlan2Hs.hs" #-}
                          )
                 in  ( _lhsOtks)) )
-- HsTokensRoot ------------------------------------------------
{-
   alternatives:
      alternative HsTokensRoot:
         child tokens         : HsTokens 
-}
-- cata
sem_HsTokensRoot :: HsTokensRoot  ->
                    T_HsTokensRoot 
sem_HsTokensRoot (HsTokensRoot _tokens )  =
    (sem_HsTokensRoot_HsTokensRoot (sem_HsTokens _tokens ) )
-- semantic domain
newtype T_HsTokensRoot  = T_HsTokensRoot (( ))
data Inh_HsTokensRoot  = Inh_HsTokensRoot {}
data Syn_HsTokensRoot  = Syn_HsTokensRoot {}
wrap_HsTokensRoot :: T_HsTokensRoot  ->
                     Inh_HsTokensRoot  ->
                     Syn_HsTokensRoot 
wrap_HsTokensRoot (T_HsTokensRoot sem ) (Inh_HsTokensRoot )  =
    (let ( ) = sem 
     in  (Syn_HsTokensRoot ))
sem_HsTokensRoot_HsTokensRoot :: T_HsTokens  ->
                                 T_HsTokensRoot 
sem_HsTokensRoot_HsTokensRoot (T_HsTokens tokens_ )  =
    (T_HsTokensRoot (let _tokensItks :: ([(Pos,String)])
                         ( _tokensItks) =
                             tokens_ 
                     in  ( )) )
-- Pattern -----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         addtilde             : PP_Doc -> PP_Doc
      synthesized attributes:
         attrs                : Set.Set String
         copy                 : SELF 
         sem_lhs              :  PP_Doc 
   alternatives:
      alternative Alias:
         child field          : {Identifier}
         child attr           : {Identifier}
         child pat            : Pattern 
         child parts          : Patterns 
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
sem_Pattern :: Pattern  ->
               T_Pattern 
sem_Pattern (Alias _field _attr _pat _parts )  =
    (sem_Pattern_Alias _field _attr (sem_Pattern _pat ) (sem_Patterns _parts ) )
sem_Pattern (Constr _name _pats )  =
    (sem_Pattern_Constr _name (sem_Patterns _pats ) )
sem_Pattern (Irrefutable _pat )  =
    (sem_Pattern_Irrefutable (sem_Pattern _pat ) )
sem_Pattern (Product _pos _pats )  =
    (sem_Pattern_Product _pos (sem_Patterns _pats ) )
sem_Pattern (Underscore _pos )  =
    (sem_Pattern_Underscore _pos )
-- semantic domain
newtype T_Pattern  = T_Pattern ((PP_Doc -> PP_Doc) ->
                                ( (Set.Set String),Pattern ,( PP_Doc )))
data Inh_Pattern  = Inh_Pattern {addtilde_Inh_Pattern :: (PP_Doc -> PP_Doc)}
data Syn_Pattern  = Syn_Pattern {attrs_Syn_Pattern :: (Set.Set String),copy_Syn_Pattern :: Pattern ,sem_lhs_Syn_Pattern :: ( PP_Doc )}
wrap_Pattern :: T_Pattern  ->
                Inh_Pattern  ->
                Syn_Pattern 
wrap_Pattern (T_Pattern sem ) (Inh_Pattern _lhsIaddtilde )  =
    (let ( _lhsOattrs,_lhsOcopy,_lhsOsem_lhs) = sem _lhsIaddtilde 
     in  (Syn_Pattern _lhsOattrs _lhsOcopy _lhsOsem_lhs ))
sem_Pattern_Alias :: Identifier ->
                     Identifier ->
                     T_Pattern  ->
                     T_Patterns  ->
                     T_Pattern 
sem_Pattern_Alias field_ attr_ (T_Pattern pat_ ) (T_Patterns parts_ )  =
    (T_Pattern (\ _lhsIaddtilde ->
                    (let _lhsOsem_lhs :: ( PP_Doc )
                         _lhsOattrs :: (Set.Set String)
                         _lhsOcopy :: Pattern 
                         _patOaddtilde :: (PP_Doc -> PP_Doc)
                         _partsOaddtilde :: (PP_Doc -> PP_Doc)
                         _patIattrs :: (Set.Set String)
                         _patIcopy :: Pattern 
                         _patIsem_lhs :: ( PP_Doc )
                         _partsIattrs :: (Set.Set String)
                         _partsIcopy :: Patterns 
                         _partsIsem_lhs :: ([PP_Doc])
                         -- "src-ag/ExecutionPlan2Hs.ag"(line 430, column 17)
                         _lhsOsem_lhs =
                             ({-# LINE 430 "src-ag/ExecutionPlan2Hs.ag" #-}
                              text $ attrname False field_ attr_
                              {-# LINE 3231 "src-ag/ExecutionPlan2Hs.hs" #-}
                              )
                         -- "src-ag/ExecutionPlan2Hs.ag"(line 431, column 17)
                         _lhsOattrs =
                             ({-# LINE 431 "src-ag/ExecutionPlan2Hs.ag" #-}
                              Set.singleton $ attrname False field_ attr_
                              {-# LINE 3237 "src-ag/ExecutionPlan2Hs.hs" #-}
                              )
                         -- self rule
                         _copy =
                             ({-# LINE 23 "src-ag/Patterns.ag" #-}
                              Alias field_ attr_ _patIcopy _partsIcopy
                              {-# LINE 3243 "src-ag/ExecutionPlan2Hs.hs" #-}
                              )
                         -- self rule
                         _lhsOcopy =
                             ({-# LINE 23 "src-ag/Patterns.ag" #-}
                              _copy
                              {-# LINE 3249 "src-ag/ExecutionPlan2Hs.hs" #-}
                              )
                         -- copy rule (down)
                         _patOaddtilde =
                             ({-# LINE 425 "src-ag/ExecutionPlan2Hs.ag" #-}
                              _lhsIaddtilde
                              {-# LINE 3255 "src-ag/ExecutionPlan2Hs.hs" #-}
                              )
                         -- copy rule (down)
                         _partsOaddtilde =
                             ({-# LINE 425 "src-ag/ExecutionPlan2Hs.ag" #-}
                              _lhsIaddtilde
                              {-# LINE 3261 "src-ag/ExecutionPlan2Hs.hs" #-}
                              )
                         ( _patIattrs,_patIcopy,_patIsem_lhs) =
                             pat_ _patOaddtilde 
                         ( _partsIattrs,_partsIcopy,_partsIsem_lhs) =
                             parts_ _partsOaddtilde 
                     in  ( _lhsOattrs,_lhsOcopy,_lhsOsem_lhs))) )
sem_Pattern_Constr :: ConstructorIdent ->
                      T_Patterns  ->
                      T_Pattern 
sem_Pattern_Constr name_ (T_Patterns pats_ )  =
    (T_Pattern (\ _lhsIaddtilde ->
                    (let _lhsOsem_lhs :: ( PP_Doc )
                         _patsOaddtilde :: (PP_Doc -> PP_Doc)
                         _lhsOattrs :: (Set.Set String)
                         _lhsOcopy :: Pattern 
                         _patsIattrs :: (Set.Set String)
                         _patsIcopy :: Patterns 
                         _patsIsem_lhs :: ([PP_Doc])
                         -- "src-ag/ExecutionPlan2Hs.ag"(line 434, column 17)
                         _lhsOsem_lhs =
                             ({-# LINE 434 "src-ag/ExecutionPlan2Hs.ag" #-}
                              _lhsIaddtilde $ pp_parens $ name_ >#< hv_sp _patsIsem_lhs
                              {-# LINE 3284 "src-ag/ExecutionPlan2Hs.hs" #-}
                              )
                         -- "src-ag/ExecutionPlan2Hs.ag"(line 435, column 17)
                         _patsOaddtilde =
                             ({-# LINE 435 "src-ag/ExecutionPlan2Hs.ag" #-}
                              id
                              {-# LINE 3290 "src-ag/ExecutionPlan2Hs.hs" #-}
                              )
                         -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 427, column 36)
                         _lhsOattrs =
                             ({-# LINE 427 "src-ag/ExecutionPlan2Hs.ag" #-}
                              _patsIattrs
                              {-# LINE 3296 "src-ag/ExecutionPlan2Hs.hs" #-}
                              )
                         -- self rule
                         _copy =
                             ({-# LINE 23 "src-ag/Patterns.ag" #-}
                              Constr name_ _patsIcopy
                              {-# LINE 3302 "src-ag/ExecutionPlan2Hs.hs" #-}
                              )
                         -- self rule
                         _lhsOcopy =
                             ({-# LINE 23 "src-ag/Patterns.ag" #-}
                              _copy
                              {-# LINE 3308 "src-ag/ExecutionPlan2Hs.hs" #-}
                              )
                         ( _patsIattrs,_patsIcopy,_patsIsem_lhs) =
                             pats_ _patsOaddtilde 
                     in  ( _lhsOattrs,_lhsOcopy,_lhsOsem_lhs))) )
sem_Pattern_Irrefutable :: T_Pattern  ->
                           T_Pattern 
sem_Pattern_Irrefutable (T_Pattern pat_ )  =
    (T_Pattern (\ _lhsIaddtilde ->
                    (let _lhsOsem_lhs :: ( PP_Doc )
                         _patOaddtilde :: (PP_Doc -> PP_Doc)
                         _lhsOattrs :: (Set.Set String)
                         _lhsOcopy :: Pattern 
                         _patIattrs :: (Set.Set String)
                         _patIcopy :: Pattern 
                         _patIsem_lhs :: ( PP_Doc )
                         -- "src-ag/ExecutionPlan2Hs.ag"(line 437, column 17)
                         _lhsOsem_lhs =
                             ({-# LINE 437 "src-ag/ExecutionPlan2Hs.ag" #-}
                              text "~" >|< pp_parens _patIsem_lhs
                              {-# LINE 3328 "src-ag/ExecutionPlan2Hs.hs" #-}
                              )
                         -- "src-ag/ExecutionPlan2Hs.ag"(line 438, column 17)
                         _patOaddtilde =
                             ({-# LINE 438 "src-ag/ExecutionPlan2Hs.ag" #-}
                              id
                              {-# LINE 3334 "src-ag/ExecutionPlan2Hs.hs" #-}
                              )
                         -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 427, column 36)
                         _lhsOattrs =
                             ({-# LINE 427 "src-ag/ExecutionPlan2Hs.ag" #-}
                              _patIattrs
                              {-# LINE 3340 "src-ag/ExecutionPlan2Hs.hs" #-}
                              )
                         -- self rule
                         _copy =
                             ({-# LINE 23 "src-ag/Patterns.ag" #-}
                              Irrefutable _patIcopy
                              {-# LINE 3346 "src-ag/ExecutionPlan2Hs.hs" #-}
                              )
                         -- self rule
                         _lhsOcopy =
                             ({-# LINE 23 "src-ag/Patterns.ag" #-}
                              _copy
                              {-# LINE 3352 "src-ag/ExecutionPlan2Hs.hs" #-}
                              )
                         ( _patIattrs,_patIcopy,_patIsem_lhs) =
                             pat_ _patOaddtilde 
                     in  ( _lhsOattrs,_lhsOcopy,_lhsOsem_lhs))) )
sem_Pattern_Product :: Pos ->
                       T_Patterns  ->
                       T_Pattern 
sem_Pattern_Product pos_ (T_Patterns pats_ )  =
    (T_Pattern (\ _lhsIaddtilde ->
                    (let _lhsOsem_lhs :: ( PP_Doc )
                         _patsOaddtilde :: (PP_Doc -> PP_Doc)
                         _lhsOattrs :: (Set.Set String)
                         _lhsOcopy :: Pattern 
                         _patsIattrs :: (Set.Set String)
                         _patsIcopy :: Patterns 
                         _patsIsem_lhs :: ([PP_Doc])
                         -- "src-ag/ExecutionPlan2Hs.ag"(line 432, column 17)
                         _lhsOsem_lhs =
                             ({-# LINE 432 "src-ag/ExecutionPlan2Hs.ag" #-}
                              _lhsIaddtilde $ pp_block "(" ")" "," _patsIsem_lhs
                              {-# LINE 3373 "src-ag/ExecutionPlan2Hs.hs" #-}
                              )
                         -- "src-ag/ExecutionPlan2Hs.ag"(line 433, column 17)
                         _patsOaddtilde =
                             ({-# LINE 433 "src-ag/ExecutionPlan2Hs.ag" #-}
                              id
                              {-# LINE 3379 "src-ag/ExecutionPlan2Hs.hs" #-}
                              )
                         -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 427, column 36)
                         _lhsOattrs =
                             ({-# LINE 427 "src-ag/ExecutionPlan2Hs.ag" #-}
                              _patsIattrs
                              {-# LINE 3385 "src-ag/ExecutionPlan2Hs.hs" #-}
                              )
                         -- self rule
                         _copy =
                             ({-# LINE 23 "src-ag/Patterns.ag" #-}
                              Product pos_ _patsIcopy
                              {-# LINE 3391 "src-ag/ExecutionPlan2Hs.hs" #-}
                              )
                         -- self rule
                         _lhsOcopy =
                             ({-# LINE 23 "src-ag/Patterns.ag" #-}
                              _copy
                              {-# LINE 3397 "src-ag/ExecutionPlan2Hs.hs" #-}
                              )
                         ( _patsIattrs,_patsIcopy,_patsIsem_lhs) =
                             pats_ _patsOaddtilde 
                     in  ( _lhsOattrs,_lhsOcopy,_lhsOsem_lhs))) )
sem_Pattern_Underscore :: Pos ->
                          T_Pattern 
sem_Pattern_Underscore pos_  =
    (T_Pattern (\ _lhsIaddtilde ->
                    (let _lhsOsem_lhs :: ( PP_Doc )
                         _lhsOattrs :: (Set.Set String)
                         _lhsOcopy :: Pattern 
                         -- "src-ag/ExecutionPlan2Hs.ag"(line 436, column 17)
                         _lhsOsem_lhs =
                             ({-# LINE 436 "src-ag/ExecutionPlan2Hs.ag" #-}
                              text "_"
                              {-# LINE 3413 "src-ag/ExecutionPlan2Hs.hs" #-}
                              )
                         -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 427, column 36)
                         _lhsOattrs =
                             ({-# LINE 427 "src-ag/ExecutionPlan2Hs.ag" #-}
                              Set.empty
                              {-# LINE 3419 "src-ag/ExecutionPlan2Hs.hs" #-}
                              )
                         -- self rule
                         _copy =
                             ({-# LINE 23 "src-ag/Patterns.ag" #-}
                              Underscore pos_
                              {-# LINE 3425 "src-ag/ExecutionPlan2Hs.hs" #-}
                              )
                         -- self rule
                         _lhsOcopy =
                             ({-# LINE 23 "src-ag/Patterns.ag" #-}
                              _copy
                              {-# LINE 3431 "src-ag/ExecutionPlan2Hs.hs" #-}
                              )
                     in  ( _lhsOattrs,_lhsOcopy,_lhsOsem_lhs))) )
-- Patterns ----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         addtilde             : PP_Doc -> PP_Doc
      synthesized attributes:
         attrs                : Set.Set String
         copy                 : SELF 
         sem_lhs              : [PP_Doc]
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
sem_Patterns :: Patterns  ->
                T_Patterns 
sem_Patterns list  =
    (Prelude.foldr sem_Patterns_Cons sem_Patterns_Nil (Prelude.map sem_Pattern list) )
-- semantic domain
newtype T_Patterns  = T_Patterns ((PP_Doc -> PP_Doc) ->
                                  ( (Set.Set String),Patterns ,([PP_Doc])))
data Inh_Patterns  = Inh_Patterns {addtilde_Inh_Patterns :: (PP_Doc -> PP_Doc)}
data Syn_Patterns  = Syn_Patterns {attrs_Syn_Patterns :: (Set.Set String),copy_Syn_Patterns :: Patterns ,sem_lhs_Syn_Patterns :: ([PP_Doc])}
wrap_Patterns :: T_Patterns  ->
                 Inh_Patterns  ->
                 Syn_Patterns 
wrap_Patterns (T_Patterns sem ) (Inh_Patterns _lhsIaddtilde )  =
    (let ( _lhsOattrs,_lhsOcopy,_lhsOsem_lhs) = sem _lhsIaddtilde 
     in  (Syn_Patterns _lhsOattrs _lhsOcopy _lhsOsem_lhs ))
sem_Patterns_Cons :: T_Pattern  ->
                     T_Patterns  ->
                     T_Patterns 
sem_Patterns_Cons (T_Pattern hd_ ) (T_Patterns tl_ )  =
    (T_Patterns (\ _lhsIaddtilde ->
                     (let _lhsOattrs :: (Set.Set String)
                          _lhsOsem_lhs :: ([PP_Doc])
                          _lhsOcopy :: Patterns 
                          _hdOaddtilde :: (PP_Doc -> PP_Doc)
                          _tlOaddtilde :: (PP_Doc -> PP_Doc)
                          _hdIattrs :: (Set.Set String)
                          _hdIcopy :: Pattern 
                          _hdIsem_lhs :: ( PP_Doc )
                          _tlIattrs :: (Set.Set String)
                          _tlIcopy :: Patterns 
                          _tlIsem_lhs :: ([PP_Doc])
                          -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 427, column 36)
                          _lhsOattrs =
                              ({-# LINE 427 "src-ag/ExecutionPlan2Hs.ag" #-}
                               _hdIattrs `Set.union` _tlIattrs
                               {-# LINE 3489 "src-ag/ExecutionPlan2Hs.hs" #-}
                               )
                          -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 424, column 29)
                          _lhsOsem_lhs =
                              ({-# LINE 424 "src-ag/ExecutionPlan2Hs.ag" #-}
                               _hdIsem_lhs : _tlIsem_lhs
                               {-# LINE 3495 "src-ag/ExecutionPlan2Hs.hs" #-}
                               )
                          -- self rule
                          _copy =
                              ({-# LINE 23 "src-ag/Patterns.ag" #-}
                               (:) _hdIcopy _tlIcopy
                               {-# LINE 3501 "src-ag/ExecutionPlan2Hs.hs" #-}
                               )
                          -- self rule
                          _lhsOcopy =
                              ({-# LINE 23 "src-ag/Patterns.ag" #-}
                               _copy
                               {-# LINE 3507 "src-ag/ExecutionPlan2Hs.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOaddtilde =
                              ({-# LINE 425 "src-ag/ExecutionPlan2Hs.ag" #-}
                               _lhsIaddtilde
                               {-# LINE 3513 "src-ag/ExecutionPlan2Hs.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOaddtilde =
                              ({-# LINE 425 "src-ag/ExecutionPlan2Hs.ag" #-}
                               _lhsIaddtilde
                               {-# LINE 3519 "src-ag/ExecutionPlan2Hs.hs" #-}
                               )
                          ( _hdIattrs,_hdIcopy,_hdIsem_lhs) =
                              hd_ _hdOaddtilde 
                          ( _tlIattrs,_tlIcopy,_tlIsem_lhs) =
                              tl_ _tlOaddtilde 
                      in  ( _lhsOattrs,_lhsOcopy,_lhsOsem_lhs))) )
sem_Patterns_Nil :: T_Patterns 
sem_Patterns_Nil  =
    (T_Patterns (\ _lhsIaddtilde ->
                     (let _lhsOattrs :: (Set.Set String)
                          _lhsOsem_lhs :: ([PP_Doc])
                          _lhsOcopy :: Patterns 
                          -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 427, column 36)
                          _lhsOattrs =
                              ({-# LINE 427 "src-ag/ExecutionPlan2Hs.ag" #-}
                               Set.empty
                               {-# LINE 3536 "src-ag/ExecutionPlan2Hs.hs" #-}
                               )
                          -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 424, column 29)
                          _lhsOsem_lhs =
                              ({-# LINE 424 "src-ag/ExecutionPlan2Hs.ag" #-}
                               []
                               {-# LINE 3542 "src-ag/ExecutionPlan2Hs.hs" #-}
                               )
                          -- self rule
                          _copy =
                              ({-# LINE 23 "src-ag/Patterns.ag" #-}
                               []
                               {-# LINE 3548 "src-ag/ExecutionPlan2Hs.hs" #-}
                               )
                          -- self rule
                          _lhsOcopy =
                              ({-# LINE 23 "src-ag/Patterns.ag" #-}
                               _copy
                               {-# LINE 3554 "src-ag/ExecutionPlan2Hs.hs" #-}
                               )
                      in  ( _lhsOattrs,_lhsOcopy,_lhsOsem_lhs))) )
-- Visit -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allchildvisit        : Map.Map VisitIdentifier (Identifier -> PP_Doc)
         allintramap          : Map.Map StateIdentifier (Set.Set String)
         avisitdefs           : Map.Map VisitIdentifier (Set.Set Identifier)
         avisituses           : Map.Map VisitIdentifier (Set.Set Identifier)
         childintros          : Map.Map Identifier PP_Doc
         inhmap               : Attributes
         mrules               : Map.Map Identifier PP_Doc
         nt                   : NontermIdent
         options              : Options
         ruledefs             : Map.Map Identifier (Set.Set String)
         ruleuses             : Map.Map Identifier (Set.Set String)
         synmap               : Attributes
         terminaldefs         : Set.Set String
      synthesized attributes:
         allvisits            :  VisitStateState 
         childvisit           : Map.Map VisitIdentifier (Identifier -> PP_Doc)
         intramap             : Map.Map StateIdentifier (Set.Set String)
         sem_visit            :   (StateIdentifier,PP_Doc)  
         t_visits             : PP_Doc
         usedrules            : Set.Set Identifier
         visitdefs            : Map.Map VisitIdentifier (Set.Set Identifier)
         visituses            : Map.Map VisitIdentifier (Set.Set Identifier)
   alternatives:
      alternative Visit:
         child ident          : {VisitIdentifier}
         child from           : {StateIdentifier}
         child to             : {StateIdentifier}
         child inh            : {Set.Set Identifier}
         child syn            : {Set.Set Identifier}
         child steps          : VisitSteps 
         visit 0:
            local inhpart     : _
            local synpart     : _
            local inhargs     : _
            local synargs     : _
            local nextargs    : _
            local sem_steps   : _
            local addbang     : _
            local nextintra   : _
            local uses        : _
            local defs        : _
-}
-- cata
sem_Visit :: Visit  ->
             T_Visit 
sem_Visit (Visit _ident _from _to _inh _syn _steps )  =
    (sem_Visit_Visit _ident _from _to _inh _syn (sem_VisitSteps _steps ) )
-- semantic domain
newtype T_Visit  = T_Visit ((Map.Map VisitIdentifier (Identifier -> PP_Doc)) ->
                            (Map.Map StateIdentifier (Set.Set String)) ->
                            (Map.Map VisitIdentifier (Set.Set Identifier)) ->
                            (Map.Map VisitIdentifier (Set.Set Identifier)) ->
                            (Map.Map Identifier PP_Doc) ->
                            Attributes ->
                            (Map.Map Identifier PP_Doc) ->
                            NontermIdent ->
                            Options ->
                            (Map.Map Identifier (Set.Set String)) ->
                            (Map.Map Identifier (Set.Set String)) ->
                            Attributes ->
                            (Set.Set String) ->
                            ( ( VisitStateState ),(Map.Map VisitIdentifier (Identifier -> PP_Doc)),(Map.Map StateIdentifier (Set.Set String)),(  (StateIdentifier,PP_Doc)  ),PP_Doc,(Set.Set Identifier),(Map.Map VisitIdentifier (Set.Set Identifier)),(Map.Map VisitIdentifier (Set.Set Identifier))))
data Inh_Visit  = Inh_Visit {allchildvisit_Inh_Visit :: (Map.Map VisitIdentifier (Identifier -> PP_Doc)),allintramap_Inh_Visit :: (Map.Map StateIdentifier (Set.Set String)),avisitdefs_Inh_Visit :: (Map.Map VisitIdentifier (Set.Set Identifier)),avisituses_Inh_Visit :: (Map.Map VisitIdentifier (Set.Set Identifier)),childintros_Inh_Visit :: (Map.Map Identifier PP_Doc),inhmap_Inh_Visit :: Attributes,mrules_Inh_Visit :: (Map.Map Identifier PP_Doc),nt_Inh_Visit :: NontermIdent,options_Inh_Visit :: Options,ruledefs_Inh_Visit :: (Map.Map Identifier (Set.Set String)),ruleuses_Inh_Visit :: (Map.Map Identifier (Set.Set String)),synmap_Inh_Visit :: Attributes,terminaldefs_Inh_Visit :: (Set.Set String)}
data Syn_Visit  = Syn_Visit {allvisits_Syn_Visit :: ( VisitStateState ),childvisit_Syn_Visit :: (Map.Map VisitIdentifier (Identifier -> PP_Doc)),intramap_Syn_Visit :: (Map.Map StateIdentifier (Set.Set String)),sem_visit_Syn_Visit :: (  (StateIdentifier,PP_Doc)  ),t_visits_Syn_Visit :: PP_Doc,usedrules_Syn_Visit :: (Set.Set Identifier),visitdefs_Syn_Visit :: (Map.Map VisitIdentifier (Set.Set Identifier)),visituses_Syn_Visit :: (Map.Map VisitIdentifier (Set.Set Identifier))}
wrap_Visit :: T_Visit  ->
              Inh_Visit  ->
              Syn_Visit 
wrap_Visit (T_Visit sem ) (Inh_Visit _lhsIallchildvisit _lhsIallintramap _lhsIavisitdefs _lhsIavisituses _lhsIchildintros _lhsIinhmap _lhsImrules _lhsInt _lhsIoptions _lhsIruledefs _lhsIruleuses _lhsIsynmap _lhsIterminaldefs )  =
    (let ( _lhsOallvisits,_lhsOchildvisit,_lhsOintramap,_lhsOsem_visit,_lhsOt_visits,_lhsOusedrules,_lhsOvisitdefs,_lhsOvisituses) = sem _lhsIallchildvisit _lhsIallintramap _lhsIavisitdefs _lhsIavisituses _lhsIchildintros _lhsIinhmap _lhsImrules _lhsInt _lhsIoptions _lhsIruledefs _lhsIruleuses _lhsIsynmap _lhsIterminaldefs 
     in  (Syn_Visit _lhsOallvisits _lhsOchildvisit _lhsOintramap _lhsOsem_visit _lhsOt_visits _lhsOusedrules _lhsOvisitdefs _lhsOvisituses ))
sem_Visit_Visit :: VisitIdentifier ->
                   StateIdentifier ->
                   StateIdentifier ->
                   (Set.Set Identifier) ->
                   (Set.Set Identifier) ->
                   T_VisitSteps  ->
                   T_Visit 
sem_Visit_Visit ident_ from_ to_ inh_ syn_ (T_VisitSteps steps_ )  =
    (T_Visit (\ _lhsIallchildvisit
                _lhsIallintramap
                _lhsIavisitdefs
                _lhsIavisituses
                _lhsIchildintros
                _lhsIinhmap
                _lhsImrules
                _lhsInt
                _lhsIoptions
                _lhsIruledefs
                _lhsIruleuses
                _lhsIsynmap
                _lhsIterminaldefs ->
                  (let _lhsOallvisits :: ( VisitStateState )
                       _lhsOt_visits :: PP_Doc
                       _lhsOsem_visit :: (  (StateIdentifier,PP_Doc)  )
                       _lhsOchildvisit :: (Map.Map VisitIdentifier (Identifier -> PP_Doc))
                       _lhsOintramap :: (Map.Map StateIdentifier (Set.Set String))
                       _lhsOvisitdefs :: (Map.Map VisitIdentifier (Set.Set Identifier))
                       _lhsOvisituses :: (Map.Map VisitIdentifier (Set.Set Identifier))
                       _lhsOusedrules :: (Set.Set Identifier)
                       _stepsOallchildvisit :: (Map.Map VisitIdentifier (Identifier -> PP_Doc))
                       _stepsOavisitdefs :: (Map.Map VisitIdentifier (Set.Set Identifier))
                       _stepsOavisituses :: (Map.Map VisitIdentifier (Set.Set Identifier))
                       _stepsOchildintros :: (Map.Map Identifier PP_Doc)
                       _stepsOmrules :: (Map.Map Identifier PP_Doc)
                       _stepsOruledefs :: (Map.Map Identifier (Set.Set String))
                       _stepsOruleuses :: (Map.Map Identifier (Set.Set String))
                       _stepsIdefs :: (Set.Set String)
                       _stepsIsem_steps :: PP_Doc
                       _stepsIusedrules :: (Set.Set Identifier)
                       _stepsIuses :: (Set.Set String)
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 212, column 11)
                       _lhsOallvisits =
                           ({-# LINE 212 "src-ag/ExecutionPlan2Hs.ag" #-}
                            (ident_, from_, to_)
                            {-# LINE 3675 "src-ag/ExecutionPlan2Hs.hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 262, column 11)
                       _lhsOt_visits =
                           ({-# LINE 262 "src-ag/ExecutionPlan2Hs.ag" #-}
                            "type" >#< "T_" >|< _lhsInt >|< "_v" >|< ident_ >#< "=" >#< _inhpart     >#<
                               "Identity" >#< "(" >#< _synpart     >#< "T_" >|< _lhsInt >|< "_s" >|< to_ >#< ")"
                            {-# LINE 3682 "src-ag/ExecutionPlan2Hs.hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 264, column 11)
                       _inhpart =
                           ({-# LINE 264 "src-ag/ExecutionPlan2Hs.ag" #-}
                            if   Set.null inh_
                            then empty
                            else (ppSpaced $ map (\i -> (\x -> pp_parens x >#< "->") $ typeToHaskellString (Just _lhsInt) []
                                                  $ fromJust $ Map.lookup i _lhsIinhmap) $ Set.toList inh_)
                            {-# LINE 3691 "src-ag/ExecutionPlan2Hs.hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 268, column 11)
                       _synpart =
                           ({-# LINE 268 "src-ag/ExecutionPlan2Hs.ag" #-}
                            if   Set.null syn_
                            then empty
                            else (ppCommas $ map (\i -> typeToHaskellString (Just _lhsInt) [] $ fromJust $ Map.lookup i _lhsIsynmap) $ Set.toList syn_) >#< ","
                            {-# LINE 3699 "src-ag/ExecutionPlan2Hs.hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 364, column 11)
                       _lhsOsem_visit =
                           ({-# LINE 364 "src-ag/ExecutionPlan2Hs.ag" #-}
                            (from_, "v" >|< ident_ >#< "::" >#< "T_" >|< _lhsInt >|< "_v" >|< ident_
                                    >-< "v" >|< ident_ >#< (_inhargs     _LHS True) >#< "=" >#< "do"
                                    >-< indent 3 _sem_steps    )
                            {-# LINE 3707 "src-ag/ExecutionPlan2Hs.hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 367, column 11)
                       _inhargs =
                           ({-# LINE 367 "src-ag/ExecutionPlan2Hs.ag" #-}
                            \chn inh -> ppSpaced $ map (\arg -> attrname inh chn arg) $ Set.toList inh_
                            {-# LINE 3713 "src-ag/ExecutionPlan2Hs.hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 368, column 11)
                       _synargs =
                           ({-# LINE 368 "src-ag/ExecutionPlan2Hs.ag" #-}
                            ppSpaced $ map (\arg -> attrname False _LHS arg >#< ",") $ Set.toList syn_
                            {-# LINE 3719 "src-ag/ExecutionPlan2Hs.hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 369, column 11)
                       _nextargs =
                           ({-# LINE 369 "src-ag/ExecutionPlan2Hs.ag" #-}
                            ppSpaced $ Set.toList $ maybe Set.empty id $ Map.lookup to_ _lhsIallintramap
                            {-# LINE 3725 "src-ag/ExecutionPlan2Hs.hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 370, column 11)
                       _sem_steps =
                           ({-# LINE 370 "src-ag/ExecutionPlan2Hs.ag" #-}
                            _stepsIsem_steps
                            >-< "return" >#< "(" >#< _synargs     >#< "st" >|< to_ >#< _nextargs     >#< ")"
                            {-# LINE 3732 "src-ag/ExecutionPlan2Hs.hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 466, column 11)
                       _lhsOchildvisit =
                           ({-# LINE 466 "src-ag/ExecutionPlan2Hs.ag" #-}
                            Map.singleton ident_ $ \chn -> _addbang     ("(" >#< ppSpaced (
                               map (\x -> attrname True chn x >#< ",") $ Set.toList syn_) >#< locname chn
                            >#< ")") >#< "<-" >#< "inv_" >|< _lhsInt >|< "_s" >|< from_ >#< locname chn
                            >#< "K_" >|< _lhsInt >|< "_v" >|< ident_ >#< _inhargs     chn False
                            {-# LINE 3741 "src-ag/ExecutionPlan2Hs.hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 470, column 11)
                       _addbang =
                           ({-# LINE 470 "src-ag/ExecutionPlan2Hs.ag" #-}
                            \x -> if bangpats _lhsIoptions then "!" >|< x else x
                            {-# LINE 3747 "src-ag/ExecutionPlan2Hs.hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 495, column 11)
                       _lhsOintramap =
                           ({-# LINE 495 "src-ag/ExecutionPlan2Hs.ag" #-}
                            Map.singleton from_ $ (_uses     `Set.union` _nextintra    ) `Set.difference` _defs
                            {-# LINE 3753 "src-ag/ExecutionPlan2Hs.hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 496, column 11)
                       _nextintra =
                           ({-# LINE 496 "src-ag/ExecutionPlan2Hs.ag" #-}
                            maybe Set.empty id $ Map.lookup to_ _lhsIallintramap
                            {-# LINE 3759 "src-ag/ExecutionPlan2Hs.hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 497, column 11)
                       _uses =
                           ({-# LINE 497 "src-ag/ExecutionPlan2Hs.ag" #-}
                            _stepsIuses
                            {-# LINE 3765 "src-ag/ExecutionPlan2Hs.hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 498, column 11)
                       _defs =
                           ({-# LINE 498 "src-ag/ExecutionPlan2Hs.ag" #-}
                            _stepsIdefs `Set.union` (Set.map (lhsname True) inh_) `Set.union` _lhsIterminaldefs
                            {-# LINE 3771 "src-ag/ExecutionPlan2Hs.hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 522, column 11)
                       _lhsOvisitdefs =
                           ({-# LINE 522 "src-ag/ExecutionPlan2Hs.ag" #-}
                            Map.singleton ident_ syn_
                            {-# LINE 3777 "src-ag/ExecutionPlan2Hs.hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 523, column 11)
                       _lhsOvisituses =
                           ({-# LINE 523 "src-ag/ExecutionPlan2Hs.ag" #-}
                            Map.singleton ident_ inh_
                            {-# LINE 3783 "src-ag/ExecutionPlan2Hs.hs" #-}
                            )
                       -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 396, column 56)
                       _lhsOusedrules =
                           ({-# LINE 396 "src-ag/ExecutionPlan2Hs.ag" #-}
                            _stepsIusedrules
                            {-# LINE 3789 "src-ag/ExecutionPlan2Hs.hs" #-}
                            )
                       -- copy rule (down)
                       _stepsOallchildvisit =
                           ({-# LINE 460 "src-ag/ExecutionPlan2Hs.ag" #-}
                            _lhsIallchildvisit
                            {-# LINE 3795 "src-ag/ExecutionPlan2Hs.hs" #-}
                            )
                       -- copy rule (down)
                       _stepsOavisitdefs =
                           ({-# LINE 528 "src-ag/ExecutionPlan2Hs.ag" #-}
                            _lhsIavisitdefs
                            {-# LINE 3801 "src-ag/ExecutionPlan2Hs.hs" #-}
                            )
                       -- copy rule (down)
                       _stepsOavisituses =
                           ({-# LINE 529 "src-ag/ExecutionPlan2Hs.ag" #-}
                            _lhsIavisituses
                            {-# LINE 3807 "src-ag/ExecutionPlan2Hs.hs" #-}
                            )
                       -- copy rule (down)
                       _stepsOchildintros =
                           ({-# LINE 384 "src-ag/ExecutionPlan2Hs.ag" #-}
                            _lhsIchildintros
                            {-# LINE 3813 "src-ag/ExecutionPlan2Hs.hs" #-}
                            )
                       -- copy rule (down)
                       _stepsOmrules =
                           ({-# LINE 373 "src-ag/ExecutionPlan2Hs.ag" #-}
                            _lhsImrules
                            {-# LINE 3819 "src-ag/ExecutionPlan2Hs.hs" #-}
                            )
                       -- copy rule (down)
                       _stepsOruledefs =
                           ({-# LINE 503 "src-ag/ExecutionPlan2Hs.ag" #-}
                            _lhsIruledefs
                            {-# LINE 3825 "src-ag/ExecutionPlan2Hs.hs" #-}
                            )
                       -- copy rule (down)
                       _stepsOruleuses =
                           ({-# LINE 504 "src-ag/ExecutionPlan2Hs.ag" #-}
                            _lhsIruleuses
                            {-# LINE 3831 "src-ag/ExecutionPlan2Hs.hs" #-}
                            )
                       ( _stepsIdefs,_stepsIsem_steps,_stepsIusedrules,_stepsIuses) =
                           steps_ _stepsOallchildvisit _stepsOavisitdefs _stepsOavisituses _stepsOchildintros _stepsOmrules _stepsOruledefs _stepsOruleuses 
                   in  ( _lhsOallvisits,_lhsOchildvisit,_lhsOintramap,_lhsOsem_visit,_lhsOt_visits,_lhsOusedrules,_lhsOvisitdefs,_lhsOvisituses))) )
-- VisitStep ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allchildvisit        : Map.Map VisitIdentifier (Identifier -> PP_Doc)
         avisitdefs           : Map.Map VisitIdentifier (Set.Set Identifier)
         avisituses           : Map.Map VisitIdentifier (Set.Set Identifier)
         childintros          : Map.Map Identifier PP_Doc
         mrules               : Map.Map Identifier PP_Doc
         ruledefs             : Map.Map Identifier (Set.Set String)
         ruleuses             : Map.Map Identifier (Set.Set String)
      synthesized attributes:
         defs                 : Set.Set String
         sem_steps            : PP_Doc
         usedrules            : Set.Set Identifier
         uses                 : Set.Set String
   alternatives:
      alternative ChildIntro:
         child child          : {Identifier}
      alternative ChildVisit:
         child child          : {Identifier}
         child nonterm        : {NontermIdent}
         child visit          : {VisitIdentifier}
      alternative Sem:
         child name           : {Identifier}
      alternative Sim:
         child steps          : VisitSteps 
-}
-- cata
sem_VisitStep :: VisitStep  ->
                 T_VisitStep 
sem_VisitStep (ChildIntro _child )  =
    (sem_VisitStep_ChildIntro _child )
sem_VisitStep (ChildVisit _child _nonterm _visit )  =
    (sem_VisitStep_ChildVisit _child _nonterm _visit )
sem_VisitStep (Sem _name )  =
    (sem_VisitStep_Sem _name )
sem_VisitStep (Sim _steps )  =
    (sem_VisitStep_Sim (sem_VisitSteps _steps ) )
-- semantic domain
newtype T_VisitStep  = T_VisitStep ((Map.Map VisitIdentifier (Identifier -> PP_Doc)) ->
                                    (Map.Map VisitIdentifier (Set.Set Identifier)) ->
                                    (Map.Map VisitIdentifier (Set.Set Identifier)) ->
                                    (Map.Map Identifier PP_Doc) ->
                                    (Map.Map Identifier PP_Doc) ->
                                    (Map.Map Identifier (Set.Set String)) ->
                                    (Map.Map Identifier (Set.Set String)) ->
                                    ( (Set.Set String),PP_Doc,(Set.Set Identifier),(Set.Set String)))
data Inh_VisitStep  = Inh_VisitStep {allchildvisit_Inh_VisitStep :: (Map.Map VisitIdentifier (Identifier -> PP_Doc)),avisitdefs_Inh_VisitStep :: (Map.Map VisitIdentifier (Set.Set Identifier)),avisituses_Inh_VisitStep :: (Map.Map VisitIdentifier (Set.Set Identifier)),childintros_Inh_VisitStep :: (Map.Map Identifier PP_Doc),mrules_Inh_VisitStep :: (Map.Map Identifier PP_Doc),ruledefs_Inh_VisitStep :: (Map.Map Identifier (Set.Set String)),ruleuses_Inh_VisitStep :: (Map.Map Identifier (Set.Set String))}
data Syn_VisitStep  = Syn_VisitStep {defs_Syn_VisitStep :: (Set.Set String),sem_steps_Syn_VisitStep :: PP_Doc,usedrules_Syn_VisitStep :: (Set.Set Identifier),uses_Syn_VisitStep :: (Set.Set String)}
wrap_VisitStep :: T_VisitStep  ->
                  Inh_VisitStep  ->
                  Syn_VisitStep 
wrap_VisitStep (T_VisitStep sem ) (Inh_VisitStep _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIchildintros _lhsImrules _lhsIruledefs _lhsIruleuses )  =
    (let ( _lhsOdefs,_lhsOsem_steps,_lhsOusedrules,_lhsOuses) = sem _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIchildintros _lhsImrules _lhsIruledefs _lhsIruleuses 
     in  (Syn_VisitStep _lhsOdefs _lhsOsem_steps _lhsOusedrules _lhsOuses ))
sem_VisitStep_ChildIntro :: Identifier ->
                            T_VisitStep 
sem_VisitStep_ChildIntro child_  =
    (T_VisitStep (\ _lhsIallchildvisit
                    _lhsIavisitdefs
                    _lhsIavisituses
                    _lhsIchildintros
                    _lhsImrules
                    _lhsIruledefs
                    _lhsIruleuses ->
                      (let _lhsOsem_steps :: PP_Doc
                           _lhsOdefs :: (Set.Set String)
                           _lhsOusedrules :: (Set.Set Identifier)
                           _lhsOuses :: (Set.Set String)
                           -- "src-ag/ExecutionPlan2Hs.ag"(line 377, column 16)
                           _lhsOsem_steps =
                               ({-# LINE 377 "src-ag/ExecutionPlan2Hs.ag" #-}
                                maybe  (error $ "Child " ++ show child_ ++ " not found") id $ Map.lookup child_ _lhsIchildintros
                                {-# LINE 3910 "src-ag/ExecutionPlan2Hs.hs" #-}
                                )
                           -- "src-ag/ExecutionPlan2Hs.ag"(line 540, column 16)
                           _lhsOdefs =
                               ({-# LINE 540 "src-ag/ExecutionPlan2Hs.ag" #-}
                                Set.singleton $ locname child_
                                {-# LINE 3916 "src-ag/ExecutionPlan2Hs.hs" #-}
                                )
                           -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 396, column 56)
                           _lhsOusedrules =
                               ({-# LINE 396 "src-ag/ExecutionPlan2Hs.ag" #-}
                                Set.empty
                                {-# LINE 3922 "src-ag/ExecutionPlan2Hs.hs" #-}
                                )
                           -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 536, column 38)
                           _lhsOuses =
                               ({-# LINE 536 "src-ag/ExecutionPlan2Hs.ag" #-}
                                Set.empty
                                {-# LINE 3928 "src-ag/ExecutionPlan2Hs.hs" #-}
                                )
                       in  ( _lhsOdefs,_lhsOsem_steps,_lhsOusedrules,_lhsOuses))) )
sem_VisitStep_ChildVisit :: Identifier ->
                            NontermIdent ->
                            VisitIdentifier ->
                            T_VisitStep 
sem_VisitStep_ChildVisit child_ nonterm_ visit_  =
    (T_VisitStep (\ _lhsIallchildvisit
                    _lhsIavisitdefs
                    _lhsIavisituses
                    _lhsIchildintros
                    _lhsImrules
                    _lhsIruledefs
                    _lhsIruleuses ->
                      (let _lhsOsem_steps :: PP_Doc
                           _lhsOdefs :: (Set.Set String)
                           _lhsOuses :: (Set.Set String)
                           _lhsOusedrules :: (Set.Set Identifier)
                           -- "src-ag/ExecutionPlan2Hs.ag"(line 378, column 16)
                           _lhsOsem_steps =
                               ({-# LINE 378 "src-ag/ExecutionPlan2Hs.ag" #-}
                                (maybe (error $ "Visit " ++ show visit_ ++ " not found") id $ Map.lookup visit_ _lhsIallchildvisit) $ child_
                                {-# LINE 3951 "src-ag/ExecutionPlan2Hs.hs" #-}
                                )
                           -- "src-ag/ExecutionPlan2Hs.ag"(line 541, column 16)
                           _lhsOdefs =
                               ({-# LINE 541 "src-ag/ExecutionPlan2Hs.ag" #-}
                                maybe (error "Visit not found") (Set.map $ attrname True child_) $ Map.lookup visit_ _lhsIavisitdefs
                                {-# LINE 3957 "src-ag/ExecutionPlan2Hs.hs" #-}
                                )
                           -- "src-ag/ExecutionPlan2Hs.ag"(line 542, column 16)
                           _lhsOuses =
                               ({-# LINE 542 "src-ag/ExecutionPlan2Hs.ag" #-}
                                Set.insert (locname child_) $
                                   maybe (error "Visit not found") (Set.map $ attrname False child_) $ Map.lookup visit_ _lhsIavisituses
                                {-# LINE 3964 "src-ag/ExecutionPlan2Hs.hs" #-}
                                )
                           -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 396, column 56)
                           _lhsOusedrules =
                               ({-# LINE 396 "src-ag/ExecutionPlan2Hs.ag" #-}
                                Set.empty
                                {-# LINE 3970 "src-ag/ExecutionPlan2Hs.hs" #-}
                                )
                       in  ( _lhsOdefs,_lhsOsem_steps,_lhsOusedrules,_lhsOuses))) )
sem_VisitStep_Sem :: Identifier ->
                     T_VisitStep 
sem_VisitStep_Sem name_  =
    (T_VisitStep (\ _lhsIallchildvisit
                    _lhsIavisitdefs
                    _lhsIavisituses
                    _lhsIchildintros
                    _lhsImrules
                    _lhsIruledefs
                    _lhsIruleuses ->
                      (let _lhsOsem_steps :: PP_Doc
                           _lhsOusedrules :: (Set.Set Identifier)
                           _lhsOdefs :: (Set.Set String)
                           _lhsOuses :: (Set.Set String)
                           -- "src-ag/ExecutionPlan2Hs.ag"(line 376, column 16)
                           _lhsOsem_steps =
                               ({-# LINE 376 "src-ag/ExecutionPlan2Hs.ag" #-}
                                maybe  (error $ "Rule "  ++ show name_  ++ " not found") id $ Map.lookup name_  _lhsImrules
                                {-# LINE 3991 "src-ag/ExecutionPlan2Hs.hs" #-}
                                )
                           -- "src-ag/ExecutionPlan2Hs.ag"(line 400, column 9)
                           _lhsOusedrules =
                               ({-# LINE 400 "src-ag/ExecutionPlan2Hs.ag" #-}
                                Set.singleton name_
                                {-# LINE 3997 "src-ag/ExecutionPlan2Hs.hs" #-}
                                )
                           -- "src-ag/ExecutionPlan2Hs.ag"(line 538, column 16)
                           _lhsOdefs =
                               ({-# LINE 538 "src-ag/ExecutionPlan2Hs.ag" #-}
                                maybe (error "Rule not found") id $ Map.lookup name_ _lhsIruledefs
                                {-# LINE 4003 "src-ag/ExecutionPlan2Hs.hs" #-}
                                )
                           -- "src-ag/ExecutionPlan2Hs.ag"(line 539, column 16)
                           _lhsOuses =
                               ({-# LINE 539 "src-ag/ExecutionPlan2Hs.ag" #-}
                                maybe (error "Rule not found") id $ Map.lookup name_ _lhsIruleuses
                                {-# LINE 4009 "src-ag/ExecutionPlan2Hs.hs" #-}
                                )
                       in  ( _lhsOdefs,_lhsOsem_steps,_lhsOusedrules,_lhsOuses))) )
sem_VisitStep_Sim :: T_VisitSteps  ->
                     T_VisitStep 
sem_VisitStep_Sim (T_VisitSteps steps_ )  =
    (T_VisitStep (\ _lhsIallchildvisit
                    _lhsIavisitdefs
                    _lhsIavisituses
                    _lhsIchildintros
                    _lhsImrules
                    _lhsIruledefs
                    _lhsIruleuses ->
                      (let _lhsOdefs :: (Set.Set String)
                           _lhsOsem_steps :: PP_Doc
                           _lhsOusedrules :: (Set.Set Identifier)
                           _lhsOuses :: (Set.Set String)
                           _stepsOallchildvisit :: (Map.Map VisitIdentifier (Identifier -> PP_Doc))
                           _stepsOavisitdefs :: (Map.Map VisitIdentifier (Set.Set Identifier))
                           _stepsOavisituses :: (Map.Map VisitIdentifier (Set.Set Identifier))
                           _stepsOchildintros :: (Map.Map Identifier PP_Doc)
                           _stepsOmrules :: (Map.Map Identifier PP_Doc)
                           _stepsOruledefs :: (Map.Map Identifier (Set.Set String))
                           _stepsOruleuses :: (Map.Map Identifier (Set.Set String))
                           _stepsIdefs :: (Set.Set String)
                           _stepsIsem_steps :: PP_Doc
                           _stepsIusedrules :: (Set.Set Identifier)
                           _stepsIuses :: (Set.Set String)
                           -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 535, column 38)
                           _lhsOdefs =
                               ({-# LINE 535 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _stepsIdefs
                                {-# LINE 4041 "src-ag/ExecutionPlan2Hs.hs" #-}
                                )
                           -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 374, column 43)
                           _lhsOsem_steps =
                               ({-# LINE 374 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _stepsIsem_steps
                                {-# LINE 4047 "src-ag/ExecutionPlan2Hs.hs" #-}
                                )
                           -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 396, column 56)
                           _lhsOusedrules =
                               ({-# LINE 396 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _stepsIusedrules
                                {-# LINE 4053 "src-ag/ExecutionPlan2Hs.hs" #-}
                                )
                           -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 536, column 38)
                           _lhsOuses =
                               ({-# LINE 536 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _stepsIuses
                                {-# LINE 4059 "src-ag/ExecutionPlan2Hs.hs" #-}
                                )
                           -- copy rule (down)
                           _stepsOallchildvisit =
                               ({-# LINE 460 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsIallchildvisit
                                {-# LINE 4065 "src-ag/ExecutionPlan2Hs.hs" #-}
                                )
                           -- copy rule (down)
                           _stepsOavisitdefs =
                               ({-# LINE 528 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsIavisitdefs
                                {-# LINE 4071 "src-ag/ExecutionPlan2Hs.hs" #-}
                                )
                           -- copy rule (down)
                           _stepsOavisituses =
                               ({-# LINE 529 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsIavisituses
                                {-# LINE 4077 "src-ag/ExecutionPlan2Hs.hs" #-}
                                )
                           -- copy rule (down)
                           _stepsOchildintros =
                               ({-# LINE 384 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsIchildintros
                                {-# LINE 4083 "src-ag/ExecutionPlan2Hs.hs" #-}
                                )
                           -- copy rule (down)
                           _stepsOmrules =
                               ({-# LINE 373 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsImrules
                                {-# LINE 4089 "src-ag/ExecutionPlan2Hs.hs" #-}
                                )
                           -- copy rule (down)
                           _stepsOruledefs =
                               ({-# LINE 503 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsIruledefs
                                {-# LINE 4095 "src-ag/ExecutionPlan2Hs.hs" #-}
                                )
                           -- copy rule (down)
                           _stepsOruleuses =
                               ({-# LINE 504 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsIruleuses
                                {-# LINE 4101 "src-ag/ExecutionPlan2Hs.hs" #-}
                                )
                           ( _stepsIdefs,_stepsIsem_steps,_stepsIusedrules,_stepsIuses) =
                               steps_ _stepsOallchildvisit _stepsOavisitdefs _stepsOavisituses _stepsOchildintros _stepsOmrules _stepsOruledefs _stepsOruleuses 
                       in  ( _lhsOdefs,_lhsOsem_steps,_lhsOusedrules,_lhsOuses))) )
-- VisitSteps --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allchildvisit        : Map.Map VisitIdentifier (Identifier -> PP_Doc)
         avisitdefs           : Map.Map VisitIdentifier (Set.Set Identifier)
         avisituses           : Map.Map VisitIdentifier (Set.Set Identifier)
         childintros          : Map.Map Identifier PP_Doc
         mrules               : Map.Map Identifier PP_Doc
         ruledefs             : Map.Map Identifier (Set.Set String)
         ruleuses             : Map.Map Identifier (Set.Set String)
      synthesized attributes:
         defs                 : Set.Set String
         sem_steps            : PP_Doc
         usedrules            : Set.Set Identifier
         uses                 : Set.Set String
   alternatives:
      alternative Cons:
         child hd             : VisitStep 
         child tl             : VisitSteps 
      alternative Nil:
-}
-- cata
sem_VisitSteps :: VisitSteps  ->
                  T_VisitSteps 
sem_VisitSteps list  =
    (Prelude.foldr sem_VisitSteps_Cons sem_VisitSteps_Nil (Prelude.map sem_VisitStep list) )
-- semantic domain
newtype T_VisitSteps  = T_VisitSteps ((Map.Map VisitIdentifier (Identifier -> PP_Doc)) ->
                                      (Map.Map VisitIdentifier (Set.Set Identifier)) ->
                                      (Map.Map VisitIdentifier (Set.Set Identifier)) ->
                                      (Map.Map Identifier PP_Doc) ->
                                      (Map.Map Identifier PP_Doc) ->
                                      (Map.Map Identifier (Set.Set String)) ->
                                      (Map.Map Identifier (Set.Set String)) ->
                                      ( (Set.Set String),PP_Doc,(Set.Set Identifier),(Set.Set String)))
data Inh_VisitSteps  = Inh_VisitSteps {allchildvisit_Inh_VisitSteps :: (Map.Map VisitIdentifier (Identifier -> PP_Doc)),avisitdefs_Inh_VisitSteps :: (Map.Map VisitIdentifier (Set.Set Identifier)),avisituses_Inh_VisitSteps :: (Map.Map VisitIdentifier (Set.Set Identifier)),childintros_Inh_VisitSteps :: (Map.Map Identifier PP_Doc),mrules_Inh_VisitSteps :: (Map.Map Identifier PP_Doc),ruledefs_Inh_VisitSteps :: (Map.Map Identifier (Set.Set String)),ruleuses_Inh_VisitSteps :: (Map.Map Identifier (Set.Set String))}
data Syn_VisitSteps  = Syn_VisitSteps {defs_Syn_VisitSteps :: (Set.Set String),sem_steps_Syn_VisitSteps :: PP_Doc,usedrules_Syn_VisitSteps :: (Set.Set Identifier),uses_Syn_VisitSteps :: (Set.Set String)}
wrap_VisitSteps :: T_VisitSteps  ->
                   Inh_VisitSteps  ->
                   Syn_VisitSteps 
wrap_VisitSteps (T_VisitSteps sem ) (Inh_VisitSteps _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIchildintros _lhsImrules _lhsIruledefs _lhsIruleuses )  =
    (let ( _lhsOdefs,_lhsOsem_steps,_lhsOusedrules,_lhsOuses) = sem _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIchildintros _lhsImrules _lhsIruledefs _lhsIruleuses 
     in  (Syn_VisitSteps _lhsOdefs _lhsOsem_steps _lhsOusedrules _lhsOuses ))
sem_VisitSteps_Cons :: T_VisitStep  ->
                       T_VisitSteps  ->
                       T_VisitSteps 
sem_VisitSteps_Cons (T_VisitStep hd_ ) (T_VisitSteps tl_ )  =
    (T_VisitSteps (\ _lhsIallchildvisit
                     _lhsIavisitdefs
                     _lhsIavisituses
                     _lhsIchildintros
                     _lhsImrules
                     _lhsIruledefs
                     _lhsIruleuses ->
                       (let _lhsOdefs :: (Set.Set String)
                            _lhsOsem_steps :: PP_Doc
                            _lhsOusedrules :: (Set.Set Identifier)
                            _lhsOuses :: (Set.Set String)
                            _hdOallchildvisit :: (Map.Map VisitIdentifier (Identifier -> PP_Doc))
                            _hdOavisitdefs :: (Map.Map VisitIdentifier (Set.Set Identifier))
                            _hdOavisituses :: (Map.Map VisitIdentifier (Set.Set Identifier))
                            _hdOchildintros :: (Map.Map Identifier PP_Doc)
                            _hdOmrules :: (Map.Map Identifier PP_Doc)
                            _hdOruledefs :: (Map.Map Identifier (Set.Set String))
                            _hdOruleuses :: (Map.Map Identifier (Set.Set String))
                            _tlOallchildvisit :: (Map.Map VisitIdentifier (Identifier -> PP_Doc))
                            _tlOavisitdefs :: (Map.Map VisitIdentifier (Set.Set Identifier))
                            _tlOavisituses :: (Map.Map VisitIdentifier (Set.Set Identifier))
                            _tlOchildintros :: (Map.Map Identifier PP_Doc)
                            _tlOmrules :: (Map.Map Identifier PP_Doc)
                            _tlOruledefs :: (Map.Map Identifier (Set.Set String))
                            _tlOruleuses :: (Map.Map Identifier (Set.Set String))
                            _hdIdefs :: (Set.Set String)
                            _hdIsem_steps :: PP_Doc
                            _hdIusedrules :: (Set.Set Identifier)
                            _hdIuses :: (Set.Set String)
                            _tlIdefs :: (Set.Set String)
                            _tlIsem_steps :: PP_Doc
                            _tlIusedrules :: (Set.Set Identifier)
                            _tlIuses :: (Set.Set String)
                            -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 535, column 38)
                            _lhsOdefs =
                                ({-# LINE 535 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 _hdIdefs `Set.union` _tlIdefs
                                 {-# LINE 4191 "src-ag/ExecutionPlan2Hs.hs" #-}
                                 )
                            -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 374, column 43)
                            _lhsOsem_steps =
                                ({-# LINE 374 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 _hdIsem_steps >-< _tlIsem_steps
                                 {-# LINE 4197 "src-ag/ExecutionPlan2Hs.hs" #-}
                                 )
                            -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 396, column 56)
                            _lhsOusedrules =
                                ({-# LINE 396 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 _hdIusedrules `Set.union` _tlIusedrules
                                 {-# LINE 4203 "src-ag/ExecutionPlan2Hs.hs" #-}
                                 )
                            -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 536, column 38)
                            _lhsOuses =
                                ({-# LINE 536 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 _hdIuses `Set.union` _tlIuses
                                 {-# LINE 4209 "src-ag/ExecutionPlan2Hs.hs" #-}
                                 )
                            -- copy rule (down)
                            _hdOallchildvisit =
                                ({-# LINE 460 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 _lhsIallchildvisit
                                 {-# LINE 4215 "src-ag/ExecutionPlan2Hs.hs" #-}
                                 )
                            -- copy rule (down)
                            _hdOavisitdefs =
                                ({-# LINE 528 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 _lhsIavisitdefs
                                 {-# LINE 4221 "src-ag/ExecutionPlan2Hs.hs" #-}
                                 )
                            -- copy rule (down)
                            _hdOavisituses =
                                ({-# LINE 529 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 _lhsIavisituses
                                 {-# LINE 4227 "src-ag/ExecutionPlan2Hs.hs" #-}
                                 )
                            -- copy rule (down)
                            _hdOchildintros =
                                ({-# LINE 384 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 _lhsIchildintros
                                 {-# LINE 4233 "src-ag/ExecutionPlan2Hs.hs" #-}
                                 )
                            -- copy rule (down)
                            _hdOmrules =
                                ({-# LINE 373 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 _lhsImrules
                                 {-# LINE 4239 "src-ag/ExecutionPlan2Hs.hs" #-}
                                 )
                            -- copy rule (down)
                            _hdOruledefs =
                                ({-# LINE 503 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 _lhsIruledefs
                                 {-# LINE 4245 "src-ag/ExecutionPlan2Hs.hs" #-}
                                 )
                            -- copy rule (down)
                            _hdOruleuses =
                                ({-# LINE 504 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 _lhsIruleuses
                                 {-# LINE 4251 "src-ag/ExecutionPlan2Hs.hs" #-}
                                 )
                            -- copy rule (down)
                            _tlOallchildvisit =
                                ({-# LINE 460 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 _lhsIallchildvisit
                                 {-# LINE 4257 "src-ag/ExecutionPlan2Hs.hs" #-}
                                 )
                            -- copy rule (down)
                            _tlOavisitdefs =
                                ({-# LINE 528 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 _lhsIavisitdefs
                                 {-# LINE 4263 "src-ag/ExecutionPlan2Hs.hs" #-}
                                 )
                            -- copy rule (down)
                            _tlOavisituses =
                                ({-# LINE 529 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 _lhsIavisituses
                                 {-# LINE 4269 "src-ag/ExecutionPlan2Hs.hs" #-}
                                 )
                            -- copy rule (down)
                            _tlOchildintros =
                                ({-# LINE 384 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 _lhsIchildintros
                                 {-# LINE 4275 "src-ag/ExecutionPlan2Hs.hs" #-}
                                 )
                            -- copy rule (down)
                            _tlOmrules =
                                ({-# LINE 373 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 _lhsImrules
                                 {-# LINE 4281 "src-ag/ExecutionPlan2Hs.hs" #-}
                                 )
                            -- copy rule (down)
                            _tlOruledefs =
                                ({-# LINE 503 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 _lhsIruledefs
                                 {-# LINE 4287 "src-ag/ExecutionPlan2Hs.hs" #-}
                                 )
                            -- copy rule (down)
                            _tlOruleuses =
                                ({-# LINE 504 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 _lhsIruleuses
                                 {-# LINE 4293 "src-ag/ExecutionPlan2Hs.hs" #-}
                                 )
                            ( _hdIdefs,_hdIsem_steps,_hdIusedrules,_hdIuses) =
                                hd_ _hdOallchildvisit _hdOavisitdefs _hdOavisituses _hdOchildintros _hdOmrules _hdOruledefs _hdOruleuses 
                            ( _tlIdefs,_tlIsem_steps,_tlIusedrules,_tlIuses) =
                                tl_ _tlOallchildvisit _tlOavisitdefs _tlOavisituses _tlOchildintros _tlOmrules _tlOruledefs _tlOruleuses 
                        in  ( _lhsOdefs,_lhsOsem_steps,_lhsOusedrules,_lhsOuses))) )
sem_VisitSteps_Nil :: T_VisitSteps 
sem_VisitSteps_Nil  =
    (T_VisitSteps (\ _lhsIallchildvisit
                     _lhsIavisitdefs
                     _lhsIavisituses
                     _lhsIchildintros
                     _lhsImrules
                     _lhsIruledefs
                     _lhsIruleuses ->
                       (let _lhsOdefs :: (Set.Set String)
                            _lhsOsem_steps :: PP_Doc
                            _lhsOusedrules :: (Set.Set Identifier)
                            _lhsOuses :: (Set.Set String)
                            -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 535, column 38)
                            _lhsOdefs =
                                ({-# LINE 535 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 Set.empty
                                 {-# LINE 4317 "src-ag/ExecutionPlan2Hs.hs" #-}
                                 )
                            -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 374, column 43)
                            _lhsOsem_steps =
                                ({-# LINE 374 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 empty
                                 {-# LINE 4323 "src-ag/ExecutionPlan2Hs.hs" #-}
                                 )
                            -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 396, column 56)
                            _lhsOusedrules =
                                ({-# LINE 396 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 Set.empty
                                 {-# LINE 4329 "src-ag/ExecutionPlan2Hs.hs" #-}
                                 )
                            -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 536, column 38)
                            _lhsOuses =
                                ({-# LINE 536 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 Set.empty
                                 {-# LINE 4335 "src-ag/ExecutionPlan2Hs.hs" #-}
                                 )
                        in  ( _lhsOdefs,_lhsOsem_steps,_lhsOusedrules,_lhsOuses))) )
-- Visits ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allchildvisit        : Map.Map VisitIdentifier (Identifier -> PP_Doc)
         allintramap          : Map.Map StateIdentifier (Set.Set String)
         avisitdefs           : Map.Map VisitIdentifier (Set.Set Identifier)
         avisituses           : Map.Map VisitIdentifier (Set.Set Identifier)
         childintros          : Map.Map Identifier PP_Doc
         inhmap               : Attributes
         mrules               : Map.Map Identifier PP_Doc
         nt                   : NontermIdent
         options              : Options
         ruledefs             : Map.Map Identifier (Set.Set String)
         ruleuses             : Map.Map Identifier (Set.Set String)
         synmap               : Attributes
         terminaldefs         : Set.Set String
      synthesized attributes:
         allvisits            : [VisitStateState]
         childvisit           : Map.Map VisitIdentifier (Identifier -> PP_Doc)
         intramap             : Map.Map StateIdentifier (Set.Set String)
         sem_visit            :  [(StateIdentifier,PP_Doc)] 
         t_visits             : PP_Doc
         usedrules            : Set.Set Identifier
         visitdefs            : Map.Map VisitIdentifier (Set.Set Identifier)
         visituses            : Map.Map VisitIdentifier (Set.Set Identifier)
   alternatives:
      alternative Cons:
         child hd             : Visit 
         child tl             : Visits 
      alternative Nil:
-}
-- cata
sem_Visits :: Visits  ->
              T_Visits 
sem_Visits list  =
    (Prelude.foldr sem_Visits_Cons sem_Visits_Nil (Prelude.map sem_Visit list) )
-- semantic domain
newtype T_Visits  = T_Visits ((Map.Map VisitIdentifier (Identifier -> PP_Doc)) ->
                              (Map.Map StateIdentifier (Set.Set String)) ->
                              (Map.Map VisitIdentifier (Set.Set Identifier)) ->
                              (Map.Map VisitIdentifier (Set.Set Identifier)) ->
                              (Map.Map Identifier PP_Doc) ->
                              Attributes ->
                              (Map.Map Identifier PP_Doc) ->
                              NontermIdent ->
                              Options ->
                              (Map.Map Identifier (Set.Set String)) ->
                              (Map.Map Identifier (Set.Set String)) ->
                              Attributes ->
                              (Set.Set String) ->
                              ( ([VisitStateState]),(Map.Map VisitIdentifier (Identifier -> PP_Doc)),(Map.Map StateIdentifier (Set.Set String)),( [(StateIdentifier,PP_Doc)] ),PP_Doc,(Set.Set Identifier),(Map.Map VisitIdentifier (Set.Set Identifier)),(Map.Map VisitIdentifier (Set.Set Identifier))))
data Inh_Visits  = Inh_Visits {allchildvisit_Inh_Visits :: (Map.Map VisitIdentifier (Identifier -> PP_Doc)),allintramap_Inh_Visits :: (Map.Map StateIdentifier (Set.Set String)),avisitdefs_Inh_Visits :: (Map.Map VisitIdentifier (Set.Set Identifier)),avisituses_Inh_Visits :: (Map.Map VisitIdentifier (Set.Set Identifier)),childintros_Inh_Visits :: (Map.Map Identifier PP_Doc),inhmap_Inh_Visits :: Attributes,mrules_Inh_Visits :: (Map.Map Identifier PP_Doc),nt_Inh_Visits :: NontermIdent,options_Inh_Visits :: Options,ruledefs_Inh_Visits :: (Map.Map Identifier (Set.Set String)),ruleuses_Inh_Visits :: (Map.Map Identifier (Set.Set String)),synmap_Inh_Visits :: Attributes,terminaldefs_Inh_Visits :: (Set.Set String)}
data Syn_Visits  = Syn_Visits {allvisits_Syn_Visits :: ([VisitStateState]),childvisit_Syn_Visits :: (Map.Map VisitIdentifier (Identifier -> PP_Doc)),intramap_Syn_Visits :: (Map.Map StateIdentifier (Set.Set String)),sem_visit_Syn_Visits :: ( [(StateIdentifier,PP_Doc)] ),t_visits_Syn_Visits :: PP_Doc,usedrules_Syn_Visits :: (Set.Set Identifier),visitdefs_Syn_Visits :: (Map.Map VisitIdentifier (Set.Set Identifier)),visituses_Syn_Visits :: (Map.Map VisitIdentifier (Set.Set Identifier))}
wrap_Visits :: T_Visits  ->
               Inh_Visits  ->
               Syn_Visits 
wrap_Visits (T_Visits sem ) (Inh_Visits _lhsIallchildvisit _lhsIallintramap _lhsIavisitdefs _lhsIavisituses _lhsIchildintros _lhsIinhmap _lhsImrules _lhsInt _lhsIoptions _lhsIruledefs _lhsIruleuses _lhsIsynmap _lhsIterminaldefs )  =
    (let ( _lhsOallvisits,_lhsOchildvisit,_lhsOintramap,_lhsOsem_visit,_lhsOt_visits,_lhsOusedrules,_lhsOvisitdefs,_lhsOvisituses) = sem _lhsIallchildvisit _lhsIallintramap _lhsIavisitdefs _lhsIavisituses _lhsIchildintros _lhsIinhmap _lhsImrules _lhsInt _lhsIoptions _lhsIruledefs _lhsIruleuses _lhsIsynmap _lhsIterminaldefs 
     in  (Syn_Visits _lhsOallvisits _lhsOchildvisit _lhsOintramap _lhsOsem_visit _lhsOt_visits _lhsOusedrules _lhsOvisitdefs _lhsOvisituses ))
sem_Visits_Cons :: T_Visit  ->
                   T_Visits  ->
                   T_Visits 
sem_Visits_Cons (T_Visit hd_ ) (T_Visits tl_ )  =
    (T_Visits (\ _lhsIallchildvisit
                 _lhsIallintramap
                 _lhsIavisitdefs
                 _lhsIavisituses
                 _lhsIchildintros
                 _lhsIinhmap
                 _lhsImrules
                 _lhsInt
                 _lhsIoptions
                 _lhsIruledefs
                 _lhsIruleuses
                 _lhsIsynmap
                 _lhsIterminaldefs ->
                   (let _lhsOallvisits :: ([VisitStateState])
                        _lhsOchildvisit :: (Map.Map VisitIdentifier (Identifier -> PP_Doc))
                        _lhsOintramap :: (Map.Map StateIdentifier (Set.Set String))
                        _lhsOsem_visit :: ( [(StateIdentifier,PP_Doc)] )
                        _lhsOt_visits :: PP_Doc
                        _lhsOusedrules :: (Set.Set Identifier)
                        _lhsOvisitdefs :: (Map.Map VisitIdentifier (Set.Set Identifier))
                        _lhsOvisituses :: (Map.Map VisitIdentifier (Set.Set Identifier))
                        _hdOallchildvisit :: (Map.Map VisitIdentifier (Identifier -> PP_Doc))
                        _hdOallintramap :: (Map.Map StateIdentifier (Set.Set String))
                        _hdOavisitdefs :: (Map.Map VisitIdentifier (Set.Set Identifier))
                        _hdOavisituses :: (Map.Map VisitIdentifier (Set.Set Identifier))
                        _hdOchildintros :: (Map.Map Identifier PP_Doc)
                        _hdOinhmap :: Attributes
                        _hdOmrules :: (Map.Map Identifier PP_Doc)
                        _hdOnt :: NontermIdent
                        _hdOoptions :: Options
                        _hdOruledefs :: (Map.Map Identifier (Set.Set String))
                        _hdOruleuses :: (Map.Map Identifier (Set.Set String))
                        _hdOsynmap :: Attributes
                        _hdOterminaldefs :: (Set.Set String)
                        _tlOallchildvisit :: (Map.Map VisitIdentifier (Identifier -> PP_Doc))
                        _tlOallintramap :: (Map.Map StateIdentifier (Set.Set String))
                        _tlOavisitdefs :: (Map.Map VisitIdentifier (Set.Set Identifier))
                        _tlOavisituses :: (Map.Map VisitIdentifier (Set.Set Identifier))
                        _tlOchildintros :: (Map.Map Identifier PP_Doc)
                        _tlOinhmap :: Attributes
                        _tlOmrules :: (Map.Map Identifier PP_Doc)
                        _tlOnt :: NontermIdent
                        _tlOoptions :: Options
                        _tlOruledefs :: (Map.Map Identifier (Set.Set String))
                        _tlOruleuses :: (Map.Map Identifier (Set.Set String))
                        _tlOsynmap :: Attributes
                        _tlOterminaldefs :: (Set.Set String)
                        _hdIallvisits :: ( VisitStateState )
                        _hdIchildvisit :: (Map.Map VisitIdentifier (Identifier -> PP_Doc))
                        _hdIintramap :: (Map.Map StateIdentifier (Set.Set String))
                        _hdIsem_visit :: (  (StateIdentifier,PP_Doc)  )
                        _hdIt_visits :: PP_Doc
                        _hdIusedrules :: (Set.Set Identifier)
                        _hdIvisitdefs :: (Map.Map VisitIdentifier (Set.Set Identifier))
                        _hdIvisituses :: (Map.Map VisitIdentifier (Set.Set Identifier))
                        _tlIallvisits :: ([VisitStateState])
                        _tlIchildvisit :: (Map.Map VisitIdentifier (Identifier -> PP_Doc))
                        _tlIintramap :: (Map.Map StateIdentifier (Set.Set String))
                        _tlIsem_visit :: ( [(StateIdentifier,PP_Doc)] )
                        _tlIt_visits :: PP_Doc
                        _tlIusedrules :: (Set.Set Identifier)
                        _tlIvisitdefs :: (Map.Map VisitIdentifier (Set.Set Identifier))
                        _tlIvisituses :: (Map.Map VisitIdentifier (Set.Set Identifier))
                        -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 207, column 29)
                        _lhsOallvisits =
                            ({-# LINE 207 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _hdIallvisits : _tlIallvisits
                             {-# LINE 4469 "src-ag/ExecutionPlan2Hs.hs" #-}
                             )
                        -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 458, column 37)
                        _lhsOchildvisit =
                            ({-# LINE 458 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _hdIchildvisit `Map.union` _tlIchildvisit
                             {-# LINE 4475 "src-ag/ExecutionPlan2Hs.hs" #-}
                             )
                        -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 480, column 34)
                        _lhsOintramap =
                            ({-# LINE 480 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _hdIintramap `uwSetUnion` _tlIintramap
                             {-# LINE 4481 "src-ag/ExecutionPlan2Hs.hs" #-}
                             )
                        -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 361, column 29)
                        _lhsOsem_visit =
                            ({-# LINE 361 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _hdIsem_visit : _tlIsem_visit
                             {-# LINE 4487 "src-ag/ExecutionPlan2Hs.hs" #-}
                             )
                        -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 253, column 54)
                        _lhsOt_visits =
                            ({-# LINE 253 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _hdIt_visits >-< _tlIt_visits
                             {-# LINE 4493 "src-ag/ExecutionPlan2Hs.hs" #-}
                             )
                        -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 396, column 56)
                        _lhsOusedrules =
                            ({-# LINE 396 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _hdIusedrules `Set.union` _tlIusedrules
                             {-# LINE 4499 "src-ag/ExecutionPlan2Hs.hs" #-}
                             )
                        -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 518, column 36)
                        _lhsOvisitdefs =
                            ({-# LINE 518 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _hdIvisitdefs `uwSetUnion` _tlIvisitdefs
                             {-# LINE 4505 "src-ag/ExecutionPlan2Hs.hs" #-}
                             )
                        -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 519, column 36)
                        _lhsOvisituses =
                            ({-# LINE 519 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _hdIvisituses `uwSetUnion` _tlIvisituses
                             {-# LINE 4511 "src-ag/ExecutionPlan2Hs.hs" #-}
                             )
                        -- copy rule (down)
                        _hdOallchildvisit =
                            ({-# LINE 457 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIallchildvisit
                             {-# LINE 4517 "src-ag/ExecutionPlan2Hs.hs" #-}
                             )
                        -- copy rule (down)
                        _hdOallintramap =
                            ({-# LINE 479 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIallintramap
                             {-# LINE 4523 "src-ag/ExecutionPlan2Hs.hs" #-}
                             )
                        -- copy rule (down)
                        _hdOavisitdefs =
                            ({-# LINE 528 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIavisitdefs
                             {-# LINE 4529 "src-ag/ExecutionPlan2Hs.hs" #-}
                             )
                        -- copy rule (down)
                        _hdOavisituses =
                            ({-# LINE 529 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIavisituses
                             {-# LINE 4535 "src-ag/ExecutionPlan2Hs.hs" #-}
                             )
                        -- copy rule (down)
                        _hdOchildintros =
                            ({-# LINE 384 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIchildintros
                             {-# LINE 4541 "src-ag/ExecutionPlan2Hs.hs" #-}
                             )
                        -- copy rule (down)
                        _hdOinhmap =
                            ({-# LINE 194 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIinhmap
                             {-# LINE 4547 "src-ag/ExecutionPlan2Hs.hs" #-}
                             )
                        -- copy rule (down)
                        _hdOmrules =
                            ({-# LINE 373 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsImrules
                             {-# LINE 4553 "src-ag/ExecutionPlan2Hs.hs" #-}
                             )
                        -- copy rule (down)
                        _hdOnt =
                            ({-# LINE 253 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsInt
                             {-# LINE 4559 "src-ag/ExecutionPlan2Hs.hs" #-}
                             )
                        -- copy rule (down)
                        _hdOoptions =
                            ({-# LINE 41 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIoptions
                             {-# LINE 4565 "src-ag/ExecutionPlan2Hs.hs" #-}
                             )
                        -- copy rule (down)
                        _hdOruledefs =
                            ({-# LINE 503 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIruledefs
                             {-# LINE 4571 "src-ag/ExecutionPlan2Hs.hs" #-}
                             )
                        -- copy rule (down)
                        _hdOruleuses =
                            ({-# LINE 504 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIruleuses
                             {-# LINE 4577 "src-ag/ExecutionPlan2Hs.hs" #-}
                             )
                        -- copy rule (down)
                        _hdOsynmap =
                            ({-# LINE 195 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIsynmap
                             {-# LINE 4583 "src-ag/ExecutionPlan2Hs.hs" #-}
                             )
                        -- copy rule (down)
                        _hdOterminaldefs =
                            ({-# LINE 482 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIterminaldefs
                             {-# LINE 4589 "src-ag/ExecutionPlan2Hs.hs" #-}
                             )
                        -- copy rule (down)
                        _tlOallchildvisit =
                            ({-# LINE 457 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIallchildvisit
                             {-# LINE 4595 "src-ag/ExecutionPlan2Hs.hs" #-}
                             )
                        -- copy rule (down)
                        _tlOallintramap =
                            ({-# LINE 479 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIallintramap
                             {-# LINE 4601 "src-ag/ExecutionPlan2Hs.hs" #-}
                             )
                        -- copy rule (down)
                        _tlOavisitdefs =
                            ({-# LINE 528 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIavisitdefs
                             {-# LINE 4607 "src-ag/ExecutionPlan2Hs.hs" #-}
                             )
                        -- copy rule (down)
                        _tlOavisituses =
                            ({-# LINE 529 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIavisituses
                             {-# LINE 4613 "src-ag/ExecutionPlan2Hs.hs" #-}
                             )
                        -- copy rule (down)
                        _tlOchildintros =
                            ({-# LINE 384 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIchildintros
                             {-# LINE 4619 "src-ag/ExecutionPlan2Hs.hs" #-}
                             )
                        -- copy rule (down)
                        _tlOinhmap =
                            ({-# LINE 194 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIinhmap
                             {-# LINE 4625 "src-ag/ExecutionPlan2Hs.hs" #-}
                             )
                        -- copy rule (down)
                        _tlOmrules =
                            ({-# LINE 373 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsImrules
                             {-# LINE 4631 "src-ag/ExecutionPlan2Hs.hs" #-}
                             )
                        -- copy rule (down)
                        _tlOnt =
                            ({-# LINE 253 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsInt
                             {-# LINE 4637 "src-ag/ExecutionPlan2Hs.hs" #-}
                             )
                        -- copy rule (down)
                        _tlOoptions =
                            ({-# LINE 41 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIoptions
                             {-# LINE 4643 "src-ag/ExecutionPlan2Hs.hs" #-}
                             )
                        -- copy rule (down)
                        _tlOruledefs =
                            ({-# LINE 503 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIruledefs
                             {-# LINE 4649 "src-ag/ExecutionPlan2Hs.hs" #-}
                             )
                        -- copy rule (down)
                        _tlOruleuses =
                            ({-# LINE 504 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIruleuses
                             {-# LINE 4655 "src-ag/ExecutionPlan2Hs.hs" #-}
                             )
                        -- copy rule (down)
                        _tlOsynmap =
                            ({-# LINE 195 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIsynmap
                             {-# LINE 4661 "src-ag/ExecutionPlan2Hs.hs" #-}
                             )
                        -- copy rule (down)
                        _tlOterminaldefs =
                            ({-# LINE 482 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIterminaldefs
                             {-# LINE 4667 "src-ag/ExecutionPlan2Hs.hs" #-}
                             )
                        ( _hdIallvisits,_hdIchildvisit,_hdIintramap,_hdIsem_visit,_hdIt_visits,_hdIusedrules,_hdIvisitdefs,_hdIvisituses) =
                            hd_ _hdOallchildvisit _hdOallintramap _hdOavisitdefs _hdOavisituses _hdOchildintros _hdOinhmap _hdOmrules _hdOnt _hdOoptions _hdOruledefs _hdOruleuses _hdOsynmap _hdOterminaldefs 
                        ( _tlIallvisits,_tlIchildvisit,_tlIintramap,_tlIsem_visit,_tlIt_visits,_tlIusedrules,_tlIvisitdefs,_tlIvisituses) =
                            tl_ _tlOallchildvisit _tlOallintramap _tlOavisitdefs _tlOavisituses _tlOchildintros _tlOinhmap _tlOmrules _tlOnt _tlOoptions _tlOruledefs _tlOruleuses _tlOsynmap _tlOterminaldefs 
                    in  ( _lhsOallvisits,_lhsOchildvisit,_lhsOintramap,_lhsOsem_visit,_lhsOt_visits,_lhsOusedrules,_lhsOvisitdefs,_lhsOvisituses))) )
sem_Visits_Nil :: T_Visits 
sem_Visits_Nil  =
    (T_Visits (\ _lhsIallchildvisit
                 _lhsIallintramap
                 _lhsIavisitdefs
                 _lhsIavisituses
                 _lhsIchildintros
                 _lhsIinhmap
                 _lhsImrules
                 _lhsInt
                 _lhsIoptions
                 _lhsIruledefs
                 _lhsIruleuses
                 _lhsIsynmap
                 _lhsIterminaldefs ->
                   (let _lhsOallvisits :: ([VisitStateState])
                        _lhsOchildvisit :: (Map.Map VisitIdentifier (Identifier -> PP_Doc))
                        _lhsOintramap :: (Map.Map StateIdentifier (Set.Set String))
                        _lhsOsem_visit :: ( [(StateIdentifier,PP_Doc)] )
                        _lhsOt_visits :: PP_Doc
                        _lhsOusedrules :: (Set.Set Identifier)
                        _lhsOvisitdefs :: (Map.Map VisitIdentifier (Set.Set Identifier))
                        _lhsOvisituses :: (Map.Map VisitIdentifier (Set.Set Identifier))
                        -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 207, column 29)
                        _lhsOallvisits =
                            ({-# LINE 207 "src-ag/ExecutionPlan2Hs.ag" #-}
                             []
                             {-# LINE 4701 "src-ag/ExecutionPlan2Hs.hs" #-}
                             )
                        -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 458, column 37)
                        _lhsOchildvisit =
                            ({-# LINE 458 "src-ag/ExecutionPlan2Hs.ag" #-}
                             Map.empty
                             {-# LINE 4707 "src-ag/ExecutionPlan2Hs.hs" #-}
                             )
                        -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 480, column 34)
                        _lhsOintramap =
                            ({-# LINE 480 "src-ag/ExecutionPlan2Hs.ag" #-}
                             Map.empty
                             {-# LINE 4713 "src-ag/ExecutionPlan2Hs.hs" #-}
                             )
                        -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 361, column 29)
                        _lhsOsem_visit =
                            ({-# LINE 361 "src-ag/ExecutionPlan2Hs.ag" #-}
                             []
                             {-# LINE 4719 "src-ag/ExecutionPlan2Hs.hs" #-}
                             )
                        -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 253, column 54)
                        _lhsOt_visits =
                            ({-# LINE 253 "src-ag/ExecutionPlan2Hs.ag" #-}
                             empty
                             {-# LINE 4725 "src-ag/ExecutionPlan2Hs.hs" #-}
                             )
                        -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 396, column 56)
                        _lhsOusedrules =
                            ({-# LINE 396 "src-ag/ExecutionPlan2Hs.ag" #-}
                             Set.empty
                             {-# LINE 4731 "src-ag/ExecutionPlan2Hs.hs" #-}
                             )
                        -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 518, column 36)
                        _lhsOvisitdefs =
                            ({-# LINE 518 "src-ag/ExecutionPlan2Hs.ag" #-}
                             Map.empty
                             {-# LINE 4737 "src-ag/ExecutionPlan2Hs.hs" #-}
                             )
                        -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 519, column 36)
                        _lhsOvisituses =
                            ({-# LINE 519 "src-ag/ExecutionPlan2Hs.ag" #-}
                             Map.empty
                             {-# LINE 4743 "src-ag/ExecutionPlan2Hs.hs" #-}
                             )
                    in  ( _lhsOallvisits,_lhsOchildvisit,_lhsOintramap,_lhsOsem_visit,_lhsOt_visits,_lhsOusedrules,_lhsOvisitdefs,_lhsOvisituses))) )