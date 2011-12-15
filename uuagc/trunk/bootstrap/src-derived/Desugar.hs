{-# OPTIONS_GHC -XBangPatterns #-}

-- UUAGC 0.9.40.1 (src-ag/Desugar.ag)
module Desugar where
{-# LINE 14 "src-ag/Desugar.ag" #-}

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
{-# LINE 24 "dist/build/Desugar.hs" #-}

{-# LINE 2 "src-ag/AbstractSyntax.ag" #-}

-- AbstractSyntax.ag imports
import Data.Set(Set)
import Data.Map(Map)
import Patterns    (Pattern(..),Patterns)
import Expression  (Expression(..))
import Macro --marcos
import CommonTypes
import ErrorMessages
{-# LINE 36 "dist/build/Desugar.hs" #-}

{-# LINE 2 "src-ag/Patterns.ag" #-}

-- Patterns.ag imports
import UU.Scanner.Position(Pos)
import CommonTypes (ConstructorIdent,Identifier)
{-# LINE 43 "dist/build/Desugar.hs" #-}

{-# LINE 2 "src-ag/Expression.ag" #-}

import UU.Scanner.Position(Pos)
import HsToken
{-# LINE 49 "dist/build/Desugar.hs" #-}

{-# LINE 2 "src-ag/HsToken.ag" #-}

import CommonTypes
import UU.Scanner.Position(Pos)
{-# LINE 55 "dist/build/Desugar.hs" #-}
{-# LINE 98 "src-ag/Desugar.ag" #-}

addl :: Int -> Pos -> Pos
addl n (Pos l c f) = Pos (l+n) c f
{-# LINE 60 "dist/build/Desugar.hs" #-}

{-# LINE 133 "src-ag/Desugar.ag" #-}

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
{-# LINE 75 "dist/build/Desugar.hs" #-}

{-# LINE 204 "src-ag/Desugar.ag" #-}

mergeAttributes :: AttrMap -> AttrMap -> AttrMap
mergeAttributes = Map.unionWith $ Map.unionWith $ Set.union
{-# LINE 81 "dist/build/Desugar.hs" #-}

{-# LINE 251 "src-ag/Desugar.ag" #-}

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
{-# LINE 107 "dist/build/Desugar.hs" #-}

{-# LINE 294 "src-ag/Desugar.ag" #-}

addLateAttr :: Options -> String -> Attributes
addLateAttr options mainName
  | kennedyWarren options && lateHigherOrderBinding options =
      let tp = lateBindingType mainName
      in Map.singleton idLateBindingAttr tp
  | otherwise = Map.empty
{-# LINE 117 "dist/build/Desugar.hs" #-}
-- Child -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         inhMap               : Map Identifier Attributes
         mainName             : String
         options              : Options
         synMap               : Map Identifier Attributes
      synthesized attributes:
         childInhs            : [(Identifier, Identifier)]
         childSyns            : [(Identifier, Identifier)]
         output               : Child 
   alternatives:
      alternative Child:
         child name           : {Identifier}
         child tp             : {Type}
         child kind           : {ChildKind}
         visit 0:
            local chnt        : _
            local inh         : _
            local syn         : _
-}
-- cata
sem_Child :: Child ->
             T_Child
sem_Child !(Child _name _tp _kind) =
    (sem_Child_Child _name _tp _kind)
-- semantic domain
newtype T_Child = T_Child ((Map Identifier Attributes) ->
                           String ->
                           Options ->
                           (Map Identifier Attributes) ->
                           ( ([(Identifier, Identifier)]),([(Identifier, Identifier)]),Child))
data Inh_Child = Inh_Child {inhMap_Inh_Child :: !((Map Identifier Attributes)),mainName_Inh_Child :: !(String),options_Inh_Child :: !(Options),synMap_Inh_Child :: !((Map Identifier Attributes))}
data Syn_Child = Syn_Child {childInhs_Syn_Child :: !(([(Identifier, Identifier)])),childSyns_Syn_Child :: !(([(Identifier, Identifier)])),output_Syn_Child :: !(Child)}
wrap_Child :: T_Child ->
              Inh_Child ->
              Syn_Child
wrap_Child !(T_Child sem) !(Inh_Child _lhsIinhMap _lhsImainName _lhsIoptions _lhsIsynMap) =
    (let ( !_lhsOchildInhs,!_lhsOchildSyns,!_lhsOoutput) = sem _lhsIinhMap _lhsImainName _lhsIoptions _lhsIsynMap
     in  (Syn_Child _lhsOchildInhs _lhsOchildSyns _lhsOoutput))
sem_Child_Child :: Identifier ->
                   Type ->
                   ChildKind ->
                   T_Child
sem_Child_Child !name_ !tp_ !kind_ =
    (T_Child (\ (!_lhsIinhMap)
                (!_lhsImainName)
                (!_lhsIoptions)
                (!_lhsIsynMap) ->
                  (case (({-# LINE 19 "src-ag/DistChildAttr.ag" #-}
                          case tp_ of
                            NT nt _ _ -> nt
                            Self      -> error ("The type of child " ++ show name_ ++ " should not be a Self type.")
                            Haskell t -> identifier ""
                          {-# LINE 173 "dist/build/Desugar" #-}
                          )) of
                   { !_chnt ->
                   (case (({-# LINE 23 "src-ag/DistChildAttr.ag" #-}
                           Map.findWithDefault Map.empty _chnt     _lhsIinhMap
                           {-# LINE 178 "dist/build/Desugar" #-}
                           )) of
                    { !_inh ->
                    (case (({-# LINE 130 "src-ag/Desugar.ag" #-}
                            [(i, name_) | i <- Map.keys _inh     ]
                            {-# LINE 183 "dist/build/Desugar" #-}
                            )) of
                     { !_lhsOchildInhs ->
                     (case (({-# LINE 24 "src-ag/DistChildAttr.ag" #-}
                             Map.findWithDefault Map.empty _chnt     _lhsIsynMap
                             {-# LINE 188 "dist/build/Desugar" #-}
                             )) of
                      { !_syn ->
                      (case (({-# LINE 131 "src-ag/Desugar.ag" #-}
                              [(s, name_) | s <- Map.keys _syn     ]
                              {-# LINE 193 "dist/build/Desugar" #-}
                              )) of
                       { !_lhsOchildSyns ->
                       (case (({-# LINE 315 "src-ag/Desugar.ag" #-}
                               Child name_ tp_ kind_
                               {-# LINE 198 "dist/build/Desugar" #-}
                               )) of
                        { !_lhsOoutput ->
                        (case ((Syn_Child _lhsOchildInhs _lhsOchildSyns _lhsOoutput)) of
                         { ___node ->
                         ( _lhsOchildInhs,_lhsOchildSyns,_lhsOoutput) }) }) }) }) }) }) })))
-- Children ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         inhMap               : Map Identifier Attributes
         mainName             : String
         options              : Options
         synMap               : Map Identifier Attributes
      synthesized attributes:
         childInhs            : [(Identifier, Identifier)]
         childSyns            : [(Identifier, Identifier)]
         output               : Children 
   alternatives:
      alternative Cons:
         child hd             : Child 
         child tl             : Children 
         visit 0:
            local output      : _
      alternative Nil:
         visit 0:
            local output      : _
-}
-- cata
sem_Children :: Children ->
                T_Children
sem_Children !list =
    (Prelude.foldr sem_Children_Cons sem_Children_Nil (Prelude.map sem_Child list))
-- semantic domain
newtype T_Children = T_Children ((Map Identifier Attributes) ->
                                 String ->
                                 Options ->
                                 (Map Identifier Attributes) ->
                                 ( ([(Identifier, Identifier)]),([(Identifier, Identifier)]),Children))
data Inh_Children = Inh_Children {inhMap_Inh_Children :: !((Map Identifier Attributes)),mainName_Inh_Children :: !(String),options_Inh_Children :: !(Options),synMap_Inh_Children :: !((Map Identifier Attributes))}
data Syn_Children = Syn_Children {childInhs_Syn_Children :: !(([(Identifier, Identifier)])),childSyns_Syn_Children :: !(([(Identifier, Identifier)])),output_Syn_Children :: !(Children)}
wrap_Children :: T_Children ->
                 Inh_Children ->
                 Syn_Children
wrap_Children !(T_Children sem) !(Inh_Children _lhsIinhMap _lhsImainName _lhsIoptions _lhsIsynMap) =
    (let ( !_lhsOchildInhs,!_lhsOchildSyns,!_lhsOoutput) = sem _lhsIinhMap _lhsImainName _lhsIoptions _lhsIsynMap
     in  (Syn_Children _lhsOchildInhs _lhsOchildSyns _lhsOoutput))
sem_Children_Cons :: T_Child ->
                     T_Children ->
                     T_Children
sem_Children_Cons !(T_Child hd_) !(T_Children tl_) =
    (T_Children (\ (!_lhsIinhMap)
                   (!_lhsImainName)
                   (!_lhsIoptions)
                   (!_lhsIsynMap) ->
                     (case (({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                             _lhsIinhMap
                             {-# LINE 255 "dist/build/Desugar" #-}
                             )) of
                      { !_tlOinhMap ->
                      (case (({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                              _lhsIinhMap
                              {-# LINE 260 "dist/build/Desugar" #-}
                              )) of
                       { !_hdOinhMap ->
                       (case (({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                               _lhsIsynMap
                               {-# LINE 265 "dist/build/Desugar" #-}
                               )) of
                        { !_tlOsynMap ->
                        (case (({-# LINE 36 "src-ag/Desugar.ag" #-}
                                _lhsIoptions
                                {-# LINE 270 "dist/build/Desugar" #-}
                                )) of
                         { !_tlOoptions ->
                         (case (({-# LINE 289 "src-ag/Desugar.ag" #-}
                                 _lhsImainName
                                 {-# LINE 275 "dist/build/Desugar" #-}
                                 )) of
                          { !_tlOmainName ->
                          (case (tl_ _tlOinhMap _tlOmainName _tlOoptions _tlOsynMap) of
                           { ( !_tlIchildInhs,!_tlIchildSyns,!_tlIoutput) ->
                               (case (({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                       _lhsIsynMap
                                       {-# LINE 282 "dist/build/Desugar" #-}
                                       )) of
                                { !_hdOsynMap ->
                                (case (({-# LINE 36 "src-ag/Desugar.ag" #-}
                                        _lhsIoptions
                                        {-# LINE 287 "dist/build/Desugar" #-}
                                        )) of
                                 { !_hdOoptions ->
                                 (case (({-# LINE 289 "src-ag/Desugar.ag" #-}
                                         _lhsImainName
                                         {-# LINE 292 "dist/build/Desugar" #-}
                                         )) of
                                  { !_hdOmainName ->
                                  (case (hd_ _hdOinhMap _hdOmainName _hdOoptions _hdOsynMap) of
                                   { ( !_hdIchildInhs,!_hdIchildSyns,!_hdIoutput) ->
                                       (case (({-# LINE 125 "src-ag/Desugar.ag" #-}
                                               _hdIchildInhs ++ _tlIchildInhs
                                               {-# LINE 299 "dist/build/Desugar" #-}
                                               )) of
                                        { !_lhsOchildInhs ->
                                        (case (({-# LINE 125 "src-ag/Desugar.ag" #-}
                                                _hdIchildSyns ++ _tlIchildSyns
                                                {-# LINE 304 "dist/build/Desugar" #-}
                                                )) of
                                         { !_lhsOchildSyns ->
                                         (case (({-# LINE 40 "src-ag/Desugar.ag" #-}
                                                 (:) _hdIoutput _tlIoutput
                                                 {-# LINE 309 "dist/build/Desugar" #-}
                                                 )) of
                                          { !_output ->
                                          (case (({-# LINE 40 "src-ag/Desugar.ag" #-}
                                                  _output
                                                  {-# LINE 314 "dist/build/Desugar" #-}
                                                  )) of
                                           { !_lhsOoutput ->
                                           (case ((Syn_Children _lhsOchildInhs _lhsOchildSyns _lhsOoutput)) of
                                            { ___node ->
                                            ( _lhsOchildInhs,_lhsOchildSyns,_lhsOoutput) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })))
sem_Children_Nil :: T_Children
sem_Children_Nil =
    (T_Children (\ (!_lhsIinhMap)
                   (!_lhsImainName)
                   (!_lhsIoptions)
                   (!_lhsIsynMap) ->
                     (case (({-# LINE 125 "src-ag/Desugar.ag" #-}
                             []
                             {-# LINE 328 "dist/build/Desugar" #-}
                             )) of
                      { !_lhsOchildInhs ->
                      (case (({-# LINE 125 "src-ag/Desugar.ag" #-}
                              []
                              {-# LINE 333 "dist/build/Desugar" #-}
                              )) of
                       { !_lhsOchildSyns ->
                       (case (({-# LINE 40 "src-ag/Desugar.ag" #-}
                               []
                               {-# LINE 338 "dist/build/Desugar" #-}
                               )) of
                        { !_output ->
                        (case (({-# LINE 40 "src-ag/Desugar.ag" #-}
                                _output
                                {-# LINE 343 "dist/build/Desugar" #-}
                                )) of
                         { !_lhsOoutput ->
                         (case ((Syn_Children _lhsOchildInhs _lhsOchildSyns _lhsOoutput)) of
                          { ___node ->
                          ( _lhsOchildInhs,_lhsOchildSyns,_lhsOoutput) }) }) }) }) })))
-- Expression --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         childInhs            : [(Identifier, Identifier)]
         childSyns            : [(Identifier, Identifier)]
         con                  : ConstructorIdent
         nt                   : NontermIdent
         options              : Options
         ruleDescr            : String
      synthesized attributes:
         errors               : Seq Error
         output               : Expression 
   alternatives:
      alternative Expression:
         child pos            : {Pos}
         child tks            : {[HsToken]}
         visit 0:
            local _tup1       : _
            local tks'        : _
-}
-- cata
sem_Expression :: Expression ->
                  T_Expression
sem_Expression !(Expression _pos _tks) =
    (sem_Expression_Expression _pos _tks)
-- semantic domain
newtype T_Expression = T_Expression (([(Identifier, Identifier)]) ->
                                     ([(Identifier, Identifier)]) ->
                                     ConstructorIdent ->
                                     NontermIdent ->
                                     Options ->
                                     String ->
                                     ( (Seq Error),Expression))
data Inh_Expression = Inh_Expression {childInhs_Inh_Expression :: !(([(Identifier, Identifier)])),childSyns_Inh_Expression :: !(([(Identifier, Identifier)])),con_Inh_Expression :: !(ConstructorIdent),nt_Inh_Expression :: !(NontermIdent),options_Inh_Expression :: !(Options),ruleDescr_Inh_Expression :: !(String)}
data Syn_Expression = Syn_Expression {errors_Syn_Expression :: !((Seq Error)),output_Syn_Expression :: !(Expression)}
wrap_Expression :: T_Expression ->
                   Inh_Expression ->
                   Syn_Expression
wrap_Expression !(T_Expression sem) !(Inh_Expression _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsInt _lhsIoptions _lhsIruleDescr) =
    (let ( !_lhsOerrors,!_lhsOoutput) = sem _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsInt _lhsIoptions _lhsIruleDescr
     in  (Syn_Expression _lhsOerrors _lhsOoutput))
sem_Expression_Expression :: Pos ->
                             ([HsToken]) ->
                             T_Expression
sem_Expression_Expression !pos_ !tks_ =
    (T_Expression (\ (!_lhsIchildInhs)
                     (!_lhsIchildSyns)
                     (!_lhsIcon)
                     (!_lhsInt)
                     (!_lhsIoptions)
                     (!_lhsIruleDescr) ->
                       (case (({-# LINE 49 "src-ag/Desugar.ag" #-}
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
                               {-# LINE 412 "dist/build/Desugar" #-}
                               )) of
                        { !__tup1 ->
                        (case (({-# LINE 49 "src-ag/Desugar.ag" #-}
                                __tup1
                                {-# LINE 417 "dist/build/Desugar" #-}
                                )) of
                         { !(_,!_lhsOerrors) ->
                         (case (({-# LINE 49 "src-ag/Desugar.ag" #-}
                                 __tup1
                                 {-# LINE 422 "dist/build/Desugar" #-}
                                 )) of
                          { !(!_tks',_) ->
                          (case (({-# LINE 59 "src-ag/Desugar.ag" #-}
                                  Expression pos_ _tks'
                                  {-# LINE 427 "dist/build/Desugar" #-}
                                  )) of
                           { !_lhsOoutput ->
                           (case ((Syn_Expression _lhsOerrors _lhsOoutput)) of
                            { ___node ->
                            ( _lhsOerrors,_lhsOoutput) }) }) }) }) })))
-- Grammar -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         forcedIrrefutables   : AttrMap
         mainName             : String
         options              : Options
      synthesized attributes:
         allAttributes        : AttrMap
         errors               : Seq Error
         output               : Grammar 
   alternatives:
      alternative Grammar:
         child typeSyns       : {TypeSyns}
         child useMap         : {UseMap}
         child derivings      : {Derivings}
         child wrappers       : {Set NontermIdent}
         child nonts          : Nonterminals 
         child pragmas        : {PragmaMap}
         child manualAttrOrderMap : {AttrOrderMap}
         child paramMap       : {ParamMap}
         child contextMap     : {ContextMap}
         child quantMap       : {QuantMap}
         child uniqueMap      : {UniqueMap}
         child augmentsMap    : {Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))}
         child aroundsMap     : {Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))}
         child mergeMap       : {Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier, [Identifier], Expression)))}
-}
-- cata
sem_Grammar :: Grammar ->
               T_Grammar
sem_Grammar !(Grammar _typeSyns _useMap _derivings _wrappers _nonts _pragmas _manualAttrOrderMap _paramMap _contextMap _quantMap _uniqueMap _augmentsMap _aroundsMap _mergeMap) =
    (sem_Grammar_Grammar _typeSyns _useMap _derivings _wrappers (sem_Nonterminals _nonts) _pragmas _manualAttrOrderMap _paramMap _contextMap _quantMap _uniqueMap _augmentsMap _aroundsMap _mergeMap)
-- semantic domain
newtype T_Grammar = T_Grammar (AttrMap ->
                               String ->
                               Options ->
                               ( AttrMap,(Seq Error),Grammar))
data Inh_Grammar = Inh_Grammar {forcedIrrefutables_Inh_Grammar :: !(AttrMap),mainName_Inh_Grammar :: !(String),options_Inh_Grammar :: !(Options)}
data Syn_Grammar = Syn_Grammar {allAttributes_Syn_Grammar :: !(AttrMap),errors_Syn_Grammar :: !((Seq Error)),output_Syn_Grammar :: !(Grammar)}
wrap_Grammar :: T_Grammar ->
                Inh_Grammar ->
                Syn_Grammar
wrap_Grammar !(T_Grammar sem) !(Inh_Grammar _lhsIforcedIrrefutables _lhsImainName _lhsIoptions) =
    (let ( !_lhsOallAttributes,!_lhsOerrors,!_lhsOoutput) = sem _lhsIforcedIrrefutables _lhsImainName _lhsIoptions
     in  (Syn_Grammar _lhsOallAttributes _lhsOerrors _lhsOoutput))
sem_Grammar_Grammar :: TypeSyns ->
                       UseMap ->
                       Derivings ->
                       (Set NontermIdent) ->
                       T_Nonterminals ->
                       PragmaMap ->
                       AttrOrderMap ->
                       ParamMap ->
                       ContextMap ->
                       QuantMap ->
                       UniqueMap ->
                       (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) ->
                       (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) ->
                       (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier, [Identifier], Expression)))) ->
                       T_Grammar
sem_Grammar_Grammar !typeSyns_ !useMap_ !derivings_ !wrappers_ !(T_Nonterminals nonts_) !pragmas_ !manualAttrOrderMap_ !paramMap_ !contextMap_ !quantMap_ !uniqueMap_ !augmentsMap_ !aroundsMap_ !mergeMap_ =
    (T_Grammar (\ (!_lhsIforcedIrrefutables)
                  (!_lhsImainName)
                  (!_lhsIoptions) ->
                    (case (nonts_) of
                     { ( !_nontsIinhMap',!_nontsIsynMap',!T_Nonterminals_1 nonts_1) ->
                         (case (({-# LINE 36 "src-ag/Desugar.ag" #-}
                                 _lhsIoptions
                                 {-# LINE 502 "dist/build/Desugar" #-}
                                 )) of
                          { !_nontsOoptions ->
                          (case (({-# LINE 289 "src-ag/Desugar.ag" #-}
                                  _lhsImainName
                                  {-# LINE 507 "dist/build/Desugar" #-}
                                  )) of
                           { !_nontsOmainName ->
                           (case (({-# LINE 215 "src-ag/Desugar.ag" #-}
                                   _lhsIforcedIrrefutables
                                   {-# LINE 512 "dist/build/Desugar" #-}
                                   )) of
                            { !_nontsOforcedIrrefutables ->
                            (case (({-# LINE 16 "src-ag/DistChildAttr.ag" #-}
                                    _nontsIsynMap'
                                    {-# LINE 517 "dist/build/Desugar" #-}
                                    )) of
                             { !_nontsOsynMap ->
                             (case (({-# LINE 15 "src-ag/DistChildAttr.ag" #-}
                                     _nontsIinhMap'
                                     {-# LINE 522 "dist/build/Desugar" #-}
                                     )) of
                              { !_nontsOinhMap ->
                              (case (({-# LINE 235 "src-ag/Desugar.ag" #-}
                                      augmentsMap_
                                      {-# LINE 527 "dist/build/Desugar" #-}
                                      )) of
                               { !_nontsOaugmentsIn ->
                               (case (nonts_1 _nontsOaugmentsIn _nontsOforcedIrrefutables _nontsOinhMap _nontsOmainName _nontsOoptions _nontsOsynMap) of
                                { ( !_nontsIallAttributes,!_nontsIaugmentsOut,!_nontsIerrors,!_nontsIoutput) ->
                                    (case (({-# LINE 196 "src-ag/Desugar.ag" #-}
                                            _nontsIallAttributes
                                            {-# LINE 534 "dist/build/Desugar" #-}
                                            )) of
                                     { !_lhsOallAttributes ->
                                     (case (({-# LINE 38 "src-ag/Desugar.ag" #-}
                                             _nontsIerrors
                                             {-# LINE 539 "dist/build/Desugar" #-}
                                             )) of
                                      { !_lhsOerrors ->
                                      (case (({-# LINE 319 "src-ag/Desugar.ag" #-}
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
                                              {-# LINE 557 "dist/build/Desugar" #-}
                                              )) of
                                       { !_lhsOoutput ->
                                       (case ((Syn_Grammar _lhsOallAttributes _lhsOerrors _lhsOoutput)) of
                                        { ___node ->
                                        ( _lhsOallAttributes,_lhsOerrors,_lhsOoutput) }) }) }) }) }) }) }) }) }) }) }) })))
-- HsToken -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         childInhs            : [(Identifier, Identifier)]
         childSyns            : [(Identifier, Identifier)]
         con                  : ConstructorIdent
         nt                   : NontermIdent
         ruleDescr            : String
         useFieldIdent        : Bool
      chained attribute:
         addLines             : Int
      synthesized attributes:
         errors               : Seq Error
         tks                  : HsToken 
   alternatives:
      alternative AGField:
         child field          : {Identifier}
         child attr           : {Identifier}
         child pos            : {Pos}
         child rdesc          : {Maybe String}
         visit 0:
            local mField      : _
            local field'      : _
            local tks         : _
      alternative AGLocal:
         child var            : {Identifier}
         child pos            : {Pos}
         child rdesc          : {Maybe String}
         visit 0:
            local tks         : _
      alternative CharToken:
         child value          : {String}
         child pos            : {Pos}
         visit 0:
            local tks         : _
      alternative Err:
         child mesg           : {String}
         child pos            : {Pos}
         visit 0:
            local tks         : _
      alternative HsToken:
         child value          : {String}
         child pos            : {Pos}
         visit 0:
            local tks         : _
      alternative StrToken:
         child value          : {String}
         child pos            : {Pos}
         visit 0:
            local tks         : _
-}
-- cata
sem_HsToken :: HsToken ->
               T_HsToken
sem_HsToken !(AGField _field _attr _pos _rdesc) =
    (sem_HsToken_AGField _field _attr _pos _rdesc)
sem_HsToken !(AGLocal _var _pos _rdesc) =
    (sem_HsToken_AGLocal _var _pos _rdesc)
sem_HsToken !(CharToken _value _pos) =
    (sem_HsToken_CharToken _value _pos)
sem_HsToken !(Err _mesg _pos) =
    (sem_HsToken_Err _mesg _pos)
sem_HsToken !(HsToken _value _pos) =
    (sem_HsToken_HsToken _value _pos)
sem_HsToken !(StrToken _value _pos) =
    (sem_HsToken_StrToken _value _pos)
-- semantic domain
newtype T_HsToken = T_HsToken (Int ->
                               ([(Identifier, Identifier)]) ->
                               ([(Identifier, Identifier)]) ->
                               ConstructorIdent ->
                               NontermIdent ->
                               String ->
                               Bool ->
                               ( Int,(Seq Error),HsToken))
data Inh_HsToken = Inh_HsToken {addLines_Inh_HsToken :: !(Int),childInhs_Inh_HsToken :: !(([(Identifier, Identifier)])),childSyns_Inh_HsToken :: !(([(Identifier, Identifier)])),con_Inh_HsToken :: !(ConstructorIdent),nt_Inh_HsToken :: !(NontermIdent),ruleDescr_Inh_HsToken :: !(String),useFieldIdent_Inh_HsToken :: !(Bool)}
data Syn_HsToken = Syn_HsToken {addLines_Syn_HsToken :: !(Int),errors_Syn_HsToken :: !((Seq Error)),tks_Syn_HsToken :: !(HsToken)}
wrap_HsToken :: T_HsToken ->
                Inh_HsToken ->
                Syn_HsToken
wrap_HsToken !(T_HsToken sem) !(Inh_HsToken _lhsIaddLines _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsInt _lhsIruleDescr _lhsIuseFieldIdent) =
    (let ( !_lhsOaddLines,!_lhsOerrors,!_lhsOtks) = sem _lhsIaddLines _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsInt _lhsIruleDescr _lhsIuseFieldIdent
     in  (Syn_HsToken _lhsOaddLines _lhsOerrors _lhsOtks))
sem_HsToken_AGField :: Identifier ->
                       Identifier ->
                       Pos ->
                       (Maybe String) ->
                       T_HsToken
sem_HsToken_AGField !field_ !attr_ !pos_ !rdesc_ =
    (T_HsToken (\ (!_lhsIaddLines)
                  (!_lhsIchildInhs)
                  (!_lhsIchildSyns)
                  (!_lhsIcon)
                  (!_lhsInt)
                  (!_lhsIruleDescr)
                  (!_lhsIuseFieldIdent) ->
                    (case (({-# LINE 79 "src-ag/Desugar.ag" #-}
                            findField field_ attr_ _lhsIchildSyns
                            {-# LINE 662 "dist/build/Desugar" #-}
                            )) of
                     { !_mField ->
                     (case (({-# LINE 81 "src-ag/Desugar.ag" #-}
                             maybe field_ id _mField
                             {-# LINE 667 "dist/build/Desugar" #-}
                             )) of
                      { !_field' ->
                      (case (({-# LINE 84 "src-ag/Desugar.ag" #-}
                              if _lhsIuseFieldIdent || length (getName field_) < length (getName _field'    )
                              then _lhsIaddLines + 1
                              else _lhsIaddLines
                              {-# LINE 674 "dist/build/Desugar" #-}
                              )) of
                       { !_lhsOaddLines ->
                       (case (({-# LINE 82 "src-ag/Desugar.ag" #-}
                               maybe (Seq.singleton (UndefAttr _lhsInt _lhsIcon field_ (Ident "<ANY>" (getPos field_)) False)) (const Seq.empty) _mField
                               {-# LINE 679 "dist/build/Desugar" #-}
                               )) of
                        { !_lhsOerrors ->
                        (case (({-# LINE 88 "src-ag/Desugar.ag" #-}
                                AGField _field'     attr_ (addl _lhsIaddLines pos_) (if _lhsIuseFieldIdent then Just _lhsIruleDescr else Nothing)
                                {-# LINE 684 "dist/build/Desugar" #-}
                                )) of
                         { !_tks ->
                         (case (({-# LINE 70 "src-ag/Desugar.ag" #-}
                                 _tks
                                 {-# LINE 689 "dist/build/Desugar" #-}
                                 )) of
                          { !_lhsOtks ->
                          (case ((Syn_HsToken _lhsOaddLines _lhsOerrors _lhsOtks)) of
                           { ___node ->
                           ( _lhsOaddLines,_lhsOerrors,_lhsOtks) }) }) }) }) }) }) })))
sem_HsToken_AGLocal :: Identifier ->
                       Pos ->
                       (Maybe String) ->
                       T_HsToken
sem_HsToken_AGLocal !var_ !pos_ !rdesc_ =
    (T_HsToken (\ (!_lhsIaddLines)
                  (!_lhsIchildInhs)
                  (!_lhsIchildSyns)
                  (!_lhsIcon)
                  (!_lhsInt)
                  (!_lhsIruleDescr)
                  (!_lhsIuseFieldIdent) ->
                    (case (({-# LINE 74 "src-ag/Desugar.ag" #-}
                            if _lhsIuseFieldIdent
                            then _lhsIaddLines + 1
                            else _lhsIaddLines
                            {-# LINE 711 "dist/build/Desugar" #-}
                            )) of
                     { !_lhsOaddLines ->
                     (case (({-# LINE 38 "src-ag/Desugar.ag" #-}
                             Seq.empty
                             {-# LINE 716 "dist/build/Desugar" #-}
                             )) of
                      { !_lhsOerrors ->
                      (case (({-# LINE 77 "src-ag/Desugar.ag" #-}
                              AGLocal var_ (addl _lhsIaddLines pos_) (if _lhsIuseFieldIdent then Just _lhsIruleDescr else Nothing)
                              {-# LINE 721 "dist/build/Desugar" #-}
                              )) of
                       { !_tks ->
                       (case (({-# LINE 70 "src-ag/Desugar.ag" #-}
                               _tks
                               {-# LINE 726 "dist/build/Desugar" #-}
                               )) of
                        { !_lhsOtks ->
                        (case ((Syn_HsToken _lhsOaddLines _lhsOerrors _lhsOtks)) of
                         { ___node ->
                         ( _lhsOaddLines,_lhsOerrors,_lhsOtks) }) }) }) }) })))
sem_HsToken_CharToken :: String ->
                         Pos ->
                         T_HsToken
sem_HsToken_CharToken !value_ !pos_ =
    (T_HsToken (\ (!_lhsIaddLines)
                  (!_lhsIchildInhs)
                  (!_lhsIchildSyns)
                  (!_lhsIcon)
                  (!_lhsInt)
                  (!_lhsIruleDescr)
                  (!_lhsIuseFieldIdent) ->
                    (case (({-# LINE 64 "src-ag/Desugar.ag" #-}
                            _lhsIaddLines
                            {-# LINE 745 "dist/build/Desugar" #-}
                            )) of
                     { !_lhsOaddLines ->
                     (case (({-# LINE 38 "src-ag/Desugar.ag" #-}
                             Seq.empty
                             {-# LINE 750 "dist/build/Desugar" #-}
                             )) of
                      { !_lhsOerrors ->
                      (case (({-# LINE 92 "src-ag/Desugar.ag" #-}
                              CharToken value_ (addl _lhsIaddLines pos_)
                              {-# LINE 755 "dist/build/Desugar" #-}
                              )) of
                       { !_tks ->
                       (case (({-# LINE 70 "src-ag/Desugar.ag" #-}
                               _tks
                               {-# LINE 760 "dist/build/Desugar" #-}
                               )) of
                        { !_lhsOtks ->
                        (case ((Syn_HsToken _lhsOaddLines _lhsOerrors _lhsOtks)) of
                         { ___node ->
                         ( _lhsOaddLines,_lhsOerrors,_lhsOtks) }) }) }) }) })))
sem_HsToken_Err :: String ->
                   Pos ->
                   T_HsToken
sem_HsToken_Err !mesg_ !pos_ =
    (T_HsToken (\ (!_lhsIaddLines)
                  (!_lhsIchildInhs)
                  (!_lhsIchildSyns)
                  (!_lhsIcon)
                  (!_lhsInt)
                  (!_lhsIruleDescr)
                  (!_lhsIuseFieldIdent) ->
                    (case (({-# LINE 64 "src-ag/Desugar.ag" #-}
                            _lhsIaddLines
                            {-# LINE 779 "dist/build/Desugar" #-}
                            )) of
                     { !_lhsOaddLines ->
                     (case (({-# LINE 38 "src-ag/Desugar.ag" #-}
                             Seq.empty
                             {-# LINE 784 "dist/build/Desugar" #-}
                             )) of
                      { !_lhsOerrors ->
                      (case (({-# LINE 96 "src-ag/Desugar.ag" #-}
                              Err mesg_ (addl _lhsIaddLines pos_)
                              {-# LINE 789 "dist/build/Desugar" #-}
                              )) of
                       { !_tks ->
                       (case (({-# LINE 70 "src-ag/Desugar.ag" #-}
                               _tks
                               {-# LINE 794 "dist/build/Desugar" #-}
                               )) of
                        { !_lhsOtks ->
                        (case ((Syn_HsToken _lhsOaddLines _lhsOerrors _lhsOtks)) of
                         { ___node ->
                         ( _lhsOaddLines,_lhsOerrors,_lhsOtks) }) }) }) }) })))
sem_HsToken_HsToken :: String ->
                       Pos ->
                       T_HsToken
sem_HsToken_HsToken !value_ !pos_ =
    (T_HsToken (\ (!_lhsIaddLines)
                  (!_lhsIchildInhs)
                  (!_lhsIchildSyns)
                  (!_lhsIcon)
                  (!_lhsInt)
                  (!_lhsIruleDescr)
                  (!_lhsIuseFieldIdent) ->
                    (case (({-# LINE 64 "src-ag/Desugar.ag" #-}
                            _lhsIaddLines
                            {-# LINE 813 "dist/build/Desugar" #-}
                            )) of
                     { !_lhsOaddLines ->
                     (case (({-# LINE 38 "src-ag/Desugar.ag" #-}
                             Seq.empty
                             {-# LINE 818 "dist/build/Desugar" #-}
                             )) of
                      { !_lhsOerrors ->
                      (case (({-# LINE 90 "src-ag/Desugar.ag" #-}
                              HsToken value_ (addl _lhsIaddLines pos_)
                              {-# LINE 823 "dist/build/Desugar" #-}
                              )) of
                       { !_tks ->
                       (case (({-# LINE 70 "src-ag/Desugar.ag" #-}
                               _tks
                               {-# LINE 828 "dist/build/Desugar" #-}
                               )) of
                        { !_lhsOtks ->
                        (case ((Syn_HsToken _lhsOaddLines _lhsOerrors _lhsOtks)) of
                         { ___node ->
                         ( _lhsOaddLines,_lhsOerrors,_lhsOtks) }) }) }) }) })))
sem_HsToken_StrToken :: String ->
                        Pos ->
                        T_HsToken
sem_HsToken_StrToken !value_ !pos_ =
    (T_HsToken (\ (!_lhsIaddLines)
                  (!_lhsIchildInhs)
                  (!_lhsIchildSyns)
                  (!_lhsIcon)
                  (!_lhsInt)
                  (!_lhsIruleDescr)
                  (!_lhsIuseFieldIdent) ->
                    (case (({-# LINE 64 "src-ag/Desugar.ag" #-}
                            _lhsIaddLines
                            {-# LINE 847 "dist/build/Desugar" #-}
                            )) of
                     { !_lhsOaddLines ->
                     (case (({-# LINE 38 "src-ag/Desugar.ag" #-}
                             Seq.empty
                             {-# LINE 852 "dist/build/Desugar" #-}
                             )) of
                      { !_lhsOerrors ->
                      (case (({-# LINE 94 "src-ag/Desugar.ag" #-}
                              StrToken value_ (addl _lhsIaddLines pos_)
                              {-# LINE 857 "dist/build/Desugar" #-}
                              )) of
                       { !_tks ->
                       (case (({-# LINE 70 "src-ag/Desugar.ag" #-}
                               _tks
                               {-# LINE 862 "dist/build/Desugar" #-}
                               )) of
                        { !_lhsOtks ->
                        (case ((Syn_HsToken _lhsOaddLines _lhsOerrors _lhsOtks)) of
                         { ___node ->
                         ( _lhsOaddLines,_lhsOerrors,_lhsOtks) }) }) }) }) })))
-- HsTokens ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         childInhs            : [(Identifier, Identifier)]
         childSyns            : [(Identifier, Identifier)]
         con                  : ConstructorIdent
         nt                   : NontermIdent
         ruleDescr            : String
         useFieldIdent        : Bool
      chained attribute:
         addLines             : Int
      synthesized attributes:
         errors               : Seq Error
         tks                  : HsTokens 
   alternatives:
      alternative Cons:
         child hd             : HsToken 
         child tl             : HsTokens 
         visit 0:
            local tks         : _
      alternative Nil:
         visit 0:
            local tks         : _
-}
-- cata
sem_HsTokens :: HsTokens ->
                T_HsTokens
sem_HsTokens !list =
    (Prelude.foldr sem_HsTokens_Cons sem_HsTokens_Nil (Prelude.map sem_HsToken list))
-- semantic domain
newtype T_HsTokens = T_HsTokens (Int ->
                                 ([(Identifier, Identifier)]) ->
                                 ([(Identifier, Identifier)]) ->
                                 ConstructorIdent ->
                                 NontermIdent ->
                                 String ->
                                 Bool ->
                                 ( Int,(Seq Error),HsTokens))
data Inh_HsTokens = Inh_HsTokens {addLines_Inh_HsTokens :: !(Int),childInhs_Inh_HsTokens :: !(([(Identifier, Identifier)])),childSyns_Inh_HsTokens :: !(([(Identifier, Identifier)])),con_Inh_HsTokens :: !(ConstructorIdent),nt_Inh_HsTokens :: !(NontermIdent),ruleDescr_Inh_HsTokens :: !(String),useFieldIdent_Inh_HsTokens :: !(Bool)}
data Syn_HsTokens = Syn_HsTokens {addLines_Syn_HsTokens :: !(Int),errors_Syn_HsTokens :: !((Seq Error)),tks_Syn_HsTokens :: !(HsTokens)}
wrap_HsTokens :: T_HsTokens ->
                 Inh_HsTokens ->
                 Syn_HsTokens
wrap_HsTokens !(T_HsTokens sem) !(Inh_HsTokens _lhsIaddLines _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsInt _lhsIruleDescr _lhsIuseFieldIdent) =
    (let ( !_lhsOaddLines,!_lhsOerrors,!_lhsOtks) = sem _lhsIaddLines _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsInt _lhsIruleDescr _lhsIuseFieldIdent
     in  (Syn_HsTokens _lhsOaddLines _lhsOerrors _lhsOtks))
sem_HsTokens_Cons :: T_HsToken ->
                     T_HsTokens ->
                     T_HsTokens
sem_HsTokens_Cons !(T_HsToken hd_) !(T_HsTokens tl_) =
    (T_HsTokens (\ (!_lhsIaddLines)
                   (!_lhsIchildInhs)
                   (!_lhsIchildSyns)
                   (!_lhsIcon)
                   (!_lhsInt)
                   (!_lhsIruleDescr)
                   (!_lhsIuseFieldIdent) ->
                     (case (({-# LINE 62 "src-ag/Desugar.ag" #-}
                             _lhsIuseFieldIdent
                             {-# LINE 928 "dist/build/Desugar" #-}
                             )) of
                      { !_tlOuseFieldIdent ->
                      (case (({-# LINE 126 "src-ag/Desugar.ag" #-}
                              _lhsIchildSyns
                              {-# LINE 933 "dist/build/Desugar" #-}
                              )) of
                       { !_tlOchildSyns ->
                       (case (({-# LINE 62 "src-ag/Desugar.ag" #-}
                               _lhsIuseFieldIdent
                               {-# LINE 938 "dist/build/Desugar" #-}
                               )) of
                        { !_hdOuseFieldIdent ->
                        (case (({-# LINE 126 "src-ag/Desugar.ag" #-}
                                _lhsIchildSyns
                                {-# LINE 943 "dist/build/Desugar" #-}
                                )) of
                         { !_hdOchildSyns ->
                         (case (({-# LINE 64 "src-ag/Desugar.ag" #-}
                                 _lhsIaddLines
                                 {-# LINE 948 "dist/build/Desugar" #-}
                                 )) of
                          { !_hdOaddLines ->
                          (case (({-# LINE 168 "src-ag/Desugar.ag" #-}
                                  _lhsIruleDescr
                                  {-# LINE 953 "dist/build/Desugar" #-}
                                  )) of
                           { !_hdOruleDescr ->
                           (case (({-# LINE 152 "src-ag/Desugar.ag" #-}
                                   _lhsInt
                                   {-# LINE 958 "dist/build/Desugar" #-}
                                   )) of
                            { !_hdOnt ->
                            (case (({-# LINE 153 "src-ag/Desugar.ag" #-}
                                    _lhsIcon
                                    {-# LINE 963 "dist/build/Desugar" #-}
                                    )) of
                             { !_hdOcon ->
                             (case (({-# LINE 126 "src-ag/Desugar.ag" #-}
                                     _lhsIchildInhs
                                     {-# LINE 968 "dist/build/Desugar" #-}
                                     )) of
                              { !_hdOchildInhs ->
                              (case (hd_ _hdOaddLines _hdOchildInhs _hdOchildSyns _hdOcon _hdOnt _hdOruleDescr _hdOuseFieldIdent) of
                               { ( !_hdIaddLines,!_hdIerrors,!_hdItks) ->
                                   (case (({-# LINE 64 "src-ag/Desugar.ag" #-}
                                           _hdIaddLines
                                           {-# LINE 975 "dist/build/Desugar" #-}
                                           )) of
                                    { !_tlOaddLines ->
                                    (case (({-# LINE 168 "src-ag/Desugar.ag" #-}
                                            _lhsIruleDescr
                                            {-# LINE 980 "dist/build/Desugar" #-}
                                            )) of
                                     { !_tlOruleDescr ->
                                     (case (({-# LINE 152 "src-ag/Desugar.ag" #-}
                                             _lhsInt
                                             {-# LINE 985 "dist/build/Desugar" #-}
                                             )) of
                                      { !_tlOnt ->
                                      (case (({-# LINE 153 "src-ag/Desugar.ag" #-}
                                              _lhsIcon
                                              {-# LINE 990 "dist/build/Desugar" #-}
                                              )) of
                                       { !_tlOcon ->
                                       (case (({-# LINE 126 "src-ag/Desugar.ag" #-}
                                               _lhsIchildInhs
                                               {-# LINE 995 "dist/build/Desugar" #-}
                                               )) of
                                        { !_tlOchildInhs ->
                                        (case (tl_ _tlOaddLines _tlOchildInhs _tlOchildSyns _tlOcon _tlOnt _tlOruleDescr _tlOuseFieldIdent) of
                                         { ( !_tlIaddLines,!_tlIerrors,!_tlItks) ->
                                             (case (({-# LINE 64 "src-ag/Desugar.ag" #-}
                                                     _tlIaddLines
                                                     {-# LINE 1002 "dist/build/Desugar" #-}
                                                     )) of
                                              { !_lhsOaddLines ->
                                              (case (({-# LINE 38 "src-ag/Desugar.ag" #-}
                                                      _hdIerrors Seq.>< _tlIerrors
                                                      {-# LINE 1007 "dist/build/Desugar" #-}
                                                      )) of
                                               { !_lhsOerrors ->
                                               (case (({-# LINE 70 "src-ag/Desugar.ag" #-}
                                                       (:) _hdItks _tlItks
                                                       {-# LINE 1012 "dist/build/Desugar" #-}
                                                       )) of
                                                { !_tks ->
                                                (case (({-# LINE 70 "src-ag/Desugar.ag" #-}
                                                        _tks
                                                        {-# LINE 1017 "dist/build/Desugar" #-}
                                                        )) of
                                                 { !_lhsOtks ->
                                                 (case ((Syn_HsTokens _lhsOaddLines _lhsOerrors _lhsOtks)) of
                                                  { ___node ->
                                                  ( _lhsOaddLines,_lhsOerrors,_lhsOtks) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })))
sem_HsTokens_Nil :: T_HsTokens
sem_HsTokens_Nil =
    (T_HsTokens (\ (!_lhsIaddLines)
                   (!_lhsIchildInhs)
                   (!_lhsIchildSyns)
                   (!_lhsIcon)
                   (!_lhsInt)
                   (!_lhsIruleDescr)
                   (!_lhsIuseFieldIdent) ->
                     (case (({-# LINE 64 "src-ag/Desugar.ag" #-}
                             _lhsIaddLines
                             {-# LINE 1034 "dist/build/Desugar" #-}
                             )) of
                      { !_lhsOaddLines ->
                      (case (({-# LINE 38 "src-ag/Desugar.ag" #-}
                              Seq.empty
                              {-# LINE 1039 "dist/build/Desugar" #-}
                              )) of
                       { !_lhsOerrors ->
                       (case (({-# LINE 70 "src-ag/Desugar.ag" #-}
                               []
                               {-# LINE 1044 "dist/build/Desugar" #-}
                               )) of
                        { !_tks ->
                        (case (({-# LINE 70 "src-ag/Desugar.ag" #-}
                                _tks
                                {-# LINE 1049 "dist/build/Desugar" #-}
                                )) of
                         { !_lhsOtks ->
                         (case ((Syn_HsTokens _lhsOaddLines _lhsOerrors _lhsOtks)) of
                          { ___node ->
                          ( _lhsOaddLines,_lhsOerrors,_lhsOtks) }) }) }) }) })))
-- HsTokensRoot ------------------------------------------------
{-
   visit 0:
      inherited attributes:
         childInhs            : [(Identifier, Identifier)]
         childSyns            : [(Identifier, Identifier)]
         con                  : ConstructorIdent
         nt                   : NontermIdent
         ruleDescr            : String
         useFieldIdent        : Bool
      synthesized attributes:
         errors               : Seq Error
         tks                  : [HsToken]
   alternatives:
      alternative HsTokensRoot:
         child tokens         : HsTokens 
-}
-- cata
sem_HsTokensRoot :: HsTokensRoot ->
                    T_HsTokensRoot
sem_HsTokensRoot !(HsTokensRoot _tokens) =
    (sem_HsTokensRoot_HsTokensRoot (sem_HsTokens _tokens))
-- semantic domain
newtype T_HsTokensRoot = T_HsTokensRoot (([(Identifier, Identifier)]) ->
                                         ([(Identifier, Identifier)]) ->
                                         ConstructorIdent ->
                                         NontermIdent ->
                                         String ->
                                         Bool ->
                                         ( (Seq Error),([HsToken])))
data Inh_HsTokensRoot = Inh_HsTokensRoot {childInhs_Inh_HsTokensRoot :: !(([(Identifier, Identifier)])),childSyns_Inh_HsTokensRoot :: !(([(Identifier, Identifier)])),con_Inh_HsTokensRoot :: !(ConstructorIdent),nt_Inh_HsTokensRoot :: !(NontermIdent),ruleDescr_Inh_HsTokensRoot :: !(String),useFieldIdent_Inh_HsTokensRoot :: !(Bool)}
data Syn_HsTokensRoot = Syn_HsTokensRoot {errors_Syn_HsTokensRoot :: !((Seq Error)),tks_Syn_HsTokensRoot :: !(([HsToken]))}
wrap_HsTokensRoot :: T_HsTokensRoot ->
                     Inh_HsTokensRoot ->
                     Syn_HsTokensRoot
wrap_HsTokensRoot !(T_HsTokensRoot sem) !(Inh_HsTokensRoot _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsInt _lhsIruleDescr _lhsIuseFieldIdent) =
    (let ( !_lhsOerrors,!_lhsOtks) = sem _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsInt _lhsIruleDescr _lhsIuseFieldIdent
     in  (Syn_HsTokensRoot _lhsOerrors _lhsOtks))
sem_HsTokensRoot_HsTokensRoot :: T_HsTokens ->
                                 T_HsTokensRoot
sem_HsTokensRoot_HsTokensRoot !(T_HsTokens tokens_) =
    (T_HsTokensRoot (\ (!_lhsIchildInhs)
                       (!_lhsIchildSyns)
                       (!_lhsIcon)
                       (!_lhsInt)
                       (!_lhsIruleDescr)
                       (!_lhsIuseFieldIdent) ->
                         (case (({-# LINE 152 "src-ag/Desugar.ag" #-}
                                 _lhsInt
                                 {-# LINE 1104 "dist/build/Desugar" #-}
                                 )) of
                          { !_tokensOnt ->
                          (case (({-# LINE 153 "src-ag/Desugar.ag" #-}
                                  _lhsIcon
                                  {-# LINE 1109 "dist/build/Desugar" #-}
                                  )) of
                           { !_tokensOcon ->
                           (case (({-# LINE 126 "src-ag/Desugar.ag" #-}
                                   _lhsIchildSyns
                                   {-# LINE 1114 "dist/build/Desugar" #-}
                                   )) of
                            { !_tokensOchildSyns ->
                            (case (({-# LINE 62 "src-ag/Desugar.ag" #-}
                                    _lhsIuseFieldIdent
                                    {-# LINE 1119 "dist/build/Desugar" #-}
                                    )) of
                             { !_tokensOuseFieldIdent ->
                             (case (({-# LINE 168 "src-ag/Desugar.ag" #-}
                                     _lhsIruleDescr
                                     {-# LINE 1124 "dist/build/Desugar" #-}
                                     )) of
                              { !_tokensOruleDescr ->
                              (case (({-# LINE 126 "src-ag/Desugar.ag" #-}
                                      _lhsIchildInhs
                                      {-# LINE 1129 "dist/build/Desugar" #-}
                                      )) of
                               { !_tokensOchildInhs ->
                               (case (({-# LINE 67 "src-ag/Desugar.ag" #-}
                                       0
                                       {-# LINE 1134 "dist/build/Desugar" #-}
                                       )) of
                                { !_tokensOaddLines ->
                                (case (tokens_ _tokensOaddLines _tokensOchildInhs _tokensOchildSyns _tokensOcon _tokensOnt _tokensOruleDescr _tokensOuseFieldIdent) of
                                 { ( !_tokensIaddLines,!_tokensIerrors,!_tokensItks) ->
                                     (case (({-# LINE 38 "src-ag/Desugar.ag" #-}
                                             _tokensIerrors
                                             {-# LINE 1141 "dist/build/Desugar" #-}
                                             )) of
                                      { !_lhsOerrors ->
                                      (case (({-# LINE 69 "src-ag/Desugar.ag" #-}
                                              _tokensItks
                                              {-# LINE 1146 "dist/build/Desugar" #-}
                                              )) of
                                       { !_lhsOtks ->
                                       (case ((Syn_HsTokensRoot _lhsOerrors _lhsOtks)) of
                                        { ___node ->
                                        ( _lhsOerrors,_lhsOtks) }) }) }) }) }) }) }) }) }) }) })))
-- Nonterminal -------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         inhMap'              : Map Identifier Attributes
         synMap'              : Map Identifier Attributes
   visit 1:
      inherited attributes:
         augmentsIn           : Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))
         forcedIrrefutables   : AttrMap
         inhMap               : Map Identifier Attributes
         mainName             : String
         options              : Options
         synMap               : Map Identifier Attributes
      synthesized attributes:
         allAttributes        : AttrMap
         augmentsOut          : Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))
         errors               : Seq Error
         output               : Nonterminal 
   alternatives:
      alternative Nonterminal:
         child nt             : {NontermIdent}
         child params         : {[Identifier]}
         child inh            : {Attributes}
         child syn            : {Attributes}
         child prods          : Productions 
         visit 1:
            local augmentsIn  : _
            local augmentsOut : _
            local extraInh    : _
-}
-- cata
sem_Nonterminal :: Nonterminal ->
                   T_Nonterminal
sem_Nonterminal !(Nonterminal _nt _params _inh _syn _prods) =
    (sem_Nonterminal_Nonterminal _nt _params _inh _syn (sem_Productions _prods))
-- semantic domain
newtype T_Nonterminal = T_Nonterminal (( (Map Identifier Attributes),(Map Identifier Attributes),T_Nonterminal_1))
newtype T_Nonterminal_1 = T_Nonterminal_1 ((Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) ->
                                           AttrMap ->
                                           (Map Identifier Attributes) ->
                                           String ->
                                           Options ->
                                           (Map Identifier Attributes) ->
                                           ( AttrMap,(Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))),(Seq Error),Nonterminal))
data Inh_Nonterminal = Inh_Nonterminal {augmentsIn_Inh_Nonterminal :: !((Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression])))),forcedIrrefutables_Inh_Nonterminal :: !(AttrMap),inhMap_Inh_Nonterminal :: !((Map Identifier Attributes)),mainName_Inh_Nonterminal :: !(String),options_Inh_Nonterminal :: !(Options),synMap_Inh_Nonterminal :: !((Map Identifier Attributes))}
data Syn_Nonterminal = Syn_Nonterminal {allAttributes_Syn_Nonterminal :: !(AttrMap),augmentsOut_Syn_Nonterminal :: !((Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression])))),errors_Syn_Nonterminal :: !((Seq Error)),inhMap'_Syn_Nonterminal :: !((Map Identifier Attributes)),output_Syn_Nonterminal :: !(Nonterminal),synMap'_Syn_Nonterminal :: !((Map Identifier Attributes))}
wrap_Nonterminal :: T_Nonterminal ->
                    Inh_Nonterminal ->
                    Syn_Nonterminal
wrap_Nonterminal !(T_Nonterminal sem) !(Inh_Nonterminal _lhsIaugmentsIn _lhsIforcedIrrefutables _lhsIinhMap _lhsImainName _lhsIoptions _lhsIsynMap) =
    (let ( !_lhsOinhMap',!_lhsOsynMap',!T_Nonterminal_1 sem_1) = sem
         ( !_lhsOallAttributes,!_lhsOaugmentsOut,!_lhsOerrors,!_lhsOoutput) = sem_1 _lhsIaugmentsIn _lhsIforcedIrrefutables _lhsIinhMap _lhsImainName _lhsIoptions _lhsIsynMap
     in  (Syn_Nonterminal _lhsOallAttributes _lhsOaugmentsOut _lhsOerrors _lhsOinhMap' _lhsOoutput _lhsOsynMap'))
sem_Nonterminal_Nonterminal :: NontermIdent ->
                               ([Identifier]) ->
                               Attributes ->
                               Attributes ->
                               T_Productions ->
                               T_Nonterminal
sem_Nonterminal_Nonterminal !nt_ !params_ !inh_ !syn_ !(T_Productions prods_) =
    (T_Nonterminal (case (({-# LINE 7 "src-ag/DistChildAttr.ag" #-}
                           Map.singleton nt_ inh_
                           {-# LINE 1215 "dist/build/Desugar" #-}
                           )) of
                    { !_lhsOinhMap' ->
                    (case (({-# LINE 8 "src-ag/DistChildAttr.ag" #-}
                            Map.singleton nt_ syn_
                            {-# LINE 1220 "dist/build/Desugar" #-}
                            )) of
                     { !_lhsOsynMap' ->
                     (case ((let sem_Nonterminal_Nonterminal_1 :: T_Nonterminal_1
                                 sem_Nonterminal_Nonterminal_1 =
                                     (T_Nonterminal_1 (\ (!_lhsIaugmentsIn)
                                                         (!_lhsIforcedIrrefutables)
                                                         (!_lhsIinhMap)
                                                         (!_lhsImainName)
                                                         (!_lhsIoptions)
                                                         (!_lhsIsynMap) ->
                                                           (case (({-# LINE 157 "src-ag/Desugar.ag" #-}
                                                                   nt_
                                                                   {-# LINE 1233 "dist/build/Desugar" #-}
                                                                   )) of
                                                            { !_prodsOnt ->
                                                            (case (({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                                                    _lhsIsynMap
                                                                    {-# LINE 1238 "dist/build/Desugar" #-}
                                                                    )) of
                                                             { !_prodsOsynMap ->
                                                             (case (({-# LINE 36 "src-ag/Desugar.ag" #-}
                                                                     _lhsIoptions
                                                                     {-# LINE 1243 "dist/build/Desugar" #-}
                                                                     )) of
                                                              { !_prodsOoptions ->
                                                              (case (({-# LINE 289 "src-ag/Desugar.ag" #-}
                                                                      _lhsImainName
                                                                      {-# LINE 1248 "dist/build/Desugar" #-}
                                                                      )) of
                                                               { !_prodsOmainName ->
                                                               (case (({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                                                       _lhsIinhMap
                                                                       {-# LINE 1253 "dist/build/Desugar" #-}
                                                                       )) of
                                                                { !_prodsOinhMap ->
                                                                (case (({-# LINE 215 "src-ag/Desugar.ag" #-}
                                                                        _lhsIforcedIrrefutables
                                                                        {-# LINE 1258 "dist/build/Desugar" #-}
                                                                        )) of
                                                                 { !_prodsOforcedIrrefutables ->
                                                                 (case (({-# LINE 239 "src-ag/Desugar.ag" #-}
                                                                         Map.findWithDefault Map.empty nt_ _lhsIaugmentsIn
                                                                         {-# LINE 1263 "dist/build/Desugar" #-}
                                                                         )) of
                                                                  { !_augmentsIn ->
                                                                  (case (({-# LINE 230 "src-ag/Desugar.ag" #-}
                                                                          _augmentsIn
                                                                          {-# LINE 1268 "dist/build/Desugar" #-}
                                                                          )) of
                                                                   { !_prodsOaugmentsIn ->
                                                                   (case (prods_ _prodsOaugmentsIn _prodsOforcedIrrefutables _prodsOinhMap _prodsOmainName _prodsOnt _prodsOoptions _prodsOsynMap) of
                                                                    { ( !_prodsIallAttributes,!_prodsIaugmentsOut,!_prodsIerrors,!_prodsIoutput) ->
                                                                        (case (({-# LINE 196 "src-ag/Desugar.ag" #-}
                                                                                _prodsIallAttributes
                                                                                {-# LINE 1275 "dist/build/Desugar" #-}
                                                                                )) of
                                                                         { !_lhsOallAttributes ->
                                                                         (case (({-# LINE 240 "src-ag/Desugar.ag" #-}
                                                                                 Map.singleton nt_ _prodsIaugmentsOut
                                                                                 {-# LINE 1280 "dist/build/Desugar" #-}
                                                                                 )) of
                                                                          { !_augmentsOut ->
                                                                          (case (({-# LINE 229 "src-ag/Desugar.ag" #-}
                                                                                  _augmentsOut
                                                                                  {-# LINE 1285 "dist/build/Desugar" #-}
                                                                                  )) of
                                                                           { !_lhsOaugmentsOut ->
                                                                           (case (({-# LINE 38 "src-ag/Desugar.ag" #-}
                                                                                   _prodsIerrors
                                                                                   {-# LINE 1290 "dist/build/Desugar" #-}
                                                                                   )) of
                                                                            { !_lhsOerrors ->
                                                                            (case (({-# LINE 292 "src-ag/Desugar.ag" #-}
                                                                                    addLateAttr _lhsIoptions _lhsImainName
                                                                                    {-# LINE 1295 "dist/build/Desugar" #-}
                                                                                    )) of
                                                                             { !_extraInh ->
                                                                             (case (({-# LINE 308 "src-ag/Desugar.ag" #-}
                                                                                     Nonterminal
                                                                                       nt_ params_
                                                                                       (_extraInh     `Map.union` inh_)
                                                                                       syn_
                                                                                       _prodsIoutput
                                                                                     {-# LINE 1304 "dist/build/Desugar" #-}
                                                                                     )) of
                                                                              { !_lhsOoutput ->
                                                                              ( _lhsOallAttributes,_lhsOaugmentsOut,_lhsOerrors,_lhsOoutput) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })))
                             in  sem_Nonterminal_Nonterminal_1)) of
                      { ( !sem_Nonterminal_1) ->
                      ( _lhsOinhMap',_lhsOsynMap',sem_Nonterminal_1) }) }) }))
-- Nonterminals ------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         inhMap'              : Map Identifier Attributes
         synMap'              : Map Identifier Attributes
   visit 1:
      inherited attributes:
         augmentsIn           : Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))
         forcedIrrefutables   : AttrMap
         inhMap               : Map Identifier Attributes
         mainName             : String
         options              : Options
         synMap               : Map Identifier Attributes
      synthesized attributes:
         allAttributes        : AttrMap
         augmentsOut          : Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))
         errors               : Seq Error
         output               : Nonterminals 
   alternatives:
      alternative Cons:
         child hd             : Nonterminal 
         child tl             : Nonterminals 
         visit 1:
            local output      : _
      alternative Nil:
         visit 1:
            local output      : _
-}
-- cata
sem_Nonterminals :: Nonterminals ->
                    T_Nonterminals
sem_Nonterminals !list =
    (Prelude.foldr sem_Nonterminals_Cons sem_Nonterminals_Nil (Prelude.map sem_Nonterminal list))
-- semantic domain
newtype T_Nonterminals = T_Nonterminals (( (Map Identifier Attributes),(Map Identifier Attributes),T_Nonterminals_1))
newtype T_Nonterminals_1 = T_Nonterminals_1 ((Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) ->
                                             AttrMap ->
                                             (Map Identifier Attributes) ->
                                             String ->
                                             Options ->
                                             (Map Identifier Attributes) ->
                                             ( AttrMap,(Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))),(Seq Error),Nonterminals))
data Inh_Nonterminals = Inh_Nonterminals {augmentsIn_Inh_Nonterminals :: !((Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression])))),forcedIrrefutables_Inh_Nonterminals :: !(AttrMap),inhMap_Inh_Nonterminals :: !((Map Identifier Attributes)),mainName_Inh_Nonterminals :: !(String),options_Inh_Nonterminals :: !(Options),synMap_Inh_Nonterminals :: !((Map Identifier Attributes))}
data Syn_Nonterminals = Syn_Nonterminals {allAttributes_Syn_Nonterminals :: !(AttrMap),augmentsOut_Syn_Nonterminals :: !((Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression])))),errors_Syn_Nonterminals :: !((Seq Error)),inhMap'_Syn_Nonterminals :: !((Map Identifier Attributes)),output_Syn_Nonterminals :: !(Nonterminals),synMap'_Syn_Nonterminals :: !((Map Identifier Attributes))}
wrap_Nonterminals :: T_Nonterminals ->
                     Inh_Nonterminals ->
                     Syn_Nonterminals
wrap_Nonterminals !(T_Nonterminals sem) !(Inh_Nonterminals _lhsIaugmentsIn _lhsIforcedIrrefutables _lhsIinhMap _lhsImainName _lhsIoptions _lhsIsynMap) =
    (let ( !_lhsOinhMap',!_lhsOsynMap',!T_Nonterminals_1 sem_1) = sem
         ( !_lhsOallAttributes,!_lhsOaugmentsOut,!_lhsOerrors,!_lhsOoutput) = sem_1 _lhsIaugmentsIn _lhsIforcedIrrefutables _lhsIinhMap _lhsImainName _lhsIoptions _lhsIsynMap
     in  (Syn_Nonterminals _lhsOallAttributes _lhsOaugmentsOut _lhsOerrors _lhsOinhMap' _lhsOoutput _lhsOsynMap'))
sem_Nonterminals_Cons :: T_Nonterminal ->
                         T_Nonterminals ->
                         T_Nonterminals
sem_Nonterminals_Cons !(T_Nonterminal hd_) !(T_Nonterminals tl_) =
    (T_Nonterminals (case (tl_) of
                     { ( !_tlIinhMap',!_tlIsynMap',!T_Nonterminals_1 tl_1) ->
                         (case (hd_) of
                          { ( !_hdIinhMap',!_hdIsynMap',!T_Nonterminal_1 hd_1) ->
                              (case (({-# LINE 4 "src-ag/DistChildAttr.ag" #-}
                                      _hdIinhMap' `Map.union` _tlIinhMap'
                                      {-# LINE 1373 "dist/build/Desugar" #-}
                                      )) of
                               { !_lhsOinhMap' ->
                               (case (({-# LINE 4 "src-ag/DistChildAttr.ag" #-}
                                       _hdIsynMap' `Map.union` _tlIsynMap'
                                       {-# LINE 1378 "dist/build/Desugar" #-}
                                       )) of
                                { !_lhsOsynMap' ->
                                (case ((let sem_Nonterminals_Cons_1 :: T_Nonterminals_1
                                            sem_Nonterminals_Cons_1 =
                                                (T_Nonterminals_1 (\ (!_lhsIaugmentsIn)
                                                                     (!_lhsIforcedIrrefutables)
                                                                     (!_lhsIinhMap)
                                                                     (!_lhsImainName)
                                                                     (!_lhsIoptions)
                                                                     (!_lhsIsynMap) ->
                                                                       (case (({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                                                               _lhsIsynMap
                                                                               {-# LINE 1391 "dist/build/Desugar" #-}
                                                                               )) of
                                                                        { !_tlOsynMap ->
                                                                        (case (({-# LINE 36 "src-ag/Desugar.ag" #-}
                                                                                _lhsIoptions
                                                                                {-# LINE 1396 "dist/build/Desugar" #-}
                                                                                )) of
                                                                         { !_tlOoptions ->
                                                                         (case (({-# LINE 289 "src-ag/Desugar.ag" #-}
                                                                                 _lhsImainName
                                                                                 {-# LINE 1401 "dist/build/Desugar" #-}
                                                                                 )) of
                                                                          { !_tlOmainName ->
                                                                          (case (({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                                                                  _lhsIinhMap
                                                                                  {-# LINE 1406 "dist/build/Desugar" #-}
                                                                                  )) of
                                                                           { !_tlOinhMap ->
                                                                           (case (({-# LINE 215 "src-ag/Desugar.ag" #-}
                                                                                   _lhsIforcedIrrefutables
                                                                                   {-# LINE 1411 "dist/build/Desugar" #-}
                                                                                   )) of
                                                                            { !_tlOforcedIrrefutables ->
                                                                            (case (({-# LINE 228 "src-ag/Desugar.ag" #-}
                                                                                    _lhsIaugmentsIn
                                                                                    {-# LINE 1416 "dist/build/Desugar" #-}
                                                                                    )) of
                                                                             { !_tlOaugmentsIn ->
                                                                             (case (tl_1 _tlOaugmentsIn _tlOforcedIrrefutables _tlOinhMap _tlOmainName _tlOoptions _tlOsynMap) of
                                                                              { ( !_tlIallAttributes,!_tlIaugmentsOut,!_tlIerrors,!_tlIoutput) ->
                                                                                  (case (({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                                                                          _lhsIsynMap
                                                                                          {-# LINE 1423 "dist/build/Desugar" #-}
                                                                                          )) of
                                                                                   { !_hdOsynMap ->
                                                                                   (case (({-# LINE 36 "src-ag/Desugar.ag" #-}
                                                                                           _lhsIoptions
                                                                                           {-# LINE 1428 "dist/build/Desugar" #-}
                                                                                           )) of
                                                                                    { !_hdOoptions ->
                                                                                    (case (({-# LINE 289 "src-ag/Desugar.ag" #-}
                                                                                            _lhsImainName
                                                                                            {-# LINE 1433 "dist/build/Desugar" #-}
                                                                                            )) of
                                                                                     { !_hdOmainName ->
                                                                                     (case (({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                                                                             _lhsIinhMap
                                                                                             {-# LINE 1438 "dist/build/Desugar" #-}
                                                                                             )) of
                                                                                      { !_hdOinhMap ->
                                                                                      (case (({-# LINE 215 "src-ag/Desugar.ag" #-}
                                                                                              _lhsIforcedIrrefutables
                                                                                              {-# LINE 1443 "dist/build/Desugar" #-}
                                                                                              )) of
                                                                                       { !_hdOforcedIrrefutables ->
                                                                                       (case (({-# LINE 228 "src-ag/Desugar.ag" #-}
                                                                                               _lhsIaugmentsIn
                                                                                               {-# LINE 1448 "dist/build/Desugar" #-}
                                                                                               )) of
                                                                                        { !_hdOaugmentsIn ->
                                                                                        (case (hd_1 _hdOaugmentsIn _hdOforcedIrrefutables _hdOinhMap _hdOmainName _hdOoptions _hdOsynMap) of
                                                                                         { ( !_hdIallAttributes,!_hdIaugmentsOut,!_hdIerrors,!_hdIoutput) ->
                                                                                             (case (({-# LINE 196 "src-ag/Desugar.ag" #-}
                                                                                                     _hdIallAttributes `mergeAttributes` _tlIallAttributes
                                                                                                     {-# LINE 1455 "dist/build/Desugar" #-}
                                                                                                     )) of
                                                                                              { !_lhsOallAttributes ->
                                                                                              (case (({-# LINE 229 "src-ag/Desugar.ag" #-}
                                                                                                      _hdIaugmentsOut `Map.union` _tlIaugmentsOut
                                                                                                      {-# LINE 1460 "dist/build/Desugar" #-}
                                                                                                      )) of
                                                                                               { !_lhsOaugmentsOut ->
                                                                                               (case (({-# LINE 38 "src-ag/Desugar.ag" #-}
                                                                                                       _hdIerrors Seq.>< _tlIerrors
                                                                                                       {-# LINE 1465 "dist/build/Desugar" #-}
                                                                                                       )) of
                                                                                                { !_lhsOerrors ->
                                                                                                (case (({-# LINE 40 "src-ag/Desugar.ag" #-}
                                                                                                        (:) _hdIoutput _tlIoutput
                                                                                                        {-# LINE 1470 "dist/build/Desugar" #-}
                                                                                                        )) of
                                                                                                 { !_output ->
                                                                                                 (case (({-# LINE 40 "src-ag/Desugar.ag" #-}
                                                                                                         _output
                                                                                                         {-# LINE 1475 "dist/build/Desugar" #-}
                                                                                                         )) of
                                                                                                  { !_lhsOoutput ->
                                                                                                  ( _lhsOallAttributes,_lhsOaugmentsOut,_lhsOerrors,_lhsOoutput) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })))
                                        in  sem_Nonterminals_Cons_1)) of
                                 { ( !sem_Nonterminals_1) ->
                                 ( _lhsOinhMap',_lhsOsynMap',sem_Nonterminals_1) }) }) }) }) }))
sem_Nonterminals_Nil :: T_Nonterminals
sem_Nonterminals_Nil =
    (T_Nonterminals (case (({-# LINE 4 "src-ag/DistChildAttr.ag" #-}
                            Map.empty
                            {-# LINE 1486 "dist/build/Desugar" #-}
                            )) of
                     { !_lhsOinhMap' ->
                     (case (({-# LINE 4 "src-ag/DistChildAttr.ag" #-}
                             Map.empty
                             {-# LINE 1491 "dist/build/Desugar" #-}
                             )) of
                      { !_lhsOsynMap' ->
                      (case ((let sem_Nonterminals_Nil_1 :: T_Nonterminals_1
                                  sem_Nonterminals_Nil_1 =
                                      (T_Nonterminals_1 (\ (!_lhsIaugmentsIn)
                                                           (!_lhsIforcedIrrefutables)
                                                           (!_lhsIinhMap)
                                                           (!_lhsImainName)
                                                           (!_lhsIoptions)
                                                           (!_lhsIsynMap) ->
                                                             (case (({-# LINE 196 "src-ag/Desugar.ag" #-}
                                                                     Map.empty
                                                                     {-# LINE 1504 "dist/build/Desugar" #-}
                                                                     )) of
                                                              { !_lhsOallAttributes ->
                                                              (case (({-# LINE 229 "src-ag/Desugar.ag" #-}
                                                                      Map.empty
                                                                      {-# LINE 1509 "dist/build/Desugar" #-}
                                                                      )) of
                                                               { !_lhsOaugmentsOut ->
                                                               (case (({-# LINE 38 "src-ag/Desugar.ag" #-}
                                                                       Seq.empty
                                                                       {-# LINE 1514 "dist/build/Desugar" #-}
                                                                       )) of
                                                                { !_lhsOerrors ->
                                                                (case (({-# LINE 40 "src-ag/Desugar.ag" #-}
                                                                        []
                                                                        {-# LINE 1519 "dist/build/Desugar" #-}
                                                                        )) of
                                                                 { !_output ->
                                                                 (case (({-# LINE 40 "src-ag/Desugar.ag" #-}
                                                                         _output
                                                                         {-# LINE 1524 "dist/build/Desugar" #-}
                                                                         )) of
                                                                  { !_lhsOoutput ->
                                                                  ( _lhsOallAttributes,_lhsOaugmentsOut,_lhsOerrors,_lhsOoutput) }) }) }) }) })))
                              in  sem_Nonterminals_Nil_1)) of
                       { ( !sem_Nonterminals_1) ->
                       ( _lhsOinhMap',_lhsOsynMap',sem_Nonterminals_1) }) }) }))
-- Pattern -----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         defsCollect          : Set (Identifier, Identifier)
   visit 1:
      inherited attributes:
         childInhs            : [(Identifier, Identifier)]
         childSyns            : [(Identifier, Identifier)]
         con                  : ConstructorIdent
         defs                 : Set (Identifier, Identifier)
         forcedIrrefutables   : AttrMap
         nt                   : NontermIdent
      synthesized attributes:
         allAttributes        : AttrMap
         copy                 : Pattern 
         errors               : Seq Error
         output               : Pattern 
   alternatives:
      alternative Alias:
         child field          : {Identifier}
         child attr           : {Identifier}
         child pat            : Pattern 
         visit 0:
            local def         : _
         visit 1:
            local copy        : _
            local _tup2       : _
            local field'      : _
            local err2        : _
            local err1        : _
            local output      : _
      alternative Constr:
         child name           : {ConstructorIdent}
         child pats           : Patterns 
         visit 1:
            local copy        : _
            local output      : _
      alternative Irrefutable:
         child pat            : Pattern 
         visit 1:
            local copy        : _
            local output      : _
      alternative Product:
         child pos            : {Pos}
         child pats           : Patterns 
         visit 1:
            local copy        : _
            local output      : _
      alternative Underscore:
         child pos            : {Pos}
         visit 1:
            local copy        : _
            local output      : _
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
newtype T_Pattern = T_Pattern (( (Set (Identifier, Identifier)),T_Pattern_1))
newtype T_Pattern_1 = T_Pattern_1 (([(Identifier, Identifier)]) ->
                                   ([(Identifier, Identifier)]) ->
                                   ConstructorIdent ->
                                   (Set (Identifier, Identifier)) ->
                                   AttrMap ->
                                   NontermIdent ->
                                   ( AttrMap,Pattern,(Seq Error),Pattern))
data Inh_Pattern = Inh_Pattern {childInhs_Inh_Pattern :: !(([(Identifier, Identifier)])),childSyns_Inh_Pattern :: !(([(Identifier, Identifier)])),con_Inh_Pattern :: !(ConstructorIdent),defs_Inh_Pattern :: !((Set (Identifier, Identifier))),forcedIrrefutables_Inh_Pattern :: !(AttrMap),nt_Inh_Pattern :: !(NontermIdent)}
data Syn_Pattern = Syn_Pattern {allAttributes_Syn_Pattern :: !(AttrMap),copy_Syn_Pattern :: !(Pattern),defsCollect_Syn_Pattern :: !((Set (Identifier, Identifier))),errors_Syn_Pattern :: !((Seq Error)),output_Syn_Pattern :: !(Pattern)}
wrap_Pattern :: T_Pattern ->
                Inh_Pattern ->
                Syn_Pattern
wrap_Pattern !(T_Pattern sem) !(Inh_Pattern _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt) =
    (let ( !_lhsOdefsCollect,!T_Pattern_1 sem_1) = sem
         ( !_lhsOallAttributes,!_lhsOcopy,!_lhsOerrors,!_lhsOoutput) = sem_1 _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt
     in  (Syn_Pattern _lhsOallAttributes _lhsOcopy _lhsOdefsCollect _lhsOerrors _lhsOoutput))
sem_Pattern_Alias :: Identifier ->
                     Identifier ->
                     T_Pattern ->
                     T_Pattern
sem_Pattern_Alias !field_ !attr_ !(T_Pattern pat_) =
    (T_Pattern (case (({-# LINE 182 "src-ag/Desugar.ag" #-}
                       Set.singleton (field_, attr_)
                       {-# LINE 1624 "dist/build/Desugar" #-}
                       )) of
                { !_def ->
                (case (pat_) of
                 { ( !_patIdefsCollect,!T_Pattern_1 pat_1) ->
                     (case (({-# LINE 183 "src-ag/Desugar.ag" #-}
                             _def     `Set.union` _patIdefsCollect
                             {-# LINE 1631 "dist/build/Desugar" #-}
                             )) of
                      { !_lhsOdefsCollect ->
                      (case ((let sem_Pattern_Alias_1 :: T_Pattern_1
                                  sem_Pattern_Alias_1 =
                                      (T_Pattern_1 (\ (!_lhsIchildInhs)
                                                      (!_lhsIchildSyns)
                                                      (!_lhsIcon)
                                                      (!_lhsIdefs)
                                                      (!_lhsIforcedIrrefutables)
                                                      (!_lhsInt) ->
                                                        (case (({-# LINE 152 "src-ag/Desugar.ag" #-}
                                                                _lhsInt
                                                                {-# LINE 1644 "dist/build/Desugar" #-}
                                                                )) of
                                                         { !_patOnt ->
                                                         (case (({-# LINE 153 "src-ag/Desugar.ag" #-}
                                                                 _lhsIcon
                                                                 {-# LINE 1649 "dist/build/Desugar" #-}
                                                                 )) of
                                                          { !_patOcon ->
                                                          (case (({-# LINE 215 "src-ag/Desugar.ag" #-}
                                                                  _lhsIforcedIrrefutables
                                                                  {-# LINE 1654 "dist/build/Desugar" #-}
                                                                  )) of
                                                           { !_patOforcedIrrefutables ->
                                                           (case (({-# LINE 185 "src-ag/Desugar.ag" #-}
                                                                   _lhsIdefs
                                                                   {-# LINE 1659 "dist/build/Desugar" #-}
                                                                   )) of
                                                            { !_patOdefs ->
                                                            (case (({-# LINE 126 "src-ag/Desugar.ag" #-}
                                                                    _lhsIchildSyns
                                                                    {-# LINE 1664 "dist/build/Desugar" #-}
                                                                    )) of
                                                             { !_patOchildSyns ->
                                                             (case (({-# LINE 126 "src-ag/Desugar.ag" #-}
                                                                     _lhsIchildInhs
                                                                     {-# LINE 1669 "dist/build/Desugar" #-}
                                                                     )) of
                                                              { !_patOchildInhs ->
                                                              (case (pat_1 _patOchildInhs _patOchildSyns _patOcon _patOdefs _patOforcedIrrefutables _patOnt) of
                                                               { ( !_patIallAttributes,!_patIcopy,!_patIerrors,!_patIoutput) ->
                                                                   (case (({-# LINE 200 "src-ag/Desugar.ag" #-}
                                                                           (Map.singleton _lhsInt $ Map.singleton _lhsIcon $ Set.singleton (field_, attr_)) `mergeAttributes` _patIallAttributes
                                                                           {-# LINE 1676 "dist/build/Desugar" #-}
                                                                           )) of
                                                                    { !_lhsOallAttributes ->
                                                                    (case (({-# LINE 22 "src-ag/Patterns.ag" #-}
                                                                            Alias field_ attr_ _patIcopy
                                                                            {-# LINE 1681 "dist/build/Desugar" #-}
                                                                            )) of
                                                                     { !_copy ->
                                                                     (case (({-# LINE 22 "src-ag/Patterns.ag" #-}
                                                                             _copy
                                                                             {-# LINE 1686 "dist/build/Desugar" #-}
                                                                             )) of
                                                                      { !_lhsOcopy ->
                                                                      (case (({-# LINE 110 "src-ag/Desugar.ag" #-}
                                                                              maybeError field_ (UndefAttr _lhsInt _lhsIcon (Ident "<ANY>" (getPos field_)) attr_ True) $
                                                                                findField field_ attr_ _lhsIchildInhs
                                                                              {-# LINE 1692 "dist/build/Desugar" #-}
                                                                              )) of
                                                                       { !__tup2 ->
                                                                       (case (({-# LINE 110 "src-ag/Desugar.ag" #-}
                                                                               __tup2
                                                                               {-# LINE 1697 "dist/build/Desugar" #-}
                                                                               )) of
                                                                        { !(!_field',_) ->
                                                                        (case (({-# LINE 112 "src-ag/Desugar.ag" #-}
                                                                                if _field'     == field_
                                                                                then Seq.empty
                                                                                else if (_field'    , attr_) `Set.member` _lhsIdefs
                                                                                     then Seq.singleton $ DupRule _lhsInt _lhsIcon field_ attr_ _field'
                                                                                     else Seq.empty
                                                                                {-# LINE 1706 "dist/build/Desugar" #-}
                                                                                )) of
                                                                         { !_err2 ->
                                                                         (case (({-# LINE 110 "src-ag/Desugar.ag" #-}
                                                                                 __tup2
                                                                                 {-# LINE 1711 "dist/build/Desugar" #-}
                                                                                 )) of
                                                                          { !(_,!_err1) ->
                                                                          (case (({-# LINE 117 "src-ag/Desugar.ag" #-}
                                                                                  _err1     Seq.>< _err2     Seq.>< _patIerrors
                                                                                  {-# LINE 1716 "dist/build/Desugar" #-}
                                                                                  )) of
                                                                           { !_lhsOerrors ->
                                                                           (case (({-# LINE 118 "src-ag/Desugar.ag" #-}
                                                                                   Alias _field'     attr_ _patIoutput
                                                                                   {-# LINE 1721 "dist/build/Desugar" #-}
                                                                                   )) of
                                                                            { !_output ->
                                                                            (case (({-# LINE 219 "src-ag/Desugar.ag" #-}
                                                                                    if Set.member (field_, attr_) $ Map.findWithDefault Set.empty _lhsIcon $ Map.findWithDefault Map.empty _lhsInt $ _lhsIforcedIrrefutables
                                                                                    then Irrefutable _output
                                                                                    else _output
                                                                                    {-# LINE 1728 "dist/build/Desugar" #-}
                                                                                    )) of
                                                                             { !_lhsOoutput ->
                                                                             ( _lhsOallAttributes,_lhsOcopy,_lhsOerrors,_lhsOoutput) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })))
                              in  sem_Pattern_Alias_1)) of
                       { ( !sem_Pattern_1) ->
                       ( _lhsOdefsCollect,sem_Pattern_1) }) }) }) }))
sem_Pattern_Constr :: ConstructorIdent ->
                      T_Patterns ->
                      T_Pattern
sem_Pattern_Constr !name_ !(T_Patterns pats_) =
    (T_Pattern (case (pats_) of
                { ( !_patsIdefsCollect,!T_Patterns_1 pats_1) ->
                    (case (({-# LINE 179 "src-ag/Desugar.ag" #-}
                            _patsIdefsCollect
                            {-# LINE 1743 "dist/build/Desugar" #-}
                            )) of
                     { !_lhsOdefsCollect ->
                     (case ((let sem_Pattern_Constr_1 :: T_Pattern_1
                                 sem_Pattern_Constr_1 =
                                     (T_Pattern_1 (\ (!_lhsIchildInhs)
                                                     (!_lhsIchildSyns)
                                                     (!_lhsIcon)
                                                     (!_lhsIdefs)
                                                     (!_lhsIforcedIrrefutables)
                                                     (!_lhsInt) ->
                                                       (case (({-# LINE 152 "src-ag/Desugar.ag" #-}
                                                               _lhsInt
                                                               {-# LINE 1756 "dist/build/Desugar" #-}
                                                               )) of
                                                        { !_patsOnt ->
                                                        (case (({-# LINE 153 "src-ag/Desugar.ag" #-}
                                                                _lhsIcon
                                                                {-# LINE 1761 "dist/build/Desugar" #-}
                                                                )) of
                                                         { !_patsOcon ->
                                                         (case (({-# LINE 215 "src-ag/Desugar.ag" #-}
                                                                 _lhsIforcedIrrefutables
                                                                 {-# LINE 1766 "dist/build/Desugar" #-}
                                                                 )) of
                                                          { !_patsOforcedIrrefutables ->
                                                          (case (({-# LINE 185 "src-ag/Desugar.ag" #-}
                                                                  _lhsIdefs
                                                                  {-# LINE 1771 "dist/build/Desugar" #-}
                                                                  )) of
                                                           { !_patsOdefs ->
                                                           (case (({-# LINE 126 "src-ag/Desugar.ag" #-}
                                                                   _lhsIchildSyns
                                                                   {-# LINE 1776 "dist/build/Desugar" #-}
                                                                   )) of
                                                            { !_patsOchildSyns ->
                                                            (case (({-# LINE 126 "src-ag/Desugar.ag" #-}
                                                                    _lhsIchildInhs
                                                                    {-# LINE 1781 "dist/build/Desugar" #-}
                                                                    )) of
                                                             { !_patsOchildInhs ->
                                                             (case (pats_1 _patsOchildInhs _patsOchildSyns _patsOcon _patsOdefs _patsOforcedIrrefutables _patsOnt) of
                                                              { ( !_patsIallAttributes,!_patsIcopy,!_patsIerrors,!_patsIoutput) ->
                                                                  (case (({-# LINE 196 "src-ag/Desugar.ag" #-}
                                                                          _patsIallAttributes
                                                                          {-# LINE 1788 "dist/build/Desugar" #-}
                                                                          )) of
                                                                   { !_lhsOallAttributes ->
                                                                   (case (({-# LINE 22 "src-ag/Patterns.ag" #-}
                                                                           Constr name_ _patsIcopy
                                                                           {-# LINE 1793 "dist/build/Desugar" #-}
                                                                           )) of
                                                                    { !_copy ->
                                                                    (case (({-# LINE 22 "src-ag/Patterns.ag" #-}
                                                                            _copy
                                                                            {-# LINE 1798 "dist/build/Desugar" #-}
                                                                            )) of
                                                                     { !_lhsOcopy ->
                                                                     (case (({-# LINE 38 "src-ag/Desugar.ag" #-}
                                                                             _patsIerrors
                                                                             {-# LINE 1803 "dist/build/Desugar" #-}
                                                                             )) of
                                                                      { !_lhsOerrors ->
                                                                      (case (({-# LINE 40 "src-ag/Desugar.ag" #-}
                                                                              Constr name_ _patsIoutput
                                                                              {-# LINE 1808 "dist/build/Desugar" #-}
                                                                              )) of
                                                                       { !_output ->
                                                                       (case (({-# LINE 40 "src-ag/Desugar.ag" #-}
                                                                               _output
                                                                               {-# LINE 1813 "dist/build/Desugar" #-}
                                                                               )) of
                                                                        { !_lhsOoutput ->
                                                                        ( _lhsOallAttributes,_lhsOcopy,_lhsOerrors,_lhsOoutput) }) }) }) }) }) }) }) }) }) }) }) }) })))
                             in  sem_Pattern_Constr_1)) of
                      { ( !sem_Pattern_1) ->
                      ( _lhsOdefsCollect,sem_Pattern_1) }) }) }))
sem_Pattern_Irrefutable :: T_Pattern ->
                           T_Pattern
sem_Pattern_Irrefutable !(T_Pattern pat_) =
    (T_Pattern (case (pat_) of
                { ( !_patIdefsCollect,!T_Pattern_1 pat_1) ->
                    (case (({-# LINE 179 "src-ag/Desugar.ag" #-}
                            _patIdefsCollect
                            {-# LINE 1827 "dist/build/Desugar" #-}
                            )) of
                     { !_lhsOdefsCollect ->
                     (case ((let sem_Pattern_Irrefutable_1 :: T_Pattern_1
                                 sem_Pattern_Irrefutable_1 =
                                     (T_Pattern_1 (\ (!_lhsIchildInhs)
                                                     (!_lhsIchildSyns)
                                                     (!_lhsIcon)
                                                     (!_lhsIdefs)
                                                     (!_lhsIforcedIrrefutables)
                                                     (!_lhsInt) ->
                                                       (case (({-# LINE 202 "src-ag/Desugar.ag" #-}
                                                               Map.empty
                                                               {-# LINE 1840 "dist/build/Desugar" #-}
                                                               )) of
                                                        { !_lhsOallAttributes ->
                                                        (case (({-# LINE 152 "src-ag/Desugar.ag" #-}
                                                                _lhsInt
                                                                {-# LINE 1845 "dist/build/Desugar" #-}
                                                                )) of
                                                         { !_patOnt ->
                                                         (case (({-# LINE 215 "src-ag/Desugar.ag" #-}
                                                                 _lhsIforcedIrrefutables
                                                                 {-# LINE 1850 "dist/build/Desugar" #-}
                                                                 )) of
                                                          { !_patOforcedIrrefutables ->
                                                          (case (({-# LINE 185 "src-ag/Desugar.ag" #-}
                                                                  _lhsIdefs
                                                                  {-# LINE 1855 "dist/build/Desugar" #-}
                                                                  )) of
                                                           { !_patOdefs ->
                                                           (case (({-# LINE 153 "src-ag/Desugar.ag" #-}
                                                                   _lhsIcon
                                                                   {-# LINE 1860 "dist/build/Desugar" #-}
                                                                   )) of
                                                            { !_patOcon ->
                                                            (case (({-# LINE 126 "src-ag/Desugar.ag" #-}
                                                                    _lhsIchildSyns
                                                                    {-# LINE 1865 "dist/build/Desugar" #-}
                                                                    )) of
                                                             { !_patOchildSyns ->
                                                             (case (({-# LINE 126 "src-ag/Desugar.ag" #-}
                                                                     _lhsIchildInhs
                                                                     {-# LINE 1870 "dist/build/Desugar" #-}
                                                                     )) of
                                                              { !_patOchildInhs ->
                                                              (case (pat_1 _patOchildInhs _patOchildSyns _patOcon _patOdefs _patOforcedIrrefutables _patOnt) of
                                                               { ( !_patIallAttributes,!_patIcopy,!_patIerrors,!_patIoutput) ->
                                                                   (case (({-# LINE 22 "src-ag/Patterns.ag" #-}
                                                                           Irrefutable _patIcopy
                                                                           {-# LINE 1877 "dist/build/Desugar" #-}
                                                                           )) of
                                                                    { !_copy ->
                                                                    (case (({-# LINE 22 "src-ag/Patterns.ag" #-}
                                                                            _copy
                                                                            {-# LINE 1882 "dist/build/Desugar" #-}
                                                                            )) of
                                                                     { !_lhsOcopy ->
                                                                     (case (({-# LINE 38 "src-ag/Desugar.ag" #-}
                                                                             _patIerrors
                                                                             {-# LINE 1887 "dist/build/Desugar" #-}
                                                                             )) of
                                                                      { !_lhsOerrors ->
                                                                      (case (({-# LINE 40 "src-ag/Desugar.ag" #-}
                                                                              Irrefutable _patIoutput
                                                                              {-# LINE 1892 "dist/build/Desugar" #-}
                                                                              )) of
                                                                       { !_output ->
                                                                       (case (({-# LINE 40 "src-ag/Desugar.ag" #-}
                                                                               _output
                                                                               {-# LINE 1897 "dist/build/Desugar" #-}
                                                                               )) of
                                                                        { !_lhsOoutput ->
                                                                        ( _lhsOallAttributes,_lhsOcopy,_lhsOerrors,_lhsOoutput) }) }) }) }) }) }) }) }) }) }) }) }) })))
                             in  sem_Pattern_Irrefutable_1)) of
                      { ( !sem_Pattern_1) ->
                      ( _lhsOdefsCollect,sem_Pattern_1) }) }) }))
sem_Pattern_Product :: Pos ->
                       T_Patterns ->
                       T_Pattern
sem_Pattern_Product !pos_ !(T_Patterns pats_) =
    (T_Pattern (case (pats_) of
                { ( !_patsIdefsCollect,!T_Patterns_1 pats_1) ->
                    (case (({-# LINE 179 "src-ag/Desugar.ag" #-}
                            _patsIdefsCollect
                            {-# LINE 1912 "dist/build/Desugar" #-}
                            )) of
                     { !_lhsOdefsCollect ->
                     (case ((let sem_Pattern_Product_1 :: T_Pattern_1
                                 sem_Pattern_Product_1 =
                                     (T_Pattern_1 (\ (!_lhsIchildInhs)
                                                     (!_lhsIchildSyns)
                                                     (!_lhsIcon)
                                                     (!_lhsIdefs)
                                                     (!_lhsIforcedIrrefutables)
                                                     (!_lhsInt) ->
                                                       (case (({-# LINE 152 "src-ag/Desugar.ag" #-}
                                                               _lhsInt
                                                               {-# LINE 1925 "dist/build/Desugar" #-}
                                                               )) of
                                                        { !_patsOnt ->
                                                        (case (({-# LINE 153 "src-ag/Desugar.ag" #-}
                                                                _lhsIcon
                                                                {-# LINE 1930 "dist/build/Desugar" #-}
                                                                )) of
                                                         { !_patsOcon ->
                                                         (case (({-# LINE 215 "src-ag/Desugar.ag" #-}
                                                                 _lhsIforcedIrrefutables
                                                                 {-# LINE 1935 "dist/build/Desugar" #-}
                                                                 )) of
                                                          { !_patsOforcedIrrefutables ->
                                                          (case (({-# LINE 185 "src-ag/Desugar.ag" #-}
                                                                  _lhsIdefs
                                                                  {-# LINE 1940 "dist/build/Desugar" #-}
                                                                  )) of
                                                           { !_patsOdefs ->
                                                           (case (({-# LINE 126 "src-ag/Desugar.ag" #-}
                                                                   _lhsIchildSyns
                                                                   {-# LINE 1945 "dist/build/Desugar" #-}
                                                                   )) of
                                                            { !_patsOchildSyns ->
                                                            (case (({-# LINE 126 "src-ag/Desugar.ag" #-}
                                                                    _lhsIchildInhs
                                                                    {-# LINE 1950 "dist/build/Desugar" #-}
                                                                    )) of
                                                             { !_patsOchildInhs ->
                                                             (case (pats_1 _patsOchildInhs _patsOchildSyns _patsOcon _patsOdefs _patsOforcedIrrefutables _patsOnt) of
                                                              { ( !_patsIallAttributes,!_patsIcopy,!_patsIerrors,!_patsIoutput) ->
                                                                  (case (({-# LINE 196 "src-ag/Desugar.ag" #-}
                                                                          _patsIallAttributes
                                                                          {-# LINE 1957 "dist/build/Desugar" #-}
                                                                          )) of
                                                                   { !_lhsOallAttributes ->
                                                                   (case (({-# LINE 22 "src-ag/Patterns.ag" #-}
                                                                           Product pos_ _patsIcopy
                                                                           {-# LINE 1962 "dist/build/Desugar" #-}
                                                                           )) of
                                                                    { !_copy ->
                                                                    (case (({-# LINE 22 "src-ag/Patterns.ag" #-}
                                                                            _copy
                                                                            {-# LINE 1967 "dist/build/Desugar" #-}
                                                                            )) of
                                                                     { !_lhsOcopy ->
                                                                     (case (({-# LINE 38 "src-ag/Desugar.ag" #-}
                                                                             _patsIerrors
                                                                             {-# LINE 1972 "dist/build/Desugar" #-}
                                                                             )) of
                                                                      { !_lhsOerrors ->
                                                                      (case (({-# LINE 40 "src-ag/Desugar.ag" #-}
                                                                              Product pos_ _patsIoutput
                                                                              {-# LINE 1977 "dist/build/Desugar" #-}
                                                                              )) of
                                                                       { !_output ->
                                                                       (case (({-# LINE 40 "src-ag/Desugar.ag" #-}
                                                                               _output
                                                                               {-# LINE 1982 "dist/build/Desugar" #-}
                                                                               )) of
                                                                        { !_lhsOoutput ->
                                                                        ( _lhsOallAttributes,_lhsOcopy,_lhsOerrors,_lhsOoutput) }) }) }) }) }) }) }) }) }) }) }) }) })))
                             in  sem_Pattern_Product_1)) of
                      { ( !sem_Pattern_1) ->
                      ( _lhsOdefsCollect,sem_Pattern_1) }) }) }))
sem_Pattern_Underscore :: Pos ->
                          T_Pattern
sem_Pattern_Underscore !pos_ =
    (T_Pattern (case (({-# LINE 179 "src-ag/Desugar.ag" #-}
                       Set.empty
                       {-# LINE 1994 "dist/build/Desugar" #-}
                       )) of
                { !_lhsOdefsCollect ->
                (case ((let sem_Pattern_Underscore_1 :: T_Pattern_1
                            sem_Pattern_Underscore_1 =
                                (T_Pattern_1 (\ (!_lhsIchildInhs)
                                                (!_lhsIchildSyns)
                                                (!_lhsIcon)
                                                (!_lhsIdefs)
                                                (!_lhsIforcedIrrefutables)
                                                (!_lhsInt) ->
                                                  (case (({-# LINE 196 "src-ag/Desugar.ag" #-}
                                                          Map.empty
                                                          {-# LINE 2007 "dist/build/Desugar" #-}
                                                          )) of
                                                   { !_lhsOallAttributes ->
                                                   (case (({-# LINE 22 "src-ag/Patterns.ag" #-}
                                                           Underscore pos_
                                                           {-# LINE 2012 "dist/build/Desugar" #-}
                                                           )) of
                                                    { !_copy ->
                                                    (case (({-# LINE 22 "src-ag/Patterns.ag" #-}
                                                            _copy
                                                            {-# LINE 2017 "dist/build/Desugar" #-}
                                                            )) of
                                                     { !_lhsOcopy ->
                                                     (case (({-# LINE 38 "src-ag/Desugar.ag" #-}
                                                             Seq.empty
                                                             {-# LINE 2022 "dist/build/Desugar" #-}
                                                             )) of
                                                      { !_lhsOerrors ->
                                                      (case (({-# LINE 40 "src-ag/Desugar.ag" #-}
                                                              Underscore pos_
                                                              {-# LINE 2027 "dist/build/Desugar" #-}
                                                              )) of
                                                       { !_output ->
                                                       (case (({-# LINE 40 "src-ag/Desugar.ag" #-}
                                                               _output
                                                               {-# LINE 2032 "dist/build/Desugar" #-}
                                                               )) of
                                                        { !_lhsOoutput ->
                                                        ( _lhsOallAttributes,_lhsOcopy,_lhsOerrors,_lhsOoutput) }) }) }) }) }) })))
                        in  sem_Pattern_Underscore_1)) of
                 { ( !sem_Pattern_1) ->
                 ( _lhsOdefsCollect,sem_Pattern_1) }) }))
-- Patterns ----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         defsCollect          : Set (Identifier, Identifier)
   visit 1:
      inherited attributes:
         childInhs            : [(Identifier, Identifier)]
         childSyns            : [(Identifier, Identifier)]
         con                  : ConstructorIdent
         defs                 : Set (Identifier, Identifier)
         forcedIrrefutables   : AttrMap
         nt                   : NontermIdent
      synthesized attributes:
         allAttributes        : AttrMap
         copy                 : Patterns 
         errors               : Seq Error
         output               : Patterns 
   alternatives:
      alternative Cons:
         child hd             : Pattern 
         child tl             : Patterns 
         visit 1:
            local copy        : _
            local output      : _
      alternative Nil:
         visit 1:
            local copy        : _
            local output      : _
-}
-- cata
sem_Patterns :: Patterns ->
                T_Patterns
sem_Patterns !list =
    (Prelude.foldr sem_Patterns_Cons sem_Patterns_Nil (Prelude.map sem_Pattern list))
-- semantic domain
newtype T_Patterns = T_Patterns (( (Set (Identifier, Identifier)),T_Patterns_1))
newtype T_Patterns_1 = T_Patterns_1 (([(Identifier, Identifier)]) ->
                                     ([(Identifier, Identifier)]) ->
                                     ConstructorIdent ->
                                     (Set (Identifier, Identifier)) ->
                                     AttrMap ->
                                     NontermIdent ->
                                     ( AttrMap,Patterns,(Seq Error),Patterns))
data Inh_Patterns = Inh_Patterns {childInhs_Inh_Patterns :: !(([(Identifier, Identifier)])),childSyns_Inh_Patterns :: !(([(Identifier, Identifier)])),con_Inh_Patterns :: !(ConstructorIdent),defs_Inh_Patterns :: !((Set (Identifier, Identifier))),forcedIrrefutables_Inh_Patterns :: !(AttrMap),nt_Inh_Patterns :: !(NontermIdent)}
data Syn_Patterns = Syn_Patterns {allAttributes_Syn_Patterns :: !(AttrMap),copy_Syn_Patterns :: !(Patterns),defsCollect_Syn_Patterns :: !((Set (Identifier, Identifier))),errors_Syn_Patterns :: !((Seq Error)),output_Syn_Patterns :: !(Patterns)}
wrap_Patterns :: T_Patterns ->
                 Inh_Patterns ->
                 Syn_Patterns
wrap_Patterns !(T_Patterns sem) !(Inh_Patterns _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt) =
    (let ( !_lhsOdefsCollect,!T_Patterns_1 sem_1) = sem
         ( !_lhsOallAttributes,!_lhsOcopy,!_lhsOerrors,!_lhsOoutput) = sem_1 _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt
     in  (Syn_Patterns _lhsOallAttributes _lhsOcopy _lhsOdefsCollect _lhsOerrors _lhsOoutput))
sem_Patterns_Cons :: T_Pattern ->
                     T_Patterns ->
                     T_Patterns
sem_Patterns_Cons !(T_Pattern hd_) !(T_Patterns tl_) =
    (T_Patterns (case (tl_) of
                 { ( !_tlIdefsCollect,!T_Patterns_1 tl_1) ->
                     (case (hd_) of
                      { ( !_hdIdefsCollect,!T_Pattern_1 hd_1) ->
                          (case (({-# LINE 179 "src-ag/Desugar.ag" #-}
                                  _hdIdefsCollect `Set.union` _tlIdefsCollect
                                  {-# LINE 2102 "dist/build/Desugar" #-}
                                  )) of
                           { !_lhsOdefsCollect ->
                           (case ((let sem_Patterns_Cons_1 :: T_Patterns_1
                                       sem_Patterns_Cons_1 =
                                           (T_Patterns_1 (\ (!_lhsIchildInhs)
                                                            (!_lhsIchildSyns)
                                                            (!_lhsIcon)
                                                            (!_lhsIdefs)
                                                            (!_lhsIforcedIrrefutables)
                                                            (!_lhsInt) ->
                                                              (case (({-# LINE 152 "src-ag/Desugar.ag" #-}
                                                                      _lhsInt
                                                                      {-# LINE 2115 "dist/build/Desugar" #-}
                                                                      )) of
                                                               { !_tlOnt ->
                                                               (case (({-# LINE 153 "src-ag/Desugar.ag" #-}
                                                                       _lhsIcon
                                                                       {-# LINE 2120 "dist/build/Desugar" #-}
                                                                       )) of
                                                                { !_tlOcon ->
                                                                (case (({-# LINE 152 "src-ag/Desugar.ag" #-}
                                                                        _lhsInt
                                                                        {-# LINE 2125 "dist/build/Desugar" #-}
                                                                        )) of
                                                                 { !_hdOnt ->
                                                                 (case (({-# LINE 153 "src-ag/Desugar.ag" #-}
                                                                         _lhsIcon
                                                                         {-# LINE 2130 "dist/build/Desugar" #-}
                                                                         )) of
                                                                  { !_hdOcon ->
                                                                  (case (({-# LINE 215 "src-ag/Desugar.ag" #-}
                                                                          _lhsIforcedIrrefutables
                                                                          {-# LINE 2135 "dist/build/Desugar" #-}
                                                                          )) of
                                                                   { !_tlOforcedIrrefutables ->
                                                                   (case (({-# LINE 185 "src-ag/Desugar.ag" #-}
                                                                           _lhsIdefs
                                                                           {-# LINE 2140 "dist/build/Desugar" #-}
                                                                           )) of
                                                                    { !_tlOdefs ->
                                                                    (case (({-# LINE 126 "src-ag/Desugar.ag" #-}
                                                                            _lhsIchildSyns
                                                                            {-# LINE 2145 "dist/build/Desugar" #-}
                                                                            )) of
                                                                     { !_tlOchildSyns ->
                                                                     (case (({-# LINE 126 "src-ag/Desugar.ag" #-}
                                                                             _lhsIchildInhs
                                                                             {-# LINE 2150 "dist/build/Desugar" #-}
                                                                             )) of
                                                                      { !_tlOchildInhs ->
                                                                      (case (tl_1 _tlOchildInhs _tlOchildSyns _tlOcon _tlOdefs _tlOforcedIrrefutables _tlOnt) of
                                                                       { ( !_tlIallAttributes,!_tlIcopy,!_tlIerrors,!_tlIoutput) ->
                                                                           (case (({-# LINE 215 "src-ag/Desugar.ag" #-}
                                                                                   _lhsIforcedIrrefutables
                                                                                   {-# LINE 2157 "dist/build/Desugar" #-}
                                                                                   )) of
                                                                            { !_hdOforcedIrrefutables ->
                                                                            (case (({-# LINE 185 "src-ag/Desugar.ag" #-}
                                                                                    _lhsIdefs
                                                                                    {-# LINE 2162 "dist/build/Desugar" #-}
                                                                                    )) of
                                                                             { !_hdOdefs ->
                                                                             (case (({-# LINE 126 "src-ag/Desugar.ag" #-}
                                                                                     _lhsIchildSyns
                                                                                     {-# LINE 2167 "dist/build/Desugar" #-}
                                                                                     )) of
                                                                              { !_hdOchildSyns ->
                                                                              (case (({-# LINE 126 "src-ag/Desugar.ag" #-}
                                                                                      _lhsIchildInhs
                                                                                      {-# LINE 2172 "dist/build/Desugar" #-}
                                                                                      )) of
                                                                               { !_hdOchildInhs ->
                                                                               (case (hd_1 _hdOchildInhs _hdOchildSyns _hdOcon _hdOdefs _hdOforcedIrrefutables _hdOnt) of
                                                                                { ( !_hdIallAttributes,!_hdIcopy,!_hdIerrors,!_hdIoutput) ->
                                                                                    (case (({-# LINE 196 "src-ag/Desugar.ag" #-}
                                                                                            _hdIallAttributes `mergeAttributes` _tlIallAttributes
                                                                                            {-# LINE 2179 "dist/build/Desugar" #-}
                                                                                            )) of
                                                                                     { !_lhsOallAttributes ->
                                                                                     (case (({-# LINE 22 "src-ag/Patterns.ag" #-}
                                                                                             (:) _hdIcopy _tlIcopy
                                                                                             {-# LINE 2184 "dist/build/Desugar" #-}
                                                                                             )) of
                                                                                      { !_copy ->
                                                                                      (case (({-# LINE 22 "src-ag/Patterns.ag" #-}
                                                                                              _copy
                                                                                              {-# LINE 2189 "dist/build/Desugar" #-}
                                                                                              )) of
                                                                                       { !_lhsOcopy ->
                                                                                       (case (({-# LINE 38 "src-ag/Desugar.ag" #-}
                                                                                               _hdIerrors Seq.>< _tlIerrors
                                                                                               {-# LINE 2194 "dist/build/Desugar" #-}
                                                                                               )) of
                                                                                        { !_lhsOerrors ->
                                                                                        (case (({-# LINE 40 "src-ag/Desugar.ag" #-}
                                                                                                (:) _hdIoutput _tlIoutput
                                                                                                {-# LINE 2199 "dist/build/Desugar" #-}
                                                                                                )) of
                                                                                         { !_output ->
                                                                                         (case (({-# LINE 40 "src-ag/Desugar.ag" #-}
                                                                                                 _output
                                                                                                 {-# LINE 2204 "dist/build/Desugar" #-}
                                                                                                 )) of
                                                                                          { !_lhsOoutput ->
                                                                                          ( _lhsOallAttributes,_lhsOcopy,_lhsOerrors,_lhsOoutput) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })))
                                   in  sem_Patterns_Cons_1)) of
                            { ( !sem_Patterns_1) ->
                            ( _lhsOdefsCollect,sem_Patterns_1) }) }) }) }))
sem_Patterns_Nil :: T_Patterns
sem_Patterns_Nil =
    (T_Patterns (case (({-# LINE 179 "src-ag/Desugar.ag" #-}
                        Set.empty
                        {-# LINE 2215 "dist/build/Desugar" #-}
                        )) of
                 { !_lhsOdefsCollect ->
                 (case ((let sem_Patterns_Nil_1 :: T_Patterns_1
                             sem_Patterns_Nil_1 =
                                 (T_Patterns_1 (\ (!_lhsIchildInhs)
                                                  (!_lhsIchildSyns)
                                                  (!_lhsIcon)
                                                  (!_lhsIdefs)
                                                  (!_lhsIforcedIrrefutables)
                                                  (!_lhsInt) ->
                                                    (case (({-# LINE 196 "src-ag/Desugar.ag" #-}
                                                            Map.empty
                                                            {-# LINE 2228 "dist/build/Desugar" #-}
                                                            )) of
                                                     { !_lhsOallAttributes ->
                                                     (case (({-# LINE 22 "src-ag/Patterns.ag" #-}
                                                             []
                                                             {-# LINE 2233 "dist/build/Desugar" #-}
                                                             )) of
                                                      { !_copy ->
                                                      (case (({-# LINE 22 "src-ag/Patterns.ag" #-}
                                                              _copy
                                                              {-# LINE 2238 "dist/build/Desugar" #-}
                                                              )) of
                                                       { !_lhsOcopy ->
                                                       (case (({-# LINE 38 "src-ag/Desugar.ag" #-}
                                                               Seq.empty
                                                               {-# LINE 2243 "dist/build/Desugar" #-}
                                                               )) of
                                                        { !_lhsOerrors ->
                                                        (case (({-# LINE 40 "src-ag/Desugar.ag" #-}
                                                                []
                                                                {-# LINE 2248 "dist/build/Desugar" #-}
                                                                )) of
                                                         { !_output ->
                                                         (case (({-# LINE 40 "src-ag/Desugar.ag" #-}
                                                                 _output
                                                                 {-# LINE 2253 "dist/build/Desugar" #-}
                                                                 )) of
                                                          { !_lhsOoutput ->
                                                          ( _lhsOallAttributes,_lhsOcopy,_lhsOerrors,_lhsOoutput) }) }) }) }) }) })))
                         in  sem_Patterns_Nil_1)) of
                  { ( !sem_Patterns_1) ->
                  ( _lhsOdefsCollect,sem_Patterns_1) }) }))
-- Production --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         augmentsIn           : Map ConstructorIdent (Map Identifier [Expression])
         forcedIrrefutables   : AttrMap
         inhMap               : Map Identifier Attributes
         mainName             : String
         nt                   : NontermIdent
         options              : Options
         synMap               : Map Identifier Attributes
      synthesized attributes:
         allAttributes        : AttrMap
         augmentsOut          : Map ConstructorIdent (Map Identifier [Expression])
         errors               : Seq Error
         output               : Production 
   alternatives:
      alternative Production:
         child con            : {ConstructorIdent}
         child params         : {[Identifier]}
         child constraints    : {[Type]}
         child children       : Children 
         child rules          : Rules 
         child typeSigs       : TypeSigs 
         child macro          : {MaybeMacro}
         visit 0:
            local augmentsIn  : _
            local _tup3       : _
            local augmentsOut1 : _
            local augmentsOut : _
            local augmentErrs : _
            local output      : _
-}
-- cata
sem_Production :: Production ->
                  T_Production
sem_Production !(Production _con _params _constraints _children _rules _typeSigs _macro) =
    (sem_Production_Production _con _params _constraints (sem_Children _children) (sem_Rules _rules) (sem_TypeSigs _typeSigs) _macro)
-- semantic domain
newtype T_Production = T_Production ((Map ConstructorIdent (Map Identifier [Expression])) ->
                                     AttrMap ->
                                     (Map Identifier Attributes) ->
                                     String ->
                                     NontermIdent ->
                                     Options ->
                                     (Map Identifier Attributes) ->
                                     ( AttrMap,(Map ConstructorIdent (Map Identifier [Expression])),(Seq Error),Production))
data Inh_Production = Inh_Production {augmentsIn_Inh_Production :: !((Map ConstructorIdent (Map Identifier [Expression]))),forcedIrrefutables_Inh_Production :: !(AttrMap),inhMap_Inh_Production :: !((Map Identifier Attributes)),mainName_Inh_Production :: !(String),nt_Inh_Production :: !(NontermIdent),options_Inh_Production :: !(Options),synMap_Inh_Production :: !((Map Identifier Attributes))}
data Syn_Production = Syn_Production {allAttributes_Syn_Production :: !(AttrMap),augmentsOut_Syn_Production :: !((Map ConstructorIdent (Map Identifier [Expression]))),errors_Syn_Production :: !((Seq Error)),output_Syn_Production :: !(Production)}
wrap_Production :: T_Production ->
                   Inh_Production ->
                   Syn_Production
wrap_Production !(T_Production sem) !(Inh_Production _lhsIaugmentsIn _lhsIforcedIrrefutables _lhsIinhMap _lhsImainName _lhsInt _lhsIoptions _lhsIsynMap) =
    (let ( !_lhsOallAttributes,!_lhsOaugmentsOut,!_lhsOerrors,!_lhsOoutput) = sem _lhsIaugmentsIn _lhsIforcedIrrefutables _lhsIinhMap _lhsImainName _lhsInt _lhsIoptions _lhsIsynMap
     in  (Syn_Production _lhsOallAttributes _lhsOaugmentsOut _lhsOerrors _lhsOoutput))
sem_Production_Production :: ConstructorIdent ->
                             ([Identifier]) ->
                             ([Type]) ->
                             T_Children ->
                             T_Rules ->
                             T_TypeSigs ->
                             MaybeMacro ->
                             T_Production
sem_Production_Production !con_ !params_ !constraints_ !(T_Children children_) !(T_Rules rules_) !(T_TypeSigs typeSigs_) !macro_ =
    (T_Production (\ (!_lhsIaugmentsIn)
                     (!_lhsIforcedIrrefutables)
                     (!_lhsIinhMap)
                     (!_lhsImainName)
                     (!_lhsInt)
                     (!_lhsIoptions)
                     (!_lhsIsynMap) ->
                       (case (({-# LINE 152 "src-ag/Desugar.ag" #-}
                               _lhsInt
                               {-# LINE 2333 "dist/build/Desugar" #-}
                               )) of
                        { !_rulesOnt ->
                        (case (({-# LINE 161 "src-ag/Desugar.ag" #-}
                                con_
                                {-# LINE 2338 "dist/build/Desugar" #-}
                                )) of
                         { !_rulesOcon ->
                         (case (rules_) of
                          { ( !_rulesIdefsCollect,!T_Rules_1 rules_1) ->
                              (case (({-# LINE 36 "src-ag/Desugar.ag" #-}
                                      _lhsIoptions
                                      {-# LINE 2345 "dist/build/Desugar" #-}
                                      )) of
                               { !_rulesOoptions ->
                               (case (({-# LINE 215 "src-ag/Desugar.ag" #-}
                                       _lhsIforcedIrrefutables
                                       {-# LINE 2350 "dist/build/Desugar" #-}
                                       )) of
                                { !_rulesOforcedIrrefutables ->
                                (case (({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                        _lhsIsynMap
                                        {-# LINE 2355 "dist/build/Desugar" #-}
                                        )) of
                                 { !_childrenOsynMap ->
                                 (case (({-# LINE 36 "src-ag/Desugar.ag" #-}
                                         _lhsIoptions
                                         {-# LINE 2360 "dist/build/Desugar" #-}
                                         )) of
                                  { !_childrenOoptions ->
                                  (case (({-# LINE 289 "src-ag/Desugar.ag" #-}
                                          _lhsImainName
                                          {-# LINE 2365 "dist/build/Desugar" #-}
                                          )) of
                                   { !_childrenOmainName ->
                                   (case (({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                           _lhsIinhMap
                                           {-# LINE 2370 "dist/build/Desugar" #-}
                                           )) of
                                    { !_childrenOinhMap ->
                                    (case (children_ _childrenOinhMap _childrenOmainName _childrenOoptions _childrenOsynMap) of
                                     { ( !_childrenIchildInhs,!_childrenIchildSyns,!_childrenIoutput) ->
                                         (case (({-# LINE 126 "src-ag/Desugar.ag" #-}
                                                 _childrenIchildSyns
                                                 {-# LINE 2377 "dist/build/Desugar" #-}
                                                 )) of
                                          { !_rulesOchildSyns ->
                                          (case (({-# LINE 126 "src-ag/Desugar.ag" #-}
                                                  _childrenIchildInhs
                                                  {-# LINE 2382 "dist/build/Desugar" #-}
                                                  )) of
                                           { !_rulesOchildInhs ->
                                           (case (({-# LINE 188 "src-ag/Desugar.ag" #-}
                                                   _rulesIdefsCollect
                                                   {-# LINE 2387 "dist/build/Desugar" #-}
                                                   )) of
                                            { !_rulesOdefs ->
                                            (case (rules_1 _rulesOchildInhs _rulesOchildSyns _rulesOcon _rulesOdefs _rulesOforcedIrrefutables _rulesOnt _rulesOoptions) of
                                             { ( !_rulesIallAttributes,!_rulesIerrors,!_rulesIoutput) ->
                                                 (case (({-# LINE 196 "src-ag/Desugar.ag" #-}
                                                         _rulesIallAttributes
                                                         {-# LINE 2394 "dist/build/Desugar" #-}
                                                         )) of
                                                  { !_lhsOallAttributes ->
                                                  (case (({-# LINE 244 "src-ag/Desugar.ag" #-}
                                                          Map.findWithDefault Map.empty con_ _lhsIaugmentsIn
                                                          {-# LINE 2399 "dist/build/Desugar" #-}
                                                          )) of
                                                   { !_augmentsIn ->
                                                   (case (({-# LINE 247 "src-ag/Desugar.ag" #-}
                                                           Map.mapAccum (desugarExprs _lhsIoptions _lhsInt con_ _childrenIchildInhs _childrenIchildSyns) Seq.empty _augmentsIn
                                                           {-# LINE 2404 "dist/build/Desugar" #-}
                                                           )) of
                                                    { !__tup3 ->
                                                    (case (({-# LINE 247 "src-ag/Desugar.ag" #-}
                                                            __tup3
                                                            {-# LINE 2409 "dist/build/Desugar" #-}
                                                            )) of
                                                     { !(_,!_augmentsOut1) ->
                                                     (case (({-# LINE 245 "src-ag/Desugar.ag" #-}
                                                             Map.singleton con_ _augmentsOut1
                                                             {-# LINE 2414 "dist/build/Desugar" #-}
                                                             )) of
                                                      { !_augmentsOut ->
                                                      (case (({-# LINE 231 "src-ag/Desugar.ag" #-}
                                                              _augmentsOut
                                                              {-# LINE 2419 "dist/build/Desugar" #-}
                                                              )) of
                                                       { !_lhsOaugmentsOut ->
                                                       (case (({-# LINE 247 "src-ag/Desugar.ag" #-}
                                                               __tup3
                                                               {-# LINE 2424 "dist/build/Desugar" #-}
                                                               )) of
                                                        { !(!_augmentErrs,_) ->
                                                        (case (({-# LINE 283 "src-ag/Desugar.ag" #-}
                                                                _rulesIerrors Seq.>< _augmentErrs
                                                                {-# LINE 2429 "dist/build/Desugar" #-}
                                                                )) of
                                                         { !_lhsOerrors ->
                                                         (case (typeSigs_) of
                                                          { ( !_typeSigsIoutput) ->
                                                              (case (({-# LINE 40 "src-ag/Desugar.ag" #-}
                                                                      Production con_ params_ constraints_ _childrenIoutput _rulesIoutput _typeSigsIoutput macro_
                                                                      {-# LINE 2436 "dist/build/Desugar" #-}
                                                                      )) of
                                                               { !_output ->
                                                               (case (({-# LINE 40 "src-ag/Desugar.ag" #-}
                                                                       _output
                                                                       {-# LINE 2441 "dist/build/Desugar" #-}
                                                                       )) of
                                                                { !_lhsOoutput ->
                                                                (case ((Syn_Production _lhsOallAttributes _lhsOaugmentsOut _lhsOerrors _lhsOoutput)) of
                                                                 { ___node ->
                                                                 ( _lhsOallAttributes,_lhsOaugmentsOut,_lhsOerrors,_lhsOoutput) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })))
-- Productions -------------------------------------------------
{-
   visit 0:
      inherited attributes:
         augmentsIn           : Map ConstructorIdent (Map Identifier [Expression])
         forcedIrrefutables   : AttrMap
         inhMap               : Map Identifier Attributes
         mainName             : String
         nt                   : NontermIdent
         options              : Options
         synMap               : Map Identifier Attributes
      synthesized attributes:
         allAttributes        : AttrMap
         augmentsOut          : Map ConstructorIdent (Map Identifier [Expression])
         errors               : Seq Error
         output               : Productions 
   alternatives:
      alternative Cons:
         child hd             : Production 
         child tl             : Productions 
         visit 0:
            local output      : _
      alternative Nil:
         visit 0:
            local output      : _
-}
-- cata
sem_Productions :: Productions ->
                   T_Productions
sem_Productions !list =
    (Prelude.foldr sem_Productions_Cons sem_Productions_Nil (Prelude.map sem_Production list))
-- semantic domain
newtype T_Productions = T_Productions ((Map ConstructorIdent (Map Identifier [Expression])) ->
                                       AttrMap ->
                                       (Map Identifier Attributes) ->
                                       String ->
                                       NontermIdent ->
                                       Options ->
                                       (Map Identifier Attributes) ->
                                       ( AttrMap,(Map ConstructorIdent (Map Identifier [Expression])),(Seq Error),Productions))
data Inh_Productions = Inh_Productions {augmentsIn_Inh_Productions :: !((Map ConstructorIdent (Map Identifier [Expression]))),forcedIrrefutables_Inh_Productions :: !(AttrMap),inhMap_Inh_Productions :: !((Map Identifier Attributes)),mainName_Inh_Productions :: !(String),nt_Inh_Productions :: !(NontermIdent),options_Inh_Productions :: !(Options),synMap_Inh_Productions :: !((Map Identifier Attributes))}
data Syn_Productions = Syn_Productions {allAttributes_Syn_Productions :: !(AttrMap),augmentsOut_Syn_Productions :: !((Map ConstructorIdent (Map Identifier [Expression]))),errors_Syn_Productions :: !((Seq Error)),output_Syn_Productions :: !(Productions)}
wrap_Productions :: T_Productions ->
                    Inh_Productions ->
                    Syn_Productions
wrap_Productions !(T_Productions sem) !(Inh_Productions _lhsIaugmentsIn _lhsIforcedIrrefutables _lhsIinhMap _lhsImainName _lhsInt _lhsIoptions _lhsIsynMap) =
    (let ( !_lhsOallAttributes,!_lhsOaugmentsOut,!_lhsOerrors,!_lhsOoutput) = sem _lhsIaugmentsIn _lhsIforcedIrrefutables _lhsIinhMap _lhsImainName _lhsInt _lhsIoptions _lhsIsynMap
     in  (Syn_Productions _lhsOallAttributes _lhsOaugmentsOut _lhsOerrors _lhsOoutput))
sem_Productions_Cons :: T_Production ->
                        T_Productions ->
                        T_Productions
sem_Productions_Cons !(T_Production hd_) !(T_Productions tl_) =
    (T_Productions (\ (!_lhsIaugmentsIn)
                      (!_lhsIforcedIrrefutables)
                      (!_lhsIinhMap)
                      (!_lhsImainName)
                      (!_lhsInt)
                      (!_lhsIoptions)
                      (!_lhsIsynMap) ->
                        (case (({-# LINE 152 "src-ag/Desugar.ag" #-}
                                _lhsInt
                                {-# LINE 2508 "dist/build/Desugar" #-}
                                )) of
                         { !_tlOnt ->
                         (case (({-# LINE 152 "src-ag/Desugar.ag" #-}
                                 _lhsInt
                                 {-# LINE 2513 "dist/build/Desugar" #-}
                                 )) of
                          { !_hdOnt ->
                          (case (({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                  _lhsIsynMap
                                  {-# LINE 2518 "dist/build/Desugar" #-}
                                  )) of
                           { !_tlOsynMap ->
                           (case (({-# LINE 36 "src-ag/Desugar.ag" #-}
                                   _lhsIoptions
                                   {-# LINE 2523 "dist/build/Desugar" #-}
                                   )) of
                            { !_tlOoptions ->
                            (case (({-# LINE 289 "src-ag/Desugar.ag" #-}
                                    _lhsImainName
                                    {-# LINE 2528 "dist/build/Desugar" #-}
                                    )) of
                             { !_tlOmainName ->
                             (case (({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                     _lhsIinhMap
                                     {-# LINE 2533 "dist/build/Desugar" #-}
                                     )) of
                              { !_tlOinhMap ->
                              (case (({-# LINE 215 "src-ag/Desugar.ag" #-}
                                      _lhsIforcedIrrefutables
                                      {-# LINE 2538 "dist/build/Desugar" #-}
                                      )) of
                               { !_tlOforcedIrrefutables ->
                               (case (({-# LINE 230 "src-ag/Desugar.ag" #-}
                                       _lhsIaugmentsIn
                                       {-# LINE 2543 "dist/build/Desugar" #-}
                                       )) of
                                { !_tlOaugmentsIn ->
                                (case (tl_ _tlOaugmentsIn _tlOforcedIrrefutables _tlOinhMap _tlOmainName _tlOnt _tlOoptions _tlOsynMap) of
                                 { ( !_tlIallAttributes,!_tlIaugmentsOut,!_tlIerrors,!_tlIoutput) ->
                                     (case (({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                             _lhsIsynMap
                                             {-# LINE 2550 "dist/build/Desugar" #-}
                                             )) of
                                      { !_hdOsynMap ->
                                      (case (({-# LINE 36 "src-ag/Desugar.ag" #-}
                                              _lhsIoptions
                                              {-# LINE 2555 "dist/build/Desugar" #-}
                                              )) of
                                       { !_hdOoptions ->
                                       (case (({-# LINE 289 "src-ag/Desugar.ag" #-}
                                               _lhsImainName
                                               {-# LINE 2560 "dist/build/Desugar" #-}
                                               )) of
                                        { !_hdOmainName ->
                                        (case (({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                                _lhsIinhMap
                                                {-# LINE 2565 "dist/build/Desugar" #-}
                                                )) of
                                         { !_hdOinhMap ->
                                         (case (({-# LINE 215 "src-ag/Desugar.ag" #-}
                                                 _lhsIforcedIrrefutables
                                                 {-# LINE 2570 "dist/build/Desugar" #-}
                                                 )) of
                                          { !_hdOforcedIrrefutables ->
                                          (case (({-# LINE 230 "src-ag/Desugar.ag" #-}
                                                  _lhsIaugmentsIn
                                                  {-# LINE 2575 "dist/build/Desugar" #-}
                                                  )) of
                                           { !_hdOaugmentsIn ->
                                           (case (hd_ _hdOaugmentsIn _hdOforcedIrrefutables _hdOinhMap _hdOmainName _hdOnt _hdOoptions _hdOsynMap) of
                                            { ( !_hdIallAttributes,!_hdIaugmentsOut,!_hdIerrors,!_hdIoutput) ->
                                                (case (({-# LINE 196 "src-ag/Desugar.ag" #-}
                                                        _hdIallAttributes `mergeAttributes` _tlIallAttributes
                                                        {-# LINE 2582 "dist/build/Desugar" #-}
                                                        )) of
                                                 { !_lhsOallAttributes ->
                                                 (case (({-# LINE 231 "src-ag/Desugar.ag" #-}
                                                         _hdIaugmentsOut `Map.union` _tlIaugmentsOut
                                                         {-# LINE 2587 "dist/build/Desugar" #-}
                                                         )) of
                                                  { !_lhsOaugmentsOut ->
                                                  (case (({-# LINE 38 "src-ag/Desugar.ag" #-}
                                                          _hdIerrors Seq.>< _tlIerrors
                                                          {-# LINE 2592 "dist/build/Desugar" #-}
                                                          )) of
                                                   { !_lhsOerrors ->
                                                   (case (({-# LINE 40 "src-ag/Desugar.ag" #-}
                                                           (:) _hdIoutput _tlIoutput
                                                           {-# LINE 2597 "dist/build/Desugar" #-}
                                                           )) of
                                                    { !_output ->
                                                    (case (({-# LINE 40 "src-ag/Desugar.ag" #-}
                                                            _output
                                                            {-# LINE 2602 "dist/build/Desugar" #-}
                                                            )) of
                                                     { !_lhsOoutput ->
                                                     (case ((Syn_Productions _lhsOallAttributes _lhsOaugmentsOut _lhsOerrors _lhsOoutput)) of
                                                      { ___node ->
                                                      ( _lhsOallAttributes,_lhsOaugmentsOut,_lhsOerrors,_lhsOoutput) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })))
sem_Productions_Nil :: T_Productions
sem_Productions_Nil =
    (T_Productions (\ (!_lhsIaugmentsIn)
                      (!_lhsIforcedIrrefutables)
                      (!_lhsIinhMap)
                      (!_lhsImainName)
                      (!_lhsInt)
                      (!_lhsIoptions)
                      (!_lhsIsynMap) ->
                        (case (({-# LINE 196 "src-ag/Desugar.ag" #-}
                                Map.empty
                                {-# LINE 2619 "dist/build/Desugar" #-}
                                )) of
                         { !_lhsOallAttributes ->
                         (case (({-# LINE 231 "src-ag/Desugar.ag" #-}
                                 Map.empty
                                 {-# LINE 2624 "dist/build/Desugar" #-}
                                 )) of
                          { !_lhsOaugmentsOut ->
                          (case (({-# LINE 38 "src-ag/Desugar.ag" #-}
                                  Seq.empty
                                  {-# LINE 2629 "dist/build/Desugar" #-}
                                  )) of
                           { !_lhsOerrors ->
                           (case (({-# LINE 40 "src-ag/Desugar.ag" #-}
                                   []
                                   {-# LINE 2634 "dist/build/Desugar" #-}
                                   )) of
                            { !_output ->
                            (case (({-# LINE 40 "src-ag/Desugar.ag" #-}
                                    _output
                                    {-# LINE 2639 "dist/build/Desugar" #-}
                                    )) of
                             { !_lhsOoutput ->
                             (case ((Syn_Productions _lhsOallAttributes _lhsOaugmentsOut _lhsOerrors _lhsOoutput)) of
                              { ___node ->
                              ( _lhsOallAttributes,_lhsOaugmentsOut,_lhsOerrors,_lhsOoutput) }) }) }) }) }) })))
-- Rule --------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         defsCollect          : Set (Identifier, Identifier)
   visit 1:
      inherited attributes:
         childInhs            : [(Identifier, Identifier)]
         childSyns            : [(Identifier, Identifier)]
         con                  : ConstructorIdent
         defs                 : Set (Identifier, Identifier)
         forcedIrrefutables   : AttrMap
         nt                   : NontermIdent
         options              : Options
      synthesized attributes:
         allAttributes        : AttrMap
         errors               : Seq Error
         output               : Rule 
   alternatives:
      alternative Rule:
         child mbName         : {Maybe Identifier}
         child pattern        : Pattern 
         child rhs            : Expression 
         child owrt           : {Bool}
         child origin         : {String}
         child explicit       : {Bool}
         child pure           : {Bool}
         child identity       : {Bool}
         child mbError        : {Maybe Error}
         child eager          : {Bool}
         visit 1:
            local ruleDescr   : _
            local output      : _
-}
-- cata
sem_Rule :: Rule ->
            T_Rule
sem_Rule !(Rule _mbName _pattern _rhs _owrt _origin _explicit _pure _identity _mbError _eager) =
    (sem_Rule_Rule _mbName (sem_Pattern _pattern) (sem_Expression _rhs) _owrt _origin _explicit _pure _identity _mbError _eager)
-- semantic domain
newtype T_Rule = T_Rule (( (Set (Identifier, Identifier)),T_Rule_1))
newtype T_Rule_1 = T_Rule_1 (([(Identifier, Identifier)]) ->
                             ([(Identifier, Identifier)]) ->
                             ConstructorIdent ->
                             (Set (Identifier, Identifier)) ->
                             AttrMap ->
                             NontermIdent ->
                             Options ->
                             ( AttrMap,(Seq Error),Rule))
data Inh_Rule = Inh_Rule {childInhs_Inh_Rule :: !(([(Identifier, Identifier)])),childSyns_Inh_Rule :: !(([(Identifier, Identifier)])),con_Inh_Rule :: !(ConstructorIdent),defs_Inh_Rule :: !((Set (Identifier, Identifier))),forcedIrrefutables_Inh_Rule :: !(AttrMap),nt_Inh_Rule :: !(NontermIdent),options_Inh_Rule :: !(Options)}
data Syn_Rule = Syn_Rule {allAttributes_Syn_Rule :: !(AttrMap),defsCollect_Syn_Rule :: !((Set (Identifier, Identifier))),errors_Syn_Rule :: !((Seq Error)),output_Syn_Rule :: !(Rule)}
wrap_Rule :: T_Rule ->
             Inh_Rule ->
             Syn_Rule
wrap_Rule !(T_Rule sem) !(Inh_Rule _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt _lhsIoptions) =
    (let ( !_lhsOdefsCollect,!T_Rule_1 sem_1) = sem
         ( !_lhsOallAttributes,!_lhsOerrors,!_lhsOoutput) = sem_1 _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt _lhsIoptions
     in  (Syn_Rule _lhsOallAttributes _lhsOdefsCollect _lhsOerrors _lhsOoutput))
sem_Rule_Rule :: (Maybe Identifier) ->
                 T_Pattern ->
                 T_Expression ->
                 Bool ->
                 String ->
                 Bool ->
                 Bool ->
                 Bool ->
                 (Maybe Error) ->
                 Bool ->
                 T_Rule
sem_Rule_Rule !mbName_ !(T_Pattern pattern_) !(T_Expression rhs_) !owrt_ !origin_ !explicit_ !pure_ !identity_ !mbError_ !eager_ =
    (T_Rule (case (pattern_) of
             { ( !_patternIdefsCollect,!T_Pattern_1 pattern_1) ->
                 (case (({-# LINE 179 "src-ag/Desugar.ag" #-}
                         _patternIdefsCollect
                         {-# LINE 2719 "dist/build/Desugar" #-}
                         )) of
                  { !_lhsOdefsCollect ->
                  (case ((let sem_Rule_Rule_1 :: T_Rule_1
                              sem_Rule_Rule_1 =
                                  (T_Rule_1 (\ (!_lhsIchildInhs)
                                               (!_lhsIchildSyns)
                                               (!_lhsIcon)
                                               (!_lhsIdefs)
                                               (!_lhsIforcedIrrefutables)
                                               (!_lhsInt)
                                               (!_lhsIoptions) ->
                                                 (case (({-# LINE 152 "src-ag/Desugar.ag" #-}
                                                         _lhsInt
                                                         {-# LINE 2733 "dist/build/Desugar" #-}
                                                         )) of
                                                  { !_patternOnt ->
                                                  (case (({-# LINE 153 "src-ag/Desugar.ag" #-}
                                                          _lhsIcon
                                                          {-# LINE 2738 "dist/build/Desugar" #-}
                                                          )) of
                                                   { !_patternOcon ->
                                                   (case (({-# LINE 215 "src-ag/Desugar.ag" #-}
                                                           _lhsIforcedIrrefutables
                                                           {-# LINE 2743 "dist/build/Desugar" #-}
                                                           )) of
                                                    { !_patternOforcedIrrefutables ->
                                                    (case (({-# LINE 185 "src-ag/Desugar.ag" #-}
                                                            _lhsIdefs
                                                            {-# LINE 2748 "dist/build/Desugar" #-}
                                                            )) of
                                                     { !_patternOdefs ->
                                                     (case (({-# LINE 126 "src-ag/Desugar.ag" #-}
                                                             _lhsIchildSyns
                                                             {-# LINE 2753 "dist/build/Desugar" #-}
                                                             )) of
                                                      { !_patternOchildSyns ->
                                                      (case (({-# LINE 126 "src-ag/Desugar.ag" #-}
                                                              _lhsIchildInhs
                                                              {-# LINE 2758 "dist/build/Desugar" #-}
                                                              )) of
                                                       { !_patternOchildInhs ->
                                                       (case (pattern_1 _patternOchildInhs _patternOchildSyns _patternOcon _patternOdefs _patternOforcedIrrefutables _patternOnt) of
                                                        { ( !_patternIallAttributes,!_patternIcopy,!_patternIerrors,!_patternIoutput) ->
                                                            (case (({-# LINE 196 "src-ag/Desugar.ag" #-}
                                                                    _patternIallAttributes
                                                                    {-# LINE 2765 "dist/build/Desugar" #-}
                                                                    )) of
                                                             { !_lhsOallAttributes ->
                                                             (case (({-# LINE 172 "src-ag/Desugar.ag" #-}
                                                                     show _lhsInt ++ " :: " ++ show _lhsIcon ++ " :: " ++ (concat $ intersperse "," $ map (\(f,a) -> show f ++ "." ++ show a) $ Set.toList _patternIdefsCollect)
                                                                     {-# LINE 2770 "dist/build/Desugar" #-}
                                                                     )) of
                                                              { !_ruleDescr ->
                                                              (case (({-# LINE 168 "src-ag/Desugar.ag" #-}
                                                                      _ruleDescr
                                                                      {-# LINE 2775 "dist/build/Desugar" #-}
                                                                      )) of
                                                               { !_rhsOruleDescr ->
                                                               (case (({-# LINE 36 "src-ag/Desugar.ag" #-}
                                                                       _lhsIoptions
                                                                       {-# LINE 2780 "dist/build/Desugar" #-}
                                                                       )) of
                                                                { !_rhsOoptions ->
                                                                (case (({-# LINE 152 "src-ag/Desugar.ag" #-}
                                                                        _lhsInt
                                                                        {-# LINE 2785 "dist/build/Desugar" #-}
                                                                        )) of
                                                                 { !_rhsOnt ->
                                                                 (case (({-# LINE 153 "src-ag/Desugar.ag" #-}
                                                                         _lhsIcon
                                                                         {-# LINE 2790 "dist/build/Desugar" #-}
                                                                         )) of
                                                                  { !_rhsOcon ->
                                                                  (case (({-# LINE 126 "src-ag/Desugar.ag" #-}
                                                                          _lhsIchildSyns
                                                                          {-# LINE 2795 "dist/build/Desugar" #-}
                                                                          )) of
                                                                   { !_rhsOchildSyns ->
                                                                   (case (({-# LINE 126 "src-ag/Desugar.ag" #-}
                                                                           _lhsIchildInhs
                                                                           {-# LINE 2800 "dist/build/Desugar" #-}
                                                                           )) of
                                                                    { !_rhsOchildInhs ->
                                                                    (case (rhs_ _rhsOchildInhs _rhsOchildSyns _rhsOcon _rhsOnt _rhsOoptions _rhsOruleDescr) of
                                                                     { ( !_rhsIerrors,!_rhsIoutput) ->
                                                                         (case (({-# LINE 38 "src-ag/Desugar.ag" #-}
                                                                                 _patternIerrors Seq.>< _rhsIerrors
                                                                                 {-# LINE 2807 "dist/build/Desugar" #-}
                                                                                 )) of
                                                                          { !_lhsOerrors ->
                                                                          (case (({-# LINE 40 "src-ag/Desugar.ag" #-}
                                                                                  Rule mbName_ _patternIoutput _rhsIoutput owrt_ origin_ explicit_ pure_ identity_ mbError_ eager_
                                                                                  {-# LINE 2812 "dist/build/Desugar" #-}
                                                                                  )) of
                                                                           { !_output ->
                                                                           (case (({-# LINE 40 "src-ag/Desugar.ag" #-}
                                                                                   _output
                                                                                   {-# LINE 2817 "dist/build/Desugar" #-}
                                                                                   )) of
                                                                            { !_lhsOoutput ->
                                                                            ( _lhsOallAttributes,_lhsOerrors,_lhsOoutput) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })))
                          in  sem_Rule_Rule_1)) of
                   { ( !sem_Rule_1) ->
                   ( _lhsOdefsCollect,sem_Rule_1) }) }) }))
-- Rules -------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         defsCollect          : Set (Identifier, Identifier)
   visit 1:
      inherited attributes:
         childInhs            : [(Identifier, Identifier)]
         childSyns            : [(Identifier, Identifier)]
         con                  : ConstructorIdent
         defs                 : Set (Identifier, Identifier)
         forcedIrrefutables   : AttrMap
         nt                   : NontermIdent
         options              : Options
      synthesized attributes:
         allAttributes        : AttrMap
         errors               : Seq Error
         output               : Rules 
   alternatives:
      alternative Cons:
         child hd             : Rule 
         child tl             : Rules 
         visit 1:
            local output      : _
      alternative Nil:
         visit 1:
            local output      : _
-}
-- cata
sem_Rules :: Rules ->
             T_Rules
sem_Rules !list =
    (Prelude.foldr sem_Rules_Cons sem_Rules_Nil (Prelude.map sem_Rule list))
-- semantic domain
newtype T_Rules = T_Rules (( (Set (Identifier, Identifier)),T_Rules_1))
newtype T_Rules_1 = T_Rules_1 (([(Identifier, Identifier)]) ->
                               ([(Identifier, Identifier)]) ->
                               ConstructorIdent ->
                               (Set (Identifier, Identifier)) ->
                               AttrMap ->
                               NontermIdent ->
                               Options ->
                               ( AttrMap,(Seq Error),Rules))
data Inh_Rules = Inh_Rules {childInhs_Inh_Rules :: !(([(Identifier, Identifier)])),childSyns_Inh_Rules :: !(([(Identifier, Identifier)])),con_Inh_Rules :: !(ConstructorIdent),defs_Inh_Rules :: !((Set (Identifier, Identifier))),forcedIrrefutables_Inh_Rules :: !(AttrMap),nt_Inh_Rules :: !(NontermIdent),options_Inh_Rules :: !(Options)}
data Syn_Rules = Syn_Rules {allAttributes_Syn_Rules :: !(AttrMap),defsCollect_Syn_Rules :: !((Set (Identifier, Identifier))),errors_Syn_Rules :: !((Seq Error)),output_Syn_Rules :: !(Rules)}
wrap_Rules :: T_Rules ->
              Inh_Rules ->
              Syn_Rules
wrap_Rules !(T_Rules sem) !(Inh_Rules _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt _lhsIoptions) =
    (let ( !_lhsOdefsCollect,!T_Rules_1 sem_1) = sem
         ( !_lhsOallAttributes,!_lhsOerrors,!_lhsOoutput) = sem_1 _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt _lhsIoptions
     in  (Syn_Rules _lhsOallAttributes _lhsOdefsCollect _lhsOerrors _lhsOoutput))
sem_Rules_Cons :: T_Rule ->
                  T_Rules ->
                  T_Rules
sem_Rules_Cons !(T_Rule hd_) !(T_Rules tl_) =
    (T_Rules (case (tl_) of
              { ( !_tlIdefsCollect,!T_Rules_1 tl_1) ->
                  (case (hd_) of
                   { ( !_hdIdefsCollect,!T_Rule_1 hd_1) ->
                       (case (({-# LINE 179 "src-ag/Desugar.ag" #-}
                               _hdIdefsCollect `Set.union` _tlIdefsCollect
                               {-# LINE 2886 "dist/build/Desugar" #-}
                               )) of
                        { !_lhsOdefsCollect ->
                        (case ((let sem_Rules_Cons_1 :: T_Rules_1
                                    sem_Rules_Cons_1 =
                                        (T_Rules_1 (\ (!_lhsIchildInhs)
                                                      (!_lhsIchildSyns)
                                                      (!_lhsIcon)
                                                      (!_lhsIdefs)
                                                      (!_lhsIforcedIrrefutables)
                                                      (!_lhsInt)
                                                      (!_lhsIoptions) ->
                                                        (case (({-# LINE 152 "src-ag/Desugar.ag" #-}
                                                                _lhsInt
                                                                {-# LINE 2900 "dist/build/Desugar" #-}
                                                                )) of
                                                         { !_tlOnt ->
                                                         (case (({-# LINE 153 "src-ag/Desugar.ag" #-}
                                                                 _lhsIcon
                                                                 {-# LINE 2905 "dist/build/Desugar" #-}
                                                                 )) of
                                                          { !_tlOcon ->
                                                          (case (({-# LINE 152 "src-ag/Desugar.ag" #-}
                                                                  _lhsInt
                                                                  {-# LINE 2910 "dist/build/Desugar" #-}
                                                                  )) of
                                                           { !_hdOnt ->
                                                           (case (({-# LINE 153 "src-ag/Desugar.ag" #-}
                                                                   _lhsIcon
                                                                   {-# LINE 2915 "dist/build/Desugar" #-}
                                                                   )) of
                                                            { !_hdOcon ->
                                                            (case (({-# LINE 36 "src-ag/Desugar.ag" #-}
                                                                    _lhsIoptions
                                                                    {-# LINE 2920 "dist/build/Desugar" #-}
                                                                    )) of
                                                             { !_tlOoptions ->
                                                             (case (({-# LINE 215 "src-ag/Desugar.ag" #-}
                                                                     _lhsIforcedIrrefutables
                                                                     {-# LINE 2925 "dist/build/Desugar" #-}
                                                                     )) of
                                                              { !_tlOforcedIrrefutables ->
                                                              (case (({-# LINE 185 "src-ag/Desugar.ag" #-}
                                                                      _lhsIdefs
                                                                      {-# LINE 2930 "dist/build/Desugar" #-}
                                                                      )) of
                                                               { !_tlOdefs ->
                                                               (case (({-# LINE 126 "src-ag/Desugar.ag" #-}
                                                                       _lhsIchildSyns
                                                                       {-# LINE 2935 "dist/build/Desugar" #-}
                                                                       )) of
                                                                { !_tlOchildSyns ->
                                                                (case (({-# LINE 126 "src-ag/Desugar.ag" #-}
                                                                        _lhsIchildInhs
                                                                        {-# LINE 2940 "dist/build/Desugar" #-}
                                                                        )) of
                                                                 { !_tlOchildInhs ->
                                                                 (case (tl_1 _tlOchildInhs _tlOchildSyns _tlOcon _tlOdefs _tlOforcedIrrefutables _tlOnt _tlOoptions) of
                                                                  { ( !_tlIallAttributes,!_tlIerrors,!_tlIoutput) ->
                                                                      (case (({-# LINE 36 "src-ag/Desugar.ag" #-}
                                                                              _lhsIoptions
                                                                              {-# LINE 2947 "dist/build/Desugar" #-}
                                                                              )) of
                                                                       { !_hdOoptions ->
                                                                       (case (({-# LINE 215 "src-ag/Desugar.ag" #-}
                                                                               _lhsIforcedIrrefutables
                                                                               {-# LINE 2952 "dist/build/Desugar" #-}
                                                                               )) of
                                                                        { !_hdOforcedIrrefutables ->
                                                                        (case (({-# LINE 185 "src-ag/Desugar.ag" #-}
                                                                                _lhsIdefs
                                                                                {-# LINE 2957 "dist/build/Desugar" #-}
                                                                                )) of
                                                                         { !_hdOdefs ->
                                                                         (case (({-# LINE 126 "src-ag/Desugar.ag" #-}
                                                                                 _lhsIchildSyns
                                                                                 {-# LINE 2962 "dist/build/Desugar" #-}
                                                                                 )) of
                                                                          { !_hdOchildSyns ->
                                                                          (case (({-# LINE 126 "src-ag/Desugar.ag" #-}
                                                                                  _lhsIchildInhs
                                                                                  {-# LINE 2967 "dist/build/Desugar" #-}
                                                                                  )) of
                                                                           { !_hdOchildInhs ->
                                                                           (case (hd_1 _hdOchildInhs _hdOchildSyns _hdOcon _hdOdefs _hdOforcedIrrefutables _hdOnt _hdOoptions) of
                                                                            { ( !_hdIallAttributes,!_hdIerrors,!_hdIoutput) ->
                                                                                (case (({-# LINE 196 "src-ag/Desugar.ag" #-}
                                                                                        _hdIallAttributes `mergeAttributes` _tlIallAttributes
                                                                                        {-# LINE 2974 "dist/build/Desugar" #-}
                                                                                        )) of
                                                                                 { !_lhsOallAttributes ->
                                                                                 (case (({-# LINE 38 "src-ag/Desugar.ag" #-}
                                                                                         _hdIerrors Seq.>< _tlIerrors
                                                                                         {-# LINE 2979 "dist/build/Desugar" #-}
                                                                                         )) of
                                                                                  { !_lhsOerrors ->
                                                                                  (case (({-# LINE 40 "src-ag/Desugar.ag" #-}
                                                                                          (:) _hdIoutput _tlIoutput
                                                                                          {-# LINE 2984 "dist/build/Desugar" #-}
                                                                                          )) of
                                                                                   { !_output ->
                                                                                   (case (({-# LINE 40 "src-ag/Desugar.ag" #-}
                                                                                           _output
                                                                                           {-# LINE 2989 "dist/build/Desugar" #-}
                                                                                           )) of
                                                                                    { !_lhsOoutput ->
                                                                                    ( _lhsOallAttributes,_lhsOerrors,_lhsOoutput) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })))
                                in  sem_Rules_Cons_1)) of
                         { ( !sem_Rules_1) ->
                         ( _lhsOdefsCollect,sem_Rules_1) }) }) }) }))
sem_Rules_Nil :: T_Rules
sem_Rules_Nil =
    (T_Rules (case (({-# LINE 179 "src-ag/Desugar.ag" #-}
                     Set.empty
                     {-# LINE 3000 "dist/build/Desugar" #-}
                     )) of
              { !_lhsOdefsCollect ->
              (case ((let sem_Rules_Nil_1 :: T_Rules_1
                          sem_Rules_Nil_1 =
                              (T_Rules_1 (\ (!_lhsIchildInhs)
                                            (!_lhsIchildSyns)
                                            (!_lhsIcon)
                                            (!_lhsIdefs)
                                            (!_lhsIforcedIrrefutables)
                                            (!_lhsInt)
                                            (!_lhsIoptions) ->
                                              (case (({-# LINE 196 "src-ag/Desugar.ag" #-}
                                                      Map.empty
                                                      {-# LINE 3014 "dist/build/Desugar" #-}
                                                      )) of
                                               { !_lhsOallAttributes ->
                                               (case (({-# LINE 38 "src-ag/Desugar.ag" #-}
                                                       Seq.empty
                                                       {-# LINE 3019 "dist/build/Desugar" #-}
                                                       )) of
                                                { !_lhsOerrors ->
                                                (case (({-# LINE 40 "src-ag/Desugar.ag" #-}
                                                        []
                                                        {-# LINE 3024 "dist/build/Desugar" #-}
                                                        )) of
                                                 { !_output ->
                                                 (case (({-# LINE 40 "src-ag/Desugar.ag" #-}
                                                         _output
                                                         {-# LINE 3029 "dist/build/Desugar" #-}
                                                         )) of
                                                  { !_lhsOoutput ->
                                                  ( _lhsOallAttributes,_lhsOerrors,_lhsOoutput) }) }) }) })))
                      in  sem_Rules_Nil_1)) of
               { ( !sem_Rules_1) ->
               ( _lhsOdefsCollect,sem_Rules_1) }) }))
-- TypeSig -----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         output               : TypeSig 
   alternatives:
      alternative TypeSig:
         child name           : {Identifier}
         child tp             : {Type}
         visit 0:
            local output      : _
-}
-- cata
sem_TypeSig :: TypeSig ->
               T_TypeSig
sem_TypeSig !(TypeSig _name _tp) =
    (sem_TypeSig_TypeSig _name _tp)
-- semantic domain
newtype T_TypeSig = T_TypeSig (( TypeSig))
data Inh_TypeSig = Inh_TypeSig {}
data Syn_TypeSig = Syn_TypeSig {output_Syn_TypeSig :: !(TypeSig)}
wrap_TypeSig :: T_TypeSig ->
                Inh_TypeSig ->
                Syn_TypeSig
wrap_TypeSig !(T_TypeSig sem) !(Inh_TypeSig) =
    (let ( !_lhsOoutput) = sem
     in  (Syn_TypeSig _lhsOoutput))
sem_TypeSig_TypeSig :: Identifier ->
                       Type ->
                       T_TypeSig
sem_TypeSig_TypeSig !name_ !tp_ =
    (T_TypeSig (case (({-# LINE 40 "src-ag/Desugar.ag" #-}
                       TypeSig name_ tp_
                       {-# LINE 3069 "dist/build/Desugar" #-}
                       )) of
                { !_output ->
                (case (({-# LINE 40 "src-ag/Desugar.ag" #-}
                        _output
                        {-# LINE 3074 "dist/build/Desugar" #-}
                        )) of
                 { !_lhsOoutput ->
                 (case ((Syn_TypeSig _lhsOoutput)) of
                  { ___node ->
                  ( _lhsOoutput) }) }) }))
-- TypeSigs ----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         output               : TypeSigs 
   alternatives:
      alternative Cons:
         child hd             : TypeSig 
         child tl             : TypeSigs 
         visit 0:
            local output      : _
      alternative Nil:
         visit 0:
            local output      : _
-}
-- cata
sem_TypeSigs :: TypeSigs ->
                T_TypeSigs
sem_TypeSigs !list =
    (Prelude.foldr sem_TypeSigs_Cons sem_TypeSigs_Nil (Prelude.map sem_TypeSig list))
-- semantic domain
newtype T_TypeSigs = T_TypeSigs (( TypeSigs))
data Inh_TypeSigs = Inh_TypeSigs {}
data Syn_TypeSigs = Syn_TypeSigs {output_Syn_TypeSigs :: !(TypeSigs)}
wrap_TypeSigs :: T_TypeSigs ->
                 Inh_TypeSigs ->
                 Syn_TypeSigs
wrap_TypeSigs !(T_TypeSigs sem) !(Inh_TypeSigs) =
    (let ( !_lhsOoutput) = sem
     in  (Syn_TypeSigs _lhsOoutput))
sem_TypeSigs_Cons :: T_TypeSig ->
                     T_TypeSigs ->
                     T_TypeSigs
sem_TypeSigs_Cons !(T_TypeSig hd_) !(T_TypeSigs tl_) =
    (T_TypeSigs (case (tl_) of
                 { ( !_tlIoutput) ->
                     (case (hd_) of
                      { ( !_hdIoutput) ->
                          (case (({-# LINE 40 "src-ag/Desugar.ag" #-}
                                  (:) _hdIoutput _tlIoutput
                                  {-# LINE 3120 "dist/build/Desugar" #-}
                                  )) of
                           { !_output ->
                           (case (({-# LINE 40 "src-ag/Desugar.ag" #-}
                                   _output
                                   {-# LINE 3125 "dist/build/Desugar" #-}
                                   )) of
                            { !_lhsOoutput ->
                            (case ((Syn_TypeSigs _lhsOoutput)) of
                             { ___node ->
                             ( _lhsOoutput) }) }) }) }) }))
sem_TypeSigs_Nil :: T_TypeSigs
sem_TypeSigs_Nil =
    (T_TypeSigs (case (({-# LINE 40 "src-ag/Desugar.ag" #-}
                        []
                        {-# LINE 3135 "dist/build/Desugar" #-}
                        )) of
                 { !_output ->
                 (case (({-# LINE 40 "src-ag/Desugar.ag" #-}
                         _output
                         {-# LINE 3140 "dist/build/Desugar" #-}
                         )) of
                  { !_lhsOoutput ->
                  (case ((Syn_TypeSigs _lhsOoutput)) of
                   { ___node ->
                   ( _lhsOoutput) }) }) }))