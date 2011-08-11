{-# OPTIONS_GHC -XBangPatterns #-}

-- UUAGC 0.9.38.6.5 (src-ag/Desugar.ag)
module Desugar where
{-# LINE 13 "src-ag/Desugar.ag" #-}

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
{-# LINE 24 "dist/build/uuagc/uuagc-tmp/Desugar.hs" #-}

{-# LINE 2 "src-ag/AbstractSyntax.ag" #-}

-- AbstractSyntax.ag imports
import Data.Set(Set)
import Data.Map(Map)
import Patterns    (Pattern(..),Patterns)
import Expression  (Expression(..))
import CommonTypes
{-# LINE 34 "dist/build/uuagc/uuagc-tmp/Desugar.hs" #-}

{-# LINE 2 "src-ag/Patterns.ag" #-}

-- Patterns.ag imports
import UU.Scanner.Position(Pos)
import CommonTypes (ConstructorIdent,Identifier)
{-# LINE 41 "dist/build/uuagc/uuagc-tmp/Desugar.hs" #-}

{-# LINE 2 "src-ag/Expression.ag" #-}

import UU.Scanner.Position(Pos)
import HsToken
{-# LINE 47 "dist/build/uuagc/uuagc-tmp/Desugar.hs" #-}

{-# LINE 2 "src-ag/HsToken.ag" #-}

import CommonTypes
import UU.Scanner.Position(Pos)
{-# LINE 53 "dist/build/uuagc/uuagc-tmp/Desugar.hs" #-}
{-# LINE 97 "src-ag/Desugar.ag" #-}

addl :: Int -> Pos -> Pos
addl n (Pos l c f) = Pos (l+n) c f
{-# LINE 58 "dist/build/uuagc/uuagc-tmp/Desugar.hs" #-}

{-# LINE 132 "src-ag/Desugar.ag" #-}

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
{-# LINE 73 "dist/build/uuagc/uuagc-tmp/Desugar.hs" #-}

{-# LINE 203 "src-ag/Desugar.ag" #-}

mergeAttributes :: AttrMap -> AttrMap -> AttrMap
mergeAttributes = Map.unionWith $ Map.unionWith $ Set.union
{-# LINE 79 "dist/build/uuagc/uuagc-tmp/Desugar.hs" #-}

{-# LINE 250 "src-ag/Desugar.ag" #-}

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
{-# LINE 105 "dist/build/uuagc/uuagc-tmp/Desugar.hs" #-}
-- Child -------------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         childInhs            : [(Identifier, Identifier)]
         childSyns            : [(Identifier, Identifier)]
         output               : SELF 
   alternatives:
      alternative Child:
         child name           : {Identifier}
         child tp             : {Type}
         child inh            : {Attributes}
         child syn            : {Attributes}
         child virtual        : {Maybe (Maybe Type)}
         visit 0:
            local output      : _
-}
-- cata
sem_Child :: Child  ->
             T_Child 
sem_Child !(Child _name _tp _inh _syn _virtual )  =
    (sem_Child_Child _name _tp _inh _syn _virtual )
-- semantic domain
newtype T_Child  = T_Child (( ([(Identifier, Identifier)]),([(Identifier, Identifier)]),Child ))
data Inh_Child  = Inh_Child {}
data Syn_Child  = Syn_Child {childInhs_Syn_Child :: !(([(Identifier, Identifier)])),childSyns_Syn_Child :: !(([(Identifier, Identifier)])),output_Syn_Child :: !(Child )}
wrap_Child :: T_Child  ->
              Inh_Child  ->
              Syn_Child 
wrap_Child !(T_Child sem ) !(Inh_Child )  =
    (let ( !_lhsOchildInhs,!_lhsOchildSyns,!_lhsOoutput) = sem 
     in  (Syn_Child _lhsOchildInhs _lhsOchildSyns _lhsOoutput ))
sem_Child_Child :: Identifier ->
                   Type ->
                   Attributes ->
                   Attributes ->
                   (Maybe (Maybe Type)) ->
                   T_Child 
sem_Child_Child !name_ !tp_ !inh_ !syn_ !virtual_  =
    (T_Child (case (({-# LINE 129 "src-ag/Desugar.ag" #-}
                     [(i, name_) | i <- Map.keys inh_ ]
                     {-# LINE 147 "src-ag/Desugar.hs" #-}
                     )) of
              { !_lhsOchildInhs ->
              (case (({-# LINE 130 "src-ag/Desugar.ag" #-}
                      [(s, name_) | s <- Map.keys syn_ ]
                      {-# LINE 152 "src-ag/Desugar.hs" #-}
                      )) of
               { !_lhsOchildSyns ->
               (case (({-# LINE 39 "src-ag/Desugar.ag" #-}
                       Child name_ tp_ inh_ syn_ virtual_
                       {-# LINE 157 "src-ag/Desugar.hs" #-}
                       )) of
                { !_output ->
                (case (({-# LINE 39 "src-ag/Desugar.ag" #-}
                        _output
                        {-# LINE 162 "src-ag/Desugar.hs" #-}
                        )) of
                 { !_lhsOoutput ->
                 ( _lhsOchildInhs,_lhsOchildSyns,_lhsOoutput) }) }) }) }) )
-- Children ----------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         childInhs            : [(Identifier, Identifier)]
         childSyns            : [(Identifier, Identifier)]
         output               : SELF 
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
sem_Children :: Children  ->
                T_Children 
sem_Children !list  =
    (Prelude.foldr sem_Children_Cons sem_Children_Nil (Prelude.map sem_Child list) )
-- semantic domain
newtype T_Children  = T_Children (( ([(Identifier, Identifier)]),([(Identifier, Identifier)]),Children ))
data Inh_Children  = Inh_Children {}
data Syn_Children  = Syn_Children {childInhs_Syn_Children :: !(([(Identifier, Identifier)])),childSyns_Syn_Children :: !(([(Identifier, Identifier)])),output_Syn_Children :: !(Children )}
wrap_Children :: T_Children  ->
                 Inh_Children  ->
                 Syn_Children 
wrap_Children !(T_Children sem ) !(Inh_Children )  =
    (let ( !_lhsOchildInhs,!_lhsOchildSyns,!_lhsOoutput) = sem 
     in  (Syn_Children _lhsOchildInhs _lhsOchildSyns _lhsOoutput ))
sem_Children_Cons :: T_Child  ->
                     T_Children  ->
                     T_Children 
sem_Children_Cons !(T_Child hd_ ) !(T_Children tl_ )  =
    (T_Children (case (tl_ ) of
                 { ( !_tlIchildInhs,!_tlIchildSyns,!_tlIoutput) ->
                     (case (hd_ ) of
                      { ( !_hdIchildInhs,!_hdIchildSyns,!_hdIoutput) ->
                          (case (({-# LINE 124 "src-ag/Desugar.ag" #-}
                                  _hdIchildInhs ++ _tlIchildInhs
                                  {-# LINE 208 "src-ag/Desugar.hs" #-}
                                  )) of
                           { !_lhsOchildInhs ->
                           (case (({-# LINE 124 "src-ag/Desugar.ag" #-}
                                   _hdIchildSyns ++ _tlIchildSyns
                                   {-# LINE 213 "src-ag/Desugar.hs" #-}
                                   )) of
                            { !_lhsOchildSyns ->
                            (case (({-# LINE 39 "src-ag/Desugar.ag" #-}
                                    (:) _hdIoutput _tlIoutput
                                    {-# LINE 218 "src-ag/Desugar.hs" #-}
                                    )) of
                             { !_output ->
                             (case (({-# LINE 39 "src-ag/Desugar.ag" #-}
                                     _output
                                     {-# LINE 223 "src-ag/Desugar.hs" #-}
                                     )) of
                              { !_lhsOoutput ->
                              ( _lhsOchildInhs,_lhsOchildSyns,_lhsOoutput) }) }) }) }) }) }) )
sem_Children_Nil :: T_Children 
sem_Children_Nil  =
    (T_Children (case (({-# LINE 124 "src-ag/Desugar.ag" #-}
                        []
                        {-# LINE 231 "src-ag/Desugar.hs" #-}
                        )) of
                 { !_lhsOchildInhs ->
                 (case (({-# LINE 124 "src-ag/Desugar.ag" #-}
                         []
                         {-# LINE 236 "src-ag/Desugar.hs" #-}
                         )) of
                  { !_lhsOchildSyns ->
                  (case (({-# LINE 39 "src-ag/Desugar.ag" #-}
                          []
                          {-# LINE 241 "src-ag/Desugar.hs" #-}
                          )) of
                   { !_output ->
                   (case (({-# LINE 39 "src-ag/Desugar.ag" #-}
                           _output
                           {-# LINE 246 "src-ag/Desugar.hs" #-}
                           )) of
                    { !_lhsOoutput ->
                    ( _lhsOchildInhs,_lhsOchildSyns,_lhsOoutput) }) }) }) }) )
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
         output               : SELF 
   alternatives:
      alternative Expression:
         child pos            : {Pos}
         child tks            : {[HsToken]}
         visit 0:
            local _tup1       : _
            local tks'        : _
-}
-- cata
sem_Expression :: Expression  ->
                  T_Expression 
sem_Expression !(Expression _pos _tks )  =
    (sem_Expression_Expression _pos _tks )
-- semantic domain
newtype T_Expression  = T_Expression (([(Identifier, Identifier)]) ->
                                      ([(Identifier, Identifier)]) ->
                                      ConstructorIdent ->
                                      NontermIdent ->
                                      Options ->
                                      String ->
                                      ( (Seq Error),Expression ))
data Inh_Expression  = Inh_Expression {childInhs_Inh_Expression :: !(([(Identifier, Identifier)])),childSyns_Inh_Expression :: !(([(Identifier, Identifier)])),con_Inh_Expression :: !(ConstructorIdent),nt_Inh_Expression :: !(NontermIdent),options_Inh_Expression :: !(Options),ruleDescr_Inh_Expression :: !(String)}
data Syn_Expression  = Syn_Expression {errors_Syn_Expression :: !((Seq Error)),output_Syn_Expression :: !(Expression )}
wrap_Expression :: T_Expression  ->
                   Inh_Expression  ->
                   Syn_Expression 
wrap_Expression !(T_Expression sem ) !(Inh_Expression _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsInt _lhsIoptions _lhsIruleDescr )  =
    (let ( !_lhsOerrors,!_lhsOoutput) = sem _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsInt _lhsIoptions _lhsIruleDescr 
     in  (Syn_Expression _lhsOerrors _lhsOoutput ))
sem_Expression_Expression :: Pos ->
                             ([HsToken]) ->
                             T_Expression 
sem_Expression_Expression !pos_ !tks_  =
    (T_Expression (\ (!_lhsIchildInhs)
                     (!_lhsIchildSyns)
                     (!_lhsIcon)
                     (!_lhsInt)
                     (!_lhsIoptions)
                     (!_lhsIruleDescr) ->
                       (case (({-# LINE 48 "src-ag/Desugar.ag" #-}
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
                               {-# LINE 313 "src-ag/Desugar.hs" #-}
                               )) of
                        { !__tup1 ->
                        (case (({-# LINE 48 "src-ag/Desugar.ag" #-}
                                __tup1
                                {-# LINE 318 "src-ag/Desugar.hs" #-}
                                )) of
                         { !(_,!_lhsOerrors) ->
                         (case (({-# LINE 48 "src-ag/Desugar.ag" #-}
                                 __tup1
                                 {-# LINE 323 "src-ag/Desugar.hs" #-}
                                 )) of
                          { !(!_tks',_) ->
                          (case (({-# LINE 58 "src-ag/Desugar.ag" #-}
                                  Expression pos_ _tks'
                                  {-# LINE 328 "src-ag/Desugar.hs" #-}
                                  )) of
                           { !_lhsOoutput ->
                           ( _lhsOerrors,_lhsOoutput) }) }) }) })) )
-- Grammar -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         forcedIrrefutables   : AttrMap
         options              : Options
      synthesized attributes:
         allAttributes        : AttrMap
         errors               : Seq Error
         output               : SELF 
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
sem_Grammar :: Grammar  ->
               T_Grammar 
sem_Grammar !(Grammar _typeSyns _useMap _derivings _wrappers _nonts _pragmas _manualAttrOrderMap _paramMap _contextMap _quantMap _uniqueMap _augmentsMap _aroundsMap _mergeMap )  =
    (sem_Grammar_Grammar _typeSyns _useMap _derivings _wrappers (sem_Nonterminals _nonts ) _pragmas _manualAttrOrderMap _paramMap _contextMap _quantMap _uniqueMap _augmentsMap _aroundsMap _mergeMap )
-- semantic domain
newtype T_Grammar  = T_Grammar (AttrMap ->
                                Options ->
                                ( AttrMap,(Seq Error),Grammar ))
data Inh_Grammar  = Inh_Grammar {forcedIrrefutables_Inh_Grammar :: !(AttrMap),options_Inh_Grammar :: !(Options)}
data Syn_Grammar  = Syn_Grammar {allAttributes_Syn_Grammar :: !(AttrMap),errors_Syn_Grammar :: !((Seq Error)),output_Syn_Grammar :: !(Grammar )}
wrap_Grammar :: T_Grammar  ->
                Inh_Grammar  ->
                Syn_Grammar 
wrap_Grammar !(T_Grammar sem ) !(Inh_Grammar _lhsIforcedIrrefutables _lhsIoptions )  =
    (let ( !_lhsOallAttributes,!_lhsOerrors,!_lhsOoutput) = sem _lhsIforcedIrrefutables _lhsIoptions 
     in  (Syn_Grammar _lhsOallAttributes _lhsOerrors _lhsOoutput ))
sem_Grammar_Grammar :: TypeSyns ->
                       UseMap ->
                       Derivings ->
                       (Set NontermIdent) ->
                       T_Nonterminals  ->
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
sem_Grammar_Grammar !typeSyns_ !useMap_ !derivings_ !wrappers_ !(T_Nonterminals nonts_ ) !pragmas_ !manualAttrOrderMap_ !paramMap_ !contextMap_ !quantMap_ !uniqueMap_ !augmentsMap_ !aroundsMap_ !mergeMap_  =
    (T_Grammar (\ (!_lhsIforcedIrrefutables)
                  (!_lhsIoptions) ->
                    (case (({-# LINE 35 "src-ag/Desugar.ag" #-}
                            _lhsIoptions
                            {-# LINE 396 "src-ag/Desugar.hs" #-}
                            )) of
                     { !_nontsOoptions ->
                     (case (({-# LINE 214 "src-ag/Desugar.ag" #-}
                             _lhsIforcedIrrefutables
                             {-# LINE 401 "src-ag/Desugar.hs" #-}
                             )) of
                      { !_nontsOforcedIrrefutables ->
                      (case (({-# LINE 234 "src-ag/Desugar.ag" #-}
                              augmentsMap_
                              {-# LINE 406 "src-ag/Desugar.hs" #-}
                              )) of
                       { !_nontsOaugmentsIn ->
                       (case (nonts_ _nontsOaugmentsIn _nontsOforcedIrrefutables _nontsOoptions ) of
                        { ( !_nontsIallAttributes,!_nontsIaugmentsOut,!_nontsIerrors,!_nontsIoutput) ->
                            (case (({-# LINE 195 "src-ag/Desugar.ag" #-}
                                    _nontsIallAttributes
                                    {-# LINE 413 "src-ag/Desugar.hs" #-}
                                    )) of
                             { !_lhsOallAttributes ->
                             (case (({-# LINE 37 "src-ag/Desugar.ag" #-}
                                     _nontsIerrors
                                     {-# LINE 418 "src-ag/Desugar.hs" #-}
                                     )) of
                              { !_lhsOerrors ->
                              (case (({-# LINE 290 "src-ag/Desugar.ag" #-}
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
                                      {-# LINE 436 "src-ag/Desugar.hs" #-}
                                      )) of
                               { !_lhsOoutput ->
                               ( _lhsOallAttributes,_lhsOerrors,_lhsOoutput) }) }) }) }) }) }) })) )
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
         tks                  : SELF 
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
sem_HsToken :: HsToken  ->
               T_HsToken 
sem_HsToken !(AGField _field _attr _pos _rdesc )  =
    (sem_HsToken_AGField _field _attr _pos _rdesc )
sem_HsToken !(AGLocal _var _pos _rdesc )  =
    (sem_HsToken_AGLocal _var _pos _rdesc )
sem_HsToken !(CharToken _value _pos )  =
    (sem_HsToken_CharToken _value _pos )
sem_HsToken !(Err _mesg _pos )  =
    (sem_HsToken_Err _mesg _pos )
sem_HsToken !(HsToken _value _pos )  =
    (sem_HsToken_HsToken _value _pos )
sem_HsToken !(StrToken _value _pos )  =
    (sem_HsToken_StrToken _value _pos )
-- semantic domain
newtype T_HsToken  = T_HsToken (Int ->
                                ([(Identifier, Identifier)]) ->
                                ([(Identifier, Identifier)]) ->
                                ConstructorIdent ->
                                NontermIdent ->
                                String ->
                                Bool ->
                                ( Int,(Seq Error),HsToken ))
data Inh_HsToken  = Inh_HsToken {addLines_Inh_HsToken :: !(Int),childInhs_Inh_HsToken :: !(([(Identifier, Identifier)])),childSyns_Inh_HsToken :: !(([(Identifier, Identifier)])),con_Inh_HsToken :: !(ConstructorIdent),nt_Inh_HsToken :: !(NontermIdent),ruleDescr_Inh_HsToken :: !(String),useFieldIdent_Inh_HsToken :: !(Bool)}
data Syn_HsToken  = Syn_HsToken {addLines_Syn_HsToken :: !(Int),errors_Syn_HsToken :: !((Seq Error)),tks_Syn_HsToken :: !(HsToken )}
wrap_HsToken :: T_HsToken  ->
                Inh_HsToken  ->
                Syn_HsToken 
wrap_HsToken !(T_HsToken sem ) !(Inh_HsToken _lhsIaddLines _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsInt _lhsIruleDescr _lhsIuseFieldIdent )  =
    (let ( !_lhsOaddLines,!_lhsOerrors,!_lhsOtks) = sem _lhsIaddLines _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsInt _lhsIruleDescr _lhsIuseFieldIdent 
     in  (Syn_HsToken _lhsOaddLines _lhsOerrors _lhsOtks ))
sem_HsToken_AGField :: Identifier ->
                       Identifier ->
                       Pos ->
                       (Maybe String) ->
                       T_HsToken 
sem_HsToken_AGField !field_ !attr_ !pos_ !rdesc_  =
    (T_HsToken (\ (!_lhsIaddLines)
                  (!_lhsIchildInhs)
                  (!_lhsIchildSyns)
                  (!_lhsIcon)
                  (!_lhsInt)
                  (!_lhsIruleDescr)
                  (!_lhsIuseFieldIdent) ->
                    (case (({-# LINE 78 "src-ag/Desugar.ag" #-}
                            findField field_ attr_ _lhsIchildSyns
                            {-# LINE 539 "src-ag/Desugar.hs" #-}
                            )) of
                     { !_mField ->
                     (case (({-# LINE 80 "src-ag/Desugar.ag" #-}
                             maybe field_ id _mField
                             {-# LINE 544 "src-ag/Desugar.hs" #-}
                             )) of
                      { !_field' ->
                      (case (({-# LINE 83 "src-ag/Desugar.ag" #-}
                              if _lhsIuseFieldIdent || length (getName field_) < length (getName _field'    )
                              then _lhsIaddLines + 1
                              else _lhsIaddLines
                              {-# LINE 551 "src-ag/Desugar.hs" #-}
                              )) of
                       { !_lhsOaddLines ->
                       (case (({-# LINE 81 "src-ag/Desugar.ag" #-}
                               maybe (Seq.singleton (UndefAttr _lhsInt _lhsIcon field_ (Ident "<ANY>" (getPos field_)) False)) (const Seq.empty) _mField
                               {-# LINE 556 "src-ag/Desugar.hs" #-}
                               )) of
                        { !_lhsOerrors ->
                        (case (({-# LINE 87 "src-ag/Desugar.ag" #-}
                                AGField _field'     attr_ (addl _lhsIaddLines pos_) (if _lhsIuseFieldIdent then Just _lhsIruleDescr else Nothing)
                                {-# LINE 561 "src-ag/Desugar.hs" #-}
                                )) of
                         { !_tks ->
                         (case (({-# LINE 69 "src-ag/Desugar.ag" #-}
                                 _tks
                                 {-# LINE 566 "src-ag/Desugar.hs" #-}
                                 )) of
                          { !_lhsOtks ->
                          ( _lhsOaddLines,_lhsOerrors,_lhsOtks) }) }) }) }) }) })) )
sem_HsToken_AGLocal :: Identifier ->
                       Pos ->
                       (Maybe String) ->
                       T_HsToken 
sem_HsToken_AGLocal !var_ !pos_ !rdesc_  =
    (T_HsToken (\ (!_lhsIaddLines)
                  (!_lhsIchildInhs)
                  (!_lhsIchildSyns)
                  (!_lhsIcon)
                  (!_lhsInt)
                  (!_lhsIruleDescr)
                  (!_lhsIuseFieldIdent) ->
                    (case (({-# LINE 73 "src-ag/Desugar.ag" #-}
                            if _lhsIuseFieldIdent
                            then _lhsIaddLines + 1
                            else _lhsIaddLines
                            {-# LINE 586 "src-ag/Desugar.hs" #-}
                            )) of
                     { !_lhsOaddLines ->
                     (case (({-# LINE 37 "src-ag/Desugar.ag" #-}
                             Seq.empty
                             {-# LINE 591 "src-ag/Desugar.hs" #-}
                             )) of
                      { !_lhsOerrors ->
                      (case (({-# LINE 76 "src-ag/Desugar.ag" #-}
                              AGLocal var_ (addl _lhsIaddLines pos_) (if _lhsIuseFieldIdent then Just _lhsIruleDescr else Nothing)
                              {-# LINE 596 "src-ag/Desugar.hs" #-}
                              )) of
                       { !_tks ->
                       (case (({-# LINE 69 "src-ag/Desugar.ag" #-}
                               _tks
                               {-# LINE 601 "src-ag/Desugar.hs" #-}
                               )) of
                        { !_lhsOtks ->
                        ( _lhsOaddLines,_lhsOerrors,_lhsOtks) }) }) }) })) )
sem_HsToken_CharToken :: String ->
                         Pos ->
                         T_HsToken 
sem_HsToken_CharToken !value_ !pos_  =
    (T_HsToken (\ (!_lhsIaddLines)
                  (!_lhsIchildInhs)
                  (!_lhsIchildSyns)
                  (!_lhsIcon)
                  (!_lhsInt)
                  (!_lhsIruleDescr)
                  (!_lhsIuseFieldIdent) ->
                    (case (({-# LINE 63 "src-ag/Desugar.ag" #-}
                            _lhsIaddLines
                            {-# LINE 618 "src-ag/Desugar.hs" #-}
                            )) of
                     { !_lhsOaddLines ->
                     (case (({-# LINE 37 "src-ag/Desugar.ag" #-}
                             Seq.empty
                             {-# LINE 623 "src-ag/Desugar.hs" #-}
                             )) of
                      { !_lhsOerrors ->
                      (case (({-# LINE 91 "src-ag/Desugar.ag" #-}
                              CharToken value_ (addl _lhsIaddLines pos_)
                              {-# LINE 628 "src-ag/Desugar.hs" #-}
                              )) of
                       { !_tks ->
                       (case (({-# LINE 69 "src-ag/Desugar.ag" #-}
                               _tks
                               {-# LINE 633 "src-ag/Desugar.hs" #-}
                               )) of
                        { !_lhsOtks ->
                        ( _lhsOaddLines,_lhsOerrors,_lhsOtks) }) }) }) })) )
sem_HsToken_Err :: String ->
                   Pos ->
                   T_HsToken 
sem_HsToken_Err !mesg_ !pos_  =
    (T_HsToken (\ (!_lhsIaddLines)
                  (!_lhsIchildInhs)
                  (!_lhsIchildSyns)
                  (!_lhsIcon)
                  (!_lhsInt)
                  (!_lhsIruleDescr)
                  (!_lhsIuseFieldIdent) ->
                    (case (({-# LINE 63 "src-ag/Desugar.ag" #-}
                            _lhsIaddLines
                            {-# LINE 650 "src-ag/Desugar.hs" #-}
                            )) of
                     { !_lhsOaddLines ->
                     (case (({-# LINE 37 "src-ag/Desugar.ag" #-}
                             Seq.empty
                             {-# LINE 655 "src-ag/Desugar.hs" #-}
                             )) of
                      { !_lhsOerrors ->
                      (case (({-# LINE 95 "src-ag/Desugar.ag" #-}
                              Err mesg_ (addl _lhsIaddLines pos_)
                              {-# LINE 660 "src-ag/Desugar.hs" #-}
                              )) of
                       { !_tks ->
                       (case (({-# LINE 69 "src-ag/Desugar.ag" #-}
                               _tks
                               {-# LINE 665 "src-ag/Desugar.hs" #-}
                               )) of
                        { !_lhsOtks ->
                        ( _lhsOaddLines,_lhsOerrors,_lhsOtks) }) }) }) })) )
sem_HsToken_HsToken :: String ->
                       Pos ->
                       T_HsToken 
sem_HsToken_HsToken !value_ !pos_  =
    (T_HsToken (\ (!_lhsIaddLines)
                  (!_lhsIchildInhs)
                  (!_lhsIchildSyns)
                  (!_lhsIcon)
                  (!_lhsInt)
                  (!_lhsIruleDescr)
                  (!_lhsIuseFieldIdent) ->
                    (case (({-# LINE 63 "src-ag/Desugar.ag" #-}
                            _lhsIaddLines
                            {-# LINE 682 "src-ag/Desugar.hs" #-}
                            )) of
                     { !_lhsOaddLines ->
                     (case (({-# LINE 37 "src-ag/Desugar.ag" #-}
                             Seq.empty
                             {-# LINE 687 "src-ag/Desugar.hs" #-}
                             )) of
                      { !_lhsOerrors ->
                      (case (({-# LINE 89 "src-ag/Desugar.ag" #-}
                              HsToken value_ (addl _lhsIaddLines pos_)
                              {-# LINE 692 "src-ag/Desugar.hs" #-}
                              )) of
                       { !_tks ->
                       (case (({-# LINE 69 "src-ag/Desugar.ag" #-}
                               _tks
                               {-# LINE 697 "src-ag/Desugar.hs" #-}
                               )) of
                        { !_lhsOtks ->
                        ( _lhsOaddLines,_lhsOerrors,_lhsOtks) }) }) }) })) )
sem_HsToken_StrToken :: String ->
                        Pos ->
                        T_HsToken 
sem_HsToken_StrToken !value_ !pos_  =
    (T_HsToken (\ (!_lhsIaddLines)
                  (!_lhsIchildInhs)
                  (!_lhsIchildSyns)
                  (!_lhsIcon)
                  (!_lhsInt)
                  (!_lhsIruleDescr)
                  (!_lhsIuseFieldIdent) ->
                    (case (({-# LINE 63 "src-ag/Desugar.ag" #-}
                            _lhsIaddLines
                            {-# LINE 714 "src-ag/Desugar.hs" #-}
                            )) of
                     { !_lhsOaddLines ->
                     (case (({-# LINE 37 "src-ag/Desugar.ag" #-}
                             Seq.empty
                             {-# LINE 719 "src-ag/Desugar.hs" #-}
                             )) of
                      { !_lhsOerrors ->
                      (case (({-# LINE 93 "src-ag/Desugar.ag" #-}
                              StrToken value_ (addl _lhsIaddLines pos_)
                              {-# LINE 724 "src-ag/Desugar.hs" #-}
                              )) of
                       { !_tks ->
                       (case (({-# LINE 69 "src-ag/Desugar.ag" #-}
                               _tks
                               {-# LINE 729 "src-ag/Desugar.hs" #-}
                               )) of
                        { !_lhsOtks ->
                        ( _lhsOaddLines,_lhsOerrors,_lhsOtks) }) }) }) })) )
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
         tks                  : SELF 
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
sem_HsTokens :: HsTokens  ->
                T_HsTokens 
sem_HsTokens !list  =
    (Prelude.foldr sem_HsTokens_Cons sem_HsTokens_Nil (Prelude.map sem_HsToken list) )
-- semantic domain
newtype T_HsTokens  = T_HsTokens (Int ->
                                  ([(Identifier, Identifier)]) ->
                                  ([(Identifier, Identifier)]) ->
                                  ConstructorIdent ->
                                  NontermIdent ->
                                  String ->
                                  Bool ->
                                  ( Int,(Seq Error),HsTokens ))
data Inh_HsTokens  = Inh_HsTokens {addLines_Inh_HsTokens :: !(Int),childInhs_Inh_HsTokens :: !(([(Identifier, Identifier)])),childSyns_Inh_HsTokens :: !(([(Identifier, Identifier)])),con_Inh_HsTokens :: !(ConstructorIdent),nt_Inh_HsTokens :: !(NontermIdent),ruleDescr_Inh_HsTokens :: !(String),useFieldIdent_Inh_HsTokens :: !(Bool)}
data Syn_HsTokens  = Syn_HsTokens {addLines_Syn_HsTokens :: !(Int),errors_Syn_HsTokens :: !((Seq Error)),tks_Syn_HsTokens :: !(HsTokens )}
wrap_HsTokens :: T_HsTokens  ->
                 Inh_HsTokens  ->
                 Syn_HsTokens 
wrap_HsTokens !(T_HsTokens sem ) !(Inh_HsTokens _lhsIaddLines _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsInt _lhsIruleDescr _lhsIuseFieldIdent )  =
    (let ( !_lhsOaddLines,!_lhsOerrors,!_lhsOtks) = sem _lhsIaddLines _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsInt _lhsIruleDescr _lhsIuseFieldIdent 
     in  (Syn_HsTokens _lhsOaddLines _lhsOerrors _lhsOtks ))
sem_HsTokens_Cons :: T_HsToken  ->
                     T_HsTokens  ->
                     T_HsTokens 
sem_HsTokens_Cons !(T_HsToken hd_ ) !(T_HsTokens tl_ )  =
    (T_HsTokens (\ (!_lhsIaddLines)
                   (!_lhsIchildInhs)
                   (!_lhsIchildSyns)
                   (!_lhsIcon)
                   (!_lhsInt)
                   (!_lhsIruleDescr)
                   (!_lhsIuseFieldIdent) ->
                     (case (({-# LINE 61 "src-ag/Desugar.ag" #-}
                             _lhsIuseFieldIdent
                             {-# LINE 793 "src-ag/Desugar.hs" #-}
                             )) of
                      { !_tlOuseFieldIdent ->
                      (case (({-# LINE 125 "src-ag/Desugar.ag" #-}
                              _lhsIchildSyns
                              {-# LINE 798 "src-ag/Desugar.hs" #-}
                              )) of
                       { !_tlOchildSyns ->
                       (case (({-# LINE 61 "src-ag/Desugar.ag" #-}
                               _lhsIuseFieldIdent
                               {-# LINE 803 "src-ag/Desugar.hs" #-}
                               )) of
                        { !_hdOuseFieldIdent ->
                        (case (({-# LINE 125 "src-ag/Desugar.ag" #-}
                                _lhsIchildSyns
                                {-# LINE 808 "src-ag/Desugar.hs" #-}
                                )) of
                         { !_hdOchildSyns ->
                         (case (({-# LINE 63 "src-ag/Desugar.ag" #-}
                                 _lhsIaddLines
                                 {-# LINE 813 "src-ag/Desugar.hs" #-}
                                 )) of
                          { !_hdOaddLines ->
                          (case (({-# LINE 167 "src-ag/Desugar.ag" #-}
                                  _lhsIruleDescr
                                  {-# LINE 818 "src-ag/Desugar.hs" #-}
                                  )) of
                           { !_hdOruleDescr ->
                           (case (({-# LINE 151 "src-ag/Desugar.ag" #-}
                                   _lhsInt
                                   {-# LINE 823 "src-ag/Desugar.hs" #-}
                                   )) of
                            { !_hdOnt ->
                            (case (({-# LINE 152 "src-ag/Desugar.ag" #-}
                                    _lhsIcon
                                    {-# LINE 828 "src-ag/Desugar.hs" #-}
                                    )) of
                             { !_hdOcon ->
                             (case (({-# LINE 125 "src-ag/Desugar.ag" #-}
                                     _lhsIchildInhs
                                     {-# LINE 833 "src-ag/Desugar.hs" #-}
                                     )) of
                              { !_hdOchildInhs ->
                              (case (hd_ _hdOaddLines _hdOchildInhs _hdOchildSyns _hdOcon _hdOnt _hdOruleDescr _hdOuseFieldIdent ) of
                               { ( !_hdIaddLines,!_hdIerrors,!_hdItks) ->
                                   (case (({-# LINE 63 "src-ag/Desugar.ag" #-}
                                           _hdIaddLines
                                           {-# LINE 840 "src-ag/Desugar.hs" #-}
                                           )) of
                                    { !_tlOaddLines ->
                                    (case (({-# LINE 167 "src-ag/Desugar.ag" #-}
                                            _lhsIruleDescr
                                            {-# LINE 845 "src-ag/Desugar.hs" #-}
                                            )) of
                                     { !_tlOruleDescr ->
                                     (case (({-# LINE 151 "src-ag/Desugar.ag" #-}
                                             _lhsInt
                                             {-# LINE 850 "src-ag/Desugar.hs" #-}
                                             )) of
                                      { !_tlOnt ->
                                      (case (({-# LINE 152 "src-ag/Desugar.ag" #-}
                                              _lhsIcon
                                              {-# LINE 855 "src-ag/Desugar.hs" #-}
                                              )) of
                                       { !_tlOcon ->
                                       (case (({-# LINE 125 "src-ag/Desugar.ag" #-}
                                               _lhsIchildInhs
                                               {-# LINE 860 "src-ag/Desugar.hs" #-}
                                               )) of
                                        { !_tlOchildInhs ->
                                        (case (tl_ _tlOaddLines _tlOchildInhs _tlOchildSyns _tlOcon _tlOnt _tlOruleDescr _tlOuseFieldIdent ) of
                                         { ( !_tlIaddLines,!_tlIerrors,!_tlItks) ->
                                             (case (({-# LINE 63 "src-ag/Desugar.ag" #-}
                                                     _tlIaddLines
                                                     {-# LINE 867 "src-ag/Desugar.hs" #-}
                                                     )) of
                                              { !_lhsOaddLines ->
                                              (case (({-# LINE 37 "src-ag/Desugar.ag" #-}
                                                      _hdIerrors Seq.>< _tlIerrors
                                                      {-# LINE 872 "src-ag/Desugar.hs" #-}
                                                      )) of
                                               { !_lhsOerrors ->
                                               (case (({-# LINE 69 "src-ag/Desugar.ag" #-}
                                                       (:) _hdItks _tlItks
                                                       {-# LINE 877 "src-ag/Desugar.hs" #-}
                                                       )) of
                                                { !_tks ->
                                                (case (({-# LINE 69 "src-ag/Desugar.ag" #-}
                                                        _tks
                                                        {-# LINE 882 "src-ag/Desugar.hs" #-}
                                                        )) of
                                                 { !_lhsOtks ->
                                                 ( _lhsOaddLines,_lhsOerrors,_lhsOtks) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })) )
sem_HsTokens_Nil :: T_HsTokens 
sem_HsTokens_Nil  =
    (T_HsTokens (\ (!_lhsIaddLines)
                   (!_lhsIchildInhs)
                   (!_lhsIchildSyns)
                   (!_lhsIcon)
                   (!_lhsInt)
                   (!_lhsIruleDescr)
                   (!_lhsIuseFieldIdent) ->
                     (case (({-# LINE 63 "src-ag/Desugar.ag" #-}
                             _lhsIaddLines
                             {-# LINE 897 "src-ag/Desugar.hs" #-}
                             )) of
                      { !_lhsOaddLines ->
                      (case (({-# LINE 37 "src-ag/Desugar.ag" #-}
                              Seq.empty
                              {-# LINE 902 "src-ag/Desugar.hs" #-}
                              )) of
                       { !_lhsOerrors ->
                       (case (({-# LINE 69 "src-ag/Desugar.ag" #-}
                               []
                               {-# LINE 907 "src-ag/Desugar.hs" #-}
                               )) of
                        { !_tks ->
                        (case (({-# LINE 69 "src-ag/Desugar.ag" #-}
                                _tks
                                {-# LINE 912 "src-ag/Desugar.hs" #-}
                                )) of
                         { !_lhsOtks ->
                         ( _lhsOaddLines,_lhsOerrors,_lhsOtks) }) }) }) })) )
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
sem_HsTokensRoot :: HsTokensRoot  ->
                    T_HsTokensRoot 
sem_HsTokensRoot !(HsTokensRoot _tokens )  =
    (sem_HsTokensRoot_HsTokensRoot (sem_HsTokens _tokens ) )
-- semantic domain
newtype T_HsTokensRoot  = T_HsTokensRoot (([(Identifier, Identifier)]) ->
                                          ([(Identifier, Identifier)]) ->
                                          ConstructorIdent ->
                                          NontermIdent ->
                                          String ->
                                          Bool ->
                                          ( (Seq Error),([HsToken])))
data Inh_HsTokensRoot  = Inh_HsTokensRoot {childInhs_Inh_HsTokensRoot :: !(([(Identifier, Identifier)])),childSyns_Inh_HsTokensRoot :: !(([(Identifier, Identifier)])),con_Inh_HsTokensRoot :: !(ConstructorIdent),nt_Inh_HsTokensRoot :: !(NontermIdent),ruleDescr_Inh_HsTokensRoot :: !(String),useFieldIdent_Inh_HsTokensRoot :: !(Bool)}
data Syn_HsTokensRoot  = Syn_HsTokensRoot {errors_Syn_HsTokensRoot :: !((Seq Error)),tks_Syn_HsTokensRoot :: !(([HsToken]))}
wrap_HsTokensRoot :: T_HsTokensRoot  ->
                     Inh_HsTokensRoot  ->
                     Syn_HsTokensRoot 
wrap_HsTokensRoot !(T_HsTokensRoot sem ) !(Inh_HsTokensRoot _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsInt _lhsIruleDescr _lhsIuseFieldIdent )  =
    (let ( !_lhsOerrors,!_lhsOtks) = sem _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsInt _lhsIruleDescr _lhsIuseFieldIdent 
     in  (Syn_HsTokensRoot _lhsOerrors _lhsOtks ))
sem_HsTokensRoot_HsTokensRoot :: T_HsTokens  ->
                                 T_HsTokensRoot 
sem_HsTokensRoot_HsTokensRoot !(T_HsTokens tokens_ )  =
    (T_HsTokensRoot (\ (!_lhsIchildInhs)
                       (!_lhsIchildSyns)
                       (!_lhsIcon)
                       (!_lhsInt)
                       (!_lhsIruleDescr)
                       (!_lhsIuseFieldIdent) ->
                         (case (({-# LINE 151 "src-ag/Desugar.ag" #-}
                                 _lhsInt
                                 {-# LINE 965 "src-ag/Desugar.hs" #-}
                                 )) of
                          { !_tokensOnt ->
                          (case (({-# LINE 152 "src-ag/Desugar.ag" #-}
                                  _lhsIcon
                                  {-# LINE 970 "src-ag/Desugar.hs" #-}
                                  )) of
                           { !_tokensOcon ->
                           (case (({-# LINE 125 "src-ag/Desugar.ag" #-}
                                   _lhsIchildSyns
                                   {-# LINE 975 "src-ag/Desugar.hs" #-}
                                   )) of
                            { !_tokensOchildSyns ->
                            (case (({-# LINE 61 "src-ag/Desugar.ag" #-}
                                    _lhsIuseFieldIdent
                                    {-# LINE 980 "src-ag/Desugar.hs" #-}
                                    )) of
                             { !_tokensOuseFieldIdent ->
                             (case (({-# LINE 167 "src-ag/Desugar.ag" #-}
                                     _lhsIruleDescr
                                     {-# LINE 985 "src-ag/Desugar.hs" #-}
                                     )) of
                              { !_tokensOruleDescr ->
                              (case (({-# LINE 125 "src-ag/Desugar.ag" #-}
                                      _lhsIchildInhs
                                      {-# LINE 990 "src-ag/Desugar.hs" #-}
                                      )) of
                               { !_tokensOchildInhs ->
                               (case (({-# LINE 66 "src-ag/Desugar.ag" #-}
                                       0
                                       {-# LINE 995 "src-ag/Desugar.hs" #-}
                                       )) of
                                { !_tokensOaddLines ->
                                (case (tokens_ _tokensOaddLines _tokensOchildInhs _tokensOchildSyns _tokensOcon _tokensOnt _tokensOruleDescr _tokensOuseFieldIdent ) of
                                 { ( !_tokensIaddLines,!_tokensIerrors,!_tokensItks) ->
                                     (case (({-# LINE 37 "src-ag/Desugar.ag" #-}
                                             _tokensIerrors
                                             {-# LINE 1002 "src-ag/Desugar.hs" #-}
                                             )) of
                                      { !_lhsOerrors ->
                                      (case (({-# LINE 68 "src-ag/Desugar.ag" #-}
                                              _tokensItks
                                              {-# LINE 1007 "src-ag/Desugar.hs" #-}
                                              )) of
                                       { !_lhsOtks ->
                                       ( _lhsOerrors,_lhsOtks) }) }) }) }) }) }) }) }) }) })) )
-- Nonterminal -------------------------------------------------
{-
   visit 0:
      inherited attributes:
         augmentsIn           : Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))
         forcedIrrefutables   : AttrMap
         options              : Options
      synthesized attributes:
         allAttributes        : AttrMap
         augmentsOut          : Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))
         errors               : Seq Error
         output               : SELF 
   alternatives:
      alternative Nonterminal:
         child nt             : {NontermIdent}
         child params         : {[Identifier]}
         child inh            : {Attributes}
         child syn            : {Attributes}
         child prods          : Productions 
         visit 0:
            local augmentsIn  : _
            local augmentsOut : _
            local output      : _
-}
-- cata
sem_Nonterminal :: Nonterminal  ->
                   T_Nonterminal 
sem_Nonterminal !(Nonterminal _nt _params _inh _syn _prods )  =
    (sem_Nonterminal_Nonterminal _nt _params _inh _syn (sem_Productions _prods ) )
-- semantic domain
newtype T_Nonterminal  = T_Nonterminal ((Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) ->
                                        AttrMap ->
                                        Options ->
                                        ( AttrMap,(Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))),(Seq Error),Nonterminal ))
data Inh_Nonterminal  = Inh_Nonterminal {augmentsIn_Inh_Nonterminal :: !((Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression])))),forcedIrrefutables_Inh_Nonterminal :: !(AttrMap),options_Inh_Nonterminal :: !(Options)}
data Syn_Nonterminal  = Syn_Nonterminal {allAttributes_Syn_Nonterminal :: !(AttrMap),augmentsOut_Syn_Nonterminal :: !((Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression])))),errors_Syn_Nonterminal :: !((Seq Error)),output_Syn_Nonterminal :: !(Nonterminal )}
wrap_Nonterminal :: T_Nonterminal  ->
                    Inh_Nonterminal  ->
                    Syn_Nonterminal 
wrap_Nonterminal !(T_Nonterminal sem ) !(Inh_Nonterminal _lhsIaugmentsIn _lhsIforcedIrrefutables _lhsIoptions )  =
    (let ( !_lhsOallAttributes,!_lhsOaugmentsOut,!_lhsOerrors,!_lhsOoutput) = sem _lhsIaugmentsIn _lhsIforcedIrrefutables _lhsIoptions 
     in  (Syn_Nonterminal _lhsOallAttributes _lhsOaugmentsOut _lhsOerrors _lhsOoutput ))
sem_Nonterminal_Nonterminal :: NontermIdent ->
                               ([Identifier]) ->
                               Attributes ->
                               Attributes ->
                               T_Productions  ->
                               T_Nonterminal 
sem_Nonterminal_Nonterminal !nt_ !params_ !inh_ !syn_ !(T_Productions prods_ )  =
    (T_Nonterminal (\ (!_lhsIaugmentsIn)
                      (!_lhsIforcedIrrefutables)
                      (!_lhsIoptions) ->
                        (case (({-# LINE 156 "src-ag/Desugar.ag" #-}
                                nt_
                                {-# LINE 1065 "src-ag/Desugar.hs" #-}
                                )) of
                         { !_prodsOnt ->
                         (case (({-# LINE 35 "src-ag/Desugar.ag" #-}
                                 _lhsIoptions
                                 {-# LINE 1070 "src-ag/Desugar.hs" #-}
                                 )) of
                          { !_prodsOoptions ->
                          (case (({-# LINE 214 "src-ag/Desugar.ag" #-}
                                  _lhsIforcedIrrefutables
                                  {-# LINE 1075 "src-ag/Desugar.hs" #-}
                                  )) of
                           { !_prodsOforcedIrrefutables ->
                           (case (({-# LINE 238 "src-ag/Desugar.ag" #-}
                                   Map.findWithDefault Map.empty nt_ _lhsIaugmentsIn
                                   {-# LINE 1080 "src-ag/Desugar.hs" #-}
                                   )) of
                            { !_augmentsIn ->
                            (case (({-# LINE 229 "src-ag/Desugar.ag" #-}
                                    _augmentsIn
                                    {-# LINE 1085 "src-ag/Desugar.hs" #-}
                                    )) of
                             { !_prodsOaugmentsIn ->
                             (case (prods_ _prodsOaugmentsIn _prodsOforcedIrrefutables _prodsOnt _prodsOoptions ) of
                              { ( !_prodsIallAttributes,!_prodsIaugmentsOut,!_prodsIerrors,!_prodsIoutput) ->
                                  (case (({-# LINE 195 "src-ag/Desugar.ag" #-}
                                          _prodsIallAttributes
                                          {-# LINE 1092 "src-ag/Desugar.hs" #-}
                                          )) of
                                   { !_lhsOallAttributes ->
                                   (case (({-# LINE 239 "src-ag/Desugar.ag" #-}
                                           Map.singleton nt_ _prodsIaugmentsOut
                                           {-# LINE 1097 "src-ag/Desugar.hs" #-}
                                           )) of
                                    { !_augmentsOut ->
                                    (case (({-# LINE 228 "src-ag/Desugar.ag" #-}
                                            _augmentsOut
                                            {-# LINE 1102 "src-ag/Desugar.hs" #-}
                                            )) of
                                     { !_lhsOaugmentsOut ->
                                     (case (({-# LINE 37 "src-ag/Desugar.ag" #-}
                                             _prodsIerrors
                                             {-# LINE 1107 "src-ag/Desugar.hs" #-}
                                             )) of
                                      { !_lhsOerrors ->
                                      (case (({-# LINE 39 "src-ag/Desugar.ag" #-}
                                              Nonterminal nt_ params_ inh_ syn_ _prodsIoutput
                                              {-# LINE 1112 "src-ag/Desugar.hs" #-}
                                              )) of
                                       { !_output ->
                                       (case (({-# LINE 39 "src-ag/Desugar.ag" #-}
                                               _output
                                               {-# LINE 1117 "src-ag/Desugar.hs" #-}
                                               )) of
                                        { !_lhsOoutput ->
                                        ( _lhsOallAttributes,_lhsOaugmentsOut,_lhsOerrors,_lhsOoutput) }) }) }) }) }) }) }) }) }) }) }) })) )
-- Nonterminals ------------------------------------------------
{-
   visit 0:
      inherited attributes:
         augmentsIn           : Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))
         forcedIrrefutables   : AttrMap
         options              : Options
      synthesized attributes:
         allAttributes        : AttrMap
         augmentsOut          : Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))
         errors               : Seq Error
         output               : SELF 
   alternatives:
      alternative Cons:
         child hd             : Nonterminal 
         child tl             : Nonterminals 
         visit 0:
            local output      : _
      alternative Nil:
         visit 0:
            local output      : _
-}
-- cata
sem_Nonterminals :: Nonterminals  ->
                    T_Nonterminals 
sem_Nonterminals !list  =
    (Prelude.foldr sem_Nonterminals_Cons sem_Nonterminals_Nil (Prelude.map sem_Nonterminal list) )
-- semantic domain
newtype T_Nonterminals  = T_Nonterminals ((Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) ->
                                          AttrMap ->
                                          Options ->
                                          ( AttrMap,(Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))),(Seq Error),Nonterminals ))
data Inh_Nonterminals  = Inh_Nonterminals {augmentsIn_Inh_Nonterminals :: !((Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression])))),forcedIrrefutables_Inh_Nonterminals :: !(AttrMap),options_Inh_Nonterminals :: !(Options)}
data Syn_Nonterminals  = Syn_Nonterminals {allAttributes_Syn_Nonterminals :: !(AttrMap),augmentsOut_Syn_Nonterminals :: !((Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression])))),errors_Syn_Nonterminals :: !((Seq Error)),output_Syn_Nonterminals :: !(Nonterminals )}
wrap_Nonterminals :: T_Nonterminals  ->
                     Inh_Nonterminals  ->
                     Syn_Nonterminals 
wrap_Nonterminals !(T_Nonterminals sem ) !(Inh_Nonterminals _lhsIaugmentsIn _lhsIforcedIrrefutables _lhsIoptions )  =
    (let ( !_lhsOallAttributes,!_lhsOaugmentsOut,!_lhsOerrors,!_lhsOoutput) = sem _lhsIaugmentsIn _lhsIforcedIrrefutables _lhsIoptions 
     in  (Syn_Nonterminals _lhsOallAttributes _lhsOaugmentsOut _lhsOerrors _lhsOoutput ))
sem_Nonterminals_Cons :: T_Nonterminal  ->
                         T_Nonterminals  ->
                         T_Nonterminals 
sem_Nonterminals_Cons !(T_Nonterminal hd_ ) !(T_Nonterminals tl_ )  =
    (T_Nonterminals (\ (!_lhsIaugmentsIn)
                       (!_lhsIforcedIrrefutables)
                       (!_lhsIoptions) ->
                         (case (({-# LINE 35 "src-ag/Desugar.ag" #-}
                                 _lhsIoptions
                                 {-# LINE 1170 "src-ag/Desugar.hs" #-}
                                 )) of
                          { !_tlOoptions ->
                          (case (({-# LINE 214 "src-ag/Desugar.ag" #-}
                                  _lhsIforcedIrrefutables
                                  {-# LINE 1175 "src-ag/Desugar.hs" #-}
                                  )) of
                           { !_tlOforcedIrrefutables ->
                           (case (({-# LINE 227 "src-ag/Desugar.ag" #-}
                                   _lhsIaugmentsIn
                                   {-# LINE 1180 "src-ag/Desugar.hs" #-}
                                   )) of
                            { !_tlOaugmentsIn ->
                            (case (tl_ _tlOaugmentsIn _tlOforcedIrrefutables _tlOoptions ) of
                             { ( !_tlIallAttributes,!_tlIaugmentsOut,!_tlIerrors,!_tlIoutput) ->
                                 (case (({-# LINE 35 "src-ag/Desugar.ag" #-}
                                         _lhsIoptions
                                         {-# LINE 1187 "src-ag/Desugar.hs" #-}
                                         )) of
                                  { !_hdOoptions ->
                                  (case (({-# LINE 214 "src-ag/Desugar.ag" #-}
                                          _lhsIforcedIrrefutables
                                          {-# LINE 1192 "src-ag/Desugar.hs" #-}
                                          )) of
                                   { !_hdOforcedIrrefutables ->
                                   (case (({-# LINE 227 "src-ag/Desugar.ag" #-}
                                           _lhsIaugmentsIn
                                           {-# LINE 1197 "src-ag/Desugar.hs" #-}
                                           )) of
                                    { !_hdOaugmentsIn ->
                                    (case (hd_ _hdOaugmentsIn _hdOforcedIrrefutables _hdOoptions ) of
                                     { ( !_hdIallAttributes,!_hdIaugmentsOut,!_hdIerrors,!_hdIoutput) ->
                                         (case (({-# LINE 195 "src-ag/Desugar.ag" #-}
                                                 _hdIallAttributes `mergeAttributes` _tlIallAttributes
                                                 {-# LINE 1204 "src-ag/Desugar.hs" #-}
                                                 )) of
                                          { !_lhsOallAttributes ->
                                          (case (({-# LINE 228 "src-ag/Desugar.ag" #-}
                                                  _hdIaugmentsOut `Map.union` _tlIaugmentsOut
                                                  {-# LINE 1209 "src-ag/Desugar.hs" #-}
                                                  )) of
                                           { !_lhsOaugmentsOut ->
                                           (case (({-# LINE 37 "src-ag/Desugar.ag" #-}
                                                   _hdIerrors Seq.>< _tlIerrors
                                                   {-# LINE 1214 "src-ag/Desugar.hs" #-}
                                                   )) of
                                            { !_lhsOerrors ->
                                            (case (({-# LINE 39 "src-ag/Desugar.ag" #-}
                                                    (:) _hdIoutput _tlIoutput
                                                    {-# LINE 1219 "src-ag/Desugar.hs" #-}
                                                    )) of
                                             { !_output ->
                                             (case (({-# LINE 39 "src-ag/Desugar.ag" #-}
                                                     _output
                                                     {-# LINE 1224 "src-ag/Desugar.hs" #-}
                                                     )) of
                                              { !_lhsOoutput ->
                                              ( _lhsOallAttributes,_lhsOaugmentsOut,_lhsOerrors,_lhsOoutput) }) }) }) }) }) }) }) }) }) }) }) }) })) )
sem_Nonterminals_Nil :: T_Nonterminals 
sem_Nonterminals_Nil  =
    (T_Nonterminals (\ (!_lhsIaugmentsIn)
                       (!_lhsIforcedIrrefutables)
                       (!_lhsIoptions) ->
                         (case (({-# LINE 195 "src-ag/Desugar.ag" #-}
                                 Map.empty
                                 {-# LINE 1235 "src-ag/Desugar.hs" #-}
                                 )) of
                          { !_lhsOallAttributes ->
                          (case (({-# LINE 228 "src-ag/Desugar.ag" #-}
                                  Map.empty
                                  {-# LINE 1240 "src-ag/Desugar.hs" #-}
                                  )) of
                           { !_lhsOaugmentsOut ->
                           (case (({-# LINE 37 "src-ag/Desugar.ag" #-}
                                   Seq.empty
                                   {-# LINE 1245 "src-ag/Desugar.hs" #-}
                                   )) of
                            { !_lhsOerrors ->
                            (case (({-# LINE 39 "src-ag/Desugar.ag" #-}
                                    []
                                    {-# LINE 1250 "src-ag/Desugar.hs" #-}
                                    )) of
                             { !_output ->
                             (case (({-# LINE 39 "src-ag/Desugar.ag" #-}
                                     _output
                                     {-# LINE 1255 "src-ag/Desugar.hs" #-}
                                     )) of
                              { !_lhsOoutput ->
                              ( _lhsOallAttributes,_lhsOaugmentsOut,_lhsOerrors,_lhsOoutput) }) }) }) }) })) )
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
         copy                 : SELF 
         errors               : Seq Error
         output               : SELF 
   alternatives:
      alternative Alias:
         child field          : {Identifier}
         child attr           : {Identifier}
         child pat            : Pattern 
         child parts          : Patterns 
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
sem_Pattern :: Pattern  ->
               T_Pattern 
sem_Pattern !(Alias _field _attr _pat _parts )  =
    (sem_Pattern_Alias _field _attr (sem_Pattern _pat ) (sem_Patterns _parts ) )
sem_Pattern !(Constr _name _pats )  =
    (sem_Pattern_Constr _name (sem_Patterns _pats ) )
sem_Pattern !(Irrefutable _pat )  =
    (sem_Pattern_Irrefutable (sem_Pattern _pat ) )
sem_Pattern !(Product _pos _pats )  =
    (sem_Pattern_Product _pos (sem_Patterns _pats ) )
sem_Pattern !(Underscore _pos )  =
    (sem_Pattern_Underscore _pos )
-- semantic domain
newtype T_Pattern  = T_Pattern (( (Set (Identifier, Identifier)),T_Pattern_1 ))
newtype T_Pattern_1  = T_Pattern_1 (([(Identifier, Identifier)]) ->
                                    ([(Identifier, Identifier)]) ->
                                    ConstructorIdent ->
                                    (Set (Identifier, Identifier)) ->
                                    AttrMap ->
                                    NontermIdent ->
                                    ( AttrMap,Pattern ,(Seq Error),Pattern ))
data Inh_Pattern  = Inh_Pattern {childInhs_Inh_Pattern :: !(([(Identifier, Identifier)])),childSyns_Inh_Pattern :: !(([(Identifier, Identifier)])),con_Inh_Pattern :: !(ConstructorIdent),defs_Inh_Pattern :: !((Set (Identifier, Identifier))),forcedIrrefutables_Inh_Pattern :: !(AttrMap),nt_Inh_Pattern :: !(NontermIdent)}
data Syn_Pattern  = Syn_Pattern {allAttributes_Syn_Pattern :: !(AttrMap),copy_Syn_Pattern :: !(Pattern ),defsCollect_Syn_Pattern :: !((Set (Identifier, Identifier))),errors_Syn_Pattern :: !((Seq Error)),output_Syn_Pattern :: !(Pattern )}
wrap_Pattern :: T_Pattern  ->
                Inh_Pattern  ->
                Syn_Pattern 
wrap_Pattern !(T_Pattern sem ) !(Inh_Pattern _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt )  =
    (let ( !_lhsOdefsCollect,!T_Pattern_1 sem_1) = sem 
         ( !_lhsOallAttributes,!_lhsOcopy,!_lhsOerrors,!_lhsOoutput) = sem_1 _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt 
     in  (Syn_Pattern _lhsOallAttributes _lhsOcopy _lhsOdefsCollect _lhsOerrors _lhsOoutput ))
sem_Pattern_Alias :: Identifier ->
                     Identifier ->
                     T_Pattern  ->
                     T_Patterns  ->
                     T_Pattern 
sem_Pattern_Alias !field_ !attr_ !(T_Pattern pat_ ) !(T_Patterns parts_ )  =
    (T_Pattern (case (({-# LINE 181 "src-ag/Desugar.ag" #-}
                       Set.singleton (field_, attr_)
                       {-# LINE 1354 "src-ag/Desugar.hs" #-}
                       )) of
                { !_def ->
                (case (parts_ ) of
                 { ( !_partsIdefsCollect,!T_Patterns_1 parts_1) ->
                     (case (pat_ ) of
                      { ( !_patIdefsCollect,!T_Pattern_1 pat_1) ->
                          (case (({-# LINE 182 "src-ag/Desugar.ag" #-}
                                  _def     `Set.union` _patIdefsCollect `Set.union` _partsIdefsCollect
                                  {-# LINE 1363 "src-ag/Desugar.hs" #-}
                                  )) of
                           { !_lhsOdefsCollect ->
                           (case ((let sem_Pattern_Alias_1 :: T_Pattern_1 
                                       sem_Pattern_Alias_1  =
                                           (T_Pattern_1 (\ (!_lhsIchildInhs)
                                                           (!_lhsIchildSyns)
                                                           (!_lhsIcon)
                                                           (!_lhsIdefs)
                                                           (!_lhsIforcedIrrefutables)
                                                           (!_lhsInt) ->
                                                             (case (({-# LINE 151 "src-ag/Desugar.ag" #-}
                                                                     _lhsInt
                                                                     {-# LINE 1376 "src-ag/Desugar.hs" #-}
                                                                     )) of
                                                              { !_patOnt ->
                                                              (case (({-# LINE 152 "src-ag/Desugar.ag" #-}
                                                                      _lhsIcon
                                                                      {-# LINE 1381 "src-ag/Desugar.hs" #-}
                                                                      )) of
                                                               { !_patOcon ->
                                                               (case (({-# LINE 214 "src-ag/Desugar.ag" #-}
                                                                       _lhsIforcedIrrefutables
                                                                       {-# LINE 1386 "src-ag/Desugar.hs" #-}
                                                                       )) of
                                                                { !_patOforcedIrrefutables ->
                                                                (case (({-# LINE 184 "src-ag/Desugar.ag" #-}
                                                                        _lhsIdefs
                                                                        {-# LINE 1391 "src-ag/Desugar.hs" #-}
                                                                        )) of
                                                                 { !_patOdefs ->
                                                                 (case (({-# LINE 125 "src-ag/Desugar.ag" #-}
                                                                         _lhsIchildSyns
                                                                         {-# LINE 1396 "src-ag/Desugar.hs" #-}
                                                                         )) of
                                                                  { !_patOchildSyns ->
                                                                  (case (({-# LINE 125 "src-ag/Desugar.ag" #-}
                                                                          _lhsIchildInhs
                                                                          {-# LINE 1401 "src-ag/Desugar.hs" #-}
                                                                          )) of
                                                                   { !_patOchildInhs ->
                                                                   (case (pat_1 _patOchildInhs _patOchildSyns _patOcon _patOdefs _patOforcedIrrefutables _patOnt ) of
                                                                    { ( !_patIallAttributes,!_patIcopy,!_patIerrors,!_patIoutput) ->
                                                                        (case (({-# LINE 199 "src-ag/Desugar.ag" #-}
                                                                                (Map.singleton _lhsInt $ Map.singleton _lhsIcon $ Set.singleton (field_, attr_)) `mergeAttributes` _patIallAttributes
                                                                                {-# LINE 1408 "src-ag/Desugar.hs" #-}
                                                                                )) of
                                                                         { !_lhsOallAttributes ->
                                                                         (case (({-# LINE 151 "src-ag/Desugar.ag" #-}
                                                                                 _lhsInt
                                                                                 {-# LINE 1413 "src-ag/Desugar.hs" #-}
                                                                                 )) of
                                                                          { !_partsOnt ->
                                                                          (case (({-# LINE 214 "src-ag/Desugar.ag" #-}
                                                                                  _lhsIforcedIrrefutables
                                                                                  {-# LINE 1418 "src-ag/Desugar.hs" #-}
                                                                                  )) of
                                                                           { !_partsOforcedIrrefutables ->
                                                                           (case (({-# LINE 184 "src-ag/Desugar.ag" #-}
                                                                                   _lhsIdefs
                                                                                   {-# LINE 1423 "src-ag/Desugar.hs" #-}
                                                                                   )) of
                                                                            { !_partsOdefs ->
                                                                            (case (({-# LINE 152 "src-ag/Desugar.ag" #-}
                                                                                    _lhsIcon
                                                                                    {-# LINE 1428 "src-ag/Desugar.hs" #-}
                                                                                    )) of
                                                                             { !_partsOcon ->
                                                                             (case (({-# LINE 125 "src-ag/Desugar.ag" #-}
                                                                                     _lhsIchildSyns
                                                                                     {-# LINE 1433 "src-ag/Desugar.hs" #-}
                                                                                     )) of
                                                                              { !_partsOchildSyns ->
                                                                              (case (({-# LINE 125 "src-ag/Desugar.ag" #-}
                                                                                      _lhsIchildInhs
                                                                                      {-# LINE 1438 "src-ag/Desugar.hs" #-}
                                                                                      )) of
                                                                               { !_partsOchildInhs ->
                                                                               (case (parts_1 _partsOchildInhs _partsOchildSyns _partsOcon _partsOdefs _partsOforcedIrrefutables _partsOnt ) of
                                                                                { ( !_partsIallAttributes,!_partsIcopy,!_partsIerrors,!_partsIoutput) ->
                                                                                    (case (({-# LINE 23 "src-ag/Patterns.ag" #-}
                                                                                            Alias field_ attr_ _patIcopy _partsIcopy
                                                                                            {-# LINE 1445 "src-ag/Desugar.hs" #-}
                                                                                            )) of
                                                                                     { !_copy ->
                                                                                     (case (({-# LINE 23 "src-ag/Patterns.ag" #-}
                                                                                             _copy
                                                                                             {-# LINE 1450 "src-ag/Desugar.hs" #-}
                                                                                             )) of
                                                                                      { !_lhsOcopy ->
                                                                                      (case (({-# LINE 109 "src-ag/Desugar.ag" #-}
                                                                                              maybeError field_ (UndefAttr _lhsInt _lhsIcon (Ident "<ANY>" (getPos field_)) attr_ True) $
                                                                                                findField field_ attr_ _lhsIchildInhs
                                                                                              {-# LINE 1456 "src-ag/Desugar.hs" #-}
                                                                                              )) of
                                                                                       { !__tup2 ->
                                                                                       (case (({-# LINE 109 "src-ag/Desugar.ag" #-}
                                                                                               __tup2
                                                                                               {-# LINE 1461 "src-ag/Desugar.hs" #-}
                                                                                               )) of
                                                                                        { !(!_field',_) ->
                                                                                        (case (({-# LINE 111 "src-ag/Desugar.ag" #-}
                                                                                                if _field'     == field_
                                                                                                then Seq.empty
                                                                                                else if (_field'    , attr_) `Set.member` _lhsIdefs
                                                                                                     then Seq.singleton $ DupRule _lhsInt _lhsIcon field_ attr_ _field'
                                                                                                     else Seq.empty
                                                                                                {-# LINE 1470 "src-ag/Desugar.hs" #-}
                                                                                                )) of
                                                                                         { !_err2 ->
                                                                                         (case (({-# LINE 109 "src-ag/Desugar.ag" #-}
                                                                                                 __tup2
                                                                                                 {-# LINE 1475 "src-ag/Desugar.hs" #-}
                                                                                                 )) of
                                                                                          { !(_,!_err1) ->
                                                                                          (case (({-# LINE 116 "src-ag/Desugar.ag" #-}
                                                                                                  _err1     Seq.>< _err2     Seq.>< _patIerrors Seq.>< _partsIerrors
                                                                                                  {-# LINE 1480 "src-ag/Desugar.hs" #-}
                                                                                                  )) of
                                                                                           { !_lhsOerrors ->
                                                                                           (case (({-# LINE 117 "src-ag/Desugar.ag" #-}
                                                                                                   Alias _field'     attr_ _patIoutput _partsIoutput
                                                                                                   {-# LINE 1485 "src-ag/Desugar.hs" #-}
                                                                                                   )) of
                                                                                            { !_output ->
                                                                                            (case (({-# LINE 218 "src-ag/Desugar.ag" #-}
                                                                                                    if Set.member (field_, attr_) $ Map.findWithDefault Set.empty _lhsIcon $ Map.findWithDefault Map.empty _lhsInt $ _lhsIforcedIrrefutables
                                                                                                    then Irrefutable _output
                                                                                                    else _output
                                                                                                    {-# LINE 1492 "src-ag/Desugar.hs" #-}
                                                                                                    )) of
                                                                                             { !_lhsOoutput ->
                                                                                             ( _lhsOallAttributes,_lhsOcopy,_lhsOerrors,_lhsOoutput) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })) )
                                   in  sem_Pattern_Alias_1)) of
                            { ( !sem_Pattern_1) ->
                            ( _lhsOdefsCollect,sem_Pattern_1) }) }) }) }) }) )
sem_Pattern_Constr :: ConstructorIdent ->
                      T_Patterns  ->
                      T_Pattern 
sem_Pattern_Constr !name_ !(T_Patterns pats_ )  =
    (T_Pattern (case (pats_ ) of
                { ( !_patsIdefsCollect,!T_Patterns_1 pats_1) ->
                    (case (({-# LINE 178 "src-ag/Desugar.ag" #-}
                            _patsIdefsCollect
                            {-# LINE 1507 "src-ag/Desugar.hs" #-}
                            )) of
                     { !_lhsOdefsCollect ->
                     (case ((let sem_Pattern_Constr_1 :: T_Pattern_1 
                                 sem_Pattern_Constr_1  =
                                     (T_Pattern_1 (\ (!_lhsIchildInhs)
                                                     (!_lhsIchildSyns)
                                                     (!_lhsIcon)
                                                     (!_lhsIdefs)
                                                     (!_lhsIforcedIrrefutables)
                                                     (!_lhsInt) ->
                                                       (case (({-# LINE 151 "src-ag/Desugar.ag" #-}
                                                               _lhsInt
                                                               {-# LINE 1520 "src-ag/Desugar.hs" #-}
                                                               )) of
                                                        { !_patsOnt ->
                                                        (case (({-# LINE 152 "src-ag/Desugar.ag" #-}
                                                                _lhsIcon
                                                                {-# LINE 1525 "src-ag/Desugar.hs" #-}
                                                                )) of
                                                         { !_patsOcon ->
                                                         (case (({-# LINE 214 "src-ag/Desugar.ag" #-}
                                                                 _lhsIforcedIrrefutables
                                                                 {-# LINE 1530 "src-ag/Desugar.hs" #-}
                                                                 )) of
                                                          { !_patsOforcedIrrefutables ->
                                                          (case (({-# LINE 184 "src-ag/Desugar.ag" #-}
                                                                  _lhsIdefs
                                                                  {-# LINE 1535 "src-ag/Desugar.hs" #-}
                                                                  )) of
                                                           { !_patsOdefs ->
                                                           (case (({-# LINE 125 "src-ag/Desugar.ag" #-}
                                                                   _lhsIchildSyns
                                                                   {-# LINE 1540 "src-ag/Desugar.hs" #-}
                                                                   )) of
                                                            { !_patsOchildSyns ->
                                                            (case (({-# LINE 125 "src-ag/Desugar.ag" #-}
                                                                    _lhsIchildInhs
                                                                    {-# LINE 1545 "src-ag/Desugar.hs" #-}
                                                                    )) of
                                                             { !_patsOchildInhs ->
                                                             (case (pats_1 _patsOchildInhs _patsOchildSyns _patsOcon _patsOdefs _patsOforcedIrrefutables _patsOnt ) of
                                                              { ( !_patsIallAttributes,!_patsIcopy,!_patsIerrors,!_patsIoutput) ->
                                                                  (case (({-# LINE 195 "src-ag/Desugar.ag" #-}
                                                                          _patsIallAttributes
                                                                          {-# LINE 1552 "src-ag/Desugar.hs" #-}
                                                                          )) of
                                                                   { !_lhsOallAttributes ->
                                                                   (case (({-# LINE 23 "src-ag/Patterns.ag" #-}
                                                                           Constr name_ _patsIcopy
                                                                           {-# LINE 1557 "src-ag/Desugar.hs" #-}
                                                                           )) of
                                                                    { !_copy ->
                                                                    (case (({-# LINE 23 "src-ag/Patterns.ag" #-}
                                                                            _copy
                                                                            {-# LINE 1562 "src-ag/Desugar.hs" #-}
                                                                            )) of
                                                                     { !_lhsOcopy ->
                                                                     (case (({-# LINE 37 "src-ag/Desugar.ag" #-}
                                                                             _patsIerrors
                                                                             {-# LINE 1567 "src-ag/Desugar.hs" #-}
                                                                             )) of
                                                                      { !_lhsOerrors ->
                                                                      (case (({-# LINE 39 "src-ag/Desugar.ag" #-}
                                                                              Constr name_ _patsIoutput
                                                                              {-# LINE 1572 "src-ag/Desugar.hs" #-}
                                                                              )) of
                                                                       { !_output ->
                                                                       (case (({-# LINE 39 "src-ag/Desugar.ag" #-}
                                                                               _output
                                                                               {-# LINE 1577 "src-ag/Desugar.hs" #-}
                                                                               )) of
                                                                        { !_lhsOoutput ->
                                                                        ( _lhsOallAttributes,_lhsOcopy,_lhsOerrors,_lhsOoutput) }) }) }) }) }) }) }) }) }) }) }) }) })) )
                             in  sem_Pattern_Constr_1)) of
                      { ( !sem_Pattern_1) ->
                      ( _lhsOdefsCollect,sem_Pattern_1) }) }) }) )
sem_Pattern_Irrefutable :: T_Pattern  ->
                           T_Pattern 
sem_Pattern_Irrefutable !(T_Pattern pat_ )  =
    (T_Pattern (case (pat_ ) of
                { ( !_patIdefsCollect,!T_Pattern_1 pat_1) ->
                    (case (({-# LINE 178 "src-ag/Desugar.ag" #-}
                            _patIdefsCollect
                            {-# LINE 1591 "src-ag/Desugar.hs" #-}
                            )) of
                     { !_lhsOdefsCollect ->
                     (case ((let sem_Pattern_Irrefutable_1 :: T_Pattern_1 
                                 sem_Pattern_Irrefutable_1  =
                                     (T_Pattern_1 (\ (!_lhsIchildInhs)
                                                     (!_lhsIchildSyns)
                                                     (!_lhsIcon)
                                                     (!_lhsIdefs)
                                                     (!_lhsIforcedIrrefutables)
                                                     (!_lhsInt) ->
                                                       (case (({-# LINE 201 "src-ag/Desugar.ag" #-}
                                                               Map.empty
                                                               {-# LINE 1604 "src-ag/Desugar.hs" #-}
                                                               )) of
                                                        { !_lhsOallAttributes ->
                                                        (case (({-# LINE 151 "src-ag/Desugar.ag" #-}
                                                                _lhsInt
                                                                {-# LINE 1609 "src-ag/Desugar.hs" #-}
                                                                )) of
                                                         { !_patOnt ->
                                                         (case (({-# LINE 214 "src-ag/Desugar.ag" #-}
                                                                 _lhsIforcedIrrefutables
                                                                 {-# LINE 1614 "src-ag/Desugar.hs" #-}
                                                                 )) of
                                                          { !_patOforcedIrrefutables ->
                                                          (case (({-# LINE 184 "src-ag/Desugar.ag" #-}
                                                                  _lhsIdefs
                                                                  {-# LINE 1619 "src-ag/Desugar.hs" #-}
                                                                  )) of
                                                           { !_patOdefs ->
                                                           (case (({-# LINE 152 "src-ag/Desugar.ag" #-}
                                                                   _lhsIcon
                                                                   {-# LINE 1624 "src-ag/Desugar.hs" #-}
                                                                   )) of
                                                            { !_patOcon ->
                                                            (case (({-# LINE 125 "src-ag/Desugar.ag" #-}
                                                                    _lhsIchildSyns
                                                                    {-# LINE 1629 "src-ag/Desugar.hs" #-}
                                                                    )) of
                                                             { !_patOchildSyns ->
                                                             (case (({-# LINE 125 "src-ag/Desugar.ag" #-}
                                                                     _lhsIchildInhs
                                                                     {-# LINE 1634 "src-ag/Desugar.hs" #-}
                                                                     )) of
                                                              { !_patOchildInhs ->
                                                              (case (pat_1 _patOchildInhs _patOchildSyns _patOcon _patOdefs _patOforcedIrrefutables _patOnt ) of
                                                               { ( !_patIallAttributes,!_patIcopy,!_patIerrors,!_patIoutput) ->
                                                                   (case (({-# LINE 23 "src-ag/Patterns.ag" #-}
                                                                           Irrefutable _patIcopy
                                                                           {-# LINE 1641 "src-ag/Desugar.hs" #-}
                                                                           )) of
                                                                    { !_copy ->
                                                                    (case (({-# LINE 23 "src-ag/Patterns.ag" #-}
                                                                            _copy
                                                                            {-# LINE 1646 "src-ag/Desugar.hs" #-}
                                                                            )) of
                                                                     { !_lhsOcopy ->
                                                                     (case (({-# LINE 37 "src-ag/Desugar.ag" #-}
                                                                             _patIerrors
                                                                             {-# LINE 1651 "src-ag/Desugar.hs" #-}
                                                                             )) of
                                                                      { !_lhsOerrors ->
                                                                      (case (({-# LINE 39 "src-ag/Desugar.ag" #-}
                                                                              Irrefutable _patIoutput
                                                                              {-# LINE 1656 "src-ag/Desugar.hs" #-}
                                                                              )) of
                                                                       { !_output ->
                                                                       (case (({-# LINE 39 "src-ag/Desugar.ag" #-}
                                                                               _output
                                                                               {-# LINE 1661 "src-ag/Desugar.hs" #-}
                                                                               )) of
                                                                        { !_lhsOoutput ->
                                                                        ( _lhsOallAttributes,_lhsOcopy,_lhsOerrors,_lhsOoutput) }) }) }) }) }) }) }) }) }) }) }) }) })) )
                             in  sem_Pattern_Irrefutable_1)) of
                      { ( !sem_Pattern_1) ->
                      ( _lhsOdefsCollect,sem_Pattern_1) }) }) }) )
sem_Pattern_Product :: Pos ->
                       T_Patterns  ->
                       T_Pattern 
sem_Pattern_Product !pos_ !(T_Patterns pats_ )  =
    (T_Pattern (case (pats_ ) of
                { ( !_patsIdefsCollect,!T_Patterns_1 pats_1) ->
                    (case (({-# LINE 178 "src-ag/Desugar.ag" #-}
                            _patsIdefsCollect
                            {-# LINE 1676 "src-ag/Desugar.hs" #-}
                            )) of
                     { !_lhsOdefsCollect ->
                     (case ((let sem_Pattern_Product_1 :: T_Pattern_1 
                                 sem_Pattern_Product_1  =
                                     (T_Pattern_1 (\ (!_lhsIchildInhs)
                                                     (!_lhsIchildSyns)
                                                     (!_lhsIcon)
                                                     (!_lhsIdefs)
                                                     (!_lhsIforcedIrrefutables)
                                                     (!_lhsInt) ->
                                                       (case (({-# LINE 151 "src-ag/Desugar.ag" #-}
                                                               _lhsInt
                                                               {-# LINE 1689 "src-ag/Desugar.hs" #-}
                                                               )) of
                                                        { !_patsOnt ->
                                                        (case (({-# LINE 152 "src-ag/Desugar.ag" #-}
                                                                _lhsIcon
                                                                {-# LINE 1694 "src-ag/Desugar.hs" #-}
                                                                )) of
                                                         { !_patsOcon ->
                                                         (case (({-# LINE 214 "src-ag/Desugar.ag" #-}
                                                                 _lhsIforcedIrrefutables
                                                                 {-# LINE 1699 "src-ag/Desugar.hs" #-}
                                                                 )) of
                                                          { !_patsOforcedIrrefutables ->
                                                          (case (({-# LINE 184 "src-ag/Desugar.ag" #-}
                                                                  _lhsIdefs
                                                                  {-# LINE 1704 "src-ag/Desugar.hs" #-}
                                                                  )) of
                                                           { !_patsOdefs ->
                                                           (case (({-# LINE 125 "src-ag/Desugar.ag" #-}
                                                                   _lhsIchildSyns
                                                                   {-# LINE 1709 "src-ag/Desugar.hs" #-}
                                                                   )) of
                                                            { !_patsOchildSyns ->
                                                            (case (({-# LINE 125 "src-ag/Desugar.ag" #-}
                                                                    _lhsIchildInhs
                                                                    {-# LINE 1714 "src-ag/Desugar.hs" #-}
                                                                    )) of
                                                             { !_patsOchildInhs ->
                                                             (case (pats_1 _patsOchildInhs _patsOchildSyns _patsOcon _patsOdefs _patsOforcedIrrefutables _patsOnt ) of
                                                              { ( !_patsIallAttributes,!_patsIcopy,!_patsIerrors,!_patsIoutput) ->
                                                                  (case (({-# LINE 195 "src-ag/Desugar.ag" #-}
                                                                          _patsIallAttributes
                                                                          {-# LINE 1721 "src-ag/Desugar.hs" #-}
                                                                          )) of
                                                                   { !_lhsOallAttributes ->
                                                                   (case (({-# LINE 23 "src-ag/Patterns.ag" #-}
                                                                           Product pos_ _patsIcopy
                                                                           {-# LINE 1726 "src-ag/Desugar.hs" #-}
                                                                           )) of
                                                                    { !_copy ->
                                                                    (case (({-# LINE 23 "src-ag/Patterns.ag" #-}
                                                                            _copy
                                                                            {-# LINE 1731 "src-ag/Desugar.hs" #-}
                                                                            )) of
                                                                     { !_lhsOcopy ->
                                                                     (case (({-# LINE 37 "src-ag/Desugar.ag" #-}
                                                                             _patsIerrors
                                                                             {-# LINE 1736 "src-ag/Desugar.hs" #-}
                                                                             )) of
                                                                      { !_lhsOerrors ->
                                                                      (case (({-# LINE 39 "src-ag/Desugar.ag" #-}
                                                                              Product pos_ _patsIoutput
                                                                              {-# LINE 1741 "src-ag/Desugar.hs" #-}
                                                                              )) of
                                                                       { !_output ->
                                                                       (case (({-# LINE 39 "src-ag/Desugar.ag" #-}
                                                                               _output
                                                                               {-# LINE 1746 "src-ag/Desugar.hs" #-}
                                                                               )) of
                                                                        { !_lhsOoutput ->
                                                                        ( _lhsOallAttributes,_lhsOcopy,_lhsOerrors,_lhsOoutput) }) }) }) }) }) }) }) }) }) }) }) }) })) )
                             in  sem_Pattern_Product_1)) of
                      { ( !sem_Pattern_1) ->
                      ( _lhsOdefsCollect,sem_Pattern_1) }) }) }) )
sem_Pattern_Underscore :: Pos ->
                          T_Pattern 
sem_Pattern_Underscore !pos_  =
    (T_Pattern (case (({-# LINE 178 "src-ag/Desugar.ag" #-}
                       Set.empty
                       {-# LINE 1758 "src-ag/Desugar.hs" #-}
                       )) of
                { !_lhsOdefsCollect ->
                (case ((let sem_Pattern_Underscore_1 :: T_Pattern_1 
                            sem_Pattern_Underscore_1  =
                                (T_Pattern_1 (\ (!_lhsIchildInhs)
                                                (!_lhsIchildSyns)
                                                (!_lhsIcon)
                                                (!_lhsIdefs)
                                                (!_lhsIforcedIrrefutables)
                                                (!_lhsInt) ->
                                                  (case (({-# LINE 195 "src-ag/Desugar.ag" #-}
                                                          Map.empty
                                                          {-# LINE 1771 "src-ag/Desugar.hs" #-}
                                                          )) of
                                                   { !_lhsOallAttributes ->
                                                   (case (({-# LINE 23 "src-ag/Patterns.ag" #-}
                                                           Underscore pos_
                                                           {-# LINE 1776 "src-ag/Desugar.hs" #-}
                                                           )) of
                                                    { !_copy ->
                                                    (case (({-# LINE 23 "src-ag/Patterns.ag" #-}
                                                            _copy
                                                            {-# LINE 1781 "src-ag/Desugar.hs" #-}
                                                            )) of
                                                     { !_lhsOcopy ->
                                                     (case (({-# LINE 37 "src-ag/Desugar.ag" #-}
                                                             Seq.empty
                                                             {-# LINE 1786 "src-ag/Desugar.hs" #-}
                                                             )) of
                                                      { !_lhsOerrors ->
                                                      (case (({-# LINE 39 "src-ag/Desugar.ag" #-}
                                                              Underscore pos_
                                                              {-# LINE 1791 "src-ag/Desugar.hs" #-}
                                                              )) of
                                                       { !_output ->
                                                       (case (({-# LINE 39 "src-ag/Desugar.ag" #-}
                                                               _output
                                                               {-# LINE 1796 "src-ag/Desugar.hs" #-}
                                                               )) of
                                                        { !_lhsOoutput ->
                                                        ( _lhsOallAttributes,_lhsOcopy,_lhsOerrors,_lhsOoutput) }) }) }) }) }) })) )
                        in  sem_Pattern_Underscore_1)) of
                 { ( !sem_Pattern_1) ->
                 ( _lhsOdefsCollect,sem_Pattern_1) }) }) )
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
         copy                 : SELF 
         errors               : Seq Error
         output               : SELF 
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
sem_Patterns :: Patterns  ->
                T_Patterns 
sem_Patterns !list  =
    (Prelude.foldr sem_Patterns_Cons sem_Patterns_Nil (Prelude.map sem_Pattern list) )
-- semantic domain
newtype T_Patterns  = T_Patterns (( (Set (Identifier, Identifier)),T_Patterns_1 ))
newtype T_Patterns_1  = T_Patterns_1 (([(Identifier, Identifier)]) ->
                                      ([(Identifier, Identifier)]) ->
                                      ConstructorIdent ->
                                      (Set (Identifier, Identifier)) ->
                                      AttrMap ->
                                      NontermIdent ->
                                      ( AttrMap,Patterns ,(Seq Error),Patterns ))
data Inh_Patterns  = Inh_Patterns {childInhs_Inh_Patterns :: !(([(Identifier, Identifier)])),childSyns_Inh_Patterns :: !(([(Identifier, Identifier)])),con_Inh_Patterns :: !(ConstructorIdent),defs_Inh_Patterns :: !((Set (Identifier, Identifier))),forcedIrrefutables_Inh_Patterns :: !(AttrMap),nt_Inh_Patterns :: !(NontermIdent)}
data Syn_Patterns  = Syn_Patterns {allAttributes_Syn_Patterns :: !(AttrMap),copy_Syn_Patterns :: !(Patterns ),defsCollect_Syn_Patterns :: !((Set (Identifier, Identifier))),errors_Syn_Patterns :: !((Seq Error)),output_Syn_Patterns :: !(Patterns )}
wrap_Patterns :: T_Patterns  ->
                 Inh_Patterns  ->
                 Syn_Patterns 
wrap_Patterns !(T_Patterns sem ) !(Inh_Patterns _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt )  =
    (let ( !_lhsOdefsCollect,!T_Patterns_1 sem_1) = sem 
         ( !_lhsOallAttributes,!_lhsOcopy,!_lhsOerrors,!_lhsOoutput) = sem_1 _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt 
     in  (Syn_Patterns _lhsOallAttributes _lhsOcopy _lhsOdefsCollect _lhsOerrors _lhsOoutput ))
sem_Patterns_Cons :: T_Pattern  ->
                     T_Patterns  ->
                     T_Patterns 
sem_Patterns_Cons !(T_Pattern hd_ ) !(T_Patterns tl_ )  =
    (T_Patterns (case (tl_ ) of
                 { ( !_tlIdefsCollect,!T_Patterns_1 tl_1) ->
                     (case (hd_ ) of
                      { ( !_hdIdefsCollect,!T_Pattern_1 hd_1) ->
                          (case (({-# LINE 178 "src-ag/Desugar.ag" #-}
                                  _hdIdefsCollect `Set.union` _tlIdefsCollect
                                  {-# LINE 1866 "src-ag/Desugar.hs" #-}
                                  )) of
                           { !_lhsOdefsCollect ->
                           (case ((let sem_Patterns_Cons_1 :: T_Patterns_1 
                                       sem_Patterns_Cons_1  =
                                           (T_Patterns_1 (\ (!_lhsIchildInhs)
                                                            (!_lhsIchildSyns)
                                                            (!_lhsIcon)
                                                            (!_lhsIdefs)
                                                            (!_lhsIforcedIrrefutables)
                                                            (!_lhsInt) ->
                                                              (case (({-# LINE 151 "src-ag/Desugar.ag" #-}
                                                                      _lhsInt
                                                                      {-# LINE 1879 "src-ag/Desugar.hs" #-}
                                                                      )) of
                                                               { !_tlOnt ->
                                                               (case (({-# LINE 152 "src-ag/Desugar.ag" #-}
                                                                       _lhsIcon
                                                                       {-# LINE 1884 "src-ag/Desugar.hs" #-}
                                                                       )) of
                                                                { !_tlOcon ->
                                                                (case (({-# LINE 151 "src-ag/Desugar.ag" #-}
                                                                        _lhsInt
                                                                        {-# LINE 1889 "src-ag/Desugar.hs" #-}
                                                                        )) of
                                                                 { !_hdOnt ->
                                                                 (case (({-# LINE 152 "src-ag/Desugar.ag" #-}
                                                                         _lhsIcon
                                                                         {-# LINE 1894 "src-ag/Desugar.hs" #-}
                                                                         )) of
                                                                  { !_hdOcon ->
                                                                  (case (({-# LINE 214 "src-ag/Desugar.ag" #-}
                                                                          _lhsIforcedIrrefutables
                                                                          {-# LINE 1899 "src-ag/Desugar.hs" #-}
                                                                          )) of
                                                                   { !_tlOforcedIrrefutables ->
                                                                   (case (({-# LINE 184 "src-ag/Desugar.ag" #-}
                                                                           _lhsIdefs
                                                                           {-# LINE 1904 "src-ag/Desugar.hs" #-}
                                                                           )) of
                                                                    { !_tlOdefs ->
                                                                    (case (({-# LINE 125 "src-ag/Desugar.ag" #-}
                                                                            _lhsIchildSyns
                                                                            {-# LINE 1909 "src-ag/Desugar.hs" #-}
                                                                            )) of
                                                                     { !_tlOchildSyns ->
                                                                     (case (({-# LINE 125 "src-ag/Desugar.ag" #-}
                                                                             _lhsIchildInhs
                                                                             {-# LINE 1914 "src-ag/Desugar.hs" #-}
                                                                             )) of
                                                                      { !_tlOchildInhs ->
                                                                      (case (tl_1 _tlOchildInhs _tlOchildSyns _tlOcon _tlOdefs _tlOforcedIrrefutables _tlOnt ) of
                                                                       { ( !_tlIallAttributes,!_tlIcopy,!_tlIerrors,!_tlIoutput) ->
                                                                           (case (({-# LINE 214 "src-ag/Desugar.ag" #-}
                                                                                   _lhsIforcedIrrefutables
                                                                                   {-# LINE 1921 "src-ag/Desugar.hs" #-}
                                                                                   )) of
                                                                            { !_hdOforcedIrrefutables ->
                                                                            (case (({-# LINE 184 "src-ag/Desugar.ag" #-}
                                                                                    _lhsIdefs
                                                                                    {-# LINE 1926 "src-ag/Desugar.hs" #-}
                                                                                    )) of
                                                                             { !_hdOdefs ->
                                                                             (case (({-# LINE 125 "src-ag/Desugar.ag" #-}
                                                                                     _lhsIchildSyns
                                                                                     {-# LINE 1931 "src-ag/Desugar.hs" #-}
                                                                                     )) of
                                                                              { !_hdOchildSyns ->
                                                                              (case (({-# LINE 125 "src-ag/Desugar.ag" #-}
                                                                                      _lhsIchildInhs
                                                                                      {-# LINE 1936 "src-ag/Desugar.hs" #-}
                                                                                      )) of
                                                                               { !_hdOchildInhs ->
                                                                               (case (hd_1 _hdOchildInhs _hdOchildSyns _hdOcon _hdOdefs _hdOforcedIrrefutables _hdOnt ) of
                                                                                { ( !_hdIallAttributes,!_hdIcopy,!_hdIerrors,!_hdIoutput) ->
                                                                                    (case (({-# LINE 195 "src-ag/Desugar.ag" #-}
                                                                                            _hdIallAttributes `mergeAttributes` _tlIallAttributes
                                                                                            {-# LINE 1943 "src-ag/Desugar.hs" #-}
                                                                                            )) of
                                                                                     { !_lhsOallAttributes ->
                                                                                     (case (({-# LINE 23 "src-ag/Patterns.ag" #-}
                                                                                             (:) _hdIcopy _tlIcopy
                                                                                             {-# LINE 1948 "src-ag/Desugar.hs" #-}
                                                                                             )) of
                                                                                      { !_copy ->
                                                                                      (case (({-# LINE 23 "src-ag/Patterns.ag" #-}
                                                                                              _copy
                                                                                              {-# LINE 1953 "src-ag/Desugar.hs" #-}
                                                                                              )) of
                                                                                       { !_lhsOcopy ->
                                                                                       (case (({-# LINE 37 "src-ag/Desugar.ag" #-}
                                                                                               _hdIerrors Seq.>< _tlIerrors
                                                                                               {-# LINE 1958 "src-ag/Desugar.hs" #-}
                                                                                               )) of
                                                                                        { !_lhsOerrors ->
                                                                                        (case (({-# LINE 39 "src-ag/Desugar.ag" #-}
                                                                                                (:) _hdIoutput _tlIoutput
                                                                                                {-# LINE 1963 "src-ag/Desugar.hs" #-}
                                                                                                )) of
                                                                                         { !_output ->
                                                                                         (case (({-# LINE 39 "src-ag/Desugar.ag" #-}
                                                                                                 _output
                                                                                                 {-# LINE 1968 "src-ag/Desugar.hs" #-}
                                                                                                 )) of
                                                                                          { !_lhsOoutput ->
                                                                                          ( _lhsOallAttributes,_lhsOcopy,_lhsOerrors,_lhsOoutput) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })) )
                                   in  sem_Patterns_Cons_1)) of
                            { ( !sem_Patterns_1) ->
                            ( _lhsOdefsCollect,sem_Patterns_1) }) }) }) }) )
sem_Patterns_Nil :: T_Patterns 
sem_Patterns_Nil  =
    (T_Patterns (case (({-# LINE 178 "src-ag/Desugar.ag" #-}
                        Set.empty
                        {-# LINE 1979 "src-ag/Desugar.hs" #-}
                        )) of
                 { !_lhsOdefsCollect ->
                 (case ((let sem_Patterns_Nil_1 :: T_Patterns_1 
                             sem_Patterns_Nil_1  =
                                 (T_Patterns_1 (\ (!_lhsIchildInhs)
                                                  (!_lhsIchildSyns)
                                                  (!_lhsIcon)
                                                  (!_lhsIdefs)
                                                  (!_lhsIforcedIrrefutables)
                                                  (!_lhsInt) ->
                                                    (case (({-# LINE 195 "src-ag/Desugar.ag" #-}
                                                            Map.empty
                                                            {-# LINE 1992 "src-ag/Desugar.hs" #-}
                                                            )) of
                                                     { !_lhsOallAttributes ->
                                                     (case (({-# LINE 23 "src-ag/Patterns.ag" #-}
                                                             []
                                                             {-# LINE 1997 "src-ag/Desugar.hs" #-}
                                                             )) of
                                                      { !_copy ->
                                                      (case (({-# LINE 23 "src-ag/Patterns.ag" #-}
                                                              _copy
                                                              {-# LINE 2002 "src-ag/Desugar.hs" #-}
                                                              )) of
                                                       { !_lhsOcopy ->
                                                       (case (({-# LINE 37 "src-ag/Desugar.ag" #-}
                                                               Seq.empty
                                                               {-# LINE 2007 "src-ag/Desugar.hs" #-}
                                                               )) of
                                                        { !_lhsOerrors ->
                                                        (case (({-# LINE 39 "src-ag/Desugar.ag" #-}
                                                                []
                                                                {-# LINE 2012 "src-ag/Desugar.hs" #-}
                                                                )) of
                                                         { !_output ->
                                                         (case (({-# LINE 39 "src-ag/Desugar.ag" #-}
                                                                 _output
                                                                 {-# LINE 2017 "src-ag/Desugar.hs" #-}
                                                                 )) of
                                                          { !_lhsOoutput ->
                                                          ( _lhsOallAttributes,_lhsOcopy,_lhsOerrors,_lhsOoutput) }) }) }) }) }) })) )
                         in  sem_Patterns_Nil_1)) of
                  { ( !sem_Patterns_1) ->
                  ( _lhsOdefsCollect,sem_Patterns_1) }) }) )
-- Production --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         augmentsIn           : Map ConstructorIdent (Map Identifier [Expression])
         forcedIrrefutables   : AttrMap
         nt                   : NontermIdent
         options              : Options
      synthesized attributes:
         allAttributes        : AttrMap
         augmentsOut          : Map ConstructorIdent (Map Identifier [Expression])
         errors               : Seq Error
         output               : SELF 
   alternatives:
      alternative Production:
         child con            : {ConstructorIdent}
         child children       : Children 
         child rules          : Rules 
         child typeSigs       : TypeSigs 
         visit 0:
            local augmentsIn  : _
            local _tup3       : _
            local augmentsOut1 : _
            local augmentsOut : _
            local augmentErrs : _
            local output      : _
-}
-- cata
sem_Production :: Production  ->
                  T_Production 
sem_Production !(Production _con _children _rules _typeSigs )  =
    (sem_Production_Production _con (sem_Children _children ) (sem_Rules _rules ) (sem_TypeSigs _typeSigs ) )
-- semantic domain
newtype T_Production  = T_Production ((Map ConstructorIdent (Map Identifier [Expression])) ->
                                      AttrMap ->
                                      NontermIdent ->
                                      Options ->
                                      ( AttrMap,(Map ConstructorIdent (Map Identifier [Expression])),(Seq Error),Production ))
data Inh_Production  = Inh_Production {augmentsIn_Inh_Production :: !((Map ConstructorIdent (Map Identifier [Expression]))),forcedIrrefutables_Inh_Production :: !(AttrMap),nt_Inh_Production :: !(NontermIdent),options_Inh_Production :: !(Options)}
data Syn_Production  = Syn_Production {allAttributes_Syn_Production :: !(AttrMap),augmentsOut_Syn_Production :: !((Map ConstructorIdent (Map Identifier [Expression]))),errors_Syn_Production :: !((Seq Error)),output_Syn_Production :: !(Production )}
wrap_Production :: T_Production  ->
                   Inh_Production  ->
                   Syn_Production 
wrap_Production !(T_Production sem ) !(Inh_Production _lhsIaugmentsIn _lhsIforcedIrrefutables _lhsInt _lhsIoptions )  =
    (let ( !_lhsOallAttributes,!_lhsOaugmentsOut,!_lhsOerrors,!_lhsOoutput) = sem _lhsIaugmentsIn _lhsIforcedIrrefutables _lhsInt _lhsIoptions 
     in  (Syn_Production _lhsOallAttributes _lhsOaugmentsOut _lhsOerrors _lhsOoutput ))
sem_Production_Production :: ConstructorIdent ->
                             T_Children  ->
                             T_Rules  ->
                             T_TypeSigs  ->
                             T_Production 
sem_Production_Production !con_ !(T_Children children_ ) !(T_Rules rules_ ) !(T_TypeSigs typeSigs_ )  =
    (T_Production (\ (!_lhsIaugmentsIn)
                     (!_lhsIforcedIrrefutables)
                     (!_lhsInt)
                     (!_lhsIoptions) ->
                       (case (({-# LINE 151 "src-ag/Desugar.ag" #-}
                               _lhsInt
                               {-# LINE 2082 "src-ag/Desugar.hs" #-}
                               )) of
                        { !_rulesOnt ->
                        (case (({-# LINE 160 "src-ag/Desugar.ag" #-}
                                con_
                                {-# LINE 2087 "src-ag/Desugar.hs" #-}
                                )) of
                         { !_rulesOcon ->
                         (case (rules_ ) of
                          { ( !_rulesIdefsCollect,!T_Rules_1 rules_1) ->
                              (case (({-# LINE 35 "src-ag/Desugar.ag" #-}
                                      _lhsIoptions
                                      {-# LINE 2094 "src-ag/Desugar.hs" #-}
                                      )) of
                               { !_rulesOoptions ->
                               (case (({-# LINE 214 "src-ag/Desugar.ag" #-}
                                       _lhsIforcedIrrefutables
                                       {-# LINE 2099 "src-ag/Desugar.hs" #-}
                                       )) of
                                { !_rulesOforcedIrrefutables ->
                                (case (children_ ) of
                                 { ( !_childrenIchildInhs,!_childrenIchildSyns,!_childrenIoutput) ->
                                     (case (({-# LINE 125 "src-ag/Desugar.ag" #-}
                                             _childrenIchildSyns
                                             {-# LINE 2106 "src-ag/Desugar.hs" #-}
                                             )) of
                                      { !_rulesOchildSyns ->
                                      (case (({-# LINE 125 "src-ag/Desugar.ag" #-}
                                              _childrenIchildInhs
                                              {-# LINE 2111 "src-ag/Desugar.hs" #-}
                                              )) of
                                       { !_rulesOchildInhs ->
                                       (case (({-# LINE 187 "src-ag/Desugar.ag" #-}
                                               _rulesIdefsCollect
                                               {-# LINE 2116 "src-ag/Desugar.hs" #-}
                                               )) of
                                        { !_rulesOdefs ->
                                        (case (rules_1 _rulesOchildInhs _rulesOchildSyns _rulesOcon _rulesOdefs _rulesOforcedIrrefutables _rulesOnt _rulesOoptions ) of
                                         { ( !_rulesIallAttributes,!_rulesIerrors,!_rulesIoutput) ->
                                             (case (({-# LINE 195 "src-ag/Desugar.ag" #-}
                                                     _rulesIallAttributes
                                                     {-# LINE 2123 "src-ag/Desugar.hs" #-}
                                                     )) of
                                              { !_lhsOallAttributes ->
                                              (case (({-# LINE 243 "src-ag/Desugar.ag" #-}
                                                      Map.findWithDefault Map.empty con_ _lhsIaugmentsIn
                                                      {-# LINE 2128 "src-ag/Desugar.hs" #-}
                                                      )) of
                                               { !_augmentsIn ->
                                               (case (({-# LINE 246 "src-ag/Desugar.ag" #-}
                                                       Map.mapAccum (desugarExprs _lhsIoptions _lhsInt con_ _childrenIchildInhs _childrenIchildSyns) Seq.empty _augmentsIn
                                                       {-# LINE 2133 "src-ag/Desugar.hs" #-}
                                                       )) of
                                                { !__tup3 ->
                                                (case (({-# LINE 246 "src-ag/Desugar.ag" #-}
                                                        __tup3
                                                        {-# LINE 2138 "src-ag/Desugar.hs" #-}
                                                        )) of
                                                 { !(_,!_augmentsOut1) ->
                                                 (case (({-# LINE 244 "src-ag/Desugar.ag" #-}
                                                         Map.singleton con_ _augmentsOut1
                                                         {-# LINE 2143 "src-ag/Desugar.hs" #-}
                                                         )) of
                                                  { !_augmentsOut ->
                                                  (case (({-# LINE 230 "src-ag/Desugar.ag" #-}
                                                          _augmentsOut
                                                          {-# LINE 2148 "src-ag/Desugar.hs" #-}
                                                          )) of
                                                   { !_lhsOaugmentsOut ->
                                                   (case (({-# LINE 246 "src-ag/Desugar.ag" #-}
                                                           __tup3
                                                           {-# LINE 2153 "src-ag/Desugar.hs" #-}
                                                           )) of
                                                    { !(!_augmentErrs,_) ->
                                                    (case (({-# LINE 282 "src-ag/Desugar.ag" #-}
                                                            _rulesIerrors Seq.>< _augmentErrs
                                                            {-# LINE 2158 "src-ag/Desugar.hs" #-}
                                                            )) of
                                                     { !_lhsOerrors ->
                                                     (case (typeSigs_ ) of
                                                      { ( !_typeSigsIoutput) ->
                                                          (case (({-# LINE 39 "src-ag/Desugar.ag" #-}
                                                                  Production con_ _childrenIoutput _rulesIoutput _typeSigsIoutput
                                                                  {-# LINE 2165 "src-ag/Desugar.hs" #-}
                                                                  )) of
                                                           { !_output ->
                                                           (case (({-# LINE 39 "src-ag/Desugar.ag" #-}
                                                                   _output
                                                                   {-# LINE 2170 "src-ag/Desugar.hs" #-}
                                                                   )) of
                                                            { !_lhsOoutput ->
                                                            ( _lhsOallAttributes,_lhsOaugmentsOut,_lhsOerrors,_lhsOoutput) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })) )
-- Productions -------------------------------------------------
{-
   visit 0:
      inherited attributes:
         augmentsIn           : Map ConstructorIdent (Map Identifier [Expression])
         forcedIrrefutables   : AttrMap
         nt                   : NontermIdent
         options              : Options
      synthesized attributes:
         allAttributes        : AttrMap
         augmentsOut          : Map ConstructorIdent (Map Identifier [Expression])
         errors               : Seq Error
         output               : SELF 
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
sem_Productions :: Productions  ->
                   T_Productions 
sem_Productions !list  =
    (Prelude.foldr sem_Productions_Cons sem_Productions_Nil (Prelude.map sem_Production list) )
-- semantic domain
newtype T_Productions  = T_Productions ((Map ConstructorIdent (Map Identifier [Expression])) ->
                                        AttrMap ->
                                        NontermIdent ->
                                        Options ->
                                        ( AttrMap,(Map ConstructorIdent (Map Identifier [Expression])),(Seq Error),Productions ))
data Inh_Productions  = Inh_Productions {augmentsIn_Inh_Productions :: !((Map ConstructorIdent (Map Identifier [Expression]))),forcedIrrefutables_Inh_Productions :: !(AttrMap),nt_Inh_Productions :: !(NontermIdent),options_Inh_Productions :: !(Options)}
data Syn_Productions  = Syn_Productions {allAttributes_Syn_Productions :: !(AttrMap),augmentsOut_Syn_Productions :: !((Map ConstructorIdent (Map Identifier [Expression]))),errors_Syn_Productions :: !((Seq Error)),output_Syn_Productions :: !(Productions )}
wrap_Productions :: T_Productions  ->
                    Inh_Productions  ->
                    Syn_Productions 
wrap_Productions !(T_Productions sem ) !(Inh_Productions _lhsIaugmentsIn _lhsIforcedIrrefutables _lhsInt _lhsIoptions )  =
    (let ( !_lhsOallAttributes,!_lhsOaugmentsOut,!_lhsOerrors,!_lhsOoutput) = sem _lhsIaugmentsIn _lhsIforcedIrrefutables _lhsInt _lhsIoptions 
     in  (Syn_Productions _lhsOallAttributes _lhsOaugmentsOut _lhsOerrors _lhsOoutput ))
sem_Productions_Cons :: T_Production  ->
                        T_Productions  ->
                        T_Productions 
sem_Productions_Cons !(T_Production hd_ ) !(T_Productions tl_ )  =
    (T_Productions (\ (!_lhsIaugmentsIn)
                      (!_lhsIforcedIrrefutables)
                      (!_lhsInt)
                      (!_lhsIoptions) ->
                        (case (({-# LINE 151 "src-ag/Desugar.ag" #-}
                                _lhsInt
                                {-# LINE 2226 "src-ag/Desugar.hs" #-}
                                )) of
                         { !_tlOnt ->
                         (case (({-# LINE 151 "src-ag/Desugar.ag" #-}
                                 _lhsInt
                                 {-# LINE 2231 "src-ag/Desugar.hs" #-}
                                 )) of
                          { !_hdOnt ->
                          (case (({-# LINE 35 "src-ag/Desugar.ag" #-}
                                  _lhsIoptions
                                  {-# LINE 2236 "src-ag/Desugar.hs" #-}
                                  )) of
                           { !_tlOoptions ->
                           (case (({-# LINE 214 "src-ag/Desugar.ag" #-}
                                   _lhsIforcedIrrefutables
                                   {-# LINE 2241 "src-ag/Desugar.hs" #-}
                                   )) of
                            { !_tlOforcedIrrefutables ->
                            (case (({-# LINE 229 "src-ag/Desugar.ag" #-}
                                    _lhsIaugmentsIn
                                    {-# LINE 2246 "src-ag/Desugar.hs" #-}
                                    )) of
                             { !_tlOaugmentsIn ->
                             (case (tl_ _tlOaugmentsIn _tlOforcedIrrefutables _tlOnt _tlOoptions ) of
                              { ( !_tlIallAttributes,!_tlIaugmentsOut,!_tlIerrors,!_tlIoutput) ->
                                  (case (({-# LINE 35 "src-ag/Desugar.ag" #-}
                                          _lhsIoptions
                                          {-# LINE 2253 "src-ag/Desugar.hs" #-}
                                          )) of
                                   { !_hdOoptions ->
                                   (case (({-# LINE 214 "src-ag/Desugar.ag" #-}
                                           _lhsIforcedIrrefutables
                                           {-# LINE 2258 "src-ag/Desugar.hs" #-}
                                           )) of
                                    { !_hdOforcedIrrefutables ->
                                    (case (({-# LINE 229 "src-ag/Desugar.ag" #-}
                                            _lhsIaugmentsIn
                                            {-# LINE 2263 "src-ag/Desugar.hs" #-}
                                            )) of
                                     { !_hdOaugmentsIn ->
                                     (case (hd_ _hdOaugmentsIn _hdOforcedIrrefutables _hdOnt _hdOoptions ) of
                                      { ( !_hdIallAttributes,!_hdIaugmentsOut,!_hdIerrors,!_hdIoutput) ->
                                          (case (({-# LINE 195 "src-ag/Desugar.ag" #-}
                                                  _hdIallAttributes `mergeAttributes` _tlIallAttributes
                                                  {-# LINE 2270 "src-ag/Desugar.hs" #-}
                                                  )) of
                                           { !_lhsOallAttributes ->
                                           (case (({-# LINE 230 "src-ag/Desugar.ag" #-}
                                                   _hdIaugmentsOut `Map.union` _tlIaugmentsOut
                                                   {-# LINE 2275 "src-ag/Desugar.hs" #-}
                                                   )) of
                                            { !_lhsOaugmentsOut ->
                                            (case (({-# LINE 37 "src-ag/Desugar.ag" #-}
                                                    _hdIerrors Seq.>< _tlIerrors
                                                    {-# LINE 2280 "src-ag/Desugar.hs" #-}
                                                    )) of
                                             { !_lhsOerrors ->
                                             (case (({-# LINE 39 "src-ag/Desugar.ag" #-}
                                                     (:) _hdIoutput _tlIoutput
                                                     {-# LINE 2285 "src-ag/Desugar.hs" #-}
                                                     )) of
                                              { !_output ->
                                              (case (({-# LINE 39 "src-ag/Desugar.ag" #-}
                                                      _output
                                                      {-# LINE 2290 "src-ag/Desugar.hs" #-}
                                                      )) of
                                               { !_lhsOoutput ->
                                               ( _lhsOallAttributes,_lhsOaugmentsOut,_lhsOerrors,_lhsOoutput) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })) )
sem_Productions_Nil :: T_Productions 
sem_Productions_Nil  =
    (T_Productions (\ (!_lhsIaugmentsIn)
                      (!_lhsIforcedIrrefutables)
                      (!_lhsInt)
                      (!_lhsIoptions) ->
                        (case (({-# LINE 195 "src-ag/Desugar.ag" #-}
                                Map.empty
                                {-# LINE 2302 "src-ag/Desugar.hs" #-}
                                )) of
                         { !_lhsOallAttributes ->
                         (case (({-# LINE 230 "src-ag/Desugar.ag" #-}
                                 Map.empty
                                 {-# LINE 2307 "src-ag/Desugar.hs" #-}
                                 )) of
                          { !_lhsOaugmentsOut ->
                          (case (({-# LINE 37 "src-ag/Desugar.ag" #-}
                                  Seq.empty
                                  {-# LINE 2312 "src-ag/Desugar.hs" #-}
                                  )) of
                           { !_lhsOerrors ->
                           (case (({-# LINE 39 "src-ag/Desugar.ag" #-}
                                   []
                                   {-# LINE 2317 "src-ag/Desugar.hs" #-}
                                   )) of
                            { !_output ->
                            (case (({-# LINE 39 "src-ag/Desugar.ag" #-}
                                    _output
                                    {-# LINE 2322 "src-ag/Desugar.hs" #-}
                                    )) of
                             { !_lhsOoutput ->
                             ( _lhsOallAttributes,_lhsOaugmentsOut,_lhsOerrors,_lhsOoutput) }) }) }) }) })) )
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
         output               : SELF 
   alternatives:
      alternative Rule:
         child mbName         : {Maybe Identifier}
         child pattern        : Pattern 
         child rhs            : Expression 
         child owrt           : {Bool}
         child origin         : {String}
         child explicit       : {Bool}
         visit 1:
            local ruleDescr   : _
            local output      : _
-}
-- cata
sem_Rule :: Rule  ->
            T_Rule 
sem_Rule !(Rule _mbName _pattern _rhs _owrt _origin _explicit )  =
    (sem_Rule_Rule _mbName (sem_Pattern _pattern ) (sem_Expression _rhs ) _owrt _origin _explicit )
-- semantic domain
newtype T_Rule  = T_Rule (( (Set (Identifier, Identifier)),T_Rule_1 ))
newtype T_Rule_1  = T_Rule_1 (([(Identifier, Identifier)]) ->
                              ([(Identifier, Identifier)]) ->
                              ConstructorIdent ->
                              (Set (Identifier, Identifier)) ->
                              AttrMap ->
                              NontermIdent ->
                              Options ->
                              ( AttrMap,(Seq Error),Rule ))
data Inh_Rule  = Inh_Rule {childInhs_Inh_Rule :: !(([(Identifier, Identifier)])),childSyns_Inh_Rule :: !(([(Identifier, Identifier)])),con_Inh_Rule :: !(ConstructorIdent),defs_Inh_Rule :: !((Set (Identifier, Identifier))),forcedIrrefutables_Inh_Rule :: !(AttrMap),nt_Inh_Rule :: !(NontermIdent),options_Inh_Rule :: !(Options)}
data Syn_Rule  = Syn_Rule {allAttributes_Syn_Rule :: !(AttrMap),defsCollect_Syn_Rule :: !((Set (Identifier, Identifier))),errors_Syn_Rule :: !((Seq Error)),output_Syn_Rule :: !(Rule )}
wrap_Rule :: T_Rule  ->
             Inh_Rule  ->
             Syn_Rule 
wrap_Rule !(T_Rule sem ) !(Inh_Rule _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt _lhsIoptions )  =
    (let ( !_lhsOdefsCollect,!T_Rule_1 sem_1) = sem 
         ( !_lhsOallAttributes,!_lhsOerrors,!_lhsOoutput) = sem_1 _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt _lhsIoptions 
     in  (Syn_Rule _lhsOallAttributes _lhsOdefsCollect _lhsOerrors _lhsOoutput ))
sem_Rule_Rule :: (Maybe Identifier) ->
                 T_Pattern  ->
                 T_Expression  ->
                 Bool ->
                 String ->
                 Bool ->
                 T_Rule 
sem_Rule_Rule !mbName_ !(T_Pattern pattern_ ) !(T_Expression rhs_ ) !owrt_ !origin_ !explicit_  =
    (T_Rule (case (pattern_ ) of
             { ( !_patternIdefsCollect,!T_Pattern_1 pattern_1) ->
                 (case (({-# LINE 178 "src-ag/Desugar.ag" #-}
                         _patternIdefsCollect
                         {-# LINE 2392 "src-ag/Desugar.hs" #-}
                         )) of
                  { !_lhsOdefsCollect ->
                  (case ((let sem_Rule_Rule_1 :: T_Rule_1 
                              sem_Rule_Rule_1  =
                                  (T_Rule_1 (\ (!_lhsIchildInhs)
                                               (!_lhsIchildSyns)
                                               (!_lhsIcon)
                                               (!_lhsIdefs)
                                               (!_lhsIforcedIrrefutables)
                                               (!_lhsInt)
                                               (!_lhsIoptions) ->
                                                 (case (({-# LINE 151 "src-ag/Desugar.ag" #-}
                                                         _lhsInt
                                                         {-# LINE 2406 "src-ag/Desugar.hs" #-}
                                                         )) of
                                                  { !_patternOnt ->
                                                  (case (({-# LINE 152 "src-ag/Desugar.ag" #-}
                                                          _lhsIcon
                                                          {-# LINE 2411 "src-ag/Desugar.hs" #-}
                                                          )) of
                                                   { !_patternOcon ->
                                                   (case (({-# LINE 214 "src-ag/Desugar.ag" #-}
                                                           _lhsIforcedIrrefutables
                                                           {-# LINE 2416 "src-ag/Desugar.hs" #-}
                                                           )) of
                                                    { !_patternOforcedIrrefutables ->
                                                    (case (({-# LINE 184 "src-ag/Desugar.ag" #-}
                                                            _lhsIdefs
                                                            {-# LINE 2421 "src-ag/Desugar.hs" #-}
                                                            )) of
                                                     { !_patternOdefs ->
                                                     (case (({-# LINE 125 "src-ag/Desugar.ag" #-}
                                                             _lhsIchildSyns
                                                             {-# LINE 2426 "src-ag/Desugar.hs" #-}
                                                             )) of
                                                      { !_patternOchildSyns ->
                                                      (case (({-# LINE 125 "src-ag/Desugar.ag" #-}
                                                              _lhsIchildInhs
                                                              {-# LINE 2431 "src-ag/Desugar.hs" #-}
                                                              )) of
                                                       { !_patternOchildInhs ->
                                                       (case (pattern_1 _patternOchildInhs _patternOchildSyns _patternOcon _patternOdefs _patternOforcedIrrefutables _patternOnt ) of
                                                        { ( !_patternIallAttributes,!_patternIcopy,!_patternIerrors,!_patternIoutput) ->
                                                            (case (({-# LINE 195 "src-ag/Desugar.ag" #-}
                                                                    _patternIallAttributes
                                                                    {-# LINE 2438 "src-ag/Desugar.hs" #-}
                                                                    )) of
                                                             { !_lhsOallAttributes ->
                                                             (case (({-# LINE 171 "src-ag/Desugar.ag" #-}
                                                                     show _lhsInt ++ " :: " ++ show _lhsIcon ++ " :: " ++ (concat $ intersperse "," $ map (\(f,a) -> show f ++ "." ++ show a) $ Set.toList _patternIdefsCollect)
                                                                     {-# LINE 2443 "src-ag/Desugar.hs" #-}
                                                                     )) of
                                                              { !_ruleDescr ->
                                                              (case (({-# LINE 167 "src-ag/Desugar.ag" #-}
                                                                      _ruleDescr
                                                                      {-# LINE 2448 "src-ag/Desugar.hs" #-}
                                                                      )) of
                                                               { !_rhsOruleDescr ->
                                                               (case (({-# LINE 35 "src-ag/Desugar.ag" #-}
                                                                       _lhsIoptions
                                                                       {-# LINE 2453 "src-ag/Desugar.hs" #-}
                                                                       )) of
                                                                { !_rhsOoptions ->
                                                                (case (({-# LINE 151 "src-ag/Desugar.ag" #-}
                                                                        _lhsInt
                                                                        {-# LINE 2458 "src-ag/Desugar.hs" #-}
                                                                        )) of
                                                                 { !_rhsOnt ->
                                                                 (case (({-# LINE 152 "src-ag/Desugar.ag" #-}
                                                                         _lhsIcon
                                                                         {-# LINE 2463 "src-ag/Desugar.hs" #-}
                                                                         )) of
                                                                  { !_rhsOcon ->
                                                                  (case (({-# LINE 125 "src-ag/Desugar.ag" #-}
                                                                          _lhsIchildSyns
                                                                          {-# LINE 2468 "src-ag/Desugar.hs" #-}
                                                                          )) of
                                                                   { !_rhsOchildSyns ->
                                                                   (case (({-# LINE 125 "src-ag/Desugar.ag" #-}
                                                                           _lhsIchildInhs
                                                                           {-# LINE 2473 "src-ag/Desugar.hs" #-}
                                                                           )) of
                                                                    { !_rhsOchildInhs ->
                                                                    (case (rhs_ _rhsOchildInhs _rhsOchildSyns _rhsOcon _rhsOnt _rhsOoptions _rhsOruleDescr ) of
                                                                     { ( !_rhsIerrors,!_rhsIoutput) ->
                                                                         (case (({-# LINE 37 "src-ag/Desugar.ag" #-}
                                                                                 _patternIerrors Seq.>< _rhsIerrors
                                                                                 {-# LINE 2480 "src-ag/Desugar.hs" #-}
                                                                                 )) of
                                                                          { !_lhsOerrors ->
                                                                          (case (({-# LINE 39 "src-ag/Desugar.ag" #-}
                                                                                  Rule mbName_ _patternIoutput _rhsIoutput owrt_ origin_ explicit_
                                                                                  {-# LINE 2485 "src-ag/Desugar.hs" #-}
                                                                                  )) of
                                                                           { !_output ->
                                                                           (case (({-# LINE 39 "src-ag/Desugar.ag" #-}
                                                                                   _output
                                                                                   {-# LINE 2490 "src-ag/Desugar.hs" #-}
                                                                                   )) of
                                                                            { !_lhsOoutput ->
                                                                            ( _lhsOallAttributes,_lhsOerrors,_lhsOoutput) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })) )
                          in  sem_Rule_Rule_1)) of
                   { ( !sem_Rule_1) ->
                   ( _lhsOdefsCollect,sem_Rule_1) }) }) }) )
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
         output               : SELF 
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
sem_Rules :: Rules  ->
             T_Rules 
sem_Rules !list  =
    (Prelude.foldr sem_Rules_Cons sem_Rules_Nil (Prelude.map sem_Rule list) )
-- semantic domain
newtype T_Rules  = T_Rules (( (Set (Identifier, Identifier)),T_Rules_1 ))
newtype T_Rules_1  = T_Rules_1 (([(Identifier, Identifier)]) ->
                                ([(Identifier, Identifier)]) ->
                                ConstructorIdent ->
                                (Set (Identifier, Identifier)) ->
                                AttrMap ->
                                NontermIdent ->
                                Options ->
                                ( AttrMap,(Seq Error),Rules ))
data Inh_Rules  = Inh_Rules {childInhs_Inh_Rules :: !(([(Identifier, Identifier)])),childSyns_Inh_Rules :: !(([(Identifier, Identifier)])),con_Inh_Rules :: !(ConstructorIdent),defs_Inh_Rules :: !((Set (Identifier, Identifier))),forcedIrrefutables_Inh_Rules :: !(AttrMap),nt_Inh_Rules :: !(NontermIdent),options_Inh_Rules :: !(Options)}
data Syn_Rules  = Syn_Rules {allAttributes_Syn_Rules :: !(AttrMap),defsCollect_Syn_Rules :: !((Set (Identifier, Identifier))),errors_Syn_Rules :: !((Seq Error)),output_Syn_Rules :: !(Rules )}
wrap_Rules :: T_Rules  ->
              Inh_Rules  ->
              Syn_Rules 
wrap_Rules !(T_Rules sem ) !(Inh_Rules _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt _lhsIoptions )  =
    (let ( !_lhsOdefsCollect,!T_Rules_1 sem_1) = sem 
         ( !_lhsOallAttributes,!_lhsOerrors,!_lhsOoutput) = sem_1 _lhsIchildInhs _lhsIchildSyns _lhsIcon _lhsIdefs _lhsIforcedIrrefutables _lhsInt _lhsIoptions 
     in  (Syn_Rules _lhsOallAttributes _lhsOdefsCollect _lhsOerrors _lhsOoutput ))
sem_Rules_Cons :: T_Rule  ->
                  T_Rules  ->
                  T_Rules 
sem_Rules_Cons !(T_Rule hd_ ) !(T_Rules tl_ )  =
    (T_Rules (case (tl_ ) of
              { ( !_tlIdefsCollect,!T_Rules_1 tl_1) ->
                  (case (hd_ ) of
                   { ( !_hdIdefsCollect,!T_Rule_1 hd_1) ->
                       (case (({-# LINE 178 "src-ag/Desugar.ag" #-}
                               _hdIdefsCollect `Set.union` _tlIdefsCollect
                               {-# LINE 2559 "src-ag/Desugar.hs" #-}
                               )) of
                        { !_lhsOdefsCollect ->
                        (case ((let sem_Rules_Cons_1 :: T_Rules_1 
                                    sem_Rules_Cons_1  =
                                        (T_Rules_1 (\ (!_lhsIchildInhs)
                                                      (!_lhsIchildSyns)
                                                      (!_lhsIcon)
                                                      (!_lhsIdefs)
                                                      (!_lhsIforcedIrrefutables)
                                                      (!_lhsInt)
                                                      (!_lhsIoptions) ->
                                                        (case (({-# LINE 151 "src-ag/Desugar.ag" #-}
                                                                _lhsInt
                                                                {-# LINE 2573 "src-ag/Desugar.hs" #-}
                                                                )) of
                                                         { !_tlOnt ->
                                                         (case (({-# LINE 152 "src-ag/Desugar.ag" #-}
                                                                 _lhsIcon
                                                                 {-# LINE 2578 "src-ag/Desugar.hs" #-}
                                                                 )) of
                                                          { !_tlOcon ->
                                                          (case (({-# LINE 151 "src-ag/Desugar.ag" #-}
                                                                  _lhsInt
                                                                  {-# LINE 2583 "src-ag/Desugar.hs" #-}
                                                                  )) of
                                                           { !_hdOnt ->
                                                           (case (({-# LINE 152 "src-ag/Desugar.ag" #-}
                                                                   _lhsIcon
                                                                   {-# LINE 2588 "src-ag/Desugar.hs" #-}
                                                                   )) of
                                                            { !_hdOcon ->
                                                            (case (({-# LINE 35 "src-ag/Desugar.ag" #-}
                                                                    _lhsIoptions
                                                                    {-# LINE 2593 "src-ag/Desugar.hs" #-}
                                                                    )) of
                                                             { !_tlOoptions ->
                                                             (case (({-# LINE 214 "src-ag/Desugar.ag" #-}
                                                                     _lhsIforcedIrrefutables
                                                                     {-# LINE 2598 "src-ag/Desugar.hs" #-}
                                                                     )) of
                                                              { !_tlOforcedIrrefutables ->
                                                              (case (({-# LINE 184 "src-ag/Desugar.ag" #-}
                                                                      _lhsIdefs
                                                                      {-# LINE 2603 "src-ag/Desugar.hs" #-}
                                                                      )) of
                                                               { !_tlOdefs ->
                                                               (case (({-# LINE 125 "src-ag/Desugar.ag" #-}
                                                                       _lhsIchildSyns
                                                                       {-# LINE 2608 "src-ag/Desugar.hs" #-}
                                                                       )) of
                                                                { !_tlOchildSyns ->
                                                                (case (({-# LINE 125 "src-ag/Desugar.ag" #-}
                                                                        _lhsIchildInhs
                                                                        {-# LINE 2613 "src-ag/Desugar.hs" #-}
                                                                        )) of
                                                                 { !_tlOchildInhs ->
                                                                 (case (tl_1 _tlOchildInhs _tlOchildSyns _tlOcon _tlOdefs _tlOforcedIrrefutables _tlOnt _tlOoptions ) of
                                                                  { ( !_tlIallAttributes,!_tlIerrors,!_tlIoutput) ->
                                                                      (case (({-# LINE 35 "src-ag/Desugar.ag" #-}
                                                                              _lhsIoptions
                                                                              {-# LINE 2620 "src-ag/Desugar.hs" #-}
                                                                              )) of
                                                                       { !_hdOoptions ->
                                                                       (case (({-# LINE 214 "src-ag/Desugar.ag" #-}
                                                                               _lhsIforcedIrrefutables
                                                                               {-# LINE 2625 "src-ag/Desugar.hs" #-}
                                                                               )) of
                                                                        { !_hdOforcedIrrefutables ->
                                                                        (case (({-# LINE 184 "src-ag/Desugar.ag" #-}
                                                                                _lhsIdefs
                                                                                {-# LINE 2630 "src-ag/Desugar.hs" #-}
                                                                                )) of
                                                                         { !_hdOdefs ->
                                                                         (case (({-# LINE 125 "src-ag/Desugar.ag" #-}
                                                                                 _lhsIchildSyns
                                                                                 {-# LINE 2635 "src-ag/Desugar.hs" #-}
                                                                                 )) of
                                                                          { !_hdOchildSyns ->
                                                                          (case (({-# LINE 125 "src-ag/Desugar.ag" #-}
                                                                                  _lhsIchildInhs
                                                                                  {-# LINE 2640 "src-ag/Desugar.hs" #-}
                                                                                  )) of
                                                                           { !_hdOchildInhs ->
                                                                           (case (hd_1 _hdOchildInhs _hdOchildSyns _hdOcon _hdOdefs _hdOforcedIrrefutables _hdOnt _hdOoptions ) of
                                                                            { ( !_hdIallAttributes,!_hdIerrors,!_hdIoutput) ->
                                                                                (case (({-# LINE 195 "src-ag/Desugar.ag" #-}
                                                                                        _hdIallAttributes `mergeAttributes` _tlIallAttributes
                                                                                        {-# LINE 2647 "src-ag/Desugar.hs" #-}
                                                                                        )) of
                                                                                 { !_lhsOallAttributes ->
                                                                                 (case (({-# LINE 37 "src-ag/Desugar.ag" #-}
                                                                                         _hdIerrors Seq.>< _tlIerrors
                                                                                         {-# LINE 2652 "src-ag/Desugar.hs" #-}
                                                                                         )) of
                                                                                  { !_lhsOerrors ->
                                                                                  (case (({-# LINE 39 "src-ag/Desugar.ag" #-}
                                                                                          (:) _hdIoutput _tlIoutput
                                                                                          {-# LINE 2657 "src-ag/Desugar.hs" #-}
                                                                                          )) of
                                                                                   { !_output ->
                                                                                   (case (({-# LINE 39 "src-ag/Desugar.ag" #-}
                                                                                           _output
                                                                                           {-# LINE 2662 "src-ag/Desugar.hs" #-}
                                                                                           )) of
                                                                                    { !_lhsOoutput ->
                                                                                    ( _lhsOallAttributes,_lhsOerrors,_lhsOoutput) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })) )
                                in  sem_Rules_Cons_1)) of
                         { ( !sem_Rules_1) ->
                         ( _lhsOdefsCollect,sem_Rules_1) }) }) }) }) )
sem_Rules_Nil :: T_Rules 
sem_Rules_Nil  =
    (T_Rules (case (({-# LINE 178 "src-ag/Desugar.ag" #-}
                     Set.empty
                     {-# LINE 2673 "src-ag/Desugar.hs" #-}
                     )) of
              { !_lhsOdefsCollect ->
              (case ((let sem_Rules_Nil_1 :: T_Rules_1 
                          sem_Rules_Nil_1  =
                              (T_Rules_1 (\ (!_lhsIchildInhs)
                                            (!_lhsIchildSyns)
                                            (!_lhsIcon)
                                            (!_lhsIdefs)
                                            (!_lhsIforcedIrrefutables)
                                            (!_lhsInt)
                                            (!_lhsIoptions) ->
                                              (case (({-# LINE 195 "src-ag/Desugar.ag" #-}
                                                      Map.empty
                                                      {-# LINE 2687 "src-ag/Desugar.hs" #-}
                                                      )) of
                                               { !_lhsOallAttributes ->
                                               (case (({-# LINE 37 "src-ag/Desugar.ag" #-}
                                                       Seq.empty
                                                       {-# LINE 2692 "src-ag/Desugar.hs" #-}
                                                       )) of
                                                { !_lhsOerrors ->
                                                (case (({-# LINE 39 "src-ag/Desugar.ag" #-}
                                                        []
                                                        {-# LINE 2697 "src-ag/Desugar.hs" #-}
                                                        )) of
                                                 { !_output ->
                                                 (case (({-# LINE 39 "src-ag/Desugar.ag" #-}
                                                         _output
                                                         {-# LINE 2702 "src-ag/Desugar.hs" #-}
                                                         )) of
                                                  { !_lhsOoutput ->
                                                  ( _lhsOallAttributes,_lhsOerrors,_lhsOoutput) }) }) }) })) )
                      in  sem_Rules_Nil_1)) of
               { ( !sem_Rules_1) ->
               ( _lhsOdefsCollect,sem_Rules_1) }) }) )
-- TypeSig -----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         output               : SELF 
   alternatives:
      alternative TypeSig:
         child name           : {Identifier}
         child tp             : {Type}
         visit 0:
            local output      : _
-}
-- cata
sem_TypeSig :: TypeSig  ->
               T_TypeSig 
sem_TypeSig !(TypeSig _name _tp )  =
    (sem_TypeSig_TypeSig _name _tp )
-- semantic domain
newtype T_TypeSig  = T_TypeSig (( TypeSig ))
data Inh_TypeSig  = Inh_TypeSig {}
data Syn_TypeSig  = Syn_TypeSig {output_Syn_TypeSig :: !(TypeSig )}
wrap_TypeSig :: T_TypeSig  ->
                Inh_TypeSig  ->
                Syn_TypeSig 
wrap_TypeSig !(T_TypeSig sem ) !(Inh_TypeSig )  =
    (let ( !_lhsOoutput) = sem 
     in  (Syn_TypeSig _lhsOoutput ))
sem_TypeSig_TypeSig :: Identifier ->
                       Type ->
                       T_TypeSig 
sem_TypeSig_TypeSig !name_ !tp_  =
    (T_TypeSig (case (({-# LINE 39 "src-ag/Desugar.ag" #-}
                       TypeSig name_ tp_
                       {-# LINE 2742 "src-ag/Desugar.hs" #-}
                       )) of
                { !_output ->
                (case (({-# LINE 39 "src-ag/Desugar.ag" #-}
                        _output
                        {-# LINE 2747 "src-ag/Desugar.hs" #-}
                        )) of
                 { !_lhsOoutput ->
                 ( _lhsOoutput) }) }) )
-- TypeSigs ----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         output               : SELF 
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
sem_TypeSigs :: TypeSigs  ->
                T_TypeSigs 
sem_TypeSigs !list  =
    (Prelude.foldr sem_TypeSigs_Cons sem_TypeSigs_Nil (Prelude.map sem_TypeSig list) )
-- semantic domain
newtype T_TypeSigs  = T_TypeSigs (( TypeSigs ))
data Inh_TypeSigs  = Inh_TypeSigs {}
data Syn_TypeSigs  = Syn_TypeSigs {output_Syn_TypeSigs :: !(TypeSigs )}
wrap_TypeSigs :: T_TypeSigs  ->
                 Inh_TypeSigs  ->
                 Syn_TypeSigs 
wrap_TypeSigs !(T_TypeSigs sem ) !(Inh_TypeSigs )  =
    (let ( !_lhsOoutput) = sem 
     in  (Syn_TypeSigs _lhsOoutput ))
sem_TypeSigs_Cons :: T_TypeSig  ->
                     T_TypeSigs  ->
                     T_TypeSigs 
sem_TypeSigs_Cons !(T_TypeSig hd_ ) !(T_TypeSigs tl_ )  =
    (T_TypeSigs (case (tl_ ) of
                 { ( !_tlIoutput) ->
                     (case (hd_ ) of
                      { ( !_hdIoutput) ->
                          (case (({-# LINE 39 "src-ag/Desugar.ag" #-}
                                  (:) _hdIoutput _tlIoutput
                                  {-# LINE 2791 "src-ag/Desugar.hs" #-}
                                  )) of
                           { !_output ->
                           (case (({-# LINE 39 "src-ag/Desugar.ag" #-}
                                   _output
                                   {-# LINE 2796 "src-ag/Desugar.hs" #-}
                                   )) of
                            { !_lhsOoutput ->
                            ( _lhsOoutput) }) }) }) }) )
sem_TypeSigs_Nil :: T_TypeSigs 
sem_TypeSigs_Nil  =
    (T_TypeSigs (case (({-# LINE 39 "src-ag/Desugar.ag" #-}
                        []
                        {-# LINE 2804 "src-ag/Desugar.hs" #-}
                        )) of
                 { !_output ->
                 (case (({-# LINE 39 "src-ag/Desugar.ag" #-}
                         _output
                         {-# LINE 2809 "src-ag/Desugar.hs" #-}
                         )) of
                  { !_lhsOoutput ->
                  ( _lhsOoutput) }) }) )