

-- UUAGC 0.9.42.1 (src-ag/AbstractSyntaxDump.ag)
module AbstractSyntaxDump where
{-# LINE 6 "./src-ag/AbstractSyntaxDump.ag" #-}

import Data.List
import qualified Data.Map as Map

import Pretty
import PPUtil

import AbstractSyntax
import TokenDef
{-# LINE 16 "dist/build/AbstractSyntaxDump.hs" #-}

{-# LINE 2 "./src-ag/AbstractSyntax.ag" #-}

-- AbstractSyntax.ag imports
import Data.Set(Set)
import Data.Map(Map)
import Patterns    (Pattern(..),Patterns)
import Expression  (Expression(..))
import Macro --marcos
import CommonTypes
import ErrorMessages
{-# LINE 28 "dist/build/AbstractSyntaxDump.hs" #-}

{-# LINE 2 "./src-ag/Patterns.ag" #-}

-- Patterns.ag imports
import UU.Scanner.Position(Pos)
import CommonTypes (ConstructorIdent,Identifier)
{-# LINE 35 "dist/build/AbstractSyntaxDump.hs" #-}

{-# LINE 2 "./src-ag/Expression.ag" #-}

import UU.Scanner.Position(Pos)
import HsToken
{-# LINE 41 "dist/build/AbstractSyntaxDump.hs" #-}
-- Child -------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative Child:
         child name           : {Identifier}
         child tp             : {Type}
         child kind           : {ChildKind}
-}
-- cata
sem_Child :: Child ->
             T_Child
sem_Child (Child _name _tp _kind) =
    (sem_Child_Child _name _tp _kind)
-- semantic domain
newtype T_Child = T_Child (( PP_Doc))
data Inh_Child = Inh_Child {}
data Syn_Child = Syn_Child {pp_Syn_Child :: PP_Doc}
wrap_Child :: T_Child ->
              Inh_Child ->
              Syn_Child
wrap_Child (T_Child sem) (Inh_Child) =
    (let ( _lhsOpp) = sem
     in  (Syn_Child _lhsOpp))
sem_Child_Child :: Identifier ->
                   Type ->
                   ChildKind ->
                   T_Child
sem_Child_Child name_ tp_ kind_ =
    (T_Child (let _lhsOpp :: PP_Doc
                  -- "./src-ag/AbstractSyntaxDump.ag"(line 35, column 33)
                  _lhsOpp =
                      ({-# LINE 35 "./src-ag/AbstractSyntaxDump.ag" #-}
                       ppNestInfo ["Child","Child"] [pp name_, ppShow tp_] [ppF "kind" $ ppShow kind_] []
                       {-# LINE 78 "dist/build/AbstractSyntaxDump.hs" #-}
                       )
              in  ( _lhsOpp)))
-- Children ----------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         pp                   : PP_Doc
         ppL                  : [PP_Doc]
   alternatives:
      alternative Cons:
         child hd             : Child 
         child tl             : Children 
      alternative Nil:
-}
-- cata
sem_Children :: Children ->
                T_Children
sem_Children list =
    (Prelude.foldr sem_Children_Cons sem_Children_Nil (Prelude.map sem_Child list))
-- semantic domain
newtype T_Children = T_Children (( PP_Doc,([PP_Doc])))
data Inh_Children = Inh_Children {}
data Syn_Children = Syn_Children {pp_Syn_Children :: PP_Doc,ppL_Syn_Children :: ([PP_Doc])}
wrap_Children :: T_Children ->
                 Inh_Children ->
                 Syn_Children
wrap_Children (T_Children sem) (Inh_Children) =
    (let ( _lhsOpp,_lhsOppL) = sem
     in  (Syn_Children _lhsOpp _lhsOppL))
sem_Children_Cons :: T_Child ->
                     T_Children ->
                     T_Children
sem_Children_Cons (T_Child hd_) (T_Children tl_) =
    (T_Children (let _lhsOppL :: ([PP_Doc])
                     _lhsOpp :: PP_Doc
                     _hdIpp :: PP_Doc
                     _tlIpp :: PP_Doc
                     _tlIppL :: ([PP_Doc])
                     -- "./src-ag/AbstractSyntaxDump.ag"(line 67, column 33)
                     _lhsOppL =
                         ({-# LINE 67 "./src-ag/AbstractSyntaxDump.ag" #-}
                          _hdIpp : _tlIppL
                          {-# LINE 121 "dist/build/AbstractSyntaxDump.hs" #-}
                          )
                     -- use rule "./src-ag/AbstractSyntaxDump.ag"(line 17, column 58)
                     _lhsOpp =
                         ({-# LINE 17 "./src-ag/AbstractSyntaxDump.ag" #-}
                          _hdIpp >-< _tlIpp
                          {-# LINE 127 "dist/build/AbstractSyntaxDump.hs" #-}
                          )
                     ( _hdIpp) =
                         hd_
                     ( _tlIpp,_tlIppL) =
                         tl_
                 in  ( _lhsOpp,_lhsOppL)))
sem_Children_Nil :: T_Children
sem_Children_Nil =
    (T_Children (let _lhsOppL :: ([PP_Doc])
                     _lhsOpp :: PP_Doc
                     -- "./src-ag/AbstractSyntaxDump.ag"(line 68, column 33)
                     _lhsOppL =
                         ({-# LINE 68 "./src-ag/AbstractSyntaxDump.ag" #-}
                          []
                          {-# LINE 142 "dist/build/AbstractSyntaxDump.hs" #-}
                          )
                     -- use rule "./src-ag/AbstractSyntaxDump.ag"(line 17, column 58)
                     _lhsOpp =
                         ({-# LINE 17 "./src-ag/AbstractSyntaxDump.ag" #-}
                          empty
                          {-# LINE 148 "dist/build/AbstractSyntaxDump.hs" #-}
                          )
                 in  ( _lhsOpp,_lhsOppL)))
-- Expression --------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative Expression:
         child pos            : {Pos}
         child tks            : {[HsToken]}
-}
-- cata
sem_Expression :: Expression ->
                  T_Expression
sem_Expression (Expression _pos _tks) =
    (sem_Expression_Expression _pos _tks)
-- semantic domain
newtype T_Expression = T_Expression (( PP_Doc))
data Inh_Expression = Inh_Expression {}
data Syn_Expression = Syn_Expression {pp_Syn_Expression :: PP_Doc}
wrap_Expression :: T_Expression ->
                   Inh_Expression ->
                   Syn_Expression
wrap_Expression (T_Expression sem) (Inh_Expression) =
    (let ( _lhsOpp) = sem
     in  (Syn_Expression _lhsOpp))
sem_Expression_Expression :: Pos ->
                             ([HsToken]) ->
                             T_Expression
sem_Expression_Expression pos_ tks_ =
    (T_Expression (let _lhsOpp :: PP_Doc
                       -- "./src-ag/AbstractSyntaxDump.ag"(line 50, column 25)
                       _lhsOpp =
                           ({-# LINE 50 "./src-ag/AbstractSyntaxDump.ag" #-}
                            ppNestInfo ["Expression","Expression"] [ppShow pos_] [ppF "txt" $ vlist . showTokens . tokensToStrings $ tks_] []
                            {-# LINE 185 "dist/build/AbstractSyntaxDump.hs" #-}
                            )
                   in  ( _lhsOpp)))
-- Grammar -----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         pp                   : PP_Doc
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
sem_Grammar (Grammar _typeSyns _useMap _derivings _wrappers _nonts _pragmas _manualAttrOrderMap _paramMap _contextMap _quantMap _uniqueMap _augmentsMap _aroundsMap _mergeMap) =
    (sem_Grammar_Grammar _typeSyns _useMap _derivings _wrappers (sem_Nonterminals _nonts) _pragmas _manualAttrOrderMap _paramMap _contextMap _quantMap _uniqueMap _augmentsMap _aroundsMap _mergeMap)
-- semantic domain
newtype T_Grammar = T_Grammar (( PP_Doc))
data Inh_Grammar = Inh_Grammar {}
data Syn_Grammar = Syn_Grammar {pp_Syn_Grammar :: PP_Doc}
wrap_Grammar :: T_Grammar ->
                Inh_Grammar ->
                Syn_Grammar
wrap_Grammar (T_Grammar sem) (Inh_Grammar) =
    (let ( _lhsOpp) = sem
     in  (Syn_Grammar _lhsOpp))
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
sem_Grammar_Grammar typeSyns_ useMap_ derivings_ wrappers_ (T_Nonterminals nonts_) pragmas_ manualAttrOrderMap_ paramMap_ contextMap_ quantMap_ uniqueMap_ augmentsMap_ aroundsMap_ mergeMap_ =
    (T_Grammar (let _lhsOpp :: PP_Doc
                    _nontsIpp :: PP_Doc
                    _nontsIppL :: ([PP_Doc])
                    -- "./src-ag/AbstractSyntaxDump.ag"(line 20, column 25)
                    _lhsOpp =
                        ({-# LINE 20 "./src-ag/AbstractSyntaxDump.ag" #-}
                         ppNestInfo ["Grammar","Grammar"] []
                            [ ppF "typeSyns" $ ppAssocL typeSyns_
                            , ppF "useMap" $ ppMap $ Map.map ppMap $ useMap_
                            , ppF "derivings" $ ppMap $ derivings_
                            , ppF "wrappers" $ ppShow $ wrappers_
                            , ppF "nonts" $ ppVList _nontsIppL
                            ] []
                         {-# LINE 254 "dist/build/AbstractSyntaxDump.hs" #-}
                         )
                    ( _nontsIpp,_nontsIppL) =
                        nonts_
                in  ( _lhsOpp)))
-- Nonterminal -------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative Nonterminal:
         child nt             : {NontermIdent}
         child params         : {[Identifier]}
         child inh            : {Attributes}
         child syn            : {Attributes}
         child prods          : Productions 
-}
-- cata
sem_Nonterminal :: Nonterminal ->
                   T_Nonterminal
sem_Nonterminal (Nonterminal _nt _params _inh _syn _prods) =
    (sem_Nonterminal_Nonterminal _nt _params _inh _syn (sem_Productions _prods))
-- semantic domain
newtype T_Nonterminal = T_Nonterminal (( PP_Doc))
data Inh_Nonterminal = Inh_Nonterminal {}
data Syn_Nonterminal = Syn_Nonterminal {pp_Syn_Nonterminal :: PP_Doc}
wrap_Nonterminal :: T_Nonterminal ->
                    Inh_Nonterminal ->
                    Syn_Nonterminal
wrap_Nonterminal (T_Nonterminal sem) (Inh_Nonterminal) =
    (let ( _lhsOpp) = sem
     in  (Syn_Nonterminal _lhsOpp))
sem_Nonterminal_Nonterminal :: NontermIdent ->
                               ([Identifier]) ->
                               Attributes ->
                               Attributes ->
                               T_Productions ->
                               T_Nonterminal
sem_Nonterminal_Nonterminal nt_ params_ inh_ syn_ (T_Productions prods_) =
    (T_Nonterminal (let _lhsOpp :: PP_Doc
                        _prodsIpp :: PP_Doc
                        _prodsIppL :: ([PP_Doc])
                        -- "./src-ag/AbstractSyntaxDump.ag"(line 29, column 25)
                        _lhsOpp =
                            ({-# LINE 29 "./src-ag/AbstractSyntaxDump.ag" #-}
                             ppNestInfo ["Nonterminal","Nonterminal"] (pp nt_ : map pp params_) [ppF "inh" $ ppMap inh_, ppF "syn" $ ppMap syn_, ppF "prods" $ ppVList _prodsIppL] []
                             {-# LINE 301 "dist/build/AbstractSyntaxDump.hs" #-}
                             )
                        ( _prodsIpp,_prodsIppL) =
                            prods_
                    in  ( _lhsOpp)))
-- Nonterminals ------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         pp                   : PP_Doc
         ppL                  : [PP_Doc]
   alternatives:
      alternative Cons:
         child hd             : Nonterminal 
         child tl             : Nonterminals 
      alternative Nil:
-}
-- cata
sem_Nonterminals :: Nonterminals ->
                    T_Nonterminals
sem_Nonterminals list =
    (Prelude.foldr sem_Nonterminals_Cons sem_Nonterminals_Nil (Prelude.map sem_Nonterminal list))
-- semantic domain
newtype T_Nonterminals = T_Nonterminals (( PP_Doc,([PP_Doc])))
data Inh_Nonterminals = Inh_Nonterminals {}
data Syn_Nonterminals = Syn_Nonterminals {pp_Syn_Nonterminals :: PP_Doc,ppL_Syn_Nonterminals :: ([PP_Doc])}
wrap_Nonterminals :: T_Nonterminals ->
                     Inh_Nonterminals ->
                     Syn_Nonterminals
wrap_Nonterminals (T_Nonterminals sem) (Inh_Nonterminals) =
    (let ( _lhsOpp,_lhsOppL) = sem
     in  (Syn_Nonterminals _lhsOpp _lhsOppL))
sem_Nonterminals_Cons :: T_Nonterminal ->
                         T_Nonterminals ->
                         T_Nonterminals
sem_Nonterminals_Cons (T_Nonterminal hd_) (T_Nonterminals tl_) =
    (T_Nonterminals (let _lhsOppL :: ([PP_Doc])
                         _lhsOpp :: PP_Doc
                         _hdIpp :: PP_Doc
                         _tlIpp :: PP_Doc
                         _tlIppL :: ([PP_Doc])
                         -- "./src-ag/AbstractSyntaxDump.ag"(line 75, column 33)
                         _lhsOppL =
                             ({-# LINE 75 "./src-ag/AbstractSyntaxDump.ag" #-}
                              _hdIpp : _tlIppL
                              {-# LINE 346 "dist/build/AbstractSyntaxDump.hs" #-}
                              )
                         -- use rule "./src-ag/AbstractSyntaxDump.ag"(line 17, column 58)
                         _lhsOpp =
                             ({-# LINE 17 "./src-ag/AbstractSyntaxDump.ag" #-}
                              _hdIpp >-< _tlIpp
                              {-# LINE 352 "dist/build/AbstractSyntaxDump.hs" #-}
                              )
                         ( _hdIpp) =
                             hd_
                         ( _tlIpp,_tlIppL) =
                             tl_
                     in  ( _lhsOpp,_lhsOppL)))
sem_Nonterminals_Nil :: T_Nonterminals
sem_Nonterminals_Nil =
    (T_Nonterminals (let _lhsOppL :: ([PP_Doc])
                         _lhsOpp :: PP_Doc
                         -- "./src-ag/AbstractSyntaxDump.ag"(line 76, column 33)
                         _lhsOppL =
                             ({-# LINE 76 "./src-ag/AbstractSyntaxDump.ag" #-}
                              []
                              {-# LINE 367 "dist/build/AbstractSyntaxDump.hs" #-}
                              )
                         -- use rule "./src-ag/AbstractSyntaxDump.ag"(line 17, column 58)
                         _lhsOpp =
                             ({-# LINE 17 "./src-ag/AbstractSyntaxDump.ag" #-}
                              empty
                              {-# LINE 373 "dist/build/AbstractSyntaxDump.hs" #-}
                              )
                     in  ( _lhsOpp,_lhsOppL)))
-- Pattern -----------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         copy                 : Pattern 
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
sem_Pattern (Constr _name _pats) =
    (sem_Pattern_Constr _name (sem_Patterns _pats))
sem_Pattern (Product _pos _pats) =
    (sem_Pattern_Product _pos (sem_Patterns _pats))
sem_Pattern (Alias _field _attr _pat) =
    (sem_Pattern_Alias _field _attr (sem_Pattern _pat))
sem_Pattern (Irrefutable _pat) =
    (sem_Pattern_Irrefutable (sem_Pattern _pat))
sem_Pattern (Underscore _pos) =
    (sem_Pattern_Underscore _pos)
-- semantic domain
newtype T_Pattern = T_Pattern (( Pattern,PP_Doc))
data Inh_Pattern = Inh_Pattern {}
data Syn_Pattern = Syn_Pattern {copy_Syn_Pattern :: Pattern,pp_Syn_Pattern :: PP_Doc}
wrap_Pattern :: T_Pattern ->
                Inh_Pattern ->
                Syn_Pattern
wrap_Pattern (T_Pattern sem) (Inh_Pattern) =
    (let ( _lhsOcopy,_lhsOpp) = sem
     in  (Syn_Pattern _lhsOcopy _lhsOpp))
sem_Pattern_Constr :: ConstructorIdent ->
                      T_Patterns ->
                      T_Pattern
sem_Pattern_Constr name_ (T_Patterns pats_) =
    (T_Pattern (let _lhsOpp :: PP_Doc
                    _lhsOcopy :: Pattern
                    _patsIcopy :: Patterns
                    _patsIpp :: PP_Doc
                    _patsIppL :: ([PP_Doc])
                    -- "./src-ag/AbstractSyntaxDump.ag"(line 44, column 33)
                    _lhsOpp =
                        ({-# LINE 44 "./src-ag/AbstractSyntaxDump.ag" #-}
                         ppNestInfo ["Pattern","Constr"] [pp name_] [ppF "pats" $ ppVList _patsIppL] []
                         {-# LINE 444 "dist/build/AbstractSyntaxDump.hs" #-}
                         )
                    -- self rule
                    _copy =
                        ({-# LINE 22 "./src-ag/Patterns.ag" #-}
                         Constr name_ _patsIcopy
                         {-# LINE 450 "dist/build/AbstractSyntaxDump.hs" #-}
                         )
                    -- self rule
                    _lhsOcopy =
                        ({-# LINE 22 "./src-ag/Patterns.ag" #-}
                         _copy
                         {-# LINE 456 "dist/build/AbstractSyntaxDump.hs" #-}
                         )
                    ( _patsIcopy,_patsIpp,_patsIppL) =
                        pats_
                in  ( _lhsOcopy,_lhsOpp)))
sem_Pattern_Product :: Pos ->
                       T_Patterns ->
                       T_Pattern
sem_Pattern_Product pos_ (T_Patterns pats_) =
    (T_Pattern (let _lhsOpp :: PP_Doc
                    _lhsOcopy :: Pattern
                    _patsIcopy :: Patterns
                    _patsIpp :: PP_Doc
                    _patsIppL :: ([PP_Doc])
                    -- "./src-ag/AbstractSyntaxDump.ag"(line 45, column 33)
                    _lhsOpp =
                        ({-# LINE 45 "./src-ag/AbstractSyntaxDump.ag" #-}
                         ppNestInfo ["Pattern","Product"] [ppShow pos_] [ppF "pats" $ ppVList _patsIppL] []
                         {-# LINE 474 "dist/build/AbstractSyntaxDump.hs" #-}
                         )
                    -- self rule
                    _copy =
                        ({-# LINE 22 "./src-ag/Patterns.ag" #-}
                         Product pos_ _patsIcopy
                         {-# LINE 480 "dist/build/AbstractSyntaxDump.hs" #-}
                         )
                    -- self rule
                    _lhsOcopy =
                        ({-# LINE 22 "./src-ag/Patterns.ag" #-}
                         _copy
                         {-# LINE 486 "dist/build/AbstractSyntaxDump.hs" #-}
                         )
                    ( _patsIcopy,_patsIpp,_patsIppL) =
                        pats_
                in  ( _lhsOcopy,_lhsOpp)))
sem_Pattern_Alias :: Identifier ->
                     Identifier ->
                     T_Pattern ->
                     T_Pattern
sem_Pattern_Alias field_ attr_ (T_Pattern pat_) =
    (T_Pattern (let _lhsOpp :: PP_Doc
                    _lhsOcopy :: Pattern
                    _patIcopy :: Pattern
                    _patIpp :: PP_Doc
                    -- "./src-ag/AbstractSyntaxDump.ag"(line 46, column 33)
                    _lhsOpp =
                        ({-# LINE 46 "./src-ag/AbstractSyntaxDump.ag" #-}
                         ppNestInfo ["Pattern","Alias"] [pp field_, pp attr_] [ppF "pat" $ _patIpp] []
                         {-# LINE 504 "dist/build/AbstractSyntaxDump.hs" #-}
                         )
                    -- self rule
                    _copy =
                        ({-# LINE 22 "./src-ag/Patterns.ag" #-}
                         Alias field_ attr_ _patIcopy
                         {-# LINE 510 "dist/build/AbstractSyntaxDump.hs" #-}
                         )
                    -- self rule
                    _lhsOcopy =
                        ({-# LINE 22 "./src-ag/Patterns.ag" #-}
                         _copy
                         {-# LINE 516 "dist/build/AbstractSyntaxDump.hs" #-}
                         )
                    ( _patIcopy,_patIpp) =
                        pat_
                in  ( _lhsOcopy,_lhsOpp)))
sem_Pattern_Irrefutable :: T_Pattern ->
                           T_Pattern
sem_Pattern_Irrefutable (T_Pattern pat_) =
    (T_Pattern (let _lhsOpp :: PP_Doc
                    _lhsOcopy :: Pattern
                    _patIcopy :: Pattern
                    _patIpp :: PP_Doc
                    -- use rule "./src-ag/AbstractSyntaxDump.ag"(line 17, column 58)
                    _lhsOpp =
                        ({-# LINE 17 "./src-ag/AbstractSyntaxDump.ag" #-}
                         _patIpp
                         {-# LINE 532 "dist/build/AbstractSyntaxDump.hs" #-}
                         )
                    -- self rule
                    _copy =
                        ({-# LINE 22 "./src-ag/Patterns.ag" #-}
                         Irrefutable _patIcopy
                         {-# LINE 538 "dist/build/AbstractSyntaxDump.hs" #-}
                         )
                    -- self rule
                    _lhsOcopy =
                        ({-# LINE 22 "./src-ag/Patterns.ag" #-}
                         _copy
                         {-# LINE 544 "dist/build/AbstractSyntaxDump.hs" #-}
                         )
                    ( _patIcopy,_patIpp) =
                        pat_
                in  ( _lhsOcopy,_lhsOpp)))
sem_Pattern_Underscore :: Pos ->
                          T_Pattern
sem_Pattern_Underscore pos_ =
    (T_Pattern (let _lhsOpp :: PP_Doc
                    _lhsOcopy :: Pattern
                    -- "./src-ag/AbstractSyntaxDump.ag"(line 47, column 25)
                    _lhsOpp =
                        ({-# LINE 47 "./src-ag/AbstractSyntaxDump.ag" #-}
                         ppNestInfo ["Pattern","Underscore"] [ppShow pos_] [] []
                         {-# LINE 558 "dist/build/AbstractSyntaxDump.hs" #-}
                         )
                    -- self rule
                    _copy =
                        ({-# LINE 22 "./src-ag/Patterns.ag" #-}
                         Underscore pos_
                         {-# LINE 564 "dist/build/AbstractSyntaxDump.hs" #-}
                         )
                    -- self rule
                    _lhsOcopy =
                        ({-# LINE 22 "./src-ag/Patterns.ag" #-}
                         _copy
                         {-# LINE 570 "dist/build/AbstractSyntaxDump.hs" #-}
                         )
                in  ( _lhsOcopy,_lhsOpp)))
-- Patterns ----------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         copy                 : Patterns 
         pp                   : PP_Doc
         ppL                  : [PP_Doc]
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
sem_Patterns list =
    (Prelude.foldr sem_Patterns_Cons sem_Patterns_Nil (Prelude.map sem_Pattern list))
-- semantic domain
newtype T_Patterns = T_Patterns (( Patterns,PP_Doc,([PP_Doc])))
data Inh_Patterns = Inh_Patterns {}
data Syn_Patterns = Syn_Patterns {copy_Syn_Patterns :: Patterns,pp_Syn_Patterns :: PP_Doc,ppL_Syn_Patterns :: ([PP_Doc])}
wrap_Patterns :: T_Patterns ->
                 Inh_Patterns ->
                 Syn_Patterns
wrap_Patterns (T_Patterns sem) (Inh_Patterns) =
    (let ( _lhsOcopy,_lhsOpp,_lhsOppL) = sem
     in  (Syn_Patterns _lhsOcopy _lhsOpp _lhsOppL))
sem_Patterns_Cons :: T_Pattern ->
                     T_Patterns ->
                     T_Patterns
sem_Patterns_Cons (T_Pattern hd_) (T_Patterns tl_) =
    (T_Patterns (let _lhsOppL :: ([PP_Doc])
                     _lhsOpp :: PP_Doc
                     _lhsOcopy :: Patterns
                     _hdIcopy :: Pattern
                     _hdIpp :: PP_Doc
                     _tlIcopy :: Patterns
                     _tlIpp :: PP_Doc
                     _tlIppL :: ([PP_Doc])
                     -- "./src-ag/AbstractSyntaxDump.ag"(line 55, column 33)
                     _lhsOppL =
                         ({-# LINE 55 "./src-ag/AbstractSyntaxDump.ag" #-}
                          _hdIpp : _tlIppL
                          {-# LINE 621 "dist/build/AbstractSyntaxDump.hs" #-}
                          )
                     -- use rule "./src-ag/AbstractSyntaxDump.ag"(line 17, column 58)
                     _lhsOpp =
                         ({-# LINE 17 "./src-ag/AbstractSyntaxDump.ag" #-}
                          _hdIpp >-< _tlIpp
                          {-# LINE 627 "dist/build/AbstractSyntaxDump.hs" #-}
                          )
                     -- self rule
                     _copy =
                         ({-# LINE 22 "./src-ag/Patterns.ag" #-}
                          (:) _hdIcopy _tlIcopy
                          {-# LINE 633 "dist/build/AbstractSyntaxDump.hs" #-}
                          )
                     -- self rule
                     _lhsOcopy =
                         ({-# LINE 22 "./src-ag/Patterns.ag" #-}
                          _copy
                          {-# LINE 639 "dist/build/AbstractSyntaxDump.hs" #-}
                          )
                     ( _hdIcopy,_hdIpp) =
                         hd_
                     ( _tlIcopy,_tlIpp,_tlIppL) =
                         tl_
                 in  ( _lhsOcopy,_lhsOpp,_lhsOppL)))
sem_Patterns_Nil :: T_Patterns
sem_Patterns_Nil =
    (T_Patterns (let _lhsOppL :: ([PP_Doc])
                     _lhsOpp :: PP_Doc
                     _lhsOcopy :: Patterns
                     -- "./src-ag/AbstractSyntaxDump.ag"(line 56, column 33)
                     _lhsOppL =
                         ({-# LINE 56 "./src-ag/AbstractSyntaxDump.ag" #-}
                          []
                          {-# LINE 655 "dist/build/AbstractSyntaxDump.hs" #-}
                          )
                     -- use rule "./src-ag/AbstractSyntaxDump.ag"(line 17, column 58)
                     _lhsOpp =
                         ({-# LINE 17 "./src-ag/AbstractSyntaxDump.ag" #-}
                          empty
                          {-# LINE 661 "dist/build/AbstractSyntaxDump.hs" #-}
                          )
                     -- self rule
                     _copy =
                         ({-# LINE 22 "./src-ag/Patterns.ag" #-}
                          []
                          {-# LINE 667 "dist/build/AbstractSyntaxDump.hs" #-}
                          )
                     -- self rule
                     _lhsOcopy =
                         ({-# LINE 22 "./src-ag/Patterns.ag" #-}
                          _copy
                          {-# LINE 673 "dist/build/AbstractSyntaxDump.hs" #-}
                          )
                 in  ( _lhsOcopy,_lhsOpp,_lhsOppL)))
-- Production --------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative Production:
         child con            : {ConstructorIdent}
         child params         : {[Identifier]}
         child constraints    : {[Type]}
         child children       : Children 
         child rules          : Rules 
         child typeSigs       : TypeSigs 
         child macro          : {MaybeMacro}
-}
-- cata
sem_Production :: Production ->
                  T_Production
sem_Production (Production _con _params _constraints _children _rules _typeSigs _macro) =
    (sem_Production_Production _con _params _constraints (sem_Children _children) (sem_Rules _rules) (sem_TypeSigs _typeSigs) _macro)
-- semantic domain
newtype T_Production = T_Production (( PP_Doc))
data Inh_Production = Inh_Production {}
data Syn_Production = Syn_Production {pp_Syn_Production :: PP_Doc}
wrap_Production :: T_Production ->
                   Inh_Production ->
                   Syn_Production
wrap_Production (T_Production sem) (Inh_Production) =
    (let ( _lhsOpp) = sem
     in  (Syn_Production _lhsOpp))
sem_Production_Production :: ConstructorIdent ->
                             ([Identifier]) ->
                             ([Type]) ->
                             T_Children ->
                             T_Rules ->
                             T_TypeSigs ->
                             MaybeMacro ->
                             T_Production
sem_Production_Production con_ params_ constraints_ (T_Children children_) (T_Rules rules_) (T_TypeSigs typeSigs_) macro_ =
    (T_Production (let _lhsOpp :: PP_Doc
                       _childrenIpp :: PP_Doc
                       _childrenIppL :: ([PP_Doc])
                       _rulesIpp :: PP_Doc
                       _rulesIppL :: ([PP_Doc])
                       _typeSigsIpp :: PP_Doc
                       _typeSigsIppL :: ([PP_Doc])
                       -- "./src-ag/AbstractSyntaxDump.ag"(line 32, column 25)
                       _lhsOpp =
                           ({-# LINE 32 "./src-ag/AbstractSyntaxDump.ag" #-}
                            ppNestInfo ["Production","Production"] [pp con_] [ppF "children" $ ppVList _childrenIppL,ppF "rules" $ ppVList _rulesIppL,ppF "typeSigs" $ ppVList _typeSigsIppL] []
                            {-# LINE 726 "dist/build/AbstractSyntaxDump.hs" #-}
                            )
                       ( _childrenIpp,_childrenIppL) =
                           children_
                       ( _rulesIpp,_rulesIppL) =
                           rules_
                       ( _typeSigsIpp,_typeSigsIppL) =
                           typeSigs_
                   in  ( _lhsOpp)))
-- Productions -------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         pp                   : PP_Doc
         ppL                  : [PP_Doc]
   alternatives:
      alternative Cons:
         child hd             : Production 
         child tl             : Productions 
      alternative Nil:
-}
-- cata
sem_Productions :: Productions ->
                   T_Productions
sem_Productions list =
    (Prelude.foldr sem_Productions_Cons sem_Productions_Nil (Prelude.map sem_Production list))
-- semantic domain
newtype T_Productions = T_Productions (( PP_Doc,([PP_Doc])))
data Inh_Productions = Inh_Productions {}
data Syn_Productions = Syn_Productions {pp_Syn_Productions :: PP_Doc,ppL_Syn_Productions :: ([PP_Doc])}
wrap_Productions :: T_Productions ->
                    Inh_Productions ->
                    Syn_Productions
wrap_Productions (T_Productions sem) (Inh_Productions) =
    (let ( _lhsOpp,_lhsOppL) = sem
     in  (Syn_Productions _lhsOpp _lhsOppL))
sem_Productions_Cons :: T_Production ->
                        T_Productions ->
                        T_Productions
sem_Productions_Cons (T_Production hd_) (T_Productions tl_) =
    (T_Productions (let _lhsOppL :: ([PP_Doc])
                        _lhsOpp :: PP_Doc
                        _hdIpp :: PP_Doc
                        _tlIpp :: PP_Doc
                        _tlIppL :: ([PP_Doc])
                        -- "./src-ag/AbstractSyntaxDump.ag"(line 71, column 33)
                        _lhsOppL =
                            ({-# LINE 71 "./src-ag/AbstractSyntaxDump.ag" #-}
                             _hdIpp : _tlIppL
                             {-# LINE 775 "dist/build/AbstractSyntaxDump.hs" #-}
                             )
                        -- use rule "./src-ag/AbstractSyntaxDump.ag"(line 17, column 58)
                        _lhsOpp =
                            ({-# LINE 17 "./src-ag/AbstractSyntaxDump.ag" #-}
                             _hdIpp >-< _tlIpp
                             {-# LINE 781 "dist/build/AbstractSyntaxDump.hs" #-}
                             )
                        ( _hdIpp) =
                            hd_
                        ( _tlIpp,_tlIppL) =
                            tl_
                    in  ( _lhsOpp,_lhsOppL)))
sem_Productions_Nil :: T_Productions
sem_Productions_Nil =
    (T_Productions (let _lhsOppL :: ([PP_Doc])
                        _lhsOpp :: PP_Doc
                        -- "./src-ag/AbstractSyntaxDump.ag"(line 72, column 33)
                        _lhsOppL =
                            ({-# LINE 72 "./src-ag/AbstractSyntaxDump.ag" #-}
                             []
                             {-# LINE 796 "dist/build/AbstractSyntaxDump.hs" #-}
                             )
                        -- use rule "./src-ag/AbstractSyntaxDump.ag"(line 17, column 58)
                        _lhsOpp =
                            ({-# LINE 17 "./src-ag/AbstractSyntaxDump.ag" #-}
                             empty
                             {-# LINE 802 "dist/build/AbstractSyntaxDump.hs" #-}
                             )
                    in  ( _lhsOpp,_lhsOppL)))
-- Rule --------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         pp                   : PP_Doc
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
-}
-- cata
sem_Rule :: Rule ->
            T_Rule
sem_Rule (Rule _mbName _pattern _rhs _owrt _origin _explicit _pure _identity _mbError _eager) =
    (sem_Rule_Rule _mbName (sem_Pattern _pattern) (sem_Expression _rhs) _owrt _origin _explicit _pure _identity _mbError _eager)
-- semantic domain
newtype T_Rule = T_Rule (( PP_Doc))
data Inh_Rule = Inh_Rule {}
data Syn_Rule = Syn_Rule {pp_Syn_Rule :: PP_Doc}
wrap_Rule :: T_Rule ->
             Inh_Rule ->
             Syn_Rule
wrap_Rule (T_Rule sem) (Inh_Rule) =
    (let ( _lhsOpp) = sem
     in  (Syn_Rule _lhsOpp))
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
sem_Rule_Rule mbName_ (T_Pattern pattern_) (T_Expression rhs_) owrt_ origin_ explicit_ pure_ identity_ mbError_ eager_ =
    (T_Rule (let _lhsOpp :: PP_Doc
                 _patternIcopy :: Pattern
                 _patternIpp :: PP_Doc
                 _rhsIpp :: PP_Doc
                 -- "./src-ag/AbstractSyntaxDump.ag"(line 38, column 33)
                 _lhsOpp =
                     ({-# LINE 38 "./src-ag/AbstractSyntaxDump.ag" #-}
                      ppNestInfo ["Rule","Rule"] [ppShow owrt_, pp origin_] [ppF "pattern" $ _patternIpp, ppF "rhs" $ _rhsIpp] []
                      {-# LINE 858 "dist/build/AbstractSyntaxDump.hs" #-}
                      )
                 ( _patternIcopy,_patternIpp) =
                     pattern_
                 ( _rhsIpp) =
                     rhs_
             in  ( _lhsOpp)))
-- Rules -------------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         pp                   : PP_Doc
         ppL                  : [PP_Doc]
   alternatives:
      alternative Cons:
         child hd             : Rule 
         child tl             : Rules 
      alternative Nil:
-}
-- cata
sem_Rules :: Rules ->
             T_Rules
sem_Rules list =
    (Prelude.foldr sem_Rules_Cons sem_Rules_Nil (Prelude.map sem_Rule list))
-- semantic domain
newtype T_Rules = T_Rules (( PP_Doc,([PP_Doc])))
data Inh_Rules = Inh_Rules {}
data Syn_Rules = Syn_Rules {pp_Syn_Rules :: PP_Doc,ppL_Syn_Rules :: ([PP_Doc])}
wrap_Rules :: T_Rules ->
              Inh_Rules ->
              Syn_Rules
wrap_Rules (T_Rules sem) (Inh_Rules) =
    (let ( _lhsOpp,_lhsOppL) = sem
     in  (Syn_Rules _lhsOpp _lhsOppL))
sem_Rules_Cons :: T_Rule ->
                  T_Rules ->
                  T_Rules
sem_Rules_Cons (T_Rule hd_) (T_Rules tl_) =
    (T_Rules (let _lhsOppL :: ([PP_Doc])
                  _lhsOpp :: PP_Doc
                  _hdIpp :: PP_Doc
                  _tlIpp :: PP_Doc
                  _tlIppL :: ([PP_Doc])
                  -- "./src-ag/AbstractSyntaxDump.ag"(line 63, column 33)
                  _lhsOppL =
                      ({-# LINE 63 "./src-ag/AbstractSyntaxDump.ag" #-}
                       _hdIpp : _tlIppL
                       {-# LINE 905 "dist/build/AbstractSyntaxDump.hs" #-}
                       )
                  -- use rule "./src-ag/AbstractSyntaxDump.ag"(line 17, column 58)
                  _lhsOpp =
                      ({-# LINE 17 "./src-ag/AbstractSyntaxDump.ag" #-}
                       _hdIpp >-< _tlIpp
                       {-# LINE 911 "dist/build/AbstractSyntaxDump.hs" #-}
                       )
                  ( _hdIpp) =
                      hd_
                  ( _tlIpp,_tlIppL) =
                      tl_
              in  ( _lhsOpp,_lhsOppL)))
sem_Rules_Nil :: T_Rules
sem_Rules_Nil =
    (T_Rules (let _lhsOppL :: ([PP_Doc])
                  _lhsOpp :: PP_Doc
                  -- "./src-ag/AbstractSyntaxDump.ag"(line 64, column 33)
                  _lhsOppL =
                      ({-# LINE 64 "./src-ag/AbstractSyntaxDump.ag" #-}
                       []
                       {-# LINE 926 "dist/build/AbstractSyntaxDump.hs" #-}
                       )
                  -- use rule "./src-ag/AbstractSyntaxDump.ag"(line 17, column 58)
                  _lhsOpp =
                      ({-# LINE 17 "./src-ag/AbstractSyntaxDump.ag" #-}
                       empty
                       {-# LINE 932 "dist/build/AbstractSyntaxDump.hs" #-}
                       )
              in  ( _lhsOpp,_lhsOppL)))
-- TypeSig -----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative TypeSig:
         child name           : {Identifier}
         child tp             : {Type}
-}
-- cata
sem_TypeSig :: TypeSig ->
               T_TypeSig
sem_TypeSig (TypeSig _name _tp) =
    (sem_TypeSig_TypeSig _name _tp)
-- semantic domain
newtype T_TypeSig = T_TypeSig (( PP_Doc))
data Inh_TypeSig = Inh_TypeSig {}
data Syn_TypeSig = Syn_TypeSig {pp_Syn_TypeSig :: PP_Doc}
wrap_TypeSig :: T_TypeSig ->
                Inh_TypeSig ->
                Syn_TypeSig
wrap_TypeSig (T_TypeSig sem) (Inh_TypeSig) =
    (let ( _lhsOpp) = sem
     in  (Syn_TypeSig _lhsOpp))
sem_TypeSig_TypeSig :: Identifier ->
                       Type ->
                       T_TypeSig
sem_TypeSig_TypeSig name_ tp_ =
    (T_TypeSig (let _lhsOpp :: PP_Doc
                    -- "./src-ag/AbstractSyntaxDump.ag"(line 41, column 33)
                    _lhsOpp =
                        ({-# LINE 41 "./src-ag/AbstractSyntaxDump.ag" #-}
                         ppNestInfo ["TypeSig","TypeSig"] [pp name_, ppShow tp_] [] []
                         {-# LINE 969 "dist/build/AbstractSyntaxDump.hs" #-}
                         )
                in  ( _lhsOpp)))
-- TypeSigs ----------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         pp                   : PP_Doc
         ppL                  : [PP_Doc]
   alternatives:
      alternative Cons:
         child hd             : TypeSig 
         child tl             : TypeSigs 
      alternative Nil:
-}
-- cata
sem_TypeSigs :: TypeSigs ->
                T_TypeSigs
sem_TypeSigs list =
    (Prelude.foldr sem_TypeSigs_Cons sem_TypeSigs_Nil (Prelude.map sem_TypeSig list))
-- semantic domain
newtype T_TypeSigs = T_TypeSigs (( PP_Doc,([PP_Doc])))
data Inh_TypeSigs = Inh_TypeSigs {}
data Syn_TypeSigs = Syn_TypeSigs {pp_Syn_TypeSigs :: PP_Doc,ppL_Syn_TypeSigs :: ([PP_Doc])}
wrap_TypeSigs :: T_TypeSigs ->
                 Inh_TypeSigs ->
                 Syn_TypeSigs
wrap_TypeSigs (T_TypeSigs sem) (Inh_TypeSigs) =
    (let ( _lhsOpp,_lhsOppL) = sem
     in  (Syn_TypeSigs _lhsOpp _lhsOppL))
sem_TypeSigs_Cons :: T_TypeSig ->
                     T_TypeSigs ->
                     T_TypeSigs
sem_TypeSigs_Cons (T_TypeSig hd_) (T_TypeSigs tl_) =
    (T_TypeSigs (let _lhsOppL :: ([PP_Doc])
                     _lhsOpp :: PP_Doc
                     _hdIpp :: PP_Doc
                     _tlIpp :: PP_Doc
                     _tlIppL :: ([PP_Doc])
                     -- "./src-ag/AbstractSyntaxDump.ag"(line 59, column 33)
                     _lhsOppL =
                         ({-# LINE 59 "./src-ag/AbstractSyntaxDump.ag" #-}
                          _hdIpp : _tlIppL
                          {-# LINE 1012 "dist/build/AbstractSyntaxDump.hs" #-}
                          )
                     -- use rule "./src-ag/AbstractSyntaxDump.ag"(line 17, column 58)
                     _lhsOpp =
                         ({-# LINE 17 "./src-ag/AbstractSyntaxDump.ag" #-}
                          _hdIpp >-< _tlIpp
                          {-# LINE 1018 "dist/build/AbstractSyntaxDump.hs" #-}
                          )
                     ( _hdIpp) =
                         hd_
                     ( _tlIpp,_tlIppL) =
                         tl_
                 in  ( _lhsOpp,_lhsOppL)))
sem_TypeSigs_Nil :: T_TypeSigs
sem_TypeSigs_Nil =
    (T_TypeSigs (let _lhsOppL :: ([PP_Doc])
                     _lhsOpp :: PP_Doc
                     -- "./src-ag/AbstractSyntaxDump.ag"(line 60, column 33)
                     _lhsOppL =
                         ({-# LINE 60 "./src-ag/AbstractSyntaxDump.ag" #-}
                          []
                          {-# LINE 1033 "dist/build/AbstractSyntaxDump.hs" #-}
                          )
                     -- use rule "./src-ag/AbstractSyntaxDump.ag"(line 17, column 58)
                     _lhsOpp =
                         ({-# LINE 17 "./src-ag/AbstractSyntaxDump.ag" #-}
                          empty
                          {-# LINE 1039 "dist/build/AbstractSyntaxDump.hs" #-}
                          )
                 in  ( _lhsOpp,_lhsOppL)))