

-- UUAGC 0.9.39.0.0 (src-ag/TfmToVisage.ag)
module TfmToVisage where
{-# LINE 8 "src-ag/TfmToVisage.ag" #-}

import AbstractSyntax
import VisagePatterns
import VisageSyntax
{-# LINE 11 "dist/build/uuagc/uuagc-tmp/TfmToVisage.hs" #-}

{-# LINE 2 "src-ag/AbstractSyntax.ag" #-}

-- AbstractSyntax.ag imports
import Data.Set(Set)
import Data.Map(Map)
import Patterns    (Pattern(..),Patterns)
import Expression  (Expression(..))
import CommonTypes
{-# LINE 21 "dist/build/uuagc/uuagc-tmp/TfmToVisage.hs" #-}

{-# LINE 2 "src-ag/Patterns.ag" #-}

-- Patterns.ag imports
import UU.Scanner.Position(Pos)
import CommonTypes (ConstructorIdent,Identifier)
{-# LINE 28 "dist/build/uuagc/uuagc-tmp/TfmToVisage.hs" #-}

{-# LINE 2 "src-ag/Expression.ag" #-}

import UU.Scanner.Position(Pos)
import HsToken
{-# LINE 34 "dist/build/uuagc/uuagc-tmp/TfmToVisage.hs" #-}
{-# LINE 14 "src-ag/TfmToVisage.ag" #-}

-- Maps a rule to a pair 
-- Later, I expect to map to a list of rules, because we might need to unfold.


-- Checks that a certain alias is in fact a Var in the old representation of the AG system
isVar (Alias _ _ (Underscore _) _) = True
isVar _ = False

type VisageRuleMap = [(String, VisageRule)]

splitVRules :: [VisageRule] -> VisageRuleMap
splitVRules vrs = concat (map unfoldvrs vrs)

unfoldvrs :: VisageRule -> VisageRuleMap
unfoldvrs vr@(VRule attrfields _ _ _ _) = zip (map (getName . fst) attrfields) (map (copyRule vr) attrfields)

copyRule :: VisageRule -> (Identifier,Identifier) -> VisageRule
copyRule (VRule attrfields _ pat expr owrt) (field,attr) = VRule attrfields attr pat expr owrt

getForField :: String -> VisageRuleMap -> [VisageRule]
getForField field xs = map snd (filter ((field ==) . fst) xs)

{-
   Delivers a map from fieldname to VisageRule with all references to others underscored.
   So, (lhs.x, rt.y, loc.z) = (0,1,2) becomes something like
   [("lhs", (lhs.x,_,_) = (0,1,2)

   At this point, we do not use this anymore.
   
allways :: VisageRule -> VisageRuleMap
allways vr@(VRule vrfields _ _ _ _) = zip vrfields (map (underScoreRule vr) (nub vrfields))

splitVRules :: [VisageRule] -> VisageRuleMap
splitVRules vrs = concat (map allways vrs)

underScoreRule :: VisageRule -> String -> VisageRule
underScoreRule (VRule fields pat expr owrt rule) s = VRule fields (underScore s pat) expr owrt rule

underScore :: String -> VisagePattern -> VisagePattern
underScore field (VConstr name pats) = VConstr name (map (underScore field) pats) 
underScore field (VProduct pos pats) = VProduct pos (map (underScore field) pats)
underScore field vp@(VVar vfield attr)  = 
   if (field == getName vfield) 
   then vp
   else (VUnderscore (getPos vfield))
-- Should I recurse into the pat of VAlias?
underScore field vp@(VAlias afield attr pat) =
   if (field == getName afield) 
   then vp
   else (VUnderscore (getPos afield))
underScore field vp@(VUnderscore pos) = vp
                 
-}
{-# LINE 90 "dist/build/uuagc/uuagc-tmp/TfmToVisage.hs" #-}
-- Child -------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         rulemap              : VisageRuleMap
      synthesized attribute:
         vchild               : VisageChild
   alternatives:
      alternative Child:
         child name           : {Identifier}
         child tp             : {Type}
         child inh            : {Attributes}
         child syn            : {Attributes}
         child virtual        : {Maybe (Maybe Type)}
-}
-- cata
sem_Child :: Child  ->
             T_Child 
sem_Child (Child _name _tp _inh _syn _virtual )  =
    (sem_Child_Child _name _tp _inh _syn _virtual )
-- semantic domain
newtype T_Child  = T_Child (VisageRuleMap ->
                            ( VisageChild))
data Inh_Child  = Inh_Child {rulemap_Inh_Child :: VisageRuleMap}
data Syn_Child  = Syn_Child {vchild_Syn_Child :: VisageChild}
wrap_Child :: T_Child  ->
              Inh_Child  ->
              Syn_Child 
wrap_Child (T_Child sem ) (Inh_Child _lhsIrulemap )  =
    (let ( _lhsOvchild) = sem _lhsIrulemap 
     in  (Syn_Child _lhsOvchild ))
sem_Child_Child :: Identifier ->
                   Type ->
                   Attributes ->
                   Attributes ->
                   (Maybe (Maybe Type)) ->
                   T_Child 
sem_Child_Child name_ tp_ inh_ syn_ virtual_  =
    (T_Child (\ _lhsIrulemap ->
                  (let _lhsOvchild :: VisageChild
                       -- "src-ag/TfmToVisage.ag"(line 118, column 11)
                       _lhsOvchild =
                           ({-# LINE 118 "src-ag/TfmToVisage.ag" #-}
                            VChild name_ tp_ inh_ syn_ (getForField (getName name_) _lhsIrulemap)
                            {-# LINE 135 "src-ag/TfmToVisage.hs" #-}
                            )
                   in  ( _lhsOvchild))) )
-- Children ----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         rulemap              : VisageRuleMap
      synthesized attribute:
         vchildren            : [VisageChild]
   alternatives:
      alternative Cons:
         child hd             : Child 
         child tl             : Children 
      alternative Nil:
-}
-- cata
sem_Children :: Children  ->
                T_Children 
sem_Children list  =
    (Prelude.foldr sem_Children_Cons sem_Children_Nil (Prelude.map sem_Child list) )
-- semantic domain
newtype T_Children  = T_Children (VisageRuleMap ->
                                  ( ([VisageChild])))
data Inh_Children  = Inh_Children {rulemap_Inh_Children :: VisageRuleMap}
data Syn_Children  = Syn_Children {vchildren_Syn_Children :: ([VisageChild])}
wrap_Children :: T_Children  ->
                 Inh_Children  ->
                 Syn_Children 
wrap_Children (T_Children sem ) (Inh_Children _lhsIrulemap )  =
    (let ( _lhsOvchildren) = sem _lhsIrulemap 
     in  (Syn_Children _lhsOvchildren ))
sem_Children_Cons :: T_Child  ->
                     T_Children  ->
                     T_Children 
sem_Children_Cons (T_Child hd_ ) (T_Children tl_ )  =
    (T_Children (\ _lhsIrulemap ->
                     (let _lhsOvchildren :: ([VisageChild])
                          _hdOrulemap :: VisageRuleMap
                          _tlOrulemap :: VisageRuleMap
                          _hdIvchild :: VisageChild
                          _tlIvchildren :: ([VisageChild])
                          -- "src-ag/TfmToVisage.ag"(line 114, column 17)
                          _lhsOvchildren =
                              ({-# LINE 114 "src-ag/TfmToVisage.ag" #-}
                               _hdIvchild : _tlIvchildren
                               {-# LINE 181 "src-ag/TfmToVisage.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOrulemap =
                              ({-# LINE 80 "src-ag/TfmToVisage.ag" #-}
                               _lhsIrulemap
                               {-# LINE 187 "src-ag/TfmToVisage.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOrulemap =
                              ({-# LINE 81 "src-ag/TfmToVisage.ag" #-}
                               _lhsIrulemap
                               {-# LINE 193 "src-ag/TfmToVisage.hs" #-}
                               )
                          ( _hdIvchild) =
                              hd_ _hdOrulemap 
                          ( _tlIvchildren) =
                              tl_ _tlOrulemap 
                      in  ( _lhsOvchildren))) )
sem_Children_Nil :: T_Children 
sem_Children_Nil  =
    (T_Children (\ _lhsIrulemap ->
                     (let _lhsOvchildren :: ([VisageChild])
                          -- "src-ag/TfmToVisage.ag"(line 115, column 17)
                          _lhsOvchildren =
                              ({-# LINE 115 "src-ag/TfmToVisage.ag" #-}
                               []
                               {-# LINE 208 "src-ag/TfmToVisage.hs" #-}
                               )
                      in  ( _lhsOvchildren))) )
-- Expression --------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         self                 : SELF 
   alternatives:
      alternative Expression:
         child pos            : {Pos}
         child tks            : {[HsToken]}
         visit 0:
            local self        : _
-}
-- cata
sem_Expression :: Expression  ->
                  T_Expression 
sem_Expression (Expression _pos _tks )  =
    (sem_Expression_Expression _pos _tks )
-- semantic domain
newtype T_Expression  = T_Expression (( Expression ))
data Inh_Expression  = Inh_Expression {}
data Syn_Expression  = Syn_Expression {self_Syn_Expression :: Expression }
wrap_Expression :: T_Expression  ->
                   Inh_Expression  ->
                   Syn_Expression 
wrap_Expression (T_Expression sem ) (Inh_Expression )  =
    (let ( _lhsOself) = sem 
     in  (Syn_Expression _lhsOself ))
sem_Expression_Expression :: Pos ->
                             ([HsToken]) ->
                             T_Expression 
sem_Expression_Expression pos_ tks_  =
    (T_Expression (let _lhsOself :: Expression 
                       -- self rule
                       _self =
                           ({-# LINE 71 "src-ag/TfmToVisage.ag" #-}
                            Expression pos_ tks_
                            {-# LINE 247 "src-ag/TfmToVisage.hs" #-}
                            )
                       -- self rule
                       _lhsOself =
                           ({-# LINE 71 "src-ag/TfmToVisage.ag" #-}
                            _self
                            {-# LINE 253 "src-ag/TfmToVisage.hs" #-}
                            )
                   in  ( _lhsOself)) )
-- Grammar -----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         visage               : VisageGrammar
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
sem_Grammar (Grammar _typeSyns _useMap _derivings _wrappers _nonts _pragmas _manualAttrOrderMap _paramMap _contextMap _quantMap _uniqueMap _augmentsMap _aroundsMap _mergeMap )  =
    (sem_Grammar_Grammar _typeSyns _useMap _derivings _wrappers (sem_Nonterminals _nonts ) _pragmas _manualAttrOrderMap _paramMap _contextMap _quantMap _uniqueMap _augmentsMap _aroundsMap _mergeMap )
-- semantic domain
newtype T_Grammar  = T_Grammar (( VisageGrammar))
data Inh_Grammar  = Inh_Grammar {}
data Syn_Grammar  = Syn_Grammar {visage_Syn_Grammar :: VisageGrammar}
wrap_Grammar :: T_Grammar  ->
                Inh_Grammar  ->
                Syn_Grammar 
wrap_Grammar (T_Grammar sem ) (Inh_Grammar )  =
    (let ( _lhsOvisage) = sem 
     in  (Syn_Grammar _lhsOvisage ))
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
sem_Grammar_Grammar typeSyns_ useMap_ derivings_ wrappers_ (T_Nonterminals nonts_ ) pragmas_ manualAttrOrderMap_ paramMap_ contextMap_ quantMap_ uniqueMap_ augmentsMap_ aroundsMap_ mergeMap_  =
    (T_Grammar (let _lhsOvisage :: VisageGrammar
                    _nontsIvnonts :: ([VisageNonterminal])
                    -- "src-ag/TfmToVisage.ag"(line 87, column 7)
                    _lhsOvisage =
                        ({-# LINE 87 "src-ag/TfmToVisage.ag" #-}
                         VGrammar _nontsIvnonts
                         {-# LINE 315 "src-ag/TfmToVisage.hs" #-}
                         )
                    ( _nontsIvnonts) =
                        nonts_ 
                in  ( _lhsOvisage)) )
-- Nonterminal -------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         vnont                : VisageNonterminal
   alternatives:
      alternative Nonterminal:
         child nt             : {NontermIdent}
         child params         : {[Identifier]}
         child inh            : {Attributes}
         child syn            : {Attributes}
         child prods          : Productions 
-}
-- cata
sem_Nonterminal :: Nonterminal  ->
                   T_Nonterminal 
sem_Nonterminal (Nonterminal _nt _params _inh _syn _prods )  =
    (sem_Nonterminal_Nonterminal _nt _params _inh _syn (sem_Productions _prods ) )
-- semantic domain
newtype T_Nonterminal  = T_Nonterminal (( VisageNonterminal))
data Inh_Nonterminal  = Inh_Nonterminal {}
data Syn_Nonterminal  = Syn_Nonterminal {vnont_Syn_Nonterminal :: VisageNonterminal}
wrap_Nonterminal :: T_Nonterminal  ->
                    Inh_Nonterminal  ->
                    Syn_Nonterminal 
wrap_Nonterminal (T_Nonterminal sem ) (Inh_Nonterminal )  =
    (let ( _lhsOvnont) = sem 
     in  (Syn_Nonterminal _lhsOvnont ))
sem_Nonterminal_Nonterminal :: NontermIdent ->
                               ([Identifier]) ->
                               Attributes ->
                               Attributes ->
                               T_Productions  ->
                               T_Nonterminal 
sem_Nonterminal_Nonterminal nt_ params_ inh_ syn_ (T_Productions prods_ )  =
    (T_Nonterminal (let _lhsOvnont :: VisageNonterminal
                        _prodsIvprods :: ([VisageProduction])
                        -- "src-ag/TfmToVisage.ag"(line 97, column 7)
                        _lhsOvnont =
                            ({-# LINE 97 "src-ag/TfmToVisage.ag" #-}
                             VNonterminal nt_ inh_ syn_ _prodsIvprods
                             {-# LINE 361 "src-ag/TfmToVisage.hs" #-}
                             )
                        ( _prodsIvprods) =
                            prods_ 
                    in  ( _lhsOvnont)) )
-- Nonterminals ------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         vnonts               : [VisageNonterminal]
   alternatives:
      alternative Cons:
         child hd             : Nonterminal 
         child tl             : Nonterminals 
      alternative Nil:
-}
-- cata
sem_Nonterminals :: Nonterminals  ->
                    T_Nonterminals 
sem_Nonterminals list  =
    (Prelude.foldr sem_Nonterminals_Cons sem_Nonterminals_Nil (Prelude.map sem_Nonterminal list) )
-- semantic domain
newtype T_Nonterminals  = T_Nonterminals (( ([VisageNonterminal])))
data Inh_Nonterminals  = Inh_Nonterminals {}
data Syn_Nonterminals  = Syn_Nonterminals {vnonts_Syn_Nonterminals :: ([VisageNonterminal])}
wrap_Nonterminals :: T_Nonterminals  ->
                     Inh_Nonterminals  ->
                     Syn_Nonterminals 
wrap_Nonterminals (T_Nonterminals sem ) (Inh_Nonterminals )  =
    (let ( _lhsOvnonts) = sem 
     in  (Syn_Nonterminals _lhsOvnonts ))
sem_Nonterminals_Cons :: T_Nonterminal  ->
                         T_Nonterminals  ->
                         T_Nonterminals 
sem_Nonterminals_Cons (T_Nonterminal hd_ ) (T_Nonterminals tl_ )  =
    (T_Nonterminals (let _lhsOvnonts :: ([VisageNonterminal])
                         _hdIvnont :: VisageNonterminal
                         _tlIvnonts :: ([VisageNonterminal])
                         -- "src-ag/TfmToVisage.ag"(line 91, column 7)
                         _lhsOvnonts =
                             ({-# LINE 91 "src-ag/TfmToVisage.ag" #-}
                              _hdIvnont : _tlIvnonts
                              {-# LINE 403 "src-ag/TfmToVisage.hs" #-}
                              )
                         ( _hdIvnont) =
                             hd_ 
                         ( _tlIvnonts) =
                             tl_ 
                     in  ( _lhsOvnonts)) )
sem_Nonterminals_Nil :: T_Nonterminals 
sem_Nonterminals_Nil  =
    (T_Nonterminals (let _lhsOvnonts :: ([VisageNonterminal])
                         -- "src-ag/TfmToVisage.ag"(line 93, column 7)
                         _lhsOvnonts =
                             ({-# LINE 93 "src-ag/TfmToVisage.ag" #-}
                              []
                              {-# LINE 417 "src-ag/TfmToVisage.hs" #-}
                              )
                     in  ( _lhsOvnonts)) )
-- Pattern -----------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         copy                 : SELF 
         fieldattrs           :  [(Identifier,Identifier)] 
         self                 : SELF 
         vpat                 : VisagePattern
   alternatives:
      alternative Alias:
         child field          : {Identifier}
         child attr           : {Identifier}
         child pat            : Pattern 
         child parts          : Patterns 
         visit 0:
            local copy        : _
            local self        : _
      alternative Constr:
         child name           : {ConstructorIdent}
         child pats           : Patterns 
         visit 0:
            local copy        : _
            local self        : _
      alternative Irrefutable:
         child pat            : Pattern 
         visit 0:
            local copy        : _
            local self        : _
      alternative Product:
         child pos            : {Pos}
         child pats           : Patterns 
         visit 0:
            local copy        : _
            local self        : _
      alternative Underscore:
         child pos            : {Pos}
         visit 0:
            local copy        : _
            local self        : _
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
newtype T_Pattern  = T_Pattern (( Pattern ,( [(Identifier,Identifier)] ),Pattern ,VisagePattern))
data Inh_Pattern  = Inh_Pattern {}
data Syn_Pattern  = Syn_Pattern {copy_Syn_Pattern :: Pattern ,fieldattrs_Syn_Pattern :: ( [(Identifier,Identifier)] ),self_Syn_Pattern :: Pattern ,vpat_Syn_Pattern :: VisagePattern}
wrap_Pattern :: T_Pattern  ->
                Inh_Pattern  ->
                Syn_Pattern 
wrap_Pattern (T_Pattern sem ) (Inh_Pattern )  =
    (let ( _lhsOcopy,_lhsOfieldattrs,_lhsOself,_lhsOvpat) = sem 
     in  (Syn_Pattern _lhsOcopy _lhsOfieldattrs _lhsOself _lhsOvpat ))
sem_Pattern_Alias :: Identifier ->
                     Identifier ->
                     T_Pattern  ->
                     T_Patterns  ->
                     T_Pattern 
sem_Pattern_Alias field_ attr_ (T_Pattern pat_ ) (T_Patterns parts_ )  =
    (T_Pattern (let _lhsOvpat :: VisagePattern
                    _lhsOfieldattrs :: ( [(Identifier,Identifier)] )
                    _lhsOcopy :: Pattern 
                    _lhsOself :: Pattern 
                    _patIcopy :: Pattern 
                    _patIfieldattrs :: ( [(Identifier,Identifier)] )
                    _patIself :: Pattern 
                    _patIvpat :: VisagePattern
                    _partsIcopy :: Patterns 
                    _partsIfieldattrs :: ( [(Identifier,Identifier)] )
                    _partsIself :: Patterns 
                    _partsIvpats :: ([VisagePattern])
                    -- "src-ag/TfmToVisage.ag"(line 135, column 17)
                    _lhsOvpat =
                        ({-# LINE 135 "src-ag/TfmToVisage.ag" #-}
                         if (isVar _self)
                         then VVar field_ attr_
                         else VAlias field_ attr_ _patIvpat
                         {-# LINE 507 "src-ag/TfmToVisage.hs" #-}
                         )
                    -- "src-ag/TfmToVisage.ag"(line 144, column 17)
                    _lhsOfieldattrs =
                        ({-# LINE 144 "src-ag/TfmToVisage.ag" #-}
                         [(field_, attr_)]
                         {-# LINE 513 "src-ag/TfmToVisage.hs" #-}
                         )
                    -- self rule
                    _copy =
                        ({-# LINE 23 "src-ag/Patterns.ag" #-}
                         Alias field_ attr_ _patIcopy _partsIcopy
                         {-# LINE 519 "src-ag/TfmToVisage.hs" #-}
                         )
                    -- self rule
                    _self =
                        ({-# LINE 71 "src-ag/TfmToVisage.ag" #-}
                         Alias field_ attr_ _patIself _partsIself
                         {-# LINE 525 "src-ag/TfmToVisage.hs" #-}
                         )
                    -- self rule
                    _lhsOcopy =
                        ({-# LINE 23 "src-ag/Patterns.ag" #-}
                         _copy
                         {-# LINE 531 "src-ag/TfmToVisage.hs" #-}
                         )
                    -- self rule
                    _lhsOself =
                        ({-# LINE 71 "src-ag/TfmToVisage.ag" #-}
                         _self
                         {-# LINE 537 "src-ag/TfmToVisage.hs" #-}
                         )
                    ( _patIcopy,_patIfieldattrs,_patIself,_patIvpat) =
                        pat_ 
                    ( _partsIcopy,_partsIfieldattrs,_partsIself,_partsIvpats) =
                        parts_ 
                in  ( _lhsOcopy,_lhsOfieldattrs,_lhsOself,_lhsOvpat)) )
sem_Pattern_Constr :: ConstructorIdent ->
                      T_Patterns  ->
                      T_Pattern 
sem_Pattern_Constr name_ (T_Patterns pats_ )  =
    (T_Pattern (let _lhsOvpat :: VisagePattern
                    _lhsOfieldattrs :: ( [(Identifier,Identifier)] )
                    _lhsOcopy :: Pattern 
                    _lhsOself :: Pattern 
                    _patsIcopy :: Patterns 
                    _patsIfieldattrs :: ( [(Identifier,Identifier)] )
                    _patsIself :: Patterns 
                    _patsIvpats :: ([VisagePattern])
                    -- "src-ag/TfmToVisage.ag"(line 133, column 17)
                    _lhsOvpat =
                        ({-# LINE 133 "src-ag/TfmToVisage.ag" #-}
                         VConstr name_ _patsIvpats
                         {-# LINE 560 "src-ag/TfmToVisage.hs" #-}
                         )
                    -- use rule "src-ag/TfmToVisage.ag"(line 141, column 43)
                    _lhsOfieldattrs =
                        ({-# LINE 141 "src-ag/TfmToVisage.ag" #-}
                         _patsIfieldattrs
                         {-# LINE 566 "src-ag/TfmToVisage.hs" #-}
                         )
                    -- self rule
                    _copy =
                        ({-# LINE 23 "src-ag/Patterns.ag" #-}
                         Constr name_ _patsIcopy
                         {-# LINE 572 "src-ag/TfmToVisage.hs" #-}
                         )
                    -- self rule
                    _self =
                        ({-# LINE 71 "src-ag/TfmToVisage.ag" #-}
                         Constr name_ _patsIself
                         {-# LINE 578 "src-ag/TfmToVisage.hs" #-}
                         )
                    -- self rule
                    _lhsOcopy =
                        ({-# LINE 23 "src-ag/Patterns.ag" #-}
                         _copy
                         {-# LINE 584 "src-ag/TfmToVisage.hs" #-}
                         )
                    -- self rule
                    _lhsOself =
                        ({-# LINE 71 "src-ag/TfmToVisage.ag" #-}
                         _self
                         {-# LINE 590 "src-ag/TfmToVisage.hs" #-}
                         )
                    ( _patsIcopy,_patsIfieldattrs,_patsIself,_patsIvpats) =
                        pats_ 
                in  ( _lhsOcopy,_lhsOfieldattrs,_lhsOself,_lhsOvpat)) )
sem_Pattern_Irrefutable :: T_Pattern  ->
                           T_Pattern 
sem_Pattern_Irrefutable (T_Pattern pat_ )  =
    (T_Pattern (let _lhsOfieldattrs :: ( [(Identifier,Identifier)] )
                    _lhsOcopy :: Pattern 
                    _lhsOself :: Pattern 
                    _lhsOvpat :: VisagePattern
                    _patIcopy :: Pattern 
                    _patIfieldattrs :: ( [(Identifier,Identifier)] )
                    _patIself :: Pattern 
                    _patIvpat :: VisagePattern
                    -- use rule "src-ag/TfmToVisage.ag"(line 141, column 43)
                    _lhsOfieldattrs =
                        ({-# LINE 141 "src-ag/TfmToVisage.ag" #-}
                         _patIfieldattrs
                         {-# LINE 610 "src-ag/TfmToVisage.hs" #-}
                         )
                    -- self rule
                    _copy =
                        ({-# LINE 23 "src-ag/Patterns.ag" #-}
                         Irrefutable _patIcopy
                         {-# LINE 616 "src-ag/TfmToVisage.hs" #-}
                         )
                    -- self rule
                    _self =
                        ({-# LINE 71 "src-ag/TfmToVisage.ag" #-}
                         Irrefutable _patIself
                         {-# LINE 622 "src-ag/TfmToVisage.hs" #-}
                         )
                    -- self rule
                    _lhsOcopy =
                        ({-# LINE 23 "src-ag/Patterns.ag" #-}
                         _copy
                         {-# LINE 628 "src-ag/TfmToVisage.hs" #-}
                         )
                    -- self rule
                    _lhsOself =
                        ({-# LINE 71 "src-ag/TfmToVisage.ag" #-}
                         _self
                         {-# LINE 634 "src-ag/TfmToVisage.hs" #-}
                         )
                    -- copy rule (up)
                    _lhsOvpat =
                        ({-# LINE 82 "src-ag/TfmToVisage.ag" #-}
                         _patIvpat
                         {-# LINE 640 "src-ag/TfmToVisage.hs" #-}
                         )
                    ( _patIcopy,_patIfieldattrs,_patIself,_patIvpat) =
                        pat_ 
                in  ( _lhsOcopy,_lhsOfieldattrs,_lhsOself,_lhsOvpat)) )
sem_Pattern_Product :: Pos ->
                       T_Patterns  ->
                       T_Pattern 
sem_Pattern_Product pos_ (T_Patterns pats_ )  =
    (T_Pattern (let _lhsOvpat :: VisagePattern
                    _lhsOfieldattrs :: ( [(Identifier,Identifier)] )
                    _lhsOcopy :: Pattern 
                    _lhsOself :: Pattern 
                    _patsIcopy :: Patterns 
                    _patsIfieldattrs :: ( [(Identifier,Identifier)] )
                    _patsIself :: Patterns 
                    _patsIvpats :: ([VisagePattern])
                    -- "src-ag/TfmToVisage.ag"(line 134, column 17)
                    _lhsOvpat =
                        ({-# LINE 134 "src-ag/TfmToVisage.ag" #-}
                         VProduct pos_ _patsIvpats
                         {-# LINE 661 "src-ag/TfmToVisage.hs" #-}
                         )
                    -- use rule "src-ag/TfmToVisage.ag"(line 141, column 43)
                    _lhsOfieldattrs =
                        ({-# LINE 141 "src-ag/TfmToVisage.ag" #-}
                         _patsIfieldattrs
                         {-# LINE 667 "src-ag/TfmToVisage.hs" #-}
                         )
                    -- self rule
                    _copy =
                        ({-# LINE 23 "src-ag/Patterns.ag" #-}
                         Product pos_ _patsIcopy
                         {-# LINE 673 "src-ag/TfmToVisage.hs" #-}
                         )
                    -- self rule
                    _self =
                        ({-# LINE 71 "src-ag/TfmToVisage.ag" #-}
                         Product pos_ _patsIself
                         {-# LINE 679 "src-ag/TfmToVisage.hs" #-}
                         )
                    -- self rule
                    _lhsOcopy =
                        ({-# LINE 23 "src-ag/Patterns.ag" #-}
                         _copy
                         {-# LINE 685 "src-ag/TfmToVisage.hs" #-}
                         )
                    -- self rule
                    _lhsOself =
                        ({-# LINE 71 "src-ag/TfmToVisage.ag" #-}
                         _self
                         {-# LINE 691 "src-ag/TfmToVisage.hs" #-}
                         )
                    ( _patsIcopy,_patsIfieldattrs,_patsIself,_patsIvpats) =
                        pats_ 
                in  ( _lhsOcopy,_lhsOfieldattrs,_lhsOself,_lhsOvpat)) )
sem_Pattern_Underscore :: Pos ->
                          T_Pattern 
sem_Pattern_Underscore pos_  =
    (T_Pattern (let _lhsOvpat :: VisagePattern
                    _lhsOfieldattrs :: ( [(Identifier,Identifier)] )
                    _lhsOcopy :: Pattern 
                    _lhsOself :: Pattern 
                    -- "src-ag/TfmToVisage.ag"(line 138, column 17)
                    _lhsOvpat =
                        ({-# LINE 138 "src-ag/TfmToVisage.ag" #-}
                         VUnderscore pos_
                         {-# LINE 707 "src-ag/TfmToVisage.hs" #-}
                         )
                    -- use rule "src-ag/TfmToVisage.ag"(line 141, column 43)
                    _lhsOfieldattrs =
                        ({-# LINE 141 "src-ag/TfmToVisage.ag" #-}
                         []
                         {-# LINE 713 "src-ag/TfmToVisage.hs" #-}
                         )
                    -- self rule
                    _copy =
                        ({-# LINE 23 "src-ag/Patterns.ag" #-}
                         Underscore pos_
                         {-# LINE 719 "src-ag/TfmToVisage.hs" #-}
                         )
                    -- self rule
                    _self =
                        ({-# LINE 71 "src-ag/TfmToVisage.ag" #-}
                         Underscore pos_
                         {-# LINE 725 "src-ag/TfmToVisage.hs" #-}
                         )
                    -- self rule
                    _lhsOcopy =
                        ({-# LINE 23 "src-ag/Patterns.ag" #-}
                         _copy
                         {-# LINE 731 "src-ag/TfmToVisage.hs" #-}
                         )
                    -- self rule
                    _lhsOself =
                        ({-# LINE 71 "src-ag/TfmToVisage.ag" #-}
                         _self
                         {-# LINE 737 "src-ag/TfmToVisage.hs" #-}
                         )
                in  ( _lhsOcopy,_lhsOfieldattrs,_lhsOself,_lhsOvpat)) )
-- Patterns ----------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         copy                 : SELF 
         fieldattrs           :  [(Identifier,Identifier)] 
         self                 : SELF 
         vpats                : [VisagePattern]
   alternatives:
      alternative Cons:
         child hd             : Pattern 
         child tl             : Patterns 
         visit 0:
            local copy        : _
            local self        : _
      alternative Nil:
         visit 0:
            local copy        : _
            local self        : _
-}
-- cata
sem_Patterns :: Patterns  ->
                T_Patterns 
sem_Patterns list  =
    (Prelude.foldr sem_Patterns_Cons sem_Patterns_Nil (Prelude.map sem_Pattern list) )
-- semantic domain
newtype T_Patterns  = T_Patterns (( Patterns ,( [(Identifier,Identifier)] ),Patterns ,([VisagePattern])))
data Inh_Patterns  = Inh_Patterns {}
data Syn_Patterns  = Syn_Patterns {copy_Syn_Patterns :: Patterns ,fieldattrs_Syn_Patterns :: ( [(Identifier,Identifier)] ),self_Syn_Patterns :: Patterns ,vpats_Syn_Patterns :: ([VisagePattern])}
wrap_Patterns :: T_Patterns  ->
                 Inh_Patterns  ->
                 Syn_Patterns 
wrap_Patterns (T_Patterns sem ) (Inh_Patterns )  =
    (let ( _lhsOcopy,_lhsOfieldattrs,_lhsOself,_lhsOvpats) = sem 
     in  (Syn_Patterns _lhsOcopy _lhsOfieldattrs _lhsOself _lhsOvpats ))
sem_Patterns_Cons :: T_Pattern  ->
                     T_Patterns  ->
                     T_Patterns 
sem_Patterns_Cons (T_Pattern hd_ ) (T_Patterns tl_ )  =
    (T_Patterns (let _lhsOvpats :: ([VisagePattern])
                     _lhsOfieldattrs :: ( [(Identifier,Identifier)] )
                     _lhsOcopy :: Patterns 
                     _lhsOself :: Patterns 
                     _hdIcopy :: Pattern 
                     _hdIfieldattrs :: ( [(Identifier,Identifier)] )
                     _hdIself :: Pattern 
                     _hdIvpat :: VisagePattern
                     _tlIcopy :: Patterns 
                     _tlIfieldattrs :: ( [(Identifier,Identifier)] )
                     _tlIself :: Patterns 
                     _tlIvpats :: ([VisagePattern])
                     -- "src-ag/TfmToVisage.ag"(line 129, column 17)
                     _lhsOvpats =
                         ({-# LINE 129 "src-ag/TfmToVisage.ag" #-}
                          _hdIvpat : _tlIvpats
                          {-# LINE 795 "src-ag/TfmToVisage.hs" #-}
                          )
                     -- use rule "src-ag/TfmToVisage.ag"(line 141, column 43)
                     _lhsOfieldattrs =
                         ({-# LINE 141 "src-ag/TfmToVisage.ag" #-}
                          _hdIfieldattrs  ++  _tlIfieldattrs
                          {-# LINE 801 "src-ag/TfmToVisage.hs" #-}
                          )
                     -- self rule
                     _copy =
                         ({-# LINE 23 "src-ag/Patterns.ag" #-}
                          (:) _hdIcopy _tlIcopy
                          {-# LINE 807 "src-ag/TfmToVisage.hs" #-}
                          )
                     -- self rule
                     _self =
                         ({-# LINE 71 "src-ag/TfmToVisage.ag" #-}
                          (:) _hdIself _tlIself
                          {-# LINE 813 "src-ag/TfmToVisage.hs" #-}
                          )
                     -- self rule
                     _lhsOcopy =
                         ({-# LINE 23 "src-ag/Patterns.ag" #-}
                          _copy
                          {-# LINE 819 "src-ag/TfmToVisage.hs" #-}
                          )
                     -- self rule
                     _lhsOself =
                         ({-# LINE 71 "src-ag/TfmToVisage.ag" #-}
                          _self
                          {-# LINE 825 "src-ag/TfmToVisage.hs" #-}
                          )
                     ( _hdIcopy,_hdIfieldattrs,_hdIself,_hdIvpat) =
                         hd_ 
                     ( _tlIcopy,_tlIfieldattrs,_tlIself,_tlIvpats) =
                         tl_ 
                 in  ( _lhsOcopy,_lhsOfieldattrs,_lhsOself,_lhsOvpats)) )
sem_Patterns_Nil :: T_Patterns 
sem_Patterns_Nil  =
    (T_Patterns (let _lhsOvpats :: ([VisagePattern])
                     _lhsOfieldattrs :: ( [(Identifier,Identifier)] )
                     _lhsOcopy :: Patterns 
                     _lhsOself :: Patterns 
                     -- "src-ag/TfmToVisage.ag"(line 130, column 17)
                     _lhsOvpats =
                         ({-# LINE 130 "src-ag/TfmToVisage.ag" #-}
                          []
                          {-# LINE 842 "src-ag/TfmToVisage.hs" #-}
                          )
                     -- use rule "src-ag/TfmToVisage.ag"(line 141, column 43)
                     _lhsOfieldattrs =
                         ({-# LINE 141 "src-ag/TfmToVisage.ag" #-}
                          []
                          {-# LINE 848 "src-ag/TfmToVisage.hs" #-}
                          )
                     -- self rule
                     _copy =
                         ({-# LINE 23 "src-ag/Patterns.ag" #-}
                          []
                          {-# LINE 854 "src-ag/TfmToVisage.hs" #-}
                          )
                     -- self rule
                     _self =
                         ({-# LINE 71 "src-ag/TfmToVisage.ag" #-}
                          []
                          {-# LINE 860 "src-ag/TfmToVisage.hs" #-}
                          )
                     -- self rule
                     _lhsOcopy =
                         ({-# LINE 23 "src-ag/Patterns.ag" #-}
                          _copy
                          {-# LINE 866 "src-ag/TfmToVisage.hs" #-}
                          )
                     -- self rule
                     _lhsOself =
                         ({-# LINE 71 "src-ag/TfmToVisage.ag" #-}
                          _self
                          {-# LINE 872 "src-ag/TfmToVisage.hs" #-}
                          )
                 in  ( _lhsOcopy,_lhsOfieldattrs,_lhsOself,_lhsOvpats)) )
-- Production --------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         vprod                : VisageProduction
   alternatives:
      alternative Production:
         child con            : {ConstructorIdent}
         child children       : Children 
         child rules          : Rules 
         child typeSigs       : TypeSigs 
         visit 0:
            local splitVRules : _
            local locrules    : _
            local lhsrules    : _
-}
-- cata
sem_Production :: Production  ->
                  T_Production 
sem_Production (Production _con _children _rules _typeSigs )  =
    (sem_Production_Production _con (sem_Children _children ) (sem_Rules _rules ) (sem_TypeSigs _typeSigs ) )
-- semantic domain
newtype T_Production  = T_Production (( VisageProduction))
data Inh_Production  = Inh_Production {}
data Syn_Production  = Syn_Production {vprod_Syn_Production :: VisageProduction}
wrap_Production :: T_Production  ->
                   Inh_Production  ->
                   Syn_Production 
wrap_Production (T_Production sem ) (Inh_Production )  =
    (let ( _lhsOvprod) = sem 
     in  (Syn_Production _lhsOvprod ))
sem_Production_Production :: ConstructorIdent ->
                             T_Children  ->
                             T_Rules  ->
                             T_TypeSigs  ->
                             T_Production 
sem_Production_Production con_ (T_Children children_ ) (T_Rules rules_ ) (T_TypeSigs typeSigs_ )  =
    (T_Production (let _lhsOvprod :: VisageProduction
                       _childrenOrulemap :: VisageRuleMap
                       _childrenIvchildren :: ([VisageChild])
                       _rulesIvrules :: ([VisageRule])
                       -- "src-ag/TfmToVisage.ag"(line 107, column 7)
                       _lhsOvprod =
                           ({-# LINE 107 "src-ag/TfmToVisage.ag" #-}
                            VProduction con_ _childrenIvchildren _lhsrules _locrules
                            {-# LINE 920 "src-ag/TfmToVisage.hs" #-}
                            )
                       -- "src-ag/TfmToVisage.ag"(line 108, column 7)
                       _splitVRules =
                           ({-# LINE 108 "src-ag/TfmToVisage.ag" #-}
                            splitVRules _rulesIvrules
                            {-# LINE 926 "src-ag/TfmToVisage.hs" #-}
                            )
                       -- "src-ag/TfmToVisage.ag"(line 109, column 7)
                       _locrules =
                           ({-# LINE 109 "src-ag/TfmToVisage.ag" #-}
                            getForField "loc" _splitVRules
                            {-# LINE 932 "src-ag/TfmToVisage.hs" #-}
                            )
                       -- "src-ag/TfmToVisage.ag"(line 110, column 7)
                       _lhsrules =
                           ({-# LINE 110 "src-ag/TfmToVisage.ag" #-}
                            getForField "lhs" _splitVRules
                            {-# LINE 938 "src-ag/TfmToVisage.hs" #-}
                            )
                       -- "src-ag/TfmToVisage.ag"(line 111, column 7)
                       _childrenOrulemap =
                           ({-# LINE 111 "src-ag/TfmToVisage.ag" #-}
                            _splitVRules
                            {-# LINE 944 "src-ag/TfmToVisage.hs" #-}
                            )
                       ( _childrenIvchildren) =
                           children_ _childrenOrulemap 
                       ( _rulesIvrules) =
                           rules_ 
                   in  ( _lhsOvprod)) )
-- Productions -------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         vprods               : [VisageProduction]
   alternatives:
      alternative Cons:
         child hd             : Production 
         child tl             : Productions 
      alternative Nil:
-}
-- cata
sem_Productions :: Productions  ->
                   T_Productions 
sem_Productions list  =
    (Prelude.foldr sem_Productions_Cons sem_Productions_Nil (Prelude.map sem_Production list) )
-- semantic domain
newtype T_Productions  = T_Productions (( ([VisageProduction])))
data Inh_Productions  = Inh_Productions {}
data Syn_Productions  = Syn_Productions {vprods_Syn_Productions :: ([VisageProduction])}
wrap_Productions :: T_Productions  ->
                    Inh_Productions  ->
                    Syn_Productions 
wrap_Productions (T_Productions sem ) (Inh_Productions )  =
    (let ( _lhsOvprods) = sem 
     in  (Syn_Productions _lhsOvprods ))
sem_Productions_Cons :: T_Production  ->
                        T_Productions  ->
                        T_Productions 
sem_Productions_Cons (T_Production hd_ ) (T_Productions tl_ )  =
    (T_Productions (let _lhsOvprods :: ([VisageProduction])
                        _hdIvprod :: VisageProduction
                        _tlIvprods :: ([VisageProduction])
                        -- "src-ag/TfmToVisage.ag"(line 101, column 7)
                        _lhsOvprods =
                            ({-# LINE 101 "src-ag/TfmToVisage.ag" #-}
                             _hdIvprod : _tlIvprods
                             {-# LINE 988 "src-ag/TfmToVisage.hs" #-}
                             )
                        ( _hdIvprod) =
                            hd_ 
                        ( _tlIvprods) =
                            tl_ 
                    in  ( _lhsOvprods)) )
sem_Productions_Nil :: T_Productions 
sem_Productions_Nil  =
    (T_Productions (let _lhsOvprods :: ([VisageProduction])
                        -- "src-ag/TfmToVisage.ag"(line 103, column 7)
                        _lhsOvprods =
                            ({-# LINE 103 "src-ag/TfmToVisage.ag" #-}
                             []
                             {-# LINE 1002 "src-ag/TfmToVisage.hs" #-}
                             )
                    in  ( _lhsOvprods)) )
-- Rule --------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         vrule                : VisageRule
   alternatives:
      alternative Rule:
         child mbName         : {Maybe Identifier}
         child pattern        : Pattern 
         child rhs            : Expression 
         child owrt           : {Bool}
         child origin         : {String}
         child explicit       : {Bool}
-}
-- cata
sem_Rule :: Rule  ->
            T_Rule 
sem_Rule (Rule _mbName _pattern _rhs _owrt _origin _explicit )  =
    (sem_Rule_Rule _mbName (sem_Pattern _pattern ) (sem_Expression _rhs ) _owrt _origin _explicit )
-- semantic domain
newtype T_Rule  = T_Rule (( VisageRule))
data Inh_Rule  = Inh_Rule {}
data Syn_Rule  = Syn_Rule {vrule_Syn_Rule :: VisageRule}
wrap_Rule :: T_Rule  ->
             Inh_Rule  ->
             Syn_Rule 
wrap_Rule (T_Rule sem ) (Inh_Rule )  =
    (let ( _lhsOvrule) = sem 
     in  (Syn_Rule _lhsOvrule ))
sem_Rule_Rule :: (Maybe Identifier) ->
                 T_Pattern  ->
                 T_Expression  ->
                 Bool ->
                 String ->
                 Bool ->
                 T_Rule 
sem_Rule_Rule mbName_ (T_Pattern pattern_ ) (T_Expression rhs_ ) owrt_ origin_ explicit_  =
    (T_Rule (let _lhsOvrule :: VisageRule
                 _patternIcopy :: Pattern 
                 _patternIfieldattrs :: ( [(Identifier,Identifier)] )
                 _patternIself :: Pattern 
                 _patternIvpat :: VisagePattern
                 _rhsIself :: Expression 
                 -- "src-ag/TfmToVisage.ag"(line 126, column 11)
                 _lhsOvrule =
                     ({-# LINE 126 "src-ag/TfmToVisage.ag" #-}
                      VRule _patternIfieldattrs undefined _patternIvpat _rhsIself owrt_
                      {-# LINE 1052 "src-ag/TfmToVisage.hs" #-}
                      )
                 ( _patternIcopy,_patternIfieldattrs,_patternIself,_patternIvpat) =
                     pattern_ 
                 ( _rhsIself) =
                     rhs_ 
             in  ( _lhsOvrule)) )
-- Rules -------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         vrules               : [VisageRule]
   alternatives:
      alternative Cons:
         child hd             : Rule 
         child tl             : Rules 
      alternative Nil:
-}
-- cata
sem_Rules :: Rules  ->
             T_Rules 
sem_Rules list  =
    (Prelude.foldr sem_Rules_Cons sem_Rules_Nil (Prelude.map sem_Rule list) )
-- semantic domain
newtype T_Rules  = T_Rules (( ([VisageRule])))
data Inh_Rules  = Inh_Rules {}
data Syn_Rules  = Syn_Rules {vrules_Syn_Rules :: ([VisageRule])}
wrap_Rules :: T_Rules  ->
              Inh_Rules  ->
              Syn_Rules 
wrap_Rules (T_Rules sem ) (Inh_Rules )  =
    (let ( _lhsOvrules) = sem 
     in  (Syn_Rules _lhsOvrules ))
sem_Rules_Cons :: T_Rule  ->
                  T_Rules  ->
                  T_Rules 
sem_Rules_Cons (T_Rule hd_ ) (T_Rules tl_ )  =
    (T_Rules (let _lhsOvrules :: ([VisageRule])
                  _hdIvrule :: VisageRule
                  _tlIvrules :: ([VisageRule])
                  -- "src-ag/TfmToVisage.ag"(line 121, column 17)
                  _lhsOvrules =
                      ({-# LINE 121 "src-ag/TfmToVisage.ag" #-}
                       _hdIvrule : _tlIvrules
                       {-# LINE 1096 "src-ag/TfmToVisage.hs" #-}
                       )
                  ( _hdIvrule) =
                      hd_ 
                  ( _tlIvrules) =
                      tl_ 
              in  ( _lhsOvrules)) )
sem_Rules_Nil :: T_Rules 
sem_Rules_Nil  =
    (T_Rules (let _lhsOvrules :: ([VisageRule])
                  -- "src-ag/TfmToVisage.ag"(line 122, column 17)
                  _lhsOvrules =
                      ({-# LINE 122 "src-ag/TfmToVisage.ag" #-}
                       []
                       {-# LINE 1110 "src-ag/TfmToVisage.hs" #-}
                       )
              in  ( _lhsOvrules)) )
-- TypeSig -----------------------------------------------------
{-
   alternatives:
      alternative TypeSig:
         child name           : {Identifier}
         child tp             : {Type}
-}
-- cata
sem_TypeSig :: TypeSig  ->
               T_TypeSig 
sem_TypeSig (TypeSig _name _tp )  =
    (sem_TypeSig_TypeSig _name _tp )
-- semantic domain
newtype T_TypeSig  = T_TypeSig (( ))
data Inh_TypeSig  = Inh_TypeSig {}
data Syn_TypeSig  = Syn_TypeSig {}
wrap_TypeSig :: T_TypeSig  ->
                Inh_TypeSig  ->
                Syn_TypeSig 
wrap_TypeSig (T_TypeSig sem ) (Inh_TypeSig )  =
    (let ( ) = sem 
     in  (Syn_TypeSig ))
sem_TypeSig_TypeSig :: Identifier ->
                       Type ->
                       T_TypeSig 
sem_TypeSig_TypeSig name_ tp_  =
    (T_TypeSig (let 
                in  ( )) )
-- TypeSigs ----------------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : TypeSig 
         child tl             : TypeSigs 
      alternative Nil:
-}
-- cata
sem_TypeSigs :: TypeSigs  ->
                T_TypeSigs 
sem_TypeSigs list  =
    (Prelude.foldr sem_TypeSigs_Cons sem_TypeSigs_Nil (Prelude.map sem_TypeSig list) )
-- semantic domain
newtype T_TypeSigs  = T_TypeSigs (( ))
data Inh_TypeSigs  = Inh_TypeSigs {}
data Syn_TypeSigs  = Syn_TypeSigs {}
wrap_TypeSigs :: T_TypeSigs  ->
                 Inh_TypeSigs  ->
                 Syn_TypeSigs 
wrap_TypeSigs (T_TypeSigs sem ) (Inh_TypeSigs )  =
    (let ( ) = sem 
     in  (Syn_TypeSigs ))
sem_TypeSigs_Cons :: T_TypeSig  ->
                     T_TypeSigs  ->
                     T_TypeSigs 
sem_TypeSigs_Cons (T_TypeSig hd_ ) (T_TypeSigs tl_ )  =
    (T_TypeSigs (let 
                 in  ( )) )
sem_TypeSigs_Nil :: T_TypeSigs 
sem_TypeSigs_Nil  =
    (T_TypeSigs (let 
                 in  ( )) )