

-- UUAGC 0.9.39.1.0 (src-ag/TfmToVisage.ag)
module TfmToVisage where
{-# LINE 9 "src-ag/TfmToVisage.ag" #-}

import AbstractSyntax
import VisagePatterns
import VisageSyntax
import qualified Data.Map as Map
import Data.Map (Map)
{-# LINE 13 "dist/build/uuagc/uuagc-tmp/TfmToVisage.hs" #-}

{-# LINE 2 "src-ag/AbstractSyntax.ag" #-}

-- AbstractSyntax.ag imports
import Data.Set(Set)
import Data.Map(Map)
import Patterns    (Pattern(..),Patterns)
import Expression  (Expression(..))
import Macro --marcos
import CommonTypes
import ErrorMessages
{-# LINE 25 "dist/build/uuagc/uuagc-tmp/TfmToVisage.hs" #-}

{-# LINE 2 "src-ag/Patterns.ag" #-}

-- Patterns.ag imports
import UU.Scanner.Position(Pos)
import CommonTypes (ConstructorIdent,Identifier)
{-# LINE 32 "dist/build/uuagc/uuagc-tmp/TfmToVisage.hs" #-}

{-# LINE 2 "src-ag/Expression.ag" #-}

import UU.Scanner.Position(Pos)
import HsToken
{-# LINE 38 "dist/build/uuagc/uuagc-tmp/TfmToVisage.hs" #-}
{-# LINE 17 "src-ag/TfmToVisage.ag" #-}

-- Maps a rule to a pair
-- Later, I expect to map to a list of rules, because we might need to unfold.


-- Checks that a certain alias is in fact a Var in the old representation of the AG system
isVar (Alias _ _ (Underscore _)) = True
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
{-# LINE 94 "dist/build/uuagc/uuagc-tmp/TfmToVisage.hs" #-}
-- Child -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         inhMap               : Map Identifier Attributes
         rulemap              : VisageRuleMap
         synMap               : Map Identifier Attributes
      synthesized attribute:
         vchild               : VisageChild
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
sem_Child :: Child  ->
             T_Child 
sem_Child (Child _name _tp _kind )  =
    (sem_Child_Child _name _tp _kind )
-- semantic domain
newtype T_Child  = T_Child ((Map Identifier Attributes) ->
                            VisageRuleMap ->
                            (Map Identifier Attributes) ->
                            ( VisageChild))
data Inh_Child  = Inh_Child {inhMap_Inh_Child :: (Map Identifier Attributes),rulemap_Inh_Child :: VisageRuleMap,synMap_Inh_Child :: (Map Identifier Attributes)}
data Syn_Child  = Syn_Child {vchild_Syn_Child :: VisageChild}
wrap_Child :: T_Child  ->
              Inh_Child  ->
              Syn_Child 
wrap_Child (T_Child sem ) (Inh_Child _lhsIinhMap _lhsIrulemap _lhsIsynMap )  =
    (let ( _lhsOvchild) = sem _lhsIinhMap _lhsIrulemap _lhsIsynMap 
     in  (Syn_Child _lhsOvchild ))
sem_Child_Child :: Identifier ->
                   Type ->
                   ChildKind ->
                   T_Child 
sem_Child_Child name_ tp_ kind_  =
    (T_Child (\ _lhsIinhMap
                _lhsIrulemap
                _lhsIsynMap ->
                  (let _lhsOvchild :: VisageChild
                       -- "src-ag/TfmToVisage.ag"(line 121, column 11)
                       _lhsOvchild =
                           ({-# LINE 121 "src-ag/TfmToVisage.ag" #-}
                            VChild name_ tp_ _inh     _syn     (getForField (getName name_) _lhsIrulemap)
                            {-# LINE 145 "src-ag/TfmToVisage.hs" #-}
                            )
                       -- "src-ag/DistChildAttr.ag"(line 19, column 11)
                       _chnt =
                           ({-# LINE 19 "src-ag/DistChildAttr.ag" #-}
                            case tp_ of
                              NT nt _ _ -> nt
                              Self      -> error ("The type of child " ++ show name_ ++ " should not be a Self type.")
                              Haskell t -> identifier t
                            {-# LINE 154 "src-ag/TfmToVisage.hs" #-}
                            )
                       -- "src-ag/DistChildAttr.ag"(line 23, column 11)
                       _inh =
                           ({-# LINE 23 "src-ag/DistChildAttr.ag" #-}
                            Map.findWithDefault Map.empty _chnt     _lhsIinhMap
                            {-# LINE 160 "src-ag/TfmToVisage.hs" #-}
                            )
                       -- "src-ag/DistChildAttr.ag"(line 24, column 11)
                       _syn =
                           ({-# LINE 24 "src-ag/DistChildAttr.ag" #-}
                            Map.findWithDefault Map.empty _chnt     _lhsIsynMap
                            {-# LINE 166 "src-ag/TfmToVisage.hs" #-}
                            )
                   in  ( _lhsOvchild))) )
-- Children ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         inhMap               : Map Identifier Attributes
         rulemap              : VisageRuleMap
         synMap               : Map Identifier Attributes
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
newtype T_Children  = T_Children ((Map Identifier Attributes) ->
                                  VisageRuleMap ->
                                  (Map Identifier Attributes) ->
                                  ( ([VisageChild])))
data Inh_Children  = Inh_Children {inhMap_Inh_Children :: (Map Identifier Attributes),rulemap_Inh_Children :: VisageRuleMap,synMap_Inh_Children :: (Map Identifier Attributes)}
data Syn_Children  = Syn_Children {vchildren_Syn_Children :: ([VisageChild])}
wrap_Children :: T_Children  ->
                 Inh_Children  ->
                 Syn_Children 
wrap_Children (T_Children sem ) (Inh_Children _lhsIinhMap _lhsIrulemap _lhsIsynMap )  =
    (let ( _lhsOvchildren) = sem _lhsIinhMap _lhsIrulemap _lhsIsynMap 
     in  (Syn_Children _lhsOvchildren ))
sem_Children_Cons :: T_Child  ->
                     T_Children  ->
                     T_Children 
sem_Children_Cons (T_Child hd_ ) (T_Children tl_ )  =
    (T_Children (\ _lhsIinhMap
                   _lhsIrulemap
                   _lhsIsynMap ->
                     (let _lhsOvchildren :: ([VisageChild])
                          _hdOinhMap :: (Map Identifier Attributes)
                          _hdOrulemap :: VisageRuleMap
                          _hdOsynMap :: (Map Identifier Attributes)
                          _tlOinhMap :: (Map Identifier Attributes)
                          _tlOrulemap :: VisageRuleMap
                          _tlOsynMap :: (Map Identifier Attributes)
                          _hdIvchild :: VisageChild
                          _tlIvchildren :: ([VisageChild])
                          -- "src-ag/TfmToVisage.ag"(line 117, column 17)
                          _lhsOvchildren =
                              ({-# LINE 117 "src-ag/TfmToVisage.ag" #-}
                               _hdIvchild : _tlIvchildren
                               {-# LINE 222 "src-ag/TfmToVisage.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOinhMap =
                              ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                               _lhsIinhMap
                               {-# LINE 228 "src-ag/TfmToVisage.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOrulemap =
                              ({-# LINE 83 "src-ag/TfmToVisage.ag" #-}
                               _lhsIrulemap
                               {-# LINE 234 "src-ag/TfmToVisage.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOsynMap =
                              ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                               _lhsIsynMap
                               {-# LINE 240 "src-ag/TfmToVisage.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOinhMap =
                              ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                               _lhsIinhMap
                               {-# LINE 246 "src-ag/TfmToVisage.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOrulemap =
                              ({-# LINE 84 "src-ag/TfmToVisage.ag" #-}
                               _lhsIrulemap
                               {-# LINE 252 "src-ag/TfmToVisage.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOsynMap =
                              ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                               _lhsIsynMap
                               {-# LINE 258 "src-ag/TfmToVisage.hs" #-}
                               )
                          ( _hdIvchild) =
                              hd_ _hdOinhMap _hdOrulemap _hdOsynMap 
                          ( _tlIvchildren) =
                              tl_ _tlOinhMap _tlOrulemap _tlOsynMap 
                      in  ( _lhsOvchildren))) )
sem_Children_Nil :: T_Children 
sem_Children_Nil  =
    (T_Children (\ _lhsIinhMap
                   _lhsIrulemap
                   _lhsIsynMap ->
                     (let _lhsOvchildren :: ([VisageChild])
                          -- "src-ag/TfmToVisage.ag"(line 118, column 17)
                          _lhsOvchildren =
                              ({-# LINE 118 "src-ag/TfmToVisage.ag" #-}
                               []
                               {-# LINE 275 "src-ag/TfmToVisage.hs" #-}
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
                           ({-# LINE 74 "src-ag/TfmToVisage.ag" #-}
                            Expression pos_ tks_
                            {-# LINE 314 "src-ag/TfmToVisage.hs" #-}
                            )
                       -- self rule
                       _lhsOself =
                           ({-# LINE 74 "src-ag/TfmToVisage.ag" #-}
                            _self
                            {-# LINE 320 "src-ag/TfmToVisage.hs" #-}
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
                    _nontsOinhMap :: (Map Identifier Attributes)
                    _nontsOsynMap :: (Map Identifier Attributes)
                    _nontsIinhMap' :: (Map Identifier Attributes)
                    _nontsIsynMap' :: (Map Identifier Attributes)
                    _nontsIvnonts :: ([VisageNonterminal])
                    -- "src-ag/TfmToVisage.ag"(line 90, column 7)
                    _lhsOvisage =
                        ({-# LINE 90 "src-ag/TfmToVisage.ag" #-}
                         VGrammar _nontsIvnonts
                         {-# LINE 386 "src-ag/TfmToVisage.hs" #-}
                         )
                    -- "src-ag/DistChildAttr.ag"(line 15, column 13)
                    _nontsOinhMap =
                        ({-# LINE 15 "src-ag/DistChildAttr.ag" #-}
                         _nontsIinhMap'
                         {-# LINE 392 "src-ag/TfmToVisage.hs" #-}
                         )
                    -- "src-ag/DistChildAttr.ag"(line 16, column 13)
                    _nontsOsynMap =
                        ({-# LINE 16 "src-ag/DistChildAttr.ag" #-}
                         _nontsIsynMap'
                         {-# LINE 398 "src-ag/TfmToVisage.hs" #-}
                         )
                    ( _nontsIinhMap',_nontsIsynMap',_nontsIvnonts) =
                        nonts_ _nontsOinhMap _nontsOsynMap 
                in  ( _lhsOvisage)) )
-- Nonterminal -------------------------------------------------
{-
   visit 0:
      inherited attributes:
         inhMap               : Map Identifier Attributes
         synMap               : Map Identifier Attributes
      synthesized attributes:
         inhMap'              : Map Identifier Attributes
         synMap'              : Map Identifier Attributes
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
newtype T_Nonterminal  = T_Nonterminal ((Map Identifier Attributes) ->
                                        (Map Identifier Attributes) ->
                                        ( (Map Identifier Attributes),(Map Identifier Attributes),VisageNonterminal))
data Inh_Nonterminal  = Inh_Nonterminal {inhMap_Inh_Nonterminal :: (Map Identifier Attributes),synMap_Inh_Nonterminal :: (Map Identifier Attributes)}
data Syn_Nonterminal  = Syn_Nonterminal {inhMap'_Syn_Nonterminal :: (Map Identifier Attributes),synMap'_Syn_Nonterminal :: (Map Identifier Attributes),vnont_Syn_Nonterminal :: VisageNonterminal}
wrap_Nonterminal :: T_Nonterminal  ->
                    Inh_Nonterminal  ->
                    Syn_Nonterminal 
wrap_Nonterminal (T_Nonterminal sem ) (Inh_Nonterminal _lhsIinhMap _lhsIsynMap )  =
    (let ( _lhsOinhMap',_lhsOsynMap',_lhsOvnont) = sem _lhsIinhMap _lhsIsynMap 
     in  (Syn_Nonterminal _lhsOinhMap' _lhsOsynMap' _lhsOvnont ))
sem_Nonterminal_Nonterminal :: NontermIdent ->
                               ([Identifier]) ->
                               Attributes ->
                               Attributes ->
                               T_Productions  ->
                               T_Nonterminal 
sem_Nonterminal_Nonterminal nt_ params_ inh_ syn_ (T_Productions prods_ )  =
    (T_Nonterminal (\ _lhsIinhMap
                      _lhsIsynMap ->
                        (let _lhsOvnont :: VisageNonterminal
                             _lhsOinhMap' :: (Map Identifier Attributes)
                             _lhsOsynMap' :: (Map Identifier Attributes)
                             _prodsOinhMap :: (Map Identifier Attributes)
                             _prodsOsynMap :: (Map Identifier Attributes)
                             _prodsIvprods :: ([VisageProduction])
                             -- "src-ag/TfmToVisage.ag"(line 100, column 7)
                             _lhsOvnont =
                                 ({-# LINE 100 "src-ag/TfmToVisage.ag" #-}
                                  VNonterminal nt_ inh_ syn_ _prodsIvprods
                                  {-# LINE 457 "src-ag/TfmToVisage.hs" #-}
                                  )
                             -- "src-ag/DistChildAttr.ag"(line 7, column 18)
                             _lhsOinhMap' =
                                 ({-# LINE 7 "src-ag/DistChildAttr.ag" #-}
                                  Map.singleton nt_ inh_
                                  {-# LINE 463 "src-ag/TfmToVisage.hs" #-}
                                  )
                             -- "src-ag/DistChildAttr.ag"(line 8, column 18)
                             _lhsOsynMap' =
                                 ({-# LINE 8 "src-ag/DistChildAttr.ag" #-}
                                  Map.singleton nt_ syn_
                                  {-# LINE 469 "src-ag/TfmToVisage.hs" #-}
                                  )
                             -- copy rule (down)
                             _prodsOinhMap =
                                 ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                  _lhsIinhMap
                                  {-# LINE 475 "src-ag/TfmToVisage.hs" #-}
                                  )
                             -- copy rule (down)
                             _prodsOsynMap =
                                 ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                  _lhsIsynMap
                                  {-# LINE 481 "src-ag/TfmToVisage.hs" #-}
                                  )
                             ( _prodsIvprods) =
                                 prods_ _prodsOinhMap _prodsOsynMap 
                         in  ( _lhsOinhMap',_lhsOsynMap',_lhsOvnont))) )
-- Nonterminals ------------------------------------------------
{-
   visit 0:
      inherited attributes:
         inhMap               : Map Identifier Attributes
         synMap               : Map Identifier Attributes
      synthesized attributes:
         inhMap'              : Map Identifier Attributes
         synMap'              : Map Identifier Attributes
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
newtype T_Nonterminals  = T_Nonterminals ((Map Identifier Attributes) ->
                                          (Map Identifier Attributes) ->
                                          ( (Map Identifier Attributes),(Map Identifier Attributes),([VisageNonterminal])))
data Inh_Nonterminals  = Inh_Nonterminals {inhMap_Inh_Nonterminals :: (Map Identifier Attributes),synMap_Inh_Nonterminals :: (Map Identifier Attributes)}
data Syn_Nonterminals  = Syn_Nonterminals {inhMap'_Syn_Nonterminals :: (Map Identifier Attributes),synMap'_Syn_Nonterminals :: (Map Identifier Attributes),vnonts_Syn_Nonterminals :: ([VisageNonterminal])}
wrap_Nonterminals :: T_Nonterminals  ->
                     Inh_Nonterminals  ->
                     Syn_Nonterminals 
wrap_Nonterminals (T_Nonterminals sem ) (Inh_Nonterminals _lhsIinhMap _lhsIsynMap )  =
    (let ( _lhsOinhMap',_lhsOsynMap',_lhsOvnonts) = sem _lhsIinhMap _lhsIsynMap 
     in  (Syn_Nonterminals _lhsOinhMap' _lhsOsynMap' _lhsOvnonts ))
sem_Nonterminals_Cons :: T_Nonterminal  ->
                         T_Nonterminals  ->
                         T_Nonterminals 
sem_Nonterminals_Cons (T_Nonterminal hd_ ) (T_Nonterminals tl_ )  =
    (T_Nonterminals (\ _lhsIinhMap
                       _lhsIsynMap ->
                         (let _lhsOvnonts :: ([VisageNonterminal])
                              _lhsOinhMap' :: (Map Identifier Attributes)
                              _lhsOsynMap' :: (Map Identifier Attributes)
                              _hdOinhMap :: (Map Identifier Attributes)
                              _hdOsynMap :: (Map Identifier Attributes)
                              _tlOinhMap :: (Map Identifier Attributes)
                              _tlOsynMap :: (Map Identifier Attributes)
                              _hdIinhMap' :: (Map Identifier Attributes)
                              _hdIsynMap' :: (Map Identifier Attributes)
                              _hdIvnont :: VisageNonterminal
                              _tlIinhMap' :: (Map Identifier Attributes)
                              _tlIsynMap' :: (Map Identifier Attributes)
                              _tlIvnonts :: ([VisageNonterminal])
                              -- "src-ag/TfmToVisage.ag"(line 94, column 7)
                              _lhsOvnonts =
                                  ({-# LINE 94 "src-ag/TfmToVisage.ag" #-}
                                   _hdIvnont : _tlIvnonts
                                   {-# LINE 542 "src-ag/TfmToVisage.hs" #-}
                                   )
                              -- use rule "src-ag/DistChildAttr.ag"(line 4, column 53)
                              _lhsOinhMap' =
                                  ({-# LINE 4 "src-ag/DistChildAttr.ag" #-}
                                   _hdIinhMap' `Map.union` _tlIinhMap'
                                   {-# LINE 548 "src-ag/TfmToVisage.hs" #-}
                                   )
                              -- use rule "src-ag/DistChildAttr.ag"(line 4, column 53)
                              _lhsOsynMap' =
                                  ({-# LINE 4 "src-ag/DistChildAttr.ag" #-}
                                   _hdIsynMap' `Map.union` _tlIsynMap'
                                   {-# LINE 554 "src-ag/TfmToVisage.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOinhMap =
                                  ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                   _lhsIinhMap
                                   {-# LINE 560 "src-ag/TfmToVisage.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOsynMap =
                                  ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                   _lhsIsynMap
                                   {-# LINE 566 "src-ag/TfmToVisage.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOinhMap =
                                  ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                   _lhsIinhMap
                                   {-# LINE 572 "src-ag/TfmToVisage.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOsynMap =
                                  ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                   _lhsIsynMap
                                   {-# LINE 578 "src-ag/TfmToVisage.hs" #-}
                                   )
                              ( _hdIinhMap',_hdIsynMap',_hdIvnont) =
                                  hd_ _hdOinhMap _hdOsynMap 
                              ( _tlIinhMap',_tlIsynMap',_tlIvnonts) =
                                  tl_ _tlOinhMap _tlOsynMap 
                          in  ( _lhsOinhMap',_lhsOsynMap',_lhsOvnonts))) )
sem_Nonterminals_Nil :: T_Nonterminals 
sem_Nonterminals_Nil  =
    (T_Nonterminals (\ _lhsIinhMap
                       _lhsIsynMap ->
                         (let _lhsOvnonts :: ([VisageNonterminal])
                              _lhsOinhMap' :: (Map Identifier Attributes)
                              _lhsOsynMap' :: (Map Identifier Attributes)
                              -- "src-ag/TfmToVisage.ag"(line 96, column 7)
                              _lhsOvnonts =
                                  ({-# LINE 96 "src-ag/TfmToVisage.ag" #-}
                                   []
                                   {-# LINE 596 "src-ag/TfmToVisage.hs" #-}
                                   )
                              -- use rule "src-ag/DistChildAttr.ag"(line 4, column 53)
                              _lhsOinhMap' =
                                  ({-# LINE 4 "src-ag/DistChildAttr.ag" #-}
                                   Map.empty
                                   {-# LINE 602 "src-ag/TfmToVisage.hs" #-}
                                   )
                              -- use rule "src-ag/DistChildAttr.ag"(line 4, column 53)
                              _lhsOsynMap' =
                                  ({-# LINE 4 "src-ag/DistChildAttr.ag" #-}
                                   Map.empty
                                   {-# LINE 608 "src-ag/TfmToVisage.hs" #-}
                                   )
                          in  ( _lhsOinhMap',_lhsOsynMap',_lhsOvnonts))) )
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
sem_Pattern (Alias _field _attr _pat )  =
    (sem_Pattern_Alias _field _attr (sem_Pattern _pat ) )
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
                     T_Pattern 
sem_Pattern_Alias field_ attr_ (T_Pattern pat_ )  =
    (T_Pattern (let _lhsOvpat :: VisagePattern
                    _lhsOfieldattrs :: ( [(Identifier,Identifier)] )
                    _lhsOcopy :: Pattern 
                    _lhsOself :: Pattern 
                    _patIcopy :: Pattern 
                    _patIfieldattrs :: ( [(Identifier,Identifier)] )
                    _patIself :: Pattern 
                    _patIvpat :: VisagePattern
                    -- "src-ag/TfmToVisage.ag"(line 138, column 17)
                    _lhsOvpat =
                        ({-# LINE 138 "src-ag/TfmToVisage.ag" #-}
                         if (isVar _self)
                         then VVar field_ attr_
                         else VAlias field_ attr_ _patIvpat
                         {-# LINE 692 "src-ag/TfmToVisage.hs" #-}
                         )
                    -- "src-ag/TfmToVisage.ag"(line 147, column 17)
                    _lhsOfieldattrs =
                        ({-# LINE 147 "src-ag/TfmToVisage.ag" #-}
                         [(field_, attr_)]
                         {-# LINE 698 "src-ag/TfmToVisage.hs" #-}
                         )
                    -- self rule
                    _copy =
                        ({-# LINE 22 "src-ag/Patterns.ag" #-}
                         Alias field_ attr_ _patIcopy
                         {-# LINE 704 "src-ag/TfmToVisage.hs" #-}
                         )
                    -- self rule
                    _self =
                        ({-# LINE 74 "src-ag/TfmToVisage.ag" #-}
                         Alias field_ attr_ _patIself
                         {-# LINE 710 "src-ag/TfmToVisage.hs" #-}
                         )
                    -- self rule
                    _lhsOcopy =
                        ({-# LINE 22 "src-ag/Patterns.ag" #-}
                         _copy
                         {-# LINE 716 "src-ag/TfmToVisage.hs" #-}
                         )
                    -- self rule
                    _lhsOself =
                        ({-# LINE 74 "src-ag/TfmToVisage.ag" #-}
                         _self
                         {-# LINE 722 "src-ag/TfmToVisage.hs" #-}
                         )
                    ( _patIcopy,_patIfieldattrs,_patIself,_patIvpat) =
                        pat_ 
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
                    -- "src-ag/TfmToVisage.ag"(line 136, column 17)
                    _lhsOvpat =
                        ({-# LINE 136 "src-ag/TfmToVisage.ag" #-}
                         VConstr name_ _patsIvpats
                         {-# LINE 743 "src-ag/TfmToVisage.hs" #-}
                         )
                    -- use rule "src-ag/TfmToVisage.ag"(line 144, column 43)
                    _lhsOfieldattrs =
                        ({-# LINE 144 "src-ag/TfmToVisage.ag" #-}
                         _patsIfieldattrs
                         {-# LINE 749 "src-ag/TfmToVisage.hs" #-}
                         )
                    -- self rule
                    _copy =
                        ({-# LINE 22 "src-ag/Patterns.ag" #-}
                         Constr name_ _patsIcopy
                         {-# LINE 755 "src-ag/TfmToVisage.hs" #-}
                         )
                    -- self rule
                    _self =
                        ({-# LINE 74 "src-ag/TfmToVisage.ag" #-}
                         Constr name_ _patsIself
                         {-# LINE 761 "src-ag/TfmToVisage.hs" #-}
                         )
                    -- self rule
                    _lhsOcopy =
                        ({-# LINE 22 "src-ag/Patterns.ag" #-}
                         _copy
                         {-# LINE 767 "src-ag/TfmToVisage.hs" #-}
                         )
                    -- self rule
                    _lhsOself =
                        ({-# LINE 74 "src-ag/TfmToVisage.ag" #-}
                         _self
                         {-# LINE 773 "src-ag/TfmToVisage.hs" #-}
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
                    -- use rule "src-ag/TfmToVisage.ag"(line 144, column 43)
                    _lhsOfieldattrs =
                        ({-# LINE 144 "src-ag/TfmToVisage.ag" #-}
                         _patIfieldattrs
                         {-# LINE 793 "src-ag/TfmToVisage.hs" #-}
                         )
                    -- self rule
                    _copy =
                        ({-# LINE 22 "src-ag/Patterns.ag" #-}
                         Irrefutable _patIcopy
                         {-# LINE 799 "src-ag/TfmToVisage.hs" #-}
                         )
                    -- self rule
                    _self =
                        ({-# LINE 74 "src-ag/TfmToVisage.ag" #-}
                         Irrefutable _patIself
                         {-# LINE 805 "src-ag/TfmToVisage.hs" #-}
                         )
                    -- self rule
                    _lhsOcopy =
                        ({-# LINE 22 "src-ag/Patterns.ag" #-}
                         _copy
                         {-# LINE 811 "src-ag/TfmToVisage.hs" #-}
                         )
                    -- self rule
                    _lhsOself =
                        ({-# LINE 74 "src-ag/TfmToVisage.ag" #-}
                         _self
                         {-# LINE 817 "src-ag/TfmToVisage.hs" #-}
                         )
                    -- copy rule (up)
                    _lhsOvpat =
                        ({-# LINE 85 "src-ag/TfmToVisage.ag" #-}
                         _patIvpat
                         {-# LINE 823 "src-ag/TfmToVisage.hs" #-}
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
                    -- "src-ag/TfmToVisage.ag"(line 137, column 17)
                    _lhsOvpat =
                        ({-# LINE 137 "src-ag/TfmToVisage.ag" #-}
                         VProduct pos_ _patsIvpats
                         {-# LINE 844 "src-ag/TfmToVisage.hs" #-}
                         )
                    -- use rule "src-ag/TfmToVisage.ag"(line 144, column 43)
                    _lhsOfieldattrs =
                        ({-# LINE 144 "src-ag/TfmToVisage.ag" #-}
                         _patsIfieldattrs
                         {-# LINE 850 "src-ag/TfmToVisage.hs" #-}
                         )
                    -- self rule
                    _copy =
                        ({-# LINE 22 "src-ag/Patterns.ag" #-}
                         Product pos_ _patsIcopy
                         {-# LINE 856 "src-ag/TfmToVisage.hs" #-}
                         )
                    -- self rule
                    _self =
                        ({-# LINE 74 "src-ag/TfmToVisage.ag" #-}
                         Product pos_ _patsIself
                         {-# LINE 862 "src-ag/TfmToVisage.hs" #-}
                         )
                    -- self rule
                    _lhsOcopy =
                        ({-# LINE 22 "src-ag/Patterns.ag" #-}
                         _copy
                         {-# LINE 868 "src-ag/TfmToVisage.hs" #-}
                         )
                    -- self rule
                    _lhsOself =
                        ({-# LINE 74 "src-ag/TfmToVisage.ag" #-}
                         _self
                         {-# LINE 874 "src-ag/TfmToVisage.hs" #-}
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
                    -- "src-ag/TfmToVisage.ag"(line 141, column 17)
                    _lhsOvpat =
                        ({-# LINE 141 "src-ag/TfmToVisage.ag" #-}
                         VUnderscore pos_
                         {-# LINE 890 "src-ag/TfmToVisage.hs" #-}
                         )
                    -- use rule "src-ag/TfmToVisage.ag"(line 144, column 43)
                    _lhsOfieldattrs =
                        ({-# LINE 144 "src-ag/TfmToVisage.ag" #-}
                         []
                         {-# LINE 896 "src-ag/TfmToVisage.hs" #-}
                         )
                    -- self rule
                    _copy =
                        ({-# LINE 22 "src-ag/Patterns.ag" #-}
                         Underscore pos_
                         {-# LINE 902 "src-ag/TfmToVisage.hs" #-}
                         )
                    -- self rule
                    _self =
                        ({-# LINE 74 "src-ag/TfmToVisage.ag" #-}
                         Underscore pos_
                         {-# LINE 908 "src-ag/TfmToVisage.hs" #-}
                         )
                    -- self rule
                    _lhsOcopy =
                        ({-# LINE 22 "src-ag/Patterns.ag" #-}
                         _copy
                         {-# LINE 914 "src-ag/TfmToVisage.hs" #-}
                         )
                    -- self rule
                    _lhsOself =
                        ({-# LINE 74 "src-ag/TfmToVisage.ag" #-}
                         _self
                         {-# LINE 920 "src-ag/TfmToVisage.hs" #-}
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
                     -- "src-ag/TfmToVisage.ag"(line 132, column 17)
                     _lhsOvpats =
                         ({-# LINE 132 "src-ag/TfmToVisage.ag" #-}
                          _hdIvpat : _tlIvpats
                          {-# LINE 978 "src-ag/TfmToVisage.hs" #-}
                          )
                     -- use rule "src-ag/TfmToVisage.ag"(line 144, column 43)
                     _lhsOfieldattrs =
                         ({-# LINE 144 "src-ag/TfmToVisage.ag" #-}
                          _hdIfieldattrs  ++  _tlIfieldattrs
                          {-# LINE 984 "src-ag/TfmToVisage.hs" #-}
                          )
                     -- self rule
                     _copy =
                         ({-# LINE 22 "src-ag/Patterns.ag" #-}
                          (:) _hdIcopy _tlIcopy
                          {-# LINE 990 "src-ag/TfmToVisage.hs" #-}
                          )
                     -- self rule
                     _self =
                         ({-# LINE 74 "src-ag/TfmToVisage.ag" #-}
                          (:) _hdIself _tlIself
                          {-# LINE 996 "src-ag/TfmToVisage.hs" #-}
                          )
                     -- self rule
                     _lhsOcopy =
                         ({-# LINE 22 "src-ag/Patterns.ag" #-}
                          _copy
                          {-# LINE 1002 "src-ag/TfmToVisage.hs" #-}
                          )
                     -- self rule
                     _lhsOself =
                         ({-# LINE 74 "src-ag/TfmToVisage.ag" #-}
                          _self
                          {-# LINE 1008 "src-ag/TfmToVisage.hs" #-}
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
                     -- "src-ag/TfmToVisage.ag"(line 133, column 17)
                     _lhsOvpats =
                         ({-# LINE 133 "src-ag/TfmToVisage.ag" #-}
                          []
                          {-# LINE 1025 "src-ag/TfmToVisage.hs" #-}
                          )
                     -- use rule "src-ag/TfmToVisage.ag"(line 144, column 43)
                     _lhsOfieldattrs =
                         ({-# LINE 144 "src-ag/TfmToVisage.ag" #-}
                          []
                          {-# LINE 1031 "src-ag/TfmToVisage.hs" #-}
                          )
                     -- self rule
                     _copy =
                         ({-# LINE 22 "src-ag/Patterns.ag" #-}
                          []
                          {-# LINE 1037 "src-ag/TfmToVisage.hs" #-}
                          )
                     -- self rule
                     _self =
                         ({-# LINE 74 "src-ag/TfmToVisage.ag" #-}
                          []
                          {-# LINE 1043 "src-ag/TfmToVisage.hs" #-}
                          )
                     -- self rule
                     _lhsOcopy =
                         ({-# LINE 22 "src-ag/Patterns.ag" #-}
                          _copy
                          {-# LINE 1049 "src-ag/TfmToVisage.hs" #-}
                          )
                     -- self rule
                     _lhsOself =
                         ({-# LINE 74 "src-ag/TfmToVisage.ag" #-}
                          _self
                          {-# LINE 1055 "src-ag/TfmToVisage.hs" #-}
                          )
                 in  ( _lhsOcopy,_lhsOfieldattrs,_lhsOself,_lhsOvpats)) )
-- Production --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         inhMap               : Map Identifier Attributes
         synMap               : Map Identifier Attributes
      synthesized attribute:
         vprod                : VisageProduction
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
            local splitVRules : _
            local locrules    : _
            local lhsrules    : _
-}
-- cata
sem_Production :: Production  ->
                  T_Production 
sem_Production (Production _con _params _constraints _children _rules _typeSigs _macro )  =
    (sem_Production_Production _con _params _constraints (sem_Children _children ) (sem_Rules _rules ) (sem_TypeSigs _typeSigs ) _macro )
-- semantic domain
newtype T_Production  = T_Production ((Map Identifier Attributes) ->
                                      (Map Identifier Attributes) ->
                                      ( VisageProduction))
data Inh_Production  = Inh_Production {inhMap_Inh_Production :: (Map Identifier Attributes),synMap_Inh_Production :: (Map Identifier Attributes)}
data Syn_Production  = Syn_Production {vprod_Syn_Production :: VisageProduction}
wrap_Production :: T_Production  ->
                   Inh_Production  ->
                   Syn_Production 
wrap_Production (T_Production sem ) (Inh_Production _lhsIinhMap _lhsIsynMap )  =
    (let ( _lhsOvprod) = sem _lhsIinhMap _lhsIsynMap 
     in  (Syn_Production _lhsOvprod ))
sem_Production_Production :: ConstructorIdent ->
                             ([Identifier]) ->
                             ([Type]) ->
                             T_Children  ->
                             T_Rules  ->
                             T_TypeSigs  ->
                             MaybeMacro ->
                             T_Production 
sem_Production_Production con_ params_ constraints_ (T_Children children_ ) (T_Rules rules_ ) (T_TypeSigs typeSigs_ ) macro_  =
    (T_Production (\ _lhsIinhMap
                     _lhsIsynMap ->
                       (let _lhsOvprod :: VisageProduction
                            _childrenOrulemap :: VisageRuleMap
                            _childrenOinhMap :: (Map Identifier Attributes)
                            _childrenOsynMap :: (Map Identifier Attributes)
                            _childrenIvchildren :: ([VisageChild])
                            _rulesIvrules :: ([VisageRule])
                            -- "src-ag/TfmToVisage.ag"(line 110, column 7)
                            _lhsOvprod =
                                ({-# LINE 110 "src-ag/TfmToVisage.ag" #-}
                                 VProduction con_ _childrenIvchildren _lhsrules _locrules
                                 {-# LINE 1118 "src-ag/TfmToVisage.hs" #-}
                                 )
                            -- "src-ag/TfmToVisage.ag"(line 111, column 7)
                            _splitVRules =
                                ({-# LINE 111 "src-ag/TfmToVisage.ag" #-}
                                 splitVRules _rulesIvrules
                                 {-# LINE 1124 "src-ag/TfmToVisage.hs" #-}
                                 )
                            -- "src-ag/TfmToVisage.ag"(line 112, column 7)
                            _locrules =
                                ({-# LINE 112 "src-ag/TfmToVisage.ag" #-}
                                 getForField "loc" _splitVRules
                                 {-# LINE 1130 "src-ag/TfmToVisage.hs" #-}
                                 )
                            -- "src-ag/TfmToVisage.ag"(line 113, column 7)
                            _lhsrules =
                                ({-# LINE 113 "src-ag/TfmToVisage.ag" #-}
                                 getForField "lhs" _splitVRules
                                 {-# LINE 1136 "src-ag/TfmToVisage.hs" #-}
                                 )
                            -- "src-ag/TfmToVisage.ag"(line 114, column 7)
                            _childrenOrulemap =
                                ({-# LINE 114 "src-ag/TfmToVisage.ag" #-}
                                 _splitVRules
                                 {-# LINE 1142 "src-ag/TfmToVisage.hs" #-}
                                 )
                            -- copy rule (down)
                            _childrenOinhMap =
                                ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                 _lhsIinhMap
                                 {-# LINE 1148 "src-ag/TfmToVisage.hs" #-}
                                 )
                            -- copy rule (down)
                            _childrenOsynMap =
                                ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                 _lhsIsynMap
                                 {-# LINE 1154 "src-ag/TfmToVisage.hs" #-}
                                 )
                            ( _childrenIvchildren) =
                                children_ _childrenOinhMap _childrenOrulemap _childrenOsynMap 
                            ( _rulesIvrules) =
                                rules_ 
                        in  ( _lhsOvprod))) )
-- Productions -------------------------------------------------
{-
   visit 0:
      inherited attributes:
         inhMap               : Map Identifier Attributes
         synMap               : Map Identifier Attributes
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
newtype T_Productions  = T_Productions ((Map Identifier Attributes) ->
                                        (Map Identifier Attributes) ->
                                        ( ([VisageProduction])))
data Inh_Productions  = Inh_Productions {inhMap_Inh_Productions :: (Map Identifier Attributes),synMap_Inh_Productions :: (Map Identifier Attributes)}
data Syn_Productions  = Syn_Productions {vprods_Syn_Productions :: ([VisageProduction])}
wrap_Productions :: T_Productions  ->
                    Inh_Productions  ->
                    Syn_Productions 
wrap_Productions (T_Productions sem ) (Inh_Productions _lhsIinhMap _lhsIsynMap )  =
    (let ( _lhsOvprods) = sem _lhsIinhMap _lhsIsynMap 
     in  (Syn_Productions _lhsOvprods ))
sem_Productions_Cons :: T_Production  ->
                        T_Productions  ->
                        T_Productions 
sem_Productions_Cons (T_Production hd_ ) (T_Productions tl_ )  =
    (T_Productions (\ _lhsIinhMap
                      _lhsIsynMap ->
                        (let _lhsOvprods :: ([VisageProduction])
                             _hdOinhMap :: (Map Identifier Attributes)
                             _hdOsynMap :: (Map Identifier Attributes)
                             _tlOinhMap :: (Map Identifier Attributes)
                             _tlOsynMap :: (Map Identifier Attributes)
                             _hdIvprod :: VisageProduction
                             _tlIvprods :: ([VisageProduction])
                             -- "src-ag/TfmToVisage.ag"(line 104, column 7)
                             _lhsOvprods =
                                 ({-# LINE 104 "src-ag/TfmToVisage.ag" #-}
                                  _hdIvprod : _tlIvprods
                                  {-# LINE 1209 "src-ag/TfmToVisage.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOinhMap =
                                 ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                  _lhsIinhMap
                                  {-# LINE 1215 "src-ag/TfmToVisage.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOsynMap =
                                 ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                  _lhsIsynMap
                                  {-# LINE 1221 "src-ag/TfmToVisage.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOinhMap =
                                 ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                  _lhsIinhMap
                                  {-# LINE 1227 "src-ag/TfmToVisage.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOsynMap =
                                 ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                  _lhsIsynMap
                                  {-# LINE 1233 "src-ag/TfmToVisage.hs" #-}
                                  )
                             ( _hdIvprod) =
                                 hd_ _hdOinhMap _hdOsynMap 
                             ( _tlIvprods) =
                                 tl_ _tlOinhMap _tlOsynMap 
                         in  ( _lhsOvprods))) )
sem_Productions_Nil :: T_Productions 
sem_Productions_Nil  =
    (T_Productions (\ _lhsIinhMap
                      _lhsIsynMap ->
                        (let _lhsOvprods :: ([VisageProduction])
                             -- "src-ag/TfmToVisage.ag"(line 106, column 7)
                             _lhsOvprods =
                                 ({-# LINE 106 "src-ag/TfmToVisage.ag" #-}
                                  []
                                  {-# LINE 1249 "src-ag/TfmToVisage.hs" #-}
                                  )
                         in  ( _lhsOvprods))) )
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
         child pure           : {Bool}
         child identity       : {Bool}
         child mbError        : {Maybe Error}
         child eager          : {Bool}
-}
-- cata
sem_Rule :: Rule  ->
            T_Rule 
sem_Rule (Rule _mbName _pattern _rhs _owrt _origin _explicit _pure _identity _mbError _eager )  =
    (sem_Rule_Rule _mbName (sem_Pattern _pattern ) (sem_Expression _rhs ) _owrt _origin _explicit _pure _identity _mbError _eager )
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
                 Bool ->
                 Bool ->
                 (Maybe Error) ->
                 Bool ->
                 T_Rule 
sem_Rule_Rule mbName_ (T_Pattern pattern_ ) (T_Expression rhs_ ) owrt_ origin_ explicit_ pure_ identity_ mbError_ eager_  =
    (T_Rule (let _lhsOvrule :: VisageRule
                 _patternIcopy :: Pattern 
                 _patternIfieldattrs :: ( [(Identifier,Identifier)] )
                 _patternIself :: Pattern 
                 _patternIvpat :: VisagePattern
                 _rhsIself :: Expression 
                 -- "src-ag/TfmToVisage.ag"(line 129, column 11)
                 _lhsOvrule =
                     ({-# LINE 129 "src-ag/TfmToVisage.ag" #-}
                      VRule _patternIfieldattrs undefined _patternIvpat _rhsIself owrt_
                      {-# LINE 1307 "src-ag/TfmToVisage.hs" #-}
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
                  -- "src-ag/TfmToVisage.ag"(line 124, column 17)
                  _lhsOvrules =
                      ({-# LINE 124 "src-ag/TfmToVisage.ag" #-}
                       _hdIvrule : _tlIvrules
                       {-# LINE 1351 "src-ag/TfmToVisage.hs" #-}
                       )
                  ( _hdIvrule) =
                      hd_ 
                  ( _tlIvrules) =
                      tl_ 
              in  ( _lhsOvrules)) )
sem_Rules_Nil :: T_Rules 
sem_Rules_Nil  =
    (T_Rules (let _lhsOvrules :: ([VisageRule])
                  -- "src-ag/TfmToVisage.ag"(line 125, column 17)
                  _lhsOvrules =
                      ({-# LINE 125 "src-ag/TfmToVisage.ag" #-}
                       []
                       {-# LINE 1365 "src-ag/TfmToVisage.hs" #-}
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