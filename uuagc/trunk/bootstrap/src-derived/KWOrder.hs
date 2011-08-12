

-- UUAGC 0.9.39.0.0 (src-ag/KWOrder.ag)
module KWOrder where
{-# LINE 6 "src-ag/KWOrder.ag" #-}

import AbstractSyntax
import HsToken
import Expression
import Patterns
import Options
import PPUtil
import Pretty
import Knuth1
import KennedyWarren
import ExecutionPlan
import Data.Maybe
import Debug.Trace
import qualified Data.Set as Set
import qualified Data.Map as Map
{-# LINE 22 "dist/build/uuagc/uuagc-tmp/KWOrder.hs" #-}

{-# LINE 2 "src-ag/AbstractSyntax.ag" #-}

-- AbstractSyntax.ag imports
import Data.Set(Set)
import Data.Map(Map)
import Patterns    (Pattern(..),Patterns)
import Expression  (Expression(..))
import CommonTypes
{-# LINE 32 "dist/build/uuagc/uuagc-tmp/KWOrder.hs" #-}

{-# LINE 2 "src-ag/HsToken.ag" #-}

import CommonTypes
import UU.Scanner.Position(Pos)
{-# LINE 38 "dist/build/uuagc/uuagc-tmp/KWOrder.hs" #-}

{-# LINE 2 "src-ag/Expression.ag" #-}

import UU.Scanner.Position(Pos)
import HsToken
{-# LINE 44 "dist/build/uuagc/uuagc-tmp/KWOrder.hs" #-}

{-# LINE 2 "src-ag/Patterns.ag" #-}

-- Patterns.ag imports
import UU.Scanner.Position(Pos)
import CommonTypes (ConstructorIdent,Identifier)
{-# LINE 51 "dist/build/uuagc/uuagc-tmp/KWOrder.hs" #-}
-- Child -------------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         echilds              : EChild
         edges                : Set.Set Edge
         nontnames            : [(Identifier, Identifier)]
         vertices             : Set.Set Vertex
   alternatives:
      alternative Child:
         child name           : {Identifier}
         child tp             : {Type}
         child inh            : {Attributes}
         child syn            : {Attributes}
         child virtual        : {Maybe (Maybe Type)}
         visit 0:
            local vertex      : _
            local synvertices : _
            local inhvertices : _
            local edgesout    : _
            local edgesin     : _
-}
-- cata
sem_Child :: Child  ->
             T_Child 
sem_Child (Child _name _tp _inh _syn _virtual )  =
    (sem_Child_Child _name _tp _inh _syn _virtual )
-- semantic domain
newtype T_Child  = T_Child (( EChild,(Set.Set Edge),([(Identifier, Identifier)]),(Set.Set Vertex)))
data Inh_Child  = Inh_Child {}
data Syn_Child  = Syn_Child {echilds_Syn_Child :: EChild,edges_Syn_Child :: (Set.Set Edge),nontnames_Syn_Child :: ([(Identifier, Identifier)]),vertices_Syn_Child :: (Set.Set Vertex)}
wrap_Child :: T_Child  ->
              Inh_Child  ->
              Syn_Child 
wrap_Child (T_Child sem ) (Inh_Child )  =
    (let ( _lhsOechilds,_lhsOedges,_lhsOnontnames,_lhsOvertices) = sem 
     in  (Syn_Child _lhsOechilds _lhsOedges _lhsOnontnames _lhsOvertices ))
sem_Child_Child :: Identifier ->
                   Type ->
                   Attributes ->
                   Attributes ->
                   (Maybe (Maybe Type)) ->
                   T_Child 
sem_Child_Child name_ tp_ inh_ syn_ virtual_  =
    (T_Child (let _lhsOechilds :: EChild
                  _lhsOvertices :: (Set.Set Vertex)
                  _lhsOedges :: (Set.Set Edge)
                  _lhsOnontnames :: ([(Identifier, Identifier)])
                  -- "src-ag/KWOrder.ag"(line 67, column 11)
                  _lhsOechilds =
                      ({-# LINE 67 "src-ag/KWOrder.ag" #-}
                       EChild name_ tp_ virtual_
                       {-# LINE 104 "src-ag/KWOrder.hs" #-}
                       )
                  -- "src-ag/KWOrder.ag"(line 100, column 12)
                  _vertex =
                      ({-# LINE 100 "src-ag/KWOrder.ag" #-}
                       VChild name_
                       {-# LINE 110 "src-ag/KWOrder.hs" #-}
                       )
                  -- "src-ag/KWOrder.ag"(line 101, column 12)
                  _synvertices =
                      ({-# LINE 101 "src-ag/KWOrder.ag" #-}
                       map (VAttr Syn name_) . Map.keys $ syn_
                       {-# LINE 116 "src-ag/KWOrder.hs" #-}
                       )
                  -- "src-ag/KWOrder.ag"(line 102, column 12)
                  _inhvertices =
                      ({-# LINE 102 "src-ag/KWOrder.ag" #-}
                       map (VAttr Inh name_) . Map.keys $ inh_
                       {-# LINE 122 "src-ag/KWOrder.hs" #-}
                       )
                  -- "src-ag/KWOrder.ag"(line 103, column 12)
                  _lhsOvertices =
                      ({-# LINE 103 "src-ag/KWOrder.ag" #-}
                       case tp_ of
                          NT _ _ -> Set.insert _vertex     (Set.fromList $ _synvertices     ++ _inhvertices    )
                          _      -> Set.empty
                       {-# LINE 130 "src-ag/KWOrder.hs" #-}
                       )
                  -- "src-ag/KWOrder.ag"(line 128, column 12)
                  _edgesout =
                      ({-# LINE 128 "src-ag/KWOrder.ag" #-}
                       []
                       {-# LINE 136 "src-ag/KWOrder.hs" #-}
                       )
                  -- "src-ag/KWOrder.ag"(line 129, column 12)
                  _edgesin =
                      ({-# LINE 129 "src-ag/KWOrder.ag" #-}
                       map (flip (,) _vertex    ) _synvertices
                       {-# LINE 142 "src-ag/KWOrder.hs" #-}
                       )
                  -- "src-ag/KWOrder.ag"(line 130, column 12)
                  _lhsOedges =
                      ({-# LINE 130 "src-ag/KWOrder.ag" #-}
                       Set.fromList $ _edgesout     ++ _edgesin
                       {-# LINE 148 "src-ag/KWOrder.hs" #-}
                       )
                  -- "src-ag/KWOrder.ag"(line 140, column 12)
                  _lhsOnontnames =
                      ({-# LINE 140 "src-ag/KWOrder.ag" #-}
                       case tp_ of
                         NT nont _ -> [(name_, nont)]
                         _         -> []
                       {-# LINE 156 "src-ag/KWOrder.hs" #-}
                       )
              in  ( _lhsOechilds,_lhsOedges,_lhsOnontnames,_lhsOvertices)) )
-- Children ----------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         echilds              : EChildren
         edges                : Set.Set Edge
         nontnames            : [(Identifier, Identifier)]
         vertices             : Set.Set Vertex
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
newtype T_Children  = T_Children (( EChildren,(Set.Set Edge),([(Identifier, Identifier)]),(Set.Set Vertex)))
data Inh_Children  = Inh_Children {}
data Syn_Children  = Syn_Children {echilds_Syn_Children :: EChildren,edges_Syn_Children :: (Set.Set Edge),nontnames_Syn_Children :: ([(Identifier, Identifier)]),vertices_Syn_Children :: (Set.Set Vertex)}
wrap_Children :: T_Children  ->
                 Inh_Children  ->
                 Syn_Children 
wrap_Children (T_Children sem ) (Inh_Children )  =
    (let ( _lhsOechilds,_lhsOedges,_lhsOnontnames,_lhsOvertices) = sem 
     in  (Syn_Children _lhsOechilds _lhsOedges _lhsOnontnames _lhsOvertices ))
sem_Children_Cons :: T_Child  ->
                     T_Children  ->
                     T_Children 
sem_Children_Cons (T_Child hd_ ) (T_Children tl_ )  =
    (T_Children (let _lhsOechilds :: EChildren
                     _lhsOedges :: (Set.Set Edge)
                     _lhsOnontnames :: ([(Identifier, Identifier)])
                     _lhsOvertices :: (Set.Set Vertex)
                     _hdIechilds :: EChild
                     _hdIedges :: (Set.Set Edge)
                     _hdInontnames :: ([(Identifier, Identifier)])
                     _hdIvertices :: (Set.Set Vertex)
                     _tlIechilds :: EChildren
                     _tlIedges :: (Set.Set Edge)
                     _tlInontnames :: ([(Identifier, Identifier)])
                     _tlIvertices :: (Set.Set Vertex)
                     -- use rule "src-ag/KWOrder.ag"(line 64, column 29)
                     _lhsOechilds =
                         ({-# LINE 64 "src-ag/KWOrder.ag" #-}
                          _hdIechilds : _tlIechilds
                          {-# LINE 208 "src-ag/KWOrder.hs" #-}
                          )
                     -- use rule "src-ag/KWOrder.ag"(line 118, column 33)
                     _lhsOedges =
                         ({-# LINE 118 "src-ag/KWOrder.ag" #-}
                          _hdIedges `Set.union` _tlIedges
                          {-# LINE 214 "src-ag/KWOrder.hs" #-}
                          )
                     -- use rule "src-ag/KWOrder.ag"(line 137, column 37)
                     _lhsOnontnames =
                         ({-# LINE 137 "src-ag/KWOrder.ag" #-}
                          _hdInontnames ++ _tlInontnames
                          {-# LINE 220 "src-ag/KWOrder.hs" #-}
                          )
                     -- use rule "src-ag/KWOrder.ag"(line 76, column 36)
                     _lhsOvertices =
                         ({-# LINE 76 "src-ag/KWOrder.ag" #-}
                          _hdIvertices `Set.union` _tlIvertices
                          {-# LINE 226 "src-ag/KWOrder.hs" #-}
                          )
                     ( _hdIechilds,_hdIedges,_hdInontnames,_hdIvertices) =
                         hd_ 
                     ( _tlIechilds,_tlIedges,_tlInontnames,_tlIvertices) =
                         tl_ 
                 in  ( _lhsOechilds,_lhsOedges,_lhsOnontnames,_lhsOvertices)) )
sem_Children_Nil :: T_Children 
sem_Children_Nil  =
    (T_Children (let _lhsOechilds :: EChildren
                     _lhsOedges :: (Set.Set Edge)
                     _lhsOnontnames :: ([(Identifier, Identifier)])
                     _lhsOvertices :: (Set.Set Vertex)
                     -- use rule "src-ag/KWOrder.ag"(line 64, column 29)
                     _lhsOechilds =
                         ({-# LINE 64 "src-ag/KWOrder.ag" #-}
                          []
                          {-# LINE 243 "src-ag/KWOrder.hs" #-}
                          )
                     -- use rule "src-ag/KWOrder.ag"(line 118, column 33)
                     _lhsOedges =
                         ({-# LINE 118 "src-ag/KWOrder.ag" #-}
                          Set.empty
                          {-# LINE 249 "src-ag/KWOrder.hs" #-}
                          )
                     -- use rule "src-ag/KWOrder.ag"(line 137, column 37)
                     _lhsOnontnames =
                         ({-# LINE 137 "src-ag/KWOrder.ag" #-}
                          []
                          {-# LINE 255 "src-ag/KWOrder.hs" #-}
                          )
                     -- use rule "src-ag/KWOrder.ag"(line 76, column 36)
                     _lhsOvertices =
                         ({-# LINE 76 "src-ag/KWOrder.ag" #-}
                          Set.empty
                          {-# LINE 261 "src-ag/KWOrder.hs" #-}
                          )
                 in  ( _lhsOechilds,_lhsOedges,_lhsOnontnames,_lhsOvertices)) )
-- Expression --------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         copy                 : SELF 
         vertices             : Set.Set Vertex
   alternatives:
      alternative Expression:
         child pos            : {Pos}
         child tks            : {[HsToken]}
         visit 0:
            local copy        : _
-}
-- cata
sem_Expression :: Expression  ->
                  T_Expression 
sem_Expression (Expression _pos _tks )  =
    (sem_Expression_Expression _pos _tks )
-- semantic domain
newtype T_Expression  = T_Expression (( Expression ,(Set.Set Vertex)))
data Inh_Expression  = Inh_Expression {}
data Syn_Expression  = Syn_Expression {copy_Syn_Expression :: Expression ,vertices_Syn_Expression :: (Set.Set Vertex)}
wrap_Expression :: T_Expression  ->
                   Inh_Expression  ->
                   Syn_Expression 
wrap_Expression (T_Expression sem ) (Inh_Expression )  =
    (let ( _lhsOcopy,_lhsOvertices) = sem 
     in  (Syn_Expression _lhsOcopy _lhsOvertices ))
sem_Expression_Expression :: Pos ->
                             ([HsToken]) ->
                             T_Expression 
sem_Expression_Expression pos_ tks_  =
    (T_Expression (let _lhsOvertices :: (Set.Set Vertex)
                       _lhsOcopy :: Expression 
                       -- "src-ag/KWOrder.ag"(line 87, column 17)
                       _lhsOvertices =
                           ({-# LINE 87 "src-ag/KWOrder.ag" #-}
                            Set.unions $ map (\tok -> vertices_Syn_HsToken
                                         (wrap_HsToken (sem_HsToken tok) Inh_HsToken)) tks_
                            {-# LINE 303 "src-ag/KWOrder.hs" #-}
                            )
                       -- self rule
                       _copy =
                           ({-# LINE 45 "src-ag/KWOrder.ag" #-}
                            Expression pos_ tks_
                            {-# LINE 309 "src-ag/KWOrder.hs" #-}
                            )
                       -- self rule
                       _lhsOcopy =
                           ({-# LINE 45 "src-ag/KWOrder.ag" #-}
                            _copy
                            {-# LINE 315 "src-ag/KWOrder.hs" #-}
                            )
                   in  ( _lhsOcopy,_lhsOvertices)) )
-- Grammar -----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         options              : Options
      synthesized attributes:
         depgraphs            : PP_Doc
         inhmap               : Map.Map NontermIdent Attributes
         output               : ExecutionPlan
         synmap               : Map.Map NontermIdent Attributes
         visitgraph           : PP_Doc
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
newtype T_Grammar  = T_Grammar (Options ->
                                ( PP_Doc,(Map.Map NontermIdent Attributes),ExecutionPlan,(Map.Map NontermIdent Attributes),PP_Doc))
data Inh_Grammar  = Inh_Grammar {options_Inh_Grammar :: Options}
data Syn_Grammar  = Syn_Grammar {depgraphs_Syn_Grammar :: PP_Doc,inhmap_Syn_Grammar :: (Map.Map NontermIdent Attributes),output_Syn_Grammar :: ExecutionPlan,synmap_Syn_Grammar :: (Map.Map NontermIdent Attributes),visitgraph_Syn_Grammar :: PP_Doc}
wrap_Grammar :: T_Grammar  ->
                Inh_Grammar  ->
                Syn_Grammar 
wrap_Grammar (T_Grammar sem ) (Inh_Grammar _lhsIoptions )  =
    (let ( _lhsOdepgraphs,_lhsOinhmap,_lhsOoutput,_lhsOsynmap,_lhsOvisitgraph) = sem _lhsIoptions 
     in  (Syn_Grammar _lhsOdepgraphs _lhsOinhmap _lhsOoutput _lhsOsynmap _lhsOvisitgraph ))
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
    (T_Grammar (\ _lhsIoptions ->
                    (let _nontsOrulenumber :: Int
                         _lhsOoutput :: ExecutionPlan
                         _lhsOdepgraphs :: PP_Doc
                         _lhsOvisitgraph :: PP_Doc
                         _lhsOinhmap :: (Map.Map NontermIdent Attributes)
                         _lhsOsynmap :: (Map.Map NontermIdent Attributes)
                         _nontsIdepinfo :: ([NontDependencyInformation])
                         _nontsIinhmap :: (Map.Map NontermIdent Attributes)
                         _nontsIrulenumber :: Int
                         _nontsIsynmap :: (Map.Map NontermIdent Attributes)
                         -- "src-ag/KWOrder.ag"(line 36, column 14)
                         _nontsOrulenumber =
                             ({-# LINE 36 "src-ag/KWOrder.ag" #-}
                              0
                              {-# LINE 393 "src-ag/KWOrder.hs" #-}
                              )
                         -- "src-ag/KWOrder.ag"(line 191, column 15)
                         (Just (_lhsOoutput,_lhsOdepgraphs,_lhsOvisitgraph) ) =
                             ({-# LINE 191 "src-ag/KWOrder.ag" #-}
                              kennedyWarrenOrder wrappers_ _nontsIdepinfo typeSyns_ derivings_
                              {-# LINE 399 "src-ag/KWOrder.hs" #-}
                              )
                         -- use rule "src-ag/KWOrder.ag"(line 198, column 33)
                         _lhsOinhmap =
                             ({-# LINE 198 "src-ag/KWOrder.ag" #-}
                              _nontsIinhmap
                              {-# LINE 405 "src-ag/KWOrder.hs" #-}
                              )
                         -- use rule "src-ag/KWOrder.ag"(line 199, column 33)
                         _lhsOsynmap =
                             ({-# LINE 199 "src-ag/KWOrder.ag" #-}
                              _nontsIsynmap
                              {-# LINE 411 "src-ag/KWOrder.hs" #-}
                              )
                         ( _nontsIdepinfo,_nontsIinhmap,_nontsIrulenumber,_nontsIsynmap) =
                             nonts_ _nontsOrulenumber 
                     in  ( _lhsOdepgraphs,_lhsOinhmap,_lhsOoutput,_lhsOsynmap,_lhsOvisitgraph))) )
-- HsToken -----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         vertices             : Set.Set Vertex
   alternatives:
      alternative AGField:
         child field          : {Identifier}
         child attr           : {Identifier}
         child pos            : {Pos}
         child rdesc          : {Maybe String}
      alternative AGLocal:
         child var            : {Identifier}
         child pos            : {Pos}
         child rdesc          : {Maybe String}
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
newtype T_HsToken  = T_HsToken (( (Set.Set Vertex)))
data Inh_HsToken  = Inh_HsToken {}
data Syn_HsToken  = Syn_HsToken {vertices_Syn_HsToken :: (Set.Set Vertex)}
wrap_HsToken :: T_HsToken  ->
                Inh_HsToken  ->
                Syn_HsToken 
wrap_HsToken (T_HsToken sem ) (Inh_HsToken )  =
    (let ( _lhsOvertices) = sem 
     in  (Syn_HsToken _lhsOvertices ))
sem_HsToken_AGField :: Identifier ->
                       Identifier ->
                       Pos ->
                       (Maybe String) ->
                       T_HsToken 
sem_HsToken_AGField field_ attr_ pos_ rdesc_  =
    (T_HsToken (let _lhsOvertices :: (Set.Set Vertex)
                    -- "src-ag/KWOrder.ag"(line 81, column 14)
                    _lhsOvertices =
                        ({-# LINE 81 "src-ag/KWOrder.ag" #-}
                         Set.singleton $ VAttr (if      field_ == _LHS then Inh
                                                else if field_ == _LOC then Loc
                                                else                        Syn) field_ attr_
                         {-# LINE 482 "src-ag/KWOrder.hs" #-}
                         )
                in  ( _lhsOvertices)) )
sem_HsToken_AGLocal :: Identifier ->
                       Pos ->
                       (Maybe String) ->
                       T_HsToken 
sem_HsToken_AGLocal var_ pos_ rdesc_  =
    (T_HsToken (let _lhsOvertices :: (Set.Set Vertex)
                    -- "src-ag/KWOrder.ag"(line 80, column 14)
                    _lhsOvertices =
                        ({-# LINE 80 "src-ag/KWOrder.ag" #-}
                         Set.singleton $ VAttr Loc _LOC var_
                         {-# LINE 495 "src-ag/KWOrder.hs" #-}
                         )
                in  ( _lhsOvertices)) )
sem_HsToken_CharToken :: String ->
                         Pos ->
                         T_HsToken 
sem_HsToken_CharToken value_ pos_  =
    (T_HsToken (let _lhsOvertices :: (Set.Set Vertex)
                    -- use rule "src-ag/KWOrder.ag"(line 76, column 36)
                    _lhsOvertices =
                        ({-# LINE 76 "src-ag/KWOrder.ag" #-}
                         Set.empty
                         {-# LINE 507 "src-ag/KWOrder.hs" #-}
                         )
                in  ( _lhsOvertices)) )
sem_HsToken_Err :: String ->
                   Pos ->
                   T_HsToken 
sem_HsToken_Err mesg_ pos_  =
    (T_HsToken (let _lhsOvertices :: (Set.Set Vertex)
                    -- use rule "src-ag/KWOrder.ag"(line 76, column 36)
                    _lhsOvertices =
                        ({-# LINE 76 "src-ag/KWOrder.ag" #-}
                         Set.empty
                         {-# LINE 519 "src-ag/KWOrder.hs" #-}
                         )
                in  ( _lhsOvertices)) )
sem_HsToken_HsToken :: String ->
                       Pos ->
                       T_HsToken 
sem_HsToken_HsToken value_ pos_  =
    (T_HsToken (let _lhsOvertices :: (Set.Set Vertex)
                    -- use rule "src-ag/KWOrder.ag"(line 76, column 36)
                    _lhsOvertices =
                        ({-# LINE 76 "src-ag/KWOrder.ag" #-}
                         Set.empty
                         {-# LINE 531 "src-ag/KWOrder.hs" #-}
                         )
                in  ( _lhsOvertices)) )
sem_HsToken_StrToken :: String ->
                        Pos ->
                        T_HsToken 
sem_HsToken_StrToken value_ pos_  =
    (T_HsToken (let _lhsOvertices :: (Set.Set Vertex)
                    -- use rule "src-ag/KWOrder.ag"(line 76, column 36)
                    _lhsOvertices =
                        ({-# LINE 76 "src-ag/KWOrder.ag" #-}
                         Set.empty
                         {-# LINE 543 "src-ag/KWOrder.hs" #-}
                         )
                in  ( _lhsOvertices)) )
-- HsTokens ----------------------------------------------------
{-
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
newtype T_HsTokens  = T_HsTokens (( ))
data Inh_HsTokens  = Inh_HsTokens {}
data Syn_HsTokens  = Syn_HsTokens {}
wrap_HsTokens :: T_HsTokens  ->
                 Inh_HsTokens  ->
                 Syn_HsTokens 
wrap_HsTokens (T_HsTokens sem ) (Inh_HsTokens )  =
    (let ( ) = sem 
     in  (Syn_HsTokens ))
sem_HsTokens_Cons :: T_HsToken  ->
                     T_HsTokens  ->
                     T_HsTokens 
sem_HsTokens_Cons (T_HsToken hd_ ) (T_HsTokens tl_ )  =
    (T_HsTokens (let _hdIvertices :: (Set.Set Vertex)
                     ( _hdIvertices) =
                         hd_ 
                 in  ( )) )
sem_HsTokens_Nil :: T_HsTokens 
sem_HsTokens_Nil  =
    (T_HsTokens (let 
                 in  ( )) )
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
    (T_HsTokensRoot (let 
                     in  ( )) )
-- Nonterminal -------------------------------------------------
{-
   visit 0:
      chained attribute:
         rulenumber           : Int
      synthesized attributes:
         depinfo              : NontDependencyInformation
         inhmap               : Map.Map NontermIdent Attributes
         synmap               : Map.Map NontermIdent Attributes
   alternatives:
      alternative Nonterminal:
         child nt             : {NontermIdent}
         child params         : {[Identifier]}
         child inh            : {Attributes}
         child syn            : {Attributes}
         child prods          : Productions 
         visit 0:
            local synvertices : _
            local inhvertices : _
            local vertices    : _
            local nontgraph   : _
-}
-- cata
sem_Nonterminal :: Nonterminal  ->
                   T_Nonterminal 
sem_Nonterminal (Nonterminal _nt _params _inh _syn _prods )  =
    (sem_Nonterminal_Nonterminal _nt _params _inh _syn (sem_Productions _prods ) )
-- semantic domain
newtype T_Nonterminal  = T_Nonterminal (Int ->
                                        ( NontDependencyInformation,(Map.Map NontermIdent Attributes),Int,(Map.Map NontermIdent Attributes)))
data Inh_Nonterminal  = Inh_Nonterminal {rulenumber_Inh_Nonterminal :: Int}
data Syn_Nonterminal  = Syn_Nonterminal {depinfo_Syn_Nonterminal :: NontDependencyInformation,inhmap_Syn_Nonterminal :: (Map.Map NontermIdent Attributes),rulenumber_Syn_Nonterminal :: Int,synmap_Syn_Nonterminal :: (Map.Map NontermIdent Attributes)}
wrap_Nonterminal :: T_Nonterminal  ->
                    Inh_Nonterminal  ->
                    Syn_Nonterminal 
wrap_Nonterminal (T_Nonterminal sem ) (Inh_Nonterminal _lhsIrulenumber )  =
    (let ( _lhsOdepinfo,_lhsOinhmap,_lhsOrulenumber,_lhsOsynmap) = sem _lhsIrulenumber 
     in  (Syn_Nonterminal _lhsOdepinfo _lhsOinhmap _lhsOrulenumber _lhsOsynmap ))
sem_Nonterminal_Nonterminal :: NontermIdent ->
                               ([Identifier]) ->
                               Attributes ->
                               Attributes ->
                               T_Productions  ->
                               T_Nonterminal 
sem_Nonterminal_Nonterminal nt_ params_ inh_ syn_ (T_Productions prods_ )  =
    (T_Nonterminal (\ _lhsIrulenumber ->
                        (let _lhsOdepinfo :: NontDependencyInformation
                             _lhsOinhmap :: (Map.Map NontermIdent Attributes)
                             _lhsOsynmap :: (Map.Map NontermIdent Attributes)
                             _lhsOrulenumber :: Int
                             _prodsOrulenumber :: Int
                             _prodsIdepgraph :: ([ProdDependencyGraph])
                             _prodsIrulenumber :: Int
                             -- "src-ag/KWOrder.ag"(line 162, column 18)
                             _synvertices =
                                 ({-# LINE 162 "src-ag/KWOrder.ag" #-}
                                  map (VAttr Syn nt_) . Map.keys $ syn_
                                  {-# LINE 664 "src-ag/KWOrder.hs" #-}
                                  )
                             -- "src-ag/KWOrder.ag"(line 163, column 18)
                             _inhvertices =
                                 ({-# LINE 163 "src-ag/KWOrder.ag" #-}
                                  map (VAttr Inh nt_) . Map.keys $ inh_
                                  {-# LINE 670 "src-ag/KWOrder.hs" #-}
                                  )
                             -- "src-ag/KWOrder.ag"(line 164, column 18)
                             _vertices =
                                 ({-# LINE 164 "src-ag/KWOrder.ag" #-}
                                  _synvertices     ++ _inhvertices
                                  {-# LINE 676 "src-ag/KWOrder.hs" #-}
                                  )
                             -- "src-ag/KWOrder.ag"(line 168, column 18)
                             _nontgraph =
                                 ({-# LINE 168 "src-ag/KWOrder.ag" #-}
                                  NontDependencyGraph { ndgVertices = _vertices
                                                      , ndgEdges    = [] }
                                  {-# LINE 683 "src-ag/KWOrder.hs" #-}
                                  )
                             -- "src-ag/KWOrder.ag"(line 176, column 18)
                             _lhsOdepinfo =
                                 ({-# LINE 176 "src-ag/KWOrder.ag" #-}
                                  NontDependencyInformation { ndiNonterminal = nt_
                                                            , ndiParams      = params_
                                                            , ndiInh         = Map.keys inh_
                                                            , ndiSyn         = Map.keys syn_
                                                            , ndiDepGraph    = _nontgraph
                                                            , ndiProds       = _prodsIdepgraph }
                                  {-# LINE 694 "src-ag/KWOrder.hs" #-}
                                  )
                             -- "src-ag/KWOrder.ag"(line 204, column 17)
                             _lhsOinhmap =
                                 ({-# LINE 204 "src-ag/KWOrder.ag" #-}
                                  Map.singleton nt_ inh_
                                  {-# LINE 700 "src-ag/KWOrder.hs" #-}
                                  )
                             -- "src-ag/KWOrder.ag"(line 205, column 17)
                             _lhsOsynmap =
                                 ({-# LINE 205 "src-ag/KWOrder.ag" #-}
                                  Map.singleton nt_ syn_
                                  {-# LINE 706 "src-ag/KWOrder.hs" #-}
                                  )
                             -- copy rule (up)
                             _lhsOrulenumber =
                                 ({-# LINE 33 "src-ag/KWOrder.ag" #-}
                                  _prodsIrulenumber
                                  {-# LINE 712 "src-ag/KWOrder.hs" #-}
                                  )
                             -- copy rule (down)
                             _prodsOrulenumber =
                                 ({-# LINE 33 "src-ag/KWOrder.ag" #-}
                                  _lhsIrulenumber
                                  {-# LINE 718 "src-ag/KWOrder.hs" #-}
                                  )
                             ( _prodsIdepgraph,_prodsIrulenumber) =
                                 prods_ _prodsOrulenumber 
                         in  ( _lhsOdepinfo,_lhsOinhmap,_lhsOrulenumber,_lhsOsynmap))) )
-- Nonterminals ------------------------------------------------
{-
   visit 0:
      chained attribute:
         rulenumber           : Int
      synthesized attributes:
         depinfo              : [NontDependencyInformation]
         inhmap               : Map.Map NontermIdent Attributes
         synmap               : Map.Map NontermIdent Attributes
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
newtype T_Nonterminals  = T_Nonterminals (Int ->
                                          ( ([NontDependencyInformation]),(Map.Map NontermIdent Attributes),Int,(Map.Map NontermIdent Attributes)))
data Inh_Nonterminals  = Inh_Nonterminals {rulenumber_Inh_Nonterminals :: Int}
data Syn_Nonterminals  = Syn_Nonterminals {depinfo_Syn_Nonterminals :: ([NontDependencyInformation]),inhmap_Syn_Nonterminals :: (Map.Map NontermIdent Attributes),rulenumber_Syn_Nonterminals :: Int,synmap_Syn_Nonterminals :: (Map.Map NontermIdent Attributes)}
wrap_Nonterminals :: T_Nonterminals  ->
                     Inh_Nonterminals  ->
                     Syn_Nonterminals 
wrap_Nonterminals (T_Nonterminals sem ) (Inh_Nonterminals _lhsIrulenumber )  =
    (let ( _lhsOdepinfo,_lhsOinhmap,_lhsOrulenumber,_lhsOsynmap) = sem _lhsIrulenumber 
     in  (Syn_Nonterminals _lhsOdepinfo _lhsOinhmap _lhsOrulenumber _lhsOsynmap ))
sem_Nonterminals_Cons :: T_Nonterminal  ->
                         T_Nonterminals  ->
                         T_Nonterminals 
sem_Nonterminals_Cons (T_Nonterminal hd_ ) (T_Nonterminals tl_ )  =
    (T_Nonterminals (\ _lhsIrulenumber ->
                         (let _lhsOdepinfo :: ([NontDependencyInformation])
                              _lhsOinhmap :: (Map.Map NontermIdent Attributes)
                              _lhsOsynmap :: (Map.Map NontermIdent Attributes)
                              _lhsOrulenumber :: Int
                              _hdOrulenumber :: Int
                              _tlOrulenumber :: Int
                              _hdIdepinfo :: NontDependencyInformation
                              _hdIinhmap :: (Map.Map NontermIdent Attributes)
                              _hdIrulenumber :: Int
                              _hdIsynmap :: (Map.Map NontermIdent Attributes)
                              _tlIdepinfo :: ([NontDependencyInformation])
                              _tlIinhmap :: (Map.Map NontermIdent Attributes)
                              _tlIrulenumber :: Int
                              _tlIsynmap :: (Map.Map NontermIdent Attributes)
                              -- use rule "src-ag/KWOrder.ag"(line 173, column 33)
                              _lhsOdepinfo =
                                  ({-# LINE 173 "src-ag/KWOrder.ag" #-}
                                   _hdIdepinfo : _tlIdepinfo
                                   {-# LINE 777 "src-ag/KWOrder.hs" #-}
                                   )
                              -- use rule "src-ag/KWOrder.ag"(line 198, column 33)
                              _lhsOinhmap =
                                  ({-# LINE 198 "src-ag/KWOrder.ag" #-}
                                   _hdIinhmap `Map.union` _tlIinhmap
                                   {-# LINE 783 "src-ag/KWOrder.hs" #-}
                                   )
                              -- use rule "src-ag/KWOrder.ag"(line 199, column 33)
                              _lhsOsynmap =
                                  ({-# LINE 199 "src-ag/KWOrder.ag" #-}
                                   _hdIsynmap `Map.union` _tlIsynmap
                                   {-# LINE 789 "src-ag/KWOrder.hs" #-}
                                   )
                              -- copy rule (up)
                              _lhsOrulenumber =
                                  ({-# LINE 33 "src-ag/KWOrder.ag" #-}
                                   _tlIrulenumber
                                   {-# LINE 795 "src-ag/KWOrder.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOrulenumber =
                                  ({-# LINE 33 "src-ag/KWOrder.ag" #-}
                                   _lhsIrulenumber
                                   {-# LINE 801 "src-ag/KWOrder.hs" #-}
                                   )
                              -- copy rule (chain)
                              _tlOrulenumber =
                                  ({-# LINE 33 "src-ag/KWOrder.ag" #-}
                                   _hdIrulenumber
                                   {-# LINE 807 "src-ag/KWOrder.hs" #-}
                                   )
                              ( _hdIdepinfo,_hdIinhmap,_hdIrulenumber,_hdIsynmap) =
                                  hd_ _hdOrulenumber 
                              ( _tlIdepinfo,_tlIinhmap,_tlIrulenumber,_tlIsynmap) =
                                  tl_ _tlOrulenumber 
                          in  ( _lhsOdepinfo,_lhsOinhmap,_lhsOrulenumber,_lhsOsynmap))) )
sem_Nonterminals_Nil :: T_Nonterminals 
sem_Nonterminals_Nil  =
    (T_Nonterminals (\ _lhsIrulenumber ->
                         (let _lhsOdepinfo :: ([NontDependencyInformation])
                              _lhsOinhmap :: (Map.Map NontermIdent Attributes)
                              _lhsOsynmap :: (Map.Map NontermIdent Attributes)
                              _lhsOrulenumber :: Int
                              -- use rule "src-ag/KWOrder.ag"(line 173, column 33)
                              _lhsOdepinfo =
                                  ({-# LINE 173 "src-ag/KWOrder.ag" #-}
                                   []
                                   {-# LINE 825 "src-ag/KWOrder.hs" #-}
                                   )
                              -- use rule "src-ag/KWOrder.ag"(line 198, column 33)
                              _lhsOinhmap =
                                  ({-# LINE 198 "src-ag/KWOrder.ag" #-}
                                   Map.empty
                                   {-# LINE 831 "src-ag/KWOrder.hs" #-}
                                   )
                              -- use rule "src-ag/KWOrder.ag"(line 199, column 33)
                              _lhsOsynmap =
                                  ({-# LINE 199 "src-ag/KWOrder.ag" #-}
                                   Map.empty
                                   {-# LINE 837 "src-ag/KWOrder.hs" #-}
                                   )
                              -- copy rule (chain)
                              _lhsOrulenumber =
                                  ({-# LINE 33 "src-ag/KWOrder.ag" #-}
                                   _lhsIrulenumber
                                   {-# LINE 843 "src-ag/KWOrder.hs" #-}
                                   )
                          in  ( _lhsOdepinfo,_lhsOinhmap,_lhsOrulenumber,_lhsOsynmap))) )
-- Pattern -----------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         copy                 : SELF 
         vertices             : Set.Set Vertex
   alternatives:
      alternative Alias:
         child field          : {Identifier}
         child attr           : {Identifier}
         child pat            : Pattern 
         child parts          : Patterns 
         visit 0:
            local vertex      : _
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
newtype T_Pattern  = T_Pattern (( Pattern ,(Set.Set Vertex)))
data Inh_Pattern  = Inh_Pattern {}
data Syn_Pattern  = Syn_Pattern {copy_Syn_Pattern :: Pattern ,vertices_Syn_Pattern :: (Set.Set Vertex)}
wrap_Pattern :: T_Pattern  ->
                Inh_Pattern  ->
                Syn_Pattern 
wrap_Pattern (T_Pattern sem ) (Inh_Pattern )  =
    (let ( _lhsOcopy,_lhsOvertices) = sem 
     in  (Syn_Pattern _lhsOcopy _lhsOvertices ))
sem_Pattern_Alias :: Identifier ->
                     Identifier ->
                     T_Pattern  ->
                     T_Patterns  ->
                     T_Pattern 
sem_Pattern_Alias field_ attr_ (T_Pattern pat_ ) (T_Patterns parts_ )  =
    (T_Pattern (let _lhsOvertices :: (Set.Set Vertex)
                    _lhsOcopy :: Pattern 
                    _patIcopy :: Pattern 
                    _patIvertices :: (Set.Set Vertex)
                    _partsIcopy :: Patterns 
                    _partsIvertices :: (Set.Set Vertex)
                    -- "src-ag/KWOrder.ag"(line 92, column 12)
                    _vertex =
                        ({-# LINE 92 "src-ag/KWOrder.ag" #-}
                         if                  field_ == _INST then VChild attr_
                         else VAttr (if      field_ == _LHS  then Syn
                                     else if field_ == _LOC  then Loc
                                     else                         Inh) field_ attr_
                         {-# LINE 922 "src-ag/KWOrder.hs" #-}
                         )
                    -- "src-ag/KWOrder.ag"(line 96, column 12)
                    _lhsOvertices =
                        ({-# LINE 96 "src-ag/KWOrder.ag" #-}
                         Set.insert _vertex     $ _partsIvertices `Set.union` _patIvertices
                         {-# LINE 928 "src-ag/KWOrder.hs" #-}
                         )
                    -- self rule
                    _copy =
                        ({-# LINE 23 "src-ag/Patterns.ag" #-}
                         Alias field_ attr_ _patIcopy _partsIcopy
                         {-# LINE 934 "src-ag/KWOrder.hs" #-}
                         )
                    -- self rule
                    _lhsOcopy =
                        ({-# LINE 23 "src-ag/Patterns.ag" #-}
                         _copy
                         {-# LINE 940 "src-ag/KWOrder.hs" #-}
                         )
                    ( _patIcopy,_patIvertices) =
                        pat_ 
                    ( _partsIcopy,_partsIvertices) =
                        parts_ 
                in  ( _lhsOcopy,_lhsOvertices)) )
sem_Pattern_Constr :: ConstructorIdent ->
                      T_Patterns  ->
                      T_Pattern 
sem_Pattern_Constr name_ (T_Patterns pats_ )  =
    (T_Pattern (let _lhsOvertices :: (Set.Set Vertex)
                    _lhsOcopy :: Pattern 
                    _patsIcopy :: Patterns 
                    _patsIvertices :: (Set.Set Vertex)
                    -- use rule "src-ag/KWOrder.ag"(line 76, column 36)
                    _lhsOvertices =
                        ({-# LINE 76 "src-ag/KWOrder.ag" #-}
                         _patsIvertices
                         {-# LINE 959 "src-ag/KWOrder.hs" #-}
                         )
                    -- self rule
                    _copy =
                        ({-# LINE 23 "src-ag/Patterns.ag" #-}
                         Constr name_ _patsIcopy
                         {-# LINE 965 "src-ag/KWOrder.hs" #-}
                         )
                    -- self rule
                    _lhsOcopy =
                        ({-# LINE 23 "src-ag/Patterns.ag" #-}
                         _copy
                         {-# LINE 971 "src-ag/KWOrder.hs" #-}
                         )
                    ( _patsIcopy,_patsIvertices) =
                        pats_ 
                in  ( _lhsOcopy,_lhsOvertices)) )
sem_Pattern_Irrefutable :: T_Pattern  ->
                           T_Pattern 
sem_Pattern_Irrefutable (T_Pattern pat_ )  =
    (T_Pattern (let _lhsOvertices :: (Set.Set Vertex)
                    _lhsOcopy :: Pattern 
                    _patIcopy :: Pattern 
                    _patIvertices :: (Set.Set Vertex)
                    -- use rule "src-ag/KWOrder.ag"(line 76, column 36)
                    _lhsOvertices =
                        ({-# LINE 76 "src-ag/KWOrder.ag" #-}
                         _patIvertices
                         {-# LINE 987 "src-ag/KWOrder.hs" #-}
                         )
                    -- self rule
                    _copy =
                        ({-# LINE 23 "src-ag/Patterns.ag" #-}
                         Irrefutable _patIcopy
                         {-# LINE 993 "src-ag/KWOrder.hs" #-}
                         )
                    -- self rule
                    _lhsOcopy =
                        ({-# LINE 23 "src-ag/Patterns.ag" #-}
                         _copy
                         {-# LINE 999 "src-ag/KWOrder.hs" #-}
                         )
                    ( _patIcopy,_patIvertices) =
                        pat_ 
                in  ( _lhsOcopy,_lhsOvertices)) )
sem_Pattern_Product :: Pos ->
                       T_Patterns  ->
                       T_Pattern 
sem_Pattern_Product pos_ (T_Patterns pats_ )  =
    (T_Pattern (let _lhsOvertices :: (Set.Set Vertex)
                    _lhsOcopy :: Pattern 
                    _patsIcopy :: Patterns 
                    _patsIvertices :: (Set.Set Vertex)
                    -- use rule "src-ag/KWOrder.ag"(line 76, column 36)
                    _lhsOvertices =
                        ({-# LINE 76 "src-ag/KWOrder.ag" #-}
                         _patsIvertices
                         {-# LINE 1016 "src-ag/KWOrder.hs" #-}
                         )
                    -- self rule
                    _copy =
                        ({-# LINE 23 "src-ag/Patterns.ag" #-}
                         Product pos_ _patsIcopy
                         {-# LINE 1022 "src-ag/KWOrder.hs" #-}
                         )
                    -- self rule
                    _lhsOcopy =
                        ({-# LINE 23 "src-ag/Patterns.ag" #-}
                         _copy
                         {-# LINE 1028 "src-ag/KWOrder.hs" #-}
                         )
                    ( _patsIcopy,_patsIvertices) =
                        pats_ 
                in  ( _lhsOcopy,_lhsOvertices)) )
sem_Pattern_Underscore :: Pos ->
                          T_Pattern 
sem_Pattern_Underscore pos_  =
    (T_Pattern (let _lhsOvertices :: (Set.Set Vertex)
                    _lhsOcopy :: Pattern 
                    -- use rule "src-ag/KWOrder.ag"(line 76, column 36)
                    _lhsOvertices =
                        ({-# LINE 76 "src-ag/KWOrder.ag" #-}
                         Set.empty
                         {-# LINE 1042 "src-ag/KWOrder.hs" #-}
                         )
                    -- self rule
                    _copy =
                        ({-# LINE 23 "src-ag/Patterns.ag" #-}
                         Underscore pos_
                         {-# LINE 1048 "src-ag/KWOrder.hs" #-}
                         )
                    -- self rule
                    _lhsOcopy =
                        ({-# LINE 23 "src-ag/Patterns.ag" #-}
                         _copy
                         {-# LINE 1054 "src-ag/KWOrder.hs" #-}
                         )
                in  ( _lhsOcopy,_lhsOvertices)) )
-- Patterns ----------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         copy                 : SELF 
         vertices             : Set.Set Vertex
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
newtype T_Patterns  = T_Patterns (( Patterns ,(Set.Set Vertex)))
data Inh_Patterns  = Inh_Patterns {}
data Syn_Patterns  = Syn_Patterns {copy_Syn_Patterns :: Patterns ,vertices_Syn_Patterns :: (Set.Set Vertex)}
wrap_Patterns :: T_Patterns  ->
                 Inh_Patterns  ->
                 Syn_Patterns 
wrap_Patterns (T_Patterns sem ) (Inh_Patterns )  =
    (let ( _lhsOcopy,_lhsOvertices) = sem 
     in  (Syn_Patterns _lhsOcopy _lhsOvertices ))
sem_Patterns_Cons :: T_Pattern  ->
                     T_Patterns  ->
                     T_Patterns 
sem_Patterns_Cons (T_Pattern hd_ ) (T_Patterns tl_ )  =
    (T_Patterns (let _lhsOvertices :: (Set.Set Vertex)
                     _lhsOcopy :: Patterns 
                     _hdIcopy :: Pattern 
                     _hdIvertices :: (Set.Set Vertex)
                     _tlIcopy :: Patterns 
                     _tlIvertices :: (Set.Set Vertex)
                     -- use rule "src-ag/KWOrder.ag"(line 76, column 36)
                     _lhsOvertices =
                         ({-# LINE 76 "src-ag/KWOrder.ag" #-}
                          _hdIvertices `Set.union` _tlIvertices
                          {-# LINE 1102 "src-ag/KWOrder.hs" #-}
                          )
                     -- self rule
                     _copy =
                         ({-# LINE 23 "src-ag/Patterns.ag" #-}
                          (:) _hdIcopy _tlIcopy
                          {-# LINE 1108 "src-ag/KWOrder.hs" #-}
                          )
                     -- self rule
                     _lhsOcopy =
                         ({-# LINE 23 "src-ag/Patterns.ag" #-}
                          _copy
                          {-# LINE 1114 "src-ag/KWOrder.hs" #-}
                          )
                     ( _hdIcopy,_hdIvertices) =
                         hd_ 
                     ( _tlIcopy,_tlIvertices) =
                         tl_ 
                 in  ( _lhsOcopy,_lhsOvertices)) )
sem_Patterns_Nil :: T_Patterns 
sem_Patterns_Nil  =
    (T_Patterns (let _lhsOvertices :: (Set.Set Vertex)
                     _lhsOcopy :: Patterns 
                     -- use rule "src-ag/KWOrder.ag"(line 76, column 36)
                     _lhsOvertices =
                         ({-# LINE 76 "src-ag/KWOrder.ag" #-}
                          Set.empty
                          {-# LINE 1129 "src-ag/KWOrder.hs" #-}
                          )
                     -- self rule
                     _copy =
                         ({-# LINE 23 "src-ag/Patterns.ag" #-}
                          []
                          {-# LINE 1135 "src-ag/KWOrder.hs" #-}
                          )
                     -- self rule
                     _lhsOcopy =
                         ({-# LINE 23 "src-ag/Patterns.ag" #-}
                          _copy
                          {-# LINE 1141 "src-ag/KWOrder.hs" #-}
                          )
                 in  ( _lhsOcopy,_lhsOvertices)) )
-- Production --------------------------------------------------
{-
   visit 0:
      chained attribute:
         rulenumber           : Int
      synthesized attribute:
         depgraph             : ProdDependencyGraph
   alternatives:
      alternative Production:
         child con            : {ConstructorIdent}
         child children       : Children 
         child rules          : Rules 
         child typeSigs       : TypeSigs 
         visit 0:
            local vertices    : _
            local edges       : _
-}
-- cata
sem_Production :: Production  ->
                  T_Production 
sem_Production (Production _con _children _rules _typeSigs )  =
    (sem_Production_Production _con (sem_Children _children ) (sem_Rules _rules ) (sem_TypeSigs _typeSigs ) )
-- semantic domain
newtype T_Production  = T_Production (Int ->
                                      ( ProdDependencyGraph,Int))
data Inh_Production  = Inh_Production {rulenumber_Inh_Production :: Int}
data Syn_Production  = Syn_Production {depgraph_Syn_Production :: ProdDependencyGraph,rulenumber_Syn_Production :: Int}
wrap_Production :: T_Production  ->
                   Inh_Production  ->
                   Syn_Production 
wrap_Production (T_Production sem ) (Inh_Production _lhsIrulenumber )  =
    (let ( _lhsOdepgraph,_lhsOrulenumber) = sem _lhsIrulenumber 
     in  (Syn_Production _lhsOdepgraph _lhsOrulenumber ))
sem_Production_Production :: ConstructorIdent ->
                             T_Children  ->
                             T_Rules  ->
                             T_TypeSigs  ->
                             T_Production 
sem_Production_Production con_ (T_Children children_ ) (T_Rules rules_ ) (T_TypeSigs typeSigs_ )  =
    (T_Production (\ _lhsIrulenumber ->
                       (let _lhsOdepgraph :: ProdDependencyGraph
                            _lhsOrulenumber :: Int
                            _rulesOrulenumber :: Int
                            _childrenIechilds :: EChildren
                            _childrenIedges :: (Set.Set Edge)
                            _childrenInontnames :: ([(Identifier, Identifier)])
                            _childrenIvertices :: (Set.Set Vertex)
                            _rulesIedges :: (Set.Set Edge)
                            _rulesIerules :: ERules
                            _rulesIrulenumber :: Int
                            _rulesIvertices :: (Set.Set Vertex)
                            -- "src-ag/KWOrder.ag"(line 114, column 17)
                            _vertices =
                                ({-# LINE 114 "src-ag/KWOrder.ag" #-}
                                 _rulesIvertices `Set.union` _childrenIvertices
                                 {-# LINE 1199 "src-ag/KWOrder.hs" #-}
                                 )
                            -- "src-ag/KWOrder.ag"(line 134, column 17)
                            _edges =
                                ({-# LINE 134 "src-ag/KWOrder.ag" #-}
                                 _rulesIedges `Set.union` _childrenIedges
                                 {-# LINE 1205 "src-ag/KWOrder.hs" #-}
                                 )
                            -- "src-ag/KWOrder.ag"(line 149, column 17)
                            _lhsOdepgraph =
                                ({-# LINE 149 "src-ag/KWOrder.ag" #-}
                                 ProdDependencyGraph { pdgVertices    = Set.toList _vertices
                                                     , pdgEdges       = Set.toList _edges
                                                     , pdgRules       = _rulesIerules
                                                     , pdgChilds      = _childrenIechilds
                                                     , pdgProduction  = con_
                                                     , pdgChildMap    = _childrenInontnames }
                                 {-# LINE 1216 "src-ag/KWOrder.hs" #-}
                                 )
                            -- copy rule (up)
                            _lhsOrulenumber =
                                ({-# LINE 33 "src-ag/KWOrder.ag" #-}
                                 _rulesIrulenumber
                                 {-# LINE 1222 "src-ag/KWOrder.hs" #-}
                                 )
                            -- copy rule (down)
                            _rulesOrulenumber =
                                ({-# LINE 33 "src-ag/KWOrder.ag" #-}
                                 _lhsIrulenumber
                                 {-# LINE 1228 "src-ag/KWOrder.hs" #-}
                                 )
                            ( _childrenIechilds,_childrenIedges,_childrenInontnames,_childrenIvertices) =
                                children_ 
                            ( _rulesIedges,_rulesIerules,_rulesIrulenumber,_rulesIvertices) =
                                rules_ _rulesOrulenumber 
                        in  ( _lhsOdepgraph,_lhsOrulenumber))) )
-- Productions -------------------------------------------------
{-
   visit 0:
      chained attribute:
         rulenumber           : Int
      synthesized attribute:
         depgraph             : [ProdDependencyGraph]
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
newtype T_Productions  = T_Productions (Int ->
                                        ( ([ProdDependencyGraph]),Int))
data Inh_Productions  = Inh_Productions {rulenumber_Inh_Productions :: Int}
data Syn_Productions  = Syn_Productions {depgraph_Syn_Productions :: ([ProdDependencyGraph]),rulenumber_Syn_Productions :: Int}
wrap_Productions :: T_Productions  ->
                    Inh_Productions  ->
                    Syn_Productions 
wrap_Productions (T_Productions sem ) (Inh_Productions _lhsIrulenumber )  =
    (let ( _lhsOdepgraph,_lhsOrulenumber) = sem _lhsIrulenumber 
     in  (Syn_Productions _lhsOdepgraph _lhsOrulenumber ))
sem_Productions_Cons :: T_Production  ->
                        T_Productions  ->
                        T_Productions 
sem_Productions_Cons (T_Production hd_ ) (T_Productions tl_ )  =
    (T_Productions (\ _lhsIrulenumber ->
                        (let _lhsOdepgraph :: ([ProdDependencyGraph])
                             _lhsOrulenumber :: Int
                             _hdOrulenumber :: Int
                             _tlOrulenumber :: Int
                             _hdIdepgraph :: ProdDependencyGraph
                             _hdIrulenumber :: Int
                             _tlIdepgraph :: ([ProdDependencyGraph])
                             _tlIrulenumber :: Int
                             -- use rule "src-ag/KWOrder.ag"(line 146, column 33)
                             _lhsOdepgraph =
                                 ({-# LINE 146 "src-ag/KWOrder.ag" #-}
                                  _hdIdepgraph : _tlIdepgraph
                                  {-# LINE 1281 "src-ag/KWOrder.hs" #-}
                                  )
                             -- copy rule (up)
                             _lhsOrulenumber =
                                 ({-# LINE 33 "src-ag/KWOrder.ag" #-}
                                  _tlIrulenumber
                                  {-# LINE 1287 "src-ag/KWOrder.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOrulenumber =
                                 ({-# LINE 33 "src-ag/KWOrder.ag" #-}
                                  _lhsIrulenumber
                                  {-# LINE 1293 "src-ag/KWOrder.hs" #-}
                                  )
                             -- copy rule (chain)
                             _tlOrulenumber =
                                 ({-# LINE 33 "src-ag/KWOrder.ag" #-}
                                  _hdIrulenumber
                                  {-# LINE 1299 "src-ag/KWOrder.hs" #-}
                                  )
                             ( _hdIdepgraph,_hdIrulenumber) =
                                 hd_ _hdOrulenumber 
                             ( _tlIdepgraph,_tlIrulenumber) =
                                 tl_ _tlOrulenumber 
                         in  ( _lhsOdepgraph,_lhsOrulenumber))) )
sem_Productions_Nil :: T_Productions 
sem_Productions_Nil  =
    (T_Productions (\ _lhsIrulenumber ->
                        (let _lhsOdepgraph :: ([ProdDependencyGraph])
                             _lhsOrulenumber :: Int
                             -- use rule "src-ag/KWOrder.ag"(line 146, column 33)
                             _lhsOdepgraph =
                                 ({-# LINE 146 "src-ag/KWOrder.ag" #-}
                                  []
                                  {-# LINE 1315 "src-ag/KWOrder.hs" #-}
                                  )
                             -- copy rule (chain)
                             _lhsOrulenumber =
                                 ({-# LINE 33 "src-ag/KWOrder.ag" #-}
                                  _lhsIrulenumber
                                  {-# LINE 1321 "src-ag/KWOrder.hs" #-}
                                  )
                         in  ( _lhsOdepgraph,_lhsOrulenumber))) )
-- Rule --------------------------------------------------------
{-
   visit 0:
      chained attribute:
         rulenumber           : Int
      synthesized attributes:
         edges                : Set.Set Edge
         erules               : ERule
         vertices             : Set.Set Vertex
   alternatives:
      alternative Rule:
         child mbName         : {Maybe Identifier}
         child pattern        : Pattern 
         child rhs            : Expression 
         child owrt           : {Bool}
         child origin         : {String}
         child explicit       : {Bool}
         visit 0:
            local rulename    : _
            local vertex      : _
            local edgesout    : _
            local edgesin     : _
-}
-- cata
sem_Rule :: Rule  ->
            T_Rule 
sem_Rule (Rule _mbName _pattern _rhs _owrt _origin _explicit )  =
    (sem_Rule_Rule _mbName (sem_Pattern _pattern ) (sem_Expression _rhs ) _owrt _origin _explicit )
-- semantic domain
newtype T_Rule  = T_Rule (Int ->
                          ( (Set.Set Edge),ERule,Int,(Set.Set Vertex)))
data Inh_Rule  = Inh_Rule {rulenumber_Inh_Rule :: Int}
data Syn_Rule  = Syn_Rule {edges_Syn_Rule :: (Set.Set Edge),erules_Syn_Rule :: ERule,rulenumber_Syn_Rule :: Int,vertices_Syn_Rule :: (Set.Set Vertex)}
wrap_Rule :: T_Rule  ->
             Inh_Rule  ->
             Syn_Rule 
wrap_Rule (T_Rule sem ) (Inh_Rule _lhsIrulenumber )  =
    (let ( _lhsOedges,_lhsOerules,_lhsOrulenumber,_lhsOvertices) = sem _lhsIrulenumber 
     in  (Syn_Rule _lhsOedges _lhsOerules _lhsOrulenumber _lhsOvertices ))
sem_Rule_Rule :: (Maybe Identifier) ->
                 T_Pattern  ->
                 T_Expression  ->
                 Bool ->
                 String ->
                 Bool ->
                 T_Rule 
sem_Rule_Rule mbName_ (T_Pattern pattern_ ) (T_Expression rhs_ ) owrt_ origin_ explicit_  =
    (T_Rule (\ _lhsIrulenumber ->
                 (let _lhsOrulenumber :: Int
                      _lhsOerules :: ERule
                      _lhsOvertices :: (Set.Set Vertex)
                      _lhsOedges :: (Set.Set Edge)
                      _patternIcopy :: Pattern 
                      _patternIvertices :: (Set.Set Vertex)
                      _rhsIcopy :: Expression 
                      _rhsIvertices :: (Set.Set Vertex)
                      -- "src-ag/KWOrder.ag"(line 39, column 11)
                      _lhsOrulenumber =
                          ({-# LINE 39 "src-ag/KWOrder.ag" #-}
                           _lhsIrulenumber + 1
                           {-# LINE 1384 "src-ag/KWOrder.hs" #-}
                           )
                      -- "src-ag/KWOrder.ag"(line 40, column 11)
                      _rulename =
                          ({-# LINE 40 "src-ag/KWOrder.ag" #-}
                           maybe (identifier $ "rule" ++ show _lhsIrulenumber) id mbName_
                           {-# LINE 1390 "src-ag/KWOrder.hs" #-}
                           )
                      -- "src-ag/KWOrder.ag"(line 51, column 10)
                      _lhsOerules =
                          ({-# LINE 51 "src-ag/KWOrder.ag" #-}
                           ERule _rulename
                                 _patternIcopy
                                 _rhsIcopy
                                 owrt_
                                 origin_
                                 explicit_
                           {-# LINE 1401 "src-ag/KWOrder.hs" #-}
                           )
                      -- "src-ag/KWOrder.ag"(line 109, column 11)
                      _vertex =
                          ({-# LINE 109 "src-ag/KWOrder.ag" #-}
                           VRule _rulename
                           {-# LINE 1407 "src-ag/KWOrder.hs" #-}
                           )
                      -- "src-ag/KWOrder.ag"(line 110, column 11)
                      _lhsOvertices =
                          ({-# LINE 110 "src-ag/KWOrder.ag" #-}
                           Set.insert _vertex     $ _patternIvertices `Set.union` _rhsIvertices
                           {-# LINE 1413 "src-ag/KWOrder.hs" #-}
                           )
                      -- "src-ag/KWOrder.ag"(line 122, column 11)
                      _edgesout =
                          ({-# LINE 122 "src-ag/KWOrder.ag" #-}
                           map ((,) _vertex    ) (Set.toList _rhsIvertices)
                           {-# LINE 1419 "src-ag/KWOrder.hs" #-}
                           )
                      -- "src-ag/KWOrder.ag"(line 123, column 11)
                      _edgesin =
                          ({-# LINE 123 "src-ag/KWOrder.ag" #-}
                           map (flip (,) _vertex    ) (Set.toList _patternIvertices)
                           {-# LINE 1425 "src-ag/KWOrder.hs" #-}
                           )
                      -- "src-ag/KWOrder.ag"(line 124, column 11)
                      _lhsOedges =
                          ({-# LINE 124 "src-ag/KWOrder.ag" #-}
                           Set.fromList $ _edgesout     ++ _edgesin
                           {-# LINE 1431 "src-ag/KWOrder.hs" #-}
                           )
                      ( _patternIcopy,_patternIvertices) =
                          pattern_ 
                      ( _rhsIcopy,_rhsIvertices) =
                          rhs_ 
                  in  ( _lhsOedges,_lhsOerules,_lhsOrulenumber,_lhsOvertices))) )
-- Rules -------------------------------------------------------
{-
   visit 0:
      chained attribute:
         rulenumber           : Int
      synthesized attributes:
         edges                : Set.Set Edge
         erules               : ERules
         vertices             : Set.Set Vertex
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
newtype T_Rules  = T_Rules (Int ->
                            ( (Set.Set Edge),ERules,Int,(Set.Set Vertex)))
data Inh_Rules  = Inh_Rules {rulenumber_Inh_Rules :: Int}
data Syn_Rules  = Syn_Rules {edges_Syn_Rules :: (Set.Set Edge),erules_Syn_Rules :: ERules,rulenumber_Syn_Rules :: Int,vertices_Syn_Rules :: (Set.Set Vertex)}
wrap_Rules :: T_Rules  ->
              Inh_Rules  ->
              Syn_Rules 
wrap_Rules (T_Rules sem ) (Inh_Rules _lhsIrulenumber )  =
    (let ( _lhsOedges,_lhsOerules,_lhsOrulenumber,_lhsOvertices) = sem _lhsIrulenumber 
     in  (Syn_Rules _lhsOedges _lhsOerules _lhsOrulenumber _lhsOvertices ))
sem_Rules_Cons :: T_Rule  ->
                  T_Rules  ->
                  T_Rules 
sem_Rules_Cons (T_Rule hd_ ) (T_Rules tl_ )  =
    (T_Rules (\ _lhsIrulenumber ->
                  (let _lhsOedges :: (Set.Set Edge)
                       _lhsOerules :: ERules
                       _lhsOvertices :: (Set.Set Vertex)
                       _lhsOrulenumber :: Int
                       _hdOrulenumber :: Int
                       _tlOrulenumber :: Int
                       _hdIedges :: (Set.Set Edge)
                       _hdIerules :: ERule
                       _hdIrulenumber :: Int
                       _hdIvertices :: (Set.Set Vertex)
                       _tlIedges :: (Set.Set Edge)
                       _tlIerules :: ERules
                       _tlIrulenumber :: Int
                       _tlIvertices :: (Set.Set Vertex)
                       -- use rule "src-ag/KWOrder.ag"(line 118, column 33)
                       _lhsOedges =
                           ({-# LINE 118 "src-ag/KWOrder.ag" #-}
                            _hdIedges `Set.union` _tlIedges
                            {-# LINE 1492 "src-ag/KWOrder.hs" #-}
                            )
                       -- use rule "src-ag/KWOrder.ag"(line 48, column 25)
                       _lhsOerules =
                           ({-# LINE 48 "src-ag/KWOrder.ag" #-}
                            _hdIerules : _tlIerules
                            {-# LINE 1498 "src-ag/KWOrder.hs" #-}
                            )
                       -- use rule "src-ag/KWOrder.ag"(line 76, column 36)
                       _lhsOvertices =
                           ({-# LINE 76 "src-ag/KWOrder.ag" #-}
                            _hdIvertices `Set.union` _tlIvertices
                            {-# LINE 1504 "src-ag/KWOrder.hs" #-}
                            )
                       -- copy rule (up)
                       _lhsOrulenumber =
                           ({-# LINE 33 "src-ag/KWOrder.ag" #-}
                            _tlIrulenumber
                            {-# LINE 1510 "src-ag/KWOrder.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOrulenumber =
                           ({-# LINE 33 "src-ag/KWOrder.ag" #-}
                            _lhsIrulenumber
                            {-# LINE 1516 "src-ag/KWOrder.hs" #-}
                            )
                       -- copy rule (chain)
                       _tlOrulenumber =
                           ({-# LINE 33 "src-ag/KWOrder.ag" #-}
                            _hdIrulenumber
                            {-# LINE 1522 "src-ag/KWOrder.hs" #-}
                            )
                       ( _hdIedges,_hdIerules,_hdIrulenumber,_hdIvertices) =
                           hd_ _hdOrulenumber 
                       ( _tlIedges,_tlIerules,_tlIrulenumber,_tlIvertices) =
                           tl_ _tlOrulenumber 
                   in  ( _lhsOedges,_lhsOerules,_lhsOrulenumber,_lhsOvertices))) )
sem_Rules_Nil :: T_Rules 
sem_Rules_Nil  =
    (T_Rules (\ _lhsIrulenumber ->
                  (let _lhsOedges :: (Set.Set Edge)
                       _lhsOerules :: ERules
                       _lhsOvertices :: (Set.Set Vertex)
                       _lhsOrulenumber :: Int
                       -- use rule "src-ag/KWOrder.ag"(line 118, column 33)
                       _lhsOedges =
                           ({-# LINE 118 "src-ag/KWOrder.ag" #-}
                            Set.empty
                            {-# LINE 1540 "src-ag/KWOrder.hs" #-}
                            )
                       -- use rule "src-ag/KWOrder.ag"(line 48, column 25)
                       _lhsOerules =
                           ({-# LINE 48 "src-ag/KWOrder.ag" #-}
                            []
                            {-# LINE 1546 "src-ag/KWOrder.hs" #-}
                            )
                       -- use rule "src-ag/KWOrder.ag"(line 76, column 36)
                       _lhsOvertices =
                           ({-# LINE 76 "src-ag/KWOrder.ag" #-}
                            Set.empty
                            {-# LINE 1552 "src-ag/KWOrder.hs" #-}
                            )
                       -- copy rule (chain)
                       _lhsOrulenumber =
                           ({-# LINE 33 "src-ag/KWOrder.ag" #-}
                            _lhsIrulenumber
                            {-# LINE 1558 "src-ag/KWOrder.hs" #-}
                            )
                   in  ( _lhsOedges,_lhsOerules,_lhsOrulenumber,_lhsOvertices))) )
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