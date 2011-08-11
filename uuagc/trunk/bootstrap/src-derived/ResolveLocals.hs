

-- UUAGC 0.9.38.6.5 (src-ag/ResolveLocals.ag)
module ResolveLocals where
{-# LINE 7 "src-ag/ResolveLocals.ag" #-}

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Map(Map)
import qualified Data.Sequence as Seq
import Data.Sequence(Seq,(><))
import CommonTypes
import Patterns
import ErrorMessages
import AbstractSyntax
import Expression
import Options
import HsToken(HsTokensRoot(HsTokensRoot))
import HsTokenScanner(lexTokens)
import SemHsTokens(sem_HsTokensRoot,wrap_HsTokensRoot, Syn_HsTokensRoot(..),Inh_HsTokensRoot(..))
import Data.Maybe
{-# LINE 23 "dist/build/uuagc/uuagc-tmp/ResolveLocals.hs" #-}

{-# LINE 2 "src-ag/AbstractSyntax.ag" #-}

-- AbstractSyntax.ag imports
import Data.Set(Set)
import Data.Map(Map)
import Patterns    (Pattern(..),Patterns)
import Expression  (Expression(..))
import CommonTypes
{-# LINE 33 "dist/build/uuagc/uuagc-tmp/ResolveLocals.hs" #-}

{-# LINE 2 "src-ag/Patterns.ag" #-}

-- Patterns.ag imports
import UU.Scanner.Position(Pos)
import CommonTypes (ConstructorIdent,Identifier)
{-# LINE 40 "dist/build/uuagc/uuagc-tmp/ResolveLocals.hs" #-}

{-# LINE 2 "src-ag/Expression.ag" #-}

import UU.Scanner.Position(Pos)
import HsToken
{-# LINE 46 "dist/build/uuagc/uuagc-tmp/ResolveLocals.hs" #-}
-- Child -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allfields            : [(Identifier,Type,Maybe (Maybe Type))]
         allnts               : [Identifier]
         attrs                : [(Identifier,Identifier)]
         con                  : Identifier
         inh                  : Attributes
         mergeMap             : Map Identifier (Identifier,[Identifier])
         nt                   : Identifier
         syn                  : Attributes
      synthesized attributes:
         attributes           : [(Identifier,Attributes,Attributes)]
         field                : (Identifier,Type,Maybe (Maybe Type))
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
sem_Child (Child _name _tp _inh _syn _virtual )  =
    (sem_Child_Child _name _tp _inh _syn _virtual )
-- semantic domain
newtype T_Child  = T_Child (([(Identifier,Type,Maybe (Maybe Type))]) ->
                            ([Identifier]) ->
                            ([(Identifier,Identifier)]) ->
                            Identifier ->
                            Attributes ->
                            (Map Identifier (Identifier,[Identifier])) ->
                            Identifier ->
                            Attributes ->
                            ( ([(Identifier,Attributes,Attributes)]),((Identifier,Type,Maybe (Maybe Type))),Child ))
data Inh_Child  = Inh_Child {allfields_Inh_Child :: ([(Identifier,Type,Maybe (Maybe Type))]),allnts_Inh_Child :: ([Identifier]),attrs_Inh_Child :: ([(Identifier,Identifier)]),con_Inh_Child :: Identifier,inh_Inh_Child :: Attributes,mergeMap_Inh_Child :: (Map Identifier (Identifier,[Identifier])),nt_Inh_Child :: Identifier,syn_Inh_Child :: Attributes}
data Syn_Child  = Syn_Child {attributes_Syn_Child :: ([(Identifier,Attributes,Attributes)]),field_Syn_Child :: ((Identifier,Type,Maybe (Maybe Type))),output_Syn_Child :: Child }
wrap_Child :: T_Child  ->
              Inh_Child  ->
              Syn_Child 
wrap_Child (T_Child sem ) (Inh_Child _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsIinh _lhsImergeMap _lhsInt _lhsIsyn )  =
    (let ( _lhsOattributes,_lhsOfield,_lhsOoutput) = sem _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsIinh _lhsImergeMap _lhsInt _lhsIsyn 
     in  (Syn_Child _lhsOattributes _lhsOfield _lhsOoutput ))
sem_Child_Child :: Identifier ->
                   Type ->
                   Attributes ->
                   Attributes ->
                   (Maybe (Maybe Type)) ->
                   T_Child 
sem_Child_Child name_ tp_ inh_ syn_ virtual_  =
    (T_Child (\ _lhsIallfields
                _lhsIallnts
                _lhsIattrs
                _lhsIcon
                _lhsIinh
                _lhsImergeMap
                _lhsInt
                _lhsIsyn ->
                  (let _lhsOattributes :: ([(Identifier,Attributes,Attributes)])
                       _lhsOfield :: ((Identifier,Type,Maybe (Maybe Type)))
                       _lhsOoutput :: Child 
                       -- "src-ag/ResolveLocals.ag"(line 76, column 11)
                       _lhsOattributes =
                           ({-# LINE 76 "src-ag/ResolveLocals.ag" #-}
                            [(name_, inh_, syn_)]
                            {-# LINE 118 "src-ag/ResolveLocals.hs" #-}
                            )
                       -- "src-ag/ResolveLocals.ag"(line 79, column 11)
                       _lhsOfield =
                           ({-# LINE 79 "src-ag/ResolveLocals.ag" #-}
                            (name_, tp_, virtual_)
                            {-# LINE 124 "src-ag/ResolveLocals.hs" #-}
                            )
                       -- self rule
                       _output =
                           ({-# LINE 39 "src-ag/ResolveLocals.ag" #-}
                            Child name_ tp_ inh_ syn_ virtual_
                            {-# LINE 130 "src-ag/ResolveLocals.hs" #-}
                            )
                       -- self rule
                       _lhsOoutput =
                           ({-# LINE 39 "src-ag/ResolveLocals.ag" #-}
                            _output
                            {-# LINE 136 "src-ag/ResolveLocals.hs" #-}
                            )
                   in  ( _lhsOattributes,_lhsOfield,_lhsOoutput))) )
-- Children ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allfields            : [(Identifier,Type,Maybe (Maybe Type))]
         allnts               : [Identifier]
         attrs                : [(Identifier,Identifier)]
         con                  : Identifier
         inh                  : Attributes
         mergeMap             : Map Identifier (Identifier,[Identifier])
         nt                   : Identifier
         syn                  : Attributes
      synthesized attributes:
         attributes           : [(Identifier,Attributes,Attributes)]
         fields               : [(Identifier,Type,Maybe (Maybe Type))]
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
sem_Children list  =
    (Prelude.foldr sem_Children_Cons sem_Children_Nil (Prelude.map sem_Child list) )
-- semantic domain
newtype T_Children  = T_Children (([(Identifier,Type,Maybe (Maybe Type))]) ->
                                  ([Identifier]) ->
                                  ([(Identifier,Identifier)]) ->
                                  Identifier ->
                                  Attributes ->
                                  (Map Identifier (Identifier,[Identifier])) ->
                                  Identifier ->
                                  Attributes ->
                                  ( ([(Identifier,Attributes,Attributes)]),([(Identifier,Type,Maybe (Maybe Type))]),Children ))
data Inh_Children  = Inh_Children {allfields_Inh_Children :: ([(Identifier,Type,Maybe (Maybe Type))]),allnts_Inh_Children :: ([Identifier]),attrs_Inh_Children :: ([(Identifier,Identifier)]),con_Inh_Children :: Identifier,inh_Inh_Children :: Attributes,mergeMap_Inh_Children :: (Map Identifier (Identifier,[Identifier])),nt_Inh_Children :: Identifier,syn_Inh_Children :: Attributes}
data Syn_Children  = Syn_Children {attributes_Syn_Children :: ([(Identifier,Attributes,Attributes)]),fields_Syn_Children :: ([(Identifier,Type,Maybe (Maybe Type))]),output_Syn_Children :: Children }
wrap_Children :: T_Children  ->
                 Inh_Children  ->
                 Syn_Children 
wrap_Children (T_Children sem ) (Inh_Children _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsIinh _lhsImergeMap _lhsInt _lhsIsyn )  =
    (let ( _lhsOattributes,_lhsOfields,_lhsOoutput) = sem _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsIinh _lhsImergeMap _lhsInt _lhsIsyn 
     in  (Syn_Children _lhsOattributes _lhsOfields _lhsOoutput ))
sem_Children_Cons :: T_Child  ->
                     T_Children  ->
                     T_Children 
sem_Children_Cons (T_Child hd_ ) (T_Children tl_ )  =
    (T_Children (\ _lhsIallfields
                   _lhsIallnts
                   _lhsIattrs
                   _lhsIcon
                   _lhsIinh
                   _lhsImergeMap
                   _lhsInt
                   _lhsIsyn ->
                     (let _lhsOfields :: ([(Identifier,Type,Maybe (Maybe Type))])
                          _lhsOattributes :: ([(Identifier,Attributes,Attributes)])
                          _lhsOoutput :: Children 
                          _hdOallfields :: ([(Identifier,Type,Maybe (Maybe Type))])
                          _hdOallnts :: ([Identifier])
                          _hdOattrs :: ([(Identifier,Identifier)])
                          _hdOcon :: Identifier
                          _hdOinh :: Attributes
                          _hdOmergeMap :: (Map Identifier (Identifier,[Identifier]))
                          _hdOnt :: Identifier
                          _hdOsyn :: Attributes
                          _tlOallfields :: ([(Identifier,Type,Maybe (Maybe Type))])
                          _tlOallnts :: ([Identifier])
                          _tlOattrs :: ([(Identifier,Identifier)])
                          _tlOcon :: Identifier
                          _tlOinh :: Attributes
                          _tlOmergeMap :: (Map Identifier (Identifier,[Identifier]))
                          _tlOnt :: Identifier
                          _tlOsyn :: Attributes
                          _hdIattributes :: ([(Identifier,Attributes,Attributes)])
                          _hdIfield :: ((Identifier,Type,Maybe (Maybe Type)))
                          _hdIoutput :: Child 
                          _tlIattributes :: ([(Identifier,Attributes,Attributes)])
                          _tlIfields :: ([(Identifier,Type,Maybe (Maybe Type))])
                          _tlIoutput :: Children 
                          -- "src-ag/ResolveLocals.ag"(line 82, column 11)
                          _lhsOfields =
                              ({-# LINE 82 "src-ag/ResolveLocals.ag" #-}
                               _hdIfield : _tlIfields
                               {-# LINE 229 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- use rule "src-ag/ResolveLocals.ag"(line 74, column 32)
                          _lhsOattributes =
                              ({-# LINE 74 "src-ag/ResolveLocals.ag" #-}
                               _hdIattributes ++ _tlIattributes
                               {-# LINE 235 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- self rule
                          _output =
                              ({-# LINE 39 "src-ag/ResolveLocals.ag" #-}
                               (:) _hdIoutput _tlIoutput
                               {-# LINE 241 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- self rule
                          _lhsOoutput =
                              ({-# LINE 39 "src-ag/ResolveLocals.ag" #-}
                               _output
                               {-# LINE 247 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOallfields =
                              ({-# LINE 63 "src-ag/ResolveLocals.ag" #-}
                               _lhsIallfields
                               {-# LINE 253 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOallnts =
                              ({-# LINE 49 "src-ag/ResolveLocals.ag" #-}
                               _lhsIallnts
                               {-# LINE 259 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOattrs =
                              ({-# LINE 63 "src-ag/ResolveLocals.ag" #-}
                               _lhsIattrs
                               {-# LINE 265 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOcon =
                              ({-# LINE 97 "src-ag/ResolveLocals.ag" #-}
                               _lhsIcon
                               {-# LINE 271 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOinh =
                              ({-# LINE 96 "src-ag/ResolveLocals.ag" #-}
                               _lhsIinh
                               {-# LINE 277 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOmergeMap =
                              ({-# LINE 123 "src-ag/ResolveLocals.ag" #-}
                               _lhsImergeMap
                               {-# LINE 283 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOnt =
                              ({-# LINE 96 "src-ag/ResolveLocals.ag" #-}
                               _lhsInt
                               {-# LINE 289 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOsyn =
                              ({-# LINE 96 "src-ag/ResolveLocals.ag" #-}
                               _lhsIsyn
                               {-# LINE 295 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOallfields =
                              ({-# LINE 63 "src-ag/ResolveLocals.ag" #-}
                               _lhsIallfields
                               {-# LINE 301 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOallnts =
                              ({-# LINE 49 "src-ag/ResolveLocals.ag" #-}
                               _lhsIallnts
                               {-# LINE 307 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOattrs =
                              ({-# LINE 63 "src-ag/ResolveLocals.ag" #-}
                               _lhsIattrs
                               {-# LINE 313 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOcon =
                              ({-# LINE 97 "src-ag/ResolveLocals.ag" #-}
                               _lhsIcon
                               {-# LINE 319 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOinh =
                              ({-# LINE 96 "src-ag/ResolveLocals.ag" #-}
                               _lhsIinh
                               {-# LINE 325 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOmergeMap =
                              ({-# LINE 123 "src-ag/ResolveLocals.ag" #-}
                               _lhsImergeMap
                               {-# LINE 331 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOnt =
                              ({-# LINE 96 "src-ag/ResolveLocals.ag" #-}
                               _lhsInt
                               {-# LINE 337 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOsyn =
                              ({-# LINE 96 "src-ag/ResolveLocals.ag" #-}
                               _lhsIsyn
                               {-# LINE 343 "src-ag/ResolveLocals.hs" #-}
                               )
                          ( _hdIattributes,_hdIfield,_hdIoutput) =
                              hd_ _hdOallfields _hdOallnts _hdOattrs _hdOcon _hdOinh _hdOmergeMap _hdOnt _hdOsyn 
                          ( _tlIattributes,_tlIfields,_tlIoutput) =
                              tl_ _tlOallfields _tlOallnts _tlOattrs _tlOcon _tlOinh _tlOmergeMap _tlOnt _tlOsyn 
                      in  ( _lhsOattributes,_lhsOfields,_lhsOoutput))) )
sem_Children_Nil :: T_Children 
sem_Children_Nil  =
    (T_Children (\ _lhsIallfields
                   _lhsIallnts
                   _lhsIattrs
                   _lhsIcon
                   _lhsIinh
                   _lhsImergeMap
                   _lhsInt
                   _lhsIsyn ->
                     (let _lhsOfields :: ([(Identifier,Type,Maybe (Maybe Type))])
                          _lhsOattributes :: ([(Identifier,Attributes,Attributes)])
                          _lhsOoutput :: Children 
                          -- "src-ag/ResolveLocals.ag"(line 83, column 11)
                          _lhsOfields =
                              ({-# LINE 83 "src-ag/ResolveLocals.ag" #-}
                               []
                               {-# LINE 367 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- use rule "src-ag/ResolveLocals.ag"(line 74, column 32)
                          _lhsOattributes =
                              ({-# LINE 74 "src-ag/ResolveLocals.ag" #-}
                               []
                               {-# LINE 373 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- self rule
                          _output =
                              ({-# LINE 39 "src-ag/ResolveLocals.ag" #-}
                               []
                               {-# LINE 379 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- self rule
                          _lhsOoutput =
                              ({-# LINE 39 "src-ag/ResolveLocals.ag" #-}
                               _output
                               {-# LINE 385 "src-ag/ResolveLocals.hs" #-}
                               )
                      in  ( _lhsOattributes,_lhsOfields,_lhsOoutput))) )
-- Expression --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allfields            : [(Identifier,Type,Maybe (Maybe Type))]
         allnts               : [Identifier]
         attrs                : [(Identifier,Identifier)]
         con                  : Identifier
         mergeMap             : Map Identifier (Identifier,[Identifier])
         nt                   : Identifier
         options              : Options
      synthesized attributes:
         errors               : Seq Error
         output               : SELF 
   alternatives:
      alternative Expression:
         child pos            : {Pos}
         child tks            : {[HsToken]}
         visit 0:
            local _tup1       : _
            local errors      : _
            local newTks      : _
            local output      : _
-}
-- cata
sem_Expression :: Expression  ->
                  T_Expression 
sem_Expression (Expression _pos _tks )  =
    (sem_Expression_Expression _pos _tks )
-- semantic domain
newtype T_Expression  = T_Expression (([(Identifier,Type,Maybe (Maybe Type))]) ->
                                      ([Identifier]) ->
                                      ([(Identifier,Identifier)]) ->
                                      Identifier ->
                                      (Map Identifier (Identifier,[Identifier])) ->
                                      Identifier ->
                                      Options ->
                                      ( (Seq Error),Expression ))
data Inh_Expression  = Inh_Expression {allfields_Inh_Expression :: ([(Identifier,Type,Maybe (Maybe Type))]),allnts_Inh_Expression :: ([Identifier]),attrs_Inh_Expression :: ([(Identifier,Identifier)]),con_Inh_Expression :: Identifier,mergeMap_Inh_Expression :: (Map Identifier (Identifier,[Identifier])),nt_Inh_Expression :: Identifier,options_Inh_Expression :: Options}
data Syn_Expression  = Syn_Expression {errors_Syn_Expression :: (Seq Error),output_Syn_Expression :: Expression }
wrap_Expression :: T_Expression  ->
                   Inh_Expression  ->
                   Syn_Expression 
wrap_Expression (T_Expression sem ) (Inh_Expression _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsImergeMap _lhsInt _lhsIoptions )  =
    (let ( _lhsOerrors,_lhsOoutput) = sem _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsImergeMap _lhsInt _lhsIoptions 
     in  (Syn_Expression _lhsOerrors _lhsOoutput ))
sem_Expression_Expression :: Pos ->
                             ([HsToken]) ->
                             T_Expression 
sem_Expression_Expression pos_ tks_  =
    (T_Expression (\ _lhsIallfields
                     _lhsIallnts
                     _lhsIattrs
                     _lhsIcon
                     _lhsImergeMap
                     _lhsInt
                     _lhsIoptions ->
                       (let _lhsOoutput :: Expression 
                            _lhsOerrors :: (Seq Error)
                            -- "src-ag/ResolveLocals.ag"(line 137, column 21)
                            __tup1 =
                                ({-# LINE 137 "src-ag/ResolveLocals.ag" #-}
                                 let mergedChildren = [ x | (_,xs) <- Map.elems _lhsImergeMap, x <- xs ]
                                     attrsIn = filter (\(fld,_) -> not (fld `elem` mergedChildren)) _lhsIattrs
                                     inherited = Inh_HsTokensRoot
                                                 { attrs_Inh_HsTokensRoot      = attrsIn
                                                 , con_Inh_HsTokensRoot        = _lhsIcon
                                                 , allfields_Inh_HsTokensRoot  = _lhsIallfields
                                                 , allnts_Inh_HsTokensRoot     = _lhsIallnts
                                                 , nt_Inh_HsTokensRoot         = _lhsInt
                                                 }
                                     synthesized = wrap_HsTokensRoot (sem_HsTokensRoot (HsTokensRoot tks_)) inherited
                                 in (errors_Syn_HsTokensRoot synthesized, output_Syn_HsTokensRoot synthesized)
                                 {-# LINE 461 "src-ag/ResolveLocals.hs" #-}
                                 )
                            -- "src-ag/ResolveLocals.ag"(line 137, column 21)
                            (_errors,_) =
                                ({-# LINE 137 "src-ag/ResolveLocals.ag" #-}
                                 __tup1
                                 {-# LINE 467 "src-ag/ResolveLocals.hs" #-}
                                 )
                            -- "src-ag/ResolveLocals.ag"(line 137, column 21)
                            (_,_newTks) =
                                ({-# LINE 137 "src-ag/ResolveLocals.ag" #-}
                                 __tup1
                                 {-# LINE 473 "src-ag/ResolveLocals.hs" #-}
                                 )
                            -- "src-ag/ResolveLocals.ag"(line 149, column 17)
                            _lhsOoutput =
                                ({-# LINE 149 "src-ag/ResolveLocals.ag" #-}
                                 Expression pos_ _newTks
                                 {-# LINE 479 "src-ag/ResolveLocals.hs" #-}
                                 )
                            -- use rule "src-ag/ResolveLocals.ag"(line 36, column 16)
                            _lhsOerrors =
                                ({-# LINE 36 "src-ag/ResolveLocals.ag" #-}
                                 _errors
                                 {-# LINE 485 "src-ag/ResolveLocals.hs" #-}
                                 )
                            -- self rule
                            _output =
                                ({-# LINE 39 "src-ag/ResolveLocals.ag" #-}
                                 Expression pos_ tks_
                                 {-# LINE 491 "src-ag/ResolveLocals.hs" #-}
                                 )
                        in  ( _lhsOerrors,_lhsOoutput))) )
-- Grammar -----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         options              : Options
      synthesized attributes:
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
         visit 0:
            local output      : _
-}
-- cata
sem_Grammar :: Grammar  ->
               T_Grammar 
sem_Grammar (Grammar _typeSyns _useMap _derivings _wrappers _nonts _pragmas _manualAttrOrderMap _paramMap _contextMap _quantMap _uniqueMap _augmentsMap _aroundsMap _mergeMap )  =
    (sem_Grammar_Grammar _typeSyns _useMap _derivings _wrappers (sem_Nonterminals _nonts ) _pragmas _manualAttrOrderMap _paramMap _contextMap _quantMap _uniqueMap _augmentsMap _aroundsMap _mergeMap )
-- semantic domain
newtype T_Grammar  = T_Grammar (Options ->
                                ( (Seq Error),Grammar ))
data Inh_Grammar  = Inh_Grammar {options_Inh_Grammar :: Options}
data Syn_Grammar  = Syn_Grammar {errors_Syn_Grammar :: (Seq Error),output_Syn_Grammar :: Grammar }
wrap_Grammar :: T_Grammar  ->
                Inh_Grammar  ->
                Syn_Grammar 
wrap_Grammar (T_Grammar sem ) (Inh_Grammar _lhsIoptions )  =
    (let ( _lhsOerrors,_lhsOoutput) = sem _lhsIoptions 
     in  (Syn_Grammar _lhsOerrors _lhsOoutput ))
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
                    (let _nontsOallnts :: ([Identifier])
                         _nontsOmergeMap :: (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier]))))
                         _lhsOerrors :: (Seq Error)
                         _lhsOoutput :: Grammar 
                         _nontsOoptions :: Options
                         _nontsIerrors :: (Seq Error)
                         _nontsInonts :: ([(NontermIdent,[ConstructorIdent])])
                         _nontsIoutput :: Nonterminals 
                         -- "src-ag/ResolveLocals.ag"(line 52, column 13)
                         _nontsOallnts =
                             ({-# LINE 52 "src-ag/ResolveLocals.ag" #-}
                              map fst (_nontsInonts)
                              {-# LINE 566 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- "src-ag/ResolveLocals.ag"(line 112, column 14)
                         _nontsOmergeMap =
                             ({-# LINE 112 "src-ag/ResolveLocals.ag" #-}
                              Map.map (Map.map (Map.map (\(nt,srcs,_) -> (nt,srcs)))) mergeMap_
                              {-# LINE 572 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- use rule "src-ag/ResolveLocals.ag"(line 36, column 16)
                         _lhsOerrors =
                             ({-# LINE 36 "src-ag/ResolveLocals.ag" #-}
                              _nontsIerrors
                              {-# LINE 578 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- self rule
                         _output =
                             ({-# LINE 39 "src-ag/ResolveLocals.ag" #-}
                              Grammar typeSyns_ useMap_ derivings_ wrappers_ _nontsIoutput pragmas_ manualAttrOrderMap_ paramMap_ contextMap_ quantMap_ uniqueMap_ augmentsMap_ aroundsMap_ mergeMap_
                              {-# LINE 584 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- self rule
                         _lhsOoutput =
                             ({-# LINE 39 "src-ag/ResolveLocals.ag" #-}
                              _output
                              {-# LINE 590 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- copy rule (down)
                         _nontsOoptions =
                             ({-# LINE 33 "src-ag/ResolveLocals.ag" #-}
                              _lhsIoptions
                              {-# LINE 596 "src-ag/ResolveLocals.hs" #-}
                              )
                         ( _nontsIerrors,_nontsInonts,_nontsIoutput) =
                             nonts_ _nontsOallnts _nontsOmergeMap _nontsOoptions 
                     in  ( _lhsOerrors,_lhsOoutput))) )
-- Nonterminal -------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allnts               : [Identifier]
         mergeMap             : Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier])))
         options              : Options
      synthesized attributes:
         errors               : Seq Error
         nonts                : [(NontermIdent,[ConstructorIdent])]
         output               : SELF 
   alternatives:
      alternative Nonterminal:
         child nt             : {NontermIdent}
         child params         : {[Identifier]}
         child inh            : {Attributes}
         child syn            : {Attributes}
         child prods          : Productions 
         visit 0:
            local mergeMap    : _
            local output      : _
-}
-- cata
sem_Nonterminal :: Nonterminal  ->
                   T_Nonterminal 
sem_Nonterminal (Nonterminal _nt _params _inh _syn _prods )  =
    (sem_Nonterminal_Nonterminal _nt _params _inh _syn (sem_Productions _prods ) )
-- semantic domain
newtype T_Nonterminal  = T_Nonterminal (([Identifier]) ->
                                        (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier])))) ->
                                        Options ->
                                        ( (Seq Error),([(NontermIdent,[ConstructorIdent])]),Nonterminal ))
data Inh_Nonterminal  = Inh_Nonterminal {allnts_Inh_Nonterminal :: ([Identifier]),mergeMap_Inh_Nonterminal :: (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier])))),options_Inh_Nonterminal :: Options}
data Syn_Nonterminal  = Syn_Nonterminal {errors_Syn_Nonterminal :: (Seq Error),nonts_Syn_Nonterminal :: ([(NontermIdent,[ConstructorIdent])]),output_Syn_Nonterminal :: Nonterminal }
wrap_Nonterminal :: T_Nonterminal  ->
                    Inh_Nonterminal  ->
                    Syn_Nonterminal 
wrap_Nonterminal (T_Nonterminal sem ) (Inh_Nonterminal _lhsIallnts _lhsImergeMap _lhsIoptions )  =
    (let ( _lhsOerrors,_lhsOnonts,_lhsOoutput) = sem _lhsIallnts _lhsImergeMap _lhsIoptions 
     in  (Syn_Nonterminal _lhsOerrors _lhsOnonts _lhsOoutput ))
sem_Nonterminal_Nonterminal :: NontermIdent ->
                               ([Identifier]) ->
                               Attributes ->
                               Attributes ->
                               T_Productions  ->
                               T_Nonterminal 
sem_Nonterminal_Nonterminal nt_ params_ inh_ syn_ (T_Productions prods_ )  =
    (T_Nonterminal (\ _lhsIallnts
                      _lhsImergeMap
                      _lhsIoptions ->
                        (let _lhsOnonts :: ([(NontermIdent,[ConstructorIdent])])
                             _prodsOnt :: Identifier
                             _prodsOinh :: Attributes
                             _prodsOsyn :: Attributes
                             _lhsOerrors :: (Seq Error)
                             _lhsOoutput :: Nonterminal 
                             _prodsOallnts :: ([Identifier])
                             _prodsOmergeMap :: (Map ConstructorIdent (Map Identifier (Identifier,[Identifier])))
                             _prodsOoptions :: Options
                             _prodsIcons :: ([ConstructorIdent])
                             _prodsIerrors :: (Seq Error)
                             _prodsIoutput :: Productions 
                             -- "src-ag/ResolveLocals.ag"(line 56, column 19)
                             _lhsOnonts =
                                 ({-# LINE 56 "src-ag/ResolveLocals.ag" #-}
                                  [(nt_,_prodsIcons)]
                                  {-# LINE 667 "src-ag/ResolveLocals.hs" #-}
                                  )
                             -- "src-ag/ResolveLocals.ag"(line 104, column 17)
                             _prodsOnt =
                                 ({-# LINE 104 "src-ag/ResolveLocals.ag" #-}
                                  nt_
                                  {-# LINE 673 "src-ag/ResolveLocals.hs" #-}
                                  )
                             -- "src-ag/ResolveLocals.ag"(line 107, column 17)
                             _prodsOinh =
                                 ({-# LINE 107 "src-ag/ResolveLocals.ag" #-}
                                  inh_
                                  {-# LINE 679 "src-ag/ResolveLocals.hs" #-}
                                  )
                             -- "src-ag/ResolveLocals.ag"(line 108, column 17)
                             _prodsOsyn =
                                 ({-# LINE 108 "src-ag/ResolveLocals.ag" #-}
                                  syn_
                                  {-# LINE 685 "src-ag/ResolveLocals.hs" #-}
                                  )
                             -- "src-ag/ResolveLocals.ag"(line 120, column 32)
                             _mergeMap =
                                 ({-# LINE 120 "src-ag/ResolveLocals.ag" #-}
                                  Map.findWithDefault Map.empty nt_ _lhsImergeMap
                                  {-# LINE 691 "src-ag/ResolveLocals.hs" #-}
                                  )
                             -- use rule "src-ag/ResolveLocals.ag"(line 36, column 16)
                             _lhsOerrors =
                                 ({-# LINE 36 "src-ag/ResolveLocals.ag" #-}
                                  _prodsIerrors
                                  {-# LINE 697 "src-ag/ResolveLocals.hs" #-}
                                  )
                             -- self rule
                             _output =
                                 ({-# LINE 39 "src-ag/ResolveLocals.ag" #-}
                                  Nonterminal nt_ params_ inh_ syn_ _prodsIoutput
                                  {-# LINE 703 "src-ag/ResolveLocals.hs" #-}
                                  )
                             -- self rule
                             _lhsOoutput =
                                 ({-# LINE 39 "src-ag/ResolveLocals.ag" #-}
                                  _output
                                  {-# LINE 709 "src-ag/ResolveLocals.hs" #-}
                                  )
                             -- copy rule (down)
                             _prodsOallnts =
                                 ({-# LINE 49 "src-ag/ResolveLocals.ag" #-}
                                  _lhsIallnts
                                  {-# LINE 715 "src-ag/ResolveLocals.hs" #-}
                                  )
                             -- copy rule (from local)
                             _prodsOmergeMap =
                                 ({-# LINE 118 "src-ag/ResolveLocals.ag" #-}
                                  _mergeMap
                                  {-# LINE 721 "src-ag/ResolveLocals.hs" #-}
                                  )
                             -- copy rule (down)
                             _prodsOoptions =
                                 ({-# LINE 33 "src-ag/ResolveLocals.ag" #-}
                                  _lhsIoptions
                                  {-# LINE 727 "src-ag/ResolveLocals.hs" #-}
                                  )
                             ( _prodsIcons,_prodsIerrors,_prodsIoutput) =
                                 prods_ _prodsOallnts _prodsOinh _prodsOmergeMap _prodsOnt _prodsOoptions _prodsOsyn 
                         in  ( _lhsOerrors,_lhsOnonts,_lhsOoutput))) )
-- Nonterminals ------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allnts               : [Identifier]
         mergeMap             : Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier])))
         options              : Options
      synthesized attributes:
         errors               : Seq Error
         nonts                : [(NontermIdent,[ConstructorIdent])]
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
sem_Nonterminals list  =
    (Prelude.foldr sem_Nonterminals_Cons sem_Nonterminals_Nil (Prelude.map sem_Nonterminal list) )
-- semantic domain
newtype T_Nonterminals  = T_Nonterminals (([Identifier]) ->
                                          (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier])))) ->
                                          Options ->
                                          ( (Seq Error),([(NontermIdent,[ConstructorIdent])]),Nonterminals ))
data Inh_Nonterminals  = Inh_Nonterminals {allnts_Inh_Nonterminals :: ([Identifier]),mergeMap_Inh_Nonterminals :: (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier])))),options_Inh_Nonterminals :: Options}
data Syn_Nonterminals  = Syn_Nonterminals {errors_Syn_Nonterminals :: (Seq Error),nonts_Syn_Nonterminals :: ([(NontermIdent,[ConstructorIdent])]),output_Syn_Nonterminals :: Nonterminals }
wrap_Nonterminals :: T_Nonterminals  ->
                     Inh_Nonterminals  ->
                     Syn_Nonterminals 
wrap_Nonterminals (T_Nonterminals sem ) (Inh_Nonterminals _lhsIallnts _lhsImergeMap _lhsIoptions )  =
    (let ( _lhsOerrors,_lhsOnonts,_lhsOoutput) = sem _lhsIallnts _lhsImergeMap _lhsIoptions 
     in  (Syn_Nonterminals _lhsOerrors _lhsOnonts _lhsOoutput ))
sem_Nonterminals_Cons :: T_Nonterminal  ->
                         T_Nonterminals  ->
                         T_Nonterminals 
sem_Nonterminals_Cons (T_Nonterminal hd_ ) (T_Nonterminals tl_ )  =
    (T_Nonterminals (\ _lhsIallnts
                       _lhsImergeMap
                       _lhsIoptions ->
                         (let _lhsOerrors :: (Seq Error)
                              _lhsOnonts :: ([(NontermIdent,[ConstructorIdent])])
                              _lhsOoutput :: Nonterminals 
                              _hdOallnts :: ([Identifier])
                              _hdOmergeMap :: (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier]))))
                              _hdOoptions :: Options
                              _tlOallnts :: ([Identifier])
                              _tlOmergeMap :: (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier]))))
                              _tlOoptions :: Options
                              _hdIerrors :: (Seq Error)
                              _hdInonts :: ([(NontermIdent,[ConstructorIdent])])
                              _hdIoutput :: Nonterminal 
                              _tlIerrors :: (Seq Error)
                              _tlInonts :: ([(NontermIdent,[ConstructorIdent])])
                              _tlIoutput :: Nonterminals 
                              -- use rule "src-ag/ResolveLocals.ag"(line 36, column 16)
                              _lhsOerrors =
                                  ({-# LINE 36 "src-ag/ResolveLocals.ag" #-}
                                   _hdIerrors Seq.>< _tlIerrors
                                   {-# LINE 797 "src-ag/ResolveLocals.hs" #-}
                                   )
                              -- use rule "src-ag/ResolveLocals.ag"(line 54, column 43)
                              _lhsOnonts =
                                  ({-# LINE 54 "src-ag/ResolveLocals.ag" #-}
                                   _hdInonts ++ _tlInonts
                                   {-# LINE 803 "src-ag/ResolveLocals.hs" #-}
                                   )
                              -- self rule
                              _output =
                                  ({-# LINE 39 "src-ag/ResolveLocals.ag" #-}
                                   (:) _hdIoutput _tlIoutput
                                   {-# LINE 809 "src-ag/ResolveLocals.hs" #-}
                                   )
                              -- self rule
                              _lhsOoutput =
                                  ({-# LINE 39 "src-ag/ResolveLocals.ag" #-}
                                   _output
                                   {-# LINE 815 "src-ag/ResolveLocals.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOallnts =
                                  ({-# LINE 49 "src-ag/ResolveLocals.ag" #-}
                                   _lhsIallnts
                                   {-# LINE 821 "src-ag/ResolveLocals.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOmergeMap =
                                  ({-# LINE 115 "src-ag/ResolveLocals.ag" #-}
                                   _lhsImergeMap
                                   {-# LINE 827 "src-ag/ResolveLocals.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOoptions =
                                  ({-# LINE 33 "src-ag/ResolveLocals.ag" #-}
                                   _lhsIoptions
                                   {-# LINE 833 "src-ag/ResolveLocals.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOallnts =
                                  ({-# LINE 49 "src-ag/ResolveLocals.ag" #-}
                                   _lhsIallnts
                                   {-# LINE 839 "src-ag/ResolveLocals.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOmergeMap =
                                  ({-# LINE 115 "src-ag/ResolveLocals.ag" #-}
                                   _lhsImergeMap
                                   {-# LINE 845 "src-ag/ResolveLocals.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOoptions =
                                  ({-# LINE 33 "src-ag/ResolveLocals.ag" #-}
                                   _lhsIoptions
                                   {-# LINE 851 "src-ag/ResolveLocals.hs" #-}
                                   )
                              ( _hdIerrors,_hdInonts,_hdIoutput) =
                                  hd_ _hdOallnts _hdOmergeMap _hdOoptions 
                              ( _tlIerrors,_tlInonts,_tlIoutput) =
                                  tl_ _tlOallnts _tlOmergeMap _tlOoptions 
                          in  ( _lhsOerrors,_lhsOnonts,_lhsOoutput))) )
sem_Nonterminals_Nil :: T_Nonterminals 
sem_Nonterminals_Nil  =
    (T_Nonterminals (\ _lhsIallnts
                       _lhsImergeMap
                       _lhsIoptions ->
                         (let _lhsOerrors :: (Seq Error)
                              _lhsOnonts :: ([(NontermIdent,[ConstructorIdent])])
                              _lhsOoutput :: Nonterminals 
                              -- use rule "src-ag/ResolveLocals.ag"(line 36, column 16)
                              _lhsOerrors =
                                  ({-# LINE 36 "src-ag/ResolveLocals.ag" #-}
                                   Seq.empty
                                   {-# LINE 870 "src-ag/ResolveLocals.hs" #-}
                                   )
                              -- use rule "src-ag/ResolveLocals.ag"(line 54, column 43)
                              _lhsOnonts =
                                  ({-# LINE 54 "src-ag/ResolveLocals.ag" #-}
                                   []
                                   {-# LINE 876 "src-ag/ResolveLocals.hs" #-}
                                   )
                              -- self rule
                              _output =
                                  ({-# LINE 39 "src-ag/ResolveLocals.ag" #-}
                                   []
                                   {-# LINE 882 "src-ag/ResolveLocals.hs" #-}
                                   )
                              -- self rule
                              _lhsOoutput =
                                  ({-# LINE 39 "src-ag/ResolveLocals.ag" #-}
                                   _output
                                   {-# LINE 888 "src-ag/ResolveLocals.hs" #-}
                                   )
                          in  ( _lhsOerrors,_lhsOnonts,_lhsOoutput))) )
-- Pattern -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         con                  : Identifier
         inh                  : Attributes
         nt                   : Identifier
         syn                  : Attributes
      synthesized attributes:
         copy                 : SELF 
         errors               : Seq Error
         instVars             : [Identifier]
         locVars              : [Identifier]
         output               : SELF 
   alternatives:
      alternative Alias:
         child field          : {Identifier}
         child attr           : {Identifier}
         child pat            : Pattern 
         child parts          : Patterns 
         visit 0:
            local copy        : _
            local output      : _
      alternative Constr:
         child name           : {ConstructorIdent}
         child pats           : Patterns 
         visit 0:
            local copy        : _
            local output      : _
      alternative Irrefutable:
         child pat            : Pattern 
         visit 0:
            local copy        : _
            local output      : _
      alternative Product:
         child pos            : {Pos}
         child pats           : Patterns 
         visit 0:
            local copy        : _
            local output      : _
      alternative Underscore:
         child pos            : {Pos}
         visit 0:
            local copy        : _
            local output      : _
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
newtype T_Pattern  = T_Pattern (Identifier ->
                                Attributes ->
                                Identifier ->
                                Attributes ->
                                ( Pattern ,(Seq Error),([Identifier]),([Identifier]),Pattern ))
data Inh_Pattern  = Inh_Pattern {con_Inh_Pattern :: Identifier,inh_Inh_Pattern :: Attributes,nt_Inh_Pattern :: Identifier,syn_Inh_Pattern :: Attributes}
data Syn_Pattern  = Syn_Pattern {copy_Syn_Pattern :: Pattern ,errors_Syn_Pattern :: (Seq Error),instVars_Syn_Pattern :: ([Identifier]),locVars_Syn_Pattern :: ([Identifier]),output_Syn_Pattern :: Pattern }
wrap_Pattern :: T_Pattern  ->
                Inh_Pattern  ->
                Syn_Pattern 
wrap_Pattern (T_Pattern sem ) (Inh_Pattern _lhsIcon _lhsIinh _lhsInt _lhsIsyn )  =
    (let ( _lhsOcopy,_lhsOerrors,_lhsOinstVars,_lhsOlocVars,_lhsOoutput) = sem _lhsIcon _lhsIinh _lhsInt _lhsIsyn 
     in  (Syn_Pattern _lhsOcopy _lhsOerrors _lhsOinstVars _lhsOlocVars _lhsOoutput ))
sem_Pattern_Alias :: Identifier ->
                     Identifier ->
                     T_Pattern  ->
                     T_Patterns  ->
                     T_Pattern 
sem_Pattern_Alias field_ attr_ (T_Pattern pat_ ) (T_Patterns parts_ )  =
    (T_Pattern (\ _lhsIcon
                  _lhsIinh
                  _lhsInt
                  _lhsIsyn ->
                    (let _lhsOlocVars :: ([Identifier])
                         _lhsOinstVars :: ([Identifier])
                         _lhsOerrors :: (Seq Error)
                         _lhsOcopy :: Pattern 
                         _lhsOoutput :: Pattern 
                         _patOcon :: Identifier
                         _patOinh :: Attributes
                         _patOnt :: Identifier
                         _patOsyn :: Attributes
                         _partsOcon :: Identifier
                         _partsOinh :: Attributes
                         _partsOnt :: Identifier
                         _partsOsyn :: Attributes
                         _patIcopy :: Pattern 
                         _patIerrors :: (Seq Error)
                         _patIinstVars :: ([Identifier])
                         _patIlocVars :: ([Identifier])
                         _patIoutput :: Pattern 
                         _partsIcopy :: Patterns 
                         _partsIerrors :: (Seq Error)
                         _partsIinstVars :: ([Identifier])
                         _partsIlocVars :: ([Identifier])
                         _partsIoutput :: Patterns 
                         -- "src-ag/ResolveLocals.ag"(line 88, column 14)
                         _lhsOlocVars =
                             ({-# LINE 88 "src-ag/ResolveLocals.ag" #-}
                              if field_ == _LOC
                                 then [attr_]
                                 else []
                              {-# LINE 1003 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- "src-ag/ResolveLocals.ag"(line 91, column 14)
                         _lhsOinstVars =
                             ({-# LINE 91 "src-ag/ResolveLocals.ag" #-}
                              if field_ == _INST
                                 then [attr_]
                                 else []
                              {-# LINE 1011 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- use rule "src-ag/ResolveLocals.ag"(line 36, column 16)
                         _lhsOerrors =
                             ({-# LINE 36 "src-ag/ResolveLocals.ag" #-}
                              _patIerrors Seq.>< _partsIerrors
                              {-# LINE 1017 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- self rule
                         _copy =
                             ({-# LINE 23 "src-ag/Patterns.ag" #-}
                              Alias field_ attr_ _patIcopy _partsIcopy
                              {-# LINE 1023 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- self rule
                         _output =
                             ({-# LINE 39 "src-ag/ResolveLocals.ag" #-}
                              Alias field_ attr_ _patIoutput _partsIoutput
                              {-# LINE 1029 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- self rule
                         _lhsOcopy =
                             ({-# LINE 23 "src-ag/Patterns.ag" #-}
                              _copy
                              {-# LINE 1035 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- self rule
                         _lhsOoutput =
                             ({-# LINE 39 "src-ag/ResolveLocals.ag" #-}
                              _output
                              {-# LINE 1041 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- copy rule (down)
                         _patOcon =
                             ({-# LINE 97 "src-ag/ResolveLocals.ag" #-}
                              _lhsIcon
                              {-# LINE 1047 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- copy rule (down)
                         _patOinh =
                             ({-# LINE 96 "src-ag/ResolveLocals.ag" #-}
                              _lhsIinh
                              {-# LINE 1053 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- copy rule (down)
                         _patOnt =
                             ({-# LINE 96 "src-ag/ResolveLocals.ag" #-}
                              _lhsInt
                              {-# LINE 1059 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- copy rule (down)
                         _patOsyn =
                             ({-# LINE 96 "src-ag/ResolveLocals.ag" #-}
                              _lhsIsyn
                              {-# LINE 1065 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- copy rule (down)
                         _partsOcon =
                             ({-# LINE 97 "src-ag/ResolveLocals.ag" #-}
                              _lhsIcon
                              {-# LINE 1071 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- copy rule (down)
                         _partsOinh =
                             ({-# LINE 96 "src-ag/ResolveLocals.ag" #-}
                              _lhsIinh
                              {-# LINE 1077 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- copy rule (down)
                         _partsOnt =
                             ({-# LINE 96 "src-ag/ResolveLocals.ag" #-}
                              _lhsInt
                              {-# LINE 1083 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- copy rule (down)
                         _partsOsyn =
                             ({-# LINE 96 "src-ag/ResolveLocals.ag" #-}
                              _lhsIsyn
                              {-# LINE 1089 "src-ag/ResolveLocals.hs" #-}
                              )
                         ( _patIcopy,_patIerrors,_patIinstVars,_patIlocVars,_patIoutput) =
                             pat_ _patOcon _patOinh _patOnt _patOsyn 
                         ( _partsIcopy,_partsIerrors,_partsIinstVars,_partsIlocVars,_partsIoutput) =
                             parts_ _partsOcon _partsOinh _partsOnt _partsOsyn 
                     in  ( _lhsOcopy,_lhsOerrors,_lhsOinstVars,_lhsOlocVars,_lhsOoutput))) )
sem_Pattern_Constr :: ConstructorIdent ->
                      T_Patterns  ->
                      T_Pattern 
sem_Pattern_Constr name_ (T_Patterns pats_ )  =
    (T_Pattern (\ _lhsIcon
                  _lhsIinh
                  _lhsInt
                  _lhsIsyn ->
                    (let _lhsOerrors :: (Seq Error)
                         _lhsOinstVars :: ([Identifier])
                         _lhsOlocVars :: ([Identifier])
                         _lhsOcopy :: Pattern 
                         _lhsOoutput :: Pattern 
                         _patsOcon :: Identifier
                         _patsOinh :: Attributes
                         _patsOnt :: Identifier
                         _patsOsyn :: Attributes
                         _patsIcopy :: Patterns 
                         _patsIerrors :: (Seq Error)
                         _patsIinstVars :: ([Identifier])
                         _patsIlocVars :: ([Identifier])
                         _patsIoutput :: Patterns 
                         -- use rule "src-ag/ResolveLocals.ag"(line 36, column 16)
                         _lhsOerrors =
                             ({-# LINE 36 "src-ag/ResolveLocals.ag" #-}
                              _patsIerrors
                              {-# LINE 1122 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- use rule "src-ag/ResolveLocals.ag"(line 85, column 86)
                         _lhsOinstVars =
                             ({-# LINE 85 "src-ag/ResolveLocals.ag" #-}
                              _patsIinstVars
                              {-# LINE 1128 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- use rule "src-ag/ResolveLocals.ag"(line 85, column 48)
                         _lhsOlocVars =
                             ({-# LINE 85 "src-ag/ResolveLocals.ag" #-}
                              _patsIlocVars
                              {-# LINE 1134 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- self rule
                         _copy =
                             ({-# LINE 23 "src-ag/Patterns.ag" #-}
                              Constr name_ _patsIcopy
                              {-# LINE 1140 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- self rule
                         _output =
                             ({-# LINE 39 "src-ag/ResolveLocals.ag" #-}
                              Constr name_ _patsIoutput
                              {-# LINE 1146 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- self rule
                         _lhsOcopy =
                             ({-# LINE 23 "src-ag/Patterns.ag" #-}
                              _copy
                              {-# LINE 1152 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- self rule
                         _lhsOoutput =
                             ({-# LINE 39 "src-ag/ResolveLocals.ag" #-}
                              _output
                              {-# LINE 1158 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- copy rule (down)
                         _patsOcon =
                             ({-# LINE 97 "src-ag/ResolveLocals.ag" #-}
                              _lhsIcon
                              {-# LINE 1164 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- copy rule (down)
                         _patsOinh =
                             ({-# LINE 96 "src-ag/ResolveLocals.ag" #-}
                              _lhsIinh
                              {-# LINE 1170 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- copy rule (down)
                         _patsOnt =
                             ({-# LINE 96 "src-ag/ResolveLocals.ag" #-}
                              _lhsInt
                              {-# LINE 1176 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- copy rule (down)
                         _patsOsyn =
                             ({-# LINE 96 "src-ag/ResolveLocals.ag" #-}
                              _lhsIsyn
                              {-# LINE 1182 "src-ag/ResolveLocals.hs" #-}
                              )
                         ( _patsIcopy,_patsIerrors,_patsIinstVars,_patsIlocVars,_patsIoutput) =
                             pats_ _patsOcon _patsOinh _patsOnt _patsOsyn 
                     in  ( _lhsOcopy,_lhsOerrors,_lhsOinstVars,_lhsOlocVars,_lhsOoutput))) )
sem_Pattern_Irrefutable :: T_Pattern  ->
                           T_Pattern 
sem_Pattern_Irrefutable (T_Pattern pat_ )  =
    (T_Pattern (\ _lhsIcon
                  _lhsIinh
                  _lhsInt
                  _lhsIsyn ->
                    (let _lhsOerrors :: (Seq Error)
                         _lhsOinstVars :: ([Identifier])
                         _lhsOlocVars :: ([Identifier])
                         _lhsOcopy :: Pattern 
                         _lhsOoutput :: Pattern 
                         _patOcon :: Identifier
                         _patOinh :: Attributes
                         _patOnt :: Identifier
                         _patOsyn :: Attributes
                         _patIcopy :: Pattern 
                         _patIerrors :: (Seq Error)
                         _patIinstVars :: ([Identifier])
                         _patIlocVars :: ([Identifier])
                         _patIoutput :: Pattern 
                         -- use rule "src-ag/ResolveLocals.ag"(line 36, column 16)
                         _lhsOerrors =
                             ({-# LINE 36 "src-ag/ResolveLocals.ag" #-}
                              _patIerrors
                              {-# LINE 1212 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- use rule "src-ag/ResolveLocals.ag"(line 85, column 86)
                         _lhsOinstVars =
                             ({-# LINE 85 "src-ag/ResolveLocals.ag" #-}
                              _patIinstVars
                              {-# LINE 1218 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- use rule "src-ag/ResolveLocals.ag"(line 85, column 48)
                         _lhsOlocVars =
                             ({-# LINE 85 "src-ag/ResolveLocals.ag" #-}
                              _patIlocVars
                              {-# LINE 1224 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- self rule
                         _copy =
                             ({-# LINE 23 "src-ag/Patterns.ag" #-}
                              Irrefutable _patIcopy
                              {-# LINE 1230 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- self rule
                         _output =
                             ({-# LINE 39 "src-ag/ResolveLocals.ag" #-}
                              Irrefutable _patIoutput
                              {-# LINE 1236 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- self rule
                         _lhsOcopy =
                             ({-# LINE 23 "src-ag/Patterns.ag" #-}
                              _copy
                              {-# LINE 1242 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- self rule
                         _lhsOoutput =
                             ({-# LINE 39 "src-ag/ResolveLocals.ag" #-}
                              _output
                              {-# LINE 1248 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- copy rule (down)
                         _patOcon =
                             ({-# LINE 97 "src-ag/ResolveLocals.ag" #-}
                              _lhsIcon
                              {-# LINE 1254 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- copy rule (down)
                         _patOinh =
                             ({-# LINE 96 "src-ag/ResolveLocals.ag" #-}
                              _lhsIinh
                              {-# LINE 1260 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- copy rule (down)
                         _patOnt =
                             ({-# LINE 96 "src-ag/ResolveLocals.ag" #-}
                              _lhsInt
                              {-# LINE 1266 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- copy rule (down)
                         _patOsyn =
                             ({-# LINE 96 "src-ag/ResolveLocals.ag" #-}
                              _lhsIsyn
                              {-# LINE 1272 "src-ag/ResolveLocals.hs" #-}
                              )
                         ( _patIcopy,_patIerrors,_patIinstVars,_patIlocVars,_patIoutput) =
                             pat_ _patOcon _patOinh _patOnt _patOsyn 
                     in  ( _lhsOcopy,_lhsOerrors,_lhsOinstVars,_lhsOlocVars,_lhsOoutput))) )
sem_Pattern_Product :: Pos ->
                       T_Patterns  ->
                       T_Pattern 
sem_Pattern_Product pos_ (T_Patterns pats_ )  =
    (T_Pattern (\ _lhsIcon
                  _lhsIinh
                  _lhsInt
                  _lhsIsyn ->
                    (let _lhsOerrors :: (Seq Error)
                         _lhsOinstVars :: ([Identifier])
                         _lhsOlocVars :: ([Identifier])
                         _lhsOcopy :: Pattern 
                         _lhsOoutput :: Pattern 
                         _patsOcon :: Identifier
                         _patsOinh :: Attributes
                         _patsOnt :: Identifier
                         _patsOsyn :: Attributes
                         _patsIcopy :: Patterns 
                         _patsIerrors :: (Seq Error)
                         _patsIinstVars :: ([Identifier])
                         _patsIlocVars :: ([Identifier])
                         _patsIoutput :: Patterns 
                         -- use rule "src-ag/ResolveLocals.ag"(line 36, column 16)
                         _lhsOerrors =
                             ({-# LINE 36 "src-ag/ResolveLocals.ag" #-}
                              _patsIerrors
                              {-# LINE 1303 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- use rule "src-ag/ResolveLocals.ag"(line 85, column 86)
                         _lhsOinstVars =
                             ({-# LINE 85 "src-ag/ResolveLocals.ag" #-}
                              _patsIinstVars
                              {-# LINE 1309 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- use rule "src-ag/ResolveLocals.ag"(line 85, column 48)
                         _lhsOlocVars =
                             ({-# LINE 85 "src-ag/ResolveLocals.ag" #-}
                              _patsIlocVars
                              {-# LINE 1315 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- self rule
                         _copy =
                             ({-# LINE 23 "src-ag/Patterns.ag" #-}
                              Product pos_ _patsIcopy
                              {-# LINE 1321 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- self rule
                         _output =
                             ({-# LINE 39 "src-ag/ResolveLocals.ag" #-}
                              Product pos_ _patsIoutput
                              {-# LINE 1327 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- self rule
                         _lhsOcopy =
                             ({-# LINE 23 "src-ag/Patterns.ag" #-}
                              _copy
                              {-# LINE 1333 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- self rule
                         _lhsOoutput =
                             ({-# LINE 39 "src-ag/ResolveLocals.ag" #-}
                              _output
                              {-# LINE 1339 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- copy rule (down)
                         _patsOcon =
                             ({-# LINE 97 "src-ag/ResolveLocals.ag" #-}
                              _lhsIcon
                              {-# LINE 1345 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- copy rule (down)
                         _patsOinh =
                             ({-# LINE 96 "src-ag/ResolveLocals.ag" #-}
                              _lhsIinh
                              {-# LINE 1351 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- copy rule (down)
                         _patsOnt =
                             ({-# LINE 96 "src-ag/ResolveLocals.ag" #-}
                              _lhsInt
                              {-# LINE 1357 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- copy rule (down)
                         _patsOsyn =
                             ({-# LINE 96 "src-ag/ResolveLocals.ag" #-}
                              _lhsIsyn
                              {-# LINE 1363 "src-ag/ResolveLocals.hs" #-}
                              )
                         ( _patsIcopy,_patsIerrors,_patsIinstVars,_patsIlocVars,_patsIoutput) =
                             pats_ _patsOcon _patsOinh _patsOnt _patsOsyn 
                     in  ( _lhsOcopy,_lhsOerrors,_lhsOinstVars,_lhsOlocVars,_lhsOoutput))) )
sem_Pattern_Underscore :: Pos ->
                          T_Pattern 
sem_Pattern_Underscore pos_  =
    (T_Pattern (\ _lhsIcon
                  _lhsIinh
                  _lhsInt
                  _lhsIsyn ->
                    (let _lhsOerrors :: (Seq Error)
                         _lhsOinstVars :: ([Identifier])
                         _lhsOlocVars :: ([Identifier])
                         _lhsOcopy :: Pattern 
                         _lhsOoutput :: Pattern 
                         -- use rule "src-ag/ResolveLocals.ag"(line 36, column 16)
                         _lhsOerrors =
                             ({-# LINE 36 "src-ag/ResolveLocals.ag" #-}
                              Seq.empty
                              {-# LINE 1384 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- use rule "src-ag/ResolveLocals.ag"(line 85, column 86)
                         _lhsOinstVars =
                             ({-# LINE 85 "src-ag/ResolveLocals.ag" #-}
                              []
                              {-# LINE 1390 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- use rule "src-ag/ResolveLocals.ag"(line 85, column 48)
                         _lhsOlocVars =
                             ({-# LINE 85 "src-ag/ResolveLocals.ag" #-}
                              []
                              {-# LINE 1396 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- self rule
                         _copy =
                             ({-# LINE 23 "src-ag/Patterns.ag" #-}
                              Underscore pos_
                              {-# LINE 1402 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- self rule
                         _output =
                             ({-# LINE 39 "src-ag/ResolveLocals.ag" #-}
                              Underscore pos_
                              {-# LINE 1408 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- self rule
                         _lhsOcopy =
                             ({-# LINE 23 "src-ag/Patterns.ag" #-}
                              _copy
                              {-# LINE 1414 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- self rule
                         _lhsOoutput =
                             ({-# LINE 39 "src-ag/ResolveLocals.ag" #-}
                              _output
                              {-# LINE 1420 "src-ag/ResolveLocals.hs" #-}
                              )
                     in  ( _lhsOcopy,_lhsOerrors,_lhsOinstVars,_lhsOlocVars,_lhsOoutput))) )
-- Patterns ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         con                  : Identifier
         inh                  : Attributes
         nt                   : Identifier
         syn                  : Attributes
      synthesized attributes:
         copy                 : SELF 
         errors               : Seq Error
         instVars             : [Identifier]
         locVars              : [Identifier]
         output               : SELF 
   alternatives:
      alternative Cons:
         child hd             : Pattern 
         child tl             : Patterns 
         visit 0:
            local copy        : _
            local output      : _
      alternative Nil:
         visit 0:
            local copy        : _
            local output      : _
-}
-- cata
sem_Patterns :: Patterns  ->
                T_Patterns 
sem_Patterns list  =
    (Prelude.foldr sem_Patterns_Cons sem_Patterns_Nil (Prelude.map sem_Pattern list) )
-- semantic domain
newtype T_Patterns  = T_Patterns (Identifier ->
                                  Attributes ->
                                  Identifier ->
                                  Attributes ->
                                  ( Patterns ,(Seq Error),([Identifier]),([Identifier]),Patterns ))
data Inh_Patterns  = Inh_Patterns {con_Inh_Patterns :: Identifier,inh_Inh_Patterns :: Attributes,nt_Inh_Patterns :: Identifier,syn_Inh_Patterns :: Attributes}
data Syn_Patterns  = Syn_Patterns {copy_Syn_Patterns :: Patterns ,errors_Syn_Patterns :: (Seq Error),instVars_Syn_Patterns :: ([Identifier]),locVars_Syn_Patterns :: ([Identifier]),output_Syn_Patterns :: Patterns }
wrap_Patterns :: T_Patterns  ->
                 Inh_Patterns  ->
                 Syn_Patterns 
wrap_Patterns (T_Patterns sem ) (Inh_Patterns _lhsIcon _lhsIinh _lhsInt _lhsIsyn )  =
    (let ( _lhsOcopy,_lhsOerrors,_lhsOinstVars,_lhsOlocVars,_lhsOoutput) = sem _lhsIcon _lhsIinh _lhsInt _lhsIsyn 
     in  (Syn_Patterns _lhsOcopy _lhsOerrors _lhsOinstVars _lhsOlocVars _lhsOoutput ))
sem_Patterns_Cons :: T_Pattern  ->
                     T_Patterns  ->
                     T_Patterns 
sem_Patterns_Cons (T_Pattern hd_ ) (T_Patterns tl_ )  =
    (T_Patterns (\ _lhsIcon
                   _lhsIinh
                   _lhsInt
                   _lhsIsyn ->
                     (let _lhsOerrors :: (Seq Error)
                          _lhsOinstVars :: ([Identifier])
                          _lhsOlocVars :: ([Identifier])
                          _lhsOcopy :: Patterns 
                          _lhsOoutput :: Patterns 
                          _hdOcon :: Identifier
                          _hdOinh :: Attributes
                          _hdOnt :: Identifier
                          _hdOsyn :: Attributes
                          _tlOcon :: Identifier
                          _tlOinh :: Attributes
                          _tlOnt :: Identifier
                          _tlOsyn :: Attributes
                          _hdIcopy :: Pattern 
                          _hdIerrors :: (Seq Error)
                          _hdIinstVars :: ([Identifier])
                          _hdIlocVars :: ([Identifier])
                          _hdIoutput :: Pattern 
                          _tlIcopy :: Patterns 
                          _tlIerrors :: (Seq Error)
                          _tlIinstVars :: ([Identifier])
                          _tlIlocVars :: ([Identifier])
                          _tlIoutput :: Patterns 
                          -- use rule "src-ag/ResolveLocals.ag"(line 36, column 16)
                          _lhsOerrors =
                              ({-# LINE 36 "src-ag/ResolveLocals.ag" #-}
                               _hdIerrors Seq.>< _tlIerrors
                               {-# LINE 1503 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- use rule "src-ag/ResolveLocals.ag"(line 85, column 86)
                          _lhsOinstVars =
                              ({-# LINE 85 "src-ag/ResolveLocals.ag" #-}
                               _hdIinstVars ++ _tlIinstVars
                               {-# LINE 1509 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- use rule "src-ag/ResolveLocals.ag"(line 85, column 48)
                          _lhsOlocVars =
                              ({-# LINE 85 "src-ag/ResolveLocals.ag" #-}
                               _hdIlocVars ++ _tlIlocVars
                               {-# LINE 1515 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- self rule
                          _copy =
                              ({-# LINE 23 "src-ag/Patterns.ag" #-}
                               (:) _hdIcopy _tlIcopy
                               {-# LINE 1521 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- self rule
                          _output =
                              ({-# LINE 39 "src-ag/ResolveLocals.ag" #-}
                               (:) _hdIoutput _tlIoutput
                               {-# LINE 1527 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- self rule
                          _lhsOcopy =
                              ({-# LINE 23 "src-ag/Patterns.ag" #-}
                               _copy
                               {-# LINE 1533 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- self rule
                          _lhsOoutput =
                              ({-# LINE 39 "src-ag/ResolveLocals.ag" #-}
                               _output
                               {-# LINE 1539 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOcon =
                              ({-# LINE 97 "src-ag/ResolveLocals.ag" #-}
                               _lhsIcon
                               {-# LINE 1545 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOinh =
                              ({-# LINE 96 "src-ag/ResolveLocals.ag" #-}
                               _lhsIinh
                               {-# LINE 1551 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOnt =
                              ({-# LINE 96 "src-ag/ResolveLocals.ag" #-}
                               _lhsInt
                               {-# LINE 1557 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOsyn =
                              ({-# LINE 96 "src-ag/ResolveLocals.ag" #-}
                               _lhsIsyn
                               {-# LINE 1563 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOcon =
                              ({-# LINE 97 "src-ag/ResolveLocals.ag" #-}
                               _lhsIcon
                               {-# LINE 1569 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOinh =
                              ({-# LINE 96 "src-ag/ResolveLocals.ag" #-}
                               _lhsIinh
                               {-# LINE 1575 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOnt =
                              ({-# LINE 96 "src-ag/ResolveLocals.ag" #-}
                               _lhsInt
                               {-# LINE 1581 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOsyn =
                              ({-# LINE 96 "src-ag/ResolveLocals.ag" #-}
                               _lhsIsyn
                               {-# LINE 1587 "src-ag/ResolveLocals.hs" #-}
                               )
                          ( _hdIcopy,_hdIerrors,_hdIinstVars,_hdIlocVars,_hdIoutput) =
                              hd_ _hdOcon _hdOinh _hdOnt _hdOsyn 
                          ( _tlIcopy,_tlIerrors,_tlIinstVars,_tlIlocVars,_tlIoutput) =
                              tl_ _tlOcon _tlOinh _tlOnt _tlOsyn 
                      in  ( _lhsOcopy,_lhsOerrors,_lhsOinstVars,_lhsOlocVars,_lhsOoutput))) )
sem_Patterns_Nil :: T_Patterns 
sem_Patterns_Nil  =
    (T_Patterns (\ _lhsIcon
                   _lhsIinh
                   _lhsInt
                   _lhsIsyn ->
                     (let _lhsOerrors :: (Seq Error)
                          _lhsOinstVars :: ([Identifier])
                          _lhsOlocVars :: ([Identifier])
                          _lhsOcopy :: Patterns 
                          _lhsOoutput :: Patterns 
                          -- use rule "src-ag/ResolveLocals.ag"(line 36, column 16)
                          _lhsOerrors =
                              ({-# LINE 36 "src-ag/ResolveLocals.ag" #-}
                               Seq.empty
                               {-# LINE 1609 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- use rule "src-ag/ResolveLocals.ag"(line 85, column 86)
                          _lhsOinstVars =
                              ({-# LINE 85 "src-ag/ResolveLocals.ag" #-}
                               []
                               {-# LINE 1615 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- use rule "src-ag/ResolveLocals.ag"(line 85, column 48)
                          _lhsOlocVars =
                              ({-# LINE 85 "src-ag/ResolveLocals.ag" #-}
                               []
                               {-# LINE 1621 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- self rule
                          _copy =
                              ({-# LINE 23 "src-ag/Patterns.ag" #-}
                               []
                               {-# LINE 1627 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- self rule
                          _output =
                              ({-# LINE 39 "src-ag/ResolveLocals.ag" #-}
                               []
                               {-# LINE 1633 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- self rule
                          _lhsOcopy =
                              ({-# LINE 23 "src-ag/Patterns.ag" #-}
                               _copy
                               {-# LINE 1639 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- self rule
                          _lhsOoutput =
                              ({-# LINE 39 "src-ag/ResolveLocals.ag" #-}
                               _output
                               {-# LINE 1645 "src-ag/ResolveLocals.hs" #-}
                               )
                      in  ( _lhsOcopy,_lhsOerrors,_lhsOinstVars,_lhsOlocVars,_lhsOoutput))) )
-- Production --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allnts               : [Identifier]
         inh                  : Attributes
         mergeMap             : Map ConstructorIdent (Map Identifier (Identifier,[Identifier]))
         nt                   : Identifier
         options              : Options
         syn                  : Attributes
      synthesized attributes:
         cons                 : [ConstructorIdent]
         errors               : Seq Error
         output               : SELF 
   alternatives:
      alternative Production:
         child con            : {ConstructorIdent}
         child children       : Children 
         child rules          : Rules 
         child typeSigs       : TypeSigs 
         visit 0:
            local allfields   : _
            local attrs       : _
            local inhnames    : _
            local synnames    : _
            local mergeMap    : _
            local output      : _
-}
-- cata
sem_Production :: Production  ->
                  T_Production 
sem_Production (Production _con _children _rules _typeSigs )  =
    (sem_Production_Production _con (sem_Children _children ) (sem_Rules _rules ) (sem_TypeSigs _typeSigs ) )
-- semantic domain
newtype T_Production  = T_Production (([Identifier]) ->
                                      Attributes ->
                                      (Map ConstructorIdent (Map Identifier (Identifier,[Identifier]))) ->
                                      Identifier ->
                                      Options ->
                                      Attributes ->
                                      ( ([ConstructorIdent]),(Seq Error),Production ))
data Inh_Production  = Inh_Production {allnts_Inh_Production :: ([Identifier]),inh_Inh_Production :: Attributes,mergeMap_Inh_Production :: (Map ConstructorIdent (Map Identifier (Identifier,[Identifier]))),nt_Inh_Production :: Identifier,options_Inh_Production :: Options,syn_Inh_Production :: Attributes}
data Syn_Production  = Syn_Production {cons_Syn_Production :: ([ConstructorIdent]),errors_Syn_Production :: (Seq Error),output_Syn_Production :: Production }
wrap_Production :: T_Production  ->
                   Inh_Production  ->
                   Syn_Production 
wrap_Production (T_Production sem ) (Inh_Production _lhsIallnts _lhsIinh _lhsImergeMap _lhsInt _lhsIoptions _lhsIsyn )  =
    (let ( _lhsOcons,_lhsOerrors,_lhsOoutput) = sem _lhsIallnts _lhsIinh _lhsImergeMap _lhsInt _lhsIoptions _lhsIsyn 
     in  (Syn_Production _lhsOcons _lhsOerrors _lhsOoutput ))
sem_Production_Production :: ConstructorIdent ->
                             T_Children  ->
                             T_Rules  ->
                             T_TypeSigs  ->
                             T_Production 
sem_Production_Production con_ (T_Children children_ ) (T_Rules rules_ ) (T_TypeSigs typeSigs_ )  =
    (T_Production (\ _lhsIallnts
                     _lhsIinh
                     _lhsImergeMap
                     _lhsInt
                     _lhsIoptions
                     _lhsIsyn ->
                       (let _lhsOcons :: ([ConstructorIdent])
                            _childrenOcon :: Identifier
                            _rulesOcon :: Identifier
                            _lhsOerrors :: (Seq Error)
                            _lhsOoutput :: Production 
                            _childrenOallfields :: ([(Identifier,Type,Maybe (Maybe Type))])
                            _childrenOallnts :: ([Identifier])
                            _childrenOattrs :: ([(Identifier,Identifier)])
                            _childrenOinh :: Attributes
                            _childrenOmergeMap :: (Map Identifier (Identifier,[Identifier]))
                            _childrenOnt :: Identifier
                            _childrenOsyn :: Attributes
                            _rulesOallfields :: ([(Identifier,Type,Maybe (Maybe Type))])
                            _rulesOallnts :: ([Identifier])
                            _rulesOattrs :: ([(Identifier,Identifier)])
                            _rulesOinh :: Attributes
                            _rulesOmergeMap :: (Map Identifier (Identifier,[Identifier]))
                            _rulesOnt :: Identifier
                            _rulesOoptions :: Options
                            _rulesOsyn :: Attributes
                            _childrenIattributes :: ([(Identifier,Attributes,Attributes)])
                            _childrenIfields :: ([(Identifier,Type,Maybe (Maybe Type))])
                            _childrenIoutput :: Children 
                            _rulesIerrors :: (Seq Error)
                            _rulesIinstVars :: ([Identifier])
                            _rulesIlocVars :: ([Identifier])
                            _rulesIoutput :: Rules 
                            _typeSigsIoutput :: TypeSigs 
                            -- "src-ag/ResolveLocals.ag"(line 59, column 18)
                            _lhsOcons =
                                ({-# LINE 59 "src-ag/ResolveLocals.ag" #-}
                                 [con_]
                                 {-# LINE 1741 "src-ag/ResolveLocals.hs" #-}
                                 )
                            -- "src-ag/ResolveLocals.ag"(line 66, column 16)
                            _allfields =
                                ({-# LINE 66 "src-ag/ResolveLocals.ag" #-}
                                 _childrenIfields
                                 {-# LINE 1747 "src-ag/ResolveLocals.hs" #-}
                                 )
                            -- "src-ag/ResolveLocals.ag"(line 66, column 16)
                            _attrs =
                                ({-# LINE 67 "src-ag/ResolveLocals.ag" #-}
                                 map ((,) _LOC)  _rulesIlocVars ++
                                 map ((,) _INST) _rulesIinstVars ++
                                 map ((,) _LHS)  _inhnames ++
                                 concat [map ((,) nm) (Map.keys as) | (nm,_,as) <- _childrenIattributes]
                                 {-# LINE 1756 "src-ag/ResolveLocals.hs" #-}
                                 )
                            -- "src-ag/ResolveLocals.ag"(line 66, column 16)
                            _inhnames =
                                ({-# LINE 71 "src-ag/ResolveLocals.ag" #-}
                                 Map.keys _lhsIinh
                                 {-# LINE 1762 "src-ag/ResolveLocals.hs" #-}
                                 )
                            -- "src-ag/ResolveLocals.ag"(line 66, column 16)
                            _synnames =
                                ({-# LINE 72 "src-ag/ResolveLocals.ag" #-}
                                 Map.keys _lhsIsyn
                                 {-# LINE 1768 "src-ag/ResolveLocals.hs" #-}
                                 )
                            -- "src-ag/ResolveLocals.ag"(line 100, column 16)
                            _childrenOcon =
                                ({-# LINE 100 "src-ag/ResolveLocals.ag" #-}
                                 con_
                                 {-# LINE 1774 "src-ag/ResolveLocals.hs" #-}
                                 )
                            -- "src-ag/ResolveLocals.ag"(line 102, column 16)
                            _rulesOcon =
                                ({-# LINE 102 "src-ag/ResolveLocals.ag" #-}
                                 con_
                                 {-# LINE 1780 "src-ag/ResolveLocals.hs" #-}
                                 )
                            -- "src-ag/ResolveLocals.ag"(line 121, column 32)
                            _mergeMap =
                                ({-# LINE 121 "src-ag/ResolveLocals.ag" #-}
                                 Map.findWithDefault Map.empty con_ _lhsImergeMap
                                 {-# LINE 1786 "src-ag/ResolveLocals.hs" #-}
                                 )
                            -- use rule "src-ag/ResolveLocals.ag"(line 36, column 16)
                            _lhsOerrors =
                                ({-# LINE 36 "src-ag/ResolveLocals.ag" #-}
                                 _rulesIerrors
                                 {-# LINE 1792 "src-ag/ResolveLocals.hs" #-}
                                 )
                            -- self rule
                            _output =
                                ({-# LINE 39 "src-ag/ResolveLocals.ag" #-}
                                 Production con_ _childrenIoutput _rulesIoutput _typeSigsIoutput
                                 {-# LINE 1798 "src-ag/ResolveLocals.hs" #-}
                                 )
                            -- self rule
                            _lhsOoutput =
                                ({-# LINE 39 "src-ag/ResolveLocals.ag" #-}
                                 _output
                                 {-# LINE 1804 "src-ag/ResolveLocals.hs" #-}
                                 )
                            -- copy rule (from local)
                            _childrenOallfields =
                                ({-# LINE 63 "src-ag/ResolveLocals.ag" #-}
                                 _allfields
                                 {-# LINE 1810 "src-ag/ResolveLocals.hs" #-}
                                 )
                            -- copy rule (down)
                            _childrenOallnts =
                                ({-# LINE 49 "src-ag/ResolveLocals.ag" #-}
                                 _lhsIallnts
                                 {-# LINE 1816 "src-ag/ResolveLocals.hs" #-}
                                 )
                            -- copy rule (from local)
                            _childrenOattrs =
                                ({-# LINE 63 "src-ag/ResolveLocals.ag" #-}
                                 _attrs
                                 {-# LINE 1822 "src-ag/ResolveLocals.hs" #-}
                                 )
                            -- copy rule (down)
                            _childrenOinh =
                                ({-# LINE 96 "src-ag/ResolveLocals.ag" #-}
                                 _lhsIinh
                                 {-# LINE 1828 "src-ag/ResolveLocals.hs" #-}
                                 )
                            -- copy rule (from local)
                            _childrenOmergeMap =
                                ({-# LINE 123 "src-ag/ResolveLocals.ag" #-}
                                 _mergeMap
                                 {-# LINE 1834 "src-ag/ResolveLocals.hs" #-}
                                 )
                            -- copy rule (down)
                            _childrenOnt =
                                ({-# LINE 96 "src-ag/ResolveLocals.ag" #-}
                                 _lhsInt
                                 {-# LINE 1840 "src-ag/ResolveLocals.hs" #-}
                                 )
                            -- copy rule (down)
                            _childrenOsyn =
                                ({-# LINE 96 "src-ag/ResolveLocals.ag" #-}
                                 _lhsIsyn
                                 {-# LINE 1846 "src-ag/ResolveLocals.hs" #-}
                                 )
                            -- copy rule (from local)
                            _rulesOallfields =
                                ({-# LINE 63 "src-ag/ResolveLocals.ag" #-}
                                 _allfields
                                 {-# LINE 1852 "src-ag/ResolveLocals.hs" #-}
                                 )
                            -- copy rule (down)
                            _rulesOallnts =
                                ({-# LINE 49 "src-ag/ResolveLocals.ag" #-}
                                 _lhsIallnts
                                 {-# LINE 1858 "src-ag/ResolveLocals.hs" #-}
                                 )
                            -- copy rule (from local)
                            _rulesOattrs =
                                ({-# LINE 63 "src-ag/ResolveLocals.ag" #-}
                                 _attrs
                                 {-# LINE 1864 "src-ag/ResolveLocals.hs" #-}
                                 )
                            -- copy rule (down)
                            _rulesOinh =
                                ({-# LINE 96 "src-ag/ResolveLocals.ag" #-}
                                 _lhsIinh
                                 {-# LINE 1870 "src-ag/ResolveLocals.hs" #-}
                                 )
                            -- copy rule (from local)
                            _rulesOmergeMap =
                                ({-# LINE 123 "src-ag/ResolveLocals.ag" #-}
                                 _mergeMap
                                 {-# LINE 1876 "src-ag/ResolveLocals.hs" #-}
                                 )
                            -- copy rule (down)
                            _rulesOnt =
                                ({-# LINE 96 "src-ag/ResolveLocals.ag" #-}
                                 _lhsInt
                                 {-# LINE 1882 "src-ag/ResolveLocals.hs" #-}
                                 )
                            -- copy rule (down)
                            _rulesOoptions =
                                ({-# LINE 33 "src-ag/ResolveLocals.ag" #-}
                                 _lhsIoptions
                                 {-# LINE 1888 "src-ag/ResolveLocals.hs" #-}
                                 )
                            -- copy rule (down)
                            _rulesOsyn =
                                ({-# LINE 96 "src-ag/ResolveLocals.ag" #-}
                                 _lhsIsyn
                                 {-# LINE 1894 "src-ag/ResolveLocals.hs" #-}
                                 )
                            ( _childrenIattributes,_childrenIfields,_childrenIoutput) =
                                children_ _childrenOallfields _childrenOallnts _childrenOattrs _childrenOcon _childrenOinh _childrenOmergeMap _childrenOnt _childrenOsyn 
                            ( _rulesIerrors,_rulesIinstVars,_rulesIlocVars,_rulesIoutput) =
                                rules_ _rulesOallfields _rulesOallnts _rulesOattrs _rulesOcon _rulesOinh _rulesOmergeMap _rulesOnt _rulesOoptions _rulesOsyn 
                            ( _typeSigsIoutput) =
                                typeSigs_ 
                        in  ( _lhsOcons,_lhsOerrors,_lhsOoutput))) )
-- Productions -------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allnts               : [Identifier]
         inh                  : Attributes
         mergeMap             : Map ConstructorIdent (Map Identifier (Identifier,[Identifier]))
         nt                   : Identifier
         options              : Options
         syn                  : Attributes
      synthesized attributes:
         cons                 : [ConstructorIdent]
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
sem_Productions list  =
    (Prelude.foldr sem_Productions_Cons sem_Productions_Nil (Prelude.map sem_Production list) )
-- semantic domain
newtype T_Productions  = T_Productions (([Identifier]) ->
                                        Attributes ->
                                        (Map ConstructorIdent (Map Identifier (Identifier,[Identifier]))) ->
                                        Identifier ->
                                        Options ->
                                        Attributes ->
                                        ( ([ConstructorIdent]),(Seq Error),Productions ))
data Inh_Productions  = Inh_Productions {allnts_Inh_Productions :: ([Identifier]),inh_Inh_Productions :: Attributes,mergeMap_Inh_Productions :: (Map ConstructorIdent (Map Identifier (Identifier,[Identifier]))),nt_Inh_Productions :: Identifier,options_Inh_Productions :: Options,syn_Inh_Productions :: Attributes}
data Syn_Productions  = Syn_Productions {cons_Syn_Productions :: ([ConstructorIdent]),errors_Syn_Productions :: (Seq Error),output_Syn_Productions :: Productions }
wrap_Productions :: T_Productions  ->
                    Inh_Productions  ->
                    Syn_Productions 
wrap_Productions (T_Productions sem ) (Inh_Productions _lhsIallnts _lhsIinh _lhsImergeMap _lhsInt _lhsIoptions _lhsIsyn )  =
    (let ( _lhsOcons,_lhsOerrors,_lhsOoutput) = sem _lhsIallnts _lhsIinh _lhsImergeMap _lhsInt _lhsIoptions _lhsIsyn 
     in  (Syn_Productions _lhsOcons _lhsOerrors _lhsOoutput ))
sem_Productions_Cons :: T_Production  ->
                        T_Productions  ->
                        T_Productions 
sem_Productions_Cons (T_Production hd_ ) (T_Productions tl_ )  =
    (T_Productions (\ _lhsIallnts
                      _lhsIinh
                      _lhsImergeMap
                      _lhsInt
                      _lhsIoptions
                      _lhsIsyn ->
                        (let _lhsOcons :: ([ConstructorIdent])
                             _lhsOerrors :: (Seq Error)
                             _lhsOoutput :: Productions 
                             _hdOallnts :: ([Identifier])
                             _hdOinh :: Attributes
                             _hdOmergeMap :: (Map ConstructorIdent (Map Identifier (Identifier,[Identifier])))
                             _hdOnt :: Identifier
                             _hdOoptions :: Options
                             _hdOsyn :: Attributes
                             _tlOallnts :: ([Identifier])
                             _tlOinh :: Attributes
                             _tlOmergeMap :: (Map ConstructorIdent (Map Identifier (Identifier,[Identifier])))
                             _tlOnt :: Identifier
                             _tlOoptions :: Options
                             _tlOsyn :: Attributes
                             _hdIcons :: ([ConstructorIdent])
                             _hdIerrors :: (Seq Error)
                             _hdIoutput :: Production 
                             _tlIcons :: ([ConstructorIdent])
                             _tlIerrors :: (Seq Error)
                             _tlIoutput :: Productions 
                             -- use rule "src-ag/ResolveLocals.ag"(line 57, column 40)
                             _lhsOcons =
                                 ({-# LINE 57 "src-ag/ResolveLocals.ag" #-}
                                  _hdIcons ++ _tlIcons
                                  {-# LINE 1983 "src-ag/ResolveLocals.hs" #-}
                                  )
                             -- use rule "src-ag/ResolveLocals.ag"(line 36, column 16)
                             _lhsOerrors =
                                 ({-# LINE 36 "src-ag/ResolveLocals.ag" #-}
                                  _hdIerrors Seq.>< _tlIerrors
                                  {-# LINE 1989 "src-ag/ResolveLocals.hs" #-}
                                  )
                             -- self rule
                             _output =
                                 ({-# LINE 39 "src-ag/ResolveLocals.ag" #-}
                                  (:) _hdIoutput _tlIoutput
                                  {-# LINE 1995 "src-ag/ResolveLocals.hs" #-}
                                  )
                             -- self rule
                             _lhsOoutput =
                                 ({-# LINE 39 "src-ag/ResolveLocals.ag" #-}
                                  _output
                                  {-# LINE 2001 "src-ag/ResolveLocals.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOallnts =
                                 ({-# LINE 49 "src-ag/ResolveLocals.ag" #-}
                                  _lhsIallnts
                                  {-# LINE 2007 "src-ag/ResolveLocals.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOinh =
                                 ({-# LINE 96 "src-ag/ResolveLocals.ag" #-}
                                  _lhsIinh
                                  {-# LINE 2013 "src-ag/ResolveLocals.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOmergeMap =
                                 ({-# LINE 118 "src-ag/ResolveLocals.ag" #-}
                                  _lhsImergeMap
                                  {-# LINE 2019 "src-ag/ResolveLocals.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOnt =
                                 ({-# LINE 96 "src-ag/ResolveLocals.ag" #-}
                                  _lhsInt
                                  {-# LINE 2025 "src-ag/ResolveLocals.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOoptions =
                                 ({-# LINE 33 "src-ag/ResolveLocals.ag" #-}
                                  _lhsIoptions
                                  {-# LINE 2031 "src-ag/ResolveLocals.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOsyn =
                                 ({-# LINE 96 "src-ag/ResolveLocals.ag" #-}
                                  _lhsIsyn
                                  {-# LINE 2037 "src-ag/ResolveLocals.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOallnts =
                                 ({-# LINE 49 "src-ag/ResolveLocals.ag" #-}
                                  _lhsIallnts
                                  {-# LINE 2043 "src-ag/ResolveLocals.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOinh =
                                 ({-# LINE 96 "src-ag/ResolveLocals.ag" #-}
                                  _lhsIinh
                                  {-# LINE 2049 "src-ag/ResolveLocals.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOmergeMap =
                                 ({-# LINE 118 "src-ag/ResolveLocals.ag" #-}
                                  _lhsImergeMap
                                  {-# LINE 2055 "src-ag/ResolveLocals.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOnt =
                                 ({-# LINE 96 "src-ag/ResolveLocals.ag" #-}
                                  _lhsInt
                                  {-# LINE 2061 "src-ag/ResolveLocals.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOoptions =
                                 ({-# LINE 33 "src-ag/ResolveLocals.ag" #-}
                                  _lhsIoptions
                                  {-# LINE 2067 "src-ag/ResolveLocals.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOsyn =
                                 ({-# LINE 96 "src-ag/ResolveLocals.ag" #-}
                                  _lhsIsyn
                                  {-# LINE 2073 "src-ag/ResolveLocals.hs" #-}
                                  )
                             ( _hdIcons,_hdIerrors,_hdIoutput) =
                                 hd_ _hdOallnts _hdOinh _hdOmergeMap _hdOnt _hdOoptions _hdOsyn 
                             ( _tlIcons,_tlIerrors,_tlIoutput) =
                                 tl_ _tlOallnts _tlOinh _tlOmergeMap _tlOnt _tlOoptions _tlOsyn 
                         in  ( _lhsOcons,_lhsOerrors,_lhsOoutput))) )
sem_Productions_Nil :: T_Productions 
sem_Productions_Nil  =
    (T_Productions (\ _lhsIallnts
                      _lhsIinh
                      _lhsImergeMap
                      _lhsInt
                      _lhsIoptions
                      _lhsIsyn ->
                        (let _lhsOcons :: ([ConstructorIdent])
                             _lhsOerrors :: (Seq Error)
                             _lhsOoutput :: Productions 
                             -- use rule "src-ag/ResolveLocals.ag"(line 57, column 40)
                             _lhsOcons =
                                 ({-# LINE 57 "src-ag/ResolveLocals.ag" #-}
                                  []
                                  {-# LINE 2095 "src-ag/ResolveLocals.hs" #-}
                                  )
                             -- use rule "src-ag/ResolveLocals.ag"(line 36, column 16)
                             _lhsOerrors =
                                 ({-# LINE 36 "src-ag/ResolveLocals.ag" #-}
                                  Seq.empty
                                  {-# LINE 2101 "src-ag/ResolveLocals.hs" #-}
                                  )
                             -- self rule
                             _output =
                                 ({-# LINE 39 "src-ag/ResolveLocals.ag" #-}
                                  []
                                  {-# LINE 2107 "src-ag/ResolveLocals.hs" #-}
                                  )
                             -- self rule
                             _lhsOoutput =
                                 ({-# LINE 39 "src-ag/ResolveLocals.ag" #-}
                                  _output
                                  {-# LINE 2113 "src-ag/ResolveLocals.hs" #-}
                                  )
                         in  ( _lhsOcons,_lhsOerrors,_lhsOoutput))) )
-- Rule --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allfields            : [(Identifier,Type,Maybe (Maybe Type))]
         allnts               : [Identifier]
         attrs                : [(Identifier,Identifier)]
         con                  : Identifier
         inh                  : Attributes
         mergeMap             : Map Identifier (Identifier,[Identifier])
         nt                   : Identifier
         options              : Options
         syn                  : Attributes
      synthesized attributes:
         errors               : Seq Error
         instVars             : [Identifier]
         locVars              : [Identifier]
         output               : SELF 
   alternatives:
      alternative Rule:
         child mbName         : {Maybe Identifier}
         child pattern        : Pattern 
         child rhs            : Expression 
         child owrt           : {Bool}
         child origin         : {String}
         child explicit       : {Bool}
         visit 0:
            local output      : _
-}
-- cata
sem_Rule :: Rule  ->
            T_Rule 
sem_Rule (Rule _mbName _pattern _rhs _owrt _origin _explicit )  =
    (sem_Rule_Rule _mbName (sem_Pattern _pattern ) (sem_Expression _rhs ) _owrt _origin _explicit )
-- semantic domain
newtype T_Rule  = T_Rule (([(Identifier,Type,Maybe (Maybe Type))]) ->
                          ([Identifier]) ->
                          ([(Identifier,Identifier)]) ->
                          Identifier ->
                          Attributes ->
                          (Map Identifier (Identifier,[Identifier])) ->
                          Identifier ->
                          Options ->
                          Attributes ->
                          ( (Seq Error),([Identifier]),([Identifier]),Rule ))
data Inh_Rule  = Inh_Rule {allfields_Inh_Rule :: ([(Identifier,Type,Maybe (Maybe Type))]),allnts_Inh_Rule :: ([Identifier]),attrs_Inh_Rule :: ([(Identifier,Identifier)]),con_Inh_Rule :: Identifier,inh_Inh_Rule :: Attributes,mergeMap_Inh_Rule :: (Map Identifier (Identifier,[Identifier])),nt_Inh_Rule :: Identifier,options_Inh_Rule :: Options,syn_Inh_Rule :: Attributes}
data Syn_Rule  = Syn_Rule {errors_Syn_Rule :: (Seq Error),instVars_Syn_Rule :: ([Identifier]),locVars_Syn_Rule :: ([Identifier]),output_Syn_Rule :: Rule }
wrap_Rule :: T_Rule  ->
             Inh_Rule  ->
             Syn_Rule 
wrap_Rule (T_Rule sem ) (Inh_Rule _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsIinh _lhsImergeMap _lhsInt _lhsIoptions _lhsIsyn )  =
    (let ( _lhsOerrors,_lhsOinstVars,_lhsOlocVars,_lhsOoutput) = sem _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsIinh _lhsImergeMap _lhsInt _lhsIoptions _lhsIsyn 
     in  (Syn_Rule _lhsOerrors _lhsOinstVars _lhsOlocVars _lhsOoutput ))
sem_Rule_Rule :: (Maybe Identifier) ->
                 T_Pattern  ->
                 T_Expression  ->
                 Bool ->
                 String ->
                 Bool ->
                 T_Rule 
sem_Rule_Rule mbName_ (T_Pattern pattern_ ) (T_Expression rhs_ ) owrt_ origin_ explicit_  =
    (T_Rule (\ _lhsIallfields
               _lhsIallnts
               _lhsIattrs
               _lhsIcon
               _lhsIinh
               _lhsImergeMap
               _lhsInt
               _lhsIoptions
               _lhsIsyn ->
                 (let _lhsOerrors :: (Seq Error)
                      _lhsOinstVars :: ([Identifier])
                      _lhsOlocVars :: ([Identifier])
                      _lhsOoutput :: Rule 
                      _patternOcon :: Identifier
                      _patternOinh :: Attributes
                      _patternOnt :: Identifier
                      _patternOsyn :: Attributes
                      _rhsOallfields :: ([(Identifier,Type,Maybe (Maybe Type))])
                      _rhsOallnts :: ([Identifier])
                      _rhsOattrs :: ([(Identifier,Identifier)])
                      _rhsOcon :: Identifier
                      _rhsOmergeMap :: (Map Identifier (Identifier,[Identifier]))
                      _rhsOnt :: Identifier
                      _rhsOoptions :: Options
                      _patternIcopy :: Pattern 
                      _patternIerrors :: (Seq Error)
                      _patternIinstVars :: ([Identifier])
                      _patternIlocVars :: ([Identifier])
                      _patternIoutput :: Pattern 
                      _rhsIerrors :: (Seq Error)
                      _rhsIoutput :: Expression 
                      -- use rule "src-ag/ResolveLocals.ag"(line 36, column 16)
                      _lhsOerrors =
                          ({-# LINE 36 "src-ag/ResolveLocals.ag" #-}
                           _patternIerrors Seq.>< _rhsIerrors
                           {-# LINE 2212 "src-ag/ResolveLocals.hs" #-}
                           )
                      -- use rule "src-ag/ResolveLocals.ag"(line 85, column 86)
                      _lhsOinstVars =
                          ({-# LINE 85 "src-ag/ResolveLocals.ag" #-}
                           _patternIinstVars
                           {-# LINE 2218 "src-ag/ResolveLocals.hs" #-}
                           )
                      -- use rule "src-ag/ResolveLocals.ag"(line 85, column 48)
                      _lhsOlocVars =
                          ({-# LINE 85 "src-ag/ResolveLocals.ag" #-}
                           _patternIlocVars
                           {-# LINE 2224 "src-ag/ResolveLocals.hs" #-}
                           )
                      -- self rule
                      _output =
                          ({-# LINE 39 "src-ag/ResolveLocals.ag" #-}
                           Rule mbName_ _patternIoutput _rhsIoutput owrt_ origin_ explicit_
                           {-# LINE 2230 "src-ag/ResolveLocals.hs" #-}
                           )
                      -- self rule
                      _lhsOoutput =
                          ({-# LINE 39 "src-ag/ResolveLocals.ag" #-}
                           _output
                           {-# LINE 2236 "src-ag/ResolveLocals.hs" #-}
                           )
                      -- copy rule (down)
                      _patternOcon =
                          ({-# LINE 97 "src-ag/ResolveLocals.ag" #-}
                           _lhsIcon
                           {-# LINE 2242 "src-ag/ResolveLocals.hs" #-}
                           )
                      -- copy rule (down)
                      _patternOinh =
                          ({-# LINE 96 "src-ag/ResolveLocals.ag" #-}
                           _lhsIinh
                           {-# LINE 2248 "src-ag/ResolveLocals.hs" #-}
                           )
                      -- copy rule (down)
                      _patternOnt =
                          ({-# LINE 96 "src-ag/ResolveLocals.ag" #-}
                           _lhsInt
                           {-# LINE 2254 "src-ag/ResolveLocals.hs" #-}
                           )
                      -- copy rule (down)
                      _patternOsyn =
                          ({-# LINE 96 "src-ag/ResolveLocals.ag" #-}
                           _lhsIsyn
                           {-# LINE 2260 "src-ag/ResolveLocals.hs" #-}
                           )
                      -- copy rule (down)
                      _rhsOallfields =
                          ({-# LINE 131 "src-ag/ResolveLocals.ag" #-}
                           _lhsIallfields
                           {-# LINE 2266 "src-ag/ResolveLocals.hs" #-}
                           )
                      -- copy rule (down)
                      _rhsOallnts =
                          ({-# LINE 132 "src-ag/ResolveLocals.ag" #-}
                           _lhsIallnts
                           {-# LINE 2272 "src-ag/ResolveLocals.hs" #-}
                           )
                      -- copy rule (down)
                      _rhsOattrs =
                          ({-# LINE 133 "src-ag/ResolveLocals.ag" #-}
                           _lhsIattrs
                           {-# LINE 2278 "src-ag/ResolveLocals.hs" #-}
                           )
                      -- copy rule (down)
                      _rhsOcon =
                          ({-# LINE 130 "src-ag/ResolveLocals.ag" #-}
                           _lhsIcon
                           {-# LINE 2284 "src-ag/ResolveLocals.hs" #-}
                           )
                      -- copy rule (down)
                      _rhsOmergeMap =
                          ({-# LINE 123 "src-ag/ResolveLocals.ag" #-}
                           _lhsImergeMap
                           {-# LINE 2290 "src-ag/ResolveLocals.hs" #-}
                           )
                      -- copy rule (down)
                      _rhsOnt =
                          ({-# LINE 130 "src-ag/ResolveLocals.ag" #-}
                           _lhsInt
                           {-# LINE 2296 "src-ag/ResolveLocals.hs" #-}
                           )
                      -- copy rule (down)
                      _rhsOoptions =
                          ({-# LINE 33 "src-ag/ResolveLocals.ag" #-}
                           _lhsIoptions
                           {-# LINE 2302 "src-ag/ResolveLocals.hs" #-}
                           )
                      ( _patternIcopy,_patternIerrors,_patternIinstVars,_patternIlocVars,_patternIoutput) =
                          pattern_ _patternOcon _patternOinh _patternOnt _patternOsyn 
                      ( _rhsIerrors,_rhsIoutput) =
                          rhs_ _rhsOallfields _rhsOallnts _rhsOattrs _rhsOcon _rhsOmergeMap _rhsOnt _rhsOoptions 
                  in  ( _lhsOerrors,_lhsOinstVars,_lhsOlocVars,_lhsOoutput))) )
-- Rules -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allfields            : [(Identifier,Type,Maybe (Maybe Type))]
         allnts               : [Identifier]
         attrs                : [(Identifier,Identifier)]
         con                  : Identifier
         inh                  : Attributes
         mergeMap             : Map Identifier (Identifier,[Identifier])
         nt                   : Identifier
         options              : Options
         syn                  : Attributes
      synthesized attributes:
         errors               : Seq Error
         instVars             : [Identifier]
         locVars              : [Identifier]
         output               : SELF 
   alternatives:
      alternative Cons:
         child hd             : Rule 
         child tl             : Rules 
         visit 0:
            local output      : _
      alternative Nil:
         visit 0:
            local output      : _
-}
-- cata
sem_Rules :: Rules  ->
             T_Rules 
sem_Rules list  =
    (Prelude.foldr sem_Rules_Cons sem_Rules_Nil (Prelude.map sem_Rule list) )
-- semantic domain
newtype T_Rules  = T_Rules (([(Identifier,Type,Maybe (Maybe Type))]) ->
                            ([Identifier]) ->
                            ([(Identifier,Identifier)]) ->
                            Identifier ->
                            Attributes ->
                            (Map Identifier (Identifier,[Identifier])) ->
                            Identifier ->
                            Options ->
                            Attributes ->
                            ( (Seq Error),([Identifier]),([Identifier]),Rules ))
data Inh_Rules  = Inh_Rules {allfields_Inh_Rules :: ([(Identifier,Type,Maybe (Maybe Type))]),allnts_Inh_Rules :: ([Identifier]),attrs_Inh_Rules :: ([(Identifier,Identifier)]),con_Inh_Rules :: Identifier,inh_Inh_Rules :: Attributes,mergeMap_Inh_Rules :: (Map Identifier (Identifier,[Identifier])),nt_Inh_Rules :: Identifier,options_Inh_Rules :: Options,syn_Inh_Rules :: Attributes}
data Syn_Rules  = Syn_Rules {errors_Syn_Rules :: (Seq Error),instVars_Syn_Rules :: ([Identifier]),locVars_Syn_Rules :: ([Identifier]),output_Syn_Rules :: Rules }
wrap_Rules :: T_Rules  ->
              Inh_Rules  ->
              Syn_Rules 
wrap_Rules (T_Rules sem ) (Inh_Rules _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsIinh _lhsImergeMap _lhsInt _lhsIoptions _lhsIsyn )  =
    (let ( _lhsOerrors,_lhsOinstVars,_lhsOlocVars,_lhsOoutput) = sem _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsIinh _lhsImergeMap _lhsInt _lhsIoptions _lhsIsyn 
     in  (Syn_Rules _lhsOerrors _lhsOinstVars _lhsOlocVars _lhsOoutput ))
sem_Rules_Cons :: T_Rule  ->
                  T_Rules  ->
                  T_Rules 
sem_Rules_Cons (T_Rule hd_ ) (T_Rules tl_ )  =
    (T_Rules (\ _lhsIallfields
                _lhsIallnts
                _lhsIattrs
                _lhsIcon
                _lhsIinh
                _lhsImergeMap
                _lhsInt
                _lhsIoptions
                _lhsIsyn ->
                  (let _lhsOerrors :: (Seq Error)
                       _lhsOinstVars :: ([Identifier])
                       _lhsOlocVars :: ([Identifier])
                       _lhsOoutput :: Rules 
                       _hdOallfields :: ([(Identifier,Type,Maybe (Maybe Type))])
                       _hdOallnts :: ([Identifier])
                       _hdOattrs :: ([(Identifier,Identifier)])
                       _hdOcon :: Identifier
                       _hdOinh :: Attributes
                       _hdOmergeMap :: (Map Identifier (Identifier,[Identifier]))
                       _hdOnt :: Identifier
                       _hdOoptions :: Options
                       _hdOsyn :: Attributes
                       _tlOallfields :: ([(Identifier,Type,Maybe (Maybe Type))])
                       _tlOallnts :: ([Identifier])
                       _tlOattrs :: ([(Identifier,Identifier)])
                       _tlOcon :: Identifier
                       _tlOinh :: Attributes
                       _tlOmergeMap :: (Map Identifier (Identifier,[Identifier]))
                       _tlOnt :: Identifier
                       _tlOoptions :: Options
                       _tlOsyn :: Attributes
                       _hdIerrors :: (Seq Error)
                       _hdIinstVars :: ([Identifier])
                       _hdIlocVars :: ([Identifier])
                       _hdIoutput :: Rule 
                       _tlIerrors :: (Seq Error)
                       _tlIinstVars :: ([Identifier])
                       _tlIlocVars :: ([Identifier])
                       _tlIoutput :: Rules 
                       -- use rule "src-ag/ResolveLocals.ag"(line 36, column 16)
                       _lhsOerrors =
                           ({-# LINE 36 "src-ag/ResolveLocals.ag" #-}
                            _hdIerrors Seq.>< _tlIerrors
                            {-# LINE 2408 "src-ag/ResolveLocals.hs" #-}
                            )
                       -- use rule "src-ag/ResolveLocals.ag"(line 85, column 86)
                       _lhsOinstVars =
                           ({-# LINE 85 "src-ag/ResolveLocals.ag" #-}
                            _hdIinstVars ++ _tlIinstVars
                            {-# LINE 2414 "src-ag/ResolveLocals.hs" #-}
                            )
                       -- use rule "src-ag/ResolveLocals.ag"(line 85, column 48)
                       _lhsOlocVars =
                           ({-# LINE 85 "src-ag/ResolveLocals.ag" #-}
                            _hdIlocVars ++ _tlIlocVars
                            {-# LINE 2420 "src-ag/ResolveLocals.hs" #-}
                            )
                       -- self rule
                       _output =
                           ({-# LINE 39 "src-ag/ResolveLocals.ag" #-}
                            (:) _hdIoutput _tlIoutput
                            {-# LINE 2426 "src-ag/ResolveLocals.hs" #-}
                            )
                       -- self rule
                       _lhsOoutput =
                           ({-# LINE 39 "src-ag/ResolveLocals.ag" #-}
                            _output
                            {-# LINE 2432 "src-ag/ResolveLocals.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOallfields =
                           ({-# LINE 63 "src-ag/ResolveLocals.ag" #-}
                            _lhsIallfields
                            {-# LINE 2438 "src-ag/ResolveLocals.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOallnts =
                           ({-# LINE 49 "src-ag/ResolveLocals.ag" #-}
                            _lhsIallnts
                            {-# LINE 2444 "src-ag/ResolveLocals.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOattrs =
                           ({-# LINE 63 "src-ag/ResolveLocals.ag" #-}
                            _lhsIattrs
                            {-# LINE 2450 "src-ag/ResolveLocals.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOcon =
                           ({-# LINE 97 "src-ag/ResolveLocals.ag" #-}
                            _lhsIcon
                            {-# LINE 2456 "src-ag/ResolveLocals.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOinh =
                           ({-# LINE 96 "src-ag/ResolveLocals.ag" #-}
                            _lhsIinh
                            {-# LINE 2462 "src-ag/ResolveLocals.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOmergeMap =
                           ({-# LINE 123 "src-ag/ResolveLocals.ag" #-}
                            _lhsImergeMap
                            {-# LINE 2468 "src-ag/ResolveLocals.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOnt =
                           ({-# LINE 96 "src-ag/ResolveLocals.ag" #-}
                            _lhsInt
                            {-# LINE 2474 "src-ag/ResolveLocals.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOoptions =
                           ({-# LINE 33 "src-ag/ResolveLocals.ag" #-}
                            _lhsIoptions
                            {-# LINE 2480 "src-ag/ResolveLocals.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOsyn =
                           ({-# LINE 96 "src-ag/ResolveLocals.ag" #-}
                            _lhsIsyn
                            {-# LINE 2486 "src-ag/ResolveLocals.hs" #-}
                            )
                       -- copy rule (down)
                       _tlOallfields =
                           ({-# LINE 63 "src-ag/ResolveLocals.ag" #-}
                            _lhsIallfields
                            {-# LINE 2492 "src-ag/ResolveLocals.hs" #-}
                            )
                       -- copy rule (down)
                       _tlOallnts =
                           ({-# LINE 49 "src-ag/ResolveLocals.ag" #-}
                            _lhsIallnts
                            {-# LINE 2498 "src-ag/ResolveLocals.hs" #-}
                            )
                       -- copy rule (down)
                       _tlOattrs =
                           ({-# LINE 63 "src-ag/ResolveLocals.ag" #-}
                            _lhsIattrs
                            {-# LINE 2504 "src-ag/ResolveLocals.hs" #-}
                            )
                       -- copy rule (down)
                       _tlOcon =
                           ({-# LINE 97 "src-ag/ResolveLocals.ag" #-}
                            _lhsIcon
                            {-# LINE 2510 "src-ag/ResolveLocals.hs" #-}
                            )
                       -- copy rule (down)
                       _tlOinh =
                           ({-# LINE 96 "src-ag/ResolveLocals.ag" #-}
                            _lhsIinh
                            {-# LINE 2516 "src-ag/ResolveLocals.hs" #-}
                            )
                       -- copy rule (down)
                       _tlOmergeMap =
                           ({-# LINE 123 "src-ag/ResolveLocals.ag" #-}
                            _lhsImergeMap
                            {-# LINE 2522 "src-ag/ResolveLocals.hs" #-}
                            )
                       -- copy rule (down)
                       _tlOnt =
                           ({-# LINE 96 "src-ag/ResolveLocals.ag" #-}
                            _lhsInt
                            {-# LINE 2528 "src-ag/ResolveLocals.hs" #-}
                            )
                       -- copy rule (down)
                       _tlOoptions =
                           ({-# LINE 33 "src-ag/ResolveLocals.ag" #-}
                            _lhsIoptions
                            {-# LINE 2534 "src-ag/ResolveLocals.hs" #-}
                            )
                       -- copy rule (down)
                       _tlOsyn =
                           ({-# LINE 96 "src-ag/ResolveLocals.ag" #-}
                            _lhsIsyn
                            {-# LINE 2540 "src-ag/ResolveLocals.hs" #-}
                            )
                       ( _hdIerrors,_hdIinstVars,_hdIlocVars,_hdIoutput) =
                           hd_ _hdOallfields _hdOallnts _hdOattrs _hdOcon _hdOinh _hdOmergeMap _hdOnt _hdOoptions _hdOsyn 
                       ( _tlIerrors,_tlIinstVars,_tlIlocVars,_tlIoutput) =
                           tl_ _tlOallfields _tlOallnts _tlOattrs _tlOcon _tlOinh _tlOmergeMap _tlOnt _tlOoptions _tlOsyn 
                   in  ( _lhsOerrors,_lhsOinstVars,_lhsOlocVars,_lhsOoutput))) )
sem_Rules_Nil :: T_Rules 
sem_Rules_Nil  =
    (T_Rules (\ _lhsIallfields
                _lhsIallnts
                _lhsIattrs
                _lhsIcon
                _lhsIinh
                _lhsImergeMap
                _lhsInt
                _lhsIoptions
                _lhsIsyn ->
                  (let _lhsOerrors :: (Seq Error)
                       _lhsOinstVars :: ([Identifier])
                       _lhsOlocVars :: ([Identifier])
                       _lhsOoutput :: Rules 
                       -- use rule "src-ag/ResolveLocals.ag"(line 36, column 16)
                       _lhsOerrors =
                           ({-# LINE 36 "src-ag/ResolveLocals.ag" #-}
                            Seq.empty
                            {-# LINE 2566 "src-ag/ResolveLocals.hs" #-}
                            )
                       -- use rule "src-ag/ResolveLocals.ag"(line 85, column 86)
                       _lhsOinstVars =
                           ({-# LINE 85 "src-ag/ResolveLocals.ag" #-}
                            []
                            {-# LINE 2572 "src-ag/ResolveLocals.hs" #-}
                            )
                       -- use rule "src-ag/ResolveLocals.ag"(line 85, column 48)
                       _lhsOlocVars =
                           ({-# LINE 85 "src-ag/ResolveLocals.ag" #-}
                            []
                            {-# LINE 2578 "src-ag/ResolveLocals.hs" #-}
                            )
                       -- self rule
                       _output =
                           ({-# LINE 39 "src-ag/ResolveLocals.ag" #-}
                            []
                            {-# LINE 2584 "src-ag/ResolveLocals.hs" #-}
                            )
                       -- self rule
                       _lhsOoutput =
                           ({-# LINE 39 "src-ag/ResolveLocals.ag" #-}
                            _output
                            {-# LINE 2590 "src-ag/ResolveLocals.hs" #-}
                            )
                   in  ( _lhsOerrors,_lhsOinstVars,_lhsOlocVars,_lhsOoutput))) )
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
sem_TypeSig (TypeSig _name _tp )  =
    (sem_TypeSig_TypeSig _name _tp )
-- semantic domain
newtype T_TypeSig  = T_TypeSig (( TypeSig ))
data Inh_TypeSig  = Inh_TypeSig {}
data Syn_TypeSig  = Syn_TypeSig {output_Syn_TypeSig :: TypeSig }
wrap_TypeSig :: T_TypeSig  ->
                Inh_TypeSig  ->
                Syn_TypeSig 
wrap_TypeSig (T_TypeSig sem ) (Inh_TypeSig )  =
    (let ( _lhsOoutput) = sem 
     in  (Syn_TypeSig _lhsOoutput ))
sem_TypeSig_TypeSig :: Identifier ->
                       Type ->
                       T_TypeSig 
sem_TypeSig_TypeSig name_ tp_  =
    (T_TypeSig (let _lhsOoutput :: TypeSig 
                    -- self rule
                    _output =
                        ({-# LINE 39 "src-ag/ResolveLocals.ag" #-}
                         TypeSig name_ tp_
                         {-# LINE 2629 "src-ag/ResolveLocals.hs" #-}
                         )
                    -- self rule
                    _lhsOoutput =
                        ({-# LINE 39 "src-ag/ResolveLocals.ag" #-}
                         _output
                         {-# LINE 2635 "src-ag/ResolveLocals.hs" #-}
                         )
                in  ( _lhsOoutput)) )
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
sem_TypeSigs list  =
    (Prelude.foldr sem_TypeSigs_Cons sem_TypeSigs_Nil (Prelude.map sem_TypeSig list) )
-- semantic domain
newtype T_TypeSigs  = T_TypeSigs (( TypeSigs ))
data Inh_TypeSigs  = Inh_TypeSigs {}
data Syn_TypeSigs  = Syn_TypeSigs {output_Syn_TypeSigs :: TypeSigs }
wrap_TypeSigs :: T_TypeSigs  ->
                 Inh_TypeSigs  ->
                 Syn_TypeSigs 
wrap_TypeSigs (T_TypeSigs sem ) (Inh_TypeSigs )  =
    (let ( _lhsOoutput) = sem 
     in  (Syn_TypeSigs _lhsOoutput ))
sem_TypeSigs_Cons :: T_TypeSig  ->
                     T_TypeSigs  ->
                     T_TypeSigs 
sem_TypeSigs_Cons (T_TypeSig hd_ ) (T_TypeSigs tl_ )  =
    (T_TypeSigs (let _lhsOoutput :: TypeSigs 
                     _hdIoutput :: TypeSig 
                     _tlIoutput :: TypeSigs 
                     -- self rule
                     _output =
                         ({-# LINE 39 "src-ag/ResolveLocals.ag" #-}
                          (:) _hdIoutput _tlIoutput
                          {-# LINE 2679 "src-ag/ResolveLocals.hs" #-}
                          )
                     -- self rule
                     _lhsOoutput =
                         ({-# LINE 39 "src-ag/ResolveLocals.ag" #-}
                          _output
                          {-# LINE 2685 "src-ag/ResolveLocals.hs" #-}
                          )
                     ( _hdIoutput) =
                         hd_ 
                     ( _tlIoutput) =
                         tl_ 
                 in  ( _lhsOoutput)) )
sem_TypeSigs_Nil :: T_TypeSigs 
sem_TypeSigs_Nil  =
    (T_TypeSigs (let _lhsOoutput :: TypeSigs 
                     -- self rule
                     _output =
                         ({-# LINE 39 "src-ag/ResolveLocals.ag" #-}
                          []
                          {-# LINE 2699 "src-ag/ResolveLocals.hs" #-}
                          )
                     -- self rule
                     _lhsOoutput =
                         ({-# LINE 39 "src-ag/ResolveLocals.ag" #-}
                          _output
                          {-# LINE 2705 "src-ag/ResolveLocals.hs" #-}
                          )
                 in  ( _lhsOoutput)) )