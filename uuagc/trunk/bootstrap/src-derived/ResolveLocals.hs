

-- UUAGC 0.9.39.1.0 (src-ag/ResolveLocals.ag)
module ResolveLocals where
{-# LINE 15 "src-ag/ResolveLocals.ag" #-}

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
import Macro --marcos
import CommonTypes
import ErrorMessages
{-# LINE 35 "dist/build/uuagc/uuagc-tmp/ResolveLocals.hs" #-}

{-# LINE 2 "src-ag/Patterns.ag" #-}

-- Patterns.ag imports
import UU.Scanner.Position(Pos)
import CommonTypes (ConstructorIdent,Identifier)
{-# LINE 42 "dist/build/uuagc/uuagc-tmp/ResolveLocals.hs" #-}

{-# LINE 2 "src-ag/Expression.ag" #-}

import UU.Scanner.Position(Pos)
import HsToken
{-# LINE 48 "dist/build/uuagc/uuagc-tmp/ResolveLocals.hs" #-}
-- Child -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allfields            : [(Identifier,Type,ChildKind)]
         allnts               : [Identifier]
         attrs                : [(Identifier,Identifier)]
         con                  : Identifier
         inh                  : Attributes
         inhMap               : Map Identifier Attributes
         mergeMap             : Map Identifier (Identifier,[Identifier])
         nt                   : Identifier
         syn                  : Attributes
         synMap               : Map Identifier Attributes
      synthesized attributes:
         attributes           : [(Identifier,Attributes,Attributes)]
         field                : (Identifier,Type,ChildKind)
         output               : SELF 
   alternatives:
      alternative Child:
         child name           : {Identifier}
         child tp             : {Type}
         child kind           : {ChildKind}
         visit 0:
            local chnt        : _
            local inh         : _
            local syn         : _
            local output      : _
-}
-- cata
sem_Child :: Child  ->
             T_Child 
sem_Child (Child _name _tp _kind )  =
    (sem_Child_Child _name _tp _kind )
-- semantic domain
newtype T_Child  = T_Child (([(Identifier,Type,ChildKind)]) ->
                            ([Identifier]) ->
                            ([(Identifier,Identifier)]) ->
                            Identifier ->
                            Attributes ->
                            (Map Identifier Attributes) ->
                            (Map Identifier (Identifier,[Identifier])) ->
                            Identifier ->
                            Attributes ->
                            (Map Identifier Attributes) ->
                            ( ([(Identifier,Attributes,Attributes)]),((Identifier,Type,ChildKind)),Child ))
data Inh_Child  = Inh_Child {allfields_Inh_Child :: ([(Identifier,Type,ChildKind)]),allnts_Inh_Child :: ([Identifier]),attrs_Inh_Child :: ([(Identifier,Identifier)]),con_Inh_Child :: Identifier,inh_Inh_Child :: Attributes,inhMap_Inh_Child :: (Map Identifier Attributes),mergeMap_Inh_Child :: (Map Identifier (Identifier,[Identifier])),nt_Inh_Child :: Identifier,syn_Inh_Child :: Attributes,synMap_Inh_Child :: (Map Identifier Attributes)}
data Syn_Child  = Syn_Child {attributes_Syn_Child :: ([(Identifier,Attributes,Attributes)]),field_Syn_Child :: ((Identifier,Type,ChildKind)),output_Syn_Child :: Child }
wrap_Child :: T_Child  ->
              Inh_Child  ->
              Syn_Child 
wrap_Child (T_Child sem ) (Inh_Child _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsIinh _lhsIinhMap _lhsImergeMap _lhsInt _lhsIsyn _lhsIsynMap )  =
    (let ( _lhsOattributes,_lhsOfield,_lhsOoutput) = sem _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsIinh _lhsIinhMap _lhsImergeMap _lhsInt _lhsIsyn _lhsIsynMap 
     in  (Syn_Child _lhsOattributes _lhsOfield _lhsOoutput ))
sem_Child_Child :: Identifier ->
                   Type ->
                   ChildKind ->
                   T_Child 
sem_Child_Child name_ tp_ kind_  =
    (T_Child (\ _lhsIallfields
                _lhsIallnts
                _lhsIattrs
                _lhsIcon
                _lhsIinh
                _lhsIinhMap
                _lhsImergeMap
                _lhsInt
                _lhsIsyn
                _lhsIsynMap ->
                  (let _lhsOattributes :: ([(Identifier,Attributes,Attributes)])
                       _lhsOfield :: ((Identifier,Type,ChildKind))
                       _lhsOoutput :: Child 
                       -- "src-ag/ResolveLocals.ag"(line 84, column 11)
                       _lhsOattributes =
                           ({-# LINE 84 "src-ag/ResolveLocals.ag" #-}
                            [(name_, _inh    , _syn    )]
                            {-# LINE 125 "src-ag/ResolveLocals.hs" #-}
                            )
                       -- "src-ag/ResolveLocals.ag"(line 87, column 11)
                       _lhsOfield =
                           ({-# LINE 87 "src-ag/ResolveLocals.ag" #-}
                            (name_, tp_, kind_)
                            {-# LINE 131 "src-ag/ResolveLocals.hs" #-}
                            )
                       -- "src-ag/DistChildAttr.ag"(line 19, column 11)
                       _chnt =
                           ({-# LINE 19 "src-ag/DistChildAttr.ag" #-}
                            case tp_ of
                              NT nt _ _ -> nt
                              Self      -> error ("The type of child " ++ show name_ ++ " should not be a Self type.")
                              Haskell t -> identifier t
                            {-# LINE 140 "src-ag/ResolveLocals.hs" #-}
                            )
                       -- "src-ag/DistChildAttr.ag"(line 23, column 11)
                       _inh =
                           ({-# LINE 23 "src-ag/DistChildAttr.ag" #-}
                            Map.findWithDefault Map.empty _chnt     _lhsIinhMap
                            {-# LINE 146 "src-ag/ResolveLocals.hs" #-}
                            )
                       -- "src-ag/DistChildAttr.ag"(line 24, column 11)
                       _syn =
                           ({-# LINE 24 "src-ag/DistChildAttr.ag" #-}
                            Map.findWithDefault Map.empty _chnt     _lhsIsynMap
                            {-# LINE 152 "src-ag/ResolveLocals.hs" #-}
                            )
                       -- self rule
                       _output =
                           ({-# LINE 47 "src-ag/ResolveLocals.ag" #-}
                            Child name_ tp_ kind_
                            {-# LINE 158 "src-ag/ResolveLocals.hs" #-}
                            )
                       -- self rule
                       _lhsOoutput =
                           ({-# LINE 47 "src-ag/ResolveLocals.ag" #-}
                            _output
                            {-# LINE 164 "src-ag/ResolveLocals.hs" #-}
                            )
                   in  ( _lhsOattributes,_lhsOfield,_lhsOoutput))) )
-- Children ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allfields            : [(Identifier,Type,ChildKind)]
         allnts               : [Identifier]
         attrs                : [(Identifier,Identifier)]
         con                  : Identifier
         inh                  : Attributes
         inhMap               : Map Identifier Attributes
         mergeMap             : Map Identifier (Identifier,[Identifier])
         nt                   : Identifier
         syn                  : Attributes
         synMap               : Map Identifier Attributes
      synthesized attributes:
         attributes           : [(Identifier,Attributes,Attributes)]
         fields               : [(Identifier,Type,ChildKind)]
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
newtype T_Children  = T_Children (([(Identifier,Type,ChildKind)]) ->
                                  ([Identifier]) ->
                                  ([(Identifier,Identifier)]) ->
                                  Identifier ->
                                  Attributes ->
                                  (Map Identifier Attributes) ->
                                  (Map Identifier (Identifier,[Identifier])) ->
                                  Identifier ->
                                  Attributes ->
                                  (Map Identifier Attributes) ->
                                  ( ([(Identifier,Attributes,Attributes)]),([(Identifier,Type,ChildKind)]),Children ))
data Inh_Children  = Inh_Children {allfields_Inh_Children :: ([(Identifier,Type,ChildKind)]),allnts_Inh_Children :: ([Identifier]),attrs_Inh_Children :: ([(Identifier,Identifier)]),con_Inh_Children :: Identifier,inh_Inh_Children :: Attributes,inhMap_Inh_Children :: (Map Identifier Attributes),mergeMap_Inh_Children :: (Map Identifier (Identifier,[Identifier])),nt_Inh_Children :: Identifier,syn_Inh_Children :: Attributes,synMap_Inh_Children :: (Map Identifier Attributes)}
data Syn_Children  = Syn_Children {attributes_Syn_Children :: ([(Identifier,Attributes,Attributes)]),fields_Syn_Children :: ([(Identifier,Type,ChildKind)]),output_Syn_Children :: Children }
wrap_Children :: T_Children  ->
                 Inh_Children  ->
                 Syn_Children 
wrap_Children (T_Children sem ) (Inh_Children _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsIinh _lhsIinhMap _lhsImergeMap _lhsInt _lhsIsyn _lhsIsynMap )  =
    (let ( _lhsOattributes,_lhsOfields,_lhsOoutput) = sem _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsIinh _lhsIinhMap _lhsImergeMap _lhsInt _lhsIsyn _lhsIsynMap 
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
                   _lhsIinhMap
                   _lhsImergeMap
                   _lhsInt
                   _lhsIsyn
                   _lhsIsynMap ->
                     (let _lhsOfields :: ([(Identifier,Type,ChildKind)])
                          _lhsOattributes :: ([(Identifier,Attributes,Attributes)])
                          _lhsOoutput :: Children 
                          _hdOallfields :: ([(Identifier,Type,ChildKind)])
                          _hdOallnts :: ([Identifier])
                          _hdOattrs :: ([(Identifier,Identifier)])
                          _hdOcon :: Identifier
                          _hdOinh :: Attributes
                          _hdOinhMap :: (Map Identifier Attributes)
                          _hdOmergeMap :: (Map Identifier (Identifier,[Identifier]))
                          _hdOnt :: Identifier
                          _hdOsyn :: Attributes
                          _hdOsynMap :: (Map Identifier Attributes)
                          _tlOallfields :: ([(Identifier,Type,ChildKind)])
                          _tlOallnts :: ([Identifier])
                          _tlOattrs :: ([(Identifier,Identifier)])
                          _tlOcon :: Identifier
                          _tlOinh :: Attributes
                          _tlOinhMap :: (Map Identifier Attributes)
                          _tlOmergeMap :: (Map Identifier (Identifier,[Identifier]))
                          _tlOnt :: Identifier
                          _tlOsyn :: Attributes
                          _tlOsynMap :: (Map Identifier Attributes)
                          _hdIattributes :: ([(Identifier,Attributes,Attributes)])
                          _hdIfield :: ((Identifier,Type,ChildKind))
                          _hdIoutput :: Child 
                          _tlIattributes :: ([(Identifier,Attributes,Attributes)])
                          _tlIfields :: ([(Identifier,Type,ChildKind)])
                          _tlIoutput :: Children 
                          -- "src-ag/ResolveLocals.ag"(line 90, column 11)
                          _lhsOfields =
                              ({-# LINE 90 "src-ag/ResolveLocals.ag" #-}
                               _hdIfield : _tlIfields
                               {-# LINE 267 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- use rule "src-ag/ResolveLocals.ag"(line 82, column 32)
                          _lhsOattributes =
                              ({-# LINE 82 "src-ag/ResolveLocals.ag" #-}
                               _hdIattributes ++ _tlIattributes
                               {-# LINE 273 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- self rule
                          _output =
                              ({-# LINE 47 "src-ag/ResolveLocals.ag" #-}
                               (:) _hdIoutput _tlIoutput
                               {-# LINE 279 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- self rule
                          _lhsOoutput =
                              ({-# LINE 47 "src-ag/ResolveLocals.ag" #-}
                               _output
                               {-# LINE 285 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOallfields =
                              ({-# LINE 71 "src-ag/ResolveLocals.ag" #-}
                               _lhsIallfields
                               {-# LINE 291 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOallnts =
                              ({-# LINE 57 "src-ag/ResolveLocals.ag" #-}
                               _lhsIallnts
                               {-# LINE 297 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOattrs =
                              ({-# LINE 71 "src-ag/ResolveLocals.ag" #-}
                               _lhsIattrs
                               {-# LINE 303 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOcon =
                              ({-# LINE 105 "src-ag/ResolveLocals.ag" #-}
                               _lhsIcon
                               {-# LINE 309 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOinh =
                              ({-# LINE 104 "src-ag/ResolveLocals.ag" #-}
                               _lhsIinh
                               {-# LINE 315 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOinhMap =
                              ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                               _lhsIinhMap
                               {-# LINE 321 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOmergeMap =
                              ({-# LINE 131 "src-ag/ResolveLocals.ag" #-}
                               _lhsImergeMap
                               {-# LINE 327 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOnt =
                              ({-# LINE 104 "src-ag/ResolveLocals.ag" #-}
                               _lhsInt
                               {-# LINE 333 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOsyn =
                              ({-# LINE 104 "src-ag/ResolveLocals.ag" #-}
                               _lhsIsyn
                               {-# LINE 339 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOsynMap =
                              ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                               _lhsIsynMap
                               {-# LINE 345 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOallfields =
                              ({-# LINE 71 "src-ag/ResolveLocals.ag" #-}
                               _lhsIallfields
                               {-# LINE 351 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOallnts =
                              ({-# LINE 57 "src-ag/ResolveLocals.ag" #-}
                               _lhsIallnts
                               {-# LINE 357 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOattrs =
                              ({-# LINE 71 "src-ag/ResolveLocals.ag" #-}
                               _lhsIattrs
                               {-# LINE 363 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOcon =
                              ({-# LINE 105 "src-ag/ResolveLocals.ag" #-}
                               _lhsIcon
                               {-# LINE 369 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOinh =
                              ({-# LINE 104 "src-ag/ResolveLocals.ag" #-}
                               _lhsIinh
                               {-# LINE 375 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOinhMap =
                              ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                               _lhsIinhMap
                               {-# LINE 381 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOmergeMap =
                              ({-# LINE 131 "src-ag/ResolveLocals.ag" #-}
                               _lhsImergeMap
                               {-# LINE 387 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOnt =
                              ({-# LINE 104 "src-ag/ResolveLocals.ag" #-}
                               _lhsInt
                               {-# LINE 393 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOsyn =
                              ({-# LINE 104 "src-ag/ResolveLocals.ag" #-}
                               _lhsIsyn
                               {-# LINE 399 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOsynMap =
                              ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                               _lhsIsynMap
                               {-# LINE 405 "src-ag/ResolveLocals.hs" #-}
                               )
                          ( _hdIattributes,_hdIfield,_hdIoutput) =
                              hd_ _hdOallfields _hdOallnts _hdOattrs _hdOcon _hdOinh _hdOinhMap _hdOmergeMap _hdOnt _hdOsyn _hdOsynMap 
                          ( _tlIattributes,_tlIfields,_tlIoutput) =
                              tl_ _tlOallfields _tlOallnts _tlOattrs _tlOcon _tlOinh _tlOinhMap _tlOmergeMap _tlOnt _tlOsyn _tlOsynMap 
                      in  ( _lhsOattributes,_lhsOfields,_lhsOoutput))) )
sem_Children_Nil :: T_Children 
sem_Children_Nil  =
    (T_Children (\ _lhsIallfields
                   _lhsIallnts
                   _lhsIattrs
                   _lhsIcon
                   _lhsIinh
                   _lhsIinhMap
                   _lhsImergeMap
                   _lhsInt
                   _lhsIsyn
                   _lhsIsynMap ->
                     (let _lhsOfields :: ([(Identifier,Type,ChildKind)])
                          _lhsOattributes :: ([(Identifier,Attributes,Attributes)])
                          _lhsOoutput :: Children 
                          -- "src-ag/ResolveLocals.ag"(line 91, column 11)
                          _lhsOfields =
                              ({-# LINE 91 "src-ag/ResolveLocals.ag" #-}
                               []
                               {-# LINE 431 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- use rule "src-ag/ResolveLocals.ag"(line 82, column 32)
                          _lhsOattributes =
                              ({-# LINE 82 "src-ag/ResolveLocals.ag" #-}
                               []
                               {-# LINE 437 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- self rule
                          _output =
                              ({-# LINE 47 "src-ag/ResolveLocals.ag" #-}
                               []
                               {-# LINE 443 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- self rule
                          _lhsOoutput =
                              ({-# LINE 47 "src-ag/ResolveLocals.ag" #-}
                               _output
                               {-# LINE 449 "src-ag/ResolveLocals.hs" #-}
                               )
                      in  ( _lhsOattributes,_lhsOfields,_lhsOoutput))) )
-- Expression --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allfields            : [(Identifier,Type,ChildKind)]
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
newtype T_Expression  = T_Expression (([(Identifier,Type,ChildKind)]) ->
                                      ([Identifier]) ->
                                      ([(Identifier,Identifier)]) ->
                                      Identifier ->
                                      (Map Identifier (Identifier,[Identifier])) ->
                                      Identifier ->
                                      Options ->
                                      ( (Seq Error),Expression ))
data Inh_Expression  = Inh_Expression {allfields_Inh_Expression :: ([(Identifier,Type,ChildKind)]),allnts_Inh_Expression :: ([Identifier]),attrs_Inh_Expression :: ([(Identifier,Identifier)]),con_Inh_Expression :: Identifier,mergeMap_Inh_Expression :: (Map Identifier (Identifier,[Identifier])),nt_Inh_Expression :: Identifier,options_Inh_Expression :: Options}
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
                            -- "src-ag/ResolveLocals.ag"(line 145, column 21)
                            __tup1 =
                                ({-# LINE 145 "src-ag/ResolveLocals.ag" #-}
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
                                 {-# LINE 525 "src-ag/ResolveLocals.hs" #-}
                                 )
                            -- "src-ag/ResolveLocals.ag"(line 145, column 21)
                            (_errors,_) =
                                ({-# LINE 145 "src-ag/ResolveLocals.ag" #-}
                                 __tup1
                                 {-# LINE 531 "src-ag/ResolveLocals.hs" #-}
                                 )
                            -- "src-ag/ResolveLocals.ag"(line 145, column 21)
                            (_,_newTks) =
                                ({-# LINE 145 "src-ag/ResolveLocals.ag" #-}
                                 __tup1
                                 {-# LINE 537 "src-ag/ResolveLocals.hs" #-}
                                 )
                            -- "src-ag/ResolveLocals.ag"(line 157, column 17)
                            _lhsOoutput =
                                ({-# LINE 157 "src-ag/ResolveLocals.ag" #-}
                                 Expression pos_ _newTks
                                 {-# LINE 543 "src-ag/ResolveLocals.hs" #-}
                                 )
                            -- use rule "src-ag/ResolveLocals.ag"(line 44, column 16)
                            _lhsOerrors =
                                ({-# LINE 44 "src-ag/ResolveLocals.ag" #-}
                                 _errors
                                 {-# LINE 549 "src-ag/ResolveLocals.hs" #-}
                                 )
                            -- self rule
                            _output =
                                ({-# LINE 47 "src-ag/ResolveLocals.ag" #-}
                                 Expression pos_ tks_
                                 {-# LINE 555 "src-ag/ResolveLocals.hs" #-}
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
                         _nontsOinhMap :: (Map Identifier Attributes)
                         _nontsOsynMap :: (Map Identifier Attributes)
                         _lhsOerrors :: (Seq Error)
                         _lhsOoutput :: Grammar 
                         _nontsOoptions :: Options
                         _nontsIerrors :: (Seq Error)
                         _nontsIinhMap' :: (Map Identifier Attributes)
                         _nontsInonts :: ([(NontermIdent,[ConstructorIdent])])
                         _nontsIoutput :: Nonterminals 
                         _nontsIsynMap' :: (Map Identifier Attributes)
                         -- "src-ag/ResolveLocals.ag"(line 60, column 13)
                         _nontsOallnts =
                             ({-# LINE 60 "src-ag/ResolveLocals.ag" #-}
                              map fst (_nontsInonts)
                              {-# LINE 634 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- "src-ag/ResolveLocals.ag"(line 120, column 14)
                         _nontsOmergeMap =
                             ({-# LINE 120 "src-ag/ResolveLocals.ag" #-}
                              Map.map (Map.map (Map.map (\(nt,srcs,_) -> (nt,srcs)))) mergeMap_
                              {-# LINE 640 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- "src-ag/DistChildAttr.ag"(line 15, column 13)
                         _nontsOinhMap =
                             ({-# LINE 15 "src-ag/DistChildAttr.ag" #-}
                              _nontsIinhMap'
                              {-# LINE 646 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- "src-ag/DistChildAttr.ag"(line 16, column 13)
                         _nontsOsynMap =
                             ({-# LINE 16 "src-ag/DistChildAttr.ag" #-}
                              _nontsIsynMap'
                              {-# LINE 652 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- use rule "src-ag/ResolveLocals.ag"(line 44, column 16)
                         _lhsOerrors =
                             ({-# LINE 44 "src-ag/ResolveLocals.ag" #-}
                              _nontsIerrors
                              {-# LINE 658 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- self rule
                         _output =
                             ({-# LINE 47 "src-ag/ResolveLocals.ag" #-}
                              Grammar typeSyns_ useMap_ derivings_ wrappers_ _nontsIoutput pragmas_ manualAttrOrderMap_ paramMap_ contextMap_ quantMap_ uniqueMap_ augmentsMap_ aroundsMap_ mergeMap_
                              {-# LINE 664 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- self rule
                         _lhsOoutput =
                             ({-# LINE 47 "src-ag/ResolveLocals.ag" #-}
                              _output
                              {-# LINE 670 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- copy rule (down)
                         _nontsOoptions =
                             ({-# LINE 41 "src-ag/ResolveLocals.ag" #-}
                              _lhsIoptions
                              {-# LINE 676 "src-ag/ResolveLocals.hs" #-}
                              )
                         ( _nontsIerrors,_nontsIinhMap',_nontsInonts,_nontsIoutput,_nontsIsynMap') =
                             nonts_ _nontsOallnts _nontsOinhMap _nontsOmergeMap _nontsOoptions _nontsOsynMap 
                     in  ( _lhsOerrors,_lhsOoutput))) )
-- Nonterminal -------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allnts               : [Identifier]
         inhMap               : Map Identifier Attributes
         mergeMap             : Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier])))
         options              : Options
         synMap               : Map Identifier Attributes
      synthesized attributes:
         errors               : Seq Error
         inhMap'              : Map Identifier Attributes
         nonts                : [(NontermIdent,[ConstructorIdent])]
         output               : SELF 
         synMap'              : Map Identifier Attributes
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
                                        (Map Identifier Attributes) ->
                                        (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier])))) ->
                                        Options ->
                                        (Map Identifier Attributes) ->
                                        ( (Seq Error),(Map Identifier Attributes),([(NontermIdent,[ConstructorIdent])]),Nonterminal ,(Map Identifier Attributes)))
data Inh_Nonterminal  = Inh_Nonterminal {allnts_Inh_Nonterminal :: ([Identifier]),inhMap_Inh_Nonterminal :: (Map Identifier Attributes),mergeMap_Inh_Nonterminal :: (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier])))),options_Inh_Nonterminal :: Options,synMap_Inh_Nonterminal :: (Map Identifier Attributes)}
data Syn_Nonterminal  = Syn_Nonterminal {errors_Syn_Nonterminal :: (Seq Error),inhMap'_Syn_Nonterminal :: (Map Identifier Attributes),nonts_Syn_Nonterminal :: ([(NontermIdent,[ConstructorIdent])]),output_Syn_Nonterminal :: Nonterminal ,synMap'_Syn_Nonterminal :: (Map Identifier Attributes)}
wrap_Nonterminal :: T_Nonterminal  ->
                    Inh_Nonterminal  ->
                    Syn_Nonterminal 
wrap_Nonterminal (T_Nonterminal sem ) (Inh_Nonterminal _lhsIallnts _lhsIinhMap _lhsImergeMap _lhsIoptions _lhsIsynMap )  =
    (let ( _lhsOerrors,_lhsOinhMap',_lhsOnonts,_lhsOoutput,_lhsOsynMap') = sem _lhsIallnts _lhsIinhMap _lhsImergeMap _lhsIoptions _lhsIsynMap 
     in  (Syn_Nonterminal _lhsOerrors _lhsOinhMap' _lhsOnonts _lhsOoutput _lhsOsynMap' ))
sem_Nonterminal_Nonterminal :: NontermIdent ->
                               ([Identifier]) ->
                               Attributes ->
                               Attributes ->
                               T_Productions  ->
                               T_Nonterminal 
sem_Nonterminal_Nonterminal nt_ params_ inh_ syn_ (T_Productions prods_ )  =
    (T_Nonterminal (\ _lhsIallnts
                      _lhsIinhMap
                      _lhsImergeMap
                      _lhsIoptions
                      _lhsIsynMap ->
                        (let _lhsOnonts :: ([(NontermIdent,[ConstructorIdent])])
                             _prodsOnt :: Identifier
                             _prodsOinh :: Attributes
                             _prodsOsyn :: Attributes
                             _lhsOinhMap' :: (Map Identifier Attributes)
                             _lhsOsynMap' :: (Map Identifier Attributes)
                             _lhsOerrors :: (Seq Error)
                             _lhsOoutput :: Nonterminal 
                             _prodsOallnts :: ([Identifier])
                             _prodsOinhMap :: (Map Identifier Attributes)
                             _prodsOmergeMap :: (Map ConstructorIdent (Map Identifier (Identifier,[Identifier])))
                             _prodsOoptions :: Options
                             _prodsOsynMap :: (Map Identifier Attributes)
                             _prodsIcons :: ([ConstructorIdent])
                             _prodsIerrors :: (Seq Error)
                             _prodsIoutput :: Productions 
                             -- "src-ag/ResolveLocals.ag"(line 64, column 19)
                             _lhsOnonts =
                                 ({-# LINE 64 "src-ag/ResolveLocals.ag" #-}
                                  [(nt_,_prodsIcons)]
                                  {-# LINE 759 "src-ag/ResolveLocals.hs" #-}
                                  )
                             -- "src-ag/ResolveLocals.ag"(line 112, column 17)
                             _prodsOnt =
                                 ({-# LINE 112 "src-ag/ResolveLocals.ag" #-}
                                  nt_
                                  {-# LINE 765 "src-ag/ResolveLocals.hs" #-}
                                  )
                             -- "src-ag/ResolveLocals.ag"(line 115, column 17)
                             _prodsOinh =
                                 ({-# LINE 115 "src-ag/ResolveLocals.ag" #-}
                                  inh_
                                  {-# LINE 771 "src-ag/ResolveLocals.hs" #-}
                                  )
                             -- "src-ag/ResolveLocals.ag"(line 116, column 17)
                             _prodsOsyn =
                                 ({-# LINE 116 "src-ag/ResolveLocals.ag" #-}
                                  syn_
                                  {-# LINE 777 "src-ag/ResolveLocals.hs" #-}
                                  )
                             -- "src-ag/ResolveLocals.ag"(line 128, column 32)
                             _mergeMap =
                                 ({-# LINE 128 "src-ag/ResolveLocals.ag" #-}
                                  Map.findWithDefault Map.empty nt_ _lhsImergeMap
                                  {-# LINE 783 "src-ag/ResolveLocals.hs" #-}
                                  )
                             -- "src-ag/DistChildAttr.ag"(line 7, column 18)
                             _lhsOinhMap' =
                                 ({-# LINE 7 "src-ag/DistChildAttr.ag" #-}
                                  Map.singleton nt_ inh_
                                  {-# LINE 789 "src-ag/ResolveLocals.hs" #-}
                                  )
                             -- "src-ag/DistChildAttr.ag"(line 8, column 18)
                             _lhsOsynMap' =
                                 ({-# LINE 8 "src-ag/DistChildAttr.ag" #-}
                                  Map.singleton nt_ syn_
                                  {-# LINE 795 "src-ag/ResolveLocals.hs" #-}
                                  )
                             -- use rule "src-ag/ResolveLocals.ag"(line 44, column 16)
                             _lhsOerrors =
                                 ({-# LINE 44 "src-ag/ResolveLocals.ag" #-}
                                  _prodsIerrors
                                  {-# LINE 801 "src-ag/ResolveLocals.hs" #-}
                                  )
                             -- self rule
                             _output =
                                 ({-# LINE 47 "src-ag/ResolveLocals.ag" #-}
                                  Nonterminal nt_ params_ inh_ syn_ _prodsIoutput
                                  {-# LINE 807 "src-ag/ResolveLocals.hs" #-}
                                  )
                             -- self rule
                             _lhsOoutput =
                                 ({-# LINE 47 "src-ag/ResolveLocals.ag" #-}
                                  _output
                                  {-# LINE 813 "src-ag/ResolveLocals.hs" #-}
                                  )
                             -- copy rule (down)
                             _prodsOallnts =
                                 ({-# LINE 57 "src-ag/ResolveLocals.ag" #-}
                                  _lhsIallnts
                                  {-# LINE 819 "src-ag/ResolveLocals.hs" #-}
                                  )
                             -- copy rule (down)
                             _prodsOinhMap =
                                 ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                  _lhsIinhMap
                                  {-# LINE 825 "src-ag/ResolveLocals.hs" #-}
                                  )
                             -- copy rule (from local)
                             _prodsOmergeMap =
                                 ({-# LINE 126 "src-ag/ResolveLocals.ag" #-}
                                  _mergeMap
                                  {-# LINE 831 "src-ag/ResolveLocals.hs" #-}
                                  )
                             -- copy rule (down)
                             _prodsOoptions =
                                 ({-# LINE 41 "src-ag/ResolveLocals.ag" #-}
                                  _lhsIoptions
                                  {-# LINE 837 "src-ag/ResolveLocals.hs" #-}
                                  )
                             -- copy rule (down)
                             _prodsOsynMap =
                                 ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                  _lhsIsynMap
                                  {-# LINE 843 "src-ag/ResolveLocals.hs" #-}
                                  )
                             ( _prodsIcons,_prodsIerrors,_prodsIoutput) =
                                 prods_ _prodsOallnts _prodsOinh _prodsOinhMap _prodsOmergeMap _prodsOnt _prodsOoptions _prodsOsyn _prodsOsynMap 
                         in  ( _lhsOerrors,_lhsOinhMap',_lhsOnonts,_lhsOoutput,_lhsOsynMap'))) )
-- Nonterminals ------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allnts               : [Identifier]
         inhMap               : Map Identifier Attributes
         mergeMap             : Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier])))
         options              : Options
         synMap               : Map Identifier Attributes
      synthesized attributes:
         errors               : Seq Error
         inhMap'              : Map Identifier Attributes
         nonts                : [(NontermIdent,[ConstructorIdent])]
         output               : SELF 
         synMap'              : Map Identifier Attributes
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
                                          (Map Identifier Attributes) ->
                                          (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier])))) ->
                                          Options ->
                                          (Map Identifier Attributes) ->
                                          ( (Seq Error),(Map Identifier Attributes),([(NontermIdent,[ConstructorIdent])]),Nonterminals ,(Map Identifier Attributes)))
data Inh_Nonterminals  = Inh_Nonterminals {allnts_Inh_Nonterminals :: ([Identifier]),inhMap_Inh_Nonterminals :: (Map Identifier Attributes),mergeMap_Inh_Nonterminals :: (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier])))),options_Inh_Nonterminals :: Options,synMap_Inh_Nonterminals :: (Map Identifier Attributes)}
data Syn_Nonterminals  = Syn_Nonterminals {errors_Syn_Nonterminals :: (Seq Error),inhMap'_Syn_Nonterminals :: (Map Identifier Attributes),nonts_Syn_Nonterminals :: ([(NontermIdent,[ConstructorIdent])]),output_Syn_Nonterminals :: Nonterminals ,synMap'_Syn_Nonterminals :: (Map Identifier Attributes)}
wrap_Nonterminals :: T_Nonterminals  ->
                     Inh_Nonterminals  ->
                     Syn_Nonterminals 
wrap_Nonterminals (T_Nonterminals sem ) (Inh_Nonterminals _lhsIallnts _lhsIinhMap _lhsImergeMap _lhsIoptions _lhsIsynMap )  =
    (let ( _lhsOerrors,_lhsOinhMap',_lhsOnonts,_lhsOoutput,_lhsOsynMap') = sem _lhsIallnts _lhsIinhMap _lhsImergeMap _lhsIoptions _lhsIsynMap 
     in  (Syn_Nonterminals _lhsOerrors _lhsOinhMap' _lhsOnonts _lhsOoutput _lhsOsynMap' ))
sem_Nonterminals_Cons :: T_Nonterminal  ->
                         T_Nonterminals  ->
                         T_Nonterminals 
sem_Nonterminals_Cons (T_Nonterminal hd_ ) (T_Nonterminals tl_ )  =
    (T_Nonterminals (\ _lhsIallnts
                       _lhsIinhMap
                       _lhsImergeMap
                       _lhsIoptions
                       _lhsIsynMap ->
                         (let _lhsOerrors :: (Seq Error)
                              _lhsOinhMap' :: (Map Identifier Attributes)
                              _lhsOnonts :: ([(NontermIdent,[ConstructorIdent])])
                              _lhsOsynMap' :: (Map Identifier Attributes)
                              _lhsOoutput :: Nonterminals 
                              _hdOallnts :: ([Identifier])
                              _hdOinhMap :: (Map Identifier Attributes)
                              _hdOmergeMap :: (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier]))))
                              _hdOoptions :: Options
                              _hdOsynMap :: (Map Identifier Attributes)
                              _tlOallnts :: ([Identifier])
                              _tlOinhMap :: (Map Identifier Attributes)
                              _tlOmergeMap :: (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier]))))
                              _tlOoptions :: Options
                              _tlOsynMap :: (Map Identifier Attributes)
                              _hdIerrors :: (Seq Error)
                              _hdIinhMap' :: (Map Identifier Attributes)
                              _hdInonts :: ([(NontermIdent,[ConstructorIdent])])
                              _hdIoutput :: Nonterminal 
                              _hdIsynMap' :: (Map Identifier Attributes)
                              _tlIerrors :: (Seq Error)
                              _tlIinhMap' :: (Map Identifier Attributes)
                              _tlInonts :: ([(NontermIdent,[ConstructorIdent])])
                              _tlIoutput :: Nonterminals 
                              _tlIsynMap' :: (Map Identifier Attributes)
                              -- use rule "src-ag/ResolveLocals.ag"(line 44, column 16)
                              _lhsOerrors =
                                  ({-# LINE 44 "src-ag/ResolveLocals.ag" #-}
                                   _hdIerrors Seq.>< _tlIerrors
                                   {-# LINE 931 "src-ag/ResolveLocals.hs" #-}
                                   )
                              -- use rule "src-ag/DistChildAttr.ag"(line 4, column 53)
                              _lhsOinhMap' =
                                  ({-# LINE 4 "src-ag/DistChildAttr.ag" #-}
                                   _hdIinhMap' `Map.union` _tlIinhMap'
                                   {-# LINE 937 "src-ag/ResolveLocals.hs" #-}
                                   )
                              -- use rule "src-ag/ResolveLocals.ag"(line 62, column 43)
                              _lhsOnonts =
                                  ({-# LINE 62 "src-ag/ResolveLocals.ag" #-}
                                   _hdInonts ++ _tlInonts
                                   {-# LINE 943 "src-ag/ResolveLocals.hs" #-}
                                   )
                              -- use rule "src-ag/DistChildAttr.ag"(line 4, column 53)
                              _lhsOsynMap' =
                                  ({-# LINE 4 "src-ag/DistChildAttr.ag" #-}
                                   _hdIsynMap' `Map.union` _tlIsynMap'
                                   {-# LINE 949 "src-ag/ResolveLocals.hs" #-}
                                   )
                              -- self rule
                              _output =
                                  ({-# LINE 47 "src-ag/ResolveLocals.ag" #-}
                                   (:) _hdIoutput _tlIoutput
                                   {-# LINE 955 "src-ag/ResolveLocals.hs" #-}
                                   )
                              -- self rule
                              _lhsOoutput =
                                  ({-# LINE 47 "src-ag/ResolveLocals.ag" #-}
                                   _output
                                   {-# LINE 961 "src-ag/ResolveLocals.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOallnts =
                                  ({-# LINE 57 "src-ag/ResolveLocals.ag" #-}
                                   _lhsIallnts
                                   {-# LINE 967 "src-ag/ResolveLocals.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOinhMap =
                                  ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                   _lhsIinhMap
                                   {-# LINE 973 "src-ag/ResolveLocals.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOmergeMap =
                                  ({-# LINE 123 "src-ag/ResolveLocals.ag" #-}
                                   _lhsImergeMap
                                   {-# LINE 979 "src-ag/ResolveLocals.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOoptions =
                                  ({-# LINE 41 "src-ag/ResolveLocals.ag" #-}
                                   _lhsIoptions
                                   {-# LINE 985 "src-ag/ResolveLocals.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOsynMap =
                                  ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                   _lhsIsynMap
                                   {-# LINE 991 "src-ag/ResolveLocals.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOallnts =
                                  ({-# LINE 57 "src-ag/ResolveLocals.ag" #-}
                                   _lhsIallnts
                                   {-# LINE 997 "src-ag/ResolveLocals.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOinhMap =
                                  ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                   _lhsIinhMap
                                   {-# LINE 1003 "src-ag/ResolveLocals.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOmergeMap =
                                  ({-# LINE 123 "src-ag/ResolveLocals.ag" #-}
                                   _lhsImergeMap
                                   {-# LINE 1009 "src-ag/ResolveLocals.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOoptions =
                                  ({-# LINE 41 "src-ag/ResolveLocals.ag" #-}
                                   _lhsIoptions
                                   {-# LINE 1015 "src-ag/ResolveLocals.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOsynMap =
                                  ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                   _lhsIsynMap
                                   {-# LINE 1021 "src-ag/ResolveLocals.hs" #-}
                                   )
                              ( _hdIerrors,_hdIinhMap',_hdInonts,_hdIoutput,_hdIsynMap') =
                                  hd_ _hdOallnts _hdOinhMap _hdOmergeMap _hdOoptions _hdOsynMap 
                              ( _tlIerrors,_tlIinhMap',_tlInonts,_tlIoutput,_tlIsynMap') =
                                  tl_ _tlOallnts _tlOinhMap _tlOmergeMap _tlOoptions _tlOsynMap 
                          in  ( _lhsOerrors,_lhsOinhMap',_lhsOnonts,_lhsOoutput,_lhsOsynMap'))) )
sem_Nonterminals_Nil :: T_Nonterminals 
sem_Nonterminals_Nil  =
    (T_Nonterminals (\ _lhsIallnts
                       _lhsIinhMap
                       _lhsImergeMap
                       _lhsIoptions
                       _lhsIsynMap ->
                         (let _lhsOerrors :: (Seq Error)
                              _lhsOinhMap' :: (Map Identifier Attributes)
                              _lhsOnonts :: ([(NontermIdent,[ConstructorIdent])])
                              _lhsOsynMap' :: (Map Identifier Attributes)
                              _lhsOoutput :: Nonterminals 
                              -- use rule "src-ag/ResolveLocals.ag"(line 44, column 16)
                              _lhsOerrors =
                                  ({-# LINE 44 "src-ag/ResolveLocals.ag" #-}
                                   Seq.empty
                                   {-# LINE 1044 "src-ag/ResolveLocals.hs" #-}
                                   )
                              -- use rule "src-ag/DistChildAttr.ag"(line 4, column 53)
                              _lhsOinhMap' =
                                  ({-# LINE 4 "src-ag/DistChildAttr.ag" #-}
                                   Map.empty
                                   {-# LINE 1050 "src-ag/ResolveLocals.hs" #-}
                                   )
                              -- use rule "src-ag/ResolveLocals.ag"(line 62, column 43)
                              _lhsOnonts =
                                  ({-# LINE 62 "src-ag/ResolveLocals.ag" #-}
                                   []
                                   {-# LINE 1056 "src-ag/ResolveLocals.hs" #-}
                                   )
                              -- use rule "src-ag/DistChildAttr.ag"(line 4, column 53)
                              _lhsOsynMap' =
                                  ({-# LINE 4 "src-ag/DistChildAttr.ag" #-}
                                   Map.empty
                                   {-# LINE 1062 "src-ag/ResolveLocals.hs" #-}
                                   )
                              -- self rule
                              _output =
                                  ({-# LINE 47 "src-ag/ResolveLocals.ag" #-}
                                   []
                                   {-# LINE 1068 "src-ag/ResolveLocals.hs" #-}
                                   )
                              -- self rule
                              _lhsOoutput =
                                  ({-# LINE 47 "src-ag/ResolveLocals.ag" #-}
                                   _output
                                   {-# LINE 1074 "src-ag/ResolveLocals.hs" #-}
                                   )
                          in  ( _lhsOerrors,_lhsOinhMap',_lhsOnonts,_lhsOoutput,_lhsOsynMap'))) )
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
                     T_Pattern 
sem_Pattern_Alias field_ attr_ (T_Pattern pat_ )  =
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
                         _patIcopy :: Pattern 
                         _patIerrors :: (Seq Error)
                         _patIinstVars :: ([Identifier])
                         _patIlocVars :: ([Identifier])
                         _patIoutput :: Pattern 
                         -- "src-ag/ResolveLocals.ag"(line 96, column 14)
                         _lhsOlocVars =
                             ({-# LINE 96 "src-ag/ResolveLocals.ag" #-}
                              if field_ == _LOC
                                 then [attr_]
                                 else []
                              {-# LINE 1178 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- "src-ag/ResolveLocals.ag"(line 99, column 14)
                         _lhsOinstVars =
                             ({-# LINE 99 "src-ag/ResolveLocals.ag" #-}
                              if field_ == _INST
                                 then [attr_]
                                 else []
                              {-# LINE 1186 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- use rule "src-ag/ResolveLocals.ag"(line 44, column 16)
                         _lhsOerrors =
                             ({-# LINE 44 "src-ag/ResolveLocals.ag" #-}
                              _patIerrors
                              {-# LINE 1192 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- self rule
                         _copy =
                             ({-# LINE 22 "src-ag/Patterns.ag" #-}
                              Alias field_ attr_ _patIcopy
                              {-# LINE 1198 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- self rule
                         _output =
                             ({-# LINE 47 "src-ag/ResolveLocals.ag" #-}
                              Alias field_ attr_ _patIoutput
                              {-# LINE 1204 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- self rule
                         _lhsOcopy =
                             ({-# LINE 22 "src-ag/Patterns.ag" #-}
                              _copy
                              {-# LINE 1210 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- self rule
                         _lhsOoutput =
                             ({-# LINE 47 "src-ag/ResolveLocals.ag" #-}
                              _output
                              {-# LINE 1216 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- copy rule (down)
                         _patOcon =
                             ({-# LINE 105 "src-ag/ResolveLocals.ag" #-}
                              _lhsIcon
                              {-# LINE 1222 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- copy rule (down)
                         _patOinh =
                             ({-# LINE 104 "src-ag/ResolveLocals.ag" #-}
                              _lhsIinh
                              {-# LINE 1228 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- copy rule (down)
                         _patOnt =
                             ({-# LINE 104 "src-ag/ResolveLocals.ag" #-}
                              _lhsInt
                              {-# LINE 1234 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- copy rule (down)
                         _patOsyn =
                             ({-# LINE 104 "src-ag/ResolveLocals.ag" #-}
                              _lhsIsyn
                              {-# LINE 1240 "src-ag/ResolveLocals.hs" #-}
                              )
                         ( _patIcopy,_patIerrors,_patIinstVars,_patIlocVars,_patIoutput) =
                             pat_ _patOcon _patOinh _patOnt _patOsyn 
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
                         -- use rule "src-ag/ResolveLocals.ag"(line 44, column 16)
                         _lhsOerrors =
                             ({-# LINE 44 "src-ag/ResolveLocals.ag" #-}
                              _patsIerrors
                              {-# LINE 1271 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- use rule "src-ag/ResolveLocals.ag"(line 93, column 86)
                         _lhsOinstVars =
                             ({-# LINE 93 "src-ag/ResolveLocals.ag" #-}
                              _patsIinstVars
                              {-# LINE 1277 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- use rule "src-ag/ResolveLocals.ag"(line 93, column 48)
                         _lhsOlocVars =
                             ({-# LINE 93 "src-ag/ResolveLocals.ag" #-}
                              _patsIlocVars
                              {-# LINE 1283 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- self rule
                         _copy =
                             ({-# LINE 22 "src-ag/Patterns.ag" #-}
                              Constr name_ _patsIcopy
                              {-# LINE 1289 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- self rule
                         _output =
                             ({-# LINE 47 "src-ag/ResolveLocals.ag" #-}
                              Constr name_ _patsIoutput
                              {-# LINE 1295 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- self rule
                         _lhsOcopy =
                             ({-# LINE 22 "src-ag/Patterns.ag" #-}
                              _copy
                              {-# LINE 1301 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- self rule
                         _lhsOoutput =
                             ({-# LINE 47 "src-ag/ResolveLocals.ag" #-}
                              _output
                              {-# LINE 1307 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- copy rule (down)
                         _patsOcon =
                             ({-# LINE 105 "src-ag/ResolveLocals.ag" #-}
                              _lhsIcon
                              {-# LINE 1313 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- copy rule (down)
                         _patsOinh =
                             ({-# LINE 104 "src-ag/ResolveLocals.ag" #-}
                              _lhsIinh
                              {-# LINE 1319 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- copy rule (down)
                         _patsOnt =
                             ({-# LINE 104 "src-ag/ResolveLocals.ag" #-}
                              _lhsInt
                              {-# LINE 1325 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- copy rule (down)
                         _patsOsyn =
                             ({-# LINE 104 "src-ag/ResolveLocals.ag" #-}
                              _lhsIsyn
                              {-# LINE 1331 "src-ag/ResolveLocals.hs" #-}
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
                         -- use rule "src-ag/ResolveLocals.ag"(line 44, column 16)
                         _lhsOerrors =
                             ({-# LINE 44 "src-ag/ResolveLocals.ag" #-}
                              _patIerrors
                              {-# LINE 1361 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- use rule "src-ag/ResolveLocals.ag"(line 93, column 86)
                         _lhsOinstVars =
                             ({-# LINE 93 "src-ag/ResolveLocals.ag" #-}
                              _patIinstVars
                              {-# LINE 1367 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- use rule "src-ag/ResolveLocals.ag"(line 93, column 48)
                         _lhsOlocVars =
                             ({-# LINE 93 "src-ag/ResolveLocals.ag" #-}
                              _patIlocVars
                              {-# LINE 1373 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- self rule
                         _copy =
                             ({-# LINE 22 "src-ag/Patterns.ag" #-}
                              Irrefutable _patIcopy
                              {-# LINE 1379 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- self rule
                         _output =
                             ({-# LINE 47 "src-ag/ResolveLocals.ag" #-}
                              Irrefutable _patIoutput
                              {-# LINE 1385 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- self rule
                         _lhsOcopy =
                             ({-# LINE 22 "src-ag/Patterns.ag" #-}
                              _copy
                              {-# LINE 1391 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- self rule
                         _lhsOoutput =
                             ({-# LINE 47 "src-ag/ResolveLocals.ag" #-}
                              _output
                              {-# LINE 1397 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- copy rule (down)
                         _patOcon =
                             ({-# LINE 105 "src-ag/ResolveLocals.ag" #-}
                              _lhsIcon
                              {-# LINE 1403 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- copy rule (down)
                         _patOinh =
                             ({-# LINE 104 "src-ag/ResolveLocals.ag" #-}
                              _lhsIinh
                              {-# LINE 1409 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- copy rule (down)
                         _patOnt =
                             ({-# LINE 104 "src-ag/ResolveLocals.ag" #-}
                              _lhsInt
                              {-# LINE 1415 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- copy rule (down)
                         _patOsyn =
                             ({-# LINE 104 "src-ag/ResolveLocals.ag" #-}
                              _lhsIsyn
                              {-# LINE 1421 "src-ag/ResolveLocals.hs" #-}
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
                         -- use rule "src-ag/ResolveLocals.ag"(line 44, column 16)
                         _lhsOerrors =
                             ({-# LINE 44 "src-ag/ResolveLocals.ag" #-}
                              _patsIerrors
                              {-# LINE 1452 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- use rule "src-ag/ResolveLocals.ag"(line 93, column 86)
                         _lhsOinstVars =
                             ({-# LINE 93 "src-ag/ResolveLocals.ag" #-}
                              _patsIinstVars
                              {-# LINE 1458 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- use rule "src-ag/ResolveLocals.ag"(line 93, column 48)
                         _lhsOlocVars =
                             ({-# LINE 93 "src-ag/ResolveLocals.ag" #-}
                              _patsIlocVars
                              {-# LINE 1464 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- self rule
                         _copy =
                             ({-# LINE 22 "src-ag/Patterns.ag" #-}
                              Product pos_ _patsIcopy
                              {-# LINE 1470 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- self rule
                         _output =
                             ({-# LINE 47 "src-ag/ResolveLocals.ag" #-}
                              Product pos_ _patsIoutput
                              {-# LINE 1476 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- self rule
                         _lhsOcopy =
                             ({-# LINE 22 "src-ag/Patterns.ag" #-}
                              _copy
                              {-# LINE 1482 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- self rule
                         _lhsOoutput =
                             ({-# LINE 47 "src-ag/ResolveLocals.ag" #-}
                              _output
                              {-# LINE 1488 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- copy rule (down)
                         _patsOcon =
                             ({-# LINE 105 "src-ag/ResolveLocals.ag" #-}
                              _lhsIcon
                              {-# LINE 1494 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- copy rule (down)
                         _patsOinh =
                             ({-# LINE 104 "src-ag/ResolveLocals.ag" #-}
                              _lhsIinh
                              {-# LINE 1500 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- copy rule (down)
                         _patsOnt =
                             ({-# LINE 104 "src-ag/ResolveLocals.ag" #-}
                              _lhsInt
                              {-# LINE 1506 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- copy rule (down)
                         _patsOsyn =
                             ({-# LINE 104 "src-ag/ResolveLocals.ag" #-}
                              _lhsIsyn
                              {-# LINE 1512 "src-ag/ResolveLocals.hs" #-}
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
                         -- use rule "src-ag/ResolveLocals.ag"(line 44, column 16)
                         _lhsOerrors =
                             ({-# LINE 44 "src-ag/ResolveLocals.ag" #-}
                              Seq.empty
                              {-# LINE 1533 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- use rule "src-ag/ResolveLocals.ag"(line 93, column 86)
                         _lhsOinstVars =
                             ({-# LINE 93 "src-ag/ResolveLocals.ag" #-}
                              []
                              {-# LINE 1539 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- use rule "src-ag/ResolveLocals.ag"(line 93, column 48)
                         _lhsOlocVars =
                             ({-# LINE 93 "src-ag/ResolveLocals.ag" #-}
                              []
                              {-# LINE 1545 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- self rule
                         _copy =
                             ({-# LINE 22 "src-ag/Patterns.ag" #-}
                              Underscore pos_
                              {-# LINE 1551 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- self rule
                         _output =
                             ({-# LINE 47 "src-ag/ResolveLocals.ag" #-}
                              Underscore pos_
                              {-# LINE 1557 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- self rule
                         _lhsOcopy =
                             ({-# LINE 22 "src-ag/Patterns.ag" #-}
                              _copy
                              {-# LINE 1563 "src-ag/ResolveLocals.hs" #-}
                              )
                         -- self rule
                         _lhsOoutput =
                             ({-# LINE 47 "src-ag/ResolveLocals.ag" #-}
                              _output
                              {-# LINE 1569 "src-ag/ResolveLocals.hs" #-}
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
                          -- use rule "src-ag/ResolveLocals.ag"(line 44, column 16)
                          _lhsOerrors =
                              ({-# LINE 44 "src-ag/ResolveLocals.ag" #-}
                               _hdIerrors Seq.>< _tlIerrors
                               {-# LINE 1652 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- use rule "src-ag/ResolveLocals.ag"(line 93, column 86)
                          _lhsOinstVars =
                              ({-# LINE 93 "src-ag/ResolveLocals.ag" #-}
                               _hdIinstVars ++ _tlIinstVars
                               {-# LINE 1658 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- use rule "src-ag/ResolveLocals.ag"(line 93, column 48)
                          _lhsOlocVars =
                              ({-# LINE 93 "src-ag/ResolveLocals.ag" #-}
                               _hdIlocVars ++ _tlIlocVars
                               {-# LINE 1664 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- self rule
                          _copy =
                              ({-# LINE 22 "src-ag/Patterns.ag" #-}
                               (:) _hdIcopy _tlIcopy
                               {-# LINE 1670 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- self rule
                          _output =
                              ({-# LINE 47 "src-ag/ResolveLocals.ag" #-}
                               (:) _hdIoutput _tlIoutput
                               {-# LINE 1676 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- self rule
                          _lhsOcopy =
                              ({-# LINE 22 "src-ag/Patterns.ag" #-}
                               _copy
                               {-# LINE 1682 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- self rule
                          _lhsOoutput =
                              ({-# LINE 47 "src-ag/ResolveLocals.ag" #-}
                               _output
                               {-# LINE 1688 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOcon =
                              ({-# LINE 105 "src-ag/ResolveLocals.ag" #-}
                               _lhsIcon
                               {-# LINE 1694 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOinh =
                              ({-# LINE 104 "src-ag/ResolveLocals.ag" #-}
                               _lhsIinh
                               {-# LINE 1700 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOnt =
                              ({-# LINE 104 "src-ag/ResolveLocals.ag" #-}
                               _lhsInt
                               {-# LINE 1706 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOsyn =
                              ({-# LINE 104 "src-ag/ResolveLocals.ag" #-}
                               _lhsIsyn
                               {-# LINE 1712 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOcon =
                              ({-# LINE 105 "src-ag/ResolveLocals.ag" #-}
                               _lhsIcon
                               {-# LINE 1718 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOinh =
                              ({-# LINE 104 "src-ag/ResolveLocals.ag" #-}
                               _lhsIinh
                               {-# LINE 1724 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOnt =
                              ({-# LINE 104 "src-ag/ResolveLocals.ag" #-}
                               _lhsInt
                               {-# LINE 1730 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOsyn =
                              ({-# LINE 104 "src-ag/ResolveLocals.ag" #-}
                               _lhsIsyn
                               {-# LINE 1736 "src-ag/ResolveLocals.hs" #-}
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
                          -- use rule "src-ag/ResolveLocals.ag"(line 44, column 16)
                          _lhsOerrors =
                              ({-# LINE 44 "src-ag/ResolveLocals.ag" #-}
                               Seq.empty
                               {-# LINE 1758 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- use rule "src-ag/ResolveLocals.ag"(line 93, column 86)
                          _lhsOinstVars =
                              ({-# LINE 93 "src-ag/ResolveLocals.ag" #-}
                               []
                               {-# LINE 1764 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- use rule "src-ag/ResolveLocals.ag"(line 93, column 48)
                          _lhsOlocVars =
                              ({-# LINE 93 "src-ag/ResolveLocals.ag" #-}
                               []
                               {-# LINE 1770 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- self rule
                          _copy =
                              ({-# LINE 22 "src-ag/Patterns.ag" #-}
                               []
                               {-# LINE 1776 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- self rule
                          _output =
                              ({-# LINE 47 "src-ag/ResolveLocals.ag" #-}
                               []
                               {-# LINE 1782 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- self rule
                          _lhsOcopy =
                              ({-# LINE 22 "src-ag/Patterns.ag" #-}
                               _copy
                               {-# LINE 1788 "src-ag/ResolveLocals.hs" #-}
                               )
                          -- self rule
                          _lhsOoutput =
                              ({-# LINE 47 "src-ag/ResolveLocals.ag" #-}
                               _output
                               {-# LINE 1794 "src-ag/ResolveLocals.hs" #-}
                               )
                      in  ( _lhsOcopy,_lhsOerrors,_lhsOinstVars,_lhsOlocVars,_lhsOoutput))) )
-- Production --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allnts               : [Identifier]
         inh                  : Attributes
         inhMap               : Map Identifier Attributes
         mergeMap             : Map ConstructorIdent (Map Identifier (Identifier,[Identifier]))
         nt                   : Identifier
         options              : Options
         syn                  : Attributes
         synMap               : Map Identifier Attributes
      synthesized attributes:
         cons                 : [ConstructorIdent]
         errors               : Seq Error
         output               : SELF 
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
sem_Production (Production _con _params _constraints _children _rules _typeSigs _macro )  =
    (sem_Production_Production _con _params _constraints (sem_Children _children ) (sem_Rules _rules ) (sem_TypeSigs _typeSigs ) _macro )
-- semantic domain
newtype T_Production  = T_Production (([Identifier]) ->
                                      Attributes ->
                                      (Map Identifier Attributes) ->
                                      (Map ConstructorIdent (Map Identifier (Identifier,[Identifier]))) ->
                                      Identifier ->
                                      Options ->
                                      Attributes ->
                                      (Map Identifier Attributes) ->
                                      ( ([ConstructorIdent]),(Seq Error),Production ))
data Inh_Production  = Inh_Production {allnts_Inh_Production :: ([Identifier]),inh_Inh_Production :: Attributes,inhMap_Inh_Production :: (Map Identifier Attributes),mergeMap_Inh_Production :: (Map ConstructorIdent (Map Identifier (Identifier,[Identifier]))),nt_Inh_Production :: Identifier,options_Inh_Production :: Options,syn_Inh_Production :: Attributes,synMap_Inh_Production :: (Map Identifier Attributes)}
data Syn_Production  = Syn_Production {cons_Syn_Production :: ([ConstructorIdent]),errors_Syn_Production :: (Seq Error),output_Syn_Production :: Production }
wrap_Production :: T_Production  ->
                   Inh_Production  ->
                   Syn_Production 
wrap_Production (T_Production sem ) (Inh_Production _lhsIallnts _lhsIinh _lhsIinhMap _lhsImergeMap _lhsInt _lhsIoptions _lhsIsyn _lhsIsynMap )  =
    (let ( _lhsOcons,_lhsOerrors,_lhsOoutput) = sem _lhsIallnts _lhsIinh _lhsIinhMap _lhsImergeMap _lhsInt _lhsIoptions _lhsIsyn _lhsIsynMap 
     in  (Syn_Production _lhsOcons _lhsOerrors _lhsOoutput ))
sem_Production_Production :: ConstructorIdent ->
                             ([Identifier]) ->
                             ([Type]) ->
                             T_Children  ->
                             T_Rules  ->
                             T_TypeSigs  ->
                             MaybeMacro ->
                             T_Production 
sem_Production_Production con_ params_ constraints_ (T_Children children_ ) (T_Rules rules_ ) (T_TypeSigs typeSigs_ ) macro_  =
    (T_Production (\ _lhsIallnts
                     _lhsIinh
                     _lhsIinhMap
                     _lhsImergeMap
                     _lhsInt
                     _lhsIoptions
                     _lhsIsyn
                     _lhsIsynMap ->
                       (let _lhsOcons :: ([ConstructorIdent])
                            _childrenOcon :: Identifier
                            _rulesOcon :: Identifier
                            _lhsOerrors :: (Seq Error)
                            _lhsOoutput :: Production 
                            _childrenOallfields :: ([(Identifier,Type,ChildKind)])
                            _childrenOallnts :: ([Identifier])
                            _childrenOattrs :: ([(Identifier,Identifier)])
                            _childrenOinh :: Attributes
                            _childrenOinhMap :: (Map Identifier Attributes)
                            _childrenOmergeMap :: (Map Identifier (Identifier,[Identifier]))
                            _childrenOnt :: Identifier
                            _childrenOsyn :: Attributes
                            _childrenOsynMap :: (Map Identifier Attributes)
                            _rulesOallfields :: ([(Identifier,Type,ChildKind)])
                            _rulesOallnts :: ([Identifier])
                            _rulesOattrs :: ([(Identifier,Identifier)])
                            _rulesOinh :: Attributes
                            _rulesOmergeMap :: (Map Identifier (Identifier,[Identifier]))
                            _rulesOnt :: Identifier
                            _rulesOoptions :: Options
                            _rulesOsyn :: Attributes
                            _childrenIattributes :: ([(Identifier,Attributes,Attributes)])
                            _childrenIfields :: ([(Identifier,Type,ChildKind)])
                            _childrenIoutput :: Children 
                            _rulesIerrors :: (Seq Error)
                            _rulesIinstVars :: ([Identifier])
                            _rulesIlocVars :: ([Identifier])
                            _rulesIoutput :: Rules 
                            _typeSigsIoutput :: TypeSigs 
                            -- "src-ag/ResolveLocals.ag"(line 67, column 18)
                            _lhsOcons =
                                ({-# LINE 67 "src-ag/ResolveLocals.ag" #-}
                                 [con_]
                                 {-# LINE 1904 "src-ag/ResolveLocals.hs" #-}
                                 )
                            -- "src-ag/ResolveLocals.ag"(line 74, column 16)
                            _allfields =
                                ({-# LINE 74 "src-ag/ResolveLocals.ag" #-}
                                 _childrenIfields
                                 {-# LINE 1910 "src-ag/ResolveLocals.hs" #-}
                                 )
                            -- "src-ag/ResolveLocals.ag"(line 74, column 16)
                            _attrs =
                                ({-# LINE 75 "src-ag/ResolveLocals.ag" #-}
                                 map ((,) _LOC)  _rulesIlocVars ++
                                 map ((,) _INST) _rulesIinstVars ++
                                 map ((,) _LHS)  _inhnames ++
                                 concat [map ((,) nm) (Map.keys as) | (nm,_,as) <- _childrenIattributes]
                                 {-# LINE 1919 "src-ag/ResolveLocals.hs" #-}
                                 )
                            -- "src-ag/ResolveLocals.ag"(line 74, column 16)
                            _inhnames =
                                ({-# LINE 79 "src-ag/ResolveLocals.ag" #-}
                                 Map.keys _lhsIinh
                                 {-# LINE 1925 "src-ag/ResolveLocals.hs" #-}
                                 )
                            -- "src-ag/ResolveLocals.ag"(line 74, column 16)
                            _synnames =
                                ({-# LINE 80 "src-ag/ResolveLocals.ag" #-}
                                 Map.keys _lhsIsyn
                                 {-# LINE 1931 "src-ag/ResolveLocals.hs" #-}
                                 )
                            -- "src-ag/ResolveLocals.ag"(line 108, column 16)
                            _childrenOcon =
                                ({-# LINE 108 "src-ag/ResolveLocals.ag" #-}
                                 con_
                                 {-# LINE 1937 "src-ag/ResolveLocals.hs" #-}
                                 )
                            -- "src-ag/ResolveLocals.ag"(line 110, column 16)
                            _rulesOcon =
                                ({-# LINE 110 "src-ag/ResolveLocals.ag" #-}
                                 con_
                                 {-# LINE 1943 "src-ag/ResolveLocals.hs" #-}
                                 )
                            -- "src-ag/ResolveLocals.ag"(line 129, column 32)
                            _mergeMap =
                                ({-# LINE 129 "src-ag/ResolveLocals.ag" #-}
                                 Map.findWithDefault Map.empty con_ _lhsImergeMap
                                 {-# LINE 1949 "src-ag/ResolveLocals.hs" #-}
                                 )
                            -- use rule "src-ag/ResolveLocals.ag"(line 44, column 16)
                            _lhsOerrors =
                                ({-# LINE 44 "src-ag/ResolveLocals.ag" #-}
                                 _rulesIerrors
                                 {-# LINE 1955 "src-ag/ResolveLocals.hs" #-}
                                 )
                            -- self rule
                            _output =
                                ({-# LINE 47 "src-ag/ResolveLocals.ag" #-}
                                 Production con_ params_ constraints_ _childrenIoutput _rulesIoutput _typeSigsIoutput macro_
                                 {-# LINE 1961 "src-ag/ResolveLocals.hs" #-}
                                 )
                            -- self rule
                            _lhsOoutput =
                                ({-# LINE 47 "src-ag/ResolveLocals.ag" #-}
                                 _output
                                 {-# LINE 1967 "src-ag/ResolveLocals.hs" #-}
                                 )
                            -- copy rule (from local)
                            _childrenOallfields =
                                ({-# LINE 71 "src-ag/ResolveLocals.ag" #-}
                                 _allfields
                                 {-# LINE 1973 "src-ag/ResolveLocals.hs" #-}
                                 )
                            -- copy rule (down)
                            _childrenOallnts =
                                ({-# LINE 57 "src-ag/ResolveLocals.ag" #-}
                                 _lhsIallnts
                                 {-# LINE 1979 "src-ag/ResolveLocals.hs" #-}
                                 )
                            -- copy rule (from local)
                            _childrenOattrs =
                                ({-# LINE 71 "src-ag/ResolveLocals.ag" #-}
                                 _attrs
                                 {-# LINE 1985 "src-ag/ResolveLocals.hs" #-}
                                 )
                            -- copy rule (down)
                            _childrenOinh =
                                ({-# LINE 104 "src-ag/ResolveLocals.ag" #-}
                                 _lhsIinh
                                 {-# LINE 1991 "src-ag/ResolveLocals.hs" #-}
                                 )
                            -- copy rule (down)
                            _childrenOinhMap =
                                ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                 _lhsIinhMap
                                 {-# LINE 1997 "src-ag/ResolveLocals.hs" #-}
                                 )
                            -- copy rule (from local)
                            _childrenOmergeMap =
                                ({-# LINE 131 "src-ag/ResolveLocals.ag" #-}
                                 _mergeMap
                                 {-# LINE 2003 "src-ag/ResolveLocals.hs" #-}
                                 )
                            -- copy rule (down)
                            _childrenOnt =
                                ({-# LINE 104 "src-ag/ResolveLocals.ag" #-}
                                 _lhsInt
                                 {-# LINE 2009 "src-ag/ResolveLocals.hs" #-}
                                 )
                            -- copy rule (down)
                            _childrenOsyn =
                                ({-# LINE 104 "src-ag/ResolveLocals.ag" #-}
                                 _lhsIsyn
                                 {-# LINE 2015 "src-ag/ResolveLocals.hs" #-}
                                 )
                            -- copy rule (down)
                            _childrenOsynMap =
                                ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                 _lhsIsynMap
                                 {-# LINE 2021 "src-ag/ResolveLocals.hs" #-}
                                 )
                            -- copy rule (from local)
                            _rulesOallfields =
                                ({-# LINE 71 "src-ag/ResolveLocals.ag" #-}
                                 _allfields
                                 {-# LINE 2027 "src-ag/ResolveLocals.hs" #-}
                                 )
                            -- copy rule (down)
                            _rulesOallnts =
                                ({-# LINE 57 "src-ag/ResolveLocals.ag" #-}
                                 _lhsIallnts
                                 {-# LINE 2033 "src-ag/ResolveLocals.hs" #-}
                                 )
                            -- copy rule (from local)
                            _rulesOattrs =
                                ({-# LINE 71 "src-ag/ResolveLocals.ag" #-}
                                 _attrs
                                 {-# LINE 2039 "src-ag/ResolveLocals.hs" #-}
                                 )
                            -- copy rule (down)
                            _rulesOinh =
                                ({-# LINE 104 "src-ag/ResolveLocals.ag" #-}
                                 _lhsIinh
                                 {-# LINE 2045 "src-ag/ResolveLocals.hs" #-}
                                 )
                            -- copy rule (from local)
                            _rulesOmergeMap =
                                ({-# LINE 131 "src-ag/ResolveLocals.ag" #-}
                                 _mergeMap
                                 {-# LINE 2051 "src-ag/ResolveLocals.hs" #-}
                                 )
                            -- copy rule (down)
                            _rulesOnt =
                                ({-# LINE 104 "src-ag/ResolveLocals.ag" #-}
                                 _lhsInt
                                 {-# LINE 2057 "src-ag/ResolveLocals.hs" #-}
                                 )
                            -- copy rule (down)
                            _rulesOoptions =
                                ({-# LINE 41 "src-ag/ResolveLocals.ag" #-}
                                 _lhsIoptions
                                 {-# LINE 2063 "src-ag/ResolveLocals.hs" #-}
                                 )
                            -- copy rule (down)
                            _rulesOsyn =
                                ({-# LINE 104 "src-ag/ResolveLocals.ag" #-}
                                 _lhsIsyn
                                 {-# LINE 2069 "src-ag/ResolveLocals.hs" #-}
                                 )
                            ( _childrenIattributes,_childrenIfields,_childrenIoutput) =
                                children_ _childrenOallfields _childrenOallnts _childrenOattrs _childrenOcon _childrenOinh _childrenOinhMap _childrenOmergeMap _childrenOnt _childrenOsyn _childrenOsynMap 
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
         inhMap               : Map Identifier Attributes
         mergeMap             : Map ConstructorIdent (Map Identifier (Identifier,[Identifier]))
         nt                   : Identifier
         options              : Options
         syn                  : Attributes
         synMap               : Map Identifier Attributes
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
                                        (Map Identifier Attributes) ->
                                        (Map ConstructorIdent (Map Identifier (Identifier,[Identifier]))) ->
                                        Identifier ->
                                        Options ->
                                        Attributes ->
                                        (Map Identifier Attributes) ->
                                        ( ([ConstructorIdent]),(Seq Error),Productions ))
data Inh_Productions  = Inh_Productions {allnts_Inh_Productions :: ([Identifier]),inh_Inh_Productions :: Attributes,inhMap_Inh_Productions :: (Map Identifier Attributes),mergeMap_Inh_Productions :: (Map ConstructorIdent (Map Identifier (Identifier,[Identifier]))),nt_Inh_Productions :: Identifier,options_Inh_Productions :: Options,syn_Inh_Productions :: Attributes,synMap_Inh_Productions :: (Map Identifier Attributes)}
data Syn_Productions  = Syn_Productions {cons_Syn_Productions :: ([ConstructorIdent]),errors_Syn_Productions :: (Seq Error),output_Syn_Productions :: Productions }
wrap_Productions :: T_Productions  ->
                    Inh_Productions  ->
                    Syn_Productions 
wrap_Productions (T_Productions sem ) (Inh_Productions _lhsIallnts _lhsIinh _lhsIinhMap _lhsImergeMap _lhsInt _lhsIoptions _lhsIsyn _lhsIsynMap )  =
    (let ( _lhsOcons,_lhsOerrors,_lhsOoutput) = sem _lhsIallnts _lhsIinh _lhsIinhMap _lhsImergeMap _lhsInt _lhsIoptions _lhsIsyn _lhsIsynMap 
     in  (Syn_Productions _lhsOcons _lhsOerrors _lhsOoutput ))
sem_Productions_Cons :: T_Production  ->
                        T_Productions  ->
                        T_Productions 
sem_Productions_Cons (T_Production hd_ ) (T_Productions tl_ )  =
    (T_Productions (\ _lhsIallnts
                      _lhsIinh
                      _lhsIinhMap
                      _lhsImergeMap
                      _lhsInt
                      _lhsIoptions
                      _lhsIsyn
                      _lhsIsynMap ->
                        (let _lhsOcons :: ([ConstructorIdent])
                             _lhsOerrors :: (Seq Error)
                             _lhsOoutput :: Productions 
                             _hdOallnts :: ([Identifier])
                             _hdOinh :: Attributes
                             _hdOinhMap :: (Map Identifier Attributes)
                             _hdOmergeMap :: (Map ConstructorIdent (Map Identifier (Identifier,[Identifier])))
                             _hdOnt :: Identifier
                             _hdOoptions :: Options
                             _hdOsyn :: Attributes
                             _hdOsynMap :: (Map Identifier Attributes)
                             _tlOallnts :: ([Identifier])
                             _tlOinh :: Attributes
                             _tlOinhMap :: (Map Identifier Attributes)
                             _tlOmergeMap :: (Map ConstructorIdent (Map Identifier (Identifier,[Identifier])))
                             _tlOnt :: Identifier
                             _tlOoptions :: Options
                             _tlOsyn :: Attributes
                             _tlOsynMap :: (Map Identifier Attributes)
                             _hdIcons :: ([ConstructorIdent])
                             _hdIerrors :: (Seq Error)
                             _hdIoutput :: Production 
                             _tlIcons :: ([ConstructorIdent])
                             _tlIerrors :: (Seq Error)
                             _tlIoutput :: Productions 
                             -- use rule "src-ag/ResolveLocals.ag"(line 65, column 40)
                             _lhsOcons =
                                 ({-# LINE 65 "src-ag/ResolveLocals.ag" #-}
                                  _hdIcons ++ _tlIcons
                                  {-# LINE 2168 "src-ag/ResolveLocals.hs" #-}
                                  )
                             -- use rule "src-ag/ResolveLocals.ag"(line 44, column 16)
                             _lhsOerrors =
                                 ({-# LINE 44 "src-ag/ResolveLocals.ag" #-}
                                  _hdIerrors Seq.>< _tlIerrors
                                  {-# LINE 2174 "src-ag/ResolveLocals.hs" #-}
                                  )
                             -- self rule
                             _output =
                                 ({-# LINE 47 "src-ag/ResolveLocals.ag" #-}
                                  (:) _hdIoutput _tlIoutput
                                  {-# LINE 2180 "src-ag/ResolveLocals.hs" #-}
                                  )
                             -- self rule
                             _lhsOoutput =
                                 ({-# LINE 47 "src-ag/ResolveLocals.ag" #-}
                                  _output
                                  {-# LINE 2186 "src-ag/ResolveLocals.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOallnts =
                                 ({-# LINE 57 "src-ag/ResolveLocals.ag" #-}
                                  _lhsIallnts
                                  {-# LINE 2192 "src-ag/ResolveLocals.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOinh =
                                 ({-# LINE 104 "src-ag/ResolveLocals.ag" #-}
                                  _lhsIinh
                                  {-# LINE 2198 "src-ag/ResolveLocals.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOinhMap =
                                 ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                  _lhsIinhMap
                                  {-# LINE 2204 "src-ag/ResolveLocals.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOmergeMap =
                                 ({-# LINE 126 "src-ag/ResolveLocals.ag" #-}
                                  _lhsImergeMap
                                  {-# LINE 2210 "src-ag/ResolveLocals.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOnt =
                                 ({-# LINE 104 "src-ag/ResolveLocals.ag" #-}
                                  _lhsInt
                                  {-# LINE 2216 "src-ag/ResolveLocals.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOoptions =
                                 ({-# LINE 41 "src-ag/ResolveLocals.ag" #-}
                                  _lhsIoptions
                                  {-# LINE 2222 "src-ag/ResolveLocals.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOsyn =
                                 ({-# LINE 104 "src-ag/ResolveLocals.ag" #-}
                                  _lhsIsyn
                                  {-# LINE 2228 "src-ag/ResolveLocals.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOsynMap =
                                 ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                  _lhsIsynMap
                                  {-# LINE 2234 "src-ag/ResolveLocals.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOallnts =
                                 ({-# LINE 57 "src-ag/ResolveLocals.ag" #-}
                                  _lhsIallnts
                                  {-# LINE 2240 "src-ag/ResolveLocals.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOinh =
                                 ({-# LINE 104 "src-ag/ResolveLocals.ag" #-}
                                  _lhsIinh
                                  {-# LINE 2246 "src-ag/ResolveLocals.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOinhMap =
                                 ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                  _lhsIinhMap
                                  {-# LINE 2252 "src-ag/ResolveLocals.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOmergeMap =
                                 ({-# LINE 126 "src-ag/ResolveLocals.ag" #-}
                                  _lhsImergeMap
                                  {-# LINE 2258 "src-ag/ResolveLocals.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOnt =
                                 ({-# LINE 104 "src-ag/ResolveLocals.ag" #-}
                                  _lhsInt
                                  {-# LINE 2264 "src-ag/ResolveLocals.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOoptions =
                                 ({-# LINE 41 "src-ag/ResolveLocals.ag" #-}
                                  _lhsIoptions
                                  {-# LINE 2270 "src-ag/ResolveLocals.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOsyn =
                                 ({-# LINE 104 "src-ag/ResolveLocals.ag" #-}
                                  _lhsIsyn
                                  {-# LINE 2276 "src-ag/ResolveLocals.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOsynMap =
                                 ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                  _lhsIsynMap
                                  {-# LINE 2282 "src-ag/ResolveLocals.hs" #-}
                                  )
                             ( _hdIcons,_hdIerrors,_hdIoutput) =
                                 hd_ _hdOallnts _hdOinh _hdOinhMap _hdOmergeMap _hdOnt _hdOoptions _hdOsyn _hdOsynMap 
                             ( _tlIcons,_tlIerrors,_tlIoutput) =
                                 tl_ _tlOallnts _tlOinh _tlOinhMap _tlOmergeMap _tlOnt _tlOoptions _tlOsyn _tlOsynMap 
                         in  ( _lhsOcons,_lhsOerrors,_lhsOoutput))) )
sem_Productions_Nil :: T_Productions 
sem_Productions_Nil  =
    (T_Productions (\ _lhsIallnts
                      _lhsIinh
                      _lhsIinhMap
                      _lhsImergeMap
                      _lhsInt
                      _lhsIoptions
                      _lhsIsyn
                      _lhsIsynMap ->
                        (let _lhsOcons :: ([ConstructorIdent])
                             _lhsOerrors :: (Seq Error)
                             _lhsOoutput :: Productions 
                             -- use rule "src-ag/ResolveLocals.ag"(line 65, column 40)
                             _lhsOcons =
                                 ({-# LINE 65 "src-ag/ResolveLocals.ag" #-}
                                  []
                                  {-# LINE 2306 "src-ag/ResolveLocals.hs" #-}
                                  )
                             -- use rule "src-ag/ResolveLocals.ag"(line 44, column 16)
                             _lhsOerrors =
                                 ({-# LINE 44 "src-ag/ResolveLocals.ag" #-}
                                  Seq.empty
                                  {-# LINE 2312 "src-ag/ResolveLocals.hs" #-}
                                  )
                             -- self rule
                             _output =
                                 ({-# LINE 47 "src-ag/ResolveLocals.ag" #-}
                                  []
                                  {-# LINE 2318 "src-ag/ResolveLocals.hs" #-}
                                  )
                             -- self rule
                             _lhsOoutput =
                                 ({-# LINE 47 "src-ag/ResolveLocals.ag" #-}
                                  _output
                                  {-# LINE 2324 "src-ag/ResolveLocals.hs" #-}
                                  )
                         in  ( _lhsOcons,_lhsOerrors,_lhsOoutput))) )
-- Rule --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allfields            : [(Identifier,Type,ChildKind)]
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
         child pure           : {Bool}
         child identity       : {Bool}
         child mbError        : {Maybe Error}
         child eager          : {Bool}
         visit 0:
            local output      : _
-}
-- cata
sem_Rule :: Rule  ->
            T_Rule 
sem_Rule (Rule _mbName _pattern _rhs _owrt _origin _explicit _pure _identity _mbError _eager )  =
    (sem_Rule_Rule _mbName (sem_Pattern _pattern ) (sem_Expression _rhs ) _owrt _origin _explicit _pure _identity _mbError _eager )
-- semantic domain
newtype T_Rule  = T_Rule (([(Identifier,Type,ChildKind)]) ->
                          ([Identifier]) ->
                          ([(Identifier,Identifier)]) ->
                          Identifier ->
                          Attributes ->
                          (Map Identifier (Identifier,[Identifier])) ->
                          Identifier ->
                          Options ->
                          Attributes ->
                          ( (Seq Error),([Identifier]),([Identifier]),Rule ))
data Inh_Rule  = Inh_Rule {allfields_Inh_Rule :: ([(Identifier,Type,ChildKind)]),allnts_Inh_Rule :: ([Identifier]),attrs_Inh_Rule :: ([(Identifier,Identifier)]),con_Inh_Rule :: Identifier,inh_Inh_Rule :: Attributes,mergeMap_Inh_Rule :: (Map Identifier (Identifier,[Identifier])),nt_Inh_Rule :: Identifier,options_Inh_Rule :: Options,syn_Inh_Rule :: Attributes}
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
                 Bool ->
                 Bool ->
                 (Maybe Error) ->
                 Bool ->
                 T_Rule 
sem_Rule_Rule mbName_ (T_Pattern pattern_ ) (T_Expression rhs_ ) owrt_ origin_ explicit_ pure_ identity_ mbError_ eager_  =
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
                      _rhsOallfields :: ([(Identifier,Type,ChildKind)])
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
                      -- use rule "src-ag/ResolveLocals.ag"(line 44, column 16)
                      _lhsOerrors =
                          ({-# LINE 44 "src-ag/ResolveLocals.ag" #-}
                           _patternIerrors Seq.>< _rhsIerrors
                           {-# LINE 2431 "src-ag/ResolveLocals.hs" #-}
                           )
                      -- use rule "src-ag/ResolveLocals.ag"(line 93, column 86)
                      _lhsOinstVars =
                          ({-# LINE 93 "src-ag/ResolveLocals.ag" #-}
                           _patternIinstVars
                           {-# LINE 2437 "src-ag/ResolveLocals.hs" #-}
                           )
                      -- use rule "src-ag/ResolveLocals.ag"(line 93, column 48)
                      _lhsOlocVars =
                          ({-# LINE 93 "src-ag/ResolveLocals.ag" #-}
                           _patternIlocVars
                           {-# LINE 2443 "src-ag/ResolveLocals.hs" #-}
                           )
                      -- self rule
                      _output =
                          ({-# LINE 47 "src-ag/ResolveLocals.ag" #-}
                           Rule mbName_ _patternIoutput _rhsIoutput owrt_ origin_ explicit_ pure_ identity_ mbError_ eager_
                           {-# LINE 2449 "src-ag/ResolveLocals.hs" #-}
                           )
                      -- self rule
                      _lhsOoutput =
                          ({-# LINE 47 "src-ag/ResolveLocals.ag" #-}
                           _output
                           {-# LINE 2455 "src-ag/ResolveLocals.hs" #-}
                           )
                      -- copy rule (down)
                      _patternOcon =
                          ({-# LINE 105 "src-ag/ResolveLocals.ag" #-}
                           _lhsIcon
                           {-# LINE 2461 "src-ag/ResolveLocals.hs" #-}
                           )
                      -- copy rule (down)
                      _patternOinh =
                          ({-# LINE 104 "src-ag/ResolveLocals.ag" #-}
                           _lhsIinh
                           {-# LINE 2467 "src-ag/ResolveLocals.hs" #-}
                           )
                      -- copy rule (down)
                      _patternOnt =
                          ({-# LINE 104 "src-ag/ResolveLocals.ag" #-}
                           _lhsInt
                           {-# LINE 2473 "src-ag/ResolveLocals.hs" #-}
                           )
                      -- copy rule (down)
                      _patternOsyn =
                          ({-# LINE 104 "src-ag/ResolveLocals.ag" #-}
                           _lhsIsyn
                           {-# LINE 2479 "src-ag/ResolveLocals.hs" #-}
                           )
                      -- copy rule (down)
                      _rhsOallfields =
                          ({-# LINE 139 "src-ag/ResolveLocals.ag" #-}
                           _lhsIallfields
                           {-# LINE 2485 "src-ag/ResolveLocals.hs" #-}
                           )
                      -- copy rule (down)
                      _rhsOallnts =
                          ({-# LINE 140 "src-ag/ResolveLocals.ag" #-}
                           _lhsIallnts
                           {-# LINE 2491 "src-ag/ResolveLocals.hs" #-}
                           )
                      -- copy rule (down)
                      _rhsOattrs =
                          ({-# LINE 141 "src-ag/ResolveLocals.ag" #-}
                           _lhsIattrs
                           {-# LINE 2497 "src-ag/ResolveLocals.hs" #-}
                           )
                      -- copy rule (down)
                      _rhsOcon =
                          ({-# LINE 138 "src-ag/ResolveLocals.ag" #-}
                           _lhsIcon
                           {-# LINE 2503 "src-ag/ResolveLocals.hs" #-}
                           )
                      -- copy rule (down)
                      _rhsOmergeMap =
                          ({-# LINE 131 "src-ag/ResolveLocals.ag" #-}
                           _lhsImergeMap
                           {-# LINE 2509 "src-ag/ResolveLocals.hs" #-}
                           )
                      -- copy rule (down)
                      _rhsOnt =
                          ({-# LINE 138 "src-ag/ResolveLocals.ag" #-}
                           _lhsInt
                           {-# LINE 2515 "src-ag/ResolveLocals.hs" #-}
                           )
                      -- copy rule (down)
                      _rhsOoptions =
                          ({-# LINE 41 "src-ag/ResolveLocals.ag" #-}
                           _lhsIoptions
                           {-# LINE 2521 "src-ag/ResolveLocals.hs" #-}
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
         allfields            : [(Identifier,Type,ChildKind)]
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
newtype T_Rules  = T_Rules (([(Identifier,Type,ChildKind)]) ->
                            ([Identifier]) ->
                            ([(Identifier,Identifier)]) ->
                            Identifier ->
                            Attributes ->
                            (Map Identifier (Identifier,[Identifier])) ->
                            Identifier ->
                            Options ->
                            Attributes ->
                            ( (Seq Error),([Identifier]),([Identifier]),Rules ))
data Inh_Rules  = Inh_Rules {allfields_Inh_Rules :: ([(Identifier,Type,ChildKind)]),allnts_Inh_Rules :: ([Identifier]),attrs_Inh_Rules :: ([(Identifier,Identifier)]),con_Inh_Rules :: Identifier,inh_Inh_Rules :: Attributes,mergeMap_Inh_Rules :: (Map Identifier (Identifier,[Identifier])),nt_Inh_Rules :: Identifier,options_Inh_Rules :: Options,syn_Inh_Rules :: Attributes}
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
                       _hdOallfields :: ([(Identifier,Type,ChildKind)])
                       _hdOallnts :: ([Identifier])
                       _hdOattrs :: ([(Identifier,Identifier)])
                       _hdOcon :: Identifier
                       _hdOinh :: Attributes
                       _hdOmergeMap :: (Map Identifier (Identifier,[Identifier]))
                       _hdOnt :: Identifier
                       _hdOoptions :: Options
                       _hdOsyn :: Attributes
                       _tlOallfields :: ([(Identifier,Type,ChildKind)])
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
                       -- use rule "src-ag/ResolveLocals.ag"(line 44, column 16)
                       _lhsOerrors =
                           ({-# LINE 44 "src-ag/ResolveLocals.ag" #-}
                            _hdIerrors Seq.>< _tlIerrors
                            {-# LINE 2627 "src-ag/ResolveLocals.hs" #-}
                            )
                       -- use rule "src-ag/ResolveLocals.ag"(line 93, column 86)
                       _lhsOinstVars =
                           ({-# LINE 93 "src-ag/ResolveLocals.ag" #-}
                            _hdIinstVars ++ _tlIinstVars
                            {-# LINE 2633 "src-ag/ResolveLocals.hs" #-}
                            )
                       -- use rule "src-ag/ResolveLocals.ag"(line 93, column 48)
                       _lhsOlocVars =
                           ({-# LINE 93 "src-ag/ResolveLocals.ag" #-}
                            _hdIlocVars ++ _tlIlocVars
                            {-# LINE 2639 "src-ag/ResolveLocals.hs" #-}
                            )
                       -- self rule
                       _output =
                           ({-# LINE 47 "src-ag/ResolveLocals.ag" #-}
                            (:) _hdIoutput _tlIoutput
                            {-# LINE 2645 "src-ag/ResolveLocals.hs" #-}
                            )
                       -- self rule
                       _lhsOoutput =
                           ({-# LINE 47 "src-ag/ResolveLocals.ag" #-}
                            _output
                            {-# LINE 2651 "src-ag/ResolveLocals.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOallfields =
                           ({-# LINE 71 "src-ag/ResolveLocals.ag" #-}
                            _lhsIallfields
                            {-# LINE 2657 "src-ag/ResolveLocals.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOallnts =
                           ({-# LINE 57 "src-ag/ResolveLocals.ag" #-}
                            _lhsIallnts
                            {-# LINE 2663 "src-ag/ResolveLocals.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOattrs =
                           ({-# LINE 71 "src-ag/ResolveLocals.ag" #-}
                            _lhsIattrs
                            {-# LINE 2669 "src-ag/ResolveLocals.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOcon =
                           ({-# LINE 105 "src-ag/ResolveLocals.ag" #-}
                            _lhsIcon
                            {-# LINE 2675 "src-ag/ResolveLocals.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOinh =
                           ({-# LINE 104 "src-ag/ResolveLocals.ag" #-}
                            _lhsIinh
                            {-# LINE 2681 "src-ag/ResolveLocals.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOmergeMap =
                           ({-# LINE 131 "src-ag/ResolveLocals.ag" #-}
                            _lhsImergeMap
                            {-# LINE 2687 "src-ag/ResolveLocals.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOnt =
                           ({-# LINE 104 "src-ag/ResolveLocals.ag" #-}
                            _lhsInt
                            {-# LINE 2693 "src-ag/ResolveLocals.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOoptions =
                           ({-# LINE 41 "src-ag/ResolveLocals.ag" #-}
                            _lhsIoptions
                            {-# LINE 2699 "src-ag/ResolveLocals.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOsyn =
                           ({-# LINE 104 "src-ag/ResolveLocals.ag" #-}
                            _lhsIsyn
                            {-# LINE 2705 "src-ag/ResolveLocals.hs" #-}
                            )
                       -- copy rule (down)
                       _tlOallfields =
                           ({-# LINE 71 "src-ag/ResolveLocals.ag" #-}
                            _lhsIallfields
                            {-# LINE 2711 "src-ag/ResolveLocals.hs" #-}
                            )
                       -- copy rule (down)
                       _tlOallnts =
                           ({-# LINE 57 "src-ag/ResolveLocals.ag" #-}
                            _lhsIallnts
                            {-# LINE 2717 "src-ag/ResolveLocals.hs" #-}
                            )
                       -- copy rule (down)
                       _tlOattrs =
                           ({-# LINE 71 "src-ag/ResolveLocals.ag" #-}
                            _lhsIattrs
                            {-# LINE 2723 "src-ag/ResolveLocals.hs" #-}
                            )
                       -- copy rule (down)
                       _tlOcon =
                           ({-# LINE 105 "src-ag/ResolveLocals.ag" #-}
                            _lhsIcon
                            {-# LINE 2729 "src-ag/ResolveLocals.hs" #-}
                            )
                       -- copy rule (down)
                       _tlOinh =
                           ({-# LINE 104 "src-ag/ResolveLocals.ag" #-}
                            _lhsIinh
                            {-# LINE 2735 "src-ag/ResolveLocals.hs" #-}
                            )
                       -- copy rule (down)
                       _tlOmergeMap =
                           ({-# LINE 131 "src-ag/ResolveLocals.ag" #-}
                            _lhsImergeMap
                            {-# LINE 2741 "src-ag/ResolveLocals.hs" #-}
                            )
                       -- copy rule (down)
                       _tlOnt =
                           ({-# LINE 104 "src-ag/ResolveLocals.ag" #-}
                            _lhsInt
                            {-# LINE 2747 "src-ag/ResolveLocals.hs" #-}
                            )
                       -- copy rule (down)
                       _tlOoptions =
                           ({-# LINE 41 "src-ag/ResolveLocals.ag" #-}
                            _lhsIoptions
                            {-# LINE 2753 "src-ag/ResolveLocals.hs" #-}
                            )
                       -- copy rule (down)
                       _tlOsyn =
                           ({-# LINE 104 "src-ag/ResolveLocals.ag" #-}
                            _lhsIsyn
                            {-# LINE 2759 "src-ag/ResolveLocals.hs" #-}
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
                       -- use rule "src-ag/ResolveLocals.ag"(line 44, column 16)
                       _lhsOerrors =
                           ({-# LINE 44 "src-ag/ResolveLocals.ag" #-}
                            Seq.empty
                            {-# LINE 2785 "src-ag/ResolveLocals.hs" #-}
                            )
                       -- use rule "src-ag/ResolveLocals.ag"(line 93, column 86)
                       _lhsOinstVars =
                           ({-# LINE 93 "src-ag/ResolveLocals.ag" #-}
                            []
                            {-# LINE 2791 "src-ag/ResolveLocals.hs" #-}
                            )
                       -- use rule "src-ag/ResolveLocals.ag"(line 93, column 48)
                       _lhsOlocVars =
                           ({-# LINE 93 "src-ag/ResolveLocals.ag" #-}
                            []
                            {-# LINE 2797 "src-ag/ResolveLocals.hs" #-}
                            )
                       -- self rule
                       _output =
                           ({-# LINE 47 "src-ag/ResolveLocals.ag" #-}
                            []
                            {-# LINE 2803 "src-ag/ResolveLocals.hs" #-}
                            )
                       -- self rule
                       _lhsOoutput =
                           ({-# LINE 47 "src-ag/ResolveLocals.ag" #-}
                            _output
                            {-# LINE 2809 "src-ag/ResolveLocals.hs" #-}
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
                        ({-# LINE 47 "src-ag/ResolveLocals.ag" #-}
                         TypeSig name_ tp_
                         {-# LINE 2848 "src-ag/ResolveLocals.hs" #-}
                         )
                    -- self rule
                    _lhsOoutput =
                        ({-# LINE 47 "src-ag/ResolveLocals.ag" #-}
                         _output
                         {-# LINE 2854 "src-ag/ResolveLocals.hs" #-}
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
                         ({-# LINE 47 "src-ag/ResolveLocals.ag" #-}
                          (:) _hdIoutput _tlIoutput
                          {-# LINE 2898 "src-ag/ResolveLocals.hs" #-}
                          )
                     -- self rule
                     _lhsOoutput =
                         ({-# LINE 47 "src-ag/ResolveLocals.ag" #-}
                          _output
                          {-# LINE 2904 "src-ag/ResolveLocals.hs" #-}
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
                         ({-# LINE 47 "src-ag/ResolveLocals.ag" #-}
                          []
                          {-# LINE 2918 "src-ag/ResolveLocals.hs" #-}
                          )
                     -- self rule
                     _lhsOoutput =
                         ({-# LINE 47 "src-ag/ResolveLocals.ag" #-}
                          _output
                          {-# LINE 2924 "src-ag/ResolveLocals.hs" #-}
                          )
                 in  ( _lhsOoutput)) )