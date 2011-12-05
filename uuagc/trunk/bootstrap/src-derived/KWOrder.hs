

-- UUAGC 0.9.39.1.0 (src-ag/KWOrder.ag)
module KWOrder where
{-# LINE 7 "src-ag/KWOrder.ag" #-}

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
import Data.Set(Set)
import Data.Map(Map)
import Data.Sequence(Seq)
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Data.Monoid(mappend,mempty)
{-# LINE 27 "dist/build/uuagc/uuagc-tmp/KWOrder.hs" #-}

{-# LINE 2 "src-ag/AbstractSyntax.ag" #-}

-- AbstractSyntax.ag imports
import Data.Set(Set)
import Data.Map(Map)
import Patterns    (Pattern(..),Patterns)
import Expression  (Expression(..))
import Macro --marcos
import CommonTypes
import ErrorMessages
{-# LINE 39 "dist/build/uuagc/uuagc-tmp/KWOrder.hs" #-}

{-# LINE 2 "src-ag/HsToken.ag" #-}

import CommonTypes
import UU.Scanner.Position(Pos)
{-# LINE 45 "dist/build/uuagc/uuagc-tmp/KWOrder.hs" #-}

{-# LINE 2 "src-ag/Expression.ag" #-}

import UU.Scanner.Position(Pos)
import HsToken
{-# LINE 51 "dist/build/uuagc/uuagc-tmp/KWOrder.hs" #-}

{-# LINE 2 "src-ag/Patterns.ag" #-}

-- Patterns.ag imports
import UU.Scanner.Position(Pos)
import CommonTypes (ConstructorIdent,Identifier)
{-# LINE 58 "dist/build/uuagc/uuagc-tmp/KWOrder.hs" #-}
{-# LINE 274 "src-ag/KWOrder.ag" #-}

-- a depends on b, thus a is a successor of b
depToEdge :: Dependency -> Edge
depToEdge (Dependency a b) = (occToVertex False b, occToVertex True a)

occToVertex :: Bool -> Occurrence -> Vertex
occToVertex _ (OccRule nm) = VRule nm
occToVertex isDependency (OccAttr c a)
  | c == _LOC  = VAttr Syn c a   -- local attributes are treated as synthesized attrs of 'loc'
  | c == _INST = VChild a        -- higher-order attributes are treated as children
  | otherwise  = VAttr kind c a where
      kind | isDependency && c == _LHS     = Inh     -- these dependencies have the property that
           | isDependency && c /= _LHS     = Syn     -- they can all be faked by writing a 'const' rule
           | not isDependency && c == _LHS = Syn     -- Perhaps we should also allow other forms of dependencies
           | not isDependency && c /= _LHS = Inh     -- as well, such as two inherited attributes, which would
                                                     -- force them in different visits
{-# LINE 76 "dist/build/uuagc/uuagc-tmp/KWOrder.hs" #-}
-- Child -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         aroundMap            : Map Identifier [Expression]
         inhMap               : Map Identifier Attributes
         mergeMap             : Map Identifier (Identifier, [Identifier], Expression)
         mergedChildren       : Set Identifier
         options              : Options
         synMap               : Map Identifier Attributes
      synthesized attributes:
         echilds              : EChild
         edges                : Set.Set Edge
         nontnames            : [(Identifier, Identifier)]
         refHoNts             : Set NontermIdent
         refNts               : Set NontermIdent
         vertices             : Set.Set Vertex
   alternatives:
      alternative Child:
         child name           : {Identifier}
         child tp             : {Type}
         child kind           : {ChildKind}
         visit 0:
            local refNts      : _
            local refHoNts    : _
            local isHigherOrder : _
            local hasArounds  : _
            local merges      : _
            local isMerged    : _
            local vertex      : _
            local synvertices : _
            local inhvertices : _
            local childIsDeforested : _
            local higherOrderEdges : _
            local aroundEdges : _
            local edgesout    : _
            local edgesin     : _
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
newtype T_Child  = T_Child ((Map Identifier [Expression]) ->
                            (Map Identifier Attributes) ->
                            (Map Identifier (Identifier, [Identifier], Expression)) ->
                            (Set Identifier) ->
                            Options ->
                            (Map Identifier Attributes) ->
                            ( EChild,(Set.Set Edge),([(Identifier, Identifier)]),(Set NontermIdent),(Set NontermIdent),(Set.Set Vertex)))
data Inh_Child  = Inh_Child {aroundMap_Inh_Child :: (Map Identifier [Expression]),inhMap_Inh_Child :: (Map Identifier Attributes),mergeMap_Inh_Child :: (Map Identifier (Identifier, [Identifier], Expression)),mergedChildren_Inh_Child :: (Set Identifier),options_Inh_Child :: Options,synMap_Inh_Child :: (Map Identifier Attributes)}
data Syn_Child  = Syn_Child {echilds_Syn_Child :: EChild,edges_Syn_Child :: (Set.Set Edge),nontnames_Syn_Child :: ([(Identifier, Identifier)]),refHoNts_Syn_Child :: (Set NontermIdent),refNts_Syn_Child :: (Set NontermIdent),vertices_Syn_Child :: (Set.Set Vertex)}
wrap_Child :: T_Child  ->
              Inh_Child  ->
              Syn_Child 
wrap_Child (T_Child sem ) (Inh_Child _lhsIaroundMap _lhsIinhMap _lhsImergeMap _lhsImergedChildren _lhsIoptions _lhsIsynMap )  =
    (let ( _lhsOechilds,_lhsOedges,_lhsOnontnames,_lhsOrefHoNts,_lhsOrefNts,_lhsOvertices) = sem _lhsIaroundMap _lhsIinhMap _lhsImergeMap _lhsImergedChildren _lhsIoptions _lhsIsynMap 
     in  (Syn_Child _lhsOechilds _lhsOedges _lhsOnontnames _lhsOrefHoNts _lhsOrefNts _lhsOvertices ))
sem_Child_Child :: Identifier ->
                   Type ->
                   ChildKind ->
                   T_Child 
sem_Child_Child name_ tp_ kind_  =
    (T_Child (\ _lhsIaroundMap
                _lhsIinhMap
                _lhsImergeMap
                _lhsImergedChildren
                _lhsIoptions
                _lhsIsynMap ->
                  (let _lhsOechilds :: EChild
                       _lhsOvertices :: (Set.Set Vertex)
                       _lhsOedges :: (Set.Set Edge)
                       _lhsOnontnames :: ([(Identifier, Identifier)])
                       _lhsOrefHoNts :: (Set NontermIdent)
                       _lhsOrefNts :: (Set NontermIdent)
                       -- "src-ag/KWOrder.ag"(line 73, column 3)
                       _refNts =
                           ({-# LINE 73 "src-ag/KWOrder.ag" #-}
                            case tp_ of
                              NT nt _ _ -> Set.singleton nt
                              _         -> mempty
                            {-# LINE 162 "src-ag/KWOrder.hs" #-}
                            )
                       -- "src-ag/KWOrder.ag"(line 76, column 3)
                       _refHoNts =
                           ({-# LINE 76 "src-ag/KWOrder.ag" #-}
                            if _isHigherOrder     then _refNts     else mempty
                            {-# LINE 168 "src-ag/KWOrder.hs" #-}
                            )
                       -- "src-ag/KWOrder.ag"(line 77, column 3)
                       _isHigherOrder =
                           ({-# LINE 77 "src-ag/KWOrder.ag" #-}
                            case kind_ of
                              ChildSyntax -> False
                              _           -> True
                            {-# LINE 176 "src-ag/KWOrder.hs" #-}
                            )
                       -- "src-ag/KWOrder.ag"(line 107, column 3)
                       _hasArounds =
                           ({-# LINE 107 "src-ag/KWOrder.ag" #-}
                            case Map.lookup name_ _lhsIaroundMap of
                              Nothing -> False
                              Just as -> not (null as)
                            {-# LINE 184 "src-ag/KWOrder.hs" #-}
                            )
                       -- "src-ag/KWOrder.ag"(line 135, column 3)
                       _merges =
                           ({-# LINE 135 "src-ag/KWOrder.ag" #-}
                            maybe Nothing (\(_,ms,_) -> Just ms) $ Map.lookup name_ _lhsImergeMap
                            {-# LINE 190 "src-ag/KWOrder.hs" #-}
                            )
                       -- "src-ag/KWOrder.ag"(line 136, column 3)
                       _isMerged =
                           ({-# LINE 136 "src-ag/KWOrder.ag" #-}
                            name_ `Set.member` _lhsImergedChildren
                            {-# LINE 196 "src-ag/KWOrder.hs" #-}
                            )
                       -- "src-ag/KWOrder.ag"(line 177, column 11)
                       _lhsOechilds =
                           ({-# LINE 177 "src-ag/KWOrder.ag" #-}
                            case tp_ of
                              NT _ _ _ -> EChild name_ tp_ kind_ _hasArounds     _merges     _isMerged
                              _        -> ETerm name_ tp_
                            {-# LINE 204 "src-ag/KWOrder.hs" #-}
                            )
                       -- "src-ag/KWOrder.ag"(line 214, column 12)
                       _vertex =
                           ({-# LINE 214 "src-ag/KWOrder.ag" #-}
                            VChild name_
                            {-# LINE 210 "src-ag/KWOrder.hs" #-}
                            )
                       -- "src-ag/KWOrder.ag"(line 215, column 12)
                       _synvertices =
                           ({-# LINE 215 "src-ag/KWOrder.ag" #-}
                            map (VAttr Syn name_) . Map.keys $ _syn
                            {-# LINE 216 "src-ag/KWOrder.hs" #-}
                            )
                       -- "src-ag/KWOrder.ag"(line 216, column 12)
                       _inhvertices =
                           ({-# LINE 216 "src-ag/KWOrder.ag" #-}
                            map (VAttr Inh name_) . Map.keys $ _inh
                            {-# LINE 222 "src-ag/KWOrder.hs" #-}
                            )
                       -- "src-ag/KWOrder.ag"(line 217, column 12)
                       _lhsOvertices =
                           ({-# LINE 217 "src-ag/KWOrder.ag" #-}
                            case tp_ of
                               NT _ _ _ -> Set.insert _vertex     $ Set.fromList (_synvertices     ++ _inhvertices    )
                               _        -> Set.empty
                            {-# LINE 230 "src-ag/KWOrder.hs" #-}
                            )
                       -- "src-ag/KWOrder.ag"(line 247, column 3)
                       _childIsDeforested =
                           ({-# LINE 247 "src-ag/KWOrder.ag" #-}
                            case tp_ of
                              NT _ _ defor -> defor
                              _            -> False
                            {-# LINE 238 "src-ag/KWOrder.hs" #-}
                            )
                       -- "src-ag/KWOrder.ag"(line 250, column 3)
                       _higherOrderEdges =
                           ({-# LINE 250 "src-ag/KWOrder.ag" #-}
                            case kind_ of
                              ChildAttr | lateHigherOrderBinding _lhsIoptions && not _childIsDeforested
                                           -> [(_vertex    , VAttr Inh _LHS idLateBindingAttr)]
                              _            -> []
                            {-# LINE 247 "src-ag/KWOrder.hs" #-}
                            )
                       -- "src-ag/KWOrder.ag"(line 254, column 3)
                       _aroundEdges =
                           ({-# LINE 254 "src-ag/KWOrder.ag" #-}
                            if _hasArounds
                            then [(_vertex    , VAttr Syn _LOC (Ident (getName name_ ++ "_around") (getPos name_)))]
                            else []
                            {-# LINE 255 "src-ag/KWOrder.hs" #-}
                            )
                       -- "src-ag/KWOrder.ag"(line 260, column 12)
                       _edgesout =
                           ({-# LINE 260 "src-ag/KWOrder.ag" #-}
                            _higherOrderEdges
                            {-# LINE 261 "src-ag/KWOrder.hs" #-}
                            )
                       -- "src-ag/KWOrder.ag"(line 261, column 12)
                       _edgesin =
                           ({-# LINE 261 "src-ag/KWOrder.ag" #-}
                            map (flip (,) _vertex    ) _synvertices
                            {-# LINE 267 "src-ag/KWOrder.hs" #-}
                            )
                       -- "src-ag/KWOrder.ag"(line 262, column 12)
                       _lhsOedges =
                           ({-# LINE 262 "src-ag/KWOrder.ag" #-}
                            Set.fromList (_edgesout     ++ _edgesin    )
                            {-# LINE 273 "src-ag/KWOrder.hs" #-}
                            )
                       -- "src-ag/KWOrder.ag"(line 300, column 12)
                       _lhsOnontnames =
                           ({-# LINE 300 "src-ag/KWOrder.ag" #-}
                            case tp_ of
                              NT nont _ _ -> [(name_, nont)]
                              _           -> []
                            {-# LINE 281 "src-ag/KWOrder.hs" #-}
                            )
                       -- "src-ag/DistChildAttr.ag"(line 19, column 11)
                       _chnt =
                           ({-# LINE 19 "src-ag/DistChildAttr.ag" #-}
                            case tp_ of
                              NT nt _ _ -> nt
                              Self      -> error ("The type of child " ++ show name_ ++ " should not be a Self type.")
                              Haskell t -> identifier t
                            {-# LINE 290 "src-ag/KWOrder.hs" #-}
                            )
                       -- "src-ag/DistChildAttr.ag"(line 23, column 11)
                       _inh =
                           ({-# LINE 23 "src-ag/DistChildAttr.ag" #-}
                            Map.findWithDefault Map.empty _chnt     _lhsIinhMap
                            {-# LINE 296 "src-ag/KWOrder.hs" #-}
                            )
                       -- "src-ag/DistChildAttr.ag"(line 24, column 11)
                       _syn =
                           ({-# LINE 24 "src-ag/DistChildAttr.ag" #-}
                            Map.findWithDefault Map.empty _chnt     _lhsIsynMap
                            {-# LINE 302 "src-ag/KWOrder.hs" #-}
                            )
                       -- use rule "src-ag/KWOrder.ag"(line 55, column 67)
                       _lhsOrefHoNts =
                           ({-# LINE 55 "src-ag/KWOrder.ag" #-}
                            _refHoNts
                            {-# LINE 308 "src-ag/KWOrder.hs" #-}
                            )
                       -- use rule "src-ag/KWOrder.ag"(line 55, column 67)
                       _lhsOrefNts =
                           ({-# LINE 55 "src-ag/KWOrder.ag" #-}
                            _refNts
                            {-# LINE 314 "src-ag/KWOrder.hs" #-}
                            )
                   in  ( _lhsOechilds,_lhsOedges,_lhsOnontnames,_lhsOrefHoNts,_lhsOrefNts,_lhsOvertices))) )
-- Children ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         aroundMap            : Map Identifier [Expression]
         inhMap               : Map Identifier Attributes
         mergeMap             : Map Identifier (Identifier, [Identifier], Expression)
         mergedChildren       : Set Identifier
         options              : Options
         synMap               : Map Identifier Attributes
      synthesized attributes:
         echilds              : EChildren
         edges                : Set.Set Edge
         nontnames            : [(Identifier, Identifier)]
         refHoNts             : Set NontermIdent
         refNts               : Set NontermIdent
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
newtype T_Children  = T_Children ((Map Identifier [Expression]) ->
                                  (Map Identifier Attributes) ->
                                  (Map Identifier (Identifier, [Identifier], Expression)) ->
                                  (Set Identifier) ->
                                  Options ->
                                  (Map Identifier Attributes) ->
                                  ( EChildren,(Set.Set Edge),([(Identifier, Identifier)]),(Set NontermIdent),(Set NontermIdent),(Set.Set Vertex)))
data Inh_Children  = Inh_Children {aroundMap_Inh_Children :: (Map Identifier [Expression]),inhMap_Inh_Children :: (Map Identifier Attributes),mergeMap_Inh_Children :: (Map Identifier (Identifier, [Identifier], Expression)),mergedChildren_Inh_Children :: (Set Identifier),options_Inh_Children :: Options,synMap_Inh_Children :: (Map Identifier Attributes)}
data Syn_Children  = Syn_Children {echilds_Syn_Children :: EChildren,edges_Syn_Children :: (Set.Set Edge),nontnames_Syn_Children :: ([(Identifier, Identifier)]),refHoNts_Syn_Children :: (Set NontermIdent),refNts_Syn_Children :: (Set NontermIdent),vertices_Syn_Children :: (Set.Set Vertex)}
wrap_Children :: T_Children  ->
                 Inh_Children  ->
                 Syn_Children 
wrap_Children (T_Children sem ) (Inh_Children _lhsIaroundMap _lhsIinhMap _lhsImergeMap _lhsImergedChildren _lhsIoptions _lhsIsynMap )  =
    (let ( _lhsOechilds,_lhsOedges,_lhsOnontnames,_lhsOrefHoNts,_lhsOrefNts,_lhsOvertices) = sem _lhsIaroundMap _lhsIinhMap _lhsImergeMap _lhsImergedChildren _lhsIoptions _lhsIsynMap 
     in  (Syn_Children _lhsOechilds _lhsOedges _lhsOnontnames _lhsOrefHoNts _lhsOrefNts _lhsOvertices ))
sem_Children_Cons :: T_Child  ->
                     T_Children  ->
                     T_Children 
sem_Children_Cons (T_Child hd_ ) (T_Children tl_ )  =
    (T_Children (\ _lhsIaroundMap
                   _lhsIinhMap
                   _lhsImergeMap
                   _lhsImergedChildren
                   _lhsIoptions
                   _lhsIsynMap ->
                     (let _lhsOechilds :: EChildren
                          _lhsOedges :: (Set.Set Edge)
                          _lhsOnontnames :: ([(Identifier, Identifier)])
                          _lhsOrefHoNts :: (Set NontermIdent)
                          _lhsOrefNts :: (Set NontermIdent)
                          _lhsOvertices :: (Set.Set Vertex)
                          _hdOaroundMap :: (Map Identifier [Expression])
                          _hdOinhMap :: (Map Identifier Attributes)
                          _hdOmergeMap :: (Map Identifier (Identifier, [Identifier], Expression))
                          _hdOmergedChildren :: (Set Identifier)
                          _hdOoptions :: Options
                          _hdOsynMap :: (Map Identifier Attributes)
                          _tlOaroundMap :: (Map Identifier [Expression])
                          _tlOinhMap :: (Map Identifier Attributes)
                          _tlOmergeMap :: (Map Identifier (Identifier, [Identifier], Expression))
                          _tlOmergedChildren :: (Set Identifier)
                          _tlOoptions :: Options
                          _tlOsynMap :: (Map Identifier Attributes)
                          _hdIechilds :: EChild
                          _hdIedges :: (Set.Set Edge)
                          _hdInontnames :: ([(Identifier, Identifier)])
                          _hdIrefHoNts :: (Set NontermIdent)
                          _hdIrefNts :: (Set NontermIdent)
                          _hdIvertices :: (Set.Set Vertex)
                          _tlIechilds :: EChildren
                          _tlIedges :: (Set.Set Edge)
                          _tlInontnames :: ([(Identifier, Identifier)])
                          _tlIrefHoNts :: (Set NontermIdent)
                          _tlIrefNts :: (Set NontermIdent)
                          _tlIvertices :: (Set.Set Vertex)
                          -- use rule "src-ag/KWOrder.ag"(line 174, column 29)
                          _lhsOechilds =
                              ({-# LINE 174 "src-ag/KWOrder.ag" #-}
                               _hdIechilds : _tlIechilds
                               {-# LINE 405 "src-ag/KWOrder.hs" #-}
                               )
                          -- use rule "src-ag/KWOrder.ag"(line 232, column 33)
                          _lhsOedges =
                              ({-# LINE 232 "src-ag/KWOrder.ag" #-}
                               _hdIedges `Set.union` _tlIedges
                               {-# LINE 411 "src-ag/KWOrder.hs" #-}
                               )
                          -- use rule "src-ag/KWOrder.ag"(line 297, column 37)
                          _lhsOnontnames =
                              ({-# LINE 297 "src-ag/KWOrder.ag" #-}
                               _hdInontnames ++ _tlInontnames
                               {-# LINE 417 "src-ag/KWOrder.hs" #-}
                               )
                          -- use rule "src-ag/KWOrder.ag"(line 55, column 67)
                          _lhsOrefHoNts =
                              ({-# LINE 55 "src-ag/KWOrder.ag" #-}
                               _hdIrefHoNts `mappend` _tlIrefHoNts
                               {-# LINE 423 "src-ag/KWOrder.hs" #-}
                               )
                          -- use rule "src-ag/KWOrder.ag"(line 55, column 67)
                          _lhsOrefNts =
                              ({-# LINE 55 "src-ag/KWOrder.ag" #-}
                               _hdIrefNts `mappend` _tlIrefNts
                               {-# LINE 429 "src-ag/KWOrder.hs" #-}
                               )
                          -- use rule "src-ag/KWOrder.ag"(line 188, column 36)
                          _lhsOvertices =
                              ({-# LINE 188 "src-ag/KWOrder.ag" #-}
                               _hdIvertices `Set.union` _tlIvertices
                               {-# LINE 435 "src-ag/KWOrder.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOaroundMap =
                              ({-# LINE 98 "src-ag/KWOrder.ag" #-}
                               _lhsIaroundMap
                               {-# LINE 441 "src-ag/KWOrder.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOinhMap =
                              ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                               _lhsIinhMap
                               {-# LINE 447 "src-ag/KWOrder.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOmergeMap =
                              ({-# LINE 123 "src-ag/KWOrder.ag" #-}
                               _lhsImergeMap
                               {-# LINE 453 "src-ag/KWOrder.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOmergedChildren =
                              ({-# LINE 123 "src-ag/KWOrder.ag" #-}
                               _lhsImergedChildren
                               {-# LINE 459 "src-ag/KWOrder.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOoptions =
                              ({-# LINE 33 "src-ag/KWOrder.ag" #-}
                               _lhsIoptions
                               {-# LINE 465 "src-ag/KWOrder.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOsynMap =
                              ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                               _lhsIsynMap
                               {-# LINE 471 "src-ag/KWOrder.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOaroundMap =
                              ({-# LINE 98 "src-ag/KWOrder.ag" #-}
                               _lhsIaroundMap
                               {-# LINE 477 "src-ag/KWOrder.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOinhMap =
                              ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                               _lhsIinhMap
                               {-# LINE 483 "src-ag/KWOrder.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOmergeMap =
                              ({-# LINE 123 "src-ag/KWOrder.ag" #-}
                               _lhsImergeMap
                               {-# LINE 489 "src-ag/KWOrder.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOmergedChildren =
                              ({-# LINE 123 "src-ag/KWOrder.ag" #-}
                               _lhsImergedChildren
                               {-# LINE 495 "src-ag/KWOrder.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOoptions =
                              ({-# LINE 33 "src-ag/KWOrder.ag" #-}
                               _lhsIoptions
                               {-# LINE 501 "src-ag/KWOrder.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOsynMap =
                              ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                               _lhsIsynMap
                               {-# LINE 507 "src-ag/KWOrder.hs" #-}
                               )
                          ( _hdIechilds,_hdIedges,_hdInontnames,_hdIrefHoNts,_hdIrefNts,_hdIvertices) =
                              hd_ _hdOaroundMap _hdOinhMap _hdOmergeMap _hdOmergedChildren _hdOoptions _hdOsynMap 
                          ( _tlIechilds,_tlIedges,_tlInontnames,_tlIrefHoNts,_tlIrefNts,_tlIvertices) =
                              tl_ _tlOaroundMap _tlOinhMap _tlOmergeMap _tlOmergedChildren _tlOoptions _tlOsynMap 
                      in  ( _lhsOechilds,_lhsOedges,_lhsOnontnames,_lhsOrefHoNts,_lhsOrefNts,_lhsOvertices))) )
sem_Children_Nil :: T_Children 
sem_Children_Nil  =
    (T_Children (\ _lhsIaroundMap
                   _lhsIinhMap
                   _lhsImergeMap
                   _lhsImergedChildren
                   _lhsIoptions
                   _lhsIsynMap ->
                     (let _lhsOechilds :: EChildren
                          _lhsOedges :: (Set.Set Edge)
                          _lhsOnontnames :: ([(Identifier, Identifier)])
                          _lhsOrefHoNts :: (Set NontermIdent)
                          _lhsOrefNts :: (Set NontermIdent)
                          _lhsOvertices :: (Set.Set Vertex)
                          -- use rule "src-ag/KWOrder.ag"(line 174, column 29)
                          _lhsOechilds =
                              ({-# LINE 174 "src-ag/KWOrder.ag" #-}
                               []
                               {-# LINE 532 "src-ag/KWOrder.hs" #-}
                               )
                          -- use rule "src-ag/KWOrder.ag"(line 232, column 33)
                          _lhsOedges =
                              ({-# LINE 232 "src-ag/KWOrder.ag" #-}
                               Set.empty
                               {-# LINE 538 "src-ag/KWOrder.hs" #-}
                               )
                          -- use rule "src-ag/KWOrder.ag"(line 297, column 37)
                          _lhsOnontnames =
                              ({-# LINE 297 "src-ag/KWOrder.ag" #-}
                               []
                               {-# LINE 544 "src-ag/KWOrder.hs" #-}
                               )
                          -- use rule "src-ag/KWOrder.ag"(line 55, column 67)
                          _lhsOrefHoNts =
                              ({-# LINE 55 "src-ag/KWOrder.ag" #-}
                               mempty
                               {-# LINE 550 "src-ag/KWOrder.hs" #-}
                               )
                          -- use rule "src-ag/KWOrder.ag"(line 55, column 67)
                          _lhsOrefNts =
                              ({-# LINE 55 "src-ag/KWOrder.ag" #-}
                               mempty
                               {-# LINE 556 "src-ag/KWOrder.hs" #-}
                               )
                          -- use rule "src-ag/KWOrder.ag"(line 188, column 36)
                          _lhsOvertices =
                              ({-# LINE 188 "src-ag/KWOrder.ag" #-}
                               Set.empty
                               {-# LINE 562 "src-ag/KWOrder.hs" #-}
                               )
                      in  ( _lhsOechilds,_lhsOedges,_lhsOnontnames,_lhsOrefHoNts,_lhsOrefNts,_lhsOvertices))) )
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
                       -- "src-ag/KWOrder.ag"(line 199, column 17)
                       _lhsOvertices =
                           ({-# LINE 199 "src-ag/KWOrder.ag" #-}
                            Set.unions $ map (\tok -> vertices_Syn_HsToken
                                         (wrap_HsToken (sem_HsToken tok) Inh_HsToken)) tks_
                            {-# LINE 604 "src-ag/KWOrder.hs" #-}
                            )
                       -- self rule
                       _copy =
                           ({-# LINE 153 "src-ag/KWOrder.ag" #-}
                            Expression pos_ tks_
                            {-# LINE 610 "src-ag/KWOrder.hs" #-}
                            )
                       -- self rule
                       _lhsOcopy =
                           ({-# LINE 153 "src-ag/KWOrder.ag" #-}
                            _copy
                            {-# LINE 616 "src-ag/KWOrder.hs" #-}
                            )
                   in  ( _lhsOcopy,_lhsOvertices)) )
-- Grammar -----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         options              : Options
      synthesized attributes:
         depgraphs            : PP_Doc
         errors               : Seq Error
         inhmap               : Map.Map NontermIdent Attributes
         localSigMap          : Map.Map NontermIdent (Map.Map ConstructorIdent (Map.Map Identifier Type))
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
         visit 0:
            local closedNtDeps : _
            local closedHoNtDeps : _
            local closedHoNtRevDeps : _
            local _tup1       : {(ExecutionPlan,PP_Doc,PP_Doc,Seq Error)}
-}
-- cata
sem_Grammar :: Grammar  ->
               T_Grammar 
sem_Grammar (Grammar _typeSyns _useMap _derivings _wrappers _nonts _pragmas _manualAttrOrderMap _paramMap _contextMap _quantMap _uniqueMap _augmentsMap _aroundsMap _mergeMap )  =
    (sem_Grammar_Grammar _typeSyns _useMap _derivings _wrappers (sem_Nonterminals _nonts ) _pragmas _manualAttrOrderMap _paramMap _contextMap _quantMap _uniqueMap _augmentsMap _aroundsMap _mergeMap )
-- semantic domain
newtype T_Grammar  = T_Grammar (Options ->
                                ( PP_Doc,(Seq Error),(Map.Map NontermIdent Attributes),(Map.Map NontermIdent (Map.Map ConstructorIdent (Map.Map Identifier Type))),ExecutionPlan,(Map.Map NontermIdent Attributes),PP_Doc))
data Inh_Grammar  = Inh_Grammar {options_Inh_Grammar :: Options}
data Syn_Grammar  = Syn_Grammar {depgraphs_Syn_Grammar :: PP_Doc,errors_Syn_Grammar :: (Seq Error),inhmap_Syn_Grammar :: (Map.Map NontermIdent Attributes),localSigMap_Syn_Grammar :: (Map.Map NontermIdent (Map.Map ConstructorIdent (Map.Map Identifier Type))),output_Syn_Grammar :: ExecutionPlan,synmap_Syn_Grammar :: (Map.Map NontermIdent Attributes),visitgraph_Syn_Grammar :: PP_Doc}
wrap_Grammar :: T_Grammar  ->
                Inh_Grammar  ->
                Syn_Grammar 
wrap_Grammar (T_Grammar sem ) (Inh_Grammar _lhsIoptions )  =
    (let ( _lhsOdepgraphs,_lhsOerrors,_lhsOinhmap,_lhsOlocalSigMap,_lhsOoutput,_lhsOsynmap,_lhsOvisitgraph) = sem _lhsIoptions 
     in  (Syn_Grammar _lhsOdepgraphs _lhsOerrors _lhsOinhmap _lhsOlocalSigMap _lhsOoutput _lhsOsynmap _lhsOvisitgraph ))
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
                         _nontsOaroundMap :: (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression])))
                         _nontsOmergeMap :: (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier, [Identifier], Expression))))
                         _nontsOclassContexts :: ContextMap
                         _nontsOmanualDeps :: AttrOrderMap
                         __tup1 :: ((ExecutionPlan,PP_Doc,PP_Doc,Seq Error))
                         _lhsOoutput :: ExecutionPlan
                         _lhsOdepgraphs :: PP_Doc
                         _lhsOvisitgraph :: PP_Doc
                         _lhsOerrors :: (Seq Error)
                         _nontsOinhMap :: (Map Identifier Attributes)
                         _nontsOsynMap :: (Map Identifier Attributes)
                         _lhsOinhmap :: (Map.Map NontermIdent Attributes)
                         _lhsOlocalSigMap :: (Map.Map NontermIdent (Map.Map ConstructorIdent (Map.Map Identifier Type)))
                         _lhsOsynmap :: (Map.Map NontermIdent Attributes)
                         _nontsOclosedHoNtDeps :: (Map NontermIdent (Set NontermIdent))
                         _nontsOclosedHoNtRevDeps :: (Map NontermIdent (Set NontermIdent))
                         _nontsOclosedNtDeps :: (Map NontermIdent (Set NontermIdent))
                         _nontsOoptions :: Options
                         _nontsIdepinfo :: ([NontDependencyInformation])
                         _nontsIinhMap' :: (Map Identifier Attributes)
                         _nontsIinhmap :: (Map.Map NontermIdent Attributes)
                         _nontsIlocalSigMap :: (Map.Map NontermIdent (Map.Map ConstructorIdent (Map.Map Identifier Type)))
                         _nontsIntDeps :: (Map NontermIdent (Set NontermIdent))
                         _nontsIntHoDeps :: (Map NontermIdent (Set NontermIdent))
                         _nontsIrulenumber :: Int
                         _nontsIsynMap' :: (Map Identifier Attributes)
                         _nontsIsynmap :: (Map.Map NontermIdent Attributes)
                         -- "src-ag/KWOrder.ag"(line 43, column 14)
                         _nontsOrulenumber =
                             ({-# LINE 43 "src-ag/KWOrder.ag" #-}
                              0
                              {-# LINE 719 "src-ag/KWOrder.hs" #-}
                              )
                         -- "src-ag/KWOrder.ag"(line 82, column 3)
                         _closedNtDeps =
                             ({-# LINE 82 "src-ag/KWOrder.ag" #-}
                              closeMap _nontsIntDeps
                              {-# LINE 725 "src-ag/KWOrder.hs" #-}
                              )
                         -- "src-ag/KWOrder.ag"(line 83, column 3)
                         _closedHoNtDeps =
                             ({-# LINE 83 "src-ag/KWOrder.ag" #-}
                              closeMap _nontsIntHoDeps
                              {-# LINE 731 "src-ag/KWOrder.hs" #-}
                              )
                         -- "src-ag/KWOrder.ag"(line 84, column 3)
                         _closedHoNtRevDeps =
                             ({-# LINE 84 "src-ag/KWOrder.ag" #-}
                              revDeps _closedHoNtDeps
                              {-# LINE 737 "src-ag/KWOrder.hs" #-}
                              )
                         -- "src-ag/KWOrder.ag"(line 104, column 3)
                         _nontsOaroundMap =
                             ({-# LINE 104 "src-ag/KWOrder.ag" #-}
                              aroundsMap_
                              {-# LINE 743 "src-ag/KWOrder.hs" #-}
                              )
                         -- "src-ag/KWOrder.ag"(line 129, column 3)
                         _nontsOmergeMap =
                             ({-# LINE 129 "src-ag/KWOrder.ag" #-}
                              mergeMap_
                              {-# LINE 749 "src-ag/KWOrder.hs" #-}
                              )
                         -- "src-ag/KWOrder.ag"(line 145, column 3)
                         _nontsOclassContexts =
                             ({-# LINE 145 "src-ag/KWOrder.ag" #-}
                              contextMap_
                              {-# LINE 755 "src-ag/KWOrder.hs" #-}
                              )
                         -- "src-ag/KWOrder.ag"(line 268, column 31)
                         _nontsOmanualDeps =
                             ({-# LINE 268 "src-ag/KWOrder.ag" #-}
                              manualAttrOrderMap_
                              {-# LINE 761 "src-ag/KWOrder.hs" #-}
                              )
                         -- "src-ag/KWOrder.ag"(line 358, column 15)
                         __tup1 =
                             ({-# LINE 358 "src-ag/KWOrder.ag" #-}
                              let lazyPlan = kennedyWarrenLazy _lhsIoptions wrappers_ _nontsIdepinfo typeSyns_ derivings_
                              in if visit _lhsIoptions
                                 then case kennedyWarrenOrder _lhsIoptions wrappers_ _nontsIdepinfo typeSyns_ derivings_ of
                                        Left e        -> (lazyPlan,empty,empty,Seq.singleton e)
                                        Right (o,d,v) -> (o,d,v,Seq.empty)
                                 else (lazyPlan,empty,empty,Seq.empty)
                              {-# LINE 772 "src-ag/KWOrder.hs" #-}
                              )
                         -- "src-ag/KWOrder.ag"(line 358, column 15)
                         (_lhsOoutput,_,_,_) =
                             ({-# LINE 358 "src-ag/KWOrder.ag" #-}
                              __tup1
                              {-# LINE 778 "src-ag/KWOrder.hs" #-}
                              )
                         -- "src-ag/KWOrder.ag"(line 358, column 15)
                         (_,_lhsOdepgraphs,_,_) =
                             ({-# LINE 358 "src-ag/KWOrder.ag" #-}
                              __tup1
                              {-# LINE 784 "src-ag/KWOrder.hs" #-}
                              )
                         -- "src-ag/KWOrder.ag"(line 358, column 15)
                         (_,_,_lhsOvisitgraph,_) =
                             ({-# LINE 358 "src-ag/KWOrder.ag" #-}
                              __tup1
                              {-# LINE 790 "src-ag/KWOrder.hs" #-}
                              )
                         -- "src-ag/KWOrder.ag"(line 358, column 15)
                         (_,_,_,_lhsOerrors) =
                             ({-# LINE 358 "src-ag/KWOrder.ag" #-}
                              __tup1
                              {-# LINE 796 "src-ag/KWOrder.hs" #-}
                              )
                         -- "src-ag/DistChildAttr.ag"(line 15, column 13)
                         _nontsOinhMap =
                             ({-# LINE 15 "src-ag/DistChildAttr.ag" #-}
                              _nontsIinhMap'
                              {-# LINE 802 "src-ag/KWOrder.hs" #-}
                              )
                         -- "src-ag/DistChildAttr.ag"(line 16, column 13)
                         _nontsOsynMap =
                             ({-# LINE 16 "src-ag/DistChildAttr.ag" #-}
                              _nontsIsynMap'
                              {-# LINE 808 "src-ag/KWOrder.hs" #-}
                              )
                         -- use rule "src-ag/KWOrder.ag"(line 370, column 33)
                         _lhsOinhmap =
                             ({-# LINE 370 "src-ag/KWOrder.ag" #-}
                              _nontsIinhmap
                              {-# LINE 814 "src-ag/KWOrder.hs" #-}
                              )
                         -- use rule "src-ag/KWOrder.ag"(line 382, column 57)
                         _lhsOlocalSigMap =
                             ({-# LINE 382 "src-ag/KWOrder.ag" #-}
                              _nontsIlocalSigMap
                              {-# LINE 820 "src-ag/KWOrder.hs" #-}
                              )
                         -- use rule "src-ag/KWOrder.ag"(line 371, column 33)
                         _lhsOsynmap =
                             ({-# LINE 371 "src-ag/KWOrder.ag" #-}
                              _nontsIsynmap
                              {-# LINE 826 "src-ag/KWOrder.hs" #-}
                              )
                         -- copy rule (from local)
                         _nontsOclosedHoNtDeps =
                             ({-# LINE 54 "src-ag/KWOrder.ag" #-}
                              _closedHoNtDeps
                              {-# LINE 832 "src-ag/KWOrder.hs" #-}
                              )
                         -- copy rule (from local)
                         _nontsOclosedHoNtRevDeps =
                             ({-# LINE 54 "src-ag/KWOrder.ag" #-}
                              _closedHoNtRevDeps
                              {-# LINE 838 "src-ag/KWOrder.hs" #-}
                              )
                         -- copy rule (from local)
                         _nontsOclosedNtDeps =
                             ({-# LINE 54 "src-ag/KWOrder.ag" #-}
                              _closedNtDeps
                              {-# LINE 844 "src-ag/KWOrder.hs" #-}
                              )
                         -- copy rule (down)
                         _nontsOoptions =
                             ({-# LINE 33 "src-ag/KWOrder.ag" #-}
                              _lhsIoptions
                              {-# LINE 850 "src-ag/KWOrder.hs" #-}
                              )
                         ( _nontsIdepinfo,_nontsIinhMap',_nontsIinhmap,_nontsIlocalSigMap,_nontsIntDeps,_nontsIntHoDeps,_nontsIrulenumber,_nontsIsynMap',_nontsIsynmap) =
                             nonts_ _nontsOaroundMap _nontsOclassContexts _nontsOclosedHoNtDeps _nontsOclosedHoNtRevDeps _nontsOclosedNtDeps _nontsOinhMap _nontsOmanualDeps _nontsOmergeMap _nontsOoptions _nontsOrulenumber _nontsOsynMap 
                     in  ( _lhsOdepgraphs,_lhsOerrors,_lhsOinhmap,_lhsOlocalSigMap,_lhsOoutput,_lhsOsynmap,_lhsOvisitgraph))) )
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
                    -- "src-ag/KWOrder.ag"(line 193, column 14)
                    _lhsOvertices =
                        ({-# LINE 193 "src-ag/KWOrder.ag" #-}
                         Set.singleton $ VAttr (if      field_ == _LHS then Inh
                                                else if field_ == _LOC then Loc
                                                else                        Syn) field_ attr_
                         {-# LINE 921 "src-ag/KWOrder.hs" #-}
                         )
                in  ( _lhsOvertices)) )
sem_HsToken_AGLocal :: Identifier ->
                       Pos ->
                       (Maybe String) ->
                       T_HsToken 
sem_HsToken_AGLocal var_ pos_ rdesc_  =
    (T_HsToken (let _lhsOvertices :: (Set.Set Vertex)
                    -- "src-ag/KWOrder.ag"(line 192, column 14)
                    _lhsOvertices =
                        ({-# LINE 192 "src-ag/KWOrder.ag" #-}
                         Set.singleton $ VAttr Loc _LOC var_
                         {-# LINE 934 "src-ag/KWOrder.hs" #-}
                         )
                in  ( _lhsOvertices)) )
sem_HsToken_CharToken :: String ->
                         Pos ->
                         T_HsToken 
sem_HsToken_CharToken value_ pos_  =
    (T_HsToken (let _lhsOvertices :: (Set.Set Vertex)
                    -- use rule "src-ag/KWOrder.ag"(line 188, column 36)
                    _lhsOvertices =
                        ({-# LINE 188 "src-ag/KWOrder.ag" #-}
                         Set.empty
                         {-# LINE 946 "src-ag/KWOrder.hs" #-}
                         )
                in  ( _lhsOvertices)) )
sem_HsToken_Err :: String ->
                   Pos ->
                   T_HsToken 
sem_HsToken_Err mesg_ pos_  =
    (T_HsToken (let _lhsOvertices :: (Set.Set Vertex)
                    -- use rule "src-ag/KWOrder.ag"(line 188, column 36)
                    _lhsOvertices =
                        ({-# LINE 188 "src-ag/KWOrder.ag" #-}
                         Set.empty
                         {-# LINE 958 "src-ag/KWOrder.hs" #-}
                         )
                in  ( _lhsOvertices)) )
sem_HsToken_HsToken :: String ->
                       Pos ->
                       T_HsToken 
sem_HsToken_HsToken value_ pos_  =
    (T_HsToken (let _lhsOvertices :: (Set.Set Vertex)
                    -- use rule "src-ag/KWOrder.ag"(line 188, column 36)
                    _lhsOvertices =
                        ({-# LINE 188 "src-ag/KWOrder.ag" #-}
                         Set.empty
                         {-# LINE 970 "src-ag/KWOrder.hs" #-}
                         )
                in  ( _lhsOvertices)) )
sem_HsToken_StrToken :: String ->
                        Pos ->
                        T_HsToken 
sem_HsToken_StrToken value_ pos_  =
    (T_HsToken (let _lhsOvertices :: (Set.Set Vertex)
                    -- use rule "src-ag/KWOrder.ag"(line 188, column 36)
                    _lhsOvertices =
                        ({-# LINE 188 "src-ag/KWOrder.ag" #-}
                         Set.empty
                         {-# LINE 982 "src-ag/KWOrder.hs" #-}
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
      inherited attributes:
         aroundMap            : Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))
         classContexts        : ContextMap
         closedHoNtDeps       : Map NontermIdent (Set NontermIdent)
         closedHoNtRevDeps    : Map NontermIdent (Set NontermIdent)
         closedNtDeps         : Map NontermIdent (Set NontermIdent)
         inhMap               : Map Identifier Attributes
         manualDeps           : AttrOrderMap
         mergeMap             : Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier, [Identifier], Expression)))
         options              : Options
         synMap               : Map Identifier Attributes
      chained attribute:
         rulenumber           : Int
      synthesized attributes:
         depinfo              : NontDependencyInformation
         inhMap'              : Map Identifier Attributes
         inhmap               : Map.Map NontermIdent Attributes
         localSigMap          : Map.Map NontermIdent (Map.Map ConstructorIdent (Map.Map Identifier Type))
         ntDeps               : Map NontermIdent (Set NontermIdent)
         ntHoDeps             : Map NontermIdent (Set NontermIdent)
         synMap'              : Map Identifier Attributes
         synmap               : Map.Map NontermIdent Attributes
   alternatives:
      alternative Nonterminal:
         child nt             : {NontermIdent}
         child params         : {[Identifier]}
         child inh            : {Attributes}
         child syn            : {Attributes}
         child prods          : Productions 
         visit 0:
            local closedNtDeps : _
            local closedHoNtDeps : _
            local closedHoNtRevDeps : _
            local recursive   : _
            local nontrivAcyc : _
            local hoInfo      : _
            local aroundMap   : _
            local mergeMap    : _
            local classContexts : _
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
newtype T_Nonterminal  = T_Nonterminal ((Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) ->
                                        ContextMap ->
                                        (Map NontermIdent (Set NontermIdent)) ->
                                        (Map NontermIdent (Set NontermIdent)) ->
                                        (Map NontermIdent (Set NontermIdent)) ->
                                        (Map Identifier Attributes) ->
                                        AttrOrderMap ->
                                        (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier, [Identifier], Expression)))) ->
                                        Options ->
                                        Int ->
                                        (Map Identifier Attributes) ->
                                        ( NontDependencyInformation,(Map Identifier Attributes),(Map.Map NontermIdent Attributes),(Map.Map NontermIdent (Map.Map ConstructorIdent (Map.Map Identifier Type))),(Map NontermIdent (Set NontermIdent)),(Map NontermIdent (Set NontermIdent)),Int,(Map Identifier Attributes),(Map.Map NontermIdent Attributes)))
data Inh_Nonterminal  = Inh_Nonterminal {aroundMap_Inh_Nonterminal :: (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))),classContexts_Inh_Nonterminal :: ContextMap,closedHoNtDeps_Inh_Nonterminal :: (Map NontermIdent (Set NontermIdent)),closedHoNtRevDeps_Inh_Nonterminal :: (Map NontermIdent (Set NontermIdent)),closedNtDeps_Inh_Nonterminal :: (Map NontermIdent (Set NontermIdent)),inhMap_Inh_Nonterminal :: (Map Identifier Attributes),manualDeps_Inh_Nonterminal :: AttrOrderMap,mergeMap_Inh_Nonterminal :: (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier, [Identifier], Expression)))),options_Inh_Nonterminal :: Options,rulenumber_Inh_Nonterminal :: Int,synMap_Inh_Nonterminal :: (Map Identifier Attributes)}
data Syn_Nonterminal  = Syn_Nonterminal {depinfo_Syn_Nonterminal :: NontDependencyInformation,inhMap'_Syn_Nonterminal :: (Map Identifier Attributes),inhmap_Syn_Nonterminal :: (Map.Map NontermIdent Attributes),localSigMap_Syn_Nonterminal :: (Map.Map NontermIdent (Map.Map ConstructorIdent (Map.Map Identifier Type))),ntDeps_Syn_Nonterminal :: (Map NontermIdent (Set NontermIdent)),ntHoDeps_Syn_Nonterminal :: (Map NontermIdent (Set NontermIdent)),rulenumber_Syn_Nonterminal :: Int,synMap'_Syn_Nonterminal :: (Map Identifier Attributes),synmap_Syn_Nonterminal :: (Map.Map NontermIdent Attributes)}
wrap_Nonterminal :: T_Nonterminal  ->
                    Inh_Nonterminal  ->
                    Syn_Nonterminal 
wrap_Nonterminal (T_Nonterminal sem ) (Inh_Nonterminal _lhsIaroundMap _lhsIclassContexts _lhsIclosedHoNtDeps _lhsIclosedHoNtRevDeps _lhsIclosedNtDeps _lhsIinhMap _lhsImanualDeps _lhsImergeMap _lhsIoptions _lhsIrulenumber _lhsIsynMap )  =
    (let ( _lhsOdepinfo,_lhsOinhMap',_lhsOinhmap,_lhsOlocalSigMap,_lhsOntDeps,_lhsOntHoDeps,_lhsOrulenumber,_lhsOsynMap',_lhsOsynmap) = sem _lhsIaroundMap _lhsIclassContexts _lhsIclosedHoNtDeps _lhsIclosedHoNtRevDeps _lhsIclosedNtDeps _lhsIinhMap _lhsImanualDeps _lhsImergeMap _lhsIoptions _lhsIrulenumber _lhsIsynMap 
     in  (Syn_Nonterminal _lhsOdepinfo _lhsOinhMap' _lhsOinhmap _lhsOlocalSigMap _lhsOntDeps _lhsOntHoDeps _lhsOrulenumber _lhsOsynMap' _lhsOsynmap ))
sem_Nonterminal_Nonterminal :: NontermIdent ->
                               ([Identifier]) ->
                               Attributes ->
                               Attributes ->
                               T_Productions  ->
                               T_Nonterminal 
sem_Nonterminal_Nonterminal nt_ params_ inh_ syn_ (T_Productions prods_ )  =
    (T_Nonterminal (\ _lhsIaroundMap
                      _lhsIclassContexts
                      _lhsIclosedHoNtDeps
                      _lhsIclosedHoNtRevDeps
                      _lhsIclosedNtDeps
                      _lhsIinhMap
                      _lhsImanualDeps
                      _lhsImergeMap
                      _lhsIoptions
                      _lhsIrulenumber
                      _lhsIsynMap ->
                        (let _lhsOntDeps :: (Map NontermIdent (Set NontermIdent))
                             _lhsOntHoDeps :: (Map NontermIdent (Set NontermIdent))
                             _prodsOmanualDeps :: (Map ConstructorIdent (Set Dependency))
                             _lhsOdepinfo :: NontDependencyInformation
                             _lhsOinhmap :: (Map.Map NontermIdent Attributes)
                             _lhsOsynmap :: (Map.Map NontermIdent Attributes)
                             _lhsOlocalSigMap :: (Map.Map NontermIdent (Map.Map ConstructorIdent (Map.Map Identifier Type)))
                             _lhsOinhMap' :: (Map Identifier Attributes)
                             _lhsOsynMap' :: (Map Identifier Attributes)
                             _lhsOrulenumber :: Int
                             _prodsOaroundMap :: (Map ConstructorIdent (Map Identifier [Expression]))
                             _prodsOinhMap :: (Map Identifier Attributes)
                             _prodsOmergeMap :: (Map ConstructorIdent (Map Identifier (Identifier, [Identifier], Expression)))
                             _prodsOoptions :: Options
                             _prodsOrulenumber :: Int
                             _prodsOsynMap :: (Map Identifier Attributes)
                             _prodsIdepgraph :: ([ProdDependencyGraph])
                             _prodsIlocalSigMap :: (Map.Map ConstructorIdent (Map.Map Identifier Type))
                             _prodsIrefHoNts :: (Set NontermIdent)
                             _prodsIrefNts :: (Set NontermIdent)
                             _prodsIrulenumber :: Int
                             -- "src-ag/KWOrder.ag"(line 58, column 3)
                             _lhsOntDeps =
                                 ({-# LINE 58 "src-ag/KWOrder.ag" #-}
                                  Map.singleton nt_ _prodsIrefNts
                                  {-# LINE 1162 "src-ag/KWOrder.hs" #-}
                                  )
                             -- "src-ag/KWOrder.ag"(line 59, column 3)
                             _lhsOntHoDeps =
                                 ({-# LINE 59 "src-ag/KWOrder.ag" #-}
                                  Map.singleton nt_ _prodsIrefHoNts
                                  {-# LINE 1168 "src-ag/KWOrder.hs" #-}
                                  )
                             -- "src-ag/KWOrder.ag"(line 61, column 3)
                             _closedNtDeps =
                                 ({-# LINE 61 "src-ag/KWOrder.ag" #-}
                                  Map.findWithDefault Set.empty nt_ _lhsIclosedNtDeps
                                  {-# LINE 1174 "src-ag/KWOrder.hs" #-}
                                  )
                             -- "src-ag/KWOrder.ag"(line 62, column 3)
                             _closedHoNtDeps =
                                 ({-# LINE 62 "src-ag/KWOrder.ag" #-}
                                  Map.findWithDefault Set.empty nt_ _lhsIclosedHoNtDeps
                                  {-# LINE 1180 "src-ag/KWOrder.hs" #-}
                                  )
                             -- "src-ag/KWOrder.ag"(line 63, column 3)
                             _closedHoNtRevDeps =
                                 ({-# LINE 63 "src-ag/KWOrder.ag" #-}
                                  Map.findWithDefault Set.empty nt_ _lhsIclosedHoNtRevDeps
                                  {-# LINE 1186 "src-ag/KWOrder.hs" #-}
                                  )
                             -- "src-ag/KWOrder.ag"(line 65, column 3)
                             _recursive =
                                 ({-# LINE 65 "src-ag/KWOrder.ag" #-}
                                  nt_ `Set.member` _closedNtDeps
                                  {-# LINE 1192 "src-ag/KWOrder.hs" #-}
                                  )
                             -- "src-ag/KWOrder.ag"(line 66, column 3)
                             _nontrivAcyc =
                                 ({-# LINE 66 "src-ag/KWOrder.ag" #-}
                                  nt_ `Set.member` _closedHoNtDeps
                                  {-# LINE 1198 "src-ag/KWOrder.hs" #-}
                                  )
                             -- "src-ag/KWOrder.ag"(line 67, column 3)
                             _hoInfo =
                                 ({-# LINE 67 "src-ag/KWOrder.ag" #-}
                                  HigherOrderInfo { hoNtDeps            = _closedHoNtDeps
                                                  , hoNtRevDeps         = _closedHoNtRevDeps
                                                  , hoAcyclic           = _nontrivAcyc
                                                  }
                                  {-# LINE 1207 "src-ag/KWOrder.hs" #-}
                                  )
                             -- "src-ag/KWOrder.ag"(line 100, column 32)
                             _aroundMap =
                                 ({-# LINE 100 "src-ag/KWOrder.ag" #-}
                                  Map.findWithDefault Map.empty nt_ _lhsIaroundMap
                                  {-# LINE 1213 "src-ag/KWOrder.hs" #-}
                                  )
                             -- "src-ag/KWOrder.ag"(line 125, column 32)
                             _mergeMap =
                                 ({-# LINE 125 "src-ag/KWOrder.ag" #-}
                                  Map.findWithDefault Map.empty nt_ _lhsImergeMap
                                  {-# LINE 1219 "src-ag/KWOrder.hs" #-}
                                  )
                             -- "src-ag/KWOrder.ag"(line 148, column 3)
                             _classContexts =
                                 ({-# LINE 148 "src-ag/KWOrder.ag" #-}
                                  Map.findWithDefault [] nt_ _lhsIclassContexts
                                  {-# LINE 1225 "src-ag/KWOrder.hs" #-}
                                  )
                             -- "src-ag/KWOrder.ag"(line 269, column 31)
                             _prodsOmanualDeps =
                                 ({-# LINE 269 "src-ag/KWOrder.ag" #-}
                                  Map.findWithDefault Map.empty nt_ _lhsImanualDeps
                                  {-# LINE 1231 "src-ag/KWOrder.hs" #-}
                                  )
                             -- "src-ag/KWOrder.ag"(line 324, column 18)
                             _synvertices =
                                 ({-# LINE 324 "src-ag/KWOrder.ag" #-}
                                  map (VAttr Syn nt_) . Map.keys $ syn_
                                  {-# LINE 1237 "src-ag/KWOrder.hs" #-}
                                  )
                             -- "src-ag/KWOrder.ag"(line 325, column 18)
                             _inhvertices =
                                 ({-# LINE 325 "src-ag/KWOrder.ag" #-}
                                  map (VAttr Inh nt_) . Map.keys $ inh_
                                  {-# LINE 1243 "src-ag/KWOrder.hs" #-}
                                  )
                             -- "src-ag/KWOrder.ag"(line 326, column 18)
                             _vertices =
                                 ({-# LINE 326 "src-ag/KWOrder.ag" #-}
                                  _synvertices     ++ _inhvertices
                                  {-# LINE 1249 "src-ag/KWOrder.hs" #-}
                                  )
                             -- "src-ag/KWOrder.ag"(line 330, column 18)
                             _nontgraph =
                                 ({-# LINE 330 "src-ag/KWOrder.ag" #-}
                                  NontDependencyGraph { ndgVertices = _vertices
                                                      , ndgEdges    = [] }
                                  {-# LINE 1256 "src-ag/KWOrder.hs" #-}
                                  )
                             -- "src-ag/KWOrder.ag"(line 338, column 18)
                             _lhsOdepinfo =
                                 ({-# LINE 338 "src-ag/KWOrder.ag" #-}
                                  NontDependencyInformation { ndiNonterminal = nt_
                                                            , ndiParams      = params_
                                                            , ndiInh         = Map.keys inh_
                                                            , ndiSyn         = Map.keys syn_
                                                            , ndiDepGraph    = _nontgraph
                                                            , ndiProds       = _prodsIdepgraph
                                                            , ndiRecursive   = _recursive
                                                            , ndiHoInfo      = _hoInfo
                                                            , ndiClassCtxs   = _classContexts
                                                            }
                                  {-# LINE 1271 "src-ag/KWOrder.hs" #-}
                                  )
                             -- "src-ag/KWOrder.ag"(line 376, column 17)
                             _lhsOinhmap =
                                 ({-# LINE 376 "src-ag/KWOrder.ag" #-}
                                  Map.singleton nt_ inh_
                                  {-# LINE 1277 "src-ag/KWOrder.hs" #-}
                                  )
                             -- "src-ag/KWOrder.ag"(line 377, column 17)
                             _lhsOsynmap =
                                 ({-# LINE 377 "src-ag/KWOrder.ag" #-}
                                  Map.singleton nt_ syn_
                                  {-# LINE 1283 "src-ag/KWOrder.hs" #-}
                                  )
                             -- "src-ag/KWOrder.ag"(line 386, column 32)
                             _lhsOlocalSigMap =
                                 ({-# LINE 386 "src-ag/KWOrder.ag" #-}
                                  Map.singleton nt_ _prodsIlocalSigMap
                                  {-# LINE 1289 "src-ag/KWOrder.hs" #-}
                                  )
                             -- "src-ag/DistChildAttr.ag"(line 7, column 18)
                             _lhsOinhMap' =
                                 ({-# LINE 7 "src-ag/DistChildAttr.ag" #-}
                                  Map.singleton nt_ inh_
                                  {-# LINE 1295 "src-ag/KWOrder.hs" #-}
                                  )
                             -- "src-ag/DistChildAttr.ag"(line 8, column 18)
                             _lhsOsynMap' =
                                 ({-# LINE 8 "src-ag/DistChildAttr.ag" #-}
                                  Map.singleton nt_ syn_
                                  {-# LINE 1301 "src-ag/KWOrder.hs" #-}
                                  )
                             -- copy rule (up)
                             _lhsOrulenumber =
                                 ({-# LINE 40 "src-ag/KWOrder.ag" #-}
                                  _prodsIrulenumber
                                  {-# LINE 1307 "src-ag/KWOrder.hs" #-}
                                  )
                             -- copy rule (from local)
                             _prodsOaroundMap =
                                 ({-# LINE 95 "src-ag/KWOrder.ag" #-}
                                  _aroundMap
                                  {-# LINE 1313 "src-ag/KWOrder.hs" #-}
                                  )
                             -- copy rule (down)
                             _prodsOinhMap =
                                 ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                  _lhsIinhMap
                                  {-# LINE 1319 "src-ag/KWOrder.hs" #-}
                                  )
                             -- copy rule (from local)
                             _prodsOmergeMap =
                                 ({-# LINE 120 "src-ag/KWOrder.ag" #-}
                                  _mergeMap
                                  {-# LINE 1325 "src-ag/KWOrder.hs" #-}
                                  )
                             -- copy rule (down)
                             _prodsOoptions =
                                 ({-# LINE 33 "src-ag/KWOrder.ag" #-}
                                  _lhsIoptions
                                  {-# LINE 1331 "src-ag/KWOrder.hs" #-}
                                  )
                             -- copy rule (down)
                             _prodsOrulenumber =
                                 ({-# LINE 40 "src-ag/KWOrder.ag" #-}
                                  _lhsIrulenumber
                                  {-# LINE 1337 "src-ag/KWOrder.hs" #-}
                                  )
                             -- copy rule (down)
                             _prodsOsynMap =
                                 ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                  _lhsIsynMap
                                  {-# LINE 1343 "src-ag/KWOrder.hs" #-}
                                  )
                             ( _prodsIdepgraph,_prodsIlocalSigMap,_prodsIrefHoNts,_prodsIrefNts,_prodsIrulenumber) =
                                 prods_ _prodsOaroundMap _prodsOinhMap _prodsOmanualDeps _prodsOmergeMap _prodsOoptions _prodsOrulenumber _prodsOsynMap 
                         in  ( _lhsOdepinfo,_lhsOinhMap',_lhsOinhmap,_lhsOlocalSigMap,_lhsOntDeps,_lhsOntHoDeps,_lhsOrulenumber,_lhsOsynMap',_lhsOsynmap))) )
-- Nonterminals ------------------------------------------------
{-
   visit 0:
      inherited attributes:
         aroundMap            : Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))
         classContexts        : ContextMap
         closedHoNtDeps       : Map NontermIdent (Set NontermIdent)
         closedHoNtRevDeps    : Map NontermIdent (Set NontermIdent)
         closedNtDeps         : Map NontermIdent (Set NontermIdent)
         inhMap               : Map Identifier Attributes
         manualDeps           : AttrOrderMap
         mergeMap             : Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier, [Identifier], Expression)))
         options              : Options
         synMap               : Map Identifier Attributes
      chained attribute:
         rulenumber           : Int
      synthesized attributes:
         depinfo              : [NontDependencyInformation]
         inhMap'              : Map Identifier Attributes
         inhmap               : Map.Map NontermIdent Attributes
         localSigMap          : Map.Map NontermIdent (Map.Map ConstructorIdent (Map.Map Identifier Type))
         ntDeps               : Map NontermIdent (Set NontermIdent)
         ntHoDeps             : Map NontermIdent (Set NontermIdent)
         synMap'              : Map Identifier Attributes
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
newtype T_Nonterminals  = T_Nonterminals ((Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) ->
                                          ContextMap ->
                                          (Map NontermIdent (Set NontermIdent)) ->
                                          (Map NontermIdent (Set NontermIdent)) ->
                                          (Map NontermIdent (Set NontermIdent)) ->
                                          (Map Identifier Attributes) ->
                                          AttrOrderMap ->
                                          (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier, [Identifier], Expression)))) ->
                                          Options ->
                                          Int ->
                                          (Map Identifier Attributes) ->
                                          ( ([NontDependencyInformation]),(Map Identifier Attributes),(Map.Map NontermIdent Attributes),(Map.Map NontermIdent (Map.Map ConstructorIdent (Map.Map Identifier Type))),(Map NontermIdent (Set NontermIdent)),(Map NontermIdent (Set NontermIdent)),Int,(Map Identifier Attributes),(Map.Map NontermIdent Attributes)))
data Inh_Nonterminals  = Inh_Nonterminals {aroundMap_Inh_Nonterminals :: (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))),classContexts_Inh_Nonterminals :: ContextMap,closedHoNtDeps_Inh_Nonterminals :: (Map NontermIdent (Set NontermIdent)),closedHoNtRevDeps_Inh_Nonterminals :: (Map NontermIdent (Set NontermIdent)),closedNtDeps_Inh_Nonterminals :: (Map NontermIdent (Set NontermIdent)),inhMap_Inh_Nonterminals :: (Map Identifier Attributes),manualDeps_Inh_Nonterminals :: AttrOrderMap,mergeMap_Inh_Nonterminals :: (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier, [Identifier], Expression)))),options_Inh_Nonterminals :: Options,rulenumber_Inh_Nonterminals :: Int,synMap_Inh_Nonterminals :: (Map Identifier Attributes)}
data Syn_Nonterminals  = Syn_Nonterminals {depinfo_Syn_Nonterminals :: ([NontDependencyInformation]),inhMap'_Syn_Nonterminals :: (Map Identifier Attributes),inhmap_Syn_Nonterminals :: (Map.Map NontermIdent Attributes),localSigMap_Syn_Nonterminals :: (Map.Map NontermIdent (Map.Map ConstructorIdent (Map.Map Identifier Type))),ntDeps_Syn_Nonterminals :: (Map NontermIdent (Set NontermIdent)),ntHoDeps_Syn_Nonterminals :: (Map NontermIdent (Set NontermIdent)),rulenumber_Syn_Nonterminals :: Int,synMap'_Syn_Nonterminals :: (Map Identifier Attributes),synmap_Syn_Nonterminals :: (Map.Map NontermIdent Attributes)}
wrap_Nonterminals :: T_Nonterminals  ->
                     Inh_Nonterminals  ->
                     Syn_Nonterminals 
wrap_Nonterminals (T_Nonterminals sem ) (Inh_Nonterminals _lhsIaroundMap _lhsIclassContexts _lhsIclosedHoNtDeps _lhsIclosedHoNtRevDeps _lhsIclosedNtDeps _lhsIinhMap _lhsImanualDeps _lhsImergeMap _lhsIoptions _lhsIrulenumber _lhsIsynMap )  =
    (let ( _lhsOdepinfo,_lhsOinhMap',_lhsOinhmap,_lhsOlocalSigMap,_lhsOntDeps,_lhsOntHoDeps,_lhsOrulenumber,_lhsOsynMap',_lhsOsynmap) = sem _lhsIaroundMap _lhsIclassContexts _lhsIclosedHoNtDeps _lhsIclosedHoNtRevDeps _lhsIclosedNtDeps _lhsIinhMap _lhsImanualDeps _lhsImergeMap _lhsIoptions _lhsIrulenumber _lhsIsynMap 
     in  (Syn_Nonterminals _lhsOdepinfo _lhsOinhMap' _lhsOinhmap _lhsOlocalSigMap _lhsOntDeps _lhsOntHoDeps _lhsOrulenumber _lhsOsynMap' _lhsOsynmap ))
sem_Nonterminals_Cons :: T_Nonterminal  ->
                         T_Nonterminals  ->
                         T_Nonterminals 
sem_Nonterminals_Cons (T_Nonterminal hd_ ) (T_Nonterminals tl_ )  =
    (T_Nonterminals (\ _lhsIaroundMap
                       _lhsIclassContexts
                       _lhsIclosedHoNtDeps
                       _lhsIclosedHoNtRevDeps
                       _lhsIclosedNtDeps
                       _lhsIinhMap
                       _lhsImanualDeps
                       _lhsImergeMap
                       _lhsIoptions
                       _lhsIrulenumber
                       _lhsIsynMap ->
                         (let _lhsOdepinfo :: ([NontDependencyInformation])
                              _lhsOinhMap' :: (Map Identifier Attributes)
                              _lhsOinhmap :: (Map.Map NontermIdent Attributes)
                              _lhsOlocalSigMap :: (Map.Map NontermIdent (Map.Map ConstructorIdent (Map.Map Identifier Type)))
                              _lhsOntDeps :: (Map NontermIdent (Set NontermIdent))
                              _lhsOntHoDeps :: (Map NontermIdent (Set NontermIdent))
                              _lhsOsynMap' :: (Map Identifier Attributes)
                              _lhsOsynmap :: (Map.Map NontermIdent Attributes)
                              _lhsOrulenumber :: Int
                              _hdOaroundMap :: (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression])))
                              _hdOclassContexts :: ContextMap
                              _hdOclosedHoNtDeps :: (Map NontermIdent (Set NontermIdent))
                              _hdOclosedHoNtRevDeps :: (Map NontermIdent (Set NontermIdent))
                              _hdOclosedNtDeps :: (Map NontermIdent (Set NontermIdent))
                              _hdOinhMap :: (Map Identifier Attributes)
                              _hdOmanualDeps :: AttrOrderMap
                              _hdOmergeMap :: (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier, [Identifier], Expression))))
                              _hdOoptions :: Options
                              _hdOrulenumber :: Int
                              _hdOsynMap :: (Map Identifier Attributes)
                              _tlOaroundMap :: (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression])))
                              _tlOclassContexts :: ContextMap
                              _tlOclosedHoNtDeps :: (Map NontermIdent (Set NontermIdent))
                              _tlOclosedHoNtRevDeps :: (Map NontermIdent (Set NontermIdent))
                              _tlOclosedNtDeps :: (Map NontermIdent (Set NontermIdent))
                              _tlOinhMap :: (Map Identifier Attributes)
                              _tlOmanualDeps :: AttrOrderMap
                              _tlOmergeMap :: (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier, [Identifier], Expression))))
                              _tlOoptions :: Options
                              _tlOrulenumber :: Int
                              _tlOsynMap :: (Map Identifier Attributes)
                              _hdIdepinfo :: NontDependencyInformation
                              _hdIinhMap' :: (Map Identifier Attributes)
                              _hdIinhmap :: (Map.Map NontermIdent Attributes)
                              _hdIlocalSigMap :: (Map.Map NontermIdent (Map.Map ConstructorIdent (Map.Map Identifier Type)))
                              _hdIntDeps :: (Map NontermIdent (Set NontermIdent))
                              _hdIntHoDeps :: (Map NontermIdent (Set NontermIdent))
                              _hdIrulenumber :: Int
                              _hdIsynMap' :: (Map Identifier Attributes)
                              _hdIsynmap :: (Map.Map NontermIdent Attributes)
                              _tlIdepinfo :: ([NontDependencyInformation])
                              _tlIinhMap' :: (Map Identifier Attributes)
                              _tlIinhmap :: (Map.Map NontermIdent Attributes)
                              _tlIlocalSigMap :: (Map.Map NontermIdent (Map.Map ConstructorIdent (Map.Map Identifier Type)))
                              _tlIntDeps :: (Map NontermIdent (Set NontermIdent))
                              _tlIntHoDeps :: (Map NontermIdent (Set NontermIdent))
                              _tlIrulenumber :: Int
                              _tlIsynMap' :: (Map Identifier Attributes)
                              _tlIsynmap :: (Map.Map NontermIdent Attributes)
                              -- use rule "src-ag/KWOrder.ag"(line 335, column 33)
                              _lhsOdepinfo =
                                  ({-# LINE 335 "src-ag/KWOrder.ag" #-}
                                   _hdIdepinfo : _tlIdepinfo
                                   {-# LINE 1473 "src-ag/KWOrder.hs" #-}
                                   )
                              -- use rule "src-ag/DistChildAttr.ag"(line 4, column 53)
                              _lhsOinhMap' =
                                  ({-# LINE 4 "src-ag/DistChildAttr.ag" #-}
                                   _hdIinhMap' `Map.union` _tlIinhMap'
                                   {-# LINE 1479 "src-ag/KWOrder.hs" #-}
                                   )
                              -- use rule "src-ag/KWOrder.ag"(line 370, column 33)
                              _lhsOinhmap =
                                  ({-# LINE 370 "src-ag/KWOrder.ag" #-}
                                   _hdIinhmap `Map.union` _tlIinhmap
                                   {-# LINE 1485 "src-ag/KWOrder.hs" #-}
                                   )
                              -- use rule "src-ag/KWOrder.ag"(line 382, column 57)
                              _lhsOlocalSigMap =
                                  ({-# LINE 382 "src-ag/KWOrder.ag" #-}
                                   _hdIlocalSigMap `Map.union` _tlIlocalSigMap
                                   {-# LINE 1491 "src-ag/KWOrder.hs" #-}
                                   )
                              -- use rule "src-ag/KWOrder.ag"(line 53, column 54)
                              _lhsOntDeps =
                                  ({-# LINE 53 "src-ag/KWOrder.ag" #-}
                                   _hdIntDeps `mappend` _tlIntDeps
                                   {-# LINE 1497 "src-ag/KWOrder.hs" #-}
                                   )
                              -- use rule "src-ag/KWOrder.ag"(line 53, column 54)
                              _lhsOntHoDeps =
                                  ({-# LINE 53 "src-ag/KWOrder.ag" #-}
                                   _hdIntHoDeps `mappend` _tlIntHoDeps
                                   {-# LINE 1503 "src-ag/KWOrder.hs" #-}
                                   )
                              -- use rule "src-ag/DistChildAttr.ag"(line 4, column 53)
                              _lhsOsynMap' =
                                  ({-# LINE 4 "src-ag/DistChildAttr.ag" #-}
                                   _hdIsynMap' `Map.union` _tlIsynMap'
                                   {-# LINE 1509 "src-ag/KWOrder.hs" #-}
                                   )
                              -- use rule "src-ag/KWOrder.ag"(line 371, column 33)
                              _lhsOsynmap =
                                  ({-# LINE 371 "src-ag/KWOrder.ag" #-}
                                   _hdIsynmap `Map.union` _tlIsynmap
                                   {-# LINE 1515 "src-ag/KWOrder.hs" #-}
                                   )
                              -- copy rule (up)
                              _lhsOrulenumber =
                                  ({-# LINE 40 "src-ag/KWOrder.ag" #-}
                                   _tlIrulenumber
                                   {-# LINE 1521 "src-ag/KWOrder.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOaroundMap =
                                  ({-# LINE 92 "src-ag/KWOrder.ag" #-}
                                   _lhsIaroundMap
                                   {-# LINE 1527 "src-ag/KWOrder.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOclassContexts =
                                  ({-# LINE 142 "src-ag/KWOrder.ag" #-}
                                   _lhsIclassContexts
                                   {-# LINE 1533 "src-ag/KWOrder.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOclosedHoNtDeps =
                                  ({-# LINE 54 "src-ag/KWOrder.ag" #-}
                                   _lhsIclosedHoNtDeps
                                   {-# LINE 1539 "src-ag/KWOrder.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOclosedHoNtRevDeps =
                                  ({-# LINE 54 "src-ag/KWOrder.ag" #-}
                                   _lhsIclosedHoNtRevDeps
                                   {-# LINE 1545 "src-ag/KWOrder.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOclosedNtDeps =
                                  ({-# LINE 54 "src-ag/KWOrder.ag" #-}
                                   _lhsIclosedNtDeps
                                   {-# LINE 1551 "src-ag/KWOrder.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOinhMap =
                                  ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                   _lhsIinhMap
                                   {-# LINE 1557 "src-ag/KWOrder.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOmanualDeps =
                                  ({-# LINE 265 "src-ag/KWOrder.ag" #-}
                                   _lhsImanualDeps
                                   {-# LINE 1563 "src-ag/KWOrder.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOmergeMap =
                                  ({-# LINE 117 "src-ag/KWOrder.ag" #-}
                                   _lhsImergeMap
                                   {-# LINE 1569 "src-ag/KWOrder.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOoptions =
                                  ({-# LINE 33 "src-ag/KWOrder.ag" #-}
                                   _lhsIoptions
                                   {-# LINE 1575 "src-ag/KWOrder.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOrulenumber =
                                  ({-# LINE 40 "src-ag/KWOrder.ag" #-}
                                   _lhsIrulenumber
                                   {-# LINE 1581 "src-ag/KWOrder.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOsynMap =
                                  ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                   _lhsIsynMap
                                   {-# LINE 1587 "src-ag/KWOrder.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOaroundMap =
                                  ({-# LINE 92 "src-ag/KWOrder.ag" #-}
                                   _lhsIaroundMap
                                   {-# LINE 1593 "src-ag/KWOrder.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOclassContexts =
                                  ({-# LINE 142 "src-ag/KWOrder.ag" #-}
                                   _lhsIclassContexts
                                   {-# LINE 1599 "src-ag/KWOrder.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOclosedHoNtDeps =
                                  ({-# LINE 54 "src-ag/KWOrder.ag" #-}
                                   _lhsIclosedHoNtDeps
                                   {-# LINE 1605 "src-ag/KWOrder.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOclosedHoNtRevDeps =
                                  ({-# LINE 54 "src-ag/KWOrder.ag" #-}
                                   _lhsIclosedHoNtRevDeps
                                   {-# LINE 1611 "src-ag/KWOrder.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOclosedNtDeps =
                                  ({-# LINE 54 "src-ag/KWOrder.ag" #-}
                                   _lhsIclosedNtDeps
                                   {-# LINE 1617 "src-ag/KWOrder.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOinhMap =
                                  ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                   _lhsIinhMap
                                   {-# LINE 1623 "src-ag/KWOrder.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOmanualDeps =
                                  ({-# LINE 265 "src-ag/KWOrder.ag" #-}
                                   _lhsImanualDeps
                                   {-# LINE 1629 "src-ag/KWOrder.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOmergeMap =
                                  ({-# LINE 117 "src-ag/KWOrder.ag" #-}
                                   _lhsImergeMap
                                   {-# LINE 1635 "src-ag/KWOrder.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOoptions =
                                  ({-# LINE 33 "src-ag/KWOrder.ag" #-}
                                   _lhsIoptions
                                   {-# LINE 1641 "src-ag/KWOrder.hs" #-}
                                   )
                              -- copy rule (chain)
                              _tlOrulenumber =
                                  ({-# LINE 40 "src-ag/KWOrder.ag" #-}
                                   _hdIrulenumber
                                   {-# LINE 1647 "src-ag/KWOrder.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOsynMap =
                                  ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                   _lhsIsynMap
                                   {-# LINE 1653 "src-ag/KWOrder.hs" #-}
                                   )
                              ( _hdIdepinfo,_hdIinhMap',_hdIinhmap,_hdIlocalSigMap,_hdIntDeps,_hdIntHoDeps,_hdIrulenumber,_hdIsynMap',_hdIsynmap) =
                                  hd_ _hdOaroundMap _hdOclassContexts _hdOclosedHoNtDeps _hdOclosedHoNtRevDeps _hdOclosedNtDeps _hdOinhMap _hdOmanualDeps _hdOmergeMap _hdOoptions _hdOrulenumber _hdOsynMap 
                              ( _tlIdepinfo,_tlIinhMap',_tlIinhmap,_tlIlocalSigMap,_tlIntDeps,_tlIntHoDeps,_tlIrulenumber,_tlIsynMap',_tlIsynmap) =
                                  tl_ _tlOaroundMap _tlOclassContexts _tlOclosedHoNtDeps _tlOclosedHoNtRevDeps _tlOclosedNtDeps _tlOinhMap _tlOmanualDeps _tlOmergeMap _tlOoptions _tlOrulenumber _tlOsynMap 
                          in  ( _lhsOdepinfo,_lhsOinhMap',_lhsOinhmap,_lhsOlocalSigMap,_lhsOntDeps,_lhsOntHoDeps,_lhsOrulenumber,_lhsOsynMap',_lhsOsynmap))) )
sem_Nonterminals_Nil :: T_Nonterminals 
sem_Nonterminals_Nil  =
    (T_Nonterminals (\ _lhsIaroundMap
                       _lhsIclassContexts
                       _lhsIclosedHoNtDeps
                       _lhsIclosedHoNtRevDeps
                       _lhsIclosedNtDeps
                       _lhsIinhMap
                       _lhsImanualDeps
                       _lhsImergeMap
                       _lhsIoptions
                       _lhsIrulenumber
                       _lhsIsynMap ->
                         (let _lhsOdepinfo :: ([NontDependencyInformation])
                              _lhsOinhMap' :: (Map Identifier Attributes)
                              _lhsOinhmap :: (Map.Map NontermIdent Attributes)
                              _lhsOlocalSigMap :: (Map.Map NontermIdent (Map.Map ConstructorIdent (Map.Map Identifier Type)))
                              _lhsOntDeps :: (Map NontermIdent (Set NontermIdent))
                              _lhsOntHoDeps :: (Map NontermIdent (Set NontermIdent))
                              _lhsOsynMap' :: (Map Identifier Attributes)
                              _lhsOsynmap :: (Map.Map NontermIdent Attributes)
                              _lhsOrulenumber :: Int
                              -- use rule "src-ag/KWOrder.ag"(line 335, column 33)
                              _lhsOdepinfo =
                                  ({-# LINE 335 "src-ag/KWOrder.ag" #-}
                                   []
                                   {-# LINE 1686 "src-ag/KWOrder.hs" #-}
                                   )
                              -- use rule "src-ag/DistChildAttr.ag"(line 4, column 53)
                              _lhsOinhMap' =
                                  ({-# LINE 4 "src-ag/DistChildAttr.ag" #-}
                                   Map.empty
                                   {-# LINE 1692 "src-ag/KWOrder.hs" #-}
                                   )
                              -- use rule "src-ag/KWOrder.ag"(line 370, column 33)
                              _lhsOinhmap =
                                  ({-# LINE 370 "src-ag/KWOrder.ag" #-}
                                   Map.empty
                                   {-# LINE 1698 "src-ag/KWOrder.hs" #-}
                                   )
                              -- use rule "src-ag/KWOrder.ag"(line 382, column 57)
                              _lhsOlocalSigMap =
                                  ({-# LINE 382 "src-ag/KWOrder.ag" #-}
                                   Map.empty
                                   {-# LINE 1704 "src-ag/KWOrder.hs" #-}
                                   )
                              -- use rule "src-ag/KWOrder.ag"(line 53, column 54)
                              _lhsOntDeps =
                                  ({-# LINE 53 "src-ag/KWOrder.ag" #-}
                                   mempty
                                   {-# LINE 1710 "src-ag/KWOrder.hs" #-}
                                   )
                              -- use rule "src-ag/KWOrder.ag"(line 53, column 54)
                              _lhsOntHoDeps =
                                  ({-# LINE 53 "src-ag/KWOrder.ag" #-}
                                   mempty
                                   {-# LINE 1716 "src-ag/KWOrder.hs" #-}
                                   )
                              -- use rule "src-ag/DistChildAttr.ag"(line 4, column 53)
                              _lhsOsynMap' =
                                  ({-# LINE 4 "src-ag/DistChildAttr.ag" #-}
                                   Map.empty
                                   {-# LINE 1722 "src-ag/KWOrder.hs" #-}
                                   )
                              -- use rule "src-ag/KWOrder.ag"(line 371, column 33)
                              _lhsOsynmap =
                                  ({-# LINE 371 "src-ag/KWOrder.ag" #-}
                                   Map.empty
                                   {-# LINE 1728 "src-ag/KWOrder.hs" #-}
                                   )
                              -- copy rule (chain)
                              _lhsOrulenumber =
                                  ({-# LINE 40 "src-ag/KWOrder.ag" #-}
                                   _lhsIrulenumber
                                   {-# LINE 1734 "src-ag/KWOrder.hs" #-}
                                   )
                          in  ( _lhsOdepinfo,_lhsOinhMap',_lhsOinhmap,_lhsOlocalSigMap,_lhsOntDeps,_lhsOntHoDeps,_lhsOrulenumber,_lhsOsynMap',_lhsOsynmap))) )
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
                     T_Pattern 
sem_Pattern_Alias field_ attr_ (T_Pattern pat_ )  =
    (T_Pattern (let _lhsOvertices :: (Set.Set Vertex)
                    _lhsOcopy :: Pattern 
                    _patIcopy :: Pattern 
                    _patIvertices :: (Set.Set Vertex)
                    -- "src-ag/KWOrder.ag"(line 204, column 12)
                    _vertex =
                        ({-# LINE 204 "src-ag/KWOrder.ag" #-}
                         if                  field_ == _INST then VChild attr_
                         else VAttr (if      field_ == _LHS  then Syn
                                     else if field_ == _LOC  then Loc
                                     else                         Inh) field_ attr_
                         {-# LINE 1809 "src-ag/KWOrder.hs" #-}
                         )
                    -- "src-ag/KWOrder.ag"(line 208, column 12)
                    _lhsOvertices =
                        ({-# LINE 208 "src-ag/KWOrder.ag" #-}
                         Set.insert _vertex     _patIvertices
                         {-# LINE 1815 "src-ag/KWOrder.hs" #-}
                         )
                    -- self rule
                    _copy =
                        ({-# LINE 22 "src-ag/Patterns.ag" #-}
                         Alias field_ attr_ _patIcopy
                         {-# LINE 1821 "src-ag/KWOrder.hs" #-}
                         )
                    -- self rule
                    _lhsOcopy =
                        ({-# LINE 22 "src-ag/Patterns.ag" #-}
                         _copy
                         {-# LINE 1827 "src-ag/KWOrder.hs" #-}
                         )
                    ( _patIcopy,_patIvertices) =
                        pat_ 
                in  ( _lhsOcopy,_lhsOvertices)) )
sem_Pattern_Constr :: ConstructorIdent ->
                      T_Patterns  ->
                      T_Pattern 
sem_Pattern_Constr name_ (T_Patterns pats_ )  =
    (T_Pattern (let _lhsOvertices :: (Set.Set Vertex)
                    _lhsOcopy :: Pattern 
                    _patsIcopy :: Patterns 
                    _patsIvertices :: (Set.Set Vertex)
                    -- use rule "src-ag/KWOrder.ag"(line 188, column 36)
                    _lhsOvertices =
                        ({-# LINE 188 "src-ag/KWOrder.ag" #-}
                         _patsIvertices
                         {-# LINE 1844 "src-ag/KWOrder.hs" #-}
                         )
                    -- self rule
                    _copy =
                        ({-# LINE 22 "src-ag/Patterns.ag" #-}
                         Constr name_ _patsIcopy
                         {-# LINE 1850 "src-ag/KWOrder.hs" #-}
                         )
                    -- self rule
                    _lhsOcopy =
                        ({-# LINE 22 "src-ag/Patterns.ag" #-}
                         _copy
                         {-# LINE 1856 "src-ag/KWOrder.hs" #-}
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
                    -- use rule "src-ag/KWOrder.ag"(line 188, column 36)
                    _lhsOvertices =
                        ({-# LINE 188 "src-ag/KWOrder.ag" #-}
                         _patIvertices
                         {-# LINE 1872 "src-ag/KWOrder.hs" #-}
                         )
                    -- self rule
                    _copy =
                        ({-# LINE 22 "src-ag/Patterns.ag" #-}
                         Irrefutable _patIcopy
                         {-# LINE 1878 "src-ag/KWOrder.hs" #-}
                         )
                    -- self rule
                    _lhsOcopy =
                        ({-# LINE 22 "src-ag/Patterns.ag" #-}
                         _copy
                         {-# LINE 1884 "src-ag/KWOrder.hs" #-}
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
                    -- use rule "src-ag/KWOrder.ag"(line 188, column 36)
                    _lhsOvertices =
                        ({-# LINE 188 "src-ag/KWOrder.ag" #-}
                         _patsIvertices
                         {-# LINE 1901 "src-ag/KWOrder.hs" #-}
                         )
                    -- self rule
                    _copy =
                        ({-# LINE 22 "src-ag/Patterns.ag" #-}
                         Product pos_ _patsIcopy
                         {-# LINE 1907 "src-ag/KWOrder.hs" #-}
                         )
                    -- self rule
                    _lhsOcopy =
                        ({-# LINE 22 "src-ag/Patterns.ag" #-}
                         _copy
                         {-# LINE 1913 "src-ag/KWOrder.hs" #-}
                         )
                    ( _patsIcopy,_patsIvertices) =
                        pats_ 
                in  ( _lhsOcopy,_lhsOvertices)) )
sem_Pattern_Underscore :: Pos ->
                          T_Pattern 
sem_Pattern_Underscore pos_  =
    (T_Pattern (let _lhsOvertices :: (Set.Set Vertex)
                    _lhsOcopy :: Pattern 
                    -- use rule "src-ag/KWOrder.ag"(line 188, column 36)
                    _lhsOvertices =
                        ({-# LINE 188 "src-ag/KWOrder.ag" #-}
                         Set.empty
                         {-# LINE 1927 "src-ag/KWOrder.hs" #-}
                         )
                    -- self rule
                    _copy =
                        ({-# LINE 22 "src-ag/Patterns.ag" #-}
                         Underscore pos_
                         {-# LINE 1933 "src-ag/KWOrder.hs" #-}
                         )
                    -- self rule
                    _lhsOcopy =
                        ({-# LINE 22 "src-ag/Patterns.ag" #-}
                         _copy
                         {-# LINE 1939 "src-ag/KWOrder.hs" #-}
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
                     -- use rule "src-ag/KWOrder.ag"(line 188, column 36)
                     _lhsOvertices =
                         ({-# LINE 188 "src-ag/KWOrder.ag" #-}
                          _hdIvertices `Set.union` _tlIvertices
                          {-# LINE 1987 "src-ag/KWOrder.hs" #-}
                          )
                     -- self rule
                     _copy =
                         ({-# LINE 22 "src-ag/Patterns.ag" #-}
                          (:) _hdIcopy _tlIcopy
                          {-# LINE 1993 "src-ag/KWOrder.hs" #-}
                          )
                     -- self rule
                     _lhsOcopy =
                         ({-# LINE 22 "src-ag/Patterns.ag" #-}
                          _copy
                          {-# LINE 1999 "src-ag/KWOrder.hs" #-}
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
                     -- use rule "src-ag/KWOrder.ag"(line 188, column 36)
                     _lhsOvertices =
                         ({-# LINE 188 "src-ag/KWOrder.ag" #-}
                          Set.empty
                          {-# LINE 2014 "src-ag/KWOrder.hs" #-}
                          )
                     -- self rule
                     _copy =
                         ({-# LINE 22 "src-ag/Patterns.ag" #-}
                          []
                          {-# LINE 2020 "src-ag/KWOrder.hs" #-}
                          )
                     -- self rule
                     _lhsOcopy =
                         ({-# LINE 22 "src-ag/Patterns.ag" #-}
                          _copy
                          {-# LINE 2026 "src-ag/KWOrder.hs" #-}
                          )
                 in  ( _lhsOcopy,_lhsOvertices)) )
-- Production --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         aroundMap            : Map ConstructorIdent (Map Identifier [Expression])
         inhMap               : Map Identifier Attributes
         manualDeps           : Map ConstructorIdent (Set Dependency)
         mergeMap             : Map ConstructorIdent (Map Identifier (Identifier, [Identifier], Expression))
         options              : Options
         synMap               : Map Identifier Attributes
      chained attribute:
         rulenumber           : Int
      synthesized attributes:
         depgraph             : ProdDependencyGraph
         localSigMap          : Map.Map ConstructorIdent (Map.Map Identifier Type)
         refHoNts             : Set NontermIdent
         refNts               : Set NontermIdent
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
            local aroundMap   : _
            local mergeMap    : _
            local mergedChildren : _
            local vertices    : _
            local manualDeps  : _
            local manualEdges : _
            local edges       : _
-}
-- cata
sem_Production :: Production  ->
                  T_Production 
sem_Production (Production _con _params _constraints _children _rules _typeSigs _macro )  =
    (sem_Production_Production _con _params _constraints (sem_Children _children ) (sem_Rules _rules ) (sem_TypeSigs _typeSigs ) _macro )
-- semantic domain
newtype T_Production  = T_Production ((Map ConstructorIdent (Map Identifier [Expression])) ->
                                      (Map Identifier Attributes) ->
                                      (Map ConstructorIdent (Set Dependency)) ->
                                      (Map ConstructorIdent (Map Identifier (Identifier, [Identifier], Expression))) ->
                                      Options ->
                                      Int ->
                                      (Map Identifier Attributes) ->
                                      ( ProdDependencyGraph,(Map.Map ConstructorIdent (Map.Map Identifier Type)),(Set NontermIdent),(Set NontermIdent),Int))
data Inh_Production  = Inh_Production {aroundMap_Inh_Production :: (Map ConstructorIdent (Map Identifier [Expression])),inhMap_Inh_Production :: (Map Identifier Attributes),manualDeps_Inh_Production :: (Map ConstructorIdent (Set Dependency)),mergeMap_Inh_Production :: (Map ConstructorIdent (Map Identifier (Identifier, [Identifier], Expression))),options_Inh_Production :: Options,rulenumber_Inh_Production :: Int,synMap_Inh_Production :: (Map Identifier Attributes)}
data Syn_Production  = Syn_Production {depgraph_Syn_Production :: ProdDependencyGraph,localSigMap_Syn_Production :: (Map.Map ConstructorIdent (Map.Map Identifier Type)),refHoNts_Syn_Production :: (Set NontermIdent),refNts_Syn_Production :: (Set NontermIdent),rulenumber_Syn_Production :: Int}
wrap_Production :: T_Production  ->
                   Inh_Production  ->
                   Syn_Production 
wrap_Production (T_Production sem ) (Inh_Production _lhsIaroundMap _lhsIinhMap _lhsImanualDeps _lhsImergeMap _lhsIoptions _lhsIrulenumber _lhsIsynMap )  =
    (let ( _lhsOdepgraph,_lhsOlocalSigMap,_lhsOrefHoNts,_lhsOrefNts,_lhsOrulenumber) = sem _lhsIaroundMap _lhsIinhMap _lhsImanualDeps _lhsImergeMap _lhsIoptions _lhsIrulenumber _lhsIsynMap 
     in  (Syn_Production _lhsOdepgraph _lhsOlocalSigMap _lhsOrefHoNts _lhsOrefNts _lhsOrulenumber ))
sem_Production_Production :: ConstructorIdent ->
                             ([Identifier]) ->
                             ([Type]) ->
                             T_Children  ->
                             T_Rules  ->
                             T_TypeSigs  ->
                             MaybeMacro ->
                             T_Production 
sem_Production_Production con_ params_ constraints_ (T_Children children_ ) (T_Rules rules_ ) (T_TypeSigs typeSigs_ ) macro_  =
    (T_Production (\ _lhsIaroundMap
                     _lhsIinhMap
                     _lhsImanualDeps
                     _lhsImergeMap
                     _lhsIoptions
                     _lhsIrulenumber
                     _lhsIsynMap ->
                       (let _lhsOdepgraph :: ProdDependencyGraph
                            _lhsOlocalSigMap :: (Map.Map ConstructorIdent (Map.Map Identifier Type))
                            _lhsOrefHoNts :: (Set NontermIdent)
                            _lhsOrefNts :: (Set NontermIdent)
                            _lhsOrulenumber :: Int
                            _childrenOaroundMap :: (Map Identifier [Expression])
                            _childrenOinhMap :: (Map Identifier Attributes)
                            _childrenOmergeMap :: (Map Identifier (Identifier, [Identifier], Expression))
                            _childrenOmergedChildren :: (Set Identifier)
                            _childrenOoptions :: Options
                            _childrenOsynMap :: (Map Identifier Attributes)
                            _rulesOrulenumber :: Int
                            _childrenIechilds :: EChildren
                            _childrenIedges :: (Set.Set Edge)
                            _childrenInontnames :: ([(Identifier, Identifier)])
                            _childrenIrefHoNts :: (Set NontermIdent)
                            _childrenIrefNts :: (Set NontermIdent)
                            _childrenIvertices :: (Set.Set Vertex)
                            _rulesIedges :: (Set.Set Edge)
                            _rulesIerules :: ERules
                            _rulesIrulenumber :: Int
                            _rulesIvertices :: (Set.Set Vertex)
                            _typeSigsIlocalSigMap :: (Map Identifier Type)
                            -- "src-ag/KWOrder.ag"(line 101, column 32)
                            _aroundMap =
                                ({-# LINE 101 "src-ag/KWOrder.ag" #-}
                                 Map.findWithDefault Map.empty con_ _lhsIaroundMap
                                 {-# LINE 2129 "src-ag/KWOrder.hs" #-}
                                 )
                            -- "src-ag/KWOrder.ag"(line 126, column 32)
                            _mergeMap =
                                ({-# LINE 126 "src-ag/KWOrder.ag" #-}
                                 Map.findWithDefault Map.empty con_ _lhsImergeMap
                                 {-# LINE 2135 "src-ag/KWOrder.hs" #-}
                                 )
                            -- "src-ag/KWOrder.ag"(line 132, column 3)
                            _mergedChildren =
                                ({-# LINE 132 "src-ag/KWOrder.ag" #-}
                                 Set.unions [ Set.fromList ms | (_,ms,_) <- Map.elems _mergeMap     ]
                                 {-# LINE 2141 "src-ag/KWOrder.hs" #-}
                                 )
                            -- "src-ag/KWOrder.ag"(line 228, column 17)
                            _vertices =
                                ({-# LINE 228 "src-ag/KWOrder.ag" #-}
                                 _rulesIvertices `Set.union` _childrenIvertices
                                 {-# LINE 2147 "src-ag/KWOrder.hs" #-}
                                 )
                            -- "src-ag/KWOrder.ag"(line 271, column 3)
                            _manualDeps =
                                ({-# LINE 271 "src-ag/KWOrder.ag" #-}
                                 Map.findWithDefault Set.empty con_ _lhsImanualDeps
                                 {-# LINE 2153 "src-ag/KWOrder.hs" #-}
                                 )
                            -- "src-ag/KWOrder.ag"(line 272, column 3)
                            _manualEdges =
                                ({-# LINE 272 "src-ag/KWOrder.ag" #-}
                                 Set.map depToEdge _manualDeps
                                 {-# LINE 2159 "src-ag/KWOrder.hs" #-}
                                 )
                            -- "src-ag/KWOrder.ag"(line 294, column 17)
                            _edges =
                                ({-# LINE 294 "src-ag/KWOrder.ag" #-}
                                 _rulesIedges `Set.union` _childrenIedges
                                 {-# LINE 2165 "src-ag/KWOrder.hs" #-}
                                 )
                            -- "src-ag/KWOrder.ag"(line 309, column 17)
                            _lhsOdepgraph =
                                ({-# LINE 309 "src-ag/KWOrder.ag" #-}
                                 ProdDependencyGraph { pdgVertices    = Set.toList _vertices
                                                     , pdgEdges       = Set.toList _edges
                                                     , pdgRules       = _rulesIerules
                                                     , pdgChilds      = _childrenIechilds
                                                     , pdgProduction  = con_
                                                     , pdgChildMap    = _childrenInontnames
                                                     , pdgConstraints = constraints_
                                                     , pdgParams      = params_ }
                                 {-# LINE 2178 "src-ag/KWOrder.hs" #-}
                                 )
                            -- "src-ag/KWOrder.ag"(line 387, column 32)
                            _lhsOlocalSigMap =
                                ({-# LINE 387 "src-ag/KWOrder.ag" #-}
                                 Map.singleton con_ _typeSigsIlocalSigMap
                                 {-# LINE 2184 "src-ag/KWOrder.hs" #-}
                                 )
                            -- use rule "src-ag/KWOrder.ag"(line 55, column 67)
                            _lhsOrefHoNts =
                                ({-# LINE 55 "src-ag/KWOrder.ag" #-}
                                 _childrenIrefHoNts
                                 {-# LINE 2190 "src-ag/KWOrder.hs" #-}
                                 )
                            -- use rule "src-ag/KWOrder.ag"(line 55, column 67)
                            _lhsOrefNts =
                                ({-# LINE 55 "src-ag/KWOrder.ag" #-}
                                 _childrenIrefNts
                                 {-# LINE 2196 "src-ag/KWOrder.hs" #-}
                                 )
                            -- copy rule (up)
                            _lhsOrulenumber =
                                ({-# LINE 40 "src-ag/KWOrder.ag" #-}
                                 _rulesIrulenumber
                                 {-# LINE 2202 "src-ag/KWOrder.hs" #-}
                                 )
                            -- copy rule (from local)
                            _childrenOaroundMap =
                                ({-# LINE 98 "src-ag/KWOrder.ag" #-}
                                 _aroundMap
                                 {-# LINE 2208 "src-ag/KWOrder.hs" #-}
                                 )
                            -- copy rule (down)
                            _childrenOinhMap =
                                ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                 _lhsIinhMap
                                 {-# LINE 2214 "src-ag/KWOrder.hs" #-}
                                 )
                            -- copy rule (from local)
                            _childrenOmergeMap =
                                ({-# LINE 123 "src-ag/KWOrder.ag" #-}
                                 _mergeMap
                                 {-# LINE 2220 "src-ag/KWOrder.hs" #-}
                                 )
                            -- copy rule (from local)
                            _childrenOmergedChildren =
                                ({-# LINE 123 "src-ag/KWOrder.ag" #-}
                                 _mergedChildren
                                 {-# LINE 2226 "src-ag/KWOrder.hs" #-}
                                 )
                            -- copy rule (down)
                            _childrenOoptions =
                                ({-# LINE 33 "src-ag/KWOrder.ag" #-}
                                 _lhsIoptions
                                 {-# LINE 2232 "src-ag/KWOrder.hs" #-}
                                 )
                            -- copy rule (down)
                            _childrenOsynMap =
                                ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                 _lhsIsynMap
                                 {-# LINE 2238 "src-ag/KWOrder.hs" #-}
                                 )
                            -- copy rule (down)
                            _rulesOrulenumber =
                                ({-# LINE 40 "src-ag/KWOrder.ag" #-}
                                 _lhsIrulenumber
                                 {-# LINE 2244 "src-ag/KWOrder.hs" #-}
                                 )
                            ( _childrenIechilds,_childrenIedges,_childrenInontnames,_childrenIrefHoNts,_childrenIrefNts,_childrenIvertices) =
                                children_ _childrenOaroundMap _childrenOinhMap _childrenOmergeMap _childrenOmergedChildren _childrenOoptions _childrenOsynMap 
                            ( _rulesIedges,_rulesIerules,_rulesIrulenumber,_rulesIvertices) =
                                rules_ _rulesOrulenumber 
                            ( _typeSigsIlocalSigMap) =
                                typeSigs_ 
                        in  ( _lhsOdepgraph,_lhsOlocalSigMap,_lhsOrefHoNts,_lhsOrefNts,_lhsOrulenumber))) )
-- Productions -------------------------------------------------
{-
   visit 0:
      inherited attributes:
         aroundMap            : Map ConstructorIdent (Map Identifier [Expression])
         inhMap               : Map Identifier Attributes
         manualDeps           : Map ConstructorIdent (Set Dependency)
         mergeMap             : Map ConstructorIdent (Map Identifier (Identifier, [Identifier], Expression))
         options              : Options
         synMap               : Map Identifier Attributes
      chained attribute:
         rulenumber           : Int
      synthesized attributes:
         depgraph             : [ProdDependencyGraph]
         localSigMap          : Map.Map ConstructorIdent (Map.Map Identifier Type)
         refHoNts             : Set NontermIdent
         refNts               : Set NontermIdent
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
newtype T_Productions  = T_Productions ((Map ConstructorIdent (Map Identifier [Expression])) ->
                                        (Map Identifier Attributes) ->
                                        (Map ConstructorIdent (Set Dependency)) ->
                                        (Map ConstructorIdent (Map Identifier (Identifier, [Identifier], Expression))) ->
                                        Options ->
                                        Int ->
                                        (Map Identifier Attributes) ->
                                        ( ([ProdDependencyGraph]),(Map.Map ConstructorIdent (Map.Map Identifier Type)),(Set NontermIdent),(Set NontermIdent),Int))
data Inh_Productions  = Inh_Productions {aroundMap_Inh_Productions :: (Map ConstructorIdent (Map Identifier [Expression])),inhMap_Inh_Productions :: (Map Identifier Attributes),manualDeps_Inh_Productions :: (Map ConstructorIdent (Set Dependency)),mergeMap_Inh_Productions :: (Map ConstructorIdent (Map Identifier (Identifier, [Identifier], Expression))),options_Inh_Productions :: Options,rulenumber_Inh_Productions :: Int,synMap_Inh_Productions :: (Map Identifier Attributes)}
data Syn_Productions  = Syn_Productions {depgraph_Syn_Productions :: ([ProdDependencyGraph]),localSigMap_Syn_Productions :: (Map.Map ConstructorIdent (Map.Map Identifier Type)),refHoNts_Syn_Productions :: (Set NontermIdent),refNts_Syn_Productions :: (Set NontermIdent),rulenumber_Syn_Productions :: Int}
wrap_Productions :: T_Productions  ->
                    Inh_Productions  ->
                    Syn_Productions 
wrap_Productions (T_Productions sem ) (Inh_Productions _lhsIaroundMap _lhsIinhMap _lhsImanualDeps _lhsImergeMap _lhsIoptions _lhsIrulenumber _lhsIsynMap )  =
    (let ( _lhsOdepgraph,_lhsOlocalSigMap,_lhsOrefHoNts,_lhsOrefNts,_lhsOrulenumber) = sem _lhsIaroundMap _lhsIinhMap _lhsImanualDeps _lhsImergeMap _lhsIoptions _lhsIrulenumber _lhsIsynMap 
     in  (Syn_Productions _lhsOdepgraph _lhsOlocalSigMap _lhsOrefHoNts _lhsOrefNts _lhsOrulenumber ))
sem_Productions_Cons :: T_Production  ->
                        T_Productions  ->
                        T_Productions 
sem_Productions_Cons (T_Production hd_ ) (T_Productions tl_ )  =
    (T_Productions (\ _lhsIaroundMap
                      _lhsIinhMap
                      _lhsImanualDeps
                      _lhsImergeMap
                      _lhsIoptions
                      _lhsIrulenumber
                      _lhsIsynMap ->
                        (let _lhsOdepgraph :: ([ProdDependencyGraph])
                             _lhsOlocalSigMap :: (Map.Map ConstructorIdent (Map.Map Identifier Type))
                             _lhsOrefHoNts :: (Set NontermIdent)
                             _lhsOrefNts :: (Set NontermIdent)
                             _lhsOrulenumber :: Int
                             _hdOaroundMap :: (Map ConstructorIdent (Map Identifier [Expression]))
                             _hdOinhMap :: (Map Identifier Attributes)
                             _hdOmanualDeps :: (Map ConstructorIdent (Set Dependency))
                             _hdOmergeMap :: (Map ConstructorIdent (Map Identifier (Identifier, [Identifier], Expression)))
                             _hdOoptions :: Options
                             _hdOrulenumber :: Int
                             _hdOsynMap :: (Map Identifier Attributes)
                             _tlOaroundMap :: (Map ConstructorIdent (Map Identifier [Expression]))
                             _tlOinhMap :: (Map Identifier Attributes)
                             _tlOmanualDeps :: (Map ConstructorIdent (Set Dependency))
                             _tlOmergeMap :: (Map ConstructorIdent (Map Identifier (Identifier, [Identifier], Expression)))
                             _tlOoptions :: Options
                             _tlOrulenumber :: Int
                             _tlOsynMap :: (Map Identifier Attributes)
                             _hdIdepgraph :: ProdDependencyGraph
                             _hdIlocalSigMap :: (Map.Map ConstructorIdent (Map.Map Identifier Type))
                             _hdIrefHoNts :: (Set NontermIdent)
                             _hdIrefNts :: (Set NontermIdent)
                             _hdIrulenumber :: Int
                             _tlIdepgraph :: ([ProdDependencyGraph])
                             _tlIlocalSigMap :: (Map.Map ConstructorIdent (Map.Map Identifier Type))
                             _tlIrefHoNts :: (Set NontermIdent)
                             _tlIrefNts :: (Set NontermIdent)
                             _tlIrulenumber :: Int
                             -- use rule "src-ag/KWOrder.ag"(line 306, column 33)
                             _lhsOdepgraph =
                                 ({-# LINE 306 "src-ag/KWOrder.ag" #-}
                                  _hdIdepgraph : _tlIdepgraph
                                  {-# LINE 2342 "src-ag/KWOrder.hs" #-}
                                  )
                             -- use rule "src-ag/KWOrder.ag"(line 383, column 57)
                             _lhsOlocalSigMap =
                                 ({-# LINE 383 "src-ag/KWOrder.ag" #-}
                                  _hdIlocalSigMap `Map.union` _tlIlocalSigMap
                                  {-# LINE 2348 "src-ag/KWOrder.hs" #-}
                                  )
                             -- use rule "src-ag/KWOrder.ag"(line 55, column 67)
                             _lhsOrefHoNts =
                                 ({-# LINE 55 "src-ag/KWOrder.ag" #-}
                                  _hdIrefHoNts `mappend` _tlIrefHoNts
                                  {-# LINE 2354 "src-ag/KWOrder.hs" #-}
                                  )
                             -- use rule "src-ag/KWOrder.ag"(line 55, column 67)
                             _lhsOrefNts =
                                 ({-# LINE 55 "src-ag/KWOrder.ag" #-}
                                  _hdIrefNts `mappend` _tlIrefNts
                                  {-# LINE 2360 "src-ag/KWOrder.hs" #-}
                                  )
                             -- copy rule (up)
                             _lhsOrulenumber =
                                 ({-# LINE 40 "src-ag/KWOrder.ag" #-}
                                  _tlIrulenumber
                                  {-# LINE 2366 "src-ag/KWOrder.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOaroundMap =
                                 ({-# LINE 95 "src-ag/KWOrder.ag" #-}
                                  _lhsIaroundMap
                                  {-# LINE 2372 "src-ag/KWOrder.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOinhMap =
                                 ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                  _lhsIinhMap
                                  {-# LINE 2378 "src-ag/KWOrder.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOmanualDeps =
                                 ({-# LINE 266 "src-ag/KWOrder.ag" #-}
                                  _lhsImanualDeps
                                  {-# LINE 2384 "src-ag/KWOrder.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOmergeMap =
                                 ({-# LINE 120 "src-ag/KWOrder.ag" #-}
                                  _lhsImergeMap
                                  {-# LINE 2390 "src-ag/KWOrder.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOoptions =
                                 ({-# LINE 33 "src-ag/KWOrder.ag" #-}
                                  _lhsIoptions
                                  {-# LINE 2396 "src-ag/KWOrder.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOrulenumber =
                                 ({-# LINE 40 "src-ag/KWOrder.ag" #-}
                                  _lhsIrulenumber
                                  {-# LINE 2402 "src-ag/KWOrder.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOsynMap =
                                 ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                  _lhsIsynMap
                                  {-# LINE 2408 "src-ag/KWOrder.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOaroundMap =
                                 ({-# LINE 95 "src-ag/KWOrder.ag" #-}
                                  _lhsIaroundMap
                                  {-# LINE 2414 "src-ag/KWOrder.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOinhMap =
                                 ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                  _lhsIinhMap
                                  {-# LINE 2420 "src-ag/KWOrder.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOmanualDeps =
                                 ({-# LINE 266 "src-ag/KWOrder.ag" #-}
                                  _lhsImanualDeps
                                  {-# LINE 2426 "src-ag/KWOrder.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOmergeMap =
                                 ({-# LINE 120 "src-ag/KWOrder.ag" #-}
                                  _lhsImergeMap
                                  {-# LINE 2432 "src-ag/KWOrder.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOoptions =
                                 ({-# LINE 33 "src-ag/KWOrder.ag" #-}
                                  _lhsIoptions
                                  {-# LINE 2438 "src-ag/KWOrder.hs" #-}
                                  )
                             -- copy rule (chain)
                             _tlOrulenumber =
                                 ({-# LINE 40 "src-ag/KWOrder.ag" #-}
                                  _hdIrulenumber
                                  {-# LINE 2444 "src-ag/KWOrder.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOsynMap =
                                 ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                  _lhsIsynMap
                                  {-# LINE 2450 "src-ag/KWOrder.hs" #-}
                                  )
                             ( _hdIdepgraph,_hdIlocalSigMap,_hdIrefHoNts,_hdIrefNts,_hdIrulenumber) =
                                 hd_ _hdOaroundMap _hdOinhMap _hdOmanualDeps _hdOmergeMap _hdOoptions _hdOrulenumber _hdOsynMap 
                             ( _tlIdepgraph,_tlIlocalSigMap,_tlIrefHoNts,_tlIrefNts,_tlIrulenumber) =
                                 tl_ _tlOaroundMap _tlOinhMap _tlOmanualDeps _tlOmergeMap _tlOoptions _tlOrulenumber _tlOsynMap 
                         in  ( _lhsOdepgraph,_lhsOlocalSigMap,_lhsOrefHoNts,_lhsOrefNts,_lhsOrulenumber))) )
sem_Productions_Nil :: T_Productions 
sem_Productions_Nil  =
    (T_Productions (\ _lhsIaroundMap
                      _lhsIinhMap
                      _lhsImanualDeps
                      _lhsImergeMap
                      _lhsIoptions
                      _lhsIrulenumber
                      _lhsIsynMap ->
                        (let _lhsOdepgraph :: ([ProdDependencyGraph])
                             _lhsOlocalSigMap :: (Map.Map ConstructorIdent (Map.Map Identifier Type))
                             _lhsOrefHoNts :: (Set NontermIdent)
                             _lhsOrefNts :: (Set NontermIdent)
                             _lhsOrulenumber :: Int
                             -- use rule "src-ag/KWOrder.ag"(line 306, column 33)
                             _lhsOdepgraph =
                                 ({-# LINE 306 "src-ag/KWOrder.ag" #-}
                                  []
                                  {-# LINE 2475 "src-ag/KWOrder.hs" #-}
                                  )
                             -- use rule "src-ag/KWOrder.ag"(line 383, column 57)
                             _lhsOlocalSigMap =
                                 ({-# LINE 383 "src-ag/KWOrder.ag" #-}
                                  Map.empty
                                  {-# LINE 2481 "src-ag/KWOrder.hs" #-}
                                  )
                             -- use rule "src-ag/KWOrder.ag"(line 55, column 67)
                             _lhsOrefHoNts =
                                 ({-# LINE 55 "src-ag/KWOrder.ag" #-}
                                  mempty
                                  {-# LINE 2487 "src-ag/KWOrder.hs" #-}
                                  )
                             -- use rule "src-ag/KWOrder.ag"(line 55, column 67)
                             _lhsOrefNts =
                                 ({-# LINE 55 "src-ag/KWOrder.ag" #-}
                                  mempty
                                  {-# LINE 2493 "src-ag/KWOrder.hs" #-}
                                  )
                             -- copy rule (chain)
                             _lhsOrulenumber =
                                 ({-# LINE 40 "src-ag/KWOrder.ag" #-}
                                  _lhsIrulenumber
                                  {-# LINE 2499 "src-ag/KWOrder.hs" #-}
                                  )
                         in  ( _lhsOdepgraph,_lhsOlocalSigMap,_lhsOrefHoNts,_lhsOrefNts,_lhsOrulenumber))) )
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
         child pure           : {Bool}
         child identity       : {Bool}
         child mbError        : {Maybe Error}
         child eager          : {Bool}
         visit 0:
            local rulename    : _
            local vertex      : _
            local edgesout    : _
            local edgesin     : _
-}
-- cata
sem_Rule :: Rule  ->
            T_Rule 
sem_Rule (Rule _mbName _pattern _rhs _owrt _origin _explicit _pure _identity _mbError _eager )  =
    (sem_Rule_Rule _mbName (sem_Pattern _pattern ) (sem_Expression _rhs ) _owrt _origin _explicit _pure _identity _mbError _eager )
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
                 Bool ->
                 Bool ->
                 (Maybe Error) ->
                 Bool ->
                 T_Rule 
sem_Rule_Rule mbName_ (T_Pattern pattern_ ) (T_Expression rhs_ ) owrt_ origin_ explicit_ pure_ identity_ mbError_ eager_  =
    (T_Rule (\ _lhsIrulenumber ->
                 (let _lhsOrulenumber :: Int
                      _lhsOerules :: ERule
                      _lhsOvertices :: (Set.Set Vertex)
                      _lhsOedges :: (Set.Set Edge)
                      _patternIcopy :: Pattern 
                      _patternIvertices :: (Set.Set Vertex)
                      _rhsIcopy :: Expression 
                      _rhsIvertices :: (Set.Set Vertex)
                      -- "src-ag/KWOrder.ag"(line 46, column 11)
                      _lhsOrulenumber =
                          ({-# LINE 46 "src-ag/KWOrder.ag" #-}
                           _lhsIrulenumber + 1
                           {-# LINE 2570 "src-ag/KWOrder.hs" #-}
                           )
                      -- "src-ag/KWOrder.ag"(line 47, column 11)
                      _rulename =
                          ({-# LINE 47 "src-ag/KWOrder.ag" #-}
                           maybe (identifier $ "rule" ++ show _lhsIrulenumber) id mbName_
                           {-# LINE 2576 "src-ag/KWOrder.hs" #-}
                           )
                      -- "src-ag/KWOrder.ag"(line 159, column 10)
                      _lhsOerules =
                          ({-# LINE 159 "src-ag/KWOrder.ag" #-}
                           ERule _rulename
                                 _patternIcopy
                                 _rhsIcopy
                                 owrt_
                                 origin_
                                 explicit_
                                 pure_
                                 mbError_
                           {-# LINE 2589 "src-ag/KWOrder.hs" #-}
                           )
                      -- "src-ag/KWOrder.ag"(line 223, column 11)
                      _vertex =
                          ({-# LINE 223 "src-ag/KWOrder.ag" #-}
                           VRule _rulename
                           {-# LINE 2595 "src-ag/KWOrder.hs" #-}
                           )
                      -- "src-ag/KWOrder.ag"(line 224, column 11)
                      _lhsOvertices =
                          ({-# LINE 224 "src-ag/KWOrder.ag" #-}
                           Set.insert _vertex     $ _patternIvertices `Set.union` _rhsIvertices
                           {-# LINE 2601 "src-ag/KWOrder.hs" #-}
                           )
                      -- "src-ag/KWOrder.ag"(line 236, column 11)
                      _edgesout =
                          ({-# LINE 236 "src-ag/KWOrder.ag" #-}
                           map ((,) _vertex    ) (Set.toList _rhsIvertices)
                           {-# LINE 2607 "src-ag/KWOrder.hs" #-}
                           )
                      -- "src-ag/KWOrder.ag"(line 237, column 11)
                      _edgesin =
                          ({-# LINE 237 "src-ag/KWOrder.ag" #-}
                           map (flip (,) _vertex    ) (Set.toList _patternIvertices)
                           {-# LINE 2613 "src-ag/KWOrder.hs" #-}
                           )
                      -- "src-ag/KWOrder.ag"(line 238, column 11)
                      _lhsOedges =
                          ({-# LINE 238 "src-ag/KWOrder.ag" #-}
                           Set.fromList $ _edgesout     ++ _edgesin
                           {-# LINE 2619 "src-ag/KWOrder.hs" #-}
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
                       -- use rule "src-ag/KWOrder.ag"(line 232, column 33)
                       _lhsOedges =
                           ({-# LINE 232 "src-ag/KWOrder.ag" #-}
                            _hdIedges `Set.union` _tlIedges
                            {-# LINE 2680 "src-ag/KWOrder.hs" #-}
                            )
                       -- use rule "src-ag/KWOrder.ag"(line 156, column 25)
                       _lhsOerules =
                           ({-# LINE 156 "src-ag/KWOrder.ag" #-}
                            _hdIerules : _tlIerules
                            {-# LINE 2686 "src-ag/KWOrder.hs" #-}
                            )
                       -- use rule "src-ag/KWOrder.ag"(line 188, column 36)
                       _lhsOvertices =
                           ({-# LINE 188 "src-ag/KWOrder.ag" #-}
                            _hdIvertices `Set.union` _tlIvertices
                            {-# LINE 2692 "src-ag/KWOrder.hs" #-}
                            )
                       -- copy rule (up)
                       _lhsOrulenumber =
                           ({-# LINE 40 "src-ag/KWOrder.ag" #-}
                            _tlIrulenumber
                            {-# LINE 2698 "src-ag/KWOrder.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOrulenumber =
                           ({-# LINE 40 "src-ag/KWOrder.ag" #-}
                            _lhsIrulenumber
                            {-# LINE 2704 "src-ag/KWOrder.hs" #-}
                            )
                       -- copy rule (chain)
                       _tlOrulenumber =
                           ({-# LINE 40 "src-ag/KWOrder.ag" #-}
                            _hdIrulenumber
                            {-# LINE 2710 "src-ag/KWOrder.hs" #-}
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
                       -- use rule "src-ag/KWOrder.ag"(line 232, column 33)
                       _lhsOedges =
                           ({-# LINE 232 "src-ag/KWOrder.ag" #-}
                            Set.empty
                            {-# LINE 2728 "src-ag/KWOrder.hs" #-}
                            )
                       -- use rule "src-ag/KWOrder.ag"(line 156, column 25)
                       _lhsOerules =
                           ({-# LINE 156 "src-ag/KWOrder.ag" #-}
                            []
                            {-# LINE 2734 "src-ag/KWOrder.hs" #-}
                            )
                       -- use rule "src-ag/KWOrder.ag"(line 188, column 36)
                       _lhsOvertices =
                           ({-# LINE 188 "src-ag/KWOrder.ag" #-}
                            Set.empty
                            {-# LINE 2740 "src-ag/KWOrder.hs" #-}
                            )
                       -- copy rule (chain)
                       _lhsOrulenumber =
                           ({-# LINE 40 "src-ag/KWOrder.ag" #-}
                            _lhsIrulenumber
                            {-# LINE 2746 "src-ag/KWOrder.hs" #-}
                            )
                   in  ( _lhsOedges,_lhsOerules,_lhsOrulenumber,_lhsOvertices))) )
-- TypeSig -----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         localSigMap          : Map Identifier Type
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
newtype T_TypeSig  = T_TypeSig (( (Map Identifier Type)))
data Inh_TypeSig  = Inh_TypeSig {}
data Syn_TypeSig  = Syn_TypeSig {localSigMap_Syn_TypeSig :: (Map Identifier Type)}
wrap_TypeSig :: T_TypeSig  ->
                Inh_TypeSig  ->
                Syn_TypeSig 
wrap_TypeSig (T_TypeSig sem ) (Inh_TypeSig )  =
    (let ( _lhsOlocalSigMap) = sem 
     in  (Syn_TypeSig _lhsOlocalSigMap ))
sem_TypeSig_TypeSig :: Identifier ->
                       Type ->
                       T_TypeSig 
sem_TypeSig_TypeSig name_ tp_  =
    (T_TypeSig (let _lhsOlocalSigMap :: (Map Identifier Type)
                    -- "src-ag/KWOrder.ag"(line 388, column 32)
                    _lhsOlocalSigMap =
                        ({-# LINE 388 "src-ag/KWOrder.ag" #-}
                         Map.singleton name_ tp_
                         {-# LINE 2783 "src-ag/KWOrder.hs" #-}
                         )
                in  ( _lhsOlocalSigMap)) )
-- TypeSigs ----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         localSigMap          : Map Identifier Type
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
newtype T_TypeSigs  = T_TypeSigs (( (Map Identifier Type)))
data Inh_TypeSigs  = Inh_TypeSigs {}
data Syn_TypeSigs  = Syn_TypeSigs {localSigMap_Syn_TypeSigs :: (Map Identifier Type)}
wrap_TypeSigs :: T_TypeSigs  ->
                 Inh_TypeSigs  ->
                 Syn_TypeSigs 
wrap_TypeSigs (T_TypeSigs sem ) (Inh_TypeSigs )  =
    (let ( _lhsOlocalSigMap) = sem 
     in  (Syn_TypeSigs _lhsOlocalSigMap ))
sem_TypeSigs_Cons :: T_TypeSig  ->
                     T_TypeSigs  ->
                     T_TypeSigs 
sem_TypeSigs_Cons (T_TypeSig hd_ ) (T_TypeSigs tl_ )  =
    (T_TypeSigs (let _lhsOlocalSigMap :: (Map Identifier Type)
                     _hdIlocalSigMap :: (Map Identifier Type)
                     _tlIlocalSigMap :: (Map Identifier Type)
                     -- use rule "src-ag/KWOrder.ag"(line 384, column 57)
                     _lhsOlocalSigMap =
                         ({-# LINE 384 "src-ag/KWOrder.ag" #-}
                          _hdIlocalSigMap `Map.union` _tlIlocalSigMap
                          {-# LINE 2823 "src-ag/KWOrder.hs" #-}
                          )
                     ( _hdIlocalSigMap) =
                         hd_ 
                     ( _tlIlocalSigMap) =
                         tl_ 
                 in  ( _lhsOlocalSigMap)) )
sem_TypeSigs_Nil :: T_TypeSigs 
sem_TypeSigs_Nil  =
    (T_TypeSigs (let _lhsOlocalSigMap :: (Map Identifier Type)
                     -- use rule "src-ag/KWOrder.ag"(line 384, column 57)
                     _lhsOlocalSigMap =
                         ({-# LINE 384 "src-ag/KWOrder.ag" #-}
                          Map.empty
                          {-# LINE 2837 "src-ag/KWOrder.hs" #-}
                          )
                 in  ( _lhsOlocalSigMap)) )