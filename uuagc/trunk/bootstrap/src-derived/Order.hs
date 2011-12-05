

-- UUAGC 0.9.39.1.0 (src-ag/Order.ag)
module Order where
{-# LINE 9 "src-ag/Order.ag" #-}

-- From uuagc
import CommonTypes
import Patterns
import ErrorMessages
import AbstractSyntax
import Code hiding (Type)
import qualified Code
import Expression
import Options
import SequentialComputation
import SequentialTypes
import CodeSyntax
import GrammarInfo
import HsToken(HsTokensRoot(HsTokensRoot))
import HsTokenScanner(lexTokens)
import SemHsTokens(sem_HsTokensRoot,wrap_HsTokensRoot, Syn_HsTokensRoot(..),Inh_HsTokensRoot(..))
-- From uulib
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import Data.Map(Map)
import Data.Set(Set)
import Data.Sequence(Seq, (><))
import UU.Util.Utils
import UU.Scanner.Position(Pos(..),initPos)
import Data.Foldable(toList)

-- From haskell libraries
import Control.Monad(liftM)
import qualified Data.Array as Array
import Data.Array((!),bounds,inRange)
import Data.List(elemIndex,partition,sort,mapAccumL,find,nubBy,intersperse,groupBy,transpose)
import qualified Data.Tree as Tree
import Data.Maybe
{-# LINE 42 "dist/build/uuagc/uuagc-tmp/Order.hs" #-}

{-# LINE 2 "src-ag/Patterns.ag" #-}

-- Patterns.ag imports
import UU.Scanner.Position(Pos)
import CommonTypes (ConstructorIdent,Identifier)
{-# LINE 49 "dist/build/uuagc/uuagc-tmp/Order.hs" #-}

{-# LINE 2 "src-ag/Expression.ag" #-}

import UU.Scanner.Position(Pos)
import HsToken
{-# LINE 55 "dist/build/uuagc/uuagc-tmp/Order.hs" #-}

{-# LINE 2 "src-ag/AbstractSyntax.ag" #-}

-- AbstractSyntax.ag imports
import Data.Set(Set)
import Data.Map(Map)
import Patterns    (Pattern(..),Patterns)
import Expression  (Expression(..))
import Macro --marcos
import CommonTypes
import ErrorMessages
{-# LINE 67 "dist/build/uuagc/uuagc-tmp/Order.hs" #-}
{-# LINE 46 "src-ag/Order.ag" #-}

-- Terminates with an error if the key is not in the map
findWithErr1 :: (Ord k, Show k) => String -> k -> Map k a -> a
findWithErr1 s k
  = Map.findWithDefault (error ("findWithErr1 " ++ s ++ ": key " ++ show k ++ " not in map.")) k

findWithErr2 :: (Ord k, Show k, Show a) => k -> Map k a -> a
findWithErr2 k m
  = Map.findWithDefault (error ("findWithErr2: key " ++ show k ++ " not in map: " ++ show m)) k m
{-# LINE 78 "dist/build/uuagc/uuagc-tmp/Order.hs" #-}

{-# LINE 71 "src-ag/Order.ag" #-}

startsWith :: String -> String -> Bool
startsWith k h = k == take (length k) h
{-# LINE 84 "dist/build/uuagc/uuagc-tmp/Order.hs" #-}

{-# LINE 138 "src-ag/Order.ag" #-}

getNtName :: Type -> NontermIdent
getNtName (NT nt _ _) = nt
getNtName _           = nullIdent
{-# LINE 91 "dist/build/uuagc/uuagc-tmp/Order.hs" #-}

{-# LINE 166 "src-ag/Order.ag" #-}

data AltAttr = AltAttr Identifier Identifier Bool
               deriving (Eq, Ord, Show)
{-# LINE 97 "dist/build/uuagc/uuagc-tmp/Order.hs" #-}

{-# LINE 236 "src-ag/Order.ag" #-}

substSelf nt tp
  = case tp of
      NT n tps defor | n == _SELF -> NT nt tps defor
      _                           -> tp

haskellTupel :: [Type] -> Maybe Type
haskellTupel ts =  Just ( Haskell ( '(' : (concat (intersperse "," (map show ts))) ++ ")" ))
{-# LINE 108 "dist/build/uuagc/uuagc-tmp/Order.hs" #-}

{-# LINE 687 "src-ag/Order.ag" #-}

swap (a,b) = (b,a)

showPath :: Table CRule -> [Vertex] -> [String]
showPath ruleTable path
  =  let  look a | inRange (bounds ruleTable) a = [showOrigin (ruleTable ! a)]
                 | otherwise = ["Vertex " ++ show a]
          showOrigin cr  | getHasCode cr && getName (getAttr cr) /= "self" = prettyCRule cr ++ " (" ++ show (getPos (getAttr cr)) ++ ")"
                         | otherwise = prettyCRule cr
     in concatMap look path


showPathLocal :: Table CRule -> [Vertex] -> [String]
showPathLocal _ [] = []
showPathLocal ruleTable xs = showP (xs++[-1])
 where showP []         = []
       showP (v1:v2:vs) = let line  = step v1 v2
                              lines = showP vs
                          in  line:lines
       step v1 v2  = " - " ++ a1
        where r1 = ruleTable ! v1
              a1 = show (getAttr  r1)


limitTo :: Int -> [String] -> [String]
limitTo _ [] = []
limitTo 0 _ = ["....etcetera, etcetera...."]
limitTo n (x:xs) = x : limitTo (n-1) xs

showPathNice :: Table CRule -> [Vertex] -> [String]
showPathNice _ [] = []
showPathNice ruleTable xs = limitTo 100 (showP ((-1):xs++[-1]))
 where [maxf, maxa, maxn, maxc] = maxWidths ruleTable (take 100 xs)
       showP []         = []
       showP (v1:v2:vs) = let line  = step v1 v2
                              lines = showP vs
                          in  if null line  then lines  else line:lines
       step v1 v2  |  last &&      first    = induced
                   |  last &&     isSyn r1  = "pass up        "  ++ alignR maxf ""    ++ " " ++ alignL maxa a1 ++ " in " ++ alignR maxn n1 ++ "|" ++ c1 ++ induced
                   |  first&& not(isSyn r2) = "get from above "  ++ alignR maxf ""    ++ " " ++ alignL maxa a2 ++ " in " ++ alignR maxn n2 ++ "|" ++ c2
                   |  last                  = "pass down      "  ++ alignR maxf f1    ++ "." ++ a1                                                      ++ induced
                   |              isSyn r2  = "get from below "  ++ alignR maxf f2    ++ "." ++ alignL maxa a2 ++ " in " ++ alignR maxn n2 ++ "|" ++ c2
                   |  isLocal r1  = if head a1 == '_'
                                         then ""
                                         else "calculate      "  ++ alignR maxf "loc" ++ "." ++ a1
                   |  otherwise             = "pass down      "  ++ alignR maxf f1    ++ "." ++ alignL maxa a1 ++ " to " ++ alignR maxn n2 ++ "|" ++ c2
          where
              first = v1<0
              last  = v2<0
              r1 = ruleTable ! v1
              r2 = ruleTable ! v2
              a1 = show (getAttr  r1)
              a2 = show (getAttr  r2)
              f1 = show (getField r1)
              f2 = show (getField r2)
              n1 = show (getLhsNt r1)
              n2 = show (getLhsNt r2)
              c1 = show (getCon   r1)
              c2 = show (getCon   r2)
              induced | v2== -2   =  " INDUCED dependency to "
                      | otherwise = ""


maxWidths ruleTable vs
  = map maximum (transpose (map getWidth vs))
  where getWidth v | v<0       = [0,0,0,0]
                   | otherwise = map (length . show . ($ (ruleTable!v))) [getField, getAttr, getLhsNt, getCon]

alignL n xs | k<n       = xs ++ replicate (n-k) ' '
            | otherwise = xs
              where k = length xs

alignR n xs | k<n       = replicate (n-k) ' ' ++ xs
            | otherwise = xs
              where k = length xs

localCycleErr :: Table CRule -> Bool -> Route -> Error
localCycleErr ruleTable o_visit (s:path)
  =  let cr = ruleTable ! s
         attr = getAttr cr
         nt = getLhsNt cr
         con = getCon cr
     in LocalCirc nt con attr o_visit (showPathLocal ruleTable path)

instCycleErr :: Table CRule -> Bool -> Route -> Error
instCycleErr ruleTable o_visit (s:path)
  =  let cr = ruleTable ! s
         attr = getAttr cr
         nt = getLhsNt cr
         con = getCon cr
     in InstCirc nt con attr o_visit (showPathLocal ruleTable path)

directCycleErrs :: Table NTAttr -> Table CRule -> Bool -> [EdgeRoutes] -> [Error]
directCycleErrs attrTable ruleTable o_visit xs
  = let getNont v = case attrTable ! v of
                      NTASyn nt _ _ -> nt
                      NTAInh nt _ _ -> nt
        getAttr v = case attrTable ! v of
                      NTASyn _ a _  -> a
                      NTAInh _ a _  -> a
        sameNont ((v1,_),_,_) ((v2,_),_,_) =  getNont v1 == getNont v2
        procCycle ((v1,v2),p1,p2) = ((getAttr v1, getAttr v2), showPathNice ruleTable p1, showPathNice ruleTable p2)
        wrapGroup gr@(((v1,_),_,_):_) = DirectCirc (getNont v1) o_visit (map procCycle gr)
    in  map wrapGroup (groupBy sameNont xs)

inducedCycleErrs :: Table NTAttr -> Table CRule -> CInterfaceMap -> [EdgeRoutes] -> [Error]
inducedCycleErrs attrTable ruleTable cim xs
  = let getNont v = case attrTable ! v of
                      NTASyn nt _ _ -> nt
                      NTAInh nt _ _ -> nt
        getAttr v = case attrTable ! v of
                      NTASyn _ a _  -> a
                      NTAInh _ a _  -> a
        sameNont ((v1,_),_,_) ((v2,_),_,_) =  getNont v1 == getNont v2
        procCycle ((v1,v2),p1,p2) = ((getAttr v1, getAttr v2), showPathNice ruleTable p1, showPathNice ruleTable p2)
        wrapGroup gr@(((v1,_),_,_):_) = InducedCirc (getNont v1) (findWithErr1 "inducedCycleErr.cinter" (getNont v1) cim) (map procCycle gr)
    in  map wrapGroup (groupBy sameNont xs)
{-# LINE 228 "dist/build/uuagc/uuagc-tmp/Order.hs" #-}
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
         o_unbox              : Bool
         syn                  : Attributes
         synMap               : Map Identifier Attributes
      synthesized attributes:
         attributes           : [(Identifier,Attributes,Attributes)]
         collectChildrenInhs  : Map Identifier Attributes 
         collectChildrenSyns  : Map Identifier Attributes 
         errors               : Seq Error
         field                : (Identifier,Type,ChildKind)
         gathAltAttrs         : [AltAttr]
         gathRules            : Seq CRule
         inhs                 : Seq (Identifier,Attributes)
         nts                  : Seq (Identifier,NontermIdent)
         singlevisits         : [CRule]
         terminals            : [Identifier]
   alternatives:
      alternative Child:
         child name           : {Identifier}
         child tp             : {Type}
         child kind           : {ChildKind}
         visit 0:
            local maptolocal  : _
            local gathRules   : _
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
newtype T_Child  = T_Child (([(Identifier,Type,ChildKind)]) ->
                            ([Identifier]) ->
                            ([(Identifier,Identifier)]) ->
                            Identifier ->
                            Attributes ->
                            (Map Identifier Attributes) ->
                            (Map Identifier (Identifier,[Identifier])) ->
                            Identifier ->
                            Bool ->
                            Attributes ->
                            (Map Identifier Attributes) ->
                            ( ([(Identifier,Attributes,Attributes)]),(Map Identifier Attributes ),(Map Identifier Attributes ),(Seq Error),((Identifier,Type,ChildKind)),([AltAttr]),(Seq CRule),(Seq (Identifier,Attributes)),(Seq (Identifier,NontermIdent)),([CRule]),([Identifier])))
data Inh_Child  = Inh_Child {allfields_Inh_Child :: !(([(Identifier,Type,ChildKind)])),allnts_Inh_Child :: !(([Identifier])),attrs_Inh_Child :: !(([(Identifier,Identifier)])),con_Inh_Child :: !(Identifier),inh_Inh_Child :: !(Attributes),inhMap_Inh_Child :: !((Map Identifier Attributes)),mergeMap_Inh_Child :: !((Map Identifier (Identifier,[Identifier]))),nt_Inh_Child :: !(Identifier),o_unbox_Inh_Child :: !(Bool),syn_Inh_Child :: !(Attributes),synMap_Inh_Child :: !((Map Identifier Attributes))}
data Syn_Child  = Syn_Child {attributes_Syn_Child :: !(([(Identifier,Attributes,Attributes)])),collectChildrenInhs_Syn_Child :: !((Map Identifier Attributes )),collectChildrenSyns_Syn_Child :: !((Map Identifier Attributes )),errors_Syn_Child :: !((Seq Error)),field_Syn_Child :: !(((Identifier,Type,ChildKind))),gathAltAttrs_Syn_Child :: !(([AltAttr])),gathRules_Syn_Child :: !((Seq CRule)),inhs_Syn_Child :: !((Seq (Identifier,Attributes))),nts_Syn_Child :: !((Seq (Identifier,NontermIdent))),singlevisits_Syn_Child :: !(([CRule])),terminals_Syn_Child :: !(([Identifier]))}
wrap_Child :: T_Child  ->
              Inh_Child  ->
              Syn_Child 
wrap_Child (T_Child sem ) (Inh_Child _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsIinh _lhsIinhMap _lhsImergeMap _lhsInt _lhsIo_unbox _lhsIsyn _lhsIsynMap )  =
    (let ( _lhsOattributes,_lhsOcollectChildrenInhs,_lhsOcollectChildrenSyns,_lhsOerrors,_lhsOfield,_lhsOgathAltAttrs,_lhsOgathRules,_lhsOinhs,_lhsOnts,_lhsOsinglevisits,_lhsOterminals) = sem _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsIinh _lhsIinhMap _lhsImergeMap _lhsInt _lhsIo_unbox _lhsIsyn _lhsIsynMap 
     in  (Syn_Child _lhsOattributes _lhsOcollectChildrenInhs _lhsOcollectChildrenSyns _lhsOerrors _lhsOfield _lhsOgathAltAttrs _lhsOgathRules _lhsOinhs _lhsOnts _lhsOsinglevisits _lhsOterminals ))
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
                _lhsIo_unbox
                _lhsIsyn
                _lhsIsynMap ->
                  (let _lhsOgathAltAttrs :: ([AltAttr])
                       _lhsOnts :: (Seq (Identifier,NontermIdent))
                       _lhsOinhs :: (Seq (Identifier,Attributes))
                       _lhsOcollectChildrenSyns :: (Map Identifier Attributes )
                       _lhsOcollectChildrenInhs :: (Map Identifier Attributes )
                       _lhsOsinglevisits :: ([CRule])
                       _lhsOterminals :: ([Identifier])
                       _lhsOattributes :: ([(Identifier,Attributes,Attributes)])
                       _lhsOfield :: ((Identifier,Type,ChildKind))
                       _lhsOerrors :: (Seq Error)
                       _lhsOgathRules :: (Seq CRule)
                       -- "src-ag/Order.ag"(line 177, column 13)
                       _maptolocal =
                           ({-# LINE 177 "src-ag/Order.ag" #-}
                            case tp_ of
                              NT nt _ _ -> Map.null _syn
                              _         -> True
                            {-# LINE 327 "src-ag/Order.hs" #-}
                            )
                       -- "src-ag/Order.ag"(line 180, column 13)
                       _lhsOgathAltAttrs =
                           ({-# LINE 180 "src-ag/Order.ag" #-}
                            if  _maptolocal
                                then [ AltAttr _LOC name_ True ]
                                else [ AltAttr name_ syn True | syn <- Map.keys _syn     ]
                            {-# LINE 335 "src-ag/Order.hs" #-}
                            )
                       -- "src-ag/Order.ag"(line 195, column 13)
                       _lhsOnts =
                           ({-# LINE 195 "src-ag/Order.ag" #-}
                            Seq.singleton (name_,getNtName tp_)
                            {-# LINE 341 "src-ag/Order.hs" #-}
                            )
                       -- "src-ag/Order.ag"(line 196, column 13)
                       _lhsOinhs =
                           ({-# LINE 196 "src-ag/Order.ag" #-}
                            Seq.singleton (name_,_inh    )
                            {-# LINE 347 "src-ag/Order.hs" #-}
                            )
                       -- "src-ag/Order.ag"(line 212, column 13)
                       _gathRules =
                           ({-# LINE 212 "src-ag/Order.ag" #-}
                            if  _maptolocal
                                then Seq.singleton (cRuleTerminal name_ _lhsInt _lhsIcon tp_)
                                else Seq.fromList [ cRuleRhsSyn syn _lhsInt _lhsIcon tp name_ (getNtName tp_) | (syn,tp) <- Map.assocs _syn    ]
                            {-# LINE 355 "src-ag/Order.hs" #-}
                            )
                       -- "src-ag/Order.ag"(line 344, column 12)
                       _lhsOcollectChildrenSyns =
                           ({-# LINE 344 "src-ag/Order.ag" #-}
                            Map.singleton name_ _syn
                            {-# LINE 361 "src-ag/Order.hs" #-}
                            )
                       -- "src-ag/Order.ag"(line 345, column 12)
                       _lhsOcollectChildrenInhs =
                           ({-# LINE 345 "src-ag/Order.ag" #-}
                            Map.singleton name_ _inh
                            {-# LINE 367 "src-ag/Order.hs" #-}
                            )
                       -- "src-ag/Order.ag"(line 613, column 11)
                       _lhsOsinglevisits =
                           ({-# LINE 613 "src-ag/Order.ag" #-}
                            if  _maptolocal
                                then []
                                else [CChildVisit name_ (getNtName tp_) 0 _inh     _syn     True]
                            {-# LINE 375 "src-ag/Order.hs" #-}
                            )
                       -- "src-ag/Order.ag"(line 638, column 11)
                       _lhsOterminals =
                           ({-# LINE 638 "src-ag/Order.ag" #-}
                            if _maptolocal
                            then [name_]
                            else []
                            {-# LINE 383 "src-ag/Order.hs" #-}
                            )
                       -- "src-ag/Order.ag"(line 667, column 11)
                       _lhsOattributes =
                           ({-# LINE 667 "src-ag/Order.ag" #-}
                            [(name_, _inh    , _syn    )]
                            {-# LINE 389 "src-ag/Order.hs" #-}
                            )
                       -- "src-ag/Order.ag"(line 671, column 11)
                       _lhsOfield =
                           ({-# LINE 671 "src-ag/Order.ag" #-}
                            (name_, tp_, kind_)
                            {-# LINE 395 "src-ag/Order.hs" #-}
                            )
                       -- "src-ag/DistChildAttr.ag"(line 19, column 11)
                       _chnt =
                           ({-# LINE 19 "src-ag/DistChildAttr.ag" #-}
                            case tp_ of
                              NT nt _ _ -> nt
                              Self      -> error ("The type of child " ++ show name_ ++ " should not be a Self type.")
                              Haskell t -> identifier t
                            {-# LINE 404 "src-ag/Order.hs" #-}
                            )
                       -- "src-ag/DistChildAttr.ag"(line 23, column 11)
                       _inh =
                           ({-# LINE 23 "src-ag/DistChildAttr.ag" #-}
                            Map.findWithDefault Map.empty _chnt     _lhsIinhMap
                            {-# LINE 410 "src-ag/Order.hs" #-}
                            )
                       -- "src-ag/DistChildAttr.ag"(line 24, column 11)
                       _syn =
                           ({-# LINE 24 "src-ag/DistChildAttr.ag" #-}
                            Map.findWithDefault Map.empty _chnt     _lhsIsynMap
                            {-# LINE 416 "src-ag/Order.hs" #-}
                            )
                       -- use rule "src-ag/Order.ag"(line 84, column 70)
                       _lhsOerrors =
                           ({-# LINE 84 "src-ag/Order.ag" #-}
                            Seq.empty
                            {-# LINE 422 "src-ag/Order.hs" #-}
                            )
                       -- use rule "src-ag/Order.ag"(line 206, column 23)
                       _lhsOgathRules =
                           ({-# LINE 206 "src-ag/Order.ag" #-}
                            _gathRules
                            {-# LINE 428 "src-ag/Order.hs" #-}
                            )
                   in  ( _lhsOattributes,_lhsOcollectChildrenInhs,_lhsOcollectChildrenSyns,_lhsOerrors,_lhsOfield,_lhsOgathAltAttrs,_lhsOgathRules,_lhsOinhs,_lhsOnts,_lhsOsinglevisits,_lhsOterminals))) )
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
         o_unbox              : Bool
         syn                  : Attributes
         synMap               : Map Identifier Attributes
      synthesized attributes:
         attributes           : [(Identifier,Attributes,Attributes)]
         collectChildrenInhs  : Map Identifier Attributes 
         collectChildrenSyns  : Map Identifier Attributes 
         errors               : Seq Error
         fields               : [(Identifier,Type,ChildKind)]
         gathAltAttrs         : [AltAttr]
         gathRules            : Seq CRule
         inhs                 : Seq (Identifier,Attributes)
         nts                  : Seq (Identifier,NontermIdent)
         singlevisits         : [CRule]
         terminals            : [Identifier]
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
newtype T_Children  = T_Children (([(Identifier,Type,ChildKind)]) ->
                                  ([Identifier]) ->
                                  ([(Identifier,Identifier)]) ->
                                  Identifier ->
                                  Attributes ->
                                  (Map Identifier Attributes) ->
                                  (Map Identifier (Identifier,[Identifier])) ->
                                  Identifier ->
                                  Bool ->
                                  Attributes ->
                                  (Map Identifier Attributes) ->
                                  ( ([(Identifier,Attributes,Attributes)]),(Map Identifier Attributes ),(Map Identifier Attributes ),(Seq Error),([(Identifier,Type,ChildKind)]),([AltAttr]),(Seq CRule),(Seq (Identifier,Attributes)),(Seq (Identifier,NontermIdent)),([CRule]),([Identifier])))
data Inh_Children  = Inh_Children {allfields_Inh_Children :: !(([(Identifier,Type,ChildKind)])),allnts_Inh_Children :: !(([Identifier])),attrs_Inh_Children :: !(([(Identifier,Identifier)])),con_Inh_Children :: !(Identifier),inh_Inh_Children :: !(Attributes),inhMap_Inh_Children :: !((Map Identifier Attributes)),mergeMap_Inh_Children :: !((Map Identifier (Identifier,[Identifier]))),nt_Inh_Children :: !(Identifier),o_unbox_Inh_Children :: !(Bool),syn_Inh_Children :: !(Attributes),synMap_Inh_Children :: !((Map Identifier Attributes))}
data Syn_Children  = Syn_Children {attributes_Syn_Children :: !(([(Identifier,Attributes,Attributes)])),collectChildrenInhs_Syn_Children :: !((Map Identifier Attributes )),collectChildrenSyns_Syn_Children :: !((Map Identifier Attributes )),errors_Syn_Children :: !((Seq Error)),fields_Syn_Children :: !(([(Identifier,Type,ChildKind)])),gathAltAttrs_Syn_Children :: !(([AltAttr])),gathRules_Syn_Children :: !((Seq CRule)),inhs_Syn_Children :: !((Seq (Identifier,Attributes))),nts_Syn_Children :: !((Seq (Identifier,NontermIdent))),singlevisits_Syn_Children :: !(([CRule])),terminals_Syn_Children :: !(([Identifier]))}
wrap_Children :: T_Children  ->
                 Inh_Children  ->
                 Syn_Children 
wrap_Children (T_Children sem ) (Inh_Children _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsIinh _lhsIinhMap _lhsImergeMap _lhsInt _lhsIo_unbox _lhsIsyn _lhsIsynMap )  =
    (let ( _lhsOattributes,_lhsOcollectChildrenInhs,_lhsOcollectChildrenSyns,_lhsOerrors,_lhsOfields,_lhsOgathAltAttrs,_lhsOgathRules,_lhsOinhs,_lhsOnts,_lhsOsinglevisits,_lhsOterminals) = sem _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsIinh _lhsIinhMap _lhsImergeMap _lhsInt _lhsIo_unbox _lhsIsyn _lhsIsynMap 
     in  (Syn_Children _lhsOattributes _lhsOcollectChildrenInhs _lhsOcollectChildrenSyns _lhsOerrors _lhsOfields _lhsOgathAltAttrs _lhsOgathRules _lhsOinhs _lhsOnts _lhsOsinglevisits _lhsOterminals ))
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
                   _lhsIo_unbox
                   _lhsIsyn
                   _lhsIsynMap ->
                     (let _lhsOfields :: ([(Identifier,Type,ChildKind)])
                          _lhsOattributes :: ([(Identifier,Attributes,Attributes)])
                          _lhsOcollectChildrenInhs :: (Map Identifier Attributes )
                          _lhsOcollectChildrenSyns :: (Map Identifier Attributes )
                          _lhsOerrors :: (Seq Error)
                          _lhsOgathAltAttrs :: ([AltAttr])
                          _lhsOgathRules :: (Seq CRule)
                          _lhsOinhs :: (Seq (Identifier,Attributes))
                          _lhsOnts :: (Seq (Identifier,NontermIdent))
                          _lhsOsinglevisits :: ([CRule])
                          _lhsOterminals :: ([Identifier])
                          _hdOallfields :: ([(Identifier,Type,ChildKind)])
                          _hdOallnts :: ([Identifier])
                          _hdOattrs :: ([(Identifier,Identifier)])
                          _hdOcon :: Identifier
                          _hdOinh :: Attributes
                          _hdOinhMap :: (Map Identifier Attributes)
                          _hdOmergeMap :: (Map Identifier (Identifier,[Identifier]))
                          _hdOnt :: Identifier
                          _hdOo_unbox :: Bool
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
                          _tlOo_unbox :: Bool
                          _tlOsyn :: Attributes
                          _tlOsynMap :: (Map Identifier Attributes)
                          _hdIattributes :: ([(Identifier,Attributes,Attributes)])
                          _hdIcollectChildrenInhs :: (Map Identifier Attributes )
                          _hdIcollectChildrenSyns :: (Map Identifier Attributes )
                          _hdIerrors :: (Seq Error)
                          _hdIfield :: ((Identifier,Type,ChildKind))
                          _hdIgathAltAttrs :: ([AltAttr])
                          _hdIgathRules :: (Seq CRule)
                          _hdIinhs :: (Seq (Identifier,Attributes))
                          _hdInts :: (Seq (Identifier,NontermIdent))
                          _hdIsinglevisits :: ([CRule])
                          _hdIterminals :: ([Identifier])
                          _tlIattributes :: ([(Identifier,Attributes,Attributes)])
                          _tlIcollectChildrenInhs :: (Map Identifier Attributes )
                          _tlIcollectChildrenSyns :: (Map Identifier Attributes )
                          _tlIerrors :: (Seq Error)
                          _tlIfields :: ([(Identifier,Type,ChildKind)])
                          _tlIgathAltAttrs :: ([AltAttr])
                          _tlIgathRules :: (Seq CRule)
                          _tlIinhs :: (Seq (Identifier,Attributes))
                          _tlInts :: (Seq (Identifier,NontermIdent))
                          _tlIsinglevisits :: ([CRule])
                          _tlIterminals :: ([Identifier])
                          -- "src-ag/Order.ag"(line 674, column 11)
                          _lhsOfields =
                              ({-# LINE 674 "src-ag/Order.ag" #-}
                               _hdIfield : _tlIfields
                               {-# LINE 564 "src-ag/Order.hs" #-}
                               )
                          -- use rule "src-ag/Order.ag"(line 665, column 32)
                          _lhsOattributes =
                              ({-# LINE 665 "src-ag/Order.ag" #-}
                               _hdIattributes ++ _tlIattributes
                               {-# LINE 570 "src-ag/Order.hs" #-}
                               )
                          -- use rule "src-ag/Order.ag"(line 342, column 68)
                          _lhsOcollectChildrenInhs =
                              ({-# LINE 342 "src-ag/Order.ag" #-}
                               _hdIcollectChildrenInhs `Map.union` _tlIcollectChildrenInhs
                               {-# LINE 576 "src-ag/Order.hs" #-}
                               )
                          -- use rule "src-ag/Order.ag"(line 342, column 68)
                          _lhsOcollectChildrenSyns =
                              ({-# LINE 342 "src-ag/Order.ag" #-}
                               _hdIcollectChildrenSyns `Map.union` _tlIcollectChildrenSyns
                               {-# LINE 582 "src-ag/Order.hs" #-}
                               )
                          -- use rule "src-ag/Order.ag"(line 84, column 70)
                          _lhsOerrors =
                              ({-# LINE 84 "src-ag/Order.ag" #-}
                               _hdIerrors Seq.>< _tlIerrors
                               {-# LINE 588 "src-ag/Order.hs" #-}
                               )
                          -- use rule "src-ag/Order.ag"(line 170, column 68)
                          _lhsOgathAltAttrs =
                              ({-# LINE 170 "src-ag/Order.ag" #-}
                               _hdIgathAltAttrs ++ _tlIgathAltAttrs
                               {-# LINE 594 "src-ag/Order.hs" #-}
                               )
                          -- use rule "src-ag/Order.ag"(line 206, column 23)
                          _lhsOgathRules =
                              ({-# LINE 206 "src-ag/Order.ag" #-}
                               _hdIgathRules Seq.>< _tlIgathRules
                               {-# LINE 600 "src-ag/Order.hs" #-}
                               )
                          -- use rule "src-ag/Order.ag"(line 193, column 20)
                          _lhsOinhs =
                              ({-# LINE 193 "src-ag/Order.ag" #-}
                               _hdIinhs Seq.>< _tlIinhs
                               {-# LINE 606 "src-ag/Order.hs" #-}
                               )
                          -- use rule "src-ag/Order.ag"(line 192, column 19)
                          _lhsOnts =
                              ({-# LINE 192 "src-ag/Order.ag" #-}
                               _hdInts Seq.>< _tlInts
                               {-# LINE 612 "src-ag/Order.hs" #-}
                               )
                          -- use rule "src-ag/Order.ag"(line 611, column 40)
                          _lhsOsinglevisits =
                              ({-# LINE 611 "src-ag/Order.ag" #-}
                               _hdIsinglevisits ++ _tlIsinglevisits
                               {-# LINE 618 "src-ag/Order.hs" #-}
                               )
                          -- use rule "src-ag/Order.ag"(line 636, column 38)
                          _lhsOterminals =
                              ({-# LINE 636 "src-ag/Order.ag" #-}
                               _hdIterminals ++ _tlIterminals
                               {-# LINE 624 "src-ag/Order.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOallfields =
                              ({-# LINE 654 "src-ag/Order.ag" #-}
                               _lhsIallfields
                               {-# LINE 630 "src-ag/Order.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOallnts =
                              ({-# LINE 647 "src-ag/Order.ag" #-}
                               _lhsIallnts
                               {-# LINE 636 "src-ag/Order.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOattrs =
                              ({-# LINE 654 "src-ag/Order.ag" #-}
                               _lhsIattrs
                               {-# LINE 642 "src-ag/Order.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOcon =
                              ({-# LINE 90 "src-ag/Order.ag" #-}
                               _lhsIcon
                               {-# LINE 648 "src-ag/Order.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOinh =
                              ({-# LINE 89 "src-ag/Order.ag" #-}
                               _lhsIinh
                               {-# LINE 654 "src-ag/Order.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOinhMap =
                              ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                               _lhsIinhMap
                               {-# LINE 660 "src-ag/Order.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOmergeMap =
                              ({-# LINE 360 "src-ag/Order.ag" #-}
                               _lhsImergeMap
                               {-# LINE 666 "src-ag/Order.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOnt =
                              ({-# LINE 89 "src-ag/Order.ag" #-}
                               _lhsInt
                               {-# LINE 672 "src-ag/Order.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOo_unbox =
                              ({-# LINE 119 "src-ag/Order.ag" #-}
                               _lhsIo_unbox
                               {-# LINE 678 "src-ag/Order.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOsyn =
                              ({-# LINE 89 "src-ag/Order.ag" #-}
                               _lhsIsyn
                               {-# LINE 684 "src-ag/Order.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOsynMap =
                              ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                               _lhsIsynMap
                               {-# LINE 690 "src-ag/Order.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOallfields =
                              ({-# LINE 654 "src-ag/Order.ag" #-}
                               _lhsIallfields
                               {-# LINE 696 "src-ag/Order.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOallnts =
                              ({-# LINE 647 "src-ag/Order.ag" #-}
                               _lhsIallnts
                               {-# LINE 702 "src-ag/Order.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOattrs =
                              ({-# LINE 654 "src-ag/Order.ag" #-}
                               _lhsIattrs
                               {-# LINE 708 "src-ag/Order.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOcon =
                              ({-# LINE 90 "src-ag/Order.ag" #-}
                               _lhsIcon
                               {-# LINE 714 "src-ag/Order.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOinh =
                              ({-# LINE 89 "src-ag/Order.ag" #-}
                               _lhsIinh
                               {-# LINE 720 "src-ag/Order.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOinhMap =
                              ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                               _lhsIinhMap
                               {-# LINE 726 "src-ag/Order.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOmergeMap =
                              ({-# LINE 360 "src-ag/Order.ag" #-}
                               _lhsImergeMap
                               {-# LINE 732 "src-ag/Order.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOnt =
                              ({-# LINE 89 "src-ag/Order.ag" #-}
                               _lhsInt
                               {-# LINE 738 "src-ag/Order.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOo_unbox =
                              ({-# LINE 119 "src-ag/Order.ag" #-}
                               _lhsIo_unbox
                               {-# LINE 744 "src-ag/Order.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOsyn =
                              ({-# LINE 89 "src-ag/Order.ag" #-}
                               _lhsIsyn
                               {-# LINE 750 "src-ag/Order.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOsynMap =
                              ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                               _lhsIsynMap
                               {-# LINE 756 "src-ag/Order.hs" #-}
                               )
                          ( _hdIattributes,_hdIcollectChildrenInhs,_hdIcollectChildrenSyns,_hdIerrors,_hdIfield,_hdIgathAltAttrs,_hdIgathRules,_hdIinhs,_hdInts,_hdIsinglevisits,_hdIterminals) =
                              hd_ _hdOallfields _hdOallnts _hdOattrs _hdOcon _hdOinh _hdOinhMap _hdOmergeMap _hdOnt _hdOo_unbox _hdOsyn _hdOsynMap 
                          ( _tlIattributes,_tlIcollectChildrenInhs,_tlIcollectChildrenSyns,_tlIerrors,_tlIfields,_tlIgathAltAttrs,_tlIgathRules,_tlIinhs,_tlInts,_tlIsinglevisits,_tlIterminals) =
                              tl_ _tlOallfields _tlOallnts _tlOattrs _tlOcon _tlOinh _tlOinhMap _tlOmergeMap _tlOnt _tlOo_unbox _tlOsyn _tlOsynMap 
                      in  ( _lhsOattributes,_lhsOcollectChildrenInhs,_lhsOcollectChildrenSyns,_lhsOerrors,_lhsOfields,_lhsOgathAltAttrs,_lhsOgathRules,_lhsOinhs,_lhsOnts,_lhsOsinglevisits,_lhsOterminals))) )
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
                   _lhsIo_unbox
                   _lhsIsyn
                   _lhsIsynMap ->
                     (let _lhsOfields :: ([(Identifier,Type,ChildKind)])
                          _lhsOattributes :: ([(Identifier,Attributes,Attributes)])
                          _lhsOcollectChildrenInhs :: (Map Identifier Attributes )
                          _lhsOcollectChildrenSyns :: (Map Identifier Attributes )
                          _lhsOerrors :: (Seq Error)
                          _lhsOgathAltAttrs :: ([AltAttr])
                          _lhsOgathRules :: (Seq CRule)
                          _lhsOinhs :: (Seq (Identifier,Attributes))
                          _lhsOnts :: (Seq (Identifier,NontermIdent))
                          _lhsOsinglevisits :: ([CRule])
                          _lhsOterminals :: ([Identifier])
                          -- "src-ag/Order.ag"(line 675, column 11)
                          _lhsOfields =
                              ({-# LINE 675 "src-ag/Order.ag" #-}
                               []
                               {-# LINE 791 "src-ag/Order.hs" #-}
                               )
                          -- use rule "src-ag/Order.ag"(line 665, column 32)
                          _lhsOattributes =
                              ({-# LINE 665 "src-ag/Order.ag" #-}
                               []
                               {-# LINE 797 "src-ag/Order.hs" #-}
                               )
                          -- use rule "src-ag/Order.ag"(line 342, column 68)
                          _lhsOcollectChildrenInhs =
                              ({-# LINE 342 "src-ag/Order.ag" #-}
                               Map.empty
                               {-# LINE 803 "src-ag/Order.hs" #-}
                               )
                          -- use rule "src-ag/Order.ag"(line 342, column 68)
                          _lhsOcollectChildrenSyns =
                              ({-# LINE 342 "src-ag/Order.ag" #-}
                               Map.empty
                               {-# LINE 809 "src-ag/Order.hs" #-}
                               )
                          -- use rule "src-ag/Order.ag"(line 84, column 70)
                          _lhsOerrors =
                              ({-# LINE 84 "src-ag/Order.ag" #-}
                               Seq.empty
                               {-# LINE 815 "src-ag/Order.hs" #-}
                               )
                          -- use rule "src-ag/Order.ag"(line 170, column 68)
                          _lhsOgathAltAttrs =
                              ({-# LINE 170 "src-ag/Order.ag" #-}
                               []
                               {-# LINE 821 "src-ag/Order.hs" #-}
                               )
                          -- use rule "src-ag/Order.ag"(line 206, column 23)
                          _lhsOgathRules =
                              ({-# LINE 206 "src-ag/Order.ag" #-}
                               Seq.empty
                               {-# LINE 827 "src-ag/Order.hs" #-}
                               )
                          -- use rule "src-ag/Order.ag"(line 193, column 20)
                          _lhsOinhs =
                              ({-# LINE 193 "src-ag/Order.ag" #-}
                               Seq.empty
                               {-# LINE 833 "src-ag/Order.hs" #-}
                               )
                          -- use rule "src-ag/Order.ag"(line 192, column 19)
                          _lhsOnts =
                              ({-# LINE 192 "src-ag/Order.ag" #-}
                               Seq.empty
                               {-# LINE 839 "src-ag/Order.hs" #-}
                               )
                          -- use rule "src-ag/Order.ag"(line 611, column 40)
                          _lhsOsinglevisits =
                              ({-# LINE 611 "src-ag/Order.ag" #-}
                               []
                               {-# LINE 845 "src-ag/Order.hs" #-}
                               )
                          -- use rule "src-ag/Order.ag"(line 636, column 38)
                          _lhsOterminals =
                              ({-# LINE 636 "src-ag/Order.ag" #-}
                               []
                               {-# LINE 851 "src-ag/Order.hs" #-}
                               )
                      in  ( _lhsOattributes,_lhsOcollectChildrenInhs,_lhsOcollectChildrenSyns,_lhsOerrors,_lhsOfields,_lhsOgathAltAttrs,_lhsOgathRules,_lhsOinhs,_lhsOnts,_lhsOsinglevisits,_lhsOterminals))) )
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
      synthesized attributes:
         allRhsVars           : Set (Identifier,Identifier)
         copy                 : SELF 
         errors               : Seq Error
         textLines            : [String]
         usedAttrs            : [(Identifier,Identifier)]
         usedFields           : [Identifier]
         usedLocals           : [Identifier]
   alternatives:
      alternative Expression:
         child pos            : {Pos}
         child tks            : {[HsToken]}
         visit 0:
            local _tup1       : _
            local textLines   : _
            local usedAttrs   : _
            local usedLocals  : _
            local usedFields  : _
            local copy        : _
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
                                      ( (Set (Identifier,Identifier)),Expression ,(Seq Error),([String]),([(Identifier,Identifier)]),([Identifier]),([Identifier])))
data Inh_Expression  = Inh_Expression {allfields_Inh_Expression :: !(([(Identifier,Type,ChildKind)])),allnts_Inh_Expression :: !(([Identifier])),attrs_Inh_Expression :: !(([(Identifier,Identifier)])),con_Inh_Expression :: !(Identifier),mergeMap_Inh_Expression :: !((Map Identifier (Identifier,[Identifier]))),nt_Inh_Expression :: !(Identifier)}
data Syn_Expression  = Syn_Expression {allRhsVars_Syn_Expression :: !((Set (Identifier,Identifier))),copy_Syn_Expression :: !(Expression ),errors_Syn_Expression :: !((Seq Error)),textLines_Syn_Expression :: !(([String])),usedAttrs_Syn_Expression :: !(([(Identifier,Identifier)])),usedFields_Syn_Expression :: !(([Identifier])),usedLocals_Syn_Expression :: !(([Identifier]))}
wrap_Expression :: T_Expression  ->
                   Inh_Expression  ->
                   Syn_Expression 
wrap_Expression (T_Expression sem ) (Inh_Expression _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsImergeMap _lhsInt )  =
    (let ( _lhsOallRhsVars,_lhsOcopy,_lhsOerrors,_lhsOtextLines,_lhsOusedAttrs,_lhsOusedFields,_lhsOusedLocals) = sem _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsImergeMap _lhsInt 
     in  (Syn_Expression _lhsOallRhsVars _lhsOcopy _lhsOerrors _lhsOtextLines _lhsOusedAttrs _lhsOusedFields _lhsOusedLocals ))
sem_Expression_Expression :: Pos ->
                             ([HsToken]) ->
                             T_Expression 
sem_Expression_Expression pos_ tks_  =
    (T_Expression (\ _lhsIallfields
                     _lhsIallnts
                     _lhsIattrs
                     _lhsIcon
                     _lhsImergeMap
                     _lhsInt ->
                       (let _lhsOerrors :: (Seq Error)
                            _lhsOallRhsVars :: (Set (Identifier,Identifier))
                            _lhsOcopy :: Expression 
                            _lhsOtextLines :: ([String])
                            _lhsOusedAttrs :: ([(Identifier,Identifier)])
                            _lhsOusedFields :: ([Identifier])
                            _lhsOusedLocals :: ([Identifier])
                            -- "src-ag/Order.ag"(line 464, column 21)
                            __tup1 =
                                ({-# LINE 464 "src-ag/Order.ag" #-}
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
                                 in case synthesized of
                                      Syn_HsTokensRoot
                                       { textLines_Syn_HsTokensRoot  = textLines
                                       , usedAttrs_Syn_HsTokensRoot  = usedAttrs
                                       , usedLocals_Syn_HsTokensRoot = usedLocals
                                       , usedFields_Syn_HsTokensRoot = usedFields
                                       }  -> let extraAttrs = [ (src,attr)
                                                              | (fld,attr) <- usedAttrs, let mbMerged = Map.lookup fld _lhsImergeMap, isJust mbMerged
                                                              , let (Just (_, srcs)) = mbMerged, src <- srcs ]
                                                 usedAttrs' = usedAttrs ++ extraAttrs
                                             in (textLines,usedAttrs',usedLocals,usedFields)
                                 {-# LINE 946 "src-ag/Order.hs" #-}
                                 )
                            -- "src-ag/Order.ag"(line 464, column 21)
                            (_textLines,_,_,_) =
                                ({-# LINE 464 "src-ag/Order.ag" #-}
                                 __tup1
                                 {-# LINE 952 "src-ag/Order.hs" #-}
                                 )
                            -- "src-ag/Order.ag"(line 464, column 21)
                            (_,_usedAttrs,_,_) =
                                ({-# LINE 464 "src-ag/Order.ag" #-}
                                 __tup1
                                 {-# LINE 958 "src-ag/Order.hs" #-}
                                 )
                            -- "src-ag/Order.ag"(line 464, column 21)
                            (_,_,_usedLocals,_) =
                                ({-# LINE 464 "src-ag/Order.ag" #-}
                                 __tup1
                                 {-# LINE 964 "src-ag/Order.hs" #-}
                                 )
                            -- "src-ag/Order.ag"(line 464, column 21)
                            (_,_,_,_usedFields) =
                                ({-# LINE 464 "src-ag/Order.ag" #-}
                                 __tup1
                                 {-# LINE 970 "src-ag/Order.hs" #-}
                                 )
                            -- "src-ag/Order.ag"(line 487, column 17)
                            _lhsOerrors =
                                ({-# LINE 487 "src-ag/Order.ag" #-}
                                 Seq.empty
                                 {-# LINE 976 "src-ag/Order.hs" #-}
                                 )
                            -- "src-ag/Order.ag"(line 488, column 17)
                            _lhsOallRhsVars =
                                ({-# LINE 488 "src-ag/Order.ag" #-}
                                 Set.fromList _usedAttrs
                                 `Set.union`
                                 Set.fromList [ (_LOC, l) | l <- _usedLocals    ]
                                 `Set.union`
                                 Set.fromList [ (_FIELD, fld) | fld <- _usedFields    ]
                                 {-# LINE 986 "src-ag/Order.hs" #-}
                                 )
                            -- self rule
                            _copy =
                                ({-# LINE 455 "src-ag/Order.ag" #-}
                                 Expression pos_ tks_
                                 {-# LINE 992 "src-ag/Order.hs" #-}
                                 )
                            -- self rule
                            _lhsOcopy =
                                ({-# LINE 455 "src-ag/Order.ag" #-}
                                 _copy
                                 {-# LINE 998 "src-ag/Order.hs" #-}
                                 )
                            -- copy rule (from local)
                            _lhsOtextLines =
                                ({-# LINE 454 "src-ag/Order.ag" #-}
                                 _textLines
                                 {-# LINE 1004 "src-ag/Order.hs" #-}
                                 )
                            -- copy rule (from local)
                            _lhsOusedAttrs =
                                ({-# LINE 452 "src-ag/Order.ag" #-}
                                 _usedAttrs
                                 {-# LINE 1010 "src-ag/Order.hs" #-}
                                 )
                            -- copy rule (from local)
                            _lhsOusedFields =
                                ({-# LINE 453 "src-ag/Order.ag" #-}
                                 _usedFields
                                 {-# LINE 1016 "src-ag/Order.hs" #-}
                                 )
                            -- copy rule (from local)
                            _lhsOusedLocals =
                                ({-# LINE 451 "src-ag/Order.ag" #-}
                                 _usedLocals
                                 {-# LINE 1022 "src-ag/Order.hs" #-}
                                 )
                        in  ( _lhsOallRhsVars,_lhsOcopy,_lhsOerrors,_lhsOtextLines,_lhsOusedAttrs,_lhsOusedFields,_lhsOusedLocals))) )
-- Grammar -----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         options              : Options
      synthesized attributes:
         errors               : Seq Error
         nAutoRules           : Int
         nExplicitRules       : Int
         output               : CGrammar
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
            local o_dovisit   : _
            local ruleTable   : _
            local attrTable   : _
            local attrVertex  : _
            local tdpToTds    : _
            local tdsToTdp    : _
            local directDep   : _
            local instDep     : _
            local aroundDep   : _
            local mergeDep    : _
            local info        : _
            local _tup2       : _
            local cInterfaceMap : _
            local cVisitsMap  : _
            local cyclesErrors : _
            local aroundMap   : _
            local mergeMap    : _
-}
-- cata
sem_Grammar :: Grammar  ->
               T_Grammar 
sem_Grammar (Grammar _typeSyns _useMap _derivings _wrappers _nonts _pragmas _manualAttrOrderMap _paramMap _contextMap _quantMap _uniqueMap _augmentsMap _aroundsMap _mergeMap )  =
    (sem_Grammar_Grammar _typeSyns _useMap _derivings _wrappers (sem_Nonterminals _nonts ) _pragmas _manualAttrOrderMap _paramMap _contextMap _quantMap _uniqueMap _augmentsMap _aroundsMap _mergeMap )
-- semantic domain
newtype T_Grammar  = T_Grammar (Options ->
                                ( (Seq Error),Int,Int,CGrammar))
data Inh_Grammar  = Inh_Grammar {options_Inh_Grammar :: !(Options)}
data Syn_Grammar  = Syn_Grammar {errors_Syn_Grammar :: !((Seq Error)),nAutoRules_Syn_Grammar :: !(Int),nExplicitRules_Syn_Grammar :: !(Int),output_Syn_Grammar :: !(CGrammar)}
wrap_Grammar :: T_Grammar  ->
                Inh_Grammar  ->
                Syn_Grammar 
wrap_Grammar (T_Grammar sem ) (Inh_Grammar _lhsIoptions )  =
    (let ( _lhsOerrors,_lhsOnAutoRules,_lhsOnExplicitRules,_lhsOoutput) = sem _lhsIoptions 
     in  (Syn_Grammar _lhsOerrors _lhsOnAutoRules _lhsOnExplicitRules _lhsOoutput ))
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
                    (let _nontsOo_cata :: Bool
                         _nontsOo_data :: Bool
                         _nontsOo_sig :: Bool
                         _nontsOo_sem :: Bool
                         _nontsOo_rename :: Bool
                         _nontsOo_newtypes :: Bool
                         _nontsOo_wantvisit :: Bool
                         _nontsOo_unbox :: Bool
                         _nontsOo_case :: Bool
                         _nontsOprefix :: String
                         _nontsOvcount :: Int
                         _nontsOmanualAttrDepMap :: AttrOrderMap
                         _nontsOaroundMap :: (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression])))
                         _nontsOacount :: Int
                         _lhsOerrors :: (Seq Error)
                         _lhsOoutput :: CGrammar
                         _nontsOallnts :: ([Identifier])
                         _nontsOinhMap :: (Map Identifier Attributes)
                         _nontsOsynMap :: (Map Identifier Attributes)
                         _lhsOnAutoRules :: Int
                         _lhsOnExplicitRules :: Int
                         _nontsOcInterfaceMap :: CInterfaceMap
                         _nontsOcVisitsMap :: CVisitsMap
                         _nontsOmergeMap :: (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier]))))
                         _nontsOo_dovisit :: Bool
                         _nontsIacount :: Int
                         _nontsIadditionalDep :: (Seq Edge)
                         _nontsIaranges :: (Seq (Int,Int,Int))
                         _nontsIaroundDep :: (Seq Edge)
                         _nontsIcNonterminals :: CNonterminals
                         _nontsIdirectDep :: (Seq Edge)
                         _nontsIerrors :: (Seq Error)
                         _nontsIinhMap' :: (Map Identifier Attributes)
                         _nontsIinstDep :: (Seq Edge)
                         _nontsImergeDep :: (Seq Edge)
                         _nontsInAutoRules :: Int
                         _nontsInExplicitRules :: Int
                         _nontsInonts :: ([(NontermIdent,[ConstructorIdent])])
                         _nontsIntattrs :: (Seq (Vertex,NTAttr))
                         _nontsIrules :: (Seq (Vertex,CRule))
                         _nontsIsynMap' :: (Map Identifier Attributes)
                         _nontsIvcount :: Int
                         -- "src-ag/Order.ag"(line 123, column 17)
                         _o_dovisit =
                             ({-# LINE 123 "src-ag/Order.ag" #-}
                              visit     _lhsIoptions && null _cyclesErrors
                              {-# LINE 1149 "src-ag/Order.hs" #-}
                              )
                         -- "src-ag/Order.ag"(line 124, column 17)
                         _nontsOo_cata =
                             ({-# LINE 124 "src-ag/Order.ag" #-}
                              folds     _lhsIoptions
                              {-# LINE 1155 "src-ag/Order.hs" #-}
                              )
                         -- "src-ag/Order.ag"(line 124, column 17)
                         _nontsOo_data =
                             ({-# LINE 125 "src-ag/Order.ag" #-}
                              dataTypes _lhsIoptions
                              {-# LINE 1161 "src-ag/Order.hs" #-}
                              )
                         -- "src-ag/Order.ag"(line 124, column 17)
                         _nontsOo_sig =
                             ({-# LINE 126 "src-ag/Order.ag" #-}
                              typeSigs  _lhsIoptions
                              {-# LINE 1167 "src-ag/Order.hs" #-}
                              )
                         -- "src-ag/Order.ag"(line 124, column 17)
                         _nontsOo_sem =
                             ({-# LINE 127 "src-ag/Order.ag" #-}
                              semfuns   _lhsIoptions
                              {-# LINE 1173 "src-ag/Order.hs" #-}
                              )
                         -- "src-ag/Order.ag"(line 124, column 17)
                         _nontsOo_rename =
                             ({-# LINE 128 "src-ag/Order.ag" #-}
                              rename    _lhsIoptions
                              {-# LINE 1179 "src-ag/Order.hs" #-}
                              )
                         -- "src-ag/Order.ag"(line 124, column 17)
                         _nontsOo_newtypes =
                             ({-# LINE 129 "src-ag/Order.ag" #-}
                              newtypes  _lhsIoptions
                              {-# LINE 1185 "src-ag/Order.hs" #-}
                              )
                         -- "src-ag/Order.ag"(line 124, column 17)
                         _nontsOo_wantvisit =
                             ({-# LINE 130 "src-ag/Order.ag" #-}
                              visit   _lhsIoptions
                              {-# LINE 1191 "src-ag/Order.hs" #-}
                              )
                         -- "src-ag/Order.ag"(line 124, column 17)
                         _nontsOo_unbox =
                             ({-# LINE 131 "src-ag/Order.ag" #-}
                              unbox     _lhsIoptions
                              {-# LINE 1197 "src-ag/Order.hs" #-}
                              )
                         -- "src-ag/Order.ag"(line 124, column 17)
                         _nontsOo_case =
                             ({-# LINE 132 "src-ag/Order.ag" #-}
                              cases     _lhsIoptions
                              {-# LINE 1203 "src-ag/Order.hs" #-}
                              )
                         -- "src-ag/Order.ag"(line 124, column 17)
                         _nontsOprefix =
                             ({-# LINE 133 "src-ag/Order.ag" #-}
                              prefix    _lhsIoptions
                              {-# LINE 1209 "src-ag/Order.hs" #-}
                              )
                         -- "src-ag/Order.ag"(line 259, column 15)
                         _nontsOvcount =
                             ({-# LINE 259 "src-ag/Order.ag" #-}
                              0
                              {-# LINE 1215 "src-ag/Order.hs" #-}
                              )
                         -- "src-ag/Order.ag"(line 285, column 7)
                         _nontsOmanualAttrDepMap =
                             ({-# LINE 285 "src-ag/Order.ag" #-}
                              manualAttrOrderMap_
                              {-# LINE 1221 "src-ag/Order.hs" #-}
                              )
                         -- "src-ag/Order.ag"(line 414, column 14)
                         _nontsOaroundMap =
                             ({-# LINE 414 "src-ag/Order.ag" #-}
                              aroundsMap_
                              {-# LINE 1227 "src-ag/Order.hs" #-}
                              )
                         -- "src-ag/Order.ag"(line 503, column 13)
                         _nontsOacount =
                             ({-# LINE 503 "src-ag/Order.ag" #-}
                              0
                              {-# LINE 1233 "src-ag/Order.hs" #-}
                              )
                         -- "src-ag/Order.ag"(line 541, column 13)
                         _ruleTable =
                             ({-# LINE 541 "src-ag/Order.ag" #-}
                              Array.array (0,_nontsIvcount-1) (toList _nontsIrules)
                              {-# LINE 1239 "src-ag/Order.hs" #-}
                              )
                         -- "src-ag/Order.ag"(line 542, column 13)
                         _attrTable =
                             ({-# LINE 542 "src-ag/Order.ag" #-}
                              Array.array (0,_nontsIacount-1) (toList _nontsIntattrs)
                              {-# LINE 1245 "src-ag/Order.hs" #-}
                              )
                         -- "src-ag/Order.ag"(line 543, column 13)
                         _attrVertex =
                             ({-# LINE 543 "src-ag/Order.ag" #-}
                              Map.fromList (map swap (toList _nontsIntattrs))
                              {-# LINE 1251 "src-ag/Order.hs" #-}
                              )
                         -- "src-ag/Order.ag"(line 544, column 13)
                         _tdpToTds =
                             ({-# LINE 544 "src-ag/Order.ag" #-}
                              [ (s, maybe (-1) (\v -> findWithErr1 "Grammar.tdpToTds" v _attrVertex) (ntattr cr))
                              | (s,cr) <- toList _nontsIrules]
                              {-# LINE 1258 "src-ag/Order.hs" #-}
                              )
                         -- "src-ag/Order.ag"(line 546, column 13)
                         _tdsToTdp =
                             ({-# LINE 546 "src-ag/Order.ag" #-}
                              let  eq (_,v) (_,v') = v == v'
                                   conv ((s,v):svs)  | v == -1 = Nothing
                                                     | otherwise = Just (v,s:map fst svs)
                              in mapMaybe conv (eqClasses eq _tdpToTds)
                              {-# LINE 1267 "src-ag/Order.hs" #-}
                              )
                         -- "src-ag/Order.ag"(line 550, column 13)
                         _directDep =
                             ({-# LINE 550 "src-ag/Order.ag" #-}
                              toList (_nontsIdirectDep Seq.>< _nontsIadditionalDep)
                              {-# LINE 1273 "src-ag/Order.hs" #-}
                              )
                         -- "src-ag/Order.ag"(line 551, column 13)
                         _instDep =
                             ({-# LINE 551 "src-ag/Order.ag" #-}
                              toList _nontsIinstDep
                              {-# LINE 1279 "src-ag/Order.hs" #-}
                              )
                         -- "src-ag/Order.ag"(line 552, column 13)
                         _aroundDep =
                             ({-# LINE 552 "src-ag/Order.ag" #-}
                              toList _nontsIaroundDep
                              {-# LINE 1285 "src-ag/Order.hs" #-}
                              )
                         -- "src-ag/Order.ag"(line 553, column 13)
                         _mergeDep =
                             ({-# LINE 553 "src-ag/Order.ag" #-}
                              toList _nontsImergeDep
                              {-# LINE 1291 "src-ag/Order.hs" #-}
                              )
                         -- "src-ag/Order.ag"(line 554, column 13)
                         _info =
                             ({-# LINE 554 "src-ag/Order.ag" #-}
                              let def [] = -1
                                  def (v:vs) = v
                               in Info { tdsToTdp   = Array.array (0,_nontsIacount-1) _tdsToTdp
                                       , tdpToTds   = Array.array (0,_nontsIvcount-1) _tdpToTds
                                       , attrTable  = _attrTable
                                       , ruleTable  = _ruleTable
                                       , lmh        = toList _nontsIaranges
                                       , nonts      = _nontsInonts
                                       , wraps      = wrappers_
                                       }
                              {-# LINE 1306 "src-ag/Order.hs" #-}
                              )
                         -- "src-ag/Order.ag"(line 565, column 17)
                         __tup2 =
                             ({-# LINE 565 "src-ag/Order.ag" #-}
                              case computeSequential _info _directDep (_instDep ++ _aroundDep ++ _mergeDep    ) of
                                           CycleFree    cim cvm   -> ( cim
                                                                     , cvm
                                                                     , []
                                                                     )
                                           LocalCycle   errs      -> ( error "No interfaces for AG with local cycles"
                                                                     , error "No visit sub-sequences for AG with local cycles"
                                                                     , map (localCycleErr _ruleTable (visit _lhsIoptions)) errs
                                                                     )
                                           InstCycle    errs      -> ( error "No interfaces for AG with cycles through insts"
                                                                     , error "No visit sub-sequences for AG with cycles through insts"
                                                                     , map (instCycleErr _ruleTable (visit _lhsIoptions)) errs
                                                                     )
                                           DirectCycle  errs      -> ( error "No interfaces for AG with direct cycles"
                                                                     , error "No visit sub-sequences for AG with direct cycles"
                                                                     , directCycleErrs _attrTable _ruleTable (visit _lhsIoptions) errs
                                                                     )
                                           InducedCycle cim errs ->  ( cim
                                                                     , error "No visit sub-sequences for AG with induced cycles"
                                                                     , inducedCycleErrs _attrTable _ruleTable cim errs
                                                                     )
                              {-# LINE 1332 "src-ag/Order.hs" #-}
                              )
                         -- "src-ag/Order.ag"(line 565, column 17)
                         (_cInterfaceMap,_,_) =
                             ({-# LINE 565 "src-ag/Order.ag" #-}
                              __tup2
                              {-# LINE 1338 "src-ag/Order.hs" #-}
                              )
                         -- "src-ag/Order.ag"(line 565, column 17)
                         (_,_cVisitsMap,_) =
                             ({-# LINE 565 "src-ag/Order.ag" #-}
                              __tup2
                              {-# LINE 1344 "src-ag/Order.hs" #-}
                              )
                         -- "src-ag/Order.ag"(line 565, column 17)
                         (_,_,_cyclesErrors) =
                             ({-# LINE 565 "src-ag/Order.ag" #-}
                              __tup2
                              {-# LINE 1350 "src-ag/Order.hs" #-}
                              )
                         -- "src-ag/Order.ag"(line 587, column 13)
                         _lhsOerrors =
                             ({-# LINE 587 "src-ag/Order.ag" #-}
                              (if withCycle _lhsIoptions then Seq.fromList _cyclesErrors else Seq.empty)
                              Seq.>< _nontsIerrors
                              {-# LINE 1357 "src-ag/Order.hs" #-}
                              )
                         -- "src-ag/Order.ag"(line 619, column 15)
                         _lhsOoutput =
                             ({-# LINE 619 "src-ag/Order.ag" #-}
                              CGrammar typeSyns_ derivings_ wrappers_ _nontsIcNonterminals pragmas_ paramMap_ contextMap_ quantMap_ _aroundMap     _mergeMap     _o_dovisit
                              {-# LINE 1363 "src-ag/Order.hs" #-}
                              )
                         -- "src-ag/Order.ag"(line 632, column 14)
                         _aroundMap =
                             ({-# LINE 632 "src-ag/Order.ag" #-}
                              Map.map (Map.map Map.keysSet) aroundsMap_
                              {-# LINE 1369 "src-ag/Order.hs" #-}
                              )
                         -- "src-ag/Order.ag"(line 633, column 14)
                         _mergeMap =
                             ({-# LINE 633 "src-ag/Order.ag" #-}
                              Map.map (Map.map (Map.map (\(nt,srcs,_) -> (nt,srcs)))) mergeMap_
                              {-# LINE 1375 "src-ag/Order.hs" #-}
                              )
                         -- "src-ag/Order.ag"(line 650, column 13)
                         _nontsOallnts =
                             ({-# LINE 650 "src-ag/Order.ag" #-}
                              map fst (_nontsInonts)
                              {-# LINE 1381 "src-ag/Order.hs" #-}
                              )
                         -- "src-ag/DistChildAttr.ag"(line 15, column 13)
                         _nontsOinhMap =
                             ({-# LINE 15 "src-ag/DistChildAttr.ag" #-}
                              _nontsIinhMap'
                              {-# LINE 1387 "src-ag/Order.hs" #-}
                              )
                         -- "src-ag/DistChildAttr.ag"(line 16, column 13)
                         _nontsOsynMap =
                             ({-# LINE 16 "src-ag/DistChildAttr.ag" #-}
                              _nontsIsynMap'
                              {-# LINE 1393 "src-ag/Order.hs" #-}
                              )
                         -- use rule "src-ag/Order.ag"(line 61, column 105)
                         _lhsOnAutoRules =
                             ({-# LINE 61 "src-ag/Order.ag" #-}
                              _nontsInAutoRules
                              {-# LINE 1399 "src-ag/Order.hs" #-}
                              )
                         -- use rule "src-ag/Order.ag"(line 61, column 105)
                         _lhsOnExplicitRules =
                             ({-# LINE 61 "src-ag/Order.ag" #-}
                              _nontsInExplicitRules
                              {-# LINE 1405 "src-ag/Order.hs" #-}
                              )
                         -- copy rule (from local)
                         _nontsOcInterfaceMap =
                             ({-# LINE 594 "src-ag/Order.ag" #-}
                              _cInterfaceMap
                              {-# LINE 1411 "src-ag/Order.hs" #-}
                              )
                         -- copy rule (from local)
                         _nontsOcVisitsMap =
                             ({-# LINE 601 "src-ag/Order.ag" #-}
                              _cVisitsMap
                              {-# LINE 1417 "src-ag/Order.hs" #-}
                              )
                         -- copy rule (from local)
                         _nontsOmergeMap =
                             ({-# LINE 352 "src-ag/Order.ag" #-}
                              _mergeMap
                              {-# LINE 1423 "src-ag/Order.hs" #-}
                              )
                         -- copy rule (from local)
                         _nontsOo_dovisit =
                             ({-# LINE 116 "src-ag/Order.ag" #-}
                              _o_dovisit
                              {-# LINE 1429 "src-ag/Order.hs" #-}
                              )
                         ( _nontsIacount,_nontsIadditionalDep,_nontsIaranges,_nontsIaroundDep,_nontsIcNonterminals,_nontsIdirectDep,_nontsIerrors,_nontsIinhMap',_nontsIinstDep,_nontsImergeDep,_nontsInAutoRules,_nontsInExplicitRules,_nontsInonts,_nontsIntattrs,_nontsIrules,_nontsIsynMap',_nontsIvcount) =
                             nonts_ _nontsOacount _nontsOallnts _nontsOaroundMap _nontsOcInterfaceMap _nontsOcVisitsMap _nontsOinhMap _nontsOmanualAttrDepMap _nontsOmergeMap _nontsOo_case _nontsOo_cata _nontsOo_data _nontsOo_dovisit _nontsOo_newtypes _nontsOo_rename _nontsOo_sem _nontsOo_sig _nontsOo_unbox _nontsOo_wantvisit _nontsOprefix _nontsOsynMap _nontsOvcount 
                     in  ( _lhsOerrors,_lhsOnAutoRules,_lhsOnExplicitRules,_lhsOoutput))) )
-- Nonterminal -------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allnts               : [Identifier]
         aroundMap            : Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))
         cInterfaceMap        : CInterfaceMap
         cVisitsMap           : CVisitsMap
         inhMap               : Map Identifier Attributes
         manualAttrDepMap     : AttrOrderMap
         mergeMap             : Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier])))
         o_case               : Bool
         o_cata               : Bool
         o_data               : Bool
         o_dovisit            : Bool
         o_newtypes           : Bool
         o_rename             : Bool
         o_sem                : Bool
         o_sig                : Bool
         o_unbox              : Bool
         o_wantvisit          : Bool
         prefix               : String
         synMap               : Map Identifier Attributes
      chained attributes:
         acount               : Int
         vcount               : Int
      synthesized attributes:
         additionalDep        : Seq Edge
         aranges              : Seq (Int,Int,Int)
         aroundDep            : Seq Edge
         cNonterminal         : CNonterminal
         directDep            : Seq Edge
         errors               : Seq Error
         inhMap'              : Map Identifier Attributes
         instDep              : Seq Edge
         mergeDep             : Seq Edge
         nAutoRules           : Int
         nExplicitRules       : Int
         nonts                : [(NontermIdent,[ConstructorIdent])]
         ntattrs              : Seq (Vertex,NTAttr)
         rules                : Seq (Vertex,CRule)
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
            local aroundMap   : _
            local ntattrs     : _
            local cInter      : _
-}
-- cata
sem_Nonterminal :: Nonterminal  ->
                   T_Nonterminal 
sem_Nonterminal (Nonterminal _nt _params _inh _syn _prods )  =
    (sem_Nonterminal_Nonterminal _nt _params _inh _syn (sem_Productions _prods ) )
-- semantic domain
newtype T_Nonterminal  = T_Nonterminal (Int ->
                                        ([Identifier]) ->
                                        (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) ->
                                        CInterfaceMap ->
                                        CVisitsMap ->
                                        (Map Identifier Attributes) ->
                                        AttrOrderMap ->
                                        (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier])))) ->
                                        Bool ->
                                        Bool ->
                                        Bool ->
                                        Bool ->
                                        Bool ->
                                        Bool ->
                                        Bool ->
                                        Bool ->
                                        Bool ->
                                        Bool ->
                                        String ->
                                        (Map Identifier Attributes) ->
                                        Int ->
                                        ( Int,(Seq Edge),(Seq (Int,Int,Int)),(Seq Edge),CNonterminal,(Seq Edge),(Seq Error),(Map Identifier Attributes),(Seq Edge),(Seq Edge),Int,Int,([(NontermIdent,[ConstructorIdent])]),(Seq (Vertex,NTAttr)),(Seq (Vertex,CRule)),(Map Identifier Attributes),Int))
data Inh_Nonterminal  = Inh_Nonterminal {acount_Inh_Nonterminal :: !(Int),allnts_Inh_Nonterminal :: !(([Identifier])),aroundMap_Inh_Nonterminal :: !((Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression])))),cInterfaceMap_Inh_Nonterminal :: !(CInterfaceMap),cVisitsMap_Inh_Nonterminal :: !(CVisitsMap),inhMap_Inh_Nonterminal :: !((Map Identifier Attributes)),manualAttrDepMap_Inh_Nonterminal :: !(AttrOrderMap),mergeMap_Inh_Nonterminal :: !((Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier]))))),o_case_Inh_Nonterminal :: !(Bool),o_cata_Inh_Nonterminal :: !(Bool),o_data_Inh_Nonterminal :: !(Bool),o_dovisit_Inh_Nonterminal :: !(Bool),o_newtypes_Inh_Nonterminal :: !(Bool),o_rename_Inh_Nonterminal :: !(Bool),o_sem_Inh_Nonterminal :: !(Bool),o_sig_Inh_Nonterminal :: !(Bool),o_unbox_Inh_Nonterminal :: !(Bool),o_wantvisit_Inh_Nonterminal :: !(Bool),prefix_Inh_Nonterminal :: !(String),synMap_Inh_Nonterminal :: !((Map Identifier Attributes)),vcount_Inh_Nonterminal :: !(Int)}
data Syn_Nonterminal  = Syn_Nonterminal {acount_Syn_Nonterminal :: !(Int),additionalDep_Syn_Nonterminal :: !((Seq Edge)),aranges_Syn_Nonterminal :: !((Seq (Int,Int,Int))),aroundDep_Syn_Nonterminal :: !((Seq Edge)),cNonterminal_Syn_Nonterminal :: !(CNonterminal),directDep_Syn_Nonterminal :: !((Seq Edge)),errors_Syn_Nonterminal :: !((Seq Error)),inhMap'_Syn_Nonterminal :: !((Map Identifier Attributes)),instDep_Syn_Nonterminal :: !((Seq Edge)),mergeDep_Syn_Nonterminal :: !((Seq Edge)),nAutoRules_Syn_Nonterminal :: !(Int),nExplicitRules_Syn_Nonterminal :: !(Int),nonts_Syn_Nonterminal :: !(([(NontermIdent,[ConstructorIdent])])),ntattrs_Syn_Nonterminal :: !((Seq (Vertex,NTAttr))),rules_Syn_Nonterminal :: !((Seq (Vertex,CRule))),synMap'_Syn_Nonterminal :: !((Map Identifier Attributes)),vcount_Syn_Nonterminal :: !(Int)}
wrap_Nonterminal :: T_Nonterminal  ->
                    Inh_Nonterminal  ->
                    Syn_Nonterminal 
wrap_Nonterminal (T_Nonterminal sem ) (Inh_Nonterminal _lhsIacount _lhsIallnts _lhsIaroundMap _lhsIcInterfaceMap _lhsIcVisitsMap _lhsIinhMap _lhsImanualAttrDepMap _lhsImergeMap _lhsIo_case _lhsIo_cata _lhsIo_data _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_unbox _lhsIo_wantvisit _lhsIprefix _lhsIsynMap _lhsIvcount )  =
    (let ( _lhsOacount,_lhsOadditionalDep,_lhsOaranges,_lhsOaroundDep,_lhsOcNonterminal,_lhsOdirectDep,_lhsOerrors,_lhsOinhMap',_lhsOinstDep,_lhsOmergeDep,_lhsOnAutoRules,_lhsOnExplicitRules,_lhsOnonts,_lhsOntattrs,_lhsOrules,_lhsOsynMap',_lhsOvcount) = sem _lhsIacount _lhsIallnts _lhsIaroundMap _lhsIcInterfaceMap _lhsIcVisitsMap _lhsIinhMap _lhsImanualAttrDepMap _lhsImergeMap _lhsIo_case _lhsIo_cata _lhsIo_data _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_unbox _lhsIo_wantvisit _lhsIprefix _lhsIsynMap _lhsIvcount 
     in  (Syn_Nonterminal _lhsOacount _lhsOadditionalDep _lhsOaranges _lhsOaroundDep _lhsOcNonterminal _lhsOdirectDep _lhsOerrors _lhsOinhMap' _lhsOinstDep _lhsOmergeDep _lhsOnAutoRules _lhsOnExplicitRules _lhsOnonts _lhsOntattrs _lhsOrules _lhsOsynMap' _lhsOvcount ))
sem_Nonterminal_Nonterminal :: NontermIdent ->
                               ([Identifier]) ->
                               Attributes ->
                               Attributes ->
                               T_Productions  ->
                               T_Nonterminal 
sem_Nonterminal_Nonterminal nt_ params_ inh_ syn_ (T_Productions prods_ )  =
    (T_Nonterminal (\ _lhsIacount
                      _lhsIallnts
                      _lhsIaroundMap
                      _lhsIcInterfaceMap
                      _lhsIcVisitsMap
                      _lhsIinhMap
                      _lhsImanualAttrDepMap
                      _lhsImergeMap
                      _lhsIo_case
                      _lhsIo_cata
                      _lhsIo_data
                      _lhsIo_dovisit
                      _lhsIo_newtypes
                      _lhsIo_rename
                      _lhsIo_sem
                      _lhsIo_sig
                      _lhsIo_unbox
                      _lhsIo_wantvisit
                      _lhsIprefix
                      _lhsIsynMap
                      _lhsIvcount ->
                        (let _prodsOnt :: Identifier
                             _prodsOinh :: Attributes
                             _prodsOsyn :: Attributes
                             _lhsOntattrs :: (Seq (Vertex,NTAttr))
                             _lhsOacount :: Int
                             _lhsOaranges :: (Seq (Int,Int,Int))
                             _lhsOnonts :: ([(NontermIdent,[ConstructorIdent])])
                             _lhsOcNonterminal :: CNonterminal
                             _lhsOinhMap' :: (Map Identifier Attributes)
                             _lhsOsynMap' :: (Map Identifier Attributes)
                             _lhsOadditionalDep :: (Seq Edge)
                             _lhsOaroundDep :: (Seq Edge)
                             _lhsOdirectDep :: (Seq Edge)
                             _lhsOerrors :: (Seq Error)
                             _lhsOinstDep :: (Seq Edge)
                             _lhsOmergeDep :: (Seq Edge)
                             _lhsOnAutoRules :: Int
                             _lhsOnExplicitRules :: Int
                             _lhsOrules :: (Seq (Vertex,CRule))
                             _lhsOvcount :: Int
                             _prodsOallnts :: ([Identifier])
                             _prodsOaroundMap :: (Map ConstructorIdent (Map Identifier [Expression]))
                             _prodsOcVisitsMap :: CVisitsMap
                             _prodsOinhMap :: (Map Identifier Attributes)
                             _prodsOmanualAttrDepMap :: AttrOrderMap
                             _prodsOmergeMap :: (Map ConstructorIdent (Map Identifier (Identifier,[Identifier])))
                             _prodsOo_case :: Bool
                             _prodsOo_cata :: Bool
                             _prodsOo_dovisit :: Bool
                             _prodsOo_newtypes :: Bool
                             _prodsOo_rename :: Bool
                             _prodsOo_sem :: Bool
                             _prodsOo_sig :: Bool
                             _prodsOo_unbox :: Bool
                             _prodsOo_wantvisit :: Bool
                             _prodsOprefix :: String
                             _prodsOsynMap :: (Map Identifier Attributes)
                             _prodsOvcount :: Int
                             _prodsIadditionalDep :: (Seq Edge)
                             _prodsIaroundDep :: (Seq Edge)
                             _prodsIcProductions :: CProductions
                             _prodsIcons :: ([ConstructorIdent])
                             _prodsIdirectDep :: (Seq Edge)
                             _prodsIerrors :: (Seq Error)
                             _prodsIinstDep :: (Seq Edge)
                             _prodsImergeDep :: (Seq Edge)
                             _prodsInAutoRules :: Int
                             _prodsInExplicitRules :: Int
                             _prodsIrules :: (Seq (Vertex,CRule))
                             _prodsIvcount :: Int
                             -- "src-ag/Order.ag"(line 97, column 17)
                             _prodsOnt =
                                 ({-# LINE 97 "src-ag/Order.ag" #-}
                                  nt_
                                  {-# LINE 1607 "src-ag/Order.hs" #-}
                                  )
                             -- "src-ag/Order.ag"(line 100, column 17)
                             _prodsOinh =
                                 ({-# LINE 100 "src-ag/Order.ag" #-}
                                  inh_
                                  {-# LINE 1613 "src-ag/Order.hs" #-}
                                  )
                             -- "src-ag/Order.ag"(line 101, column 17)
                             _prodsOsyn =
                                 ({-# LINE 101 "src-ag/Order.ag" #-}
                                  syn_
                                  {-# LINE 1619 "src-ag/Order.hs" #-}
                                  )
                             -- "src-ag/Order.ag"(line 357, column 32)
                             _mergeMap =
                                 ({-# LINE 357 "src-ag/Order.ag" #-}
                                  Map.findWithDefault Map.empty nt_ _lhsImergeMap
                                  {-# LINE 1625 "src-ag/Order.hs" #-}
                                  )
                             -- "src-ag/Order.ag"(line 410, column 32)
                             _aroundMap =
                                 ({-# LINE 410 "src-ag/Order.ag" #-}
                                  Map.findWithDefault Map.empty nt_ _lhsIaroundMap
                                  {-# LINE 1631 "src-ag/Order.hs" #-}
                                  )
                             -- "src-ag/Order.ag"(line 506, column 17)
                             _ntattrs =
                                 ({-# LINE 506 "src-ag/Order.ag" #-}
                                  [ NTAInh nt_ inh tp | (inh,tp) <- Map.assocs inh_ ]
                                  ++ [NTASyn nt_ syn tp | (syn,tp) <- Map.assocs syn_ ]
                                  {-# LINE 1638 "src-ag/Order.hs" #-}
                                  )
                             -- "src-ag/Order.ag"(line 508, column 17)
                             _lhsOntattrs =
                                 ({-# LINE 508 "src-ag/Order.ag" #-}
                                  Seq.fromList (zip [_lhsIacount ..] _ntattrs)
                                  {-# LINE 1644 "src-ag/Order.hs" #-}
                                  )
                             -- "src-ag/Order.ag"(line 509, column 17)
                             _lhsOacount =
                                 ({-# LINE 509 "src-ag/Order.ag" #-}
                                  _lhsIacount + Map.size inh_ + Map.size syn_
                                  {-# LINE 1650 "src-ag/Order.hs" #-}
                                  )
                             -- "src-ag/Order.ag"(line 510, column 17)
                             _lhsOaranges =
                                 ({-# LINE 510 "src-ag/Order.ag" #-}
                                  Seq.singleton
                                   (_lhsIacount
                                   ,_lhsIacount + Map.size inh_
                                   ,_lhsIacount + Map.size syn_ + Map.size inh_ - 1)
                                  {-# LINE 1659 "src-ag/Order.hs" #-}
                                  )
                             -- "src-ag/Order.ag"(line 519, column 19)
                             _lhsOnonts =
                                 ({-# LINE 519 "src-ag/Order.ag" #-}
                                  [(nt_,_prodsIcons)]
                                  {-# LINE 1665 "src-ag/Order.hs" #-}
                                  )
                             -- "src-ag/Order.ag"(line 596, column 19)
                             _cInter =
                                 ({-# LINE 596 "src-ag/Order.ag" #-}
                                  if  _lhsIo_dovisit
                                         then findWithErr1 "Nonterminal.cInter" nt_ _lhsIcInterfaceMap
                                         else CInterface [CSegment inh_ syn_]
                                  {-# LINE 1673 "src-ag/Order.hs" #-}
                                  )
                             -- "src-ag/Order.ag"(line 624, column 19)
                             _lhsOcNonterminal =
                                 ({-# LINE 624 "src-ag/Order.ag" #-}
                                  CNonterminal nt_ params_ inh_ syn_ _prodsIcProductions _cInter
                                  {-# LINE 1679 "src-ag/Order.hs" #-}
                                  )
                             -- "src-ag/DistChildAttr.ag"(line 7, column 18)
                             _lhsOinhMap' =
                                 ({-# LINE 7 "src-ag/DistChildAttr.ag" #-}
                                  Map.singleton nt_ inh_
                                  {-# LINE 1685 "src-ag/Order.hs" #-}
                                  )
                             -- "src-ag/DistChildAttr.ag"(line 8, column 18)
                             _lhsOsynMap' =
                                 ({-# LINE 8 "src-ag/DistChildAttr.ag" #-}
                                  Map.singleton nt_ syn_
                                  {-# LINE 1691 "src-ag/Order.hs" #-}
                                  )
                             -- use rule "src-ag/Order.ag"(line 281, column 60)
                             _lhsOadditionalDep =
                                 ({-# LINE 281 "src-ag/Order.ag" #-}
                                  _prodsIadditionalDep
                                  {-# LINE 1697 "src-ag/Order.hs" #-}
                                  )
                             -- use rule "src-ag/Order.ag"(line 402, column 24)
                             _lhsOaroundDep =
                                 ({-# LINE 402 "src-ag/Order.ag" #-}
                                  _prodsIaroundDep
                                  {-# LINE 1703 "src-ag/Order.hs" #-}
                                  )
                             -- use rule "src-ag/Order.ag"(line 267, column 33)
                             _lhsOdirectDep =
                                 ({-# LINE 267 "src-ag/Order.ag" #-}
                                  _prodsIdirectDep
                                  {-# LINE 1709 "src-ag/Order.hs" #-}
                                  )
                             -- use rule "src-ag/Order.ag"(line 84, column 70)
                             _lhsOerrors =
                                 ({-# LINE 84 "src-ag/Order.ag" #-}
                                  _prodsIerrors
                                  {-# LINE 1715 "src-ag/Order.hs" #-}
                                  )
                             -- use rule "src-ag/Order.ag"(line 310, column 31)
                             _lhsOinstDep =
                                 ({-# LINE 310 "src-ag/Order.ag" #-}
                                  _prodsIinstDep
                                  {-# LINE 1721 "src-ag/Order.hs" #-}
                                  )
                             -- use rule "src-ag/Order.ag"(line 365, column 18)
                             _lhsOmergeDep =
                                 ({-# LINE 365 "src-ag/Order.ag" #-}
                                  _prodsImergeDep
                                  {-# LINE 1727 "src-ag/Order.hs" #-}
                                  )
                             -- use rule "src-ag/Order.ag"(line 61, column 105)
                             _lhsOnAutoRules =
                                 ({-# LINE 61 "src-ag/Order.ag" #-}
                                  _prodsInAutoRules
                                  {-# LINE 1733 "src-ag/Order.hs" #-}
                                  )
                             -- use rule "src-ag/Order.ag"(line 61, column 105)
                             _lhsOnExplicitRules =
                                 ({-# LINE 61 "src-ag/Order.ag" #-}
                                  _prodsInExplicitRules
                                  {-# LINE 1739 "src-ag/Order.hs" #-}
                                  )
                             -- use rule "src-ag/Order.ag"(line 257, column 18)
                             _lhsOrules =
                                 ({-# LINE 257 "src-ag/Order.ag" #-}
                                  _prodsIrules
                                  {-# LINE 1745 "src-ag/Order.hs" #-}
                                  )
                             -- copy rule (up)
                             _lhsOvcount =
                                 ({-# LINE 256 "src-ag/Order.ag" #-}
                                  _prodsIvcount
                                  {-# LINE 1751 "src-ag/Order.hs" #-}
                                  )
                             -- copy rule (down)
                             _prodsOallnts =
                                 ({-# LINE 647 "src-ag/Order.ag" #-}
                                  _lhsIallnts
                                  {-# LINE 1757 "src-ag/Order.hs" #-}
                                  )
                             -- copy rule (from local)
                             _prodsOaroundMap =
                                 ({-# LINE 408 "src-ag/Order.ag" #-}
                                  _aroundMap
                                  {-# LINE 1763 "src-ag/Order.hs" #-}
                                  )
                             -- copy rule (down)
                             _prodsOcVisitsMap =
                                 ({-# LINE 601 "src-ag/Order.ag" #-}
                                  _lhsIcVisitsMap
                                  {-# LINE 1769 "src-ag/Order.hs" #-}
                                  )
                             -- copy rule (down)
                             _prodsOinhMap =
                                 ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                  _lhsIinhMap
                                  {-# LINE 1775 "src-ag/Order.hs" #-}
                                  )
                             -- copy rule (down)
                             _prodsOmanualAttrDepMap =
                                 ({-# LINE 281 "src-ag/Order.ag" #-}
                                  _lhsImanualAttrDepMap
                                  {-# LINE 1781 "src-ag/Order.hs" #-}
                                  )
                             -- copy rule (from local)
                             _prodsOmergeMap =
                                 ({-# LINE 355 "src-ag/Order.ag" #-}
                                  _mergeMap
                                  {-# LINE 1787 "src-ag/Order.hs" #-}
                                  )
                             -- copy rule (down)
                             _prodsOo_case =
                                 ({-# LINE 117 "src-ag/Order.ag" #-}
                                  _lhsIo_case
                                  {-# LINE 1793 "src-ag/Order.hs" #-}
                                  )
                             -- copy rule (down)
                             _prodsOo_cata =
                                 ({-# LINE 111 "src-ag/Order.ag" #-}
                                  _lhsIo_cata
                                  {-# LINE 1799 "src-ag/Order.hs" #-}
                                  )
                             -- copy rule (down)
                             _prodsOo_dovisit =
                                 ({-# LINE 116 "src-ag/Order.ag" #-}
                                  _lhsIo_dovisit
                                  {-# LINE 1805 "src-ag/Order.hs" #-}
                                  )
                             -- copy rule (down)
                             _prodsOo_newtypes =
                                 ({-# LINE 110 "src-ag/Order.ag" #-}
                                  _lhsIo_newtypes
                                  {-# LINE 1811 "src-ag/Order.hs" #-}
                                  )
                             -- copy rule (down)
                             _prodsOo_rename =
                                 ({-# LINE 114 "src-ag/Order.ag" #-}
                                  _lhsIo_rename
                                  {-# LINE 1817 "src-ag/Order.hs" #-}
                                  )
                             -- copy rule (down)
                             _prodsOo_sem =
                                 ({-# LINE 113 "src-ag/Order.ag" #-}
                                  _lhsIo_sem
                                  {-# LINE 1823 "src-ag/Order.hs" #-}
                                  )
                             -- copy rule (down)
                             _prodsOo_sig =
                                 ({-# LINE 112 "src-ag/Order.ag" #-}
                                  _lhsIo_sig
                                  {-# LINE 1829 "src-ag/Order.hs" #-}
                                  )
                             -- copy rule (down)
                             _prodsOo_unbox =
                                 ({-# LINE 119 "src-ag/Order.ag" #-}
                                  _lhsIo_unbox
                                  {-# LINE 1835 "src-ag/Order.hs" #-}
                                  )
                             -- copy rule (down)
                             _prodsOo_wantvisit =
                                 ({-# LINE 115 "src-ag/Order.ag" #-}
                                  _lhsIo_wantvisit
                                  {-# LINE 1841 "src-ag/Order.hs" #-}
                                  )
                             -- copy rule (down)
                             _prodsOprefix =
                                 ({-# LINE 118 "src-ag/Order.ag" #-}
                                  _lhsIprefix
                                  {-# LINE 1847 "src-ag/Order.hs" #-}
                                  )
                             -- copy rule (down)
                             _prodsOsynMap =
                                 ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                  _lhsIsynMap
                                  {-# LINE 1853 "src-ag/Order.hs" #-}
                                  )
                             -- copy rule (down)
                             _prodsOvcount =
                                 ({-# LINE 256 "src-ag/Order.ag" #-}
                                  _lhsIvcount
                                  {-# LINE 1859 "src-ag/Order.hs" #-}
                                  )
                             ( _prodsIadditionalDep,_prodsIaroundDep,_prodsIcProductions,_prodsIcons,_prodsIdirectDep,_prodsIerrors,_prodsIinstDep,_prodsImergeDep,_prodsInAutoRules,_prodsInExplicitRules,_prodsIrules,_prodsIvcount) =
                                 prods_ _prodsOallnts _prodsOaroundMap _prodsOcVisitsMap _prodsOinh _prodsOinhMap _prodsOmanualAttrDepMap _prodsOmergeMap _prodsOnt _prodsOo_case _prodsOo_cata _prodsOo_dovisit _prodsOo_newtypes _prodsOo_rename _prodsOo_sem _prodsOo_sig _prodsOo_unbox _prodsOo_wantvisit _prodsOprefix _prodsOsyn _prodsOsynMap _prodsOvcount 
                         in  ( _lhsOacount,_lhsOadditionalDep,_lhsOaranges,_lhsOaroundDep,_lhsOcNonterminal,_lhsOdirectDep,_lhsOerrors,_lhsOinhMap',_lhsOinstDep,_lhsOmergeDep,_lhsOnAutoRules,_lhsOnExplicitRules,_lhsOnonts,_lhsOntattrs,_lhsOrules,_lhsOsynMap',_lhsOvcount))) )
-- Nonterminals ------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allnts               : [Identifier]
         aroundMap            : Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))
         cInterfaceMap        : CInterfaceMap
         cVisitsMap           : CVisitsMap
         inhMap               : Map Identifier Attributes
         manualAttrDepMap     : AttrOrderMap
         mergeMap             : Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier])))
         o_case               : Bool
         o_cata               : Bool
         o_data               : Bool
         o_dovisit            : Bool
         o_newtypes           : Bool
         o_rename             : Bool
         o_sem                : Bool
         o_sig                : Bool
         o_unbox              : Bool
         o_wantvisit          : Bool
         prefix               : String
         synMap               : Map Identifier Attributes
      chained attributes:
         acount               : Int
         vcount               : Int
      synthesized attributes:
         additionalDep        : Seq Edge
         aranges              : Seq (Int,Int,Int)
         aroundDep            : Seq Edge
         cNonterminals        : CNonterminals
         directDep            : Seq Edge
         errors               : Seq Error
         inhMap'              : Map Identifier Attributes
         instDep              : Seq Edge
         mergeDep             : Seq Edge
         nAutoRules           : Int
         nExplicitRules       : Int
         nonts                : [(NontermIdent,[ConstructorIdent])]
         ntattrs              : Seq (Vertex,NTAttr)
         rules                : Seq (Vertex,CRule)
         synMap'              : Map Identifier Attributes
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
                                          ([Identifier]) ->
                                          (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) ->
                                          CInterfaceMap ->
                                          CVisitsMap ->
                                          (Map Identifier Attributes) ->
                                          AttrOrderMap ->
                                          (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier])))) ->
                                          Bool ->
                                          Bool ->
                                          Bool ->
                                          Bool ->
                                          Bool ->
                                          Bool ->
                                          Bool ->
                                          Bool ->
                                          Bool ->
                                          Bool ->
                                          String ->
                                          (Map Identifier Attributes) ->
                                          Int ->
                                          ( Int,(Seq Edge),(Seq (Int,Int,Int)),(Seq Edge),CNonterminals,(Seq Edge),(Seq Error),(Map Identifier Attributes),(Seq Edge),(Seq Edge),Int,Int,([(NontermIdent,[ConstructorIdent])]),(Seq (Vertex,NTAttr)),(Seq (Vertex,CRule)),(Map Identifier Attributes),Int))
data Inh_Nonterminals  = Inh_Nonterminals {acount_Inh_Nonterminals :: !(Int),allnts_Inh_Nonterminals :: !(([Identifier])),aroundMap_Inh_Nonterminals :: !((Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression])))),cInterfaceMap_Inh_Nonterminals :: !(CInterfaceMap),cVisitsMap_Inh_Nonterminals :: !(CVisitsMap),inhMap_Inh_Nonterminals :: !((Map Identifier Attributes)),manualAttrDepMap_Inh_Nonterminals :: !(AttrOrderMap),mergeMap_Inh_Nonterminals :: !((Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier]))))),o_case_Inh_Nonterminals :: !(Bool),o_cata_Inh_Nonterminals :: !(Bool),o_data_Inh_Nonterminals :: !(Bool),o_dovisit_Inh_Nonterminals :: !(Bool),o_newtypes_Inh_Nonterminals :: !(Bool),o_rename_Inh_Nonterminals :: !(Bool),o_sem_Inh_Nonterminals :: !(Bool),o_sig_Inh_Nonterminals :: !(Bool),o_unbox_Inh_Nonterminals :: !(Bool),o_wantvisit_Inh_Nonterminals :: !(Bool),prefix_Inh_Nonterminals :: !(String),synMap_Inh_Nonterminals :: !((Map Identifier Attributes)),vcount_Inh_Nonterminals :: !(Int)}
data Syn_Nonterminals  = Syn_Nonterminals {acount_Syn_Nonterminals :: !(Int),additionalDep_Syn_Nonterminals :: !((Seq Edge)),aranges_Syn_Nonterminals :: !((Seq (Int,Int,Int))),aroundDep_Syn_Nonterminals :: !((Seq Edge)),cNonterminals_Syn_Nonterminals :: !(CNonterminals),directDep_Syn_Nonterminals :: !((Seq Edge)),errors_Syn_Nonterminals :: !((Seq Error)),inhMap'_Syn_Nonterminals :: !((Map Identifier Attributes)),instDep_Syn_Nonterminals :: !((Seq Edge)),mergeDep_Syn_Nonterminals :: !((Seq Edge)),nAutoRules_Syn_Nonterminals :: !(Int),nExplicitRules_Syn_Nonterminals :: !(Int),nonts_Syn_Nonterminals :: !(([(NontermIdent,[ConstructorIdent])])),ntattrs_Syn_Nonterminals :: !((Seq (Vertex,NTAttr))),rules_Syn_Nonterminals :: !((Seq (Vertex,CRule))),synMap'_Syn_Nonterminals :: !((Map Identifier Attributes)),vcount_Syn_Nonterminals :: !(Int)}
wrap_Nonterminals :: T_Nonterminals  ->
                     Inh_Nonterminals  ->
                     Syn_Nonterminals 
wrap_Nonterminals (T_Nonterminals sem ) (Inh_Nonterminals _lhsIacount _lhsIallnts _lhsIaroundMap _lhsIcInterfaceMap _lhsIcVisitsMap _lhsIinhMap _lhsImanualAttrDepMap _lhsImergeMap _lhsIo_case _lhsIo_cata _lhsIo_data _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_unbox _lhsIo_wantvisit _lhsIprefix _lhsIsynMap _lhsIvcount )  =
    (let ( _lhsOacount,_lhsOadditionalDep,_lhsOaranges,_lhsOaroundDep,_lhsOcNonterminals,_lhsOdirectDep,_lhsOerrors,_lhsOinhMap',_lhsOinstDep,_lhsOmergeDep,_lhsOnAutoRules,_lhsOnExplicitRules,_lhsOnonts,_lhsOntattrs,_lhsOrules,_lhsOsynMap',_lhsOvcount) = sem _lhsIacount _lhsIallnts _lhsIaroundMap _lhsIcInterfaceMap _lhsIcVisitsMap _lhsIinhMap _lhsImanualAttrDepMap _lhsImergeMap _lhsIo_case _lhsIo_cata _lhsIo_data _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_unbox _lhsIo_wantvisit _lhsIprefix _lhsIsynMap _lhsIvcount 
     in  (Syn_Nonterminals _lhsOacount _lhsOadditionalDep _lhsOaranges _lhsOaroundDep _lhsOcNonterminals _lhsOdirectDep _lhsOerrors _lhsOinhMap' _lhsOinstDep _lhsOmergeDep _lhsOnAutoRules _lhsOnExplicitRules _lhsOnonts _lhsOntattrs _lhsOrules _lhsOsynMap' _lhsOvcount ))
sem_Nonterminals_Cons :: T_Nonterminal  ->
                         T_Nonterminals  ->
                         T_Nonterminals 
sem_Nonterminals_Cons (T_Nonterminal hd_ ) (T_Nonterminals tl_ )  =
    (T_Nonterminals (\ _lhsIacount
                       _lhsIallnts
                       _lhsIaroundMap
                       _lhsIcInterfaceMap
                       _lhsIcVisitsMap
                       _lhsIinhMap
                       _lhsImanualAttrDepMap
                       _lhsImergeMap
                       _lhsIo_case
                       _lhsIo_cata
                       _lhsIo_data
                       _lhsIo_dovisit
                       _lhsIo_newtypes
                       _lhsIo_rename
                       _lhsIo_sem
                       _lhsIo_sig
                       _lhsIo_unbox
                       _lhsIo_wantvisit
                       _lhsIprefix
                       _lhsIsynMap
                       _lhsIvcount ->
                         (let _lhsOcNonterminals :: CNonterminals
                              _lhsOadditionalDep :: (Seq Edge)
                              _lhsOaranges :: (Seq (Int,Int,Int))
                              _lhsOaroundDep :: (Seq Edge)
                              _lhsOdirectDep :: (Seq Edge)
                              _lhsOerrors :: (Seq Error)
                              _lhsOinhMap' :: (Map Identifier Attributes)
                              _lhsOinstDep :: (Seq Edge)
                              _lhsOmergeDep :: (Seq Edge)
                              _lhsOnAutoRules :: Int
                              _lhsOnExplicitRules :: Int
                              _lhsOnonts :: ([(NontermIdent,[ConstructorIdent])])
                              _lhsOntattrs :: (Seq (Vertex,NTAttr))
                              _lhsOrules :: (Seq (Vertex,CRule))
                              _lhsOsynMap' :: (Map Identifier Attributes)
                              _lhsOacount :: Int
                              _lhsOvcount :: Int
                              _hdOacount :: Int
                              _hdOallnts :: ([Identifier])
                              _hdOaroundMap :: (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression])))
                              _hdOcInterfaceMap :: CInterfaceMap
                              _hdOcVisitsMap :: CVisitsMap
                              _hdOinhMap :: (Map Identifier Attributes)
                              _hdOmanualAttrDepMap :: AttrOrderMap
                              _hdOmergeMap :: (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier]))))
                              _hdOo_case :: Bool
                              _hdOo_cata :: Bool
                              _hdOo_data :: Bool
                              _hdOo_dovisit :: Bool
                              _hdOo_newtypes :: Bool
                              _hdOo_rename :: Bool
                              _hdOo_sem :: Bool
                              _hdOo_sig :: Bool
                              _hdOo_unbox :: Bool
                              _hdOo_wantvisit :: Bool
                              _hdOprefix :: String
                              _hdOsynMap :: (Map Identifier Attributes)
                              _hdOvcount :: Int
                              _tlOacount :: Int
                              _tlOallnts :: ([Identifier])
                              _tlOaroundMap :: (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression])))
                              _tlOcInterfaceMap :: CInterfaceMap
                              _tlOcVisitsMap :: CVisitsMap
                              _tlOinhMap :: (Map Identifier Attributes)
                              _tlOmanualAttrDepMap :: AttrOrderMap
                              _tlOmergeMap :: (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier]))))
                              _tlOo_case :: Bool
                              _tlOo_cata :: Bool
                              _tlOo_data :: Bool
                              _tlOo_dovisit :: Bool
                              _tlOo_newtypes :: Bool
                              _tlOo_rename :: Bool
                              _tlOo_sem :: Bool
                              _tlOo_sig :: Bool
                              _tlOo_unbox :: Bool
                              _tlOo_wantvisit :: Bool
                              _tlOprefix :: String
                              _tlOsynMap :: (Map Identifier Attributes)
                              _tlOvcount :: Int
                              _hdIacount :: Int
                              _hdIadditionalDep :: (Seq Edge)
                              _hdIaranges :: (Seq (Int,Int,Int))
                              _hdIaroundDep :: (Seq Edge)
                              _hdIcNonterminal :: CNonterminal
                              _hdIdirectDep :: (Seq Edge)
                              _hdIerrors :: (Seq Error)
                              _hdIinhMap' :: (Map Identifier Attributes)
                              _hdIinstDep :: (Seq Edge)
                              _hdImergeDep :: (Seq Edge)
                              _hdInAutoRules :: Int
                              _hdInExplicitRules :: Int
                              _hdInonts :: ([(NontermIdent,[ConstructorIdent])])
                              _hdIntattrs :: (Seq (Vertex,NTAttr))
                              _hdIrules :: (Seq (Vertex,CRule))
                              _hdIsynMap' :: (Map Identifier Attributes)
                              _hdIvcount :: Int
                              _tlIacount :: Int
                              _tlIadditionalDep :: (Seq Edge)
                              _tlIaranges :: (Seq (Int,Int,Int))
                              _tlIaroundDep :: (Seq Edge)
                              _tlIcNonterminals :: CNonterminals
                              _tlIdirectDep :: (Seq Edge)
                              _tlIerrors :: (Seq Error)
                              _tlIinhMap' :: (Map Identifier Attributes)
                              _tlIinstDep :: (Seq Edge)
                              _tlImergeDep :: (Seq Edge)
                              _tlInAutoRules :: Int
                              _tlInExplicitRules :: Int
                              _tlInonts :: ([(NontermIdent,[ConstructorIdent])])
                              _tlIntattrs :: (Seq (Vertex,NTAttr))
                              _tlIrules :: (Seq (Vertex,CRule))
                              _tlIsynMap' :: (Map Identifier Attributes)
                              _tlIvcount :: Int
                              -- "src-ag/Order.ag"(line 621, column 12)
                              _lhsOcNonterminals =
                                  ({-# LINE 621 "src-ag/Order.ag" #-}
                                   _hdIcNonterminal : _tlIcNonterminals
                                   {-# LINE 2070 "src-ag/Order.hs" #-}
                                   )
                              -- use rule "src-ag/Order.ag"(line 281, column 60)
                              _lhsOadditionalDep =
                                  ({-# LINE 281 "src-ag/Order.ag" #-}
                                   _hdIadditionalDep Seq.>< _tlIadditionalDep
                                   {-# LINE 2076 "src-ag/Order.hs" #-}
                                   )
                              -- use rule "src-ag/Order.ag"(line 500, column 36)
                              _lhsOaranges =
                                  ({-# LINE 500 "src-ag/Order.ag" #-}
                                   _hdIaranges Seq.>< _tlIaranges
                                   {-# LINE 2082 "src-ag/Order.hs" #-}
                                   )
                              -- use rule "src-ag/Order.ag"(line 402, column 24)
                              _lhsOaroundDep =
                                  ({-# LINE 402 "src-ag/Order.ag" #-}
                                   _hdIaroundDep Seq.>< _tlIaroundDep
                                   {-# LINE 2088 "src-ag/Order.hs" #-}
                                   )
                              -- use rule "src-ag/Order.ag"(line 267, column 33)
                              _lhsOdirectDep =
                                  ({-# LINE 267 "src-ag/Order.ag" #-}
                                   _hdIdirectDep Seq.>< _tlIdirectDep
                                   {-# LINE 2094 "src-ag/Order.hs" #-}
                                   )
                              -- use rule "src-ag/Order.ag"(line 84, column 70)
                              _lhsOerrors =
                                  ({-# LINE 84 "src-ag/Order.ag" #-}
                                   _hdIerrors Seq.>< _tlIerrors
                                   {-# LINE 2100 "src-ag/Order.hs" #-}
                                   )
                              -- use rule "src-ag/DistChildAttr.ag"(line 4, column 53)
                              _lhsOinhMap' =
                                  ({-# LINE 4 "src-ag/DistChildAttr.ag" #-}
                                   _hdIinhMap' `Map.union` _tlIinhMap'
                                   {-# LINE 2106 "src-ag/Order.hs" #-}
                                   )
                              -- use rule "src-ag/Order.ag"(line 310, column 31)
                              _lhsOinstDep =
                                  ({-# LINE 310 "src-ag/Order.ag" #-}
                                   _hdIinstDep Seq.>< _tlIinstDep
                                   {-# LINE 2112 "src-ag/Order.hs" #-}
                                   )
                              -- use rule "src-ag/Order.ag"(line 365, column 18)
                              _lhsOmergeDep =
                                  ({-# LINE 365 "src-ag/Order.ag" #-}
                                   _hdImergeDep Seq.>< _tlImergeDep
                                   {-# LINE 2118 "src-ag/Order.hs" #-}
                                   )
                              -- use rule "src-ag/Order.ag"(line 61, column 105)
                              _lhsOnAutoRules =
                                  ({-# LINE 61 "src-ag/Order.ag" #-}
                                   _hdInAutoRules + _tlInAutoRules
                                   {-# LINE 2124 "src-ag/Order.hs" #-}
                                   )
                              -- use rule "src-ag/Order.ag"(line 61, column 105)
                              _lhsOnExplicitRules =
                                  ({-# LINE 61 "src-ag/Order.ag" #-}
                                   _hdInExplicitRules + _tlInExplicitRules
                                   {-# LINE 2130 "src-ag/Order.hs" #-}
                                   )
                              -- use rule "src-ag/Order.ag"(line 517, column 43)
                              _lhsOnonts =
                                  ({-# LINE 517 "src-ag/Order.ag" #-}
                                   _hdInonts ++ _tlInonts
                                   {-# LINE 2136 "src-ag/Order.hs" #-}
                                   )
                              -- use rule "src-ag/Order.ag"(line 499, column 35)
                              _lhsOntattrs =
                                  ({-# LINE 499 "src-ag/Order.ag" #-}
                                   _hdIntattrs Seq.>< _tlIntattrs
                                   {-# LINE 2142 "src-ag/Order.hs" #-}
                                   )
                              -- use rule "src-ag/Order.ag"(line 257, column 18)
                              _lhsOrules =
                                  ({-# LINE 257 "src-ag/Order.ag" #-}
                                   _hdIrules Seq.>< _tlIrules
                                   {-# LINE 2148 "src-ag/Order.hs" #-}
                                   )
                              -- use rule "src-ag/DistChildAttr.ag"(line 4, column 53)
                              _lhsOsynMap' =
                                  ({-# LINE 4 "src-ag/DistChildAttr.ag" #-}
                                   _hdIsynMap' `Map.union` _tlIsynMap'
                                   {-# LINE 2154 "src-ag/Order.hs" #-}
                                   )
                              -- copy rule (up)
                              _lhsOacount =
                                  ({-# LINE 499 "src-ag/Order.ag" #-}
                                   _tlIacount
                                   {-# LINE 2160 "src-ag/Order.hs" #-}
                                   )
                              -- copy rule (up)
                              _lhsOvcount =
                                  ({-# LINE 256 "src-ag/Order.ag" #-}
                                   _tlIvcount
                                   {-# LINE 2166 "src-ag/Order.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOacount =
                                  ({-# LINE 499 "src-ag/Order.ag" #-}
                                   _lhsIacount
                                   {-# LINE 2172 "src-ag/Order.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOallnts =
                                  ({-# LINE 647 "src-ag/Order.ag" #-}
                                   _lhsIallnts
                                   {-# LINE 2178 "src-ag/Order.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOaroundMap =
                                  ({-# LINE 405 "src-ag/Order.ag" #-}
                                   _lhsIaroundMap
                                   {-# LINE 2184 "src-ag/Order.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOcInterfaceMap =
                                  ({-# LINE 594 "src-ag/Order.ag" #-}
                                   _lhsIcInterfaceMap
                                   {-# LINE 2190 "src-ag/Order.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOcVisitsMap =
                                  ({-# LINE 601 "src-ag/Order.ag" #-}
                                   _lhsIcVisitsMap
                                   {-# LINE 2196 "src-ag/Order.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOinhMap =
                                  ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                   _lhsIinhMap
                                   {-# LINE 2202 "src-ag/Order.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOmanualAttrDepMap =
                                  ({-# LINE 281 "src-ag/Order.ag" #-}
                                   _lhsImanualAttrDepMap
                                   {-# LINE 2208 "src-ag/Order.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOmergeMap =
                                  ({-# LINE 352 "src-ag/Order.ag" #-}
                                   _lhsImergeMap
                                   {-# LINE 2214 "src-ag/Order.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOo_case =
                                  ({-# LINE 117 "src-ag/Order.ag" #-}
                                   _lhsIo_case
                                   {-# LINE 2220 "src-ag/Order.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOo_cata =
                                  ({-# LINE 111 "src-ag/Order.ag" #-}
                                   _lhsIo_cata
                                   {-# LINE 2226 "src-ag/Order.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOo_data =
                                  ({-# LINE 120 "src-ag/Order.ag" #-}
                                   _lhsIo_data
                                   {-# LINE 2232 "src-ag/Order.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOo_dovisit =
                                  ({-# LINE 116 "src-ag/Order.ag" #-}
                                   _lhsIo_dovisit
                                   {-# LINE 2238 "src-ag/Order.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOo_newtypes =
                                  ({-# LINE 110 "src-ag/Order.ag" #-}
                                   _lhsIo_newtypes
                                   {-# LINE 2244 "src-ag/Order.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOo_rename =
                                  ({-# LINE 114 "src-ag/Order.ag" #-}
                                   _lhsIo_rename
                                   {-# LINE 2250 "src-ag/Order.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOo_sem =
                                  ({-# LINE 113 "src-ag/Order.ag" #-}
                                   _lhsIo_sem
                                   {-# LINE 2256 "src-ag/Order.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOo_sig =
                                  ({-# LINE 112 "src-ag/Order.ag" #-}
                                   _lhsIo_sig
                                   {-# LINE 2262 "src-ag/Order.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOo_unbox =
                                  ({-# LINE 119 "src-ag/Order.ag" #-}
                                   _lhsIo_unbox
                                   {-# LINE 2268 "src-ag/Order.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOo_wantvisit =
                                  ({-# LINE 115 "src-ag/Order.ag" #-}
                                   _lhsIo_wantvisit
                                   {-# LINE 2274 "src-ag/Order.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOprefix =
                                  ({-# LINE 118 "src-ag/Order.ag" #-}
                                   _lhsIprefix
                                   {-# LINE 2280 "src-ag/Order.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOsynMap =
                                  ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                   _lhsIsynMap
                                   {-# LINE 2286 "src-ag/Order.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOvcount =
                                  ({-# LINE 256 "src-ag/Order.ag" #-}
                                   _lhsIvcount
                                   {-# LINE 2292 "src-ag/Order.hs" #-}
                                   )
                              -- copy rule (chain)
                              _tlOacount =
                                  ({-# LINE 499 "src-ag/Order.ag" #-}
                                   _hdIacount
                                   {-# LINE 2298 "src-ag/Order.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOallnts =
                                  ({-# LINE 647 "src-ag/Order.ag" #-}
                                   _lhsIallnts
                                   {-# LINE 2304 "src-ag/Order.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOaroundMap =
                                  ({-# LINE 405 "src-ag/Order.ag" #-}
                                   _lhsIaroundMap
                                   {-# LINE 2310 "src-ag/Order.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOcInterfaceMap =
                                  ({-# LINE 594 "src-ag/Order.ag" #-}
                                   _lhsIcInterfaceMap
                                   {-# LINE 2316 "src-ag/Order.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOcVisitsMap =
                                  ({-# LINE 601 "src-ag/Order.ag" #-}
                                   _lhsIcVisitsMap
                                   {-# LINE 2322 "src-ag/Order.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOinhMap =
                                  ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                   _lhsIinhMap
                                   {-# LINE 2328 "src-ag/Order.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOmanualAttrDepMap =
                                  ({-# LINE 281 "src-ag/Order.ag" #-}
                                   _lhsImanualAttrDepMap
                                   {-# LINE 2334 "src-ag/Order.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOmergeMap =
                                  ({-# LINE 352 "src-ag/Order.ag" #-}
                                   _lhsImergeMap
                                   {-# LINE 2340 "src-ag/Order.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOo_case =
                                  ({-# LINE 117 "src-ag/Order.ag" #-}
                                   _lhsIo_case
                                   {-# LINE 2346 "src-ag/Order.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOo_cata =
                                  ({-# LINE 111 "src-ag/Order.ag" #-}
                                   _lhsIo_cata
                                   {-# LINE 2352 "src-ag/Order.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOo_data =
                                  ({-# LINE 120 "src-ag/Order.ag" #-}
                                   _lhsIo_data
                                   {-# LINE 2358 "src-ag/Order.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOo_dovisit =
                                  ({-# LINE 116 "src-ag/Order.ag" #-}
                                   _lhsIo_dovisit
                                   {-# LINE 2364 "src-ag/Order.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOo_newtypes =
                                  ({-# LINE 110 "src-ag/Order.ag" #-}
                                   _lhsIo_newtypes
                                   {-# LINE 2370 "src-ag/Order.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOo_rename =
                                  ({-# LINE 114 "src-ag/Order.ag" #-}
                                   _lhsIo_rename
                                   {-# LINE 2376 "src-ag/Order.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOo_sem =
                                  ({-# LINE 113 "src-ag/Order.ag" #-}
                                   _lhsIo_sem
                                   {-# LINE 2382 "src-ag/Order.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOo_sig =
                                  ({-# LINE 112 "src-ag/Order.ag" #-}
                                   _lhsIo_sig
                                   {-# LINE 2388 "src-ag/Order.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOo_unbox =
                                  ({-# LINE 119 "src-ag/Order.ag" #-}
                                   _lhsIo_unbox
                                   {-# LINE 2394 "src-ag/Order.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOo_wantvisit =
                                  ({-# LINE 115 "src-ag/Order.ag" #-}
                                   _lhsIo_wantvisit
                                   {-# LINE 2400 "src-ag/Order.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOprefix =
                                  ({-# LINE 118 "src-ag/Order.ag" #-}
                                   _lhsIprefix
                                   {-# LINE 2406 "src-ag/Order.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOsynMap =
                                  ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                   _lhsIsynMap
                                   {-# LINE 2412 "src-ag/Order.hs" #-}
                                   )
                              -- copy rule (chain)
                              _tlOvcount =
                                  ({-# LINE 256 "src-ag/Order.ag" #-}
                                   _hdIvcount
                                   {-# LINE 2418 "src-ag/Order.hs" #-}
                                   )
                              ( _hdIacount,_hdIadditionalDep,_hdIaranges,_hdIaroundDep,_hdIcNonterminal,_hdIdirectDep,_hdIerrors,_hdIinhMap',_hdIinstDep,_hdImergeDep,_hdInAutoRules,_hdInExplicitRules,_hdInonts,_hdIntattrs,_hdIrules,_hdIsynMap',_hdIvcount) =
                                  hd_ _hdOacount _hdOallnts _hdOaroundMap _hdOcInterfaceMap _hdOcVisitsMap _hdOinhMap _hdOmanualAttrDepMap _hdOmergeMap _hdOo_case _hdOo_cata _hdOo_data _hdOo_dovisit _hdOo_newtypes _hdOo_rename _hdOo_sem _hdOo_sig _hdOo_unbox _hdOo_wantvisit _hdOprefix _hdOsynMap _hdOvcount 
                              ( _tlIacount,_tlIadditionalDep,_tlIaranges,_tlIaroundDep,_tlIcNonterminals,_tlIdirectDep,_tlIerrors,_tlIinhMap',_tlIinstDep,_tlImergeDep,_tlInAutoRules,_tlInExplicitRules,_tlInonts,_tlIntattrs,_tlIrules,_tlIsynMap',_tlIvcount) =
                                  tl_ _tlOacount _tlOallnts _tlOaroundMap _tlOcInterfaceMap _tlOcVisitsMap _tlOinhMap _tlOmanualAttrDepMap _tlOmergeMap _tlOo_case _tlOo_cata _tlOo_data _tlOo_dovisit _tlOo_newtypes _tlOo_rename _tlOo_sem _tlOo_sig _tlOo_unbox _tlOo_wantvisit _tlOprefix _tlOsynMap _tlOvcount 
                          in  ( _lhsOacount,_lhsOadditionalDep,_lhsOaranges,_lhsOaroundDep,_lhsOcNonterminals,_lhsOdirectDep,_lhsOerrors,_lhsOinhMap',_lhsOinstDep,_lhsOmergeDep,_lhsOnAutoRules,_lhsOnExplicitRules,_lhsOnonts,_lhsOntattrs,_lhsOrules,_lhsOsynMap',_lhsOvcount))) )
sem_Nonterminals_Nil :: T_Nonterminals 
sem_Nonterminals_Nil  =
    (T_Nonterminals (\ _lhsIacount
                       _lhsIallnts
                       _lhsIaroundMap
                       _lhsIcInterfaceMap
                       _lhsIcVisitsMap
                       _lhsIinhMap
                       _lhsImanualAttrDepMap
                       _lhsImergeMap
                       _lhsIo_case
                       _lhsIo_cata
                       _lhsIo_data
                       _lhsIo_dovisit
                       _lhsIo_newtypes
                       _lhsIo_rename
                       _lhsIo_sem
                       _lhsIo_sig
                       _lhsIo_unbox
                       _lhsIo_wantvisit
                       _lhsIprefix
                       _lhsIsynMap
                       _lhsIvcount ->
                         (let _lhsOcNonterminals :: CNonterminals
                              _lhsOadditionalDep :: (Seq Edge)
                              _lhsOaranges :: (Seq (Int,Int,Int))
                              _lhsOaroundDep :: (Seq Edge)
                              _lhsOdirectDep :: (Seq Edge)
                              _lhsOerrors :: (Seq Error)
                              _lhsOinhMap' :: (Map Identifier Attributes)
                              _lhsOinstDep :: (Seq Edge)
                              _lhsOmergeDep :: (Seq Edge)
                              _lhsOnAutoRules :: Int
                              _lhsOnExplicitRules :: Int
                              _lhsOnonts :: ([(NontermIdent,[ConstructorIdent])])
                              _lhsOntattrs :: (Seq (Vertex,NTAttr))
                              _lhsOrules :: (Seq (Vertex,CRule))
                              _lhsOsynMap' :: (Map Identifier Attributes)
                              _lhsOacount :: Int
                              _lhsOvcount :: Int
                              -- "src-ag/Order.ag"(line 622, column 12)
                              _lhsOcNonterminals =
                                  ({-# LINE 622 "src-ag/Order.ag" #-}
                                   []
                                   {-# LINE 2469 "src-ag/Order.hs" #-}
                                   )
                              -- use rule "src-ag/Order.ag"(line 281, column 60)
                              _lhsOadditionalDep =
                                  ({-# LINE 281 "src-ag/Order.ag" #-}
                                   Seq.empty
                                   {-# LINE 2475 "src-ag/Order.hs" #-}
                                   )
                              -- use rule "src-ag/Order.ag"(line 500, column 36)
                              _lhsOaranges =
                                  ({-# LINE 500 "src-ag/Order.ag" #-}
                                   Seq.empty
                                   {-# LINE 2481 "src-ag/Order.hs" #-}
                                   )
                              -- use rule "src-ag/Order.ag"(line 402, column 24)
                              _lhsOaroundDep =
                                  ({-# LINE 402 "src-ag/Order.ag" #-}
                                   Seq.empty
                                   {-# LINE 2487 "src-ag/Order.hs" #-}
                                   )
                              -- use rule "src-ag/Order.ag"(line 267, column 33)
                              _lhsOdirectDep =
                                  ({-# LINE 267 "src-ag/Order.ag" #-}
                                   Seq.empty
                                   {-# LINE 2493 "src-ag/Order.hs" #-}
                                   )
                              -- use rule "src-ag/Order.ag"(line 84, column 70)
                              _lhsOerrors =
                                  ({-# LINE 84 "src-ag/Order.ag" #-}
                                   Seq.empty
                                   {-# LINE 2499 "src-ag/Order.hs" #-}
                                   )
                              -- use rule "src-ag/DistChildAttr.ag"(line 4, column 53)
                              _lhsOinhMap' =
                                  ({-# LINE 4 "src-ag/DistChildAttr.ag" #-}
                                   Map.empty
                                   {-# LINE 2505 "src-ag/Order.hs" #-}
                                   )
                              -- use rule "src-ag/Order.ag"(line 310, column 31)
                              _lhsOinstDep =
                                  ({-# LINE 310 "src-ag/Order.ag" #-}
                                   Seq.empty
                                   {-# LINE 2511 "src-ag/Order.hs" #-}
                                   )
                              -- use rule "src-ag/Order.ag"(line 365, column 18)
                              _lhsOmergeDep =
                                  ({-# LINE 365 "src-ag/Order.ag" #-}
                                   Seq.empty
                                   {-# LINE 2517 "src-ag/Order.hs" #-}
                                   )
                              -- use rule "src-ag/Order.ag"(line 61, column 105)
                              _lhsOnAutoRules =
                                  ({-# LINE 61 "src-ag/Order.ag" #-}
                                   0
                                   {-# LINE 2523 "src-ag/Order.hs" #-}
                                   )
                              -- use rule "src-ag/Order.ag"(line 61, column 105)
                              _lhsOnExplicitRules =
                                  ({-# LINE 61 "src-ag/Order.ag" #-}
                                   0
                                   {-# LINE 2529 "src-ag/Order.hs" #-}
                                   )
                              -- use rule "src-ag/Order.ag"(line 517, column 43)
                              _lhsOnonts =
                                  ({-# LINE 517 "src-ag/Order.ag" #-}
                                   []
                                   {-# LINE 2535 "src-ag/Order.hs" #-}
                                   )
                              -- use rule "src-ag/Order.ag"(line 499, column 35)
                              _lhsOntattrs =
                                  ({-# LINE 499 "src-ag/Order.ag" #-}
                                   Seq.empty
                                   {-# LINE 2541 "src-ag/Order.hs" #-}
                                   )
                              -- use rule "src-ag/Order.ag"(line 257, column 18)
                              _lhsOrules =
                                  ({-# LINE 257 "src-ag/Order.ag" #-}
                                   Seq.empty
                                   {-# LINE 2547 "src-ag/Order.hs" #-}
                                   )
                              -- use rule "src-ag/DistChildAttr.ag"(line 4, column 53)
                              _lhsOsynMap' =
                                  ({-# LINE 4 "src-ag/DistChildAttr.ag" #-}
                                   Map.empty
                                   {-# LINE 2553 "src-ag/Order.hs" #-}
                                   )
                              -- copy rule (chain)
                              _lhsOacount =
                                  ({-# LINE 499 "src-ag/Order.ag" #-}
                                   _lhsIacount
                                   {-# LINE 2559 "src-ag/Order.hs" #-}
                                   )
                              -- copy rule (chain)
                              _lhsOvcount =
                                  ({-# LINE 256 "src-ag/Order.ag" #-}
                                   _lhsIvcount
                                   {-# LINE 2565 "src-ag/Order.hs" #-}
                                   )
                          in  ( _lhsOacount,_lhsOadditionalDep,_lhsOaranges,_lhsOaroundDep,_lhsOcNonterminals,_lhsOdirectDep,_lhsOerrors,_lhsOinhMap',_lhsOinstDep,_lhsOmergeDep,_lhsOnAutoRules,_lhsOnExplicitRules,_lhsOnonts,_lhsOntattrs,_lhsOrules,_lhsOsynMap',_lhsOvcount))) )
-- Pattern -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allTypeSigs          : Map Identifier Type
         altAttrs             : Map AltAttr Vertex
         con                  : Identifier
         inh                  : Attributes
         nt                   : Identifier
         syn                  : Attributes
      synthesized attributes:
         copy                 : SELF 
         errors               : Seq Error
         gathAltAttrs         : [AltAttr]
         instVars             : [Identifier]
         locVars              : [Identifier]
         patternAttrs         : [(Identifier,Identifier,Bool)]
   alternatives:
      alternative Alias:
         child field          : {Identifier}
         child attr           : {Identifier}
         child pat            : Pattern 
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
newtype T_Pattern  = T_Pattern ((Map Identifier Type) ->
                                (Map AltAttr Vertex) ->
                                Identifier ->
                                Attributes ->
                                Identifier ->
                                Attributes ->
                                ( Pattern ,(Seq Error),([AltAttr]),([Identifier]),([Identifier]),([(Identifier,Identifier,Bool)])))
data Inh_Pattern  = Inh_Pattern {allTypeSigs_Inh_Pattern :: !((Map Identifier Type)),altAttrs_Inh_Pattern :: !((Map AltAttr Vertex)),con_Inh_Pattern :: !(Identifier),inh_Inh_Pattern :: !(Attributes),nt_Inh_Pattern :: !(Identifier),syn_Inh_Pattern :: !(Attributes)}
data Syn_Pattern  = Syn_Pattern {copy_Syn_Pattern :: !(Pattern ),errors_Syn_Pattern :: !((Seq Error)),gathAltAttrs_Syn_Pattern :: !(([AltAttr])),instVars_Syn_Pattern :: !(([Identifier])),locVars_Syn_Pattern :: !(([Identifier])),patternAttrs_Syn_Pattern :: !(([(Identifier,Identifier,Bool)]))}
wrap_Pattern :: T_Pattern  ->
                Inh_Pattern  ->
                Syn_Pattern 
wrap_Pattern (T_Pattern sem ) (Inh_Pattern _lhsIallTypeSigs _lhsIaltAttrs _lhsIcon _lhsIinh _lhsInt _lhsIsyn )  =
    (let ( _lhsOcopy,_lhsOerrors,_lhsOgathAltAttrs,_lhsOinstVars,_lhsOlocVars,_lhsOpatternAttrs) = sem _lhsIallTypeSigs _lhsIaltAttrs _lhsIcon _lhsIinh _lhsInt _lhsIsyn 
     in  (Syn_Pattern _lhsOcopy _lhsOerrors _lhsOgathAltAttrs _lhsOinstVars _lhsOlocVars _lhsOpatternAttrs ))
sem_Pattern_Alias :: Identifier ->
                     Identifier ->
                     T_Pattern  ->
                     T_Pattern 
sem_Pattern_Alias field_ attr_ (T_Pattern pat_ )  =
    (T_Pattern (\ _lhsIallTypeSigs
                  _lhsIaltAttrs
                  _lhsIcon
                  _lhsIinh
                  _lhsInt
                  _lhsIsyn ->
                    (let _lhsOgathAltAttrs :: ([AltAttr])
                         _lhsOpatternAttrs :: ([(Identifier,Identifier,Bool)])
                         _lhsOlocVars :: ([Identifier])
                         _lhsOinstVars :: ([Identifier])
                         _lhsOerrors :: (Seq Error)
                         _lhsOcopy :: Pattern 
                         _patOallTypeSigs :: (Map Identifier Type)
                         _patOaltAttrs :: (Map AltAttr Vertex)
                         _patOcon :: Identifier
                         _patOinh :: Attributes
                         _patOnt :: Identifier
                         _patOsyn :: Attributes
                         _patIcopy :: Pattern 
                         _patIerrors :: (Seq Error)
                         _patIgathAltAttrs :: ([AltAttr])
                         _patIinstVars :: ([Identifier])
                         _patIlocVars :: ([Identifier])
                         _patIpatternAttrs :: ([(Identifier,Identifier,Bool)])
                         -- "src-ag/Order.ag"(line 184, column 12)
                         _lhsOgathAltAttrs =
                             ({-# LINE 184 "src-ag/Order.ag" #-}
                              [AltAttr field_ attr_ (field_ == _LOC || field_ == _INST)]
                              {-# LINE 2673 "src-ag/Order.hs" #-}
                              )
                         -- "src-ag/Order.ag"(line 250, column 12)
                         _lhsOpatternAttrs =
                             ({-# LINE 250 "src-ag/Order.ag" #-}
                              [(field_,attr_,(field_ == _LOC || field_ == _INST))]
                              {-# LINE 2679 "src-ag/Order.hs" #-}
                              )
                         -- "src-ag/Order.ag"(line 680, column 14)
                         _lhsOlocVars =
                             ({-# LINE 680 "src-ag/Order.ag" #-}
                              if field_ == _LOC
                                 then [attr_]
                                 else []
                              {-# LINE 2687 "src-ag/Order.hs" #-}
                              )
                         -- "src-ag/Order.ag"(line 683, column 14)
                         _lhsOinstVars =
                             ({-# LINE 683 "src-ag/Order.ag" #-}
                              if field_ == _INST
                                 then [attr_]
                                 else []
                              {-# LINE 2695 "src-ag/Order.hs" #-}
                              )
                         -- use rule "src-ag/Order.ag"(line 84, column 70)
                         _lhsOerrors =
                             ({-# LINE 84 "src-ag/Order.ag" #-}
                              _patIerrors
                              {-# LINE 2701 "src-ag/Order.hs" #-}
                              )
                         -- self rule
                         _copy =
                             ({-# LINE 22 "src-ag/Patterns.ag" #-}
                              Alias field_ attr_ _patIcopy
                              {-# LINE 2707 "src-ag/Order.hs" #-}
                              )
                         -- self rule
                         _lhsOcopy =
                             ({-# LINE 22 "src-ag/Patterns.ag" #-}
                              _copy
                              {-# LINE 2713 "src-ag/Order.hs" #-}
                              )
                         -- copy rule (down)
                         _patOallTypeSigs =
                             ({-# LINE 533 "src-ag/Order.ag" #-}
                              _lhsIallTypeSigs
                              {-# LINE 2719 "src-ag/Order.hs" #-}
                              )
                         -- copy rule (down)
                         _patOaltAttrs =
                             ({-# LINE 186 "src-ag/Order.ag" #-}
                              _lhsIaltAttrs
                              {-# LINE 2725 "src-ag/Order.hs" #-}
                              )
                         -- copy rule (down)
                         _patOcon =
                             ({-# LINE 90 "src-ag/Order.ag" #-}
                              _lhsIcon
                              {-# LINE 2731 "src-ag/Order.hs" #-}
                              )
                         -- copy rule (down)
                         _patOinh =
                             ({-# LINE 89 "src-ag/Order.ag" #-}
                              _lhsIinh
                              {-# LINE 2737 "src-ag/Order.hs" #-}
                              )
                         -- copy rule (down)
                         _patOnt =
                             ({-# LINE 89 "src-ag/Order.ag" #-}
                              _lhsInt
                              {-# LINE 2743 "src-ag/Order.hs" #-}
                              )
                         -- copy rule (down)
                         _patOsyn =
                             ({-# LINE 89 "src-ag/Order.ag" #-}
                              _lhsIsyn
                              {-# LINE 2749 "src-ag/Order.hs" #-}
                              )
                         ( _patIcopy,_patIerrors,_patIgathAltAttrs,_patIinstVars,_patIlocVars,_patIpatternAttrs) =
                             pat_ _patOallTypeSigs _patOaltAttrs _patOcon _patOinh _patOnt _patOsyn 
                     in  ( _lhsOcopy,_lhsOerrors,_lhsOgathAltAttrs,_lhsOinstVars,_lhsOlocVars,_lhsOpatternAttrs))) )
sem_Pattern_Constr :: ConstructorIdent ->
                      T_Patterns  ->
                      T_Pattern 
sem_Pattern_Constr name_ (T_Patterns pats_ )  =
    (T_Pattern (\ _lhsIallTypeSigs
                  _lhsIaltAttrs
                  _lhsIcon
                  _lhsIinh
                  _lhsInt
                  _lhsIsyn ->
                    (let _lhsOerrors :: (Seq Error)
                         _lhsOgathAltAttrs :: ([AltAttr])
                         _lhsOinstVars :: ([Identifier])
                         _lhsOlocVars :: ([Identifier])
                         _lhsOpatternAttrs :: ([(Identifier,Identifier,Bool)])
                         _lhsOcopy :: Pattern 
                         _patsOallTypeSigs :: (Map Identifier Type)
                         _patsOaltAttrs :: (Map AltAttr Vertex)
                         _patsOcon :: Identifier
                         _patsOinh :: Attributes
                         _patsOnt :: Identifier
                         _patsOsyn :: Attributes
                         _patsIcopy :: Patterns 
                         _patsIerrors :: (Seq Error)
                         _patsIgathAltAttrs :: ([AltAttr])
                         _patsIinstVars :: ([Identifier])
                         _patsIlocVars :: ([Identifier])
                         _patsIpatternAttrs :: ([(Identifier,Identifier,Bool)])
                         -- use rule "src-ag/Order.ag"(line 84, column 70)
                         _lhsOerrors =
                             ({-# LINE 84 "src-ag/Order.ag" #-}
                              _patsIerrors
                              {-# LINE 2786 "src-ag/Order.hs" #-}
                              )
                         -- use rule "src-ag/Order.ag"(line 170, column 68)
                         _lhsOgathAltAttrs =
                             ({-# LINE 170 "src-ag/Order.ag" #-}
                              _patsIgathAltAttrs
                              {-# LINE 2792 "src-ag/Order.hs" #-}
                              )
                         -- use rule "src-ag/Order.ag"(line 677, column 86)
                         _lhsOinstVars =
                             ({-# LINE 677 "src-ag/Order.ag" #-}
                              _patsIinstVars
                              {-# LINE 2798 "src-ag/Order.hs" #-}
                              )
                         -- use rule "src-ag/Order.ag"(line 677, column 48)
                         _lhsOlocVars =
                             ({-# LINE 677 "src-ag/Order.ag" #-}
                              _patsIlocVars
                              {-# LINE 2804 "src-ag/Order.hs" #-}
                              )
                         -- use rule "src-ag/Order.ag"(line 247, column 42)
                         _lhsOpatternAttrs =
                             ({-# LINE 247 "src-ag/Order.ag" #-}
                              _patsIpatternAttrs
                              {-# LINE 2810 "src-ag/Order.hs" #-}
                              )
                         -- self rule
                         _copy =
                             ({-# LINE 22 "src-ag/Patterns.ag" #-}
                              Constr name_ _patsIcopy
                              {-# LINE 2816 "src-ag/Order.hs" #-}
                              )
                         -- self rule
                         _lhsOcopy =
                             ({-# LINE 22 "src-ag/Patterns.ag" #-}
                              _copy
                              {-# LINE 2822 "src-ag/Order.hs" #-}
                              )
                         -- copy rule (down)
                         _patsOallTypeSigs =
                             ({-# LINE 533 "src-ag/Order.ag" #-}
                              _lhsIallTypeSigs
                              {-# LINE 2828 "src-ag/Order.hs" #-}
                              )
                         -- copy rule (down)
                         _patsOaltAttrs =
                             ({-# LINE 186 "src-ag/Order.ag" #-}
                              _lhsIaltAttrs
                              {-# LINE 2834 "src-ag/Order.hs" #-}
                              )
                         -- copy rule (down)
                         _patsOcon =
                             ({-# LINE 90 "src-ag/Order.ag" #-}
                              _lhsIcon
                              {-# LINE 2840 "src-ag/Order.hs" #-}
                              )
                         -- copy rule (down)
                         _patsOinh =
                             ({-# LINE 89 "src-ag/Order.ag" #-}
                              _lhsIinh
                              {-# LINE 2846 "src-ag/Order.hs" #-}
                              )
                         -- copy rule (down)
                         _patsOnt =
                             ({-# LINE 89 "src-ag/Order.ag" #-}
                              _lhsInt
                              {-# LINE 2852 "src-ag/Order.hs" #-}
                              )
                         -- copy rule (down)
                         _patsOsyn =
                             ({-# LINE 89 "src-ag/Order.ag" #-}
                              _lhsIsyn
                              {-# LINE 2858 "src-ag/Order.hs" #-}
                              )
                         ( _patsIcopy,_patsIerrors,_patsIgathAltAttrs,_patsIinstVars,_patsIlocVars,_patsIpatternAttrs) =
                             pats_ _patsOallTypeSigs _patsOaltAttrs _patsOcon _patsOinh _patsOnt _patsOsyn 
                     in  ( _lhsOcopy,_lhsOerrors,_lhsOgathAltAttrs,_lhsOinstVars,_lhsOlocVars,_lhsOpatternAttrs))) )
sem_Pattern_Irrefutable :: T_Pattern  ->
                           T_Pattern 
sem_Pattern_Irrefutable (T_Pattern pat_ )  =
    (T_Pattern (\ _lhsIallTypeSigs
                  _lhsIaltAttrs
                  _lhsIcon
                  _lhsIinh
                  _lhsInt
                  _lhsIsyn ->
                    (let _lhsOerrors :: (Seq Error)
                         _lhsOgathAltAttrs :: ([AltAttr])
                         _lhsOinstVars :: ([Identifier])
                         _lhsOlocVars :: ([Identifier])
                         _lhsOpatternAttrs :: ([(Identifier,Identifier,Bool)])
                         _lhsOcopy :: Pattern 
                         _patOallTypeSigs :: (Map Identifier Type)
                         _patOaltAttrs :: (Map AltAttr Vertex)
                         _patOcon :: Identifier
                         _patOinh :: Attributes
                         _patOnt :: Identifier
                         _patOsyn :: Attributes
                         _patIcopy :: Pattern 
                         _patIerrors :: (Seq Error)
                         _patIgathAltAttrs :: ([AltAttr])
                         _patIinstVars :: ([Identifier])
                         _patIlocVars :: ([Identifier])
                         _patIpatternAttrs :: ([(Identifier,Identifier,Bool)])
                         -- use rule "src-ag/Order.ag"(line 84, column 70)
                         _lhsOerrors =
                             ({-# LINE 84 "src-ag/Order.ag" #-}
                              _patIerrors
                              {-# LINE 2894 "src-ag/Order.hs" #-}
                              )
                         -- use rule "src-ag/Order.ag"(line 170, column 68)
                         _lhsOgathAltAttrs =
                             ({-# LINE 170 "src-ag/Order.ag" #-}
                              _patIgathAltAttrs
                              {-# LINE 2900 "src-ag/Order.hs" #-}
                              )
                         -- use rule "src-ag/Order.ag"(line 677, column 86)
                         _lhsOinstVars =
                             ({-# LINE 677 "src-ag/Order.ag" #-}
                              _patIinstVars
                              {-# LINE 2906 "src-ag/Order.hs" #-}
                              )
                         -- use rule "src-ag/Order.ag"(line 677, column 48)
                         _lhsOlocVars =
                             ({-# LINE 677 "src-ag/Order.ag" #-}
                              _patIlocVars
                              {-# LINE 2912 "src-ag/Order.hs" #-}
                              )
                         -- use rule "src-ag/Order.ag"(line 247, column 42)
                         _lhsOpatternAttrs =
                             ({-# LINE 247 "src-ag/Order.ag" #-}
                              _patIpatternAttrs
                              {-# LINE 2918 "src-ag/Order.hs" #-}
                              )
                         -- self rule
                         _copy =
                             ({-# LINE 22 "src-ag/Patterns.ag" #-}
                              Irrefutable _patIcopy
                              {-# LINE 2924 "src-ag/Order.hs" #-}
                              )
                         -- self rule
                         _lhsOcopy =
                             ({-# LINE 22 "src-ag/Patterns.ag" #-}
                              _copy
                              {-# LINE 2930 "src-ag/Order.hs" #-}
                              )
                         -- copy rule (down)
                         _patOallTypeSigs =
                             ({-# LINE 533 "src-ag/Order.ag" #-}
                              _lhsIallTypeSigs
                              {-# LINE 2936 "src-ag/Order.hs" #-}
                              )
                         -- copy rule (down)
                         _patOaltAttrs =
                             ({-# LINE 186 "src-ag/Order.ag" #-}
                              _lhsIaltAttrs
                              {-# LINE 2942 "src-ag/Order.hs" #-}
                              )
                         -- copy rule (down)
                         _patOcon =
                             ({-# LINE 90 "src-ag/Order.ag" #-}
                              _lhsIcon
                              {-# LINE 2948 "src-ag/Order.hs" #-}
                              )
                         -- copy rule (down)
                         _patOinh =
                             ({-# LINE 89 "src-ag/Order.ag" #-}
                              _lhsIinh
                              {-# LINE 2954 "src-ag/Order.hs" #-}
                              )
                         -- copy rule (down)
                         _patOnt =
                             ({-# LINE 89 "src-ag/Order.ag" #-}
                              _lhsInt
                              {-# LINE 2960 "src-ag/Order.hs" #-}
                              )
                         -- copy rule (down)
                         _patOsyn =
                             ({-# LINE 89 "src-ag/Order.ag" #-}
                              _lhsIsyn
                              {-# LINE 2966 "src-ag/Order.hs" #-}
                              )
                         ( _patIcopy,_patIerrors,_patIgathAltAttrs,_patIinstVars,_patIlocVars,_patIpatternAttrs) =
                             pat_ _patOallTypeSigs _patOaltAttrs _patOcon _patOinh _patOnt _patOsyn 
                     in  ( _lhsOcopy,_lhsOerrors,_lhsOgathAltAttrs,_lhsOinstVars,_lhsOlocVars,_lhsOpatternAttrs))) )
sem_Pattern_Product :: Pos ->
                       T_Patterns  ->
                       T_Pattern 
sem_Pattern_Product pos_ (T_Patterns pats_ )  =
    (T_Pattern (\ _lhsIallTypeSigs
                  _lhsIaltAttrs
                  _lhsIcon
                  _lhsIinh
                  _lhsInt
                  _lhsIsyn ->
                    (let _lhsOerrors :: (Seq Error)
                         _lhsOgathAltAttrs :: ([AltAttr])
                         _lhsOinstVars :: ([Identifier])
                         _lhsOlocVars :: ([Identifier])
                         _lhsOpatternAttrs :: ([(Identifier,Identifier,Bool)])
                         _lhsOcopy :: Pattern 
                         _patsOallTypeSigs :: (Map Identifier Type)
                         _patsOaltAttrs :: (Map AltAttr Vertex)
                         _patsOcon :: Identifier
                         _patsOinh :: Attributes
                         _patsOnt :: Identifier
                         _patsOsyn :: Attributes
                         _patsIcopy :: Patterns 
                         _patsIerrors :: (Seq Error)
                         _patsIgathAltAttrs :: ([AltAttr])
                         _patsIinstVars :: ([Identifier])
                         _patsIlocVars :: ([Identifier])
                         _patsIpatternAttrs :: ([(Identifier,Identifier,Bool)])
                         -- use rule "src-ag/Order.ag"(line 84, column 70)
                         _lhsOerrors =
                             ({-# LINE 84 "src-ag/Order.ag" #-}
                              _patsIerrors
                              {-# LINE 3003 "src-ag/Order.hs" #-}
                              )
                         -- use rule "src-ag/Order.ag"(line 170, column 68)
                         _lhsOgathAltAttrs =
                             ({-# LINE 170 "src-ag/Order.ag" #-}
                              _patsIgathAltAttrs
                              {-# LINE 3009 "src-ag/Order.hs" #-}
                              )
                         -- use rule "src-ag/Order.ag"(line 677, column 86)
                         _lhsOinstVars =
                             ({-# LINE 677 "src-ag/Order.ag" #-}
                              _patsIinstVars
                              {-# LINE 3015 "src-ag/Order.hs" #-}
                              )
                         -- use rule "src-ag/Order.ag"(line 677, column 48)
                         _lhsOlocVars =
                             ({-# LINE 677 "src-ag/Order.ag" #-}
                              _patsIlocVars
                              {-# LINE 3021 "src-ag/Order.hs" #-}
                              )
                         -- use rule "src-ag/Order.ag"(line 247, column 42)
                         _lhsOpatternAttrs =
                             ({-# LINE 247 "src-ag/Order.ag" #-}
                              _patsIpatternAttrs
                              {-# LINE 3027 "src-ag/Order.hs" #-}
                              )
                         -- self rule
                         _copy =
                             ({-# LINE 22 "src-ag/Patterns.ag" #-}
                              Product pos_ _patsIcopy
                              {-# LINE 3033 "src-ag/Order.hs" #-}
                              )
                         -- self rule
                         _lhsOcopy =
                             ({-# LINE 22 "src-ag/Patterns.ag" #-}
                              _copy
                              {-# LINE 3039 "src-ag/Order.hs" #-}
                              )
                         -- copy rule (down)
                         _patsOallTypeSigs =
                             ({-# LINE 533 "src-ag/Order.ag" #-}
                              _lhsIallTypeSigs
                              {-# LINE 3045 "src-ag/Order.hs" #-}
                              )
                         -- copy rule (down)
                         _patsOaltAttrs =
                             ({-# LINE 186 "src-ag/Order.ag" #-}
                              _lhsIaltAttrs
                              {-# LINE 3051 "src-ag/Order.hs" #-}
                              )
                         -- copy rule (down)
                         _patsOcon =
                             ({-# LINE 90 "src-ag/Order.ag" #-}
                              _lhsIcon
                              {-# LINE 3057 "src-ag/Order.hs" #-}
                              )
                         -- copy rule (down)
                         _patsOinh =
                             ({-# LINE 89 "src-ag/Order.ag" #-}
                              _lhsIinh
                              {-# LINE 3063 "src-ag/Order.hs" #-}
                              )
                         -- copy rule (down)
                         _patsOnt =
                             ({-# LINE 89 "src-ag/Order.ag" #-}
                              _lhsInt
                              {-# LINE 3069 "src-ag/Order.hs" #-}
                              )
                         -- copy rule (down)
                         _patsOsyn =
                             ({-# LINE 89 "src-ag/Order.ag" #-}
                              _lhsIsyn
                              {-# LINE 3075 "src-ag/Order.hs" #-}
                              )
                         ( _patsIcopy,_patsIerrors,_patsIgathAltAttrs,_patsIinstVars,_patsIlocVars,_patsIpatternAttrs) =
                             pats_ _patsOallTypeSigs _patsOaltAttrs _patsOcon _patsOinh _patsOnt _patsOsyn 
                     in  ( _lhsOcopy,_lhsOerrors,_lhsOgathAltAttrs,_lhsOinstVars,_lhsOlocVars,_lhsOpatternAttrs))) )
sem_Pattern_Underscore :: Pos ->
                          T_Pattern 
sem_Pattern_Underscore pos_  =
    (T_Pattern (\ _lhsIallTypeSigs
                  _lhsIaltAttrs
                  _lhsIcon
                  _lhsIinh
                  _lhsInt
                  _lhsIsyn ->
                    (let _lhsOerrors :: (Seq Error)
                         _lhsOgathAltAttrs :: ([AltAttr])
                         _lhsOinstVars :: ([Identifier])
                         _lhsOlocVars :: ([Identifier])
                         _lhsOpatternAttrs :: ([(Identifier,Identifier,Bool)])
                         _lhsOcopy :: Pattern 
                         -- use rule "src-ag/Order.ag"(line 84, column 70)
                         _lhsOerrors =
                             ({-# LINE 84 "src-ag/Order.ag" #-}
                              Seq.empty
                              {-# LINE 3099 "src-ag/Order.hs" #-}
                              )
                         -- use rule "src-ag/Order.ag"(line 170, column 68)
                         _lhsOgathAltAttrs =
                             ({-# LINE 170 "src-ag/Order.ag" #-}
                              []
                              {-# LINE 3105 "src-ag/Order.hs" #-}
                              )
                         -- use rule "src-ag/Order.ag"(line 677, column 86)
                         _lhsOinstVars =
                             ({-# LINE 677 "src-ag/Order.ag" #-}
                              []
                              {-# LINE 3111 "src-ag/Order.hs" #-}
                              )
                         -- use rule "src-ag/Order.ag"(line 677, column 48)
                         _lhsOlocVars =
                             ({-# LINE 677 "src-ag/Order.ag" #-}
                              []
                              {-# LINE 3117 "src-ag/Order.hs" #-}
                              )
                         -- use rule "src-ag/Order.ag"(line 247, column 42)
                         _lhsOpatternAttrs =
                             ({-# LINE 247 "src-ag/Order.ag" #-}
                              []
                              {-# LINE 3123 "src-ag/Order.hs" #-}
                              )
                         -- self rule
                         _copy =
                             ({-# LINE 22 "src-ag/Patterns.ag" #-}
                              Underscore pos_
                              {-# LINE 3129 "src-ag/Order.hs" #-}
                              )
                         -- self rule
                         _lhsOcopy =
                             ({-# LINE 22 "src-ag/Patterns.ag" #-}
                              _copy
                              {-# LINE 3135 "src-ag/Order.hs" #-}
                              )
                     in  ( _lhsOcopy,_lhsOerrors,_lhsOgathAltAttrs,_lhsOinstVars,_lhsOlocVars,_lhsOpatternAttrs))) )
-- Patterns ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allTypeSigs          : Map Identifier Type
         altAttrs             : Map AltAttr Vertex
         con                  : Identifier
         inh                  : Attributes
         nt                   : Identifier
         syn                  : Attributes
      synthesized attributes:
         copy                 : SELF 
         errors               : Seq Error
         gathAltAttrs         : [AltAttr]
         instVars             : [Identifier]
         locVars              : [Identifier]
         patternAttrs         : [(Identifier,Identifier,Bool)]
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
newtype T_Patterns  = T_Patterns ((Map Identifier Type) ->
                                  (Map AltAttr Vertex) ->
                                  Identifier ->
                                  Attributes ->
                                  Identifier ->
                                  Attributes ->
                                  ( Patterns ,(Seq Error),([AltAttr]),([Identifier]),([Identifier]),([(Identifier,Identifier,Bool)])))
data Inh_Patterns  = Inh_Patterns {allTypeSigs_Inh_Patterns :: !((Map Identifier Type)),altAttrs_Inh_Patterns :: !((Map AltAttr Vertex)),con_Inh_Patterns :: !(Identifier),inh_Inh_Patterns :: !(Attributes),nt_Inh_Patterns :: !(Identifier),syn_Inh_Patterns :: !(Attributes)}
data Syn_Patterns  = Syn_Patterns {copy_Syn_Patterns :: !(Patterns ),errors_Syn_Patterns :: !((Seq Error)),gathAltAttrs_Syn_Patterns :: !(([AltAttr])),instVars_Syn_Patterns :: !(([Identifier])),locVars_Syn_Patterns :: !(([Identifier])),patternAttrs_Syn_Patterns :: !(([(Identifier,Identifier,Bool)]))}
wrap_Patterns :: T_Patterns  ->
                 Inh_Patterns  ->
                 Syn_Patterns 
wrap_Patterns (T_Patterns sem ) (Inh_Patterns _lhsIallTypeSigs _lhsIaltAttrs _lhsIcon _lhsIinh _lhsInt _lhsIsyn )  =
    (let ( _lhsOcopy,_lhsOerrors,_lhsOgathAltAttrs,_lhsOinstVars,_lhsOlocVars,_lhsOpatternAttrs) = sem _lhsIallTypeSigs _lhsIaltAttrs _lhsIcon _lhsIinh _lhsInt _lhsIsyn 
     in  (Syn_Patterns _lhsOcopy _lhsOerrors _lhsOgathAltAttrs _lhsOinstVars _lhsOlocVars _lhsOpatternAttrs ))
sem_Patterns_Cons :: T_Pattern  ->
                     T_Patterns  ->
                     T_Patterns 
sem_Patterns_Cons (T_Pattern hd_ ) (T_Patterns tl_ )  =
    (T_Patterns (\ _lhsIallTypeSigs
                   _lhsIaltAttrs
                   _lhsIcon
                   _lhsIinh
                   _lhsInt
                   _lhsIsyn ->
                     (let _lhsOerrors :: (Seq Error)
                          _lhsOgathAltAttrs :: ([AltAttr])
                          _lhsOinstVars :: ([Identifier])
                          _lhsOlocVars :: ([Identifier])
                          _lhsOpatternAttrs :: ([(Identifier,Identifier,Bool)])
                          _lhsOcopy :: Patterns 
                          _hdOallTypeSigs :: (Map Identifier Type)
                          _hdOaltAttrs :: (Map AltAttr Vertex)
                          _hdOcon :: Identifier
                          _hdOinh :: Attributes
                          _hdOnt :: Identifier
                          _hdOsyn :: Attributes
                          _tlOallTypeSigs :: (Map Identifier Type)
                          _tlOaltAttrs :: (Map AltAttr Vertex)
                          _tlOcon :: Identifier
                          _tlOinh :: Attributes
                          _tlOnt :: Identifier
                          _tlOsyn :: Attributes
                          _hdIcopy :: Pattern 
                          _hdIerrors :: (Seq Error)
                          _hdIgathAltAttrs :: ([AltAttr])
                          _hdIinstVars :: ([Identifier])
                          _hdIlocVars :: ([Identifier])
                          _hdIpatternAttrs :: ([(Identifier,Identifier,Bool)])
                          _tlIcopy :: Patterns 
                          _tlIerrors :: (Seq Error)
                          _tlIgathAltAttrs :: ([AltAttr])
                          _tlIinstVars :: ([Identifier])
                          _tlIlocVars :: ([Identifier])
                          _tlIpatternAttrs :: ([(Identifier,Identifier,Bool)])
                          -- use rule "src-ag/Order.ag"(line 84, column 70)
                          _lhsOerrors =
                              ({-# LINE 84 "src-ag/Order.ag" #-}
                               _hdIerrors Seq.>< _tlIerrors
                               {-# LINE 3230 "src-ag/Order.hs" #-}
                               )
                          -- use rule "src-ag/Order.ag"(line 170, column 68)
                          _lhsOgathAltAttrs =
                              ({-# LINE 170 "src-ag/Order.ag" #-}
                               _hdIgathAltAttrs ++ _tlIgathAltAttrs
                               {-# LINE 3236 "src-ag/Order.hs" #-}
                               )
                          -- use rule "src-ag/Order.ag"(line 677, column 86)
                          _lhsOinstVars =
                              ({-# LINE 677 "src-ag/Order.ag" #-}
                               _hdIinstVars ++ _tlIinstVars
                               {-# LINE 3242 "src-ag/Order.hs" #-}
                               )
                          -- use rule "src-ag/Order.ag"(line 677, column 48)
                          _lhsOlocVars =
                              ({-# LINE 677 "src-ag/Order.ag" #-}
                               _hdIlocVars ++ _tlIlocVars
                               {-# LINE 3248 "src-ag/Order.hs" #-}
                               )
                          -- use rule "src-ag/Order.ag"(line 247, column 42)
                          _lhsOpatternAttrs =
                              ({-# LINE 247 "src-ag/Order.ag" #-}
                               _hdIpatternAttrs ++ _tlIpatternAttrs
                               {-# LINE 3254 "src-ag/Order.hs" #-}
                               )
                          -- self rule
                          _copy =
                              ({-# LINE 22 "src-ag/Patterns.ag" #-}
                               (:) _hdIcopy _tlIcopy
                               {-# LINE 3260 "src-ag/Order.hs" #-}
                               )
                          -- self rule
                          _lhsOcopy =
                              ({-# LINE 22 "src-ag/Patterns.ag" #-}
                               _copy
                               {-# LINE 3266 "src-ag/Order.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOallTypeSigs =
                              ({-# LINE 533 "src-ag/Order.ag" #-}
                               _lhsIallTypeSigs
                               {-# LINE 3272 "src-ag/Order.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOaltAttrs =
                              ({-# LINE 186 "src-ag/Order.ag" #-}
                               _lhsIaltAttrs
                               {-# LINE 3278 "src-ag/Order.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOcon =
                              ({-# LINE 90 "src-ag/Order.ag" #-}
                               _lhsIcon
                               {-# LINE 3284 "src-ag/Order.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOinh =
                              ({-# LINE 89 "src-ag/Order.ag" #-}
                               _lhsIinh
                               {-# LINE 3290 "src-ag/Order.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOnt =
                              ({-# LINE 89 "src-ag/Order.ag" #-}
                               _lhsInt
                               {-# LINE 3296 "src-ag/Order.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOsyn =
                              ({-# LINE 89 "src-ag/Order.ag" #-}
                               _lhsIsyn
                               {-# LINE 3302 "src-ag/Order.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOallTypeSigs =
                              ({-# LINE 533 "src-ag/Order.ag" #-}
                               _lhsIallTypeSigs
                               {-# LINE 3308 "src-ag/Order.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOaltAttrs =
                              ({-# LINE 186 "src-ag/Order.ag" #-}
                               _lhsIaltAttrs
                               {-# LINE 3314 "src-ag/Order.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOcon =
                              ({-# LINE 90 "src-ag/Order.ag" #-}
                               _lhsIcon
                               {-# LINE 3320 "src-ag/Order.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOinh =
                              ({-# LINE 89 "src-ag/Order.ag" #-}
                               _lhsIinh
                               {-# LINE 3326 "src-ag/Order.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOnt =
                              ({-# LINE 89 "src-ag/Order.ag" #-}
                               _lhsInt
                               {-# LINE 3332 "src-ag/Order.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOsyn =
                              ({-# LINE 89 "src-ag/Order.ag" #-}
                               _lhsIsyn
                               {-# LINE 3338 "src-ag/Order.hs" #-}
                               )
                          ( _hdIcopy,_hdIerrors,_hdIgathAltAttrs,_hdIinstVars,_hdIlocVars,_hdIpatternAttrs) =
                              hd_ _hdOallTypeSigs _hdOaltAttrs _hdOcon _hdOinh _hdOnt _hdOsyn 
                          ( _tlIcopy,_tlIerrors,_tlIgathAltAttrs,_tlIinstVars,_tlIlocVars,_tlIpatternAttrs) =
                              tl_ _tlOallTypeSigs _tlOaltAttrs _tlOcon _tlOinh _tlOnt _tlOsyn 
                      in  ( _lhsOcopy,_lhsOerrors,_lhsOgathAltAttrs,_lhsOinstVars,_lhsOlocVars,_lhsOpatternAttrs))) )
sem_Patterns_Nil :: T_Patterns 
sem_Patterns_Nil  =
    (T_Patterns (\ _lhsIallTypeSigs
                   _lhsIaltAttrs
                   _lhsIcon
                   _lhsIinh
                   _lhsInt
                   _lhsIsyn ->
                     (let _lhsOerrors :: (Seq Error)
                          _lhsOgathAltAttrs :: ([AltAttr])
                          _lhsOinstVars :: ([Identifier])
                          _lhsOlocVars :: ([Identifier])
                          _lhsOpatternAttrs :: ([(Identifier,Identifier,Bool)])
                          _lhsOcopy :: Patterns 
                          -- use rule "src-ag/Order.ag"(line 84, column 70)
                          _lhsOerrors =
                              ({-# LINE 84 "src-ag/Order.ag" #-}
                               Seq.empty
                               {-# LINE 3363 "src-ag/Order.hs" #-}
                               )
                          -- use rule "src-ag/Order.ag"(line 170, column 68)
                          _lhsOgathAltAttrs =
                              ({-# LINE 170 "src-ag/Order.ag" #-}
                               []
                               {-# LINE 3369 "src-ag/Order.hs" #-}
                               )
                          -- use rule "src-ag/Order.ag"(line 677, column 86)
                          _lhsOinstVars =
                              ({-# LINE 677 "src-ag/Order.ag" #-}
                               []
                               {-# LINE 3375 "src-ag/Order.hs" #-}
                               )
                          -- use rule "src-ag/Order.ag"(line 677, column 48)
                          _lhsOlocVars =
                              ({-# LINE 677 "src-ag/Order.ag" #-}
                               []
                               {-# LINE 3381 "src-ag/Order.hs" #-}
                               )
                          -- use rule "src-ag/Order.ag"(line 247, column 42)
                          _lhsOpatternAttrs =
                              ({-# LINE 247 "src-ag/Order.ag" #-}
                               []
                               {-# LINE 3387 "src-ag/Order.hs" #-}
                               )
                          -- self rule
                          _copy =
                              ({-# LINE 22 "src-ag/Patterns.ag" #-}
                               []
                               {-# LINE 3393 "src-ag/Order.hs" #-}
                               )
                          -- self rule
                          _lhsOcopy =
                              ({-# LINE 22 "src-ag/Patterns.ag" #-}
                               _copy
                               {-# LINE 3399 "src-ag/Order.hs" #-}
                               )
                      in  ( _lhsOcopy,_lhsOerrors,_lhsOgathAltAttrs,_lhsOinstVars,_lhsOlocVars,_lhsOpatternAttrs))) )
-- Production --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allnts               : [Identifier]
         aroundMap            : Map ConstructorIdent (Map Identifier [Expression])
         cVisitsMap           : CVisitsMap
         inh                  : Attributes
         inhMap               : Map Identifier Attributes
         manualAttrDepMap     : AttrOrderMap
         mergeMap             : Map ConstructorIdent (Map Identifier (Identifier,[Identifier]))
         nt                   : Identifier
         o_case               : Bool
         o_cata               : Bool
         o_dovisit            : Bool
         o_newtypes           : Bool
         o_rename             : Bool
         o_sem                : Bool
         o_sig                : Bool
         o_unbox              : Bool
         o_wantvisit          : Bool
         prefix               : String
         syn                  : Attributes
         synMap               : Map Identifier Attributes
      chained attribute:
         vcount               : Int
      synthesized attributes:
         additionalDep        : Seq Edge
         aroundDep            : Seq Edge
         cProduction          : CProduction
         cons                 : [ConstructorIdent]
         directDep            : Seq Edge
         errors               : Seq Error
         instDep              : Seq Edge
         mergeDep             : Seq Edge
         nAutoRules           : Int
         nExplicitRules       : Int
         rules                : Seq (Vertex,CRule)
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
            local gathAltAttrs : _
            local altAttrs    : _
            local inhRules    : _
            local gathRules   : _
            local manualDeps  : _
            local mergeMap    : _
            local mergeDep1   : _
            local mergeDep2   : _
            local aroundMap   : _
            local aroundDep1  : _
            local aroundDep2  : _
            local cVisits     : _
            local allfields   : _
            local attrs       : _
            local inhnames    : _
            local synnames    : _
-}
-- cata
sem_Production :: Production  ->
                  T_Production 
sem_Production (Production _con _params _constraints _children _rules _typeSigs _macro )  =
    (sem_Production_Production _con _params _constraints (sem_Children _children ) (sem_Rules _rules ) (sem_TypeSigs _typeSigs ) _macro )
-- semantic domain
newtype T_Production  = T_Production (([Identifier]) ->
                                      (Map ConstructorIdent (Map Identifier [Expression])) ->
                                      CVisitsMap ->
                                      Attributes ->
                                      (Map Identifier Attributes) ->
                                      AttrOrderMap ->
                                      (Map ConstructorIdent (Map Identifier (Identifier,[Identifier]))) ->
                                      Identifier ->
                                      Bool ->
                                      Bool ->
                                      Bool ->
                                      Bool ->
                                      Bool ->
                                      Bool ->
                                      Bool ->
                                      Bool ->
                                      Bool ->
                                      String ->
                                      Attributes ->
                                      (Map Identifier Attributes) ->
                                      Int ->
                                      ( (Seq Edge),(Seq Edge),CProduction,([ConstructorIdent]),(Seq Edge),(Seq Error),(Seq Edge),(Seq Edge),Int,Int,(Seq (Vertex,CRule)),Int))
data Inh_Production  = Inh_Production {allnts_Inh_Production :: !(([Identifier])),aroundMap_Inh_Production :: !((Map ConstructorIdent (Map Identifier [Expression]))),cVisitsMap_Inh_Production :: !(CVisitsMap),inh_Inh_Production :: !(Attributes),inhMap_Inh_Production :: !((Map Identifier Attributes)),manualAttrDepMap_Inh_Production :: !(AttrOrderMap),mergeMap_Inh_Production :: !((Map ConstructorIdent (Map Identifier (Identifier,[Identifier])))),nt_Inh_Production :: !(Identifier),o_case_Inh_Production :: !(Bool),o_cata_Inh_Production :: !(Bool),o_dovisit_Inh_Production :: !(Bool),o_newtypes_Inh_Production :: !(Bool),o_rename_Inh_Production :: !(Bool),o_sem_Inh_Production :: !(Bool),o_sig_Inh_Production :: !(Bool),o_unbox_Inh_Production :: !(Bool),o_wantvisit_Inh_Production :: !(Bool),prefix_Inh_Production :: !(String),syn_Inh_Production :: !(Attributes),synMap_Inh_Production :: !((Map Identifier Attributes)),vcount_Inh_Production :: !(Int)}
data Syn_Production  = Syn_Production {additionalDep_Syn_Production :: !((Seq Edge)),aroundDep_Syn_Production :: !((Seq Edge)),cProduction_Syn_Production :: !(CProduction),cons_Syn_Production :: !(([ConstructorIdent])),directDep_Syn_Production :: !((Seq Edge)),errors_Syn_Production :: !((Seq Error)),instDep_Syn_Production :: !((Seq Edge)),mergeDep_Syn_Production :: !((Seq Edge)),nAutoRules_Syn_Production :: !(Int),nExplicitRules_Syn_Production :: !(Int),rules_Syn_Production :: !((Seq (Vertex,CRule))),vcount_Syn_Production :: !(Int)}
wrap_Production :: T_Production  ->
                   Inh_Production  ->
                   Syn_Production 
wrap_Production (T_Production sem ) (Inh_Production _lhsIallnts _lhsIaroundMap _lhsIcVisitsMap _lhsIinh _lhsIinhMap _lhsImanualAttrDepMap _lhsImergeMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_unbox _lhsIo_wantvisit _lhsIprefix _lhsIsyn _lhsIsynMap _lhsIvcount )  =
    (let ( _lhsOadditionalDep,_lhsOaroundDep,_lhsOcProduction,_lhsOcons,_lhsOdirectDep,_lhsOerrors,_lhsOinstDep,_lhsOmergeDep,_lhsOnAutoRules,_lhsOnExplicitRules,_lhsOrules,_lhsOvcount) = sem _lhsIallnts _lhsIaroundMap _lhsIcVisitsMap _lhsIinh _lhsIinhMap _lhsImanualAttrDepMap _lhsImergeMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_unbox _lhsIo_wantvisit _lhsIprefix _lhsIsyn _lhsIsynMap _lhsIvcount 
     in  (Syn_Production _lhsOadditionalDep _lhsOaroundDep _lhsOcProduction _lhsOcons _lhsOdirectDep _lhsOerrors _lhsOinstDep _lhsOmergeDep _lhsOnAutoRules _lhsOnExplicitRules _lhsOrules _lhsOvcount ))
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
                     _lhsIaroundMap
                     _lhsIcVisitsMap
                     _lhsIinh
                     _lhsIinhMap
                     _lhsImanualAttrDepMap
                     _lhsImergeMap
                     _lhsInt
                     _lhsIo_case
                     _lhsIo_cata
                     _lhsIo_dovisit
                     _lhsIo_newtypes
                     _lhsIo_rename
                     _lhsIo_sem
                     _lhsIo_sig
                     _lhsIo_unbox
                     _lhsIo_wantvisit
                     _lhsIprefix
                     _lhsIsyn
                     _lhsIsynMap
                     _lhsIvcount ->
                       (let _childrenOcon :: Identifier
                            _rulesOcon :: Identifier
                            _rulesOchildNts :: (Map Identifier NontermIdent)
                            _rulesOchildInhs :: (Map Identifier Attributes)
                            _lhsOrules :: (Seq (Vertex,CRule))
                            _lhsOvcount :: Int
                            _lhsOadditionalDep :: (Seq Edge)
                            _rulesOsynsOfChildren :: (Map Identifier Attributes)
                            _rulesOinhsOfChildren :: (Map Identifier Attributes)
                            _lhsOmergeDep :: (Seq Edge)
                            _lhsOaroundDep :: (Seq Edge)
                            _lhsOcons :: ([ConstructorIdent])
                            _typeSigsOtypeSigs :: (Map Identifier Type)
                            _rulesOallTypeSigs :: (Map Identifier Type)
                            _lhsOcProduction :: CProduction
                            _lhsOdirectDep :: (Seq Edge)
                            _lhsOerrors :: (Seq Error)
                            _lhsOinstDep :: (Seq Edge)
                            _lhsOnAutoRules :: Int
                            _lhsOnExplicitRules :: Int
                            _childrenOallfields :: ([(Identifier,Type,ChildKind)])
                            _childrenOallnts :: ([Identifier])
                            _childrenOattrs :: ([(Identifier,Identifier)])
                            _childrenOinh :: Attributes
                            _childrenOinhMap :: (Map Identifier Attributes)
                            _childrenOmergeMap :: (Map Identifier (Identifier,[Identifier]))
                            _childrenOnt :: Identifier
                            _childrenOo_unbox :: Bool
                            _childrenOsyn :: Attributes
                            _childrenOsynMap :: (Map Identifier Attributes)
                            _rulesOallfields :: ([(Identifier,Type,ChildKind)])
                            _rulesOallnts :: ([Identifier])
                            _rulesOaltAttrs :: (Map AltAttr Vertex)
                            _rulesOattrs :: ([(Identifier,Identifier)])
                            _rulesOinh :: Attributes
                            _rulesOmergeMap :: (Map Identifier (Identifier,[Identifier]))
                            _rulesOnt :: Identifier
                            _rulesOo_case :: Bool
                            _rulesOo_cata :: Bool
                            _rulesOo_dovisit :: Bool
                            _rulesOo_newtypes :: Bool
                            _rulesOo_rename :: Bool
                            _rulesOo_sem :: Bool
                            _rulesOo_sig :: Bool
                            _rulesOo_wantvisit :: Bool
                            _rulesOprefix :: String
                            _rulesOsyn :: Attributes
                            _childrenIattributes :: ([(Identifier,Attributes,Attributes)])
                            _childrenIcollectChildrenInhs :: (Map Identifier Attributes )
                            _childrenIcollectChildrenSyns :: (Map Identifier Attributes )
                            _childrenIerrors :: (Seq Error)
                            _childrenIfields :: ([(Identifier,Type,ChildKind)])
                            _childrenIgathAltAttrs :: ([AltAttr])
                            _childrenIgathRules :: (Seq CRule)
                            _childrenIinhs :: (Seq (Identifier,Attributes))
                            _childrenInts :: (Seq (Identifier,NontermIdent))
                            _childrenIsinglevisits :: ([CRule])
                            _childrenIterminals :: ([Identifier])
                            _rulesIdirectDep :: (Seq Edge)
                            _rulesIerrors :: (Seq Error)
                            _rulesIgathAltAttrs :: ([AltAttr])
                            _rulesIgathRules :: (Seq CRule)
                            _rulesIinstDep :: (Seq Edge)
                            _rulesIinstVars :: ([Identifier])
                            _rulesIlocVars :: ([Identifier])
                            _rulesInAutoRules :: Int
                            _rulesInExplicitRules :: Int
                            _typeSigsItypeSigs :: (Map Identifier Type)
                            -- "src-ag/Order.ag"(line 93, column 16)
                            _childrenOcon =
                                ({-# LINE 93 "src-ag/Order.ag" #-}
                                 con_
                                 {-# LINE 3605 "src-ag/Order.hs" #-}
                                 )
                            -- "src-ag/Order.ag"(line 95, column 16)
                            _rulesOcon =
                                ({-# LINE 95 "src-ag/Order.ag" #-}
                                 con_
                                 {-# LINE 3611 "src-ag/Order.hs" #-}
                                 )
                            -- "src-ag/Order.ag"(line 172, column 18)
                            _gathAltAttrs =
                                ({-# LINE 172 "src-ag/Order.ag" #-}
                                 [ AltAttr _LHS inh True | inh <- Map.keys _lhsIinh ]
                                  ++ _childrenIgathAltAttrs
                                  ++ _rulesIgathAltAttrs
                                 {-# LINE 3619 "src-ag/Order.hs" #-}
                                 )
                            -- "src-ag/Order.ag"(line 188, column 17)
                            _altAttrs =
                                ({-# LINE 188 "src-ag/Order.ag" #-}
                                 Map.fromList (zip _gathAltAttrs [_lhsIvcount..])
                                 {-# LINE 3625 "src-ag/Order.hs" #-}
                                 )
                            -- "src-ag/Order.ag"(line 201, column 18)
                            _rulesOchildNts =
                                ({-# LINE 201 "src-ag/Order.ag" #-}
                                 Map.fromList (toList _childrenInts)
                                 {-# LINE 3631 "src-ag/Order.hs" #-}
                                 )
                            -- "src-ag/Order.ag"(line 202, column 19)
                            _rulesOchildInhs =
                                ({-# LINE 202 "src-ag/Order.ag" #-}
                                 Map.fromList (toList _childrenIinhs)
                                 {-# LINE 3637 "src-ag/Order.hs" #-}
                                 )
                            -- "src-ag/Order.ag"(line 208, column 18)
                            _inhRules =
                                ({-# LINE 208 "src-ag/Order.ag" #-}
                                 [ cRuleLhsInh inh _lhsInt con_ tp | (inh,tp) <- Map.assocs _lhsIinh ]
                                 {-# LINE 3643 "src-ag/Order.hs" #-}
                                 )
                            -- "src-ag/Order.ag"(line 209, column 19)
                            _gathRules =
                                ({-# LINE 209 "src-ag/Order.ag" #-}
                                 _inhRules ++ toList (_childrenIgathRules Seq.>< _rulesIgathRules)
                                 {-# LINE 3649 "src-ag/Order.hs" #-}
                                 )
                            -- "src-ag/Order.ag"(line 261, column 18)
                            _lhsOrules =
                                ({-# LINE 261 "src-ag/Order.ag" #-}
                                 Seq.fromList (zip [_lhsIvcount..] _gathRules)
                                 {-# LINE 3655 "src-ag/Order.hs" #-}
                                 )
                            -- "src-ag/Order.ag"(line 262, column 19)
                            _lhsOvcount =
                                ({-# LINE 262 "src-ag/Order.ag" #-}
                                 _lhsIvcount + length _gathRules
                                 {-# LINE 3661 "src-ag/Order.hs" #-}
                                 )
                            -- "src-ag/Order.ag"(line 289, column 7)
                            _manualDeps =
                                ({-# LINE 289 "src-ag/Order.ag" #-}
                                 Set.toList $ Map.findWithDefault Set.empty con_ $ Map.findWithDefault Map.empty _lhsInt _lhsImanualAttrDepMap
                                 {-# LINE 3667 "src-ag/Order.hs" #-}
                                 )
                            -- "src-ag/Order.ag"(line 292, column 7)
                            _lhsOadditionalDep =
                                ({-# LINE 292 "src-ag/Order.ag" #-}
                                 Seq.fromList [ (vertex True occA, vertex False occB)
                                              | Dependency occA occB <- _manualDeps
                                              , let vertex inout (OccAttr child nm)
                                                      | child == _LOC = findWithErr2 (AltAttr _LOC nm True) _altAttrs
                                                      | otherwise     = findWithErr2 (AltAttr child nm inout) _altAttrs
                                                    vertex _ (OccRule nm)
                                                      = findWithErr2 (AltAttr _LOC (Ident ("_rule_" ++ show nm) (getPos nm)) True) _altAttrs
                                              ]
                                 {-# LINE 3680 "src-ag/Order.hs" #-}
                                 )
                            -- "src-ag/Order.ag"(line 339, column 17)
                            _rulesOsynsOfChildren =
                                ({-# LINE 339 "src-ag/Order.ag" #-}
                                 _childrenIcollectChildrenSyns
                                 {-# LINE 3686 "src-ag/Order.hs" #-}
                                 )
                            -- "src-ag/Order.ag"(line 340, column 17)
                            _rulesOinhsOfChildren =
                                ({-# LINE 340 "src-ag/Order.ag" #-}
                                 _childrenIcollectChildrenInhs
                                 {-# LINE 3692 "src-ag/Order.hs" #-}
                                 )
                            -- "src-ag/Order.ag"(line 358, column 32)
                            _mergeMap =
                                ({-# LINE 358 "src-ag/Order.ag" #-}
                                 Map.findWithDefault Map.empty con_ _lhsImergeMap
                                 {-# LINE 3698 "src-ag/Order.hs" #-}
                                 )
                            -- "src-ag/Order.ag"(line 369, column 7)
                            _lhsOmergeDep =
                                ({-# LINE 369 "src-ag/Order.ag" #-}
                                 _mergeDep1     Seq.>< _mergeDep2
                                 {-# LINE 3704 "src-ag/Order.hs" #-}
                                 )
                            -- "src-ag/Order.ag"(line 370, column 7)
                            _mergeDep1 =
                                ({-# LINE 370 "src-ag/Order.ag" #-}
                                 Seq.fromList $
                                    [ (childVert, synVert)
                                    | childNm <- Map.keys _mergeMap
                                    , synNm <- Map.keys (findWithErr2 childNm _childrenIcollectChildrenSyns)
                                    , let childNm' = Ident (show childNm ++ "_merge") (getPos childNm)
                                          childAttr = AltAttr _LOC childNm' True
                                          synAttr  = AltAttr childNm synNm True
                                          childVert = findWithErr2 childAttr _altAttrs
                                          synVert  = findWithErr2 synAttr _altAttrs
                                    ]
                                 {-# LINE 3719 "src-ag/Order.hs" #-}
                                 )
                            -- "src-ag/Order.ag"(line 381, column 7)
                            _mergeDep2 =
                                ({-# LINE 381 "src-ag/Order.ag" #-}
                                 Seq.fromList $
                                    [ (mergedVert, sourceVert)
                                    | (childNm, (_,cs)) <- Map.assocs _mergeMap
                                    , c <- cs
                                    , synNm <- Map.keys (findWithErr2 childNm _childrenIcollectChildrenSyns)
                                    , let sourceAttr = AltAttr childNm synNm True
                                          mergedAttr = AltAttr c synNm True
                                          sourceVert = findWithErr2 sourceAttr _altAttrs
                                          mergedVert = findWithErr2 mergedAttr _altAttrs
                                    ]
                                 {-# LINE 3734 "src-ag/Order.hs" #-}
                                 )
                            -- "src-ag/Order.ag"(line 411, column 32)
                            _aroundMap =
                                ({-# LINE 411 "src-ag/Order.ag" #-}
                                 Map.findWithDefault Map.empty con_ _lhsIaroundMap
                                 {-# LINE 3740 "src-ag/Order.hs" #-}
                                 )
                            -- "src-ag/Order.ag"(line 418, column 6)
                            _aroundDep1 =
                                ({-# LINE 418 "src-ag/Order.ag" #-}
                                 Seq.fromList $
                                   [ (childVert, synVert)
                                   | childNm <- Map.keys _aroundMap
                                   , synNm <- Map.keys (findWithErr2 childNm _childrenIcollectChildrenSyns)
                                   , let childNm' = Ident (show childNm ++ "_around") (getPos childNm)
                                         childAttr = AltAttr _LOC childNm' True
                                         synAttr  = AltAttr childNm synNm True
                                         childVert = findWithErr2 childAttr _altAttrs
                                         synVert  = findWithErr2 synAttr _altAttrs
                                   ]
                                 {-# LINE 3755 "src-ag/Order.hs" #-}
                                 )
                            -- "src-ag/Order.ag"(line 429, column 6)
                            _aroundDep2 =
                                ({-# LINE 429 "src-ag/Order.ag" #-}
                                 Seq.fromList $
                                   [ (childVert, inhVert)
                                   | childNm <- Map.keys _aroundMap
                                   , inhNm <- Map.keys (findWithErr2 childNm _childrenIcollectChildrenInhs)
                                   , let childNm'  = Ident (show childNm ++ "_around") (getPos childNm)
                                         childAttr = AltAttr _LOC childNm' True
                                         inhAttr   = AltAttr childNm inhNm False
                                         childVert = findWithErr2 childAttr _altAttrs
                                         inhVert   = findWithErr2 inhAttr _altAttrs
                                   ]
                                 {-# LINE 3770 "src-ag/Order.hs" #-}
                                 )
                            -- "src-ag/Order.ag"(line 440, column 6)
                            _lhsOaroundDep =
                                ({-# LINE 440 "src-ag/Order.ag" #-}
                                 _aroundDep1     Seq.>< _aroundDep2
                                 {-# LINE 3776 "src-ag/Order.hs" #-}
                                 )
                            -- "src-ag/Order.ag"(line 522, column 18)
                            _lhsOcons =
                                ({-# LINE 522 "src-ag/Order.ag" #-}
                                 [con_]
                                 {-# LINE 3782 "src-ag/Order.hs" #-}
                                 )
                            -- "src-ag/Order.ag"(line 529, column 16)
                            _typeSigsOtypeSigs =
                                ({-# LINE 529 "src-ag/Order.ag" #-}
                                 Map.empty
                                 {-# LINE 3788 "src-ag/Order.hs" #-}
                                 )
                            -- "src-ag/Order.ag"(line 535, column 17)
                            _rulesOallTypeSigs =
                                ({-# LINE 535 "src-ag/Order.ag" #-}
                                 _typeSigsItypeSigs
                                 {-# LINE 3794 "src-ag/Order.hs" #-}
                                 )
                            -- "src-ag/Order.ag"(line 603, column 17)
                            _cVisits =
                                ({-# LINE 603 "src-ag/Order.ag" #-}
                                 if  _lhsIo_dovisit
                                      then let prodsVisitsMap = findWithErr1 "Production.cVisits.nt" _lhsInt _lhsIcVisitsMap
                                               visits = findWithErr1 "Production.cVisits.con" con_ prodsVisitsMap
                                            in visits
                                      else  let  vss = nubBy eqCRuleDefines _gathRules ++ _childrenIsinglevisits
                                            in  [CVisit _lhsIinh _lhsIsyn vss [] False]
                                 {-# LINE 3805 "src-ag/Order.hs" #-}
                                 )
                            -- "src-ag/Order.ag"(line 629, column 18)
                            _lhsOcProduction =
                                ({-# LINE 629 "src-ag/Order.ag" #-}
                                 CProduction con_ _cVisits _childrenIfields _childrenIterminals
                                 {-# LINE 3811 "src-ag/Order.hs" #-}
                                 )
                            -- "src-ag/Order.ag"(line 657, column 16)
                            _allfields =
                                ({-# LINE 657 "src-ag/Order.ag" #-}
                                 _childrenIfields
                                 {-# LINE 3817 "src-ag/Order.hs" #-}
                                 )
                            -- "src-ag/Order.ag"(line 657, column 16)
                            _attrs =
                                ({-# LINE 658 "src-ag/Order.ag" #-}
                                 map ((,) _LOC)  _rulesIlocVars ++
                                 map ((,) _INST) _rulesIinstVars ++
                                 map ((,) _LHS)  _inhnames ++
                                 concat [map ((,) nm) (Map.keys as) | (nm,_,as) <- _childrenIattributes]
                                 {-# LINE 3826 "src-ag/Order.hs" #-}
                                 )
                            -- "src-ag/Order.ag"(line 657, column 16)
                            _inhnames =
                                ({-# LINE 662 "src-ag/Order.ag" #-}
                                 Map.keys _lhsIinh
                                 {-# LINE 3832 "src-ag/Order.hs" #-}
                                 )
                            -- "src-ag/Order.ag"(line 657, column 16)
                            _synnames =
                                ({-# LINE 663 "src-ag/Order.ag" #-}
                                 Map.keys _lhsIsyn
                                 {-# LINE 3838 "src-ag/Order.hs" #-}
                                 )
                            -- use rule "src-ag/Order.ag"(line 267, column 33)
                            _lhsOdirectDep =
                                ({-# LINE 267 "src-ag/Order.ag" #-}
                                 _rulesIdirectDep
                                 {-# LINE 3844 "src-ag/Order.hs" #-}
                                 )
                            -- use rule "src-ag/Order.ag"(line 84, column 70)
                            _lhsOerrors =
                                ({-# LINE 84 "src-ag/Order.ag" #-}
                                 _childrenIerrors Seq.>< _rulesIerrors
                                 {-# LINE 3850 "src-ag/Order.hs" #-}
                                 )
                            -- use rule "src-ag/Order.ag"(line 310, column 31)
                            _lhsOinstDep =
                                ({-# LINE 310 "src-ag/Order.ag" #-}
                                 _rulesIinstDep
                                 {-# LINE 3856 "src-ag/Order.hs" #-}
                                 )
                            -- use rule "src-ag/Order.ag"(line 61, column 105)
                            _lhsOnAutoRules =
                                ({-# LINE 61 "src-ag/Order.ag" #-}
                                 _rulesInAutoRules
                                 {-# LINE 3862 "src-ag/Order.hs" #-}
                                 )
                            -- use rule "src-ag/Order.ag"(line 61, column 105)
                            _lhsOnExplicitRules =
                                ({-# LINE 61 "src-ag/Order.ag" #-}
                                 _rulesInExplicitRules
                                 {-# LINE 3868 "src-ag/Order.hs" #-}
                                 )
                            -- copy rule (from local)
                            _childrenOallfields =
                                ({-# LINE 654 "src-ag/Order.ag" #-}
                                 _allfields
                                 {-# LINE 3874 "src-ag/Order.hs" #-}
                                 )
                            -- copy rule (down)
                            _childrenOallnts =
                                ({-# LINE 647 "src-ag/Order.ag" #-}
                                 _lhsIallnts
                                 {-# LINE 3880 "src-ag/Order.hs" #-}
                                 )
                            -- copy rule (from local)
                            _childrenOattrs =
                                ({-# LINE 654 "src-ag/Order.ag" #-}
                                 _attrs
                                 {-# LINE 3886 "src-ag/Order.hs" #-}
                                 )
                            -- copy rule (down)
                            _childrenOinh =
                                ({-# LINE 89 "src-ag/Order.ag" #-}
                                 _lhsIinh
                                 {-# LINE 3892 "src-ag/Order.hs" #-}
                                 )
                            -- copy rule (down)
                            _childrenOinhMap =
                                ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                 _lhsIinhMap
                                 {-# LINE 3898 "src-ag/Order.hs" #-}
                                 )
                            -- copy rule (from local)
                            _childrenOmergeMap =
                                ({-# LINE 360 "src-ag/Order.ag" #-}
                                 _mergeMap
                                 {-# LINE 3904 "src-ag/Order.hs" #-}
                                 )
                            -- copy rule (down)
                            _childrenOnt =
                                ({-# LINE 89 "src-ag/Order.ag" #-}
                                 _lhsInt
                                 {-# LINE 3910 "src-ag/Order.hs" #-}
                                 )
                            -- copy rule (down)
                            _childrenOo_unbox =
                                ({-# LINE 119 "src-ag/Order.ag" #-}
                                 _lhsIo_unbox
                                 {-# LINE 3916 "src-ag/Order.hs" #-}
                                 )
                            -- copy rule (down)
                            _childrenOsyn =
                                ({-# LINE 89 "src-ag/Order.ag" #-}
                                 _lhsIsyn
                                 {-# LINE 3922 "src-ag/Order.hs" #-}
                                 )
                            -- copy rule (down)
                            _childrenOsynMap =
                                ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                 _lhsIsynMap
                                 {-# LINE 3928 "src-ag/Order.hs" #-}
                                 )
                            -- copy rule (from local)
                            _rulesOallfields =
                                ({-# LINE 654 "src-ag/Order.ag" #-}
                                 _allfields
                                 {-# LINE 3934 "src-ag/Order.hs" #-}
                                 )
                            -- copy rule (down)
                            _rulesOallnts =
                                ({-# LINE 647 "src-ag/Order.ag" #-}
                                 _lhsIallnts
                                 {-# LINE 3940 "src-ag/Order.hs" #-}
                                 )
                            -- copy rule (from local)
                            _rulesOaltAttrs =
                                ({-# LINE 186 "src-ag/Order.ag" #-}
                                 _altAttrs
                                 {-# LINE 3946 "src-ag/Order.hs" #-}
                                 )
                            -- copy rule (from local)
                            _rulesOattrs =
                                ({-# LINE 654 "src-ag/Order.ag" #-}
                                 _attrs
                                 {-# LINE 3952 "src-ag/Order.hs" #-}
                                 )
                            -- copy rule (down)
                            _rulesOinh =
                                ({-# LINE 89 "src-ag/Order.ag" #-}
                                 _lhsIinh
                                 {-# LINE 3958 "src-ag/Order.hs" #-}
                                 )
                            -- copy rule (from local)
                            _rulesOmergeMap =
                                ({-# LINE 360 "src-ag/Order.ag" #-}
                                 _mergeMap
                                 {-# LINE 3964 "src-ag/Order.hs" #-}
                                 )
                            -- copy rule (down)
                            _rulesOnt =
                                ({-# LINE 89 "src-ag/Order.ag" #-}
                                 _lhsInt
                                 {-# LINE 3970 "src-ag/Order.hs" #-}
                                 )
                            -- copy rule (down)
                            _rulesOo_case =
                                ({-# LINE 117 "src-ag/Order.ag" #-}
                                 _lhsIo_case
                                 {-# LINE 3976 "src-ag/Order.hs" #-}
                                 )
                            -- copy rule (down)
                            _rulesOo_cata =
                                ({-# LINE 111 "src-ag/Order.ag" #-}
                                 _lhsIo_cata
                                 {-# LINE 3982 "src-ag/Order.hs" #-}
                                 )
                            -- copy rule (down)
                            _rulesOo_dovisit =
                                ({-# LINE 116 "src-ag/Order.ag" #-}
                                 _lhsIo_dovisit
                                 {-# LINE 3988 "src-ag/Order.hs" #-}
                                 )
                            -- copy rule (down)
                            _rulesOo_newtypes =
                                ({-# LINE 110 "src-ag/Order.ag" #-}
                                 _lhsIo_newtypes
                                 {-# LINE 3994 "src-ag/Order.hs" #-}
                                 )
                            -- copy rule (down)
                            _rulesOo_rename =
                                ({-# LINE 114 "src-ag/Order.ag" #-}
                                 _lhsIo_rename
                                 {-# LINE 4000 "src-ag/Order.hs" #-}
                                 )
                            -- copy rule (down)
                            _rulesOo_sem =
                                ({-# LINE 113 "src-ag/Order.ag" #-}
                                 _lhsIo_sem
                                 {-# LINE 4006 "src-ag/Order.hs" #-}
                                 )
                            -- copy rule (down)
                            _rulesOo_sig =
                                ({-# LINE 112 "src-ag/Order.ag" #-}
                                 _lhsIo_sig
                                 {-# LINE 4012 "src-ag/Order.hs" #-}
                                 )
                            -- copy rule (down)
                            _rulesOo_wantvisit =
                                ({-# LINE 115 "src-ag/Order.ag" #-}
                                 _lhsIo_wantvisit
                                 {-# LINE 4018 "src-ag/Order.hs" #-}
                                 )
                            -- copy rule (down)
                            _rulesOprefix =
                                ({-# LINE 118 "src-ag/Order.ag" #-}
                                 _lhsIprefix
                                 {-# LINE 4024 "src-ag/Order.hs" #-}
                                 )
                            -- copy rule (down)
                            _rulesOsyn =
                                ({-# LINE 89 "src-ag/Order.ag" #-}
                                 _lhsIsyn
                                 {-# LINE 4030 "src-ag/Order.hs" #-}
                                 )
                            ( _childrenIattributes,_childrenIcollectChildrenInhs,_childrenIcollectChildrenSyns,_childrenIerrors,_childrenIfields,_childrenIgathAltAttrs,_childrenIgathRules,_childrenIinhs,_childrenInts,_childrenIsinglevisits,_childrenIterminals) =
                                children_ _childrenOallfields _childrenOallnts _childrenOattrs _childrenOcon _childrenOinh _childrenOinhMap _childrenOmergeMap _childrenOnt _childrenOo_unbox _childrenOsyn _childrenOsynMap 
                            ( _rulesIdirectDep,_rulesIerrors,_rulesIgathAltAttrs,_rulesIgathRules,_rulesIinstDep,_rulesIinstVars,_rulesIlocVars,_rulesInAutoRules,_rulesInExplicitRules) =
                                rules_ _rulesOallTypeSigs _rulesOallfields _rulesOallnts _rulesOaltAttrs _rulesOattrs _rulesOchildInhs _rulesOchildNts _rulesOcon _rulesOinh _rulesOinhsOfChildren _rulesOmergeMap _rulesOnt _rulesOo_case _rulesOo_cata _rulesOo_dovisit _rulesOo_newtypes _rulesOo_rename _rulesOo_sem _rulesOo_sig _rulesOo_wantvisit _rulesOprefix _rulesOsyn _rulesOsynsOfChildren 
                            ( _typeSigsItypeSigs) =
                                typeSigs_ _typeSigsOtypeSigs 
                        in  ( _lhsOadditionalDep,_lhsOaroundDep,_lhsOcProduction,_lhsOcons,_lhsOdirectDep,_lhsOerrors,_lhsOinstDep,_lhsOmergeDep,_lhsOnAutoRules,_lhsOnExplicitRules,_lhsOrules,_lhsOvcount))) )
-- Productions -------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allnts               : [Identifier]
         aroundMap            : Map ConstructorIdent (Map Identifier [Expression])
         cVisitsMap           : CVisitsMap
         inh                  : Attributes
         inhMap               : Map Identifier Attributes
         manualAttrDepMap     : AttrOrderMap
         mergeMap             : Map ConstructorIdent (Map Identifier (Identifier,[Identifier]))
         nt                   : Identifier
         o_case               : Bool
         o_cata               : Bool
         o_dovisit            : Bool
         o_newtypes           : Bool
         o_rename             : Bool
         o_sem                : Bool
         o_sig                : Bool
         o_unbox              : Bool
         o_wantvisit          : Bool
         prefix               : String
         syn                  : Attributes
         synMap               : Map Identifier Attributes
      chained attribute:
         vcount               : Int
      synthesized attributes:
         additionalDep        : Seq Edge
         aroundDep            : Seq Edge
         cProductions         : CProductions
         cons                 : [ConstructorIdent]
         directDep            : Seq Edge
         errors               : Seq Error
         instDep              : Seq Edge
         mergeDep             : Seq Edge
         nAutoRules           : Int
         nExplicitRules       : Int
         rules                : Seq (Vertex,CRule)
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
newtype T_Productions  = T_Productions (([Identifier]) ->
                                        (Map ConstructorIdent (Map Identifier [Expression])) ->
                                        CVisitsMap ->
                                        Attributes ->
                                        (Map Identifier Attributes) ->
                                        AttrOrderMap ->
                                        (Map ConstructorIdent (Map Identifier (Identifier,[Identifier]))) ->
                                        Identifier ->
                                        Bool ->
                                        Bool ->
                                        Bool ->
                                        Bool ->
                                        Bool ->
                                        Bool ->
                                        Bool ->
                                        Bool ->
                                        Bool ->
                                        String ->
                                        Attributes ->
                                        (Map Identifier Attributes) ->
                                        Int ->
                                        ( (Seq Edge),(Seq Edge),CProductions,([ConstructorIdent]),(Seq Edge),(Seq Error),(Seq Edge),(Seq Edge),Int,Int,(Seq (Vertex,CRule)),Int))
data Inh_Productions  = Inh_Productions {allnts_Inh_Productions :: !(([Identifier])),aroundMap_Inh_Productions :: !((Map ConstructorIdent (Map Identifier [Expression]))),cVisitsMap_Inh_Productions :: !(CVisitsMap),inh_Inh_Productions :: !(Attributes),inhMap_Inh_Productions :: !((Map Identifier Attributes)),manualAttrDepMap_Inh_Productions :: !(AttrOrderMap),mergeMap_Inh_Productions :: !((Map ConstructorIdent (Map Identifier (Identifier,[Identifier])))),nt_Inh_Productions :: !(Identifier),o_case_Inh_Productions :: !(Bool),o_cata_Inh_Productions :: !(Bool),o_dovisit_Inh_Productions :: !(Bool),o_newtypes_Inh_Productions :: !(Bool),o_rename_Inh_Productions :: !(Bool),o_sem_Inh_Productions :: !(Bool),o_sig_Inh_Productions :: !(Bool),o_unbox_Inh_Productions :: !(Bool),o_wantvisit_Inh_Productions :: !(Bool),prefix_Inh_Productions :: !(String),syn_Inh_Productions :: !(Attributes),synMap_Inh_Productions :: !((Map Identifier Attributes)),vcount_Inh_Productions :: !(Int)}
data Syn_Productions  = Syn_Productions {additionalDep_Syn_Productions :: !((Seq Edge)),aroundDep_Syn_Productions :: !((Seq Edge)),cProductions_Syn_Productions :: !(CProductions),cons_Syn_Productions :: !(([ConstructorIdent])),directDep_Syn_Productions :: !((Seq Edge)),errors_Syn_Productions :: !((Seq Error)),instDep_Syn_Productions :: !((Seq Edge)),mergeDep_Syn_Productions :: !((Seq Edge)),nAutoRules_Syn_Productions :: !(Int),nExplicitRules_Syn_Productions :: !(Int),rules_Syn_Productions :: !((Seq (Vertex,CRule))),vcount_Syn_Productions :: !(Int)}
wrap_Productions :: T_Productions  ->
                    Inh_Productions  ->
                    Syn_Productions 
wrap_Productions (T_Productions sem ) (Inh_Productions _lhsIallnts _lhsIaroundMap _lhsIcVisitsMap _lhsIinh _lhsIinhMap _lhsImanualAttrDepMap _lhsImergeMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_unbox _lhsIo_wantvisit _lhsIprefix _lhsIsyn _lhsIsynMap _lhsIvcount )  =
    (let ( _lhsOadditionalDep,_lhsOaroundDep,_lhsOcProductions,_lhsOcons,_lhsOdirectDep,_lhsOerrors,_lhsOinstDep,_lhsOmergeDep,_lhsOnAutoRules,_lhsOnExplicitRules,_lhsOrules,_lhsOvcount) = sem _lhsIallnts _lhsIaroundMap _lhsIcVisitsMap _lhsIinh _lhsIinhMap _lhsImanualAttrDepMap _lhsImergeMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_unbox _lhsIo_wantvisit _lhsIprefix _lhsIsyn _lhsIsynMap _lhsIvcount 
     in  (Syn_Productions _lhsOadditionalDep _lhsOaroundDep _lhsOcProductions _lhsOcons _lhsOdirectDep _lhsOerrors _lhsOinstDep _lhsOmergeDep _lhsOnAutoRules _lhsOnExplicitRules _lhsOrules _lhsOvcount ))
sem_Productions_Cons :: T_Production  ->
                        T_Productions  ->
                        T_Productions 
sem_Productions_Cons (T_Production hd_ ) (T_Productions tl_ )  =
    (T_Productions (\ _lhsIallnts
                      _lhsIaroundMap
                      _lhsIcVisitsMap
                      _lhsIinh
                      _lhsIinhMap
                      _lhsImanualAttrDepMap
                      _lhsImergeMap
                      _lhsInt
                      _lhsIo_case
                      _lhsIo_cata
                      _lhsIo_dovisit
                      _lhsIo_newtypes
                      _lhsIo_rename
                      _lhsIo_sem
                      _lhsIo_sig
                      _lhsIo_unbox
                      _lhsIo_wantvisit
                      _lhsIprefix
                      _lhsIsyn
                      _lhsIsynMap
                      _lhsIvcount ->
                        (let _lhsOcProductions :: CProductions
                             _lhsOadditionalDep :: (Seq Edge)
                             _lhsOaroundDep :: (Seq Edge)
                             _lhsOcons :: ([ConstructorIdent])
                             _lhsOdirectDep :: (Seq Edge)
                             _lhsOerrors :: (Seq Error)
                             _lhsOinstDep :: (Seq Edge)
                             _lhsOmergeDep :: (Seq Edge)
                             _lhsOnAutoRules :: Int
                             _lhsOnExplicitRules :: Int
                             _lhsOrules :: (Seq (Vertex,CRule))
                             _lhsOvcount :: Int
                             _hdOallnts :: ([Identifier])
                             _hdOaroundMap :: (Map ConstructorIdent (Map Identifier [Expression]))
                             _hdOcVisitsMap :: CVisitsMap
                             _hdOinh :: Attributes
                             _hdOinhMap :: (Map Identifier Attributes)
                             _hdOmanualAttrDepMap :: AttrOrderMap
                             _hdOmergeMap :: (Map ConstructorIdent (Map Identifier (Identifier,[Identifier])))
                             _hdOnt :: Identifier
                             _hdOo_case :: Bool
                             _hdOo_cata :: Bool
                             _hdOo_dovisit :: Bool
                             _hdOo_newtypes :: Bool
                             _hdOo_rename :: Bool
                             _hdOo_sem :: Bool
                             _hdOo_sig :: Bool
                             _hdOo_unbox :: Bool
                             _hdOo_wantvisit :: Bool
                             _hdOprefix :: String
                             _hdOsyn :: Attributes
                             _hdOsynMap :: (Map Identifier Attributes)
                             _hdOvcount :: Int
                             _tlOallnts :: ([Identifier])
                             _tlOaroundMap :: (Map ConstructorIdent (Map Identifier [Expression]))
                             _tlOcVisitsMap :: CVisitsMap
                             _tlOinh :: Attributes
                             _tlOinhMap :: (Map Identifier Attributes)
                             _tlOmanualAttrDepMap :: AttrOrderMap
                             _tlOmergeMap :: (Map ConstructorIdent (Map Identifier (Identifier,[Identifier])))
                             _tlOnt :: Identifier
                             _tlOo_case :: Bool
                             _tlOo_cata :: Bool
                             _tlOo_dovisit :: Bool
                             _tlOo_newtypes :: Bool
                             _tlOo_rename :: Bool
                             _tlOo_sem :: Bool
                             _tlOo_sig :: Bool
                             _tlOo_unbox :: Bool
                             _tlOo_wantvisit :: Bool
                             _tlOprefix :: String
                             _tlOsyn :: Attributes
                             _tlOsynMap :: (Map Identifier Attributes)
                             _tlOvcount :: Int
                             _hdIadditionalDep :: (Seq Edge)
                             _hdIaroundDep :: (Seq Edge)
                             _hdIcProduction :: CProduction
                             _hdIcons :: ([ConstructorIdent])
                             _hdIdirectDep :: (Seq Edge)
                             _hdIerrors :: (Seq Error)
                             _hdIinstDep :: (Seq Edge)
                             _hdImergeDep :: (Seq Edge)
                             _hdInAutoRules :: Int
                             _hdInExplicitRules :: Int
                             _hdIrules :: (Seq (Vertex,CRule))
                             _hdIvcount :: Int
                             _tlIadditionalDep :: (Seq Edge)
                             _tlIaroundDep :: (Seq Edge)
                             _tlIcProductions :: CProductions
                             _tlIcons :: ([ConstructorIdent])
                             _tlIdirectDep :: (Seq Edge)
                             _tlIerrors :: (Seq Error)
                             _tlIinstDep :: (Seq Edge)
                             _tlImergeDep :: (Seq Edge)
                             _tlInAutoRules :: Int
                             _tlInExplicitRules :: Int
                             _tlIrules :: (Seq (Vertex,CRule))
                             _tlIvcount :: Int
                             -- "src-ag/Order.ag"(line 626, column 12)
                             _lhsOcProductions =
                                 ({-# LINE 626 "src-ag/Order.ag" #-}
                                  _hdIcProduction : _tlIcProductions
                                  {-# LINE 4226 "src-ag/Order.hs" #-}
                                  )
                             -- use rule "src-ag/Order.ag"(line 281, column 60)
                             _lhsOadditionalDep =
                                 ({-# LINE 281 "src-ag/Order.ag" #-}
                                  _hdIadditionalDep Seq.>< _tlIadditionalDep
                                  {-# LINE 4232 "src-ag/Order.hs" #-}
                                  )
                             -- use rule "src-ag/Order.ag"(line 402, column 24)
                             _lhsOaroundDep =
                                 ({-# LINE 402 "src-ag/Order.ag" #-}
                                  _hdIaroundDep Seq.>< _tlIaroundDep
                                  {-# LINE 4238 "src-ag/Order.hs" #-}
                                  )
                             -- use rule "src-ag/Order.ag"(line 520, column 40)
                             _lhsOcons =
                                 ({-# LINE 520 "src-ag/Order.ag" #-}
                                  _hdIcons ++ _tlIcons
                                  {-# LINE 4244 "src-ag/Order.hs" #-}
                                  )
                             -- use rule "src-ag/Order.ag"(line 267, column 33)
                             _lhsOdirectDep =
                                 ({-# LINE 267 "src-ag/Order.ag" #-}
                                  _hdIdirectDep Seq.>< _tlIdirectDep
                                  {-# LINE 4250 "src-ag/Order.hs" #-}
                                  )
                             -- use rule "src-ag/Order.ag"(line 84, column 70)
                             _lhsOerrors =
                                 ({-# LINE 84 "src-ag/Order.ag" #-}
                                  _hdIerrors Seq.>< _tlIerrors
                                  {-# LINE 4256 "src-ag/Order.hs" #-}
                                  )
                             -- use rule "src-ag/Order.ag"(line 310, column 31)
                             _lhsOinstDep =
                                 ({-# LINE 310 "src-ag/Order.ag" #-}
                                  _hdIinstDep Seq.>< _tlIinstDep
                                  {-# LINE 4262 "src-ag/Order.hs" #-}
                                  )
                             -- use rule "src-ag/Order.ag"(line 365, column 18)
                             _lhsOmergeDep =
                                 ({-# LINE 365 "src-ag/Order.ag" #-}
                                  _hdImergeDep Seq.>< _tlImergeDep
                                  {-# LINE 4268 "src-ag/Order.hs" #-}
                                  )
                             -- use rule "src-ag/Order.ag"(line 61, column 105)
                             _lhsOnAutoRules =
                                 ({-# LINE 61 "src-ag/Order.ag" #-}
                                  _hdInAutoRules + _tlInAutoRules
                                  {-# LINE 4274 "src-ag/Order.hs" #-}
                                  )
                             -- use rule "src-ag/Order.ag"(line 61, column 105)
                             _lhsOnExplicitRules =
                                 ({-# LINE 61 "src-ag/Order.ag" #-}
                                  _hdInExplicitRules + _tlInExplicitRules
                                  {-# LINE 4280 "src-ag/Order.hs" #-}
                                  )
                             -- use rule "src-ag/Order.ag"(line 257, column 18)
                             _lhsOrules =
                                 ({-# LINE 257 "src-ag/Order.ag" #-}
                                  _hdIrules Seq.>< _tlIrules
                                  {-# LINE 4286 "src-ag/Order.hs" #-}
                                  )
                             -- copy rule (up)
                             _lhsOvcount =
                                 ({-# LINE 256 "src-ag/Order.ag" #-}
                                  _tlIvcount
                                  {-# LINE 4292 "src-ag/Order.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOallnts =
                                 ({-# LINE 647 "src-ag/Order.ag" #-}
                                  _lhsIallnts
                                  {-# LINE 4298 "src-ag/Order.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOaroundMap =
                                 ({-# LINE 408 "src-ag/Order.ag" #-}
                                  _lhsIaroundMap
                                  {-# LINE 4304 "src-ag/Order.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOcVisitsMap =
                                 ({-# LINE 601 "src-ag/Order.ag" #-}
                                  _lhsIcVisitsMap
                                  {-# LINE 4310 "src-ag/Order.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOinh =
                                 ({-# LINE 89 "src-ag/Order.ag" #-}
                                  _lhsIinh
                                  {-# LINE 4316 "src-ag/Order.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOinhMap =
                                 ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                  _lhsIinhMap
                                  {-# LINE 4322 "src-ag/Order.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOmanualAttrDepMap =
                                 ({-# LINE 281 "src-ag/Order.ag" #-}
                                  _lhsImanualAttrDepMap
                                  {-# LINE 4328 "src-ag/Order.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOmergeMap =
                                 ({-# LINE 355 "src-ag/Order.ag" #-}
                                  _lhsImergeMap
                                  {-# LINE 4334 "src-ag/Order.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOnt =
                                 ({-# LINE 89 "src-ag/Order.ag" #-}
                                  _lhsInt
                                  {-# LINE 4340 "src-ag/Order.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOo_case =
                                 ({-# LINE 117 "src-ag/Order.ag" #-}
                                  _lhsIo_case
                                  {-# LINE 4346 "src-ag/Order.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOo_cata =
                                 ({-# LINE 111 "src-ag/Order.ag" #-}
                                  _lhsIo_cata
                                  {-# LINE 4352 "src-ag/Order.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOo_dovisit =
                                 ({-# LINE 116 "src-ag/Order.ag" #-}
                                  _lhsIo_dovisit
                                  {-# LINE 4358 "src-ag/Order.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOo_newtypes =
                                 ({-# LINE 110 "src-ag/Order.ag" #-}
                                  _lhsIo_newtypes
                                  {-# LINE 4364 "src-ag/Order.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOo_rename =
                                 ({-# LINE 114 "src-ag/Order.ag" #-}
                                  _lhsIo_rename
                                  {-# LINE 4370 "src-ag/Order.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOo_sem =
                                 ({-# LINE 113 "src-ag/Order.ag" #-}
                                  _lhsIo_sem
                                  {-# LINE 4376 "src-ag/Order.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOo_sig =
                                 ({-# LINE 112 "src-ag/Order.ag" #-}
                                  _lhsIo_sig
                                  {-# LINE 4382 "src-ag/Order.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOo_unbox =
                                 ({-# LINE 119 "src-ag/Order.ag" #-}
                                  _lhsIo_unbox
                                  {-# LINE 4388 "src-ag/Order.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOo_wantvisit =
                                 ({-# LINE 115 "src-ag/Order.ag" #-}
                                  _lhsIo_wantvisit
                                  {-# LINE 4394 "src-ag/Order.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOprefix =
                                 ({-# LINE 118 "src-ag/Order.ag" #-}
                                  _lhsIprefix
                                  {-# LINE 4400 "src-ag/Order.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOsyn =
                                 ({-# LINE 89 "src-ag/Order.ag" #-}
                                  _lhsIsyn
                                  {-# LINE 4406 "src-ag/Order.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOsynMap =
                                 ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                  _lhsIsynMap
                                  {-# LINE 4412 "src-ag/Order.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOvcount =
                                 ({-# LINE 256 "src-ag/Order.ag" #-}
                                  _lhsIvcount
                                  {-# LINE 4418 "src-ag/Order.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOallnts =
                                 ({-# LINE 647 "src-ag/Order.ag" #-}
                                  _lhsIallnts
                                  {-# LINE 4424 "src-ag/Order.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOaroundMap =
                                 ({-# LINE 408 "src-ag/Order.ag" #-}
                                  _lhsIaroundMap
                                  {-# LINE 4430 "src-ag/Order.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOcVisitsMap =
                                 ({-# LINE 601 "src-ag/Order.ag" #-}
                                  _lhsIcVisitsMap
                                  {-# LINE 4436 "src-ag/Order.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOinh =
                                 ({-# LINE 89 "src-ag/Order.ag" #-}
                                  _lhsIinh
                                  {-# LINE 4442 "src-ag/Order.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOinhMap =
                                 ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                  _lhsIinhMap
                                  {-# LINE 4448 "src-ag/Order.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOmanualAttrDepMap =
                                 ({-# LINE 281 "src-ag/Order.ag" #-}
                                  _lhsImanualAttrDepMap
                                  {-# LINE 4454 "src-ag/Order.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOmergeMap =
                                 ({-# LINE 355 "src-ag/Order.ag" #-}
                                  _lhsImergeMap
                                  {-# LINE 4460 "src-ag/Order.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOnt =
                                 ({-# LINE 89 "src-ag/Order.ag" #-}
                                  _lhsInt
                                  {-# LINE 4466 "src-ag/Order.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOo_case =
                                 ({-# LINE 117 "src-ag/Order.ag" #-}
                                  _lhsIo_case
                                  {-# LINE 4472 "src-ag/Order.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOo_cata =
                                 ({-# LINE 111 "src-ag/Order.ag" #-}
                                  _lhsIo_cata
                                  {-# LINE 4478 "src-ag/Order.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOo_dovisit =
                                 ({-# LINE 116 "src-ag/Order.ag" #-}
                                  _lhsIo_dovisit
                                  {-# LINE 4484 "src-ag/Order.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOo_newtypes =
                                 ({-# LINE 110 "src-ag/Order.ag" #-}
                                  _lhsIo_newtypes
                                  {-# LINE 4490 "src-ag/Order.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOo_rename =
                                 ({-# LINE 114 "src-ag/Order.ag" #-}
                                  _lhsIo_rename
                                  {-# LINE 4496 "src-ag/Order.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOo_sem =
                                 ({-# LINE 113 "src-ag/Order.ag" #-}
                                  _lhsIo_sem
                                  {-# LINE 4502 "src-ag/Order.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOo_sig =
                                 ({-# LINE 112 "src-ag/Order.ag" #-}
                                  _lhsIo_sig
                                  {-# LINE 4508 "src-ag/Order.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOo_unbox =
                                 ({-# LINE 119 "src-ag/Order.ag" #-}
                                  _lhsIo_unbox
                                  {-# LINE 4514 "src-ag/Order.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOo_wantvisit =
                                 ({-# LINE 115 "src-ag/Order.ag" #-}
                                  _lhsIo_wantvisit
                                  {-# LINE 4520 "src-ag/Order.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOprefix =
                                 ({-# LINE 118 "src-ag/Order.ag" #-}
                                  _lhsIprefix
                                  {-# LINE 4526 "src-ag/Order.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOsyn =
                                 ({-# LINE 89 "src-ag/Order.ag" #-}
                                  _lhsIsyn
                                  {-# LINE 4532 "src-ag/Order.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOsynMap =
                                 ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                  _lhsIsynMap
                                  {-# LINE 4538 "src-ag/Order.hs" #-}
                                  )
                             -- copy rule (chain)
                             _tlOvcount =
                                 ({-# LINE 256 "src-ag/Order.ag" #-}
                                  _hdIvcount
                                  {-# LINE 4544 "src-ag/Order.hs" #-}
                                  )
                             ( _hdIadditionalDep,_hdIaroundDep,_hdIcProduction,_hdIcons,_hdIdirectDep,_hdIerrors,_hdIinstDep,_hdImergeDep,_hdInAutoRules,_hdInExplicitRules,_hdIrules,_hdIvcount) =
                                 hd_ _hdOallnts _hdOaroundMap _hdOcVisitsMap _hdOinh _hdOinhMap _hdOmanualAttrDepMap _hdOmergeMap _hdOnt _hdOo_case _hdOo_cata _hdOo_dovisit _hdOo_newtypes _hdOo_rename _hdOo_sem _hdOo_sig _hdOo_unbox _hdOo_wantvisit _hdOprefix _hdOsyn _hdOsynMap _hdOvcount 
                             ( _tlIadditionalDep,_tlIaroundDep,_tlIcProductions,_tlIcons,_tlIdirectDep,_tlIerrors,_tlIinstDep,_tlImergeDep,_tlInAutoRules,_tlInExplicitRules,_tlIrules,_tlIvcount) =
                                 tl_ _tlOallnts _tlOaroundMap _tlOcVisitsMap _tlOinh _tlOinhMap _tlOmanualAttrDepMap _tlOmergeMap _tlOnt _tlOo_case _tlOo_cata _tlOo_dovisit _tlOo_newtypes _tlOo_rename _tlOo_sem _tlOo_sig _tlOo_unbox _tlOo_wantvisit _tlOprefix _tlOsyn _tlOsynMap _tlOvcount 
                         in  ( _lhsOadditionalDep,_lhsOaroundDep,_lhsOcProductions,_lhsOcons,_lhsOdirectDep,_lhsOerrors,_lhsOinstDep,_lhsOmergeDep,_lhsOnAutoRules,_lhsOnExplicitRules,_lhsOrules,_lhsOvcount))) )
sem_Productions_Nil :: T_Productions 
sem_Productions_Nil  =
    (T_Productions (\ _lhsIallnts
                      _lhsIaroundMap
                      _lhsIcVisitsMap
                      _lhsIinh
                      _lhsIinhMap
                      _lhsImanualAttrDepMap
                      _lhsImergeMap
                      _lhsInt
                      _lhsIo_case
                      _lhsIo_cata
                      _lhsIo_dovisit
                      _lhsIo_newtypes
                      _lhsIo_rename
                      _lhsIo_sem
                      _lhsIo_sig
                      _lhsIo_unbox
                      _lhsIo_wantvisit
                      _lhsIprefix
                      _lhsIsyn
                      _lhsIsynMap
                      _lhsIvcount ->
                        (let _lhsOcProductions :: CProductions
                             _lhsOadditionalDep :: (Seq Edge)
                             _lhsOaroundDep :: (Seq Edge)
                             _lhsOcons :: ([ConstructorIdent])
                             _lhsOdirectDep :: (Seq Edge)
                             _lhsOerrors :: (Seq Error)
                             _lhsOinstDep :: (Seq Edge)
                             _lhsOmergeDep :: (Seq Edge)
                             _lhsOnAutoRules :: Int
                             _lhsOnExplicitRules :: Int
                             _lhsOrules :: (Seq (Vertex,CRule))
                             _lhsOvcount :: Int
                             -- "src-ag/Order.ag"(line 627, column 12)
                             _lhsOcProductions =
                                 ({-# LINE 627 "src-ag/Order.ag" #-}
                                  []
                                  {-# LINE 4590 "src-ag/Order.hs" #-}
                                  )
                             -- use rule "src-ag/Order.ag"(line 281, column 60)
                             _lhsOadditionalDep =
                                 ({-# LINE 281 "src-ag/Order.ag" #-}
                                  Seq.empty
                                  {-# LINE 4596 "src-ag/Order.hs" #-}
                                  )
                             -- use rule "src-ag/Order.ag"(line 402, column 24)
                             _lhsOaroundDep =
                                 ({-# LINE 402 "src-ag/Order.ag" #-}
                                  Seq.empty
                                  {-# LINE 4602 "src-ag/Order.hs" #-}
                                  )
                             -- use rule "src-ag/Order.ag"(line 520, column 40)
                             _lhsOcons =
                                 ({-# LINE 520 "src-ag/Order.ag" #-}
                                  []
                                  {-# LINE 4608 "src-ag/Order.hs" #-}
                                  )
                             -- use rule "src-ag/Order.ag"(line 267, column 33)
                             _lhsOdirectDep =
                                 ({-# LINE 267 "src-ag/Order.ag" #-}
                                  Seq.empty
                                  {-# LINE 4614 "src-ag/Order.hs" #-}
                                  )
                             -- use rule "src-ag/Order.ag"(line 84, column 70)
                             _lhsOerrors =
                                 ({-# LINE 84 "src-ag/Order.ag" #-}
                                  Seq.empty
                                  {-# LINE 4620 "src-ag/Order.hs" #-}
                                  )
                             -- use rule "src-ag/Order.ag"(line 310, column 31)
                             _lhsOinstDep =
                                 ({-# LINE 310 "src-ag/Order.ag" #-}
                                  Seq.empty
                                  {-# LINE 4626 "src-ag/Order.hs" #-}
                                  )
                             -- use rule "src-ag/Order.ag"(line 365, column 18)
                             _lhsOmergeDep =
                                 ({-# LINE 365 "src-ag/Order.ag" #-}
                                  Seq.empty
                                  {-# LINE 4632 "src-ag/Order.hs" #-}
                                  )
                             -- use rule "src-ag/Order.ag"(line 61, column 105)
                             _lhsOnAutoRules =
                                 ({-# LINE 61 "src-ag/Order.ag" #-}
                                  0
                                  {-# LINE 4638 "src-ag/Order.hs" #-}
                                  )
                             -- use rule "src-ag/Order.ag"(line 61, column 105)
                             _lhsOnExplicitRules =
                                 ({-# LINE 61 "src-ag/Order.ag" #-}
                                  0
                                  {-# LINE 4644 "src-ag/Order.hs" #-}
                                  )
                             -- use rule "src-ag/Order.ag"(line 257, column 18)
                             _lhsOrules =
                                 ({-# LINE 257 "src-ag/Order.ag" #-}
                                  Seq.empty
                                  {-# LINE 4650 "src-ag/Order.hs" #-}
                                  )
                             -- copy rule (chain)
                             _lhsOvcount =
                                 ({-# LINE 256 "src-ag/Order.ag" #-}
                                  _lhsIvcount
                                  {-# LINE 4656 "src-ag/Order.hs" #-}
                                  )
                         in  ( _lhsOadditionalDep,_lhsOaroundDep,_lhsOcProductions,_lhsOcons,_lhsOdirectDep,_lhsOerrors,_lhsOinstDep,_lhsOmergeDep,_lhsOnAutoRules,_lhsOnExplicitRules,_lhsOrules,_lhsOvcount))) )
-- Rule --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allTypeSigs          : Map Identifier Type
         allfields            : [(Identifier,Type,ChildKind)]
         allnts               : [Identifier]
         altAttrs             : Map AltAttr Vertex
         attrs                : [(Identifier,Identifier)]
         childInhs            : Map Identifier Attributes
         childNts             : Map Identifier NontermIdent
         con                  : Identifier
         inh                  : Attributes
         inhsOfChildren       : Map Identifier Attributes
         mergeMap             : Map Identifier (Identifier,[Identifier])
         nt                   : Identifier
         o_case               : Bool
         o_cata               : Bool
         o_dovisit            : Bool
         o_newtypes           : Bool
         o_rename             : Bool
         o_sem                : Bool
         o_sig                : Bool
         o_wantvisit          : Bool
         prefix               : String
         syn                  : Attributes
         synsOfChildren       : Map Identifier Attributes
      synthesized attributes:
         directDep            : Seq Edge
         errors               : Seq Error
         gathAltAttrs         : [AltAttr]
         gathRules            : Seq CRule
         instDep              : Seq Edge
         instVars             : [Identifier]
         locVars              : [Identifier]
         nAutoRules           : Int
         nExplicitRules       : Int
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
            local defines     : _
            local gathRules   : _
            local instDep1    : _
            local instDep2    : _
-}
-- cata
sem_Rule :: Rule  ->
            T_Rule 
sem_Rule (Rule _mbName _pattern _rhs _owrt _origin _explicit _pure _identity _mbError _eager )  =
    (sem_Rule_Rule _mbName (sem_Pattern _pattern ) (sem_Expression _rhs ) _owrt _origin _explicit _pure _identity _mbError _eager )
-- semantic domain
newtype T_Rule  = T_Rule ((Map Identifier Type) ->
                          ([(Identifier,Type,ChildKind)]) ->
                          ([Identifier]) ->
                          (Map AltAttr Vertex) ->
                          ([(Identifier,Identifier)]) ->
                          (Map Identifier Attributes) ->
                          (Map Identifier NontermIdent) ->
                          Identifier ->
                          Attributes ->
                          (Map Identifier Attributes) ->
                          (Map Identifier (Identifier,[Identifier])) ->
                          Identifier ->
                          Bool ->
                          Bool ->
                          Bool ->
                          Bool ->
                          Bool ->
                          Bool ->
                          Bool ->
                          Bool ->
                          String ->
                          Attributes ->
                          (Map Identifier Attributes) ->
                          ( (Seq Edge),(Seq Error),([AltAttr]),(Seq CRule),(Seq Edge),([Identifier]),([Identifier]),Int,Int))
data Inh_Rule  = Inh_Rule {allTypeSigs_Inh_Rule :: !((Map Identifier Type)),allfields_Inh_Rule :: !(([(Identifier,Type,ChildKind)])),allnts_Inh_Rule :: !(([Identifier])),altAttrs_Inh_Rule :: !((Map AltAttr Vertex)),attrs_Inh_Rule :: !(([(Identifier,Identifier)])),childInhs_Inh_Rule :: !((Map Identifier Attributes)),childNts_Inh_Rule :: !((Map Identifier NontermIdent)),con_Inh_Rule :: !(Identifier),inh_Inh_Rule :: !(Attributes),inhsOfChildren_Inh_Rule :: !((Map Identifier Attributes)),mergeMap_Inh_Rule :: !((Map Identifier (Identifier,[Identifier]))),nt_Inh_Rule :: !(Identifier),o_case_Inh_Rule :: !(Bool),o_cata_Inh_Rule :: !(Bool),o_dovisit_Inh_Rule :: !(Bool),o_newtypes_Inh_Rule :: !(Bool),o_rename_Inh_Rule :: !(Bool),o_sem_Inh_Rule :: !(Bool),o_sig_Inh_Rule :: !(Bool),o_wantvisit_Inh_Rule :: !(Bool),prefix_Inh_Rule :: !(String),syn_Inh_Rule :: !(Attributes),synsOfChildren_Inh_Rule :: !((Map Identifier Attributes))}
data Syn_Rule  = Syn_Rule {directDep_Syn_Rule :: !((Seq Edge)),errors_Syn_Rule :: !((Seq Error)),gathAltAttrs_Syn_Rule :: !(([AltAttr])),gathRules_Syn_Rule :: !((Seq CRule)),instDep_Syn_Rule :: !((Seq Edge)),instVars_Syn_Rule :: !(([Identifier])),locVars_Syn_Rule :: !(([Identifier])),nAutoRules_Syn_Rule :: !(Int),nExplicitRules_Syn_Rule :: !(Int)}
wrap_Rule :: T_Rule  ->
             Inh_Rule  ->
             Syn_Rule 
wrap_Rule (T_Rule sem ) (Inh_Rule _lhsIallTypeSigs _lhsIallfields _lhsIallnts _lhsIaltAttrs _lhsIattrs _lhsIchildInhs _lhsIchildNts _lhsIcon _lhsIinh _lhsIinhsOfChildren _lhsImergeMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_wantvisit _lhsIprefix _lhsIsyn _lhsIsynsOfChildren )  =
    (let ( _lhsOdirectDep,_lhsOerrors,_lhsOgathAltAttrs,_lhsOgathRules,_lhsOinstDep,_lhsOinstVars,_lhsOlocVars,_lhsOnAutoRules,_lhsOnExplicitRules) = sem _lhsIallTypeSigs _lhsIallfields _lhsIallnts _lhsIaltAttrs _lhsIattrs _lhsIchildInhs _lhsIchildNts _lhsIcon _lhsIinh _lhsIinhsOfChildren _lhsImergeMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_wantvisit _lhsIprefix _lhsIsyn _lhsIsynsOfChildren 
     in  (Syn_Rule _lhsOdirectDep _lhsOerrors _lhsOgathAltAttrs _lhsOgathRules _lhsOinstDep _lhsOinstVars _lhsOlocVars _lhsOnAutoRules _lhsOnExplicitRules ))
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
    (T_Rule (\ _lhsIallTypeSigs
               _lhsIallfields
               _lhsIallnts
               _lhsIaltAttrs
               _lhsIattrs
               _lhsIchildInhs
               _lhsIchildNts
               _lhsIcon
               _lhsIinh
               _lhsIinhsOfChildren
               _lhsImergeMap
               _lhsInt
               _lhsIo_case
               _lhsIo_cata
               _lhsIo_dovisit
               _lhsIo_newtypes
               _lhsIo_rename
               _lhsIo_sem
               _lhsIo_sig
               _lhsIo_wantvisit
               _lhsIprefix
               _lhsIsyn
               _lhsIsynsOfChildren ->
                 (let _lhsOnExplicitRules :: Int
                      _lhsOnAutoRules :: Int
                      _lhsOdirectDep :: (Seq Edge)
                      _lhsOinstDep :: (Seq Edge)
                      _lhsOerrors :: (Seq Error)
                      _lhsOgathAltAttrs :: ([AltAttr])
                      _lhsOgathRules :: (Seq CRule)
                      _lhsOinstVars :: ([Identifier])
                      _lhsOlocVars :: ([Identifier])
                      _patternOallTypeSigs :: (Map Identifier Type)
                      _patternOaltAttrs :: (Map AltAttr Vertex)
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
                      _patternIcopy :: Pattern 
                      _patternIerrors :: (Seq Error)
                      _patternIgathAltAttrs :: ([AltAttr])
                      _patternIinstVars :: ([Identifier])
                      _patternIlocVars :: ([Identifier])
                      _patternIpatternAttrs :: ([(Identifier,Identifier,Bool)])
                      _rhsIallRhsVars :: (Set (Identifier,Identifier))
                      _rhsIcopy :: Expression 
                      _rhsIerrors :: (Seq Error)
                      _rhsItextLines :: ([String])
                      _rhsIusedAttrs :: ([(Identifier,Identifier)])
                      _rhsIusedFields :: ([Identifier])
                      _rhsIusedLocals :: ([Identifier])
                      -- "src-ag/Order.ag"(line 64, column 11)
                      _lhsOnExplicitRules =
                          ({-# LINE 64 "src-ag/Order.ag" #-}
                           if explicit_
                           then 1
                           else 0
                           {-# LINE 4827 "src-ag/Order.hs" #-}
                           )
                      -- "src-ag/Order.ag"(line 67, column 11)
                      _lhsOnAutoRules =
                          ({-# LINE 67 "src-ag/Order.ag" #-}
                           if startsWith "use rule" origin_ || startsWith "copy rule" origin_
                           then 1
                           else 0
                           {-# LINE 4835 "src-ag/Order.hs" #-}
                           )
                      -- "src-ag/Order.ag"(line 217, column 12)
                      _defines =
                          ({-# LINE 217 "src-ag/Order.ag" #-}
                           let  tp field attr      | field == _LOC || field == _INST
                                                                    = Map.lookup attr _lhsIallTypeSigs
                                                    | field == _LHS = Map.lookup attr _lhsIsyn
                                                    | otherwise     = Map.lookup attr (findWithErr1 "Rule.defines.tp" field _lhsIchildInhs)
                                typ :: Pattern -> Maybe Type
                                typ (Alias field attr _)       = tp field attr
                                typ (Underscore _)             = Nothing
                                typ _                          = Nothing
                           in Map.fromList  [ (findWithErr1 "Rule.defines" aa _lhsIaltAttrs, (field,attr,(tp field attr)))
                                            | (field,attr,isLocalOrInst) <- _patternIpatternAttrs
                                            , let aa = AltAttr field attr isLocalOrInst
                                            ]
                           {-# LINE 4852 "src-ag/Order.hs" #-}
                           )
                      -- "src-ag/Order.ag"(line 231, column 12)
                      _gathRules =
                          ({-# LINE 231 "src-ag/Order.ag" #-}
                           let childnt field = Map.lookup field _lhsIchildNts
                           in Seq.fromList [ CRule attr False True _lhsInt _lhsIcon field (childnt field) tp _patternIcopy _rhsItextLines _defines owrt_ origin_ _rhsIallRhsVars explicit_ mbName_
                                           | (field,attr,tp) <- Map.elems _defines
                                           ]
                           {-# LINE 4861 "src-ag/Order.hs" #-}
                           )
                      -- "src-ag/Order.ag"(line 269, column 12)
                      _lhsOdirectDep =
                          ({-# LINE 269 "src-ag/Order.ag" #-}
                           let  defined = Map.keys _defines
                                used =  [ Map.lookup (AltAttr field attr True) _lhsIaltAttrs | (field,attr) <- _rhsIusedAttrs]
                                        ++ [ Map.lookup (AltAttr _LOC attr True) _lhsIaltAttrs | attr <- _rhsIusedLocals ++ _rhsIusedFields ]
                           in Seq.fromList [ (x,y) | Just x <- used, y <- defined ]
                           {-# LINE 4870 "src-ag/Order.hs" #-}
                           )
                      -- "src-ag/Order.ag"(line 313, column 6)
                      _instDep1 =
                          ({-# LINE 313 "src-ag/Order.ag" #-}
                           Seq.fromList $
                             [ (instVert, synVert)
                             | (field,instNm,_) <- Map.elems _defines
                             , field == _INST
                             , synNm <- Map.keys (findWithErr2 instNm _lhsIsynsOfChildren)
                             , let instAttr = AltAttr _INST instNm True
                                   synAttr  = AltAttr instNm synNm True
                                   instVert = findWithErr2 instAttr _lhsIaltAttrs
                                   synVert  = findWithErr2 synAttr _lhsIaltAttrs
                             ]
                           {-# LINE 4885 "src-ag/Order.hs" #-}
                           )
                      -- "src-ag/Order.ag"(line 324, column 6)
                      _instDep2 =
                          ({-# LINE 324 "src-ag/Order.ag" #-}
                           Seq.fromList $
                             [ (instVert, inhVert)
                             | (field,instNm,_) <- Map.elems _defines
                             , field == _INST
                             , inhNm <- Map.keys (findWithErr2 instNm _lhsIinhsOfChildren)
                             , let instAttr = AltAttr _INST instNm True
                                   inhAttr  = AltAttr instNm inhNm False
                                   instVert = findWithErr2 instAttr _lhsIaltAttrs
                                   inhVert  = findWithErr2 inhAttr _lhsIaltAttrs
                             ]
                           {-# LINE 4900 "src-ag/Order.hs" #-}
                           )
                      -- "src-ag/Order.ag"(line 335, column 6)
                      _lhsOinstDep =
                          ({-# LINE 335 "src-ag/Order.ag" #-}
                           _instDep1     Seq.>< _instDep2
                           {-# LINE 4906 "src-ag/Order.hs" #-}
                           )
                      -- use rule "src-ag/Order.ag"(line 84, column 70)
                      _lhsOerrors =
                          ({-# LINE 84 "src-ag/Order.ag" #-}
                           _patternIerrors Seq.>< _rhsIerrors
                           {-# LINE 4912 "src-ag/Order.hs" #-}
                           )
                      -- use rule "src-ag/Order.ag"(line 170, column 68)
                      _lhsOgathAltAttrs =
                          ({-# LINE 170 "src-ag/Order.ag" #-}
                           _patternIgathAltAttrs
                           {-# LINE 4918 "src-ag/Order.hs" #-}
                           )
                      -- use rule "src-ag/Order.ag"(line 206, column 23)
                      _lhsOgathRules =
                          ({-# LINE 206 "src-ag/Order.ag" #-}
                           _gathRules
                           {-# LINE 4924 "src-ag/Order.hs" #-}
                           )
                      -- use rule "src-ag/Order.ag"(line 677, column 86)
                      _lhsOinstVars =
                          ({-# LINE 677 "src-ag/Order.ag" #-}
                           _patternIinstVars
                           {-# LINE 4930 "src-ag/Order.hs" #-}
                           )
                      -- use rule "src-ag/Order.ag"(line 677, column 48)
                      _lhsOlocVars =
                          ({-# LINE 677 "src-ag/Order.ag" #-}
                           _patternIlocVars
                           {-# LINE 4936 "src-ag/Order.hs" #-}
                           )
                      -- copy rule (down)
                      _patternOallTypeSigs =
                          ({-# LINE 533 "src-ag/Order.ag" #-}
                           _lhsIallTypeSigs
                           {-# LINE 4942 "src-ag/Order.hs" #-}
                           )
                      -- copy rule (down)
                      _patternOaltAttrs =
                          ({-# LINE 186 "src-ag/Order.ag" #-}
                           _lhsIaltAttrs
                           {-# LINE 4948 "src-ag/Order.hs" #-}
                           )
                      -- copy rule (down)
                      _patternOcon =
                          ({-# LINE 90 "src-ag/Order.ag" #-}
                           _lhsIcon
                           {-# LINE 4954 "src-ag/Order.hs" #-}
                           )
                      -- copy rule (down)
                      _patternOinh =
                          ({-# LINE 89 "src-ag/Order.ag" #-}
                           _lhsIinh
                           {-# LINE 4960 "src-ag/Order.hs" #-}
                           )
                      -- copy rule (down)
                      _patternOnt =
                          ({-# LINE 89 "src-ag/Order.ag" #-}
                           _lhsInt
                           {-# LINE 4966 "src-ag/Order.hs" #-}
                           )
                      -- copy rule (down)
                      _patternOsyn =
                          ({-# LINE 89 "src-ag/Order.ag" #-}
                           _lhsIsyn
                           {-# LINE 4972 "src-ag/Order.hs" #-}
                           )
                      -- copy rule (down)
                      _rhsOallfields =
                          ({-# LINE 446 "src-ag/Order.ag" #-}
                           _lhsIallfields
                           {-# LINE 4978 "src-ag/Order.hs" #-}
                           )
                      -- copy rule (down)
                      _rhsOallnts =
                          ({-# LINE 447 "src-ag/Order.ag" #-}
                           _lhsIallnts
                           {-# LINE 4984 "src-ag/Order.hs" #-}
                           )
                      -- copy rule (down)
                      _rhsOattrs =
                          ({-# LINE 448 "src-ag/Order.ag" #-}
                           _lhsIattrs
                           {-# LINE 4990 "src-ag/Order.hs" #-}
                           )
                      -- copy rule (down)
                      _rhsOcon =
                          ({-# LINE 445 "src-ag/Order.ag" #-}
                           _lhsIcon
                           {-# LINE 4996 "src-ag/Order.hs" #-}
                           )
                      -- copy rule (down)
                      _rhsOmergeMap =
                          ({-# LINE 360 "src-ag/Order.ag" #-}
                           _lhsImergeMap
                           {-# LINE 5002 "src-ag/Order.hs" #-}
                           )
                      -- copy rule (down)
                      _rhsOnt =
                          ({-# LINE 445 "src-ag/Order.ag" #-}
                           _lhsInt
                           {-# LINE 5008 "src-ag/Order.hs" #-}
                           )
                      ( _patternIcopy,_patternIerrors,_patternIgathAltAttrs,_patternIinstVars,_patternIlocVars,_patternIpatternAttrs) =
                          pattern_ _patternOallTypeSigs _patternOaltAttrs _patternOcon _patternOinh _patternOnt _patternOsyn 
                      ( _rhsIallRhsVars,_rhsIcopy,_rhsIerrors,_rhsItextLines,_rhsIusedAttrs,_rhsIusedFields,_rhsIusedLocals) =
                          rhs_ _rhsOallfields _rhsOallnts _rhsOattrs _rhsOcon _rhsOmergeMap _rhsOnt 
                  in  ( _lhsOdirectDep,_lhsOerrors,_lhsOgathAltAttrs,_lhsOgathRules,_lhsOinstDep,_lhsOinstVars,_lhsOlocVars,_lhsOnAutoRules,_lhsOnExplicitRules))) )
-- Rules -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allTypeSigs          : Map Identifier Type
         allfields            : [(Identifier,Type,ChildKind)]
         allnts               : [Identifier]
         altAttrs             : Map AltAttr Vertex
         attrs                : [(Identifier,Identifier)]
         childInhs            : Map Identifier Attributes
         childNts             : Map Identifier NontermIdent
         con                  : Identifier
         inh                  : Attributes
         inhsOfChildren       : Map Identifier Attributes
         mergeMap             : Map Identifier (Identifier,[Identifier])
         nt                   : Identifier
         o_case               : Bool
         o_cata               : Bool
         o_dovisit            : Bool
         o_newtypes           : Bool
         o_rename             : Bool
         o_sem                : Bool
         o_sig                : Bool
         o_wantvisit          : Bool
         prefix               : String
         syn                  : Attributes
         synsOfChildren       : Map Identifier Attributes
      synthesized attributes:
         directDep            : Seq Edge
         errors               : Seq Error
         gathAltAttrs         : [AltAttr]
         gathRules            : Seq CRule
         instDep              : Seq Edge
         instVars             : [Identifier]
         locVars              : [Identifier]
         nAutoRules           : Int
         nExplicitRules       : Int
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
newtype T_Rules  = T_Rules ((Map Identifier Type) ->
                            ([(Identifier,Type,ChildKind)]) ->
                            ([Identifier]) ->
                            (Map AltAttr Vertex) ->
                            ([(Identifier,Identifier)]) ->
                            (Map Identifier Attributes) ->
                            (Map Identifier NontermIdent) ->
                            Identifier ->
                            Attributes ->
                            (Map Identifier Attributes) ->
                            (Map Identifier (Identifier,[Identifier])) ->
                            Identifier ->
                            Bool ->
                            Bool ->
                            Bool ->
                            Bool ->
                            Bool ->
                            Bool ->
                            Bool ->
                            Bool ->
                            String ->
                            Attributes ->
                            (Map Identifier Attributes) ->
                            ( (Seq Edge),(Seq Error),([AltAttr]),(Seq CRule),(Seq Edge),([Identifier]),([Identifier]),Int,Int))
data Inh_Rules  = Inh_Rules {allTypeSigs_Inh_Rules :: !((Map Identifier Type)),allfields_Inh_Rules :: !(([(Identifier,Type,ChildKind)])),allnts_Inh_Rules :: !(([Identifier])),altAttrs_Inh_Rules :: !((Map AltAttr Vertex)),attrs_Inh_Rules :: !(([(Identifier,Identifier)])),childInhs_Inh_Rules :: !((Map Identifier Attributes)),childNts_Inh_Rules :: !((Map Identifier NontermIdent)),con_Inh_Rules :: !(Identifier),inh_Inh_Rules :: !(Attributes),inhsOfChildren_Inh_Rules :: !((Map Identifier Attributes)),mergeMap_Inh_Rules :: !((Map Identifier (Identifier,[Identifier]))),nt_Inh_Rules :: !(Identifier),o_case_Inh_Rules :: !(Bool),o_cata_Inh_Rules :: !(Bool),o_dovisit_Inh_Rules :: !(Bool),o_newtypes_Inh_Rules :: !(Bool),o_rename_Inh_Rules :: !(Bool),o_sem_Inh_Rules :: !(Bool),o_sig_Inh_Rules :: !(Bool),o_wantvisit_Inh_Rules :: !(Bool),prefix_Inh_Rules :: !(String),syn_Inh_Rules :: !(Attributes),synsOfChildren_Inh_Rules :: !((Map Identifier Attributes))}
data Syn_Rules  = Syn_Rules {directDep_Syn_Rules :: !((Seq Edge)),errors_Syn_Rules :: !((Seq Error)),gathAltAttrs_Syn_Rules :: !(([AltAttr])),gathRules_Syn_Rules :: !((Seq CRule)),instDep_Syn_Rules :: !((Seq Edge)),instVars_Syn_Rules :: !(([Identifier])),locVars_Syn_Rules :: !(([Identifier])),nAutoRules_Syn_Rules :: !(Int),nExplicitRules_Syn_Rules :: !(Int)}
wrap_Rules :: T_Rules  ->
              Inh_Rules  ->
              Syn_Rules 
wrap_Rules (T_Rules sem ) (Inh_Rules _lhsIallTypeSigs _lhsIallfields _lhsIallnts _lhsIaltAttrs _lhsIattrs _lhsIchildInhs _lhsIchildNts _lhsIcon _lhsIinh _lhsIinhsOfChildren _lhsImergeMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_wantvisit _lhsIprefix _lhsIsyn _lhsIsynsOfChildren )  =
    (let ( _lhsOdirectDep,_lhsOerrors,_lhsOgathAltAttrs,_lhsOgathRules,_lhsOinstDep,_lhsOinstVars,_lhsOlocVars,_lhsOnAutoRules,_lhsOnExplicitRules) = sem _lhsIallTypeSigs _lhsIallfields _lhsIallnts _lhsIaltAttrs _lhsIattrs _lhsIchildInhs _lhsIchildNts _lhsIcon _lhsIinh _lhsIinhsOfChildren _lhsImergeMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_wantvisit _lhsIprefix _lhsIsyn _lhsIsynsOfChildren 
     in  (Syn_Rules _lhsOdirectDep _lhsOerrors _lhsOgathAltAttrs _lhsOgathRules _lhsOinstDep _lhsOinstVars _lhsOlocVars _lhsOnAutoRules _lhsOnExplicitRules ))
sem_Rules_Cons :: T_Rule  ->
                  T_Rules  ->
                  T_Rules 
sem_Rules_Cons (T_Rule hd_ ) (T_Rules tl_ )  =
    (T_Rules (\ _lhsIallTypeSigs
                _lhsIallfields
                _lhsIallnts
                _lhsIaltAttrs
                _lhsIattrs
                _lhsIchildInhs
                _lhsIchildNts
                _lhsIcon
                _lhsIinh
                _lhsIinhsOfChildren
                _lhsImergeMap
                _lhsInt
                _lhsIo_case
                _lhsIo_cata
                _lhsIo_dovisit
                _lhsIo_newtypes
                _lhsIo_rename
                _lhsIo_sem
                _lhsIo_sig
                _lhsIo_wantvisit
                _lhsIprefix
                _lhsIsyn
                _lhsIsynsOfChildren ->
                  (let _lhsOdirectDep :: (Seq Edge)
                       _lhsOerrors :: (Seq Error)
                       _lhsOgathAltAttrs :: ([AltAttr])
                       _lhsOgathRules :: (Seq CRule)
                       _lhsOinstDep :: (Seq Edge)
                       _lhsOinstVars :: ([Identifier])
                       _lhsOlocVars :: ([Identifier])
                       _lhsOnAutoRules :: Int
                       _lhsOnExplicitRules :: Int
                       _hdOallTypeSigs :: (Map Identifier Type)
                       _hdOallfields :: ([(Identifier,Type,ChildKind)])
                       _hdOallnts :: ([Identifier])
                       _hdOaltAttrs :: (Map AltAttr Vertex)
                       _hdOattrs :: ([(Identifier,Identifier)])
                       _hdOchildInhs :: (Map Identifier Attributes)
                       _hdOchildNts :: (Map Identifier NontermIdent)
                       _hdOcon :: Identifier
                       _hdOinh :: Attributes
                       _hdOinhsOfChildren :: (Map Identifier Attributes)
                       _hdOmergeMap :: (Map Identifier (Identifier,[Identifier]))
                       _hdOnt :: Identifier
                       _hdOo_case :: Bool
                       _hdOo_cata :: Bool
                       _hdOo_dovisit :: Bool
                       _hdOo_newtypes :: Bool
                       _hdOo_rename :: Bool
                       _hdOo_sem :: Bool
                       _hdOo_sig :: Bool
                       _hdOo_wantvisit :: Bool
                       _hdOprefix :: String
                       _hdOsyn :: Attributes
                       _hdOsynsOfChildren :: (Map Identifier Attributes)
                       _tlOallTypeSigs :: (Map Identifier Type)
                       _tlOallfields :: ([(Identifier,Type,ChildKind)])
                       _tlOallnts :: ([Identifier])
                       _tlOaltAttrs :: (Map AltAttr Vertex)
                       _tlOattrs :: ([(Identifier,Identifier)])
                       _tlOchildInhs :: (Map Identifier Attributes)
                       _tlOchildNts :: (Map Identifier NontermIdent)
                       _tlOcon :: Identifier
                       _tlOinh :: Attributes
                       _tlOinhsOfChildren :: (Map Identifier Attributes)
                       _tlOmergeMap :: (Map Identifier (Identifier,[Identifier]))
                       _tlOnt :: Identifier
                       _tlOo_case :: Bool
                       _tlOo_cata :: Bool
                       _tlOo_dovisit :: Bool
                       _tlOo_newtypes :: Bool
                       _tlOo_rename :: Bool
                       _tlOo_sem :: Bool
                       _tlOo_sig :: Bool
                       _tlOo_wantvisit :: Bool
                       _tlOprefix :: String
                       _tlOsyn :: Attributes
                       _tlOsynsOfChildren :: (Map Identifier Attributes)
                       _hdIdirectDep :: (Seq Edge)
                       _hdIerrors :: (Seq Error)
                       _hdIgathAltAttrs :: ([AltAttr])
                       _hdIgathRules :: (Seq CRule)
                       _hdIinstDep :: (Seq Edge)
                       _hdIinstVars :: ([Identifier])
                       _hdIlocVars :: ([Identifier])
                       _hdInAutoRules :: Int
                       _hdInExplicitRules :: Int
                       _tlIdirectDep :: (Seq Edge)
                       _tlIerrors :: (Seq Error)
                       _tlIgathAltAttrs :: ([AltAttr])
                       _tlIgathRules :: (Seq CRule)
                       _tlIinstDep :: (Seq Edge)
                       _tlIinstVars :: ([Identifier])
                       _tlIlocVars :: ([Identifier])
                       _tlInAutoRules :: Int
                       _tlInExplicitRules :: Int
                       -- use rule "src-ag/Order.ag"(line 267, column 33)
                       _lhsOdirectDep =
                           ({-# LINE 267 "src-ag/Order.ag" #-}
                            _hdIdirectDep Seq.>< _tlIdirectDep
                            {-# LINE 5200 "src-ag/Order.hs" #-}
                            )
                       -- use rule "src-ag/Order.ag"(line 84, column 70)
                       _lhsOerrors =
                           ({-# LINE 84 "src-ag/Order.ag" #-}
                            _hdIerrors Seq.>< _tlIerrors
                            {-# LINE 5206 "src-ag/Order.hs" #-}
                            )
                       -- use rule "src-ag/Order.ag"(line 170, column 68)
                       _lhsOgathAltAttrs =
                           ({-# LINE 170 "src-ag/Order.ag" #-}
                            _hdIgathAltAttrs ++ _tlIgathAltAttrs
                            {-# LINE 5212 "src-ag/Order.hs" #-}
                            )
                       -- use rule "src-ag/Order.ag"(line 206, column 23)
                       _lhsOgathRules =
                           ({-# LINE 206 "src-ag/Order.ag" #-}
                            _hdIgathRules Seq.>< _tlIgathRules
                            {-# LINE 5218 "src-ag/Order.hs" #-}
                            )
                       -- use rule "src-ag/Order.ag"(line 310, column 31)
                       _lhsOinstDep =
                           ({-# LINE 310 "src-ag/Order.ag" #-}
                            _hdIinstDep Seq.>< _tlIinstDep
                            {-# LINE 5224 "src-ag/Order.hs" #-}
                            )
                       -- use rule "src-ag/Order.ag"(line 677, column 86)
                       _lhsOinstVars =
                           ({-# LINE 677 "src-ag/Order.ag" #-}
                            _hdIinstVars ++ _tlIinstVars
                            {-# LINE 5230 "src-ag/Order.hs" #-}
                            )
                       -- use rule "src-ag/Order.ag"(line 677, column 48)
                       _lhsOlocVars =
                           ({-# LINE 677 "src-ag/Order.ag" #-}
                            _hdIlocVars ++ _tlIlocVars
                            {-# LINE 5236 "src-ag/Order.hs" #-}
                            )
                       -- use rule "src-ag/Order.ag"(line 61, column 105)
                       _lhsOnAutoRules =
                           ({-# LINE 61 "src-ag/Order.ag" #-}
                            _hdInAutoRules + _tlInAutoRules
                            {-# LINE 5242 "src-ag/Order.hs" #-}
                            )
                       -- use rule "src-ag/Order.ag"(line 61, column 105)
                       _lhsOnExplicitRules =
                           ({-# LINE 61 "src-ag/Order.ag" #-}
                            _hdInExplicitRules + _tlInExplicitRules
                            {-# LINE 5248 "src-ag/Order.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOallTypeSigs =
                           ({-# LINE 533 "src-ag/Order.ag" #-}
                            _lhsIallTypeSigs
                            {-# LINE 5254 "src-ag/Order.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOallfields =
                           ({-# LINE 654 "src-ag/Order.ag" #-}
                            _lhsIallfields
                            {-# LINE 5260 "src-ag/Order.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOallnts =
                           ({-# LINE 647 "src-ag/Order.ag" #-}
                            _lhsIallnts
                            {-# LINE 5266 "src-ag/Order.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOaltAttrs =
                           ({-# LINE 186 "src-ag/Order.ag" #-}
                            _lhsIaltAttrs
                            {-# LINE 5272 "src-ag/Order.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOattrs =
                           ({-# LINE 654 "src-ag/Order.ag" #-}
                            _lhsIattrs
                            {-# LINE 5278 "src-ag/Order.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOchildInhs =
                           ({-# LINE 199 "src-ag/Order.ag" #-}
                            _lhsIchildInhs
                            {-# LINE 5284 "src-ag/Order.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOchildNts =
                           ({-# LINE 198 "src-ag/Order.ag" #-}
                            _lhsIchildNts
                            {-# LINE 5290 "src-ag/Order.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOcon =
                           ({-# LINE 90 "src-ag/Order.ag" #-}
                            _lhsIcon
                            {-# LINE 5296 "src-ag/Order.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOinh =
                           ({-# LINE 89 "src-ag/Order.ag" #-}
                            _lhsIinh
                            {-# LINE 5302 "src-ag/Order.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOinhsOfChildren =
                           ({-# LINE 337 "src-ag/Order.ag" #-}
                            _lhsIinhsOfChildren
                            {-# LINE 5308 "src-ag/Order.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOmergeMap =
                           ({-# LINE 360 "src-ag/Order.ag" #-}
                            _lhsImergeMap
                            {-# LINE 5314 "src-ag/Order.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOnt =
                           ({-# LINE 89 "src-ag/Order.ag" #-}
                            _lhsInt
                            {-# LINE 5320 "src-ag/Order.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOo_case =
                           ({-# LINE 117 "src-ag/Order.ag" #-}
                            _lhsIo_case
                            {-# LINE 5326 "src-ag/Order.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOo_cata =
                           ({-# LINE 111 "src-ag/Order.ag" #-}
                            _lhsIo_cata
                            {-# LINE 5332 "src-ag/Order.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOo_dovisit =
                           ({-# LINE 116 "src-ag/Order.ag" #-}
                            _lhsIo_dovisit
                            {-# LINE 5338 "src-ag/Order.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOo_newtypes =
                           ({-# LINE 110 "src-ag/Order.ag" #-}
                            _lhsIo_newtypes
                            {-# LINE 5344 "src-ag/Order.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOo_rename =
                           ({-# LINE 114 "src-ag/Order.ag" #-}
                            _lhsIo_rename
                            {-# LINE 5350 "src-ag/Order.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOo_sem =
                           ({-# LINE 113 "src-ag/Order.ag" #-}
                            _lhsIo_sem
                            {-# LINE 5356 "src-ag/Order.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOo_sig =
                           ({-# LINE 112 "src-ag/Order.ag" #-}
                            _lhsIo_sig
                            {-# LINE 5362 "src-ag/Order.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOo_wantvisit =
                           ({-# LINE 115 "src-ag/Order.ag" #-}
                            _lhsIo_wantvisit
                            {-# LINE 5368 "src-ag/Order.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOprefix =
                           ({-# LINE 118 "src-ag/Order.ag" #-}
                            _lhsIprefix
                            {-# LINE 5374 "src-ag/Order.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOsyn =
                           ({-# LINE 89 "src-ag/Order.ag" #-}
                            _lhsIsyn
                            {-# LINE 5380 "src-ag/Order.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOsynsOfChildren =
                           ({-# LINE 337 "src-ag/Order.ag" #-}
                            _lhsIsynsOfChildren
                            {-# LINE 5386 "src-ag/Order.hs" #-}
                            )
                       -- copy rule (down)
                       _tlOallTypeSigs =
                           ({-# LINE 533 "src-ag/Order.ag" #-}
                            _lhsIallTypeSigs
                            {-# LINE 5392 "src-ag/Order.hs" #-}
                            )
                       -- copy rule (down)
                       _tlOallfields =
                           ({-# LINE 654 "src-ag/Order.ag" #-}
                            _lhsIallfields
                            {-# LINE 5398 "src-ag/Order.hs" #-}
                            )
                       -- copy rule (down)
                       _tlOallnts =
                           ({-# LINE 647 "src-ag/Order.ag" #-}
                            _lhsIallnts
                            {-# LINE 5404 "src-ag/Order.hs" #-}
                            )
                       -- copy rule (down)
                       _tlOaltAttrs =
                           ({-# LINE 186 "src-ag/Order.ag" #-}
                            _lhsIaltAttrs
                            {-# LINE 5410 "src-ag/Order.hs" #-}
                            )
                       -- copy rule (down)
                       _tlOattrs =
                           ({-# LINE 654 "src-ag/Order.ag" #-}
                            _lhsIattrs
                            {-# LINE 5416 "src-ag/Order.hs" #-}
                            )
                       -- copy rule (down)
                       _tlOchildInhs =
                           ({-# LINE 199 "src-ag/Order.ag" #-}
                            _lhsIchildInhs
                            {-# LINE 5422 "src-ag/Order.hs" #-}
                            )
                       -- copy rule (down)
                       _tlOchildNts =
                           ({-# LINE 198 "src-ag/Order.ag" #-}
                            _lhsIchildNts
                            {-# LINE 5428 "src-ag/Order.hs" #-}
                            )
                       -- copy rule (down)
                       _tlOcon =
                           ({-# LINE 90 "src-ag/Order.ag" #-}
                            _lhsIcon
                            {-# LINE 5434 "src-ag/Order.hs" #-}
                            )
                       -- copy rule (down)
                       _tlOinh =
                           ({-# LINE 89 "src-ag/Order.ag" #-}
                            _lhsIinh
                            {-# LINE 5440 "src-ag/Order.hs" #-}
                            )
                       -- copy rule (down)
                       _tlOinhsOfChildren =
                           ({-# LINE 337 "src-ag/Order.ag" #-}
                            _lhsIinhsOfChildren
                            {-# LINE 5446 "src-ag/Order.hs" #-}
                            )
                       -- copy rule (down)
                       _tlOmergeMap =
                           ({-# LINE 360 "src-ag/Order.ag" #-}
                            _lhsImergeMap
                            {-# LINE 5452 "src-ag/Order.hs" #-}
                            )
                       -- copy rule (down)
                       _tlOnt =
                           ({-# LINE 89 "src-ag/Order.ag" #-}
                            _lhsInt
                            {-# LINE 5458 "src-ag/Order.hs" #-}
                            )
                       -- copy rule (down)
                       _tlOo_case =
                           ({-# LINE 117 "src-ag/Order.ag" #-}
                            _lhsIo_case
                            {-# LINE 5464 "src-ag/Order.hs" #-}
                            )
                       -- copy rule (down)
                       _tlOo_cata =
                           ({-# LINE 111 "src-ag/Order.ag" #-}
                            _lhsIo_cata
                            {-# LINE 5470 "src-ag/Order.hs" #-}
                            )
                       -- copy rule (down)
                       _tlOo_dovisit =
                           ({-# LINE 116 "src-ag/Order.ag" #-}
                            _lhsIo_dovisit
                            {-# LINE 5476 "src-ag/Order.hs" #-}
                            )
                       -- copy rule (down)
                       _tlOo_newtypes =
                           ({-# LINE 110 "src-ag/Order.ag" #-}
                            _lhsIo_newtypes
                            {-# LINE 5482 "src-ag/Order.hs" #-}
                            )
                       -- copy rule (down)
                       _tlOo_rename =
                           ({-# LINE 114 "src-ag/Order.ag" #-}
                            _lhsIo_rename
                            {-# LINE 5488 "src-ag/Order.hs" #-}
                            )
                       -- copy rule (down)
                       _tlOo_sem =
                           ({-# LINE 113 "src-ag/Order.ag" #-}
                            _lhsIo_sem
                            {-# LINE 5494 "src-ag/Order.hs" #-}
                            )
                       -- copy rule (down)
                       _tlOo_sig =
                           ({-# LINE 112 "src-ag/Order.ag" #-}
                            _lhsIo_sig
                            {-# LINE 5500 "src-ag/Order.hs" #-}
                            )
                       -- copy rule (down)
                       _tlOo_wantvisit =
                           ({-# LINE 115 "src-ag/Order.ag" #-}
                            _lhsIo_wantvisit
                            {-# LINE 5506 "src-ag/Order.hs" #-}
                            )
                       -- copy rule (down)
                       _tlOprefix =
                           ({-# LINE 118 "src-ag/Order.ag" #-}
                            _lhsIprefix
                            {-# LINE 5512 "src-ag/Order.hs" #-}
                            )
                       -- copy rule (down)
                       _tlOsyn =
                           ({-# LINE 89 "src-ag/Order.ag" #-}
                            _lhsIsyn
                            {-# LINE 5518 "src-ag/Order.hs" #-}
                            )
                       -- copy rule (down)
                       _tlOsynsOfChildren =
                           ({-# LINE 337 "src-ag/Order.ag" #-}
                            _lhsIsynsOfChildren
                            {-# LINE 5524 "src-ag/Order.hs" #-}
                            )
                       ( _hdIdirectDep,_hdIerrors,_hdIgathAltAttrs,_hdIgathRules,_hdIinstDep,_hdIinstVars,_hdIlocVars,_hdInAutoRules,_hdInExplicitRules) =
                           hd_ _hdOallTypeSigs _hdOallfields _hdOallnts _hdOaltAttrs _hdOattrs _hdOchildInhs _hdOchildNts _hdOcon _hdOinh _hdOinhsOfChildren _hdOmergeMap _hdOnt _hdOo_case _hdOo_cata _hdOo_dovisit _hdOo_newtypes _hdOo_rename _hdOo_sem _hdOo_sig _hdOo_wantvisit _hdOprefix _hdOsyn _hdOsynsOfChildren 
                       ( _tlIdirectDep,_tlIerrors,_tlIgathAltAttrs,_tlIgathRules,_tlIinstDep,_tlIinstVars,_tlIlocVars,_tlInAutoRules,_tlInExplicitRules) =
                           tl_ _tlOallTypeSigs _tlOallfields _tlOallnts _tlOaltAttrs _tlOattrs _tlOchildInhs _tlOchildNts _tlOcon _tlOinh _tlOinhsOfChildren _tlOmergeMap _tlOnt _tlOo_case _tlOo_cata _tlOo_dovisit _tlOo_newtypes _tlOo_rename _tlOo_sem _tlOo_sig _tlOo_wantvisit _tlOprefix _tlOsyn _tlOsynsOfChildren 
                   in  ( _lhsOdirectDep,_lhsOerrors,_lhsOgathAltAttrs,_lhsOgathRules,_lhsOinstDep,_lhsOinstVars,_lhsOlocVars,_lhsOnAutoRules,_lhsOnExplicitRules))) )
sem_Rules_Nil :: T_Rules 
sem_Rules_Nil  =
    (T_Rules (\ _lhsIallTypeSigs
                _lhsIallfields
                _lhsIallnts
                _lhsIaltAttrs
                _lhsIattrs
                _lhsIchildInhs
                _lhsIchildNts
                _lhsIcon
                _lhsIinh
                _lhsIinhsOfChildren
                _lhsImergeMap
                _lhsInt
                _lhsIo_case
                _lhsIo_cata
                _lhsIo_dovisit
                _lhsIo_newtypes
                _lhsIo_rename
                _lhsIo_sem
                _lhsIo_sig
                _lhsIo_wantvisit
                _lhsIprefix
                _lhsIsyn
                _lhsIsynsOfChildren ->
                  (let _lhsOdirectDep :: (Seq Edge)
                       _lhsOerrors :: (Seq Error)
                       _lhsOgathAltAttrs :: ([AltAttr])
                       _lhsOgathRules :: (Seq CRule)
                       _lhsOinstDep :: (Seq Edge)
                       _lhsOinstVars :: ([Identifier])
                       _lhsOlocVars :: ([Identifier])
                       _lhsOnAutoRules :: Int
                       _lhsOnExplicitRules :: Int
                       -- use rule "src-ag/Order.ag"(line 267, column 33)
                       _lhsOdirectDep =
                           ({-# LINE 267 "src-ag/Order.ag" #-}
                            Seq.empty
                            {-# LINE 5569 "src-ag/Order.hs" #-}
                            )
                       -- use rule "src-ag/Order.ag"(line 84, column 70)
                       _lhsOerrors =
                           ({-# LINE 84 "src-ag/Order.ag" #-}
                            Seq.empty
                            {-# LINE 5575 "src-ag/Order.hs" #-}
                            )
                       -- use rule "src-ag/Order.ag"(line 170, column 68)
                       _lhsOgathAltAttrs =
                           ({-# LINE 170 "src-ag/Order.ag" #-}
                            []
                            {-# LINE 5581 "src-ag/Order.hs" #-}
                            )
                       -- use rule "src-ag/Order.ag"(line 206, column 23)
                       _lhsOgathRules =
                           ({-# LINE 206 "src-ag/Order.ag" #-}
                            Seq.empty
                            {-# LINE 5587 "src-ag/Order.hs" #-}
                            )
                       -- use rule "src-ag/Order.ag"(line 310, column 31)
                       _lhsOinstDep =
                           ({-# LINE 310 "src-ag/Order.ag" #-}
                            Seq.empty
                            {-# LINE 5593 "src-ag/Order.hs" #-}
                            )
                       -- use rule "src-ag/Order.ag"(line 677, column 86)
                       _lhsOinstVars =
                           ({-# LINE 677 "src-ag/Order.ag" #-}
                            []
                            {-# LINE 5599 "src-ag/Order.hs" #-}
                            )
                       -- use rule "src-ag/Order.ag"(line 677, column 48)
                       _lhsOlocVars =
                           ({-# LINE 677 "src-ag/Order.ag" #-}
                            []
                            {-# LINE 5605 "src-ag/Order.hs" #-}
                            )
                       -- use rule "src-ag/Order.ag"(line 61, column 105)
                       _lhsOnAutoRules =
                           ({-# LINE 61 "src-ag/Order.ag" #-}
                            0
                            {-# LINE 5611 "src-ag/Order.hs" #-}
                            )
                       -- use rule "src-ag/Order.ag"(line 61, column 105)
                       _lhsOnExplicitRules =
                           ({-# LINE 61 "src-ag/Order.ag" #-}
                            0
                            {-# LINE 5617 "src-ag/Order.hs" #-}
                            )
                   in  ( _lhsOdirectDep,_lhsOerrors,_lhsOgathAltAttrs,_lhsOgathRules,_lhsOinstDep,_lhsOinstVars,_lhsOlocVars,_lhsOnAutoRules,_lhsOnExplicitRules))) )
-- TypeSig -----------------------------------------------------
{-
   visit 0:
      chained attribute:
         typeSigs             : Map Identifier Type
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
newtype T_TypeSig  = T_TypeSig ((Map Identifier Type) ->
                                ( (Map Identifier Type)))
data Inh_TypeSig  = Inh_TypeSig {typeSigs_Inh_TypeSig :: !((Map Identifier Type))}
data Syn_TypeSig  = Syn_TypeSig {typeSigs_Syn_TypeSig :: !((Map Identifier Type))}
wrap_TypeSig :: T_TypeSig  ->
                Inh_TypeSig  ->
                Syn_TypeSig 
wrap_TypeSig (T_TypeSig sem ) (Inh_TypeSig _lhsItypeSigs )  =
    (let ( _lhsOtypeSigs) = sem _lhsItypeSigs 
     in  (Syn_TypeSig _lhsOtypeSigs ))
sem_TypeSig_TypeSig :: Identifier ->
                       Type ->
                       T_TypeSig 
sem_TypeSig_TypeSig name_ tp_  =
    (T_TypeSig (\ _lhsItypeSigs ->
                    (let _lhsOtypeSigs :: (Map Identifier Type)
                         -- "src-ag/Order.ag"(line 531, column 13)
                         _lhsOtypeSigs =
                             ({-# LINE 531 "src-ag/Order.ag" #-}
                              Map.insert name_ tp_ _lhsItypeSigs
                              {-# LINE 5656 "src-ag/Order.hs" #-}
                              )
                     in  ( _lhsOtypeSigs))) )
-- TypeSigs ----------------------------------------------------
{-
   visit 0:
      chained attribute:
         typeSigs             : Map Identifier Type
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
newtype T_TypeSigs  = T_TypeSigs ((Map Identifier Type) ->
                                  ( (Map Identifier Type)))
data Inh_TypeSigs  = Inh_TypeSigs {typeSigs_Inh_TypeSigs :: !((Map Identifier Type))}
data Syn_TypeSigs  = Syn_TypeSigs {typeSigs_Syn_TypeSigs :: !((Map Identifier Type))}
wrap_TypeSigs :: T_TypeSigs  ->
                 Inh_TypeSigs  ->
                 Syn_TypeSigs 
wrap_TypeSigs (T_TypeSigs sem ) (Inh_TypeSigs _lhsItypeSigs )  =
    (let ( _lhsOtypeSigs) = sem _lhsItypeSigs 
     in  (Syn_TypeSigs _lhsOtypeSigs ))
sem_TypeSigs_Cons :: T_TypeSig  ->
                     T_TypeSigs  ->
                     T_TypeSigs 
sem_TypeSigs_Cons (T_TypeSig hd_ ) (T_TypeSigs tl_ )  =
    (T_TypeSigs (\ _lhsItypeSigs ->
                     (let _lhsOtypeSigs :: (Map Identifier Type)
                          _hdOtypeSigs :: (Map Identifier Type)
                          _tlOtypeSigs :: (Map Identifier Type)
                          _hdItypeSigs :: (Map Identifier Type)
                          _tlItypeSigs :: (Map Identifier Type)
                          -- copy rule (up)
                          _lhsOtypeSigs =
                              ({-# LINE 527 "src-ag/Order.ag" #-}
                               _tlItypeSigs
                               {-# LINE 5700 "src-ag/Order.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOtypeSigs =
                              ({-# LINE 527 "src-ag/Order.ag" #-}
                               _lhsItypeSigs
                               {-# LINE 5706 "src-ag/Order.hs" #-}
                               )
                          -- copy rule (chain)
                          _tlOtypeSigs =
                              ({-# LINE 527 "src-ag/Order.ag" #-}
                               _hdItypeSigs
                               {-# LINE 5712 "src-ag/Order.hs" #-}
                               )
                          ( _hdItypeSigs) =
                              hd_ _hdOtypeSigs 
                          ( _tlItypeSigs) =
                              tl_ _tlOtypeSigs 
                      in  ( _lhsOtypeSigs))) )
sem_TypeSigs_Nil :: T_TypeSigs 
sem_TypeSigs_Nil  =
    (T_TypeSigs (\ _lhsItypeSigs ->
                     (let _lhsOtypeSigs :: (Map Identifier Type)
                          -- copy rule (chain)
                          _lhsOtypeSigs =
                              ({-# LINE 527 "src-ag/Order.ag" #-}
                               _lhsItypeSigs
                               {-# LINE 5727 "src-ag/Order.hs" #-}
                               )
                      in  ( _lhsOtypeSigs))) )