{-# LANGUAGE Rank2Types, GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TfmToMirage where
{-# LINE 2 "src-ag/Patterns.ag" #-}

-- Patterns.ag imports
import UU.Scanner.Position(Pos)
import CommonTypes (ConstructorIdent,Identifier)
{-# LINE 11 "src-generated/TfmToMirage.hs" #-}

{-# LINE 10 "src-ag/Order.ag" #-}

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
{-# LINE 48 "src-generated/TfmToMirage.hs" #-}

{-# LINE 2 "src-ag/Expression.ag" #-}

import UU.Scanner.Position(Pos)
import HsToken
{-# LINE 54 "src-generated/TfmToMirage.hs" #-}

{-# LINE 2 "src-ag/AbstractSyntax.ag" #-}

-- AbstractSyntax.ag imports
import Data.Set(Set)
import Data.Map(Map)
import Patterns    (Pattern(..),Patterns)
import Expression  (Expression(..))
import Macro --marcos
import CommonTypes
import ErrorMessages
{-# LINE 66 "src-generated/TfmToMirage.hs" #-}

{-# LINE 5 "src-ag/TfmToMirage.ag" #-}

import AbstractSyntax
import qualified Data.Map as Map
import Pretty
import TokenDef
import qualified MirageSyntax as Mirage
{-# LINE 75 "src-generated/TfmToMirage.hs" #-}
import Control.Monad.Identity (Identity)
import qualified Control.Monad.Identity
{-# LINE 46 "src-ag/Order.ag" #-}

-- Terminates with an error if the key is not in the map
findWithErr1 :: (Ord k, Show k) => String -> k -> Map k a -> a
findWithErr1 s k
  = Map.findWithDefault (error ("findWithErr1 " ++ s ++ ": key " ++ show k ++ " not in map.")) k

findWithErr2 :: (Ord k, Show k, Show a) => k -> Map k a -> a
findWithErr2 k m
  = Map.findWithDefault (error ("findWithErr2: key " ++ show k ++ " not in map: " ++ show m)) k m
{-# LINE 88 "src-generated/TfmToMirage.hs" #-}

{-# LINE 71 "src-ag/Order.ag" #-}

startsWith :: String -> String -> Bool
startsWith k h = k == take (length k) h
{-# LINE 94 "src-generated/TfmToMirage.hs" #-}

{-# LINE 138 "src-ag/Order.ag" #-}

getNtName :: Type -> NontermIdent
getNtName (NT nt _ _) = nt
getNtName _           = nullIdent
{-# LINE 101 "src-generated/TfmToMirage.hs" #-}

{-# LINE 166 "src-ag/Order.ag" #-}

data AltAttr = AltAttr Identifier Identifier Bool
               deriving (Eq, Ord, Show)
{-# LINE 107 "src-generated/TfmToMirage.hs" #-}

{-# LINE 239 "src-ag/Order.ag" #-}

substSelf nt tp
  = case tp of
      NT n tps defor | n == _SELF -> NT nt tps defor
      _                           -> tp

haskellTupel :: [Type] -> Maybe Type
haskellTupel ts =  Just ( Haskell ( '(' : (concat (intersperse "," (map show ts))) ++ ")" ))
{-# LINE 118 "src-generated/TfmToMirage.hs" #-}

{-# LINE 692 "src-ag/Order.ag" #-}

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
{-# LINE 238 "src-generated/TfmToMirage.hs" #-}

{-# LINE 13 "src-ag/TfmToMirage.ag" #-}

typeToMirage :: Type -> Mirage.Type
typeToMirage x = case x of
  Haskell y -> Mirage.Haskell y
  NT y ys _ -> Mirage.NT (getName y) ys
  Self      -> Mirage.Self
{-# LINE 247 "src-generated/TfmToMirage.hs" #-}
-- Child -------------------------------------------------------
-- wrapper
data Inh_Child  = Inh_Child { allfields_Inh_Child :: ([(Identifier,Type,ChildKind)]), allnts_Inh_Child :: ([Identifier]), attrs_Inh_Child :: ([(Identifier,Identifier)]), con_Inh_Child :: (Identifier), inh_Inh_Child :: (Attributes), inhMap_Inh_Child :: (Map Identifier Attributes), mergeMap_Inh_Child :: (Map Identifier (Identifier,[Identifier])), nt_Inh_Child :: (Identifier), o_unbox_Inh_Child :: (Bool), syn_Inh_Child :: (Attributes), synMap_Inh_Child :: (Map Identifier Attributes) }
data Syn_Child  = Syn_Child { attributes_Syn_Child :: ([(Identifier,Attributes,Attributes)]), collectChildrenInhs_Syn_Child :: (Map Identifier Attributes ), collectChildrenSyns_Syn_Child :: (Map Identifier Attributes ), errors_Syn_Child :: (Seq Error), field_Syn_Child :: ((Identifier,Type,ChildKind)), gathAltAttrs_Syn_Child :: ([AltAttr]), gathRules_Syn_Child :: (Seq CRule), inhs_Syn_Child :: (Seq (Identifier,Attributes)), mirage_Syn_Child :: (Mirage.Child), nts_Syn_Child :: (Seq (Identifier,NontermIdent)), singlevisits_Syn_Child :: ([CRule]), terminals_Syn_Child :: ([Identifier]) }
{-# INLINABLE wrap_Child #-}
wrap_Child :: T_Child  -> Inh_Child  -> (Syn_Child )
wrap_Child (T_Child act) (Inh_Child _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsIinh _lhsIinhMap _lhsImergeMap _lhsInt _lhsIo_unbox _lhsIsyn _lhsIsynMap) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg1 = T_Child_vIn1 _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsIinh _lhsIinhMap _lhsImergeMap _lhsInt _lhsIo_unbox _lhsIsyn _lhsIsynMap
        (T_Child_vOut1 _lhsOattributes _lhsOcollectChildrenInhs _lhsOcollectChildrenSyns _lhsOerrors _lhsOfield _lhsOgathAltAttrs _lhsOgathRules _lhsOinhs _lhsOmirage _lhsOnts _lhsOsinglevisits _lhsOterminals) <- return (inv_Child_s2 sem arg1)
        return (Syn_Child _lhsOattributes _lhsOcollectChildrenInhs _lhsOcollectChildrenSyns _lhsOerrors _lhsOfield _lhsOgathAltAttrs _lhsOgathRules _lhsOinhs _lhsOmirage _lhsOnts _lhsOsinglevisits _lhsOterminals)
   )

-- cata
{-# INLINE sem_Child #-}
sem_Child :: Child  -> T_Child 
sem_Child ( Child name_ tp_ kind_ ) = sem_Child_Child name_ tp_ kind_

-- semantic domain
newtype T_Child  = T_Child {
                           attach_T_Child :: Identity (T_Child_s2 )
                           }
newtype T_Child_s2  = C_Child_s2 {
                                 inv_Child_s2 :: (T_Child_v1 )
                                 }
data T_Child_s3  = C_Child_s3
type T_Child_v1  = (T_Child_vIn1 ) -> (T_Child_vOut1 )
data T_Child_vIn1  = T_Child_vIn1 ([(Identifier,Type,ChildKind)]) ([Identifier]) ([(Identifier,Identifier)]) (Identifier) (Attributes) (Map Identifier Attributes) (Map Identifier (Identifier,[Identifier])) (Identifier) (Bool) (Attributes) (Map Identifier Attributes)
data T_Child_vOut1  = T_Child_vOut1 ([(Identifier,Attributes,Attributes)]) (Map Identifier Attributes ) (Map Identifier Attributes ) (Seq Error) ((Identifier,Type,ChildKind)) ([AltAttr]) (Seq CRule) (Seq (Identifier,Attributes)) (Mirage.Child) (Seq (Identifier,NontermIdent)) ([CRule]) ([Identifier])
{-# NOINLINE sem_Child_Child #-}
sem_Child_Child :: (Identifier) -> (Type) -> (ChildKind) -> T_Child 
sem_Child_Child arg_name_ arg_tp_ arg_kind_ = T_Child (return st2) where
   {-# NOINLINE st2 #-}
   st2 = let
      v1 :: T_Child_v1 
      v1 = \ (T_Child_vIn1 _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsIinh _lhsIinhMap _lhsImergeMap _lhsInt _lhsIo_unbox _lhsIsyn _lhsIsynMap) -> ( let
         _chnt = rule0 arg_name_ arg_tp_
         _inh = rule1 _chnt _lhsIinhMap
         _syn = rule2 _chnt _lhsIsynMap
         _maptolocal = rule3 _syn arg_tp_
         _lhsOgathAltAttrs :: [AltAttr]
         _lhsOgathAltAttrs = rule4 _maptolocal _syn arg_name_
         _lhsOnts :: Seq (Identifier,NontermIdent)
         _lhsOnts = rule5 arg_name_ arg_tp_
         _lhsOinhs :: Seq (Identifier,Attributes)
         _lhsOinhs = rule6 _inh arg_name_
         _gathRules = rule7 _lhsIcon _lhsInt _maptolocal _syn arg_name_ arg_tp_
         _lhsOcollectChildrenSyns :: Map Identifier Attributes 
         _lhsOcollectChildrenSyns = rule8 _syn arg_name_
         _lhsOcollectChildrenInhs :: Map Identifier Attributes 
         _lhsOcollectChildrenInhs = rule9 _inh arg_name_
         _lhsOsinglevisits :: [CRule]
         _lhsOsinglevisits = rule10 _inh _maptolocal _syn arg_name_ arg_tp_
         _lhsOterminals :: [Identifier]
         _lhsOterminals = rule11 _maptolocal arg_name_
         _lhsOattributes :: [(Identifier,Attributes,Attributes)]
         _lhsOattributes = rule12 _inh _syn arg_name_
         _lhsOfield :: (Identifier,Type,ChildKind)
         _lhsOfield = rule13 arg_kind_ arg_name_ arg_tp_
         _lhsOmirage :: Mirage.Child
         _lhsOmirage = rule14 arg_name_ arg_tp_
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule15  ()
         _lhsOgathRules :: Seq CRule
         _lhsOgathRules = rule16 _gathRules
         __result_ = T_Child_vOut1 _lhsOattributes _lhsOcollectChildrenInhs _lhsOcollectChildrenSyns _lhsOerrors _lhsOfield _lhsOgathAltAttrs _lhsOgathRules _lhsOinhs _lhsOmirage _lhsOnts _lhsOsinglevisits _lhsOterminals
         in __result_ )
     in C_Child_s2 v1
   {-# INLINE rule0 #-}
   {-# LINE 19 "src-ag/DistChildAttr.ag" #-}
   rule0 = \ name_ tp_ ->
                       {-# LINE 19 "src-ag/DistChildAttr.ag" #-}
                       case tp_ of
                         NT nt _ _ -> nt
                         Self      -> error ("The type of child " ++ show name_ ++ " should not be a Self type.")
                         Haskell t -> identifier ""
                       {-# LINE 325 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule1 #-}
   {-# LINE 23 "src-ag/DistChildAttr.ag" #-}
   rule1 = \ _chnt ((_lhsIinhMap) :: Map Identifier Attributes) ->
                      {-# LINE 23 "src-ag/DistChildAttr.ag" #-}
                      Map.findWithDefault Map.empty _chnt     _lhsIinhMap
                      {-# LINE 331 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule2 #-}
   {-# LINE 24 "src-ag/DistChildAttr.ag" #-}
   rule2 = \ _chnt ((_lhsIsynMap) :: Map Identifier Attributes) ->
                      {-# LINE 24 "src-ag/DistChildAttr.ag" #-}
                      Map.findWithDefault Map.empty _chnt     _lhsIsynMap
                      {-# LINE 337 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule3 #-}
   {-# LINE 180 "src-ag/Order.ag" #-}
   rule3 = \ _syn tp_ ->
                                {-# LINE 180 "src-ag/Order.ag" #-}
                                case tp_ of
                                  NT nt _ _ -> Map.null _syn
                                  _         -> True
                                {-# LINE 345 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule4 #-}
   {-# LINE 183 "src-ag/Order.ag" #-}
   rule4 = \ _maptolocal _syn name_ ->
                                 {-# LINE 183 "src-ag/Order.ag" #-}
                                 if  _maptolocal
                                     then [ AltAttr _LOC name_ True ]
                                     else [ AltAttr name_ syn True | syn <- Map.keys _syn     ]
                                 {-# LINE 353 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule5 #-}
   {-# LINE 198 "src-ag/Order.ag" #-}
   rule5 = \ name_ tp_ ->
                        {-# LINE 198 "src-ag/Order.ag" #-}
                        Seq.singleton (name_,getNtName tp_)
                        {-# LINE 359 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule6 #-}
   {-# LINE 199 "src-ag/Order.ag" #-}
   rule6 = \ _inh name_ ->
                         {-# LINE 199 "src-ag/Order.ag" #-}
                         Seq.singleton (name_,_inh    )
                         {-# LINE 365 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule7 #-}
   {-# LINE 215 "src-ag/Order.ag" #-}
   rule7 = \ ((_lhsIcon) :: Identifier) ((_lhsInt) :: Identifier) _maptolocal _syn name_ tp_ ->
                              {-# LINE 215 "src-ag/Order.ag" #-}
                              if  _maptolocal
                                  then Seq.singleton (cRuleTerminal name_ _lhsInt _lhsIcon tp_)
                                  else Seq.fromList [ cRuleRhsSyn syn _lhsInt _lhsIcon tp name_ (getNtName tp_) | (syn,tp) <- Map.assocs _syn    ]
                              {-# LINE 373 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule8 #-}
   {-# LINE 347 "src-ag/Order.ag" #-}
   rule8 = \ _syn name_ ->
                                       {-# LINE 347 "src-ag/Order.ag" #-}
                                       Map.singleton name_ _syn
                                       {-# LINE 379 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule9 #-}
   {-# LINE 348 "src-ag/Order.ag" #-}
   rule9 = \ _inh name_ ->
                                       {-# LINE 348 "src-ag/Order.ag" #-}
                                       Map.singleton name_ _inh
                                       {-# LINE 385 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule10 #-}
   {-# LINE 618 "src-ag/Order.ag" #-}
   rule10 = \ _inh _maptolocal _syn name_ tp_ ->
                                 {-# LINE 618 "src-ag/Order.ag" #-}
                                 if  _maptolocal
                                     then []
                                     else [CChildVisit name_ (getNtName tp_) 0 _inh     _syn     True]
                                 {-# LINE 393 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule11 #-}
   {-# LINE 643 "src-ag/Order.ag" #-}
   rule11 = \ _maptolocal name_ ->
                            {-# LINE 643 "src-ag/Order.ag" #-}
                            if _maptolocal
                            then [name_]
                            else []
                            {-# LINE 401 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule12 #-}
   {-# LINE 672 "src-ag/Order.ag" #-}
   rule12 = \ _inh _syn name_ ->
                             {-# LINE 672 "src-ag/Order.ag" #-}
                             [(name_, _inh    , _syn    )]
                             {-# LINE 407 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule13 #-}
   {-# LINE 676 "src-ag/Order.ag" #-}
   rule13 = \ kind_ name_ tp_ ->
                        {-# LINE 676 "src-ag/Order.ag" #-}
                        (name_, tp_, kind_)
                        {-# LINE 413 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule14 #-}
   {-# LINE 50 "src-ag/TfmToMirage.ag" #-}
   rule14 = \ name_ tp_ ->
                         {-# LINE 50 "src-ag/TfmToMirage.ag" #-}
                         Mirage.Child (getName name_) (typeToMirage tp_)
                         {-# LINE 419 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule15 #-}
   rule15 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule16 #-}
   rule16 = \ _gathRules ->
     _gathRules

-- Children ----------------------------------------------------
-- wrapper
data Inh_Children  = Inh_Children { allfields_Inh_Children :: ([(Identifier,Type,ChildKind)]), allnts_Inh_Children :: ([Identifier]), attrs_Inh_Children :: ([(Identifier,Identifier)]), con_Inh_Children :: (Identifier), inh_Inh_Children :: (Attributes), inhMap_Inh_Children :: (Map Identifier Attributes), mergeMap_Inh_Children :: (Map Identifier (Identifier,[Identifier])), nt_Inh_Children :: (Identifier), o_unbox_Inh_Children :: (Bool), syn_Inh_Children :: (Attributes), synMap_Inh_Children :: (Map Identifier Attributes) }
data Syn_Children  = Syn_Children { attributes_Syn_Children :: ([(Identifier,Attributes,Attributes)]), collectChildrenInhs_Syn_Children :: (Map Identifier Attributes ), collectChildrenSyns_Syn_Children :: (Map Identifier Attributes ), errors_Syn_Children :: (Seq Error), fields_Syn_Children :: ([(Identifier,Type,ChildKind)]), gathAltAttrs_Syn_Children :: ([AltAttr]), gathRules_Syn_Children :: (Seq CRule), inhs_Syn_Children :: (Seq (Identifier,Attributes)), mirages_Syn_Children :: ([Mirage.Child]), nts_Syn_Children :: (Seq (Identifier,NontermIdent)), singlevisits_Syn_Children :: ([CRule]), terminals_Syn_Children :: ([Identifier]) }
{-# INLINABLE wrap_Children #-}
wrap_Children :: T_Children  -> Inh_Children  -> (Syn_Children )
wrap_Children (T_Children act) (Inh_Children _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsIinh _lhsIinhMap _lhsImergeMap _lhsInt _lhsIo_unbox _lhsIsyn _lhsIsynMap) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg4 = T_Children_vIn4 _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsIinh _lhsIinhMap _lhsImergeMap _lhsInt _lhsIo_unbox _lhsIsyn _lhsIsynMap
        (T_Children_vOut4 _lhsOattributes _lhsOcollectChildrenInhs _lhsOcollectChildrenSyns _lhsOerrors _lhsOfields _lhsOgathAltAttrs _lhsOgathRules _lhsOinhs _lhsOmirages _lhsOnts _lhsOsinglevisits _lhsOterminals) <- return (inv_Children_s5 sem arg4)
        return (Syn_Children _lhsOattributes _lhsOcollectChildrenInhs _lhsOcollectChildrenSyns _lhsOerrors _lhsOfields _lhsOgathAltAttrs _lhsOgathRules _lhsOinhs _lhsOmirages _lhsOnts _lhsOsinglevisits _lhsOterminals)
   )

-- cata
{-# NOINLINE sem_Children #-}
sem_Children :: Children  -> T_Children 
sem_Children list = Prelude.foldr sem_Children_Cons sem_Children_Nil (Prelude.map sem_Child list)

-- semantic domain
newtype T_Children  = T_Children {
                                 attach_T_Children :: Identity (T_Children_s5 )
                                 }
newtype T_Children_s5  = C_Children_s5 {
                                       inv_Children_s5 :: (T_Children_v4 )
                                       }
data T_Children_s6  = C_Children_s6
type T_Children_v4  = (T_Children_vIn4 ) -> (T_Children_vOut4 )
data T_Children_vIn4  = T_Children_vIn4 ([(Identifier,Type,ChildKind)]) ([Identifier]) ([(Identifier,Identifier)]) (Identifier) (Attributes) (Map Identifier Attributes) (Map Identifier (Identifier,[Identifier])) (Identifier) (Bool) (Attributes) (Map Identifier Attributes)
data T_Children_vOut4  = T_Children_vOut4 ([(Identifier,Attributes,Attributes)]) (Map Identifier Attributes ) (Map Identifier Attributes ) (Seq Error) ([(Identifier,Type,ChildKind)]) ([AltAttr]) (Seq CRule) (Seq (Identifier,Attributes)) ([Mirage.Child]) (Seq (Identifier,NontermIdent)) ([CRule]) ([Identifier])
{-# NOINLINE sem_Children_Cons #-}
sem_Children_Cons :: T_Child  -> T_Children  -> T_Children 
sem_Children_Cons arg_hd_ arg_tl_ = T_Children (return st5) where
   {-# NOINLINE st5 #-}
   st5 = let
      v4 :: T_Children_v4 
      v4 = \ (T_Children_vIn4 _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsIinh _lhsIinhMap _lhsImergeMap _lhsInt _lhsIo_unbox _lhsIsyn _lhsIsynMap) -> ( let
         _hdX2 = Control.Monad.Identity.runIdentity (attach_T_Child (arg_hd_))
         _tlX5 = Control.Monad.Identity.runIdentity (attach_T_Children (arg_tl_))
         (T_Child_vOut1 _hdIattributes _hdIcollectChildrenInhs _hdIcollectChildrenSyns _hdIerrors _hdIfield _hdIgathAltAttrs _hdIgathRules _hdIinhs _hdImirage _hdInts _hdIsinglevisits _hdIterminals) = inv_Child_s2 _hdX2 (T_Child_vIn1 _hdOallfields _hdOallnts _hdOattrs _hdOcon _hdOinh _hdOinhMap _hdOmergeMap _hdOnt _hdOo_unbox _hdOsyn _hdOsynMap)
         (T_Children_vOut4 _tlIattributes _tlIcollectChildrenInhs _tlIcollectChildrenSyns _tlIerrors _tlIfields _tlIgathAltAttrs _tlIgathRules _tlIinhs _tlImirages _tlInts _tlIsinglevisits _tlIterminals) = inv_Children_s5 _tlX5 (T_Children_vIn4 _tlOallfields _tlOallnts _tlOattrs _tlOcon _tlOinh _tlOinhMap _tlOmergeMap _tlOnt _tlOo_unbox _tlOsyn _tlOsynMap)
         _lhsOfields :: [(Identifier,Type,ChildKind)]
         _lhsOfields = rule17 _hdIfield _tlIfields
         _lhsOmirages :: [Mirage.Child]
         _lhsOmirages = rule18 _hdImirage _tlImirages
         _lhsOattributes :: [(Identifier,Attributes,Attributes)]
         _lhsOattributes = rule19 _hdIattributes _tlIattributes
         _lhsOcollectChildrenInhs :: Map Identifier Attributes 
         _lhsOcollectChildrenInhs = rule20 _hdIcollectChildrenInhs _tlIcollectChildrenInhs
         _lhsOcollectChildrenSyns :: Map Identifier Attributes 
         _lhsOcollectChildrenSyns = rule21 _hdIcollectChildrenSyns _tlIcollectChildrenSyns
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule22 _hdIerrors _tlIerrors
         _lhsOgathAltAttrs :: [AltAttr]
         _lhsOgathAltAttrs = rule23 _hdIgathAltAttrs _tlIgathAltAttrs
         _lhsOgathRules :: Seq CRule
         _lhsOgathRules = rule24 _hdIgathRules _tlIgathRules
         _lhsOinhs :: Seq (Identifier,Attributes)
         _lhsOinhs = rule25 _hdIinhs _tlIinhs
         _lhsOnts :: Seq (Identifier,NontermIdent)
         _lhsOnts = rule26 _hdInts _tlInts
         _lhsOsinglevisits :: [CRule]
         _lhsOsinglevisits = rule27 _hdIsinglevisits _tlIsinglevisits
         _lhsOterminals :: [Identifier]
         _lhsOterminals = rule28 _hdIterminals _tlIterminals
         _hdOallfields = rule29 _lhsIallfields
         _hdOallnts = rule30 _lhsIallnts
         _hdOattrs = rule31 _lhsIattrs
         _hdOcon = rule32 _lhsIcon
         _hdOinh = rule33 _lhsIinh
         _hdOinhMap = rule34 _lhsIinhMap
         _hdOmergeMap = rule35 _lhsImergeMap
         _hdOnt = rule36 _lhsInt
         _hdOo_unbox = rule37 _lhsIo_unbox
         _hdOsyn = rule38 _lhsIsyn
         _hdOsynMap = rule39 _lhsIsynMap
         _tlOallfields = rule40 _lhsIallfields
         _tlOallnts = rule41 _lhsIallnts
         _tlOattrs = rule42 _lhsIattrs
         _tlOcon = rule43 _lhsIcon
         _tlOinh = rule44 _lhsIinh
         _tlOinhMap = rule45 _lhsIinhMap
         _tlOmergeMap = rule46 _lhsImergeMap
         _tlOnt = rule47 _lhsInt
         _tlOo_unbox = rule48 _lhsIo_unbox
         _tlOsyn = rule49 _lhsIsyn
         _tlOsynMap = rule50 _lhsIsynMap
         __result_ = T_Children_vOut4 _lhsOattributes _lhsOcollectChildrenInhs _lhsOcollectChildrenSyns _lhsOerrors _lhsOfields _lhsOgathAltAttrs _lhsOgathRules _lhsOinhs _lhsOmirages _lhsOnts _lhsOsinglevisits _lhsOterminals
         in __result_ )
     in C_Children_s5 v4
   {-# INLINE rule17 #-}
   {-# LINE 679 "src-ag/Order.ag" #-}
   rule17 = \ ((_hdIfield) :: (Identifier,Type,ChildKind)) ((_tlIfields) :: [(Identifier,Type,ChildKind)]) ->
                         {-# LINE 679 "src-ag/Order.ag" #-}
                         _hdIfield : _tlIfields
                         {-# LINE 522 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule18 #-}
   {-# LINE 46 "src-ag/TfmToMirage.ag" #-}
   rule18 = \ ((_hdImirage) :: Mirage.Child) ((_tlImirages) :: [Mirage.Child]) ->
                         {-# LINE 46 "src-ag/TfmToMirage.ag" #-}
                         _hdImirage : _tlImirages
                         {-# LINE 528 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule19 #-}
   rule19 = \ ((_hdIattributes) :: [(Identifier,Attributes,Attributes)]) ((_tlIattributes) :: [(Identifier,Attributes,Attributes)]) ->
     _hdIattributes ++ _tlIattributes
   {-# INLINE rule20 #-}
   rule20 = \ ((_hdIcollectChildrenInhs) :: Map Identifier Attributes ) ((_tlIcollectChildrenInhs) :: Map Identifier Attributes ) ->
     _hdIcollectChildrenInhs `Map.union` _tlIcollectChildrenInhs
   {-# INLINE rule21 #-}
   rule21 = \ ((_hdIcollectChildrenSyns) :: Map Identifier Attributes ) ((_tlIcollectChildrenSyns) :: Map Identifier Attributes ) ->
     _hdIcollectChildrenSyns `Map.union` _tlIcollectChildrenSyns
   {-# INLINE rule22 #-}
   rule22 = \ ((_hdIerrors) :: Seq Error) ((_tlIerrors) :: Seq Error) ->
     _hdIerrors Seq.>< _tlIerrors
   {-# INLINE rule23 #-}
   rule23 = \ ((_hdIgathAltAttrs) :: [AltAttr]) ((_tlIgathAltAttrs) :: [AltAttr]) ->
     _hdIgathAltAttrs ++ _tlIgathAltAttrs
   {-# INLINE rule24 #-}
   rule24 = \ ((_hdIgathRules) :: Seq CRule) ((_tlIgathRules) :: Seq CRule) ->
     _hdIgathRules Seq.>< _tlIgathRules
   {-# INLINE rule25 #-}
   rule25 = \ ((_hdIinhs) :: Seq (Identifier,Attributes)) ((_tlIinhs) :: Seq (Identifier,Attributes)) ->
     _hdIinhs Seq.>< _tlIinhs
   {-# INLINE rule26 #-}
   rule26 = \ ((_hdInts) :: Seq (Identifier,NontermIdent)) ((_tlInts) :: Seq (Identifier,NontermIdent)) ->
     _hdInts Seq.>< _tlInts
   {-# INLINE rule27 #-}
   rule27 = \ ((_hdIsinglevisits) :: [CRule]) ((_tlIsinglevisits) :: [CRule]) ->
     _hdIsinglevisits ++ _tlIsinglevisits
   {-# INLINE rule28 #-}
   rule28 = \ ((_hdIterminals) :: [Identifier]) ((_tlIterminals) :: [Identifier]) ->
     _hdIterminals ++ _tlIterminals
   {-# INLINE rule29 #-}
   rule29 = \ ((_lhsIallfields) :: [(Identifier,Type,ChildKind)]) ->
     _lhsIallfields
   {-# INLINE rule30 #-}
   rule30 = \ ((_lhsIallnts) :: [Identifier]) ->
     _lhsIallnts
   {-# INLINE rule31 #-}
   rule31 = \ ((_lhsIattrs) :: [(Identifier,Identifier)]) ->
     _lhsIattrs
   {-# INLINE rule32 #-}
   rule32 = \ ((_lhsIcon) :: Identifier) ->
     _lhsIcon
   {-# INLINE rule33 #-}
   rule33 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule34 #-}
   rule34 = \ ((_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# INLINE rule35 #-}
   rule35 = \ ((_lhsImergeMap) :: Map Identifier (Identifier,[Identifier])) ->
     _lhsImergeMap
   {-# INLINE rule36 #-}
   rule36 = \ ((_lhsInt) :: Identifier) ->
     _lhsInt
   {-# INLINE rule37 #-}
   rule37 = \ ((_lhsIo_unbox) :: Bool) ->
     _lhsIo_unbox
   {-# INLINE rule38 #-}
   rule38 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
   {-# INLINE rule39 #-}
   rule39 = \ ((_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
   {-# INLINE rule40 #-}
   rule40 = \ ((_lhsIallfields) :: [(Identifier,Type,ChildKind)]) ->
     _lhsIallfields
   {-# INLINE rule41 #-}
   rule41 = \ ((_lhsIallnts) :: [Identifier]) ->
     _lhsIallnts
   {-# INLINE rule42 #-}
   rule42 = \ ((_lhsIattrs) :: [(Identifier,Identifier)]) ->
     _lhsIattrs
   {-# INLINE rule43 #-}
   rule43 = \ ((_lhsIcon) :: Identifier) ->
     _lhsIcon
   {-# INLINE rule44 #-}
   rule44 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule45 #-}
   rule45 = \ ((_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# INLINE rule46 #-}
   rule46 = \ ((_lhsImergeMap) :: Map Identifier (Identifier,[Identifier])) ->
     _lhsImergeMap
   {-# INLINE rule47 #-}
   rule47 = \ ((_lhsInt) :: Identifier) ->
     _lhsInt
   {-# INLINE rule48 #-}
   rule48 = \ ((_lhsIo_unbox) :: Bool) ->
     _lhsIo_unbox
   {-# INLINE rule49 #-}
   rule49 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
   {-# INLINE rule50 #-}
   rule50 = \ ((_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
{-# NOINLINE sem_Children_Nil #-}
sem_Children_Nil ::  T_Children 
sem_Children_Nil  = T_Children (return st5) where
   {-# NOINLINE st5 #-}
   st5 = let
      v4 :: T_Children_v4 
      v4 = \ (T_Children_vIn4 _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsIinh _lhsIinhMap _lhsImergeMap _lhsInt _lhsIo_unbox _lhsIsyn _lhsIsynMap) -> ( let
         _lhsOfields :: [(Identifier,Type,ChildKind)]
         _lhsOfields = rule51  ()
         _lhsOmirages :: [Mirage.Child]
         _lhsOmirages = rule52  ()
         _lhsOattributes :: [(Identifier,Attributes,Attributes)]
         _lhsOattributes = rule53  ()
         _lhsOcollectChildrenInhs :: Map Identifier Attributes 
         _lhsOcollectChildrenInhs = rule54  ()
         _lhsOcollectChildrenSyns :: Map Identifier Attributes 
         _lhsOcollectChildrenSyns = rule55  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule56  ()
         _lhsOgathAltAttrs :: [AltAttr]
         _lhsOgathAltAttrs = rule57  ()
         _lhsOgathRules :: Seq CRule
         _lhsOgathRules = rule58  ()
         _lhsOinhs :: Seq (Identifier,Attributes)
         _lhsOinhs = rule59  ()
         _lhsOnts :: Seq (Identifier,NontermIdent)
         _lhsOnts = rule60  ()
         _lhsOsinglevisits :: [CRule]
         _lhsOsinglevisits = rule61  ()
         _lhsOterminals :: [Identifier]
         _lhsOterminals = rule62  ()
         __result_ = T_Children_vOut4 _lhsOattributes _lhsOcollectChildrenInhs _lhsOcollectChildrenSyns _lhsOerrors _lhsOfields _lhsOgathAltAttrs _lhsOgathRules _lhsOinhs _lhsOmirages _lhsOnts _lhsOsinglevisits _lhsOterminals
         in __result_ )
     in C_Children_s5 v4
   {-# INLINE rule51 #-}
   {-# LINE 680 "src-ag/Order.ag" #-}
   rule51 = \  (_ :: ()) ->
                         {-# LINE 680 "src-ag/Order.ag" #-}
                         []
                         {-# LINE 664 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule52 #-}
   {-# LINE 47 "src-ag/TfmToMirage.ag" #-}
   rule52 = \  (_ :: ()) ->
                         {-# LINE 47 "src-ag/TfmToMirage.ag" #-}
                         []
                         {-# LINE 670 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule53 #-}
   rule53 = \  (_ :: ()) ->
     []
   {-# INLINE rule54 #-}
   rule54 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule55 #-}
   rule55 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule56 #-}
   rule56 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule57 #-}
   rule57 = \  (_ :: ()) ->
     []
   {-# INLINE rule58 #-}
   rule58 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule59 #-}
   rule59 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule60 #-}
   rule60 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule61 #-}
   rule61 = \  (_ :: ()) ->
     []
   {-# INLINE rule62 #-}
   rule62 = \  (_ :: ()) ->
     []

-- Expression --------------------------------------------------
-- wrapper
data Inh_Expression  = Inh_Expression { allfields_Inh_Expression :: ([(Identifier,Type,ChildKind)]), allnts_Inh_Expression :: ([Identifier]), attrs_Inh_Expression :: ([(Identifier,Identifier)]), con_Inh_Expression :: (Identifier), mergeMap_Inh_Expression :: (Map Identifier (Identifier,[Identifier])), nt_Inh_Expression :: (Identifier), options_Inh_Expression :: (Options) }
data Syn_Expression  = Syn_Expression { allRhsVars_Syn_Expression :: (Set (Identifier,Identifier)), copy_Syn_Expression :: (Expression), errors_Syn_Expression :: (Seq Error), lns_Syn_Expression :: ([String]), textLines_Syn_Expression :: ([String]), usedAttrs_Syn_Expression :: ([(Identifier,Identifier)]), usedFields_Syn_Expression :: ([Identifier]), usedLocals_Syn_Expression :: ([Identifier]) }
{-# INLINABLE wrap_Expression #-}
wrap_Expression :: T_Expression  -> Inh_Expression  -> (Syn_Expression )
wrap_Expression (T_Expression act) (Inh_Expression _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsImergeMap _lhsInt _lhsIoptions) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg7 = T_Expression_vIn7 _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsImergeMap _lhsInt _lhsIoptions
        (T_Expression_vOut7 _lhsOallRhsVars _lhsOcopy _lhsOerrors _lhsOlns _lhsOtextLines _lhsOusedAttrs _lhsOusedFields _lhsOusedLocals) <- return (inv_Expression_s8 sem arg7)
        return (Syn_Expression _lhsOallRhsVars _lhsOcopy _lhsOerrors _lhsOlns _lhsOtextLines _lhsOusedAttrs _lhsOusedFields _lhsOusedLocals)
   )

-- cata
{-# INLINE sem_Expression #-}
sem_Expression :: Expression  -> T_Expression 
sem_Expression ( Expression pos_ tks_ ) = sem_Expression_Expression pos_ tks_

-- semantic domain
newtype T_Expression  = T_Expression {
                                     attach_T_Expression :: Identity (T_Expression_s8 )
                                     }
newtype T_Expression_s8  = C_Expression_s8 {
                                           inv_Expression_s8 :: (T_Expression_v7 )
                                           }
data T_Expression_s9  = C_Expression_s9
type T_Expression_v7  = (T_Expression_vIn7 ) -> (T_Expression_vOut7 )
data T_Expression_vIn7  = T_Expression_vIn7 ([(Identifier,Type,ChildKind)]) ([Identifier]) ([(Identifier,Identifier)]) (Identifier) (Map Identifier (Identifier,[Identifier])) (Identifier) (Options)
data T_Expression_vOut7  = T_Expression_vOut7 (Set (Identifier,Identifier)) (Expression) (Seq Error) ([String]) ([String]) ([(Identifier,Identifier)]) ([Identifier]) ([Identifier])
{-# NOINLINE sem_Expression_Expression #-}
sem_Expression_Expression :: (Pos) -> ([HsToken]) -> T_Expression 
sem_Expression_Expression arg_pos_ arg_tks_ = T_Expression (return st8) where
   {-# NOINLINE st8 #-}
   st8 = let
      v7 :: T_Expression_v7 
      v7 = \ (T_Expression_vIn7 _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsImergeMap _lhsInt _lhsIoptions) -> ( let
         (_textLines,_usedAttrs,_usedLocals,_usedFields) = rule63 _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsImergeMap _lhsInt _lhsIoptions arg_tks_
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule64  ()
         _lhsOallRhsVars :: Set (Identifier,Identifier)
         _lhsOallRhsVars = rule65 _usedAttrs _usedFields _usedLocals
         _lhsOlns :: [String]
         _lhsOlns = rule66 arg_tks_
         _copy = rule67 arg_pos_ arg_tks_
         _lhsOcopy :: Expression
         _lhsOcopy = rule68 _copy
         _lhsOtextLines :: [String]
         _lhsOtextLines = rule69 _textLines
         _lhsOusedAttrs :: [(Identifier,Identifier)]
         _lhsOusedAttrs = rule70 _usedAttrs
         _lhsOusedFields :: [Identifier]
         _lhsOusedFields = rule71 _usedFields
         _lhsOusedLocals :: [Identifier]
         _lhsOusedLocals = rule72 _usedLocals
         __result_ = T_Expression_vOut7 _lhsOallRhsVars _lhsOcopy _lhsOerrors _lhsOlns _lhsOtextLines _lhsOusedAttrs _lhsOusedFields _lhsOusedLocals
         in __result_ )
     in C_Expression_s8 v7
   {-# INLINE rule63 #-}
   {-# LINE 469 "src-ag/Order.ag" #-}
   rule63 = \ ((_lhsIallfields) :: [(Identifier,Type,ChildKind)]) ((_lhsIallnts) :: [Identifier]) ((_lhsIattrs) :: [(Identifier,Identifier)]) ((_lhsIcon) :: Identifier) ((_lhsImergeMap) :: Map Identifier (Identifier,[Identifier])) ((_lhsInt) :: Identifier) ((_lhsIoptions) :: Options) tks_ ->
                                {-# LINE 469 "src-ag/Order.ag" #-}
                                let mergedChildren = [ x | (_,xs) <- Map.elems _lhsImergeMap, x <- xs ]
                                    attrsIn = filter (\(fld,_) -> not (fld `elem` mergedChildren)) _lhsIattrs
                                    inherited = Inh_HsTokensRoot
                                                { attrs_Inh_HsTokensRoot      = attrsIn
                                                , con_Inh_HsTokensRoot        = _lhsIcon
                                                , allfields_Inh_HsTokensRoot  = _lhsIallfields
                                                , allnts_Inh_HsTokensRoot     = _lhsIallnts
                                                , nt_Inh_HsTokensRoot         = _lhsInt
                                                , options_Inh_HsTokensRoot    = _lhsIoptions
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
                                {-# LINE 786 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule64 #-}
   {-# LINE 492 "src-ag/Order.ag" #-}
   rule64 = \  (_ :: ()) ->
                               {-# LINE 492 "src-ag/Order.ag" #-}
                               Seq.empty
                               {-# LINE 792 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule65 #-}
   {-# LINE 493 "src-ag/Order.ag" #-}
   rule65 = \ _usedAttrs _usedFields _usedLocals ->
                                   {-# LINE 493 "src-ag/Order.ag" #-}
                                   Set.fromList _usedAttrs
                                   `Set.union`
                                   Set.fromList [ (_LOC, l) | l <- _usedLocals    ]
                                   `Set.union`
                                   Set.fromList [ (_FIELD, fld) | fld <- _usedFields    ]
                                   {-# LINE 802 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule66 #-}
   {-# LINE 71 "src-ag/TfmToMirage.ag" #-}
   rule66 = \ tks_ ->
                            {-# LINE 71 "src-ag/TfmToMirage.ag" #-}
                            showTokens . tokensToStrings $ tks_
                            {-# LINE 808 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule67 #-}
   rule67 = \ pos_ tks_ ->
     Expression pos_ tks_
   {-# INLINE rule68 #-}
   rule68 = \ _copy ->
     _copy
   {-# INLINE rule69 #-}
   rule69 = \ _textLines ->
     _textLines
   {-# INLINE rule70 #-}
   rule70 = \ _usedAttrs ->
     _usedAttrs
   {-# INLINE rule71 #-}
   rule71 = \ _usedFields ->
     _usedFields
   {-# INLINE rule72 #-}
   rule72 = \ _usedLocals ->
     _usedLocals

-- Grammar -----------------------------------------------------
-- wrapper
data Inh_Grammar  = Inh_Grammar { options_Inh_Grammar :: (Options) }
data Syn_Grammar  = Syn_Grammar { errors_Syn_Grammar :: (Seq Error), mirage_Syn_Grammar :: (Mirage.Grammar), nAutoRules_Syn_Grammar :: (Int), nExplicitRules_Syn_Grammar :: (Int), output_Syn_Grammar :: (CGrammar) }
{-# INLINABLE wrap_Grammar #-}
wrap_Grammar :: T_Grammar  -> Inh_Grammar  -> (Syn_Grammar )
wrap_Grammar (T_Grammar act) (Inh_Grammar _lhsIoptions) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg10 = T_Grammar_vIn10 _lhsIoptions
        (T_Grammar_vOut10 _lhsOerrors _lhsOmirage _lhsOnAutoRules _lhsOnExplicitRules _lhsOoutput) <- return (inv_Grammar_s11 sem arg10)
        return (Syn_Grammar _lhsOerrors _lhsOmirage _lhsOnAutoRules _lhsOnExplicitRules _lhsOoutput)
   )

-- cata
{-# INLINE sem_Grammar #-}
sem_Grammar :: Grammar  -> T_Grammar 
sem_Grammar ( Grammar typeSyns_ useMap_ derivings_ wrappers_ nonts_ pragmas_ manualAttrOrderMap_ paramMap_ contextMap_ quantMap_ uniqueMap_ augmentsMap_ aroundsMap_ mergeMap_ ) = sem_Grammar_Grammar typeSyns_ useMap_ derivings_ wrappers_ ( sem_Nonterminals nonts_ ) pragmas_ manualAttrOrderMap_ paramMap_ contextMap_ quantMap_ uniqueMap_ augmentsMap_ aroundsMap_ mergeMap_

-- semantic domain
newtype T_Grammar  = T_Grammar {
                               attach_T_Grammar :: Identity (T_Grammar_s11 )
                               }
newtype T_Grammar_s11  = C_Grammar_s11 {
                                       inv_Grammar_s11 :: (T_Grammar_v10 )
                                       }
data T_Grammar_s12  = C_Grammar_s12
type T_Grammar_v10  = (T_Grammar_vIn10 ) -> (T_Grammar_vOut10 )
data T_Grammar_vIn10  = T_Grammar_vIn10 (Options)
data T_Grammar_vOut10  = T_Grammar_vOut10 (Seq Error) (Mirage.Grammar) (Int) (Int) (CGrammar)
{-# NOINLINE sem_Grammar_Grammar #-}
sem_Grammar_Grammar :: (TypeSyns) -> (UseMap) -> (Derivings) -> (Set NontermIdent) -> T_Nonterminals  -> (PragmaMap) -> (AttrOrderMap) -> (ParamMap) -> (ContextMap) -> (QuantMap) -> (UniqueMap) -> (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) -> (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) -> (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier, [Identifier], Expression)))) -> T_Grammar 
sem_Grammar_Grammar arg_typeSyns_ _ arg_derivings_ arg_wrappers_ arg_nonts_ arg_pragmas_ arg_manualAttrOrderMap_ arg_paramMap_ arg_contextMap_ arg_quantMap_ _ _ arg_aroundsMap_ arg_mergeMap_ = T_Grammar (return st11) where
   {-# NOINLINE st11 #-}
   st11 = let
      v10 :: T_Grammar_v10 
      v10 = \ (T_Grammar_vIn10 _lhsIoptions) -> ( let
         _nontsX17 = Control.Monad.Identity.runIdentity (attach_T_Nonterminals (arg_nonts_))
         (T_Nonterminals_vOut16 _nontsIacount _nontsIadditionalDep _nontsIaranges _nontsIaroundDep _nontsIcNonterminals _nontsIdirectDep _nontsIerrors _nontsIinhMap' _nontsIinstDep _nontsImergeDep _nontsImirages _nontsInAutoRules _nontsInExplicitRules _nontsInonts _nontsIntattrs _nontsIrules _nontsIsynMap' _nontsIvcount) = inv_Nonterminals_s17 _nontsX17 (T_Nonterminals_vIn16 _nontsOacount _nontsOallnts _nontsOaroundMap _nontsOcInterfaceMap _nontsOcVisitsMap _nontsOinhMap _nontsOmanualAttrDepMap _nontsOmergeMap _nontsOo_case _nontsOo_cata _nontsOo_data _nontsOo_dovisit _nontsOo_newtypes _nontsOo_rename _nontsOo_sem _nontsOo_sig _nontsOo_unbox _nontsOo_wantvisit _nontsOoptions _nontsOprefix _nontsOsynMap _nontsOvcount)
         _nontsOinhMap = rule73 _nontsIinhMap'
         _nontsOsynMap = rule74 _nontsIsynMap'
         _o_dovisit = rule75 _cyclesErrors _lhsIoptions
         _nontsOo_cata = rule76 _lhsIoptions
         _nontsOo_data = rule77 _lhsIoptions
         _nontsOo_sig = rule78 _lhsIoptions
         _nontsOo_sem = rule79 _lhsIoptions
         _nontsOo_rename = rule80 _lhsIoptions
         _nontsOo_newtypes = rule81 _lhsIoptions
         _nontsOo_wantvisit = rule82 _lhsIoptions
         _nontsOo_unbox = rule83 _lhsIoptions
         _nontsOo_case = rule84 _lhsIoptions
         _nontsOprefix = rule85 _lhsIoptions
         _nontsOvcount = rule86  ()
         _nontsOmanualAttrDepMap = rule87 arg_manualAttrOrderMap_
         _nontsOaroundMap = rule88 arg_aroundsMap_
         _nontsOacount = rule89  ()
         _ruleTable = rule90 _nontsIrules _nontsIvcount
         _attrTable = rule91 _nontsIacount _nontsIntattrs
         _attrVertex = rule92 _nontsIntattrs
         _tdpToTds = rule93 _attrVertex _nontsIrules
         _tdsToTdp = rule94 _tdpToTds
         _directDep = rule95 _nontsIadditionalDep _nontsIdirectDep
         _instDep = rule96 _nontsIinstDep
         _aroundDep = rule97 _nontsIaroundDep
         _mergeDep = rule98 _nontsImergeDep
         _info = rule99 _attrTable _nontsIacount _nontsIaranges _nontsInonts _nontsIvcount _ruleTable _tdpToTds _tdsToTdp arg_wrappers_
         (_cInterfaceMap,_cVisitsMap,_cyclesErrors) = rule100 _aroundDep _attrTable _directDep _info _instDep _lhsIoptions _mergeDep _ruleTable
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule101 _cyclesErrors _lhsIoptions _nontsIerrors
         _lhsOoutput :: CGrammar
         _lhsOoutput = rule102 _aroundMap _mergeMap _nontsIcNonterminals _o_dovisit arg_contextMap_ arg_derivings_ arg_paramMap_ arg_pragmas_ arg_quantMap_ arg_typeSyns_ arg_wrappers_
         _aroundMap = rule103 arg_aroundsMap_
         _mergeMap = rule104 arg_mergeMap_
         _nontsOallnts = rule105 _nontsInonts
         _lhsOmirage :: Mirage.Grammar
         _lhsOmirage = rule106 _nontsImirages
         _lhsOnAutoRules :: Int
         _lhsOnAutoRules = rule107 _nontsInAutoRules
         _lhsOnExplicitRules :: Int
         _lhsOnExplicitRules = rule108 _nontsInExplicitRules
         _nontsOcInterfaceMap = rule109 _cInterfaceMap
         _nontsOcVisitsMap = rule110 _cVisitsMap
         _nontsOmergeMap = rule111 _mergeMap
         _nontsOo_dovisit = rule112 _o_dovisit
         _nontsOoptions = rule113 _lhsIoptions
         __result_ = T_Grammar_vOut10 _lhsOerrors _lhsOmirage _lhsOnAutoRules _lhsOnExplicitRules _lhsOoutput
         in __result_ )
     in C_Grammar_s11 v10
   {-# INLINE rule73 #-}
   {-# LINE 15 "src-ag/DistChildAttr.ag" #-}
   rule73 = \ ((_nontsIinhMap') :: Map Identifier Attributes) ->
                             {-# LINE 15 "src-ag/DistChildAttr.ag" #-}
                             _nontsIinhMap'
                             {-# LINE 921 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule74 #-}
   {-# LINE 16 "src-ag/DistChildAttr.ag" #-}
   rule74 = \ ((_nontsIsynMap') :: Map Identifier Attributes) ->
                             {-# LINE 16 "src-ag/DistChildAttr.ag" #-}
                             _nontsIsynMap'
                             {-# LINE 927 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule75 #-}
   {-# LINE 123 "src-ag/Order.ag" #-}
   rule75 = \ _cyclesErrors ((_lhsIoptions) :: Options) ->
                                    {-# LINE 123 "src-ag/Order.ag" #-}
                                    visit     _lhsIoptions && null _cyclesErrors
                                    {-# LINE 933 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule76 #-}
   {-# LINE 124 "src-ag/Order.ag" #-}
   rule76 = \ ((_lhsIoptions) :: Options) ->
                                    {-# LINE 124 "src-ag/Order.ag" #-}
                                    folds     _lhsIoptions
                                    {-# LINE 939 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule77 #-}
   {-# LINE 125 "src-ag/Order.ag" #-}
   rule77 = \ ((_lhsIoptions) :: Options) ->
                                    {-# LINE 125 "src-ag/Order.ag" #-}
                                    dataTypes _lhsIoptions
                                    {-# LINE 945 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule78 #-}
   {-# LINE 126 "src-ag/Order.ag" #-}
   rule78 = \ ((_lhsIoptions) :: Options) ->
                                    {-# LINE 126 "src-ag/Order.ag" #-}
                                    typeSigs  _lhsIoptions
                                    {-# LINE 951 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule79 #-}
   {-# LINE 127 "src-ag/Order.ag" #-}
   rule79 = \ ((_lhsIoptions) :: Options) ->
                                    {-# LINE 127 "src-ag/Order.ag" #-}
                                    semfuns   _lhsIoptions
                                    {-# LINE 957 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule80 #-}
   {-# LINE 128 "src-ag/Order.ag" #-}
   rule80 = \ ((_lhsIoptions) :: Options) ->
                                    {-# LINE 128 "src-ag/Order.ag" #-}
                                    rename    _lhsIoptions
                                    {-# LINE 963 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule81 #-}
   {-# LINE 129 "src-ag/Order.ag" #-}
   rule81 = \ ((_lhsIoptions) :: Options) ->
                                    {-# LINE 129 "src-ag/Order.ag" #-}
                                    newtypes  _lhsIoptions
                                    {-# LINE 969 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule82 #-}
   {-# LINE 130 "src-ag/Order.ag" #-}
   rule82 = \ ((_lhsIoptions) :: Options) ->
                                      {-# LINE 130 "src-ag/Order.ag" #-}
                                      visit   _lhsIoptions
                                      {-# LINE 975 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule83 #-}
   {-# LINE 131 "src-ag/Order.ag" #-}
   rule83 = \ ((_lhsIoptions) :: Options) ->
                                    {-# LINE 131 "src-ag/Order.ag" #-}
                                    unbox     _lhsIoptions
                                    {-# LINE 981 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule84 #-}
   {-# LINE 132 "src-ag/Order.ag" #-}
   rule84 = \ ((_lhsIoptions) :: Options) ->
                                    {-# LINE 132 "src-ag/Order.ag" #-}
                                    cases     _lhsIoptions
                                    {-# LINE 987 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule85 #-}
   {-# LINE 133 "src-ag/Order.ag" #-}
   rule85 = \ ((_lhsIoptions) :: Options) ->
                                    {-# LINE 133 "src-ag/Order.ag" #-}
                                    prefix    _lhsIoptions
                                    {-# LINE 993 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule86 #-}
   {-# LINE 262 "src-ag/Order.ag" #-}
   rule86 = \  (_ :: ()) ->
                               {-# LINE 262 "src-ag/Order.ag" #-}
                               0
                               {-# LINE 999 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule87 #-}
   {-# LINE 288 "src-ag/Order.ag" #-}
   rule87 = \ manualAttrOrderMap_ ->
                                 {-# LINE 288 "src-ag/Order.ag" #-}
                                 manualAttrOrderMap_
                                 {-# LINE 1005 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule88 #-}
   {-# LINE 417 "src-ag/Order.ag" #-}
   rule88 = \ aroundsMap_ ->
                                 {-# LINE 417 "src-ag/Order.ag" #-}
                                 aroundsMap_
                                 {-# LINE 1011 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule89 #-}
   {-# LINE 508 "src-ag/Order.ag" #-}
   rule89 = \  (_ :: ()) ->
                             {-# LINE 508 "src-ag/Order.ag" #-}
                             0
                             {-# LINE 1017 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule90 #-}
   {-# LINE 546 "src-ag/Order.ag" #-}
   rule90 = \ ((_nontsIrules) :: Seq (Vertex,CRule)) ((_nontsIvcount) :: Int) ->
                              {-# LINE 546 "src-ag/Order.ag" #-}
                              Array.array (0,_nontsIvcount-1) (toList _nontsIrules)
                              {-# LINE 1023 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule91 #-}
   {-# LINE 547 "src-ag/Order.ag" #-}
   rule91 = \ ((_nontsIacount) :: Int) ((_nontsIntattrs) :: Seq (Vertex,NTAttr)) ->
                              {-# LINE 547 "src-ag/Order.ag" #-}
                              Array.array (0,_nontsIacount-1) (toList _nontsIntattrs)
                              {-# LINE 1029 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule92 #-}
   {-# LINE 548 "src-ag/Order.ag" #-}
   rule92 = \ ((_nontsIntattrs) :: Seq (Vertex,NTAttr)) ->
                               {-# LINE 548 "src-ag/Order.ag" #-}
                               Map.fromList (map swap (toList _nontsIntattrs))
                               {-# LINE 1035 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule93 #-}
   {-# LINE 549 "src-ag/Order.ag" #-}
   rule93 = \ _attrVertex ((_nontsIrules) :: Seq (Vertex,CRule)) ->
                              {-# LINE 549 "src-ag/Order.ag" #-}
                              [ (s, maybe (-1) (\v -> findWithErr1 "Grammar.tdpToTds" v _attrVertex) (ntattr cr))
                              | (s,cr) <- toList _nontsIrules]
                              {-# LINE 1042 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule94 #-}
   {-# LINE 551 "src-ag/Order.ag" #-}
   rule94 = \ _tdpToTds ->
                               {-# LINE 551 "src-ag/Order.ag" #-}
                               let  eq (_,v) (_,v') = v == v'
                                    conv ((s,v):svs)  | v == -1 = Nothing
                                                      | otherwise = Just (v,s:map fst svs)
                               in mapMaybe conv (eqClasses eq _tdpToTds)
                               {-# LINE 1051 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule95 #-}
   {-# LINE 555 "src-ag/Order.ag" #-}
   rule95 = \ ((_nontsIadditionalDep) :: Seq Edge) ((_nontsIdirectDep) :: Seq Edge) ->
                              {-# LINE 555 "src-ag/Order.ag" #-}
                              toList (_nontsIdirectDep Seq.>< _nontsIadditionalDep)
                              {-# LINE 1057 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule96 #-}
   {-# LINE 556 "src-ag/Order.ag" #-}
   rule96 = \ ((_nontsIinstDep) :: Seq Edge) ->
                              {-# LINE 556 "src-ag/Order.ag" #-}
                              toList _nontsIinstDep
                              {-# LINE 1063 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule97 #-}
   {-# LINE 557 "src-ag/Order.ag" #-}
   rule97 = \ ((_nontsIaroundDep) :: Seq Edge) ->
                              {-# LINE 557 "src-ag/Order.ag" #-}
                              toList _nontsIaroundDep
                              {-# LINE 1069 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule98 #-}
   {-# LINE 558 "src-ag/Order.ag" #-}
   rule98 = \ ((_nontsImergeDep) :: Seq Edge) ->
                              {-# LINE 558 "src-ag/Order.ag" #-}
                              toList _nontsImergeDep
                              {-# LINE 1075 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule99 #-}
   {-# LINE 559 "src-ag/Order.ag" #-}
   rule99 = \ _attrTable ((_nontsIacount) :: Int) ((_nontsIaranges) :: Seq (Int,Int,Int)) ((_nontsInonts) :: [(NontermIdent,[ConstructorIdent])]) ((_nontsIvcount) :: Int) _ruleTable _tdpToTds _tdsToTdp wrappers_ ->
                              {-# LINE 559 "src-ag/Order.ag" #-}
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
                              {-# LINE 1090 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule100 #-}
   {-# LINE 571 "src-ag/Order.ag" #-}
   rule100 = \ _aroundDep _attrTable _directDep _info _instDep ((_lhsIoptions) :: Options) _mergeDep _ruleTable ->
                                {-# LINE 571 "src-ag/Order.ag" #-}
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
                                {-# LINE 1116 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule101 #-}
   {-# LINE 592 "src-ag/Order.ag" #-}
   rule101 = \ _cyclesErrors ((_lhsIoptions) :: Options) ((_nontsIerrors) :: Seq Error) ->
                           {-# LINE 592 "src-ag/Order.ag" #-}
                           (if withCycle _lhsIoptions then Seq.fromList _cyclesErrors else Seq.empty)
                            Seq.>< _nontsIerrors
                           {-# LINE 1123 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule102 #-}
   {-# LINE 624 "src-ag/Order.ag" #-}
   rule102 = \ _aroundMap _mergeMap ((_nontsIcNonterminals) :: CNonterminals) _o_dovisit contextMap_ derivings_ paramMap_ pragmas_ quantMap_ typeSyns_ wrappers_ ->
                             {-# LINE 624 "src-ag/Order.ag" #-}
                             CGrammar typeSyns_ derivings_ wrappers_ _nontsIcNonterminals pragmas_ paramMap_ contextMap_ quantMap_ _aroundMap     _mergeMap     _o_dovisit
                             {-# LINE 1129 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule103 #-}
   {-# LINE 637 "src-ag/Order.ag" #-}
   rule103 = \ aroundsMap_ ->
                               {-# LINE 637 "src-ag/Order.ag" #-}
                               Map.map (Map.map Map.keysSet) aroundsMap_
                               {-# LINE 1135 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule104 #-}
   {-# LINE 638 "src-ag/Order.ag" #-}
   rule104 = \ mergeMap_ ->
                               {-# LINE 638 "src-ag/Order.ag" #-}
                               Map.map (Map.map (Map.map (\(nt,srcs,_) -> (nt,srcs)))) mergeMap_
                               {-# LINE 1141 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule105 #-}
   {-# LINE 655 "src-ag/Order.ag" #-}
   rule105 = \ ((_nontsInonts) :: [(NontermIdent,[ConstructorIdent])]) ->
                             {-# LINE 655 "src-ag/Order.ag" #-}
                             map fst (_nontsInonts)
                             {-# LINE 1147 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule106 #-}
   {-# LINE 22 "src-ag/TfmToMirage.ag" #-}
   rule106 = \ ((_nontsImirages) :: [Mirage.Nonterminal]) ->
                           {-# LINE 22 "src-ag/TfmToMirage.ag" #-}
                           Mirage.Grammar _nontsImirages
                           {-# LINE 1153 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule107 #-}
   rule107 = \ ((_nontsInAutoRules) :: Int) ->
     _nontsInAutoRules
   {-# INLINE rule108 #-}
   rule108 = \ ((_nontsInExplicitRules) :: Int) ->
     _nontsInExplicitRules
   {-# INLINE rule109 #-}
   rule109 = \ _cInterfaceMap ->
     _cInterfaceMap
   {-# INLINE rule110 #-}
   rule110 = \ _cVisitsMap ->
     _cVisitsMap
   {-# INLINE rule111 #-}
   rule111 = \ _mergeMap ->
     _mergeMap
   {-# INLINE rule112 #-}
   rule112 = \ _o_dovisit ->
     _o_dovisit
   {-# INLINE rule113 #-}
   rule113 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions

-- Nonterminal -------------------------------------------------
-- wrapper
data Inh_Nonterminal  = Inh_Nonterminal { acount_Inh_Nonterminal :: (Int), allnts_Inh_Nonterminal :: ([Identifier]), aroundMap_Inh_Nonterminal :: (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))), cInterfaceMap_Inh_Nonterminal :: (CInterfaceMap), cVisitsMap_Inh_Nonterminal :: (CVisitsMap), inhMap_Inh_Nonterminal :: (Map Identifier Attributes), manualAttrDepMap_Inh_Nonterminal :: (AttrOrderMap), mergeMap_Inh_Nonterminal :: (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier])))), o_case_Inh_Nonterminal :: (Bool), o_cata_Inh_Nonterminal :: (Bool), o_data_Inh_Nonterminal :: (Bool), o_dovisit_Inh_Nonterminal :: (Bool), o_newtypes_Inh_Nonterminal :: (Bool), o_rename_Inh_Nonterminal :: (Bool), o_sem_Inh_Nonterminal :: (Bool), o_sig_Inh_Nonterminal :: (Bool), o_unbox_Inh_Nonterminal :: (Bool), o_wantvisit_Inh_Nonterminal :: (Bool), options_Inh_Nonterminal :: (Options), prefix_Inh_Nonterminal :: (String), synMap_Inh_Nonterminal :: (Map Identifier Attributes), vcount_Inh_Nonterminal :: (Int) }
data Syn_Nonterminal  = Syn_Nonterminal { acount_Syn_Nonterminal :: (Int), additionalDep_Syn_Nonterminal :: (Seq Edge), aranges_Syn_Nonterminal :: (Seq (Int,Int,Int)), aroundDep_Syn_Nonterminal :: (Seq Edge), cNonterminal_Syn_Nonterminal :: (CNonterminal), directDep_Syn_Nonterminal :: (Seq Edge), errors_Syn_Nonterminal :: (Seq Error), inhMap'_Syn_Nonterminal :: (Map Identifier Attributes), instDep_Syn_Nonterminal :: (Seq Edge), mergeDep_Syn_Nonterminal :: (Seq Edge), mirage_Syn_Nonterminal :: (Mirage.Nonterminal), nAutoRules_Syn_Nonterminal :: (Int), nExplicitRules_Syn_Nonterminal :: (Int), nonts_Syn_Nonterminal :: ([(NontermIdent,[ConstructorIdent])]), ntattrs_Syn_Nonterminal :: (Seq (Vertex,NTAttr)), rules_Syn_Nonterminal :: (Seq (Vertex,CRule)), synMap'_Syn_Nonterminal :: (Map Identifier Attributes), vcount_Syn_Nonterminal :: (Int) }
{-# INLINABLE wrap_Nonterminal #-}
wrap_Nonterminal :: T_Nonterminal  -> Inh_Nonterminal  -> (Syn_Nonterminal )
wrap_Nonterminal (T_Nonterminal act) (Inh_Nonterminal _lhsIacount _lhsIallnts _lhsIaroundMap _lhsIcInterfaceMap _lhsIcVisitsMap _lhsIinhMap _lhsImanualAttrDepMap _lhsImergeMap _lhsIo_case _lhsIo_cata _lhsIo_data _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_unbox _lhsIo_wantvisit _lhsIoptions _lhsIprefix _lhsIsynMap _lhsIvcount) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg13 = T_Nonterminal_vIn13 _lhsIacount _lhsIallnts _lhsIaroundMap _lhsIcInterfaceMap _lhsIcVisitsMap _lhsIinhMap _lhsImanualAttrDepMap _lhsImergeMap _lhsIo_case _lhsIo_cata _lhsIo_data _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_unbox _lhsIo_wantvisit _lhsIoptions _lhsIprefix _lhsIsynMap _lhsIvcount
        (T_Nonterminal_vOut13 _lhsOacount _lhsOadditionalDep _lhsOaranges _lhsOaroundDep _lhsOcNonterminal _lhsOdirectDep _lhsOerrors _lhsOinhMap' _lhsOinstDep _lhsOmergeDep _lhsOmirage _lhsOnAutoRules _lhsOnExplicitRules _lhsOnonts _lhsOntattrs _lhsOrules _lhsOsynMap' _lhsOvcount) <- return (inv_Nonterminal_s14 sem arg13)
        return (Syn_Nonterminal _lhsOacount _lhsOadditionalDep _lhsOaranges _lhsOaroundDep _lhsOcNonterminal _lhsOdirectDep _lhsOerrors _lhsOinhMap' _lhsOinstDep _lhsOmergeDep _lhsOmirage _lhsOnAutoRules _lhsOnExplicitRules _lhsOnonts _lhsOntattrs _lhsOrules _lhsOsynMap' _lhsOvcount)
   )

-- cata
{-# INLINE sem_Nonterminal #-}
sem_Nonterminal :: Nonterminal  -> T_Nonterminal 
sem_Nonterminal ( Nonterminal nt_ params_ inh_ syn_ prods_ ) = sem_Nonterminal_Nonterminal nt_ params_ inh_ syn_ ( sem_Productions prods_ )

-- semantic domain
newtype T_Nonterminal  = T_Nonterminal {
                                       attach_T_Nonterminal :: Identity (T_Nonterminal_s14 )
                                       }
newtype T_Nonterminal_s14  = C_Nonterminal_s14 {
                                               inv_Nonterminal_s14 :: (T_Nonterminal_v13 )
                                               }
data T_Nonterminal_s15  = C_Nonterminal_s15
type T_Nonterminal_v13  = (T_Nonterminal_vIn13 ) -> (T_Nonterminal_vOut13 )
data T_Nonterminal_vIn13  = T_Nonterminal_vIn13 (Int) ([Identifier]) (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) (CInterfaceMap) (CVisitsMap) (Map Identifier Attributes) (AttrOrderMap) (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier])))) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Options) (String) (Map Identifier Attributes) (Int)
data T_Nonterminal_vOut13  = T_Nonterminal_vOut13 (Int) (Seq Edge) (Seq (Int,Int,Int)) (Seq Edge) (CNonterminal) (Seq Edge) (Seq Error) (Map Identifier Attributes) (Seq Edge) (Seq Edge) (Mirage.Nonterminal) (Int) (Int) ([(NontermIdent,[ConstructorIdent])]) (Seq (Vertex,NTAttr)) (Seq (Vertex,CRule)) (Map Identifier Attributes) (Int)
{-# NOINLINE sem_Nonterminal_Nonterminal #-}
sem_Nonterminal_Nonterminal :: (NontermIdent) -> ([Identifier]) -> (Attributes) -> (Attributes) -> T_Productions  -> T_Nonterminal 
sem_Nonterminal_Nonterminal arg_nt_ arg_params_ arg_inh_ arg_syn_ arg_prods_ = T_Nonterminal (return st14) where
   {-# NOINLINE st14 #-}
   st14 = let
      v13 :: T_Nonterminal_v13 
      v13 = \ (T_Nonterminal_vIn13 _lhsIacount _lhsIallnts _lhsIaroundMap _lhsIcInterfaceMap _lhsIcVisitsMap _lhsIinhMap _lhsImanualAttrDepMap _lhsImergeMap _lhsIo_case _lhsIo_cata _lhsIo_data _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_unbox _lhsIo_wantvisit _lhsIoptions _lhsIprefix _lhsIsynMap _lhsIvcount) -> ( let
         _prodsX29 = Control.Monad.Identity.runIdentity (attach_T_Productions (arg_prods_))
         (T_Productions_vOut28 _prodsIadditionalDep _prodsIaroundDep _prodsIcProductions _prodsIcons _prodsIdirectDep _prodsIerrors _prodsIinstDep _prodsImergeDep _prodsImirages _prodsInAutoRules _prodsInExplicitRules _prodsIrules _prodsIvcount) = inv_Productions_s29 _prodsX29 (T_Productions_vIn28 _prodsOallnts _prodsOaroundMap _prodsOcVisitsMap _prodsOinh _prodsOinhMap _prodsOmanualAttrDepMap _prodsOmergeMap _prodsOnt _prodsOo_case _prodsOo_cata _prodsOo_dovisit _prodsOo_newtypes _prodsOo_rename _prodsOo_sem _prodsOo_sig _prodsOo_unbox _prodsOo_wantvisit _prodsOoptions _prodsOprefix _prodsOsyn _prodsOsynMap _prodsOvcount)
         _lhsOinhMap' :: Map Identifier Attributes
         _lhsOinhMap' = rule114 arg_inh_ arg_nt_
         _lhsOsynMap' :: Map Identifier Attributes
         _lhsOsynMap' = rule115 arg_nt_ arg_syn_
         _prodsOnt = rule116 arg_nt_
         _prodsOinh = rule117 arg_inh_
         _prodsOsyn = rule118 arg_syn_
         _mergeMap = rule119 _lhsImergeMap arg_nt_
         _aroundMap = rule120 _lhsIaroundMap arg_nt_
         _ntattrs = rule121 arg_inh_ arg_nt_ arg_syn_
         _lhsOntattrs :: Seq (Vertex,NTAttr)
         _lhsOntattrs = rule122 _lhsIacount _ntattrs
         _lhsOacount :: Int
         _lhsOacount = rule123 _lhsIacount arg_inh_ arg_syn_
         _lhsOaranges :: Seq (Int,Int,Int)
         _lhsOaranges = rule124 _lhsIacount arg_inh_ arg_syn_
         _lhsOnonts :: [(NontermIdent,[ConstructorIdent])]
         _lhsOnonts = rule125 _prodsIcons arg_nt_
         _cInter = rule126 _lhsIcInterfaceMap _lhsIo_dovisit arg_inh_ arg_nt_ arg_syn_
         _lhsOcNonterminal :: CNonterminal
         _lhsOcNonterminal = rule127 _cInter _prodsIcProductions arg_inh_ arg_nt_ arg_params_ arg_syn_
         _lhsOmirage :: Mirage.Nonterminal
         _lhsOmirage = rule128 _prodsImirages arg_inh_ arg_nt_ arg_params_ arg_syn_
         _lhsOadditionalDep :: Seq Edge
         _lhsOadditionalDep = rule129 _prodsIadditionalDep
         _lhsOaroundDep :: Seq Edge
         _lhsOaroundDep = rule130 _prodsIaroundDep
         _lhsOdirectDep :: Seq Edge
         _lhsOdirectDep = rule131 _prodsIdirectDep
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule132 _prodsIerrors
         _lhsOinstDep :: Seq Edge
         _lhsOinstDep = rule133 _prodsIinstDep
         _lhsOmergeDep :: Seq Edge
         _lhsOmergeDep = rule134 _prodsImergeDep
         _lhsOnAutoRules :: Int
         _lhsOnAutoRules = rule135 _prodsInAutoRules
         _lhsOnExplicitRules :: Int
         _lhsOnExplicitRules = rule136 _prodsInExplicitRules
         _lhsOrules :: Seq (Vertex,CRule)
         _lhsOrules = rule137 _prodsIrules
         _lhsOvcount :: Int
         _lhsOvcount = rule138 _prodsIvcount
         _prodsOallnts = rule139 _lhsIallnts
         _prodsOaroundMap = rule140 _aroundMap
         _prodsOcVisitsMap = rule141 _lhsIcVisitsMap
         _prodsOinhMap = rule142 _lhsIinhMap
         _prodsOmanualAttrDepMap = rule143 _lhsImanualAttrDepMap
         _prodsOmergeMap = rule144 _mergeMap
         _prodsOo_case = rule145 _lhsIo_case
         _prodsOo_cata = rule146 _lhsIo_cata
         _prodsOo_dovisit = rule147 _lhsIo_dovisit
         _prodsOo_newtypes = rule148 _lhsIo_newtypes
         _prodsOo_rename = rule149 _lhsIo_rename
         _prodsOo_sem = rule150 _lhsIo_sem
         _prodsOo_sig = rule151 _lhsIo_sig
         _prodsOo_unbox = rule152 _lhsIo_unbox
         _prodsOo_wantvisit = rule153 _lhsIo_wantvisit
         _prodsOoptions = rule154 _lhsIoptions
         _prodsOprefix = rule155 _lhsIprefix
         _prodsOsynMap = rule156 _lhsIsynMap
         _prodsOvcount = rule157 _lhsIvcount
         __result_ = T_Nonterminal_vOut13 _lhsOacount _lhsOadditionalDep _lhsOaranges _lhsOaroundDep _lhsOcNonterminal _lhsOdirectDep _lhsOerrors _lhsOinhMap' _lhsOinstDep _lhsOmergeDep _lhsOmirage _lhsOnAutoRules _lhsOnExplicitRules _lhsOnonts _lhsOntattrs _lhsOrules _lhsOsynMap' _lhsOvcount
         in __result_ )
     in C_Nonterminal_s14 v13
   {-# INLINE rule114 #-}
   {-# LINE 7 "src-ag/DistChildAttr.ag" #-}
   rule114 = \ inh_ nt_ ->
                                 {-# LINE 7 "src-ag/DistChildAttr.ag" #-}
                                 Map.singleton nt_ inh_
                                 {-# LINE 1285 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule115 #-}
   {-# LINE 8 "src-ag/DistChildAttr.ag" #-}
   rule115 = \ nt_ syn_ ->
                                 {-# LINE 8 "src-ag/DistChildAttr.ag" #-}
                                 Map.singleton nt_ syn_
                                 {-# LINE 1291 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule116 #-}
   {-# LINE 97 "src-ag/Order.ag" #-}
   rule116 = \ nt_ ->
                               {-# LINE 97 "src-ag/Order.ag" #-}
                               nt_
                               {-# LINE 1297 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule117 #-}
   {-# LINE 100 "src-ag/Order.ag" #-}
   rule117 = \ inh_ ->
                               {-# LINE 100 "src-ag/Order.ag" #-}
                               inh_
                               {-# LINE 1303 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule118 #-}
   {-# LINE 101 "src-ag/Order.ag" #-}
   rule118 = \ syn_ ->
                               {-# LINE 101 "src-ag/Order.ag" #-}
                               syn_
                               {-# LINE 1309 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule119 #-}
   {-# LINE 360 "src-ag/Order.ag" #-}
   rule119 = \ ((_lhsImergeMap) :: Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier])))) nt_ ->
                                                {-# LINE 360 "src-ag/Order.ag" #-}
                                                Map.findWithDefault Map.empty nt_ _lhsImergeMap
                                                {-# LINE 1315 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule120 #-}
   {-# LINE 413 "src-ag/Order.ag" #-}
   rule120 = \ ((_lhsIaroundMap) :: Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) nt_ ->
                                                 {-# LINE 413 "src-ag/Order.ag" #-}
                                                 Map.findWithDefault Map.empty nt_ _lhsIaroundMap
                                                 {-# LINE 1321 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule121 #-}
   {-# LINE 511 "src-ag/Order.ag" #-}
   rule121 = \ inh_ nt_ syn_ ->
                                 {-# LINE 511 "src-ag/Order.ag" #-}
                                 [ NTAInh nt_ inh tp | (inh,tp) <- Map.assocs inh_ ]
                                 ++ [NTASyn nt_ syn tp | (syn,tp) <- Map.assocs syn_ ]
                                 {-# LINE 1328 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule122 #-}
   {-# LINE 513 "src-ag/Order.ag" #-}
   rule122 = \ ((_lhsIacount) :: Int) _ntattrs ->
                                {-# LINE 513 "src-ag/Order.ag" #-}
                                Seq.fromList (zip [_lhsIacount ..] _ntattrs)
                                {-# LINE 1334 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule123 #-}
   {-# LINE 514 "src-ag/Order.ag" #-}
   rule123 = \ ((_lhsIacount) :: Int) inh_ syn_ ->
                                {-# LINE 514 "src-ag/Order.ag" #-}
                                _lhsIacount + Map.size inh_ + Map.size syn_
                                {-# LINE 1340 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule124 #-}
   {-# LINE 515 "src-ag/Order.ag" #-}
   rule124 = \ ((_lhsIacount) :: Int) inh_ syn_ ->
                                 {-# LINE 515 "src-ag/Order.ag" #-}
                                 Seq.singleton
                                  (_lhsIacount
                                  ,_lhsIacount + Map.size inh_
                                  ,_lhsIacount + Map.size syn_ + Map.size inh_ - 1)
                                 {-# LINE 1349 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule125 #-}
   {-# LINE 524 "src-ag/Order.ag" #-}
   rule125 = \ ((_prodsIcons) :: [ConstructorIdent]) nt_ ->
                                {-# LINE 524 "src-ag/Order.ag" #-}
                                [(nt_,_prodsIcons)]
                                {-# LINE 1355 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule126 #-}
   {-# LINE 601 "src-ag/Order.ag" #-}
   rule126 = \ ((_lhsIcInterfaceMap) :: CInterfaceMap) ((_lhsIo_dovisit) :: Bool) inh_ nt_ syn_ ->
                                 {-# LINE 601 "src-ag/Order.ag" #-}
                                 if  _lhsIo_dovisit
                                        then findWithErr1 "Nonterminal.cInter" nt_ _lhsIcInterfaceMap
                                        else CInterface [CSegment inh_ syn_]
                                 {-# LINE 1363 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule127 #-}
   {-# LINE 629 "src-ag/Order.ag" #-}
   rule127 = \ _cInter ((_prodsIcProductions) :: CProductions) inh_ nt_ params_ syn_ ->
                                       {-# LINE 629 "src-ag/Order.ag" #-}
                                       CNonterminal nt_ params_ inh_ syn_ _prodsIcProductions _cInter
                                       {-# LINE 1369 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule128 #-}
   {-# LINE 30 "src-ag/TfmToMirage.ag" #-}
   rule128 = \ ((_prodsImirages) :: [Mirage.Production]) inh_ nt_ params_ syn_ ->
          {-# LINE 30 "src-ag/TfmToMirage.ag" #-}
          Mirage.Nonterminal
            (getName nt_)
            (map getName params_)
            (Map.foldrWithKey (\k x xs -> Mirage.Attribute (getName k) (typeToMirage x) : xs) [] inh_)
            (Map.foldrWithKey (\k x xs -> Mirage.Attribute (getName k) (typeToMirage x) : xs) [] syn_)
            _prodsImirages
          {-# LINE 1380 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule129 #-}
   rule129 = \ ((_prodsIadditionalDep) :: Seq Edge) ->
     _prodsIadditionalDep
   {-# INLINE rule130 #-}
   rule130 = \ ((_prodsIaroundDep) :: Seq Edge) ->
     _prodsIaroundDep
   {-# INLINE rule131 #-}
   rule131 = \ ((_prodsIdirectDep) :: Seq Edge) ->
     _prodsIdirectDep
   {-# INLINE rule132 #-}
   rule132 = \ ((_prodsIerrors) :: Seq Error) ->
     _prodsIerrors
   {-# INLINE rule133 #-}
   rule133 = \ ((_prodsIinstDep) :: Seq Edge) ->
     _prodsIinstDep
   {-# INLINE rule134 #-}
   rule134 = \ ((_prodsImergeDep) :: Seq Edge) ->
     _prodsImergeDep
   {-# INLINE rule135 #-}
   rule135 = \ ((_prodsInAutoRules) :: Int) ->
     _prodsInAutoRules
   {-# INLINE rule136 #-}
   rule136 = \ ((_prodsInExplicitRules) :: Int) ->
     _prodsInExplicitRules
   {-# INLINE rule137 #-}
   rule137 = \ ((_prodsIrules) :: Seq (Vertex,CRule)) ->
     _prodsIrules
   {-# INLINE rule138 #-}
   rule138 = \ ((_prodsIvcount) :: Int) ->
     _prodsIvcount
   {-# INLINE rule139 #-}
   rule139 = \ ((_lhsIallnts) :: [Identifier]) ->
     _lhsIallnts
   {-# INLINE rule140 #-}
   rule140 = \ _aroundMap ->
     _aroundMap
   {-# INLINE rule141 #-}
   rule141 = \ ((_lhsIcVisitsMap) :: CVisitsMap) ->
     _lhsIcVisitsMap
   {-# INLINE rule142 #-}
   rule142 = \ ((_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# INLINE rule143 #-}
   rule143 = \ ((_lhsImanualAttrDepMap) :: AttrOrderMap) ->
     _lhsImanualAttrDepMap
   {-# INLINE rule144 #-}
   rule144 = \ _mergeMap ->
     _mergeMap
   {-# INLINE rule145 #-}
   rule145 = \ ((_lhsIo_case) :: Bool) ->
     _lhsIo_case
   {-# INLINE rule146 #-}
   rule146 = \ ((_lhsIo_cata) :: Bool) ->
     _lhsIo_cata
   {-# INLINE rule147 #-}
   rule147 = \ ((_lhsIo_dovisit) :: Bool) ->
     _lhsIo_dovisit
   {-# INLINE rule148 #-}
   rule148 = \ ((_lhsIo_newtypes) :: Bool) ->
     _lhsIo_newtypes
   {-# INLINE rule149 #-}
   rule149 = \ ((_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# INLINE rule150 #-}
   rule150 = \ ((_lhsIo_sem) :: Bool) ->
     _lhsIo_sem
   {-# INLINE rule151 #-}
   rule151 = \ ((_lhsIo_sig) :: Bool) ->
     _lhsIo_sig
   {-# INLINE rule152 #-}
   rule152 = \ ((_lhsIo_unbox) :: Bool) ->
     _lhsIo_unbox
   {-# INLINE rule153 #-}
   rule153 = \ ((_lhsIo_wantvisit) :: Bool) ->
     _lhsIo_wantvisit
   {-# INLINE rule154 #-}
   rule154 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule155 #-}
   rule155 = \ ((_lhsIprefix) :: String) ->
     _lhsIprefix
   {-# INLINE rule156 #-}
   rule156 = \ ((_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
   {-# INLINE rule157 #-}
   rule157 = \ ((_lhsIvcount) :: Int) ->
     _lhsIvcount

-- Nonterminals ------------------------------------------------
-- wrapper
data Inh_Nonterminals  = Inh_Nonterminals { acount_Inh_Nonterminals :: (Int), allnts_Inh_Nonterminals :: ([Identifier]), aroundMap_Inh_Nonterminals :: (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))), cInterfaceMap_Inh_Nonterminals :: (CInterfaceMap), cVisitsMap_Inh_Nonterminals :: (CVisitsMap), inhMap_Inh_Nonterminals :: (Map Identifier Attributes), manualAttrDepMap_Inh_Nonterminals :: (AttrOrderMap), mergeMap_Inh_Nonterminals :: (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier])))), o_case_Inh_Nonterminals :: (Bool), o_cata_Inh_Nonterminals :: (Bool), o_data_Inh_Nonterminals :: (Bool), o_dovisit_Inh_Nonterminals :: (Bool), o_newtypes_Inh_Nonterminals :: (Bool), o_rename_Inh_Nonterminals :: (Bool), o_sem_Inh_Nonterminals :: (Bool), o_sig_Inh_Nonterminals :: (Bool), o_unbox_Inh_Nonterminals :: (Bool), o_wantvisit_Inh_Nonterminals :: (Bool), options_Inh_Nonterminals :: (Options), prefix_Inh_Nonterminals :: (String), synMap_Inh_Nonterminals :: (Map Identifier Attributes), vcount_Inh_Nonterminals :: (Int) }
data Syn_Nonterminals  = Syn_Nonterminals { acount_Syn_Nonterminals :: (Int), additionalDep_Syn_Nonterminals :: (Seq Edge), aranges_Syn_Nonterminals :: (Seq (Int,Int,Int)), aroundDep_Syn_Nonterminals :: (Seq Edge), cNonterminals_Syn_Nonterminals :: (CNonterminals), directDep_Syn_Nonterminals :: (Seq Edge), errors_Syn_Nonterminals :: (Seq Error), inhMap'_Syn_Nonterminals :: (Map Identifier Attributes), instDep_Syn_Nonterminals :: (Seq Edge), mergeDep_Syn_Nonterminals :: (Seq Edge), mirages_Syn_Nonterminals :: ([Mirage.Nonterminal]), nAutoRules_Syn_Nonterminals :: (Int), nExplicitRules_Syn_Nonterminals :: (Int), nonts_Syn_Nonterminals :: ([(NontermIdent,[ConstructorIdent])]), ntattrs_Syn_Nonterminals :: (Seq (Vertex,NTAttr)), rules_Syn_Nonterminals :: (Seq (Vertex,CRule)), synMap'_Syn_Nonterminals :: (Map Identifier Attributes), vcount_Syn_Nonterminals :: (Int) }
{-# INLINABLE wrap_Nonterminals #-}
wrap_Nonterminals :: T_Nonterminals  -> Inh_Nonterminals  -> (Syn_Nonterminals )
wrap_Nonterminals (T_Nonterminals act) (Inh_Nonterminals _lhsIacount _lhsIallnts _lhsIaroundMap _lhsIcInterfaceMap _lhsIcVisitsMap _lhsIinhMap _lhsImanualAttrDepMap _lhsImergeMap _lhsIo_case _lhsIo_cata _lhsIo_data _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_unbox _lhsIo_wantvisit _lhsIoptions _lhsIprefix _lhsIsynMap _lhsIvcount) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg16 = T_Nonterminals_vIn16 _lhsIacount _lhsIallnts _lhsIaroundMap _lhsIcInterfaceMap _lhsIcVisitsMap _lhsIinhMap _lhsImanualAttrDepMap _lhsImergeMap _lhsIo_case _lhsIo_cata _lhsIo_data _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_unbox _lhsIo_wantvisit _lhsIoptions _lhsIprefix _lhsIsynMap _lhsIvcount
        (T_Nonterminals_vOut16 _lhsOacount _lhsOadditionalDep _lhsOaranges _lhsOaroundDep _lhsOcNonterminals _lhsOdirectDep _lhsOerrors _lhsOinhMap' _lhsOinstDep _lhsOmergeDep _lhsOmirages _lhsOnAutoRules _lhsOnExplicitRules _lhsOnonts _lhsOntattrs _lhsOrules _lhsOsynMap' _lhsOvcount) <- return (inv_Nonterminals_s17 sem arg16)
        return (Syn_Nonterminals _lhsOacount _lhsOadditionalDep _lhsOaranges _lhsOaroundDep _lhsOcNonterminals _lhsOdirectDep _lhsOerrors _lhsOinhMap' _lhsOinstDep _lhsOmergeDep _lhsOmirages _lhsOnAutoRules _lhsOnExplicitRules _lhsOnonts _lhsOntattrs _lhsOrules _lhsOsynMap' _lhsOvcount)
   )

-- cata
{-# NOINLINE sem_Nonterminals #-}
sem_Nonterminals :: Nonterminals  -> T_Nonterminals 
sem_Nonterminals list = Prelude.foldr sem_Nonterminals_Cons sem_Nonterminals_Nil (Prelude.map sem_Nonterminal list)

-- semantic domain
newtype T_Nonterminals  = T_Nonterminals {
                                         attach_T_Nonterminals :: Identity (T_Nonterminals_s17 )
                                         }
newtype T_Nonterminals_s17  = C_Nonterminals_s17 {
                                                 inv_Nonterminals_s17 :: (T_Nonterminals_v16 )
                                                 }
data T_Nonterminals_s18  = C_Nonterminals_s18
type T_Nonterminals_v16  = (T_Nonterminals_vIn16 ) -> (T_Nonterminals_vOut16 )
data T_Nonterminals_vIn16  = T_Nonterminals_vIn16 (Int) ([Identifier]) (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) (CInterfaceMap) (CVisitsMap) (Map Identifier Attributes) (AttrOrderMap) (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier])))) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Options) (String) (Map Identifier Attributes) (Int)
data T_Nonterminals_vOut16  = T_Nonterminals_vOut16 (Int) (Seq Edge) (Seq (Int,Int,Int)) (Seq Edge) (CNonterminals) (Seq Edge) (Seq Error) (Map Identifier Attributes) (Seq Edge) (Seq Edge) ([Mirage.Nonterminal]) (Int) (Int) ([(NontermIdent,[ConstructorIdent])]) (Seq (Vertex,NTAttr)) (Seq (Vertex,CRule)) (Map Identifier Attributes) (Int)
{-# NOINLINE sem_Nonterminals_Cons #-}
sem_Nonterminals_Cons :: T_Nonterminal  -> T_Nonterminals  -> T_Nonterminals 
sem_Nonterminals_Cons arg_hd_ arg_tl_ = T_Nonterminals (return st17) where
   {-# NOINLINE st17 #-}
   st17 = let
      v16 :: T_Nonterminals_v16 
      v16 = \ (T_Nonterminals_vIn16 _lhsIacount _lhsIallnts _lhsIaroundMap _lhsIcInterfaceMap _lhsIcVisitsMap _lhsIinhMap _lhsImanualAttrDepMap _lhsImergeMap _lhsIo_case _lhsIo_cata _lhsIo_data _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_unbox _lhsIo_wantvisit _lhsIoptions _lhsIprefix _lhsIsynMap _lhsIvcount) -> ( let
         _hdX14 = Control.Monad.Identity.runIdentity (attach_T_Nonterminal (arg_hd_))
         _tlX17 = Control.Monad.Identity.runIdentity (attach_T_Nonterminals (arg_tl_))
         (T_Nonterminal_vOut13 _hdIacount _hdIadditionalDep _hdIaranges _hdIaroundDep _hdIcNonterminal _hdIdirectDep _hdIerrors _hdIinhMap' _hdIinstDep _hdImergeDep _hdImirage _hdInAutoRules _hdInExplicitRules _hdInonts _hdIntattrs _hdIrules _hdIsynMap' _hdIvcount) = inv_Nonterminal_s14 _hdX14 (T_Nonterminal_vIn13 _hdOacount _hdOallnts _hdOaroundMap _hdOcInterfaceMap _hdOcVisitsMap _hdOinhMap _hdOmanualAttrDepMap _hdOmergeMap _hdOo_case _hdOo_cata _hdOo_data _hdOo_dovisit _hdOo_newtypes _hdOo_rename _hdOo_sem _hdOo_sig _hdOo_unbox _hdOo_wantvisit _hdOoptions _hdOprefix _hdOsynMap _hdOvcount)
         (T_Nonterminals_vOut16 _tlIacount _tlIadditionalDep _tlIaranges _tlIaroundDep _tlIcNonterminals _tlIdirectDep _tlIerrors _tlIinhMap' _tlIinstDep _tlImergeDep _tlImirages _tlInAutoRules _tlInExplicitRules _tlInonts _tlIntattrs _tlIrules _tlIsynMap' _tlIvcount) = inv_Nonterminals_s17 _tlX17 (T_Nonterminals_vIn16 _tlOacount _tlOallnts _tlOaroundMap _tlOcInterfaceMap _tlOcVisitsMap _tlOinhMap _tlOmanualAttrDepMap _tlOmergeMap _tlOo_case _tlOo_cata _tlOo_data _tlOo_dovisit _tlOo_newtypes _tlOo_rename _tlOo_sem _tlOo_sig _tlOo_unbox _tlOo_wantvisit _tlOoptions _tlOprefix _tlOsynMap _tlOvcount)
         _lhsOcNonterminals :: CNonterminals
         _lhsOcNonterminals = rule158 _hdIcNonterminal _tlIcNonterminals
         _lhsOmirages :: [Mirage.Nonterminal]
         _lhsOmirages = rule159 _hdImirage _tlImirages
         _lhsOadditionalDep :: Seq Edge
         _lhsOadditionalDep = rule160 _hdIadditionalDep _tlIadditionalDep
         _lhsOaranges :: Seq (Int,Int,Int)
         _lhsOaranges = rule161 _hdIaranges _tlIaranges
         _lhsOaroundDep :: Seq Edge
         _lhsOaroundDep = rule162 _hdIaroundDep _tlIaroundDep
         _lhsOdirectDep :: Seq Edge
         _lhsOdirectDep = rule163 _hdIdirectDep _tlIdirectDep
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule164 _hdIerrors _tlIerrors
         _lhsOinhMap' :: Map Identifier Attributes
         _lhsOinhMap' = rule165 _hdIinhMap' _tlIinhMap'
         _lhsOinstDep :: Seq Edge
         _lhsOinstDep = rule166 _hdIinstDep _tlIinstDep
         _lhsOmergeDep :: Seq Edge
         _lhsOmergeDep = rule167 _hdImergeDep _tlImergeDep
         _lhsOnAutoRules :: Int
         _lhsOnAutoRules = rule168 _hdInAutoRules _tlInAutoRules
         _lhsOnExplicitRules :: Int
         _lhsOnExplicitRules = rule169 _hdInExplicitRules _tlInExplicitRules
         _lhsOnonts :: [(NontermIdent,[ConstructorIdent])]
         _lhsOnonts = rule170 _hdInonts _tlInonts
         _lhsOntattrs :: Seq (Vertex,NTAttr)
         _lhsOntattrs = rule171 _hdIntattrs _tlIntattrs
         _lhsOrules :: Seq (Vertex,CRule)
         _lhsOrules = rule172 _hdIrules _tlIrules
         _lhsOsynMap' :: Map Identifier Attributes
         _lhsOsynMap' = rule173 _hdIsynMap' _tlIsynMap'
         _lhsOacount :: Int
         _lhsOacount = rule174 _tlIacount
         _lhsOvcount :: Int
         _lhsOvcount = rule175 _tlIvcount
         _hdOacount = rule176 _lhsIacount
         _hdOallnts = rule177 _lhsIallnts
         _hdOaroundMap = rule178 _lhsIaroundMap
         _hdOcInterfaceMap = rule179 _lhsIcInterfaceMap
         _hdOcVisitsMap = rule180 _lhsIcVisitsMap
         _hdOinhMap = rule181 _lhsIinhMap
         _hdOmanualAttrDepMap = rule182 _lhsImanualAttrDepMap
         _hdOmergeMap = rule183 _lhsImergeMap
         _hdOo_case = rule184 _lhsIo_case
         _hdOo_cata = rule185 _lhsIo_cata
         _hdOo_data = rule186 _lhsIo_data
         _hdOo_dovisit = rule187 _lhsIo_dovisit
         _hdOo_newtypes = rule188 _lhsIo_newtypes
         _hdOo_rename = rule189 _lhsIo_rename
         _hdOo_sem = rule190 _lhsIo_sem
         _hdOo_sig = rule191 _lhsIo_sig
         _hdOo_unbox = rule192 _lhsIo_unbox
         _hdOo_wantvisit = rule193 _lhsIo_wantvisit
         _hdOoptions = rule194 _lhsIoptions
         _hdOprefix = rule195 _lhsIprefix
         _hdOsynMap = rule196 _lhsIsynMap
         _hdOvcount = rule197 _lhsIvcount
         _tlOacount = rule198 _hdIacount
         _tlOallnts = rule199 _lhsIallnts
         _tlOaroundMap = rule200 _lhsIaroundMap
         _tlOcInterfaceMap = rule201 _lhsIcInterfaceMap
         _tlOcVisitsMap = rule202 _lhsIcVisitsMap
         _tlOinhMap = rule203 _lhsIinhMap
         _tlOmanualAttrDepMap = rule204 _lhsImanualAttrDepMap
         _tlOmergeMap = rule205 _lhsImergeMap
         _tlOo_case = rule206 _lhsIo_case
         _tlOo_cata = rule207 _lhsIo_cata
         _tlOo_data = rule208 _lhsIo_data
         _tlOo_dovisit = rule209 _lhsIo_dovisit
         _tlOo_newtypes = rule210 _lhsIo_newtypes
         _tlOo_rename = rule211 _lhsIo_rename
         _tlOo_sem = rule212 _lhsIo_sem
         _tlOo_sig = rule213 _lhsIo_sig
         _tlOo_unbox = rule214 _lhsIo_unbox
         _tlOo_wantvisit = rule215 _lhsIo_wantvisit
         _tlOoptions = rule216 _lhsIoptions
         _tlOprefix = rule217 _lhsIprefix
         _tlOsynMap = rule218 _lhsIsynMap
         _tlOvcount = rule219 _hdIvcount
         __result_ = T_Nonterminals_vOut16 _lhsOacount _lhsOadditionalDep _lhsOaranges _lhsOaroundDep _lhsOcNonterminals _lhsOdirectDep _lhsOerrors _lhsOinhMap' _lhsOinstDep _lhsOmergeDep _lhsOmirages _lhsOnAutoRules _lhsOnExplicitRules _lhsOnonts _lhsOntattrs _lhsOrules _lhsOsynMap' _lhsOvcount
         in __result_ )
     in C_Nonterminals_s17 v16
   {-# INLINE rule158 #-}
   {-# LINE 626 "src-ag/Order.ag" #-}
   rule158 = \ ((_hdIcNonterminal) :: CNonterminal) ((_tlIcNonterminals) :: CNonterminals) ->
                                 {-# LINE 626 "src-ag/Order.ag" #-}
                                 _hdIcNonterminal : _tlIcNonterminals
                                 {-# LINE 1598 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule159 #-}
   {-# LINE 25 "src-ag/TfmToMirage.ag" #-}
   rule159 = \ ((_hdImirage) :: Mirage.Nonterminal) ((_tlImirages) :: [Mirage.Nonterminal]) ->
                         {-# LINE 25 "src-ag/TfmToMirage.ag" #-}
                         _hdImirage : _tlImirages
                         {-# LINE 1604 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule160 #-}
   rule160 = \ ((_hdIadditionalDep) :: Seq Edge) ((_tlIadditionalDep) :: Seq Edge) ->
     _hdIadditionalDep Seq.>< _tlIadditionalDep
   {-# INLINE rule161 #-}
   rule161 = \ ((_hdIaranges) :: Seq (Int,Int,Int)) ((_tlIaranges) :: Seq (Int,Int,Int)) ->
     _hdIaranges Seq.>< _tlIaranges
   {-# INLINE rule162 #-}
   rule162 = \ ((_hdIaroundDep) :: Seq Edge) ((_tlIaroundDep) :: Seq Edge) ->
     _hdIaroundDep Seq.>< _tlIaroundDep
   {-# INLINE rule163 #-}
   rule163 = \ ((_hdIdirectDep) :: Seq Edge) ((_tlIdirectDep) :: Seq Edge) ->
     _hdIdirectDep Seq.>< _tlIdirectDep
   {-# INLINE rule164 #-}
   rule164 = \ ((_hdIerrors) :: Seq Error) ((_tlIerrors) :: Seq Error) ->
     _hdIerrors Seq.>< _tlIerrors
   {-# INLINE rule165 #-}
   rule165 = \ ((_hdIinhMap') :: Map Identifier Attributes) ((_tlIinhMap') :: Map Identifier Attributes) ->
     _hdIinhMap' `Map.union` _tlIinhMap'
   {-# INLINE rule166 #-}
   rule166 = \ ((_hdIinstDep) :: Seq Edge) ((_tlIinstDep) :: Seq Edge) ->
     _hdIinstDep Seq.>< _tlIinstDep
   {-# INLINE rule167 #-}
   rule167 = \ ((_hdImergeDep) :: Seq Edge) ((_tlImergeDep) :: Seq Edge) ->
     _hdImergeDep Seq.>< _tlImergeDep
   {-# INLINE rule168 #-}
   rule168 = \ ((_hdInAutoRules) :: Int) ((_tlInAutoRules) :: Int) ->
     _hdInAutoRules + _tlInAutoRules
   {-# INLINE rule169 #-}
   rule169 = \ ((_hdInExplicitRules) :: Int) ((_tlInExplicitRules) :: Int) ->
     _hdInExplicitRules + _tlInExplicitRules
   {-# INLINE rule170 #-}
   rule170 = \ ((_hdInonts) :: [(NontermIdent,[ConstructorIdent])]) ((_tlInonts) :: [(NontermIdent,[ConstructorIdent])]) ->
     _hdInonts ++ _tlInonts
   {-# INLINE rule171 #-}
   rule171 = \ ((_hdIntattrs) :: Seq (Vertex,NTAttr)) ((_tlIntattrs) :: Seq (Vertex,NTAttr)) ->
     _hdIntattrs Seq.>< _tlIntattrs
   {-# INLINE rule172 #-}
   rule172 = \ ((_hdIrules) :: Seq (Vertex,CRule)) ((_tlIrules) :: Seq (Vertex,CRule)) ->
     _hdIrules Seq.>< _tlIrules
   {-# INLINE rule173 #-}
   rule173 = \ ((_hdIsynMap') :: Map Identifier Attributes) ((_tlIsynMap') :: Map Identifier Attributes) ->
     _hdIsynMap' `Map.union` _tlIsynMap'
   {-# INLINE rule174 #-}
   rule174 = \ ((_tlIacount) :: Int) ->
     _tlIacount
   {-# INLINE rule175 #-}
   rule175 = \ ((_tlIvcount) :: Int) ->
     _tlIvcount
   {-# INLINE rule176 #-}
   rule176 = \ ((_lhsIacount) :: Int) ->
     _lhsIacount
   {-# INLINE rule177 #-}
   rule177 = \ ((_lhsIallnts) :: [Identifier]) ->
     _lhsIallnts
   {-# INLINE rule178 #-}
   rule178 = \ ((_lhsIaroundMap) :: Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) ->
     _lhsIaroundMap
   {-# INLINE rule179 #-}
   rule179 = \ ((_lhsIcInterfaceMap) :: CInterfaceMap) ->
     _lhsIcInterfaceMap
   {-# INLINE rule180 #-}
   rule180 = \ ((_lhsIcVisitsMap) :: CVisitsMap) ->
     _lhsIcVisitsMap
   {-# INLINE rule181 #-}
   rule181 = \ ((_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# INLINE rule182 #-}
   rule182 = \ ((_lhsImanualAttrDepMap) :: AttrOrderMap) ->
     _lhsImanualAttrDepMap
   {-# INLINE rule183 #-}
   rule183 = \ ((_lhsImergeMap) :: Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier])))) ->
     _lhsImergeMap
   {-# INLINE rule184 #-}
   rule184 = \ ((_lhsIo_case) :: Bool) ->
     _lhsIo_case
   {-# INLINE rule185 #-}
   rule185 = \ ((_lhsIo_cata) :: Bool) ->
     _lhsIo_cata
   {-# INLINE rule186 #-}
   rule186 = \ ((_lhsIo_data) :: Bool) ->
     _lhsIo_data
   {-# INLINE rule187 #-}
   rule187 = \ ((_lhsIo_dovisit) :: Bool) ->
     _lhsIo_dovisit
   {-# INLINE rule188 #-}
   rule188 = \ ((_lhsIo_newtypes) :: Bool) ->
     _lhsIo_newtypes
   {-# INLINE rule189 #-}
   rule189 = \ ((_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# INLINE rule190 #-}
   rule190 = \ ((_lhsIo_sem) :: Bool) ->
     _lhsIo_sem
   {-# INLINE rule191 #-}
   rule191 = \ ((_lhsIo_sig) :: Bool) ->
     _lhsIo_sig
   {-# INLINE rule192 #-}
   rule192 = \ ((_lhsIo_unbox) :: Bool) ->
     _lhsIo_unbox
   {-# INLINE rule193 #-}
   rule193 = \ ((_lhsIo_wantvisit) :: Bool) ->
     _lhsIo_wantvisit
   {-# INLINE rule194 #-}
   rule194 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule195 #-}
   rule195 = \ ((_lhsIprefix) :: String) ->
     _lhsIprefix
   {-# INLINE rule196 #-}
   rule196 = \ ((_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
   {-# INLINE rule197 #-}
   rule197 = \ ((_lhsIvcount) :: Int) ->
     _lhsIvcount
   {-# INLINE rule198 #-}
   rule198 = \ ((_hdIacount) :: Int) ->
     _hdIacount
   {-# INLINE rule199 #-}
   rule199 = \ ((_lhsIallnts) :: [Identifier]) ->
     _lhsIallnts
   {-# INLINE rule200 #-}
   rule200 = \ ((_lhsIaroundMap) :: Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) ->
     _lhsIaroundMap
   {-# INLINE rule201 #-}
   rule201 = \ ((_lhsIcInterfaceMap) :: CInterfaceMap) ->
     _lhsIcInterfaceMap
   {-# INLINE rule202 #-}
   rule202 = \ ((_lhsIcVisitsMap) :: CVisitsMap) ->
     _lhsIcVisitsMap
   {-# INLINE rule203 #-}
   rule203 = \ ((_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# INLINE rule204 #-}
   rule204 = \ ((_lhsImanualAttrDepMap) :: AttrOrderMap) ->
     _lhsImanualAttrDepMap
   {-# INLINE rule205 #-}
   rule205 = \ ((_lhsImergeMap) :: Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier])))) ->
     _lhsImergeMap
   {-# INLINE rule206 #-}
   rule206 = \ ((_lhsIo_case) :: Bool) ->
     _lhsIo_case
   {-# INLINE rule207 #-}
   rule207 = \ ((_lhsIo_cata) :: Bool) ->
     _lhsIo_cata
   {-# INLINE rule208 #-}
   rule208 = \ ((_lhsIo_data) :: Bool) ->
     _lhsIo_data
   {-# INLINE rule209 #-}
   rule209 = \ ((_lhsIo_dovisit) :: Bool) ->
     _lhsIo_dovisit
   {-# INLINE rule210 #-}
   rule210 = \ ((_lhsIo_newtypes) :: Bool) ->
     _lhsIo_newtypes
   {-# INLINE rule211 #-}
   rule211 = \ ((_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# INLINE rule212 #-}
   rule212 = \ ((_lhsIo_sem) :: Bool) ->
     _lhsIo_sem
   {-# INLINE rule213 #-}
   rule213 = \ ((_lhsIo_sig) :: Bool) ->
     _lhsIo_sig
   {-# INLINE rule214 #-}
   rule214 = \ ((_lhsIo_unbox) :: Bool) ->
     _lhsIo_unbox
   {-# INLINE rule215 #-}
   rule215 = \ ((_lhsIo_wantvisit) :: Bool) ->
     _lhsIo_wantvisit
   {-# INLINE rule216 #-}
   rule216 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule217 #-}
   rule217 = \ ((_lhsIprefix) :: String) ->
     _lhsIprefix
   {-# INLINE rule218 #-}
   rule218 = \ ((_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
   {-# INLINE rule219 #-}
   rule219 = \ ((_hdIvcount) :: Int) ->
     _hdIvcount
{-# NOINLINE sem_Nonterminals_Nil #-}
sem_Nonterminals_Nil ::  T_Nonterminals 
sem_Nonterminals_Nil  = T_Nonterminals (return st17) where
   {-# NOINLINE st17 #-}
   st17 = let
      v16 :: T_Nonterminals_v16 
      v16 = \ (T_Nonterminals_vIn16 _lhsIacount _lhsIallnts _lhsIaroundMap _lhsIcInterfaceMap _lhsIcVisitsMap _lhsIinhMap _lhsImanualAttrDepMap _lhsImergeMap _lhsIo_case _lhsIo_cata _lhsIo_data _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_unbox _lhsIo_wantvisit _lhsIoptions _lhsIprefix _lhsIsynMap _lhsIvcount) -> ( let
         _lhsOcNonterminals :: CNonterminals
         _lhsOcNonterminals = rule220  ()
         _lhsOmirages :: [Mirage.Nonterminal]
         _lhsOmirages = rule221  ()
         _lhsOadditionalDep :: Seq Edge
         _lhsOadditionalDep = rule222  ()
         _lhsOaranges :: Seq (Int,Int,Int)
         _lhsOaranges = rule223  ()
         _lhsOaroundDep :: Seq Edge
         _lhsOaroundDep = rule224  ()
         _lhsOdirectDep :: Seq Edge
         _lhsOdirectDep = rule225  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule226  ()
         _lhsOinhMap' :: Map Identifier Attributes
         _lhsOinhMap' = rule227  ()
         _lhsOinstDep :: Seq Edge
         _lhsOinstDep = rule228  ()
         _lhsOmergeDep :: Seq Edge
         _lhsOmergeDep = rule229  ()
         _lhsOnAutoRules :: Int
         _lhsOnAutoRules = rule230  ()
         _lhsOnExplicitRules :: Int
         _lhsOnExplicitRules = rule231  ()
         _lhsOnonts :: [(NontermIdent,[ConstructorIdent])]
         _lhsOnonts = rule232  ()
         _lhsOntattrs :: Seq (Vertex,NTAttr)
         _lhsOntattrs = rule233  ()
         _lhsOrules :: Seq (Vertex,CRule)
         _lhsOrules = rule234  ()
         _lhsOsynMap' :: Map Identifier Attributes
         _lhsOsynMap' = rule235  ()
         _lhsOacount :: Int
         _lhsOacount = rule236 _lhsIacount
         _lhsOvcount :: Int
         _lhsOvcount = rule237 _lhsIvcount
         __result_ = T_Nonterminals_vOut16 _lhsOacount _lhsOadditionalDep _lhsOaranges _lhsOaroundDep _lhsOcNonterminals _lhsOdirectDep _lhsOerrors _lhsOinhMap' _lhsOinstDep _lhsOmergeDep _lhsOmirages _lhsOnAutoRules _lhsOnExplicitRules _lhsOnonts _lhsOntattrs _lhsOrules _lhsOsynMap' _lhsOvcount
         in __result_ )
     in C_Nonterminals_s17 v16
   {-# INLINE rule220 #-}
   {-# LINE 627 "src-ag/Order.ag" #-}
   rule220 = \  (_ :: ()) ->
                                 {-# LINE 627 "src-ag/Order.ag" #-}
                                 []
                                 {-# LINE 1836 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule221 #-}
   {-# LINE 26 "src-ag/TfmToMirage.ag" #-}
   rule221 = \  (_ :: ()) ->
                         {-# LINE 26 "src-ag/TfmToMirage.ag" #-}
                         []
                         {-# LINE 1842 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule222 #-}
   rule222 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule223 #-}
   rule223 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule224 #-}
   rule224 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule225 #-}
   rule225 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule226 #-}
   rule226 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule227 #-}
   rule227 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule228 #-}
   rule228 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule229 #-}
   rule229 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule230 #-}
   rule230 = \  (_ :: ()) ->
     0
   {-# INLINE rule231 #-}
   rule231 = \  (_ :: ()) ->
     0
   {-# INLINE rule232 #-}
   rule232 = \  (_ :: ()) ->
     []
   {-# INLINE rule233 #-}
   rule233 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule234 #-}
   rule234 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule235 #-}
   rule235 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule236 #-}
   rule236 = \ ((_lhsIacount) :: Int) ->
     _lhsIacount
   {-# INLINE rule237 #-}
   rule237 = \ ((_lhsIvcount) :: Int) ->
     _lhsIvcount

-- Pattern -----------------------------------------------------
-- wrapper
data Inh_Pattern  = Inh_Pattern { allTypeSigs_Inh_Pattern :: (Map Identifier Type), altAttrs_Inh_Pattern :: (Map AltAttr Vertex), belowIrrefutable_Inh_Pattern :: (Bool), con_Inh_Pattern :: (Identifier), inh_Inh_Pattern :: (Attributes), nt_Inh_Pattern :: (Identifier), syn_Inh_Pattern :: (Attributes) }
data Syn_Pattern  = Syn_Pattern { copy_Syn_Pattern :: (Pattern), errors_Syn_Pattern :: (Seq Error), gathAltAttrs_Syn_Pattern :: ([AltAttr]), instVars_Syn_Pattern :: ([Identifier]), isUnderscore_Syn_Pattern :: (Bool), locVars_Syn_Pattern :: ([Identifier]), patternAttrs_Syn_Pattern :: ([(Identifier,Identifier,Bool)]), pp_Syn_Pattern :: (PP_Doc) }
{-# INLINABLE wrap_Pattern #-}
wrap_Pattern :: T_Pattern  -> Inh_Pattern  -> (Syn_Pattern )
wrap_Pattern (T_Pattern act) (Inh_Pattern _lhsIallTypeSigs _lhsIaltAttrs _lhsIbelowIrrefutable _lhsIcon _lhsIinh _lhsInt _lhsIsyn) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg19 = T_Pattern_vIn19 _lhsIallTypeSigs _lhsIaltAttrs _lhsIbelowIrrefutable _lhsIcon _lhsIinh _lhsInt _lhsIsyn
        (T_Pattern_vOut19 _lhsOcopy _lhsOerrors _lhsOgathAltAttrs _lhsOinstVars _lhsOisUnderscore _lhsOlocVars _lhsOpatternAttrs _lhsOpp) <- return (inv_Pattern_s20 sem arg19)
        return (Syn_Pattern _lhsOcopy _lhsOerrors _lhsOgathAltAttrs _lhsOinstVars _lhsOisUnderscore _lhsOlocVars _lhsOpatternAttrs _lhsOpp)
   )

-- cata
{-# NOINLINE sem_Pattern #-}
sem_Pattern :: Pattern  -> T_Pattern 
sem_Pattern ( Constr name_ pats_ ) = sem_Pattern_Constr name_ ( sem_Patterns pats_ )
sem_Pattern ( Product pos_ pats_ ) = sem_Pattern_Product pos_ ( sem_Patterns pats_ )
sem_Pattern ( Alias field_ attr_ pat_ ) = sem_Pattern_Alias field_ attr_ ( sem_Pattern pat_ )
sem_Pattern ( Irrefutable pat_ ) = sem_Pattern_Irrefutable ( sem_Pattern pat_ )
sem_Pattern ( Underscore pos_ ) = sem_Pattern_Underscore pos_

-- semantic domain
newtype T_Pattern  = T_Pattern {
                               attach_T_Pattern :: Identity (T_Pattern_s20 )
                               }
newtype T_Pattern_s20  = C_Pattern_s20 {
                                       inv_Pattern_s20 :: (T_Pattern_v19 )
                                       }
data T_Pattern_s21  = C_Pattern_s21
type T_Pattern_v19  = (T_Pattern_vIn19 ) -> (T_Pattern_vOut19 )
data T_Pattern_vIn19  = T_Pattern_vIn19 (Map Identifier Type) (Map AltAttr Vertex) (Bool) (Identifier) (Attributes) (Identifier) (Attributes)
data T_Pattern_vOut19  = T_Pattern_vOut19 (Pattern) (Seq Error) ([AltAttr]) ([Identifier]) (Bool) ([Identifier]) ([(Identifier,Identifier,Bool)]) (PP_Doc)
{-# NOINLINE sem_Pattern_Constr #-}
sem_Pattern_Constr :: (ConstructorIdent) -> T_Patterns  -> T_Pattern 
sem_Pattern_Constr arg_name_ arg_pats_ = T_Pattern (return st20) where
   {-# NOINLINE st20 #-}
   st20 = let
      v19 :: T_Pattern_v19 
      v19 = \ (T_Pattern_vIn19 _lhsIallTypeSigs _lhsIaltAttrs _lhsIbelowIrrefutable _lhsIcon _lhsIinh _lhsInt _lhsIsyn) -> ( let
         _patsX23 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_pats_))
         (T_Patterns_vOut22 _patsIcopy _patsIerrors _patsIgathAltAttrs _patsIinstVars _patsIlocVars _patsIpatternAttrs _patsIpps) = inv_Patterns_s23 _patsX23 (T_Patterns_vIn22 _patsOallTypeSigs _patsOaltAttrs _patsObelowIrrefutable _patsOcon _patsOinh _patsOnt _patsOsyn)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule238 _patsIpps arg_name_
         _lhsOisUnderscore :: Bool
         _lhsOisUnderscore = rule239  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule240 _patsIerrors
         _lhsOgathAltAttrs :: [AltAttr]
         _lhsOgathAltAttrs = rule241 _patsIgathAltAttrs
         _lhsOinstVars :: [Identifier]
         _lhsOinstVars = rule242 _patsIinstVars
         _lhsOlocVars :: [Identifier]
         _lhsOlocVars = rule243 _patsIlocVars
         _lhsOpatternAttrs :: [(Identifier,Identifier,Bool)]
         _lhsOpatternAttrs = rule244 _patsIpatternAttrs
         _copy = rule245 _patsIcopy arg_name_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule246 _copy
         _patsOallTypeSigs = rule247 _lhsIallTypeSigs
         _patsOaltAttrs = rule248 _lhsIaltAttrs
         _patsObelowIrrefutable = rule249 _lhsIbelowIrrefutable
         _patsOcon = rule250 _lhsIcon
         _patsOinh = rule251 _lhsIinh
         _patsOnt = rule252 _lhsInt
         _patsOsyn = rule253 _lhsIsyn
         __result_ = T_Pattern_vOut19 _lhsOcopy _lhsOerrors _lhsOgathAltAttrs _lhsOinstVars _lhsOisUnderscore _lhsOlocVars _lhsOpatternAttrs _lhsOpp
         in __result_ )
     in C_Pattern_s20 v19
   {-# INLINE rule238 #-}
   {-# LINE 80 "src-ag/TfmToMirage.ag" #-}
   rule238 = \ ((_patsIpps) :: [PP_Doc]) name_ ->
                           {-# LINE 80 "src-ag/TfmToMirage.ag" #-}
                           pp_parens $ name_ >#< hv_sp _patsIpps
                           {-# LINE 1967 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule239 #-}
   {-# LINE 91 "src-ag/TfmToMirage.ag" #-}
   rule239 = \  (_ :: ()) ->
                                    {-# LINE 91 "src-ag/TfmToMirage.ag" #-}
                                    False
                                    {-# LINE 1973 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule240 #-}
   rule240 = \ ((_patsIerrors) :: Seq Error) ->
     _patsIerrors
   {-# INLINE rule241 #-}
   rule241 = \ ((_patsIgathAltAttrs) :: [AltAttr]) ->
     _patsIgathAltAttrs
   {-# INLINE rule242 #-}
   rule242 = \ ((_patsIinstVars) :: [Identifier]) ->
     _patsIinstVars
   {-# INLINE rule243 #-}
   rule243 = \ ((_patsIlocVars) :: [Identifier]) ->
     _patsIlocVars
   {-# INLINE rule244 #-}
   rule244 = \ ((_patsIpatternAttrs) :: [(Identifier,Identifier,Bool)]) ->
     _patsIpatternAttrs
   {-# INLINE rule245 #-}
   rule245 = \ ((_patsIcopy) :: Patterns) name_ ->
     Constr name_ _patsIcopy
   {-# INLINE rule246 #-}
   rule246 = \ _copy ->
     _copy
   {-# INLINE rule247 #-}
   rule247 = \ ((_lhsIallTypeSigs) :: Map Identifier Type) ->
     _lhsIallTypeSigs
   {-# INLINE rule248 #-}
   rule248 = \ ((_lhsIaltAttrs) :: Map AltAttr Vertex) ->
     _lhsIaltAttrs
   {-# INLINE rule249 #-}
   rule249 = \ ((_lhsIbelowIrrefutable) :: Bool) ->
     _lhsIbelowIrrefutable
   {-# INLINE rule250 #-}
   rule250 = \ ((_lhsIcon) :: Identifier) ->
     _lhsIcon
   {-# INLINE rule251 #-}
   rule251 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule252 #-}
   rule252 = \ ((_lhsInt) :: Identifier) ->
     _lhsInt
   {-# INLINE rule253 #-}
   rule253 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
{-# NOINLINE sem_Pattern_Product #-}
sem_Pattern_Product :: (Pos) -> T_Patterns  -> T_Pattern 
sem_Pattern_Product arg_pos_ arg_pats_ = T_Pattern (return st20) where
   {-# NOINLINE st20 #-}
   st20 = let
      v19 :: T_Pattern_v19 
      v19 = \ (T_Pattern_vIn19 _lhsIallTypeSigs _lhsIaltAttrs _lhsIbelowIrrefutable _lhsIcon _lhsIinh _lhsInt _lhsIsyn) -> ( let
         _patsX23 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_pats_))
         (T_Patterns_vOut22 _patsIcopy _patsIerrors _patsIgathAltAttrs _patsIinstVars _patsIlocVars _patsIpatternAttrs _patsIpps) = inv_Patterns_s23 _patsX23 (T_Patterns_vIn22 _patsOallTypeSigs _patsOaltAttrs _patsObelowIrrefutable _patsOcon _patsOinh _patsOnt _patsOsyn)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule254 _patsIpps
         _lhsOisUnderscore :: Bool
         _lhsOisUnderscore = rule255  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule256 _patsIerrors
         _lhsOgathAltAttrs :: [AltAttr]
         _lhsOgathAltAttrs = rule257 _patsIgathAltAttrs
         _lhsOinstVars :: [Identifier]
         _lhsOinstVars = rule258 _patsIinstVars
         _lhsOlocVars :: [Identifier]
         _lhsOlocVars = rule259 _patsIlocVars
         _lhsOpatternAttrs :: [(Identifier,Identifier,Bool)]
         _lhsOpatternAttrs = rule260 _patsIpatternAttrs
         _copy = rule261 _patsIcopy arg_pos_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule262 _copy
         _patsOallTypeSigs = rule263 _lhsIallTypeSigs
         _patsOaltAttrs = rule264 _lhsIaltAttrs
         _patsObelowIrrefutable = rule265 _lhsIbelowIrrefutable
         _patsOcon = rule266 _lhsIcon
         _patsOinh = rule267 _lhsIinh
         _patsOnt = rule268 _lhsInt
         _patsOsyn = rule269 _lhsIsyn
         __result_ = T_Pattern_vOut19 _lhsOcopy _lhsOerrors _lhsOgathAltAttrs _lhsOinstVars _lhsOisUnderscore _lhsOlocVars _lhsOpatternAttrs _lhsOpp
         in __result_ )
     in C_Pattern_s20 v19
   {-# INLINE rule254 #-}
   {-# LINE 81 "src-ag/TfmToMirage.ag" #-}
   rule254 = \ ((_patsIpps) :: [PP_Doc]) ->
                           {-# LINE 81 "src-ag/TfmToMirage.ag" #-}
                           pp_block "(" ")" "," _patsIpps
                           {-# LINE 2057 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule255 #-}
   {-# LINE 92 "src-ag/TfmToMirage.ag" #-}
   rule255 = \  (_ :: ()) ->
                                    {-# LINE 92 "src-ag/TfmToMirage.ag" #-}
                                    False
                                    {-# LINE 2063 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule256 #-}
   rule256 = \ ((_patsIerrors) :: Seq Error) ->
     _patsIerrors
   {-# INLINE rule257 #-}
   rule257 = \ ((_patsIgathAltAttrs) :: [AltAttr]) ->
     _patsIgathAltAttrs
   {-# INLINE rule258 #-}
   rule258 = \ ((_patsIinstVars) :: [Identifier]) ->
     _patsIinstVars
   {-# INLINE rule259 #-}
   rule259 = \ ((_patsIlocVars) :: [Identifier]) ->
     _patsIlocVars
   {-# INLINE rule260 #-}
   rule260 = \ ((_patsIpatternAttrs) :: [(Identifier,Identifier,Bool)]) ->
     _patsIpatternAttrs
   {-# INLINE rule261 #-}
   rule261 = \ ((_patsIcopy) :: Patterns) pos_ ->
     Product pos_ _patsIcopy
   {-# INLINE rule262 #-}
   rule262 = \ _copy ->
     _copy
   {-# INLINE rule263 #-}
   rule263 = \ ((_lhsIallTypeSigs) :: Map Identifier Type) ->
     _lhsIallTypeSigs
   {-# INLINE rule264 #-}
   rule264 = \ ((_lhsIaltAttrs) :: Map AltAttr Vertex) ->
     _lhsIaltAttrs
   {-# INLINE rule265 #-}
   rule265 = \ ((_lhsIbelowIrrefutable) :: Bool) ->
     _lhsIbelowIrrefutable
   {-# INLINE rule266 #-}
   rule266 = \ ((_lhsIcon) :: Identifier) ->
     _lhsIcon
   {-# INLINE rule267 #-}
   rule267 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule268 #-}
   rule268 = \ ((_lhsInt) :: Identifier) ->
     _lhsInt
   {-# INLINE rule269 #-}
   rule269 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
{-# NOINLINE sem_Pattern_Alias #-}
sem_Pattern_Alias :: (Identifier) -> (Identifier) -> T_Pattern  -> T_Pattern 
sem_Pattern_Alias arg_field_ arg_attr_ arg_pat_ = T_Pattern (return st20) where
   {-# NOINLINE st20 #-}
   st20 = let
      v19 :: T_Pattern_v19 
      v19 = \ (T_Pattern_vIn19 _lhsIallTypeSigs _lhsIaltAttrs _lhsIbelowIrrefutable _lhsIcon _lhsIinh _lhsInt _lhsIsyn) -> ( let
         _patX20 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pat_))
         (T_Pattern_vOut19 _patIcopy _patIerrors _patIgathAltAttrs _patIinstVars _patIisUnderscore _patIlocVars _patIpatternAttrs _patIpp) = inv_Pattern_s20 _patX20 (T_Pattern_vIn19 _patOallTypeSigs _patOaltAttrs _patObelowIrrefutable _patOcon _patOinh _patOnt _patOsyn)
         _lhsOgathAltAttrs :: [AltAttr]
         _lhsOgathAltAttrs = rule270 arg_attr_ arg_field_
         _lhsOpatternAttrs :: [(Identifier,Identifier,Bool)]
         _lhsOpatternAttrs = rule271 arg_attr_ arg_field_
         _lhsOlocVars :: [Identifier]
         _lhsOlocVars = rule272 arg_attr_ arg_field_
         _lhsOinstVars :: [Identifier]
         _lhsOinstVars = rule273 arg_attr_ arg_field_
         _ppVar = rule274 arg_attr_ arg_field_
         _ppVarBang = rule275 _ppVar
         _lhsOpp :: PP_Doc
         _lhsOpp = rule276 _patIisUnderscore _patIpp _ppVarBang
         _lhsOisUnderscore :: Bool
         _lhsOisUnderscore = rule277  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule278 _patIerrors
         _copy = rule279 _patIcopy arg_attr_ arg_field_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule280 _copy
         _patOallTypeSigs = rule281 _lhsIallTypeSigs
         _patOaltAttrs = rule282 _lhsIaltAttrs
         _patObelowIrrefutable = rule283 _lhsIbelowIrrefutable
         _patOcon = rule284 _lhsIcon
         _patOinh = rule285 _lhsIinh
         _patOnt = rule286 _lhsInt
         _patOsyn = rule287 _lhsIsyn
         __result_ = T_Pattern_vOut19 _lhsOcopy _lhsOerrors _lhsOgathAltAttrs _lhsOinstVars _lhsOisUnderscore _lhsOlocVars _lhsOpatternAttrs _lhsOpp
         in __result_ )
     in C_Pattern_s20 v19
   {-# INLINE rule270 #-}
   {-# LINE 187 "src-ag/Order.ag" #-}
   rule270 = \ attr_ field_ ->
                                {-# LINE 187 "src-ag/Order.ag" #-}
                                [AltAttr field_ attr_ (field_ == _LOC || field_ == _INST)]
                                {-# LINE 2149 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule271 #-}
   {-# LINE 253 "src-ag/Order.ag" #-}
   rule271 = \ attr_ field_ ->
                                {-# LINE 253 "src-ag/Order.ag" #-}
                                [(field_,attr_,(field_ == _LOC || field_ == _INST))]
                                {-# LINE 2155 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule272 #-}
   {-# LINE 685 "src-ag/Order.ag" #-}
   rule272 = \ attr_ field_ ->
                               {-# LINE 685 "src-ag/Order.ag" #-}
                               if field_ == _LOC
                                  then [attr_]
                                  else []
                               {-# LINE 2163 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule273 #-}
   {-# LINE 688 "src-ag/Order.ag" #-}
   rule273 = \ attr_ field_ ->
                               {-# LINE 688 "src-ag/Order.ag" #-}
                               if field_ == _INST
                                  then [attr_]
                                  else []
                               {-# LINE 2171 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule274 #-}
   {-# LINE 82 "src-ag/TfmToMirage.ag" #-}
   rule274 = \ attr_ field_ ->
                           {-# LINE 82 "src-ag/TfmToMirage.ag" #-}
                           pp field_ >|< "." >|< pp attr_
                           {-# LINE 2177 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule275 #-}
   {-# LINE 83 "src-ag/TfmToMirage.ag" #-}
   rule275 = \ _ppVar ->
                              {-# LINE 83 "src-ag/TfmToMirage.ag" #-}
                              _ppVar
                              {-# LINE 2183 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule276 #-}
   {-# LINE 84 "src-ag/TfmToMirage.ag" #-}
   rule276 = \ ((_patIisUnderscore) :: Bool) ((_patIpp) :: PP_Doc) _ppVarBang ->
                           {-# LINE 84 "src-ag/TfmToMirage.ag" #-}
                           if _patIisUnderscore
                            then _ppVarBang
                            else _ppVarBang     >|< "@" >|< _patIpp
                           {-# LINE 2191 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule277 #-}
   {-# LINE 93 "src-ag/TfmToMirage.ag" #-}
   rule277 = \  (_ :: ()) ->
                                    {-# LINE 93 "src-ag/TfmToMirage.ag" #-}
                                    False
                                    {-# LINE 2197 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule278 #-}
   rule278 = \ ((_patIerrors) :: Seq Error) ->
     _patIerrors
   {-# INLINE rule279 #-}
   rule279 = \ ((_patIcopy) :: Pattern) attr_ field_ ->
     Alias field_ attr_ _patIcopy
   {-# INLINE rule280 #-}
   rule280 = \ _copy ->
     _copy
   {-# INLINE rule281 #-}
   rule281 = \ ((_lhsIallTypeSigs) :: Map Identifier Type) ->
     _lhsIallTypeSigs
   {-# INLINE rule282 #-}
   rule282 = \ ((_lhsIaltAttrs) :: Map AltAttr Vertex) ->
     _lhsIaltAttrs
   {-# INLINE rule283 #-}
   rule283 = \ ((_lhsIbelowIrrefutable) :: Bool) ->
     _lhsIbelowIrrefutable
   {-# INLINE rule284 #-}
   rule284 = \ ((_lhsIcon) :: Identifier) ->
     _lhsIcon
   {-# INLINE rule285 #-}
   rule285 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule286 #-}
   rule286 = \ ((_lhsInt) :: Identifier) ->
     _lhsInt
   {-# INLINE rule287 #-}
   rule287 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
{-# NOINLINE sem_Pattern_Irrefutable #-}
sem_Pattern_Irrefutable :: T_Pattern  -> T_Pattern 
sem_Pattern_Irrefutable arg_pat_ = T_Pattern (return st20) where
   {-# NOINLINE st20 #-}
   st20 = let
      v19 :: T_Pattern_v19 
      v19 = \ (T_Pattern_vIn19 _lhsIallTypeSigs _lhsIaltAttrs _lhsIbelowIrrefutable _lhsIcon _lhsIinh _lhsInt _lhsIsyn) -> ( let
         _patX20 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pat_))
         (T_Pattern_vOut19 _patIcopy _patIerrors _patIgathAltAttrs _patIinstVars _patIisUnderscore _patIlocVars _patIpatternAttrs _patIpp) = inv_Pattern_s20 _patX20 (T_Pattern_vIn19 _patOallTypeSigs _patOaltAttrs _patObelowIrrefutable _patOcon _patOinh _patOnt _patOsyn)
         _lhsOpp :: PP_Doc
         _lhsOpp = rule288 _patIpp
         _patObelowIrrefutable = rule289  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule290 _patIerrors
         _lhsOgathAltAttrs :: [AltAttr]
         _lhsOgathAltAttrs = rule291 _patIgathAltAttrs
         _lhsOinstVars :: [Identifier]
         _lhsOinstVars = rule292 _patIinstVars
         _lhsOlocVars :: [Identifier]
         _lhsOlocVars = rule293 _patIlocVars
         _lhsOpatternAttrs :: [(Identifier,Identifier,Bool)]
         _lhsOpatternAttrs = rule294 _patIpatternAttrs
         _copy = rule295 _patIcopy
         _lhsOcopy :: Pattern
         _lhsOcopy = rule296 _copy
         _lhsOisUnderscore :: Bool
         _lhsOisUnderscore = rule297 _patIisUnderscore
         _patOallTypeSigs = rule298 _lhsIallTypeSigs
         _patOaltAttrs = rule299 _lhsIaltAttrs
         _patOcon = rule300 _lhsIcon
         _patOinh = rule301 _lhsIinh
         _patOnt = rule302 _lhsInt
         _patOsyn = rule303 _lhsIsyn
         __result_ = T_Pattern_vOut19 _lhsOcopy _lhsOerrors _lhsOgathAltAttrs _lhsOinstVars _lhsOisUnderscore _lhsOlocVars _lhsOpatternAttrs _lhsOpp
         in __result_ )
     in C_Pattern_s20 v19
   {-# INLINE rule288 #-}
   {-# LINE 87 "src-ag/TfmToMirage.ag" #-}
   rule288 = \ ((_patIpp) :: PP_Doc) ->
                           {-# LINE 87 "src-ag/TfmToMirage.ag" #-}
                           text "~" >|< pp_parens _patIpp
                           {-# LINE 2269 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule289 #-}
   {-# LINE 100 "src-ag/TfmToMirage.ag" #-}
   rule289 = \  (_ :: ()) ->
                                         {-# LINE 100 "src-ag/TfmToMirage.ag" #-}
                                         True
                                         {-# LINE 2275 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule290 #-}
   rule290 = \ ((_patIerrors) :: Seq Error) ->
     _patIerrors
   {-# INLINE rule291 #-}
   rule291 = \ ((_patIgathAltAttrs) :: [AltAttr]) ->
     _patIgathAltAttrs
   {-# INLINE rule292 #-}
   rule292 = \ ((_patIinstVars) :: [Identifier]) ->
     _patIinstVars
   {-# INLINE rule293 #-}
   rule293 = \ ((_patIlocVars) :: [Identifier]) ->
     _patIlocVars
   {-# INLINE rule294 #-}
   rule294 = \ ((_patIpatternAttrs) :: [(Identifier,Identifier,Bool)]) ->
     _patIpatternAttrs
   {-# INLINE rule295 #-}
   rule295 = \ ((_patIcopy) :: Pattern) ->
     Irrefutable _patIcopy
   {-# INLINE rule296 #-}
   rule296 = \ _copy ->
     _copy
   {-# INLINE rule297 #-}
   rule297 = \ ((_patIisUnderscore) :: Bool) ->
     _patIisUnderscore
   {-# INLINE rule298 #-}
   rule298 = \ ((_lhsIallTypeSigs) :: Map Identifier Type) ->
     _lhsIallTypeSigs
   {-# INLINE rule299 #-}
   rule299 = \ ((_lhsIaltAttrs) :: Map AltAttr Vertex) ->
     _lhsIaltAttrs
   {-# INLINE rule300 #-}
   rule300 = \ ((_lhsIcon) :: Identifier) ->
     _lhsIcon
   {-# INLINE rule301 #-}
   rule301 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule302 #-}
   rule302 = \ ((_lhsInt) :: Identifier) ->
     _lhsInt
   {-# INLINE rule303 #-}
   rule303 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
{-# NOINLINE sem_Pattern_Underscore #-}
sem_Pattern_Underscore :: (Pos) -> T_Pattern 
sem_Pattern_Underscore arg_pos_ = T_Pattern (return st20) where
   {-# NOINLINE st20 #-}
   st20 = let
      v19 :: T_Pattern_v19 
      v19 = \ (T_Pattern_vIn19 _lhsIallTypeSigs _lhsIaltAttrs _lhsIbelowIrrefutable _lhsIcon _lhsIinh _lhsInt _lhsIsyn) -> ( let
         _lhsOpp :: PP_Doc
         _lhsOpp = rule304  ()
         _lhsOisUnderscore :: Bool
         _lhsOisUnderscore = rule305  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule306  ()
         _lhsOgathAltAttrs :: [AltAttr]
         _lhsOgathAltAttrs = rule307  ()
         _lhsOinstVars :: [Identifier]
         _lhsOinstVars = rule308  ()
         _lhsOlocVars :: [Identifier]
         _lhsOlocVars = rule309  ()
         _lhsOpatternAttrs :: [(Identifier,Identifier,Bool)]
         _lhsOpatternAttrs = rule310  ()
         _copy = rule311 arg_pos_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule312 _copy
         __result_ = T_Pattern_vOut19 _lhsOcopy _lhsOerrors _lhsOgathAltAttrs _lhsOinstVars _lhsOisUnderscore _lhsOlocVars _lhsOpatternAttrs _lhsOpp
         in __result_ )
     in C_Pattern_s20 v19
   {-# INLINE rule304 #-}
   {-# LINE 88 "src-ag/TfmToMirage.ag" #-}
   rule304 = \  (_ :: ()) ->
                           {-# LINE 88 "src-ag/TfmToMirage.ag" #-}
                           text "_"
                           {-# LINE 2350 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule305 #-}
   {-# LINE 94 "src-ag/TfmToMirage.ag" #-}
   rule305 = \  (_ :: ()) ->
                                    {-# LINE 94 "src-ag/TfmToMirage.ag" #-}
                                    True
                                    {-# LINE 2356 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule306 #-}
   rule306 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule307 #-}
   rule307 = \  (_ :: ()) ->
     []
   {-# INLINE rule308 #-}
   rule308 = \  (_ :: ()) ->
     []
   {-# INLINE rule309 #-}
   rule309 = \  (_ :: ()) ->
     []
   {-# INLINE rule310 #-}
   rule310 = \  (_ :: ()) ->
     []
   {-# INLINE rule311 #-}
   rule311 = \ pos_ ->
     Underscore pos_
   {-# INLINE rule312 #-}
   rule312 = \ _copy ->
     _copy

-- Patterns ----------------------------------------------------
-- wrapper
data Inh_Patterns  = Inh_Patterns { allTypeSigs_Inh_Patterns :: (Map Identifier Type), altAttrs_Inh_Patterns :: (Map AltAttr Vertex), belowIrrefutable_Inh_Patterns :: (Bool), con_Inh_Patterns :: (Identifier), inh_Inh_Patterns :: (Attributes), nt_Inh_Patterns :: (Identifier), syn_Inh_Patterns :: (Attributes) }
data Syn_Patterns  = Syn_Patterns { copy_Syn_Patterns :: (Patterns), errors_Syn_Patterns :: (Seq Error), gathAltAttrs_Syn_Patterns :: ([AltAttr]), instVars_Syn_Patterns :: ([Identifier]), locVars_Syn_Patterns :: ([Identifier]), patternAttrs_Syn_Patterns :: ([(Identifier,Identifier,Bool)]), pps_Syn_Patterns :: ([PP_Doc]) }
{-# INLINABLE wrap_Patterns #-}
wrap_Patterns :: T_Patterns  -> Inh_Patterns  -> (Syn_Patterns )
wrap_Patterns (T_Patterns act) (Inh_Patterns _lhsIallTypeSigs _lhsIaltAttrs _lhsIbelowIrrefutable _lhsIcon _lhsIinh _lhsInt _lhsIsyn) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg22 = T_Patterns_vIn22 _lhsIallTypeSigs _lhsIaltAttrs _lhsIbelowIrrefutable _lhsIcon _lhsIinh _lhsInt _lhsIsyn
        (T_Patterns_vOut22 _lhsOcopy _lhsOerrors _lhsOgathAltAttrs _lhsOinstVars _lhsOlocVars _lhsOpatternAttrs _lhsOpps) <- return (inv_Patterns_s23 sem arg22)
        return (Syn_Patterns _lhsOcopy _lhsOerrors _lhsOgathAltAttrs _lhsOinstVars _lhsOlocVars _lhsOpatternAttrs _lhsOpps)
   )

-- cata
{-# NOINLINE sem_Patterns #-}
sem_Patterns :: Patterns  -> T_Patterns 
sem_Patterns list = Prelude.foldr sem_Patterns_Cons sem_Patterns_Nil (Prelude.map sem_Pattern list)

-- semantic domain
newtype T_Patterns  = T_Patterns {
                                 attach_T_Patterns :: Identity (T_Patterns_s23 )
                                 }
newtype T_Patterns_s23  = C_Patterns_s23 {
                                         inv_Patterns_s23 :: (T_Patterns_v22 )
                                         }
data T_Patterns_s24  = C_Patterns_s24
type T_Patterns_v22  = (T_Patterns_vIn22 ) -> (T_Patterns_vOut22 )
data T_Patterns_vIn22  = T_Patterns_vIn22 (Map Identifier Type) (Map AltAttr Vertex) (Bool) (Identifier) (Attributes) (Identifier) (Attributes)
data T_Patterns_vOut22  = T_Patterns_vOut22 (Patterns) (Seq Error) ([AltAttr]) ([Identifier]) ([Identifier]) ([(Identifier,Identifier,Bool)]) ([PP_Doc])
{-# NOINLINE sem_Patterns_Cons #-}
sem_Patterns_Cons :: T_Pattern  -> T_Patterns  -> T_Patterns 
sem_Patterns_Cons arg_hd_ arg_tl_ = T_Patterns (return st23) where
   {-# NOINLINE st23 #-}
   st23 = let
      v22 :: T_Patterns_v22 
      v22 = \ (T_Patterns_vIn22 _lhsIallTypeSigs _lhsIaltAttrs _lhsIbelowIrrefutable _lhsIcon _lhsIinh _lhsInt _lhsIsyn) -> ( let
         _hdX20 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_hd_))
         _tlX23 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_tl_))
         (T_Pattern_vOut19 _hdIcopy _hdIerrors _hdIgathAltAttrs _hdIinstVars _hdIisUnderscore _hdIlocVars _hdIpatternAttrs _hdIpp) = inv_Pattern_s20 _hdX20 (T_Pattern_vIn19 _hdOallTypeSigs _hdOaltAttrs _hdObelowIrrefutable _hdOcon _hdOinh _hdOnt _hdOsyn)
         (T_Patterns_vOut22 _tlIcopy _tlIerrors _tlIgathAltAttrs _tlIinstVars _tlIlocVars _tlIpatternAttrs _tlIpps) = inv_Patterns_s23 _tlX23 (T_Patterns_vIn22 _tlOallTypeSigs _tlOaltAttrs _tlObelowIrrefutable _tlOcon _tlOinh _tlOnt _tlOsyn)
         _lhsOpps :: [PP_Doc]
         _lhsOpps = rule313 _hdIpp _tlIpps
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule314 _hdIerrors _tlIerrors
         _lhsOgathAltAttrs :: [AltAttr]
         _lhsOgathAltAttrs = rule315 _hdIgathAltAttrs _tlIgathAltAttrs
         _lhsOinstVars :: [Identifier]
         _lhsOinstVars = rule316 _hdIinstVars _tlIinstVars
         _lhsOlocVars :: [Identifier]
         _lhsOlocVars = rule317 _hdIlocVars _tlIlocVars
         _lhsOpatternAttrs :: [(Identifier,Identifier,Bool)]
         _lhsOpatternAttrs = rule318 _hdIpatternAttrs _tlIpatternAttrs
         _copy = rule319 _hdIcopy _tlIcopy
         _lhsOcopy :: Patterns
         _lhsOcopy = rule320 _copy
         _hdOallTypeSigs = rule321 _lhsIallTypeSigs
         _hdOaltAttrs = rule322 _lhsIaltAttrs
         _hdObelowIrrefutable = rule323 _lhsIbelowIrrefutable
         _hdOcon = rule324 _lhsIcon
         _hdOinh = rule325 _lhsIinh
         _hdOnt = rule326 _lhsInt
         _hdOsyn = rule327 _lhsIsyn
         _tlOallTypeSigs = rule328 _lhsIallTypeSigs
         _tlOaltAttrs = rule329 _lhsIaltAttrs
         _tlObelowIrrefutable = rule330 _lhsIbelowIrrefutable
         _tlOcon = rule331 _lhsIcon
         _tlOinh = rule332 _lhsIinh
         _tlOnt = rule333 _lhsInt
         _tlOsyn = rule334 _lhsIsyn
         __result_ = T_Patterns_vOut22 _lhsOcopy _lhsOerrors _lhsOgathAltAttrs _lhsOinstVars _lhsOlocVars _lhsOpatternAttrs _lhsOpps
         in __result_ )
     in C_Patterns_s23 v22
   {-# INLINE rule313 #-}
   {-# LINE 76 "src-ag/TfmToMirage.ag" #-}
   rule313 = \ ((_hdIpp) :: PP_Doc) ((_tlIpps) :: [PP_Doc]) ->
                     {-# LINE 76 "src-ag/TfmToMirage.ag" #-}
                     _hdIpp : _tlIpps
                     {-# LINE 2457 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule314 #-}
   rule314 = \ ((_hdIerrors) :: Seq Error) ((_tlIerrors) :: Seq Error) ->
     _hdIerrors Seq.>< _tlIerrors
   {-# INLINE rule315 #-}
   rule315 = \ ((_hdIgathAltAttrs) :: [AltAttr]) ((_tlIgathAltAttrs) :: [AltAttr]) ->
     _hdIgathAltAttrs ++ _tlIgathAltAttrs
   {-# INLINE rule316 #-}
   rule316 = \ ((_hdIinstVars) :: [Identifier]) ((_tlIinstVars) :: [Identifier]) ->
     _hdIinstVars ++ _tlIinstVars
   {-# INLINE rule317 #-}
   rule317 = \ ((_hdIlocVars) :: [Identifier]) ((_tlIlocVars) :: [Identifier]) ->
     _hdIlocVars ++ _tlIlocVars
   {-# INLINE rule318 #-}
   rule318 = \ ((_hdIpatternAttrs) :: [(Identifier,Identifier,Bool)]) ((_tlIpatternAttrs) :: [(Identifier,Identifier,Bool)]) ->
     _hdIpatternAttrs ++ _tlIpatternAttrs
   {-# INLINE rule319 #-}
   rule319 = \ ((_hdIcopy) :: Pattern) ((_tlIcopy) :: Patterns) ->
     (:) _hdIcopy _tlIcopy
   {-# INLINE rule320 #-}
   rule320 = \ _copy ->
     _copy
   {-# INLINE rule321 #-}
   rule321 = \ ((_lhsIallTypeSigs) :: Map Identifier Type) ->
     _lhsIallTypeSigs
   {-# INLINE rule322 #-}
   rule322 = \ ((_lhsIaltAttrs) :: Map AltAttr Vertex) ->
     _lhsIaltAttrs
   {-# INLINE rule323 #-}
   rule323 = \ ((_lhsIbelowIrrefutable) :: Bool) ->
     _lhsIbelowIrrefutable
   {-# INLINE rule324 #-}
   rule324 = \ ((_lhsIcon) :: Identifier) ->
     _lhsIcon
   {-# INLINE rule325 #-}
   rule325 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule326 #-}
   rule326 = \ ((_lhsInt) :: Identifier) ->
     _lhsInt
   {-# INLINE rule327 #-}
   rule327 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
   {-# INLINE rule328 #-}
   rule328 = \ ((_lhsIallTypeSigs) :: Map Identifier Type) ->
     _lhsIallTypeSigs
   {-# INLINE rule329 #-}
   rule329 = \ ((_lhsIaltAttrs) :: Map AltAttr Vertex) ->
     _lhsIaltAttrs
   {-# INLINE rule330 #-}
   rule330 = \ ((_lhsIbelowIrrefutable) :: Bool) ->
     _lhsIbelowIrrefutable
   {-# INLINE rule331 #-}
   rule331 = \ ((_lhsIcon) :: Identifier) ->
     _lhsIcon
   {-# INLINE rule332 #-}
   rule332 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule333 #-}
   rule333 = \ ((_lhsInt) :: Identifier) ->
     _lhsInt
   {-# INLINE rule334 #-}
   rule334 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
{-# NOINLINE sem_Patterns_Nil #-}
sem_Patterns_Nil ::  T_Patterns 
sem_Patterns_Nil  = T_Patterns (return st23) where
   {-# NOINLINE st23 #-}
   st23 = let
      v22 :: T_Patterns_v22 
      v22 = \ (T_Patterns_vIn22 _lhsIallTypeSigs _lhsIaltAttrs _lhsIbelowIrrefutable _lhsIcon _lhsIinh _lhsInt _lhsIsyn) -> ( let
         _lhsOpps :: [PP_Doc]
         _lhsOpps = rule335  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule336  ()
         _lhsOgathAltAttrs :: [AltAttr]
         _lhsOgathAltAttrs = rule337  ()
         _lhsOinstVars :: [Identifier]
         _lhsOinstVars = rule338  ()
         _lhsOlocVars :: [Identifier]
         _lhsOlocVars = rule339  ()
         _lhsOpatternAttrs :: [(Identifier,Identifier,Bool)]
         _lhsOpatternAttrs = rule340  ()
         _copy = rule341  ()
         _lhsOcopy :: Patterns
         _lhsOcopy = rule342 _copy
         __result_ = T_Patterns_vOut22 _lhsOcopy _lhsOerrors _lhsOgathAltAttrs _lhsOinstVars _lhsOlocVars _lhsOpatternAttrs _lhsOpps
         in __result_ )
     in C_Patterns_s23 v22
   {-# INLINE rule335 #-}
   {-# LINE 77 "src-ag/TfmToMirage.ag" #-}
   rule335 = \  (_ :: ()) ->
                     {-# LINE 77 "src-ag/TfmToMirage.ag" #-}
                     []
                     {-# LINE 2551 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule336 #-}
   rule336 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule337 #-}
   rule337 = \  (_ :: ()) ->
     []
   {-# INLINE rule338 #-}
   rule338 = \  (_ :: ()) ->
     []
   {-# INLINE rule339 #-}
   rule339 = \  (_ :: ()) ->
     []
   {-# INLINE rule340 #-}
   rule340 = \  (_ :: ()) ->
     []
   {-# INLINE rule341 #-}
   rule341 = \  (_ :: ()) ->
     []
   {-# INLINE rule342 #-}
   rule342 = \ _copy ->
     _copy

-- Production --------------------------------------------------
-- wrapper
data Inh_Production  = Inh_Production { allnts_Inh_Production :: ([Identifier]), aroundMap_Inh_Production :: (Map ConstructorIdent (Map Identifier [Expression])), cVisitsMap_Inh_Production :: (CVisitsMap), inh_Inh_Production :: (Attributes), inhMap_Inh_Production :: (Map Identifier Attributes), manualAttrDepMap_Inh_Production :: (AttrOrderMap), mergeMap_Inh_Production :: (Map ConstructorIdent (Map Identifier (Identifier,[Identifier]))), nt_Inh_Production :: (Identifier), o_case_Inh_Production :: (Bool), o_cata_Inh_Production :: (Bool), o_dovisit_Inh_Production :: (Bool), o_newtypes_Inh_Production :: (Bool), o_rename_Inh_Production :: (Bool), o_sem_Inh_Production :: (Bool), o_sig_Inh_Production :: (Bool), o_unbox_Inh_Production :: (Bool), o_wantvisit_Inh_Production :: (Bool), options_Inh_Production :: (Options), prefix_Inh_Production :: (String), syn_Inh_Production :: (Attributes), synMap_Inh_Production :: (Map Identifier Attributes), vcount_Inh_Production :: (Int) }
data Syn_Production  = Syn_Production { additionalDep_Syn_Production :: (Seq Edge), aroundDep_Syn_Production :: (Seq Edge), cProduction_Syn_Production :: (CProduction), cons_Syn_Production :: ([ConstructorIdent]), directDep_Syn_Production :: (Seq Edge), errors_Syn_Production :: (Seq Error), instDep_Syn_Production :: (Seq Edge), mergeDep_Syn_Production :: (Seq Edge), mirage_Syn_Production :: (Mirage.Production), nAutoRules_Syn_Production :: (Int), nExplicitRules_Syn_Production :: (Int), rules_Syn_Production :: (Seq (Vertex,CRule)), vcount_Syn_Production :: (Int) }
{-# INLINABLE wrap_Production #-}
wrap_Production :: T_Production  -> Inh_Production  -> (Syn_Production )
wrap_Production (T_Production act) (Inh_Production _lhsIallnts _lhsIaroundMap _lhsIcVisitsMap _lhsIinh _lhsIinhMap _lhsImanualAttrDepMap _lhsImergeMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_unbox _lhsIo_wantvisit _lhsIoptions _lhsIprefix _lhsIsyn _lhsIsynMap _lhsIvcount) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg25 = T_Production_vIn25 _lhsIallnts _lhsIaroundMap _lhsIcVisitsMap _lhsIinh _lhsIinhMap _lhsImanualAttrDepMap _lhsImergeMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_unbox _lhsIo_wantvisit _lhsIoptions _lhsIprefix _lhsIsyn _lhsIsynMap _lhsIvcount
        (T_Production_vOut25 _lhsOadditionalDep _lhsOaroundDep _lhsOcProduction _lhsOcons _lhsOdirectDep _lhsOerrors _lhsOinstDep _lhsOmergeDep _lhsOmirage _lhsOnAutoRules _lhsOnExplicitRules _lhsOrules _lhsOvcount) <- return (inv_Production_s26 sem arg25)
        return (Syn_Production _lhsOadditionalDep _lhsOaroundDep _lhsOcProduction _lhsOcons _lhsOdirectDep _lhsOerrors _lhsOinstDep _lhsOmergeDep _lhsOmirage _lhsOnAutoRules _lhsOnExplicitRules _lhsOrules _lhsOvcount)
   )

-- cata
{-# INLINE sem_Production #-}
sem_Production :: Production  -> T_Production 
sem_Production ( Production con_ params_ constraints_ children_ rules_ typeSigs_ macro_ ) = sem_Production_Production con_ params_ constraints_ ( sem_Children children_ ) ( sem_Rules rules_ ) ( sem_TypeSigs typeSigs_ ) macro_

-- semantic domain
newtype T_Production  = T_Production {
                                     attach_T_Production :: Identity (T_Production_s26 )
                                     }
newtype T_Production_s26  = C_Production_s26 {
                                             inv_Production_s26 :: (T_Production_v25 )
                                             }
data T_Production_s27  = C_Production_s27
type T_Production_v25  = (T_Production_vIn25 ) -> (T_Production_vOut25 )
data T_Production_vIn25  = T_Production_vIn25 ([Identifier]) (Map ConstructorIdent (Map Identifier [Expression])) (CVisitsMap) (Attributes) (Map Identifier Attributes) (AttrOrderMap) (Map ConstructorIdent (Map Identifier (Identifier,[Identifier]))) (Identifier) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Options) (String) (Attributes) (Map Identifier Attributes) (Int)
data T_Production_vOut25  = T_Production_vOut25 (Seq Edge) (Seq Edge) (CProduction) ([ConstructorIdent]) (Seq Edge) (Seq Error) (Seq Edge) (Seq Edge) (Mirage.Production) (Int) (Int) (Seq (Vertex,CRule)) (Int)
{-# NOINLINE sem_Production_Production #-}
sem_Production_Production :: (ConstructorIdent) -> ([Identifier]) -> ([Type]) -> T_Children  -> T_Rules  -> T_TypeSigs  -> (MaybeMacro) -> T_Production 
sem_Production_Production arg_con_ _ _ arg_children_ arg_rules_ arg_typeSigs_ _ = T_Production (return st26) where
   {-# NOINLINE st26 #-}
   st26 = let
      v25 :: T_Production_v25 
      v25 = \ (T_Production_vIn25 _lhsIallnts _lhsIaroundMap _lhsIcVisitsMap _lhsIinh _lhsIinhMap _lhsImanualAttrDepMap _lhsImergeMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_unbox _lhsIo_wantvisit _lhsIoptions _lhsIprefix _lhsIsyn _lhsIsynMap _lhsIvcount) -> ( let
         _childrenX5 = Control.Monad.Identity.runIdentity (attach_T_Children (arg_children_))
         _rulesX35 = Control.Monad.Identity.runIdentity (attach_T_Rules (arg_rules_))
         _typeSigsX41 = Control.Monad.Identity.runIdentity (attach_T_TypeSigs (arg_typeSigs_))
         (T_Children_vOut4 _childrenIattributes _childrenIcollectChildrenInhs _childrenIcollectChildrenSyns _childrenIerrors _childrenIfields _childrenIgathAltAttrs _childrenIgathRules _childrenIinhs _childrenImirages _childrenInts _childrenIsinglevisits _childrenIterminals) = inv_Children_s5 _childrenX5 (T_Children_vIn4 _childrenOallfields _childrenOallnts _childrenOattrs _childrenOcon _childrenOinh _childrenOinhMap _childrenOmergeMap _childrenOnt _childrenOo_unbox _childrenOsyn _childrenOsynMap)
         (T_Rules_vOut34 _rulesIdirectDep _rulesIerrors _rulesIgathAltAttrs _rulesIgathRules _rulesIinstDep _rulesIinstVars _rulesIlocVars _rulesImirages _rulesInAutoRules _rulesInExplicitRules) = inv_Rules_s35 _rulesX35 (T_Rules_vIn34 _rulesOallTypeSigs _rulesOallfields _rulesOallnts _rulesOaltAttrs _rulesOattrs _rulesOchildInhs _rulesOchildNts _rulesOcon _rulesOinh _rulesOinhsOfChildren _rulesOmergeMap _rulesOnt _rulesOo_case _rulesOo_cata _rulesOo_dovisit _rulesOo_newtypes _rulesOo_rename _rulesOo_sem _rulesOo_sig _rulesOo_wantvisit _rulesOoptions _rulesOprefix _rulesOsyn _rulesOsynsOfChildren)
         (T_TypeSigs_vOut40 _typeSigsItypeSigs) = inv_TypeSigs_s41 _typeSigsX41 (T_TypeSigs_vIn40 _typeSigsOtypeSigs)
         _childrenOcon = rule343 arg_con_
         _rulesOcon = rule344 arg_con_
         _gathAltAttrs = rule345 _childrenIgathAltAttrs _lhsIinh _rulesIgathAltAttrs
         _altAttrs = rule346 _gathAltAttrs _lhsIvcount
         _rulesOchildNts = rule347 _childrenInts
         _rulesOchildInhs = rule348 _childrenIinhs
         _inhRules = rule349 _lhsIinh _lhsInt arg_con_
         _gathRules = rule350 _childrenIgathRules _inhRules _rulesIgathRules
         _lhsOrules :: Seq (Vertex,CRule)
         _lhsOrules = rule351 _gathRules _lhsIvcount
         _lhsOvcount :: Int
         _lhsOvcount = rule352 _gathRules _lhsIvcount
         _manualDeps = rule353 _lhsImanualAttrDepMap _lhsInt arg_con_
         _lhsOadditionalDep :: Seq Edge
         _lhsOadditionalDep = rule354 _altAttrs _manualDeps
         _rulesOsynsOfChildren = rule355 _childrenIcollectChildrenSyns
         _rulesOinhsOfChildren = rule356 _childrenIcollectChildrenInhs
         _mergeMap = rule357 _lhsImergeMap arg_con_
         _lhsOmergeDep :: Seq Edge
         _lhsOmergeDep = rule358 _mergeDep1 _mergeDep2
         _mergeDep1 = rule359 _altAttrs _childrenIcollectChildrenSyns _mergeMap
         _mergeDep2 = rule360 _altAttrs _childrenIcollectChildrenSyns _mergeMap
         _aroundMap = rule361 _lhsIaroundMap arg_con_
         _aroundDep1 = rule362 _altAttrs _aroundMap _childrenIcollectChildrenSyns
         _aroundDep2 = rule363 _altAttrs _aroundMap _childrenIcollectChildrenInhs
         _lhsOaroundDep :: Seq Edge
         _lhsOaroundDep = rule364 _aroundDep1 _aroundDep2
         _lhsOcons :: [ConstructorIdent]
         _lhsOcons = rule365 arg_con_
         _typeSigsOtypeSigs = rule366  ()
         _rulesOallTypeSigs = rule367 _typeSigsItypeSigs
         _cVisits = rule368 _childrenIsinglevisits _gathRules _lhsIcVisitsMap _lhsIinh _lhsInt _lhsIo_dovisit _lhsIsyn arg_con_
         _lhsOcProduction :: CProduction
         _lhsOcProduction = rule369 _cVisits _childrenIfields _childrenIterminals arg_con_
         _allfields = rule370 _childrenIfields
         _attrs = rule371 _childrenIattributes _inhnames _rulesIinstVars _rulesIlocVars
         _inhnames = rule372 _lhsIinh
         _synnames = rule373 _lhsIsyn
         _lhsOmirage :: Mirage.Production
         _lhsOmirage = rule374 _childrenImirages _rulesImirages arg_con_
         _lhsOdirectDep :: Seq Edge
         _lhsOdirectDep = rule375 _rulesIdirectDep
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule376 _childrenIerrors _rulesIerrors
         _lhsOinstDep :: Seq Edge
         _lhsOinstDep = rule377 _rulesIinstDep
         _lhsOnAutoRules :: Int
         _lhsOnAutoRules = rule378 _rulesInAutoRules
         _lhsOnExplicitRules :: Int
         _lhsOnExplicitRules = rule379 _rulesInExplicitRules
         _childrenOallfields = rule380 _allfields
         _childrenOallnts = rule381 _lhsIallnts
         _childrenOattrs = rule382 _attrs
         _childrenOinh = rule383 _lhsIinh
         _childrenOinhMap = rule384 _lhsIinhMap
         _childrenOmergeMap = rule385 _mergeMap
         _childrenOnt = rule386 _lhsInt
         _childrenOo_unbox = rule387 _lhsIo_unbox
         _childrenOsyn = rule388 _lhsIsyn
         _childrenOsynMap = rule389 _lhsIsynMap
         _rulesOallfields = rule390 _allfields
         _rulesOallnts = rule391 _lhsIallnts
         _rulesOaltAttrs = rule392 _altAttrs
         _rulesOattrs = rule393 _attrs
         _rulesOinh = rule394 _lhsIinh
         _rulesOmergeMap = rule395 _mergeMap
         _rulesOnt = rule396 _lhsInt
         _rulesOo_case = rule397 _lhsIo_case
         _rulesOo_cata = rule398 _lhsIo_cata
         _rulesOo_dovisit = rule399 _lhsIo_dovisit
         _rulesOo_newtypes = rule400 _lhsIo_newtypes
         _rulesOo_rename = rule401 _lhsIo_rename
         _rulesOo_sem = rule402 _lhsIo_sem
         _rulesOo_sig = rule403 _lhsIo_sig
         _rulesOo_wantvisit = rule404 _lhsIo_wantvisit
         _rulesOoptions = rule405 _lhsIoptions
         _rulesOprefix = rule406 _lhsIprefix
         _rulesOsyn = rule407 _lhsIsyn
         __result_ = T_Production_vOut25 _lhsOadditionalDep _lhsOaroundDep _lhsOcProduction _lhsOcons _lhsOdirectDep _lhsOerrors _lhsOinstDep _lhsOmergeDep _lhsOmirage _lhsOnAutoRules _lhsOnExplicitRules _lhsOrules _lhsOvcount
         in __result_ )
     in C_Production_s26 v25
   {-# INLINE rule343 #-}
   {-# LINE 93 "src-ag/Order.ag" #-}
   rule343 = \ con_ ->
                                  {-# LINE 93 "src-ag/Order.ag" #-}
                                  con_
                                  {-# LINE 2703 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule344 #-}
   {-# LINE 95 "src-ag/Order.ag" #-}
   rule344 = \ con_ ->
                               {-# LINE 95 "src-ag/Order.ag" #-}
                               con_
                               {-# LINE 2709 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule345 #-}
   {-# LINE 175 "src-ag/Order.ag" #-}
   rule345 = \ ((_childrenIgathAltAttrs) :: [AltAttr]) ((_lhsIinh) :: Attributes) ((_rulesIgathAltAttrs) :: [AltAttr]) ->
                                       {-# LINE 175 "src-ag/Order.ag" #-}
                                       [ AltAttr _LHS inh True | inh <- Map.keys _lhsIinh ]
                                        ++ _childrenIgathAltAttrs
                                        ++ _rulesIgathAltAttrs
                                       {-# LINE 2717 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule346 #-}
   {-# LINE 191 "src-ag/Order.ag" #-}
   rule346 = \ _gathAltAttrs ((_lhsIvcount) :: Int) ->
                                 {-# LINE 191 "src-ag/Order.ag" #-}
                                 Map.fromList (zip _gathAltAttrs [_lhsIvcount..])
                                 {-# LINE 2723 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule347 #-}
   {-# LINE 204 "src-ag/Order.ag" #-}
   rule347 = \ ((_childrenInts) :: Seq (Identifier,NontermIdent)) ->
                                    {-# LINE 204 "src-ag/Order.ag" #-}
                                    Map.fromList (toList _childrenInts)
                                    {-# LINE 2729 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule348 #-}
   {-# LINE 205 "src-ag/Order.ag" #-}
   rule348 = \ ((_childrenIinhs) :: Seq (Identifier,Attributes)) ->
                                      {-# LINE 205 "src-ag/Order.ag" #-}
                                      Map.fromList (toList _childrenIinhs)
                                      {-# LINE 2735 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule349 #-}
   {-# LINE 211 "src-ag/Order.ag" #-}
   rule349 = \ ((_lhsIinh) :: Attributes) ((_lhsInt) :: Identifier) con_ ->
                                  {-# LINE 211 "src-ag/Order.ag" #-}
                                  [ cRuleLhsInh inh _lhsInt con_ tp | (inh,tp) <- Map.assocs _lhsIinh ]
                                  {-# LINE 2741 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule350 #-}
   {-# LINE 212 "src-ag/Order.ag" #-}
   rule350 = \ ((_childrenIgathRules) :: Seq CRule) _inhRules ((_rulesIgathRules) :: Seq CRule) ->
                                    {-# LINE 212 "src-ag/Order.ag" #-}
                                    _inhRules ++ toList (_childrenIgathRules Seq.>< _rulesIgathRules)
                                    {-# LINE 2747 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule351 #-}
   {-# LINE 264 "src-ag/Order.ag" #-}
   rule351 = \ _gathRules ((_lhsIvcount) :: Int) ->
                               {-# LINE 264 "src-ag/Order.ag" #-}
                               Seq.fromList (zip [_lhsIvcount..] _gathRules)
                               {-# LINE 2753 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule352 #-}
   {-# LINE 265 "src-ag/Order.ag" #-}
   rule352 = \ _gathRules ((_lhsIvcount) :: Int) ->
                                 {-# LINE 265 "src-ag/Order.ag" #-}
                                 _lhsIvcount + length _gathRules
                                 {-# LINE 2759 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule353 #-}
   {-# LINE 293 "src-ag/Order.ag" #-}
   rule353 = \ ((_lhsImanualAttrDepMap) :: AttrOrderMap) ((_lhsInt) :: Identifier) con_ ->
            {-# LINE 293 "src-ag/Order.ag" #-}
            Set.toList $ Map.findWithDefault Set.empty con_ $ Map.findWithDefault Map.empty _lhsInt _lhsImanualAttrDepMap
            {-# LINE 2765 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule354 #-}
   {-# LINE 296 "src-ag/Order.ag" #-}
   rule354 = \ _altAttrs _manualDeps ->
            {-# LINE 296 "src-ag/Order.ag" #-}
            Seq.fromList [ (vertex True occA, vertex False occB)
                         | Dependency occA occB <- _manualDeps
                         , let vertex inout (OccAttr child nm)
                                 | child == _LOC = findWithErr2 (AltAttr _LOC nm True) _altAttrs
                                 | otherwise     = findWithErr2 (AltAttr child nm inout) _altAttrs
                               vertex _ (OccRule nm)
                                 = findWithErr2 (AltAttr _LOC (Ident ("_rule_" ++ show nm) (getPos nm)) True) _altAttrs
                         ]
            {-# LINE 2778 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule355 #-}
   {-# LINE 342 "src-ag/Order.ag" #-}
   rule355 = \ ((_childrenIcollectChildrenSyns) :: Map Identifier Attributes ) ->
                                         {-# LINE 342 "src-ag/Order.ag" #-}
                                         _childrenIcollectChildrenSyns
                                         {-# LINE 2784 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule356 #-}
   {-# LINE 343 "src-ag/Order.ag" #-}
   rule356 = \ ((_childrenIcollectChildrenInhs) :: Map Identifier Attributes ) ->
                                         {-# LINE 343 "src-ag/Order.ag" #-}
                                         _childrenIcollectChildrenInhs
                                         {-# LINE 2790 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule357 #-}
   {-# LINE 361 "src-ag/Order.ag" #-}
   rule357 = \ ((_lhsImergeMap) :: Map ConstructorIdent (Map Identifier (Identifier,[Identifier]))) con_ ->
                                                {-# LINE 361 "src-ag/Order.ag" #-}
                                                Map.findWithDefault Map.empty con_ _lhsImergeMap
                                                {-# LINE 2796 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule358 #-}
   {-# LINE 372 "src-ag/Order.ag" #-}
   rule358 = \ _mergeDep1 _mergeDep2 ->
                       {-# LINE 372 "src-ag/Order.ag" #-}
                       _mergeDep1     Seq.>< _mergeDep2
                       {-# LINE 2802 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule359 #-}
   {-# LINE 374 "src-ag/Order.ag" #-}
   rule359 = \ _altAttrs ((_childrenIcollectChildrenSyns) :: Map Identifier Attributes ) _mergeMap ->
            {-# LINE 374 "src-ag/Order.ag" #-}
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
            {-# LINE 2817 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule360 #-}
   {-# LINE 385 "src-ag/Order.ag" #-}
   rule360 = \ _altAttrs ((_childrenIcollectChildrenSyns) :: Map Identifier Attributes ) _mergeMap ->
            {-# LINE 385 "src-ag/Order.ag" #-}
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
            {-# LINE 2832 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule361 #-}
   {-# LINE 414 "src-ag/Order.ag" #-}
   rule361 = \ ((_lhsIaroundMap) :: Map ConstructorIdent (Map Identifier [Expression])) con_ ->
                                                 {-# LINE 414 "src-ag/Order.ag" #-}
                                                 Map.findWithDefault Map.empty con_ _lhsIaroundMap
                                                 {-# LINE 2838 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule362 #-}
   {-# LINE 422 "src-ag/Order.ag" #-}
   rule362 = \ _altAttrs _aroundMap ((_childrenIcollectChildrenSyns) :: Map Identifier Attributes ) ->
           {-# LINE 422 "src-ag/Order.ag" #-}
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
           {-# LINE 2853 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule363 #-}
   {-# LINE 433 "src-ag/Order.ag" #-}
   rule363 = \ _altAttrs _aroundMap ((_childrenIcollectChildrenInhs) :: Map Identifier Attributes ) ->
            {-# LINE 433 "src-ag/Order.ag" #-}
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
            {-# LINE 2868 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule364 #-}
   {-# LINE 443 "src-ag/Order.ag" #-}
   rule364 = \ _aroundDep1 _aroundDep2 ->
                       {-# LINE 443 "src-ag/Order.ag" #-}
                       _aroundDep1     Seq.>< _aroundDep2
                       {-# LINE 2874 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule365 #-}
   {-# LINE 527 "src-ag/Order.ag" #-}
   rule365 = \ con_ ->
                              {-# LINE 527 "src-ag/Order.ag" #-}
                              [con_]
                              {-# LINE 2880 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule366 #-}
   {-# LINE 534 "src-ag/Order.ag" #-}
   rule366 = \  (_ :: ()) ->
                                     {-# LINE 534 "src-ag/Order.ag" #-}
                                     Map.empty
                                     {-# LINE 2886 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule367 #-}
   {-# LINE 540 "src-ag/Order.ag" #-}
   rule367 = \ ((_typeSigsItypeSigs) :: Map Identifier Type) ->
                                      {-# LINE 540 "src-ag/Order.ag" #-}
                                      _typeSigsItypeSigs
                                      {-# LINE 2892 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule368 #-}
   {-# LINE 608 "src-ag/Order.ag" #-}
   rule368 = \ ((_childrenIsinglevisits) :: [CRule]) _gathRules ((_lhsIcVisitsMap) :: CVisitsMap) ((_lhsIinh) :: Attributes) ((_lhsInt) :: Identifier) ((_lhsIo_dovisit) :: Bool) ((_lhsIsyn) :: Attributes) con_ ->
                                {-# LINE 608 "src-ag/Order.ag" #-}
                                if  _lhsIo_dovisit
                                     then let prodsVisitsMap = findWithErr1 "Production.cVisits.nt" _lhsInt _lhsIcVisitsMap
                                              visits = findWithErr1 "Production.cVisits.con" con_ prodsVisitsMap
                                           in visits
                                     else  let  vss = nubBy eqCRuleDefines _gathRules ++ _childrenIsinglevisits
                                           in  [CVisit _lhsIinh _lhsIsyn vss [] False]
                                {-# LINE 2903 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule369 #-}
   {-# LINE 634 "src-ag/Order.ag" #-}
   rule369 = \ _cVisits ((_childrenIfields) :: [(Identifier,Type,ChildKind)]) ((_childrenIterminals) :: [Identifier]) con_ ->
                                     {-# LINE 634 "src-ag/Order.ag" #-}
                                     CProduction con_ _cVisits _childrenIfields _childrenIterminals
                                     {-# LINE 2909 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule370 #-}
   {-# LINE 662 "src-ag/Order.ag" #-}
   rule370 = \ ((_childrenIfields) :: [(Identifier,Type,ChildKind)]) ->
                                  {-# LINE 662 "src-ag/Order.ag" #-}
                                  _childrenIfields
                                  {-# LINE 2915 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule371 #-}
   {-# LINE 663 "src-ag/Order.ag" #-}
   rule371 = \ ((_childrenIattributes) :: [(Identifier,Attributes,Attributes)]) _inhnames ((_rulesIinstVars) :: [Identifier]) ((_rulesIlocVars) :: [Identifier]) ->
                                   {-# LINE 663 "src-ag/Order.ag" #-}
                                   map ((,) _LOC)  _rulesIlocVars ++
                                   map ((,) _INST) _rulesIinstVars ++
                                   map ((,) _LHS)  _inhnames ++
                                   concat [map ((,) nm) (Map.keys as) | (nm,_,as) <- _childrenIattributes]
                                   {-# LINE 2924 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule372 #-}
   {-# LINE 667 "src-ag/Order.ag" #-}
   rule372 = \ ((_lhsIinh) :: Attributes) ->
                                   {-# LINE 667 "src-ag/Order.ag" #-}
                                   Map.keys _lhsIinh
                                   {-# LINE 2930 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule373 #-}
   {-# LINE 668 "src-ag/Order.ag" #-}
   rule373 = \ ((_lhsIsyn) :: Attributes) ->
                                   {-# LINE 668 "src-ag/Order.ag" #-}
                                   Map.keys _lhsIsyn
                                   {-# LINE 2936 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule374 #-}
   {-# LINE 43 "src-ag/TfmToMirage.ag" #-}
   rule374 = \ ((_childrenImirages) :: [Mirage.Child]) ((_rulesImirages) :: [Mirage.Rule]) con_ ->
          {-# LINE 43 "src-ag/TfmToMirage.ag" #-}
          Mirage.Production (getName con_) _childrenImirages _rulesImirages
          {-# LINE 2942 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule375 #-}
   rule375 = \ ((_rulesIdirectDep) :: Seq Edge) ->
     _rulesIdirectDep
   {-# INLINE rule376 #-}
   rule376 = \ ((_childrenIerrors) :: Seq Error) ((_rulesIerrors) :: Seq Error) ->
     _childrenIerrors Seq.>< _rulesIerrors
   {-# INLINE rule377 #-}
   rule377 = \ ((_rulesIinstDep) :: Seq Edge) ->
     _rulesIinstDep
   {-# INLINE rule378 #-}
   rule378 = \ ((_rulesInAutoRules) :: Int) ->
     _rulesInAutoRules
   {-# INLINE rule379 #-}
   rule379 = \ ((_rulesInExplicitRules) :: Int) ->
     _rulesInExplicitRules
   {-# INLINE rule380 #-}
   rule380 = \ _allfields ->
     _allfields
   {-# INLINE rule381 #-}
   rule381 = \ ((_lhsIallnts) :: [Identifier]) ->
     _lhsIallnts
   {-# INLINE rule382 #-}
   rule382 = \ _attrs ->
     _attrs
   {-# INLINE rule383 #-}
   rule383 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule384 #-}
   rule384 = \ ((_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# INLINE rule385 #-}
   rule385 = \ _mergeMap ->
     _mergeMap
   {-# INLINE rule386 #-}
   rule386 = \ ((_lhsInt) :: Identifier) ->
     _lhsInt
   {-# INLINE rule387 #-}
   rule387 = \ ((_lhsIo_unbox) :: Bool) ->
     _lhsIo_unbox
   {-# INLINE rule388 #-}
   rule388 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
   {-# INLINE rule389 #-}
   rule389 = \ ((_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
   {-# INLINE rule390 #-}
   rule390 = \ _allfields ->
     _allfields
   {-# INLINE rule391 #-}
   rule391 = \ ((_lhsIallnts) :: [Identifier]) ->
     _lhsIallnts
   {-# INLINE rule392 #-}
   rule392 = \ _altAttrs ->
     _altAttrs
   {-# INLINE rule393 #-}
   rule393 = \ _attrs ->
     _attrs
   {-# INLINE rule394 #-}
   rule394 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule395 #-}
   rule395 = \ _mergeMap ->
     _mergeMap
   {-# INLINE rule396 #-}
   rule396 = \ ((_lhsInt) :: Identifier) ->
     _lhsInt
   {-# INLINE rule397 #-}
   rule397 = \ ((_lhsIo_case) :: Bool) ->
     _lhsIo_case
   {-# INLINE rule398 #-}
   rule398 = \ ((_lhsIo_cata) :: Bool) ->
     _lhsIo_cata
   {-# INLINE rule399 #-}
   rule399 = \ ((_lhsIo_dovisit) :: Bool) ->
     _lhsIo_dovisit
   {-# INLINE rule400 #-}
   rule400 = \ ((_lhsIo_newtypes) :: Bool) ->
     _lhsIo_newtypes
   {-# INLINE rule401 #-}
   rule401 = \ ((_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# INLINE rule402 #-}
   rule402 = \ ((_lhsIo_sem) :: Bool) ->
     _lhsIo_sem
   {-# INLINE rule403 #-}
   rule403 = \ ((_lhsIo_sig) :: Bool) ->
     _lhsIo_sig
   {-# INLINE rule404 #-}
   rule404 = \ ((_lhsIo_wantvisit) :: Bool) ->
     _lhsIo_wantvisit
   {-# INLINE rule405 #-}
   rule405 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule406 #-}
   rule406 = \ ((_lhsIprefix) :: String) ->
     _lhsIprefix
   {-# INLINE rule407 #-}
   rule407 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn

-- Productions -------------------------------------------------
-- wrapper
data Inh_Productions  = Inh_Productions { allnts_Inh_Productions :: ([Identifier]), aroundMap_Inh_Productions :: (Map ConstructorIdent (Map Identifier [Expression])), cVisitsMap_Inh_Productions :: (CVisitsMap), inh_Inh_Productions :: (Attributes), inhMap_Inh_Productions :: (Map Identifier Attributes), manualAttrDepMap_Inh_Productions :: (AttrOrderMap), mergeMap_Inh_Productions :: (Map ConstructorIdent (Map Identifier (Identifier,[Identifier]))), nt_Inh_Productions :: (Identifier), o_case_Inh_Productions :: (Bool), o_cata_Inh_Productions :: (Bool), o_dovisit_Inh_Productions :: (Bool), o_newtypes_Inh_Productions :: (Bool), o_rename_Inh_Productions :: (Bool), o_sem_Inh_Productions :: (Bool), o_sig_Inh_Productions :: (Bool), o_unbox_Inh_Productions :: (Bool), o_wantvisit_Inh_Productions :: (Bool), options_Inh_Productions :: (Options), prefix_Inh_Productions :: (String), syn_Inh_Productions :: (Attributes), synMap_Inh_Productions :: (Map Identifier Attributes), vcount_Inh_Productions :: (Int) }
data Syn_Productions  = Syn_Productions { additionalDep_Syn_Productions :: (Seq Edge), aroundDep_Syn_Productions :: (Seq Edge), cProductions_Syn_Productions :: (CProductions), cons_Syn_Productions :: ([ConstructorIdent]), directDep_Syn_Productions :: (Seq Edge), errors_Syn_Productions :: (Seq Error), instDep_Syn_Productions :: (Seq Edge), mergeDep_Syn_Productions :: (Seq Edge), mirages_Syn_Productions :: ([Mirage.Production]), nAutoRules_Syn_Productions :: (Int), nExplicitRules_Syn_Productions :: (Int), rules_Syn_Productions :: (Seq (Vertex,CRule)), vcount_Syn_Productions :: (Int) }
{-# INLINABLE wrap_Productions #-}
wrap_Productions :: T_Productions  -> Inh_Productions  -> (Syn_Productions )
wrap_Productions (T_Productions act) (Inh_Productions _lhsIallnts _lhsIaroundMap _lhsIcVisitsMap _lhsIinh _lhsIinhMap _lhsImanualAttrDepMap _lhsImergeMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_unbox _lhsIo_wantvisit _lhsIoptions _lhsIprefix _lhsIsyn _lhsIsynMap _lhsIvcount) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg28 = T_Productions_vIn28 _lhsIallnts _lhsIaroundMap _lhsIcVisitsMap _lhsIinh _lhsIinhMap _lhsImanualAttrDepMap _lhsImergeMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_unbox _lhsIo_wantvisit _lhsIoptions _lhsIprefix _lhsIsyn _lhsIsynMap _lhsIvcount
        (T_Productions_vOut28 _lhsOadditionalDep _lhsOaroundDep _lhsOcProductions _lhsOcons _lhsOdirectDep _lhsOerrors _lhsOinstDep _lhsOmergeDep _lhsOmirages _lhsOnAutoRules _lhsOnExplicitRules _lhsOrules _lhsOvcount) <- return (inv_Productions_s29 sem arg28)
        return (Syn_Productions _lhsOadditionalDep _lhsOaroundDep _lhsOcProductions _lhsOcons _lhsOdirectDep _lhsOerrors _lhsOinstDep _lhsOmergeDep _lhsOmirages _lhsOnAutoRules _lhsOnExplicitRules _lhsOrules _lhsOvcount)
   )

-- cata
{-# NOINLINE sem_Productions #-}
sem_Productions :: Productions  -> T_Productions 
sem_Productions list = Prelude.foldr sem_Productions_Cons sem_Productions_Nil (Prelude.map sem_Production list)

-- semantic domain
newtype T_Productions  = T_Productions {
                                       attach_T_Productions :: Identity (T_Productions_s29 )
                                       }
newtype T_Productions_s29  = C_Productions_s29 {
                                               inv_Productions_s29 :: (T_Productions_v28 )
                                               }
data T_Productions_s30  = C_Productions_s30
type T_Productions_v28  = (T_Productions_vIn28 ) -> (T_Productions_vOut28 )
data T_Productions_vIn28  = T_Productions_vIn28 ([Identifier]) (Map ConstructorIdent (Map Identifier [Expression])) (CVisitsMap) (Attributes) (Map Identifier Attributes) (AttrOrderMap) (Map ConstructorIdent (Map Identifier (Identifier,[Identifier]))) (Identifier) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Options) (String) (Attributes) (Map Identifier Attributes) (Int)
data T_Productions_vOut28  = T_Productions_vOut28 (Seq Edge) (Seq Edge) (CProductions) ([ConstructorIdent]) (Seq Edge) (Seq Error) (Seq Edge) (Seq Edge) ([Mirage.Production]) (Int) (Int) (Seq (Vertex,CRule)) (Int)
{-# NOINLINE sem_Productions_Cons #-}
sem_Productions_Cons :: T_Production  -> T_Productions  -> T_Productions 
sem_Productions_Cons arg_hd_ arg_tl_ = T_Productions (return st29) where
   {-# NOINLINE st29 #-}
   st29 = let
      v28 :: T_Productions_v28 
      v28 = \ (T_Productions_vIn28 _lhsIallnts _lhsIaroundMap _lhsIcVisitsMap _lhsIinh _lhsIinhMap _lhsImanualAttrDepMap _lhsImergeMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_unbox _lhsIo_wantvisit _lhsIoptions _lhsIprefix _lhsIsyn _lhsIsynMap _lhsIvcount) -> ( let
         _hdX26 = Control.Monad.Identity.runIdentity (attach_T_Production (arg_hd_))
         _tlX29 = Control.Monad.Identity.runIdentity (attach_T_Productions (arg_tl_))
         (T_Production_vOut25 _hdIadditionalDep _hdIaroundDep _hdIcProduction _hdIcons _hdIdirectDep _hdIerrors _hdIinstDep _hdImergeDep _hdImirage _hdInAutoRules _hdInExplicitRules _hdIrules _hdIvcount) = inv_Production_s26 _hdX26 (T_Production_vIn25 _hdOallnts _hdOaroundMap _hdOcVisitsMap _hdOinh _hdOinhMap _hdOmanualAttrDepMap _hdOmergeMap _hdOnt _hdOo_case _hdOo_cata _hdOo_dovisit _hdOo_newtypes _hdOo_rename _hdOo_sem _hdOo_sig _hdOo_unbox _hdOo_wantvisit _hdOoptions _hdOprefix _hdOsyn _hdOsynMap _hdOvcount)
         (T_Productions_vOut28 _tlIadditionalDep _tlIaroundDep _tlIcProductions _tlIcons _tlIdirectDep _tlIerrors _tlIinstDep _tlImergeDep _tlImirages _tlInAutoRules _tlInExplicitRules _tlIrules _tlIvcount) = inv_Productions_s29 _tlX29 (T_Productions_vIn28 _tlOallnts _tlOaroundMap _tlOcVisitsMap _tlOinh _tlOinhMap _tlOmanualAttrDepMap _tlOmergeMap _tlOnt _tlOo_case _tlOo_cata _tlOo_dovisit _tlOo_newtypes _tlOo_rename _tlOo_sem _tlOo_sig _tlOo_unbox _tlOo_wantvisit _tlOoptions _tlOprefix _tlOsyn _tlOsynMap _tlOvcount)
         _lhsOcProductions :: CProductions
         _lhsOcProductions = rule408 _hdIcProduction _tlIcProductions
         _lhsOmirages :: [Mirage.Production]
         _lhsOmirages = rule409 _hdImirage _tlImirages
         _lhsOadditionalDep :: Seq Edge
         _lhsOadditionalDep = rule410 _hdIadditionalDep _tlIadditionalDep
         _lhsOaroundDep :: Seq Edge
         _lhsOaroundDep = rule411 _hdIaroundDep _tlIaroundDep
         _lhsOcons :: [ConstructorIdent]
         _lhsOcons = rule412 _hdIcons _tlIcons
         _lhsOdirectDep :: Seq Edge
         _lhsOdirectDep = rule413 _hdIdirectDep _tlIdirectDep
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule414 _hdIerrors _tlIerrors
         _lhsOinstDep :: Seq Edge
         _lhsOinstDep = rule415 _hdIinstDep _tlIinstDep
         _lhsOmergeDep :: Seq Edge
         _lhsOmergeDep = rule416 _hdImergeDep _tlImergeDep
         _lhsOnAutoRules :: Int
         _lhsOnAutoRules = rule417 _hdInAutoRules _tlInAutoRules
         _lhsOnExplicitRules :: Int
         _lhsOnExplicitRules = rule418 _hdInExplicitRules _tlInExplicitRules
         _lhsOrules :: Seq (Vertex,CRule)
         _lhsOrules = rule419 _hdIrules _tlIrules
         _lhsOvcount :: Int
         _lhsOvcount = rule420 _tlIvcount
         _hdOallnts = rule421 _lhsIallnts
         _hdOaroundMap = rule422 _lhsIaroundMap
         _hdOcVisitsMap = rule423 _lhsIcVisitsMap
         _hdOinh = rule424 _lhsIinh
         _hdOinhMap = rule425 _lhsIinhMap
         _hdOmanualAttrDepMap = rule426 _lhsImanualAttrDepMap
         _hdOmergeMap = rule427 _lhsImergeMap
         _hdOnt = rule428 _lhsInt
         _hdOo_case = rule429 _lhsIo_case
         _hdOo_cata = rule430 _lhsIo_cata
         _hdOo_dovisit = rule431 _lhsIo_dovisit
         _hdOo_newtypes = rule432 _lhsIo_newtypes
         _hdOo_rename = rule433 _lhsIo_rename
         _hdOo_sem = rule434 _lhsIo_sem
         _hdOo_sig = rule435 _lhsIo_sig
         _hdOo_unbox = rule436 _lhsIo_unbox
         _hdOo_wantvisit = rule437 _lhsIo_wantvisit
         _hdOoptions = rule438 _lhsIoptions
         _hdOprefix = rule439 _lhsIprefix
         _hdOsyn = rule440 _lhsIsyn
         _hdOsynMap = rule441 _lhsIsynMap
         _hdOvcount = rule442 _lhsIvcount
         _tlOallnts = rule443 _lhsIallnts
         _tlOaroundMap = rule444 _lhsIaroundMap
         _tlOcVisitsMap = rule445 _lhsIcVisitsMap
         _tlOinh = rule446 _lhsIinh
         _tlOinhMap = rule447 _lhsIinhMap
         _tlOmanualAttrDepMap = rule448 _lhsImanualAttrDepMap
         _tlOmergeMap = rule449 _lhsImergeMap
         _tlOnt = rule450 _lhsInt
         _tlOo_case = rule451 _lhsIo_case
         _tlOo_cata = rule452 _lhsIo_cata
         _tlOo_dovisit = rule453 _lhsIo_dovisit
         _tlOo_newtypes = rule454 _lhsIo_newtypes
         _tlOo_rename = rule455 _lhsIo_rename
         _tlOo_sem = rule456 _lhsIo_sem
         _tlOo_sig = rule457 _lhsIo_sig
         _tlOo_unbox = rule458 _lhsIo_unbox
         _tlOo_wantvisit = rule459 _lhsIo_wantvisit
         _tlOoptions = rule460 _lhsIoptions
         _tlOprefix = rule461 _lhsIprefix
         _tlOsyn = rule462 _lhsIsyn
         _tlOsynMap = rule463 _lhsIsynMap
         _tlOvcount = rule464 _hdIvcount
         __result_ = T_Productions_vOut28 _lhsOadditionalDep _lhsOaroundDep _lhsOcProductions _lhsOcons _lhsOdirectDep _lhsOerrors _lhsOinstDep _lhsOmergeDep _lhsOmirages _lhsOnAutoRules _lhsOnExplicitRules _lhsOrules _lhsOvcount
         in __result_ )
     in C_Productions_s29 v28
   {-# INLINE rule408 #-}
   {-# LINE 631 "src-ag/Order.ag" #-}
   rule408 = \ ((_hdIcProduction) :: CProduction) ((_tlIcProductions) :: CProductions) ->
                                {-# LINE 631 "src-ag/Order.ag" #-}
                                _hdIcProduction : _tlIcProductions
                                {-# LINE 3162 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule409 #-}
   {-# LINE 38 "src-ag/TfmToMirage.ag" #-}
   rule409 = \ ((_hdImirage) :: Mirage.Production) ((_tlImirages) :: [Mirage.Production]) ->
                         {-# LINE 38 "src-ag/TfmToMirage.ag" #-}
                         _hdImirage : _tlImirages
                         {-# LINE 3168 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule410 #-}
   rule410 = \ ((_hdIadditionalDep) :: Seq Edge) ((_tlIadditionalDep) :: Seq Edge) ->
     _hdIadditionalDep Seq.>< _tlIadditionalDep
   {-# INLINE rule411 #-}
   rule411 = \ ((_hdIaroundDep) :: Seq Edge) ((_tlIaroundDep) :: Seq Edge) ->
     _hdIaroundDep Seq.>< _tlIaroundDep
   {-# INLINE rule412 #-}
   rule412 = \ ((_hdIcons) :: [ConstructorIdent]) ((_tlIcons) :: [ConstructorIdent]) ->
     _hdIcons ++ _tlIcons
   {-# INLINE rule413 #-}
   rule413 = \ ((_hdIdirectDep) :: Seq Edge) ((_tlIdirectDep) :: Seq Edge) ->
     _hdIdirectDep Seq.>< _tlIdirectDep
   {-# INLINE rule414 #-}
   rule414 = \ ((_hdIerrors) :: Seq Error) ((_tlIerrors) :: Seq Error) ->
     _hdIerrors Seq.>< _tlIerrors
   {-# INLINE rule415 #-}
   rule415 = \ ((_hdIinstDep) :: Seq Edge) ((_tlIinstDep) :: Seq Edge) ->
     _hdIinstDep Seq.>< _tlIinstDep
   {-# INLINE rule416 #-}
   rule416 = \ ((_hdImergeDep) :: Seq Edge) ((_tlImergeDep) :: Seq Edge) ->
     _hdImergeDep Seq.>< _tlImergeDep
   {-# INLINE rule417 #-}
   rule417 = \ ((_hdInAutoRules) :: Int) ((_tlInAutoRules) :: Int) ->
     _hdInAutoRules + _tlInAutoRules
   {-# INLINE rule418 #-}
   rule418 = \ ((_hdInExplicitRules) :: Int) ((_tlInExplicitRules) :: Int) ->
     _hdInExplicitRules + _tlInExplicitRules
   {-# INLINE rule419 #-}
   rule419 = \ ((_hdIrules) :: Seq (Vertex,CRule)) ((_tlIrules) :: Seq (Vertex,CRule)) ->
     _hdIrules Seq.>< _tlIrules
   {-# INLINE rule420 #-}
   rule420 = \ ((_tlIvcount) :: Int) ->
     _tlIvcount
   {-# INLINE rule421 #-}
   rule421 = \ ((_lhsIallnts) :: [Identifier]) ->
     _lhsIallnts
   {-# INLINE rule422 #-}
   rule422 = \ ((_lhsIaroundMap) :: Map ConstructorIdent (Map Identifier [Expression])) ->
     _lhsIaroundMap
   {-# INLINE rule423 #-}
   rule423 = \ ((_lhsIcVisitsMap) :: CVisitsMap) ->
     _lhsIcVisitsMap
   {-# INLINE rule424 #-}
   rule424 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule425 #-}
   rule425 = \ ((_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# INLINE rule426 #-}
   rule426 = \ ((_lhsImanualAttrDepMap) :: AttrOrderMap) ->
     _lhsImanualAttrDepMap
   {-# INLINE rule427 #-}
   rule427 = \ ((_lhsImergeMap) :: Map ConstructorIdent (Map Identifier (Identifier,[Identifier]))) ->
     _lhsImergeMap
   {-# INLINE rule428 #-}
   rule428 = \ ((_lhsInt) :: Identifier) ->
     _lhsInt
   {-# INLINE rule429 #-}
   rule429 = \ ((_lhsIo_case) :: Bool) ->
     _lhsIo_case
   {-# INLINE rule430 #-}
   rule430 = \ ((_lhsIo_cata) :: Bool) ->
     _lhsIo_cata
   {-# INLINE rule431 #-}
   rule431 = \ ((_lhsIo_dovisit) :: Bool) ->
     _lhsIo_dovisit
   {-# INLINE rule432 #-}
   rule432 = \ ((_lhsIo_newtypes) :: Bool) ->
     _lhsIo_newtypes
   {-# INLINE rule433 #-}
   rule433 = \ ((_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# INLINE rule434 #-}
   rule434 = \ ((_lhsIo_sem) :: Bool) ->
     _lhsIo_sem
   {-# INLINE rule435 #-}
   rule435 = \ ((_lhsIo_sig) :: Bool) ->
     _lhsIo_sig
   {-# INLINE rule436 #-}
   rule436 = \ ((_lhsIo_unbox) :: Bool) ->
     _lhsIo_unbox
   {-# INLINE rule437 #-}
   rule437 = \ ((_lhsIo_wantvisit) :: Bool) ->
     _lhsIo_wantvisit
   {-# INLINE rule438 #-}
   rule438 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule439 #-}
   rule439 = \ ((_lhsIprefix) :: String) ->
     _lhsIprefix
   {-# INLINE rule440 #-}
   rule440 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
   {-# INLINE rule441 #-}
   rule441 = \ ((_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
   {-# INLINE rule442 #-}
   rule442 = \ ((_lhsIvcount) :: Int) ->
     _lhsIvcount
   {-# INLINE rule443 #-}
   rule443 = \ ((_lhsIallnts) :: [Identifier]) ->
     _lhsIallnts
   {-# INLINE rule444 #-}
   rule444 = \ ((_lhsIaroundMap) :: Map ConstructorIdent (Map Identifier [Expression])) ->
     _lhsIaroundMap
   {-# INLINE rule445 #-}
   rule445 = \ ((_lhsIcVisitsMap) :: CVisitsMap) ->
     _lhsIcVisitsMap
   {-# INLINE rule446 #-}
   rule446 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule447 #-}
   rule447 = \ ((_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# INLINE rule448 #-}
   rule448 = \ ((_lhsImanualAttrDepMap) :: AttrOrderMap) ->
     _lhsImanualAttrDepMap
   {-# INLINE rule449 #-}
   rule449 = \ ((_lhsImergeMap) :: Map ConstructorIdent (Map Identifier (Identifier,[Identifier]))) ->
     _lhsImergeMap
   {-# INLINE rule450 #-}
   rule450 = \ ((_lhsInt) :: Identifier) ->
     _lhsInt
   {-# INLINE rule451 #-}
   rule451 = \ ((_lhsIo_case) :: Bool) ->
     _lhsIo_case
   {-# INLINE rule452 #-}
   rule452 = \ ((_lhsIo_cata) :: Bool) ->
     _lhsIo_cata
   {-# INLINE rule453 #-}
   rule453 = \ ((_lhsIo_dovisit) :: Bool) ->
     _lhsIo_dovisit
   {-# INLINE rule454 #-}
   rule454 = \ ((_lhsIo_newtypes) :: Bool) ->
     _lhsIo_newtypes
   {-# INLINE rule455 #-}
   rule455 = \ ((_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# INLINE rule456 #-}
   rule456 = \ ((_lhsIo_sem) :: Bool) ->
     _lhsIo_sem
   {-# INLINE rule457 #-}
   rule457 = \ ((_lhsIo_sig) :: Bool) ->
     _lhsIo_sig
   {-# INLINE rule458 #-}
   rule458 = \ ((_lhsIo_unbox) :: Bool) ->
     _lhsIo_unbox
   {-# INLINE rule459 #-}
   rule459 = \ ((_lhsIo_wantvisit) :: Bool) ->
     _lhsIo_wantvisit
   {-# INLINE rule460 #-}
   rule460 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule461 #-}
   rule461 = \ ((_lhsIprefix) :: String) ->
     _lhsIprefix
   {-# INLINE rule462 #-}
   rule462 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
   {-# INLINE rule463 #-}
   rule463 = \ ((_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
   {-# INLINE rule464 #-}
   rule464 = \ ((_hdIvcount) :: Int) ->
     _hdIvcount
{-# NOINLINE sem_Productions_Nil #-}
sem_Productions_Nil ::  T_Productions 
sem_Productions_Nil  = T_Productions (return st29) where
   {-# NOINLINE st29 #-}
   st29 = let
      v28 :: T_Productions_v28 
      v28 = \ (T_Productions_vIn28 _lhsIallnts _lhsIaroundMap _lhsIcVisitsMap _lhsIinh _lhsIinhMap _lhsImanualAttrDepMap _lhsImergeMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_unbox _lhsIo_wantvisit _lhsIoptions _lhsIprefix _lhsIsyn _lhsIsynMap _lhsIvcount) -> ( let
         _lhsOcProductions :: CProductions
         _lhsOcProductions = rule465  ()
         _lhsOmirages :: [Mirage.Production]
         _lhsOmirages = rule466  ()
         _lhsOadditionalDep :: Seq Edge
         _lhsOadditionalDep = rule467  ()
         _lhsOaroundDep :: Seq Edge
         _lhsOaroundDep = rule468  ()
         _lhsOcons :: [ConstructorIdent]
         _lhsOcons = rule469  ()
         _lhsOdirectDep :: Seq Edge
         _lhsOdirectDep = rule470  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule471  ()
         _lhsOinstDep :: Seq Edge
         _lhsOinstDep = rule472  ()
         _lhsOmergeDep :: Seq Edge
         _lhsOmergeDep = rule473  ()
         _lhsOnAutoRules :: Int
         _lhsOnAutoRules = rule474  ()
         _lhsOnExplicitRules :: Int
         _lhsOnExplicitRules = rule475  ()
         _lhsOrules :: Seq (Vertex,CRule)
         _lhsOrules = rule476  ()
         _lhsOvcount :: Int
         _lhsOvcount = rule477 _lhsIvcount
         __result_ = T_Productions_vOut28 _lhsOadditionalDep _lhsOaroundDep _lhsOcProductions _lhsOcons _lhsOdirectDep _lhsOerrors _lhsOinstDep _lhsOmergeDep _lhsOmirages _lhsOnAutoRules _lhsOnExplicitRules _lhsOrules _lhsOvcount
         in __result_ )
     in C_Productions_s29 v28
   {-# INLINE rule465 #-}
   {-# LINE 632 "src-ag/Order.ag" #-}
   rule465 = \  (_ :: ()) ->
                                {-# LINE 632 "src-ag/Order.ag" #-}
                                []
                                {-# LINE 3375 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule466 #-}
   {-# LINE 39 "src-ag/TfmToMirage.ag" #-}
   rule466 = \  (_ :: ()) ->
                         {-# LINE 39 "src-ag/TfmToMirage.ag" #-}
                         []
                         {-# LINE 3381 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule467 #-}
   rule467 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule468 #-}
   rule468 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule469 #-}
   rule469 = \  (_ :: ()) ->
     []
   {-# INLINE rule470 #-}
   rule470 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule471 #-}
   rule471 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule472 #-}
   rule472 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule473 #-}
   rule473 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule474 #-}
   rule474 = \  (_ :: ()) ->
     0
   {-# INLINE rule475 #-}
   rule475 = \  (_ :: ()) ->
     0
   {-# INLINE rule476 #-}
   rule476 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule477 #-}
   rule477 = \ ((_lhsIvcount) :: Int) ->
     _lhsIvcount

-- Rule --------------------------------------------------------
-- wrapper
data Inh_Rule  = Inh_Rule { allTypeSigs_Inh_Rule :: (Map Identifier Type), allfields_Inh_Rule :: ([(Identifier,Type,ChildKind)]), allnts_Inh_Rule :: ([Identifier]), altAttrs_Inh_Rule :: (Map AltAttr Vertex), attrs_Inh_Rule :: ([(Identifier,Identifier)]), childInhs_Inh_Rule :: (Map Identifier Attributes), childNts_Inh_Rule :: (Map Identifier NontermIdent), con_Inh_Rule :: (Identifier), inh_Inh_Rule :: (Attributes), inhsOfChildren_Inh_Rule :: (Map Identifier Attributes), mergeMap_Inh_Rule :: (Map Identifier (Identifier,[Identifier])), nt_Inh_Rule :: (Identifier), o_case_Inh_Rule :: (Bool), o_cata_Inh_Rule :: (Bool), o_dovisit_Inh_Rule :: (Bool), o_newtypes_Inh_Rule :: (Bool), o_rename_Inh_Rule :: (Bool), o_sem_Inh_Rule :: (Bool), o_sig_Inh_Rule :: (Bool), o_wantvisit_Inh_Rule :: (Bool), options_Inh_Rule :: (Options), prefix_Inh_Rule :: (String), syn_Inh_Rule :: (Attributes), synsOfChildren_Inh_Rule :: (Map Identifier Attributes) }
data Syn_Rule  = Syn_Rule { directDep_Syn_Rule :: (Seq Edge), errors_Syn_Rule :: (Seq Error), gathAltAttrs_Syn_Rule :: ([AltAttr]), gathRules_Syn_Rule :: (Seq CRule), instDep_Syn_Rule :: (Seq Edge), instVars_Syn_Rule :: ([Identifier]), locVars_Syn_Rule :: ([Identifier]), mirage_Syn_Rule :: (Mirage.Rule), nAutoRules_Syn_Rule :: (Int), nExplicitRules_Syn_Rule :: (Int) }
{-# INLINABLE wrap_Rule #-}
wrap_Rule :: T_Rule  -> Inh_Rule  -> (Syn_Rule )
wrap_Rule (T_Rule act) (Inh_Rule _lhsIallTypeSigs _lhsIallfields _lhsIallnts _lhsIaltAttrs _lhsIattrs _lhsIchildInhs _lhsIchildNts _lhsIcon _lhsIinh _lhsIinhsOfChildren _lhsImergeMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_wantvisit _lhsIoptions _lhsIprefix _lhsIsyn _lhsIsynsOfChildren) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg31 = T_Rule_vIn31 _lhsIallTypeSigs _lhsIallfields _lhsIallnts _lhsIaltAttrs _lhsIattrs _lhsIchildInhs _lhsIchildNts _lhsIcon _lhsIinh _lhsIinhsOfChildren _lhsImergeMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_wantvisit _lhsIoptions _lhsIprefix _lhsIsyn _lhsIsynsOfChildren
        (T_Rule_vOut31 _lhsOdirectDep _lhsOerrors _lhsOgathAltAttrs _lhsOgathRules _lhsOinstDep _lhsOinstVars _lhsOlocVars _lhsOmirage _lhsOnAutoRules _lhsOnExplicitRules) <- return (inv_Rule_s32 sem arg31)
        return (Syn_Rule _lhsOdirectDep _lhsOerrors _lhsOgathAltAttrs _lhsOgathRules _lhsOinstDep _lhsOinstVars _lhsOlocVars _lhsOmirage _lhsOnAutoRules _lhsOnExplicitRules)
   )

-- cata
{-# INLINE sem_Rule #-}
sem_Rule :: Rule  -> T_Rule 
sem_Rule ( Rule mbName_ pattern_ rhs_ owrt_ origin_ explicit_ pure_ identity_ mbError_ eager_ ) = sem_Rule_Rule mbName_ ( sem_Pattern pattern_ ) ( sem_Expression rhs_ ) owrt_ origin_ explicit_ pure_ identity_ mbError_ eager_

-- semantic domain
newtype T_Rule  = T_Rule {
                         attach_T_Rule :: Identity (T_Rule_s32 )
                         }
newtype T_Rule_s32  = C_Rule_s32 {
                                 inv_Rule_s32 :: (T_Rule_v31 )
                                 }
data T_Rule_s33  = C_Rule_s33
type T_Rule_v31  = (T_Rule_vIn31 ) -> (T_Rule_vOut31 )
data T_Rule_vIn31  = T_Rule_vIn31 (Map Identifier Type) ([(Identifier,Type,ChildKind)]) ([Identifier]) (Map AltAttr Vertex) ([(Identifier,Identifier)]) (Map Identifier Attributes) (Map Identifier NontermIdent) (Identifier) (Attributes) (Map Identifier Attributes) (Map Identifier (Identifier,[Identifier])) (Identifier) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Options) (String) (Attributes) (Map Identifier Attributes)
data T_Rule_vOut31  = T_Rule_vOut31 (Seq Edge) (Seq Error) ([AltAttr]) (Seq CRule) (Seq Edge) ([Identifier]) ([Identifier]) (Mirage.Rule) (Int) (Int)
{-# NOINLINE sem_Rule_Rule #-}
sem_Rule_Rule :: (Maybe Identifier) -> T_Pattern  -> T_Expression  -> (Bool) -> (String) -> (Bool) -> (Bool) -> (Bool) -> (Maybe Error) -> (Bool) -> T_Rule 
sem_Rule_Rule arg_mbName_ arg_pattern_ arg_rhs_ arg_owrt_ arg_origin_ arg_explicit_ _ _ _ _ = T_Rule (return st32) where
   {-# NOINLINE st32 #-}
   st32 = let
      v31 :: T_Rule_v31 
      v31 = \ (T_Rule_vIn31 _lhsIallTypeSigs _lhsIallfields _lhsIallnts _lhsIaltAttrs _lhsIattrs _lhsIchildInhs _lhsIchildNts _lhsIcon _lhsIinh _lhsIinhsOfChildren _lhsImergeMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_wantvisit _lhsIoptions _lhsIprefix _lhsIsyn _lhsIsynsOfChildren) -> ( let
         _patternX20 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pattern_))
         _rhsX8 = Control.Monad.Identity.runIdentity (attach_T_Expression (arg_rhs_))
         (T_Pattern_vOut19 _patternIcopy _patternIerrors _patternIgathAltAttrs _patternIinstVars _patternIisUnderscore _patternIlocVars _patternIpatternAttrs _patternIpp) = inv_Pattern_s20 _patternX20 (T_Pattern_vIn19 _patternOallTypeSigs _patternOaltAttrs _patternObelowIrrefutable _patternOcon _patternOinh _patternOnt _patternOsyn)
         (T_Expression_vOut7 _rhsIallRhsVars _rhsIcopy _rhsIerrors _rhsIlns _rhsItextLines _rhsIusedAttrs _rhsIusedFields _rhsIusedLocals) = inv_Expression_s8 _rhsX8 (T_Expression_vIn7 _rhsOallfields _rhsOallnts _rhsOattrs _rhsOcon _rhsOmergeMap _rhsOnt _rhsOoptions)
         _lhsOnExplicitRules :: Int
         _lhsOnExplicitRules = rule478 arg_explicit_
         _lhsOnAutoRules :: Int
         _lhsOnAutoRules = rule479 arg_origin_
         _defines = rule480 _lhsIallTypeSigs _lhsIaltAttrs _lhsIchildInhs _lhsIsyn _patternIpatternAttrs
         _gathRules = rule481 _defines _lhsIchildNts _lhsIcon _lhsInt _patternIcopy _rhsIallRhsVars _rhsItextLines arg_explicit_ arg_mbName_ arg_origin_ arg_owrt_
         _lhsOdirectDep :: Seq Edge
         _lhsOdirectDep = rule482 _defines _lhsIaltAttrs _rhsIusedAttrs _rhsIusedFields _rhsIusedLocals
         _instDep1 = rule483 _defines _lhsIaltAttrs _lhsIsynsOfChildren
         _instDep2 = rule484 _defines _lhsIaltAttrs _lhsIinhsOfChildren
         _lhsOinstDep :: Seq Edge
         _lhsOinstDep = rule485 _instDep1 _instDep2
         _lhsOmirage :: Mirage.Rule
         _lhsOmirage = rule486 _patternIpatternAttrs _patternIpp _rhsIlns _rhsIusedAttrs _rhsIusedFields _rhsIusedLocals arg_explicit_ arg_origin_
         _patternObelowIrrefutable = rule487  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule488 _patternIerrors _rhsIerrors
         _lhsOgathAltAttrs :: [AltAttr]
         _lhsOgathAltAttrs = rule489 _patternIgathAltAttrs
         _lhsOgathRules :: Seq CRule
         _lhsOgathRules = rule490 _gathRules
         _lhsOinstVars :: [Identifier]
         _lhsOinstVars = rule491 _patternIinstVars
         _lhsOlocVars :: [Identifier]
         _lhsOlocVars = rule492 _patternIlocVars
         _patternOallTypeSigs = rule493 _lhsIallTypeSigs
         _patternOaltAttrs = rule494 _lhsIaltAttrs
         _patternOcon = rule495 _lhsIcon
         _patternOinh = rule496 _lhsIinh
         _patternOnt = rule497 _lhsInt
         _patternOsyn = rule498 _lhsIsyn
         _rhsOallfields = rule499 _lhsIallfields
         _rhsOallnts = rule500 _lhsIallnts
         _rhsOattrs = rule501 _lhsIattrs
         _rhsOcon = rule502 _lhsIcon
         _rhsOmergeMap = rule503 _lhsImergeMap
         _rhsOnt = rule504 _lhsInt
         _rhsOoptions = rule505 _lhsIoptions
         __result_ = T_Rule_vOut31 _lhsOdirectDep _lhsOerrors _lhsOgathAltAttrs _lhsOgathRules _lhsOinstDep _lhsOinstVars _lhsOlocVars _lhsOmirage _lhsOnAutoRules _lhsOnExplicitRules
         in __result_ )
     in C_Rule_s32 v31
   {-# INLINE rule478 #-}
   {-# LINE 64 "src-ag/Order.ag" #-}
   rule478 = \ explicit_ ->
                                 {-# LINE 64 "src-ag/Order.ag" #-}
                                 if explicit_
                                 then 1
                                 else 0
                                 {-# LINE 3505 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule479 #-}
   {-# LINE 67 "src-ag/Order.ag" #-}
   rule479 = \ origin_ ->
                             {-# LINE 67 "src-ag/Order.ag" #-}
                             if startsWith "use rule" origin_ || startsWith "copy rule" origin_
                             then 1
                             else 0
                             {-# LINE 3513 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule480 #-}
   {-# LINE 220 "src-ag/Order.ag" #-}
   rule480 = \ ((_lhsIallTypeSigs) :: Map Identifier Type) ((_lhsIaltAttrs) :: Map AltAttr Vertex) ((_lhsIchildInhs) :: Map Identifier Attributes) ((_lhsIsyn) :: Attributes) ((_patternIpatternAttrs) :: [(Identifier,Identifier,Bool)]) ->
                           {-# LINE 220 "src-ag/Order.ag" #-}
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
                           {-# LINE 3530 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule481 #-}
   {-# LINE 234 "src-ag/Order.ag" #-}
   rule481 = \ _defines ((_lhsIchildNts) :: Map Identifier NontermIdent) ((_lhsIcon) :: Identifier) ((_lhsInt) :: Identifier) ((_patternIcopy) :: Pattern) ((_rhsIallRhsVars) :: Set (Identifier,Identifier)) ((_rhsItextLines) :: [String]) explicit_ mbName_ origin_ owrt_ ->
                              {-# LINE 234 "src-ag/Order.ag" #-}
                              let childnt field = Map.lookup field _lhsIchildNts
                              in Seq.fromList [ CRule attr False True _lhsInt _lhsIcon field (childnt field) tp _patternIcopy _rhsItextLines _defines owrt_ origin_ _rhsIallRhsVars explicit_ mbName_
                                              | (field,attr,tp) <- Map.elems _defines
                                              ]
                              {-# LINE 3539 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule482 #-}
   {-# LINE 273 "src-ag/Order.ag" #-}
   rule482 = \ _defines ((_lhsIaltAttrs) :: Map AltAttr Vertex) ((_rhsIusedAttrs) :: [(Identifier,Identifier)]) ((_rhsIusedFields) :: [Identifier]) ((_rhsIusedLocals) :: [Identifier]) ->
                 {-# LINE 273 "src-ag/Order.ag" #-}
                 let  defined = Map.keys _defines
                      used =  [ Map.lookup (AltAttr field attr True) _lhsIaltAttrs | (field,attr) <- _rhsIusedAttrs]
                              ++ [ Map.lookup (AltAttr _LOC attr True) _lhsIaltAttrs | attr <- _rhsIusedLocals ++ _rhsIusedFields ]
                 in Seq.fromList [ (x,y) | Just x <- used, y <- defined ]
                 {-# LINE 3548 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule483 #-}
   {-# LINE 317 "src-ag/Order.ag" #-}
   rule483 = \ _defines ((_lhsIaltAttrs) :: Map AltAttr Vertex) ((_lhsIsynsOfChildren) :: Map Identifier Attributes) ->
            {-# LINE 317 "src-ag/Order.ag" #-}
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
            {-# LINE 3563 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule484 #-}
   {-# LINE 328 "src-ag/Order.ag" #-}
   rule484 = \ _defines ((_lhsIaltAttrs) :: Map AltAttr Vertex) ((_lhsIinhsOfChildren) :: Map Identifier Attributes) ->
            {-# LINE 328 "src-ag/Order.ag" #-}
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
            {-# LINE 3578 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule485 #-}
   {-# LINE 338 "src-ag/Order.ag" #-}
   rule485 = \ _instDep1 _instDep2 ->
                     {-# LINE 338 "src-ag/Order.ag" #-}
                     _instDep1     Seq.>< _instDep2
                     {-# LINE 3584 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule486 #-}
   {-# LINE 60 "src-ag/TfmToMirage.ag" #-}
   rule486 = \ ((_patternIpatternAttrs) :: [(Identifier,Identifier,Bool)]) ((_patternIpp) :: PP_Doc) ((_rhsIlns) :: [String]) ((_rhsIusedAttrs) :: [(Identifier,Identifier)]) ((_rhsIusedFields) :: [Identifier]) ((_rhsIusedLocals) :: [Identifier]) explicit_ origin_ ->
          {-# LINE 60 "src-ag/TfmToMirage.ag" #-}
          Mirage.Rule
            [Mirage.Address (getName field) (getName attr) | (field,attr,_) <- _patternIpatternAttrs]
            ([Mirage.Address (getName field) (getName attr) | (field,attr) <- _rhsIusedAttrs]
              ++ [ Mirage.Address (getName _LOC) (getName attr) | attr <- _rhsIusedLocals ++ _rhsIusedFields])
            explicit_
            origin_
            (disp (_patternIpp >-< indent 1 (text "= " >|< vlist _rhsIlns)) 0 "")
          {-# LINE 3596 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule487 #-}
   {-# LINE 98 "src-ag/TfmToMirage.ag" #-}
   rule487 = \  (_ :: ()) ->
                                      {-# LINE 98 "src-ag/TfmToMirage.ag" #-}
                                      False
                                      {-# LINE 3602 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule488 #-}
   rule488 = \ ((_patternIerrors) :: Seq Error) ((_rhsIerrors) :: Seq Error) ->
     _patternIerrors Seq.>< _rhsIerrors
   {-# INLINE rule489 #-}
   rule489 = \ ((_patternIgathAltAttrs) :: [AltAttr]) ->
     _patternIgathAltAttrs
   {-# INLINE rule490 #-}
   rule490 = \ _gathRules ->
     _gathRules
   {-# INLINE rule491 #-}
   rule491 = \ ((_patternIinstVars) :: [Identifier]) ->
     _patternIinstVars
   {-# INLINE rule492 #-}
   rule492 = \ ((_patternIlocVars) :: [Identifier]) ->
     _patternIlocVars
   {-# INLINE rule493 #-}
   rule493 = \ ((_lhsIallTypeSigs) :: Map Identifier Type) ->
     _lhsIallTypeSigs
   {-# INLINE rule494 #-}
   rule494 = \ ((_lhsIaltAttrs) :: Map AltAttr Vertex) ->
     _lhsIaltAttrs
   {-# INLINE rule495 #-}
   rule495 = \ ((_lhsIcon) :: Identifier) ->
     _lhsIcon
   {-# INLINE rule496 #-}
   rule496 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule497 #-}
   rule497 = \ ((_lhsInt) :: Identifier) ->
     _lhsInt
   {-# INLINE rule498 #-}
   rule498 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
   {-# INLINE rule499 #-}
   rule499 = \ ((_lhsIallfields) :: [(Identifier,Type,ChildKind)]) ->
     _lhsIallfields
   {-# INLINE rule500 #-}
   rule500 = \ ((_lhsIallnts) :: [Identifier]) ->
     _lhsIallnts
   {-# INLINE rule501 #-}
   rule501 = \ ((_lhsIattrs) :: [(Identifier,Identifier)]) ->
     _lhsIattrs
   {-# INLINE rule502 #-}
   rule502 = \ ((_lhsIcon) :: Identifier) ->
     _lhsIcon
   {-# INLINE rule503 #-}
   rule503 = \ ((_lhsImergeMap) :: Map Identifier (Identifier,[Identifier])) ->
     _lhsImergeMap
   {-# INLINE rule504 #-}
   rule504 = \ ((_lhsInt) :: Identifier) ->
     _lhsInt
   {-# INLINE rule505 #-}
   rule505 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions

-- Rules -------------------------------------------------------
-- wrapper
data Inh_Rules  = Inh_Rules { allTypeSigs_Inh_Rules :: (Map Identifier Type), allfields_Inh_Rules :: ([(Identifier,Type,ChildKind)]), allnts_Inh_Rules :: ([Identifier]), altAttrs_Inh_Rules :: (Map AltAttr Vertex), attrs_Inh_Rules :: ([(Identifier,Identifier)]), childInhs_Inh_Rules :: (Map Identifier Attributes), childNts_Inh_Rules :: (Map Identifier NontermIdent), con_Inh_Rules :: (Identifier), inh_Inh_Rules :: (Attributes), inhsOfChildren_Inh_Rules :: (Map Identifier Attributes), mergeMap_Inh_Rules :: (Map Identifier (Identifier,[Identifier])), nt_Inh_Rules :: (Identifier), o_case_Inh_Rules :: (Bool), o_cata_Inh_Rules :: (Bool), o_dovisit_Inh_Rules :: (Bool), o_newtypes_Inh_Rules :: (Bool), o_rename_Inh_Rules :: (Bool), o_sem_Inh_Rules :: (Bool), o_sig_Inh_Rules :: (Bool), o_wantvisit_Inh_Rules :: (Bool), options_Inh_Rules :: (Options), prefix_Inh_Rules :: (String), syn_Inh_Rules :: (Attributes), synsOfChildren_Inh_Rules :: (Map Identifier Attributes) }
data Syn_Rules  = Syn_Rules { directDep_Syn_Rules :: (Seq Edge), errors_Syn_Rules :: (Seq Error), gathAltAttrs_Syn_Rules :: ([AltAttr]), gathRules_Syn_Rules :: (Seq CRule), instDep_Syn_Rules :: (Seq Edge), instVars_Syn_Rules :: ([Identifier]), locVars_Syn_Rules :: ([Identifier]), mirages_Syn_Rules :: ([Mirage.Rule]), nAutoRules_Syn_Rules :: (Int), nExplicitRules_Syn_Rules :: (Int) }
{-# INLINABLE wrap_Rules #-}
wrap_Rules :: T_Rules  -> Inh_Rules  -> (Syn_Rules )
wrap_Rules (T_Rules act) (Inh_Rules _lhsIallTypeSigs _lhsIallfields _lhsIallnts _lhsIaltAttrs _lhsIattrs _lhsIchildInhs _lhsIchildNts _lhsIcon _lhsIinh _lhsIinhsOfChildren _lhsImergeMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_wantvisit _lhsIoptions _lhsIprefix _lhsIsyn _lhsIsynsOfChildren) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg34 = T_Rules_vIn34 _lhsIallTypeSigs _lhsIallfields _lhsIallnts _lhsIaltAttrs _lhsIattrs _lhsIchildInhs _lhsIchildNts _lhsIcon _lhsIinh _lhsIinhsOfChildren _lhsImergeMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_wantvisit _lhsIoptions _lhsIprefix _lhsIsyn _lhsIsynsOfChildren
        (T_Rules_vOut34 _lhsOdirectDep _lhsOerrors _lhsOgathAltAttrs _lhsOgathRules _lhsOinstDep _lhsOinstVars _lhsOlocVars _lhsOmirages _lhsOnAutoRules _lhsOnExplicitRules) <- return (inv_Rules_s35 sem arg34)
        return (Syn_Rules _lhsOdirectDep _lhsOerrors _lhsOgathAltAttrs _lhsOgathRules _lhsOinstDep _lhsOinstVars _lhsOlocVars _lhsOmirages _lhsOnAutoRules _lhsOnExplicitRules)
   )

-- cata
{-# NOINLINE sem_Rules #-}
sem_Rules :: Rules  -> T_Rules 
sem_Rules list = Prelude.foldr sem_Rules_Cons sem_Rules_Nil (Prelude.map sem_Rule list)

-- semantic domain
newtype T_Rules  = T_Rules {
                           attach_T_Rules :: Identity (T_Rules_s35 )
                           }
newtype T_Rules_s35  = C_Rules_s35 {
                                   inv_Rules_s35 :: (T_Rules_v34 )
                                   }
data T_Rules_s36  = C_Rules_s36
type T_Rules_v34  = (T_Rules_vIn34 ) -> (T_Rules_vOut34 )
data T_Rules_vIn34  = T_Rules_vIn34 (Map Identifier Type) ([(Identifier,Type,ChildKind)]) ([Identifier]) (Map AltAttr Vertex) ([(Identifier,Identifier)]) (Map Identifier Attributes) (Map Identifier NontermIdent) (Identifier) (Attributes) (Map Identifier Attributes) (Map Identifier (Identifier,[Identifier])) (Identifier) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Options) (String) (Attributes) (Map Identifier Attributes)
data T_Rules_vOut34  = T_Rules_vOut34 (Seq Edge) (Seq Error) ([AltAttr]) (Seq CRule) (Seq Edge) ([Identifier]) ([Identifier]) ([Mirage.Rule]) (Int) (Int)
{-# NOINLINE sem_Rules_Cons #-}
sem_Rules_Cons :: T_Rule  -> T_Rules  -> T_Rules 
sem_Rules_Cons arg_hd_ arg_tl_ = T_Rules (return st35) where
   {-# NOINLINE st35 #-}
   st35 = let
      v34 :: T_Rules_v34 
      v34 = \ (T_Rules_vIn34 _lhsIallTypeSigs _lhsIallfields _lhsIallnts _lhsIaltAttrs _lhsIattrs _lhsIchildInhs _lhsIchildNts _lhsIcon _lhsIinh _lhsIinhsOfChildren _lhsImergeMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_wantvisit _lhsIoptions _lhsIprefix _lhsIsyn _lhsIsynsOfChildren) -> ( let
         _hdX32 = Control.Monad.Identity.runIdentity (attach_T_Rule (arg_hd_))
         _tlX35 = Control.Monad.Identity.runIdentity (attach_T_Rules (arg_tl_))
         (T_Rule_vOut31 _hdIdirectDep _hdIerrors _hdIgathAltAttrs _hdIgathRules _hdIinstDep _hdIinstVars _hdIlocVars _hdImirage _hdInAutoRules _hdInExplicitRules) = inv_Rule_s32 _hdX32 (T_Rule_vIn31 _hdOallTypeSigs _hdOallfields _hdOallnts _hdOaltAttrs _hdOattrs _hdOchildInhs _hdOchildNts _hdOcon _hdOinh _hdOinhsOfChildren _hdOmergeMap _hdOnt _hdOo_case _hdOo_cata _hdOo_dovisit _hdOo_newtypes _hdOo_rename _hdOo_sem _hdOo_sig _hdOo_wantvisit _hdOoptions _hdOprefix _hdOsyn _hdOsynsOfChildren)
         (T_Rules_vOut34 _tlIdirectDep _tlIerrors _tlIgathAltAttrs _tlIgathRules _tlIinstDep _tlIinstVars _tlIlocVars _tlImirages _tlInAutoRules _tlInExplicitRules) = inv_Rules_s35 _tlX35 (T_Rules_vIn34 _tlOallTypeSigs _tlOallfields _tlOallnts _tlOaltAttrs _tlOattrs _tlOchildInhs _tlOchildNts _tlOcon _tlOinh _tlOinhsOfChildren _tlOmergeMap _tlOnt _tlOo_case _tlOo_cata _tlOo_dovisit _tlOo_newtypes _tlOo_rename _tlOo_sem _tlOo_sig _tlOo_wantvisit _tlOoptions _tlOprefix _tlOsyn _tlOsynsOfChildren)
         _lhsOmirages :: [Mirage.Rule]
         _lhsOmirages = rule506 _hdImirage _tlImirages
         _lhsOdirectDep :: Seq Edge
         _lhsOdirectDep = rule507 _hdIdirectDep _tlIdirectDep
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule508 _hdIerrors _tlIerrors
         _lhsOgathAltAttrs :: [AltAttr]
         _lhsOgathAltAttrs = rule509 _hdIgathAltAttrs _tlIgathAltAttrs
         _lhsOgathRules :: Seq CRule
         _lhsOgathRules = rule510 _hdIgathRules _tlIgathRules
         _lhsOinstDep :: Seq Edge
         _lhsOinstDep = rule511 _hdIinstDep _tlIinstDep
         _lhsOinstVars :: [Identifier]
         _lhsOinstVars = rule512 _hdIinstVars _tlIinstVars
         _lhsOlocVars :: [Identifier]
         _lhsOlocVars = rule513 _hdIlocVars _tlIlocVars
         _lhsOnAutoRules :: Int
         _lhsOnAutoRules = rule514 _hdInAutoRules _tlInAutoRules
         _lhsOnExplicitRules :: Int
         _lhsOnExplicitRules = rule515 _hdInExplicitRules _tlInExplicitRules
         _hdOallTypeSigs = rule516 _lhsIallTypeSigs
         _hdOallfields = rule517 _lhsIallfields
         _hdOallnts = rule518 _lhsIallnts
         _hdOaltAttrs = rule519 _lhsIaltAttrs
         _hdOattrs = rule520 _lhsIattrs
         _hdOchildInhs = rule521 _lhsIchildInhs
         _hdOchildNts = rule522 _lhsIchildNts
         _hdOcon = rule523 _lhsIcon
         _hdOinh = rule524 _lhsIinh
         _hdOinhsOfChildren = rule525 _lhsIinhsOfChildren
         _hdOmergeMap = rule526 _lhsImergeMap
         _hdOnt = rule527 _lhsInt
         _hdOo_case = rule528 _lhsIo_case
         _hdOo_cata = rule529 _lhsIo_cata
         _hdOo_dovisit = rule530 _lhsIo_dovisit
         _hdOo_newtypes = rule531 _lhsIo_newtypes
         _hdOo_rename = rule532 _lhsIo_rename
         _hdOo_sem = rule533 _lhsIo_sem
         _hdOo_sig = rule534 _lhsIo_sig
         _hdOo_wantvisit = rule535 _lhsIo_wantvisit
         _hdOoptions = rule536 _lhsIoptions
         _hdOprefix = rule537 _lhsIprefix
         _hdOsyn = rule538 _lhsIsyn
         _hdOsynsOfChildren = rule539 _lhsIsynsOfChildren
         _tlOallTypeSigs = rule540 _lhsIallTypeSigs
         _tlOallfields = rule541 _lhsIallfields
         _tlOallnts = rule542 _lhsIallnts
         _tlOaltAttrs = rule543 _lhsIaltAttrs
         _tlOattrs = rule544 _lhsIattrs
         _tlOchildInhs = rule545 _lhsIchildInhs
         _tlOchildNts = rule546 _lhsIchildNts
         _tlOcon = rule547 _lhsIcon
         _tlOinh = rule548 _lhsIinh
         _tlOinhsOfChildren = rule549 _lhsIinhsOfChildren
         _tlOmergeMap = rule550 _lhsImergeMap
         _tlOnt = rule551 _lhsInt
         _tlOo_case = rule552 _lhsIo_case
         _tlOo_cata = rule553 _lhsIo_cata
         _tlOo_dovisit = rule554 _lhsIo_dovisit
         _tlOo_newtypes = rule555 _lhsIo_newtypes
         _tlOo_rename = rule556 _lhsIo_rename
         _tlOo_sem = rule557 _lhsIo_sem
         _tlOo_sig = rule558 _lhsIo_sig
         _tlOo_wantvisit = rule559 _lhsIo_wantvisit
         _tlOoptions = rule560 _lhsIoptions
         _tlOprefix = rule561 _lhsIprefix
         _tlOsyn = rule562 _lhsIsyn
         _tlOsynsOfChildren = rule563 _lhsIsynsOfChildren
         __result_ = T_Rules_vOut34 _lhsOdirectDep _lhsOerrors _lhsOgathAltAttrs _lhsOgathRules _lhsOinstDep _lhsOinstVars _lhsOlocVars _lhsOmirages _lhsOnAutoRules _lhsOnExplicitRules
         in __result_ )
     in C_Rules_s35 v34
   {-# INLINE rule506 #-}
   {-# LINE 53 "src-ag/TfmToMirage.ag" #-}
   rule506 = \ ((_hdImirage) :: Mirage.Rule) ((_tlImirages) :: [Mirage.Rule]) ->
                         {-# LINE 53 "src-ag/TfmToMirage.ag" #-}
                         _hdImirage : _tlImirages
                         {-# LINE 3775 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule507 #-}
   rule507 = \ ((_hdIdirectDep) :: Seq Edge) ((_tlIdirectDep) :: Seq Edge) ->
     _hdIdirectDep Seq.>< _tlIdirectDep
   {-# INLINE rule508 #-}
   rule508 = \ ((_hdIerrors) :: Seq Error) ((_tlIerrors) :: Seq Error) ->
     _hdIerrors Seq.>< _tlIerrors
   {-# INLINE rule509 #-}
   rule509 = \ ((_hdIgathAltAttrs) :: [AltAttr]) ((_tlIgathAltAttrs) :: [AltAttr]) ->
     _hdIgathAltAttrs ++ _tlIgathAltAttrs
   {-# INLINE rule510 #-}
   rule510 = \ ((_hdIgathRules) :: Seq CRule) ((_tlIgathRules) :: Seq CRule) ->
     _hdIgathRules Seq.>< _tlIgathRules
   {-# INLINE rule511 #-}
   rule511 = \ ((_hdIinstDep) :: Seq Edge) ((_tlIinstDep) :: Seq Edge) ->
     _hdIinstDep Seq.>< _tlIinstDep
   {-# INLINE rule512 #-}
   rule512 = \ ((_hdIinstVars) :: [Identifier]) ((_tlIinstVars) :: [Identifier]) ->
     _hdIinstVars ++ _tlIinstVars
   {-# INLINE rule513 #-}
   rule513 = \ ((_hdIlocVars) :: [Identifier]) ((_tlIlocVars) :: [Identifier]) ->
     _hdIlocVars ++ _tlIlocVars
   {-# INLINE rule514 #-}
   rule514 = \ ((_hdInAutoRules) :: Int) ((_tlInAutoRules) :: Int) ->
     _hdInAutoRules + _tlInAutoRules
   {-# INLINE rule515 #-}
   rule515 = \ ((_hdInExplicitRules) :: Int) ((_tlInExplicitRules) :: Int) ->
     _hdInExplicitRules + _tlInExplicitRules
   {-# INLINE rule516 #-}
   rule516 = \ ((_lhsIallTypeSigs) :: Map Identifier Type) ->
     _lhsIallTypeSigs
   {-# INLINE rule517 #-}
   rule517 = \ ((_lhsIallfields) :: [(Identifier,Type,ChildKind)]) ->
     _lhsIallfields
   {-# INLINE rule518 #-}
   rule518 = \ ((_lhsIallnts) :: [Identifier]) ->
     _lhsIallnts
   {-# INLINE rule519 #-}
   rule519 = \ ((_lhsIaltAttrs) :: Map AltAttr Vertex) ->
     _lhsIaltAttrs
   {-# INLINE rule520 #-}
   rule520 = \ ((_lhsIattrs) :: [(Identifier,Identifier)]) ->
     _lhsIattrs
   {-# INLINE rule521 #-}
   rule521 = \ ((_lhsIchildInhs) :: Map Identifier Attributes) ->
     _lhsIchildInhs
   {-# INLINE rule522 #-}
   rule522 = \ ((_lhsIchildNts) :: Map Identifier NontermIdent) ->
     _lhsIchildNts
   {-# INLINE rule523 #-}
   rule523 = \ ((_lhsIcon) :: Identifier) ->
     _lhsIcon
   {-# INLINE rule524 #-}
   rule524 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule525 #-}
   rule525 = \ ((_lhsIinhsOfChildren) :: Map Identifier Attributes) ->
     _lhsIinhsOfChildren
   {-# INLINE rule526 #-}
   rule526 = \ ((_lhsImergeMap) :: Map Identifier (Identifier,[Identifier])) ->
     _lhsImergeMap
   {-# INLINE rule527 #-}
   rule527 = \ ((_lhsInt) :: Identifier) ->
     _lhsInt
   {-# INLINE rule528 #-}
   rule528 = \ ((_lhsIo_case) :: Bool) ->
     _lhsIo_case
   {-# INLINE rule529 #-}
   rule529 = \ ((_lhsIo_cata) :: Bool) ->
     _lhsIo_cata
   {-# INLINE rule530 #-}
   rule530 = \ ((_lhsIo_dovisit) :: Bool) ->
     _lhsIo_dovisit
   {-# INLINE rule531 #-}
   rule531 = \ ((_lhsIo_newtypes) :: Bool) ->
     _lhsIo_newtypes
   {-# INLINE rule532 #-}
   rule532 = \ ((_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# INLINE rule533 #-}
   rule533 = \ ((_lhsIo_sem) :: Bool) ->
     _lhsIo_sem
   {-# INLINE rule534 #-}
   rule534 = \ ((_lhsIo_sig) :: Bool) ->
     _lhsIo_sig
   {-# INLINE rule535 #-}
   rule535 = \ ((_lhsIo_wantvisit) :: Bool) ->
     _lhsIo_wantvisit
   {-# INLINE rule536 #-}
   rule536 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule537 #-}
   rule537 = \ ((_lhsIprefix) :: String) ->
     _lhsIprefix
   {-# INLINE rule538 #-}
   rule538 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
   {-# INLINE rule539 #-}
   rule539 = \ ((_lhsIsynsOfChildren) :: Map Identifier Attributes) ->
     _lhsIsynsOfChildren
   {-# INLINE rule540 #-}
   rule540 = \ ((_lhsIallTypeSigs) :: Map Identifier Type) ->
     _lhsIallTypeSigs
   {-# INLINE rule541 #-}
   rule541 = \ ((_lhsIallfields) :: [(Identifier,Type,ChildKind)]) ->
     _lhsIallfields
   {-# INLINE rule542 #-}
   rule542 = \ ((_lhsIallnts) :: [Identifier]) ->
     _lhsIallnts
   {-# INLINE rule543 #-}
   rule543 = \ ((_lhsIaltAttrs) :: Map AltAttr Vertex) ->
     _lhsIaltAttrs
   {-# INLINE rule544 #-}
   rule544 = \ ((_lhsIattrs) :: [(Identifier,Identifier)]) ->
     _lhsIattrs
   {-# INLINE rule545 #-}
   rule545 = \ ((_lhsIchildInhs) :: Map Identifier Attributes) ->
     _lhsIchildInhs
   {-# INLINE rule546 #-}
   rule546 = \ ((_lhsIchildNts) :: Map Identifier NontermIdent) ->
     _lhsIchildNts
   {-# INLINE rule547 #-}
   rule547 = \ ((_lhsIcon) :: Identifier) ->
     _lhsIcon
   {-# INLINE rule548 #-}
   rule548 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule549 #-}
   rule549 = \ ((_lhsIinhsOfChildren) :: Map Identifier Attributes) ->
     _lhsIinhsOfChildren
   {-# INLINE rule550 #-}
   rule550 = \ ((_lhsImergeMap) :: Map Identifier (Identifier,[Identifier])) ->
     _lhsImergeMap
   {-# INLINE rule551 #-}
   rule551 = \ ((_lhsInt) :: Identifier) ->
     _lhsInt
   {-# INLINE rule552 #-}
   rule552 = \ ((_lhsIo_case) :: Bool) ->
     _lhsIo_case
   {-# INLINE rule553 #-}
   rule553 = \ ((_lhsIo_cata) :: Bool) ->
     _lhsIo_cata
   {-# INLINE rule554 #-}
   rule554 = \ ((_lhsIo_dovisit) :: Bool) ->
     _lhsIo_dovisit
   {-# INLINE rule555 #-}
   rule555 = \ ((_lhsIo_newtypes) :: Bool) ->
     _lhsIo_newtypes
   {-# INLINE rule556 #-}
   rule556 = \ ((_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# INLINE rule557 #-}
   rule557 = \ ((_lhsIo_sem) :: Bool) ->
     _lhsIo_sem
   {-# INLINE rule558 #-}
   rule558 = \ ((_lhsIo_sig) :: Bool) ->
     _lhsIo_sig
   {-# INLINE rule559 #-}
   rule559 = \ ((_lhsIo_wantvisit) :: Bool) ->
     _lhsIo_wantvisit
   {-# INLINE rule560 #-}
   rule560 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule561 #-}
   rule561 = \ ((_lhsIprefix) :: String) ->
     _lhsIprefix
   {-# INLINE rule562 #-}
   rule562 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
   {-# INLINE rule563 #-}
   rule563 = \ ((_lhsIsynsOfChildren) :: Map Identifier Attributes) ->
     _lhsIsynsOfChildren
{-# NOINLINE sem_Rules_Nil #-}
sem_Rules_Nil ::  T_Rules 
sem_Rules_Nil  = T_Rules (return st35) where
   {-# NOINLINE st35 #-}
   st35 = let
      v34 :: T_Rules_v34 
      v34 = \ (T_Rules_vIn34 _lhsIallTypeSigs _lhsIallfields _lhsIallnts _lhsIaltAttrs _lhsIattrs _lhsIchildInhs _lhsIchildNts _lhsIcon _lhsIinh _lhsIinhsOfChildren _lhsImergeMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_wantvisit _lhsIoptions _lhsIprefix _lhsIsyn _lhsIsynsOfChildren) -> ( let
         _lhsOmirages :: [Mirage.Rule]
         _lhsOmirages = rule564  ()
         _lhsOdirectDep :: Seq Edge
         _lhsOdirectDep = rule565  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule566  ()
         _lhsOgathAltAttrs :: [AltAttr]
         _lhsOgathAltAttrs = rule567  ()
         _lhsOgathRules :: Seq CRule
         _lhsOgathRules = rule568  ()
         _lhsOinstDep :: Seq Edge
         _lhsOinstDep = rule569  ()
         _lhsOinstVars :: [Identifier]
         _lhsOinstVars = rule570  ()
         _lhsOlocVars :: [Identifier]
         _lhsOlocVars = rule571  ()
         _lhsOnAutoRules :: Int
         _lhsOnAutoRules = rule572  ()
         _lhsOnExplicitRules :: Int
         _lhsOnExplicitRules = rule573  ()
         __result_ = T_Rules_vOut34 _lhsOdirectDep _lhsOerrors _lhsOgathAltAttrs _lhsOgathRules _lhsOinstDep _lhsOinstVars _lhsOlocVars _lhsOmirages _lhsOnAutoRules _lhsOnExplicitRules
         in __result_ )
     in C_Rules_s35 v34
   {-# INLINE rule564 #-}
   {-# LINE 54 "src-ag/TfmToMirage.ag" #-}
   rule564 = \  (_ :: ()) ->
                         {-# LINE 54 "src-ag/TfmToMirage.ag" #-}
                         []
                         {-# LINE 3982 "src-generated/TfmToMirage.hs" #-}
   {-# INLINE rule565 #-}
   rule565 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule566 #-}
   rule566 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule567 #-}
   rule567 = \  (_ :: ()) ->
     []
   {-# INLINE rule568 #-}
   rule568 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule569 #-}
   rule569 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule570 #-}
   rule570 = \  (_ :: ()) ->
     []
   {-# INLINE rule571 #-}
   rule571 = \  (_ :: ()) ->
     []
   {-# INLINE rule572 #-}
   rule572 = \  (_ :: ()) ->
     0
   {-# INLINE rule573 #-}
   rule573 = \  (_ :: ()) ->
     0

-- TypeSig -----------------------------------------------------
-- wrapper
data Inh_TypeSig  = Inh_TypeSig { typeSigs_Inh_TypeSig :: (Map Identifier Type) }
data Syn_TypeSig  = Syn_TypeSig { typeSigs_Syn_TypeSig :: (Map Identifier Type) }
{-# INLINABLE wrap_TypeSig #-}
wrap_TypeSig :: T_TypeSig  -> Inh_TypeSig  -> (Syn_TypeSig )
wrap_TypeSig (T_TypeSig act) (Inh_TypeSig _lhsItypeSigs) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg37 = T_TypeSig_vIn37 _lhsItypeSigs
        (T_TypeSig_vOut37 _lhsOtypeSigs) <- return (inv_TypeSig_s38 sem arg37)
        return (Syn_TypeSig _lhsOtypeSigs)
   )

-- cata
{-# INLINE sem_TypeSig #-}
sem_TypeSig :: TypeSig  -> T_TypeSig 
sem_TypeSig ( TypeSig name_ tp_ ) = sem_TypeSig_TypeSig name_ tp_

-- semantic domain
newtype T_TypeSig  = T_TypeSig {
                               attach_T_TypeSig :: Identity (T_TypeSig_s38 )
                               }
newtype T_TypeSig_s38  = C_TypeSig_s38 {
                                       inv_TypeSig_s38 :: (T_TypeSig_v37 )
                                       }
data T_TypeSig_s39  = C_TypeSig_s39
type T_TypeSig_v37  = (T_TypeSig_vIn37 ) -> (T_TypeSig_vOut37 )
data T_TypeSig_vIn37  = T_TypeSig_vIn37 (Map Identifier Type)
data T_TypeSig_vOut37  = T_TypeSig_vOut37 (Map Identifier Type)
{-# NOINLINE sem_TypeSig_TypeSig #-}
sem_TypeSig_TypeSig :: (Identifier) -> (Type) -> T_TypeSig 
sem_TypeSig_TypeSig arg_name_ arg_tp_ = T_TypeSig (return st38) where
   {-# NOINLINE st38 #-}
   st38 = let
      v37 :: T_TypeSig_v37 
      v37 = \ (T_TypeSig_vIn37 _lhsItypeSigs) -> ( let
         _lhsOtypeSigs :: Map Identifier Type
         _lhsOtypeSigs = rule574 _lhsItypeSigs arg_name_ arg_tp_
         __result_ = T_TypeSig_vOut37 _lhsOtypeSigs
         in __result_ )
     in C_TypeSig_s38 v37
   {-# INLINE rule574 #-}
   {-# LINE 536 "src-ag/Order.ag" #-}
   rule574 = \ ((_lhsItypeSigs) :: Map Identifier Type) name_ tp_ ->
                             {-# LINE 536 "src-ag/Order.ag" #-}
                             Map.insert name_ tp_ _lhsItypeSigs
                             {-# LINE 4058 "src-generated/TfmToMirage.hs" #-}

-- TypeSigs ----------------------------------------------------
-- wrapper
data Inh_TypeSigs  = Inh_TypeSigs { typeSigs_Inh_TypeSigs :: (Map Identifier Type) }
data Syn_TypeSigs  = Syn_TypeSigs { typeSigs_Syn_TypeSigs :: (Map Identifier Type) }
{-# INLINABLE wrap_TypeSigs #-}
wrap_TypeSigs :: T_TypeSigs  -> Inh_TypeSigs  -> (Syn_TypeSigs )
wrap_TypeSigs (T_TypeSigs act) (Inh_TypeSigs _lhsItypeSigs) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg40 = T_TypeSigs_vIn40 _lhsItypeSigs
        (T_TypeSigs_vOut40 _lhsOtypeSigs) <- return (inv_TypeSigs_s41 sem arg40)
        return (Syn_TypeSigs _lhsOtypeSigs)
   )

-- cata
{-# NOINLINE sem_TypeSigs #-}
sem_TypeSigs :: TypeSigs  -> T_TypeSigs 
sem_TypeSigs list = Prelude.foldr sem_TypeSigs_Cons sem_TypeSigs_Nil (Prelude.map sem_TypeSig list)

-- semantic domain
newtype T_TypeSigs  = T_TypeSigs {
                                 attach_T_TypeSigs :: Identity (T_TypeSigs_s41 )
                                 }
newtype T_TypeSigs_s41  = C_TypeSigs_s41 {
                                         inv_TypeSigs_s41 :: (T_TypeSigs_v40 )
                                         }
data T_TypeSigs_s42  = C_TypeSigs_s42
type T_TypeSigs_v40  = (T_TypeSigs_vIn40 ) -> (T_TypeSigs_vOut40 )
data T_TypeSigs_vIn40  = T_TypeSigs_vIn40 (Map Identifier Type)
data T_TypeSigs_vOut40  = T_TypeSigs_vOut40 (Map Identifier Type)
{-# NOINLINE sem_TypeSigs_Cons #-}
sem_TypeSigs_Cons :: T_TypeSig  -> T_TypeSigs  -> T_TypeSigs 
sem_TypeSigs_Cons arg_hd_ arg_tl_ = T_TypeSigs (return st41) where
   {-# NOINLINE st41 #-}
   st41 = let
      v40 :: T_TypeSigs_v40 
      v40 = \ (T_TypeSigs_vIn40 _lhsItypeSigs) -> ( let
         _hdX38 = Control.Monad.Identity.runIdentity (attach_T_TypeSig (arg_hd_))
         _tlX41 = Control.Monad.Identity.runIdentity (attach_T_TypeSigs (arg_tl_))
         (T_TypeSig_vOut37 _hdItypeSigs) = inv_TypeSig_s38 _hdX38 (T_TypeSig_vIn37 _hdOtypeSigs)
         (T_TypeSigs_vOut40 _tlItypeSigs) = inv_TypeSigs_s41 _tlX41 (T_TypeSigs_vIn40 _tlOtypeSigs)
         _lhsOtypeSigs :: Map Identifier Type
         _lhsOtypeSigs = rule575 _tlItypeSigs
         _hdOtypeSigs = rule576 _lhsItypeSigs
         _tlOtypeSigs = rule577 _hdItypeSigs
         __result_ = T_TypeSigs_vOut40 _lhsOtypeSigs
         in __result_ )
     in C_TypeSigs_s41 v40
   {-# INLINE rule575 #-}
   rule575 = \ ((_tlItypeSigs) :: Map Identifier Type) ->
     _tlItypeSigs
   {-# INLINE rule576 #-}
   rule576 = \ ((_lhsItypeSigs) :: Map Identifier Type) ->
     _lhsItypeSigs
   {-# INLINE rule577 #-}
   rule577 = \ ((_hdItypeSigs) :: Map Identifier Type) ->
     _hdItypeSigs
{-# NOINLINE sem_TypeSigs_Nil #-}
sem_TypeSigs_Nil ::  T_TypeSigs 
sem_TypeSigs_Nil  = T_TypeSigs (return st41) where
   {-# NOINLINE st41 #-}
   st41 = let
      v40 :: T_TypeSigs_v40 
      v40 = \ (T_TypeSigs_vIn40 _lhsItypeSigs) -> ( let
         _lhsOtypeSigs :: Map Identifier Type
         _lhsOtypeSigs = rule578 _lhsItypeSigs
         __result_ = T_TypeSigs_vOut40 _lhsOtypeSigs
         in __result_ )
     in C_TypeSigs_s41 v40
   {-# INLINE rule578 #-}
   rule578 = \ ((_lhsItypeSigs) :: Map Identifier Type) ->
     _lhsItypeSigs
