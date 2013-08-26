{-# LANGUAGE Rank2Types, GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Order where
{-# LINE 2 "./src-ag/AbstractSyntax.ag" #-}

-- AbstractSyntax.ag imports
import Data.Set(Set)
import Data.Map(Map)
import Patterns    (Pattern(..),Patterns)
import Expression  (Expression(..))
import Macro --marcos
import CommonTypes
import ErrorMessages
{-# LINE 16 "dist/build/Order.hs" #-}

{-# LINE 2 "./src-ag/Expression.ag" #-}

import UU.Scanner.Position(Pos)
import HsToken
{-# LINE 22 "dist/build/Order.hs" #-}

{-# LINE 2 "./src-ag/Patterns.ag" #-}

-- Patterns.ag imports
import UU.Scanner.Position(Pos)
import CommonTypes (ConstructorIdent,Identifier)
{-# LINE 29 "dist/build/Order.hs" #-}

{-# LINE 10 "./src-ag/Order.ag" #-}

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
{-# LINE 66 "dist/build/Order.hs" #-}
import Control.Monad.Identity (Identity)
import qualified Control.Monad.Identity
{-# LINE 46 "./src-ag/Order.ag" #-}

-- Terminates with an error if the key is not in the map
findWithErr1 :: (Ord k, Show k) => String -> k -> Map k a -> a
findWithErr1 s k
  = Map.findWithDefault (error ("findWithErr1 " ++ s ++ ": key " ++ show k ++ " not in map.")) k

findWithErr2 :: (Ord k, Show k, Show a) => k -> Map k a -> a
findWithErr2 k m
  = Map.findWithDefault (error ("findWithErr2: key " ++ show k ++ " not in map: " ++ show m)) k m
{-# LINE 79 "dist/build/Order.hs" #-}

{-# LINE 71 "./src-ag/Order.ag" #-}

startsWith :: String -> String -> Bool
startsWith k h = k == take (length k) h
{-# LINE 85 "dist/build/Order.hs" #-}

{-# LINE 138 "./src-ag/Order.ag" #-}

getNtName :: Type -> NontermIdent
getNtName (NT nt _ _) = nt
getNtName _           = nullIdent
{-# LINE 92 "dist/build/Order.hs" #-}

{-# LINE 166 "./src-ag/Order.ag" #-}

data AltAttr = AltAttr Identifier Identifier Bool
               deriving (Eq, Ord, Show)
{-# LINE 98 "dist/build/Order.hs" #-}

{-# LINE 239 "./src-ag/Order.ag" #-}

substSelf nt tp
  = case tp of
      NT n tps defor | n == _SELF -> NT nt tps defor
      _                           -> tp

haskellTupel :: [Type] -> Maybe Type
haskellTupel ts =  Just ( Haskell ( '(' : (concat (intersperse "," (map show ts))) ++ ")" ))
{-# LINE 109 "dist/build/Order.hs" #-}

{-# LINE 692 "./src-ag/Order.ag" #-}

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
{-# LINE 229 "dist/build/Order.hs" #-}
-- Child -------------------------------------------------------
-- wrapper
data Inh_Child  = Inh_Child { allfields_Inh_Child :: ([(Identifier,Type,ChildKind)]), allnts_Inh_Child :: ([Identifier]), attrs_Inh_Child :: ([(Identifier,Identifier)]), con_Inh_Child :: (Identifier), inh_Inh_Child :: (Attributes), inhMap_Inh_Child :: (Map Identifier Attributes), mergeMap_Inh_Child :: (Map Identifier (Identifier,[Identifier])), nt_Inh_Child :: (Identifier), o_unbox_Inh_Child :: (Bool), syn_Inh_Child :: (Attributes), synMap_Inh_Child :: (Map Identifier Attributes) }
data Syn_Child  = Syn_Child { attributes_Syn_Child :: ([(Identifier,Attributes,Attributes)]), collectChildrenInhs_Syn_Child :: (Map Identifier Attributes ), collectChildrenSyns_Syn_Child :: (Map Identifier Attributes ), errors_Syn_Child :: (Seq Error), field_Syn_Child :: ((Identifier,Type,ChildKind)), gathAltAttrs_Syn_Child :: ([AltAttr]), gathRules_Syn_Child :: (Seq CRule), inhs_Syn_Child :: (Seq (Identifier,Attributes)), nts_Syn_Child :: (Seq (Identifier,NontermIdent)), singlevisits_Syn_Child :: ([CRule]), terminals_Syn_Child :: ([Identifier]) }
{-# INLINABLE wrap_Child #-}
wrap_Child :: T_Child  -> Inh_Child  -> (Syn_Child )
wrap_Child (T_Child act) (Inh_Child _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsIinh _lhsIinhMap _lhsImergeMap _lhsInt _lhsIo_unbox _lhsIsyn _lhsIsynMap) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Child_vIn1 _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsIinh _lhsIinhMap _lhsImergeMap _lhsInt _lhsIo_unbox _lhsIsyn _lhsIsynMap
        (T_Child_vOut1 _lhsOattributes _lhsOcollectChildrenInhs _lhsOcollectChildrenSyns _lhsOerrors _lhsOfield _lhsOgathAltAttrs _lhsOgathRules _lhsOinhs _lhsOnts _lhsOsinglevisits _lhsOterminals) <- return (inv_Child_s2 sem arg)
        return (Syn_Child _lhsOattributes _lhsOcollectChildrenInhs _lhsOcollectChildrenSyns _lhsOerrors _lhsOfield _lhsOgathAltAttrs _lhsOgathRules _lhsOinhs _lhsOnts _lhsOsinglevisits _lhsOterminals)
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
data T_Child_vOut1  = T_Child_vOut1 ([(Identifier,Attributes,Attributes)]) (Map Identifier Attributes ) (Map Identifier Attributes ) (Seq Error) ((Identifier,Type,ChildKind)) ([AltAttr]) (Seq CRule) (Seq (Identifier,Attributes)) (Seq (Identifier,NontermIdent)) ([CRule]) ([Identifier])
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
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule14  ()
         _lhsOgathRules :: Seq CRule
         _lhsOgathRules = rule15 _gathRules
         __result_ = T_Child_vOut1 _lhsOattributes _lhsOcollectChildrenInhs _lhsOcollectChildrenSyns _lhsOerrors _lhsOfield _lhsOgathAltAttrs _lhsOgathRules _lhsOinhs _lhsOnts _lhsOsinglevisits _lhsOterminals
         in __result_ )
     in C_Child_s2 v1
   {-# INLINE rule0 #-}
   {-# LINE 19 "./src-ag/DistChildAttr.ag" #-}
   rule0 = \ name_ tp_ ->
                       {-# LINE 19 "./src-ag/DistChildAttr.ag" #-}
                       case tp_ of
                         NT nt _ _ -> nt
                         Self      -> error ("The type of child " ++ show name_ ++ " should not be a Self type.")
                         Haskell t -> identifier ""
                       {-# LINE 305 "dist/build/Order.hs"#-}
   {-# INLINE rule1 #-}
   {-# LINE 23 "./src-ag/DistChildAttr.ag" #-}
   rule1 = \ _chnt ((_lhsIinhMap) :: Map Identifier Attributes) ->
                      {-# LINE 23 "./src-ag/DistChildAttr.ag" #-}
                      Map.findWithDefault Map.empty _chnt     _lhsIinhMap
                      {-# LINE 311 "dist/build/Order.hs"#-}
   {-# INLINE rule2 #-}
   {-# LINE 24 "./src-ag/DistChildAttr.ag" #-}
   rule2 = \ _chnt ((_lhsIsynMap) :: Map Identifier Attributes) ->
                      {-# LINE 24 "./src-ag/DistChildAttr.ag" #-}
                      Map.findWithDefault Map.empty _chnt     _lhsIsynMap
                      {-# LINE 317 "dist/build/Order.hs"#-}
   {-# INLINE rule3 #-}
   {-# LINE 180 "./src-ag/Order.ag" #-}
   rule3 = \ _syn tp_ ->
                                {-# LINE 180 "./src-ag/Order.ag" #-}
                                case tp_ of
                                  NT nt _ _ -> Map.null _syn
                                  _         -> True
                                {-# LINE 325 "dist/build/Order.hs"#-}
   {-# INLINE rule4 #-}
   {-# LINE 183 "./src-ag/Order.ag" #-}
   rule4 = \ _maptolocal _syn name_ ->
                                 {-# LINE 183 "./src-ag/Order.ag" #-}
                                 if  _maptolocal
                                     then [ AltAttr _LOC name_ True ]
                                     else [ AltAttr name_ syn True | syn <- Map.keys _syn     ]
                                 {-# LINE 333 "dist/build/Order.hs"#-}
   {-# INLINE rule5 #-}
   {-# LINE 198 "./src-ag/Order.ag" #-}
   rule5 = \ name_ tp_ ->
                        {-# LINE 198 "./src-ag/Order.ag" #-}
                        Seq.singleton (name_,getNtName tp_)
                        {-# LINE 339 "dist/build/Order.hs"#-}
   {-# INLINE rule6 #-}
   {-# LINE 199 "./src-ag/Order.ag" #-}
   rule6 = \ _inh name_ ->
                         {-# LINE 199 "./src-ag/Order.ag" #-}
                         Seq.singleton (name_,_inh    )
                         {-# LINE 345 "dist/build/Order.hs"#-}
   {-# INLINE rule7 #-}
   {-# LINE 215 "./src-ag/Order.ag" #-}
   rule7 = \ ((_lhsIcon) :: Identifier) ((_lhsInt) :: Identifier) _maptolocal _syn name_ tp_ ->
                              {-# LINE 215 "./src-ag/Order.ag" #-}
                              if  _maptolocal
                                  then Seq.singleton (cRuleTerminal name_ _lhsInt _lhsIcon tp_)
                                  else Seq.fromList [ cRuleRhsSyn syn _lhsInt _lhsIcon tp name_ (getNtName tp_) | (syn,tp) <- Map.assocs _syn    ]
                              {-# LINE 353 "dist/build/Order.hs"#-}
   {-# INLINE rule8 #-}
   {-# LINE 347 "./src-ag/Order.ag" #-}
   rule8 = \ _syn name_ ->
                                       {-# LINE 347 "./src-ag/Order.ag" #-}
                                       Map.singleton name_ _syn
                                       {-# LINE 359 "dist/build/Order.hs"#-}
   {-# INLINE rule9 #-}
   {-# LINE 348 "./src-ag/Order.ag" #-}
   rule9 = \ _inh name_ ->
                                       {-# LINE 348 "./src-ag/Order.ag" #-}
                                       Map.singleton name_ _inh
                                       {-# LINE 365 "dist/build/Order.hs"#-}
   {-# INLINE rule10 #-}
   {-# LINE 618 "./src-ag/Order.ag" #-}
   rule10 = \ _inh _maptolocal _syn name_ tp_ ->
                                 {-# LINE 618 "./src-ag/Order.ag" #-}
                                 if  _maptolocal
                                     then []
                                     else [CChildVisit name_ (getNtName tp_) 0 _inh     _syn     True]
                                 {-# LINE 373 "dist/build/Order.hs"#-}
   {-# INLINE rule11 #-}
   {-# LINE 643 "./src-ag/Order.ag" #-}
   rule11 = \ _maptolocal name_ ->
                            {-# LINE 643 "./src-ag/Order.ag" #-}
                            if _maptolocal
                            then [name_]
                            else []
                            {-# LINE 381 "dist/build/Order.hs"#-}
   {-# INLINE rule12 #-}
   {-# LINE 672 "./src-ag/Order.ag" #-}
   rule12 = \ _inh _syn name_ ->
                             {-# LINE 672 "./src-ag/Order.ag" #-}
                             [(name_, _inh    , _syn    )]
                             {-# LINE 387 "dist/build/Order.hs"#-}
   {-# INLINE rule13 #-}
   {-# LINE 676 "./src-ag/Order.ag" #-}
   rule13 = \ kind_ name_ tp_ ->
                        {-# LINE 676 "./src-ag/Order.ag" #-}
                        (name_, tp_, kind_)
                        {-# LINE 393 "dist/build/Order.hs"#-}
   {-# INLINE rule14 #-}
   rule14 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule15 #-}
   rule15 = \ _gathRules ->
     _gathRules

-- Children ----------------------------------------------------
-- wrapper
data Inh_Children  = Inh_Children { allfields_Inh_Children :: ([(Identifier,Type,ChildKind)]), allnts_Inh_Children :: ([Identifier]), attrs_Inh_Children :: ([(Identifier,Identifier)]), con_Inh_Children :: (Identifier), inh_Inh_Children :: (Attributes), inhMap_Inh_Children :: (Map Identifier Attributes), mergeMap_Inh_Children :: (Map Identifier (Identifier,[Identifier])), nt_Inh_Children :: (Identifier), o_unbox_Inh_Children :: (Bool), syn_Inh_Children :: (Attributes), synMap_Inh_Children :: (Map Identifier Attributes) }
data Syn_Children  = Syn_Children { attributes_Syn_Children :: ([(Identifier,Attributes,Attributes)]), collectChildrenInhs_Syn_Children :: (Map Identifier Attributes ), collectChildrenSyns_Syn_Children :: (Map Identifier Attributes ), errors_Syn_Children :: (Seq Error), fields_Syn_Children :: ([(Identifier,Type,ChildKind)]), gathAltAttrs_Syn_Children :: ([AltAttr]), gathRules_Syn_Children :: (Seq CRule), inhs_Syn_Children :: (Seq (Identifier,Attributes)), nts_Syn_Children :: (Seq (Identifier,NontermIdent)), singlevisits_Syn_Children :: ([CRule]), terminals_Syn_Children :: ([Identifier]) }
{-# INLINABLE wrap_Children #-}
wrap_Children :: T_Children  -> Inh_Children  -> (Syn_Children )
wrap_Children (T_Children act) (Inh_Children _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsIinh _lhsIinhMap _lhsImergeMap _lhsInt _lhsIo_unbox _lhsIsyn _lhsIsynMap) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Children_vIn4 _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsIinh _lhsIinhMap _lhsImergeMap _lhsInt _lhsIo_unbox _lhsIsyn _lhsIsynMap
        (T_Children_vOut4 _lhsOattributes _lhsOcollectChildrenInhs _lhsOcollectChildrenSyns _lhsOerrors _lhsOfields _lhsOgathAltAttrs _lhsOgathRules _lhsOinhs _lhsOnts _lhsOsinglevisits _lhsOterminals) <- return (inv_Children_s5 sem arg)
        return (Syn_Children _lhsOattributes _lhsOcollectChildrenInhs _lhsOcollectChildrenSyns _lhsOerrors _lhsOfields _lhsOgathAltAttrs _lhsOgathRules _lhsOinhs _lhsOnts _lhsOsinglevisits _lhsOterminals)
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
data T_Children_vOut4  = T_Children_vOut4 ([(Identifier,Attributes,Attributes)]) (Map Identifier Attributes ) (Map Identifier Attributes ) (Seq Error) ([(Identifier,Type,ChildKind)]) ([AltAttr]) (Seq CRule) (Seq (Identifier,Attributes)) (Seq (Identifier,NontermIdent)) ([CRule]) ([Identifier])
{-# NOINLINE sem_Children_Cons #-}
sem_Children_Cons :: T_Child  -> T_Children  -> T_Children 
sem_Children_Cons arg_hd_ arg_tl_ = T_Children (return st5) where
   {-# NOINLINE st5 #-}
   st5 = let
      v4 :: T_Children_v4 
      v4 = \ (T_Children_vIn4 _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsIinh _lhsIinhMap _lhsImergeMap _lhsInt _lhsIo_unbox _lhsIsyn _lhsIsynMap) -> ( let
         _hdX2 = Control.Monad.Identity.runIdentity (attach_T_Child (arg_hd_))
         _tlX5 = Control.Monad.Identity.runIdentity (attach_T_Children (arg_tl_))
         (T_Child_vOut1 _hdIattributes _hdIcollectChildrenInhs _hdIcollectChildrenSyns _hdIerrors _hdIfield _hdIgathAltAttrs _hdIgathRules _hdIinhs _hdInts _hdIsinglevisits _hdIterminals) = inv_Child_s2 _hdX2 (T_Child_vIn1 _hdOallfields _hdOallnts _hdOattrs _hdOcon _hdOinh _hdOinhMap _hdOmergeMap _hdOnt _hdOo_unbox _hdOsyn _hdOsynMap)
         (T_Children_vOut4 _tlIattributes _tlIcollectChildrenInhs _tlIcollectChildrenSyns _tlIerrors _tlIfields _tlIgathAltAttrs _tlIgathRules _tlIinhs _tlInts _tlIsinglevisits _tlIterminals) = inv_Children_s5 _tlX5 (T_Children_vIn4 _tlOallfields _tlOallnts _tlOattrs _tlOcon _tlOinh _tlOinhMap _tlOmergeMap _tlOnt _tlOo_unbox _tlOsyn _tlOsynMap)
         _lhsOfields :: [(Identifier,Type,ChildKind)]
         _lhsOfields = rule16 _hdIfield _tlIfields
         _lhsOattributes :: [(Identifier,Attributes,Attributes)]
         _lhsOattributes = rule17 _hdIattributes _tlIattributes
         _lhsOcollectChildrenInhs :: Map Identifier Attributes 
         _lhsOcollectChildrenInhs = rule18 _hdIcollectChildrenInhs _tlIcollectChildrenInhs
         _lhsOcollectChildrenSyns :: Map Identifier Attributes 
         _lhsOcollectChildrenSyns = rule19 _hdIcollectChildrenSyns _tlIcollectChildrenSyns
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule20 _hdIerrors _tlIerrors
         _lhsOgathAltAttrs :: [AltAttr]
         _lhsOgathAltAttrs = rule21 _hdIgathAltAttrs _tlIgathAltAttrs
         _lhsOgathRules :: Seq CRule
         _lhsOgathRules = rule22 _hdIgathRules _tlIgathRules
         _lhsOinhs :: Seq (Identifier,Attributes)
         _lhsOinhs = rule23 _hdIinhs _tlIinhs
         _lhsOnts :: Seq (Identifier,NontermIdent)
         _lhsOnts = rule24 _hdInts _tlInts
         _lhsOsinglevisits :: [CRule]
         _lhsOsinglevisits = rule25 _hdIsinglevisits _tlIsinglevisits
         _lhsOterminals :: [Identifier]
         _lhsOterminals = rule26 _hdIterminals _tlIterminals
         _hdOallfields = rule27 _lhsIallfields
         _hdOallnts = rule28 _lhsIallnts
         _hdOattrs = rule29 _lhsIattrs
         _hdOcon = rule30 _lhsIcon
         _hdOinh = rule31 _lhsIinh
         _hdOinhMap = rule32 _lhsIinhMap
         _hdOmergeMap = rule33 _lhsImergeMap
         _hdOnt = rule34 _lhsInt
         _hdOo_unbox = rule35 _lhsIo_unbox
         _hdOsyn = rule36 _lhsIsyn
         _hdOsynMap = rule37 _lhsIsynMap
         _tlOallfields = rule38 _lhsIallfields
         _tlOallnts = rule39 _lhsIallnts
         _tlOattrs = rule40 _lhsIattrs
         _tlOcon = rule41 _lhsIcon
         _tlOinh = rule42 _lhsIinh
         _tlOinhMap = rule43 _lhsIinhMap
         _tlOmergeMap = rule44 _lhsImergeMap
         _tlOnt = rule45 _lhsInt
         _tlOo_unbox = rule46 _lhsIo_unbox
         _tlOsyn = rule47 _lhsIsyn
         _tlOsynMap = rule48 _lhsIsynMap
         __result_ = T_Children_vOut4 _lhsOattributes _lhsOcollectChildrenInhs _lhsOcollectChildrenSyns _lhsOerrors _lhsOfields _lhsOgathAltAttrs _lhsOgathRules _lhsOinhs _lhsOnts _lhsOsinglevisits _lhsOterminals
         in __result_ )
     in C_Children_s5 v4
   {-# INLINE rule16 #-}
   {-# LINE 679 "./src-ag/Order.ag" #-}
   rule16 = \ ((_hdIfield) :: (Identifier,Type,ChildKind)) ((_tlIfields) :: [(Identifier,Type,ChildKind)]) ->
                         {-# LINE 679 "./src-ag/Order.ag" #-}
                         _hdIfield : _tlIfields
                         {-# LINE 494 "dist/build/Order.hs"#-}
   {-# INLINE rule17 #-}
   rule17 = \ ((_hdIattributes) :: [(Identifier,Attributes,Attributes)]) ((_tlIattributes) :: [(Identifier,Attributes,Attributes)]) ->
     _hdIattributes ++ _tlIattributes
   {-# INLINE rule18 #-}
   rule18 = \ ((_hdIcollectChildrenInhs) :: Map Identifier Attributes ) ((_tlIcollectChildrenInhs) :: Map Identifier Attributes ) ->
     _hdIcollectChildrenInhs `Map.union` _tlIcollectChildrenInhs
   {-# INLINE rule19 #-}
   rule19 = \ ((_hdIcollectChildrenSyns) :: Map Identifier Attributes ) ((_tlIcollectChildrenSyns) :: Map Identifier Attributes ) ->
     _hdIcollectChildrenSyns `Map.union` _tlIcollectChildrenSyns
   {-# INLINE rule20 #-}
   rule20 = \ ((_hdIerrors) :: Seq Error) ((_tlIerrors) :: Seq Error) ->
     _hdIerrors Seq.>< _tlIerrors
   {-# INLINE rule21 #-}
   rule21 = \ ((_hdIgathAltAttrs) :: [AltAttr]) ((_tlIgathAltAttrs) :: [AltAttr]) ->
     _hdIgathAltAttrs ++ _tlIgathAltAttrs
   {-# INLINE rule22 #-}
   rule22 = \ ((_hdIgathRules) :: Seq CRule) ((_tlIgathRules) :: Seq CRule) ->
     _hdIgathRules Seq.>< _tlIgathRules
   {-# INLINE rule23 #-}
   rule23 = \ ((_hdIinhs) :: Seq (Identifier,Attributes)) ((_tlIinhs) :: Seq (Identifier,Attributes)) ->
     _hdIinhs Seq.>< _tlIinhs
   {-# INLINE rule24 #-}
   rule24 = \ ((_hdInts) :: Seq (Identifier,NontermIdent)) ((_tlInts) :: Seq (Identifier,NontermIdent)) ->
     _hdInts Seq.>< _tlInts
   {-# INLINE rule25 #-}
   rule25 = \ ((_hdIsinglevisits) :: [CRule]) ((_tlIsinglevisits) :: [CRule]) ->
     _hdIsinglevisits ++ _tlIsinglevisits
   {-# INLINE rule26 #-}
   rule26 = \ ((_hdIterminals) :: [Identifier]) ((_tlIterminals) :: [Identifier]) ->
     _hdIterminals ++ _tlIterminals
   {-# INLINE rule27 #-}
   rule27 = \ ((_lhsIallfields) :: [(Identifier,Type,ChildKind)]) ->
     _lhsIallfields
   {-# INLINE rule28 #-}
   rule28 = \ ((_lhsIallnts) :: [Identifier]) ->
     _lhsIallnts
   {-# INLINE rule29 #-}
   rule29 = \ ((_lhsIattrs) :: [(Identifier,Identifier)]) ->
     _lhsIattrs
   {-# INLINE rule30 #-}
   rule30 = \ ((_lhsIcon) :: Identifier) ->
     _lhsIcon
   {-# INLINE rule31 #-}
   rule31 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule32 #-}
   rule32 = \ ((_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# INLINE rule33 #-}
   rule33 = \ ((_lhsImergeMap) :: Map Identifier (Identifier,[Identifier])) ->
     _lhsImergeMap
   {-# INLINE rule34 #-}
   rule34 = \ ((_lhsInt) :: Identifier) ->
     _lhsInt
   {-# INLINE rule35 #-}
   rule35 = \ ((_lhsIo_unbox) :: Bool) ->
     _lhsIo_unbox
   {-# INLINE rule36 #-}
   rule36 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
   {-# INLINE rule37 #-}
   rule37 = \ ((_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
   {-# INLINE rule38 #-}
   rule38 = \ ((_lhsIallfields) :: [(Identifier,Type,ChildKind)]) ->
     _lhsIallfields
   {-# INLINE rule39 #-}
   rule39 = \ ((_lhsIallnts) :: [Identifier]) ->
     _lhsIallnts
   {-# INLINE rule40 #-}
   rule40 = \ ((_lhsIattrs) :: [(Identifier,Identifier)]) ->
     _lhsIattrs
   {-# INLINE rule41 #-}
   rule41 = \ ((_lhsIcon) :: Identifier) ->
     _lhsIcon
   {-# INLINE rule42 #-}
   rule42 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule43 #-}
   rule43 = \ ((_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# INLINE rule44 #-}
   rule44 = \ ((_lhsImergeMap) :: Map Identifier (Identifier,[Identifier])) ->
     _lhsImergeMap
   {-# INLINE rule45 #-}
   rule45 = \ ((_lhsInt) :: Identifier) ->
     _lhsInt
   {-# INLINE rule46 #-}
   rule46 = \ ((_lhsIo_unbox) :: Bool) ->
     _lhsIo_unbox
   {-# INLINE rule47 #-}
   rule47 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
   {-# INLINE rule48 #-}
   rule48 = \ ((_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
{-# NOINLINE sem_Children_Nil #-}
sem_Children_Nil ::  T_Children 
sem_Children_Nil  = T_Children (return st5) where
   {-# NOINLINE st5 #-}
   st5 = let
      v4 :: T_Children_v4 
      v4 = \ (T_Children_vIn4 _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsIinh _lhsIinhMap _lhsImergeMap _lhsInt _lhsIo_unbox _lhsIsyn _lhsIsynMap) -> ( let
         _lhsOfields :: [(Identifier,Type,ChildKind)]
         _lhsOfields = rule49  ()
         _lhsOattributes :: [(Identifier,Attributes,Attributes)]
         _lhsOattributes = rule50  ()
         _lhsOcollectChildrenInhs :: Map Identifier Attributes 
         _lhsOcollectChildrenInhs = rule51  ()
         _lhsOcollectChildrenSyns :: Map Identifier Attributes 
         _lhsOcollectChildrenSyns = rule52  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule53  ()
         _lhsOgathAltAttrs :: [AltAttr]
         _lhsOgathAltAttrs = rule54  ()
         _lhsOgathRules :: Seq CRule
         _lhsOgathRules = rule55  ()
         _lhsOinhs :: Seq (Identifier,Attributes)
         _lhsOinhs = rule56  ()
         _lhsOnts :: Seq (Identifier,NontermIdent)
         _lhsOnts = rule57  ()
         _lhsOsinglevisits :: [CRule]
         _lhsOsinglevisits = rule58  ()
         _lhsOterminals :: [Identifier]
         _lhsOterminals = rule59  ()
         __result_ = T_Children_vOut4 _lhsOattributes _lhsOcollectChildrenInhs _lhsOcollectChildrenSyns _lhsOerrors _lhsOfields _lhsOgathAltAttrs _lhsOgathRules _lhsOinhs _lhsOnts _lhsOsinglevisits _lhsOterminals
         in __result_ )
     in C_Children_s5 v4
   {-# INLINE rule49 #-}
   {-# LINE 680 "./src-ag/Order.ag" #-}
   rule49 = \  (_ :: ()) ->
                         {-# LINE 680 "./src-ag/Order.ag" #-}
                         []
                         {-# LINE 628 "dist/build/Order.hs"#-}
   {-# INLINE rule50 #-}
   rule50 = \  (_ :: ()) ->
     []
   {-# INLINE rule51 #-}
   rule51 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule52 #-}
   rule52 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule53 #-}
   rule53 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule54 #-}
   rule54 = \  (_ :: ()) ->
     []
   {-# INLINE rule55 #-}
   rule55 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule56 #-}
   rule56 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule57 #-}
   rule57 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule58 #-}
   rule58 = \  (_ :: ()) ->
     []
   {-# INLINE rule59 #-}
   rule59 = \  (_ :: ()) ->
     []

-- Expression --------------------------------------------------
-- wrapper
data Inh_Expression  = Inh_Expression { allfields_Inh_Expression :: ([(Identifier,Type,ChildKind)]), allnts_Inh_Expression :: ([Identifier]), attrs_Inh_Expression :: ([(Identifier,Identifier)]), con_Inh_Expression :: (Identifier), mergeMap_Inh_Expression :: (Map Identifier (Identifier,[Identifier])), nt_Inh_Expression :: (Identifier), options_Inh_Expression :: (Options) }
data Syn_Expression  = Syn_Expression { allRhsVars_Syn_Expression :: (Set (Identifier,Identifier)), copy_Syn_Expression :: (Expression), errors_Syn_Expression :: (Seq Error), textLines_Syn_Expression :: ([String]), usedAttrs_Syn_Expression :: ([(Identifier,Identifier)]), usedFields_Syn_Expression :: ([Identifier]), usedLocals_Syn_Expression :: ([Identifier]) }
{-# INLINABLE wrap_Expression #-}
wrap_Expression :: T_Expression  -> Inh_Expression  -> (Syn_Expression )
wrap_Expression (T_Expression act) (Inh_Expression _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsImergeMap _lhsInt _lhsIoptions) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Expression_vIn7 _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsImergeMap _lhsInt _lhsIoptions
        (T_Expression_vOut7 _lhsOallRhsVars _lhsOcopy _lhsOerrors _lhsOtextLines _lhsOusedAttrs _lhsOusedFields _lhsOusedLocals) <- return (inv_Expression_s8 sem arg)
        return (Syn_Expression _lhsOallRhsVars _lhsOcopy _lhsOerrors _lhsOtextLines _lhsOusedAttrs _lhsOusedFields _lhsOusedLocals)
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
data T_Expression_vOut7  = T_Expression_vOut7 (Set (Identifier,Identifier)) (Expression) (Seq Error) ([String]) ([(Identifier,Identifier)]) ([Identifier]) ([Identifier])
{-# NOINLINE sem_Expression_Expression #-}
sem_Expression_Expression :: (Pos) -> ([HsToken]) -> T_Expression 
sem_Expression_Expression arg_pos_ arg_tks_ = T_Expression (return st8) where
   {-# NOINLINE st8 #-}
   st8 = let
      v7 :: T_Expression_v7 
      v7 = \ (T_Expression_vIn7 _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsImergeMap _lhsInt _lhsIoptions) -> ( let
         (_textLines,_usedAttrs,_usedLocals,_usedFields) = rule60 _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsImergeMap _lhsInt _lhsIoptions arg_tks_
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule61  ()
         _lhsOallRhsVars :: Set (Identifier,Identifier)
         _lhsOallRhsVars = rule62 _usedAttrs _usedFields _usedLocals
         _copy = rule63 arg_pos_ arg_tks_
         _lhsOcopy :: Expression
         _lhsOcopy = rule64 _copy
         _lhsOtextLines :: [String]
         _lhsOtextLines = rule65 _textLines
         _lhsOusedAttrs :: [(Identifier,Identifier)]
         _lhsOusedAttrs = rule66 _usedAttrs
         _lhsOusedFields :: [Identifier]
         _lhsOusedFields = rule67 _usedFields
         _lhsOusedLocals :: [Identifier]
         _lhsOusedLocals = rule68 _usedLocals
         __result_ = T_Expression_vOut7 _lhsOallRhsVars _lhsOcopy _lhsOerrors _lhsOtextLines _lhsOusedAttrs _lhsOusedFields _lhsOusedLocals
         in __result_ )
     in C_Expression_s8 v7
   {-# INLINE rule60 #-}
   {-# LINE 469 "./src-ag/Order.ag" #-}
   rule60 = \ ((_lhsIallfields) :: [(Identifier,Type,ChildKind)]) ((_lhsIallnts) :: [Identifier]) ((_lhsIattrs) :: [(Identifier,Identifier)]) ((_lhsIcon) :: Identifier) ((_lhsImergeMap) :: Map Identifier (Identifier,[Identifier])) ((_lhsInt) :: Identifier) ((_lhsIoptions) :: Options) tks_ ->
                                {-# LINE 469 "./src-ag/Order.ag" #-}
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
                                {-# LINE 742 "dist/build/Order.hs"#-}
   {-# INLINE rule61 #-}
   {-# LINE 492 "./src-ag/Order.ag" #-}
   rule61 = \  (_ :: ()) ->
                               {-# LINE 492 "./src-ag/Order.ag" #-}
                               Seq.empty
                               {-# LINE 748 "dist/build/Order.hs"#-}
   {-# INLINE rule62 #-}
   {-# LINE 493 "./src-ag/Order.ag" #-}
   rule62 = \ _usedAttrs _usedFields _usedLocals ->
                                   {-# LINE 493 "./src-ag/Order.ag" #-}
                                   Set.fromList _usedAttrs
                                   `Set.union`
                                   Set.fromList [ (_LOC, l) | l <- _usedLocals    ]
                                   `Set.union`
                                   Set.fromList [ (_FIELD, fld) | fld <- _usedFields    ]
                                   {-# LINE 758 "dist/build/Order.hs"#-}
   {-# INLINE rule63 #-}
   rule63 = \ pos_ tks_ ->
     Expression pos_ tks_
   {-# INLINE rule64 #-}
   rule64 = \ _copy ->
     _copy
   {-# INLINE rule65 #-}
   rule65 = \ _textLines ->
     _textLines
   {-# INLINE rule66 #-}
   rule66 = \ _usedAttrs ->
     _usedAttrs
   {-# INLINE rule67 #-}
   rule67 = \ _usedFields ->
     _usedFields
   {-# INLINE rule68 #-}
   rule68 = \ _usedLocals ->
     _usedLocals

-- Grammar -----------------------------------------------------
-- wrapper
data Inh_Grammar  = Inh_Grammar { options_Inh_Grammar :: (Options) }
data Syn_Grammar  = Syn_Grammar { errors_Syn_Grammar :: (Seq Error), nAutoRules_Syn_Grammar :: (Int), nExplicitRules_Syn_Grammar :: (Int), output_Syn_Grammar :: (CGrammar) }
{-# INLINABLE wrap_Grammar #-}
wrap_Grammar :: T_Grammar  -> Inh_Grammar  -> (Syn_Grammar )
wrap_Grammar (T_Grammar act) (Inh_Grammar _lhsIoptions) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Grammar_vIn10 _lhsIoptions
        (T_Grammar_vOut10 _lhsOerrors _lhsOnAutoRules _lhsOnExplicitRules _lhsOoutput) <- return (inv_Grammar_s11 sem arg)
        return (Syn_Grammar _lhsOerrors _lhsOnAutoRules _lhsOnExplicitRules _lhsOoutput)
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
data T_Grammar_vOut10  = T_Grammar_vOut10 (Seq Error) (Int) (Int) (CGrammar)
{-# NOINLINE sem_Grammar_Grammar #-}
sem_Grammar_Grammar :: (TypeSyns) -> (UseMap) -> (Derivings) -> (Set NontermIdent) -> T_Nonterminals  -> (PragmaMap) -> (AttrOrderMap) -> (ParamMap) -> (ContextMap) -> (QuantMap) -> (UniqueMap) -> (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) -> (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) -> (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier, [Identifier], Expression)))) -> T_Grammar 
sem_Grammar_Grammar arg_typeSyns_ _ arg_derivings_ arg_wrappers_ arg_nonts_ arg_pragmas_ arg_manualAttrOrderMap_ arg_paramMap_ arg_contextMap_ arg_quantMap_ _ _ arg_aroundsMap_ arg_mergeMap_ = T_Grammar (return st11) where
   {-# NOINLINE st11 #-}
   st11 = let
      v10 :: T_Grammar_v10 
      v10 = \ (T_Grammar_vIn10 _lhsIoptions) -> ( let
         _nontsX17 = Control.Monad.Identity.runIdentity (attach_T_Nonterminals (arg_nonts_))
         (T_Nonterminals_vOut16 _nontsIacount _nontsIadditionalDep _nontsIaranges _nontsIaroundDep _nontsIcNonterminals _nontsIdirectDep _nontsIerrors _nontsIinhMap' _nontsIinstDep _nontsImergeDep _nontsInAutoRules _nontsInExplicitRules _nontsInonts _nontsIntattrs _nontsIrules _nontsIsynMap' _nontsIvcount) = inv_Nonterminals_s17 _nontsX17 (T_Nonterminals_vIn16 _nontsOacount _nontsOallnts _nontsOaroundMap _nontsOcInterfaceMap _nontsOcVisitsMap _nontsOinhMap _nontsOmanualAttrDepMap _nontsOmergeMap _nontsOo_case _nontsOo_cata _nontsOo_data _nontsOo_dovisit _nontsOo_newtypes _nontsOo_rename _nontsOo_sem _nontsOo_sig _nontsOo_unbox _nontsOo_wantvisit _nontsOoptions _nontsOprefix _nontsOsynMap _nontsOvcount)
         _nontsOinhMap = rule69 _nontsIinhMap'
         _nontsOsynMap = rule70 _nontsIsynMap'
         _o_dovisit = rule71 _cyclesErrors _lhsIoptions
         _nontsOo_cata = rule72 _lhsIoptions
         _nontsOo_data = rule73 _lhsIoptions
         _nontsOo_sig = rule74 _lhsIoptions
         _nontsOo_sem = rule75 _lhsIoptions
         _nontsOo_rename = rule76 _lhsIoptions
         _nontsOo_newtypes = rule77 _lhsIoptions
         _nontsOo_wantvisit = rule78 _lhsIoptions
         _nontsOo_unbox = rule79 _lhsIoptions
         _nontsOo_case = rule80 _lhsIoptions
         _nontsOprefix = rule81 _lhsIoptions
         _nontsOvcount = rule82  ()
         _nontsOmanualAttrDepMap = rule83 arg_manualAttrOrderMap_
         _nontsOaroundMap = rule84 arg_aroundsMap_
         _nontsOacount = rule85  ()
         _ruleTable = rule86 _nontsIrules _nontsIvcount
         _attrTable = rule87 _nontsIacount _nontsIntattrs
         _attrVertex = rule88 _nontsIntattrs
         _tdpToTds = rule89 _attrVertex _nontsIrules
         _tdsToTdp = rule90 _tdpToTds
         _directDep = rule91 _nontsIadditionalDep _nontsIdirectDep
         _instDep = rule92 _nontsIinstDep
         _aroundDep = rule93 _nontsIaroundDep
         _mergeDep = rule94 _nontsImergeDep
         _info = rule95 _attrTable _nontsIacount _nontsIaranges _nontsInonts _nontsIvcount _ruleTable _tdpToTds _tdsToTdp arg_wrappers_
         (_cInterfaceMap,_cVisitsMap,_cyclesErrors) = rule96 _aroundDep _attrTable _directDep _info _instDep _lhsIoptions _mergeDep _ruleTable
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule97 _cyclesErrors _lhsIoptions _nontsIerrors
         _lhsOoutput :: CGrammar
         _lhsOoutput = rule98 _aroundMap _mergeMap _nontsIcNonterminals _o_dovisit arg_contextMap_ arg_derivings_ arg_paramMap_ arg_pragmas_ arg_quantMap_ arg_typeSyns_ arg_wrappers_
         _aroundMap = rule99 arg_aroundsMap_
         _mergeMap = rule100 arg_mergeMap_
         _nontsOallnts = rule101 _nontsInonts
         _lhsOnAutoRules :: Int
         _lhsOnAutoRules = rule102 _nontsInAutoRules
         _lhsOnExplicitRules :: Int
         _lhsOnExplicitRules = rule103 _nontsInExplicitRules
         _nontsOcInterfaceMap = rule104 _cInterfaceMap
         _nontsOcVisitsMap = rule105 _cVisitsMap
         _nontsOmergeMap = rule106 _mergeMap
         _nontsOo_dovisit = rule107 _o_dovisit
         _nontsOoptions = rule108 _lhsIoptions
         __result_ = T_Grammar_vOut10 _lhsOerrors _lhsOnAutoRules _lhsOnExplicitRules _lhsOoutput
         in __result_ )
     in C_Grammar_s11 v10
   {-# INLINE rule69 #-}
   {-# LINE 15 "./src-ag/DistChildAttr.ag" #-}
   rule69 = \ ((_nontsIinhMap') :: Map Identifier Attributes) ->
                             {-# LINE 15 "./src-ag/DistChildAttr.ag" #-}
                             _nontsIinhMap'
                             {-# LINE 869 "dist/build/Order.hs"#-}
   {-# INLINE rule70 #-}
   {-# LINE 16 "./src-ag/DistChildAttr.ag" #-}
   rule70 = \ ((_nontsIsynMap') :: Map Identifier Attributes) ->
                             {-# LINE 16 "./src-ag/DistChildAttr.ag" #-}
                             _nontsIsynMap'
                             {-# LINE 875 "dist/build/Order.hs"#-}
   {-# INLINE rule71 #-}
   {-# LINE 123 "./src-ag/Order.ag" #-}
   rule71 = \ _cyclesErrors ((_lhsIoptions) :: Options) ->
                                    {-# LINE 123 "./src-ag/Order.ag" #-}
                                    visit     _lhsIoptions && null _cyclesErrors
                                    {-# LINE 881 "dist/build/Order.hs"#-}
   {-# INLINE rule72 #-}
   {-# LINE 124 "./src-ag/Order.ag" #-}
   rule72 = \ ((_lhsIoptions) :: Options) ->
                                    {-# LINE 124 "./src-ag/Order.ag" #-}
                                    folds     _lhsIoptions
                                    {-# LINE 887 "dist/build/Order.hs"#-}
   {-# INLINE rule73 #-}
   {-# LINE 125 "./src-ag/Order.ag" #-}
   rule73 = \ ((_lhsIoptions) :: Options) ->
                                    {-# LINE 125 "./src-ag/Order.ag" #-}
                                    dataTypes _lhsIoptions
                                    {-# LINE 893 "dist/build/Order.hs"#-}
   {-# INLINE rule74 #-}
   {-# LINE 126 "./src-ag/Order.ag" #-}
   rule74 = \ ((_lhsIoptions) :: Options) ->
                                    {-# LINE 126 "./src-ag/Order.ag" #-}
                                    typeSigs  _lhsIoptions
                                    {-# LINE 899 "dist/build/Order.hs"#-}
   {-# INLINE rule75 #-}
   {-# LINE 127 "./src-ag/Order.ag" #-}
   rule75 = \ ((_lhsIoptions) :: Options) ->
                                    {-# LINE 127 "./src-ag/Order.ag" #-}
                                    semfuns   _lhsIoptions
                                    {-# LINE 905 "dist/build/Order.hs"#-}
   {-# INLINE rule76 #-}
   {-# LINE 128 "./src-ag/Order.ag" #-}
   rule76 = \ ((_lhsIoptions) :: Options) ->
                                    {-# LINE 128 "./src-ag/Order.ag" #-}
                                    rename    _lhsIoptions
                                    {-# LINE 911 "dist/build/Order.hs"#-}
   {-# INLINE rule77 #-}
   {-# LINE 129 "./src-ag/Order.ag" #-}
   rule77 = \ ((_lhsIoptions) :: Options) ->
                                    {-# LINE 129 "./src-ag/Order.ag" #-}
                                    newtypes  _lhsIoptions
                                    {-# LINE 917 "dist/build/Order.hs"#-}
   {-# INLINE rule78 #-}
   {-# LINE 130 "./src-ag/Order.ag" #-}
   rule78 = \ ((_lhsIoptions) :: Options) ->
                                      {-# LINE 130 "./src-ag/Order.ag" #-}
                                      visit   _lhsIoptions
                                      {-# LINE 923 "dist/build/Order.hs"#-}
   {-# INLINE rule79 #-}
   {-# LINE 131 "./src-ag/Order.ag" #-}
   rule79 = \ ((_lhsIoptions) :: Options) ->
                                    {-# LINE 131 "./src-ag/Order.ag" #-}
                                    unbox     _lhsIoptions
                                    {-# LINE 929 "dist/build/Order.hs"#-}
   {-# INLINE rule80 #-}
   {-# LINE 132 "./src-ag/Order.ag" #-}
   rule80 = \ ((_lhsIoptions) :: Options) ->
                                    {-# LINE 132 "./src-ag/Order.ag" #-}
                                    cases     _lhsIoptions
                                    {-# LINE 935 "dist/build/Order.hs"#-}
   {-# INLINE rule81 #-}
   {-# LINE 133 "./src-ag/Order.ag" #-}
   rule81 = \ ((_lhsIoptions) :: Options) ->
                                    {-# LINE 133 "./src-ag/Order.ag" #-}
                                    prefix    _lhsIoptions
                                    {-# LINE 941 "dist/build/Order.hs"#-}
   {-# INLINE rule82 #-}
   {-# LINE 262 "./src-ag/Order.ag" #-}
   rule82 = \  (_ :: ()) ->
                               {-# LINE 262 "./src-ag/Order.ag" #-}
                               0
                               {-# LINE 947 "dist/build/Order.hs"#-}
   {-# INLINE rule83 #-}
   {-# LINE 288 "./src-ag/Order.ag" #-}
   rule83 = \ manualAttrOrderMap_ ->
                                 {-# LINE 288 "./src-ag/Order.ag" #-}
                                 manualAttrOrderMap_
                                 {-# LINE 953 "dist/build/Order.hs"#-}
   {-# INLINE rule84 #-}
   {-# LINE 417 "./src-ag/Order.ag" #-}
   rule84 = \ aroundsMap_ ->
                                 {-# LINE 417 "./src-ag/Order.ag" #-}
                                 aroundsMap_
                                 {-# LINE 959 "dist/build/Order.hs"#-}
   {-# INLINE rule85 #-}
   {-# LINE 508 "./src-ag/Order.ag" #-}
   rule85 = \  (_ :: ()) ->
                             {-# LINE 508 "./src-ag/Order.ag" #-}
                             0
                             {-# LINE 965 "dist/build/Order.hs"#-}
   {-# INLINE rule86 #-}
   {-# LINE 546 "./src-ag/Order.ag" #-}
   rule86 = \ ((_nontsIrules) :: Seq (Vertex,CRule)) ((_nontsIvcount) :: Int) ->
                              {-# LINE 546 "./src-ag/Order.ag" #-}
                              Array.array (0,_nontsIvcount-1) (toList _nontsIrules)
                              {-# LINE 971 "dist/build/Order.hs"#-}
   {-# INLINE rule87 #-}
   {-# LINE 547 "./src-ag/Order.ag" #-}
   rule87 = \ ((_nontsIacount) :: Int) ((_nontsIntattrs) :: Seq (Vertex,NTAttr)) ->
                              {-# LINE 547 "./src-ag/Order.ag" #-}
                              Array.array (0,_nontsIacount-1) (toList _nontsIntattrs)
                              {-# LINE 977 "dist/build/Order.hs"#-}
   {-# INLINE rule88 #-}
   {-# LINE 548 "./src-ag/Order.ag" #-}
   rule88 = \ ((_nontsIntattrs) :: Seq (Vertex,NTAttr)) ->
                               {-# LINE 548 "./src-ag/Order.ag" #-}
                               Map.fromList (map swap (toList _nontsIntattrs))
                               {-# LINE 983 "dist/build/Order.hs"#-}
   {-# INLINE rule89 #-}
   {-# LINE 549 "./src-ag/Order.ag" #-}
   rule89 = \ _attrVertex ((_nontsIrules) :: Seq (Vertex,CRule)) ->
                              {-# LINE 549 "./src-ag/Order.ag" #-}
                              [ (s, maybe (-1) (\v -> findWithErr1 "Grammar.tdpToTds" v _attrVertex) (ntattr cr))
                              | (s,cr) <- toList _nontsIrules]
                              {-# LINE 990 "dist/build/Order.hs"#-}
   {-# INLINE rule90 #-}
   {-# LINE 551 "./src-ag/Order.ag" #-}
   rule90 = \ _tdpToTds ->
                               {-# LINE 551 "./src-ag/Order.ag" #-}
                               let  eq (_,v) (_,v') = v == v'
                                    conv ((s,v):svs)  | v == -1 = Nothing
                                                      | otherwise = Just (v,s:map fst svs)
                               in mapMaybe conv (eqClasses eq _tdpToTds)
                               {-# LINE 999 "dist/build/Order.hs"#-}
   {-# INLINE rule91 #-}
   {-# LINE 555 "./src-ag/Order.ag" #-}
   rule91 = \ ((_nontsIadditionalDep) :: Seq Edge) ((_nontsIdirectDep) :: Seq Edge) ->
                              {-# LINE 555 "./src-ag/Order.ag" #-}
                              toList (_nontsIdirectDep Seq.>< _nontsIadditionalDep)
                              {-# LINE 1005 "dist/build/Order.hs"#-}
   {-# INLINE rule92 #-}
   {-# LINE 556 "./src-ag/Order.ag" #-}
   rule92 = \ ((_nontsIinstDep) :: Seq Edge) ->
                              {-# LINE 556 "./src-ag/Order.ag" #-}
                              toList _nontsIinstDep
                              {-# LINE 1011 "dist/build/Order.hs"#-}
   {-# INLINE rule93 #-}
   {-# LINE 557 "./src-ag/Order.ag" #-}
   rule93 = \ ((_nontsIaroundDep) :: Seq Edge) ->
                              {-# LINE 557 "./src-ag/Order.ag" #-}
                              toList _nontsIaroundDep
                              {-# LINE 1017 "dist/build/Order.hs"#-}
   {-# INLINE rule94 #-}
   {-# LINE 558 "./src-ag/Order.ag" #-}
   rule94 = \ ((_nontsImergeDep) :: Seq Edge) ->
                              {-# LINE 558 "./src-ag/Order.ag" #-}
                              toList _nontsImergeDep
                              {-# LINE 1023 "dist/build/Order.hs"#-}
   {-# INLINE rule95 #-}
   {-# LINE 559 "./src-ag/Order.ag" #-}
   rule95 = \ _attrTable ((_nontsIacount) :: Int) ((_nontsIaranges) :: Seq (Int,Int,Int)) ((_nontsInonts) :: [(NontermIdent,[ConstructorIdent])]) ((_nontsIvcount) :: Int) _ruleTable _tdpToTds _tdsToTdp wrappers_ ->
                              {-# LINE 559 "./src-ag/Order.ag" #-}
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
                              {-# LINE 1038 "dist/build/Order.hs"#-}
   {-# INLINE rule96 #-}
   {-# LINE 571 "./src-ag/Order.ag" #-}
   rule96 = \ _aroundDep _attrTable _directDep _info _instDep ((_lhsIoptions) :: Options) _mergeDep _ruleTable ->
                                {-# LINE 571 "./src-ag/Order.ag" #-}
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
                                {-# LINE 1064 "dist/build/Order.hs"#-}
   {-# INLINE rule97 #-}
   {-# LINE 592 "./src-ag/Order.ag" #-}
   rule97 = \ _cyclesErrors ((_lhsIoptions) :: Options) ((_nontsIerrors) :: Seq Error) ->
                           {-# LINE 592 "./src-ag/Order.ag" #-}
                           (if withCycle _lhsIoptions then Seq.fromList _cyclesErrors else Seq.empty)
                            Seq.>< _nontsIerrors
                           {-# LINE 1071 "dist/build/Order.hs"#-}
   {-# INLINE rule98 #-}
   {-# LINE 624 "./src-ag/Order.ag" #-}
   rule98 = \ _aroundMap _mergeMap ((_nontsIcNonterminals) :: CNonterminals) _o_dovisit contextMap_ derivings_ paramMap_ pragmas_ quantMap_ typeSyns_ wrappers_ ->
                             {-# LINE 624 "./src-ag/Order.ag" #-}
                             CGrammar typeSyns_ derivings_ wrappers_ _nontsIcNonterminals pragmas_ paramMap_ contextMap_ quantMap_ _aroundMap     _mergeMap     _o_dovisit
                             {-# LINE 1077 "dist/build/Order.hs"#-}
   {-# INLINE rule99 #-}
   {-# LINE 637 "./src-ag/Order.ag" #-}
   rule99 = \ aroundsMap_ ->
                               {-# LINE 637 "./src-ag/Order.ag" #-}
                               Map.map (Map.map Map.keysSet) aroundsMap_
                               {-# LINE 1083 "dist/build/Order.hs"#-}
   {-# INLINE rule100 #-}
   {-# LINE 638 "./src-ag/Order.ag" #-}
   rule100 = \ mergeMap_ ->
                               {-# LINE 638 "./src-ag/Order.ag" #-}
                               Map.map (Map.map (Map.map (\(nt,srcs,_) -> (nt,srcs)))) mergeMap_
                               {-# LINE 1089 "dist/build/Order.hs"#-}
   {-# INLINE rule101 #-}
   {-# LINE 655 "./src-ag/Order.ag" #-}
   rule101 = \ ((_nontsInonts) :: [(NontermIdent,[ConstructorIdent])]) ->
                             {-# LINE 655 "./src-ag/Order.ag" #-}
                             map fst (_nontsInonts)
                             {-# LINE 1095 "dist/build/Order.hs"#-}
   {-# INLINE rule102 #-}
   rule102 = \ ((_nontsInAutoRules) :: Int) ->
     _nontsInAutoRules
   {-# INLINE rule103 #-}
   rule103 = \ ((_nontsInExplicitRules) :: Int) ->
     _nontsInExplicitRules
   {-# INLINE rule104 #-}
   rule104 = \ _cInterfaceMap ->
     _cInterfaceMap
   {-# INLINE rule105 #-}
   rule105 = \ _cVisitsMap ->
     _cVisitsMap
   {-# INLINE rule106 #-}
   rule106 = \ _mergeMap ->
     _mergeMap
   {-# INLINE rule107 #-}
   rule107 = \ _o_dovisit ->
     _o_dovisit
   {-# INLINE rule108 #-}
   rule108 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions

-- Nonterminal -------------------------------------------------
-- wrapper
data Inh_Nonterminal  = Inh_Nonterminal { acount_Inh_Nonterminal :: (Int), allnts_Inh_Nonterminal :: ([Identifier]), aroundMap_Inh_Nonterminal :: (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))), cInterfaceMap_Inh_Nonterminal :: (CInterfaceMap), cVisitsMap_Inh_Nonterminal :: (CVisitsMap), inhMap_Inh_Nonterminal :: (Map Identifier Attributes), manualAttrDepMap_Inh_Nonterminal :: (AttrOrderMap), mergeMap_Inh_Nonterminal :: (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier])))), o_case_Inh_Nonterminal :: (Bool), o_cata_Inh_Nonterminal :: (Bool), o_data_Inh_Nonterminal :: (Bool), o_dovisit_Inh_Nonterminal :: (Bool), o_newtypes_Inh_Nonterminal :: (Bool), o_rename_Inh_Nonterminal :: (Bool), o_sem_Inh_Nonterminal :: (Bool), o_sig_Inh_Nonterminal :: (Bool), o_unbox_Inh_Nonterminal :: (Bool), o_wantvisit_Inh_Nonterminal :: (Bool), options_Inh_Nonterminal :: (Options), prefix_Inh_Nonterminal :: (String), synMap_Inh_Nonterminal :: (Map Identifier Attributes), vcount_Inh_Nonterminal :: (Int) }
data Syn_Nonterminal  = Syn_Nonterminal { acount_Syn_Nonterminal :: (Int), additionalDep_Syn_Nonterminal :: (Seq Edge), aranges_Syn_Nonterminal :: (Seq (Int,Int,Int)), aroundDep_Syn_Nonterminal :: (Seq Edge), cNonterminal_Syn_Nonterminal :: (CNonterminal), directDep_Syn_Nonterminal :: (Seq Edge), errors_Syn_Nonterminal :: (Seq Error), inhMap'_Syn_Nonterminal :: (Map Identifier Attributes), instDep_Syn_Nonterminal :: (Seq Edge), mergeDep_Syn_Nonterminal :: (Seq Edge), nAutoRules_Syn_Nonterminal :: (Int), nExplicitRules_Syn_Nonterminal :: (Int), nonts_Syn_Nonterminal :: ([(NontermIdent,[ConstructorIdent])]), ntattrs_Syn_Nonterminal :: (Seq (Vertex,NTAttr)), rules_Syn_Nonterminal :: (Seq (Vertex,CRule)), synMap'_Syn_Nonterminal :: (Map Identifier Attributes), vcount_Syn_Nonterminal :: (Int) }
{-# INLINABLE wrap_Nonterminal #-}
wrap_Nonterminal :: T_Nonterminal  -> Inh_Nonterminal  -> (Syn_Nonterminal )
wrap_Nonterminal (T_Nonterminal act) (Inh_Nonterminal _lhsIacount _lhsIallnts _lhsIaroundMap _lhsIcInterfaceMap _lhsIcVisitsMap _lhsIinhMap _lhsImanualAttrDepMap _lhsImergeMap _lhsIo_case _lhsIo_cata _lhsIo_data _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_unbox _lhsIo_wantvisit _lhsIoptions _lhsIprefix _lhsIsynMap _lhsIvcount) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Nonterminal_vIn13 _lhsIacount _lhsIallnts _lhsIaroundMap _lhsIcInterfaceMap _lhsIcVisitsMap _lhsIinhMap _lhsImanualAttrDepMap _lhsImergeMap _lhsIo_case _lhsIo_cata _lhsIo_data _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_unbox _lhsIo_wantvisit _lhsIoptions _lhsIprefix _lhsIsynMap _lhsIvcount
        (T_Nonterminal_vOut13 _lhsOacount _lhsOadditionalDep _lhsOaranges _lhsOaroundDep _lhsOcNonterminal _lhsOdirectDep _lhsOerrors _lhsOinhMap' _lhsOinstDep _lhsOmergeDep _lhsOnAutoRules _lhsOnExplicitRules _lhsOnonts _lhsOntattrs _lhsOrules _lhsOsynMap' _lhsOvcount) <- return (inv_Nonterminal_s14 sem arg)
        return (Syn_Nonterminal _lhsOacount _lhsOadditionalDep _lhsOaranges _lhsOaroundDep _lhsOcNonterminal _lhsOdirectDep _lhsOerrors _lhsOinhMap' _lhsOinstDep _lhsOmergeDep _lhsOnAutoRules _lhsOnExplicitRules _lhsOnonts _lhsOntattrs _lhsOrules _lhsOsynMap' _lhsOvcount)
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
data T_Nonterminal_vOut13  = T_Nonterminal_vOut13 (Int) (Seq Edge) (Seq (Int,Int,Int)) (Seq Edge) (CNonterminal) (Seq Edge) (Seq Error) (Map Identifier Attributes) (Seq Edge) (Seq Edge) (Int) (Int) ([(NontermIdent,[ConstructorIdent])]) (Seq (Vertex,NTAttr)) (Seq (Vertex,CRule)) (Map Identifier Attributes) (Int)
{-# NOINLINE sem_Nonterminal_Nonterminal #-}
sem_Nonterminal_Nonterminal :: (NontermIdent) -> ([Identifier]) -> (Attributes) -> (Attributes) -> T_Productions  -> T_Nonterminal 
sem_Nonterminal_Nonterminal arg_nt_ arg_params_ arg_inh_ arg_syn_ arg_prods_ = T_Nonterminal (return st14) where
   {-# NOINLINE st14 #-}
   st14 = let
      v13 :: T_Nonterminal_v13 
      v13 = \ (T_Nonterminal_vIn13 _lhsIacount _lhsIallnts _lhsIaroundMap _lhsIcInterfaceMap _lhsIcVisitsMap _lhsIinhMap _lhsImanualAttrDepMap _lhsImergeMap _lhsIo_case _lhsIo_cata _lhsIo_data _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_unbox _lhsIo_wantvisit _lhsIoptions _lhsIprefix _lhsIsynMap _lhsIvcount) -> ( let
         _prodsX29 = Control.Monad.Identity.runIdentity (attach_T_Productions (arg_prods_))
         (T_Productions_vOut28 _prodsIadditionalDep _prodsIaroundDep _prodsIcProductions _prodsIcons _prodsIdirectDep _prodsIerrors _prodsIinstDep _prodsImergeDep _prodsInAutoRules _prodsInExplicitRules _prodsIrules _prodsIvcount) = inv_Productions_s29 _prodsX29 (T_Productions_vIn28 _prodsOallnts _prodsOaroundMap _prodsOcVisitsMap _prodsOinh _prodsOinhMap _prodsOmanualAttrDepMap _prodsOmergeMap _prodsOnt _prodsOo_case _prodsOo_cata _prodsOo_dovisit _prodsOo_newtypes _prodsOo_rename _prodsOo_sem _prodsOo_sig _prodsOo_unbox _prodsOo_wantvisit _prodsOoptions _prodsOprefix _prodsOsyn _prodsOsynMap _prodsOvcount)
         _lhsOinhMap' :: Map Identifier Attributes
         _lhsOinhMap' = rule109 arg_inh_ arg_nt_
         _lhsOsynMap' :: Map Identifier Attributes
         _lhsOsynMap' = rule110 arg_nt_ arg_syn_
         _prodsOnt = rule111 arg_nt_
         _prodsOinh = rule112 arg_inh_
         _prodsOsyn = rule113 arg_syn_
         _mergeMap = rule114 _lhsImergeMap arg_nt_
         _aroundMap = rule115 _lhsIaroundMap arg_nt_
         _ntattrs = rule116 arg_inh_ arg_nt_ arg_syn_
         _lhsOntattrs :: Seq (Vertex,NTAttr)
         _lhsOntattrs = rule117 _lhsIacount _ntattrs
         _lhsOacount :: Int
         _lhsOacount = rule118 _lhsIacount arg_inh_ arg_syn_
         _lhsOaranges :: Seq (Int,Int,Int)
         _lhsOaranges = rule119 _lhsIacount arg_inh_ arg_syn_
         _lhsOnonts :: [(NontermIdent,[ConstructorIdent])]
         _lhsOnonts = rule120 _prodsIcons arg_nt_
         _cInter = rule121 _lhsIcInterfaceMap _lhsIo_dovisit arg_inh_ arg_nt_ arg_syn_
         _lhsOcNonterminal :: CNonterminal
         _lhsOcNonterminal = rule122 _cInter _prodsIcProductions arg_inh_ arg_nt_ arg_params_ arg_syn_
         _lhsOadditionalDep :: Seq Edge
         _lhsOadditionalDep = rule123 _prodsIadditionalDep
         _lhsOaroundDep :: Seq Edge
         _lhsOaroundDep = rule124 _prodsIaroundDep
         _lhsOdirectDep :: Seq Edge
         _lhsOdirectDep = rule125 _prodsIdirectDep
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule126 _prodsIerrors
         _lhsOinstDep :: Seq Edge
         _lhsOinstDep = rule127 _prodsIinstDep
         _lhsOmergeDep :: Seq Edge
         _lhsOmergeDep = rule128 _prodsImergeDep
         _lhsOnAutoRules :: Int
         _lhsOnAutoRules = rule129 _prodsInAutoRules
         _lhsOnExplicitRules :: Int
         _lhsOnExplicitRules = rule130 _prodsInExplicitRules
         _lhsOrules :: Seq (Vertex,CRule)
         _lhsOrules = rule131 _prodsIrules
         _lhsOvcount :: Int
         _lhsOvcount = rule132 _prodsIvcount
         _prodsOallnts = rule133 _lhsIallnts
         _prodsOaroundMap = rule134 _aroundMap
         _prodsOcVisitsMap = rule135 _lhsIcVisitsMap
         _prodsOinhMap = rule136 _lhsIinhMap
         _prodsOmanualAttrDepMap = rule137 _lhsImanualAttrDepMap
         _prodsOmergeMap = rule138 _mergeMap
         _prodsOo_case = rule139 _lhsIo_case
         _prodsOo_cata = rule140 _lhsIo_cata
         _prodsOo_dovisit = rule141 _lhsIo_dovisit
         _prodsOo_newtypes = rule142 _lhsIo_newtypes
         _prodsOo_rename = rule143 _lhsIo_rename
         _prodsOo_sem = rule144 _lhsIo_sem
         _prodsOo_sig = rule145 _lhsIo_sig
         _prodsOo_unbox = rule146 _lhsIo_unbox
         _prodsOo_wantvisit = rule147 _lhsIo_wantvisit
         _prodsOoptions = rule148 _lhsIoptions
         _prodsOprefix = rule149 _lhsIprefix
         _prodsOsynMap = rule150 _lhsIsynMap
         _prodsOvcount = rule151 _lhsIvcount
         __result_ = T_Nonterminal_vOut13 _lhsOacount _lhsOadditionalDep _lhsOaranges _lhsOaroundDep _lhsOcNonterminal _lhsOdirectDep _lhsOerrors _lhsOinhMap' _lhsOinstDep _lhsOmergeDep _lhsOnAutoRules _lhsOnExplicitRules _lhsOnonts _lhsOntattrs _lhsOrules _lhsOsynMap' _lhsOvcount
         in __result_ )
     in C_Nonterminal_s14 v13
   {-# INLINE rule109 #-}
   {-# LINE 7 "./src-ag/DistChildAttr.ag" #-}
   rule109 = \ inh_ nt_ ->
                                 {-# LINE 7 "./src-ag/DistChildAttr.ag" #-}
                                 Map.singleton nt_ inh_
                                 {-# LINE 1225 "dist/build/Order.hs"#-}
   {-# INLINE rule110 #-}
   {-# LINE 8 "./src-ag/DistChildAttr.ag" #-}
   rule110 = \ nt_ syn_ ->
                                 {-# LINE 8 "./src-ag/DistChildAttr.ag" #-}
                                 Map.singleton nt_ syn_
                                 {-# LINE 1231 "dist/build/Order.hs"#-}
   {-# INLINE rule111 #-}
   {-# LINE 97 "./src-ag/Order.ag" #-}
   rule111 = \ nt_ ->
                               {-# LINE 97 "./src-ag/Order.ag" #-}
                               nt_
                               {-# LINE 1237 "dist/build/Order.hs"#-}
   {-# INLINE rule112 #-}
   {-# LINE 100 "./src-ag/Order.ag" #-}
   rule112 = \ inh_ ->
                               {-# LINE 100 "./src-ag/Order.ag" #-}
                               inh_
                               {-# LINE 1243 "dist/build/Order.hs"#-}
   {-# INLINE rule113 #-}
   {-# LINE 101 "./src-ag/Order.ag" #-}
   rule113 = \ syn_ ->
                               {-# LINE 101 "./src-ag/Order.ag" #-}
                               syn_
                               {-# LINE 1249 "dist/build/Order.hs"#-}
   {-# INLINE rule114 #-}
   {-# LINE 360 "./src-ag/Order.ag" #-}
   rule114 = \ ((_lhsImergeMap) :: Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier])))) nt_ ->
                                                {-# LINE 360 "./src-ag/Order.ag" #-}
                                                Map.findWithDefault Map.empty nt_ _lhsImergeMap
                                                {-# LINE 1255 "dist/build/Order.hs"#-}
   {-# INLINE rule115 #-}
   {-# LINE 413 "./src-ag/Order.ag" #-}
   rule115 = \ ((_lhsIaroundMap) :: Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) nt_ ->
                                                 {-# LINE 413 "./src-ag/Order.ag" #-}
                                                 Map.findWithDefault Map.empty nt_ _lhsIaroundMap
                                                 {-# LINE 1261 "dist/build/Order.hs"#-}
   {-# INLINE rule116 #-}
   {-# LINE 511 "./src-ag/Order.ag" #-}
   rule116 = \ inh_ nt_ syn_ ->
                                 {-# LINE 511 "./src-ag/Order.ag" #-}
                                 [ NTAInh nt_ inh tp | (inh,tp) <- Map.assocs inh_ ]
                                 ++ [NTASyn nt_ syn tp | (syn,tp) <- Map.assocs syn_ ]
                                 {-# LINE 1268 "dist/build/Order.hs"#-}
   {-# INLINE rule117 #-}
   {-# LINE 513 "./src-ag/Order.ag" #-}
   rule117 = \ ((_lhsIacount) :: Int) _ntattrs ->
                                {-# LINE 513 "./src-ag/Order.ag" #-}
                                Seq.fromList (zip [_lhsIacount ..] _ntattrs)
                                {-# LINE 1274 "dist/build/Order.hs"#-}
   {-# INLINE rule118 #-}
   {-# LINE 514 "./src-ag/Order.ag" #-}
   rule118 = \ ((_lhsIacount) :: Int) inh_ syn_ ->
                                {-# LINE 514 "./src-ag/Order.ag" #-}
                                _lhsIacount + Map.size inh_ + Map.size syn_
                                {-# LINE 1280 "dist/build/Order.hs"#-}
   {-# INLINE rule119 #-}
   {-# LINE 515 "./src-ag/Order.ag" #-}
   rule119 = \ ((_lhsIacount) :: Int) inh_ syn_ ->
                                 {-# LINE 515 "./src-ag/Order.ag" #-}
                                 Seq.singleton
                                  (_lhsIacount
                                  ,_lhsIacount + Map.size inh_
                                  ,_lhsIacount + Map.size syn_ + Map.size inh_ - 1)
                                 {-# LINE 1289 "dist/build/Order.hs"#-}
   {-# INLINE rule120 #-}
   {-# LINE 524 "./src-ag/Order.ag" #-}
   rule120 = \ ((_prodsIcons) :: [ConstructorIdent]) nt_ ->
                                {-# LINE 524 "./src-ag/Order.ag" #-}
                                [(nt_,_prodsIcons)]
                                {-# LINE 1295 "dist/build/Order.hs"#-}
   {-# INLINE rule121 #-}
   {-# LINE 601 "./src-ag/Order.ag" #-}
   rule121 = \ ((_lhsIcInterfaceMap) :: CInterfaceMap) ((_lhsIo_dovisit) :: Bool) inh_ nt_ syn_ ->
                                 {-# LINE 601 "./src-ag/Order.ag" #-}
                                 if  _lhsIo_dovisit
                                        then findWithErr1 "Nonterminal.cInter" nt_ _lhsIcInterfaceMap
                                        else CInterface [CSegment inh_ syn_]
                                 {-# LINE 1303 "dist/build/Order.hs"#-}
   {-# INLINE rule122 #-}
   {-# LINE 629 "./src-ag/Order.ag" #-}
   rule122 = \ _cInter ((_prodsIcProductions) :: CProductions) inh_ nt_ params_ syn_ ->
                                       {-# LINE 629 "./src-ag/Order.ag" #-}
                                       CNonterminal nt_ params_ inh_ syn_ _prodsIcProductions _cInter
                                       {-# LINE 1309 "dist/build/Order.hs"#-}
   {-# INLINE rule123 #-}
   rule123 = \ ((_prodsIadditionalDep) :: Seq Edge) ->
     _prodsIadditionalDep
   {-# INLINE rule124 #-}
   rule124 = \ ((_prodsIaroundDep) :: Seq Edge) ->
     _prodsIaroundDep
   {-# INLINE rule125 #-}
   rule125 = \ ((_prodsIdirectDep) :: Seq Edge) ->
     _prodsIdirectDep
   {-# INLINE rule126 #-}
   rule126 = \ ((_prodsIerrors) :: Seq Error) ->
     _prodsIerrors
   {-# INLINE rule127 #-}
   rule127 = \ ((_prodsIinstDep) :: Seq Edge) ->
     _prodsIinstDep
   {-# INLINE rule128 #-}
   rule128 = \ ((_prodsImergeDep) :: Seq Edge) ->
     _prodsImergeDep
   {-# INLINE rule129 #-}
   rule129 = \ ((_prodsInAutoRules) :: Int) ->
     _prodsInAutoRules
   {-# INLINE rule130 #-}
   rule130 = \ ((_prodsInExplicitRules) :: Int) ->
     _prodsInExplicitRules
   {-# INLINE rule131 #-}
   rule131 = \ ((_prodsIrules) :: Seq (Vertex,CRule)) ->
     _prodsIrules
   {-# INLINE rule132 #-}
   rule132 = \ ((_prodsIvcount) :: Int) ->
     _prodsIvcount
   {-# INLINE rule133 #-}
   rule133 = \ ((_lhsIallnts) :: [Identifier]) ->
     _lhsIallnts
   {-# INLINE rule134 #-}
   rule134 = \ _aroundMap ->
     _aroundMap
   {-# INLINE rule135 #-}
   rule135 = \ ((_lhsIcVisitsMap) :: CVisitsMap) ->
     _lhsIcVisitsMap
   {-# INLINE rule136 #-}
   rule136 = \ ((_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# INLINE rule137 #-}
   rule137 = \ ((_lhsImanualAttrDepMap) :: AttrOrderMap) ->
     _lhsImanualAttrDepMap
   {-# INLINE rule138 #-}
   rule138 = \ _mergeMap ->
     _mergeMap
   {-# INLINE rule139 #-}
   rule139 = \ ((_lhsIo_case) :: Bool) ->
     _lhsIo_case
   {-# INLINE rule140 #-}
   rule140 = \ ((_lhsIo_cata) :: Bool) ->
     _lhsIo_cata
   {-# INLINE rule141 #-}
   rule141 = \ ((_lhsIo_dovisit) :: Bool) ->
     _lhsIo_dovisit
   {-# INLINE rule142 #-}
   rule142 = \ ((_lhsIo_newtypes) :: Bool) ->
     _lhsIo_newtypes
   {-# INLINE rule143 #-}
   rule143 = \ ((_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# INLINE rule144 #-}
   rule144 = \ ((_lhsIo_sem) :: Bool) ->
     _lhsIo_sem
   {-# INLINE rule145 #-}
   rule145 = \ ((_lhsIo_sig) :: Bool) ->
     _lhsIo_sig
   {-# INLINE rule146 #-}
   rule146 = \ ((_lhsIo_unbox) :: Bool) ->
     _lhsIo_unbox
   {-# INLINE rule147 #-}
   rule147 = \ ((_lhsIo_wantvisit) :: Bool) ->
     _lhsIo_wantvisit
   {-# INLINE rule148 #-}
   rule148 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule149 #-}
   rule149 = \ ((_lhsIprefix) :: String) ->
     _lhsIprefix
   {-# INLINE rule150 #-}
   rule150 = \ ((_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
   {-# INLINE rule151 #-}
   rule151 = \ ((_lhsIvcount) :: Int) ->
     _lhsIvcount

-- Nonterminals ------------------------------------------------
-- wrapper
data Inh_Nonterminals  = Inh_Nonterminals { acount_Inh_Nonterminals :: (Int), allnts_Inh_Nonterminals :: ([Identifier]), aroundMap_Inh_Nonterminals :: (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))), cInterfaceMap_Inh_Nonterminals :: (CInterfaceMap), cVisitsMap_Inh_Nonterminals :: (CVisitsMap), inhMap_Inh_Nonterminals :: (Map Identifier Attributes), manualAttrDepMap_Inh_Nonterminals :: (AttrOrderMap), mergeMap_Inh_Nonterminals :: (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier])))), o_case_Inh_Nonterminals :: (Bool), o_cata_Inh_Nonterminals :: (Bool), o_data_Inh_Nonterminals :: (Bool), o_dovisit_Inh_Nonterminals :: (Bool), o_newtypes_Inh_Nonterminals :: (Bool), o_rename_Inh_Nonterminals :: (Bool), o_sem_Inh_Nonterminals :: (Bool), o_sig_Inh_Nonterminals :: (Bool), o_unbox_Inh_Nonterminals :: (Bool), o_wantvisit_Inh_Nonterminals :: (Bool), options_Inh_Nonterminals :: (Options), prefix_Inh_Nonterminals :: (String), synMap_Inh_Nonterminals :: (Map Identifier Attributes), vcount_Inh_Nonterminals :: (Int) }
data Syn_Nonterminals  = Syn_Nonterminals { acount_Syn_Nonterminals :: (Int), additionalDep_Syn_Nonterminals :: (Seq Edge), aranges_Syn_Nonterminals :: (Seq (Int,Int,Int)), aroundDep_Syn_Nonterminals :: (Seq Edge), cNonterminals_Syn_Nonterminals :: (CNonterminals), directDep_Syn_Nonterminals :: (Seq Edge), errors_Syn_Nonterminals :: (Seq Error), inhMap'_Syn_Nonterminals :: (Map Identifier Attributes), instDep_Syn_Nonterminals :: (Seq Edge), mergeDep_Syn_Nonterminals :: (Seq Edge), nAutoRules_Syn_Nonterminals :: (Int), nExplicitRules_Syn_Nonterminals :: (Int), nonts_Syn_Nonterminals :: ([(NontermIdent,[ConstructorIdent])]), ntattrs_Syn_Nonterminals :: (Seq (Vertex,NTAttr)), rules_Syn_Nonterminals :: (Seq (Vertex,CRule)), synMap'_Syn_Nonterminals :: (Map Identifier Attributes), vcount_Syn_Nonterminals :: (Int) }
{-# INLINABLE wrap_Nonterminals #-}
wrap_Nonterminals :: T_Nonterminals  -> Inh_Nonterminals  -> (Syn_Nonterminals )
wrap_Nonterminals (T_Nonterminals act) (Inh_Nonterminals _lhsIacount _lhsIallnts _lhsIaroundMap _lhsIcInterfaceMap _lhsIcVisitsMap _lhsIinhMap _lhsImanualAttrDepMap _lhsImergeMap _lhsIo_case _lhsIo_cata _lhsIo_data _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_unbox _lhsIo_wantvisit _lhsIoptions _lhsIprefix _lhsIsynMap _lhsIvcount) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Nonterminals_vIn16 _lhsIacount _lhsIallnts _lhsIaroundMap _lhsIcInterfaceMap _lhsIcVisitsMap _lhsIinhMap _lhsImanualAttrDepMap _lhsImergeMap _lhsIo_case _lhsIo_cata _lhsIo_data _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_unbox _lhsIo_wantvisit _lhsIoptions _lhsIprefix _lhsIsynMap _lhsIvcount
        (T_Nonterminals_vOut16 _lhsOacount _lhsOadditionalDep _lhsOaranges _lhsOaroundDep _lhsOcNonterminals _lhsOdirectDep _lhsOerrors _lhsOinhMap' _lhsOinstDep _lhsOmergeDep _lhsOnAutoRules _lhsOnExplicitRules _lhsOnonts _lhsOntattrs _lhsOrules _lhsOsynMap' _lhsOvcount) <- return (inv_Nonterminals_s17 sem arg)
        return (Syn_Nonterminals _lhsOacount _lhsOadditionalDep _lhsOaranges _lhsOaroundDep _lhsOcNonterminals _lhsOdirectDep _lhsOerrors _lhsOinhMap' _lhsOinstDep _lhsOmergeDep _lhsOnAutoRules _lhsOnExplicitRules _lhsOnonts _lhsOntattrs _lhsOrules _lhsOsynMap' _lhsOvcount)
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
data T_Nonterminals_vOut16  = T_Nonterminals_vOut16 (Int) (Seq Edge) (Seq (Int,Int,Int)) (Seq Edge) (CNonterminals) (Seq Edge) (Seq Error) (Map Identifier Attributes) (Seq Edge) (Seq Edge) (Int) (Int) ([(NontermIdent,[ConstructorIdent])]) (Seq (Vertex,NTAttr)) (Seq (Vertex,CRule)) (Map Identifier Attributes) (Int)
{-# NOINLINE sem_Nonterminals_Cons #-}
sem_Nonterminals_Cons :: T_Nonterminal  -> T_Nonterminals  -> T_Nonterminals 
sem_Nonterminals_Cons arg_hd_ arg_tl_ = T_Nonterminals (return st17) where
   {-# NOINLINE st17 #-}
   st17 = let
      v16 :: T_Nonterminals_v16 
      v16 = \ (T_Nonterminals_vIn16 _lhsIacount _lhsIallnts _lhsIaroundMap _lhsIcInterfaceMap _lhsIcVisitsMap _lhsIinhMap _lhsImanualAttrDepMap _lhsImergeMap _lhsIo_case _lhsIo_cata _lhsIo_data _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_unbox _lhsIo_wantvisit _lhsIoptions _lhsIprefix _lhsIsynMap _lhsIvcount) -> ( let
         _hdX14 = Control.Monad.Identity.runIdentity (attach_T_Nonterminal (arg_hd_))
         _tlX17 = Control.Monad.Identity.runIdentity (attach_T_Nonterminals (arg_tl_))
         (T_Nonterminal_vOut13 _hdIacount _hdIadditionalDep _hdIaranges _hdIaroundDep _hdIcNonterminal _hdIdirectDep _hdIerrors _hdIinhMap' _hdIinstDep _hdImergeDep _hdInAutoRules _hdInExplicitRules _hdInonts _hdIntattrs _hdIrules _hdIsynMap' _hdIvcount) = inv_Nonterminal_s14 _hdX14 (T_Nonterminal_vIn13 _hdOacount _hdOallnts _hdOaroundMap _hdOcInterfaceMap _hdOcVisitsMap _hdOinhMap _hdOmanualAttrDepMap _hdOmergeMap _hdOo_case _hdOo_cata _hdOo_data _hdOo_dovisit _hdOo_newtypes _hdOo_rename _hdOo_sem _hdOo_sig _hdOo_unbox _hdOo_wantvisit _hdOoptions _hdOprefix _hdOsynMap _hdOvcount)
         (T_Nonterminals_vOut16 _tlIacount _tlIadditionalDep _tlIaranges _tlIaroundDep _tlIcNonterminals _tlIdirectDep _tlIerrors _tlIinhMap' _tlIinstDep _tlImergeDep _tlInAutoRules _tlInExplicitRules _tlInonts _tlIntattrs _tlIrules _tlIsynMap' _tlIvcount) = inv_Nonterminals_s17 _tlX17 (T_Nonterminals_vIn16 _tlOacount _tlOallnts _tlOaroundMap _tlOcInterfaceMap _tlOcVisitsMap _tlOinhMap _tlOmanualAttrDepMap _tlOmergeMap _tlOo_case _tlOo_cata _tlOo_data _tlOo_dovisit _tlOo_newtypes _tlOo_rename _tlOo_sem _tlOo_sig _tlOo_unbox _tlOo_wantvisit _tlOoptions _tlOprefix _tlOsynMap _tlOvcount)
         _lhsOcNonterminals :: CNonterminals
         _lhsOcNonterminals = rule152 _hdIcNonterminal _tlIcNonterminals
         _lhsOadditionalDep :: Seq Edge
         _lhsOadditionalDep = rule153 _hdIadditionalDep _tlIadditionalDep
         _lhsOaranges :: Seq (Int,Int,Int)
         _lhsOaranges = rule154 _hdIaranges _tlIaranges
         _lhsOaroundDep :: Seq Edge
         _lhsOaroundDep = rule155 _hdIaroundDep _tlIaroundDep
         _lhsOdirectDep :: Seq Edge
         _lhsOdirectDep = rule156 _hdIdirectDep _tlIdirectDep
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule157 _hdIerrors _tlIerrors
         _lhsOinhMap' :: Map Identifier Attributes
         _lhsOinhMap' = rule158 _hdIinhMap' _tlIinhMap'
         _lhsOinstDep :: Seq Edge
         _lhsOinstDep = rule159 _hdIinstDep _tlIinstDep
         _lhsOmergeDep :: Seq Edge
         _lhsOmergeDep = rule160 _hdImergeDep _tlImergeDep
         _lhsOnAutoRules :: Int
         _lhsOnAutoRules = rule161 _hdInAutoRules _tlInAutoRules
         _lhsOnExplicitRules :: Int
         _lhsOnExplicitRules = rule162 _hdInExplicitRules _tlInExplicitRules
         _lhsOnonts :: [(NontermIdent,[ConstructorIdent])]
         _lhsOnonts = rule163 _hdInonts _tlInonts
         _lhsOntattrs :: Seq (Vertex,NTAttr)
         _lhsOntattrs = rule164 _hdIntattrs _tlIntattrs
         _lhsOrules :: Seq (Vertex,CRule)
         _lhsOrules = rule165 _hdIrules _tlIrules
         _lhsOsynMap' :: Map Identifier Attributes
         _lhsOsynMap' = rule166 _hdIsynMap' _tlIsynMap'
         _lhsOacount :: Int
         _lhsOacount = rule167 _tlIacount
         _lhsOvcount :: Int
         _lhsOvcount = rule168 _tlIvcount
         _hdOacount = rule169 _lhsIacount
         _hdOallnts = rule170 _lhsIallnts
         _hdOaroundMap = rule171 _lhsIaroundMap
         _hdOcInterfaceMap = rule172 _lhsIcInterfaceMap
         _hdOcVisitsMap = rule173 _lhsIcVisitsMap
         _hdOinhMap = rule174 _lhsIinhMap
         _hdOmanualAttrDepMap = rule175 _lhsImanualAttrDepMap
         _hdOmergeMap = rule176 _lhsImergeMap
         _hdOo_case = rule177 _lhsIo_case
         _hdOo_cata = rule178 _lhsIo_cata
         _hdOo_data = rule179 _lhsIo_data
         _hdOo_dovisit = rule180 _lhsIo_dovisit
         _hdOo_newtypes = rule181 _lhsIo_newtypes
         _hdOo_rename = rule182 _lhsIo_rename
         _hdOo_sem = rule183 _lhsIo_sem
         _hdOo_sig = rule184 _lhsIo_sig
         _hdOo_unbox = rule185 _lhsIo_unbox
         _hdOo_wantvisit = rule186 _lhsIo_wantvisit
         _hdOoptions = rule187 _lhsIoptions
         _hdOprefix = rule188 _lhsIprefix
         _hdOsynMap = rule189 _lhsIsynMap
         _hdOvcount = rule190 _lhsIvcount
         _tlOacount = rule191 _hdIacount
         _tlOallnts = rule192 _lhsIallnts
         _tlOaroundMap = rule193 _lhsIaroundMap
         _tlOcInterfaceMap = rule194 _lhsIcInterfaceMap
         _tlOcVisitsMap = rule195 _lhsIcVisitsMap
         _tlOinhMap = rule196 _lhsIinhMap
         _tlOmanualAttrDepMap = rule197 _lhsImanualAttrDepMap
         _tlOmergeMap = rule198 _lhsImergeMap
         _tlOo_case = rule199 _lhsIo_case
         _tlOo_cata = rule200 _lhsIo_cata
         _tlOo_data = rule201 _lhsIo_data
         _tlOo_dovisit = rule202 _lhsIo_dovisit
         _tlOo_newtypes = rule203 _lhsIo_newtypes
         _tlOo_rename = rule204 _lhsIo_rename
         _tlOo_sem = rule205 _lhsIo_sem
         _tlOo_sig = rule206 _lhsIo_sig
         _tlOo_unbox = rule207 _lhsIo_unbox
         _tlOo_wantvisit = rule208 _lhsIo_wantvisit
         _tlOoptions = rule209 _lhsIoptions
         _tlOprefix = rule210 _lhsIprefix
         _tlOsynMap = rule211 _lhsIsynMap
         _tlOvcount = rule212 _hdIvcount
         __result_ = T_Nonterminals_vOut16 _lhsOacount _lhsOadditionalDep _lhsOaranges _lhsOaroundDep _lhsOcNonterminals _lhsOdirectDep _lhsOerrors _lhsOinhMap' _lhsOinstDep _lhsOmergeDep _lhsOnAutoRules _lhsOnExplicitRules _lhsOnonts _lhsOntattrs _lhsOrules _lhsOsynMap' _lhsOvcount
         in __result_ )
     in C_Nonterminals_s17 v16
   {-# INLINE rule152 #-}
   {-# LINE 626 "./src-ag/Order.ag" #-}
   rule152 = \ ((_hdIcNonterminal) :: CNonterminal) ((_tlIcNonterminals) :: CNonterminals) ->
                                 {-# LINE 626 "./src-ag/Order.ag" #-}
                                 _hdIcNonterminal : _tlIcNonterminals
                                 {-# LINE 1525 "dist/build/Order.hs"#-}
   {-# INLINE rule153 #-}
   rule153 = \ ((_hdIadditionalDep) :: Seq Edge) ((_tlIadditionalDep) :: Seq Edge) ->
     _hdIadditionalDep Seq.>< _tlIadditionalDep
   {-# INLINE rule154 #-}
   rule154 = \ ((_hdIaranges) :: Seq (Int,Int,Int)) ((_tlIaranges) :: Seq (Int,Int,Int)) ->
     _hdIaranges Seq.>< _tlIaranges
   {-# INLINE rule155 #-}
   rule155 = \ ((_hdIaroundDep) :: Seq Edge) ((_tlIaroundDep) :: Seq Edge) ->
     _hdIaroundDep Seq.>< _tlIaroundDep
   {-# INLINE rule156 #-}
   rule156 = \ ((_hdIdirectDep) :: Seq Edge) ((_tlIdirectDep) :: Seq Edge) ->
     _hdIdirectDep Seq.>< _tlIdirectDep
   {-# INLINE rule157 #-}
   rule157 = \ ((_hdIerrors) :: Seq Error) ((_tlIerrors) :: Seq Error) ->
     _hdIerrors Seq.>< _tlIerrors
   {-# INLINE rule158 #-}
   rule158 = \ ((_hdIinhMap') :: Map Identifier Attributes) ((_tlIinhMap') :: Map Identifier Attributes) ->
     _hdIinhMap' `Map.union` _tlIinhMap'
   {-# INLINE rule159 #-}
   rule159 = \ ((_hdIinstDep) :: Seq Edge) ((_tlIinstDep) :: Seq Edge) ->
     _hdIinstDep Seq.>< _tlIinstDep
   {-# INLINE rule160 #-}
   rule160 = \ ((_hdImergeDep) :: Seq Edge) ((_tlImergeDep) :: Seq Edge) ->
     _hdImergeDep Seq.>< _tlImergeDep
   {-# INLINE rule161 #-}
   rule161 = \ ((_hdInAutoRules) :: Int) ((_tlInAutoRules) :: Int) ->
     _hdInAutoRules + _tlInAutoRules
   {-# INLINE rule162 #-}
   rule162 = \ ((_hdInExplicitRules) :: Int) ((_tlInExplicitRules) :: Int) ->
     _hdInExplicitRules + _tlInExplicitRules
   {-# INLINE rule163 #-}
   rule163 = \ ((_hdInonts) :: [(NontermIdent,[ConstructorIdent])]) ((_tlInonts) :: [(NontermIdent,[ConstructorIdent])]) ->
     _hdInonts ++ _tlInonts
   {-# INLINE rule164 #-}
   rule164 = \ ((_hdIntattrs) :: Seq (Vertex,NTAttr)) ((_tlIntattrs) :: Seq (Vertex,NTAttr)) ->
     _hdIntattrs Seq.>< _tlIntattrs
   {-# INLINE rule165 #-}
   rule165 = \ ((_hdIrules) :: Seq (Vertex,CRule)) ((_tlIrules) :: Seq (Vertex,CRule)) ->
     _hdIrules Seq.>< _tlIrules
   {-# INLINE rule166 #-}
   rule166 = \ ((_hdIsynMap') :: Map Identifier Attributes) ((_tlIsynMap') :: Map Identifier Attributes) ->
     _hdIsynMap' `Map.union` _tlIsynMap'
   {-# INLINE rule167 #-}
   rule167 = \ ((_tlIacount) :: Int) ->
     _tlIacount
   {-# INLINE rule168 #-}
   rule168 = \ ((_tlIvcount) :: Int) ->
     _tlIvcount
   {-# INLINE rule169 #-}
   rule169 = \ ((_lhsIacount) :: Int) ->
     _lhsIacount
   {-# INLINE rule170 #-}
   rule170 = \ ((_lhsIallnts) :: [Identifier]) ->
     _lhsIallnts
   {-# INLINE rule171 #-}
   rule171 = \ ((_lhsIaroundMap) :: Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) ->
     _lhsIaroundMap
   {-# INLINE rule172 #-}
   rule172 = \ ((_lhsIcInterfaceMap) :: CInterfaceMap) ->
     _lhsIcInterfaceMap
   {-# INLINE rule173 #-}
   rule173 = \ ((_lhsIcVisitsMap) :: CVisitsMap) ->
     _lhsIcVisitsMap
   {-# INLINE rule174 #-}
   rule174 = \ ((_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# INLINE rule175 #-}
   rule175 = \ ((_lhsImanualAttrDepMap) :: AttrOrderMap) ->
     _lhsImanualAttrDepMap
   {-# INLINE rule176 #-}
   rule176 = \ ((_lhsImergeMap) :: Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier])))) ->
     _lhsImergeMap
   {-# INLINE rule177 #-}
   rule177 = \ ((_lhsIo_case) :: Bool) ->
     _lhsIo_case
   {-# INLINE rule178 #-}
   rule178 = \ ((_lhsIo_cata) :: Bool) ->
     _lhsIo_cata
   {-# INLINE rule179 #-}
   rule179 = \ ((_lhsIo_data) :: Bool) ->
     _lhsIo_data
   {-# INLINE rule180 #-}
   rule180 = \ ((_lhsIo_dovisit) :: Bool) ->
     _lhsIo_dovisit
   {-# INLINE rule181 #-}
   rule181 = \ ((_lhsIo_newtypes) :: Bool) ->
     _lhsIo_newtypes
   {-# INLINE rule182 #-}
   rule182 = \ ((_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# INLINE rule183 #-}
   rule183 = \ ((_lhsIo_sem) :: Bool) ->
     _lhsIo_sem
   {-# INLINE rule184 #-}
   rule184 = \ ((_lhsIo_sig) :: Bool) ->
     _lhsIo_sig
   {-# INLINE rule185 #-}
   rule185 = \ ((_lhsIo_unbox) :: Bool) ->
     _lhsIo_unbox
   {-# INLINE rule186 #-}
   rule186 = \ ((_lhsIo_wantvisit) :: Bool) ->
     _lhsIo_wantvisit
   {-# INLINE rule187 #-}
   rule187 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule188 #-}
   rule188 = \ ((_lhsIprefix) :: String) ->
     _lhsIprefix
   {-# INLINE rule189 #-}
   rule189 = \ ((_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
   {-# INLINE rule190 #-}
   rule190 = \ ((_lhsIvcount) :: Int) ->
     _lhsIvcount
   {-# INLINE rule191 #-}
   rule191 = \ ((_hdIacount) :: Int) ->
     _hdIacount
   {-# INLINE rule192 #-}
   rule192 = \ ((_lhsIallnts) :: [Identifier]) ->
     _lhsIallnts
   {-# INLINE rule193 #-}
   rule193 = \ ((_lhsIaroundMap) :: Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) ->
     _lhsIaroundMap
   {-# INLINE rule194 #-}
   rule194 = \ ((_lhsIcInterfaceMap) :: CInterfaceMap) ->
     _lhsIcInterfaceMap
   {-# INLINE rule195 #-}
   rule195 = \ ((_lhsIcVisitsMap) :: CVisitsMap) ->
     _lhsIcVisitsMap
   {-# INLINE rule196 #-}
   rule196 = \ ((_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# INLINE rule197 #-}
   rule197 = \ ((_lhsImanualAttrDepMap) :: AttrOrderMap) ->
     _lhsImanualAttrDepMap
   {-# INLINE rule198 #-}
   rule198 = \ ((_lhsImergeMap) :: Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier])))) ->
     _lhsImergeMap
   {-# INLINE rule199 #-}
   rule199 = \ ((_lhsIo_case) :: Bool) ->
     _lhsIo_case
   {-# INLINE rule200 #-}
   rule200 = \ ((_lhsIo_cata) :: Bool) ->
     _lhsIo_cata
   {-# INLINE rule201 #-}
   rule201 = \ ((_lhsIo_data) :: Bool) ->
     _lhsIo_data
   {-# INLINE rule202 #-}
   rule202 = \ ((_lhsIo_dovisit) :: Bool) ->
     _lhsIo_dovisit
   {-# INLINE rule203 #-}
   rule203 = \ ((_lhsIo_newtypes) :: Bool) ->
     _lhsIo_newtypes
   {-# INLINE rule204 #-}
   rule204 = \ ((_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# INLINE rule205 #-}
   rule205 = \ ((_lhsIo_sem) :: Bool) ->
     _lhsIo_sem
   {-# INLINE rule206 #-}
   rule206 = \ ((_lhsIo_sig) :: Bool) ->
     _lhsIo_sig
   {-# INLINE rule207 #-}
   rule207 = \ ((_lhsIo_unbox) :: Bool) ->
     _lhsIo_unbox
   {-# INLINE rule208 #-}
   rule208 = \ ((_lhsIo_wantvisit) :: Bool) ->
     _lhsIo_wantvisit
   {-# INLINE rule209 #-}
   rule209 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule210 #-}
   rule210 = \ ((_lhsIprefix) :: String) ->
     _lhsIprefix
   {-# INLINE rule211 #-}
   rule211 = \ ((_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
   {-# INLINE rule212 #-}
   rule212 = \ ((_hdIvcount) :: Int) ->
     _hdIvcount
{-# NOINLINE sem_Nonterminals_Nil #-}
sem_Nonterminals_Nil ::  T_Nonterminals 
sem_Nonterminals_Nil  = T_Nonterminals (return st17) where
   {-# NOINLINE st17 #-}
   st17 = let
      v16 :: T_Nonterminals_v16 
      v16 = \ (T_Nonterminals_vIn16 _lhsIacount _lhsIallnts _lhsIaroundMap _lhsIcInterfaceMap _lhsIcVisitsMap _lhsIinhMap _lhsImanualAttrDepMap _lhsImergeMap _lhsIo_case _lhsIo_cata _lhsIo_data _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_unbox _lhsIo_wantvisit _lhsIoptions _lhsIprefix _lhsIsynMap _lhsIvcount) -> ( let
         _lhsOcNonterminals :: CNonterminals
         _lhsOcNonterminals = rule213  ()
         _lhsOadditionalDep :: Seq Edge
         _lhsOadditionalDep = rule214  ()
         _lhsOaranges :: Seq (Int,Int,Int)
         _lhsOaranges = rule215  ()
         _lhsOaroundDep :: Seq Edge
         _lhsOaroundDep = rule216  ()
         _lhsOdirectDep :: Seq Edge
         _lhsOdirectDep = rule217  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule218  ()
         _lhsOinhMap' :: Map Identifier Attributes
         _lhsOinhMap' = rule219  ()
         _lhsOinstDep :: Seq Edge
         _lhsOinstDep = rule220  ()
         _lhsOmergeDep :: Seq Edge
         _lhsOmergeDep = rule221  ()
         _lhsOnAutoRules :: Int
         _lhsOnAutoRules = rule222  ()
         _lhsOnExplicitRules :: Int
         _lhsOnExplicitRules = rule223  ()
         _lhsOnonts :: [(NontermIdent,[ConstructorIdent])]
         _lhsOnonts = rule224  ()
         _lhsOntattrs :: Seq (Vertex,NTAttr)
         _lhsOntattrs = rule225  ()
         _lhsOrules :: Seq (Vertex,CRule)
         _lhsOrules = rule226  ()
         _lhsOsynMap' :: Map Identifier Attributes
         _lhsOsynMap' = rule227  ()
         _lhsOacount :: Int
         _lhsOacount = rule228 _lhsIacount
         _lhsOvcount :: Int
         _lhsOvcount = rule229 _lhsIvcount
         __result_ = T_Nonterminals_vOut16 _lhsOacount _lhsOadditionalDep _lhsOaranges _lhsOaroundDep _lhsOcNonterminals _lhsOdirectDep _lhsOerrors _lhsOinhMap' _lhsOinstDep _lhsOmergeDep _lhsOnAutoRules _lhsOnExplicitRules _lhsOnonts _lhsOntattrs _lhsOrules _lhsOsynMap' _lhsOvcount
         in __result_ )
     in C_Nonterminals_s17 v16
   {-# INLINE rule213 #-}
   {-# LINE 627 "./src-ag/Order.ag" #-}
   rule213 = \  (_ :: ()) ->
                                 {-# LINE 627 "./src-ag/Order.ag" #-}
                                 []
                                 {-# LINE 1755 "dist/build/Order.hs"#-}
   {-# INLINE rule214 #-}
   rule214 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule215 #-}
   rule215 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule216 #-}
   rule216 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule217 #-}
   rule217 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule218 #-}
   rule218 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule219 #-}
   rule219 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule220 #-}
   rule220 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule221 #-}
   rule221 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule222 #-}
   rule222 = \  (_ :: ()) ->
     0
   {-# INLINE rule223 #-}
   rule223 = \  (_ :: ()) ->
     0
   {-# INLINE rule224 #-}
   rule224 = \  (_ :: ()) ->
     []
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
   rule228 = \ ((_lhsIacount) :: Int) ->
     _lhsIacount
   {-# INLINE rule229 #-}
   rule229 = \ ((_lhsIvcount) :: Int) ->
     _lhsIvcount

-- Pattern -----------------------------------------------------
-- wrapper
data Inh_Pattern  = Inh_Pattern { allTypeSigs_Inh_Pattern :: (Map Identifier Type), altAttrs_Inh_Pattern :: (Map AltAttr Vertex), con_Inh_Pattern :: (Identifier), inh_Inh_Pattern :: (Attributes), nt_Inh_Pattern :: (Identifier), syn_Inh_Pattern :: (Attributes) }
data Syn_Pattern  = Syn_Pattern { copy_Syn_Pattern :: (Pattern), errors_Syn_Pattern :: (Seq Error), gathAltAttrs_Syn_Pattern :: ([AltAttr]), instVars_Syn_Pattern :: ([Identifier]), locVars_Syn_Pattern :: ([Identifier]), patternAttrs_Syn_Pattern :: ([(Identifier,Identifier,Bool)]) }
{-# INLINABLE wrap_Pattern #-}
wrap_Pattern :: T_Pattern  -> Inh_Pattern  -> (Syn_Pattern )
wrap_Pattern (T_Pattern act) (Inh_Pattern _lhsIallTypeSigs _lhsIaltAttrs _lhsIcon _lhsIinh _lhsInt _lhsIsyn) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Pattern_vIn19 _lhsIallTypeSigs _lhsIaltAttrs _lhsIcon _lhsIinh _lhsInt _lhsIsyn
        (T_Pattern_vOut19 _lhsOcopy _lhsOerrors _lhsOgathAltAttrs _lhsOinstVars _lhsOlocVars _lhsOpatternAttrs) <- return (inv_Pattern_s20 sem arg)
        return (Syn_Pattern _lhsOcopy _lhsOerrors _lhsOgathAltAttrs _lhsOinstVars _lhsOlocVars _lhsOpatternAttrs)
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
data T_Pattern_vIn19  = T_Pattern_vIn19 (Map Identifier Type) (Map AltAttr Vertex) (Identifier) (Attributes) (Identifier) (Attributes)
data T_Pattern_vOut19  = T_Pattern_vOut19 (Pattern) (Seq Error) ([AltAttr]) ([Identifier]) ([Identifier]) ([(Identifier,Identifier,Bool)])
{-# NOINLINE sem_Pattern_Constr #-}
sem_Pattern_Constr :: (ConstructorIdent) -> T_Patterns  -> T_Pattern 
sem_Pattern_Constr arg_name_ arg_pats_ = T_Pattern (return st20) where
   {-# NOINLINE st20 #-}
   st20 = let
      v19 :: T_Pattern_v19 
      v19 = \ (T_Pattern_vIn19 _lhsIallTypeSigs _lhsIaltAttrs _lhsIcon _lhsIinh _lhsInt _lhsIsyn) -> ( let
         _patsX23 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_pats_))
         (T_Patterns_vOut22 _patsIcopy _patsIerrors _patsIgathAltAttrs _patsIinstVars _patsIlocVars _patsIpatternAttrs) = inv_Patterns_s23 _patsX23 (T_Patterns_vIn22 _patsOallTypeSigs _patsOaltAttrs _patsOcon _patsOinh _patsOnt _patsOsyn)
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule230 _patsIerrors
         _lhsOgathAltAttrs :: [AltAttr]
         _lhsOgathAltAttrs = rule231 _patsIgathAltAttrs
         _lhsOinstVars :: [Identifier]
         _lhsOinstVars = rule232 _patsIinstVars
         _lhsOlocVars :: [Identifier]
         _lhsOlocVars = rule233 _patsIlocVars
         _lhsOpatternAttrs :: [(Identifier,Identifier,Bool)]
         _lhsOpatternAttrs = rule234 _patsIpatternAttrs
         _copy = rule235 _patsIcopy arg_name_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule236 _copy
         _patsOallTypeSigs = rule237 _lhsIallTypeSigs
         _patsOaltAttrs = rule238 _lhsIaltAttrs
         _patsOcon = rule239 _lhsIcon
         _patsOinh = rule240 _lhsIinh
         _patsOnt = rule241 _lhsInt
         _patsOsyn = rule242 _lhsIsyn
         __result_ = T_Pattern_vOut19 _lhsOcopy _lhsOerrors _lhsOgathAltAttrs _lhsOinstVars _lhsOlocVars _lhsOpatternAttrs
         in __result_ )
     in C_Pattern_s20 v19
   {-# INLINE rule230 #-}
   rule230 = \ ((_patsIerrors) :: Seq Error) ->
     _patsIerrors
   {-# INLINE rule231 #-}
   rule231 = \ ((_patsIgathAltAttrs) :: [AltAttr]) ->
     _patsIgathAltAttrs
   {-# INLINE rule232 #-}
   rule232 = \ ((_patsIinstVars) :: [Identifier]) ->
     _patsIinstVars
   {-# INLINE rule233 #-}
   rule233 = \ ((_patsIlocVars) :: [Identifier]) ->
     _patsIlocVars
   {-# INLINE rule234 #-}
   rule234 = \ ((_patsIpatternAttrs) :: [(Identifier,Identifier,Bool)]) ->
     _patsIpatternAttrs
   {-# INLINE rule235 #-}
   rule235 = \ ((_patsIcopy) :: Patterns) name_ ->
     Constr name_ _patsIcopy
   {-# INLINE rule236 #-}
   rule236 = \ _copy ->
     _copy
   {-# INLINE rule237 #-}
   rule237 = \ ((_lhsIallTypeSigs) :: Map Identifier Type) ->
     _lhsIallTypeSigs
   {-# INLINE rule238 #-}
   rule238 = \ ((_lhsIaltAttrs) :: Map AltAttr Vertex) ->
     _lhsIaltAttrs
   {-# INLINE rule239 #-}
   rule239 = \ ((_lhsIcon) :: Identifier) ->
     _lhsIcon
   {-# INLINE rule240 #-}
   rule240 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule241 #-}
   rule241 = \ ((_lhsInt) :: Identifier) ->
     _lhsInt
   {-# INLINE rule242 #-}
   rule242 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
{-# NOINLINE sem_Pattern_Product #-}
sem_Pattern_Product :: (Pos) -> T_Patterns  -> T_Pattern 
sem_Pattern_Product arg_pos_ arg_pats_ = T_Pattern (return st20) where
   {-# NOINLINE st20 #-}
   st20 = let
      v19 :: T_Pattern_v19 
      v19 = \ (T_Pattern_vIn19 _lhsIallTypeSigs _lhsIaltAttrs _lhsIcon _lhsIinh _lhsInt _lhsIsyn) -> ( let
         _patsX23 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_pats_))
         (T_Patterns_vOut22 _patsIcopy _patsIerrors _patsIgathAltAttrs _patsIinstVars _patsIlocVars _patsIpatternAttrs) = inv_Patterns_s23 _patsX23 (T_Patterns_vIn22 _patsOallTypeSigs _patsOaltAttrs _patsOcon _patsOinh _patsOnt _patsOsyn)
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule243 _patsIerrors
         _lhsOgathAltAttrs :: [AltAttr]
         _lhsOgathAltAttrs = rule244 _patsIgathAltAttrs
         _lhsOinstVars :: [Identifier]
         _lhsOinstVars = rule245 _patsIinstVars
         _lhsOlocVars :: [Identifier]
         _lhsOlocVars = rule246 _patsIlocVars
         _lhsOpatternAttrs :: [(Identifier,Identifier,Bool)]
         _lhsOpatternAttrs = rule247 _patsIpatternAttrs
         _copy = rule248 _patsIcopy arg_pos_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule249 _copy
         _patsOallTypeSigs = rule250 _lhsIallTypeSigs
         _patsOaltAttrs = rule251 _lhsIaltAttrs
         _patsOcon = rule252 _lhsIcon
         _patsOinh = rule253 _lhsIinh
         _patsOnt = rule254 _lhsInt
         _patsOsyn = rule255 _lhsIsyn
         __result_ = T_Pattern_vOut19 _lhsOcopy _lhsOerrors _lhsOgathAltAttrs _lhsOinstVars _lhsOlocVars _lhsOpatternAttrs
         in __result_ )
     in C_Pattern_s20 v19
   {-# INLINE rule243 #-}
   rule243 = \ ((_patsIerrors) :: Seq Error) ->
     _patsIerrors
   {-# INLINE rule244 #-}
   rule244 = \ ((_patsIgathAltAttrs) :: [AltAttr]) ->
     _patsIgathAltAttrs
   {-# INLINE rule245 #-}
   rule245 = \ ((_patsIinstVars) :: [Identifier]) ->
     _patsIinstVars
   {-# INLINE rule246 #-}
   rule246 = \ ((_patsIlocVars) :: [Identifier]) ->
     _patsIlocVars
   {-# INLINE rule247 #-}
   rule247 = \ ((_patsIpatternAttrs) :: [(Identifier,Identifier,Bool)]) ->
     _patsIpatternAttrs
   {-# INLINE rule248 #-}
   rule248 = \ ((_patsIcopy) :: Patterns) pos_ ->
     Product pos_ _patsIcopy
   {-# INLINE rule249 #-}
   rule249 = \ _copy ->
     _copy
   {-# INLINE rule250 #-}
   rule250 = \ ((_lhsIallTypeSigs) :: Map Identifier Type) ->
     _lhsIallTypeSigs
   {-# INLINE rule251 #-}
   rule251 = \ ((_lhsIaltAttrs) :: Map AltAttr Vertex) ->
     _lhsIaltAttrs
   {-# INLINE rule252 #-}
   rule252 = \ ((_lhsIcon) :: Identifier) ->
     _lhsIcon
   {-# INLINE rule253 #-}
   rule253 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule254 #-}
   rule254 = \ ((_lhsInt) :: Identifier) ->
     _lhsInt
   {-# INLINE rule255 #-}
   rule255 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
{-# NOINLINE sem_Pattern_Alias #-}
sem_Pattern_Alias :: (Identifier) -> (Identifier) -> T_Pattern  -> T_Pattern 
sem_Pattern_Alias arg_field_ arg_attr_ arg_pat_ = T_Pattern (return st20) where
   {-# NOINLINE st20 #-}
   st20 = let
      v19 :: T_Pattern_v19 
      v19 = \ (T_Pattern_vIn19 _lhsIallTypeSigs _lhsIaltAttrs _lhsIcon _lhsIinh _lhsInt _lhsIsyn) -> ( let
         _patX20 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pat_))
         (T_Pattern_vOut19 _patIcopy _patIerrors _patIgathAltAttrs _patIinstVars _patIlocVars _patIpatternAttrs) = inv_Pattern_s20 _patX20 (T_Pattern_vIn19 _patOallTypeSigs _patOaltAttrs _patOcon _patOinh _patOnt _patOsyn)
         _lhsOgathAltAttrs :: [AltAttr]
         _lhsOgathAltAttrs = rule256 arg_attr_ arg_field_
         _lhsOpatternAttrs :: [(Identifier,Identifier,Bool)]
         _lhsOpatternAttrs = rule257 arg_attr_ arg_field_
         _lhsOlocVars :: [Identifier]
         _lhsOlocVars = rule258 arg_attr_ arg_field_
         _lhsOinstVars :: [Identifier]
         _lhsOinstVars = rule259 arg_attr_ arg_field_
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule260 _patIerrors
         _copy = rule261 _patIcopy arg_attr_ arg_field_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule262 _copy
         _patOallTypeSigs = rule263 _lhsIallTypeSigs
         _patOaltAttrs = rule264 _lhsIaltAttrs
         _patOcon = rule265 _lhsIcon
         _patOinh = rule266 _lhsIinh
         _patOnt = rule267 _lhsInt
         _patOsyn = rule268 _lhsIsyn
         __result_ = T_Pattern_vOut19 _lhsOcopy _lhsOerrors _lhsOgathAltAttrs _lhsOinstVars _lhsOlocVars _lhsOpatternAttrs
         in __result_ )
     in C_Pattern_s20 v19
   {-# INLINE rule256 #-}
   {-# LINE 187 "./src-ag/Order.ag" #-}
   rule256 = \ attr_ field_ ->
                                {-# LINE 187 "./src-ag/Order.ag" #-}
                                [AltAttr field_ attr_ (field_ == _LOC || field_ == _INST)]
                                {-# LINE 2015 "dist/build/Order.hs"#-}
   {-# INLINE rule257 #-}
   {-# LINE 253 "./src-ag/Order.ag" #-}
   rule257 = \ attr_ field_ ->
                                {-# LINE 253 "./src-ag/Order.ag" #-}
                                [(field_,attr_,(field_ == _LOC || field_ == _INST))]
                                {-# LINE 2021 "dist/build/Order.hs"#-}
   {-# INLINE rule258 #-}
   {-# LINE 685 "./src-ag/Order.ag" #-}
   rule258 = \ attr_ field_ ->
                               {-# LINE 685 "./src-ag/Order.ag" #-}
                               if field_ == _LOC
                                  then [attr_]
                                  else []
                               {-# LINE 2029 "dist/build/Order.hs"#-}
   {-# INLINE rule259 #-}
   {-# LINE 688 "./src-ag/Order.ag" #-}
   rule259 = \ attr_ field_ ->
                               {-# LINE 688 "./src-ag/Order.ag" #-}
                               if field_ == _INST
                                  then [attr_]
                                  else []
                               {-# LINE 2037 "dist/build/Order.hs"#-}
   {-# INLINE rule260 #-}
   rule260 = \ ((_patIerrors) :: Seq Error) ->
     _patIerrors
   {-# INLINE rule261 #-}
   rule261 = \ ((_patIcopy) :: Pattern) attr_ field_ ->
     Alias field_ attr_ _patIcopy
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
   rule265 = \ ((_lhsIcon) :: Identifier) ->
     _lhsIcon
   {-# INLINE rule266 #-}
   rule266 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule267 #-}
   rule267 = \ ((_lhsInt) :: Identifier) ->
     _lhsInt
   {-# INLINE rule268 #-}
   rule268 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
{-# NOINLINE sem_Pattern_Irrefutable #-}
sem_Pattern_Irrefutable :: T_Pattern  -> T_Pattern 
sem_Pattern_Irrefutable arg_pat_ = T_Pattern (return st20) where
   {-# NOINLINE st20 #-}
   st20 = let
      v19 :: T_Pattern_v19 
      v19 = \ (T_Pattern_vIn19 _lhsIallTypeSigs _lhsIaltAttrs _lhsIcon _lhsIinh _lhsInt _lhsIsyn) -> ( let
         _patX20 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pat_))
         (T_Pattern_vOut19 _patIcopy _patIerrors _patIgathAltAttrs _patIinstVars _patIlocVars _patIpatternAttrs) = inv_Pattern_s20 _patX20 (T_Pattern_vIn19 _patOallTypeSigs _patOaltAttrs _patOcon _patOinh _patOnt _patOsyn)
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule269 _patIerrors
         _lhsOgathAltAttrs :: [AltAttr]
         _lhsOgathAltAttrs = rule270 _patIgathAltAttrs
         _lhsOinstVars :: [Identifier]
         _lhsOinstVars = rule271 _patIinstVars
         _lhsOlocVars :: [Identifier]
         _lhsOlocVars = rule272 _patIlocVars
         _lhsOpatternAttrs :: [(Identifier,Identifier,Bool)]
         _lhsOpatternAttrs = rule273 _patIpatternAttrs
         _copy = rule274 _patIcopy
         _lhsOcopy :: Pattern
         _lhsOcopy = rule275 _copy
         _patOallTypeSigs = rule276 _lhsIallTypeSigs
         _patOaltAttrs = rule277 _lhsIaltAttrs
         _patOcon = rule278 _lhsIcon
         _patOinh = rule279 _lhsIinh
         _patOnt = rule280 _lhsInt
         _patOsyn = rule281 _lhsIsyn
         __result_ = T_Pattern_vOut19 _lhsOcopy _lhsOerrors _lhsOgathAltAttrs _lhsOinstVars _lhsOlocVars _lhsOpatternAttrs
         in __result_ )
     in C_Pattern_s20 v19
   {-# INLINE rule269 #-}
   rule269 = \ ((_patIerrors) :: Seq Error) ->
     _patIerrors
   {-# INLINE rule270 #-}
   rule270 = \ ((_patIgathAltAttrs) :: [AltAttr]) ->
     _patIgathAltAttrs
   {-# INLINE rule271 #-}
   rule271 = \ ((_patIinstVars) :: [Identifier]) ->
     _patIinstVars
   {-# INLINE rule272 #-}
   rule272 = \ ((_patIlocVars) :: [Identifier]) ->
     _patIlocVars
   {-# INLINE rule273 #-}
   rule273 = \ ((_patIpatternAttrs) :: [(Identifier,Identifier,Bool)]) ->
     _patIpatternAttrs
   {-# INLINE rule274 #-}
   rule274 = \ ((_patIcopy) :: Pattern) ->
     Irrefutable _patIcopy
   {-# INLINE rule275 #-}
   rule275 = \ _copy ->
     _copy
   {-# INLINE rule276 #-}
   rule276 = \ ((_lhsIallTypeSigs) :: Map Identifier Type) ->
     _lhsIallTypeSigs
   {-# INLINE rule277 #-}
   rule277 = \ ((_lhsIaltAttrs) :: Map AltAttr Vertex) ->
     _lhsIaltAttrs
   {-# INLINE rule278 #-}
   rule278 = \ ((_lhsIcon) :: Identifier) ->
     _lhsIcon
   {-# INLINE rule279 #-}
   rule279 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule280 #-}
   rule280 = \ ((_lhsInt) :: Identifier) ->
     _lhsInt
   {-# INLINE rule281 #-}
   rule281 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
{-# NOINLINE sem_Pattern_Underscore #-}
sem_Pattern_Underscore :: (Pos) -> T_Pattern 
sem_Pattern_Underscore arg_pos_ = T_Pattern (return st20) where
   {-# NOINLINE st20 #-}
   st20 = let
      v19 :: T_Pattern_v19 
      v19 = \ (T_Pattern_vIn19 _lhsIallTypeSigs _lhsIaltAttrs _lhsIcon _lhsIinh _lhsInt _lhsIsyn) -> ( let
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule282  ()
         _lhsOgathAltAttrs :: [AltAttr]
         _lhsOgathAltAttrs = rule283  ()
         _lhsOinstVars :: [Identifier]
         _lhsOinstVars = rule284  ()
         _lhsOlocVars :: [Identifier]
         _lhsOlocVars = rule285  ()
         _lhsOpatternAttrs :: [(Identifier,Identifier,Bool)]
         _lhsOpatternAttrs = rule286  ()
         _copy = rule287 arg_pos_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule288 _copy
         __result_ = T_Pattern_vOut19 _lhsOcopy _lhsOerrors _lhsOgathAltAttrs _lhsOinstVars _lhsOlocVars _lhsOpatternAttrs
         in __result_ )
     in C_Pattern_s20 v19
   {-# INLINE rule282 #-}
   rule282 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule283 #-}
   rule283 = \  (_ :: ()) ->
     []
   {-# INLINE rule284 #-}
   rule284 = \  (_ :: ()) ->
     []
   {-# INLINE rule285 #-}
   rule285 = \  (_ :: ()) ->
     []
   {-# INLINE rule286 #-}
   rule286 = \  (_ :: ()) ->
     []
   {-# INLINE rule287 #-}
   rule287 = \ pos_ ->
     Underscore pos_
   {-# INLINE rule288 #-}
   rule288 = \ _copy ->
     _copy

-- Patterns ----------------------------------------------------
-- wrapper
data Inh_Patterns  = Inh_Patterns { allTypeSigs_Inh_Patterns :: (Map Identifier Type), altAttrs_Inh_Patterns :: (Map AltAttr Vertex), con_Inh_Patterns :: (Identifier), inh_Inh_Patterns :: (Attributes), nt_Inh_Patterns :: (Identifier), syn_Inh_Patterns :: (Attributes) }
data Syn_Patterns  = Syn_Patterns { copy_Syn_Patterns :: (Patterns), errors_Syn_Patterns :: (Seq Error), gathAltAttrs_Syn_Patterns :: ([AltAttr]), instVars_Syn_Patterns :: ([Identifier]), locVars_Syn_Patterns :: ([Identifier]), patternAttrs_Syn_Patterns :: ([(Identifier,Identifier,Bool)]) }
{-# INLINABLE wrap_Patterns #-}
wrap_Patterns :: T_Patterns  -> Inh_Patterns  -> (Syn_Patterns )
wrap_Patterns (T_Patterns act) (Inh_Patterns _lhsIallTypeSigs _lhsIaltAttrs _lhsIcon _lhsIinh _lhsInt _lhsIsyn) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Patterns_vIn22 _lhsIallTypeSigs _lhsIaltAttrs _lhsIcon _lhsIinh _lhsInt _lhsIsyn
        (T_Patterns_vOut22 _lhsOcopy _lhsOerrors _lhsOgathAltAttrs _lhsOinstVars _lhsOlocVars _lhsOpatternAttrs) <- return (inv_Patterns_s23 sem arg)
        return (Syn_Patterns _lhsOcopy _lhsOerrors _lhsOgathAltAttrs _lhsOinstVars _lhsOlocVars _lhsOpatternAttrs)
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
data T_Patterns_vIn22  = T_Patterns_vIn22 (Map Identifier Type) (Map AltAttr Vertex) (Identifier) (Attributes) (Identifier) (Attributes)
data T_Patterns_vOut22  = T_Patterns_vOut22 (Patterns) (Seq Error) ([AltAttr]) ([Identifier]) ([Identifier]) ([(Identifier,Identifier,Bool)])
{-# NOINLINE sem_Patterns_Cons #-}
sem_Patterns_Cons :: T_Pattern  -> T_Patterns  -> T_Patterns 
sem_Patterns_Cons arg_hd_ arg_tl_ = T_Patterns (return st23) where
   {-# NOINLINE st23 #-}
   st23 = let
      v22 :: T_Patterns_v22 
      v22 = \ (T_Patterns_vIn22 _lhsIallTypeSigs _lhsIaltAttrs _lhsIcon _lhsIinh _lhsInt _lhsIsyn) -> ( let
         _hdX20 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_hd_))
         _tlX23 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_tl_))
         (T_Pattern_vOut19 _hdIcopy _hdIerrors _hdIgathAltAttrs _hdIinstVars _hdIlocVars _hdIpatternAttrs) = inv_Pattern_s20 _hdX20 (T_Pattern_vIn19 _hdOallTypeSigs _hdOaltAttrs _hdOcon _hdOinh _hdOnt _hdOsyn)
         (T_Patterns_vOut22 _tlIcopy _tlIerrors _tlIgathAltAttrs _tlIinstVars _tlIlocVars _tlIpatternAttrs) = inv_Patterns_s23 _tlX23 (T_Patterns_vIn22 _tlOallTypeSigs _tlOaltAttrs _tlOcon _tlOinh _tlOnt _tlOsyn)
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule289 _hdIerrors _tlIerrors
         _lhsOgathAltAttrs :: [AltAttr]
         _lhsOgathAltAttrs = rule290 _hdIgathAltAttrs _tlIgathAltAttrs
         _lhsOinstVars :: [Identifier]
         _lhsOinstVars = rule291 _hdIinstVars _tlIinstVars
         _lhsOlocVars :: [Identifier]
         _lhsOlocVars = rule292 _hdIlocVars _tlIlocVars
         _lhsOpatternAttrs :: [(Identifier,Identifier,Bool)]
         _lhsOpatternAttrs = rule293 _hdIpatternAttrs _tlIpatternAttrs
         _copy = rule294 _hdIcopy _tlIcopy
         _lhsOcopy :: Patterns
         _lhsOcopy = rule295 _copy
         _hdOallTypeSigs = rule296 _lhsIallTypeSigs
         _hdOaltAttrs = rule297 _lhsIaltAttrs
         _hdOcon = rule298 _lhsIcon
         _hdOinh = rule299 _lhsIinh
         _hdOnt = rule300 _lhsInt
         _hdOsyn = rule301 _lhsIsyn
         _tlOallTypeSigs = rule302 _lhsIallTypeSigs
         _tlOaltAttrs = rule303 _lhsIaltAttrs
         _tlOcon = rule304 _lhsIcon
         _tlOinh = rule305 _lhsIinh
         _tlOnt = rule306 _lhsInt
         _tlOsyn = rule307 _lhsIsyn
         __result_ = T_Patterns_vOut22 _lhsOcopy _lhsOerrors _lhsOgathAltAttrs _lhsOinstVars _lhsOlocVars _lhsOpatternAttrs
         in __result_ )
     in C_Patterns_s23 v22
   {-# INLINE rule289 #-}
   rule289 = \ ((_hdIerrors) :: Seq Error) ((_tlIerrors) :: Seq Error) ->
     _hdIerrors Seq.>< _tlIerrors
   {-# INLINE rule290 #-}
   rule290 = \ ((_hdIgathAltAttrs) :: [AltAttr]) ((_tlIgathAltAttrs) :: [AltAttr]) ->
     _hdIgathAltAttrs ++ _tlIgathAltAttrs
   {-# INLINE rule291 #-}
   rule291 = \ ((_hdIinstVars) :: [Identifier]) ((_tlIinstVars) :: [Identifier]) ->
     _hdIinstVars ++ _tlIinstVars
   {-# INLINE rule292 #-}
   rule292 = \ ((_hdIlocVars) :: [Identifier]) ((_tlIlocVars) :: [Identifier]) ->
     _hdIlocVars ++ _tlIlocVars
   {-# INLINE rule293 #-}
   rule293 = \ ((_hdIpatternAttrs) :: [(Identifier,Identifier,Bool)]) ((_tlIpatternAttrs) :: [(Identifier,Identifier,Bool)]) ->
     _hdIpatternAttrs ++ _tlIpatternAttrs
   {-# INLINE rule294 #-}
   rule294 = \ ((_hdIcopy) :: Pattern) ((_tlIcopy) :: Patterns) ->
     (:) _hdIcopy _tlIcopy
   {-# INLINE rule295 #-}
   rule295 = \ _copy ->
     _copy
   {-# INLINE rule296 #-}
   rule296 = \ ((_lhsIallTypeSigs) :: Map Identifier Type) ->
     _lhsIallTypeSigs
   {-# INLINE rule297 #-}
   rule297 = \ ((_lhsIaltAttrs) :: Map AltAttr Vertex) ->
     _lhsIaltAttrs
   {-# INLINE rule298 #-}
   rule298 = \ ((_lhsIcon) :: Identifier) ->
     _lhsIcon
   {-# INLINE rule299 #-}
   rule299 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule300 #-}
   rule300 = \ ((_lhsInt) :: Identifier) ->
     _lhsInt
   {-# INLINE rule301 #-}
   rule301 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
   {-# INLINE rule302 #-}
   rule302 = \ ((_lhsIallTypeSigs) :: Map Identifier Type) ->
     _lhsIallTypeSigs
   {-# INLINE rule303 #-}
   rule303 = \ ((_lhsIaltAttrs) :: Map AltAttr Vertex) ->
     _lhsIaltAttrs
   {-# INLINE rule304 #-}
   rule304 = \ ((_lhsIcon) :: Identifier) ->
     _lhsIcon
   {-# INLINE rule305 #-}
   rule305 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule306 #-}
   rule306 = \ ((_lhsInt) :: Identifier) ->
     _lhsInt
   {-# INLINE rule307 #-}
   rule307 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
{-# NOINLINE sem_Patterns_Nil #-}
sem_Patterns_Nil ::  T_Patterns 
sem_Patterns_Nil  = T_Patterns (return st23) where
   {-# NOINLINE st23 #-}
   st23 = let
      v22 :: T_Patterns_v22 
      v22 = \ (T_Patterns_vIn22 _lhsIallTypeSigs _lhsIaltAttrs _lhsIcon _lhsIinh _lhsInt _lhsIsyn) -> ( let
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule308  ()
         _lhsOgathAltAttrs :: [AltAttr]
         _lhsOgathAltAttrs = rule309  ()
         _lhsOinstVars :: [Identifier]
         _lhsOinstVars = rule310  ()
         _lhsOlocVars :: [Identifier]
         _lhsOlocVars = rule311  ()
         _lhsOpatternAttrs :: [(Identifier,Identifier,Bool)]
         _lhsOpatternAttrs = rule312  ()
         _copy = rule313  ()
         _lhsOcopy :: Patterns
         _lhsOcopy = rule314 _copy
         __result_ = T_Patterns_vOut22 _lhsOcopy _lhsOerrors _lhsOgathAltAttrs _lhsOinstVars _lhsOlocVars _lhsOpatternAttrs
         in __result_ )
     in C_Patterns_s23 v22
   {-# INLINE rule308 #-}
   rule308 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule309 #-}
   rule309 = \  (_ :: ()) ->
     []
   {-# INLINE rule310 #-}
   rule310 = \  (_ :: ()) ->
     []
   {-# INLINE rule311 #-}
   rule311 = \  (_ :: ()) ->
     []
   {-# INLINE rule312 #-}
   rule312 = \  (_ :: ()) ->
     []
   {-# INLINE rule313 #-}
   rule313 = \  (_ :: ()) ->
     []
   {-# INLINE rule314 #-}
   rule314 = \ _copy ->
     _copy

-- Production --------------------------------------------------
-- wrapper
data Inh_Production  = Inh_Production { allnts_Inh_Production :: ([Identifier]), aroundMap_Inh_Production :: (Map ConstructorIdent (Map Identifier [Expression])), cVisitsMap_Inh_Production :: (CVisitsMap), inh_Inh_Production :: (Attributes), inhMap_Inh_Production :: (Map Identifier Attributes), manualAttrDepMap_Inh_Production :: (AttrOrderMap), mergeMap_Inh_Production :: (Map ConstructorIdent (Map Identifier (Identifier,[Identifier]))), nt_Inh_Production :: (Identifier), o_case_Inh_Production :: (Bool), o_cata_Inh_Production :: (Bool), o_dovisit_Inh_Production :: (Bool), o_newtypes_Inh_Production :: (Bool), o_rename_Inh_Production :: (Bool), o_sem_Inh_Production :: (Bool), o_sig_Inh_Production :: (Bool), o_unbox_Inh_Production :: (Bool), o_wantvisit_Inh_Production :: (Bool), options_Inh_Production :: (Options), prefix_Inh_Production :: (String), syn_Inh_Production :: (Attributes), synMap_Inh_Production :: (Map Identifier Attributes), vcount_Inh_Production :: (Int) }
data Syn_Production  = Syn_Production { additionalDep_Syn_Production :: (Seq Edge), aroundDep_Syn_Production :: (Seq Edge), cProduction_Syn_Production :: (CProduction), cons_Syn_Production :: ([ConstructorIdent]), directDep_Syn_Production :: (Seq Edge), errors_Syn_Production :: (Seq Error), instDep_Syn_Production :: (Seq Edge), mergeDep_Syn_Production :: (Seq Edge), nAutoRules_Syn_Production :: (Int), nExplicitRules_Syn_Production :: (Int), rules_Syn_Production :: (Seq (Vertex,CRule)), vcount_Syn_Production :: (Int) }
{-# INLINABLE wrap_Production #-}
wrap_Production :: T_Production  -> Inh_Production  -> (Syn_Production )
wrap_Production (T_Production act) (Inh_Production _lhsIallnts _lhsIaroundMap _lhsIcVisitsMap _lhsIinh _lhsIinhMap _lhsImanualAttrDepMap _lhsImergeMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_unbox _lhsIo_wantvisit _lhsIoptions _lhsIprefix _lhsIsyn _lhsIsynMap _lhsIvcount) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Production_vIn25 _lhsIallnts _lhsIaroundMap _lhsIcVisitsMap _lhsIinh _lhsIinhMap _lhsImanualAttrDepMap _lhsImergeMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_unbox _lhsIo_wantvisit _lhsIoptions _lhsIprefix _lhsIsyn _lhsIsynMap _lhsIvcount
        (T_Production_vOut25 _lhsOadditionalDep _lhsOaroundDep _lhsOcProduction _lhsOcons _lhsOdirectDep _lhsOerrors _lhsOinstDep _lhsOmergeDep _lhsOnAutoRules _lhsOnExplicitRules _lhsOrules _lhsOvcount) <- return (inv_Production_s26 sem arg)
        return (Syn_Production _lhsOadditionalDep _lhsOaroundDep _lhsOcProduction _lhsOcons _lhsOdirectDep _lhsOerrors _lhsOinstDep _lhsOmergeDep _lhsOnAutoRules _lhsOnExplicitRules _lhsOrules _lhsOvcount)
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
data T_Production_vOut25  = T_Production_vOut25 (Seq Edge) (Seq Edge) (CProduction) ([ConstructorIdent]) (Seq Edge) (Seq Error) (Seq Edge) (Seq Edge) (Int) (Int) (Seq (Vertex,CRule)) (Int)
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
         (T_Children_vOut4 _childrenIattributes _childrenIcollectChildrenInhs _childrenIcollectChildrenSyns _childrenIerrors _childrenIfields _childrenIgathAltAttrs _childrenIgathRules _childrenIinhs _childrenInts _childrenIsinglevisits _childrenIterminals) = inv_Children_s5 _childrenX5 (T_Children_vIn4 _childrenOallfields _childrenOallnts _childrenOattrs _childrenOcon _childrenOinh _childrenOinhMap _childrenOmergeMap _childrenOnt _childrenOo_unbox _childrenOsyn _childrenOsynMap)
         (T_Rules_vOut34 _rulesIdirectDep _rulesIerrors _rulesIgathAltAttrs _rulesIgathRules _rulesIinstDep _rulesIinstVars _rulesIlocVars _rulesInAutoRules _rulesInExplicitRules) = inv_Rules_s35 _rulesX35 (T_Rules_vIn34 _rulesOallTypeSigs _rulesOallfields _rulesOallnts _rulesOaltAttrs _rulesOattrs _rulesOchildInhs _rulesOchildNts _rulesOcon _rulesOinh _rulesOinhsOfChildren _rulesOmergeMap _rulesOnt _rulesOo_case _rulesOo_cata _rulesOo_dovisit _rulesOo_newtypes _rulesOo_rename _rulesOo_sem _rulesOo_sig _rulesOo_wantvisit _rulesOoptions _rulesOprefix _rulesOsyn _rulesOsynsOfChildren)
         (T_TypeSigs_vOut40 _typeSigsItypeSigs) = inv_TypeSigs_s41 _typeSigsX41 (T_TypeSigs_vIn40 _typeSigsOtypeSigs)
         _childrenOcon = rule315 arg_con_
         _rulesOcon = rule316 arg_con_
         _gathAltAttrs = rule317 _childrenIgathAltAttrs _lhsIinh _rulesIgathAltAttrs
         _altAttrs = rule318 _gathAltAttrs _lhsIvcount
         _rulesOchildNts = rule319 _childrenInts
         _rulesOchildInhs = rule320 _childrenIinhs
         _inhRules = rule321 _lhsIinh _lhsInt arg_con_
         _gathRules = rule322 _childrenIgathRules _inhRules _rulesIgathRules
         _lhsOrules :: Seq (Vertex,CRule)
         _lhsOrules = rule323 _gathRules _lhsIvcount
         _lhsOvcount :: Int
         _lhsOvcount = rule324 _gathRules _lhsIvcount
         _manualDeps = rule325 _lhsImanualAttrDepMap _lhsInt arg_con_
         _lhsOadditionalDep :: Seq Edge
         _lhsOadditionalDep = rule326 _altAttrs _manualDeps
         _rulesOsynsOfChildren = rule327 _childrenIcollectChildrenSyns
         _rulesOinhsOfChildren = rule328 _childrenIcollectChildrenInhs
         _mergeMap = rule329 _lhsImergeMap arg_con_
         _lhsOmergeDep :: Seq Edge
         _lhsOmergeDep = rule330 _mergeDep1 _mergeDep2
         _mergeDep1 = rule331 _altAttrs _childrenIcollectChildrenSyns _mergeMap
         _mergeDep2 = rule332 _altAttrs _childrenIcollectChildrenSyns _mergeMap
         _aroundMap = rule333 _lhsIaroundMap arg_con_
         _aroundDep1 = rule334 _altAttrs _aroundMap _childrenIcollectChildrenSyns
         _aroundDep2 = rule335 _altAttrs _aroundMap _childrenIcollectChildrenInhs
         _lhsOaroundDep :: Seq Edge
         _lhsOaroundDep = rule336 _aroundDep1 _aroundDep2
         _lhsOcons :: [ConstructorIdent]
         _lhsOcons = rule337 arg_con_
         _typeSigsOtypeSigs = rule338  ()
         _rulesOallTypeSigs = rule339 _typeSigsItypeSigs
         _cVisits = rule340 _childrenIsinglevisits _gathRules _lhsIcVisitsMap _lhsIinh _lhsInt _lhsIo_dovisit _lhsIsyn arg_con_
         _lhsOcProduction :: CProduction
         _lhsOcProduction = rule341 _cVisits _childrenIfields _childrenIterminals arg_con_
         _allfields = rule342 _childrenIfields
         _attrs = rule343 _childrenIattributes _inhnames _rulesIinstVars _rulesIlocVars
         _inhnames = rule344 _lhsIinh
         _synnames = rule345 _lhsIsyn
         _lhsOdirectDep :: Seq Edge
         _lhsOdirectDep = rule346 _rulesIdirectDep
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule347 _childrenIerrors _rulesIerrors
         _lhsOinstDep :: Seq Edge
         _lhsOinstDep = rule348 _rulesIinstDep
         _lhsOnAutoRules :: Int
         _lhsOnAutoRules = rule349 _rulesInAutoRules
         _lhsOnExplicitRules :: Int
         _lhsOnExplicitRules = rule350 _rulesInExplicitRules
         _childrenOallfields = rule351 _allfields
         _childrenOallnts = rule352 _lhsIallnts
         _childrenOattrs = rule353 _attrs
         _childrenOinh = rule354 _lhsIinh
         _childrenOinhMap = rule355 _lhsIinhMap
         _childrenOmergeMap = rule356 _mergeMap
         _childrenOnt = rule357 _lhsInt
         _childrenOo_unbox = rule358 _lhsIo_unbox
         _childrenOsyn = rule359 _lhsIsyn
         _childrenOsynMap = rule360 _lhsIsynMap
         _rulesOallfields = rule361 _allfields
         _rulesOallnts = rule362 _lhsIallnts
         _rulesOaltAttrs = rule363 _altAttrs
         _rulesOattrs = rule364 _attrs
         _rulesOinh = rule365 _lhsIinh
         _rulesOmergeMap = rule366 _mergeMap
         _rulesOnt = rule367 _lhsInt
         _rulesOo_case = rule368 _lhsIo_case
         _rulesOo_cata = rule369 _lhsIo_cata
         _rulesOo_dovisit = rule370 _lhsIo_dovisit
         _rulesOo_newtypes = rule371 _lhsIo_newtypes
         _rulesOo_rename = rule372 _lhsIo_rename
         _rulesOo_sem = rule373 _lhsIo_sem
         _rulesOo_sig = rule374 _lhsIo_sig
         _rulesOo_wantvisit = rule375 _lhsIo_wantvisit
         _rulesOoptions = rule376 _lhsIoptions
         _rulesOprefix = rule377 _lhsIprefix
         _rulesOsyn = rule378 _lhsIsyn
         __result_ = T_Production_vOut25 _lhsOadditionalDep _lhsOaroundDep _lhsOcProduction _lhsOcons _lhsOdirectDep _lhsOerrors _lhsOinstDep _lhsOmergeDep _lhsOnAutoRules _lhsOnExplicitRules _lhsOrules _lhsOvcount
         in __result_ )
     in C_Production_s26 v25
   {-# INLINE rule315 #-}
   {-# LINE 93 "./src-ag/Order.ag" #-}
   rule315 = \ con_ ->
                                  {-# LINE 93 "./src-ag/Order.ag" #-}
                                  con_
                                  {-# LINE 2478 "dist/build/Order.hs"#-}
   {-# INLINE rule316 #-}
   {-# LINE 95 "./src-ag/Order.ag" #-}
   rule316 = \ con_ ->
                               {-# LINE 95 "./src-ag/Order.ag" #-}
                               con_
                               {-# LINE 2484 "dist/build/Order.hs"#-}
   {-# INLINE rule317 #-}
   {-# LINE 175 "./src-ag/Order.ag" #-}
   rule317 = \ ((_childrenIgathAltAttrs) :: [AltAttr]) ((_lhsIinh) :: Attributes) ((_rulesIgathAltAttrs) :: [AltAttr]) ->
                                       {-# LINE 175 "./src-ag/Order.ag" #-}
                                       [ AltAttr _LHS inh True | inh <- Map.keys _lhsIinh ]
                                        ++ _childrenIgathAltAttrs
                                        ++ _rulesIgathAltAttrs
                                       {-# LINE 2492 "dist/build/Order.hs"#-}
   {-# INLINE rule318 #-}
   {-# LINE 191 "./src-ag/Order.ag" #-}
   rule318 = \ _gathAltAttrs ((_lhsIvcount) :: Int) ->
                                 {-# LINE 191 "./src-ag/Order.ag" #-}
                                 Map.fromList (zip _gathAltAttrs [_lhsIvcount..])
                                 {-# LINE 2498 "dist/build/Order.hs"#-}
   {-# INLINE rule319 #-}
   {-# LINE 204 "./src-ag/Order.ag" #-}
   rule319 = \ ((_childrenInts) :: Seq (Identifier,NontermIdent)) ->
                                    {-# LINE 204 "./src-ag/Order.ag" #-}
                                    Map.fromList (toList _childrenInts)
                                    {-# LINE 2504 "dist/build/Order.hs"#-}
   {-# INLINE rule320 #-}
   {-# LINE 205 "./src-ag/Order.ag" #-}
   rule320 = \ ((_childrenIinhs) :: Seq (Identifier,Attributes)) ->
                                      {-# LINE 205 "./src-ag/Order.ag" #-}
                                      Map.fromList (toList _childrenIinhs)
                                      {-# LINE 2510 "dist/build/Order.hs"#-}
   {-# INLINE rule321 #-}
   {-# LINE 211 "./src-ag/Order.ag" #-}
   rule321 = \ ((_lhsIinh) :: Attributes) ((_lhsInt) :: Identifier) con_ ->
                                  {-# LINE 211 "./src-ag/Order.ag" #-}
                                  [ cRuleLhsInh inh _lhsInt con_ tp | (inh,tp) <- Map.assocs _lhsIinh ]
                                  {-# LINE 2516 "dist/build/Order.hs"#-}
   {-# INLINE rule322 #-}
   {-# LINE 212 "./src-ag/Order.ag" #-}
   rule322 = \ ((_childrenIgathRules) :: Seq CRule) _inhRules ((_rulesIgathRules) :: Seq CRule) ->
                                    {-# LINE 212 "./src-ag/Order.ag" #-}
                                    _inhRules ++ toList (_childrenIgathRules Seq.>< _rulesIgathRules)
                                    {-# LINE 2522 "dist/build/Order.hs"#-}
   {-# INLINE rule323 #-}
   {-# LINE 264 "./src-ag/Order.ag" #-}
   rule323 = \ _gathRules ((_lhsIvcount) :: Int) ->
                               {-# LINE 264 "./src-ag/Order.ag" #-}
                               Seq.fromList (zip [_lhsIvcount..] _gathRules)
                               {-# LINE 2528 "dist/build/Order.hs"#-}
   {-# INLINE rule324 #-}
   {-# LINE 265 "./src-ag/Order.ag" #-}
   rule324 = \ _gathRules ((_lhsIvcount) :: Int) ->
                                 {-# LINE 265 "./src-ag/Order.ag" #-}
                                 _lhsIvcount + length _gathRules
                                 {-# LINE 2534 "dist/build/Order.hs"#-}
   {-# INLINE rule325 #-}
   {-# LINE 293 "./src-ag/Order.ag" #-}
   rule325 = \ ((_lhsImanualAttrDepMap) :: AttrOrderMap) ((_lhsInt) :: Identifier) con_ ->
            {-# LINE 293 "./src-ag/Order.ag" #-}
            Set.toList $ Map.findWithDefault Set.empty con_ $ Map.findWithDefault Map.empty _lhsInt _lhsImanualAttrDepMap
            {-# LINE 2540 "dist/build/Order.hs"#-}
   {-# INLINE rule326 #-}
   {-# LINE 296 "./src-ag/Order.ag" #-}
   rule326 = \ _altAttrs _manualDeps ->
            {-# LINE 296 "./src-ag/Order.ag" #-}
            Seq.fromList [ (vertex True occA, vertex False occB)
                         | Dependency occA occB <- _manualDeps
                         , let vertex inout (OccAttr child nm)
                                 | child == _LOC = findWithErr2 (AltAttr _LOC nm True) _altAttrs
                                 | otherwise     = findWithErr2 (AltAttr child nm inout) _altAttrs
                               vertex _ (OccRule nm)
                                 = findWithErr2 (AltAttr _LOC (Ident ("_rule_" ++ show nm) (getPos nm)) True) _altAttrs
                         ]
            {-# LINE 2553 "dist/build/Order.hs"#-}
   {-# INLINE rule327 #-}
   {-# LINE 342 "./src-ag/Order.ag" #-}
   rule327 = \ ((_childrenIcollectChildrenSyns) :: Map Identifier Attributes ) ->
                                         {-# LINE 342 "./src-ag/Order.ag" #-}
                                         _childrenIcollectChildrenSyns
                                         {-# LINE 2559 "dist/build/Order.hs"#-}
   {-# INLINE rule328 #-}
   {-# LINE 343 "./src-ag/Order.ag" #-}
   rule328 = \ ((_childrenIcollectChildrenInhs) :: Map Identifier Attributes ) ->
                                         {-# LINE 343 "./src-ag/Order.ag" #-}
                                         _childrenIcollectChildrenInhs
                                         {-# LINE 2565 "dist/build/Order.hs"#-}
   {-# INLINE rule329 #-}
   {-# LINE 361 "./src-ag/Order.ag" #-}
   rule329 = \ ((_lhsImergeMap) :: Map ConstructorIdent (Map Identifier (Identifier,[Identifier]))) con_ ->
                                                {-# LINE 361 "./src-ag/Order.ag" #-}
                                                Map.findWithDefault Map.empty con_ _lhsImergeMap
                                                {-# LINE 2571 "dist/build/Order.hs"#-}
   {-# INLINE rule330 #-}
   {-# LINE 372 "./src-ag/Order.ag" #-}
   rule330 = \ _mergeDep1 _mergeDep2 ->
                       {-# LINE 372 "./src-ag/Order.ag" #-}
                       _mergeDep1     Seq.>< _mergeDep2
                       {-# LINE 2577 "dist/build/Order.hs"#-}
   {-# INLINE rule331 #-}
   {-# LINE 374 "./src-ag/Order.ag" #-}
   rule331 = \ _altAttrs ((_childrenIcollectChildrenSyns) :: Map Identifier Attributes ) _mergeMap ->
            {-# LINE 374 "./src-ag/Order.ag" #-}
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
            {-# LINE 2592 "dist/build/Order.hs"#-}
   {-# INLINE rule332 #-}
   {-# LINE 385 "./src-ag/Order.ag" #-}
   rule332 = \ _altAttrs ((_childrenIcollectChildrenSyns) :: Map Identifier Attributes ) _mergeMap ->
            {-# LINE 385 "./src-ag/Order.ag" #-}
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
            {-# LINE 2607 "dist/build/Order.hs"#-}
   {-# INLINE rule333 #-}
   {-# LINE 414 "./src-ag/Order.ag" #-}
   rule333 = \ ((_lhsIaroundMap) :: Map ConstructorIdent (Map Identifier [Expression])) con_ ->
                                                 {-# LINE 414 "./src-ag/Order.ag" #-}
                                                 Map.findWithDefault Map.empty con_ _lhsIaroundMap
                                                 {-# LINE 2613 "dist/build/Order.hs"#-}
   {-# INLINE rule334 #-}
   {-# LINE 422 "./src-ag/Order.ag" #-}
   rule334 = \ _altAttrs _aroundMap ((_childrenIcollectChildrenSyns) :: Map Identifier Attributes ) ->
           {-# LINE 422 "./src-ag/Order.ag" #-}
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
           {-# LINE 2628 "dist/build/Order.hs"#-}
   {-# INLINE rule335 #-}
   {-# LINE 433 "./src-ag/Order.ag" #-}
   rule335 = \ _altAttrs _aroundMap ((_childrenIcollectChildrenInhs) :: Map Identifier Attributes ) ->
            {-# LINE 433 "./src-ag/Order.ag" #-}
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
            {-# LINE 2643 "dist/build/Order.hs"#-}
   {-# INLINE rule336 #-}
   {-# LINE 443 "./src-ag/Order.ag" #-}
   rule336 = \ _aroundDep1 _aroundDep2 ->
                       {-# LINE 443 "./src-ag/Order.ag" #-}
                       _aroundDep1     Seq.>< _aroundDep2
                       {-# LINE 2649 "dist/build/Order.hs"#-}
   {-# INLINE rule337 #-}
   {-# LINE 527 "./src-ag/Order.ag" #-}
   rule337 = \ con_ ->
                              {-# LINE 527 "./src-ag/Order.ag" #-}
                              [con_]
                              {-# LINE 2655 "dist/build/Order.hs"#-}
   {-# INLINE rule338 #-}
   {-# LINE 534 "./src-ag/Order.ag" #-}
   rule338 = \  (_ :: ()) ->
                                     {-# LINE 534 "./src-ag/Order.ag" #-}
                                     Map.empty
                                     {-# LINE 2661 "dist/build/Order.hs"#-}
   {-# INLINE rule339 #-}
   {-# LINE 540 "./src-ag/Order.ag" #-}
   rule339 = \ ((_typeSigsItypeSigs) :: Map Identifier Type) ->
                                      {-# LINE 540 "./src-ag/Order.ag" #-}
                                      _typeSigsItypeSigs
                                      {-# LINE 2667 "dist/build/Order.hs"#-}
   {-# INLINE rule340 #-}
   {-# LINE 608 "./src-ag/Order.ag" #-}
   rule340 = \ ((_childrenIsinglevisits) :: [CRule]) _gathRules ((_lhsIcVisitsMap) :: CVisitsMap) ((_lhsIinh) :: Attributes) ((_lhsInt) :: Identifier) ((_lhsIo_dovisit) :: Bool) ((_lhsIsyn) :: Attributes) con_ ->
                                {-# LINE 608 "./src-ag/Order.ag" #-}
                                if  _lhsIo_dovisit
                                     then let prodsVisitsMap = findWithErr1 "Production.cVisits.nt" _lhsInt _lhsIcVisitsMap
                                              visits = findWithErr1 "Production.cVisits.con" con_ prodsVisitsMap
                                           in visits
                                     else  let  vss = nubBy eqCRuleDefines _gathRules ++ _childrenIsinglevisits
                                           in  [CVisit _lhsIinh _lhsIsyn vss [] False]
                                {-# LINE 2678 "dist/build/Order.hs"#-}
   {-# INLINE rule341 #-}
   {-# LINE 634 "./src-ag/Order.ag" #-}
   rule341 = \ _cVisits ((_childrenIfields) :: [(Identifier,Type,ChildKind)]) ((_childrenIterminals) :: [Identifier]) con_ ->
                                     {-# LINE 634 "./src-ag/Order.ag" #-}
                                     CProduction con_ _cVisits _childrenIfields _childrenIterminals
                                     {-# LINE 2684 "dist/build/Order.hs"#-}
   {-# INLINE rule342 #-}
   {-# LINE 662 "./src-ag/Order.ag" #-}
   rule342 = \ ((_childrenIfields) :: [(Identifier,Type,ChildKind)]) ->
                                  {-# LINE 662 "./src-ag/Order.ag" #-}
                                  _childrenIfields
                                  {-# LINE 2690 "dist/build/Order.hs"#-}
   {-# INLINE rule343 #-}
   {-# LINE 663 "./src-ag/Order.ag" #-}
   rule343 = \ ((_childrenIattributes) :: [(Identifier,Attributes,Attributes)]) _inhnames ((_rulesIinstVars) :: [Identifier]) ((_rulesIlocVars) :: [Identifier]) ->
                                   {-# LINE 663 "./src-ag/Order.ag" #-}
                                   map ((,) _LOC)  _rulesIlocVars ++
                                   map ((,) _INST) _rulesIinstVars ++
                                   map ((,) _LHS)  _inhnames ++
                                   concat [map ((,) nm) (Map.keys as) | (nm,_,as) <- _childrenIattributes]
                                   {-# LINE 2699 "dist/build/Order.hs"#-}
   {-# INLINE rule344 #-}
   {-# LINE 667 "./src-ag/Order.ag" #-}
   rule344 = \ ((_lhsIinh) :: Attributes) ->
                                   {-# LINE 667 "./src-ag/Order.ag" #-}
                                   Map.keys _lhsIinh
                                   {-# LINE 2705 "dist/build/Order.hs"#-}
   {-# INLINE rule345 #-}
   {-# LINE 668 "./src-ag/Order.ag" #-}
   rule345 = \ ((_lhsIsyn) :: Attributes) ->
                                   {-# LINE 668 "./src-ag/Order.ag" #-}
                                   Map.keys _lhsIsyn
                                   {-# LINE 2711 "dist/build/Order.hs"#-}
   {-# INLINE rule346 #-}
   rule346 = \ ((_rulesIdirectDep) :: Seq Edge) ->
     _rulesIdirectDep
   {-# INLINE rule347 #-}
   rule347 = \ ((_childrenIerrors) :: Seq Error) ((_rulesIerrors) :: Seq Error) ->
     _childrenIerrors Seq.>< _rulesIerrors
   {-# INLINE rule348 #-}
   rule348 = \ ((_rulesIinstDep) :: Seq Edge) ->
     _rulesIinstDep
   {-# INLINE rule349 #-}
   rule349 = \ ((_rulesInAutoRules) :: Int) ->
     _rulesInAutoRules
   {-# INLINE rule350 #-}
   rule350 = \ ((_rulesInExplicitRules) :: Int) ->
     _rulesInExplicitRules
   {-# INLINE rule351 #-}
   rule351 = \ _allfields ->
     _allfields
   {-# INLINE rule352 #-}
   rule352 = \ ((_lhsIallnts) :: [Identifier]) ->
     _lhsIallnts
   {-# INLINE rule353 #-}
   rule353 = \ _attrs ->
     _attrs
   {-# INLINE rule354 #-}
   rule354 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule355 #-}
   rule355 = \ ((_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# INLINE rule356 #-}
   rule356 = \ _mergeMap ->
     _mergeMap
   {-# INLINE rule357 #-}
   rule357 = \ ((_lhsInt) :: Identifier) ->
     _lhsInt
   {-# INLINE rule358 #-}
   rule358 = \ ((_lhsIo_unbox) :: Bool) ->
     _lhsIo_unbox
   {-# INLINE rule359 #-}
   rule359 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
   {-# INLINE rule360 #-}
   rule360 = \ ((_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
   {-# INLINE rule361 #-}
   rule361 = \ _allfields ->
     _allfields
   {-# INLINE rule362 #-}
   rule362 = \ ((_lhsIallnts) :: [Identifier]) ->
     _lhsIallnts
   {-# INLINE rule363 #-}
   rule363 = \ _altAttrs ->
     _altAttrs
   {-# INLINE rule364 #-}
   rule364 = \ _attrs ->
     _attrs
   {-# INLINE rule365 #-}
   rule365 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule366 #-}
   rule366 = \ _mergeMap ->
     _mergeMap
   {-# INLINE rule367 #-}
   rule367 = \ ((_lhsInt) :: Identifier) ->
     _lhsInt
   {-# INLINE rule368 #-}
   rule368 = \ ((_lhsIo_case) :: Bool) ->
     _lhsIo_case
   {-# INLINE rule369 #-}
   rule369 = \ ((_lhsIo_cata) :: Bool) ->
     _lhsIo_cata
   {-# INLINE rule370 #-}
   rule370 = \ ((_lhsIo_dovisit) :: Bool) ->
     _lhsIo_dovisit
   {-# INLINE rule371 #-}
   rule371 = \ ((_lhsIo_newtypes) :: Bool) ->
     _lhsIo_newtypes
   {-# INLINE rule372 #-}
   rule372 = \ ((_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# INLINE rule373 #-}
   rule373 = \ ((_lhsIo_sem) :: Bool) ->
     _lhsIo_sem
   {-# INLINE rule374 #-}
   rule374 = \ ((_lhsIo_sig) :: Bool) ->
     _lhsIo_sig
   {-# INLINE rule375 #-}
   rule375 = \ ((_lhsIo_wantvisit) :: Bool) ->
     _lhsIo_wantvisit
   {-# INLINE rule376 #-}
   rule376 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule377 #-}
   rule377 = \ ((_lhsIprefix) :: String) ->
     _lhsIprefix
   {-# INLINE rule378 #-}
   rule378 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn

-- Productions -------------------------------------------------
-- wrapper
data Inh_Productions  = Inh_Productions { allnts_Inh_Productions :: ([Identifier]), aroundMap_Inh_Productions :: (Map ConstructorIdent (Map Identifier [Expression])), cVisitsMap_Inh_Productions :: (CVisitsMap), inh_Inh_Productions :: (Attributes), inhMap_Inh_Productions :: (Map Identifier Attributes), manualAttrDepMap_Inh_Productions :: (AttrOrderMap), mergeMap_Inh_Productions :: (Map ConstructorIdent (Map Identifier (Identifier,[Identifier]))), nt_Inh_Productions :: (Identifier), o_case_Inh_Productions :: (Bool), o_cata_Inh_Productions :: (Bool), o_dovisit_Inh_Productions :: (Bool), o_newtypes_Inh_Productions :: (Bool), o_rename_Inh_Productions :: (Bool), o_sem_Inh_Productions :: (Bool), o_sig_Inh_Productions :: (Bool), o_unbox_Inh_Productions :: (Bool), o_wantvisit_Inh_Productions :: (Bool), options_Inh_Productions :: (Options), prefix_Inh_Productions :: (String), syn_Inh_Productions :: (Attributes), synMap_Inh_Productions :: (Map Identifier Attributes), vcount_Inh_Productions :: (Int) }
data Syn_Productions  = Syn_Productions { additionalDep_Syn_Productions :: (Seq Edge), aroundDep_Syn_Productions :: (Seq Edge), cProductions_Syn_Productions :: (CProductions), cons_Syn_Productions :: ([ConstructorIdent]), directDep_Syn_Productions :: (Seq Edge), errors_Syn_Productions :: (Seq Error), instDep_Syn_Productions :: (Seq Edge), mergeDep_Syn_Productions :: (Seq Edge), nAutoRules_Syn_Productions :: (Int), nExplicitRules_Syn_Productions :: (Int), rules_Syn_Productions :: (Seq (Vertex,CRule)), vcount_Syn_Productions :: (Int) }
{-# INLINABLE wrap_Productions #-}
wrap_Productions :: T_Productions  -> Inh_Productions  -> (Syn_Productions )
wrap_Productions (T_Productions act) (Inh_Productions _lhsIallnts _lhsIaroundMap _lhsIcVisitsMap _lhsIinh _lhsIinhMap _lhsImanualAttrDepMap _lhsImergeMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_unbox _lhsIo_wantvisit _lhsIoptions _lhsIprefix _lhsIsyn _lhsIsynMap _lhsIvcount) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Productions_vIn28 _lhsIallnts _lhsIaroundMap _lhsIcVisitsMap _lhsIinh _lhsIinhMap _lhsImanualAttrDepMap _lhsImergeMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_unbox _lhsIo_wantvisit _lhsIoptions _lhsIprefix _lhsIsyn _lhsIsynMap _lhsIvcount
        (T_Productions_vOut28 _lhsOadditionalDep _lhsOaroundDep _lhsOcProductions _lhsOcons _lhsOdirectDep _lhsOerrors _lhsOinstDep _lhsOmergeDep _lhsOnAutoRules _lhsOnExplicitRules _lhsOrules _lhsOvcount) <- return (inv_Productions_s29 sem arg)
        return (Syn_Productions _lhsOadditionalDep _lhsOaroundDep _lhsOcProductions _lhsOcons _lhsOdirectDep _lhsOerrors _lhsOinstDep _lhsOmergeDep _lhsOnAutoRules _lhsOnExplicitRules _lhsOrules _lhsOvcount)
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
data T_Productions_vOut28  = T_Productions_vOut28 (Seq Edge) (Seq Edge) (CProductions) ([ConstructorIdent]) (Seq Edge) (Seq Error) (Seq Edge) (Seq Edge) (Int) (Int) (Seq (Vertex,CRule)) (Int)
{-# NOINLINE sem_Productions_Cons #-}
sem_Productions_Cons :: T_Production  -> T_Productions  -> T_Productions 
sem_Productions_Cons arg_hd_ arg_tl_ = T_Productions (return st29) where
   {-# NOINLINE st29 #-}
   st29 = let
      v28 :: T_Productions_v28 
      v28 = \ (T_Productions_vIn28 _lhsIallnts _lhsIaroundMap _lhsIcVisitsMap _lhsIinh _lhsIinhMap _lhsImanualAttrDepMap _lhsImergeMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_unbox _lhsIo_wantvisit _lhsIoptions _lhsIprefix _lhsIsyn _lhsIsynMap _lhsIvcount) -> ( let
         _hdX26 = Control.Monad.Identity.runIdentity (attach_T_Production (arg_hd_))
         _tlX29 = Control.Monad.Identity.runIdentity (attach_T_Productions (arg_tl_))
         (T_Production_vOut25 _hdIadditionalDep _hdIaroundDep _hdIcProduction _hdIcons _hdIdirectDep _hdIerrors _hdIinstDep _hdImergeDep _hdInAutoRules _hdInExplicitRules _hdIrules _hdIvcount) = inv_Production_s26 _hdX26 (T_Production_vIn25 _hdOallnts _hdOaroundMap _hdOcVisitsMap _hdOinh _hdOinhMap _hdOmanualAttrDepMap _hdOmergeMap _hdOnt _hdOo_case _hdOo_cata _hdOo_dovisit _hdOo_newtypes _hdOo_rename _hdOo_sem _hdOo_sig _hdOo_unbox _hdOo_wantvisit _hdOoptions _hdOprefix _hdOsyn _hdOsynMap _hdOvcount)
         (T_Productions_vOut28 _tlIadditionalDep _tlIaroundDep _tlIcProductions _tlIcons _tlIdirectDep _tlIerrors _tlIinstDep _tlImergeDep _tlInAutoRules _tlInExplicitRules _tlIrules _tlIvcount) = inv_Productions_s29 _tlX29 (T_Productions_vIn28 _tlOallnts _tlOaroundMap _tlOcVisitsMap _tlOinh _tlOinhMap _tlOmanualAttrDepMap _tlOmergeMap _tlOnt _tlOo_case _tlOo_cata _tlOo_dovisit _tlOo_newtypes _tlOo_rename _tlOo_sem _tlOo_sig _tlOo_unbox _tlOo_wantvisit _tlOoptions _tlOprefix _tlOsyn _tlOsynMap _tlOvcount)
         _lhsOcProductions :: CProductions
         _lhsOcProductions = rule379 _hdIcProduction _tlIcProductions
         _lhsOadditionalDep :: Seq Edge
         _lhsOadditionalDep = rule380 _hdIadditionalDep _tlIadditionalDep
         _lhsOaroundDep :: Seq Edge
         _lhsOaroundDep = rule381 _hdIaroundDep _tlIaroundDep
         _lhsOcons :: [ConstructorIdent]
         _lhsOcons = rule382 _hdIcons _tlIcons
         _lhsOdirectDep :: Seq Edge
         _lhsOdirectDep = rule383 _hdIdirectDep _tlIdirectDep
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule384 _hdIerrors _tlIerrors
         _lhsOinstDep :: Seq Edge
         _lhsOinstDep = rule385 _hdIinstDep _tlIinstDep
         _lhsOmergeDep :: Seq Edge
         _lhsOmergeDep = rule386 _hdImergeDep _tlImergeDep
         _lhsOnAutoRules :: Int
         _lhsOnAutoRules = rule387 _hdInAutoRules _tlInAutoRules
         _lhsOnExplicitRules :: Int
         _lhsOnExplicitRules = rule388 _hdInExplicitRules _tlInExplicitRules
         _lhsOrules :: Seq (Vertex,CRule)
         _lhsOrules = rule389 _hdIrules _tlIrules
         _lhsOvcount :: Int
         _lhsOvcount = rule390 _tlIvcount
         _hdOallnts = rule391 _lhsIallnts
         _hdOaroundMap = rule392 _lhsIaroundMap
         _hdOcVisitsMap = rule393 _lhsIcVisitsMap
         _hdOinh = rule394 _lhsIinh
         _hdOinhMap = rule395 _lhsIinhMap
         _hdOmanualAttrDepMap = rule396 _lhsImanualAttrDepMap
         _hdOmergeMap = rule397 _lhsImergeMap
         _hdOnt = rule398 _lhsInt
         _hdOo_case = rule399 _lhsIo_case
         _hdOo_cata = rule400 _lhsIo_cata
         _hdOo_dovisit = rule401 _lhsIo_dovisit
         _hdOo_newtypes = rule402 _lhsIo_newtypes
         _hdOo_rename = rule403 _lhsIo_rename
         _hdOo_sem = rule404 _lhsIo_sem
         _hdOo_sig = rule405 _lhsIo_sig
         _hdOo_unbox = rule406 _lhsIo_unbox
         _hdOo_wantvisit = rule407 _lhsIo_wantvisit
         _hdOoptions = rule408 _lhsIoptions
         _hdOprefix = rule409 _lhsIprefix
         _hdOsyn = rule410 _lhsIsyn
         _hdOsynMap = rule411 _lhsIsynMap
         _hdOvcount = rule412 _lhsIvcount
         _tlOallnts = rule413 _lhsIallnts
         _tlOaroundMap = rule414 _lhsIaroundMap
         _tlOcVisitsMap = rule415 _lhsIcVisitsMap
         _tlOinh = rule416 _lhsIinh
         _tlOinhMap = rule417 _lhsIinhMap
         _tlOmanualAttrDepMap = rule418 _lhsImanualAttrDepMap
         _tlOmergeMap = rule419 _lhsImergeMap
         _tlOnt = rule420 _lhsInt
         _tlOo_case = rule421 _lhsIo_case
         _tlOo_cata = rule422 _lhsIo_cata
         _tlOo_dovisit = rule423 _lhsIo_dovisit
         _tlOo_newtypes = rule424 _lhsIo_newtypes
         _tlOo_rename = rule425 _lhsIo_rename
         _tlOo_sem = rule426 _lhsIo_sem
         _tlOo_sig = rule427 _lhsIo_sig
         _tlOo_unbox = rule428 _lhsIo_unbox
         _tlOo_wantvisit = rule429 _lhsIo_wantvisit
         _tlOoptions = rule430 _lhsIoptions
         _tlOprefix = rule431 _lhsIprefix
         _tlOsyn = rule432 _lhsIsyn
         _tlOsynMap = rule433 _lhsIsynMap
         _tlOvcount = rule434 _hdIvcount
         __result_ = T_Productions_vOut28 _lhsOadditionalDep _lhsOaroundDep _lhsOcProductions _lhsOcons _lhsOdirectDep _lhsOerrors _lhsOinstDep _lhsOmergeDep _lhsOnAutoRules _lhsOnExplicitRules _lhsOrules _lhsOvcount
         in __result_ )
     in C_Productions_s29 v28
   {-# INLINE rule379 #-}
   {-# LINE 631 "./src-ag/Order.ag" #-}
   rule379 = \ ((_hdIcProduction) :: CProduction) ((_tlIcProductions) :: CProductions) ->
                                {-# LINE 631 "./src-ag/Order.ag" #-}
                                _hdIcProduction : _tlIcProductions
                                {-# LINE 2929 "dist/build/Order.hs"#-}
   {-# INLINE rule380 #-}
   rule380 = \ ((_hdIadditionalDep) :: Seq Edge) ((_tlIadditionalDep) :: Seq Edge) ->
     _hdIadditionalDep Seq.>< _tlIadditionalDep
   {-# INLINE rule381 #-}
   rule381 = \ ((_hdIaroundDep) :: Seq Edge) ((_tlIaroundDep) :: Seq Edge) ->
     _hdIaroundDep Seq.>< _tlIaroundDep
   {-# INLINE rule382 #-}
   rule382 = \ ((_hdIcons) :: [ConstructorIdent]) ((_tlIcons) :: [ConstructorIdent]) ->
     _hdIcons ++ _tlIcons
   {-# INLINE rule383 #-}
   rule383 = \ ((_hdIdirectDep) :: Seq Edge) ((_tlIdirectDep) :: Seq Edge) ->
     _hdIdirectDep Seq.>< _tlIdirectDep
   {-# INLINE rule384 #-}
   rule384 = \ ((_hdIerrors) :: Seq Error) ((_tlIerrors) :: Seq Error) ->
     _hdIerrors Seq.>< _tlIerrors
   {-# INLINE rule385 #-}
   rule385 = \ ((_hdIinstDep) :: Seq Edge) ((_tlIinstDep) :: Seq Edge) ->
     _hdIinstDep Seq.>< _tlIinstDep
   {-# INLINE rule386 #-}
   rule386 = \ ((_hdImergeDep) :: Seq Edge) ((_tlImergeDep) :: Seq Edge) ->
     _hdImergeDep Seq.>< _tlImergeDep
   {-# INLINE rule387 #-}
   rule387 = \ ((_hdInAutoRules) :: Int) ((_tlInAutoRules) :: Int) ->
     _hdInAutoRules + _tlInAutoRules
   {-# INLINE rule388 #-}
   rule388 = \ ((_hdInExplicitRules) :: Int) ((_tlInExplicitRules) :: Int) ->
     _hdInExplicitRules + _tlInExplicitRules
   {-# INLINE rule389 #-}
   rule389 = \ ((_hdIrules) :: Seq (Vertex,CRule)) ((_tlIrules) :: Seq (Vertex,CRule)) ->
     _hdIrules Seq.>< _tlIrules
   {-# INLINE rule390 #-}
   rule390 = \ ((_tlIvcount) :: Int) ->
     _tlIvcount
   {-# INLINE rule391 #-}
   rule391 = \ ((_lhsIallnts) :: [Identifier]) ->
     _lhsIallnts
   {-# INLINE rule392 #-}
   rule392 = \ ((_lhsIaroundMap) :: Map ConstructorIdent (Map Identifier [Expression])) ->
     _lhsIaroundMap
   {-# INLINE rule393 #-}
   rule393 = \ ((_lhsIcVisitsMap) :: CVisitsMap) ->
     _lhsIcVisitsMap
   {-# INLINE rule394 #-}
   rule394 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule395 #-}
   rule395 = \ ((_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# INLINE rule396 #-}
   rule396 = \ ((_lhsImanualAttrDepMap) :: AttrOrderMap) ->
     _lhsImanualAttrDepMap
   {-# INLINE rule397 #-}
   rule397 = \ ((_lhsImergeMap) :: Map ConstructorIdent (Map Identifier (Identifier,[Identifier]))) ->
     _lhsImergeMap
   {-# INLINE rule398 #-}
   rule398 = \ ((_lhsInt) :: Identifier) ->
     _lhsInt
   {-# INLINE rule399 #-}
   rule399 = \ ((_lhsIo_case) :: Bool) ->
     _lhsIo_case
   {-# INLINE rule400 #-}
   rule400 = \ ((_lhsIo_cata) :: Bool) ->
     _lhsIo_cata
   {-# INLINE rule401 #-}
   rule401 = \ ((_lhsIo_dovisit) :: Bool) ->
     _lhsIo_dovisit
   {-# INLINE rule402 #-}
   rule402 = \ ((_lhsIo_newtypes) :: Bool) ->
     _lhsIo_newtypes
   {-# INLINE rule403 #-}
   rule403 = \ ((_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# INLINE rule404 #-}
   rule404 = \ ((_lhsIo_sem) :: Bool) ->
     _lhsIo_sem
   {-# INLINE rule405 #-}
   rule405 = \ ((_lhsIo_sig) :: Bool) ->
     _lhsIo_sig
   {-# INLINE rule406 #-}
   rule406 = \ ((_lhsIo_unbox) :: Bool) ->
     _lhsIo_unbox
   {-# INLINE rule407 #-}
   rule407 = \ ((_lhsIo_wantvisit) :: Bool) ->
     _lhsIo_wantvisit
   {-# INLINE rule408 #-}
   rule408 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule409 #-}
   rule409 = \ ((_lhsIprefix) :: String) ->
     _lhsIprefix
   {-# INLINE rule410 #-}
   rule410 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
   {-# INLINE rule411 #-}
   rule411 = \ ((_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
   {-# INLINE rule412 #-}
   rule412 = \ ((_lhsIvcount) :: Int) ->
     _lhsIvcount
   {-# INLINE rule413 #-}
   rule413 = \ ((_lhsIallnts) :: [Identifier]) ->
     _lhsIallnts
   {-# INLINE rule414 #-}
   rule414 = \ ((_lhsIaroundMap) :: Map ConstructorIdent (Map Identifier [Expression])) ->
     _lhsIaroundMap
   {-# INLINE rule415 #-}
   rule415 = \ ((_lhsIcVisitsMap) :: CVisitsMap) ->
     _lhsIcVisitsMap
   {-# INLINE rule416 #-}
   rule416 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule417 #-}
   rule417 = \ ((_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# INLINE rule418 #-}
   rule418 = \ ((_lhsImanualAttrDepMap) :: AttrOrderMap) ->
     _lhsImanualAttrDepMap
   {-# INLINE rule419 #-}
   rule419 = \ ((_lhsImergeMap) :: Map ConstructorIdent (Map Identifier (Identifier,[Identifier]))) ->
     _lhsImergeMap
   {-# INLINE rule420 #-}
   rule420 = \ ((_lhsInt) :: Identifier) ->
     _lhsInt
   {-# INLINE rule421 #-}
   rule421 = \ ((_lhsIo_case) :: Bool) ->
     _lhsIo_case
   {-# INLINE rule422 #-}
   rule422 = \ ((_lhsIo_cata) :: Bool) ->
     _lhsIo_cata
   {-# INLINE rule423 #-}
   rule423 = \ ((_lhsIo_dovisit) :: Bool) ->
     _lhsIo_dovisit
   {-# INLINE rule424 #-}
   rule424 = \ ((_lhsIo_newtypes) :: Bool) ->
     _lhsIo_newtypes
   {-# INLINE rule425 #-}
   rule425 = \ ((_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# INLINE rule426 #-}
   rule426 = \ ((_lhsIo_sem) :: Bool) ->
     _lhsIo_sem
   {-# INLINE rule427 #-}
   rule427 = \ ((_lhsIo_sig) :: Bool) ->
     _lhsIo_sig
   {-# INLINE rule428 #-}
   rule428 = \ ((_lhsIo_unbox) :: Bool) ->
     _lhsIo_unbox
   {-# INLINE rule429 #-}
   rule429 = \ ((_lhsIo_wantvisit) :: Bool) ->
     _lhsIo_wantvisit
   {-# INLINE rule430 #-}
   rule430 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule431 #-}
   rule431 = \ ((_lhsIprefix) :: String) ->
     _lhsIprefix
   {-# INLINE rule432 #-}
   rule432 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
   {-# INLINE rule433 #-}
   rule433 = \ ((_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
   {-# INLINE rule434 #-}
   rule434 = \ ((_hdIvcount) :: Int) ->
     _hdIvcount
{-# NOINLINE sem_Productions_Nil #-}
sem_Productions_Nil ::  T_Productions 
sem_Productions_Nil  = T_Productions (return st29) where
   {-# NOINLINE st29 #-}
   st29 = let
      v28 :: T_Productions_v28 
      v28 = \ (T_Productions_vIn28 _lhsIallnts _lhsIaroundMap _lhsIcVisitsMap _lhsIinh _lhsIinhMap _lhsImanualAttrDepMap _lhsImergeMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_unbox _lhsIo_wantvisit _lhsIoptions _lhsIprefix _lhsIsyn _lhsIsynMap _lhsIvcount) -> ( let
         _lhsOcProductions :: CProductions
         _lhsOcProductions = rule435  ()
         _lhsOadditionalDep :: Seq Edge
         _lhsOadditionalDep = rule436  ()
         _lhsOaroundDep :: Seq Edge
         _lhsOaroundDep = rule437  ()
         _lhsOcons :: [ConstructorIdent]
         _lhsOcons = rule438  ()
         _lhsOdirectDep :: Seq Edge
         _lhsOdirectDep = rule439  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule440  ()
         _lhsOinstDep :: Seq Edge
         _lhsOinstDep = rule441  ()
         _lhsOmergeDep :: Seq Edge
         _lhsOmergeDep = rule442  ()
         _lhsOnAutoRules :: Int
         _lhsOnAutoRules = rule443  ()
         _lhsOnExplicitRules :: Int
         _lhsOnExplicitRules = rule444  ()
         _lhsOrules :: Seq (Vertex,CRule)
         _lhsOrules = rule445  ()
         _lhsOvcount :: Int
         _lhsOvcount = rule446 _lhsIvcount
         __result_ = T_Productions_vOut28 _lhsOadditionalDep _lhsOaroundDep _lhsOcProductions _lhsOcons _lhsOdirectDep _lhsOerrors _lhsOinstDep _lhsOmergeDep _lhsOnAutoRules _lhsOnExplicitRules _lhsOrules _lhsOvcount
         in __result_ )
     in C_Productions_s29 v28
   {-# INLINE rule435 #-}
   {-# LINE 632 "./src-ag/Order.ag" #-}
   rule435 = \  (_ :: ()) ->
                                {-# LINE 632 "./src-ag/Order.ag" #-}
                                []
                                {-# LINE 3134 "dist/build/Order.hs"#-}
   {-# INLINE rule436 #-}
   rule436 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule437 #-}
   rule437 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule438 #-}
   rule438 = \  (_ :: ()) ->
     []
   {-# INLINE rule439 #-}
   rule439 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule440 #-}
   rule440 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule441 #-}
   rule441 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule442 #-}
   rule442 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule443 #-}
   rule443 = \  (_ :: ()) ->
     0
   {-# INLINE rule444 #-}
   rule444 = \  (_ :: ()) ->
     0
   {-# INLINE rule445 #-}
   rule445 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule446 #-}
   rule446 = \ ((_lhsIvcount) :: Int) ->
     _lhsIvcount

-- Rule --------------------------------------------------------
-- wrapper
data Inh_Rule  = Inh_Rule { allTypeSigs_Inh_Rule :: (Map Identifier Type), allfields_Inh_Rule :: ([(Identifier,Type,ChildKind)]), allnts_Inh_Rule :: ([Identifier]), altAttrs_Inh_Rule :: (Map AltAttr Vertex), attrs_Inh_Rule :: ([(Identifier,Identifier)]), childInhs_Inh_Rule :: (Map Identifier Attributes), childNts_Inh_Rule :: (Map Identifier NontermIdent), con_Inh_Rule :: (Identifier), inh_Inh_Rule :: (Attributes), inhsOfChildren_Inh_Rule :: (Map Identifier Attributes), mergeMap_Inh_Rule :: (Map Identifier (Identifier,[Identifier])), nt_Inh_Rule :: (Identifier), o_case_Inh_Rule :: (Bool), o_cata_Inh_Rule :: (Bool), o_dovisit_Inh_Rule :: (Bool), o_newtypes_Inh_Rule :: (Bool), o_rename_Inh_Rule :: (Bool), o_sem_Inh_Rule :: (Bool), o_sig_Inh_Rule :: (Bool), o_wantvisit_Inh_Rule :: (Bool), options_Inh_Rule :: (Options), prefix_Inh_Rule :: (String), syn_Inh_Rule :: (Attributes), synsOfChildren_Inh_Rule :: (Map Identifier Attributes) }
data Syn_Rule  = Syn_Rule { directDep_Syn_Rule :: (Seq Edge), errors_Syn_Rule :: (Seq Error), gathAltAttrs_Syn_Rule :: ([AltAttr]), gathRules_Syn_Rule :: (Seq CRule), instDep_Syn_Rule :: (Seq Edge), instVars_Syn_Rule :: ([Identifier]), locVars_Syn_Rule :: ([Identifier]), nAutoRules_Syn_Rule :: (Int), nExplicitRules_Syn_Rule :: (Int) }
{-# INLINABLE wrap_Rule #-}
wrap_Rule :: T_Rule  -> Inh_Rule  -> (Syn_Rule )
wrap_Rule (T_Rule act) (Inh_Rule _lhsIallTypeSigs _lhsIallfields _lhsIallnts _lhsIaltAttrs _lhsIattrs _lhsIchildInhs _lhsIchildNts _lhsIcon _lhsIinh _lhsIinhsOfChildren _lhsImergeMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_wantvisit _lhsIoptions _lhsIprefix _lhsIsyn _lhsIsynsOfChildren) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Rule_vIn31 _lhsIallTypeSigs _lhsIallfields _lhsIallnts _lhsIaltAttrs _lhsIattrs _lhsIchildInhs _lhsIchildNts _lhsIcon _lhsIinh _lhsIinhsOfChildren _lhsImergeMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_wantvisit _lhsIoptions _lhsIprefix _lhsIsyn _lhsIsynsOfChildren
        (T_Rule_vOut31 _lhsOdirectDep _lhsOerrors _lhsOgathAltAttrs _lhsOgathRules _lhsOinstDep _lhsOinstVars _lhsOlocVars _lhsOnAutoRules _lhsOnExplicitRules) <- return (inv_Rule_s32 sem arg)
        return (Syn_Rule _lhsOdirectDep _lhsOerrors _lhsOgathAltAttrs _lhsOgathRules _lhsOinstDep _lhsOinstVars _lhsOlocVars _lhsOnAutoRules _lhsOnExplicitRules)
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
data T_Rule_vOut31  = T_Rule_vOut31 (Seq Edge) (Seq Error) ([AltAttr]) (Seq CRule) (Seq Edge) ([Identifier]) ([Identifier]) (Int) (Int)
{-# NOINLINE sem_Rule_Rule #-}
sem_Rule_Rule :: (Maybe Identifier) -> T_Pattern  -> T_Expression  -> (Bool) -> (String) -> (Bool) -> (Bool) -> (Bool) -> (Maybe Error) -> (Bool) -> T_Rule 
sem_Rule_Rule arg_mbName_ arg_pattern_ arg_rhs_ arg_owrt_ arg_origin_ arg_explicit_ _ _ _ _ = T_Rule (return st32) where
   {-# NOINLINE st32 #-}
   st32 = let
      v31 :: T_Rule_v31 
      v31 = \ (T_Rule_vIn31 _lhsIallTypeSigs _lhsIallfields _lhsIallnts _lhsIaltAttrs _lhsIattrs _lhsIchildInhs _lhsIchildNts _lhsIcon _lhsIinh _lhsIinhsOfChildren _lhsImergeMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_wantvisit _lhsIoptions _lhsIprefix _lhsIsyn _lhsIsynsOfChildren) -> ( let
         _patternX20 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pattern_))
         _rhsX8 = Control.Monad.Identity.runIdentity (attach_T_Expression (arg_rhs_))
         (T_Pattern_vOut19 _patternIcopy _patternIerrors _patternIgathAltAttrs _patternIinstVars _patternIlocVars _patternIpatternAttrs) = inv_Pattern_s20 _patternX20 (T_Pattern_vIn19 _patternOallTypeSigs _patternOaltAttrs _patternOcon _patternOinh _patternOnt _patternOsyn)
         (T_Expression_vOut7 _rhsIallRhsVars _rhsIcopy _rhsIerrors _rhsItextLines _rhsIusedAttrs _rhsIusedFields _rhsIusedLocals) = inv_Expression_s8 _rhsX8 (T_Expression_vIn7 _rhsOallfields _rhsOallnts _rhsOattrs _rhsOcon _rhsOmergeMap _rhsOnt _rhsOoptions)
         _lhsOnExplicitRules :: Int
         _lhsOnExplicitRules = rule447 arg_explicit_
         _lhsOnAutoRules :: Int
         _lhsOnAutoRules = rule448 arg_origin_
         _defines = rule449 _lhsIallTypeSigs _lhsIaltAttrs _lhsIchildInhs _lhsIsyn _patternIpatternAttrs
         _gathRules = rule450 _defines _lhsIchildNts _lhsIcon _lhsInt _patternIcopy _rhsIallRhsVars _rhsItextLines arg_explicit_ arg_mbName_ arg_origin_ arg_owrt_
         _lhsOdirectDep :: Seq Edge
         _lhsOdirectDep = rule451 _defines _lhsIaltAttrs _rhsIusedAttrs _rhsIusedFields _rhsIusedLocals
         _instDep1 = rule452 _defines _lhsIaltAttrs _lhsIsynsOfChildren
         _instDep2 = rule453 _defines _lhsIaltAttrs _lhsIinhsOfChildren
         _lhsOinstDep :: Seq Edge
         _lhsOinstDep = rule454 _instDep1 _instDep2
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule455 _patternIerrors _rhsIerrors
         _lhsOgathAltAttrs :: [AltAttr]
         _lhsOgathAltAttrs = rule456 _patternIgathAltAttrs
         _lhsOgathRules :: Seq CRule
         _lhsOgathRules = rule457 _gathRules
         _lhsOinstVars :: [Identifier]
         _lhsOinstVars = rule458 _patternIinstVars
         _lhsOlocVars :: [Identifier]
         _lhsOlocVars = rule459 _patternIlocVars
         _patternOallTypeSigs = rule460 _lhsIallTypeSigs
         _patternOaltAttrs = rule461 _lhsIaltAttrs
         _patternOcon = rule462 _lhsIcon
         _patternOinh = rule463 _lhsIinh
         _patternOnt = rule464 _lhsInt
         _patternOsyn = rule465 _lhsIsyn
         _rhsOallfields = rule466 _lhsIallfields
         _rhsOallnts = rule467 _lhsIallnts
         _rhsOattrs = rule468 _lhsIattrs
         _rhsOcon = rule469 _lhsIcon
         _rhsOmergeMap = rule470 _lhsImergeMap
         _rhsOnt = rule471 _lhsInt
         _rhsOoptions = rule472 _lhsIoptions
         __result_ = T_Rule_vOut31 _lhsOdirectDep _lhsOerrors _lhsOgathAltAttrs _lhsOgathRules _lhsOinstDep _lhsOinstVars _lhsOlocVars _lhsOnAutoRules _lhsOnExplicitRules
         in __result_ )
     in C_Rule_s32 v31
   {-# INLINE rule447 #-}
   {-# LINE 64 "./src-ag/Order.ag" #-}
   rule447 = \ explicit_ ->
                                 {-# LINE 64 "./src-ag/Order.ag" #-}
                                 if explicit_
                                 then 1
                                 else 0
                                 {-# LINE 3255 "dist/build/Order.hs"#-}
   {-# INLINE rule448 #-}
   {-# LINE 67 "./src-ag/Order.ag" #-}
   rule448 = \ origin_ ->
                             {-# LINE 67 "./src-ag/Order.ag" #-}
                             if startsWith "use rule" origin_ || startsWith "copy rule" origin_
                             then 1
                             else 0
                             {-# LINE 3263 "dist/build/Order.hs"#-}
   {-# INLINE rule449 #-}
   {-# LINE 220 "./src-ag/Order.ag" #-}
   rule449 = \ ((_lhsIallTypeSigs) :: Map Identifier Type) ((_lhsIaltAttrs) :: Map AltAttr Vertex) ((_lhsIchildInhs) :: Map Identifier Attributes) ((_lhsIsyn) :: Attributes) ((_patternIpatternAttrs) :: [(Identifier,Identifier,Bool)]) ->
                           {-# LINE 220 "./src-ag/Order.ag" #-}
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
                           {-# LINE 3280 "dist/build/Order.hs"#-}
   {-# INLINE rule450 #-}
   {-# LINE 234 "./src-ag/Order.ag" #-}
   rule450 = \ _defines ((_lhsIchildNts) :: Map Identifier NontermIdent) ((_lhsIcon) :: Identifier) ((_lhsInt) :: Identifier) ((_patternIcopy) :: Pattern) ((_rhsIallRhsVars) :: Set (Identifier,Identifier)) ((_rhsItextLines) :: [String]) explicit_ mbName_ origin_ owrt_ ->
                              {-# LINE 234 "./src-ag/Order.ag" #-}
                              let childnt field = Map.lookup field _lhsIchildNts
                              in Seq.fromList [ CRule attr False True _lhsInt _lhsIcon field (childnt field) tp _patternIcopy _rhsItextLines _defines owrt_ origin_ _rhsIallRhsVars explicit_ mbName_
                                              | (field,attr,tp) <- Map.elems _defines
                                              ]
                              {-# LINE 3289 "dist/build/Order.hs"#-}
   {-# INLINE rule451 #-}
   {-# LINE 273 "./src-ag/Order.ag" #-}
   rule451 = \ _defines ((_lhsIaltAttrs) :: Map AltAttr Vertex) ((_rhsIusedAttrs) :: [(Identifier,Identifier)]) ((_rhsIusedFields) :: [Identifier]) ((_rhsIusedLocals) :: [Identifier]) ->
                 {-# LINE 273 "./src-ag/Order.ag" #-}
                 let  defined = Map.keys _defines
                      used =  [ Map.lookup (AltAttr field attr True) _lhsIaltAttrs | (field,attr) <- _rhsIusedAttrs]
                              ++ [ Map.lookup (AltAttr _LOC attr True) _lhsIaltAttrs | attr <- _rhsIusedLocals ++ _rhsIusedFields ]
                 in Seq.fromList [ (x,y) | Just x <- used, y <- defined ]
                 {-# LINE 3298 "dist/build/Order.hs"#-}
   {-# INLINE rule452 #-}
   {-# LINE 317 "./src-ag/Order.ag" #-}
   rule452 = \ _defines ((_lhsIaltAttrs) :: Map AltAttr Vertex) ((_lhsIsynsOfChildren) :: Map Identifier Attributes) ->
            {-# LINE 317 "./src-ag/Order.ag" #-}
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
            {-# LINE 3313 "dist/build/Order.hs"#-}
   {-# INLINE rule453 #-}
   {-# LINE 328 "./src-ag/Order.ag" #-}
   rule453 = \ _defines ((_lhsIaltAttrs) :: Map AltAttr Vertex) ((_lhsIinhsOfChildren) :: Map Identifier Attributes) ->
            {-# LINE 328 "./src-ag/Order.ag" #-}
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
            {-# LINE 3328 "dist/build/Order.hs"#-}
   {-# INLINE rule454 #-}
   {-# LINE 338 "./src-ag/Order.ag" #-}
   rule454 = \ _instDep1 _instDep2 ->
                     {-# LINE 338 "./src-ag/Order.ag" #-}
                     _instDep1     Seq.>< _instDep2
                     {-# LINE 3334 "dist/build/Order.hs"#-}
   {-# INLINE rule455 #-}
   rule455 = \ ((_patternIerrors) :: Seq Error) ((_rhsIerrors) :: Seq Error) ->
     _patternIerrors Seq.>< _rhsIerrors
   {-# INLINE rule456 #-}
   rule456 = \ ((_patternIgathAltAttrs) :: [AltAttr]) ->
     _patternIgathAltAttrs
   {-# INLINE rule457 #-}
   rule457 = \ _gathRules ->
     _gathRules
   {-# INLINE rule458 #-}
   rule458 = \ ((_patternIinstVars) :: [Identifier]) ->
     _patternIinstVars
   {-# INLINE rule459 #-}
   rule459 = \ ((_patternIlocVars) :: [Identifier]) ->
     _patternIlocVars
   {-# INLINE rule460 #-}
   rule460 = \ ((_lhsIallTypeSigs) :: Map Identifier Type) ->
     _lhsIallTypeSigs
   {-# INLINE rule461 #-}
   rule461 = \ ((_lhsIaltAttrs) :: Map AltAttr Vertex) ->
     _lhsIaltAttrs
   {-# INLINE rule462 #-}
   rule462 = \ ((_lhsIcon) :: Identifier) ->
     _lhsIcon
   {-# INLINE rule463 #-}
   rule463 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule464 #-}
   rule464 = \ ((_lhsInt) :: Identifier) ->
     _lhsInt
   {-# INLINE rule465 #-}
   rule465 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
   {-# INLINE rule466 #-}
   rule466 = \ ((_lhsIallfields) :: [(Identifier,Type,ChildKind)]) ->
     _lhsIallfields
   {-# INLINE rule467 #-}
   rule467 = \ ((_lhsIallnts) :: [Identifier]) ->
     _lhsIallnts
   {-# INLINE rule468 #-}
   rule468 = \ ((_lhsIattrs) :: [(Identifier,Identifier)]) ->
     _lhsIattrs
   {-# INLINE rule469 #-}
   rule469 = \ ((_lhsIcon) :: Identifier) ->
     _lhsIcon
   {-# INLINE rule470 #-}
   rule470 = \ ((_lhsImergeMap) :: Map Identifier (Identifier,[Identifier])) ->
     _lhsImergeMap
   {-# INLINE rule471 #-}
   rule471 = \ ((_lhsInt) :: Identifier) ->
     _lhsInt
   {-# INLINE rule472 #-}
   rule472 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions

-- Rules -------------------------------------------------------
-- wrapper
data Inh_Rules  = Inh_Rules { allTypeSigs_Inh_Rules :: (Map Identifier Type), allfields_Inh_Rules :: ([(Identifier,Type,ChildKind)]), allnts_Inh_Rules :: ([Identifier]), altAttrs_Inh_Rules :: (Map AltAttr Vertex), attrs_Inh_Rules :: ([(Identifier,Identifier)]), childInhs_Inh_Rules :: (Map Identifier Attributes), childNts_Inh_Rules :: (Map Identifier NontermIdent), con_Inh_Rules :: (Identifier), inh_Inh_Rules :: (Attributes), inhsOfChildren_Inh_Rules :: (Map Identifier Attributes), mergeMap_Inh_Rules :: (Map Identifier (Identifier,[Identifier])), nt_Inh_Rules :: (Identifier), o_case_Inh_Rules :: (Bool), o_cata_Inh_Rules :: (Bool), o_dovisit_Inh_Rules :: (Bool), o_newtypes_Inh_Rules :: (Bool), o_rename_Inh_Rules :: (Bool), o_sem_Inh_Rules :: (Bool), o_sig_Inh_Rules :: (Bool), o_wantvisit_Inh_Rules :: (Bool), options_Inh_Rules :: (Options), prefix_Inh_Rules :: (String), syn_Inh_Rules :: (Attributes), synsOfChildren_Inh_Rules :: (Map Identifier Attributes) }
data Syn_Rules  = Syn_Rules { directDep_Syn_Rules :: (Seq Edge), errors_Syn_Rules :: (Seq Error), gathAltAttrs_Syn_Rules :: ([AltAttr]), gathRules_Syn_Rules :: (Seq CRule), instDep_Syn_Rules :: (Seq Edge), instVars_Syn_Rules :: ([Identifier]), locVars_Syn_Rules :: ([Identifier]), nAutoRules_Syn_Rules :: (Int), nExplicitRules_Syn_Rules :: (Int) }
{-# INLINABLE wrap_Rules #-}
wrap_Rules :: T_Rules  -> Inh_Rules  -> (Syn_Rules )
wrap_Rules (T_Rules act) (Inh_Rules _lhsIallTypeSigs _lhsIallfields _lhsIallnts _lhsIaltAttrs _lhsIattrs _lhsIchildInhs _lhsIchildNts _lhsIcon _lhsIinh _lhsIinhsOfChildren _lhsImergeMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_wantvisit _lhsIoptions _lhsIprefix _lhsIsyn _lhsIsynsOfChildren) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Rules_vIn34 _lhsIallTypeSigs _lhsIallfields _lhsIallnts _lhsIaltAttrs _lhsIattrs _lhsIchildInhs _lhsIchildNts _lhsIcon _lhsIinh _lhsIinhsOfChildren _lhsImergeMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_wantvisit _lhsIoptions _lhsIprefix _lhsIsyn _lhsIsynsOfChildren
        (T_Rules_vOut34 _lhsOdirectDep _lhsOerrors _lhsOgathAltAttrs _lhsOgathRules _lhsOinstDep _lhsOinstVars _lhsOlocVars _lhsOnAutoRules _lhsOnExplicitRules) <- return (inv_Rules_s35 sem arg)
        return (Syn_Rules _lhsOdirectDep _lhsOerrors _lhsOgathAltAttrs _lhsOgathRules _lhsOinstDep _lhsOinstVars _lhsOlocVars _lhsOnAutoRules _lhsOnExplicitRules)
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
data T_Rules_vOut34  = T_Rules_vOut34 (Seq Edge) (Seq Error) ([AltAttr]) (Seq CRule) (Seq Edge) ([Identifier]) ([Identifier]) (Int) (Int)
{-# NOINLINE sem_Rules_Cons #-}
sem_Rules_Cons :: T_Rule  -> T_Rules  -> T_Rules 
sem_Rules_Cons arg_hd_ arg_tl_ = T_Rules (return st35) where
   {-# NOINLINE st35 #-}
   st35 = let
      v34 :: T_Rules_v34 
      v34 = \ (T_Rules_vIn34 _lhsIallTypeSigs _lhsIallfields _lhsIallnts _lhsIaltAttrs _lhsIattrs _lhsIchildInhs _lhsIchildNts _lhsIcon _lhsIinh _lhsIinhsOfChildren _lhsImergeMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_wantvisit _lhsIoptions _lhsIprefix _lhsIsyn _lhsIsynsOfChildren) -> ( let
         _hdX32 = Control.Monad.Identity.runIdentity (attach_T_Rule (arg_hd_))
         _tlX35 = Control.Monad.Identity.runIdentity (attach_T_Rules (arg_tl_))
         (T_Rule_vOut31 _hdIdirectDep _hdIerrors _hdIgathAltAttrs _hdIgathRules _hdIinstDep _hdIinstVars _hdIlocVars _hdInAutoRules _hdInExplicitRules) = inv_Rule_s32 _hdX32 (T_Rule_vIn31 _hdOallTypeSigs _hdOallfields _hdOallnts _hdOaltAttrs _hdOattrs _hdOchildInhs _hdOchildNts _hdOcon _hdOinh _hdOinhsOfChildren _hdOmergeMap _hdOnt _hdOo_case _hdOo_cata _hdOo_dovisit _hdOo_newtypes _hdOo_rename _hdOo_sem _hdOo_sig _hdOo_wantvisit _hdOoptions _hdOprefix _hdOsyn _hdOsynsOfChildren)
         (T_Rules_vOut34 _tlIdirectDep _tlIerrors _tlIgathAltAttrs _tlIgathRules _tlIinstDep _tlIinstVars _tlIlocVars _tlInAutoRules _tlInExplicitRules) = inv_Rules_s35 _tlX35 (T_Rules_vIn34 _tlOallTypeSigs _tlOallfields _tlOallnts _tlOaltAttrs _tlOattrs _tlOchildInhs _tlOchildNts _tlOcon _tlOinh _tlOinhsOfChildren _tlOmergeMap _tlOnt _tlOo_case _tlOo_cata _tlOo_dovisit _tlOo_newtypes _tlOo_rename _tlOo_sem _tlOo_sig _tlOo_wantvisit _tlOoptions _tlOprefix _tlOsyn _tlOsynsOfChildren)
         _lhsOdirectDep :: Seq Edge
         _lhsOdirectDep = rule473 _hdIdirectDep _tlIdirectDep
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule474 _hdIerrors _tlIerrors
         _lhsOgathAltAttrs :: [AltAttr]
         _lhsOgathAltAttrs = rule475 _hdIgathAltAttrs _tlIgathAltAttrs
         _lhsOgathRules :: Seq CRule
         _lhsOgathRules = rule476 _hdIgathRules _tlIgathRules
         _lhsOinstDep :: Seq Edge
         _lhsOinstDep = rule477 _hdIinstDep _tlIinstDep
         _lhsOinstVars :: [Identifier]
         _lhsOinstVars = rule478 _hdIinstVars _tlIinstVars
         _lhsOlocVars :: [Identifier]
         _lhsOlocVars = rule479 _hdIlocVars _tlIlocVars
         _lhsOnAutoRules :: Int
         _lhsOnAutoRules = rule480 _hdInAutoRules _tlInAutoRules
         _lhsOnExplicitRules :: Int
         _lhsOnExplicitRules = rule481 _hdInExplicitRules _tlInExplicitRules
         _hdOallTypeSigs = rule482 _lhsIallTypeSigs
         _hdOallfields = rule483 _lhsIallfields
         _hdOallnts = rule484 _lhsIallnts
         _hdOaltAttrs = rule485 _lhsIaltAttrs
         _hdOattrs = rule486 _lhsIattrs
         _hdOchildInhs = rule487 _lhsIchildInhs
         _hdOchildNts = rule488 _lhsIchildNts
         _hdOcon = rule489 _lhsIcon
         _hdOinh = rule490 _lhsIinh
         _hdOinhsOfChildren = rule491 _lhsIinhsOfChildren
         _hdOmergeMap = rule492 _lhsImergeMap
         _hdOnt = rule493 _lhsInt
         _hdOo_case = rule494 _lhsIo_case
         _hdOo_cata = rule495 _lhsIo_cata
         _hdOo_dovisit = rule496 _lhsIo_dovisit
         _hdOo_newtypes = rule497 _lhsIo_newtypes
         _hdOo_rename = rule498 _lhsIo_rename
         _hdOo_sem = rule499 _lhsIo_sem
         _hdOo_sig = rule500 _lhsIo_sig
         _hdOo_wantvisit = rule501 _lhsIo_wantvisit
         _hdOoptions = rule502 _lhsIoptions
         _hdOprefix = rule503 _lhsIprefix
         _hdOsyn = rule504 _lhsIsyn
         _hdOsynsOfChildren = rule505 _lhsIsynsOfChildren
         _tlOallTypeSigs = rule506 _lhsIallTypeSigs
         _tlOallfields = rule507 _lhsIallfields
         _tlOallnts = rule508 _lhsIallnts
         _tlOaltAttrs = rule509 _lhsIaltAttrs
         _tlOattrs = rule510 _lhsIattrs
         _tlOchildInhs = rule511 _lhsIchildInhs
         _tlOchildNts = rule512 _lhsIchildNts
         _tlOcon = rule513 _lhsIcon
         _tlOinh = rule514 _lhsIinh
         _tlOinhsOfChildren = rule515 _lhsIinhsOfChildren
         _tlOmergeMap = rule516 _lhsImergeMap
         _tlOnt = rule517 _lhsInt
         _tlOo_case = rule518 _lhsIo_case
         _tlOo_cata = rule519 _lhsIo_cata
         _tlOo_dovisit = rule520 _lhsIo_dovisit
         _tlOo_newtypes = rule521 _lhsIo_newtypes
         _tlOo_rename = rule522 _lhsIo_rename
         _tlOo_sem = rule523 _lhsIo_sem
         _tlOo_sig = rule524 _lhsIo_sig
         _tlOo_wantvisit = rule525 _lhsIo_wantvisit
         _tlOoptions = rule526 _lhsIoptions
         _tlOprefix = rule527 _lhsIprefix
         _tlOsyn = rule528 _lhsIsyn
         _tlOsynsOfChildren = rule529 _lhsIsynsOfChildren
         __result_ = T_Rules_vOut34 _lhsOdirectDep _lhsOerrors _lhsOgathAltAttrs _lhsOgathRules _lhsOinstDep _lhsOinstVars _lhsOlocVars _lhsOnAutoRules _lhsOnExplicitRules
         in __result_ )
     in C_Rules_s35 v34
   {-# INLINE rule473 #-}
   rule473 = \ ((_hdIdirectDep) :: Seq Edge) ((_tlIdirectDep) :: Seq Edge) ->
     _hdIdirectDep Seq.>< _tlIdirectDep
   {-# INLINE rule474 #-}
   rule474 = \ ((_hdIerrors) :: Seq Error) ((_tlIerrors) :: Seq Error) ->
     _hdIerrors Seq.>< _tlIerrors
   {-# INLINE rule475 #-}
   rule475 = \ ((_hdIgathAltAttrs) :: [AltAttr]) ((_tlIgathAltAttrs) :: [AltAttr]) ->
     _hdIgathAltAttrs ++ _tlIgathAltAttrs
   {-# INLINE rule476 #-}
   rule476 = \ ((_hdIgathRules) :: Seq CRule) ((_tlIgathRules) :: Seq CRule) ->
     _hdIgathRules Seq.>< _tlIgathRules
   {-# INLINE rule477 #-}
   rule477 = \ ((_hdIinstDep) :: Seq Edge) ((_tlIinstDep) :: Seq Edge) ->
     _hdIinstDep Seq.>< _tlIinstDep
   {-# INLINE rule478 #-}
   rule478 = \ ((_hdIinstVars) :: [Identifier]) ((_tlIinstVars) :: [Identifier]) ->
     _hdIinstVars ++ _tlIinstVars
   {-# INLINE rule479 #-}
   rule479 = \ ((_hdIlocVars) :: [Identifier]) ((_tlIlocVars) :: [Identifier]) ->
     _hdIlocVars ++ _tlIlocVars
   {-# INLINE rule480 #-}
   rule480 = \ ((_hdInAutoRules) :: Int) ((_tlInAutoRules) :: Int) ->
     _hdInAutoRules + _tlInAutoRules
   {-# INLINE rule481 #-}
   rule481 = \ ((_hdInExplicitRules) :: Int) ((_tlInExplicitRules) :: Int) ->
     _hdInExplicitRules + _tlInExplicitRules
   {-# INLINE rule482 #-}
   rule482 = \ ((_lhsIallTypeSigs) :: Map Identifier Type) ->
     _lhsIallTypeSigs
   {-# INLINE rule483 #-}
   rule483 = \ ((_lhsIallfields) :: [(Identifier,Type,ChildKind)]) ->
     _lhsIallfields
   {-# INLINE rule484 #-}
   rule484 = \ ((_lhsIallnts) :: [Identifier]) ->
     _lhsIallnts
   {-# INLINE rule485 #-}
   rule485 = \ ((_lhsIaltAttrs) :: Map AltAttr Vertex) ->
     _lhsIaltAttrs
   {-# INLINE rule486 #-}
   rule486 = \ ((_lhsIattrs) :: [(Identifier,Identifier)]) ->
     _lhsIattrs
   {-# INLINE rule487 #-}
   rule487 = \ ((_lhsIchildInhs) :: Map Identifier Attributes) ->
     _lhsIchildInhs
   {-# INLINE rule488 #-}
   rule488 = \ ((_lhsIchildNts) :: Map Identifier NontermIdent) ->
     _lhsIchildNts
   {-# INLINE rule489 #-}
   rule489 = \ ((_lhsIcon) :: Identifier) ->
     _lhsIcon
   {-# INLINE rule490 #-}
   rule490 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule491 #-}
   rule491 = \ ((_lhsIinhsOfChildren) :: Map Identifier Attributes) ->
     _lhsIinhsOfChildren
   {-# INLINE rule492 #-}
   rule492 = \ ((_lhsImergeMap) :: Map Identifier (Identifier,[Identifier])) ->
     _lhsImergeMap
   {-# INLINE rule493 #-}
   rule493 = \ ((_lhsInt) :: Identifier) ->
     _lhsInt
   {-# INLINE rule494 #-}
   rule494 = \ ((_lhsIo_case) :: Bool) ->
     _lhsIo_case
   {-# INLINE rule495 #-}
   rule495 = \ ((_lhsIo_cata) :: Bool) ->
     _lhsIo_cata
   {-# INLINE rule496 #-}
   rule496 = \ ((_lhsIo_dovisit) :: Bool) ->
     _lhsIo_dovisit
   {-# INLINE rule497 #-}
   rule497 = \ ((_lhsIo_newtypes) :: Bool) ->
     _lhsIo_newtypes
   {-# INLINE rule498 #-}
   rule498 = \ ((_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# INLINE rule499 #-}
   rule499 = \ ((_lhsIo_sem) :: Bool) ->
     _lhsIo_sem
   {-# INLINE rule500 #-}
   rule500 = \ ((_lhsIo_sig) :: Bool) ->
     _lhsIo_sig
   {-# INLINE rule501 #-}
   rule501 = \ ((_lhsIo_wantvisit) :: Bool) ->
     _lhsIo_wantvisit
   {-# INLINE rule502 #-}
   rule502 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule503 #-}
   rule503 = \ ((_lhsIprefix) :: String) ->
     _lhsIprefix
   {-# INLINE rule504 #-}
   rule504 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
   {-# INLINE rule505 #-}
   rule505 = \ ((_lhsIsynsOfChildren) :: Map Identifier Attributes) ->
     _lhsIsynsOfChildren
   {-# INLINE rule506 #-}
   rule506 = \ ((_lhsIallTypeSigs) :: Map Identifier Type) ->
     _lhsIallTypeSigs
   {-# INLINE rule507 #-}
   rule507 = \ ((_lhsIallfields) :: [(Identifier,Type,ChildKind)]) ->
     _lhsIallfields
   {-# INLINE rule508 #-}
   rule508 = \ ((_lhsIallnts) :: [Identifier]) ->
     _lhsIallnts
   {-# INLINE rule509 #-}
   rule509 = \ ((_lhsIaltAttrs) :: Map AltAttr Vertex) ->
     _lhsIaltAttrs
   {-# INLINE rule510 #-}
   rule510 = \ ((_lhsIattrs) :: [(Identifier,Identifier)]) ->
     _lhsIattrs
   {-# INLINE rule511 #-}
   rule511 = \ ((_lhsIchildInhs) :: Map Identifier Attributes) ->
     _lhsIchildInhs
   {-# INLINE rule512 #-}
   rule512 = \ ((_lhsIchildNts) :: Map Identifier NontermIdent) ->
     _lhsIchildNts
   {-# INLINE rule513 #-}
   rule513 = \ ((_lhsIcon) :: Identifier) ->
     _lhsIcon
   {-# INLINE rule514 #-}
   rule514 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule515 #-}
   rule515 = \ ((_lhsIinhsOfChildren) :: Map Identifier Attributes) ->
     _lhsIinhsOfChildren
   {-# INLINE rule516 #-}
   rule516 = \ ((_lhsImergeMap) :: Map Identifier (Identifier,[Identifier])) ->
     _lhsImergeMap
   {-# INLINE rule517 #-}
   rule517 = \ ((_lhsInt) :: Identifier) ->
     _lhsInt
   {-# INLINE rule518 #-}
   rule518 = \ ((_lhsIo_case) :: Bool) ->
     _lhsIo_case
   {-# INLINE rule519 #-}
   rule519 = \ ((_lhsIo_cata) :: Bool) ->
     _lhsIo_cata
   {-# INLINE rule520 #-}
   rule520 = \ ((_lhsIo_dovisit) :: Bool) ->
     _lhsIo_dovisit
   {-# INLINE rule521 #-}
   rule521 = \ ((_lhsIo_newtypes) :: Bool) ->
     _lhsIo_newtypes
   {-# INLINE rule522 #-}
   rule522 = \ ((_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# INLINE rule523 #-}
   rule523 = \ ((_lhsIo_sem) :: Bool) ->
     _lhsIo_sem
   {-# INLINE rule524 #-}
   rule524 = \ ((_lhsIo_sig) :: Bool) ->
     _lhsIo_sig
   {-# INLINE rule525 #-}
   rule525 = \ ((_lhsIo_wantvisit) :: Bool) ->
     _lhsIo_wantvisit
   {-# INLINE rule526 #-}
   rule526 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule527 #-}
   rule527 = \ ((_lhsIprefix) :: String) ->
     _lhsIprefix
   {-# INLINE rule528 #-}
   rule528 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
   {-# INLINE rule529 #-}
   rule529 = \ ((_lhsIsynsOfChildren) :: Map Identifier Attributes) ->
     _lhsIsynsOfChildren
{-# NOINLINE sem_Rules_Nil #-}
sem_Rules_Nil ::  T_Rules 
sem_Rules_Nil  = T_Rules (return st35) where
   {-# NOINLINE st35 #-}
   st35 = let
      v34 :: T_Rules_v34 
      v34 = \ (T_Rules_vIn34 _lhsIallTypeSigs _lhsIallfields _lhsIallnts _lhsIaltAttrs _lhsIattrs _lhsIchildInhs _lhsIchildNts _lhsIcon _lhsIinh _lhsIinhsOfChildren _lhsImergeMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_wantvisit _lhsIoptions _lhsIprefix _lhsIsyn _lhsIsynsOfChildren) -> ( let
         _lhsOdirectDep :: Seq Edge
         _lhsOdirectDep = rule530  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule531  ()
         _lhsOgathAltAttrs :: [AltAttr]
         _lhsOgathAltAttrs = rule532  ()
         _lhsOgathRules :: Seq CRule
         _lhsOgathRules = rule533  ()
         _lhsOinstDep :: Seq Edge
         _lhsOinstDep = rule534  ()
         _lhsOinstVars :: [Identifier]
         _lhsOinstVars = rule535  ()
         _lhsOlocVars :: [Identifier]
         _lhsOlocVars = rule536  ()
         _lhsOnAutoRules :: Int
         _lhsOnAutoRules = rule537  ()
         _lhsOnExplicitRules :: Int
         _lhsOnExplicitRules = rule538  ()
         __result_ = T_Rules_vOut34 _lhsOdirectDep _lhsOerrors _lhsOgathAltAttrs _lhsOgathRules _lhsOinstDep _lhsOinstVars _lhsOlocVars _lhsOnAutoRules _lhsOnExplicitRules
         in __result_ )
     in C_Rules_s35 v34
   {-# INLINE rule530 #-}
   rule530 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule531 #-}
   rule531 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule532 #-}
   rule532 = \  (_ :: ()) ->
     []
   {-# INLINE rule533 #-}
   rule533 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule534 #-}
   rule534 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule535 #-}
   rule535 = \  (_ :: ()) ->
     []
   {-# INLINE rule536 #-}
   rule536 = \  (_ :: ()) ->
     []
   {-# INLINE rule537 #-}
   rule537 = \  (_ :: ()) ->
     0
   {-# INLINE rule538 #-}
   rule538 = \  (_ :: ()) ->
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
        let arg = T_TypeSig_vIn37 _lhsItypeSigs
        (T_TypeSig_vOut37 _lhsOtypeSigs) <- return (inv_TypeSig_s38 sem arg)
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
         _lhsOtypeSigs = rule539 _lhsItypeSigs arg_name_ arg_tp_
         __result_ = T_TypeSig_vOut37 _lhsOtypeSigs
         in __result_ )
     in C_TypeSig_s38 v37
   {-# INLINE rule539 #-}
   {-# LINE 536 "./src-ag/Order.ag" #-}
   rule539 = \ ((_lhsItypeSigs) :: Map Identifier Type) name_ tp_ ->
                             {-# LINE 536 "./src-ag/Order.ag" #-}
                             Map.insert name_ tp_ _lhsItypeSigs
                             {-# LINE 3774 "dist/build/Order.hs"#-}

-- TypeSigs ----------------------------------------------------
-- wrapper
data Inh_TypeSigs  = Inh_TypeSigs { typeSigs_Inh_TypeSigs :: (Map Identifier Type) }
data Syn_TypeSigs  = Syn_TypeSigs { typeSigs_Syn_TypeSigs :: (Map Identifier Type) }
{-# INLINABLE wrap_TypeSigs #-}
wrap_TypeSigs :: T_TypeSigs  -> Inh_TypeSigs  -> (Syn_TypeSigs )
wrap_TypeSigs (T_TypeSigs act) (Inh_TypeSigs _lhsItypeSigs) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_TypeSigs_vIn40 _lhsItypeSigs
        (T_TypeSigs_vOut40 _lhsOtypeSigs) <- return (inv_TypeSigs_s41 sem arg)
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
         _lhsOtypeSigs = rule540 _tlItypeSigs
         _hdOtypeSigs = rule541 _lhsItypeSigs
         _tlOtypeSigs = rule542 _hdItypeSigs
         __result_ = T_TypeSigs_vOut40 _lhsOtypeSigs
         in __result_ )
     in C_TypeSigs_s41 v40
   {-# INLINE rule540 #-}
   rule540 = \ ((_tlItypeSigs) :: Map Identifier Type) ->
     _tlItypeSigs
   {-# INLINE rule541 #-}
   rule541 = \ ((_lhsItypeSigs) :: Map Identifier Type) ->
     _lhsItypeSigs
   {-# INLINE rule542 #-}
   rule542 = \ ((_hdItypeSigs) :: Map Identifier Type) ->
     _hdItypeSigs
{-# NOINLINE sem_TypeSigs_Nil #-}
sem_TypeSigs_Nil ::  T_TypeSigs 
sem_TypeSigs_Nil  = T_TypeSigs (return st41) where
   {-# NOINLINE st41 #-}
   st41 = let
      v40 :: T_TypeSigs_v40 
      v40 = \ (T_TypeSigs_vIn40 _lhsItypeSigs) -> ( let
         _lhsOtypeSigs :: Map Identifier Type
         _lhsOtypeSigs = rule543 _lhsItypeSigs
         __result_ = T_TypeSigs_vOut40 _lhsOtypeSigs
         in __result_ )
     in C_TypeSigs_s41 v40
   {-# INLINE rule543 #-}
   rule543 = \ ((_lhsItypeSigs) :: Map Identifier Type) ->
     _lhsItypeSigs
