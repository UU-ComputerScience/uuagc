{-# LANGUAGE Rank2Types, GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Order where
{-# LINE 2 "./src-ag/Patterns.ag" #-}

-- Patterns.ag imports
import UU.Scanner.Position(Pos)
import CommonTypes (ConstructorIdent,Identifier)
{-# LINE 11 "dist/build/Order.hs" #-}

{-# LINE 2 "./src-ag/Expression.ag" #-}

import UU.Scanner.Position(Pos)
import HsToken
{-# LINE 17 "dist/build/Order.hs" #-}

{-# LINE 2 "./src-ag/AbstractSyntax.ag" #-}

-- AbstractSyntax.ag imports
import Data.Set(Set)
import Data.Map(Map)
import Patterns    (Pattern(..),Patterns)
import Expression  (Expression(..))
import Macro --marcos
import CommonTypes
import ErrorMessages
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
{-# LINE 67 "dist/build/Order.hs" #-}
import Control.Monad.Identity (Identity)
import qualified Control.Monad.Identity
{-# LINE 47 "./src-ag/Order.ag" #-}

-- Terminates with an error if the key is not in the map
findWithErr1 :: (Ord k, Show k) => String -> k -> Map k a -> a
findWithErr1 s k
  = Map.findWithDefault (error ("findWithErr1 " ++ s ++ ": key " ++ show k ++ " not in map.")) k

findWithErr2 :: (Ord k, Show k, Show a) => k -> Map k a -> a
findWithErr2 k m
  = Map.findWithDefault (error ("findWithErr2: key " ++ show k ++ " not in map: " ++ show m)) k m
{-# LINE 80 "dist/build/Order.hs" #-}

{-# LINE 72 "./src-ag/Order.ag" #-}

startsWith :: String -> String -> Bool
startsWith k h = k == take (length k) h
{-# LINE 86 "dist/build/Order.hs" #-}

{-# LINE 139 "./src-ag/Order.ag" #-}

getNtName :: Type -> NontermIdent
getNtName (NT nt _ _) = nt
getNtName _           = nullIdent
{-# LINE 93 "dist/build/Order.hs" #-}

{-# LINE 167 "./src-ag/Order.ag" #-}

data AltAttr = AltAttr Identifier Identifier Bool
               deriving (Eq, Ord, Show)
{-# LINE 99 "dist/build/Order.hs" #-}

{-# LINE 237 "./src-ag/Order.ag" #-}

substSelf nt tp
  = case tp of
      NT n tps defor | n == _SELF -> NT nt tps defor
      _                           -> tp

haskellTupel :: [Type] -> Maybe Type
haskellTupel ts =  Just ( Haskell ( '(' : (concat (intersperse "," (map show ts))) ++ ")" ))
{-# LINE 110 "dist/build/Order.hs" #-}

{-# LINE 688 "./src-ag/Order.ag" #-}

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
{-# LINE 230 "dist/build/Order.hs" #-}
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
                       {-# LINE 306 "dist/build/Order.hs"#-}
   {-# INLINE rule1 #-}
   {-# LINE 23 "./src-ag/DistChildAttr.ag" #-}
   rule1 = \ _chnt ((_lhsIinhMap) :: Map Identifier Attributes) ->
                      {-# LINE 23 "./src-ag/DistChildAttr.ag" #-}
                      Map.findWithDefault Map.empty _chnt     _lhsIinhMap
                      {-# LINE 312 "dist/build/Order.hs"#-}
   {-# INLINE rule2 #-}
   {-# LINE 24 "./src-ag/DistChildAttr.ag" #-}
   rule2 = \ _chnt ((_lhsIsynMap) :: Map Identifier Attributes) ->
                      {-# LINE 24 "./src-ag/DistChildAttr.ag" #-}
                      Map.findWithDefault Map.empty _chnt     _lhsIsynMap
                      {-# LINE 318 "dist/build/Order.hs"#-}
   {-# INLINE rule3 #-}
   {-# LINE 178 "./src-ag/Order.ag" #-}
   rule3 = \ _syn tp_ ->
                                {-# LINE 178 "./src-ag/Order.ag" #-}
                                case tp_ of
                                  NT nt _ _ -> Map.null _syn
                                  _         -> True
                                {-# LINE 326 "dist/build/Order.hs"#-}
   {-# INLINE rule4 #-}
   {-# LINE 181 "./src-ag/Order.ag" #-}
   rule4 = \ _maptolocal _syn name_ ->
                                 {-# LINE 181 "./src-ag/Order.ag" #-}
                                 if  _maptolocal
                                     then [ AltAttr _LOC name_ True ]
                                     else [ AltAttr name_ syn True | syn <- Map.keys _syn     ]
                                 {-# LINE 334 "dist/build/Order.hs"#-}
   {-# INLINE rule5 #-}
   {-# LINE 196 "./src-ag/Order.ag" #-}
   rule5 = \ name_ tp_ ->
                        {-# LINE 196 "./src-ag/Order.ag" #-}
                        Seq.singleton (name_,getNtName tp_)
                        {-# LINE 340 "dist/build/Order.hs"#-}
   {-# INLINE rule6 #-}
   {-# LINE 197 "./src-ag/Order.ag" #-}
   rule6 = \ _inh name_ ->
                         {-# LINE 197 "./src-ag/Order.ag" #-}
                         Seq.singleton (name_,_inh    )
                         {-# LINE 346 "dist/build/Order.hs"#-}
   {-# INLINE rule7 #-}
   {-# LINE 213 "./src-ag/Order.ag" #-}
   rule7 = \ ((_lhsIcon) :: Identifier) ((_lhsInt) :: Identifier) _maptolocal _syn name_ tp_ ->
                              {-# LINE 213 "./src-ag/Order.ag" #-}
                              if  _maptolocal
                                  then Seq.singleton (cRuleTerminal name_ _lhsInt _lhsIcon tp_)
                                  else Seq.fromList [ cRuleRhsSyn syn _lhsInt _lhsIcon tp name_ (getNtName tp_) | (syn,tp) <- Map.assocs _syn    ]
                              {-# LINE 354 "dist/build/Order.hs"#-}
   {-# INLINE rule8 #-}
   {-# LINE 345 "./src-ag/Order.ag" #-}
   rule8 = \ _syn name_ ->
                                       {-# LINE 345 "./src-ag/Order.ag" #-}
                                       Map.singleton name_ _syn
                                       {-# LINE 360 "dist/build/Order.hs"#-}
   {-# INLINE rule9 #-}
   {-# LINE 346 "./src-ag/Order.ag" #-}
   rule9 = \ _inh name_ ->
                                       {-# LINE 346 "./src-ag/Order.ag" #-}
                                       Map.singleton name_ _inh
                                       {-# LINE 366 "dist/build/Order.hs"#-}
   {-# INLINE rule10 #-}
   {-# LINE 614 "./src-ag/Order.ag" #-}
   rule10 = \ _inh _maptolocal _syn name_ tp_ ->
                                 {-# LINE 614 "./src-ag/Order.ag" #-}
                                 if  _maptolocal
                                     then []
                                     else [CChildVisit name_ (getNtName tp_) 0 _inh     _syn     True]
                                 {-# LINE 374 "dist/build/Order.hs"#-}
   {-# INLINE rule11 #-}
   {-# LINE 639 "./src-ag/Order.ag" #-}
   rule11 = \ _maptolocal name_ ->
                            {-# LINE 639 "./src-ag/Order.ag" #-}
                            if _maptolocal
                            then [name_]
                            else []
                            {-# LINE 382 "dist/build/Order.hs"#-}
   {-# INLINE rule12 #-}
   {-# LINE 668 "./src-ag/Order.ag" #-}
   rule12 = \ _inh _syn name_ ->
                             {-# LINE 668 "./src-ag/Order.ag" #-}
                             [(name_, _inh    , _syn    )]
                             {-# LINE 388 "dist/build/Order.hs"#-}
   {-# INLINE rule13 #-}
   {-# LINE 672 "./src-ag/Order.ag" #-}
   rule13 = \ kind_ name_ tp_ ->
                        {-# LINE 672 "./src-ag/Order.ag" #-}
                        (name_, tp_, kind_)
                        {-# LINE 394 "dist/build/Order.hs"#-}
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
   {-# LINE 675 "./src-ag/Order.ag" #-}
   rule16 = \ ((_hdIfield) :: (Identifier,Type,ChildKind)) ((_tlIfields) :: [(Identifier,Type,ChildKind)]) ->
                         {-# LINE 675 "./src-ag/Order.ag" #-}
                         _hdIfield : _tlIfields
                         {-# LINE 495 "dist/build/Order.hs"#-}
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
   {-# LINE 676 "./src-ag/Order.ag" #-}
   rule49 = \  (_ :: ()) ->
                         {-# LINE 676 "./src-ag/Order.ag" #-}
                         []
                         {-# LINE 629 "dist/build/Order.hs"#-}
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
data Inh_Expression  = Inh_Expression { allfields_Inh_Expression :: ([(Identifier,Type,ChildKind)]), allnts_Inh_Expression :: ([Identifier]), attrs_Inh_Expression :: ([(Identifier,Identifier)]), con_Inh_Expression :: (Identifier), mergeMap_Inh_Expression :: (Map Identifier (Identifier,[Identifier])), nt_Inh_Expression :: (Identifier) }
data Syn_Expression  = Syn_Expression { allRhsVars_Syn_Expression :: (Set (Identifier,Identifier)), copy_Syn_Expression :: (Expression), errors_Syn_Expression :: (Seq Error), textLines_Syn_Expression :: ([String]), usedAttrs_Syn_Expression :: ([(Identifier,Identifier)]), usedFields_Syn_Expression :: ([Identifier]), usedLocals_Syn_Expression :: ([Identifier]) }
{-# INLINABLE wrap_Expression #-}
wrap_Expression :: T_Expression  -> Inh_Expression  -> (Syn_Expression )
wrap_Expression (T_Expression act) (Inh_Expression _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsImergeMap _lhsInt) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Expression_vIn7 _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsImergeMap _lhsInt
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
data T_Expression_vIn7  = T_Expression_vIn7 ([(Identifier,Type,ChildKind)]) ([Identifier]) ([(Identifier,Identifier)]) (Identifier) (Map Identifier (Identifier,[Identifier])) (Identifier)
data T_Expression_vOut7  = T_Expression_vOut7 (Set (Identifier,Identifier)) (Expression) (Seq Error) ([String]) ([(Identifier,Identifier)]) ([Identifier]) ([Identifier])
{-# NOINLINE sem_Expression_Expression #-}
sem_Expression_Expression :: (Pos) -> ([HsToken]) -> T_Expression 
sem_Expression_Expression arg_pos_ arg_tks_ = T_Expression (return st8) where
   {-# NOINLINE st8 #-}
   st8 = let
      v7 :: T_Expression_v7 
      v7 = \ (T_Expression_vIn7 _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsImergeMap _lhsInt) -> ( let
         (_textLines,_usedAttrs,_usedLocals,_usedFields) = rule60 _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsImergeMap _lhsInt arg_tks_
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
   {-# LINE 466 "./src-ag/Order.ag" #-}
   rule60 = \ ((_lhsIallfields) :: [(Identifier,Type,ChildKind)]) ((_lhsIallnts) :: [Identifier]) ((_lhsIattrs) :: [(Identifier,Identifier)]) ((_lhsIcon) :: Identifier) ((_lhsImergeMap) :: Map Identifier (Identifier,[Identifier])) ((_lhsInt) :: Identifier) tks_ ->
                                {-# LINE 466 "./src-ag/Order.ag" #-}
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
                                {-# LINE 742 "dist/build/Order.hs"#-}
   {-# INLINE rule61 #-}
   {-# LINE 488 "./src-ag/Order.ag" #-}
   rule61 = \  (_ :: ()) ->
                               {-# LINE 488 "./src-ag/Order.ag" #-}
                               Seq.empty
                               {-# LINE 748 "dist/build/Order.hs"#-}
   {-# INLINE rule62 #-}
   {-# LINE 489 "./src-ag/Order.ag" #-}
   rule62 = \ _usedAttrs _usedFields _usedLocals ->
                                   {-# LINE 489 "./src-ag/Order.ag" #-}
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
         (T_Nonterminals_vOut16 _nontsIacount _nontsIadditionalDep _nontsIaranges _nontsIaroundDep _nontsIcNonterminals _nontsIdirectDep _nontsIerrors _nontsIinhMap' _nontsIinstDep _nontsImergeDep _nontsInAutoRules _nontsInExplicitRules _nontsInonts _nontsIntattrs _nontsIrules _nontsIsynMap' _nontsIvcount) = inv_Nonterminals_s17 _nontsX17 (T_Nonterminals_vIn16 _nontsOacount _nontsOallnts _nontsOaroundMap _nontsOcInterfaceMap _nontsOcVisitsMap _nontsOinhMap _nontsOmanualAttrDepMap _nontsOmergeMap _nontsOo_case _nontsOo_cata _nontsOo_data _nontsOo_dovisit _nontsOo_newtypes _nontsOo_rename _nontsOo_sem _nontsOo_sig _nontsOo_unbox _nontsOo_wantvisit _nontsOprefix _nontsOsynMap _nontsOvcount)
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
         __result_ = T_Grammar_vOut10 _lhsOerrors _lhsOnAutoRules _lhsOnExplicitRules _lhsOoutput
         in __result_ )
     in C_Grammar_s11 v10
   {-# INLINE rule69 #-}
   {-# LINE 15 "./src-ag/DistChildAttr.ag" #-}
   rule69 = \ ((_nontsIinhMap') :: Map Identifier Attributes) ->
                             {-# LINE 15 "./src-ag/DistChildAttr.ag" #-}
                             _nontsIinhMap'
                             {-# LINE 868 "dist/build/Order.hs"#-}
   {-# INLINE rule70 #-}
   {-# LINE 16 "./src-ag/DistChildAttr.ag" #-}
   rule70 = \ ((_nontsIsynMap') :: Map Identifier Attributes) ->
                             {-# LINE 16 "./src-ag/DistChildAttr.ag" #-}
                             _nontsIsynMap'
                             {-# LINE 874 "dist/build/Order.hs"#-}
   {-# INLINE rule71 #-}
   {-# LINE 124 "./src-ag/Order.ag" #-}
   rule71 = \ _cyclesErrors ((_lhsIoptions) :: Options) ->
                                    {-# LINE 124 "./src-ag/Order.ag" #-}
                                    visit     _lhsIoptions && null _cyclesErrors
                                    {-# LINE 880 "dist/build/Order.hs"#-}
   {-# INLINE rule72 #-}
   {-# LINE 125 "./src-ag/Order.ag" #-}
   rule72 = \ ((_lhsIoptions) :: Options) ->
                                    {-# LINE 125 "./src-ag/Order.ag" #-}
                                    folds     _lhsIoptions
                                    {-# LINE 886 "dist/build/Order.hs"#-}
   {-# INLINE rule73 #-}
   {-# LINE 126 "./src-ag/Order.ag" #-}
   rule73 = \ ((_lhsIoptions) :: Options) ->
                                    {-# LINE 126 "./src-ag/Order.ag" #-}
                                    dataTypes _lhsIoptions
                                    {-# LINE 892 "dist/build/Order.hs"#-}
   {-# INLINE rule74 #-}
   {-# LINE 127 "./src-ag/Order.ag" #-}
   rule74 = \ ((_lhsIoptions) :: Options) ->
                                    {-# LINE 127 "./src-ag/Order.ag" #-}
                                    typeSigs  _lhsIoptions
                                    {-# LINE 898 "dist/build/Order.hs"#-}
   {-# INLINE rule75 #-}
   {-# LINE 128 "./src-ag/Order.ag" #-}
   rule75 = \ ((_lhsIoptions) :: Options) ->
                                    {-# LINE 128 "./src-ag/Order.ag" #-}
                                    semfuns   _lhsIoptions
                                    {-# LINE 904 "dist/build/Order.hs"#-}
   {-# INLINE rule76 #-}
   {-# LINE 129 "./src-ag/Order.ag" #-}
   rule76 = \ ((_lhsIoptions) :: Options) ->
                                    {-# LINE 129 "./src-ag/Order.ag" #-}
                                    rename    _lhsIoptions
                                    {-# LINE 910 "dist/build/Order.hs"#-}
   {-# INLINE rule77 #-}
   {-# LINE 130 "./src-ag/Order.ag" #-}
   rule77 = \ ((_lhsIoptions) :: Options) ->
                                    {-# LINE 130 "./src-ag/Order.ag" #-}
                                    newtypes  _lhsIoptions
                                    {-# LINE 916 "dist/build/Order.hs"#-}
   {-# INLINE rule78 #-}
   {-# LINE 131 "./src-ag/Order.ag" #-}
   rule78 = \ ((_lhsIoptions) :: Options) ->
                                      {-# LINE 131 "./src-ag/Order.ag" #-}
                                      visit   _lhsIoptions
                                      {-# LINE 922 "dist/build/Order.hs"#-}
   {-# INLINE rule79 #-}
   {-# LINE 132 "./src-ag/Order.ag" #-}
   rule79 = \ ((_lhsIoptions) :: Options) ->
                                    {-# LINE 132 "./src-ag/Order.ag" #-}
                                    unbox     _lhsIoptions
                                    {-# LINE 928 "dist/build/Order.hs"#-}
   {-# INLINE rule80 #-}
   {-# LINE 133 "./src-ag/Order.ag" #-}
   rule80 = \ ((_lhsIoptions) :: Options) ->
                                    {-# LINE 133 "./src-ag/Order.ag" #-}
                                    cases     _lhsIoptions
                                    {-# LINE 934 "dist/build/Order.hs"#-}
   {-# INLINE rule81 #-}
   {-# LINE 134 "./src-ag/Order.ag" #-}
   rule81 = \ ((_lhsIoptions) :: Options) ->
                                    {-# LINE 134 "./src-ag/Order.ag" #-}
                                    prefix    _lhsIoptions
                                    {-# LINE 940 "dist/build/Order.hs"#-}
   {-# INLINE rule82 #-}
   {-# LINE 260 "./src-ag/Order.ag" #-}
   rule82 = \  (_ :: ()) ->
                               {-# LINE 260 "./src-ag/Order.ag" #-}
                               0
                               {-# LINE 946 "dist/build/Order.hs"#-}
   {-# INLINE rule83 #-}
   {-# LINE 286 "./src-ag/Order.ag" #-}
   rule83 = \ manualAttrOrderMap_ ->
                                 {-# LINE 286 "./src-ag/Order.ag" #-}
                                 manualAttrOrderMap_
                                 {-# LINE 952 "dist/build/Order.hs"#-}
   {-# INLINE rule84 #-}
   {-# LINE 415 "./src-ag/Order.ag" #-}
   rule84 = \ aroundsMap_ ->
                                 {-# LINE 415 "./src-ag/Order.ag" #-}
                                 aroundsMap_
                                 {-# LINE 958 "dist/build/Order.hs"#-}
   {-# INLINE rule85 #-}
   {-# LINE 504 "./src-ag/Order.ag" #-}
   rule85 = \  (_ :: ()) ->
                             {-# LINE 504 "./src-ag/Order.ag" #-}
                             0
                             {-# LINE 964 "dist/build/Order.hs"#-}
   {-# INLINE rule86 #-}
   {-# LINE 542 "./src-ag/Order.ag" #-}
   rule86 = \ ((_nontsIrules) :: Seq (Vertex,CRule)) ((_nontsIvcount) :: Int) ->
                              {-# LINE 542 "./src-ag/Order.ag" #-}
                              Array.array (0,_nontsIvcount-1) (toList _nontsIrules)
                              {-# LINE 970 "dist/build/Order.hs"#-}
   {-# INLINE rule87 #-}
   {-# LINE 543 "./src-ag/Order.ag" #-}
   rule87 = \ ((_nontsIacount) :: Int) ((_nontsIntattrs) :: Seq (Vertex,NTAttr)) ->
                              {-# LINE 543 "./src-ag/Order.ag" #-}
                              Array.array (0,_nontsIacount-1) (toList _nontsIntattrs)
                              {-# LINE 976 "dist/build/Order.hs"#-}
   {-# INLINE rule88 #-}
   {-# LINE 544 "./src-ag/Order.ag" #-}
   rule88 = \ ((_nontsIntattrs) :: Seq (Vertex,NTAttr)) ->
                               {-# LINE 544 "./src-ag/Order.ag" #-}
                               Map.fromList (map swap (toList _nontsIntattrs))
                               {-# LINE 982 "dist/build/Order.hs"#-}
   {-# INLINE rule89 #-}
   {-# LINE 545 "./src-ag/Order.ag" #-}
   rule89 = \ _attrVertex ((_nontsIrules) :: Seq (Vertex,CRule)) ->
                              {-# LINE 545 "./src-ag/Order.ag" #-}
                              [ (s, maybe (-1) (\v -> findWithErr1 "Grammar.tdpToTds" v _attrVertex) (ntattr cr))
                              | (s,cr) <- toList _nontsIrules]
                              {-# LINE 989 "dist/build/Order.hs"#-}
   {-# INLINE rule90 #-}
   {-# LINE 547 "./src-ag/Order.ag" #-}
   rule90 = \ _tdpToTds ->
                               {-# LINE 547 "./src-ag/Order.ag" #-}
                               let  eq (_,v) (_,v') = v == v'
                                    conv ((s,v):svs)  | v == -1 = Nothing
                                                      | otherwise = Just (v,s:map fst svs)
                               in mapMaybe conv (eqClasses eq _tdpToTds)
                               {-# LINE 998 "dist/build/Order.hs"#-}
   {-# INLINE rule91 #-}
   {-# LINE 551 "./src-ag/Order.ag" #-}
   rule91 = \ ((_nontsIadditionalDep) :: Seq Edge) ((_nontsIdirectDep) :: Seq Edge) ->
                              {-# LINE 551 "./src-ag/Order.ag" #-}
                              toList (_nontsIdirectDep Seq.>< _nontsIadditionalDep)
                              {-# LINE 1004 "dist/build/Order.hs"#-}
   {-# INLINE rule92 #-}
   {-# LINE 552 "./src-ag/Order.ag" #-}
   rule92 = \ ((_nontsIinstDep) :: Seq Edge) ->
                              {-# LINE 552 "./src-ag/Order.ag" #-}
                              toList _nontsIinstDep
                              {-# LINE 1010 "dist/build/Order.hs"#-}
   {-# INLINE rule93 #-}
   {-# LINE 553 "./src-ag/Order.ag" #-}
   rule93 = \ ((_nontsIaroundDep) :: Seq Edge) ->
                              {-# LINE 553 "./src-ag/Order.ag" #-}
                              toList _nontsIaroundDep
                              {-# LINE 1016 "dist/build/Order.hs"#-}
   {-# INLINE rule94 #-}
   {-# LINE 554 "./src-ag/Order.ag" #-}
   rule94 = \ ((_nontsImergeDep) :: Seq Edge) ->
                              {-# LINE 554 "./src-ag/Order.ag" #-}
                              toList _nontsImergeDep
                              {-# LINE 1022 "dist/build/Order.hs"#-}
   {-# INLINE rule95 #-}
   {-# LINE 555 "./src-ag/Order.ag" #-}
   rule95 = \ _attrTable ((_nontsIacount) :: Int) ((_nontsIaranges) :: Seq (Int,Int,Int)) ((_nontsInonts) :: [(NontermIdent,[ConstructorIdent])]) ((_nontsIvcount) :: Int) _ruleTable _tdpToTds _tdsToTdp wrappers_ ->
                              {-# LINE 555 "./src-ag/Order.ag" #-}
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
                              {-# LINE 1037 "dist/build/Order.hs"#-}
   {-# INLINE rule96 #-}
   {-# LINE 567 "./src-ag/Order.ag" #-}
   rule96 = \ _aroundDep _attrTable _directDep _info _instDep ((_lhsIoptions) :: Options) _mergeDep _ruleTable ->
                                {-# LINE 567 "./src-ag/Order.ag" #-}
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
                                {-# LINE 1063 "dist/build/Order.hs"#-}
   {-# INLINE rule97 #-}
   {-# LINE 588 "./src-ag/Order.ag" #-}
   rule97 = \ _cyclesErrors ((_lhsIoptions) :: Options) ((_nontsIerrors) :: Seq Error) ->
                           {-# LINE 588 "./src-ag/Order.ag" #-}
                           (if withCycle _lhsIoptions then Seq.fromList _cyclesErrors else Seq.empty)
                            Seq.>< _nontsIerrors
                           {-# LINE 1070 "dist/build/Order.hs"#-}
   {-# INLINE rule98 #-}
   {-# LINE 620 "./src-ag/Order.ag" #-}
   rule98 = \ _aroundMap _mergeMap ((_nontsIcNonterminals) :: CNonterminals) _o_dovisit contextMap_ derivings_ paramMap_ pragmas_ quantMap_ typeSyns_ wrappers_ ->
                             {-# LINE 620 "./src-ag/Order.ag" #-}
                             CGrammar typeSyns_ derivings_ wrappers_ _nontsIcNonterminals pragmas_ paramMap_ contextMap_ quantMap_ _aroundMap     _mergeMap     _o_dovisit
                             {-# LINE 1076 "dist/build/Order.hs"#-}
   {-# INLINE rule99 #-}
   {-# LINE 633 "./src-ag/Order.ag" #-}
   rule99 = \ aroundsMap_ ->
                               {-# LINE 633 "./src-ag/Order.ag" #-}
                               Map.map (Map.map Map.keysSet) aroundsMap_
                               {-# LINE 1082 "dist/build/Order.hs"#-}
   {-# INLINE rule100 #-}
   {-# LINE 634 "./src-ag/Order.ag" #-}
   rule100 = \ mergeMap_ ->
                               {-# LINE 634 "./src-ag/Order.ag" #-}
                               Map.map (Map.map (Map.map (\(nt,srcs,_) -> (nt,srcs)))) mergeMap_
                               {-# LINE 1088 "dist/build/Order.hs"#-}
   {-# INLINE rule101 #-}
   {-# LINE 651 "./src-ag/Order.ag" #-}
   rule101 = \ ((_nontsInonts) :: [(NontermIdent,[ConstructorIdent])]) ->
                             {-# LINE 651 "./src-ag/Order.ag" #-}
                             map fst (_nontsInonts)
                             {-# LINE 1094 "dist/build/Order.hs"#-}
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

-- Nonterminal -------------------------------------------------
-- wrapper
data Inh_Nonterminal  = Inh_Nonterminal { acount_Inh_Nonterminal :: (Int), allnts_Inh_Nonterminal :: ([Identifier]), aroundMap_Inh_Nonterminal :: (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))), cInterfaceMap_Inh_Nonterminal :: (CInterfaceMap), cVisitsMap_Inh_Nonterminal :: (CVisitsMap), inhMap_Inh_Nonterminal :: (Map Identifier Attributes), manualAttrDepMap_Inh_Nonterminal :: (AttrOrderMap), mergeMap_Inh_Nonterminal :: (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier])))), o_case_Inh_Nonterminal :: (Bool), o_cata_Inh_Nonterminal :: (Bool), o_data_Inh_Nonterminal :: (Bool), o_dovisit_Inh_Nonterminal :: (Bool), o_newtypes_Inh_Nonterminal :: (Bool), o_rename_Inh_Nonterminal :: (Bool), o_sem_Inh_Nonterminal :: (Bool), o_sig_Inh_Nonterminal :: (Bool), o_unbox_Inh_Nonterminal :: (Bool), o_wantvisit_Inh_Nonterminal :: (Bool), prefix_Inh_Nonterminal :: (String), synMap_Inh_Nonterminal :: (Map Identifier Attributes), vcount_Inh_Nonterminal :: (Int) }
data Syn_Nonterminal  = Syn_Nonterminal { acount_Syn_Nonterminal :: (Int), additionalDep_Syn_Nonterminal :: (Seq Edge), aranges_Syn_Nonterminal :: (Seq (Int,Int,Int)), aroundDep_Syn_Nonterminal :: (Seq Edge), cNonterminal_Syn_Nonterminal :: (CNonterminal), directDep_Syn_Nonterminal :: (Seq Edge), errors_Syn_Nonterminal :: (Seq Error), inhMap'_Syn_Nonterminal :: (Map Identifier Attributes), instDep_Syn_Nonterminal :: (Seq Edge), mergeDep_Syn_Nonterminal :: (Seq Edge), nAutoRules_Syn_Nonterminal :: (Int), nExplicitRules_Syn_Nonterminal :: (Int), nonts_Syn_Nonterminal :: ([(NontermIdent,[ConstructorIdent])]), ntattrs_Syn_Nonterminal :: (Seq (Vertex,NTAttr)), rules_Syn_Nonterminal :: (Seq (Vertex,CRule)), synMap'_Syn_Nonterminal :: (Map Identifier Attributes), vcount_Syn_Nonterminal :: (Int) }
{-# INLINABLE wrap_Nonterminal #-}
wrap_Nonterminal :: T_Nonterminal  -> Inh_Nonterminal  -> (Syn_Nonterminal )
wrap_Nonterminal (T_Nonterminal act) (Inh_Nonterminal _lhsIacount _lhsIallnts _lhsIaroundMap _lhsIcInterfaceMap _lhsIcVisitsMap _lhsIinhMap _lhsImanualAttrDepMap _lhsImergeMap _lhsIo_case _lhsIo_cata _lhsIo_data _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_unbox _lhsIo_wantvisit _lhsIprefix _lhsIsynMap _lhsIvcount) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Nonterminal_vIn13 _lhsIacount _lhsIallnts _lhsIaroundMap _lhsIcInterfaceMap _lhsIcVisitsMap _lhsIinhMap _lhsImanualAttrDepMap _lhsImergeMap _lhsIo_case _lhsIo_cata _lhsIo_data _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_unbox _lhsIo_wantvisit _lhsIprefix _lhsIsynMap _lhsIvcount
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
data T_Nonterminal_vIn13  = T_Nonterminal_vIn13 (Int) ([Identifier]) (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) (CInterfaceMap) (CVisitsMap) (Map Identifier Attributes) (AttrOrderMap) (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier])))) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (String) (Map Identifier Attributes) (Int)
data T_Nonterminal_vOut13  = T_Nonterminal_vOut13 (Int) (Seq Edge) (Seq (Int,Int,Int)) (Seq Edge) (CNonterminal) (Seq Edge) (Seq Error) (Map Identifier Attributes) (Seq Edge) (Seq Edge) (Int) (Int) ([(NontermIdent,[ConstructorIdent])]) (Seq (Vertex,NTAttr)) (Seq (Vertex,CRule)) (Map Identifier Attributes) (Int)
{-# NOINLINE sem_Nonterminal_Nonterminal #-}
sem_Nonterminal_Nonterminal :: (NontermIdent) -> ([Identifier]) -> (Attributes) -> (Attributes) -> T_Productions  -> T_Nonterminal 
sem_Nonterminal_Nonterminal arg_nt_ arg_params_ arg_inh_ arg_syn_ arg_prods_ = T_Nonterminal (return st14) where
   {-# NOINLINE st14 #-}
   st14 = let
      v13 :: T_Nonterminal_v13 
      v13 = \ (T_Nonterminal_vIn13 _lhsIacount _lhsIallnts _lhsIaroundMap _lhsIcInterfaceMap _lhsIcVisitsMap _lhsIinhMap _lhsImanualAttrDepMap _lhsImergeMap _lhsIo_case _lhsIo_cata _lhsIo_data _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_unbox _lhsIo_wantvisit _lhsIprefix _lhsIsynMap _lhsIvcount) -> ( let
         _prodsX29 = Control.Monad.Identity.runIdentity (attach_T_Productions (arg_prods_))
         (T_Productions_vOut28 _prodsIadditionalDep _prodsIaroundDep _prodsIcProductions _prodsIcons _prodsIdirectDep _prodsIerrors _prodsIinstDep _prodsImergeDep _prodsInAutoRules _prodsInExplicitRules _prodsIrules _prodsIvcount) = inv_Productions_s29 _prodsX29 (T_Productions_vIn28 _prodsOallnts _prodsOaroundMap _prodsOcVisitsMap _prodsOinh _prodsOinhMap _prodsOmanualAttrDepMap _prodsOmergeMap _prodsOnt _prodsOo_case _prodsOo_cata _prodsOo_dovisit _prodsOo_newtypes _prodsOo_rename _prodsOo_sem _prodsOo_sig _prodsOo_unbox _prodsOo_wantvisit _prodsOprefix _prodsOsyn _prodsOsynMap _prodsOvcount)
         _lhsOinhMap' :: Map Identifier Attributes
         _lhsOinhMap' = rule108 arg_inh_ arg_nt_
         _lhsOsynMap' :: Map Identifier Attributes
         _lhsOsynMap' = rule109 arg_nt_ arg_syn_
         _prodsOnt = rule110 arg_nt_
         _prodsOinh = rule111 arg_inh_
         _prodsOsyn = rule112 arg_syn_
         _mergeMap = rule113 _lhsImergeMap arg_nt_
         _aroundMap = rule114 _lhsIaroundMap arg_nt_
         _ntattrs = rule115 arg_inh_ arg_nt_ arg_syn_
         _lhsOntattrs :: Seq (Vertex,NTAttr)
         _lhsOntattrs = rule116 _lhsIacount _ntattrs
         _lhsOacount :: Int
         _lhsOacount = rule117 _lhsIacount arg_inh_ arg_syn_
         _lhsOaranges :: Seq (Int,Int,Int)
         _lhsOaranges = rule118 _lhsIacount arg_inh_ arg_syn_
         _lhsOnonts :: [(NontermIdent,[ConstructorIdent])]
         _lhsOnonts = rule119 _prodsIcons arg_nt_
         _cInter = rule120 _lhsIcInterfaceMap _lhsIo_dovisit arg_inh_ arg_nt_ arg_syn_
         _lhsOcNonterminal :: CNonterminal
         _lhsOcNonterminal = rule121 _cInter _prodsIcProductions arg_inh_ arg_nt_ arg_params_ arg_syn_
         _lhsOadditionalDep :: Seq Edge
         _lhsOadditionalDep = rule122 _prodsIadditionalDep
         _lhsOaroundDep :: Seq Edge
         _lhsOaroundDep = rule123 _prodsIaroundDep
         _lhsOdirectDep :: Seq Edge
         _lhsOdirectDep = rule124 _prodsIdirectDep
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule125 _prodsIerrors
         _lhsOinstDep :: Seq Edge
         _lhsOinstDep = rule126 _prodsIinstDep
         _lhsOmergeDep :: Seq Edge
         _lhsOmergeDep = rule127 _prodsImergeDep
         _lhsOnAutoRules :: Int
         _lhsOnAutoRules = rule128 _prodsInAutoRules
         _lhsOnExplicitRules :: Int
         _lhsOnExplicitRules = rule129 _prodsInExplicitRules
         _lhsOrules :: Seq (Vertex,CRule)
         _lhsOrules = rule130 _prodsIrules
         _lhsOvcount :: Int
         _lhsOvcount = rule131 _prodsIvcount
         _prodsOallnts = rule132 _lhsIallnts
         _prodsOaroundMap = rule133 _aroundMap
         _prodsOcVisitsMap = rule134 _lhsIcVisitsMap
         _prodsOinhMap = rule135 _lhsIinhMap
         _prodsOmanualAttrDepMap = rule136 _lhsImanualAttrDepMap
         _prodsOmergeMap = rule137 _mergeMap
         _prodsOo_case = rule138 _lhsIo_case
         _prodsOo_cata = rule139 _lhsIo_cata
         _prodsOo_dovisit = rule140 _lhsIo_dovisit
         _prodsOo_newtypes = rule141 _lhsIo_newtypes
         _prodsOo_rename = rule142 _lhsIo_rename
         _prodsOo_sem = rule143 _lhsIo_sem
         _prodsOo_sig = rule144 _lhsIo_sig
         _prodsOo_unbox = rule145 _lhsIo_unbox
         _prodsOo_wantvisit = rule146 _lhsIo_wantvisit
         _prodsOprefix = rule147 _lhsIprefix
         _prodsOsynMap = rule148 _lhsIsynMap
         _prodsOvcount = rule149 _lhsIvcount
         __result_ = T_Nonterminal_vOut13 _lhsOacount _lhsOadditionalDep _lhsOaranges _lhsOaroundDep _lhsOcNonterminal _lhsOdirectDep _lhsOerrors _lhsOinhMap' _lhsOinstDep _lhsOmergeDep _lhsOnAutoRules _lhsOnExplicitRules _lhsOnonts _lhsOntattrs _lhsOrules _lhsOsynMap' _lhsOvcount
         in __result_ )
     in C_Nonterminal_s14 v13
   {-# INLINE rule108 #-}
   {-# LINE 7 "./src-ag/DistChildAttr.ag" #-}
   rule108 = \ inh_ nt_ ->
                                 {-# LINE 7 "./src-ag/DistChildAttr.ag" #-}
                                 Map.singleton nt_ inh_
                                 {-# LINE 1220 "dist/build/Order.hs"#-}
   {-# INLINE rule109 #-}
   {-# LINE 8 "./src-ag/DistChildAttr.ag" #-}
   rule109 = \ nt_ syn_ ->
                                 {-# LINE 8 "./src-ag/DistChildAttr.ag" #-}
                                 Map.singleton nt_ syn_
                                 {-# LINE 1226 "dist/build/Order.hs"#-}
   {-# INLINE rule110 #-}
   {-# LINE 98 "./src-ag/Order.ag" #-}
   rule110 = \ nt_ ->
                               {-# LINE 98 "./src-ag/Order.ag" #-}
                               nt_
                               {-# LINE 1232 "dist/build/Order.hs"#-}
   {-# INLINE rule111 #-}
   {-# LINE 101 "./src-ag/Order.ag" #-}
   rule111 = \ inh_ ->
                               {-# LINE 101 "./src-ag/Order.ag" #-}
                               inh_
                               {-# LINE 1238 "dist/build/Order.hs"#-}
   {-# INLINE rule112 #-}
   {-# LINE 102 "./src-ag/Order.ag" #-}
   rule112 = \ syn_ ->
                               {-# LINE 102 "./src-ag/Order.ag" #-}
                               syn_
                               {-# LINE 1244 "dist/build/Order.hs"#-}
   {-# INLINE rule113 #-}
   {-# LINE 358 "./src-ag/Order.ag" #-}
   rule113 = \ ((_lhsImergeMap) :: Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier])))) nt_ ->
                                                {-# LINE 358 "./src-ag/Order.ag" #-}
                                                Map.findWithDefault Map.empty nt_ _lhsImergeMap
                                                {-# LINE 1250 "dist/build/Order.hs"#-}
   {-# INLINE rule114 #-}
   {-# LINE 411 "./src-ag/Order.ag" #-}
   rule114 = \ ((_lhsIaroundMap) :: Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) nt_ ->
                                                 {-# LINE 411 "./src-ag/Order.ag" #-}
                                                 Map.findWithDefault Map.empty nt_ _lhsIaroundMap
                                                 {-# LINE 1256 "dist/build/Order.hs"#-}
   {-# INLINE rule115 #-}
   {-# LINE 507 "./src-ag/Order.ag" #-}
   rule115 = \ inh_ nt_ syn_ ->
                                 {-# LINE 507 "./src-ag/Order.ag" #-}
                                 [ NTAInh nt_ inh tp | (inh,tp) <- Map.assocs inh_ ]
                                 ++ [NTASyn nt_ syn tp | (syn,tp) <- Map.assocs syn_ ]
                                 {-# LINE 1263 "dist/build/Order.hs"#-}
   {-# INLINE rule116 #-}
   {-# LINE 509 "./src-ag/Order.ag" #-}
   rule116 = \ ((_lhsIacount) :: Int) _ntattrs ->
                                {-# LINE 509 "./src-ag/Order.ag" #-}
                                Seq.fromList (zip [_lhsIacount ..] _ntattrs)
                                {-# LINE 1269 "dist/build/Order.hs"#-}
   {-# INLINE rule117 #-}
   {-# LINE 510 "./src-ag/Order.ag" #-}
   rule117 = \ ((_lhsIacount) :: Int) inh_ syn_ ->
                                {-# LINE 510 "./src-ag/Order.ag" #-}
                                _lhsIacount + Map.size inh_ + Map.size syn_
                                {-# LINE 1275 "dist/build/Order.hs"#-}
   {-# INLINE rule118 #-}
   {-# LINE 511 "./src-ag/Order.ag" #-}
   rule118 = \ ((_lhsIacount) :: Int) inh_ syn_ ->
                                 {-# LINE 511 "./src-ag/Order.ag" #-}
                                 Seq.singleton
                                  (_lhsIacount
                                  ,_lhsIacount + Map.size inh_
                                  ,_lhsIacount + Map.size syn_ + Map.size inh_ - 1)
                                 {-# LINE 1284 "dist/build/Order.hs"#-}
   {-# INLINE rule119 #-}
   {-# LINE 520 "./src-ag/Order.ag" #-}
   rule119 = \ ((_prodsIcons) :: [ConstructorIdent]) nt_ ->
                                {-# LINE 520 "./src-ag/Order.ag" #-}
                                [(nt_,_prodsIcons)]
                                {-# LINE 1290 "dist/build/Order.hs"#-}
   {-# INLINE rule120 #-}
   {-# LINE 597 "./src-ag/Order.ag" #-}
   rule120 = \ ((_lhsIcInterfaceMap) :: CInterfaceMap) ((_lhsIo_dovisit) :: Bool) inh_ nt_ syn_ ->
                                 {-# LINE 597 "./src-ag/Order.ag" #-}
                                 if  _lhsIo_dovisit
                                        then findWithErr1 "Nonterminal.cInter" nt_ _lhsIcInterfaceMap
                                        else CInterface [CSegment inh_ syn_]
                                 {-# LINE 1298 "dist/build/Order.hs"#-}
   {-# INLINE rule121 #-}
   {-# LINE 625 "./src-ag/Order.ag" #-}
   rule121 = \ _cInter ((_prodsIcProductions) :: CProductions) inh_ nt_ params_ syn_ ->
                                       {-# LINE 625 "./src-ag/Order.ag" #-}
                                       CNonterminal nt_ params_ inh_ syn_ _prodsIcProductions _cInter
                                       {-# LINE 1304 "dist/build/Order.hs"#-}
   {-# INLINE rule122 #-}
   rule122 = \ ((_prodsIadditionalDep) :: Seq Edge) ->
     _prodsIadditionalDep
   {-# INLINE rule123 #-}
   rule123 = \ ((_prodsIaroundDep) :: Seq Edge) ->
     _prodsIaroundDep
   {-# INLINE rule124 #-}
   rule124 = \ ((_prodsIdirectDep) :: Seq Edge) ->
     _prodsIdirectDep
   {-# INLINE rule125 #-}
   rule125 = \ ((_prodsIerrors) :: Seq Error) ->
     _prodsIerrors
   {-# INLINE rule126 #-}
   rule126 = \ ((_prodsIinstDep) :: Seq Edge) ->
     _prodsIinstDep
   {-# INLINE rule127 #-}
   rule127 = \ ((_prodsImergeDep) :: Seq Edge) ->
     _prodsImergeDep
   {-# INLINE rule128 #-}
   rule128 = \ ((_prodsInAutoRules) :: Int) ->
     _prodsInAutoRules
   {-# INLINE rule129 #-}
   rule129 = \ ((_prodsInExplicitRules) :: Int) ->
     _prodsInExplicitRules
   {-# INLINE rule130 #-}
   rule130 = \ ((_prodsIrules) :: Seq (Vertex,CRule)) ->
     _prodsIrules
   {-# INLINE rule131 #-}
   rule131 = \ ((_prodsIvcount) :: Int) ->
     _prodsIvcount
   {-# INLINE rule132 #-}
   rule132 = \ ((_lhsIallnts) :: [Identifier]) ->
     _lhsIallnts
   {-# INLINE rule133 #-}
   rule133 = \ _aroundMap ->
     _aroundMap
   {-# INLINE rule134 #-}
   rule134 = \ ((_lhsIcVisitsMap) :: CVisitsMap) ->
     _lhsIcVisitsMap
   {-# INLINE rule135 #-}
   rule135 = \ ((_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# INLINE rule136 #-}
   rule136 = \ ((_lhsImanualAttrDepMap) :: AttrOrderMap) ->
     _lhsImanualAttrDepMap
   {-# INLINE rule137 #-}
   rule137 = \ _mergeMap ->
     _mergeMap
   {-# INLINE rule138 #-}
   rule138 = \ ((_lhsIo_case) :: Bool) ->
     _lhsIo_case
   {-# INLINE rule139 #-}
   rule139 = \ ((_lhsIo_cata) :: Bool) ->
     _lhsIo_cata
   {-# INLINE rule140 #-}
   rule140 = \ ((_lhsIo_dovisit) :: Bool) ->
     _lhsIo_dovisit
   {-# INLINE rule141 #-}
   rule141 = \ ((_lhsIo_newtypes) :: Bool) ->
     _lhsIo_newtypes
   {-# INLINE rule142 #-}
   rule142 = \ ((_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# INLINE rule143 #-}
   rule143 = \ ((_lhsIo_sem) :: Bool) ->
     _lhsIo_sem
   {-# INLINE rule144 #-}
   rule144 = \ ((_lhsIo_sig) :: Bool) ->
     _lhsIo_sig
   {-# INLINE rule145 #-}
   rule145 = \ ((_lhsIo_unbox) :: Bool) ->
     _lhsIo_unbox
   {-# INLINE rule146 #-}
   rule146 = \ ((_lhsIo_wantvisit) :: Bool) ->
     _lhsIo_wantvisit
   {-# INLINE rule147 #-}
   rule147 = \ ((_lhsIprefix) :: String) ->
     _lhsIprefix
   {-# INLINE rule148 #-}
   rule148 = \ ((_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
   {-# INLINE rule149 #-}
   rule149 = \ ((_lhsIvcount) :: Int) ->
     _lhsIvcount

-- Nonterminals ------------------------------------------------
-- wrapper
data Inh_Nonterminals  = Inh_Nonterminals { acount_Inh_Nonterminals :: (Int), allnts_Inh_Nonterminals :: ([Identifier]), aroundMap_Inh_Nonterminals :: (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))), cInterfaceMap_Inh_Nonterminals :: (CInterfaceMap), cVisitsMap_Inh_Nonterminals :: (CVisitsMap), inhMap_Inh_Nonterminals :: (Map Identifier Attributes), manualAttrDepMap_Inh_Nonterminals :: (AttrOrderMap), mergeMap_Inh_Nonterminals :: (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier])))), o_case_Inh_Nonterminals :: (Bool), o_cata_Inh_Nonterminals :: (Bool), o_data_Inh_Nonterminals :: (Bool), o_dovisit_Inh_Nonterminals :: (Bool), o_newtypes_Inh_Nonterminals :: (Bool), o_rename_Inh_Nonterminals :: (Bool), o_sem_Inh_Nonterminals :: (Bool), o_sig_Inh_Nonterminals :: (Bool), o_unbox_Inh_Nonterminals :: (Bool), o_wantvisit_Inh_Nonterminals :: (Bool), prefix_Inh_Nonterminals :: (String), synMap_Inh_Nonterminals :: (Map Identifier Attributes), vcount_Inh_Nonterminals :: (Int) }
data Syn_Nonterminals  = Syn_Nonterminals { acount_Syn_Nonterminals :: (Int), additionalDep_Syn_Nonterminals :: (Seq Edge), aranges_Syn_Nonterminals :: (Seq (Int,Int,Int)), aroundDep_Syn_Nonterminals :: (Seq Edge), cNonterminals_Syn_Nonterminals :: (CNonterminals), directDep_Syn_Nonterminals :: (Seq Edge), errors_Syn_Nonterminals :: (Seq Error), inhMap'_Syn_Nonterminals :: (Map Identifier Attributes), instDep_Syn_Nonterminals :: (Seq Edge), mergeDep_Syn_Nonterminals :: (Seq Edge), nAutoRules_Syn_Nonterminals :: (Int), nExplicitRules_Syn_Nonterminals :: (Int), nonts_Syn_Nonterminals :: ([(NontermIdent,[ConstructorIdent])]), ntattrs_Syn_Nonterminals :: (Seq (Vertex,NTAttr)), rules_Syn_Nonterminals :: (Seq (Vertex,CRule)), synMap'_Syn_Nonterminals :: (Map Identifier Attributes), vcount_Syn_Nonterminals :: (Int) }
{-# INLINABLE wrap_Nonterminals #-}
wrap_Nonterminals :: T_Nonterminals  -> Inh_Nonterminals  -> (Syn_Nonterminals )
wrap_Nonterminals (T_Nonterminals act) (Inh_Nonterminals _lhsIacount _lhsIallnts _lhsIaroundMap _lhsIcInterfaceMap _lhsIcVisitsMap _lhsIinhMap _lhsImanualAttrDepMap _lhsImergeMap _lhsIo_case _lhsIo_cata _lhsIo_data _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_unbox _lhsIo_wantvisit _lhsIprefix _lhsIsynMap _lhsIvcount) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Nonterminals_vIn16 _lhsIacount _lhsIallnts _lhsIaroundMap _lhsIcInterfaceMap _lhsIcVisitsMap _lhsIinhMap _lhsImanualAttrDepMap _lhsImergeMap _lhsIo_case _lhsIo_cata _lhsIo_data _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_unbox _lhsIo_wantvisit _lhsIprefix _lhsIsynMap _lhsIvcount
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
data T_Nonterminals_vIn16  = T_Nonterminals_vIn16 (Int) ([Identifier]) (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) (CInterfaceMap) (CVisitsMap) (Map Identifier Attributes) (AttrOrderMap) (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier])))) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (String) (Map Identifier Attributes) (Int)
data T_Nonterminals_vOut16  = T_Nonterminals_vOut16 (Int) (Seq Edge) (Seq (Int,Int,Int)) (Seq Edge) (CNonterminals) (Seq Edge) (Seq Error) (Map Identifier Attributes) (Seq Edge) (Seq Edge) (Int) (Int) ([(NontermIdent,[ConstructorIdent])]) (Seq (Vertex,NTAttr)) (Seq (Vertex,CRule)) (Map Identifier Attributes) (Int)
{-# NOINLINE sem_Nonterminals_Cons #-}
sem_Nonterminals_Cons :: T_Nonterminal  -> T_Nonterminals  -> T_Nonterminals 
sem_Nonterminals_Cons arg_hd_ arg_tl_ = T_Nonterminals (return st17) where
   {-# NOINLINE st17 #-}
   st17 = let
      v16 :: T_Nonterminals_v16 
      v16 = \ (T_Nonterminals_vIn16 _lhsIacount _lhsIallnts _lhsIaroundMap _lhsIcInterfaceMap _lhsIcVisitsMap _lhsIinhMap _lhsImanualAttrDepMap _lhsImergeMap _lhsIo_case _lhsIo_cata _lhsIo_data _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_unbox _lhsIo_wantvisit _lhsIprefix _lhsIsynMap _lhsIvcount) -> ( let
         _hdX14 = Control.Monad.Identity.runIdentity (attach_T_Nonterminal (arg_hd_))
         _tlX17 = Control.Monad.Identity.runIdentity (attach_T_Nonterminals (arg_tl_))
         (T_Nonterminal_vOut13 _hdIacount _hdIadditionalDep _hdIaranges _hdIaroundDep _hdIcNonterminal _hdIdirectDep _hdIerrors _hdIinhMap' _hdIinstDep _hdImergeDep _hdInAutoRules _hdInExplicitRules _hdInonts _hdIntattrs _hdIrules _hdIsynMap' _hdIvcount) = inv_Nonterminal_s14 _hdX14 (T_Nonterminal_vIn13 _hdOacount _hdOallnts _hdOaroundMap _hdOcInterfaceMap _hdOcVisitsMap _hdOinhMap _hdOmanualAttrDepMap _hdOmergeMap _hdOo_case _hdOo_cata _hdOo_data _hdOo_dovisit _hdOo_newtypes _hdOo_rename _hdOo_sem _hdOo_sig _hdOo_unbox _hdOo_wantvisit _hdOprefix _hdOsynMap _hdOvcount)
         (T_Nonterminals_vOut16 _tlIacount _tlIadditionalDep _tlIaranges _tlIaroundDep _tlIcNonterminals _tlIdirectDep _tlIerrors _tlIinhMap' _tlIinstDep _tlImergeDep _tlInAutoRules _tlInExplicitRules _tlInonts _tlIntattrs _tlIrules _tlIsynMap' _tlIvcount) = inv_Nonterminals_s17 _tlX17 (T_Nonterminals_vIn16 _tlOacount _tlOallnts _tlOaroundMap _tlOcInterfaceMap _tlOcVisitsMap _tlOinhMap _tlOmanualAttrDepMap _tlOmergeMap _tlOo_case _tlOo_cata _tlOo_data _tlOo_dovisit _tlOo_newtypes _tlOo_rename _tlOo_sem _tlOo_sig _tlOo_unbox _tlOo_wantvisit _tlOprefix _tlOsynMap _tlOvcount)
         _lhsOcNonterminals :: CNonterminals
         _lhsOcNonterminals = rule150 _hdIcNonterminal _tlIcNonterminals
         _lhsOadditionalDep :: Seq Edge
         _lhsOadditionalDep = rule151 _hdIadditionalDep _tlIadditionalDep
         _lhsOaranges :: Seq (Int,Int,Int)
         _lhsOaranges = rule152 _hdIaranges _tlIaranges
         _lhsOaroundDep :: Seq Edge
         _lhsOaroundDep = rule153 _hdIaroundDep _tlIaroundDep
         _lhsOdirectDep :: Seq Edge
         _lhsOdirectDep = rule154 _hdIdirectDep _tlIdirectDep
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule155 _hdIerrors _tlIerrors
         _lhsOinhMap' :: Map Identifier Attributes
         _lhsOinhMap' = rule156 _hdIinhMap' _tlIinhMap'
         _lhsOinstDep :: Seq Edge
         _lhsOinstDep = rule157 _hdIinstDep _tlIinstDep
         _lhsOmergeDep :: Seq Edge
         _lhsOmergeDep = rule158 _hdImergeDep _tlImergeDep
         _lhsOnAutoRules :: Int
         _lhsOnAutoRules = rule159 _hdInAutoRules _tlInAutoRules
         _lhsOnExplicitRules :: Int
         _lhsOnExplicitRules = rule160 _hdInExplicitRules _tlInExplicitRules
         _lhsOnonts :: [(NontermIdent,[ConstructorIdent])]
         _lhsOnonts = rule161 _hdInonts _tlInonts
         _lhsOntattrs :: Seq (Vertex,NTAttr)
         _lhsOntattrs = rule162 _hdIntattrs _tlIntattrs
         _lhsOrules :: Seq (Vertex,CRule)
         _lhsOrules = rule163 _hdIrules _tlIrules
         _lhsOsynMap' :: Map Identifier Attributes
         _lhsOsynMap' = rule164 _hdIsynMap' _tlIsynMap'
         _lhsOacount :: Int
         _lhsOacount = rule165 _tlIacount
         _lhsOvcount :: Int
         _lhsOvcount = rule166 _tlIvcount
         _hdOacount = rule167 _lhsIacount
         _hdOallnts = rule168 _lhsIallnts
         _hdOaroundMap = rule169 _lhsIaroundMap
         _hdOcInterfaceMap = rule170 _lhsIcInterfaceMap
         _hdOcVisitsMap = rule171 _lhsIcVisitsMap
         _hdOinhMap = rule172 _lhsIinhMap
         _hdOmanualAttrDepMap = rule173 _lhsImanualAttrDepMap
         _hdOmergeMap = rule174 _lhsImergeMap
         _hdOo_case = rule175 _lhsIo_case
         _hdOo_cata = rule176 _lhsIo_cata
         _hdOo_data = rule177 _lhsIo_data
         _hdOo_dovisit = rule178 _lhsIo_dovisit
         _hdOo_newtypes = rule179 _lhsIo_newtypes
         _hdOo_rename = rule180 _lhsIo_rename
         _hdOo_sem = rule181 _lhsIo_sem
         _hdOo_sig = rule182 _lhsIo_sig
         _hdOo_unbox = rule183 _lhsIo_unbox
         _hdOo_wantvisit = rule184 _lhsIo_wantvisit
         _hdOprefix = rule185 _lhsIprefix
         _hdOsynMap = rule186 _lhsIsynMap
         _hdOvcount = rule187 _lhsIvcount
         _tlOacount = rule188 _hdIacount
         _tlOallnts = rule189 _lhsIallnts
         _tlOaroundMap = rule190 _lhsIaroundMap
         _tlOcInterfaceMap = rule191 _lhsIcInterfaceMap
         _tlOcVisitsMap = rule192 _lhsIcVisitsMap
         _tlOinhMap = rule193 _lhsIinhMap
         _tlOmanualAttrDepMap = rule194 _lhsImanualAttrDepMap
         _tlOmergeMap = rule195 _lhsImergeMap
         _tlOo_case = rule196 _lhsIo_case
         _tlOo_cata = rule197 _lhsIo_cata
         _tlOo_data = rule198 _lhsIo_data
         _tlOo_dovisit = rule199 _lhsIo_dovisit
         _tlOo_newtypes = rule200 _lhsIo_newtypes
         _tlOo_rename = rule201 _lhsIo_rename
         _tlOo_sem = rule202 _lhsIo_sem
         _tlOo_sig = rule203 _lhsIo_sig
         _tlOo_unbox = rule204 _lhsIo_unbox
         _tlOo_wantvisit = rule205 _lhsIo_wantvisit
         _tlOprefix = rule206 _lhsIprefix
         _tlOsynMap = rule207 _lhsIsynMap
         _tlOvcount = rule208 _hdIvcount
         __result_ = T_Nonterminals_vOut16 _lhsOacount _lhsOadditionalDep _lhsOaranges _lhsOaroundDep _lhsOcNonterminals _lhsOdirectDep _lhsOerrors _lhsOinhMap' _lhsOinstDep _lhsOmergeDep _lhsOnAutoRules _lhsOnExplicitRules _lhsOnonts _lhsOntattrs _lhsOrules _lhsOsynMap' _lhsOvcount
         in __result_ )
     in C_Nonterminals_s17 v16
   {-# INLINE rule150 #-}
   {-# LINE 622 "./src-ag/Order.ag" #-}
   rule150 = \ ((_hdIcNonterminal) :: CNonterminal) ((_tlIcNonterminals) :: CNonterminals) ->
                                 {-# LINE 622 "./src-ag/Order.ag" #-}
                                 _hdIcNonterminal : _tlIcNonterminals
                                 {-# LINE 1515 "dist/build/Order.hs"#-}
   {-# INLINE rule151 #-}
   rule151 = \ ((_hdIadditionalDep) :: Seq Edge) ((_tlIadditionalDep) :: Seq Edge) ->
     _hdIadditionalDep Seq.>< _tlIadditionalDep
   {-# INLINE rule152 #-}
   rule152 = \ ((_hdIaranges) :: Seq (Int,Int,Int)) ((_tlIaranges) :: Seq (Int,Int,Int)) ->
     _hdIaranges Seq.>< _tlIaranges
   {-# INLINE rule153 #-}
   rule153 = \ ((_hdIaroundDep) :: Seq Edge) ((_tlIaroundDep) :: Seq Edge) ->
     _hdIaroundDep Seq.>< _tlIaroundDep
   {-# INLINE rule154 #-}
   rule154 = \ ((_hdIdirectDep) :: Seq Edge) ((_tlIdirectDep) :: Seq Edge) ->
     _hdIdirectDep Seq.>< _tlIdirectDep
   {-# INLINE rule155 #-}
   rule155 = \ ((_hdIerrors) :: Seq Error) ((_tlIerrors) :: Seq Error) ->
     _hdIerrors Seq.>< _tlIerrors
   {-# INLINE rule156 #-}
   rule156 = \ ((_hdIinhMap') :: Map Identifier Attributes) ((_tlIinhMap') :: Map Identifier Attributes) ->
     _hdIinhMap' `Map.union` _tlIinhMap'
   {-# INLINE rule157 #-}
   rule157 = \ ((_hdIinstDep) :: Seq Edge) ((_tlIinstDep) :: Seq Edge) ->
     _hdIinstDep Seq.>< _tlIinstDep
   {-# INLINE rule158 #-}
   rule158 = \ ((_hdImergeDep) :: Seq Edge) ((_tlImergeDep) :: Seq Edge) ->
     _hdImergeDep Seq.>< _tlImergeDep
   {-# INLINE rule159 #-}
   rule159 = \ ((_hdInAutoRules) :: Int) ((_tlInAutoRules) :: Int) ->
     _hdInAutoRules + _tlInAutoRules
   {-# INLINE rule160 #-}
   rule160 = \ ((_hdInExplicitRules) :: Int) ((_tlInExplicitRules) :: Int) ->
     _hdInExplicitRules + _tlInExplicitRules
   {-# INLINE rule161 #-}
   rule161 = \ ((_hdInonts) :: [(NontermIdent,[ConstructorIdent])]) ((_tlInonts) :: [(NontermIdent,[ConstructorIdent])]) ->
     _hdInonts ++ _tlInonts
   {-# INLINE rule162 #-}
   rule162 = \ ((_hdIntattrs) :: Seq (Vertex,NTAttr)) ((_tlIntattrs) :: Seq (Vertex,NTAttr)) ->
     _hdIntattrs Seq.>< _tlIntattrs
   {-# INLINE rule163 #-}
   rule163 = \ ((_hdIrules) :: Seq (Vertex,CRule)) ((_tlIrules) :: Seq (Vertex,CRule)) ->
     _hdIrules Seq.>< _tlIrules
   {-# INLINE rule164 #-}
   rule164 = \ ((_hdIsynMap') :: Map Identifier Attributes) ((_tlIsynMap') :: Map Identifier Attributes) ->
     _hdIsynMap' `Map.union` _tlIsynMap'
   {-# INLINE rule165 #-}
   rule165 = \ ((_tlIacount) :: Int) ->
     _tlIacount
   {-# INLINE rule166 #-}
   rule166 = \ ((_tlIvcount) :: Int) ->
     _tlIvcount
   {-# INLINE rule167 #-}
   rule167 = \ ((_lhsIacount) :: Int) ->
     _lhsIacount
   {-# INLINE rule168 #-}
   rule168 = \ ((_lhsIallnts) :: [Identifier]) ->
     _lhsIallnts
   {-# INLINE rule169 #-}
   rule169 = \ ((_lhsIaroundMap) :: Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) ->
     _lhsIaroundMap
   {-# INLINE rule170 #-}
   rule170 = \ ((_lhsIcInterfaceMap) :: CInterfaceMap) ->
     _lhsIcInterfaceMap
   {-# INLINE rule171 #-}
   rule171 = \ ((_lhsIcVisitsMap) :: CVisitsMap) ->
     _lhsIcVisitsMap
   {-# INLINE rule172 #-}
   rule172 = \ ((_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# INLINE rule173 #-}
   rule173 = \ ((_lhsImanualAttrDepMap) :: AttrOrderMap) ->
     _lhsImanualAttrDepMap
   {-# INLINE rule174 #-}
   rule174 = \ ((_lhsImergeMap) :: Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier])))) ->
     _lhsImergeMap
   {-# INLINE rule175 #-}
   rule175 = \ ((_lhsIo_case) :: Bool) ->
     _lhsIo_case
   {-# INLINE rule176 #-}
   rule176 = \ ((_lhsIo_cata) :: Bool) ->
     _lhsIo_cata
   {-# INLINE rule177 #-}
   rule177 = \ ((_lhsIo_data) :: Bool) ->
     _lhsIo_data
   {-# INLINE rule178 #-}
   rule178 = \ ((_lhsIo_dovisit) :: Bool) ->
     _lhsIo_dovisit
   {-# INLINE rule179 #-}
   rule179 = \ ((_lhsIo_newtypes) :: Bool) ->
     _lhsIo_newtypes
   {-# INLINE rule180 #-}
   rule180 = \ ((_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# INLINE rule181 #-}
   rule181 = \ ((_lhsIo_sem) :: Bool) ->
     _lhsIo_sem
   {-# INLINE rule182 #-}
   rule182 = \ ((_lhsIo_sig) :: Bool) ->
     _lhsIo_sig
   {-# INLINE rule183 #-}
   rule183 = \ ((_lhsIo_unbox) :: Bool) ->
     _lhsIo_unbox
   {-# INLINE rule184 #-}
   rule184 = \ ((_lhsIo_wantvisit) :: Bool) ->
     _lhsIo_wantvisit
   {-# INLINE rule185 #-}
   rule185 = \ ((_lhsIprefix) :: String) ->
     _lhsIprefix
   {-# INLINE rule186 #-}
   rule186 = \ ((_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
   {-# INLINE rule187 #-}
   rule187 = \ ((_lhsIvcount) :: Int) ->
     _lhsIvcount
   {-# INLINE rule188 #-}
   rule188 = \ ((_hdIacount) :: Int) ->
     _hdIacount
   {-# INLINE rule189 #-}
   rule189 = \ ((_lhsIallnts) :: [Identifier]) ->
     _lhsIallnts
   {-# INLINE rule190 #-}
   rule190 = \ ((_lhsIaroundMap) :: Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) ->
     _lhsIaroundMap
   {-# INLINE rule191 #-}
   rule191 = \ ((_lhsIcInterfaceMap) :: CInterfaceMap) ->
     _lhsIcInterfaceMap
   {-# INLINE rule192 #-}
   rule192 = \ ((_lhsIcVisitsMap) :: CVisitsMap) ->
     _lhsIcVisitsMap
   {-# INLINE rule193 #-}
   rule193 = \ ((_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# INLINE rule194 #-}
   rule194 = \ ((_lhsImanualAttrDepMap) :: AttrOrderMap) ->
     _lhsImanualAttrDepMap
   {-# INLINE rule195 #-}
   rule195 = \ ((_lhsImergeMap) :: Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier])))) ->
     _lhsImergeMap
   {-# INLINE rule196 #-}
   rule196 = \ ((_lhsIo_case) :: Bool) ->
     _lhsIo_case
   {-# INLINE rule197 #-}
   rule197 = \ ((_lhsIo_cata) :: Bool) ->
     _lhsIo_cata
   {-# INLINE rule198 #-}
   rule198 = \ ((_lhsIo_data) :: Bool) ->
     _lhsIo_data
   {-# INLINE rule199 #-}
   rule199 = \ ((_lhsIo_dovisit) :: Bool) ->
     _lhsIo_dovisit
   {-# INLINE rule200 #-}
   rule200 = \ ((_lhsIo_newtypes) :: Bool) ->
     _lhsIo_newtypes
   {-# INLINE rule201 #-}
   rule201 = \ ((_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# INLINE rule202 #-}
   rule202 = \ ((_lhsIo_sem) :: Bool) ->
     _lhsIo_sem
   {-# INLINE rule203 #-}
   rule203 = \ ((_lhsIo_sig) :: Bool) ->
     _lhsIo_sig
   {-# INLINE rule204 #-}
   rule204 = \ ((_lhsIo_unbox) :: Bool) ->
     _lhsIo_unbox
   {-# INLINE rule205 #-}
   rule205 = \ ((_lhsIo_wantvisit) :: Bool) ->
     _lhsIo_wantvisit
   {-# INLINE rule206 #-}
   rule206 = \ ((_lhsIprefix) :: String) ->
     _lhsIprefix
   {-# INLINE rule207 #-}
   rule207 = \ ((_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
   {-# INLINE rule208 #-}
   rule208 = \ ((_hdIvcount) :: Int) ->
     _hdIvcount
{-# NOINLINE sem_Nonterminals_Nil #-}
sem_Nonterminals_Nil ::  T_Nonterminals 
sem_Nonterminals_Nil  = T_Nonterminals (return st17) where
   {-# NOINLINE st17 #-}
   st17 = let
      v16 :: T_Nonterminals_v16 
      v16 = \ (T_Nonterminals_vIn16 _lhsIacount _lhsIallnts _lhsIaroundMap _lhsIcInterfaceMap _lhsIcVisitsMap _lhsIinhMap _lhsImanualAttrDepMap _lhsImergeMap _lhsIo_case _lhsIo_cata _lhsIo_data _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_unbox _lhsIo_wantvisit _lhsIprefix _lhsIsynMap _lhsIvcount) -> ( let
         _lhsOcNonterminals :: CNonterminals
         _lhsOcNonterminals = rule209  ()
         _lhsOadditionalDep :: Seq Edge
         _lhsOadditionalDep = rule210  ()
         _lhsOaranges :: Seq (Int,Int,Int)
         _lhsOaranges = rule211  ()
         _lhsOaroundDep :: Seq Edge
         _lhsOaroundDep = rule212  ()
         _lhsOdirectDep :: Seq Edge
         _lhsOdirectDep = rule213  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule214  ()
         _lhsOinhMap' :: Map Identifier Attributes
         _lhsOinhMap' = rule215  ()
         _lhsOinstDep :: Seq Edge
         _lhsOinstDep = rule216  ()
         _lhsOmergeDep :: Seq Edge
         _lhsOmergeDep = rule217  ()
         _lhsOnAutoRules :: Int
         _lhsOnAutoRules = rule218  ()
         _lhsOnExplicitRules :: Int
         _lhsOnExplicitRules = rule219  ()
         _lhsOnonts :: [(NontermIdent,[ConstructorIdent])]
         _lhsOnonts = rule220  ()
         _lhsOntattrs :: Seq (Vertex,NTAttr)
         _lhsOntattrs = rule221  ()
         _lhsOrules :: Seq (Vertex,CRule)
         _lhsOrules = rule222  ()
         _lhsOsynMap' :: Map Identifier Attributes
         _lhsOsynMap' = rule223  ()
         _lhsOacount :: Int
         _lhsOacount = rule224 _lhsIacount
         _lhsOvcount :: Int
         _lhsOvcount = rule225 _lhsIvcount
         __result_ = T_Nonterminals_vOut16 _lhsOacount _lhsOadditionalDep _lhsOaranges _lhsOaroundDep _lhsOcNonterminals _lhsOdirectDep _lhsOerrors _lhsOinhMap' _lhsOinstDep _lhsOmergeDep _lhsOnAutoRules _lhsOnExplicitRules _lhsOnonts _lhsOntattrs _lhsOrules _lhsOsynMap' _lhsOvcount
         in __result_ )
     in C_Nonterminals_s17 v16
   {-# INLINE rule209 #-}
   {-# LINE 623 "./src-ag/Order.ag" #-}
   rule209 = \  (_ :: ()) ->
                                 {-# LINE 623 "./src-ag/Order.ag" #-}
                                 []
                                 {-# LINE 1739 "dist/build/Order.hs"#-}
   {-# INLINE rule210 #-}
   rule210 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule211 #-}
   rule211 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule212 #-}
   rule212 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule213 #-}
   rule213 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule214 #-}
   rule214 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule215 #-}
   rule215 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule216 #-}
   rule216 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule217 #-}
   rule217 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule218 #-}
   rule218 = \  (_ :: ()) ->
     0
   {-# INLINE rule219 #-}
   rule219 = \  (_ :: ()) ->
     0
   {-# INLINE rule220 #-}
   rule220 = \  (_ :: ()) ->
     []
   {-# INLINE rule221 #-}
   rule221 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule222 #-}
   rule222 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule223 #-}
   rule223 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule224 #-}
   rule224 = \ ((_lhsIacount) :: Int) ->
     _lhsIacount
   {-# INLINE rule225 #-}
   rule225 = \ ((_lhsIvcount) :: Int) ->
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
         _lhsOerrors = rule226 _patsIerrors
         _lhsOgathAltAttrs :: [AltAttr]
         _lhsOgathAltAttrs = rule227 _patsIgathAltAttrs
         _lhsOinstVars :: [Identifier]
         _lhsOinstVars = rule228 _patsIinstVars
         _lhsOlocVars :: [Identifier]
         _lhsOlocVars = rule229 _patsIlocVars
         _lhsOpatternAttrs :: [(Identifier,Identifier,Bool)]
         _lhsOpatternAttrs = rule230 _patsIpatternAttrs
         _copy = rule231 _patsIcopy arg_name_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule232 _copy
         _patsOallTypeSigs = rule233 _lhsIallTypeSigs
         _patsOaltAttrs = rule234 _lhsIaltAttrs
         _patsOcon = rule235 _lhsIcon
         _patsOinh = rule236 _lhsIinh
         _patsOnt = rule237 _lhsInt
         _patsOsyn = rule238 _lhsIsyn
         __result_ = T_Pattern_vOut19 _lhsOcopy _lhsOerrors _lhsOgathAltAttrs _lhsOinstVars _lhsOlocVars _lhsOpatternAttrs
         in __result_ )
     in C_Pattern_s20 v19
   {-# INLINE rule226 #-}
   rule226 = \ ((_patsIerrors) :: Seq Error) ->
     _patsIerrors
   {-# INLINE rule227 #-}
   rule227 = \ ((_patsIgathAltAttrs) :: [AltAttr]) ->
     _patsIgathAltAttrs
   {-# INLINE rule228 #-}
   rule228 = \ ((_patsIinstVars) :: [Identifier]) ->
     _patsIinstVars
   {-# INLINE rule229 #-}
   rule229 = \ ((_patsIlocVars) :: [Identifier]) ->
     _patsIlocVars
   {-# INLINE rule230 #-}
   rule230 = \ ((_patsIpatternAttrs) :: [(Identifier,Identifier,Bool)]) ->
     _patsIpatternAttrs
   {-# INLINE rule231 #-}
   rule231 = \ ((_patsIcopy) :: Patterns) name_ ->
     Constr name_ _patsIcopy
   {-# INLINE rule232 #-}
   rule232 = \ _copy ->
     _copy
   {-# INLINE rule233 #-}
   rule233 = \ ((_lhsIallTypeSigs) :: Map Identifier Type) ->
     _lhsIallTypeSigs
   {-# INLINE rule234 #-}
   rule234 = \ ((_lhsIaltAttrs) :: Map AltAttr Vertex) ->
     _lhsIaltAttrs
   {-# INLINE rule235 #-}
   rule235 = \ ((_lhsIcon) :: Identifier) ->
     _lhsIcon
   {-# INLINE rule236 #-}
   rule236 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule237 #-}
   rule237 = \ ((_lhsInt) :: Identifier) ->
     _lhsInt
   {-# INLINE rule238 #-}
   rule238 = \ ((_lhsIsyn) :: Attributes) ->
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
         _lhsOerrors = rule239 _patsIerrors
         _lhsOgathAltAttrs :: [AltAttr]
         _lhsOgathAltAttrs = rule240 _patsIgathAltAttrs
         _lhsOinstVars :: [Identifier]
         _lhsOinstVars = rule241 _patsIinstVars
         _lhsOlocVars :: [Identifier]
         _lhsOlocVars = rule242 _patsIlocVars
         _lhsOpatternAttrs :: [(Identifier,Identifier,Bool)]
         _lhsOpatternAttrs = rule243 _patsIpatternAttrs
         _copy = rule244 _patsIcopy arg_pos_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule245 _copy
         _patsOallTypeSigs = rule246 _lhsIallTypeSigs
         _patsOaltAttrs = rule247 _lhsIaltAttrs
         _patsOcon = rule248 _lhsIcon
         _patsOinh = rule249 _lhsIinh
         _patsOnt = rule250 _lhsInt
         _patsOsyn = rule251 _lhsIsyn
         __result_ = T_Pattern_vOut19 _lhsOcopy _lhsOerrors _lhsOgathAltAttrs _lhsOinstVars _lhsOlocVars _lhsOpatternAttrs
         in __result_ )
     in C_Pattern_s20 v19
   {-# INLINE rule239 #-}
   rule239 = \ ((_patsIerrors) :: Seq Error) ->
     _patsIerrors
   {-# INLINE rule240 #-}
   rule240 = \ ((_patsIgathAltAttrs) :: [AltAttr]) ->
     _patsIgathAltAttrs
   {-# INLINE rule241 #-}
   rule241 = \ ((_patsIinstVars) :: [Identifier]) ->
     _patsIinstVars
   {-# INLINE rule242 #-}
   rule242 = \ ((_patsIlocVars) :: [Identifier]) ->
     _patsIlocVars
   {-# INLINE rule243 #-}
   rule243 = \ ((_patsIpatternAttrs) :: [(Identifier,Identifier,Bool)]) ->
     _patsIpatternAttrs
   {-# INLINE rule244 #-}
   rule244 = \ ((_patsIcopy) :: Patterns) pos_ ->
     Product pos_ _patsIcopy
   {-# INLINE rule245 #-}
   rule245 = \ _copy ->
     _copy
   {-# INLINE rule246 #-}
   rule246 = \ ((_lhsIallTypeSigs) :: Map Identifier Type) ->
     _lhsIallTypeSigs
   {-# INLINE rule247 #-}
   rule247 = \ ((_lhsIaltAttrs) :: Map AltAttr Vertex) ->
     _lhsIaltAttrs
   {-# INLINE rule248 #-}
   rule248 = \ ((_lhsIcon) :: Identifier) ->
     _lhsIcon
   {-# INLINE rule249 #-}
   rule249 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule250 #-}
   rule250 = \ ((_lhsInt) :: Identifier) ->
     _lhsInt
   {-# INLINE rule251 #-}
   rule251 = \ ((_lhsIsyn) :: Attributes) ->
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
         _lhsOgathAltAttrs = rule252 arg_attr_ arg_field_
         _lhsOpatternAttrs :: [(Identifier,Identifier,Bool)]
         _lhsOpatternAttrs = rule253 arg_attr_ arg_field_
         _lhsOlocVars :: [Identifier]
         _lhsOlocVars = rule254 arg_attr_ arg_field_
         _lhsOinstVars :: [Identifier]
         _lhsOinstVars = rule255 arg_attr_ arg_field_
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule256 _patIerrors
         _copy = rule257 _patIcopy arg_attr_ arg_field_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule258 _copy
         _patOallTypeSigs = rule259 _lhsIallTypeSigs
         _patOaltAttrs = rule260 _lhsIaltAttrs
         _patOcon = rule261 _lhsIcon
         _patOinh = rule262 _lhsIinh
         _patOnt = rule263 _lhsInt
         _patOsyn = rule264 _lhsIsyn
         __result_ = T_Pattern_vOut19 _lhsOcopy _lhsOerrors _lhsOgathAltAttrs _lhsOinstVars _lhsOlocVars _lhsOpatternAttrs
         in __result_ )
     in C_Pattern_s20 v19
   {-# INLINE rule252 #-}
   {-# LINE 185 "./src-ag/Order.ag" #-}
   rule252 = \ attr_ field_ ->
                                {-# LINE 185 "./src-ag/Order.ag" #-}
                                [AltAttr field_ attr_ (field_ == _LOC || field_ == _INST)]
                                {-# LINE 1999 "dist/build/Order.hs"#-}
   {-# INLINE rule253 #-}
   {-# LINE 251 "./src-ag/Order.ag" #-}
   rule253 = \ attr_ field_ ->
                                {-# LINE 251 "./src-ag/Order.ag" #-}
                                [(field_,attr_,(field_ == _LOC || field_ == _INST))]
                                {-# LINE 2005 "dist/build/Order.hs"#-}
   {-# INLINE rule254 #-}
   {-# LINE 681 "./src-ag/Order.ag" #-}
   rule254 = \ attr_ field_ ->
                               {-# LINE 681 "./src-ag/Order.ag" #-}
                               if field_ == _LOC
                                  then [attr_]
                                  else []
                               {-# LINE 2013 "dist/build/Order.hs"#-}
   {-# INLINE rule255 #-}
   {-# LINE 684 "./src-ag/Order.ag" #-}
   rule255 = \ attr_ field_ ->
                               {-# LINE 684 "./src-ag/Order.ag" #-}
                               if field_ == _INST
                                  then [attr_]
                                  else []
                               {-# LINE 2021 "dist/build/Order.hs"#-}
   {-# INLINE rule256 #-}
   rule256 = \ ((_patIerrors) :: Seq Error) ->
     _patIerrors
   {-# INLINE rule257 #-}
   rule257 = \ ((_patIcopy) :: Pattern) attr_ field_ ->
     Alias field_ attr_ _patIcopy
   {-# INLINE rule258 #-}
   rule258 = \ _copy ->
     _copy
   {-# INLINE rule259 #-}
   rule259 = \ ((_lhsIallTypeSigs) :: Map Identifier Type) ->
     _lhsIallTypeSigs
   {-# INLINE rule260 #-}
   rule260 = \ ((_lhsIaltAttrs) :: Map AltAttr Vertex) ->
     _lhsIaltAttrs
   {-# INLINE rule261 #-}
   rule261 = \ ((_lhsIcon) :: Identifier) ->
     _lhsIcon
   {-# INLINE rule262 #-}
   rule262 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule263 #-}
   rule263 = \ ((_lhsInt) :: Identifier) ->
     _lhsInt
   {-# INLINE rule264 #-}
   rule264 = \ ((_lhsIsyn) :: Attributes) ->
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
         _lhsOerrors = rule265 _patIerrors
         _lhsOgathAltAttrs :: [AltAttr]
         _lhsOgathAltAttrs = rule266 _patIgathAltAttrs
         _lhsOinstVars :: [Identifier]
         _lhsOinstVars = rule267 _patIinstVars
         _lhsOlocVars :: [Identifier]
         _lhsOlocVars = rule268 _patIlocVars
         _lhsOpatternAttrs :: [(Identifier,Identifier,Bool)]
         _lhsOpatternAttrs = rule269 _patIpatternAttrs
         _copy = rule270 _patIcopy
         _lhsOcopy :: Pattern
         _lhsOcopy = rule271 _copy
         _patOallTypeSigs = rule272 _lhsIallTypeSigs
         _patOaltAttrs = rule273 _lhsIaltAttrs
         _patOcon = rule274 _lhsIcon
         _patOinh = rule275 _lhsIinh
         _patOnt = rule276 _lhsInt
         _patOsyn = rule277 _lhsIsyn
         __result_ = T_Pattern_vOut19 _lhsOcopy _lhsOerrors _lhsOgathAltAttrs _lhsOinstVars _lhsOlocVars _lhsOpatternAttrs
         in __result_ )
     in C_Pattern_s20 v19
   {-# INLINE rule265 #-}
   rule265 = \ ((_patIerrors) :: Seq Error) ->
     _patIerrors
   {-# INLINE rule266 #-}
   rule266 = \ ((_patIgathAltAttrs) :: [AltAttr]) ->
     _patIgathAltAttrs
   {-# INLINE rule267 #-}
   rule267 = \ ((_patIinstVars) :: [Identifier]) ->
     _patIinstVars
   {-# INLINE rule268 #-}
   rule268 = \ ((_patIlocVars) :: [Identifier]) ->
     _patIlocVars
   {-# INLINE rule269 #-}
   rule269 = \ ((_patIpatternAttrs) :: [(Identifier,Identifier,Bool)]) ->
     _patIpatternAttrs
   {-# INLINE rule270 #-}
   rule270 = \ ((_patIcopy) :: Pattern) ->
     Irrefutable _patIcopy
   {-# INLINE rule271 #-}
   rule271 = \ _copy ->
     _copy
   {-# INLINE rule272 #-}
   rule272 = \ ((_lhsIallTypeSigs) :: Map Identifier Type) ->
     _lhsIallTypeSigs
   {-# INLINE rule273 #-}
   rule273 = \ ((_lhsIaltAttrs) :: Map AltAttr Vertex) ->
     _lhsIaltAttrs
   {-# INLINE rule274 #-}
   rule274 = \ ((_lhsIcon) :: Identifier) ->
     _lhsIcon
   {-# INLINE rule275 #-}
   rule275 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule276 #-}
   rule276 = \ ((_lhsInt) :: Identifier) ->
     _lhsInt
   {-# INLINE rule277 #-}
   rule277 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
{-# NOINLINE sem_Pattern_Underscore #-}
sem_Pattern_Underscore :: (Pos) -> T_Pattern 
sem_Pattern_Underscore arg_pos_ = T_Pattern (return st20) where
   {-# NOINLINE st20 #-}
   st20 = let
      v19 :: T_Pattern_v19 
      v19 = \ (T_Pattern_vIn19 _lhsIallTypeSigs _lhsIaltAttrs _lhsIcon _lhsIinh _lhsInt _lhsIsyn) -> ( let
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule278  ()
         _lhsOgathAltAttrs :: [AltAttr]
         _lhsOgathAltAttrs = rule279  ()
         _lhsOinstVars :: [Identifier]
         _lhsOinstVars = rule280  ()
         _lhsOlocVars :: [Identifier]
         _lhsOlocVars = rule281  ()
         _lhsOpatternAttrs :: [(Identifier,Identifier,Bool)]
         _lhsOpatternAttrs = rule282  ()
         _copy = rule283 arg_pos_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule284 _copy
         __result_ = T_Pattern_vOut19 _lhsOcopy _lhsOerrors _lhsOgathAltAttrs _lhsOinstVars _lhsOlocVars _lhsOpatternAttrs
         in __result_ )
     in C_Pattern_s20 v19
   {-# INLINE rule278 #-}
   rule278 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule279 #-}
   rule279 = \  (_ :: ()) ->
     []
   {-# INLINE rule280 #-}
   rule280 = \  (_ :: ()) ->
     []
   {-# INLINE rule281 #-}
   rule281 = \  (_ :: ()) ->
     []
   {-# INLINE rule282 #-}
   rule282 = \  (_ :: ()) ->
     []
   {-# INLINE rule283 #-}
   rule283 = \ pos_ ->
     Underscore pos_
   {-# INLINE rule284 #-}
   rule284 = \ _copy ->
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
         _lhsOerrors = rule285 _hdIerrors _tlIerrors
         _lhsOgathAltAttrs :: [AltAttr]
         _lhsOgathAltAttrs = rule286 _hdIgathAltAttrs _tlIgathAltAttrs
         _lhsOinstVars :: [Identifier]
         _lhsOinstVars = rule287 _hdIinstVars _tlIinstVars
         _lhsOlocVars :: [Identifier]
         _lhsOlocVars = rule288 _hdIlocVars _tlIlocVars
         _lhsOpatternAttrs :: [(Identifier,Identifier,Bool)]
         _lhsOpatternAttrs = rule289 _hdIpatternAttrs _tlIpatternAttrs
         _copy = rule290 _hdIcopy _tlIcopy
         _lhsOcopy :: Patterns
         _lhsOcopy = rule291 _copy
         _hdOallTypeSigs = rule292 _lhsIallTypeSigs
         _hdOaltAttrs = rule293 _lhsIaltAttrs
         _hdOcon = rule294 _lhsIcon
         _hdOinh = rule295 _lhsIinh
         _hdOnt = rule296 _lhsInt
         _hdOsyn = rule297 _lhsIsyn
         _tlOallTypeSigs = rule298 _lhsIallTypeSigs
         _tlOaltAttrs = rule299 _lhsIaltAttrs
         _tlOcon = rule300 _lhsIcon
         _tlOinh = rule301 _lhsIinh
         _tlOnt = rule302 _lhsInt
         _tlOsyn = rule303 _lhsIsyn
         __result_ = T_Patterns_vOut22 _lhsOcopy _lhsOerrors _lhsOgathAltAttrs _lhsOinstVars _lhsOlocVars _lhsOpatternAttrs
         in __result_ )
     in C_Patterns_s23 v22
   {-# INLINE rule285 #-}
   rule285 = \ ((_hdIerrors) :: Seq Error) ((_tlIerrors) :: Seq Error) ->
     _hdIerrors Seq.>< _tlIerrors
   {-# INLINE rule286 #-}
   rule286 = \ ((_hdIgathAltAttrs) :: [AltAttr]) ((_tlIgathAltAttrs) :: [AltAttr]) ->
     _hdIgathAltAttrs ++ _tlIgathAltAttrs
   {-# INLINE rule287 #-}
   rule287 = \ ((_hdIinstVars) :: [Identifier]) ((_tlIinstVars) :: [Identifier]) ->
     _hdIinstVars ++ _tlIinstVars
   {-# INLINE rule288 #-}
   rule288 = \ ((_hdIlocVars) :: [Identifier]) ((_tlIlocVars) :: [Identifier]) ->
     _hdIlocVars ++ _tlIlocVars
   {-# INLINE rule289 #-}
   rule289 = \ ((_hdIpatternAttrs) :: [(Identifier,Identifier,Bool)]) ((_tlIpatternAttrs) :: [(Identifier,Identifier,Bool)]) ->
     _hdIpatternAttrs ++ _tlIpatternAttrs
   {-# INLINE rule290 #-}
   rule290 = \ ((_hdIcopy) :: Pattern) ((_tlIcopy) :: Patterns) ->
     (:) _hdIcopy _tlIcopy
   {-# INLINE rule291 #-}
   rule291 = \ _copy ->
     _copy
   {-# INLINE rule292 #-}
   rule292 = \ ((_lhsIallTypeSigs) :: Map Identifier Type) ->
     _lhsIallTypeSigs
   {-# INLINE rule293 #-}
   rule293 = \ ((_lhsIaltAttrs) :: Map AltAttr Vertex) ->
     _lhsIaltAttrs
   {-# INLINE rule294 #-}
   rule294 = \ ((_lhsIcon) :: Identifier) ->
     _lhsIcon
   {-# INLINE rule295 #-}
   rule295 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule296 #-}
   rule296 = \ ((_lhsInt) :: Identifier) ->
     _lhsInt
   {-# INLINE rule297 #-}
   rule297 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
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
{-# NOINLINE sem_Patterns_Nil #-}
sem_Patterns_Nil ::  T_Patterns 
sem_Patterns_Nil  = T_Patterns (return st23) where
   {-# NOINLINE st23 #-}
   st23 = let
      v22 :: T_Patterns_v22 
      v22 = \ (T_Patterns_vIn22 _lhsIallTypeSigs _lhsIaltAttrs _lhsIcon _lhsIinh _lhsInt _lhsIsyn) -> ( let
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule304  ()
         _lhsOgathAltAttrs :: [AltAttr]
         _lhsOgathAltAttrs = rule305  ()
         _lhsOinstVars :: [Identifier]
         _lhsOinstVars = rule306  ()
         _lhsOlocVars :: [Identifier]
         _lhsOlocVars = rule307  ()
         _lhsOpatternAttrs :: [(Identifier,Identifier,Bool)]
         _lhsOpatternAttrs = rule308  ()
         _copy = rule309  ()
         _lhsOcopy :: Patterns
         _lhsOcopy = rule310 _copy
         __result_ = T_Patterns_vOut22 _lhsOcopy _lhsOerrors _lhsOgathAltAttrs _lhsOinstVars _lhsOlocVars _lhsOpatternAttrs
         in __result_ )
     in C_Patterns_s23 v22
   {-# INLINE rule304 #-}
   rule304 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule305 #-}
   rule305 = \  (_ :: ()) ->
     []
   {-# INLINE rule306 #-}
   rule306 = \  (_ :: ()) ->
     []
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
   rule310 = \ _copy ->
     _copy

-- Production --------------------------------------------------
-- wrapper
data Inh_Production  = Inh_Production { allnts_Inh_Production :: ([Identifier]), aroundMap_Inh_Production :: (Map ConstructorIdent (Map Identifier [Expression])), cVisitsMap_Inh_Production :: (CVisitsMap), inh_Inh_Production :: (Attributes), inhMap_Inh_Production :: (Map Identifier Attributes), manualAttrDepMap_Inh_Production :: (AttrOrderMap), mergeMap_Inh_Production :: (Map ConstructorIdent (Map Identifier (Identifier,[Identifier]))), nt_Inh_Production :: (Identifier), o_case_Inh_Production :: (Bool), o_cata_Inh_Production :: (Bool), o_dovisit_Inh_Production :: (Bool), o_newtypes_Inh_Production :: (Bool), o_rename_Inh_Production :: (Bool), o_sem_Inh_Production :: (Bool), o_sig_Inh_Production :: (Bool), o_unbox_Inh_Production :: (Bool), o_wantvisit_Inh_Production :: (Bool), prefix_Inh_Production :: (String), syn_Inh_Production :: (Attributes), synMap_Inh_Production :: (Map Identifier Attributes), vcount_Inh_Production :: (Int) }
data Syn_Production  = Syn_Production { additionalDep_Syn_Production :: (Seq Edge), aroundDep_Syn_Production :: (Seq Edge), cProduction_Syn_Production :: (CProduction), cons_Syn_Production :: ([ConstructorIdent]), directDep_Syn_Production :: (Seq Edge), errors_Syn_Production :: (Seq Error), instDep_Syn_Production :: (Seq Edge), mergeDep_Syn_Production :: (Seq Edge), nAutoRules_Syn_Production :: (Int), nExplicitRules_Syn_Production :: (Int), rules_Syn_Production :: (Seq (Vertex,CRule)), vcount_Syn_Production :: (Int) }
{-# INLINABLE wrap_Production #-}
wrap_Production :: T_Production  -> Inh_Production  -> (Syn_Production )
wrap_Production (T_Production act) (Inh_Production _lhsIallnts _lhsIaroundMap _lhsIcVisitsMap _lhsIinh _lhsIinhMap _lhsImanualAttrDepMap _lhsImergeMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_unbox _lhsIo_wantvisit _lhsIprefix _lhsIsyn _lhsIsynMap _lhsIvcount) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Production_vIn25 _lhsIallnts _lhsIaroundMap _lhsIcVisitsMap _lhsIinh _lhsIinhMap _lhsImanualAttrDepMap _lhsImergeMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_unbox _lhsIo_wantvisit _lhsIprefix _lhsIsyn _lhsIsynMap _lhsIvcount
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
data T_Production_vIn25  = T_Production_vIn25 ([Identifier]) (Map ConstructorIdent (Map Identifier [Expression])) (CVisitsMap) (Attributes) (Map Identifier Attributes) (AttrOrderMap) (Map ConstructorIdent (Map Identifier (Identifier,[Identifier]))) (Identifier) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (String) (Attributes) (Map Identifier Attributes) (Int)
data T_Production_vOut25  = T_Production_vOut25 (Seq Edge) (Seq Edge) (CProduction) ([ConstructorIdent]) (Seq Edge) (Seq Error) (Seq Edge) (Seq Edge) (Int) (Int) (Seq (Vertex,CRule)) (Int)
{-# NOINLINE sem_Production_Production #-}
sem_Production_Production :: (ConstructorIdent) -> ([Identifier]) -> ([Type]) -> T_Children  -> T_Rules  -> T_TypeSigs  -> (MaybeMacro) -> T_Production 
sem_Production_Production arg_con_ _ _ arg_children_ arg_rules_ arg_typeSigs_ _ = T_Production (return st26) where
   {-# NOINLINE st26 #-}
   st26 = let
      v25 :: T_Production_v25 
      v25 = \ (T_Production_vIn25 _lhsIallnts _lhsIaroundMap _lhsIcVisitsMap _lhsIinh _lhsIinhMap _lhsImanualAttrDepMap _lhsImergeMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_unbox _lhsIo_wantvisit _lhsIprefix _lhsIsyn _lhsIsynMap _lhsIvcount) -> ( let
         _childrenX5 = Control.Monad.Identity.runIdentity (attach_T_Children (arg_children_))
         _rulesX35 = Control.Monad.Identity.runIdentity (attach_T_Rules (arg_rules_))
         _typeSigsX41 = Control.Monad.Identity.runIdentity (attach_T_TypeSigs (arg_typeSigs_))
         (T_Children_vOut4 _childrenIattributes _childrenIcollectChildrenInhs _childrenIcollectChildrenSyns _childrenIerrors _childrenIfields _childrenIgathAltAttrs _childrenIgathRules _childrenIinhs _childrenInts _childrenIsinglevisits _childrenIterminals) = inv_Children_s5 _childrenX5 (T_Children_vIn4 _childrenOallfields _childrenOallnts _childrenOattrs _childrenOcon _childrenOinh _childrenOinhMap _childrenOmergeMap _childrenOnt _childrenOo_unbox _childrenOsyn _childrenOsynMap)
         (T_Rules_vOut34 _rulesIdirectDep _rulesIerrors _rulesIgathAltAttrs _rulesIgathRules _rulesIinstDep _rulesIinstVars _rulesIlocVars _rulesInAutoRules _rulesInExplicitRules) = inv_Rules_s35 _rulesX35 (T_Rules_vIn34 _rulesOallTypeSigs _rulesOallfields _rulesOallnts _rulesOaltAttrs _rulesOattrs _rulesOchildInhs _rulesOchildNts _rulesOcon _rulesOinh _rulesOinhsOfChildren _rulesOmergeMap _rulesOnt _rulesOo_case _rulesOo_cata _rulesOo_dovisit _rulesOo_newtypes _rulesOo_rename _rulesOo_sem _rulesOo_sig _rulesOo_wantvisit _rulesOprefix _rulesOsyn _rulesOsynsOfChildren)
         (T_TypeSigs_vOut40 _typeSigsItypeSigs) = inv_TypeSigs_s41 _typeSigsX41 (T_TypeSigs_vIn40 _typeSigsOtypeSigs)
         _childrenOcon = rule311 arg_con_
         _rulesOcon = rule312 arg_con_
         _gathAltAttrs = rule313 _childrenIgathAltAttrs _lhsIinh _rulesIgathAltAttrs
         _altAttrs = rule314 _gathAltAttrs _lhsIvcount
         _rulesOchildNts = rule315 _childrenInts
         _rulesOchildInhs = rule316 _childrenIinhs
         _inhRules = rule317 _lhsIinh _lhsInt arg_con_
         _gathRules = rule318 _childrenIgathRules _inhRules _rulesIgathRules
         _lhsOrules :: Seq (Vertex,CRule)
         _lhsOrules = rule319 _gathRules _lhsIvcount
         _lhsOvcount :: Int
         _lhsOvcount = rule320 _gathRules _lhsIvcount
         _manualDeps = rule321 _lhsImanualAttrDepMap _lhsInt arg_con_
         _lhsOadditionalDep :: Seq Edge
         _lhsOadditionalDep = rule322 _altAttrs _manualDeps
         _rulesOsynsOfChildren = rule323 _childrenIcollectChildrenSyns
         _rulesOinhsOfChildren = rule324 _childrenIcollectChildrenInhs
         _mergeMap = rule325 _lhsImergeMap arg_con_
         _lhsOmergeDep :: Seq Edge
         _lhsOmergeDep = rule326 _mergeDep1 _mergeDep2
         _mergeDep1 = rule327 _altAttrs _childrenIcollectChildrenSyns _mergeMap
         _mergeDep2 = rule328 _altAttrs _childrenIcollectChildrenSyns _mergeMap
         _aroundMap = rule329 _lhsIaroundMap arg_con_
         _aroundDep1 = rule330 _altAttrs _aroundMap _childrenIcollectChildrenSyns
         _aroundDep2 = rule331 _altAttrs _aroundMap _childrenIcollectChildrenInhs
         _lhsOaroundDep :: Seq Edge
         _lhsOaroundDep = rule332 _aroundDep1 _aroundDep2
         _lhsOcons :: [ConstructorIdent]
         _lhsOcons = rule333 arg_con_
         _typeSigsOtypeSigs = rule334  ()
         _rulesOallTypeSigs = rule335 _typeSigsItypeSigs
         _cVisits = rule336 _childrenIsinglevisits _gathRules _lhsIcVisitsMap _lhsIinh _lhsInt _lhsIo_dovisit _lhsIsyn arg_con_
         _lhsOcProduction :: CProduction
         _lhsOcProduction = rule337 _cVisits _childrenIfields _childrenIterminals arg_con_
         _allfields = rule338 _childrenIfields
         _attrs = rule339 _childrenIattributes _inhnames _rulesIinstVars _rulesIlocVars
         _inhnames = rule340 _lhsIinh
         _synnames = rule341 _lhsIsyn
         _lhsOdirectDep :: Seq Edge
         _lhsOdirectDep = rule342 _rulesIdirectDep
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule343 _childrenIerrors _rulesIerrors
         _lhsOinstDep :: Seq Edge
         _lhsOinstDep = rule344 _rulesIinstDep
         _lhsOnAutoRules :: Int
         _lhsOnAutoRules = rule345 _rulesInAutoRules
         _lhsOnExplicitRules :: Int
         _lhsOnExplicitRules = rule346 _rulesInExplicitRules
         _childrenOallfields = rule347 _allfields
         _childrenOallnts = rule348 _lhsIallnts
         _childrenOattrs = rule349 _attrs
         _childrenOinh = rule350 _lhsIinh
         _childrenOinhMap = rule351 _lhsIinhMap
         _childrenOmergeMap = rule352 _mergeMap
         _childrenOnt = rule353 _lhsInt
         _childrenOo_unbox = rule354 _lhsIo_unbox
         _childrenOsyn = rule355 _lhsIsyn
         _childrenOsynMap = rule356 _lhsIsynMap
         _rulesOallfields = rule357 _allfields
         _rulesOallnts = rule358 _lhsIallnts
         _rulesOaltAttrs = rule359 _altAttrs
         _rulesOattrs = rule360 _attrs
         _rulesOinh = rule361 _lhsIinh
         _rulesOmergeMap = rule362 _mergeMap
         _rulesOnt = rule363 _lhsInt
         _rulesOo_case = rule364 _lhsIo_case
         _rulesOo_cata = rule365 _lhsIo_cata
         _rulesOo_dovisit = rule366 _lhsIo_dovisit
         _rulesOo_newtypes = rule367 _lhsIo_newtypes
         _rulesOo_rename = rule368 _lhsIo_rename
         _rulesOo_sem = rule369 _lhsIo_sem
         _rulesOo_sig = rule370 _lhsIo_sig
         _rulesOo_wantvisit = rule371 _lhsIo_wantvisit
         _rulesOprefix = rule372 _lhsIprefix
         _rulesOsyn = rule373 _lhsIsyn
         __result_ = T_Production_vOut25 _lhsOadditionalDep _lhsOaroundDep _lhsOcProduction _lhsOcons _lhsOdirectDep _lhsOerrors _lhsOinstDep _lhsOmergeDep _lhsOnAutoRules _lhsOnExplicitRules _lhsOrules _lhsOvcount
         in __result_ )
     in C_Production_s26 v25
   {-# INLINE rule311 #-}
   {-# LINE 94 "./src-ag/Order.ag" #-}
   rule311 = \ con_ ->
                                  {-# LINE 94 "./src-ag/Order.ag" #-}
                                  con_
                                  {-# LINE 2461 "dist/build/Order.hs"#-}
   {-# INLINE rule312 #-}
   {-# LINE 96 "./src-ag/Order.ag" #-}
   rule312 = \ con_ ->
                               {-# LINE 96 "./src-ag/Order.ag" #-}
                               con_
                               {-# LINE 2467 "dist/build/Order.hs"#-}
   {-# INLINE rule313 #-}
   {-# LINE 173 "./src-ag/Order.ag" #-}
   rule313 = \ ((_childrenIgathAltAttrs) :: [AltAttr]) ((_lhsIinh) :: Attributes) ((_rulesIgathAltAttrs) :: [AltAttr]) ->
                                       {-# LINE 173 "./src-ag/Order.ag" #-}
                                       [ AltAttr _LHS inh True | inh <- Map.keys _lhsIinh ]
                                        ++ _childrenIgathAltAttrs
                                        ++ _rulesIgathAltAttrs
                                       {-# LINE 2475 "dist/build/Order.hs"#-}
   {-# INLINE rule314 #-}
   {-# LINE 189 "./src-ag/Order.ag" #-}
   rule314 = \ _gathAltAttrs ((_lhsIvcount) :: Int) ->
                                 {-# LINE 189 "./src-ag/Order.ag" #-}
                                 Map.fromList (zip _gathAltAttrs [_lhsIvcount..])
                                 {-# LINE 2481 "dist/build/Order.hs"#-}
   {-# INLINE rule315 #-}
   {-# LINE 202 "./src-ag/Order.ag" #-}
   rule315 = \ ((_childrenInts) :: Seq (Identifier,NontermIdent)) ->
                                    {-# LINE 202 "./src-ag/Order.ag" #-}
                                    Map.fromList (toList _childrenInts)
                                    {-# LINE 2487 "dist/build/Order.hs"#-}
   {-# INLINE rule316 #-}
   {-# LINE 203 "./src-ag/Order.ag" #-}
   rule316 = \ ((_childrenIinhs) :: Seq (Identifier,Attributes)) ->
                                      {-# LINE 203 "./src-ag/Order.ag" #-}
                                      Map.fromList (toList _childrenIinhs)
                                      {-# LINE 2493 "dist/build/Order.hs"#-}
   {-# INLINE rule317 #-}
   {-# LINE 209 "./src-ag/Order.ag" #-}
   rule317 = \ ((_lhsIinh) :: Attributes) ((_lhsInt) :: Identifier) con_ ->
                                  {-# LINE 209 "./src-ag/Order.ag" #-}
                                  [ cRuleLhsInh inh _lhsInt con_ tp | (inh,tp) <- Map.assocs _lhsIinh ]
                                  {-# LINE 2499 "dist/build/Order.hs"#-}
   {-# INLINE rule318 #-}
   {-# LINE 210 "./src-ag/Order.ag" #-}
   rule318 = \ ((_childrenIgathRules) :: Seq CRule) _inhRules ((_rulesIgathRules) :: Seq CRule) ->
                                    {-# LINE 210 "./src-ag/Order.ag" #-}
                                    _inhRules ++ toList (_childrenIgathRules Seq.>< _rulesIgathRules)
                                    {-# LINE 2505 "dist/build/Order.hs"#-}
   {-# INLINE rule319 #-}
   {-# LINE 262 "./src-ag/Order.ag" #-}
   rule319 = \ _gathRules ((_lhsIvcount) :: Int) ->
                               {-# LINE 262 "./src-ag/Order.ag" #-}
                               Seq.fromList (zip [_lhsIvcount..] _gathRules)
                               {-# LINE 2511 "dist/build/Order.hs"#-}
   {-# INLINE rule320 #-}
   {-# LINE 263 "./src-ag/Order.ag" #-}
   rule320 = \ _gathRules ((_lhsIvcount) :: Int) ->
                                 {-# LINE 263 "./src-ag/Order.ag" #-}
                                 _lhsIvcount + length _gathRules
                                 {-# LINE 2517 "dist/build/Order.hs"#-}
   {-# INLINE rule321 #-}
   {-# LINE 291 "./src-ag/Order.ag" #-}
   rule321 = \ ((_lhsImanualAttrDepMap) :: AttrOrderMap) ((_lhsInt) :: Identifier) con_ ->
            {-# LINE 291 "./src-ag/Order.ag" #-}
            Set.toList $ Map.findWithDefault Set.empty con_ $ Map.findWithDefault Map.empty _lhsInt _lhsImanualAttrDepMap
            {-# LINE 2523 "dist/build/Order.hs"#-}
   {-# INLINE rule322 #-}
   {-# LINE 294 "./src-ag/Order.ag" #-}
   rule322 = \ _altAttrs _manualDeps ->
            {-# LINE 294 "./src-ag/Order.ag" #-}
            Seq.fromList [ (vertex True occA, vertex False occB)
                         | Dependency occA occB <- _manualDeps
                         , let vertex inout (OccAttr child nm)
                                 | child == _LOC = findWithErr2 (AltAttr _LOC nm True) _altAttrs
                                 | otherwise     = findWithErr2 (AltAttr child nm inout) _altAttrs
                               vertex _ (OccRule nm)
                                 = findWithErr2 (AltAttr _LOC (Ident ("_rule_" ++ show nm) (getPos nm)) True) _altAttrs
                         ]
            {-# LINE 2536 "dist/build/Order.hs"#-}
   {-# INLINE rule323 #-}
   {-# LINE 340 "./src-ag/Order.ag" #-}
   rule323 = \ ((_childrenIcollectChildrenSyns) :: Map Identifier Attributes ) ->
                                         {-# LINE 340 "./src-ag/Order.ag" #-}
                                         _childrenIcollectChildrenSyns
                                         {-# LINE 2542 "dist/build/Order.hs"#-}
   {-# INLINE rule324 #-}
   {-# LINE 341 "./src-ag/Order.ag" #-}
   rule324 = \ ((_childrenIcollectChildrenInhs) :: Map Identifier Attributes ) ->
                                         {-# LINE 341 "./src-ag/Order.ag" #-}
                                         _childrenIcollectChildrenInhs
                                         {-# LINE 2548 "dist/build/Order.hs"#-}
   {-# INLINE rule325 #-}
   {-# LINE 359 "./src-ag/Order.ag" #-}
   rule325 = \ ((_lhsImergeMap) :: Map ConstructorIdent (Map Identifier (Identifier,[Identifier]))) con_ ->
                                                {-# LINE 359 "./src-ag/Order.ag" #-}
                                                Map.findWithDefault Map.empty con_ _lhsImergeMap
                                                {-# LINE 2554 "dist/build/Order.hs"#-}
   {-# INLINE rule326 #-}
   {-# LINE 370 "./src-ag/Order.ag" #-}
   rule326 = \ _mergeDep1 _mergeDep2 ->
                       {-# LINE 370 "./src-ag/Order.ag" #-}
                       _mergeDep1     Seq.>< _mergeDep2
                       {-# LINE 2560 "dist/build/Order.hs"#-}
   {-# INLINE rule327 #-}
   {-# LINE 372 "./src-ag/Order.ag" #-}
   rule327 = \ _altAttrs ((_childrenIcollectChildrenSyns) :: Map Identifier Attributes ) _mergeMap ->
            {-# LINE 372 "./src-ag/Order.ag" #-}
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
            {-# LINE 2575 "dist/build/Order.hs"#-}
   {-# INLINE rule328 #-}
   {-# LINE 383 "./src-ag/Order.ag" #-}
   rule328 = \ _altAttrs ((_childrenIcollectChildrenSyns) :: Map Identifier Attributes ) _mergeMap ->
            {-# LINE 383 "./src-ag/Order.ag" #-}
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
            {-# LINE 2590 "dist/build/Order.hs"#-}
   {-# INLINE rule329 #-}
   {-# LINE 412 "./src-ag/Order.ag" #-}
   rule329 = \ ((_lhsIaroundMap) :: Map ConstructorIdent (Map Identifier [Expression])) con_ ->
                                                 {-# LINE 412 "./src-ag/Order.ag" #-}
                                                 Map.findWithDefault Map.empty con_ _lhsIaroundMap
                                                 {-# LINE 2596 "dist/build/Order.hs"#-}
   {-# INLINE rule330 #-}
   {-# LINE 420 "./src-ag/Order.ag" #-}
   rule330 = \ _altAttrs _aroundMap ((_childrenIcollectChildrenSyns) :: Map Identifier Attributes ) ->
           {-# LINE 420 "./src-ag/Order.ag" #-}
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
           {-# LINE 2611 "dist/build/Order.hs"#-}
   {-# INLINE rule331 #-}
   {-# LINE 431 "./src-ag/Order.ag" #-}
   rule331 = \ _altAttrs _aroundMap ((_childrenIcollectChildrenInhs) :: Map Identifier Attributes ) ->
            {-# LINE 431 "./src-ag/Order.ag" #-}
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
            {-# LINE 2626 "dist/build/Order.hs"#-}
   {-# INLINE rule332 #-}
   {-# LINE 441 "./src-ag/Order.ag" #-}
   rule332 = \ _aroundDep1 _aroundDep2 ->
                       {-# LINE 441 "./src-ag/Order.ag" #-}
                       _aroundDep1     Seq.>< _aroundDep2
                       {-# LINE 2632 "dist/build/Order.hs"#-}
   {-# INLINE rule333 #-}
   {-# LINE 523 "./src-ag/Order.ag" #-}
   rule333 = \ con_ ->
                              {-# LINE 523 "./src-ag/Order.ag" #-}
                              [con_]
                              {-# LINE 2638 "dist/build/Order.hs"#-}
   {-# INLINE rule334 #-}
   {-# LINE 530 "./src-ag/Order.ag" #-}
   rule334 = \  (_ :: ()) ->
                                     {-# LINE 530 "./src-ag/Order.ag" #-}
                                     Map.empty
                                     {-# LINE 2644 "dist/build/Order.hs"#-}
   {-# INLINE rule335 #-}
   {-# LINE 536 "./src-ag/Order.ag" #-}
   rule335 = \ ((_typeSigsItypeSigs) :: Map Identifier Type) ->
                                      {-# LINE 536 "./src-ag/Order.ag" #-}
                                      _typeSigsItypeSigs
                                      {-# LINE 2650 "dist/build/Order.hs"#-}
   {-# INLINE rule336 #-}
   {-# LINE 604 "./src-ag/Order.ag" #-}
   rule336 = \ ((_childrenIsinglevisits) :: [CRule]) _gathRules ((_lhsIcVisitsMap) :: CVisitsMap) ((_lhsIinh) :: Attributes) ((_lhsInt) :: Identifier) ((_lhsIo_dovisit) :: Bool) ((_lhsIsyn) :: Attributes) con_ ->
                                {-# LINE 604 "./src-ag/Order.ag" #-}
                                if  _lhsIo_dovisit
                                     then let prodsVisitsMap = findWithErr1 "Production.cVisits.nt" _lhsInt _lhsIcVisitsMap
                                              visits = findWithErr1 "Production.cVisits.con" con_ prodsVisitsMap
                                           in visits
                                     else  let  vss = nubBy eqCRuleDefines _gathRules ++ _childrenIsinglevisits
                                           in  [CVisit _lhsIinh _lhsIsyn vss [] False]
                                {-# LINE 2661 "dist/build/Order.hs"#-}
   {-# INLINE rule337 #-}
   {-# LINE 630 "./src-ag/Order.ag" #-}
   rule337 = \ _cVisits ((_childrenIfields) :: [(Identifier,Type,ChildKind)]) ((_childrenIterminals) :: [Identifier]) con_ ->
                                     {-# LINE 630 "./src-ag/Order.ag" #-}
                                     CProduction con_ _cVisits _childrenIfields _childrenIterminals
                                     {-# LINE 2667 "dist/build/Order.hs"#-}
   {-# INLINE rule338 #-}
   {-# LINE 658 "./src-ag/Order.ag" #-}
   rule338 = \ ((_childrenIfields) :: [(Identifier,Type,ChildKind)]) ->
                                  {-# LINE 658 "./src-ag/Order.ag" #-}
                                  _childrenIfields
                                  {-# LINE 2673 "dist/build/Order.hs"#-}
   {-# INLINE rule339 #-}
   {-# LINE 659 "./src-ag/Order.ag" #-}
   rule339 = \ ((_childrenIattributes) :: [(Identifier,Attributes,Attributes)]) _inhnames ((_rulesIinstVars) :: [Identifier]) ((_rulesIlocVars) :: [Identifier]) ->
                                   {-# LINE 659 "./src-ag/Order.ag" #-}
                                   map ((,) _LOC)  _rulesIlocVars ++
                                   map ((,) _INST) _rulesIinstVars ++
                                   map ((,) _LHS)  _inhnames ++
                                   concat [map ((,) nm) (Map.keys as) | (nm,_,as) <- _childrenIattributes]
                                   {-# LINE 2682 "dist/build/Order.hs"#-}
   {-# INLINE rule340 #-}
   {-# LINE 663 "./src-ag/Order.ag" #-}
   rule340 = \ ((_lhsIinh) :: Attributes) ->
                                   {-# LINE 663 "./src-ag/Order.ag" #-}
                                   Map.keys _lhsIinh
                                   {-# LINE 2688 "dist/build/Order.hs"#-}
   {-# INLINE rule341 #-}
   {-# LINE 664 "./src-ag/Order.ag" #-}
   rule341 = \ ((_lhsIsyn) :: Attributes) ->
                                   {-# LINE 664 "./src-ag/Order.ag" #-}
                                   Map.keys _lhsIsyn
                                   {-# LINE 2694 "dist/build/Order.hs"#-}
   {-# INLINE rule342 #-}
   rule342 = \ ((_rulesIdirectDep) :: Seq Edge) ->
     _rulesIdirectDep
   {-# INLINE rule343 #-}
   rule343 = \ ((_childrenIerrors) :: Seq Error) ((_rulesIerrors) :: Seq Error) ->
     _childrenIerrors Seq.>< _rulesIerrors
   {-# INLINE rule344 #-}
   rule344 = \ ((_rulesIinstDep) :: Seq Edge) ->
     _rulesIinstDep
   {-# INLINE rule345 #-}
   rule345 = \ ((_rulesInAutoRules) :: Int) ->
     _rulesInAutoRules
   {-# INLINE rule346 #-}
   rule346 = \ ((_rulesInExplicitRules) :: Int) ->
     _rulesInExplicitRules
   {-# INLINE rule347 #-}
   rule347 = \ _allfields ->
     _allfields
   {-# INLINE rule348 #-}
   rule348 = \ ((_lhsIallnts) :: [Identifier]) ->
     _lhsIallnts
   {-# INLINE rule349 #-}
   rule349 = \ _attrs ->
     _attrs
   {-# INLINE rule350 #-}
   rule350 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule351 #-}
   rule351 = \ ((_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# INLINE rule352 #-}
   rule352 = \ _mergeMap ->
     _mergeMap
   {-# INLINE rule353 #-}
   rule353 = \ ((_lhsInt) :: Identifier) ->
     _lhsInt
   {-# INLINE rule354 #-}
   rule354 = \ ((_lhsIo_unbox) :: Bool) ->
     _lhsIo_unbox
   {-# INLINE rule355 #-}
   rule355 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
   {-# INLINE rule356 #-}
   rule356 = \ ((_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
   {-# INLINE rule357 #-}
   rule357 = \ _allfields ->
     _allfields
   {-# INLINE rule358 #-}
   rule358 = \ ((_lhsIallnts) :: [Identifier]) ->
     _lhsIallnts
   {-# INLINE rule359 #-}
   rule359 = \ _altAttrs ->
     _altAttrs
   {-# INLINE rule360 #-}
   rule360 = \ _attrs ->
     _attrs
   {-# INLINE rule361 #-}
   rule361 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule362 #-}
   rule362 = \ _mergeMap ->
     _mergeMap
   {-# INLINE rule363 #-}
   rule363 = \ ((_lhsInt) :: Identifier) ->
     _lhsInt
   {-# INLINE rule364 #-}
   rule364 = \ ((_lhsIo_case) :: Bool) ->
     _lhsIo_case
   {-# INLINE rule365 #-}
   rule365 = \ ((_lhsIo_cata) :: Bool) ->
     _lhsIo_cata
   {-# INLINE rule366 #-}
   rule366 = \ ((_lhsIo_dovisit) :: Bool) ->
     _lhsIo_dovisit
   {-# INLINE rule367 #-}
   rule367 = \ ((_lhsIo_newtypes) :: Bool) ->
     _lhsIo_newtypes
   {-# INLINE rule368 #-}
   rule368 = \ ((_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# INLINE rule369 #-}
   rule369 = \ ((_lhsIo_sem) :: Bool) ->
     _lhsIo_sem
   {-# INLINE rule370 #-}
   rule370 = \ ((_lhsIo_sig) :: Bool) ->
     _lhsIo_sig
   {-# INLINE rule371 #-}
   rule371 = \ ((_lhsIo_wantvisit) :: Bool) ->
     _lhsIo_wantvisit
   {-# INLINE rule372 #-}
   rule372 = \ ((_lhsIprefix) :: String) ->
     _lhsIprefix
   {-# INLINE rule373 #-}
   rule373 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn

-- Productions -------------------------------------------------
-- wrapper
data Inh_Productions  = Inh_Productions { allnts_Inh_Productions :: ([Identifier]), aroundMap_Inh_Productions :: (Map ConstructorIdent (Map Identifier [Expression])), cVisitsMap_Inh_Productions :: (CVisitsMap), inh_Inh_Productions :: (Attributes), inhMap_Inh_Productions :: (Map Identifier Attributes), manualAttrDepMap_Inh_Productions :: (AttrOrderMap), mergeMap_Inh_Productions :: (Map ConstructorIdent (Map Identifier (Identifier,[Identifier]))), nt_Inh_Productions :: (Identifier), o_case_Inh_Productions :: (Bool), o_cata_Inh_Productions :: (Bool), o_dovisit_Inh_Productions :: (Bool), o_newtypes_Inh_Productions :: (Bool), o_rename_Inh_Productions :: (Bool), o_sem_Inh_Productions :: (Bool), o_sig_Inh_Productions :: (Bool), o_unbox_Inh_Productions :: (Bool), o_wantvisit_Inh_Productions :: (Bool), prefix_Inh_Productions :: (String), syn_Inh_Productions :: (Attributes), synMap_Inh_Productions :: (Map Identifier Attributes), vcount_Inh_Productions :: (Int) }
data Syn_Productions  = Syn_Productions { additionalDep_Syn_Productions :: (Seq Edge), aroundDep_Syn_Productions :: (Seq Edge), cProductions_Syn_Productions :: (CProductions), cons_Syn_Productions :: ([ConstructorIdent]), directDep_Syn_Productions :: (Seq Edge), errors_Syn_Productions :: (Seq Error), instDep_Syn_Productions :: (Seq Edge), mergeDep_Syn_Productions :: (Seq Edge), nAutoRules_Syn_Productions :: (Int), nExplicitRules_Syn_Productions :: (Int), rules_Syn_Productions :: (Seq (Vertex,CRule)), vcount_Syn_Productions :: (Int) }
{-# INLINABLE wrap_Productions #-}
wrap_Productions :: T_Productions  -> Inh_Productions  -> (Syn_Productions )
wrap_Productions (T_Productions act) (Inh_Productions _lhsIallnts _lhsIaroundMap _lhsIcVisitsMap _lhsIinh _lhsIinhMap _lhsImanualAttrDepMap _lhsImergeMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_unbox _lhsIo_wantvisit _lhsIprefix _lhsIsyn _lhsIsynMap _lhsIvcount) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Productions_vIn28 _lhsIallnts _lhsIaroundMap _lhsIcVisitsMap _lhsIinh _lhsIinhMap _lhsImanualAttrDepMap _lhsImergeMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_unbox _lhsIo_wantvisit _lhsIprefix _lhsIsyn _lhsIsynMap _lhsIvcount
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
data T_Productions_vIn28  = T_Productions_vIn28 ([Identifier]) (Map ConstructorIdent (Map Identifier [Expression])) (CVisitsMap) (Attributes) (Map Identifier Attributes) (AttrOrderMap) (Map ConstructorIdent (Map Identifier (Identifier,[Identifier]))) (Identifier) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (String) (Attributes) (Map Identifier Attributes) (Int)
data T_Productions_vOut28  = T_Productions_vOut28 (Seq Edge) (Seq Edge) (CProductions) ([ConstructorIdent]) (Seq Edge) (Seq Error) (Seq Edge) (Seq Edge) (Int) (Int) (Seq (Vertex,CRule)) (Int)
{-# NOINLINE sem_Productions_Cons #-}
sem_Productions_Cons :: T_Production  -> T_Productions  -> T_Productions 
sem_Productions_Cons arg_hd_ arg_tl_ = T_Productions (return st29) where
   {-# NOINLINE st29 #-}
   st29 = let
      v28 :: T_Productions_v28 
      v28 = \ (T_Productions_vIn28 _lhsIallnts _lhsIaroundMap _lhsIcVisitsMap _lhsIinh _lhsIinhMap _lhsImanualAttrDepMap _lhsImergeMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_unbox _lhsIo_wantvisit _lhsIprefix _lhsIsyn _lhsIsynMap _lhsIvcount) -> ( let
         _hdX26 = Control.Monad.Identity.runIdentity (attach_T_Production (arg_hd_))
         _tlX29 = Control.Monad.Identity.runIdentity (attach_T_Productions (arg_tl_))
         (T_Production_vOut25 _hdIadditionalDep _hdIaroundDep _hdIcProduction _hdIcons _hdIdirectDep _hdIerrors _hdIinstDep _hdImergeDep _hdInAutoRules _hdInExplicitRules _hdIrules _hdIvcount) = inv_Production_s26 _hdX26 (T_Production_vIn25 _hdOallnts _hdOaroundMap _hdOcVisitsMap _hdOinh _hdOinhMap _hdOmanualAttrDepMap _hdOmergeMap _hdOnt _hdOo_case _hdOo_cata _hdOo_dovisit _hdOo_newtypes _hdOo_rename _hdOo_sem _hdOo_sig _hdOo_unbox _hdOo_wantvisit _hdOprefix _hdOsyn _hdOsynMap _hdOvcount)
         (T_Productions_vOut28 _tlIadditionalDep _tlIaroundDep _tlIcProductions _tlIcons _tlIdirectDep _tlIerrors _tlIinstDep _tlImergeDep _tlInAutoRules _tlInExplicitRules _tlIrules _tlIvcount) = inv_Productions_s29 _tlX29 (T_Productions_vIn28 _tlOallnts _tlOaroundMap _tlOcVisitsMap _tlOinh _tlOinhMap _tlOmanualAttrDepMap _tlOmergeMap _tlOnt _tlOo_case _tlOo_cata _tlOo_dovisit _tlOo_newtypes _tlOo_rename _tlOo_sem _tlOo_sig _tlOo_unbox _tlOo_wantvisit _tlOprefix _tlOsyn _tlOsynMap _tlOvcount)
         _lhsOcProductions :: CProductions
         _lhsOcProductions = rule374 _hdIcProduction _tlIcProductions
         _lhsOadditionalDep :: Seq Edge
         _lhsOadditionalDep = rule375 _hdIadditionalDep _tlIadditionalDep
         _lhsOaroundDep :: Seq Edge
         _lhsOaroundDep = rule376 _hdIaroundDep _tlIaroundDep
         _lhsOcons :: [ConstructorIdent]
         _lhsOcons = rule377 _hdIcons _tlIcons
         _lhsOdirectDep :: Seq Edge
         _lhsOdirectDep = rule378 _hdIdirectDep _tlIdirectDep
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule379 _hdIerrors _tlIerrors
         _lhsOinstDep :: Seq Edge
         _lhsOinstDep = rule380 _hdIinstDep _tlIinstDep
         _lhsOmergeDep :: Seq Edge
         _lhsOmergeDep = rule381 _hdImergeDep _tlImergeDep
         _lhsOnAutoRules :: Int
         _lhsOnAutoRules = rule382 _hdInAutoRules _tlInAutoRules
         _lhsOnExplicitRules :: Int
         _lhsOnExplicitRules = rule383 _hdInExplicitRules _tlInExplicitRules
         _lhsOrules :: Seq (Vertex,CRule)
         _lhsOrules = rule384 _hdIrules _tlIrules
         _lhsOvcount :: Int
         _lhsOvcount = rule385 _tlIvcount
         _hdOallnts = rule386 _lhsIallnts
         _hdOaroundMap = rule387 _lhsIaroundMap
         _hdOcVisitsMap = rule388 _lhsIcVisitsMap
         _hdOinh = rule389 _lhsIinh
         _hdOinhMap = rule390 _lhsIinhMap
         _hdOmanualAttrDepMap = rule391 _lhsImanualAttrDepMap
         _hdOmergeMap = rule392 _lhsImergeMap
         _hdOnt = rule393 _lhsInt
         _hdOo_case = rule394 _lhsIo_case
         _hdOo_cata = rule395 _lhsIo_cata
         _hdOo_dovisit = rule396 _lhsIo_dovisit
         _hdOo_newtypes = rule397 _lhsIo_newtypes
         _hdOo_rename = rule398 _lhsIo_rename
         _hdOo_sem = rule399 _lhsIo_sem
         _hdOo_sig = rule400 _lhsIo_sig
         _hdOo_unbox = rule401 _lhsIo_unbox
         _hdOo_wantvisit = rule402 _lhsIo_wantvisit
         _hdOprefix = rule403 _lhsIprefix
         _hdOsyn = rule404 _lhsIsyn
         _hdOsynMap = rule405 _lhsIsynMap
         _hdOvcount = rule406 _lhsIvcount
         _tlOallnts = rule407 _lhsIallnts
         _tlOaroundMap = rule408 _lhsIaroundMap
         _tlOcVisitsMap = rule409 _lhsIcVisitsMap
         _tlOinh = rule410 _lhsIinh
         _tlOinhMap = rule411 _lhsIinhMap
         _tlOmanualAttrDepMap = rule412 _lhsImanualAttrDepMap
         _tlOmergeMap = rule413 _lhsImergeMap
         _tlOnt = rule414 _lhsInt
         _tlOo_case = rule415 _lhsIo_case
         _tlOo_cata = rule416 _lhsIo_cata
         _tlOo_dovisit = rule417 _lhsIo_dovisit
         _tlOo_newtypes = rule418 _lhsIo_newtypes
         _tlOo_rename = rule419 _lhsIo_rename
         _tlOo_sem = rule420 _lhsIo_sem
         _tlOo_sig = rule421 _lhsIo_sig
         _tlOo_unbox = rule422 _lhsIo_unbox
         _tlOo_wantvisit = rule423 _lhsIo_wantvisit
         _tlOprefix = rule424 _lhsIprefix
         _tlOsyn = rule425 _lhsIsyn
         _tlOsynMap = rule426 _lhsIsynMap
         _tlOvcount = rule427 _hdIvcount
         __result_ = T_Productions_vOut28 _lhsOadditionalDep _lhsOaroundDep _lhsOcProductions _lhsOcons _lhsOdirectDep _lhsOerrors _lhsOinstDep _lhsOmergeDep _lhsOnAutoRules _lhsOnExplicitRules _lhsOrules _lhsOvcount
         in __result_ )
     in C_Productions_s29 v28
   {-# INLINE rule374 #-}
   {-# LINE 627 "./src-ag/Order.ag" #-}
   rule374 = \ ((_hdIcProduction) :: CProduction) ((_tlIcProductions) :: CProductions) ->
                                {-# LINE 627 "./src-ag/Order.ag" #-}
                                _hdIcProduction : _tlIcProductions
                                {-# LINE 2907 "dist/build/Order.hs"#-}
   {-# INLINE rule375 #-}
   rule375 = \ ((_hdIadditionalDep) :: Seq Edge) ((_tlIadditionalDep) :: Seq Edge) ->
     _hdIadditionalDep Seq.>< _tlIadditionalDep
   {-# INLINE rule376 #-}
   rule376 = \ ((_hdIaroundDep) :: Seq Edge) ((_tlIaroundDep) :: Seq Edge) ->
     _hdIaroundDep Seq.>< _tlIaroundDep
   {-# INLINE rule377 #-}
   rule377 = \ ((_hdIcons) :: [ConstructorIdent]) ((_tlIcons) :: [ConstructorIdent]) ->
     _hdIcons ++ _tlIcons
   {-# INLINE rule378 #-}
   rule378 = \ ((_hdIdirectDep) :: Seq Edge) ((_tlIdirectDep) :: Seq Edge) ->
     _hdIdirectDep Seq.>< _tlIdirectDep
   {-# INLINE rule379 #-}
   rule379 = \ ((_hdIerrors) :: Seq Error) ((_tlIerrors) :: Seq Error) ->
     _hdIerrors Seq.>< _tlIerrors
   {-# INLINE rule380 #-}
   rule380 = \ ((_hdIinstDep) :: Seq Edge) ((_tlIinstDep) :: Seq Edge) ->
     _hdIinstDep Seq.>< _tlIinstDep
   {-# INLINE rule381 #-}
   rule381 = \ ((_hdImergeDep) :: Seq Edge) ((_tlImergeDep) :: Seq Edge) ->
     _hdImergeDep Seq.>< _tlImergeDep
   {-# INLINE rule382 #-}
   rule382 = \ ((_hdInAutoRules) :: Int) ((_tlInAutoRules) :: Int) ->
     _hdInAutoRules + _tlInAutoRules
   {-# INLINE rule383 #-}
   rule383 = \ ((_hdInExplicitRules) :: Int) ((_tlInExplicitRules) :: Int) ->
     _hdInExplicitRules + _tlInExplicitRules
   {-# INLINE rule384 #-}
   rule384 = \ ((_hdIrules) :: Seq (Vertex,CRule)) ((_tlIrules) :: Seq (Vertex,CRule)) ->
     _hdIrules Seq.>< _tlIrules
   {-# INLINE rule385 #-}
   rule385 = \ ((_tlIvcount) :: Int) ->
     _tlIvcount
   {-# INLINE rule386 #-}
   rule386 = \ ((_lhsIallnts) :: [Identifier]) ->
     _lhsIallnts
   {-# INLINE rule387 #-}
   rule387 = \ ((_lhsIaroundMap) :: Map ConstructorIdent (Map Identifier [Expression])) ->
     _lhsIaroundMap
   {-# INLINE rule388 #-}
   rule388 = \ ((_lhsIcVisitsMap) :: CVisitsMap) ->
     _lhsIcVisitsMap
   {-# INLINE rule389 #-}
   rule389 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule390 #-}
   rule390 = \ ((_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# INLINE rule391 #-}
   rule391 = \ ((_lhsImanualAttrDepMap) :: AttrOrderMap) ->
     _lhsImanualAttrDepMap
   {-# INLINE rule392 #-}
   rule392 = \ ((_lhsImergeMap) :: Map ConstructorIdent (Map Identifier (Identifier,[Identifier]))) ->
     _lhsImergeMap
   {-# INLINE rule393 #-}
   rule393 = \ ((_lhsInt) :: Identifier) ->
     _lhsInt
   {-# INLINE rule394 #-}
   rule394 = \ ((_lhsIo_case) :: Bool) ->
     _lhsIo_case
   {-# INLINE rule395 #-}
   rule395 = \ ((_lhsIo_cata) :: Bool) ->
     _lhsIo_cata
   {-# INLINE rule396 #-}
   rule396 = \ ((_lhsIo_dovisit) :: Bool) ->
     _lhsIo_dovisit
   {-# INLINE rule397 #-}
   rule397 = \ ((_lhsIo_newtypes) :: Bool) ->
     _lhsIo_newtypes
   {-# INLINE rule398 #-}
   rule398 = \ ((_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# INLINE rule399 #-}
   rule399 = \ ((_lhsIo_sem) :: Bool) ->
     _lhsIo_sem
   {-# INLINE rule400 #-}
   rule400 = \ ((_lhsIo_sig) :: Bool) ->
     _lhsIo_sig
   {-# INLINE rule401 #-}
   rule401 = \ ((_lhsIo_unbox) :: Bool) ->
     _lhsIo_unbox
   {-# INLINE rule402 #-}
   rule402 = \ ((_lhsIo_wantvisit) :: Bool) ->
     _lhsIo_wantvisit
   {-# INLINE rule403 #-}
   rule403 = \ ((_lhsIprefix) :: String) ->
     _lhsIprefix
   {-# INLINE rule404 #-}
   rule404 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
   {-# INLINE rule405 #-}
   rule405 = \ ((_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
   {-# INLINE rule406 #-}
   rule406 = \ ((_lhsIvcount) :: Int) ->
     _lhsIvcount
   {-# INLINE rule407 #-}
   rule407 = \ ((_lhsIallnts) :: [Identifier]) ->
     _lhsIallnts
   {-# INLINE rule408 #-}
   rule408 = \ ((_lhsIaroundMap) :: Map ConstructorIdent (Map Identifier [Expression])) ->
     _lhsIaroundMap
   {-# INLINE rule409 #-}
   rule409 = \ ((_lhsIcVisitsMap) :: CVisitsMap) ->
     _lhsIcVisitsMap
   {-# INLINE rule410 #-}
   rule410 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule411 #-}
   rule411 = \ ((_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# INLINE rule412 #-}
   rule412 = \ ((_lhsImanualAttrDepMap) :: AttrOrderMap) ->
     _lhsImanualAttrDepMap
   {-# INLINE rule413 #-}
   rule413 = \ ((_lhsImergeMap) :: Map ConstructorIdent (Map Identifier (Identifier,[Identifier]))) ->
     _lhsImergeMap
   {-# INLINE rule414 #-}
   rule414 = \ ((_lhsInt) :: Identifier) ->
     _lhsInt
   {-# INLINE rule415 #-}
   rule415 = \ ((_lhsIo_case) :: Bool) ->
     _lhsIo_case
   {-# INLINE rule416 #-}
   rule416 = \ ((_lhsIo_cata) :: Bool) ->
     _lhsIo_cata
   {-# INLINE rule417 #-}
   rule417 = \ ((_lhsIo_dovisit) :: Bool) ->
     _lhsIo_dovisit
   {-# INLINE rule418 #-}
   rule418 = \ ((_lhsIo_newtypes) :: Bool) ->
     _lhsIo_newtypes
   {-# INLINE rule419 #-}
   rule419 = \ ((_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# INLINE rule420 #-}
   rule420 = \ ((_lhsIo_sem) :: Bool) ->
     _lhsIo_sem
   {-# INLINE rule421 #-}
   rule421 = \ ((_lhsIo_sig) :: Bool) ->
     _lhsIo_sig
   {-# INLINE rule422 #-}
   rule422 = \ ((_lhsIo_unbox) :: Bool) ->
     _lhsIo_unbox
   {-# INLINE rule423 #-}
   rule423 = \ ((_lhsIo_wantvisit) :: Bool) ->
     _lhsIo_wantvisit
   {-# INLINE rule424 #-}
   rule424 = \ ((_lhsIprefix) :: String) ->
     _lhsIprefix
   {-# INLINE rule425 #-}
   rule425 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
   {-# INLINE rule426 #-}
   rule426 = \ ((_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
   {-# INLINE rule427 #-}
   rule427 = \ ((_hdIvcount) :: Int) ->
     _hdIvcount
{-# NOINLINE sem_Productions_Nil #-}
sem_Productions_Nil ::  T_Productions 
sem_Productions_Nil  = T_Productions (return st29) where
   {-# NOINLINE st29 #-}
   st29 = let
      v28 :: T_Productions_v28 
      v28 = \ (T_Productions_vIn28 _lhsIallnts _lhsIaroundMap _lhsIcVisitsMap _lhsIinh _lhsIinhMap _lhsImanualAttrDepMap _lhsImergeMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_unbox _lhsIo_wantvisit _lhsIprefix _lhsIsyn _lhsIsynMap _lhsIvcount) -> ( let
         _lhsOcProductions :: CProductions
         _lhsOcProductions = rule428  ()
         _lhsOadditionalDep :: Seq Edge
         _lhsOadditionalDep = rule429  ()
         _lhsOaroundDep :: Seq Edge
         _lhsOaroundDep = rule430  ()
         _lhsOcons :: [ConstructorIdent]
         _lhsOcons = rule431  ()
         _lhsOdirectDep :: Seq Edge
         _lhsOdirectDep = rule432  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule433  ()
         _lhsOinstDep :: Seq Edge
         _lhsOinstDep = rule434  ()
         _lhsOmergeDep :: Seq Edge
         _lhsOmergeDep = rule435  ()
         _lhsOnAutoRules :: Int
         _lhsOnAutoRules = rule436  ()
         _lhsOnExplicitRules :: Int
         _lhsOnExplicitRules = rule437  ()
         _lhsOrules :: Seq (Vertex,CRule)
         _lhsOrules = rule438  ()
         _lhsOvcount :: Int
         _lhsOvcount = rule439 _lhsIvcount
         __result_ = T_Productions_vOut28 _lhsOadditionalDep _lhsOaroundDep _lhsOcProductions _lhsOcons _lhsOdirectDep _lhsOerrors _lhsOinstDep _lhsOmergeDep _lhsOnAutoRules _lhsOnExplicitRules _lhsOrules _lhsOvcount
         in __result_ )
     in C_Productions_s29 v28
   {-# INLINE rule428 #-}
   {-# LINE 628 "./src-ag/Order.ag" #-}
   rule428 = \  (_ :: ()) ->
                                {-# LINE 628 "./src-ag/Order.ag" #-}
                                []
                                {-# LINE 3106 "dist/build/Order.hs"#-}
   {-# INLINE rule429 #-}
   rule429 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule430 #-}
   rule430 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule431 #-}
   rule431 = \  (_ :: ()) ->
     []
   {-# INLINE rule432 #-}
   rule432 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule433 #-}
   rule433 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule434 #-}
   rule434 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule435 #-}
   rule435 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule436 #-}
   rule436 = \  (_ :: ()) ->
     0
   {-# INLINE rule437 #-}
   rule437 = \  (_ :: ()) ->
     0
   {-# INLINE rule438 #-}
   rule438 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule439 #-}
   rule439 = \ ((_lhsIvcount) :: Int) ->
     _lhsIvcount

-- Rule --------------------------------------------------------
-- wrapper
data Inh_Rule  = Inh_Rule { allTypeSigs_Inh_Rule :: (Map Identifier Type), allfields_Inh_Rule :: ([(Identifier,Type,ChildKind)]), allnts_Inh_Rule :: ([Identifier]), altAttrs_Inh_Rule :: (Map AltAttr Vertex), attrs_Inh_Rule :: ([(Identifier,Identifier)]), childInhs_Inh_Rule :: (Map Identifier Attributes), childNts_Inh_Rule :: (Map Identifier NontermIdent), con_Inh_Rule :: (Identifier), inh_Inh_Rule :: (Attributes), inhsOfChildren_Inh_Rule :: (Map Identifier Attributes), mergeMap_Inh_Rule :: (Map Identifier (Identifier,[Identifier])), nt_Inh_Rule :: (Identifier), o_case_Inh_Rule :: (Bool), o_cata_Inh_Rule :: (Bool), o_dovisit_Inh_Rule :: (Bool), o_newtypes_Inh_Rule :: (Bool), o_rename_Inh_Rule :: (Bool), o_sem_Inh_Rule :: (Bool), o_sig_Inh_Rule :: (Bool), o_wantvisit_Inh_Rule :: (Bool), prefix_Inh_Rule :: (String), syn_Inh_Rule :: (Attributes), synsOfChildren_Inh_Rule :: (Map Identifier Attributes) }
data Syn_Rule  = Syn_Rule { directDep_Syn_Rule :: (Seq Edge), errors_Syn_Rule :: (Seq Error), gathAltAttrs_Syn_Rule :: ([AltAttr]), gathRules_Syn_Rule :: (Seq CRule), instDep_Syn_Rule :: (Seq Edge), instVars_Syn_Rule :: ([Identifier]), locVars_Syn_Rule :: ([Identifier]), nAutoRules_Syn_Rule :: (Int), nExplicitRules_Syn_Rule :: (Int) }
{-# INLINABLE wrap_Rule #-}
wrap_Rule :: T_Rule  -> Inh_Rule  -> (Syn_Rule )
wrap_Rule (T_Rule act) (Inh_Rule _lhsIallTypeSigs _lhsIallfields _lhsIallnts _lhsIaltAttrs _lhsIattrs _lhsIchildInhs _lhsIchildNts _lhsIcon _lhsIinh _lhsIinhsOfChildren _lhsImergeMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_wantvisit _lhsIprefix _lhsIsyn _lhsIsynsOfChildren) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Rule_vIn31 _lhsIallTypeSigs _lhsIallfields _lhsIallnts _lhsIaltAttrs _lhsIattrs _lhsIchildInhs _lhsIchildNts _lhsIcon _lhsIinh _lhsIinhsOfChildren _lhsImergeMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_wantvisit _lhsIprefix _lhsIsyn _lhsIsynsOfChildren
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
data T_Rule_vIn31  = T_Rule_vIn31 (Map Identifier Type) ([(Identifier,Type,ChildKind)]) ([Identifier]) (Map AltAttr Vertex) ([(Identifier,Identifier)]) (Map Identifier Attributes) (Map Identifier NontermIdent) (Identifier) (Attributes) (Map Identifier Attributes) (Map Identifier (Identifier,[Identifier])) (Identifier) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (String) (Attributes) (Map Identifier Attributes)
data T_Rule_vOut31  = T_Rule_vOut31 (Seq Edge) (Seq Error) ([AltAttr]) (Seq CRule) (Seq Edge) ([Identifier]) ([Identifier]) (Int) (Int)
{-# NOINLINE sem_Rule_Rule #-}
sem_Rule_Rule :: (Maybe Identifier) -> T_Pattern  -> T_Expression  -> (Bool) -> (String) -> (Bool) -> (Bool) -> (Bool) -> (Maybe Error) -> (Bool) -> T_Rule 
sem_Rule_Rule arg_mbName_ arg_pattern_ arg_rhs_ arg_owrt_ arg_origin_ arg_explicit_ _ _ _ _ = T_Rule (return st32) where
   {-# NOINLINE st32 #-}
   st32 = let
      v31 :: T_Rule_v31 
      v31 = \ (T_Rule_vIn31 _lhsIallTypeSigs _lhsIallfields _lhsIallnts _lhsIaltAttrs _lhsIattrs _lhsIchildInhs _lhsIchildNts _lhsIcon _lhsIinh _lhsIinhsOfChildren _lhsImergeMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_wantvisit _lhsIprefix _lhsIsyn _lhsIsynsOfChildren) -> ( let
         _patternX20 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pattern_))
         _rhsX8 = Control.Monad.Identity.runIdentity (attach_T_Expression (arg_rhs_))
         (T_Pattern_vOut19 _patternIcopy _patternIerrors _patternIgathAltAttrs _patternIinstVars _patternIlocVars _patternIpatternAttrs) = inv_Pattern_s20 _patternX20 (T_Pattern_vIn19 _patternOallTypeSigs _patternOaltAttrs _patternOcon _patternOinh _patternOnt _patternOsyn)
         (T_Expression_vOut7 _rhsIallRhsVars _rhsIcopy _rhsIerrors _rhsItextLines _rhsIusedAttrs _rhsIusedFields _rhsIusedLocals) = inv_Expression_s8 _rhsX8 (T_Expression_vIn7 _rhsOallfields _rhsOallnts _rhsOattrs _rhsOcon _rhsOmergeMap _rhsOnt)
         _lhsOnExplicitRules :: Int
         _lhsOnExplicitRules = rule440 arg_explicit_
         _lhsOnAutoRules :: Int
         _lhsOnAutoRules = rule441 arg_origin_
         _defines = rule442 _lhsIallTypeSigs _lhsIaltAttrs _lhsIchildInhs _lhsIsyn _patternIpatternAttrs
         _gathRules = rule443 _defines _lhsIchildNts _lhsIcon _lhsInt _patternIcopy _rhsIallRhsVars _rhsItextLines arg_explicit_ arg_mbName_ arg_origin_ arg_owrt_
         _lhsOdirectDep :: Seq Edge
         _lhsOdirectDep = rule444 _defines _lhsIaltAttrs _rhsIusedAttrs _rhsIusedFields _rhsIusedLocals
         _instDep1 = rule445 _defines _lhsIaltAttrs _lhsIsynsOfChildren
         _instDep2 = rule446 _defines _lhsIaltAttrs _lhsIinhsOfChildren
         _lhsOinstDep :: Seq Edge
         _lhsOinstDep = rule447 _instDep1 _instDep2
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule448 _patternIerrors _rhsIerrors
         _lhsOgathAltAttrs :: [AltAttr]
         _lhsOgathAltAttrs = rule449 _patternIgathAltAttrs
         _lhsOgathRules :: Seq CRule
         _lhsOgathRules = rule450 _gathRules
         _lhsOinstVars :: [Identifier]
         _lhsOinstVars = rule451 _patternIinstVars
         _lhsOlocVars :: [Identifier]
         _lhsOlocVars = rule452 _patternIlocVars
         _patternOallTypeSigs = rule453 _lhsIallTypeSigs
         _patternOaltAttrs = rule454 _lhsIaltAttrs
         _patternOcon = rule455 _lhsIcon
         _patternOinh = rule456 _lhsIinh
         _patternOnt = rule457 _lhsInt
         _patternOsyn = rule458 _lhsIsyn
         _rhsOallfields = rule459 _lhsIallfields
         _rhsOallnts = rule460 _lhsIallnts
         _rhsOattrs = rule461 _lhsIattrs
         _rhsOcon = rule462 _lhsIcon
         _rhsOmergeMap = rule463 _lhsImergeMap
         _rhsOnt = rule464 _lhsInt
         __result_ = T_Rule_vOut31 _lhsOdirectDep _lhsOerrors _lhsOgathAltAttrs _lhsOgathRules _lhsOinstDep _lhsOinstVars _lhsOlocVars _lhsOnAutoRules _lhsOnExplicitRules
         in __result_ )
     in C_Rule_s32 v31
   {-# INLINE rule440 #-}
   {-# LINE 65 "./src-ag/Order.ag" #-}
   rule440 = \ explicit_ ->
                                 {-# LINE 65 "./src-ag/Order.ag" #-}
                                 if explicit_
                                 then 1
                                 else 0
                                 {-# LINE 3226 "dist/build/Order.hs"#-}
   {-# INLINE rule441 #-}
   {-# LINE 68 "./src-ag/Order.ag" #-}
   rule441 = \ origin_ ->
                             {-# LINE 68 "./src-ag/Order.ag" #-}
                             if startsWith "use rule" origin_ || startsWith "copy rule" origin_
                             then 1
                             else 0
                             {-# LINE 3234 "dist/build/Order.hs"#-}
   {-# INLINE rule442 #-}
   {-# LINE 218 "./src-ag/Order.ag" #-}
   rule442 = \ ((_lhsIallTypeSigs) :: Map Identifier Type) ((_lhsIaltAttrs) :: Map AltAttr Vertex) ((_lhsIchildInhs) :: Map Identifier Attributes) ((_lhsIsyn) :: Attributes) ((_patternIpatternAttrs) :: [(Identifier,Identifier,Bool)]) ->
                           {-# LINE 218 "./src-ag/Order.ag" #-}
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
                           {-# LINE 3251 "dist/build/Order.hs"#-}
   {-# INLINE rule443 #-}
   {-# LINE 232 "./src-ag/Order.ag" #-}
   rule443 = \ _defines ((_lhsIchildNts) :: Map Identifier NontermIdent) ((_lhsIcon) :: Identifier) ((_lhsInt) :: Identifier) ((_patternIcopy) :: Pattern) ((_rhsIallRhsVars) :: Set (Identifier,Identifier)) ((_rhsItextLines) :: [String]) explicit_ mbName_ origin_ owrt_ ->
                              {-# LINE 232 "./src-ag/Order.ag" #-}
                              let childnt field = Map.lookup field _lhsIchildNts
                              in Seq.fromList [ CRule attr False True _lhsInt _lhsIcon field (childnt field) tp _patternIcopy _rhsItextLines _defines owrt_ origin_ _rhsIallRhsVars explicit_ mbName_
                                              | (field,attr,tp) <- Map.elems _defines
                                              ]
                              {-# LINE 3260 "dist/build/Order.hs"#-}
   {-# INLINE rule444 #-}
   {-# LINE 271 "./src-ag/Order.ag" #-}
   rule444 = \ _defines ((_lhsIaltAttrs) :: Map AltAttr Vertex) ((_rhsIusedAttrs) :: [(Identifier,Identifier)]) ((_rhsIusedFields) :: [Identifier]) ((_rhsIusedLocals) :: [Identifier]) ->
                 {-# LINE 271 "./src-ag/Order.ag" #-}
                 let  defined = Map.keys _defines
                      used =  [ Map.lookup (AltAttr field attr True) _lhsIaltAttrs | (field,attr) <- _rhsIusedAttrs]
                              ++ [ Map.lookup (AltAttr _LOC attr True) _lhsIaltAttrs | attr <- _rhsIusedLocals ++ _rhsIusedFields ]
                 in Seq.fromList [ (x,y) | Just x <- used, y <- defined ]
                 {-# LINE 3269 "dist/build/Order.hs"#-}
   {-# INLINE rule445 #-}
   {-# LINE 315 "./src-ag/Order.ag" #-}
   rule445 = \ _defines ((_lhsIaltAttrs) :: Map AltAttr Vertex) ((_lhsIsynsOfChildren) :: Map Identifier Attributes) ->
            {-# LINE 315 "./src-ag/Order.ag" #-}
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
            {-# LINE 3284 "dist/build/Order.hs"#-}
   {-# INLINE rule446 #-}
   {-# LINE 326 "./src-ag/Order.ag" #-}
   rule446 = \ _defines ((_lhsIaltAttrs) :: Map AltAttr Vertex) ((_lhsIinhsOfChildren) :: Map Identifier Attributes) ->
            {-# LINE 326 "./src-ag/Order.ag" #-}
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
            {-# LINE 3299 "dist/build/Order.hs"#-}
   {-# INLINE rule447 #-}
   {-# LINE 336 "./src-ag/Order.ag" #-}
   rule447 = \ _instDep1 _instDep2 ->
                     {-# LINE 336 "./src-ag/Order.ag" #-}
                     _instDep1     Seq.>< _instDep2
                     {-# LINE 3305 "dist/build/Order.hs"#-}
   {-# INLINE rule448 #-}
   rule448 = \ ((_patternIerrors) :: Seq Error) ((_rhsIerrors) :: Seq Error) ->
     _patternIerrors Seq.>< _rhsIerrors
   {-# INLINE rule449 #-}
   rule449 = \ ((_patternIgathAltAttrs) :: [AltAttr]) ->
     _patternIgathAltAttrs
   {-# INLINE rule450 #-}
   rule450 = \ _gathRules ->
     _gathRules
   {-# INLINE rule451 #-}
   rule451 = \ ((_patternIinstVars) :: [Identifier]) ->
     _patternIinstVars
   {-# INLINE rule452 #-}
   rule452 = \ ((_patternIlocVars) :: [Identifier]) ->
     _patternIlocVars
   {-# INLINE rule453 #-}
   rule453 = \ ((_lhsIallTypeSigs) :: Map Identifier Type) ->
     _lhsIallTypeSigs
   {-# INLINE rule454 #-}
   rule454 = \ ((_lhsIaltAttrs) :: Map AltAttr Vertex) ->
     _lhsIaltAttrs
   {-# INLINE rule455 #-}
   rule455 = \ ((_lhsIcon) :: Identifier) ->
     _lhsIcon
   {-# INLINE rule456 #-}
   rule456 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule457 #-}
   rule457 = \ ((_lhsInt) :: Identifier) ->
     _lhsInt
   {-# INLINE rule458 #-}
   rule458 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
   {-# INLINE rule459 #-}
   rule459 = \ ((_lhsIallfields) :: [(Identifier,Type,ChildKind)]) ->
     _lhsIallfields
   {-# INLINE rule460 #-}
   rule460 = \ ((_lhsIallnts) :: [Identifier]) ->
     _lhsIallnts
   {-# INLINE rule461 #-}
   rule461 = \ ((_lhsIattrs) :: [(Identifier,Identifier)]) ->
     _lhsIattrs
   {-# INLINE rule462 #-}
   rule462 = \ ((_lhsIcon) :: Identifier) ->
     _lhsIcon
   {-# INLINE rule463 #-}
   rule463 = \ ((_lhsImergeMap) :: Map Identifier (Identifier,[Identifier])) ->
     _lhsImergeMap
   {-# INLINE rule464 #-}
   rule464 = \ ((_lhsInt) :: Identifier) ->
     _lhsInt

-- Rules -------------------------------------------------------
-- wrapper
data Inh_Rules  = Inh_Rules { allTypeSigs_Inh_Rules :: (Map Identifier Type), allfields_Inh_Rules :: ([(Identifier,Type,ChildKind)]), allnts_Inh_Rules :: ([Identifier]), altAttrs_Inh_Rules :: (Map AltAttr Vertex), attrs_Inh_Rules :: ([(Identifier,Identifier)]), childInhs_Inh_Rules :: (Map Identifier Attributes), childNts_Inh_Rules :: (Map Identifier NontermIdent), con_Inh_Rules :: (Identifier), inh_Inh_Rules :: (Attributes), inhsOfChildren_Inh_Rules :: (Map Identifier Attributes), mergeMap_Inh_Rules :: (Map Identifier (Identifier,[Identifier])), nt_Inh_Rules :: (Identifier), o_case_Inh_Rules :: (Bool), o_cata_Inh_Rules :: (Bool), o_dovisit_Inh_Rules :: (Bool), o_newtypes_Inh_Rules :: (Bool), o_rename_Inh_Rules :: (Bool), o_sem_Inh_Rules :: (Bool), o_sig_Inh_Rules :: (Bool), o_wantvisit_Inh_Rules :: (Bool), prefix_Inh_Rules :: (String), syn_Inh_Rules :: (Attributes), synsOfChildren_Inh_Rules :: (Map Identifier Attributes) }
data Syn_Rules  = Syn_Rules { directDep_Syn_Rules :: (Seq Edge), errors_Syn_Rules :: (Seq Error), gathAltAttrs_Syn_Rules :: ([AltAttr]), gathRules_Syn_Rules :: (Seq CRule), instDep_Syn_Rules :: (Seq Edge), instVars_Syn_Rules :: ([Identifier]), locVars_Syn_Rules :: ([Identifier]), nAutoRules_Syn_Rules :: (Int), nExplicitRules_Syn_Rules :: (Int) }
{-# INLINABLE wrap_Rules #-}
wrap_Rules :: T_Rules  -> Inh_Rules  -> (Syn_Rules )
wrap_Rules (T_Rules act) (Inh_Rules _lhsIallTypeSigs _lhsIallfields _lhsIallnts _lhsIaltAttrs _lhsIattrs _lhsIchildInhs _lhsIchildNts _lhsIcon _lhsIinh _lhsIinhsOfChildren _lhsImergeMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_wantvisit _lhsIprefix _lhsIsyn _lhsIsynsOfChildren) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Rules_vIn34 _lhsIallTypeSigs _lhsIallfields _lhsIallnts _lhsIaltAttrs _lhsIattrs _lhsIchildInhs _lhsIchildNts _lhsIcon _lhsIinh _lhsIinhsOfChildren _lhsImergeMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_wantvisit _lhsIprefix _lhsIsyn _lhsIsynsOfChildren
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
data T_Rules_vIn34  = T_Rules_vIn34 (Map Identifier Type) ([(Identifier,Type,ChildKind)]) ([Identifier]) (Map AltAttr Vertex) ([(Identifier,Identifier)]) (Map Identifier Attributes) (Map Identifier NontermIdent) (Identifier) (Attributes) (Map Identifier Attributes) (Map Identifier (Identifier,[Identifier])) (Identifier) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (String) (Attributes) (Map Identifier Attributes)
data T_Rules_vOut34  = T_Rules_vOut34 (Seq Edge) (Seq Error) ([AltAttr]) (Seq CRule) (Seq Edge) ([Identifier]) ([Identifier]) (Int) (Int)
{-# NOINLINE sem_Rules_Cons #-}
sem_Rules_Cons :: T_Rule  -> T_Rules  -> T_Rules 
sem_Rules_Cons arg_hd_ arg_tl_ = T_Rules (return st35) where
   {-# NOINLINE st35 #-}
   st35 = let
      v34 :: T_Rules_v34 
      v34 = \ (T_Rules_vIn34 _lhsIallTypeSigs _lhsIallfields _lhsIallnts _lhsIaltAttrs _lhsIattrs _lhsIchildInhs _lhsIchildNts _lhsIcon _lhsIinh _lhsIinhsOfChildren _lhsImergeMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_wantvisit _lhsIprefix _lhsIsyn _lhsIsynsOfChildren) -> ( let
         _hdX32 = Control.Monad.Identity.runIdentity (attach_T_Rule (arg_hd_))
         _tlX35 = Control.Monad.Identity.runIdentity (attach_T_Rules (arg_tl_))
         (T_Rule_vOut31 _hdIdirectDep _hdIerrors _hdIgathAltAttrs _hdIgathRules _hdIinstDep _hdIinstVars _hdIlocVars _hdInAutoRules _hdInExplicitRules) = inv_Rule_s32 _hdX32 (T_Rule_vIn31 _hdOallTypeSigs _hdOallfields _hdOallnts _hdOaltAttrs _hdOattrs _hdOchildInhs _hdOchildNts _hdOcon _hdOinh _hdOinhsOfChildren _hdOmergeMap _hdOnt _hdOo_case _hdOo_cata _hdOo_dovisit _hdOo_newtypes _hdOo_rename _hdOo_sem _hdOo_sig _hdOo_wantvisit _hdOprefix _hdOsyn _hdOsynsOfChildren)
         (T_Rules_vOut34 _tlIdirectDep _tlIerrors _tlIgathAltAttrs _tlIgathRules _tlIinstDep _tlIinstVars _tlIlocVars _tlInAutoRules _tlInExplicitRules) = inv_Rules_s35 _tlX35 (T_Rules_vIn34 _tlOallTypeSigs _tlOallfields _tlOallnts _tlOaltAttrs _tlOattrs _tlOchildInhs _tlOchildNts _tlOcon _tlOinh _tlOinhsOfChildren _tlOmergeMap _tlOnt _tlOo_case _tlOo_cata _tlOo_dovisit _tlOo_newtypes _tlOo_rename _tlOo_sem _tlOo_sig _tlOo_wantvisit _tlOprefix _tlOsyn _tlOsynsOfChildren)
         _lhsOdirectDep :: Seq Edge
         _lhsOdirectDep = rule465 _hdIdirectDep _tlIdirectDep
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule466 _hdIerrors _tlIerrors
         _lhsOgathAltAttrs :: [AltAttr]
         _lhsOgathAltAttrs = rule467 _hdIgathAltAttrs _tlIgathAltAttrs
         _lhsOgathRules :: Seq CRule
         _lhsOgathRules = rule468 _hdIgathRules _tlIgathRules
         _lhsOinstDep :: Seq Edge
         _lhsOinstDep = rule469 _hdIinstDep _tlIinstDep
         _lhsOinstVars :: [Identifier]
         _lhsOinstVars = rule470 _hdIinstVars _tlIinstVars
         _lhsOlocVars :: [Identifier]
         _lhsOlocVars = rule471 _hdIlocVars _tlIlocVars
         _lhsOnAutoRules :: Int
         _lhsOnAutoRules = rule472 _hdInAutoRules _tlInAutoRules
         _lhsOnExplicitRules :: Int
         _lhsOnExplicitRules = rule473 _hdInExplicitRules _tlInExplicitRules
         _hdOallTypeSigs = rule474 _lhsIallTypeSigs
         _hdOallfields = rule475 _lhsIallfields
         _hdOallnts = rule476 _lhsIallnts
         _hdOaltAttrs = rule477 _lhsIaltAttrs
         _hdOattrs = rule478 _lhsIattrs
         _hdOchildInhs = rule479 _lhsIchildInhs
         _hdOchildNts = rule480 _lhsIchildNts
         _hdOcon = rule481 _lhsIcon
         _hdOinh = rule482 _lhsIinh
         _hdOinhsOfChildren = rule483 _lhsIinhsOfChildren
         _hdOmergeMap = rule484 _lhsImergeMap
         _hdOnt = rule485 _lhsInt
         _hdOo_case = rule486 _lhsIo_case
         _hdOo_cata = rule487 _lhsIo_cata
         _hdOo_dovisit = rule488 _lhsIo_dovisit
         _hdOo_newtypes = rule489 _lhsIo_newtypes
         _hdOo_rename = rule490 _lhsIo_rename
         _hdOo_sem = rule491 _lhsIo_sem
         _hdOo_sig = rule492 _lhsIo_sig
         _hdOo_wantvisit = rule493 _lhsIo_wantvisit
         _hdOprefix = rule494 _lhsIprefix
         _hdOsyn = rule495 _lhsIsyn
         _hdOsynsOfChildren = rule496 _lhsIsynsOfChildren
         _tlOallTypeSigs = rule497 _lhsIallTypeSigs
         _tlOallfields = rule498 _lhsIallfields
         _tlOallnts = rule499 _lhsIallnts
         _tlOaltAttrs = rule500 _lhsIaltAttrs
         _tlOattrs = rule501 _lhsIattrs
         _tlOchildInhs = rule502 _lhsIchildInhs
         _tlOchildNts = rule503 _lhsIchildNts
         _tlOcon = rule504 _lhsIcon
         _tlOinh = rule505 _lhsIinh
         _tlOinhsOfChildren = rule506 _lhsIinhsOfChildren
         _tlOmergeMap = rule507 _lhsImergeMap
         _tlOnt = rule508 _lhsInt
         _tlOo_case = rule509 _lhsIo_case
         _tlOo_cata = rule510 _lhsIo_cata
         _tlOo_dovisit = rule511 _lhsIo_dovisit
         _tlOo_newtypes = rule512 _lhsIo_newtypes
         _tlOo_rename = rule513 _lhsIo_rename
         _tlOo_sem = rule514 _lhsIo_sem
         _tlOo_sig = rule515 _lhsIo_sig
         _tlOo_wantvisit = rule516 _lhsIo_wantvisit
         _tlOprefix = rule517 _lhsIprefix
         _tlOsyn = rule518 _lhsIsyn
         _tlOsynsOfChildren = rule519 _lhsIsynsOfChildren
         __result_ = T_Rules_vOut34 _lhsOdirectDep _lhsOerrors _lhsOgathAltAttrs _lhsOgathRules _lhsOinstDep _lhsOinstVars _lhsOlocVars _lhsOnAutoRules _lhsOnExplicitRules
         in __result_ )
     in C_Rules_s35 v34
   {-# INLINE rule465 #-}
   rule465 = \ ((_hdIdirectDep) :: Seq Edge) ((_tlIdirectDep) :: Seq Edge) ->
     _hdIdirectDep Seq.>< _tlIdirectDep
   {-# INLINE rule466 #-}
   rule466 = \ ((_hdIerrors) :: Seq Error) ((_tlIerrors) :: Seq Error) ->
     _hdIerrors Seq.>< _tlIerrors
   {-# INLINE rule467 #-}
   rule467 = \ ((_hdIgathAltAttrs) :: [AltAttr]) ((_tlIgathAltAttrs) :: [AltAttr]) ->
     _hdIgathAltAttrs ++ _tlIgathAltAttrs
   {-# INLINE rule468 #-}
   rule468 = \ ((_hdIgathRules) :: Seq CRule) ((_tlIgathRules) :: Seq CRule) ->
     _hdIgathRules Seq.>< _tlIgathRules
   {-# INLINE rule469 #-}
   rule469 = \ ((_hdIinstDep) :: Seq Edge) ((_tlIinstDep) :: Seq Edge) ->
     _hdIinstDep Seq.>< _tlIinstDep
   {-# INLINE rule470 #-}
   rule470 = \ ((_hdIinstVars) :: [Identifier]) ((_tlIinstVars) :: [Identifier]) ->
     _hdIinstVars ++ _tlIinstVars
   {-# INLINE rule471 #-}
   rule471 = \ ((_hdIlocVars) :: [Identifier]) ((_tlIlocVars) :: [Identifier]) ->
     _hdIlocVars ++ _tlIlocVars
   {-# INLINE rule472 #-}
   rule472 = \ ((_hdInAutoRules) :: Int) ((_tlInAutoRules) :: Int) ->
     _hdInAutoRules + _tlInAutoRules
   {-# INLINE rule473 #-}
   rule473 = \ ((_hdInExplicitRules) :: Int) ((_tlInExplicitRules) :: Int) ->
     _hdInExplicitRules + _tlInExplicitRules
   {-# INLINE rule474 #-}
   rule474 = \ ((_lhsIallTypeSigs) :: Map Identifier Type) ->
     _lhsIallTypeSigs
   {-# INLINE rule475 #-}
   rule475 = \ ((_lhsIallfields) :: [(Identifier,Type,ChildKind)]) ->
     _lhsIallfields
   {-# INLINE rule476 #-}
   rule476 = \ ((_lhsIallnts) :: [Identifier]) ->
     _lhsIallnts
   {-# INLINE rule477 #-}
   rule477 = \ ((_lhsIaltAttrs) :: Map AltAttr Vertex) ->
     _lhsIaltAttrs
   {-# INLINE rule478 #-}
   rule478 = \ ((_lhsIattrs) :: [(Identifier,Identifier)]) ->
     _lhsIattrs
   {-# INLINE rule479 #-}
   rule479 = \ ((_lhsIchildInhs) :: Map Identifier Attributes) ->
     _lhsIchildInhs
   {-# INLINE rule480 #-}
   rule480 = \ ((_lhsIchildNts) :: Map Identifier NontermIdent) ->
     _lhsIchildNts
   {-# INLINE rule481 #-}
   rule481 = \ ((_lhsIcon) :: Identifier) ->
     _lhsIcon
   {-# INLINE rule482 #-}
   rule482 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule483 #-}
   rule483 = \ ((_lhsIinhsOfChildren) :: Map Identifier Attributes) ->
     _lhsIinhsOfChildren
   {-# INLINE rule484 #-}
   rule484 = \ ((_lhsImergeMap) :: Map Identifier (Identifier,[Identifier])) ->
     _lhsImergeMap
   {-# INLINE rule485 #-}
   rule485 = \ ((_lhsInt) :: Identifier) ->
     _lhsInt
   {-# INLINE rule486 #-}
   rule486 = \ ((_lhsIo_case) :: Bool) ->
     _lhsIo_case
   {-# INLINE rule487 #-}
   rule487 = \ ((_lhsIo_cata) :: Bool) ->
     _lhsIo_cata
   {-# INLINE rule488 #-}
   rule488 = \ ((_lhsIo_dovisit) :: Bool) ->
     _lhsIo_dovisit
   {-# INLINE rule489 #-}
   rule489 = \ ((_lhsIo_newtypes) :: Bool) ->
     _lhsIo_newtypes
   {-# INLINE rule490 #-}
   rule490 = \ ((_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# INLINE rule491 #-}
   rule491 = \ ((_lhsIo_sem) :: Bool) ->
     _lhsIo_sem
   {-# INLINE rule492 #-}
   rule492 = \ ((_lhsIo_sig) :: Bool) ->
     _lhsIo_sig
   {-# INLINE rule493 #-}
   rule493 = \ ((_lhsIo_wantvisit) :: Bool) ->
     _lhsIo_wantvisit
   {-# INLINE rule494 #-}
   rule494 = \ ((_lhsIprefix) :: String) ->
     _lhsIprefix
   {-# INLINE rule495 #-}
   rule495 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
   {-# INLINE rule496 #-}
   rule496 = \ ((_lhsIsynsOfChildren) :: Map Identifier Attributes) ->
     _lhsIsynsOfChildren
   {-# INLINE rule497 #-}
   rule497 = \ ((_lhsIallTypeSigs) :: Map Identifier Type) ->
     _lhsIallTypeSigs
   {-# INLINE rule498 #-}
   rule498 = \ ((_lhsIallfields) :: [(Identifier,Type,ChildKind)]) ->
     _lhsIallfields
   {-# INLINE rule499 #-}
   rule499 = \ ((_lhsIallnts) :: [Identifier]) ->
     _lhsIallnts
   {-# INLINE rule500 #-}
   rule500 = \ ((_lhsIaltAttrs) :: Map AltAttr Vertex) ->
     _lhsIaltAttrs
   {-# INLINE rule501 #-}
   rule501 = \ ((_lhsIattrs) :: [(Identifier,Identifier)]) ->
     _lhsIattrs
   {-# INLINE rule502 #-}
   rule502 = \ ((_lhsIchildInhs) :: Map Identifier Attributes) ->
     _lhsIchildInhs
   {-# INLINE rule503 #-}
   rule503 = \ ((_lhsIchildNts) :: Map Identifier NontermIdent) ->
     _lhsIchildNts
   {-# INLINE rule504 #-}
   rule504 = \ ((_lhsIcon) :: Identifier) ->
     _lhsIcon
   {-# INLINE rule505 #-}
   rule505 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule506 #-}
   rule506 = \ ((_lhsIinhsOfChildren) :: Map Identifier Attributes) ->
     _lhsIinhsOfChildren
   {-# INLINE rule507 #-}
   rule507 = \ ((_lhsImergeMap) :: Map Identifier (Identifier,[Identifier])) ->
     _lhsImergeMap
   {-# INLINE rule508 #-}
   rule508 = \ ((_lhsInt) :: Identifier) ->
     _lhsInt
   {-# INLINE rule509 #-}
   rule509 = \ ((_lhsIo_case) :: Bool) ->
     _lhsIo_case
   {-# INLINE rule510 #-}
   rule510 = \ ((_lhsIo_cata) :: Bool) ->
     _lhsIo_cata
   {-# INLINE rule511 #-}
   rule511 = \ ((_lhsIo_dovisit) :: Bool) ->
     _lhsIo_dovisit
   {-# INLINE rule512 #-}
   rule512 = \ ((_lhsIo_newtypes) :: Bool) ->
     _lhsIo_newtypes
   {-# INLINE rule513 #-}
   rule513 = \ ((_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# INLINE rule514 #-}
   rule514 = \ ((_lhsIo_sem) :: Bool) ->
     _lhsIo_sem
   {-# INLINE rule515 #-}
   rule515 = \ ((_lhsIo_sig) :: Bool) ->
     _lhsIo_sig
   {-# INLINE rule516 #-}
   rule516 = \ ((_lhsIo_wantvisit) :: Bool) ->
     _lhsIo_wantvisit
   {-# INLINE rule517 #-}
   rule517 = \ ((_lhsIprefix) :: String) ->
     _lhsIprefix
   {-# INLINE rule518 #-}
   rule518 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
   {-# INLINE rule519 #-}
   rule519 = \ ((_lhsIsynsOfChildren) :: Map Identifier Attributes) ->
     _lhsIsynsOfChildren
{-# NOINLINE sem_Rules_Nil #-}
sem_Rules_Nil ::  T_Rules 
sem_Rules_Nil  = T_Rules (return st35) where
   {-# NOINLINE st35 #-}
   st35 = let
      v34 :: T_Rules_v34 
      v34 = \ (T_Rules_vIn34 _lhsIallTypeSigs _lhsIallfields _lhsIallnts _lhsIaltAttrs _lhsIattrs _lhsIchildInhs _lhsIchildNts _lhsIcon _lhsIinh _lhsIinhsOfChildren _lhsImergeMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_dovisit _lhsIo_newtypes _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_wantvisit _lhsIprefix _lhsIsyn _lhsIsynsOfChildren) -> ( let
         _lhsOdirectDep :: Seq Edge
         _lhsOdirectDep = rule520  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule521  ()
         _lhsOgathAltAttrs :: [AltAttr]
         _lhsOgathAltAttrs = rule522  ()
         _lhsOgathRules :: Seq CRule
         _lhsOgathRules = rule523  ()
         _lhsOinstDep :: Seq Edge
         _lhsOinstDep = rule524  ()
         _lhsOinstVars :: [Identifier]
         _lhsOinstVars = rule525  ()
         _lhsOlocVars :: [Identifier]
         _lhsOlocVars = rule526  ()
         _lhsOnAutoRules :: Int
         _lhsOnAutoRules = rule527  ()
         _lhsOnExplicitRules :: Int
         _lhsOnExplicitRules = rule528  ()
         __result_ = T_Rules_vOut34 _lhsOdirectDep _lhsOerrors _lhsOgathAltAttrs _lhsOgathRules _lhsOinstDep _lhsOinstVars _lhsOlocVars _lhsOnAutoRules _lhsOnExplicitRules
         in __result_ )
     in C_Rules_s35 v34
   {-# INLINE rule520 #-}
   rule520 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule521 #-}
   rule521 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule522 #-}
   rule522 = \  (_ :: ()) ->
     []
   {-# INLINE rule523 #-}
   rule523 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule524 #-}
   rule524 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule525 #-}
   rule525 = \  (_ :: ()) ->
     []
   {-# INLINE rule526 #-}
   rule526 = \  (_ :: ()) ->
     []
   {-# INLINE rule527 #-}
   rule527 = \  (_ :: ()) ->
     0
   {-# INLINE rule528 #-}
   rule528 = \  (_ :: ()) ->
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
         _lhsOtypeSigs = rule529 _lhsItypeSigs arg_name_ arg_tp_
         __result_ = T_TypeSig_vOut37 _lhsOtypeSigs
         in __result_ )
     in C_TypeSig_s38 v37
   {-# INLINE rule529 #-}
   {-# LINE 532 "./src-ag/Order.ag" #-}
   rule529 = \ ((_lhsItypeSigs) :: Map Identifier Type) name_ tp_ ->
                             {-# LINE 532 "./src-ag/Order.ag" #-}
                             Map.insert name_ tp_ _lhsItypeSigs
                             {-# LINE 3734 "dist/build/Order.hs"#-}

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
         _lhsOtypeSigs = rule530 _tlItypeSigs
         _hdOtypeSigs = rule531 _lhsItypeSigs
         _tlOtypeSigs = rule532 _hdItypeSigs
         __result_ = T_TypeSigs_vOut40 _lhsOtypeSigs
         in __result_ )
     in C_TypeSigs_s41 v40
   {-# INLINE rule530 #-}
   rule530 = \ ((_tlItypeSigs) :: Map Identifier Type) ->
     _tlItypeSigs
   {-# INLINE rule531 #-}
   rule531 = \ ((_lhsItypeSigs) :: Map Identifier Type) ->
     _lhsItypeSigs
   {-# INLINE rule532 #-}
   rule532 = \ ((_hdItypeSigs) :: Map Identifier Type) ->
     _hdItypeSigs
{-# NOINLINE sem_TypeSigs_Nil #-}
sem_TypeSigs_Nil ::  T_TypeSigs 
sem_TypeSigs_Nil  = T_TypeSigs (return st41) where
   {-# NOINLINE st41 #-}
   st41 = let
      v40 :: T_TypeSigs_v40 
      v40 = \ (T_TypeSigs_vIn40 _lhsItypeSigs) -> ( let
         _lhsOtypeSigs :: Map Identifier Type
         _lhsOtypeSigs = rule533 _lhsItypeSigs
         __result_ = T_TypeSigs_vOut40 _lhsOtypeSigs
         in __result_ )
     in C_TypeSigs_s41 v40
   {-# INLINE rule533 #-}
   rule533 = \ ((_lhsItypeSigs) :: Map Identifier Type) ->
     _lhsItypeSigs
