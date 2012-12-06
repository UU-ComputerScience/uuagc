module SequentialTypes where

import CodeSyntax
import CommonTypes
import Data.Array(Array)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe(fromJust)
import Data.List(partition)

type Vertex    = Int
data PathStep  = AttrStep Vertex Vertex
               | AtOcStep Vertex Vertex
               | AttrIndu Vertex Vertex
               deriving (Show, Eq)
               
type Path      = [PathStep]
type Route     = [Vertex]
            
type Edge      = (Int,Int)
type EdgePath  = (Edge,Path)
type EdgePaths = (Edge,Path,Path)
type EdgeRoute = (Edge,Route)
type EdgeRoutes= (Edge,Route,Route)

type Table a   = Array     Vertex a


data ChildVisit = ChildVisit Identifier Identifier Int [Vertex] [Vertex] deriving (Eq,Show) -- field, rhs nt, visit nr., inh, syn
data NTAttr = NTAInh NontermIdent Identifier Type -- nt, attribute, type
            | NTASyn NontermIdent Identifier Type -- nt, attribute, type
               deriving Show

getNtaNameType :: NTAttr -> (Identifier, Type)
getNtaNameType (NTAInh _ name tp) = (name,tp)
getNtaNameType (NTASyn _ name tp) = (name,tp)

getAttr :: CRule -> Identifier
getAttr     (CRule name _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = name
getAttr     _ = error "Only defined for CRule"
getIsIn :: CRule -> Bool
getIsIn     (CRule _ ii _ _ _ _ _ _ _ _ _ _ _ _ _ _) = ii
getIsIn     _ = error "Only defined for CRule"
getHasCode :: CRule -> Bool
getHasCode  (CRule _ _ hc _ _ _ _ _ _ _ _ _ _ _ _ _) = hc
getHasCode  _ = error "Only defined for CRule"
getLhsNt :: CRule -> NontermIdent
getLhsNt    (CRule _ _ _ nt _ _ _ _ _ _ _ _ _ _ _ _) = nt
getLhsNt    _ = error "Only defined for CRule"
getCon :: CRule -> ConstructorIdent
getCon      (CRule _ _ _ _ con _ _ _ _ _ _ _ _ _ _ _) = con
getCon      _ = error "Only defined for CRule"
getField :: CRule -> Identifier
getField    (CRule _ _ _ _ _ field _ _ _ _ _ _ _ _ _ _) = field
getField    _ = error "Only defined for CRule"
getRhsNt :: CRule -> Maybe NontermIdent
getRhsNt    (CRule _ _ _ _ _ _ childnt _ _ _ _ _ _ _ _ _) = childnt
getRhsNt    _ = error "Only defined for CRule"
getType :: CRule -> Maybe Type
getType     (CRule _ _ _ _ _ _ _ tp _ _ _ _ _ _ _ _) = tp
getType     _ = error "Only defined for CRule"
getDefines :: CRule -> Map Int (Identifier, Identifier, Maybe Type)
getDefines  (CRule _ _ _ _ _ _ _ _ _ _ defines _ _ _ _ _) = defines
getDefines  _ = error "Only defined for CRule"
getUses :: CRule -> Set (Identifier, Identifier)
getUses     (CRule _ _ _ _ _ _ _ _ _ _ _ _ _ uses _ _) = uses
getUses     _ = error "Only defined for CRule"
getExplicit :: CRule -> Bool
getExplicit (CRule _ _ _ _ _ _ _ _ _ _ _ _ _ _ expl _) = expl
getExplicit _ = error "Only defined for CRule"

isLocal, isInst, isLhs, isRhs, isSyn, isInh, hasCode :: CRule -> Bool
isLocal = (_LOC==) . getField
isInst = (_INST==) . getField
isLhs = (_LHS==) . getField
isRhs cr = not (isLhs cr || isLocal cr)
isSyn cr | isLocal cr  = False
         | getIsIn cr  = isRhs cr
         | otherwise   = isLhs cr
isInh = not . isSyn
hasCode cr = isLocal cr || (isLhs cr && isInh cr) || (isRhs cr && isSyn cr)

isEqualField, isDifferentField, isEqualCon, isRhsOfSameCon :: CRule -> CRule -> Bool
isEqualField      a b = isEqualCon a b && getField a == getField b
isDifferentField  a b = isEqualCon a b && getField a /= getField b 
isEqualCon        a b = getLhsNt a == getLhsNt b && getCon a == getCon b
isRhsOfSameCon    a b = isEqualCon a b && isRhs a && isRhs b

isSynAttr, isInhAttr :: NTAttr -> Bool
isSynAttr (NTAInh _ _ _) = False
isSynAttr (NTASyn _ _ _) = True
isInhAttr = not . isSynAttr

ntattr :: CRule -> Maybe NTAttr
ntattr cr  | isLocal cr =  Nothing
           | isInst  cr =  Nothing -- an inst definition is just considered as a local attribute definition
           | otherwise  =  let  at = if isSyn cr then NTASyn else NTAInh
                                getNt cr' = if isRhs cr' then fromJust (getRhsNt cr') else getLhsNt cr'
                           in Just (at (getNt cr) (getAttr cr) (fromJust (getType cr)))

cRuleLhsInh :: Identifier -> NontermIdent -> ConstructorIdent -> Type -> CRule
cRuleLhsInh attr nt con tp = CRule attr True False nt con _LHS Nothing (Just tp) (error "cRuleLhsInh") [] Map.empty False "" Set.empty False Nothing
cRuleTerminal :: Identifier -> NontermIdent -> ConstructorIdent -> Type -> CRule
cRuleTerminal attr nt con tp = CRule attr True False nt con _LOC Nothing (Just tp) (error ("cRuleTerminal: " ++ show (attr, nt, con, tp))) [] Map.empty False "" Set.empty False Nothing
cRuleRhsSyn :: Identifier -> NontermIdent -> ConstructorIdent -> Type -> Identifier -> NontermIdent -> CRule
cRuleRhsSyn attr nt con tp field childnt = CRule attr True False nt con field (Just childnt) (Just tp) (error ("cRuleRhsSyn: " ++ show (attr, nt, con, tp, field))) [] Map.empty False "" Set.empty False Nothing

defaultRule :: Identifier -> NontermIdent -> ConstructorIdent -> Identifier -> CRule
defaultRule attr nt con field =  CRule attr (er 1) (er 2) nt con field (er 3) (er 4) (er 5) (er 6) (er 7) (er 8) (er 9) (er 10) False Nothing
                                 where er :: Int -> a
                                       er i = error ("Default rule has no code " ++ show i)

instance Eq CRule where
  a == b = getAttr a == getAttr b && isEqualField a b
instance Ord CRule where
  compare a b =  compare (getLhsNt a) (getLhsNt b) 
                 >/< compare (getCon a) (getCon b)
                 >/< compare (getField a) (getField b)
                 >/< compare (getAttr a) (getAttr b)
instance Eq NTAttr where
  (NTAInh _ _ _) == (NTASyn _ _ _) = False
  (NTASyn _ _ _) == (NTAInh _ _ _) = False
  (NTAInh nt name _) == (NTAInh nt' name' _) = nt == nt' && name == name'
  (NTASyn nt name _) == (NTASyn nt' name' _) = nt == nt' && name == name'
instance Ord NTAttr where
  compare (NTAInh _ _ _) (NTASyn _ _ _) = LT
  compare (NTASyn _ _ _) (NTAInh _ _ _) = GT
  compare (NTAInh nt name _) (NTAInh nt' name' _) = compare nt nt' >/< compare name name'
  compare (NTASyn nt name _) (NTASyn nt' name' _) = compare nt nt' >/< compare name name'

eqCRuleDefines :: CRule -> CRule -> Bool
eqCRuleDefines a b
  = Map.keys (getDefines a) == Map.keys (getDefines b)

(>/<) :: Ordering -> Ordering -> Ordering
EQ >/< b = b
a >/< _ = a


eqClasses :: (a -> a -> Bool) -> [a] -> [[a]]
eqClasses _ [] = []
eqClasses p (a:as) = let (isA,rest) = partition (p a) as
                     in (a:isA):eqClasses p rest

lhsshow :: NTAttr -> String
lhsshow (NTAInh _ attr _) = lhsname True attr
lhsshow (NTASyn _ attr _) = lhsname False attr 

rhsshow :: Identifier -> NTAttr -> String
rhsshow field (NTAInh _ attr _) = attrname False field attr
rhsshow field (NTASyn _ attr _) = attrname True field attr 

prettyCRule :: CRule -> String
prettyCRule cr 
   =  let descr | isLocal cr = "local attribute " ++ show (getAttr cr)
                | otherwise =     (if isSyn cr then "synthesized " else "inherited ")
                               ++ "attribute "
                               ++ (if isRhs cr then show (getField cr) ++ "." else "")
                               ++ (if isLhs cr then "lhs." else "")
                               ++ (show (getAttr cr))
      in show (getLhsNt cr) ++ "." ++ show (getCon cr) ++ ", " ++ descr

