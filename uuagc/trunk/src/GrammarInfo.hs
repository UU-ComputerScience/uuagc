module GrammarInfo where

import SequentialTypes
import CodeSyntax
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
import CommonTypes
import Data.List(intersect,(\\))
import Options

type LMH = (Vertex,Vertex,Vertex)
data Info = Info  {  tdpToTds    ::  Table Vertex
                  ,  tdsToTdp    ::  Table [Vertex]
                  ,  attrTable   ::  Table NTAttr
                  ,  ruleTable   ::  Table CRule
                  ,  lmh         ::  [LMH]
                  ,  nonts       ::  [(NontermIdent,[ConstructorIdent])]
                  ,  wraps       ::  Set NontermIdent
                  }
                  deriving Show

instance Show CRule
 where show (CRule name _ _ nt con field childnt _ _ rhs _ _ _ uses _ _)
         = "CRule " ++ show name ++ " nt: " ++ show nt ++ " con: " ++ show con ++ " field: " ++ show field
         ++ " childnt: " ++ show childnt ++ " rhs: " ++ concat rhs ++ " uses: " ++ show [ attrname noOptions True fld nm | (fld,nm) <- Set.toList uses ]
       show _ = error "Only CRule is supported"

type CInterfaceMap = Map NontermIdent CInterface
type CVisitsMap = Map NontermIdent (Map ConstructorIdent CVisits)

data CycleStatus
  = CycleFree     CInterfaceMap CVisitsMap
  | LocalCycle    [Route]
  | InstCycle     [Route]
  | DirectCycle   [EdgeRoutes]
  | InducedCycle  CInterfaceMap [EdgeRoutes]

showsSegment :: CSegment -> [String]
showsSegment (CSegment inh syn)
   = let syn'     = map toString (Map.toList syn)
         inh'     = map toString (Map.toList inh)
         toString (a,t) = (getName a, case t of (NT nt tps _) -> getName nt ++ " " ++ unwords tps; Haskell t' -> t'; Self -> error "Self type not supported.")
         chnn     = inh' `intersect` syn'
         inhn     = inh' \\ chnn
         synn     = syn' \\ chnn
         disp _ [] = []
         disp name as =  (name ++ if length as == 1 then " attribute:" else " attributes:") :
                         map (\(x,y) -> ind x ++ replicate ((20 - length x) `max` 0) ' ' ++ " : " ++ y) as
     in  disp "inherited" inhn
         ++ disp "chained" chnn
         ++ disp "synthesized" synn

