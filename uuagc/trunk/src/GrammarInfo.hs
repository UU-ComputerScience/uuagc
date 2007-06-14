module GrammarInfo where

import SequentialTypes
import CodeSyntax
import UU.DData.Map(Map)
import qualified UU.DData.Map as Map
import UU.DData.Set(Set)
import qualified UU.DData.Set as Set
import CommonTypes
import Data.List(intersect,(\\))

type LMH = (Vertex,Vertex,Vertex)
data Info = Info  {  tdpToTds    ::  Table Vertex
                  ,  tdsToTdp    ::  Table [Vertex]
                  ,  attrTable   ::  Table NTAttr
                  ,  ruleTable   ::  Table CRule
                  ,  lmh         ::  [LMH]
                  ,  prods       ::  [(Nonterminal,[Constructor])]
                  ,  wraps       ::  Set Nonterminal
                  }
                  deriving Show

instance Show CRule
 where show (CRule name isIn hasCode nt con field childnt tp pattern rhs defines owrt origin uses) 
         = "CRule " ++ show name ++ " nt: " ++ show nt ++ " con: " ++ show con ++ " field: " ++ show field
         ++ " childnt: " ++ show childnt ++ " rhs: " ++ concat rhs ++ " uses: " ++ show [ attrname True fld nm | (fld,nm) <- Set.toList uses ]

type CInterfaceMap = Map Nonterminal CInterface
type CVisitsMap = Map Nonterminal (Map Constructor CVisits)

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
         toString (a,t) = (getName a, case t of (NT nt) -> getName nt; Haskell t -> t)
         chnn     = inh' `intersect` syn'
         inhn     = inh' \\ chnn
         synn     = syn' \\ chnn
         disp name [] = []
         disp name as =  (name ++ if length as == 1 then " attribute:" else " attributes:") :
                         map (\(x,y) -> ind x ++ replicate ((20 - length x) `max` 0) ' ' ++ " : " ++ y) as
     in  disp "inherited" inhn 
         ++ disp "chained" chnn
         ++ disp "synthesized" synn

