module GrammarInfo where

import SequentialTypes
import Data.Graph (Table,Vertex,Edge)
import CodeSyntax
import UU.DData.Map(Map,toList)
import UU.DData.Set(Set)
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
                  ,  cyclesOnly  ::  Bool
                  }

type CInterfaceMap = Map Nonterminal CInterface
type CVisitsMap = Map Nonterminal (Map Constructor CVisits)

data SeqResult  = SeqResult     CInterfaceMap CVisitsMap
                | LocLocCycle   [(Vertex,[Vertex])]
                | DirectCycle   [(Edge,[Vertex])]
                | InducedCycle  CInterfaceMap [Edge] 

showsSegment :: CSegment -> [String]
showsSegment (CSegment inh syn)
   = let syn'     = map toString (toList syn)
         inh'     = map toString (toList inh)
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

