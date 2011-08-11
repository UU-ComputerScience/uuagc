

-- UUAGC 0.9.38.6.5 (src-ag/ExecutionPlan.ag)
module ExecutionPlan where
{-# LINE 2 "src-ag/ExecutionPlan.ag" #-}

-- VisitSyntax.ag imports
import Patterns    (Pattern(..),Patterns)
import Expression  (Expression(..))
import CommonTypes

import qualified Data.Set as Set
{-# LINE 14 "dist/build/uuagc/uuagc-tmp/ExecutionPlan.hs" #-}
-- EChild ------------------------------------------------------
{-
   alternatives:
      alternative EChild:
         child name           : {Identifier}
         child tp             : {Type}
         child virtual        : {Maybe (Maybe Type)}
-}
data EChild  = EChild (Identifier) (Type) ((Maybe (Maybe Type))) 
-- EChildren ---------------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : EChild 
         child tl             : EChildren 
      alternative Nil:
-}
type EChildren  = [EChild ]
-- ENonterminal ------------------------------------------------
{-
   alternatives:
      alternative ENonterminal:
         child nt             : {NontermIdent}
         child params         : {[Identifier]}
         child initial        : {StateIdentifier}
         child initialv       : {Maybe VisitIdentifier}
         child prods          : EProductions 
-}
data ENonterminal  = ENonterminal (NontermIdent) (([Identifier])) (StateIdentifier) ((Maybe VisitIdentifier)) (EProductions ) 
-- ENonterminals -----------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : ENonterminal 
         child tl             : ENonterminals 
      alternative Nil:
-}
type ENonterminals  = [ENonterminal ]
-- EProduction -------------------------------------------------
{-
   alternatives:
      alternative EProduction:
         child con            : {ConstructorIdent}
         child rules          : ERules 
         child children       : EChildren 
         child visits         : Visits 
-}
data EProduction  = EProduction (ConstructorIdent) (ERules ) (EChildren ) (Visits ) 
-- EProductions ------------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : EProduction 
         child tl             : EProductions 
      alternative Nil:
-}
type EProductions  = [EProduction ]
-- ERule -------------------------------------------------------
{-
   alternatives:
      alternative ERule:
         child name           : {Identifier}
         child pattern        : {Pattern}
         child rhs            : {Expression}
         child owrt           : {Bool}
         child origin         : {String}
         child explicit       : {Bool}
-}
data ERule  = ERule (Identifier) (Pattern) (Expression) (Bool) (String) (Bool) 
-- ERules ------------------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : ERule 
         child tl             : ERules 
      alternative Nil:
-}
type ERules  = [ERule ]
-- ExecutionPlan -----------------------------------------------
{-
   alternatives:
      alternative ExecutionPlan:
         child nonts          : ENonterminals 
         child typeSyns       : {TypeSyns}
         child wrappers       : {Set.Set NontermIdent}
         child derivings      : {Derivings}
-}
data ExecutionPlan  = ExecutionPlan (ENonterminals ) (TypeSyns) ((Set.Set NontermIdent)) (Derivings) 
-- Visit -------------------------------------------------------
{-
   alternatives:
      alternative Visit:
         child ident          : {VisitIdentifier}
         child from           : {StateIdentifier}
         child to             : {StateIdentifier}
         child inh            : {Set.Set Identifier}
         child syn            : {Set.Set Identifier}
         child steps          : VisitSteps 
-}
data Visit  = Visit (VisitIdentifier) (StateIdentifier) (StateIdentifier) ((Set.Set Identifier)) ((Set.Set Identifier)) (VisitSteps ) 
-- VisitStep ---------------------------------------------------
{-
   alternatives:
      alternative ChildIntro:
         child child          : {Identifier}
      alternative ChildVisit:
         child child          : {Identifier}
         child nonterm        : {NontermIdent}
         child visit          : {VisitIdentifier}
      alternative Sem:
         child name           : {Identifier}
      alternative Sim:
         child steps          : VisitSteps 
-}
data VisitStep  = ChildIntro (Identifier) 
                | ChildVisit (Identifier) (NontermIdent) (VisitIdentifier) 
                | Sem (Identifier) 
                | Sim (VisitSteps ) 
-- VisitSteps --------------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : VisitStep 
         child tl             : VisitSteps 
      alternative Nil:
-}
type VisitSteps  = [VisitStep ]
-- Visits ------------------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : Visit 
         child tl             : Visits 
      alternative Nil:
-}
type Visits  = [Visit ]