

-- UUAGC 0.9.42.3 (src-ag/ExecutionPlan.ag)
module ExecutionPlan where
{-# LINE 2 "./src-ag/ExecutionPlan.ag" #-}

-- VisitSyntax.ag imports
import Patterns    (Pattern(..),Patterns)
import Expression  (Expression(..))
import CommonTypes
import ErrorMessages

import qualified Data.Set as Set
import Data.Set(Set)
import qualified Data.Map as Map
import Data.Map(Map)
{-# LINE 18 "dist/build/ExecutionPlan.hs" #-}
-- EChild ------------------------------------------------------
{-
   alternatives:
      alternative EChild:
         child name           : {Identifier}
         child tp             : {Type}
         child kind           : {ChildKind}
         child hasAround      : {Bool}
         child merges         : {Maybe [Identifier]}
         child isMerged       : {Bool}
      alternative ETerm:
         child name           : {Identifier}
         child tp             : {Type}
-}
data EChild = EChild (Identifier) (Type) (ChildKind) (Bool) ((Maybe [Identifier])) (Bool)
            | ETerm (Identifier) (Type)
-- EChildren ---------------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : EChild 
         child tl             : EChildren 
      alternative Nil:
-}
type EChildren = [EChild]
-- ENonterminal ------------------------------------------------
{-
   alternatives:
      alternative ENonterminal:
         child nt             : {NontermIdent}
         child params         : {[Identifier]}
         child classCtxs      : {ClassContext}
         child initial        : {StateIdentifier}
         child initialv       : {Maybe VisitIdentifier}
         child nextVisits     : {Map StateIdentifier StateCtx}
         child prevVisits     : {Map StateIdentifier StateCtx}
         child prods          : EProductions 
         child recursive      : {Bool}
         child hoInfo         : {HigherOrderInfo}
-}
data ENonterminal = ENonterminal (NontermIdent) (([Identifier])) (ClassContext) (StateIdentifier) ((Maybe VisitIdentifier)) ((Map StateIdentifier StateCtx)) ((Map StateIdentifier StateCtx)) (EProductions) (Bool) (HigherOrderInfo)
-- ENonterminals -----------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : ENonterminal 
         child tl             : ENonterminals 
      alternative Nil:
-}
type ENonterminals = [ENonterminal]
-- EProduction -------------------------------------------------
{-
   alternatives:
      alternative EProduction:
         child con            : {ConstructorIdent}
         child params         : {[Identifier]}
         child constraints    : {[Type]}
         child rules          : ERules 
         child children       : EChildren 
         child visits         : Visits 
-}
data EProduction = EProduction (ConstructorIdent) (([Identifier])) (([Type])) (ERules) (EChildren) (Visits)
-- EProductions ------------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : EProduction 
         child tl             : EProductions 
      alternative Nil:
-}
type EProductions = [EProduction]
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
         child pure           : {Bool}
         child mbError        : {Maybe Error}
-}
data ERule = ERule (Identifier) (Pattern) (Expression) (Bool) (String) (Bool) (Bool) ((Maybe Error))
-- ERules ------------------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : ERule 
         child tl             : ERules 
      alternative Nil:
-}
type ERules = [ERule]
-- ExecutionPlan -----------------------------------------------
{-
   alternatives:
      alternative ExecutionPlan:
         child nonts          : ENonterminals 
         child typeSyns       : {TypeSyns}
         child wrappers       : {Set NontermIdent}
         child derivings      : {Derivings}
-}
data ExecutionPlan = ExecutionPlan (ENonterminals) (TypeSyns) ((Set NontermIdent)) (Derivings)
-- Visit -------------------------------------------------------
{-
   alternatives:
      alternative Visit:
         child ident          : {VisitIdentifier}
         child from           : {StateIdentifier}
         child to             : {StateIdentifier}
         child inh            : {Set Identifier}
         child syn            : {Set Identifier}
         child steps          : VisitSteps 
         child kind           : {VisitKind}
-}
data Visit = Visit (VisitIdentifier) (StateIdentifier) (StateIdentifier) ((Set Identifier)) ((Set Identifier)) (VisitSteps) (VisitKind)
-- VisitStep ---------------------------------------------------
{-
   alternatives:
      alternative Sem:
         child name           : {Identifier}
      alternative ChildVisit:
         child child          : {Identifier}
         child nonterm        : {NontermIdent}
         child visit          : {VisitIdentifier}
      alternative PureGroup:
         child steps          : VisitSteps 
         child ordered        : {Bool}
      alternative Sim:
         child steps          : VisitSteps 
      alternative ChildIntro:
         child child          : {Identifier}
-}
data VisitStep = Sem (Identifier)
               | ChildVisit (Identifier) (NontermIdent) (VisitIdentifier)
               | PureGroup (VisitSteps) (Bool)
               | Sim (VisitSteps)
               | ChildIntro (Identifier)
-- VisitSteps --------------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : VisitStep 
         child tl             : VisitSteps 
      alternative Nil:
-}
type VisitSteps = [VisitStep]
-- Visits ------------------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : Visit 
         child tl             : Visits 
      alternative Nil:
-}
type Visits = [Visit]