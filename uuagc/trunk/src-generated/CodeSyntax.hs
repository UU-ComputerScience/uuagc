

-- UUAGC 0.9.42.3 (src-ag/CodeSyntax.ag)
module CodeSyntax where
{-# LINE 2 "./src-ag/CodeSyntax.ag" #-}

import Patterns
import CommonTypes
import Data.Map(Map)
import Data.Set(Set)
{-# LINE 12 "dist/build/CodeSyntax.hs" #-}
-- CGrammar ----------------------------------------------------
{-
   alternatives:
      alternative CGrammar:
         child typeSyns       : {TypeSyns}
         child derivings      : {Derivings}
         child wrappers       : {Set NontermIdent}
         child nonts          : CNonterminals 
         child pragmas        : {PragmaMap}
         child paramMap       : {ParamMap}
         child contextMap     : {ContextMap}
         child quantMap       : {QuantMap}
         child aroundsMap     : {Map NontermIdent (Map ConstructorIdent (Set Identifier))}
         child mergeMap       : {Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier])))}
         child multivisit     : {Bool}
-}
data CGrammar = CGrammar (TypeSyns) (Derivings) ((Set NontermIdent)) (CNonterminals) (PragmaMap) (ParamMap) (ContextMap) (QuantMap) ((Map NontermIdent (Map ConstructorIdent (Set Identifier)))) ((Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier]))))) (Bool)
-- CInterface --------------------------------------------------
{-
   alternatives:
      alternative CInterface:
         child seg            : CSegments 
-}
data CInterface = CInterface (CSegments)
-- CNonterminal ------------------------------------------------
{-
   alternatives:
      alternative CNonterminal:
         child nt             : {NontermIdent}
         child params         : {[Identifier]}
         child inh            : {Attributes}
         child syn            : {Attributes}
         child prods          : CProductions 
         child inter          : CInterface 
-}
data CNonterminal = CNonterminal (NontermIdent) (([Identifier])) (Attributes) (Attributes) (CProductions) (CInterface)
-- CNonterminals -----------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : CNonterminal 
         child tl             : CNonterminals 
      alternative Nil:
-}
type CNonterminals = [CNonterminal]
-- CProduction -------------------------------------------------
{-
   alternatives:
      alternative CProduction:
         child con            : {ConstructorIdent}
         child visits         : CVisits 
         child children       : {[(Identifier,Type,ChildKind)]}
         child terminals      : {[Identifier]}
-}
data CProduction = CProduction (ConstructorIdent) (CVisits) (([(Identifier,Type,ChildKind)])) (([Identifier]))
-- CProductions ------------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : CProduction 
         child tl             : CProductions 
      alternative Nil:
-}
type CProductions = [CProduction]
-- CRule -------------------------------------------------------
{-
   alternatives:
      alternative CRule:
         child name           : {Identifier}
         child isIn           : {Bool}
         child hasCode        : {Bool}
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
         child field          : {Identifier}
         child childnt        : {Maybe NontermIdent}
         child tp             : {Maybe Type}
         child pattern        : {Pattern}
         child rhs            : {[String]}
         child defines        : {Map Int (Identifier,Identifier,Maybe Type)}
         child owrt           : {Bool}
         child origin         : {String}
         child uses           : {Set (Identifier, Identifier)}
         child explicit       : {Bool}
         child mbNamed        : {Maybe Identifier}
      alternative CChildVisit:
         child name           : {Identifier}
         child nt             : {NontermIdent}
         child nr             : {Int}
         child inh            : {Attributes}
         child syn            : {Attributes}
         child isLast         : {Bool}
-}
data CRule = CRule (Identifier) (Bool) (Bool) (NontermIdent) (ConstructorIdent) (Identifier) ((Maybe NontermIdent)) ((Maybe Type)) (Pattern) (([String])) ((Map Int (Identifier,Identifier,Maybe Type))) (Bool) (String) ((Set (Identifier, Identifier))) (Bool) ((Maybe Identifier))
           | CChildVisit (Identifier) (NontermIdent) (Int) (Attributes) (Attributes) (Bool)
-- CSegment ----------------------------------------------------
{-
   alternatives:
      alternative CSegment:
         child inh            : {Attributes}
         child syn            : {Attributes}
-}
data CSegment = CSegment (Attributes) (Attributes)
-- CSegments ---------------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : CSegment 
         child tl             : CSegments 
      alternative Nil:
-}
type CSegments = [CSegment]
-- CVisit ------------------------------------------------------
{-
   alternatives:
      alternative CVisit:
         child inh            : {Attributes}
         child syn            : {Attributes}
         child vss            : Sequence 
         child intra          : Sequence 
         child ordered        : {Bool}
-}
data CVisit = CVisit (Attributes) (Attributes) (Sequence) (Sequence) (Bool)
-- CVisits -----------------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : CVisit 
         child tl             : CVisits 
      alternative Nil:
-}
type CVisits = [CVisit]
-- Sequence ----------------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : CRule 
         child tl             : Sequence 
      alternative Nil:
-}
type Sequence = [CRule]