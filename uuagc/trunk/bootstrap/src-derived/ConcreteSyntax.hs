

-- UUAGC 0.9.40.1 (src-ag/ConcreteSyntax.ag)
module ConcreteSyntax where
{-# LINE 2 "src-ag/ConcreteSyntax.ag" #-}

import UU.Scanner.Position (Pos)
import Patterns   (Pattern)
import Expression (Expression)
import CommonTypes
import Macro --marcos
{-# LINE 13 "dist/build/ConcreteSyntax.hs" #-}
-- AG ----------------------------------------------------------
{-
   alternatives:
      alternative AG:
         child elems          : Elems 
-}
data AG = AG (Elems)
-- Alt ---------------------------------------------------------
{-
   alternatives:
      alternative Alt:
         child pos            : {Pos}
         child names          : ConstructorSet 
         child tyvars         : {[Identifier]}
         child fields         : Fields 
         child macro          : {MaybeMacro}
-}
data Alt = Alt (Pos) (ConstructorSet) (([Identifier])) (Fields) (MaybeMacro)
-- Alts --------------------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : Alt 
         child tl             : Alts 
      alternative Nil:
-}
type Alts = [Alt]
-- Attrs -------------------------------------------------------
{-
   alternatives:
      alternative Attrs:
         child pos            : {Pos}
         child inh            : {AttrNames}
         child chn            : {AttrNames}
         child syn            : {AttrNames}
-}
data Attrs = Attrs (Pos) (AttrNames) (AttrNames) (AttrNames)
-- ConstructorSet ----------------------------------------------
{-
   alternatives:
      alternative CAll:
      alternative CDifference:
         child set1           : ConstructorSet 
         child set2           : ConstructorSet 
      alternative CName:
         child name           : {ConstructorIdent}
      alternative CUnion:
         child set1           : ConstructorSet 
         child set2           : ConstructorSet 
-}
data ConstructorSet = CAll
                    | CDifference (ConstructorSet) (ConstructorSet)
                    | CName (ConstructorIdent)
                    | CUnion (ConstructorSet) (ConstructorSet)
-- Elem --------------------------------------------------------
{-
   alternatives:
      alternative Attr:
         child pos            : {Pos}
         child ctx            : {ClassContext}
         child names          : NontSet 
         child quants         : {[String]}
         child attrs          : Attrs 
      alternative Data:
         child pos            : {Pos}
         child ctx            : {ClassContext}
         child names          : NontSet 
         child params         : {[Identifier]}
         child attrs          : Attrs 
         child alts           : Alts 
         child ext            : {Bool}
      alternative Deriving:
         child pos            : {Pos}
         child set            : NontSet 
         child classes        : {[NontermIdent]}
      alternative Module:
         child pos            : {Pos}
         child name           : {String}
         child exports        : {String}
         child imports        : {String}
      alternative Nocatas:
         child pos            : {Pos}
         child set            : NontSet 
      alternative Pragma:
         child pos            : {Pos}
         child names          : {[NontermIdent]}
      alternative Sem:
         child pos            : {Pos}
         child ctx            : {ClassContext}
         child names          : NontSet 
         child attrs          : Attrs 
         child quants         : {[String]}
         child alts           : SemAlts 
      alternative Set:
         child pos            : {Pos}
         child name           : {NontermIdent}
         child merge          : {Bool}
         child set            : NontSet 
      alternative Txt:
         child pos            : {Pos}
         child kind           : {BlockKind}
         child mbNt           : {Maybe NontermIdent}
         child lines          : {[String]}
      alternative Type:
         child pos            : {Pos}
         child ctx            : {ClassContext}
         child name           : {NontermIdent}
         child params         : {[Identifier]}
         child type           : {ComplexType}
      alternative Wrapper:
         child pos            : {Pos}
         child set            : NontSet 
-}
data Elem = Attr (Pos) (ClassContext) (NontSet) (([String])) (Attrs)
          | Data (Pos) (ClassContext) (NontSet) (([Identifier])) (Attrs) (Alts) (Bool)
          | Deriving (Pos) (NontSet) (([NontermIdent]))
          | Module (Pos) (String) (String) (String)
          | Nocatas (Pos) (NontSet)
          | Pragma (Pos) (([NontermIdent]))
          | Sem (Pos) (ClassContext) (NontSet) (Attrs) (([String])) (SemAlts)
          | Set (Pos) (NontermIdent) (Bool) (NontSet)
          | Txt (Pos) (BlockKind) ((Maybe NontermIdent)) (([String]))
          | Type (Pos) (ClassContext) (NontermIdent) (([Identifier])) (ComplexType)
          | Wrapper (Pos) (NontSet)
-- Elems -------------------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : Elem 
         child tl             : Elems 
      alternative Nil:
-}
type Elems = [Elem]
-- Field -------------------------------------------------------
{-
   alternatives:
      alternative FChild:
         child name           : {Identifier}
         child tp             : {Type}
      alternative FCtx:
         child tps            : {[Type]}
-}
data Field = FChild (Identifier) (Type)
           | FCtx (([Type]))
-- Fields ------------------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : Field 
         child tl             : Fields 
      alternative Nil:
-}
type Fields = [Field]
-- NontSet -----------------------------------------------------
{-
   alternatives:
      alternative All:
      alternative Difference:
         child set1           : NontSet 
         child set2           : NontSet 
      alternative Intersect:
         child set1           : NontSet 
         child set2           : NontSet 
      alternative NamedSet:
         child name           : {NontermIdent}
      alternative Path:
         child from           : {NontermIdent}
         child to             : {NontermIdent}
      alternative Union:
         child set1           : NontSet 
         child set2           : NontSet 
-}
data NontSet = All
             | Difference (NontSet) (NontSet)
             | Intersect (NontSet) (NontSet)
             | NamedSet (NontermIdent)
             | Path (NontermIdent) (NontermIdent)
             | Union (NontSet) (NontSet)
-- SemAlt ------------------------------------------------------
{-
   alternatives:
      alternative SemAlt:
         child pos            : {Pos}
         child constructorSet : ConstructorSet 
         child rules          : SemDefs 
-}
data SemAlt = SemAlt (Pos) (ConstructorSet) (SemDefs)
-- SemAlts -----------------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : SemAlt 
         child tl             : SemAlts 
      alternative Nil:
-}
type SemAlts = [SemAlt]
-- SemDef ------------------------------------------------------
{-
   alternatives:
      alternative AroundDef:
         child ident          : {Identifier}
         child rhs            : {Expression}
      alternative AttrOrderBefore:
         child before         : {[Occurrence]}
         child after          : {[Occurrence]}
      alternative AugmentDef:
         child ident          : {Identifier}
         child rhs            : {Expression}
      alternative Def:
         child pos            : {Pos}
         child mbName         : {Maybe Identifier}
         child pattern        : {Pattern}
         child rhs            : {Expression}
         child owrt           : {Bool}
         child pure           : {Bool}
         child eager          : {Bool}
      alternative MergeDef:
         child target         : {Identifier}
         child nt             : {Identifier}
         child sources        : {[Identifier]}
         child rhs            : {Expression}
      alternative SemPragma:
         child names          : {[NontermIdent]}
      alternative TypeDef:
         child pos            : {Pos}
         child ident          : {Identifier}
         child tp             : {Type}
      alternative UniqueDef:
         child ident          : {Identifier}
         child ref            : {Identifier}
-}
data SemDef = AroundDef (Identifier) (Expression)
            | AttrOrderBefore (([Occurrence])) (([Occurrence]))
            | AugmentDef (Identifier) (Expression)
            | Def (Pos) ((Maybe Identifier)) (Pattern) (Expression) (Bool) (Bool) (Bool)
            | MergeDef (Identifier) (Identifier) (([Identifier])) (Expression)
            | SemPragma (([NontermIdent]))
            | TypeDef (Pos) (Identifier) (Type)
            | UniqueDef (Identifier) (Identifier)
-- SemDefs -----------------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : SemDef 
         child tl             : SemDefs 
      alternative Nil:
-}
type SemDefs = [SemDef]