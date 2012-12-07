

-- UUAGC 0.9.42.2 (src-ag/ConcreteSyntax.ag)
module ConcreteSyntax where
{-# LINE 2 "./src-ag/ConcreteSyntax.ag" #-}

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
      alternative CName:
         child name           : {ConstructorIdent}
      alternative CUnion:
         child set1           : ConstructorSet 
         child set2           : ConstructorSet 
      alternative CDifference:
         child set1           : ConstructorSet 
         child set2           : ConstructorSet 
      alternative CAll:
-}
data ConstructorSet = CName (ConstructorIdent)
                    | CUnion (ConstructorSet) (ConstructorSet)
                    | CDifference (ConstructorSet) (ConstructorSet)
                    | CAll
-- Elem --------------------------------------------------------
{-
   alternatives:
      alternative Data:
         child pos            : {Pos}
         child ctx            : {ClassContext}
         child names          : NontSet 
         child params         : {[Identifier]}
         child attrs          : Attrs 
         child alts           : Alts 
         child ext            : {Bool}
      alternative Type:
         child pos            : {Pos}
         child ctx            : {ClassContext}
         child name           : {NontermIdent}
         child params         : {[Identifier]}
         child type           : {ComplexType}
      alternative Attr:
         child pos            : {Pos}
         child ctx            : {ClassContext}
         child names          : NontSet 
         child quants         : {[String]}
         child attrs          : Attrs 
      alternative Sem:
         child pos            : {Pos}
         child ctx            : {ClassContext}
         child names          : NontSet 
         child attrs          : Attrs 
         child quants         : {[String]}
         child alts           : SemAlts 
      alternative Txt:
         child pos            : {Pos}
         child kind           : {BlockKind}
         child mbNt           : {Maybe NontermIdent}
         child lines          : {[String]}
      alternative Set:
         child pos            : {Pos}
         child name           : {NontermIdent}
         child merge          : {Bool}
         child set            : NontSet 
      alternative Deriving:
         child pos            : {Pos}
         child set            : NontSet 
         child classes        : {[NontermIdent]}
      alternative Wrapper:
         child pos            : {Pos}
         child set            : NontSet 
      alternative Nocatas:
         child pos            : {Pos}
         child set            : NontSet 
      alternative Pragma:
         child pos            : {Pos}
         child names          : {[NontermIdent]}
      alternative Module:
         child pos            : {Pos}
         child name           : {String}
         child exports        : {String}
         child imports        : {String}
-}
data Elem = Data (Pos) (ClassContext) (NontSet) (([Identifier])) (Attrs) (Alts) (Bool)
          | Type (Pos) (ClassContext) (NontermIdent) (([Identifier])) (ComplexType)
          | Attr (Pos) (ClassContext) (NontSet) (([String])) (Attrs)
          | Sem (Pos) (ClassContext) (NontSet) (Attrs) (([String])) (SemAlts)
          | Txt (Pos) (BlockKind) ((Maybe NontermIdent)) (([String]))
          | Set (Pos) (NontermIdent) (Bool) (NontSet)
          | Deriving (Pos) (NontSet) (([NontermIdent]))
          | Wrapper (Pos) (NontSet)
          | Nocatas (Pos) (NontSet)
          | Pragma (Pos) (([NontermIdent]))
          | Module (Pos) (String) (String) (String)
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
      alternative NamedSet:
         child name           : {NontermIdent}
      alternative All:
      alternative Union:
         child set1           : NontSet 
         child set2           : NontSet 
      alternative Intersect:
         child set1           : NontSet 
         child set2           : NontSet 
      alternative Difference:
         child set1           : NontSet 
         child set2           : NontSet 
      alternative Path:
         child from           : {NontermIdent}
         child to             : {NontermIdent}
-}
data NontSet = NamedSet (NontermIdent)
             | All
             | Union (NontSet) (NontSet)
             | Intersect (NontSet) (NontSet)
             | Difference (NontSet) (NontSet)
             | Path (NontermIdent) (NontermIdent)
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
      alternative Def:
         child pos            : {Pos}
         child mbName         : {Maybe Identifier}
         child pattern        : {Pattern}
         child rhs            : {Expression}
         child owrt           : {Bool}
         child pure           : {Bool}
         child eager          : {Bool}
      alternative TypeDef:
         child pos            : {Pos}
         child ident          : {Identifier}
         child tp             : {Type}
      alternative UniqueDef:
         child ident          : {Identifier}
         child ref            : {Identifier}
      alternative AugmentDef:
         child ident          : {Identifier}
         child rhs            : {Expression}
      alternative AroundDef:
         child ident          : {Identifier}
         child rhs            : {Expression}
      alternative MergeDef:
         child target         : {Identifier}
         child nt             : {Identifier}
         child sources        : {[Identifier]}
         child rhs            : {Expression}
      alternative SemPragma:
         child names          : {[NontermIdent]}
      alternative AttrOrderBefore:
         child before         : {[Occurrence]}
         child after          : {[Occurrence]}
-}
data SemDef = Def (Pos) ((Maybe Identifier)) (Pattern) (Expression) (Bool) (Bool) (Bool)
            | TypeDef (Pos) (Identifier) (Type)
            | UniqueDef (Identifier) (Identifier)
            | AugmentDef (Identifier) (Expression)
            | AroundDef (Identifier) (Expression)
            | MergeDef (Identifier) (Identifier) (([Identifier])) (Expression)
            | SemPragma (([NontermIdent]))
            | AttrOrderBefore (([Occurrence])) (([Occurrence]))
-- SemDefs -----------------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : SemDef 
         child tl             : SemDefs 
      alternative Nil:
-}
type SemDefs = [SemDef]