

-- UUAGC 0.9.42.0 (src-ag/Patterns.ag)
module Patterns where
{-# LINE 2 "./src-ag/Patterns.ag" #-}

-- Patterns.ag imports
import UU.Scanner.Position(Pos)
import CommonTypes (ConstructorIdent,Identifier)
{-# LINE 11 "dist/build/Patterns.hs" #-}
-- Pattern -----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         copy                 : Pattern 
   alternatives:
      alternative Constr:
         child name           : {ConstructorIdent}
         child pats           : Patterns 
         visit 0:
            local copy        : _
      alternative Product:
         child pos            : {Pos}
         child pats           : Patterns 
         visit 0:
            local copy        : _
      alternative Alias:
         child field          : {Identifier}
         child attr           : {Identifier}
         child pat            : Pattern 
         visit 0:
            local copy        : _
      alternative Irrefutable:
         child pat            : Pattern 
         visit 0:
            local copy        : _
      alternative Underscore:
         child pos            : {Pos}
         visit 0:
            local copy        : _
-}
data Pattern = Constr (ConstructorIdent) (Patterns)
             | Product (Pos) (Patterns)
             | Alias (Identifier) (Identifier) (Pattern)
             | Irrefutable (Pattern)
             | Underscore (Pos)
             deriving ( Show)
-- Patterns ----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         copy                 : Patterns 
   alternatives:
      alternative Cons:
         child hd             : Pattern 
         child tl             : Patterns 
         visit 0:
            local copy        : _
      alternative Nil:
         visit 0:
            local copy        : _
-}
type Patterns = [Pattern]