

-- UUAGC 0.9.40.1 (src-ag/VisagePatterns.ag)
module VisagePatterns where
{-# LINE 2 "src-ag/VisagePatterns.ag" #-}

import UU.Scanner.Position(Pos)
import CommonTypes
{-# LINE 10 "dist/build/VisagePatterns.hs" #-}
-- VisagePattern -----------------------------------------------
{-
   alternatives:
      alternative VAlias:
         child field          : {Identifier}
         child attr           : {Identifier}
         child pat            : VisagePattern 
      alternative VConstr:
         child name           : {ConstructorIdent}
         child pats           : VisagePatterns 
      alternative VProduct:
         child pos            : {Pos}
         child pats           : VisagePatterns 
      alternative VUnderscore:
         child pos            : {Pos}
      alternative VVar:
         child field          : {Identifier}
         child attr           : {Identifier}
-}
data VisagePattern = VAlias (Identifier) (Identifier) (VisagePattern)
                   | VConstr (ConstructorIdent) (VisagePatterns)
                   | VProduct (Pos) (VisagePatterns)
                   | VUnderscore (Pos)
                   | VVar (Identifier) (Identifier)
-- VisagePatterns ----------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : VisagePattern 
         child tl             : VisagePatterns 
      alternative Nil:
-}
type VisagePatterns = [VisagePattern]