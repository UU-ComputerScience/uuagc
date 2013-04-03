

-- UUAGC 0.9.42.3 (src-ag/VisagePatterns.ag)
module VisagePatterns where
{-# LINE 2 "./src-ag/VisagePatterns.ag" #-}

import UU.Scanner.Position(Pos)
import CommonTypes
{-# LINE 10 "dist/build/VisagePatterns.hs" #-}
-- VisagePattern -----------------------------------------------
{-
   alternatives:
      alternative VConstr:
         child name           : {ConstructorIdent}
         child pats           : VisagePatterns 
      alternative VProduct:
         child pos            : {Pos}
         child pats           : VisagePatterns 
      alternative VVar:
         child field          : {Identifier}
         child attr           : {Identifier}
      alternative VAlias:
         child field          : {Identifier}
         child attr           : {Identifier}
         child pat            : VisagePattern 
      alternative VUnderscore:
         child pos            : {Pos}
-}
data VisagePattern = VConstr (ConstructorIdent) (VisagePatterns)
                   | VProduct (Pos) (VisagePatterns)
                   | VVar (Identifier) (Identifier)
                   | VAlias (Identifier) (Identifier) (VisagePattern)
                   | VUnderscore (Pos)
-- VisagePatterns ----------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : VisagePattern 
         child tl             : VisagePatterns 
      alternative Nil:
-}
type VisagePatterns = [VisagePattern]