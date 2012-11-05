

-- UUAGC 0.9.42.0 (src-ag/Macro.ag)
module Macro where
{-# LINE 4 "./src-ag/Macro.ag" #-}

import CommonTypes
{-# LINE 9 "dist/build/Macro.hs" #-}
-- Macro -------------------------------------------------------
{-
   alternatives:
      alternative Macro:
         child con            : {ConstructorIdent}
         child children       : MacroChildren 
      alternative None:
-}
data Macro = Macro (ConstructorIdent) (MacroChildren)
           | None
           deriving ( Show)
-- MacroChild --------------------------------------------------
{-
   alternatives:
      alternative RuleChild:
         child name           : {Identifier}
         child macro          : Macro 
      alternative ChildChild:
         child name           : {Identifier}
         child child          : {Identifier}
      alternative ValueChild:
         child name           : {Identifier}
         child value          : {String}
-}
data MacroChild = RuleChild (Identifier) (Macro)
                | ChildChild (Identifier) (Identifier)
                | ValueChild (Identifier) (String)
                deriving ( Show)
-- MacroChildren -----------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : MacroChild 
         child tl             : MacroChildren 
      alternative Nil:
-}
type MacroChildren = [MacroChild]
-- MaybeMacro --------------------------------------------------
{-
   alternatives:
      alternative Just:
         child just           : Macro 
      alternative Nothing:
-}
type MaybeMacro = Maybe Macro