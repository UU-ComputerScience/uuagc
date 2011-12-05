

-- UUAGC 0.9.39.1.0 (src-ag/Macro.ag)
module Macro where
{-# LINE 4 "src-ag/Macro.ag" #-}

import CommonTypes
{-# LINE 9 "dist/build/uuagc/uuagc-tmp/Macro.hs" #-}
-- Macro -------------------------------------------------------
{-
   alternatives:
      alternative Macro:
         child con            : {ConstructorIdent}
         child children       : MacroChildren 
      alternative None:
-}
data Macro  = Macro (ConstructorIdent) (MacroChildren ) 
            | None 
            deriving ( Show)
-- MacroChild --------------------------------------------------
{-
   alternatives:
      alternative ChildChild:
         child name           : {Identifier}
         child child          : {Identifier}
      alternative RuleChild:
         child name           : {Identifier}
         child macro          : Macro 
      alternative ValueChild:
         child name           : {Identifier}
         child value          : {String}
-}
data MacroChild  = ChildChild (Identifier) (Identifier) 
                 | RuleChild (Identifier) (Macro ) 
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
type MacroChildren  = [MacroChild ]
-- MaybeMacro --------------------------------------------------
{-
   alternatives:
      alternative Just:
         child just           : Macro 
      alternative Nothing:
-}
type MaybeMacro  = Maybe Macro 