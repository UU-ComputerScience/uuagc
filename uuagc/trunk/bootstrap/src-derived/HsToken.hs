

-- UUAGC 0.9.40.1 (src-ag/HsToken.ag)
module HsToken where
{-# LINE 2 "src-ag/HsToken.ag" #-}

import CommonTypes
import UU.Scanner.Position(Pos)
{-# LINE 10 "dist/build/HsToken.hs" #-}
-- HsToken -----------------------------------------------------
{-
   alternatives:
      alternative AGField:
         child field          : {Identifier}
         child attr           : {Identifier}
         child pos            : {Pos}
         child rdesc          : {Maybe String}
      alternative AGLocal:
         child var            : {Identifier}
         child pos            : {Pos}
         child rdesc          : {Maybe String}
      alternative CharToken:
         child value          : {String}
         child pos            : {Pos}
      alternative Err:
         child mesg           : {String}
         child pos            : {Pos}
      alternative HsToken:
         child value          : {String}
         child pos            : {Pos}
      alternative StrToken:
         child value          : {String}
         child pos            : {Pos}
-}
data HsToken = AGField (Identifier) (Identifier) (Pos) ((Maybe String))
             | AGLocal (Identifier) (Pos) ((Maybe String))
             | CharToken (String) (Pos)
             | Err (String) (Pos)
             | HsToken (String) (Pos)
             | StrToken (String) (Pos)
             deriving ( Show)
-- HsTokens ----------------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : HsToken 
         child tl             : HsTokens 
      alternative Nil:
-}
type HsTokens = [HsToken]
-- HsTokensRoot ------------------------------------------------
{-
   alternatives:
      alternative HsTokensRoot:
         child tokens         : HsTokens 
-}
data HsTokensRoot = HsTokensRoot (HsTokens)