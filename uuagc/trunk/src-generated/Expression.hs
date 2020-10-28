

-- UUAGC 0.9.53 (src-ag/Expression.ag)
module Expression where
{-# LINE 2 "src-ag/Expression.ag" #-}

import UU.Scanner.Position(Pos)
import HsToken
{-# LINE 10 "src-generated/Expression.hs" #-}
-- Expression --------------------------------------------------
{-
   alternatives:
      alternative Expression:
         child pos            : {Pos}
         child tks            : {[HsToken]}
-}
data Expression = Expression (Pos) (([HsToken]))