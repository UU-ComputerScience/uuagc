module RhsCheck(checkRhs,checkBlock,checkTy) where

import Language.Haskell.Exts
import ErrorMessages
import ConcreteSyntax
import Expression
import HsToken
import UU.Scanner.Position

checkRhs,checkBlock,checkTy :: Expression -> Errors
checkRhs = check parseExpWithMode
checkBlock = check parseModuleWithMode
checkTy = check parseTypeWithMode

check :: (ParseMode -> String -> ParseResult a) -> Expression -> Errors
check p (Expression pos tks) = case res of
   ParseOk _           -> []
   ParseFailed loc msg -> let pos' = Pos (srcLine loc + line pos - 1) (srcColumn loc) (srcFilename loc)
                          in [HsParseError pos' msg]
 where
  pos0 = Pos (line pos) 1 (file pos)
  str  = toString pos0 tks
  res  = p mode str
  mode = defaultParseMode { parseFilename = file pos, ignoreLanguagePragmas = False, extensions = glasgowExts }

toString :: Pos -> HsTokens -> String
toString _    []       = ""
toString cPos (tk:tks) = move ++ current ++ next
  where
    tkPos   = getPos tk
    move    = addSpacing (line tkPos - line cPos) (column cPos) (column tkPos)
    current = fmt tk
    nPos    = upd tkPos current
    next    = toString nPos tks

getPos :: HsToken -> Pos
getPos (AGLocal _ pos _)    = pos
getPos (AGField _ _ pos _)  = pos
getPos (HsToken _ pos)      = pos
getPos (CharToken _ pos)    = pos
getPos (StrToken _ pos)     = pos
getPos (Err _ pos)          = pos

fmt :: HsToken -> String
fmt (AGLocal var _ _)         = show var
fmt (AGField field attr _ _)  = "_" ++ show field ++ "_" ++ show attr
fmt (HsToken val _)           = val
fmt (CharToken val _)         = show val
fmt (StrToken val _)          = show val
fmt (Err val _)               = val

upd :: Pos -> String -> Pos
upd p s = foldl adv p s

addSpacing :: Int -> Int -> Int -> String
addSpacing l c1 c2 = replicate l '\n' ++ replicate c ' '
  where
    c  | l == 0 = c2 - c1
       | otherwise = c2 - 1
