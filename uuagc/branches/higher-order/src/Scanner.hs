
module Scanner where
import TokenDef
import UU.Scanner.Position
import UU.Scanner.Token
import UU.Parsing(InputState(..),Either'(..))
import Maybe
import List
import Char
import UU.Scanner.GenToken

data Input = Input !Pos String (Maybe (Token, Input))  

instance InputState Input Token Pos where
 splitStateE input@(Input _ _ next) = 
                case next of
                     Nothing         -> Right' input
                     Just (s, rest)  -> Left' s rest
 splitState (Input _ _ next) = 
                case next of
                     Nothing         -> error "splitState on empty input"
                     Just (s, rest)  -> ( s, rest)
 getPosition (Input pos _ next) =  case next of 
                                    Just (s,_) -> position s
                                    Nothing    -> pos -- end of file


input :: Pos -> String -> Input
input pos inp = Input pos 
                      inp 
                      (case scan pos inp of
                             Nothing      -> Nothing
                             Just (s,p,r) -> Just (s, input p r)
                      )

type Lexer s = Pos -> String -> Maybe (s,Pos,String)

scan :: Lexer Token
scan p []                        = Nothing
scan p ('-':'-':xs)              = let (com,rest) = span (/= '\n') xs
                                   in advc' (2+length com) p scan rest
scan p ('{':'-':xs)              = advc' 2 p (ncomment scan) xs
scan p ('{'    :xs)              = advc' 1 p codescrap xs
scan p ('\CR':xs)                = case xs of
                                    '\LF':ys -> newl' p scan ys --ms newline
                                    _        -> newl' p scan xs --mac newline
scan p ('\LF':xs)                =  newl' p scan xs             --unix newline
scan p (x:xs) | isSpace x        = updPos'  x p scan  xs
scan p xs = Just (scan' xs)
  where scan' ('.' :rs)          = (reserved "." p, advc 1 p, rs)
        scan' ('@' :rs)          = (reserved "@" p, advc 1 p, rs)
        scan' (',' :rs)          = (reserved "," p, advc 1 p, rs)
        scan' ('_' :rs)          = (reserved "_" p, advc 1 p, rs)
        scan' ('[' :rs)          = (reserved "[" p, advc 1 p, rs)
        scan' (']' :rs)          = (reserved "]" p, advc 1 p, rs)
        scan' ('(' :rs)          = (reserved "(" p, advc 1 p, rs)
        scan' (')' :rs)          = (reserved ")" p, advc 1 p, rs)
--        scan' ('{'    :rs)       = (OBrace      p, advc 1 p, rs)
--        scan' ('}'    :rs)       = (CBrace      p, advc 1 p, rs)

        scan' ('\"' :rs)         = let isOk c = c /= '"' && c /= '\n'
                                       (str,rest) = span isOk rs
                                   in if null rest || head rest /= '"'
                                          then (errToken "unterminated string literal"   p
                                               , advc (1+length str) p,rest)
                                          else (valueToken TkString str p, advc (2+length str) p, tail rest)

        scan' ('=' :rs)          = (reserved "=" p, advc 1 p, rs)
        scan' (':':'=':rs)       = (reserved ":=" p, advc 2 p, rs)

        scan' (':' :rs)          = (reserved ":" p, advc 1 p, rs)
        scan' ('|' :rs)          = (reserved "|" p, advc 1 p, rs)

        scan' ('/':'\\':rs)      = (reserved "/\\" p, advc 2 p, rs)
        scan' ('-':'>' :rs)      = (reserved "->" p, advc 2 p, rs)
        scan' ('-'     :rs)      = (reserved "-" p, advc 1 p, rs)
        scan' ('*'     :rs)      = (reserved "*" p, advc 1 p, rs)

        scan' (x:rs) | isLower x = let (var,rest) = ident rs
                                       str        = (x:var)
                                       tok | str `elem` keywords = reserved str
                                           | otherwise           = valueToken TkVarid str
                                   in (tok p, advc (length var+1) p, rest)
                     | isUpper x = let (var,rest) = ident rs
                                       str        = (x:var)
                                       tok | str `elem` keywords = reserved str 
                                           | otherwise           = valueToken TkConid str
                                   in (tok p, advc (length var+1) p,rest)
                     | otherwise = (errToken ("unexpected character " ++ show x) p, advc 1 p, rs)

ident = span isValid
 where isValid x = isAlphaNum x || x =='_' || x == '\''
keywords = [ "DATA", "EXT", "ATTR", "SEM","TYPE", "USE", "loc","lhs", "inst", "INCLUDE"
           , "SET","DERIVING","FOR", "WRAPPER", "MAYBE"
           , "PRAGMA"
           ]

ncomment c p ('-':'}':xs) = advc' 2 p c  xs
ncomment c p ('{':'-':xs) = advc' 2 p (ncomment (ncomment c)) xs
ncomment c p (x:xs)       = updPos' x p (ncomment c)  xs
ncomment c p []           = Just (errToken "unterminated nested comment" p, p,[])

codescrap p xs = let (p2,xs2,sc) = codescrap' 1 p xs
                 in case xs2 of
                         ('}':rest) -> Just (valueToken TkTextln sc p,advc 1 p2,rest)
                         _          -> Just (errToken "unterminated codescrap" p,p2,xs2)


codescrap' d p [] = (p,[],[])
{-
codescrap' d p ('{':'{':xs) = let (p2,xs2,sc) = advc' 2 p (codescrap' d) xs
                              in (p2,xs2,'{':' ':sc)
codescrap' d p ('}':'}':xs) = let (p2,xs2,sc) = advc' 2 p (codescrap' d) xs
                              in (p2,xs2,'}':' ':sc)
-}                              
codescrap' d p ('{':xs)     = let (p2,xs2,sc) = advc' 1 p (codescrap' (d+1)) xs
                              in (p2,xs2,'{' : sc)
codescrap' d p ('}':xs)     | d == 1 = (p,'}':xs,[])
                            | otherwise = let (p2,xs2,sc) = advc' 1 p (codescrap' (d-1)) xs
                                          in (p2,xs2,'}' : sc)
codescrap' d p (x  :xs)     = let (p2,xs2,sc) = updPos' x p (codescrap' d) xs
                              in (p2,xs2,x:sc)
--Literate Mode
scanLit xs = (fs, foldr insNL (const "") codeLns 1)
  where insNL (n,line) rec = \n1 -> replicate (n-n1) '\n' ++ line ++ rec n
        (fs,codeLns,_) = getBlocks ([1..] `zip`  toLines xs)
        getBlocks [] = ([],[],[])
        getBlocks xs = let (files1,txt1,r1) = getBlock xs
                           (files2,txt2,r2) = getBlocks r1
                       in (files1++files2, txt1++txt2, r2)


        getBlock = getLines . dropWhile comment
        getLines [] = ([],[],[])
        getLines ((n,l):ls) | "\\begin{Code}" `isPrefixOf` l = let (lns,rest) = codelines ls
                                                               in ([],lns,rest)
                            | "\\IN{" `isPrefixOf` l        =
                                     let name = getName l
                                     in  ([name],[],ls)
                            | otherwise = getBlock ls
        comment = not . ("\\" `isPrefixOf`) .snd

toLines     :: String -> [String]
toLines ""   = []
toLines s    = let (l,s') = breakLine s
               in l :  toLines s'
breakLine xs = case xs of
                '\CR' : ys -> case ys of
                                '\LF' : zs -> ([],zs) 
                                _          -> ([],ys)
                '\LF' : ys -> ([], ys)
                '\n'  : ys -> ([], ys)
                x     : ys -> let (l,s) = breakLine ys
                              in (x:l,s)
                []         -> ([],[])
 
codelines [] = error "Unterminated code block"
codelines ((n,l):ls) | "\\end{Code}" `isPrefixOf` l = ([],ls)
                     | otherwise                    = let (lns,r) = codelines ls
                                                      in ((n,l):lns,r)

getName l = case r of
   ('}':_) -> nm
   _       -> error $ "missing '}' in \\IN"
 where (nm,r) = span (/='}') (drop 4 l)