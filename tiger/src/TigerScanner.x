{
module TigerScanner(scanFile) where

import UU.Scanner.Token
import UU.Scanner.TokenShow
import UU.Scanner.Position
import qualified Data.Set as Set
}
%wrapper "gscan"

$letter = [a-zA-Z]		    -- alphabetic characters
$digit = 0-9			    -- digits
$octal = 0-7			    -- octal digits
$hex   = [0-9A-Fa-f]  	    -- hexadecimal digits
$ident = [$letter $digit _] -- identifier character

@escape  = \\([abfnrtv\^"\\] | $octal{1,3} | x$hex{1,2})  -- character escape
@content = $printable # ["\\] | @escape                   -- string character

tiger :-
    $white+                       ;

    "/*"                          { comment }


    "," | ":" | ";" | "(" | ")" |
    "[" | "]" | "{" | "}" | "." | 
    "+" | "-" | "*" | "/" | "=" | 
    "<>" | "<" | "<=" | ">" | 
    ">=" | "&" | "|" | ":="       { symbol }

    $digit+                       { integer }

    \" @content* \"               { string }

    $letter $ident*               { identifier }
    
 



{ 
-- semantic actions
-- if a sequence of letters is an identifier, except when it is member of the set of reserved words.
identifier = makeToken keywordOrIdentifier 
    where keywordOrIdentifier str | Set.member str reservedwords  = reserved str 
                                  | otherwise                     = valueToken TkVarid str 
                           

reservedwords = Set.fromList [ "array", "if", "then", "else", "while", "for", "to", "do", "let", "in", "end", "of", "break", "nil", "function", "var", "type" ]

-- special symbols and operators
symbol = makeToken reserved

-- decimal integers
integer  = makeToken (valueToken TkInteger10)

-- string literals
string   = makeToken (valueToken TkString)

-- comment-lexer: drops a comment-block enclosed by /* and */ from the input, and resumes lexing. 
-- block comments can be nested, hence: /* this /* is some */ text */ 
-- is a single comment-block

comment pos _ inp _ _ st@(_,file) = dropcomments (move_pos2 pos) 1 (drop 2 inp)
  where move_pos2 (AlexPn a l c) = AlexPn (a+2) l (c+2)
        dropcomments p 0 ss = continue p ss 
        dropcomments p n inp = case inp of
                                ('*':'/':ss) -> dropcomments (move_pos2 p ) (n-1) ss
                                ('/':'*':ss) -> dropcomments (move_pos2 p ) (n+1) ss
                                (s      :ss) -> dropcomments (alexMove p s) n     ss
                                []           -> errToken ("unterminated comment") (makePos file pos)
                                              : continue p [] 
        continue = tiger_scan file                                   
                           
makeToken :: (String -> Pos -> Token) 
          -> AlexPosn 
          -> a 
          -> String 
          -> Int 
          -> ((Int,FilePath) -> [Token]) 
          -> (Int,FilePath) -> [Token]
makeToken f p _ inp len cont state@(_,file) = f (take len inp) (makePos file p) : cont state 

makePos :: FilePath -> AlexPosn -> Pos
makePos f (AlexPn _ l c) = Pos l c f 

scanFile :: FilePath -> IO [Token]
scanFile file = do txt <- readFile file
                   return (tiger_scan file alexStartPos txt)

tiger_scan :: FilePath -> AlexPosn -> String -> [Token]
tiger_scan file pos input = alex_gscan stop_act pos '\n' input (0,file)

stop_act pos char ""     state = []
stop_act pos char (s:ss) state = errToken ("unexpected character: " ++ show s) (makePos (snd state) pos)
                                    : tiger_scan (snd state) (alexMove pos s) ss
}

