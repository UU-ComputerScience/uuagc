module ATermWrite where

import ATermAbstractSyntax
import Data.List (intersperse)

writeATerm              :: ATerm -> String
writeATerm t            = writeAT 0 t

writeAT 		:: Int -> ATerm -> String
writeAT n (AAppl c ts) = (if (n > 0) then "\n" else "")
                         ++ replicate n ' '  
                         ++ writeATermAux c (map (writeAT (n+2)) ts)
writeAT n (AList ts)    =  bracket (commaSep (map (writeAT n) ts))
writeAT _ (AInt i)      =  show i
writeAT _ (AString s)   =  quote s

writeATermAux           :: [Char] -> [[Char]] -> [Char]
writeATermAux c []	=  c++(parenthesise "")
writeATermAux c ts	=  c++(parenthesise (commaSep ts))

commaSep                :: [[Char]] -> [Char]
commaSep strs		=  concat (intersperse "," strs)
bracket                 :: [Char] -> [Char]
bracket str		= "["++str++"]"
parenthesise            :: [Char] -> [Char]
parenthesise str	= "("++str++")"
quote                   :: [Char] -> [Char]
quote str		= "\""++str++"\""