module Main where

import Prelude hiding (exp)
import UU.Parsing
import TigerScanner (scanFile)
import UU.Scanner.Token
import UU.Scanner.Position
import UU.Scanner.TokenParser
import TigerSem(sem_Program)
import TigerError(sem_Error)
import TigerAS
import TigerTypes
import System.Environment(getArgs)
import UU.Scanner.GenTokenSymbol
import UU.Scanner.GenTokenOrd

pBracksPos p = (,) <$> pOBrackPos <*> p <* pCBrack

ident :: Parser Token Ident
ident = uncurry Id <$> (pVaridPos <|> pConidPos)

integer :: Parser Token (Integer,Pos)
integer = (\(v,p) -> (read v,p) ) <$> pIntegerPos

string :: Parser Token (String,Pos)
string =  pStringPos
{-
Precedence of the operators (high to low):

* / 		-- left
+ -		-- left
>= <= = <> < >	-- non
&		-- left
|		-- left
:= 		-- right
-}



main = do args <- getArgs
          case args of
           [fn] -> parseFile fn
           _    -> putStrLn "usage: tiger <file>"

parseFile f = do text <- readFile f
                 putStrLn text
                 tokens <- scanFile  f
                 ast <- parseIO program tokens
                 --print ast
                 mapM (putStrLn.sem_Error)
                      (sem_Program ast)
                 return ()
{-
 where keywords = ["array", "if", "then", "else", "while", "for", "to", "do", "let", "in", "end", "of", "break", "nil", "function", "var", "type" ]
       keyops   = [".",":", "+", "-", "*", "/", "=", "<>", "<", "<=", ">", ">=", "&", "|", ":=" ]
       symbols  = ",;()[]{}"
       opchars  = ":+-*/=<>&.|"
-}
program = Program <$> exp


oper ops = case ops of
             [] -> error "empty operator list"
             [x] -> mkOp x
             _   -> foldr1 (<|>) (map mkOp ops)
  where mkOp x = Op x <$> pKeyPos x

operators = [ (["*", "/"                       ], LeftAssoc)
            , (["+", "-"                       ], LeftAssoc)
            , ([">=", "<=", "=", "<>", "<", ">"], NonAssoc )
            , (["&"                            ], LeftAssoc)
            , (["|"                            ], LeftAssoc)
            ]

expression = foldl f atom operators
 where f p (ops,assoc) = case assoc of
                          LeftAssoc  -> pChainl (oper ops) p
                          RightAssoc -> pChainr (oper ops) p
                          NonAssoc   -> p <??>  (oper ops <*> p)

data Assoc = LeftAssoc | RightAssoc | NonAssoc deriving Show

assignField = AssignField <$> ident <*  pKey "=" <*> exp

exp =  Assign <$> lvalue <*> pKeyPos ":=" <*> exp
   <|> expression

atom =  Nil <$> pKeyPos "nil"
    <|> uncurry IntLit <$> integer
    <|> uncurry StringLit <$> string
    <|> RecordVal <$> type_ident <*> pCurly (pListSep (pKey ",") assignField)
    <|> LValue <$> lvalue
    <|> Apply <$>ident <*> pParens (pListSep (pKey ",") exp)
    <|> pParens exps
    <|> Let   <$> pKeyPos "let" <*> decs
              <* pKey "in" <*> exps
              <* pKey "end"
    <|> ArrayVal  <$> type_ident <*> pBracks exp <* pKey "of" <*> exp
    <|> (\p e -> UnOp p "-" e) <$> pKeyPos "-" <*> exp
    <|> Break <$> pKeyPos "break"
  -- Control structures
    <|> If <$> pKeyPos "if" <*> exp
                            <*  pKey "then" <*> exp
                            <*> ((pKey "else" *> exp) `opt` Skip)
    <|> While <$> pKeyPos "while" <*> exp
              <*  pKey "do" <*> exp
    <|> For   <$> pKeyPos "for" <*> ident <* pKey ":=" <*> exp <* pKey "to" <*> exp
              <*  pKey "do" <*> exp

lvalue = (\i f -> f (Ident i)) <$> ident <*> subOrFields
  where subOrFields = (\xs x -> foldl (flip ($)) x xs) <$> pList subOrField
  	subOrField  =  (\p f e -> Dot p e f)   <$>  pKeyPos "." <*> ident
	  	   <|> (\(p,i) e -> Sub p e i) <$> pBracksPos exp

exps = f <$> pListSep (pKey ";") exp
 where f xs = case xs of
               []  -> Skip
               [x] -> x
               _   -> foldr1 Sequence xs

decs = pList declGroup

declGroup =  FunDecs  <$> pList1 funDec
         <|> TypeDecs <$> pList1 typeDec
         <|> varDec

typeDec = TypeDec <$> pKeyPos "type" <*> ident <* pKey "=" <*> ty
varDec  = VarDec  <$> pKeyPos "var" <*> ident <*> optType <* pKey ":=" <*> exp
funDec  = FunDec  <$> pKeyPos "function" <*> ident <*> pParens tyfields <*> optType <* pKey "=" <*> exp

optType = (Just <$ pKey ":" <*> type_ident) `opt` Nothing
-- Types
ty  =  Var <$> type_ident
   <|> Record <$> pCurly tyfields
   <|> Array <$ pKey "array" <* pKey "of" <*> type_ident

tyfields = pListSep (pKey ",") tyfield
 where tyfield = TypedVar <$> ident <* pKey ":" <*> type_ident

type_ident  = ident
