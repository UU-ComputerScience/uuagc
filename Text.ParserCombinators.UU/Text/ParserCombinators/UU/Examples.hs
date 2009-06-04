{-# LANGUAGE  RankNTypes, 
              GADTs,
              MultiParamTypeClasses,
              FunctionalDependencies, 
              FlexibleInstances, 
              FlexibleContexts, 
              UndecidableInstances,
              NoMonomorphismRestriction#-}

module Text.ParserCombinators.UU.Examples where
import Char
import Text.ParserCombinators.UU.Parsing

type P b =  P_m (Str Char) b -> String -> (b, [Error Char Char Int]) 
test :: P b
test p inp = parse ( (,) <$> p <*> pEnd) (listToStr inp)

lift a = [a]

pa, pb, paz :: P_m (Str  Char) [Char] 
pa = lift <$> pSym 'a'
pb = lift <$> pSym 'b'
p <++> q = (++) <$> p <*> q
pa2 =   pa <++> pa
pa3 =   pa <++> pa2

pCount p = (\ a b -> b+1) <$> p <*> pCount p <<|> pReturn 0
pExact 0 p = pReturn []
pExact n p = (:) <$> p <*> pExact (n-1) p

paz = pList (pSym ('a', 'z'))

paz' = pSym (\t -> 'a' <= t && t <= 'z', "a .. z", 'k')

main :: IO ()
main = do print (test pa "a")
          print (test pa "b")
          print (test pa2 "bbab")
          print (test pa "ba")
          print (test pa "aa")
          print (test  (do  l <- pCount pa
                            pExact l pb) "aaacabbb")
          print (test (amb ( (++) <$> pa2 <*> pa3 <|> (++) <$> pa3 <*> pa2))  "aaabaa")
          print (test paz "ab1z7")
          print (test paz' "m")
          print (test paz' "")



-- bracketing expressions
pParens p = id <$ pSym '(' <*> p <* pSym ')'
pBracks p = id <$ pSym '[' <*> p <* pSym ']'
pCurlys p = id <$ pSym '{' <*> p <* pSym '}'

-- parsing numbers
pDigit = pSym ('0', '9')
pDigitAsInt = digit2Int <$> pDigit 
pNatural = foldl (\a b -> a * 10 + b ) 0 <$> pList1 pDigitAsInt
digit2Int a =  ord a - ord '0'

-- parsing letters and identifiers
pLower  = pSym ('a','z')
pUpper  = pSym ('A','Z')
pLetter = pUpper <|> pLower
pVarId  = (:) <$> pLower <*> pList pIdChar
pConId  = (:) <$> pUpper <*> pList pIdChar
pIdChar = pLower <|> pUpper <|> pDigit <|> pAnySym "='"


-- running the parser; if complete input accepted return the result else fail with reporting unconsumed tokens
run :: forall t. P_m (Str Char) t -> String -> t
run p i = do let (a,b) = exec p i
             if null b then a else error (show b)

exec :: P_m (Str Char) b -> String -> (b, [Error Char Char Int])
exec p inp = parse ( (,) <$> p <*> pEnd) (listToStr inp)


-- Testing
pTest_MS :: P_m (Str Char) Char
pTest_MS = id <$ pSym 'u' <*> pSym '2'

pOp (c, op) = op <$ pSym c

sepBy p op = pChainl op p
expr    = term   `sepBy` (pOp ('+', (+)) <|> pOp ('-', (-)))
term    = factor `sepBy` pOp ('*' , (*))
factor  = pNatural <|> pSym '(' *> expr <* pSym ')'

rune ::  String -> IO ()
rune i = do let (a,b) = exec expr i
            if null b then  print ("Result: " ++ show a)
                      else do print b
                              print ("Result: " ++ show a)