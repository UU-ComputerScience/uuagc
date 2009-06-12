module Text.ParserCombinators.UU.Derived where

import Text.ParserCombinators.UU.Core

infixl 4  <??>
infixl 2 `opt`

-- | Optionally recognize parser 'p'.
-- 
-- If 'p' can be recognized, the return value of 'p' is used. Otherwise,
-- the value 'v' is used. Note that opt is greedy, if you do not want
-- this use @... <|> pure v@  instead. Furthermore, 'p' should not
-- recognise the empty string, since this would make your parser ambiguous!!
opt ::  (Parser p) => p a -> a -> p a
p `opt` v       =  p <<|> pure v  
                                                
(<$$>)    :: (Parser p) => (a -> b -> c) -> p b -> p (a -> c)
f <$$> p  =  flip f <$> p

(<??>) :: (Parser p) => p a -> p (a -> a) -> p a
p <??> q        = p <**> (q `opt` id)

-- | This can be used to parse 'x' surrounded by 'l' and 'r'.
-- 
-- Example:
--
-- > pParens = pPacked pOParen pCParen
pPacked :: (Parser p) => R (State p) b1 -> R (State p) b2 -> p a -> p a
pPacked l r x   =   l *>  x <*   r

-- =======================================================================================
-- ===== Iterating ps ===============================================================
-- =======================================================================================
pFoldr    :: (Parser p) => (a -> a1 -> a1, a1) -> p a -> p a1
pFoldr_ng :: (Parser p) => (a -> a1 -> a1, a1) -> p a -> p a1
pFoldr         alg@(op,e)     p = pfm where pfm = (op <$> p <*> pfm) `opt` e
pFoldr_ng      alg@(op,e)     p = pfm where pfm = (op <$> p <*> pfm)  <|> pure e


pFoldr1    :: (Parser p) => (v -> b -> b, b) -> p v -> p b
pFoldr1_ng :: (Parser p) => (v -> b -> b, b) -> p v -> p b
pFoldr1        alg@(op,e)     p = op <$> p <*> pFoldr  alg p
pFoldr1_ng     alg@(op,e)     p = op <$> p <*> pFoldr_ng  alg p

pFoldrSep    :: (Parser p) => (v -> b -> b, b) -> R (State p) a -> p v -> p b
pFoldrSep_ng :: (Parser p) => (v -> b -> b, b) -> R (State p) a -> p v -> p b
pFoldrSep      alg@(op,e) sep p = op <$> p <*> pFoldr    alg sepp `opt` e
                                  where sepp = sep *> p
pFoldrSep_ng   alg@(op,e) sep p = op <$> p <*> pFoldr_ng alg sepp <|>  pure e
                                  where sepp = sep *> p

pFoldr1Sep    :: (Parser p) => (a -> b -> b, b) -> R (State p) a1 -> p a -> p b
pFoldr1Sep_ng :: (Parser p) => (a -> b -> b, b) -> R (State p) a1 -> p a -> p b
pFoldr1Sep     alg@(op,e) sep p = pfm where pfm = op <$> p <*> pFoldr    alg (sep *> p)
pFoldr1Sep_ng  alg@(op,e) sep p = pfm where pfm = op <$> p <*> pFoldr_ng alg (sep *> p)

list_alg :: (a -> [a] -> [a], [a1])
list_alg = ((:), [])

pList    ::  (Parser p) =>  p a -> p [a]
pList_ng ::  (Parser p) =>  p a -> p [a]
pList           p = pFoldr        list_alg   p
pList_ng        p = pFoldr_ng     list_alg   p

pList1    ::  (Parser p) => p a -> p [a]
pList1_ng ::  (Parser p) => p a -> p [a]
pList1          p = pFoldr1       list_alg   p
pList1_ng       p = pFoldr1_ng    list_alg   p


pListSep    :: (Parser p) => R (State p) a1 -> p a -> p [a]
pListSep_ng :: (Parser p) => R (State p) a1 -> p a -> p [a]
pListSep      s p = pFoldrSep     list_alg s p
pListSep_ng   s p = pFoldrSep_ng  list_alg s p

pList1Sep    :: (Parser p) => R (State p) a1 -> p a -> p [a]
pList1Sep_ng :: (Parser p) => R (State p) a1 -> p a -> p [a]
pList1Sep     s p = pFoldr1Sep    list_alg s p
pList1Sep_ng  s p = pFoldr1Sep_ng list_alg s p

pChainr    :: (Parser p) => p (c -> c -> c) -> p c -> p c
pChainr_ng :: (Parser p) => p (c -> c -> c) -> p c -> p c
pChainr    op x    =  r where r = x <??> (flip <$> op <*> r)
pChainr_ng op x    =  r where r = x <**> ((flip <$> op <*> r)  <|> pure id)

pChainl    :: (Parser p) => p (c -> c -> c) -> p c -> p c
pChainl_ng :: (Parser p) => p (c -> c -> c) -> p c -> p c
pChainl   op x    = f <$> x <*> pList (flip <$> op <*> x) 
                    where  f x [] = x
                           f x (func:rest) = f (func x) rest
pChainl_ng op x    = f <$> x <*> pList_ng (flip <$> op <*> x) 
                     where f x [] = x
                           f x (func:rest) = f (func x) rest

-- | Parses using any of the parsers in the list 'l'.

pAny :: (Alternative p) =>(a -> p a1) -> [a] -> p a1
pAny  f l =  foldr (<|>) empty (map f l)

-- | Parses any of the symbols in 'l'.
pAnySym :: (Alternative p, Symbol p s s) =>[s] -> p s
pAnySym = pAny pSym 

pToken :: (Applicative p, Symbol p s s) => [s] -> p [s]
pToken []     = pure []
pToken (a:as) = (:) <$> pSym a <*> pToken as

pAnyToken :: (Parser p, Symbol p s s) => [[s]] -> p [s]
pAnyToken = pAny pToken

