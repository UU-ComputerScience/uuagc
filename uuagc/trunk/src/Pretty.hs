{-# OPTIONS_GHC -fglasgow-exts #-}
-- Layer on top of the original UU.Pretty class
module Pretty
  ( (>|<)
  , (>-<)
  , (>#<)
  , text
  , empty
  , indent
  , pp_block
  , hlist
  , vlist
  , vlist_sep
  , fill
  , pp_parens
  , hv_sp
  , ppWithLineNr
  , disp
  , PP_Doc
  , PP (..)
  ) where

import qualified UU.Pretty as U
import Data.List(intersperse)


--
-- Type classes and data types that mimic UU.Pretty
--

class Show a => PP a where
  pp :: a -> PP_Doc
  pp = text . show

  ppList :: [a] -> PP_Doc
  ppList as = if null as
              then empty
              else foldr (>|<) empty . map pp $ as

instance PP PP_Doc where
  pp = id

instance Show PP_Doc where
  show (PP_Doc f) = let (doc, _, _) = f 1
                    in show doc

instance PP String where
  pp = text

instance PP Char where
  pp = text . (\c -> [c])


newtype PP_Doc = PP_Doc (Int -> (U.PP_Doc, Int, Bool))

fromInternal :: PP a => a -> Int -> (U.PP_Doc, Int, Bool)
fromInternal x l
  = let (PP_Doc f) = pp x
    in f l


--
-- Layer on top of primitive combinators
--

infixr 3 >|<
infixr 2 >-<
infixr 3 >#<

(>|<) :: (PP a, PP b) => a -> b -> PP_Doc
x >|< y
  = PP_Doc $
      \l -> let (p, n, s) = fromInternal x l
                (q, m, t) = fromInternal y n
            in  (p U.>|< q, m, s && t)

(>-<) :: (PP a, PP b) => a -> b -> PP_Doc
x >-< y
  = PP_Doc $
      \l -> let (p, n, s) = fromInternal x l
                (q, m, t) = fromInternal y (if s then n else n+1)
             in (p U.>-< q, if t then m-1 else m, s && t)

text :: String -> PP_Doc
text s
  = let ls = lines s
        ls' | null ls   = [""]
            | otherwise = ls
    in vlist (map ppLine ls')
  where
    ppLine txt
      = PP_Doc $
          \l -> (U.text txt, l, False)

empty :: PP_Doc
empty
  = PP_Doc $
      \l -> (U.empty, l, True)


--
-- Layer on top of the higher-level combinators
-- (shameless copy of their definition)
--

(>#<) :: (PP a, PP b) => a -> b -> PP_Doc
l >#< r = l >|< " " >|< r

hlist :: PP a => [a] -> PP_Doc
hlist = foldr (>|<) empty

vlist :: PP a => [a] -> PP_Doc
vlist = foldr (>-<) empty

vlist_sep :: (PP a, PP b) => a -> [b] -> PP_Doc
vlist_sep sep lst
  = vlist (intersperse (pp sep) (map pp lst))

pp_block:: (PP a, PP b, PP c) => a -> b -> c -> [PP_Doc] -> PP_Doc
pp_block o c s as = pp o >|< hlist (intersperse (pp s) as) >|< pp c

pp_parens :: PP a => a -> PP_Doc
pp_parens p = '(' >|< p >|< ')'


--
-- Layer on top of the higher-level "smart" combinators
-- (dump alternative implementation)
--

indent :: PP a => Int -> a -> PP_Doc
indent n p = foldr (>#<) (pp p) (replicate n empty)

fill :: PP a => [a] -> PP_Doc
fill = foldr (>#<) empty

hv_sp :: PP a => [a] -> PP_Doc
hv_sp = foldr (>#<) empty


-- Special combinator that exposes the line number
ppWithLineNr :: (Int -> PP_Doc) -> PP_Doc
ppWithLineNr f
  = PP_Doc $
      \l -> fromInternal (f l) l

-- Render function
disp :: PP a => a -> Int -> String -> String
disp p n = let (doc, _, _) = fromInternal (pp p) 1
            in U.disp doc n

