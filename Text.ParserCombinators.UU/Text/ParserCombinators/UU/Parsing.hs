
{-# LANGUAGE  RankNTypes, 
              GADTs,
              MultiParamTypeClasses,
              FunctionalDependencies, 
              FlexibleInstances, 
              FlexibleContexts, 
              UndecidableInstances,
              NoMonomorphismRestriction#-}



 
module Text.ParserCombinators.UU.Parsing where
import Prelude hiding (fail)
import Char
import Debug.Trace
import Maybe

infixl  5  <*>, <*, *>
infixr  3  <|> 
infixl  5  <$>, <$

ap f a = f a 

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%% Classes     %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

class  Applicative p where
  (<*>)     ::   p (b -> a)  -> p b   ->   p a
  (<|>)     ::   p a         -> p a   ->   p a
  (<$>)     ::   (b -> a)    -> p b   ->   p a
  pReturn   ::   a                    ->   p a
  pFail     ::                             p a
  f <$> p   =  pReturn f <*> p

instance Applicative p => Functor p where
  fmap = (<$>)

class  Symbol p  symbol token | symbol -> token where
  pSym  ::  symbol -> p token

type Strings = [String]

type Cost = Int
type Progress = Int

class  Provides state symbol token | state symbol -> token  where
       splitState   ::  symbol -> (token -> state  -> Steps a) -> state -> Steps a

class Eof state where
       eof          ::  state   -> Bool
       deleteAtEnd  ::  state   -> Maybe (Cost, state)

class  Parser p  where
       parse  ::   Eof state => p state a -> state -> a

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%% Steps      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

data  Steps   a  where
      Step   ::              Progress       ->  Steps a                                -> Steps   a
      Fail   ::              [String]       ->  [[String]  ->       (Int, Steps   a)]  -> Steps   a
      Apply  ::  forall b.   (b -> a)       ->  Steps   b                              -> Steps   a
      End_h  ::              ([a] , [a] -> Steps r)        ->  Steps   (a,r)           -> Steps   (a, r)
      End_f  ::              [Steps   a]   ->  Steps   a                               -> Steps   a

fail        =  Fail [] [const ((0, fail))]
noAlts      =  Fail [] []

eval :: Steps   a      ->  a
eval (Step  _    l)     =   eval l
eval (Fail   ss  ls  )  =   eval (getCheapest 3 [f ss | f <- ls]) 
eval (Apply  f   l   )  =   f (eval l)
eval (End_f   _  _   )  =   error "dangling End_fconstructor"
eval (End_h   _  _   )  =   error "dangling End_h constructor"

push    :: v -> Steps   r -> Steps   (v, r)
push v  =  Apply (\ r -> (v, r))
apply   :: Steps (b -> a, (b, r)) -> Steps (a, r)
apply   =  Apply (\(b2a, ~(b, r)) -> (b2a b, r))  

norm ::  Steps a ->  Steps   a
norm     (Apply f (Step   p    l  ))   =   Step p (Apply f l)
norm     (Apply f (Fail   ss   ls ))   =   Fail ss (applyFail (Apply f) ls)
norm     (Apply f (Apply  g    l  ))   =   norm (Apply (f.g) l)
norm     (Apply f (End_f  ss   l  ))   =   End_f (map (Apply f) ss) (Apply f l)
norm     (Apply f (End_h  _    _  ))   =   error "Apply before End_h"
norm     steps                         =   steps

applyFail f  = map (\ g -> \ ex -> let (c, l) =  g ex in  (c, f l))

best :: Steps   a -> Steps   a -> Steps   a
x `best` y =   norm x `best'` norm y

best' :: Steps   b -> Steps   b -> Steps   b
Fail  sl  ll     `best'`  Fail  sr rr     =   Fail (sl ++ sr) (ll++rr)
Fail  _   _      `best'`  r               =   r
l                `best'`  Fail  _  _      =   l
Step  n   l      `best'`  Step  m  r
    | n == m                              =   Step n (l `best'` r)     
    | n < m                               =   Step n (l  `best'`  Step (m - n)  r)
    | n > m                               =   Step m (Step (n - m)  l  `best'` r)
End_f  as  l            `best'`  End_f  bs r     =   End_f (as++bs)  (l `best` r)
End_f  as  l            `best'`  r               =   End_f as        (l `best` r)
l                       `best'`  End_f  bs r     =   End_f bs        (l `best` r)
End_h  (as, k_h_st)  l  `best'`  End_h  (bs, _) r     =   End_h (as++bs, k_h_st)  (l `best` r)
End_h  as  l            `best'`  r               =   End_h as (l `best` r)
l                       `best'`  End_h  bs r     =   End_h bs (l `best` r)
l                       `best'`  r               =   l `best` r 

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%% History     %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-- do not change into data !!
newtype  P_h    st  a =  P_h  (forall r . (a  -> st -> Steps r)  -> st -> Steps r)
unP_h (P_h p) = p

instance   Applicative (P_h  state) where
  (P_h p) <*> (P_h q)  =  P_h  (\  k -> p (\ f -> q (\ a -> k (f a)))) 
  (P_h p) <|> (P_h q)  =  P_h  (\  k inp  -> p k inp `best` q k inp) 
  f  <$> (P_h p)       =  P_h  (\  k -> p (\a -> k (f a))) 
  pFail                =  P_h  (\  k -> const noAlts) 
  pReturn a            =  P_h  (\  k -> k a)


instance  ( Provides state symbol token) => Symbol (P_h  state) symbol token where
  pSym a =  P_h (splitState a)

data Id a = Id a deriving Show

instance   Parser P_h  where
  parse (P_h p)
   =  fst . eval . p  (\ a rest -> if eof rest then push a fail else error "pEnd missing?") 

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%% Future      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-- do not change into data !!
newtype  P_f st a  = P_f (forall r . (st -> Steps   r) -> st -> Steps   (a, r))
unP_f (P_f p) = p

instance Applicative (P_f st) where
 P_f p  <*>  P_f q  =   P_f ( (apply .) . (p .q))
 P_f p  <|>  P_f q  =   P_f (\ k inp  -> p k inp `best` q k inp)  
 pReturn a          =   P_f ((push a).)
 pFail              =   P_f (\ k inp  -> noAlts)

instance  (Provides state symbol token) =>  Symbol (P_f  state) symbol token where
  pSym a =  P_f (\ k inp-> splitState a (\ t inp' -> push t (k inp')) inp)

instance  Parser P_f  where
  parse (P_f p) =  fst . eval . p (\ rest -> if eof rest then fail else error "pEnd missing")

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%% Monads      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

infixr 1 >>>=
class GenMonad  m_1 m_2 where
   (>>>=) :: m_1 b -> ( b -> m_2  a) -> m_2 a

instance     Monad (P_h  state) 
         =>  GenMonad (P_h  state) (P_h state) where
  (>>>=)  = (>>=) --  the monadic bind defined before

instance GenMonad (P_h  state) (P_f  state) where
  (P_h p)  >>>= pv2q 
           = P_f (\ k st -> p (\ pv st -> unP_f (pv2q pv) k st) st)

newtype P_m state a = P_m (P_h  state a, P_f state a) 
unP_m_h (P_m  (P_h h,  _    ))  =  h
unP_m_f (P_m  (_    ,  P_f f))  =  f

instance  (   Applicative (P_h  st), Applicative (P_f  st)) 
          =>  Applicative (P_m  st) where
 P_m (hp, fp)  <*> P_m ~(hq, fq)   = P_m  (hp <*> hq, fp <*> fq) 
 P_m (hp, fp)  <|> P_m (hq, fq)    = P_m  (hp <|> hq, fp <|> fq)
 pReturn a                         = P_m  (pReturn a, pReturn a) 
 pFail                             = P_m  (pFail,         pFail)       
 
instance  (Provides state symbol token)  => Symbol (P_m state) symbol token where
  pSym a =  P_m (pSym a, pSym a)

instance   Parser P_m  where
  parse (P_m (_, (P_f fp)))  
      =  fst . eval. fp (\ rest -> if eof rest  then fail else error "End_fmissing?") 

instance Applicative (P_h state) => Monad (P_h state) where
  P_h p >>= a2q  = P_h ( \ k -> p (\ a -> unP_h (a2q a) k))
  return     = pReturn

instance Applicative (P_m st) => Monad (P_m st) where
     P_m  (P_h p, _)  >>=  a2q = 
           P_m  (  P_h   (\k -> p (\ a -> unP_m_h (a2q a) k))
                ,  P_f   (\k -> p (\ a -> unP_m_f (a2q a) k))
                )
     return  = pReturn 

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%% Greedy      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

best_gr :: Steps a -> Steps a -> Steps a

l@  (Step _ _)   `best_gr` _  = l
l                `best_gr` r  = l `best` r

class  Greedy p where 
  (<<|>) :: p a -> p a -> p a

instance Greedy (P_h state)  where
  P_h p <<|> P_h q = P_h (\ k st  -> norm (p k st) `best_gr` norm (q k st))

instance Greedy (P_f state)  where
  P_f p <<|> P_f q = P_f (\ k st  -> norm (p k st) `best_gr` norm (q k st))

instance Greedy (P_m state) where
    P_m (hp, fp)  <<|> P_m (hq, fq) = P_m  (hp <<|> hq, fp <<|> fq) 


-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%% Ambiguous   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

class Ambiguous p where
 amb :: p a -> p [a]

instance Ambiguous (P_h state) where
  amb (P_h p) = P_h ( \k ->  removeEnd_h . p (\ a st' -> End_h ([a], \ as -> k as st') noAlts))
removeEnd_h     :: Steps (a, r) -> Steps r
removeEnd_h (Fail  m ls             )  =   Fail m (applyFail removeEnd_h ls)
removeEnd_h (Step  ps l             )  =   Step  ps (removeEnd_h l)
removeEnd_h (Apply f l              )  =   error "not in history parsers"
removeEnd_h (End_h  (as, k_st  ) r  )  =   k_st as `best` removeEnd_h r 


instance Ambiguous (P_f state) where
  amb (P_f p) = P_f (\k inp -> combinevalues . removeEnd_f $ p (\st -> End_f [k st] noAlts) inp)
removeEnd_f      :: Steps r -> Steps [r]
removeEnd_f (Fail m ls)        =   Fail m (applyFail removeEnd_f ls)
removeEnd_f (Step ps l)        =   Step ps (removeEnd_f l)
removeEnd_f (Apply f l)        =   Apply (map' f) (removeEnd_f l)
removeEnd_f (End_f(s:ss) r)    =   Apply  (:(map  eval ss)) s 
                                                 `best`
                                          removeEnd_f r

combinevalues  :: Steps [(a,r)] -> Steps ([a],r)
combinevalues lar           =   Apply (\ lar -> (map fst lar, snd (head lar))) lar
map' f ~(x:xs)              =   f x : map f xs

instance (Ambiguous (P_h state), Ambiguous (P_f state)) => Ambiguous (P_m state) where
  amb  (P_m (hp, fp))  = P_m (amb hp, amb fp)
       
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%% getCheapest  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getCheapest :: Int -> [(Int, Steps a)] -> Steps a 
getCheapest _ [] = error "no correcting alternative found"
getCheapest n l  =  snd $  foldr (\(w,ll) btf@(c, l)
                               ->    if w < c 
                                     then let new = (traverse n ll w c) 
                                          in if new < c then (new, ll) else btf
                                     else btf 
                               )   (maxBound, error "getCheapest") l


traverse :: Int -> Steps a -> Int -> Int -> Int
traverse 0 _                =  \ v c ->  v
traverse n (Step ps l)      =  traverse (n-1) l
traverse n (Apply _ l)      =  traverse n     l
traverse n (Fail m m2ls)    =  \ v c ->  foldr (\ (w,l) c' -> if v + w < c' then traverse (n-1) l (v+w) c'
                                                                            else c'
                                               ) c (map ($m) m2ls)
traverse n (End_h ((a, lf))    r)  =  traverse n (lf a `best` removeEnd_h r)
traverse n (End_f (l      :_)  r)  =  traverse n (l `best` r)   


-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%% pErrors     %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

class state `Stores`  errors where
  getErrors    ::  state   -> (errors, state)

class  p `AsksFor` errors where
  pErrors :: p errors
  pEnd    :: p errors

instance (Eof state, Stores state errors) =>  AsksFor (P_h state) errors where
  pErrors = P_h (\ k inp -> let (errs, inp') = getErrors inp
                            in k errs inp')
  pEnd    = P_h (\ k inp -> let deleterest inp =  case deleteAtEnd inp of
                                                  Nothing -> let (finalerrors, finalstate) = getErrors inp
                                                             in k  finalerrors finalstate
                                                  Just (i, inp') -> Fail []  [const ((i,  deleterest inp'))]
                             in deleterest inp
                )

instance (Eof state, Stores state errors) => AsksFor (P_f state) errors where
  pErrors = P_f (\ k   inp -> let (errs, inp') = getErrors inp
                              in push errs (k inp'))
  pEnd    = P_f (\ k   inp -> let deleterest inp =  case deleteAtEnd inp of
                                                    Nothing -> let (finalerrors, finalstate) = getErrors inp
                                                               in push finalerrors (k finalstate)
                                                    Just (i, inp') -> Fail [] [const ((i, deleterest inp'))]
                              in deleterest inp
                )

instance  (state `Stores` errors, Eof state) => AsksFor (P_m state)  errors where
  pErrors   = P_m  (pErrors,  pErrors)
  pEnd      = P_m  (pEnd,     pEnd)

{-
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%% Microsteps  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


class MicroStep result where
  microstep :: result a -> result a

instance MicroStep Steps where
   microstep steps = Micro steps

class Micro p where
  micro :: p a -> p a

instance  Micro (P_f  st) where
  micro (P_f p) = P_f (\k st -> microstep ( p k st ) )
-}

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%% State Change          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

class Switch p where
  pSwitch :: (st1 -> (st2, st2 -> st1)) -> p st2 a -> p st1 a

instance Switch P_h where
  pSwitch split (P_h p) = P_h  (\ k st1 ->  let (st2, back) = split st1
                                            in p (\ a st2' -> k a (back st2')) st2)

instance Switch P_f where
  pSwitch split (P_f p) = P_f  (\k st1 ->  let (st2, back) = split st1
                                           in p (\st2' -> k (back st2')) st2)

instance Switch P_m where
  pSwitch split (P_m (p, q)) = P_m (pSwitch split p, pSwitch split q)

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%% Recognisers           %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

newtype  R st a  = R (forall r . (st -> Steps   r) -> st -> Steps r)
unR (R p) = p

instance Applicative (R st) where
 R p  <*>  R q   =   R (p.q)
 R p  <|>  R q   =   R (\ k inp  -> p k inp `best` q k inp)  
 pReturn a       =   R (id)
 pFail           =   R (\ k inp  -> noAlts)

instance  (Provides state symbol token) =>  Symbol (R  state) symbol token where
  pSym a =  R (\k inp ->  splitState a (\ v inp' -> k inp') inp) 

{-
class  ExtApplicative p  where
  (<*)      ::  p st a          -> R st b   ->   p st a
  (*>)      ::  R st b          -> p st a   ->   p st a
  (<$)      ::  a               -> R st b   ->   p st a

instance ExtApplicative P_h  where
  P_h p <* R r     = P_h ( p. (r.)) 
  R   r *> P_h p   = P_h ( r .p   )
  f     <$  R r   = P_h ( r . ($f))
-}

class  Applicative p => ExtApplicative p st | p -> st where
  (<*)      ::  p  a          -> R st b   ->   p  a
  (*>)      ::  R st b        -> p    a   ->   p  a
  (<$)      ::  a             -> R st b   ->   p  a

instance ExtApplicative (P_h st) st where
  P_h p <* R r     = P_h ( p. (r.)) 
  R   r *> P_h p   = P_h ( r .p   )
  f     <$  R r    = P_h ( r . ($f))

instance ExtApplicative (P_f st) st where
  P_f p <* R r     = P_f (\ k st -> p (r k) st)
  R   r *> P_f p   = P_f (\ k st -> r (p k) st)
  f     <$  R r    = P_f (\ k st -> push f (r k st))

instance  (ExtApplicative (P_h  st) st, ExtApplicative (P_f  st) st )
          =>  ExtApplicative (P_m  st) st where
 P_m (hp, fp)  <*  r               = P_m  (hp <* r, fp <* r) 
 r             *>  P_m (hq, fq)    = P_m  (r  *> hq , r *> fq)
 f             <$  r               = P_m  (f  <$ r, f <$ r)       
 

