
module LOAG.Common where

import qualified Data.Array as A
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Set as S
import qualified Data.Sequence as Seq
import Data.Maybe (isNothing)
import Data.STRef
import Data.Array.ST
import Data.List (intercalate, foldl', nub)
import Data.Tuple (swap)
import CommonTypes
import Control.Arrow
import Control.Monad.ST
import Control.Monad (forM, when, forM_, forM_, foldM)

import AbstractSyntax
import LOAG.Graphs

data Ag = Ag    (Int,Int)   -- attribute  range
                (Int,Int)   -- occurrence range
                [Edge]      -- direct dependencies
                [Nt]        -- non-terminals
data Nt = Nt String 
                -- TODO can remove instances from next 2?
                [(Edge,[Edge])] -- direct dps from inh -> syn + instances
                [(Edge,[Edge])] -- direct dps from syn -> inh + instances
                -- inh attributes with direction and instances
                [(Vertex,[Vertex],Direction)]
                -- syn attributes with direction and instances
                [(Vertex,[Vertex],Direction)]
                [Pr]            -- productions of this Nt
    deriving (Show)
data Pr = Pr    PLabel
                [Edge]          -- direct dependencies between fields
                [(Edge,Edge,Bool)] -- all siblings pairs, with generalised version, and boolean that denotes whether if it is an edge of LHS
                [Fd]            -- the fields of this production, including lhs
    deriving (Show)
data Fd = Fd    String          -- field name
                String          -- type of the field
                [(Vertex,Vertex)]        -- inherited atts (gen, inst)
                [(Vertex,Vertex)]        -- synthesized atts (gen, inst)
    deriving (Show)

type Attrs = [Attr]
data Attr  = Attr String Direction MyType
    deriving (Show, Eq, Ord)
data Direction = Inh | AnyDir | Syn
    deriving (Show, Ord, Enum)


foldM' :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
foldM' _ a [] = return a
foldM' f a (x:xs) = f a x >>= \fax -> fax `seq` foldM f fax xs

modifyArray r k f = do
    v <- readArray r k
    writeArray r k (f v)

setConcatMap f = S.foldr (S.union . f) S.empty
isLoc (MyOccurrence (_,f) _) = f == "loc" || f == "inst" -- transparent occr ? 

instance Eq Direction where
    Inh == Syn = False
    Syn == Inh = False
    _ == _     = True

data MyType = TyInt
            | TyBool
            | TyString
            | TyData String
            | TyLit String
            | TyArr MyType MyType
            | NoType  -- the empty set of values (no members)
            | AnyType -- the set of all values (union of all types)

type SchedRef s = (STArray s Vertex (Maybe Int),ThreadRef s)
type AttrAssRef s = STArray s Vertex (Maybe Int)
type ThreadRef s = STRef s InterfaceRes
-- production is identified by its name and its parent non-terminal
type PLabel = (MyType,String) 
type FLabel = String -- field label
-- attribute is identified by its name and its direction
type ALabel = (String, Direction)
type AI_N   = M.Map MyType MyAttributes
type AS_N   = M.Map MyType MyAttributes
type A_N    = M.Map MyType MyAttributes
type A_P    = M.Map PLabel MyOccurrences
-- Get the (data)type of a certain child at a certain production
type FTY    = M.Map (PLabel, FLabel) MyType
-- Get the fields corresponding to a certain type
type TYFS   = M.Map MyType [(PLabel, FLabel)]
-- the definition of given occ uses these occs
type SF_P   = M.Map MyOccurrence (S.Set MyOccurrence) 
type PMP    = M.Map Int MyOccurrence
type PMP_R  = M.Map MyOccurrence Int
type NMP    = M.Map Int MyAttribute
type NMP_R  = M.Map MyAttribute Int
type FMap   = M.Map (PLabel,FLabel) (S.Set MyOccurrence, S.Set MyOccurrence)
type FsInP  = M.Map PLabel [(PLabel, FLabel)]
type LOAGRes =  ( Maybe TDPRes 
                , InterfaceRes
                , ADSRes)
type VisCount= (Int, Int, Float)
type ADSRes  = [Edge]
type TDPRes  = A.Array Vertex Vertices --M.Map PLabel TDPGraph
type TDPGraph = (IM.IntMap Vertices, IM.IntMap Vertices) 
type InterfaceRes = M.Map String (IM.IntMap [Vertex])
type HOMap   = M.Map PLabel (S.Set FLabel) 

findWithErr :: (Ord k, Show k, Show a) => M.Map k a -> String -> k -> a
findWithErr m err k = maybe (error err) id $ M.lookup k m
findWithErr' m err k= maybe (error err) id $ IM.lookup k m

-- Defining the MyAttribute (attribute at non-terimal
-- and the MyOccurrences (attribute at a production)
type MyAttributes = [MyAttribute]
data MyAttribute  = MyAttribute {typeOf :: MyType, alab :: ALabel}
    deriving (Ord, Eq)
(<.>)         = MyAttribute
infixl 7 <.>
instance Show MyAttribute where
    show (MyAttribute t a) = show t ++ "<.>" ++ show a

type MyOccurrences = [MyOccurrence]
data MyOccurrence = MyOccurrence {argsOf :: (PLabel, FLabel), attr :: ALabel}
    deriving (Ord, Eq)
(>.<)         = MyOccurrence
infixl 8 >.<
instance Show MyOccurrence where
    show (MyOccurrence ((t,p),f) a) = 
        intercalate "." [show t,p,f] ++ "."++ show a

dirOfOcc :: MyOccurrence -> Direction
dirOfOcc = snd . attr

handOut :: (PLabel, FLabel) -> MyAttribute -> MyOccurrence
handOut p = (p >.<) . alab

handAllOut :: (PLabel, FLabel) -> MyAttributes -> MyOccurrences
handAllOut p os = map (handOut p) os

map2F  :: (Ord a)        => M.Map a [b] -> a -> [b]
map2F m a = case M.lookup a m of
              Nothing -> []
              Just bs -> bs

map2F'  :: (Ord a)        => M.Map a (S.Set b) -> a -> (S.Set b)
map2F' m a = case M.lookup a m of
               Nothing -> S.empty
               Just bs -> bs

flipDir :: Direction -> Direction
flipDir Syn = Inh
flipDir Inh = Syn

-- creates all pairs of elements such that no equal elements end up in a pair
-- and considering only one direction
pairs :: [a] -> [(a,a)]
pairs [] = []
pairs (x:xs) = map ((,) x) xs ++ pairs xs

toMyTy :: Type -> MyType
toMyTy (Haskell str) = TyLit str
toMyTy (NT id _ _ )  = TyData $ getName id
toMyTy Self          = error "Type Self in phase 3"

fromMyTy :: MyType -> Type
fromMyTy (TyLit str) = (Haskell str)
fromMyTy (TyData id) = NT (identifier id) [] False

toMyAttr :: Direction -> MyType -> Attributes -> MyAttributes
toMyAttr d dty = M.foldrWithKey 
                    (\ident ty as -> dty <.> (getName ident,d):as) []

completing :: FrGraph -> SchedRef s -> [Nt] -> ST s InterfaceRes
completing ids sched nts = do   
    ims <- forM nts $ completingN ids (fst sched)
    let threads = (M.fromList ims)
    writeSTRef (snd sched) threads
    return $ threads 

completingN :: FrGraph -> AttrAssRef s -> Nt -> 
                    ST s ((String, IM.IntMap [Vertex]))
completingN ids@(idsf, idst) schedA
                (Nt nt_id _ _ inhs syns _) = do
    schedS <- newSTRef IM.empty
    let attrs = inhs ++ syns
        dty = TyData nt_id
        assign (attr,_,dAttr) = do
         let succs = idsf A.! attr
         assigned <- freeze schedA
         when (isNothing $ assigned A.! attr) $ do
           case IS.toList succs of 
             [] ->wrap_up attr(if Syn==dAttr then 1 else 2)
             ss ->case selMax $ map (id&&&(assigned A.!)) ss of
                      Nothing -> return ()
                      Just (a,mx) -> do
                          let dA | even mx   = Inh
                                 | otherwise = Syn
                          wrap_up attr (if dA == dAttr 
                                          then mx else mx+1)
        wrap_up attr k = do
         modifySTRef schedS (IM.insertWith (++) k [attr])
         writeArray schedA attr (Just k)
         -- TODO assign only predecessors
         -- tmp fix by lack of direction for preds
         forM_ attrs assign
        selMax :: [(Vertex, Maybe Int)] -> Maybe (Vertex, Int)
        selMax [(v,mi)] = fmap ((,) v) mi
        selMax (x:xs)  = case x of 
                          (a', Nothing) -> Nothing
                          (a', Just i') -> 
                            case selMax xs of 
                             Nothing -> Nothing
                             Just (a,i)  -> 
                                case compare i i' of
                                 LT -> Just (a',i')
                                 _  -> Just (a,i)
    --make sure all are assigned
    case attrs of
      [] -> return (nt_id, IM.fromList [(1,[]),(2,[])])
      as -> forM_ as assign >> readSTRef schedS >>= return . ((,) nt_id)

fetchEdges :: FrGraph -> InterfaceRes -> [Nt] -> ([Edge],[Edge])
fetchEdges ids threads nts =
    let ivdNs = map (fetchEdgesN ids threads) nts
    in (concat *** concat) $ unzip ivdNs

fetchEdgesN :: FrGraph -> InterfaceRes -> Nt
                    -> ([Edge],[Edge])
fetchEdgesN (idsf, idst) threads 
        (Nt nt_id _ _ _ _ _) =
    let sched = findWithErr threads "schedule err" nt_id
        mx    = if IM.null sched then 0 else fst $ IM.findMax sched
        findK 0 = []
        findK k = (maybe [] id $ IM.lookup k sched) ++ findK (k-1)
        ivd =  [ (f,t) | k <- [2..mx]
                   , f <- maybe [] id $ IM.lookup k sched
                   , t <- findK (k-1)]
     in (ivd, [ (f, t) | (f, t) <- ivd
                          , not $ IS.member t (idsf A.! f) ])

instance Show MyType where
    show TyInt        = "Int"
    show TyBool       = "Bool"
    show TyString     = "String"
    show (TyData t)   = t
    show (TyLit t)    = show t
    show (TyArr a b)  = show a ++ " -> (" ++ show b ++ ")" 
    show NoType       = error "Trying to show NoType"
    show AnyType      = "AnyType"

-- | Instance for Eq and Ord are required to make sure that AnyType
-- | Equals every other type in every other situation
instance Eq MyType where
    TyInt       == TyInt        = True
    TyBool      == TyBool       = True
    TyString    == TyString     = True
    TyData n    == TyData n'    = n == n'
    TyLit ty    == TyLit ty'    = ty == ty'
    TyArr l r   == TyArr l' r'  = l == l' && r == r'
    NoType      == _            = False
    _           == NoType       = False
    AnyType     == _            = True
    _           == AnyType      = True
    _           == _            = False

instance Ord MyType where
    NoType `compare` _          = LT
    _   `compare` NoType        = GT
    AnyType `compare` _         = EQ
    _   `compare` AnyType       = EQ
    TyInt `compare` TyInt       = EQ
    TyInt `compare` _           = LT
    TyBool `compare` TyInt      = GT
    TyBool `compare` TyBool     = EQ
    TyBool `compare` _          = LT
    TyString `compare` TyInt    = GT
    TyString `compare` TyBool   = GT
    TyString `compare` TyString = EQ
    TyString `compare` _        = LT
    TyData _ `compare` TyInt    = GT
    TyData _ `compare` TyBool   = GT
    TyData _ `compare` TyString = GT
    TyData a `compare` TyData b = compare a b
    TyData _ `compare` _        = LT
    TyLit  a `compare` TyLit b  = compare a b
    TyLit  _ `compare` TyArr _ _= LT    
    TyLit  _ `compare` _        = GT
    TyArr a a' `compare` TyArr b b' = 
        case compare a b of
            LT -> LT
            GT -> GT
            EQ -> compare a' b'
    TyArr _ _ `compare` _       = GT

