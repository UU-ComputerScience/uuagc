
module LOAG.Solver.MiniSat ( 
    newSolvable,-- IO SatContainer 
    newLit,     -- Sat -> IO MyVar
    satValues,  -- Sat -> IO (Int, Int)
    addClause,  -- Sat -> [MyVar] -> IO ()
    satsolve,   -- Sat -> IO Bool
    value,      -- Sat -> IO (Maybe Bool)
    fixed,       -- Sat -> IO Bool
    MyVar(..),
    Sat,
    varnot,
    VarMap,
    Mini.conflict
    ) where

import LOAG.Graphs
import Control.Monad
import Data.Maybe
import qualified Data.Map as M
import qualified MiniSat as Mini

type Sat = Mini.Solver
type VarMap = M.Map Edge MyVar

data MyVar = Var Mini.Lit 
           | VarTrue
           | VarFalse
           | VarNot MyVar
    deriving (Show, Eq)

varnot :: MyVar -> MyVar
varnot v@(Var i) = Var (Mini.neg i)
varnot (VarNot v)= v
varnot VarFalse  = VarTrue
varnot VarTrue   = VarFalse


newSolvable :: IO Mini.Solver
newSolvable = do    sat <-  Mini.newSolver
                    Mini.eliminate sat True
                    return sat

newLit      :: Sat -> IO MyVar
newLit sat =
 do p <- Mini.newLit sat
    return (Var p)

satValues   :: Sat -> IO (Int,Int)
satValues sat = 
    do  v <- Mini.minisat_num_vars sat
        p <- Mini.minisat_num_clauses sat
        return (v,p)

addClause   :: Sat -> [MyVar] -> IO Bool
addClause sat [] = return True 
addClause sat es = do
    if satisfied
      then return True
      else case fes of
             []  -> error "unsatisfiable"
             fes -> Mini.addClause sat $ map toMini fes
 where  
        satisfied = any (True ==?) es
        fes       = filter (not . (False ==?)) es
        b ==? v = case v of 
                    VarTrue  -> b
                    VarFalse -> not b
                    _       -> False
        toMini :: MyVar -> Mini.Lit
        toMini (Var v)    = v
        toMini (VarNot v) = Mini.neg (toMini v)
        _                 = error "incorrect clause filtering"

satsolve :: Sat -> [MyVar] -> IO Bool
satsolve sat = Mini.solve sat . map extract

extract :: MyVar -> Mini.Lit
extract VarTrue     = error "cannot extract True"
extract VarFalse    = error "cannot extract False"
extract (VarNot v)  = Mini.neg (extract v)
extract (Var l)     = l

value :: Sat -> MyVar -> IO (Maybe Bool)
value _ VarTrue     = return $ Just True
value _ VarFalse    = return $ Just False
value sat (VarNot v)  = value sat v >>= return . fmap not
value sat (Var v)     = Mini.modelValue sat v

fixed :: Sat -> MyVar -> IO Bool
fixed _ VarTrue     = return $ True
fixed _ VarFalse    = return $ True 
fixed sat (VarNot v)= fixed sat v
fixed sat (Var l)   = Mini.value sat l >>= return . isJust
