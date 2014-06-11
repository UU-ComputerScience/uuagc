{-# LANGUAGE FlexibleInstances #-}

-- Module for containing results in LOAG tests
module LOAG.Result where

import Control.Monad (MonadPlus(..))
import Control.Monad.Trans (lift, MonadTrans(..))
import Control.Monad.State (MonadState(..))
import Control.Monad.ST
import ErrorMessages as Err

import LOAG.Graphs

type LOAG s a = ResultT (ST s) a

data Result a = Give  a     
                -- the edge that caused the cyclep of ctype 
              | Cycle Edge Cycle CType
              | Limit
              | NotLOAG
    deriving (Show)            

fromGive :: Result a -> a
fromGive (Give a) = a
fromGive _        = error "fromGive"

data CType = T1 | T2 
           | T3 [Edge] -- completing edges from which to select candidates
    deriving (Show)
-- | Inspired by ErrorT
newtype ResultT m a = Result { runResult :: m (Result a) }

instance (Monad m) => Monad (ResultT m) where
    return               = Result . return . Give
    (>>=) rt f = Result $ do
        ma <- runResult rt
        case ma of 
            Give a      -> runResult (f a)
            Cycle e c t -> return $ Cycle e c t 
            Limit       -> return $ Limit
            NotLOAG     -> return $ NotLOAG

instance MonadTrans ResultT where
    lift m = Result $ m >>= return . Give
instance MonadState s (ResultT (ST s)) where
    get = get
    put = put

instance (Monad m) => MonadPlus (ResultT m) where
    mzero = Result $ return NotLOAG
    mplus a b = Result $ do
        ma <- runResult a 
        case ma of
            Give a  -> return $ Give a
            f       -> do   mb <- runResult b
                            case mb of 
                              Give b  -> return $ Give b
                              _       -> return f

-- | Return an error (from detecting a cycle) in the ResultT monad
throwCycle :: (Monad m) => Edge -> Cycle -> CType -> ResultT m a
throwCycle e c t = Result $ return $ Cycle e c t

throwNotLOAG :: (Monad m) => ResultT m a
throwNotLOAG = Result $ return NotLOAG
