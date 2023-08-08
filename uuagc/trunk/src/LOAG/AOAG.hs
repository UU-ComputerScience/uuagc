{-# LANGUAGE Rank2Types #-}

module LOAG.AOAG where

import LOAG.Common
import LOAG.Graphs
import LOAG.Rep

import AbstractSyntax
import CommonTypes
import Control.Arrow ((&&&), (***))
import Control.Monad (forM, forM_, MonadPlus(..), when, unless)
import Control.Monad.ST
import Control.Monad.State (MonadState(..))
import Data.Maybe (fromMaybe, catMaybes, fromJust, isNothing)
import Data.List (elemIndex, foldl', delete, (\\), insert, nub)
import Data.STRef
import Data.Tuple (swap)
import qualified Data.Set as S
import qualified Data.IntSet as IS
import qualified Data.IntMap as IM
import qualified Data.Map as M
import           Data.Array.MArray
import qualified Data.Array as A
import Data.Array.ST
import ErrorMessages as Err
import Pretty
import UU.Scanner.Position

data Settings = Settings  
                { -- current minimum ads size
                  prune_val  :: Int 
                  -- whether to minimize the number of fake dependencies
                  -- could be very costly
                , min_ads :: Bool
                }
default_settings = Settings 999 False

type AOAG s a = ST s a

-- | Catch a type 3 cycle-error made by a given constructor
-- |  two alternatives are given to proceed
type ADS = [Edge]
type AOAGRes =  Either Error LOAGRes
-- | Calculate a total order if the semantics given 
--    originate from a linearly-ordered AG

type2error,limiterror,aoagerror :: Error
type2error = Err.CustomError False noPos $ text "Type 2 cycle"
limiterror = Err.CustomError False noPos $ text "Limit reached"
aoagerror  = Err.CustomError False noPos $ text "Not an LOAG/AOAG"

schedule :: LOAGRep -> Grammar -> Ag -> [Edge] -> AOAGRes
schedule sem gram@(Grammar _ _ _ _ dats _ _ _ _ _ _ _ _ _) 
                ag@(Ag bounds_s bounds_p de nts) ads 
    = runST $ aoag default_settings ads
 where
    -- get the maps from semantics and translate them to functions    
    nmp  = (nmp_LOAGRep_LOAGRep  sem)     
    ofld = (ofld_LOAGRep_LOAGRep sem)       
    genA = gen_LOAGRep_LOAGRep sem             
    inss = inss_LOAGRep_LOAGRep sem        
 
    -- select candidates, using the edge that caused the cycle
    -- from the list of intra-thread dependencies 
    -- (intra-visit dependencies without edges in ids)
    candidates :: Edge -> Cycle -> [Edge] -> [Edge]
    candidates _ c = foldr (\(f,t) acc -> 
                                if f `IS.member` c &&t `IS.member` c
                                    then (t,f):acc else acc) []
    -- | Move occurrence to its corresponding attribute 
    gen :: Vertex -> Vertex
    gen v = genA A.! v

    genEdge :: Edge -> Edge
    genEdge (f,t) = (gen f, gen t)

    -- | Decide for a given production edge whether the vertices 
    --      belong to the same field
    siblings :: Edge -> Bool
    siblings (f, t) = ofld A.! f == ofld A.! t

    -- | Given an nonterminal-edge, instantiate it
    --   assumes that the occurrences of fields are added in the same order
    instEdge :: Edge -> [Edge]
    instEdge (f, t) = zip (inss A.! f) (inss A.! t)
 
    aoag :: Settings -> [Edge] -> AOAG s AOAGRes
    aoag cfg init_ads = run
        where
            run :: AOAG s AOAGRes
            run = induced ads >>= detect

            detect (Left err) = return $ Left err
            detect (Right (dp,idp,ids@(idsf,idst))) = do
                -- Attribute -> TimeSlot
                schedA <- mapArray (const Nothing) idsf
                -- map TimeSlot -> [Attribute]
                schedS <- newSTRef $ 
                    foldr (\(Nonterminal nt _ _ _ _) -> M.insert (getName nt) 
                                (IM.singleton 1 [])) M.empty dats
                fr_ids <- freeze_graph ids
                threads <- completing fr_ids (schedA, schedS) nts
                let (ivd, comp) = fetchEdges fr_ids threads nts
                eRoC <- m_edp dp init_ads ivd comp (schedA, schedS)
                case eRoC of
                    Left res -> return $ Right res
                    Right (e,c,T3 cs) -> find_ads dp idp ids (schedA, schedS) e c cs

            find_ads :: Graph s -> Graph s -> Graph s -> SchedRef s -> 
                         Edge -> Cycle -> [Edge] -> AOAG s AOAGRes
            find_ads dp idp ids sched e cycle comp = do
                pruner <- newSTRef 999
                explore dp idp ids sched init_ads pruner e cycle comp
 
            explore :: Graph s -> Graph s -> Graph s -> SchedRef s -> 
                        [Edge] -> STRef s Int -> Edge -> Cycle -> [Edge] ->
                            AOAG s AOAGRes
            explore dp idp ids sched@(schedA, schedS) ads pruner e c comp =
                explore' dp idp ids sched ads (candidates e c comp) pruner
             where
              explore' :: Graph s -> Graph s -> Graph s -> SchedRef s -> 
                          [Edge] -> [Edge] -> STRef s Int -> 
                            AOAG s AOAGRes
              explore' _  _   _   _  _   [] _ = return $ Left aoagerror
              explore' dp idp ids sched@(schedA,schedS) ads (fd:cs) pruner = do
                  p_val <- readSTRef pruner
                  if length ads >= p_val -1
                   then return $ Left limiterror 
                   else do 
                    idpf_clone <- mapArray id (fst idp)
                    idpt_clone <- mapArray id (snd idp)
                    let idp_c  =  (idpf_clone, idpt_clone)
                    idsf_clone <- mapArray id (fst ids)
                    idst_clone <- mapArray id (snd ids)
                    let ids_c  =  (idsf_clone, idst_clone)
                    schedA_c   <- mapArray id schedA
                    schedS_v   <- readSTRef schedS
                    schedS_c   <- newSTRef schedS_v
                    let sched_c = (schedA_c, schedS_c)
    
                    let runM = reschedule dp idp ids sched 
                                                (fd:ads) fd pruner
                    let backtrack = explore' dp idp_c ids_c sched_c ads cs 
                                        pruner
                    maoag <- runM
                    case maoag of 
                      Left _                 -> backtrack
                      Right (tdp1,inf1,ads1) -> 
                            if LOAG.AOAG.min_ads cfg 
                             then do
                              writeSTRef pruner (length ads1)
                              maoag' <- backtrack
                              case maoag' of
                                Right (tdp2,inf2,ads2)
                                          -> return $ Right (tdp2,inf2,ads2)
                                otherwise -> return $ Right (tdp1,inf1,ads1)
                             else return $ Right (tdp1,inf1,ads1)

            -- step 1, 2 and 3
            induced :: [Edge] -> AOAG s (Either Error (Graph s, Graph s, Graph s))
            induced ads = do
                dpf  <- newArray bounds_p IS.empty
                dpt  <- newArray bounds_p IS.empty
                idpf <- newArray bounds_p IS.empty
                idpt <- newArray bounds_p IS.empty
                idsf <- newArray bounds_s IS.empty
                idst <- newArray bounds_s IS.empty
                let ids = (idsf,idst)
                let idp = (idpf,idpt)
                let dp  = (dpf ,dpt)
                inducing dp idp ids (de ++ ads) 

            inducing :: Graph s -> Graph s -> Graph s -> [Edge] 
                            -> AOAG s (Either Error (Graph s, Graph s, Graph s))
            inducing dp idp ids es = do
                res <- adds (addD dp idp ids) [] es
                case res of 
                    Left _ -> return $ Left $ type2error
                    Right _  -> return $ Right (dp, idp, ids)
            addD :: Graph s -> Graph s -> Graph s -> Edge -> AOAG s (Either Error [Edge])
            addD dp' idp' ids' e = do
                resd <- e `insErt` dp'
                resdp <- e `inserT` idp'
                case resdp of 
                  Right es  -> adds (addN idp' ids') [] (e:es)
                  Left c    -> return $ Left $ type2error

            addI :: Graph s -> Graph s -> Edge -> AOAG s (Either Error [Edge])
            addI idp' ids' e = do
                exists <- member e idp'
                if not exists then do
                    res <- e `inserT` idp'
                    case res of
                        Right es -> adds (addN idp' ids') [] es
                        Left c   -> return $ Left $ type2error
                 else return $ Right []

            adds f acc [] = return $ Right acc
            adds f acc (e:es) = do
                mes <- f e
                case mes of 
                    Left err -> return $ Left err
                    Right news -> adds f (acc++news) es

            addN :: Graph s -> Graph s -> Edge -> AOAG s (Either Error [Edge])
            addN idp' ids' e = do
                    if (siblings e) then do
                        let s_edge = genEdge e
                        exists <- member s_edge ids'
                        if not exists then do
                            _ <- inserT s_edge ids'
                            let es = instEdge s_edge
                            addedEx <- adds (addI idp' ids') [] es
                            case addedEx of
                                Right news -> return $ Right (s_edge : news)
                                Left err   -> return $ Left err
                         else return $ Right []
                     else return $ Right []            

            
            -- step 6, 7
            m_edp :: Graph s -> [Edge] -> [Edge] -> [Edge] -> SchedRef s ->
                        AOAG s (Either LOAGRes (Edge,Cycle,CType))
            m_edp (dpf, dpt) ads ivd comp sched = do
                edpf <- mapArray id dpf
                edpt <- mapArray id dpt
                mc   <- addEDs (edpf,edpt) (concatMap instEdge ivd) 
                case mc of
                  Just (e, c) -> return $ Right (e,c,T3 $ concatMap instEdge comp)
                  Nothing     -> do 
                        tdp  <- freeze edpt
                        infs <- readSTRef (snd sched)
                        return $ Left (Just tdp,infs,ads)

            reschedule :: Graph s -> Graph s -> Graph s -> SchedRef s -> 
                           [Edge] -> Edge -> STRef s Int 
                            -> AOAG s AOAGRes
            reschedule dp idp ids sched@(_,threadRef) ads e pruner = do
                extra <- addN idp ids e
                case extra of 
                    Left err -> return $ Left err
                    Right extra -> do
                        forM_ extra $ swap_ivd ids sched
                        fr_ids <- freeze_graph ids
                        threads <- readSTRef threadRef
                        let (ivd, comp) = fetchEdges fr_ids threads nts 
                        eRoC <- m_edp dp ads ivd comp sched
                        case eRoC of
                            Left res -> return $ Right res 
                            Right (e,c,(T3 cs)) -> explore dp idp ids sched ads pruner e c cs
             where 
              swap_ivd :: Graph s -> SchedRef s -> Edge -> AOAG s ()
              swap_ivd ids@(idsf, idst) sr@(schedA, schedS) (f,t) = do
                --the edge should point from higher to lower timeslot
                assigned <- freeze schedA
                let oldf = maybe (error "unassigned f") id $ assigned A.! f
                    oldt = maybe (error "unassigned t") id $ assigned A.! t
                    dirf = snd $ alab $ nmp M.! f
                    dirt = snd $ alab $ nmp M.! t
                    newf | oldf < oldt = oldt + (if dirf /= dirt then 1 else 0)
                         | otherwise   = oldf
                    nt   = show $ typeOf $ nmp M.! f

                -- the edge was pointing in wrong direction so we moved 
                -- the attribute to a new interaction, now some of its
                -- predecessors/ancestors might need to be moved too
                unless (oldf == newf) $ do
                    writeArray schedA f (Just newf)
                    modifySTRef schedS 
                            (M.adjust (IM.update (Just . delete f) oldf) nt)
                    modifySTRef schedS 
                        (M.adjust(IM.alter(Just. maybe [f] (insert f))newf)nt)
                    predsf <- readArray idst f
                    succsf <- readArray idsf f
                    let rest = (map (flip (,) f) $ IS.toList predsf) ++ 
                               (map ((,) f)      $ IS.toList succsf)
                     in mapM_ (swap_ivd ids sr) rest
                        
                        
