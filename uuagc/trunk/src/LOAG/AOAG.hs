{-# LANGUAGE Rank2Types #-}

module LOAG.AOAG where

import LOAG.Common
import LOAG.Graphs
import LOAG.Rep
import LOAG.Result

import AbstractSyntax
import CommonTypes
import Control.Arrow ((&&&), (***))
import Control.Monad (forM, forM_, MonadPlus(..), when, unless)
import Control.Monad.ST
import Control.Monad.Error (ErrorT(..))
import Control.Monad.Trans (lift, MonadTrans(..))
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

type AOAG s a = ResultT (ST s) a
runAOAG :: (forall s. AOAG s a) -> Either Err.Error a
runAOAG l = 
    case runST (runResult l) of
      Give res          -> Right res
      Cycle e c T1      -> Left $ t1err
      Cycle e c T2      -> Left $ t2err
      Cycle e c (T3 _)  -> Left $ t3err
      Limit             -> Left $ lerr
      NotLOAG           -> Left $ naoag
    where   t1err = Err.CustomError False noPos $ text "Type 1 cycle"
            t2err = Err.CustomError False noPos $ text "Type 2 cycle"
            t3err = Err.CustomError False noPos $ text "Type 3 cycle"
            lerr  = Err.CustomError False noPos $ text "Limit reached!"
            naoag = Err.CustomError False noPos $ text "Not arranged orderly..."
-- | Catch a type 3 cycle-error made by a given constructor
-- |  two alternatives are given to proceed
catchType3 :: (Monad m) => 
                    ResultT m a             -- The monad to catch from
                 -- If the catch is made
                 -> (Edge -> Cycle -> [Edge] -> ResultT m a)
                 -> ResultT m a              
catchType3 mt3 alt = Result $ do
    let runM = runResult mt3
    mt3a <- runM
    case mt3a of
        Cycle e c (T3 comp) -> runResult (alt e c comp)
        otherwise           -> runM

type ADS = [Edge]
type AOAGRes =  LOAGRes
-- | Calculate a total order if the semantics given 
--    originate from a linearly-ordered AG
schedule :: LOAGRep -> Grammar -> [Edge] -> Either Error AOAGRes
schedule sem (Grammar _ _ _ _ dats _ _ _ _ _ _ _ _ _) ads 
    = runAOAG $ aoag default_settings ads
 where
    -- get the maps from semantics and translate them to functions    
    ain  = map (findWithErr nmpr "building an") . map2F (ain_LOAGRep_LOAGRep sem) --X
    asn  = map (findWithErr nmpr "building an") . map2F (asn_LOAGRep_LOAGRep sem) --X
    ap   = map (findWithErr pmpr "building ap") . map2F  (ap_LOAGRep_LOAGRep   sem) -- only dpe
    afp  = filter inOutput . ap --X
    fieldM  = fieldMap_LOAGRep_LOAGRep sem --X
    sfp  = map2F' (sfp_LOAGRep_LOAGRep sem) -- only dpe
    pmp  = (pmp_LOAGRep_LOAGRep  sem)       -- bounds, dpe, m_edp
    pmpr = (pmpr_LOAGRep_LOAGRep sem)       -- only dpe
    nmp  = (nmp_LOAGRep_LOAGRep  sem)       -- bounds, reschedule
    nmpr = (nmpr_LOAGRep_LOAGRep sem)       -- X
    ofld = (ofld_LOAGRep_LOAGRep sem)       -- X
    ps   = ps_LOAGRep_LOAGRep   sem         -- only dpe
    nonts= map (\(Nonterminal nt_id _ _ _ _) -> TyData $ getName nt_id) dats -- X
    nts=    map toNt nonts 
    fsInP  = map2F (fsInP_LOAGRep_LOAGRep sem) --X 
    genA = gen_LOAGRep_LOAGRep sem             --X
    inss = inss_LOAGRep_LOAGRep sem            -- instEdge
 
    bounds_p = if M.null pmp then (0,-1) 
                else (fst $ M.findMin pmp, fst $ M.findMax pmp)
    bounds_s = if M.null nmp then (0,-1) 
                else (fst $ M.findMin nmp, fst $ M.findMax nmp)
 
    de    = [ e      | p <- ps,   e <- dpe p ]
    dpe p = [ (findWithErr pmpr "building dpe" a, b) 
            | b <- ap p, a <- S.toList $ sfp (findWithErr pmp "fetching sfp" b) ]

    -- select candidates, using the edge that caused the cycle
    -- from the list of intra-thread dependencies 
    -- (intra-visit dependencies without edges in ids)
    candidates :: Edge -> Cycle -> [Edge] -> [Edge]
    candidates _ c = foldr (\(f,t) acc -> 
                                if f `IS.member` c &&t `IS.member` c
                                    then (t,f):acc else acc) []
    -- X
    inContext :: Vertex -> Bool
    inContext f = (f1 == "lhs" && d1 == Inh || f1 /= "lhs" && d1 == Syn) 
        where (MyOccurrence (_,f1) (_,d1)) = pmp M.! f
    -- X
    inOutput :: Vertex -> Bool
    inOutput = not . inContext 

    -- | Move occurrence to its corresponding attribute 
    -- X
    gen :: Vertex -> Vertex
    gen v = genA A.! v

    -- X 
    genEdge :: Edge -> Edge
    genEdge (f,t) = (gen f, gen t)

    -- | Decide for a given production edge whether the vertices 
    --      belong to the same field
    --X
    siblings :: Edge -> Bool
    siblings (f, t) = ofld A.! f == ofld A.! t

    -- | Given an nonterminal-edge, instantiate it
    --   assumes that the occurrences of fields are added in the same order
    -- only m_edp
    instEdge :: Edge -> [Edge]
    instEdge (f, t) = zip (inss A.! f) (inss A.! t)

    -- TODO MAJOR TODO, copy-past from LOAG.hs , generalise!!
    toNt :: MyType -> Nt
    toNt ty@(TyData ntid) = Nt ntid dpf dpt (addD Inh $ ain ty) 
                                            (addD Syn $ asn ty) (map toPr ps)
     where dpt =  [ ((as, ai), instEdge (as,ai)) | ai <- ain ty
                   , as <- nub $ [ gen s |
                                   i <- inss A.! ai
                                 , s <- map (pmpr M.!) $ 
                                    S.toList (sfp $ pmp M.! i)
                                 , siblings (s,i)]]
           dpf =  [ ((ai, as),instEdge (ai,as)) | as <- asn ty
                   , ai <- nub $   [ gen i |
                                     s <- inss A.! as
                                   , i <- map (pmpr M.!) $
                                            S.toList (sfp $ pmp M.! s)
                                   , siblings (i,s)]]
           addD d = map (\i -> (i,inss A.! i,d))
    toPr :: PLabel -> Pr
    toPr p = Pr p dpp fc_occs (map toFd $ fsInP p)
     where dpp = [ (f',t)
                    | t <- afp p, f <- (S.toList $ sfp (pmp M.! t))
                    , let f' = pmpr M.! f
                    , not (siblings (f',t))]
           fc_occs = foldl' match [] fss
            where fss = fsInP p
           match s fs = [ ready (inp, out) lhs | inp <- S.toList inhs
                                           , out <- S.toList syns] ++ s
            where ((inhs, syns),lhs) | (snd fs) /= "lhs" = 
                                        (swap (fieldM M.! fs), False)
                               | otherwise = (fieldM M.! fs, True)
                  ready e@(f,t) b = (e', genEdge e', b)
                   where e' = (pmpr M.! f, pmpr M.! t)
    toFd :: (PLabel, FLabel) -> Fd
    toFd fs@((TyData ty, pr), fd) = Fd fd ty inhs syns
     where (is,ss) = fieldM M.! fs
           inhs = map (((genA A.!) &&& id).(pmpr M.!))$ S.toList is
           syns = map (((genA A.!) &&& id).(pmpr M.!))$ S.toList ss

 
    aoag :: Settings -> [Edge] -> AOAG s AOAGRes
    aoag cfg init_ads = run
        where
            run :: AOAG s AOAGRes
            run = induced ads >>= detect

            detect (dp,idp,ids@(idsf,idst)) = do
                -- Attribute -> TimeSlot
                schedA <- lift (mapArray (const Nothing) idsf)
                -- map TimeSlot -> [Attribute]
                schedS <- lift (newSTRef $ 
                    foldr (\(Nonterminal nt _ _ _ _) -> M.insert (getName nt) 
                                (IM.singleton 1 [])) M.empty dats)
                fr_ids <- freeze_graph ids
                threads <- lift (completing fr_ids (schedA, schedS) nts)
                let (ivd, comp) = fetchEdges fr_ids threads nts
                m_edp dp init_ads ivd comp (schedA, schedS) `catchType3` 
                                find_ads dp idp ids (schedA, schedS)

            find_ads :: Graph s -> Graph s -> Graph s -> SchedRef s -> 
                         Edge -> Cycle -> [Edge] -> AOAG s AOAGRes 
            find_ads dp idp ids sched e cycle comp = do
                pruner <- lift (newSTRef 999) 
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
              explore' _  _   _   _  _   [] _ = Result $ return NotLOAG
              explore' dp idp ids sched@(schedA,schedS) ads (fd:cs) pruner 
               = Result $ do
                  p_val <- readSTRef pruner
                  if length ads >= p_val -1
                   then return Limit
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
    
                    let runM = runResult $ reschedule dp idp ids sched 
                                                (fd:ads) fd pruner
                    let backtrack = explore' dp idp_c ids_c sched_c ads cs 
                                        pruner
                    maoag <- runM
                    case maoag of
                      Cycle e c T2        -> runResult backtrack
                      NotLOAG             -> runResult backtrack
                      Limit               -> runResult backtrack
                      Cycle e c (T3 comp) -> error "Uncaught type 3"
                      Cycle e c T1        -> error "Type 1 error"
                      Give (tdp1,inf1,ads1) -> 
                            if LOAG.AOAG.min_ads cfg 
                             then do
                              writeSTRef pruner (length ads1)
                              maoag' <- runResult backtrack
                              case maoag' of
                                Give (tdp2,inf2,ads2)
                                          -> return $ Give (tdp2,inf2,ads2)
                                otherwise -> return $ Give (tdp1,inf1,ads1)
                             else return $ Give (tdp1,inf1,ads1)

            -- step 1, 2 and 3
            induced :: [Edge] -> AOAG s (Graph s, Graph s, Graph s)
            induced ads = do
                dpf  <- lift (newArray bounds_p IS.empty)
                dpt  <- lift (newArray bounds_p IS.empty)
                idpf <- lift (newArray bounds_p IS.empty)
                idpt <- lift (newArray bounds_p IS.empty)
                idsf <- lift (newArray bounds_s IS.empty)
                idst <- lift (newArray bounds_s IS.empty)
                let ids = (idsf,idst)
                let idp = (idpf,idpt)
                let dp  = (dpf ,dpt)
                inducing dp idp ids (de ++ ads) 

            inducing :: Graph s -> Graph s -> Graph s -> [Edge] 
                            -> AOAG s (Graph s, Graph s, Graph s)
            inducing dp idp ids es = do
                mapM_ (addD dp idp ids) es
                return (dp, idp, ids)
            addD :: Graph s -> Graph s -> Graph s -> Edge -> AOAG s [Edge]
            addD dp' idp' ids' e = do
                resd <- e `insErt` dp'
                resdp <- e `inserT` idp'
                case resdp of 
                  Right es  -> do 
                    addedExtras <- mapM (addN idp' ids') (e:es)
                    return $ concat addedExtras
                  Left c    -> throwCycle e c T2

            addI :: Graph s -> Graph s -> Edge -> AOAG s [Edge]
            addI idp' ids' e = do
                exists <- member e idp'
                if not exists then do
                    res <- e `inserT` idp'
                    case res of
                        Right es -> do
                            addedExtras <- mapM (addN idp' ids') es
                            return (concat addedExtras) 
                        Left c   -> throwCycle e c T2
                 else return []
            addN :: Graph s -> Graph s -> Edge -> AOAG s [Edge]
            addN idp' ids' e = do
                    if (siblings e) then do
                        let s_edge = genEdge e
                        exists <- member s_edge ids'
                        if not exists then do
                            _ <- inserT s_edge ids'
                            addedEx <- mapM (addI idp' ids') (instEdge s_edge)
                            return (s_edge : concat addedEx)
                         else return []
                     else return []

            
            -- step 6, 7
            m_edp :: Graph s -> [Edge] -> [Edge] -> [Edge] -> SchedRef s ->
                        AOAG s AOAGRes 
            m_edp (dpf, dpt) ads ivd comp sched = do
                edpf <- lift (mapArray id dpf)
                edpt <- lift (mapArray id dpt)
                mc   <- addEDs (edpf,edpt) (concatMap instEdge ivd) 
                case mc of
                  Just (e, c) -> throwCycle e c (T3 $ concatMap instEdge comp)
                  Nothing     -> do 
                        tdp  <- lift (freeze edpt)
                        infs <- lift (readSTRef (snd sched))
                        return $ (Just tdp,infs,ads)

            reschedule :: Graph s -> Graph s -> Graph s -> SchedRef s -> 
                           [Edge] -> Edge -> STRef s Int 
                            -> AOAG s AOAGRes
            reschedule dp idp ids sched@(_,threadRef) ads e pruner = do
                extra <- addN idp ids e
                forM_ extra $ swap_ivd ids sched
                fr_ids <- freeze_graph ids
                threads <- lift (readSTRef threadRef)
                let (ivd, comp) = fetchEdges fr_ids threads nts 
                m_edp dp ads ivd comp sched `catchType3` 
                    explore dp idp ids sched ads pruner
             where 
              swap_ivd :: Graph s -> SchedRef s -> Edge -> AOAG s ()
              swap_ivd ids@(idsf, idst) sr@(schedA, schedS) (f,t) = do
                --the edge should point from higher to lower timeslot
                assigned <- lift (freeze schedA)
                let oldf = maybe (error "unassigned f") id $ assigned A.! f
                    oldt = maybe (error "unassigned t") id $ assigned A.! t
                    dirf = snd $ alab $ nmp M.! f
                    dirt = snd $ alab $ nmp M.! t
                    newf | oldf < oldt = oldt + (if dirf /= dirt then 1 else 0)
                         | otherwise   = oldf
                    nt   = show $ typeOf $ findWithErr nmp "m_edp" f

                -- the edge was pointing in wrong direction so we moved 
                -- the attribute to a new interaction, now some of its
                -- predecessors/ancestors might need to be moved too
                unless (oldf == newf) $ do
                    lift (writeArray schedA f (Just newf))
                    lift (modifySTRef schedS 
                            (M.adjust (IM.update (Just . delete f) oldf) nt))
                    lift (modifySTRef schedS 
                        (M.adjust(IM.alter(Just. maybe [f] (insert f))newf)nt))
                    predsf <- lift (readArray idst f)
                    succsf <- lift (readArray idsf f)
                    mapM_ (swap_ivd ids sr) (
                        (map (flip (,) f) $ IS.toList predsf) ++ 
                        (map ((,) f)      $ IS.toList succsf))

