{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}

module LOAG.LOAG where

import LOAG.Common
import LOAG.Rep
import LOAG.Graphs
import LOAG.Result
import LOAG.Chordal

import              AbstractSyntax
import              CommonTypes         hiding (verbose)
import qualified    Control.Arrow       as Arr ((&&&))
import              Control.Monad       (when)
import              Control.Monad.ST
import qualified    Data.Array          as A
import              Data.Array.ST
import qualified    Data.IntSet         as IS
import qualified    Data.IntMap         as IM
import              Data.List           (foldl',minimum,intercalate,nub)
import qualified    Data.Map            as M
import qualified    Data.Set            as S
import              Data.Tuple          (swap)
import              ErrorMessages       as Err
import              Pretty
import qualified    System.IO           as IO
import              System.IO.Unsafe
import              UU.Scanner.Position

---- Chordal stuff

runLOAG :: (forall s. LOAG s a) -> Either Err.Error a
runLOAG l = 
    case runST (runResult l) of
      Give res          -> Right res
      Cycle e c T1      -> Left $ t1err
      Cycle e c T2      -> Left $ t2err
      Cycle e c (T3 _)  -> Left $ t3err
      Limit             -> Left $ lerr
      NotLOAG           -> Left $ nloag
    where   t1err = Err.CustomError False noPos $ text "Type 1 cycle"
            t2err = Err.CustomError False noPos $ text "Type 2 cycle"
            t3err = Err.CustomError False noPos $ text "Type 3 cycle"
            lerr  = Err.CustomError False noPos $ text "Limit reached!"
            nloag = Err.CustomError False noPos $ text "Not arranged orderly..."

-- | Calculate a total order if the semantics given 
--    originate from a linearly-ordered AG
schedule :: LOAGRep -> Grammar -> [Edge] -> Bool -> Either Error LOAGRes
schedule sem (Grammar _ _ _ _ dats _ _ _ _ _ _ _ _ _) ads verbose
    = runLOAG $ loag ads
 where
    -- get the maps from semantics and translate them to functions    
    anM  = an_LOAGRep_LOAGRep   sem 
    an   = map (findWithErr nmpr "building an") . map2F anM
    ain  = map (findWithErr nmpr "building an") . map2F (ain_LOAGRep_LOAGRep sem)
    asn  = map (findWithErr nmpr "building an") . map2F (asn_LOAGRep_LOAGRep sem)
    ap   = map (findWithErr pmpr "building ap") . map2F (ap_LOAGRep_LOAGRep  sem)
    acp  = filter inContext . ap
    afp  = filter inOutput . ap
    fieldM  = fieldMap_LOAGRep_LOAGRep sem
    varsInF :: (PLabel, FLabel) -> [Edge]
    varsInF = map2F $ M.mapMaybe (\(is, os) ->  Just $
                        [(pmpr M.! o, pmpr M.! i)|
                        i <- S.toList is, o <- S.toList os]) fieldM 
    fsInP  = map2F (fsInP_LOAGRep_LOAGRep sem)
    sfp  = map2F' (sfp_LOAGRep_LOAGRep sem)
    pmp  = (pmp_LOAGRep_LOAGRep  sem)
    pmpr = (pmpr_LOAGRep_LOAGRep sem)
    nmp  = (nmp_LOAGRep_LOAGRep  sem)
    nmpr = (nmpr_LOAGRep_LOAGRep sem)
    ofld = (ofld_LOAGRep_LOAGRep sem)
    fty  = (fty_LOAGRep_LOAGRep sem)
    ps   = ps_LOAGRep_LOAGRep   sem
    nonts= map (\(Nonterminal nt_id _ _ _ _) -> TyData $ getName nt_id) dats
    genA = gen_LOAGRep_LOAGRep sem
    inss = inss_LOAGRep_LOAGRep sem 
    dirO = snd . attr . (findWithErr pmp "getting occr dir")
    dirA = snd . alab . (findWithErr nmp "getting attr dir")
 
    putStrLn s = when verbose (IO.putStrLn s)

    bounds_p = if M.null pmp then (0,-1) 
                else (fst $ M.findMin pmp, fst $ M.findMax pmp)
    bounds_s = if M.null nmp then (0,-1) 
                else (fst $ M.findMin nmp, fst $ M.findMax nmp)
 
    de    = [ e      | p <- ps,   e <- dpe p ]
    dpe p = [ (findWithErr pmpr "building dpe" a, b) 
            | b <- ap p, a <- S.toList $ sfp (findWithErr pmp "fetching sfp" b) ]

    inContext :: Vertex -> Bool
    inContext f = (f1 == "lhs" && d1 == Inh || f1 /= "lhs" && d1 == Syn) 
        where (MyOccurrence (_,f1) (_,d1)) = pmp M.! f

    inOutput :: Vertex -> Bool
    inOutput = not . inContext 

    -- Whether the edge is normalized
    bnf :: Edge -> Bool -- f is input occ, t is output occ
    bnf (f,t) = inContext f && inOutput t 

    -- reverse bnf
    bnf_rev :: Edge -> Bool
    bnf_rev = bnf . swap

    -- | Move occurrence to its corresponding attribute 
    gen :: Vertex -> Vertex
    gen v = genA A.! v
 
    genEdge :: Edge -> Edge
    genEdge (f,t) = (gen f, gen t)

    -- | Decide for a given production edge whether the vertices 
    --      belong to the same field
    siblings :: Edge -> Bool
    siblings (f, t) = ofld A.! f == ofld A.! t

    -- | Decide for a given non-terminal edge whether the vertices
    --      belong to the same non-terminal 
    siblingsNT :: Edge -> Bool
    siblingsNT (f, t) = typeOf fa == typeOf ta
        where fa = findWithErr nmp "siblingsNT" f
              ta = findWithErr nmp "siblingsNT" t

    -- | Given an nonterminal-edge, instantiate it
    --   assumes that the occurrences of fields are added in the same order
    instEdge :: Edge -> [Edge]
    instEdge (f, t) = zip (inss A.! f) (inss A.! t)
 
    loag :: [Edge] -> LOAG s LOAGRes
    loag ads = run
        where
            run :: LOAG s LOAGRes
            run = return $ unsafePerformIO $ scheduleLOAG ag putStrLn
            varsInP p = ((length $ edgesInP p))
            edgesInP p= [ (f, t) | f <- ap p, t <- ap p, siblings (f,t), bnf (f,t)]
            varsInN n = (length $ edgesInN n)
            edgesInN n= [(f,t) | f <- ain n, t <- asn n]

            allAttrs = [ a | n <- nonts, a <- an n]

            ag = Ag bounds_s bounds_p de (map toNt dats)
            toNt :: Nonterminal -> Nt
            toNt (Nonterminal ntid _ _ _ prods) = Nt nt dpf dpt 
                    (addD Inh $ ain ty) (addD Syn $ asn ty) (map (toPr ty) prods)
             where nt  = getName ntid
                   ty  = TyData nt
                   dpt =  [ ((as, ai),instEdge (as,ai)) | ai <- ain ty
                           , as <- nub$ [ gen s |
                                          i <- inss A.! ai
                                        , s <- map (pmpr M.!) $ 
                                            S.toList (sfp $ pmp M.! i)
                                        , siblings (s,i)]]
                   dpf =  [ ((ai, as),instEdge (ai,as)) | as <- asn ty
                           , ai <- nub$ [ gen i |
                                          s <- inss A.! as
                                        , i <- map (pmpr M.!) $
                                            S.toList (sfp $ pmp M.! s)
                                        , siblings (i,s)]]
                   addD d = map (\i -> (i,inss A.! i,d))
            toPr :: MyType -> Production -> Pr
            toPr ty (Production con _ _ _ _ _ _) = 
                        Pr p dpp fc_occs (map toFd $ fsInP p)
             where p = (ty, getName con)
                   dpp = [ (f',t)
                            | t <- afp p, f <- (S.toList $ sfp (pmp M.! t))
                            , let f' = pmpr M.! f
                            , not (siblings (f',t))]
                   fc_occs = foldl' match [] fss
                    where fss = fsInP p
                   match s fs = [ ready (inp, out) lhs | inp <- S.toList inhs
                                                   , out <- S.toList syns] ++ s
                    where ((inhs, syns), lhs)
                                       | (snd fs) /= "lhs" = 
                                            (swap (fieldM M.! fs),False)
                                       | otherwise = (fieldM M.! fs, True)
                          ready e@(f,t) b = (e', genEdge e', b)
                           where e' = (pmpr M.! f, pmpr M.! t)
            toFd :: (PLabel, FLabel) -> Fd
            toFd fs@((TyData ty, pr), fd) = Fd fd ty inhs syns
             where (is,ss) = fieldM M.! fs
                   inhs = map (((genA A.!) Arr.&&& id).(pmpr M.!))$ S.toList is
                   syns = map (((genA A.!) Arr.&&& id).(pmpr M.!))$ S.toList ss

