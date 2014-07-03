
module LOAG.Chordal where

import LOAG.Common
import LOAG.Graphs
import LOAG.Optimise
import LOAG.Solver.MiniSat

import              Control.Monad (unless, forM, when, foldM)
import              Control.Monad.ST
import qualified    Data.Array as A
import              Data.Array.IO
import              Data.Array.ST as ST
import qualified    Data.IntMap as IM
import qualified    Data.IntSet as IS
import qualified    Data.Map as M
import              Data.Maybe (isNothing, catMaybes)
import qualified    Data.Set as S
import              Data.STRef

type Neigh     = (Vertex, EdgeType)
data EdgeType  = Sib    MyVar   -- Edge between siblings 
               | Any    MyVar   -- Unknown
               | NSib   MyVar   -- Edge between non-siblings
    deriving (Show)

extract (Sib l) = l
extract (Any l) = l
extract (NSib l)= l

neg :: EdgeType -> EdgeType
neg (Sib p)  = Sib  $ varnot p
neg (NSib p) = NSib $ varnot p
neg (Any p)  = Any  $ varnot p


type NtGraph   = IM.IntMap [(Vertex,MyVar)]
type ToChordal = IM.IntMap [Neigh]
type SatEdge = (Vertex, Neigh)

addClauses :: Sat -> [[EdgeType]] -> IO ()
addClauses sat = mapM_ (addClause sat . map extract) 

toChordal :: [(Edge,EdgeType)] -> IO ToChordal
toChordal es = 
    let edges = concatMap (\((a,b),c) -> [(a,[(b,c)]),(b,[(a,neg c)])]) es
      in return $ IM.fromListWith (++) edges

toNtGraph :: [(Edge,MyVar)] -> IO NtGraph
toNtGraph es = 
    let edges = concatMap (\((a,b),c) -> [(a,[(b,c)]),(b,[(a,varnot c)])]) es
      in return $ IM.fromListWith (++) edges


remove :: Vertex -> [Neigh] -> ToChordal -> ToChordal
remove v ns g = foldr (IM.adjust remN . fst) (IM.delete v g) ns 
 where remN ns = filter ((/= v).fst) ns

adds :: ToChordal -> [SatEdge] -> ToChordal
adds g = foldl add g

add :: ToChordal -> SatEdge -> ToChordal
add g (v,n@(v2,c)) = IM.adjust (n:)          v
                   $ IM.adjust ((v,neg c):)  v2 g

removeNt :: Vertex -> [(Vertex,MyVar)] -> NtGraph -> NtGraph
removeNt v ns g = foldr (IM.adjust remN . fst) (IM.delete v g) ns 
 where remN ns = filter ((/= v).fst) ns

addsNt g = foldl addNt g

addNt g (v,n@(v2,c)) = IM.adjust (n:)          v
                     $ IM.adjust ((v,varnot c):)  v2 g

scheduleLOAG :: Ag -> (String -> IO ()) -> Opts -> IO LOAGRes
scheduleLOAG ag@(Ag nbounds pbounds dps nts) putStrLn opts = do
    putStrLn "--- Starting ---"
    sat <- newSolvable 
    varMap <- noNtCycles sat nts putStrLn
    noPrCycles sat prs varMap putStrLn 
    (v,p) <- satValues sat
    putStrLn $ "nr. of variables: " ++ show v 
    putStrLn $ "nr. of clauses: "   ++ show p
    putStrLn "--- Solving ---"
    b <- satsolve sat []
    if not b then error "Not LOAG" 
      else do   putStrLn "--- Constructing Interfaces ---"
                (ids,edp,interfaces) <- loagRes sat varMap dps
                let oldct = getVisCount nts interfaces
                when minvisit $ 
                    putStrLn "--- Minimising #Visit"
                optimise sat varMap opts nbounds nts interfaces 
                (ids',edp',interfaces') <- loagRes sat varMap dps
                let cycles = length $ maxSCCs edp'
                when (cycles > 0) $ 
                    error ("Bad LOAG scheduling: " ++ show cycles ++ " cycles")
                let visC@newct = getVisCount nts interfaces'
                when minvisit $ do
                    putStrLn ("--- #Visits (max,sum,avg) " ++(show oldct)
                                ++" --> " ++(show newct))
                putStrLn "--- Code Generation ---"
                return (Just edp',interfaces',[])
 where  loagRes sat varMap dps = do    
            (ids,edp) <- mkGraphs sat varMap dps
            interfaces <- mkInterfaces ids
            return (ids,edp,interfaces)
        minvisit = MinVisits `elem` opts
        prs =  [ p | (Nt _ _ _ _ _ ps) <- nts, p <- ps]
        mkInterfaces ids = return $ runST $ do
                schedA <- newArray nbounds Nothing
                schedS <- newSTRef $ foldr (\(Nt nt _ _ _ _ _) -> 
                                M.insert nt (IM.singleton 1 [])) 
                                M.empty nts
                completing ids (schedA, schedS) nts

        mkGraphs :: Sat -> M.Map Edge MyVar -> [Edge] ->
                        IO (FrGraph,TDPRes)
        mkGraphs sat varMap dps = do
            idsf <- newArray nbounds IS.empty :: IO (IOArray Vertex Vertices)
            idst <- newArray nbounds IS.empty :: IO (IOArray Vertex Vertices)
            edp  <- newArray pbounds IS.empty :: IO (IOArray Vertex Vertices)
            let (ids) = (idsf,idst)
            sequence_ [ do  v <- value sat pred
                            case v of
                              Nothing -> error "no val"
                              Just True -> addEdges (i,s) (zip ios sos) ids edp
                              Just False-> addEdges (s,i) (zip sos ios) ids edp
                    | Nt _ _ _ inhs outs _ <- nts
                    , (i,ios,_) <- inhs
                    , (s,sos,_) <- outs
                    , let pred = varMap M.! (i,s)
                    ]
            forM dps $ \(f,t) -> do
                modifyArray edp t (f `IS.insert`)
            f_idsf <- freeze idsf
            f_idst <- freeze idst
            f_edp  <- freeze edp
            return ((f_idsf,f_idst),f_edp)
         where  addEdges (f,t) es (idsf,idst) edp = do
                    modifyArray idsf f (t `IS.insert`)
                    modifyArray idst t (f `IS.insert`)
                    forM es $ \(f,t) -> do --edp does not reflect flow
                        modifyArray edp t (f `IS.insert`)                

noCyclesNt :: Sat -> NtGraph -> IO ()
noCyclesNt sat g  | IM.null g  = return ()
                  | otherwise = do
    news <- sequence [ noTriangleNt sat g p q | (p,q) <- pairs neighs]
    let g' = addsNt (removeNt node neighs g) (concat news)
    noCyclesNt sat g'
    where   
        node = snd $ minimum   [ (length xs, a) | (a,xs) <- IM.toList g ]
        Just neighs = IM.lookup node g

noTriangleNt :: Sat -> NtGraph -> (Vertex, MyVar) -> 
                          (Vertex, MyVar) -> IO [(Vertex,(Vertex,MyVar))]
noTriangleNt sat g e1@(t1,c1) e2@(t2,c2) = 
    case IM.lookup t1 g of
      Just ns -> 
        case [ c | (t2',c) <- ns, t2' == t2 ] of
          [] -> do  c3 <- newLit sat
                    ruleOut c1 c2 c3
                    return [(t1,(t2,c3))]
          [c3] -> ruleOut c1 c2 c3 >> return []
          _   -> error "multiple edges between two nodes" 
      Nothing -> error "pointer outside of graph"
 where 
        ruleOut ea eb ab= do addClause sat [ea, ab, varnot eb]
                             addClause sat [varnot ea,varnot ab,eb]

noCyclesPr :: Sat -> ToChordal -> IO ()
noCyclesPr sat g  | IM.null g  = return ()
                  | otherwise = do
    news <- sequence [ noTriangle sat g p q | (p,q) <- validPairs neighs]
    let g' = adds (remove node neighs g) (concat news)
    noCyclesPr sat g'
    where   
        node = snd $ minimum   [ (weight xs, a) 
                               | (a,xs) <- IM.toList g ]
        Just neighs = IM.lookup node g
        validPairs ns = [ (p,q)  | p <- sibs,  q <- nsibs ] ++
                        [ (p,q)  | p <- nsibs, q <- anys  ] ++
                        [ (p,q)  | p <- sibs,  q <- anys  ]
                      ++  (pairs anys)
         where sibs = [ n | n@(_,Sib _)  <- ns ]
               nsibs= [ n | n@(_,NSib _) <- ns ]
               anys = [ n | n@(_,Any _)  <- ns ]
        weight :: [Neigh] -> Int
        weight xs = ss*3 * (ds + cs) + (ds + cs)^2
         where
          ss = length [ x | x@(_,Sib _)  <- xs ]
          ds = length [ x | x@(_,NSib _) <- xs ]
          cs = length [ x | x@(_,Any _)  <- xs ]

noTriangle :: Sat -> ToChordal -> Neigh -> Neigh -> IO [SatEdge]
noTriangle sat g e1@(t1,c1) e2@(t2,c2) = 
    case IM.lookup t1 g of
      Just ns -> 
        case [ c | (t2',c) <- ns, t2' == t2 ] of
          [] -> do  p <- newLit sat
                    ruleOut c1 c2 (Any p)
                    return [(t1,(t2,(Any p)))]
          [c3] -> ruleOut c1 c2 c3 >> return [] 
          _   -> error "multiple edges between two nodes" 
      Nothing -> error "pointer outside of graph"
 where  ruleOut ea eb ab= addClauses sat [[ea, ab, neg eb],[neg ea,neg ab,eb]] 
noNtCycles :: Sat -> [Nt] -> (String -> IO ()) -> IO VarMap 
noNtCycles sat tps putStrLn = do
    putStrLn "--- Non-Terminals ---"
    maps <- mapM forNt tps
    return $ M.unions maps
 where  -- at non-terminal level, all cycles are between siblings
        -- that is why we force all the edges to be the same
        -- (not filtered by validPairs) 
        forNt tp@(Nt tid dpf dpt inhs syns _) = do
            vars <- satValues sat
            putStrLn ("nt : " ++ tid ++ " ... " ++ 
                        show vars ++ " ...")
            ass <- sequence $
                    [ return ((i,s),VarTrue) | ((i,s)) <- dpf ]++
                    [ return ((i,s),VarFalse)| ((s,i)) <- dpt ] 
            let assM = M.fromList ass
            mvars<- sequence   
                    [ if new then do
                        p <- newLit sat
                        return $ Just ((i,s),p) else return Nothing
                    | (i,_,_) <- inhs
                    , (s,_,_) <- syns
                    , let mmv = maybe (M.lookup (s,i) assM) Just 
                                      (M.lookup (i,s) assM)
                          new = isNothing mmv ]
            let vars = ass ++ catMaybes mvars
            g <- toNtGraph vars 
            noCyclesNt sat g
            return $ M.fromList vars


noPrCycles :: Sat -> [Pr] -> VarMap -> (String -> IO ()) -> IO ()
noPrCycles sat prods varMap putStrLn = do
    putStrLn "--- Productions ---"
    mapM_ forProd prods
 where  forProd (Pr prod es ses fs) | length fs ==1 = return () --taken care of
                                    | otherwise =  do 
            vars <- satValues sat
            putStrLn ("prod: " ++ show prod ++ " ... " ++ 
                           show vars ++ " ...") 
            g <- toChordal (sibs ++ dps) 
            noCyclesPr sat g
         where sibs= [ ((f,t),Sib c) | (e@(f,t),ge,_) <- ses
                     , let c = case M.lookup ge varMap of
                                      Just p  -> p
                                      Nothing -> error "no var found"]
               dps = [ (e,NSib VarTrue) | e <- es ]


