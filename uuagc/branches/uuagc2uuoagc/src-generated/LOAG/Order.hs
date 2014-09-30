{-# LANGUAGE Rank2Types, GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LOAG.Order where

{-# LINE 10 "src-ag/LOAG/Rep.ag" #-}

import Data.List (intercalate, foldl', nub)
import Data.Tuple (swap)
import Control.Arrow
{-# LINE 12 "dist/build/LOAG/Order.hs" #-}

{-# LINE 2 "src-ag/HsToken.ag" #-}

import CommonTypes
import UU.Scanner.Position(Pos)
{-# LINE 18 "dist/build/LOAG/Order.hs" #-}

{-# LINE 2 "src-ag/Expression.ag" #-}

import UU.Scanner.Position(Pos)
import HsToken
{-# LINE 24 "dist/build/LOAG/Order.hs" #-}

{-# LINE 2 "src-ag/CodeSyntax.ag" #-}

import Patterns
import CommonTypes
import Data.Map(Map)
import Data.Set(Set)
{-# LINE 32 "dist/build/LOAG/Order.hs" #-}

{-# LINE 2 "src-ag/Patterns.ag" #-}

-- Patterns.ag imports
import UU.Scanner.Position(Pos)
import CommonTypes (ConstructorIdent,Identifier)
{-# LINE 39 "dist/build/LOAG/Order.hs" #-}

{-# LINE 2 "src-ag/AbstractSyntax.ag" #-}

-- AbstractSyntax.ag imports
import Data.Set(Set)
import Data.Map(Map)
import Patterns    (Pattern(..),Patterns)
import Expression  (Expression(..))
import Macro --marcos
import CommonTypes
import ErrorMessages
{-# LINE 51 "dist/build/LOAG/Order.hs" #-}

{-# LINE 13 "src-ag/LOAG/Order.ag" #-}

import qualified Data.Array as A
import qualified Data.Map as Map
import qualified Data.IntMap as IMap
import qualified Data.Set as Set
import qualified Data.IntSet as IS
import qualified Data.Sequence as Seq
import qualified CommonTypes as CT
import Control.Monad (forM,when)
import Control.Monad.ST
import Data.Maybe(catMaybes)
import Data.Monoid(mappend,mempty)
import Data.STRef
import AbstractSyntax
import qualified LOAG.AOAG as   AOAG
import LOAG.Common
import LOAG.Chordal
import LOAG.Rep
import LOAG.Graphs
import CodeSyntax
import Data.Maybe (isJust, fromJust)
import ExecutionPlan
import GrammarInfo
import HsToken (HsToken(..))
import Pretty
import qualified System.IO as IO
import           System.IO.Unsafe
{-# LINE 81 "dist/build/LOAG/Order.hs" #-}
import Control.Monad.Identity (Identity)
import qualified Control.Monad.Identity
{-# LINE 13 "src-ag/LOAG/Prepare.ag" #-}

-- | Translating UUAGC types to MyTypes
drhs f | f == _LHS               = Inh
       | f == _LOC               = AnyDir
       | f == _INST              = AnyDir
       | otherwise               = Syn
dlhs f | f == _LHS               = Syn
       | f == _LOC               = AnyDir
       | f == _INST              = AnyDir
       | otherwise               = Inh 

depToEdge :: PMP_R -> PLabel -> Dependency -> Edge
depToEdge pmpr p e = 
    (findWithErr pmpr "depToEdge" $ MyOccurrence (p,getName f1) (getName i1, drhs f1),
     findWithErr pmpr "depToEdge" $ MyOccurrence (p,getName f2) (getName i2, dlhs f2))
    where Dependency (OccAttr f1 i1) (OccAttr f2 i2) = e

vertexToAttr :: NMP -> Vertex -> Attributes
vertexToAttr nmp v = Map.singleton (identifier a) (fromMyTy ty)
    where (MyAttribute ty (a,_)) = findWithErr nmp "vertexToAttr" v

{-# LINE 106 "dist/build/LOAG/Order.hs" #-}

{-# LINE 292 "src-ag/LOAG/Prepare.ag" #-}

-- | Replace the references to local attributes, by his attrs dependencies
repLocRefs :: SF_P -> SF_P -> SF_P
repLocRefs lfp sfp =
    Map.map (setConcatMap rep) sfp
    where rep :: MyOccurrence -> Set.Set MyOccurrence 
          rep occ | isLoc occ   = setConcatMap rep $ 
                                    findWithErr lfp "repping locals" occ
                  | otherwise   = Set.singleton occ
{-# LINE 118 "dist/build/LOAG/Order.hs" #-}

{-# LINE 42 "src-ag/LOAG/Order.ag" #-}

fst' (a,_,_) = a
snd' (_,b,_) = b
trd' (_,_,c) = c
{-# LINE 125 "dist/build/LOAG/Order.hs" #-}

{-# LINE 95 "src-ag/LOAG/Order.ag" #-}

data AltAttr = AltAttr Identifier Identifier Bool
               deriving (Eq, Ord, Show)

edgeToDep :: PMP -> Edge -> Dependency
edgeToDep pmp (f,t) = 
    Dependency (OccAttr (identifier f1) (identifier i1)) 
               (OccAttr (identifier f2) (identifier i2))
    where (MyOccurrence (_,f1) (i1,_),MyOccurrence (_,f2) (i2,_))
            = (findWithErr pmp "edgeToDep" f, 
               findWithErr pmp "edgeToDep" t) 

ppAds :: Options -> PMP -> [Edge] -> PP_Doc
ppAds opts pmp = foldr ((>-<) . ppEdge opts pmp) empty

ppEdge :: Options -> PMP -> Edge -> PP_Doc
ppEdge opts pmp (f,t) = 
    text sem    >#< text (show ty) >|< " | " >|< text p >|< "   "
                >|< ppOcc pmp f >|< text " < " >|< ppOcc pmp t
 where (MyOccurrence ((ty,p),_) _) = pmp Map.! f
       sem | lcKeywords opts = "sem"
           | otherwise       = "SEM"
   
ppOcc :: PMP -> Vertex -> PP_Doc
ppOcc pmp v = text f >|< text "." >|< fst a
 where (MyOccurrence ((t,p),f) a) = findWithErr pmp "ppOcc" v

{-# LINE 155 "dist/build/LOAG/Order.hs" #-}

{-# LINE 239 "src-ag/LOAG/Order.ag" #-}

getVss (done,intros,rules,vnrs) ps tdp synsO lfp nmpr pmp pmpr fty visMap ruleMap hoMap = do
    ref   <- newSTRef done
    introed   <- newSTRef intros
    ruleref   <- newSTRef rules 
    vnrsref   <- newSTRef vnrs
    lists <- forM synsO (visit ref introed ruleref vnrsref . (pmp Map.!))
    done  <- readSTRef ref
    intros  <- readSTRef introed
    rules  <- readSTRef ruleref
    vnrs  <- readSTRef vnrsref 
    return (concat lists, (done, intros, rules, vnrs))
 where 
    hochildren = maybe Set.empty id $ Map.lookup ps hoMap
    visit ref introed ruleref vnrsref o@(MyOccurrence (_,f) (_,d)) = do
        visited <- readSTRef ref
        if (o `Set.member` visited) 
         then return [] -- already visited
         else do        -- prevent doubles
          modifySTRef ref (Set.insert o)
          if inOutput
           then do -- has to be calculated in this sequence
                rest' <- rest
                locs' <- locs
                sem'  <- sem o
                return $ (rest' ++ locs' ++ sem')
           else if "lhs" == (snd $ argsOf o)
                 then return [] -- inherited of parent, nothing todo
                 else do   -- other input occurrence, perform visit
                    locs' <- locs
                    rest' <- rest
                    visit'<- toVisit o
                    return (rest' ++ locs' ++ visit')
     where preds  = maybe [] (IS.toList . (tdp A.!)) $ Map.lookup o pmpr
           rest   = forM preds (visit ref introed ruleref vnrsref. (pmp Map.!)) 
                        >>= (return . concat)
           free   = maybe [] (Set.toList) $ Map.lookup o lfp
           locs   = forM free (visit ref introed ruleref vnrsref)
                        >>= (return . concat)
           sem o  = do  rules <- readSTRef ruleref 
                        if r `Set.member` rules
                          then return []
                          else do   writeSTRef ruleref (r `Set.insert` rules) 
                                    return [Sem r]
            where r = maybe (error "ruleMap") id $ Map.lookup o ruleMap
           inOutput = f == "lhs" && d == Syn || f /= "lhs" && d == Inh
           toVisit o = do
             vnrs <- readSTRef vnrsref 
             if (child,visnr) `Set.member` vnrs
              then return []
              else writeSTRef vnrsref ((child,visnr) `Set.insert` vnrs) >>
                   if child `Set.member` hochildren 
                   then do intros <- readSTRef introed
                           case child `Set.member` intros of
                            True    -> return [cvisit]
                            False   -> do
                                writeSTRef introed (Set.insert child intros)
                                let occ = (ps,"inst") >.< (child, AnyDir)
                                    preds = Set.toList $ setConcatMap rep $ 
                                                        findWithErr lfp "woot4" occ
                                    rep :: MyOccurrence -> Set.Set MyOccurrence 
                                    rep occ | isLoc occ   = Set.insert occ $ 
                                                setConcatMap rep $ findWithErr lfp "woot3" occ
                                            | otherwise   = Set.singleton occ
                                rest <- forM preds 
                                            (visit ref introed ruleref vnrsref)
                                sem' <- sem occ
                                return $ (concat rest) ++
                                         sem' ++
                                         [ChildIntro (identifier child)] ++
                                         [cvisit]
                   else return [cvisit]
             where  cvisit= ChildVisit (identifier child) ntid visnr
                    child = snd $ argsOf o
                    ntid  = ((\(NT name _ _ )-> name) . fromMyTy) nt 
                    visnr = (\x-> findWithErr' visMap (show (inOutput,o,x)) x) (findWithErr nmpr "woot3" (nt <.> attr o))
                    nt    = findWithErr fty "woot" (ps,child)
{-# LINE 235 "dist/build/LOAG/Order.hs" #-}

{-# LINE 356 "src-ag/LOAG/Order.ag" #-}

repToAg :: LOAGRep -> Grammar -> Ag
repToAg sem (Grammar _ _ _ _ dats _ _ _ _ _ _ _ _ _) = 
    Ag bounds_s bounds_p de (map toNt dats)
 where
    pmp  = (pmp_LOAGRep_LOAGRep  sem)
    pmpr = (pmpr_LOAGRep_LOAGRep sem)
    nmp  = (nmp_LOAGRep_LOAGRep  sem)
    nmpr = (nmpr_LOAGRep_LOAGRep sem)
    genA = gen_LOAGRep_LOAGRep sem
    fieldM  = fieldMap_LOAGRep_LOAGRep sem
    genEdge (f,t) = (gen f, gen t)
    fsInP  = map2F (fsInP_LOAGRep_LOAGRep sem)
    siblings (f, t) = ofld A.! f == ofld A.! t
    ofld = (ofld_LOAGRep_LOAGRep sem)
    sfp  = map2F' (sfp_LOAGRep_LOAGRep sem)
    afp  = filter inOutput . ap
    ap   = map (findWithErr pmpr "building ap") . map2F (ap_LOAGRep_LOAGRep  sem)
    inss = inss_LOAGRep_LOAGRep sem 
    gen v = genA A.! v
    ain  = map (findWithErr nmpr "building an") . map2F (ain_LOAGRep_LOAGRep sem)
    asn  = map (findWithErr nmpr "building an") . map2F (asn_LOAGRep_LOAGRep sem)
    inOutput = not . inContext 
    inContext f = (f1 == "lhs" && d1 == Inh || f1 /= "lhs" && d1 == Syn) 
        where (MyOccurrence (_,f1) (_,d1)) = pmp Map.! f
    de    = [ e      | p <- ps,   e <- dpe p ]
    dpe p = [ (findWithErr pmpr "building dpe" a, b) 
            | b <- ap p, a <- Set.toList $ sfp (findWithErr pmp "fetching sfp" b) ]
    ps   = ps_LOAGRep_LOAGRep   sem
    bounds_p = if Map.null pmp then (0,-1) 
                else (fst $ Map.findMin pmp, fst $ Map.findMax pmp)
    bounds_s = if Map.null nmp then (0,-1) 
                else (fst $ Map.findMin nmp, fst $ Map.findMax nmp)
 


    toNt :: Nonterminal -> Nt
    toNt (Nonterminal ntid _ _ _ prods) = Nt nt dpf dpt 
            (addD Inh $ ain ty) (addD Syn $ asn ty) (map (toPr ty) prods)
     where nt  = getName ntid
           ty  = TyData nt
           dpt =  [ (as, ai) | ai <- ain ty
                   , as <- nub$ [ gen s |
                                  i <- inss A.! ai
                                , s <- map (pmpr Map.!) $ 
                                    Set.toList (sfp $ pmp Map.! i)
                                , siblings (s,i)]]
           dpf =  [ (ai, as) | as <- asn ty
                   , ai <- nub$ [ gen i |
                                  s <- inss A.! as
                                , i <- map (pmpr Map.!) $
                                    Set.toList (sfp $ pmp Map.! s)
                                , siblings (i,s)]]
           addD d = map (\i -> (i,inss A.! i,d))
    toPr :: MyType -> Production -> Pr
    toPr ty (Production con _ _ _ _ _ _) = 
                Pr p dpp fc_occs (map toFd $ fsInP p)
     where p = (ty, getName con)
           dpp = [ (f',t)
                    | t <- afp p, f <- (Set.toList $ sfp (pmp Map.! t))
                    , let f' = pmpr Map.! f
                    , not (siblings (f',t))]
           fc_occs = foldl' match [] fss
            where fss = fsInP p
           match s fs = [ ready (inp, out) lhs | inp <- Set.toList inhs
                                           , out <- Set.toList syns] ++ s
            where ((inhs, syns), lhs)
                               | (snd fs) /= "lhs" = 
                                    (swap (fieldM Map.! fs),False)
                               | otherwise = (fieldM Map.! fs, True)
                  ready e@(f,t) b = (e', genEdge e', b)
                   where e' = (pmpr Map.! f, pmpr Map.! t)
    toFd :: (PLabel, FLabel) -> Fd
    toFd fs@((TyData ty, pr), fd) = Fd fd ty inhs syns
     where (is,ss) = fieldM Map.! fs
           inhs = map (((genA A.!) &&& id).(pmpr Map.!))$ Set.toList is
           syns = map (((genA A.!) &&& id).(pmpr Map.!))$ Set.toList ss


{-# LINE 317 "dist/build/LOAG/Order.hs" #-}
-- CGrammar ----------------------------------------------------
-- wrapper
data Inh_CGrammar  = Inh_CGrammar {  }
data Syn_CGrammar  = Syn_CGrammar { self_Syn_CGrammar :: (CGrammar) }
{-# INLINABLE wrap_CGrammar #-}
wrap_CGrammar :: T_CGrammar  -> Inh_CGrammar  -> (Syn_CGrammar )
wrap_CGrammar (T_CGrammar act) (Inh_CGrammar ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg1 = T_CGrammar_vIn1 
        (T_CGrammar_vOut1 _lhsOself) <- return (inv_CGrammar_s2 sem arg1)
        return (Syn_CGrammar _lhsOself)
   )

-- cata
{-# INLINE sem_CGrammar #-}
sem_CGrammar :: CGrammar  -> T_CGrammar 
sem_CGrammar ( CGrammar typeSyns_ derivings_ wrappers_ nonts_ pragmas_ paramMap_ contextMap_ quantMap_ aroundsMap_ mergeMap_ multivisit_ ) = sem_CGrammar_CGrammar typeSyns_ derivings_ wrappers_ ( sem_CNonterminals nonts_ ) pragmas_ paramMap_ contextMap_ quantMap_ aroundsMap_ mergeMap_ multivisit_

-- semantic domain
newtype T_CGrammar  = T_CGrammar {
                                 attach_T_CGrammar :: Identity (T_CGrammar_s2 )
                                 }
newtype T_CGrammar_s2  = C_CGrammar_s2 {
                                       inv_CGrammar_s2 :: (T_CGrammar_v1 )
                                       }
data T_CGrammar_s3  = C_CGrammar_s3
type T_CGrammar_v1  = (T_CGrammar_vIn1 ) -> (T_CGrammar_vOut1 )
data T_CGrammar_vIn1  = T_CGrammar_vIn1 
data T_CGrammar_vOut1  = T_CGrammar_vOut1 (CGrammar)
{-# NOINLINE sem_CGrammar_CGrammar #-}
sem_CGrammar_CGrammar :: (TypeSyns) -> (Derivings) -> (Set NontermIdent) -> T_CNonterminals  -> (PragmaMap) -> (ParamMap) -> (ContextMap) -> (QuantMap) -> (Map NontermIdent (Map ConstructorIdent (Set Identifier))) -> (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier])))) -> (Bool) -> T_CGrammar 
sem_CGrammar_CGrammar arg_typeSyns_ arg_derivings_ arg_wrappers_ arg_nonts_ arg_pragmas_ arg_paramMap_ arg_contextMap_ arg_quantMap_ arg_aroundsMap_ arg_mergeMap_ arg_multivisit_ = T_CGrammar (return st2) where
   {-# NOINLINE st2 #-}
   st2 = let
      v1 :: T_CGrammar_v1 
      v1 = \ (T_CGrammar_vIn1 ) -> ( let
         _nontsX11 = Control.Monad.Identity.runIdentity (attach_T_CNonterminals (arg_nonts_))
         (T_CNonterminals_vOut10 _nontsIself) = inv_CNonterminals_s11 _nontsX11 (T_CNonterminals_vIn10 )
         _self = rule0 _nontsIself arg_aroundsMap_ arg_contextMap_ arg_derivings_ arg_mergeMap_ arg_multivisit_ arg_paramMap_ arg_pragmas_ arg_quantMap_ arg_typeSyns_ arg_wrappers_
         _lhsOself :: CGrammar
         _lhsOself = rule1 _self
         __result_ = T_CGrammar_vOut1 _lhsOself
         in __result_ )
     in C_CGrammar_s2 v1
   {-# INLINE rule0 #-}
   rule0 = \ ((_nontsIself) :: CNonterminals) aroundsMap_ contextMap_ derivings_ mergeMap_ multivisit_ paramMap_ pragmas_ quantMap_ typeSyns_ wrappers_ ->
     CGrammar typeSyns_ derivings_ wrappers_ _nontsIself pragmas_ paramMap_ contextMap_ quantMap_ aroundsMap_ mergeMap_ multivisit_
   {-# INLINE rule1 #-}
   rule1 = \ _self ->
     _self

-- CInterface --------------------------------------------------
-- wrapper
data Inh_CInterface  = Inh_CInterface {  }
data Syn_CInterface  = Syn_CInterface { self_Syn_CInterface :: (CInterface) }
{-# INLINABLE wrap_CInterface #-}
wrap_CInterface :: T_CInterface  -> Inh_CInterface  -> (Syn_CInterface )
wrap_CInterface (T_CInterface act) (Inh_CInterface ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg4 = T_CInterface_vIn4 
        (T_CInterface_vOut4 _lhsOself) <- return (inv_CInterface_s5 sem arg4)
        return (Syn_CInterface _lhsOself)
   )

-- cata
{-# INLINE sem_CInterface #-}
sem_CInterface :: CInterface  -> T_CInterface 
sem_CInterface ( CInterface seg_ ) = sem_CInterface_CInterface ( sem_CSegments seg_ )

-- semantic domain
newtype T_CInterface  = T_CInterface {
                                     attach_T_CInterface :: Identity (T_CInterface_s5 )
                                     }
newtype T_CInterface_s5  = C_CInterface_s5 {
                                           inv_CInterface_s5 :: (T_CInterface_v4 )
                                           }
data T_CInterface_s6  = C_CInterface_s6
type T_CInterface_v4  = (T_CInterface_vIn4 ) -> (T_CInterface_vOut4 )
data T_CInterface_vIn4  = T_CInterface_vIn4 
data T_CInterface_vOut4  = T_CInterface_vOut4 (CInterface)
{-# NOINLINE sem_CInterface_CInterface #-}
sem_CInterface_CInterface :: T_CSegments  -> T_CInterface 
sem_CInterface_CInterface arg_seg_ = T_CInterface (return st5) where
   {-# NOINLINE st5 #-}
   st5 = let
      v4 :: T_CInterface_v4 
      v4 = \ (T_CInterface_vIn4 ) -> ( let
         _segX26 = Control.Monad.Identity.runIdentity (attach_T_CSegments (arg_seg_))
         (T_CSegments_vOut25 _segIself) = inv_CSegments_s26 _segX26 (T_CSegments_vIn25 )
         _self = rule2 _segIself
         _lhsOself :: CInterface
         _lhsOself = rule3 _self
         __result_ = T_CInterface_vOut4 _lhsOself
         in __result_ )
     in C_CInterface_s5 v4
   {-# INLINE rule2 #-}
   rule2 = \ ((_segIself) :: CSegments) ->
     CInterface _segIself
   {-# INLINE rule3 #-}
   rule3 = \ _self ->
     _self

-- CNonterminal ------------------------------------------------
-- wrapper
data Inh_CNonterminal  = Inh_CNonterminal {  }
data Syn_CNonterminal  = Syn_CNonterminal { self_Syn_CNonterminal :: (CNonterminal) }
{-# INLINABLE wrap_CNonterminal #-}
wrap_CNonterminal :: T_CNonterminal  -> Inh_CNonterminal  -> (Syn_CNonterminal )
wrap_CNonterminal (T_CNonterminal act) (Inh_CNonterminal ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg7 = T_CNonterminal_vIn7 
        (T_CNonterminal_vOut7 _lhsOself) <- return (inv_CNonterminal_s8 sem arg7)
        return (Syn_CNonterminal _lhsOself)
   )

-- cata
{-# INLINE sem_CNonterminal #-}
sem_CNonterminal :: CNonterminal  -> T_CNonterminal 
sem_CNonterminal ( CNonterminal nt_ params_ inh_ syn_ prods_ inter_ ) = sem_CNonterminal_CNonterminal nt_ params_ inh_ syn_ ( sem_CProductions prods_ ) ( sem_CInterface inter_ )

-- semantic domain
newtype T_CNonterminal  = T_CNonterminal {
                                         attach_T_CNonterminal :: Identity (T_CNonterminal_s8 )
                                         }
newtype T_CNonterminal_s8  = C_CNonterminal_s8 {
                                               inv_CNonterminal_s8 :: (T_CNonterminal_v7 )
                                               }
data T_CNonterminal_s9  = C_CNonterminal_s9
type T_CNonterminal_v7  = (T_CNonterminal_vIn7 ) -> (T_CNonterminal_vOut7 )
data T_CNonterminal_vIn7  = T_CNonterminal_vIn7 
data T_CNonterminal_vOut7  = T_CNonterminal_vOut7 (CNonterminal)
{-# NOINLINE sem_CNonterminal_CNonterminal #-}
sem_CNonterminal_CNonterminal :: (NontermIdent) -> ([Identifier]) -> (Attributes) -> (Attributes) -> T_CProductions  -> T_CInterface  -> T_CNonterminal 
sem_CNonterminal_CNonterminal arg_nt_ arg_params_ arg_inh_ arg_syn_ arg_prods_ arg_inter_ = T_CNonterminal (return st8) where
   {-# NOINLINE st8 #-}
   st8 = let
      v7 :: T_CNonterminal_v7 
      v7 = \ (T_CNonterminal_vIn7 ) -> ( let
         _prodsX17 = Control.Monad.Identity.runIdentity (attach_T_CProductions (arg_prods_))
         _interX5 = Control.Monad.Identity.runIdentity (attach_T_CInterface (arg_inter_))
         (T_CProductions_vOut16 _prodsIself) = inv_CProductions_s17 _prodsX17 (T_CProductions_vIn16 )
         (T_CInterface_vOut4 _interIself) = inv_CInterface_s5 _interX5 (T_CInterface_vIn4 )
         _self = rule4 _interIself _prodsIself arg_inh_ arg_nt_ arg_params_ arg_syn_
         _lhsOself :: CNonterminal
         _lhsOself = rule5 _self
         __result_ = T_CNonterminal_vOut7 _lhsOself
         in __result_ )
     in C_CNonterminal_s8 v7
   {-# INLINE rule4 #-}
   rule4 = \ ((_interIself) :: CInterface) ((_prodsIself) :: CProductions) inh_ nt_ params_ syn_ ->
     CNonterminal nt_ params_ inh_ syn_ _prodsIself _interIself
   {-# INLINE rule5 #-}
   rule5 = \ _self ->
     _self

-- CNonterminals -----------------------------------------------
-- wrapper
data Inh_CNonterminals  = Inh_CNonterminals {  }
data Syn_CNonterminals  = Syn_CNonterminals { self_Syn_CNonterminals :: (CNonterminals) }
{-# INLINABLE wrap_CNonterminals #-}
wrap_CNonterminals :: T_CNonterminals  -> Inh_CNonterminals  -> (Syn_CNonterminals )
wrap_CNonterminals (T_CNonterminals act) (Inh_CNonterminals ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg10 = T_CNonterminals_vIn10 
        (T_CNonterminals_vOut10 _lhsOself) <- return (inv_CNonterminals_s11 sem arg10)
        return (Syn_CNonterminals _lhsOself)
   )

-- cata
{-# NOINLINE sem_CNonterminals #-}
sem_CNonterminals :: CNonterminals  -> T_CNonterminals 
sem_CNonterminals list = Prelude.foldr sem_CNonterminals_Cons sem_CNonterminals_Nil (Prelude.map sem_CNonterminal list)

-- semantic domain
newtype T_CNonterminals  = T_CNonterminals {
                                           attach_T_CNonterminals :: Identity (T_CNonterminals_s11 )
                                           }
newtype T_CNonterminals_s11  = C_CNonterminals_s11 {
                                                   inv_CNonterminals_s11 :: (T_CNonterminals_v10 )
                                                   }
data T_CNonterminals_s12  = C_CNonterminals_s12
type T_CNonterminals_v10  = (T_CNonterminals_vIn10 ) -> (T_CNonterminals_vOut10 )
data T_CNonterminals_vIn10  = T_CNonterminals_vIn10 
data T_CNonterminals_vOut10  = T_CNonterminals_vOut10 (CNonterminals)
{-# NOINLINE sem_CNonterminals_Cons #-}
sem_CNonterminals_Cons :: T_CNonterminal  -> T_CNonterminals  -> T_CNonterminals 
sem_CNonterminals_Cons arg_hd_ arg_tl_ = T_CNonterminals (return st11) where
   {-# NOINLINE st11 #-}
   st11 = let
      v10 :: T_CNonterminals_v10 
      v10 = \ (T_CNonterminals_vIn10 ) -> ( let
         _hdX8 = Control.Monad.Identity.runIdentity (attach_T_CNonterminal (arg_hd_))
         _tlX11 = Control.Monad.Identity.runIdentity (attach_T_CNonterminals (arg_tl_))
         (T_CNonterminal_vOut7 _hdIself) = inv_CNonterminal_s8 _hdX8 (T_CNonterminal_vIn7 )
         (T_CNonterminals_vOut10 _tlIself) = inv_CNonterminals_s11 _tlX11 (T_CNonterminals_vIn10 )
         _self = rule6 _hdIself _tlIself
         _lhsOself :: CNonterminals
         _lhsOself = rule7 _self
         __result_ = T_CNonterminals_vOut10 _lhsOself
         in __result_ )
     in C_CNonterminals_s11 v10
   {-# INLINE rule6 #-}
   rule6 = \ ((_hdIself) :: CNonterminal) ((_tlIself) :: CNonterminals) ->
     (:) _hdIself _tlIself
   {-# INLINE rule7 #-}
   rule7 = \ _self ->
     _self
{-# NOINLINE sem_CNonterminals_Nil #-}
sem_CNonterminals_Nil ::  T_CNonterminals 
sem_CNonterminals_Nil  = T_CNonterminals (return st11) where
   {-# NOINLINE st11 #-}
   st11 = let
      v10 :: T_CNonterminals_v10 
      v10 = \ (T_CNonterminals_vIn10 ) -> ( let
         _self = rule8  ()
         _lhsOself :: CNonterminals
         _lhsOself = rule9 _self
         __result_ = T_CNonterminals_vOut10 _lhsOself
         in __result_ )
     in C_CNonterminals_s11 v10
   {-# INLINE rule8 #-}
   rule8 = \  (_ :: ()) ->
     []
   {-# INLINE rule9 #-}
   rule9 = \ _self ->
     _self

-- CProduction -------------------------------------------------
-- wrapper
data Inh_CProduction  = Inh_CProduction {  }
data Syn_CProduction  = Syn_CProduction { self_Syn_CProduction :: (CProduction) }
{-# INLINABLE wrap_CProduction #-}
wrap_CProduction :: T_CProduction  -> Inh_CProduction  -> (Syn_CProduction )
wrap_CProduction (T_CProduction act) (Inh_CProduction ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg13 = T_CProduction_vIn13 
        (T_CProduction_vOut13 _lhsOself) <- return (inv_CProduction_s14 sem arg13)
        return (Syn_CProduction _lhsOself)
   )

-- cata
{-# INLINE sem_CProduction #-}
sem_CProduction :: CProduction  -> T_CProduction 
sem_CProduction ( CProduction con_ visits_ children_ terminals_ ) = sem_CProduction_CProduction con_ ( sem_CVisits visits_ ) children_ terminals_

-- semantic domain
newtype T_CProduction  = T_CProduction {
                                       attach_T_CProduction :: Identity (T_CProduction_s14 )
                                       }
newtype T_CProduction_s14  = C_CProduction_s14 {
                                               inv_CProduction_s14 :: (T_CProduction_v13 )
                                               }
data T_CProduction_s15  = C_CProduction_s15
type T_CProduction_v13  = (T_CProduction_vIn13 ) -> (T_CProduction_vOut13 )
data T_CProduction_vIn13  = T_CProduction_vIn13 
data T_CProduction_vOut13  = T_CProduction_vOut13 (CProduction)
{-# NOINLINE sem_CProduction_CProduction #-}
sem_CProduction_CProduction :: (ConstructorIdent) -> T_CVisits  -> ([(Identifier,Type,ChildKind)]) -> ([Identifier]) -> T_CProduction 
sem_CProduction_CProduction arg_con_ arg_visits_ arg_children_ arg_terminals_ = T_CProduction (return st14) where
   {-# NOINLINE st14 #-}
   st14 = let
      v13 :: T_CProduction_v13 
      v13 = \ (T_CProduction_vIn13 ) -> ( let
         _visitsX32 = Control.Monad.Identity.runIdentity (attach_T_CVisits (arg_visits_))
         (T_CVisits_vOut31 _visitsIself) = inv_CVisits_s32 _visitsX32 (T_CVisits_vIn31 )
         _self = rule10 _visitsIself arg_children_ arg_con_ arg_terminals_
         _lhsOself :: CProduction
         _lhsOself = rule11 _self
         __result_ = T_CProduction_vOut13 _lhsOself
         in __result_ )
     in C_CProduction_s14 v13
   {-# INLINE rule10 #-}
   rule10 = \ ((_visitsIself) :: CVisits) children_ con_ terminals_ ->
     CProduction con_ _visitsIself children_ terminals_
   {-# INLINE rule11 #-}
   rule11 = \ _self ->
     _self

-- CProductions ------------------------------------------------
-- wrapper
data Inh_CProductions  = Inh_CProductions {  }
data Syn_CProductions  = Syn_CProductions { self_Syn_CProductions :: (CProductions) }
{-# INLINABLE wrap_CProductions #-}
wrap_CProductions :: T_CProductions  -> Inh_CProductions  -> (Syn_CProductions )
wrap_CProductions (T_CProductions act) (Inh_CProductions ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg16 = T_CProductions_vIn16 
        (T_CProductions_vOut16 _lhsOself) <- return (inv_CProductions_s17 sem arg16)
        return (Syn_CProductions _lhsOself)
   )

-- cata
{-# NOINLINE sem_CProductions #-}
sem_CProductions :: CProductions  -> T_CProductions 
sem_CProductions list = Prelude.foldr sem_CProductions_Cons sem_CProductions_Nil (Prelude.map sem_CProduction list)

-- semantic domain
newtype T_CProductions  = T_CProductions {
                                         attach_T_CProductions :: Identity (T_CProductions_s17 )
                                         }
newtype T_CProductions_s17  = C_CProductions_s17 {
                                                 inv_CProductions_s17 :: (T_CProductions_v16 )
                                                 }
data T_CProductions_s18  = C_CProductions_s18
type T_CProductions_v16  = (T_CProductions_vIn16 ) -> (T_CProductions_vOut16 )
data T_CProductions_vIn16  = T_CProductions_vIn16 
data T_CProductions_vOut16  = T_CProductions_vOut16 (CProductions)
{-# NOINLINE sem_CProductions_Cons #-}
sem_CProductions_Cons :: T_CProduction  -> T_CProductions  -> T_CProductions 
sem_CProductions_Cons arg_hd_ arg_tl_ = T_CProductions (return st17) where
   {-# NOINLINE st17 #-}
   st17 = let
      v16 :: T_CProductions_v16 
      v16 = \ (T_CProductions_vIn16 ) -> ( let
         _hdX14 = Control.Monad.Identity.runIdentity (attach_T_CProduction (arg_hd_))
         _tlX17 = Control.Monad.Identity.runIdentity (attach_T_CProductions (arg_tl_))
         (T_CProduction_vOut13 _hdIself) = inv_CProduction_s14 _hdX14 (T_CProduction_vIn13 )
         (T_CProductions_vOut16 _tlIself) = inv_CProductions_s17 _tlX17 (T_CProductions_vIn16 )
         _self = rule12 _hdIself _tlIself
         _lhsOself :: CProductions
         _lhsOself = rule13 _self
         __result_ = T_CProductions_vOut16 _lhsOself
         in __result_ )
     in C_CProductions_s17 v16
   {-# INLINE rule12 #-}
   rule12 = \ ((_hdIself) :: CProduction) ((_tlIself) :: CProductions) ->
     (:) _hdIself _tlIself
   {-# INLINE rule13 #-}
   rule13 = \ _self ->
     _self
{-# NOINLINE sem_CProductions_Nil #-}
sem_CProductions_Nil ::  T_CProductions 
sem_CProductions_Nil  = T_CProductions (return st17) where
   {-# NOINLINE st17 #-}
   st17 = let
      v16 :: T_CProductions_v16 
      v16 = \ (T_CProductions_vIn16 ) -> ( let
         _self = rule14  ()
         _lhsOself :: CProductions
         _lhsOself = rule15 _self
         __result_ = T_CProductions_vOut16 _lhsOself
         in __result_ )
     in C_CProductions_s17 v16
   {-# INLINE rule14 #-}
   rule14 = \  (_ :: ()) ->
     []
   {-# INLINE rule15 #-}
   rule15 = \ _self ->
     _self

-- CRule -------------------------------------------------------
-- wrapper
data Inh_CRule  = Inh_CRule {  }
data Syn_CRule  = Syn_CRule { self_Syn_CRule :: (CRule) }
{-# INLINABLE wrap_CRule #-}
wrap_CRule :: T_CRule  -> Inh_CRule  -> (Syn_CRule )
wrap_CRule (T_CRule act) (Inh_CRule ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg19 = T_CRule_vIn19 
        (T_CRule_vOut19 _lhsOself) <- return (inv_CRule_s20 sem arg19)
        return (Syn_CRule _lhsOself)
   )

-- cata
{-# NOINLINE sem_CRule #-}
sem_CRule :: CRule  -> T_CRule 
sem_CRule ( CRule name_ isIn_ hasCode_ nt_ con_ field_ childnt_ tp_ pattern_ rhs_ defines_ owrt_ origin_ uses_ explicit_ mbNamed_ ) = sem_CRule_CRule name_ isIn_ hasCode_ nt_ con_ field_ childnt_ tp_ ( sem_Pattern pattern_ ) rhs_ defines_ owrt_ origin_ uses_ explicit_ mbNamed_
sem_CRule ( CChildVisit name_ nt_ nr_ inh_ syn_ isLast_ ) = sem_CRule_CChildVisit name_ nt_ nr_ inh_ syn_ isLast_

-- semantic domain
newtype T_CRule  = T_CRule {
                           attach_T_CRule :: Identity (T_CRule_s20 )
                           }
newtype T_CRule_s20  = C_CRule_s20 {
                                   inv_CRule_s20 :: (T_CRule_v19 )
                                   }
data T_CRule_s21  = C_CRule_s21
type T_CRule_v19  = (T_CRule_vIn19 ) -> (T_CRule_vOut19 )
data T_CRule_vIn19  = T_CRule_vIn19 
data T_CRule_vOut19  = T_CRule_vOut19 (CRule)
{-# NOINLINE sem_CRule_CRule #-}
sem_CRule_CRule :: (Identifier) -> (Bool) -> (Bool) -> (NontermIdent) -> (ConstructorIdent) -> (Identifier) -> (Maybe NontermIdent) -> (Maybe Type) -> T_Pattern  -> ([String]) -> (Map Int (Identifier,Identifier,Maybe Type)) -> (Bool) -> (String) -> (Set (Identifier, Identifier)) -> (Bool) -> (Maybe Identifier) -> T_CRule 
sem_CRule_CRule arg_name_ arg_isIn_ arg_hasCode_ arg_nt_ arg_con_ arg_field_ arg_childnt_ arg_tp_ arg_pattern_ arg_rhs_ arg_defines_ arg_owrt_ arg_origin_ arg_uses_ arg_explicit_ arg_mbNamed_ = T_CRule (return st20) where
   {-# NOINLINE st20 #-}
   st20 = let
      v19 :: T_CRule_v19 
      v19 = \ (T_CRule_vIn19 ) -> ( let
         _patternX77 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pattern_))
         (T_Pattern_vOut76 _patternIafs _patternIcopy _patternIself) = inv_Pattern_s77 _patternX77 (T_Pattern_vIn76 )
         _self = rule16 _patternIself arg_childnt_ arg_con_ arg_defines_ arg_explicit_ arg_field_ arg_hasCode_ arg_isIn_ arg_mbNamed_ arg_name_ arg_nt_ arg_origin_ arg_owrt_ arg_rhs_ arg_tp_ arg_uses_
         _lhsOself :: CRule
         _lhsOself = rule17 _self
         __result_ = T_CRule_vOut19 _lhsOself
         in __result_ )
     in C_CRule_s20 v19
   {-# INLINE rule16 #-}
   rule16 = \ ((_patternIself) :: Pattern) childnt_ con_ defines_ explicit_ field_ hasCode_ isIn_ mbNamed_ name_ nt_ origin_ owrt_ rhs_ tp_ uses_ ->
     CRule name_ isIn_ hasCode_ nt_ con_ field_ childnt_ tp_ _patternIself rhs_ defines_ owrt_ origin_ uses_ explicit_ mbNamed_
   {-# INLINE rule17 #-}
   rule17 = \ _self ->
     _self
{-# NOINLINE sem_CRule_CChildVisit #-}
sem_CRule_CChildVisit :: (Identifier) -> (NontermIdent) -> (Int) -> (Attributes) -> (Attributes) -> (Bool) -> T_CRule 
sem_CRule_CChildVisit arg_name_ arg_nt_ arg_nr_ arg_inh_ arg_syn_ arg_isLast_ = T_CRule (return st20) where
   {-# NOINLINE st20 #-}
   st20 = let
      v19 :: T_CRule_v19 
      v19 = \ (T_CRule_vIn19 ) -> ( let
         _self = rule18 arg_inh_ arg_isLast_ arg_name_ arg_nr_ arg_nt_ arg_syn_
         _lhsOself :: CRule
         _lhsOself = rule19 _self
         __result_ = T_CRule_vOut19 _lhsOself
         in __result_ )
     in C_CRule_s20 v19
   {-# INLINE rule18 #-}
   rule18 = \ inh_ isLast_ name_ nr_ nt_ syn_ ->
     CChildVisit name_ nt_ nr_ inh_ syn_ isLast_
   {-# INLINE rule19 #-}
   rule19 = \ _self ->
     _self

-- CSegment ----------------------------------------------------
-- wrapper
data Inh_CSegment  = Inh_CSegment {  }
data Syn_CSegment  = Syn_CSegment { self_Syn_CSegment :: (CSegment) }
{-# INLINABLE wrap_CSegment #-}
wrap_CSegment :: T_CSegment  -> Inh_CSegment  -> (Syn_CSegment )
wrap_CSegment (T_CSegment act) (Inh_CSegment ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg22 = T_CSegment_vIn22 
        (T_CSegment_vOut22 _lhsOself) <- return (inv_CSegment_s23 sem arg22)
        return (Syn_CSegment _lhsOself)
   )

-- cata
{-# INLINE sem_CSegment #-}
sem_CSegment :: CSegment  -> T_CSegment 
sem_CSegment ( CSegment inh_ syn_ ) = sem_CSegment_CSegment inh_ syn_

-- semantic domain
newtype T_CSegment  = T_CSegment {
                                 attach_T_CSegment :: Identity (T_CSegment_s23 )
                                 }
newtype T_CSegment_s23  = C_CSegment_s23 {
                                         inv_CSegment_s23 :: (T_CSegment_v22 )
                                         }
data T_CSegment_s24  = C_CSegment_s24
type T_CSegment_v22  = (T_CSegment_vIn22 ) -> (T_CSegment_vOut22 )
data T_CSegment_vIn22  = T_CSegment_vIn22 
data T_CSegment_vOut22  = T_CSegment_vOut22 (CSegment)
{-# NOINLINE sem_CSegment_CSegment #-}
sem_CSegment_CSegment :: (Attributes) -> (Attributes) -> T_CSegment 
sem_CSegment_CSegment arg_inh_ arg_syn_ = T_CSegment (return st23) where
   {-# NOINLINE st23 #-}
   st23 = let
      v22 :: T_CSegment_v22 
      v22 = \ (T_CSegment_vIn22 ) -> ( let
         _self = rule20 arg_inh_ arg_syn_
         _lhsOself :: CSegment
         _lhsOself = rule21 _self
         __result_ = T_CSegment_vOut22 _lhsOself
         in __result_ )
     in C_CSegment_s23 v22
   {-# INLINE rule20 #-}
   rule20 = \ inh_ syn_ ->
     CSegment inh_ syn_
   {-# INLINE rule21 #-}
   rule21 = \ _self ->
     _self

-- CSegments ---------------------------------------------------
-- wrapper
data Inh_CSegments  = Inh_CSegments {  }
data Syn_CSegments  = Syn_CSegments { self_Syn_CSegments :: (CSegments) }
{-# INLINABLE wrap_CSegments #-}
wrap_CSegments :: T_CSegments  -> Inh_CSegments  -> (Syn_CSegments )
wrap_CSegments (T_CSegments act) (Inh_CSegments ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg25 = T_CSegments_vIn25 
        (T_CSegments_vOut25 _lhsOself) <- return (inv_CSegments_s26 sem arg25)
        return (Syn_CSegments _lhsOself)
   )

-- cata
{-# NOINLINE sem_CSegments #-}
sem_CSegments :: CSegments  -> T_CSegments 
sem_CSegments list = Prelude.foldr sem_CSegments_Cons sem_CSegments_Nil (Prelude.map sem_CSegment list)

-- semantic domain
newtype T_CSegments  = T_CSegments {
                                   attach_T_CSegments :: Identity (T_CSegments_s26 )
                                   }
newtype T_CSegments_s26  = C_CSegments_s26 {
                                           inv_CSegments_s26 :: (T_CSegments_v25 )
                                           }
data T_CSegments_s27  = C_CSegments_s27
type T_CSegments_v25  = (T_CSegments_vIn25 ) -> (T_CSegments_vOut25 )
data T_CSegments_vIn25  = T_CSegments_vIn25 
data T_CSegments_vOut25  = T_CSegments_vOut25 (CSegments)
{-# NOINLINE sem_CSegments_Cons #-}
sem_CSegments_Cons :: T_CSegment  -> T_CSegments  -> T_CSegments 
sem_CSegments_Cons arg_hd_ arg_tl_ = T_CSegments (return st26) where
   {-# NOINLINE st26 #-}
   st26 = let
      v25 :: T_CSegments_v25 
      v25 = \ (T_CSegments_vIn25 ) -> ( let
         _hdX23 = Control.Monad.Identity.runIdentity (attach_T_CSegment (arg_hd_))
         _tlX26 = Control.Monad.Identity.runIdentity (attach_T_CSegments (arg_tl_))
         (T_CSegment_vOut22 _hdIself) = inv_CSegment_s23 _hdX23 (T_CSegment_vIn22 )
         (T_CSegments_vOut25 _tlIself) = inv_CSegments_s26 _tlX26 (T_CSegments_vIn25 )
         _self = rule22 _hdIself _tlIself
         _lhsOself :: CSegments
         _lhsOself = rule23 _self
         __result_ = T_CSegments_vOut25 _lhsOself
         in __result_ )
     in C_CSegments_s26 v25
   {-# INLINE rule22 #-}
   rule22 = \ ((_hdIself) :: CSegment) ((_tlIself) :: CSegments) ->
     (:) _hdIself _tlIself
   {-# INLINE rule23 #-}
   rule23 = \ _self ->
     _self
{-# NOINLINE sem_CSegments_Nil #-}
sem_CSegments_Nil ::  T_CSegments 
sem_CSegments_Nil  = T_CSegments (return st26) where
   {-# NOINLINE st26 #-}
   st26 = let
      v25 :: T_CSegments_v25 
      v25 = \ (T_CSegments_vIn25 ) -> ( let
         _self = rule24  ()
         _lhsOself :: CSegments
         _lhsOself = rule25 _self
         __result_ = T_CSegments_vOut25 _lhsOself
         in __result_ )
     in C_CSegments_s26 v25
   {-# INLINE rule24 #-}
   rule24 = \  (_ :: ()) ->
     []
   {-# INLINE rule25 #-}
   rule25 = \ _self ->
     _self

-- CVisit ------------------------------------------------------
-- wrapper
data Inh_CVisit  = Inh_CVisit {  }
data Syn_CVisit  = Syn_CVisit { self_Syn_CVisit :: (CVisit) }
{-# INLINABLE wrap_CVisit #-}
wrap_CVisit :: T_CVisit  -> Inh_CVisit  -> (Syn_CVisit )
wrap_CVisit (T_CVisit act) (Inh_CVisit ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg28 = T_CVisit_vIn28 
        (T_CVisit_vOut28 _lhsOself) <- return (inv_CVisit_s29 sem arg28)
        return (Syn_CVisit _lhsOself)
   )

-- cata
{-# INLINE sem_CVisit #-}
sem_CVisit :: CVisit  -> T_CVisit 
sem_CVisit ( CVisit inh_ syn_ vss_ intra_ ordered_ ) = sem_CVisit_CVisit inh_ syn_ ( sem_Sequence vss_ ) ( sem_Sequence intra_ ) ordered_

-- semantic domain
newtype T_CVisit  = T_CVisit {
                             attach_T_CVisit :: Identity (T_CVisit_s29 )
                             }
newtype T_CVisit_s29  = C_CVisit_s29 {
                                     inv_CVisit_s29 :: (T_CVisit_v28 )
                                     }
data T_CVisit_s30  = C_CVisit_s30
type T_CVisit_v28  = (T_CVisit_vIn28 ) -> (T_CVisit_vOut28 )
data T_CVisit_vIn28  = T_CVisit_vIn28 
data T_CVisit_vOut28  = T_CVisit_vOut28 (CVisit)
{-# NOINLINE sem_CVisit_CVisit #-}
sem_CVisit_CVisit :: (Attributes) -> (Attributes) -> T_Sequence  -> T_Sequence  -> (Bool) -> T_CVisit 
sem_CVisit_CVisit arg_inh_ arg_syn_ arg_vss_ arg_intra_ arg_ordered_ = T_CVisit (return st29) where
   {-# NOINLINE st29 #-}
   st29 = let
      v28 :: T_CVisit_v28 
      v28 = \ (T_CVisit_vIn28 ) -> ( let
         _vssX95 = Control.Monad.Identity.runIdentity (attach_T_Sequence (arg_vss_))
         _intraX95 = Control.Monad.Identity.runIdentity (attach_T_Sequence (arg_intra_))
         (T_Sequence_vOut94 _vssIself) = inv_Sequence_s95 _vssX95 (T_Sequence_vIn94 )
         (T_Sequence_vOut94 _intraIself) = inv_Sequence_s95 _intraX95 (T_Sequence_vIn94 )
         _self = rule26 _intraIself _vssIself arg_inh_ arg_ordered_ arg_syn_
         _lhsOself :: CVisit
         _lhsOself = rule27 _self
         __result_ = T_CVisit_vOut28 _lhsOself
         in __result_ )
     in C_CVisit_s29 v28
   {-# INLINE rule26 #-}
   rule26 = \ ((_intraIself) :: Sequence) ((_vssIself) :: Sequence) inh_ ordered_ syn_ ->
     CVisit inh_ syn_ _vssIself _intraIself ordered_
   {-# INLINE rule27 #-}
   rule27 = \ _self ->
     _self

-- CVisits -----------------------------------------------------
-- wrapper
data Inh_CVisits  = Inh_CVisits {  }
data Syn_CVisits  = Syn_CVisits { self_Syn_CVisits :: (CVisits) }
{-# INLINABLE wrap_CVisits #-}
wrap_CVisits :: T_CVisits  -> Inh_CVisits  -> (Syn_CVisits )
wrap_CVisits (T_CVisits act) (Inh_CVisits ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg31 = T_CVisits_vIn31 
        (T_CVisits_vOut31 _lhsOself) <- return (inv_CVisits_s32 sem arg31)
        return (Syn_CVisits _lhsOself)
   )

-- cata
{-# NOINLINE sem_CVisits #-}
sem_CVisits :: CVisits  -> T_CVisits 
sem_CVisits list = Prelude.foldr sem_CVisits_Cons sem_CVisits_Nil (Prelude.map sem_CVisit list)

-- semantic domain
newtype T_CVisits  = T_CVisits {
                               attach_T_CVisits :: Identity (T_CVisits_s32 )
                               }
newtype T_CVisits_s32  = C_CVisits_s32 {
                                       inv_CVisits_s32 :: (T_CVisits_v31 )
                                       }
data T_CVisits_s33  = C_CVisits_s33
type T_CVisits_v31  = (T_CVisits_vIn31 ) -> (T_CVisits_vOut31 )
data T_CVisits_vIn31  = T_CVisits_vIn31 
data T_CVisits_vOut31  = T_CVisits_vOut31 (CVisits)
{-# NOINLINE sem_CVisits_Cons #-}
sem_CVisits_Cons :: T_CVisit  -> T_CVisits  -> T_CVisits 
sem_CVisits_Cons arg_hd_ arg_tl_ = T_CVisits (return st32) where
   {-# NOINLINE st32 #-}
   st32 = let
      v31 :: T_CVisits_v31 
      v31 = \ (T_CVisits_vIn31 ) -> ( let
         _hdX29 = Control.Monad.Identity.runIdentity (attach_T_CVisit (arg_hd_))
         _tlX32 = Control.Monad.Identity.runIdentity (attach_T_CVisits (arg_tl_))
         (T_CVisit_vOut28 _hdIself) = inv_CVisit_s29 _hdX29 (T_CVisit_vIn28 )
         (T_CVisits_vOut31 _tlIself) = inv_CVisits_s32 _tlX32 (T_CVisits_vIn31 )
         _self = rule28 _hdIself _tlIself
         _lhsOself :: CVisits
         _lhsOself = rule29 _self
         __result_ = T_CVisits_vOut31 _lhsOself
         in __result_ )
     in C_CVisits_s32 v31
   {-# INLINE rule28 #-}
   rule28 = \ ((_hdIself) :: CVisit) ((_tlIself) :: CVisits) ->
     (:) _hdIself _tlIself
   {-# INLINE rule29 #-}
   rule29 = \ _self ->
     _self
{-# NOINLINE sem_CVisits_Nil #-}
sem_CVisits_Nil ::  T_CVisits 
sem_CVisits_Nil  = T_CVisits (return st32) where
   {-# NOINLINE st32 #-}
   st32 = let
      v31 :: T_CVisits_v31 
      v31 = \ (T_CVisits_vIn31 ) -> ( let
         _self = rule30  ()
         _lhsOself :: CVisits
         _lhsOself = rule31 _self
         __result_ = T_CVisits_vOut31 _lhsOself
         in __result_ )
     in C_CVisits_s32 v31
   {-# INLINE rule30 #-}
   rule30 = \  (_ :: ()) ->
     []
   {-# INLINE rule31 #-}
   rule31 = \ _self ->
     _self

-- Child -------------------------------------------------------
-- wrapper
data Inh_Child  = Inh_Child { ain_Inh_Child :: (MyType -> MyAttributes), an_Inh_Child :: (MyType -> MyAttributes), aroundMap_Inh_Child :: (Map Identifier [Expression]), asn_Inh_Child :: (MyType -> MyAttributes), flab_Inh_Child :: (Int), fty_Inh_Child :: (FTY), hoMapf_Inh_Child :: (HOMap), lfpf_Inh_Child :: (SF_P), mergeMap_Inh_Child :: (Map Identifier (Identifier, [Identifier], Expression)), mergedChildren_Inh_Child :: (Set Identifier), nmp_Inh_Child :: (NMP), nmprf_Inh_Child :: (NMP_R), olab_Inh_Child :: (Int), options_Inh_Child :: (Options), pll_Inh_Child :: (PLabel), pmpf_Inh_Child :: (PMP), pmprf_Inh_Child :: (PMP_R) }
data Syn_Child  = Syn_Child { ap_Syn_Child :: (A_P), echilds_Syn_Child :: (EChild), fieldMap_Syn_Child :: (FMap), flab_Syn_Child :: (Int), fty_Syn_Child :: (FTY), gen_Syn_Child :: (Map Int Int), hoMap_Syn_Child :: (HOMap), inss_Syn_Child :: (Map Int [Int]), ofld_Syn_Child :: ([(Int, Int)]), olab_Syn_Child :: (Int), pmp_Syn_Child :: (PMP), pmpr_Syn_Child :: (PMP_R), pts_Syn_Child :: (Set.Set FLabel), refHoNts_Syn_Child :: (Set NontermIdent), refNts_Syn_Child :: (Set NontermIdent), self_Syn_Child :: (Child) }
{-# INLINABLE wrap_Child #-}
wrap_Child :: T_Child  -> Inh_Child  -> (Syn_Child )
wrap_Child (T_Child act) (Inh_Child _lhsIain _lhsIan _lhsIaroundMap _lhsIasn _lhsIflab _lhsIfty _lhsIhoMapf _lhsIlfpf _lhsImergeMap _lhsImergedChildren _lhsInmp _lhsInmprf _lhsIolab _lhsIoptions _lhsIpll _lhsIpmpf _lhsIpmprf) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg34 = T_Child_vIn34 _lhsIain _lhsIan _lhsIaroundMap _lhsIasn _lhsIflab _lhsIfty _lhsIhoMapf _lhsIlfpf _lhsImergeMap _lhsImergedChildren _lhsInmp _lhsInmprf _lhsIolab _lhsIoptions _lhsIpll _lhsIpmpf _lhsIpmprf
        (T_Child_vOut34 _lhsOap _lhsOechilds _lhsOfieldMap _lhsOflab _lhsOfty _lhsOgen _lhsOhoMap _lhsOinss _lhsOofld _lhsOolab _lhsOpmp _lhsOpmpr _lhsOpts _lhsOrefHoNts _lhsOrefNts _lhsOself) <- return (inv_Child_s35 sem arg34)
        return (Syn_Child _lhsOap _lhsOechilds _lhsOfieldMap _lhsOflab _lhsOfty _lhsOgen _lhsOhoMap _lhsOinss _lhsOofld _lhsOolab _lhsOpmp _lhsOpmpr _lhsOpts _lhsOrefHoNts _lhsOrefNts _lhsOself)
   )

-- cata
{-# INLINE sem_Child #-}
sem_Child :: Child  -> T_Child 
sem_Child ( Child name_ tp_ kind_  ) = sem_Child_Child name_ tp_ kind_ 

-- semantic domain
newtype T_Child  = T_Child {
                           attach_T_Child :: Identity (T_Child_s35 )
                           }
newtype T_Child_s35  = C_Child_s35 {
                                   inv_Child_s35 :: (T_Child_v34 )
                                   }
data T_Child_s36  = C_Child_s36
type T_Child_v34  = (T_Child_vIn34 ) -> (T_Child_vOut34 )
data T_Child_vIn34  = T_Child_vIn34 (MyType -> MyAttributes) (MyType -> MyAttributes) (Map Identifier [Expression]) (MyType -> MyAttributes) (Int) (FTY) (HOMap) (SF_P) (Map Identifier (Identifier, [Identifier], Expression)) (Set Identifier) (NMP) (NMP_R) (Int) (Options) (PLabel) (PMP) (PMP_R)
data T_Child_vOut34  = T_Child_vOut34 (A_P) (EChild) (FMap) (Int) (FTY) (Map Int Int) (HOMap) (Map Int [Int]) ([(Int, Int)]) (Int) (PMP) (PMP_R) (Set.Set FLabel) (Set NontermIdent) (Set NontermIdent) (Child)
{-# NOINLINE sem_Child_Child #-}
sem_Child_Child :: (Identifier) -> (Type) -> (ChildKind) ->  T_Child 
sem_Child_Child arg_name_ arg_tp_ arg_kind_  = T_Child (return st35) where
   {-# NOINLINE st35 #-}
   st35 = let
      v34 :: T_Child_v34 
      v34 = \ (T_Child_vIn34 _lhsIain _lhsIan _lhsIaroundMap _lhsIasn _lhsIflab _lhsIfty _lhsIhoMapf _lhsIlfpf _lhsImergeMap _lhsImergedChildren _lhsInmp _lhsInmprf _lhsIolab _lhsIoptions _lhsIpll _lhsIpmpf _lhsIpmprf) -> ( let
         _fattsX47 = Control.Monad.Identity.runIdentity (attach_T_FieldAtts ((sem_FieldAtts fatts_val_)))
         (T_FieldAtts_vOut46 _fattsIap _fattsIflab _fattsIfty _fattsIgen _fattsIinss _fattsIofld _fattsIolab _fattsIpmp _fattsIpmpr _fattsIself) = inv_FieldAtts_s47 _fattsX47 (T_FieldAtts_vIn46 _fattsOan _fattsOflab _fattsOnmprf _fattsOolab)
         _refNts = rule32 arg_tp_
         _refHoNts = rule33 _isHigherOrder _refNts
         _isHigherOrder = rule34 arg_kind_
         _hasArounds = rule35 _lhsIaroundMap arg_name_
         _merges = rule36 _lhsImergeMap arg_name_
         _isMerged = rule37 _lhsImergedChildren arg_name_
         _lhsOechilds :: EChild
         _lhsOechilds = rule38 _hasArounds _isMerged _merges arg_kind_ arg_name_ arg_tp_
         _flab = rule39 _lhsIflab
         _atp = rule40 arg_tp_
         fatts_val_ = rule41 _atp _lhsIan _lhsIpll arg_name_
         _fattsOflab = rule42 _flab
         _ident = rule43 arg_name_
         _label = rule44 _ident _lhsIpll
         _foccsI = rule45 _atp _label _lhsIain
         _foccsS = rule46 _atp _label _lhsIasn
         _fieldMap = rule47 _foccsI _foccsS _label
         _hoMap = rule48 _ident _lhsIpll arg_kind_
         _lhsOfty :: FTY
         _lhsOfty = rule49 _atp _lhsIpll arg_name_
         _lhsOpts :: Set.Set FLabel
         _lhsOpts = rule50 arg_name_
         _lhsOap :: A_P
         _lhsOap = rule51 _fattsIap
         _lhsOfieldMap :: FMap
         _lhsOfieldMap = rule52 _fieldMap
         _lhsOgen :: Map Int Int
         _lhsOgen = rule53 _fattsIgen
         _lhsOhoMap :: HOMap
         _lhsOhoMap = rule54 _hoMap
         _lhsOinss :: Map Int [Int]
         _lhsOinss = rule55 _fattsIinss
         _lhsOofld :: [(Int, Int)]
         _lhsOofld = rule56 _fattsIofld
         _lhsOpmp :: PMP
         _lhsOpmp = rule57 _fattsIpmp
         _lhsOpmpr :: PMP_R
         _lhsOpmpr = rule58 _fattsIpmpr
         _lhsOrefHoNts :: Set NontermIdent
         _lhsOrefHoNts = rule59 _refHoNts
         _lhsOrefNts :: Set NontermIdent
         _lhsOrefNts = rule60 _refNts
         _self = rule61 arg_kind_ arg_name_ arg_tp_
         _lhsOself :: Child
         _lhsOself = rule62 _self
         _lhsOflab :: Int
         _lhsOflab = rule63 _flab
         _lhsOolab :: Int
         _lhsOolab = rule64 _fattsIolab
         _fattsOan = rule65 _lhsIan
         _fattsOnmprf = rule66 _lhsInmprf
         _fattsOolab = rule67 _lhsIolab
         __result_ = T_Child_vOut34 _lhsOap _lhsOechilds _lhsOfieldMap _lhsOflab _lhsOfty _lhsOgen _lhsOhoMap _lhsOinss _lhsOofld _lhsOolab _lhsOpmp _lhsOpmpr _lhsOpts _lhsOrefHoNts _lhsOrefNts _lhsOself
         in __result_ )
     in C_Child_s35 v34
   {-# INLINE rule32 #-}
   {-# LINE 31 "src-ag/ExecutionPlanCommon.ag" #-}
   rule32 = \ tp_ ->
                 {-# LINE 31 "src-ag/ExecutionPlanCommon.ag" #-}
                 case tp_ of
                   NT nt _ _ -> Set.singleton nt
                   _         -> mempty
                 {-# LINE 1097 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule33 #-}
   {-# LINE 34 "src-ag/ExecutionPlanCommon.ag" #-}
   rule33 = \ _isHigherOrder _refNts ->
                   {-# LINE 34 "src-ag/ExecutionPlanCommon.ag" #-}
                   if _isHigherOrder     then _refNts     else mempty
                   {-# LINE 1103 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule34 #-}
   {-# LINE 35 "src-ag/ExecutionPlanCommon.ag" #-}
   rule34 = \ kind_ ->
                        {-# LINE 35 "src-ag/ExecutionPlanCommon.ag" #-}
                        case kind_ of
                          ChildSyntax -> False
                          _           -> True
                        {-# LINE 1111 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule35 #-}
   {-# LINE 95 "src-ag/ExecutionPlanCommon.ag" #-}
   rule35 = \ ((_lhsIaroundMap) :: Map Identifier [Expression]) name_ ->
                     {-# LINE 95 "src-ag/ExecutionPlanCommon.ag" #-}
                     case Map.lookup name_ _lhsIaroundMap of
                       Nothing -> False
                       Just as -> not (null as)
                     {-# LINE 1119 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule36 #-}
   {-# LINE 123 "src-ag/ExecutionPlanCommon.ag" #-}
   rule36 = \ ((_lhsImergeMap) :: Map Identifier (Identifier, [Identifier], Expression)) name_ ->
                   {-# LINE 123 "src-ag/ExecutionPlanCommon.ag" #-}
                   maybe Nothing (\(_,ms,_) -> Just ms) $ Map.lookup name_ _lhsImergeMap
                   {-# LINE 1125 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule37 #-}
   {-# LINE 124 "src-ag/ExecutionPlanCommon.ag" #-}
   rule37 = \ ((_lhsImergedChildren) :: Set Identifier) name_ ->
                   {-# LINE 124 "src-ag/ExecutionPlanCommon.ag" #-}
                   name_ `Set.member` _lhsImergedChildren
                   {-# LINE 1131 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule38 #-}
   {-# LINE 135 "src-ag/ExecutionPlanCommon.ag" #-}
   rule38 = \ _hasArounds _isMerged _merges kind_ name_ tp_ ->
                          {-# LINE 135 "src-ag/ExecutionPlanCommon.ag" #-}
                          case tp_ of
                            NT _ _ _ -> EChild name_ tp_ kind_ _hasArounds     _merges     _isMerged
                            _        -> ETerm name_ tp_
                          {-# LINE 1139 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule39 #-}
   {-# LINE 174 "src-ag/LOAG/Prepare.ag" #-}
   rule39 = \ ((_lhsIflab) :: Int) ->
                    {-# LINE 174 "src-ag/LOAG/Prepare.ag" #-}
                    _lhsIflab + 1
                    {-# LINE 1145 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule40 #-}
   {-# LINE 175 "src-ag/LOAG/Prepare.ag" #-}
   rule40 = \ tp_ ->
                    {-# LINE 175 "src-ag/LOAG/Prepare.ag" #-}
                    toMyTy tp_
                    {-# LINE 1151 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule41 #-}
   {-# LINE 177 "src-ag/LOAG/Prepare.ag" #-}
   rule41 = \ _atp ((_lhsIan) :: MyType -> MyAttributes) ((_lhsIpll) :: PLabel) name_ ->
                    {-# LINE 177 "src-ag/LOAG/Prepare.ag" #-}
                    map ((FieldAtt _atp     _lhsIpll (getName name_)) . alab)
                          $ _lhsIan _atp
                    {-# LINE 1158 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule42 #-}
   {-# LINE 179 "src-ag/LOAG/Prepare.ag" #-}
   rule42 = \ _flab ->
                    {-# LINE 179 "src-ag/LOAG/Prepare.ag" #-}
                    _flab
                    {-# LINE 1164 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule43 #-}
   {-# LINE 180 "src-ag/LOAG/Prepare.ag" #-}
   rule43 = \ name_ ->
                    {-# LINE 180 "src-ag/LOAG/Prepare.ag" #-}
                    getName name_
                    {-# LINE 1170 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule44 #-}
   {-# LINE 181 "src-ag/LOAG/Prepare.ag" #-}
   rule44 = \ _ident ((_lhsIpll) :: PLabel) ->
                    {-# LINE 181 "src-ag/LOAG/Prepare.ag" #-}
                    (_lhsIpll, _ident    )
                    {-# LINE 1176 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule45 #-}
   {-# LINE 182 "src-ag/LOAG/Prepare.ag" #-}
   rule45 = \ _atp _label ((_lhsIain) :: MyType -> MyAttributes) ->
                    {-# LINE 182 "src-ag/LOAG/Prepare.ag" #-}
                    Set.fromList $ handAllOut _label     $ _lhsIain _atp
                    {-# LINE 1182 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule46 #-}
   {-# LINE 183 "src-ag/LOAG/Prepare.ag" #-}
   rule46 = \ _atp _label ((_lhsIasn) :: MyType -> MyAttributes) ->
                    {-# LINE 183 "src-ag/LOAG/Prepare.ag" #-}
                    Set.fromList $ handAllOut _label     $ _lhsIasn _atp
                    {-# LINE 1188 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule47 #-}
   {-# LINE 184 "src-ag/LOAG/Prepare.ag" #-}
   rule47 = \ _foccsI _foccsS _label ->
                    {-# LINE 184 "src-ag/LOAG/Prepare.ag" #-}
                    if Set.null _foccsI     && Set.null _foccsS
                          then Map.empty
                          else Map.singleton _label     (_foccsS    ,_foccsI    )
                    {-# LINE 1196 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule48 #-}
   {-# LINE 187 "src-ag/LOAG/Prepare.ag" #-}
   rule48 = \ _ident ((_lhsIpll) :: PLabel) kind_ ->
                    {-# LINE 187 "src-ag/LOAG/Prepare.ag" #-}
                    case kind_ of
                      ChildAttr -> Map.singleton _lhsIpll (Set.singleton _ident    )
                      _         -> Map.empty
                    {-# LINE 1204 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule49 #-}
   {-# LINE 190 "src-ag/LOAG/Prepare.ag" #-}
   rule49 = \ _atp ((_lhsIpll) :: PLabel) name_ ->
                    {-# LINE 190 "src-ag/LOAG/Prepare.ag" #-}
                    Map.singleton (_lhsIpll, getName name_) _atp
                    {-# LINE 1210 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule50 #-}
   {-# LINE 223 "src-ag/LOAG/Prepare.ag" #-}
   rule50 = \ name_ ->
                {-# LINE 223 "src-ag/LOAG/Prepare.ag" #-}
                Set.singleton $ getName name_
                {-# LINE 1216 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule51 #-}
   rule51 = \ ((_fattsIap) :: A_P) ->
     _fattsIap
   {-# INLINE rule52 #-}
   rule52 = \ _fieldMap ->
     _fieldMap
   {-# INLINE rule53 #-}
   rule53 = \ ((_fattsIgen) :: Map Int Int) ->
     _fattsIgen
   {-# INLINE rule54 #-}
   rule54 = \ _hoMap ->
     _hoMap
   {-# INLINE rule55 #-}
   rule55 = \ ((_fattsIinss) :: Map Int [Int]) ->
     _fattsIinss
   {-# INLINE rule56 #-}
   rule56 = \ ((_fattsIofld) :: [(Int, Int)]) ->
     _fattsIofld
   {-# INLINE rule57 #-}
   rule57 = \ ((_fattsIpmp) :: PMP) ->
     _fattsIpmp
   {-# INLINE rule58 #-}
   rule58 = \ ((_fattsIpmpr) :: PMP_R) ->
     _fattsIpmpr
   {-# INLINE rule59 #-}
   rule59 = \ _refHoNts ->
     _refHoNts
   {-# INLINE rule60 #-}
   rule60 = \ _refNts ->
     _refNts
   {-# INLINE rule61 #-}
   rule61 = \ kind_ name_ tp_ ->
     Child name_ tp_ kind_
   {-# INLINE rule62 #-}
   rule62 = \ _self ->
     _self
   {-# INLINE rule63 #-}
   rule63 = \ _flab ->
     _flab
   {-# INLINE rule64 #-}
   rule64 = \ ((_fattsIolab) :: Int) ->
     _fattsIolab
   {-# INLINE rule65 #-}
   rule65 = \ ((_lhsIan) :: MyType -> MyAttributes) ->
     _lhsIan
   {-# INLINE rule66 #-}
   rule66 = \ ((_lhsInmprf) :: NMP_R) ->
     _lhsInmprf
   {-# INLINE rule67 #-}
   rule67 = \ ((_lhsIolab) :: Int) ->
     _lhsIolab

-- Children ----------------------------------------------------
-- wrapper
data Inh_Children  = Inh_Children { ain_Inh_Children :: (MyType -> MyAttributes), an_Inh_Children :: (MyType -> MyAttributes), aroundMap_Inh_Children :: (Map Identifier [Expression]), asn_Inh_Children :: (MyType -> MyAttributes), dty_Inh_Children :: (MyType), flab_Inh_Children :: (Int), fty_Inh_Children :: (FTY), hoMapf_Inh_Children :: (HOMap), lfpf_Inh_Children :: (SF_P), mergeMap_Inh_Children :: (Map Identifier (Identifier, [Identifier], Expression)), mergedChildren_Inh_Children :: (Set Identifier), nmp_Inh_Children :: (NMP), nmprf_Inh_Children :: (NMP_R), olab_Inh_Children :: (Int), options_Inh_Children :: (Options), pll_Inh_Children :: (PLabel), pmpf_Inh_Children :: (PMP), pmprf_Inh_Children :: (PMP_R) }
data Syn_Children  = Syn_Children { ap_Syn_Children :: (A_P), echilds_Syn_Children :: (EChildren), fieldMap_Syn_Children :: (FMap), flab_Syn_Children :: (Int), fty_Syn_Children :: (FTY), gen_Syn_Children :: (Map Int Int), hoMap_Syn_Children :: (HOMap), inss_Syn_Children :: (Map Int [Int]), ofld_Syn_Children :: ([(Int, Int)]), olab_Syn_Children :: (Int), pmp_Syn_Children :: (PMP), pmpr_Syn_Children :: (PMP_R), pts_Syn_Children :: (Set.Set FLabel), refHoNts_Syn_Children :: (Set NontermIdent), refNts_Syn_Children :: (Set NontermIdent), self_Syn_Children :: (Children) }
{-# INLINABLE wrap_Children #-}
wrap_Children :: T_Children  -> Inh_Children  -> (Syn_Children )
wrap_Children (T_Children act) (Inh_Children _lhsIain _lhsIan _lhsIaroundMap _lhsIasn _lhsIdty _lhsIflab _lhsIfty _lhsIhoMapf _lhsIlfpf _lhsImergeMap _lhsImergedChildren _lhsInmp _lhsInmprf _lhsIolab _lhsIoptions _lhsIpll _lhsIpmpf _lhsIpmprf) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg37 = T_Children_vIn37 _lhsIain _lhsIan _lhsIaroundMap _lhsIasn _lhsIdty _lhsIflab _lhsIfty _lhsIhoMapf _lhsIlfpf _lhsImergeMap _lhsImergedChildren _lhsInmp _lhsInmprf _lhsIolab _lhsIoptions _lhsIpll _lhsIpmpf _lhsIpmprf
        (T_Children_vOut37 _lhsOap _lhsOechilds _lhsOfieldMap _lhsOflab _lhsOfty _lhsOgen _lhsOhoMap _lhsOinss _lhsOofld _lhsOolab _lhsOpmp _lhsOpmpr _lhsOpts _lhsOrefHoNts _lhsOrefNts _lhsOself) <- return (inv_Children_s38 sem arg37)
        return (Syn_Children _lhsOap _lhsOechilds _lhsOfieldMap _lhsOflab _lhsOfty _lhsOgen _lhsOhoMap _lhsOinss _lhsOofld _lhsOolab _lhsOpmp _lhsOpmpr _lhsOpts _lhsOrefHoNts _lhsOrefNts _lhsOself)
   )

-- cata
{-# NOINLINE sem_Children #-}
sem_Children :: Children  -> T_Children 
sem_Children list = Prelude.foldr sem_Children_Cons sem_Children_Nil (Prelude.map sem_Child list)

-- semantic domain
newtype T_Children  = T_Children {
                                 attach_T_Children :: Identity (T_Children_s38 )
                                 }
newtype T_Children_s38  = C_Children_s38 {
                                         inv_Children_s38 :: (T_Children_v37 )
                                         }
data T_Children_s39  = C_Children_s39
type T_Children_v37  = (T_Children_vIn37 ) -> (T_Children_vOut37 )
data T_Children_vIn37  = T_Children_vIn37 (MyType -> MyAttributes) (MyType -> MyAttributes) (Map Identifier [Expression]) (MyType -> MyAttributes) (MyType) (Int) (FTY) (HOMap) (SF_P) (Map Identifier (Identifier, [Identifier], Expression)) (Set Identifier) (NMP) (NMP_R) (Int) (Options) (PLabel) (PMP) (PMP_R)
data T_Children_vOut37  = T_Children_vOut37 (A_P) (EChildren) (FMap) (Int) (FTY) (Map Int Int) (HOMap) (Map Int [Int]) ([(Int, Int)]) (Int) (PMP) (PMP_R) (Set.Set FLabel) (Set NontermIdent) (Set NontermIdent) (Children)
{-# NOINLINE sem_Children_Cons #-}
sem_Children_Cons :: T_Child  -> T_Children  -> T_Children 
sem_Children_Cons arg_hd_ arg_tl_ = T_Children (return st38) where
   {-# NOINLINE st38 #-}
   st38 = let
      v37 :: T_Children_v37 
      v37 = \ (T_Children_vIn37 _lhsIain _lhsIan _lhsIaroundMap _lhsIasn _lhsIdty _lhsIflab _lhsIfty _lhsIhoMapf _lhsIlfpf _lhsImergeMap _lhsImergedChildren _lhsInmp _lhsInmprf _lhsIolab _lhsIoptions _lhsIpll _lhsIpmpf _lhsIpmprf) -> ( let
         _hdX35 = Control.Monad.Identity.runIdentity (attach_T_Child (arg_hd_))
         _tlX38 = Control.Monad.Identity.runIdentity (attach_T_Children (arg_tl_))
         (T_Child_vOut34 _hdIap _hdIechilds _hdIfieldMap _hdIflab _hdIfty _hdIgen _hdIhoMap _hdIinss _hdIofld _hdIolab _hdIpmp _hdIpmpr _hdIpts _hdIrefHoNts _hdIrefNts _hdIself) = inv_Child_s35 _hdX35 (T_Child_vIn34 _hdOain _hdOan _hdOaroundMap _hdOasn _hdOflab _hdOfty _hdOhoMapf _hdOlfpf _hdOmergeMap _hdOmergedChildren _hdOnmp _hdOnmprf _hdOolab _hdOoptions _hdOpll _hdOpmpf _hdOpmprf)
         (T_Children_vOut37 _tlIap _tlIechilds _tlIfieldMap _tlIflab _tlIfty _tlIgen _tlIhoMap _tlIinss _tlIofld _tlIolab _tlIpmp _tlIpmpr _tlIpts _tlIrefHoNts _tlIrefNts _tlIself) = inv_Children_s38 _tlX38 (T_Children_vIn37 _tlOain _tlOan _tlOaroundMap _tlOasn _tlOdty _tlOflab _tlOfty _tlOhoMapf _tlOlfpf _tlOmergeMap _tlOmergedChildren _tlOnmp _tlOnmprf _tlOolab _tlOoptions _tlOpll _tlOpmpf _tlOpmprf)
         _lhsOap :: A_P
         _lhsOap = rule68 _hdIap _tlIap
         _lhsOechilds :: EChildren
         _lhsOechilds = rule69 _hdIechilds _tlIechilds
         _lhsOfieldMap :: FMap
         _lhsOfieldMap = rule70 _hdIfieldMap _tlIfieldMap
         _lhsOfty :: FTY
         _lhsOfty = rule71 _hdIfty _tlIfty
         _lhsOgen :: Map Int Int
         _lhsOgen = rule72 _hdIgen _tlIgen
         _lhsOhoMap :: HOMap
         _lhsOhoMap = rule73 _hdIhoMap _tlIhoMap
         _lhsOinss :: Map Int [Int]
         _lhsOinss = rule74 _hdIinss _tlIinss
         _lhsOofld :: [(Int, Int)]
         _lhsOofld = rule75 _hdIofld _tlIofld
         _lhsOpmp :: PMP
         _lhsOpmp = rule76 _hdIpmp _tlIpmp
         _lhsOpmpr :: PMP_R
         _lhsOpmpr = rule77 _hdIpmpr _tlIpmpr
         _lhsOpts :: Set.Set FLabel
         _lhsOpts = rule78 _hdIpts _tlIpts
         _lhsOrefHoNts :: Set NontermIdent
         _lhsOrefHoNts = rule79 _hdIrefHoNts _tlIrefHoNts
         _lhsOrefNts :: Set NontermIdent
         _lhsOrefNts = rule80 _hdIrefNts _tlIrefNts
         _self = rule81 _hdIself _tlIself
         _lhsOself :: Children
         _lhsOself = rule82 _self
         _lhsOflab :: Int
         _lhsOflab = rule83 _tlIflab
         _lhsOolab :: Int
         _lhsOolab = rule84 _tlIolab
         _hdOain = rule85 _lhsIain
         _hdOan = rule86 _lhsIan
         _hdOaroundMap = rule87 _lhsIaroundMap
         _hdOasn = rule88 _lhsIasn
         _hdOflab = rule89 _lhsIflab
         _hdOfty = rule90 _lhsIfty
         _hdOhoMapf = rule91 _lhsIhoMapf
         _hdOlfpf = rule92 _lhsIlfpf
         _hdOmergeMap = rule93 _lhsImergeMap
         _hdOmergedChildren = rule94 _lhsImergedChildren
         _hdOnmp = rule95 _lhsInmp
         _hdOnmprf = rule96 _lhsInmprf
         _hdOolab = rule97 _lhsIolab
         _hdOoptions = rule98 _lhsIoptions
         _hdOpll = rule99 _lhsIpll
         _hdOpmpf = rule100 _lhsIpmpf
         _hdOpmprf = rule101 _lhsIpmprf
         _tlOain = rule102 _lhsIain
         _tlOan = rule103 _lhsIan
         _tlOaroundMap = rule104 _lhsIaroundMap
         _tlOasn = rule105 _lhsIasn
         _tlOdty = rule106 _lhsIdty
         _tlOflab = rule107 _hdIflab
         _tlOfty = rule108 _hdIfty
         _tlOhoMapf = rule109 _lhsIhoMapf
         _tlOlfpf = rule110 _lhsIlfpf
         _tlOmergeMap = rule111 _lhsImergeMap
         _tlOmergedChildren = rule112 _lhsImergedChildren
         _tlOnmp = rule113 _lhsInmp
         _tlOnmprf = rule114 _lhsInmprf
         _tlOolab = rule115 _hdIolab
         _tlOoptions = rule116 _lhsIoptions
         _tlOpll = rule117 _lhsIpll
         _tlOpmpf = rule118 _lhsIpmpf
         _tlOpmprf = rule119 _lhsIpmprf
         __result_ = T_Children_vOut37 _lhsOap _lhsOechilds _lhsOfieldMap _lhsOflab _lhsOfty _lhsOgen _lhsOhoMap _lhsOinss _lhsOofld _lhsOolab _lhsOpmp _lhsOpmpr _lhsOpts _lhsOrefHoNts _lhsOrefNts _lhsOself
         in __result_ )
     in C_Children_s38 v37
   {-# INLINE rule68 #-}
   rule68 = \ ((_hdIap) :: A_P) ((_tlIap) :: A_P) ->
     (Map.unionWith (++) _hdIap _tlIap)
   {-# INLINE rule69 #-}
   rule69 = \ ((_hdIechilds) :: EChild) ((_tlIechilds) :: EChildren) ->
     _hdIechilds : _tlIechilds
   {-# INLINE rule70 #-}
   rule70 = \ ((_hdIfieldMap) :: FMap) ((_tlIfieldMap) :: FMap) ->
     (Map.union _hdIfieldMap _tlIfieldMap)
   {-# INLINE rule71 #-}
   rule71 = \ ((_hdIfty) :: FTY) ((_tlIfty) :: FTY) ->
     (Map.union _hdIfty _tlIfty)
   {-# INLINE rule72 #-}
   rule72 = \ ((_hdIgen) :: Map Int Int) ((_tlIgen) :: Map Int Int) ->
     (Map.union _hdIgen _tlIgen)
   {-# INLINE rule73 #-}
   rule73 = \ ((_hdIhoMap) :: HOMap) ((_tlIhoMap) :: HOMap) ->
     (Map.unionWith (Set.union) _hdIhoMap _tlIhoMap)
   {-# INLINE rule74 #-}
   rule74 = \ ((_hdIinss) :: Map Int [Int]) ((_tlIinss) :: Map Int [Int]) ->
     (Map.unionWith (++) _hdIinss _tlIinss)
   {-# INLINE rule75 #-}
   rule75 = \ ((_hdIofld) :: [(Int, Int)]) ((_tlIofld) :: [(Int, Int)]) ->
     ((++) _hdIofld _tlIofld)
   {-# INLINE rule76 #-}
   rule76 = \ ((_hdIpmp) :: PMP) ((_tlIpmp) :: PMP) ->
     (Map.union _hdIpmp _tlIpmp)
   {-# INLINE rule77 #-}
   rule77 = \ ((_hdIpmpr) :: PMP_R) ((_tlIpmpr) :: PMP_R) ->
     (Map.union _hdIpmpr _tlIpmpr)
   {-# INLINE rule78 #-}
   rule78 = \ ((_hdIpts) :: Set.Set FLabel) ((_tlIpts) :: Set.Set FLabel) ->
     (Set.union _hdIpts _tlIpts)
   {-# INLINE rule79 #-}
   rule79 = \ ((_hdIrefHoNts) :: Set NontermIdent) ((_tlIrefHoNts) :: Set NontermIdent) ->
     _hdIrefHoNts `mappend` _tlIrefHoNts
   {-# INLINE rule80 #-}
   rule80 = \ ((_hdIrefNts) :: Set NontermIdent) ((_tlIrefNts) :: Set NontermIdent) ->
     _hdIrefNts `mappend` _tlIrefNts
   {-# INLINE rule81 #-}
   rule81 = \ ((_hdIself) :: Child) ((_tlIself) :: Children) ->
     (:) _hdIself _tlIself
   {-# INLINE rule82 #-}
   rule82 = \ _self ->
     _self
   {-# INLINE rule83 #-}
   rule83 = \ ((_tlIflab) :: Int) ->
     _tlIflab
   {-# INLINE rule84 #-}
   rule84 = \ ((_tlIolab) :: Int) ->
     _tlIolab
   {-# INLINE rule85 #-}
   rule85 = \ ((_lhsIain) :: MyType -> MyAttributes) ->
     _lhsIain
   {-# INLINE rule86 #-}
   rule86 = \ ((_lhsIan) :: MyType -> MyAttributes) ->
     _lhsIan
   {-# INLINE rule87 #-}
   rule87 = \ ((_lhsIaroundMap) :: Map Identifier [Expression]) ->
     _lhsIaroundMap
   {-# INLINE rule88 #-}
   rule88 = \ ((_lhsIasn) :: MyType -> MyAttributes) ->
     _lhsIasn
   {-# INLINE rule89 #-}
   rule89 = \ ((_lhsIflab) :: Int) ->
     _lhsIflab
   {-# INLINE rule90 #-}
   rule90 = \ ((_lhsIfty) :: FTY) ->
     _lhsIfty
   {-# INLINE rule91 #-}
   rule91 = \ ((_lhsIhoMapf) :: HOMap) ->
     _lhsIhoMapf
   {-# INLINE rule92 #-}
   rule92 = \ ((_lhsIlfpf) :: SF_P) ->
     _lhsIlfpf
   {-# INLINE rule93 #-}
   rule93 = \ ((_lhsImergeMap) :: Map Identifier (Identifier, [Identifier], Expression)) ->
     _lhsImergeMap
   {-# INLINE rule94 #-}
   rule94 = \ ((_lhsImergedChildren) :: Set Identifier) ->
     _lhsImergedChildren
   {-# INLINE rule95 #-}
   rule95 = \ ((_lhsInmp) :: NMP) ->
     _lhsInmp
   {-# INLINE rule96 #-}
   rule96 = \ ((_lhsInmprf) :: NMP_R) ->
     _lhsInmprf
   {-# INLINE rule97 #-}
   rule97 = \ ((_lhsIolab) :: Int) ->
     _lhsIolab
   {-# INLINE rule98 #-}
   rule98 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule99 #-}
   rule99 = \ ((_lhsIpll) :: PLabel) ->
     _lhsIpll
   {-# INLINE rule100 #-}
   rule100 = \ ((_lhsIpmpf) :: PMP) ->
     _lhsIpmpf
   {-# INLINE rule101 #-}
   rule101 = \ ((_lhsIpmprf) :: PMP_R) ->
     _lhsIpmprf
   {-# INLINE rule102 #-}
   rule102 = \ ((_lhsIain) :: MyType -> MyAttributes) ->
     _lhsIain
   {-# INLINE rule103 #-}
   rule103 = \ ((_lhsIan) :: MyType -> MyAttributes) ->
     _lhsIan
   {-# INLINE rule104 #-}
   rule104 = \ ((_lhsIaroundMap) :: Map Identifier [Expression]) ->
     _lhsIaroundMap
   {-# INLINE rule105 #-}
   rule105 = \ ((_lhsIasn) :: MyType -> MyAttributes) ->
     _lhsIasn
   {-# INLINE rule106 #-}
   rule106 = \ ((_lhsIdty) :: MyType) ->
     _lhsIdty
   {-# INLINE rule107 #-}
   rule107 = \ ((_hdIflab) :: Int) ->
     _hdIflab
   {-# INLINE rule108 #-}
   rule108 = \ ((_hdIfty) :: FTY) ->
     _hdIfty
   {-# INLINE rule109 #-}
   rule109 = \ ((_lhsIhoMapf) :: HOMap) ->
     _lhsIhoMapf
   {-# INLINE rule110 #-}
   rule110 = \ ((_lhsIlfpf) :: SF_P) ->
     _lhsIlfpf
   {-# INLINE rule111 #-}
   rule111 = \ ((_lhsImergeMap) :: Map Identifier (Identifier, [Identifier], Expression)) ->
     _lhsImergeMap
   {-# INLINE rule112 #-}
   rule112 = \ ((_lhsImergedChildren) :: Set Identifier) ->
     _lhsImergedChildren
   {-# INLINE rule113 #-}
   rule113 = \ ((_lhsInmp) :: NMP) ->
     _lhsInmp
   {-# INLINE rule114 #-}
   rule114 = \ ((_lhsInmprf) :: NMP_R) ->
     _lhsInmprf
   {-# INLINE rule115 #-}
   rule115 = \ ((_hdIolab) :: Int) ->
     _hdIolab
   {-# INLINE rule116 #-}
   rule116 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule117 #-}
   rule117 = \ ((_lhsIpll) :: PLabel) ->
     _lhsIpll
   {-# INLINE rule118 #-}
   rule118 = \ ((_lhsIpmpf) :: PMP) ->
     _lhsIpmpf
   {-# INLINE rule119 #-}
   rule119 = \ ((_lhsIpmprf) :: PMP_R) ->
     _lhsIpmprf
{-# NOINLINE sem_Children_Nil #-}
sem_Children_Nil ::  T_Children 
sem_Children_Nil  = T_Children (return st38) where
   {-# NOINLINE st38 #-}
   st38 = let
      v37 :: T_Children_v37 
      v37 = \ (T_Children_vIn37 _lhsIain _lhsIan _lhsIaroundMap _lhsIasn _lhsIdty _lhsIflab _lhsIfty _lhsIhoMapf _lhsIlfpf _lhsImergeMap _lhsImergedChildren _lhsInmp _lhsInmprf _lhsIolab _lhsIoptions _lhsIpll _lhsIpmpf _lhsIpmprf) -> ( let
         _fattsX47 = Control.Monad.Identity.runIdentity (attach_T_FieldAtts ((sem_FieldAtts fatts_val_)))
         (T_FieldAtts_vOut46 _fattsIap _fattsIflab _fattsIfty _fattsIgen _fattsIinss _fattsIofld _fattsIolab _fattsIpmp _fattsIpmpr _fattsIself) = inv_FieldAtts_s47 _fattsX47 (T_FieldAtts_vIn46 _fattsOan _fattsOflab _fattsOnmprf _fattsOolab)
         _flab = rule120 _lhsIflab
         _atp = rule121 _lhsIpll
         fatts_val_ = rule122 _atp _lhsIan _lhsIpll
         _fattsOflab = rule123 _flab
         _label = rule124 _lhsIpll
         _foccsI = rule125 _atp _label _lhsIain
         _foccsS = rule126 _atp _label _lhsIasn
         _fieldMap = rule127 _foccsI _foccsS _label
         _lhsOfty :: FTY
         _lhsOfty = rule128 _label _lhsIdty
         _lhsOap :: A_P
         _lhsOap = rule129 _fattsIap
         _lhsOechilds :: EChildren
         _lhsOechilds = rule130  ()
         _lhsOfieldMap :: FMap
         _lhsOfieldMap = rule131 _fieldMap
         _lhsOgen :: Map Int Int
         _lhsOgen = rule132 _fattsIgen
         _lhsOhoMap :: HOMap
         _lhsOhoMap = rule133  ()
         _lhsOinss :: Map Int [Int]
         _lhsOinss = rule134 _fattsIinss
         _lhsOofld :: [(Int, Int)]
         _lhsOofld = rule135 _fattsIofld
         _lhsOpmp :: PMP
         _lhsOpmp = rule136 _fattsIpmp
         _lhsOpmpr :: PMP_R
         _lhsOpmpr = rule137 _fattsIpmpr
         _lhsOpts :: Set.Set FLabel
         _lhsOpts = rule138  ()
         _lhsOrefHoNts :: Set NontermIdent
         _lhsOrefHoNts = rule139  ()
         _lhsOrefNts :: Set NontermIdent
         _lhsOrefNts = rule140  ()
         _self = rule141  ()
         _lhsOself :: Children
         _lhsOself = rule142 _self
         _lhsOflab :: Int
         _lhsOflab = rule143 _flab
         _lhsOolab :: Int
         _lhsOolab = rule144 _fattsIolab
         _fattsOan = rule145 _lhsIan
         _fattsOnmprf = rule146 _lhsInmprf
         _fattsOolab = rule147 _lhsIolab
         __result_ = T_Children_vOut37 _lhsOap _lhsOechilds _lhsOfieldMap _lhsOflab _lhsOfty _lhsOgen _lhsOhoMap _lhsOinss _lhsOofld _lhsOolab _lhsOpmp _lhsOpmpr _lhsOpts _lhsOrefHoNts _lhsOrefNts _lhsOself
         in __result_ )
     in C_Children_s38 v37
   {-# INLINE rule120 #-}
   {-# LINE 161 "src-ag/LOAG/Prepare.ag" #-}
   rule120 = \ ((_lhsIflab) :: Int) ->
                    {-# LINE 161 "src-ag/LOAG/Prepare.ag" #-}
                    _lhsIflab + 1
                    {-# LINE 1598 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule121 #-}
   {-# LINE 162 "src-ag/LOAG/Prepare.ag" #-}
   rule121 = \ ((_lhsIpll) :: PLabel) ->
                    {-# LINE 162 "src-ag/LOAG/Prepare.ag" #-}
                    fst _lhsIpll
                    {-# LINE 1604 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule122 #-}
   {-# LINE 164 "src-ag/LOAG/Prepare.ag" #-}
   rule122 = \ _atp ((_lhsIan) :: MyType -> MyAttributes) ((_lhsIpll) :: PLabel) ->
                    {-# LINE 164 "src-ag/LOAG/Prepare.ag" #-}
                    map ((FieldAtt _atp     _lhsIpll "lhs") . alab) $
                          _lhsIan _atp
                    {-# LINE 1611 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule123 #-}
   {-# LINE 166 "src-ag/LOAG/Prepare.ag" #-}
   rule123 = \ _flab ->
                    {-# LINE 166 "src-ag/LOAG/Prepare.ag" #-}
                    _flab
                    {-# LINE 1617 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule124 #-}
   {-# LINE 167 "src-ag/LOAG/Prepare.ag" #-}
   rule124 = \ ((_lhsIpll) :: PLabel) ->
                    {-# LINE 167 "src-ag/LOAG/Prepare.ag" #-}
                    (_lhsIpll, "lhs")
                    {-# LINE 1623 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule125 #-}
   {-# LINE 168 "src-ag/LOAG/Prepare.ag" #-}
   rule125 = \ _atp _label ((_lhsIain) :: MyType -> MyAttributes) ->
                    {-# LINE 168 "src-ag/LOAG/Prepare.ag" #-}
                    Set.fromList $ handAllOut _label     $ _lhsIain _atp
                    {-# LINE 1629 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule126 #-}
   {-# LINE 169 "src-ag/LOAG/Prepare.ag" #-}
   rule126 = \ _atp _label ((_lhsIasn) :: MyType -> MyAttributes) ->
                    {-# LINE 169 "src-ag/LOAG/Prepare.ag" #-}
                    Set.fromList $ handAllOut _label     $ _lhsIasn _atp
                    {-# LINE 1635 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule127 #-}
   {-# LINE 170 "src-ag/LOAG/Prepare.ag" #-}
   rule127 = \ _foccsI _foccsS _label ->
                    {-# LINE 170 "src-ag/LOAG/Prepare.ag" #-}
                    Map.singleton _label     (_foccsI    , _foccsS    )
                    {-# LINE 1641 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule128 #-}
   {-# LINE 171 "src-ag/LOAG/Prepare.ag" #-}
   rule128 = \ _label ((_lhsIdty) :: MyType) ->
                    {-# LINE 171 "src-ag/LOAG/Prepare.ag" #-}
                    Map.singleton _label     _lhsIdty
                    {-# LINE 1647 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule129 #-}
   rule129 = \ ((_fattsIap) :: A_P) ->
     _fattsIap
   {-# INLINE rule130 #-}
   rule130 = \  (_ :: ()) ->
     []
   {-# INLINE rule131 #-}
   rule131 = \ _fieldMap ->
     _fieldMap
   {-# INLINE rule132 #-}
   rule132 = \ ((_fattsIgen) :: Map Int Int) ->
     _fattsIgen
   {-# INLINE rule133 #-}
   rule133 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule134 #-}
   rule134 = \ ((_fattsIinss) :: Map Int [Int]) ->
     _fattsIinss
   {-# INLINE rule135 #-}
   rule135 = \ ((_fattsIofld) :: [(Int, Int)]) ->
     _fattsIofld
   {-# INLINE rule136 #-}
   rule136 = \ ((_fattsIpmp) :: PMP) ->
     _fattsIpmp
   {-# INLINE rule137 #-}
   rule137 = \ ((_fattsIpmpr) :: PMP_R) ->
     _fattsIpmpr
   {-# INLINE rule138 #-}
   rule138 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule139 #-}
   rule139 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule140 #-}
   rule140 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule141 #-}
   rule141 = \  (_ :: ()) ->
     []
   {-# INLINE rule142 #-}
   rule142 = \ _self ->
     _self
   {-# INLINE rule143 #-}
   rule143 = \ _flab ->
     _flab
   {-# INLINE rule144 #-}
   rule144 = \ ((_fattsIolab) :: Int) ->
     _fattsIolab
   {-# INLINE rule145 #-}
   rule145 = \ ((_lhsIan) :: MyType -> MyAttributes) ->
     _lhsIan
   {-# INLINE rule146 #-}
   rule146 = \ ((_lhsInmprf) :: NMP_R) ->
     _lhsInmprf
   {-# INLINE rule147 #-}
   rule147 = \ ((_lhsIolab) :: Int) ->
     _lhsIolab

-- Expression --------------------------------------------------
-- wrapper
data Inh_Expression  = Inh_Expression { pll_Inh_Expression :: (PLabel), pts_Inh_Expression :: (Set.Set (FLabel)) }
data Syn_Expression  = Syn_Expression { copy_Syn_Expression :: (Expression), self_Syn_Expression :: (Expression), used_Syn_Expression :: (Set.Set MyOccurrence) }
{-# INLINABLE wrap_Expression #-}
wrap_Expression :: T_Expression  -> Inh_Expression  -> (Syn_Expression )
wrap_Expression (T_Expression act) (Inh_Expression _lhsIpll _lhsIpts) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg40 = T_Expression_vIn40 _lhsIpll _lhsIpts
        (T_Expression_vOut40 _lhsOcopy _lhsOself _lhsOused) <- return (inv_Expression_s41 sem arg40)
        return (Syn_Expression _lhsOcopy _lhsOself _lhsOused)
   )

-- cata
{-# INLINE sem_Expression #-}
sem_Expression :: Expression  -> T_Expression 
sem_Expression ( Expression pos_ tks_  ) = sem_Expression_Expression pos_ tks_ 

-- semantic domain
newtype T_Expression  = T_Expression {
                                     attach_T_Expression :: Identity (T_Expression_s41 )
                                     }
newtype T_Expression_s41  = C_Expression_s41 {
                                             inv_Expression_s41 :: (T_Expression_v40 )
                                             }
data T_Expression_s42  = C_Expression_s42
type T_Expression_v40  = (T_Expression_vIn40 ) -> (T_Expression_vOut40 )
data T_Expression_vIn40  = T_Expression_vIn40 (PLabel) (Set.Set (FLabel))
data T_Expression_vOut40  = T_Expression_vOut40 (Expression) (Expression) (Set.Set MyOccurrence)
{-# NOINLINE sem_Expression_Expression #-}
sem_Expression_Expression :: (Pos) -> ([HsToken]) ->  T_Expression 
sem_Expression_Expression arg_pos_ arg_tks_  = T_Expression (return st41) where
   {-# NOINLINE st41 #-}
   st41 = let
      v40 :: T_Expression_v40 
      v40 = \ (T_Expression_vIn40 _lhsIpll _lhsIpts) -> ( let
         _tokensX59 = Control.Monad.Identity.runIdentity (attach_T_HsTokensRoot ((sem_HsTokensRoot tokens_val_)))
         (T_HsTokensRoot_vOut58 _tokensIself _tokensIused) = inv_HsTokensRoot_s59 _tokensX59 (T_HsTokensRoot_vIn58 _tokensOpll _tokensOpts)
         tokens_val_ = rule148 arg_tks_
         _tokensOpll = rule149 _lhsIpll
         _tokensOpts = rule150 _lhsIpts
         _lhsOused :: Set.Set MyOccurrence
         _lhsOused = rule151 _tokensIused
         _copy = rule152 arg_pos_ arg_tks_
         _self = rule153 arg_pos_ arg_tks_
         _lhsOcopy :: Expression
         _lhsOcopy = rule154 _copy
         _lhsOself :: Expression
         _lhsOself = rule155 _self
         __result_ = T_Expression_vOut40 _lhsOcopy _lhsOself _lhsOused
         in __result_ )
     in C_Expression_s41 v40
   {-# INLINE rule148 #-}
   {-# LINE 273 "src-ag/LOAG/Prepare.ag" #-}
   rule148 = \ tks_ ->
                    {-# LINE 273 "src-ag/LOAG/Prepare.ag" #-}
                    HsTokensRoot tks_
                    {-# LINE 1764 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule149 #-}
   {-# LINE 274 "src-ag/LOAG/Prepare.ag" #-}
   rule149 = \ ((_lhsIpll) :: PLabel) ->
                    {-# LINE 274 "src-ag/LOAG/Prepare.ag" #-}
                    _lhsIpll
                    {-# LINE 1770 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule150 #-}
   {-# LINE 275 "src-ag/LOAG/Prepare.ag" #-}
   rule150 = \ ((_lhsIpts) :: Set.Set (FLabel)) ->
                    {-# LINE 275 "src-ag/LOAG/Prepare.ag" #-}
                    _lhsIpts
                    {-# LINE 1776 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule151 #-}
   {-# LINE 276 "src-ag/LOAG/Prepare.ag" #-}
   rule151 = \ ((_tokensIused) :: Set.Set MyOccurrence) ->
                    {-# LINE 276 "src-ag/LOAG/Prepare.ag" #-}
                    _tokensIused
                    {-# LINE 1782 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule152 #-}
   rule152 = \ pos_ tks_ ->
     Expression pos_ tks_
   {-# INLINE rule153 #-}
   rule153 = \ pos_ tks_ ->
     Expression pos_ tks_
   {-# INLINE rule154 #-}
   rule154 = \ _copy ->
     _copy
   {-# INLINE rule155 #-}
   rule155 = \ _self ->
     _self

-- FieldAtt ----------------------------------------------------
-- wrapper
data Inh_FieldAtt  = Inh_FieldAtt { an_Inh_FieldAtt :: (MyType -> MyAttributes), flab_Inh_FieldAtt :: (Int), nmprf_Inh_FieldAtt :: (NMP_R), olab_Inh_FieldAtt :: (Int) }
data Syn_FieldAtt  = Syn_FieldAtt { ap_Syn_FieldAtt :: (A_P), flab_Syn_FieldAtt :: (Int), fty_Syn_FieldAtt :: (FTY), gen_Syn_FieldAtt :: (Map Int Int), inss_Syn_FieldAtt :: (Map Int [Int]), ofld_Syn_FieldAtt :: ([(Int, Int)]), olab_Syn_FieldAtt :: (Int), pmp_Syn_FieldAtt :: (PMP), pmpr_Syn_FieldAtt :: (PMP_R), self_Syn_FieldAtt :: (FieldAtt) }
{-# INLINABLE wrap_FieldAtt #-}
wrap_FieldAtt :: T_FieldAtt  -> Inh_FieldAtt  -> (Syn_FieldAtt )
wrap_FieldAtt (T_FieldAtt act) (Inh_FieldAtt _lhsIan _lhsIflab _lhsInmprf _lhsIolab) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg43 = T_FieldAtt_vIn43 _lhsIan _lhsIflab _lhsInmprf _lhsIolab
        (T_FieldAtt_vOut43 _lhsOap _lhsOflab _lhsOfty _lhsOgen _lhsOinss _lhsOofld _lhsOolab _lhsOpmp _lhsOpmpr _lhsOself) <- return (inv_FieldAtt_s44 sem arg43)
        return (Syn_FieldAtt _lhsOap _lhsOflab _lhsOfty _lhsOgen _lhsOinss _lhsOofld _lhsOolab _lhsOpmp _lhsOpmpr _lhsOself)
   )

-- cata
{-# INLINE sem_FieldAtt #-}
sem_FieldAtt :: FieldAtt  -> T_FieldAtt 
sem_FieldAtt ( FieldAtt t_ p_ f_ a_ ) = sem_FieldAtt_FieldAtt t_ p_ f_ a_

-- semantic domain
newtype T_FieldAtt  = T_FieldAtt {
                                 attach_T_FieldAtt :: Identity (T_FieldAtt_s44 )
                                 }
newtype T_FieldAtt_s44  = C_FieldAtt_s44 {
                                         inv_FieldAtt_s44 :: (T_FieldAtt_v43 )
                                         }
data T_FieldAtt_s45  = C_FieldAtt_s45
type T_FieldAtt_v43  = (T_FieldAtt_vIn43 ) -> (T_FieldAtt_vOut43 )
data T_FieldAtt_vIn43  = T_FieldAtt_vIn43 (MyType -> MyAttributes) (Int) (NMP_R) (Int)
data T_FieldAtt_vOut43  = T_FieldAtt_vOut43 (A_P) (Int) (FTY) (Map Int Int) (Map Int [Int]) ([(Int, Int)]) (Int) (PMP) (PMP_R) (FieldAtt)
{-# NOINLINE sem_FieldAtt_FieldAtt #-}
sem_FieldAtt_FieldAtt :: (MyType) -> (PLabel) -> (FLabel) -> (ALabel) -> T_FieldAtt 
sem_FieldAtt_FieldAtt arg_t_ arg_p_ arg_f_ arg_a_ = T_FieldAtt (return st44) where
   {-# NOINLINE st44 #-}
   st44 = let
      v43 :: T_FieldAtt_v43 
      v43 = \ (T_FieldAtt_vIn43 _lhsIan _lhsIflab _lhsInmprf _lhsIolab) -> ( let
         _olab = rule156 _lhsIolab
         _alab = rule157 _att _lhsInmprf
         _att = rule158 arg_a_ arg_t_
         _occ = rule159 arg_a_ arg_f_ arg_p_
         _pmp = rule160 _occ _olab
         _pmpr = rule161 _occ _olab
         _inss = rule162 _alab _olab
         _gen = rule163 _alab _olab
         _lhsOap :: A_P
         _lhsOap = rule164 _occ arg_p_
         _lhsOofld :: [(Int, Int)]
         _lhsOofld = rule165 _lhsIflab _olab
         _lhsOfty :: FTY
         _lhsOfty = rule166  ()
         _lhsOgen :: Map Int Int
         _lhsOgen = rule167 _gen
         _lhsOinss :: Map Int [Int]
         _lhsOinss = rule168 _inss
         _lhsOpmp :: PMP
         _lhsOpmp = rule169 _pmp
         _lhsOpmpr :: PMP_R
         _lhsOpmpr = rule170 _pmpr
         _self = rule171 arg_a_ arg_f_ arg_p_ arg_t_
         _lhsOself :: FieldAtt
         _lhsOself = rule172 _self
         _lhsOflab :: Int
         _lhsOflab = rule173 _lhsIflab
         _lhsOolab :: Int
         _lhsOolab = rule174 _olab
         __result_ = T_FieldAtt_vOut43 _lhsOap _lhsOflab _lhsOfty _lhsOgen _lhsOinss _lhsOofld _lhsOolab _lhsOpmp _lhsOpmpr _lhsOself
         in __result_ )
     in C_FieldAtt_s44 v43
   {-# INLINE rule156 #-}
   {-# LINE 193 "src-ag/LOAG/Prepare.ag" #-}
   rule156 = \ ((_lhsIolab) :: Int) ->
                    {-# LINE 193 "src-ag/LOAG/Prepare.ag" #-}
                    _lhsIolab + 1
                    {-# LINE 1870 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule157 #-}
   {-# LINE 194 "src-ag/LOAG/Prepare.ag" #-}
   rule157 = \ _att ((_lhsInmprf) :: NMP_R) ->
                    {-# LINE 194 "src-ag/LOAG/Prepare.ag" #-}
                    findWithErr _lhsInmprf "getting attr label" _att
                    {-# LINE 1876 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule158 #-}
   {-# LINE 195 "src-ag/LOAG/Prepare.ag" #-}
   rule158 = \ a_ t_ ->
                    {-# LINE 195 "src-ag/LOAG/Prepare.ag" #-}
                    t_ <.> a_
                    {-# LINE 1882 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule159 #-}
   {-# LINE 196 "src-ag/LOAG/Prepare.ag" #-}
   rule159 = \ a_ f_ p_ ->
                    {-# LINE 196 "src-ag/LOAG/Prepare.ag" #-}
                    (p_, f_) >.< a_
                    {-# LINE 1888 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule160 #-}
   {-# LINE 197 "src-ag/LOAG/Prepare.ag" #-}
   rule160 = \ _occ _olab ->
                    {-# LINE 197 "src-ag/LOAG/Prepare.ag" #-}
                    Map.singleton _olab     _occ
                    {-# LINE 1894 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule161 #-}
   {-# LINE 198 "src-ag/LOAG/Prepare.ag" #-}
   rule161 = \ _occ _olab ->
                    {-# LINE 198 "src-ag/LOAG/Prepare.ag" #-}
                    Map.singleton _occ      _olab
                    {-# LINE 1900 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule162 #-}
   {-# LINE 199 "src-ag/LOAG/Prepare.ag" #-}
   rule162 = \ _alab _olab ->
                    {-# LINE 199 "src-ag/LOAG/Prepare.ag" #-}
                    Map.singleton _alab     [_olab    ]
                    {-# LINE 1906 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule163 #-}
   {-# LINE 200 "src-ag/LOAG/Prepare.ag" #-}
   rule163 = \ _alab _olab ->
                    {-# LINE 200 "src-ag/LOAG/Prepare.ag" #-}
                    Map.singleton _olab     _alab
                    {-# LINE 1912 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule164 #-}
   {-# LINE 201 "src-ag/LOAG/Prepare.ag" #-}
   rule164 = \ _occ p_ ->
                    {-# LINE 201 "src-ag/LOAG/Prepare.ag" #-}
                    Map.singleton p_ [_occ    ]
                    {-# LINE 1918 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule165 #-}
   {-# LINE 202 "src-ag/LOAG/Prepare.ag" #-}
   rule165 = \ ((_lhsIflab) :: Int) _olab ->
                    {-# LINE 202 "src-ag/LOAG/Prepare.ag" #-}
                    [(_olab    , _lhsIflab)]
                    {-# LINE 1924 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule166 #-}
   rule166 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule167 #-}
   rule167 = \ _gen ->
     _gen
   {-# INLINE rule168 #-}
   rule168 = \ _inss ->
     _inss
   {-# INLINE rule169 #-}
   rule169 = \ _pmp ->
     _pmp
   {-# INLINE rule170 #-}
   rule170 = \ _pmpr ->
     _pmpr
   {-# INLINE rule171 #-}
   rule171 = \ a_ f_ p_ t_ ->
     FieldAtt t_ p_ f_ a_
   {-# INLINE rule172 #-}
   rule172 = \ _self ->
     _self
   {-# INLINE rule173 #-}
   rule173 = \ ((_lhsIflab) :: Int) ->
     _lhsIflab
   {-# INLINE rule174 #-}
   rule174 = \ _olab ->
     _olab

-- FieldAtts ---------------------------------------------------
-- wrapper
data Inh_FieldAtts  = Inh_FieldAtts { an_Inh_FieldAtts :: (MyType -> MyAttributes), flab_Inh_FieldAtts :: (Int), nmprf_Inh_FieldAtts :: (NMP_R), olab_Inh_FieldAtts :: (Int) }
data Syn_FieldAtts  = Syn_FieldAtts { ap_Syn_FieldAtts :: (A_P), flab_Syn_FieldAtts :: (Int), fty_Syn_FieldAtts :: (FTY), gen_Syn_FieldAtts :: (Map Int Int), inss_Syn_FieldAtts :: (Map Int [Int]), ofld_Syn_FieldAtts :: ([(Int, Int)]), olab_Syn_FieldAtts :: (Int), pmp_Syn_FieldAtts :: (PMP), pmpr_Syn_FieldAtts :: (PMP_R), self_Syn_FieldAtts :: (FieldAtts) }
{-# INLINABLE wrap_FieldAtts #-}
wrap_FieldAtts :: T_FieldAtts  -> Inh_FieldAtts  -> (Syn_FieldAtts )
wrap_FieldAtts (T_FieldAtts act) (Inh_FieldAtts _lhsIan _lhsIflab _lhsInmprf _lhsIolab) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg46 = T_FieldAtts_vIn46 _lhsIan _lhsIflab _lhsInmprf _lhsIolab
        (T_FieldAtts_vOut46 _lhsOap _lhsOflab _lhsOfty _lhsOgen _lhsOinss _lhsOofld _lhsOolab _lhsOpmp _lhsOpmpr _lhsOself) <- return (inv_FieldAtts_s47 sem arg46)
        return (Syn_FieldAtts _lhsOap _lhsOflab _lhsOfty _lhsOgen _lhsOinss _lhsOofld _lhsOolab _lhsOpmp _lhsOpmpr _lhsOself)
   )

-- cata
{-# NOINLINE sem_FieldAtts #-}
sem_FieldAtts :: FieldAtts  -> T_FieldAtts 
sem_FieldAtts list = Prelude.foldr sem_FieldAtts_Cons sem_FieldAtts_Nil (Prelude.map sem_FieldAtt list)

-- semantic domain
newtype T_FieldAtts  = T_FieldAtts {
                                   attach_T_FieldAtts :: Identity (T_FieldAtts_s47 )
                                   }
newtype T_FieldAtts_s47  = C_FieldAtts_s47 {
                                           inv_FieldAtts_s47 :: (T_FieldAtts_v46 )
                                           }
data T_FieldAtts_s48  = C_FieldAtts_s48
type T_FieldAtts_v46  = (T_FieldAtts_vIn46 ) -> (T_FieldAtts_vOut46 )
data T_FieldAtts_vIn46  = T_FieldAtts_vIn46 (MyType -> MyAttributes) (Int) (NMP_R) (Int)
data T_FieldAtts_vOut46  = T_FieldAtts_vOut46 (A_P) (Int) (FTY) (Map Int Int) (Map Int [Int]) ([(Int, Int)]) (Int) (PMP) (PMP_R) (FieldAtts)
{-# NOINLINE sem_FieldAtts_Cons #-}
sem_FieldAtts_Cons :: T_FieldAtt  -> T_FieldAtts  -> T_FieldAtts 
sem_FieldAtts_Cons arg_hd_ arg_tl_ = T_FieldAtts (return st47) where
   {-# NOINLINE st47 #-}
   st47 = let
      v46 :: T_FieldAtts_v46 
      v46 = \ (T_FieldAtts_vIn46 _lhsIan _lhsIflab _lhsInmprf _lhsIolab) -> ( let
         _hdX44 = Control.Monad.Identity.runIdentity (attach_T_FieldAtt (arg_hd_))
         _tlX47 = Control.Monad.Identity.runIdentity (attach_T_FieldAtts (arg_tl_))
         (T_FieldAtt_vOut43 _hdIap _hdIflab _hdIfty _hdIgen _hdIinss _hdIofld _hdIolab _hdIpmp _hdIpmpr _hdIself) = inv_FieldAtt_s44 _hdX44 (T_FieldAtt_vIn43 _hdOan _hdOflab _hdOnmprf _hdOolab)
         (T_FieldAtts_vOut46 _tlIap _tlIflab _tlIfty _tlIgen _tlIinss _tlIofld _tlIolab _tlIpmp _tlIpmpr _tlIself) = inv_FieldAtts_s47 _tlX47 (T_FieldAtts_vIn46 _tlOan _tlOflab _tlOnmprf _tlOolab)
         _lhsOap :: A_P
         _lhsOap = rule175 _hdIap _tlIap
         _lhsOfty :: FTY
         _lhsOfty = rule176 _hdIfty _tlIfty
         _lhsOgen :: Map Int Int
         _lhsOgen = rule177 _hdIgen _tlIgen
         _lhsOinss :: Map Int [Int]
         _lhsOinss = rule178 _hdIinss _tlIinss
         _lhsOofld :: [(Int, Int)]
         _lhsOofld = rule179 _hdIofld _tlIofld
         _lhsOpmp :: PMP
         _lhsOpmp = rule180 _hdIpmp _tlIpmp
         _lhsOpmpr :: PMP_R
         _lhsOpmpr = rule181 _hdIpmpr _tlIpmpr
         _self = rule182 _hdIself _tlIself
         _lhsOself :: FieldAtts
         _lhsOself = rule183 _self
         _lhsOflab :: Int
         _lhsOflab = rule184 _tlIflab
         _lhsOolab :: Int
         _lhsOolab = rule185 _tlIolab
         _hdOan = rule186 _lhsIan
         _hdOflab = rule187 _lhsIflab
         _hdOnmprf = rule188 _lhsInmprf
         _hdOolab = rule189 _lhsIolab
         _tlOan = rule190 _lhsIan
         _tlOflab = rule191 _hdIflab
         _tlOnmprf = rule192 _lhsInmprf
         _tlOolab = rule193 _hdIolab
         __result_ = T_FieldAtts_vOut46 _lhsOap _lhsOflab _lhsOfty _lhsOgen _lhsOinss _lhsOofld _lhsOolab _lhsOpmp _lhsOpmpr _lhsOself
         in __result_ )
     in C_FieldAtts_s47 v46
   {-# INLINE rule175 #-}
   rule175 = \ ((_hdIap) :: A_P) ((_tlIap) :: A_P) ->
     (Map.unionWith (++) _hdIap _tlIap)
   {-# INLINE rule176 #-}
   rule176 = \ ((_hdIfty) :: FTY) ((_tlIfty) :: FTY) ->
     (Map.union _hdIfty _tlIfty)
   {-# INLINE rule177 #-}
   rule177 = \ ((_hdIgen) :: Map Int Int) ((_tlIgen) :: Map Int Int) ->
     (Map.union _hdIgen _tlIgen)
   {-# INLINE rule178 #-}
   rule178 = \ ((_hdIinss) :: Map Int [Int]) ((_tlIinss) :: Map Int [Int]) ->
     (Map.unionWith (++) _hdIinss _tlIinss)
   {-# INLINE rule179 #-}
   rule179 = \ ((_hdIofld) :: [(Int, Int)]) ((_tlIofld) :: [(Int, Int)]) ->
     ((++) _hdIofld _tlIofld)
   {-# INLINE rule180 #-}
   rule180 = \ ((_hdIpmp) :: PMP) ((_tlIpmp) :: PMP) ->
     (Map.union _hdIpmp _tlIpmp)
   {-# INLINE rule181 #-}
   rule181 = \ ((_hdIpmpr) :: PMP_R) ((_tlIpmpr) :: PMP_R) ->
     (Map.union _hdIpmpr _tlIpmpr)
   {-# INLINE rule182 #-}
   rule182 = \ ((_hdIself) :: FieldAtt) ((_tlIself) :: FieldAtts) ->
     (:) _hdIself _tlIself
   {-# INLINE rule183 #-}
   rule183 = \ _self ->
     _self
   {-# INLINE rule184 #-}
   rule184 = \ ((_tlIflab) :: Int) ->
     _tlIflab
   {-# INLINE rule185 #-}
   rule185 = \ ((_tlIolab) :: Int) ->
     _tlIolab
   {-# INLINE rule186 #-}
   rule186 = \ ((_lhsIan) :: MyType -> MyAttributes) ->
     _lhsIan
   {-# INLINE rule187 #-}
   rule187 = \ ((_lhsIflab) :: Int) ->
     _lhsIflab
   {-# INLINE rule188 #-}
   rule188 = \ ((_lhsInmprf) :: NMP_R) ->
     _lhsInmprf
   {-# INLINE rule189 #-}
   rule189 = \ ((_lhsIolab) :: Int) ->
     _lhsIolab
   {-# INLINE rule190 #-}
   rule190 = \ ((_lhsIan) :: MyType -> MyAttributes) ->
     _lhsIan
   {-# INLINE rule191 #-}
   rule191 = \ ((_hdIflab) :: Int) ->
     _hdIflab
   {-# INLINE rule192 #-}
   rule192 = \ ((_lhsInmprf) :: NMP_R) ->
     _lhsInmprf
   {-# INLINE rule193 #-}
   rule193 = \ ((_hdIolab) :: Int) ->
     _hdIolab
{-# NOINLINE sem_FieldAtts_Nil #-}
sem_FieldAtts_Nil ::  T_FieldAtts 
sem_FieldAtts_Nil  = T_FieldAtts (return st47) where
   {-# NOINLINE st47 #-}
   st47 = let
      v46 :: T_FieldAtts_v46 
      v46 = \ (T_FieldAtts_vIn46 _lhsIan _lhsIflab _lhsInmprf _lhsIolab) -> ( let
         _lhsOap :: A_P
         _lhsOap = rule194  ()
         _lhsOfty :: FTY
         _lhsOfty = rule195  ()
         _lhsOgen :: Map Int Int
         _lhsOgen = rule196  ()
         _lhsOinss :: Map Int [Int]
         _lhsOinss = rule197  ()
         _lhsOofld :: [(Int, Int)]
         _lhsOofld = rule198  ()
         _lhsOpmp :: PMP
         _lhsOpmp = rule199  ()
         _lhsOpmpr :: PMP_R
         _lhsOpmpr = rule200  ()
         _self = rule201  ()
         _lhsOself :: FieldAtts
         _lhsOself = rule202 _self
         _lhsOflab :: Int
         _lhsOflab = rule203 _lhsIflab
         _lhsOolab :: Int
         _lhsOolab = rule204 _lhsIolab
         __result_ = T_FieldAtts_vOut46 _lhsOap _lhsOflab _lhsOfty _lhsOgen _lhsOinss _lhsOofld _lhsOolab _lhsOpmp _lhsOpmpr _lhsOself
         in __result_ )
     in C_FieldAtts_s47 v46
   {-# INLINE rule194 #-}
   rule194 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule195 #-}
   rule195 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule196 #-}
   rule196 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule197 #-}
   rule197 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule198 #-}
   rule198 = \  (_ :: ()) ->
     []
   {-# INLINE rule199 #-}
   rule199 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule200 #-}
   rule200 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule201 #-}
   rule201 = \  (_ :: ()) ->
     []
   {-# INLINE rule202 #-}
   rule202 = \ _self ->
     _self
   {-# INLINE rule203 #-}
   rule203 = \ ((_lhsIflab) :: Int) ->
     _lhsIflab
   {-# INLINE rule204 #-}
   rule204 = \ ((_lhsIolab) :: Int) ->
     _lhsIolab

-- Grammar -----------------------------------------------------
-- wrapper
data Inh_Grammar  = Inh_Grammar { options_Inh_Grammar :: (Options) }
data Syn_Grammar  = Syn_Grammar { ads_Syn_Grammar :: (Maybe PP_Doc), errors_Syn_Grammar :: (Seq.Seq Error), inhmap_Syn_Grammar :: (Map.Map NontermIdent Attributes), localSigMap_Syn_Grammar :: (Map.Map NontermIdent (Map.Map ConstructorIdent (Map.Map Identifier Type))), output_Syn_Grammar :: (ExecutionPlan), self_Syn_Grammar :: (Grammar), synmap_Syn_Grammar :: (Map.Map NontermIdent Attributes) }
{-# INLINABLE wrap_Grammar #-}
wrap_Grammar :: T_Grammar  -> Inh_Grammar  -> (Syn_Grammar )
wrap_Grammar (T_Grammar act) (Inh_Grammar _lhsIoptions) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg49 = T_Grammar_vIn49 _lhsIoptions
        (T_Grammar_vOut49 _lhsOads _lhsOerrors _lhsOinhmap _lhsOlocalSigMap _lhsOoutput _lhsOself _lhsOsynmap) <- return (inv_Grammar_s50 sem arg49)
        return (Syn_Grammar _lhsOads _lhsOerrors _lhsOinhmap _lhsOlocalSigMap _lhsOoutput _lhsOself _lhsOsynmap)
   )

-- cata
{-# INLINE sem_Grammar #-}
sem_Grammar :: Grammar  -> T_Grammar 
sem_Grammar ( Grammar typeSyns_ useMap_ derivings_ wrappers_ nonts_ pragmas_ manualAttrOrderMap_ paramMap_ contextMap_ quantMap_ uniqueMap_ augmentsMap_ aroundsMap_ mergeMap_  ) = sem_Grammar_Grammar typeSyns_ useMap_ derivings_ wrappers_ ( sem_Nonterminals nonts_ ) pragmas_ manualAttrOrderMap_ paramMap_ contextMap_ quantMap_ uniqueMap_ augmentsMap_ aroundsMap_ mergeMap_ 

-- semantic domain
newtype T_Grammar  = T_Grammar {
                               attach_T_Grammar :: Identity (T_Grammar_s50 )
                               }
newtype T_Grammar_s50  = C_Grammar_s50 {
                                       inv_Grammar_s50 :: (T_Grammar_v49 )
                                       }
data T_Grammar_s51  = C_Grammar_s51
type T_Grammar_v49  = (T_Grammar_vIn49 ) -> (T_Grammar_vOut49 )
data T_Grammar_vIn49  = T_Grammar_vIn49 (Options)
data T_Grammar_vOut49  = T_Grammar_vOut49 (Maybe PP_Doc) (Seq.Seq Error) (Map.Map NontermIdent Attributes) (Map.Map NontermIdent (Map.Map ConstructorIdent (Map.Map Identifier Type))) (ExecutionPlan) (Grammar) (Map.Map NontermIdent Attributes)
{-# NOINLINE sem_Grammar_Grammar #-}
sem_Grammar_Grammar :: (TypeSyns) -> (UseMap) -> (Derivings) -> (Set NontermIdent) -> T_Nonterminals  -> (PragmaMap) -> (AttrOrderMap) -> (ParamMap) -> (ContextMap) -> (QuantMap) -> (UniqueMap) -> (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) -> (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) -> (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier, [Identifier], Expression)))) ->  T_Grammar 
sem_Grammar_Grammar arg_typeSyns_ arg_useMap_ arg_derivings_ arg_wrappers_ arg_nonts_ arg_pragmas_ arg_manualAttrOrderMap_ arg_paramMap_ arg_contextMap_ arg_quantMap_ arg_uniqueMap_ arg_augmentsMap_ arg_aroundsMap_ arg_mergeMap_  = T_Grammar (return st50) where
   {-# NOINLINE st50 #-}
   st50 = let
      v49 :: T_Grammar_v49 
      v49 = \ (T_Grammar_vIn49 _lhsIoptions) -> ( let
         _nontsX74 = Control.Monad.Identity.runIdentity (attach_T_Nonterminals (arg_nonts_))
         _smfX62 = Control.Monad.Identity.runIdentity (attach_T_LOAGRep ((sem_LOAGRep smf_val_)))
         (T_Nonterminals_vOut73 _nontsIads _nontsIap _nontsIenonts _nontsIfdps _nontsIfieldMap _nontsIflab _nontsIfsInP _nontsIfty _nontsIgen _nontsIhoMap _nontsIinhmap _nontsIinhs _nontsIinss _nontsIlfp _nontsIlfpr _nontsIlocalSigMap _nontsIntDeps _nontsIntHoDeps _nontsIofld _nontsIolab _nontsIpmp _nontsIpmpr _nontsIps _nontsIruleMap _nontsIrulenumber _nontsIself _nontsIsfp _nontsIsynmap _nontsIsyns _nontsIvisMap _nontsIvisitnum) = inv_Nonterminals_s74 _nontsX74 (T_Nonterminals_vIn73 _nontsOain _nontsOan _nontsOaroundMap _nontsOasn _nontsOaugM _nontsOclassContexts _nontsOclosedHoNtDeps _nontsOclosedHoNtRevDeps _nontsOclosedNtDeps _nontsOflab _nontsOfty _nontsOftyf _nontsOhoMapf _nontsOlfpf _nontsOmergeMap _nontsOnmp _nontsOnmprf _nontsOolab _nontsOoptions _nontsOpmpf _nontsOpmprf _nontsOres_ads _nontsOrulenumber _nontsOsched _nontsOtdp _nontsOvisMapf _nontsOvisitnum)
         (T_LOAGRep_vOut61 _smfIself) = inv_LOAGRep_s62 _smfX62 (T_LOAGRep_vIn61 )
         _closedNtDeps = rule205 _nontsIntDeps
         _closedHoNtDeps = rule206 _nontsIntHoDeps
         _closedHoNtRevDeps = rule207 _closedHoNtDeps
         _nontsOclassContexts = rule208 arg_contextMap_
         _nontsOaroundMap = rule209 arg_aroundsMap_
         _nontsOmergeMap = rule210 arg_mergeMap_
         _nontsOrulenumber = rule211  ()
         _initO = rule212 _nontsIpmp
         smf_val_ = rule213 _ain _an _asn _initO _nmp _nmpr _nontsIap _nontsIfieldMap _nontsIfsInP _nontsIfty _nontsIgen _nontsIinss _nontsIofld _nontsIpmp _nontsIpmpr _nontsIps _sfp
         _nmp = rule214 _atts
         _nmpr = rule215 _atts
         _an = rule216 _ain _asn
         _ain = rule217 _nontsIinhs
         _asn = rule218 _nontsIsyns
         _atts = rule219 _an
         _occs = rule220 _nontsIap
         _nontsOaugM = rule221 arg_manualAttrOrderMap_
         _nontsOain = rule222 _ain
         _nontsOasn = rule223 _asn
         _nontsOpmpf = rule224 _nontsIpmp
         _nontsOpmprf = rule225 _nontsIpmpr
         _nontsOlfpf = rule226 _nontsIlfp
         _nontsOhoMapf = rule227 _nontsIhoMap
         _nontsOftyf = rule228 _nontsIfty
         _nontsOfty = rule229 _nontsIfty
         _ps = rule230 _nontsIps
         _nontsOan = rule231 _an
         _nontsOnmprf = rule232 _nmpr
         _nontsOolab = rule233 _nmp
         _nontsOflab = rule234  ()
         _sfp = rule235 _nontsIlfp _nontsIsfp
         _lhsOerrors :: Seq.Seq Error
         _lhsOerrors = rule236 _schedRes
         _lhsOads :: Maybe PP_Doc
         _lhsOads = rule237 _lhsIoptions _nontsIpmp _schedRes
         _lhsOoutput :: ExecutionPlan
         _lhsOoutput = rule238 _nontsIenonts arg_derivings_ arg_typeSyns_ arg_wrappers_
         _nontsOsched = rule239 _schedRes
         _nontsOtdp = rule240 _schedRes
         _schedRes = rule241 _ag _lhsIoptions _loagRes _nontsIads _self _smfIself
         _loagRes = rule242 _ag _lhsIoptions
         _ag = rule243 _self _smfIself
         _nontsOres_ads = rule244 _schedRes
         _nontsOvisMapf = rule245 _nontsIvisMap
         _nontsOvisitnum = rule246  ()
         _lhsOinhmap :: Map.Map NontermIdent Attributes
         _lhsOinhmap = rule247 _nontsIinhmap
         _lhsOlocalSigMap :: Map.Map NontermIdent (Map.Map ConstructorIdent (Map.Map Identifier Type))
         _lhsOlocalSigMap = rule248 _nontsIlocalSigMap
         _lhsOsynmap :: Map.Map NontermIdent Attributes
         _lhsOsynmap = rule249 _nontsIsynmap
         _self = rule250 _nontsIself arg_aroundsMap_ arg_augmentsMap_ arg_contextMap_ arg_derivings_ arg_manualAttrOrderMap_ arg_mergeMap_ arg_paramMap_ arg_pragmas_ arg_quantMap_ arg_typeSyns_ arg_uniqueMap_ arg_useMap_ arg_wrappers_
         _lhsOself :: Grammar
         _lhsOself = rule251 _self
         _nontsOclosedHoNtDeps = rule252 _closedHoNtDeps
         _nontsOclosedHoNtRevDeps = rule253 _closedHoNtRevDeps
         _nontsOclosedNtDeps = rule254 _closedNtDeps
         _nontsOnmp = rule255 _nmp
         _nontsOoptions = rule256 _lhsIoptions
         __result_ = T_Grammar_vOut49 _lhsOads _lhsOerrors _lhsOinhmap _lhsOlocalSigMap _lhsOoutput _lhsOself _lhsOsynmap
         in __result_ )
     in C_Grammar_s50 v49
   {-# INLINE rule205 #-}
   {-# LINE 40 "src-ag/ExecutionPlanCommon.ag" #-}
   rule205 = \ ((_nontsIntDeps) :: Map NontermIdent (Set NontermIdent)) ->
                            {-# LINE 40 "src-ag/ExecutionPlanCommon.ag" #-}
                            closeMap _nontsIntDeps
                            {-# LINE 2256 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule206 #-}
   {-# LINE 41 "src-ag/ExecutionPlanCommon.ag" #-}
   rule206 = \ ((_nontsIntHoDeps) :: Map NontermIdent (Set NontermIdent)) ->
                            {-# LINE 41 "src-ag/ExecutionPlanCommon.ag" #-}
                            closeMap _nontsIntHoDeps
                            {-# LINE 2262 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule207 #-}
   {-# LINE 42 "src-ag/ExecutionPlanCommon.ag" #-}
   rule207 = \ _closedHoNtDeps ->
                            {-# LINE 42 "src-ag/ExecutionPlanCommon.ag" #-}
                            revDeps _closedHoNtDeps
                            {-# LINE 2268 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule208 #-}
   {-# LINE 51 "src-ag/ExecutionPlanCommon.ag" #-}
   rule208 = \ contextMap_ ->
                          {-# LINE 51 "src-ag/ExecutionPlanCommon.ag" #-}
                          contextMap_
                          {-# LINE 2274 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule209 #-}
   {-# LINE 92 "src-ag/ExecutionPlanCommon.ag" #-}
   rule209 = \ aroundsMap_ ->
                      {-# LINE 92 "src-ag/ExecutionPlanCommon.ag" #-}
                      aroundsMap_
                      {-# LINE 2280 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule210 #-}
   {-# LINE 117 "src-ag/ExecutionPlanCommon.ag" #-}
   rule210 = \ mergeMap_ ->
                     {-# LINE 117 "src-ag/ExecutionPlanCommon.ag" #-}
                     mergeMap_
                     {-# LINE 2286 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule211 #-}
   {-# LINE 9 "src-ag/ExecutionPlanPre.ag" #-}
   rule211 = \  (_ :: ()) ->
                                  {-# LINE 9 "src-ag/ExecutionPlanPre.ag" #-}
                                  0
                                  {-# LINE 2292 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule212 #-}
   {-# LINE 38 "src-ag/LOAG/Prepare.ag" #-}
   rule212 = \ ((_nontsIpmp) :: PMP) ->
                 {-# LINE 38 "src-ag/LOAG/Prepare.ag" #-}
                 if Map.null _nontsIpmp then 1 else fst $ Map.findMin _nontsIpmp
                 {-# LINE 2298 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule213 #-}
   {-# LINE 40 "src-ag/LOAG/Prepare.ag" #-}
   rule213 = \ _ain _an _asn _initO _nmp _nmpr ((_nontsIap) :: A_P) ((_nontsIfieldMap) :: FMap) ((_nontsIfsInP) :: FsInP) ((_nontsIfty) :: FTY) ((_nontsIgen) :: Map Int Int) ((_nontsIinss) :: Map Int [Int]) ((_nontsIofld) :: [(Int, Int)]) ((_nontsIpmp) :: PMP) ((_nontsIpmpr) :: PMP_R) ((_nontsIps) :: [PLabel]) _sfp ->
          {-# LINE 40 "src-ag/LOAG/Prepare.ag" #-}
          LOAGRep _nontsIps _nontsIap _an
             _ain     _asn     _sfp
             _nontsIpmp _nontsIpmpr _nmp     _nmpr
             (A.array (_initO    , _initO     + Map.size _nontsIgen)  $
                  Map.toList $ _nontsIgen)
             (A.array (1,Map.size _nontsIinss) $
                  Map.toList $ _nontsIinss)
             (A.array (_initO    , _initO     + length _nontsIofld) $
                  _nontsIofld) _nontsIfty _nontsIfieldMap _nontsIfsInP
          {-# LINE 2312 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule214 #-}
   {-# LINE 49 "src-ag/LOAG/Prepare.ag" #-}
   rule214 = \ _atts ->
                 {-# LINE 49 "src-ag/LOAG/Prepare.ag" #-}
                 Map.fromList $ zip [1..] _atts
                 {-# LINE 2318 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule215 #-}
   {-# LINE 50 "src-ag/LOAG/Prepare.ag" #-}
   rule215 = \ _atts ->
                 {-# LINE 50 "src-ag/LOAG/Prepare.ag" #-}
                 Map.fromList $ zip _atts     [1..]
                 {-# LINE 2324 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule216 #-}
   {-# LINE 51 "src-ag/LOAG/Prepare.ag" #-}
   rule216 = \ _ain _asn ->
                 {-# LINE 51 "src-ag/LOAG/Prepare.ag" #-}
                 Map.unionWith (++) _ain     _asn
                 {-# LINE 2330 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule217 #-}
   {-# LINE 52 "src-ag/LOAG/Prepare.ag" #-}
   rule217 = \ ((_nontsIinhs) :: AI_N) ->
                 {-# LINE 52 "src-ag/LOAG/Prepare.ag" #-}
                 _nontsIinhs
                 {-# LINE 2336 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule218 #-}
   {-# LINE 53 "src-ag/LOAG/Prepare.ag" #-}
   rule218 = \ ((_nontsIsyns) :: AS_N) ->
                 {-# LINE 53 "src-ag/LOAG/Prepare.ag" #-}
                 _nontsIsyns
                 {-# LINE 2342 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule219 #-}
   {-# LINE 54 "src-ag/LOAG/Prepare.ag" #-}
   rule219 = \ _an ->
                 {-# LINE 54 "src-ag/LOAG/Prepare.ag" #-}
                 concat $ Map.elems _an
                 {-# LINE 2348 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule220 #-}
   {-# LINE 55 "src-ag/LOAG/Prepare.ag" #-}
   rule220 = \ ((_nontsIap) :: A_P) ->
                 {-# LINE 55 "src-ag/LOAG/Prepare.ag" #-}
                 concat $ Map.elems _nontsIap
                 {-# LINE 2354 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule221 #-}
   {-# LINE 56 "src-ag/LOAG/Prepare.ag" #-}
   rule221 = \ manualAttrOrderMap_ ->
                   {-# LINE 56 "src-ag/LOAG/Prepare.ag" #-}
                   manualAttrOrderMap_
                   {-# LINE 2360 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule222 #-}
   {-# LINE 87 "src-ag/LOAG/Prepare.ag" #-}
   rule222 = \ _ain ->
                   {-# LINE 87 "src-ag/LOAG/Prepare.ag" #-}
                   map2F _ain
                   {-# LINE 2366 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule223 #-}
   {-# LINE 88 "src-ag/LOAG/Prepare.ag" #-}
   rule223 = \ _asn ->
                   {-# LINE 88 "src-ag/LOAG/Prepare.ag" #-}
                   map2F _asn
                   {-# LINE 2372 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule224 #-}
   {-# LINE 89 "src-ag/LOAG/Prepare.ag" #-}
   rule224 = \ ((_nontsIpmp) :: PMP) ->
                    {-# LINE 89 "src-ag/LOAG/Prepare.ag" #-}
                    _nontsIpmp
                    {-# LINE 2378 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule225 #-}
   {-# LINE 90 "src-ag/LOAG/Prepare.ag" #-}
   rule225 = \ ((_nontsIpmpr) :: PMP_R) ->
                    {-# LINE 90 "src-ag/LOAG/Prepare.ag" #-}
                    _nontsIpmpr
                    {-# LINE 2384 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule226 #-}
   {-# LINE 91 "src-ag/LOAG/Prepare.ag" #-}
   rule226 = \ ((_nontsIlfp) :: SF_P) ->
                    {-# LINE 91 "src-ag/LOAG/Prepare.ag" #-}
                    _nontsIlfp
                    {-# LINE 2390 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule227 #-}
   {-# LINE 92 "src-ag/LOAG/Prepare.ag" #-}
   rule227 = \ ((_nontsIhoMap) :: HOMap) ->
                    {-# LINE 92 "src-ag/LOAG/Prepare.ag" #-}
                    _nontsIhoMap
                    {-# LINE 2396 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule228 #-}
   {-# LINE 93 "src-ag/LOAG/Prepare.ag" #-}
   rule228 = \ ((_nontsIfty) :: FTY) ->
                    {-# LINE 93 "src-ag/LOAG/Prepare.ag" #-}
                    _nontsIfty
                    {-# LINE 2402 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule229 #-}
   {-# LINE 94 "src-ag/LOAG/Prepare.ag" #-}
   rule229 = \ ((_nontsIfty) :: FTY) ->
                    {-# LINE 94 "src-ag/LOAG/Prepare.ag" #-}
                    _nontsIfty
                    {-# LINE 2408 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule230 #-}
   {-# LINE 103 "src-ag/LOAG/Prepare.ag" #-}
   rule230 = \ ((_nontsIps) :: [PLabel]) ->
                {-# LINE 103 "src-ag/LOAG/Prepare.ag" #-}
                _nontsIps
                {-# LINE 2414 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule231 #-}
   {-# LINE 150 "src-ag/LOAG/Prepare.ag" #-}
   rule231 = \ _an ->
                 {-# LINE 150 "src-ag/LOAG/Prepare.ag" #-}
                 map2F _an
                 {-# LINE 2420 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule232 #-}
   {-# LINE 151 "src-ag/LOAG/Prepare.ag" #-}
   rule232 = \ _nmpr ->
                   {-# LINE 151 "src-ag/LOAG/Prepare.ag" #-}
                   _nmpr
                   {-# LINE 2426 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule233 #-}
   {-# LINE 152 "src-ag/LOAG/Prepare.ag" #-}
   rule233 = \ _nmp ->
                   {-# LINE 152 "src-ag/LOAG/Prepare.ag" #-}
                   if Map.null _nmp     then 0 else (fst $ Map.findMax _nmp    )
                   {-# LINE 2432 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule234 #-}
   {-# LINE 153 "src-ag/LOAG/Prepare.ag" #-}
   rule234 = \  (_ :: ()) ->
                   {-# LINE 153 "src-ag/LOAG/Prepare.ag" #-}
                   0
                   {-# LINE 2438 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule235 #-}
   {-# LINE 207 "src-ag/LOAG/Prepare.ag" #-}
   rule235 = \ ((_nontsIlfp) :: SF_P) ((_nontsIsfp) :: SF_P) ->
                 {-# LINE 207 "src-ag/LOAG/Prepare.ag" #-}
                 repLocRefs _nontsIlfp _nontsIsfp
                 {-# LINE 2444 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule236 #-}
   {-# LINE 54 "src-ag/LOAG/Order.ag" #-}
   rule236 = \ _schedRes ->
                   {-# LINE 54 "src-ag/LOAG/Order.ag" #-}
                   either Seq.singleton (const Seq.empty) _schedRes
                   {-# LINE 2450 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule237 #-}
   {-# LINE 55 "src-ag/LOAG/Order.ag" #-}
   rule237 = \ ((_lhsIoptions) :: Options) ((_nontsIpmp) :: PMP) _schedRes ->
                   {-# LINE 55 "src-ag/LOAG/Order.ag" #-}
                   case either (const []) trd' _schedRes     of
                      []  -> Nothing
                      ads -> Just $ ppAds _lhsIoptions _nontsIpmp ads
                   {-# LINE 2458 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule238 #-}
   {-# LINE 58 "src-ag/LOAG/Order.ag" #-}
   rule238 = \ ((_nontsIenonts) :: ENonterminals) derivings_ typeSyns_ wrappers_ ->
                   {-# LINE 58 "src-ag/LOAG/Order.ag" #-}
                   ExecutionPlan _nontsIenonts typeSyns_ wrappers_ derivings_
                   {-# LINE 2464 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule239 #-}
   {-# LINE 60 "src-ag/LOAG/Order.ag" #-}
   rule239 = \ _schedRes ->
                      {-# LINE 60 "src-ag/LOAG/Order.ag" #-}
                      either (const Map.empty) snd' _schedRes
                      {-# LINE 2470 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule240 #-}
   {-# LINE 61 "src-ag/LOAG/Order.ag" #-}
   rule240 = \ _schedRes ->
                      {-# LINE 61 "src-ag/LOAG/Order.ag" #-}
                      either (error "no tdp") (fromJust.fst') _schedRes
                      {-# LINE 2476 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule241 #-}
   {-# LINE 63 "src-ag/LOAG/Order.ag" #-}
   rule241 = \ _ag ((_lhsIoptions) :: Options) _loagRes ((_nontsIads) :: [Edge]) _self ((_smfIself) :: LOAGRep) ->
                       {-# LINE 63 "src-ag/LOAG/Order.ag" #-}
                       if CT.loag _lhsIoptions
                          then if CT.aoag _lhsIoptions
                                  then AOAG.schedule _smfIself _self _ag     _nontsIads
                                  else _loagRes
                          else Right (Nothing,Map.empty,[])
                       {-# LINE 2486 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule242 #-}
   {-# LINE 68 "src-ag/LOAG/Order.ag" #-}
   rule242 = \ _ag ((_lhsIoptions) :: Options) ->
                    {-# LINE 68 "src-ag/LOAG/Order.ag" #-}
                    let putStrLn s = when (verbose _lhsIoptions) (IO.putStrLn s)
                    in  Right $ unsafePerformIO $ scheduleLOAG _ag     putStrLn _lhsIoptions
                    {-# LINE 2493 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule243 #-}
   {-# LINE 70 "src-ag/LOAG/Order.ag" #-}
   rule243 = \ _self ((_smfIself) :: LOAGRep) ->
               {-# LINE 70 "src-ag/LOAG/Order.ag" #-}
               repToAg _smfIself _self
               {-# LINE 2499 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule244 #-}
   {-# LINE 72 "src-ag/LOAG/Order.ag" #-}
   rule244 = \ _schedRes ->
                      {-# LINE 72 "src-ag/LOAG/Order.ag" #-}
                      either (const []) trd' _schedRes
                      {-# LINE 2505 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule245 #-}
   {-# LINE 133 "src-ag/LOAG/Order.ag" #-}
   rule245 = \ ((_nontsIvisMap) :: IMap.IntMap Int) ->
                              {-# LINE 133 "src-ag/LOAG/Order.ag" #-}
                              _nontsIvisMap
                              {-# LINE 2511 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule246 #-}
   {-# LINE 134 "src-ag/LOAG/Order.ag" #-}
   rule246 = \  (_ :: ()) ->
                               {-# LINE 134 "src-ag/LOAG/Order.ag" #-}
                               0
                               {-# LINE 2517 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule247 #-}
   rule247 = \ ((_nontsIinhmap) :: Map.Map NontermIdent Attributes) ->
     _nontsIinhmap
   {-# INLINE rule248 #-}
   rule248 = \ ((_nontsIlocalSigMap) :: Map.Map NontermIdent (Map.Map ConstructorIdent (Map.Map Identifier Type))) ->
     _nontsIlocalSigMap
   {-# INLINE rule249 #-}
   rule249 = \ ((_nontsIsynmap) :: Map.Map NontermIdent Attributes) ->
     _nontsIsynmap
   {-# INLINE rule250 #-}
   rule250 = \ ((_nontsIself) :: Nonterminals) aroundsMap_ augmentsMap_ contextMap_ derivings_ manualAttrOrderMap_ mergeMap_ paramMap_ pragmas_ quantMap_ typeSyns_ uniqueMap_ useMap_ wrappers_ ->
     Grammar typeSyns_ useMap_ derivings_ wrappers_ _nontsIself pragmas_ manualAttrOrderMap_ paramMap_ contextMap_ quantMap_ uniqueMap_ augmentsMap_ aroundsMap_ mergeMap_
   {-# INLINE rule251 #-}
   rule251 = \ _self ->
     _self
   {-# INLINE rule252 #-}
   rule252 = \ _closedHoNtDeps ->
     _closedHoNtDeps
   {-# INLINE rule253 #-}
   rule253 = \ _closedHoNtRevDeps ->
     _closedHoNtRevDeps
   {-# INLINE rule254 #-}
   rule254 = \ _closedNtDeps ->
     _closedNtDeps
   {-# INLINE rule255 #-}
   rule255 = \ _nmp ->
     _nmp
   {-# INLINE rule256 #-}
   rule256 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions

-- HsToken -----------------------------------------------------
-- wrapper
data Inh_HsToken  = Inh_HsToken { pll_Inh_HsToken :: (PLabel), pts_Inh_HsToken :: (Set.Set (FLabel)) }
data Syn_HsToken  = Syn_HsToken { self_Syn_HsToken :: (HsToken), used_Syn_HsToken :: (Set.Set MyOccurrence) }
{-# INLINABLE wrap_HsToken #-}
wrap_HsToken :: T_HsToken  -> Inh_HsToken  -> (Syn_HsToken )
wrap_HsToken (T_HsToken act) (Inh_HsToken _lhsIpll _lhsIpts) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg52 = T_HsToken_vIn52 _lhsIpll _lhsIpts
        (T_HsToken_vOut52 _lhsOself _lhsOused) <- return (inv_HsToken_s53 sem arg52)
        return (Syn_HsToken _lhsOself _lhsOused)
   )

-- cata
{-# NOINLINE sem_HsToken #-}
sem_HsToken :: HsToken  -> T_HsToken 
sem_HsToken ( AGLocal var_ pos_ rdesc_ ) = sem_HsToken_AGLocal var_ pos_ rdesc_
sem_HsToken ( AGField field_ attr_ pos_ rdesc_ ) = sem_HsToken_AGField field_ attr_ pos_ rdesc_
sem_HsToken ( HsToken value_ pos_ ) = sem_HsToken_HsToken value_ pos_
sem_HsToken ( CharToken value_ pos_ ) = sem_HsToken_CharToken value_ pos_
sem_HsToken ( StrToken value_ pos_ ) = sem_HsToken_StrToken value_ pos_
sem_HsToken ( Err mesg_ pos_ ) = sem_HsToken_Err mesg_ pos_

-- semantic domain
newtype T_HsToken  = T_HsToken {
                               attach_T_HsToken :: Identity (T_HsToken_s53 )
                               }
newtype T_HsToken_s53  = C_HsToken_s53 {
                                       inv_HsToken_s53 :: (T_HsToken_v52 )
                                       }
data T_HsToken_s54  = C_HsToken_s54
type T_HsToken_v52  = (T_HsToken_vIn52 ) -> (T_HsToken_vOut52 )
data T_HsToken_vIn52  = T_HsToken_vIn52 (PLabel) (Set.Set (FLabel))
data T_HsToken_vOut52  = T_HsToken_vOut52 (HsToken) (Set.Set MyOccurrence)
{-# NOINLINE sem_HsToken_AGLocal #-}
sem_HsToken_AGLocal :: (Identifier) -> (Pos) -> (Maybe String) -> T_HsToken 
sem_HsToken_AGLocal arg_var_ arg_pos_ arg_rdesc_ = T_HsToken (return st53) where
   {-# NOINLINE st53 #-}
   st53 = let
      v52 :: T_HsToken_v52 
      v52 = \ (T_HsToken_vIn52 _lhsIpll _lhsIpts) -> ( let
         _lhsOused :: Set.Set MyOccurrence
         _lhsOused = rule257 _lhsIpll _lhsIpts arg_var_
         _self = rule258 arg_pos_ arg_rdesc_ arg_var_
         _lhsOself :: HsToken
         _lhsOself = rule259 _self
         __result_ = T_HsToken_vOut52 _lhsOself _lhsOused
         in __result_ )
     in C_HsToken_s53 v52
   {-# INLINE rule257 #-}
   {-# LINE 281 "src-ag/LOAG/Prepare.ag" #-}
   rule257 = \ ((_lhsIpll) :: PLabel) ((_lhsIpts) :: Set.Set (FLabel)) var_ ->
          {-# LINE 281 "src-ag/LOAG/Prepare.ag" #-}
          case getName var_ `Set.member` _lhsIpts of
            True  -> Set.empty
            False -> Set.singleton $ (_lhsIpll, getName _LOC) >.<
                          (getName var_, drhs _LOC)
          {-# LINE 2607 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule258 #-}
   rule258 = \ pos_ rdesc_ var_ ->
     AGLocal var_ pos_ rdesc_
   {-# INLINE rule259 #-}
   rule259 = \ _self ->
     _self
{-# NOINLINE sem_HsToken_AGField #-}
sem_HsToken_AGField :: (Identifier) -> (Identifier) -> (Pos) -> (Maybe String) -> T_HsToken 
sem_HsToken_AGField arg_field_ arg_attr_ arg_pos_ arg_rdesc_ = T_HsToken (return st53) where
   {-# NOINLINE st53 #-}
   st53 = let
      v52 :: T_HsToken_v52 
      v52 = \ (T_HsToken_vIn52 _lhsIpll _lhsIpts) -> ( let
         _lhsOused :: Set.Set MyOccurrence
         _lhsOused = rule260 _lhsIpll arg_attr_ arg_field_
         _self = rule261 arg_attr_ arg_field_ arg_pos_ arg_rdesc_
         _lhsOself :: HsToken
         _lhsOself = rule262 _self
         __result_ = T_HsToken_vOut52 _lhsOself _lhsOused
         in __result_ )
     in C_HsToken_s53 v52
   {-# INLINE rule260 #-}
   {-# LINE 289 "src-ag/LOAG/Prepare.ag" #-}
   rule260 = \ ((_lhsIpll) :: PLabel) attr_ field_ ->
                 {-# LINE 289 "src-ag/LOAG/Prepare.ag" #-}
                 Set.singleton $ (_lhsIpll, getName field_) >.<
                                  (getName attr_, drhs field_)
                 {-# LINE 2635 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule261 #-}
   rule261 = \ attr_ field_ pos_ rdesc_ ->
     AGField field_ attr_ pos_ rdesc_
   {-# INLINE rule262 #-}
   rule262 = \ _self ->
     _self
{-# NOINLINE sem_HsToken_HsToken #-}
sem_HsToken_HsToken :: (String) -> (Pos) -> T_HsToken 
sem_HsToken_HsToken arg_value_ arg_pos_ = T_HsToken (return st53) where
   {-# NOINLINE st53 #-}
   st53 = let
      v52 :: T_HsToken_v52 
      v52 = \ (T_HsToken_vIn52 _lhsIpll _lhsIpts) -> ( let
         _lhsOused :: Set.Set MyOccurrence
         _lhsOused = rule263  ()
         _self = rule264 arg_pos_ arg_value_
         _lhsOself :: HsToken
         _lhsOself = rule265 _self
         __result_ = T_HsToken_vOut52 _lhsOself _lhsOused
         in __result_ )
     in C_HsToken_s53 v52
   {-# INLINE rule263 #-}
   rule263 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule264 #-}
   rule264 = \ pos_ value_ ->
     HsToken value_ pos_
   {-# INLINE rule265 #-}
   rule265 = \ _self ->
     _self
{-# NOINLINE sem_HsToken_CharToken #-}
sem_HsToken_CharToken :: (String) -> (Pos) -> T_HsToken 
sem_HsToken_CharToken arg_value_ arg_pos_ = T_HsToken (return st53) where
   {-# NOINLINE st53 #-}
   st53 = let
      v52 :: T_HsToken_v52 
      v52 = \ (T_HsToken_vIn52 _lhsIpll _lhsIpts) -> ( let
         _lhsOused :: Set.Set MyOccurrence
         _lhsOused = rule266  ()
         _self = rule267 arg_pos_ arg_value_
         _lhsOself :: HsToken
         _lhsOself = rule268 _self
         __result_ = T_HsToken_vOut52 _lhsOself _lhsOused
         in __result_ )
     in C_HsToken_s53 v52
   {-# INLINE rule266 #-}
   rule266 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule267 #-}
   rule267 = \ pos_ value_ ->
     CharToken value_ pos_
   {-# INLINE rule268 #-}
   rule268 = \ _self ->
     _self
{-# NOINLINE sem_HsToken_StrToken #-}
sem_HsToken_StrToken :: (String) -> (Pos) -> T_HsToken 
sem_HsToken_StrToken arg_value_ arg_pos_ = T_HsToken (return st53) where
   {-# NOINLINE st53 #-}
   st53 = let
      v52 :: T_HsToken_v52 
      v52 = \ (T_HsToken_vIn52 _lhsIpll _lhsIpts) -> ( let
         _lhsOused :: Set.Set MyOccurrence
         _lhsOused = rule269  ()
         _self = rule270 arg_pos_ arg_value_
         _lhsOself :: HsToken
         _lhsOself = rule271 _self
         __result_ = T_HsToken_vOut52 _lhsOself _lhsOused
         in __result_ )
     in C_HsToken_s53 v52
   {-# INLINE rule269 #-}
   rule269 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule270 #-}
   rule270 = \ pos_ value_ ->
     StrToken value_ pos_
   {-# INLINE rule271 #-}
   rule271 = \ _self ->
     _self
{-# NOINLINE sem_HsToken_Err #-}
sem_HsToken_Err :: (String) -> (Pos) -> T_HsToken 
sem_HsToken_Err arg_mesg_ arg_pos_ = T_HsToken (return st53) where
   {-# NOINLINE st53 #-}
   st53 = let
      v52 :: T_HsToken_v52 
      v52 = \ (T_HsToken_vIn52 _lhsIpll _lhsIpts) -> ( let
         _lhsOused :: Set.Set MyOccurrence
         _lhsOused = rule272  ()
         _self = rule273 arg_mesg_ arg_pos_
         _lhsOself :: HsToken
         _lhsOself = rule274 _self
         __result_ = T_HsToken_vOut52 _lhsOself _lhsOused
         in __result_ )
     in C_HsToken_s53 v52
   {-# INLINE rule272 #-}
   rule272 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule273 #-}
   rule273 = \ mesg_ pos_ ->
     Err mesg_ pos_
   {-# INLINE rule274 #-}
   rule274 = \ _self ->
     _self

-- HsTokens ----------------------------------------------------
-- wrapper
data Inh_HsTokens  = Inh_HsTokens { pll_Inh_HsTokens :: (PLabel), pts_Inh_HsTokens :: (Set.Set (FLabel)) }
data Syn_HsTokens  = Syn_HsTokens { self_Syn_HsTokens :: (HsTokens), used_Syn_HsTokens :: (Set.Set MyOccurrence) }
{-# INLINABLE wrap_HsTokens #-}
wrap_HsTokens :: T_HsTokens  -> Inh_HsTokens  -> (Syn_HsTokens )
wrap_HsTokens (T_HsTokens act) (Inh_HsTokens _lhsIpll _lhsIpts) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg55 = T_HsTokens_vIn55 _lhsIpll _lhsIpts
        (T_HsTokens_vOut55 _lhsOself _lhsOused) <- return (inv_HsTokens_s56 sem arg55)
        return (Syn_HsTokens _lhsOself _lhsOused)
   )

-- cata
{-# NOINLINE sem_HsTokens #-}
sem_HsTokens :: HsTokens  -> T_HsTokens 
sem_HsTokens list = Prelude.foldr sem_HsTokens_Cons sem_HsTokens_Nil (Prelude.map sem_HsToken list)

-- semantic domain
newtype T_HsTokens  = T_HsTokens {
                                 attach_T_HsTokens :: Identity (T_HsTokens_s56 )
                                 }
newtype T_HsTokens_s56  = C_HsTokens_s56 {
                                         inv_HsTokens_s56 :: (T_HsTokens_v55 )
                                         }
data T_HsTokens_s57  = C_HsTokens_s57
type T_HsTokens_v55  = (T_HsTokens_vIn55 ) -> (T_HsTokens_vOut55 )
data T_HsTokens_vIn55  = T_HsTokens_vIn55 (PLabel) (Set.Set (FLabel))
data T_HsTokens_vOut55  = T_HsTokens_vOut55 (HsTokens) (Set.Set MyOccurrence)
{-# NOINLINE sem_HsTokens_Cons #-}
sem_HsTokens_Cons :: T_HsToken  -> T_HsTokens  -> T_HsTokens 
sem_HsTokens_Cons arg_hd_ arg_tl_ = T_HsTokens (return st56) where
   {-# NOINLINE st56 #-}
   st56 = let
      v55 :: T_HsTokens_v55 
      v55 = \ (T_HsTokens_vIn55 _lhsIpll _lhsIpts) -> ( let
         _hdX53 = Control.Monad.Identity.runIdentity (attach_T_HsToken (arg_hd_))
         _tlX56 = Control.Monad.Identity.runIdentity (attach_T_HsTokens (arg_tl_))
         (T_HsToken_vOut52 _hdIself _hdIused) = inv_HsToken_s53 _hdX53 (T_HsToken_vIn52 _hdOpll _hdOpts)
         (T_HsTokens_vOut55 _tlIself _tlIused) = inv_HsTokens_s56 _tlX56 (T_HsTokens_vIn55 _tlOpll _tlOpts)
         _lhsOused :: Set.Set MyOccurrence
         _lhsOused = rule275 _hdIused _tlIused
         _self = rule276 _hdIself _tlIself
         _lhsOself :: HsTokens
         _lhsOself = rule277 _self
         _hdOpll = rule278 _lhsIpll
         _hdOpts = rule279 _lhsIpts
         _tlOpll = rule280 _lhsIpll
         _tlOpts = rule281 _lhsIpts
         __result_ = T_HsTokens_vOut55 _lhsOself _lhsOused
         in __result_ )
     in C_HsTokens_s56 v55
   {-# INLINE rule275 #-}
   rule275 = \ ((_hdIused) :: Set.Set MyOccurrence) ((_tlIused) :: Set.Set MyOccurrence) ->
     (Set.union _hdIused _tlIused)
   {-# INLINE rule276 #-}
   rule276 = \ ((_hdIself) :: HsToken) ((_tlIself) :: HsTokens) ->
     (:) _hdIself _tlIself
   {-# INLINE rule277 #-}
   rule277 = \ _self ->
     _self
   {-# INLINE rule278 #-}
   rule278 = \ ((_lhsIpll) :: PLabel) ->
     _lhsIpll
   {-# INLINE rule279 #-}
   rule279 = \ ((_lhsIpts) :: Set.Set (FLabel)) ->
     _lhsIpts
   {-# INLINE rule280 #-}
   rule280 = \ ((_lhsIpll) :: PLabel) ->
     _lhsIpll
   {-# INLINE rule281 #-}
   rule281 = \ ((_lhsIpts) :: Set.Set (FLabel)) ->
     _lhsIpts
{-# NOINLINE sem_HsTokens_Nil #-}
sem_HsTokens_Nil ::  T_HsTokens 
sem_HsTokens_Nil  = T_HsTokens (return st56) where
   {-# NOINLINE st56 #-}
   st56 = let
      v55 :: T_HsTokens_v55 
      v55 = \ (T_HsTokens_vIn55 _lhsIpll _lhsIpts) -> ( let
         _lhsOused :: Set.Set MyOccurrence
         _lhsOused = rule282  ()
         _self = rule283  ()
         _lhsOself :: HsTokens
         _lhsOself = rule284 _self
         __result_ = T_HsTokens_vOut55 _lhsOself _lhsOused
         in __result_ )
     in C_HsTokens_s56 v55
   {-# INLINE rule282 #-}
   rule282 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule283 #-}
   rule283 = \  (_ :: ()) ->
     []
   {-# INLINE rule284 #-}
   rule284 = \ _self ->
     _self

-- HsTokensRoot ------------------------------------------------
-- wrapper
data Inh_HsTokensRoot  = Inh_HsTokensRoot { pll_Inh_HsTokensRoot :: (PLabel), pts_Inh_HsTokensRoot :: (Set.Set (FLabel)) }
data Syn_HsTokensRoot  = Syn_HsTokensRoot { self_Syn_HsTokensRoot :: (HsTokensRoot), used_Syn_HsTokensRoot :: (Set.Set MyOccurrence) }
{-# INLINABLE wrap_HsTokensRoot #-}
wrap_HsTokensRoot :: T_HsTokensRoot  -> Inh_HsTokensRoot  -> (Syn_HsTokensRoot )
wrap_HsTokensRoot (T_HsTokensRoot act) (Inh_HsTokensRoot _lhsIpll _lhsIpts) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg58 = T_HsTokensRoot_vIn58 _lhsIpll _lhsIpts
        (T_HsTokensRoot_vOut58 _lhsOself _lhsOused) <- return (inv_HsTokensRoot_s59 sem arg58)
        return (Syn_HsTokensRoot _lhsOself _lhsOused)
   )

-- cata
{-# INLINE sem_HsTokensRoot #-}
sem_HsTokensRoot :: HsTokensRoot  -> T_HsTokensRoot 
sem_HsTokensRoot ( HsTokensRoot tokens_ ) = sem_HsTokensRoot_HsTokensRoot ( sem_HsTokens tokens_ )

-- semantic domain
newtype T_HsTokensRoot  = T_HsTokensRoot {
                                         attach_T_HsTokensRoot :: Identity (T_HsTokensRoot_s59 )
                                         }
newtype T_HsTokensRoot_s59  = C_HsTokensRoot_s59 {
                                                 inv_HsTokensRoot_s59 :: (T_HsTokensRoot_v58 )
                                                 }
data T_HsTokensRoot_s60  = C_HsTokensRoot_s60
type T_HsTokensRoot_v58  = (T_HsTokensRoot_vIn58 ) -> (T_HsTokensRoot_vOut58 )
data T_HsTokensRoot_vIn58  = T_HsTokensRoot_vIn58 (PLabel) (Set.Set (FLabel))
data T_HsTokensRoot_vOut58  = T_HsTokensRoot_vOut58 (HsTokensRoot) (Set.Set MyOccurrence)
{-# NOINLINE sem_HsTokensRoot_HsTokensRoot #-}
sem_HsTokensRoot_HsTokensRoot :: T_HsTokens  -> T_HsTokensRoot 
sem_HsTokensRoot_HsTokensRoot arg_tokens_ = T_HsTokensRoot (return st59) where
   {-# NOINLINE st59 #-}
   st59 = let
      v58 :: T_HsTokensRoot_v58 
      v58 = \ (T_HsTokensRoot_vIn58 _lhsIpll _lhsIpts) -> ( let
         _tokensX56 = Control.Monad.Identity.runIdentity (attach_T_HsTokens (arg_tokens_))
         (T_HsTokens_vOut55 _tokensIself _tokensIused) = inv_HsTokens_s56 _tokensX56 (T_HsTokens_vIn55 _tokensOpll _tokensOpts)
         _lhsOused :: Set.Set MyOccurrence
         _lhsOused = rule285 _tokensIused
         _self = rule286 _tokensIself
         _lhsOself :: HsTokensRoot
         _lhsOself = rule287 _self
         _tokensOpll = rule288 _lhsIpll
         _tokensOpts = rule289 _lhsIpts
         __result_ = T_HsTokensRoot_vOut58 _lhsOself _lhsOused
         in __result_ )
     in C_HsTokensRoot_s59 v58
   {-# INLINE rule285 #-}
   rule285 = \ ((_tokensIused) :: Set.Set MyOccurrence) ->
     _tokensIused
   {-# INLINE rule286 #-}
   rule286 = \ ((_tokensIself) :: HsTokens) ->
     HsTokensRoot _tokensIself
   {-# INLINE rule287 #-}
   rule287 = \ _self ->
     _self
   {-# INLINE rule288 #-}
   rule288 = \ ((_lhsIpll) :: PLabel) ->
     _lhsIpll
   {-# INLINE rule289 #-}
   rule289 = \ ((_lhsIpts) :: Set.Set (FLabel)) ->
     _lhsIpts

-- LOAGRep -----------------------------------------------------
-- wrapper
data Inh_LOAGRep  = Inh_LOAGRep {  }
data Syn_LOAGRep  = Syn_LOAGRep { self_Syn_LOAGRep :: (LOAGRep) }
{-# INLINABLE wrap_LOAGRep #-}
wrap_LOAGRep :: T_LOAGRep  -> Inh_LOAGRep  -> (Syn_LOAGRep )
wrap_LOAGRep (T_LOAGRep act) (Inh_LOAGRep ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg61 = T_LOAGRep_vIn61 
        (T_LOAGRep_vOut61 _lhsOself) <- return (inv_LOAGRep_s62 sem arg61)
        return (Syn_LOAGRep _lhsOself)
   )

-- cata
{-# INLINE sem_LOAGRep #-}
sem_LOAGRep :: LOAGRep  -> T_LOAGRep 
sem_LOAGRep ( LOAGRep ps_ ap_ an_ ain_ asn_ sfp_ pmp_ pmpr_ nmp_ nmpr_ gen_ inss_ ofld_ fty_ fieldMap_ fsInP_ ) = sem_LOAGRep_LOAGRep ps_ ap_ an_ ain_ asn_ sfp_ pmp_ pmpr_ nmp_ nmpr_ gen_ inss_ ofld_ fty_ fieldMap_ fsInP_

-- semantic domain
newtype T_LOAGRep  = T_LOAGRep {
                               attach_T_LOAGRep :: Identity (T_LOAGRep_s62 )
                               }
newtype T_LOAGRep_s62  = C_LOAGRep_s62 {
                                       inv_LOAGRep_s62 :: (T_LOAGRep_v61 )
                                       }
data T_LOAGRep_s63  = C_LOAGRep_s63
type T_LOAGRep_v61  = (T_LOAGRep_vIn61 ) -> (T_LOAGRep_vOut61 )
data T_LOAGRep_vIn61  = T_LOAGRep_vIn61 
data T_LOAGRep_vOut61  = T_LOAGRep_vOut61 (LOAGRep)
{-# NOINLINE sem_LOAGRep_LOAGRep #-}
sem_LOAGRep_LOAGRep :: ([PLabel]) -> (A_P) -> (A_N) -> (AI_N) -> (AS_N) -> (SF_P) -> (PMP) -> (PMP_R) -> (NMP) -> (NMP_R) -> (A.Array Int Int) -> (A.Array Int [Int]) -> (A.Array Int Int) -> (FTY) -> (FMap) -> (Map.Map PLabel [(PLabel,FLabel)]) -> T_LOAGRep 
sem_LOAGRep_LOAGRep arg_ps_ arg_ap_ arg_an_ arg_ain_ arg_asn_ arg_sfp_ arg_pmp_ arg_pmpr_ arg_nmp_ arg_nmpr_ arg_gen_ arg_inss_ arg_ofld_ arg_fty_ arg_fieldMap_ arg_fsInP_ = T_LOAGRep (return st62) where
   {-# NOINLINE st62 #-}
   st62 = let
      v61 :: T_LOAGRep_v61 
      v61 = \ (T_LOAGRep_vIn61 ) -> ( let
         _self = rule290 arg_ain_ arg_an_ arg_ap_ arg_asn_ arg_fieldMap_ arg_fsInP_ arg_fty_ arg_gen_ arg_inss_ arg_nmp_ arg_nmpr_ arg_ofld_ arg_pmp_ arg_pmpr_ arg_ps_ arg_sfp_
         _lhsOself :: LOAGRep
         _lhsOself = rule291 _self
         __result_ = T_LOAGRep_vOut61 _lhsOself
         in __result_ )
     in C_LOAGRep_s62 v61
   {-# INLINE rule290 #-}
   rule290 = \ ain_ an_ ap_ asn_ fieldMap_ fsInP_ fty_ gen_ inss_ nmp_ nmpr_ ofld_ pmp_ pmpr_ ps_ sfp_ ->
     LOAGRep ps_ ap_ an_ ain_ asn_ sfp_ pmp_ pmpr_ nmp_ nmpr_ gen_ inss_ ofld_ fty_ fieldMap_ fsInP_
   {-# INLINE rule291 #-}
   rule291 = \ _self ->
     _self

-- MySegment ---------------------------------------------------
-- wrapper
data Inh_MySegment  = Inh_MySegment { ain_Inh_MySegment :: (MyType -> MyAttributes), asn_Inh_MySegment :: (MyType -> MyAttributes), done_Inh_MySegment :: ( (Set.Set MyOccurrence, Set.Set FLabel
                                                                                                                                                                           , Set.Set Identifier, Set.Set (FLabel,Int))), fty_Inh_MySegment :: (FTY), hoMapf_Inh_MySegment :: (HOMap), lfpf_Inh_MySegment :: (SF_P), nmp_Inh_MySegment :: (NMP), nmprf_Inh_MySegment :: (NMP_R), options_Inh_MySegment :: (Options), pmpf_Inh_MySegment :: (PMP), pmprf_Inh_MySegment :: (PMP_R), ps_Inh_MySegment :: (PLabel), ruleMap_Inh_MySegment :: (Map.Map MyOccurrence Identifier), tdp_Inh_MySegment :: (TDPRes), visMapf_Inh_MySegment :: (IMap.IntMap Int), visitnum_Inh_MySegment :: (Int) }
data Syn_MySegment  = Syn_MySegment { done_Syn_MySegment :: ( (Set.Set MyOccurrence, Set.Set FLabel
                                                                             ,Set.Set Identifier, Set.Set (FLabel,Int))), evisits_Syn_MySegment :: (Visit), self_Syn_MySegment :: (MySegment), synsO_Syn_MySegment :: ([Int]), visitnum_Syn_MySegment :: (Int), visnr_Syn_MySegment :: (Int) }
{-# INLINABLE wrap_MySegment #-}
wrap_MySegment :: T_MySegment  -> Inh_MySegment  -> (Syn_MySegment )
wrap_MySegment (T_MySegment act) (Inh_MySegment _lhsIain _lhsIasn _lhsIdone _lhsIfty _lhsIhoMapf _lhsIlfpf _lhsInmp _lhsInmprf _lhsIoptions _lhsIpmpf _lhsIpmprf _lhsIps _lhsIruleMap _lhsItdp _lhsIvisMapf _lhsIvisitnum) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg64 = T_MySegment_vIn64 _lhsIain _lhsIasn _lhsIdone _lhsIfty _lhsIhoMapf _lhsIlfpf _lhsInmp _lhsInmprf _lhsIoptions _lhsIpmpf _lhsIpmprf _lhsIps _lhsIruleMap _lhsItdp _lhsIvisMapf _lhsIvisitnum
        (T_MySegment_vOut64 _lhsOdone _lhsOevisits _lhsOself _lhsOsynsO _lhsOvisitnum _lhsOvisnr) <- return (inv_MySegment_s65 sem arg64)
        return (Syn_MySegment _lhsOdone _lhsOevisits _lhsOself _lhsOsynsO _lhsOvisitnum _lhsOvisnr)
   )

-- cata
{-# INLINE sem_MySegment #-}
sem_MySegment :: MySegment  -> T_MySegment 
sem_MySegment ( MySegment visnr_ inhAttr_ synAttr_ inhOccs_ synOccs_ ) = sem_MySegment_MySegment visnr_ inhAttr_ synAttr_ inhOccs_ synOccs_

-- semantic domain
newtype T_MySegment  = T_MySegment {
                                   attach_T_MySegment :: Identity (T_MySegment_s65 )
                                   }
newtype T_MySegment_s65  = C_MySegment_s65 {
                                           inv_MySegment_s65 :: (T_MySegment_v64 )
                                           }
data T_MySegment_s66  = C_MySegment_s66
type T_MySegment_v64  = (T_MySegment_vIn64 ) -> (T_MySegment_vOut64 )
data T_MySegment_vIn64  = T_MySegment_vIn64 (MyType -> MyAttributes) (MyType -> MyAttributes) ( (Set.Set MyOccurrence, Set.Set FLabel
                                                                                                               , Set.Set Identifier, Set.Set (FLabel,Int))) (FTY) (HOMap) (SF_P) (NMP) (NMP_R) (Options) (PMP) (PMP_R) (PLabel) (Map.Map MyOccurrence Identifier) (TDPRes) (IMap.IntMap Int) (Int)
data T_MySegment_vOut64  = T_MySegment_vOut64 ( (Set.Set MyOccurrence, Set.Set FLabel
                                                               ,Set.Set Identifier, Set.Set (FLabel,Int))) (Visit) (MySegment) ([Int]) (Int) (Int)
{-# NOINLINE sem_MySegment_MySegment #-}
sem_MySegment_MySegment :: (Int) -> ([Int]) -> ([Int]) -> (Maybe [Int]) -> (Maybe [Int]) -> T_MySegment 
sem_MySegment_MySegment arg_visnr_ arg_inhAttr_ arg_synAttr_ arg_inhOccs_ arg_synOccs_ = T_MySegment (return st65) where
   {-# NOINLINE st65 #-}
   st65 = let
      v64 :: T_MySegment_v64 
      v64 = \ (T_MySegment_vIn64 _lhsIain _lhsIasn _lhsIdone _lhsIfty _lhsIhoMapf _lhsIlfpf _lhsInmp _lhsInmprf _lhsIoptions _lhsIpmpf _lhsIpmprf _lhsIps _lhsIruleMap _lhsItdp _lhsIvisMapf _lhsIvisitnum) -> ( let
         _lhsOvisitnum :: Int
         _lhsOvisitnum = rule292 _visitnum_augmented_f1 _visitnum_augmented_syn
         _visitnum_augmented_f1 = rule293  ()
         _inhs = rule294 _lhsInmp arg_inhAttr_
         _syns = rule295 _lhsInmp arg_synAttr_
         _inhsO = rule296 arg_inhOccs_
         _synsO = rule297 arg_synOccs_
         _lhsOvisnr :: Int
         _lhsOvisnr = rule298 arg_visnr_
         _kind = rule299 _lhsIoptions
         _lhsOevisits :: Visit
         _lhsOevisits = rule300 _inhs _kind _lhsIvisitnum _steps _syns
         _steps = rule301 _lhsIoptions _vss
         _lhsOdone ::  (Set.Set MyOccurrence, Set.Set FLabel
                                      ,Set.Set Identifier, Set.Set (FLabel,Int))
         (_vss,_lhsOdone) = rule302 _lhsIdone _lhsIfty _lhsIhoMapf _lhsIlfpf _lhsInmprf _lhsIpmpf _lhsIpmprf _lhsIps _lhsIruleMap _lhsItdp _lhsIvisMapf _synsO
         _self = rule303 arg_inhAttr_ arg_inhOccs_ arg_synAttr_ arg_synOccs_ arg_visnr_
         _lhsOself :: MySegment
         _lhsOself = rule304 _self
         _lhsOsynsO :: [Int]
         _lhsOsynsO = rule305 _synsO
         _visitnum_augmented_syn = rule306 _lhsIvisitnum
         __result_ = T_MySegment_vOut64 _lhsOdone _lhsOevisits _lhsOself _lhsOsynsO _lhsOvisitnum _lhsOvisnr
         in __result_ )
     in C_MySegment_s65 v64
   {-# INLINE rule292 #-}
   rule292 = \ _visitnum_augmented_f1 _visitnum_augmented_syn ->
     foldr ($) _visitnum_augmented_syn [_visitnum_augmented_f1]
   {-# INLINE rule293 #-}
   rule293 = \  (_ :: ()) ->
                                        (+1)
   {-# INLINE rule294 #-}
   {-# LINE 225 "src-ag/LOAG/Order.ag" #-}
   rule294 = \ ((_lhsInmp) :: NMP) inhAttr_ ->
                   {-# LINE 225 "src-ag/LOAG/Order.ag" #-}
                   Map.keysSet$ Map.unions $ map (vertexToAttr _lhsInmp) inhAttr_
                   {-# LINE 3030 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule295 #-}
   {-# LINE 226 "src-ag/LOAG/Order.ag" #-}
   rule295 = \ ((_lhsInmp) :: NMP) synAttr_ ->
                   {-# LINE 226 "src-ag/LOAG/Order.ag" #-}
                   Map.keysSet$ Map.unions $ map (vertexToAttr _lhsInmp) synAttr_
                   {-# LINE 3036 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule296 #-}
   {-# LINE 227 "src-ag/LOAG/Order.ag" #-}
   rule296 = \ inhOccs_ ->
                   {-# LINE 227 "src-ag/LOAG/Order.ag" #-}
                   maybe (error "segment not instantiated") id inhOccs_
                   {-# LINE 3042 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule297 #-}
   {-# LINE 228 "src-ag/LOAG/Order.ag" #-}
   rule297 = \ synOccs_ ->
                   {-# LINE 228 "src-ag/LOAG/Order.ag" #-}
                   maybe (error "segment not instantiated") id synOccs_
                   {-# LINE 3048 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule298 #-}
   {-# LINE 229 "src-ag/LOAG/Order.ag" #-}
   rule298 = \ visnr_ ->
                   {-# LINE 229 "src-ag/LOAG/Order.ag" #-}
                   visnr_
                   {-# LINE 3054 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule299 #-}
   {-# LINE 230 "src-ag/LOAG/Order.ag" #-}
   rule299 = \ ((_lhsIoptions) :: Options) ->
                   {-# LINE 230 "src-ag/LOAG/Order.ag" #-}
                   if monadic _lhsIoptions then VisitMonadic else VisitPure True
                   {-# LINE 3060 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule300 #-}
   {-# LINE 231 "src-ag/LOAG/Order.ag" #-}
   rule300 = \ _inhs _kind ((_lhsIvisitnum) :: Int) _steps _syns ->
                      {-# LINE 231 "src-ag/LOAG/Order.ag" #-}
                      Visit _lhsIvisitnum _lhsIvisitnum (_lhsIvisitnum+1)
                            _inhs     _syns     _steps     _kind
                      {-# LINE 3067 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule301 #-}
   {-# LINE 233 "src-ag/LOAG/Order.ag" #-}
   rule301 = \ ((_lhsIoptions) :: Options) _vss ->
                      {-# LINE 233 "src-ag/LOAG/Order.ag" #-}
                      if monadic _lhsIoptions
                          then [Sim _vss    ] else [PureGroup _vss     True]
                      {-# LINE 3074 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule302 #-}
   {-# LINE 235 "src-ag/LOAG/Order.ag" #-}
   rule302 = \ ((_lhsIdone) ::  (Set.Set MyOccurrence, Set.Set FLabel
                                               , Set.Set Identifier, Set.Set (FLabel,Int))) ((_lhsIfty) :: FTY) ((_lhsIhoMapf) :: HOMap) ((_lhsIlfpf) :: SF_P) ((_lhsInmprf) :: NMP_R) ((_lhsIpmpf) :: PMP) ((_lhsIpmprf) :: PMP_R) ((_lhsIps) :: PLabel) ((_lhsIruleMap) :: Map.Map MyOccurrence Identifier) ((_lhsItdp) :: TDPRes) ((_lhsIvisMapf) :: IMap.IntMap Int) _synsO ->
                             {-# LINE 235 "src-ag/LOAG/Order.ag" #-}
                             (runST $ getVss _lhsIdone _lhsIps _lhsItdp _synsO
                              _lhsIlfpf _lhsInmprf _lhsIpmpf _lhsIpmprf _lhsIfty
                              _lhsIvisMapf _lhsIruleMap _lhsIhoMapf)
                             {-# LINE 3083 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule303 #-}
   rule303 = \ inhAttr_ inhOccs_ synAttr_ synOccs_ visnr_ ->
     MySegment visnr_ inhAttr_ synAttr_ inhOccs_ synOccs_
   {-# INLINE rule304 #-}
   rule304 = \ _self ->
     _self
   {-# INLINE rule305 #-}
   rule305 = \ _synsO ->
     _synsO
   {-# INLINE rule306 #-}
   rule306 = \ ((_lhsIvisitnum) :: Int) ->
     _lhsIvisitnum

-- MySegments --------------------------------------------------
-- wrapper
data Inh_MySegments  = Inh_MySegments { ain_Inh_MySegments :: (MyType -> MyAttributes), asn_Inh_MySegments :: (MyType -> MyAttributes), done_Inh_MySegments :: ( (Set.Set MyOccurrence, Set.Set FLabel
                                                                                                                                                                                , Set.Set Identifier, Set.Set (FLabel,Int))), fty_Inh_MySegments :: (FTY), hoMapf_Inh_MySegments :: (HOMap), lfpf_Inh_MySegments :: (SF_P), nmp_Inh_MySegments :: (NMP), nmprf_Inh_MySegments :: (NMP_R), options_Inh_MySegments :: (Options), pmpf_Inh_MySegments :: (PMP), pmprf_Inh_MySegments :: (PMP_R), ps_Inh_MySegments :: (PLabel), ruleMap_Inh_MySegments :: (Map.Map MyOccurrence Identifier), tdp_Inh_MySegments :: (TDPRes), visMapf_Inh_MySegments :: (IMap.IntMap Int), visitnum_Inh_MySegments :: (Int) }
data Syn_MySegments  = Syn_MySegments { evisits_Syn_MySegments :: (Visits), self_Syn_MySegments :: (MySegments), visitnum_Syn_MySegments :: (Int) }
{-# INLINABLE wrap_MySegments #-}
wrap_MySegments :: T_MySegments  -> Inh_MySegments  -> (Syn_MySegments )
wrap_MySegments (T_MySegments act) (Inh_MySegments _lhsIain _lhsIasn _lhsIdone _lhsIfty _lhsIhoMapf _lhsIlfpf _lhsInmp _lhsInmprf _lhsIoptions _lhsIpmpf _lhsIpmprf _lhsIps _lhsIruleMap _lhsItdp _lhsIvisMapf _lhsIvisitnum) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg67 = T_MySegments_vIn67 _lhsIain _lhsIasn _lhsIdone _lhsIfty _lhsIhoMapf _lhsIlfpf _lhsInmp _lhsInmprf _lhsIoptions _lhsIpmpf _lhsIpmprf _lhsIps _lhsIruleMap _lhsItdp _lhsIvisMapf _lhsIvisitnum
        (T_MySegments_vOut67 _lhsOevisits _lhsOself _lhsOvisitnum) <- return (inv_MySegments_s68 sem arg67)
        return (Syn_MySegments _lhsOevisits _lhsOself _lhsOvisitnum)
   )

-- cata
{-# NOINLINE sem_MySegments #-}
sem_MySegments :: MySegments  -> T_MySegments 
sem_MySegments list = Prelude.foldr sem_MySegments_Cons sem_MySegments_Nil (Prelude.map sem_MySegment list)

-- semantic domain
newtype T_MySegments  = T_MySegments {
                                     attach_T_MySegments :: Identity (T_MySegments_s68 )
                                     }
newtype T_MySegments_s68  = C_MySegments_s68 {
                                             inv_MySegments_s68 :: (T_MySegments_v67 )
                                             }
data T_MySegments_s69  = C_MySegments_s69
type T_MySegments_v67  = (T_MySegments_vIn67 ) -> (T_MySegments_vOut67 )
data T_MySegments_vIn67  = T_MySegments_vIn67 (MyType -> MyAttributes) (MyType -> MyAttributes) ( (Set.Set MyOccurrence, Set.Set FLabel
                                                                                                                 , Set.Set Identifier, Set.Set (FLabel,Int))) (FTY) (HOMap) (SF_P) (NMP) (NMP_R) (Options) (PMP) (PMP_R) (PLabel) (Map.Map MyOccurrence Identifier) (TDPRes) (IMap.IntMap Int) (Int)
data T_MySegments_vOut67  = T_MySegments_vOut67 (Visits) (MySegments) (Int)
{-# NOINLINE sem_MySegments_Cons #-}
sem_MySegments_Cons :: T_MySegment  -> T_MySegments  -> T_MySegments 
sem_MySegments_Cons arg_hd_ arg_tl_ = T_MySegments (return st68) where
   {-# NOINLINE st68 #-}
   st68 = let
      v67 :: T_MySegments_v67 
      v67 = \ (T_MySegments_vIn67 _lhsIain _lhsIasn _lhsIdone _lhsIfty _lhsIhoMapf _lhsIlfpf _lhsInmp _lhsInmprf _lhsIoptions _lhsIpmpf _lhsIpmprf _lhsIps _lhsIruleMap _lhsItdp _lhsIvisMapf _lhsIvisitnum) -> ( let
         _hdX65 = Control.Monad.Identity.runIdentity (attach_T_MySegment (arg_hd_))
         _tlX68 = Control.Monad.Identity.runIdentity (attach_T_MySegments (arg_tl_))
         (T_MySegment_vOut64 _hdIdone _hdIevisits _hdIself _hdIsynsO _hdIvisitnum _hdIvisnr) = inv_MySegment_s65 _hdX65 (T_MySegment_vIn64 _hdOain _hdOasn _hdOdone _hdOfty _hdOhoMapf _hdOlfpf _hdOnmp _hdOnmprf _hdOoptions _hdOpmpf _hdOpmprf _hdOps _hdOruleMap _hdOtdp _hdOvisMapf _hdOvisitnum)
         (T_MySegments_vOut67 _tlIevisits _tlIself _tlIvisitnum) = inv_MySegments_s68 _tlX68 (T_MySegments_vIn67 _tlOain _tlOasn _tlOdone _tlOfty _tlOhoMapf _tlOlfpf _tlOnmp _tlOnmprf _tlOoptions _tlOpmpf _tlOpmprf _tlOps _tlOruleMap _tlOtdp _tlOvisMapf _tlOvisitnum)
         _hdOdone = rule307 _lhsIdone
         _tlOdone = rule308 _hdIdone
         _lhsOevisits :: Visits
         _lhsOevisits = rule309 _hdIevisits _tlIevisits
         _self = rule310 _hdIself _tlIself
         _lhsOself :: MySegments
         _lhsOself = rule311 _self
         _lhsOvisitnum :: Int
         _lhsOvisitnum = rule312 _tlIvisitnum
         _hdOain = rule313 _lhsIain
         _hdOasn = rule314 _lhsIasn
         _hdOfty = rule315 _lhsIfty
         _hdOhoMapf = rule316 _lhsIhoMapf
         _hdOlfpf = rule317 _lhsIlfpf
         _hdOnmp = rule318 _lhsInmp
         _hdOnmprf = rule319 _lhsInmprf
         _hdOoptions = rule320 _lhsIoptions
         _hdOpmpf = rule321 _lhsIpmpf
         _hdOpmprf = rule322 _lhsIpmprf
         _hdOps = rule323 _lhsIps
         _hdOruleMap = rule324 _lhsIruleMap
         _hdOtdp = rule325 _lhsItdp
         _hdOvisMapf = rule326 _lhsIvisMapf
         _hdOvisitnum = rule327 _lhsIvisitnum
         _tlOain = rule328 _lhsIain
         _tlOasn = rule329 _lhsIasn
         _tlOfty = rule330 _lhsIfty
         _tlOhoMapf = rule331 _lhsIhoMapf
         _tlOlfpf = rule332 _lhsIlfpf
         _tlOnmp = rule333 _lhsInmp
         _tlOnmprf = rule334 _lhsInmprf
         _tlOoptions = rule335 _lhsIoptions
         _tlOpmpf = rule336 _lhsIpmpf
         _tlOpmprf = rule337 _lhsIpmprf
         _tlOps = rule338 _lhsIps
         _tlOruleMap = rule339 _lhsIruleMap
         _tlOtdp = rule340 _lhsItdp
         _tlOvisMapf = rule341 _lhsIvisMapf
         _tlOvisitnum = rule342 _hdIvisitnum
         __result_ = T_MySegments_vOut67 _lhsOevisits _lhsOself _lhsOvisitnum
         in __result_ )
     in C_MySegments_s68 v67
   {-# INLINE rule307 #-}
   {-# LINE 220 "src-ag/LOAG/Order.ag" #-}
   rule307 = \ ((_lhsIdone) ::  (Set.Set MyOccurrence, Set.Set FLabel
                                               , Set.Set Identifier, Set.Set (FLabel,Int))) ->
                        {-# LINE 220 "src-ag/LOAG/Order.ag" #-}
                        _lhsIdone
                        {-# LINE 3188 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule308 #-}
   {-# LINE 221 "src-ag/LOAG/Order.ag" #-}
   rule308 = \ ((_hdIdone) ::  (Set.Set MyOccurrence, Set.Set FLabel
                                              ,Set.Set Identifier, Set.Set (FLabel,Int))) ->
                        {-# LINE 221 "src-ag/LOAG/Order.ag" #-}
                        _hdIdone
                        {-# LINE 3195 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule309 #-}
   rule309 = \ ((_hdIevisits) :: Visit) ((_tlIevisits) :: Visits) ->
     _hdIevisits : _tlIevisits
   {-# INLINE rule310 #-}
   rule310 = \ ((_hdIself) :: MySegment) ((_tlIself) :: MySegments) ->
     (:) _hdIself _tlIself
   {-# INLINE rule311 #-}
   rule311 = \ _self ->
     _self
   {-# INLINE rule312 #-}
   rule312 = \ ((_tlIvisitnum) :: Int) ->
     _tlIvisitnum
   {-# INLINE rule313 #-}
   rule313 = \ ((_lhsIain) :: MyType -> MyAttributes) ->
     _lhsIain
   {-# INLINE rule314 #-}
   rule314 = \ ((_lhsIasn) :: MyType -> MyAttributes) ->
     _lhsIasn
   {-# INLINE rule315 #-}
   rule315 = \ ((_lhsIfty) :: FTY) ->
     _lhsIfty
   {-# INLINE rule316 #-}
   rule316 = \ ((_lhsIhoMapf) :: HOMap) ->
     _lhsIhoMapf
   {-# INLINE rule317 #-}
   rule317 = \ ((_lhsIlfpf) :: SF_P) ->
     _lhsIlfpf
   {-# INLINE rule318 #-}
   rule318 = \ ((_lhsInmp) :: NMP) ->
     _lhsInmp
   {-# INLINE rule319 #-}
   rule319 = \ ((_lhsInmprf) :: NMP_R) ->
     _lhsInmprf
   {-# INLINE rule320 #-}
   rule320 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule321 #-}
   rule321 = \ ((_lhsIpmpf) :: PMP) ->
     _lhsIpmpf
   {-# INLINE rule322 #-}
   rule322 = \ ((_lhsIpmprf) :: PMP_R) ->
     _lhsIpmprf
   {-# INLINE rule323 #-}
   rule323 = \ ((_lhsIps) :: PLabel) ->
     _lhsIps
   {-# INLINE rule324 #-}
   rule324 = \ ((_lhsIruleMap) :: Map.Map MyOccurrence Identifier) ->
     _lhsIruleMap
   {-# INLINE rule325 #-}
   rule325 = \ ((_lhsItdp) :: TDPRes) ->
     _lhsItdp
   {-# INLINE rule326 #-}
   rule326 = \ ((_lhsIvisMapf) :: IMap.IntMap Int) ->
     _lhsIvisMapf
   {-# INLINE rule327 #-}
   rule327 = \ ((_lhsIvisitnum) :: Int) ->
     _lhsIvisitnum
   {-# INLINE rule328 #-}
   rule328 = \ ((_lhsIain) :: MyType -> MyAttributes) ->
     _lhsIain
   {-# INLINE rule329 #-}
   rule329 = \ ((_lhsIasn) :: MyType -> MyAttributes) ->
     _lhsIasn
   {-# INLINE rule330 #-}
   rule330 = \ ((_lhsIfty) :: FTY) ->
     _lhsIfty
   {-# INLINE rule331 #-}
   rule331 = \ ((_lhsIhoMapf) :: HOMap) ->
     _lhsIhoMapf
   {-# INLINE rule332 #-}
   rule332 = \ ((_lhsIlfpf) :: SF_P) ->
     _lhsIlfpf
   {-# INLINE rule333 #-}
   rule333 = \ ((_lhsInmp) :: NMP) ->
     _lhsInmp
   {-# INLINE rule334 #-}
   rule334 = \ ((_lhsInmprf) :: NMP_R) ->
     _lhsInmprf
   {-# INLINE rule335 #-}
   rule335 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule336 #-}
   rule336 = \ ((_lhsIpmpf) :: PMP) ->
     _lhsIpmpf
   {-# INLINE rule337 #-}
   rule337 = \ ((_lhsIpmprf) :: PMP_R) ->
     _lhsIpmprf
   {-# INLINE rule338 #-}
   rule338 = \ ((_lhsIps) :: PLabel) ->
     _lhsIps
   {-# INLINE rule339 #-}
   rule339 = \ ((_lhsIruleMap) :: Map.Map MyOccurrence Identifier) ->
     _lhsIruleMap
   {-# INLINE rule340 #-}
   rule340 = \ ((_lhsItdp) :: TDPRes) ->
     _lhsItdp
   {-# INLINE rule341 #-}
   rule341 = \ ((_lhsIvisMapf) :: IMap.IntMap Int) ->
     _lhsIvisMapf
   {-# INLINE rule342 #-}
   rule342 = \ ((_hdIvisitnum) :: Int) ->
     _hdIvisitnum
{-# NOINLINE sem_MySegments_Nil #-}
sem_MySegments_Nil ::  T_MySegments 
sem_MySegments_Nil  = T_MySegments (return st68) where
   {-# NOINLINE st68 #-}
   st68 = let
      v67 :: T_MySegments_v67 
      v67 = \ (T_MySegments_vIn67 _lhsIain _lhsIasn _lhsIdone _lhsIfty _lhsIhoMapf _lhsIlfpf _lhsInmp _lhsInmprf _lhsIoptions _lhsIpmpf _lhsIpmprf _lhsIps _lhsIruleMap _lhsItdp _lhsIvisMapf _lhsIvisitnum) -> ( let
         _lhsOevisits :: Visits
         _lhsOevisits = rule343  ()
         _self = rule344  ()
         _lhsOself :: MySegments
         _lhsOself = rule345 _self
         _lhsOvisitnum :: Int
         _lhsOvisitnum = rule346 _lhsIvisitnum
         __result_ = T_MySegments_vOut67 _lhsOevisits _lhsOself _lhsOvisitnum
         in __result_ )
     in C_MySegments_s68 v67
   {-# INLINE rule343 #-}
   rule343 = \  (_ :: ()) ->
     []
   {-# INLINE rule344 #-}
   rule344 = \  (_ :: ()) ->
     []
   {-# INLINE rule345 #-}
   rule345 = \ _self ->
     _self
   {-# INLINE rule346 #-}
   rule346 = \ ((_lhsIvisitnum) :: Int) ->
     _lhsIvisitnum

-- Nonterminal -------------------------------------------------
-- wrapper
data Inh_Nonterminal  = Inh_Nonterminal { ain_Inh_Nonterminal :: (MyType -> MyAttributes), an_Inh_Nonterminal :: (MyType -> MyAttributes), aroundMap_Inh_Nonterminal :: (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))), asn_Inh_Nonterminal :: (MyType -> MyAttributes), augM_Inh_Nonterminal :: (Map.Map Identifier (Map.Map Identifier (Set.Set Dependency))), classContexts_Inh_Nonterminal :: (ContextMap), closedHoNtDeps_Inh_Nonterminal :: (Map NontermIdent (Set NontermIdent)), closedHoNtRevDeps_Inh_Nonterminal :: (Map NontermIdent (Set NontermIdent)), closedNtDeps_Inh_Nonterminal :: (Map NontermIdent (Set NontermIdent)), flab_Inh_Nonterminal :: (Int), fty_Inh_Nonterminal :: (FTY), ftyf_Inh_Nonterminal :: (FTY), hoMapf_Inh_Nonterminal :: (HOMap), lfpf_Inh_Nonterminal :: (SF_P), mergeMap_Inh_Nonterminal :: (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier, [Identifier], Expression)))), nmp_Inh_Nonterminal :: (NMP), nmprf_Inh_Nonterminal :: (NMP_R), olab_Inh_Nonterminal :: (Int), options_Inh_Nonterminal :: (Options), pmpf_Inh_Nonterminal :: (PMP), pmprf_Inh_Nonterminal :: (PMP_R), res_ads_Inh_Nonterminal :: ([Edge]), rulenumber_Inh_Nonterminal :: (Int), sched_Inh_Nonterminal :: (InterfaceRes), tdp_Inh_Nonterminal :: (TDPRes), visMapf_Inh_Nonterminal :: (IMap.IntMap Int), visitnum_Inh_Nonterminal :: (Int) }
data Syn_Nonterminal  = Syn_Nonterminal { ads_Syn_Nonterminal :: ([Edge]), ap_Syn_Nonterminal :: (A_P), enonts_Syn_Nonterminal :: (ENonterminals), fdps_Syn_Nonterminal :: (AttrOrderMap), fieldMap_Syn_Nonterminal :: (FMap), flab_Syn_Nonterminal :: (Int), fsInP_Syn_Nonterminal :: (FsInP), fty_Syn_Nonterminal :: (FTY), gen_Syn_Nonterminal :: (Map Int Int), hoMap_Syn_Nonterminal :: (HOMap), inhmap_Syn_Nonterminal :: (Map.Map NontermIdent Attributes), inhs_Syn_Nonterminal :: (AI_N), inss_Syn_Nonterminal :: (Map Int [Int]), lfp_Syn_Nonterminal :: (SF_P), lfpr_Syn_Nonterminal :: (SF_P), localSigMap_Syn_Nonterminal :: (Map.Map NontermIdent (Map.Map ConstructorIdent (Map.Map Identifier Type))), ntDeps_Syn_Nonterminal :: (Map NontermIdent (Set NontermIdent)), ntHoDeps_Syn_Nonterminal :: (Map NontermIdent (Set NontermIdent)), ofld_Syn_Nonterminal :: ([(Int, Int)]), olab_Syn_Nonterminal :: (Int), pmp_Syn_Nonterminal :: (PMP), pmpr_Syn_Nonterminal :: (PMP_R), ps_Syn_Nonterminal :: ([PLabel]), ruleMap_Syn_Nonterminal :: (Map.Map MyOccurrence Identifier), rulenumber_Syn_Nonterminal :: (Int), self_Syn_Nonterminal :: (Nonterminal), sfp_Syn_Nonterminal :: (SF_P), synmap_Syn_Nonterminal :: (Map.Map NontermIdent Attributes), syns_Syn_Nonterminal :: (AS_N), visMap_Syn_Nonterminal :: (IMap.IntMap Int), visitnum_Syn_Nonterminal :: (Int) }
{-# INLINABLE wrap_Nonterminal #-}
wrap_Nonterminal :: T_Nonterminal  -> Inh_Nonterminal  -> (Syn_Nonterminal )
wrap_Nonterminal (T_Nonterminal act) (Inh_Nonterminal _lhsIain _lhsIan _lhsIaroundMap _lhsIasn _lhsIaugM _lhsIclassContexts _lhsIclosedHoNtDeps _lhsIclosedHoNtRevDeps _lhsIclosedNtDeps _lhsIflab _lhsIfty _lhsIftyf _lhsIhoMapf _lhsIlfpf _lhsImergeMap _lhsInmp _lhsInmprf _lhsIolab _lhsIoptions _lhsIpmpf _lhsIpmprf _lhsIres_ads _lhsIrulenumber _lhsIsched _lhsItdp _lhsIvisMapf _lhsIvisitnum) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg70 = T_Nonterminal_vIn70 _lhsIain _lhsIan _lhsIaroundMap _lhsIasn _lhsIaugM _lhsIclassContexts _lhsIclosedHoNtDeps _lhsIclosedHoNtRevDeps _lhsIclosedNtDeps _lhsIflab _lhsIfty _lhsIftyf _lhsIhoMapf _lhsIlfpf _lhsImergeMap _lhsInmp _lhsInmprf _lhsIolab _lhsIoptions _lhsIpmpf _lhsIpmprf _lhsIres_ads _lhsIrulenumber _lhsIsched _lhsItdp _lhsIvisMapf _lhsIvisitnum
        (T_Nonterminal_vOut70 _lhsOads _lhsOap _lhsOenonts _lhsOfdps _lhsOfieldMap _lhsOflab _lhsOfsInP _lhsOfty _lhsOgen _lhsOhoMap _lhsOinhmap _lhsOinhs _lhsOinss _lhsOlfp _lhsOlfpr _lhsOlocalSigMap _lhsOntDeps _lhsOntHoDeps _lhsOofld _lhsOolab _lhsOpmp _lhsOpmpr _lhsOps _lhsOruleMap _lhsOrulenumber _lhsOself _lhsOsfp _lhsOsynmap _lhsOsyns _lhsOvisMap _lhsOvisitnum) <- return (inv_Nonterminal_s71 sem arg70)
        return (Syn_Nonterminal _lhsOads _lhsOap _lhsOenonts _lhsOfdps _lhsOfieldMap _lhsOflab _lhsOfsInP _lhsOfty _lhsOgen _lhsOhoMap _lhsOinhmap _lhsOinhs _lhsOinss _lhsOlfp _lhsOlfpr _lhsOlocalSigMap _lhsOntDeps _lhsOntHoDeps _lhsOofld _lhsOolab _lhsOpmp _lhsOpmpr _lhsOps _lhsOruleMap _lhsOrulenumber _lhsOself _lhsOsfp _lhsOsynmap _lhsOsyns _lhsOvisMap _lhsOvisitnum)
   )

-- cata
{-# INLINE sem_Nonterminal #-}
sem_Nonterminal :: Nonterminal  -> T_Nonterminal 
sem_Nonterminal ( Nonterminal nt_ params_ inh_ syn_ prods_ ) = sem_Nonterminal_Nonterminal nt_ params_ inh_ syn_ ( sem_Productions prods_ )

-- semantic domain
newtype T_Nonterminal  = T_Nonterminal {
                                       attach_T_Nonterminal :: Identity (T_Nonterminal_s71 )
                                       }
newtype T_Nonterminal_s71  = C_Nonterminal_s71 {
                                               inv_Nonterminal_s71 :: (T_Nonterminal_v70 )
                                               }
data T_Nonterminal_s72  = C_Nonterminal_s72
type T_Nonterminal_v70  = (T_Nonterminal_vIn70 ) -> (T_Nonterminal_vOut70 )
data T_Nonterminal_vIn70  = T_Nonterminal_vIn70 (MyType -> MyAttributes) (MyType -> MyAttributes) (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) (MyType -> MyAttributes) (Map.Map Identifier (Map.Map Identifier (Set.Set Dependency))) (ContextMap) (Map NontermIdent (Set NontermIdent)) (Map NontermIdent (Set NontermIdent)) (Map NontermIdent (Set NontermIdent)) (Int) (FTY) (FTY) (HOMap) (SF_P) (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier, [Identifier], Expression)))) (NMP) (NMP_R) (Int) (Options) (PMP) (PMP_R) ([Edge]) (Int) (InterfaceRes) (TDPRes) (IMap.IntMap Int) (Int)
data T_Nonterminal_vOut70  = T_Nonterminal_vOut70 ([Edge]) (A_P) (ENonterminals) (AttrOrderMap) (FMap) (Int) (FsInP) (FTY) (Map Int Int) (HOMap) (Map.Map NontermIdent Attributes) (AI_N) (Map Int [Int]) (SF_P) (SF_P) (Map.Map NontermIdent (Map.Map ConstructorIdent (Map.Map Identifier Type))) (Map NontermIdent (Set NontermIdent)) (Map NontermIdent (Set NontermIdent)) ([(Int, Int)]) (Int) (PMP) (PMP_R) ([PLabel]) (Map.Map MyOccurrence Identifier) (Int) (Nonterminal) (SF_P) (Map.Map NontermIdent Attributes) (AS_N) (IMap.IntMap Int) (Int)
{-# NOINLINE sem_Nonterminal_Nonterminal #-}
sem_Nonterminal_Nonterminal :: (NontermIdent) -> ([Identifier]) -> (Attributes) -> (Attributes) -> T_Productions  -> T_Nonterminal 
sem_Nonterminal_Nonterminal arg_nt_ arg_params_ arg_inh_ arg_syn_ arg_prods_ = T_Nonterminal (return st71) where
   {-# NOINLINE st71 #-}
   st71 = let
      v70 :: T_Nonterminal_v70 
      v70 = \ (T_Nonterminal_vIn70 _lhsIain _lhsIan _lhsIaroundMap _lhsIasn _lhsIaugM _lhsIclassContexts _lhsIclosedHoNtDeps _lhsIclosedHoNtRevDeps _lhsIclosedNtDeps _lhsIflab _lhsIfty _lhsIftyf _lhsIhoMapf _lhsIlfpf _lhsImergeMap _lhsInmp _lhsInmprf _lhsIolab _lhsIoptions _lhsIpmpf _lhsIpmprf _lhsIres_ads _lhsIrulenumber _lhsIsched _lhsItdp _lhsIvisMapf _lhsIvisitnum) -> ( let
         _prodsX86 = Control.Monad.Identity.runIdentity (attach_T_Productions (arg_prods_))
         (T_Productions_vOut85 _prodsIads _prodsIap _prodsIeprods _prodsIfdps _prodsIfieldMap _prodsIflab _prodsIfsInP _prodsIfty _prodsIgen _prodsIhoMap _prodsIinss _prodsIlfp _prodsIlfpr _prodsIlocalSigMap _prodsIofld _prodsIolab _prodsIpmp _prodsIpmpr _prodsIps _prodsIrefHoNts _prodsIrefNts _prodsIruleMap _prodsIrulenumber _prodsIself _prodsIsfp _prodsIvisitnum) = inv_Productions_s86 _prodsX86 (T_Productions_vIn85 _prodsOain _prodsOan _prodsOaroundMap _prodsOasn _prodsOaugM _prodsOdty _prodsOflab _prodsOfty _prodsOftyf _prodsOhoMapf _prodsOlfpf _prodsOmergeMap _prodsOmysegments _prodsOnmp _prodsOnmprf _prodsOolab _prodsOoptions _prodsOpmpf _prodsOpmprf _prodsOres_ads _prodsOrulenumber _prodsOtdp _prodsOvisMapf _prodsOvisitnum)
         _lhsOntDeps :: Map NontermIdent (Set NontermIdent)
         _lhsOntDeps = rule347 _prodsIrefNts arg_nt_
         _lhsOntHoDeps :: Map NontermIdent (Set NontermIdent)
         _lhsOntHoDeps = rule348 _prodsIrefHoNts arg_nt_
         _closedNtDeps = rule349 _lhsIclosedNtDeps arg_nt_
         _closedHoNtDeps = rule350 _lhsIclosedHoNtDeps arg_nt_
         _closedHoNtRevDeps = rule351 _lhsIclosedHoNtRevDeps arg_nt_
         _recursive = rule352 _closedNtDeps arg_nt_
         _nontrivAcyc = rule353 _closedHoNtDeps arg_nt_
         _hoInfo = rule354 _closedHoNtDeps _closedHoNtRevDeps _nontrivAcyc
         _classContexts = rule355 _lhsIclassContexts arg_nt_
         _aroundMap = rule356 _lhsIaroundMap arg_nt_
         _mergeMap = rule357 _lhsImergeMap arg_nt_
         _lhsOinhmap :: Map.Map NontermIdent Attributes
         _lhsOinhmap = rule358 arg_inh_ arg_nt_
         _lhsOsynmap :: Map.Map NontermIdent Attributes
         _lhsOsynmap = rule359 arg_nt_ arg_syn_
         _lhsOlocalSigMap :: Map.Map NontermIdent (Map.Map ConstructorIdent (Map.Map Identifier Type))
         _lhsOlocalSigMap = rule360 _prodsIlocalSigMap arg_nt_
         _lhsOinhs :: AI_N
         _lhsOinhs = rule361 arg_inh_ arg_nt_
         _lhsOsyns :: AS_N
         _lhsOsyns = rule362 arg_nt_ arg_syn_
         _prodsOaugM = rule363 _lhsIaugM arg_nt_
         _dty = rule364 arg_nt_
         _lhsOfdps :: AttrOrderMap
         _lhsOfdps = rule365 _prodsIfdps arg_nt_
         _initial = rule366 _lhsIvisitnum
         _vnums = rule367 _initial _segments
         _initialVisit = rule368 _vnums
         _nextVis = rule369 _initial _vnums
         _prevVis = rule370 _initial _vnums
         _visMap = rule371 _initial _mysegments
         _lhsOenonts :: ENonterminals
         _lhsOenonts = rule372 _classContexts _hoInfo _initial _initialVisit _nextVis _prevVis _prodsIeprods _recursive arg_nt_ arg_params_
         _assigned = rule373 _lhsIsched arg_nt_
         _mx = rule374 _assigned _lhsIsched
         _mysegments = rule375 _assigned _mx
         _segments = rule376 _lhsInmp _mysegments
         _lhsOads :: [Edge]
         _lhsOads = rule377 _prodsIads
         _lhsOap :: A_P
         _lhsOap = rule378 _prodsIap
         _lhsOfieldMap :: FMap
         _lhsOfieldMap = rule379 _prodsIfieldMap
         _lhsOfsInP :: FsInP
         _lhsOfsInP = rule380 _prodsIfsInP
         _lhsOfty :: FTY
         _lhsOfty = rule381 _prodsIfty
         _lhsOgen :: Map Int Int
         _lhsOgen = rule382 _prodsIgen
         _lhsOhoMap :: HOMap
         _lhsOhoMap = rule383 _prodsIhoMap
         _lhsOinss :: Map Int [Int]
         _lhsOinss = rule384 _prodsIinss
         _lhsOlfp :: SF_P
         _lhsOlfp = rule385 _prodsIlfp
         _lhsOlfpr :: SF_P
         _lhsOlfpr = rule386 _prodsIlfpr
         _lhsOofld :: [(Int, Int)]
         _lhsOofld = rule387 _prodsIofld
         _lhsOpmp :: PMP
         _lhsOpmp = rule388 _prodsIpmp
         _lhsOpmpr :: PMP_R
         _lhsOpmpr = rule389 _prodsIpmpr
         _lhsOps :: [PLabel]
         _lhsOps = rule390 _prodsIps
         _lhsOruleMap :: Map.Map MyOccurrence Identifier
         _lhsOruleMap = rule391 _prodsIruleMap
         _lhsOsfp :: SF_P
         _lhsOsfp = rule392 _prodsIsfp
         _lhsOvisMap :: IMap.IntMap Int
         _lhsOvisMap = rule393 _visMap
         _self = rule394 _prodsIself arg_inh_ arg_nt_ arg_params_ arg_syn_
         _lhsOself :: Nonterminal
         _lhsOself = rule395 _self
         _lhsOflab :: Int
         _lhsOflab = rule396 _prodsIflab
         _lhsOolab :: Int
         _lhsOolab = rule397 _prodsIolab
         _lhsOrulenumber :: Int
         _lhsOrulenumber = rule398 _prodsIrulenumber
         _lhsOvisitnum :: Int
         _lhsOvisitnum = rule399 _prodsIvisitnum
         _prodsOain = rule400 _lhsIain
         _prodsOan = rule401 _lhsIan
         _prodsOaroundMap = rule402 _aroundMap
         _prodsOasn = rule403 _lhsIasn
         _prodsOdty = rule404 _dty
         _prodsOflab = rule405 _lhsIflab
         _prodsOfty = rule406 _lhsIfty
         _prodsOftyf = rule407 _lhsIftyf
         _prodsOhoMapf = rule408 _lhsIhoMapf
         _prodsOlfpf = rule409 _lhsIlfpf
         _prodsOmergeMap = rule410 _mergeMap
         _prodsOmysegments = rule411 _mysegments
         _prodsOnmp = rule412 _lhsInmp
         _prodsOnmprf = rule413 _lhsInmprf
         _prodsOolab = rule414 _lhsIolab
         _prodsOoptions = rule415 _lhsIoptions
         _prodsOpmpf = rule416 _lhsIpmpf
         _prodsOpmprf = rule417 _lhsIpmprf
         _prodsOres_ads = rule418 _lhsIres_ads
         _prodsOrulenumber = rule419 _lhsIrulenumber
         _prodsOtdp = rule420 _lhsItdp
         _prodsOvisMapf = rule421 _lhsIvisMapf
         _prodsOvisitnum = rule422 _lhsIvisitnum
         __result_ = T_Nonterminal_vOut70 _lhsOads _lhsOap _lhsOenonts _lhsOfdps _lhsOfieldMap _lhsOflab _lhsOfsInP _lhsOfty _lhsOgen _lhsOhoMap _lhsOinhmap _lhsOinhs _lhsOinss _lhsOlfp _lhsOlfpr _lhsOlocalSigMap _lhsOntDeps _lhsOntHoDeps _lhsOofld _lhsOolab _lhsOpmp _lhsOpmpr _lhsOps _lhsOruleMap _lhsOrulenumber _lhsOself _lhsOsfp _lhsOsynmap _lhsOsyns _lhsOvisMap _lhsOvisitnum
         in __result_ )
     in C_Nonterminal_s71 v70
   {-# INLINE rule347 #-}
   {-# LINE 16 "src-ag/ExecutionPlanCommon.ag" #-}
   rule347 = \ ((_prodsIrefNts) :: Set NontermIdent) nt_ ->
                            {-# LINE 16 "src-ag/ExecutionPlanCommon.ag" #-}
                            Map.singleton nt_ _prodsIrefNts
                            {-# LINE 3482 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule348 #-}
   {-# LINE 17 "src-ag/ExecutionPlanCommon.ag" #-}
   rule348 = \ ((_prodsIrefHoNts) :: Set NontermIdent) nt_ ->
                            {-# LINE 17 "src-ag/ExecutionPlanCommon.ag" #-}
                            Map.singleton nt_ _prodsIrefHoNts
                            {-# LINE 3488 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule349 #-}
   {-# LINE 19 "src-ag/ExecutionPlanCommon.ag" #-}
   rule349 = \ ((_lhsIclosedNtDeps) :: Map NontermIdent (Set NontermIdent)) nt_ ->
                            {-# LINE 19 "src-ag/ExecutionPlanCommon.ag" #-}
                            Map.findWithDefault Set.empty nt_ _lhsIclosedNtDeps
                            {-# LINE 3494 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule350 #-}
   {-# LINE 20 "src-ag/ExecutionPlanCommon.ag" #-}
   rule350 = \ ((_lhsIclosedHoNtDeps) :: Map NontermIdent (Set NontermIdent)) nt_ ->
                            {-# LINE 20 "src-ag/ExecutionPlanCommon.ag" #-}
                            Map.findWithDefault Set.empty nt_ _lhsIclosedHoNtDeps
                            {-# LINE 3500 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule351 #-}
   {-# LINE 21 "src-ag/ExecutionPlanCommon.ag" #-}
   rule351 = \ ((_lhsIclosedHoNtRevDeps) :: Map NontermIdent (Set NontermIdent)) nt_ ->
                            {-# LINE 21 "src-ag/ExecutionPlanCommon.ag" #-}
                            Map.findWithDefault Set.empty nt_ _lhsIclosedHoNtRevDeps
                            {-# LINE 3506 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule352 #-}
   {-# LINE 23 "src-ag/ExecutionPlanCommon.ag" #-}
   rule352 = \ _closedNtDeps nt_ ->
                            {-# LINE 23 "src-ag/ExecutionPlanCommon.ag" #-}
                            nt_ `Set.member` _closedNtDeps
                            {-# LINE 3512 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule353 #-}
   {-# LINE 24 "src-ag/ExecutionPlanCommon.ag" #-}
   rule353 = \ _closedHoNtDeps nt_ ->
                            {-# LINE 24 "src-ag/ExecutionPlanCommon.ag" #-}
                            nt_ `Set.member` _closedHoNtDeps
                            {-# LINE 3518 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule354 #-}
   {-# LINE 25 "src-ag/ExecutionPlanCommon.ag" #-}
   rule354 = \ _closedHoNtDeps _closedHoNtRevDeps _nontrivAcyc ->
                            {-# LINE 25 "src-ag/ExecutionPlanCommon.ag" #-}
                            HigherOrderInfo { hoNtDeps            = _closedHoNtDeps
                                            , hoNtRevDeps         = _closedHoNtRevDeps
                                            , hoAcyclic           = _nontrivAcyc
                                            }
                            {-# LINE 3527 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule355 #-}
   {-# LINE 54 "src-ag/ExecutionPlanCommon.ag" #-}
   rule355 = \ ((_lhsIclassContexts) :: ContextMap) nt_ ->
                        {-# LINE 54 "src-ag/ExecutionPlanCommon.ag" #-}
                        Map.findWithDefault [] nt_ _lhsIclassContexts
                        {-# LINE 3533 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule356 #-}
   {-# LINE 88 "src-ag/ExecutionPlanCommon.ag" #-}
   rule356 = \ ((_lhsIaroundMap) :: Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) nt_ ->
                                                 {-# LINE 88 "src-ag/ExecutionPlanCommon.ag" #-}
                                                 Map.findWithDefault Map.empty nt_ _lhsIaroundMap
                                                 {-# LINE 3539 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule357 #-}
   {-# LINE 113 "src-ag/ExecutionPlanCommon.ag" #-}
   rule357 = \ ((_lhsImergeMap) :: Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier, [Identifier], Expression)))) nt_ ->
                                                {-# LINE 113 "src-ag/ExecutionPlanCommon.ag" #-}
                                                Map.findWithDefault Map.empty nt_ _lhsImergeMap
                                                {-# LINE 3545 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule358 #-}
   {-# LINE 149 "src-ag/ExecutionPlanCommon.ag" #-}
   rule358 = \ inh_ nt_ ->
                               {-# LINE 149 "src-ag/ExecutionPlanCommon.ag" #-}
                               Map.singleton nt_ inh_
                               {-# LINE 3551 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule359 #-}
   {-# LINE 150 "src-ag/ExecutionPlanCommon.ag" #-}
   rule359 = \ nt_ syn_ ->
                               {-# LINE 150 "src-ag/ExecutionPlanCommon.ag" #-}
                               Map.singleton nt_ syn_
                               {-# LINE 3557 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule360 #-}
   {-# LINE 159 "src-ag/ExecutionPlanCommon.ag" #-}
   rule360 = \ ((_prodsIlocalSigMap) :: Map.Map ConstructorIdent (Map.Map Identifier Type)) nt_ ->
                                                   {-# LINE 159 "src-ag/ExecutionPlanCommon.ag" #-}
                                                   Map.singleton nt_ _prodsIlocalSigMap
                                                   {-# LINE 3563 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule361 #-}
   {-# LINE 65 "src-ag/LOAG/Prepare.ag" #-}
   rule361 = \ inh_ nt_ ->
                 {-# LINE 65 "src-ag/LOAG/Prepare.ag" #-}
                 let dty = TyData (getName nt_)
                  in Map.singleton dty (toMyAttr Inh dty inh_)
                 {-# LINE 3570 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule362 #-}
   {-# LINE 67 "src-ag/LOAG/Prepare.ag" #-}
   rule362 = \ nt_ syn_ ->
                 {-# LINE 67 "src-ag/LOAG/Prepare.ag" #-}
                 let dty = TyData (getName nt_)
                  in Map.singleton dty (toMyAttr Syn dty syn_)
                 {-# LINE 3577 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule363 #-}
   {-# LINE 69 "src-ag/LOAG/Prepare.ag" #-}
   rule363 = \ ((_lhsIaugM) :: Map.Map Identifier (Map.Map Identifier (Set.Set Dependency))) nt_ ->
                   {-# LINE 69 "src-ag/LOAG/Prepare.ag" #-}
                   case Map.lookup nt_ _lhsIaugM of
                      Nothing -> Map.empty
                      Just a  -> a
                   {-# LINE 3585 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule364 #-}
   {-# LINE 131 "src-ag/LOAG/Prepare.ag" #-}
   rule364 = \ nt_ ->
                 {-# LINE 131 "src-ag/LOAG/Prepare.ag" #-}
                 TyData (getName nt_)
                 {-# LINE 3591 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule365 #-}
   {-# LINE 82 "src-ag/LOAG/Order.ag" #-}
   rule365 = \ ((_prodsIfdps) :: Map.Map ConstructorIdent (Set Dependency)) nt_ ->
                    {-# LINE 82 "src-ag/LOAG/Order.ag" #-}
                    Map.singleton nt_ _prodsIfdps
                    {-# LINE 3597 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule366 #-}
   {-# LINE 138 "src-ag/LOAG/Order.ag" #-}
   rule366 = \ ((_lhsIvisitnum) :: Int) ->
                        {-# LINE 138 "src-ag/LOAG/Order.ag" #-}
                        _lhsIvisitnum
                        {-# LINE 3603 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule367 #-}
   {-# LINE 139 "src-ag/LOAG/Order.ag" #-}
   rule367 = \ _initial _segments ->
                        {-# LINE 139 "src-ag/LOAG/Order.ag" #-}
                        zipWith const [_initial    ..] _segments
                        {-# LINE 3609 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule368 #-}
   {-# LINE 140 "src-ag/LOAG/Order.ag" #-}
   rule368 = \ _vnums ->
                             {-# LINE 140 "src-ag/LOAG/Order.ag" #-}
                             _vnums
                             {-# LINE 3615 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule369 #-}
   {-# LINE 141 "src-ag/LOAG/Order.ag" #-}
   rule369 = \ _initial _vnums ->
                        {-# LINE 141 "src-ag/LOAG/Order.ag" #-}
                        Map.fromList $ (_initial     + length _vnums, NoneVis)
                                     : [(v, OneVis v) | v <- _vnums ]
                        {-# LINE 3622 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule370 #-}
   {-# LINE 143 "src-ag/LOAG/Order.ag" #-}
   rule370 = \ _initial _vnums ->
                        {-# LINE 143 "src-ag/LOAG/Order.ag" #-}
                        Map.fromList $ (_initial    , NoneVis)
                                     : [(v+1, OneVis v) | v <- _vnums ]
                        {-# LINE 3629 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule371 #-}
   {-# LINE 145 "src-ag/LOAG/Order.ag" #-}
   rule371 = \ _initial _mysegments ->
                        {-# LINE 145 "src-ag/LOAG/Order.ag" #-}
                        let op vnr (MySegment visnr ins syns _ _) =
                              IMap.fromList $ zip syns (repeat vnr)
                         in IMap.unions $ zipWith op [_initial    ..] _mysegments
                        {-# LINE 3637 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule372 #-}
   {-# LINE 148 "src-ag/LOAG/Order.ag" #-}
   rule372 = \ _classContexts _hoInfo _initial _initialVisit _nextVis _prevVis ((_prodsIeprods) :: EProductions) _recursive nt_ params_ ->
                       {-# LINE 148 "src-ag/LOAG/Order.ag" #-}
                       [ENonterminal
                          nt_
                          params_
                          _classContexts
                          _initial
                          _initialVisit
                          _nextVis
                          _prevVis
                          _prodsIeprods
                          _recursive
                          _hoInfo     ]
                       {-# LINE 3653 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule373 #-}
   {-# LINE 322 "src-ag/LOAG/Order.ag" #-}
   rule373 = \ ((_lhsIsched) :: InterfaceRes) nt_ ->
                         {-# LINE 322 "src-ag/LOAG/Order.ag" #-}
                         findWithErr _lhsIsched "could not const. interfaces"
                              (getName nt_)
                         {-# LINE 3660 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule374 #-}
   {-# LINE 324 "src-ag/LOAG/Order.ag" #-}
   rule374 = \ _assigned ((_lhsIsched) :: InterfaceRes) ->
                         {-# LINE 324 "src-ag/LOAG/Order.ag" #-}
                         if Map.null _lhsIsched
                          then 0
                          else let mx = fst $ IMap.findMax _assigned     in
                                if even mx then mx else mx + 1
                         {-# LINE 3669 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule375 #-}
   {-# LINE 329 "src-ag/LOAG/Order.ag" #-}
   rule375 = \ _assigned _mx ->
              {-# LINE 329 "src-ag/LOAG/Order.ag" #-}
              map (\i -> MySegment ((_mx     - i) `div` 2)
                          (maybe [] id $ IMap.lookup i _assigned    )
                          (maybe [] id $ IMap.lookup (i-1) _assigned    )
                              Nothing Nothing)
                   [_mx    ,_mx    -2 .. 2]
              {-# LINE 3679 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule376 #-}
   {-# LINE 335 "src-ag/LOAG/Order.ag" #-}
   rule376 = \ ((_lhsInmp) :: NMP) _mysegments ->
              {-# LINE 335 "src-ag/LOAG/Order.ag" #-}
              map (\(MySegment visnr is ss _ _) ->
                      CSegment (Map.unions $ map (vertexToAttr _lhsInmp) is)
                               (Map.unions $ map (vertexToAttr _lhsInmp) ss))
                  _mysegments
              {-# LINE 3688 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule377 #-}
   rule377 = \ ((_prodsIads) :: [Edge]) ->
     _prodsIads
   {-# INLINE rule378 #-}
   rule378 = \ ((_prodsIap) :: A_P) ->
     _prodsIap
   {-# INLINE rule379 #-}
   rule379 = \ ((_prodsIfieldMap) :: FMap) ->
     _prodsIfieldMap
   {-# INLINE rule380 #-}
   rule380 = \ ((_prodsIfsInP) :: FsInP) ->
     _prodsIfsInP
   {-# INLINE rule381 #-}
   rule381 = \ ((_prodsIfty) :: FTY) ->
     _prodsIfty
   {-# INLINE rule382 #-}
   rule382 = \ ((_prodsIgen) :: Map Int Int) ->
     _prodsIgen
   {-# INLINE rule383 #-}
   rule383 = \ ((_prodsIhoMap) :: HOMap) ->
     _prodsIhoMap
   {-# INLINE rule384 #-}
   rule384 = \ ((_prodsIinss) :: Map Int [Int]) ->
     _prodsIinss
   {-# INLINE rule385 #-}
   rule385 = \ ((_prodsIlfp) :: SF_P) ->
     _prodsIlfp
   {-# INLINE rule386 #-}
   rule386 = \ ((_prodsIlfpr) :: SF_P) ->
     _prodsIlfpr
   {-# INLINE rule387 #-}
   rule387 = \ ((_prodsIofld) :: [(Int, Int)]) ->
     _prodsIofld
   {-# INLINE rule388 #-}
   rule388 = \ ((_prodsIpmp) :: PMP) ->
     _prodsIpmp
   {-# INLINE rule389 #-}
   rule389 = \ ((_prodsIpmpr) :: PMP_R) ->
     _prodsIpmpr
   {-# INLINE rule390 #-}
   rule390 = \ ((_prodsIps) :: [PLabel]) ->
     _prodsIps
   {-# INLINE rule391 #-}
   rule391 = \ ((_prodsIruleMap) :: Map.Map MyOccurrence Identifier) ->
     _prodsIruleMap
   {-# INLINE rule392 #-}
   rule392 = \ ((_prodsIsfp) :: SF_P) ->
     _prodsIsfp
   {-# INLINE rule393 #-}
   rule393 = \ _visMap ->
     _visMap
   {-# INLINE rule394 #-}
   rule394 = \ ((_prodsIself) :: Productions) inh_ nt_ params_ syn_ ->
     Nonterminal nt_ params_ inh_ syn_ _prodsIself
   {-# INLINE rule395 #-}
   rule395 = \ _self ->
     _self
   {-# INLINE rule396 #-}
   rule396 = \ ((_prodsIflab) :: Int) ->
     _prodsIflab
   {-# INLINE rule397 #-}
   rule397 = \ ((_prodsIolab) :: Int) ->
     _prodsIolab
   {-# INLINE rule398 #-}
   rule398 = \ ((_prodsIrulenumber) :: Int) ->
     _prodsIrulenumber
   {-# INLINE rule399 #-}
   rule399 = \ ((_prodsIvisitnum) :: Int) ->
     _prodsIvisitnum
   {-# INLINE rule400 #-}
   rule400 = \ ((_lhsIain) :: MyType -> MyAttributes) ->
     _lhsIain
   {-# INLINE rule401 #-}
   rule401 = \ ((_lhsIan) :: MyType -> MyAttributes) ->
     _lhsIan
   {-# INLINE rule402 #-}
   rule402 = \ _aroundMap ->
     _aroundMap
   {-# INLINE rule403 #-}
   rule403 = \ ((_lhsIasn) :: MyType -> MyAttributes) ->
     _lhsIasn
   {-# INLINE rule404 #-}
   rule404 = \ _dty ->
     _dty
   {-# INLINE rule405 #-}
   rule405 = \ ((_lhsIflab) :: Int) ->
     _lhsIflab
   {-# INLINE rule406 #-}
   rule406 = \ ((_lhsIfty) :: FTY) ->
     _lhsIfty
   {-# INLINE rule407 #-}
   rule407 = \ ((_lhsIftyf) :: FTY) ->
     _lhsIftyf
   {-# INLINE rule408 #-}
   rule408 = \ ((_lhsIhoMapf) :: HOMap) ->
     _lhsIhoMapf
   {-# INLINE rule409 #-}
   rule409 = \ ((_lhsIlfpf) :: SF_P) ->
     _lhsIlfpf
   {-# INLINE rule410 #-}
   rule410 = \ _mergeMap ->
     _mergeMap
   {-# INLINE rule411 #-}
   rule411 = \ _mysegments ->
     _mysegments
   {-# INLINE rule412 #-}
   rule412 = \ ((_lhsInmp) :: NMP) ->
     _lhsInmp
   {-# INLINE rule413 #-}
   rule413 = \ ((_lhsInmprf) :: NMP_R) ->
     _lhsInmprf
   {-# INLINE rule414 #-}
   rule414 = \ ((_lhsIolab) :: Int) ->
     _lhsIolab
   {-# INLINE rule415 #-}
   rule415 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule416 #-}
   rule416 = \ ((_lhsIpmpf) :: PMP) ->
     _lhsIpmpf
   {-# INLINE rule417 #-}
   rule417 = \ ((_lhsIpmprf) :: PMP_R) ->
     _lhsIpmprf
   {-# INLINE rule418 #-}
   rule418 = \ ((_lhsIres_ads) :: [Edge]) ->
     _lhsIres_ads
   {-# INLINE rule419 #-}
   rule419 = \ ((_lhsIrulenumber) :: Int) ->
     _lhsIrulenumber
   {-# INLINE rule420 #-}
   rule420 = \ ((_lhsItdp) :: TDPRes) ->
     _lhsItdp
   {-# INLINE rule421 #-}
   rule421 = \ ((_lhsIvisMapf) :: IMap.IntMap Int) ->
     _lhsIvisMapf
   {-# INLINE rule422 #-}
   rule422 = \ ((_lhsIvisitnum) :: Int) ->
     _lhsIvisitnum

-- Nonterminals ------------------------------------------------
-- wrapper
data Inh_Nonterminals  = Inh_Nonterminals { ain_Inh_Nonterminals :: (MyType -> MyAttributes), an_Inh_Nonterminals :: (MyType -> MyAttributes), aroundMap_Inh_Nonterminals :: (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))), asn_Inh_Nonterminals :: (MyType -> MyAttributes), augM_Inh_Nonterminals :: (Map.Map Identifier (Map.Map Identifier (Set.Set Dependency))), classContexts_Inh_Nonterminals :: (ContextMap), closedHoNtDeps_Inh_Nonterminals :: (Map NontermIdent (Set NontermIdent)), closedHoNtRevDeps_Inh_Nonterminals :: (Map NontermIdent (Set NontermIdent)), closedNtDeps_Inh_Nonterminals :: (Map NontermIdent (Set NontermIdent)), flab_Inh_Nonterminals :: (Int), fty_Inh_Nonterminals :: (FTY), ftyf_Inh_Nonterminals :: (FTY), hoMapf_Inh_Nonterminals :: (HOMap), lfpf_Inh_Nonterminals :: (SF_P), mergeMap_Inh_Nonterminals :: (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier, [Identifier], Expression)))), nmp_Inh_Nonterminals :: (NMP), nmprf_Inh_Nonterminals :: (NMP_R), olab_Inh_Nonterminals :: (Int), options_Inh_Nonterminals :: (Options), pmpf_Inh_Nonterminals :: (PMP), pmprf_Inh_Nonterminals :: (PMP_R), res_ads_Inh_Nonterminals :: ([Edge]), rulenumber_Inh_Nonterminals :: (Int), sched_Inh_Nonterminals :: (InterfaceRes), tdp_Inh_Nonterminals :: (TDPRes), visMapf_Inh_Nonterminals :: (IMap.IntMap Int), visitnum_Inh_Nonterminals :: (Int) }
data Syn_Nonterminals  = Syn_Nonterminals { ads_Syn_Nonterminals :: ([Edge]), ap_Syn_Nonterminals :: (A_P), enonts_Syn_Nonterminals :: (ENonterminals), fdps_Syn_Nonterminals :: (AttrOrderMap), fieldMap_Syn_Nonterminals :: (FMap), flab_Syn_Nonterminals :: (Int), fsInP_Syn_Nonterminals :: (FsInP), fty_Syn_Nonterminals :: (FTY), gen_Syn_Nonterminals :: (Map Int Int), hoMap_Syn_Nonterminals :: (HOMap), inhmap_Syn_Nonterminals :: (Map.Map NontermIdent Attributes), inhs_Syn_Nonterminals :: (AI_N), inss_Syn_Nonterminals :: (Map Int [Int]), lfp_Syn_Nonterminals :: (SF_P), lfpr_Syn_Nonterminals :: (SF_P), localSigMap_Syn_Nonterminals :: (Map.Map NontermIdent (Map.Map ConstructorIdent (Map.Map Identifier Type))), ntDeps_Syn_Nonterminals :: (Map NontermIdent (Set NontermIdent)), ntHoDeps_Syn_Nonterminals :: (Map NontermIdent (Set NontermIdent)), ofld_Syn_Nonterminals :: ([(Int, Int)]), olab_Syn_Nonterminals :: (Int), pmp_Syn_Nonterminals :: (PMP), pmpr_Syn_Nonterminals :: (PMP_R), ps_Syn_Nonterminals :: ([PLabel]), ruleMap_Syn_Nonterminals :: (Map.Map MyOccurrence Identifier), rulenumber_Syn_Nonterminals :: (Int), self_Syn_Nonterminals :: (Nonterminals), sfp_Syn_Nonterminals :: (SF_P), synmap_Syn_Nonterminals :: (Map.Map NontermIdent Attributes), syns_Syn_Nonterminals :: (AS_N), visMap_Syn_Nonterminals :: (IMap.IntMap Int), visitnum_Syn_Nonterminals :: (Int) }
{-# INLINABLE wrap_Nonterminals #-}
wrap_Nonterminals :: T_Nonterminals  -> Inh_Nonterminals  -> (Syn_Nonterminals )
wrap_Nonterminals (T_Nonterminals act) (Inh_Nonterminals _lhsIain _lhsIan _lhsIaroundMap _lhsIasn _lhsIaugM _lhsIclassContexts _lhsIclosedHoNtDeps _lhsIclosedHoNtRevDeps _lhsIclosedNtDeps _lhsIflab _lhsIfty _lhsIftyf _lhsIhoMapf _lhsIlfpf _lhsImergeMap _lhsInmp _lhsInmprf _lhsIolab _lhsIoptions _lhsIpmpf _lhsIpmprf _lhsIres_ads _lhsIrulenumber _lhsIsched _lhsItdp _lhsIvisMapf _lhsIvisitnum) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg73 = T_Nonterminals_vIn73 _lhsIain _lhsIan _lhsIaroundMap _lhsIasn _lhsIaugM _lhsIclassContexts _lhsIclosedHoNtDeps _lhsIclosedHoNtRevDeps _lhsIclosedNtDeps _lhsIflab _lhsIfty _lhsIftyf _lhsIhoMapf _lhsIlfpf _lhsImergeMap _lhsInmp _lhsInmprf _lhsIolab _lhsIoptions _lhsIpmpf _lhsIpmprf _lhsIres_ads _lhsIrulenumber _lhsIsched _lhsItdp _lhsIvisMapf _lhsIvisitnum
        (T_Nonterminals_vOut73 _lhsOads _lhsOap _lhsOenonts _lhsOfdps _lhsOfieldMap _lhsOflab _lhsOfsInP _lhsOfty _lhsOgen _lhsOhoMap _lhsOinhmap _lhsOinhs _lhsOinss _lhsOlfp _lhsOlfpr _lhsOlocalSigMap _lhsOntDeps _lhsOntHoDeps _lhsOofld _lhsOolab _lhsOpmp _lhsOpmpr _lhsOps _lhsOruleMap _lhsOrulenumber _lhsOself _lhsOsfp _lhsOsynmap _lhsOsyns _lhsOvisMap _lhsOvisitnum) <- return (inv_Nonterminals_s74 sem arg73)
        return (Syn_Nonterminals _lhsOads _lhsOap _lhsOenonts _lhsOfdps _lhsOfieldMap _lhsOflab _lhsOfsInP _lhsOfty _lhsOgen _lhsOhoMap _lhsOinhmap _lhsOinhs _lhsOinss _lhsOlfp _lhsOlfpr _lhsOlocalSigMap _lhsOntDeps _lhsOntHoDeps _lhsOofld _lhsOolab _lhsOpmp _lhsOpmpr _lhsOps _lhsOruleMap _lhsOrulenumber _lhsOself _lhsOsfp _lhsOsynmap _lhsOsyns _lhsOvisMap _lhsOvisitnum)
   )

-- cata
{-# NOINLINE sem_Nonterminals #-}
sem_Nonterminals :: Nonterminals  -> T_Nonterminals 
sem_Nonterminals list = Prelude.foldr sem_Nonterminals_Cons sem_Nonterminals_Nil (Prelude.map sem_Nonterminal list)

-- semantic domain
newtype T_Nonterminals  = T_Nonterminals {
                                         attach_T_Nonterminals :: Identity (T_Nonterminals_s74 )
                                         }
newtype T_Nonterminals_s74  = C_Nonterminals_s74 {
                                                 inv_Nonterminals_s74 :: (T_Nonterminals_v73 )
                                                 }
data T_Nonterminals_s75  = C_Nonterminals_s75
type T_Nonterminals_v73  = (T_Nonterminals_vIn73 ) -> (T_Nonterminals_vOut73 )
data T_Nonterminals_vIn73  = T_Nonterminals_vIn73 (MyType -> MyAttributes) (MyType -> MyAttributes) (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) (MyType -> MyAttributes) (Map.Map Identifier (Map.Map Identifier (Set.Set Dependency))) (ContextMap) (Map NontermIdent (Set NontermIdent)) (Map NontermIdent (Set NontermIdent)) (Map NontermIdent (Set NontermIdent)) (Int) (FTY) (FTY) (HOMap) (SF_P) (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier, [Identifier], Expression)))) (NMP) (NMP_R) (Int) (Options) (PMP) (PMP_R) ([Edge]) (Int) (InterfaceRes) (TDPRes) (IMap.IntMap Int) (Int)
data T_Nonterminals_vOut73  = T_Nonterminals_vOut73 ([Edge]) (A_P) (ENonterminals) (AttrOrderMap) (FMap) (Int) (FsInP) (FTY) (Map Int Int) (HOMap) (Map.Map NontermIdent Attributes) (AI_N) (Map Int [Int]) (SF_P) (SF_P) (Map.Map NontermIdent (Map.Map ConstructorIdent (Map.Map Identifier Type))) (Map NontermIdent (Set NontermIdent)) (Map NontermIdent (Set NontermIdent)) ([(Int, Int)]) (Int) (PMP) (PMP_R) ([PLabel]) (Map.Map MyOccurrence Identifier) (Int) (Nonterminals) (SF_P) (Map.Map NontermIdent Attributes) (AS_N) (IMap.IntMap Int) (Int)
{-# NOINLINE sem_Nonterminals_Cons #-}
sem_Nonterminals_Cons :: T_Nonterminal  -> T_Nonterminals  -> T_Nonterminals 
sem_Nonterminals_Cons arg_hd_ arg_tl_ = T_Nonterminals (return st74) where
   {-# NOINLINE st74 #-}
   st74 = let
      v73 :: T_Nonterminals_v73 
      v73 = \ (T_Nonterminals_vIn73 _lhsIain _lhsIan _lhsIaroundMap _lhsIasn _lhsIaugM _lhsIclassContexts _lhsIclosedHoNtDeps _lhsIclosedHoNtRevDeps _lhsIclosedNtDeps _lhsIflab _lhsIfty _lhsIftyf _lhsIhoMapf _lhsIlfpf _lhsImergeMap _lhsInmp _lhsInmprf _lhsIolab _lhsIoptions _lhsIpmpf _lhsIpmprf _lhsIres_ads _lhsIrulenumber _lhsIsched _lhsItdp _lhsIvisMapf _lhsIvisitnum) -> ( let
         _hdX71 = Control.Monad.Identity.runIdentity (attach_T_Nonterminal (arg_hd_))
         _tlX74 = Control.Monad.Identity.runIdentity (attach_T_Nonterminals (arg_tl_))
         (T_Nonterminal_vOut70 _hdIads _hdIap _hdIenonts _hdIfdps _hdIfieldMap _hdIflab _hdIfsInP _hdIfty _hdIgen _hdIhoMap _hdIinhmap _hdIinhs _hdIinss _hdIlfp _hdIlfpr _hdIlocalSigMap _hdIntDeps _hdIntHoDeps _hdIofld _hdIolab _hdIpmp _hdIpmpr _hdIps _hdIruleMap _hdIrulenumber _hdIself _hdIsfp _hdIsynmap _hdIsyns _hdIvisMap _hdIvisitnum) = inv_Nonterminal_s71 _hdX71 (T_Nonterminal_vIn70 _hdOain _hdOan _hdOaroundMap _hdOasn _hdOaugM _hdOclassContexts _hdOclosedHoNtDeps _hdOclosedHoNtRevDeps _hdOclosedNtDeps _hdOflab _hdOfty _hdOftyf _hdOhoMapf _hdOlfpf _hdOmergeMap _hdOnmp _hdOnmprf _hdOolab _hdOoptions _hdOpmpf _hdOpmprf _hdOres_ads _hdOrulenumber _hdOsched _hdOtdp _hdOvisMapf _hdOvisitnum)
         (T_Nonterminals_vOut73 _tlIads _tlIap _tlIenonts _tlIfdps _tlIfieldMap _tlIflab _tlIfsInP _tlIfty _tlIgen _tlIhoMap _tlIinhmap _tlIinhs _tlIinss _tlIlfp _tlIlfpr _tlIlocalSigMap _tlIntDeps _tlIntHoDeps _tlIofld _tlIolab _tlIpmp _tlIpmpr _tlIps _tlIruleMap _tlIrulenumber _tlIself _tlIsfp _tlIsynmap _tlIsyns _tlIvisMap _tlIvisitnum) = inv_Nonterminals_s74 _tlX74 (T_Nonterminals_vIn73 _tlOain _tlOan _tlOaroundMap _tlOasn _tlOaugM _tlOclassContexts _tlOclosedHoNtDeps _tlOclosedHoNtRevDeps _tlOclosedNtDeps _tlOflab _tlOfty _tlOftyf _tlOhoMapf _tlOlfpf _tlOmergeMap _tlOnmp _tlOnmprf _tlOolab _tlOoptions _tlOpmpf _tlOpmprf _tlOres_ads _tlOrulenumber _tlOsched _tlOtdp _tlOvisMapf _tlOvisitnum)
         _lhsOads :: [Edge]
         _lhsOads = rule423 _hdIads _tlIads
         _lhsOap :: A_P
         _lhsOap = rule424 _hdIap _tlIap
         _lhsOenonts :: ENonterminals
         _lhsOenonts = rule425 _hdIenonts _tlIenonts
         _lhsOfdps :: AttrOrderMap
         _lhsOfdps = rule426 _hdIfdps _tlIfdps
         _lhsOfieldMap :: FMap
         _lhsOfieldMap = rule427 _hdIfieldMap _tlIfieldMap
         _lhsOfsInP :: FsInP
         _lhsOfsInP = rule428 _hdIfsInP _tlIfsInP
         _lhsOfty :: FTY
         _lhsOfty = rule429 _hdIfty _tlIfty
         _lhsOgen :: Map Int Int
         _lhsOgen = rule430 _hdIgen _tlIgen
         _lhsOhoMap :: HOMap
         _lhsOhoMap = rule431 _hdIhoMap _tlIhoMap
         _lhsOinhmap :: Map.Map NontermIdent Attributes
         _lhsOinhmap = rule432 _hdIinhmap _tlIinhmap
         _lhsOinhs :: AI_N
         _lhsOinhs = rule433 _hdIinhs _tlIinhs
         _lhsOinss :: Map Int [Int]
         _lhsOinss = rule434 _hdIinss _tlIinss
         _lhsOlfp :: SF_P
         _lhsOlfp = rule435 _hdIlfp _tlIlfp
         _lhsOlfpr :: SF_P
         _lhsOlfpr = rule436 _hdIlfpr _tlIlfpr
         _lhsOlocalSigMap :: Map.Map NontermIdent (Map.Map ConstructorIdent (Map.Map Identifier Type))
         _lhsOlocalSigMap = rule437 _hdIlocalSigMap _tlIlocalSigMap
         _lhsOntDeps :: Map NontermIdent (Set NontermIdent)
         _lhsOntDeps = rule438 _hdIntDeps _tlIntDeps
         _lhsOntHoDeps :: Map NontermIdent (Set NontermIdent)
         _lhsOntHoDeps = rule439 _hdIntHoDeps _tlIntHoDeps
         _lhsOofld :: [(Int, Int)]
         _lhsOofld = rule440 _hdIofld _tlIofld
         _lhsOpmp :: PMP
         _lhsOpmp = rule441 _hdIpmp _tlIpmp
         _lhsOpmpr :: PMP_R
         _lhsOpmpr = rule442 _hdIpmpr _tlIpmpr
         _lhsOps :: [PLabel]
         _lhsOps = rule443 _hdIps _tlIps
         _lhsOruleMap :: Map.Map MyOccurrence Identifier
         _lhsOruleMap = rule444 _hdIruleMap _tlIruleMap
         _lhsOsfp :: SF_P
         _lhsOsfp = rule445 _hdIsfp _tlIsfp
         _lhsOsynmap :: Map.Map NontermIdent Attributes
         _lhsOsynmap = rule446 _hdIsynmap _tlIsynmap
         _lhsOsyns :: AS_N
         _lhsOsyns = rule447 _hdIsyns _tlIsyns
         _lhsOvisMap :: IMap.IntMap Int
         _lhsOvisMap = rule448 _hdIvisMap _tlIvisMap
         _self = rule449 _hdIself _tlIself
         _lhsOself :: Nonterminals
         _lhsOself = rule450 _self
         _lhsOflab :: Int
         _lhsOflab = rule451 _tlIflab
         _lhsOolab :: Int
         _lhsOolab = rule452 _tlIolab
         _lhsOrulenumber :: Int
         _lhsOrulenumber = rule453 _tlIrulenumber
         _lhsOvisitnum :: Int
         _lhsOvisitnum = rule454 _tlIvisitnum
         _hdOain = rule455 _lhsIain
         _hdOan = rule456 _lhsIan
         _hdOaroundMap = rule457 _lhsIaroundMap
         _hdOasn = rule458 _lhsIasn
         _hdOaugM = rule459 _lhsIaugM
         _hdOclassContexts = rule460 _lhsIclassContexts
         _hdOclosedHoNtDeps = rule461 _lhsIclosedHoNtDeps
         _hdOclosedHoNtRevDeps = rule462 _lhsIclosedHoNtRevDeps
         _hdOclosedNtDeps = rule463 _lhsIclosedNtDeps
         _hdOflab = rule464 _lhsIflab
         _hdOfty = rule465 _lhsIfty
         _hdOftyf = rule466 _lhsIftyf
         _hdOhoMapf = rule467 _lhsIhoMapf
         _hdOlfpf = rule468 _lhsIlfpf
         _hdOmergeMap = rule469 _lhsImergeMap
         _hdOnmp = rule470 _lhsInmp
         _hdOnmprf = rule471 _lhsInmprf
         _hdOolab = rule472 _lhsIolab
         _hdOoptions = rule473 _lhsIoptions
         _hdOpmpf = rule474 _lhsIpmpf
         _hdOpmprf = rule475 _lhsIpmprf
         _hdOres_ads = rule476 _lhsIres_ads
         _hdOrulenumber = rule477 _lhsIrulenumber
         _hdOsched = rule478 _lhsIsched
         _hdOtdp = rule479 _lhsItdp
         _hdOvisMapf = rule480 _lhsIvisMapf
         _hdOvisitnum = rule481 _lhsIvisitnum
         _tlOain = rule482 _lhsIain
         _tlOan = rule483 _lhsIan
         _tlOaroundMap = rule484 _lhsIaroundMap
         _tlOasn = rule485 _lhsIasn
         _tlOaugM = rule486 _lhsIaugM
         _tlOclassContexts = rule487 _lhsIclassContexts
         _tlOclosedHoNtDeps = rule488 _lhsIclosedHoNtDeps
         _tlOclosedHoNtRevDeps = rule489 _lhsIclosedHoNtRevDeps
         _tlOclosedNtDeps = rule490 _lhsIclosedNtDeps
         _tlOflab = rule491 _hdIflab
         _tlOfty = rule492 _hdIfty
         _tlOftyf = rule493 _lhsIftyf
         _tlOhoMapf = rule494 _lhsIhoMapf
         _tlOlfpf = rule495 _lhsIlfpf
         _tlOmergeMap = rule496 _lhsImergeMap
         _tlOnmp = rule497 _lhsInmp
         _tlOnmprf = rule498 _lhsInmprf
         _tlOolab = rule499 _hdIolab
         _tlOoptions = rule500 _lhsIoptions
         _tlOpmpf = rule501 _lhsIpmpf
         _tlOpmprf = rule502 _lhsIpmprf
         _tlOres_ads = rule503 _lhsIres_ads
         _tlOrulenumber = rule504 _hdIrulenumber
         _tlOsched = rule505 _lhsIsched
         _tlOtdp = rule506 _lhsItdp
         _tlOvisMapf = rule507 _lhsIvisMapf
         _tlOvisitnum = rule508 _hdIvisitnum
         __result_ = T_Nonterminals_vOut73 _lhsOads _lhsOap _lhsOenonts _lhsOfdps _lhsOfieldMap _lhsOflab _lhsOfsInP _lhsOfty _lhsOgen _lhsOhoMap _lhsOinhmap _lhsOinhs _lhsOinss _lhsOlfp _lhsOlfpr _lhsOlocalSigMap _lhsOntDeps _lhsOntHoDeps _lhsOofld _lhsOolab _lhsOpmp _lhsOpmpr _lhsOps _lhsOruleMap _lhsOrulenumber _lhsOself _lhsOsfp _lhsOsynmap _lhsOsyns _lhsOvisMap _lhsOvisitnum
         in __result_ )
     in C_Nonterminals_s74 v73
   {-# INLINE rule423 #-}
   rule423 = \ ((_hdIads) :: [Edge]) ((_tlIads) :: [Edge]) ->
     ((++) _hdIads _tlIads)
   {-# INLINE rule424 #-}
   rule424 = \ ((_hdIap) :: A_P) ((_tlIap) :: A_P) ->
     (Map.unionWith (++) _hdIap _tlIap)
   {-# INLINE rule425 #-}
   rule425 = \ ((_hdIenonts) :: ENonterminals) ((_tlIenonts) :: ENonterminals) ->
     ((++) _hdIenonts _tlIenonts)
   {-# INLINE rule426 #-}
   rule426 = \ ((_hdIfdps) :: AttrOrderMap) ((_tlIfdps) :: AttrOrderMap) ->
     (Map.union _hdIfdps _tlIfdps)
   {-# INLINE rule427 #-}
   rule427 = \ ((_hdIfieldMap) :: FMap) ((_tlIfieldMap) :: FMap) ->
     ((Map.union) _hdIfieldMap _tlIfieldMap)
   {-# INLINE rule428 #-}
   rule428 = \ ((_hdIfsInP) :: FsInP) ((_tlIfsInP) :: FsInP) ->
     ((Map.union) _hdIfsInP _tlIfsInP)
   {-# INLINE rule429 #-}
   rule429 = \ ((_hdIfty) :: FTY) ((_tlIfty) :: FTY) ->
     (Map.union _hdIfty _tlIfty)
   {-# INLINE rule430 #-}
   rule430 = \ ((_hdIgen) :: Map Int Int) ((_tlIgen) :: Map Int Int) ->
     (Map.union _hdIgen _tlIgen)
   {-# INLINE rule431 #-}
   rule431 = \ ((_hdIhoMap) :: HOMap) ((_tlIhoMap) :: HOMap) ->
     ((Map.union) _hdIhoMap _tlIhoMap)
   {-# INLINE rule432 #-}
   rule432 = \ ((_hdIinhmap) :: Map.Map NontermIdent Attributes) ((_tlIinhmap) :: Map.Map NontermIdent Attributes) ->
     _hdIinhmap `Map.union` _tlIinhmap
   {-# INLINE rule433 #-}
   rule433 = \ ((_hdIinhs) :: AI_N) ((_tlIinhs) :: AI_N) ->
     (Map.union _hdIinhs _tlIinhs)
   {-# INLINE rule434 #-}
   rule434 = \ ((_hdIinss) :: Map Int [Int]) ((_tlIinss) :: Map Int [Int]) ->
     (Map.unionWith (++) _hdIinss _tlIinss)
   {-# INLINE rule435 #-}
   rule435 = \ ((_hdIlfp) :: SF_P) ((_tlIlfp) :: SF_P) ->
     (Map.unionWith (Set.union) _hdIlfp _tlIlfp)
   {-# INLINE rule436 #-}
   rule436 = \ ((_hdIlfpr) :: SF_P) ((_tlIlfpr) :: SF_P) ->
     (Map.unionWith (Set.union) _hdIlfpr _tlIlfpr)
   {-# INLINE rule437 #-}
   rule437 = \ ((_hdIlocalSigMap) :: Map.Map NontermIdent (Map.Map ConstructorIdent (Map.Map Identifier Type))) ((_tlIlocalSigMap) :: Map.Map NontermIdent (Map.Map ConstructorIdent (Map.Map Identifier Type))) ->
     _hdIlocalSigMap `Map.union` _tlIlocalSigMap
   {-# INLINE rule438 #-}
   rule438 = \ ((_hdIntDeps) :: Map NontermIdent (Set NontermIdent)) ((_tlIntDeps) :: Map NontermIdent (Set NontermIdent)) ->
     _hdIntDeps `mappend` _tlIntDeps
   {-# INLINE rule439 #-}
   rule439 = \ ((_hdIntHoDeps) :: Map NontermIdent (Set NontermIdent)) ((_tlIntHoDeps) :: Map NontermIdent (Set NontermIdent)) ->
     _hdIntHoDeps `mappend` _tlIntHoDeps
   {-# INLINE rule440 #-}
   rule440 = \ ((_hdIofld) :: [(Int, Int)]) ((_tlIofld) :: [(Int, Int)]) ->
     ((++) _hdIofld _tlIofld)
   {-# INLINE rule441 #-}
   rule441 = \ ((_hdIpmp) :: PMP) ((_tlIpmp) :: PMP) ->
     (Map.union _hdIpmp _tlIpmp)
   {-# INLINE rule442 #-}
   rule442 = \ ((_hdIpmpr) :: PMP_R) ((_tlIpmpr) :: PMP_R) ->
     (Map.union _hdIpmpr _tlIpmpr)
   {-# INLINE rule443 #-}
   rule443 = \ ((_hdIps) :: [PLabel]) ((_tlIps) :: [PLabel]) ->
     ((++) _hdIps _tlIps)
   {-# INLINE rule444 #-}
   rule444 = \ ((_hdIruleMap) :: Map.Map MyOccurrence Identifier) ((_tlIruleMap) :: Map.Map MyOccurrence Identifier) ->
     (Map.union _hdIruleMap _tlIruleMap)
   {-# INLINE rule445 #-}
   rule445 = \ ((_hdIsfp) :: SF_P) ((_tlIsfp) :: SF_P) ->
     (Map.unionWith (Set.union) _hdIsfp _tlIsfp)
   {-# INLINE rule446 #-}
   rule446 = \ ((_hdIsynmap) :: Map.Map NontermIdent Attributes) ((_tlIsynmap) :: Map.Map NontermIdent Attributes) ->
     _hdIsynmap `Map.union` _tlIsynmap
   {-# INLINE rule447 #-}
   rule447 = \ ((_hdIsyns) :: AS_N) ((_tlIsyns) :: AS_N) ->
     (Map.union _hdIsyns _tlIsyns)
   {-# INLINE rule448 #-}
   rule448 = \ ((_hdIvisMap) :: IMap.IntMap Int) ((_tlIvisMap) :: IMap.IntMap Int) ->
     (IMap.union _hdIvisMap _tlIvisMap)
   {-# INLINE rule449 #-}
   rule449 = \ ((_hdIself) :: Nonterminal) ((_tlIself) :: Nonterminals) ->
     (:) _hdIself _tlIself
   {-# INLINE rule450 #-}
   rule450 = \ _self ->
     _self
   {-# INLINE rule451 #-}
   rule451 = \ ((_tlIflab) :: Int) ->
     _tlIflab
   {-# INLINE rule452 #-}
   rule452 = \ ((_tlIolab) :: Int) ->
     _tlIolab
   {-# INLINE rule453 #-}
   rule453 = \ ((_tlIrulenumber) :: Int) ->
     _tlIrulenumber
   {-# INLINE rule454 #-}
   rule454 = \ ((_tlIvisitnum) :: Int) ->
     _tlIvisitnum
   {-# INLINE rule455 #-}
   rule455 = \ ((_lhsIain) :: MyType -> MyAttributes) ->
     _lhsIain
   {-# INLINE rule456 #-}
   rule456 = \ ((_lhsIan) :: MyType -> MyAttributes) ->
     _lhsIan
   {-# INLINE rule457 #-}
   rule457 = \ ((_lhsIaroundMap) :: Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) ->
     _lhsIaroundMap
   {-# INLINE rule458 #-}
   rule458 = \ ((_lhsIasn) :: MyType -> MyAttributes) ->
     _lhsIasn
   {-# INLINE rule459 #-}
   rule459 = \ ((_lhsIaugM) :: Map.Map Identifier (Map.Map Identifier (Set.Set Dependency))) ->
     _lhsIaugM
   {-# INLINE rule460 #-}
   rule460 = \ ((_lhsIclassContexts) :: ContextMap) ->
     _lhsIclassContexts
   {-# INLINE rule461 #-}
   rule461 = \ ((_lhsIclosedHoNtDeps) :: Map NontermIdent (Set NontermIdent)) ->
     _lhsIclosedHoNtDeps
   {-# INLINE rule462 #-}
   rule462 = \ ((_lhsIclosedHoNtRevDeps) :: Map NontermIdent (Set NontermIdent)) ->
     _lhsIclosedHoNtRevDeps
   {-# INLINE rule463 #-}
   rule463 = \ ((_lhsIclosedNtDeps) :: Map NontermIdent (Set NontermIdent)) ->
     _lhsIclosedNtDeps
   {-# INLINE rule464 #-}
   rule464 = \ ((_lhsIflab) :: Int) ->
     _lhsIflab
   {-# INLINE rule465 #-}
   rule465 = \ ((_lhsIfty) :: FTY) ->
     _lhsIfty
   {-# INLINE rule466 #-}
   rule466 = \ ((_lhsIftyf) :: FTY) ->
     _lhsIftyf
   {-# INLINE rule467 #-}
   rule467 = \ ((_lhsIhoMapf) :: HOMap) ->
     _lhsIhoMapf
   {-# INLINE rule468 #-}
   rule468 = \ ((_lhsIlfpf) :: SF_P) ->
     _lhsIlfpf
   {-# INLINE rule469 #-}
   rule469 = \ ((_lhsImergeMap) :: Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier, [Identifier], Expression)))) ->
     _lhsImergeMap
   {-# INLINE rule470 #-}
   rule470 = \ ((_lhsInmp) :: NMP) ->
     _lhsInmp
   {-# INLINE rule471 #-}
   rule471 = \ ((_lhsInmprf) :: NMP_R) ->
     _lhsInmprf
   {-# INLINE rule472 #-}
   rule472 = \ ((_lhsIolab) :: Int) ->
     _lhsIolab
   {-# INLINE rule473 #-}
   rule473 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule474 #-}
   rule474 = \ ((_lhsIpmpf) :: PMP) ->
     _lhsIpmpf
   {-# INLINE rule475 #-}
   rule475 = \ ((_lhsIpmprf) :: PMP_R) ->
     _lhsIpmprf
   {-# INLINE rule476 #-}
   rule476 = \ ((_lhsIres_ads) :: [Edge]) ->
     _lhsIres_ads
   {-# INLINE rule477 #-}
   rule477 = \ ((_lhsIrulenumber) :: Int) ->
     _lhsIrulenumber
   {-# INLINE rule478 #-}
   rule478 = \ ((_lhsIsched) :: InterfaceRes) ->
     _lhsIsched
   {-# INLINE rule479 #-}
   rule479 = \ ((_lhsItdp) :: TDPRes) ->
     _lhsItdp
   {-# INLINE rule480 #-}
   rule480 = \ ((_lhsIvisMapf) :: IMap.IntMap Int) ->
     _lhsIvisMapf
   {-# INLINE rule481 #-}
   rule481 = \ ((_lhsIvisitnum) :: Int) ->
     _lhsIvisitnum
   {-# INLINE rule482 #-}
   rule482 = \ ((_lhsIain) :: MyType -> MyAttributes) ->
     _lhsIain
   {-# INLINE rule483 #-}
   rule483 = \ ((_lhsIan) :: MyType -> MyAttributes) ->
     _lhsIan
   {-# INLINE rule484 #-}
   rule484 = \ ((_lhsIaroundMap) :: Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) ->
     _lhsIaroundMap
   {-# INLINE rule485 #-}
   rule485 = \ ((_lhsIasn) :: MyType -> MyAttributes) ->
     _lhsIasn
   {-# INLINE rule486 #-}
   rule486 = \ ((_lhsIaugM) :: Map.Map Identifier (Map.Map Identifier (Set.Set Dependency))) ->
     _lhsIaugM
   {-# INLINE rule487 #-}
   rule487 = \ ((_lhsIclassContexts) :: ContextMap) ->
     _lhsIclassContexts
   {-# INLINE rule488 #-}
   rule488 = \ ((_lhsIclosedHoNtDeps) :: Map NontermIdent (Set NontermIdent)) ->
     _lhsIclosedHoNtDeps
   {-# INLINE rule489 #-}
   rule489 = \ ((_lhsIclosedHoNtRevDeps) :: Map NontermIdent (Set NontermIdent)) ->
     _lhsIclosedHoNtRevDeps
   {-# INLINE rule490 #-}
   rule490 = \ ((_lhsIclosedNtDeps) :: Map NontermIdent (Set NontermIdent)) ->
     _lhsIclosedNtDeps
   {-# INLINE rule491 #-}
   rule491 = \ ((_hdIflab) :: Int) ->
     _hdIflab
   {-# INLINE rule492 #-}
   rule492 = \ ((_hdIfty) :: FTY) ->
     _hdIfty
   {-# INLINE rule493 #-}
   rule493 = \ ((_lhsIftyf) :: FTY) ->
     _lhsIftyf
   {-# INLINE rule494 #-}
   rule494 = \ ((_lhsIhoMapf) :: HOMap) ->
     _lhsIhoMapf
   {-# INLINE rule495 #-}
   rule495 = \ ((_lhsIlfpf) :: SF_P) ->
     _lhsIlfpf
   {-# INLINE rule496 #-}
   rule496 = \ ((_lhsImergeMap) :: Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier, [Identifier], Expression)))) ->
     _lhsImergeMap
   {-# INLINE rule497 #-}
   rule497 = \ ((_lhsInmp) :: NMP) ->
     _lhsInmp
   {-# INLINE rule498 #-}
   rule498 = \ ((_lhsInmprf) :: NMP_R) ->
     _lhsInmprf
   {-# INLINE rule499 #-}
   rule499 = \ ((_hdIolab) :: Int) ->
     _hdIolab
   {-# INLINE rule500 #-}
   rule500 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule501 #-}
   rule501 = \ ((_lhsIpmpf) :: PMP) ->
     _lhsIpmpf
   {-# INLINE rule502 #-}
   rule502 = \ ((_lhsIpmprf) :: PMP_R) ->
     _lhsIpmprf
   {-# INLINE rule503 #-}
   rule503 = \ ((_lhsIres_ads) :: [Edge]) ->
     _lhsIres_ads
   {-# INLINE rule504 #-}
   rule504 = \ ((_hdIrulenumber) :: Int) ->
     _hdIrulenumber
   {-# INLINE rule505 #-}
   rule505 = \ ((_lhsIsched) :: InterfaceRes) ->
     _lhsIsched
   {-# INLINE rule506 #-}
   rule506 = \ ((_lhsItdp) :: TDPRes) ->
     _lhsItdp
   {-# INLINE rule507 #-}
   rule507 = \ ((_lhsIvisMapf) :: IMap.IntMap Int) ->
     _lhsIvisMapf
   {-# INLINE rule508 #-}
   rule508 = \ ((_hdIvisitnum) :: Int) ->
     _hdIvisitnum
{-# NOINLINE sem_Nonterminals_Nil #-}
sem_Nonterminals_Nil ::  T_Nonterminals 
sem_Nonterminals_Nil  = T_Nonterminals (return st74) where
   {-# NOINLINE st74 #-}
   st74 = let
      v73 :: T_Nonterminals_v73 
      v73 = \ (T_Nonterminals_vIn73 _lhsIain _lhsIan _lhsIaroundMap _lhsIasn _lhsIaugM _lhsIclassContexts _lhsIclosedHoNtDeps _lhsIclosedHoNtRevDeps _lhsIclosedNtDeps _lhsIflab _lhsIfty _lhsIftyf _lhsIhoMapf _lhsIlfpf _lhsImergeMap _lhsInmp _lhsInmprf _lhsIolab _lhsIoptions _lhsIpmpf _lhsIpmprf _lhsIres_ads _lhsIrulenumber _lhsIsched _lhsItdp _lhsIvisMapf _lhsIvisitnum) -> ( let
         _lhsOads :: [Edge]
         _lhsOads = rule509  ()
         _lhsOap :: A_P
         _lhsOap = rule510  ()
         _lhsOenonts :: ENonterminals
         _lhsOenonts = rule511  ()
         _lhsOfdps :: AttrOrderMap
         _lhsOfdps = rule512  ()
         _lhsOfieldMap :: FMap
         _lhsOfieldMap = rule513  ()
         _lhsOfsInP :: FsInP
         _lhsOfsInP = rule514  ()
         _lhsOfty :: FTY
         _lhsOfty = rule515  ()
         _lhsOgen :: Map Int Int
         _lhsOgen = rule516  ()
         _lhsOhoMap :: HOMap
         _lhsOhoMap = rule517  ()
         _lhsOinhmap :: Map.Map NontermIdent Attributes
         _lhsOinhmap = rule518  ()
         _lhsOinhs :: AI_N
         _lhsOinhs = rule519  ()
         _lhsOinss :: Map Int [Int]
         _lhsOinss = rule520  ()
         _lhsOlfp :: SF_P
         _lhsOlfp = rule521  ()
         _lhsOlfpr :: SF_P
         _lhsOlfpr = rule522  ()
         _lhsOlocalSigMap :: Map.Map NontermIdent (Map.Map ConstructorIdent (Map.Map Identifier Type))
         _lhsOlocalSigMap = rule523  ()
         _lhsOntDeps :: Map NontermIdent (Set NontermIdent)
         _lhsOntDeps = rule524  ()
         _lhsOntHoDeps :: Map NontermIdent (Set NontermIdent)
         _lhsOntHoDeps = rule525  ()
         _lhsOofld :: [(Int, Int)]
         _lhsOofld = rule526  ()
         _lhsOpmp :: PMP
         _lhsOpmp = rule527  ()
         _lhsOpmpr :: PMP_R
         _lhsOpmpr = rule528  ()
         _lhsOps :: [PLabel]
         _lhsOps = rule529  ()
         _lhsOruleMap :: Map.Map MyOccurrence Identifier
         _lhsOruleMap = rule530  ()
         _lhsOsfp :: SF_P
         _lhsOsfp = rule531  ()
         _lhsOsynmap :: Map.Map NontermIdent Attributes
         _lhsOsynmap = rule532  ()
         _lhsOsyns :: AS_N
         _lhsOsyns = rule533  ()
         _lhsOvisMap :: IMap.IntMap Int
         _lhsOvisMap = rule534  ()
         _self = rule535  ()
         _lhsOself :: Nonterminals
         _lhsOself = rule536 _self
         _lhsOflab :: Int
         _lhsOflab = rule537 _lhsIflab
         _lhsOolab :: Int
         _lhsOolab = rule538 _lhsIolab
         _lhsOrulenumber :: Int
         _lhsOrulenumber = rule539 _lhsIrulenumber
         _lhsOvisitnum :: Int
         _lhsOvisitnum = rule540 _lhsIvisitnum
         __result_ = T_Nonterminals_vOut73 _lhsOads _lhsOap _lhsOenonts _lhsOfdps _lhsOfieldMap _lhsOflab _lhsOfsInP _lhsOfty _lhsOgen _lhsOhoMap _lhsOinhmap _lhsOinhs _lhsOinss _lhsOlfp _lhsOlfpr _lhsOlocalSigMap _lhsOntDeps _lhsOntHoDeps _lhsOofld _lhsOolab _lhsOpmp _lhsOpmpr _lhsOps _lhsOruleMap _lhsOrulenumber _lhsOself _lhsOsfp _lhsOsynmap _lhsOsyns _lhsOvisMap _lhsOvisitnum
         in __result_ )
     in C_Nonterminals_s74 v73
   {-# INLINE rule509 #-}
   rule509 = \  (_ :: ()) ->
     []
   {-# INLINE rule510 #-}
   rule510 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule511 #-}
   rule511 = \  (_ :: ()) ->
     []
   {-# INLINE rule512 #-}
   rule512 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule513 #-}
   rule513 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule514 #-}
   rule514 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule515 #-}
   rule515 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule516 #-}
   rule516 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule517 #-}
   rule517 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule518 #-}
   rule518 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule519 #-}
   rule519 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule520 #-}
   rule520 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule521 #-}
   rule521 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule522 #-}
   rule522 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule523 #-}
   rule523 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule524 #-}
   rule524 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule525 #-}
   rule525 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule526 #-}
   rule526 = \  (_ :: ()) ->
     []
   {-# INLINE rule527 #-}
   rule527 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule528 #-}
   rule528 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule529 #-}
   rule529 = \  (_ :: ()) ->
     ([])
   {-# INLINE rule530 #-}
   rule530 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule531 #-}
   rule531 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule532 #-}
   rule532 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule533 #-}
   rule533 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule534 #-}
   rule534 = \  (_ :: ()) ->
     IMap.empty
   {-# INLINE rule535 #-}
   rule535 = \  (_ :: ()) ->
     []
   {-# INLINE rule536 #-}
   rule536 = \ _self ->
     _self
   {-# INLINE rule537 #-}
   rule537 = \ ((_lhsIflab) :: Int) ->
     _lhsIflab
   {-# INLINE rule538 #-}
   rule538 = \ ((_lhsIolab) :: Int) ->
     _lhsIolab
   {-# INLINE rule539 #-}
   rule539 = \ ((_lhsIrulenumber) :: Int) ->
     _lhsIrulenumber
   {-# INLINE rule540 #-}
   rule540 = \ ((_lhsIvisitnum) :: Int) ->
     _lhsIvisitnum

-- Pattern -----------------------------------------------------
-- wrapper
data Inh_Pattern  = Inh_Pattern {  }
data Syn_Pattern  = Syn_Pattern { afs_Syn_Pattern :: ([(FLabel, ALabel, Bool)]), copy_Syn_Pattern :: (Pattern), self_Syn_Pattern :: (Pattern) }
{-# INLINABLE wrap_Pattern #-}
wrap_Pattern :: T_Pattern  -> Inh_Pattern  -> (Syn_Pattern )
wrap_Pattern (T_Pattern act) (Inh_Pattern ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg76 = T_Pattern_vIn76 
        (T_Pattern_vOut76 _lhsOafs _lhsOcopy _lhsOself) <- return (inv_Pattern_s77 sem arg76)
        return (Syn_Pattern _lhsOafs _lhsOcopy _lhsOself)
   )

-- cata
{-# NOINLINE sem_Pattern #-}
sem_Pattern :: Pattern  -> T_Pattern 
sem_Pattern ( Constr name_ pats_ ) = sem_Pattern_Constr name_ ( sem_Patterns pats_ )
sem_Pattern ( Product pos_ pats_ ) = sem_Pattern_Product pos_ ( sem_Patterns pats_ )
sem_Pattern ( Alias field_ attr_ pat_ ) = sem_Pattern_Alias field_ attr_ ( sem_Pattern pat_ )
sem_Pattern ( Irrefutable pat_ ) = sem_Pattern_Irrefutable ( sem_Pattern pat_ )
sem_Pattern ( Underscore pos_ ) = sem_Pattern_Underscore pos_

-- semantic domain
newtype T_Pattern  = T_Pattern {
                               attach_T_Pattern :: Identity (T_Pattern_s77 )
                               }
newtype T_Pattern_s77  = C_Pattern_s77 {
                                       inv_Pattern_s77 :: (T_Pattern_v76 )
                                       }
data T_Pattern_s78  = C_Pattern_s78
type T_Pattern_v76  = (T_Pattern_vIn76 ) -> (T_Pattern_vOut76 )
data T_Pattern_vIn76  = T_Pattern_vIn76 
data T_Pattern_vOut76  = T_Pattern_vOut76 ([(FLabel, ALabel, Bool)]) (Pattern) (Pattern)
{-# NOINLINE sem_Pattern_Constr #-}
sem_Pattern_Constr :: (ConstructorIdent) -> T_Patterns  -> T_Pattern 
sem_Pattern_Constr arg_name_ arg_pats_ = T_Pattern (return st77) where
   {-# NOINLINE st77 #-}
   st77 = let
      v76 :: T_Pattern_v76 
      v76 = \ (T_Pattern_vIn76 ) -> ( let
         _patsX80 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_pats_))
         (T_Patterns_vOut79 _patsIafs _patsIcopy _patsIself) = inv_Patterns_s80 _patsX80 (T_Patterns_vIn79 )
         _lhsOafs :: [(FLabel, ALabel, Bool)]
         _lhsOafs = rule541 _patsIafs
         _copy = rule542 _patsIcopy arg_name_
         _self = rule543 _patsIself arg_name_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule544 _copy
         _lhsOself :: Pattern
         _lhsOself = rule545 _self
         __result_ = T_Pattern_vOut76 _lhsOafs _lhsOcopy _lhsOself
         in __result_ )
     in C_Pattern_s77 v76
   {-# INLINE rule541 #-}
   rule541 = \ ((_patsIafs) :: [(FLabel, ALabel, Bool)]) ->
     _patsIafs
   {-# INLINE rule542 #-}
   rule542 = \ ((_patsIcopy) :: Patterns) name_ ->
     Constr name_ _patsIcopy
   {-# INLINE rule543 #-}
   rule543 = \ ((_patsIself) :: Patterns) name_ ->
     Constr name_ _patsIself
   {-# INLINE rule544 #-}
   rule544 = \ _copy ->
     _copy
   {-# INLINE rule545 #-}
   rule545 = \ _self ->
     _self
{-# NOINLINE sem_Pattern_Product #-}
sem_Pattern_Product :: (Pos) -> T_Patterns  -> T_Pattern 
sem_Pattern_Product arg_pos_ arg_pats_ = T_Pattern (return st77) where
   {-# NOINLINE st77 #-}
   st77 = let
      v76 :: T_Pattern_v76 
      v76 = \ (T_Pattern_vIn76 ) -> ( let
         _patsX80 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_pats_))
         (T_Patterns_vOut79 _patsIafs _patsIcopy _patsIself) = inv_Patterns_s80 _patsX80 (T_Patterns_vIn79 )
         _lhsOafs :: [(FLabel, ALabel, Bool)]
         _lhsOafs = rule546 _patsIafs
         _copy = rule547 _patsIcopy arg_pos_
         _self = rule548 _patsIself arg_pos_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule549 _copy
         _lhsOself :: Pattern
         _lhsOself = rule550 _self
         __result_ = T_Pattern_vOut76 _lhsOafs _lhsOcopy _lhsOself
         in __result_ )
     in C_Pattern_s77 v76
   {-# INLINE rule546 #-}
   rule546 = \ ((_patsIafs) :: [(FLabel, ALabel, Bool)]) ->
     _patsIafs
   {-# INLINE rule547 #-}
   rule547 = \ ((_patsIcopy) :: Patterns) pos_ ->
     Product pos_ _patsIcopy
   {-# INLINE rule548 #-}
   rule548 = \ ((_patsIself) :: Patterns) pos_ ->
     Product pos_ _patsIself
   {-# INLINE rule549 #-}
   rule549 = \ _copy ->
     _copy
   {-# INLINE rule550 #-}
   rule550 = \ _self ->
     _self
{-# NOINLINE sem_Pattern_Alias #-}
sem_Pattern_Alias :: (Identifier) -> (Identifier) -> T_Pattern  -> T_Pattern 
sem_Pattern_Alias arg_field_ arg_attr_ arg_pat_ = T_Pattern (return st77) where
   {-# NOINLINE st77 #-}
   st77 = let
      v76 :: T_Pattern_v76 
      v76 = \ (T_Pattern_vIn76 ) -> ( let
         _patX77 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pat_))
         (T_Pattern_vOut76 _patIafs _patIcopy _patIself) = inv_Pattern_s77 _patX77 (T_Pattern_vIn76 )
         _lhsOafs :: [(FLabel, ALabel, Bool)]
         _lhsOafs = rule551 _patIafs arg_attr_ arg_field_
         _copy = rule552 _patIcopy arg_attr_ arg_field_
         _self = rule553 _patIself arg_attr_ arg_field_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule554 _copy
         _lhsOself :: Pattern
         _lhsOself = rule555 _self
         __result_ = T_Pattern_vOut76 _lhsOafs _lhsOcopy _lhsOself
         in __result_ )
     in C_Pattern_s77 v76
   {-# INLINE rule551 #-}
   {-# LINE 260 "src-ag/LOAG/Prepare.ag" #-}
   rule551 = \ ((_patIafs) :: [(FLabel, ALabel, Bool)]) attr_ field_ ->
                {-# LINE 260 "src-ag/LOAG/Prepare.ag" #-}
                let isLocal = (field_ == _LOC || field_ == _INST)
                 in [(getName field_, (getName attr_, dlhs field_),
                      isLocal)] ++ _patIafs
                {-# LINE 4548 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule552 #-}
   rule552 = \ ((_patIcopy) :: Pattern) attr_ field_ ->
     Alias field_ attr_ _patIcopy
   {-# INLINE rule553 #-}
   rule553 = \ ((_patIself) :: Pattern) attr_ field_ ->
     Alias field_ attr_ _patIself
   {-# INLINE rule554 #-}
   rule554 = \ _copy ->
     _copy
   {-# INLINE rule555 #-}
   rule555 = \ _self ->
     _self
{-# NOINLINE sem_Pattern_Irrefutable #-}
sem_Pattern_Irrefutable :: T_Pattern  -> T_Pattern 
sem_Pattern_Irrefutable arg_pat_ = T_Pattern (return st77) where
   {-# NOINLINE st77 #-}
   st77 = let
      v76 :: T_Pattern_v76 
      v76 = \ (T_Pattern_vIn76 ) -> ( let
         _patX77 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pat_))
         (T_Pattern_vOut76 _patIafs _patIcopy _patIself) = inv_Pattern_s77 _patX77 (T_Pattern_vIn76 )
         _lhsOafs :: [(FLabel, ALabel, Bool)]
         _lhsOafs = rule556 _patIafs
         _copy = rule557 _patIcopy
         _self = rule558 _patIself
         _lhsOcopy :: Pattern
         _lhsOcopy = rule559 _copy
         _lhsOself :: Pattern
         _lhsOself = rule560 _self
         __result_ = T_Pattern_vOut76 _lhsOafs _lhsOcopy _lhsOself
         in __result_ )
     in C_Pattern_s77 v76
   {-# INLINE rule556 #-}
   rule556 = \ ((_patIafs) :: [(FLabel, ALabel, Bool)]) ->
     _patIafs
   {-# INLINE rule557 #-}
   rule557 = \ ((_patIcopy) :: Pattern) ->
     Irrefutable _patIcopy
   {-# INLINE rule558 #-}
   rule558 = \ ((_patIself) :: Pattern) ->
     Irrefutable _patIself
   {-# INLINE rule559 #-}
   rule559 = \ _copy ->
     _copy
   {-# INLINE rule560 #-}
   rule560 = \ _self ->
     _self
{-# NOINLINE sem_Pattern_Underscore #-}
sem_Pattern_Underscore :: (Pos) -> T_Pattern 
sem_Pattern_Underscore arg_pos_ = T_Pattern (return st77) where
   {-# NOINLINE st77 #-}
   st77 = let
      v76 :: T_Pattern_v76 
      v76 = \ (T_Pattern_vIn76 ) -> ( let
         _lhsOafs :: [(FLabel, ALabel, Bool)]
         _lhsOafs = rule561  ()
         _copy = rule562 arg_pos_
         _self = rule563 arg_pos_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule564 _copy
         _lhsOself :: Pattern
         _lhsOself = rule565 _self
         __result_ = T_Pattern_vOut76 _lhsOafs _lhsOcopy _lhsOself
         in __result_ )
     in C_Pattern_s77 v76
   {-# INLINE rule561 #-}
   rule561 = \  (_ :: ()) ->
     []
   {-# INLINE rule562 #-}
   rule562 = \ pos_ ->
     Underscore pos_
   {-# INLINE rule563 #-}
   rule563 = \ pos_ ->
     Underscore pos_
   {-# INLINE rule564 #-}
   rule564 = \ _copy ->
     _copy
   {-# INLINE rule565 #-}
   rule565 = \ _self ->
     _self

-- Patterns ----------------------------------------------------
-- wrapper
data Inh_Patterns  = Inh_Patterns {  }
data Syn_Patterns  = Syn_Patterns { afs_Syn_Patterns :: ([(FLabel, ALabel, Bool)]), copy_Syn_Patterns :: (Patterns), self_Syn_Patterns :: (Patterns) }
{-# INLINABLE wrap_Patterns #-}
wrap_Patterns :: T_Patterns  -> Inh_Patterns  -> (Syn_Patterns )
wrap_Patterns (T_Patterns act) (Inh_Patterns ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg79 = T_Patterns_vIn79 
        (T_Patterns_vOut79 _lhsOafs _lhsOcopy _lhsOself) <- return (inv_Patterns_s80 sem arg79)
        return (Syn_Patterns _lhsOafs _lhsOcopy _lhsOself)
   )

-- cata
{-# NOINLINE sem_Patterns #-}
sem_Patterns :: Patterns  -> T_Patterns 
sem_Patterns list = Prelude.foldr sem_Patterns_Cons sem_Patterns_Nil (Prelude.map sem_Pattern list)

-- semantic domain
newtype T_Patterns  = T_Patterns {
                                 attach_T_Patterns :: Identity (T_Patterns_s80 )
                                 }
newtype T_Patterns_s80  = C_Patterns_s80 {
                                         inv_Patterns_s80 :: (T_Patterns_v79 )
                                         }
data T_Patterns_s81  = C_Patterns_s81
type T_Patterns_v79  = (T_Patterns_vIn79 ) -> (T_Patterns_vOut79 )
data T_Patterns_vIn79  = T_Patterns_vIn79 
data T_Patterns_vOut79  = T_Patterns_vOut79 ([(FLabel, ALabel, Bool)]) (Patterns) (Patterns)
{-# NOINLINE sem_Patterns_Cons #-}
sem_Patterns_Cons :: T_Pattern  -> T_Patterns  -> T_Patterns 
sem_Patterns_Cons arg_hd_ arg_tl_ = T_Patterns (return st80) where
   {-# NOINLINE st80 #-}
   st80 = let
      v79 :: T_Patterns_v79 
      v79 = \ (T_Patterns_vIn79 ) -> ( let
         _hdX77 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_hd_))
         _tlX80 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_tl_))
         (T_Pattern_vOut76 _hdIafs _hdIcopy _hdIself) = inv_Pattern_s77 _hdX77 (T_Pattern_vIn76 )
         (T_Patterns_vOut79 _tlIafs _tlIcopy _tlIself) = inv_Patterns_s80 _tlX80 (T_Patterns_vIn79 )
         _lhsOafs :: [(FLabel, ALabel, Bool)]
         _lhsOafs = rule566 _hdIafs _tlIafs
         _copy = rule567 _hdIcopy _tlIcopy
         _self = rule568 _hdIself _tlIself
         _lhsOcopy :: Patterns
         _lhsOcopy = rule569 _copy
         _lhsOself :: Patterns
         _lhsOself = rule570 _self
         __result_ = T_Patterns_vOut79 _lhsOafs _lhsOcopy _lhsOself
         in __result_ )
     in C_Patterns_s80 v79
   {-# INLINE rule566 #-}
   rule566 = \ ((_hdIafs) :: [(FLabel, ALabel, Bool)]) ((_tlIafs) :: [(FLabel, ALabel, Bool)]) ->
     _hdIafs ++ _tlIafs
   {-# INLINE rule567 #-}
   rule567 = \ ((_hdIcopy) :: Pattern) ((_tlIcopy) :: Patterns) ->
     (:) _hdIcopy _tlIcopy
   {-# INLINE rule568 #-}
   rule568 = \ ((_hdIself) :: Pattern) ((_tlIself) :: Patterns) ->
     (:) _hdIself _tlIself
   {-# INLINE rule569 #-}
   rule569 = \ _copy ->
     _copy
   {-# INLINE rule570 #-}
   rule570 = \ _self ->
     _self
{-# NOINLINE sem_Patterns_Nil #-}
sem_Patterns_Nil ::  T_Patterns 
sem_Patterns_Nil  = T_Patterns (return st80) where
   {-# NOINLINE st80 #-}
   st80 = let
      v79 :: T_Patterns_v79 
      v79 = \ (T_Patterns_vIn79 ) -> ( let
         _lhsOafs :: [(FLabel, ALabel, Bool)]
         _lhsOafs = rule571  ()
         _copy = rule572  ()
         _self = rule573  ()
         _lhsOcopy :: Patterns
         _lhsOcopy = rule574 _copy
         _lhsOself :: Patterns
         _lhsOself = rule575 _self
         __result_ = T_Patterns_vOut79 _lhsOafs _lhsOcopy _lhsOself
         in __result_ )
     in C_Patterns_s80 v79
   {-# INLINE rule571 #-}
   rule571 = \  (_ :: ()) ->
     []
   {-# INLINE rule572 #-}
   rule572 = \  (_ :: ()) ->
     []
   {-# INLINE rule573 #-}
   rule573 = \  (_ :: ()) ->
     []
   {-# INLINE rule574 #-}
   rule574 = \ _copy ->
     _copy
   {-# INLINE rule575 #-}
   rule575 = \ _self ->
     _self

-- Production --------------------------------------------------
-- wrapper
data Inh_Production  = Inh_Production { ain_Inh_Production :: (MyType -> MyAttributes), an_Inh_Production :: (MyType -> MyAttributes), aroundMap_Inh_Production :: (Map ConstructorIdent (Map Identifier [Expression])), asn_Inh_Production :: (MyType -> MyAttributes), augM_Inh_Production :: (Map.Map Identifier (Set.Set Dependency)), dty_Inh_Production :: (MyType), flab_Inh_Production :: (Int), fty_Inh_Production :: (FTY), ftyf_Inh_Production :: (FTY), hoMapf_Inh_Production :: (HOMap), lfpf_Inh_Production :: (SF_P), mergeMap_Inh_Production :: (Map ConstructorIdent (Map Identifier (Identifier, [Identifier], Expression))), mysegments_Inh_Production :: (MySegments), nmp_Inh_Production :: (NMP), nmprf_Inh_Production :: (NMP_R), olab_Inh_Production :: (Int), options_Inh_Production :: (Options), pmpf_Inh_Production :: (PMP), pmprf_Inh_Production :: (PMP_R), res_ads_Inh_Production :: ([Edge]), rulenumber_Inh_Production :: (Int), tdp_Inh_Production :: (TDPRes), visMapf_Inh_Production :: (IMap.IntMap Int), visitnum_Inh_Production :: (Int) }
data Syn_Production  = Syn_Production { ads_Syn_Production :: ([Edge]), ap_Syn_Production :: (A_P), eprods_Syn_Production :: (EProductions), fdps_Syn_Production :: (Map.Map ConstructorIdent (Set Dependency)), fieldMap_Syn_Production :: (FMap), flab_Syn_Production :: (Int), fsInP_Syn_Production :: (FsInP), fty_Syn_Production :: (FTY), gen_Syn_Production :: (Map Int Int), hoMap_Syn_Production :: (HOMap), inss_Syn_Production :: (Map Int [Int]), lfp_Syn_Production :: (SF_P), lfpr_Syn_Production :: (SF_P), localSigMap_Syn_Production :: (Map.Map ConstructorIdent (Map.Map Identifier Type)), ofld_Syn_Production :: ([(Int, Int)]), olab_Syn_Production :: (Int), pmp_Syn_Production :: (PMP), pmpr_Syn_Production :: (PMP_R), ps_Syn_Production :: (PLabel), refHoNts_Syn_Production :: (Set NontermIdent), refNts_Syn_Production :: (Set NontermIdent), ruleMap_Syn_Production :: (Map.Map MyOccurrence Identifier), rulenumber_Syn_Production :: (Int), self_Syn_Production :: (Production), sfp_Syn_Production :: (SF_P), visitnum_Syn_Production :: (Int) }
{-# INLINABLE wrap_Production #-}
wrap_Production :: T_Production  -> Inh_Production  -> (Syn_Production )
wrap_Production (T_Production act) (Inh_Production _lhsIain _lhsIan _lhsIaroundMap _lhsIasn _lhsIaugM _lhsIdty _lhsIflab _lhsIfty _lhsIftyf _lhsIhoMapf _lhsIlfpf _lhsImergeMap _lhsImysegments _lhsInmp _lhsInmprf _lhsIolab _lhsIoptions _lhsIpmpf _lhsIpmprf _lhsIres_ads _lhsIrulenumber _lhsItdp _lhsIvisMapf _lhsIvisitnum) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg82 = T_Production_vIn82 _lhsIain _lhsIan _lhsIaroundMap _lhsIasn _lhsIaugM _lhsIdty _lhsIflab _lhsIfty _lhsIftyf _lhsIhoMapf _lhsIlfpf _lhsImergeMap _lhsImysegments _lhsInmp _lhsInmprf _lhsIolab _lhsIoptions _lhsIpmpf _lhsIpmprf _lhsIres_ads _lhsIrulenumber _lhsItdp _lhsIvisMapf _lhsIvisitnum
        (T_Production_vOut82 _lhsOads _lhsOap _lhsOeprods _lhsOfdps _lhsOfieldMap _lhsOflab _lhsOfsInP _lhsOfty _lhsOgen _lhsOhoMap _lhsOinss _lhsOlfp _lhsOlfpr _lhsOlocalSigMap _lhsOofld _lhsOolab _lhsOpmp _lhsOpmpr _lhsOps _lhsOrefHoNts _lhsOrefNts _lhsOruleMap _lhsOrulenumber _lhsOself _lhsOsfp _lhsOvisitnum) <- return (inv_Production_s83 sem arg82)
        return (Syn_Production _lhsOads _lhsOap _lhsOeprods _lhsOfdps _lhsOfieldMap _lhsOflab _lhsOfsInP _lhsOfty _lhsOgen _lhsOhoMap _lhsOinss _lhsOlfp _lhsOlfpr _lhsOlocalSigMap _lhsOofld _lhsOolab _lhsOpmp _lhsOpmpr _lhsOps _lhsOrefHoNts _lhsOrefNts _lhsOruleMap _lhsOrulenumber _lhsOself _lhsOsfp _lhsOvisitnum)
   )

-- cata
{-# INLINE sem_Production #-}
sem_Production :: Production  -> T_Production 
sem_Production ( Production con_ params_ constraints_ children_ rules_ typeSigs_ macro_  ) = sem_Production_Production con_ params_ constraints_ ( sem_Children children_ ) ( sem_Rules rules_ ) ( sem_TypeSigs typeSigs_ ) macro_ 

-- semantic domain
newtype T_Production  = T_Production {
                                     attach_T_Production :: Identity (T_Production_s83 )
                                     }
newtype T_Production_s83  = C_Production_s83 {
                                             inv_Production_s83 :: (T_Production_v82 )
                                             }
data T_Production_s84  = C_Production_s84
type T_Production_v82  = (T_Production_vIn82 ) -> (T_Production_vOut82 )
data T_Production_vIn82  = T_Production_vIn82 (MyType -> MyAttributes) (MyType -> MyAttributes) (Map ConstructorIdent (Map Identifier [Expression])) (MyType -> MyAttributes) (Map.Map Identifier (Set.Set Dependency)) (MyType) (Int) (FTY) (FTY) (HOMap) (SF_P) (Map ConstructorIdent (Map Identifier (Identifier, [Identifier], Expression))) (MySegments) (NMP) (NMP_R) (Int) (Options) (PMP) (PMP_R) ([Edge]) (Int) (TDPRes) (IMap.IntMap Int) (Int)
data T_Production_vOut82  = T_Production_vOut82 ([Edge]) (A_P) (EProductions) (Map.Map ConstructorIdent (Set Dependency)) (FMap) (Int) (FsInP) (FTY) (Map Int Int) (HOMap) (Map Int [Int]) (SF_P) (SF_P) (Map.Map ConstructorIdent (Map.Map Identifier Type)) ([(Int, Int)]) (Int) (PMP) (PMP_R) (PLabel) (Set NontermIdent) (Set NontermIdent) (Map.Map MyOccurrence Identifier) (Int) (Production) (SF_P) (Int)
{-# NOINLINE sem_Production_Production #-}
sem_Production_Production :: (ConstructorIdent) -> ([Identifier]) -> ([Type]) -> T_Children  -> T_Rules  -> T_TypeSigs  -> (MaybeMacro) ->  T_Production 
sem_Production_Production arg_con_ arg_params_ arg_constraints_ arg_children_ arg_rules_ arg_typeSigs_ arg_macro_  = T_Production (return st83) where
   {-# NOINLINE st83 #-}
   st83 = let
      v82 :: T_Production_v82 
      v82 = \ (T_Production_vIn82 _lhsIain _lhsIan _lhsIaroundMap _lhsIasn _lhsIaugM _lhsIdty _lhsIflab _lhsIfty _lhsIftyf _lhsIhoMapf _lhsIlfpf _lhsImergeMap _lhsImysegments _lhsInmp _lhsInmprf _lhsIolab _lhsIoptions _lhsIpmpf _lhsIpmprf _lhsIres_ads _lhsIrulenumber _lhsItdp _lhsIvisMapf _lhsIvisitnum) -> ( let
         _childrenX38 = Control.Monad.Identity.runIdentity (attach_T_Children (arg_children_))
         _rulesX92 = Control.Monad.Identity.runIdentity (attach_T_Rules (arg_rules_))
         _typeSigsX101 = Control.Monad.Identity.runIdentity (attach_T_TypeSigs (arg_typeSigs_))
         _segsX68 = Control.Monad.Identity.runIdentity (attach_T_MySegments ((sem_MySegments segs_val_)))
         (T_Children_vOut37 _childrenIap _childrenIechilds _childrenIfieldMap _childrenIflab _childrenIfty _childrenIgen _childrenIhoMap _childrenIinss _childrenIofld _childrenIolab _childrenIpmp _childrenIpmpr _childrenIpts _childrenIrefHoNts _childrenIrefNts _childrenIself) = inv_Children_s38 _childrenX38 (T_Children_vIn37 _childrenOain _childrenOan _childrenOaroundMap _childrenOasn _childrenOdty _childrenOflab _childrenOfty _childrenOhoMapf _childrenOlfpf _childrenOmergeMap _childrenOmergedChildren _childrenOnmp _childrenOnmprf _childrenOolab _childrenOoptions _childrenOpll _childrenOpmpf _childrenOpmprf)
         (T_Rules_vOut91 _rulesIerules _rulesIlfp _rulesIlfpr _rulesIruleMap _rulesIrulenumber _rulesIself _rulesIsfp _rulesIusedLocals) = inv_Rules_s92 _rulesX92 (T_Rules_vIn91 _rulesOdty _rulesOlfpf _rulesOpll _rulesOpts _rulesOrulenumber)
         (T_TypeSigs_vOut100 _typeSigsIlocalSigMap _typeSigsIself) = inv_TypeSigs_s101 _typeSigsX101 (T_TypeSigs_vIn100 )
         (T_MySegments_vOut67 _segsIevisits _segsIself _segsIvisitnum) = inv_MySegments_s68 _segsX68 (T_MySegments_vIn67 _segsOain _segsOasn _segsOdone _segsOfty _segsOhoMapf _segsOlfpf _segsOnmp _segsOnmprf _segsOoptions _segsOpmpf _segsOpmprf _segsOps _segsOruleMap _segsOtdp _segsOvisMapf _segsOvisitnum)
         _aroundMap = rule576 _lhsIaroundMap arg_con_
         _mergeMap = rule577 _lhsImergeMap arg_con_
         _mergedChildren = rule578 _mergeMap
         _lhsOlocalSigMap :: Map.Map ConstructorIdent (Map.Map Identifier Type)
         _lhsOlocalSigMap = rule579 _typeSigsIlocalSigMap arg_con_
         _ps = rule580 _lhsIdty arg_con_
         _lhsOads :: [Edge]
         _lhsOads = rule581 _childrenIpmpr _lhsIaugM _pll arg_con_
         _childrenOdty = rule582 _lhsIdty
         _pll = rule583 _lhsIdty arg_con_
         _rulesOpll = rule584 _pll
         _rulesOpts = rule585 _childrenIpts
         _lhsOfsInP :: FsInP
         _lhsOfsInP = rule586 _childrenIfieldMap _pll
         _lhsOfdps :: Map.Map ConstructorIdent (Set Dependency)
         _lhsOfdps = rule587 _lhsIdty _lhsIpmpf _lhsIres_ads arg_con_
         _segsOruleMap = rule588 _rulesIruleMap
         _segsOdone = rule589  ()
         _intros = rule590 _childrenIself
         _lhsOeprods :: EProductions
         _lhsOeprods = rule591 _childrenIechilds _intros _rulesIerules _segsIevisits arg_con_ arg_constraints_ arg_params_
         segs_val_ = rule592 _lhsImysegments _lhsInmp _lhsIpmprf _ps
         _lhsOap :: A_P
         _lhsOap = rule593 _childrenIap
         _lhsOfieldMap :: FMap
         _lhsOfieldMap = rule594 _childrenIfieldMap
         _lhsOfty :: FTY
         _lhsOfty = rule595 _childrenIfty
         _lhsOgen :: Map Int Int
         _lhsOgen = rule596 _childrenIgen
         _lhsOhoMap :: HOMap
         _lhsOhoMap = rule597 _childrenIhoMap
         _lhsOinss :: Map Int [Int]
         _lhsOinss = rule598 _childrenIinss
         _lhsOlfp :: SF_P
         _lhsOlfp = rule599 _rulesIlfp
         _lhsOlfpr :: SF_P
         _lhsOlfpr = rule600 _rulesIlfpr
         _lhsOofld :: [(Int, Int)]
         _lhsOofld = rule601 _childrenIofld
         _lhsOpmp :: PMP
         _lhsOpmp = rule602 _childrenIpmp
         _lhsOpmpr :: PMP_R
         _lhsOpmpr = rule603 _childrenIpmpr
         _lhsOrefHoNts :: Set NontermIdent
         _lhsOrefHoNts = rule604 _childrenIrefHoNts
         _lhsOrefNts :: Set NontermIdent
         _lhsOrefNts = rule605 _childrenIrefNts
         _lhsOruleMap :: Map.Map MyOccurrence Identifier
         _lhsOruleMap = rule606 _rulesIruleMap
         _lhsOsfp :: SF_P
         _lhsOsfp = rule607 _rulesIsfp
         _self = rule608 _childrenIself _rulesIself _typeSigsIself arg_con_ arg_constraints_ arg_macro_ arg_params_
         _lhsOself :: Production
         _lhsOself = rule609 _self
         _lhsOflab :: Int
         _lhsOflab = rule610 _childrenIflab
         _lhsOolab :: Int
         _lhsOolab = rule611 _childrenIolab
         _lhsOps :: PLabel
         _lhsOps = rule612 _ps
         _lhsOrulenumber :: Int
         _lhsOrulenumber = rule613 _rulesIrulenumber
         _lhsOvisitnum :: Int
         _lhsOvisitnum = rule614 _segsIvisitnum
         _childrenOain = rule615 _lhsIain
         _childrenOan = rule616 _lhsIan
         _childrenOaroundMap = rule617 _aroundMap
         _childrenOasn = rule618 _lhsIasn
         _childrenOflab = rule619 _lhsIflab
         _childrenOfty = rule620 _lhsIfty
         _childrenOhoMapf = rule621 _lhsIhoMapf
         _childrenOlfpf = rule622 _lhsIlfpf
         _childrenOmergeMap = rule623 _mergeMap
         _childrenOmergedChildren = rule624 _mergedChildren
         _childrenOnmp = rule625 _lhsInmp
         _childrenOnmprf = rule626 _lhsInmprf
         _childrenOolab = rule627 _lhsIolab
         _childrenOoptions = rule628 _lhsIoptions
         _childrenOpll = rule629 _pll
         _childrenOpmpf = rule630 _lhsIpmpf
         _childrenOpmprf = rule631 _lhsIpmprf
         _rulesOdty = rule632 _lhsIdty
         _rulesOlfpf = rule633 _lhsIlfpf
         _rulesOrulenumber = rule634 _lhsIrulenumber
         _segsOain = rule635 _lhsIain
         _segsOasn = rule636 _lhsIasn
         _segsOfty = rule637 _childrenIfty
         _segsOhoMapf = rule638 _lhsIhoMapf
         _segsOlfpf = rule639 _lhsIlfpf
         _segsOnmp = rule640 _lhsInmp
         _segsOnmprf = rule641 _lhsInmprf
         _segsOoptions = rule642 _lhsIoptions
         _segsOpmpf = rule643 _lhsIpmpf
         _segsOpmprf = rule644 _lhsIpmprf
         _segsOps = rule645 _ps
         _segsOtdp = rule646 _lhsItdp
         _segsOvisMapf = rule647 _lhsIvisMapf
         _segsOvisitnum = rule648 _lhsIvisitnum
         __result_ = T_Production_vOut82 _lhsOads _lhsOap _lhsOeprods _lhsOfdps _lhsOfieldMap _lhsOflab _lhsOfsInP _lhsOfty _lhsOgen _lhsOhoMap _lhsOinss _lhsOlfp _lhsOlfpr _lhsOlocalSigMap _lhsOofld _lhsOolab _lhsOpmp _lhsOpmpr _lhsOps _lhsOrefHoNts _lhsOrefNts _lhsOruleMap _lhsOrulenumber _lhsOself _lhsOsfp _lhsOvisitnum
         in __result_ )
     in C_Production_s83 v82
   {-# INLINE rule576 #-}
   {-# LINE 89 "src-ag/ExecutionPlanCommon.ag" #-}
   rule576 = \ ((_lhsIaroundMap) :: Map ConstructorIdent (Map Identifier [Expression])) con_ ->
                                                 {-# LINE 89 "src-ag/ExecutionPlanCommon.ag" #-}
                                                 Map.findWithDefault Map.empty con_ _lhsIaroundMap
                                                 {-# LINE 4883 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule577 #-}
   {-# LINE 114 "src-ag/ExecutionPlanCommon.ag" #-}
   rule577 = \ ((_lhsImergeMap) :: Map ConstructorIdent (Map Identifier (Identifier, [Identifier], Expression))) con_ ->
                                                {-# LINE 114 "src-ag/ExecutionPlanCommon.ag" #-}
                                                Map.findWithDefault Map.empty con_ _lhsImergeMap
                                                {-# LINE 4889 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule578 #-}
   {-# LINE 120 "src-ag/ExecutionPlanCommon.ag" #-}
   rule578 = \ _mergeMap ->
                         {-# LINE 120 "src-ag/ExecutionPlanCommon.ag" #-}
                         Set.unions [ Set.fromList ms | (_,ms,_) <- Map.elems _mergeMap     ]
                         {-# LINE 4895 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule579 #-}
   {-# LINE 160 "src-ag/ExecutionPlanCommon.ag" #-}
   rule579 = \ ((_typeSigsIlocalSigMap) :: Map Identifier Type) con_ ->
                                                   {-# LINE 160 "src-ag/ExecutionPlanCommon.ag" #-}
                                                   Map.singleton con_ _typeSigsIlocalSigMap
                                                   {-# LINE 4901 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule580 #-}
   {-# LINE 115 "src-ag/LOAG/Prepare.ag" #-}
   rule580 = \ ((_lhsIdty) :: MyType) con_ ->
               {-# LINE 115 "src-ag/LOAG/Prepare.ag" #-}
               (_lhsIdty,getName con_)
               {-# LINE 4907 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule581 #-}
   {-# LINE 117 "src-ag/LOAG/Prepare.ag" #-}
   rule581 = \ ((_childrenIpmpr) :: PMP_R) ((_lhsIaugM) :: Map.Map Identifier (Set.Set Dependency)) _pll con_ ->
          {-# LINE 117 "src-ag/LOAG/Prepare.ag" #-}
          case Map.lookup con_ _lhsIaugM of
           Nothing -> []
           Just a  -> Set.toList $ Set.map (depToEdge _childrenIpmpr _pll    ) a
          {-# LINE 4915 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule582 #-}
   {-# LINE 120 "src-ag/LOAG/Prepare.ag" #-}
   rule582 = \ ((_lhsIdty) :: MyType) ->
                     {-# LINE 120 "src-ag/LOAG/Prepare.ag" #-}
                     _lhsIdty
                     {-# LINE 4921 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule583 #-}
   {-# LINE 214 "src-ag/LOAG/Prepare.ag" #-}
   rule583 = \ ((_lhsIdty) :: MyType) con_ ->
                  {-# LINE 214 "src-ag/LOAG/Prepare.ag" #-}
                  (_lhsIdty,getName con_)
                  {-# LINE 4927 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule584 #-}
   {-# LINE 215 "src-ag/LOAG/Prepare.ag" #-}
   rule584 = \ _pll ->
                  {-# LINE 215 "src-ag/LOAG/Prepare.ag" #-}
                  _pll
                  {-# LINE 4933 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule585 #-}
   {-# LINE 216 "src-ag/LOAG/Prepare.ag" #-}
   rule585 = \ ((_childrenIpts) :: Set.Set FLabel) ->
                  {-# LINE 216 "src-ag/LOAG/Prepare.ag" #-}
                  _childrenIpts
                  {-# LINE 4939 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule586 #-}
   {-# LINE 217 "src-ag/LOAG/Prepare.ag" #-}
   rule586 = \ ((_childrenIfieldMap) :: FMap) _pll ->
                  {-# LINE 217 "src-ag/LOAG/Prepare.ag" #-}
                  Map.singleton _pll $ Map.keys _childrenIfieldMap
                  {-# LINE 4945 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule587 #-}
   {-# LINE 89 "src-ag/LOAG/Order.ag" #-}
   rule587 = \ ((_lhsIdty) :: MyType) ((_lhsIpmpf) :: PMP) ((_lhsIres_ads) :: [Edge]) con_ ->
        {-# LINE 89 "src-ag/LOAG/Order.ag" #-}
        let op d@(f,t) ds
              | fst (argsOf $ findWithErr _lhsIpmpf "fdps" f) == (_lhsIdty,getName con_)
                  = Set.insert (edgeToDep _lhsIpmpf d) ds
              | otherwise
                  = ds
        in Map.singleton con_ $ foldr op Set.empty _lhsIres_ads
        {-# LINE 4956 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule588 #-}
   {-# LINE 167 "src-ag/LOAG/Order.ag" #-}
   rule588 = \ ((_rulesIruleMap) :: Map.Map MyOccurrence Identifier) ->
                          {-# LINE 167 "src-ag/LOAG/Order.ag" #-}
                          _rulesIruleMap
                          {-# LINE 4962 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule589 #-}
   {-# LINE 168 "src-ag/LOAG/Order.ag" #-}
   rule589 = \  (_ :: ()) ->
                          {-# LINE 168 "src-ag/LOAG/Order.ag" #-}
                          (Set.empty, Set.empty, Set.empty, Set.empty)
                          {-# LINE 4968 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule590 #-}
   {-# LINE 169 "src-ag/LOAG/Order.ag" #-}
   rule590 = \ ((_childrenIself) :: Children) ->
                        {-# LINE 169 "src-ag/LOAG/Order.ag" #-}
                        let intro (Child nm _ kind)
                              | kind == ChildAttr = Nothing
                              | otherwise = Just $ ChildIntro nm
                          in catMaybes $ map intro _childrenIself
                        {-# LINE 4977 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule591 #-}
   {-# LINE 174 "src-ag/LOAG/Order.ag" #-}
   rule591 = \ ((_childrenIechilds) :: EChildren) _intros ((_rulesIerules) :: ERules) ((_segsIevisits) :: Visits) con_ constraints_ params_ ->
              {-# LINE 174 "src-ag/LOAG/Order.ag" #-}
              let ((Visit ident from to inh syn steps kind):vss) = _segsIevisits
                  steps' = _intros     ++ steps
                  visits | null _segsIevisits = []
                         | otherwise =
                              ((Visit ident from to inh syn steps' kind):vss)
               in [EProduction
                          con_
                          params_
                          constraints_
                          _rulesIerules
                          _childrenIechilds
                          visits ]
              {-# LINE 4994 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule592 #-}
   {-# LINE 346 "src-ag/LOAG/Order.ag" #-}
   rule592 = \ ((_lhsImysegments) :: MySegments) ((_lhsInmp) :: NMP) ((_lhsIpmprf) :: PMP_R) _ps ->
              {-# LINE 346 "src-ag/LOAG/Order.ag" #-}
              map (\(MySegment visnr inhs syns _ _) ->
                     MySegment visnr inhs syns
                               (Just $ map (_lhsIpmprf Map.!) $
                                      handAllOut (_ps    ,"lhs") $
                                          map (_lhsInmp Map.!) inhs)
                               (Just $ map (_lhsIpmprf Map.!) $
                                      handAllOut (_ps    ,"lhs") $
                                          map (_lhsInmp Map.!) syns)
                           ) _lhsImysegments
              {-# LINE 5008 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule593 #-}
   rule593 = \ ((_childrenIap) :: A_P) ->
     _childrenIap
   {-# INLINE rule594 #-}
   rule594 = \ ((_childrenIfieldMap) :: FMap) ->
     _childrenIfieldMap
   {-# INLINE rule595 #-}
   rule595 = \ ((_childrenIfty) :: FTY) ->
     _childrenIfty
   {-# INLINE rule596 #-}
   rule596 = \ ((_childrenIgen) :: Map Int Int) ->
     _childrenIgen
   {-# INLINE rule597 #-}
   rule597 = \ ((_childrenIhoMap) :: HOMap) ->
     _childrenIhoMap
   {-# INLINE rule598 #-}
   rule598 = \ ((_childrenIinss) :: Map Int [Int]) ->
     _childrenIinss
   {-# INLINE rule599 #-}
   rule599 = \ ((_rulesIlfp) :: SF_P) ->
     _rulesIlfp
   {-# INLINE rule600 #-}
   rule600 = \ ((_rulesIlfpr) :: SF_P) ->
     _rulesIlfpr
   {-# INLINE rule601 #-}
   rule601 = \ ((_childrenIofld) :: [(Int, Int)]) ->
     _childrenIofld
   {-# INLINE rule602 #-}
   rule602 = \ ((_childrenIpmp) :: PMP) ->
     _childrenIpmp
   {-# INLINE rule603 #-}
   rule603 = \ ((_childrenIpmpr) :: PMP_R) ->
     _childrenIpmpr
   {-# INLINE rule604 #-}
   rule604 = \ ((_childrenIrefHoNts) :: Set NontermIdent) ->
     _childrenIrefHoNts
   {-# INLINE rule605 #-}
   rule605 = \ ((_childrenIrefNts) :: Set NontermIdent) ->
     _childrenIrefNts
   {-# INLINE rule606 #-}
   rule606 = \ ((_rulesIruleMap) :: Map.Map MyOccurrence Identifier) ->
     _rulesIruleMap
   {-# INLINE rule607 #-}
   rule607 = \ ((_rulesIsfp) :: SF_P) ->
     _rulesIsfp
   {-# INLINE rule608 #-}
   rule608 = \ ((_childrenIself) :: Children) ((_rulesIself) :: Rules) ((_typeSigsIself) :: TypeSigs) con_ constraints_ macro_ params_ ->
     Production con_ params_ constraints_ _childrenIself _rulesIself _typeSigsIself macro_
   {-# INLINE rule609 #-}
   rule609 = \ _self ->
     _self
   {-# INLINE rule610 #-}
   rule610 = \ ((_childrenIflab) :: Int) ->
     _childrenIflab
   {-# INLINE rule611 #-}
   rule611 = \ ((_childrenIolab) :: Int) ->
     _childrenIolab
   {-# INLINE rule612 #-}
   rule612 = \ _ps ->
     _ps
   {-# INLINE rule613 #-}
   rule613 = \ ((_rulesIrulenumber) :: Int) ->
     _rulesIrulenumber
   {-# INLINE rule614 #-}
   rule614 = \ ((_segsIvisitnum) :: Int) ->
     _segsIvisitnum
   {-# INLINE rule615 #-}
   rule615 = \ ((_lhsIain) :: MyType -> MyAttributes) ->
     _lhsIain
   {-# INLINE rule616 #-}
   rule616 = \ ((_lhsIan) :: MyType -> MyAttributes) ->
     _lhsIan
   {-# INLINE rule617 #-}
   rule617 = \ _aroundMap ->
     _aroundMap
   {-# INLINE rule618 #-}
   rule618 = \ ((_lhsIasn) :: MyType -> MyAttributes) ->
     _lhsIasn
   {-# INLINE rule619 #-}
   rule619 = \ ((_lhsIflab) :: Int) ->
     _lhsIflab
   {-# INLINE rule620 #-}
   rule620 = \ ((_lhsIfty) :: FTY) ->
     _lhsIfty
   {-# INLINE rule621 #-}
   rule621 = \ ((_lhsIhoMapf) :: HOMap) ->
     _lhsIhoMapf
   {-# INLINE rule622 #-}
   rule622 = \ ((_lhsIlfpf) :: SF_P) ->
     _lhsIlfpf
   {-# INLINE rule623 #-}
   rule623 = \ _mergeMap ->
     _mergeMap
   {-# INLINE rule624 #-}
   rule624 = \ _mergedChildren ->
     _mergedChildren
   {-# INLINE rule625 #-}
   rule625 = \ ((_lhsInmp) :: NMP) ->
     _lhsInmp
   {-# INLINE rule626 #-}
   rule626 = \ ((_lhsInmprf) :: NMP_R) ->
     _lhsInmprf
   {-# INLINE rule627 #-}
   rule627 = \ ((_lhsIolab) :: Int) ->
     _lhsIolab
   {-# INLINE rule628 #-}
   rule628 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule629 #-}
   rule629 = \ _pll ->
     _pll
   {-# INLINE rule630 #-}
   rule630 = \ ((_lhsIpmpf) :: PMP) ->
     _lhsIpmpf
   {-# INLINE rule631 #-}
   rule631 = \ ((_lhsIpmprf) :: PMP_R) ->
     _lhsIpmprf
   {-# INLINE rule632 #-}
   rule632 = \ ((_lhsIdty) :: MyType) ->
     _lhsIdty
   {-# INLINE rule633 #-}
   rule633 = \ ((_lhsIlfpf) :: SF_P) ->
     _lhsIlfpf
   {-# INLINE rule634 #-}
   rule634 = \ ((_lhsIrulenumber) :: Int) ->
     _lhsIrulenumber
   {-# INLINE rule635 #-}
   rule635 = \ ((_lhsIain) :: MyType -> MyAttributes) ->
     _lhsIain
   {-# INLINE rule636 #-}
   rule636 = \ ((_lhsIasn) :: MyType -> MyAttributes) ->
     _lhsIasn
   {-# INLINE rule637 #-}
   rule637 = \ ((_childrenIfty) :: FTY) ->
     _childrenIfty
   {-# INLINE rule638 #-}
   rule638 = \ ((_lhsIhoMapf) :: HOMap) ->
     _lhsIhoMapf
   {-# INLINE rule639 #-}
   rule639 = \ ((_lhsIlfpf) :: SF_P) ->
     _lhsIlfpf
   {-# INLINE rule640 #-}
   rule640 = \ ((_lhsInmp) :: NMP) ->
     _lhsInmp
   {-# INLINE rule641 #-}
   rule641 = \ ((_lhsInmprf) :: NMP_R) ->
     _lhsInmprf
   {-# INLINE rule642 #-}
   rule642 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule643 #-}
   rule643 = \ ((_lhsIpmpf) :: PMP) ->
     _lhsIpmpf
   {-# INLINE rule644 #-}
   rule644 = \ ((_lhsIpmprf) :: PMP_R) ->
     _lhsIpmprf
   {-# INLINE rule645 #-}
   rule645 = \ _ps ->
     _ps
   {-# INLINE rule646 #-}
   rule646 = \ ((_lhsItdp) :: TDPRes) ->
     _lhsItdp
   {-# INLINE rule647 #-}
   rule647 = \ ((_lhsIvisMapf) :: IMap.IntMap Int) ->
     _lhsIvisMapf
   {-# INLINE rule648 #-}
   rule648 = \ ((_lhsIvisitnum) :: Int) ->
     _lhsIvisitnum

-- Productions -------------------------------------------------
-- wrapper
data Inh_Productions  = Inh_Productions { ain_Inh_Productions :: (MyType -> MyAttributes), an_Inh_Productions :: (MyType -> MyAttributes), aroundMap_Inh_Productions :: (Map ConstructorIdent (Map Identifier [Expression])), asn_Inh_Productions :: (MyType -> MyAttributes), augM_Inh_Productions :: (Map.Map Identifier (Set.Set Dependency)), dty_Inh_Productions :: (MyType), flab_Inh_Productions :: (Int), fty_Inh_Productions :: (FTY), ftyf_Inh_Productions :: (FTY), hoMapf_Inh_Productions :: (HOMap), lfpf_Inh_Productions :: (SF_P), mergeMap_Inh_Productions :: (Map ConstructorIdent (Map Identifier (Identifier, [Identifier], Expression))), mysegments_Inh_Productions :: (MySegments), nmp_Inh_Productions :: (NMP), nmprf_Inh_Productions :: (NMP_R), olab_Inh_Productions :: (Int), options_Inh_Productions :: (Options), pmpf_Inh_Productions :: (PMP), pmprf_Inh_Productions :: (PMP_R), res_ads_Inh_Productions :: ([Edge]), rulenumber_Inh_Productions :: (Int), tdp_Inh_Productions :: (TDPRes), visMapf_Inh_Productions :: (IMap.IntMap Int), visitnum_Inh_Productions :: (Int) }
data Syn_Productions  = Syn_Productions { ads_Syn_Productions :: ([Edge]), ap_Syn_Productions :: (A_P), eprods_Syn_Productions :: (EProductions), fdps_Syn_Productions :: (Map.Map ConstructorIdent (Set Dependency)), fieldMap_Syn_Productions :: (FMap), flab_Syn_Productions :: (Int), fsInP_Syn_Productions :: (FsInP), fty_Syn_Productions :: (FTY), gen_Syn_Productions :: (Map Int Int), hoMap_Syn_Productions :: (HOMap), inss_Syn_Productions :: (Map Int [Int]), lfp_Syn_Productions :: (SF_P), lfpr_Syn_Productions :: (SF_P), localSigMap_Syn_Productions :: (Map.Map ConstructorIdent (Map.Map Identifier Type)), ofld_Syn_Productions :: ([(Int, Int)]), olab_Syn_Productions :: (Int), pmp_Syn_Productions :: (PMP), pmpr_Syn_Productions :: (PMP_R), ps_Syn_Productions :: ([PLabel]), refHoNts_Syn_Productions :: (Set NontermIdent), refNts_Syn_Productions :: (Set NontermIdent), ruleMap_Syn_Productions :: (Map.Map MyOccurrence Identifier), rulenumber_Syn_Productions :: (Int), self_Syn_Productions :: (Productions), sfp_Syn_Productions :: (SF_P), visitnum_Syn_Productions :: (Int) }
{-# INLINABLE wrap_Productions #-}
wrap_Productions :: T_Productions  -> Inh_Productions  -> (Syn_Productions )
wrap_Productions (T_Productions act) (Inh_Productions _lhsIain _lhsIan _lhsIaroundMap _lhsIasn _lhsIaugM _lhsIdty _lhsIflab _lhsIfty _lhsIftyf _lhsIhoMapf _lhsIlfpf _lhsImergeMap _lhsImysegments _lhsInmp _lhsInmprf _lhsIolab _lhsIoptions _lhsIpmpf _lhsIpmprf _lhsIres_ads _lhsIrulenumber _lhsItdp _lhsIvisMapf _lhsIvisitnum) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg85 = T_Productions_vIn85 _lhsIain _lhsIan _lhsIaroundMap _lhsIasn _lhsIaugM _lhsIdty _lhsIflab _lhsIfty _lhsIftyf _lhsIhoMapf _lhsIlfpf _lhsImergeMap _lhsImysegments _lhsInmp _lhsInmprf _lhsIolab _lhsIoptions _lhsIpmpf _lhsIpmprf _lhsIres_ads _lhsIrulenumber _lhsItdp _lhsIvisMapf _lhsIvisitnum
        (T_Productions_vOut85 _lhsOads _lhsOap _lhsOeprods _lhsOfdps _lhsOfieldMap _lhsOflab _lhsOfsInP _lhsOfty _lhsOgen _lhsOhoMap _lhsOinss _lhsOlfp _lhsOlfpr _lhsOlocalSigMap _lhsOofld _lhsOolab _lhsOpmp _lhsOpmpr _lhsOps _lhsOrefHoNts _lhsOrefNts _lhsOruleMap _lhsOrulenumber _lhsOself _lhsOsfp _lhsOvisitnum) <- return (inv_Productions_s86 sem arg85)
        return (Syn_Productions _lhsOads _lhsOap _lhsOeprods _lhsOfdps _lhsOfieldMap _lhsOflab _lhsOfsInP _lhsOfty _lhsOgen _lhsOhoMap _lhsOinss _lhsOlfp _lhsOlfpr _lhsOlocalSigMap _lhsOofld _lhsOolab _lhsOpmp _lhsOpmpr _lhsOps _lhsOrefHoNts _lhsOrefNts _lhsOruleMap _lhsOrulenumber _lhsOself _lhsOsfp _lhsOvisitnum)
   )

-- cata
{-# NOINLINE sem_Productions #-}
sem_Productions :: Productions  -> T_Productions 
sem_Productions list = Prelude.foldr sem_Productions_Cons sem_Productions_Nil (Prelude.map sem_Production list)

-- semantic domain
newtype T_Productions  = T_Productions {
                                       attach_T_Productions :: Identity (T_Productions_s86 )
                                       }
newtype T_Productions_s86  = C_Productions_s86 {
                                               inv_Productions_s86 :: (T_Productions_v85 )
                                               }
data T_Productions_s87  = C_Productions_s87
type T_Productions_v85  = (T_Productions_vIn85 ) -> (T_Productions_vOut85 )
data T_Productions_vIn85  = T_Productions_vIn85 (MyType -> MyAttributes) (MyType -> MyAttributes) (Map ConstructorIdent (Map Identifier [Expression])) (MyType -> MyAttributes) (Map.Map Identifier (Set.Set Dependency)) (MyType) (Int) (FTY) (FTY) (HOMap) (SF_P) (Map ConstructorIdent (Map Identifier (Identifier, [Identifier], Expression))) (MySegments) (NMP) (NMP_R) (Int) (Options) (PMP) (PMP_R) ([Edge]) (Int) (TDPRes) (IMap.IntMap Int) (Int)
data T_Productions_vOut85  = T_Productions_vOut85 ([Edge]) (A_P) (EProductions) (Map.Map ConstructorIdent (Set Dependency)) (FMap) (Int) (FsInP) (FTY) (Map Int Int) (HOMap) (Map Int [Int]) (SF_P) (SF_P) (Map.Map ConstructorIdent (Map.Map Identifier Type)) ([(Int, Int)]) (Int) (PMP) (PMP_R) ([PLabel]) (Set NontermIdent) (Set NontermIdent) (Map.Map MyOccurrence Identifier) (Int) (Productions) (SF_P) (Int)
{-# NOINLINE sem_Productions_Cons #-}
sem_Productions_Cons :: T_Production  -> T_Productions  -> T_Productions 
sem_Productions_Cons arg_hd_ arg_tl_ = T_Productions (return st86) where
   {-# NOINLINE st86 #-}
   st86 = let
      v85 :: T_Productions_v85 
      v85 = \ (T_Productions_vIn85 _lhsIain _lhsIan _lhsIaroundMap _lhsIasn _lhsIaugM _lhsIdty _lhsIflab _lhsIfty _lhsIftyf _lhsIhoMapf _lhsIlfpf _lhsImergeMap _lhsImysegments _lhsInmp _lhsInmprf _lhsIolab _lhsIoptions _lhsIpmpf _lhsIpmprf _lhsIres_ads _lhsIrulenumber _lhsItdp _lhsIvisMapf _lhsIvisitnum) -> ( let
         _hdX83 = Control.Monad.Identity.runIdentity (attach_T_Production (arg_hd_))
         _tlX86 = Control.Monad.Identity.runIdentity (attach_T_Productions (arg_tl_))
         (T_Production_vOut82 _hdIads _hdIap _hdIeprods _hdIfdps _hdIfieldMap _hdIflab _hdIfsInP _hdIfty _hdIgen _hdIhoMap _hdIinss _hdIlfp _hdIlfpr _hdIlocalSigMap _hdIofld _hdIolab _hdIpmp _hdIpmpr _hdIps _hdIrefHoNts _hdIrefNts _hdIruleMap _hdIrulenumber _hdIself _hdIsfp _hdIvisitnum) = inv_Production_s83 _hdX83 (T_Production_vIn82 _hdOain _hdOan _hdOaroundMap _hdOasn _hdOaugM _hdOdty _hdOflab _hdOfty _hdOftyf _hdOhoMapf _hdOlfpf _hdOmergeMap _hdOmysegments _hdOnmp _hdOnmprf _hdOolab _hdOoptions _hdOpmpf _hdOpmprf _hdOres_ads _hdOrulenumber _hdOtdp _hdOvisMapf _hdOvisitnum)
         (T_Productions_vOut85 _tlIads _tlIap _tlIeprods _tlIfdps _tlIfieldMap _tlIflab _tlIfsInP _tlIfty _tlIgen _tlIhoMap _tlIinss _tlIlfp _tlIlfpr _tlIlocalSigMap _tlIofld _tlIolab _tlIpmp _tlIpmpr _tlIps _tlIrefHoNts _tlIrefNts _tlIruleMap _tlIrulenumber _tlIself _tlIsfp _tlIvisitnum) = inv_Productions_s86 _tlX86 (T_Productions_vIn85 _tlOain _tlOan _tlOaroundMap _tlOasn _tlOaugM _tlOdty _tlOflab _tlOfty _tlOftyf _tlOhoMapf _tlOlfpf _tlOmergeMap _tlOmysegments _tlOnmp _tlOnmprf _tlOolab _tlOoptions _tlOpmpf _tlOpmprf _tlOres_ads _tlOrulenumber _tlOtdp _tlOvisMapf _tlOvisitnum)
         _tlOvisitnum = rule649 _lhsIvisitnum
         _lhsOvisitnum :: Int
         _lhsOvisitnum = rule650 _hdIvisitnum
         _lhsOads :: [Edge]
         _lhsOads = rule651 _hdIads _tlIads
         _lhsOap :: A_P
         _lhsOap = rule652 _hdIap _tlIap
         _lhsOeprods :: EProductions
         _lhsOeprods = rule653 _hdIeprods _tlIeprods
         _lhsOfdps :: Map.Map ConstructorIdent (Set Dependency)
         _lhsOfdps = rule654 _hdIfdps _tlIfdps
         _lhsOfieldMap :: FMap
         _lhsOfieldMap = rule655 _hdIfieldMap _tlIfieldMap
         _lhsOfsInP :: FsInP
         _lhsOfsInP = rule656 _hdIfsInP _tlIfsInP
         _lhsOfty :: FTY
         _lhsOfty = rule657 _hdIfty _tlIfty
         _lhsOgen :: Map Int Int
         _lhsOgen = rule658 _hdIgen _tlIgen
         _lhsOhoMap :: HOMap
         _lhsOhoMap = rule659 _hdIhoMap _tlIhoMap
         _lhsOinss :: Map Int [Int]
         _lhsOinss = rule660 _hdIinss _tlIinss
         _lhsOlfp :: SF_P
         _lhsOlfp = rule661 _hdIlfp _tlIlfp
         _lhsOlfpr :: SF_P
         _lhsOlfpr = rule662 _hdIlfpr _tlIlfpr
         _lhsOlocalSigMap :: Map.Map ConstructorIdent (Map.Map Identifier Type)
         _lhsOlocalSigMap = rule663 _hdIlocalSigMap _tlIlocalSigMap
         _lhsOofld :: [(Int, Int)]
         _lhsOofld = rule664 _hdIofld _tlIofld
         _lhsOpmp :: PMP
         _lhsOpmp = rule665 _hdIpmp _tlIpmp
         _lhsOpmpr :: PMP_R
         _lhsOpmpr = rule666 _hdIpmpr _tlIpmpr
         _lhsOps :: [PLabel]
         _lhsOps = rule667 _hdIps _tlIps
         _lhsOrefHoNts :: Set NontermIdent
         _lhsOrefHoNts = rule668 _hdIrefHoNts _tlIrefHoNts
         _lhsOrefNts :: Set NontermIdent
         _lhsOrefNts = rule669 _hdIrefNts _tlIrefNts
         _lhsOruleMap :: Map.Map MyOccurrence Identifier
         _lhsOruleMap = rule670 _hdIruleMap _tlIruleMap
         _lhsOsfp :: SF_P
         _lhsOsfp = rule671 _hdIsfp _tlIsfp
         _self = rule672 _hdIself _tlIself
         _lhsOself :: Productions
         _lhsOself = rule673 _self
         _lhsOflab :: Int
         _lhsOflab = rule674 _tlIflab
         _lhsOolab :: Int
         _lhsOolab = rule675 _tlIolab
         _lhsOrulenumber :: Int
         _lhsOrulenumber = rule676 _tlIrulenumber
         _hdOain = rule677 _lhsIain
         _hdOan = rule678 _lhsIan
         _hdOaroundMap = rule679 _lhsIaroundMap
         _hdOasn = rule680 _lhsIasn
         _hdOaugM = rule681 _lhsIaugM
         _hdOdty = rule682 _lhsIdty
         _hdOflab = rule683 _lhsIflab
         _hdOfty = rule684 _lhsIfty
         _hdOftyf = rule685 _lhsIftyf
         _hdOhoMapf = rule686 _lhsIhoMapf
         _hdOlfpf = rule687 _lhsIlfpf
         _hdOmergeMap = rule688 _lhsImergeMap
         _hdOmysegments = rule689 _lhsImysegments
         _hdOnmp = rule690 _lhsInmp
         _hdOnmprf = rule691 _lhsInmprf
         _hdOolab = rule692 _lhsIolab
         _hdOoptions = rule693 _lhsIoptions
         _hdOpmpf = rule694 _lhsIpmpf
         _hdOpmprf = rule695 _lhsIpmprf
         _hdOres_ads = rule696 _lhsIres_ads
         _hdOrulenumber = rule697 _lhsIrulenumber
         _hdOtdp = rule698 _lhsItdp
         _hdOvisMapf = rule699 _lhsIvisMapf
         _hdOvisitnum = rule700 _lhsIvisitnum
         _tlOain = rule701 _lhsIain
         _tlOan = rule702 _lhsIan
         _tlOaroundMap = rule703 _lhsIaroundMap
         _tlOasn = rule704 _lhsIasn
         _tlOaugM = rule705 _lhsIaugM
         _tlOdty = rule706 _lhsIdty
         _tlOflab = rule707 _hdIflab
         _tlOfty = rule708 _hdIfty
         _tlOftyf = rule709 _lhsIftyf
         _tlOhoMapf = rule710 _lhsIhoMapf
         _tlOlfpf = rule711 _lhsIlfpf
         _tlOmergeMap = rule712 _lhsImergeMap
         _tlOmysegments = rule713 _lhsImysegments
         _tlOnmp = rule714 _lhsInmp
         _tlOnmprf = rule715 _lhsInmprf
         _tlOolab = rule716 _hdIolab
         _tlOoptions = rule717 _lhsIoptions
         _tlOpmpf = rule718 _lhsIpmpf
         _tlOpmprf = rule719 _lhsIpmprf
         _tlOres_ads = rule720 _lhsIres_ads
         _tlOrulenumber = rule721 _hdIrulenumber
         _tlOtdp = rule722 _lhsItdp
         _tlOvisMapf = rule723 _lhsIvisMapf
         __result_ = T_Productions_vOut85 _lhsOads _lhsOap _lhsOeprods _lhsOfdps _lhsOfieldMap _lhsOflab _lhsOfsInP _lhsOfty _lhsOgen _lhsOhoMap _lhsOinss _lhsOlfp _lhsOlfpr _lhsOlocalSigMap _lhsOofld _lhsOolab _lhsOpmp _lhsOpmpr _lhsOps _lhsOrefHoNts _lhsOrefNts _lhsOruleMap _lhsOrulenumber _lhsOself _lhsOsfp _lhsOvisitnum
         in __result_ )
     in C_Productions_s86 v85
   {-# INLINE rule649 #-}
   {-# LINE 192 "src-ag/LOAG/Order.ag" #-}
   rule649 = \ ((_lhsIvisitnum) :: Int) ->
                          {-# LINE 192 "src-ag/LOAG/Order.ag" #-}
                          _lhsIvisitnum
                          {-# LINE 5328 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule650 #-}
   {-# LINE 193 "src-ag/LOAG/Order.ag" #-}
   rule650 = \ ((_hdIvisitnum) :: Int) ->
                          {-# LINE 193 "src-ag/LOAG/Order.ag" #-}
                          _hdIvisitnum
                          {-# LINE 5334 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule651 #-}
   rule651 = \ ((_hdIads) :: [Edge]) ((_tlIads) :: [Edge]) ->
     ((++) _hdIads _tlIads)
   {-# INLINE rule652 #-}
   rule652 = \ ((_hdIap) :: A_P) ((_tlIap) :: A_P) ->
     (Map.unionWith (++) _hdIap _tlIap)
   {-# INLINE rule653 #-}
   rule653 = \ ((_hdIeprods) :: EProductions) ((_tlIeprods) :: EProductions) ->
     ((++) _hdIeprods _tlIeprods)
   {-# INLINE rule654 #-}
   rule654 = \ ((_hdIfdps) :: Map.Map ConstructorIdent (Set Dependency)) ((_tlIfdps) :: Map.Map ConstructorIdent (Set Dependency)) ->
     (Map.union _hdIfdps _tlIfdps)
   {-# INLINE rule655 #-}
   rule655 = \ ((_hdIfieldMap) :: FMap) ((_tlIfieldMap) :: FMap) ->
     ((Map.union) _hdIfieldMap _tlIfieldMap)
   {-# INLINE rule656 #-}
   rule656 = \ ((_hdIfsInP) :: FsInP) ((_tlIfsInP) :: FsInP) ->
     ((Map.union) _hdIfsInP _tlIfsInP)
   {-# INLINE rule657 #-}
   rule657 = \ ((_hdIfty) :: FTY) ((_tlIfty) :: FTY) ->
     (Map.union _hdIfty _tlIfty)
   {-# INLINE rule658 #-}
   rule658 = \ ((_hdIgen) :: Map Int Int) ((_tlIgen) :: Map Int Int) ->
     (Map.union _hdIgen _tlIgen)
   {-# INLINE rule659 #-}
   rule659 = \ ((_hdIhoMap) :: HOMap) ((_tlIhoMap) :: HOMap) ->
     ((Map.union) _hdIhoMap _tlIhoMap)
   {-# INLINE rule660 #-}
   rule660 = \ ((_hdIinss) :: Map Int [Int]) ((_tlIinss) :: Map Int [Int]) ->
     (Map.unionWith (++) _hdIinss _tlIinss)
   {-# INLINE rule661 #-}
   rule661 = \ ((_hdIlfp) :: SF_P) ((_tlIlfp) :: SF_P) ->
     (Map.unionWith (Set.union) _hdIlfp _tlIlfp)
   {-# INLINE rule662 #-}
   rule662 = \ ((_hdIlfpr) :: SF_P) ((_tlIlfpr) :: SF_P) ->
     (Map.unionWith (Set.union) _hdIlfpr _tlIlfpr)
   {-# INLINE rule663 #-}
   rule663 = \ ((_hdIlocalSigMap) :: Map.Map ConstructorIdent (Map.Map Identifier Type)) ((_tlIlocalSigMap) :: Map.Map ConstructorIdent (Map.Map Identifier Type)) ->
     _hdIlocalSigMap `Map.union` _tlIlocalSigMap
   {-# INLINE rule664 #-}
   rule664 = \ ((_hdIofld) :: [(Int, Int)]) ((_tlIofld) :: [(Int, Int)]) ->
     ((++) _hdIofld _tlIofld)
   {-# INLINE rule665 #-}
   rule665 = \ ((_hdIpmp) :: PMP) ((_tlIpmp) :: PMP) ->
     (Map.union _hdIpmp _tlIpmp)
   {-# INLINE rule666 #-}
   rule666 = \ ((_hdIpmpr) :: PMP_R) ((_tlIpmpr) :: PMP_R) ->
     (Map.union _hdIpmpr _tlIpmpr)
   {-# INLINE rule667 #-}
   rule667 = \ ((_hdIps) :: PLabel) ((_tlIps) :: [PLabel]) ->
     _hdIps : _tlIps
   {-# INLINE rule668 #-}
   rule668 = \ ((_hdIrefHoNts) :: Set NontermIdent) ((_tlIrefHoNts) :: Set NontermIdent) ->
     _hdIrefHoNts `mappend` _tlIrefHoNts
   {-# INLINE rule669 #-}
   rule669 = \ ((_hdIrefNts) :: Set NontermIdent) ((_tlIrefNts) :: Set NontermIdent) ->
     _hdIrefNts `mappend` _tlIrefNts
   {-# INLINE rule670 #-}
   rule670 = \ ((_hdIruleMap) :: Map.Map MyOccurrence Identifier) ((_tlIruleMap) :: Map.Map MyOccurrence Identifier) ->
     (Map.union _hdIruleMap _tlIruleMap)
   {-# INLINE rule671 #-}
   rule671 = \ ((_hdIsfp) :: SF_P) ((_tlIsfp) :: SF_P) ->
     (Map.unionWith (Set.union) _hdIsfp _tlIsfp)
   {-# INLINE rule672 #-}
   rule672 = \ ((_hdIself) :: Production) ((_tlIself) :: Productions) ->
     (:) _hdIself _tlIself
   {-# INLINE rule673 #-}
   rule673 = \ _self ->
     _self
   {-# INLINE rule674 #-}
   rule674 = \ ((_tlIflab) :: Int) ->
     _tlIflab
   {-# INLINE rule675 #-}
   rule675 = \ ((_tlIolab) :: Int) ->
     _tlIolab
   {-# INLINE rule676 #-}
   rule676 = \ ((_tlIrulenumber) :: Int) ->
     _tlIrulenumber
   {-# INLINE rule677 #-}
   rule677 = \ ((_lhsIain) :: MyType -> MyAttributes) ->
     _lhsIain
   {-# INLINE rule678 #-}
   rule678 = \ ((_lhsIan) :: MyType -> MyAttributes) ->
     _lhsIan
   {-# INLINE rule679 #-}
   rule679 = \ ((_lhsIaroundMap) :: Map ConstructorIdent (Map Identifier [Expression])) ->
     _lhsIaroundMap
   {-# INLINE rule680 #-}
   rule680 = \ ((_lhsIasn) :: MyType -> MyAttributes) ->
     _lhsIasn
   {-# INLINE rule681 #-}
   rule681 = \ ((_lhsIaugM) :: Map.Map Identifier (Set.Set Dependency)) ->
     _lhsIaugM
   {-# INLINE rule682 #-}
   rule682 = \ ((_lhsIdty) :: MyType) ->
     _lhsIdty
   {-# INLINE rule683 #-}
   rule683 = \ ((_lhsIflab) :: Int) ->
     _lhsIflab
   {-# INLINE rule684 #-}
   rule684 = \ ((_lhsIfty) :: FTY) ->
     _lhsIfty
   {-# INLINE rule685 #-}
   rule685 = \ ((_lhsIftyf) :: FTY) ->
     _lhsIftyf
   {-# INLINE rule686 #-}
   rule686 = \ ((_lhsIhoMapf) :: HOMap) ->
     _lhsIhoMapf
   {-# INLINE rule687 #-}
   rule687 = \ ((_lhsIlfpf) :: SF_P) ->
     _lhsIlfpf
   {-# INLINE rule688 #-}
   rule688 = \ ((_lhsImergeMap) :: Map ConstructorIdent (Map Identifier (Identifier, [Identifier], Expression))) ->
     _lhsImergeMap
   {-# INLINE rule689 #-}
   rule689 = \ ((_lhsImysegments) :: MySegments) ->
     _lhsImysegments
   {-# INLINE rule690 #-}
   rule690 = \ ((_lhsInmp) :: NMP) ->
     _lhsInmp
   {-# INLINE rule691 #-}
   rule691 = \ ((_lhsInmprf) :: NMP_R) ->
     _lhsInmprf
   {-# INLINE rule692 #-}
   rule692 = \ ((_lhsIolab) :: Int) ->
     _lhsIolab
   {-# INLINE rule693 #-}
   rule693 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule694 #-}
   rule694 = \ ((_lhsIpmpf) :: PMP) ->
     _lhsIpmpf
   {-# INLINE rule695 #-}
   rule695 = \ ((_lhsIpmprf) :: PMP_R) ->
     _lhsIpmprf
   {-# INLINE rule696 #-}
   rule696 = \ ((_lhsIres_ads) :: [Edge]) ->
     _lhsIres_ads
   {-# INLINE rule697 #-}
   rule697 = \ ((_lhsIrulenumber) :: Int) ->
     _lhsIrulenumber
   {-# INLINE rule698 #-}
   rule698 = \ ((_lhsItdp) :: TDPRes) ->
     _lhsItdp
   {-# INLINE rule699 #-}
   rule699 = \ ((_lhsIvisMapf) :: IMap.IntMap Int) ->
     _lhsIvisMapf
   {-# INLINE rule700 #-}
   rule700 = \ ((_lhsIvisitnum) :: Int) ->
     _lhsIvisitnum
   {-# INLINE rule701 #-}
   rule701 = \ ((_lhsIain) :: MyType -> MyAttributes) ->
     _lhsIain
   {-# INLINE rule702 #-}
   rule702 = \ ((_lhsIan) :: MyType -> MyAttributes) ->
     _lhsIan
   {-# INLINE rule703 #-}
   rule703 = \ ((_lhsIaroundMap) :: Map ConstructorIdent (Map Identifier [Expression])) ->
     _lhsIaroundMap
   {-# INLINE rule704 #-}
   rule704 = \ ((_lhsIasn) :: MyType -> MyAttributes) ->
     _lhsIasn
   {-# INLINE rule705 #-}
   rule705 = \ ((_lhsIaugM) :: Map.Map Identifier (Set.Set Dependency)) ->
     _lhsIaugM
   {-# INLINE rule706 #-}
   rule706 = \ ((_lhsIdty) :: MyType) ->
     _lhsIdty
   {-# INLINE rule707 #-}
   rule707 = \ ((_hdIflab) :: Int) ->
     _hdIflab
   {-# INLINE rule708 #-}
   rule708 = \ ((_hdIfty) :: FTY) ->
     _hdIfty
   {-# INLINE rule709 #-}
   rule709 = \ ((_lhsIftyf) :: FTY) ->
     _lhsIftyf
   {-# INLINE rule710 #-}
   rule710 = \ ((_lhsIhoMapf) :: HOMap) ->
     _lhsIhoMapf
   {-# INLINE rule711 #-}
   rule711 = \ ((_lhsIlfpf) :: SF_P) ->
     _lhsIlfpf
   {-# INLINE rule712 #-}
   rule712 = \ ((_lhsImergeMap) :: Map ConstructorIdent (Map Identifier (Identifier, [Identifier], Expression))) ->
     _lhsImergeMap
   {-# INLINE rule713 #-}
   rule713 = \ ((_lhsImysegments) :: MySegments) ->
     _lhsImysegments
   {-# INLINE rule714 #-}
   rule714 = \ ((_lhsInmp) :: NMP) ->
     _lhsInmp
   {-# INLINE rule715 #-}
   rule715 = \ ((_lhsInmprf) :: NMP_R) ->
     _lhsInmprf
   {-# INLINE rule716 #-}
   rule716 = \ ((_hdIolab) :: Int) ->
     _hdIolab
   {-# INLINE rule717 #-}
   rule717 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule718 #-}
   rule718 = \ ((_lhsIpmpf) :: PMP) ->
     _lhsIpmpf
   {-# INLINE rule719 #-}
   rule719 = \ ((_lhsIpmprf) :: PMP_R) ->
     _lhsIpmprf
   {-# INLINE rule720 #-}
   rule720 = \ ((_lhsIres_ads) :: [Edge]) ->
     _lhsIres_ads
   {-# INLINE rule721 #-}
   rule721 = \ ((_hdIrulenumber) :: Int) ->
     _hdIrulenumber
   {-# INLINE rule722 #-}
   rule722 = \ ((_lhsItdp) :: TDPRes) ->
     _lhsItdp
   {-# INLINE rule723 #-}
   rule723 = \ ((_lhsIvisMapf) :: IMap.IntMap Int) ->
     _lhsIvisMapf
{-# NOINLINE sem_Productions_Nil #-}
sem_Productions_Nil ::  T_Productions 
sem_Productions_Nil  = T_Productions (return st86) where
   {-# NOINLINE st86 #-}
   st86 = let
      v85 :: T_Productions_v85 
      v85 = \ (T_Productions_vIn85 _lhsIain _lhsIan _lhsIaroundMap _lhsIasn _lhsIaugM _lhsIdty _lhsIflab _lhsIfty _lhsIftyf _lhsIhoMapf _lhsIlfpf _lhsImergeMap _lhsImysegments _lhsInmp _lhsInmprf _lhsIolab _lhsIoptions _lhsIpmpf _lhsIpmprf _lhsIres_ads _lhsIrulenumber _lhsItdp _lhsIvisMapf _lhsIvisitnum) -> ( let
         _lhsOads :: [Edge]
         _lhsOads = rule724  ()
         _lhsOap :: A_P
         _lhsOap = rule725  ()
         _lhsOeprods :: EProductions
         _lhsOeprods = rule726  ()
         _lhsOfdps :: Map.Map ConstructorIdent (Set Dependency)
         _lhsOfdps = rule727  ()
         _lhsOfieldMap :: FMap
         _lhsOfieldMap = rule728  ()
         _lhsOfsInP :: FsInP
         _lhsOfsInP = rule729  ()
         _lhsOfty :: FTY
         _lhsOfty = rule730  ()
         _lhsOgen :: Map Int Int
         _lhsOgen = rule731  ()
         _lhsOhoMap :: HOMap
         _lhsOhoMap = rule732  ()
         _lhsOinss :: Map Int [Int]
         _lhsOinss = rule733  ()
         _lhsOlfp :: SF_P
         _lhsOlfp = rule734  ()
         _lhsOlfpr :: SF_P
         _lhsOlfpr = rule735  ()
         _lhsOlocalSigMap :: Map.Map ConstructorIdent (Map.Map Identifier Type)
         _lhsOlocalSigMap = rule736  ()
         _lhsOofld :: [(Int, Int)]
         _lhsOofld = rule737  ()
         _lhsOpmp :: PMP
         _lhsOpmp = rule738  ()
         _lhsOpmpr :: PMP_R
         _lhsOpmpr = rule739  ()
         _lhsOps :: [PLabel]
         _lhsOps = rule740  ()
         _lhsOrefHoNts :: Set NontermIdent
         _lhsOrefHoNts = rule741  ()
         _lhsOrefNts :: Set NontermIdent
         _lhsOrefNts = rule742  ()
         _lhsOruleMap :: Map.Map MyOccurrence Identifier
         _lhsOruleMap = rule743  ()
         _lhsOsfp :: SF_P
         _lhsOsfp = rule744  ()
         _self = rule745  ()
         _lhsOself :: Productions
         _lhsOself = rule746 _self
         _lhsOflab :: Int
         _lhsOflab = rule747 _lhsIflab
         _lhsOolab :: Int
         _lhsOolab = rule748 _lhsIolab
         _lhsOrulenumber :: Int
         _lhsOrulenumber = rule749 _lhsIrulenumber
         _lhsOvisitnum :: Int
         _lhsOvisitnum = rule750 _lhsIvisitnum
         __result_ = T_Productions_vOut85 _lhsOads _lhsOap _lhsOeprods _lhsOfdps _lhsOfieldMap _lhsOflab _lhsOfsInP _lhsOfty _lhsOgen _lhsOhoMap _lhsOinss _lhsOlfp _lhsOlfpr _lhsOlocalSigMap _lhsOofld _lhsOolab _lhsOpmp _lhsOpmpr _lhsOps _lhsOrefHoNts _lhsOrefNts _lhsOruleMap _lhsOrulenumber _lhsOself _lhsOsfp _lhsOvisitnum
         in __result_ )
     in C_Productions_s86 v85
   {-# INLINE rule724 #-}
   rule724 = \  (_ :: ()) ->
     []
   {-# INLINE rule725 #-}
   rule725 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule726 #-}
   rule726 = \  (_ :: ()) ->
     []
   {-# INLINE rule727 #-}
   rule727 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule728 #-}
   rule728 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule729 #-}
   rule729 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule730 #-}
   rule730 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule731 #-}
   rule731 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule732 #-}
   rule732 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule733 #-}
   rule733 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule734 #-}
   rule734 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule735 #-}
   rule735 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule736 #-}
   rule736 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule737 #-}
   rule737 = \  (_ :: ()) ->
     []
   {-# INLINE rule738 #-}
   rule738 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule739 #-}
   rule739 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule740 #-}
   rule740 = \  (_ :: ()) ->
     ([])
   {-# INLINE rule741 #-}
   rule741 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule742 #-}
   rule742 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule743 #-}
   rule743 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule744 #-}
   rule744 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule745 #-}
   rule745 = \  (_ :: ()) ->
     []
   {-# INLINE rule746 #-}
   rule746 = \ _self ->
     _self
   {-# INLINE rule747 #-}
   rule747 = \ ((_lhsIflab) :: Int) ->
     _lhsIflab
   {-# INLINE rule748 #-}
   rule748 = \ ((_lhsIolab) :: Int) ->
     _lhsIolab
   {-# INLINE rule749 #-}
   rule749 = \ ((_lhsIrulenumber) :: Int) ->
     _lhsIrulenumber
   {-# INLINE rule750 #-}
   rule750 = \ ((_lhsIvisitnum) :: Int) ->
     _lhsIvisitnum

-- Rule --------------------------------------------------------
-- wrapper
data Inh_Rule  = Inh_Rule { dty_Inh_Rule :: (MyType), lfpf_Inh_Rule :: (SF_P), pll_Inh_Rule :: (PLabel), pts_Inh_Rule :: (Set.Set (FLabel)), rulenumber_Inh_Rule :: (Int) }
data Syn_Rule  = Syn_Rule { erules_Syn_Rule :: (ERule), lfp_Syn_Rule :: (SF_P), lfpr_Syn_Rule :: (SF_P), ruleMap_Syn_Rule :: (Map.Map MyOccurrence Identifier), rulenumber_Syn_Rule :: (Int), self_Syn_Rule :: (Rule), sfp_Syn_Rule :: (SF_P), used_Syn_Rule :: (Set.Set MyOccurrence), usedLocals_Syn_Rule :: (Set.Set MyOccurrence) }
{-# INLINABLE wrap_Rule #-}
wrap_Rule :: T_Rule  -> Inh_Rule  -> (Syn_Rule )
wrap_Rule (T_Rule act) (Inh_Rule _lhsIdty _lhsIlfpf _lhsIpll _lhsIpts _lhsIrulenumber) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg88 = T_Rule_vIn88 _lhsIdty _lhsIlfpf _lhsIpll _lhsIpts _lhsIrulenumber
        (T_Rule_vOut88 _lhsOerules _lhsOlfp _lhsOlfpr _lhsOruleMap _lhsOrulenumber _lhsOself _lhsOsfp _lhsOused _lhsOusedLocals) <- return (inv_Rule_s89 sem arg88)
        return (Syn_Rule _lhsOerules _lhsOlfp _lhsOlfpr _lhsOruleMap _lhsOrulenumber _lhsOself _lhsOsfp _lhsOused _lhsOusedLocals)
   )

-- cata
{-# INLINE sem_Rule #-}
sem_Rule :: Rule  -> T_Rule 
sem_Rule ( Rule mbName_ pattern_ rhs_ owrt_ origin_ explicit_ pure_ identity_ mbError_ eager_ ) = sem_Rule_Rule mbName_ ( sem_Pattern pattern_ ) ( sem_Expression rhs_ ) owrt_ origin_ explicit_ pure_ identity_ mbError_ eager_

-- semantic domain
newtype T_Rule  = T_Rule {
                         attach_T_Rule :: Identity (T_Rule_s89 )
                         }
newtype T_Rule_s89  = C_Rule_s89 {
                                 inv_Rule_s89 :: (T_Rule_v88 )
                                 }
data T_Rule_s90  = C_Rule_s90
type T_Rule_v88  = (T_Rule_vIn88 ) -> (T_Rule_vOut88 )
data T_Rule_vIn88  = T_Rule_vIn88 (MyType) (SF_P) (PLabel) (Set.Set (FLabel)) (Int)
data T_Rule_vOut88  = T_Rule_vOut88 (ERule) (SF_P) (SF_P) (Map.Map MyOccurrence Identifier) (Int) (Rule) (SF_P) (Set.Set MyOccurrence) (Set.Set MyOccurrence)
{-# NOINLINE sem_Rule_Rule #-}
sem_Rule_Rule :: (Maybe Identifier) -> T_Pattern  -> T_Expression  -> (Bool) -> (String) -> (Bool) -> (Bool) -> (Bool) -> (Maybe Error) -> (Bool) -> T_Rule 
sem_Rule_Rule arg_mbName_ arg_pattern_ arg_rhs_ arg_owrt_ arg_origin_ arg_explicit_ arg_pure_ arg_identity_ arg_mbError_ arg_eager_ = T_Rule (return st89) where
   {-# NOINLINE st89 #-}
   st89 = let
      v88 :: T_Rule_v88 
      v88 = \ (T_Rule_vIn88 _lhsIdty _lhsIlfpf _lhsIpll _lhsIpts _lhsIrulenumber) -> ( let
         _patternX77 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pattern_))
         _rhsX41 = Control.Monad.Identity.runIdentity (attach_T_Expression (arg_rhs_))
         (T_Pattern_vOut76 _patternIafs _patternIcopy _patternIself) = inv_Pattern_s77 _patternX77 (T_Pattern_vIn76 )
         (T_Expression_vOut40 _rhsIcopy _rhsIself _rhsIused) = inv_Expression_s41 _rhsX41 (T_Expression_vIn40 _rhsOpll _rhsOpts)
         _lhsOerules :: ERule
         _lhsOerules = rule751 _patternIcopy _rhsIcopy _rulename arg_explicit_ arg_mbError_ arg_origin_ arg_owrt_ arg_pure_
         _lhsOrulenumber :: Int
         _lhsOrulenumber = rule752 _lhsIrulenumber
         _rulename = rule753 _lhsIrulenumber arg_mbName_
         _usedLocals = rule754 _rhsIused
         _usesLocals = rule755 _usedLocals
         _lhsOsfp :: SF_P
         _lhsOruleMap :: Map.Map MyOccurrence Identifier
         _lhsOlfp :: SF_P
         _lhsOlfpr :: SF_P
         (_lhsOsfp,_lhsOruleMap,_lhsOlfp,_lhsOlfpr) = rule756 _lhsIlfpf _lhsIpll _patternIafs _rhsIused _rulename _usedLocals _usesLocals
         _lhsOused :: Set.Set MyOccurrence
         _lhsOused = rule757 _rhsIused
         _lhsOusedLocals :: Set.Set MyOccurrence
         _lhsOusedLocals = rule758 _usedLocals
         _self = rule759 _patternIself _rhsIself arg_eager_ arg_explicit_ arg_identity_ arg_mbError_ arg_mbName_ arg_origin_ arg_owrt_ arg_pure_
         _lhsOself :: Rule
         _lhsOself = rule760 _self
         _rhsOpll = rule761 _lhsIpll
         _rhsOpts = rule762 _lhsIpts
         __result_ = T_Rule_vOut88 _lhsOerules _lhsOlfp _lhsOlfpr _lhsOruleMap _lhsOrulenumber _lhsOself _lhsOsfp _lhsOused _lhsOusedLocals
         in __result_ )
     in C_Rule_s89 v88
   {-# INLINE rule751 #-}
   {-# LINE 65 "src-ag/ExecutionPlanCommon.ag" #-}
   rule751 = \ ((_patternIcopy) :: Pattern) ((_rhsIcopy) :: Expression) _rulename explicit_ mbError_ origin_ owrt_ pure_ ->
                        {-# LINE 65 "src-ag/ExecutionPlanCommon.ag" #-}
                        ERule _rulename
                              _patternIcopy
                              _rhsIcopy
                              owrt_
                              origin_
                              explicit_
                              pure_
                              mbError_
                        {-# LINE 5776 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule752 #-}
   {-# LINE 12 "src-ag/ExecutionPlanPre.ag" #-}
   rule752 = \ ((_lhsIrulenumber) :: Int) ->
                             {-# LINE 12 "src-ag/ExecutionPlanPre.ag" #-}
                             _lhsIrulenumber + 1
                             {-# LINE 5782 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule753 #-}
   {-# LINE 13 "src-ag/ExecutionPlanPre.ag" #-}
   rule753 = \ ((_lhsIrulenumber) :: Int) mbName_ ->
                             {-# LINE 13 "src-ag/ExecutionPlanPre.ag" #-}
                             maybe (identifier $ "rule" ++ show _lhsIrulenumber) id mbName_
                             {-# LINE 5788 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule754 #-}
   {-# LINE 230 "src-ag/LOAG/Prepare.ag" #-}
   rule754 = \ ((_rhsIused) :: Set.Set MyOccurrence) ->
                       {-# LINE 230 "src-ag/LOAG/Prepare.ag" #-}
                       Set.filter (\(MyOccurrence (_,f) _) -> f == "loc") _rhsIused
                       {-# LINE 5794 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule755 #-}
   {-# LINE 231 "src-ag/LOAG/Prepare.ag" #-}
   rule755 = \ _usedLocals ->
                       {-# LINE 231 "src-ag/LOAG/Prepare.ag" #-}
                       not $ Set.null _usedLocals
                       {-# LINE 5800 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule756 #-}
   {-# LINE 233 "src-ag/LOAG/Prepare.ag" #-}
   rule756 = \ ((_lhsIlfpf) :: SF_P) ((_lhsIpll) :: PLabel) ((_patternIafs) :: [(FLabel, ALabel, Bool)]) ((_rhsIused) :: Set.Set MyOccurrence) _rulename _usedLocals _usesLocals ->
          {-# LINE 233 "src-ag/LOAG/Prepare.ag" #-}
          foldr (\(f, a, b) (m',rm', l', lr') ->
            let att = (_lhsIpll, f) >.< a
                rm  = Map.insert att _rulename rm'
                l   = if _usesLocals     && not b
                        then Map.insert att _usedLocals     l'
                        else l'
                lr  = if _usesLocals     && not b
                       then Set.fold (\k m -> Map.insertWith (Set.union) k
                                   (Set.singleton att) m) lr' _usedLocals
                       else lr'
                sfpins = Map.insert att (_rhsIused `Set.union` fromHO) m'
                fromHO = maybe Set.empty id (Map.lookup hOcc _lhsIlfpf)
                  where hOcc = (_lhsIpll, "inst") >.< (f, AnyDir)
              in if b
                  then (m',rm, Map.insert att _rhsIused l,
                          Set.fold (\k m -> Map.insertWith (Set.union) k
                                  (Set.singleton att) m) lr _rhsIused)
                  else (sfpins,rm,l,lr))
                          (Map.empty,Map.empty,Map.empty,Map.empty) _patternIafs
          {-# LINE 5824 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule757 #-}
   rule757 = \ ((_rhsIused) :: Set.Set MyOccurrence) ->
     _rhsIused
   {-# INLINE rule758 #-}
   rule758 = \ _usedLocals ->
     _usedLocals
   {-# INLINE rule759 #-}
   rule759 = \ ((_patternIself) :: Pattern) ((_rhsIself) :: Expression) eager_ explicit_ identity_ mbError_ mbName_ origin_ owrt_ pure_ ->
     Rule mbName_ _patternIself _rhsIself owrt_ origin_ explicit_ pure_ identity_ mbError_ eager_
   {-# INLINE rule760 #-}
   rule760 = \ _self ->
     _self
   {-# INLINE rule761 #-}
   rule761 = \ ((_lhsIpll) :: PLabel) ->
     _lhsIpll
   {-# INLINE rule762 #-}
   rule762 = \ ((_lhsIpts) :: Set.Set (FLabel)) ->
     _lhsIpts

-- Rules -------------------------------------------------------
-- wrapper
data Inh_Rules  = Inh_Rules { dty_Inh_Rules :: (MyType), lfpf_Inh_Rules :: (SF_P), pll_Inh_Rules :: (PLabel), pts_Inh_Rules :: (Set.Set (FLabel)), rulenumber_Inh_Rules :: (Int) }
data Syn_Rules  = Syn_Rules { erules_Syn_Rules :: (ERules), lfp_Syn_Rules :: (SF_P), lfpr_Syn_Rules :: (SF_P), ruleMap_Syn_Rules :: (Map.Map MyOccurrence Identifier), rulenumber_Syn_Rules :: (Int), self_Syn_Rules :: (Rules), sfp_Syn_Rules :: (SF_P), usedLocals_Syn_Rules :: (Set.Set MyOccurrence) }
{-# INLINABLE wrap_Rules #-}
wrap_Rules :: T_Rules  -> Inh_Rules  -> (Syn_Rules )
wrap_Rules (T_Rules act) (Inh_Rules _lhsIdty _lhsIlfpf _lhsIpll _lhsIpts _lhsIrulenumber) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg91 = T_Rules_vIn91 _lhsIdty _lhsIlfpf _lhsIpll _lhsIpts _lhsIrulenumber
        (T_Rules_vOut91 _lhsOerules _lhsOlfp _lhsOlfpr _lhsOruleMap _lhsOrulenumber _lhsOself _lhsOsfp _lhsOusedLocals) <- return (inv_Rules_s92 sem arg91)
        return (Syn_Rules _lhsOerules _lhsOlfp _lhsOlfpr _lhsOruleMap _lhsOrulenumber _lhsOself _lhsOsfp _lhsOusedLocals)
   )

-- cata
{-# NOINLINE sem_Rules #-}
sem_Rules :: Rules  -> T_Rules 
sem_Rules list = Prelude.foldr sem_Rules_Cons sem_Rules_Nil (Prelude.map sem_Rule list)

-- semantic domain
newtype T_Rules  = T_Rules {
                           attach_T_Rules :: Identity (T_Rules_s92 )
                           }
newtype T_Rules_s92  = C_Rules_s92 {
                                   inv_Rules_s92 :: (T_Rules_v91 )
                                   }
data T_Rules_s93  = C_Rules_s93
type T_Rules_v91  = (T_Rules_vIn91 ) -> (T_Rules_vOut91 )
data T_Rules_vIn91  = T_Rules_vIn91 (MyType) (SF_P) (PLabel) (Set.Set (FLabel)) (Int)
data T_Rules_vOut91  = T_Rules_vOut91 (ERules) (SF_P) (SF_P) (Map.Map MyOccurrence Identifier) (Int) (Rules) (SF_P) (Set.Set MyOccurrence)
{-# NOINLINE sem_Rules_Cons #-}
sem_Rules_Cons :: T_Rule  -> T_Rules  -> T_Rules 
sem_Rules_Cons arg_hd_ arg_tl_ = T_Rules (return st92) where
   {-# NOINLINE st92 #-}
   st92 = let
      v91 :: T_Rules_v91 
      v91 = \ (T_Rules_vIn91 _lhsIdty _lhsIlfpf _lhsIpll _lhsIpts _lhsIrulenumber) -> ( let
         _hdX89 = Control.Monad.Identity.runIdentity (attach_T_Rule (arg_hd_))
         _tlX92 = Control.Monad.Identity.runIdentity (attach_T_Rules (arg_tl_))
         (T_Rule_vOut88 _hdIerules _hdIlfp _hdIlfpr _hdIruleMap _hdIrulenumber _hdIself _hdIsfp _hdIused _hdIusedLocals) = inv_Rule_s89 _hdX89 (T_Rule_vIn88 _hdOdty _hdOlfpf _hdOpll _hdOpts _hdOrulenumber)
         (T_Rules_vOut91 _tlIerules _tlIlfp _tlIlfpr _tlIruleMap _tlIrulenumber _tlIself _tlIsfp _tlIusedLocals) = inv_Rules_s92 _tlX92 (T_Rules_vIn91 _tlOdty _tlOlfpf _tlOpll _tlOpts _tlOrulenumber)
         _lhsOerules :: ERules
         _lhsOerules = rule763 _hdIerules _tlIerules
         _lhsOlfp :: SF_P
         _lhsOlfp = rule764 _hdIlfp _tlIlfp
         _lhsOlfpr :: SF_P
         _lhsOlfpr = rule765 _hdIlfpr _tlIlfpr
         _lhsOruleMap :: Map.Map MyOccurrence Identifier
         _lhsOruleMap = rule766 _hdIruleMap _tlIruleMap
         _lhsOsfp :: SF_P
         _lhsOsfp = rule767 _hdIsfp _tlIsfp
         _lhsOusedLocals :: Set.Set MyOccurrence
         _lhsOusedLocals = rule768 _hdIusedLocals _tlIusedLocals
         _self = rule769 _hdIself _tlIself
         _lhsOself :: Rules
         _lhsOself = rule770 _self
         _lhsOrulenumber :: Int
         _lhsOrulenumber = rule771 _tlIrulenumber
         _hdOdty = rule772 _lhsIdty
         _hdOlfpf = rule773 _lhsIlfpf
         _hdOpll = rule774 _lhsIpll
         _hdOpts = rule775 _lhsIpts
         _hdOrulenumber = rule776 _lhsIrulenumber
         _tlOdty = rule777 _lhsIdty
         _tlOlfpf = rule778 _lhsIlfpf
         _tlOpll = rule779 _lhsIpll
         _tlOpts = rule780 _lhsIpts
         _tlOrulenumber = rule781 _hdIrulenumber
         __result_ = T_Rules_vOut91 _lhsOerules _lhsOlfp _lhsOlfpr _lhsOruleMap _lhsOrulenumber _lhsOself _lhsOsfp _lhsOusedLocals
         in __result_ )
     in C_Rules_s92 v91
   {-# INLINE rule763 #-}
   rule763 = \ ((_hdIerules) :: ERule) ((_tlIerules) :: ERules) ->
     _hdIerules : _tlIerules
   {-# INLINE rule764 #-}
   rule764 = \ ((_hdIlfp) :: SF_P) ((_tlIlfp) :: SF_P) ->
     (Map.unionWith (Set.union) _hdIlfp _tlIlfp)
   {-# INLINE rule765 #-}
   rule765 = \ ((_hdIlfpr) :: SF_P) ((_tlIlfpr) :: SF_P) ->
     (Map.unionWith (Set.union) _hdIlfpr _tlIlfpr)
   {-# INLINE rule766 #-}
   rule766 = \ ((_hdIruleMap) :: Map.Map MyOccurrence Identifier) ((_tlIruleMap) :: Map.Map MyOccurrence Identifier) ->
     (Map.union _hdIruleMap _tlIruleMap)
   {-# INLINE rule767 #-}
   rule767 = \ ((_hdIsfp) :: SF_P) ((_tlIsfp) :: SF_P) ->
     (Map.unionWith (Set.union) _hdIsfp _tlIsfp)
   {-# INLINE rule768 #-}
   rule768 = \ ((_hdIusedLocals) :: Set.Set MyOccurrence) ((_tlIusedLocals) :: Set.Set MyOccurrence) ->
     ((Set.union) _hdIusedLocals _tlIusedLocals)
   {-# INLINE rule769 #-}
   rule769 = \ ((_hdIself) :: Rule) ((_tlIself) :: Rules) ->
     (:) _hdIself _tlIself
   {-# INLINE rule770 #-}
   rule770 = \ _self ->
     _self
   {-# INLINE rule771 #-}
   rule771 = \ ((_tlIrulenumber) :: Int) ->
     _tlIrulenumber
   {-# INLINE rule772 #-}
   rule772 = \ ((_lhsIdty) :: MyType) ->
     _lhsIdty
   {-# INLINE rule773 #-}
   rule773 = \ ((_lhsIlfpf) :: SF_P) ->
     _lhsIlfpf
   {-# INLINE rule774 #-}
   rule774 = \ ((_lhsIpll) :: PLabel) ->
     _lhsIpll
   {-# INLINE rule775 #-}
   rule775 = \ ((_lhsIpts) :: Set.Set (FLabel)) ->
     _lhsIpts
   {-# INLINE rule776 #-}
   rule776 = \ ((_lhsIrulenumber) :: Int) ->
     _lhsIrulenumber
   {-# INLINE rule777 #-}
   rule777 = \ ((_lhsIdty) :: MyType) ->
     _lhsIdty
   {-# INLINE rule778 #-}
   rule778 = \ ((_lhsIlfpf) :: SF_P) ->
     _lhsIlfpf
   {-# INLINE rule779 #-}
   rule779 = \ ((_lhsIpll) :: PLabel) ->
     _lhsIpll
   {-# INLINE rule780 #-}
   rule780 = \ ((_lhsIpts) :: Set.Set (FLabel)) ->
     _lhsIpts
   {-# INLINE rule781 #-}
   rule781 = \ ((_hdIrulenumber) :: Int) ->
     _hdIrulenumber
{-# NOINLINE sem_Rules_Nil #-}
sem_Rules_Nil ::  T_Rules 
sem_Rules_Nil  = T_Rules (return st92) where
   {-# NOINLINE st92 #-}
   st92 = let
      v91 :: T_Rules_v91 
      v91 = \ (T_Rules_vIn91 _lhsIdty _lhsIlfpf _lhsIpll _lhsIpts _lhsIrulenumber) -> ( let
         _lhsOerules :: ERules
         _lhsOerules = rule782  ()
         _lhsOlfp :: SF_P
         _lhsOlfp = rule783  ()
         _lhsOlfpr :: SF_P
         _lhsOlfpr = rule784  ()
         _lhsOruleMap :: Map.Map MyOccurrence Identifier
         _lhsOruleMap = rule785  ()
         _lhsOsfp :: SF_P
         _lhsOsfp = rule786  ()
         _lhsOusedLocals :: Set.Set MyOccurrence
         _lhsOusedLocals = rule787  ()
         _self = rule788  ()
         _lhsOself :: Rules
         _lhsOself = rule789 _self
         _lhsOrulenumber :: Int
         _lhsOrulenumber = rule790 _lhsIrulenumber
         __result_ = T_Rules_vOut91 _lhsOerules _lhsOlfp _lhsOlfpr _lhsOruleMap _lhsOrulenumber _lhsOself _lhsOsfp _lhsOusedLocals
         in __result_ )
     in C_Rules_s92 v91
   {-# INLINE rule782 #-}
   rule782 = \  (_ :: ()) ->
     []
   {-# INLINE rule783 #-}
   rule783 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule784 #-}
   rule784 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule785 #-}
   rule785 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule786 #-}
   rule786 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule787 #-}
   rule787 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule788 #-}
   rule788 = \  (_ :: ()) ->
     []
   {-# INLINE rule789 #-}
   rule789 = \ _self ->
     _self
   {-# INLINE rule790 #-}
   rule790 = \ ((_lhsIrulenumber) :: Int) ->
     _lhsIrulenumber

-- Sequence ----------------------------------------------------
-- wrapper
data Inh_Sequence  = Inh_Sequence {  }
data Syn_Sequence  = Syn_Sequence { self_Syn_Sequence :: (Sequence) }
{-# INLINABLE wrap_Sequence #-}
wrap_Sequence :: T_Sequence  -> Inh_Sequence  -> (Syn_Sequence )
wrap_Sequence (T_Sequence act) (Inh_Sequence ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg94 = T_Sequence_vIn94 
        (T_Sequence_vOut94 _lhsOself) <- return (inv_Sequence_s95 sem arg94)
        return (Syn_Sequence _lhsOself)
   )

-- cata
{-# NOINLINE sem_Sequence #-}
sem_Sequence :: Sequence  -> T_Sequence 
sem_Sequence list = Prelude.foldr sem_Sequence_Cons sem_Sequence_Nil (Prelude.map sem_CRule list)

-- semantic domain
newtype T_Sequence  = T_Sequence {
                                 attach_T_Sequence :: Identity (T_Sequence_s95 )
                                 }
newtype T_Sequence_s95  = C_Sequence_s95 {
                                         inv_Sequence_s95 :: (T_Sequence_v94 )
                                         }
data T_Sequence_s96  = C_Sequence_s96
type T_Sequence_v94  = (T_Sequence_vIn94 ) -> (T_Sequence_vOut94 )
data T_Sequence_vIn94  = T_Sequence_vIn94 
data T_Sequence_vOut94  = T_Sequence_vOut94 (Sequence)
{-# NOINLINE sem_Sequence_Cons #-}
sem_Sequence_Cons :: T_CRule  -> T_Sequence  -> T_Sequence 
sem_Sequence_Cons arg_hd_ arg_tl_ = T_Sequence (return st95) where
   {-# NOINLINE st95 #-}
   st95 = let
      v94 :: T_Sequence_v94 
      v94 = \ (T_Sequence_vIn94 ) -> ( let
         _hdX20 = Control.Monad.Identity.runIdentity (attach_T_CRule (arg_hd_))
         _tlX95 = Control.Monad.Identity.runIdentity (attach_T_Sequence (arg_tl_))
         (T_CRule_vOut19 _hdIself) = inv_CRule_s20 _hdX20 (T_CRule_vIn19 )
         (T_Sequence_vOut94 _tlIself) = inv_Sequence_s95 _tlX95 (T_Sequence_vIn94 )
         _self = rule791 _hdIself _tlIself
         _lhsOself :: Sequence
         _lhsOself = rule792 _self
         __result_ = T_Sequence_vOut94 _lhsOself
         in __result_ )
     in C_Sequence_s95 v94
   {-# INLINE rule791 #-}
   rule791 = \ ((_hdIself) :: CRule) ((_tlIself) :: Sequence) ->
     (:) _hdIself _tlIself
   {-# INLINE rule792 #-}
   rule792 = \ _self ->
     _self
{-# NOINLINE sem_Sequence_Nil #-}
sem_Sequence_Nil ::  T_Sequence 
sem_Sequence_Nil  = T_Sequence (return st95) where
   {-# NOINLINE st95 #-}
   st95 = let
      v94 :: T_Sequence_v94 
      v94 = \ (T_Sequence_vIn94 ) -> ( let
         _self = rule793  ()
         _lhsOself :: Sequence
         _lhsOself = rule794 _self
         __result_ = T_Sequence_vOut94 _lhsOself
         in __result_ )
     in C_Sequence_s95 v94
   {-# INLINE rule793 #-}
   rule793 = \  (_ :: ()) ->
     []
   {-# INLINE rule794 #-}
   rule794 = \ _self ->
     _self

-- TypeSig -----------------------------------------------------
-- wrapper
data Inh_TypeSig  = Inh_TypeSig {  }
data Syn_TypeSig  = Syn_TypeSig { localSigMap_Syn_TypeSig :: (Map Identifier Type), self_Syn_TypeSig :: (TypeSig) }
{-# INLINABLE wrap_TypeSig #-}
wrap_TypeSig :: T_TypeSig  -> Inh_TypeSig  -> (Syn_TypeSig )
wrap_TypeSig (T_TypeSig act) (Inh_TypeSig ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg97 = T_TypeSig_vIn97 
        (T_TypeSig_vOut97 _lhsOlocalSigMap _lhsOself) <- return (inv_TypeSig_s98 sem arg97)
        return (Syn_TypeSig _lhsOlocalSigMap _lhsOself)
   )

-- cata
{-# INLINE sem_TypeSig #-}
sem_TypeSig :: TypeSig  -> T_TypeSig 
sem_TypeSig ( TypeSig name_ tp_ ) = sem_TypeSig_TypeSig name_ tp_

-- semantic domain
newtype T_TypeSig  = T_TypeSig {
                               attach_T_TypeSig :: Identity (T_TypeSig_s98 )
                               }
newtype T_TypeSig_s98  = C_TypeSig_s98 {
                                       inv_TypeSig_s98 :: (T_TypeSig_v97 )
                                       }
data T_TypeSig_s99  = C_TypeSig_s99
type T_TypeSig_v97  = (T_TypeSig_vIn97 ) -> (T_TypeSig_vOut97 )
data T_TypeSig_vIn97  = T_TypeSig_vIn97 
data T_TypeSig_vOut97  = T_TypeSig_vOut97 (Map Identifier Type) (TypeSig)
{-# NOINLINE sem_TypeSig_TypeSig #-}
sem_TypeSig_TypeSig :: (Identifier) -> (Type) -> T_TypeSig 
sem_TypeSig_TypeSig arg_name_ arg_tp_ = T_TypeSig (return st98) where
   {-# NOINLINE st98 #-}
   st98 = let
      v97 :: T_TypeSig_v97 
      v97 = \ (T_TypeSig_vIn97 ) -> ( let
         _lhsOlocalSigMap :: Map Identifier Type
         _lhsOlocalSigMap = rule795 arg_name_ arg_tp_
         _self = rule796 arg_name_ arg_tp_
         _lhsOself :: TypeSig
         _lhsOself = rule797 _self
         __result_ = T_TypeSig_vOut97 _lhsOlocalSigMap _lhsOself
         in __result_ )
     in C_TypeSig_s98 v97
   {-# INLINE rule795 #-}
   {-# LINE 161 "src-ag/ExecutionPlanCommon.ag" #-}
   rule795 = \ name_ tp_ ->
                                                   {-# LINE 161 "src-ag/ExecutionPlanCommon.ag" #-}
                                                   Map.singleton name_ tp_
                                                   {-# LINE 6150 "dist/build/LOAG/Order.hs"#-}
   {-# INLINE rule796 #-}
   rule796 = \ name_ tp_ ->
     TypeSig name_ tp_
   {-# INLINE rule797 #-}
   rule797 = \ _self ->
     _self

-- TypeSigs ----------------------------------------------------
-- wrapper
data Inh_TypeSigs  = Inh_TypeSigs {  }
data Syn_TypeSigs  = Syn_TypeSigs { localSigMap_Syn_TypeSigs :: (Map Identifier Type), self_Syn_TypeSigs :: (TypeSigs) }
{-# INLINABLE wrap_TypeSigs #-}
wrap_TypeSigs :: T_TypeSigs  -> Inh_TypeSigs  -> (Syn_TypeSigs )
wrap_TypeSigs (T_TypeSigs act) (Inh_TypeSigs ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg100 = T_TypeSigs_vIn100 
        (T_TypeSigs_vOut100 _lhsOlocalSigMap _lhsOself) <- return (inv_TypeSigs_s101 sem arg100)
        return (Syn_TypeSigs _lhsOlocalSigMap _lhsOself)
   )

-- cata
{-# NOINLINE sem_TypeSigs #-}
sem_TypeSigs :: TypeSigs  -> T_TypeSigs 
sem_TypeSigs list = Prelude.foldr sem_TypeSigs_Cons sem_TypeSigs_Nil (Prelude.map sem_TypeSig list)

-- semantic domain
newtype T_TypeSigs  = T_TypeSigs {
                                 attach_T_TypeSigs :: Identity (T_TypeSigs_s101 )
                                 }
newtype T_TypeSigs_s101  = C_TypeSigs_s101 {
                                           inv_TypeSigs_s101 :: (T_TypeSigs_v100 )
                                           }
data T_TypeSigs_s102  = C_TypeSigs_s102
type T_TypeSigs_v100  = (T_TypeSigs_vIn100 ) -> (T_TypeSigs_vOut100 )
data T_TypeSigs_vIn100  = T_TypeSigs_vIn100 
data T_TypeSigs_vOut100  = T_TypeSigs_vOut100 (Map Identifier Type) (TypeSigs)
{-# NOINLINE sem_TypeSigs_Cons #-}
sem_TypeSigs_Cons :: T_TypeSig  -> T_TypeSigs  -> T_TypeSigs 
sem_TypeSigs_Cons arg_hd_ arg_tl_ = T_TypeSigs (return st101) where
   {-# NOINLINE st101 #-}
   st101 = let
      v100 :: T_TypeSigs_v100 
      v100 = \ (T_TypeSigs_vIn100 ) -> ( let
         _hdX98 = Control.Monad.Identity.runIdentity (attach_T_TypeSig (arg_hd_))
         _tlX101 = Control.Monad.Identity.runIdentity (attach_T_TypeSigs (arg_tl_))
         (T_TypeSig_vOut97 _hdIlocalSigMap _hdIself) = inv_TypeSig_s98 _hdX98 (T_TypeSig_vIn97 )
         (T_TypeSigs_vOut100 _tlIlocalSigMap _tlIself) = inv_TypeSigs_s101 _tlX101 (T_TypeSigs_vIn100 )
         _lhsOlocalSigMap :: Map Identifier Type
         _lhsOlocalSigMap = rule798 _hdIlocalSigMap _tlIlocalSigMap
         _self = rule799 _hdIself _tlIself
         _lhsOself :: TypeSigs
         _lhsOself = rule800 _self
         __result_ = T_TypeSigs_vOut100 _lhsOlocalSigMap _lhsOself
         in __result_ )
     in C_TypeSigs_s101 v100
   {-# INLINE rule798 #-}
   rule798 = \ ((_hdIlocalSigMap) :: Map Identifier Type) ((_tlIlocalSigMap) :: Map Identifier Type) ->
     _hdIlocalSigMap `Map.union` _tlIlocalSigMap
   {-# INLINE rule799 #-}
   rule799 = \ ((_hdIself) :: TypeSig) ((_tlIself) :: TypeSigs) ->
     (:) _hdIself _tlIself
   {-# INLINE rule800 #-}
   rule800 = \ _self ->
     _self
{-# NOINLINE sem_TypeSigs_Nil #-}
sem_TypeSigs_Nil ::  T_TypeSigs 
sem_TypeSigs_Nil  = T_TypeSigs (return st101) where
   {-# NOINLINE st101 #-}
   st101 = let
      v100 :: T_TypeSigs_v100 
      v100 = \ (T_TypeSigs_vIn100 ) -> ( let
         _lhsOlocalSigMap :: Map Identifier Type
         _lhsOlocalSigMap = rule801  ()
         _self = rule802  ()
         _lhsOself :: TypeSigs
         _lhsOself = rule803 _self
         __result_ = T_TypeSigs_vOut100 _lhsOlocalSigMap _lhsOself
         in __result_ )
     in C_TypeSigs_s101 v100
   {-# INLINE rule801 #-}
   rule801 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule802 #-}
   rule802 = \  (_ :: ()) ->
     []
   {-# INLINE rule803 #-}
   rule803 = \ _self ->
     _self
