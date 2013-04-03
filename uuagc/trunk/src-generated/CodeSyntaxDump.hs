{-# LANGUAGE Rank2Types, GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CodeSyntaxDump where
{-# LINE 2 "./src-ag/CodeSyntax.ag" #-}

import Patterns
import CommonTypes
import Data.Map(Map)
import Data.Set(Set)
{-# LINE 12 "dist/build/CodeSyntaxDump.hs" #-}

{-# LINE 2 "./src-ag/Patterns.ag" #-}

-- Patterns.ag imports
import UU.Scanner.Position(Pos)
import CommonTypes (ConstructorIdent,Identifier)
{-# LINE 19 "dist/build/CodeSyntaxDump.hs" #-}

{-# LINE 5 "./src-ag/CodeSyntaxDump.ag" #-}

import Data.List
import qualified Data.Map as Map

import Pretty
import PPUtil

import CodeSyntax
{-# LINE 30 "dist/build/CodeSyntaxDump.hs" #-}
import Control.Monad.Identity (Identity)
import qualified Control.Monad.Identity
{-# LINE 15 "./src-ag/CodeSyntaxDump.ag" #-}

ppChild :: (Identifier,Type,ChildKind) -> PP_Doc
ppChild (nm,tp,_)
  = pp nm >#< "::" >#< pp (show tp)

ppVertexMap :: Map Int (Identifier,Identifier,Maybe Type) -> PP_Doc
ppVertexMap m
  = ppVList [ ppF (show k) $ ppAttr v | (k,v) <- Map.toList m ]

ppAttr :: (Identifier,Identifier,Maybe Type) -> PP_Doc
ppAttr (fld,nm,mTp)
  = pp fld >|< "." >|< pp nm >#<
    case mTp of
      Just tp -> pp "::" >#< show tp
      Nothing -> empty

ppBool :: Bool -> PP_Doc
ppBool True  = pp "T"
ppBool False = pp "F"

ppMaybeShow :: Show a => Maybe a -> PP_Doc
ppMaybeShow (Just x) = pp (show x)
ppMaybeShow Nothing  = pp "_"

ppStrings :: [String] -> PP_Doc
ppStrings = vlist
{-# LINE 60 "dist/build/CodeSyntaxDump.hs" #-}
-- CGrammar ----------------------------------------------------
-- wrapper
data Inh_CGrammar  = Inh_CGrammar {  }
data Syn_CGrammar  = Syn_CGrammar { pp_Syn_CGrammar :: (PP_Doc) }
{-# INLINABLE wrap_CGrammar #-}
wrap_CGrammar :: T_CGrammar  -> Inh_CGrammar  -> (Syn_CGrammar )
wrap_CGrammar (T_CGrammar act) (Inh_CGrammar ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_CGrammar_vIn1 
        (T_CGrammar_vOut1 _lhsOpp) <- return (inv_CGrammar_s2 sem arg)
        return (Syn_CGrammar _lhsOpp)
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
data T_CGrammar_vOut1  = T_CGrammar_vOut1 (PP_Doc)
{-# NOINLINE sem_CGrammar_CGrammar #-}
sem_CGrammar_CGrammar :: (TypeSyns) -> (Derivings) -> (Set NontermIdent) -> T_CNonterminals  -> (PragmaMap) -> (ParamMap) -> (ContextMap) -> (QuantMap) -> (Map NontermIdent (Map ConstructorIdent (Set Identifier))) -> (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier])))) -> (Bool) -> T_CGrammar 
sem_CGrammar_CGrammar arg_typeSyns_ arg_derivings_ _ arg_nonts_ _ _ _ _ _ _ _ = T_CGrammar (return st2) where
   {-# NOINLINE st2 #-}
   st2 = let
      v1 :: T_CGrammar_v1 
      v1 = \ (T_CGrammar_vIn1 ) -> ( let
         _nontsX11 = Control.Monad.Identity.runIdentity (attach_T_CNonterminals (arg_nonts_))
         (T_CNonterminals_vOut10 _nontsIpp _nontsIppL) = inv_CNonterminals_s11 _nontsX11 (T_CNonterminals_vIn10 )
         _lhsOpp :: PP_Doc
         _lhsOpp = rule0 _nontsIppL arg_derivings_ arg_typeSyns_
         __result_ = T_CGrammar_vOut1 _lhsOpp
         in __result_ )
     in C_CGrammar_s2 v1
   {-# INLINE rule0 #-}
   {-# LINE 47 "./src-ag/CodeSyntaxDump.ag" #-}
   rule0 = \ ((_nontsIppL) :: [PP_Doc]) derivings_ typeSyns_ ->
                                              {-# LINE 47 "./src-ag/CodeSyntaxDump.ag" #-}
                                              ppNestInfo ["CGrammar","CGrammar"] []
                                                         [ ppF "typeSyns"  $ ppAssocL typeSyns_
                                                         , ppF "derivings" $ ppMap $ derivings_
                                                         , ppF "nonts"     $ ppVList _nontsIppL
                                                         ] []
                                              {-# LINE 114 "dist/build/CodeSyntaxDump.hs"#-}

-- CInterface --------------------------------------------------
-- wrapper
data Inh_CInterface  = Inh_CInterface {  }
data Syn_CInterface  = Syn_CInterface { pp_Syn_CInterface :: (PP_Doc) }
{-# INLINABLE wrap_CInterface #-}
wrap_CInterface :: T_CInterface  -> Inh_CInterface  -> (Syn_CInterface )
wrap_CInterface (T_CInterface act) (Inh_CInterface ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_CInterface_vIn4 
        (T_CInterface_vOut4 _lhsOpp) <- return (inv_CInterface_s5 sem arg)
        return (Syn_CInterface _lhsOpp)
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
data T_CInterface_vOut4  = T_CInterface_vOut4 (PP_Doc)
{-# NOINLINE sem_CInterface_CInterface #-}
sem_CInterface_CInterface :: T_CSegments  -> T_CInterface 
sem_CInterface_CInterface arg_seg_ = T_CInterface (return st5) where
   {-# NOINLINE st5 #-}
   st5 = let
      v4 :: T_CInterface_v4 
      v4 = \ (T_CInterface_vIn4 ) -> ( let
         _segX26 = Control.Monad.Identity.runIdentity (attach_T_CSegments (arg_seg_))
         (T_CSegments_vOut25 _segIpp _segIppL) = inv_CSegments_s26 _segX26 (T_CSegments_vIn25 )
         _lhsOpp :: PP_Doc
         _lhsOpp = rule1 _segIppL
         __result_ = T_CInterface_vOut4 _lhsOpp
         in __result_ )
     in C_CInterface_s5 v4
   {-# INLINE rule1 #-}
   {-# LINE 57 "./src-ag/CodeSyntaxDump.ag" #-}
   rule1 = \ ((_segIppL) :: [PP_Doc]) ->
                                              {-# LINE 57 "./src-ag/CodeSyntaxDump.ag" #-}
                                              ppNestInfo ["CInterface","CInterface"] [] [ppF "seg" $ ppVList _segIppL] []
                                              {-# LINE 165 "dist/build/CodeSyntaxDump.hs"#-}

-- CNonterminal ------------------------------------------------
-- wrapper
data Inh_CNonterminal  = Inh_CNonterminal {  }
data Syn_CNonterminal  = Syn_CNonterminal { pp_Syn_CNonterminal :: (PP_Doc) }
{-# INLINABLE wrap_CNonterminal #-}
wrap_CNonterminal :: T_CNonterminal  -> Inh_CNonterminal  -> (Syn_CNonterminal )
wrap_CNonterminal (T_CNonterminal act) (Inh_CNonterminal ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_CNonterminal_vIn7 
        (T_CNonterminal_vOut7 _lhsOpp) <- return (inv_CNonterminal_s8 sem arg)
        return (Syn_CNonterminal _lhsOpp)
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
data T_CNonterminal_vOut7  = T_CNonterminal_vOut7 (PP_Doc)
{-# NOINLINE sem_CNonterminal_CNonterminal #-}
sem_CNonterminal_CNonterminal :: (NontermIdent) -> ([Identifier]) -> (Attributes) -> (Attributes) -> T_CProductions  -> T_CInterface  -> T_CNonterminal 
sem_CNonterminal_CNonterminal arg_nt_ arg_params_ arg_inh_ arg_syn_ arg_prods_ arg_inter_ = T_CNonterminal (return st8) where
   {-# NOINLINE st8 #-}
   st8 = let
      v7 :: T_CNonterminal_v7 
      v7 = \ (T_CNonterminal_vIn7 ) -> ( let
         _prodsX17 = Control.Monad.Identity.runIdentity (attach_T_CProductions (arg_prods_))
         _interX5 = Control.Monad.Identity.runIdentity (attach_T_CInterface (arg_inter_))
         (T_CProductions_vOut16 _prodsIpp _prodsIppL) = inv_CProductions_s17 _prodsX17 (T_CProductions_vIn16 )
         (T_CInterface_vOut4 _interIpp) = inv_CInterface_s5 _interX5 (T_CInterface_vIn4 )
         _lhsOpp :: PP_Doc
         _lhsOpp = rule2 _interIpp _prodsIppL arg_inh_ arg_nt_ arg_params_ arg_syn_
         __result_ = T_CNonterminal_vOut7 _lhsOpp
         in __result_ )
     in C_CNonterminal_s8 v7
   {-# INLINE rule2 #-}
   {-# LINE 54 "./src-ag/CodeSyntaxDump.ag" #-}
   rule2 = \ ((_interIpp) :: PP_Doc) ((_prodsIppL) :: [PP_Doc]) inh_ nt_ params_ syn_ ->
                                                              {-# LINE 54 "./src-ag/CodeSyntaxDump.ag" #-}
                                                              ppNestInfo ["CNonterminal","CNonterminal"] (pp nt_ : map pp params_) [ppF "inh" $ ppMap inh_, ppF "syn" $ ppMap syn_, ppF "prods" $ ppVList _prodsIppL, ppF "inter" _interIpp] []
                                                              {-# LINE 218 "dist/build/CodeSyntaxDump.hs"#-}

-- CNonterminals -----------------------------------------------
-- wrapper
data Inh_CNonterminals  = Inh_CNonterminals {  }
data Syn_CNonterminals  = Syn_CNonterminals { pp_Syn_CNonterminals :: (PP_Doc), ppL_Syn_CNonterminals :: ([PP_Doc]) }
{-# INLINABLE wrap_CNonterminals #-}
wrap_CNonterminals :: T_CNonterminals  -> Inh_CNonterminals  -> (Syn_CNonterminals )
wrap_CNonterminals (T_CNonterminals act) (Inh_CNonterminals ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_CNonterminals_vIn10 
        (T_CNonterminals_vOut10 _lhsOpp _lhsOppL) <- return (inv_CNonterminals_s11 sem arg)
        return (Syn_CNonterminals _lhsOpp _lhsOppL)
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
data T_CNonterminals_vOut10  = T_CNonterminals_vOut10 (PP_Doc) ([PP_Doc])
{-# NOINLINE sem_CNonterminals_Cons #-}
sem_CNonterminals_Cons :: T_CNonterminal  -> T_CNonterminals  -> T_CNonterminals 
sem_CNonterminals_Cons arg_hd_ arg_tl_ = T_CNonterminals (return st11) where
   {-# NOINLINE st11 #-}
   st11 = let
      v10 :: T_CNonterminals_v10 
      v10 = \ (T_CNonterminals_vIn10 ) -> ( let
         _hdX8 = Control.Monad.Identity.runIdentity (attach_T_CNonterminal (arg_hd_))
         _tlX11 = Control.Monad.Identity.runIdentity (attach_T_CNonterminals (arg_tl_))
         (T_CNonterminal_vOut7 _hdIpp) = inv_CNonterminal_s8 _hdX8 (T_CNonterminal_vIn7 )
         (T_CNonterminals_vOut10 _tlIpp _tlIppL) = inv_CNonterminals_s11 _tlX11 (T_CNonterminals_vIn10 )
         _lhsOppL :: [PP_Doc]
         _lhsOppL = rule3 _hdIpp _tlIppL
         _lhsOpp :: PP_Doc
         _lhsOpp = rule4 _hdIpp _tlIpp
         __result_ = T_CNonterminals_vOut10 _lhsOpp _lhsOppL
         in __result_ )
     in C_CNonterminals_s11 v10
   {-# INLINE rule3 #-}
   {-# LINE 102 "./src-ag/CodeSyntaxDump.ag" #-}
   rule3 = \ ((_hdIpp) :: PP_Doc) ((_tlIppL) :: [PP_Doc]) ->
                                                                                  {-# LINE 102 "./src-ag/CodeSyntaxDump.ag" #-}
                                                                                  _hdIpp : _tlIppL
                                                                                  {-# LINE 273 "dist/build/CodeSyntaxDump.hs"#-}
   {-# INLINE rule4 #-}
   rule4 = \ ((_hdIpp) :: PP_Doc) ((_tlIpp) :: PP_Doc) ->
     _hdIpp >-< _tlIpp
{-# NOINLINE sem_CNonterminals_Nil #-}
sem_CNonterminals_Nil ::  T_CNonterminals 
sem_CNonterminals_Nil  = T_CNonterminals (return st11) where
   {-# NOINLINE st11 #-}
   st11 = let
      v10 :: T_CNonterminals_v10 
      v10 = \ (T_CNonterminals_vIn10 ) -> ( let
         _lhsOppL :: [PP_Doc]
         _lhsOppL = rule5  ()
         _lhsOpp :: PP_Doc
         _lhsOpp = rule6  ()
         __result_ = T_CNonterminals_vOut10 _lhsOpp _lhsOppL
         in __result_ )
     in C_CNonterminals_s11 v10
   {-# INLINE rule5 #-}
   {-# LINE 103 "./src-ag/CodeSyntaxDump.ag" #-}
   rule5 = \  (_ :: ()) ->
                                                                                  {-# LINE 103 "./src-ag/CodeSyntaxDump.ag" #-}
                                                                                  []
                                                                                  {-# LINE 296 "dist/build/CodeSyntaxDump.hs"#-}
   {-# INLINE rule6 #-}
   rule6 = \  (_ :: ()) ->
     empty

-- CProduction -------------------------------------------------
-- wrapper
data Inh_CProduction  = Inh_CProduction {  }
data Syn_CProduction  = Syn_CProduction { pp_Syn_CProduction :: (PP_Doc) }
{-# INLINABLE wrap_CProduction #-}
wrap_CProduction :: T_CProduction  -> Inh_CProduction  -> (Syn_CProduction )
wrap_CProduction (T_CProduction act) (Inh_CProduction ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_CProduction_vIn13 
        (T_CProduction_vOut13 _lhsOpp) <- return (inv_CProduction_s14 sem arg)
        return (Syn_CProduction _lhsOpp)
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
data T_CProduction_vOut13  = T_CProduction_vOut13 (PP_Doc)
{-# NOINLINE sem_CProduction_CProduction #-}
sem_CProduction_CProduction :: (ConstructorIdent) -> T_CVisits  -> ([(Identifier,Type,ChildKind)]) -> ([Identifier]) -> T_CProduction 
sem_CProduction_CProduction arg_con_ arg_visits_ arg_children_ arg_terminals_ = T_CProduction (return st14) where
   {-# NOINLINE st14 #-}
   st14 = let
      v13 :: T_CProduction_v13 
      v13 = \ (T_CProduction_vIn13 ) -> ( let
         _visitsX32 = Control.Monad.Identity.runIdentity (attach_T_CVisits (arg_visits_))
         (T_CVisits_vOut31 _visitsIpp _visitsIppL) = inv_CVisits_s32 _visitsX32 (T_CVisits_vIn31 )
         _lhsOpp :: PP_Doc
         _lhsOpp = rule7 _visitsIppL arg_children_ arg_con_ arg_terminals_
         __result_ = T_CProduction_vOut13 _lhsOpp
         in __result_ )
     in C_CProduction_s14 v13
   {-# INLINE rule7 #-}
   {-# LINE 63 "./src-ag/CodeSyntaxDump.ag" #-}
   rule7 = \ ((_visitsIppL) :: [PP_Doc]) children_ con_ terminals_ ->
                                              {-# LINE 63 "./src-ag/CodeSyntaxDump.ag" #-}
                                              ppNestInfo ["CProduction","CProduction"] [pp con_] [ppF "visits" $ ppVList _visitsIppL, ppF "children" $ ppVList (map ppChild children_),ppF "terminals" $ ppVList (map ppShow terminals_)] []
                                              {-# LINE 350 "dist/build/CodeSyntaxDump.hs"#-}

-- CProductions ------------------------------------------------
-- wrapper
data Inh_CProductions  = Inh_CProductions {  }
data Syn_CProductions  = Syn_CProductions { pp_Syn_CProductions :: (PP_Doc), ppL_Syn_CProductions :: ([PP_Doc]) }
{-# INLINABLE wrap_CProductions #-}
wrap_CProductions :: T_CProductions  -> Inh_CProductions  -> (Syn_CProductions )
wrap_CProductions (T_CProductions act) (Inh_CProductions ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_CProductions_vIn16 
        (T_CProductions_vOut16 _lhsOpp _lhsOppL) <- return (inv_CProductions_s17 sem arg)
        return (Syn_CProductions _lhsOpp _lhsOppL)
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
data T_CProductions_vOut16  = T_CProductions_vOut16 (PP_Doc) ([PP_Doc])
{-# NOINLINE sem_CProductions_Cons #-}
sem_CProductions_Cons :: T_CProduction  -> T_CProductions  -> T_CProductions 
sem_CProductions_Cons arg_hd_ arg_tl_ = T_CProductions (return st17) where
   {-# NOINLINE st17 #-}
   st17 = let
      v16 :: T_CProductions_v16 
      v16 = \ (T_CProductions_vIn16 ) -> ( let
         _hdX14 = Control.Monad.Identity.runIdentity (attach_T_CProduction (arg_hd_))
         _tlX17 = Control.Monad.Identity.runIdentity (attach_T_CProductions (arg_tl_))
         (T_CProduction_vOut13 _hdIpp) = inv_CProduction_s14 _hdX14 (T_CProduction_vIn13 )
         (T_CProductions_vOut16 _tlIpp _tlIppL) = inv_CProductions_s17 _tlX17 (T_CProductions_vIn16 )
         _lhsOppL :: [PP_Doc]
         _lhsOppL = rule8 _hdIpp _tlIppL
         _lhsOpp :: PP_Doc
         _lhsOpp = rule9 _hdIpp _tlIpp
         __result_ = T_CProductions_vOut16 _lhsOpp _lhsOppL
         in __result_ )
     in C_CProductions_s17 v16
   {-# INLINE rule8 #-}
   {-# LINE 94 "./src-ag/CodeSyntaxDump.ag" #-}
   rule8 = \ ((_hdIpp) :: PP_Doc) ((_tlIppL) :: [PP_Doc]) ->
                                                                                  {-# LINE 94 "./src-ag/CodeSyntaxDump.ag" #-}
                                                                                  _hdIpp : _tlIppL
                                                                                  {-# LINE 405 "dist/build/CodeSyntaxDump.hs"#-}
   {-# INLINE rule9 #-}
   rule9 = \ ((_hdIpp) :: PP_Doc) ((_tlIpp) :: PP_Doc) ->
     _hdIpp >-< _tlIpp
{-# NOINLINE sem_CProductions_Nil #-}
sem_CProductions_Nil ::  T_CProductions 
sem_CProductions_Nil  = T_CProductions (return st17) where
   {-# NOINLINE st17 #-}
   st17 = let
      v16 :: T_CProductions_v16 
      v16 = \ (T_CProductions_vIn16 ) -> ( let
         _lhsOppL :: [PP_Doc]
         _lhsOppL = rule10  ()
         _lhsOpp :: PP_Doc
         _lhsOpp = rule11  ()
         __result_ = T_CProductions_vOut16 _lhsOpp _lhsOppL
         in __result_ )
     in C_CProductions_s17 v16
   {-# INLINE rule10 #-}
   {-# LINE 95 "./src-ag/CodeSyntaxDump.ag" #-}
   rule10 = \  (_ :: ()) ->
                                                                                  {-# LINE 95 "./src-ag/CodeSyntaxDump.ag" #-}
                                                                                  []
                                                                                  {-# LINE 428 "dist/build/CodeSyntaxDump.hs"#-}
   {-# INLINE rule11 #-}
   rule11 = \  (_ :: ()) ->
     empty

-- CRule -------------------------------------------------------
-- wrapper
data Inh_CRule  = Inh_CRule {  }
data Syn_CRule  = Syn_CRule { pp_Syn_CRule :: (PP_Doc) }
{-# INLINABLE wrap_CRule #-}
wrap_CRule :: T_CRule  -> Inh_CRule  -> (Syn_CRule )
wrap_CRule (T_CRule act) (Inh_CRule ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_CRule_vIn19 
        (T_CRule_vOut19 _lhsOpp) <- return (inv_CRule_s20 sem arg)
        return (Syn_CRule _lhsOpp)
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
data T_CRule_vOut19  = T_CRule_vOut19 (PP_Doc)
{-# NOINLINE sem_CRule_CRule #-}
sem_CRule_CRule :: (Identifier) -> (Bool) -> (Bool) -> (NontermIdent) -> (ConstructorIdent) -> (Identifier) -> (Maybe NontermIdent) -> (Maybe Type) -> T_Pattern  -> ([String]) -> (Map Int (Identifier,Identifier,Maybe Type)) -> (Bool) -> (String) -> (Set (Identifier, Identifier)) -> (Bool) -> (Maybe Identifier) -> T_CRule 
sem_CRule_CRule arg_name_ arg_isIn_ arg_hasCode_ arg_nt_ arg_con_ arg_field_ arg_childnt_ arg_tp_ arg_pattern_ arg_rhs_ arg_defines_ arg_owrt_ arg_origin_ _ _ _ = T_CRule (return st20) where
   {-# NOINLINE st20 #-}
   st20 = let
      v19 :: T_CRule_v19 
      v19 = \ (T_CRule_vIn19 ) -> ( let
         _patternX35 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pattern_))
         (T_Pattern_vOut34 _patternIcopy _patternIpp) = inv_Pattern_s35 _patternX35 (T_Pattern_vIn34 )
         _lhsOpp :: PP_Doc
         _lhsOpp = rule12 _patternIpp arg_childnt_ arg_con_ arg_defines_ arg_field_ arg_hasCode_ arg_isIn_ arg_name_ arg_nt_ arg_origin_ arg_owrt_ arg_rhs_ arg_tp_
         __result_ = T_CRule_vOut19 _lhsOpp
         in __result_ )
     in C_CRule_s20 v19
   {-# INLINE rule12 #-}
   {-# LINE 69 "./src-ag/CodeSyntaxDump.ag" #-}
   rule12 = \ ((_patternIpp) :: PP_Doc) childnt_ con_ defines_ field_ hasCode_ isIn_ name_ nt_ origin_ owrt_ rhs_ tp_ ->
                                                              {-# LINE 69 "./src-ag/CodeSyntaxDump.ag" #-}
                                                              ppNestInfo ["CRule","CRule"] [pp name_] [ppF "isIn" $ ppBool isIn_, ppF "hasCode" $ ppBool hasCode_, ppF "nt" $ pp nt_, ppF "con" $ pp con_, ppF "field" $ pp field_, ppF "childnt" $ ppMaybeShow childnt_, ppF "tp" $ ppMaybeShow tp_, ppF "pattern" $ if isIn_ then pp "<no pat because In>" else _patternIpp, ppF "rhs" $ ppStrings rhs_, ppF "defines" $ ppVertexMap defines_, ppF "owrt" $ ppBool owrt_, ppF "origin" $ pp origin_] []
                                                              {-# LINE 483 "dist/build/CodeSyntaxDump.hs"#-}
{-# NOINLINE sem_CRule_CChildVisit #-}
sem_CRule_CChildVisit :: (Identifier) -> (NontermIdent) -> (Int) -> (Attributes) -> (Attributes) -> (Bool) -> T_CRule 
sem_CRule_CChildVisit arg_name_ arg_nt_ arg_nr_ arg_inh_ arg_syn_ arg_isLast_ = T_CRule (return st20) where
   {-# NOINLINE st20 #-}
   st20 = let
      v19 :: T_CRule_v19 
      v19 = \ (T_CRule_vIn19 ) -> ( let
         _lhsOpp :: PP_Doc
         _lhsOpp = rule13 arg_inh_ arg_isLast_ arg_name_ arg_nr_ arg_nt_ arg_syn_
         __result_ = T_CRule_vOut19 _lhsOpp
         in __result_ )
     in C_CRule_s20 v19
   {-# INLINE rule13 #-}
   {-# LINE 70 "./src-ag/CodeSyntaxDump.ag" #-}
   rule13 = \ inh_ isLast_ name_ nr_ nt_ syn_ ->
                                              {-# LINE 70 "./src-ag/CodeSyntaxDump.ag" #-}
                                              ppNestInfo ["CRule","CChildVisit"] [pp name_] [ppF "nt" $ pp nt_, ppF "nr" $ ppShow nr_, ppF "inh" $ ppMap inh_, ppF "syn" $ ppMap syn_, ppF "last" $ ppBool isLast_] []
                                              {-# LINE 501 "dist/build/CodeSyntaxDump.hs"#-}

-- CSegment ----------------------------------------------------
-- wrapper
data Inh_CSegment  = Inh_CSegment {  }
data Syn_CSegment  = Syn_CSegment { pp_Syn_CSegment :: (PP_Doc) }
{-# INLINABLE wrap_CSegment #-}
wrap_CSegment :: T_CSegment  -> Inh_CSegment  -> (Syn_CSegment )
wrap_CSegment (T_CSegment act) (Inh_CSegment ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_CSegment_vIn22 
        (T_CSegment_vOut22 _lhsOpp) <- return (inv_CSegment_s23 sem arg)
        return (Syn_CSegment _lhsOpp)
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
data T_CSegment_vOut22  = T_CSegment_vOut22 (PP_Doc)
{-# NOINLINE sem_CSegment_CSegment #-}
sem_CSegment_CSegment :: (Attributes) -> (Attributes) -> T_CSegment 
sem_CSegment_CSegment arg_inh_ arg_syn_ = T_CSegment (return st23) where
   {-# NOINLINE st23 #-}
   st23 = let
      v22 :: T_CSegment_v22 
      v22 = \ (T_CSegment_vIn22 ) -> ( let
         _lhsOpp :: PP_Doc
         _lhsOpp = rule14 arg_inh_ arg_syn_
         __result_ = T_CSegment_vOut22 _lhsOpp
         in __result_ )
     in C_CSegment_s23 v22
   {-# INLINE rule14 #-}
   {-# LINE 60 "./src-ag/CodeSyntaxDump.ag" #-}
   rule14 = \ inh_ syn_ ->
                                              {-# LINE 60 "./src-ag/CodeSyntaxDump.ag" #-}
                                              ppNestInfo ["CSegment","CSegment"] [] [ppF "inh" $ ppMap inh_, ppF "syn" $ ppMap syn_] []
                                              {-# LINE 550 "dist/build/CodeSyntaxDump.hs"#-}

-- CSegments ---------------------------------------------------
-- wrapper
data Inh_CSegments  = Inh_CSegments {  }
data Syn_CSegments  = Syn_CSegments { pp_Syn_CSegments :: (PP_Doc), ppL_Syn_CSegments :: ([PP_Doc]) }
{-# INLINABLE wrap_CSegments #-}
wrap_CSegments :: T_CSegments  -> Inh_CSegments  -> (Syn_CSegments )
wrap_CSegments (T_CSegments act) (Inh_CSegments ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_CSegments_vIn25 
        (T_CSegments_vOut25 _lhsOpp _lhsOppL) <- return (inv_CSegments_s26 sem arg)
        return (Syn_CSegments _lhsOpp _lhsOppL)
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
data T_CSegments_vOut25  = T_CSegments_vOut25 (PP_Doc) ([PP_Doc])
{-# NOINLINE sem_CSegments_Cons #-}
sem_CSegments_Cons :: T_CSegment  -> T_CSegments  -> T_CSegments 
sem_CSegments_Cons arg_hd_ arg_tl_ = T_CSegments (return st26) where
   {-# NOINLINE st26 #-}
   st26 = let
      v25 :: T_CSegments_v25 
      v25 = \ (T_CSegments_vIn25 ) -> ( let
         _hdX23 = Control.Monad.Identity.runIdentity (attach_T_CSegment (arg_hd_))
         _tlX26 = Control.Monad.Identity.runIdentity (attach_T_CSegments (arg_tl_))
         (T_CSegment_vOut22 _hdIpp) = inv_CSegment_s23 _hdX23 (T_CSegment_vIn22 )
         (T_CSegments_vOut25 _tlIpp _tlIppL) = inv_CSegments_s26 _tlX26 (T_CSegments_vIn25 )
         _lhsOppL :: [PP_Doc]
         _lhsOppL = rule15 _hdIpp _tlIppL
         _lhsOpp :: PP_Doc
         _lhsOpp = rule16 _hdIpp _tlIpp
         __result_ = T_CSegments_vOut25 _lhsOpp _lhsOppL
         in __result_ )
     in C_CSegments_s26 v25
   {-# INLINE rule15 #-}
   {-# LINE 98 "./src-ag/CodeSyntaxDump.ag" #-}
   rule15 = \ ((_hdIpp) :: PP_Doc) ((_tlIppL) :: [PP_Doc]) ->
                                                                                  {-# LINE 98 "./src-ag/CodeSyntaxDump.ag" #-}
                                                                                  _hdIpp : _tlIppL
                                                                                  {-# LINE 605 "dist/build/CodeSyntaxDump.hs"#-}
   {-# INLINE rule16 #-}
   rule16 = \ ((_hdIpp) :: PP_Doc) ((_tlIpp) :: PP_Doc) ->
     _hdIpp >-< _tlIpp
{-# NOINLINE sem_CSegments_Nil #-}
sem_CSegments_Nil ::  T_CSegments 
sem_CSegments_Nil  = T_CSegments (return st26) where
   {-# NOINLINE st26 #-}
   st26 = let
      v25 :: T_CSegments_v25 
      v25 = \ (T_CSegments_vIn25 ) -> ( let
         _lhsOppL :: [PP_Doc]
         _lhsOppL = rule17  ()
         _lhsOpp :: PP_Doc
         _lhsOpp = rule18  ()
         __result_ = T_CSegments_vOut25 _lhsOpp _lhsOppL
         in __result_ )
     in C_CSegments_s26 v25
   {-# INLINE rule17 #-}
   {-# LINE 99 "./src-ag/CodeSyntaxDump.ag" #-}
   rule17 = \  (_ :: ()) ->
                                                                                  {-# LINE 99 "./src-ag/CodeSyntaxDump.ag" #-}
                                                                                  []
                                                                                  {-# LINE 628 "dist/build/CodeSyntaxDump.hs"#-}
   {-# INLINE rule18 #-}
   rule18 = \  (_ :: ()) ->
     empty

-- CVisit ------------------------------------------------------
-- wrapper
data Inh_CVisit  = Inh_CVisit {  }
data Syn_CVisit  = Syn_CVisit { pp_Syn_CVisit :: (PP_Doc) }
{-# INLINABLE wrap_CVisit #-}
wrap_CVisit :: T_CVisit  -> Inh_CVisit  -> (Syn_CVisit )
wrap_CVisit (T_CVisit act) (Inh_CVisit ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_CVisit_vIn28 
        (T_CVisit_vOut28 _lhsOpp) <- return (inv_CVisit_s29 sem arg)
        return (Syn_CVisit _lhsOpp)
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
data T_CVisit_vOut28  = T_CVisit_vOut28 (PP_Doc)
{-# NOINLINE sem_CVisit_CVisit #-}
sem_CVisit_CVisit :: (Attributes) -> (Attributes) -> T_Sequence  -> T_Sequence  -> (Bool) -> T_CVisit 
sem_CVisit_CVisit arg_inh_ arg_syn_ arg_vss_ arg_intra_ arg_ordered_ = T_CVisit (return st29) where
   {-# NOINLINE st29 #-}
   st29 = let
      v28 :: T_CVisit_v28 
      v28 = \ (T_CVisit_vIn28 ) -> ( let
         _vssX41 = Control.Monad.Identity.runIdentity (attach_T_Sequence (arg_vss_))
         _intraX41 = Control.Monad.Identity.runIdentity (attach_T_Sequence (arg_intra_))
         (T_Sequence_vOut40 _vssIppL) = inv_Sequence_s41 _vssX41 (T_Sequence_vIn40 )
         (T_Sequence_vOut40 _intraIppL) = inv_Sequence_s41 _intraX41 (T_Sequence_vIn40 )
         _lhsOpp :: PP_Doc
         _lhsOpp = rule19 _intraIppL _vssIppL arg_inh_ arg_ordered_ arg_syn_
         __result_ = T_CVisit_vOut28 _lhsOpp
         in __result_ )
     in C_CVisit_s29 v28
   {-# INLINE rule19 #-}
   {-# LINE 66 "./src-ag/CodeSyntaxDump.ag" #-}
   rule19 = \ ((_intraIppL) :: [PP_Doc]) ((_vssIppL) :: [PP_Doc]) inh_ ordered_ syn_ ->
                                              {-# LINE 66 "./src-ag/CodeSyntaxDump.ag" #-}
                                              ppNestInfo ["CVisit","CVisit"] [] [ppF "inh" $ ppMap inh_, ppF "syn" $ ppMap syn_, ppF "sequence" $ ppVList _vssIppL, ppF "intra" $ ppVList _intraIppL, ppF "ordered" $ ppBool ordered_] []
                                              {-# LINE 684 "dist/build/CodeSyntaxDump.hs"#-}

-- CVisits -----------------------------------------------------
-- wrapper
data Inh_CVisits  = Inh_CVisits {  }
data Syn_CVisits  = Syn_CVisits { pp_Syn_CVisits :: (PP_Doc), ppL_Syn_CVisits :: ([PP_Doc]) }
{-# INLINABLE wrap_CVisits #-}
wrap_CVisits :: T_CVisits  -> Inh_CVisits  -> (Syn_CVisits )
wrap_CVisits (T_CVisits act) (Inh_CVisits ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_CVisits_vIn31 
        (T_CVisits_vOut31 _lhsOpp _lhsOppL) <- return (inv_CVisits_s32 sem arg)
        return (Syn_CVisits _lhsOpp _lhsOppL)
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
data T_CVisits_vOut31  = T_CVisits_vOut31 (PP_Doc) ([PP_Doc])
{-# NOINLINE sem_CVisits_Cons #-}
sem_CVisits_Cons :: T_CVisit  -> T_CVisits  -> T_CVisits 
sem_CVisits_Cons arg_hd_ arg_tl_ = T_CVisits (return st32) where
   {-# NOINLINE st32 #-}
   st32 = let
      v31 :: T_CVisits_v31 
      v31 = \ (T_CVisits_vIn31 ) -> ( let
         _hdX29 = Control.Monad.Identity.runIdentity (attach_T_CVisit (arg_hd_))
         _tlX32 = Control.Monad.Identity.runIdentity (attach_T_CVisits (arg_tl_))
         (T_CVisit_vOut28 _hdIpp) = inv_CVisit_s29 _hdX29 (T_CVisit_vIn28 )
         (T_CVisits_vOut31 _tlIpp _tlIppL) = inv_CVisits_s32 _tlX32 (T_CVisits_vIn31 )
         _lhsOppL :: [PP_Doc]
         _lhsOppL = rule20 _hdIpp _tlIppL
         _lhsOpp :: PP_Doc
         _lhsOpp = rule21 _hdIpp _tlIpp
         __result_ = T_CVisits_vOut31 _lhsOpp _lhsOppL
         in __result_ )
     in C_CVisits_s32 v31
   {-# INLINE rule20 #-}
   {-# LINE 90 "./src-ag/CodeSyntaxDump.ag" #-}
   rule20 = \ ((_hdIpp) :: PP_Doc) ((_tlIppL) :: [PP_Doc]) ->
                                                                                  {-# LINE 90 "./src-ag/CodeSyntaxDump.ag" #-}
                                                                                  _hdIpp : _tlIppL
                                                                                  {-# LINE 739 "dist/build/CodeSyntaxDump.hs"#-}
   {-# INLINE rule21 #-}
   rule21 = \ ((_hdIpp) :: PP_Doc) ((_tlIpp) :: PP_Doc) ->
     _hdIpp >-< _tlIpp
{-# NOINLINE sem_CVisits_Nil #-}
sem_CVisits_Nil ::  T_CVisits 
sem_CVisits_Nil  = T_CVisits (return st32) where
   {-# NOINLINE st32 #-}
   st32 = let
      v31 :: T_CVisits_v31 
      v31 = \ (T_CVisits_vIn31 ) -> ( let
         _lhsOppL :: [PP_Doc]
         _lhsOppL = rule22  ()
         _lhsOpp :: PP_Doc
         _lhsOpp = rule23  ()
         __result_ = T_CVisits_vOut31 _lhsOpp _lhsOppL
         in __result_ )
     in C_CVisits_s32 v31
   {-# INLINE rule22 #-}
   {-# LINE 91 "./src-ag/CodeSyntaxDump.ag" #-}
   rule22 = \  (_ :: ()) ->
                                                                                  {-# LINE 91 "./src-ag/CodeSyntaxDump.ag" #-}
                                                                                  []
                                                                                  {-# LINE 762 "dist/build/CodeSyntaxDump.hs"#-}
   {-# INLINE rule23 #-}
   rule23 = \  (_ :: ()) ->
     empty

-- Pattern -----------------------------------------------------
-- wrapper
data Inh_Pattern  = Inh_Pattern {  }
data Syn_Pattern  = Syn_Pattern { copy_Syn_Pattern :: (Pattern), pp_Syn_Pattern :: (PP_Doc) }
{-# INLINABLE wrap_Pattern #-}
wrap_Pattern :: T_Pattern  -> Inh_Pattern  -> (Syn_Pattern )
wrap_Pattern (T_Pattern act) (Inh_Pattern ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Pattern_vIn34 
        (T_Pattern_vOut34 _lhsOcopy _lhsOpp) <- return (inv_Pattern_s35 sem arg)
        return (Syn_Pattern _lhsOcopy _lhsOpp)
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
                               attach_T_Pattern :: Identity (T_Pattern_s35 )
                               }
newtype T_Pattern_s35  = C_Pattern_s35 {
                                       inv_Pattern_s35 :: (T_Pattern_v34 )
                                       }
data T_Pattern_s36  = C_Pattern_s36
type T_Pattern_v34  = (T_Pattern_vIn34 ) -> (T_Pattern_vOut34 )
data T_Pattern_vIn34  = T_Pattern_vIn34 
data T_Pattern_vOut34  = T_Pattern_vOut34 (Pattern) (PP_Doc)
{-# NOINLINE sem_Pattern_Constr #-}
sem_Pattern_Constr :: (ConstructorIdent) -> T_Patterns  -> T_Pattern 
sem_Pattern_Constr arg_name_ arg_pats_ = T_Pattern (return st35) where
   {-# NOINLINE st35 #-}
   st35 = let
      v34 :: T_Pattern_v34 
      v34 = \ (T_Pattern_vIn34 ) -> ( let
         _patsX38 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_pats_))
         (T_Patterns_vOut37 _patsIcopy _patsIpp _patsIppL) = inv_Patterns_s38 _patsX38 (T_Patterns_vIn37 )
         _lhsOpp :: PP_Doc
         _lhsOpp = rule24 _patsIppL arg_name_
         _copy = rule25 _patsIcopy arg_name_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule26 _copy
         __result_ = T_Pattern_vOut34 _lhsOcopy _lhsOpp
         in __result_ )
     in C_Pattern_s35 v34
   {-# INLINE rule24 #-}
   {-# LINE 73 "./src-ag/CodeSyntaxDump.ag" #-}
   rule24 = \ ((_patsIppL) :: [PP_Doc]) name_ ->
                                                              {-# LINE 73 "./src-ag/CodeSyntaxDump.ag" #-}
                                                              ppNestInfo ["Pattern","Constr"] [pp name_] [ppF "pats" $ ppVList _patsIppL] []
                                                              {-# LINE 823 "dist/build/CodeSyntaxDump.hs"#-}
   {-# INLINE rule25 #-}
   rule25 = \ ((_patsIcopy) :: Patterns) name_ ->
     Constr name_ _patsIcopy
   {-# INLINE rule26 #-}
   rule26 = \ _copy ->
     _copy
{-# NOINLINE sem_Pattern_Product #-}
sem_Pattern_Product :: (Pos) -> T_Patterns  -> T_Pattern 
sem_Pattern_Product arg_pos_ arg_pats_ = T_Pattern (return st35) where
   {-# NOINLINE st35 #-}
   st35 = let
      v34 :: T_Pattern_v34 
      v34 = \ (T_Pattern_vIn34 ) -> ( let
         _patsX38 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_pats_))
         (T_Patterns_vOut37 _patsIcopy _patsIpp _patsIppL) = inv_Patterns_s38 _patsX38 (T_Patterns_vIn37 )
         _lhsOpp :: PP_Doc
         _lhsOpp = rule27 _patsIppL arg_pos_
         _copy = rule28 _patsIcopy arg_pos_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule29 _copy
         __result_ = T_Pattern_vOut34 _lhsOcopy _lhsOpp
         in __result_ )
     in C_Pattern_s35 v34
   {-# INLINE rule27 #-}
   {-# LINE 74 "./src-ag/CodeSyntaxDump.ag" #-}
   rule27 = \ ((_patsIppL) :: [PP_Doc]) pos_ ->
                                                              {-# LINE 74 "./src-ag/CodeSyntaxDump.ag" #-}
                                                              ppNestInfo ["Pattern","Product"] [ppShow pos_] [ppF "pats" $ ppVList _patsIppL] []
                                                              {-# LINE 852 "dist/build/CodeSyntaxDump.hs"#-}
   {-# INLINE rule28 #-}
   rule28 = \ ((_patsIcopy) :: Patterns) pos_ ->
     Product pos_ _patsIcopy
   {-# INLINE rule29 #-}
   rule29 = \ _copy ->
     _copy
{-# NOINLINE sem_Pattern_Alias #-}
sem_Pattern_Alias :: (Identifier) -> (Identifier) -> T_Pattern  -> T_Pattern 
sem_Pattern_Alias arg_field_ arg_attr_ arg_pat_ = T_Pattern (return st35) where
   {-# NOINLINE st35 #-}
   st35 = let
      v34 :: T_Pattern_v34 
      v34 = \ (T_Pattern_vIn34 ) -> ( let
         _patX35 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pat_))
         (T_Pattern_vOut34 _patIcopy _patIpp) = inv_Pattern_s35 _patX35 (T_Pattern_vIn34 )
         _lhsOpp :: PP_Doc
         _lhsOpp = rule30 _patIpp arg_attr_ arg_field_
         _copy = rule31 _patIcopy arg_attr_ arg_field_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule32 _copy
         __result_ = T_Pattern_vOut34 _lhsOcopy _lhsOpp
         in __result_ )
     in C_Pattern_s35 v34
   {-# INLINE rule30 #-}
   {-# LINE 75 "./src-ag/CodeSyntaxDump.ag" #-}
   rule30 = \ ((_patIpp) :: PP_Doc) attr_ field_ ->
                                                              {-# LINE 75 "./src-ag/CodeSyntaxDump.ag" #-}
                                                              ppNestInfo ["Pattern","Alias"] [pp field_, pp attr_] [ppF "pat" $ _patIpp] []
                                                              {-# LINE 881 "dist/build/CodeSyntaxDump.hs"#-}
   {-# INLINE rule31 #-}
   rule31 = \ ((_patIcopy) :: Pattern) attr_ field_ ->
     Alias field_ attr_ _patIcopy
   {-# INLINE rule32 #-}
   rule32 = \ _copy ->
     _copy
{-# NOINLINE sem_Pattern_Irrefutable #-}
sem_Pattern_Irrefutable :: T_Pattern  -> T_Pattern 
sem_Pattern_Irrefutable arg_pat_ = T_Pattern (return st35) where
   {-# NOINLINE st35 #-}
   st35 = let
      v34 :: T_Pattern_v34 
      v34 = \ (T_Pattern_vIn34 ) -> ( let
         _patX35 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pat_))
         (T_Pattern_vOut34 _patIcopy _patIpp) = inv_Pattern_s35 _patX35 (T_Pattern_vIn34 )
         _lhsOpp :: PP_Doc
         _lhsOpp = rule33 _patIpp
         _copy = rule34 _patIcopy
         _lhsOcopy :: Pattern
         _lhsOcopy = rule35 _copy
         __result_ = T_Pattern_vOut34 _lhsOcopy _lhsOpp
         in __result_ )
     in C_Pattern_s35 v34
   {-# INLINE rule33 #-}
   rule33 = \ ((_patIpp) :: PP_Doc) ->
     _patIpp
   {-# INLINE rule34 #-}
   rule34 = \ ((_patIcopy) :: Pattern) ->
     Irrefutable _patIcopy
   {-# INLINE rule35 #-}
   rule35 = \ _copy ->
     _copy
{-# NOINLINE sem_Pattern_Underscore #-}
sem_Pattern_Underscore :: (Pos) -> T_Pattern 
sem_Pattern_Underscore arg_pos_ = T_Pattern (return st35) where
   {-# NOINLINE st35 #-}
   st35 = let
      v34 :: T_Pattern_v34 
      v34 = \ (T_Pattern_vIn34 ) -> ( let
         _lhsOpp :: PP_Doc
         _lhsOpp = rule36 arg_pos_
         _copy = rule37 arg_pos_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule38 _copy
         __result_ = T_Pattern_vOut34 _lhsOcopy _lhsOpp
         in __result_ )
     in C_Pattern_s35 v34
   {-# INLINE rule36 #-}
   {-# LINE 76 "./src-ag/CodeSyntaxDump.ag" #-}
   rule36 = \ pos_ ->
                                                      {-# LINE 76 "./src-ag/CodeSyntaxDump.ag" #-}
                                                      ppNestInfo ["Pattern","Underscore"] [ppShow pos_] [] []
                                                      {-# LINE 934 "dist/build/CodeSyntaxDump.hs"#-}
   {-# INLINE rule37 #-}
   rule37 = \ pos_ ->
     Underscore pos_
   {-# INLINE rule38 #-}
   rule38 = \ _copy ->
     _copy

-- Patterns ----------------------------------------------------
-- wrapper
data Inh_Patterns  = Inh_Patterns {  }
data Syn_Patterns  = Syn_Patterns { copy_Syn_Patterns :: (Patterns), pp_Syn_Patterns :: (PP_Doc), ppL_Syn_Patterns :: ([PP_Doc]) }
{-# INLINABLE wrap_Patterns #-}
wrap_Patterns :: T_Patterns  -> Inh_Patterns  -> (Syn_Patterns )
wrap_Patterns (T_Patterns act) (Inh_Patterns ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Patterns_vIn37 
        (T_Patterns_vOut37 _lhsOcopy _lhsOpp _lhsOppL) <- return (inv_Patterns_s38 sem arg)
        return (Syn_Patterns _lhsOcopy _lhsOpp _lhsOppL)
   )

-- cata
{-# NOINLINE sem_Patterns #-}
sem_Patterns :: Patterns  -> T_Patterns 
sem_Patterns list = Prelude.foldr sem_Patterns_Cons sem_Patterns_Nil (Prelude.map sem_Pattern list)

-- semantic domain
newtype T_Patterns  = T_Patterns {
                                 attach_T_Patterns :: Identity (T_Patterns_s38 )
                                 }
newtype T_Patterns_s38  = C_Patterns_s38 {
                                         inv_Patterns_s38 :: (T_Patterns_v37 )
                                         }
data T_Patterns_s39  = C_Patterns_s39
type T_Patterns_v37  = (T_Patterns_vIn37 ) -> (T_Patterns_vOut37 )
data T_Patterns_vIn37  = T_Patterns_vIn37 
data T_Patterns_vOut37  = T_Patterns_vOut37 (Patterns) (PP_Doc) ([PP_Doc])
{-# NOINLINE sem_Patterns_Cons #-}
sem_Patterns_Cons :: T_Pattern  -> T_Patterns  -> T_Patterns 
sem_Patterns_Cons arg_hd_ arg_tl_ = T_Patterns (return st38) where
   {-# NOINLINE st38 #-}
   st38 = let
      v37 :: T_Patterns_v37 
      v37 = \ (T_Patterns_vIn37 ) -> ( let
         _hdX35 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_hd_))
         _tlX38 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_tl_))
         (T_Pattern_vOut34 _hdIcopy _hdIpp) = inv_Pattern_s35 _hdX35 (T_Pattern_vIn34 )
         (T_Patterns_vOut37 _tlIcopy _tlIpp _tlIppL) = inv_Patterns_s38 _tlX38 (T_Patterns_vIn37 )
         _lhsOppL :: [PP_Doc]
         _lhsOppL = rule39 _hdIpp _tlIppL
         _lhsOpp :: PP_Doc
         _lhsOpp = rule40 _hdIpp _tlIpp
         _copy = rule41 _hdIcopy _tlIcopy
         _lhsOcopy :: Patterns
         _lhsOcopy = rule42 _copy
         __result_ = T_Patterns_vOut37 _lhsOcopy _lhsOpp _lhsOppL
         in __result_ )
     in C_Patterns_s38 v37
   {-# INLINE rule39 #-}
   {-# LINE 82 "./src-ag/CodeSyntaxDump.ag" #-}
   rule39 = \ ((_hdIpp) :: PP_Doc) ((_tlIppL) :: [PP_Doc]) ->
                                                                                  {-# LINE 82 "./src-ag/CodeSyntaxDump.ag" #-}
                                                                                  _hdIpp : _tlIppL
                                                                                  {-# LINE 998 "dist/build/CodeSyntaxDump.hs"#-}
   {-# INLINE rule40 #-}
   rule40 = \ ((_hdIpp) :: PP_Doc) ((_tlIpp) :: PP_Doc) ->
     _hdIpp >-< _tlIpp
   {-# INLINE rule41 #-}
   rule41 = \ ((_hdIcopy) :: Pattern) ((_tlIcopy) :: Patterns) ->
     (:) _hdIcopy _tlIcopy
   {-# INLINE rule42 #-}
   rule42 = \ _copy ->
     _copy
{-# NOINLINE sem_Patterns_Nil #-}
sem_Patterns_Nil ::  T_Patterns 
sem_Patterns_Nil  = T_Patterns (return st38) where
   {-# NOINLINE st38 #-}
   st38 = let
      v37 :: T_Patterns_v37 
      v37 = \ (T_Patterns_vIn37 ) -> ( let
         _lhsOppL :: [PP_Doc]
         _lhsOppL = rule43  ()
         _lhsOpp :: PP_Doc
         _lhsOpp = rule44  ()
         _copy = rule45  ()
         _lhsOcopy :: Patterns
         _lhsOcopy = rule46 _copy
         __result_ = T_Patterns_vOut37 _lhsOcopy _lhsOpp _lhsOppL
         in __result_ )
     in C_Patterns_s38 v37
   {-# INLINE rule43 #-}
   {-# LINE 83 "./src-ag/CodeSyntaxDump.ag" #-}
   rule43 = \  (_ :: ()) ->
                                                                                  {-# LINE 83 "./src-ag/CodeSyntaxDump.ag" #-}
                                                                                  []
                                                                                  {-# LINE 1030 "dist/build/CodeSyntaxDump.hs"#-}
   {-# INLINE rule44 #-}
   rule44 = \  (_ :: ()) ->
     empty
   {-# INLINE rule45 #-}
   rule45 = \  (_ :: ()) ->
     []
   {-# INLINE rule46 #-}
   rule46 = \ _copy ->
     _copy

-- Sequence ----------------------------------------------------
-- wrapper
data Inh_Sequence  = Inh_Sequence {  }
data Syn_Sequence  = Syn_Sequence { ppL_Syn_Sequence :: ([PP_Doc]) }
{-# INLINABLE wrap_Sequence #-}
wrap_Sequence :: T_Sequence  -> Inh_Sequence  -> (Syn_Sequence )
wrap_Sequence (T_Sequence act) (Inh_Sequence ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Sequence_vIn40 
        (T_Sequence_vOut40 _lhsOppL) <- return (inv_Sequence_s41 sem arg)
        return (Syn_Sequence _lhsOppL)
   )

-- cata
{-# NOINLINE sem_Sequence #-}
sem_Sequence :: Sequence  -> T_Sequence 
sem_Sequence list = Prelude.foldr sem_Sequence_Cons sem_Sequence_Nil (Prelude.map sem_CRule list)

-- semantic domain
newtype T_Sequence  = T_Sequence {
                                 attach_T_Sequence :: Identity (T_Sequence_s41 )
                                 }
newtype T_Sequence_s41  = C_Sequence_s41 {
                                         inv_Sequence_s41 :: (T_Sequence_v40 )
                                         }
data T_Sequence_s42  = C_Sequence_s42
type T_Sequence_v40  = (T_Sequence_vIn40 ) -> (T_Sequence_vOut40 )
data T_Sequence_vIn40  = T_Sequence_vIn40 
data T_Sequence_vOut40  = T_Sequence_vOut40 ([PP_Doc])
{-# NOINLINE sem_Sequence_Cons #-}
sem_Sequence_Cons :: T_CRule  -> T_Sequence  -> T_Sequence 
sem_Sequence_Cons arg_hd_ arg_tl_ = T_Sequence (return st41) where
   {-# NOINLINE st41 #-}
   st41 = let
      v40 :: T_Sequence_v40 
      v40 = \ (T_Sequence_vIn40 ) -> ( let
         _hdX20 = Control.Monad.Identity.runIdentity (attach_T_CRule (arg_hd_))
         _tlX41 = Control.Monad.Identity.runIdentity (attach_T_Sequence (arg_tl_))
         (T_CRule_vOut19 _hdIpp) = inv_CRule_s20 _hdX20 (T_CRule_vIn19 )
         (T_Sequence_vOut40 _tlIppL) = inv_Sequence_s41 _tlX41 (T_Sequence_vIn40 )
         _lhsOppL :: [PP_Doc]
         _lhsOppL = rule47 _hdIpp _tlIppL
         __result_ = T_Sequence_vOut40 _lhsOppL
         in __result_ )
     in C_Sequence_s41 v40
   {-# INLINE rule47 #-}
   {-# LINE 86 "./src-ag/CodeSyntaxDump.ag" #-}
   rule47 = \ ((_hdIpp) :: PP_Doc) ((_tlIppL) :: [PP_Doc]) ->
                                                                                  {-# LINE 86 "./src-ag/CodeSyntaxDump.ag" #-}
                                                                                  _hdIpp : _tlIppL
                                                                                  {-# LINE 1092 "dist/build/CodeSyntaxDump.hs"#-}
{-# NOINLINE sem_Sequence_Nil #-}
sem_Sequence_Nil ::  T_Sequence 
sem_Sequence_Nil  = T_Sequence (return st41) where
   {-# NOINLINE st41 #-}
   st41 = let
      v40 :: T_Sequence_v40 
      v40 = \ (T_Sequence_vIn40 ) -> ( let
         _lhsOppL :: [PP_Doc]
         _lhsOppL = rule48  ()
         __result_ = T_Sequence_vOut40 _lhsOppL
         in __result_ )
     in C_Sequence_s41 v40
   {-# INLINE rule48 #-}
   {-# LINE 87 "./src-ag/CodeSyntaxDump.ag" #-}
   rule48 = \  (_ :: ()) ->
                                                                                  {-# LINE 87 "./src-ag/CodeSyntaxDump.ag" #-}
                                                                                  []
                                                                                  {-# LINE 1110 "dist/build/CodeSyntaxDump.hs"#-}
