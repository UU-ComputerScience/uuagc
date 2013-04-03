{-# LANGUAGE Rank2Types, GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PrintVisitCode where
{-# LINE 2 "./src-ag/CodeSyntax.ag" #-}

import Patterns
import CommonTypes
import Data.Map(Map)
import Data.Set(Set)
{-# LINE 12 "dist/build/PrintVisitCode.hs" #-}

{-# LINE 2 "./src-ag/Patterns.ag" #-}

-- Patterns.ag imports
import UU.Scanner.Position(Pos)
import CommonTypes (ConstructorIdent,Identifier)
{-# LINE 19 "dist/build/PrintVisitCode.hs" #-}

{-# LINE 2 "./src-ag/DeclBlocks.ag" #-}

import Code (Decl,Expr)
{-# LINE 24 "dist/build/PrintVisitCode.hs" #-}

{-# LINE 10 "./src-ag/PrintVisitCode.ag" #-}

import CommonTypes
import SequentialTypes
import Options
import CodeSyntax
import ErrorMessages
import GrammarInfo
import DeclBlocks
import Pretty

import qualified Data.Map as Map
import Data.Map(Map)
import qualified Data.Set as Set
import Data.Set(Set)
import qualified Data.Sequence as Seq
import Data.Sequence(Seq)
import UU.Scanner.Position

import Data.List(partition,intersperse,intersect,(\\))
import Data.Maybe(fromJust,isJust)
{-# LINE 47 "dist/build/PrintVisitCode.hs" #-}
import Control.Monad.Identity (Identity)
import qualified Control.Monad.Identity
{-# LINE 32 "./src-ag/PrintVisitCode.ag" #-}

type PP_Docs = [PP_Doc]

ppMultiSeqH :: [PP_Doc] -> PP_Doc -> PP_Doc
ppMultiSeqH = ppMultiSeq' (>#<)

ppMultiSeqV :: [PP_Doc] -> PP_Doc -> PP_Doc
ppMultiSeqV = ppMultiSeq' (>-<)

ppMultiSeq' :: (PP_Doc -> PP_Doc -> PP_Doc) -> [PP_Doc] -> PP_Doc -> PP_Doc
ppMultiSeq' next strictArgs expr
  = foldr (\v r -> (v >#< "`seq`") `next` pp_parens r) expr strictArgs
{-# LINE 63 "dist/build/PrintVisitCode.hs" #-}
-- CGrammar ----------------------------------------------------
-- wrapper
data Inh_CGrammar  = Inh_CGrammar { options_Inh_CGrammar :: (Options) }
data Syn_CGrammar  = Syn_CGrammar { output_Syn_CGrammar :: (PP_Docs) }
{-# INLINABLE wrap_CGrammar #-}
wrap_CGrammar :: T_CGrammar  -> Inh_CGrammar  -> (Syn_CGrammar )
wrap_CGrammar (T_CGrammar act) (Inh_CGrammar _lhsIoptions) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_CGrammar_vIn1 _lhsIoptions
        (T_CGrammar_vOut1 _lhsOoutput) <- return (inv_CGrammar_s2 sem arg)
        return (Syn_CGrammar _lhsOoutput)
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
data T_CGrammar_vIn1  = T_CGrammar_vIn1 (Options)
data T_CGrammar_vOut1  = T_CGrammar_vOut1 (PP_Docs)
{-# NOINLINE sem_CGrammar_CGrammar #-}
sem_CGrammar_CGrammar :: (TypeSyns) -> (Derivings) -> (Set NontermIdent) -> T_CNonterminals  -> (PragmaMap) -> (ParamMap) -> (ContextMap) -> (QuantMap) -> (Map NontermIdent (Map ConstructorIdent (Set Identifier))) -> (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier])))) -> (Bool) -> T_CGrammar 
sem_CGrammar_CGrammar _ _ _ arg_nonts_ _ _ _ _ _ _ _ = T_CGrammar (return st2) where
   {-# NOINLINE st2 #-}
   st2 = let
      v1 :: T_CGrammar_v1 
      v1 = \ (T_CGrammar_vIn1 _lhsIoptions) -> ( let
         _nontsX11 = Control.Monad.Identity.runIdentity (attach_T_CNonterminals (arg_nonts_))
         (T_CNonterminals_vOut10 ) = inv_CNonterminals_s11 _nontsX11 (T_CNonterminals_vIn10 )
         _lhsOoutput :: PP_Docs
         _lhsOoutput = rule0  ()
         __result_ = T_CGrammar_vOut1 _lhsOoutput
         in __result_ )
     in C_CGrammar_s2 v1
   {-# INLINE rule0 #-}
   {-# LINE 53 "./src-ag/PrintVisitCode.ag" #-}
   rule0 = \  (_ :: ()) ->
                     {-# LINE 53 "./src-ag/PrintVisitCode.ag" #-}
                     []
                     {-# LINE 113 "dist/build/PrintVisitCode.hs"#-}

-- CInterface --------------------------------------------------
-- wrapper
data Inh_CInterface  = Inh_CInterface {  }
data Syn_CInterface  = Syn_CInterface {  }
{-# INLINABLE wrap_CInterface #-}
wrap_CInterface :: T_CInterface  -> Inh_CInterface  -> (Syn_CInterface )
wrap_CInterface (T_CInterface act) (Inh_CInterface ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_CInterface_vIn4 
        (T_CInterface_vOut4 ) <- return (inv_CInterface_s5 sem arg)
        return (Syn_CInterface )
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
data T_CInterface_vOut4  = T_CInterface_vOut4 
{-# NOINLINE sem_CInterface_CInterface #-}
sem_CInterface_CInterface :: T_CSegments  -> T_CInterface 
sem_CInterface_CInterface arg_seg_ = T_CInterface (return st5) where
   {-# NOINLINE st5 #-}
   st5 = let
      v4 :: T_CInterface_v4 
      v4 = \ (T_CInterface_vIn4 ) -> ( let
         _segX26 = Control.Monad.Identity.runIdentity (attach_T_CSegments (arg_seg_))
         (T_CSegments_vOut25 ) = inv_CSegments_s26 _segX26 (T_CSegments_vIn25 )
         __result_ = T_CInterface_vOut4 
         in __result_ )
     in C_CInterface_s5 v4

-- CNonterminal ------------------------------------------------
-- wrapper
data Inh_CNonterminal  = Inh_CNonterminal {  }
data Syn_CNonterminal  = Syn_CNonterminal {  }
{-# INLINABLE wrap_CNonterminal #-}
wrap_CNonterminal :: T_CNonterminal  -> Inh_CNonterminal  -> (Syn_CNonterminal )
wrap_CNonterminal (T_CNonterminal act) (Inh_CNonterminal ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_CNonterminal_vIn7 
        (T_CNonterminal_vOut7 ) <- return (inv_CNonterminal_s8 sem arg)
        return (Syn_CNonterminal )
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
data T_CNonterminal_vOut7  = T_CNonterminal_vOut7 
{-# NOINLINE sem_CNonterminal_CNonterminal #-}
sem_CNonterminal_CNonterminal :: (NontermIdent) -> ([Identifier]) -> (Attributes) -> (Attributes) -> T_CProductions  -> T_CInterface  -> T_CNonterminal 
sem_CNonterminal_CNonterminal _ _ _ _ arg_prods_ arg_inter_ = T_CNonterminal (return st8) where
   {-# NOINLINE st8 #-}
   st8 = let
      v7 :: T_CNonterminal_v7 
      v7 = \ (T_CNonterminal_vIn7 ) -> ( let
         _prodsX17 = Control.Monad.Identity.runIdentity (attach_T_CProductions (arg_prods_))
         _interX5 = Control.Monad.Identity.runIdentity (attach_T_CInterface (arg_inter_))
         (T_CProductions_vOut16 ) = inv_CProductions_s17 _prodsX17 (T_CProductions_vIn16 )
         (T_CInterface_vOut4 ) = inv_CInterface_s5 _interX5 (T_CInterface_vIn4 )
         __result_ = T_CNonterminal_vOut7 
         in __result_ )
     in C_CNonterminal_s8 v7

-- CNonterminals -----------------------------------------------
-- wrapper
data Inh_CNonterminals  = Inh_CNonterminals {  }
data Syn_CNonterminals  = Syn_CNonterminals {  }
{-# INLINABLE wrap_CNonterminals #-}
wrap_CNonterminals :: T_CNonterminals  -> Inh_CNonterminals  -> (Syn_CNonterminals )
wrap_CNonterminals (T_CNonterminals act) (Inh_CNonterminals ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_CNonterminals_vIn10 
        (T_CNonterminals_vOut10 ) <- return (inv_CNonterminals_s11 sem arg)
        return (Syn_CNonterminals )
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
data T_CNonterminals_vOut10  = T_CNonterminals_vOut10 
{-# NOINLINE sem_CNonterminals_Cons #-}
sem_CNonterminals_Cons :: T_CNonterminal  -> T_CNonterminals  -> T_CNonterminals 
sem_CNonterminals_Cons arg_hd_ arg_tl_ = T_CNonterminals (return st11) where
   {-# NOINLINE st11 #-}
   st11 = let
      v10 :: T_CNonterminals_v10 
      v10 = \ (T_CNonterminals_vIn10 ) -> ( let
         _hdX8 = Control.Monad.Identity.runIdentity (attach_T_CNonterminal (arg_hd_))
         _tlX11 = Control.Monad.Identity.runIdentity (attach_T_CNonterminals (arg_tl_))
         (T_CNonterminal_vOut7 ) = inv_CNonterminal_s8 _hdX8 (T_CNonterminal_vIn7 )
         (T_CNonterminals_vOut10 ) = inv_CNonterminals_s11 _tlX11 (T_CNonterminals_vIn10 )
         __result_ = T_CNonterminals_vOut10 
         in __result_ )
     in C_CNonterminals_s11 v10
{-# NOINLINE sem_CNonterminals_Nil #-}
sem_CNonterminals_Nil ::  T_CNonterminals 
sem_CNonterminals_Nil  = T_CNonterminals (return st11) where
   {-# NOINLINE st11 #-}
   st11 = let
      v10 :: T_CNonterminals_v10 
      v10 = \ (T_CNonterminals_vIn10 ) -> ( let
         __result_ = T_CNonterminals_vOut10 
         in __result_ )
     in C_CNonterminals_s11 v10

-- CProduction -------------------------------------------------
-- wrapper
data Inh_CProduction  = Inh_CProduction {  }
data Syn_CProduction  = Syn_CProduction {  }
{-# INLINABLE wrap_CProduction #-}
wrap_CProduction :: T_CProduction  -> Inh_CProduction  -> (Syn_CProduction )
wrap_CProduction (T_CProduction act) (Inh_CProduction ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_CProduction_vIn13 
        (T_CProduction_vOut13 ) <- return (inv_CProduction_s14 sem arg)
        return (Syn_CProduction )
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
data T_CProduction_vOut13  = T_CProduction_vOut13 
{-# NOINLINE sem_CProduction_CProduction #-}
sem_CProduction_CProduction :: (ConstructorIdent) -> T_CVisits  -> ([(Identifier,Type,ChildKind)]) -> ([Identifier]) -> T_CProduction 
sem_CProduction_CProduction _ arg_visits_ _ _ = T_CProduction (return st14) where
   {-# NOINLINE st14 #-}
   st14 = let
      v13 :: T_CProduction_v13 
      v13 = \ (T_CProduction_vIn13 ) -> ( let
         _visitsX32 = Control.Monad.Identity.runIdentity (attach_T_CVisits (arg_visits_))
         (T_CVisits_vOut31 ) = inv_CVisits_s32 _visitsX32 (T_CVisits_vIn31 )
         __result_ = T_CProduction_vOut13 
         in __result_ )
     in C_CProduction_s14 v13

-- CProductions ------------------------------------------------
-- wrapper
data Inh_CProductions  = Inh_CProductions {  }
data Syn_CProductions  = Syn_CProductions {  }
{-# INLINABLE wrap_CProductions #-}
wrap_CProductions :: T_CProductions  -> Inh_CProductions  -> (Syn_CProductions )
wrap_CProductions (T_CProductions act) (Inh_CProductions ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_CProductions_vIn16 
        (T_CProductions_vOut16 ) <- return (inv_CProductions_s17 sem arg)
        return (Syn_CProductions )
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
data T_CProductions_vOut16  = T_CProductions_vOut16 
{-# NOINLINE sem_CProductions_Cons #-}
sem_CProductions_Cons :: T_CProduction  -> T_CProductions  -> T_CProductions 
sem_CProductions_Cons arg_hd_ arg_tl_ = T_CProductions (return st17) where
   {-# NOINLINE st17 #-}
   st17 = let
      v16 :: T_CProductions_v16 
      v16 = \ (T_CProductions_vIn16 ) -> ( let
         _hdX14 = Control.Monad.Identity.runIdentity (attach_T_CProduction (arg_hd_))
         _tlX17 = Control.Monad.Identity.runIdentity (attach_T_CProductions (arg_tl_))
         (T_CProduction_vOut13 ) = inv_CProduction_s14 _hdX14 (T_CProduction_vIn13 )
         (T_CProductions_vOut16 ) = inv_CProductions_s17 _tlX17 (T_CProductions_vIn16 )
         __result_ = T_CProductions_vOut16 
         in __result_ )
     in C_CProductions_s17 v16
{-# NOINLINE sem_CProductions_Nil #-}
sem_CProductions_Nil ::  T_CProductions 
sem_CProductions_Nil  = T_CProductions (return st17) where
   {-# NOINLINE st17 #-}
   st17 = let
      v16 :: T_CProductions_v16 
      v16 = \ (T_CProductions_vIn16 ) -> ( let
         __result_ = T_CProductions_vOut16 
         in __result_ )
     in C_CProductions_s17 v16

-- CRule -------------------------------------------------------
-- wrapper
data Inh_CRule  = Inh_CRule {  }
data Syn_CRule  = Syn_CRule {  }
{-# INLINABLE wrap_CRule #-}
wrap_CRule :: T_CRule  -> Inh_CRule  -> (Syn_CRule )
wrap_CRule (T_CRule act) (Inh_CRule ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_CRule_vIn19 
        (T_CRule_vOut19 ) <- return (inv_CRule_s20 sem arg)
        return (Syn_CRule )
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
data T_CRule_vOut19  = T_CRule_vOut19 
{-# NOINLINE sem_CRule_CRule #-}
sem_CRule_CRule :: (Identifier) -> (Bool) -> (Bool) -> (NontermIdent) -> (ConstructorIdent) -> (Identifier) -> (Maybe NontermIdent) -> (Maybe Type) -> T_Pattern  -> ([String]) -> (Map Int (Identifier,Identifier,Maybe Type)) -> (Bool) -> (String) -> (Set (Identifier, Identifier)) -> (Bool) -> (Maybe Identifier) -> T_CRule 
sem_CRule_CRule _ _ _ _ _ _ _ _ arg_pattern_ _ _ _ _ _ _ _ = T_CRule (return st20) where
   {-# NOINLINE st20 #-}
   st20 = let
      v19 :: T_CRule_v19 
      v19 = \ (T_CRule_vIn19 ) -> ( let
         _patternX41 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pattern_))
         (T_Pattern_vOut40 _patternIcopy) = inv_Pattern_s41 _patternX41 (T_Pattern_vIn40 )
         __result_ = T_CRule_vOut19 
         in __result_ )
     in C_CRule_s20 v19
{-# NOINLINE sem_CRule_CChildVisit #-}
sem_CRule_CChildVisit :: (Identifier) -> (NontermIdent) -> (Int) -> (Attributes) -> (Attributes) -> (Bool) -> T_CRule 
sem_CRule_CChildVisit _ _ _ _ _ _ = T_CRule (return st20) where
   {-# NOINLINE st20 #-}
   st20 = let
      v19 :: T_CRule_v19 
      v19 = \ (T_CRule_vIn19 ) -> ( let
         __result_ = T_CRule_vOut19 
         in __result_ )
     in C_CRule_s20 v19

-- CSegment ----------------------------------------------------
-- wrapper
data Inh_CSegment  = Inh_CSegment {  }
data Syn_CSegment  = Syn_CSegment {  }
{-# INLINABLE wrap_CSegment #-}
wrap_CSegment :: T_CSegment  -> Inh_CSegment  -> (Syn_CSegment )
wrap_CSegment (T_CSegment act) (Inh_CSegment ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_CSegment_vIn22 
        (T_CSegment_vOut22 ) <- return (inv_CSegment_s23 sem arg)
        return (Syn_CSegment )
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
data T_CSegment_vOut22  = T_CSegment_vOut22 
{-# NOINLINE sem_CSegment_CSegment #-}
sem_CSegment_CSegment :: (Attributes) -> (Attributes) -> T_CSegment 
sem_CSegment_CSegment _ _ = T_CSegment (return st23) where
   {-# NOINLINE st23 #-}
   st23 = let
      v22 :: T_CSegment_v22 
      v22 = \ (T_CSegment_vIn22 ) -> ( let
         __result_ = T_CSegment_vOut22 
         in __result_ )
     in C_CSegment_s23 v22

-- CSegments ---------------------------------------------------
-- wrapper
data Inh_CSegments  = Inh_CSegments {  }
data Syn_CSegments  = Syn_CSegments {  }
{-# INLINABLE wrap_CSegments #-}
wrap_CSegments :: T_CSegments  -> Inh_CSegments  -> (Syn_CSegments )
wrap_CSegments (T_CSegments act) (Inh_CSegments ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_CSegments_vIn25 
        (T_CSegments_vOut25 ) <- return (inv_CSegments_s26 sem arg)
        return (Syn_CSegments )
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
data T_CSegments_vOut25  = T_CSegments_vOut25 
{-# NOINLINE sem_CSegments_Cons #-}
sem_CSegments_Cons :: T_CSegment  -> T_CSegments  -> T_CSegments 
sem_CSegments_Cons arg_hd_ arg_tl_ = T_CSegments (return st26) where
   {-# NOINLINE st26 #-}
   st26 = let
      v25 :: T_CSegments_v25 
      v25 = \ (T_CSegments_vIn25 ) -> ( let
         _hdX23 = Control.Monad.Identity.runIdentity (attach_T_CSegment (arg_hd_))
         _tlX26 = Control.Monad.Identity.runIdentity (attach_T_CSegments (arg_tl_))
         (T_CSegment_vOut22 ) = inv_CSegment_s23 _hdX23 (T_CSegment_vIn22 )
         (T_CSegments_vOut25 ) = inv_CSegments_s26 _tlX26 (T_CSegments_vIn25 )
         __result_ = T_CSegments_vOut25 
         in __result_ )
     in C_CSegments_s26 v25
{-# NOINLINE sem_CSegments_Nil #-}
sem_CSegments_Nil ::  T_CSegments 
sem_CSegments_Nil  = T_CSegments (return st26) where
   {-# NOINLINE st26 #-}
   st26 = let
      v25 :: T_CSegments_v25 
      v25 = \ (T_CSegments_vIn25 ) -> ( let
         __result_ = T_CSegments_vOut25 
         in __result_ )
     in C_CSegments_s26 v25

-- CVisit ------------------------------------------------------
-- wrapper
data Inh_CVisit  = Inh_CVisit {  }
data Syn_CVisit  = Syn_CVisit {  }
{-# INLINABLE wrap_CVisit #-}
wrap_CVisit :: T_CVisit  -> Inh_CVisit  -> (Syn_CVisit )
wrap_CVisit (T_CVisit act) (Inh_CVisit ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_CVisit_vIn28 
        (T_CVisit_vOut28 ) <- return (inv_CVisit_s29 sem arg)
        return (Syn_CVisit )
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
data T_CVisit_vOut28  = T_CVisit_vOut28 
{-# NOINLINE sem_CVisit_CVisit #-}
sem_CVisit_CVisit :: (Attributes) -> (Attributes) -> T_Sequence  -> T_Sequence  -> (Bool) -> T_CVisit 
sem_CVisit_CVisit _ _ arg_vss_ arg_intra_ _ = T_CVisit (return st29) where
   {-# NOINLINE st29 #-}
   st29 = let
      v28 :: T_CVisit_v28 
      v28 = \ (T_CVisit_vIn28 ) -> ( let
         _vssX47 = Control.Monad.Identity.runIdentity (attach_T_Sequence (arg_vss_))
         _intraX47 = Control.Monad.Identity.runIdentity (attach_T_Sequence (arg_intra_))
         (T_Sequence_vOut46 ) = inv_Sequence_s47 _vssX47 (T_Sequence_vIn46 )
         (T_Sequence_vOut46 ) = inv_Sequence_s47 _intraX47 (T_Sequence_vIn46 )
         __result_ = T_CVisit_vOut28 
         in __result_ )
     in C_CVisit_s29 v28

-- CVisits -----------------------------------------------------
-- wrapper
data Inh_CVisits  = Inh_CVisits {  }
data Syn_CVisits  = Syn_CVisits {  }
{-# INLINABLE wrap_CVisits #-}
wrap_CVisits :: T_CVisits  -> Inh_CVisits  -> (Syn_CVisits )
wrap_CVisits (T_CVisits act) (Inh_CVisits ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_CVisits_vIn31 
        (T_CVisits_vOut31 ) <- return (inv_CVisits_s32 sem arg)
        return (Syn_CVisits )
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
data T_CVisits_vOut31  = T_CVisits_vOut31 
{-# NOINLINE sem_CVisits_Cons #-}
sem_CVisits_Cons :: T_CVisit  -> T_CVisits  -> T_CVisits 
sem_CVisits_Cons arg_hd_ arg_tl_ = T_CVisits (return st32) where
   {-# NOINLINE st32 #-}
   st32 = let
      v31 :: T_CVisits_v31 
      v31 = \ (T_CVisits_vIn31 ) -> ( let
         _hdX29 = Control.Monad.Identity.runIdentity (attach_T_CVisit (arg_hd_))
         _tlX32 = Control.Monad.Identity.runIdentity (attach_T_CVisits (arg_tl_))
         (T_CVisit_vOut28 ) = inv_CVisit_s29 _hdX29 (T_CVisit_vIn28 )
         (T_CVisits_vOut31 ) = inv_CVisits_s32 _tlX32 (T_CVisits_vIn31 )
         __result_ = T_CVisits_vOut31 
         in __result_ )
     in C_CVisits_s32 v31
{-# NOINLINE sem_CVisits_Nil #-}
sem_CVisits_Nil ::  T_CVisits 
sem_CVisits_Nil  = T_CVisits (return st32) where
   {-# NOINLINE st32 #-}
   st32 = let
      v31 :: T_CVisits_v31 
      v31 = \ (T_CVisits_vIn31 ) -> ( let
         __result_ = T_CVisits_vOut31 
         in __result_ )
     in C_CVisits_s32 v31

-- DeclBlocks --------------------------------------------------
-- wrapper
data Inh_DeclBlocks  = Inh_DeclBlocks {  }
data Syn_DeclBlocks  = Syn_DeclBlocks {  }
{-# INLINABLE wrap_DeclBlocks #-}
wrap_DeclBlocks :: T_DeclBlocks  -> Inh_DeclBlocks  -> (Syn_DeclBlocks )
wrap_DeclBlocks (T_DeclBlocks act) (Inh_DeclBlocks ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_DeclBlocks_vIn34 
        (T_DeclBlocks_vOut34 ) <- return (inv_DeclBlocks_s35 sem arg)
        return (Syn_DeclBlocks )
   )

-- cata
{-# NOINLINE sem_DeclBlocks #-}
sem_DeclBlocks :: DeclBlocks  -> T_DeclBlocks 
sem_DeclBlocks ( DeclBlock defs_ visit_ next_ ) = sem_DeclBlocks_DeclBlock defs_ visit_ ( sem_DeclBlocks next_ )
sem_DeclBlocks ( DeclTerminator defs_ result_ ) = sem_DeclBlocks_DeclTerminator defs_ result_

-- semantic domain
newtype T_DeclBlocks  = T_DeclBlocks {
                                     attach_T_DeclBlocks :: Identity (T_DeclBlocks_s35 )
                                     }
newtype T_DeclBlocks_s35  = C_DeclBlocks_s35 {
                                             inv_DeclBlocks_s35 :: (T_DeclBlocks_v34 )
                                             }
data T_DeclBlocks_s36  = C_DeclBlocks_s36
type T_DeclBlocks_v34  = (T_DeclBlocks_vIn34 ) -> (T_DeclBlocks_vOut34 )
data T_DeclBlocks_vIn34  = T_DeclBlocks_vIn34 
data T_DeclBlocks_vOut34  = T_DeclBlocks_vOut34 
{-# NOINLINE sem_DeclBlocks_DeclBlock #-}
sem_DeclBlocks_DeclBlock :: ([Decl]) -> (Decl) -> T_DeclBlocks  -> T_DeclBlocks 
sem_DeclBlocks_DeclBlock _ _ arg_next_ = T_DeclBlocks (return st35) where
   {-# NOINLINE st35 #-}
   st35 = let
      v34 :: T_DeclBlocks_v34 
      v34 = \ (T_DeclBlocks_vIn34 ) -> ( let
         _nextX35 = Control.Monad.Identity.runIdentity (attach_T_DeclBlocks (arg_next_))
         (T_DeclBlocks_vOut34 ) = inv_DeclBlocks_s35 _nextX35 (T_DeclBlocks_vIn34 )
         __result_ = T_DeclBlocks_vOut34 
         in __result_ )
     in C_DeclBlocks_s35 v34
{-# NOINLINE sem_DeclBlocks_DeclTerminator #-}
sem_DeclBlocks_DeclTerminator :: ([Decl]) -> (Expr) -> T_DeclBlocks 
sem_DeclBlocks_DeclTerminator _ _ = T_DeclBlocks (return st35) where
   {-# NOINLINE st35 #-}
   st35 = let
      v34 :: T_DeclBlocks_v34 
      v34 = \ (T_DeclBlocks_vIn34 ) -> ( let
         __result_ = T_DeclBlocks_vOut34 
         in __result_ )
     in C_DeclBlocks_s35 v34

-- DeclBlocksRoot ----------------------------------------------
-- wrapper
data Inh_DeclBlocksRoot  = Inh_DeclBlocksRoot {  }
data Syn_DeclBlocksRoot  = Syn_DeclBlocksRoot {  }
{-# INLINABLE wrap_DeclBlocksRoot #-}
wrap_DeclBlocksRoot :: T_DeclBlocksRoot  -> Inh_DeclBlocksRoot  -> (Syn_DeclBlocksRoot )
wrap_DeclBlocksRoot (T_DeclBlocksRoot act) (Inh_DeclBlocksRoot ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_DeclBlocksRoot_vIn37 
        (T_DeclBlocksRoot_vOut37 ) <- return (inv_DeclBlocksRoot_s38 sem arg)
        return (Syn_DeclBlocksRoot )
   )

-- cata
{-# INLINE sem_DeclBlocksRoot #-}
sem_DeclBlocksRoot :: DeclBlocksRoot  -> T_DeclBlocksRoot 
sem_DeclBlocksRoot ( DeclBlocksRoot blocks_ ) = sem_DeclBlocksRoot_DeclBlocksRoot ( sem_DeclBlocks blocks_ )

-- semantic domain
newtype T_DeclBlocksRoot  = T_DeclBlocksRoot {
                                             attach_T_DeclBlocksRoot :: Identity (T_DeclBlocksRoot_s38 )
                                             }
newtype T_DeclBlocksRoot_s38  = C_DeclBlocksRoot_s38 {
                                                     inv_DeclBlocksRoot_s38 :: (T_DeclBlocksRoot_v37 )
                                                     }
data T_DeclBlocksRoot_s39  = C_DeclBlocksRoot_s39
type T_DeclBlocksRoot_v37  = (T_DeclBlocksRoot_vIn37 ) -> (T_DeclBlocksRoot_vOut37 )
data T_DeclBlocksRoot_vIn37  = T_DeclBlocksRoot_vIn37 
data T_DeclBlocksRoot_vOut37  = T_DeclBlocksRoot_vOut37 
{-# NOINLINE sem_DeclBlocksRoot_DeclBlocksRoot #-}
sem_DeclBlocksRoot_DeclBlocksRoot :: T_DeclBlocks  -> T_DeclBlocksRoot 
sem_DeclBlocksRoot_DeclBlocksRoot arg_blocks_ = T_DeclBlocksRoot (return st38) where
   {-# NOINLINE st38 #-}
   st38 = let
      v37 :: T_DeclBlocksRoot_v37 
      v37 = \ (T_DeclBlocksRoot_vIn37 ) -> ( let
         _blocksX35 = Control.Monad.Identity.runIdentity (attach_T_DeclBlocks (arg_blocks_))
         (T_DeclBlocks_vOut34 ) = inv_DeclBlocks_s35 _blocksX35 (T_DeclBlocks_vIn34 )
         __result_ = T_DeclBlocksRoot_vOut37 
         in __result_ )
     in C_DeclBlocksRoot_s38 v37

-- Pattern -----------------------------------------------------
-- wrapper
data Inh_Pattern  = Inh_Pattern {  }
data Syn_Pattern  = Syn_Pattern { copy_Syn_Pattern :: (Pattern) }
{-# INLINABLE wrap_Pattern #-}
wrap_Pattern :: T_Pattern  -> Inh_Pattern  -> (Syn_Pattern )
wrap_Pattern (T_Pattern act) (Inh_Pattern ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Pattern_vIn40 
        (T_Pattern_vOut40 _lhsOcopy) <- return (inv_Pattern_s41 sem arg)
        return (Syn_Pattern _lhsOcopy)
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
                               attach_T_Pattern :: Identity (T_Pattern_s41 )
                               }
newtype T_Pattern_s41  = C_Pattern_s41 {
                                       inv_Pattern_s41 :: (T_Pattern_v40 )
                                       }
data T_Pattern_s42  = C_Pattern_s42
type T_Pattern_v40  = (T_Pattern_vIn40 ) -> (T_Pattern_vOut40 )
data T_Pattern_vIn40  = T_Pattern_vIn40 
data T_Pattern_vOut40  = T_Pattern_vOut40 (Pattern)
{-# NOINLINE sem_Pattern_Constr #-}
sem_Pattern_Constr :: (ConstructorIdent) -> T_Patterns  -> T_Pattern 
sem_Pattern_Constr arg_name_ arg_pats_ = T_Pattern (return st41) where
   {-# NOINLINE st41 #-}
   st41 = let
      v40 :: T_Pattern_v40 
      v40 = \ (T_Pattern_vIn40 ) -> ( let
         _patsX44 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_pats_))
         (T_Patterns_vOut43 _patsIcopy) = inv_Patterns_s44 _patsX44 (T_Patterns_vIn43 )
         _copy = rule1 _patsIcopy arg_name_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule2 _copy
         __result_ = T_Pattern_vOut40 _lhsOcopy
         in __result_ )
     in C_Pattern_s41 v40
   {-# INLINE rule1 #-}
   rule1 = \ ((_patsIcopy) :: Patterns) name_ ->
     Constr name_ _patsIcopy
   {-# INLINE rule2 #-}
   rule2 = \ _copy ->
     _copy
{-# NOINLINE sem_Pattern_Product #-}
sem_Pattern_Product :: (Pos) -> T_Patterns  -> T_Pattern 
sem_Pattern_Product arg_pos_ arg_pats_ = T_Pattern (return st41) where
   {-# NOINLINE st41 #-}
   st41 = let
      v40 :: T_Pattern_v40 
      v40 = \ (T_Pattern_vIn40 ) -> ( let
         _patsX44 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_pats_))
         (T_Patterns_vOut43 _patsIcopy) = inv_Patterns_s44 _patsX44 (T_Patterns_vIn43 )
         _copy = rule3 _patsIcopy arg_pos_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule4 _copy
         __result_ = T_Pattern_vOut40 _lhsOcopy
         in __result_ )
     in C_Pattern_s41 v40
   {-# INLINE rule3 #-}
   rule3 = \ ((_patsIcopy) :: Patterns) pos_ ->
     Product pos_ _patsIcopy
   {-# INLINE rule4 #-}
   rule4 = \ _copy ->
     _copy
{-# NOINLINE sem_Pattern_Alias #-}
sem_Pattern_Alias :: (Identifier) -> (Identifier) -> T_Pattern  -> T_Pattern 
sem_Pattern_Alias arg_field_ arg_attr_ arg_pat_ = T_Pattern (return st41) where
   {-# NOINLINE st41 #-}
   st41 = let
      v40 :: T_Pattern_v40 
      v40 = \ (T_Pattern_vIn40 ) -> ( let
         _patX41 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pat_))
         (T_Pattern_vOut40 _patIcopy) = inv_Pattern_s41 _patX41 (T_Pattern_vIn40 )
         _copy = rule5 _patIcopy arg_attr_ arg_field_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule6 _copy
         __result_ = T_Pattern_vOut40 _lhsOcopy
         in __result_ )
     in C_Pattern_s41 v40
   {-# INLINE rule5 #-}
   rule5 = \ ((_patIcopy) :: Pattern) attr_ field_ ->
     Alias field_ attr_ _patIcopy
   {-# INLINE rule6 #-}
   rule6 = \ _copy ->
     _copy
{-# NOINLINE sem_Pattern_Irrefutable #-}
sem_Pattern_Irrefutable :: T_Pattern  -> T_Pattern 
sem_Pattern_Irrefutable arg_pat_ = T_Pattern (return st41) where
   {-# NOINLINE st41 #-}
   st41 = let
      v40 :: T_Pattern_v40 
      v40 = \ (T_Pattern_vIn40 ) -> ( let
         _patX41 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pat_))
         (T_Pattern_vOut40 _patIcopy) = inv_Pattern_s41 _patX41 (T_Pattern_vIn40 )
         _copy = rule7 _patIcopy
         _lhsOcopy :: Pattern
         _lhsOcopy = rule8 _copy
         __result_ = T_Pattern_vOut40 _lhsOcopy
         in __result_ )
     in C_Pattern_s41 v40
   {-# INLINE rule7 #-}
   rule7 = \ ((_patIcopy) :: Pattern) ->
     Irrefutable _patIcopy
   {-# INLINE rule8 #-}
   rule8 = \ _copy ->
     _copy
{-# NOINLINE sem_Pattern_Underscore #-}
sem_Pattern_Underscore :: (Pos) -> T_Pattern 
sem_Pattern_Underscore arg_pos_ = T_Pattern (return st41) where
   {-# NOINLINE st41 #-}
   st41 = let
      v40 :: T_Pattern_v40 
      v40 = \ (T_Pattern_vIn40 ) -> ( let
         _copy = rule9 arg_pos_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule10 _copy
         __result_ = T_Pattern_vOut40 _lhsOcopy
         in __result_ )
     in C_Pattern_s41 v40
   {-# INLINE rule9 #-}
   rule9 = \ pos_ ->
     Underscore pos_
   {-# INLINE rule10 #-}
   rule10 = \ _copy ->
     _copy

-- Patterns ----------------------------------------------------
-- wrapper
data Inh_Patterns  = Inh_Patterns {  }
data Syn_Patterns  = Syn_Patterns { copy_Syn_Patterns :: (Patterns) }
{-# INLINABLE wrap_Patterns #-}
wrap_Patterns :: T_Patterns  -> Inh_Patterns  -> (Syn_Patterns )
wrap_Patterns (T_Patterns act) (Inh_Patterns ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Patterns_vIn43 
        (T_Patterns_vOut43 _lhsOcopy) <- return (inv_Patterns_s44 sem arg)
        return (Syn_Patterns _lhsOcopy)
   )

-- cata
{-# NOINLINE sem_Patterns #-}
sem_Patterns :: Patterns  -> T_Patterns 
sem_Patterns list = Prelude.foldr sem_Patterns_Cons sem_Patterns_Nil (Prelude.map sem_Pattern list)

-- semantic domain
newtype T_Patterns  = T_Patterns {
                                 attach_T_Patterns :: Identity (T_Patterns_s44 )
                                 }
newtype T_Patterns_s44  = C_Patterns_s44 {
                                         inv_Patterns_s44 :: (T_Patterns_v43 )
                                         }
data T_Patterns_s45  = C_Patterns_s45
type T_Patterns_v43  = (T_Patterns_vIn43 ) -> (T_Patterns_vOut43 )
data T_Patterns_vIn43  = T_Patterns_vIn43 
data T_Patterns_vOut43  = T_Patterns_vOut43 (Patterns)
{-# NOINLINE sem_Patterns_Cons #-}
sem_Patterns_Cons :: T_Pattern  -> T_Patterns  -> T_Patterns 
sem_Patterns_Cons arg_hd_ arg_tl_ = T_Patterns (return st44) where
   {-# NOINLINE st44 #-}
   st44 = let
      v43 :: T_Patterns_v43 
      v43 = \ (T_Patterns_vIn43 ) -> ( let
         _hdX41 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_hd_))
         _tlX44 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_tl_))
         (T_Pattern_vOut40 _hdIcopy) = inv_Pattern_s41 _hdX41 (T_Pattern_vIn40 )
         (T_Patterns_vOut43 _tlIcopy) = inv_Patterns_s44 _tlX44 (T_Patterns_vIn43 )
         _copy = rule11 _hdIcopy _tlIcopy
         _lhsOcopy :: Patterns
         _lhsOcopy = rule12 _copy
         __result_ = T_Patterns_vOut43 _lhsOcopy
         in __result_ )
     in C_Patterns_s44 v43
   {-# INLINE rule11 #-}
   rule11 = \ ((_hdIcopy) :: Pattern) ((_tlIcopy) :: Patterns) ->
     (:) _hdIcopy _tlIcopy
   {-# INLINE rule12 #-}
   rule12 = \ _copy ->
     _copy
{-# NOINLINE sem_Patterns_Nil #-}
sem_Patterns_Nil ::  T_Patterns 
sem_Patterns_Nil  = T_Patterns (return st44) where
   {-# NOINLINE st44 #-}
   st44 = let
      v43 :: T_Patterns_v43 
      v43 = \ (T_Patterns_vIn43 ) -> ( let
         _copy = rule13  ()
         _lhsOcopy :: Patterns
         _lhsOcopy = rule14 _copy
         __result_ = T_Patterns_vOut43 _lhsOcopy
         in __result_ )
     in C_Patterns_s44 v43
   {-# INLINE rule13 #-}
   rule13 = \  (_ :: ()) ->
     []
   {-# INLINE rule14 #-}
   rule14 = \ _copy ->
     _copy

-- Sequence ----------------------------------------------------
-- wrapper
data Inh_Sequence  = Inh_Sequence {  }
data Syn_Sequence  = Syn_Sequence {  }
{-# INLINABLE wrap_Sequence #-}
wrap_Sequence :: T_Sequence  -> Inh_Sequence  -> (Syn_Sequence )
wrap_Sequence (T_Sequence act) (Inh_Sequence ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Sequence_vIn46 
        (T_Sequence_vOut46 ) <- return (inv_Sequence_s47 sem arg)
        return (Syn_Sequence )
   )

-- cata
{-# NOINLINE sem_Sequence #-}
sem_Sequence :: Sequence  -> T_Sequence 
sem_Sequence list = Prelude.foldr sem_Sequence_Cons sem_Sequence_Nil (Prelude.map sem_CRule list)

-- semantic domain
newtype T_Sequence  = T_Sequence {
                                 attach_T_Sequence :: Identity (T_Sequence_s47 )
                                 }
newtype T_Sequence_s47  = C_Sequence_s47 {
                                         inv_Sequence_s47 :: (T_Sequence_v46 )
                                         }
data T_Sequence_s48  = C_Sequence_s48
type T_Sequence_v46  = (T_Sequence_vIn46 ) -> (T_Sequence_vOut46 )
data T_Sequence_vIn46  = T_Sequence_vIn46 
data T_Sequence_vOut46  = T_Sequence_vOut46 
{-# NOINLINE sem_Sequence_Cons #-}
sem_Sequence_Cons :: T_CRule  -> T_Sequence  -> T_Sequence 
sem_Sequence_Cons arg_hd_ arg_tl_ = T_Sequence (return st47) where
   {-# NOINLINE st47 #-}
   st47 = let
      v46 :: T_Sequence_v46 
      v46 = \ (T_Sequence_vIn46 ) -> ( let
         _hdX20 = Control.Monad.Identity.runIdentity (attach_T_CRule (arg_hd_))
         _tlX47 = Control.Monad.Identity.runIdentity (attach_T_Sequence (arg_tl_))
         (T_CRule_vOut19 ) = inv_CRule_s20 _hdX20 (T_CRule_vIn19 )
         (T_Sequence_vOut46 ) = inv_Sequence_s47 _tlX47 (T_Sequence_vIn46 )
         __result_ = T_Sequence_vOut46 
         in __result_ )
     in C_Sequence_s47 v46
{-# NOINLINE sem_Sequence_Nil #-}
sem_Sequence_Nil ::  T_Sequence 
sem_Sequence_Nil  = T_Sequence (return st47) where
   {-# NOINLINE st47 #-}
   st47 = let
      v46 :: T_Sequence_v46 
      v46 = \ (T_Sequence_vIn46 ) -> ( let
         __result_ = T_Sequence_vOut46 
         in __result_ )
     in C_Sequence_s47 v46
