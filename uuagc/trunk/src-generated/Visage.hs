{-# LANGUAGE Rank2Types, GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Visage where
{-# LINE 2 "./src-ag/Expression.ag" #-}

import UU.Scanner.Position(Pos)
import HsToken
{-# LINE 10 "dist/build/Visage.hs" #-}

{-# LINE 2 "./src-ag/VisagePatterns.ag" #-}

import UU.Scanner.Position(Pos)
import CommonTypes
{-# LINE 16 "dist/build/Visage.hs" #-}

{-# LINE 2 "./src-ag/VisageSyntax.ag" #-}

import CommonTypes
import UU.Pretty
import AbstractSyntax
import VisagePatterns
import Expression
{-# LINE 25 "dist/build/Visage.hs" #-}

{-# LINE 6 "./src-ag/Visage.ag" #-}

import UU.Scanner.Position(Pos(..))
import CommonTypes
import ATermAbstractSyntax
import Expression
import VisagePatterns
import VisageSyntax
import qualified Data.Map as Map
import Data.Map(Map)
import Data.List(intersperse)
import TokenDef
{-# LINE 39 "dist/build/Visage.hs" #-}
import Control.Monad.Identity (Identity)
import qualified Control.Monad.Identity
{-# LINE 19 "./src-ag/Visage.ag" #-}

convert :: String -> String
convert [] = []
convert (c:ct) | c == '\n' = '\\' : 'n' : convert ct
               | otherwise = c : convert ct
 
sQ :: String -> String
sQ []     = []
sQ (x:xs) = if (x=='"') then rest else x:rest
    where
      rest = if not (null xs) && last xs == '"' then init xs else xs

showAGPos :: Pos -> String
showAGPos (Pos l c f) | l == (-1) = ""
                      | otherwise = let file = if null f then "" else f -- No show of f
                                        lc = "(line " ++ show l ++ ", column " ++ show c ++")"
                                    in file ++ lc

showMap :: (Show a, Show b) => Map a b -> String
showMap
  = braces . concat . intersperse "," . map (uncurry assign) . Map.assocs
  where
    braces s = "{" ++ s ++ "}"
    assign a b = show a ++ ":=" ++ show b
{-# LINE 67 "dist/build/Visage.hs" #-}
-- Expression --------------------------------------------------
-- wrapper
data Inh_Expression  = Inh_Expression {  }
data Syn_Expression  = Syn_Expression { aterm_Syn_Expression :: (ATerm) }
{-# INLINABLE wrap_Expression #-}
wrap_Expression :: T_Expression  -> Inh_Expression  -> (Syn_Expression )
wrap_Expression (T_Expression act) (Inh_Expression ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Expression_vIn1 
        (T_Expression_vOut1 _lhsOaterm) <- return (inv_Expression_s2 sem arg)
        return (Syn_Expression _lhsOaterm)
   )

-- cata
{-# INLINE sem_Expression #-}
sem_Expression :: Expression  -> T_Expression 
sem_Expression ( Expression pos_ tks_ ) = sem_Expression_Expression pos_ tks_

-- semantic domain
newtype T_Expression  = T_Expression {
                                     attach_T_Expression :: Identity (T_Expression_s2 )
                                     }
newtype T_Expression_s2  = C_Expression_s2 {
                                           inv_Expression_s2 :: (T_Expression_v1 )
                                           }
data T_Expression_s3  = C_Expression_s3
type T_Expression_v1  = (T_Expression_vIn1 ) -> (T_Expression_vOut1 )
data T_Expression_vIn1  = T_Expression_vIn1 
data T_Expression_vOut1  = T_Expression_vOut1 (ATerm)
{-# NOINLINE sem_Expression_Expression #-}
sem_Expression_Expression :: (Pos) -> ([HsToken]) -> T_Expression 
sem_Expression_Expression arg_pos_ arg_tks_ = T_Expression (return st2) where
   {-# NOINLINE st2 #-}
   st2 = let
      v1 :: T_Expression_v1 
      v1 = \ (T_Expression_vIn1 ) -> ( let
         _lhsOaterm :: ATerm
         _lhsOaterm = rule0 arg_pos_ arg_tks_
         __result_ = T_Expression_vOut1 _lhsOaterm
         in __result_ )
     in C_Expression_s2 v1
   {-# INLINE rule0 #-}
   {-# LINE 103 "./src-ag/Visage.ag" #-}
   rule0 = \ pos_ tks_ ->
                              {-# LINE 103 "./src-ag/Visage.ag" #-}
                              AAppl "Expression" [AString (sQ (showAGPos pos_)), AString (sQ (unlines . showTokens . tokensToStrings $ tks_))]
                              {-# LINE 115 "dist/build/Visage.hs"#-}

-- VisageChild -------------------------------------------------
-- wrapper
data Inh_VisageChild  = Inh_VisageChild {  }
data Syn_VisageChild  = Syn_VisageChild { aterm_Syn_VisageChild :: (ATerm) }
{-# INLINABLE wrap_VisageChild #-}
wrap_VisageChild :: T_VisageChild  -> Inh_VisageChild  -> (Syn_VisageChild )
wrap_VisageChild (T_VisageChild act) (Inh_VisageChild ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_VisageChild_vIn4 
        (T_VisageChild_vOut4 _lhsOaterm) <- return (inv_VisageChild_s5 sem arg)
        return (Syn_VisageChild _lhsOaterm)
   )

-- cata
{-# INLINE sem_VisageChild #-}
sem_VisageChild :: VisageChild  -> T_VisageChild 
sem_VisageChild ( VChild name_ tp_ inh_ syn_ rules_ ) = sem_VisageChild_VChild name_ tp_ inh_ syn_ ( sem_VisageRules rules_ )

-- semantic domain
newtype T_VisageChild  = T_VisageChild {
                                       attach_T_VisageChild :: Identity (T_VisageChild_s5 )
                                       }
newtype T_VisageChild_s5  = C_VisageChild_s5 {
                                             inv_VisageChild_s5 :: (T_VisageChild_v4 )
                                             }
data T_VisageChild_s6  = C_VisageChild_s6
type T_VisageChild_v4  = (T_VisageChild_vIn4 ) -> (T_VisageChild_vOut4 )
data T_VisageChild_vIn4  = T_VisageChild_vIn4 
data T_VisageChild_vOut4  = T_VisageChild_vOut4 (ATerm)
{-# NOINLINE sem_VisageChild_VChild #-}
sem_VisageChild_VChild :: (Identifier) -> (Type) -> (Attributes) -> (Attributes) -> T_VisageRules  -> T_VisageChild 
sem_VisageChild_VChild arg_name_ arg_tp_ arg_inh_ arg_syn_ arg_rules_ = T_VisageChild (return st5) where
   {-# NOINLINE st5 #-}
   st5 = let
      v4 :: T_VisageChild_v4 
      v4 = \ (T_VisageChild_vIn4 ) -> ( let
         _rulesX35 = Control.Monad.Identity.runIdentity (attach_T_VisageRules (arg_rules_))
         (T_VisageRules_vOut34 _rulesIaterms) = inv_VisageRules_s35 _rulesX35 (T_VisageRules_vIn34 _rulesOisLoc)
         _lhsOaterm :: ATerm
         _lhsOaterm = rule1 _rulesIaterms arg_inh_ arg_name_ arg_syn_ arg_tp_
         _rulesOisLoc = rule2  ()
         __result_ = T_VisageChild_vOut4 _lhsOaterm
         in __result_ )
     in C_VisageChild_s5 v4
   {-# INLINE rule1 #-}
   {-# LINE 85 "./src-ag/Visage.ag" #-}
   rule1 = \ ((_rulesIaterms) :: [ATerm]) inh_ name_ syn_ tp_ ->
                               {-# LINE 85 "./src-ag/Visage.ag" #-}
                               AAppl "Child" [AString (sQ (getName name_)), AString (sQ (show tp_)),
                                              AString (sQ (showMap inh_)),
                                              AString (sQ (showMap syn_)),
                                              AAppl "Rules" _rulesIaterms]
                               {-# LINE 170 "dist/build/Visage.hs"#-}
   {-# INLINE rule2 #-}
   {-# LINE 89 "./src-ag/Visage.ag" #-}
   rule2 = \  (_ :: ()) ->
                                 {-# LINE 89 "./src-ag/Visage.ag" #-}
                                 False
                                 {-# LINE 176 "dist/build/Visage.hs"#-}

-- VisageChildren ----------------------------------------------
-- wrapper
data Inh_VisageChildren  = Inh_VisageChildren {  }
data Syn_VisageChildren  = Syn_VisageChildren { aterms_Syn_VisageChildren :: ([ATerm]) }
{-# INLINABLE wrap_VisageChildren #-}
wrap_VisageChildren :: T_VisageChildren  -> Inh_VisageChildren  -> (Syn_VisageChildren )
wrap_VisageChildren (T_VisageChildren act) (Inh_VisageChildren ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_VisageChildren_vIn7 
        (T_VisageChildren_vOut7 _lhsOaterms) <- return (inv_VisageChildren_s8 sem arg)
        return (Syn_VisageChildren _lhsOaterms)
   )

-- cata
{-# NOINLINE sem_VisageChildren #-}
sem_VisageChildren :: VisageChildren  -> T_VisageChildren 
sem_VisageChildren list = Prelude.foldr sem_VisageChildren_Cons sem_VisageChildren_Nil (Prelude.map sem_VisageChild list)

-- semantic domain
newtype T_VisageChildren  = T_VisageChildren {
                                             attach_T_VisageChildren :: Identity (T_VisageChildren_s8 )
                                             }
newtype T_VisageChildren_s8  = C_VisageChildren_s8 {
                                                   inv_VisageChildren_s8 :: (T_VisageChildren_v7 )
                                                   }
data T_VisageChildren_s9  = C_VisageChildren_s9
type T_VisageChildren_v7  = (T_VisageChildren_vIn7 ) -> (T_VisageChildren_vOut7 )
data T_VisageChildren_vIn7  = T_VisageChildren_vIn7 
data T_VisageChildren_vOut7  = T_VisageChildren_vOut7 ([ATerm])
{-# NOINLINE sem_VisageChildren_Cons #-}
sem_VisageChildren_Cons :: T_VisageChild  -> T_VisageChildren  -> T_VisageChildren 
sem_VisageChildren_Cons arg_hd_ arg_tl_ = T_VisageChildren (return st8) where
   {-# NOINLINE st8 #-}
   st8 = let
      v7 :: T_VisageChildren_v7 
      v7 = \ (T_VisageChildren_vIn7 ) -> ( let
         _hdX5 = Control.Monad.Identity.runIdentity (attach_T_VisageChild (arg_hd_))
         _tlX8 = Control.Monad.Identity.runIdentity (attach_T_VisageChildren (arg_tl_))
         (T_VisageChild_vOut4 _hdIaterm) = inv_VisageChild_s5 _hdX5 (T_VisageChild_vIn4 )
         (T_VisageChildren_vOut7 _tlIaterms) = inv_VisageChildren_s8 _tlX8 (T_VisageChildren_vIn7 )
         _lhsOaterms :: [ATerm]
         _lhsOaterms = rule3 _hdIaterm _tlIaterms
         __result_ = T_VisageChildren_vOut7 _lhsOaterms
         in __result_ )
     in C_VisageChildren_s8 v7
   {-# INLINE rule3 #-}
   {-# LINE 80 "./src-ag/Visage.ag" #-}
   rule3 = \ ((_hdIaterm) :: ATerm) ((_tlIaterms) :: [ATerm]) ->
                               {-# LINE 80 "./src-ag/Visage.ag" #-}
                               _hdIaterm : _tlIaterms
                               {-# LINE 229 "dist/build/Visage.hs"#-}
{-# NOINLINE sem_VisageChildren_Nil #-}
sem_VisageChildren_Nil ::  T_VisageChildren 
sem_VisageChildren_Nil  = T_VisageChildren (return st8) where
   {-# NOINLINE st8 #-}
   st8 = let
      v7 :: T_VisageChildren_v7 
      v7 = \ (T_VisageChildren_vIn7 ) -> ( let
         _lhsOaterms :: [ATerm]
         _lhsOaterms = rule4  ()
         __result_ = T_VisageChildren_vOut7 _lhsOaterms
         in __result_ )
     in C_VisageChildren_s8 v7
   {-# INLINE rule4 #-}
   {-# LINE 81 "./src-ag/Visage.ag" #-}
   rule4 = \  (_ :: ()) ->
                               {-# LINE 81 "./src-ag/Visage.ag" #-}
                               []
                               {-# LINE 247 "dist/build/Visage.hs"#-}

-- VisageGrammar -----------------------------------------------
-- wrapper
data Inh_VisageGrammar  = Inh_VisageGrammar {  }
data Syn_VisageGrammar  = Syn_VisageGrammar { aterm_Syn_VisageGrammar :: (ATerm) }
{-# INLINABLE wrap_VisageGrammar #-}
wrap_VisageGrammar :: T_VisageGrammar  -> Inh_VisageGrammar  -> (Syn_VisageGrammar )
wrap_VisageGrammar (T_VisageGrammar act) (Inh_VisageGrammar ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_VisageGrammar_vIn10 
        (T_VisageGrammar_vOut10 _lhsOaterm) <- return (inv_VisageGrammar_s11 sem arg)
        return (Syn_VisageGrammar _lhsOaterm)
   )

-- cata
{-# INLINE sem_VisageGrammar #-}
sem_VisageGrammar :: VisageGrammar  -> T_VisageGrammar 
sem_VisageGrammar ( VGrammar nonts_ ) = sem_VisageGrammar_VGrammar ( sem_VisageNonterminals nonts_ )

-- semantic domain
newtype T_VisageGrammar  = T_VisageGrammar {
                                           attach_T_VisageGrammar :: Identity (T_VisageGrammar_s11 )
                                           }
newtype T_VisageGrammar_s11  = C_VisageGrammar_s11 {
                                                   inv_VisageGrammar_s11 :: (T_VisageGrammar_v10 )
                                                   }
data T_VisageGrammar_s12  = C_VisageGrammar_s12
type T_VisageGrammar_v10  = (T_VisageGrammar_vIn10 ) -> (T_VisageGrammar_vOut10 )
data T_VisageGrammar_vIn10  = T_VisageGrammar_vIn10 
data T_VisageGrammar_vOut10  = T_VisageGrammar_vOut10 (ATerm)
{-# NOINLINE sem_VisageGrammar_VGrammar #-}
sem_VisageGrammar_VGrammar :: T_VisageNonterminals  -> T_VisageGrammar 
sem_VisageGrammar_VGrammar arg_nonts_ = T_VisageGrammar (return st11) where
   {-# NOINLINE st11 #-}
   st11 = let
      v10 :: T_VisageGrammar_v10 
      v10 = \ (T_VisageGrammar_vIn10 ) -> ( let
         _nontsX17 = Control.Monad.Identity.runIdentity (attach_T_VisageNonterminals (arg_nonts_))
         (T_VisageNonterminals_vOut16 _nontsIaterms) = inv_VisageNonterminals_s17 _nontsX17 (T_VisageNonterminals_vIn16 )
         _lhsOaterm :: ATerm
         _lhsOaterm = rule5 _nontsIaterms
         __result_ = T_VisageGrammar_vOut10 _lhsOaterm
         in __result_ )
     in C_VisageGrammar_s11 v10
   {-# INLINE rule5 #-}
   {-# LINE 54 "./src-ag/Visage.ag" #-}
   rule5 = \ ((_nontsIaterms) :: [ATerm]) ->
                               {-# LINE 54 "./src-ag/Visage.ag" #-}
                               AAppl "Productions" _nontsIaterms
                               {-# LINE 298 "dist/build/Visage.hs"#-}

-- VisageNonterminal -------------------------------------------
-- wrapper
data Inh_VisageNonterminal  = Inh_VisageNonterminal {  }
data Syn_VisageNonterminal  = Syn_VisageNonterminal { aterm_Syn_VisageNonterminal :: (ATerm) }
{-# INLINABLE wrap_VisageNonterminal #-}
wrap_VisageNonterminal :: T_VisageNonterminal  -> Inh_VisageNonterminal  -> (Syn_VisageNonterminal )
wrap_VisageNonterminal (T_VisageNonterminal act) (Inh_VisageNonterminal ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_VisageNonterminal_vIn13 
        (T_VisageNonterminal_vOut13 _lhsOaterm) <- return (inv_VisageNonterminal_s14 sem arg)
        return (Syn_VisageNonterminal _lhsOaterm)
   )

-- cata
{-# INLINE sem_VisageNonterminal #-}
sem_VisageNonterminal :: VisageNonterminal  -> T_VisageNonterminal 
sem_VisageNonterminal ( VNonterminal nt_ inh_ syn_ alts_ ) = sem_VisageNonterminal_VNonterminal nt_ inh_ syn_ ( sem_VisageProductions alts_ )

-- semantic domain
newtype T_VisageNonterminal  = T_VisageNonterminal {
                                                   attach_T_VisageNonterminal :: Identity (T_VisageNonterminal_s14 )
                                                   }
newtype T_VisageNonterminal_s14  = C_VisageNonterminal_s14 {
                                                           inv_VisageNonterminal_s14 :: (T_VisageNonterminal_v13 )
                                                           }
data T_VisageNonterminal_s15  = C_VisageNonterminal_s15
type T_VisageNonterminal_v13  = (T_VisageNonterminal_vIn13 ) -> (T_VisageNonterminal_vOut13 )
data T_VisageNonterminal_vIn13  = T_VisageNonterminal_vIn13 
data T_VisageNonterminal_vOut13  = T_VisageNonterminal_vOut13 (ATerm)
{-# NOINLINE sem_VisageNonterminal_VNonterminal #-}
sem_VisageNonterminal_VNonterminal :: (NontermIdent) -> (Attributes) -> (Attributes) -> T_VisageProductions  -> T_VisageNonterminal 
sem_VisageNonterminal_VNonterminal arg_nt_ arg_inh_ arg_syn_ arg_alts_ = T_VisageNonterminal (return st14) where
   {-# NOINLINE st14 #-}
   st14 = let
      v13 :: T_VisageNonterminal_v13 
      v13 = \ (T_VisageNonterminal_vIn13 ) -> ( let
         _altsX29 = Control.Monad.Identity.runIdentity (attach_T_VisageProductions (arg_alts_))
         (T_VisageProductions_vOut28 _altsIaterms) = inv_VisageProductions_s29 _altsX29 (T_VisageProductions_vIn28 )
         _lhsOaterm :: ATerm
         _lhsOaterm = rule6 _altsIaterms arg_inh_ arg_nt_ arg_syn_
         __result_ = T_VisageNonterminal_vOut13 _lhsOaterm
         in __result_ )
     in C_VisageNonterminal_s14 v13
   {-# INLINE rule6 #-}
   {-# LINE 63 "./src-ag/Visage.ag" #-}
   rule6 = \ ((_altsIaterms) :: [ATerm]) inh_ nt_ syn_ ->
                                {-# LINE 63 "./src-ag/Visage.ag" #-}
                                AAppl "Production" [AString (sQ (getName nt_)), AString (sQ(showMap inh_)),
                                                   AString (sQ(showMap syn_)), AAppl "Alternatives" _altsIaterms]
                                {-# LINE 350 "dist/build/Visage.hs"#-}

-- VisageNonterminals ------------------------------------------
-- wrapper
data Inh_VisageNonterminals  = Inh_VisageNonterminals {  }
data Syn_VisageNonterminals  = Syn_VisageNonterminals { aterms_Syn_VisageNonterminals :: ([ATerm]) }
{-# INLINABLE wrap_VisageNonterminals #-}
wrap_VisageNonterminals :: T_VisageNonterminals  -> Inh_VisageNonterminals  -> (Syn_VisageNonterminals )
wrap_VisageNonterminals (T_VisageNonterminals act) (Inh_VisageNonterminals ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_VisageNonterminals_vIn16 
        (T_VisageNonterminals_vOut16 _lhsOaterms) <- return (inv_VisageNonterminals_s17 sem arg)
        return (Syn_VisageNonterminals _lhsOaterms)
   )

-- cata
{-# NOINLINE sem_VisageNonterminals #-}
sem_VisageNonterminals :: VisageNonterminals  -> T_VisageNonterminals 
sem_VisageNonterminals list = Prelude.foldr sem_VisageNonterminals_Cons sem_VisageNonterminals_Nil (Prelude.map sem_VisageNonterminal list)

-- semantic domain
newtype T_VisageNonterminals  = T_VisageNonterminals {
                                                     attach_T_VisageNonterminals :: Identity (T_VisageNonterminals_s17 )
                                                     }
newtype T_VisageNonterminals_s17  = C_VisageNonterminals_s17 {
                                                             inv_VisageNonterminals_s17 :: (T_VisageNonterminals_v16 )
                                                             }
data T_VisageNonterminals_s18  = C_VisageNonterminals_s18
type T_VisageNonterminals_v16  = (T_VisageNonterminals_vIn16 ) -> (T_VisageNonterminals_vOut16 )
data T_VisageNonterminals_vIn16  = T_VisageNonterminals_vIn16 
data T_VisageNonterminals_vOut16  = T_VisageNonterminals_vOut16 ([ATerm])
{-# NOINLINE sem_VisageNonterminals_Cons #-}
sem_VisageNonterminals_Cons :: T_VisageNonterminal  -> T_VisageNonterminals  -> T_VisageNonterminals 
sem_VisageNonterminals_Cons arg_hd_ arg_tl_ = T_VisageNonterminals (return st17) where
   {-# NOINLINE st17 #-}
   st17 = let
      v16 :: T_VisageNonterminals_v16 
      v16 = \ (T_VisageNonterminals_vIn16 ) -> ( let
         _hdX14 = Control.Monad.Identity.runIdentity (attach_T_VisageNonterminal (arg_hd_))
         _tlX17 = Control.Monad.Identity.runIdentity (attach_T_VisageNonterminals (arg_tl_))
         (T_VisageNonterminal_vOut13 _hdIaterm) = inv_VisageNonterminal_s14 _hdX14 (T_VisageNonterminal_vIn13 )
         (T_VisageNonterminals_vOut16 _tlIaterms) = inv_VisageNonterminals_s17 _tlX17 (T_VisageNonterminals_vIn16 )
         _lhsOaterms :: [ATerm]
         _lhsOaterms = rule7 _hdIaterm _tlIaterms
         __result_ = T_VisageNonterminals_vOut16 _lhsOaterms
         in __result_ )
     in C_VisageNonterminals_s17 v16
   {-# INLINE rule7 #-}
   {-# LINE 58 "./src-ag/Visage.ag" #-}
   rule7 = \ ((_hdIaterm) :: ATerm) ((_tlIaterms) :: [ATerm]) ->
                               {-# LINE 58 "./src-ag/Visage.ag" #-}
                               _hdIaterm : _tlIaterms
                               {-# LINE 403 "dist/build/Visage.hs"#-}
{-# NOINLINE sem_VisageNonterminals_Nil #-}
sem_VisageNonterminals_Nil ::  T_VisageNonterminals 
sem_VisageNonterminals_Nil  = T_VisageNonterminals (return st17) where
   {-# NOINLINE st17 #-}
   st17 = let
      v16 :: T_VisageNonterminals_v16 
      v16 = \ (T_VisageNonterminals_vIn16 ) -> ( let
         _lhsOaterms :: [ATerm]
         _lhsOaterms = rule8  ()
         __result_ = T_VisageNonterminals_vOut16 _lhsOaterms
         in __result_ )
     in C_VisageNonterminals_s17 v16
   {-# INLINE rule8 #-}
   {-# LINE 59 "./src-ag/Visage.ag" #-}
   rule8 = \  (_ :: ()) ->
                               {-# LINE 59 "./src-ag/Visage.ag" #-}
                               []
                               {-# LINE 421 "dist/build/Visage.hs"#-}

-- VisagePattern -----------------------------------------------
-- wrapper
data Inh_VisagePattern  = Inh_VisagePattern {  }
data Syn_VisagePattern  = Syn_VisagePattern { aterm_Syn_VisagePattern :: (ATerm) }
{-# INLINABLE wrap_VisagePattern #-}
wrap_VisagePattern :: T_VisagePattern  -> Inh_VisagePattern  -> (Syn_VisagePattern )
wrap_VisagePattern (T_VisagePattern act) (Inh_VisagePattern ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_VisagePattern_vIn19 
        (T_VisagePattern_vOut19 _lhsOaterm) <- return (inv_VisagePattern_s20 sem arg)
        return (Syn_VisagePattern _lhsOaterm)
   )

-- cata
{-# NOINLINE sem_VisagePattern #-}
sem_VisagePattern :: VisagePattern  -> T_VisagePattern 
sem_VisagePattern ( VConstr name_ pats_ ) = sem_VisagePattern_VConstr name_ ( sem_VisagePatterns pats_ )
sem_VisagePattern ( VProduct pos_ pats_ ) = sem_VisagePattern_VProduct pos_ ( sem_VisagePatterns pats_ )
sem_VisagePattern ( VVar field_ attr_ ) = sem_VisagePattern_VVar field_ attr_
sem_VisagePattern ( VAlias field_ attr_ pat_ ) = sem_VisagePattern_VAlias field_ attr_ ( sem_VisagePattern pat_ )
sem_VisagePattern ( VUnderscore pos_ ) = sem_VisagePattern_VUnderscore pos_

-- semantic domain
newtype T_VisagePattern  = T_VisagePattern {
                                           attach_T_VisagePattern :: Identity (T_VisagePattern_s20 )
                                           }
newtype T_VisagePattern_s20  = C_VisagePattern_s20 {
                                                   inv_VisagePattern_s20 :: (T_VisagePattern_v19 )
                                                   }
data T_VisagePattern_s21  = C_VisagePattern_s21
type T_VisagePattern_v19  = (T_VisagePattern_vIn19 ) -> (T_VisagePattern_vOut19 )
data T_VisagePattern_vIn19  = T_VisagePattern_vIn19 
data T_VisagePattern_vOut19  = T_VisagePattern_vOut19 (ATerm)
{-# NOINLINE sem_VisagePattern_VConstr #-}
sem_VisagePattern_VConstr :: (ConstructorIdent) -> T_VisagePatterns  -> T_VisagePattern 
sem_VisagePattern_VConstr arg_name_ arg_pats_ = T_VisagePattern (return st20) where
   {-# NOINLINE st20 #-}
   st20 = let
      v19 :: T_VisagePattern_v19 
      v19 = \ (T_VisagePattern_vIn19 ) -> ( let
         _patsX23 = Control.Monad.Identity.runIdentity (attach_T_VisagePatterns (arg_pats_))
         (T_VisagePatterns_vOut22 _patsIaterms) = inv_VisagePatterns_s23 _patsX23 (T_VisagePatterns_vIn22 )
         _lhsOaterm :: ATerm
         _lhsOaterm = rule9 _patsIaterms arg_name_
         __result_ = T_VisagePattern_vOut19 _lhsOaterm
         in __result_ )
     in C_VisagePattern_s20 v19
   {-# INLINE rule9 #-}
   {-# LINE 112 "./src-ag/Visage.ag" #-}
   rule9 = \ ((_patsIaterms) :: [ATerm]) name_ ->
                               {-# LINE 112 "./src-ag/Visage.ag" #-}
                               AAppl "Pattern" [AAppl "Constr" [AString (sQ (showAGPos (getPos name_))),
                                                AString (sQ (getName name_)),
                                                AAppl "Patterns" _patsIaterms]]
                               {-# LINE 478 "dist/build/Visage.hs"#-}
{-# NOINLINE sem_VisagePattern_VProduct #-}
sem_VisagePattern_VProduct :: (Pos) -> T_VisagePatterns  -> T_VisagePattern 
sem_VisagePattern_VProduct arg_pos_ arg_pats_ = T_VisagePattern (return st20) where
   {-# NOINLINE st20 #-}
   st20 = let
      v19 :: T_VisagePattern_v19 
      v19 = \ (T_VisagePattern_vIn19 ) -> ( let
         _patsX23 = Control.Monad.Identity.runIdentity (attach_T_VisagePatterns (arg_pats_))
         (T_VisagePatterns_vOut22 _patsIaterms) = inv_VisagePatterns_s23 _patsX23 (T_VisagePatterns_vIn22 )
         _lhsOaterm :: ATerm
         _lhsOaterm = rule10 _patsIaterms arg_pos_
         __result_ = T_VisagePattern_vOut19 _lhsOaterm
         in __result_ )
     in C_VisagePattern_s20 v19
   {-# INLINE rule10 #-}
   {-# LINE 115 "./src-ag/Visage.ag" #-}
   rule10 = \ ((_patsIaterms) :: [ATerm]) pos_ ->
                               {-# LINE 115 "./src-ag/Visage.ag" #-}
                               AAppl "Pattern" [AAppl "Product" [AString (sQ (showAGPos pos_)),
                                                                 AAppl "Patterns" _patsIaterms]]
                               {-# LINE 499 "dist/build/Visage.hs"#-}
{-# NOINLINE sem_VisagePattern_VVar #-}
sem_VisagePattern_VVar :: (Identifier) -> (Identifier) -> T_VisagePattern 
sem_VisagePattern_VVar arg_field_ arg_attr_ = T_VisagePattern (return st20) where
   {-# NOINLINE st20 #-}
   st20 = let
      v19 :: T_VisagePattern_v19 
      v19 = \ (T_VisagePattern_vIn19 ) -> ( let
         _lhsOaterm :: ATerm
         _lhsOaterm = rule11 arg_attr_ arg_field_
         __result_ = T_VisagePattern_vOut19 _lhsOaterm
         in __result_ )
     in C_VisagePattern_s20 v19
   {-# INLINE rule11 #-}
   {-# LINE 117 "./src-ag/Visage.ag" #-}
   rule11 = \ attr_ field_ ->
                               {-# LINE 117 "./src-ag/Visage.ag" #-}
                               AAppl "Pattern" [AAppl "Var" [AString (sQ (showAGPos (getPos field_))),
                                                             AString (sQ (getName field_ ++ "." ++ getName attr_))]]
                               {-# LINE 518 "dist/build/Visage.hs"#-}
{-# NOINLINE sem_VisagePattern_VAlias #-}
sem_VisagePattern_VAlias :: (Identifier) -> (Identifier) -> T_VisagePattern  -> T_VisagePattern 
sem_VisagePattern_VAlias arg_field_ arg_attr_ arg_pat_ = T_VisagePattern (return st20) where
   {-# NOINLINE st20 #-}
   st20 = let
      v19 :: T_VisagePattern_v19 
      v19 = \ (T_VisagePattern_vIn19 ) -> ( let
         _patX20 = Control.Monad.Identity.runIdentity (attach_T_VisagePattern (arg_pat_))
         (T_VisagePattern_vOut19 _patIaterm) = inv_VisagePattern_s20 _patX20 (T_VisagePattern_vIn19 )
         _lhsOaterm :: ATerm
         _lhsOaterm = rule12 _patIaterm arg_attr_ arg_field_
         __result_ = T_VisagePattern_vOut19 _lhsOaterm
         in __result_ )
     in C_VisagePattern_s20 v19
   {-# INLINE rule12 #-}
   {-# LINE 119 "./src-ag/Visage.ag" #-}
   rule12 = \ ((_patIaterm) :: ATerm) attr_ field_ ->
                               {-# LINE 119 "./src-ag/Visage.ag" #-}
                               AAppl "Pattern" [AAppl "Alias" [AString (sQ (showAGPos (getPos field_))),
                                                               AString (sQ (getName field_ ++ "." ++ getName attr_)), _patIaterm]]
                               {-# LINE 539 "dist/build/Visage.hs"#-}
{-# NOINLINE sem_VisagePattern_VUnderscore #-}
sem_VisagePattern_VUnderscore :: (Pos) -> T_VisagePattern 
sem_VisagePattern_VUnderscore arg_pos_ = T_VisagePattern (return st20) where
   {-# NOINLINE st20 #-}
   st20 = let
      v19 :: T_VisagePattern_v19 
      v19 = \ (T_VisagePattern_vIn19 ) -> ( let
         _lhsOaterm :: ATerm
         _lhsOaterm = rule13 arg_pos_
         __result_ = T_VisagePattern_vOut19 _lhsOaterm
         in __result_ )
     in C_VisagePattern_s20 v19
   {-# INLINE rule13 #-}
   {-# LINE 121 "./src-ag/Visage.ag" #-}
   rule13 = \ pos_ ->
                               {-# LINE 121 "./src-ag/Visage.ag" #-}
                               AAppl "Pattern" [AAppl "Underscore" [AString (sQ (showAGPos pos_))]]
                               {-# LINE 557 "dist/build/Visage.hs"#-}

-- VisagePatterns ----------------------------------------------
-- wrapper
data Inh_VisagePatterns  = Inh_VisagePatterns {  }
data Syn_VisagePatterns  = Syn_VisagePatterns { aterms_Syn_VisagePatterns :: ([ATerm]) }
{-# INLINABLE wrap_VisagePatterns #-}
wrap_VisagePatterns :: T_VisagePatterns  -> Inh_VisagePatterns  -> (Syn_VisagePatterns )
wrap_VisagePatterns (T_VisagePatterns act) (Inh_VisagePatterns ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_VisagePatterns_vIn22 
        (T_VisagePatterns_vOut22 _lhsOaterms) <- return (inv_VisagePatterns_s23 sem arg)
        return (Syn_VisagePatterns _lhsOaterms)
   )

-- cata
{-# NOINLINE sem_VisagePatterns #-}
sem_VisagePatterns :: VisagePatterns  -> T_VisagePatterns 
sem_VisagePatterns list = Prelude.foldr sem_VisagePatterns_Cons sem_VisagePatterns_Nil (Prelude.map sem_VisagePattern list)

-- semantic domain
newtype T_VisagePatterns  = T_VisagePatterns {
                                             attach_T_VisagePatterns :: Identity (T_VisagePatterns_s23 )
                                             }
newtype T_VisagePatterns_s23  = C_VisagePatterns_s23 {
                                                     inv_VisagePatterns_s23 :: (T_VisagePatterns_v22 )
                                                     }
data T_VisagePatterns_s24  = C_VisagePatterns_s24
type T_VisagePatterns_v22  = (T_VisagePatterns_vIn22 ) -> (T_VisagePatterns_vOut22 )
data T_VisagePatterns_vIn22  = T_VisagePatterns_vIn22 
data T_VisagePatterns_vOut22  = T_VisagePatterns_vOut22 ([ATerm])
{-# NOINLINE sem_VisagePatterns_Cons #-}
sem_VisagePatterns_Cons :: T_VisagePattern  -> T_VisagePatterns  -> T_VisagePatterns 
sem_VisagePatterns_Cons arg_hd_ arg_tl_ = T_VisagePatterns (return st23) where
   {-# NOINLINE st23 #-}
   st23 = let
      v22 :: T_VisagePatterns_v22 
      v22 = \ (T_VisagePatterns_vIn22 ) -> ( let
         _hdX20 = Control.Monad.Identity.runIdentity (attach_T_VisagePattern (arg_hd_))
         _tlX23 = Control.Monad.Identity.runIdentity (attach_T_VisagePatterns (arg_tl_))
         (T_VisagePattern_vOut19 _hdIaterm) = inv_VisagePattern_s20 _hdX20 (T_VisagePattern_vIn19 )
         (T_VisagePatterns_vOut22 _tlIaterms) = inv_VisagePatterns_s23 _tlX23 (T_VisagePatterns_vIn22 )
         _lhsOaterms :: [ATerm]
         _lhsOaterms = rule14 _hdIaterm _tlIaterms
         __result_ = T_VisagePatterns_vOut22 _lhsOaterms
         in __result_ )
     in C_VisagePatterns_s23 v22
   {-# INLINE rule14 #-}
   {-# LINE 107 "./src-ag/Visage.ag" #-}
   rule14 = \ ((_hdIaterm) :: ATerm) ((_tlIaterms) :: [ATerm]) ->
                               {-# LINE 107 "./src-ag/Visage.ag" #-}
                               _hdIaterm : _tlIaterms
                               {-# LINE 610 "dist/build/Visage.hs"#-}
{-# NOINLINE sem_VisagePatterns_Nil #-}
sem_VisagePatterns_Nil ::  T_VisagePatterns 
sem_VisagePatterns_Nil  = T_VisagePatterns (return st23) where
   {-# NOINLINE st23 #-}
   st23 = let
      v22 :: T_VisagePatterns_v22 
      v22 = \ (T_VisagePatterns_vIn22 ) -> ( let
         _lhsOaterms :: [ATerm]
         _lhsOaterms = rule15  ()
         __result_ = T_VisagePatterns_vOut22 _lhsOaterms
         in __result_ )
     in C_VisagePatterns_s23 v22
   {-# INLINE rule15 #-}
   {-# LINE 108 "./src-ag/Visage.ag" #-}
   rule15 = \  (_ :: ()) ->
                               {-# LINE 108 "./src-ag/Visage.ag" #-}
                               []
                               {-# LINE 628 "dist/build/Visage.hs"#-}

-- VisageProduction --------------------------------------------
-- wrapper
data Inh_VisageProduction  = Inh_VisageProduction {  }
data Syn_VisageProduction  = Syn_VisageProduction { aterm_Syn_VisageProduction :: (ATerm) }
{-# INLINABLE wrap_VisageProduction #-}
wrap_VisageProduction :: T_VisageProduction  -> Inh_VisageProduction  -> (Syn_VisageProduction )
wrap_VisageProduction (T_VisageProduction act) (Inh_VisageProduction ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_VisageProduction_vIn25 
        (T_VisageProduction_vOut25 _lhsOaterm) <- return (inv_VisageProduction_s26 sem arg)
        return (Syn_VisageProduction _lhsOaterm)
   )

-- cata
{-# INLINE sem_VisageProduction #-}
sem_VisageProduction :: VisageProduction  -> T_VisageProduction 
sem_VisageProduction ( VProduction con_ children_ rules_ locrules_ ) = sem_VisageProduction_VProduction con_ ( sem_VisageChildren children_ ) ( sem_VisageRules rules_ ) ( sem_VisageRules locrules_ )

-- semantic domain
newtype T_VisageProduction  = T_VisageProduction {
                                                 attach_T_VisageProduction :: Identity (T_VisageProduction_s26 )
                                                 }
newtype T_VisageProduction_s26  = C_VisageProduction_s26 {
                                                         inv_VisageProduction_s26 :: (T_VisageProduction_v25 )
                                                         }
data T_VisageProduction_s27  = C_VisageProduction_s27
type T_VisageProduction_v25  = (T_VisageProduction_vIn25 ) -> (T_VisageProduction_vOut25 )
data T_VisageProduction_vIn25  = T_VisageProduction_vIn25 
data T_VisageProduction_vOut25  = T_VisageProduction_vOut25 (ATerm)
{-# NOINLINE sem_VisageProduction_VProduction #-}
sem_VisageProduction_VProduction :: (ConstructorIdent) -> T_VisageChildren  -> T_VisageRules  -> T_VisageRules  -> T_VisageProduction 
sem_VisageProduction_VProduction arg_con_ arg_children_ arg_rules_ arg_locrules_ = T_VisageProduction (return st26) where
   {-# NOINLINE st26 #-}
   st26 = let
      v25 :: T_VisageProduction_v25 
      v25 = \ (T_VisageProduction_vIn25 ) -> ( let
         _childrenX8 = Control.Monad.Identity.runIdentity (attach_T_VisageChildren (arg_children_))
         _rulesX35 = Control.Monad.Identity.runIdentity (attach_T_VisageRules (arg_rules_))
         _locrulesX35 = Control.Monad.Identity.runIdentity (attach_T_VisageRules (arg_locrules_))
         (T_VisageChildren_vOut7 _childrenIaterms) = inv_VisageChildren_s8 _childrenX8 (T_VisageChildren_vIn7 )
         (T_VisageRules_vOut34 _rulesIaterms) = inv_VisageRules_s35 _rulesX35 (T_VisageRules_vIn34 _rulesOisLoc)
         (T_VisageRules_vOut34 _locrulesIaterms) = inv_VisageRules_s35 _locrulesX35 (T_VisageRules_vIn34 _locrulesOisLoc)
         _lhsOaterm :: ATerm
         _lhsOaterm = rule16 _childrenIaterms _locrulesIaterms _rulesIaterms arg_con_
         _locrulesOisLoc = rule17  ()
         _rulesOisLoc = rule18  ()
         __result_ = T_VisageProduction_vOut25 _lhsOaterm
         in __result_ )
     in C_VisageProduction_s26 v25
   {-# INLINE rule16 #-}
   {-# LINE 73 "./src-ag/Visage.ag" #-}
   rule16 = \ ((_childrenIaterms) :: [ATerm]) ((_locrulesIaterms) :: [ATerm]) ((_rulesIaterms) :: [ATerm]) con_ ->
                              {-# LINE 73 "./src-ag/Visage.ag" #-}
                              AAppl "Alternative" [AString (sQ (getName con_)), AAppl "Children" _childrenIaterms,
                                                    AAppl "Rules" _rulesIaterms,
                                                    AAppl "LocRules" _locrulesIaterms]
                              {-# LINE 687 "dist/build/Visage.hs"#-}
   {-# INLINE rule17 #-}
   {-# LINE 76 "./src-ag/Visage.ag" #-}
   rule17 = \  (_ :: ()) ->
                                    {-# LINE 76 "./src-ag/Visage.ag" #-}
                                    True
                                    {-# LINE 693 "dist/build/Visage.hs"#-}
   {-# INLINE rule18 #-}
   {-# LINE 77 "./src-ag/Visage.ag" #-}
   rule18 = \  (_ :: ()) ->
                                    {-# LINE 77 "./src-ag/Visage.ag" #-}
                                    False
                                    {-# LINE 699 "dist/build/Visage.hs"#-}

-- VisageProductions -------------------------------------------
-- wrapper
data Inh_VisageProductions  = Inh_VisageProductions {  }
data Syn_VisageProductions  = Syn_VisageProductions { aterms_Syn_VisageProductions :: ([ATerm]) }
{-# INLINABLE wrap_VisageProductions #-}
wrap_VisageProductions :: T_VisageProductions  -> Inh_VisageProductions  -> (Syn_VisageProductions )
wrap_VisageProductions (T_VisageProductions act) (Inh_VisageProductions ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_VisageProductions_vIn28 
        (T_VisageProductions_vOut28 _lhsOaterms) <- return (inv_VisageProductions_s29 sem arg)
        return (Syn_VisageProductions _lhsOaterms)
   )

-- cata
{-# NOINLINE sem_VisageProductions #-}
sem_VisageProductions :: VisageProductions  -> T_VisageProductions 
sem_VisageProductions list = Prelude.foldr sem_VisageProductions_Cons sem_VisageProductions_Nil (Prelude.map sem_VisageProduction list)

-- semantic domain
newtype T_VisageProductions  = T_VisageProductions {
                                                   attach_T_VisageProductions :: Identity (T_VisageProductions_s29 )
                                                   }
newtype T_VisageProductions_s29  = C_VisageProductions_s29 {
                                                           inv_VisageProductions_s29 :: (T_VisageProductions_v28 )
                                                           }
data T_VisageProductions_s30  = C_VisageProductions_s30
type T_VisageProductions_v28  = (T_VisageProductions_vIn28 ) -> (T_VisageProductions_vOut28 )
data T_VisageProductions_vIn28  = T_VisageProductions_vIn28 
data T_VisageProductions_vOut28  = T_VisageProductions_vOut28 ([ATerm])
{-# NOINLINE sem_VisageProductions_Cons #-}
sem_VisageProductions_Cons :: T_VisageProduction  -> T_VisageProductions  -> T_VisageProductions 
sem_VisageProductions_Cons arg_hd_ arg_tl_ = T_VisageProductions (return st29) where
   {-# NOINLINE st29 #-}
   st29 = let
      v28 :: T_VisageProductions_v28 
      v28 = \ (T_VisageProductions_vIn28 ) -> ( let
         _hdX26 = Control.Monad.Identity.runIdentity (attach_T_VisageProduction (arg_hd_))
         _tlX29 = Control.Monad.Identity.runIdentity (attach_T_VisageProductions (arg_tl_))
         (T_VisageProduction_vOut25 _hdIaterm) = inv_VisageProduction_s26 _hdX26 (T_VisageProduction_vIn25 )
         (T_VisageProductions_vOut28 _tlIaterms) = inv_VisageProductions_s29 _tlX29 (T_VisageProductions_vIn28 )
         _lhsOaterms :: [ATerm]
         _lhsOaterms = rule19 _hdIaterm _tlIaterms
         __result_ = T_VisageProductions_vOut28 _lhsOaterms
         in __result_ )
     in C_VisageProductions_s29 v28
   {-# INLINE rule19 #-}
   {-# LINE 68 "./src-ag/Visage.ag" #-}
   rule19 = \ ((_hdIaterm) :: ATerm) ((_tlIaterms) :: [ATerm]) ->
                               {-# LINE 68 "./src-ag/Visage.ag" #-}
                               _hdIaterm : _tlIaterms
                               {-# LINE 752 "dist/build/Visage.hs"#-}
{-# NOINLINE sem_VisageProductions_Nil #-}
sem_VisageProductions_Nil ::  T_VisageProductions 
sem_VisageProductions_Nil  = T_VisageProductions (return st29) where
   {-# NOINLINE st29 #-}
   st29 = let
      v28 :: T_VisageProductions_v28 
      v28 = \ (T_VisageProductions_vIn28 ) -> ( let
         _lhsOaterms :: [ATerm]
         _lhsOaterms = rule20  ()
         __result_ = T_VisageProductions_vOut28 _lhsOaterms
         in __result_ )
     in C_VisageProductions_s29 v28
   {-# INLINE rule20 #-}
   {-# LINE 69 "./src-ag/Visage.ag" #-}
   rule20 = \  (_ :: ()) ->
                               {-# LINE 69 "./src-ag/Visage.ag" #-}
                               []
                               {-# LINE 770 "dist/build/Visage.hs"#-}

-- VisageRule --------------------------------------------------
-- wrapper
data Inh_VisageRule  = Inh_VisageRule { isLoc_Inh_VisageRule :: (Bool) }
data Syn_VisageRule  = Syn_VisageRule { aterm_Syn_VisageRule :: (ATerm) }
{-# INLINABLE wrap_VisageRule #-}
wrap_VisageRule :: T_VisageRule  -> Inh_VisageRule  -> (Syn_VisageRule )
wrap_VisageRule (T_VisageRule act) (Inh_VisageRule _lhsIisLoc) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_VisageRule_vIn31 _lhsIisLoc
        (T_VisageRule_vOut31 _lhsOaterm) <- return (inv_VisageRule_s32 sem arg)
        return (Syn_VisageRule _lhsOaterm)
   )

-- cata
{-# INLINE sem_VisageRule #-}
sem_VisageRule :: VisageRule  -> T_VisageRule 
sem_VisageRule ( VRule fieldattrs_ attr_ pat_ rhs_ owrt_ ) = sem_VisageRule_VRule fieldattrs_ attr_ ( sem_VisagePattern pat_ ) ( sem_Expression rhs_ ) owrt_

-- semantic domain
newtype T_VisageRule  = T_VisageRule {
                                     attach_T_VisageRule :: Identity (T_VisageRule_s32 )
                                     }
newtype T_VisageRule_s32  = C_VisageRule_s32 {
                                             inv_VisageRule_s32 :: (T_VisageRule_v31 )
                                             }
data T_VisageRule_s33  = C_VisageRule_s33
type T_VisageRule_v31  = (T_VisageRule_vIn31 ) -> (T_VisageRule_vOut31 )
data T_VisageRule_vIn31  = T_VisageRule_vIn31 (Bool)
data T_VisageRule_vOut31  = T_VisageRule_vOut31 (ATerm)
{-# NOINLINE sem_VisageRule_VRule #-}
sem_VisageRule_VRule :: ([(Identifier,Identifier)]) -> (Identifier) -> T_VisagePattern  -> T_Expression  -> (Bool) -> T_VisageRule 
sem_VisageRule_VRule _ arg_attr_ arg_pat_ arg_rhs_ arg_owrt_ = T_VisageRule (return st32) where
   {-# NOINLINE st32 #-}
   st32 = let
      v31 :: T_VisageRule_v31 
      v31 = \ (T_VisageRule_vIn31 _lhsIisLoc) -> ( let
         _patX20 = Control.Monad.Identity.runIdentity (attach_T_VisagePattern (arg_pat_))
         _rhsX2 = Control.Monad.Identity.runIdentity (attach_T_Expression (arg_rhs_))
         (T_VisagePattern_vOut19 _patIaterm) = inv_VisagePattern_s20 _patX20 (T_VisagePattern_vIn19 )
         (T_Expression_vOut1 _rhsIaterm) = inv_Expression_s2 _rhsX2 (T_Expression_vIn1 )
         _lhsOaterm :: ATerm
         _lhsOaterm = rule21 _lhsIisLoc _patIaterm _rhsIaterm arg_attr_ arg_owrt_
         __result_ = T_VisageRule_vOut31 _lhsOaterm
         in __result_ )
     in C_VisageRule_s32 v31
   {-# INLINE rule21 #-}
   {-# LINE 97 "./src-ag/Visage.ag" #-}
   rule21 = \ ((_lhsIisLoc) :: Bool) ((_patIaterm) :: ATerm) ((_rhsIaterm) :: ATerm) attr_ owrt_ ->
                               {-# LINE 97 "./src-ag/Visage.ag" #-}
                               AAppl (if _lhsIisLoc then "LocRule" else "Rule")
                                     ([AString (sQ (getName attr_)), _patIaterm, _rhsIaterm] ++ if _lhsIisLoc then [AString (sQ (show owrt_))] else [])
                               {-# LINE 824 "dist/build/Visage.hs"#-}

-- VisageRules -------------------------------------------------
-- wrapper
data Inh_VisageRules  = Inh_VisageRules { isLoc_Inh_VisageRules :: (Bool) }
data Syn_VisageRules  = Syn_VisageRules { aterms_Syn_VisageRules :: ([ATerm]) }
{-# INLINABLE wrap_VisageRules #-}
wrap_VisageRules :: T_VisageRules  -> Inh_VisageRules  -> (Syn_VisageRules )
wrap_VisageRules (T_VisageRules act) (Inh_VisageRules _lhsIisLoc) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_VisageRules_vIn34 _lhsIisLoc
        (T_VisageRules_vOut34 _lhsOaterms) <- return (inv_VisageRules_s35 sem arg)
        return (Syn_VisageRules _lhsOaterms)
   )

-- cata
{-# NOINLINE sem_VisageRules #-}
sem_VisageRules :: VisageRules  -> T_VisageRules 
sem_VisageRules list = Prelude.foldr sem_VisageRules_Cons sem_VisageRules_Nil (Prelude.map sem_VisageRule list)

-- semantic domain
newtype T_VisageRules  = T_VisageRules {
                                       attach_T_VisageRules :: Identity (T_VisageRules_s35 )
                                       }
newtype T_VisageRules_s35  = C_VisageRules_s35 {
                                               inv_VisageRules_s35 :: (T_VisageRules_v34 )
                                               }
data T_VisageRules_s36  = C_VisageRules_s36
type T_VisageRules_v34  = (T_VisageRules_vIn34 ) -> (T_VisageRules_vOut34 )
data T_VisageRules_vIn34  = T_VisageRules_vIn34 (Bool)
data T_VisageRules_vOut34  = T_VisageRules_vOut34 ([ATerm])
{-# NOINLINE sem_VisageRules_Cons #-}
sem_VisageRules_Cons :: T_VisageRule  -> T_VisageRules  -> T_VisageRules 
sem_VisageRules_Cons arg_hd_ arg_tl_ = T_VisageRules (return st35) where
   {-# NOINLINE st35 #-}
   st35 = let
      v34 :: T_VisageRules_v34 
      v34 = \ (T_VisageRules_vIn34 _lhsIisLoc) -> ( let
         _hdX32 = Control.Monad.Identity.runIdentity (attach_T_VisageRule (arg_hd_))
         _tlX35 = Control.Monad.Identity.runIdentity (attach_T_VisageRules (arg_tl_))
         (T_VisageRule_vOut31 _hdIaterm) = inv_VisageRule_s32 _hdX32 (T_VisageRule_vIn31 _hdOisLoc)
         (T_VisageRules_vOut34 _tlIaterms) = inv_VisageRules_s35 _tlX35 (T_VisageRules_vIn34 _tlOisLoc)
         _lhsOaterms :: [ATerm]
         _lhsOaterms = rule22 _hdIaterm _tlIaterms
         _hdOisLoc = rule23 _lhsIisLoc
         _tlOisLoc = rule24 _lhsIisLoc
         __result_ = T_VisageRules_vOut34 _lhsOaterms
         in __result_ )
     in C_VisageRules_s35 v34
   {-# INLINE rule22 #-}
   {-# LINE 92 "./src-ag/Visage.ag" #-}
   rule22 = \ ((_hdIaterm) :: ATerm) ((_tlIaterms) :: [ATerm]) ->
                               {-# LINE 92 "./src-ag/Visage.ag" #-}
                               _hdIaterm : _tlIaterms
                               {-# LINE 879 "dist/build/Visage.hs"#-}
   {-# INLINE rule23 #-}
   rule23 = \ ((_lhsIisLoc) :: Bool) ->
     _lhsIisLoc
   {-# INLINE rule24 #-}
   rule24 = \ ((_lhsIisLoc) :: Bool) ->
     _lhsIisLoc
{-# NOINLINE sem_VisageRules_Nil #-}
sem_VisageRules_Nil ::  T_VisageRules 
sem_VisageRules_Nil  = T_VisageRules (return st35) where
   {-# NOINLINE st35 #-}
   st35 = let
      v34 :: T_VisageRules_v34 
      v34 = \ (T_VisageRules_vIn34 _lhsIisLoc) -> ( let
         _lhsOaterms :: [ATerm]
         _lhsOaterms = rule25  ()
         __result_ = T_VisageRules_vOut34 _lhsOaterms
         in __result_ )
     in C_VisageRules_s35 v34
   {-# INLINE rule25 #-}
   {-# LINE 93 "./src-ag/Visage.ag" #-}
   rule25 = \  (_ :: ()) ->
                               {-# LINE 93 "./src-ag/Visage.ag" #-}
                               []
                               {-# LINE 903 "dist/build/Visage.hs"#-}
