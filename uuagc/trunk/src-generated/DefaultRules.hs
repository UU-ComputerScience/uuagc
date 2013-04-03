{-# LANGUAGE Rank2Types, GADTs #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DefaultRules where
{-# LINE 2 "./src-ag/AbstractSyntax.ag" #-}

-- AbstractSyntax.ag imports
import Data.Set(Set)
import Data.Map(Map)
import Patterns    (Pattern(..),Patterns)
import Expression  (Expression(..))
import Macro --marcos
import CommonTypes
import ErrorMessages
{-# LINE 17 "dist/build/DefaultRules.hs" #-}

{-# LINE 2 "./src-ag/Patterns.ag" #-}

-- Patterns.ag imports
import UU.Scanner.Position(Pos)
import CommonTypes (ConstructorIdent,Identifier)
{-# LINE 24 "dist/build/DefaultRules.hs" #-}

{-# LINE 15 "./src-ag/DefaultRules.ag" #-}

import qualified Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Map(Map)
import qualified Data.Sequence as Seq
import Data.Sequence(Seq,(><))
import UU.Scanner.Position(noPos)
import Pretty
import Data.Maybe
import HsToken
import HsTokenScanner
import Data.List(intersperse)
import Data.Char

import AbstractSyntax
import ErrorMessages

import Options
{-# LINE 46 "dist/build/DefaultRules.hs" #-}
import Control.Monad.Identity (Identity)
import qualified Control.Monad.Identity
{-# LINE 78 "./src-ag/DefaultRules.ag" #-}

fieldName n       = '@' : getName n

locName n         = "@loc." ++ getName n

attrName fld attr
 | fld == _LOC    = locName attr
 | fld == _FIELD  = fieldName attr
 | otherwise      = '@' : getName fld ++ "." ++ getName attr

_ACHILD = Ident "(" noPos -- hack

mkLocVar = AGField _LOC


buildConExpr ocaml typeSyns rename nt con1 fs
 | nt `elem` map fst typeSyns  =  if ocaml then synonymMl else synonymHs
 | otherwise                   =  normalExpr
 where con                     = getName con1
       tup                     = " " ++ buildTuple fs
       args                    = " " ++ unwords fs
       normalExpr              = conname' ++ args

       conname' | rename    = getName nt ++ "_" ++ getName con1
                | otherwise = getName con1

       synonymHs  | con == "Tuple"    = buildTuple fs
                  | con == "Cons"     = "(:)" ++ args
                  | con == "Nil"      = case lookup nt typeSyns of
                                          Just (Map _ _)  -> "Data.Map.empty"
                                          Just (IntMap _) -> "Data.IntMap.empty"
                                          Just (OrdSet _) -> "Data.Set.empty"
                                          Just IntSet     -> "Data.IntSet.empty"
                                          _               -> "[]"
                  | con == "Just"     = "Just" ++ args
                  | con == "Nothing"  = "Nothing"
                  | con == "Entry"    = ( case lookup nt typeSyns of
                                            Just (Map _ _)  -> "Data.Map.insert"
                                            Just (IntMap _) -> "Data.IntMap.insert"
                                            Just (OrdSet _) -> "Data.Set.insert"
                                            Just IntSet     -> "Data.IntSet.insert" ) ++ args
                  | otherwise         = normalExpr

       synonymMl  | con == "Tuple"    = buildTuple fs
                  | con == "Cons"     = "(::)" ++ tup
                  | con == "Nil"      = case lookup nt typeSyns of
                                          Just (Map _ _)  -> prefixMod nt "empty"
                                          Just (IntMap _) -> prefixMod nt "empty"
                                          Just (OrdSet _) -> prefixMod nt "empty"
                                          Just IntSet     -> prefixMod nt "empty"
                                          _               -> "[]"
                  | con == "Just"     = "Some" ++ tup
                  | con == "Nothing"  = "None"
                  | con == "Entry"    = ( case lookup nt typeSyns of
                                            Just (Map _ _)  -> prefixMod nt "add"
                                            Just (IntMap _) -> prefixMod nt "add"
                                            Just (OrdSet _) -> prefixMod nt "add"
                                            Just IntSet     -> prefixMod nt "add" ) ++ args
                  | otherwise         = normalExpr

       prefixMod nt nm = "M_" ++ getName nt ++ "." ++ nm

concatSeq = foldr (Seq.><) Seq.empty

splitAttrs :: Map Identifier a -> [Identifier] -> ([(Identifier,a)],[Identifier])	  -- a used as (String,String)
splitAttrs _      []
  =  ([],[])
splitAttrs useMap (n:rest)
  =  let (uses,normals) = splitAttrs useMap rest
     in case Map.lookup n useMap of
          Just x  -> ((n,x):uses ,   normals )
          Nothing -> (      uses , n:normals )

removeDefined ::  Set (Identifier,Identifier) -> (Identifier,Attributes) -> (Identifier,[Identifier])
removeDefined defined (fld,as)
  = ( fld
    , [ a
      | a <- Map.keys as
      , not (Set.member (fld,a) defined)
      ]
    )

{-# LINE 132 "dist/build/DefaultRules.hs" #-}

{-# LINE 226 "./src-ag/DefaultRules.ag" #-}




deprecatedCopyRuleError nt con fld a
 = let mesg =
                "In the definitions for alternative"
            >#< getName con
            >#< "of nonterminal"
            >#< getName nt
            >|< ","
            >-< "the value of field"
            >#< getName a
            >#< "is copied by a copy-rule."
            >-< "Copying the value of a field using a copy-rule is deprecated"
            >-< "Please add the following lines to your code:"
            >-< (    "SEM"
                >#< getName nt
                >-< indent 2 (      "|"
                             >#< getName con
                             >#< getName fld
                             >#< "."
                             >#< a
                             >#< "="
                             >#< "@"
                             >|< a
                             )
                )
    in  CustomError True (getPos a) mesg


missingRuleErrorExpr nt con fld a
 = "error \"missing rule: "
   ++ show nt  ++ "." ++ show con ++ "."
   ++ show fld ++ "." ++ show a   ++ "\""



makeRule :: (Identifier,Identifier) -> Expression -> String -> Bool -> Maybe Error -> Rule
makeRule (f1,a1) expr origin identity mbDelayedError
 = Rule Nothing
        (Alias f1 a1 (Underscore noPos))
        expr
        False
        origin
        False
        True
        identity
        mbDelayedError
        False


useRule :: Set Identifier -> [(Identifier,Attributes)] -> (Identifier,(String,String,String)) -> Rule
useRule locals ch_outs (n,(op,e,pos))
 =  let elems = [ fld
                | (fld,as) <- ch_outs
                , Map.member n as
                ]

        isOp [] = False
        isOp (c:cs)
          | isSpace c = isOp cs
          | isAlpha c = case dropWhile isAlpha cs of
	    	           ('.':cs2) -> isOp cs2 -- fully qualified name, drop prefix
			   _         -> False
          | c == '('  = False
          | otherwise = True

        tks | Set.member n locals  =  [mkLocVar n noPos Nothing]
            | null elems           =  lexTokens noPos e
            | otherwise            =  lexTokens noPos str
                                      where
                                        opExpr l r
                                          | isOp op   = l ++ " " ++ op ++ " " ++ r         -- takes the associativity of the operator
                                          | otherwise = "(" ++ op ++ " " ++ l ++ " " ++ r ++ ")"  -- associates to the right
                                        str = foldr1 opExpr (map (flip attrName n) elems)

    in makeRule (_LHS,n)
                (Expression noPos tks)
                ("use rule " ++ pos)
                False
                Nothing


selfRule :: Bool -> Identifier -> [HsToken] -> Rule
selfRule lhsNecLoc attr tks
 = makeRule (if lhsNecLoc then _LHS else _LOC,attr)
               (Expression noPos tks)
               "self rule"
               False
               Nothing




concatRE rsess = let (rss,ess) = unzip rsess
                 in (concat rss, concatSeq ess)


copyRule :: Options -> Set NontermIdent -> Identifier -> Identifier -> Bool -> Set Identifier -> (Map Identifier Identifier, (Identifier,[Identifier])) -> ([Rule], Seq Error)
copyRule options wrappers nt con modcopy locals (env,(fld,as))
 = concatRE (map copyRu as)

 where
       copyRu a
           = ( [ makeRule (fld,a)
                          (Expression noPos tks)
                          (cruletxt sel)
                          True
                          mbDelayedErr
               ]
             , err
             )

        where
              sel
               |    not modcopy
                 && Set.member a locals  =  Just _LOC
               | otherwise               =  Map.lookup a env

              (tks,err,mbDelayedErr)
               = case sel of
                  Nothing         -> let tks = [HsToken (missingRuleErrorExpr nt con fld a) noPos]
                                         err = MissingRule nt con fld a
                                     in if nt `Set.member` wrappers && kennedyWarren options
                                        then (tks, Seq.empty, Just err)  -- yield error only if the rule is actually scheduled; for kennedyWarren code gen only
                                        else (tks, Seq.singleton err, Nothing)
                  Just f
                   | f == _ACHILD -> ( [AGLocal a noPos Nothing]
                                     , Seq.singleton (deprecatedCopyRuleError nt con fld a)
                                     , Nothing
                                     )
                   | otherwise    -> ( [AGField f a noPos Nothing]
                                     , Seq.empty
                                     , Nothing
                                     )

              cruletxt sel
               | local                            = "copy rule (from local)"
               | deprChild                        = "deprecated child copy"
               | Set.member a locals && nonlocal  = "modified copy rule"
               | incoming && outgoing             = "copy rule (chain)"
               | incoming                         = "copy rule (down)"
               | outgoing                         = "copy rule (up)"
               | otherwise                        = "copy rule (chain)"
                where outgoing  =  fld == _LHS
                      incoming  =  maybe False (== _LHS)    sel
                      nonlocal  =  maybe False (/= _LOC)    sel
                      local     =  maybe False (== _LOC)    sel
                      deprChild =  maybe False (== _ACHILD) sel
{-# LINE 285 "dist/build/DefaultRules.hs" #-}

{-# LINE 460 "./src-ag/DefaultRules.ag" #-}

buildTuple fs = "(" ++ concat (intersperse "," fs) ++ ")"

addAugments :: (Identifier, [Expression]) -> [Rule] -> [Rule]
addAugments (_, exprs) rules
  | null exprs = rules
addAugments (syn, exprs) rules
  = [rule] ++ funRules ++ map modify rules
  where
    rule = Rule Nothing (Alias _LHS syn (Underscore noPos)) rhs False "augmented rule" False True False Nothing False
    rhs  = Expression noPos tks
    tks  = [ HsToken "foldr ($) " noPos, mkLocVar substSyn noPos Nothing, HsToken " [" noPos] ++ funs ++ [HsToken "]" noPos]
    funs = intersperse (HsToken ", " noPos) (map (\n -> mkLocVar n noPos Nothing) funNames)

    substSyn = Ident (show syn ++ "_augmented_syn") (getPos syn)
    funNames = zipWith (\i _ -> Ident (show syn ++ "_augmented_f" ++ show i) (getPos syn)) [1..] exprs
    funRules = zipWith (\name expr -> Rule Nothing (Alias _LOC name (Underscore noPos)) expr False "augment function" False True False Nothing False) funNames exprs

    modify (Rule mbNm pat rhs owrt origin expl pure identity mbErr eager)
      | containsSyn pat = Rule mbNm (modifyPat pat) rhs owrt origin expl pure identity mbErr eager
    modify r = r

    containsSyn (Constr _ pats)   = any containsSyn pats
    containsSyn (Product _ pats)  = any containsSyn pats
    containsSyn (Irrefutable pat) = containsSyn pat
    containsSyn (Alias field attr pat) = (field == _LHS && attr == syn) || containsSyn pat
    containsSyn _ = False

    modifyPat (Constr name pats) = Constr name (map modifyPat pats)
    modifyPat (Product pos pats) = Product pos (map modifyPat pats)
    modifyPat (Irrefutable pat)  = Irrefutable (modifyPat pat)
    modifyPat (Alias field attr pat)
      | field == _LHS && attr == syn = Alias _LOC substSyn (modifyPat pat)
      | otherwise                    = Alias field attr (modifyPat pat)
    modifyPat p = p

-- adds the additional rules needed for around, which creates a sequence of
-- rules that form a function that each transforms the semantics of a child
-- before attaching the child.
-- The rule defines a local attribute "<child>_around" and <child> is dependent
-- on this attribute.
addArounds :: (Identifier, [Expression]) -> [Rule] -> [Rule]
addArounds (_, exprs) rules | null exprs = rules
addArounds (child, exprs) rules
  = [rule] ++ funRules ++ rules
  where
    rule = Rule Nothing (Alias _LOC childLoc (Underscore noPos)) rhs False "around rule" False True False Nothing False
    rhs  = Expression noPos tks
    tks  = [ HsToken "\\s -> foldr ($) s " noPos, HsToken " [" noPos] ++ funs ++ [HsToken "]" noPos]
    funs = intersperse (HsToken ", " noPos) (map (\n -> mkLocVar n noPos Nothing) funNames)

    childLoc = Ident (show child ++ "_around") (getPos child)
    funNames = zipWith (\i _ -> Ident (show child ++ "_around_f" ++ show i) (getPos child)) [1..] exprs
    funRules = zipWith (\name expr -> Rule Nothing (Alias _LOC name (Underscore noPos)) expr False "around function" False True False Nothing False) funNames exprs

-- adds the additional rules needed for merging.
-- It produces for each merging child a rule with local attribute: "<child>_merged".
-- this rules takes the semantics of the first children and feeds it to the function
-- represented by this attribute. This attribute then defines the semantics for
-- the merging child.
addMerges :: (Identifier, (Identifier,[Identifier],Expression)) -> [Rule] -> [Rule]
addMerges (target,(_,_,expr)) rules
  = rule : rules
  where
    rule = Rule Nothing (Alias _LOC childLoc (Underscore noPos)) expr False "merge rule" False True False Nothing False
    childLoc = Ident (show target ++ "_merge") (getPos target)
{-# LINE 354 "dist/build/DefaultRules.hs" #-}

{-# LINE 578 "./src-ag/DefaultRules.ag" #-}

elimSelfId :: NontermIdent -> [Identifier] -> Type -> Type
elimSelfId nt args Self = NT nt (map getName args) False
elimSelfId _ _ tp = tp

elimSelfStr :: NontermIdent -> [String] -> Type -> Type
elimSelfStr nt args Self = NT nt args False
elimSelfStr _ _ tp = tp
{-# LINE 365 "dist/build/DefaultRules.hs" #-}

{-# LINE 630 "./src-ag/DefaultRules.ag" #-}

-- When a rule has a name, create an alias for a rule
-- and a modified rule that refers to the alias
-- Thus it removes rule names from rules
mkRuleAlias :: Rule -> (Rule, Maybe Rule)
mkRuleAlias r@(Rule Nothing _ _ _ _ _ _ _ _ _) = (r, Nothing)
mkRuleAlias (Rule (Just nm) pat expr owrt origin expl pure identity mbErr eager) = (r', Just alias) where
  alias = Rule Nothing (Alias _LOC (Ident ("_rule_" ++ show nm) pos) (Underscore pos)) expr owrt origin expl pure identity mbErr eager
  pos   = getPos nm
  expr' = Expression pos tks
  tks   = [mkLocVar (Ident ("_rule_" ++ show nm) pos) pos (Just ("Indirection to rule " ++ show nm))]
  r'    = Rule Nothing pat expr' owrt origin False True identity Nothing False
{-# LINE 380 "dist/build/DefaultRules.hs" #-}

{-# LINE 647 "./src-ag/DefaultRules.ag" #-}

needsMultiRules :: Options -> Bool
needsMultiRules opts = (visit opts || withCycle opts) && not (kennedyWarren opts)
{-# LINE 386 "dist/build/DefaultRules.hs" #-}

{-# LINE 652 "./src-ag/DefaultRules.ag" #-}

{-
multiRule replaces
  loc.(a,b) = e
by
  loc.tup1  = e
  loc.(a,_) = @loc.tup1
  loc.(_,b) = @loc.tup1
It needs to thread a unique number for inventing names for the tuples.

It also works for nested tuples:
  loc.(a,(b,c)) = e
becomes
  loc.tup1      = e
  loc.(a,_)     = @loc.tup1
  loc.(_,tup2)  = @loc.tup1
  loc.(b,_)     = @loc.tup2
  loc.(_,c)     = @loc.tup2
-}

multiRule :: Rule -> Int -> ([Rule], Int)
multiRule (Rule _ pat expr owrt origin expl pure identity mbErr eager) uniq
  =  let f :: Bool -> (Pattern->Pattern) -> Expression -> Pattern -> Int -> (Pattern, ([Rule], Int))
         f expl' w e (Product pos pats) n
           = let freshName = Ident ("_tup" ++ show n) pos
                 freshExpr = Expression pos freshTks
                 freshTks  = [AGField _LOC freshName pos Nothing]
                 freshPat  = Alias _LOC freshName (Underscore pos)
                 a = length pats - 1
                 us b p = Product pos (replicate (a-b) (Underscore pos) ++ [p] ++ replicate b (Underscore pos))
                 g :: Pattern -> ([Pattern],[Rule],Int) -> ([Pattern],[Rule],Int)
                 g p (xs1,rs1,n1)   = let (x2,(rs2,n2)) = f False (us (length xs1)) freshExpr p n1
                                      in  (x2:xs1, rs2++rs1, n2)
                 (xs9,rs9,n9) = foldr g ([], [], n+1) pats
             in  ( freshPat
                 , ( Rule Nothing (w freshPat) e owrt origin expl' True False mbErr eager : rs9
                   , n9
                   )
                 )
         f expl' w e p n
           = ( p
             , ( [Rule Nothing (w p) e owrt origin expl' True False mbErr eager]
               , n
               )
             )
     in snd (f expl id expr pat uniq)

{-# LINE 436 "dist/build/DefaultRules.hs" #-}
-- Child -------------------------------------------------------
-- wrapper
data Inh_Child  = Inh_Child { con_Inh_Child :: !(ConstructorIdent), cr_Inh_Child :: !(Bool), inhMap_Inh_Child :: !(Map Identifier Attributes), merged_Inh_Child :: !(Set Identifier), nt_Inh_Child :: !(NontermIdent), params_Inh_Child :: !([Identifier]), synMap_Inh_Child :: !(Map Identifier Attributes) }
data Syn_Child  = Syn_Child { errors_Syn_Child :: !(Seq Error), field_Syn_Child :: !( (Identifier,Type,ChildKind) ), inherited_Syn_Child :: !(Attributes), name_Syn_Child :: !(Identifier), output_Syn_Child :: !(Child), synthesized_Syn_Child :: !(Attributes) }
{-# INLINABLE wrap_Child #-}
wrap_Child :: T_Child  -> Inh_Child  -> (Syn_Child )
wrap_Child !(T_Child act) !(Inh_Child _lhsIcon _lhsIcr _lhsIinhMap _lhsImerged _lhsInt _lhsIparams _lhsIsynMap) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_Child_vIn0 _lhsIcon _lhsIcr _lhsIinhMap _lhsImerged _lhsInt _lhsIparams _lhsIsynMap
        !(T_Child_vOut0 _lhsOerrors _lhsOfield _lhsOinherited _lhsOname _lhsOoutput _lhsOsynthesized) <- return (inv_Child_s0 sem K_Child_v0 arg)
        return (Syn_Child _lhsOerrors _lhsOfield _lhsOinherited _lhsOname _lhsOoutput _lhsOsynthesized)
   )

-- cata
{-# INLINE sem_Child #-}
sem_Child :: Child  -> T_Child 
sem_Child ( Child !name_ !tp_ !kind_ ) = sem_Child_Child name_ tp_ kind_

-- semantic domain
newtype T_Child  = T_Child {
                           attach_T_Child :: Identity (T_Child_s0 )
                           }
data T_Child_s0  where C_Child_s0 :: {
                                     inv_Child_s0 :: !(forall t. K_Child_s0  t -> t)
                                     } -> T_Child_s0 
data T_Child_s1  = C_Child_s1
data T_Child_s26  = C_Child_s26
newtype T_Child_s56  = C_Child_s56 {
                                   inv_Child_s56 :: (T_Child_v53 )
                                   }
data K_Child_s0 k  where
   K_Child_v0 :: K_Child_s0  (T_Child_v0 )
   K_Child_v13 :: K_Child_s0  (T_Child_v13 )
   K_Child_v52 :: K_Child_s0  (T_Child_v52 )
type T_Child_v0  = (T_Child_vIn0 ) -> (T_Child_vOut0 )
data T_Child_vIn0  = T_Child_vIn0 !(ConstructorIdent) !(Bool) !(Map Identifier Attributes) !(Set Identifier) !(NontermIdent) !([Identifier]) !(Map Identifier Attributes)
data T_Child_vOut0  = T_Child_vOut0 !(Seq Error) !( (Identifier,Type,ChildKind) ) !(Attributes) !(Identifier) !(Child) !(Attributes)
type T_Child_v13  = (T_Child_vIn13 ) -> (T_Child_vOut13 )
data T_Child_vIn13  = T_Child_vIn13 !(Map Identifier Attributes) !(Set Identifier) !(Map Identifier Attributes)
data T_Child_vOut13  = T_Child_vOut13 !(Seq Error) !( (Identifier,Type,ChildKind) ) !(Attributes) !(Identifier) !(Child) !(Attributes)
type T_Child_v52  = (T_Child_vIn52 ) -> (T_Child_vOut52 )
data T_Child_vIn52  = T_Child_vIn52 !(Map Identifier Attributes) !(Set Identifier) !(Map Identifier Attributes)
data T_Child_vOut52  = T_Child_vOut52 !(Seq Error) !( (Identifier,Type,ChildKind) ) !(Attributes) !(Identifier) !(Attributes) !(T_Child_s56 )
type T_Child_v53  = (T_Child_vIn53 ) -> (T_Child_vOut53 )
data T_Child_vIn53  = T_Child_vIn53 
data T_Child_vOut53  = T_Child_vOut53 !(Child)
{-# NOINLINE sem_Child_Child #-}
sem_Child_Child :: (Identifier) -> (Type) -> (ChildKind) -> T_Child 
sem_Child_Child !arg_name_ !arg_tp_ !arg_kind_ = T_Child (return st0) where
   {-# NOINLINE st0 #-}
   !st0 = let
      k0 :: K_Child_s0  t -> t
      k0 K_Child_v0 = v0
      k0 K_Child_v13 = v13
      k0 K_Child_v52 = v52
      v0 :: T_Child_v0 
      v0 = \ !(T_Child_vIn0 _lhsIcon _lhsIcr _lhsIinhMap _lhsImerged _lhsInt _lhsIparams _lhsIsynMap) -> (
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule11  () in
         let _lhsOfield ::  (Identifier,Type,ChildKind) 
             !_lhsOfield = rule6 arg_kind_ arg_name_ arg_tp_ in
         let !_chnt = rule0 arg_name_ arg_tp_ in
         let !_inh = rule1 _chnt _lhsIinhMap in
         let !(!_nt,!_params) = rule7 arg_name_ arg_tp_ in
         let !_inh1 = rule8 _inh _nt _params in
         let _lhsOinherited :: Attributes
             !_lhsOinherited = rule4 _inh1 in
         let _lhsOname :: Identifier
             !_lhsOname = rule3 arg_name_ in
         let _lhsOoutput :: Child
             !_lhsOoutput = rule10 arg_kind_ arg_name_ arg_tp_ in
         let !_syn = rule2 _chnt _lhsIsynMap in
         let !_syn1 = rule9 _nt _params _syn in
         let _lhsOsynthesized :: Attributes
             !_lhsOsynthesized = rule5 _lhsImerged _syn1 arg_name_ in
         let !__result_ = T_Child_vOut0 _lhsOerrors _lhsOfield _lhsOinherited _lhsOname _lhsOoutput _lhsOsynthesized
          in __result_ )
      v13 :: T_Child_v13 
      v13 = \ !(T_Child_vIn13 _lhsIinhMap _lhsImerged _lhsIsynMap) -> (
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule11  () in
         let _lhsOfield ::  (Identifier,Type,ChildKind) 
             !_lhsOfield = rule6 arg_kind_ arg_name_ arg_tp_ in
         let !_chnt = rule0 arg_name_ arg_tp_ in
         let !_inh = rule1 _chnt _lhsIinhMap in
         let !(!_nt,!_params) = rule7 arg_name_ arg_tp_ in
         let !_inh1 = rule8 _inh _nt _params in
         let _lhsOinherited :: Attributes
             !_lhsOinherited = rule4 _inh1 in
         let _lhsOname :: Identifier
             !_lhsOname = rule3 arg_name_ in
         let _lhsOoutput :: Child
             !_lhsOoutput = rule10 arg_kind_ arg_name_ arg_tp_ in
         let !_syn = rule2 _chnt _lhsIsynMap in
         let !_syn1 = rule9 _nt _params _syn in
         let _lhsOsynthesized :: Attributes
             !_lhsOsynthesized = rule5 _lhsImerged _syn1 arg_name_ in
         let !__result_ = T_Child_vOut13 _lhsOerrors _lhsOfield _lhsOinherited _lhsOname _lhsOoutput _lhsOsynthesized
          in __result_ )
      v52 :: T_Child_v52 
      v52 = \ !(T_Child_vIn52 _lhsIinhMap _lhsImerged _lhsIsynMap) -> (
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule11  () in
         let _lhsOfield ::  (Identifier,Type,ChildKind) 
             !_lhsOfield = rule6 arg_kind_ arg_name_ arg_tp_ in
         let !_chnt = rule0 arg_name_ arg_tp_ in
         let !_inh = rule1 _chnt _lhsIinhMap in
         let !(!_nt,!_params) = rule7 arg_name_ arg_tp_ in
         let !_inh1 = rule8 _inh _nt _params in
         let _lhsOinherited :: Attributes
             !_lhsOinherited = rule4 _inh1 in
         let _lhsOname :: Identifier
             !_lhsOname = rule3 arg_name_ in
         let !_syn = rule2 _chnt _lhsIsynMap in
         let !_syn1 = rule9 _nt _params _syn in
         let _lhsOsynthesized :: Attributes
             !_lhsOsynthesized = rule5 _lhsImerged _syn1 arg_name_ in
         let !__st_ = st56  ()
             !__result_ = T_Child_vOut52 _lhsOerrors _lhsOfield _lhsOinherited _lhsOname _lhsOsynthesized __st_
          in __result_ )
     in C_Child_s0 k0
   {-# NOINLINE st56 #-}
   st56 = \  (_ :: ()) -> let
      v53 :: T_Child_v53 
      v53 = \ !(T_Child_vIn53 ) -> (
         let _lhsOoutput :: Child
             !_lhsOoutput = rule10 arg_kind_ arg_name_ arg_tp_ in
         let !__result_ = T_Child_vOut53 _lhsOoutput
          in __result_ )
     in C_Child_s56 v53
   {-# NOINLINE[1] rule0 #-}
   {-# LINE 19 "./src-ag/DistChildAttr.ag" #-}
   rule0 = \ !name_ !tp_ ->
                       {-# LINE 19 "./src-ag/DistChildAttr.ag" #-}
                       case tp_ of
                         NT nt _ _ -> nt
                         Self      -> error ("The type of child " ++ show name_ ++ " should not be a Self type.")
                         Haskell t -> identifier ""
                       {-# LINE 576 "dist/build/DefaultRules.hs"#-}
   {-# NOINLINE[1] rule1 #-}
   {-# LINE 23 "./src-ag/DistChildAttr.ag" #-}
   rule1 = \ !_chnt ((!_lhsIinhMap) :: Map Identifier Attributes) ->
                      {-# LINE 23 "./src-ag/DistChildAttr.ag" #-}
                      Map.findWithDefault Map.empty _chnt     _lhsIinhMap
                      {-# LINE 582 "dist/build/DefaultRules.hs"#-}
   {-# NOINLINE[1] rule2 #-}
   {-# LINE 24 "./src-ag/DistChildAttr.ag" #-}
   rule2 = \ !_chnt ((!_lhsIsynMap) :: Map Identifier Attributes) ->
                      {-# LINE 24 "./src-ag/DistChildAttr.ag" #-}
                      Map.findWithDefault Map.empty _chnt     _lhsIsynMap
                      {-# LINE 588 "dist/build/DefaultRules.hs"#-}
   {-# NOINLINE[1] rule3 #-}
   {-# LINE 200 "./src-ag/DefaultRules.ag" #-}
   rule3 = \ !name_ ->
                         {-# LINE 200 "./src-ag/DefaultRules.ag" #-}
                         name_
                         {-# LINE 594 "dist/build/DefaultRules.hs"#-}
   {-# NOINLINE[1] rule4 #-}
   {-# LINE 209 "./src-ag/DefaultRules.ag" #-}
   rule4 = \ !_inh1 ->
                            {-# LINE 209 "./src-ag/DefaultRules.ag" #-}
                            _inh1
                            {-# LINE 600 "dist/build/DefaultRules.hs"#-}
   {-# NOINLINE[1] rule5 #-}
   {-# LINE 210 "./src-ag/DefaultRules.ag" #-}
   rule5 = \ ((!_lhsImerged) :: Set Identifier) !_syn1 !name_ ->
                              {-# LINE 210 "./src-ag/DefaultRules.ag" #-}
                              if name_ `Set.member` _lhsImerged
                              then Map.empty
                              else _syn1
                              {-# LINE 608 "dist/build/DefaultRules.hs"#-}
   {-# NOINLINE[1] rule6 #-}
   {-# LINE 546 "./src-ag/DefaultRules.ag" #-}
   rule6 = \ !kind_ !name_ !tp_ ->
                        {-# LINE 546 "./src-ag/DefaultRules.ag" #-}
                        (name_,tp_,kind_)
                        {-# LINE 614 "dist/build/DefaultRules.hs"#-}
   {-# NOINLINE[1] rule7 #-}
   {-# LINE 568 "./src-ag/DefaultRules.ag" #-}
   rule7 = \ !name_ !tp_ ->
                           {-# LINE 568 "./src-ag/DefaultRules.ag" #-}
                           case tp_ of
                             NT nt params _ -> (nt, params)
                             Self           -> error ("The type of child " ++ show name_ ++ " should not be a Self type.")
                             Haskell t      -> (identifier t, [])
                           {-# LINE 623 "dist/build/DefaultRules.hs"#-}
   {-# NOINLINE[1] rule8 #-}
   {-# LINE 572 "./src-ag/DefaultRules.ag" #-}
   rule8 = \ !_inh !_nt !_params ->
               {-# LINE 572 "./src-ag/DefaultRules.ag" #-}
               Map.map (elimSelfStr _nt     _params    ) _inh
               {-# LINE 629 "dist/build/DefaultRules.hs"#-}
   {-# NOINLINE[1] rule9 #-}
   {-# LINE 573 "./src-ag/DefaultRules.ag" #-}
   rule9 = \ !_nt !_params !_syn ->
               {-# LINE 573 "./src-ag/DefaultRules.ag" #-}
               Map.map (elimSelfStr _nt     _params    ) _syn
               {-# LINE 635 "dist/build/DefaultRules.hs"#-}
   {-# NOINLINE[1] rule10 #-}
   {-# LINE 614 "./src-ag/DefaultRules.ag" #-}
   rule10 = \ !kind_ !name_ !tp_ ->
                 {-# LINE 614 "./src-ag/DefaultRules.ag" #-}
                 Child name_ tp_ kind_
                 {-# LINE 641 "dist/build/DefaultRules.hs"#-}
   {-# NOINLINE[1] rule11 #-}
   rule11 = \  (_ :: ()) ->
     Seq.empty

-- Children ----------------------------------------------------
-- wrapper
data Inh_Children  = Inh_Children { con_Inh_Children :: !(ConstructorIdent), cr_Inh_Children :: !(Bool), inhMap_Inh_Children :: !(Map Identifier Attributes), merged_Inh_Children :: !(Set Identifier), nt_Inh_Children :: !(NontermIdent), params_Inh_Children :: !([Identifier]), synMap_Inh_Children :: !(Map Identifier Attributes) }
data Syn_Children  = Syn_Children { errors_Syn_Children :: !(Seq Error), fields_Syn_Children :: !([(Identifier,Type,ChildKind)]), inputs_Syn_Children :: !([(Identifier, Attributes)]), output_Syn_Children :: !(Children), outputs_Syn_Children :: !([(Identifier, Attributes)]) }
{-# INLINABLE wrap_Children #-}
wrap_Children :: T_Children  -> Inh_Children  -> (Syn_Children )
wrap_Children !(T_Children act) !(Inh_Children _lhsIcon _lhsIcr _lhsIinhMap _lhsImerged _lhsInt _lhsIparams _lhsIsynMap) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_Children_vIn1 _lhsIcon _lhsIcr _lhsIinhMap _lhsImerged _lhsInt _lhsIparams _lhsIsynMap
        !(T_Children_vOut1 _lhsOerrors _lhsOfields _lhsOinputs _lhsOoutput _lhsOoutputs) <- return (inv_Children_s2 sem K_Children_v1 arg)
        return (Syn_Children _lhsOerrors _lhsOfields _lhsOinputs _lhsOoutput _lhsOoutputs)
   )

-- cata
{-# NOINLINE sem_Children #-}
sem_Children :: Children  -> T_Children 
sem_Children list = Prelude.foldr sem_Children_Cons sem_Children_Nil (Prelude.map sem_Child list)

-- semantic domain
newtype T_Children  = T_Children {
                                 attach_T_Children :: Identity (T_Children_s2 )
                                 }
data T_Children_s2  where C_Children_s2 :: {
                                           inv_Children_s2 :: !(forall t. K_Children_s2  t -> t)
                                           } -> T_Children_s2 
data T_Children_s3  = C_Children_s3
data T_Children_s27  = C_Children_s27
newtype T_Children_s53  = C_Children_s53 {
                                         inv_Children_s53 :: (T_Children_v48 )
                                         }
data K_Children_s2 k  where
   K_Children_v1 :: K_Children_s2  (T_Children_v1 )
   K_Children_v14 :: K_Children_s2  (T_Children_v14 )
   K_Children_v47 :: K_Children_s2  (T_Children_v47 )
type T_Children_v1  = (T_Children_vIn1 ) -> (T_Children_vOut1 )
data T_Children_vIn1  = T_Children_vIn1 !(ConstructorIdent) !(Bool) !(Map Identifier Attributes) !(Set Identifier) !(NontermIdent) !([Identifier]) !(Map Identifier Attributes)
data T_Children_vOut1  = T_Children_vOut1 !(Seq Error) !([(Identifier,Type,ChildKind)]) !([(Identifier, Attributes)]) !(Children) !([(Identifier, Attributes)])
type T_Children_v14  = (T_Children_vIn14 ) -> (T_Children_vOut14 )
data T_Children_vIn14  = T_Children_vIn14 !(Map Identifier Attributes) !(Set Identifier) !(Map Identifier Attributes)
data T_Children_vOut14  = T_Children_vOut14 !(Seq Error) !([(Identifier,Type,ChildKind)]) !([(Identifier, Attributes)]) !(Children) !([(Identifier, Attributes)])
type T_Children_v47  = (T_Children_vIn47 ) -> (T_Children_vOut47 )
data T_Children_vIn47  = T_Children_vIn47 !(Map Identifier Attributes) !(Set Identifier) !(Map Identifier Attributes)
data T_Children_vOut47  = T_Children_vOut47 !(Seq Error) !([(Identifier,Type,ChildKind)]) !([(Identifier, Attributes)]) !([(Identifier, Attributes)]) !(T_Children_s53 )
type T_Children_v48  = (T_Children_vIn48 ) -> (T_Children_vOut48 )
data T_Children_vIn48  = T_Children_vIn48 
data T_Children_vOut48  = T_Children_vOut48 !(Children)
{-# NOINLINE sem_Children_Cons #-}
sem_Children_Cons :: T_Child  -> T_Children  -> T_Children 
sem_Children_Cons arg_hd_ arg_tl_ = T_Children (return st2) where
   {-# NOINLINE st2 #-}
   !st2 = let
      k2 :: K_Children_s2  t -> t
      k2 K_Children_v1 = v1
      k2 K_Children_v14 = v14
      k2 K_Children_v47 = v47
      v1 :: T_Children_v1 
      v1 = \ !(T_Children_vIn1 _lhsIcon _lhsIcr _lhsIinhMap _lhsImerged _lhsInt _lhsIparams _lhsIsynMap) -> (
         let !_hdX0 = Control.Monad.Identity.runIdentity (attach_T_Child (arg_hd_)) in
         let !_tlX2 = Control.Monad.Identity.runIdentity (attach_T_Children (arg_tl_)) in
         let !_hdOinhMap = rule21 _lhsIinhMap in
         let !_tlOinhMap = rule28 _lhsIinhMap in
         let !_hdOmerged = rule22 _lhsImerged in
         let !_hdOsynMap = rule25 _lhsIsynMap in
         let !_tlOmerged = rule29 _lhsImerged in
         let !_tlOsynMap = rule32 _lhsIsynMap in
         let !(T_Child_vOut13 _hdIerrors _hdIfield _hdIinherited _hdIname _hdIoutput _hdIsynthesized) = inv_Child_s0 _hdX0 K_Child_v13 (T_Child_vIn13 _hdOinhMap _hdOmerged _hdOsynMap) in
         let !(T_Children_vOut14 _tlIerrors _tlIfields _tlIinputs _tlIoutput _tlIoutputs) = inv_Children_s2 _tlX2 K_Children_v14 (T_Children_vIn14 _tlOinhMap _tlOmerged _tlOsynMap) in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule16 _hdIerrors _tlIerrors in
         let _lhsOfields :: [(Identifier,Type,ChildKind)]
             !_lhsOfields = rule15 _hdIfield _tlIfields in
         let _lhsOinputs :: [(Identifier, Attributes)]
             !_lhsOinputs = rule13 _hdIinherited _hdIname _tlIinputs in
         let !_output = rule17 _hdIoutput _tlIoutput in
         let _lhsOoutput :: Children
             !_lhsOoutput = rule18 _output in
         let _lhsOoutputs :: [(Identifier, Attributes)]
             !_lhsOoutputs = rule14 _hdIname _hdIsynthesized _tlIoutputs in
         let !__result_ = T_Children_vOut1 _lhsOerrors _lhsOfields _lhsOinputs _lhsOoutput _lhsOoutputs
          in __result_ )
      v14 :: T_Children_v14 
      v14 = \ !(T_Children_vIn14 _lhsIinhMap _lhsImerged _lhsIsynMap) -> (
         let !_hdX0 = Control.Monad.Identity.runIdentity (attach_T_Child (arg_hd_)) in
         let !_tlX2 = Control.Monad.Identity.runIdentity (attach_T_Children (arg_tl_)) in
         let !_hdOinhMap = rule21 _lhsIinhMap in
         let !_tlOinhMap = rule28 _lhsIinhMap in
         let !_hdOmerged = rule22 _lhsImerged in
         let !_hdOsynMap = rule25 _lhsIsynMap in
         let !_tlOmerged = rule29 _lhsImerged in
         let !_tlOsynMap = rule32 _lhsIsynMap in
         let !(T_Child_vOut13 _hdIerrors _hdIfield _hdIinherited _hdIname _hdIoutput _hdIsynthesized) = inv_Child_s0 _hdX0 K_Child_v13 (T_Child_vIn13 _hdOinhMap _hdOmerged _hdOsynMap) in
         let !(T_Children_vOut14 _tlIerrors _tlIfields _tlIinputs _tlIoutput _tlIoutputs) = inv_Children_s2 _tlX2 K_Children_v14 (T_Children_vIn14 _tlOinhMap _tlOmerged _tlOsynMap) in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule16 _hdIerrors _tlIerrors in
         let _lhsOfields :: [(Identifier,Type,ChildKind)]
             !_lhsOfields = rule15 _hdIfield _tlIfields in
         let _lhsOinputs :: [(Identifier, Attributes)]
             !_lhsOinputs = rule13 _hdIinherited _hdIname _tlIinputs in
         let !_output = rule17 _hdIoutput _tlIoutput in
         let _lhsOoutput :: Children
             !_lhsOoutput = rule18 _output in
         let _lhsOoutputs :: [(Identifier, Attributes)]
             !_lhsOoutputs = rule14 _hdIname _hdIsynthesized _tlIoutputs in
         let !__result_ = T_Children_vOut14 _lhsOerrors _lhsOfields _lhsOinputs _lhsOoutput _lhsOoutputs
          in __result_ )
      v47 :: T_Children_v47 
      v47 = \ !(T_Children_vIn47 _lhsIinhMap _lhsImerged _lhsIsynMap) -> (
         let !_hdX0 = Control.Monad.Identity.runIdentity (attach_T_Child (arg_hd_)) in
         let !_tlX2 = Control.Monad.Identity.runIdentity (attach_T_Children (arg_tl_)) in
         let !_hdOinhMap = rule21 _lhsIinhMap in
         let !_tlOinhMap = rule28 _lhsIinhMap in
         let !_hdOmerged = rule22 _lhsImerged in
         let !_hdOsynMap = rule25 _lhsIsynMap in
         let !_tlOmerged = rule29 _lhsImerged in
         let !_tlOsynMap = rule32 _lhsIsynMap in
         let !(T_Child_vOut52 _hdIerrors _hdIfield _hdIinherited _hdIname _hdIsynthesized _hdX56) = inv_Child_s0 _hdX0 K_Child_v52 (T_Child_vIn52 _hdOinhMap _hdOmerged _hdOsynMap) in
         let !(T_Children_vOut47 _tlIerrors _tlIfields _tlIinputs _tlIoutputs _tlX53) = inv_Children_s2 _tlX2 K_Children_v47 (T_Children_vIn47 _tlOinhMap _tlOmerged _tlOsynMap) in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule16 _hdIerrors _tlIerrors in
         let _lhsOfields :: [(Identifier,Type,ChildKind)]
             !_lhsOfields = rule15 _hdIfield _tlIfields in
         let _lhsOinputs :: [(Identifier, Attributes)]
             !_lhsOinputs = rule13 _hdIinherited _hdIname _tlIinputs in
         let _lhsOoutputs :: [(Identifier, Attributes)]
             !_lhsOoutputs = rule14 _hdIname _hdIsynthesized _tlIoutputs in
         let !__st_ = st53 _hdX56 _tlX53
             !__result_ = T_Children_vOut47 _lhsOerrors _lhsOfields _lhsOinputs _lhsOoutputs __st_
          in __result_ )
     in C_Children_s2 k2
   {-# NOINLINE st53 #-}
   st53 = \ !_hdX56 !_tlX53 -> let
      v48 :: T_Children_v48 
      v48 = \ !(T_Children_vIn48 ) -> (
         let !(T_Child_vOut53 _hdIoutput) = inv_Child_s56 _hdX56 (T_Child_vIn53 ) in
         let !(T_Children_vOut48 _tlIoutput) = inv_Children_s53 _tlX53 (T_Children_vIn48 ) in
         let !_output = rule17 _hdIoutput _tlIoutput in
         let _lhsOoutput :: Children
             !_lhsOoutput = rule18 _output in
         let !__result_ = T_Children_vOut48 _lhsOoutput
          in __result_ )
     in C_Children_s53 v48
   {-# NOINLINE[1] rule13 #-}
   {-# LINE 215 "./src-ag/DefaultRules.ag" #-}
   rule13 = \ ((!_hdIinherited) :: Attributes) ((!_hdIname) :: Identifier) ((!_tlIinputs) :: [(Identifier, Attributes)]) ->
                         {-# LINE 215 "./src-ag/DefaultRules.ag" #-}
                         (_hdIname, _hdIinherited) : _tlIinputs
                         {-# LINE 793 "dist/build/DefaultRules.hs"#-}
   {-# NOINLINE[1] rule14 #-}
   {-# LINE 216 "./src-ag/DefaultRules.ag" #-}
   rule14 = \ ((!_hdIname) :: Identifier) ((!_hdIsynthesized) :: Attributes) ((!_tlIoutputs) :: [(Identifier, Attributes)]) ->
                         {-# LINE 216 "./src-ag/DefaultRules.ag" #-}
                         (_hdIname, _hdIsynthesized) : _tlIoutputs
                         {-# LINE 799 "dist/build/DefaultRules.hs"#-}
   {-# NOINLINE[1] rule15 #-}
   {-# LINE 542 "./src-ag/DefaultRules.ag" #-}
   rule15 = \ ((!_hdIfield) ::  (Identifier,Type,ChildKind) ) ((!_tlIfields) :: [(Identifier,Type,ChildKind)]) ->
                        {-# LINE 542 "./src-ag/DefaultRules.ag" #-}
                        _hdIfield : _tlIfields
                        {-# LINE 805 "dist/build/DefaultRules.hs"#-}
   {-# NOINLINE[1] rule16 #-}
   rule16 = \ ((!_hdIerrors) :: Seq Error) ((!_tlIerrors) :: Seq Error) ->
     _hdIerrors Seq.>< _tlIerrors
   {-# NOINLINE[1] rule17 #-}
   rule17 = \ ((!_hdIoutput) :: Child) ((!_tlIoutput) :: Children) ->
     (:) _hdIoutput _tlIoutput
   {-# NOINLINE[1] rule18 #-}
   rule18 = \ !_output ->
     _output
   {-# NOINLINE[1] rule21 #-}
   rule21 = \ ((!_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# NOINLINE[1] rule22 #-}
   rule22 = \ ((!_lhsImerged) :: Set Identifier) ->
     _lhsImerged
   {-# NOINLINE[1] rule25 #-}
   rule25 = \ ((!_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
   {-# NOINLINE[1] rule28 #-}
   rule28 = \ ((!_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# NOINLINE[1] rule29 #-}
   rule29 = \ ((!_lhsImerged) :: Set Identifier) ->
     _lhsImerged
   {-# NOINLINE[1] rule32 #-}
   rule32 = \ ((!_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
{-# NOINLINE sem_Children_Nil #-}
sem_Children_Nil ::  T_Children 
sem_Children_Nil  = T_Children (return st2) where
   {-# NOINLINE st2 #-}
   !st2 = let
      k2 :: K_Children_s2  t -> t
      k2 K_Children_v1 = v1
      k2 K_Children_v14 = v14
      k2 K_Children_v47 = v47
      v1 :: T_Children_v1 
      v1 = \ !(T_Children_vIn1 _lhsIcon _lhsIcr _lhsIinhMap _lhsImerged _lhsInt _lhsIparams _lhsIsynMap) -> (
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule36  () in
         let _lhsOfields :: [(Identifier,Type,ChildKind)]
             !_lhsOfields = rule35  () in
         let _lhsOinputs :: [(Identifier, Attributes)]
             !_lhsOinputs = rule33  () in
         let !_output = rule37  () in
         let _lhsOoutputs :: [(Identifier, Attributes)]
             !_lhsOoutputs = rule34  () in
         let _lhsOoutput :: Children
             !_lhsOoutput = rule38 _output in
         let !__result_ = T_Children_vOut1 _lhsOerrors _lhsOfields _lhsOinputs _lhsOoutput _lhsOoutputs
          in __result_ )
      v14 :: T_Children_v14 
      v14 = \ !(T_Children_vIn14 _lhsIinhMap _lhsImerged _lhsIsynMap) -> (
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule36  () in
         let _lhsOfields :: [(Identifier,Type,ChildKind)]
             !_lhsOfields = rule35  () in
         let _lhsOinputs :: [(Identifier, Attributes)]
             !_lhsOinputs = rule33  () in
         let !_output = rule37  () in
         let _lhsOoutputs :: [(Identifier, Attributes)]
             !_lhsOoutputs = rule34  () in
         let _lhsOoutput :: Children
             !_lhsOoutput = rule38 _output in
         let !__result_ = T_Children_vOut14 _lhsOerrors _lhsOfields _lhsOinputs _lhsOoutput _lhsOoutputs
          in __result_ )
      v47 :: T_Children_v47 
      v47 = \ !(T_Children_vIn47 _lhsIinhMap _lhsImerged _lhsIsynMap) -> (
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule36  () in
         let _lhsOfields :: [(Identifier,Type,ChildKind)]
             !_lhsOfields = rule35  () in
         let _lhsOinputs :: [(Identifier, Attributes)]
             !_lhsOinputs = rule33  () in
         let _lhsOoutputs :: [(Identifier, Attributes)]
             !_lhsOoutputs = rule34  () in
         let !__st_ = st53  ()
             !__result_ = T_Children_vOut47 _lhsOerrors _lhsOfields _lhsOinputs _lhsOoutputs __st_
          in __result_ )
     in C_Children_s2 k2
   {-# NOINLINE st53 #-}
   st53 = \  (_ :: ()) -> let
      v48 :: T_Children_v48 
      v48 = \ !(T_Children_vIn48 ) -> (
         let !_output = rule37  () in
         let _lhsOoutput :: Children
             !_lhsOoutput = rule38 _output in
         let !__result_ = T_Children_vOut48 _lhsOoutput
          in __result_ )
     in C_Children_s53 v48
   {-# NOINLINE[1] rule33 #-}
   {-# LINE 217 "./src-ag/DefaultRules.ag" #-}
   rule33 = \  (_ :: ()) ->
                         {-# LINE 217 "./src-ag/DefaultRules.ag" #-}
                         []
                         {-# LINE 901 "dist/build/DefaultRules.hs"#-}
   {-# NOINLINE[1] rule34 #-}
   {-# LINE 218 "./src-ag/DefaultRules.ag" #-}
   rule34 = \  (_ :: ()) ->
                         {-# LINE 218 "./src-ag/DefaultRules.ag" #-}
                         []
                         {-# LINE 907 "dist/build/DefaultRules.hs"#-}
   {-# NOINLINE[1] rule35 #-}
   {-# LINE 543 "./src-ag/DefaultRules.ag" #-}
   rule35 = \  (_ :: ()) ->
                        {-# LINE 543 "./src-ag/DefaultRules.ag" #-}
                        []
                        {-# LINE 913 "dist/build/DefaultRules.hs"#-}
   {-# NOINLINE[1] rule36 #-}
   rule36 = \  (_ :: ()) ->
     Seq.empty
   {-# NOINLINE[1] rule37 #-}
   rule37 = \  (_ :: ()) ->
     []
   {-# NOINLINE[1] rule38 #-}
   rule38 = \ !_output ->
     _output

-- Grammar -----------------------------------------------------
-- wrapper
data Inh_Grammar  = Inh_Grammar { options_Inh_Grammar :: !(Options) }
data Syn_Grammar  = Syn_Grammar { errors_Syn_Grammar :: !(Seq Error), output_Syn_Grammar :: !(Grammar) }
{-# INLINABLE wrap_Grammar #-}
wrap_Grammar :: T_Grammar  -> Inh_Grammar  -> (Syn_Grammar )
wrap_Grammar !(T_Grammar act) !(Inh_Grammar _lhsIoptions) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_Grammar_vIn2 _lhsIoptions
        !(T_Grammar_vOut2 _lhsOerrors _lhsOoutput) <- return (inv_Grammar_s4 sem arg)
        return (Syn_Grammar _lhsOerrors _lhsOoutput)
   )

-- cata
{-# INLINE sem_Grammar #-}
sem_Grammar :: Grammar  -> T_Grammar 
sem_Grammar ( Grammar !typeSyns_ !useMap_ !derivings_ !wrappers_ nonts_ !pragmas_ !manualAttrOrderMap_ !paramMap_ !contextMap_ !quantMap_ !uniqueMap_ !augmentsMap_ !aroundsMap_ !mergeMap_ ) = sem_Grammar_Grammar typeSyns_ useMap_ derivings_ wrappers_ ( sem_Nonterminals nonts_ ) pragmas_ manualAttrOrderMap_ paramMap_ contextMap_ quantMap_ uniqueMap_ augmentsMap_ aroundsMap_ mergeMap_

-- semantic domain
newtype T_Grammar  = T_Grammar {
                               attach_T_Grammar :: Identity (T_Grammar_s4 )
                               }
newtype T_Grammar_s4  = C_Grammar_s4 {
                                     inv_Grammar_s4 :: (T_Grammar_v2 )
                                     }
data T_Grammar_s5  = C_Grammar_s5
type T_Grammar_v2  = (T_Grammar_vIn2 ) -> (T_Grammar_vOut2 )
data T_Grammar_vIn2  = T_Grammar_vIn2 !(Options)
data T_Grammar_vOut2  = T_Grammar_vOut2 !(Seq Error) !(Grammar)
{-# NOINLINE sem_Grammar_Grammar #-}
sem_Grammar_Grammar :: (TypeSyns) -> (UseMap) -> (Derivings) -> (Set NontermIdent) -> T_Nonterminals  -> (PragmaMap) -> (AttrOrderMap) -> (ParamMap) -> (ContextMap) -> (QuantMap) -> (UniqueMap) -> (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) -> (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) -> (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier, [Identifier], Expression)))) -> T_Grammar 
sem_Grammar_Grammar !arg_typeSyns_ !arg_useMap_ !arg_derivings_ !arg_wrappers_ arg_nonts_ !arg_pragmas_ !arg_manualAttrOrderMap_ !arg_paramMap_ !arg_contextMap_ !arg_quantMap_ !arg_uniqueMap_ !arg_augmentsMap_ !arg_aroundsMap_ !arg_mergeMap_ = T_Grammar (return st4) where
   {-# NOINLINE st4 #-}
   !st4 = let
      v2 :: T_Grammar_v2 
      v2 = \ !(T_Grammar_vIn2 _lhsIoptions) -> (
         let !_nontsX8 = Control.Monad.Identity.runIdentity (attach_T_Nonterminals (arg_nonts_)) in
         let !_nontsOuniq = rule47  () in
         let !_nontsOcr = rule42 _lhsIoptions in
         let !_nontsOmanualAttrOrderMap = rule48 arg_manualAttrOrderMap_ in
         let !_nontsOmergesIn = rule51 arg_mergeMap_ in
         let !_nontsOo_rename = rule41 _lhsIoptions in
         let !_nontsOoptions = rule55 _lhsIoptions in
         let !_nontsOtypeSyns = rule46 arg_typeSyns_ in
         let !_nontsOuseMap = rule45 arg_useMap_ in
         let !_nontsOwrappers = rule43 arg_wrappers_ in
         let !_nontsOaroundsIn = rule50 arg_aroundsMap_ in
         let !_nontsOaugmentsIn = rule49 arg_augmentsMap_ in
         let !(T_Nonterminals_vOut15 _nontsIinhMap' _nontsIsynMap' _nontsX28) = inv_Nonterminals_s8 _nontsX8 K_Nonterminals_v15 (T_Nonterminals_vIn15 ) in
         let !_nontsOinhMap = rule39 _nontsIinhMap' in
         let !_nontsOsynMap = rule40 _nontsIsynMap' in
         let !(T_Nonterminals_vOut16 _nontsIerrors _nontsIoutput _nontsIuniq) = inv_Nonterminals_s28 _nontsX28 K_Nonterminals_v16 (T_Nonterminals_vIn16 _nontsOaroundsIn _nontsOaugmentsIn _nontsOcr _nontsOinhMap _nontsOmanualAttrOrderMap _nontsOmergesIn _nontsOo_rename _nontsOoptions _nontsOsynMap _nontsOtypeSyns _nontsOuniq _nontsOuseMap _nontsOwrappers) in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule52 _nontsIerrors in
         let !_output = rule53 _nontsIoutput arg_aroundsMap_ arg_augmentsMap_ arg_contextMap_ arg_derivings_ arg_manualAttrOrderMap_ arg_mergeMap_ arg_paramMap_ arg_pragmas_ arg_quantMap_ arg_typeSyns_ arg_uniqueMap_ arg_useMap_ arg_wrappers_ in
         let _lhsOoutput :: Grammar
             !_lhsOoutput = rule54 _output in
         let !__result_ = T_Grammar_vOut2 _lhsOerrors _lhsOoutput
          in __result_ )
     in C_Grammar_s4 v2
   {-# INLINE rule39 #-}
   {-# LINE 15 "./src-ag/DistChildAttr.ag" #-}
   rule39 = \ ((!_nontsIinhMap') :: Map Identifier Attributes) ->
                             {-# LINE 15 "./src-ag/DistChildAttr.ag" #-}
                             _nontsIinhMap'
                             {-# LINE 990 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule40 #-}
   {-# LINE 16 "./src-ag/DistChildAttr.ag" #-}
   rule40 = \ ((!_nontsIsynMap') :: Map Identifier Attributes) ->
                             {-# LINE 16 "./src-ag/DistChildAttr.ag" #-}
                             _nontsIsynMap'
                             {-# LINE 996 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule41 #-}
   {-# LINE 58 "./src-ag/DefaultRules.ag" #-}
   rule41 = \ ((!_lhsIoptions) :: Options) ->
                                    {-# LINE 58 "./src-ag/DefaultRules.ag" #-}
                                    rename    _lhsIoptions
                                    {-# LINE 1002 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule42 #-}
   {-# LINE 59 "./src-ag/DefaultRules.ag" #-}
   rule42 = \ ((!_lhsIoptions) :: Options) ->
                                    {-# LINE 59 "./src-ag/DefaultRules.ag" #-}
                                    modcopy   _lhsIoptions
                                    {-# LINE 1008 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule43 #-}
   {-# LINE 67 "./src-ag/DefaultRules.ag" #-}
   rule43 = \ !wrappers_ ->
                     {-# LINE 67 "./src-ag/DefaultRules.ag" #-}
                     wrappers_
                     {-# LINE 1014 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule45 #-}
   {-# LINE 202 "./src-ag/DefaultRules.ag" #-}
   rule45 = \ !useMap_ ->
                               {-# LINE 202 "./src-ag/DefaultRules.ag" #-}
                               useMap_
                               {-# LINE 1020 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule46 #-}
   {-# LINE 204 "./src-ag/DefaultRules.ag" #-}
   rule46 = \ !typeSyns_ ->
                                 {-# LINE 204 "./src-ag/DefaultRules.ag" #-}
                                 typeSyns_
                                 {-# LINE 1026 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule47 #-}
   {-# LINE 595 "./src-ag/DefaultRules.ag" #-}
   rule47 = \  (_ :: ()) ->
                           {-# LINE 595 "./src-ag/DefaultRules.ag" #-}
                           1
                           {-# LINE 1032 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule48 #-}
   {-# LINE 709 "./src-ag/DefaultRules.ag" #-}
   rule48 = \ !manualAttrOrderMap_ ->
                                   {-# LINE 709 "./src-ag/DefaultRules.ag" #-}
                                   manualAttrOrderMap_
                                   {-# LINE 1038 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule49 #-}
   {-# LINE 775 "./src-ag/DefaultRules.ag" #-}
   rule49 = \ !augmentsMap_ ->
                                                    {-# LINE 775 "./src-ag/DefaultRules.ag" #-}
                                                    augmentsMap_
                                                    {-# LINE 1044 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule50 #-}
   {-# LINE 782 "./src-ag/DefaultRules.ag" #-}
   rule50 = \ !aroundsMap_ ->
                                                   {-# LINE 782 "./src-ag/DefaultRules.ag" #-}
                                                   aroundsMap_
                                                   {-# LINE 1050 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule51 #-}
   {-# LINE 790 "./src-ag/DefaultRules.ag" #-}
   rule51 = \ !mergeMap_ ->
                                                  {-# LINE 790 "./src-ag/DefaultRules.ag" #-}
                                                  mergeMap_
                                                  {-# LINE 1056 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule52 #-}
   rule52 = \ ((!_nontsIerrors) :: Seq Error) ->
     _nontsIerrors
   {-# INLINE rule53 #-}
   rule53 = \ ((!_nontsIoutput) :: Nonterminals) !aroundsMap_ !augmentsMap_ !contextMap_ !derivings_ !manualAttrOrderMap_ !mergeMap_ !paramMap_ !pragmas_ !quantMap_ !typeSyns_ !uniqueMap_ !useMap_ !wrappers_ ->
     Grammar typeSyns_ useMap_ derivings_ wrappers_ _nontsIoutput pragmas_ manualAttrOrderMap_ paramMap_ contextMap_ quantMap_ uniqueMap_ augmentsMap_ aroundsMap_ mergeMap_
   {-# INLINE rule54 #-}
   rule54 = \ !_output ->
     _output
   {-# INLINE rule55 #-}
   rule55 = \ ((!_lhsIoptions) :: Options) ->
     _lhsIoptions

-- Nonterminal -------------------------------------------------
-- wrapper
data Inh_Nonterminal  = Inh_Nonterminal { aroundsIn_Inh_Nonterminal :: !(Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))), augmentsIn_Inh_Nonterminal :: !(Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))), cr_Inh_Nonterminal :: !(Bool), inhMap_Inh_Nonterminal :: !(Map Identifier Attributes), manualAttrOrderMap_Inh_Nonterminal :: !(AttrOrderMap), mergesIn_Inh_Nonterminal :: !(Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier],Expression)))), nonterminals_Inh_Nonterminal :: !(Set NontermIdent), o_rename_Inh_Nonterminal :: !(Bool), options_Inh_Nonterminal :: !(Options), synMap_Inh_Nonterminal :: !(Map Identifier Attributes), typeSyns_Inh_Nonterminal :: !(TypeSyns), uniq_Inh_Nonterminal :: !(Int), useMap_Inh_Nonterminal :: !(UseMap), wrappers_Inh_Nonterminal :: !(Set NontermIdent) }
data Syn_Nonterminal  = Syn_Nonterminal { collect_nts_Syn_Nonterminal :: !(Set NontermIdent), errors_Syn_Nonterminal :: !(Seq Error), inhMap'_Syn_Nonterminal :: !(Map Identifier Attributes), output_Syn_Nonterminal :: !(Nonterminal), synMap'_Syn_Nonterminal :: !(Map Identifier Attributes), uniq_Syn_Nonterminal :: !(Int) }
{-# INLINABLE wrap_Nonterminal #-}
wrap_Nonterminal :: T_Nonterminal  -> Inh_Nonterminal  -> (Syn_Nonterminal )
wrap_Nonterminal !(T_Nonterminal act) !(Inh_Nonterminal _lhsIaroundsIn _lhsIaugmentsIn _lhsIcr _lhsIinhMap _lhsImanualAttrOrderMap _lhsImergesIn _lhsInonterminals _lhsIo_rename _lhsIoptions _lhsIsynMap _lhsItypeSyns _lhsIuniq _lhsIuseMap _lhsIwrappers) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_Nonterminal_vIn3 _lhsIaroundsIn _lhsIaugmentsIn _lhsIcr _lhsIinhMap _lhsImanualAttrOrderMap _lhsImergesIn _lhsInonterminals _lhsIo_rename _lhsIoptions _lhsIsynMap _lhsItypeSyns _lhsIuniq _lhsIuseMap _lhsIwrappers
        !(T_Nonterminal_vOut3 _lhsOcollect_nts _lhsOerrors _lhsOinhMap' _lhsOoutput _lhsOsynMap' _lhsOuniq) <- return (inv_Nonterminal_s6 sem K_Nonterminal_v3 arg)
        return (Syn_Nonterminal _lhsOcollect_nts _lhsOerrors _lhsOinhMap' _lhsOoutput _lhsOsynMap' _lhsOuniq)
   )

-- cata
{-# INLINE sem_Nonterminal #-}
sem_Nonterminal :: Nonterminal  -> T_Nonterminal 
sem_Nonterminal ( Nonterminal !nt_ !params_ !inh_ !syn_ prods_ ) = sem_Nonterminal_Nonterminal nt_ params_ inh_ syn_ ( sem_Productions prods_ )

-- semantic domain
newtype T_Nonterminal  = T_Nonterminal {
                                       attach_T_Nonterminal :: Identity (T_Nonterminal_s6 )
                                       }
data T_Nonterminal_s6  where C_Nonterminal_s6 :: {
                                                 inv_Nonterminal_s6 :: !(forall t. K_Nonterminal_s6  t -> t)
                                                 } -> T_Nonterminal_s6 
data T_Nonterminal_s7  = C_Nonterminal_s7
data T_Nonterminal_s31  = C_Nonterminal_s31
data T_Nonterminal_s43  where C_Nonterminal_s43 :: {
                                                   inv_Nonterminal_s43 :: !(forall t. K_Nonterminal_s43  t -> t)
                                                   } -> T_Nonterminal_s43 
data T_Nonterminal_s44  = C_Nonterminal_s44
newtype T_Nonterminal_s46  = C_Nonterminal_s46 {
                                               inv_Nonterminal_s46 :: (T_Nonterminal_v37 )
                                               }
newtype T_Nonterminal_s52  = C_Nonterminal_s52 {
                                               inv_Nonterminal_s52 :: (T_Nonterminal_v46 )
                                               }
data K_Nonterminal_s6 k  where
   K_Nonterminal_v3 :: K_Nonterminal_s6  (T_Nonterminal_v3 )
   K_Nonterminal_v18 :: K_Nonterminal_s6  (T_Nonterminal_v18 )
   K_Nonterminal_v32 :: K_Nonterminal_s6  (T_Nonterminal_v32 )
   K_Nonterminal_v36 :: K_Nonterminal_s6  (T_Nonterminal_v36 )
data K_Nonterminal_s43 k  where
   K_Nonterminal_v33 :: K_Nonterminal_s43  (T_Nonterminal_v33 )
   K_Nonterminal_v45 :: K_Nonterminal_s43  (T_Nonterminal_v45 )
type T_Nonterminal_v3  = (T_Nonterminal_vIn3 ) -> (T_Nonterminal_vOut3 )
data T_Nonterminal_vIn3  = T_Nonterminal_vIn3 !(Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) !(Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) !(Bool) !(Map Identifier Attributes) !(AttrOrderMap) !(Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier],Expression)))) !(Set NontermIdent) !(Bool) !(Options) !(Map Identifier Attributes) !(TypeSyns) !(Int) !(UseMap) !(Set NontermIdent)
data T_Nonterminal_vOut3  = T_Nonterminal_vOut3 !(Set NontermIdent) !(Seq Error) !(Map Identifier Attributes) !(Nonterminal) !(Map Identifier Attributes) !(Int)
type T_Nonterminal_v18  = (T_Nonterminal_vIn18 ) -> (T_Nonterminal_vOut18 )
data T_Nonterminal_vIn18  = T_Nonterminal_vIn18 !(Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) !(Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) !(Bool) !(Map Identifier Attributes) !(AttrOrderMap) !(Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier],Expression)))) !(Bool) !(Options) !(Map Identifier Attributes) !(TypeSyns) !(Int) !(UseMap) !(Set NontermIdent)
data T_Nonterminal_vOut18  = T_Nonterminal_vOut18 !(Set NontermIdent) !(Seq Error) !(Map Identifier Attributes) !(Nonterminal) !(Map Identifier Attributes) !(Int)
type T_Nonterminal_v32  = (T_Nonterminal_vIn32 ) -> (T_Nonterminal_vOut32 )
data T_Nonterminal_vIn32  = T_Nonterminal_vIn32 
data T_Nonterminal_vOut32  = T_Nonterminal_vOut32 !(Map Identifier Attributes) !(Map Identifier Attributes) !(T_Nonterminal_s43 )
type T_Nonterminal_v33  = (T_Nonterminal_vIn33 ) -> (T_Nonterminal_vOut33 )
data T_Nonterminal_vIn33  = T_Nonterminal_vIn33 !(Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) !(Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) !(Bool) !(Map Identifier Attributes) !(AttrOrderMap) !(Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier],Expression)))) !(Bool) !(Options) !(Map Identifier Attributes) !(TypeSyns) !(Int) !(UseMap) !(Set NontermIdent)
data T_Nonterminal_vOut33  = T_Nonterminal_vOut33 !(Seq Error) !(Nonterminal) !(Int)
type T_Nonterminal_v36  = (T_Nonterminal_vIn36 ) -> (T_Nonterminal_vOut36 )
data T_Nonterminal_vIn36  = T_Nonterminal_vIn36 !(Bool) !(Map Identifier Attributes) !(AttrOrderMap) !(Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier],Expression)))) !(Bool) !(Options) !(Map Identifier Attributes) !(TypeSyns) !(UseMap) !(Set NontermIdent)
data T_Nonterminal_vOut36  = T_Nonterminal_vOut36 !(Set NontermIdent) !(Seq Error) !(Map Identifier Attributes) !(Map Identifier Attributes) !(T_Nonterminal_s46 )
type T_Nonterminal_v37  = (T_Nonterminal_vIn37 ) -> (T_Nonterminal_vOut37 )
data T_Nonterminal_vIn37  = T_Nonterminal_vIn37 !(Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) !(Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) !(Int)
data T_Nonterminal_vOut37  = T_Nonterminal_vOut37 !(Nonterminal) !(Int)
type T_Nonterminal_v45  = (T_Nonterminal_vIn45 ) -> (T_Nonterminal_vOut45 )
data T_Nonterminal_vIn45  = T_Nonterminal_vIn45 !(Bool) !(Map Identifier Attributes) !(AttrOrderMap) !(Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier],Expression)))) !(Bool) !(Options) !(Map Identifier Attributes) !(TypeSyns) !(UseMap) !(Set NontermIdent)
data T_Nonterminal_vOut45  = T_Nonterminal_vOut45 !(Seq Error) !(T_Nonterminal_s52 )
type T_Nonterminal_v46  = (T_Nonterminal_vIn46 ) -> (T_Nonterminal_vOut46 )
data T_Nonterminal_vIn46  = T_Nonterminal_vIn46 !(Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) !(Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) !(Int)
data T_Nonterminal_vOut46  = T_Nonterminal_vOut46 !(Nonterminal) !(Int)
{-# NOINLINE sem_Nonterminal_Nonterminal #-}
sem_Nonterminal_Nonterminal :: (NontermIdent) -> ([Identifier]) -> (Attributes) -> (Attributes) -> T_Productions  -> T_Nonterminal 
sem_Nonterminal_Nonterminal !arg_nt_ !arg_params_ !arg_inh_ !arg_syn_ arg_prods_ = T_Nonterminal (return st6) where
   {-# NOINLINE st6 #-}
   !st6 = let
      k6 :: K_Nonterminal_s6  t -> t
      k6 K_Nonterminal_v3 = v3
      k6 K_Nonterminal_v18 = v18
      k6 K_Nonterminal_v32 = v32
      k6 K_Nonterminal_v36 = v36
      v3 :: T_Nonterminal_v3 
      v3 = \ !(T_Nonterminal_vIn3 _lhsIaroundsIn _lhsIaugmentsIn _lhsIcr _lhsIinhMap _lhsImanualAttrOrderMap _lhsImergesIn _lhsInonterminals _lhsIo_rename _lhsIoptions _lhsIsynMap _lhsItypeSyns _lhsIuniq _lhsIuseMap _lhsIwrappers) -> (
         let !_prodsX16 = Control.Monad.Identity.runIdentity (attach_T_Productions (arg_prods_)) in
         let _lhsOcollect_nts :: Set NontermIdent
             !_lhsOcollect_nts = rule59 arg_nt_ in
         let !_prodsOcr = rule77 _lhsIcr in
         let !_inh1 = rule66 arg_inh_ arg_nt_ arg_params_ in
         let !_prodsOinh = rule60 _inh1 in
         let !_prodsOinhMap = rule78 _lhsIinhMap in
         let !_prodsOmanualAttrOrderMap = rule79 _lhsImanualAttrOrderMap in
         let !_mergesIn = rule71 _lhsImergesIn arg_nt_ in
         let !_prodsOmergesIn = rule80 _mergesIn in
         let !_prodsOnt = rule65 arg_nt_ in
         let !_prodsOo_rename = rule82 _lhsIo_rename in
         let !_prodsOoptions = rule83 _lhsIoptions in
         let !_syn1 = rule67 arg_nt_ arg_params_ arg_syn_ in
         let !_prodsOsyn = rule61 _syn1 in
         let !_prodsOsynMap = rule84 _lhsIsynMap in
         let !_prodsOsynOrig = rule63 arg_syn_ in
         let !_prodsOtypeSyns = rule85 _lhsItypeSyns in
         let !_prodsOuseMap = rule64 _lhsIuseMap arg_nt_ in
         let !_prodsOwrappers = rule87 _lhsIwrappers in
         let _lhsOinhMap' :: Map Identifier Attributes
             !_lhsOinhMap' = rule56 arg_inh_ arg_nt_ in
         let !_aroundsIn = rule70 _lhsIaroundsIn arg_nt_ in
         let !_prodsOaroundsIn = rule75 _aroundsIn in
         let !_augmentsIn = rule69 _lhsIaugmentsIn arg_nt_ in
         let !_prodsOaugmentsIn = rule76 _augmentsIn in
         let !_prodsOparams = rule58 arg_params_ in
         let !_prodsOuniq = rule86 _lhsIuniq in
         let _lhsOsynMap' :: Map Identifier Attributes
             !_lhsOsynMap' = rule57 arg_nt_ arg_syn_ in
         let !(T_Productions_vOut17 _prodsIerrors _prodsIoutput _prodsIuniq) = inv_Productions_s16 _prodsX16 K_Productions_v17 (T_Productions_vIn17 _prodsOaroundsIn _prodsOaugmentsIn _prodsOcr _prodsOinh _prodsOinhMap _prodsOmanualAttrOrderMap _prodsOmergesIn _prodsOnt _prodsOo_rename _prodsOoptions _prodsOparams _prodsOsyn _prodsOsynMap _prodsOsynOrig _prodsOtypeSyns _prodsOuniq _prodsOuseMap _prodsOwrappers) in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule72 _prodsIerrors in
         let _lhsOoutput :: Nonterminal
             !_lhsOoutput = rule68 _inh1 _prodsIoutput _syn1 arg_nt_ arg_params_ in
         let _lhsOuniq :: Int
             !_lhsOuniq = rule74 _prodsIuniq in
         let !__result_ = T_Nonterminal_vOut3 _lhsOcollect_nts _lhsOerrors _lhsOinhMap' _lhsOoutput _lhsOsynMap' _lhsOuniq
          in __result_ )
      v18 :: T_Nonterminal_v18 
      v18 = \ !(T_Nonterminal_vIn18 _lhsIaroundsIn _lhsIaugmentsIn _lhsIcr _lhsIinhMap _lhsImanualAttrOrderMap _lhsImergesIn _lhsIo_rename _lhsIoptions _lhsIsynMap _lhsItypeSyns _lhsIuniq _lhsIuseMap _lhsIwrappers) -> (
         let !_prodsX16 = Control.Monad.Identity.runIdentity (attach_T_Productions (arg_prods_)) in
         let _lhsOcollect_nts :: Set NontermIdent
             !_lhsOcollect_nts = rule59 arg_nt_ in
         let !_prodsOcr = rule77 _lhsIcr in
         let !_inh1 = rule66 arg_inh_ arg_nt_ arg_params_ in
         let !_prodsOinh = rule60 _inh1 in
         let !_prodsOinhMap = rule78 _lhsIinhMap in
         let !_prodsOmanualAttrOrderMap = rule79 _lhsImanualAttrOrderMap in
         let !_mergesIn = rule71 _lhsImergesIn arg_nt_ in
         let !_prodsOmergesIn = rule80 _mergesIn in
         let !_prodsOnt = rule65 arg_nt_ in
         let !_prodsOo_rename = rule82 _lhsIo_rename in
         let !_prodsOoptions = rule83 _lhsIoptions in
         let !_syn1 = rule67 arg_nt_ arg_params_ arg_syn_ in
         let !_prodsOsyn = rule61 _syn1 in
         let !_prodsOsynMap = rule84 _lhsIsynMap in
         let !_prodsOsynOrig = rule63 arg_syn_ in
         let !_prodsOtypeSyns = rule85 _lhsItypeSyns in
         let !_prodsOuseMap = rule64 _lhsIuseMap arg_nt_ in
         let !_prodsOwrappers = rule87 _lhsIwrappers in
         let _lhsOinhMap' :: Map Identifier Attributes
             !_lhsOinhMap' = rule56 arg_inh_ arg_nt_ in
         let !_aroundsIn = rule70 _lhsIaroundsIn arg_nt_ in
         let !_prodsOaroundsIn = rule75 _aroundsIn in
         let !_augmentsIn = rule69 _lhsIaugmentsIn arg_nt_ in
         let !_prodsOaugmentsIn = rule76 _augmentsIn in
         let !_prodsOparams = rule58 arg_params_ in
         let !_prodsOuniq = rule86 _lhsIuniq in
         let _lhsOsynMap' :: Map Identifier Attributes
             !_lhsOsynMap' = rule57 arg_nt_ arg_syn_ in
         let !(T_Productions_vOut17 _prodsIerrors _prodsIoutput _prodsIuniq) = inv_Productions_s16 _prodsX16 K_Productions_v17 (T_Productions_vIn17 _prodsOaroundsIn _prodsOaugmentsIn _prodsOcr _prodsOinh _prodsOinhMap _prodsOmanualAttrOrderMap _prodsOmergesIn _prodsOnt _prodsOo_rename _prodsOoptions _prodsOparams _prodsOsyn _prodsOsynMap _prodsOsynOrig _prodsOtypeSyns _prodsOuniq _prodsOuseMap _prodsOwrappers) in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule72 _prodsIerrors in
         let _lhsOoutput :: Nonterminal
             !_lhsOoutput = rule68 _inh1 _prodsIoutput _syn1 arg_nt_ arg_params_ in
         let _lhsOuniq :: Int
             !_lhsOuniq = rule74 _prodsIuniq in
         let !__result_ = T_Nonterminal_vOut18 _lhsOcollect_nts _lhsOerrors _lhsOinhMap' _lhsOoutput _lhsOsynMap' _lhsOuniq
          in __result_ )
      v32 :: T_Nonterminal_v32 
      v32 = \ !(T_Nonterminal_vIn32 ) -> (
         let _lhsOinhMap' :: Map Identifier Attributes
             !_lhsOinhMap' = rule56 arg_inh_ arg_nt_ in
         let _lhsOsynMap' :: Map Identifier Attributes
             !_lhsOsynMap' = rule57 arg_nt_ arg_syn_ in
         let !__st_ = st43  ()
             !__result_ = T_Nonterminal_vOut32 _lhsOinhMap' _lhsOsynMap' __st_
          in __result_ )
      v36 :: T_Nonterminal_v36 
      v36 = \ !(T_Nonterminal_vIn36 _lhsIcr _lhsIinhMap _lhsImanualAttrOrderMap _lhsImergesIn _lhsIo_rename _lhsIoptions _lhsIsynMap _lhsItypeSyns _lhsIuseMap _lhsIwrappers) -> (
         let !_prodsX16 = Control.Monad.Identity.runIdentity (attach_T_Productions (arg_prods_)) in
         let _lhsOcollect_nts :: Set NontermIdent
             !_lhsOcollect_nts = rule59 arg_nt_ in
         let !_prodsOcr = rule77 _lhsIcr in
         let !_inh1 = rule66 arg_inh_ arg_nt_ arg_params_ in
         let !_prodsOinh = rule60 _inh1 in
         let !_prodsOinhMap = rule78 _lhsIinhMap in
         let !_prodsOmanualAttrOrderMap = rule79 _lhsImanualAttrOrderMap in
         let !_mergesIn = rule71 _lhsImergesIn arg_nt_ in
         let !_prodsOmergesIn = rule80 _mergesIn in
         let !_prodsOnt = rule65 arg_nt_ in
         let !_prodsOo_rename = rule82 _lhsIo_rename in
         let !_prodsOoptions = rule83 _lhsIoptions in
         let !_syn1 = rule67 arg_nt_ arg_params_ arg_syn_ in
         let !_prodsOsyn = rule61 _syn1 in
         let !_prodsOsynMap = rule84 _lhsIsynMap in
         let !_prodsOsynOrig = rule63 arg_syn_ in
         let !_prodsOtypeSyns = rule85 _lhsItypeSyns in
         let !_prodsOuseMap = rule64 _lhsIuseMap arg_nt_ in
         let !_prodsOwrappers = rule87 _lhsIwrappers in
         let _lhsOinhMap' :: Map Identifier Attributes
             !_lhsOinhMap' = rule56 arg_inh_ arg_nt_ in
         let _lhsOsynMap' :: Map Identifier Attributes
             !_lhsOsynMap' = rule57 arg_nt_ arg_syn_ in
         let !(T_Productions_vOut26 _prodsIerrors _prodsX39) = inv_Productions_s16 _prodsX16 K_Productions_v26 (T_Productions_vIn26 _prodsOcr _prodsOinh _prodsOinhMap _prodsOmanualAttrOrderMap _prodsOmergesIn _prodsOnt _prodsOo_rename _prodsOoptions _prodsOsyn _prodsOsynMap _prodsOsynOrig _prodsOtypeSyns _prodsOuseMap _prodsOwrappers) in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule72 _prodsIerrors in
         let !__st_ = st46 _inh1 _prodsX39 _syn1
             !__result_ = T_Nonterminal_vOut36 _lhsOcollect_nts _lhsOerrors _lhsOinhMap' _lhsOsynMap' __st_
          in __result_ )
     in C_Nonterminal_s6 k6
   {-# NOINLINE st43 #-}
   st43 = \  (_ :: ()) -> let
      k43 :: K_Nonterminal_s43  t -> t
      k43 K_Nonterminal_v33 = v33
      k43 K_Nonterminal_v45 = v45
      v33 :: T_Nonterminal_v33 
      v33 = \ !(T_Nonterminal_vIn33 _lhsIaroundsIn _lhsIaugmentsIn _lhsIcr _lhsIinhMap _lhsImanualAttrOrderMap _lhsImergesIn _lhsIo_rename _lhsIoptions _lhsIsynMap _lhsItypeSyns _lhsIuniq _lhsIuseMap _lhsIwrappers) -> (
         let !_prodsOnt = rule65 arg_nt_ in
         let !_prodsOsynOrig = rule63 arg_syn_ in
         let !_prodsX16 = Control.Monad.Identity.runIdentity (attach_T_Productions (arg_prods_)) in
         let !_prodsOcr = rule77 _lhsIcr in
         let !_inh1 = rule66 arg_inh_ arg_nt_ arg_params_ in
         let !_prodsOinh = rule60 _inh1 in
         let !_prodsOinhMap = rule78 _lhsIinhMap in
         let !_prodsOmanualAttrOrderMap = rule79 _lhsImanualAttrOrderMap in
         let !_mergesIn = rule71 _lhsImergesIn arg_nt_ in
         let !_prodsOmergesIn = rule80 _mergesIn in
         let !_prodsOo_rename = rule82 _lhsIo_rename in
         let !_prodsOoptions = rule83 _lhsIoptions in
         let !_syn1 = rule67 arg_nt_ arg_params_ arg_syn_ in
         let !_prodsOsyn = rule61 _syn1 in
         let !_prodsOsynMap = rule84 _lhsIsynMap in
         let !_prodsOtypeSyns = rule85 _lhsItypeSyns in
         let !_prodsOuseMap = rule64 _lhsIuseMap arg_nt_ in
         let !_prodsOwrappers = rule87 _lhsIwrappers in
         let !_aroundsIn = rule70 _lhsIaroundsIn arg_nt_ in
         let !_prodsOaroundsIn = rule75 _aroundsIn in
         let !_augmentsIn = rule69 _lhsIaugmentsIn arg_nt_ in
         let !_prodsOaugmentsIn = rule76 _augmentsIn in
         let !_prodsOparams = rule58 arg_params_ in
         let !_prodsOuniq = rule86 _lhsIuniq in
         let !(T_Productions_vOut17 _prodsIerrors _prodsIoutput _prodsIuniq) = inv_Productions_s16 _prodsX16 K_Productions_v17 (T_Productions_vIn17 _prodsOaroundsIn _prodsOaugmentsIn _prodsOcr _prodsOinh _prodsOinhMap _prodsOmanualAttrOrderMap _prodsOmergesIn _prodsOnt _prodsOo_rename _prodsOoptions _prodsOparams _prodsOsyn _prodsOsynMap _prodsOsynOrig _prodsOtypeSyns _prodsOuniq _prodsOuseMap _prodsOwrappers) in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule72 _prodsIerrors in
         let _lhsOoutput :: Nonterminal
             !_lhsOoutput = rule68 _inh1 _prodsIoutput _syn1 arg_nt_ arg_params_ in
         let _lhsOuniq :: Int
             !_lhsOuniq = rule74 _prodsIuniq in
         let !__result_ = T_Nonterminal_vOut33 _lhsOerrors _lhsOoutput _lhsOuniq
          in __result_ )
      v45 :: T_Nonterminal_v45 
      v45 = \ !(T_Nonterminal_vIn45 _lhsIcr _lhsIinhMap _lhsImanualAttrOrderMap _lhsImergesIn _lhsIo_rename _lhsIoptions _lhsIsynMap _lhsItypeSyns _lhsIuseMap _lhsIwrappers) -> (
         let !_prodsOnt = rule65 arg_nt_ in
         let !_prodsOsynOrig = rule63 arg_syn_ in
         let !_prodsX16 = Control.Monad.Identity.runIdentity (attach_T_Productions (arg_prods_)) in
         let !_prodsOcr = rule77 _lhsIcr in
         let !_inh1 = rule66 arg_inh_ arg_nt_ arg_params_ in
         let !_prodsOinh = rule60 _inh1 in
         let !_prodsOinhMap = rule78 _lhsIinhMap in
         let !_prodsOmanualAttrOrderMap = rule79 _lhsImanualAttrOrderMap in
         let !_mergesIn = rule71 _lhsImergesIn arg_nt_ in
         let !_prodsOmergesIn = rule80 _mergesIn in
         let !_prodsOo_rename = rule82 _lhsIo_rename in
         let !_prodsOoptions = rule83 _lhsIoptions in
         let !_syn1 = rule67 arg_nt_ arg_params_ arg_syn_ in
         let !_prodsOsyn = rule61 _syn1 in
         let !_prodsOsynMap = rule84 _lhsIsynMap in
         let !_prodsOtypeSyns = rule85 _lhsItypeSyns in
         let !_prodsOuseMap = rule64 _lhsIuseMap arg_nt_ in
         let !_prodsOwrappers = rule87 _lhsIwrappers in
         let !(T_Productions_vOut26 _prodsIerrors _prodsX39) = inv_Productions_s16 _prodsX16 K_Productions_v26 (T_Productions_vIn26 _prodsOcr _prodsOinh _prodsOinhMap _prodsOmanualAttrOrderMap _prodsOmergesIn _prodsOnt _prodsOo_rename _prodsOoptions _prodsOsyn _prodsOsynMap _prodsOsynOrig _prodsOtypeSyns _prodsOuseMap _prodsOwrappers) in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule72 _prodsIerrors in
         let !__st_ = st52 _inh1 _prodsX39 _syn1
             !__result_ = T_Nonterminal_vOut45 _lhsOerrors __st_
          in __result_ )
     in C_Nonterminal_s43 k43
   {-# NOINLINE st46 #-}
   st46 = \ !_inh1 !_prodsX39 !_syn1 -> let
      v37 :: T_Nonterminal_v37 
      v37 = \ !(T_Nonterminal_vIn37 _lhsIaroundsIn _lhsIaugmentsIn _lhsIuniq) -> (
         let !_prodsOparams = rule58 arg_params_ in
         let !_aroundsIn = rule70 _lhsIaroundsIn arg_nt_ in
         let !_prodsOaroundsIn = rule75 _aroundsIn in
         let !_augmentsIn = rule69 _lhsIaugmentsIn arg_nt_ in
         let !_prodsOaugmentsIn = rule76 _augmentsIn in
         let !_prodsOuniq = rule86 _lhsIuniq in
         let !(T_Productions_vOut27 _prodsIoutput _prodsIuniq) = inv_Productions_s39 _prodsX39 (T_Productions_vIn27 _prodsOaroundsIn _prodsOaugmentsIn _prodsOparams _prodsOuniq) in
         let _lhsOoutput :: Nonterminal
             !_lhsOoutput = rule68 _inh1 _prodsIoutput _syn1 arg_nt_ arg_params_ in
         let _lhsOuniq :: Int
             !_lhsOuniq = rule74 _prodsIuniq in
         let !__result_ = T_Nonterminal_vOut37 _lhsOoutput _lhsOuniq
          in __result_ )
     in C_Nonterminal_s46 v37
   {-# NOINLINE st52 #-}
   st52 = \ !_inh1 !_prodsX39 !_syn1 -> let
      v46 :: T_Nonterminal_v46 
      v46 = \ !(T_Nonterminal_vIn46 _lhsIaroundsIn _lhsIaugmentsIn _lhsIuniq) -> (
         let !_prodsOparams = rule58 arg_params_ in
         let !_aroundsIn = rule70 _lhsIaroundsIn arg_nt_ in
         let !_prodsOaroundsIn = rule75 _aroundsIn in
         let !_augmentsIn = rule69 _lhsIaugmentsIn arg_nt_ in
         let !_prodsOaugmentsIn = rule76 _augmentsIn in
         let !_prodsOuniq = rule86 _lhsIuniq in
         let !(T_Productions_vOut27 _prodsIoutput _prodsIuniq) = inv_Productions_s39 _prodsX39 (T_Productions_vIn27 _prodsOaroundsIn _prodsOaugmentsIn _prodsOparams _prodsOuniq) in
         let _lhsOoutput :: Nonterminal
             !_lhsOoutput = rule68 _inh1 _prodsIoutput _syn1 arg_nt_ arg_params_ in
         let _lhsOuniq :: Int
             !_lhsOuniq = rule74 _prodsIuniq in
         let !__result_ = T_Nonterminal_vOut46 _lhsOoutput _lhsOuniq
          in __result_ )
     in C_Nonterminal_s52 v46
   {-# NOINLINE rule56 #-}
   {-# LINE 7 "./src-ag/DistChildAttr.ag" #-}
   rule56 = \ !inh_ !nt_ ->
                                 {-# LINE 7 "./src-ag/DistChildAttr.ag" #-}
                                 Map.singleton nt_ inh_
                                 {-# LINE 1382 "dist/build/DefaultRules.hs"#-}
   {-# NOINLINE rule57 #-}
   {-# LINE 8 "./src-ag/DistChildAttr.ag" #-}
   rule57 = \ !nt_ !syn_ ->
                                 {-# LINE 8 "./src-ag/DistChildAttr.ag" #-}
                                 Map.singleton nt_ syn_
                                 {-# LINE 1388 "dist/build/DefaultRules.hs"#-}
   {-# NOINLINE rule58 #-}
   {-# LINE 44 "./src-ag/DefaultRules.ag" #-}
   rule58 = \ !params_ ->
                   {-# LINE 44 "./src-ag/DefaultRules.ag" #-}
                   params_
                   {-# LINE 1394 "dist/build/DefaultRules.hs"#-}
   {-# NOINLINE[1] rule59 #-}
   {-# LINE 176 "./src-ag/DefaultRules.ag" #-}
   rule59 = \ !nt_ ->
                                    {-# LINE 176 "./src-ag/DefaultRules.ag" #-}
                                    Set.singleton nt_
                                    {-# LINE 1400 "dist/build/DefaultRules.hs"#-}
   {-# NOINLINE rule60 #-}
   {-# LINE 190 "./src-ag/DefaultRules.ag" #-}
   rule60 = \ !_inh1 ->
                                   {-# LINE 190 "./src-ag/DefaultRules.ag" #-}
                                   _inh1
                                   {-# LINE 1406 "dist/build/DefaultRules.hs"#-}
   {-# NOINLINE rule61 #-}
   {-# LINE 191 "./src-ag/DefaultRules.ag" #-}
   rule61 = \ !_syn1 ->
                                   {-# LINE 191 "./src-ag/DefaultRules.ag" #-}
                                   _syn1
                                   {-# LINE 1412 "dist/build/DefaultRules.hs"#-}
   {-# NOINLINE rule63 #-}
   {-# LINE 193 "./src-ag/DefaultRules.ag" #-}
   rule63 = \ !syn_ ->
                                   {-# LINE 193 "./src-ag/DefaultRules.ag" #-}
                                   syn_
                                   {-# LINE 1418 "dist/build/DefaultRules.hs"#-}
   {-# NOINLINE rule64 #-}
   {-# LINE 194 "./src-ag/DefaultRules.ag" #-}
   rule64 = \ ((!_lhsIuseMap) :: UseMap) !nt_ ->
                                   {-# LINE 194 "./src-ag/DefaultRules.ag" #-}
                                   Map.findWithDefault Map.empty nt_ _lhsIuseMap
                                   {-# LINE 1424 "dist/build/DefaultRules.hs"#-}
   {-# NOINLINE rule65 #-}
   {-# LINE 206 "./src-ag/DefaultRules.ag" #-}
   rule65 = \ !nt_ ->
                               {-# LINE 206 "./src-ag/DefaultRules.ag" #-}
                               nt_
                               {-# LINE 1430 "dist/build/DefaultRules.hs"#-}
   {-# NOINLINE rule66 #-}
   {-# LINE 564 "./src-ag/DefaultRules.ag" #-}
   rule66 = \ !inh_ !nt_ !params_ ->
               {-# LINE 564 "./src-ag/DefaultRules.ag" #-}
               Map.map (elimSelfId nt_ params_) inh_
               {-# LINE 1436 "dist/build/DefaultRules.hs"#-}
   {-# NOINLINE rule67 #-}
   {-# LINE 565 "./src-ag/DefaultRules.ag" #-}
   rule67 = \ !nt_ !params_ !syn_ ->
               {-# LINE 565 "./src-ag/DefaultRules.ag" #-}
               Map.map (elimSelfId nt_ params_) syn_
               {-# LINE 1442 "dist/build/DefaultRules.hs"#-}
   {-# NOINLINE rule68 #-}
   {-# LINE 604 "./src-ag/DefaultRules.ag" #-}
   rule68 = \ !_inh1 ((!_prodsIoutput) :: Productions) !_syn1 !nt_ !params_ ->
                 {-# LINE 604 "./src-ag/DefaultRules.ag" #-}
                 Nonterminal nt_ params_ _inh1     _syn1     _prodsIoutput
                 {-# LINE 1448 "dist/build/DefaultRules.hs"#-}
   {-# NOINLINE rule69 #-}
   {-# LINE 776 "./src-ag/DefaultRules.ag" #-}
   rule69 = \ ((!_lhsIaugmentsIn) :: Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) !nt_ ->
                                                  {-# LINE 776 "./src-ag/DefaultRules.ag" #-}
                                                  Map.findWithDefault Map.empty nt_ _lhsIaugmentsIn
                                                  {-# LINE 1454 "dist/build/DefaultRules.hs"#-}
   {-# NOINLINE rule70 #-}
   {-# LINE 783 "./src-ag/DefaultRules.ag" #-}
   rule70 = \ ((!_lhsIaroundsIn) :: Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) !nt_ ->
                                                   {-# LINE 783 "./src-ag/DefaultRules.ag" #-}
                                                   Map.findWithDefault Map.empty nt_ _lhsIaroundsIn
                                                   {-# LINE 1460 "dist/build/DefaultRules.hs"#-}
   {-# NOINLINE rule71 #-}
   {-# LINE 791 "./src-ag/DefaultRules.ag" #-}
   rule71 = \ ((!_lhsImergesIn) :: Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier],Expression)))) !nt_ ->
                                                  {-# LINE 791 "./src-ag/DefaultRules.ag" #-}
                                                  Map.findWithDefault Map.empty nt_ _lhsImergesIn
                                                  {-# LINE 1466 "dist/build/DefaultRules.hs"#-}
   {-# NOINLINE[1] rule72 #-}
   rule72 = \ ((!_prodsIerrors) :: Seq Error) ->
     _prodsIerrors
   {-# NOINLINE[1] rule74 #-}
   rule74 = \ ((!_prodsIuniq) :: Int) ->
     _prodsIuniq
   {-# NOINLINE[1] rule75 #-}
   rule75 = \ !_aroundsIn ->
     _aroundsIn
   {-# NOINLINE[1] rule76 #-}
   rule76 = \ !_augmentsIn ->
     _augmentsIn
   {-# NOINLINE[1] rule77 #-}
   rule77 = \ ((!_lhsIcr) :: Bool) ->
     _lhsIcr
   {-# NOINLINE[1] rule78 #-}
   rule78 = \ ((!_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# NOINLINE[1] rule79 #-}
   rule79 = \ ((!_lhsImanualAttrOrderMap) :: AttrOrderMap) ->
     _lhsImanualAttrOrderMap
   {-# NOINLINE[1] rule80 #-}
   rule80 = \ !_mergesIn ->
     _mergesIn
   {-# NOINLINE[1] rule82 #-}
   rule82 = \ ((!_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# NOINLINE[1] rule83 #-}
   rule83 = \ ((!_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# NOINLINE[1] rule84 #-}
   rule84 = \ ((!_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
   {-# NOINLINE[1] rule85 #-}
   rule85 = \ ((!_lhsItypeSyns) :: TypeSyns) ->
     _lhsItypeSyns
   {-# NOINLINE[1] rule86 #-}
   rule86 = \ ((!_lhsIuniq) :: Int) ->
     _lhsIuniq
   {-# NOINLINE[1] rule87 #-}
   rule87 = \ ((!_lhsIwrappers) :: Set NontermIdent) ->
     _lhsIwrappers

-- Nonterminals ------------------------------------------------
-- wrapper
data Inh_Nonterminals  = Inh_Nonterminals { aroundsIn_Inh_Nonterminals :: !(Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))), augmentsIn_Inh_Nonterminals :: !(Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))), cr_Inh_Nonterminals :: !(Bool), inhMap_Inh_Nonterminals :: !(Map Identifier Attributes), manualAttrOrderMap_Inh_Nonterminals :: !(AttrOrderMap), mergesIn_Inh_Nonterminals :: !(Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier],Expression)))), nonterminals_Inh_Nonterminals :: !(Set NontermIdent), o_rename_Inh_Nonterminals :: !(Bool), options_Inh_Nonterminals :: !(Options), synMap_Inh_Nonterminals :: !(Map Identifier Attributes), typeSyns_Inh_Nonterminals :: !(TypeSyns), uniq_Inh_Nonterminals :: !(Int), useMap_Inh_Nonterminals :: !(UseMap), wrappers_Inh_Nonterminals :: !(Set NontermIdent) }
data Syn_Nonterminals  = Syn_Nonterminals { collect_nts_Syn_Nonterminals :: !(Set NontermIdent), errors_Syn_Nonterminals :: !(Seq Error), inhMap'_Syn_Nonterminals :: !(Map Identifier Attributes), output_Syn_Nonterminals :: !(Nonterminals), synMap'_Syn_Nonterminals :: !(Map Identifier Attributes), uniq_Syn_Nonterminals :: !(Int) }
{-# INLINABLE wrap_Nonterminals #-}
wrap_Nonterminals :: T_Nonterminals  -> Inh_Nonterminals  -> (Syn_Nonterminals )
wrap_Nonterminals !(T_Nonterminals act) !(Inh_Nonterminals _lhsIaroundsIn _lhsIaugmentsIn _lhsIcr _lhsIinhMap _lhsImanualAttrOrderMap _lhsImergesIn _lhsInonterminals _lhsIo_rename _lhsIoptions _lhsIsynMap _lhsItypeSyns _lhsIuniq _lhsIuseMap _lhsIwrappers) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_Nonterminals_vIn4 _lhsIaroundsIn _lhsIaugmentsIn _lhsIcr _lhsIinhMap _lhsImanualAttrOrderMap _lhsImergesIn _lhsInonterminals _lhsIo_rename _lhsIoptions _lhsIsynMap _lhsItypeSyns _lhsIuniq _lhsIuseMap _lhsIwrappers
        !(T_Nonterminals_vOut4 _lhsOcollect_nts _lhsOerrors _lhsOinhMap' _lhsOoutput _lhsOsynMap' _lhsOuniq) <- return (inv_Nonterminals_s8 sem K_Nonterminals_v4 arg)
        return (Syn_Nonterminals _lhsOcollect_nts _lhsOerrors _lhsOinhMap' _lhsOoutput _lhsOsynMap' _lhsOuniq)
   )

-- cata
{-# NOINLINE sem_Nonterminals #-}
sem_Nonterminals :: Nonterminals  -> T_Nonterminals 
sem_Nonterminals list = Prelude.foldr sem_Nonterminals_Cons sem_Nonterminals_Nil (Prelude.map sem_Nonterminal list)

-- semantic domain
newtype T_Nonterminals  = T_Nonterminals {
                                         attach_T_Nonterminals :: Identity (T_Nonterminals_s8 )
                                         }
data T_Nonterminals_s8  where C_Nonterminals_s8 :: {
                                                   inv_Nonterminals_s8 :: !(forall t. K_Nonterminals_s8  t -> t)
                                                   } -> T_Nonterminals_s8 
data T_Nonterminals_s9  = C_Nonterminals_s9
data T_Nonterminals_s28  where C_Nonterminals_s28 :: {
                                                     inv_Nonterminals_s28 :: !(forall t. K_Nonterminals_s28  t -> t)
                                                     } -> T_Nonterminals_s28 
data T_Nonterminals_s29  = C_Nonterminals_s29
newtype T_Nonterminals_s32  = C_Nonterminals_s32 {
                                                 inv_Nonterminals_s32 :: (T_Nonterminals_v20 )
                                                 }
data T_Nonterminals_s33  = C_Nonterminals_s33
newtype T_Nonterminals_s45  = C_Nonterminals_s45 {
                                                 inv_Nonterminals_s45 :: (T_Nonterminals_v35 )
                                                 }
data K_Nonterminals_s8 k  where
   K_Nonterminals_v4 :: K_Nonterminals_s8  (T_Nonterminals_v4 )
   K_Nonterminals_v15 :: K_Nonterminals_s8  (T_Nonterminals_v15 )
   K_Nonterminals_v19 :: K_Nonterminals_s8  (T_Nonterminals_v19 )
data K_Nonterminals_s28 k  where
   K_Nonterminals_v16 :: K_Nonterminals_s28  (T_Nonterminals_v16 )
   K_Nonterminals_v34 :: K_Nonterminals_s28  (T_Nonterminals_v34 )
type T_Nonterminals_v4  = (T_Nonterminals_vIn4 ) -> (T_Nonterminals_vOut4 )
data T_Nonterminals_vIn4  = T_Nonterminals_vIn4 !(Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) !(Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) !(Bool) !(Map Identifier Attributes) !(AttrOrderMap) !(Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier],Expression)))) !(Set NontermIdent) !(Bool) !(Options) !(Map Identifier Attributes) !(TypeSyns) !(Int) !(UseMap) !(Set NontermIdent)
data T_Nonterminals_vOut4  = T_Nonterminals_vOut4 !(Set NontermIdent) !(Seq Error) !(Map Identifier Attributes) !(Nonterminals) !(Map Identifier Attributes) !(Int)
type T_Nonterminals_v15  = (T_Nonterminals_vIn15 ) -> (T_Nonterminals_vOut15 )
data T_Nonterminals_vIn15  = T_Nonterminals_vIn15 
data T_Nonterminals_vOut15  = T_Nonterminals_vOut15 !(Map Identifier Attributes) !(Map Identifier Attributes) !(T_Nonterminals_s28 )
type T_Nonterminals_v16  = (T_Nonterminals_vIn16 ) -> (T_Nonterminals_vOut16 )
data T_Nonterminals_vIn16  = T_Nonterminals_vIn16 !(Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) !(Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) !(Bool) !(Map Identifier Attributes) !(AttrOrderMap) !(Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier],Expression)))) !(Bool) !(Options) !(Map Identifier Attributes) !(TypeSyns) !(Int) !(UseMap) !(Set NontermIdent)
data T_Nonterminals_vOut16  = T_Nonterminals_vOut16 !(Seq Error) !(Nonterminals) !(Int)
type T_Nonterminals_v19  = (T_Nonterminals_vIn19 ) -> (T_Nonterminals_vOut19 )
data T_Nonterminals_vIn19  = T_Nonterminals_vIn19 !(Bool) !(Map Identifier Attributes) !(AttrOrderMap) !(Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier],Expression)))) !(Bool) !(Options) !(Map Identifier Attributes) !(TypeSyns) !(UseMap) !(Set NontermIdent)
data T_Nonterminals_vOut19  = T_Nonterminals_vOut19 !(Set NontermIdent) !(Seq Error) !(Map Identifier Attributes) !(Map Identifier Attributes) !(T_Nonterminals_s32 )
type T_Nonterminals_v20  = (T_Nonterminals_vIn20 ) -> (T_Nonterminals_vOut20 )
data T_Nonterminals_vIn20  = T_Nonterminals_vIn20 !(Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) !(Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) !(Int)
data T_Nonterminals_vOut20  = T_Nonterminals_vOut20 !(Nonterminals) !(Int)
type T_Nonterminals_v34  = (T_Nonterminals_vIn34 ) -> (T_Nonterminals_vOut34 )
data T_Nonterminals_vIn34  = T_Nonterminals_vIn34 !(Bool) !(Map Identifier Attributes) !(AttrOrderMap) !(Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier],Expression)))) !(Bool) !(Options) !(Map Identifier Attributes) !(TypeSyns) !(UseMap) !(Set NontermIdent)
data T_Nonterminals_vOut34  = T_Nonterminals_vOut34 !(Seq Error) !(T_Nonterminals_s45 )
type T_Nonterminals_v35  = (T_Nonterminals_vIn35 ) -> (T_Nonterminals_vOut35 )
data T_Nonterminals_vIn35  = T_Nonterminals_vIn35 !(Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) !(Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) !(Int)
data T_Nonterminals_vOut35  = T_Nonterminals_vOut35 !(Nonterminals) !(Int)
{-# NOINLINE sem_Nonterminals_Cons #-}
sem_Nonterminals_Cons :: T_Nonterminal  -> T_Nonterminals  -> T_Nonterminals 
sem_Nonterminals_Cons arg_hd_ arg_tl_ = T_Nonterminals (return st8) where
   {-# NOINLINE st8 #-}
   !st8 = let
      k8 :: K_Nonterminals_s8  t -> t
      k8 K_Nonterminals_v4 = v4
      k8 K_Nonterminals_v15 = v15
      k8 K_Nonterminals_v19 = v19
      v4 :: T_Nonterminals_v4 
      v4 = \ !(T_Nonterminals_vIn4 _lhsIaroundsIn _lhsIaugmentsIn _lhsIcr _lhsIinhMap _lhsImanualAttrOrderMap _lhsImergesIn _lhsInonterminals _lhsIo_rename _lhsIoptions _lhsIsynMap _lhsItypeSyns _lhsIuniq _lhsIuseMap _lhsIwrappers) -> (
         let !_hdX6 = Control.Monad.Identity.runIdentity (attach_T_Nonterminal (arg_hd_)) in
         let !_tlX8 = Control.Monad.Identity.runIdentity (attach_T_Nonterminals (arg_tl_)) in
         let !_hdOcr = rule97 _lhsIcr in
         let !_hdOinhMap = rule98 _lhsIinhMap in
         let !_hdOmanualAttrOrderMap = rule99 _lhsImanualAttrOrderMap in
         let !_hdOmergesIn = rule100 _lhsImergesIn in
         let !_hdOo_rename = rule102 _lhsIo_rename in
         let !_hdOoptions = rule103 _lhsIoptions in
         let !_hdOsynMap = rule104 _lhsIsynMap in
         let !_hdOtypeSyns = rule105 _lhsItypeSyns in
         let !_hdOuseMap = rule107 _lhsIuseMap in
         let !_hdOwrappers = rule108 _lhsIwrappers in
         let !_tlOcr = rule111 _lhsIcr in
         let !_tlOinhMap = rule112 _lhsIinhMap in
         let !_tlOmanualAttrOrderMap = rule113 _lhsImanualAttrOrderMap in
         let !_tlOmergesIn = rule114 _lhsImergesIn in
         let !_tlOo_rename = rule116 _lhsIo_rename in
         let !_tlOoptions = rule117 _lhsIoptions in
         let !_tlOsynMap = rule118 _lhsIsynMap in
         let !_tlOtypeSyns = rule119 _lhsItypeSyns in
         let !_tlOuseMap = rule121 _lhsIuseMap in
         let !_tlOwrappers = rule122 _lhsIwrappers in
         let !_hdOaroundsIn = rule95 _lhsIaroundsIn in
         let !_hdOaugmentsIn = rule96 _lhsIaugmentsIn in
         let !_hdOuniq = rule106 _lhsIuniq in
         let !_tlOaroundsIn = rule109 _lhsIaroundsIn in
         let !_tlOaugmentsIn = rule110 _lhsIaugmentsIn in
         let !(T_Nonterminal_vOut18 _hdIcollect_nts _hdIerrors _hdIinhMap' _hdIoutput _hdIsynMap' _hdIuniq) = inv_Nonterminal_s6 _hdX6 K_Nonterminal_v18 (T_Nonterminal_vIn18 _hdOaroundsIn _hdOaugmentsIn _hdOcr _hdOinhMap _hdOmanualAttrOrderMap _hdOmergesIn _hdOo_rename _hdOoptions _hdOsynMap _hdOtypeSyns _hdOuniq _hdOuseMap _hdOwrappers) in
         let !(T_Nonterminals_vOut19 _tlIcollect_nts _tlIerrors _tlIinhMap' _tlIsynMap' _tlX32) = inv_Nonterminals_s8 _tlX8 K_Nonterminals_v19 (T_Nonterminals_vIn19 _tlOcr _tlOinhMap _tlOmanualAttrOrderMap _tlOmergesIn _tlOo_rename _tlOoptions _tlOsynMap _tlOtypeSyns _tlOuseMap _tlOwrappers) in
         let _lhsOcollect_nts :: Set NontermIdent
             !_lhsOcollect_nts = rule88 _hdIcollect_nts _tlIcollect_nts in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule89 _hdIerrors _tlIerrors in
         let _lhsOinhMap' :: Map Identifier Attributes
             !_lhsOinhMap' = rule90 _hdIinhMap' _tlIinhMap' in
         let !_tlOuniq = rule120 _hdIuniq in
         let _lhsOsynMap' :: Map Identifier Attributes
             !_lhsOsynMap' = rule91 _hdIsynMap' _tlIsynMap' in
         let !(T_Nonterminals_vOut20 _tlIoutput _tlIuniq) = inv_Nonterminals_s32 _tlX32 (T_Nonterminals_vIn20 _tlOaroundsIn _tlOaugmentsIn _tlOuniq) in
         let !_output = rule92 _hdIoutput _tlIoutput in
         let _lhsOoutput :: Nonterminals
             !_lhsOoutput = rule93 _output in
         let _lhsOuniq :: Int
             !_lhsOuniq = rule94 _tlIuniq in
         let !__result_ = T_Nonterminals_vOut4 _lhsOcollect_nts _lhsOerrors _lhsOinhMap' _lhsOoutput _lhsOsynMap' _lhsOuniq
          in __result_ )
      v15 :: T_Nonterminals_v15 
      v15 = \ !(T_Nonterminals_vIn15 ) -> (
         let !_hdX6 = Control.Monad.Identity.runIdentity (attach_T_Nonterminal (arg_hd_)) in
         let !_tlX8 = Control.Monad.Identity.runIdentity (attach_T_Nonterminals (arg_tl_)) in
         let !(T_Nonterminal_vOut32 _hdIinhMap' _hdIsynMap' _hdX43) = inv_Nonterminal_s6 _hdX6 K_Nonterminal_v32 (T_Nonterminal_vIn32 ) in
         let !(T_Nonterminals_vOut15 _tlIinhMap' _tlIsynMap' _tlX28) = inv_Nonterminals_s8 _tlX8 K_Nonterminals_v15 (T_Nonterminals_vIn15 ) in
         let _lhsOinhMap' :: Map Identifier Attributes
             !_lhsOinhMap' = rule90 _hdIinhMap' _tlIinhMap' in
         let _lhsOsynMap' :: Map Identifier Attributes
             !_lhsOsynMap' = rule91 _hdIsynMap' _tlIsynMap' in
         let !__st_ = st28 _hdX43 _tlX28
             !__result_ = T_Nonterminals_vOut15 _lhsOinhMap' _lhsOsynMap' __st_
          in __result_ )
      v19 :: T_Nonterminals_v19 
      v19 = \ !(T_Nonterminals_vIn19 _lhsIcr _lhsIinhMap _lhsImanualAttrOrderMap _lhsImergesIn _lhsIo_rename _lhsIoptions _lhsIsynMap _lhsItypeSyns _lhsIuseMap _lhsIwrappers) -> (
         let !_hdX6 = Control.Monad.Identity.runIdentity (attach_T_Nonterminal (arg_hd_)) in
         let !_tlX8 = Control.Monad.Identity.runIdentity (attach_T_Nonterminals (arg_tl_)) in
         let !_hdOcr = rule97 _lhsIcr in
         let !_hdOinhMap = rule98 _lhsIinhMap in
         let !_hdOmanualAttrOrderMap = rule99 _lhsImanualAttrOrderMap in
         let !_hdOmergesIn = rule100 _lhsImergesIn in
         let !_hdOo_rename = rule102 _lhsIo_rename in
         let !_hdOoptions = rule103 _lhsIoptions in
         let !_hdOsynMap = rule104 _lhsIsynMap in
         let !_hdOtypeSyns = rule105 _lhsItypeSyns in
         let !_hdOuseMap = rule107 _lhsIuseMap in
         let !_hdOwrappers = rule108 _lhsIwrappers in
         let !_tlOcr = rule111 _lhsIcr in
         let !_tlOinhMap = rule112 _lhsIinhMap in
         let !_tlOmanualAttrOrderMap = rule113 _lhsImanualAttrOrderMap in
         let !_tlOmergesIn = rule114 _lhsImergesIn in
         let !_tlOo_rename = rule116 _lhsIo_rename in
         let !_tlOoptions = rule117 _lhsIoptions in
         let !_tlOsynMap = rule118 _lhsIsynMap in
         let !_tlOtypeSyns = rule119 _lhsItypeSyns in
         let !_tlOuseMap = rule121 _lhsIuseMap in
         let !_tlOwrappers = rule122 _lhsIwrappers in
         let !(T_Nonterminal_vOut36 _hdIcollect_nts _hdIerrors _hdIinhMap' _hdIsynMap' _hdX46) = inv_Nonterminal_s6 _hdX6 K_Nonterminal_v36 (T_Nonterminal_vIn36 _hdOcr _hdOinhMap _hdOmanualAttrOrderMap _hdOmergesIn _hdOo_rename _hdOoptions _hdOsynMap _hdOtypeSyns _hdOuseMap _hdOwrappers) in
         let !(T_Nonterminals_vOut19 _tlIcollect_nts _tlIerrors _tlIinhMap' _tlIsynMap' _tlX32) = inv_Nonterminals_s8 _tlX8 K_Nonterminals_v19 (T_Nonterminals_vIn19 _tlOcr _tlOinhMap _tlOmanualAttrOrderMap _tlOmergesIn _tlOo_rename _tlOoptions _tlOsynMap _tlOtypeSyns _tlOuseMap _tlOwrappers) in
         let _lhsOcollect_nts :: Set NontermIdent
             !_lhsOcollect_nts = rule88 _hdIcollect_nts _tlIcollect_nts in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule89 _hdIerrors _tlIerrors in
         let _lhsOinhMap' :: Map Identifier Attributes
             !_lhsOinhMap' = rule90 _hdIinhMap' _tlIinhMap' in
         let _lhsOsynMap' :: Map Identifier Attributes
             !_lhsOsynMap' = rule91 _hdIsynMap' _tlIsynMap' in
         let !__st_ = st32 _hdX46 _tlX32
             !__result_ = T_Nonterminals_vOut19 _lhsOcollect_nts _lhsOerrors _lhsOinhMap' _lhsOsynMap' __st_
          in __result_ )
     in C_Nonterminals_s8 k8
   {-# NOINLINE st28 #-}
   st28 = \ !_hdX43 !_tlX28 -> let
      k28 :: K_Nonterminals_s28  t -> t
      k28 K_Nonterminals_v16 = v16
      k28 K_Nonterminals_v34 = v34
      v16 :: T_Nonterminals_v16 
      v16 = \ !(T_Nonterminals_vIn16 _lhsIaroundsIn _lhsIaugmentsIn _lhsIcr _lhsIinhMap _lhsImanualAttrOrderMap _lhsImergesIn _lhsIo_rename _lhsIoptions _lhsIsynMap _lhsItypeSyns _lhsIuniq _lhsIuseMap _lhsIwrappers) -> (
         let !_hdOcr = rule97 _lhsIcr in
         let !_hdOinhMap = rule98 _lhsIinhMap in
         let !_hdOmanualAttrOrderMap = rule99 _lhsImanualAttrOrderMap in
         let !_hdOmergesIn = rule100 _lhsImergesIn in
         let !_hdOo_rename = rule102 _lhsIo_rename in
         let !_hdOoptions = rule103 _lhsIoptions in
         let !_hdOsynMap = rule104 _lhsIsynMap in
         let !_hdOtypeSyns = rule105 _lhsItypeSyns in
         let !_hdOuseMap = rule107 _lhsIuseMap in
         let !_hdOwrappers = rule108 _lhsIwrappers in
         let !_tlOcr = rule111 _lhsIcr in
         let !_tlOinhMap = rule112 _lhsIinhMap in
         let !_tlOmanualAttrOrderMap = rule113 _lhsImanualAttrOrderMap in
         let !_tlOmergesIn = rule114 _lhsImergesIn in
         let !_tlOo_rename = rule116 _lhsIo_rename in
         let !_tlOoptions = rule117 _lhsIoptions in
         let !_tlOsynMap = rule118 _lhsIsynMap in
         let !_tlOtypeSyns = rule119 _lhsItypeSyns in
         let !_tlOuseMap = rule121 _lhsIuseMap in
         let !_tlOwrappers = rule122 _lhsIwrappers in
         let !_hdOaroundsIn = rule95 _lhsIaroundsIn in
         let !_hdOaugmentsIn = rule96 _lhsIaugmentsIn in
         let !_hdOuniq = rule106 _lhsIuniq in
         let !_tlOaroundsIn = rule109 _lhsIaroundsIn in
         let !_tlOaugmentsIn = rule110 _lhsIaugmentsIn in
         let !(T_Nonterminal_vOut33 _hdIerrors _hdIoutput _hdIuniq) = inv_Nonterminal_s43 _hdX43 K_Nonterminal_v33 (T_Nonterminal_vIn33 _hdOaroundsIn _hdOaugmentsIn _hdOcr _hdOinhMap _hdOmanualAttrOrderMap _hdOmergesIn _hdOo_rename _hdOoptions _hdOsynMap _hdOtypeSyns _hdOuniq _hdOuseMap _hdOwrappers) in
         let !(T_Nonterminals_vOut34 _tlIerrors _tlX45) = inv_Nonterminals_s28 _tlX28 K_Nonterminals_v34 (T_Nonterminals_vIn34 _tlOcr _tlOinhMap _tlOmanualAttrOrderMap _tlOmergesIn _tlOo_rename _tlOoptions _tlOsynMap _tlOtypeSyns _tlOuseMap _tlOwrappers) in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule89 _hdIerrors _tlIerrors in
         let !_tlOuniq = rule120 _hdIuniq in
         let !(T_Nonterminals_vOut35 _tlIoutput _tlIuniq) = inv_Nonterminals_s45 _tlX45 (T_Nonterminals_vIn35 _tlOaroundsIn _tlOaugmentsIn _tlOuniq) in
         let !_output = rule92 _hdIoutput _tlIoutput in
         let _lhsOoutput :: Nonterminals
             !_lhsOoutput = rule93 _output in
         let _lhsOuniq :: Int
             !_lhsOuniq = rule94 _tlIuniq in
         let !__result_ = T_Nonterminals_vOut16 _lhsOerrors _lhsOoutput _lhsOuniq
          in __result_ )
      v34 :: T_Nonterminals_v34 
      v34 = \ !(T_Nonterminals_vIn34 _lhsIcr _lhsIinhMap _lhsImanualAttrOrderMap _lhsImergesIn _lhsIo_rename _lhsIoptions _lhsIsynMap _lhsItypeSyns _lhsIuseMap _lhsIwrappers) -> (
         let !_hdOcr = rule97 _lhsIcr in
         let !_hdOinhMap = rule98 _lhsIinhMap in
         let !_hdOmanualAttrOrderMap = rule99 _lhsImanualAttrOrderMap in
         let !_hdOmergesIn = rule100 _lhsImergesIn in
         let !_hdOo_rename = rule102 _lhsIo_rename in
         let !_hdOoptions = rule103 _lhsIoptions in
         let !_hdOsynMap = rule104 _lhsIsynMap in
         let !_hdOtypeSyns = rule105 _lhsItypeSyns in
         let !_hdOuseMap = rule107 _lhsIuseMap in
         let !_hdOwrappers = rule108 _lhsIwrappers in
         let !_tlOcr = rule111 _lhsIcr in
         let !_tlOinhMap = rule112 _lhsIinhMap in
         let !_tlOmanualAttrOrderMap = rule113 _lhsImanualAttrOrderMap in
         let !_tlOmergesIn = rule114 _lhsImergesIn in
         let !_tlOo_rename = rule116 _lhsIo_rename in
         let !_tlOoptions = rule117 _lhsIoptions in
         let !_tlOsynMap = rule118 _lhsIsynMap in
         let !_tlOtypeSyns = rule119 _lhsItypeSyns in
         let !_tlOuseMap = rule121 _lhsIuseMap in
         let !_tlOwrappers = rule122 _lhsIwrappers in
         let !(T_Nonterminal_vOut45 _hdIerrors _hdX52) = inv_Nonterminal_s43 _hdX43 K_Nonterminal_v45 (T_Nonterminal_vIn45 _hdOcr _hdOinhMap _hdOmanualAttrOrderMap _hdOmergesIn _hdOo_rename _hdOoptions _hdOsynMap _hdOtypeSyns _hdOuseMap _hdOwrappers) in
         let !(T_Nonterminals_vOut34 _tlIerrors _tlX45) = inv_Nonterminals_s28 _tlX28 K_Nonterminals_v34 (T_Nonterminals_vIn34 _tlOcr _tlOinhMap _tlOmanualAttrOrderMap _tlOmergesIn _tlOo_rename _tlOoptions _tlOsynMap _tlOtypeSyns _tlOuseMap _tlOwrappers) in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule89 _hdIerrors _tlIerrors in
         let !__st_ = st45 _hdX52 _tlX45
             !__result_ = T_Nonterminals_vOut34 _lhsOerrors __st_
          in __result_ )
     in C_Nonterminals_s28 k28
   {-# NOINLINE st32 #-}
   st32 = \ !_hdX46 !_tlX32 -> let
      v20 :: T_Nonterminals_v20 
      v20 = \ !(T_Nonterminals_vIn20 _lhsIaroundsIn _lhsIaugmentsIn _lhsIuniq) -> (
         let !_hdOaroundsIn = rule95 _lhsIaroundsIn in
         let !_hdOaugmentsIn = rule96 _lhsIaugmentsIn in
         let !_hdOuniq = rule106 _lhsIuniq in
         let !_tlOaroundsIn = rule109 _lhsIaroundsIn in
         let !_tlOaugmentsIn = rule110 _lhsIaugmentsIn in
         let !(T_Nonterminal_vOut37 _hdIoutput _hdIuniq) = inv_Nonterminal_s46 _hdX46 (T_Nonterminal_vIn37 _hdOaroundsIn _hdOaugmentsIn _hdOuniq) in
         let !_tlOuniq = rule120 _hdIuniq in
         let !(T_Nonterminals_vOut20 _tlIoutput _tlIuniq) = inv_Nonterminals_s32 _tlX32 (T_Nonterminals_vIn20 _tlOaroundsIn _tlOaugmentsIn _tlOuniq) in
         let !_output = rule92 _hdIoutput _tlIoutput in
         let _lhsOoutput :: Nonterminals
             !_lhsOoutput = rule93 _output in
         let _lhsOuniq :: Int
             !_lhsOuniq = rule94 _tlIuniq in
         let !__result_ = T_Nonterminals_vOut20 _lhsOoutput _lhsOuniq
          in __result_ )
     in C_Nonterminals_s32 v20
   {-# NOINLINE st45 #-}
   st45 = \ !_hdX52 !_tlX45 -> let
      v35 :: T_Nonterminals_v35 
      v35 = \ !(T_Nonterminals_vIn35 _lhsIaroundsIn _lhsIaugmentsIn _lhsIuniq) -> (
         let !_hdOaroundsIn = rule95 _lhsIaroundsIn in
         let !_hdOaugmentsIn = rule96 _lhsIaugmentsIn in
         let !_hdOuniq = rule106 _lhsIuniq in
         let !_tlOaroundsIn = rule109 _lhsIaroundsIn in
         let !_tlOaugmentsIn = rule110 _lhsIaugmentsIn in
         let !(T_Nonterminal_vOut46 _hdIoutput _hdIuniq) = inv_Nonterminal_s52 _hdX52 (T_Nonterminal_vIn46 _hdOaroundsIn _hdOaugmentsIn _hdOuniq) in
         let !_tlOuniq = rule120 _hdIuniq in
         let !(T_Nonterminals_vOut35 _tlIoutput _tlIuniq) = inv_Nonterminals_s45 _tlX45 (T_Nonterminals_vIn35 _tlOaroundsIn _tlOaugmentsIn _tlOuniq) in
         let !_output = rule92 _hdIoutput _tlIoutput in
         let _lhsOoutput :: Nonterminals
             !_lhsOoutput = rule93 _output in
         let _lhsOuniq :: Int
             !_lhsOuniq = rule94 _tlIuniq in
         let !__result_ = T_Nonterminals_vOut35 _lhsOoutput _lhsOuniq
          in __result_ )
     in C_Nonterminals_s45 v35
   {-# NOINLINE[1] rule88 #-}
   rule88 = \ ((!_hdIcollect_nts) :: Set NontermIdent) ((!_tlIcollect_nts) :: Set NontermIdent) ->
     _hdIcollect_nts `Set.union` _tlIcollect_nts
   {-# NOINLINE[1] rule89 #-}
   rule89 = \ ((!_hdIerrors) :: Seq Error) ((!_tlIerrors) :: Seq Error) ->
     _hdIerrors Seq.>< _tlIerrors
   {-# NOINLINE[1] rule90 #-}
   rule90 = \ ((!_hdIinhMap') :: Map Identifier Attributes) ((!_tlIinhMap') :: Map Identifier Attributes) ->
     _hdIinhMap' `Map.union` _tlIinhMap'
   {-# NOINLINE[1] rule91 #-}
   rule91 = \ ((!_hdIsynMap') :: Map Identifier Attributes) ((!_tlIsynMap') :: Map Identifier Attributes) ->
     _hdIsynMap' `Map.union` _tlIsynMap'
   {-# NOINLINE[1] rule92 #-}
   rule92 = \ ((!_hdIoutput) :: Nonterminal) ((!_tlIoutput) :: Nonterminals) ->
     (:) _hdIoutput _tlIoutput
   {-# NOINLINE[1] rule93 #-}
   rule93 = \ !_output ->
     _output
   {-# NOINLINE[1] rule94 #-}
   rule94 = \ ((!_tlIuniq) :: Int) ->
     _tlIuniq
   {-# NOINLINE[1] rule95 #-}
   rule95 = \ ((!_lhsIaroundsIn) :: Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) ->
     _lhsIaroundsIn
   {-# NOINLINE[1] rule96 #-}
   rule96 = \ ((!_lhsIaugmentsIn) :: Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) ->
     _lhsIaugmentsIn
   {-# NOINLINE[1] rule97 #-}
   rule97 = \ ((!_lhsIcr) :: Bool) ->
     _lhsIcr
   {-# NOINLINE[1] rule98 #-}
   rule98 = \ ((!_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# NOINLINE[1] rule99 #-}
   rule99 = \ ((!_lhsImanualAttrOrderMap) :: AttrOrderMap) ->
     _lhsImanualAttrOrderMap
   {-# NOINLINE[1] rule100 #-}
   rule100 = \ ((!_lhsImergesIn) :: Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier],Expression)))) ->
     _lhsImergesIn
   {-# NOINLINE[1] rule102 #-}
   rule102 = \ ((!_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# NOINLINE[1] rule103 #-}
   rule103 = \ ((!_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# NOINLINE[1] rule104 #-}
   rule104 = \ ((!_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
   {-# NOINLINE[1] rule105 #-}
   rule105 = \ ((!_lhsItypeSyns) :: TypeSyns) ->
     _lhsItypeSyns
   {-# NOINLINE[1] rule106 #-}
   rule106 = \ ((!_lhsIuniq) :: Int) ->
     _lhsIuniq
   {-# NOINLINE[1] rule107 #-}
   rule107 = \ ((!_lhsIuseMap) :: UseMap) ->
     _lhsIuseMap
   {-# NOINLINE[1] rule108 #-}
   rule108 = \ ((!_lhsIwrappers) :: Set NontermIdent) ->
     _lhsIwrappers
   {-# NOINLINE[1] rule109 #-}
   rule109 = \ ((!_lhsIaroundsIn) :: Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) ->
     _lhsIaroundsIn
   {-# NOINLINE[1] rule110 #-}
   rule110 = \ ((!_lhsIaugmentsIn) :: Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) ->
     _lhsIaugmentsIn
   {-# NOINLINE[1] rule111 #-}
   rule111 = \ ((!_lhsIcr) :: Bool) ->
     _lhsIcr
   {-# NOINLINE[1] rule112 #-}
   rule112 = \ ((!_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# NOINLINE[1] rule113 #-}
   rule113 = \ ((!_lhsImanualAttrOrderMap) :: AttrOrderMap) ->
     _lhsImanualAttrOrderMap
   {-# NOINLINE[1] rule114 #-}
   rule114 = \ ((!_lhsImergesIn) :: Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier],Expression)))) ->
     _lhsImergesIn
   {-# NOINLINE[1] rule116 #-}
   rule116 = \ ((!_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# NOINLINE[1] rule117 #-}
   rule117 = \ ((!_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# NOINLINE[1] rule118 #-}
   rule118 = \ ((!_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
   {-# NOINLINE[1] rule119 #-}
   rule119 = \ ((!_lhsItypeSyns) :: TypeSyns) ->
     _lhsItypeSyns
   {-# NOINLINE[1] rule120 #-}
   rule120 = \ ((!_hdIuniq) :: Int) ->
     _hdIuniq
   {-# NOINLINE[1] rule121 #-}
   rule121 = \ ((!_lhsIuseMap) :: UseMap) ->
     _lhsIuseMap
   {-# NOINLINE[1] rule122 #-}
   rule122 = \ ((!_lhsIwrappers) :: Set NontermIdent) ->
     _lhsIwrappers
{-# NOINLINE sem_Nonterminals_Nil #-}
sem_Nonterminals_Nil ::  T_Nonterminals 
sem_Nonterminals_Nil  = T_Nonterminals (return st8) where
   {-# NOINLINE st8 #-}
   !st8 = let
      k8 :: K_Nonterminals_s8  t -> t
      k8 K_Nonterminals_v4 = v4
      k8 K_Nonterminals_v15 = v15
      k8 K_Nonterminals_v19 = v19
      v4 :: T_Nonterminals_v4 
      v4 = \ !(T_Nonterminals_vIn4 _lhsIaroundsIn _lhsIaugmentsIn _lhsIcr _lhsIinhMap _lhsImanualAttrOrderMap _lhsImergesIn _lhsInonterminals _lhsIo_rename _lhsIoptions _lhsIsynMap _lhsItypeSyns _lhsIuniq _lhsIuseMap _lhsIwrappers) -> (
         let _lhsOcollect_nts :: Set NontermIdent
             !_lhsOcollect_nts = rule123  () in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule124  () in
         let _lhsOinhMap' :: Map Identifier Attributes
             !_lhsOinhMap' = rule125  () in
         let !_output = rule127  () in
         let _lhsOsynMap' :: Map Identifier Attributes
             !_lhsOsynMap' = rule126  () in
         let _lhsOoutput :: Nonterminals
             !_lhsOoutput = rule128 _output in
         let _lhsOuniq :: Int
             !_lhsOuniq = rule129 _lhsIuniq in
         let !__result_ = T_Nonterminals_vOut4 _lhsOcollect_nts _lhsOerrors _lhsOinhMap' _lhsOoutput _lhsOsynMap' _lhsOuniq
          in __result_ )
      v15 :: T_Nonterminals_v15 
      v15 = \ !(T_Nonterminals_vIn15 ) -> (
         let _lhsOinhMap' :: Map Identifier Attributes
             !_lhsOinhMap' = rule125  () in
         let _lhsOsynMap' :: Map Identifier Attributes
             !_lhsOsynMap' = rule126  () in
         let !__st_ = st28  ()
             !__result_ = T_Nonterminals_vOut15 _lhsOinhMap' _lhsOsynMap' __st_
          in __result_ )
      v19 :: T_Nonterminals_v19 
      v19 = \ !(T_Nonterminals_vIn19 _lhsIcr _lhsIinhMap _lhsImanualAttrOrderMap _lhsImergesIn _lhsIo_rename _lhsIoptions _lhsIsynMap _lhsItypeSyns _lhsIuseMap _lhsIwrappers) -> (
         let _lhsOcollect_nts :: Set NontermIdent
             !_lhsOcollect_nts = rule123  () in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule124  () in
         let _lhsOinhMap' :: Map Identifier Attributes
             !_lhsOinhMap' = rule125  () in
         let _lhsOsynMap' :: Map Identifier Attributes
             !_lhsOsynMap' = rule126  () in
         let !__st_ = st32  ()
             !__result_ = T_Nonterminals_vOut19 _lhsOcollect_nts _lhsOerrors _lhsOinhMap' _lhsOsynMap' __st_
          in __result_ )
     in C_Nonterminals_s8 k8
   {-# NOINLINE st28 #-}
   st28 = \  (_ :: ()) -> let
      k28 :: K_Nonterminals_s28  t -> t
      k28 K_Nonterminals_v16 = v16
      k28 K_Nonterminals_v34 = v34
      v16 :: T_Nonterminals_v16 
      v16 = \ !(T_Nonterminals_vIn16 _lhsIaroundsIn _lhsIaugmentsIn _lhsIcr _lhsIinhMap _lhsImanualAttrOrderMap _lhsImergesIn _lhsIo_rename _lhsIoptions _lhsIsynMap _lhsItypeSyns _lhsIuniq _lhsIuseMap _lhsIwrappers) -> (
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule124  () in
         let !_output = rule127  () in
         let _lhsOoutput :: Nonterminals
             !_lhsOoutput = rule128 _output in
         let _lhsOuniq :: Int
             !_lhsOuniq = rule129 _lhsIuniq in
         let !__result_ = T_Nonterminals_vOut16 _lhsOerrors _lhsOoutput _lhsOuniq
          in __result_ )
      v34 :: T_Nonterminals_v34 
      v34 = \ !(T_Nonterminals_vIn34 _lhsIcr _lhsIinhMap _lhsImanualAttrOrderMap _lhsImergesIn _lhsIo_rename _lhsIoptions _lhsIsynMap _lhsItypeSyns _lhsIuseMap _lhsIwrappers) -> (
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule124  () in
         let !__st_ = st45  ()
             !__result_ = T_Nonterminals_vOut34 _lhsOerrors __st_
          in __result_ )
     in C_Nonterminals_s28 k28
   {-# NOINLINE st32 #-}
   st32 = \  (_ :: ()) -> let
      v20 :: T_Nonterminals_v20 
      v20 = \ !(T_Nonterminals_vIn20 _lhsIaroundsIn _lhsIaugmentsIn _lhsIuniq) -> (
         let !_output = rule127  () in
         let _lhsOoutput :: Nonterminals
             !_lhsOoutput = rule128 _output in
         let _lhsOuniq :: Int
             !_lhsOuniq = rule129 _lhsIuniq in
         let !__result_ = T_Nonterminals_vOut20 _lhsOoutput _lhsOuniq
          in __result_ )
     in C_Nonterminals_s32 v20
   {-# NOINLINE st45 #-}
   st45 = \  (_ :: ()) -> let
      v35 :: T_Nonterminals_v35 
      v35 = \ !(T_Nonterminals_vIn35 _lhsIaroundsIn _lhsIaugmentsIn _lhsIuniq) -> (
         let !_output = rule127  () in
         let _lhsOoutput :: Nonterminals
             !_lhsOoutput = rule128 _output in
         let _lhsOuniq :: Int
             !_lhsOuniq = rule129 _lhsIuniq in
         let !__result_ = T_Nonterminals_vOut35 _lhsOoutput _lhsOuniq
          in __result_ )
     in C_Nonterminals_s45 v35
   {-# NOINLINE[1] rule123 #-}
   rule123 = \  (_ :: ()) ->
     Set.empty
   {-# NOINLINE[1] rule124 #-}
   rule124 = \  (_ :: ()) ->
     Seq.empty
   {-# NOINLINE[1] rule125 #-}
   rule125 = \  (_ :: ()) ->
     Map.empty
   {-# NOINLINE[1] rule126 #-}
   rule126 = \  (_ :: ()) ->
     Map.empty
   {-# NOINLINE[1] rule127 #-}
   rule127 = \  (_ :: ()) ->
     []
   {-# NOINLINE[1] rule128 #-}
   rule128 = \ !_output ->
     _output
   {-# NOINLINE[1] rule129 #-}
   rule129 = \ ((!_lhsIuniq) :: Int) ->
     _lhsIuniq

-- Pattern -----------------------------------------------------
-- wrapper
data Inh_Pattern  = Inh_Pattern { con_Inh_Pattern :: !(ConstructorIdent), nt_Inh_Pattern :: !(NontermIdent) }
data Syn_Pattern  = Syn_Pattern { containsVars_Syn_Pattern :: !(Bool), copy_Syn_Pattern :: !(Pattern), definedAttrs_Syn_Pattern :: !(Set (Identifier,Identifier)), errors_Syn_Pattern :: !(Seq Error), locals_Syn_Pattern :: !(Set Identifier), output_Syn_Pattern :: !(Pattern) }
{-# INLINABLE wrap_Pattern #-}
wrap_Pattern :: T_Pattern  -> Inh_Pattern  -> (Syn_Pattern )
wrap_Pattern !(T_Pattern act) !(Inh_Pattern _lhsIcon _lhsInt) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_Pattern_vIn5 _lhsIcon _lhsInt
        !(T_Pattern_vOut5 _lhsOcontainsVars _lhsOcopy _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput) <- return (inv_Pattern_s10 sem K_Pattern_v5 arg)
        return (Syn_Pattern _lhsOcontainsVars _lhsOcopy _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput)
   )

-- cata
{-# NOINLINE sem_Pattern #-}
sem_Pattern :: Pattern  -> T_Pattern 
sem_Pattern ( Constr !name_ pats_ ) = sem_Pattern_Constr name_ ( sem_Patterns pats_ )
sem_Pattern ( Product !pos_ pats_ ) = sem_Pattern_Product pos_ ( sem_Patterns pats_ )
sem_Pattern ( Alias !field_ !attr_ pat_ ) = sem_Pattern_Alias field_ attr_ ( sem_Pattern pat_ )
sem_Pattern ( Irrefutable pat_ ) = sem_Pattern_Irrefutable ( sem_Pattern pat_ )
sem_Pattern ( Underscore !pos_ ) = sem_Pattern_Underscore pos_

-- semantic domain
newtype T_Pattern  = T_Pattern {
                               attach_T_Pattern :: Identity (T_Pattern_s10 )
                               }
data T_Pattern_s10  where C_Pattern_s10 :: {
                                           inv_Pattern_s10 :: !(forall t. K_Pattern_s10  t -> t)
                                           } -> T_Pattern_s10 
data T_Pattern_s11  = C_Pattern_s11
data T_Pattern_s35  = C_Pattern_s35
data T_Pattern_s36  = C_Pattern_s36
data T_Pattern_s40  = C_Pattern_s40
data T_Pattern_s50  = C_Pattern_s50
data T_Pattern_s55  where C_Pattern_s55 :: {
                                           inv_Pattern_s55 :: !(forall t. K_Pattern_s55  t -> t)
                                           } -> T_Pattern_s55 
data K_Pattern_s10 k  where
   K_Pattern_v5 :: K_Pattern_s10  (T_Pattern_v5 )
   K_Pattern_v22 :: K_Pattern_s10  (T_Pattern_v22 )
   K_Pattern_v23 :: K_Pattern_s10  (T_Pattern_v23 )
   K_Pattern_v28 :: K_Pattern_s10  (T_Pattern_v28 )
   K_Pattern_v42 :: K_Pattern_s10  (T_Pattern_v42 )
   K_Pattern_v50 :: K_Pattern_s10  (T_Pattern_v50 )
data K_Pattern_s55 k  where
   K_Pattern_v51 :: K_Pattern_s55  (T_Pattern_v51 )
   K_Pattern_v56 :: K_Pattern_s55  (T_Pattern_v56 )
type T_Pattern_v5  = (T_Pattern_vIn5 ) -> (T_Pattern_vOut5 )
data T_Pattern_vIn5  = T_Pattern_vIn5 !(ConstructorIdent) !(NontermIdent)
data T_Pattern_vOut5  = T_Pattern_vOut5 !(Bool) !(Pattern) !(Set (Identifier,Identifier)) !(Seq Error) !(Set Identifier) !(Pattern)
type T_Pattern_v22  = (T_Pattern_vIn22 ) -> (T_Pattern_vOut22 )
data T_Pattern_vIn22  = T_Pattern_vIn22 
data T_Pattern_vOut22  = T_Pattern_vOut22 !(Pattern) !(Set (Identifier,Identifier)) !(Seq Error) !(Set Identifier) !(Pattern)
type T_Pattern_v23  = (T_Pattern_vIn23 ) -> (T_Pattern_vOut23 )
data T_Pattern_vIn23  = T_Pattern_vIn23 
data T_Pattern_vOut23  = T_Pattern_vOut23 !(Bool) !(Pattern) !(Set (Identifier,Identifier)) !(Seq Error) !(Set Identifier) !(Pattern)
type T_Pattern_v28  = (T_Pattern_vIn28 ) -> (T_Pattern_vOut28 )
data T_Pattern_vIn28  = T_Pattern_vIn28 
data T_Pattern_vOut28  = T_Pattern_vOut28 !(Bool) !(Set (Identifier,Identifier)) !(Seq Error) !(Set Identifier) !(Pattern)
type T_Pattern_v42  = (T_Pattern_vIn42 ) -> (T_Pattern_vOut42 )
data T_Pattern_vIn42  = T_Pattern_vIn42 
data T_Pattern_vOut42  = T_Pattern_vOut42 !(Set (Identifier,Identifier)) !(Seq Error) !(Set Identifier) !(Pattern)
type T_Pattern_v50  = (T_Pattern_vIn50 ) -> (T_Pattern_vOut50 )
data T_Pattern_vIn50  = T_Pattern_vIn50 
data T_Pattern_vOut50  = T_Pattern_vOut50 !(Set (Identifier,Identifier)) !(Seq Error) !(Set Identifier) !(T_Pattern_s55 )
type T_Pattern_v51  = (T_Pattern_vIn51 ) -> (T_Pattern_vOut51 )
data T_Pattern_vIn51  = T_Pattern_vIn51 
data T_Pattern_vOut51  = T_Pattern_vOut51 !(Bool) !(Pattern)
type T_Pattern_v56  = (T_Pattern_vIn56 ) -> (T_Pattern_vOut56 )
data T_Pattern_vIn56  = T_Pattern_vIn56 
data T_Pattern_vOut56  = T_Pattern_vOut56 !(Pattern)
{-# NOINLINE sem_Pattern_Constr #-}
sem_Pattern_Constr :: (ConstructorIdent) -> T_Patterns  -> T_Pattern 
sem_Pattern_Constr !arg_name_ arg_pats_ = T_Pattern (return st10) where
   {-# NOINLINE st10 #-}
   !st10 = let
      k10 :: K_Pattern_s10  t -> t
      k10 K_Pattern_v5 = v5
      k10 K_Pattern_v22 = v22
      k10 K_Pattern_v23 = v23
      k10 K_Pattern_v28 = v28
      k10 K_Pattern_v42 = v42
      k10 K_Pattern_v50 = v50
      v5 :: T_Pattern_v5 
      v5 = \ !(T_Pattern_vIn5 _lhsIcon _lhsInt) -> (
         let !_patsX12 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_pats_)) in
         let !(T_Patterns_vOut21 _patsIcontainsVars _patsIcopy _patsIdefinedAttrs _patsIerrors _patsIlocals _patsIoutput) = inv_Patterns_s12 _patsX12 K_Patterns_v21 (T_Patterns_vIn21 ) in
         let _lhsOcontainsVars :: Bool
             !_lhsOcontainsVars = rule130 _patsIcontainsVars in
         let !_copy = rule134 _patsIcopy arg_name_ in
         let _lhsOcopy :: Pattern
             !_lhsOcopy = rule136 _copy in
         let _lhsOdefinedAttrs :: Set (Identifier,Identifier)
             !_lhsOdefinedAttrs = rule131 _patsIdefinedAttrs in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule132 _patsIerrors in
         let _lhsOlocals :: Set Identifier
             !_lhsOlocals = rule133 _patsIlocals in
         let !_output = rule135 _patsIoutput arg_name_ in
         let _lhsOoutput :: Pattern
             !_lhsOoutput = rule137 _output in
         let !__result_ = T_Pattern_vOut5 _lhsOcontainsVars _lhsOcopy _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput
          in __result_ )
      v22 :: T_Pattern_v22 
      v22 = \ !(T_Pattern_vIn22 ) -> (
         let !_patsX12 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_pats_)) in
         let !(T_Patterns_vOut38 _patsIcopy _patsIdefinedAttrs _patsIerrors _patsIlocals _patsIoutput) = inv_Patterns_s12 _patsX12 K_Patterns_v38 (T_Patterns_vIn38 ) in
         let !_copy = rule134 _patsIcopy arg_name_ in
         let _lhsOcopy :: Pattern
             !_lhsOcopy = rule136 _copy in
         let _lhsOdefinedAttrs :: Set (Identifier,Identifier)
             !_lhsOdefinedAttrs = rule131 _patsIdefinedAttrs in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule132 _patsIerrors in
         let _lhsOlocals :: Set Identifier
             !_lhsOlocals = rule133 _patsIlocals in
         let !_output = rule135 _patsIoutput arg_name_ in
         let _lhsOoutput :: Pattern
             !_lhsOoutput = rule137 _output in
         let !__result_ = T_Pattern_vOut22 _lhsOcopy _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput
          in __result_ )
      v23 :: T_Pattern_v23 
      v23 = \ !(T_Pattern_vIn23 ) -> (
         let !_patsX12 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_pats_)) in
         let !(T_Patterns_vOut21 _patsIcontainsVars _patsIcopy _patsIdefinedAttrs _patsIerrors _patsIlocals _patsIoutput) = inv_Patterns_s12 _patsX12 K_Patterns_v21 (T_Patterns_vIn21 ) in
         let _lhsOcontainsVars :: Bool
             !_lhsOcontainsVars = rule130 _patsIcontainsVars in
         let !_copy = rule134 _patsIcopy arg_name_ in
         let _lhsOcopy :: Pattern
             !_lhsOcopy = rule136 _copy in
         let _lhsOdefinedAttrs :: Set (Identifier,Identifier)
             !_lhsOdefinedAttrs = rule131 _patsIdefinedAttrs in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule132 _patsIerrors in
         let _lhsOlocals :: Set Identifier
             !_lhsOlocals = rule133 _patsIlocals in
         let !_output = rule135 _patsIoutput arg_name_ in
         let _lhsOoutput :: Pattern
             !_lhsOoutput = rule137 _output in
         let !__result_ = T_Pattern_vOut23 _lhsOcontainsVars _lhsOcopy _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput
          in __result_ )
      v28 :: T_Pattern_v28 
      v28 = \ !(T_Pattern_vIn28 ) -> (
         let !_patsX12 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_pats_)) in
         let !(T_Patterns_vOut41 _patsIcontainsVars _patsIdefinedAttrs _patsIerrors _patsIlocals _patsIoutput) = inv_Patterns_s12 _patsX12 K_Patterns_v41 (T_Patterns_vIn41 ) in
         let _lhsOcontainsVars :: Bool
             !_lhsOcontainsVars = rule130 _patsIcontainsVars in
         let _lhsOdefinedAttrs :: Set (Identifier,Identifier)
             !_lhsOdefinedAttrs = rule131 _patsIdefinedAttrs in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule132 _patsIerrors in
         let _lhsOlocals :: Set Identifier
             !_lhsOlocals = rule133 _patsIlocals in
         let !_output = rule135 _patsIoutput arg_name_ in
         let _lhsOoutput :: Pattern
             !_lhsOoutput = rule137 _output in
         let !__result_ = T_Pattern_vOut28 _lhsOcontainsVars _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput
          in __result_ )
      v42 :: T_Pattern_v42 
      v42 = \ !(T_Pattern_vIn42 ) -> (
         let !_patsX12 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_pats_)) in
         let !(T_Patterns_vOut49 _patsIdefinedAttrs _patsIerrors _patsIlocals _patsIoutput) = inv_Patterns_s12 _patsX12 K_Patterns_v49 (T_Patterns_vIn49 ) in
         let _lhsOdefinedAttrs :: Set (Identifier,Identifier)
             !_lhsOdefinedAttrs = rule131 _patsIdefinedAttrs in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule132 _patsIerrors in
         let _lhsOlocals :: Set Identifier
             !_lhsOlocals = rule133 _patsIlocals in
         let !_output = rule135 _patsIoutput arg_name_ in
         let _lhsOoutput :: Pattern
             !_lhsOoutput = rule137 _output in
         let !__result_ = T_Pattern_vOut42 _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput
          in __result_ )
      v50 :: T_Pattern_v50 
      v50 = \ !(T_Pattern_vIn50 ) -> (
         let !_patsX12 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_pats_)) in
         let !(T_Patterns_vOut54 _patsIdefinedAttrs _patsIerrors _patsIlocals _patsX57) = inv_Patterns_s12 _patsX12 K_Patterns_v54 (T_Patterns_vIn54 ) in
         let _lhsOdefinedAttrs :: Set (Identifier,Identifier)
             !_lhsOdefinedAttrs = rule131 _patsIdefinedAttrs in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule132 _patsIerrors in
         let _lhsOlocals :: Set Identifier
             !_lhsOlocals = rule133 _patsIlocals in
         let !__st_ = st55 _patsX57
             !__result_ = T_Pattern_vOut50 _lhsOdefinedAttrs _lhsOerrors _lhsOlocals __st_
          in __result_ )
     in C_Pattern_s10 k10
   {-# NOINLINE st55 #-}
   st55 = \ !_patsX57 -> let
      k55 :: K_Pattern_s55  t -> t
      k55 K_Pattern_v51 = v51
      k55 K_Pattern_v56 = v56
      v51 :: T_Pattern_v51 
      v51 = \ !(T_Pattern_vIn51 ) -> (
         let !(T_Patterns_vOut55 _patsIcontainsVars _patsIoutput) = inv_Patterns_s57 _patsX57 K_Patterns_v55 (T_Patterns_vIn55 ) in
         let _lhsOcontainsVars :: Bool
             !_lhsOcontainsVars = rule130 _patsIcontainsVars in
         let !_output = rule135 _patsIoutput arg_name_ in
         let _lhsOoutput :: Pattern
             !_lhsOoutput = rule137 _output in
         let !__result_ = T_Pattern_vOut51 _lhsOcontainsVars _lhsOoutput
          in __result_ )
      v56 :: T_Pattern_v56 
      v56 = \ !(T_Pattern_vIn56 ) -> (
         let !(T_Patterns_vOut57 _patsIoutput) = inv_Patterns_s57 _patsX57 K_Patterns_v57 (T_Patterns_vIn57 ) in
         let !_output = rule135 _patsIoutput arg_name_ in
         let _lhsOoutput :: Pattern
             !_lhsOoutput = rule137 _output in
         let !__result_ = T_Pattern_vOut56 _lhsOoutput
          in __result_ )
     in C_Pattern_s55 k55
   {-# NOINLINE[1] rule130 #-}
   rule130 = \ ((!_patsIcontainsVars) :: Bool) ->
     _patsIcontainsVars
   {-# NOINLINE[1] rule131 #-}
   rule131 = \ ((!_patsIdefinedAttrs) :: Set (Identifier,Identifier)) ->
     _patsIdefinedAttrs
   {-# NOINLINE[1] rule132 #-}
   rule132 = \ ((!_patsIerrors) :: Seq Error) ->
     _patsIerrors
   {-# NOINLINE[1] rule133 #-}
   rule133 = \ ((!_patsIlocals) :: Set Identifier) ->
     _patsIlocals
   {-# NOINLINE[1] rule134 #-}
   rule134 = \ ((!_patsIcopy) :: Patterns) !name_ ->
     Constr name_ _patsIcopy
   {-# NOINLINE[1] rule135 #-}
   rule135 = \ ((!_patsIoutput) :: Patterns) !name_ ->
     Constr name_ _patsIoutput
   {-# NOINLINE[1] rule136 #-}
   rule136 = \ !_copy ->
     _copy
   {-# NOINLINE[1] rule137 #-}
   rule137 = \ !_output ->
     _output
{-# NOINLINE sem_Pattern_Product #-}
sem_Pattern_Product :: (Pos) -> T_Patterns  -> T_Pattern 
sem_Pattern_Product !arg_pos_ arg_pats_ = T_Pattern (return st10) where
   {-# NOINLINE st10 #-}
   !st10 = let
      k10 :: K_Pattern_s10  t -> t
      k10 K_Pattern_v5 = v5
      k10 K_Pattern_v22 = v22
      k10 K_Pattern_v23 = v23
      k10 K_Pattern_v28 = v28
      k10 K_Pattern_v42 = v42
      k10 K_Pattern_v50 = v50
      v5 :: T_Pattern_v5 
      v5 = \ !(T_Pattern_vIn5 _lhsIcon _lhsInt) -> (
         let !_patsX12 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_pats_)) in
         let !(T_Patterns_vOut21 _patsIcontainsVars _patsIcopy _patsIdefinedAttrs _patsIerrors _patsIlocals _patsIoutput) = inv_Patterns_s12 _patsX12 K_Patterns_v21 (T_Patterns_vIn21 ) in
         let _lhsOcontainsVars :: Bool
             !_lhsOcontainsVars = rule140 _patsIcontainsVars in
         let !_copy = rule144 _patsIcopy arg_pos_ in
         let _lhsOcopy :: Pattern
             !_lhsOcopy = rule146 _copy in
         let _lhsOdefinedAttrs :: Set (Identifier,Identifier)
             !_lhsOdefinedAttrs = rule141 _patsIdefinedAttrs in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule142 _patsIerrors in
         let _lhsOlocals :: Set Identifier
             !_lhsOlocals = rule143 _patsIlocals in
         let !_output = rule145 _patsIoutput arg_pos_ in
         let _lhsOoutput :: Pattern
             !_lhsOoutput = rule147 _output in
         let !__result_ = T_Pattern_vOut5 _lhsOcontainsVars _lhsOcopy _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput
          in __result_ )
      v22 :: T_Pattern_v22 
      v22 = \ !(T_Pattern_vIn22 ) -> (
         let !_patsX12 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_pats_)) in
         let !(T_Patterns_vOut38 _patsIcopy _patsIdefinedAttrs _patsIerrors _patsIlocals _patsIoutput) = inv_Patterns_s12 _patsX12 K_Patterns_v38 (T_Patterns_vIn38 ) in
         let !_copy = rule144 _patsIcopy arg_pos_ in
         let _lhsOcopy :: Pattern
             !_lhsOcopy = rule146 _copy in
         let _lhsOdefinedAttrs :: Set (Identifier,Identifier)
             !_lhsOdefinedAttrs = rule141 _patsIdefinedAttrs in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule142 _patsIerrors in
         let _lhsOlocals :: Set Identifier
             !_lhsOlocals = rule143 _patsIlocals in
         let !_output = rule145 _patsIoutput arg_pos_ in
         let _lhsOoutput :: Pattern
             !_lhsOoutput = rule147 _output in
         let !__result_ = T_Pattern_vOut22 _lhsOcopy _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput
          in __result_ )
      v23 :: T_Pattern_v23 
      v23 = \ !(T_Pattern_vIn23 ) -> (
         let !_patsX12 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_pats_)) in
         let !(T_Patterns_vOut21 _patsIcontainsVars _patsIcopy _patsIdefinedAttrs _patsIerrors _patsIlocals _patsIoutput) = inv_Patterns_s12 _patsX12 K_Patterns_v21 (T_Patterns_vIn21 ) in
         let _lhsOcontainsVars :: Bool
             !_lhsOcontainsVars = rule140 _patsIcontainsVars in
         let !_copy = rule144 _patsIcopy arg_pos_ in
         let _lhsOcopy :: Pattern
             !_lhsOcopy = rule146 _copy in
         let _lhsOdefinedAttrs :: Set (Identifier,Identifier)
             !_lhsOdefinedAttrs = rule141 _patsIdefinedAttrs in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule142 _patsIerrors in
         let _lhsOlocals :: Set Identifier
             !_lhsOlocals = rule143 _patsIlocals in
         let !_output = rule145 _patsIoutput arg_pos_ in
         let _lhsOoutput :: Pattern
             !_lhsOoutput = rule147 _output in
         let !__result_ = T_Pattern_vOut23 _lhsOcontainsVars _lhsOcopy _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput
          in __result_ )
      v28 :: T_Pattern_v28 
      v28 = \ !(T_Pattern_vIn28 ) -> (
         let !_patsX12 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_pats_)) in
         let !(T_Patterns_vOut41 _patsIcontainsVars _patsIdefinedAttrs _patsIerrors _patsIlocals _patsIoutput) = inv_Patterns_s12 _patsX12 K_Patterns_v41 (T_Patterns_vIn41 ) in
         let _lhsOcontainsVars :: Bool
             !_lhsOcontainsVars = rule140 _patsIcontainsVars in
         let _lhsOdefinedAttrs :: Set (Identifier,Identifier)
             !_lhsOdefinedAttrs = rule141 _patsIdefinedAttrs in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule142 _patsIerrors in
         let _lhsOlocals :: Set Identifier
             !_lhsOlocals = rule143 _patsIlocals in
         let !_output = rule145 _patsIoutput arg_pos_ in
         let _lhsOoutput :: Pattern
             !_lhsOoutput = rule147 _output in
         let !__result_ = T_Pattern_vOut28 _lhsOcontainsVars _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput
          in __result_ )
      v42 :: T_Pattern_v42 
      v42 = \ !(T_Pattern_vIn42 ) -> (
         let !_patsX12 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_pats_)) in
         let !(T_Patterns_vOut49 _patsIdefinedAttrs _patsIerrors _patsIlocals _patsIoutput) = inv_Patterns_s12 _patsX12 K_Patterns_v49 (T_Patterns_vIn49 ) in
         let _lhsOdefinedAttrs :: Set (Identifier,Identifier)
             !_lhsOdefinedAttrs = rule141 _patsIdefinedAttrs in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule142 _patsIerrors in
         let _lhsOlocals :: Set Identifier
             !_lhsOlocals = rule143 _patsIlocals in
         let !_output = rule145 _patsIoutput arg_pos_ in
         let _lhsOoutput :: Pattern
             !_lhsOoutput = rule147 _output in
         let !__result_ = T_Pattern_vOut42 _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput
          in __result_ )
      v50 :: T_Pattern_v50 
      v50 = \ !(T_Pattern_vIn50 ) -> (
         let !_patsX12 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_pats_)) in
         let !(T_Patterns_vOut54 _patsIdefinedAttrs _patsIerrors _patsIlocals _patsX57) = inv_Patterns_s12 _patsX12 K_Patterns_v54 (T_Patterns_vIn54 ) in
         let _lhsOdefinedAttrs :: Set (Identifier,Identifier)
             !_lhsOdefinedAttrs = rule141 _patsIdefinedAttrs in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule142 _patsIerrors in
         let _lhsOlocals :: Set Identifier
             !_lhsOlocals = rule143 _patsIlocals in
         let !__st_ = st55 _patsX57
             !__result_ = T_Pattern_vOut50 _lhsOdefinedAttrs _lhsOerrors _lhsOlocals __st_
          in __result_ )
     in C_Pattern_s10 k10
   {-# NOINLINE st55 #-}
   st55 = \ !_patsX57 -> let
      k55 :: K_Pattern_s55  t -> t
      k55 K_Pattern_v51 = v51
      k55 K_Pattern_v56 = v56
      v51 :: T_Pattern_v51 
      v51 = \ !(T_Pattern_vIn51 ) -> (
         let !(T_Patterns_vOut55 _patsIcontainsVars _patsIoutput) = inv_Patterns_s57 _patsX57 K_Patterns_v55 (T_Patterns_vIn55 ) in
         let _lhsOcontainsVars :: Bool
             !_lhsOcontainsVars = rule140 _patsIcontainsVars in
         let !_output = rule145 _patsIoutput arg_pos_ in
         let _lhsOoutput :: Pattern
             !_lhsOoutput = rule147 _output in
         let !__result_ = T_Pattern_vOut51 _lhsOcontainsVars _lhsOoutput
          in __result_ )
      v56 :: T_Pattern_v56 
      v56 = \ !(T_Pattern_vIn56 ) -> (
         let !(T_Patterns_vOut57 _patsIoutput) = inv_Patterns_s57 _patsX57 K_Patterns_v57 (T_Patterns_vIn57 ) in
         let !_output = rule145 _patsIoutput arg_pos_ in
         let _lhsOoutput :: Pattern
             !_lhsOoutput = rule147 _output in
         let !__result_ = T_Pattern_vOut56 _lhsOoutput
          in __result_ )
     in C_Pattern_s55 k55
   {-# NOINLINE[1] rule140 #-}
   rule140 = \ ((!_patsIcontainsVars) :: Bool) ->
     _patsIcontainsVars
   {-# NOINLINE[1] rule141 #-}
   rule141 = \ ((!_patsIdefinedAttrs) :: Set (Identifier,Identifier)) ->
     _patsIdefinedAttrs
   {-# NOINLINE[1] rule142 #-}
   rule142 = \ ((!_patsIerrors) :: Seq Error) ->
     _patsIerrors
   {-# NOINLINE[1] rule143 #-}
   rule143 = \ ((!_patsIlocals) :: Set Identifier) ->
     _patsIlocals
   {-# NOINLINE[1] rule144 #-}
   rule144 = \ ((!_patsIcopy) :: Patterns) !pos_ ->
     Product pos_ _patsIcopy
   {-# NOINLINE[1] rule145 #-}
   rule145 = \ ((!_patsIoutput) :: Patterns) !pos_ ->
     Product pos_ _patsIoutput
   {-# NOINLINE[1] rule146 #-}
   rule146 = \ !_copy ->
     _copy
   {-# NOINLINE[1] rule147 #-}
   rule147 = \ !_output ->
     _output
{-# NOINLINE sem_Pattern_Alias #-}
sem_Pattern_Alias :: (Identifier) -> (Identifier) -> T_Pattern  -> T_Pattern 
sem_Pattern_Alias !arg_field_ !arg_attr_ arg_pat_ = T_Pattern (return st10) where
   {-# NOINLINE st10 #-}
   !st10 = let
      k10 :: K_Pattern_s10  t -> t
      k10 K_Pattern_v5 = v5
      k10 K_Pattern_v22 = v22
      k10 K_Pattern_v23 = v23
      k10 K_Pattern_v28 = v28
      k10 K_Pattern_v42 = v42
      k10 K_Pattern_v50 = v50
      v5 :: T_Pattern_v5 
      v5 = \ !(T_Pattern_vIn5 _lhsIcon _lhsInt) -> (
         let _lhsOcontainsVars :: Bool
             !_lhsOcontainsVars = rule152  () in
         let !_patX10 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pat_)) in
         let !(T_Pattern_vOut22 _patIcopy _patIdefinedAttrs _patIerrors _patIlocals _patIoutput) = inv_Pattern_s10 _patX10 K_Pattern_v22 (T_Pattern_vIn22 ) in
         let !_copy = rule154 _patIcopy arg_attr_ arg_field_ in
         let _lhsOcopy :: Pattern
             !_lhsOcopy = rule156 _copy in
         let _lhsOdefinedAttrs :: Set (Identifier,Identifier)
             !_lhsOdefinedAttrs = rule150 _patIdefinedAttrs arg_attr_ arg_field_ in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule153 _patIerrors in
         let _lhsOlocals :: Set Identifier
             !_lhsOlocals = rule151 _patIlocals arg_attr_ arg_field_ in
         let !_output = rule155 _patIoutput arg_attr_ arg_field_ in
         let _lhsOoutput :: Pattern
             !_lhsOoutput = rule157 _output in
         let !__result_ = T_Pattern_vOut5 _lhsOcontainsVars _lhsOcopy _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput
          in __result_ )
      v22 :: T_Pattern_v22 
      v22 = \ !(T_Pattern_vIn22 ) -> (
         let !_patX10 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pat_)) in
         let !(T_Pattern_vOut22 _patIcopy _patIdefinedAttrs _patIerrors _patIlocals _patIoutput) = inv_Pattern_s10 _patX10 K_Pattern_v22 (T_Pattern_vIn22 ) in
         let !_copy = rule154 _patIcopy arg_attr_ arg_field_ in
         let _lhsOcopy :: Pattern
             !_lhsOcopy = rule156 _copy in
         let _lhsOdefinedAttrs :: Set (Identifier,Identifier)
             !_lhsOdefinedAttrs = rule150 _patIdefinedAttrs arg_attr_ arg_field_ in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule153 _patIerrors in
         let _lhsOlocals :: Set Identifier
             !_lhsOlocals = rule151 _patIlocals arg_attr_ arg_field_ in
         let !_output = rule155 _patIoutput arg_attr_ arg_field_ in
         let _lhsOoutput :: Pattern
             !_lhsOoutput = rule157 _output in
         let !__result_ = T_Pattern_vOut22 _lhsOcopy _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput
          in __result_ )
      v23 :: T_Pattern_v23 
      v23 = \ !(T_Pattern_vIn23 ) -> (
         let _lhsOcontainsVars :: Bool
             !_lhsOcontainsVars = rule152  () in
         let !_patX10 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pat_)) in
         let !(T_Pattern_vOut22 _patIcopy _patIdefinedAttrs _patIerrors _patIlocals _patIoutput) = inv_Pattern_s10 _patX10 K_Pattern_v22 (T_Pattern_vIn22 ) in
         let !_copy = rule154 _patIcopy arg_attr_ arg_field_ in
         let _lhsOcopy :: Pattern
             !_lhsOcopy = rule156 _copy in
         let _lhsOdefinedAttrs :: Set (Identifier,Identifier)
             !_lhsOdefinedAttrs = rule150 _patIdefinedAttrs arg_attr_ arg_field_ in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule153 _patIerrors in
         let _lhsOlocals :: Set Identifier
             !_lhsOlocals = rule151 _patIlocals arg_attr_ arg_field_ in
         let !_output = rule155 _patIoutput arg_attr_ arg_field_ in
         let _lhsOoutput :: Pattern
             !_lhsOoutput = rule157 _output in
         let !__result_ = T_Pattern_vOut23 _lhsOcontainsVars _lhsOcopy _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput
          in __result_ )
      v28 :: T_Pattern_v28 
      v28 = \ !(T_Pattern_vIn28 ) -> (
         let _lhsOcontainsVars :: Bool
             !_lhsOcontainsVars = rule152  () in
         let !_patX10 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pat_)) in
         let !(T_Pattern_vOut42 _patIdefinedAttrs _patIerrors _patIlocals _patIoutput) = inv_Pattern_s10 _patX10 K_Pattern_v42 (T_Pattern_vIn42 ) in
         let _lhsOdefinedAttrs :: Set (Identifier,Identifier)
             !_lhsOdefinedAttrs = rule150 _patIdefinedAttrs arg_attr_ arg_field_ in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule153 _patIerrors in
         let _lhsOlocals :: Set Identifier
             !_lhsOlocals = rule151 _patIlocals arg_attr_ arg_field_ in
         let !_output = rule155 _patIoutput arg_attr_ arg_field_ in
         let _lhsOoutput :: Pattern
             !_lhsOoutput = rule157 _output in
         let !__result_ = T_Pattern_vOut28 _lhsOcontainsVars _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput
          in __result_ )
      v42 :: T_Pattern_v42 
      v42 = \ !(T_Pattern_vIn42 ) -> (
         let !_patX10 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pat_)) in
         let !(T_Pattern_vOut42 _patIdefinedAttrs _patIerrors _patIlocals _patIoutput) = inv_Pattern_s10 _patX10 K_Pattern_v42 (T_Pattern_vIn42 ) in
         let _lhsOdefinedAttrs :: Set (Identifier,Identifier)
             !_lhsOdefinedAttrs = rule150 _patIdefinedAttrs arg_attr_ arg_field_ in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule153 _patIerrors in
         let _lhsOlocals :: Set Identifier
             !_lhsOlocals = rule151 _patIlocals arg_attr_ arg_field_ in
         let !_output = rule155 _patIoutput arg_attr_ arg_field_ in
         let _lhsOoutput :: Pattern
             !_lhsOoutput = rule157 _output in
         let !__result_ = T_Pattern_vOut42 _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput
          in __result_ )
      v50 :: T_Pattern_v50 
      v50 = \ !(T_Pattern_vIn50 ) -> (
         let !_patX10 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pat_)) in
         let !(T_Pattern_vOut50 _patIdefinedAttrs _patIerrors _patIlocals _patX55) = inv_Pattern_s10 _patX10 K_Pattern_v50 (T_Pattern_vIn50 ) in
         let _lhsOdefinedAttrs :: Set (Identifier,Identifier)
             !_lhsOdefinedAttrs = rule150 _patIdefinedAttrs arg_attr_ arg_field_ in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule153 _patIerrors in
         let _lhsOlocals :: Set Identifier
             !_lhsOlocals = rule151 _patIlocals arg_attr_ arg_field_ in
         let !__st_ = st55 _patX55
             !__result_ = T_Pattern_vOut50 _lhsOdefinedAttrs _lhsOerrors _lhsOlocals __st_
          in __result_ )
     in C_Pattern_s10 k10
   {-# NOINLINE st55 #-}
   st55 = \ !_patX55 -> let
      k55 :: K_Pattern_s55  t -> t
      k55 K_Pattern_v51 = v51
      k55 K_Pattern_v56 = v56
      v51 :: T_Pattern_v51 
      v51 = \ !(T_Pattern_vIn51 ) -> (
         let _lhsOcontainsVars :: Bool
             !_lhsOcontainsVars = rule152  () in
         let !(T_Pattern_vOut56 _patIoutput) = inv_Pattern_s55 _patX55 K_Pattern_v56 (T_Pattern_vIn56 ) in
         let !_output = rule155 _patIoutput arg_attr_ arg_field_ in
         let _lhsOoutput :: Pattern
             !_lhsOoutput = rule157 _output in
         let !__result_ = T_Pattern_vOut51 _lhsOcontainsVars _lhsOoutput
          in __result_ )
      v56 :: T_Pattern_v56 
      v56 = \ !(T_Pattern_vIn56 ) -> (
         let !(T_Pattern_vOut56 _patIoutput) = inv_Pattern_s55 _patX55 K_Pattern_v56 (T_Pattern_vIn56 ) in
         let !_output = rule155 _patIoutput arg_attr_ arg_field_ in
         let _lhsOoutput :: Pattern
             !_lhsOoutput = rule157 _output in
         let !__result_ = T_Pattern_vOut56 _lhsOoutput
          in __result_ )
     in C_Pattern_s55 k55
   {-# NOINLINE rule150 #-}
   {-# LINE 536 "./src-ag/DefaultRules.ag" #-}
   rule150 = \ ((!_patIdefinedAttrs) :: Set (Identifier,Identifier)) !attr_ !field_ ->
                               {-# LINE 536 "./src-ag/DefaultRules.ag" #-}
                               Set.insert (field_,attr_) _patIdefinedAttrs
                               {-# LINE 2562 "dist/build/DefaultRules.hs"#-}
   {-# NOINLINE rule151 #-}
   {-# LINE 537 "./src-ag/DefaultRules.ag" #-}
   rule151 = \ ((!_patIlocals) :: Set Identifier) !attr_ !field_ ->
                               {-# LINE 537 "./src-ag/DefaultRules.ag" #-}
                               if field_ == _LOC
                                  then Set.insert attr_ _patIlocals
                                  else _patIlocals
                               {-# LINE 2570 "dist/build/DefaultRules.hs"#-}
   {-# NOINLINE rule152 #-}
   {-# LINE 554 "./src-ag/DefaultRules.ag" #-}
   rule152 = \  (_ :: ()) ->
                                    {-# LINE 554 "./src-ag/DefaultRules.ag" #-}
                                    True
                                    {-# LINE 2576 "dist/build/DefaultRules.hs"#-}
   {-# NOINLINE[1] rule153 #-}
   rule153 = \ ((!_patIerrors) :: Seq Error) ->
     _patIerrors
   {-# NOINLINE[1] rule154 #-}
   rule154 = \ ((!_patIcopy) :: Pattern) !attr_ !field_ ->
     Alias field_ attr_ _patIcopy
   {-# NOINLINE[1] rule155 #-}
   rule155 = \ ((!_patIoutput) :: Pattern) !attr_ !field_ ->
     Alias field_ attr_ _patIoutput
   {-# NOINLINE[1] rule156 #-}
   rule156 = \ !_copy ->
     _copy
   {-# NOINLINE[1] rule157 #-}
   rule157 = \ !_output ->
     _output
{-# NOINLINE sem_Pattern_Irrefutable #-}
sem_Pattern_Irrefutable :: T_Pattern  -> T_Pattern 
sem_Pattern_Irrefutable arg_pat_ = T_Pattern (return st10) where
   {-# NOINLINE st10 #-}
   !st10 = let
      k10 :: K_Pattern_s10  t -> t
      k10 K_Pattern_v5 = v5
      k10 K_Pattern_v22 = v22
      k10 K_Pattern_v23 = v23
      k10 K_Pattern_v28 = v28
      k10 K_Pattern_v42 = v42
      k10 K_Pattern_v50 = v50
      v5 :: T_Pattern_v5 
      v5 = \ !(T_Pattern_vIn5 _lhsIcon _lhsInt) -> (
         let !_patX10 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pat_)) in
         let !(T_Pattern_vOut23 _patIcontainsVars _patIcopy _patIdefinedAttrs _patIerrors _patIlocals _patIoutput) = inv_Pattern_s10 _patX10 K_Pattern_v23 (T_Pattern_vIn23 ) in
         let _lhsOcontainsVars :: Bool
             !_lhsOcontainsVars = rule160 _patIcontainsVars in
         let !_copy = rule164 _patIcopy in
         let _lhsOcopy :: Pattern
             !_lhsOcopy = rule166 _copy in
         let _lhsOdefinedAttrs :: Set (Identifier,Identifier)
             !_lhsOdefinedAttrs = rule161 _patIdefinedAttrs in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule162 _patIerrors in
         let _lhsOlocals :: Set Identifier
             !_lhsOlocals = rule163 _patIlocals in
         let !_output = rule165 _patIoutput in
         let _lhsOoutput :: Pattern
             !_lhsOoutput = rule167 _output in
         let !__result_ = T_Pattern_vOut5 _lhsOcontainsVars _lhsOcopy _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput
          in __result_ )
      v22 :: T_Pattern_v22 
      v22 = \ !(T_Pattern_vIn22 ) -> (
         let !_patX10 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pat_)) in
         let !(T_Pattern_vOut22 _patIcopy _patIdefinedAttrs _patIerrors _patIlocals _patIoutput) = inv_Pattern_s10 _patX10 K_Pattern_v22 (T_Pattern_vIn22 ) in
         let !_copy = rule164 _patIcopy in
         let _lhsOcopy :: Pattern
             !_lhsOcopy = rule166 _copy in
         let _lhsOdefinedAttrs :: Set (Identifier,Identifier)
             !_lhsOdefinedAttrs = rule161 _patIdefinedAttrs in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule162 _patIerrors in
         let _lhsOlocals :: Set Identifier
             !_lhsOlocals = rule163 _patIlocals in
         let !_output = rule165 _patIoutput in
         let _lhsOoutput :: Pattern
             !_lhsOoutput = rule167 _output in
         let !__result_ = T_Pattern_vOut22 _lhsOcopy _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput
          in __result_ )
      v23 :: T_Pattern_v23 
      v23 = \ !(T_Pattern_vIn23 ) -> (
         let !_patX10 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pat_)) in
         let !(T_Pattern_vOut23 _patIcontainsVars _patIcopy _patIdefinedAttrs _patIerrors _patIlocals _patIoutput) = inv_Pattern_s10 _patX10 K_Pattern_v23 (T_Pattern_vIn23 ) in
         let _lhsOcontainsVars :: Bool
             !_lhsOcontainsVars = rule160 _patIcontainsVars in
         let !_copy = rule164 _patIcopy in
         let _lhsOcopy :: Pattern
             !_lhsOcopy = rule166 _copy in
         let _lhsOdefinedAttrs :: Set (Identifier,Identifier)
             !_lhsOdefinedAttrs = rule161 _patIdefinedAttrs in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule162 _patIerrors in
         let _lhsOlocals :: Set Identifier
             !_lhsOlocals = rule163 _patIlocals in
         let !_output = rule165 _patIoutput in
         let _lhsOoutput :: Pattern
             !_lhsOoutput = rule167 _output in
         let !__result_ = T_Pattern_vOut23 _lhsOcontainsVars _lhsOcopy _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput
          in __result_ )
      v28 :: T_Pattern_v28 
      v28 = \ !(T_Pattern_vIn28 ) -> (
         let !_patX10 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pat_)) in
         let !(T_Pattern_vOut28 _patIcontainsVars _patIdefinedAttrs _patIerrors _patIlocals _patIoutput) = inv_Pattern_s10 _patX10 K_Pattern_v28 (T_Pattern_vIn28 ) in
         let _lhsOcontainsVars :: Bool
             !_lhsOcontainsVars = rule160 _patIcontainsVars in
         let _lhsOdefinedAttrs :: Set (Identifier,Identifier)
             !_lhsOdefinedAttrs = rule161 _patIdefinedAttrs in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule162 _patIerrors in
         let _lhsOlocals :: Set Identifier
             !_lhsOlocals = rule163 _patIlocals in
         let !_output = rule165 _patIoutput in
         let _lhsOoutput :: Pattern
             !_lhsOoutput = rule167 _output in
         let !__result_ = T_Pattern_vOut28 _lhsOcontainsVars _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput
          in __result_ )
      v42 :: T_Pattern_v42 
      v42 = \ !(T_Pattern_vIn42 ) -> (
         let !_patX10 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pat_)) in
         let !(T_Pattern_vOut42 _patIdefinedAttrs _patIerrors _patIlocals _patIoutput) = inv_Pattern_s10 _patX10 K_Pattern_v42 (T_Pattern_vIn42 ) in
         let _lhsOdefinedAttrs :: Set (Identifier,Identifier)
             !_lhsOdefinedAttrs = rule161 _patIdefinedAttrs in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule162 _patIerrors in
         let _lhsOlocals :: Set Identifier
             !_lhsOlocals = rule163 _patIlocals in
         let !_output = rule165 _patIoutput in
         let _lhsOoutput :: Pattern
             !_lhsOoutput = rule167 _output in
         let !__result_ = T_Pattern_vOut42 _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput
          in __result_ )
      v50 :: T_Pattern_v50 
      v50 = \ !(T_Pattern_vIn50 ) -> (
         let !_patX10 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pat_)) in
         let !(T_Pattern_vOut50 _patIdefinedAttrs _patIerrors _patIlocals _patX55) = inv_Pattern_s10 _patX10 K_Pattern_v50 (T_Pattern_vIn50 ) in
         let _lhsOdefinedAttrs :: Set (Identifier,Identifier)
             !_lhsOdefinedAttrs = rule161 _patIdefinedAttrs in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule162 _patIerrors in
         let _lhsOlocals :: Set Identifier
             !_lhsOlocals = rule163 _patIlocals in
         let !__st_ = st55 _patX55
             !__result_ = T_Pattern_vOut50 _lhsOdefinedAttrs _lhsOerrors _lhsOlocals __st_
          in __result_ )
     in C_Pattern_s10 k10
   {-# NOINLINE st55 #-}
   st55 = \ !_patX55 -> let
      k55 :: K_Pattern_s55  t -> t
      k55 K_Pattern_v51 = v51
      k55 K_Pattern_v56 = v56
      v51 :: T_Pattern_v51 
      v51 = \ !(T_Pattern_vIn51 ) -> (
         let !(T_Pattern_vOut51 _patIcontainsVars _patIoutput) = inv_Pattern_s55 _patX55 K_Pattern_v51 (T_Pattern_vIn51 ) in
         let _lhsOcontainsVars :: Bool
             !_lhsOcontainsVars = rule160 _patIcontainsVars in
         let !_output = rule165 _patIoutput in
         let _lhsOoutput :: Pattern
             !_lhsOoutput = rule167 _output in
         let !__result_ = T_Pattern_vOut51 _lhsOcontainsVars _lhsOoutput
          in __result_ )
      v56 :: T_Pattern_v56 
      v56 = \ !(T_Pattern_vIn56 ) -> (
         let !(T_Pattern_vOut56 _patIoutput) = inv_Pattern_s55 _patX55 K_Pattern_v56 (T_Pattern_vIn56 ) in
         let !_output = rule165 _patIoutput in
         let _lhsOoutput :: Pattern
             !_lhsOoutput = rule167 _output in
         let !__result_ = T_Pattern_vOut56 _lhsOoutput
          in __result_ )
     in C_Pattern_s55 k55
   {-# NOINLINE[1] rule160 #-}
   rule160 = \ ((!_patIcontainsVars) :: Bool) ->
     _patIcontainsVars
   {-# NOINLINE[1] rule161 #-}
   rule161 = \ ((!_patIdefinedAttrs) :: Set (Identifier,Identifier)) ->
     _patIdefinedAttrs
   {-# NOINLINE[1] rule162 #-}
   rule162 = \ ((!_patIerrors) :: Seq Error) ->
     _patIerrors
   {-# NOINLINE[1] rule163 #-}
   rule163 = \ ((!_patIlocals) :: Set Identifier) ->
     _patIlocals
   {-# NOINLINE[1] rule164 #-}
   rule164 = \ ((!_patIcopy) :: Pattern) ->
     Irrefutable _patIcopy
   {-# NOINLINE[1] rule165 #-}
   rule165 = \ ((!_patIoutput) :: Pattern) ->
     Irrefutable _patIoutput
   {-# NOINLINE[1] rule166 #-}
   rule166 = \ !_copy ->
     _copy
   {-# NOINLINE[1] rule167 #-}
   rule167 = \ !_output ->
     _output
{-# NOINLINE sem_Pattern_Underscore #-}
sem_Pattern_Underscore :: (Pos) -> T_Pattern 
sem_Pattern_Underscore !arg_pos_ = T_Pattern (return st10) where
   {-# NOINLINE st10 #-}
   !st10 = let
      k10 :: K_Pattern_s10  t -> t
      k10 K_Pattern_v5 = v5
      k10 K_Pattern_v22 = v22
      k10 K_Pattern_v23 = v23
      k10 K_Pattern_v28 = v28
      k10 K_Pattern_v42 = v42
      k10 K_Pattern_v50 = v50
      v5 :: T_Pattern_v5 
      v5 = \ !(T_Pattern_vIn5 _lhsIcon _lhsInt) -> (
         let _lhsOcontainsVars :: Bool
             !_lhsOcontainsVars = rule170  () in
         let _lhsOdefinedAttrs :: Set (Identifier,Identifier)
             !_lhsOdefinedAttrs = rule171  () in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule172  () in
         let _lhsOlocals :: Set Identifier
             !_lhsOlocals = rule173  () in
         let !_copy = rule174 arg_pos_ in
         let _lhsOcopy :: Pattern
             !_lhsOcopy = rule176 _copy in
         let !_output = rule175 arg_pos_ in
         let _lhsOoutput :: Pattern
             !_lhsOoutput = rule177 _output in
         let !__result_ = T_Pattern_vOut5 _lhsOcontainsVars _lhsOcopy _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput
          in __result_ )
      v22 :: T_Pattern_v22 
      v22 = \ !(T_Pattern_vIn22 ) -> (
         let _lhsOdefinedAttrs :: Set (Identifier,Identifier)
             !_lhsOdefinedAttrs = rule171  () in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule172  () in
         let _lhsOlocals :: Set Identifier
             !_lhsOlocals = rule173  () in
         let !_copy = rule174 arg_pos_ in
         let _lhsOcopy :: Pattern
             !_lhsOcopy = rule176 _copy in
         let !_output = rule175 arg_pos_ in
         let _lhsOoutput :: Pattern
             !_lhsOoutput = rule177 _output in
         let !__result_ = T_Pattern_vOut22 _lhsOcopy _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput
          in __result_ )
      v23 :: T_Pattern_v23 
      v23 = \ !(T_Pattern_vIn23 ) -> (
         let _lhsOcontainsVars :: Bool
             !_lhsOcontainsVars = rule170  () in
         let _lhsOdefinedAttrs :: Set (Identifier,Identifier)
             !_lhsOdefinedAttrs = rule171  () in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule172  () in
         let _lhsOlocals :: Set Identifier
             !_lhsOlocals = rule173  () in
         let !_copy = rule174 arg_pos_ in
         let _lhsOcopy :: Pattern
             !_lhsOcopy = rule176 _copy in
         let !_output = rule175 arg_pos_ in
         let _lhsOoutput :: Pattern
             !_lhsOoutput = rule177 _output in
         let !__result_ = T_Pattern_vOut23 _lhsOcontainsVars _lhsOcopy _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput
          in __result_ )
      v28 :: T_Pattern_v28 
      v28 = \ !(T_Pattern_vIn28 ) -> (
         let _lhsOcontainsVars :: Bool
             !_lhsOcontainsVars = rule170  () in
         let _lhsOdefinedAttrs :: Set (Identifier,Identifier)
             !_lhsOdefinedAttrs = rule171  () in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule172  () in
         let _lhsOlocals :: Set Identifier
             !_lhsOlocals = rule173  () in
         let !_output = rule175 arg_pos_ in
         let _lhsOoutput :: Pattern
             !_lhsOoutput = rule177 _output in
         let !__result_ = T_Pattern_vOut28 _lhsOcontainsVars _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput
          in __result_ )
      v42 :: T_Pattern_v42 
      v42 = \ !(T_Pattern_vIn42 ) -> (
         let _lhsOdefinedAttrs :: Set (Identifier,Identifier)
             !_lhsOdefinedAttrs = rule171  () in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule172  () in
         let _lhsOlocals :: Set Identifier
             !_lhsOlocals = rule173  () in
         let !_output = rule175 arg_pos_ in
         let _lhsOoutput :: Pattern
             !_lhsOoutput = rule177 _output in
         let !__result_ = T_Pattern_vOut42 _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput
          in __result_ )
      v50 :: T_Pattern_v50 
      v50 = \ !(T_Pattern_vIn50 ) -> (
         let _lhsOdefinedAttrs :: Set (Identifier,Identifier)
             !_lhsOdefinedAttrs = rule171  () in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule172  () in
         let _lhsOlocals :: Set Identifier
             !_lhsOlocals = rule173  () in
         let !__st_ = st55  ()
             !__result_ = T_Pattern_vOut50 _lhsOdefinedAttrs _lhsOerrors _lhsOlocals __st_
          in __result_ )
     in C_Pattern_s10 k10
   {-# NOINLINE st55 #-}
   st55 = \  (_ :: ()) -> let
      k55 :: K_Pattern_s55  t -> t
      k55 K_Pattern_v51 = v51
      k55 K_Pattern_v56 = v56
      v51 :: T_Pattern_v51 
      v51 = \ !(T_Pattern_vIn51 ) -> (
         let _lhsOcontainsVars :: Bool
             !_lhsOcontainsVars = rule170  () in
         let !_output = rule175 arg_pos_ in
         let _lhsOoutput :: Pattern
             !_lhsOoutput = rule177 _output in
         let !__result_ = T_Pattern_vOut51 _lhsOcontainsVars _lhsOoutput
          in __result_ )
      v56 :: T_Pattern_v56 
      v56 = \ !(T_Pattern_vIn56 ) -> (
         let !_output = rule175 arg_pos_ in
         let _lhsOoutput :: Pattern
             !_lhsOoutput = rule177 _output in
         let !__result_ = T_Pattern_vOut56 _lhsOoutput
          in __result_ )
     in C_Pattern_s55 k55
   {-# NOINLINE[1] rule170 #-}
   rule170 = \  (_ :: ()) ->
     False
   {-# NOINLINE[1] rule171 #-}
   rule171 = \  (_ :: ()) ->
     Set.empty
   {-# NOINLINE[1] rule172 #-}
   rule172 = \  (_ :: ()) ->
     Seq.empty
   {-# NOINLINE[1] rule173 #-}
   rule173 = \  (_ :: ()) ->
     Set.empty
   {-# NOINLINE[1] rule174 #-}
   rule174 = \ !pos_ ->
     Underscore pos_
   {-# NOINLINE[1] rule175 #-}
   rule175 = \ !pos_ ->
     Underscore pos_
   {-# NOINLINE[1] rule176 #-}
   rule176 = \ !_copy ->
     _copy
   {-# NOINLINE[1] rule177 #-}
   rule177 = \ !_output ->
     _output

-- Patterns ----------------------------------------------------
-- wrapper
data Inh_Patterns  = Inh_Patterns { con_Inh_Patterns :: !(ConstructorIdent), nt_Inh_Patterns :: !(NontermIdent) }
data Syn_Patterns  = Syn_Patterns { containsVars_Syn_Patterns :: !(Bool), copy_Syn_Patterns :: !(Patterns), definedAttrs_Syn_Patterns :: !(Set (Identifier,Identifier)), errors_Syn_Patterns :: !(Seq Error), locals_Syn_Patterns :: !(Set Identifier), output_Syn_Patterns :: !(Patterns) }
{-# INLINABLE wrap_Patterns #-}
wrap_Patterns :: T_Patterns  -> Inh_Patterns  -> (Syn_Patterns )
wrap_Patterns !(T_Patterns act) !(Inh_Patterns _lhsIcon _lhsInt) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_Patterns_vIn6 _lhsIcon _lhsInt
        !(T_Patterns_vOut6 _lhsOcontainsVars _lhsOcopy _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput) <- return (inv_Patterns_s12 sem K_Patterns_v6 arg)
        return (Syn_Patterns _lhsOcontainsVars _lhsOcopy _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput)
   )

-- cata
{-# NOINLINE sem_Patterns #-}
sem_Patterns :: Patterns  -> T_Patterns 
sem_Patterns list = Prelude.foldr sem_Patterns_Cons sem_Patterns_Nil (Prelude.map sem_Pattern list)

-- semantic domain
newtype T_Patterns  = T_Patterns {
                                 attach_T_Patterns :: Identity (T_Patterns_s12 )
                                 }
data T_Patterns_s12  where C_Patterns_s12 :: {
                                             inv_Patterns_s12 :: !(forall t. K_Patterns_s12  t -> t)
                                             } -> T_Patterns_s12 
data T_Patterns_s13  = C_Patterns_s13
data T_Patterns_s34  = C_Patterns_s34
data T_Patterns_s47  = C_Patterns_s47
data T_Patterns_s49  = C_Patterns_s49
data T_Patterns_s54  = C_Patterns_s54
data T_Patterns_s57  where C_Patterns_s57 :: {
                                             inv_Patterns_s57 :: !(forall t. K_Patterns_s57  t -> t)
                                             } -> T_Patterns_s57 
data K_Patterns_s12 k  where
   K_Patterns_v6 :: K_Patterns_s12  (T_Patterns_v6 )
   K_Patterns_v21 :: K_Patterns_s12  (T_Patterns_v21 )
   K_Patterns_v38 :: K_Patterns_s12  (T_Patterns_v38 )
   K_Patterns_v41 :: K_Patterns_s12  (T_Patterns_v41 )
   K_Patterns_v49 :: K_Patterns_s12  (T_Patterns_v49 )
   K_Patterns_v54 :: K_Patterns_s12  (T_Patterns_v54 )
data K_Patterns_s57 k  where
   K_Patterns_v55 :: K_Patterns_s57  (T_Patterns_v55 )
   K_Patterns_v57 :: K_Patterns_s57  (T_Patterns_v57 )
type T_Patterns_v6  = (T_Patterns_vIn6 ) -> (T_Patterns_vOut6 )
data T_Patterns_vIn6  = T_Patterns_vIn6 !(ConstructorIdent) !(NontermIdent)
data T_Patterns_vOut6  = T_Patterns_vOut6 !(Bool) !(Patterns) !(Set (Identifier,Identifier)) !(Seq Error) !(Set Identifier) !(Patterns)
type T_Patterns_v21  = (T_Patterns_vIn21 ) -> (T_Patterns_vOut21 )
data T_Patterns_vIn21  = T_Patterns_vIn21 
data T_Patterns_vOut21  = T_Patterns_vOut21 !(Bool) !(Patterns) !(Set (Identifier,Identifier)) !(Seq Error) !(Set Identifier) !(Patterns)
type T_Patterns_v38  = (T_Patterns_vIn38 ) -> (T_Patterns_vOut38 )
data T_Patterns_vIn38  = T_Patterns_vIn38 
data T_Patterns_vOut38  = T_Patterns_vOut38 !(Patterns) !(Set (Identifier,Identifier)) !(Seq Error) !(Set Identifier) !(Patterns)
type T_Patterns_v41  = (T_Patterns_vIn41 ) -> (T_Patterns_vOut41 )
data T_Patterns_vIn41  = T_Patterns_vIn41 
data T_Patterns_vOut41  = T_Patterns_vOut41 !(Bool) !(Set (Identifier,Identifier)) !(Seq Error) !(Set Identifier) !(Patterns)
type T_Patterns_v49  = (T_Patterns_vIn49 ) -> (T_Patterns_vOut49 )
data T_Patterns_vIn49  = T_Patterns_vIn49 
data T_Patterns_vOut49  = T_Patterns_vOut49 !(Set (Identifier,Identifier)) !(Seq Error) !(Set Identifier) !(Patterns)
type T_Patterns_v54  = (T_Patterns_vIn54 ) -> (T_Patterns_vOut54 )
data T_Patterns_vIn54  = T_Patterns_vIn54 
data T_Patterns_vOut54  = T_Patterns_vOut54 !(Set (Identifier,Identifier)) !(Seq Error) !(Set Identifier) !(T_Patterns_s57 )
type T_Patterns_v55  = (T_Patterns_vIn55 ) -> (T_Patterns_vOut55 )
data T_Patterns_vIn55  = T_Patterns_vIn55 
data T_Patterns_vOut55  = T_Patterns_vOut55 !(Bool) !(Patterns)
type T_Patterns_v57  = (T_Patterns_vIn57 ) -> (T_Patterns_vOut57 )
data T_Patterns_vIn57  = T_Patterns_vIn57 
data T_Patterns_vOut57  = T_Patterns_vOut57 !(Patterns)
{-# NOINLINE sem_Patterns_Cons #-}
sem_Patterns_Cons :: T_Pattern  -> T_Patterns  -> T_Patterns 
sem_Patterns_Cons arg_hd_ arg_tl_ = T_Patterns (return st12) where
   {-# NOINLINE st12 #-}
   !st12 = let
      k12 :: K_Patterns_s12  t -> t
      k12 K_Patterns_v6 = v6
      k12 K_Patterns_v21 = v21
      k12 K_Patterns_v38 = v38
      k12 K_Patterns_v41 = v41
      k12 K_Patterns_v49 = v49
      k12 K_Patterns_v54 = v54
      v6 :: T_Patterns_v6 
      v6 = \ !(T_Patterns_vIn6 _lhsIcon _lhsInt) -> (
         let !_hdX10 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_hd_)) in
         let !_tlX12 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_tl_)) in
         let !(T_Pattern_vOut23 _hdIcontainsVars _hdIcopy _hdIdefinedAttrs _hdIerrors _hdIlocals _hdIoutput) = inv_Pattern_s10 _hdX10 K_Pattern_v23 (T_Pattern_vIn23 ) in
         let !(T_Patterns_vOut21 _tlIcontainsVars _tlIcopy _tlIdefinedAttrs _tlIerrors _tlIlocals _tlIoutput) = inv_Patterns_s12 _tlX12 K_Patterns_v21 (T_Patterns_vIn21 ) in
         let _lhsOcontainsVars :: Bool
             !_lhsOcontainsVars = rule178 _hdIcontainsVars _tlIcontainsVars in
         let !_copy = rule182 _hdIcopy _tlIcopy in
         let _lhsOcopy :: Patterns
             !_lhsOcopy = rule184 _copy in
         let _lhsOdefinedAttrs :: Set (Identifier,Identifier)
             !_lhsOdefinedAttrs = rule179 _hdIdefinedAttrs _tlIdefinedAttrs in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule180 _hdIerrors _tlIerrors in
         let _lhsOlocals :: Set Identifier
             !_lhsOlocals = rule181 _hdIlocals _tlIlocals in
         let !_output = rule183 _hdIoutput _tlIoutput in
         let _lhsOoutput :: Patterns
             !_lhsOoutput = rule185 _output in
         let !__result_ = T_Patterns_vOut6 _lhsOcontainsVars _lhsOcopy _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput
          in __result_ )
      v21 :: T_Patterns_v21 
      v21 = \ !(T_Patterns_vIn21 ) -> (
         let !_hdX10 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_hd_)) in
         let !_tlX12 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_tl_)) in
         let !(T_Pattern_vOut23 _hdIcontainsVars _hdIcopy _hdIdefinedAttrs _hdIerrors _hdIlocals _hdIoutput) = inv_Pattern_s10 _hdX10 K_Pattern_v23 (T_Pattern_vIn23 ) in
         let !(T_Patterns_vOut21 _tlIcontainsVars _tlIcopy _tlIdefinedAttrs _tlIerrors _tlIlocals _tlIoutput) = inv_Patterns_s12 _tlX12 K_Patterns_v21 (T_Patterns_vIn21 ) in
         let _lhsOcontainsVars :: Bool
             !_lhsOcontainsVars = rule178 _hdIcontainsVars _tlIcontainsVars in
         let !_copy = rule182 _hdIcopy _tlIcopy in
         let _lhsOcopy :: Patterns
             !_lhsOcopy = rule184 _copy in
         let _lhsOdefinedAttrs :: Set (Identifier,Identifier)
             !_lhsOdefinedAttrs = rule179 _hdIdefinedAttrs _tlIdefinedAttrs in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule180 _hdIerrors _tlIerrors in
         let _lhsOlocals :: Set Identifier
             !_lhsOlocals = rule181 _hdIlocals _tlIlocals in
         let !_output = rule183 _hdIoutput _tlIoutput in
         let _lhsOoutput :: Patterns
             !_lhsOoutput = rule185 _output in
         let !__result_ = T_Patterns_vOut21 _lhsOcontainsVars _lhsOcopy _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput
          in __result_ )
      v38 :: T_Patterns_v38 
      v38 = \ !(T_Patterns_vIn38 ) -> (
         let !_hdX10 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_hd_)) in
         let !_tlX12 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_tl_)) in
         let !(T_Pattern_vOut22 _hdIcopy _hdIdefinedAttrs _hdIerrors _hdIlocals _hdIoutput) = inv_Pattern_s10 _hdX10 K_Pattern_v22 (T_Pattern_vIn22 ) in
         let !(T_Patterns_vOut38 _tlIcopy _tlIdefinedAttrs _tlIerrors _tlIlocals _tlIoutput) = inv_Patterns_s12 _tlX12 K_Patterns_v38 (T_Patterns_vIn38 ) in
         let !_copy = rule182 _hdIcopy _tlIcopy in
         let _lhsOcopy :: Patterns
             !_lhsOcopy = rule184 _copy in
         let _lhsOdefinedAttrs :: Set (Identifier,Identifier)
             !_lhsOdefinedAttrs = rule179 _hdIdefinedAttrs _tlIdefinedAttrs in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule180 _hdIerrors _tlIerrors in
         let _lhsOlocals :: Set Identifier
             !_lhsOlocals = rule181 _hdIlocals _tlIlocals in
         let !_output = rule183 _hdIoutput _tlIoutput in
         let _lhsOoutput :: Patterns
             !_lhsOoutput = rule185 _output in
         let !__result_ = T_Patterns_vOut38 _lhsOcopy _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput
          in __result_ )
      v41 :: T_Patterns_v41 
      v41 = \ !(T_Patterns_vIn41 ) -> (
         let !_hdX10 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_hd_)) in
         let !_tlX12 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_tl_)) in
         let !(T_Pattern_vOut28 _hdIcontainsVars _hdIdefinedAttrs _hdIerrors _hdIlocals _hdIoutput) = inv_Pattern_s10 _hdX10 K_Pattern_v28 (T_Pattern_vIn28 ) in
         let !(T_Patterns_vOut41 _tlIcontainsVars _tlIdefinedAttrs _tlIerrors _tlIlocals _tlIoutput) = inv_Patterns_s12 _tlX12 K_Patterns_v41 (T_Patterns_vIn41 ) in
         let _lhsOcontainsVars :: Bool
             !_lhsOcontainsVars = rule178 _hdIcontainsVars _tlIcontainsVars in
         let _lhsOdefinedAttrs :: Set (Identifier,Identifier)
             !_lhsOdefinedAttrs = rule179 _hdIdefinedAttrs _tlIdefinedAttrs in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule180 _hdIerrors _tlIerrors in
         let _lhsOlocals :: Set Identifier
             !_lhsOlocals = rule181 _hdIlocals _tlIlocals in
         let !_output = rule183 _hdIoutput _tlIoutput in
         let _lhsOoutput :: Patterns
             !_lhsOoutput = rule185 _output in
         let !__result_ = T_Patterns_vOut41 _lhsOcontainsVars _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput
          in __result_ )
      v49 :: T_Patterns_v49 
      v49 = \ !(T_Patterns_vIn49 ) -> (
         let !_hdX10 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_hd_)) in
         let !_tlX12 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_tl_)) in
         let !(T_Pattern_vOut42 _hdIdefinedAttrs _hdIerrors _hdIlocals _hdIoutput) = inv_Pattern_s10 _hdX10 K_Pattern_v42 (T_Pattern_vIn42 ) in
         let !(T_Patterns_vOut49 _tlIdefinedAttrs _tlIerrors _tlIlocals _tlIoutput) = inv_Patterns_s12 _tlX12 K_Patterns_v49 (T_Patterns_vIn49 ) in
         let _lhsOdefinedAttrs :: Set (Identifier,Identifier)
             !_lhsOdefinedAttrs = rule179 _hdIdefinedAttrs _tlIdefinedAttrs in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule180 _hdIerrors _tlIerrors in
         let _lhsOlocals :: Set Identifier
             !_lhsOlocals = rule181 _hdIlocals _tlIlocals in
         let !_output = rule183 _hdIoutput _tlIoutput in
         let _lhsOoutput :: Patterns
             !_lhsOoutput = rule185 _output in
         let !__result_ = T_Patterns_vOut49 _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput
          in __result_ )
      v54 :: T_Patterns_v54 
      v54 = \ !(T_Patterns_vIn54 ) -> (
         let !_hdX10 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_hd_)) in
         let !_tlX12 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_tl_)) in
         let !(T_Pattern_vOut50 _hdIdefinedAttrs _hdIerrors _hdIlocals _hdX55) = inv_Pattern_s10 _hdX10 K_Pattern_v50 (T_Pattern_vIn50 ) in
         let !(T_Patterns_vOut54 _tlIdefinedAttrs _tlIerrors _tlIlocals _tlX57) = inv_Patterns_s12 _tlX12 K_Patterns_v54 (T_Patterns_vIn54 ) in
         let _lhsOdefinedAttrs :: Set (Identifier,Identifier)
             !_lhsOdefinedAttrs = rule179 _hdIdefinedAttrs _tlIdefinedAttrs in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule180 _hdIerrors _tlIerrors in
         let _lhsOlocals :: Set Identifier
             !_lhsOlocals = rule181 _hdIlocals _tlIlocals in
         let !__st_ = st57 _hdX55 _tlX57
             !__result_ = T_Patterns_vOut54 _lhsOdefinedAttrs _lhsOerrors _lhsOlocals __st_
          in __result_ )
     in C_Patterns_s12 k12
   {-# NOINLINE st57 #-}
   st57 = \ !_hdX55 !_tlX57 -> let
      k57 :: K_Patterns_s57  t -> t
      k57 K_Patterns_v55 = v55
      k57 K_Patterns_v57 = v57
      v55 :: T_Patterns_v55 
      v55 = \ !(T_Patterns_vIn55 ) -> (
         let !(T_Pattern_vOut51 _hdIcontainsVars _hdIoutput) = inv_Pattern_s55 _hdX55 K_Pattern_v51 (T_Pattern_vIn51 ) in
         let !(T_Patterns_vOut55 _tlIcontainsVars _tlIoutput) = inv_Patterns_s57 _tlX57 K_Patterns_v55 (T_Patterns_vIn55 ) in
         let _lhsOcontainsVars :: Bool
             !_lhsOcontainsVars = rule178 _hdIcontainsVars _tlIcontainsVars in
         let !_output = rule183 _hdIoutput _tlIoutput in
         let _lhsOoutput :: Patterns
             !_lhsOoutput = rule185 _output in
         let !__result_ = T_Patterns_vOut55 _lhsOcontainsVars _lhsOoutput
          in __result_ )
      v57 :: T_Patterns_v57 
      v57 = \ !(T_Patterns_vIn57 ) -> (
         let !(T_Pattern_vOut56 _hdIoutput) = inv_Pattern_s55 _hdX55 K_Pattern_v56 (T_Pattern_vIn56 ) in
         let !(T_Patterns_vOut57 _tlIoutput) = inv_Patterns_s57 _tlX57 K_Patterns_v57 (T_Patterns_vIn57 ) in
         let !_output = rule183 _hdIoutput _tlIoutput in
         let _lhsOoutput :: Patterns
             !_lhsOoutput = rule185 _output in
         let !__result_ = T_Patterns_vOut57 _lhsOoutput
          in __result_ )
     in C_Patterns_s57 k57
   {-# NOINLINE[1] rule178 #-}
   rule178 = \ ((!_hdIcontainsVars) :: Bool) ((!_tlIcontainsVars) :: Bool) ->
     _hdIcontainsVars || _tlIcontainsVars
   {-# NOINLINE[1] rule179 #-}
   rule179 = \ ((!_hdIdefinedAttrs) :: Set (Identifier,Identifier)) ((!_tlIdefinedAttrs) :: Set (Identifier,Identifier)) ->
     _hdIdefinedAttrs `Set.union` _tlIdefinedAttrs
   {-# NOINLINE[1] rule180 #-}
   rule180 = \ ((!_hdIerrors) :: Seq Error) ((!_tlIerrors) :: Seq Error) ->
     _hdIerrors Seq.>< _tlIerrors
   {-# NOINLINE[1] rule181 #-}
   rule181 = \ ((!_hdIlocals) :: Set Identifier) ((!_tlIlocals) :: Set Identifier) ->
     _hdIlocals `Set.union` _tlIlocals
   {-# NOINLINE[1] rule182 #-}
   rule182 = \ ((!_hdIcopy) :: Pattern) ((!_tlIcopy) :: Patterns) ->
     (:) _hdIcopy _tlIcopy
   {-# NOINLINE[1] rule183 #-}
   rule183 = \ ((!_hdIoutput) :: Pattern) ((!_tlIoutput) :: Patterns) ->
     (:) _hdIoutput _tlIoutput
   {-# NOINLINE[1] rule184 #-}
   rule184 = \ !_copy ->
     _copy
   {-# NOINLINE[1] rule185 #-}
   rule185 = \ !_output ->
     _output
{-# NOINLINE sem_Patterns_Nil #-}
sem_Patterns_Nil ::  T_Patterns 
sem_Patterns_Nil  = T_Patterns (return st12) where
   {-# NOINLINE st12 #-}
   !st12 = let
      k12 :: K_Patterns_s12  t -> t
      k12 K_Patterns_v6 = v6
      k12 K_Patterns_v21 = v21
      k12 K_Patterns_v38 = v38
      k12 K_Patterns_v41 = v41
      k12 K_Patterns_v49 = v49
      k12 K_Patterns_v54 = v54
      v6 :: T_Patterns_v6 
      v6 = \ !(T_Patterns_vIn6 _lhsIcon _lhsInt) -> (
         let _lhsOcontainsVars :: Bool
             !_lhsOcontainsVars = rule190  () in
         let !_copy = rule194  () in
         let _lhsOdefinedAttrs :: Set (Identifier,Identifier)
             !_lhsOdefinedAttrs = rule191  () in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule192  () in
         let _lhsOlocals :: Set Identifier
             !_lhsOlocals = rule193  () in
         let !_output = rule195  () in
         let _lhsOcopy :: Patterns
             !_lhsOcopy = rule196 _copy in
         let _lhsOoutput :: Patterns
             !_lhsOoutput = rule197 _output in
         let !__result_ = T_Patterns_vOut6 _lhsOcontainsVars _lhsOcopy _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput
          in __result_ )
      v21 :: T_Patterns_v21 
      v21 = \ !(T_Patterns_vIn21 ) -> (
         let _lhsOcontainsVars :: Bool
             !_lhsOcontainsVars = rule190  () in
         let !_copy = rule194  () in
         let _lhsOdefinedAttrs :: Set (Identifier,Identifier)
             !_lhsOdefinedAttrs = rule191  () in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule192  () in
         let _lhsOlocals :: Set Identifier
             !_lhsOlocals = rule193  () in
         let !_output = rule195  () in
         let _lhsOcopy :: Patterns
             !_lhsOcopy = rule196 _copy in
         let _lhsOoutput :: Patterns
             !_lhsOoutput = rule197 _output in
         let !__result_ = T_Patterns_vOut21 _lhsOcontainsVars _lhsOcopy _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput
          in __result_ )
      v38 :: T_Patterns_v38 
      v38 = \ !(T_Patterns_vIn38 ) -> (
         let !_copy = rule194  () in
         let _lhsOdefinedAttrs :: Set (Identifier,Identifier)
             !_lhsOdefinedAttrs = rule191  () in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule192  () in
         let _lhsOlocals :: Set Identifier
             !_lhsOlocals = rule193  () in
         let !_output = rule195  () in
         let _lhsOcopy :: Patterns
             !_lhsOcopy = rule196 _copy in
         let _lhsOoutput :: Patterns
             !_lhsOoutput = rule197 _output in
         let !__result_ = T_Patterns_vOut38 _lhsOcopy _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput
          in __result_ )
      v41 :: T_Patterns_v41 
      v41 = \ !(T_Patterns_vIn41 ) -> (
         let _lhsOcontainsVars :: Bool
             !_lhsOcontainsVars = rule190  () in
         let _lhsOdefinedAttrs :: Set (Identifier,Identifier)
             !_lhsOdefinedAttrs = rule191  () in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule192  () in
         let _lhsOlocals :: Set Identifier
             !_lhsOlocals = rule193  () in
         let !_output = rule195  () in
         let _lhsOoutput :: Patterns
             !_lhsOoutput = rule197 _output in
         let !__result_ = T_Patterns_vOut41 _lhsOcontainsVars _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput
          in __result_ )
      v49 :: T_Patterns_v49 
      v49 = \ !(T_Patterns_vIn49 ) -> (
         let _lhsOdefinedAttrs :: Set (Identifier,Identifier)
             !_lhsOdefinedAttrs = rule191  () in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule192  () in
         let _lhsOlocals :: Set Identifier
             !_lhsOlocals = rule193  () in
         let !_output = rule195  () in
         let _lhsOoutput :: Patterns
             !_lhsOoutput = rule197 _output in
         let !__result_ = T_Patterns_vOut49 _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput
          in __result_ )
      v54 :: T_Patterns_v54 
      v54 = \ !(T_Patterns_vIn54 ) -> (
         let _lhsOdefinedAttrs :: Set (Identifier,Identifier)
             !_lhsOdefinedAttrs = rule191  () in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule192  () in
         let _lhsOlocals :: Set Identifier
             !_lhsOlocals = rule193  () in
         let !__st_ = st57  ()
             !__result_ = T_Patterns_vOut54 _lhsOdefinedAttrs _lhsOerrors _lhsOlocals __st_
          in __result_ )
     in C_Patterns_s12 k12
   {-# NOINLINE st57 #-}
   st57 = \  (_ :: ()) -> let
      k57 :: K_Patterns_s57  t -> t
      k57 K_Patterns_v55 = v55
      k57 K_Patterns_v57 = v57
      v55 :: T_Patterns_v55 
      v55 = \ !(T_Patterns_vIn55 ) -> (
         let _lhsOcontainsVars :: Bool
             !_lhsOcontainsVars = rule190  () in
         let !_output = rule195  () in
         let _lhsOoutput :: Patterns
             !_lhsOoutput = rule197 _output in
         let !__result_ = T_Patterns_vOut55 _lhsOcontainsVars _lhsOoutput
          in __result_ )
      v57 :: T_Patterns_v57 
      v57 = \ !(T_Patterns_vIn57 ) -> (
         let !_output = rule195  () in
         let _lhsOoutput :: Patterns
             !_lhsOoutput = rule197 _output in
         let !__result_ = T_Patterns_vOut57 _lhsOoutput
          in __result_ )
     in C_Patterns_s57 k57
   {-# NOINLINE[1] rule190 #-}
   rule190 = \  (_ :: ()) ->
     False
   {-# NOINLINE[1] rule191 #-}
   rule191 = \  (_ :: ()) ->
     Set.empty
   {-# NOINLINE[1] rule192 #-}
   rule192 = \  (_ :: ()) ->
     Seq.empty
   {-# NOINLINE[1] rule193 #-}
   rule193 = \  (_ :: ()) ->
     Set.empty
   {-# NOINLINE[1] rule194 #-}
   rule194 = \  (_ :: ()) ->
     []
   {-# NOINLINE[1] rule195 #-}
   rule195 = \  (_ :: ()) ->
     []
   {-# NOINLINE[1] rule196 #-}
   rule196 = \ !_copy ->
     _copy
   {-# NOINLINE[1] rule197 #-}
   rule197 = \ !_output ->
     _output

-- Production --------------------------------------------------
-- wrapper
data Inh_Production  = Inh_Production { aroundsIn_Inh_Production :: !(Map ConstructorIdent (Map Identifier [Expression])), augmentsIn_Inh_Production :: !(Map ConstructorIdent (Map Identifier [Expression])), cr_Inh_Production :: !(Bool), inh_Inh_Production :: !(Attributes), inhMap_Inh_Production :: !(Map Identifier Attributes), inhOrig_Inh_Production :: !(Attributes), manualAttrOrderMap_Inh_Production :: !(AttrOrderMap), mergesIn_Inh_Production :: !(Map ConstructorIdent (Map Identifier (Identifier,[Identifier],Expression))), nonterminals_Inh_Production :: !(Set NontermIdent), nt_Inh_Production :: !(NontermIdent), o_rename_Inh_Production :: !(Bool), options_Inh_Production :: !(Options), params_Inh_Production :: !([Identifier]), syn_Inh_Production :: !(Attributes), synMap_Inh_Production :: !(Map Identifier Attributes), synOrig_Inh_Production :: !(Attributes), typeSyns_Inh_Production :: !(TypeSyns), uniq_Inh_Production :: !(Int), useMap_Inh_Production :: !(Map Identifier (String,String,String)), wrappers_Inh_Production :: !(Set NontermIdent) }
data Syn_Production  = Syn_Production { errors_Syn_Production :: !(Seq Error), output_Syn_Production :: !(Production), uniq_Syn_Production :: !(Int) }
{-# INLINABLE wrap_Production #-}
wrap_Production :: T_Production  -> Inh_Production  -> (Syn_Production )
wrap_Production !(T_Production act) !(Inh_Production _lhsIaroundsIn _lhsIaugmentsIn _lhsIcr _lhsIinh _lhsIinhMap _lhsIinhOrig _lhsImanualAttrOrderMap _lhsImergesIn _lhsInonterminals _lhsInt _lhsIo_rename _lhsIoptions _lhsIparams _lhsIsyn _lhsIsynMap _lhsIsynOrig _lhsItypeSyns _lhsIuniq _lhsIuseMap _lhsIwrappers) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_Production_vIn7 _lhsIaroundsIn _lhsIaugmentsIn _lhsIcr _lhsIinh _lhsIinhMap _lhsIinhOrig _lhsImanualAttrOrderMap _lhsImergesIn _lhsInonterminals _lhsInt _lhsIo_rename _lhsIoptions _lhsIparams _lhsIsyn _lhsIsynMap _lhsIsynOrig _lhsItypeSyns _lhsIuniq _lhsIuseMap _lhsIwrappers
        !(T_Production_vOut7 _lhsOerrors _lhsOoutput _lhsOuniq) <- return (inv_Production_s14 sem K_Production_v7 arg)
        return (Syn_Production _lhsOerrors _lhsOoutput _lhsOuniq)
   )

-- cata
{-# INLINE sem_Production #-}
sem_Production :: Production  -> T_Production 
sem_Production ( Production !con_ !params_ !constraints_ children_ rules_ typeSigs_ !macro_ ) = sem_Production_Production con_ params_ constraints_ ( sem_Children children_ ) ( sem_Rules rules_ ) ( sem_TypeSigs typeSigs_ ) macro_

-- semantic domain
newtype T_Production  = T_Production {
                                     attach_T_Production :: Identity (T_Production_s14 )
                                     }
data T_Production_s14  where C_Production_s14 :: {
                                                 inv_Production_s14 :: !(forall t. K_Production_s14  t -> t)
                                                 } -> T_Production_s14 
data T_Production_s15  = C_Production_s15
data T_Production_s38  = C_Production_s38
newtype T_Production_s48  = C_Production_s48 {
                                             inv_Production_s48 :: (T_Production_v40 )
                                             }
data K_Production_s14 k  where
   K_Production_v7 :: K_Production_s14  (T_Production_v7 )
   K_Production_v25 :: K_Production_s14  (T_Production_v25 )
   K_Production_v39 :: K_Production_s14  (T_Production_v39 )
type T_Production_v7  = (T_Production_vIn7 ) -> (T_Production_vOut7 )
data T_Production_vIn7  = T_Production_vIn7 !(Map ConstructorIdent (Map Identifier [Expression])) !(Map ConstructorIdent (Map Identifier [Expression])) !(Bool) !(Attributes) !(Map Identifier Attributes) !(Attributes) !(AttrOrderMap) !(Map ConstructorIdent (Map Identifier (Identifier,[Identifier],Expression))) !(Set NontermIdent) !(NontermIdent) !(Bool) !(Options) !([Identifier]) !(Attributes) !(Map Identifier Attributes) !(Attributes) !(TypeSyns) !(Int) !(Map Identifier (String,String,String)) !(Set NontermIdent)
data T_Production_vOut7  = T_Production_vOut7 !(Seq Error) !(Production) !(Int)
type T_Production_v25  = (T_Production_vIn25 ) -> (T_Production_vOut25 )
data T_Production_vIn25  = T_Production_vIn25 !(Map ConstructorIdent (Map Identifier [Expression])) !(Map ConstructorIdent (Map Identifier [Expression])) !(Bool) !(Attributes) !(Map Identifier Attributes) !(AttrOrderMap) !(Map ConstructorIdent (Map Identifier (Identifier,[Identifier],Expression))) !(NontermIdent) !(Bool) !(Options) !([Identifier]) !(Attributes) !(Map Identifier Attributes) !(Attributes) !(TypeSyns) !(Int) !(Map Identifier (String,String,String)) !(Set NontermIdent)
data T_Production_vOut25  = T_Production_vOut25 !(Seq Error) !(Production) !(Int)
type T_Production_v39  = (T_Production_vIn39 ) -> (T_Production_vOut39 )
data T_Production_vIn39  = T_Production_vIn39 !(Bool) !(Attributes) !(Map Identifier Attributes) !(AttrOrderMap) !(Map ConstructorIdent (Map Identifier (Identifier,[Identifier],Expression))) !(NontermIdent) !(Bool) !(Options) !(Attributes) !(Map Identifier Attributes) !(Attributes) !(TypeSyns) !(Map Identifier (String,String,String)) !(Set NontermIdent)
data T_Production_vOut39  = T_Production_vOut39 !(Seq Error) !(T_Production_s48 )
type T_Production_v40  = (T_Production_vIn40 ) -> (T_Production_vOut40 )
data T_Production_vIn40  = T_Production_vIn40 !(Map ConstructorIdent (Map Identifier [Expression])) !(Map ConstructorIdent (Map Identifier [Expression])) !([Identifier]) !(Int)
data T_Production_vOut40  = T_Production_vOut40 !(Production) !(Int)
{-# NOINLINE sem_Production_Production #-}
sem_Production_Production :: (ConstructorIdent) -> ([Identifier]) -> ([Type]) -> T_Children  -> T_Rules  -> T_TypeSigs  -> (MaybeMacro) -> T_Production 
sem_Production_Production !arg_con_ !arg_params_ !arg_constraints_ arg_children_ arg_rules_ arg_typeSigs_ !arg_macro_ = T_Production (return st14) where
   {-# NOINLINE st14 #-}
   !st14 = let
      k14 :: K_Production_s14  t -> t
      k14 K_Production_v7 = v7
      k14 K_Production_v25 = v25
      k14 K_Production_v39 = v39
      v7 :: T_Production_v7 
      v7 = \ !(T_Production_vIn7 _lhsIaroundsIn _lhsIaugmentsIn _lhsIcr _lhsIinh _lhsIinhMap _lhsIinhOrig _lhsImanualAttrOrderMap _lhsImergesIn _lhsInonterminals _lhsInt _lhsIo_rename _lhsIoptions _lhsIparams _lhsIsyn _lhsIsynMap _lhsIsynOrig _lhsItypeSyns _lhsIuniq _lhsIuseMap _lhsIwrappers) -> (
         let !_childrenX2 = Control.Monad.Identity.runIdentity (attach_T_Children (arg_children_)) in
         let !_rulesX20 = Control.Monad.Identity.runIdentity (attach_T_Rules (arg_rules_)) in
         let !_typeSigsX24 = Control.Monad.Identity.runIdentity (attach_T_TypeSigs (arg_typeSigs_)) in
         let !_childrenOinhMap = rule215 _lhsIinhMap in
         let !_mergesIn = rule210 _lhsImergesIn arg_con_ in
         let !_merged = rule211 _mergesIn in
         let !_childrenOmerged = rule216 _merged in
         let !_childrenOsynMap = rule219 _lhsIsynMap in
         let !_orderDeps = rule206 _lhsImanualAttrOrderMap _lhsInt arg_con_ in
         let !_typeSigsOnt = rule223 _lhsInt in
         let !_typeSigsOparams = rule224 _lhsIparams in
         let !_aroundsIn = rule209 _lhsIaroundsIn arg_con_ in
         let !_rulesOoptions = rule221 _lhsIoptions in
         let !_rulesOuniq = rule222 _lhsIuniq in
         let !_augmentsIn = rule208 _lhsIaugmentsIn arg_con_ in
         let !(T_Children_vOut14 _childrenIerrors _childrenIfields _childrenIinputs _childrenIoutput _childrenIoutputs) = inv_Children_s2 _childrenX2 K_Children_v14 (T_Children_vIn14 _childrenOinhMap _childrenOmerged _childrenOsynMap) in
         let !(T_Rules_vOut24 _rulesIdefinedAttrs _rulesIerrors _rulesIlocals _rulesIoutput _rulesIruleNames _rulesIuniq) = inv_Rules_s20 _rulesX20 K_Rules_v24 (T_Rules_vIn24 _rulesOoptions _rulesOuniq) in
         let !(T_TypeSigs_vOut12 _typeSigsIoutput) = inv_TypeSigs_s24 _typeSigsX24 (T_TypeSigs_vIn12 _typeSigsOnt _typeSigsOparams) in
         let !(!_newRls,!_errs) = rule201 _childrenIfields _childrenIinputs _childrenIoutputs _lhsIcr _lhsIinh _lhsInt _lhsIo_rename _lhsIoptions _lhsIsyn _lhsIsynOrig _lhsItypeSyns _lhsIuseMap _lhsIwrappers _rulesIdefinedAttrs _rulesIlocals arg_con_ in
         let !_orderErrs = rule207 _childrenIinputs _childrenIoutputs _lhsIinh _lhsInt _lhsIsyn _orderDeps _rulesIlocals _rulesIruleNames arg_con_ in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule200 _childrenIerrors _errs _orderErrs _rulesIerrors in
         let !_extra1 = rule202 _augmentsIn _newRls _rulesIoutput in
         let !_extra2 = rule203 _aroundsIn _extra1 in
         let !_extra3 = rule204 _extra2 _mergesIn in
         let _lhsOoutput :: Production
             !_lhsOoutput = rule205 _childrenIoutput _extra3 _typeSigsIoutput arg_con_ arg_constraints_ arg_macro_ arg_params_ in
         let _lhsOuniq :: Int
             !_lhsOuniq = rule213 _rulesIuniq in
         let !__result_ = T_Production_vOut7 _lhsOerrors _lhsOoutput _lhsOuniq
          in __result_ )
      v25 :: T_Production_v25 
      v25 = \ !(T_Production_vIn25 _lhsIaroundsIn _lhsIaugmentsIn _lhsIcr _lhsIinh _lhsIinhMap _lhsImanualAttrOrderMap _lhsImergesIn _lhsInt _lhsIo_rename _lhsIoptions _lhsIparams _lhsIsyn _lhsIsynMap _lhsIsynOrig _lhsItypeSyns _lhsIuniq _lhsIuseMap _lhsIwrappers) -> (
         let !_childrenX2 = Control.Monad.Identity.runIdentity (attach_T_Children (arg_children_)) in
         let !_rulesX20 = Control.Monad.Identity.runIdentity (attach_T_Rules (arg_rules_)) in
         let !_typeSigsX24 = Control.Monad.Identity.runIdentity (attach_T_TypeSigs (arg_typeSigs_)) in
         let !_childrenOinhMap = rule215 _lhsIinhMap in
         let !_mergesIn = rule210 _lhsImergesIn arg_con_ in
         let !_merged = rule211 _mergesIn in
         let !_childrenOmerged = rule216 _merged in
         let !_childrenOsynMap = rule219 _lhsIsynMap in
         let !_orderDeps = rule206 _lhsImanualAttrOrderMap _lhsInt arg_con_ in
         let !_typeSigsOnt = rule223 _lhsInt in
         let !_typeSigsOparams = rule224 _lhsIparams in
         let !_aroundsIn = rule209 _lhsIaroundsIn arg_con_ in
         let !_rulesOoptions = rule221 _lhsIoptions in
         let !_rulesOuniq = rule222 _lhsIuniq in
         let !_augmentsIn = rule208 _lhsIaugmentsIn arg_con_ in
         let !(T_Children_vOut14 _childrenIerrors _childrenIfields _childrenIinputs _childrenIoutput _childrenIoutputs) = inv_Children_s2 _childrenX2 K_Children_v14 (T_Children_vIn14 _childrenOinhMap _childrenOmerged _childrenOsynMap) in
         let !(T_Rules_vOut24 _rulesIdefinedAttrs _rulesIerrors _rulesIlocals _rulesIoutput _rulesIruleNames _rulesIuniq) = inv_Rules_s20 _rulesX20 K_Rules_v24 (T_Rules_vIn24 _rulesOoptions _rulesOuniq) in
         let !(T_TypeSigs_vOut12 _typeSigsIoutput) = inv_TypeSigs_s24 _typeSigsX24 (T_TypeSigs_vIn12 _typeSigsOnt _typeSigsOparams) in
         let !(!_newRls,!_errs) = rule201 _childrenIfields _childrenIinputs _childrenIoutputs _lhsIcr _lhsIinh _lhsInt _lhsIo_rename _lhsIoptions _lhsIsyn _lhsIsynOrig _lhsItypeSyns _lhsIuseMap _lhsIwrappers _rulesIdefinedAttrs _rulesIlocals arg_con_ in
         let !_orderErrs = rule207 _childrenIinputs _childrenIoutputs _lhsIinh _lhsInt _lhsIsyn _orderDeps _rulesIlocals _rulesIruleNames arg_con_ in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule200 _childrenIerrors _errs _orderErrs _rulesIerrors in
         let !_extra1 = rule202 _augmentsIn _newRls _rulesIoutput in
         let !_extra2 = rule203 _aroundsIn _extra1 in
         let !_extra3 = rule204 _extra2 _mergesIn in
         let _lhsOoutput :: Production
             !_lhsOoutput = rule205 _childrenIoutput _extra3 _typeSigsIoutput arg_con_ arg_constraints_ arg_macro_ arg_params_ in
         let _lhsOuniq :: Int
             !_lhsOuniq = rule213 _rulesIuniq in
         let !__result_ = T_Production_vOut25 _lhsOerrors _lhsOoutput _lhsOuniq
          in __result_ )
      v39 :: T_Production_v39 
      v39 = \ !(T_Production_vIn39 _lhsIcr _lhsIinh _lhsIinhMap _lhsImanualAttrOrderMap _lhsImergesIn _lhsInt _lhsIo_rename _lhsIoptions _lhsIsyn _lhsIsynMap _lhsIsynOrig _lhsItypeSyns _lhsIuseMap _lhsIwrappers) -> (
         let !_childrenX2 = Control.Monad.Identity.runIdentity (attach_T_Children (arg_children_)) in
         let !_rulesX20 = Control.Monad.Identity.runIdentity (attach_T_Rules (arg_rules_)) in
         let !_childrenOinhMap = rule215 _lhsIinhMap in
         let !_mergesIn = rule210 _lhsImergesIn arg_con_ in
         let !_merged = rule211 _mergesIn in
         let !_childrenOmerged = rule216 _merged in
         let !_childrenOsynMap = rule219 _lhsIsynMap in
         let !_orderDeps = rule206 _lhsImanualAttrOrderMap _lhsInt arg_con_ in
         let !(T_Children_vOut47 _childrenIerrors _childrenIfields _childrenIinputs _childrenIoutputs _childrenX53) = inv_Children_s2 _childrenX2 K_Children_v47 (T_Children_vIn47 _childrenOinhMap _childrenOmerged _childrenOsynMap) in
         let !(T_Rules_vOut30 _rulesIdefinedAttrs _rulesIerrors _rulesIlocals _rulesIruleNames _rulesX42) = inv_Rules_s20 _rulesX20 K_Rules_v30 (T_Rules_vIn30 ) in
         let !(!_newRls,!_errs) = rule201 _childrenIfields _childrenIinputs _childrenIoutputs _lhsIcr _lhsIinh _lhsInt _lhsIo_rename _lhsIoptions _lhsIsyn _lhsIsynOrig _lhsItypeSyns _lhsIuseMap _lhsIwrappers _rulesIdefinedAttrs _rulesIlocals arg_con_ in
         let !_orderErrs = rule207 _childrenIinputs _childrenIoutputs _lhsIinh _lhsInt _lhsIsyn _orderDeps _rulesIlocals _rulesIruleNames arg_con_ in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule200 _childrenIerrors _errs _orderErrs _rulesIerrors in
         let !__st_ = st48 _childrenX53 _lhsInt _lhsIoptions _mergesIn _newRls _rulesX42
             !__result_ = T_Production_vOut39 _lhsOerrors __st_
          in __result_ )
     in C_Production_s14 k14
   {-# NOINLINE st48 #-}
   st48 = \ !_childrenX53 ((!_lhsInt) :: NontermIdent) ((!_lhsIoptions) :: Options) !_mergesIn !_newRls !_rulesX42 -> let
      v40 :: T_Production_v40 
      v40 = \ !(T_Production_vIn40 _lhsIaroundsIn _lhsIaugmentsIn _lhsIparams _lhsIuniq) -> (
         let !_typeSigsOnt = rule223 _lhsInt in
         let !_typeSigsX24 = Control.Monad.Identity.runIdentity (attach_T_TypeSigs (arg_typeSigs_)) in
         let !_rulesOoptions = rule221 _lhsIoptions in
         let !_typeSigsOparams = rule224 _lhsIparams in
         let !_aroundsIn = rule209 _lhsIaroundsIn arg_con_ in
         let !_rulesOuniq = rule222 _lhsIuniq in
         let !_augmentsIn = rule208 _lhsIaugmentsIn arg_con_ in
         let !(T_Children_vOut48 _childrenIoutput) = inv_Children_s53 _childrenX53 (T_Children_vIn48 ) in
         let !(T_Rules_vOut31 _rulesIoutput _rulesIuniq) = inv_Rules_s42 _rulesX42 (T_Rules_vIn31 _rulesOoptions _rulesOuniq) in
         let !(T_TypeSigs_vOut12 _typeSigsIoutput) = inv_TypeSigs_s24 _typeSigsX24 (T_TypeSigs_vIn12 _typeSigsOnt _typeSigsOparams) in
         let !_extra1 = rule202 _augmentsIn _newRls _rulesIoutput in
         let !_extra2 = rule203 _aroundsIn _extra1 in
         let !_extra3 = rule204 _extra2 _mergesIn in
         let _lhsOoutput :: Production
             !_lhsOoutput = rule205 _childrenIoutput _extra3 _typeSigsIoutput arg_con_ arg_constraints_ arg_macro_ arg_params_ in
         let _lhsOuniq :: Int
             !_lhsOuniq = rule213 _rulesIuniq in
         let !__result_ = T_Production_vOut40 _lhsOoutput _lhsOuniq
          in __result_ )
     in C_Production_s48 v40
   {-# NOINLINE[1] rule200 #-}
   {-# LINE 384 "./src-ag/DefaultRules.ag" #-}
   rule200 = \ ((!_childrenIerrors) :: Seq Error) !_errs !_orderErrs ((!_rulesIerrors) :: Seq Error) ->
                  {-# LINE 384 "./src-ag/DefaultRules.ag" #-}
                  _childrenIerrors >< _errs >< _rulesIerrors >< _orderErrs
                  {-# LINE 3475 "dist/build/DefaultRules.hs"#-}
   {-# NOINLINE[1] rule201 #-}
   {-# LINE 388 "./src-ag/DefaultRules.ag" #-}
   rule201 = \ ((!_childrenIfields) :: [(Identifier,Type,ChildKind)]) ((!_childrenIinputs) :: [(Identifier, Attributes)]) ((!_childrenIoutputs) :: [(Identifier, Attributes)]) ((!_lhsIcr) :: Bool) ((!_lhsIinh) :: Attributes) ((!_lhsInt) :: NontermIdent) ((!_lhsIo_rename) :: Bool) ((!_lhsIoptions) :: Options) ((!_lhsIsyn) :: Attributes) ((!_lhsIsynOrig) :: Attributes) ((!_lhsItypeSyns) :: TypeSyns) ((!_lhsIuseMap) :: Map Identifier (String,String,String)) ((!_lhsIwrappers) :: Set NontermIdent) ((!_rulesIdefinedAttrs) :: Set (Identifier,Identifier)) ((!_rulesIlocals) :: Set Identifier) !con_ ->
      {-# LINE 388 "./src-ag/DefaultRules.ag" #-}
      let locals       = _rulesIlocals
          initenv      = Map.fromList (  [ (a,_ACHILD)
                                         | (a,_,_) <- _childrenIfields
                                         ]
                                      ++ attrs(_LHS, _lhsIinh)
                                      ++ [ (a,_LOC)
                                         |  a <- Set.toList locals
                                         ]
                                      )
          attrs (n,as) = [ (a,n) | a <- Map.keys as ]
          envs       = scanl (flip Map.union)
                             initenv
                             (map (Map.fromList . attrs ) _childrenIoutputs)
          child_envs = init envs
          lhs_env    = last envs
          (selfAttrs, normalAttrs)
            = Map.partitionWithKey (\k _ -> maybe False isSELFNonterminal $ Map.lookup k _lhsIsynOrig) _lhsIsyn
          (_,undefAttrs)
            = removeDefined _rulesIdefinedAttrs (_LHS, normalAttrs)
          (useAttrs,others)
            = splitAttrs _lhsIuseMap undefAttrs
          (rules1, errors1)
            = concatRE $ map (copyRule _lhsIoptions _lhsIwrappers _lhsInt con_ _lhsIcr locals)
                             (zip envs (map (removeDefined _rulesIdefinedAttrs) _childrenIinputs))
          uRules
            = map (useRule locals _childrenIoutputs) useAttrs
          selfLocRules
            =  [ selfRule False attr $
                   lexTokens noPos $
                   constructor [childSelf attr nm tp | (nm,tp,virt) <- _childrenIfields, childExists virt]
               | attr <- Map.keys selfAttrs
               , not (Set.member attr locals)
               ]
               where
                 childSelf self nm tp
                   = case tp of NT nt _ _                       -> attrName nm self
                                _      | nm `Set.member` locals -> locName nm
                                       | otherwise              -> fieldName nm
                 constructor fs
                   = buildConExpr (ocaml _lhsIoptions) _lhsItypeSyns _lhsIo_rename _lhsInt con_ fs
                 childExists ChildAttr = False
                 childExists _         = True
          selfRules
            = [ selfRule True attr [mkLocVar attr noPos Nothing]
              | attr <- Map.keys selfAttrs
              , not (Set.member (_LHS,attr) _rulesIdefinedAttrs)
              ]
          (rules5, errs5)
            = copyRule _lhsIoptions
                       _lhsIwrappers
                       _lhsInt
                       con_
                       _lhsIcr
                       locals
                       (lhs_env, (_LHS, others))
      in (uRules++selfLocRules++selfRules++rules5++rules1, errors1><errs5)
      {-# LINE 3536 "dist/build/DefaultRules.hs"#-}
   {-# NOINLINE[1] rule202 #-}
   {-# LINE 608 "./src-ag/DefaultRules.ag" #-}
   rule202 = \ !_augmentsIn !_newRls ((!_rulesIoutput) :: Rules) ->
                     {-# LINE 608 "./src-ag/DefaultRules.ag" #-}
                     foldr addAugments (_rulesIoutput ++ _newRls) (Map.assocs _augmentsIn    )
                     {-# LINE 3542 "dist/build/DefaultRules.hs"#-}
   {-# NOINLINE[1] rule203 #-}
   {-# LINE 609 "./src-ag/DefaultRules.ag" #-}
   rule203 = \ !_aroundsIn !_extra1 ->
                     {-# LINE 609 "./src-ag/DefaultRules.ag" #-}
                     foldr addArounds _extra1     (Map.assocs _aroundsIn    )
                     {-# LINE 3548 "dist/build/DefaultRules.hs"#-}
   {-# NOINLINE[1] rule204 #-}
   {-# LINE 610 "./src-ag/DefaultRules.ag" #-}
   rule204 = \ !_extra2 !_mergesIn ->
                     {-# LINE 610 "./src-ag/DefaultRules.ag" #-}
                     foldr addMerges _extra2     (Map.assocs _mergesIn    )
                     {-# LINE 3554 "dist/build/DefaultRules.hs"#-}
   {-# NOINLINE[1] rule205 #-}
   {-# LINE 611 "./src-ag/DefaultRules.ag" #-}
   rule205 = \ ((!_childrenIoutput) :: Children) !_extra3 ((!_typeSigsIoutput) :: TypeSigs) !con_ !constraints_ !macro_ !params_ ->
                     {-# LINE 611 "./src-ag/DefaultRules.ag" #-}
                     Production con_ params_ constraints_ _childrenIoutput _extra3     _typeSigsIoutput macro_
                     {-# LINE 3560 "dist/build/DefaultRules.hs"#-}
   {-# NOINLINE[1] rule206 #-}
   {-# LINE 719 "./src-ag/DefaultRules.ag" #-}
   rule206 = \ ((!_lhsImanualAttrOrderMap) :: AttrOrderMap) ((!_lhsInt) :: NontermIdent) !con_ ->
                        {-# LINE 719 "./src-ag/DefaultRules.ag" #-}
                        Set.toList $ Map.findWithDefault Set.empty con_ $ Map.findWithDefault Map.empty _lhsInt _lhsImanualAttrOrderMap
                        {-# LINE 3566 "dist/build/DefaultRules.hs"#-}
   {-# NOINLINE[1] rule207 #-}
   {-# LINE 722 "./src-ag/DefaultRules.ag" #-}
   rule207 = \ ((!_childrenIinputs) :: [(Identifier, Attributes)]) ((!_childrenIoutputs) :: [(Identifier, Attributes)]) ((!_lhsIinh) :: Attributes) ((!_lhsInt) :: NontermIdent) ((!_lhsIsyn) :: Attributes) !_orderDeps ((!_rulesIlocals) :: Set Identifier) ((!_rulesIruleNames) :: Set Identifier) !con_ ->
            {-# LINE 722 "./src-ag/DefaultRules.ag" #-}
            let chldOutMap = Map.fromList [ (k, Map.keysSet s) | (k,s) <- _childrenIoutputs ]
                chldInMap  = Map.fromList [ (k, Map.keysSet s) | (k,s) <- _childrenIinputs ]
                isInAttribute :: Identifier -> Identifier -> [Error]
                isInAttribute fld nm
                   | fld == _LOC = if nm `Set.member` _rulesIlocals
                                   then []
                                   else [UndefAttr _lhsInt con_ fld nm False]
                   | fld == _LHS = if nm `Map.member` _lhsIinh
                                   then []
                                   else [UndefAttr _lhsInt con_ fld nm False]
                   | otherwise   = if nm `Set.member` (Map.findWithDefault Set.empty fld chldOutMap)
                                   then []
                                   else [UndefAttr _lhsInt con_ fld nm False]
                isOutAttribute :: Identifier -> Identifier -> [Error]
                isOutAttribute fld nm
                   | fld == _LOC = if nm `Set.member` _rulesIlocals
                                   then []
                                   else [UndefAttr _lhsInt con_ fld nm True]
                   | fld == _LHS = if nm `Map.member` _lhsIsyn
                                   then []
                                   else [UndefAttr _lhsInt con_ fld nm True]
                   | otherwise   = if nm `Set.member` (Map.findWithDefault Set.empty fld chldInMap)
                                   then []
                                   else [UndefAttr _lhsInt con_ fld nm True]
                existsRule nm = if nm `Set.member` _rulesIruleNames
                                then []
                                else [MissingNamedRule _lhsInt con_ nm]
                checkIn (OccAttr fld nm)  = isInAttribute fld nm
                checkIn (OccRule nm)      = existsRule nm
                checkOut (OccAttr fld nm) = isOutAttribute fld nm
                checkOut (OccRule nm)     = existsRule nm
            in Seq.fromList . concat $
               [ checkIn occA ++ checkOut occB
               | (Dependency occA occB) <- _orderDeps
               ]
            {-# LINE 3606 "dist/build/DefaultRules.hs"#-}
   {-# NOINLINE[1] rule208 #-}
   {-# LINE 777 "./src-ag/DefaultRules.ag" #-}
   rule208 = \ ((!_lhsIaugmentsIn) :: Map ConstructorIdent (Map Identifier [Expression])) !con_ ->
                                                  {-# LINE 777 "./src-ag/DefaultRules.ag" #-}
                                                  Map.findWithDefault Map.empty con_ _lhsIaugmentsIn
                                                  {-# LINE 3612 "dist/build/DefaultRules.hs"#-}
   {-# NOINLINE[1] rule209 #-}
   {-# LINE 784 "./src-ag/DefaultRules.ag" #-}
   rule209 = \ ((!_lhsIaroundsIn) :: Map ConstructorIdent (Map Identifier [Expression])) !con_ ->
                                                   {-# LINE 784 "./src-ag/DefaultRules.ag" #-}
                                                   Map.findWithDefault Map.empty con_ _lhsIaroundsIn
                                                   {-# LINE 3618 "dist/build/DefaultRules.hs"#-}
   {-# NOINLINE[1] rule210 #-}
   {-# LINE 792 "./src-ag/DefaultRules.ag" #-}
   rule210 = \ ((!_lhsImergesIn) :: Map ConstructorIdent (Map Identifier (Identifier,[Identifier],Expression))) !con_ ->
                                                  {-# LINE 792 "./src-ag/DefaultRules.ag" #-}
                                                  Map.findWithDefault Map.empty con_ _lhsImergesIn
                                                  {-# LINE 3624 "dist/build/DefaultRules.hs"#-}
   {-# NOINLINE[1] rule211 #-}
   {-# LINE 793 "./src-ag/DefaultRules.ag" #-}
   rule211 = \ !_mergesIn ->
                                                  {-# LINE 793 "./src-ag/DefaultRules.ag" #-}
                                                  Set.fromList [ c | (_,cs,_) <- Map.elems _mergesIn    , c <- cs ]
                                                  {-# LINE 3630 "dist/build/DefaultRules.hs"#-}
   {-# NOINLINE[1] rule213 #-}
   rule213 = \ ((!_rulesIuniq) :: Int) ->
     _rulesIuniq
   {-# NOINLINE[1] rule215 #-}
   rule215 = \ ((!_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# NOINLINE[1] rule216 #-}
   rule216 = \ !_merged ->
     _merged
   {-# NOINLINE[1] rule219 #-}
   rule219 = \ ((!_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
   {-# NOINLINE[1] rule221 #-}
   rule221 = \ ((!_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# NOINLINE[1] rule222 #-}
   rule222 = \ ((!_lhsIuniq) :: Int) ->
     _lhsIuniq
   {-# NOINLINE[1] rule223 #-}
   rule223 = \ ((!_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# NOINLINE[1] rule224 #-}
   rule224 = \ ((!_lhsIparams) :: [Identifier]) ->
     _lhsIparams

-- Productions -------------------------------------------------
-- wrapper
data Inh_Productions  = Inh_Productions { aroundsIn_Inh_Productions :: !(Map ConstructorIdent (Map Identifier [Expression])), augmentsIn_Inh_Productions :: !(Map ConstructorIdent (Map Identifier [Expression])), cr_Inh_Productions :: !(Bool), inh_Inh_Productions :: !(Attributes), inhMap_Inh_Productions :: !(Map Identifier Attributes), inhOrig_Inh_Productions :: !(Attributes), manualAttrOrderMap_Inh_Productions :: !(AttrOrderMap), mergesIn_Inh_Productions :: !(Map ConstructorIdent (Map Identifier (Identifier,[Identifier],Expression))), nonterminals_Inh_Productions :: !(Set NontermIdent), nt_Inh_Productions :: !(NontermIdent), o_rename_Inh_Productions :: !(Bool), options_Inh_Productions :: !(Options), params_Inh_Productions :: !([Identifier]), syn_Inh_Productions :: !(Attributes), synMap_Inh_Productions :: !(Map Identifier Attributes), synOrig_Inh_Productions :: !(Attributes), typeSyns_Inh_Productions :: !(TypeSyns), uniq_Inh_Productions :: !(Int), useMap_Inh_Productions :: !(Map Identifier (String,String,String)), wrappers_Inh_Productions :: !(Set NontermIdent) }
data Syn_Productions  = Syn_Productions { errors_Syn_Productions :: !(Seq Error), output_Syn_Productions :: !(Productions), uniq_Syn_Productions :: !(Int) }
{-# INLINABLE wrap_Productions #-}
wrap_Productions :: T_Productions  -> Inh_Productions  -> (Syn_Productions )
wrap_Productions !(T_Productions act) !(Inh_Productions _lhsIaroundsIn _lhsIaugmentsIn _lhsIcr _lhsIinh _lhsIinhMap _lhsIinhOrig _lhsImanualAttrOrderMap _lhsImergesIn _lhsInonterminals _lhsInt _lhsIo_rename _lhsIoptions _lhsIparams _lhsIsyn _lhsIsynMap _lhsIsynOrig _lhsItypeSyns _lhsIuniq _lhsIuseMap _lhsIwrappers) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_Productions_vIn8 _lhsIaroundsIn _lhsIaugmentsIn _lhsIcr _lhsIinh _lhsIinhMap _lhsIinhOrig _lhsImanualAttrOrderMap _lhsImergesIn _lhsInonterminals _lhsInt _lhsIo_rename _lhsIoptions _lhsIparams _lhsIsyn _lhsIsynMap _lhsIsynOrig _lhsItypeSyns _lhsIuniq _lhsIuseMap _lhsIwrappers
        !(T_Productions_vOut8 _lhsOerrors _lhsOoutput _lhsOuniq) <- return (inv_Productions_s16 sem K_Productions_v8 arg)
        return (Syn_Productions _lhsOerrors _lhsOoutput _lhsOuniq)
   )

-- cata
{-# NOINLINE sem_Productions #-}
sem_Productions :: Productions  -> T_Productions 
sem_Productions list = Prelude.foldr sem_Productions_Cons sem_Productions_Nil (Prelude.map sem_Production list)

-- semantic domain
newtype T_Productions  = T_Productions {
                                       attach_T_Productions :: Identity (T_Productions_s16 )
                                       }
data T_Productions_s16  where C_Productions_s16 :: {
                                                   inv_Productions_s16 :: !(forall t. K_Productions_s16  t -> t)
                                                   } -> T_Productions_s16 
data T_Productions_s17  = C_Productions_s17
data T_Productions_s30  = C_Productions_s30
newtype T_Productions_s39  = C_Productions_s39 {
                                               inv_Productions_s39 :: (T_Productions_v27 )
                                               }
data K_Productions_s16 k  where
   K_Productions_v8 :: K_Productions_s16  (T_Productions_v8 )
   K_Productions_v17 :: K_Productions_s16  (T_Productions_v17 )
   K_Productions_v26 :: K_Productions_s16  (T_Productions_v26 )
type T_Productions_v8  = (T_Productions_vIn8 ) -> (T_Productions_vOut8 )
data T_Productions_vIn8  = T_Productions_vIn8 !(Map ConstructorIdent (Map Identifier [Expression])) !(Map ConstructorIdent (Map Identifier [Expression])) !(Bool) !(Attributes) !(Map Identifier Attributes) !(Attributes) !(AttrOrderMap) !(Map ConstructorIdent (Map Identifier (Identifier,[Identifier],Expression))) !(Set NontermIdent) !(NontermIdent) !(Bool) !(Options) !([Identifier]) !(Attributes) !(Map Identifier Attributes) !(Attributes) !(TypeSyns) !(Int) !(Map Identifier (String,String,String)) !(Set NontermIdent)
data T_Productions_vOut8  = T_Productions_vOut8 !(Seq Error) !(Productions) !(Int)
type T_Productions_v17  = (T_Productions_vIn17 ) -> (T_Productions_vOut17 )
data T_Productions_vIn17  = T_Productions_vIn17 !(Map ConstructorIdent (Map Identifier [Expression])) !(Map ConstructorIdent (Map Identifier [Expression])) !(Bool) !(Attributes) !(Map Identifier Attributes) !(AttrOrderMap) !(Map ConstructorIdent (Map Identifier (Identifier,[Identifier],Expression))) !(NontermIdent) !(Bool) !(Options) !([Identifier]) !(Attributes) !(Map Identifier Attributes) !(Attributes) !(TypeSyns) !(Int) !(Map Identifier (String,String,String)) !(Set NontermIdent)
data T_Productions_vOut17  = T_Productions_vOut17 !(Seq Error) !(Productions) !(Int)
type T_Productions_v26  = (T_Productions_vIn26 ) -> (T_Productions_vOut26 )
data T_Productions_vIn26  = T_Productions_vIn26 !(Bool) !(Attributes) !(Map Identifier Attributes) !(AttrOrderMap) !(Map ConstructorIdent (Map Identifier (Identifier,[Identifier],Expression))) !(NontermIdent) !(Bool) !(Options) !(Attributes) !(Map Identifier Attributes) !(Attributes) !(TypeSyns) !(Map Identifier (String,String,String)) !(Set NontermIdent)
data T_Productions_vOut26  = T_Productions_vOut26 !(Seq Error) !(T_Productions_s39 )
type T_Productions_v27  = (T_Productions_vIn27 ) -> (T_Productions_vOut27 )
data T_Productions_vIn27  = T_Productions_vIn27 !(Map ConstructorIdent (Map Identifier [Expression])) !(Map ConstructorIdent (Map Identifier [Expression])) !([Identifier]) !(Int)
data T_Productions_vOut27  = T_Productions_vOut27 !(Productions) !(Int)
{-# NOINLINE sem_Productions_Cons #-}
sem_Productions_Cons :: T_Production  -> T_Productions  -> T_Productions 
sem_Productions_Cons arg_hd_ arg_tl_ = T_Productions (return st16) where
   {-# NOINLINE st16 #-}
   !st16 = let
      k16 :: K_Productions_s16  t -> t
      k16 K_Productions_v8 = v8
      k16 K_Productions_v17 = v17
      k16 K_Productions_v26 = v26
      v8 :: T_Productions_v8 
      v8 = \ !(T_Productions_vIn8 _lhsIaroundsIn _lhsIaugmentsIn _lhsIcr _lhsIinh _lhsIinhMap _lhsIinhOrig _lhsImanualAttrOrderMap _lhsImergesIn _lhsInonterminals _lhsInt _lhsIo_rename _lhsIoptions _lhsIparams _lhsIsyn _lhsIsynMap _lhsIsynOrig _lhsItypeSyns _lhsIuniq _lhsIuseMap _lhsIwrappers) -> (
         let !_hdX14 = Control.Monad.Identity.runIdentity (attach_T_Production (arg_hd_)) in
         let !_tlX16 = Control.Monad.Identity.runIdentity (attach_T_Productions (arg_tl_)) in
         let !_hdOcr = rule231 _lhsIcr in
         let !_hdOinh = rule232 _lhsIinh in
         let !_hdOinhMap = rule233 _lhsIinhMap in
         let !_hdOmanualAttrOrderMap = rule235 _lhsImanualAttrOrderMap in
         let !_hdOmergesIn = rule236 _lhsImergesIn in
         let !_hdOnt = rule238 _lhsInt in
         let !_hdOo_rename = rule239 _lhsIo_rename in
         let !_hdOoptions = rule240 _lhsIoptions in
         let !_hdOsyn = rule242 _lhsIsyn in
         let !_hdOsynMap = rule243 _lhsIsynMap in
         let !_hdOsynOrig = rule244 _lhsIsynOrig in
         let !_hdOtypeSyns = rule245 _lhsItypeSyns in
         let !_hdOuseMap = rule247 _lhsIuseMap in
         let !_hdOwrappers = rule248 _lhsIwrappers in
         let !_tlOcr = rule251 _lhsIcr in
         let !_tlOinh = rule252 _lhsIinh in
         let !_tlOinhMap = rule253 _lhsIinhMap in
         let !_tlOmanualAttrOrderMap = rule255 _lhsImanualAttrOrderMap in
         let !_tlOmergesIn = rule256 _lhsImergesIn in
         let !_tlOnt = rule258 _lhsInt in
         let !_tlOo_rename = rule259 _lhsIo_rename in
         let !_tlOoptions = rule260 _lhsIoptions in
         let !_tlOsyn = rule262 _lhsIsyn in
         let !_tlOsynMap = rule263 _lhsIsynMap in
         let !_tlOsynOrig = rule264 _lhsIsynOrig in
         let !_tlOtypeSyns = rule265 _lhsItypeSyns in
         let !_tlOuseMap = rule267 _lhsIuseMap in
         let !_tlOwrappers = rule268 _lhsIwrappers in
         let !_hdOaroundsIn = rule229 _lhsIaroundsIn in
         let !_hdOaugmentsIn = rule230 _lhsIaugmentsIn in
         let !_hdOparams = rule241 _lhsIparams in
         let !_hdOuniq = rule246 _lhsIuniq in
         let !_tlOaroundsIn = rule249 _lhsIaroundsIn in
         let !_tlOaugmentsIn = rule250 _lhsIaugmentsIn in
         let !_tlOparams = rule261 _lhsIparams in
         let !(T_Production_vOut25 _hdIerrors _hdIoutput _hdIuniq) = inv_Production_s14 _hdX14 K_Production_v25 (T_Production_vIn25 _hdOaroundsIn _hdOaugmentsIn _hdOcr _hdOinh _hdOinhMap _hdOmanualAttrOrderMap _hdOmergesIn _hdOnt _hdOo_rename _hdOoptions _hdOparams _hdOsyn _hdOsynMap _hdOsynOrig _hdOtypeSyns _hdOuniq _hdOuseMap _hdOwrappers) in
         let !(T_Productions_vOut26 _tlIerrors _tlX39) = inv_Productions_s16 _tlX16 K_Productions_v26 (T_Productions_vIn26 _tlOcr _tlOinh _tlOinhMap _tlOmanualAttrOrderMap _tlOmergesIn _tlOnt _tlOo_rename _tlOoptions _tlOsyn _tlOsynMap _tlOsynOrig _tlOtypeSyns _tlOuseMap _tlOwrappers) in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule225 _hdIerrors _tlIerrors in
         let !_tlOuniq = rule266 _hdIuniq in
         let !(T_Productions_vOut27 _tlIoutput _tlIuniq) = inv_Productions_s39 _tlX39 (T_Productions_vIn27 _tlOaroundsIn _tlOaugmentsIn _tlOparams _tlOuniq) in
         let !_output = rule226 _hdIoutput _tlIoutput in
         let _lhsOoutput :: Productions
             !_lhsOoutput = rule227 _output in
         let _lhsOuniq :: Int
             !_lhsOuniq = rule228 _tlIuniq in
         let !__result_ = T_Productions_vOut8 _lhsOerrors _lhsOoutput _lhsOuniq
          in __result_ )
      v17 :: T_Productions_v17 
      v17 = \ !(T_Productions_vIn17 _lhsIaroundsIn _lhsIaugmentsIn _lhsIcr _lhsIinh _lhsIinhMap _lhsImanualAttrOrderMap _lhsImergesIn _lhsInt _lhsIo_rename _lhsIoptions _lhsIparams _lhsIsyn _lhsIsynMap _lhsIsynOrig _lhsItypeSyns _lhsIuniq _lhsIuseMap _lhsIwrappers) -> (
         let !_hdX14 = Control.Monad.Identity.runIdentity (attach_T_Production (arg_hd_)) in
         let !_tlX16 = Control.Monad.Identity.runIdentity (attach_T_Productions (arg_tl_)) in
         let !_hdOcr = rule231 _lhsIcr in
         let !_hdOinh = rule232 _lhsIinh in
         let !_hdOinhMap = rule233 _lhsIinhMap in
         let !_hdOmanualAttrOrderMap = rule235 _lhsImanualAttrOrderMap in
         let !_hdOmergesIn = rule236 _lhsImergesIn in
         let !_hdOnt = rule238 _lhsInt in
         let !_hdOo_rename = rule239 _lhsIo_rename in
         let !_hdOoptions = rule240 _lhsIoptions in
         let !_hdOsyn = rule242 _lhsIsyn in
         let !_hdOsynMap = rule243 _lhsIsynMap in
         let !_hdOsynOrig = rule244 _lhsIsynOrig in
         let !_hdOtypeSyns = rule245 _lhsItypeSyns in
         let !_hdOuseMap = rule247 _lhsIuseMap in
         let !_hdOwrappers = rule248 _lhsIwrappers in
         let !_tlOcr = rule251 _lhsIcr in
         let !_tlOinh = rule252 _lhsIinh in
         let !_tlOinhMap = rule253 _lhsIinhMap in
         let !_tlOmanualAttrOrderMap = rule255 _lhsImanualAttrOrderMap in
         let !_tlOmergesIn = rule256 _lhsImergesIn in
         let !_tlOnt = rule258 _lhsInt in
         let !_tlOo_rename = rule259 _lhsIo_rename in
         let !_tlOoptions = rule260 _lhsIoptions in
         let !_tlOsyn = rule262 _lhsIsyn in
         let !_tlOsynMap = rule263 _lhsIsynMap in
         let !_tlOsynOrig = rule264 _lhsIsynOrig in
         let !_tlOtypeSyns = rule265 _lhsItypeSyns in
         let !_tlOuseMap = rule267 _lhsIuseMap in
         let !_tlOwrappers = rule268 _lhsIwrappers in
         let !_hdOaroundsIn = rule229 _lhsIaroundsIn in
         let !_hdOaugmentsIn = rule230 _lhsIaugmentsIn in
         let !_hdOparams = rule241 _lhsIparams in
         let !_hdOuniq = rule246 _lhsIuniq in
         let !_tlOaroundsIn = rule249 _lhsIaroundsIn in
         let !_tlOaugmentsIn = rule250 _lhsIaugmentsIn in
         let !_tlOparams = rule261 _lhsIparams in
         let !(T_Production_vOut25 _hdIerrors _hdIoutput _hdIuniq) = inv_Production_s14 _hdX14 K_Production_v25 (T_Production_vIn25 _hdOaroundsIn _hdOaugmentsIn _hdOcr _hdOinh _hdOinhMap _hdOmanualAttrOrderMap _hdOmergesIn _hdOnt _hdOo_rename _hdOoptions _hdOparams _hdOsyn _hdOsynMap _hdOsynOrig _hdOtypeSyns _hdOuniq _hdOuseMap _hdOwrappers) in
         let !(T_Productions_vOut26 _tlIerrors _tlX39) = inv_Productions_s16 _tlX16 K_Productions_v26 (T_Productions_vIn26 _tlOcr _tlOinh _tlOinhMap _tlOmanualAttrOrderMap _tlOmergesIn _tlOnt _tlOo_rename _tlOoptions _tlOsyn _tlOsynMap _tlOsynOrig _tlOtypeSyns _tlOuseMap _tlOwrappers) in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule225 _hdIerrors _tlIerrors in
         let !_tlOuniq = rule266 _hdIuniq in
         let !(T_Productions_vOut27 _tlIoutput _tlIuniq) = inv_Productions_s39 _tlX39 (T_Productions_vIn27 _tlOaroundsIn _tlOaugmentsIn _tlOparams _tlOuniq) in
         let !_output = rule226 _hdIoutput _tlIoutput in
         let _lhsOoutput :: Productions
             !_lhsOoutput = rule227 _output in
         let _lhsOuniq :: Int
             !_lhsOuniq = rule228 _tlIuniq in
         let !__result_ = T_Productions_vOut17 _lhsOerrors _lhsOoutput _lhsOuniq
          in __result_ )
      v26 :: T_Productions_v26 
      v26 = \ !(T_Productions_vIn26 _lhsIcr _lhsIinh _lhsIinhMap _lhsImanualAttrOrderMap _lhsImergesIn _lhsInt _lhsIo_rename _lhsIoptions _lhsIsyn _lhsIsynMap _lhsIsynOrig _lhsItypeSyns _lhsIuseMap _lhsIwrappers) -> (
         let !_hdX14 = Control.Monad.Identity.runIdentity (attach_T_Production (arg_hd_)) in
         let !_tlX16 = Control.Monad.Identity.runIdentity (attach_T_Productions (arg_tl_)) in
         let !_hdOcr = rule231 _lhsIcr in
         let !_hdOinh = rule232 _lhsIinh in
         let !_hdOinhMap = rule233 _lhsIinhMap in
         let !_hdOmanualAttrOrderMap = rule235 _lhsImanualAttrOrderMap in
         let !_hdOmergesIn = rule236 _lhsImergesIn in
         let !_hdOnt = rule238 _lhsInt in
         let !_hdOo_rename = rule239 _lhsIo_rename in
         let !_hdOoptions = rule240 _lhsIoptions in
         let !_hdOsyn = rule242 _lhsIsyn in
         let !_hdOsynMap = rule243 _lhsIsynMap in
         let !_hdOsynOrig = rule244 _lhsIsynOrig in
         let !_hdOtypeSyns = rule245 _lhsItypeSyns in
         let !_hdOuseMap = rule247 _lhsIuseMap in
         let !_hdOwrappers = rule248 _lhsIwrappers in
         let !_tlOcr = rule251 _lhsIcr in
         let !_tlOinh = rule252 _lhsIinh in
         let !_tlOinhMap = rule253 _lhsIinhMap in
         let !_tlOmanualAttrOrderMap = rule255 _lhsImanualAttrOrderMap in
         let !_tlOmergesIn = rule256 _lhsImergesIn in
         let !_tlOnt = rule258 _lhsInt in
         let !_tlOo_rename = rule259 _lhsIo_rename in
         let !_tlOoptions = rule260 _lhsIoptions in
         let !_tlOsyn = rule262 _lhsIsyn in
         let !_tlOsynMap = rule263 _lhsIsynMap in
         let !_tlOsynOrig = rule264 _lhsIsynOrig in
         let !_tlOtypeSyns = rule265 _lhsItypeSyns in
         let !_tlOuseMap = rule267 _lhsIuseMap in
         let !_tlOwrappers = rule268 _lhsIwrappers in
         let !(T_Production_vOut39 _hdIerrors _hdX48) = inv_Production_s14 _hdX14 K_Production_v39 (T_Production_vIn39 _hdOcr _hdOinh _hdOinhMap _hdOmanualAttrOrderMap _hdOmergesIn _hdOnt _hdOo_rename _hdOoptions _hdOsyn _hdOsynMap _hdOsynOrig _hdOtypeSyns _hdOuseMap _hdOwrappers) in
         let !(T_Productions_vOut26 _tlIerrors _tlX39) = inv_Productions_s16 _tlX16 K_Productions_v26 (T_Productions_vIn26 _tlOcr _tlOinh _tlOinhMap _tlOmanualAttrOrderMap _tlOmergesIn _tlOnt _tlOo_rename _tlOoptions _tlOsyn _tlOsynMap _tlOsynOrig _tlOtypeSyns _tlOuseMap _tlOwrappers) in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule225 _hdIerrors _tlIerrors in
         let !__st_ = st39 _hdX48 _tlX39
             !__result_ = T_Productions_vOut26 _lhsOerrors __st_
          in __result_ )
     in C_Productions_s16 k16
   {-# NOINLINE st39 #-}
   st39 = \ !_hdX48 !_tlX39 -> let
      v27 :: T_Productions_v27 
      v27 = \ !(T_Productions_vIn27 _lhsIaroundsIn _lhsIaugmentsIn _lhsIparams _lhsIuniq) -> (
         let !_hdOaroundsIn = rule229 _lhsIaroundsIn in
         let !_hdOaugmentsIn = rule230 _lhsIaugmentsIn in
         let !_hdOparams = rule241 _lhsIparams in
         let !_hdOuniq = rule246 _lhsIuniq in
         let !_tlOaroundsIn = rule249 _lhsIaroundsIn in
         let !_tlOaugmentsIn = rule250 _lhsIaugmentsIn in
         let !_tlOparams = rule261 _lhsIparams in
         let !(T_Production_vOut40 _hdIoutput _hdIuniq) = inv_Production_s48 _hdX48 (T_Production_vIn40 _hdOaroundsIn _hdOaugmentsIn _hdOparams _hdOuniq) in
         let !_tlOuniq = rule266 _hdIuniq in
         let !(T_Productions_vOut27 _tlIoutput _tlIuniq) = inv_Productions_s39 _tlX39 (T_Productions_vIn27 _tlOaroundsIn _tlOaugmentsIn _tlOparams _tlOuniq) in
         let !_output = rule226 _hdIoutput _tlIoutput in
         let _lhsOoutput :: Productions
             !_lhsOoutput = rule227 _output in
         let _lhsOuniq :: Int
             !_lhsOuniq = rule228 _tlIuniq in
         let !__result_ = T_Productions_vOut27 _lhsOoutput _lhsOuniq
          in __result_ )
     in C_Productions_s39 v27
   {-# NOINLINE[1] rule225 #-}
   rule225 = \ ((!_hdIerrors) :: Seq Error) ((!_tlIerrors) :: Seq Error) ->
     _hdIerrors Seq.>< _tlIerrors
   {-# NOINLINE[1] rule226 #-}
   rule226 = \ ((!_hdIoutput) :: Production) ((!_tlIoutput) :: Productions) ->
     (:) _hdIoutput _tlIoutput
   {-# NOINLINE[1] rule227 #-}
   rule227 = \ !_output ->
     _output
   {-# NOINLINE[1] rule228 #-}
   rule228 = \ ((!_tlIuniq) :: Int) ->
     _tlIuniq
   {-# NOINLINE[1] rule229 #-}
   rule229 = \ ((!_lhsIaroundsIn) :: Map ConstructorIdent (Map Identifier [Expression])) ->
     _lhsIaroundsIn
   {-# NOINLINE[1] rule230 #-}
   rule230 = \ ((!_lhsIaugmentsIn) :: Map ConstructorIdent (Map Identifier [Expression])) ->
     _lhsIaugmentsIn
   {-# NOINLINE[1] rule231 #-}
   rule231 = \ ((!_lhsIcr) :: Bool) ->
     _lhsIcr
   {-# NOINLINE[1] rule232 #-}
   rule232 = \ ((!_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# NOINLINE[1] rule233 #-}
   rule233 = \ ((!_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# NOINLINE[1] rule235 #-}
   rule235 = \ ((!_lhsImanualAttrOrderMap) :: AttrOrderMap) ->
     _lhsImanualAttrOrderMap
   {-# NOINLINE[1] rule236 #-}
   rule236 = \ ((!_lhsImergesIn) :: Map ConstructorIdent (Map Identifier (Identifier,[Identifier],Expression))) ->
     _lhsImergesIn
   {-# NOINLINE[1] rule238 #-}
   rule238 = \ ((!_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# NOINLINE[1] rule239 #-}
   rule239 = \ ((!_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# NOINLINE[1] rule240 #-}
   rule240 = \ ((!_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# NOINLINE[1] rule241 #-}
   rule241 = \ ((!_lhsIparams) :: [Identifier]) ->
     _lhsIparams
   {-# NOINLINE[1] rule242 #-}
   rule242 = \ ((!_lhsIsyn) :: Attributes) ->
     _lhsIsyn
   {-# NOINLINE[1] rule243 #-}
   rule243 = \ ((!_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
   {-# NOINLINE[1] rule244 #-}
   rule244 = \ ((!_lhsIsynOrig) :: Attributes) ->
     _lhsIsynOrig
   {-# NOINLINE[1] rule245 #-}
   rule245 = \ ((!_lhsItypeSyns) :: TypeSyns) ->
     _lhsItypeSyns
   {-# NOINLINE[1] rule246 #-}
   rule246 = \ ((!_lhsIuniq) :: Int) ->
     _lhsIuniq
   {-# NOINLINE[1] rule247 #-}
   rule247 = \ ((!_lhsIuseMap) :: Map Identifier (String,String,String)) ->
     _lhsIuseMap
   {-# NOINLINE[1] rule248 #-}
   rule248 = \ ((!_lhsIwrappers) :: Set NontermIdent) ->
     _lhsIwrappers
   {-# NOINLINE[1] rule249 #-}
   rule249 = \ ((!_lhsIaroundsIn) :: Map ConstructorIdent (Map Identifier [Expression])) ->
     _lhsIaroundsIn
   {-# NOINLINE[1] rule250 #-}
   rule250 = \ ((!_lhsIaugmentsIn) :: Map ConstructorIdent (Map Identifier [Expression])) ->
     _lhsIaugmentsIn
   {-# NOINLINE[1] rule251 #-}
   rule251 = \ ((!_lhsIcr) :: Bool) ->
     _lhsIcr
   {-# NOINLINE[1] rule252 #-}
   rule252 = \ ((!_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# NOINLINE[1] rule253 #-}
   rule253 = \ ((!_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# NOINLINE[1] rule255 #-}
   rule255 = \ ((!_lhsImanualAttrOrderMap) :: AttrOrderMap) ->
     _lhsImanualAttrOrderMap
   {-# NOINLINE[1] rule256 #-}
   rule256 = \ ((!_lhsImergesIn) :: Map ConstructorIdent (Map Identifier (Identifier,[Identifier],Expression))) ->
     _lhsImergesIn
   {-# NOINLINE[1] rule258 #-}
   rule258 = \ ((!_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# NOINLINE[1] rule259 #-}
   rule259 = \ ((!_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# NOINLINE[1] rule260 #-}
   rule260 = \ ((!_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# NOINLINE[1] rule261 #-}
   rule261 = \ ((!_lhsIparams) :: [Identifier]) ->
     _lhsIparams
   {-# NOINLINE[1] rule262 #-}
   rule262 = \ ((!_lhsIsyn) :: Attributes) ->
     _lhsIsyn
   {-# NOINLINE[1] rule263 #-}
   rule263 = \ ((!_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
   {-# NOINLINE[1] rule264 #-}
   rule264 = \ ((!_lhsIsynOrig) :: Attributes) ->
     _lhsIsynOrig
   {-# NOINLINE[1] rule265 #-}
   rule265 = \ ((!_lhsItypeSyns) :: TypeSyns) ->
     _lhsItypeSyns
   {-# NOINLINE[1] rule266 #-}
   rule266 = \ ((!_hdIuniq) :: Int) ->
     _hdIuniq
   {-# NOINLINE[1] rule267 #-}
   rule267 = \ ((!_lhsIuseMap) :: Map Identifier (String,String,String)) ->
     _lhsIuseMap
   {-# NOINLINE[1] rule268 #-}
   rule268 = \ ((!_lhsIwrappers) :: Set NontermIdent) ->
     _lhsIwrappers
{-# NOINLINE sem_Productions_Nil #-}
sem_Productions_Nil ::  T_Productions 
sem_Productions_Nil  = T_Productions (return st16) where
   {-# NOINLINE st16 #-}
   !st16 = let
      k16 :: K_Productions_s16  t -> t
      k16 K_Productions_v8 = v8
      k16 K_Productions_v17 = v17
      k16 K_Productions_v26 = v26
      v8 :: T_Productions_v8 
      v8 = \ !(T_Productions_vIn8 _lhsIaroundsIn _lhsIaugmentsIn _lhsIcr _lhsIinh _lhsIinhMap _lhsIinhOrig _lhsImanualAttrOrderMap _lhsImergesIn _lhsInonterminals _lhsInt _lhsIo_rename _lhsIoptions _lhsIparams _lhsIsyn _lhsIsynMap _lhsIsynOrig _lhsItypeSyns _lhsIuniq _lhsIuseMap _lhsIwrappers) -> (
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule269  () in
         let !_output = rule270  () in
         let _lhsOoutput :: Productions
             !_lhsOoutput = rule271 _output in
         let _lhsOuniq :: Int
             !_lhsOuniq = rule272 _lhsIuniq in
         let !__result_ = T_Productions_vOut8 _lhsOerrors _lhsOoutput _lhsOuniq
          in __result_ )
      v17 :: T_Productions_v17 
      v17 = \ !(T_Productions_vIn17 _lhsIaroundsIn _lhsIaugmentsIn _lhsIcr _lhsIinh _lhsIinhMap _lhsImanualAttrOrderMap _lhsImergesIn _lhsInt _lhsIo_rename _lhsIoptions _lhsIparams _lhsIsyn _lhsIsynMap _lhsIsynOrig _lhsItypeSyns _lhsIuniq _lhsIuseMap _lhsIwrappers) -> (
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule269  () in
         let !_output = rule270  () in
         let _lhsOoutput :: Productions
             !_lhsOoutput = rule271 _output in
         let _lhsOuniq :: Int
             !_lhsOuniq = rule272 _lhsIuniq in
         let !__result_ = T_Productions_vOut17 _lhsOerrors _lhsOoutput _lhsOuniq
          in __result_ )
      v26 :: T_Productions_v26 
      v26 = \ !(T_Productions_vIn26 _lhsIcr _lhsIinh _lhsIinhMap _lhsImanualAttrOrderMap _lhsImergesIn _lhsInt _lhsIo_rename _lhsIoptions _lhsIsyn _lhsIsynMap _lhsIsynOrig _lhsItypeSyns _lhsIuseMap _lhsIwrappers) -> (
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule269  () in
         let !__st_ = st39  ()
             !__result_ = T_Productions_vOut26 _lhsOerrors __st_
          in __result_ )
     in C_Productions_s16 k16
   {-# NOINLINE st39 #-}
   st39 = \  (_ :: ()) -> let
      v27 :: T_Productions_v27 
      v27 = \ !(T_Productions_vIn27 _lhsIaroundsIn _lhsIaugmentsIn _lhsIparams _lhsIuniq) -> (
         let !_output = rule270  () in
         let _lhsOoutput :: Productions
             !_lhsOoutput = rule271 _output in
         let _lhsOuniq :: Int
             !_lhsOuniq = rule272 _lhsIuniq in
         let !__result_ = T_Productions_vOut27 _lhsOoutput _lhsOuniq
          in __result_ )
     in C_Productions_s39 v27
   {-# NOINLINE[1] rule269 #-}
   rule269 = \  (_ :: ()) ->
     Seq.empty
   {-# NOINLINE[1] rule270 #-}
   rule270 = \  (_ :: ()) ->
     []
   {-# NOINLINE[1] rule271 #-}
   rule271 = \ !_output ->
     _output
   {-# NOINLINE[1] rule272 #-}
   rule272 = \ ((!_lhsIuniq) :: Int) ->
     _lhsIuniq

-- Rule --------------------------------------------------------
-- wrapper
data Inh_Rule  = Inh_Rule { con_Inh_Rule :: !(ConstructorIdent), nt_Inh_Rule :: !(NontermIdent), options_Inh_Rule :: !(Options), uniq_Inh_Rule :: !(Int) }
data Syn_Rule  = Syn_Rule { containsVars_Syn_Rule :: !(Bool), definedAttrs_Syn_Rule :: !(Set (Identifier,Identifier)), errors_Syn_Rule :: !(Seq Error), isPure_Syn_Rule :: !(Bool), locals_Syn_Rule :: !(Set Identifier), output_Syn_Rule :: !(Rule), outputs_Syn_Rule :: !(Rules), ruleNames_Syn_Rule :: !(Set Identifier), uniq_Syn_Rule :: !(Int) }
{-# INLINABLE wrap_Rule #-}
wrap_Rule :: T_Rule  -> Inh_Rule  -> (Syn_Rule )
wrap_Rule !(T_Rule act) !(Inh_Rule _lhsIcon _lhsInt _lhsIoptions _lhsIuniq) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_Rule_vIn9 _lhsIcon _lhsInt _lhsIoptions _lhsIuniq
        !(T_Rule_vOut9 _lhsOcontainsVars _lhsOdefinedAttrs _lhsOerrors _lhsOisPure _lhsOlocals _lhsOoutput _lhsOoutputs _lhsOruleNames _lhsOuniq) <- return (inv_Rule_s18 sem K_Rule_v9 arg)
        return (Syn_Rule _lhsOcontainsVars _lhsOdefinedAttrs _lhsOerrors _lhsOisPure _lhsOlocals _lhsOoutput _lhsOoutputs _lhsOruleNames _lhsOuniq)
   )

-- cata
{-# INLINE sem_Rule #-}
sem_Rule :: Rule  -> T_Rule 
sem_Rule ( Rule !mbName_ pattern_ !rhs_ !owrt_ !origin_ !explicit_ !pure_ !identity_ !mbError_ !eager_ ) = sem_Rule_Rule mbName_ ( sem_Pattern pattern_ ) rhs_ owrt_ origin_ explicit_ pure_ identity_ mbError_ eager_

-- semantic domain
newtype T_Rule  = T_Rule {
                         attach_T_Rule :: Identity (T_Rule_s18 )
                         }
data T_Rule_s18  where C_Rule_s18 :: {
                                     inv_Rule_s18 :: !(forall t. K_Rule_s18  t -> t)
                                     } -> T_Rule_s18 
data T_Rule_s19  = C_Rule_s19
data T_Rule_s41  = C_Rule_s41
newtype T_Rule_s51  = C_Rule_s51 {
                                 inv_Rule_s51 :: (T_Rule_v44 )
                                 }
data K_Rule_s18 k  where
   K_Rule_v9 :: K_Rule_s18  (T_Rule_v9 )
   K_Rule_v29 :: K_Rule_s18  (T_Rule_v29 )
   K_Rule_v43 :: K_Rule_s18  (T_Rule_v43 )
type T_Rule_v9  = (T_Rule_vIn9 ) -> (T_Rule_vOut9 )
data T_Rule_vIn9  = T_Rule_vIn9 !(ConstructorIdent) !(NontermIdent) !(Options) !(Int)
data T_Rule_vOut9  = T_Rule_vOut9 !(Bool) !(Set (Identifier,Identifier)) !(Seq Error) !(Bool) !(Set Identifier) !(Rule) !(Rules) !(Set Identifier) !(Int)
type T_Rule_v29  = (T_Rule_vIn29 ) -> (T_Rule_vOut29 )
data T_Rule_vIn29  = T_Rule_vIn29 !(Options) !(Int)
data T_Rule_vOut29  = T_Rule_vOut29 !(Bool) !(Set (Identifier,Identifier)) !(Seq Error) !(Bool) !(Set Identifier) !(Rules) !(Set Identifier) !(Int)
type T_Rule_v43  = (T_Rule_vIn43 ) -> (T_Rule_vOut43 )
data T_Rule_vIn43  = T_Rule_vIn43 
data T_Rule_vOut43  = T_Rule_vOut43 !(Set (Identifier,Identifier)) !(Seq Error) !(Set Identifier) !(Set Identifier) !(T_Rule_s51 )
type T_Rule_v44  = (T_Rule_vIn44 ) -> (T_Rule_vOut44 )
data T_Rule_vIn44  = T_Rule_vIn44 !(Options) !(Int)
data T_Rule_vOut44  = T_Rule_vOut44 !(Bool) !(Bool) !(Rules) !(Int)
{-# NOINLINE sem_Rule_Rule #-}
sem_Rule_Rule :: (Maybe Identifier) -> T_Pattern  -> (Expression) -> (Bool) -> (String) -> (Bool) -> (Bool) -> (Bool) -> (Maybe Error) -> (Bool) -> T_Rule 
sem_Rule_Rule !arg_mbName_ arg_pattern_ !arg_rhs_ !arg_owrt_ !arg_origin_ !arg_explicit_ !arg_pure_ !arg_identity_ !arg_mbError_ !arg_eager_ = T_Rule (return st18) where
   {-# NOINLINE st18 #-}
   !st18 = let
      k18 :: K_Rule_s18  t -> t
      k18 K_Rule_v9 = v9
      k18 K_Rule_v29 = v29
      k18 K_Rule_v43 = v43
      v9 :: T_Rule_v9 
      v9 = \ !(T_Rule_vIn9 _lhsIcon _lhsInt _lhsIoptions _lhsIuniq) -> (
         let !_patternX10 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pattern_)) in
         let _lhsOisPure :: Bool
             !_lhsOisPure = rule273 arg_pure_ in
         let _lhsOruleNames :: Set Identifier
             !_lhsOruleNames = rule277 arg_mbName_ in
         let !(T_Pattern_vOut28 _patternIcontainsVars _patternIdefinedAttrs _patternIerrors _patternIlocals _patternIoutput) = inv_Pattern_s10 _patternX10 K_Pattern_v28 (T_Pattern_vIn28 ) in
         let _lhsOcontainsVars :: Bool
             !_lhsOcontainsVars = rule278 _patternIcontainsVars in
         let _lhsOdefinedAttrs :: Set (Identifier,Identifier)
             !_lhsOdefinedAttrs = rule279 _patternIdefinedAttrs in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule280 _patternIerrors in
         let _lhsOlocals :: Set Identifier
             !_lhsOlocals = rule281 _patternIlocals in
         let !_output = rule282 _patternIoutput arg_eager_ arg_explicit_ arg_identity_ arg_mbError_ arg_mbName_ arg_origin_ arg_owrt_ arg_pure_ arg_rhs_ in
         let _lhsOoutput :: Rule
             !_lhsOoutput = rule283 _output in
         let !(!_output1,!_mbAlias) = rule274 _output in
         let _lhsOuniq :: Int
             !(!_outputs,!_lhsOuniq) = rule275 _lhsIoptions _lhsIuniq _output1 in
         let _lhsOoutputs :: Rules
             !_lhsOoutputs = rule276 _mbAlias _outputs in
         let !__result_ = T_Rule_vOut9 _lhsOcontainsVars _lhsOdefinedAttrs _lhsOerrors _lhsOisPure _lhsOlocals _lhsOoutput _lhsOoutputs _lhsOruleNames _lhsOuniq
          in __result_ )
      v29 :: T_Rule_v29 
      v29 = \ !(T_Rule_vIn29 _lhsIoptions _lhsIuniq) -> (
         let !_patternX10 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pattern_)) in
         let _lhsOisPure :: Bool
             !_lhsOisPure = rule273 arg_pure_ in
         let _lhsOruleNames :: Set Identifier
             !_lhsOruleNames = rule277 arg_mbName_ in
         let !(T_Pattern_vOut28 _patternIcontainsVars _patternIdefinedAttrs _patternIerrors _patternIlocals _patternIoutput) = inv_Pattern_s10 _patternX10 K_Pattern_v28 (T_Pattern_vIn28 ) in
         let _lhsOcontainsVars :: Bool
             !_lhsOcontainsVars = rule278 _patternIcontainsVars in
         let _lhsOdefinedAttrs :: Set (Identifier,Identifier)
             !_lhsOdefinedAttrs = rule279 _patternIdefinedAttrs in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule280 _patternIerrors in
         let _lhsOlocals :: Set Identifier
             !_lhsOlocals = rule281 _patternIlocals in
         let !_output = rule282 _patternIoutput arg_eager_ arg_explicit_ arg_identity_ arg_mbError_ arg_mbName_ arg_origin_ arg_owrt_ arg_pure_ arg_rhs_ in
         let !(!_output1,!_mbAlias) = rule274 _output in
         let _lhsOuniq :: Int
             !(!_outputs,!_lhsOuniq) = rule275 _lhsIoptions _lhsIuniq _output1 in
         let _lhsOoutputs :: Rules
             !_lhsOoutputs = rule276 _mbAlias _outputs in
         let !__result_ = T_Rule_vOut29 _lhsOcontainsVars _lhsOdefinedAttrs _lhsOerrors _lhsOisPure _lhsOlocals _lhsOoutputs _lhsOruleNames _lhsOuniq
          in __result_ )
      v43 :: T_Rule_v43 
      v43 = \ !(T_Rule_vIn43 ) -> (
         let !_patternX10 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pattern_)) in
         let _lhsOruleNames :: Set Identifier
             !_lhsOruleNames = rule277 arg_mbName_ in
         let !(T_Pattern_vOut50 _patternIdefinedAttrs _patternIerrors _patternIlocals _patternX55) = inv_Pattern_s10 _patternX10 K_Pattern_v50 (T_Pattern_vIn50 ) in
         let _lhsOdefinedAttrs :: Set (Identifier,Identifier)
             !_lhsOdefinedAttrs = rule279 _patternIdefinedAttrs in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule280 _patternIerrors in
         let _lhsOlocals :: Set Identifier
             !_lhsOlocals = rule281 _patternIlocals in
         let !__st_ = st51 _patternX55
             !__result_ = T_Rule_vOut43 _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOruleNames __st_
          in __result_ )
     in C_Rule_s18 k18
   {-# NOINLINE st51 #-}
   st51 = \ !_patternX55 -> let
      v44 :: T_Rule_v44 
      v44 = \ !(T_Rule_vIn44 _lhsIoptions _lhsIuniq) -> (
         let _lhsOisPure :: Bool
             !_lhsOisPure = rule273 arg_pure_ in
         let !(T_Pattern_vOut51 _patternIcontainsVars _patternIoutput) = inv_Pattern_s55 _patternX55 K_Pattern_v51 (T_Pattern_vIn51 ) in
         let _lhsOcontainsVars :: Bool
             !_lhsOcontainsVars = rule278 _patternIcontainsVars in
         let !_output = rule282 _patternIoutput arg_eager_ arg_explicit_ arg_identity_ arg_mbError_ arg_mbName_ arg_origin_ arg_owrt_ arg_pure_ arg_rhs_ in
         let !(!_output1,!_mbAlias) = rule274 _output in
         let _lhsOuniq :: Int
             !(!_outputs,!_lhsOuniq) = rule275 _lhsIoptions _lhsIuniq _output1 in
         let _lhsOoutputs :: Rules
             !_lhsOoutputs = rule276 _mbAlias _outputs in
         let !__result_ = T_Rule_vOut44 _lhsOcontainsVars _lhsOisPure _lhsOoutputs _lhsOuniq
          in __result_ )
     in C_Rule_s51 v44
   {-# NOINLINE[1] rule273 #-}
   {-# LINE 557 "./src-ag/DefaultRules.ag" #-}
   rule273 = \ !pure_ ->
                                {-# LINE 557 "./src-ag/DefaultRules.ag" #-}
                                pure_
                                {-# LINE 4207 "dist/build/DefaultRules.hs"#-}
   {-# NOINLINE[1] rule274 #-}
   {-# LINE 624 "./src-ag/DefaultRules.ag" #-}
   rule274 = \ !_output ->
                                         {-# LINE 624 "./src-ag/DefaultRules.ag" #-}
                                         mkRuleAlias _output
                                         {-# LINE 4213 "dist/build/DefaultRules.hs"#-}
   {-# NOINLINE[1] rule275 #-}
   {-# LINE 625 "./src-ag/DefaultRules.ag" #-}
   rule275 = \ ((!_lhsIoptions) :: Options) ((!_lhsIuniq) :: Int) !_output1 ->
                                      {-# LINE 625 "./src-ag/DefaultRules.ag" #-}
                                      if needsMultiRules _lhsIoptions
                                      then multiRule _output1     _lhsIuniq
                                      else ([_output1    ], _lhsIuniq)
                                      {-# LINE 4221 "dist/build/DefaultRules.hs"#-}
   {-# NOINLINE[1] rule276 #-}
   {-# LINE 628 "./src-ag/DefaultRules.ag" #-}
   rule276 = \ !_mbAlias !_outputs ->
                          {-# LINE 628 "./src-ag/DefaultRules.ag" #-}
                          maybe [] return _mbAlias     ++ _outputs
                          {-# LINE 4227 "dist/build/DefaultRules.hs"#-}
   {-# NOINLINE[1] rule277 #-}
   {-# LINE 713 "./src-ag/DefaultRules.ag" #-}
   rule277 = \ !mbName_ ->
                                   {-# LINE 713 "./src-ag/DefaultRules.ag" #-}
                                   case mbName_ of
                                     Nothing -> Set.empty
                                     Just nm -> Set.singleton nm
                                   {-# LINE 4235 "dist/build/DefaultRules.hs"#-}
   {-# NOINLINE[1] rule278 #-}
   rule278 = \ ((!_patternIcontainsVars) :: Bool) ->
     _patternIcontainsVars
   {-# NOINLINE[1] rule279 #-}
   rule279 = \ ((!_patternIdefinedAttrs) :: Set (Identifier,Identifier)) ->
     _patternIdefinedAttrs
   {-# NOINLINE[1] rule280 #-}
   rule280 = \ ((!_patternIerrors) :: Seq Error) ->
     _patternIerrors
   {-# NOINLINE[1] rule281 #-}
   rule281 = \ ((!_patternIlocals) :: Set Identifier) ->
     _patternIlocals
   {-# NOINLINE[1] rule282 #-}
   rule282 = \ ((!_patternIoutput) :: Pattern) !eager_ !explicit_ !identity_ !mbError_ !mbName_ !origin_ !owrt_ !pure_ !rhs_ ->
     Rule mbName_ _patternIoutput rhs_ owrt_ origin_ explicit_ pure_ identity_ mbError_ eager_
   {-# INLINE rule283 #-}
   rule283 = \ !_output ->
     _output

-- Rules -------------------------------------------------------
-- wrapper
data Inh_Rules  = Inh_Rules { con_Inh_Rules :: !(ConstructorIdent), nt_Inh_Rules :: !(NontermIdent), options_Inh_Rules :: !(Options), uniq_Inh_Rules :: !(Int) }
data Syn_Rules  = Syn_Rules { definedAttrs_Syn_Rules :: !(Set (Identifier,Identifier)), errors_Syn_Rules :: !(Seq Error), locals_Syn_Rules :: !(Set Identifier), output_Syn_Rules :: !(Rules), ruleNames_Syn_Rules :: !(Set Identifier), uniq_Syn_Rules :: !(Int) }
{-# INLINABLE wrap_Rules #-}
wrap_Rules :: T_Rules  -> Inh_Rules  -> (Syn_Rules )
wrap_Rules !(T_Rules act) !(Inh_Rules _lhsIcon _lhsInt _lhsIoptions _lhsIuniq) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_Rules_vIn10 _lhsIcon _lhsInt _lhsIoptions _lhsIuniq
        !(T_Rules_vOut10 _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput _lhsOruleNames _lhsOuniq) <- return (inv_Rules_s20 sem K_Rules_v10 arg)
        return (Syn_Rules _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput _lhsOruleNames _lhsOuniq)
   )

-- cata
{-# NOINLINE sem_Rules #-}
sem_Rules :: Rules  -> T_Rules 
sem_Rules list = Prelude.foldr sem_Rules_Cons sem_Rules_Nil (Prelude.map sem_Rule list)

-- semantic domain
newtype T_Rules  = T_Rules {
                           attach_T_Rules :: Identity (T_Rules_s20 )
                           }
data T_Rules_s20  where C_Rules_s20 :: {
                                       inv_Rules_s20 :: !(forall t. K_Rules_s20  t -> t)
                                       } -> T_Rules_s20 
data T_Rules_s21  = C_Rules_s21
data T_Rules_s37  = C_Rules_s37
newtype T_Rules_s42  = C_Rules_s42 {
                                   inv_Rules_s42 :: (T_Rules_v31 )
                                   }
data K_Rules_s20 k  where
   K_Rules_v10 :: K_Rules_s20  (T_Rules_v10 )
   K_Rules_v24 :: K_Rules_s20  (T_Rules_v24 )
   K_Rules_v30 :: K_Rules_s20  (T_Rules_v30 )
type T_Rules_v10  = (T_Rules_vIn10 ) -> (T_Rules_vOut10 )
data T_Rules_vIn10  = T_Rules_vIn10 !(ConstructorIdent) !(NontermIdent) !(Options) !(Int)
data T_Rules_vOut10  = T_Rules_vOut10 !(Set (Identifier,Identifier)) !(Seq Error) !(Set Identifier) !(Rules) !(Set Identifier) !(Int)
type T_Rules_v24  = (T_Rules_vIn24 ) -> (T_Rules_vOut24 )
data T_Rules_vIn24  = T_Rules_vIn24 !(Options) !(Int)
data T_Rules_vOut24  = T_Rules_vOut24 !(Set (Identifier,Identifier)) !(Seq Error) !(Set Identifier) !(Rules) !(Set Identifier) !(Int)
type T_Rules_v30  = (T_Rules_vIn30 ) -> (T_Rules_vOut30 )
data T_Rules_vIn30  = T_Rules_vIn30 
data T_Rules_vOut30  = T_Rules_vOut30 !(Set (Identifier,Identifier)) !(Seq Error) !(Set Identifier) !(Set Identifier) !(T_Rules_s42 )
type T_Rules_v31  = (T_Rules_vIn31 ) -> (T_Rules_vOut31 )
data T_Rules_vIn31  = T_Rules_vIn31 !(Options) !(Int)
data T_Rules_vOut31  = T_Rules_vOut31 !(Rules) !(Int)
{-# NOINLINE sem_Rules_Cons #-}
sem_Rules_Cons :: T_Rule  -> T_Rules  -> T_Rules 
sem_Rules_Cons arg_hd_ arg_tl_ = T_Rules (return st20) where
   {-# NOINLINE st20 #-}
   !st20 = let
      k20 :: K_Rules_s20  t -> t
      k20 K_Rules_v10 = v10
      k20 K_Rules_v24 = v24
      k20 K_Rules_v30 = v30
      v10 :: T_Rules_v10 
      v10 = \ !(T_Rules_vIn10 _lhsIcon _lhsInt _lhsIoptions _lhsIuniq) -> (
         let !_hdX18 = Control.Monad.Identity.runIdentity (attach_T_Rule (arg_hd_)) in
         let !_tlX20 = Control.Monad.Identity.runIdentity (attach_T_Rules (arg_tl_)) in
         let !_hdOoptions = rule295 _lhsIoptions in
         let !_hdOuniq = rule296 _lhsIuniq in
         let !_tlOoptions = rule299 _lhsIoptions in
         let !(T_Rule_vOut29 _hdIcontainsVars _hdIdefinedAttrs _hdIerrors _hdIisPure _hdIlocals _hdIoutputs _hdIruleNames _hdIuniq) = inv_Rule_s18 _hdX18 K_Rule_v29 (T_Rule_vIn29 _hdOoptions _hdOuniq) in
         let !(T_Rules_vOut30 _tlIdefinedAttrs _tlIerrors _tlIlocals _tlIruleNames _tlX42) = inv_Rules_s20 _tlX20 K_Rules_v30 (T_Rules_vIn30 ) in
         let _lhsOdefinedAttrs :: Set (Identifier,Identifier)
             !_lhsOdefinedAttrs = rule287 _hdIdefinedAttrs _tlIdefinedAttrs in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule288 _hdIerrors _tlIerrors in
         let _lhsOlocals :: Set Identifier
             !_lhsOlocals = rule289 _hdIlocals _tlIlocals in
         let !_tlOuniq = rule300 _hdIuniq in
         let _lhsOruleNames :: Set Identifier
             !_lhsOruleNames = rule290 _hdIruleNames _tlIruleNames in
         let !(T_Rules_vOut31 _tlIoutput _tlIuniq) = inv_Rules_s42 _tlX42 (T_Rules_vIn31 _tlOoptions _tlOuniq) in
         let _lhsOoutput :: Rules
             !_lhsOoutput = rule286 _hdIcontainsVars _hdIisPure _hdIoutputs _tlIoutput in
         let _lhsOuniq :: Int
             !_lhsOuniq = rule292 _tlIuniq in
         let !__result_ = T_Rules_vOut10 _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput _lhsOruleNames _lhsOuniq
          in __result_ )
      v24 :: T_Rules_v24 
      v24 = \ !(T_Rules_vIn24 _lhsIoptions _lhsIuniq) -> (
         let !_hdX18 = Control.Monad.Identity.runIdentity (attach_T_Rule (arg_hd_)) in
         let !_tlX20 = Control.Monad.Identity.runIdentity (attach_T_Rules (arg_tl_)) in
         let !_hdOoptions = rule295 _lhsIoptions in
         let !_hdOuniq = rule296 _lhsIuniq in
         let !_tlOoptions = rule299 _lhsIoptions in
         let !(T_Rule_vOut29 _hdIcontainsVars _hdIdefinedAttrs _hdIerrors _hdIisPure _hdIlocals _hdIoutputs _hdIruleNames _hdIuniq) = inv_Rule_s18 _hdX18 K_Rule_v29 (T_Rule_vIn29 _hdOoptions _hdOuniq) in
         let !(T_Rules_vOut30 _tlIdefinedAttrs _tlIerrors _tlIlocals _tlIruleNames _tlX42) = inv_Rules_s20 _tlX20 K_Rules_v30 (T_Rules_vIn30 ) in
         let _lhsOdefinedAttrs :: Set (Identifier,Identifier)
             !_lhsOdefinedAttrs = rule287 _hdIdefinedAttrs _tlIdefinedAttrs in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule288 _hdIerrors _tlIerrors in
         let _lhsOlocals :: Set Identifier
             !_lhsOlocals = rule289 _hdIlocals _tlIlocals in
         let !_tlOuniq = rule300 _hdIuniq in
         let _lhsOruleNames :: Set Identifier
             !_lhsOruleNames = rule290 _hdIruleNames _tlIruleNames in
         let !(T_Rules_vOut31 _tlIoutput _tlIuniq) = inv_Rules_s42 _tlX42 (T_Rules_vIn31 _tlOoptions _tlOuniq) in
         let _lhsOoutput :: Rules
             !_lhsOoutput = rule286 _hdIcontainsVars _hdIisPure _hdIoutputs _tlIoutput in
         let _lhsOuniq :: Int
             !_lhsOuniq = rule292 _tlIuniq in
         let !__result_ = T_Rules_vOut24 _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput _lhsOruleNames _lhsOuniq
          in __result_ )
      v30 :: T_Rules_v30 
      v30 = \ !(T_Rules_vIn30 ) -> (
         let !_hdX18 = Control.Monad.Identity.runIdentity (attach_T_Rule (arg_hd_)) in
         let !_tlX20 = Control.Monad.Identity.runIdentity (attach_T_Rules (arg_tl_)) in
         let !(T_Rule_vOut43 _hdIdefinedAttrs _hdIerrors _hdIlocals _hdIruleNames _hdX51) = inv_Rule_s18 _hdX18 K_Rule_v43 (T_Rule_vIn43 ) in
         let !(T_Rules_vOut30 _tlIdefinedAttrs _tlIerrors _tlIlocals _tlIruleNames _tlX42) = inv_Rules_s20 _tlX20 K_Rules_v30 (T_Rules_vIn30 ) in
         let _lhsOdefinedAttrs :: Set (Identifier,Identifier)
             !_lhsOdefinedAttrs = rule287 _hdIdefinedAttrs _tlIdefinedAttrs in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule288 _hdIerrors _tlIerrors in
         let _lhsOlocals :: Set Identifier
             !_lhsOlocals = rule289 _hdIlocals _tlIlocals in
         let _lhsOruleNames :: Set Identifier
             !_lhsOruleNames = rule290 _hdIruleNames _tlIruleNames in
         let !__st_ = st42 _hdX51 _tlX42
             !__result_ = T_Rules_vOut30 _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOruleNames __st_
          in __result_ )
     in C_Rules_s20 k20
   {-# NOINLINE st42 #-}
   st42 = \ !_hdX51 !_tlX42 -> let
      v31 :: T_Rules_v31 
      v31 = \ !(T_Rules_vIn31 _lhsIoptions _lhsIuniq) -> (
         let !_hdOoptions = rule295 _lhsIoptions in
         let !_hdOuniq = rule296 _lhsIuniq in
         let !_tlOoptions = rule299 _lhsIoptions in
         let !(T_Rule_vOut44 _hdIcontainsVars _hdIisPure _hdIoutputs _hdIuniq) = inv_Rule_s51 _hdX51 (T_Rule_vIn44 _hdOoptions _hdOuniq) in
         let !_tlOuniq = rule300 _hdIuniq in
         let !(T_Rules_vOut31 _tlIoutput _tlIuniq) = inv_Rules_s42 _tlX42 (T_Rules_vIn31 _tlOoptions _tlOuniq) in
         let _lhsOoutput :: Rules
             !_lhsOoutput = rule286 _hdIcontainsVars _hdIisPure _hdIoutputs _tlIoutput in
         let _lhsOuniq :: Int
             !_lhsOuniq = rule292 _tlIuniq in
         let !__result_ = T_Rules_vOut31 _lhsOoutput _lhsOuniq
          in __result_ )
     in C_Rules_s42 v31
   {-# NOINLINE[1] rule286 #-}
   {-# LINE 620 "./src-ag/DefaultRules.ag" #-}
   rule286 = \ ((!_hdIcontainsVars) :: Bool) ((!_hdIisPure) :: Bool) ((!_hdIoutputs) :: Rules) ((!_tlIoutput) :: Rules) ->
                        {-# LINE 620 "./src-ag/DefaultRules.ag" #-}
                        if _hdIcontainsVars && _hdIisPure then _hdIoutputs ++ _tlIoutput else _tlIoutput
                        {-# LINE 4401 "dist/build/DefaultRules.hs"#-}
   {-# NOINLINE[1] rule287 #-}
   rule287 = \ ((!_hdIdefinedAttrs) :: Set (Identifier,Identifier)) ((!_tlIdefinedAttrs) :: Set (Identifier,Identifier)) ->
     _hdIdefinedAttrs `Set.union` _tlIdefinedAttrs
   {-# NOINLINE[1] rule288 #-}
   rule288 = \ ((!_hdIerrors) :: Seq Error) ((!_tlIerrors) :: Seq Error) ->
     _hdIerrors Seq.>< _tlIerrors
   {-# NOINLINE[1] rule289 #-}
   rule289 = \ ((!_hdIlocals) :: Set Identifier) ((!_tlIlocals) :: Set Identifier) ->
     _hdIlocals `Set.union` _tlIlocals
   {-# NOINLINE[1] rule290 #-}
   rule290 = \ ((!_hdIruleNames) :: Set Identifier) ((!_tlIruleNames) :: Set Identifier) ->
     _hdIruleNames `Set.union` _tlIruleNames
   {-# NOINLINE[1] rule292 #-}
   rule292 = \ ((!_tlIuniq) :: Int) ->
     _tlIuniq
   {-# NOINLINE[1] rule295 #-}
   rule295 = \ ((!_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# NOINLINE[1] rule296 #-}
   rule296 = \ ((!_lhsIuniq) :: Int) ->
     _lhsIuniq
   {-# NOINLINE[1] rule299 #-}
   rule299 = \ ((!_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# NOINLINE[1] rule300 #-}
   rule300 = \ ((!_hdIuniq) :: Int) ->
     _hdIuniq
{-# NOINLINE sem_Rules_Nil #-}
sem_Rules_Nil ::  T_Rules 
sem_Rules_Nil  = T_Rules (return st20) where
   {-# NOINLINE st20 #-}
   !st20 = let
      k20 :: K_Rules_s20  t -> t
      k20 K_Rules_v10 = v10
      k20 K_Rules_v24 = v24
      k20 K_Rules_v30 = v30
      v10 :: T_Rules_v10 
      v10 = \ !(T_Rules_vIn10 _lhsIcon _lhsInt _lhsIoptions _lhsIuniq) -> (
         let _lhsOdefinedAttrs :: Set (Identifier,Identifier)
             !_lhsOdefinedAttrs = rule301  () in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule302  () in
         let _lhsOlocals :: Set Identifier
             !_lhsOlocals = rule303  () in
         let !_output = rule305  () in
         let _lhsOruleNames :: Set Identifier
             !_lhsOruleNames = rule304  () in
         let _lhsOoutput :: Rules
             !_lhsOoutput = rule306 _output in
         let _lhsOuniq :: Int
             !_lhsOuniq = rule307 _lhsIuniq in
         let !__result_ = T_Rules_vOut10 _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput _lhsOruleNames _lhsOuniq
          in __result_ )
      v24 :: T_Rules_v24 
      v24 = \ !(T_Rules_vIn24 _lhsIoptions _lhsIuniq) -> (
         let _lhsOdefinedAttrs :: Set (Identifier,Identifier)
             !_lhsOdefinedAttrs = rule301  () in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule302  () in
         let _lhsOlocals :: Set Identifier
             !_lhsOlocals = rule303  () in
         let !_output = rule305  () in
         let _lhsOruleNames :: Set Identifier
             !_lhsOruleNames = rule304  () in
         let _lhsOoutput :: Rules
             !_lhsOoutput = rule306 _output in
         let _lhsOuniq :: Int
             !_lhsOuniq = rule307 _lhsIuniq in
         let !__result_ = T_Rules_vOut24 _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput _lhsOruleNames _lhsOuniq
          in __result_ )
      v30 :: T_Rules_v30 
      v30 = \ !(T_Rules_vIn30 ) -> (
         let _lhsOdefinedAttrs :: Set (Identifier,Identifier)
             !_lhsOdefinedAttrs = rule301  () in
         let _lhsOerrors :: Seq Error
             !_lhsOerrors = rule302  () in
         let _lhsOlocals :: Set Identifier
             !_lhsOlocals = rule303  () in
         let _lhsOruleNames :: Set Identifier
             !_lhsOruleNames = rule304  () in
         let !__st_ = st42  ()
             !__result_ = T_Rules_vOut30 _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOruleNames __st_
          in __result_ )
     in C_Rules_s20 k20
   {-# NOINLINE st42 #-}
   st42 = \  (_ :: ()) -> let
      v31 :: T_Rules_v31 
      v31 = \ !(T_Rules_vIn31 _lhsIoptions _lhsIuniq) -> (
         let !_output = rule305  () in
         let _lhsOoutput :: Rules
             !_lhsOoutput = rule306 _output in
         let _lhsOuniq :: Int
             !_lhsOuniq = rule307 _lhsIuniq in
         let !__result_ = T_Rules_vOut31 _lhsOoutput _lhsOuniq
          in __result_ )
     in C_Rules_s42 v31
   {-# NOINLINE[1] rule301 #-}
   rule301 = \  (_ :: ()) ->
     Set.empty
   {-# NOINLINE[1] rule302 #-}
   rule302 = \  (_ :: ()) ->
     Seq.empty
   {-# NOINLINE[1] rule303 #-}
   rule303 = \  (_ :: ()) ->
     Set.empty
   {-# NOINLINE[1] rule304 #-}
   rule304 = \  (_ :: ()) ->
     Set.empty
   {-# NOINLINE[1] rule305 #-}
   rule305 = \  (_ :: ()) ->
     []
   {-# NOINLINE[1] rule306 #-}
   rule306 = \ !_output ->
     _output
   {-# NOINLINE[1] rule307 #-}
   rule307 = \ ((!_lhsIuniq) :: Int) ->
     _lhsIuniq

-- TypeSig -----------------------------------------------------
-- wrapper
data Inh_TypeSig  = Inh_TypeSig { nt_Inh_TypeSig :: !(NontermIdent), params_Inh_TypeSig :: !([Identifier]) }
data Syn_TypeSig  = Syn_TypeSig { output_Syn_TypeSig :: !(TypeSig) }
{-# INLINABLE wrap_TypeSig #-}
wrap_TypeSig :: T_TypeSig  -> Inh_TypeSig  -> (Syn_TypeSig )
wrap_TypeSig !(T_TypeSig act) !(Inh_TypeSig _lhsInt _lhsIparams) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_TypeSig_vIn11 _lhsInt _lhsIparams
        !(T_TypeSig_vOut11 _lhsOoutput) <- return (inv_TypeSig_s22 sem arg)
        return (Syn_TypeSig _lhsOoutput)
   )

-- cata
{-# INLINE sem_TypeSig #-}
sem_TypeSig :: TypeSig  -> T_TypeSig 
sem_TypeSig ( TypeSig !name_ !tp_ ) = sem_TypeSig_TypeSig name_ tp_

-- semantic domain
newtype T_TypeSig  = T_TypeSig {
                               attach_T_TypeSig :: Identity (T_TypeSig_s22 )
                               }
newtype T_TypeSig_s22  = C_TypeSig_s22 {
                                       inv_TypeSig_s22 :: (T_TypeSig_v11 )
                                       }
data T_TypeSig_s23  = C_TypeSig_s23
type T_TypeSig_v11  = (T_TypeSig_vIn11 ) -> (T_TypeSig_vOut11 )
data T_TypeSig_vIn11  = T_TypeSig_vIn11 !(NontermIdent) !([Identifier])
data T_TypeSig_vOut11  = T_TypeSig_vOut11 !(TypeSig)
{-# NOINLINE sem_TypeSig_TypeSig #-}
sem_TypeSig_TypeSig :: (Identifier) -> (Type) -> T_TypeSig 
sem_TypeSig_TypeSig !arg_name_ !arg_tp_ = T_TypeSig (return st22) where
   {-# NOINLINE st22 #-}
   !st22 = let
      v11 :: T_TypeSig_v11 
      v11 = \ !(T_TypeSig_vIn11 _lhsInt _lhsIparams) -> (
         let !_tp1 = rule308 _lhsInt _lhsIparams arg_tp_ in
         let _lhsOoutput :: TypeSig
             !_lhsOoutput = rule309 _tp1 arg_name_ in
         let !__result_ = T_TypeSig_vOut11 _lhsOoutput
          in __result_ )
     in C_TypeSig_s22 v11
   {-# INLINE rule308 #-}
   {-# LINE 576 "./src-ag/DefaultRules.ag" #-}
   rule308 = \ ((!_lhsInt) :: NontermIdent) ((!_lhsIparams) :: [Identifier]) !tp_ ->
              {-# LINE 576 "./src-ag/DefaultRules.ag" #-}
              elimSelfId _lhsInt _lhsIparams tp_
              {-# LINE 4568 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule309 #-}
   {-# LINE 617 "./src-ag/DefaultRules.ag" #-}
   rule309 = \ !_tp1 !name_ ->
                 {-# LINE 617 "./src-ag/DefaultRules.ag" #-}
                 TypeSig name_ _tp1
                 {-# LINE 4574 "dist/build/DefaultRules.hs"#-}

-- TypeSigs ----------------------------------------------------
-- wrapper
data Inh_TypeSigs  = Inh_TypeSigs { nt_Inh_TypeSigs :: !(NontermIdent), params_Inh_TypeSigs :: !([Identifier]) }
data Syn_TypeSigs  = Syn_TypeSigs { output_Syn_TypeSigs :: !(TypeSigs) }
{-# INLINABLE wrap_TypeSigs #-}
wrap_TypeSigs :: T_TypeSigs  -> Inh_TypeSigs  -> (Syn_TypeSigs )
wrap_TypeSigs !(T_TypeSigs act) !(Inh_TypeSigs _lhsInt _lhsIparams) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_TypeSigs_vIn12 _lhsInt _lhsIparams
        !(T_TypeSigs_vOut12 _lhsOoutput) <- return (inv_TypeSigs_s24 sem arg)
        return (Syn_TypeSigs _lhsOoutput)
   )

-- cata
{-# NOINLINE sem_TypeSigs #-}
sem_TypeSigs :: TypeSigs  -> T_TypeSigs 
sem_TypeSigs list = Prelude.foldr sem_TypeSigs_Cons sem_TypeSigs_Nil (Prelude.map sem_TypeSig list)

-- semantic domain
newtype T_TypeSigs  = T_TypeSigs {
                                 attach_T_TypeSigs :: Identity (T_TypeSigs_s24 )
                                 }
newtype T_TypeSigs_s24  = C_TypeSigs_s24 {
                                         inv_TypeSigs_s24 :: (T_TypeSigs_v12 )
                                         }
data T_TypeSigs_s25  = C_TypeSigs_s25
type T_TypeSigs_v12  = (T_TypeSigs_vIn12 ) -> (T_TypeSigs_vOut12 )
data T_TypeSigs_vIn12  = T_TypeSigs_vIn12 !(NontermIdent) !([Identifier])
data T_TypeSigs_vOut12  = T_TypeSigs_vOut12 !(TypeSigs)
{-# NOINLINE sem_TypeSigs_Cons #-}
sem_TypeSigs_Cons :: T_TypeSig  -> T_TypeSigs  -> T_TypeSigs 
sem_TypeSigs_Cons arg_hd_ arg_tl_ = T_TypeSigs (return st24) where
   {-# NOINLINE st24 #-}
   !st24 = let
      v12 :: T_TypeSigs_v12 
      v12 = \ !(T_TypeSigs_vIn12 _lhsInt _lhsIparams) -> (
         let !_hdX22 = Control.Monad.Identity.runIdentity (attach_T_TypeSig (arg_hd_)) in
         let !_tlX24 = Control.Monad.Identity.runIdentity (attach_T_TypeSigs (arg_tl_)) in
         let !_hdOnt = rule313 _lhsInt in
         let !_hdOparams = rule314 _lhsIparams in
         let !_tlOnt = rule315 _lhsInt in
         let !_tlOparams = rule316 _lhsIparams in
         let !(T_TypeSig_vOut11 _hdIoutput) = inv_TypeSig_s22 _hdX22 (T_TypeSig_vIn11 _hdOnt _hdOparams) in
         let !(T_TypeSigs_vOut12 _tlIoutput) = inv_TypeSigs_s24 _tlX24 (T_TypeSigs_vIn12 _tlOnt _tlOparams) in
         let !_output = rule311 _hdIoutput _tlIoutput in
         let _lhsOoutput :: TypeSigs
             !_lhsOoutput = rule312 _output in
         let !__result_ = T_TypeSigs_vOut12 _lhsOoutput
          in __result_ )
     in C_TypeSigs_s24 v12
   {-# INLINE rule311 #-}
   rule311 = \ ((!_hdIoutput) :: TypeSig) ((!_tlIoutput) :: TypeSigs) ->
     (:) _hdIoutput _tlIoutput
   {-# INLINE rule312 #-}
   rule312 = \ !_output ->
     _output
   {-# INLINE rule313 #-}
   rule313 = \ ((!_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule314 #-}
   rule314 = \ ((!_lhsIparams) :: [Identifier]) ->
     _lhsIparams
   {-# INLINE rule315 #-}
   rule315 = \ ((!_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule316 #-}
   rule316 = \ ((!_lhsIparams) :: [Identifier]) ->
     _lhsIparams
{-# NOINLINE sem_TypeSigs_Nil #-}
sem_TypeSigs_Nil ::  T_TypeSigs 
sem_TypeSigs_Nil  = T_TypeSigs (return st24) where
   {-# NOINLINE st24 #-}
   !st24 = let
      v12 :: T_TypeSigs_v12 
      v12 = \ !(T_TypeSigs_vIn12 _lhsInt _lhsIparams) -> (
         let !_output = rule317  () in
         let _lhsOoutput :: TypeSigs
             !_lhsOoutput = rule318 _output in
         let !__result_ = T_TypeSigs_vOut12 _lhsOoutput
          in __result_ )
     in C_TypeSigs_s24 v12
   {-# INLINE rule317 #-}
   rule317 = \  (_ :: ()) ->
     []
   {-# INLINE rule318 #-}
   rule318 = \ !_output ->
     _output
