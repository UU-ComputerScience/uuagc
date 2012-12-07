{-# LANGUAGE Rank2Types, GADTs #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DefaultRules where
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
{-# LINE 27 "dist/build/DefaultRules.hs" #-}

{-# LINE 2 "./src-ag/AbstractSyntax.ag" #-}

-- AbstractSyntax.ag imports
import Data.Set(Set)
import Data.Map(Map)
import Patterns    (Pattern(..),Patterns)
import Expression  (Expression(..))
import Macro --marcos
import CommonTypes
import ErrorMessages
{-# LINE 39 "dist/build/DefaultRules.hs" #-}

{-# LINE 2 "./src-ag/Patterns.ag" #-}

-- Patterns.ag imports
import UU.Scanner.Position(Pos)
import CommonTypes (ConstructorIdent,Identifier)
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
        let arg = T_Child_vIn1 _lhsIcon _lhsIcr _lhsIinhMap _lhsImerged _lhsInt _lhsIparams _lhsIsynMap
        !(T_Child_vOut1 _lhsOerrors _lhsOfield _lhsOinherited _lhsOname _lhsOoutput _lhsOsynthesized) <- return (inv_Child_s2 sem arg)
        return (Syn_Child _lhsOerrors _lhsOfield _lhsOinherited _lhsOname _lhsOoutput _lhsOsynthesized)
   )

-- cata
{-# INLINE sem_Child #-}
sem_Child :: Child  -> T_Child 
sem_Child ( Child !name_ !tp_ !kind_ ) = sem_Child_Child name_ tp_ kind_

-- semantic domain
newtype T_Child  = T_Child {
                           attach_T_Child :: Identity (T_Child_s2 )
                           }
newtype T_Child_s2  = C_Child_s2 {
                                 inv_Child_s2 :: (T_Child_v1 )
                                 }
data T_Child_s3  = C_Child_s3
type T_Child_v1  = (T_Child_vIn1 ) -> (T_Child_vOut1 )
data T_Child_vIn1  = T_Child_vIn1 (ConstructorIdent) (Bool) (Map Identifier Attributes) (Set Identifier) (NontermIdent) ([Identifier]) (Map Identifier Attributes)
data T_Child_vOut1  = T_Child_vOut1 (Seq Error) ( (Identifier,Type,ChildKind) ) (Attributes) (Identifier) (Child) (Attributes)
{-# NOINLINE sem_Child_Child #-}
sem_Child_Child :: (Identifier) -> (Type) -> (ChildKind) -> T_Child 
sem_Child_Child !arg_name_ !arg_tp_ !arg_kind_ = T_Child (return st2) where
   {-# NOINLINE st2 #-}
   !st2 = let
      v1 :: T_Child_v1 
      v1 = \ !(T_Child_vIn1 _lhsIcon _lhsIcr _lhsIinhMap _lhsImerged _lhsInt _lhsIparams _lhsIsynMap) -> ( let
         _lhsOname :: Identifier
         _lhsOname = rule0 arg_name_
         _lhsOinherited :: Attributes
         _lhsOinherited = rule1 _inh1
         _lhsOsynthesized :: Attributes
         _lhsOsynthesized = rule2 _lhsImerged _syn1 arg_name_
         _lhsOfield ::  (Identifier,Type,ChildKind) 
         _lhsOfield = rule3 arg_kind_ arg_name_ arg_tp_
         (_nt,_params) = rule4 arg_name_ arg_tp_
         _inh1 = rule5 _inh _nt _params
         _syn1 = rule6 _nt _params _syn
         _lhsOoutput :: Child
         _lhsOoutput = rule7 arg_kind_ arg_name_ arg_tp_
         _chnt = rule8 arg_name_ arg_tp_
         _inh = rule9 _chnt _lhsIinhMap
         _syn = rule10 _chnt _lhsIsynMap
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule11  ()
         _output = rule12 arg_kind_ arg_name_ arg_tp_
         !__result_ = T_Child_vOut1 _lhsOerrors _lhsOfield _lhsOinherited _lhsOname _lhsOoutput _lhsOsynthesized
         in __result_ )
     in C_Child_s2 v1
   {-# INLINE rule0 #-}
   {-# LINE 200 "./src-ag/DefaultRules.ag" #-}
   rule0 = \ name_ ->
                         {-# LINE 200 "./src-ag/DefaultRules.ag" #-}
                         name_
                         {-# LINE 501 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule1 #-}
   {-# LINE 209 "./src-ag/DefaultRules.ag" #-}
   rule1 = \ _inh1 ->
                            {-# LINE 209 "./src-ag/DefaultRules.ag" #-}
                            _inh1
                            {-# LINE 507 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule2 #-}
   {-# LINE 210 "./src-ag/DefaultRules.ag" #-}
   rule2 = \ ((_lhsImerged) :: Set Identifier) _syn1 name_ ->
                              {-# LINE 210 "./src-ag/DefaultRules.ag" #-}
                              if name_ `Set.member` _lhsImerged
                              then Map.empty
                              else _syn1
                              {-# LINE 515 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule3 #-}
   {-# LINE 546 "./src-ag/DefaultRules.ag" #-}
   rule3 = \ kind_ name_ tp_ ->
                        {-# LINE 546 "./src-ag/DefaultRules.ag" #-}
                        (name_,tp_,kind_)
                        {-# LINE 521 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule4 #-}
   {-# LINE 568 "./src-ag/DefaultRules.ag" #-}
   rule4 = \ name_ tp_ ->
                           {-# LINE 568 "./src-ag/DefaultRules.ag" #-}
                           case tp_ of
                             NT nt params _ -> (nt, params)
                             Self           -> error ("The type of child " ++ show name_ ++ " should not be a Self type.")
                             Haskell t      -> (identifier t, [])
                           {-# LINE 530 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule5 #-}
   {-# LINE 572 "./src-ag/DefaultRules.ag" #-}
   rule5 = \ _inh _nt _params ->
               {-# LINE 572 "./src-ag/DefaultRules.ag" #-}
               Map.map (elimSelfStr _nt     _params    ) _inh
               {-# LINE 536 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule6 #-}
   {-# LINE 573 "./src-ag/DefaultRules.ag" #-}
   rule6 = \ _nt _params _syn ->
               {-# LINE 573 "./src-ag/DefaultRules.ag" #-}
               Map.map (elimSelfStr _nt     _params    ) _syn
               {-# LINE 542 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule7 #-}
   {-# LINE 614 "./src-ag/DefaultRules.ag" #-}
   rule7 = \ kind_ name_ tp_ ->
                 {-# LINE 614 "./src-ag/DefaultRules.ag" #-}
                 Child name_ tp_ kind_
                 {-# LINE 548 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule8 #-}
   {-# LINE 19 "./src-ag/DistChildAttr.ag" #-}
   rule8 = \ name_ tp_ ->
                       {-# LINE 19 "./src-ag/DistChildAttr.ag" #-}
                       case tp_ of
                         NT nt _ _ -> nt
                         Self      -> error ("The type of child " ++ show name_ ++ " should not be a Self type.")
                         Haskell t -> identifier ""
                       {-# LINE 557 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule9 #-}
   {-# LINE 23 "./src-ag/DistChildAttr.ag" #-}
   rule9 = \ _chnt ((_lhsIinhMap) :: Map Identifier Attributes) ->
                      {-# LINE 23 "./src-ag/DistChildAttr.ag" #-}
                      Map.findWithDefault Map.empty _chnt     _lhsIinhMap
                      {-# LINE 563 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule10 #-}
   {-# LINE 24 "./src-ag/DistChildAttr.ag" #-}
   rule10 = \ _chnt ((_lhsIsynMap) :: Map Identifier Attributes) ->
                      {-# LINE 24 "./src-ag/DistChildAttr.ag" #-}
                      Map.findWithDefault Map.empty _chnt     _lhsIsynMap
                      {-# LINE 569 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule11 #-}
   rule11 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule12 #-}
   rule12 = \ kind_ name_ tp_ ->
     Child name_ tp_ kind_

-- Children ----------------------------------------------------
-- wrapper
data Inh_Children  = Inh_Children { con_Inh_Children :: !(ConstructorIdent), cr_Inh_Children :: !(Bool), inhMap_Inh_Children :: !(Map Identifier Attributes), merged_Inh_Children :: !(Set Identifier), nt_Inh_Children :: !(NontermIdent), params_Inh_Children :: !([Identifier]), synMap_Inh_Children :: !(Map Identifier Attributes) }
data Syn_Children  = Syn_Children { errors_Syn_Children :: !(Seq Error), fields_Syn_Children :: !([(Identifier,Type,ChildKind)]), inputs_Syn_Children :: !([(Identifier, Attributes)]), output_Syn_Children :: !(Children), outputs_Syn_Children :: !([(Identifier, Attributes)]) }
{-# INLINABLE wrap_Children #-}
wrap_Children :: T_Children  -> Inh_Children  -> (Syn_Children )
wrap_Children !(T_Children act) !(Inh_Children _lhsIcon _lhsIcr _lhsIinhMap _lhsImerged _lhsInt _lhsIparams _lhsIsynMap) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_Children_vIn4 _lhsIcon _lhsIcr _lhsIinhMap _lhsImerged _lhsInt _lhsIparams _lhsIsynMap
        !(T_Children_vOut4 _lhsOerrors _lhsOfields _lhsOinputs _lhsOoutput _lhsOoutputs) <- return (inv_Children_s5 sem arg)
        return (Syn_Children _lhsOerrors _lhsOfields _lhsOinputs _lhsOoutput _lhsOoutputs)
   )

-- cata
{-# NOINLINE sem_Children #-}
sem_Children :: Children  -> T_Children 
sem_Children list = Prelude.foldr sem_Children_Cons sem_Children_Nil (Prelude.map sem_Child list)

-- semantic domain
newtype T_Children  = T_Children {
                                 attach_T_Children :: Identity (T_Children_s5 )
                                 }
newtype T_Children_s5  = C_Children_s5 {
                                       inv_Children_s5 :: (T_Children_v4 )
                                       }
data T_Children_s6  = C_Children_s6
type T_Children_v4  = (T_Children_vIn4 ) -> (T_Children_vOut4 )
data T_Children_vIn4  = T_Children_vIn4 (ConstructorIdent) (Bool) (Map Identifier Attributes) (Set Identifier) (NontermIdent) ([Identifier]) (Map Identifier Attributes)
data T_Children_vOut4  = T_Children_vOut4 (Seq Error) ([(Identifier,Type,ChildKind)]) ([(Identifier, Attributes)]) (Children) ([(Identifier, Attributes)])
{-# NOINLINE sem_Children_Cons #-}
sem_Children_Cons :: T_Child  -> T_Children  -> T_Children 
sem_Children_Cons arg_hd_ arg_tl_ = T_Children (return st5) where
   {-# NOINLINE st5 #-}
   !st5 = let
      v4 :: T_Children_v4 
      v4 = \ !(T_Children_vIn4 _lhsIcon _lhsIcr _lhsIinhMap _lhsImerged _lhsInt _lhsIparams _lhsIsynMap) -> ( let
         _hdX2 = Control.Monad.Identity.runIdentity (attach_T_Child (arg_hd_))
         _tlX5 = Control.Monad.Identity.runIdentity (attach_T_Children (arg_tl_))
         (T_Child_vOut1 _hdIerrors _hdIfield _hdIinherited _hdIname _hdIoutput _hdIsynthesized) = inv_Child_s2 _hdX2 (T_Child_vIn1 _hdOcon _hdOcr _hdOinhMap _hdOmerged _hdOnt _hdOparams _hdOsynMap)
         (T_Children_vOut4 _tlIerrors _tlIfields _tlIinputs _tlIoutput _tlIoutputs) = inv_Children_s5 _tlX5 (T_Children_vIn4 _tlOcon _tlOcr _tlOinhMap _tlOmerged _tlOnt _tlOparams _tlOsynMap)
         _lhsOinputs :: [(Identifier, Attributes)]
         _lhsOinputs = rule13 _hdIinherited _hdIname _tlIinputs
         _lhsOoutputs :: [(Identifier, Attributes)]
         _lhsOoutputs = rule14 _hdIname _hdIsynthesized _tlIoutputs
         _lhsOfields :: [(Identifier,Type,ChildKind)]
         _lhsOfields = rule15 _hdIfield _tlIfields
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule16 _hdIerrors _tlIerrors
         _output = rule17 _hdIoutput _tlIoutput
         _lhsOoutput :: Children
         _lhsOoutput = rule18 _output
         _hdOcon = rule19 _lhsIcon
         _hdOcr = rule20 _lhsIcr
         _hdOinhMap = rule21 _lhsIinhMap
         _hdOmerged = rule22 _lhsImerged
         _hdOnt = rule23 _lhsInt
         _hdOparams = rule24 _lhsIparams
         _hdOsynMap = rule25 _lhsIsynMap
         _tlOcon = rule26 _lhsIcon
         _tlOcr = rule27 _lhsIcr
         _tlOinhMap = rule28 _lhsIinhMap
         _tlOmerged = rule29 _lhsImerged
         _tlOnt = rule30 _lhsInt
         _tlOparams = rule31 _lhsIparams
         _tlOsynMap = rule32 _lhsIsynMap
         !__result_ = T_Children_vOut4 _lhsOerrors _lhsOfields _lhsOinputs _lhsOoutput _lhsOoutputs
         in __result_ )
     in C_Children_s5 v4
   {-# INLINE rule13 #-}
   {-# LINE 215 "./src-ag/DefaultRules.ag" #-}
   rule13 = \ ((_hdIinherited) :: Attributes) ((_hdIname) :: Identifier) ((_tlIinputs) :: [(Identifier, Attributes)]) ->
                         {-# LINE 215 "./src-ag/DefaultRules.ag" #-}
                         (_hdIname, _hdIinherited) : _tlIinputs
                         {-# LINE 651 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule14 #-}
   {-# LINE 216 "./src-ag/DefaultRules.ag" #-}
   rule14 = \ ((_hdIname) :: Identifier) ((_hdIsynthesized) :: Attributes) ((_tlIoutputs) :: [(Identifier, Attributes)]) ->
                         {-# LINE 216 "./src-ag/DefaultRules.ag" #-}
                         (_hdIname, _hdIsynthesized) : _tlIoutputs
                         {-# LINE 657 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule15 #-}
   {-# LINE 542 "./src-ag/DefaultRules.ag" #-}
   rule15 = \ ((_hdIfield) ::  (Identifier,Type,ChildKind) ) ((_tlIfields) :: [(Identifier,Type,ChildKind)]) ->
                        {-# LINE 542 "./src-ag/DefaultRules.ag" #-}
                        _hdIfield : _tlIfields
                        {-# LINE 663 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule16 #-}
   rule16 = \ ((_hdIerrors) :: Seq Error) ((_tlIerrors) :: Seq Error) ->
     _hdIerrors Seq.>< _tlIerrors
   {-# INLINE rule17 #-}
   rule17 = \ ((_hdIoutput) :: Child) ((_tlIoutput) :: Children) ->
     (:) _hdIoutput _tlIoutput
   {-# INLINE rule18 #-}
   rule18 = \ _output ->
     _output
   {-# INLINE rule19 #-}
   rule19 = \ ((_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# INLINE rule20 #-}
   rule20 = \ ((_lhsIcr) :: Bool) ->
     _lhsIcr
   {-# INLINE rule21 #-}
   rule21 = \ ((_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# INLINE rule22 #-}
   rule22 = \ ((_lhsImerged) :: Set Identifier) ->
     _lhsImerged
   {-# INLINE rule23 #-}
   rule23 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule24 #-}
   rule24 = \ ((_lhsIparams) :: [Identifier]) ->
     _lhsIparams
   {-# INLINE rule25 #-}
   rule25 = \ ((_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
   {-# INLINE rule26 #-}
   rule26 = \ ((_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# INLINE rule27 #-}
   rule27 = \ ((_lhsIcr) :: Bool) ->
     _lhsIcr
   {-# INLINE rule28 #-}
   rule28 = \ ((_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# INLINE rule29 #-}
   rule29 = \ ((_lhsImerged) :: Set Identifier) ->
     _lhsImerged
   {-# INLINE rule30 #-}
   rule30 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule31 #-}
   rule31 = \ ((_lhsIparams) :: [Identifier]) ->
     _lhsIparams
   {-# INLINE rule32 #-}
   rule32 = \ ((_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
{-# NOINLINE sem_Children_Nil #-}
sem_Children_Nil ::  T_Children 
sem_Children_Nil  = T_Children (return st5) where
   {-# NOINLINE st5 #-}
   !st5 = let
      v4 :: T_Children_v4 
      v4 = \ !(T_Children_vIn4 _lhsIcon _lhsIcr _lhsIinhMap _lhsImerged _lhsInt _lhsIparams _lhsIsynMap) -> ( let
         _lhsOinputs :: [(Identifier, Attributes)]
         _lhsOinputs = rule33  ()
         _lhsOoutputs :: [(Identifier, Attributes)]
         _lhsOoutputs = rule34  ()
         _lhsOfields :: [(Identifier,Type,ChildKind)]
         _lhsOfields = rule35  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule36  ()
         _output = rule37  ()
         _lhsOoutput :: Children
         _lhsOoutput = rule38 _output
         !__result_ = T_Children_vOut4 _lhsOerrors _lhsOfields _lhsOinputs _lhsOoutput _lhsOoutputs
         in __result_ )
     in C_Children_s5 v4
   {-# INLINE rule33 #-}
   {-# LINE 217 "./src-ag/DefaultRules.ag" #-}
   rule33 = \  (_ :: ()) ->
                         {-# LINE 217 "./src-ag/DefaultRules.ag" #-}
                         []
                         {-# LINE 741 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule34 #-}
   {-# LINE 218 "./src-ag/DefaultRules.ag" #-}
   rule34 = \  (_ :: ()) ->
                         {-# LINE 218 "./src-ag/DefaultRules.ag" #-}
                         []
                         {-# LINE 747 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule35 #-}
   {-# LINE 543 "./src-ag/DefaultRules.ag" #-}
   rule35 = \  (_ :: ()) ->
                        {-# LINE 543 "./src-ag/DefaultRules.ag" #-}
                        []
                        {-# LINE 753 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule36 #-}
   rule36 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule37 #-}
   rule37 = \  (_ :: ()) ->
     []
   {-# INLINE rule38 #-}
   rule38 = \ _output ->
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
        let arg = T_Grammar_vIn7 _lhsIoptions
        !(T_Grammar_vOut7 _lhsOerrors _lhsOoutput) <- return (inv_Grammar_s8 sem arg)
        return (Syn_Grammar _lhsOerrors _lhsOoutput)
   )

-- cata
{-# INLINE sem_Grammar #-}
sem_Grammar :: Grammar  -> T_Grammar 
sem_Grammar ( Grammar !typeSyns_ !useMap_ !derivings_ !wrappers_ nonts_ !pragmas_ !manualAttrOrderMap_ !paramMap_ !contextMap_ !quantMap_ !uniqueMap_ !augmentsMap_ !aroundsMap_ !mergeMap_ ) = sem_Grammar_Grammar typeSyns_ useMap_ derivings_ wrappers_ ( sem_Nonterminals nonts_ ) pragmas_ manualAttrOrderMap_ paramMap_ contextMap_ quantMap_ uniqueMap_ augmentsMap_ aroundsMap_ mergeMap_

-- semantic domain
newtype T_Grammar  = T_Grammar {
                               attach_T_Grammar :: Identity (T_Grammar_s8 )
                               }
newtype T_Grammar_s8  = C_Grammar_s8 {
                                     inv_Grammar_s8 :: (T_Grammar_v7 )
                                     }
data T_Grammar_s9  = C_Grammar_s9
type T_Grammar_v7  = (T_Grammar_vIn7 ) -> (T_Grammar_vOut7 )
data T_Grammar_vIn7  = T_Grammar_vIn7 (Options)
data T_Grammar_vOut7  = T_Grammar_vOut7 (Seq Error) (Grammar)
{-# NOINLINE sem_Grammar_Grammar #-}
sem_Grammar_Grammar :: (TypeSyns) -> (UseMap) -> (Derivings) -> (Set NontermIdent) -> T_Nonterminals  -> (PragmaMap) -> (AttrOrderMap) -> (ParamMap) -> (ContextMap) -> (QuantMap) -> (UniqueMap) -> (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) -> (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) -> (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier, [Identifier], Expression)))) -> T_Grammar 
sem_Grammar_Grammar !arg_typeSyns_ !arg_useMap_ !arg_derivings_ !arg_wrappers_ arg_nonts_ !arg_pragmas_ !arg_manualAttrOrderMap_ !arg_paramMap_ !arg_contextMap_ !arg_quantMap_ !arg_uniqueMap_ !arg_augmentsMap_ !arg_aroundsMap_ !arg_mergeMap_ = T_Grammar (return st8) where
   {-# NOINLINE st8 #-}
   !st8 = let
      v7 :: T_Grammar_v7 
      v7 = \ !(T_Grammar_vIn7 _lhsIoptions) -> ( let
         _nontsX14 = Control.Monad.Identity.runIdentity (attach_T_Nonterminals (arg_nonts_))
         (T_Nonterminals_vOut13 _nontsIcollect_nts _nontsIerrors _nontsIinhMap' _nontsIoutput _nontsIsynMap' _nontsIuniq) = inv_Nonterminals_s14 _nontsX14 (T_Nonterminals_vIn13 _nontsOaroundsIn _nontsOaugmentsIn _nontsOcr _nontsOinhMap _nontsOmanualAttrOrderMap _nontsOmergesIn _nontsOnonterminals _nontsOo_rename _nontsOoptions _nontsOsynMap _nontsOtypeSyns _nontsOuniq _nontsOuseMap _nontsOwrappers)
         _nontsOo_rename = rule39 _lhsIoptions
         _nontsOcr = rule40 _lhsIoptions
         _nontsOwrappers = rule41 arg_wrappers_
         _nontsOnonterminals = rule42 _nontsIcollect_nts
         _nontsOuseMap = rule43 arg_useMap_
         _nontsOtypeSyns = rule44 arg_typeSyns_
         _nontsOuniq = rule45  ()
         _nontsOmanualAttrOrderMap = rule46 arg_manualAttrOrderMap_
         _nontsOaugmentsIn = rule47 arg_augmentsMap_
         _nontsOaroundsIn = rule48 arg_aroundsMap_
         _nontsOmergesIn = rule49 arg_mergeMap_
         _nontsOinhMap = rule50 _nontsIinhMap'
         _nontsOsynMap = rule51 _nontsIsynMap'
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule52 _nontsIerrors
         _output = rule53 _nontsIoutput arg_aroundsMap_ arg_augmentsMap_ arg_contextMap_ arg_derivings_ arg_manualAttrOrderMap_ arg_mergeMap_ arg_paramMap_ arg_pragmas_ arg_quantMap_ arg_typeSyns_ arg_uniqueMap_ arg_useMap_ arg_wrappers_
         _lhsOoutput :: Grammar
         _lhsOoutput = rule54 _output
         _nontsOoptions = rule55 _lhsIoptions
         !__result_ = T_Grammar_vOut7 _lhsOerrors _lhsOoutput
         in __result_ )
     in C_Grammar_s8 v7
   {-# INLINE rule39 #-}
   {-# LINE 58 "./src-ag/DefaultRules.ag" #-}
   rule39 = \ ((_lhsIoptions) :: Options) ->
                                    {-# LINE 58 "./src-ag/DefaultRules.ag" #-}
                                    rename    _lhsIoptions
                                    {-# LINE 830 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule40 #-}
   {-# LINE 59 "./src-ag/DefaultRules.ag" #-}
   rule40 = \ ((_lhsIoptions) :: Options) ->
                                    {-# LINE 59 "./src-ag/DefaultRules.ag" #-}
                                    modcopy   _lhsIoptions
                                    {-# LINE 836 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule41 #-}
   {-# LINE 67 "./src-ag/DefaultRules.ag" #-}
   rule41 = \ wrappers_ ->
                     {-# LINE 67 "./src-ag/DefaultRules.ag" #-}
                     wrappers_
                     {-# LINE 842 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule42 #-}
   {-# LINE 180 "./src-ag/DefaultRules.ag" #-}
   rule42 = \ ((_nontsIcollect_nts) :: Set NontermIdent) ->
                                   {-# LINE 180 "./src-ag/DefaultRules.ag" #-}
                                   _nontsIcollect_nts
                                   {-# LINE 848 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule43 #-}
   {-# LINE 202 "./src-ag/DefaultRules.ag" #-}
   rule43 = \ useMap_ ->
                               {-# LINE 202 "./src-ag/DefaultRules.ag" #-}
                               useMap_
                               {-# LINE 854 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule44 #-}
   {-# LINE 204 "./src-ag/DefaultRules.ag" #-}
   rule44 = \ typeSyns_ ->
                                 {-# LINE 204 "./src-ag/DefaultRules.ag" #-}
                                 typeSyns_
                                 {-# LINE 860 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule45 #-}
   {-# LINE 595 "./src-ag/DefaultRules.ag" #-}
   rule45 = \  (_ :: ()) ->
                           {-# LINE 595 "./src-ag/DefaultRules.ag" #-}
                           1
                           {-# LINE 866 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule46 #-}
   {-# LINE 709 "./src-ag/DefaultRules.ag" #-}
   rule46 = \ manualAttrOrderMap_ ->
                                   {-# LINE 709 "./src-ag/DefaultRules.ag" #-}
                                   manualAttrOrderMap_
                                   {-# LINE 872 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule47 #-}
   {-# LINE 775 "./src-ag/DefaultRules.ag" #-}
   rule47 = \ augmentsMap_ ->
                                                    {-# LINE 775 "./src-ag/DefaultRules.ag" #-}
                                                    augmentsMap_
                                                    {-# LINE 878 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule48 #-}
   {-# LINE 782 "./src-ag/DefaultRules.ag" #-}
   rule48 = \ aroundsMap_ ->
                                                   {-# LINE 782 "./src-ag/DefaultRules.ag" #-}
                                                   aroundsMap_
                                                   {-# LINE 884 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule49 #-}
   {-# LINE 790 "./src-ag/DefaultRules.ag" #-}
   rule49 = \ mergeMap_ ->
                                                  {-# LINE 790 "./src-ag/DefaultRules.ag" #-}
                                                  mergeMap_
                                                  {-# LINE 890 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule50 #-}
   {-# LINE 15 "./src-ag/DistChildAttr.ag" #-}
   rule50 = \ ((_nontsIinhMap') :: Map Identifier Attributes) ->
                             {-# LINE 15 "./src-ag/DistChildAttr.ag" #-}
                             _nontsIinhMap'
                             {-# LINE 896 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule51 #-}
   {-# LINE 16 "./src-ag/DistChildAttr.ag" #-}
   rule51 = \ ((_nontsIsynMap') :: Map Identifier Attributes) ->
                             {-# LINE 16 "./src-ag/DistChildAttr.ag" #-}
                             _nontsIsynMap'
                             {-# LINE 902 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule52 #-}
   rule52 = \ ((_nontsIerrors) :: Seq Error) ->
     _nontsIerrors
   {-# INLINE rule53 #-}
   rule53 = \ ((_nontsIoutput) :: Nonterminals) aroundsMap_ augmentsMap_ contextMap_ derivings_ manualAttrOrderMap_ mergeMap_ paramMap_ pragmas_ quantMap_ typeSyns_ uniqueMap_ useMap_ wrappers_ ->
     Grammar typeSyns_ useMap_ derivings_ wrappers_ _nontsIoutput pragmas_ manualAttrOrderMap_ paramMap_ contextMap_ quantMap_ uniqueMap_ augmentsMap_ aroundsMap_ mergeMap_
   {-# INLINE rule54 #-}
   rule54 = \ _output ->
     _output
   {-# INLINE rule55 #-}
   rule55 = \ ((_lhsIoptions) :: Options) ->
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
        let arg = T_Nonterminal_vIn10 _lhsIaroundsIn _lhsIaugmentsIn _lhsIcr _lhsIinhMap _lhsImanualAttrOrderMap _lhsImergesIn _lhsInonterminals _lhsIo_rename _lhsIoptions _lhsIsynMap _lhsItypeSyns _lhsIuniq _lhsIuseMap _lhsIwrappers
        !(T_Nonterminal_vOut10 _lhsOcollect_nts _lhsOerrors _lhsOinhMap' _lhsOoutput _lhsOsynMap' _lhsOuniq) <- return (inv_Nonterminal_s11 sem arg)
        return (Syn_Nonterminal _lhsOcollect_nts _lhsOerrors _lhsOinhMap' _lhsOoutput _lhsOsynMap' _lhsOuniq)
   )

-- cata
{-# INLINE sem_Nonterminal #-}
sem_Nonterminal :: Nonterminal  -> T_Nonterminal 
sem_Nonterminal ( Nonterminal !nt_ !params_ !inh_ !syn_ prods_ ) = sem_Nonterminal_Nonterminal nt_ params_ inh_ syn_ ( sem_Productions prods_ )

-- semantic domain
newtype T_Nonterminal  = T_Nonterminal {
                                       attach_T_Nonterminal :: Identity (T_Nonterminal_s11 )
                                       }
newtype T_Nonterminal_s11  = C_Nonterminal_s11 {
                                               inv_Nonterminal_s11 :: (T_Nonterminal_v10 )
                                               }
data T_Nonterminal_s12  = C_Nonterminal_s12
type T_Nonterminal_v10  = (T_Nonterminal_vIn10 ) -> (T_Nonterminal_vOut10 )
data T_Nonterminal_vIn10  = T_Nonterminal_vIn10 (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) (Bool) (Map Identifier Attributes) (AttrOrderMap) (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier],Expression)))) (Set NontermIdent) (Bool) (Options) (Map Identifier Attributes) (TypeSyns) (Int) (UseMap) (Set NontermIdent)
data T_Nonterminal_vOut10  = T_Nonterminal_vOut10 (Set NontermIdent) (Seq Error) (Map Identifier Attributes) (Nonterminal) (Map Identifier Attributes) (Int)
{-# NOINLINE sem_Nonterminal_Nonterminal #-}
sem_Nonterminal_Nonterminal :: (NontermIdent) -> ([Identifier]) -> (Attributes) -> (Attributes) -> T_Productions  -> T_Nonterminal 
sem_Nonterminal_Nonterminal !arg_nt_ !arg_params_ !arg_inh_ !arg_syn_ arg_prods_ = T_Nonterminal (return st11) where
   {-# NOINLINE st11 #-}
   !st11 = let
      v10 :: T_Nonterminal_v10 
      v10 = \ !(T_Nonterminal_vIn10 _lhsIaroundsIn _lhsIaugmentsIn _lhsIcr _lhsIinhMap _lhsImanualAttrOrderMap _lhsImergesIn _lhsInonterminals _lhsIo_rename _lhsIoptions _lhsIsynMap _lhsItypeSyns _lhsIuniq _lhsIuseMap _lhsIwrappers) -> ( let
         _prodsX26 = Control.Monad.Identity.runIdentity (attach_T_Productions (arg_prods_))
         (T_Productions_vOut25 _prodsIerrors _prodsIoutput _prodsIuniq) = inv_Productions_s26 _prodsX26 (T_Productions_vIn25 _prodsOaroundsIn _prodsOaugmentsIn _prodsOcr _prodsOinh _prodsOinhMap _prodsOinhOrig _prodsOmanualAttrOrderMap _prodsOmergesIn _prodsOnonterminals _prodsOnt _prodsOo_rename _prodsOoptions _prodsOparams _prodsOsyn _prodsOsynMap _prodsOsynOrig _prodsOtypeSyns _prodsOuniq _prodsOuseMap _prodsOwrappers)
         _prodsOparams = rule56 arg_params_
         _lhsOcollect_nts :: Set NontermIdent
         _lhsOcollect_nts = rule57 arg_nt_
         _prodsOinh = rule58 _inh1
         _prodsOsyn = rule59 _syn1
         _prodsOinhOrig = rule60 arg_inh_
         _prodsOsynOrig = rule61 arg_syn_
         _prodsOuseMap = rule62 _lhsIuseMap arg_nt_
         _prodsOnt = rule63 arg_nt_
         _inh1 = rule64 arg_inh_ arg_nt_ arg_params_
         _syn1 = rule65 arg_nt_ arg_params_ arg_syn_
         _lhsOoutput :: Nonterminal
         _lhsOoutput = rule66 _inh1 _prodsIoutput _syn1 arg_nt_ arg_params_
         _augmentsIn = rule67 _lhsIaugmentsIn arg_nt_
         _aroundsIn = rule68 _lhsIaroundsIn arg_nt_
         _mergesIn = rule69 _lhsImergesIn arg_nt_
         _lhsOinhMap' :: Map Identifier Attributes
         _lhsOinhMap' = rule70 arg_inh_ arg_nt_
         _lhsOsynMap' :: Map Identifier Attributes
         _lhsOsynMap' = rule71 arg_nt_ arg_syn_
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule72 _prodsIerrors
         _output = rule73 _prodsIoutput arg_inh_ arg_nt_ arg_params_ arg_syn_
         _lhsOuniq :: Int
         _lhsOuniq = rule74 _prodsIuniq
         _prodsOaroundsIn = rule75 _aroundsIn
         _prodsOaugmentsIn = rule76 _augmentsIn
         _prodsOcr = rule77 _lhsIcr
         _prodsOinhMap = rule78 _lhsIinhMap
         _prodsOmanualAttrOrderMap = rule79 _lhsImanualAttrOrderMap
         _prodsOmergesIn = rule80 _mergesIn
         _prodsOnonterminals = rule81 _lhsInonterminals
         _prodsOo_rename = rule82 _lhsIo_rename
         _prodsOoptions = rule83 _lhsIoptions
         _prodsOsynMap = rule84 _lhsIsynMap
         _prodsOtypeSyns = rule85 _lhsItypeSyns
         _prodsOuniq = rule86 _lhsIuniq
         _prodsOwrappers = rule87 _lhsIwrappers
         !__result_ = T_Nonterminal_vOut10 _lhsOcollect_nts _lhsOerrors _lhsOinhMap' _lhsOoutput _lhsOsynMap' _lhsOuniq
         in __result_ )
     in C_Nonterminal_s11 v10
   {-# INLINE rule56 #-}
   {-# LINE 44 "./src-ag/DefaultRules.ag" #-}
   rule56 = \ params_ ->
                   {-# LINE 44 "./src-ag/DefaultRules.ag" #-}
                   params_
                   {-# LINE 1001 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule57 #-}
   {-# LINE 176 "./src-ag/DefaultRules.ag" #-}
   rule57 = \ nt_ ->
                                    {-# LINE 176 "./src-ag/DefaultRules.ag" #-}
                                    Set.singleton nt_
                                    {-# LINE 1007 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule58 #-}
   {-# LINE 190 "./src-ag/DefaultRules.ag" #-}
   rule58 = \ _inh1 ->
                                   {-# LINE 190 "./src-ag/DefaultRules.ag" #-}
                                   _inh1
                                   {-# LINE 1013 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule59 #-}
   {-# LINE 191 "./src-ag/DefaultRules.ag" #-}
   rule59 = \ _syn1 ->
                                   {-# LINE 191 "./src-ag/DefaultRules.ag" #-}
                                   _syn1
                                   {-# LINE 1019 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule60 #-}
   {-# LINE 192 "./src-ag/DefaultRules.ag" #-}
   rule60 = \ inh_ ->
                                   {-# LINE 192 "./src-ag/DefaultRules.ag" #-}
                                   inh_
                                   {-# LINE 1025 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule61 #-}
   {-# LINE 193 "./src-ag/DefaultRules.ag" #-}
   rule61 = \ syn_ ->
                                   {-# LINE 193 "./src-ag/DefaultRules.ag" #-}
                                   syn_
                                   {-# LINE 1031 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule62 #-}
   {-# LINE 194 "./src-ag/DefaultRules.ag" #-}
   rule62 = \ ((_lhsIuseMap) :: UseMap) nt_ ->
                                   {-# LINE 194 "./src-ag/DefaultRules.ag" #-}
                                   Map.findWithDefault Map.empty nt_ _lhsIuseMap
                                   {-# LINE 1037 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule63 #-}
   {-# LINE 206 "./src-ag/DefaultRules.ag" #-}
   rule63 = \ nt_ ->
                               {-# LINE 206 "./src-ag/DefaultRules.ag" #-}
                               nt_
                               {-# LINE 1043 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule64 #-}
   {-# LINE 564 "./src-ag/DefaultRules.ag" #-}
   rule64 = \ inh_ nt_ params_ ->
               {-# LINE 564 "./src-ag/DefaultRules.ag" #-}
               Map.map (elimSelfId nt_ params_) inh_
               {-# LINE 1049 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule65 #-}
   {-# LINE 565 "./src-ag/DefaultRules.ag" #-}
   rule65 = \ nt_ params_ syn_ ->
               {-# LINE 565 "./src-ag/DefaultRules.ag" #-}
               Map.map (elimSelfId nt_ params_) syn_
               {-# LINE 1055 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule66 #-}
   {-# LINE 604 "./src-ag/DefaultRules.ag" #-}
   rule66 = \ _inh1 ((_prodsIoutput) :: Productions) _syn1 nt_ params_ ->
                 {-# LINE 604 "./src-ag/DefaultRules.ag" #-}
                 Nonterminal nt_ params_ _inh1     _syn1     _prodsIoutput
                 {-# LINE 1061 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule67 #-}
   {-# LINE 776 "./src-ag/DefaultRules.ag" #-}
   rule67 = \ ((_lhsIaugmentsIn) :: Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) nt_ ->
                                                  {-# LINE 776 "./src-ag/DefaultRules.ag" #-}
                                                  Map.findWithDefault Map.empty nt_ _lhsIaugmentsIn
                                                  {-# LINE 1067 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule68 #-}
   {-# LINE 783 "./src-ag/DefaultRules.ag" #-}
   rule68 = \ ((_lhsIaroundsIn) :: Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) nt_ ->
                                                   {-# LINE 783 "./src-ag/DefaultRules.ag" #-}
                                                   Map.findWithDefault Map.empty nt_ _lhsIaroundsIn
                                                   {-# LINE 1073 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule69 #-}
   {-# LINE 791 "./src-ag/DefaultRules.ag" #-}
   rule69 = \ ((_lhsImergesIn) :: Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier],Expression)))) nt_ ->
                                                  {-# LINE 791 "./src-ag/DefaultRules.ag" #-}
                                                  Map.findWithDefault Map.empty nt_ _lhsImergesIn
                                                  {-# LINE 1079 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule70 #-}
   {-# LINE 7 "./src-ag/DistChildAttr.ag" #-}
   rule70 = \ inh_ nt_ ->
                                 {-# LINE 7 "./src-ag/DistChildAttr.ag" #-}
                                 Map.singleton nt_ inh_
                                 {-# LINE 1085 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule71 #-}
   {-# LINE 8 "./src-ag/DistChildAttr.ag" #-}
   rule71 = \ nt_ syn_ ->
                                 {-# LINE 8 "./src-ag/DistChildAttr.ag" #-}
                                 Map.singleton nt_ syn_
                                 {-# LINE 1091 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule72 #-}
   rule72 = \ ((_prodsIerrors) :: Seq Error) ->
     _prodsIerrors
   {-# INLINE rule73 #-}
   rule73 = \ ((_prodsIoutput) :: Productions) inh_ nt_ params_ syn_ ->
     Nonterminal nt_ params_ inh_ syn_ _prodsIoutput
   {-# INLINE rule74 #-}
   rule74 = \ ((_prodsIuniq) :: Int) ->
     _prodsIuniq
   {-# INLINE rule75 #-}
   rule75 = \ _aroundsIn ->
     _aroundsIn
   {-# INLINE rule76 #-}
   rule76 = \ _augmentsIn ->
     _augmentsIn
   {-# INLINE rule77 #-}
   rule77 = \ ((_lhsIcr) :: Bool) ->
     _lhsIcr
   {-# INLINE rule78 #-}
   rule78 = \ ((_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# INLINE rule79 #-}
   rule79 = \ ((_lhsImanualAttrOrderMap) :: AttrOrderMap) ->
     _lhsImanualAttrOrderMap
   {-# INLINE rule80 #-}
   rule80 = \ _mergesIn ->
     _mergesIn
   {-# INLINE rule81 #-}
   rule81 = \ ((_lhsInonterminals) :: Set NontermIdent) ->
     _lhsInonterminals
   {-# INLINE rule82 #-}
   rule82 = \ ((_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# INLINE rule83 #-}
   rule83 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule84 #-}
   rule84 = \ ((_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
   {-# INLINE rule85 #-}
   rule85 = \ ((_lhsItypeSyns) :: TypeSyns) ->
     _lhsItypeSyns
   {-# INLINE rule86 #-}
   rule86 = \ ((_lhsIuniq) :: Int) ->
     _lhsIuniq
   {-# INLINE rule87 #-}
   rule87 = \ ((_lhsIwrappers) :: Set NontermIdent) ->
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
        let arg = T_Nonterminals_vIn13 _lhsIaroundsIn _lhsIaugmentsIn _lhsIcr _lhsIinhMap _lhsImanualAttrOrderMap _lhsImergesIn _lhsInonterminals _lhsIo_rename _lhsIoptions _lhsIsynMap _lhsItypeSyns _lhsIuniq _lhsIuseMap _lhsIwrappers
        !(T_Nonterminals_vOut13 _lhsOcollect_nts _lhsOerrors _lhsOinhMap' _lhsOoutput _lhsOsynMap' _lhsOuniq) <- return (inv_Nonterminals_s14 sem arg)
        return (Syn_Nonterminals _lhsOcollect_nts _lhsOerrors _lhsOinhMap' _lhsOoutput _lhsOsynMap' _lhsOuniq)
   )

-- cata
{-# NOINLINE sem_Nonterminals #-}
sem_Nonterminals :: Nonterminals  -> T_Nonterminals 
sem_Nonterminals list = Prelude.foldr sem_Nonterminals_Cons sem_Nonterminals_Nil (Prelude.map sem_Nonterminal list)

-- semantic domain
newtype T_Nonterminals  = T_Nonterminals {
                                         attach_T_Nonterminals :: Identity (T_Nonterminals_s14 )
                                         }
newtype T_Nonterminals_s14  = C_Nonterminals_s14 {
                                                 inv_Nonterminals_s14 :: (T_Nonterminals_v13 )
                                                 }
data T_Nonterminals_s15  = C_Nonterminals_s15
type T_Nonterminals_v13  = (T_Nonterminals_vIn13 ) -> (T_Nonterminals_vOut13 )
data T_Nonterminals_vIn13  = T_Nonterminals_vIn13 (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) (Bool) (Map Identifier Attributes) (AttrOrderMap) (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier],Expression)))) (Set NontermIdent) (Bool) (Options) (Map Identifier Attributes) (TypeSyns) (Int) (UseMap) (Set NontermIdent)
data T_Nonterminals_vOut13  = T_Nonterminals_vOut13 (Set NontermIdent) (Seq Error) (Map Identifier Attributes) (Nonterminals) (Map Identifier Attributes) (Int)
{-# NOINLINE sem_Nonterminals_Cons #-}
sem_Nonterminals_Cons :: T_Nonterminal  -> T_Nonterminals  -> T_Nonterminals 
sem_Nonterminals_Cons arg_hd_ arg_tl_ = T_Nonterminals (return st14) where
   {-# NOINLINE st14 #-}
   !st14 = let
      v13 :: T_Nonterminals_v13 
      v13 = \ !(T_Nonterminals_vIn13 _lhsIaroundsIn _lhsIaugmentsIn _lhsIcr _lhsIinhMap _lhsImanualAttrOrderMap _lhsImergesIn _lhsInonterminals _lhsIo_rename _lhsIoptions _lhsIsynMap _lhsItypeSyns _lhsIuniq _lhsIuseMap _lhsIwrappers) -> ( let
         _hdX11 = Control.Monad.Identity.runIdentity (attach_T_Nonterminal (arg_hd_))
         _tlX14 = Control.Monad.Identity.runIdentity (attach_T_Nonterminals (arg_tl_))
         (T_Nonterminal_vOut10 _hdIcollect_nts _hdIerrors _hdIinhMap' _hdIoutput _hdIsynMap' _hdIuniq) = inv_Nonterminal_s11 _hdX11 (T_Nonterminal_vIn10 _hdOaroundsIn _hdOaugmentsIn _hdOcr _hdOinhMap _hdOmanualAttrOrderMap _hdOmergesIn _hdOnonterminals _hdOo_rename _hdOoptions _hdOsynMap _hdOtypeSyns _hdOuniq _hdOuseMap _hdOwrappers)
         (T_Nonterminals_vOut13 _tlIcollect_nts _tlIerrors _tlIinhMap' _tlIoutput _tlIsynMap' _tlIuniq) = inv_Nonterminals_s14 _tlX14 (T_Nonterminals_vIn13 _tlOaroundsIn _tlOaugmentsIn _tlOcr _tlOinhMap _tlOmanualAttrOrderMap _tlOmergesIn _tlOnonterminals _tlOo_rename _tlOoptions _tlOsynMap _tlOtypeSyns _tlOuniq _tlOuseMap _tlOwrappers)
         _lhsOcollect_nts :: Set NontermIdent
         _lhsOcollect_nts = rule88 _hdIcollect_nts _tlIcollect_nts
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule89 _hdIerrors _tlIerrors
         _lhsOinhMap' :: Map Identifier Attributes
         _lhsOinhMap' = rule90 _hdIinhMap' _tlIinhMap'
         _lhsOsynMap' :: Map Identifier Attributes
         _lhsOsynMap' = rule91 _hdIsynMap' _tlIsynMap'
         _output = rule92 _hdIoutput _tlIoutput
         _lhsOoutput :: Nonterminals
         _lhsOoutput = rule93 _output
         _lhsOuniq :: Int
         _lhsOuniq = rule94 _tlIuniq
         _hdOaroundsIn = rule95 _lhsIaroundsIn
         _hdOaugmentsIn = rule96 _lhsIaugmentsIn
         _hdOcr = rule97 _lhsIcr
         _hdOinhMap = rule98 _lhsIinhMap
         _hdOmanualAttrOrderMap = rule99 _lhsImanualAttrOrderMap
         _hdOmergesIn = rule100 _lhsImergesIn
         _hdOnonterminals = rule101 _lhsInonterminals
         _hdOo_rename = rule102 _lhsIo_rename
         _hdOoptions = rule103 _lhsIoptions
         _hdOsynMap = rule104 _lhsIsynMap
         _hdOtypeSyns = rule105 _lhsItypeSyns
         _hdOuniq = rule106 _lhsIuniq
         _hdOuseMap = rule107 _lhsIuseMap
         _hdOwrappers = rule108 _lhsIwrappers
         _tlOaroundsIn = rule109 _lhsIaroundsIn
         _tlOaugmentsIn = rule110 _lhsIaugmentsIn
         _tlOcr = rule111 _lhsIcr
         _tlOinhMap = rule112 _lhsIinhMap
         _tlOmanualAttrOrderMap = rule113 _lhsImanualAttrOrderMap
         _tlOmergesIn = rule114 _lhsImergesIn
         _tlOnonterminals = rule115 _lhsInonterminals
         _tlOo_rename = rule116 _lhsIo_rename
         _tlOoptions = rule117 _lhsIoptions
         _tlOsynMap = rule118 _lhsIsynMap
         _tlOtypeSyns = rule119 _lhsItypeSyns
         _tlOuniq = rule120 _hdIuniq
         _tlOuseMap = rule121 _lhsIuseMap
         _tlOwrappers = rule122 _lhsIwrappers
         !__result_ = T_Nonterminals_vOut13 _lhsOcollect_nts _lhsOerrors _lhsOinhMap' _lhsOoutput _lhsOsynMap' _lhsOuniq
         in __result_ )
     in C_Nonterminals_s14 v13
   {-# INLINE rule88 #-}
   rule88 = \ ((_hdIcollect_nts) :: Set NontermIdent) ((_tlIcollect_nts) :: Set NontermIdent) ->
     _hdIcollect_nts `Set.union` _tlIcollect_nts
   {-# INLINE rule89 #-}
   rule89 = \ ((_hdIerrors) :: Seq Error) ((_tlIerrors) :: Seq Error) ->
     _hdIerrors Seq.>< _tlIerrors
   {-# INLINE rule90 #-}
   rule90 = \ ((_hdIinhMap') :: Map Identifier Attributes) ((_tlIinhMap') :: Map Identifier Attributes) ->
     _hdIinhMap' `Map.union` _tlIinhMap'
   {-# INLINE rule91 #-}
   rule91 = \ ((_hdIsynMap') :: Map Identifier Attributes) ((_tlIsynMap') :: Map Identifier Attributes) ->
     _hdIsynMap' `Map.union` _tlIsynMap'
   {-# INLINE rule92 #-}
   rule92 = \ ((_hdIoutput) :: Nonterminal) ((_tlIoutput) :: Nonterminals) ->
     (:) _hdIoutput _tlIoutput
   {-# INLINE rule93 #-}
   rule93 = \ _output ->
     _output
   {-# INLINE rule94 #-}
   rule94 = \ ((_tlIuniq) :: Int) ->
     _tlIuniq
   {-# INLINE rule95 #-}
   rule95 = \ ((_lhsIaroundsIn) :: Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) ->
     _lhsIaroundsIn
   {-# INLINE rule96 #-}
   rule96 = \ ((_lhsIaugmentsIn) :: Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) ->
     _lhsIaugmentsIn
   {-# INLINE rule97 #-}
   rule97 = \ ((_lhsIcr) :: Bool) ->
     _lhsIcr
   {-# INLINE rule98 #-}
   rule98 = \ ((_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# INLINE rule99 #-}
   rule99 = \ ((_lhsImanualAttrOrderMap) :: AttrOrderMap) ->
     _lhsImanualAttrOrderMap
   {-# INLINE rule100 #-}
   rule100 = \ ((_lhsImergesIn) :: Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier],Expression)))) ->
     _lhsImergesIn
   {-# INLINE rule101 #-}
   rule101 = \ ((_lhsInonterminals) :: Set NontermIdent) ->
     _lhsInonterminals
   {-# INLINE rule102 #-}
   rule102 = \ ((_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# INLINE rule103 #-}
   rule103 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule104 #-}
   rule104 = \ ((_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
   {-# INLINE rule105 #-}
   rule105 = \ ((_lhsItypeSyns) :: TypeSyns) ->
     _lhsItypeSyns
   {-# INLINE rule106 #-}
   rule106 = \ ((_lhsIuniq) :: Int) ->
     _lhsIuniq
   {-# INLINE rule107 #-}
   rule107 = \ ((_lhsIuseMap) :: UseMap) ->
     _lhsIuseMap
   {-# INLINE rule108 #-}
   rule108 = \ ((_lhsIwrappers) :: Set NontermIdent) ->
     _lhsIwrappers
   {-# INLINE rule109 #-}
   rule109 = \ ((_lhsIaroundsIn) :: Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) ->
     _lhsIaroundsIn
   {-# INLINE rule110 #-}
   rule110 = \ ((_lhsIaugmentsIn) :: Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) ->
     _lhsIaugmentsIn
   {-# INLINE rule111 #-}
   rule111 = \ ((_lhsIcr) :: Bool) ->
     _lhsIcr
   {-# INLINE rule112 #-}
   rule112 = \ ((_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# INLINE rule113 #-}
   rule113 = \ ((_lhsImanualAttrOrderMap) :: AttrOrderMap) ->
     _lhsImanualAttrOrderMap
   {-# INLINE rule114 #-}
   rule114 = \ ((_lhsImergesIn) :: Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier],Expression)))) ->
     _lhsImergesIn
   {-# INLINE rule115 #-}
   rule115 = \ ((_lhsInonterminals) :: Set NontermIdent) ->
     _lhsInonterminals
   {-# INLINE rule116 #-}
   rule116 = \ ((_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# INLINE rule117 #-}
   rule117 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule118 #-}
   rule118 = \ ((_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
   {-# INLINE rule119 #-}
   rule119 = \ ((_lhsItypeSyns) :: TypeSyns) ->
     _lhsItypeSyns
   {-# INLINE rule120 #-}
   rule120 = \ ((_hdIuniq) :: Int) ->
     _hdIuniq
   {-# INLINE rule121 #-}
   rule121 = \ ((_lhsIuseMap) :: UseMap) ->
     _lhsIuseMap
   {-# INLINE rule122 #-}
   rule122 = \ ((_lhsIwrappers) :: Set NontermIdent) ->
     _lhsIwrappers
{-# NOINLINE sem_Nonterminals_Nil #-}
sem_Nonterminals_Nil ::  T_Nonterminals 
sem_Nonterminals_Nil  = T_Nonterminals (return st14) where
   {-# NOINLINE st14 #-}
   !st14 = let
      v13 :: T_Nonterminals_v13 
      v13 = \ !(T_Nonterminals_vIn13 _lhsIaroundsIn _lhsIaugmentsIn _lhsIcr _lhsIinhMap _lhsImanualAttrOrderMap _lhsImergesIn _lhsInonterminals _lhsIo_rename _lhsIoptions _lhsIsynMap _lhsItypeSyns _lhsIuniq _lhsIuseMap _lhsIwrappers) -> ( let
         _lhsOcollect_nts :: Set NontermIdent
         _lhsOcollect_nts = rule123  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule124  ()
         _lhsOinhMap' :: Map Identifier Attributes
         _lhsOinhMap' = rule125  ()
         _lhsOsynMap' :: Map Identifier Attributes
         _lhsOsynMap' = rule126  ()
         _output = rule127  ()
         _lhsOoutput :: Nonterminals
         _lhsOoutput = rule128 _output
         _lhsOuniq :: Int
         _lhsOuniq = rule129 _lhsIuniq
         !__result_ = T_Nonterminals_vOut13 _lhsOcollect_nts _lhsOerrors _lhsOinhMap' _lhsOoutput _lhsOsynMap' _lhsOuniq
         in __result_ )
     in C_Nonterminals_s14 v13
   {-# INLINE rule123 #-}
   rule123 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule124 #-}
   rule124 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule125 #-}
   rule125 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule126 #-}
   rule126 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule127 #-}
   rule127 = \  (_ :: ()) ->
     []
   {-# INLINE rule128 #-}
   rule128 = \ _output ->
     _output
   {-# INLINE rule129 #-}
   rule129 = \ ((_lhsIuniq) :: Int) ->
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
        let arg = T_Pattern_vIn16 _lhsIcon _lhsInt
        !(T_Pattern_vOut16 _lhsOcontainsVars _lhsOcopy _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput) <- return (inv_Pattern_s17 sem arg)
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
                               attach_T_Pattern :: Identity (T_Pattern_s17 )
                               }
newtype T_Pattern_s17  = C_Pattern_s17 {
                                       inv_Pattern_s17 :: (T_Pattern_v16 )
                                       }
data T_Pattern_s18  = C_Pattern_s18
type T_Pattern_v16  = (T_Pattern_vIn16 ) -> (T_Pattern_vOut16 )
data T_Pattern_vIn16  = T_Pattern_vIn16 (ConstructorIdent) (NontermIdent)
data T_Pattern_vOut16  = T_Pattern_vOut16 (Bool) (Pattern) (Set (Identifier,Identifier)) (Seq Error) (Set Identifier) (Pattern)
{-# NOINLINE sem_Pattern_Constr #-}
sem_Pattern_Constr :: (ConstructorIdent) -> T_Patterns  -> T_Pattern 
sem_Pattern_Constr !arg_name_ arg_pats_ = T_Pattern (return st17) where
   {-# NOINLINE st17 #-}
   !st17 = let
      v16 :: T_Pattern_v16 
      v16 = \ !(T_Pattern_vIn16 _lhsIcon _lhsInt) -> ( let
         _patsX20 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_pats_))
         (T_Patterns_vOut19 _patsIcontainsVars _patsIcopy _patsIdefinedAttrs _patsIerrors _patsIlocals _patsIoutput) = inv_Patterns_s20 _patsX20 (T_Patterns_vIn19 _patsOcon _patsOnt)
         _lhsOcontainsVars :: Bool
         _lhsOcontainsVars = rule130 _patsIcontainsVars
         _lhsOdefinedAttrs :: Set (Identifier,Identifier)
         _lhsOdefinedAttrs = rule131 _patsIdefinedAttrs
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule132 _patsIerrors
         _lhsOlocals :: Set Identifier
         _lhsOlocals = rule133 _patsIlocals
         _copy = rule134 _patsIcopy arg_name_
         _output = rule135 _patsIoutput arg_name_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule136 _copy
         _lhsOoutput :: Pattern
         _lhsOoutput = rule137 _output
         _patsOcon = rule138 _lhsIcon
         _patsOnt = rule139 _lhsInt
         !__result_ = T_Pattern_vOut16 _lhsOcontainsVars _lhsOcopy _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput
         in __result_ )
     in C_Pattern_s17 v16
   {-# INLINE rule130 #-}
   rule130 = \ ((_patsIcontainsVars) :: Bool) ->
     _patsIcontainsVars
   {-# INLINE rule131 #-}
   rule131 = \ ((_patsIdefinedAttrs) :: Set (Identifier,Identifier)) ->
     _patsIdefinedAttrs
   {-# INLINE rule132 #-}
   rule132 = \ ((_patsIerrors) :: Seq Error) ->
     _patsIerrors
   {-# INLINE rule133 #-}
   rule133 = \ ((_patsIlocals) :: Set Identifier) ->
     _patsIlocals
   {-# INLINE rule134 #-}
   rule134 = \ ((_patsIcopy) :: Patterns) name_ ->
     Constr name_ _patsIcopy
   {-# INLINE rule135 #-}
   rule135 = \ ((_patsIoutput) :: Patterns) name_ ->
     Constr name_ _patsIoutput
   {-# INLINE rule136 #-}
   rule136 = \ _copy ->
     _copy
   {-# INLINE rule137 #-}
   rule137 = \ _output ->
     _output
   {-# INLINE rule138 #-}
   rule138 = \ ((_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# INLINE rule139 #-}
   rule139 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
{-# NOINLINE sem_Pattern_Product #-}
sem_Pattern_Product :: (Pos) -> T_Patterns  -> T_Pattern 
sem_Pattern_Product !arg_pos_ arg_pats_ = T_Pattern (return st17) where
   {-# NOINLINE st17 #-}
   !st17 = let
      v16 :: T_Pattern_v16 
      v16 = \ !(T_Pattern_vIn16 _lhsIcon _lhsInt) -> ( let
         _patsX20 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_pats_))
         (T_Patterns_vOut19 _patsIcontainsVars _patsIcopy _patsIdefinedAttrs _patsIerrors _patsIlocals _patsIoutput) = inv_Patterns_s20 _patsX20 (T_Patterns_vIn19 _patsOcon _patsOnt)
         _lhsOcontainsVars :: Bool
         _lhsOcontainsVars = rule140 _patsIcontainsVars
         _lhsOdefinedAttrs :: Set (Identifier,Identifier)
         _lhsOdefinedAttrs = rule141 _patsIdefinedAttrs
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule142 _patsIerrors
         _lhsOlocals :: Set Identifier
         _lhsOlocals = rule143 _patsIlocals
         _copy = rule144 _patsIcopy arg_pos_
         _output = rule145 _patsIoutput arg_pos_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule146 _copy
         _lhsOoutput :: Pattern
         _lhsOoutput = rule147 _output
         _patsOcon = rule148 _lhsIcon
         _patsOnt = rule149 _lhsInt
         !__result_ = T_Pattern_vOut16 _lhsOcontainsVars _lhsOcopy _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput
         in __result_ )
     in C_Pattern_s17 v16
   {-# INLINE rule140 #-}
   rule140 = \ ((_patsIcontainsVars) :: Bool) ->
     _patsIcontainsVars
   {-# INLINE rule141 #-}
   rule141 = \ ((_patsIdefinedAttrs) :: Set (Identifier,Identifier)) ->
     _patsIdefinedAttrs
   {-# INLINE rule142 #-}
   rule142 = \ ((_patsIerrors) :: Seq Error) ->
     _patsIerrors
   {-# INLINE rule143 #-}
   rule143 = \ ((_patsIlocals) :: Set Identifier) ->
     _patsIlocals
   {-# INLINE rule144 #-}
   rule144 = \ ((_patsIcopy) :: Patterns) pos_ ->
     Product pos_ _patsIcopy
   {-# INLINE rule145 #-}
   rule145 = \ ((_patsIoutput) :: Patterns) pos_ ->
     Product pos_ _patsIoutput
   {-# INLINE rule146 #-}
   rule146 = \ _copy ->
     _copy
   {-# INLINE rule147 #-}
   rule147 = \ _output ->
     _output
   {-# INLINE rule148 #-}
   rule148 = \ ((_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# INLINE rule149 #-}
   rule149 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
{-# NOINLINE sem_Pattern_Alias #-}
sem_Pattern_Alias :: (Identifier) -> (Identifier) -> T_Pattern  -> T_Pattern 
sem_Pattern_Alias !arg_field_ !arg_attr_ arg_pat_ = T_Pattern (return st17) where
   {-# NOINLINE st17 #-}
   !st17 = let
      v16 :: T_Pattern_v16 
      v16 = \ !(T_Pattern_vIn16 _lhsIcon _lhsInt) -> ( let
         _patX17 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pat_))
         (T_Pattern_vOut16 _patIcontainsVars _patIcopy _patIdefinedAttrs _patIerrors _patIlocals _patIoutput) = inv_Pattern_s17 _patX17 (T_Pattern_vIn16 _patOcon _patOnt)
         _lhsOdefinedAttrs :: Set (Identifier,Identifier)
         _lhsOdefinedAttrs = rule150 _patIdefinedAttrs arg_attr_ arg_field_
         _lhsOlocals :: Set Identifier
         _lhsOlocals = rule151 _patIlocals arg_attr_ arg_field_
         _lhsOcontainsVars :: Bool
         _lhsOcontainsVars = rule152  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule153 _patIerrors
         _copy = rule154 _patIcopy arg_attr_ arg_field_
         _output = rule155 _patIoutput arg_attr_ arg_field_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule156 _copy
         _lhsOoutput :: Pattern
         _lhsOoutput = rule157 _output
         _patOcon = rule158 _lhsIcon
         _patOnt = rule159 _lhsInt
         !__result_ = T_Pattern_vOut16 _lhsOcontainsVars _lhsOcopy _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput
         in __result_ )
     in C_Pattern_s17 v16
   {-# INLINE rule150 #-}
   {-# LINE 536 "./src-ag/DefaultRules.ag" #-}
   rule150 = \ ((_patIdefinedAttrs) :: Set (Identifier,Identifier)) attr_ field_ ->
                               {-# LINE 536 "./src-ag/DefaultRules.ag" #-}
                               Set.insert (field_,attr_) _patIdefinedAttrs
                               {-# LINE 1559 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule151 #-}
   {-# LINE 537 "./src-ag/DefaultRules.ag" #-}
   rule151 = \ ((_patIlocals) :: Set Identifier) attr_ field_ ->
                               {-# LINE 537 "./src-ag/DefaultRules.ag" #-}
                               if field_ == _LOC
                                  then Set.insert attr_ _patIlocals
                                  else _patIlocals
                               {-# LINE 1567 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule152 #-}
   {-# LINE 554 "./src-ag/DefaultRules.ag" #-}
   rule152 = \  (_ :: ()) ->
                                    {-# LINE 554 "./src-ag/DefaultRules.ag" #-}
                                    True
                                    {-# LINE 1573 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule153 #-}
   rule153 = \ ((_patIerrors) :: Seq Error) ->
     _patIerrors
   {-# INLINE rule154 #-}
   rule154 = \ ((_patIcopy) :: Pattern) attr_ field_ ->
     Alias field_ attr_ _patIcopy
   {-# INLINE rule155 #-}
   rule155 = \ ((_patIoutput) :: Pattern) attr_ field_ ->
     Alias field_ attr_ _patIoutput
   {-# INLINE rule156 #-}
   rule156 = \ _copy ->
     _copy
   {-# INLINE rule157 #-}
   rule157 = \ _output ->
     _output
   {-# INLINE rule158 #-}
   rule158 = \ ((_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# INLINE rule159 #-}
   rule159 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
{-# NOINLINE sem_Pattern_Irrefutable #-}
sem_Pattern_Irrefutable :: T_Pattern  -> T_Pattern 
sem_Pattern_Irrefutable arg_pat_ = T_Pattern (return st17) where
   {-# NOINLINE st17 #-}
   !st17 = let
      v16 :: T_Pattern_v16 
      v16 = \ !(T_Pattern_vIn16 _lhsIcon _lhsInt) -> ( let
         _patX17 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pat_))
         (T_Pattern_vOut16 _patIcontainsVars _patIcopy _patIdefinedAttrs _patIerrors _patIlocals _patIoutput) = inv_Pattern_s17 _patX17 (T_Pattern_vIn16 _patOcon _patOnt)
         _lhsOcontainsVars :: Bool
         _lhsOcontainsVars = rule160 _patIcontainsVars
         _lhsOdefinedAttrs :: Set (Identifier,Identifier)
         _lhsOdefinedAttrs = rule161 _patIdefinedAttrs
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule162 _patIerrors
         _lhsOlocals :: Set Identifier
         _lhsOlocals = rule163 _patIlocals
         _copy = rule164 _patIcopy
         _output = rule165 _patIoutput
         _lhsOcopy :: Pattern
         _lhsOcopy = rule166 _copy
         _lhsOoutput :: Pattern
         _lhsOoutput = rule167 _output
         _patOcon = rule168 _lhsIcon
         _patOnt = rule169 _lhsInt
         !__result_ = T_Pattern_vOut16 _lhsOcontainsVars _lhsOcopy _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput
         in __result_ )
     in C_Pattern_s17 v16
   {-# INLINE rule160 #-}
   rule160 = \ ((_patIcontainsVars) :: Bool) ->
     _patIcontainsVars
   {-# INLINE rule161 #-}
   rule161 = \ ((_patIdefinedAttrs) :: Set (Identifier,Identifier)) ->
     _patIdefinedAttrs
   {-# INLINE rule162 #-}
   rule162 = \ ((_patIerrors) :: Seq Error) ->
     _patIerrors
   {-# INLINE rule163 #-}
   rule163 = \ ((_patIlocals) :: Set Identifier) ->
     _patIlocals
   {-# INLINE rule164 #-}
   rule164 = \ ((_patIcopy) :: Pattern) ->
     Irrefutable _patIcopy
   {-# INLINE rule165 #-}
   rule165 = \ ((_patIoutput) :: Pattern) ->
     Irrefutable _patIoutput
   {-# INLINE rule166 #-}
   rule166 = \ _copy ->
     _copy
   {-# INLINE rule167 #-}
   rule167 = \ _output ->
     _output
   {-# INLINE rule168 #-}
   rule168 = \ ((_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# INLINE rule169 #-}
   rule169 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
{-# NOINLINE sem_Pattern_Underscore #-}
sem_Pattern_Underscore :: (Pos) -> T_Pattern 
sem_Pattern_Underscore !arg_pos_ = T_Pattern (return st17) where
   {-# NOINLINE st17 #-}
   !st17 = let
      v16 :: T_Pattern_v16 
      v16 = \ !(T_Pattern_vIn16 _lhsIcon _lhsInt) -> ( let
         _lhsOcontainsVars :: Bool
         _lhsOcontainsVars = rule170  ()
         _lhsOdefinedAttrs :: Set (Identifier,Identifier)
         _lhsOdefinedAttrs = rule171  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule172  ()
         _lhsOlocals :: Set Identifier
         _lhsOlocals = rule173  ()
         _copy = rule174 arg_pos_
         _output = rule175 arg_pos_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule176 _copy
         _lhsOoutput :: Pattern
         _lhsOoutput = rule177 _output
         !__result_ = T_Pattern_vOut16 _lhsOcontainsVars _lhsOcopy _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput
         in __result_ )
     in C_Pattern_s17 v16
   {-# INLINE rule170 #-}
   rule170 = \  (_ :: ()) ->
     False
   {-# INLINE rule171 #-}
   rule171 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule172 #-}
   rule172 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule173 #-}
   rule173 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule174 #-}
   rule174 = \ pos_ ->
     Underscore pos_
   {-# INLINE rule175 #-}
   rule175 = \ pos_ ->
     Underscore pos_
   {-# INLINE rule176 #-}
   rule176 = \ _copy ->
     _copy
   {-# INLINE rule177 #-}
   rule177 = \ _output ->
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
        let arg = T_Patterns_vIn19 _lhsIcon _lhsInt
        !(T_Patterns_vOut19 _lhsOcontainsVars _lhsOcopy _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput) <- return (inv_Patterns_s20 sem arg)
        return (Syn_Patterns _lhsOcontainsVars _lhsOcopy _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput)
   )

-- cata
{-# NOINLINE sem_Patterns #-}
sem_Patterns :: Patterns  -> T_Patterns 
sem_Patterns list = Prelude.foldr sem_Patterns_Cons sem_Patterns_Nil (Prelude.map sem_Pattern list)

-- semantic domain
newtype T_Patterns  = T_Patterns {
                                 attach_T_Patterns :: Identity (T_Patterns_s20 )
                                 }
newtype T_Patterns_s20  = C_Patterns_s20 {
                                         inv_Patterns_s20 :: (T_Patterns_v19 )
                                         }
data T_Patterns_s21  = C_Patterns_s21
type T_Patterns_v19  = (T_Patterns_vIn19 ) -> (T_Patterns_vOut19 )
data T_Patterns_vIn19  = T_Patterns_vIn19 (ConstructorIdent) (NontermIdent)
data T_Patterns_vOut19  = T_Patterns_vOut19 (Bool) (Patterns) (Set (Identifier,Identifier)) (Seq Error) (Set Identifier) (Patterns)
{-# NOINLINE sem_Patterns_Cons #-}
sem_Patterns_Cons :: T_Pattern  -> T_Patterns  -> T_Patterns 
sem_Patterns_Cons arg_hd_ arg_tl_ = T_Patterns (return st20) where
   {-# NOINLINE st20 #-}
   !st20 = let
      v19 :: T_Patterns_v19 
      v19 = \ !(T_Patterns_vIn19 _lhsIcon _lhsInt) -> ( let
         _hdX17 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_hd_))
         _tlX20 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_tl_))
         (T_Pattern_vOut16 _hdIcontainsVars _hdIcopy _hdIdefinedAttrs _hdIerrors _hdIlocals _hdIoutput) = inv_Pattern_s17 _hdX17 (T_Pattern_vIn16 _hdOcon _hdOnt)
         (T_Patterns_vOut19 _tlIcontainsVars _tlIcopy _tlIdefinedAttrs _tlIerrors _tlIlocals _tlIoutput) = inv_Patterns_s20 _tlX20 (T_Patterns_vIn19 _tlOcon _tlOnt)
         _lhsOcontainsVars :: Bool
         _lhsOcontainsVars = rule178 _hdIcontainsVars _tlIcontainsVars
         _lhsOdefinedAttrs :: Set (Identifier,Identifier)
         _lhsOdefinedAttrs = rule179 _hdIdefinedAttrs _tlIdefinedAttrs
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule180 _hdIerrors _tlIerrors
         _lhsOlocals :: Set Identifier
         _lhsOlocals = rule181 _hdIlocals _tlIlocals
         _copy = rule182 _hdIcopy _tlIcopy
         _output = rule183 _hdIoutput _tlIoutput
         _lhsOcopy :: Patterns
         _lhsOcopy = rule184 _copy
         _lhsOoutput :: Patterns
         _lhsOoutput = rule185 _output
         _hdOcon = rule186 _lhsIcon
         _hdOnt = rule187 _lhsInt
         _tlOcon = rule188 _lhsIcon
         _tlOnt = rule189 _lhsInt
         !__result_ = T_Patterns_vOut19 _lhsOcontainsVars _lhsOcopy _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput
         in __result_ )
     in C_Patterns_s20 v19
   {-# INLINE rule178 #-}
   rule178 = \ ((_hdIcontainsVars) :: Bool) ((_tlIcontainsVars) :: Bool) ->
     _hdIcontainsVars || _tlIcontainsVars
   {-# INLINE rule179 #-}
   rule179 = \ ((_hdIdefinedAttrs) :: Set (Identifier,Identifier)) ((_tlIdefinedAttrs) :: Set (Identifier,Identifier)) ->
     _hdIdefinedAttrs `Set.union` _tlIdefinedAttrs
   {-# INLINE rule180 #-}
   rule180 = \ ((_hdIerrors) :: Seq Error) ((_tlIerrors) :: Seq Error) ->
     _hdIerrors Seq.>< _tlIerrors
   {-# INLINE rule181 #-}
   rule181 = \ ((_hdIlocals) :: Set Identifier) ((_tlIlocals) :: Set Identifier) ->
     _hdIlocals `Set.union` _tlIlocals
   {-# INLINE rule182 #-}
   rule182 = \ ((_hdIcopy) :: Pattern) ((_tlIcopy) :: Patterns) ->
     (:) _hdIcopy _tlIcopy
   {-# INLINE rule183 #-}
   rule183 = \ ((_hdIoutput) :: Pattern) ((_tlIoutput) :: Patterns) ->
     (:) _hdIoutput _tlIoutput
   {-# INLINE rule184 #-}
   rule184 = \ _copy ->
     _copy
   {-# INLINE rule185 #-}
   rule185 = \ _output ->
     _output
   {-# INLINE rule186 #-}
   rule186 = \ ((_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# INLINE rule187 #-}
   rule187 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule188 #-}
   rule188 = \ ((_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# INLINE rule189 #-}
   rule189 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
{-# NOINLINE sem_Patterns_Nil #-}
sem_Patterns_Nil ::  T_Patterns 
sem_Patterns_Nil  = T_Patterns (return st20) where
   {-# NOINLINE st20 #-}
   !st20 = let
      v19 :: T_Patterns_v19 
      v19 = \ !(T_Patterns_vIn19 _lhsIcon _lhsInt) -> ( let
         _lhsOcontainsVars :: Bool
         _lhsOcontainsVars = rule190  ()
         _lhsOdefinedAttrs :: Set (Identifier,Identifier)
         _lhsOdefinedAttrs = rule191  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule192  ()
         _lhsOlocals :: Set Identifier
         _lhsOlocals = rule193  ()
         _copy = rule194  ()
         _output = rule195  ()
         _lhsOcopy :: Patterns
         _lhsOcopy = rule196 _copy
         _lhsOoutput :: Patterns
         _lhsOoutput = rule197 _output
         !__result_ = T_Patterns_vOut19 _lhsOcontainsVars _lhsOcopy _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput
         in __result_ )
     in C_Patterns_s20 v19
   {-# INLINE rule190 #-}
   rule190 = \  (_ :: ()) ->
     False
   {-# INLINE rule191 #-}
   rule191 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule192 #-}
   rule192 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule193 #-}
   rule193 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule194 #-}
   rule194 = \  (_ :: ()) ->
     []
   {-# INLINE rule195 #-}
   rule195 = \  (_ :: ()) ->
     []
   {-# INLINE rule196 #-}
   rule196 = \ _copy ->
     _copy
   {-# INLINE rule197 #-}
   rule197 = \ _output ->
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
        let arg = T_Production_vIn22 _lhsIaroundsIn _lhsIaugmentsIn _lhsIcr _lhsIinh _lhsIinhMap _lhsIinhOrig _lhsImanualAttrOrderMap _lhsImergesIn _lhsInonterminals _lhsInt _lhsIo_rename _lhsIoptions _lhsIparams _lhsIsyn _lhsIsynMap _lhsIsynOrig _lhsItypeSyns _lhsIuniq _lhsIuseMap _lhsIwrappers
        !(T_Production_vOut22 _lhsOerrors _lhsOoutput _lhsOuniq) <- return (inv_Production_s23 sem arg)
        return (Syn_Production _lhsOerrors _lhsOoutput _lhsOuniq)
   )

-- cata
{-# INLINE sem_Production #-}
sem_Production :: Production  -> T_Production 
sem_Production ( Production !con_ !params_ !constraints_ children_ rules_ typeSigs_ !macro_ ) = sem_Production_Production con_ params_ constraints_ ( sem_Children children_ ) ( sem_Rules rules_ ) ( sem_TypeSigs typeSigs_ ) macro_

-- semantic domain
newtype T_Production  = T_Production {
                                     attach_T_Production :: Identity (T_Production_s23 )
                                     }
newtype T_Production_s23  = C_Production_s23 {
                                             inv_Production_s23 :: (T_Production_v22 )
                                             }
data T_Production_s24  = C_Production_s24
type T_Production_v22  = (T_Production_vIn22 ) -> (T_Production_vOut22 )
data T_Production_vIn22  = T_Production_vIn22 (Map ConstructorIdent (Map Identifier [Expression])) (Map ConstructorIdent (Map Identifier [Expression])) (Bool) (Attributes) (Map Identifier Attributes) (Attributes) (AttrOrderMap) (Map ConstructorIdent (Map Identifier (Identifier,[Identifier],Expression))) (Set NontermIdent) (NontermIdent) (Bool) (Options) ([Identifier]) (Attributes) (Map Identifier Attributes) (Attributes) (TypeSyns) (Int) (Map Identifier (String,String,String)) (Set NontermIdent)
data T_Production_vOut22  = T_Production_vOut22 (Seq Error) (Production) (Int)
{-# NOINLINE sem_Production_Production #-}
sem_Production_Production :: (ConstructorIdent) -> ([Identifier]) -> ([Type]) -> T_Children  -> T_Rules  -> T_TypeSigs  -> (MaybeMacro) -> T_Production 
sem_Production_Production !arg_con_ !arg_params_ !arg_constraints_ arg_children_ arg_rules_ arg_typeSigs_ !arg_macro_ = T_Production (return st23) where
   {-# NOINLINE st23 #-}
   !st23 = let
      v22 :: T_Production_v22 
      v22 = \ !(T_Production_vIn22 _lhsIaroundsIn _lhsIaugmentsIn _lhsIcr _lhsIinh _lhsIinhMap _lhsIinhOrig _lhsImanualAttrOrderMap _lhsImergesIn _lhsInonterminals _lhsInt _lhsIo_rename _lhsIoptions _lhsIparams _lhsIsyn _lhsIsynMap _lhsIsynOrig _lhsItypeSyns _lhsIuniq _lhsIuseMap _lhsIwrappers) -> ( let
         _childrenX5 = Control.Monad.Identity.runIdentity (attach_T_Children (arg_children_))
         _rulesX32 = Control.Monad.Identity.runIdentity (attach_T_Rules (arg_rules_))
         _typeSigsX38 = Control.Monad.Identity.runIdentity (attach_T_TypeSigs (arg_typeSigs_))
         (T_Children_vOut4 _childrenIerrors _childrenIfields _childrenIinputs _childrenIoutput _childrenIoutputs) = inv_Children_s5 _childrenX5 (T_Children_vIn4 _childrenOcon _childrenOcr _childrenOinhMap _childrenOmerged _childrenOnt _childrenOparams _childrenOsynMap)
         (T_Rules_vOut31 _rulesIdefinedAttrs _rulesIerrors _rulesIlocals _rulesIoutput _rulesIruleNames _rulesIuniq) = inv_Rules_s32 _rulesX32 (T_Rules_vIn31 _rulesOcon _rulesOnt _rulesOoptions _rulesOuniq)
         (T_TypeSigs_vOut37 _typeSigsIoutput) = inv_TypeSigs_s38 _typeSigsX38 (T_TypeSigs_vIn37 _typeSigsOnt _typeSigsOparams)
         _rulesOcon = rule198 arg_con_
         _childrenOcon = rule199 arg_con_
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule200 _childrenIerrors _errs _orderErrs _rulesIerrors
         (_newRls,_errs) = rule201 _childrenIfields _childrenIinputs _childrenIoutputs _lhsIcr _lhsIinh _lhsInt _lhsIo_rename _lhsIoptions _lhsIsyn _lhsIsynOrig _lhsItypeSyns _lhsIuseMap _lhsIwrappers _rulesIdefinedAttrs _rulesIlocals arg_con_
         _extra1 = rule202 _augmentsIn _newRls _rulesIoutput
         _extra2 = rule203 _aroundsIn _extra1
         _extra3 = rule204 _extra2 _mergesIn
         _lhsOoutput :: Production
         _lhsOoutput = rule205 _childrenIoutput _extra3 _typeSigsIoutput arg_con_ arg_constraints_ arg_macro_ arg_params_
         _orderDeps = rule206 _lhsImanualAttrOrderMap _lhsInt arg_con_
         _orderErrs = rule207 _childrenIinputs _childrenIoutputs _lhsIinh _lhsInt _lhsIsyn _orderDeps _rulesIlocals _rulesIruleNames arg_con_
         _augmentsIn = rule208 _lhsIaugmentsIn arg_con_
         _aroundsIn = rule209 _lhsIaroundsIn arg_con_
         _mergesIn = rule210 _lhsImergesIn arg_con_
         _merged = rule211 _mergesIn
         _output = rule212 _childrenIoutput _rulesIoutput _typeSigsIoutput arg_con_ arg_constraints_ arg_macro_ arg_params_
         _lhsOuniq :: Int
         _lhsOuniq = rule213 _rulesIuniq
         _childrenOcr = rule214 _lhsIcr
         _childrenOinhMap = rule215 _lhsIinhMap
         _childrenOmerged = rule216 _merged
         _childrenOnt = rule217 _lhsInt
         _childrenOparams = rule218 _lhsIparams
         _childrenOsynMap = rule219 _lhsIsynMap
         _rulesOnt = rule220 _lhsInt
         _rulesOoptions = rule221 _lhsIoptions
         _rulesOuniq = rule222 _lhsIuniq
         _typeSigsOnt = rule223 _lhsInt
         _typeSigsOparams = rule224 _lhsIparams
         !__result_ = T_Production_vOut22 _lhsOerrors _lhsOoutput _lhsOuniq
         in __result_ )
     in C_Production_s23 v22
   {-# INLINE rule198 #-}
   {-# LINE 197 "./src-ag/DefaultRules.ag" #-}
   rule198 = \ con_ ->
                                 {-# LINE 197 "./src-ag/DefaultRules.ag" #-}
                                 con_
                                 {-# LINE 1930 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule199 #-}
   {-# LINE 198 "./src-ag/DefaultRules.ag" #-}
   rule199 = \ con_ ->
                                 {-# LINE 198 "./src-ag/DefaultRules.ag" #-}
                                 con_
                                 {-# LINE 1936 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule200 #-}
   {-# LINE 384 "./src-ag/DefaultRules.ag" #-}
   rule200 = \ ((_childrenIerrors) :: Seq Error) _errs _orderErrs ((_rulesIerrors) :: Seq Error) ->
                  {-# LINE 384 "./src-ag/DefaultRules.ag" #-}
                  _childrenIerrors >< _errs >< _rulesIerrors >< _orderErrs
                  {-# LINE 1942 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule201 #-}
   {-# LINE 388 "./src-ag/DefaultRules.ag" #-}
   rule201 = \ ((_childrenIfields) :: [(Identifier,Type,ChildKind)]) ((_childrenIinputs) :: [(Identifier, Attributes)]) ((_childrenIoutputs) :: [(Identifier, Attributes)]) ((_lhsIcr) :: Bool) ((_lhsIinh) :: Attributes) ((_lhsInt) :: NontermIdent) ((_lhsIo_rename) :: Bool) ((_lhsIoptions) :: Options) ((_lhsIsyn) :: Attributes) ((_lhsIsynOrig) :: Attributes) ((_lhsItypeSyns) :: TypeSyns) ((_lhsIuseMap) :: Map Identifier (String,String,String)) ((_lhsIwrappers) :: Set NontermIdent) ((_rulesIdefinedAttrs) :: Set (Identifier,Identifier)) ((_rulesIlocals) :: Set Identifier) con_ ->
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
      {-# LINE 2003 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule202 #-}
   {-# LINE 608 "./src-ag/DefaultRules.ag" #-}
   rule202 = \ _augmentsIn _newRls ((_rulesIoutput) :: Rules) ->
                     {-# LINE 608 "./src-ag/DefaultRules.ag" #-}
                     foldr addAugments (_rulesIoutput ++ _newRls) (Map.assocs _augmentsIn    )
                     {-# LINE 2009 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule203 #-}
   {-# LINE 609 "./src-ag/DefaultRules.ag" #-}
   rule203 = \ _aroundsIn _extra1 ->
                     {-# LINE 609 "./src-ag/DefaultRules.ag" #-}
                     foldr addArounds _extra1     (Map.assocs _aroundsIn    )
                     {-# LINE 2015 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule204 #-}
   {-# LINE 610 "./src-ag/DefaultRules.ag" #-}
   rule204 = \ _extra2 _mergesIn ->
                     {-# LINE 610 "./src-ag/DefaultRules.ag" #-}
                     foldr addMerges _extra2     (Map.assocs _mergesIn    )
                     {-# LINE 2021 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule205 #-}
   {-# LINE 611 "./src-ag/DefaultRules.ag" #-}
   rule205 = \ ((_childrenIoutput) :: Children) _extra3 ((_typeSigsIoutput) :: TypeSigs) con_ constraints_ macro_ params_ ->
                     {-# LINE 611 "./src-ag/DefaultRules.ag" #-}
                     Production con_ params_ constraints_ _childrenIoutput _extra3     _typeSigsIoutput macro_
                     {-# LINE 2027 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule206 #-}
   {-# LINE 719 "./src-ag/DefaultRules.ag" #-}
   rule206 = \ ((_lhsImanualAttrOrderMap) :: AttrOrderMap) ((_lhsInt) :: NontermIdent) con_ ->
                        {-# LINE 719 "./src-ag/DefaultRules.ag" #-}
                        Set.toList $ Map.findWithDefault Set.empty con_ $ Map.findWithDefault Map.empty _lhsInt _lhsImanualAttrOrderMap
                        {-# LINE 2033 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule207 #-}
   {-# LINE 722 "./src-ag/DefaultRules.ag" #-}
   rule207 = \ ((_childrenIinputs) :: [(Identifier, Attributes)]) ((_childrenIoutputs) :: [(Identifier, Attributes)]) ((_lhsIinh) :: Attributes) ((_lhsInt) :: NontermIdent) ((_lhsIsyn) :: Attributes) _orderDeps ((_rulesIlocals) :: Set Identifier) ((_rulesIruleNames) :: Set Identifier) con_ ->
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
            {-# LINE 2073 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule208 #-}
   {-# LINE 777 "./src-ag/DefaultRules.ag" #-}
   rule208 = \ ((_lhsIaugmentsIn) :: Map ConstructorIdent (Map Identifier [Expression])) con_ ->
                                                  {-# LINE 777 "./src-ag/DefaultRules.ag" #-}
                                                  Map.findWithDefault Map.empty con_ _lhsIaugmentsIn
                                                  {-# LINE 2079 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule209 #-}
   {-# LINE 784 "./src-ag/DefaultRules.ag" #-}
   rule209 = \ ((_lhsIaroundsIn) :: Map ConstructorIdent (Map Identifier [Expression])) con_ ->
                                                   {-# LINE 784 "./src-ag/DefaultRules.ag" #-}
                                                   Map.findWithDefault Map.empty con_ _lhsIaroundsIn
                                                   {-# LINE 2085 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule210 #-}
   {-# LINE 792 "./src-ag/DefaultRules.ag" #-}
   rule210 = \ ((_lhsImergesIn) :: Map ConstructorIdent (Map Identifier (Identifier,[Identifier],Expression))) con_ ->
                                                  {-# LINE 792 "./src-ag/DefaultRules.ag" #-}
                                                  Map.findWithDefault Map.empty con_ _lhsImergesIn
                                                  {-# LINE 2091 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule211 #-}
   {-# LINE 793 "./src-ag/DefaultRules.ag" #-}
   rule211 = \ _mergesIn ->
                                                  {-# LINE 793 "./src-ag/DefaultRules.ag" #-}
                                                  Set.fromList [ c | (_,cs,_) <- Map.elems _mergesIn    , c <- cs ]
                                                  {-# LINE 2097 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule212 #-}
   rule212 = \ ((_childrenIoutput) :: Children) ((_rulesIoutput) :: Rules) ((_typeSigsIoutput) :: TypeSigs) con_ constraints_ macro_ params_ ->
     Production con_ params_ constraints_ _childrenIoutput _rulesIoutput _typeSigsIoutput macro_
   {-# INLINE rule213 #-}
   rule213 = \ ((_rulesIuniq) :: Int) ->
     _rulesIuniq
   {-# INLINE rule214 #-}
   rule214 = \ ((_lhsIcr) :: Bool) ->
     _lhsIcr
   {-# INLINE rule215 #-}
   rule215 = \ ((_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# INLINE rule216 #-}
   rule216 = \ _merged ->
     _merged
   {-# INLINE rule217 #-}
   rule217 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule218 #-}
   rule218 = \ ((_lhsIparams) :: [Identifier]) ->
     _lhsIparams
   {-# INLINE rule219 #-}
   rule219 = \ ((_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
   {-# INLINE rule220 #-}
   rule220 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule221 #-}
   rule221 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule222 #-}
   rule222 = \ ((_lhsIuniq) :: Int) ->
     _lhsIuniq
   {-# INLINE rule223 #-}
   rule223 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule224 #-}
   rule224 = \ ((_lhsIparams) :: [Identifier]) ->
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
        let arg = T_Productions_vIn25 _lhsIaroundsIn _lhsIaugmentsIn _lhsIcr _lhsIinh _lhsIinhMap _lhsIinhOrig _lhsImanualAttrOrderMap _lhsImergesIn _lhsInonterminals _lhsInt _lhsIo_rename _lhsIoptions _lhsIparams _lhsIsyn _lhsIsynMap _lhsIsynOrig _lhsItypeSyns _lhsIuniq _lhsIuseMap _lhsIwrappers
        !(T_Productions_vOut25 _lhsOerrors _lhsOoutput _lhsOuniq) <- return (inv_Productions_s26 sem arg)
        return (Syn_Productions _lhsOerrors _lhsOoutput _lhsOuniq)
   )

-- cata
{-# NOINLINE sem_Productions #-}
sem_Productions :: Productions  -> T_Productions 
sem_Productions list = Prelude.foldr sem_Productions_Cons sem_Productions_Nil (Prelude.map sem_Production list)

-- semantic domain
newtype T_Productions  = T_Productions {
                                       attach_T_Productions :: Identity (T_Productions_s26 )
                                       }
newtype T_Productions_s26  = C_Productions_s26 {
                                               inv_Productions_s26 :: (T_Productions_v25 )
                                               }
data T_Productions_s27  = C_Productions_s27
type T_Productions_v25  = (T_Productions_vIn25 ) -> (T_Productions_vOut25 )
data T_Productions_vIn25  = T_Productions_vIn25 (Map ConstructorIdent (Map Identifier [Expression])) (Map ConstructorIdent (Map Identifier [Expression])) (Bool) (Attributes) (Map Identifier Attributes) (Attributes) (AttrOrderMap) (Map ConstructorIdent (Map Identifier (Identifier,[Identifier],Expression))) (Set NontermIdent) (NontermIdent) (Bool) (Options) ([Identifier]) (Attributes) (Map Identifier Attributes) (Attributes) (TypeSyns) (Int) (Map Identifier (String,String,String)) (Set NontermIdent)
data T_Productions_vOut25  = T_Productions_vOut25 (Seq Error) (Productions) (Int)
{-# NOINLINE sem_Productions_Cons #-}
sem_Productions_Cons :: T_Production  -> T_Productions  -> T_Productions 
sem_Productions_Cons arg_hd_ arg_tl_ = T_Productions (return st26) where
   {-# NOINLINE st26 #-}
   !st26 = let
      v25 :: T_Productions_v25 
      v25 = \ !(T_Productions_vIn25 _lhsIaroundsIn _lhsIaugmentsIn _lhsIcr _lhsIinh _lhsIinhMap _lhsIinhOrig _lhsImanualAttrOrderMap _lhsImergesIn _lhsInonterminals _lhsInt _lhsIo_rename _lhsIoptions _lhsIparams _lhsIsyn _lhsIsynMap _lhsIsynOrig _lhsItypeSyns _lhsIuniq _lhsIuseMap _lhsIwrappers) -> ( let
         _hdX23 = Control.Monad.Identity.runIdentity (attach_T_Production (arg_hd_))
         _tlX26 = Control.Monad.Identity.runIdentity (attach_T_Productions (arg_tl_))
         (T_Production_vOut22 _hdIerrors _hdIoutput _hdIuniq) = inv_Production_s23 _hdX23 (T_Production_vIn22 _hdOaroundsIn _hdOaugmentsIn _hdOcr _hdOinh _hdOinhMap _hdOinhOrig _hdOmanualAttrOrderMap _hdOmergesIn _hdOnonterminals _hdOnt _hdOo_rename _hdOoptions _hdOparams _hdOsyn _hdOsynMap _hdOsynOrig _hdOtypeSyns _hdOuniq _hdOuseMap _hdOwrappers)
         (T_Productions_vOut25 _tlIerrors _tlIoutput _tlIuniq) = inv_Productions_s26 _tlX26 (T_Productions_vIn25 _tlOaroundsIn _tlOaugmentsIn _tlOcr _tlOinh _tlOinhMap _tlOinhOrig _tlOmanualAttrOrderMap _tlOmergesIn _tlOnonterminals _tlOnt _tlOo_rename _tlOoptions _tlOparams _tlOsyn _tlOsynMap _tlOsynOrig _tlOtypeSyns _tlOuniq _tlOuseMap _tlOwrappers)
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule225 _hdIerrors _tlIerrors
         _output = rule226 _hdIoutput _tlIoutput
         _lhsOoutput :: Productions
         _lhsOoutput = rule227 _output
         _lhsOuniq :: Int
         _lhsOuniq = rule228 _tlIuniq
         _hdOaroundsIn = rule229 _lhsIaroundsIn
         _hdOaugmentsIn = rule230 _lhsIaugmentsIn
         _hdOcr = rule231 _lhsIcr
         _hdOinh = rule232 _lhsIinh
         _hdOinhMap = rule233 _lhsIinhMap
         _hdOinhOrig = rule234 _lhsIinhOrig
         _hdOmanualAttrOrderMap = rule235 _lhsImanualAttrOrderMap
         _hdOmergesIn = rule236 _lhsImergesIn
         _hdOnonterminals = rule237 _lhsInonterminals
         _hdOnt = rule238 _lhsInt
         _hdOo_rename = rule239 _lhsIo_rename
         _hdOoptions = rule240 _lhsIoptions
         _hdOparams = rule241 _lhsIparams
         _hdOsyn = rule242 _lhsIsyn
         _hdOsynMap = rule243 _lhsIsynMap
         _hdOsynOrig = rule244 _lhsIsynOrig
         _hdOtypeSyns = rule245 _lhsItypeSyns
         _hdOuniq = rule246 _lhsIuniq
         _hdOuseMap = rule247 _lhsIuseMap
         _hdOwrappers = rule248 _lhsIwrappers
         _tlOaroundsIn = rule249 _lhsIaroundsIn
         _tlOaugmentsIn = rule250 _lhsIaugmentsIn
         _tlOcr = rule251 _lhsIcr
         _tlOinh = rule252 _lhsIinh
         _tlOinhMap = rule253 _lhsIinhMap
         _tlOinhOrig = rule254 _lhsIinhOrig
         _tlOmanualAttrOrderMap = rule255 _lhsImanualAttrOrderMap
         _tlOmergesIn = rule256 _lhsImergesIn
         _tlOnonterminals = rule257 _lhsInonterminals
         _tlOnt = rule258 _lhsInt
         _tlOo_rename = rule259 _lhsIo_rename
         _tlOoptions = rule260 _lhsIoptions
         _tlOparams = rule261 _lhsIparams
         _tlOsyn = rule262 _lhsIsyn
         _tlOsynMap = rule263 _lhsIsynMap
         _tlOsynOrig = rule264 _lhsIsynOrig
         _tlOtypeSyns = rule265 _lhsItypeSyns
         _tlOuniq = rule266 _hdIuniq
         _tlOuseMap = rule267 _lhsIuseMap
         _tlOwrappers = rule268 _lhsIwrappers
         !__result_ = T_Productions_vOut25 _lhsOerrors _lhsOoutput _lhsOuniq
         in __result_ )
     in C_Productions_s26 v25
   {-# INLINE rule225 #-}
   rule225 = \ ((_hdIerrors) :: Seq Error) ((_tlIerrors) :: Seq Error) ->
     _hdIerrors Seq.>< _tlIerrors
   {-# INLINE rule226 #-}
   rule226 = \ ((_hdIoutput) :: Production) ((_tlIoutput) :: Productions) ->
     (:) _hdIoutput _tlIoutput
   {-# INLINE rule227 #-}
   rule227 = \ _output ->
     _output
   {-# INLINE rule228 #-}
   rule228 = \ ((_tlIuniq) :: Int) ->
     _tlIuniq
   {-# INLINE rule229 #-}
   rule229 = \ ((_lhsIaroundsIn) :: Map ConstructorIdent (Map Identifier [Expression])) ->
     _lhsIaroundsIn
   {-# INLINE rule230 #-}
   rule230 = \ ((_lhsIaugmentsIn) :: Map ConstructorIdent (Map Identifier [Expression])) ->
     _lhsIaugmentsIn
   {-# INLINE rule231 #-}
   rule231 = \ ((_lhsIcr) :: Bool) ->
     _lhsIcr
   {-# INLINE rule232 #-}
   rule232 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule233 #-}
   rule233 = \ ((_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# INLINE rule234 #-}
   rule234 = \ ((_lhsIinhOrig) :: Attributes) ->
     _lhsIinhOrig
   {-# INLINE rule235 #-}
   rule235 = \ ((_lhsImanualAttrOrderMap) :: AttrOrderMap) ->
     _lhsImanualAttrOrderMap
   {-# INLINE rule236 #-}
   rule236 = \ ((_lhsImergesIn) :: Map ConstructorIdent (Map Identifier (Identifier,[Identifier],Expression))) ->
     _lhsImergesIn
   {-# INLINE rule237 #-}
   rule237 = \ ((_lhsInonterminals) :: Set NontermIdent) ->
     _lhsInonterminals
   {-# INLINE rule238 #-}
   rule238 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule239 #-}
   rule239 = \ ((_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# INLINE rule240 #-}
   rule240 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule241 #-}
   rule241 = \ ((_lhsIparams) :: [Identifier]) ->
     _lhsIparams
   {-# INLINE rule242 #-}
   rule242 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
   {-# INLINE rule243 #-}
   rule243 = \ ((_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
   {-# INLINE rule244 #-}
   rule244 = \ ((_lhsIsynOrig) :: Attributes) ->
     _lhsIsynOrig
   {-# INLINE rule245 #-}
   rule245 = \ ((_lhsItypeSyns) :: TypeSyns) ->
     _lhsItypeSyns
   {-# INLINE rule246 #-}
   rule246 = \ ((_lhsIuniq) :: Int) ->
     _lhsIuniq
   {-# INLINE rule247 #-}
   rule247 = \ ((_lhsIuseMap) :: Map Identifier (String,String,String)) ->
     _lhsIuseMap
   {-# INLINE rule248 #-}
   rule248 = \ ((_lhsIwrappers) :: Set NontermIdent) ->
     _lhsIwrappers
   {-# INLINE rule249 #-}
   rule249 = \ ((_lhsIaroundsIn) :: Map ConstructorIdent (Map Identifier [Expression])) ->
     _lhsIaroundsIn
   {-# INLINE rule250 #-}
   rule250 = \ ((_lhsIaugmentsIn) :: Map ConstructorIdent (Map Identifier [Expression])) ->
     _lhsIaugmentsIn
   {-# INLINE rule251 #-}
   rule251 = \ ((_lhsIcr) :: Bool) ->
     _lhsIcr
   {-# INLINE rule252 #-}
   rule252 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule253 #-}
   rule253 = \ ((_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# INLINE rule254 #-}
   rule254 = \ ((_lhsIinhOrig) :: Attributes) ->
     _lhsIinhOrig
   {-# INLINE rule255 #-}
   rule255 = \ ((_lhsImanualAttrOrderMap) :: AttrOrderMap) ->
     _lhsImanualAttrOrderMap
   {-# INLINE rule256 #-}
   rule256 = \ ((_lhsImergesIn) :: Map ConstructorIdent (Map Identifier (Identifier,[Identifier],Expression))) ->
     _lhsImergesIn
   {-# INLINE rule257 #-}
   rule257 = \ ((_lhsInonterminals) :: Set NontermIdent) ->
     _lhsInonterminals
   {-# INLINE rule258 #-}
   rule258 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule259 #-}
   rule259 = \ ((_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# INLINE rule260 #-}
   rule260 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule261 #-}
   rule261 = \ ((_lhsIparams) :: [Identifier]) ->
     _lhsIparams
   {-# INLINE rule262 #-}
   rule262 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
   {-# INLINE rule263 #-}
   rule263 = \ ((_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
   {-# INLINE rule264 #-}
   rule264 = \ ((_lhsIsynOrig) :: Attributes) ->
     _lhsIsynOrig
   {-# INLINE rule265 #-}
   rule265 = \ ((_lhsItypeSyns) :: TypeSyns) ->
     _lhsItypeSyns
   {-# INLINE rule266 #-}
   rule266 = \ ((_hdIuniq) :: Int) ->
     _hdIuniq
   {-# INLINE rule267 #-}
   rule267 = \ ((_lhsIuseMap) :: Map Identifier (String,String,String)) ->
     _lhsIuseMap
   {-# INLINE rule268 #-}
   rule268 = \ ((_lhsIwrappers) :: Set NontermIdent) ->
     _lhsIwrappers
{-# NOINLINE sem_Productions_Nil #-}
sem_Productions_Nil ::  T_Productions 
sem_Productions_Nil  = T_Productions (return st26) where
   {-# NOINLINE st26 #-}
   !st26 = let
      v25 :: T_Productions_v25 
      v25 = \ !(T_Productions_vIn25 _lhsIaroundsIn _lhsIaugmentsIn _lhsIcr _lhsIinh _lhsIinhMap _lhsIinhOrig _lhsImanualAttrOrderMap _lhsImergesIn _lhsInonterminals _lhsInt _lhsIo_rename _lhsIoptions _lhsIparams _lhsIsyn _lhsIsynMap _lhsIsynOrig _lhsItypeSyns _lhsIuniq _lhsIuseMap _lhsIwrappers) -> ( let
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule269  ()
         _output = rule270  ()
         _lhsOoutput :: Productions
         _lhsOoutput = rule271 _output
         _lhsOuniq :: Int
         _lhsOuniq = rule272 _lhsIuniq
         !__result_ = T_Productions_vOut25 _lhsOerrors _lhsOoutput _lhsOuniq
         in __result_ )
     in C_Productions_s26 v25
   {-# INLINE rule269 #-}
   rule269 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule270 #-}
   rule270 = \  (_ :: ()) ->
     []
   {-# INLINE rule271 #-}
   rule271 = \ _output ->
     _output
   {-# INLINE rule272 #-}
   rule272 = \ ((_lhsIuniq) :: Int) ->
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
        let arg = T_Rule_vIn28 _lhsIcon _lhsInt _lhsIoptions _lhsIuniq
        !(T_Rule_vOut28 _lhsOcontainsVars _lhsOdefinedAttrs _lhsOerrors _lhsOisPure _lhsOlocals _lhsOoutput _lhsOoutputs _lhsOruleNames _lhsOuniq) <- return (inv_Rule_s29 sem arg)
        return (Syn_Rule _lhsOcontainsVars _lhsOdefinedAttrs _lhsOerrors _lhsOisPure _lhsOlocals _lhsOoutput _lhsOoutputs _lhsOruleNames _lhsOuniq)
   )

-- cata
{-# INLINE sem_Rule #-}
sem_Rule :: Rule  -> T_Rule 
sem_Rule ( Rule !mbName_ pattern_ !rhs_ !owrt_ !origin_ !explicit_ !pure_ !identity_ !mbError_ !eager_ ) = sem_Rule_Rule mbName_ ( sem_Pattern pattern_ ) rhs_ owrt_ origin_ explicit_ pure_ identity_ mbError_ eager_

-- semantic domain
newtype T_Rule  = T_Rule {
                         attach_T_Rule :: Identity (T_Rule_s29 )
                         }
newtype T_Rule_s29  = C_Rule_s29 {
                                 inv_Rule_s29 :: (T_Rule_v28 )
                                 }
data T_Rule_s30  = C_Rule_s30
type T_Rule_v28  = (T_Rule_vIn28 ) -> (T_Rule_vOut28 )
data T_Rule_vIn28  = T_Rule_vIn28 (ConstructorIdent) (NontermIdent) (Options) (Int)
data T_Rule_vOut28  = T_Rule_vOut28 (Bool) (Set (Identifier,Identifier)) (Seq Error) (Bool) (Set Identifier) (Rule) (Rules) (Set Identifier) (Int)
{-# NOINLINE sem_Rule_Rule #-}
sem_Rule_Rule :: (Maybe Identifier) -> T_Pattern  -> (Expression) -> (Bool) -> (String) -> (Bool) -> (Bool) -> (Bool) -> (Maybe Error) -> (Bool) -> T_Rule 
sem_Rule_Rule !arg_mbName_ arg_pattern_ !arg_rhs_ !arg_owrt_ !arg_origin_ !arg_explicit_ !arg_pure_ !arg_identity_ !arg_mbError_ !arg_eager_ = T_Rule (return st29) where
   {-# NOINLINE st29 #-}
   !st29 = let
      v28 :: T_Rule_v28 
      v28 = \ !(T_Rule_vIn28 _lhsIcon _lhsInt _lhsIoptions _lhsIuniq) -> ( let
         _patternX17 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pattern_))
         (T_Pattern_vOut16 _patternIcontainsVars _patternIcopy _patternIdefinedAttrs _patternIerrors _patternIlocals _patternIoutput) = inv_Pattern_s17 _patternX17 (T_Pattern_vIn16 _patternOcon _patternOnt)
         _lhsOisPure :: Bool
         _lhsOisPure = rule273 arg_pure_
         (_output1,_mbAlias) = rule274 _output
         _lhsOuniq :: Int
         (_outputs,_lhsOuniq) = rule275 _lhsIoptions _lhsIuniq _output1
         _lhsOoutputs :: Rules
         _lhsOoutputs = rule276 _mbAlias _outputs
         _lhsOruleNames :: Set Identifier
         _lhsOruleNames = rule277 arg_mbName_
         _lhsOcontainsVars :: Bool
         _lhsOcontainsVars = rule278 _patternIcontainsVars
         _lhsOdefinedAttrs :: Set (Identifier,Identifier)
         _lhsOdefinedAttrs = rule279 _patternIdefinedAttrs
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule280 _patternIerrors
         _lhsOlocals :: Set Identifier
         _lhsOlocals = rule281 _patternIlocals
         _output = rule282 _patternIoutput arg_eager_ arg_explicit_ arg_identity_ arg_mbError_ arg_mbName_ arg_origin_ arg_owrt_ arg_pure_ arg_rhs_
         _lhsOoutput :: Rule
         _lhsOoutput = rule283 _output
         _patternOcon = rule284 _lhsIcon
         _patternOnt = rule285 _lhsInt
         !__result_ = T_Rule_vOut28 _lhsOcontainsVars _lhsOdefinedAttrs _lhsOerrors _lhsOisPure _lhsOlocals _lhsOoutput _lhsOoutputs _lhsOruleNames _lhsOuniq
         in __result_ )
     in C_Rule_s29 v28
   {-# INLINE rule273 #-}
   {-# LINE 557 "./src-ag/DefaultRules.ag" #-}
   rule273 = \ pure_ ->
                                {-# LINE 557 "./src-ag/DefaultRules.ag" #-}
                                pure_
                                {-# LINE 2460 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule274 #-}
   {-# LINE 624 "./src-ag/DefaultRules.ag" #-}
   rule274 = \ _output ->
                                         {-# LINE 624 "./src-ag/DefaultRules.ag" #-}
                                         mkRuleAlias _output
                                         {-# LINE 2466 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule275 #-}
   {-# LINE 625 "./src-ag/DefaultRules.ag" #-}
   rule275 = \ ((_lhsIoptions) :: Options) ((_lhsIuniq) :: Int) _output1 ->
                                      {-# LINE 625 "./src-ag/DefaultRules.ag" #-}
                                      if needsMultiRules _lhsIoptions
                                      then multiRule _output1     _lhsIuniq
                                      else ([_output1    ], _lhsIuniq)
                                      {-# LINE 2474 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule276 #-}
   {-# LINE 628 "./src-ag/DefaultRules.ag" #-}
   rule276 = \ _mbAlias _outputs ->
                          {-# LINE 628 "./src-ag/DefaultRules.ag" #-}
                          maybe [] return _mbAlias     ++ _outputs
                          {-# LINE 2480 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule277 #-}
   {-# LINE 713 "./src-ag/DefaultRules.ag" #-}
   rule277 = \ mbName_ ->
                                   {-# LINE 713 "./src-ag/DefaultRules.ag" #-}
                                   case mbName_ of
                                     Nothing -> Set.empty
                                     Just nm -> Set.singleton nm
                                   {-# LINE 2488 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule278 #-}
   rule278 = \ ((_patternIcontainsVars) :: Bool) ->
     _patternIcontainsVars
   {-# INLINE rule279 #-}
   rule279 = \ ((_patternIdefinedAttrs) :: Set (Identifier,Identifier)) ->
     _patternIdefinedAttrs
   {-# INLINE rule280 #-}
   rule280 = \ ((_patternIerrors) :: Seq Error) ->
     _patternIerrors
   {-# INLINE rule281 #-}
   rule281 = \ ((_patternIlocals) :: Set Identifier) ->
     _patternIlocals
   {-# INLINE rule282 #-}
   rule282 = \ ((_patternIoutput) :: Pattern) eager_ explicit_ identity_ mbError_ mbName_ origin_ owrt_ pure_ rhs_ ->
     Rule mbName_ _patternIoutput rhs_ owrt_ origin_ explicit_ pure_ identity_ mbError_ eager_
   {-# INLINE rule283 #-}
   rule283 = \ _output ->
     _output
   {-# INLINE rule284 #-}
   rule284 = \ ((_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# INLINE rule285 #-}
   rule285 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt

-- Rules -------------------------------------------------------
-- wrapper
data Inh_Rules  = Inh_Rules { con_Inh_Rules :: !(ConstructorIdent), nt_Inh_Rules :: !(NontermIdent), options_Inh_Rules :: !(Options), uniq_Inh_Rules :: !(Int) }
data Syn_Rules  = Syn_Rules { definedAttrs_Syn_Rules :: !(Set (Identifier,Identifier)), errors_Syn_Rules :: !(Seq Error), locals_Syn_Rules :: !(Set Identifier), output_Syn_Rules :: !(Rules), ruleNames_Syn_Rules :: !(Set Identifier), uniq_Syn_Rules :: !(Int) }
{-# INLINABLE wrap_Rules #-}
wrap_Rules :: T_Rules  -> Inh_Rules  -> (Syn_Rules )
wrap_Rules !(T_Rules act) !(Inh_Rules _lhsIcon _lhsInt _lhsIoptions _lhsIuniq) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_Rules_vIn31 _lhsIcon _lhsInt _lhsIoptions _lhsIuniq
        !(T_Rules_vOut31 _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput _lhsOruleNames _lhsOuniq) <- return (inv_Rules_s32 sem arg)
        return (Syn_Rules _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput _lhsOruleNames _lhsOuniq)
   )

-- cata
{-# NOINLINE sem_Rules #-}
sem_Rules :: Rules  -> T_Rules 
sem_Rules list = Prelude.foldr sem_Rules_Cons sem_Rules_Nil (Prelude.map sem_Rule list)

-- semantic domain
newtype T_Rules  = T_Rules {
                           attach_T_Rules :: Identity (T_Rules_s32 )
                           }
newtype T_Rules_s32  = C_Rules_s32 {
                                   inv_Rules_s32 :: (T_Rules_v31 )
                                   }
data T_Rules_s33  = C_Rules_s33
type T_Rules_v31  = (T_Rules_vIn31 ) -> (T_Rules_vOut31 )
data T_Rules_vIn31  = T_Rules_vIn31 (ConstructorIdent) (NontermIdent) (Options) (Int)
data T_Rules_vOut31  = T_Rules_vOut31 (Set (Identifier,Identifier)) (Seq Error) (Set Identifier) (Rules) (Set Identifier) (Int)
{-# NOINLINE sem_Rules_Cons #-}
sem_Rules_Cons :: T_Rule  -> T_Rules  -> T_Rules 
sem_Rules_Cons arg_hd_ arg_tl_ = T_Rules (return st32) where
   {-# NOINLINE st32 #-}
   !st32 = let
      v31 :: T_Rules_v31 
      v31 = \ !(T_Rules_vIn31 _lhsIcon _lhsInt _lhsIoptions _lhsIuniq) -> ( let
         _hdX29 = Control.Monad.Identity.runIdentity (attach_T_Rule (arg_hd_))
         _tlX32 = Control.Monad.Identity.runIdentity (attach_T_Rules (arg_tl_))
         (T_Rule_vOut28 _hdIcontainsVars _hdIdefinedAttrs _hdIerrors _hdIisPure _hdIlocals _hdIoutput _hdIoutputs _hdIruleNames _hdIuniq) = inv_Rule_s29 _hdX29 (T_Rule_vIn28 _hdOcon _hdOnt _hdOoptions _hdOuniq)
         (T_Rules_vOut31 _tlIdefinedAttrs _tlIerrors _tlIlocals _tlIoutput _tlIruleNames _tlIuniq) = inv_Rules_s32 _tlX32 (T_Rules_vIn31 _tlOcon _tlOnt _tlOoptions _tlOuniq)
         _lhsOoutput :: Rules
         _lhsOoutput = rule286 _hdIcontainsVars _hdIisPure _hdIoutputs _tlIoutput
         _lhsOdefinedAttrs :: Set (Identifier,Identifier)
         _lhsOdefinedAttrs = rule287 _hdIdefinedAttrs _tlIdefinedAttrs
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule288 _hdIerrors _tlIerrors
         _lhsOlocals :: Set Identifier
         _lhsOlocals = rule289 _hdIlocals _tlIlocals
         _lhsOruleNames :: Set Identifier
         _lhsOruleNames = rule290 _hdIruleNames _tlIruleNames
         _output = rule291 _hdIoutput _tlIoutput
         _lhsOuniq :: Int
         _lhsOuniq = rule292 _tlIuniq
         _hdOcon = rule293 _lhsIcon
         _hdOnt = rule294 _lhsInt
         _hdOoptions = rule295 _lhsIoptions
         _hdOuniq = rule296 _lhsIuniq
         _tlOcon = rule297 _lhsIcon
         _tlOnt = rule298 _lhsInt
         _tlOoptions = rule299 _lhsIoptions
         _tlOuniq = rule300 _hdIuniq
         !__result_ = T_Rules_vOut31 _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput _lhsOruleNames _lhsOuniq
         in __result_ )
     in C_Rules_s32 v31
   {-# INLINE rule286 #-}
   {-# LINE 620 "./src-ag/DefaultRules.ag" #-}
   rule286 = \ ((_hdIcontainsVars) :: Bool) ((_hdIisPure) :: Bool) ((_hdIoutputs) :: Rules) ((_tlIoutput) :: Rules) ->
                        {-# LINE 620 "./src-ag/DefaultRules.ag" #-}
                        if _hdIcontainsVars && _hdIisPure then _hdIoutputs ++ _tlIoutput else _tlIoutput
                        {-# LINE 2584 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule287 #-}
   rule287 = \ ((_hdIdefinedAttrs) :: Set (Identifier,Identifier)) ((_tlIdefinedAttrs) :: Set (Identifier,Identifier)) ->
     _hdIdefinedAttrs `Set.union` _tlIdefinedAttrs
   {-# INLINE rule288 #-}
   rule288 = \ ((_hdIerrors) :: Seq Error) ((_tlIerrors) :: Seq Error) ->
     _hdIerrors Seq.>< _tlIerrors
   {-# INLINE rule289 #-}
   rule289 = \ ((_hdIlocals) :: Set Identifier) ((_tlIlocals) :: Set Identifier) ->
     _hdIlocals `Set.union` _tlIlocals
   {-# INLINE rule290 #-}
   rule290 = \ ((_hdIruleNames) :: Set Identifier) ((_tlIruleNames) :: Set Identifier) ->
     _hdIruleNames `Set.union` _tlIruleNames
   {-# INLINE rule291 #-}
   rule291 = \ ((_hdIoutput) :: Rule) ((_tlIoutput) :: Rules) ->
     (:) _hdIoutput _tlIoutput
   {-# INLINE rule292 #-}
   rule292 = \ ((_tlIuniq) :: Int) ->
     _tlIuniq
   {-# INLINE rule293 #-}
   rule293 = \ ((_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# INLINE rule294 #-}
   rule294 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule295 #-}
   rule295 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule296 #-}
   rule296 = \ ((_lhsIuniq) :: Int) ->
     _lhsIuniq
   {-# INLINE rule297 #-}
   rule297 = \ ((_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# INLINE rule298 #-}
   rule298 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule299 #-}
   rule299 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule300 #-}
   rule300 = \ ((_hdIuniq) :: Int) ->
     _hdIuniq
{-# NOINLINE sem_Rules_Nil #-}
sem_Rules_Nil ::  T_Rules 
sem_Rules_Nil  = T_Rules (return st32) where
   {-# NOINLINE st32 #-}
   !st32 = let
      v31 :: T_Rules_v31 
      v31 = \ !(T_Rules_vIn31 _lhsIcon _lhsInt _lhsIoptions _lhsIuniq) -> ( let
         _lhsOdefinedAttrs :: Set (Identifier,Identifier)
         _lhsOdefinedAttrs = rule301  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule302  ()
         _lhsOlocals :: Set Identifier
         _lhsOlocals = rule303  ()
         _lhsOruleNames :: Set Identifier
         _lhsOruleNames = rule304  ()
         _output = rule305  ()
         _lhsOoutput :: Rules
         _lhsOoutput = rule306 _output
         _lhsOuniq :: Int
         _lhsOuniq = rule307 _lhsIuniq
         !__result_ = T_Rules_vOut31 _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput _lhsOruleNames _lhsOuniq
         in __result_ )
     in C_Rules_s32 v31
   {-# INLINE rule301 #-}
   rule301 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule302 #-}
   rule302 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule303 #-}
   rule303 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule304 #-}
   rule304 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule305 #-}
   rule305 = \  (_ :: ()) ->
     []
   {-# INLINE rule306 #-}
   rule306 = \ _output ->
     _output
   {-# INLINE rule307 #-}
   rule307 = \ ((_lhsIuniq) :: Int) ->
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
        let arg = T_TypeSig_vIn34 _lhsInt _lhsIparams
        !(T_TypeSig_vOut34 _lhsOoutput) <- return (inv_TypeSig_s35 sem arg)
        return (Syn_TypeSig _lhsOoutput)
   )

-- cata
{-# INLINE sem_TypeSig #-}
sem_TypeSig :: TypeSig  -> T_TypeSig 
sem_TypeSig ( TypeSig !name_ !tp_ ) = sem_TypeSig_TypeSig name_ tp_

-- semantic domain
newtype T_TypeSig  = T_TypeSig {
                               attach_T_TypeSig :: Identity (T_TypeSig_s35 )
                               }
newtype T_TypeSig_s35  = C_TypeSig_s35 {
                                       inv_TypeSig_s35 :: (T_TypeSig_v34 )
                                       }
data T_TypeSig_s36  = C_TypeSig_s36
type T_TypeSig_v34  = (T_TypeSig_vIn34 ) -> (T_TypeSig_vOut34 )
data T_TypeSig_vIn34  = T_TypeSig_vIn34 (NontermIdent) ([Identifier])
data T_TypeSig_vOut34  = T_TypeSig_vOut34 (TypeSig)
{-# NOINLINE sem_TypeSig_TypeSig #-}
sem_TypeSig_TypeSig :: (Identifier) -> (Type) -> T_TypeSig 
sem_TypeSig_TypeSig !arg_name_ !arg_tp_ = T_TypeSig (return st35) where
   {-# NOINLINE st35 #-}
   !st35 = let
      v34 :: T_TypeSig_v34 
      v34 = \ !(T_TypeSig_vIn34 _lhsInt _lhsIparams) -> ( let
         _tp1 = rule308 _lhsInt _lhsIparams arg_tp_
         _lhsOoutput :: TypeSig
         _lhsOoutput = rule309 _tp1 arg_name_
         _output = rule310 arg_name_ arg_tp_
         !__result_ = T_TypeSig_vOut34 _lhsOoutput
         in __result_ )
     in C_TypeSig_s35 v34
   {-# INLINE rule308 #-}
   {-# LINE 576 "./src-ag/DefaultRules.ag" #-}
   rule308 = \ ((_lhsInt) :: NontermIdent) ((_lhsIparams) :: [Identifier]) tp_ ->
              {-# LINE 576 "./src-ag/DefaultRules.ag" #-}
              elimSelfId _lhsInt _lhsIparams tp_
              {-# LINE 2721 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule309 #-}
   {-# LINE 617 "./src-ag/DefaultRules.ag" #-}
   rule309 = \ _tp1 name_ ->
                 {-# LINE 617 "./src-ag/DefaultRules.ag" #-}
                 TypeSig name_ _tp1
                 {-# LINE 2727 "dist/build/DefaultRules.hs"#-}
   {-# INLINE rule310 #-}
   rule310 = \ name_ tp_ ->
     TypeSig name_ tp_

-- TypeSigs ----------------------------------------------------
-- wrapper
data Inh_TypeSigs  = Inh_TypeSigs { nt_Inh_TypeSigs :: !(NontermIdent), params_Inh_TypeSigs :: !([Identifier]) }
data Syn_TypeSigs  = Syn_TypeSigs { output_Syn_TypeSigs :: !(TypeSigs) }
{-# INLINABLE wrap_TypeSigs #-}
wrap_TypeSigs :: T_TypeSigs  -> Inh_TypeSigs  -> (Syn_TypeSigs )
wrap_TypeSigs !(T_TypeSigs act) !(Inh_TypeSigs _lhsInt _lhsIparams) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg = T_TypeSigs_vIn37 _lhsInt _lhsIparams
        !(T_TypeSigs_vOut37 _lhsOoutput) <- return (inv_TypeSigs_s38 sem arg)
        return (Syn_TypeSigs _lhsOoutput)
   )

-- cata
{-# NOINLINE sem_TypeSigs #-}
sem_TypeSigs :: TypeSigs  -> T_TypeSigs 
sem_TypeSigs list = Prelude.foldr sem_TypeSigs_Cons sem_TypeSigs_Nil (Prelude.map sem_TypeSig list)

-- semantic domain
newtype T_TypeSigs  = T_TypeSigs {
                                 attach_T_TypeSigs :: Identity (T_TypeSigs_s38 )
                                 }
newtype T_TypeSigs_s38  = C_TypeSigs_s38 {
                                         inv_TypeSigs_s38 :: (T_TypeSigs_v37 )
                                         }
data T_TypeSigs_s39  = C_TypeSigs_s39
type T_TypeSigs_v37  = (T_TypeSigs_vIn37 ) -> (T_TypeSigs_vOut37 )
data T_TypeSigs_vIn37  = T_TypeSigs_vIn37 (NontermIdent) ([Identifier])
data T_TypeSigs_vOut37  = T_TypeSigs_vOut37 (TypeSigs)
{-# NOINLINE sem_TypeSigs_Cons #-}
sem_TypeSigs_Cons :: T_TypeSig  -> T_TypeSigs  -> T_TypeSigs 
sem_TypeSigs_Cons arg_hd_ arg_tl_ = T_TypeSigs (return st38) where
   {-# NOINLINE st38 #-}
   !st38 = let
      v37 :: T_TypeSigs_v37 
      v37 = \ !(T_TypeSigs_vIn37 _lhsInt _lhsIparams) -> ( let
         _hdX35 = Control.Monad.Identity.runIdentity (attach_T_TypeSig (arg_hd_))
         _tlX38 = Control.Monad.Identity.runIdentity (attach_T_TypeSigs (arg_tl_))
         (T_TypeSig_vOut34 _hdIoutput) = inv_TypeSig_s35 _hdX35 (T_TypeSig_vIn34 _hdOnt _hdOparams)
         (T_TypeSigs_vOut37 _tlIoutput) = inv_TypeSigs_s38 _tlX38 (T_TypeSigs_vIn37 _tlOnt _tlOparams)
         _output = rule311 _hdIoutput _tlIoutput
         _lhsOoutput :: TypeSigs
         _lhsOoutput = rule312 _output
         _hdOnt = rule313 _lhsInt
         _hdOparams = rule314 _lhsIparams
         _tlOnt = rule315 _lhsInt
         _tlOparams = rule316 _lhsIparams
         !__result_ = T_TypeSigs_vOut37 _lhsOoutput
         in __result_ )
     in C_TypeSigs_s38 v37
   {-# INLINE rule311 #-}
   rule311 = \ ((_hdIoutput) :: TypeSig) ((_tlIoutput) :: TypeSigs) ->
     (:) _hdIoutput _tlIoutput
   {-# INLINE rule312 #-}
   rule312 = \ _output ->
     _output
   {-# INLINE rule313 #-}
   rule313 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule314 #-}
   rule314 = \ ((_lhsIparams) :: [Identifier]) ->
     _lhsIparams
   {-# INLINE rule315 #-}
   rule315 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule316 #-}
   rule316 = \ ((_lhsIparams) :: [Identifier]) ->
     _lhsIparams
{-# NOINLINE sem_TypeSigs_Nil #-}
sem_TypeSigs_Nil ::  T_TypeSigs 
sem_TypeSigs_Nil  = T_TypeSigs (return st38) where
   {-# NOINLINE st38 #-}
   !st38 = let
      v37 :: T_TypeSigs_v37 
      v37 = \ !(T_TypeSigs_vIn37 _lhsInt _lhsIparams) -> ( let
         _output = rule317  ()
         _lhsOoutput :: TypeSigs
         _lhsOoutput = rule318 _output
         !__result_ = T_TypeSigs_vOut37 _lhsOoutput
         in __result_ )
     in C_TypeSigs_s38 v37
   {-# INLINE rule317 #-}
   rule317 = \  (_ :: ()) ->
     []
   {-# INLINE rule318 #-}
   rule318 = \ _output ->
     _output
