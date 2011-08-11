{-# OPTIONS_GHC -XBangPatterns #-}

-- UUAGC 0.9.38.6.5 (src-ag/DefaultRules.ag)
module DefaultRules where
{-# LINE 10 "src-ag/DefaultRules.ag" #-}

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

import AbstractSyntax
import ErrorMessages

import Options(Options,modcopy,rename)
{-# LINE 25 "dist/build/uuagc/uuagc-tmp/DefaultRules.hs" #-}

{-# LINE 2 "src-ag/AbstractSyntax.ag" #-}

-- AbstractSyntax.ag imports
import Data.Set(Set)
import Data.Map(Map)
import Patterns    (Pattern(..),Patterns)
import Expression  (Expression(..))
import CommonTypes
{-# LINE 35 "dist/build/uuagc/uuagc-tmp/DefaultRules.hs" #-}

{-# LINE 2 "src-ag/Patterns.ag" #-}

-- Patterns.ag imports
import UU.Scanner.Position(Pos)
import CommonTypes (ConstructorIdent,Identifier)
{-# LINE 42 "dist/build/uuagc/uuagc-tmp/DefaultRules.hs" #-}
{-# LINE 60 "src-ag/DefaultRules.ag" #-}

fieldName n       = '@' : getName n

locName n         = '@' : getName n

attrName fld attr
 | fld == _LOC    = '@' :                       getName attr
 | otherwise      = '@' : getName fld ++ "." ++ getName attr

_ACHILD = Ident "(" noPos -- hack


getConName typeSyns rename nt con1
 | nt `elem` map fst typeSyns  =  synonym
 | otherwise                   =  normalName
 where con                            = getName con1
       normalName | rename            = getName nt++"_"++ con
                  | otherwise         =  con
       synonym    | con == "Cons"     = "(:)"
                  | con == "Nil"      = case lookup nt typeSyns of
                                          Just (Map _ _)  -> "Data.Map.empty"
                                          Just (IntMap _) -> "Data.IntMap.empty"
                                          _               -> "[]"
                  | con == "Just"     = "Just"
                  | con == "Nothing"  = "Nothing"
                  | con == "Entry"    = case lookup nt typeSyns of
                                          Just (Map _ _)  -> "Data.Map.insert"
                                          Just (IntMap _) -> "Data.IntMap.insert"
                  | otherwise         = normalName

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

{-# LINE 94 "dist/build/uuagc/uuagc-tmp/DefaultRules.hs" #-}

{-# LINE 175 "src-ag/DefaultRules.ag" #-}




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



makeRule :: (Identifier,Identifier) -> Expression -> String -> Rule
makeRule (f1,a1) expr origin
 = Rule Nothing
        (Alias f1 a1 (Underscore noPos) [])
        expr
        False
        origin
        False


useRule :: Set Identifier -> [(Identifier,Attributes)] -> (Identifier,(String,String,String)) -> Rule
useRule locals ch_outs (n,(op,e,pos))
 =  let elems = [ fld
                | (fld,as) <- ch_outs
                , Map.member n as
                ]

        expr | Set.member n locals  =  attrName _LOC n
             | null elems           =  e
             | otherwise            =  foldr1 (\x y -> x ++ " " ++ op ++ " " ++ y)
                                              (map (flip attrName n) elems)

        tks | Set.member n locals  =  [AGLocal n noPos Nothing]
            | null elems           =  lexTokens noPos e
            | otherwise            =  lexTokens noPos str
                                      where
                                        str = foldr1 (\x y -> x ++ " " ++ op ++ " " ++ y)
                                                (map (flip attrName n) elems)

    in makeRule (_LHS,n)
                (Expression noPos tks)
                ("use rule " ++ pos)




selfRule lhsNecLoc attr x
 = let expr | lhsNecLoc  = locName attr
            | otherwise  = x

       tks | lhsNecLoc   = [AGLocal attr noPos Nothing]
           | otherwise   = lexTokens noPos x

   in makeRule (if lhsNecLoc then _LHS else _LOC,attr)
               (Expression noPos tks)
               "self rule"




concatRE rsess = let (rss,ess) = unzip rsess
                 in (concat rss, concatSeq ess)


copyRule :: Identifier -> Identifier -> Bool -> Set Identifier -> (Map Identifier Identifier, (Identifier,[Identifier])) -> ([Rule], Seq Error)
copyRule nt con modcopy locals (env,(fld,as))
 = concatRE (map copyRu as)

 where
       copyRu a
           = ( [ makeRule (fld,a)
                          (Expression noPos tks)
                          (cruletxt sel)
               ]
             , err
             )

        where
              sel
               |    not modcopy
                 && Set.member a locals  =  Just _LOC
               | otherwise               =  Map.lookup a env

              (expr,err)
               = case sel of
                  Nothing         -> ( missingRuleErrorExpr nt con fld a
                                     , Seq.singleton (MissingRule nt con fld a)
                                     )
                  Just f
                   | f == _ACHILD -> ( fieldName a
                                     , Seq.singleton (deprecatedCopyRuleError nt con fld a)
                                     )
                   | otherwise    -> ( attrName f a
                                     , Seq.empty
                                     )

              (tks,err')
               = case sel of
                  Nothing         -> ( [HsToken (missingRuleErrorExpr nt con fld a) noPos]
                                     , Seq.singleton (MissingRule nt con fld a)
                                     )
                  Just f
                   | f == _ACHILD -> ( [AGLocal a noPos Nothing]
                                     , Seq.singleton (deprecatedCopyRuleError nt con fld a)
                                     )
                   | otherwise    -> ( [AGField f a noPos Nothing]
                                     , Seq.empty
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
{-# LINE 247 "dist/build/uuagc/uuagc-tmp/DefaultRules.hs" #-}

{-# LINE 406 "src-ag/DefaultRules.ag" #-}

addAugments :: (Identifier, [Expression]) -> [Rule] -> [Rule]
addAugments (_, exprs) rules
  | null exprs = rules
addAugments (syn, exprs) rules
  = [rule] ++ funRules ++ map modify rules
  where
    rule = Rule Nothing (Alias _LHS syn (Underscore noPos) []) rhs False "augmented rule" False
    rhs  = Expression noPos tks
    tks  = [ HsToken "foldr ($) " noPos, AGLocal substSyn noPos Nothing, HsToken " [" noPos] ++ funs ++ [HsToken "]" noPos]
    funs = intersperse (HsToken ", " noPos) (map (\n -> AGLocal n noPos Nothing) funNames)

    substSyn = Ident (show syn ++ "_augmented_syn") (getPos syn)
    funNames = zipWith (\i _ -> Ident (show syn ++ "_augmented_f" ++ show i) (getPos syn)) [1..] exprs
    funRules = zipWith (\name expr -> Rule Nothing (Alias _LOC name (Underscore noPos) []) expr False "augment function" False) funNames exprs

    modify (Rule mbNm pat rhs owrt origin expl)
      | containsSyn pat = Rule mbNm (modifyPat pat) rhs owrt origin expl
    modify r = r

    containsSyn (Constr _ pats)   = any containsSyn pats
    containsSyn (Product _ pats)  = any containsSyn pats
    containsSyn (Irrefutable pat) = containsSyn pat
    containsSyn (Alias field attr pat parts) = (field == _LHS && attr == syn) || containsSyn pat || any containsSyn parts
    containsSyn _ = False

    modifyPat (Constr name pats) = Constr name (map modifyPat pats)
    modifyPat (Product pos pats) = Product pos (map modifyPat pats)
    modifyPat (Irrefutable pat)  = Irrefutable (modifyPat pat)
    modifyPat (Alias field attr pat parts)
      | field == _LHS && attr == syn = Alias _LOC substSyn (modifyPat pat) (map modifyPat parts)
      | otherwise                    = Alias field attr (modifyPat pat) (map modifyPat parts)
    modifyPat p = p

addArounds :: (Identifier, [Expression]) -> [Rule] -> [Rule]
addArounds (_, exprs) rules | null exprs = rules
addArounds (child, exprs) rules
  = [rule] ++ funRules ++ rules
  where
    rule = Rule Nothing (Alias _LOC childLoc (Underscore noPos) []) rhs False "around rule" False
    rhs  = Expression noPos tks
    tks  = [ HsToken "\\s -> foldr ($) s " noPos, HsToken " [" noPos] ++ funs ++ [HsToken "]" noPos]
    funs = intersperse (HsToken ", " noPos) (map (\n -> AGLocal n noPos Nothing) funNames)

    childLoc = Ident (show child ++ "_around") (getPos child)
    funNames = zipWith (\i _ -> Ident (show child ++ "_around_f" ++ show i) (getPos child)) [1..] exprs
    funRules = zipWith (\name expr -> Rule Nothing (Alias _LOC name (Underscore noPos) []) expr False "around function" False) funNames exprs

addMerges :: (Identifier, (Identifier,[Identifier],Expression)) -> [Rule] -> [Rule]
addMerges (target,(_,_,expr)) rules
  = rule : rules
  where
    rule = Rule Nothing (Alias _LOC childLoc (Underscore noPos) []) expr False "merge rule" False
    childLoc = Ident (show target ++ "_merge") (getPos target)
{-# LINE 304 "dist/build/uuagc/uuagc-tmp/DefaultRules.hs" #-}

{-# LINE 521 "src-ag/DefaultRules.ag" #-}

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
multiRule (Rule (Just nm) pat expr owrt origin expl) uniq
  = let pos = getPos nm
        r = Rule Nothing (Alias _LOC (Ident ("_rule_" ++ show nm) pos) (Underscore pos) []) expr owrt origin expl
        expr' = Expression pos tks
        tks = [AGLocal (Ident ("_rule_" ++ show nm) pos) pos (Just ("Indirection to rule " ++ show nm))]
        (rs,uniq') = multiRule (Rule Nothing pat expr' owrt origin False) uniq
    in (r:rs, uniq')

multiRule (Rule Nothing pat expr owrt origin expl) uniq
  =  let f :: Bool -> (Pattern->Pattern) -> Expression -> Pattern -> Int -> (Pattern, ([Rule], Int))
         f expl' w e (Product pos pats) n
           = let freshName = Ident ("_tup" ++ show n) pos
                 freshExpr = Expression pos freshTks
                 freshTks  = [AGField _LOC freshName pos Nothing]
                 freshPat  = Alias _LOC freshName (Underscore pos) pats
                 a = length pats - 1
                 us b p = Product pos (replicate (a-b) (Underscore pos) ++ [p] ++ replicate b (Underscore pos))
                 g :: Pattern -> ([Pattern],[Rule],Int) -> ([Pattern],[Rule],Int)
                 g p (xs1,rs1,n1)   = let (x2,(rs2,n2)) = f False (us (length xs1)) freshExpr p n1
                                      in  (x2:xs1, rs2++rs1, n2)
                 (xs9,rs9,n9) = foldr g ([], [], n+1) pats
             in  ( freshPat
                 , ( Rule Nothing (w freshPat) e owrt origin expl' : rs9
                   , n9
                   )
                 )
         f expl' w e p n
           = ( p
             , ( [Rule Nothing (w p) e owrt origin expl']
               , n
               )
             )
     in snd (f expl id expr pat uniq)

{-# LINE 362 "dist/build/uuagc/uuagc-tmp/DefaultRules.hs" #-}
-- Child -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         con                  : ConstructorIdent
         cr                   : Bool
         merged               : Set Identifier
         nt                   : NontermIdent
      synthesized attributes:
         errors               : Seq Error
         field                :  (Identifier,Type,Maybe (Maybe Type)) 
         inherited            : Attributes
         name                 : Identifier
         output               : SELF 
         synthesized          : Attributes
   alternatives:
      alternative Child:
         child name           : {Identifier}
         child tp             : {Type}
         child inh            : {Attributes}
         child syn            : {Attributes}
         child virtual        : {Maybe (Maybe Type)}
         visit 0:
            local output      : _
-}
-- cata
sem_Child :: Child  ->
             T_Child 
sem_Child !(Child _name _tp _inh _syn _virtual )  =
    (sem_Child_Child _name _tp _inh _syn _virtual )
-- semantic domain
newtype T_Child  = T_Child (ConstructorIdent ->
                            Bool ->
                            (Set Identifier) ->
                            NontermIdent ->
                            ( (Seq Error),( (Identifier,Type,Maybe (Maybe Type)) ),Attributes,Identifier,Child ,Attributes))
data Inh_Child  = Inh_Child {con_Inh_Child :: !(ConstructorIdent),cr_Inh_Child :: !(Bool),merged_Inh_Child :: !((Set Identifier)),nt_Inh_Child :: !(NontermIdent)}
data Syn_Child  = Syn_Child {errors_Syn_Child :: !((Seq Error)),field_Syn_Child :: !(( (Identifier,Type,Maybe (Maybe Type)) )),inherited_Syn_Child :: !(Attributes),name_Syn_Child :: !(Identifier),output_Syn_Child :: !(Child ),synthesized_Syn_Child :: !(Attributes)}
wrap_Child :: T_Child  ->
              Inh_Child  ->
              Syn_Child 
wrap_Child !(T_Child sem ) !(Inh_Child _lhsIcon _lhsIcr _lhsImerged _lhsInt )  =
    (let ( !_lhsOerrors,!_lhsOfield,!_lhsOinherited,!_lhsOname,!_lhsOoutput,!_lhsOsynthesized) = sem _lhsIcon _lhsIcr _lhsImerged _lhsInt 
     in  (Syn_Child _lhsOerrors _lhsOfield _lhsOinherited _lhsOname _lhsOoutput _lhsOsynthesized ))
sem_Child_Child :: Identifier ->
                   Type ->
                   Attributes ->
                   Attributes ->
                   (Maybe (Maybe Type)) ->
                   T_Child 
sem_Child_Child !name_ !tp_ !inh_ !syn_ !virtual_  =
    (T_Child (\ (!_lhsIcon)
                (!_lhsIcr)
                (!_lhsImerged)
                (!_lhsInt) ->
                  (case (({-# LINE 118 "src-ag/DefaultRules.ag" #-}
                          Seq.empty
                          {-# LINE 420 "src-ag/DefaultRules.hs" #-}
                          )) of
                   { !_lhsOerrors ->
                   (case (({-# LINE 480 "src-ag/DefaultRules.ag" #-}
                           (name_,tp_,virtual_)
                           {-# LINE 425 "src-ag/DefaultRules.hs" #-}
                           )) of
                    { !_lhsOfield ->
                    (case (({-# LINE 158 "src-ag/DefaultRules.ag" #-}
                            inh_
                            {-# LINE 430 "src-ag/DefaultRules.hs" #-}
                            )) of
                     { !_lhsOinherited ->
                     (case (({-# LINE 149 "src-ag/DefaultRules.ag" #-}
                             name_
                             {-# LINE 435 "src-ag/DefaultRules.hs" #-}
                             )) of
                      { !_lhsOname ->
                      (case (({-# LINE 501 "src-ag/DefaultRules.ag" #-}
                              Child name_ tp_ inh_ syn_ virtual_
                              {-# LINE 440 "src-ag/DefaultRules.hs" #-}
                              )) of
                       { !_output ->
                       (case (({-# LINE 501 "src-ag/DefaultRules.ag" #-}
                               _output
                               {-# LINE 445 "src-ag/DefaultRules.hs" #-}
                               )) of
                        { !_lhsOoutput ->
                        (case (({-# LINE 159 "src-ag/DefaultRules.ag" #-}
                                if name_ `Set.member` _lhsImerged
                                then Map.empty
                                else syn_
                                {-# LINE 452 "src-ag/DefaultRules.hs" #-}
                                )) of
                         { !_lhsOsynthesized ->
                         ( _lhsOerrors,_lhsOfield,_lhsOinherited,_lhsOname,_lhsOoutput,_lhsOsynthesized) }) }) }) }) }) }) })) )
-- Children ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         con                  : ConstructorIdent
         cr                   : Bool
         merged               : Set Identifier
         nt                   : NontermIdent
      synthesized attributes:
         errors               : Seq Error
         fields               : [(Identifier,Type,Maybe (Maybe Type))]
         inputs               : [(Identifier, Attributes)]
         output               : SELF 
         outputs              : [(Identifier, Attributes)]
   alternatives:
      alternative Cons:
         child hd             : Child 
         child tl             : Children 
         visit 0:
            local output      : _
      alternative Nil:
         visit 0:
            local output      : _
-}
-- cata
sem_Children :: Children  ->
                T_Children 
sem_Children !list  =
    (Prelude.foldr sem_Children_Cons sem_Children_Nil (Prelude.map sem_Child list) )
-- semantic domain
newtype T_Children  = T_Children (ConstructorIdent ->
                                  Bool ->
                                  (Set Identifier) ->
                                  NontermIdent ->
                                  ( (Seq Error),([(Identifier,Type,Maybe (Maybe Type))]),([(Identifier, Attributes)]),Children ,([(Identifier, Attributes)])))
data Inh_Children  = Inh_Children {con_Inh_Children :: !(ConstructorIdent),cr_Inh_Children :: !(Bool),merged_Inh_Children :: !((Set Identifier)),nt_Inh_Children :: !(NontermIdent)}
data Syn_Children  = Syn_Children {errors_Syn_Children :: !((Seq Error)),fields_Syn_Children :: !(([(Identifier,Type,Maybe (Maybe Type))])),inputs_Syn_Children :: !(([(Identifier, Attributes)])),output_Syn_Children :: !(Children ),outputs_Syn_Children :: !(([(Identifier, Attributes)]))}
wrap_Children :: T_Children  ->
                 Inh_Children  ->
                 Syn_Children 
wrap_Children !(T_Children sem ) !(Inh_Children _lhsIcon _lhsIcr _lhsImerged _lhsInt )  =
    (let ( !_lhsOerrors,!_lhsOfields,!_lhsOinputs,!_lhsOoutput,!_lhsOoutputs) = sem _lhsIcon _lhsIcr _lhsImerged _lhsInt 
     in  (Syn_Children _lhsOerrors _lhsOfields _lhsOinputs _lhsOoutput _lhsOoutputs ))
sem_Children_Cons :: T_Child  ->
                     T_Children  ->
                     T_Children 
sem_Children_Cons !(T_Child hd_ ) !(T_Children tl_ )  =
    (T_Children (\ (!_lhsIcon)
                   (!_lhsIcr)
                   (!_lhsImerged)
                   (!_lhsInt) ->
                     (case (({-# LINE 33 "src-ag/DefaultRules.ag" #-}
                             _lhsInt
                             {-# LINE 509 "src-ag/DefaultRules.hs" #-}
                             )) of
                      { !_tlOnt ->
                      (case (({-# LINE 665 "src-ag/DefaultRules.ag" #-}
                              _lhsImerged
                              {-# LINE 514 "src-ag/DefaultRules.hs" #-}
                              )) of
                       { !_tlOmerged ->
                       (case (({-# LINE 46 "src-ag/DefaultRules.ag" #-}
                               _lhsIcr
                               {-# LINE 519 "src-ag/DefaultRules.hs" #-}
                               )) of
                        { !_tlOcr ->
                        (case (({-# LINE 34 "src-ag/DefaultRules.ag" #-}
                                _lhsIcon
                                {-# LINE 524 "src-ag/DefaultRules.hs" #-}
                                )) of
                         { !_tlOcon ->
                         (case (tl_ _tlOcon _tlOcr _tlOmerged _tlOnt ) of
                          { ( !_tlIerrors,!_tlIfields,!_tlIinputs,!_tlIoutput,!_tlIoutputs) ->
                              (case (({-# LINE 33 "src-ag/DefaultRules.ag" #-}
                                      _lhsInt
                                      {-# LINE 531 "src-ag/DefaultRules.hs" #-}
                                      )) of
                               { !_hdOnt ->
                               (case (({-# LINE 665 "src-ag/DefaultRules.ag" #-}
                                       _lhsImerged
                                       {-# LINE 536 "src-ag/DefaultRules.hs" #-}
                                       )) of
                                { !_hdOmerged ->
                                (case (({-# LINE 46 "src-ag/DefaultRules.ag" #-}
                                        _lhsIcr
                                        {-# LINE 541 "src-ag/DefaultRules.hs" #-}
                                        )) of
                                 { !_hdOcr ->
                                 (case (({-# LINE 34 "src-ag/DefaultRules.ag" #-}
                                         _lhsIcon
                                         {-# LINE 546 "src-ag/DefaultRules.hs" #-}
                                         )) of
                                  { !_hdOcon ->
                                  (case (hd_ _hdOcon _hdOcr _hdOmerged _hdOnt ) of
                                   { ( !_hdIerrors,!_hdIfield,!_hdIinherited,!_hdIname,!_hdIoutput,!_hdIsynthesized) ->
                                       (case (({-# LINE 118 "src-ag/DefaultRules.ag" #-}
                                               _hdIerrors Seq.>< _tlIerrors
                                               {-# LINE 553 "src-ag/DefaultRules.hs" #-}
                                               )) of
                                        { !_lhsOerrors ->
                                        (case (({-# LINE 476 "src-ag/DefaultRules.ag" #-}
                                                _hdIfield : _tlIfields
                                                {-# LINE 558 "src-ag/DefaultRules.hs" #-}
                                                )) of
                                         { !_lhsOfields ->
                                         (case (({-# LINE 164 "src-ag/DefaultRules.ag" #-}
                                                 (_hdIname, _hdIinherited) : _tlIinputs
                                                 {-# LINE 563 "src-ag/DefaultRules.hs" #-}
                                                 )) of
                                          { !_lhsOinputs ->
                                          (case (({-# LINE 501 "src-ag/DefaultRules.ag" #-}
                                                  (:) _hdIoutput _tlIoutput
                                                  {-# LINE 568 "src-ag/DefaultRules.hs" #-}
                                                  )) of
                                           { !_output ->
                                           (case (({-# LINE 501 "src-ag/DefaultRules.ag" #-}
                                                   _output
                                                   {-# LINE 573 "src-ag/DefaultRules.hs" #-}
                                                   )) of
                                            { !_lhsOoutput ->
                                            (case (({-# LINE 165 "src-ag/DefaultRules.ag" #-}
                                                    (_hdIname, _hdIsynthesized) : _tlIoutputs
                                                    {-# LINE 578 "src-ag/DefaultRules.hs" #-}
                                                    )) of
                                             { !_lhsOoutputs ->
                                             ( _lhsOerrors,_lhsOfields,_lhsOinputs,_lhsOoutput,_lhsOoutputs) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })) )
sem_Children_Nil :: T_Children 
sem_Children_Nil  =
    (T_Children (\ (!_lhsIcon)
                   (!_lhsIcr)
                   (!_lhsImerged)
                   (!_lhsInt) ->
                     (case (({-# LINE 118 "src-ag/DefaultRules.ag" #-}
                             Seq.empty
                             {-# LINE 590 "src-ag/DefaultRules.hs" #-}
                             )) of
                      { !_lhsOerrors ->
                      (case (({-# LINE 477 "src-ag/DefaultRules.ag" #-}
                              []
                              {-# LINE 595 "src-ag/DefaultRules.hs" #-}
                              )) of
                       { !_lhsOfields ->
                       (case (({-# LINE 166 "src-ag/DefaultRules.ag" #-}
                               []
                               {-# LINE 600 "src-ag/DefaultRules.hs" #-}
                               )) of
                        { !_lhsOinputs ->
                        (case (({-# LINE 501 "src-ag/DefaultRules.ag" #-}
                                []
                                {-# LINE 605 "src-ag/DefaultRules.hs" #-}
                                )) of
                         { !_output ->
                         (case (({-# LINE 501 "src-ag/DefaultRules.ag" #-}
                                 _output
                                 {-# LINE 610 "src-ag/DefaultRules.hs" #-}
                                 )) of
                          { !_lhsOoutput ->
                          (case (({-# LINE 167 "src-ag/DefaultRules.ag" #-}
                                  []
                                  {-# LINE 615 "src-ag/DefaultRules.hs" #-}
                                  )) of
                           { !_lhsOoutputs ->
                           ( _lhsOerrors,_lhsOfields,_lhsOinputs,_lhsOoutput,_lhsOoutputs) }) }) }) }) }) })) )
-- Grammar -----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         options              : Options
      synthesized attributes:
         errors               : Seq Error
         output               : SELF 
   alternatives:
      alternative Grammar:
         child typeSyns       : {TypeSyns}
         child useMap         : {UseMap}
         child derivings      : {Derivings}
         child wrappers       : {Set NontermIdent}
         child nonts          : Nonterminals 
         child pragmas        : {PragmaMap}
         child manualAttrOrderMap : {AttrOrderMap}
         child paramMap       : {ParamMap}
         child contextMap     : {ContextMap}
         child quantMap       : {QuantMap}
         child uniqueMap      : {UniqueMap}
         child augmentsMap    : {Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))}
         child aroundsMap     : {Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))}
         child mergeMap       : {Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier, [Identifier], Expression)))}
         visit 0:
            local output      : _
-}
-- cata
sem_Grammar :: Grammar  ->
               T_Grammar 
sem_Grammar !(Grammar _typeSyns _useMap _derivings _wrappers _nonts _pragmas _manualAttrOrderMap _paramMap _contextMap _quantMap _uniqueMap _augmentsMap _aroundsMap _mergeMap )  =
    (sem_Grammar_Grammar _typeSyns _useMap _derivings _wrappers (sem_Nonterminals _nonts ) _pragmas _manualAttrOrderMap _paramMap _contextMap _quantMap _uniqueMap _augmentsMap _aroundsMap _mergeMap )
-- semantic domain
newtype T_Grammar  = T_Grammar (Options ->
                                ( (Seq Error),Grammar ))
data Inh_Grammar  = Inh_Grammar {options_Inh_Grammar :: !(Options)}
data Syn_Grammar  = Syn_Grammar {errors_Syn_Grammar :: !((Seq Error)),output_Syn_Grammar :: !(Grammar )}
wrap_Grammar :: T_Grammar  ->
                Inh_Grammar  ->
                Syn_Grammar 
wrap_Grammar !(T_Grammar sem ) !(Inh_Grammar _lhsIoptions )  =
    (let ( !_lhsOerrors,!_lhsOoutput) = sem _lhsIoptions 
     in  (Syn_Grammar _lhsOerrors _lhsOoutput ))
sem_Grammar_Grammar :: TypeSyns ->
                       UseMap ->
                       Derivings ->
                       (Set NontermIdent) ->
                       T_Nonterminals  ->
                       PragmaMap ->
                       AttrOrderMap ->
                       ParamMap ->
                       ContextMap ->
                       QuantMap ->
                       UniqueMap ->
                       (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) ->
                       (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) ->
                       (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier, [Identifier], Expression)))) ->
                       T_Grammar 
sem_Grammar_Grammar !typeSyns_ !useMap_ !derivings_ !wrappers_ !(T_Nonterminals nonts_ ) !pragmas_ !manualAttrOrderMap_ !paramMap_ !contextMap_ !quantMap_ !uniqueMap_ !augmentsMap_ !aroundsMap_ !mergeMap_  =
    (T_Grammar (\ (!_lhsIoptions) ->
                    (case (({-# LINE 667 "src-ag/DefaultRules.ag" #-}
                            mergeMap_
                            {-# LINE 681 "src-ag/DefaultRules.hs" #-}
                            )) of
                     { !_nontsOmergesIn ->
                     (case (({-# LINE 586 "src-ag/DefaultRules.ag" #-}
                             manualAttrOrderMap_
                             {-# LINE 686 "src-ag/DefaultRules.hs" #-}
                             )) of
                      { !_nontsOmanualAttrOrderMap ->
                      (case (({-# LINE 153 "src-ag/DefaultRules.ag" #-}
                              typeSyns_
                              {-# LINE 691 "src-ag/DefaultRules.hs" #-}
                              )) of
                       { !_nontsOtypeSyns ->
                       (case (({-# LINE 151 "src-ag/DefaultRules.ag" #-}
                               useMap_
                               {-# LINE 696 "src-ag/DefaultRules.hs" #-}
                               )) of
                        { !_nontsOuseMap ->
                        (case (({-# LINE 49 "src-ag/DefaultRules.ag" #-}
                                modcopy   _lhsIoptions
                                {-# LINE 701 "src-ag/DefaultRules.hs" #-}
                                )) of
                         { !_nontsOcr ->
                         (case (({-# LINE 48 "src-ag/DefaultRules.ag" #-}
                                 rename    _lhsIoptions
                                 {-# LINE 706 "src-ag/DefaultRules.hs" #-}
                                 )) of
                          { !_nontsOo_rename ->
                          (case (nonts_ ) of
                           { ( !_nontsIcollect_nts,!T_Nonterminals_1 nonts_1) ->
                               (case (({-# LINE 659 "src-ag/DefaultRules.ag" #-}
                                       aroundsMap_
                                       {-# LINE 713 "src-ag/DefaultRules.hs" #-}
                                       )) of
                                { !_nontsOaroundsIn ->
                                (case (({-# LINE 652 "src-ag/DefaultRules.ag" #-}
                                        augmentsMap_
                                        {-# LINE 718 "src-ag/DefaultRules.hs" #-}
                                        )) of
                                 { !_nontsOaugmentsIn ->
                                 (case (({-# LINE 497 "src-ag/DefaultRules.ag" #-}
                                         1
                                         {-# LINE 723 "src-ag/DefaultRules.hs" #-}
                                         )) of
                                  { !_nontsOuniq ->
                                  (case (({-# LINE 130 "src-ag/DefaultRules.ag" #-}
                                          _nontsIcollect_nts
                                          {-# LINE 728 "src-ag/DefaultRules.hs" #-}
                                          )) of
                                   { !_nontsOnonterminals ->
                                   (case (nonts_1 _nontsOaroundsIn _nontsOaugmentsIn _nontsOcr _nontsOmanualAttrOrderMap _nontsOmergesIn _nontsOnonterminals _nontsOo_rename _nontsOtypeSyns _nontsOuniq _nontsOuseMap ) of
                                    { ( !_nontsIerrors,!_nontsIoutput,!_nontsIuniq) ->
                                        (case (({-# LINE 118 "src-ag/DefaultRules.ag" #-}
                                                _nontsIerrors
                                                {-# LINE 735 "src-ag/DefaultRules.hs" #-}
                                                )) of
                                         { !_lhsOerrors ->
                                         (case (({-# LINE 501 "src-ag/DefaultRules.ag" #-}
                                                 Grammar typeSyns_ useMap_ derivings_ wrappers_ _nontsIoutput pragmas_ manualAttrOrderMap_ paramMap_ contextMap_ quantMap_ uniqueMap_ augmentsMap_ aroundsMap_ mergeMap_
                                                 {-# LINE 740 "src-ag/DefaultRules.hs" #-}
                                                 )) of
                                          { !_output ->
                                          (case (({-# LINE 501 "src-ag/DefaultRules.ag" #-}
                                                  _output
                                                  {-# LINE 745 "src-ag/DefaultRules.hs" #-}
                                                  )) of
                                           { !_lhsOoutput ->
                                           ( _lhsOerrors,_lhsOoutput) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })) )
-- Nonterminal -------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         collect_nts          : Set NontermIdent
   visit 1:
      inherited attributes:
         aroundsIn            : Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))
         augmentsIn           : Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))
         cr                   : Bool
         manualAttrOrderMap   : AttrOrderMap
         mergesIn             : Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier],Expression)))
         nonterminals         : Set NontermIdent
         o_rename             : Bool
         typeSyns             : TypeSyns
         useMap               : UseMap
      chained attribute:
         uniq                 : Int
      synthesized attributes:
         errors               : Seq Error
         output               : SELF 
   alternatives:
      alternative Nonterminal:
         child nt             : {NontermIdent}
         child params         : {[Identifier]}
         child inh            : {Attributes}
         child syn            : {Attributes}
         child prods          : Productions 
         visit 1:
            local mergesIn    : _
            local augmentsIn  : _
            local aroundsIn   : _
            local output      : _
-}
-- cata
sem_Nonterminal :: Nonterminal  ->
                   T_Nonterminal 
sem_Nonterminal !(Nonterminal _nt _params _inh _syn _prods )  =
    (sem_Nonterminal_Nonterminal _nt _params _inh _syn (sem_Productions _prods ) )
-- semantic domain
newtype T_Nonterminal  = T_Nonterminal (( (Set NontermIdent),T_Nonterminal_1 ))
newtype T_Nonterminal_1  = T_Nonterminal_1 ((Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) ->
                                            (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) ->
                                            Bool ->
                                            AttrOrderMap ->
                                            (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier],Expression)))) ->
                                            (Set NontermIdent) ->
                                            Bool ->
                                            TypeSyns ->
                                            Int ->
                                            UseMap ->
                                            ( (Seq Error),Nonterminal ,Int))
data Inh_Nonterminal  = Inh_Nonterminal {aroundsIn_Inh_Nonterminal :: !((Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression])))),augmentsIn_Inh_Nonterminal :: !((Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression])))),cr_Inh_Nonterminal :: !(Bool),manualAttrOrderMap_Inh_Nonterminal :: !(AttrOrderMap),mergesIn_Inh_Nonterminal :: !((Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier],Expression))))),nonterminals_Inh_Nonterminal :: !((Set NontermIdent)),o_rename_Inh_Nonterminal :: !(Bool),typeSyns_Inh_Nonterminal :: !(TypeSyns),uniq_Inh_Nonterminal :: !(Int),useMap_Inh_Nonterminal :: !(UseMap)}
data Syn_Nonterminal  = Syn_Nonterminal {collect_nts_Syn_Nonterminal :: !((Set NontermIdent)),errors_Syn_Nonterminal :: !((Seq Error)),output_Syn_Nonterminal :: !(Nonterminal ),uniq_Syn_Nonterminal :: !(Int)}
wrap_Nonterminal :: T_Nonterminal  ->
                    Inh_Nonterminal  ->
                    Syn_Nonterminal 
wrap_Nonterminal !(T_Nonterminal sem ) !(Inh_Nonterminal _lhsIaroundsIn _lhsIaugmentsIn _lhsIcr _lhsImanualAttrOrderMap _lhsImergesIn _lhsInonterminals _lhsIo_rename _lhsItypeSyns _lhsIuniq _lhsIuseMap )  =
    (let ( !_lhsOcollect_nts,!T_Nonterminal_1 sem_1) = sem 
         ( !_lhsOerrors,!_lhsOoutput,!_lhsOuniq) = sem_1 _lhsIaroundsIn _lhsIaugmentsIn _lhsIcr _lhsImanualAttrOrderMap _lhsImergesIn _lhsInonterminals _lhsIo_rename _lhsItypeSyns _lhsIuniq _lhsIuseMap 
     in  (Syn_Nonterminal _lhsOcollect_nts _lhsOerrors _lhsOoutput _lhsOuniq ))
sem_Nonterminal_Nonterminal :: NontermIdent ->
                               ([Identifier]) ->
                               Attributes ->
                               Attributes ->
                               T_Productions  ->
                               T_Nonterminal 
sem_Nonterminal_Nonterminal !nt_ !params_ !inh_ !syn_ !(T_Productions prods_ )  =
    (T_Nonterminal (case (({-# LINE 126 "src-ag/DefaultRules.ag" #-}
                           Set.singleton nt_
                           {-# LINE 819 "src-ag/DefaultRules.hs" #-}
                           )) of
                    { !_lhsOcollect_nts ->
                    (case ((let sem_Nonterminal_Nonterminal_1 :: T_Nonterminal_1 
                                sem_Nonterminal_Nonterminal_1  =
                                    (T_Nonterminal_1 (\ (!_lhsIaroundsIn)
                                                        (!_lhsIaugmentsIn)
                                                        (!_lhsIcr)
                                                        (!_lhsImanualAttrOrderMap)
                                                        (!_lhsImergesIn)
                                                        (!_lhsInonterminals)
                                                        (!_lhsIo_rename)
                                                        (!_lhsItypeSyns)
                                                        (!_lhsIuniq)
                                                        (!_lhsIuseMap) ->
                                                          (case (({-# LINE 54 "src-ag/DefaultRules.ag" #-}
                                                                  _lhsItypeSyns
                                                                  {-# LINE 836 "src-ag/DefaultRules.hs" #-}
                                                                  )) of
                                                           { !_prodsOtypeSyns ->
                                                           (case (({-# LINE 42 "src-ag/DefaultRules.ag" #-}
                                                                   _lhsIo_rename
                                                                   {-# LINE 841 "src-ag/DefaultRules.hs" #-}
                                                                   )) of
                                                            { !_prodsOo_rename ->
                                                            (case (({-# LINE 668 "src-ag/DefaultRules.ag" #-}
                                                                    Map.findWithDefault Map.empty nt_ _lhsImergesIn
                                                                    {-# LINE 846 "src-ag/DefaultRules.hs" #-}
                                                                    )) of
                                                             { !_mergesIn ->
                                                             (case (({-# LINE 664 "src-ag/DefaultRules.ag" #-}
                                                                     _mergesIn
                                                                     {-# LINE 851 "src-ag/DefaultRules.hs" #-}
                                                                     )) of
                                                              { !_prodsOmergesIn ->
                                                              (case (({-# LINE 582 "src-ag/DefaultRules.ag" #-}
                                                                      _lhsImanualAttrOrderMap
                                                                      {-# LINE 856 "src-ag/DefaultRules.hs" #-}
                                                                      )) of
                                                               { !_prodsOmanualAttrOrderMap ->
                                                               (case (({-# LINE 43 "src-ag/DefaultRules.ag" #-}
                                                                       _lhsIcr
                                                                       {-# LINE 861 "src-ag/DefaultRules.hs" #-}
                                                                       )) of
                                                                { !_prodsOcr ->
                                                                (case (({-# LINE 155 "src-ag/DefaultRules.ag" #-}
                                                                        nt_
                                                                        {-# LINE 866 "src-ag/DefaultRules.hs" #-}
                                                                        )) of
                                                                 { !_prodsOnt ->
                                                                 (case (({-# LINE 143 "src-ag/DefaultRules.ag" #-}
                                                                         Map.findWithDefault Map.empty nt_ _lhsIuseMap
                                                                         {-# LINE 871 "src-ag/DefaultRules.hs" #-}
                                                                         )) of
                                                                  { !_prodsOuseMap ->
                                                                  (case (({-# LINE 142 "src-ag/DefaultRules.ag" #-}
                                                                          syn_
                                                                          {-# LINE 876 "src-ag/DefaultRules.hs" #-}
                                                                          )) of
                                                                   { !_prodsOsyn ->
                                                                   (case (({-# LINE 141 "src-ag/DefaultRules.ag" #-}
                                                                           inh_
                                                                           {-# LINE 881 "src-ag/DefaultRules.hs" #-}
                                                                           )) of
                                                                    { !_prodsOinh ->
                                                                    (case (({-# LINE 494 "src-ag/DefaultRules.ag" #-}
                                                                            _lhsIuniq
                                                                            {-# LINE 886 "src-ag/DefaultRules.hs" #-}
                                                                            )) of
                                                                     { !_prodsOuniq ->
                                                                     (case (({-# LINE 128 "src-ag/DefaultRules.ag" #-}
                                                                             _lhsInonterminals
                                                                             {-# LINE 891 "src-ag/DefaultRules.hs" #-}
                                                                             )) of
                                                                      { !_prodsOnonterminals ->
                                                                      (case (({-# LINE 653 "src-ag/DefaultRules.ag" #-}
                                                                              Map.findWithDefault Map.empty nt_ _lhsIaugmentsIn
                                                                              {-# LINE 896 "src-ag/DefaultRules.hs" #-}
                                                                              )) of
                                                                       { !_augmentsIn ->
                                                                       (case (({-# LINE 650 "src-ag/DefaultRules.ag" #-}
                                                                               _augmentsIn
                                                                               {-# LINE 901 "src-ag/DefaultRules.hs" #-}
                                                                               )) of
                                                                        { !_prodsOaugmentsIn ->
                                                                        (case (({-# LINE 660 "src-ag/DefaultRules.ag" #-}
                                                                                Map.findWithDefault Map.empty nt_ _lhsIaroundsIn
                                                                                {-# LINE 906 "src-ag/DefaultRules.hs" #-}
                                                                                )) of
                                                                         { !_aroundsIn ->
                                                                         (case (({-# LINE 657 "src-ag/DefaultRules.ag" #-}
                                                                                 _aroundsIn
                                                                                 {-# LINE 911 "src-ag/DefaultRules.hs" #-}
                                                                                 )) of
                                                                          { !_prodsOaroundsIn ->
                                                                          (case (prods_ _prodsOaroundsIn _prodsOaugmentsIn _prodsOcr _prodsOinh _prodsOmanualAttrOrderMap _prodsOmergesIn _prodsOnonterminals _prodsOnt _prodsOo_rename _prodsOsyn _prodsOtypeSyns _prodsOuniq _prodsOuseMap ) of
                                                                           { ( !_prodsIerrors,!_prodsIoutput,!_prodsIuniq) ->
                                                                               (case (({-# LINE 118 "src-ag/DefaultRules.ag" #-}
                                                                                       _prodsIerrors
                                                                                       {-# LINE 918 "src-ag/DefaultRules.hs" #-}
                                                                                       )) of
                                                                                { !_lhsOerrors ->
                                                                                (case (({-# LINE 501 "src-ag/DefaultRules.ag" #-}
                                                                                        Nonterminal nt_ params_ inh_ syn_ _prodsIoutput
                                                                                        {-# LINE 923 "src-ag/DefaultRules.hs" #-}
                                                                                        )) of
                                                                                 { !_output ->
                                                                                 (case (({-# LINE 501 "src-ag/DefaultRules.ag" #-}
                                                                                         _output
                                                                                         {-# LINE 928 "src-ag/DefaultRules.hs" #-}
                                                                                         )) of
                                                                                  { !_lhsOoutput ->
                                                                                  (case (({-# LINE 494 "src-ag/DefaultRules.ag" #-}
                                                                                          _prodsIuniq
                                                                                          {-# LINE 933 "src-ag/DefaultRules.hs" #-}
                                                                                          )) of
                                                                                   { !_lhsOuniq ->
                                                                                   ( _lhsOerrors,_lhsOoutput,_lhsOuniq) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })) )
                            in  sem_Nonterminal_Nonterminal_1)) of
                     { ( !sem_Nonterminal_1) ->
                     ( _lhsOcollect_nts,sem_Nonterminal_1) }) }) )
-- Nonterminals ------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         collect_nts          : Set NontermIdent
   visit 1:
      inherited attributes:
         aroundsIn            : Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))
         augmentsIn           : Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))
         cr                   : Bool
         manualAttrOrderMap   : AttrOrderMap
         mergesIn             : Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier],Expression)))
         nonterminals         : Set NontermIdent
         o_rename             : Bool
         typeSyns             : TypeSyns
         useMap               : UseMap
      chained attribute:
         uniq                 : Int
      synthesized attributes:
         errors               : Seq Error
         output               : SELF 
   alternatives:
      alternative Cons:
         child hd             : Nonterminal 
         child tl             : Nonterminals 
         visit 1:
            local output      : _
      alternative Nil:
         visit 1:
            local output      : _
-}
-- cata
sem_Nonterminals :: Nonterminals  ->
                    T_Nonterminals 
sem_Nonterminals !list  =
    (Prelude.foldr sem_Nonterminals_Cons sem_Nonterminals_Nil (Prelude.map sem_Nonterminal list) )
-- semantic domain
newtype T_Nonterminals  = T_Nonterminals (( (Set NontermIdent),T_Nonterminals_1 ))
newtype T_Nonterminals_1  = T_Nonterminals_1 ((Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) ->
                                              (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) ->
                                              Bool ->
                                              AttrOrderMap ->
                                              (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier],Expression)))) ->
                                              (Set NontermIdent) ->
                                              Bool ->
                                              TypeSyns ->
                                              Int ->
                                              UseMap ->
                                              ( (Seq Error),Nonterminals ,Int))
data Inh_Nonterminals  = Inh_Nonterminals {aroundsIn_Inh_Nonterminals :: !((Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression])))),augmentsIn_Inh_Nonterminals :: !((Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression])))),cr_Inh_Nonterminals :: !(Bool),manualAttrOrderMap_Inh_Nonterminals :: !(AttrOrderMap),mergesIn_Inh_Nonterminals :: !((Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier],Expression))))),nonterminals_Inh_Nonterminals :: !((Set NontermIdent)),o_rename_Inh_Nonterminals :: !(Bool),typeSyns_Inh_Nonterminals :: !(TypeSyns),uniq_Inh_Nonterminals :: !(Int),useMap_Inh_Nonterminals :: !(UseMap)}
data Syn_Nonterminals  = Syn_Nonterminals {collect_nts_Syn_Nonterminals :: !((Set NontermIdent)),errors_Syn_Nonterminals :: !((Seq Error)),output_Syn_Nonterminals :: !(Nonterminals ),uniq_Syn_Nonterminals :: !(Int)}
wrap_Nonterminals :: T_Nonterminals  ->
                     Inh_Nonterminals  ->
                     Syn_Nonterminals 
wrap_Nonterminals !(T_Nonterminals sem ) !(Inh_Nonterminals _lhsIaroundsIn _lhsIaugmentsIn _lhsIcr _lhsImanualAttrOrderMap _lhsImergesIn _lhsInonterminals _lhsIo_rename _lhsItypeSyns _lhsIuniq _lhsIuseMap )  =
    (let ( !_lhsOcollect_nts,!T_Nonterminals_1 sem_1) = sem 
         ( !_lhsOerrors,!_lhsOoutput,!_lhsOuniq) = sem_1 _lhsIaroundsIn _lhsIaugmentsIn _lhsIcr _lhsImanualAttrOrderMap _lhsImergesIn _lhsInonterminals _lhsIo_rename _lhsItypeSyns _lhsIuniq _lhsIuseMap 
     in  (Syn_Nonterminals _lhsOcollect_nts _lhsOerrors _lhsOoutput _lhsOuniq ))
sem_Nonterminals_Cons :: T_Nonterminal  ->
                         T_Nonterminals  ->
                         T_Nonterminals 
sem_Nonterminals_Cons !(T_Nonterminal hd_ ) !(T_Nonterminals tl_ )  =
    (T_Nonterminals (case (tl_ ) of
                     { ( !_tlIcollect_nts,!T_Nonterminals_1 tl_1) ->
                         (case (hd_ ) of
                          { ( !_hdIcollect_nts,!T_Nonterminal_1 hd_1) ->
                              (case (({-# LINE 124 "src-ag/DefaultRules.ag" #-}
                                      _hdIcollect_nts `Set.union` _tlIcollect_nts
                                      {-# LINE 1008 "src-ag/DefaultRules.hs" #-}
                                      )) of
                               { !_lhsOcollect_nts ->
                               (case ((let sem_Nonterminals_Cons_1 :: T_Nonterminals_1 
                                           sem_Nonterminals_Cons_1  =
                                               (T_Nonterminals_1 (\ (!_lhsIaroundsIn)
                                                                    (!_lhsIaugmentsIn)
                                                                    (!_lhsIcr)
                                                                    (!_lhsImanualAttrOrderMap)
                                                                    (!_lhsImergesIn)
                                                                    (!_lhsInonterminals)
                                                                    (!_lhsIo_rename)
                                                                    (!_lhsItypeSyns)
                                                                    (!_lhsIuniq)
                                                                    (!_lhsIuseMap) ->
                                                                      (case (({-# LINE 137 "src-ag/DefaultRules.ag" #-}
                                                                              _lhsIuseMap
                                                                              {-# LINE 1025 "src-ag/DefaultRules.hs" #-}
                                                                              )) of
                                                                       { !_tlOuseMap ->
                                                                       (case (({-# LINE 54 "src-ag/DefaultRules.ag" #-}
                                                                               _lhsItypeSyns
                                                                               {-# LINE 1030 "src-ag/DefaultRules.hs" #-}
                                                                               )) of
                                                                        { !_tlOtypeSyns ->
                                                                        (case (({-# LINE 42 "src-ag/DefaultRules.ag" #-}
                                                                                _lhsIo_rename
                                                                                {-# LINE 1035 "src-ag/DefaultRules.hs" #-}
                                                                                )) of
                                                                         { !_tlOo_rename ->
                                                                         (case (({-# LINE 663 "src-ag/DefaultRules.ag" #-}
                                                                                 _lhsImergesIn
                                                                                 {-# LINE 1040 "src-ag/DefaultRules.hs" #-}
                                                                                 )) of
                                                                          { !_tlOmergesIn ->
                                                                          (case (({-# LINE 582 "src-ag/DefaultRules.ag" #-}
                                                                                  _lhsImanualAttrOrderMap
                                                                                  {-# LINE 1045 "src-ag/DefaultRules.hs" #-}
                                                                                  )) of
                                                                           { !_tlOmanualAttrOrderMap ->
                                                                           (case (({-# LINE 43 "src-ag/DefaultRules.ag" #-}
                                                                                   _lhsIcr
                                                                                   {-# LINE 1050 "src-ag/DefaultRules.hs" #-}
                                                                                   )) of
                                                                            { !_tlOcr ->
                                                                            (case (({-# LINE 137 "src-ag/DefaultRules.ag" #-}
                                                                                    _lhsIuseMap
                                                                                    {-# LINE 1055 "src-ag/DefaultRules.hs" #-}
                                                                                    )) of
                                                                             { !_hdOuseMap ->
                                                                             (case (({-# LINE 54 "src-ag/DefaultRules.ag" #-}
                                                                                     _lhsItypeSyns
                                                                                     {-# LINE 1060 "src-ag/DefaultRules.hs" #-}
                                                                                     )) of
                                                                              { !_hdOtypeSyns ->
                                                                              (case (({-# LINE 42 "src-ag/DefaultRules.ag" #-}
                                                                                      _lhsIo_rename
                                                                                      {-# LINE 1065 "src-ag/DefaultRules.hs" #-}
                                                                                      )) of
                                                                               { !_hdOo_rename ->
                                                                               (case (({-# LINE 663 "src-ag/DefaultRules.ag" #-}
                                                                                       _lhsImergesIn
                                                                                       {-# LINE 1070 "src-ag/DefaultRules.hs" #-}
                                                                                       )) of
                                                                                { !_hdOmergesIn ->
                                                                                (case (({-# LINE 582 "src-ag/DefaultRules.ag" #-}
                                                                                        _lhsImanualAttrOrderMap
                                                                                        {-# LINE 1075 "src-ag/DefaultRules.hs" #-}
                                                                                        )) of
                                                                                 { !_hdOmanualAttrOrderMap ->
                                                                                 (case (({-# LINE 43 "src-ag/DefaultRules.ag" #-}
                                                                                         _lhsIcr
                                                                                         {-# LINE 1080 "src-ag/DefaultRules.hs" #-}
                                                                                         )) of
                                                                                  { !_hdOcr ->
                                                                                  (case (({-# LINE 494 "src-ag/DefaultRules.ag" #-}
                                                                                          _lhsIuniq
                                                                                          {-# LINE 1085 "src-ag/DefaultRules.hs" #-}
                                                                                          )) of
                                                                                   { !_hdOuniq ->
                                                                                   (case (({-# LINE 128 "src-ag/DefaultRules.ag" #-}
                                                                                           _lhsInonterminals
                                                                                           {-# LINE 1090 "src-ag/DefaultRules.hs" #-}
                                                                                           )) of
                                                                                    { !_hdOnonterminals ->
                                                                                    (case (({-# LINE 649 "src-ag/DefaultRules.ag" #-}
                                                                                            _lhsIaugmentsIn
                                                                                            {-# LINE 1095 "src-ag/DefaultRules.hs" #-}
                                                                                            )) of
                                                                                     { !_hdOaugmentsIn ->
                                                                                     (case (({-# LINE 656 "src-ag/DefaultRules.ag" #-}
                                                                                             _lhsIaroundsIn
                                                                                             {-# LINE 1100 "src-ag/DefaultRules.hs" #-}
                                                                                             )) of
                                                                                      { !_hdOaroundsIn ->
                                                                                      (case (hd_1 _hdOaroundsIn _hdOaugmentsIn _hdOcr _hdOmanualAttrOrderMap _hdOmergesIn _hdOnonterminals _hdOo_rename _hdOtypeSyns _hdOuniq _hdOuseMap ) of
                                                                                       { ( !_hdIerrors,!_hdIoutput,!_hdIuniq) ->
                                                                                           (case (({-# LINE 494 "src-ag/DefaultRules.ag" #-}
                                                                                                   _hdIuniq
                                                                                                   {-# LINE 1107 "src-ag/DefaultRules.hs" #-}
                                                                                                   )) of
                                                                                            { !_tlOuniq ->
                                                                                            (case (({-# LINE 128 "src-ag/DefaultRules.ag" #-}
                                                                                                    _lhsInonterminals
                                                                                                    {-# LINE 1112 "src-ag/DefaultRules.hs" #-}
                                                                                                    )) of
                                                                                             { !_tlOnonterminals ->
                                                                                             (case (({-# LINE 649 "src-ag/DefaultRules.ag" #-}
                                                                                                     _lhsIaugmentsIn
                                                                                                     {-# LINE 1117 "src-ag/DefaultRules.hs" #-}
                                                                                                     )) of
                                                                                              { !_tlOaugmentsIn ->
                                                                                              (case (({-# LINE 656 "src-ag/DefaultRules.ag" #-}
                                                                                                      _lhsIaroundsIn
                                                                                                      {-# LINE 1122 "src-ag/DefaultRules.hs" #-}
                                                                                                      )) of
                                                                                               { !_tlOaroundsIn ->
                                                                                               (case (tl_1 _tlOaroundsIn _tlOaugmentsIn _tlOcr _tlOmanualAttrOrderMap _tlOmergesIn _tlOnonterminals _tlOo_rename _tlOtypeSyns _tlOuniq _tlOuseMap ) of
                                                                                                { ( !_tlIerrors,!_tlIoutput,!_tlIuniq) ->
                                                                                                    (case (({-# LINE 118 "src-ag/DefaultRules.ag" #-}
                                                                                                            _hdIerrors Seq.>< _tlIerrors
                                                                                                            {-# LINE 1129 "src-ag/DefaultRules.hs" #-}
                                                                                                            )) of
                                                                                                     { !_lhsOerrors ->
                                                                                                     (case (({-# LINE 501 "src-ag/DefaultRules.ag" #-}
                                                                                                             (:) _hdIoutput _tlIoutput
                                                                                                             {-# LINE 1134 "src-ag/DefaultRules.hs" #-}
                                                                                                             )) of
                                                                                                      { !_output ->
                                                                                                      (case (({-# LINE 501 "src-ag/DefaultRules.ag" #-}
                                                                                                              _output
                                                                                                              {-# LINE 1139 "src-ag/DefaultRules.hs" #-}
                                                                                                              )) of
                                                                                                       { !_lhsOoutput ->
                                                                                                       (case (({-# LINE 494 "src-ag/DefaultRules.ag" #-}
                                                                                                               _tlIuniq
                                                                                                               {-# LINE 1144 "src-ag/DefaultRules.hs" #-}
                                                                                                               )) of
                                                                                                        { !_lhsOuniq ->
                                                                                                        ( _lhsOerrors,_lhsOoutput,_lhsOuniq) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })) )
                                       in  sem_Nonterminals_Cons_1)) of
                                { ( !sem_Nonterminals_1) ->
                                ( _lhsOcollect_nts,sem_Nonterminals_1) }) }) }) }) )
sem_Nonterminals_Nil :: T_Nonterminals 
sem_Nonterminals_Nil  =
    (T_Nonterminals (case (({-# LINE 124 "src-ag/DefaultRules.ag" #-}
                            Set.empty
                            {-# LINE 1155 "src-ag/DefaultRules.hs" #-}
                            )) of
                     { !_lhsOcollect_nts ->
                     (case ((let sem_Nonterminals_Nil_1 :: T_Nonterminals_1 
                                 sem_Nonterminals_Nil_1  =
                                     (T_Nonterminals_1 (\ (!_lhsIaroundsIn)
                                                          (!_lhsIaugmentsIn)
                                                          (!_lhsIcr)
                                                          (!_lhsImanualAttrOrderMap)
                                                          (!_lhsImergesIn)
                                                          (!_lhsInonterminals)
                                                          (!_lhsIo_rename)
                                                          (!_lhsItypeSyns)
                                                          (!_lhsIuniq)
                                                          (!_lhsIuseMap) ->
                                                            (case (({-# LINE 118 "src-ag/DefaultRules.ag" #-}
                                                                    Seq.empty
                                                                    {-# LINE 1172 "src-ag/DefaultRules.hs" #-}
                                                                    )) of
                                                             { !_lhsOerrors ->
                                                             (case (({-# LINE 501 "src-ag/DefaultRules.ag" #-}
                                                                     []
                                                                     {-# LINE 1177 "src-ag/DefaultRules.hs" #-}
                                                                     )) of
                                                              { !_output ->
                                                              (case (({-# LINE 501 "src-ag/DefaultRules.ag" #-}
                                                                      _output
                                                                      {-# LINE 1182 "src-ag/DefaultRules.hs" #-}
                                                                      )) of
                                                               { !_lhsOoutput ->
                                                               (case (({-# LINE 494 "src-ag/DefaultRules.ag" #-}
                                                                       _lhsIuniq
                                                                       {-# LINE 1187 "src-ag/DefaultRules.hs" #-}
                                                                       )) of
                                                                { !_lhsOuniq ->
                                                                ( _lhsOerrors,_lhsOoutput,_lhsOuniq) }) }) }) })) )
                             in  sem_Nonterminals_Nil_1)) of
                      { ( !sem_Nonterminals_1) ->
                      ( _lhsOcollect_nts,sem_Nonterminals_1) }) }) )
-- Pattern -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         con                  : ConstructorIdent
         nt                   : NontermIdent
      synthesized attributes:
         containsVars         : Bool
         copy                 : SELF 
         definedAttrs         : Set (Identifier,Identifier)
         errors               : Seq Error
         locals               : Set Identifier
         output               : SELF 
   alternatives:
      alternative Alias:
         child field          : {Identifier}
         child attr           : {Identifier}
         child pat            : Pattern 
         child parts          : Patterns 
         visit 0:
            local copy        : _
            local output      : _
      alternative Constr:
         child name           : {ConstructorIdent}
         child pats           : Patterns 
         visit 0:
            local copy        : _
            local output      : _
      alternative Irrefutable:
         child pat            : Pattern 
         visit 0:
            local copy        : _
            local output      : _
      alternative Product:
         child pos            : {Pos}
         child pats           : Patterns 
         visit 0:
            local copy        : _
            local output      : _
      alternative Underscore:
         child pos            : {Pos}
         visit 0:
            local copy        : _
            local output      : _
-}
-- cata
sem_Pattern :: Pattern  ->
               T_Pattern 
sem_Pattern !(Alias _field _attr _pat _parts )  =
    (sem_Pattern_Alias _field _attr (sem_Pattern _pat ) (sem_Patterns _parts ) )
sem_Pattern !(Constr _name _pats )  =
    (sem_Pattern_Constr _name (sem_Patterns _pats ) )
sem_Pattern !(Irrefutable _pat )  =
    (sem_Pattern_Irrefutable (sem_Pattern _pat ) )
sem_Pattern !(Product _pos _pats )  =
    (sem_Pattern_Product _pos (sem_Patterns _pats ) )
sem_Pattern !(Underscore _pos )  =
    (sem_Pattern_Underscore _pos )
-- semantic domain
newtype T_Pattern  = T_Pattern (ConstructorIdent ->
                                NontermIdent ->
                                ( Bool,Pattern ,(Set (Identifier,Identifier)),(Seq Error),(Set Identifier),Pattern ))
data Inh_Pattern  = Inh_Pattern {con_Inh_Pattern :: !(ConstructorIdent),nt_Inh_Pattern :: !(NontermIdent)}
data Syn_Pattern  = Syn_Pattern {containsVars_Syn_Pattern :: !(Bool),copy_Syn_Pattern :: !(Pattern ),definedAttrs_Syn_Pattern :: !((Set (Identifier,Identifier))),errors_Syn_Pattern :: !((Seq Error)),locals_Syn_Pattern :: !((Set Identifier)),output_Syn_Pattern :: !(Pattern )}
wrap_Pattern :: T_Pattern  ->
                Inh_Pattern  ->
                Syn_Pattern 
wrap_Pattern !(T_Pattern sem ) !(Inh_Pattern _lhsIcon _lhsInt )  =
    (let ( !_lhsOcontainsVars,!_lhsOcopy,!_lhsOdefinedAttrs,!_lhsOerrors,!_lhsOlocals,!_lhsOoutput) = sem _lhsIcon _lhsInt 
     in  (Syn_Pattern _lhsOcontainsVars _lhsOcopy _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput ))
sem_Pattern_Alias :: Identifier ->
                     Identifier ->
                     T_Pattern  ->
                     T_Patterns  ->
                     T_Pattern 
sem_Pattern_Alias !field_ !attr_ !(T_Pattern pat_ ) !(T_Patterns parts_ )  =
    (T_Pattern (\ (!_lhsIcon)
                  (!_lhsInt) ->
                    (case (({-# LINE 488 "src-ag/DefaultRules.ag" #-}
                            True
                            {-# LINE 1274 "src-ag/DefaultRules.hs" #-}
                            )) of
                     { !_lhsOcontainsVars ->
                     (case (({-# LINE 33 "src-ag/DefaultRules.ag" #-}
                             _lhsInt
                             {-# LINE 1279 "src-ag/DefaultRules.hs" #-}
                             )) of
                      { !_partsOnt ->
                      (case (({-# LINE 34 "src-ag/DefaultRules.ag" #-}
                              _lhsIcon
                              {-# LINE 1284 "src-ag/DefaultRules.hs" #-}
                              )) of
                       { !_partsOcon ->
                       (case (parts_ _partsOcon _partsOnt ) of
                        { ( !_partsIcontainsVars,!_partsIcopy,!_partsIdefinedAttrs,!_partsIerrors,!_partsIlocals,!_partsIoutput) ->
                            (case (({-# LINE 33 "src-ag/DefaultRules.ag" #-}
                                    _lhsInt
                                    {-# LINE 1291 "src-ag/DefaultRules.hs" #-}
                                    )) of
                             { !_patOnt ->
                             (case (({-# LINE 34 "src-ag/DefaultRules.ag" #-}
                                     _lhsIcon
                                     {-# LINE 1296 "src-ag/DefaultRules.hs" #-}
                                     )) of
                              { !_patOcon ->
                              (case (pat_ _patOcon _patOnt ) of
                               { ( !_patIcontainsVars,!_patIcopy,!_patIdefinedAttrs,!_patIerrors,!_patIlocals,!_patIoutput) ->
                                   (case (({-# LINE 23 "src-ag/Patterns.ag" #-}
                                           Alias field_ attr_ _patIcopy _partsIcopy
                                           {-# LINE 1303 "src-ag/DefaultRules.hs" #-}
                                           )) of
                                    { !_copy ->
                                    (case (({-# LINE 23 "src-ag/Patterns.ag" #-}
                                            _copy
                                            {-# LINE 1308 "src-ag/DefaultRules.hs" #-}
                                            )) of
                                     { !_lhsOcopy ->
                                     (case (({-# LINE 470 "src-ag/DefaultRules.ag" #-}
                                             Set.insert (field_,attr_) _patIdefinedAttrs
                                             {-# LINE 1313 "src-ag/DefaultRules.hs" #-}
                                             )) of
                                      { !_lhsOdefinedAttrs ->
                                      (case (({-# LINE 118 "src-ag/DefaultRules.ag" #-}
                                              _patIerrors Seq.>< _partsIerrors
                                              {-# LINE 1318 "src-ag/DefaultRules.hs" #-}
                                              )) of
                                       { !_lhsOerrors ->
                                       (case (({-# LINE 471 "src-ag/DefaultRules.ag" #-}
                                               if field_ == _LOC
                                                  then Set.insert attr_ _patIlocals
                                                  else _patIlocals
                                               {-# LINE 1325 "src-ag/DefaultRules.hs" #-}
                                               )) of
                                        { !_lhsOlocals ->
                                        (case (({-# LINE 501 "src-ag/DefaultRules.ag" #-}
                                                Alias field_ attr_ _patIoutput _partsIoutput
                                                {-# LINE 1330 "src-ag/DefaultRules.hs" #-}
                                                )) of
                                         { !_output ->
                                         (case (({-# LINE 501 "src-ag/DefaultRules.ag" #-}
                                                 _output
                                                 {-# LINE 1335 "src-ag/DefaultRules.hs" #-}
                                                 )) of
                                          { !_lhsOoutput ->
                                          ( _lhsOcontainsVars,_lhsOcopy,_lhsOdefinedAttrs,_lhsOerrors,_lhsOlocals,_lhsOoutput) }) }) }) }) }) }) }) }) }) }) }) }) }) })) )
sem_Pattern_Constr :: ConstructorIdent ->
                      T_Patterns  ->
                      T_Pattern 
sem_Pattern_Constr !name_ !(T_Patterns pats_ )  =
    (T_Pattern (\ (!_lhsIcon)
                  (!_lhsInt) ->
                    (case (({-# LINE 33 "src-ag/DefaultRules.ag" #-}
                            _lhsInt
                            {-# LINE 1347 "src-ag/DefaultRules.hs" #-}
                            )) of
                     { !_patsOnt ->
                     (case (({-# LINE 34 "src-ag/DefaultRules.ag" #-}
                             _lhsIcon
                             {-# LINE 1352 "src-ag/DefaultRules.hs" #-}
                             )) of
                      { !_patsOcon ->
                      (case (pats_ _patsOcon _patsOnt ) of
                       { ( !_patsIcontainsVars,!_patsIcopy,!_patsIdefinedAttrs,!_patsIerrors,!_patsIlocals,!_patsIoutput) ->
                           (case (({-# LINE 485 "src-ag/DefaultRules.ag" #-}
                                   _patsIcontainsVars
                                   {-# LINE 1359 "src-ag/DefaultRules.hs" #-}
                                   )) of
                            { !_lhsOcontainsVars ->
                            (case (({-# LINE 23 "src-ag/Patterns.ag" #-}
                                    Constr name_ _patsIcopy
                                    {-# LINE 1364 "src-ag/DefaultRules.hs" #-}
                                    )) of
                             { !_copy ->
                             (case (({-# LINE 23 "src-ag/Patterns.ag" #-}
                                     _copy
                                     {-# LINE 1369 "src-ag/DefaultRules.hs" #-}
                                     )) of
                              { !_lhsOcopy ->
                              (case (({-# LINE 465 "src-ag/DefaultRules.ag" #-}
                                      _patsIdefinedAttrs
                                      {-# LINE 1374 "src-ag/DefaultRules.hs" #-}
                                      )) of
                               { !_lhsOdefinedAttrs ->
                               (case (({-# LINE 118 "src-ag/DefaultRules.ag" #-}
                                       _patsIerrors
                                       {-# LINE 1379 "src-ag/DefaultRules.hs" #-}
                                       )) of
                                { !_lhsOerrors ->
                                (case (({-# LINE 464 "src-ag/DefaultRules.ag" #-}
                                        _patsIlocals
                                        {-# LINE 1384 "src-ag/DefaultRules.hs" #-}
                                        )) of
                                 { !_lhsOlocals ->
                                 (case (({-# LINE 501 "src-ag/DefaultRules.ag" #-}
                                         Constr name_ _patsIoutput
                                         {-# LINE 1389 "src-ag/DefaultRules.hs" #-}
                                         )) of
                                  { !_output ->
                                  (case (({-# LINE 501 "src-ag/DefaultRules.ag" #-}
                                          _output
                                          {-# LINE 1394 "src-ag/DefaultRules.hs" #-}
                                          )) of
                                   { !_lhsOoutput ->
                                   ( _lhsOcontainsVars,_lhsOcopy,_lhsOdefinedAttrs,_lhsOerrors,_lhsOlocals,_lhsOoutput) }) }) }) }) }) }) }) }) }) }) })) )
sem_Pattern_Irrefutable :: T_Pattern  ->
                           T_Pattern 
sem_Pattern_Irrefutable !(T_Pattern pat_ )  =
    (T_Pattern (\ (!_lhsIcon)
                  (!_lhsInt) ->
                    (case (({-# LINE 33 "src-ag/DefaultRules.ag" #-}
                            _lhsInt
                            {-# LINE 1405 "src-ag/DefaultRules.hs" #-}
                            )) of
                     { !_patOnt ->
                     (case (({-# LINE 34 "src-ag/DefaultRules.ag" #-}
                             _lhsIcon
                             {-# LINE 1410 "src-ag/DefaultRules.hs" #-}
                             )) of
                      { !_patOcon ->
                      (case (pat_ _patOcon _patOnt ) of
                       { ( !_patIcontainsVars,!_patIcopy,!_patIdefinedAttrs,!_patIerrors,!_patIlocals,!_patIoutput) ->
                           (case (({-# LINE 485 "src-ag/DefaultRules.ag" #-}
                                   _patIcontainsVars
                                   {-# LINE 1417 "src-ag/DefaultRules.hs" #-}
                                   )) of
                            { !_lhsOcontainsVars ->
                            (case (({-# LINE 23 "src-ag/Patterns.ag" #-}
                                    Irrefutable _patIcopy
                                    {-# LINE 1422 "src-ag/DefaultRules.hs" #-}
                                    )) of
                             { !_copy ->
                             (case (({-# LINE 23 "src-ag/Patterns.ag" #-}
                                     _copy
                                     {-# LINE 1427 "src-ag/DefaultRules.hs" #-}
                                     )) of
                              { !_lhsOcopy ->
                              (case (({-# LINE 465 "src-ag/DefaultRules.ag" #-}
                                      _patIdefinedAttrs
                                      {-# LINE 1432 "src-ag/DefaultRules.hs" #-}
                                      )) of
                               { !_lhsOdefinedAttrs ->
                               (case (({-# LINE 118 "src-ag/DefaultRules.ag" #-}
                                       _patIerrors
                                       {-# LINE 1437 "src-ag/DefaultRules.hs" #-}
                                       )) of
                                { !_lhsOerrors ->
                                (case (({-# LINE 464 "src-ag/DefaultRules.ag" #-}
                                        _patIlocals
                                        {-# LINE 1442 "src-ag/DefaultRules.hs" #-}
                                        )) of
                                 { !_lhsOlocals ->
                                 (case (({-# LINE 501 "src-ag/DefaultRules.ag" #-}
                                         Irrefutable _patIoutput
                                         {-# LINE 1447 "src-ag/DefaultRules.hs" #-}
                                         )) of
                                  { !_output ->
                                  (case (({-# LINE 501 "src-ag/DefaultRules.ag" #-}
                                          _output
                                          {-# LINE 1452 "src-ag/DefaultRules.hs" #-}
                                          )) of
                                   { !_lhsOoutput ->
                                   ( _lhsOcontainsVars,_lhsOcopy,_lhsOdefinedAttrs,_lhsOerrors,_lhsOlocals,_lhsOoutput) }) }) }) }) }) }) }) }) }) }) })) )
sem_Pattern_Product :: Pos ->
                       T_Patterns  ->
                       T_Pattern 
sem_Pattern_Product !pos_ !(T_Patterns pats_ )  =
    (T_Pattern (\ (!_lhsIcon)
                  (!_lhsInt) ->
                    (case (({-# LINE 33 "src-ag/DefaultRules.ag" #-}
                            _lhsInt
                            {-# LINE 1464 "src-ag/DefaultRules.hs" #-}
                            )) of
                     { !_patsOnt ->
                     (case (({-# LINE 34 "src-ag/DefaultRules.ag" #-}
                             _lhsIcon
                             {-# LINE 1469 "src-ag/DefaultRules.hs" #-}
                             )) of
                      { !_patsOcon ->
                      (case (pats_ _patsOcon _patsOnt ) of
                       { ( !_patsIcontainsVars,!_patsIcopy,!_patsIdefinedAttrs,!_patsIerrors,!_patsIlocals,!_patsIoutput) ->
                           (case (({-# LINE 485 "src-ag/DefaultRules.ag" #-}
                                   _patsIcontainsVars
                                   {-# LINE 1476 "src-ag/DefaultRules.hs" #-}
                                   )) of
                            { !_lhsOcontainsVars ->
                            (case (({-# LINE 23 "src-ag/Patterns.ag" #-}
                                    Product pos_ _patsIcopy
                                    {-# LINE 1481 "src-ag/DefaultRules.hs" #-}
                                    )) of
                             { !_copy ->
                             (case (({-# LINE 23 "src-ag/Patterns.ag" #-}
                                     _copy
                                     {-# LINE 1486 "src-ag/DefaultRules.hs" #-}
                                     )) of
                              { !_lhsOcopy ->
                              (case (({-# LINE 465 "src-ag/DefaultRules.ag" #-}
                                      _patsIdefinedAttrs
                                      {-# LINE 1491 "src-ag/DefaultRules.hs" #-}
                                      )) of
                               { !_lhsOdefinedAttrs ->
                               (case (({-# LINE 118 "src-ag/DefaultRules.ag" #-}
                                       _patsIerrors
                                       {-# LINE 1496 "src-ag/DefaultRules.hs" #-}
                                       )) of
                                { !_lhsOerrors ->
                                (case (({-# LINE 464 "src-ag/DefaultRules.ag" #-}
                                        _patsIlocals
                                        {-# LINE 1501 "src-ag/DefaultRules.hs" #-}
                                        )) of
                                 { !_lhsOlocals ->
                                 (case (({-# LINE 501 "src-ag/DefaultRules.ag" #-}
                                         Product pos_ _patsIoutput
                                         {-# LINE 1506 "src-ag/DefaultRules.hs" #-}
                                         )) of
                                  { !_output ->
                                  (case (({-# LINE 501 "src-ag/DefaultRules.ag" #-}
                                          _output
                                          {-# LINE 1511 "src-ag/DefaultRules.hs" #-}
                                          )) of
                                   { !_lhsOoutput ->
                                   ( _lhsOcontainsVars,_lhsOcopy,_lhsOdefinedAttrs,_lhsOerrors,_lhsOlocals,_lhsOoutput) }) }) }) }) }) }) }) }) }) }) })) )
sem_Pattern_Underscore :: Pos ->
                          T_Pattern 
sem_Pattern_Underscore !pos_  =
    (T_Pattern (\ (!_lhsIcon)
                  (!_lhsInt) ->
                    (case (({-# LINE 485 "src-ag/DefaultRules.ag" #-}
                            False
                            {-# LINE 1522 "src-ag/DefaultRules.hs" #-}
                            )) of
                     { !_lhsOcontainsVars ->
                     (case (({-# LINE 23 "src-ag/Patterns.ag" #-}
                             Underscore pos_
                             {-# LINE 1527 "src-ag/DefaultRules.hs" #-}
                             )) of
                      { !_copy ->
                      (case (({-# LINE 23 "src-ag/Patterns.ag" #-}
                              _copy
                              {-# LINE 1532 "src-ag/DefaultRules.hs" #-}
                              )) of
                       { !_lhsOcopy ->
                       (case (({-# LINE 465 "src-ag/DefaultRules.ag" #-}
                               Set.empty
                               {-# LINE 1537 "src-ag/DefaultRules.hs" #-}
                               )) of
                        { !_lhsOdefinedAttrs ->
                        (case (({-# LINE 118 "src-ag/DefaultRules.ag" #-}
                                Seq.empty
                                {-# LINE 1542 "src-ag/DefaultRules.hs" #-}
                                )) of
                         { !_lhsOerrors ->
                         (case (({-# LINE 464 "src-ag/DefaultRules.ag" #-}
                                 Set.empty
                                 {-# LINE 1547 "src-ag/DefaultRules.hs" #-}
                                 )) of
                          { !_lhsOlocals ->
                          (case (({-# LINE 501 "src-ag/DefaultRules.ag" #-}
                                  Underscore pos_
                                  {-# LINE 1552 "src-ag/DefaultRules.hs" #-}
                                  )) of
                           { !_output ->
                           (case (({-# LINE 501 "src-ag/DefaultRules.ag" #-}
                                   _output
                                   {-# LINE 1557 "src-ag/DefaultRules.hs" #-}
                                   )) of
                            { !_lhsOoutput ->
                            ( _lhsOcontainsVars,_lhsOcopy,_lhsOdefinedAttrs,_lhsOerrors,_lhsOlocals,_lhsOoutput) }) }) }) }) }) }) }) })) )
-- Patterns ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         con                  : ConstructorIdent
         nt                   : NontermIdent
      synthesized attributes:
         containsVars         : Bool
         copy                 : SELF 
         definedAttrs         : Set (Identifier,Identifier)
         errors               : Seq Error
         locals               : Set Identifier
         output               : SELF 
   alternatives:
      alternative Cons:
         child hd             : Pattern 
         child tl             : Patterns 
         visit 0:
            local copy        : _
            local output      : _
      alternative Nil:
         visit 0:
            local copy        : _
            local output      : _
-}
-- cata
sem_Patterns :: Patterns  ->
                T_Patterns 
sem_Patterns !list  =
    (Prelude.foldr sem_Patterns_Cons sem_Patterns_Nil (Prelude.map sem_Pattern list) )
-- semantic domain
newtype T_Patterns  = T_Patterns (ConstructorIdent ->
                                  NontermIdent ->
                                  ( Bool,Patterns ,(Set (Identifier,Identifier)),(Seq Error),(Set Identifier),Patterns ))
data Inh_Patterns  = Inh_Patterns {con_Inh_Patterns :: !(ConstructorIdent),nt_Inh_Patterns :: !(NontermIdent)}
data Syn_Patterns  = Syn_Patterns {containsVars_Syn_Patterns :: !(Bool),copy_Syn_Patterns :: !(Patterns ),definedAttrs_Syn_Patterns :: !((Set (Identifier,Identifier))),errors_Syn_Patterns :: !((Seq Error)),locals_Syn_Patterns :: !((Set Identifier)),output_Syn_Patterns :: !(Patterns )}
wrap_Patterns :: T_Patterns  ->
                 Inh_Patterns  ->
                 Syn_Patterns 
wrap_Patterns !(T_Patterns sem ) !(Inh_Patterns _lhsIcon _lhsInt )  =
    (let ( !_lhsOcontainsVars,!_lhsOcopy,!_lhsOdefinedAttrs,!_lhsOerrors,!_lhsOlocals,!_lhsOoutput) = sem _lhsIcon _lhsInt 
     in  (Syn_Patterns _lhsOcontainsVars _lhsOcopy _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput ))
sem_Patterns_Cons :: T_Pattern  ->
                     T_Patterns  ->
                     T_Patterns 
sem_Patterns_Cons !(T_Pattern hd_ ) !(T_Patterns tl_ )  =
    (T_Patterns (\ (!_lhsIcon)
                   (!_lhsInt) ->
                     (case (({-# LINE 33 "src-ag/DefaultRules.ag" #-}
                             _lhsInt
                             {-# LINE 1611 "src-ag/DefaultRules.hs" #-}
                             )) of
                      { !_tlOnt ->
                      (case (({-# LINE 34 "src-ag/DefaultRules.ag" #-}
                              _lhsIcon
                              {-# LINE 1616 "src-ag/DefaultRules.hs" #-}
                              )) of
                       { !_tlOcon ->
                       (case (tl_ _tlOcon _tlOnt ) of
                        { ( !_tlIcontainsVars,!_tlIcopy,!_tlIdefinedAttrs,!_tlIerrors,!_tlIlocals,!_tlIoutput) ->
                            (case (({-# LINE 33 "src-ag/DefaultRules.ag" #-}
                                    _lhsInt
                                    {-# LINE 1623 "src-ag/DefaultRules.hs" #-}
                                    )) of
                             { !_hdOnt ->
                             (case (({-# LINE 34 "src-ag/DefaultRules.ag" #-}
                                     _lhsIcon
                                     {-# LINE 1628 "src-ag/DefaultRules.hs" #-}
                                     )) of
                              { !_hdOcon ->
                              (case (hd_ _hdOcon _hdOnt ) of
                               { ( !_hdIcontainsVars,!_hdIcopy,!_hdIdefinedAttrs,!_hdIerrors,!_hdIlocals,!_hdIoutput) ->
                                   (case (({-# LINE 485 "src-ag/DefaultRules.ag" #-}
                                           _hdIcontainsVars || _tlIcontainsVars
                                           {-# LINE 1635 "src-ag/DefaultRules.hs" #-}
                                           )) of
                                    { !_lhsOcontainsVars ->
                                    (case (({-# LINE 23 "src-ag/Patterns.ag" #-}
                                            (:) _hdIcopy _tlIcopy
                                            {-# LINE 1640 "src-ag/DefaultRules.hs" #-}
                                            )) of
                                     { !_copy ->
                                     (case (({-# LINE 23 "src-ag/Patterns.ag" #-}
                                             _copy
                                             {-# LINE 1645 "src-ag/DefaultRules.hs" #-}
                                             )) of
                                      { !_lhsOcopy ->
                                      (case (({-# LINE 465 "src-ag/DefaultRules.ag" #-}
                                              _hdIdefinedAttrs `Set.union` _tlIdefinedAttrs
                                              {-# LINE 1650 "src-ag/DefaultRules.hs" #-}
                                              )) of
                                       { !_lhsOdefinedAttrs ->
                                       (case (({-# LINE 118 "src-ag/DefaultRules.ag" #-}
                                               _hdIerrors Seq.>< _tlIerrors
                                               {-# LINE 1655 "src-ag/DefaultRules.hs" #-}
                                               )) of
                                        { !_lhsOerrors ->
                                        (case (({-# LINE 464 "src-ag/DefaultRules.ag" #-}
                                                _hdIlocals `Set.union` _tlIlocals
                                                {-# LINE 1660 "src-ag/DefaultRules.hs" #-}
                                                )) of
                                         { !_lhsOlocals ->
                                         (case (({-# LINE 501 "src-ag/DefaultRules.ag" #-}
                                                 (:) _hdIoutput _tlIoutput
                                                 {-# LINE 1665 "src-ag/DefaultRules.hs" #-}
                                                 )) of
                                          { !_output ->
                                          (case (({-# LINE 501 "src-ag/DefaultRules.ag" #-}
                                                  _output
                                                  {-# LINE 1670 "src-ag/DefaultRules.hs" #-}
                                                  )) of
                                           { !_lhsOoutput ->
                                           ( _lhsOcontainsVars,_lhsOcopy,_lhsOdefinedAttrs,_lhsOerrors,_lhsOlocals,_lhsOoutput) }) }) }) }) }) }) }) }) }) }) }) }) }) })) )
sem_Patterns_Nil :: T_Patterns 
sem_Patterns_Nil  =
    (T_Patterns (\ (!_lhsIcon)
                   (!_lhsInt) ->
                     (case (({-# LINE 485 "src-ag/DefaultRules.ag" #-}
                             False
                             {-# LINE 1680 "src-ag/DefaultRules.hs" #-}
                             )) of
                      { !_lhsOcontainsVars ->
                      (case (({-# LINE 23 "src-ag/Patterns.ag" #-}
                              []
                              {-# LINE 1685 "src-ag/DefaultRules.hs" #-}
                              )) of
                       { !_copy ->
                       (case (({-# LINE 23 "src-ag/Patterns.ag" #-}
                               _copy
                               {-# LINE 1690 "src-ag/DefaultRules.hs" #-}
                               )) of
                        { !_lhsOcopy ->
                        (case (({-# LINE 465 "src-ag/DefaultRules.ag" #-}
                                Set.empty
                                {-# LINE 1695 "src-ag/DefaultRules.hs" #-}
                                )) of
                         { !_lhsOdefinedAttrs ->
                         (case (({-# LINE 118 "src-ag/DefaultRules.ag" #-}
                                 Seq.empty
                                 {-# LINE 1700 "src-ag/DefaultRules.hs" #-}
                                 )) of
                          { !_lhsOerrors ->
                          (case (({-# LINE 464 "src-ag/DefaultRules.ag" #-}
                                  Set.empty
                                  {-# LINE 1705 "src-ag/DefaultRules.hs" #-}
                                  )) of
                           { !_lhsOlocals ->
                           (case (({-# LINE 501 "src-ag/DefaultRules.ag" #-}
                                   []
                                   {-# LINE 1710 "src-ag/DefaultRules.hs" #-}
                                   )) of
                            { !_output ->
                            (case (({-# LINE 501 "src-ag/DefaultRules.ag" #-}
                                    _output
                                    {-# LINE 1715 "src-ag/DefaultRules.hs" #-}
                                    )) of
                             { !_lhsOoutput ->
                             ( _lhsOcontainsVars,_lhsOcopy,_lhsOdefinedAttrs,_lhsOerrors,_lhsOlocals,_lhsOoutput) }) }) }) }) }) }) }) })) )
-- Production --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         aroundsIn            : Map ConstructorIdent (Map Identifier [Expression])
         augmentsIn           : Map ConstructorIdent (Map Identifier [Expression])
         cr                   : Bool
         inh                  : Attributes
         manualAttrOrderMap   : AttrOrderMap
         mergesIn             : Map ConstructorIdent (Map Identifier (Identifier,[Identifier],Expression))
         nonterminals         : Set NontermIdent
         nt                   : NontermIdent
         o_rename             : Bool
         syn                  : Attributes
         typeSyns             : TypeSyns
         useMap               : Map Identifier (String,String,String)
      chained attribute:
         uniq                 : Int
      synthesized attributes:
         errors               : Seq Error
         output               : SELF 
   alternatives:
      alternative Production:
         child con            : {ConstructorIdent}
         child children       : Children 
         child rules          : Rules 
         child typeSigs       : TypeSigs 
         visit 0:
            local mergesIn    : _
            local merged      : _
            local orderDeps   : _
            local orderErrs   : _
            local _tup1       : _
            local errs        : _
            local aroundsIn   : _
            local augmentsIn  : _
            local newRls      : _
            local extra1      : _
            local extra2      : _
            local extra3      : _
-}
-- cata
sem_Production :: Production  ->
                  T_Production 
sem_Production !(Production _con _children _rules _typeSigs )  =
    (sem_Production_Production _con (sem_Children _children ) (sem_Rules _rules ) (sem_TypeSigs _typeSigs ) )
-- semantic domain
newtype T_Production  = T_Production ((Map ConstructorIdent (Map Identifier [Expression])) ->
                                      (Map ConstructorIdent (Map Identifier [Expression])) ->
                                      Bool ->
                                      Attributes ->
                                      AttrOrderMap ->
                                      (Map ConstructorIdent (Map Identifier (Identifier,[Identifier],Expression))) ->
                                      (Set NontermIdent) ->
                                      NontermIdent ->
                                      Bool ->
                                      Attributes ->
                                      TypeSyns ->
                                      Int ->
                                      (Map Identifier (String,String,String)) ->
                                      ( (Seq Error),Production ,Int))
data Inh_Production  = Inh_Production {aroundsIn_Inh_Production :: !((Map ConstructorIdent (Map Identifier [Expression]))),augmentsIn_Inh_Production :: !((Map ConstructorIdent (Map Identifier [Expression]))),cr_Inh_Production :: !(Bool),inh_Inh_Production :: !(Attributes),manualAttrOrderMap_Inh_Production :: !(AttrOrderMap),mergesIn_Inh_Production :: !((Map ConstructorIdent (Map Identifier (Identifier,[Identifier],Expression)))),nonterminals_Inh_Production :: !((Set NontermIdent)),nt_Inh_Production :: !(NontermIdent),o_rename_Inh_Production :: !(Bool),syn_Inh_Production :: !(Attributes),typeSyns_Inh_Production :: !(TypeSyns),uniq_Inh_Production :: !(Int),useMap_Inh_Production :: !((Map Identifier (String,String,String)))}
data Syn_Production  = Syn_Production {errors_Syn_Production :: !((Seq Error)),output_Syn_Production :: !(Production ),uniq_Syn_Production :: !(Int)}
wrap_Production :: T_Production  ->
                   Inh_Production  ->
                   Syn_Production 
wrap_Production !(T_Production sem ) !(Inh_Production _lhsIaroundsIn _lhsIaugmentsIn _lhsIcr _lhsIinh _lhsImanualAttrOrderMap _lhsImergesIn _lhsInonterminals _lhsInt _lhsIo_rename _lhsIsyn _lhsItypeSyns _lhsIuniq _lhsIuseMap )  =
    (let ( !_lhsOerrors,!_lhsOoutput,!_lhsOuniq) = sem _lhsIaroundsIn _lhsIaugmentsIn _lhsIcr _lhsIinh _lhsImanualAttrOrderMap _lhsImergesIn _lhsInonterminals _lhsInt _lhsIo_rename _lhsIsyn _lhsItypeSyns _lhsIuniq _lhsIuseMap 
     in  (Syn_Production _lhsOerrors _lhsOoutput _lhsOuniq ))
sem_Production_Production :: ConstructorIdent ->
                             T_Children  ->
                             T_Rules  ->
                             T_TypeSigs  ->
                             T_Production 
sem_Production_Production !con_ !(T_Children children_ ) !(T_Rules rules_ ) !(T_TypeSigs typeSigs_ )  =
    (T_Production (\ (!_lhsIaroundsIn)
                     (!_lhsIaugmentsIn)
                     (!_lhsIcr)
                     (!_lhsIinh)
                     (!_lhsImanualAttrOrderMap)
                     (!_lhsImergesIn)
                     (!_lhsInonterminals)
                     (!_lhsInt)
                     (!_lhsIo_rename)
                     (!_lhsIsyn)
                     (!_lhsItypeSyns)
                     (!_lhsIuniq)
                     (!_lhsIuseMap) ->
                       (case (({-# LINE 669 "src-ag/DefaultRules.ag" #-}
                               Map.findWithDefault Map.empty con_ _lhsImergesIn
                               {-# LINE 1809 "src-ag/DefaultRules.hs" #-}
                               )) of
                        { !_mergesIn ->
                        (case (({-# LINE 670 "src-ag/DefaultRules.ag" #-}
                                Set.fromList [ c | (_,cs,_) <- Map.elems _mergesIn    , c <- cs ]
                                {-# LINE 1814 "src-ag/DefaultRules.hs" #-}
                                )) of
                         { !_merged ->
                         (case (({-# LINE 665 "src-ag/DefaultRules.ag" #-}
                                 _merged
                                 {-# LINE 1819 "src-ag/DefaultRules.hs" #-}
                                 )) of
                          { !_childrenOmerged ->
                          (case (({-# LINE 596 "src-ag/DefaultRules.ag" #-}
                                  Set.toList $ Map.findWithDefault Set.empty con_ $ Map.findWithDefault Map.empty _lhsInt _lhsImanualAttrOrderMap
                                  {-# LINE 1824 "src-ag/DefaultRules.hs" #-}
                                  )) of
                           { !_orderDeps ->
                           (case (({-# LINE 494 "src-ag/DefaultRules.ag" #-}
                                   _lhsIuniq
                                   {-# LINE 1829 "src-ag/DefaultRules.hs" #-}
                                   )) of
                            { !_rulesOuniq ->
                            (case (({-# LINE 33 "src-ag/DefaultRules.ag" #-}
                                    _lhsInt
                                    {-# LINE 1834 "src-ag/DefaultRules.hs" #-}
                                    )) of
                             { !_rulesOnt ->
                             (case (({-# LINE 146 "src-ag/DefaultRules.ag" #-}
                                     con_
                                     {-# LINE 1839 "src-ag/DefaultRules.hs" #-}
                                     )) of
                              { !_rulesOcon ->
                              (case (rules_ _rulesOcon _rulesOnt _rulesOuniq ) of
                               { ( !_rulesIdefinedAttrs,!_rulesIerrors,!_rulesIlocals,!_rulesIoutput,!_rulesIruleNames,!_rulesIuniq) ->
                                   (case (({-# LINE 33 "src-ag/DefaultRules.ag" #-}
                                           _lhsInt
                                           {-# LINE 1846 "src-ag/DefaultRules.hs" #-}
                                           )) of
                                    { !_childrenOnt ->
                                    (case (({-# LINE 46 "src-ag/DefaultRules.ag" #-}
                                            _lhsIcr
                                            {-# LINE 1851 "src-ag/DefaultRules.hs" #-}
                                            )) of
                                     { !_childrenOcr ->
                                     (case (({-# LINE 147 "src-ag/DefaultRules.ag" #-}
                                             con_
                                             {-# LINE 1856 "src-ag/DefaultRules.hs" #-}
                                             )) of
                                      { !_childrenOcon ->
                                      (case (children_ _childrenOcon _childrenOcr _childrenOmerged _childrenOnt ) of
                                       { ( !_childrenIerrors,!_childrenIfields,!_childrenIinputs,!_childrenIoutput,!_childrenIoutputs) ->
                                           (case (({-# LINE 598 "src-ag/DefaultRules.ag" #-}
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
                                                   {-# LINE 1897 "src-ag/DefaultRules.hs" #-}
                                                   )) of
                                            { !_orderErrs ->
                                            (case (({-# LINE 335 "src-ag/DefaultRules.ag" #-}
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
                                                          = Map.partition isSELFNonterminal _lhsIsyn
                                                        (_,undefAttrs)
                                                          = removeDefined _rulesIdefinedAttrs (_LHS, normalAttrs)
                                                        (useAttrs,others)
                                                          = splitAttrs _lhsIuseMap undefAttrs
                                                        (rules1, errors1)
                                                          = concatRE $ map (copyRule _lhsInt con_ _lhsIcr locals)
                                                                           (zip envs (map (removeDefined _rulesIdefinedAttrs) _childrenIinputs))
                                                        uRules
                                                          = map (useRule locals _childrenIoutputs) useAttrs
                                                        selfLocRules
                                                          =  [ selfRule False attr (constructor [childSelf attr nm tp | (nm,tp,virt) <- _childrenIfields, childExists virt])
                                                             | attr <- Map.keys selfAttrs
                                                             , not (Set.member attr locals)
                                                             ]
                                                             where
                                                               childSelf self nm tp
                                                                 = case tp of NT nt _                         -> attrName nm self
                                                                              _      | nm `Set.member` locals -> locname nm
                                                                                     | otherwise              -> fieldName nm
                                                               constructor fs
                                                                | getName con_ == "Tuple" && _lhsInt `elem` map fst _lhsItypeSyns
                                                                  = "(" ++ concat (intersperse "," fs) ++ ")"
                                                                | otherwise
                                                                  = getConName _lhsItypeSyns _lhsIo_rename _lhsInt con_ ++ " " ++ unwords fs
                                                               childExists Nothing         = True
                                                               childExists (Just (Just _)) = True
                                                               childExists (Just Nothing)  = False
                                                        selfRules
                                                          = [ selfRule True attr undefined
                                                            | attr <- Map.keys selfAttrs
                                                            , not (Set.member (_LHS,attr) _rulesIdefinedAttrs)
                                                            ]
                                                        (rules5, errs5)
                                                          = copyRule _lhsInt
                                                                     con_
                                                                     _lhsIcr
                                                                     locals
                                                                     (lhs_env, (_LHS, others))
                                                    in (uRules++selfLocRules++selfRules++rules5++rules1, errors1><errs5)
                                                    {-# LINE 1957 "src-ag/DefaultRules.hs" #-}
                                                    )) of
                                             { !__tup1 ->
                                             (case (({-# LINE 335 "src-ag/DefaultRules.ag" #-}
                                                     __tup1
                                                     {-# LINE 1962 "src-ag/DefaultRules.hs" #-}
                                                     )) of
                                              { !(_,!_errs) ->
                                              (case (({-# LINE 333 "src-ag/DefaultRules.ag" #-}
                                                      _childrenIerrors >< _errs >< _rulesIerrors >< _orderErrs
                                                      {-# LINE 1967 "src-ag/DefaultRules.hs" #-}
                                                      )) of
                                               { !_lhsOerrors ->
                                               (case (({-# LINE 661 "src-ag/DefaultRules.ag" #-}
                                                       Map.findWithDefault Map.empty con_ _lhsIaroundsIn
                                                       {-# LINE 1972 "src-ag/DefaultRules.hs" #-}
                                                       )) of
                                                { !_aroundsIn ->
                                                (case (({-# LINE 654 "src-ag/DefaultRules.ag" #-}
                                                        Map.findWithDefault Map.empty con_ _lhsIaugmentsIn
                                                        {-# LINE 1977 "src-ag/DefaultRules.hs" #-}
                                                        )) of
                                                 { !_augmentsIn ->
                                                 (case (({-# LINE 335 "src-ag/DefaultRules.ag" #-}
                                                         __tup1
                                                         {-# LINE 1982 "src-ag/DefaultRules.hs" #-}
                                                         )) of
                                                  { !(!_newRls,_) ->
                                                  (case (({-# LINE 508 "src-ag/DefaultRules.ag" #-}
                                                          foldr addAugments (_rulesIoutput ++ _newRls) (Map.assocs _augmentsIn    )
                                                          {-# LINE 1987 "src-ag/DefaultRules.hs" #-}
                                                          )) of
                                                   { !_extra1 ->
                                                   (case (({-# LINE 509 "src-ag/DefaultRules.ag" #-}
                                                           foldr addArounds _extra1     (Map.assocs _aroundsIn    )
                                                           {-# LINE 1992 "src-ag/DefaultRules.hs" #-}
                                                           )) of
                                                    { !_extra2 ->
                                                    (case (({-# LINE 510 "src-ag/DefaultRules.ag" #-}
                                                            foldr addMerges _extra2     (Map.assocs _mergesIn    )
                                                            {-# LINE 1997 "src-ag/DefaultRules.hs" #-}
                                                            )) of
                                                     { !_extra3 ->
                                                     (case (typeSigs_ ) of
                                                      { ( !_typeSigsIoutput) ->
                                                          (case (({-# LINE 511 "src-ag/DefaultRules.ag" #-}
                                                                  Production con_ _childrenIoutput _extra3     _typeSigsIoutput
                                                                  {-# LINE 2004 "src-ag/DefaultRules.hs" #-}
                                                                  )) of
                                                           { !_lhsOoutput ->
                                                           (case (({-# LINE 494 "src-ag/DefaultRules.ag" #-}
                                                                   _rulesIuniq
                                                                   {-# LINE 2009 "src-ag/DefaultRules.hs" #-}
                                                                   )) of
                                                            { !_lhsOuniq ->
                                                            ( _lhsOerrors,_lhsOoutput,_lhsOuniq) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })) )
-- Productions -------------------------------------------------
{-
   visit 0:
      inherited attributes:
         aroundsIn            : Map ConstructorIdent (Map Identifier [Expression])
         augmentsIn           : Map ConstructorIdent (Map Identifier [Expression])
         cr                   : Bool
         inh                  : Attributes
         manualAttrOrderMap   : AttrOrderMap
         mergesIn             : Map ConstructorIdent (Map Identifier (Identifier,[Identifier],Expression))
         nonterminals         : Set NontermIdent
         nt                   : NontermIdent
         o_rename             : Bool
         syn                  : Attributes
         typeSyns             : TypeSyns
         useMap               : Map Identifier (String,String,String)
      chained attribute:
         uniq                 : Int
      synthesized attributes:
         errors               : Seq Error
         output               : SELF 
   alternatives:
      alternative Cons:
         child hd             : Production 
         child tl             : Productions 
         visit 0:
            local output      : _
      alternative Nil:
         visit 0:
            local output      : _
-}
-- cata
sem_Productions :: Productions  ->
                   T_Productions 
sem_Productions !list  =
    (Prelude.foldr sem_Productions_Cons sem_Productions_Nil (Prelude.map sem_Production list) )
-- semantic domain
newtype T_Productions  = T_Productions ((Map ConstructorIdent (Map Identifier [Expression])) ->
                                        (Map ConstructorIdent (Map Identifier [Expression])) ->
                                        Bool ->
                                        Attributes ->
                                        AttrOrderMap ->
                                        (Map ConstructorIdent (Map Identifier (Identifier,[Identifier],Expression))) ->
                                        (Set NontermIdent) ->
                                        NontermIdent ->
                                        Bool ->
                                        Attributes ->
                                        TypeSyns ->
                                        Int ->
                                        (Map Identifier (String,String,String)) ->
                                        ( (Seq Error),Productions ,Int))
data Inh_Productions  = Inh_Productions {aroundsIn_Inh_Productions :: !((Map ConstructorIdent (Map Identifier [Expression]))),augmentsIn_Inh_Productions :: !((Map ConstructorIdent (Map Identifier [Expression]))),cr_Inh_Productions :: !(Bool),inh_Inh_Productions :: !(Attributes),manualAttrOrderMap_Inh_Productions :: !(AttrOrderMap),mergesIn_Inh_Productions :: !((Map ConstructorIdent (Map Identifier (Identifier,[Identifier],Expression)))),nonterminals_Inh_Productions :: !((Set NontermIdent)),nt_Inh_Productions :: !(NontermIdent),o_rename_Inh_Productions :: !(Bool),syn_Inh_Productions :: !(Attributes),typeSyns_Inh_Productions :: !(TypeSyns),uniq_Inh_Productions :: !(Int),useMap_Inh_Productions :: !((Map Identifier (String,String,String)))}
data Syn_Productions  = Syn_Productions {errors_Syn_Productions :: !((Seq Error)),output_Syn_Productions :: !(Productions ),uniq_Syn_Productions :: !(Int)}
wrap_Productions :: T_Productions  ->
                    Inh_Productions  ->
                    Syn_Productions 
wrap_Productions !(T_Productions sem ) !(Inh_Productions _lhsIaroundsIn _lhsIaugmentsIn _lhsIcr _lhsIinh _lhsImanualAttrOrderMap _lhsImergesIn _lhsInonterminals _lhsInt _lhsIo_rename _lhsIsyn _lhsItypeSyns _lhsIuniq _lhsIuseMap )  =
    (let ( !_lhsOerrors,!_lhsOoutput,!_lhsOuniq) = sem _lhsIaroundsIn _lhsIaugmentsIn _lhsIcr _lhsIinh _lhsImanualAttrOrderMap _lhsImergesIn _lhsInonterminals _lhsInt _lhsIo_rename _lhsIsyn _lhsItypeSyns _lhsIuniq _lhsIuseMap 
     in  (Syn_Productions _lhsOerrors _lhsOoutput _lhsOuniq ))
sem_Productions_Cons :: T_Production  ->
                        T_Productions  ->
                        T_Productions 
sem_Productions_Cons !(T_Production hd_ ) !(T_Productions tl_ )  =
    (T_Productions (\ (!_lhsIaroundsIn)
                      (!_lhsIaugmentsIn)
                      (!_lhsIcr)
                      (!_lhsIinh)
                      (!_lhsImanualAttrOrderMap)
                      (!_lhsImergesIn)
                      (!_lhsInonterminals)
                      (!_lhsInt)
                      (!_lhsIo_rename)
                      (!_lhsIsyn)
                      (!_lhsItypeSyns)
                      (!_lhsIuniq)
                      (!_lhsIuseMap) ->
                        (case (({-# LINE 138 "src-ag/DefaultRules.ag" #-}
                                _lhsIuseMap
                                {-# LINE 2091 "src-ag/DefaultRules.hs" #-}
                                )) of
                         { !_tlOuseMap ->
                         (case (({-# LINE 54 "src-ag/DefaultRules.ag" #-}
                                 _lhsItypeSyns
                                 {-# LINE 2096 "src-ag/DefaultRules.hs" #-}
                                 )) of
                          { !_tlOtypeSyns ->
                          (case (({-# LINE 138 "src-ag/DefaultRules.ag" #-}
                                  _lhsIsyn
                                  {-# LINE 2101 "src-ag/DefaultRules.hs" #-}
                                  )) of
                           { !_tlOsyn ->
                           (case (({-# LINE 42 "src-ag/DefaultRules.ag" #-}
                                   _lhsIo_rename
                                   {-# LINE 2106 "src-ag/DefaultRules.hs" #-}
                                   )) of
                            { !_tlOo_rename ->
                            (case (({-# LINE 33 "src-ag/DefaultRules.ag" #-}
                                    _lhsInt
                                    {-# LINE 2111 "src-ag/DefaultRules.hs" #-}
                                    )) of
                             { !_tlOnt ->
                             (case (({-# LINE 664 "src-ag/DefaultRules.ag" #-}
                                     _lhsImergesIn
                                     {-# LINE 2116 "src-ag/DefaultRules.hs" #-}
                                     )) of
                              { !_tlOmergesIn ->
                              (case (({-# LINE 582 "src-ag/DefaultRules.ag" #-}
                                      _lhsImanualAttrOrderMap
                                      {-# LINE 2121 "src-ag/DefaultRules.hs" #-}
                                      )) of
                               { !_tlOmanualAttrOrderMap ->
                               (case (({-# LINE 138 "src-ag/DefaultRules.ag" #-}
                                       _lhsIinh
                                       {-# LINE 2126 "src-ag/DefaultRules.hs" #-}
                                       )) of
                                { !_tlOinh ->
                                (case (({-# LINE 43 "src-ag/DefaultRules.ag" #-}
                                        _lhsIcr
                                        {-# LINE 2131 "src-ag/DefaultRules.hs" #-}
                                        )) of
                                 { !_tlOcr ->
                                 (case (({-# LINE 138 "src-ag/DefaultRules.ag" #-}
                                         _lhsIuseMap
                                         {-# LINE 2136 "src-ag/DefaultRules.hs" #-}
                                         )) of
                                  { !_hdOuseMap ->
                                  (case (({-# LINE 54 "src-ag/DefaultRules.ag" #-}
                                          _lhsItypeSyns
                                          {-# LINE 2141 "src-ag/DefaultRules.hs" #-}
                                          )) of
                                   { !_hdOtypeSyns ->
                                   (case (({-# LINE 138 "src-ag/DefaultRules.ag" #-}
                                           _lhsIsyn
                                           {-# LINE 2146 "src-ag/DefaultRules.hs" #-}
                                           )) of
                                    { !_hdOsyn ->
                                    (case (({-# LINE 42 "src-ag/DefaultRules.ag" #-}
                                            _lhsIo_rename
                                            {-# LINE 2151 "src-ag/DefaultRules.hs" #-}
                                            )) of
                                     { !_hdOo_rename ->
                                     (case (({-# LINE 33 "src-ag/DefaultRules.ag" #-}
                                             _lhsInt
                                             {-# LINE 2156 "src-ag/DefaultRules.hs" #-}
                                             )) of
                                      { !_hdOnt ->
                                      (case (({-# LINE 664 "src-ag/DefaultRules.ag" #-}
                                              _lhsImergesIn
                                              {-# LINE 2161 "src-ag/DefaultRules.hs" #-}
                                              )) of
                                       { !_hdOmergesIn ->
                                       (case (({-# LINE 582 "src-ag/DefaultRules.ag" #-}
                                               _lhsImanualAttrOrderMap
                                               {-# LINE 2166 "src-ag/DefaultRules.hs" #-}
                                               )) of
                                        { !_hdOmanualAttrOrderMap ->
                                        (case (({-# LINE 138 "src-ag/DefaultRules.ag" #-}
                                                _lhsIinh
                                                {-# LINE 2171 "src-ag/DefaultRules.hs" #-}
                                                )) of
                                         { !_hdOinh ->
                                         (case (({-# LINE 43 "src-ag/DefaultRules.ag" #-}
                                                 _lhsIcr
                                                 {-# LINE 2176 "src-ag/DefaultRules.hs" #-}
                                                 )) of
                                          { !_hdOcr ->
                                          (case (({-# LINE 494 "src-ag/DefaultRules.ag" #-}
                                                  _lhsIuniq
                                                  {-# LINE 2181 "src-ag/DefaultRules.hs" #-}
                                                  )) of
                                           { !_hdOuniq ->
                                           (case (({-# LINE 128 "src-ag/DefaultRules.ag" #-}
                                                   _lhsInonterminals
                                                   {-# LINE 2186 "src-ag/DefaultRules.hs" #-}
                                                   )) of
                                            { !_hdOnonterminals ->
                                            (case (({-# LINE 650 "src-ag/DefaultRules.ag" #-}
                                                    _lhsIaugmentsIn
                                                    {-# LINE 2191 "src-ag/DefaultRules.hs" #-}
                                                    )) of
                                             { !_hdOaugmentsIn ->
                                             (case (({-# LINE 657 "src-ag/DefaultRules.ag" #-}
                                                     _lhsIaroundsIn
                                                     {-# LINE 2196 "src-ag/DefaultRules.hs" #-}
                                                     )) of
                                              { !_hdOaroundsIn ->
                                              (case (hd_ _hdOaroundsIn _hdOaugmentsIn _hdOcr _hdOinh _hdOmanualAttrOrderMap _hdOmergesIn _hdOnonterminals _hdOnt _hdOo_rename _hdOsyn _hdOtypeSyns _hdOuniq _hdOuseMap ) of
                                               { ( !_hdIerrors,!_hdIoutput,!_hdIuniq) ->
                                                   (case (({-# LINE 494 "src-ag/DefaultRules.ag" #-}
                                                           _hdIuniq
                                                           {-# LINE 2203 "src-ag/DefaultRules.hs" #-}
                                                           )) of
                                                    { !_tlOuniq ->
                                                    (case (({-# LINE 128 "src-ag/DefaultRules.ag" #-}
                                                            _lhsInonterminals
                                                            {-# LINE 2208 "src-ag/DefaultRules.hs" #-}
                                                            )) of
                                                     { !_tlOnonterminals ->
                                                     (case (({-# LINE 650 "src-ag/DefaultRules.ag" #-}
                                                             _lhsIaugmentsIn
                                                             {-# LINE 2213 "src-ag/DefaultRules.hs" #-}
                                                             )) of
                                                      { !_tlOaugmentsIn ->
                                                      (case (({-# LINE 657 "src-ag/DefaultRules.ag" #-}
                                                              _lhsIaroundsIn
                                                              {-# LINE 2218 "src-ag/DefaultRules.hs" #-}
                                                              )) of
                                                       { !_tlOaroundsIn ->
                                                       (case (tl_ _tlOaroundsIn _tlOaugmentsIn _tlOcr _tlOinh _tlOmanualAttrOrderMap _tlOmergesIn _tlOnonterminals _tlOnt _tlOo_rename _tlOsyn _tlOtypeSyns _tlOuniq _tlOuseMap ) of
                                                        { ( !_tlIerrors,!_tlIoutput,!_tlIuniq) ->
                                                            (case (({-# LINE 118 "src-ag/DefaultRules.ag" #-}
                                                                    _hdIerrors Seq.>< _tlIerrors
                                                                    {-# LINE 2225 "src-ag/DefaultRules.hs" #-}
                                                                    )) of
                                                             { !_lhsOerrors ->
                                                             (case (({-# LINE 501 "src-ag/DefaultRules.ag" #-}
                                                                     (:) _hdIoutput _tlIoutput
                                                                     {-# LINE 2230 "src-ag/DefaultRules.hs" #-}
                                                                     )) of
                                                              { !_output ->
                                                              (case (({-# LINE 501 "src-ag/DefaultRules.ag" #-}
                                                                      _output
                                                                      {-# LINE 2235 "src-ag/DefaultRules.hs" #-}
                                                                      )) of
                                                               { !_lhsOoutput ->
                                                               (case (({-# LINE 494 "src-ag/DefaultRules.ag" #-}
                                                                       _tlIuniq
                                                                       {-# LINE 2240 "src-ag/DefaultRules.hs" #-}
                                                                       )) of
                                                                { !_lhsOuniq ->
                                                                ( _lhsOerrors,_lhsOoutput,_lhsOuniq) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })) )
sem_Productions_Nil :: T_Productions 
sem_Productions_Nil  =
    (T_Productions (\ (!_lhsIaroundsIn)
                      (!_lhsIaugmentsIn)
                      (!_lhsIcr)
                      (!_lhsIinh)
                      (!_lhsImanualAttrOrderMap)
                      (!_lhsImergesIn)
                      (!_lhsInonterminals)
                      (!_lhsInt)
                      (!_lhsIo_rename)
                      (!_lhsIsyn)
                      (!_lhsItypeSyns)
                      (!_lhsIuniq)
                      (!_lhsIuseMap) ->
                        (case (({-# LINE 118 "src-ag/DefaultRules.ag" #-}
                                Seq.empty
                                {-# LINE 2261 "src-ag/DefaultRules.hs" #-}
                                )) of
                         { !_lhsOerrors ->
                         (case (({-# LINE 501 "src-ag/DefaultRules.ag" #-}
                                 []
                                 {-# LINE 2266 "src-ag/DefaultRules.hs" #-}
                                 )) of
                          { !_output ->
                          (case (({-# LINE 501 "src-ag/DefaultRules.ag" #-}
                                  _output
                                  {-# LINE 2271 "src-ag/DefaultRules.hs" #-}
                                  )) of
                           { !_lhsOoutput ->
                           (case (({-# LINE 494 "src-ag/DefaultRules.ag" #-}
                                   _lhsIuniq
                                   {-# LINE 2276 "src-ag/DefaultRules.hs" #-}
                                   )) of
                            { !_lhsOuniq ->
                            ( _lhsOerrors,_lhsOoutput,_lhsOuniq) }) }) }) })) )
-- Rule --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         con                  : ConstructorIdent
         nt                   : NontermIdent
      chained attribute:
         uniq                 : Int
      synthesized attributes:
         containsVars         : Bool
         definedAttrs         : Set (Identifier,Identifier)
         errors               : Seq Error
         locals               : Set Identifier
         output               : SELF 
         outputs              : Rules 
         ruleNames            : Set Identifier
   alternatives:
      alternative Rule:
         child mbName         : {Maybe Identifier}
         child pattern        : Pattern 
         child rhs            : {Expression}
         child owrt           : {Bool}
         child origin         : {String}
         child explicit       : {Bool}
         visit 0:
            local output      : _
            local _tup2       : {(Rules,Int)}
-}
-- cata
sem_Rule :: Rule  ->
            T_Rule 
sem_Rule !(Rule _mbName _pattern _rhs _owrt _origin _explicit )  =
    (sem_Rule_Rule _mbName (sem_Pattern _pattern ) _rhs _owrt _origin _explicit )
-- semantic domain
newtype T_Rule  = T_Rule (ConstructorIdent ->
                          NontermIdent ->
                          Int ->
                          ( Bool,(Set (Identifier,Identifier)),(Seq Error),(Set Identifier),Rule ,Rules ,(Set Identifier),Int))
data Inh_Rule  = Inh_Rule {con_Inh_Rule :: !(ConstructorIdent),nt_Inh_Rule :: !(NontermIdent),uniq_Inh_Rule :: !(Int)}
data Syn_Rule  = Syn_Rule {containsVars_Syn_Rule :: !(Bool),definedAttrs_Syn_Rule :: !((Set (Identifier,Identifier))),errors_Syn_Rule :: !((Seq Error)),locals_Syn_Rule :: !((Set Identifier)),output_Syn_Rule :: !(Rule ),outputs_Syn_Rule :: !(Rules ),ruleNames_Syn_Rule :: !((Set Identifier)),uniq_Syn_Rule :: !(Int)}
wrap_Rule :: T_Rule  ->
             Inh_Rule  ->
             Syn_Rule 
wrap_Rule !(T_Rule sem ) !(Inh_Rule _lhsIcon _lhsInt _lhsIuniq )  =
    (let ( !_lhsOcontainsVars,!_lhsOdefinedAttrs,!_lhsOerrors,!_lhsOlocals,!_lhsOoutput,!_lhsOoutputs,!_lhsOruleNames,!_lhsOuniq) = sem _lhsIcon _lhsInt _lhsIuniq 
     in  (Syn_Rule _lhsOcontainsVars _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput _lhsOoutputs _lhsOruleNames _lhsOuniq ))
sem_Rule_Rule :: (Maybe Identifier) ->
                 T_Pattern  ->
                 Expression ->
                 Bool ->
                 String ->
                 Bool ->
                 T_Rule 
sem_Rule_Rule !mbName_ !(T_Pattern pattern_ ) !rhs_ !owrt_ !origin_ !explicit_  =
    (T_Rule (\ (!_lhsIcon)
               (!_lhsInt)
               (!_lhsIuniq) ->
                 (case (({-# LINE 33 "src-ag/DefaultRules.ag" #-}
                         _lhsInt
                         {-# LINE 2339 "src-ag/DefaultRules.hs" #-}
                         )) of
                  { !_patternOnt ->
                  (case (({-# LINE 34 "src-ag/DefaultRules.ag" #-}
                          _lhsIcon
                          {-# LINE 2344 "src-ag/DefaultRules.hs" #-}
                          )) of
                   { !_patternOcon ->
                   (case (pattern_ _patternOcon _patternOnt ) of
                    { ( !_patternIcontainsVars,!_patternIcopy,!_patternIdefinedAttrs,!_patternIerrors,!_patternIlocals,!_patternIoutput) ->
                        (case (({-# LINE 485 "src-ag/DefaultRules.ag" #-}
                                _patternIcontainsVars
                                {-# LINE 2351 "src-ag/DefaultRules.hs" #-}
                                )) of
                         { !_lhsOcontainsVars ->
                         (case (({-# LINE 465 "src-ag/DefaultRules.ag" #-}
                                 _patternIdefinedAttrs
                                 {-# LINE 2356 "src-ag/DefaultRules.hs" #-}
                                 )) of
                          { !_lhsOdefinedAttrs ->
                          (case (({-# LINE 118 "src-ag/DefaultRules.ag" #-}
                                  _patternIerrors
                                  {-# LINE 2361 "src-ag/DefaultRules.hs" #-}
                                  )) of
                           { !_lhsOerrors ->
                           (case (({-# LINE 464 "src-ag/DefaultRules.ag" #-}
                                   _patternIlocals
                                   {-# LINE 2366 "src-ag/DefaultRules.hs" #-}
                                   )) of
                            { !_lhsOlocals ->
                            (case (({-# LINE 501 "src-ag/DefaultRules.ag" #-}
                                    Rule mbName_ _patternIoutput rhs_ owrt_ origin_ explicit_
                                    {-# LINE 2371 "src-ag/DefaultRules.hs" #-}
                                    )) of
                             { !_output ->
                             (case (({-# LINE 501 "src-ag/DefaultRules.ag" #-}
                                     _output
                                     {-# LINE 2376 "src-ag/DefaultRules.hs" #-}
                                     )) of
                              { !_lhsOoutput ->
                              (case (({-# LINE 518 "src-ag/DefaultRules.ag" #-}
                                      multiRule _output     _lhsIuniq
                                      {-# LINE 2381 "src-ag/DefaultRules.hs" #-}
                                      )) of
                               { !__tup2 ->
                               (case (({-# LINE 518 "src-ag/DefaultRules.ag" #-}
                                       __tup2
                                       {-# LINE 2386 "src-ag/DefaultRules.hs" #-}
                                       )) of
                                { !(!_lhsOoutputs,_) ->
                                (case (({-# LINE 590 "src-ag/DefaultRules.ag" #-}
                                        case mbName_ of
                                          Nothing -> Set.empty
                                          Just nm -> Set.singleton nm
                                        {-# LINE 2393 "src-ag/DefaultRules.hs" #-}
                                        )) of
                                 { !_lhsOruleNames ->
                                 (case (({-# LINE 518 "src-ag/DefaultRules.ag" #-}
                                         __tup2
                                         {-# LINE 2398 "src-ag/DefaultRules.hs" #-}
                                         )) of
                                  { !(_,!_lhsOuniq) ->
                                  ( _lhsOcontainsVars,_lhsOdefinedAttrs,_lhsOerrors,_lhsOlocals,_lhsOoutput,_lhsOoutputs,_lhsOruleNames,_lhsOuniq) }) }) }) }) }) }) }) }) }) }) }) }) })) )
-- Rules -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         con                  : ConstructorIdent
         nt                   : NontermIdent
      chained attribute:
         uniq                 : Int
      synthesized attributes:
         definedAttrs         : Set (Identifier,Identifier)
         errors               : Seq Error
         locals               : Set Identifier
         output               : SELF 
         ruleNames            : Set Identifier
   alternatives:
      alternative Cons:
         child hd             : Rule 
         child tl             : Rules 
      alternative Nil:
         visit 0:
            local output      : _
-}
-- cata
sem_Rules :: Rules  ->
             T_Rules 
sem_Rules !list  =
    (Prelude.foldr sem_Rules_Cons sem_Rules_Nil (Prelude.map sem_Rule list) )
-- semantic domain
newtype T_Rules  = T_Rules (ConstructorIdent ->
                            NontermIdent ->
                            Int ->
                            ( (Set (Identifier,Identifier)),(Seq Error),(Set Identifier),Rules ,(Set Identifier),Int))
data Inh_Rules  = Inh_Rules {con_Inh_Rules :: !(ConstructorIdent),nt_Inh_Rules :: !(NontermIdent),uniq_Inh_Rules :: !(Int)}
data Syn_Rules  = Syn_Rules {definedAttrs_Syn_Rules :: !((Set (Identifier,Identifier))),errors_Syn_Rules :: !((Seq Error)),locals_Syn_Rules :: !((Set Identifier)),output_Syn_Rules :: !(Rules ),ruleNames_Syn_Rules :: !((Set Identifier)),uniq_Syn_Rules :: !(Int)}
wrap_Rules :: T_Rules  ->
              Inh_Rules  ->
              Syn_Rules 
wrap_Rules !(T_Rules sem ) !(Inh_Rules _lhsIcon _lhsInt _lhsIuniq )  =
    (let ( !_lhsOdefinedAttrs,!_lhsOerrors,!_lhsOlocals,!_lhsOoutput,!_lhsOruleNames,!_lhsOuniq) = sem _lhsIcon _lhsInt _lhsIuniq 
     in  (Syn_Rules _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput _lhsOruleNames _lhsOuniq ))
sem_Rules_Cons :: T_Rule  ->
                  T_Rules  ->
                  T_Rules 
sem_Rules_Cons !(T_Rule hd_ ) !(T_Rules tl_ )  =
    (T_Rules (\ (!_lhsIcon)
                (!_lhsInt)
                (!_lhsIuniq) ->
                  (case (({-# LINE 494 "src-ag/DefaultRules.ag" #-}
                          _lhsIuniq
                          {-# LINE 2451 "src-ag/DefaultRules.hs" #-}
                          )) of
                   { !_hdOuniq ->
                   (case (({-# LINE 33 "src-ag/DefaultRules.ag" #-}
                           _lhsInt
                           {-# LINE 2456 "src-ag/DefaultRules.hs" #-}
                           )) of
                    { !_hdOnt ->
                    (case (({-# LINE 34 "src-ag/DefaultRules.ag" #-}
                            _lhsIcon
                            {-# LINE 2461 "src-ag/DefaultRules.hs" #-}
                            )) of
                     { !_hdOcon ->
                     (case (hd_ _hdOcon _hdOnt _hdOuniq ) of
                      { ( !_hdIcontainsVars,!_hdIdefinedAttrs,!_hdIerrors,!_hdIlocals,!_hdIoutput,!_hdIoutputs,!_hdIruleNames,!_hdIuniq) ->
                          (case (({-# LINE 494 "src-ag/DefaultRules.ag" #-}
                                  _hdIuniq
                                  {-# LINE 2468 "src-ag/DefaultRules.hs" #-}
                                  )) of
                           { !_tlOuniq ->
                           (case (({-# LINE 33 "src-ag/DefaultRules.ag" #-}
                                   _lhsInt
                                   {-# LINE 2473 "src-ag/DefaultRules.hs" #-}
                                   )) of
                            { !_tlOnt ->
                            (case (({-# LINE 34 "src-ag/DefaultRules.ag" #-}
                                    _lhsIcon
                                    {-# LINE 2478 "src-ag/DefaultRules.hs" #-}
                                    )) of
                             { !_tlOcon ->
                             (case (tl_ _tlOcon _tlOnt _tlOuniq ) of
                              { ( !_tlIdefinedAttrs,!_tlIerrors,!_tlIlocals,!_tlIoutput,!_tlIruleNames,!_tlIuniq) ->
                                  (case (({-# LINE 465 "src-ag/DefaultRules.ag" #-}
                                          _hdIdefinedAttrs `Set.union` _tlIdefinedAttrs
                                          {-# LINE 2485 "src-ag/DefaultRules.hs" #-}
                                          )) of
                                   { !_lhsOdefinedAttrs ->
                                   (case (({-# LINE 118 "src-ag/DefaultRules.ag" #-}
                                           _hdIerrors Seq.>< _tlIerrors
                                           {-# LINE 2490 "src-ag/DefaultRules.hs" #-}
                                           )) of
                                    { !_lhsOerrors ->
                                    (case (({-# LINE 464 "src-ag/DefaultRules.ag" #-}
                                            _hdIlocals `Set.union` _tlIlocals
                                            {-# LINE 2495 "src-ag/DefaultRules.hs" #-}
                                            )) of
                                     { !_lhsOlocals ->
                                     (case (({-# LINE 514 "src-ag/DefaultRules.ag" #-}
                                             if _hdIcontainsVars then _hdIoutputs ++ _tlIoutput else _tlIoutput
                                             {-# LINE 2500 "src-ag/DefaultRules.hs" #-}
                                             )) of
                                      { !_lhsOoutput ->
                                      (case (({-# LINE 588 "src-ag/DefaultRules.ag" #-}
                                              _hdIruleNames `Set.union` _tlIruleNames
                                              {-# LINE 2505 "src-ag/DefaultRules.hs" #-}
                                              )) of
                                       { !_lhsOruleNames ->
                                       (case (({-# LINE 494 "src-ag/DefaultRules.ag" #-}
                                               _tlIuniq
                                               {-# LINE 2510 "src-ag/DefaultRules.hs" #-}
                                               )) of
                                        { !_lhsOuniq ->
                                        ( _lhsOdefinedAttrs,_lhsOerrors,_lhsOlocals,_lhsOoutput,_lhsOruleNames,_lhsOuniq) }) }) }) }) }) }) }) }) }) }) }) }) }) })) )
sem_Rules_Nil :: T_Rules 
sem_Rules_Nil  =
    (T_Rules (\ (!_lhsIcon)
                (!_lhsInt)
                (!_lhsIuniq) ->
                  (case (({-# LINE 465 "src-ag/DefaultRules.ag" #-}
                          Set.empty
                          {-# LINE 2521 "src-ag/DefaultRules.hs" #-}
                          )) of
                   { !_lhsOdefinedAttrs ->
                   (case (({-# LINE 118 "src-ag/DefaultRules.ag" #-}
                           Seq.empty
                           {-# LINE 2526 "src-ag/DefaultRules.hs" #-}
                           )) of
                    { !_lhsOerrors ->
                    (case (({-# LINE 464 "src-ag/DefaultRules.ag" #-}
                            Set.empty
                            {-# LINE 2531 "src-ag/DefaultRules.hs" #-}
                            )) of
                     { !_lhsOlocals ->
                     (case (({-# LINE 501 "src-ag/DefaultRules.ag" #-}
                             []
                             {-# LINE 2536 "src-ag/DefaultRules.hs" #-}
                             )) of
                      { !_output ->
                      (case (({-# LINE 501 "src-ag/DefaultRules.ag" #-}
                              _output
                              {-# LINE 2541 "src-ag/DefaultRules.hs" #-}
                              )) of
                       { !_lhsOoutput ->
                       (case (({-# LINE 588 "src-ag/DefaultRules.ag" #-}
                               Set.empty
                               {-# LINE 2546 "src-ag/DefaultRules.hs" #-}
                               )) of
                        { !_lhsOruleNames ->
                        (case (({-# LINE 494 "src-ag/DefaultRules.ag" #-}
                                _lhsIuniq
                                {-# LINE 2551 "src-ag/DefaultRules.hs" #-}
                                )) of
                         { !_lhsOuniq ->
                         ( _lhsOdefinedAttrs,_lhsOerrors,_lhsOlocals,_lhsOoutput,_lhsOruleNames,_lhsOuniq) }) }) }) }) }) }) })) )
-- TypeSig -----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         output               : SELF 
   alternatives:
      alternative TypeSig:
         child name           : {Identifier}
         child tp             : {Type}
         visit 0:
            local output      : _
-}
-- cata
sem_TypeSig :: TypeSig  ->
               T_TypeSig 
sem_TypeSig !(TypeSig _name _tp )  =
    (sem_TypeSig_TypeSig _name _tp )
-- semantic domain
newtype T_TypeSig  = T_TypeSig (( TypeSig ))
data Inh_TypeSig  = Inh_TypeSig {}
data Syn_TypeSig  = Syn_TypeSig {output_Syn_TypeSig :: !(TypeSig )}
wrap_TypeSig :: T_TypeSig  ->
                Inh_TypeSig  ->
                Syn_TypeSig 
wrap_TypeSig !(T_TypeSig sem ) !(Inh_TypeSig )  =
    (let ( !_lhsOoutput) = sem 
     in  (Syn_TypeSig _lhsOoutput ))
sem_TypeSig_TypeSig :: Identifier ->
                       Type ->
                       T_TypeSig 
sem_TypeSig_TypeSig !name_ !tp_  =
    (T_TypeSig (case (({-# LINE 501 "src-ag/DefaultRules.ag" #-}
                       TypeSig name_ tp_
                       {-# LINE 2588 "src-ag/DefaultRules.hs" #-}
                       )) of
                { !_output ->
                (case (({-# LINE 501 "src-ag/DefaultRules.ag" #-}
                        _output
                        {-# LINE 2593 "src-ag/DefaultRules.hs" #-}
                        )) of
                 { !_lhsOoutput ->
                 ( _lhsOoutput) }) }) )
-- TypeSigs ----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         output               : SELF 
   alternatives:
      alternative Cons:
         child hd             : TypeSig 
         child tl             : TypeSigs 
         visit 0:
            local output      : _
      alternative Nil:
         visit 0:
            local output      : _
-}
-- cata
sem_TypeSigs :: TypeSigs  ->
                T_TypeSigs 
sem_TypeSigs !list  =
    (Prelude.foldr sem_TypeSigs_Cons sem_TypeSigs_Nil (Prelude.map sem_TypeSig list) )
-- semantic domain
newtype T_TypeSigs  = T_TypeSigs (( TypeSigs ))
data Inh_TypeSigs  = Inh_TypeSigs {}
data Syn_TypeSigs  = Syn_TypeSigs {output_Syn_TypeSigs :: !(TypeSigs )}
wrap_TypeSigs :: T_TypeSigs  ->
                 Inh_TypeSigs  ->
                 Syn_TypeSigs 
wrap_TypeSigs !(T_TypeSigs sem ) !(Inh_TypeSigs )  =
    (let ( !_lhsOoutput) = sem 
     in  (Syn_TypeSigs _lhsOoutput ))
sem_TypeSigs_Cons :: T_TypeSig  ->
                     T_TypeSigs  ->
                     T_TypeSigs 
sem_TypeSigs_Cons !(T_TypeSig hd_ ) !(T_TypeSigs tl_ )  =
    (T_TypeSigs (case (tl_ ) of
                 { ( !_tlIoutput) ->
                     (case (hd_ ) of
                      { ( !_hdIoutput) ->
                          (case (({-# LINE 501 "src-ag/DefaultRules.ag" #-}
                                  (:) _hdIoutput _tlIoutput
                                  {-# LINE 2637 "src-ag/DefaultRules.hs" #-}
                                  )) of
                           { !_output ->
                           (case (({-# LINE 501 "src-ag/DefaultRules.ag" #-}
                                   _output
                                   {-# LINE 2642 "src-ag/DefaultRules.hs" #-}
                                   )) of
                            { !_lhsOoutput ->
                            ( _lhsOoutput) }) }) }) }) )
sem_TypeSigs_Nil :: T_TypeSigs 
sem_TypeSigs_Nil  =
    (T_TypeSigs (case (({-# LINE 501 "src-ag/DefaultRules.ag" #-}
                        []
                        {-# LINE 2650 "src-ag/DefaultRules.hs" #-}
                        )) of
                 { !_output ->
                 (case (({-# LINE 501 "src-ag/DefaultRules.ag" #-}
                         _output
                         {-# LINE 2655 "src-ag/DefaultRules.hs" #-}
                         )) of
                  { !_lhsOoutput ->
                  ( _lhsOoutput) }) }) )