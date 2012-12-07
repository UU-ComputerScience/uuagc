{-# LANGUAGE Rank2Types, GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Transform where
{-# LINE 8 "./src-ag/Transform.ag" #-}

import Control.Monad(mplus,mzero)
import Data.List (partition, nub,intersperse, union)
import Data.Maybe
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Set as Set (Set, member, union, toList, fromList, empty, singleton, member, unions, size, fold, intersection, difference, insert, elems)
import qualified Data.Sequence as Seq
import Data.Sequence(Seq, (><))
import UU.Scanner.Position(noPos)

import ConcreteSyntax
import AbstractSyntax
import ErrorMessages
import Patterns (Patterns,Pattern(..))
import Expression (Expression(..))
import HsToken

import RhsCheck
import Debug.Trace
{-# LINE 27 "dist/build/Transform.hs" #-}

{-# LINE 2 "./src-ag/ConcreteSyntax.ag" #-}

import UU.Scanner.Position (Pos)
import Patterns   (Pattern)
import Expression (Expression)
import CommonTypes
import Macro --marcos
{-# LINE 36 "dist/build/Transform.hs" #-}

{-# LINE 2 "./src-ag/Patterns.ag" #-}

-- Patterns.ag imports
import UU.Scanner.Position(Pos)
import CommonTypes (ConstructorIdent,Identifier)
{-# LINE 43 "dist/build/Transform.hs" #-}
import Control.Monad.Identity (Identity)
import qualified Control.Monad.Identity
{-# LINE 104 "./src-ag/Transform.ag" #-}
type DefinedSets = Map Identifier (Set NontermIdent) 
{-# LINE 48 "dist/build/Transform.hs" #-}

{-# LINE 124 "./src-ag/Transform.ag" #-}
type FieldMap  = [(Identifier, Type)] 
{-# LINE 52 "dist/build/Transform.hs" #-}

{-# LINE 125 "./src-ag/Transform.ag" #-}
type DataTypes = Map.Map NontermIdent (Map.Map ConstructorIdent FieldMap) 
{-# LINE 56 "dist/build/Transform.hs" #-}

{-# LINE 148 "./src-ag/Transform.ag" #-}
type AttrName   = (Identifier,Identifier) 
{-# LINE 60 "dist/build/Transform.hs" #-}

{-# LINE 149 "./src-ag/Transform.ag" #-}
type RuleInfo   = (Maybe Identifier, [AttrName]->Pattern, Expression, [AttrName], Bool, String, Bool, Bool) 
{-# LINE 64 "dist/build/Transform.hs" #-}

{-# LINE 150 "./src-ag/Transform.ag" #-}
type SigInfo    = (Identifier,Type) 
{-# LINE 68 "dist/build/Transform.hs" #-}

{-# LINE 151 "./src-ag/Transform.ag" #-}
type UniqueInfo = (Identifier,Identifier) 
{-# LINE 72 "dist/build/Transform.hs" #-}

{-# LINE 152 "./src-ag/Transform.ag" #-}
type AugmentInfo = (Identifier,Expression)
{-# LINE 76 "dist/build/Transform.hs" #-}

{-# LINE 153 "./src-ag/Transform.ag" #-}
type AroundInfo  = (Identifier,Expression)
{-# LINE 80 "dist/build/Transform.hs" #-}

{-# LINE 154 "./src-ag/Transform.ag" #-}
type MergeInfo   = (Identifier, Identifier, [Identifier], Expression)
{-# LINE 84 "dist/build/Transform.hs" #-}

{-# LINE 203 "./src-ag/Transform.ag" #-}


checkDuplicate :: (Identifier -> Identifier -> Error)
               -> Identifier -> val -> Map Identifier val -> (Map Identifier val,Seq Error)
checkDuplicate dupError key val m
  = case Map.lookupIndex key m of
     Just ix -> let (key',_) = Map.elemAt ix m
                in  (m,Seq.singleton (dupError key key'))
     Nothing -> (Map.insert key val m,Seq.empty)

checkDuplicates :: (Identifier -> Identifier -> Error)
                -> [(Identifier, val)] -> Map Identifier val -> (Map Identifier val,Seq Error)
checkDuplicates dupError new m = foldErrors check m new
 where  check = uncurry (checkDuplicate dupError)

foldErrors :: (b -> t -> (t, Seq Error)) -> t -> [b] -> (t, Seq Error)
foldErrors f n xs = foldl g (n,Seq.empty) xs
  where g ~(e,es) x = let (e',es') = f x e
                      in (e', es >< es')


checkForDuplicates :: (Identifier -> Identifier -> Error)  ->  [Identifier]  ->  [Error]
checkForDuplicates _ [] = []
checkForDuplicates err (x:xs) = let (same,other) = partition (equalId x) xs
                                in  map (err x) same ++ checkForDuplicates err other

equalId :: Identifier -> Identifier -> Bool
equalId x y = getName x == getName y

{-# LINE 116 "dist/build/Transform.hs" #-}

{-# LINE 354 "./src-ag/Transform.ag" #-}

type RulesAndErrors = ([Rule], Seq Error)
type SigsAndErrors  = ([TypeSig], Seq Error)
type InstsAndErrors = ([(Identifier, Type)], Seq Error)
type UniquesAndErrors = (Map Identifier Identifier, Seq Error)
type AugmentsAndErrors = (Map Identifier [Expression], Seq Error)
type AroundsAndErrors = (Map Identifier [Expression], Seq Error)
type MergesAndErrors  = (Map Identifier (Identifier, [Identifier], Expression), Seq Error)
type AttrOverwrite  = Map AttrName Bool
type AccumRuleCheck = (RulesAndErrors, AttrOverwrite)
type AccumDefiCheck = (Seq Error, AttrOverwrite, [AttrName], [AttrName])

checkRules :: Map NontermIdent (Attributes, Attributes) -> DataTypes ->
              Map NontermIdent (Map ConstructorIdent [Identifier]) -> Map NontermIdent (Map ConstructorIdent [SigInfo]) ->
              Map NontermIdent (Map ConstructorIdent [MergeInfo]) ->
              NontermIdent -> ConstructorIdent -> [RuleInfo] -> RulesAndErrors
checkRules attributes fields allinsts allsigs _ nt con rs
  = let fieldmap :: FieldMap
        fieldmap = (_LHS, NT nt [] False) : (_LOC, NT nullIdent [] False) : (_INST, NT nullIdent [] False) : (_FIRST, NT nullIdent [] False) : (_LAST, NT nullIdent [] False)
                 : Map.findWithDefault [] con (Map.findWithDefault Map.empty nt fields)
                 ++ mapMaybe (\instNm -> lookup instNm sigs >>= \tp -> return (instNm, tp)) (Map.findWithDefault [] con (Map.findWithDefault Map.empty nt allinsts))
                 --   merged children are not allowed to have any inherited attrs defined: do not include

        sigs = Map.findWithDefault [] con (Map.findWithDefault Map.empty nt allsigs)

        hasAttrib f tp attr  = Map.member attr (f (Map.findWithDefault (Map.empty,Map.empty) tp attributes))

        checkRule :: RuleInfo -> AccumRuleCheck -> AccumRuleCheck
        checkRule (mbNm, pat,ex,as,owrt,str, pur, eager) ((r1,e1),m1)
          = let (e2,m2,u2,_) = foldr (checkDefi owrt) (e1,m1,[],[]) as
            in  ( (Rule mbNm (pat u2) ex owrt str True pur False Nothing eager : r1, e2), m2)

        checkDefi :: Bool -> AttrName -> AccumDefiCheck -> AccumDefiCheck
        checkDefi owrt fa@(field,attr) (e,m,u,bs)
         = case lookup field fieldmap
            of  Just (NT tp _ _) ->
                  let tp' = maybe tp id (deforestedNt tp)
                  in              if field == _LOC || field == _INST || field == _FIRST || field == _LAST
                                     || hasAttrib (if getName field==getName _LHS then snd else fst) tp' attr
                                  then case Map.lookupIndex fa m of
                                           Just ix -> let ((_,attr2),b) = Map.elemAt ix m
                                                       in  if b && not (fa `elem` bs)
                                                           then (                                             e, Map.insert fa owrt m, fa:u, fa:bs)
                                                           else (((Seq.<|)) (DupRule nt con field attr2 attr)   e,                    m, fa:u,    bs)
                                           Nothing ->           (                                             e, Map.insert fa owrt m,    u, fa:bs)
                                  else                          (((Seq.<|)) (SuperfluousRule nt con field attr) e,                    m, fa:u,    bs)
                _              ->                               (((Seq.<|)) (UndefChild nt con field)           e,                    m, fa:u,    bs )

    in  fst (foldr checkRule (([],Seq.empty),Map.empty) rs)

checkRuleNames :: NontermIdent -> ConstructorIdent -> [RuleInfo] -> Seq Error
checkRuleNames nt con
  = fst . foldr checkRule (Seq.empty, Set.empty)
  where
    checkRule (Just nm,_,_,_,_,_,_,_) (errs, nms)
      | nm `Set.member` nms = (DupRuleName nt con nm Seq.<| errs, nms)
      | otherwise           = (errs, Set.insert nm nms)
    checkRule (Nothing,_,_,_,_,_,_,_) inp = inp

checkSigs :: NontermIdent -> ConstructorIdent -> [SigInfo] -> SigsAndErrors
checkSigs nt con sis
  = let checkSig (ide,typ) (sigs,errs)
         = if   ide `elem` map (\(TypeSig n _)-> n) sigs
           then (sigs, ((Seq.<|)) (DupSig nt con ide) errs)
           -- else if not (ide `elem` locattrdefs)
           -- then (sigs, ((Seq.<|)) (SupSig nt con ide) errs)
           else (TypeSig ide typ:sigs, errs)
    in  foldr checkSig ([],Seq.empty) sis

checkInsts :: Set NontermIdent -> Map NontermIdent (Map ConstructorIdent [SigInfo]) -> DataTypes -> NontermIdent -> ConstructorIdent -> [Identifier] -> InstsAndErrors
checkInsts allNts sigMap _ nt con
  = foldr (\inst (insts, errs) ->
              maybe (insts, Seq.singleton (MissingInstSig nt con inst) >< errs)
                    (\info@(k, NT nm args _) ->
                      case findInst k insts of
                        Just k' -> (insts, Seq.singleton (DupChild nt con k k') >< errs)
                        Nothing -> case nm `Set.member` allNts of
                                             True  -> (info : insts, errs)
                                             False | take 2 (getName nm) == "T_" -> let nm'   = Ident (drop 2 (getName nm)) (getPos nm)
                                                                                        info' = (k, NT nm' args True)   -- this should be the only place at which 'for' with value True can be generated
                                                                                    in case nm' `Set.member` allNts of
                                                                                         True  -> (info' : insts, errs)
                                                                                         False -> (insts, Seq.singleton (UndefNont nm') >< errs)
                                                   | otherwise                   -> (insts, Seq.singleton (UndefNont nm) >< errs)
                    )
                  $ findSig inst
          ) ([], Seq.empty)
  where
    sigs = Map.findWithDefault [] con (Map.findWithDefault Map.empty nt sigMap)

    findSig name
      = do tp@(NT _ _ _) <- lookup name sigs
           return (name, tp)

    findInst _ [] = Nothing
    findInst k ((k', _): r)
      | k == k'   = Just k'
      | otherwise = findInst k r

checkUniques :: Map NontermIdent (Attributes, Attributes) -> NontermIdent -> ConstructorIdent -> [UniqueInfo] -> UniquesAndErrors
checkUniques allAttrs nt con uniques
  = let checkUnique (ident,ref) (us,errs)
          = if ident `Map.member` us
            then (us, ((Seq.<|)) (DupUnique nt con ident) errs)
            else if Map.member ref inhs && Map.member ref syns
                 then (Map.insert ident ref us, errs)
                 else (us, ((Seq.<|)) (MissingUnique nt ref) errs)

        (inhs,syns) = Map.findWithDefault (Map.empty,Map.empty) nt allAttrs
    in foldr checkUnique (Map.empty, Seq.empty) uniques

checkAugments :: Map NontermIdent (Attributes, Attributes) -> NontermIdent -> ConstructorIdent -> [AugmentInfo] -> AugmentsAndErrors
checkAugments allAttrs nt _ augments
  = let checkAugment (ident,expr) (as,errs)
          = if ident `Map.member` as
            then (Map.update (\vs -> Just (vs ++ [expr])) ident as, errs)
            else if Map.member ident syns
                 then (Map.insert ident [expr] as, errs)
                 else (as, ((Seq.<|)) (MissingSyn nt ident) errs)

        (_,syns) = Map.findWithDefault (Map.empty,Map.empty) nt allAttrs
    in foldr checkAugment (Map.empty, Seq.empty) augments

checkArounds :: DataTypes -> NontermIdent -> ConstructorIdent -> [AroundInfo] -> AroundsAndErrors
checkArounds fieldMap nt con arounds
  = let checkAround (ident,expr) (as,errs)
          = if ident `Map.member` as
            then (Map.update (\vs -> Just (vs ++ [expr])) ident as, errs)
            else case lookup ident fields of
                   Just (NT _ _ _) -> (Map.insert ident [expr] as, errs)
                   _               -> (as, ((Seq.<|)) (UndefChild nt con ident) errs)
        fields = Map.findWithDefault [] con (Map.findWithDefault Map.empty nt fieldMap)
    in foldr checkAround (Map.empty, Seq.empty) arounds

checkMerges :: Set NontermIdent -> Map NontermIdent (Map ConstructorIdent [Identifier]) -> DataTypes -> NontermIdent -> ConstructorIdent -> [MergeInfo] -> MergesAndErrors
checkMerges allNts allInsts fieldMap _ con merges
  = let checkMerge (target,nt,sources,expr) (m,errs)
          = let fields = Map.findWithDefault [] con (Map.findWithDefault Map.empty nt fieldMap)
                insts  = Map.findWithDefault [] con (Map.findWithDefault Map.empty nt allInsts)
                allFields = insts ++ map fst fields   -- note: sources of merge may not contain a target (for simplicity)
            in if target `Map.member` m   -- check for duplicate with self
               then (m, DupChild nt con target (fst $ Map.elemAt (Map.findIndex target m) m) Seq.<| errs)
               else if target `elem` allFields
                     then (m, DupChild nt con target (head $ filter (== target) allFields) Seq.<| errs)
                     else let missing = filter (\s -> not (s `elem` allFields)) sources
                          in if null missing
                             then if nt `Set.member` allNts   -- check if the nonterm is defined
                                  then (Map.insert target (nt, sources, expr) m, errs) -- all ok..
                                  else (m, UndefNont nt Seq.<| errs)
                             else (m, (Seq.fromList $ map (UndefChild nt con) missing) Seq.>< errs)
    in foldr checkMerge (Map.empty, Seq.empty) merges

unionunionplusplus :: Map NontermIdent (Map ConstructorIdent [a]) -> Map NontermIdent (Map ConstructorIdent [a]) -> Map NontermIdent (Map ConstructorIdent [a])
unionunionplusplus = Map.unionWith (Map.unionWith (++))
{-# LINE 273 "dist/build/Transform.hs" #-}

{-# LINE 511 "./src-ag/Transform.ag" #-}

mkUniqueRules :: Options -> Map NontermIdent (Map ConstructorIdent [RuleInfo]) -> DataTypes -> Map NontermIdent (Map ConstructorIdent [(Identifier, Type)]) -> Map NontermIdent (Attributes,Attributes) -> NontermIdent -> ConstructorIdent -> Map Identifier Identifier -> [Rule]
mkUniqueRules opts allRules allFields allInsts allAttrDecls nt con usMap
  = map apply groups
  where
    fields = Map.findWithDefault [] con (Map.findWithDefault Map.empty nt allFields)
             ++ Map.findWithDefault [] con (Map.findWithDefault Map.empty nt allInsts)
             -- may have duplicates

    attrDefs = let projectDefs (_,_,_,defs,_,_,_,_) = defs
               in concatMap projectDefs $ Map.findWithDefault [] con $ Map.findWithDefault Map.empty nt allRules

    groups = Map.assocs $ Map.foldrWithKey (\i r m -> Map.insertWith (++) r [i] m) Map.empty usMap
    apply (ref,us) = mkRule ref (findOutField ref) us
    findOutField ref = case [ chld | (chld, NT tp _ _) <- fields, tp `hasSyn` ref] of
                         []    -> _LHS
                         (x:_) -> x
    hasSyn tp ref = Map.member ref $ snd $ Map.findWithDefault (Map.empty,Map.empty) tp allAttrDecls
    mkRule ref outFld locAttrs
      = let locs = filter (not . existsLoc) locAttrs
            outAttr = attr outFld ref
            defs = (if hasOut then [] else [outAttr]) ++ [attr _LOC u | u <- locs ]
            pat = Product noPos defs
            rhs = Expression noPos $ wrap ref $ foldr gencase (finalout hasOut locs) locs
                     -- [HsToken ("mkUniques" ++ show (length locAttrs) ++ " ") noPos, AGField _LHS ref noPos Nothing]
            rul = Rule Nothing pat rhs False "-- generated by the unique rule mechanism." False True False Nothing False
            hasOut = exists outAttr
            exists (Alias fld a _) = (fld,a) `elem` attrDefs
	    exists _ = False
            existsLoc nm = exists (attr _LOC nm)
        in rul
    attr fld a = Alias fld a (Underscore noPos)
    gencase nm outp
      = h ("case " ++ uniqueDispenser opts ++ " __cont of { (__cont, " ++ getName nm ++ ") -> ") ++ outp ++ h "}"
    h s = [HsToken s noPos]
    finalout noGenCont us = h ("(" ++ concat (intersperse "," ( (if noGenCont then [] else ["__cont"]) ++ map getName us)) ++ ")")
    wrap ref inp = h "let __cont = " ++ [AGField _LHS ref noPos Nothing] ++ h " in seq __cont ( " ++ inp ++ h " )"
{-# LINE 313 "dist/build/Transform.hs" #-}

{-# LINE 747 "./src-ag/Transform.ag" #-}

flattenDatas :: DataTypes -> Map NontermIdent (Set NontermIdent)
flattenDatas ds = Map.map flatten ds
  where flatten cs =  Set.fromList [ nt | (_, NT nt _ _) <- concatMap snd (Map.toList cs)]

reachableFrom :: Map NontermIdent (Set NontermIdent) -> Set NontermIdent -> Set NontermIdent
reachableFrom table = reach
  where reach nts = let nts' = Set.unions (nts : [ ns  | nt <- Set.toList nts
                                                 , let ns = Map.findWithDefault Set.empty nt table ])
                    in if Set.size nts' > Set.size nts
                          then reach nts'
                          else nts
invert :: Map NontermIdent (Set NontermIdent) -> Map NontermIdent (Set NontermIdent)
invert = foldr inv Map.empty . Map.toList
  where inv (x,ns) m = fold (\n m' -> Map.insertWith Set.union n (Set.singleton x) m') m ns

path :: Map NontermIdent (Set NontermIdent) -> NontermIdent -> NontermIdent -> Set NontermIdent
path table from to = let children = Map.findWithDefault Set.empty from table
                         forward  = reachableFrom table children
                         backward = reachableFrom (invert table)
                                                  (Set.singleton to)
                     in  Set.intersection forward backward
{-# LINE 338 "dist/build/Transform.hs" #-}

{-# LINE 873 "./src-ag/Transform.ag" #-}

extract :: String -> [String]
extract s = case dropWhile isSeparator s of
                                "" -> []
                                s' -> w : extract s''
                                      where (w, s'') = break isSeparator  s'
isSeparator :: Char -> Bool
isSeparator x = x == '_'
{-# LINE 349 "dist/build/Transform.hs" #-}

{-# LINE 899 "./src-ag/Transform.ag" #-}

pragmaMapUnion :: PragmaMap -> PragmaMap -> PragmaMap
pragmaMapUnion = Map.unionWith (Map.unionWith Set.union)

pragmaMapSingle :: NontermIdent -> ConstructorIdent -> Set Identifier -> PragmaMap
pragmaMapSingle nt con nms = Map.singleton nt (Map.singleton con nms)
{-# LINE 358 "dist/build/Transform.hs" #-}

{-# LINE 931 "./src-ag/Transform.ag" #-}

orderMapUnion :: AttrOrderMap -> AttrOrderMap -> AttrOrderMap
orderMapUnion = Map.unionWith (Map.unionWith Set.union)

orderMapSingle :: NontermIdent -> ConstructorIdent -> Set Dependency -> AttrOrderMap
orderMapSingle nt con deps = Map.singleton nt (Map.singleton con deps)
{-# LINE 367 "dist/build/Transform.hs" #-}

{-# LINE 957 "./src-ag/Transform.ag" #-}

mergeParams :: ParamMap -> ParamMap -> ParamMap
mergeParams = Map.unionWith (++)
{-# LINE 373 "dist/build/Transform.hs" #-}

{-# LINE 980 "./src-ag/Transform.ag" #-}

mergeCtx :: ContextMap -> ContextMap -> ContextMap
mergeCtx
  = Map.unionWith nubconcat
  where nubconcat a b = nub (a ++ b)
{-# LINE 381 "dist/build/Transform.hs" #-}

{-# LINE 999 "./src-ag/Transform.ag" #-}

mergeQuant :: QuantMap -> QuantMap -> QuantMap
mergeQuant = Map.unionWith (++)
{-# LINE 387 "dist/build/Transform.hs" #-}

{-# LINE 1010 "./src-ag/Transform.ag" #-}

mergeDerivings :: Derivings -> Derivings -> Derivings
mergeDerivings m1 m2 = foldr (\(n,cs) m -> Map.insertWith Set.union n cs m) m2 (Map.toList m1)
{-# LINE 393 "dist/build/Transform.hs" #-}

{-# LINE 1022 "./src-ag/Transform.ag" #-}

merge ::(Ord k, Ord k1) => Map k (Map k1 a) -> Map k (Map k1 a) -> Map k (Map k1 a)
merge x y = foldr f y (Map.toList x)
 where f ~(k,v) m = Map.insertWith (Map.union) k v m
{-# LINE 400 "dist/build/Transform.hs" #-}

{-# LINE 1065 "./src-ag/Transform.ag" #-}

checkAttrs :: DataTypes -> [NontermIdent] -> [(Identifier, a)] -> [(Identifier, b)] -> Map NontermIdent (Map Identifier a, Map Identifier b) -> (Map NontermIdent (Map Identifier a, Map Identifier b), Seq Error)
checkAttrs allFields nts inherited synthesized decls' = foldErrors check decls' nts where
  check nt decls | not (nt `Map.member` allFields) = (decls,Seq.singleton(UndefNont nt))
                 | otherwise = let (inh,syn) = Map.findWithDefault (Map.empty,Map.empty) nt decls
                                   (inh',einh) = checkDuplicates (DupInhAttr nt) inherited   inh
                                   (syn',esyn) = checkDuplicates (DupSynAttr nt) synthesized syn
                               in (Map.insert nt (inh',syn') decls,einh >< esyn)
{-# LINE 411 "dist/build/Transform.hs" #-}

{-# LINE 1077 "./src-ag/Transform.ag" #-}

addSelf :: Ord k1 => k1 -> Map k1 (Map k a, Attributes) -> Map k1 (Map k a, Attributes)
addSelf name atMap = let (eInh,eSyn) = Map.findWithDefault(Map.empty,Map.empty) name atMap
                     in  Map.insert name (eInh, Map.insert (Ident "self" noPos) Self eSyn)atMap
{-# LINE 418 "dist/build/Transform.hs" #-}

{-# LINE 1219 "./src-ag/Transform.ag" #-}

makeType :: Set NontermIdent -> Type -> Type
makeType nts tp@(NT x _ _)   | Set.member x nts = tp
                             | otherwise        = Haskell (typeToHaskellString Nothing [] tp)
makeType _   tp                                 = tp
{-# LINE 426 "dist/build/Transform.hs" #-}

{-# LINE 1225 "./src-ag/Transform.ag" #-}

constructGrammar ::    Set NontermIdent
                    -> ParamMap
                    -> Map NontermIdent (Map ConstructorIdent (Set Identifier))
                    -> DataTypes
		    -> Map NontermIdent [ConstructorIdent]
                    -> Map NontermIdent (Map ConstructorIdent [Type])
                    -> Map NontermIdent (Attributes, Attributes)
                    -> Map NontermIdent (Map Identifier (String, String, String))
                    -> Derivings
                    -> Set NontermIdent
                    -> Map NontermIdent (Map ConstructorIdent [Rule])
                    -> Map NontermIdent (Map ConstructorIdent [TypeSig])
                    -> Map NontermIdent (Map ConstructorIdent [(Identifier, Type)])
                    -> TypeSyns
                    -> PragmaMap
                    -> AttrOrderMap
                    -> ContextMap
                    -> QuantMap
                    -> UniqueMap
                    -> Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))
                    -> Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))
                    -> Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier, [Identifier], Expression)))
                    -> Map NontermIdent (Map ConstructorIdent MaybeMacro)
                    -> Grammar

constructGrammar _ ntParams prodParams gram prodOrder constraints attrs uses derivings wrap allrules tsigs allinsts tsyns pragmaMap orderMap contextMap quantMap uniqueMap augmentsMap aroundsMap mergeMap macros =
   let gr = [ (nt,alts) | (nt,alts) <- Map.toList gram]
       nonts = map nont gr
       nont (nt,alts) =  let (inh,syn) = Map.findWithDefault (Map.empty,Map.empty) nt attrs
                             rmap      = Map.findWithDefault Map.empty             nt allrules
                             tsmap     = Map.findWithDefault Map.empty             nt tsigs
                             instsmap  = Map.findWithDefault Map.empty             nt allinsts
                             params    = Map.findWithDefault []                    nt ntParams
                             mergemap  = Map.findWithDefault Map.empty             nt mergeMap
                             macromap  = Map.findWithDefault Map.empty             nt macros
                             csmap     = Map.findWithDefault Map.empty             nt constraints
                             psmap     = Map.findWithDefault Map.empty             nt prodParams
			     prs       = Map.findWithDefault []                    nt prodOrder
                             alt con   =
                                   let flds    = Map.findWithDefault [] con alts
                                       rules   = Map.findWithDefault [] con rmap
                                       tsigs'  = Map.findWithDefault [] con tsmap
                                       insts   = Map.findWithDefault [] con instsmap
                                       merges  = [ (n, NT t [] False) | (n, (t, _, _)) <- Map.assocs $ maybe Map.empty id (Map.lookup con mergemap) ]
                                       cs      = Map.findWithDefault [] con csmap
                                       ps      = Set.elems $ Map.findWithDefault Set.empty con psmap
                                       mbMacro = Map.findWithDefault Nothing con macromap

                                       -- important: keep order of children
                                       cldrn = map child (flds ++ filter (not . existsAsField) insts ++ merges)
                                       child (nm, tp) =
                                          let tpI = if existsAsInst nm
                                                    then fromJust $ lookup nm insts
                                                    else tp
                                              virt = if existsAsInst nm
                                                     then case lookup nm flds of
                                                            Just tp' -> ChildReplace tp'
                                                            Nothing  -> ChildAttr
                                                     else if existsAsMerge nm
                                                          then ChildAttr
                                                          else ChildSyntax
                                          in Child nm tpI virt
                                       existsAsInst nm = maybe False (const True) (lookup nm insts)
                                       existsAsField (nm,_) = maybe False (const True) (lookup nm flds)
                                       existsAsMerge nm = maybe False (const True) (lookup nm merges)
                                   in Production con ps cs cldrn rules tsigs' mbMacro
                            in Nonterminal nt params inh syn (map alt prs)
   in Grammar tsyns uses derivings wrap nonts pragmaMap orderMap ntParams contextMap quantMap uniqueMap augmentsMap aroundsMap mergeMap
{-# LINE 498 "dist/build/Transform.hs" #-}

{-# LINE 1296 "./src-ag/Transform.ag" #-}

mapUnionWithSetUnion :: Map NontermIdent (Set ConstructorIdent) -> Map NontermIdent (Set ConstructorIdent) -> Map NontermIdent (Set ConstructorIdent)
mapUnionWithSetUnion = Map.unionWith Set.union
mapUnionWithPlusPlus :: Map BlockInfo [a] -> Map BlockInfo [a] -> Map BlockInfo [a]
mapUnionWithPlusPlus = Map.unionWith (++)
{-# LINE 506 "dist/build/Transform.hs" #-}
-- AG ----------------------------------------------------------
-- wrapper
data Inh_AG  = Inh_AG { options_Inh_AG :: (Options) }
data Syn_AG  = Syn_AG { agi_Syn_AG :: ((Set NontermIdent, DataTypes, Map NontermIdent (Attributes, Attributes))), blocks_Syn_AG :: (Blocks), errors_Syn_AG :: (Seq Error), moduleDecl_Syn_AG :: (Maybe (String,String,String)), output_Syn_AG :: (Grammar), pragmas_Syn_AG :: (Options -> Options) }
{-# INLINABLE wrap_AG #-}
wrap_AG :: T_AG  -> Inh_AG  -> (Syn_AG )
wrap_AG (T_AG act) (Inh_AG _lhsIoptions) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_AG_vIn1 _lhsIoptions
        (T_AG_vOut1 _lhsOagi _lhsOblocks _lhsOerrors _lhsOmoduleDecl _lhsOoutput _lhsOpragmas) <- return (inv_AG_s2 sem arg)
        return (Syn_AG _lhsOagi _lhsOblocks _lhsOerrors _lhsOmoduleDecl _lhsOoutput _lhsOpragmas)
   )

-- cata
{-# INLINE sem_AG #-}
sem_AG :: AG  -> T_AG 
sem_AG ( AG elems_ ) = sem_AG_AG ( sem_Elems elems_ )

-- semantic domain
newtype T_AG  = T_AG {
                     attach_T_AG :: Identity (T_AG_s2 )
                     }
newtype T_AG_s2  = C_AG_s2 {
                           inv_AG_s2 :: (T_AG_v1 )
                           }
data T_AG_s3  = C_AG_s3
type T_AG_v1  = (T_AG_vIn1 ) -> (T_AG_vOut1 )
data T_AG_vIn1  = T_AG_vIn1 (Options)
data T_AG_vOut1  = T_AG_vOut1 ((Set NontermIdent, DataTypes, Map NontermIdent (Attributes, Attributes))) (Blocks) (Seq Error) (Maybe (String,String,String)) (Grammar) (Options -> Options)
{-# NOINLINE sem_AG_AG #-}
sem_AG_AG :: T_Elems  -> T_AG 
sem_AG_AG arg_elems_ = T_AG (return st2) where
   {-# NOINLINE st2 #-}
   st2 = let
      v1 :: T_AG_v1 
      v1 = \ (T_AG_vIn1 _lhsIoptions) -> ( let
         _elemsX20 = Control.Monad.Identity.runIdentity (attach_T_Elems (arg_elems_))
         (T_Elems_vOut19 _elemsIattrDecls _elemsIattrOrderCollect _elemsIattrs _elemsIblocks _elemsIcollectedArounds _elemsIcollectedAugments _elemsIcollectedConParams _elemsIcollectedConstraints _elemsIcollectedConstructorsMap _elemsIcollectedFields _elemsIcollectedInsts _elemsIcollectedMacros _elemsIcollectedMerges _elemsIcollectedNames _elemsIcollectedRules _elemsIcollectedSetNames _elemsIcollectedSigs _elemsIcollectedUniques _elemsIctxCollect _elemsIdefSets _elemsIderivings _elemsIerrors _elemsImoduleDecl _elemsIparamsCollect _elemsIpragmas _elemsIquantCollect _elemsIsemPragmasCollect _elemsItypeSyns _elemsIuseMap _elemsIwrappers) = inv_Elems_s20 _elemsX20 (T_Elems_vIn19 _elemsOallAttrDecls _elemsOallAttrs _elemsOallConstructors _elemsOallFields _elemsOallNonterminals _elemsOattrDecls _elemsOattrs _elemsOdefSets _elemsOdefinedSets _elemsOoptions)
         _lhsOoutput :: Grammar
         _lhsOoutput = rule0 _allAttrDecls _allConParams _allConstraints _allFields _allMacros _allNonterminals _checkedArounds _checkedAugments _checkedInsts _checkedMerges _checkedRules _checkedSigs _checkedUniques _elemsIattrOrderCollect _elemsIctxCollect _elemsIderivings _elemsIparamsCollect _elemsIquantCollect _elemsIsemPragmasCollect _elemsItypeSyns _elemsIuseMap _elemsIwrappers _lhsIoptions _prodOrder
         _prodOrder = rule1 _elemsIcollectedFields
         _allFields = rule2 _elemsIcollectedFields
         _allConstraints = rule3 _elemsIcollectedConstraints
         _allConParams = rule4 _elemsIcollectedConParams
         _allConstrs = rule5 _elemsIcollectedFields
         _allRules = rule6 _elemsIcollectedRules
         _allSigs = rule7 _allAttrDecls _elemsIcollectedSigs _elemsIcollectedUniques
         _allInsts = rule8 _elemsIcollectedInsts
         _allUniques = rule9 _elemsIcollectedUniques
         _allAugments = rule10 _elemsIcollectedAugments
         _allArounds = rule11 _elemsIcollectedArounds
         _allMerges = rule12 _elemsIcollectedMerges
         _augmentSigs = rule13 _allAugments
         _allRulesErrs = rule14 _allAttrDecls _allFields _allInsts _allMerges _allRules _allSigs
         _allNamesErrs = rule15 _allRules
         _allSigsErrs = rule16 _allSigs
         _allInstsErrs = rule17 _allFields _allInsts _allNonterminals _allSigs
         _allUniquesErrs = rule18 _allAttrDecls _allUniques
         _allAugmentErrs = rule19 _allAttrDecls _allAugments
         _allAroundsErrs = rule20 _allArounds _allFields
         _allMergesErrs = rule21 _allFields _allInsts _allMerges _allNonterminals
         _checkedRulesPre = rule22 _allRulesErrs
         _checkedSigs = rule23 _allSigsErrs _augmentSigs
         _checkedInsts = rule24 _allInstsErrs
         _checkedUniques = rule25 _allUniquesErrs
         _checkedAugments = rule26 _allAugmentErrs
         _checkedArounds = rule27 _allAroundsErrs
         _checkedRules = rule28 _allAttrDecls _allFields _allRules _checkedInsts _checkedRulesPre _checkedUniques _lhsIoptions
         _checkedMerges = rule29 _allMergesErrs
         _errs1 = rule30 _elemsItypeSyns
         _errs2 = rule31 _allFields
         _errs3 = rule32  ()
         _errs4 = rule33 _allRulesErrs
         _errs5 = rule34 _allSigsErrs
         _errs6 = rule35 _allInstsErrs
         _errs7 = rule36 _allUniquesErrs
         _errs8 = rule37 _allAugmentErrs
         _errs9 = rule38 _allAroundsErrs
         _errs10 = rule39 _allNamesErrs
         _errs11 = rule40 _allMergesErrs
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule41 _elemsIerrors _errs1 _errs10 _errs11 _errs2 _errs3 _errs4 _errs5 _errs6 _errs7 _errs8 _errs9
         _allNonterminals = rule42 _elemsIcollectedNames _elemsIcollectedSetNames
         _elemsOallConstructors = rule43 _elemsIcollectedConstructorsMap
         _elemsOdefSets = rule44 _allNonterminals
         _elemsOdefinedSets = rule45 _elemsIdefSets
         _elemsOattrDecls = rule46  ()
         _allAttrDecls = rule47 _allNonterminals _elemsIattrDecls _lhsIoptions
         _allMacros = rule48 _elemsIcollectedMacros
         _lhsOagi :: (Set NontermIdent, DataTypes, Map NontermIdent (Attributes, Attributes))
         _lhsOagi = rule49 _allAttrs _allFields _allNonterminals
         _allAttrs = rule50 _allNonterminals _elemsIattrs _lhsIoptions
         _elemsOattrs = rule51  ()
         _lhsOblocks :: Blocks
         _lhsOblocks = rule52 _elemsIblocks
         _lhsOmoduleDecl :: Maybe (String,String,String)
         _lhsOmoduleDecl = rule53 _elemsImoduleDecl
         _lhsOpragmas :: Options -> Options
         _lhsOpragmas = rule54 _elemsIpragmas
         _elemsOallAttrDecls = rule55 _allAttrDecls
         _elemsOallAttrs = rule56 _allAttrs
         _elemsOallFields = rule57 _allFields
         _elemsOallNonterminals = rule58 _allNonterminals
         _elemsOoptions = rule59 _lhsIoptions
         __result_ = T_AG_vOut1 _lhsOagi _lhsOblocks _lhsOerrors _lhsOmoduleDecl _lhsOoutput _lhsOpragmas
         in __result_ )
     in C_AG_s2 v1
   {-# INLINE rule0 #-}
   {-# LINE 50 "./src-ag/Transform.ag" #-}
   rule0 = \ _allAttrDecls _allConParams _allConstraints _allFields _allMacros _allNonterminals _checkedArounds _checkedAugments _checkedInsts _checkedMerges _checkedRules _checkedSigs _checkedUniques ((_elemsIattrOrderCollect) :: AttrOrderMap) ((_elemsIctxCollect) :: ContextMap) ((_elemsIderivings) :: Derivings) ((_elemsIparamsCollect) :: ParamMap) ((_elemsIquantCollect) :: QuantMap) ((_elemsIsemPragmasCollect) :: PragmaMap) ((_elemsItypeSyns) :: TypeSyns) ((_elemsIuseMap) :: Map NontermIdent (Map Identifier (String,String,String))) ((_elemsIwrappers) :: Set NontermIdent) ((_lhsIoptions) :: Options) _prodOrder ->
                      {-# LINE 50 "./src-ag/Transform.ag" #-}
                      constructGrammar _allNonterminals
                                       _elemsIparamsCollect
                                       _allConParams
                                       _allFields
                                       _prodOrder
                                       _allConstraints
                                       _allAttrDecls
                                       _elemsIuseMap
                                       _elemsIderivings
                                       (if wrappers _lhsIoptions then _allNonterminals     else _elemsIwrappers)
                                       _checkedRules
                                       _checkedSigs
                                       _checkedInsts
                                       _elemsItypeSyns
                                       _elemsIsemPragmasCollect
                                       _elemsIattrOrderCollect
                                       _elemsIctxCollect
                                       _elemsIquantCollect
                                       _checkedUniques
                                       _checkedAugments
                                       _checkedArounds
                                       _checkedMerges
                                       _allMacros
                      {-# LINE 642 "dist/build/Transform.hs"#-}
   {-# INLINE rule1 #-}
   {-# LINE 258 "./src-ag/Transform.ag" #-}
   rule1 = \ ((_elemsIcollectedFields) :: [(NontermIdent, ConstructorIdent, FieldMap)]) ->
                             {-# LINE 258 "./src-ag/Transform.ag" #-}
                             let f (nt,con,_) = Map.insertWith g nt [con]
                                 g [con] lst | con `elem` lst = lst
                                             | otherwise      = con : lst
                                 g _ _ = error "This is not possible"
                             in  foldr f Map.empty _elemsIcollectedFields
                             {-# LINE 652 "dist/build/Transform.hs"#-}
   {-# INLINE rule2 #-}
   {-# LINE 263 "./src-ag/Transform.ag" #-}
   rule2 = \ ((_elemsIcollectedFields) :: [(NontermIdent, ConstructorIdent, FieldMap)]) ->
                             {-# LINE 263 "./src-ag/Transform.ag" #-}
                             let f (nt,con,fm) = Map.insertWith (Map.unionWith (++)) nt (Map.singleton con fm)
                             in  foldr f (Map.empty) _elemsIcollectedFields
                             {-# LINE 659 "dist/build/Transform.hs"#-}
   {-# INLINE rule3 #-}
   {-# LINE 266 "./src-ag/Transform.ag" #-}
   rule3 = \ ((_elemsIcollectedConstraints) :: [(NontermIdent, ConstructorIdent, [Type])]) ->
                                {-# LINE 266 "./src-ag/Transform.ag" #-}
                                let f (nt,con,fm) = Map.insertWith (Map.unionWith (++)) nt (Map.singleton con fm)
                                in  foldr f (Map.empty) _elemsIcollectedConstraints
                                {-# LINE 666 "dist/build/Transform.hs"#-}
   {-# INLINE rule4 #-}
   {-# LINE 269 "./src-ag/Transform.ag" #-}
   rule4 = \ ((_elemsIcollectedConParams) :: [(NontermIdent, ConstructorIdent, Set Identifier)]) ->
                                {-# LINE 269 "./src-ag/Transform.ag" #-}
                                let f (nt,con,fm) = Map.insertWith (Map.unionWith Set.union) nt (Map.singleton con fm)
                                in  foldr f (Map.empty) _elemsIcollectedConParams
                                {-# LINE 673 "dist/build/Transform.hs"#-}
   {-# INLINE rule5 #-}
   {-# LINE 272 "./src-ag/Transform.ag" #-}
   rule5 = \ ((_elemsIcollectedFields) :: [(NontermIdent, ConstructorIdent, FieldMap)]) ->
                             {-# LINE 272 "./src-ag/Transform.ag" #-}
                             let f (nt,con,_) = Map.insertWith (++) nt [con]
                             in  foldr f (Map.empty) _elemsIcollectedFields
                             {-# LINE 680 "dist/build/Transform.hs"#-}
   {-# INLINE rule6 #-}
   {-# LINE 275 "./src-ag/Transform.ag" #-}
   rule6 = \ ((_elemsIcollectedRules) :: [ (NontermIdent, ConstructorIdent, RuleInfo)]) ->
                             {-# LINE 275 "./src-ag/Transform.ag" #-}
                             let f (nt,con,r) = Map.insertWith (Map.unionWith (++)) nt (Map.singleton con [r])
                             in  foldr f (Map.empty) _elemsIcollectedRules
                             {-# LINE 687 "dist/build/Transform.hs"#-}
   {-# INLINE rule7 #-}
   {-# LINE 278 "./src-ag/Transform.ag" #-}
   rule7 = \ _allAttrDecls ((_elemsIcollectedSigs) :: [ (NontermIdent, ConstructorIdent, SigInfo) ]) ((_elemsIcollectedUniques) :: [ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]) ->
                             {-# LINE 278 "./src-ag/Transform.ag" #-}
                             let f (nt,con,t) = Map.insertWith (Map.unionWith (++)) nt (Map.singleton con [t])
                                 typeof nt r = Map.findWithDefault (Haskell "<unknown>") r $ fst $ Map.findWithDefault (Map.empty,Map.empty) nt _allAttrDecls
                             in  foldr f (Map.empty) ( _elemsIcollectedSigs
                                                     ++ [ (nt, con, (ident,typeof nt ref))  | (nt, con, us) <- _elemsIcollectedUniques, (ident,ref) <- us ]
                                                     )
                             {-# LINE 697 "dist/build/Transform.hs"#-}
   {-# INLINE rule8 #-}
   {-# LINE 284 "./src-ag/Transform.ag" #-}
   rule8 = \ ((_elemsIcollectedInsts) :: [ (NontermIdent, ConstructorIdent, [Identifier]) ]) ->
                             {-# LINE 284 "./src-ag/Transform.ag" #-}
                             let f (nt,con,is) = Map.insertWith (Map.unionWith (++)) nt (Map.singleton con is)
                             in  foldr f (Map.empty) _elemsIcollectedInsts
                             {-# LINE 704 "dist/build/Transform.hs"#-}
   {-# INLINE rule9 #-}
   {-# LINE 287 "./src-ag/Transform.ag" #-}
   rule9 = \ ((_elemsIcollectedUniques) :: [ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]) ->
                             {-# LINE 287 "./src-ag/Transform.ag" #-}
                             let f (nt,con,us) = Map.insertWith (Map.unionWith (++)) nt (Map.singleton con us)
                             in foldr f (Map.empty) _elemsIcollectedUniques
                             {-# LINE 711 "dist/build/Transform.hs"#-}
   {-# INLINE rule10 #-}
   {-# LINE 289 "./src-ag/Transform.ag" #-}
   rule10 = \ ((_elemsIcollectedAugments) :: [ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]) ->
                             {-# LINE 289 "./src-ag/Transform.ag" #-}
                             let f (nt,con,as) = Map.insertWith (Map.unionWith (++)) nt (Map.singleton con as)
                             in foldr f Map.empty _elemsIcollectedAugments
                             {-# LINE 718 "dist/build/Transform.hs"#-}
   {-# INLINE rule11 #-}
   {-# LINE 291 "./src-ag/Transform.ag" #-}
   rule11 = \ ((_elemsIcollectedArounds) :: [ (NontermIdent, ConstructorIdent, [AroundInfo])  ]) ->
                              {-# LINE 291 "./src-ag/Transform.ag" #-}
                              let f (nt,con,as) = Map.insertWith (Map.unionWith (++)) nt (Map.singleton con as)
                              in foldr f Map.empty _elemsIcollectedArounds
                              {-# LINE 725 "dist/build/Transform.hs"#-}
   {-# INLINE rule12 #-}
   {-# LINE 293 "./src-ag/Transform.ag" #-}
   rule12 = \ ((_elemsIcollectedMerges) :: [ (NontermIdent, ConstructorIdent, [MergeInfo])   ]) ->
                              {-# LINE 293 "./src-ag/Transform.ag" #-}
                              let f (nt,con,as) = Map.insertWith (Map.unionWith (++)) nt (Map.singleton con as)
                               in foldr f Map.empty _elemsIcollectedMerges
                              {-# LINE 732 "dist/build/Transform.hs"#-}
   {-# INLINE rule13 #-}
   {-# LINE 296 "./src-ag/Transform.ag" #-}
   rule13 = \ _allAugments ->
                                {-# LINE 296 "./src-ag/Transform.ag" #-}
                                let gen _ = []
                                in Map.map (Map.map gen) _allAugments
                                {-# LINE 739 "dist/build/Transform.hs"#-}
   {-# INLINE rule14 #-}
   {-# LINE 299 "./src-ag/Transform.ag" #-}
   rule14 = \ _allAttrDecls _allFields _allInsts _allMerges _allRules _allSigs ->
                                {-# LINE 299 "./src-ag/Transform.ag" #-}
                                Map.mapWithKey (Map.mapWithKey . (checkRules _allAttrDecls _allFields _allInsts _allSigs     _allMerges    )) _allRules
                                {-# LINE 745 "dist/build/Transform.hs"#-}
   {-# INLINE rule15 #-}
   {-# LINE 300 "./src-ag/Transform.ag" #-}
   rule15 = \ _allRules ->
                                {-# LINE 300 "./src-ag/Transform.ag" #-}
                                Map.mapWithKey (Map.mapWithKey . checkRuleNames) _allRules
                                {-# LINE 751 "dist/build/Transform.hs"#-}
   {-# INLINE rule16 #-}
   {-# LINE 301 "./src-ag/Transform.ag" #-}
   rule16 = \ _allSigs ->
                                {-# LINE 301 "./src-ag/Transform.ag" #-}
                                Map.mapWithKey (Map.mapWithKey . (checkSigs                                                 )) _allSigs
                                {-# LINE 757 "dist/build/Transform.hs"#-}
   {-# INLINE rule17 #-}
   {-# LINE 302 "./src-ag/Transform.ag" #-}
   rule17 = \ _allFields _allInsts _allNonterminals _allSigs ->
                                {-# LINE 302 "./src-ag/Transform.ag" #-}
                                Map.mapWithKey (Map.mapWithKey . (checkInsts _allNonterminals     _allSigs     _allFields   )) _allInsts
                                {-# LINE 763 "dist/build/Transform.hs"#-}
   {-# INLINE rule18 #-}
   {-# LINE 303 "./src-ag/Transform.ag" #-}
   rule18 = \ _allAttrDecls _allUniques ->
                                {-# LINE 303 "./src-ag/Transform.ag" #-}
                                Map.mapWithKey (Map.mapWithKey . (checkUniques _allAttrDecls                                )) _allUniques
                                {-# LINE 769 "dist/build/Transform.hs"#-}
   {-# INLINE rule19 #-}
   {-# LINE 304 "./src-ag/Transform.ag" #-}
   rule19 = \ _allAttrDecls _allAugments ->
                                {-# LINE 304 "./src-ag/Transform.ag" #-}
                                Map.mapWithKey (Map.mapWithKey . (checkAugments _allAttrDecls                               )) _allAugments
                                {-# LINE 775 "dist/build/Transform.hs"#-}
   {-# INLINE rule20 #-}
   {-# LINE 305 "./src-ag/Transform.ag" #-}
   rule20 = \ _allArounds _allFields ->
                                {-# LINE 305 "./src-ag/Transform.ag" #-}
                                Map.mapWithKey (Map.mapWithKey . (checkArounds _allFields    )) _allArounds
                                {-# LINE 781 "dist/build/Transform.hs"#-}
   {-# INLINE rule21 #-}
   {-# LINE 306 "./src-ag/Transform.ag" #-}
   rule21 = \ _allFields _allInsts _allMerges _allNonterminals ->
                                {-# LINE 306 "./src-ag/Transform.ag" #-}
                                Map.mapWithKey (Map.mapWithKey . (checkMerges _allNonterminals     _allInsts     _allFields    )) _allMerges
                                {-# LINE 787 "dist/build/Transform.hs"#-}
   {-# INLINE rule22 #-}
   {-# LINE 308 "./src-ag/Transform.ag" #-}
   rule22 = \ _allRulesErrs ->
                                 {-# LINE 308 "./src-ag/Transform.ag" #-}
                                 Map.map (Map.map fst) _allRulesErrs
                                 {-# LINE 793 "dist/build/Transform.hs"#-}
   {-# INLINE rule23 #-}
   {-# LINE 309 "./src-ag/Transform.ag" #-}
   rule23 = \ _allSigsErrs _augmentSigs ->
                                 {-# LINE 309 "./src-ag/Transform.ag" #-}
                                 Map.map (Map.map fst) _allSigsErrs     `unionunionplusplus` _augmentSigs
                                 {-# LINE 799 "dist/build/Transform.hs"#-}
   {-# INLINE rule24 #-}
   {-# LINE 310 "./src-ag/Transform.ag" #-}
   rule24 = \ _allInstsErrs ->
                                 {-# LINE 310 "./src-ag/Transform.ag" #-}
                                 Map.map (Map.map fst) _allInstsErrs
                                 {-# LINE 805 "dist/build/Transform.hs"#-}
   {-# INLINE rule25 #-}
   {-# LINE 311 "./src-ag/Transform.ag" #-}
   rule25 = \ _allUniquesErrs ->
                                 {-# LINE 311 "./src-ag/Transform.ag" #-}
                                 Map.map (Map.map fst) _allUniquesErrs
                                 {-# LINE 811 "dist/build/Transform.hs"#-}
   {-# INLINE rule26 #-}
   {-# LINE 312 "./src-ag/Transform.ag" #-}
   rule26 = \ _allAugmentErrs ->
                                 {-# LINE 312 "./src-ag/Transform.ag" #-}
                                 Map.map (Map.map fst) _allAugmentErrs
                                 {-# LINE 817 "dist/build/Transform.hs"#-}
   {-# INLINE rule27 #-}
   {-# LINE 313 "./src-ag/Transform.ag" #-}
   rule27 = \ _allAroundsErrs ->
                                 {-# LINE 313 "./src-ag/Transform.ag" #-}
                                 Map.map (Map.map fst) _allAroundsErrs
                                 {-# LINE 823 "dist/build/Transform.hs"#-}
   {-# INLINE rule28 #-}
   {-# LINE 314 "./src-ag/Transform.ag" #-}
   rule28 = \ _allAttrDecls _allFields _allRules _checkedInsts _checkedRulesPre _checkedUniques ((_lhsIoptions) :: Options) ->
                                 {-# LINE 314 "./src-ag/Transform.ag" #-}
                                 Map.unionWith (Map.unionWith (++)) _checkedRulesPre     (Map.mapWithKey (Map.mapWithKey . (mkUniqueRules _lhsIoptions _allRules     _allFields     _checkedInsts     _allAttrDecls    )) _checkedUniques    )
                                 {-# LINE 829 "dist/build/Transform.hs"#-}
   {-# INLINE rule29 #-}
   {-# LINE 315 "./src-ag/Transform.ag" #-}
   rule29 = \ _allMergesErrs ->
                                 {-# LINE 315 "./src-ag/Transform.ag" #-}
                                 Map.map (Map.map fst) _allMergesErrs
                                 {-# LINE 835 "dist/build/Transform.hs"#-}
   {-# INLINE rule30 #-}
   {-# LINE 317 "./src-ag/Transform.ag" #-}
   rule30 = \ ((_elemsItypeSyns) :: TypeSyns) ->
                             {-# LINE 317 "./src-ag/Transform.ag" #-}
                             let f = checkForDuplicates (DupSynonym)
                             in  Seq.fromList . f . map fst $ _elemsItypeSyns
                             {-# LINE 842 "dist/build/Transform.hs"#-}
   {-# INLINE rule31 #-}
   {-# LINE 320 "./src-ag/Transform.ag" #-}
   rule31 = \ _allFields ->
                             {-# LINE 320 "./src-ag/Transform.ag" #-}
                             let g nt (con,fm) = checkForDuplicates (DupChild nt con) (map fst fm)
                                 f (nt,cfm)    = concat . map (g nt) . Map.toList $ cfm
                             in  Seq.fromList . concat . map f . Map.toList $ _allFields
                             {-# LINE 850 "dist/build/Transform.hs"#-}
   {-# INLINE rule32 #-}
   {-# LINE 324 "./src-ag/Transform.ag" #-}
   rule32 = \  (_ :: ()) ->
                             {-# LINE 324 "./src-ag/Transform.ag" #-}
                             let
                             in   Seq.empty
                             {-# LINE 857 "dist/build/Transform.hs"#-}
   {-# INLINE rule33 #-}
   {-# LINE 328 "./src-ag/Transform.ag" #-}
   rule33 = \ _allRulesErrs ->
                             {-# LINE 328 "./src-ag/Transform.ag" #-}
                             let  f m s = Map.fold ((><) . snd) s m
                             in Map.fold f Seq.empty _allRulesErrs
                             {-# LINE 864 "dist/build/Transform.hs"#-}
   {-# INLINE rule34 #-}
   {-# LINE 331 "./src-ag/Transform.ag" #-}
   rule34 = \ _allSigsErrs ->
                             {-# LINE 331 "./src-ag/Transform.ag" #-}
                             let  f m s = Map.fold ((><) . snd) s m
                             in Map.fold f Seq.empty _allSigsErrs
                             {-# LINE 871 "dist/build/Transform.hs"#-}
   {-# INLINE rule35 #-}
   {-# LINE 334 "./src-ag/Transform.ag" #-}
   rule35 = \ _allInstsErrs ->
                             {-# LINE 334 "./src-ag/Transform.ag" #-}
                             let  f m s = Map.fold ((><) . snd) s m
                             in Map.fold f Seq.empty _allInstsErrs
                             {-# LINE 878 "dist/build/Transform.hs"#-}
   {-# INLINE rule36 #-}
   {-# LINE 337 "./src-ag/Transform.ag" #-}
   rule36 = \ _allUniquesErrs ->
                             {-# LINE 337 "./src-ag/Transform.ag" #-}
                             let  f m s = Map.fold ((><) . snd) s m
                             in Map.fold f Seq.empty _allUniquesErrs
                             {-# LINE 885 "dist/build/Transform.hs"#-}
   {-# INLINE rule37 #-}
   {-# LINE 340 "./src-ag/Transform.ag" #-}
   rule37 = \ _allAugmentErrs ->
                             {-# LINE 340 "./src-ag/Transform.ag" #-}
                             let  f m s = Map.fold ((><) . snd) s m
                             in Map.fold f Seq.empty _allAugmentErrs
                             {-# LINE 892 "dist/build/Transform.hs"#-}
   {-# INLINE rule38 #-}
   {-# LINE 343 "./src-ag/Transform.ag" #-}
   rule38 = \ _allAroundsErrs ->
                             {-# LINE 343 "./src-ag/Transform.ag" #-}
                             let  f m s = Map.fold ((><) . snd) s m
                             in Map.fold f Seq.empty _allAroundsErrs
                             {-# LINE 899 "dist/build/Transform.hs"#-}
   {-# INLINE rule39 #-}
   {-# LINE 346 "./src-ag/Transform.ag" #-}
   rule39 = \ _allNamesErrs ->
                              {-# LINE 346 "./src-ag/Transform.ag" #-}
                              let  f m s = Map.fold ((><)) s m
                              in Map.fold f Seq.empty _allNamesErrs
                              {-# LINE 906 "dist/build/Transform.hs"#-}
   {-# INLINE rule40 #-}
   {-# LINE 349 "./src-ag/Transform.ag" #-}
   rule40 = \ _allMergesErrs ->
                              {-# LINE 349 "./src-ag/Transform.ag" #-}
                              let f m s = Map.fold ((><) . snd) s m
                              in Map.fold f Seq.empty _allMergesErrs
                              {-# LINE 913 "dist/build/Transform.hs"#-}
   {-# INLINE rule41 #-}
   {-# LINE 352 "./src-ag/Transform.ag" #-}
   rule41 = \ ((_elemsIerrors) :: Seq Error) _errs1 _errs10 _errs11 _errs2 _errs3 _errs4 _errs5 _errs6 _errs7 _errs8 _errs9 ->
                             {-# LINE 352 "./src-ag/Transform.ag" #-}
                             _elemsIerrors >< _errs1 >< _errs2 >< _errs3 >< _errs4 >< _errs5 >< _errs6 >< _errs7 >< _errs8 >< _errs9 >< _errs10 >< _errs11
                             {-# LINE 919 "dist/build/Transform.hs"#-}
   {-# INLINE rule42 #-}
   {-# LINE 606 "./src-ag/Transform.ag" #-}
   rule42 = \ ((_elemsIcollectedNames) :: Set Identifier) ((_elemsIcollectedSetNames) :: Set Identifier) ->
                                 {-# LINE 606 "./src-ag/Transform.ag" #-}
                                 _elemsIcollectedNames `Set.difference` _elemsIcollectedSetNames
                                 {-# LINE 925 "dist/build/Transform.hs"#-}
   {-# INLINE rule43 #-}
   {-# LINE 626 "./src-ag/Transform.ag" #-}
   rule43 = \ ((_elemsIcollectedConstructorsMap) :: Map NontermIdent (Set ConstructorIdent)) ->
                                 {-# LINE 626 "./src-ag/Transform.ag" #-}
                                 _elemsIcollectedConstructorsMap
                                 {-# LINE 931 "dist/build/Transform.hs"#-}
   {-# INLINE rule44 #-}
   {-# LINE 709 "./src-ag/Transform.ag" #-}
   rule44 = \ _allNonterminals ->
                             {-# LINE 709 "./src-ag/Transform.ag" #-}
                             Map.fromList (map (\x->(x,(Set.singleton x, Set.empty))) (Set.toList _allNonterminals    ))
                             {-# LINE 937 "dist/build/Transform.hs"#-}
   {-# INLINE rule45 #-}
   {-# LINE 710 "./src-ag/Transform.ag" #-}
   rule45 = \ ((_elemsIdefSets) :: Map Identifier (Set NontermIdent,Set Identifier)) ->
                             {-# LINE 710 "./src-ag/Transform.ag" #-}
                             Map.map fst _elemsIdefSets
                             {-# LINE 943 "dist/build/Transform.hs"#-}
   {-# INLINE rule46 #-}
   {-# LINE 1029 "./src-ag/Transform.ag" #-}
   rule46 = \  (_ :: ()) ->
                           {-# LINE 1029 "./src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 949 "dist/build/Transform.hs"#-}
   {-# INLINE rule47 #-}
   {-# LINE 1085 "./src-ag/Transform.ag" #-}
   rule47 = \ _allNonterminals ((_elemsIattrDecls) :: Map NontermIdent (Attributes, Attributes)) ((_lhsIoptions) :: Options) ->
                             {-# LINE 1085 "./src-ag/Transform.ag" #-}
                             if withSelf _lhsIoptions
                              then foldr addSelf _elemsIattrDecls (Set.toList _allNonterminals    )
                              else               _elemsIattrDecls
                             {-# LINE 957 "dist/build/Transform.hs"#-}
   {-# INLINE rule48 #-}
   {-# LINE 1323 "./src-ag/Transform.ag" #-}
   rule48 = \ ((_elemsIcollectedMacros) :: [(NontermIdent, ConstructorIdent, MaybeMacro)]) ->
                             {-# LINE 1323 "./src-ag/Transform.ag" #-}
                             let f (nt,con,m) = Map.insertWith (Map.union) nt (Map.singleton con m)
                             in  foldr f (Map.empty) _elemsIcollectedMacros
                             {-# LINE 964 "dist/build/Transform.hs"#-}
   {-# INLINE rule49 #-}
   {-# LINE 1336 "./src-ag/Transform.ag" #-}
   rule49 = \ _allAttrs _allFields _allNonterminals ->
                        {-# LINE 1336 "./src-ag/Transform.ag" #-}
                        (_allNonterminals    ,_allFields    ,_allAttrs    )
                        {-# LINE 970 "dist/build/Transform.hs"#-}
   {-# INLINE rule50 #-}
   {-# LINE 1338 "./src-ag/Transform.ag" #-}
   rule50 = \ _allNonterminals ((_elemsIattrs) :: Map NontermIdent (Attributes, Attributes)) ((_lhsIoptions) :: Options) ->
                         {-# LINE 1338 "./src-ag/Transform.ag" #-}
                         if withSelf _lhsIoptions
                              then foldr addSelf _elemsIattrs (Set.toList _allNonterminals    )
                              else               _elemsIattrs
                         {-# LINE 978 "dist/build/Transform.hs"#-}
   {-# INLINE rule51 #-}
   {-# LINE 1346 "./src-ag/Transform.ag" #-}
   rule51 = \  (_ :: ()) ->
                        {-# LINE 1346 "./src-ag/Transform.ag" #-}
                        Map.empty
                        {-# LINE 984 "dist/build/Transform.hs"#-}
   {-# INLINE rule52 #-}
   rule52 = \ ((_elemsIblocks) :: Blocks) ->
     _elemsIblocks
   {-# INLINE rule53 #-}
   rule53 = \ ((_elemsImoduleDecl) :: Maybe (String,String,String)) ->
     _elemsImoduleDecl
   {-# INLINE rule54 #-}
   rule54 = \ ((_elemsIpragmas) :: Options -> Options) ->
     _elemsIpragmas
   {-# INLINE rule55 #-}
   rule55 = \ _allAttrDecls ->
     _allAttrDecls
   {-# INLINE rule56 #-}
   rule56 = \ _allAttrs ->
     _allAttrs
   {-# INLINE rule57 #-}
   rule57 = \ _allFields ->
     _allFields
   {-# INLINE rule58 #-}
   rule58 = \ _allNonterminals ->
     _allNonterminals
   {-# INLINE rule59 #-}
   rule59 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions

-- Alt ---------------------------------------------------------
-- wrapper
data Inh_Alt  = Inh_Alt { allConstructors_Inh_Alt :: (Map NontermIdent (Set ConstructorIdent)), allNonterminals_Inh_Alt :: (Set NontermIdent), nts_Inh_Alt :: (Set NontermIdent) }
data Syn_Alt  = Syn_Alt { collectedConParams_Syn_Alt :: ([(NontermIdent, ConstructorIdent, Set Identifier)]), collectedConstraints_Syn_Alt :: ([(NontermIdent, ConstructorIdent, [Type])]), collectedConstructorNames_Syn_Alt :: (Set ConstructorIdent), collectedFields_Syn_Alt :: ([(NontermIdent, ConstructorIdent, FieldMap)]), collectedMacros_Syn_Alt :: ([(NontermIdent, ConstructorIdent, MaybeMacro)]) }
{-# INLINABLE wrap_Alt #-}
wrap_Alt :: T_Alt  -> Inh_Alt  -> (Syn_Alt )
wrap_Alt (T_Alt act) (Inh_Alt _lhsIallConstructors _lhsIallNonterminals _lhsInts) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Alt_vIn4 _lhsIallConstructors _lhsIallNonterminals _lhsInts
        (T_Alt_vOut4 _lhsOcollectedConParams _lhsOcollectedConstraints _lhsOcollectedConstructorNames _lhsOcollectedFields _lhsOcollectedMacros) <- return (inv_Alt_s5 sem arg)
        return (Syn_Alt _lhsOcollectedConParams _lhsOcollectedConstraints _lhsOcollectedConstructorNames _lhsOcollectedFields _lhsOcollectedMacros)
   )

-- cata
{-# INLINE sem_Alt #-}
sem_Alt :: Alt  -> T_Alt 
sem_Alt ( Alt pos_ names_ tyvars_ fields_ macro_ ) = sem_Alt_Alt pos_ ( sem_ConstructorSet names_ ) tyvars_ ( sem_Fields fields_ ) macro_

-- semantic domain
newtype T_Alt  = T_Alt {
                       attach_T_Alt :: Identity (T_Alt_s5 )
                       }
newtype T_Alt_s5  = C_Alt_s5 {
                             inv_Alt_s5 :: (T_Alt_v4 )
                             }
data T_Alt_s6  = C_Alt_s6
type T_Alt_v4  = (T_Alt_vIn4 ) -> (T_Alt_vOut4 )
data T_Alt_vIn4  = T_Alt_vIn4 (Map NontermIdent (Set ConstructorIdent)) (Set NontermIdent) (Set NontermIdent)
data T_Alt_vOut4  = T_Alt_vOut4 ([(NontermIdent, ConstructorIdent, Set Identifier)]) ([(NontermIdent, ConstructorIdent, [Type])]) (Set ConstructorIdent) ([(NontermIdent, ConstructorIdent, FieldMap)]) ([(NontermIdent, ConstructorIdent, MaybeMacro)])
{-# NOINLINE sem_Alt_Alt #-}
sem_Alt_Alt :: (Pos) -> T_ConstructorSet  -> ([Identifier]) -> T_Fields  -> (MaybeMacro) -> T_Alt 
sem_Alt_Alt _ arg_names_ arg_tyvars_ arg_fields_ arg_macro_ = T_Alt (return st5) where
   {-# NOINLINE st5 #-}
   st5 = let
      v4 :: T_Alt_v4 
      v4 = \ (T_Alt_vIn4 _lhsIallConstructors _lhsIallNonterminals _lhsInts) -> ( let
         _namesX14 = Control.Monad.Identity.runIdentity (attach_T_ConstructorSet (arg_names_))
         _fieldsX26 = Control.Monad.Identity.runIdentity (attach_T_Fields (arg_fields_))
         (T_ConstructorSet_vOut13 _namesIcollectedConstructorNames _namesIconstructors _namesIerrors) = inv_ConstructorSet_s14 _namesX14 (T_ConstructorSet_vIn13 )
         (T_Fields_vOut25 _fieldsIcollectedConstraints _fieldsIcollectedFields) = inv_Fields_s26 _fieldsX26 (T_Fields_vIn25 _fieldsOallNonterminals)
         _lhsOcollectedFields :: [(NontermIdent, ConstructorIdent, FieldMap)]
         _lhsOcollectedFields = rule60 _fieldsIcollectedFields _lhsIallConstructors _lhsInts _namesIconstructors
         _lhsOcollectedConstraints :: [(NontermIdent, ConstructorIdent, [Type])]
         _lhsOcollectedConstraints = rule61 _fieldsIcollectedConstraints _lhsIallConstructors _lhsInts _namesIconstructors
         _lhsOcollectedConParams :: [(NontermIdent, ConstructorIdent, Set Identifier)]
         _lhsOcollectedConParams = rule62 _lhsIallConstructors _lhsInts _namesIconstructors arg_tyvars_
         _lhsOcollectedMacros :: [(NontermIdent, ConstructorIdent, MaybeMacro)]
         _lhsOcollectedMacros = rule63 _lhsIallConstructors _lhsInts _namesIconstructors arg_macro_
         _lhsOcollectedConstructorNames :: Set ConstructorIdent
         _lhsOcollectedConstructorNames = rule64 _namesIcollectedConstructorNames
         _fieldsOallNonterminals = rule65 _lhsIallNonterminals
         __result_ = T_Alt_vOut4 _lhsOcollectedConParams _lhsOcollectedConstraints _lhsOcollectedConstructorNames _lhsOcollectedFields _lhsOcollectedMacros
         in __result_ )
     in C_Alt_s5 v4
   {-# INLINE rule60 #-}
   {-# LINE 240 "./src-ag/Transform.ag" #-}
   rule60 = \ ((_fieldsIcollectedFields) :: [(Identifier, Type)]) ((_lhsIallConstructors) :: Map NontermIdent (Set ConstructorIdent)) ((_lhsInts) :: Set NontermIdent) ((_namesIconstructors) :: (Set ConstructorIdent->Set ConstructorIdent)) ->
                                        {-# LINE 240 "./src-ag/Transform.ag" #-}
                                        [ (nt, con, _fieldsIcollectedFields)
                                        | nt  <- Set.toList _lhsInts
                                        , con <- Set.toList (_namesIconstructors (Map.findWithDefault Set.empty nt _lhsIallConstructors))
                                        ]
                                        {-# LINE 1073 "dist/build/Transform.hs"#-}
   {-# INLINE rule61 #-}
   {-# LINE 244 "./src-ag/Transform.ag" #-}
   rule61 = \ ((_fieldsIcollectedConstraints) :: [Type]) ((_lhsIallConstructors) :: Map NontermIdent (Set ConstructorIdent)) ((_lhsInts) :: Set NontermIdent) ((_namesIconstructors) :: (Set ConstructorIdent->Set ConstructorIdent)) ->
                                        {-# LINE 244 "./src-ag/Transform.ag" #-}
                                        [ (nt, con, _fieldsIcollectedConstraints)
                                        | nt  <- Set.toList _lhsInts
                                        , con <- Set.toList (_namesIconstructors (Map.findWithDefault Set.empty nt _lhsIallConstructors))
                                        ]
                                        {-# LINE 1082 "dist/build/Transform.hs"#-}
   {-# INLINE rule62 #-}
   {-# LINE 248 "./src-ag/Transform.ag" #-}
   rule62 = \ ((_lhsIallConstructors) :: Map NontermIdent (Set ConstructorIdent)) ((_lhsInts) :: Set NontermIdent) ((_namesIconstructors) :: (Set ConstructorIdent->Set ConstructorIdent)) tyvars_ ->
                                        {-# LINE 248 "./src-ag/Transform.ag" #-}
                                        [ (nt, con, Set.fromList tyvars_)
                                        | nt  <- Set.toList _lhsInts
                                        , con <- Set.toList (_namesIconstructors (Map.findWithDefault Set.empty nt _lhsIallConstructors))
                                        ]
                                        {-# LINE 1091 "dist/build/Transform.hs"#-}
   {-# INLINE rule63 #-}
   {-# LINE 1314 "./src-ag/Transform.ag" #-}
   rule63 = \ ((_lhsIallConstructors) :: Map NontermIdent (Set ConstructorIdent)) ((_lhsInts) :: Set NontermIdent) ((_namesIconstructors) :: (Set ConstructorIdent->Set ConstructorIdent)) macro_ ->
                                        {-# LINE 1314 "./src-ag/Transform.ag" #-}
                                        [ (nt, con, macro_)
                                        | nt  <- Set.toList _lhsInts
                                        , con <- Set.toList (_namesIconstructors (Map.findWithDefault Set.empty nt _lhsIallConstructors))
                                        ]
                                        {-# LINE 1100 "dist/build/Transform.hs"#-}
   {-# INLINE rule64 #-}
   rule64 = \ ((_namesIcollectedConstructorNames) :: Set ConstructorIdent) ->
     _namesIcollectedConstructorNames
   {-# INLINE rule65 #-}
   rule65 = \ ((_lhsIallNonterminals) :: Set NontermIdent) ->
     _lhsIallNonterminals

-- Alts --------------------------------------------------------
-- wrapper
data Inh_Alts  = Inh_Alts { allConstructors_Inh_Alts :: (Map NontermIdent (Set ConstructorIdent)), allNonterminals_Inh_Alts :: (Set NontermIdent), nts_Inh_Alts :: (Set NontermIdent) }
data Syn_Alts  = Syn_Alts { collectedConParams_Syn_Alts :: ([(NontermIdent, ConstructorIdent, Set Identifier)]), collectedConstraints_Syn_Alts :: ([(NontermIdent, ConstructorIdent, [Type])]), collectedConstructorNames_Syn_Alts :: (Set ConstructorIdent), collectedFields_Syn_Alts :: ([(NontermIdent, ConstructorIdent, FieldMap)]), collectedMacros_Syn_Alts :: ([(NontermIdent, ConstructorIdent, MaybeMacro)]) }
{-# INLINABLE wrap_Alts #-}
wrap_Alts :: T_Alts  -> Inh_Alts  -> (Syn_Alts )
wrap_Alts (T_Alts act) (Inh_Alts _lhsIallConstructors _lhsIallNonterminals _lhsInts) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Alts_vIn7 _lhsIallConstructors _lhsIallNonterminals _lhsInts
        (T_Alts_vOut7 _lhsOcollectedConParams _lhsOcollectedConstraints _lhsOcollectedConstructorNames _lhsOcollectedFields _lhsOcollectedMacros) <- return (inv_Alts_s8 sem arg)
        return (Syn_Alts _lhsOcollectedConParams _lhsOcollectedConstraints _lhsOcollectedConstructorNames _lhsOcollectedFields _lhsOcollectedMacros)
   )

-- cata
{-# NOINLINE sem_Alts #-}
sem_Alts :: Alts  -> T_Alts 
sem_Alts list = Prelude.foldr sem_Alts_Cons sem_Alts_Nil (Prelude.map sem_Alt list)

-- semantic domain
newtype T_Alts  = T_Alts {
                         attach_T_Alts :: Identity (T_Alts_s8 )
                         }
newtype T_Alts_s8  = C_Alts_s8 {
                               inv_Alts_s8 :: (T_Alts_v7 )
                               }
data T_Alts_s9  = C_Alts_s9
type T_Alts_v7  = (T_Alts_vIn7 ) -> (T_Alts_vOut7 )
data T_Alts_vIn7  = T_Alts_vIn7 (Map NontermIdent (Set ConstructorIdent)) (Set NontermIdent) (Set NontermIdent)
data T_Alts_vOut7  = T_Alts_vOut7 ([(NontermIdent, ConstructorIdent, Set Identifier)]) ([(NontermIdent, ConstructorIdent, [Type])]) (Set ConstructorIdent) ([(NontermIdent, ConstructorIdent, FieldMap)]) ([(NontermIdent, ConstructorIdent, MaybeMacro)])
{-# NOINLINE sem_Alts_Cons #-}
sem_Alts_Cons :: T_Alt  -> T_Alts  -> T_Alts 
sem_Alts_Cons arg_hd_ arg_tl_ = T_Alts (return st8) where
   {-# NOINLINE st8 #-}
   st8 = let
      v7 :: T_Alts_v7 
      v7 = \ (T_Alts_vIn7 _lhsIallConstructors _lhsIallNonterminals _lhsInts) -> ( let
         _hdX5 = Control.Monad.Identity.runIdentity (attach_T_Alt (arg_hd_))
         _tlX8 = Control.Monad.Identity.runIdentity (attach_T_Alts (arg_tl_))
         (T_Alt_vOut4 _hdIcollectedConParams _hdIcollectedConstraints _hdIcollectedConstructorNames _hdIcollectedFields _hdIcollectedMacros) = inv_Alt_s5 _hdX5 (T_Alt_vIn4 _hdOallConstructors _hdOallNonterminals _hdOnts)
         (T_Alts_vOut7 _tlIcollectedConParams _tlIcollectedConstraints _tlIcollectedConstructorNames _tlIcollectedFields _tlIcollectedMacros) = inv_Alts_s8 _tlX8 (T_Alts_vIn7 _tlOallConstructors _tlOallNonterminals _tlOnts)
         _lhsOcollectedConParams :: [(NontermIdent, ConstructorIdent, Set Identifier)]
         _lhsOcollectedConParams = rule66 _hdIcollectedConParams _tlIcollectedConParams
         _lhsOcollectedConstraints :: [(NontermIdent, ConstructorIdent, [Type])]
         _lhsOcollectedConstraints = rule67 _hdIcollectedConstraints _tlIcollectedConstraints
         _lhsOcollectedConstructorNames :: Set ConstructorIdent
         _lhsOcollectedConstructorNames = rule68 _hdIcollectedConstructorNames _tlIcollectedConstructorNames
         _lhsOcollectedFields :: [(NontermIdent, ConstructorIdent, FieldMap)]
         _lhsOcollectedFields = rule69 _hdIcollectedFields _tlIcollectedFields
         _lhsOcollectedMacros :: [(NontermIdent, ConstructorIdent, MaybeMacro)]
         _lhsOcollectedMacros = rule70 _hdIcollectedMacros _tlIcollectedMacros
         _hdOallConstructors = rule71 _lhsIallConstructors
         _hdOallNonterminals = rule72 _lhsIallNonterminals
         _hdOnts = rule73 _lhsInts
         _tlOallConstructors = rule74 _lhsIallConstructors
         _tlOallNonterminals = rule75 _lhsIallNonterminals
         _tlOnts = rule76 _lhsInts
         __result_ = T_Alts_vOut7 _lhsOcollectedConParams _lhsOcollectedConstraints _lhsOcollectedConstructorNames _lhsOcollectedFields _lhsOcollectedMacros
         in __result_ )
     in C_Alts_s8 v7
   {-# INLINE rule66 #-}
   rule66 = \ ((_hdIcollectedConParams) :: [(NontermIdent, ConstructorIdent, Set Identifier)]) ((_tlIcollectedConParams) :: [(NontermIdent, ConstructorIdent, Set Identifier)]) ->
     _hdIcollectedConParams ++ _tlIcollectedConParams
   {-# INLINE rule67 #-}
   rule67 = \ ((_hdIcollectedConstraints) :: [(NontermIdent, ConstructorIdent, [Type])]) ((_tlIcollectedConstraints) :: [(NontermIdent, ConstructorIdent, [Type])]) ->
     _hdIcollectedConstraints ++ _tlIcollectedConstraints
   {-# INLINE rule68 #-}
   rule68 = \ ((_hdIcollectedConstructorNames) :: Set ConstructorIdent) ((_tlIcollectedConstructorNames) :: Set ConstructorIdent) ->
     _hdIcollectedConstructorNames `Set.union` _tlIcollectedConstructorNames
   {-# INLINE rule69 #-}
   rule69 = \ ((_hdIcollectedFields) :: [(NontermIdent, ConstructorIdent, FieldMap)]) ((_tlIcollectedFields) :: [(NontermIdent, ConstructorIdent, FieldMap)]) ->
     _hdIcollectedFields ++ _tlIcollectedFields
   {-# INLINE rule70 #-}
   rule70 = \ ((_hdIcollectedMacros) :: [(NontermIdent, ConstructorIdent, MaybeMacro)]) ((_tlIcollectedMacros) :: [(NontermIdent, ConstructorIdent, MaybeMacro)]) ->
     _hdIcollectedMacros ++ _tlIcollectedMacros
   {-# INLINE rule71 #-}
   rule71 = \ ((_lhsIallConstructors) :: Map NontermIdent (Set ConstructorIdent)) ->
     _lhsIallConstructors
   {-# INLINE rule72 #-}
   rule72 = \ ((_lhsIallNonterminals) :: Set NontermIdent) ->
     _lhsIallNonterminals
   {-# INLINE rule73 #-}
   rule73 = \ ((_lhsInts) :: Set NontermIdent) ->
     _lhsInts
   {-# INLINE rule74 #-}
   rule74 = \ ((_lhsIallConstructors) :: Map NontermIdent (Set ConstructorIdent)) ->
     _lhsIallConstructors
   {-# INLINE rule75 #-}
   rule75 = \ ((_lhsIallNonterminals) :: Set NontermIdent) ->
     _lhsIallNonterminals
   {-# INLINE rule76 #-}
   rule76 = \ ((_lhsInts) :: Set NontermIdent) ->
     _lhsInts
{-# NOINLINE sem_Alts_Nil #-}
sem_Alts_Nil ::  T_Alts 
sem_Alts_Nil  = T_Alts (return st8) where
   {-# NOINLINE st8 #-}
   st8 = let
      v7 :: T_Alts_v7 
      v7 = \ (T_Alts_vIn7 _lhsIallConstructors _lhsIallNonterminals _lhsInts) -> ( let
         _lhsOcollectedConParams :: [(NontermIdent, ConstructorIdent, Set Identifier)]
         _lhsOcollectedConParams = rule77  ()
         _lhsOcollectedConstraints :: [(NontermIdent, ConstructorIdent, [Type])]
         _lhsOcollectedConstraints = rule78  ()
         _lhsOcollectedConstructorNames :: Set ConstructorIdent
         _lhsOcollectedConstructorNames = rule79  ()
         _lhsOcollectedFields :: [(NontermIdent, ConstructorIdent, FieldMap)]
         _lhsOcollectedFields = rule80  ()
         _lhsOcollectedMacros :: [(NontermIdent, ConstructorIdent, MaybeMacro)]
         _lhsOcollectedMacros = rule81  ()
         __result_ = T_Alts_vOut7 _lhsOcollectedConParams _lhsOcollectedConstraints _lhsOcollectedConstructorNames _lhsOcollectedFields _lhsOcollectedMacros
         in __result_ )
     in C_Alts_s8 v7
   {-# INLINE rule77 #-}
   rule77 = \  (_ :: ()) ->
     []
   {-# INLINE rule78 #-}
   rule78 = \  (_ :: ()) ->
     []
   {-# INLINE rule79 #-}
   rule79 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule80 #-}
   rule80 = \  (_ :: ()) ->
     []
   {-# INLINE rule81 #-}
   rule81 = \  (_ :: ()) ->
     []

-- Attrs -------------------------------------------------------
-- wrapper
data Inh_Attrs  = Inh_Attrs { allFields_Inh_Attrs :: (DataTypes), allNonterminals_Inh_Attrs :: (Set NontermIdent), attrDecls_Inh_Attrs :: (Map NontermIdent (Attributes, Attributes)), attrs_Inh_Attrs :: (Map NontermIdent (Attributes, Attributes)), nts_Inh_Attrs :: (Set NontermIdent), options_Inh_Attrs :: (Options) }
data Syn_Attrs  = Syn_Attrs { attrDecls_Syn_Attrs :: (Map NontermIdent (Attributes, Attributes)), attrs_Syn_Attrs :: (Map NontermIdent (Attributes, Attributes)), errors_Syn_Attrs :: (Seq Error), useMap_Syn_Attrs :: (Map NontermIdent (Map Identifier (String,String,String))) }
{-# INLINABLE wrap_Attrs #-}
wrap_Attrs :: T_Attrs  -> Inh_Attrs  -> (Syn_Attrs )
wrap_Attrs (T_Attrs act) (Inh_Attrs _lhsIallFields _lhsIallNonterminals _lhsIattrDecls _lhsIattrs _lhsInts _lhsIoptions) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Attrs_vIn10 _lhsIallFields _lhsIallNonterminals _lhsIattrDecls _lhsIattrs _lhsInts _lhsIoptions
        (T_Attrs_vOut10 _lhsOattrDecls _lhsOattrs _lhsOerrors _lhsOuseMap) <- return (inv_Attrs_s11 sem arg)
        return (Syn_Attrs _lhsOattrDecls _lhsOattrs _lhsOerrors _lhsOuseMap)
   )

-- cata
{-# INLINE sem_Attrs #-}
sem_Attrs :: Attrs  -> T_Attrs 
sem_Attrs ( Attrs pos_ inh_ chn_ syn_ ) = sem_Attrs_Attrs pos_ inh_ chn_ syn_

-- semantic domain
newtype T_Attrs  = T_Attrs {
                           attach_T_Attrs :: Identity (T_Attrs_s11 )
                           }
newtype T_Attrs_s11  = C_Attrs_s11 {
                                   inv_Attrs_s11 :: (T_Attrs_v10 )
                                   }
data T_Attrs_s12  = C_Attrs_s12
type T_Attrs_v10  = (T_Attrs_vIn10 ) -> (T_Attrs_vOut10 )
data T_Attrs_vIn10  = T_Attrs_vIn10 (DataTypes) (Set NontermIdent) (Map NontermIdent (Attributes, Attributes)) (Map NontermIdent (Attributes, Attributes)) (Set NontermIdent) (Options)
data T_Attrs_vOut10  = T_Attrs_vOut10 (Map NontermIdent (Attributes, Attributes)) (Map NontermIdent (Attributes, Attributes)) (Seq Error) (Map NontermIdent (Map Identifier (String,String,String)))
{-# NOINLINE sem_Attrs_Attrs #-}
sem_Attrs_Attrs :: (Pos) -> (AttrNames) -> (AttrNames) -> (AttrNames) -> T_Attrs 
sem_Attrs_Attrs _ arg_inh_ arg_chn_ arg_syn_ = T_Attrs (return st11) where
   {-# NOINLINE st11 #-}
   st11 = let
      v10 :: T_Attrs_v10 
      v10 = \ (T_Attrs_vIn10 _lhsIallFields _lhsIallNonterminals _lhsIattrDecls _lhsIattrs _lhsInts _lhsIoptions) -> ( let
         (_attrDecls,_errors) = rule82 _inherited _lhsIallFields _lhsIattrDecls _lhsInts _synthesized
         (_inherited,_synthesized,_useMap) = rule83 _lhsIallNonterminals arg_chn_ arg_inh_ arg_syn_
         _lhsOuseMap :: Map NontermIdent (Map Identifier (String,String,String))
         _lhsOuseMap = rule84 _lhsInts _useMap
         _errors1 = rule85 _lhsIoptions arg_chn_ arg_inh_ arg_syn_
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule86 _errors _errors1
         _lhsOattrs :: Map NontermIdent (Attributes, Attributes)
         _lhsOattrs = rule87 _inherited _lhsIattrs _lhsInts _synthesized
         _lhsOattrDecls :: Map NontermIdent (Attributes, Attributes)
         _lhsOattrDecls = rule88 _attrDecls
         __result_ = T_Attrs_vOut10 _lhsOattrDecls _lhsOattrs _lhsOerrors _lhsOuseMap
         in __result_ )
     in C_Attrs_s11 v10
   {-# INLINE rule82 #-}
   {-# LINE 1038 "./src-ag/Transform.ag" #-}
   rule82 = \ _inherited ((_lhsIallFields) :: DataTypes) ((_lhsIattrDecls) :: Map NontermIdent (Attributes, Attributes)) ((_lhsInts) :: Set NontermIdent) _synthesized ->
                                     {-# LINE 1038 "./src-ag/Transform.ag" #-}
                                     checkAttrs _lhsIallFields (Set.toList _lhsInts) _inherited _synthesized _lhsIattrDecls
                                     {-# LINE 1293 "dist/build/Transform.hs"#-}
   {-# INLINE rule83 #-}
   {-# LINE 1040 "./src-ag/Transform.ag" #-}
   rule83 = \ ((_lhsIallNonterminals) :: Set NontermIdent) chn_ inh_ syn_ ->
                                                 {-# LINE 1040 "./src-ag/Transform.ag" #-}
                                                 let splitAttrs xs = unzip [ ((n,makeType _lhsIallNonterminals t),(n,ud))
                                                                           | (n,t,ud) <- xs
                                                                           ]
                                                     (inh,_)     = splitAttrs inh_
                                                     (chn,uses1) = splitAttrs chn_
                                                     (syn,uses2) = splitAttrs syn_
                                                     isUse (_,(e1,e2,_)) = not (null e1 || null e2)
                                                 in (inh++chn,chn++syn, Map.fromList (Prelude.filter isUse (uses1++uses2)))
                                                 {-# LINE 1306 "dist/build/Transform.hs"#-}
   {-# INLINE rule84 #-}
   {-# LINE 1048 "./src-ag/Transform.ag" #-}
   rule84 = \ ((_lhsInts) :: Set NontermIdent) _useMap ->
                         {-# LINE 1048 "./src-ag/Transform.ag" #-}
                         Map.fromList (zip (Set.toList _lhsInts) (repeat _useMap))
                         {-# LINE 1312 "dist/build/Transform.hs"#-}
   {-# INLINE rule85 #-}
   {-# LINE 1050 "./src-ag/Transform.ag" #-}
   rule85 = \ ((_lhsIoptions) :: Options) chn_ inh_ syn_ ->
                          {-# LINE 1050 "./src-ag/Transform.ag" #-}
                          if checkParseTy _lhsIoptions
                          then let attrs  = inh_ ++ syn_ ++ chn_
                                   items = map (\(ident,tp,_) -> (getPos ident, tp)) attrs
                                   errs  = map check items
                                   check (pos,Haskell s) =
                                     let ex  = Expression pos tks
                                         tks = [tk]
                                         tk  = HsToken s pos
                                     in Seq.fromList $ checkTy ex
                                   check _ = Seq.empty
                               in foldr (Seq.><) Seq.empty errs
                          else Seq.empty
                          {-# LINE 1329 "dist/build/Transform.hs"#-}
   {-# INLINE rule86 #-}
   {-# LINE 1062 "./src-ag/Transform.ag" #-}
   rule86 = \ _errors _errors1 ->
                         {-# LINE 1062 "./src-ag/Transform.ag" #-}
                         _errors     Seq.>< _errors1
                         {-# LINE 1335 "dist/build/Transform.hs"#-}
   {-# INLINE rule87 #-}
   {-# LINE 1350 "./src-ag/Transform.ag" #-}
   rule87 = \ _inherited ((_lhsIattrs) :: Map NontermIdent (Attributes, Attributes)) ((_lhsInts) :: Set NontermIdent) _synthesized ->
                          {-# LINE 1350 "./src-ag/Transform.ag" #-}
                          let ins decls nt = if Map.member nt decls
                                             then  Map.update (\(inh,syn) -> Just ( Map.union inh $ Map.fromList _inherited
                                                                                       , Map.union syn $ Map.fromList _synthesized)) nt decls
                                             else  Map.insert nt (Map.fromList _inherited, Map.fromList _synthesized) decls
                          in  foldl ins _lhsIattrs (Set.toList _lhsInts)
                          {-# LINE 1345 "dist/build/Transform.hs"#-}
   {-# INLINE rule88 #-}
   rule88 = \ _attrDecls ->
     _attrDecls

-- ConstructorSet ----------------------------------------------
-- wrapper
data Inh_ConstructorSet  = Inh_ConstructorSet {  }
data Syn_ConstructorSet  = Syn_ConstructorSet { collectedConstructorNames_Syn_ConstructorSet :: (Set ConstructorIdent), constructors_Syn_ConstructorSet :: ((Set ConstructorIdent->Set ConstructorIdent)), errors_Syn_ConstructorSet :: (Seq Error) }
{-# INLINABLE wrap_ConstructorSet #-}
wrap_ConstructorSet :: T_ConstructorSet  -> Inh_ConstructorSet  -> (Syn_ConstructorSet )
wrap_ConstructorSet (T_ConstructorSet act) (Inh_ConstructorSet ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_ConstructorSet_vIn13 
        (T_ConstructorSet_vOut13 _lhsOcollectedConstructorNames _lhsOconstructors _lhsOerrors) <- return (inv_ConstructorSet_s14 sem arg)
        return (Syn_ConstructorSet _lhsOcollectedConstructorNames _lhsOconstructors _lhsOerrors)
   )

-- cata
{-# NOINLINE sem_ConstructorSet #-}
sem_ConstructorSet :: ConstructorSet  -> T_ConstructorSet 
sem_ConstructorSet ( CName name_ ) = sem_ConstructorSet_CName name_
sem_ConstructorSet ( CUnion set1_ set2_ ) = sem_ConstructorSet_CUnion ( sem_ConstructorSet set1_ ) ( sem_ConstructorSet set2_ )
sem_ConstructorSet ( CDifference set1_ set2_ ) = sem_ConstructorSet_CDifference ( sem_ConstructorSet set1_ ) ( sem_ConstructorSet set2_ )
sem_ConstructorSet ( CAll  ) = sem_ConstructorSet_CAll 

-- semantic domain
newtype T_ConstructorSet  = T_ConstructorSet {
                                             attach_T_ConstructorSet :: Identity (T_ConstructorSet_s14 )
                                             }
newtype T_ConstructorSet_s14  = C_ConstructorSet_s14 {
                                                     inv_ConstructorSet_s14 :: (T_ConstructorSet_v13 )
                                                     }
data T_ConstructorSet_s15  = C_ConstructorSet_s15
type T_ConstructorSet_v13  = (T_ConstructorSet_vIn13 ) -> (T_ConstructorSet_vOut13 )
data T_ConstructorSet_vIn13  = T_ConstructorSet_vIn13 
data T_ConstructorSet_vOut13  = T_ConstructorSet_vOut13 (Set ConstructorIdent) ((Set ConstructorIdent->Set ConstructorIdent)) (Seq Error)
{-# NOINLINE sem_ConstructorSet_CName #-}
sem_ConstructorSet_CName :: (ConstructorIdent) -> T_ConstructorSet 
sem_ConstructorSet_CName arg_name_ = T_ConstructorSet (return st14) where
   {-# NOINLINE st14 #-}
   st14 = let
      v13 :: T_ConstructorSet_v13 
      v13 = \ (T_ConstructorSet_vIn13 ) -> ( let
         _lhsOcollectedConstructorNames :: Set ConstructorIdent
         _lhsOcollectedConstructorNames = rule89 arg_name_
         _lhsOconstructors :: (Set ConstructorIdent->Set ConstructorIdent)
         _lhsOconstructors = rule90 arg_name_
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule91  ()
         __result_ = T_ConstructorSet_vOut13 _lhsOcollectedConstructorNames _lhsOconstructors _lhsOerrors
         in __result_ )
     in C_ConstructorSet_s14 v13
   {-# INLINE rule89 #-}
   {-# LINE 614 "./src-ag/Transform.ag" #-}
   rule89 = \ name_ ->
                                            {-# LINE 614 "./src-ag/Transform.ag" #-}
                                            Set.singleton name_
                                            {-# LINE 1404 "dist/build/Transform.hs"#-}
   {-# INLINE rule90 #-}
   {-# LINE 777 "./src-ag/Transform.ag" #-}
   rule90 = \ name_ ->
                                     {-# LINE 777 "./src-ag/Transform.ag" #-}
                                     \_  -> Set.singleton name_
                                     {-# LINE 1410 "dist/build/Transform.hs"#-}
   {-# INLINE rule91 #-}
   rule91 = \  (_ :: ()) ->
     Seq.empty
{-# NOINLINE sem_ConstructorSet_CUnion #-}
sem_ConstructorSet_CUnion :: T_ConstructorSet  -> T_ConstructorSet  -> T_ConstructorSet 
sem_ConstructorSet_CUnion arg_set1_ arg_set2_ = T_ConstructorSet (return st14) where
   {-# NOINLINE st14 #-}
   st14 = let
      v13 :: T_ConstructorSet_v13 
      v13 = \ (T_ConstructorSet_vIn13 ) -> ( let
         _set1X14 = Control.Monad.Identity.runIdentity (attach_T_ConstructorSet (arg_set1_))
         _set2X14 = Control.Monad.Identity.runIdentity (attach_T_ConstructorSet (arg_set2_))
         (T_ConstructorSet_vOut13 _set1IcollectedConstructorNames _set1Iconstructors _set1Ierrors) = inv_ConstructorSet_s14 _set1X14 (T_ConstructorSet_vIn13 )
         (T_ConstructorSet_vOut13 _set2IcollectedConstructorNames _set2Iconstructors _set2Ierrors) = inv_ConstructorSet_s14 _set2X14 (T_ConstructorSet_vIn13 )
         _lhsOconstructors :: (Set ConstructorIdent->Set ConstructorIdent)
         _lhsOconstructors = rule92 _set1Iconstructors _set2Iconstructors
         _lhsOcollectedConstructorNames :: Set ConstructorIdent
         _lhsOcollectedConstructorNames = rule93 _set1IcollectedConstructorNames _set2IcollectedConstructorNames
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule94 _set1Ierrors _set2Ierrors
         __result_ = T_ConstructorSet_vOut13 _lhsOcollectedConstructorNames _lhsOconstructors _lhsOerrors
         in __result_ )
     in C_ConstructorSet_s14 v13
   {-# INLINE rule92 #-}
   {-# LINE 778 "./src-ag/Transform.ag" #-}
   rule92 = \ ((_set1Iconstructors) :: (Set ConstructorIdent->Set ConstructorIdent)) ((_set2Iconstructors) :: (Set ConstructorIdent->Set ConstructorIdent)) ->
                                     {-# LINE 778 "./src-ag/Transform.ag" #-}
                                     \ds -> _set1Iconstructors ds `Set.union`      _set2Iconstructors ds
                                     {-# LINE 1439 "dist/build/Transform.hs"#-}
   {-# INLINE rule93 #-}
   rule93 = \ ((_set1IcollectedConstructorNames) :: Set ConstructorIdent) ((_set2IcollectedConstructorNames) :: Set ConstructorIdent) ->
     _set1IcollectedConstructorNames `Set.union` _set2IcollectedConstructorNames
   {-# INLINE rule94 #-}
   rule94 = \ ((_set1Ierrors) :: Seq Error) ((_set2Ierrors) :: Seq Error) ->
     _set1Ierrors Seq.>< _set2Ierrors
{-# NOINLINE sem_ConstructorSet_CDifference #-}
sem_ConstructorSet_CDifference :: T_ConstructorSet  -> T_ConstructorSet  -> T_ConstructorSet 
sem_ConstructorSet_CDifference arg_set1_ arg_set2_ = T_ConstructorSet (return st14) where
   {-# NOINLINE st14 #-}
   st14 = let
      v13 :: T_ConstructorSet_v13 
      v13 = \ (T_ConstructorSet_vIn13 ) -> ( let
         _set1X14 = Control.Monad.Identity.runIdentity (attach_T_ConstructorSet (arg_set1_))
         _set2X14 = Control.Monad.Identity.runIdentity (attach_T_ConstructorSet (arg_set2_))
         (T_ConstructorSet_vOut13 _set1IcollectedConstructorNames _set1Iconstructors _set1Ierrors) = inv_ConstructorSet_s14 _set1X14 (T_ConstructorSet_vIn13 )
         (T_ConstructorSet_vOut13 _set2IcollectedConstructorNames _set2Iconstructors _set2Ierrors) = inv_ConstructorSet_s14 _set2X14 (T_ConstructorSet_vIn13 )
         _lhsOconstructors :: (Set ConstructorIdent->Set ConstructorIdent)
         _lhsOconstructors = rule95 _set1Iconstructors _set2Iconstructors
         _lhsOcollectedConstructorNames :: Set ConstructorIdent
         _lhsOcollectedConstructorNames = rule96 _set1IcollectedConstructorNames _set2IcollectedConstructorNames
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule97 _set1Ierrors _set2Ierrors
         __result_ = T_ConstructorSet_vOut13 _lhsOcollectedConstructorNames _lhsOconstructors _lhsOerrors
         in __result_ )
     in C_ConstructorSet_s14 v13
   {-# INLINE rule95 #-}
   {-# LINE 779 "./src-ag/Transform.ag" #-}
   rule95 = \ ((_set1Iconstructors) :: (Set ConstructorIdent->Set ConstructorIdent)) ((_set2Iconstructors) :: (Set ConstructorIdent->Set ConstructorIdent)) ->
                                     {-# LINE 779 "./src-ag/Transform.ag" #-}
                                     \ds -> _set1Iconstructors ds `Set.difference` _set2Iconstructors ds
                                     {-# LINE 1471 "dist/build/Transform.hs"#-}
   {-# INLINE rule96 #-}
   rule96 = \ ((_set1IcollectedConstructorNames) :: Set ConstructorIdent) ((_set2IcollectedConstructorNames) :: Set ConstructorIdent) ->
     _set1IcollectedConstructorNames `Set.union` _set2IcollectedConstructorNames
   {-# INLINE rule97 #-}
   rule97 = \ ((_set1Ierrors) :: Seq Error) ((_set2Ierrors) :: Seq Error) ->
     _set1Ierrors Seq.>< _set2Ierrors
{-# NOINLINE sem_ConstructorSet_CAll #-}
sem_ConstructorSet_CAll ::  T_ConstructorSet 
sem_ConstructorSet_CAll  = T_ConstructorSet (return st14) where
   {-# NOINLINE st14 #-}
   st14 = let
      v13 :: T_ConstructorSet_v13 
      v13 = \ (T_ConstructorSet_vIn13 ) -> ( let
         _lhsOconstructors :: (Set ConstructorIdent->Set ConstructorIdent)
         _lhsOconstructors = rule98  ()
         _lhsOcollectedConstructorNames :: Set ConstructorIdent
         _lhsOcollectedConstructorNames = rule99  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule100  ()
         __result_ = T_ConstructorSet_vOut13 _lhsOcollectedConstructorNames _lhsOconstructors _lhsOerrors
         in __result_ )
     in C_ConstructorSet_s14 v13
   {-# INLINE rule98 #-}
   {-# LINE 780 "./src-ag/Transform.ag" #-}
   rule98 = \  (_ :: ()) ->
                                     {-# LINE 780 "./src-ag/Transform.ag" #-}
                                     \ds -> ds
                                     {-# LINE 1499 "dist/build/Transform.hs"#-}
   {-# INLINE rule99 #-}
   rule99 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule100 #-}
   rule100 = \  (_ :: ()) ->
     Seq.empty

-- Elem --------------------------------------------------------
-- wrapper
data Inh_Elem  = Inh_Elem { allAttrDecls_Inh_Elem :: (Map NontermIdent (Attributes, Attributes)), allAttrs_Inh_Elem :: (Map NontermIdent (Attributes, Attributes)), allConstructors_Inh_Elem :: (Map NontermIdent (Set ConstructorIdent)), allFields_Inh_Elem :: (DataTypes), allNonterminals_Inh_Elem :: (Set NontermIdent), attrDecls_Inh_Elem :: (Map NontermIdent (Attributes, Attributes)), attrs_Inh_Elem :: (Map NontermIdent (Attributes, Attributes)), defSets_Inh_Elem :: (Map Identifier (Set NontermIdent,Set Identifier)), definedSets_Inh_Elem :: (DefinedSets), options_Inh_Elem :: (Options) }
data Syn_Elem  = Syn_Elem { attrDecls_Syn_Elem :: (Map NontermIdent (Attributes, Attributes)), attrOrderCollect_Syn_Elem :: (AttrOrderMap), attrs_Syn_Elem :: (Map NontermIdent (Attributes, Attributes)), blocks_Syn_Elem :: (Blocks), collectedArounds_Syn_Elem :: ([ (NontermIdent, ConstructorIdent, [AroundInfo])  ]), collectedAugments_Syn_Elem :: ([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]), collectedConParams_Syn_Elem :: ([(NontermIdent, ConstructorIdent, Set Identifier)]), collectedConstraints_Syn_Elem :: ([(NontermIdent, ConstructorIdent, [Type])]), collectedConstructorsMap_Syn_Elem :: (Map NontermIdent (Set ConstructorIdent)), collectedFields_Syn_Elem :: ([(NontermIdent, ConstructorIdent, FieldMap)]), collectedInsts_Syn_Elem :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ]), collectedMacros_Syn_Elem :: ([(NontermIdent, ConstructorIdent, MaybeMacro)]), collectedMerges_Syn_Elem :: ([ (NontermIdent, ConstructorIdent, [MergeInfo])   ]), collectedNames_Syn_Elem :: (Set Identifier), collectedRules_Syn_Elem :: ([ (NontermIdent, ConstructorIdent, RuleInfo)]), collectedSetNames_Syn_Elem :: (Set Identifier), collectedSigs_Syn_Elem :: ([ (NontermIdent, ConstructorIdent, SigInfo) ]), collectedUniques_Syn_Elem :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]), ctxCollect_Syn_Elem :: (ContextMap), defSets_Syn_Elem :: (Map Identifier (Set NontermIdent,Set Identifier)), derivings_Syn_Elem :: (Derivings), errors_Syn_Elem :: (Seq Error), moduleDecl_Syn_Elem :: (Maybe (String,String,String)), paramsCollect_Syn_Elem :: (ParamMap), pragmas_Syn_Elem :: (Options -> Options), quantCollect_Syn_Elem :: (QuantMap), semPragmasCollect_Syn_Elem :: (PragmaMap), typeSyns_Syn_Elem :: (TypeSyns), useMap_Syn_Elem :: (Map NontermIdent (Map Identifier (String,String,String))), wrappers_Syn_Elem :: (Set NontermIdent) }
{-# INLINABLE wrap_Elem #-}
wrap_Elem :: T_Elem  -> Inh_Elem  -> (Syn_Elem )
wrap_Elem (T_Elem act) (Inh_Elem _lhsIallAttrDecls _lhsIallAttrs _lhsIallConstructors _lhsIallFields _lhsIallNonterminals _lhsIattrDecls _lhsIattrs _lhsIdefSets _lhsIdefinedSets _lhsIoptions) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Elem_vIn16 _lhsIallAttrDecls _lhsIallAttrs _lhsIallConstructors _lhsIallFields _lhsIallNonterminals _lhsIattrDecls _lhsIattrs _lhsIdefSets _lhsIdefinedSets _lhsIoptions
        (T_Elem_vOut16 _lhsOattrDecls _lhsOattrOrderCollect _lhsOattrs _lhsOblocks _lhsOcollectedArounds _lhsOcollectedAugments _lhsOcollectedConParams _lhsOcollectedConstraints _lhsOcollectedConstructorsMap _lhsOcollectedFields _lhsOcollectedInsts _lhsOcollectedMacros _lhsOcollectedMerges _lhsOcollectedNames _lhsOcollectedRules _lhsOcollectedSetNames _lhsOcollectedSigs _lhsOcollectedUniques _lhsOctxCollect _lhsOdefSets _lhsOderivings _lhsOerrors _lhsOmoduleDecl _lhsOparamsCollect _lhsOpragmas _lhsOquantCollect _lhsOsemPragmasCollect _lhsOtypeSyns _lhsOuseMap _lhsOwrappers) <- return (inv_Elem_s17 sem arg)
        return (Syn_Elem _lhsOattrDecls _lhsOattrOrderCollect _lhsOattrs _lhsOblocks _lhsOcollectedArounds _lhsOcollectedAugments _lhsOcollectedConParams _lhsOcollectedConstraints _lhsOcollectedConstructorsMap _lhsOcollectedFields _lhsOcollectedInsts _lhsOcollectedMacros _lhsOcollectedMerges _lhsOcollectedNames _lhsOcollectedRules _lhsOcollectedSetNames _lhsOcollectedSigs _lhsOcollectedUniques _lhsOctxCollect _lhsOdefSets _lhsOderivings _lhsOerrors _lhsOmoduleDecl _lhsOparamsCollect _lhsOpragmas _lhsOquantCollect _lhsOsemPragmasCollect _lhsOtypeSyns _lhsOuseMap _lhsOwrappers)
   )

-- cata
{-# NOINLINE sem_Elem #-}
sem_Elem :: Elem  -> T_Elem 
sem_Elem ( Data pos_ ctx_ names_ params_ attrs_ alts_ ext_ ) = sem_Elem_Data pos_ ctx_ ( sem_NontSet names_ ) params_ ( sem_Attrs attrs_ ) ( sem_Alts alts_ ) ext_
sem_Elem ( Type pos_ ctx_ name_ params_ type_ ) = sem_Elem_Type pos_ ctx_ name_ params_ type_
sem_Elem ( Attr pos_ ctx_ names_ quants_ attrs_ ) = sem_Elem_Attr pos_ ctx_ ( sem_NontSet names_ ) quants_ ( sem_Attrs attrs_ )
sem_Elem ( Sem pos_ ctx_ names_ attrs_ quants_ alts_ ) = sem_Elem_Sem pos_ ctx_ ( sem_NontSet names_ ) ( sem_Attrs attrs_ ) quants_ ( sem_SemAlts alts_ )
sem_Elem ( Txt pos_ kind_ mbNt_ lines_ ) = sem_Elem_Txt pos_ kind_ mbNt_ lines_
sem_Elem ( Set pos_ name_ merge_ set_ ) = sem_Elem_Set pos_ name_ merge_ ( sem_NontSet set_ )
sem_Elem ( Deriving pos_ set_ classes_ ) = sem_Elem_Deriving pos_ ( sem_NontSet set_ ) classes_
sem_Elem ( Wrapper pos_ set_ ) = sem_Elem_Wrapper pos_ ( sem_NontSet set_ )
sem_Elem ( Nocatas pos_ set_ ) = sem_Elem_Nocatas pos_ ( sem_NontSet set_ )
sem_Elem ( Pragma pos_ names_ ) = sem_Elem_Pragma pos_ names_
sem_Elem ( Module pos_ name_ exports_ imports_ ) = sem_Elem_Module pos_ name_ exports_ imports_

-- semantic domain
newtype T_Elem  = T_Elem {
                         attach_T_Elem :: Identity (T_Elem_s17 )
                         }
newtype T_Elem_s17  = C_Elem_s17 {
                                 inv_Elem_s17 :: (T_Elem_v16 )
                                 }
data T_Elem_s18  = C_Elem_s18
type T_Elem_v16  = (T_Elem_vIn16 ) -> (T_Elem_vOut16 )
data T_Elem_vIn16  = T_Elem_vIn16 (Map NontermIdent (Attributes, Attributes)) (Map NontermIdent (Attributes, Attributes)) (Map NontermIdent (Set ConstructorIdent)) (DataTypes) (Set NontermIdent) (Map NontermIdent (Attributes, Attributes)) (Map NontermIdent (Attributes, Attributes)) (Map Identifier (Set NontermIdent,Set Identifier)) (DefinedSets) (Options)
data T_Elem_vOut16  = T_Elem_vOut16 (Map NontermIdent (Attributes, Attributes)) (AttrOrderMap) (Map NontermIdent (Attributes, Attributes)) (Blocks) ([ (NontermIdent, ConstructorIdent, [AroundInfo])  ]) ([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]) ([(NontermIdent, ConstructorIdent, Set Identifier)]) ([(NontermIdent, ConstructorIdent, [Type])]) (Map NontermIdent (Set ConstructorIdent)) ([(NontermIdent, ConstructorIdent, FieldMap)]) ([ (NontermIdent, ConstructorIdent, [Identifier]) ]) ([(NontermIdent, ConstructorIdent, MaybeMacro)]) ([ (NontermIdent, ConstructorIdent, [MergeInfo])   ]) (Set Identifier) ([ (NontermIdent, ConstructorIdent, RuleInfo)]) (Set Identifier) ([ (NontermIdent, ConstructorIdent, SigInfo) ]) ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]) (ContextMap) (Map Identifier (Set NontermIdent,Set Identifier)) (Derivings) (Seq Error) (Maybe (String,String,String)) (ParamMap) (Options -> Options) (QuantMap) (PragmaMap) (TypeSyns) (Map NontermIdent (Map Identifier (String,String,String))) (Set NontermIdent)
{-# NOINLINE sem_Elem_Data #-}
sem_Elem_Data :: (Pos) -> (ClassContext) -> T_NontSet  -> ([Identifier]) -> T_Attrs  -> T_Alts  -> (Bool) -> T_Elem 
sem_Elem_Data _ arg_ctx_ arg_names_ arg_params_ arg_attrs_ arg_alts_ _ = T_Elem (return st17) where
   {-# NOINLINE st17 #-}
   st17 = let
      v16 :: T_Elem_v16 
      v16 = \ (T_Elem_vIn16 _lhsIallAttrDecls _lhsIallAttrs _lhsIallConstructors _lhsIallFields _lhsIallNonterminals _lhsIattrDecls _lhsIattrs _lhsIdefSets _lhsIdefinedSets _lhsIoptions) -> ( let
         _namesX29 = Control.Monad.Identity.runIdentity (attach_T_NontSet (arg_names_))
         _attrsX11 = Control.Monad.Identity.runIdentity (attach_T_Attrs (arg_attrs_))
         _altsX8 = Control.Monad.Identity.runIdentity (attach_T_Alts (arg_alts_))
         (T_NontSet_vOut28 _namesIcollectedNames _namesIerrors _namesInontSet) = inv_NontSet_s29 _namesX29 (T_NontSet_vIn28 _namesOallFields _namesOallNonterminals _namesOdefinedSets)
         (T_Attrs_vOut10 _attrsIattrDecls _attrsIattrs _attrsIerrors _attrsIuseMap) = inv_Attrs_s11 _attrsX11 (T_Attrs_vIn10 _attrsOallFields _attrsOallNonterminals _attrsOattrDecls _attrsOattrs _attrsOnts _attrsOoptions)
         (T_Alts_vOut7 _altsIcollectedConParams _altsIcollectedConstraints _altsIcollectedConstructorNames _altsIcollectedFields _altsIcollectedMacros) = inv_Alts_s8 _altsX8 (T_Alts_vIn7 _altsOallConstructors _altsOallNonterminals _altsOnts)
         _altsOnts = rule101 _namesInontSet
         _lhsOcollectedConstructorsMap :: Map NontermIdent (Set ConstructorIdent)
         _lhsOcollectedConstructorsMap = rule102 _altsIcollectedConstructorNames _namesInontSet
         _lhsOparamsCollect :: ParamMap
         _lhsOparamsCollect = rule103 _namesInontSet arg_params_
         _lhsOctxCollect :: ContextMap
         _lhsOctxCollect = rule104 _namesInontSet arg_ctx_
         _attrsOnts = rule105 _namesInontSet
         _lhsOattrOrderCollect :: AttrOrderMap
         _lhsOattrOrderCollect = rule106  ()
         _lhsOblocks :: Blocks
         _lhsOblocks = rule107  ()
         _lhsOcollectedArounds :: [ (NontermIdent, ConstructorIdent, [AroundInfo])  ]
         _lhsOcollectedArounds = rule108  ()
         _lhsOcollectedAugments :: [ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]
         _lhsOcollectedAugments = rule109  ()
         _lhsOcollectedConParams :: [(NontermIdent, ConstructorIdent, Set Identifier)]
         _lhsOcollectedConParams = rule110 _altsIcollectedConParams
         _lhsOcollectedConstraints :: [(NontermIdent, ConstructorIdent, [Type])]
         _lhsOcollectedConstraints = rule111 _altsIcollectedConstraints
         _lhsOcollectedFields :: [(NontermIdent, ConstructorIdent, FieldMap)]
         _lhsOcollectedFields = rule112 _altsIcollectedFields
         _lhsOcollectedInsts :: [ (NontermIdent, ConstructorIdent, [Identifier]) ]
         _lhsOcollectedInsts = rule113  ()
         _lhsOcollectedMacros :: [(NontermIdent, ConstructorIdent, MaybeMacro)]
         _lhsOcollectedMacros = rule114 _altsIcollectedMacros
         _lhsOcollectedMerges :: [ (NontermIdent, ConstructorIdent, [MergeInfo])   ]
         _lhsOcollectedMerges = rule115  ()
         _lhsOcollectedNames :: Set Identifier
         _lhsOcollectedNames = rule116 _namesIcollectedNames
         _lhsOcollectedRules :: [ (NontermIdent, ConstructorIdent, RuleInfo)]
         _lhsOcollectedRules = rule117  ()
         _lhsOcollectedSetNames :: Set Identifier
         _lhsOcollectedSetNames = rule118  ()
         _lhsOcollectedSigs :: [ (NontermIdent, ConstructorIdent, SigInfo) ]
         _lhsOcollectedSigs = rule119  ()
         _lhsOcollectedUniques :: [ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]
         _lhsOcollectedUniques = rule120  ()
         _lhsOderivings :: Derivings
         _lhsOderivings = rule121  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule122 _attrsIerrors _namesIerrors
         _lhsOmoduleDecl :: Maybe (String,String,String)
         _lhsOmoduleDecl = rule123  ()
         _lhsOpragmas :: Options -> Options
         _lhsOpragmas = rule124  ()
         _lhsOquantCollect :: QuantMap
         _lhsOquantCollect = rule125  ()
         _lhsOsemPragmasCollect :: PragmaMap
         _lhsOsemPragmasCollect = rule126  ()
         _lhsOtypeSyns :: TypeSyns
         _lhsOtypeSyns = rule127  ()
         _lhsOuseMap :: Map NontermIdent (Map Identifier (String,String,String))
         _lhsOuseMap = rule128 _attrsIuseMap
         _lhsOwrappers :: Set NontermIdent
         _lhsOwrappers = rule129  ()
         _lhsOattrDecls :: Map NontermIdent (Attributes, Attributes)
         _lhsOattrDecls = rule130 _attrsIattrDecls
         _lhsOattrs :: Map NontermIdent (Attributes, Attributes)
         _lhsOattrs = rule131 _attrsIattrs
         _lhsOdefSets :: Map Identifier (Set NontermIdent,Set Identifier)
         _lhsOdefSets = rule132 _lhsIdefSets
         _namesOallFields = rule133 _lhsIallFields
         _namesOallNonterminals = rule134 _lhsIallNonterminals
         _namesOdefinedSets = rule135 _lhsIdefinedSets
         _attrsOallFields = rule136 _lhsIallFields
         _attrsOallNonterminals = rule137 _lhsIallNonterminals
         _attrsOattrDecls = rule138 _lhsIattrDecls
         _attrsOattrs = rule139 _lhsIattrs
         _attrsOoptions = rule140 _lhsIoptions
         _altsOallConstructors = rule141 _lhsIallConstructors
         _altsOallNonterminals = rule142 _lhsIallNonterminals
         __result_ = T_Elem_vOut16 _lhsOattrDecls _lhsOattrOrderCollect _lhsOattrs _lhsOblocks _lhsOcollectedArounds _lhsOcollectedAugments _lhsOcollectedConParams _lhsOcollectedConstraints _lhsOcollectedConstructorsMap _lhsOcollectedFields _lhsOcollectedInsts _lhsOcollectedMacros _lhsOcollectedMerges _lhsOcollectedNames _lhsOcollectedRules _lhsOcollectedSetNames _lhsOcollectedSigs _lhsOcollectedUniques _lhsOctxCollect _lhsOdefSets _lhsOderivings _lhsOerrors _lhsOmoduleDecl _lhsOparamsCollect _lhsOpragmas _lhsOquantCollect _lhsOsemPragmasCollect _lhsOtypeSyns _lhsOuseMap _lhsOwrappers
         in __result_ )
     in C_Elem_s17 v16
   {-# INLINE rule101 #-}
   {-# LINE 176 "./src-ag/Transform.ag" #-}
   rule101 = \ ((_namesInontSet) :: Set NontermIdent) ->
                      {-# LINE 176 "./src-ag/Transform.ag" #-}
                      _namesInontSet
                      {-# LINE 1640 "dist/build/Transform.hs"#-}
   {-# INLINE rule102 #-}
   {-# LINE 620 "./src-ag/Transform.ag" #-}
   rule102 = \ ((_altsIcollectedConstructorNames) :: Set ConstructorIdent) ((_namesInontSet) :: Set NontermIdent) ->
                                           {-# LINE 620 "./src-ag/Transform.ag" #-}
                                           Map.fromList
                                           [ (n, _altsIcollectedConstructorNames)
                                           | n <- Set.toList _namesInontSet
                                           ]
                                           {-# LINE 1649 "dist/build/Transform.hs"#-}
   {-# INLINE rule103 #-}
   {-# LINE 947 "./src-ag/Transform.ag" #-}
   rule103 = \ ((_namesInontSet) :: Set NontermIdent) params_ ->
                            {-# LINE 947 "./src-ag/Transform.ag" #-}
                            if null params_
                            then Map.empty
                            else Map.fromList [(nt, params_) | nt <- Set.toList _namesInontSet]
                            {-# LINE 1657 "dist/build/Transform.hs"#-}
   {-# INLINE rule104 #-}
   {-# LINE 970 "./src-ag/Transform.ag" #-}
   rule104 = \ ((_namesInontSet) :: Set NontermIdent) ctx_ ->
                         {-# LINE 970 "./src-ag/Transform.ag" #-}
                         if null ctx_
                         then Map.empty
                         else Map.fromList [(nt, ctx_) | nt <- Set.toList _namesInontSet]
                         {-# LINE 1665 "dist/build/Transform.hs"#-}
   {-# INLINE rule105 #-}
   {-# LINE 1032 "./src-ag/Transform.ag" #-}
   rule105 = \ ((_namesInontSet) :: Set NontermIdent) ->
                       {-# LINE 1032 "./src-ag/Transform.ag" #-}
                       _namesInontSet
                       {-# LINE 1671 "dist/build/Transform.hs"#-}
   {-# INLINE rule106 #-}
   rule106 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule107 #-}
   rule107 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule108 #-}
   rule108 = \  (_ :: ()) ->
     []
   {-# INLINE rule109 #-}
   rule109 = \  (_ :: ()) ->
     []
   {-# INLINE rule110 #-}
   rule110 = \ ((_altsIcollectedConParams) :: [(NontermIdent, ConstructorIdent, Set Identifier)]) ->
     _altsIcollectedConParams
   {-# INLINE rule111 #-}
   rule111 = \ ((_altsIcollectedConstraints) :: [(NontermIdent, ConstructorIdent, [Type])]) ->
     _altsIcollectedConstraints
   {-# INLINE rule112 #-}
   rule112 = \ ((_altsIcollectedFields) :: [(NontermIdent, ConstructorIdent, FieldMap)]) ->
     _altsIcollectedFields
   {-# INLINE rule113 #-}
   rule113 = \  (_ :: ()) ->
     []
   {-# INLINE rule114 #-}
   rule114 = \ ((_altsIcollectedMacros) :: [(NontermIdent, ConstructorIdent, MaybeMacro)]) ->
     _altsIcollectedMacros
   {-# INLINE rule115 #-}
   rule115 = \  (_ :: ()) ->
     []
   {-# INLINE rule116 #-}
   rule116 = \ ((_namesIcollectedNames) :: Set Identifier) ->
     _namesIcollectedNames
   {-# INLINE rule117 #-}
   rule117 = \  (_ :: ()) ->
     []
   {-# INLINE rule118 #-}
   rule118 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule119 #-}
   rule119 = \  (_ :: ()) ->
     []
   {-# INLINE rule120 #-}
   rule120 = \  (_ :: ()) ->
     []
   {-# INLINE rule121 #-}
   rule121 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule122 #-}
   rule122 = \ ((_attrsIerrors) :: Seq Error) ((_namesIerrors) :: Seq Error) ->
     _namesIerrors Seq.>< _attrsIerrors
   {-# INLINE rule123 #-}
   rule123 = \  (_ :: ()) ->
     mzero
   {-# INLINE rule124 #-}
   rule124 = \  (_ :: ()) ->
     id
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
   rule128 = \ ((_attrsIuseMap) :: Map NontermIdent (Map Identifier (String,String,String))) ->
     _attrsIuseMap
   {-# INLINE rule129 #-}
   rule129 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule130 #-}
   rule130 = \ ((_attrsIattrDecls) :: Map NontermIdent (Attributes, Attributes)) ->
     _attrsIattrDecls
   {-# INLINE rule131 #-}
   rule131 = \ ((_attrsIattrs) :: Map NontermIdent (Attributes, Attributes)) ->
     _attrsIattrs
   {-# INLINE rule132 #-}
   rule132 = \ ((_lhsIdefSets) :: Map Identifier (Set NontermIdent,Set Identifier)) ->
     _lhsIdefSets
   {-# INLINE rule133 #-}
   rule133 = \ ((_lhsIallFields) :: DataTypes) ->
     _lhsIallFields
   {-# INLINE rule134 #-}
   rule134 = \ ((_lhsIallNonterminals) :: Set NontermIdent) ->
     _lhsIallNonterminals
   {-# INLINE rule135 #-}
   rule135 = \ ((_lhsIdefinedSets) :: DefinedSets) ->
     _lhsIdefinedSets
   {-# INLINE rule136 #-}
   rule136 = \ ((_lhsIallFields) :: DataTypes) ->
     _lhsIallFields
   {-# INLINE rule137 #-}
   rule137 = \ ((_lhsIallNonterminals) :: Set NontermIdent) ->
     _lhsIallNonterminals
   {-# INLINE rule138 #-}
   rule138 = \ ((_lhsIattrDecls) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIattrDecls
   {-# INLINE rule139 #-}
   rule139 = \ ((_lhsIattrs) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIattrs
   {-# INLINE rule140 #-}
   rule140 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule141 #-}
   rule141 = \ ((_lhsIallConstructors) :: Map NontermIdent (Set ConstructorIdent)) ->
     _lhsIallConstructors
   {-# INLINE rule142 #-}
   rule142 = \ ((_lhsIallNonterminals) :: Set NontermIdent) ->
     _lhsIallNonterminals
{-# NOINLINE sem_Elem_Type #-}
sem_Elem_Type :: (Pos) -> (ClassContext) -> (NontermIdent) -> ([Identifier]) -> (ComplexType) -> T_Elem 
sem_Elem_Type arg_pos_ arg_ctx_ arg_name_ arg_params_ arg_type_ = T_Elem (return st17) where
   {-# NOINLINE st17 #-}
   st17 = let
      v16 :: T_Elem_v16 
      v16 = \ (T_Elem_vIn16 _lhsIallAttrDecls _lhsIallAttrs _lhsIallConstructors _lhsIallFields _lhsIallNonterminals _lhsIattrDecls _lhsIattrs _lhsIdefSets _lhsIdefinedSets _lhsIoptions) -> ( let
         _lhsOcollectedFields :: [(NontermIdent, ConstructorIdent, FieldMap)]
         _lhsOcollectedFields = rule143 _expanded arg_name_
         _lhsOcollectedNames :: Set Identifier
         _lhsOcollectedNames = rule144 arg_name_
         _expanded = rule145 _argType arg_name_ arg_params_ arg_pos_
         _argType = rule146 _lhsIallNonterminals arg_type_
         _lhsOtypeSyns :: TypeSyns
         _lhsOtypeSyns = rule147 _argType arg_name_
         _lhsOparamsCollect :: ParamMap
         _lhsOparamsCollect = rule148 arg_name_ arg_params_
         _lhsOctxCollect :: ContextMap
         _lhsOctxCollect = rule149 arg_ctx_ arg_name_
         _lhsOattrOrderCollect :: AttrOrderMap
         _lhsOattrOrderCollect = rule150  ()
         _lhsOblocks :: Blocks
         _lhsOblocks = rule151  ()
         _lhsOcollectedArounds :: [ (NontermIdent, ConstructorIdent, [AroundInfo])  ]
         _lhsOcollectedArounds = rule152  ()
         _lhsOcollectedAugments :: [ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]
         _lhsOcollectedAugments = rule153  ()
         _lhsOcollectedConParams :: [(NontermIdent, ConstructorIdent, Set Identifier)]
         _lhsOcollectedConParams = rule154  ()
         _lhsOcollectedConstraints :: [(NontermIdent, ConstructorIdent, [Type])]
         _lhsOcollectedConstraints = rule155  ()
         _lhsOcollectedConstructorsMap :: Map NontermIdent (Set ConstructorIdent)
         _lhsOcollectedConstructorsMap = rule156  ()
         _lhsOcollectedInsts :: [ (NontermIdent, ConstructorIdent, [Identifier]) ]
         _lhsOcollectedInsts = rule157  ()
         _lhsOcollectedMacros :: [(NontermIdent, ConstructorIdent, MaybeMacro)]
         _lhsOcollectedMacros = rule158  ()
         _lhsOcollectedMerges :: [ (NontermIdent, ConstructorIdent, [MergeInfo])   ]
         _lhsOcollectedMerges = rule159  ()
         _lhsOcollectedRules :: [ (NontermIdent, ConstructorIdent, RuleInfo)]
         _lhsOcollectedRules = rule160  ()
         _lhsOcollectedSetNames :: Set Identifier
         _lhsOcollectedSetNames = rule161  ()
         _lhsOcollectedSigs :: [ (NontermIdent, ConstructorIdent, SigInfo) ]
         _lhsOcollectedSigs = rule162  ()
         _lhsOcollectedUniques :: [ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]
         _lhsOcollectedUniques = rule163  ()
         _lhsOderivings :: Derivings
         _lhsOderivings = rule164  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule165  ()
         _lhsOmoduleDecl :: Maybe (String,String,String)
         _lhsOmoduleDecl = rule166  ()
         _lhsOpragmas :: Options -> Options
         _lhsOpragmas = rule167  ()
         _lhsOquantCollect :: QuantMap
         _lhsOquantCollect = rule168  ()
         _lhsOsemPragmasCollect :: PragmaMap
         _lhsOsemPragmasCollect = rule169  ()
         _lhsOuseMap :: Map NontermIdent (Map Identifier (String,String,String))
         _lhsOuseMap = rule170  ()
         _lhsOwrappers :: Set NontermIdent
         _lhsOwrappers = rule171  ()
         _lhsOattrDecls :: Map NontermIdent (Attributes, Attributes)
         _lhsOattrDecls = rule172 _lhsIattrDecls
         _lhsOattrs :: Map NontermIdent (Attributes, Attributes)
         _lhsOattrs = rule173 _lhsIattrs
         _lhsOdefSets :: Map Identifier (Set NontermIdent,Set Identifier)
         _lhsOdefSets = rule174 _lhsIdefSets
         __result_ = T_Elem_vOut16 _lhsOattrDecls _lhsOattrOrderCollect _lhsOattrs _lhsOblocks _lhsOcollectedArounds _lhsOcollectedAugments _lhsOcollectedConParams _lhsOcollectedConstraints _lhsOcollectedConstructorsMap _lhsOcollectedFields _lhsOcollectedInsts _lhsOcollectedMacros _lhsOcollectedMerges _lhsOcollectedNames _lhsOcollectedRules _lhsOcollectedSetNames _lhsOcollectedSigs _lhsOcollectedUniques _lhsOctxCollect _lhsOdefSets _lhsOderivings _lhsOerrors _lhsOmoduleDecl _lhsOparamsCollect _lhsOpragmas _lhsOquantCollect _lhsOsemPragmasCollect _lhsOtypeSyns _lhsOuseMap _lhsOwrappers
         in __result_ )
     in C_Elem_s17 v16
   {-# INLINE rule143 #-}
   {-# LINE 254 "./src-ag/Transform.ag" #-}
   rule143 = \ _expanded name_ ->
                                 {-# LINE 254 "./src-ag/Transform.ag" #-}
                                 map (\(x,y)->(name_, x, y)) _expanded
                                 {-# LINE 1860 "dist/build/Transform.hs"#-}
   {-# INLINE rule144 #-}
   {-# LINE 600 "./src-ag/Transform.ag" #-}
   rule144 = \ name_ ->
                                 {-# LINE 600 "./src-ag/Transform.ag" #-}
                                 Set.singleton name_
                                 {-# LINE 1866 "dist/build/Transform.hs"#-}
   {-# INLINE rule145 #-}
   {-# LINE 654 "./src-ag/Transform.ag" #-}
   rule145 = \ _argType name_ params_ pos_ ->
                                 {-# LINE 654 "./src-ag/Transform.ag" #-}
                                 case _argType of
                                         List tp -> [(Ident "Cons" pos_, [(Ident "hd" pos_, tp)
                                                                         ,(Ident "tl" pos_, NT name_ (map getName params_) False)
                                                                         ]
                                                     )
                                                    ,(Ident "Nil" pos_,  [])
                                                    ]
                                         Maybe tp -> [(Ident "Just" pos_, [(Ident "just" pos_, tp)
                                                                         ]
                                                     )
                                                    ,(Ident "Nothing" pos_,  [])
                                                    ]
                                         Either tp1 tp2 -> [
                                                      (Ident "Left"    pos_,  [(Ident "left"  pos_, tp1) ])
                                                    , (Ident "Right"   pos_,  [(Ident "right" pos_, tp2) ])
                                                    ]
                                         Map tp1 tp2 -> [ (Ident "Entry" pos_, [ (Ident "key" pos_, tp1)
                                                                               , (Ident "val" pos_, tp2)
                                                                               , (Ident "tl" pos_, NT name_ (map getName params_) False)
                                                                               ])
                                                        , (Ident "Nil" pos_, [])
                                                        ]
                                         IntMap tp   -> [ (Ident "Entry" pos_, [ (Ident "key" pos_, Haskell "Int")
                                                                               , (Ident "val" pos_, tp)
                                                                               , (Ident "tl" pos_, NT name_ (map getName params_) False)
                                                                               ])
                                                        , (Ident "Nil" pos_, [])
                                                        ]
                                         OrdSet tp   -> [ (Ident "Entry" pos_, [ (Ident "val" pos_, tp)
                                                                               , (Ident "tl" pos_, NT name_ (map getName params_) False) ])
                                                        , (Ident "Nil" pos_, [])
                                                        ]
                                         IntSet      -> [ (Ident "Entry" pos_, [ (Ident "val" pos_, Haskell "Int")
                                                                               , (Ident "tl" pos_, NT name_ (map getName params_) False) ])
                                                        , (Ident "Nil" pos_, [])
                                                        ]
                                         Tuple xs -> [(Ident "Tuple" pos_, xs)]
                                 {-# LINE 1908 "dist/build/Transform.hs"#-}
   {-# INLINE rule146 #-}
   {-# LINE 691 "./src-ag/Transform.ag" #-}
   rule146 = \ ((_lhsIallNonterminals) :: Set NontermIdent) type_ ->
                                 {-# LINE 691 "./src-ag/Transform.ag" #-}
                                 case type_ of
                                  Maybe tp       -> Maybe  (  makeType _lhsIallNonterminals tp)
                                  Either tp1 tp2 -> Either (  makeType _lhsIallNonterminals tp1) (makeType _lhsIallNonterminals tp2)
                                  List tp        -> List   (  makeType _lhsIallNonterminals tp)
                                  Tuple xs       -> Tuple [(f,makeType _lhsIallNonterminals tp) | (f,tp) <- xs]
                                  Map tp1 tp2    -> Map    (  makeType _lhsIallNonterminals tp1) (makeType _lhsIallNonterminals tp2)
                                  IntMap tp      -> IntMap (  makeType _lhsIallNonterminals tp)
                                  OrdSet tp      -> OrdSet (  makeType _lhsIallNonterminals tp)
                                  IntSet         -> IntSet
                                 {-# LINE 1922 "dist/build/Transform.hs"#-}
   {-# INLINE rule147 #-}
   {-# LINE 700 "./src-ag/Transform.ag" #-}
   rule147 = \ _argType name_ ->
                                 {-# LINE 700 "./src-ag/Transform.ag" #-}
                                 [(name_,_argType)]
                                 {-# LINE 1928 "dist/build/Transform.hs"#-}
   {-# INLINE rule148 #-}
   {-# LINE 953 "./src-ag/Transform.ag" #-}
   rule148 = \ name_ params_ ->
                            {-# LINE 953 "./src-ag/Transform.ag" #-}
                            if null params_
                            then Map.empty
                            else Map.singleton name_ params_
                            {-# LINE 1936 "dist/build/Transform.hs"#-}
   {-# INLINE rule149 #-}
   {-# LINE 976 "./src-ag/Transform.ag" #-}
   rule149 = \ ctx_ name_ ->
                         {-# LINE 976 "./src-ag/Transform.ag" #-}
                         if null ctx_
                         then Map.empty
                         else Map.singleton name_ ctx_
                         {-# LINE 1944 "dist/build/Transform.hs"#-}
   {-# INLINE rule150 #-}
   rule150 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule151 #-}
   rule151 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule152 #-}
   rule152 = \  (_ :: ()) ->
     []
   {-# INLINE rule153 #-}
   rule153 = \  (_ :: ()) ->
     []
   {-# INLINE rule154 #-}
   rule154 = \  (_ :: ()) ->
     []
   {-# INLINE rule155 #-}
   rule155 = \  (_ :: ()) ->
     []
   {-# INLINE rule156 #-}
   rule156 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule157 #-}
   rule157 = \  (_ :: ()) ->
     []
   {-# INLINE rule158 #-}
   rule158 = \  (_ :: ()) ->
     []
   {-# INLINE rule159 #-}
   rule159 = \  (_ :: ()) ->
     []
   {-# INLINE rule160 #-}
   rule160 = \  (_ :: ()) ->
     []
   {-# INLINE rule161 #-}
   rule161 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule162 #-}
   rule162 = \  (_ :: ()) ->
     []
   {-# INLINE rule163 #-}
   rule163 = \  (_ :: ()) ->
     []
   {-# INLINE rule164 #-}
   rule164 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule165 #-}
   rule165 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule166 #-}
   rule166 = \  (_ :: ()) ->
     mzero
   {-# INLINE rule167 #-}
   rule167 = \  (_ :: ()) ->
     id
   {-# INLINE rule168 #-}
   rule168 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule169 #-}
   rule169 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule170 #-}
   rule170 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule171 #-}
   rule171 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule172 #-}
   rule172 = \ ((_lhsIattrDecls) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIattrDecls
   {-# INLINE rule173 #-}
   rule173 = \ ((_lhsIattrs) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIattrs
   {-# INLINE rule174 #-}
   rule174 = \ ((_lhsIdefSets) :: Map Identifier (Set NontermIdent,Set Identifier)) ->
     _lhsIdefSets
{-# NOINLINE sem_Elem_Attr #-}
sem_Elem_Attr :: (Pos) -> (ClassContext) -> T_NontSet  -> ([String]) -> T_Attrs  -> T_Elem 
sem_Elem_Attr _ arg_ctx_ arg_names_ arg_quants_ arg_attrs_ = T_Elem (return st17) where
   {-# NOINLINE st17 #-}
   st17 = let
      v16 :: T_Elem_v16 
      v16 = \ (T_Elem_vIn16 _lhsIallAttrDecls _lhsIallAttrs _lhsIallConstructors _lhsIallFields _lhsIallNonterminals _lhsIattrDecls _lhsIattrs _lhsIdefSets _lhsIdefinedSets _lhsIoptions) -> ( let
         _namesX29 = Control.Monad.Identity.runIdentity (attach_T_NontSet (arg_names_))
         _attrsX11 = Control.Monad.Identity.runIdentity (attach_T_Attrs (arg_attrs_))
         (T_NontSet_vOut28 _namesIcollectedNames _namesIerrors _namesInontSet) = inv_NontSet_s29 _namesX29 (T_NontSet_vIn28 _namesOallFields _namesOallNonterminals _namesOdefinedSets)
         (T_Attrs_vOut10 _attrsIattrDecls _attrsIattrs _attrsIerrors _attrsIuseMap) = inv_Attrs_s11 _attrsX11 (T_Attrs_vIn10 _attrsOallFields _attrsOallNonterminals _attrsOattrDecls _attrsOattrs _attrsOnts _attrsOoptions)
         _lhsOctxCollect :: ContextMap
         _lhsOctxCollect = rule175 _namesInontSet arg_ctx_
         _lhsOquantCollect :: QuantMap
         _lhsOquantCollect = rule176 _namesInontSet arg_quants_
         _attrsOnts = rule177 _namesInontSet
         _lhsOattrOrderCollect :: AttrOrderMap
         _lhsOattrOrderCollect = rule178  ()
         _lhsOblocks :: Blocks
         _lhsOblocks = rule179  ()
         _lhsOcollectedArounds :: [ (NontermIdent, ConstructorIdent, [AroundInfo])  ]
         _lhsOcollectedArounds = rule180  ()
         _lhsOcollectedAugments :: [ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]
         _lhsOcollectedAugments = rule181  ()
         _lhsOcollectedConParams :: [(NontermIdent, ConstructorIdent, Set Identifier)]
         _lhsOcollectedConParams = rule182  ()
         _lhsOcollectedConstraints :: [(NontermIdent, ConstructorIdent, [Type])]
         _lhsOcollectedConstraints = rule183  ()
         _lhsOcollectedConstructorsMap :: Map NontermIdent (Set ConstructorIdent)
         _lhsOcollectedConstructorsMap = rule184  ()
         _lhsOcollectedFields :: [(NontermIdent, ConstructorIdent, FieldMap)]
         _lhsOcollectedFields = rule185  ()
         _lhsOcollectedInsts :: [ (NontermIdent, ConstructorIdent, [Identifier]) ]
         _lhsOcollectedInsts = rule186  ()
         _lhsOcollectedMacros :: [(NontermIdent, ConstructorIdent, MaybeMacro)]
         _lhsOcollectedMacros = rule187  ()
         _lhsOcollectedMerges :: [ (NontermIdent, ConstructorIdent, [MergeInfo])   ]
         _lhsOcollectedMerges = rule188  ()
         _lhsOcollectedNames :: Set Identifier
         _lhsOcollectedNames = rule189 _namesIcollectedNames
         _lhsOcollectedRules :: [ (NontermIdent, ConstructorIdent, RuleInfo)]
         _lhsOcollectedRules = rule190  ()
         _lhsOcollectedSetNames :: Set Identifier
         _lhsOcollectedSetNames = rule191  ()
         _lhsOcollectedSigs :: [ (NontermIdent, ConstructorIdent, SigInfo) ]
         _lhsOcollectedSigs = rule192  ()
         _lhsOcollectedUniques :: [ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]
         _lhsOcollectedUniques = rule193  ()
         _lhsOderivings :: Derivings
         _lhsOderivings = rule194  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule195 _attrsIerrors _namesIerrors
         _lhsOmoduleDecl :: Maybe (String,String,String)
         _lhsOmoduleDecl = rule196  ()
         _lhsOparamsCollect :: ParamMap
         _lhsOparamsCollect = rule197  ()
         _lhsOpragmas :: Options -> Options
         _lhsOpragmas = rule198  ()
         _lhsOsemPragmasCollect :: PragmaMap
         _lhsOsemPragmasCollect = rule199  ()
         _lhsOtypeSyns :: TypeSyns
         _lhsOtypeSyns = rule200  ()
         _lhsOuseMap :: Map NontermIdent (Map Identifier (String,String,String))
         _lhsOuseMap = rule201 _attrsIuseMap
         _lhsOwrappers :: Set NontermIdent
         _lhsOwrappers = rule202  ()
         _lhsOattrDecls :: Map NontermIdent (Attributes, Attributes)
         _lhsOattrDecls = rule203 _attrsIattrDecls
         _lhsOattrs :: Map NontermIdent (Attributes, Attributes)
         _lhsOattrs = rule204 _attrsIattrs
         _lhsOdefSets :: Map Identifier (Set NontermIdent,Set Identifier)
         _lhsOdefSets = rule205 _lhsIdefSets
         _namesOallFields = rule206 _lhsIallFields
         _namesOallNonterminals = rule207 _lhsIallNonterminals
         _namesOdefinedSets = rule208 _lhsIdefinedSets
         _attrsOallFields = rule209 _lhsIallFields
         _attrsOallNonterminals = rule210 _lhsIallNonterminals
         _attrsOattrDecls = rule211 _lhsIattrDecls
         _attrsOattrs = rule212 _lhsIattrs
         _attrsOoptions = rule213 _lhsIoptions
         __result_ = T_Elem_vOut16 _lhsOattrDecls _lhsOattrOrderCollect _lhsOattrs _lhsOblocks _lhsOcollectedArounds _lhsOcollectedAugments _lhsOcollectedConParams _lhsOcollectedConstraints _lhsOcollectedConstructorsMap _lhsOcollectedFields _lhsOcollectedInsts _lhsOcollectedMacros _lhsOcollectedMerges _lhsOcollectedNames _lhsOcollectedRules _lhsOcollectedSetNames _lhsOcollectedSigs _lhsOcollectedUniques _lhsOctxCollect _lhsOdefSets _lhsOderivings _lhsOerrors _lhsOmoduleDecl _lhsOparamsCollect _lhsOpragmas _lhsOquantCollect _lhsOsemPragmasCollect _lhsOtypeSyns _lhsOuseMap _lhsOwrappers
         in __result_ )
     in C_Elem_s17 v16
   {-# INLINE rule175 #-}
   {-# LINE 970 "./src-ag/Transform.ag" #-}
   rule175 = \ ((_namesInontSet) :: Set NontermIdent) ctx_ ->
                         {-# LINE 970 "./src-ag/Transform.ag" #-}
                         if null ctx_
                         then Map.empty
                         else Map.fromList [(nt, ctx_) | nt <- Set.toList _namesInontSet]
                         {-# LINE 2110 "dist/build/Transform.hs"#-}
   {-# INLINE rule176 #-}
   {-# LINE 995 "./src-ag/Transform.ag" #-}
   rule176 = \ ((_namesInontSet) :: Set NontermIdent) quants_ ->
                           {-# LINE 995 "./src-ag/Transform.ag" #-}
                           if null quants_
                           then Map.empty
                           else Map.fromList [(nt, quants_) | nt <- Set.toList _namesInontSet]
                           {-# LINE 2118 "dist/build/Transform.hs"#-}
   {-# INLINE rule177 #-}
   {-# LINE 1033 "./src-ag/Transform.ag" #-}
   rule177 = \ ((_namesInontSet) :: Set NontermIdent) ->
                       {-# LINE 1033 "./src-ag/Transform.ag" #-}
                       _namesInontSet
                       {-# LINE 2124 "dist/build/Transform.hs"#-}
   {-# INLINE rule178 #-}
   rule178 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule179 #-}
   rule179 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule180 #-}
   rule180 = \  (_ :: ()) ->
     []
   {-# INLINE rule181 #-}
   rule181 = \  (_ :: ()) ->
     []
   {-# INLINE rule182 #-}
   rule182 = \  (_ :: ()) ->
     []
   {-# INLINE rule183 #-}
   rule183 = \  (_ :: ()) ->
     []
   {-# INLINE rule184 #-}
   rule184 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule185 #-}
   rule185 = \  (_ :: ()) ->
     []
   {-# INLINE rule186 #-}
   rule186 = \  (_ :: ()) ->
     []
   {-# INLINE rule187 #-}
   rule187 = \  (_ :: ()) ->
     []
   {-# INLINE rule188 #-}
   rule188 = \  (_ :: ()) ->
     []
   {-# INLINE rule189 #-}
   rule189 = \ ((_namesIcollectedNames) :: Set Identifier) ->
     _namesIcollectedNames
   {-# INLINE rule190 #-}
   rule190 = \  (_ :: ()) ->
     []
   {-# INLINE rule191 #-}
   rule191 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule192 #-}
   rule192 = \  (_ :: ()) ->
     []
   {-# INLINE rule193 #-}
   rule193 = \  (_ :: ()) ->
     []
   {-# INLINE rule194 #-}
   rule194 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule195 #-}
   rule195 = \ ((_attrsIerrors) :: Seq Error) ((_namesIerrors) :: Seq Error) ->
     _namesIerrors Seq.>< _attrsIerrors
   {-# INLINE rule196 #-}
   rule196 = \  (_ :: ()) ->
     mzero
   {-# INLINE rule197 #-}
   rule197 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule198 #-}
   rule198 = \  (_ :: ()) ->
     id
   {-# INLINE rule199 #-}
   rule199 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule200 #-}
   rule200 = \  (_ :: ()) ->
     []
   {-# INLINE rule201 #-}
   rule201 = \ ((_attrsIuseMap) :: Map NontermIdent (Map Identifier (String,String,String))) ->
     _attrsIuseMap
   {-# INLINE rule202 #-}
   rule202 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule203 #-}
   rule203 = \ ((_attrsIattrDecls) :: Map NontermIdent (Attributes, Attributes)) ->
     _attrsIattrDecls
   {-# INLINE rule204 #-}
   rule204 = \ ((_attrsIattrs) :: Map NontermIdent (Attributes, Attributes)) ->
     _attrsIattrs
   {-# INLINE rule205 #-}
   rule205 = \ ((_lhsIdefSets) :: Map Identifier (Set NontermIdent,Set Identifier)) ->
     _lhsIdefSets
   {-# INLINE rule206 #-}
   rule206 = \ ((_lhsIallFields) :: DataTypes) ->
     _lhsIallFields
   {-# INLINE rule207 #-}
   rule207 = \ ((_lhsIallNonterminals) :: Set NontermIdent) ->
     _lhsIallNonterminals
   {-# INLINE rule208 #-}
   rule208 = \ ((_lhsIdefinedSets) :: DefinedSets) ->
     _lhsIdefinedSets
   {-# INLINE rule209 #-}
   rule209 = \ ((_lhsIallFields) :: DataTypes) ->
     _lhsIallFields
   {-# INLINE rule210 #-}
   rule210 = \ ((_lhsIallNonterminals) :: Set NontermIdent) ->
     _lhsIallNonterminals
   {-# INLINE rule211 #-}
   rule211 = \ ((_lhsIattrDecls) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIattrDecls
   {-# INLINE rule212 #-}
   rule212 = \ ((_lhsIattrs) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIattrs
   {-# INLINE rule213 #-}
   rule213 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Elem_Sem #-}
sem_Elem_Sem :: (Pos) -> (ClassContext) -> T_NontSet  -> T_Attrs  -> ([String]) -> T_SemAlts  -> T_Elem 
sem_Elem_Sem _ arg_ctx_ arg_names_ arg_attrs_ arg_quants_ arg_alts_ = T_Elem (return st17) where
   {-# NOINLINE st17 #-}
   st17 = let
      v16 :: T_Elem_v16 
      v16 = \ (T_Elem_vIn16 _lhsIallAttrDecls _lhsIallAttrs _lhsIallConstructors _lhsIallFields _lhsIallNonterminals _lhsIattrDecls _lhsIattrs _lhsIdefSets _lhsIdefinedSets _lhsIoptions) -> ( let
         _namesX29 = Control.Monad.Identity.runIdentity (attach_T_NontSet (arg_names_))
         _attrsX11 = Control.Monad.Identity.runIdentity (attach_T_Attrs (arg_attrs_))
         _altsX41 = Control.Monad.Identity.runIdentity (attach_T_SemAlts (arg_alts_))
         (T_NontSet_vOut28 _namesIcollectedNames _namesIerrors _namesInontSet) = inv_NontSet_s29 _namesX29 (T_NontSet_vIn28 _namesOallFields _namesOallNonterminals _namesOdefinedSets)
         (T_Attrs_vOut10 _attrsIattrDecls _attrsIattrs _attrsIerrors _attrsIuseMap) = inv_Attrs_s11 _attrsX11 (T_Attrs_vIn10 _attrsOallFields _attrsOallNonterminals _attrsOattrDecls _attrsOattrs _attrsOnts _attrsOoptions)
         (T_SemAlts_vOut40 _altsIattrOrderCollect _altsIcollectedArounds _altsIcollectedAugments _altsIcollectedInsts _altsIcollectedMerges _altsIcollectedRules _altsIcollectedSigs _altsIcollectedUniques _altsIerrors _altsIsemPragmasCollect) = inv_SemAlts_s41 _altsX41 (T_SemAlts_vIn40 _altsOallAttrDecls _altsOallAttrs _altsOallFields _altsOnts _altsOoptions)
         _altsOnts = rule214 _namesInontSet
         _lhsOctxCollect :: ContextMap
         _lhsOctxCollect = rule215 _namesInontSet arg_ctx_
         _lhsOquantCollect :: QuantMap
         _lhsOquantCollect = rule216 _namesInontSet arg_quants_
         _attrsOnts = rule217 _namesInontSet
         _lhsOattrOrderCollect :: AttrOrderMap
         _lhsOattrOrderCollect = rule218 _altsIattrOrderCollect
         _lhsOblocks :: Blocks
         _lhsOblocks = rule219  ()
         _lhsOcollectedArounds :: [ (NontermIdent, ConstructorIdent, [AroundInfo])  ]
         _lhsOcollectedArounds = rule220 _altsIcollectedArounds
         _lhsOcollectedAugments :: [ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]
         _lhsOcollectedAugments = rule221 _altsIcollectedAugments
         _lhsOcollectedConParams :: [(NontermIdent, ConstructorIdent, Set Identifier)]
         _lhsOcollectedConParams = rule222  ()
         _lhsOcollectedConstraints :: [(NontermIdent, ConstructorIdent, [Type])]
         _lhsOcollectedConstraints = rule223  ()
         _lhsOcollectedConstructorsMap :: Map NontermIdent (Set ConstructorIdent)
         _lhsOcollectedConstructorsMap = rule224  ()
         _lhsOcollectedFields :: [(NontermIdent, ConstructorIdent, FieldMap)]
         _lhsOcollectedFields = rule225  ()
         _lhsOcollectedInsts :: [ (NontermIdent, ConstructorIdent, [Identifier]) ]
         _lhsOcollectedInsts = rule226 _altsIcollectedInsts
         _lhsOcollectedMacros :: [(NontermIdent, ConstructorIdent, MaybeMacro)]
         _lhsOcollectedMacros = rule227  ()
         _lhsOcollectedMerges :: [ (NontermIdent, ConstructorIdent, [MergeInfo])   ]
         _lhsOcollectedMerges = rule228 _altsIcollectedMerges
         _lhsOcollectedNames :: Set Identifier
         _lhsOcollectedNames = rule229 _namesIcollectedNames
         _lhsOcollectedRules :: [ (NontermIdent, ConstructorIdent, RuleInfo)]
         _lhsOcollectedRules = rule230 _altsIcollectedRules
         _lhsOcollectedSetNames :: Set Identifier
         _lhsOcollectedSetNames = rule231  ()
         _lhsOcollectedSigs :: [ (NontermIdent, ConstructorIdent, SigInfo) ]
         _lhsOcollectedSigs = rule232 _altsIcollectedSigs
         _lhsOcollectedUniques :: [ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]
         _lhsOcollectedUniques = rule233 _altsIcollectedUniques
         _lhsOderivings :: Derivings
         _lhsOderivings = rule234  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule235 _altsIerrors _attrsIerrors _namesIerrors
         _lhsOmoduleDecl :: Maybe (String,String,String)
         _lhsOmoduleDecl = rule236  ()
         _lhsOparamsCollect :: ParamMap
         _lhsOparamsCollect = rule237  ()
         _lhsOpragmas :: Options -> Options
         _lhsOpragmas = rule238  ()
         _lhsOsemPragmasCollect :: PragmaMap
         _lhsOsemPragmasCollect = rule239 _altsIsemPragmasCollect
         _lhsOtypeSyns :: TypeSyns
         _lhsOtypeSyns = rule240  ()
         _lhsOuseMap :: Map NontermIdent (Map Identifier (String,String,String))
         _lhsOuseMap = rule241 _attrsIuseMap
         _lhsOwrappers :: Set NontermIdent
         _lhsOwrappers = rule242  ()
         _lhsOattrDecls :: Map NontermIdent (Attributes, Attributes)
         _lhsOattrDecls = rule243 _attrsIattrDecls
         _lhsOattrs :: Map NontermIdent (Attributes, Attributes)
         _lhsOattrs = rule244 _attrsIattrs
         _lhsOdefSets :: Map Identifier (Set NontermIdent,Set Identifier)
         _lhsOdefSets = rule245 _lhsIdefSets
         _namesOallFields = rule246 _lhsIallFields
         _namesOallNonterminals = rule247 _lhsIallNonterminals
         _namesOdefinedSets = rule248 _lhsIdefinedSets
         _attrsOallFields = rule249 _lhsIallFields
         _attrsOallNonterminals = rule250 _lhsIallNonterminals
         _attrsOattrDecls = rule251 _lhsIattrDecls
         _attrsOattrs = rule252 _lhsIattrs
         _attrsOoptions = rule253 _lhsIoptions
         _altsOallAttrDecls = rule254 _lhsIallAttrDecls
         _altsOallAttrs = rule255 _lhsIallAttrs
         _altsOallFields = rule256 _lhsIallFields
         _altsOoptions = rule257 _lhsIoptions
         __result_ = T_Elem_vOut16 _lhsOattrDecls _lhsOattrOrderCollect _lhsOattrs _lhsOblocks _lhsOcollectedArounds _lhsOcollectedAugments _lhsOcollectedConParams _lhsOcollectedConstraints _lhsOcollectedConstructorsMap _lhsOcollectedFields _lhsOcollectedInsts _lhsOcollectedMacros _lhsOcollectedMerges _lhsOcollectedNames _lhsOcollectedRules _lhsOcollectedSetNames _lhsOcollectedSigs _lhsOcollectedUniques _lhsOctxCollect _lhsOdefSets _lhsOderivings _lhsOerrors _lhsOmoduleDecl _lhsOparamsCollect _lhsOpragmas _lhsOquantCollect _lhsOsemPragmasCollect _lhsOtypeSyns _lhsOuseMap _lhsOwrappers
         in __result_ )
     in C_Elem_s17 v16
   {-# INLINE rule214 #-}
   {-# LINE 177 "./src-ag/Transform.ag" #-}
   rule214 = \ ((_namesInontSet) :: Set NontermIdent) ->
                      {-# LINE 177 "./src-ag/Transform.ag" #-}
                      _namesInontSet
                      {-# LINE 2328 "dist/build/Transform.hs"#-}
   {-# INLINE rule215 #-}
   {-# LINE 970 "./src-ag/Transform.ag" #-}
   rule215 = \ ((_namesInontSet) :: Set NontermIdent) ctx_ ->
                         {-# LINE 970 "./src-ag/Transform.ag" #-}
                         if null ctx_
                         then Map.empty
                         else Map.fromList [(nt, ctx_) | nt <- Set.toList _namesInontSet]
                         {-# LINE 2336 "dist/build/Transform.hs"#-}
   {-# INLINE rule216 #-}
   {-# LINE 995 "./src-ag/Transform.ag" #-}
   rule216 = \ ((_namesInontSet) :: Set NontermIdent) quants_ ->
                           {-# LINE 995 "./src-ag/Transform.ag" #-}
                           if null quants_
                           then Map.empty
                           else Map.fromList [(nt, quants_) | nt <- Set.toList _namesInontSet]
                           {-# LINE 2344 "dist/build/Transform.hs"#-}
   {-# INLINE rule217 #-}
   {-# LINE 1034 "./src-ag/Transform.ag" #-}
   rule217 = \ ((_namesInontSet) :: Set NontermIdent) ->
                       {-# LINE 1034 "./src-ag/Transform.ag" #-}
                       _namesInontSet
                       {-# LINE 2350 "dist/build/Transform.hs"#-}
   {-# INLINE rule218 #-}
   rule218 = \ ((_altsIattrOrderCollect) :: AttrOrderMap) ->
     _altsIattrOrderCollect
   {-# INLINE rule219 #-}
   rule219 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule220 #-}
   rule220 = \ ((_altsIcollectedArounds) :: [ (NontermIdent, ConstructorIdent, [AroundInfo])  ]) ->
     _altsIcollectedArounds
   {-# INLINE rule221 #-}
   rule221 = \ ((_altsIcollectedAugments) :: [ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]) ->
     _altsIcollectedAugments
   {-# INLINE rule222 #-}
   rule222 = \  (_ :: ()) ->
     []
   {-# INLINE rule223 #-}
   rule223 = \  (_ :: ()) ->
     []
   {-# INLINE rule224 #-}
   rule224 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule225 #-}
   rule225 = \  (_ :: ()) ->
     []
   {-# INLINE rule226 #-}
   rule226 = \ ((_altsIcollectedInsts) :: [ (NontermIdent, ConstructorIdent, [Identifier]) ]) ->
     _altsIcollectedInsts
   {-# INLINE rule227 #-}
   rule227 = \  (_ :: ()) ->
     []
   {-# INLINE rule228 #-}
   rule228 = \ ((_altsIcollectedMerges) :: [ (NontermIdent, ConstructorIdent, [MergeInfo])   ]) ->
     _altsIcollectedMerges
   {-# INLINE rule229 #-}
   rule229 = \ ((_namesIcollectedNames) :: Set Identifier) ->
     _namesIcollectedNames
   {-# INLINE rule230 #-}
   rule230 = \ ((_altsIcollectedRules) :: [ (NontermIdent, ConstructorIdent, RuleInfo)]) ->
     _altsIcollectedRules
   {-# INLINE rule231 #-}
   rule231 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule232 #-}
   rule232 = \ ((_altsIcollectedSigs) :: [ (NontermIdent, ConstructorIdent, SigInfo) ]) ->
     _altsIcollectedSigs
   {-# INLINE rule233 #-}
   rule233 = \ ((_altsIcollectedUniques) :: [ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]) ->
     _altsIcollectedUniques
   {-# INLINE rule234 #-}
   rule234 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule235 #-}
   rule235 = \ ((_altsIerrors) :: Seq Error) ((_attrsIerrors) :: Seq Error) ((_namesIerrors) :: Seq Error) ->
     _namesIerrors Seq.>< _attrsIerrors Seq.>< _altsIerrors
   {-# INLINE rule236 #-}
   rule236 = \  (_ :: ()) ->
     mzero
   {-# INLINE rule237 #-}
   rule237 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule238 #-}
   rule238 = \  (_ :: ()) ->
     id
   {-# INLINE rule239 #-}
   rule239 = \ ((_altsIsemPragmasCollect) :: PragmaMap) ->
     _altsIsemPragmasCollect
   {-# INLINE rule240 #-}
   rule240 = \  (_ :: ()) ->
     []
   {-# INLINE rule241 #-}
   rule241 = \ ((_attrsIuseMap) :: Map NontermIdent (Map Identifier (String,String,String))) ->
     _attrsIuseMap
   {-# INLINE rule242 #-}
   rule242 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule243 #-}
   rule243 = \ ((_attrsIattrDecls) :: Map NontermIdent (Attributes, Attributes)) ->
     _attrsIattrDecls
   {-# INLINE rule244 #-}
   rule244 = \ ((_attrsIattrs) :: Map NontermIdent (Attributes, Attributes)) ->
     _attrsIattrs
   {-# INLINE rule245 #-}
   rule245 = \ ((_lhsIdefSets) :: Map Identifier (Set NontermIdent,Set Identifier)) ->
     _lhsIdefSets
   {-# INLINE rule246 #-}
   rule246 = \ ((_lhsIallFields) :: DataTypes) ->
     _lhsIallFields
   {-# INLINE rule247 #-}
   rule247 = \ ((_lhsIallNonterminals) :: Set NontermIdent) ->
     _lhsIallNonterminals
   {-# INLINE rule248 #-}
   rule248 = \ ((_lhsIdefinedSets) :: DefinedSets) ->
     _lhsIdefinedSets
   {-# INLINE rule249 #-}
   rule249 = \ ((_lhsIallFields) :: DataTypes) ->
     _lhsIallFields
   {-# INLINE rule250 #-}
   rule250 = \ ((_lhsIallNonterminals) :: Set NontermIdent) ->
     _lhsIallNonterminals
   {-# INLINE rule251 #-}
   rule251 = \ ((_lhsIattrDecls) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIattrDecls
   {-# INLINE rule252 #-}
   rule252 = \ ((_lhsIattrs) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIattrs
   {-# INLINE rule253 #-}
   rule253 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule254 #-}
   rule254 = \ ((_lhsIallAttrDecls) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIallAttrDecls
   {-# INLINE rule255 #-}
   rule255 = \ ((_lhsIallAttrs) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIallAttrs
   {-# INLINE rule256 #-}
   rule256 = \ ((_lhsIallFields) :: DataTypes) ->
     _lhsIallFields
   {-# INLINE rule257 #-}
   rule257 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Elem_Txt #-}
sem_Elem_Txt :: (Pos) -> (BlockKind) -> (Maybe NontermIdent) -> ([String]) -> T_Elem 
sem_Elem_Txt arg_pos_ arg_kind_ arg_mbNt_ arg_lines_ = T_Elem (return st17) where
   {-# NOINLINE st17 #-}
   st17 = let
      v16 :: T_Elem_v16 
      v16 = \ (T_Elem_vIn16 _lhsIallAttrDecls _lhsIallAttrs _lhsIallConstructors _lhsIallFields _lhsIallNonterminals _lhsIattrDecls _lhsIattrs _lhsIdefSets _lhsIdefinedSets _lhsIoptions) -> ( let
         _blockInfo = rule258 arg_kind_ arg_mbNt_
         _blockValue = rule259 arg_lines_ arg_pos_
         _lhsOblocks :: Blocks
         _lhsOblocks = rule260 _blockInfo _blockValue
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule261 _lhsIoptions arg_lines_ arg_pos_
         _lhsOattrOrderCollect :: AttrOrderMap
         _lhsOattrOrderCollect = rule262  ()
         _lhsOcollectedArounds :: [ (NontermIdent, ConstructorIdent, [AroundInfo])  ]
         _lhsOcollectedArounds = rule263  ()
         _lhsOcollectedAugments :: [ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]
         _lhsOcollectedAugments = rule264  ()
         _lhsOcollectedConParams :: [(NontermIdent, ConstructorIdent, Set Identifier)]
         _lhsOcollectedConParams = rule265  ()
         _lhsOcollectedConstraints :: [(NontermIdent, ConstructorIdent, [Type])]
         _lhsOcollectedConstraints = rule266  ()
         _lhsOcollectedConstructorsMap :: Map NontermIdent (Set ConstructorIdent)
         _lhsOcollectedConstructorsMap = rule267  ()
         _lhsOcollectedFields :: [(NontermIdent, ConstructorIdent, FieldMap)]
         _lhsOcollectedFields = rule268  ()
         _lhsOcollectedInsts :: [ (NontermIdent, ConstructorIdent, [Identifier]) ]
         _lhsOcollectedInsts = rule269  ()
         _lhsOcollectedMacros :: [(NontermIdent, ConstructorIdent, MaybeMacro)]
         _lhsOcollectedMacros = rule270  ()
         _lhsOcollectedMerges :: [ (NontermIdent, ConstructorIdent, [MergeInfo])   ]
         _lhsOcollectedMerges = rule271  ()
         _lhsOcollectedNames :: Set Identifier
         _lhsOcollectedNames = rule272  ()
         _lhsOcollectedRules :: [ (NontermIdent, ConstructorIdent, RuleInfo)]
         _lhsOcollectedRules = rule273  ()
         _lhsOcollectedSetNames :: Set Identifier
         _lhsOcollectedSetNames = rule274  ()
         _lhsOcollectedSigs :: [ (NontermIdent, ConstructorIdent, SigInfo) ]
         _lhsOcollectedSigs = rule275  ()
         _lhsOcollectedUniques :: [ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]
         _lhsOcollectedUniques = rule276  ()
         _lhsOctxCollect :: ContextMap
         _lhsOctxCollect = rule277  ()
         _lhsOderivings :: Derivings
         _lhsOderivings = rule278  ()
         _lhsOmoduleDecl :: Maybe (String,String,String)
         _lhsOmoduleDecl = rule279  ()
         _lhsOparamsCollect :: ParamMap
         _lhsOparamsCollect = rule280  ()
         _lhsOpragmas :: Options -> Options
         _lhsOpragmas = rule281  ()
         _lhsOquantCollect :: QuantMap
         _lhsOquantCollect = rule282  ()
         _lhsOsemPragmasCollect :: PragmaMap
         _lhsOsemPragmasCollect = rule283  ()
         _lhsOtypeSyns :: TypeSyns
         _lhsOtypeSyns = rule284  ()
         _lhsOuseMap :: Map NontermIdent (Map Identifier (String,String,String))
         _lhsOuseMap = rule285  ()
         _lhsOwrappers :: Set NontermIdent
         _lhsOwrappers = rule286  ()
         _lhsOattrDecls :: Map NontermIdent (Attributes, Attributes)
         _lhsOattrDecls = rule287 _lhsIattrDecls
         _lhsOattrs :: Map NontermIdent (Attributes, Attributes)
         _lhsOattrs = rule288 _lhsIattrs
         _lhsOdefSets :: Map Identifier (Set NontermIdent,Set Identifier)
         _lhsOdefSets = rule289 _lhsIdefSets
         __result_ = T_Elem_vOut16 _lhsOattrDecls _lhsOattrOrderCollect _lhsOattrs _lhsOblocks _lhsOcollectedArounds _lhsOcollectedAugments _lhsOcollectedConParams _lhsOcollectedConstraints _lhsOcollectedConstructorsMap _lhsOcollectedFields _lhsOcollectedInsts _lhsOcollectedMacros _lhsOcollectedMerges _lhsOcollectedNames _lhsOcollectedRules _lhsOcollectedSetNames _lhsOcollectedSigs _lhsOcollectedUniques _lhsOctxCollect _lhsOdefSets _lhsOderivings _lhsOerrors _lhsOmoduleDecl _lhsOparamsCollect _lhsOpragmas _lhsOquantCollect _lhsOsemPragmasCollect _lhsOtypeSyns _lhsOuseMap _lhsOwrappers
         in __result_ )
     in C_Elem_s17 v16
   {-# INLINE rule258 #-}
   {-# LINE 186 "./src-ag/Transform.ag" #-}
   rule258 = \ kind_ mbNt_ ->
                            {-# LINE 186 "./src-ag/Transform.ag" #-}
                            ( kind_
                            , mbNt_
                            )
                            {-# LINE 2550 "dist/build/Transform.hs"#-}
   {-# INLINE rule259 #-}
   {-# LINE 189 "./src-ag/Transform.ag" #-}
   rule259 = \ lines_ pos_ ->
                            {-# LINE 189 "./src-ag/Transform.ag" #-}
                            [(lines_, pos_)]
                            {-# LINE 2556 "dist/build/Transform.hs"#-}
   {-# INLINE rule260 #-}
   {-# LINE 190 "./src-ag/Transform.ag" #-}
   rule260 = \ _blockInfo _blockValue ->
                            {-# LINE 190 "./src-ag/Transform.ag" #-}
                            Map.singleton _blockInfo     _blockValue
                            {-# LINE 2562 "dist/build/Transform.hs"#-}
   {-# INLINE rule261 #-}
   {-# LINE 191 "./src-ag/Transform.ag" #-}
   rule261 = \ ((_lhsIoptions) :: Options) lines_ pos_ ->
                            {-# LINE 191 "./src-ag/Transform.ag" #-}
                            if checkParseBlock _lhsIoptions
                            then let ex  = Expression pos_ tks
                                     tks = [tk]
                                     tk  = HsToken (unlines lines_) pos_
                                 in Seq.fromList $ checkBlock $ ex
                            else Seq.empty
                            {-# LINE 2573 "dist/build/Transform.hs"#-}
   {-# INLINE rule262 #-}
   rule262 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule263 #-}
   rule263 = \  (_ :: ()) ->
     []
   {-# INLINE rule264 #-}
   rule264 = \  (_ :: ()) ->
     []
   {-# INLINE rule265 #-}
   rule265 = \  (_ :: ()) ->
     []
   {-# INLINE rule266 #-}
   rule266 = \  (_ :: ()) ->
     []
   {-# INLINE rule267 #-}
   rule267 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule268 #-}
   rule268 = \  (_ :: ()) ->
     []
   {-# INLINE rule269 #-}
   rule269 = \  (_ :: ()) ->
     []
   {-# INLINE rule270 #-}
   rule270 = \  (_ :: ()) ->
     []
   {-# INLINE rule271 #-}
   rule271 = \  (_ :: ()) ->
     []
   {-# INLINE rule272 #-}
   rule272 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule273 #-}
   rule273 = \  (_ :: ()) ->
     []
   {-# INLINE rule274 #-}
   rule274 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule275 #-}
   rule275 = \  (_ :: ()) ->
     []
   {-# INLINE rule276 #-}
   rule276 = \  (_ :: ()) ->
     []
   {-# INLINE rule277 #-}
   rule277 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule278 #-}
   rule278 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule279 #-}
   rule279 = \  (_ :: ()) ->
     mzero
   {-# INLINE rule280 #-}
   rule280 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule281 #-}
   rule281 = \  (_ :: ()) ->
     id
   {-# INLINE rule282 #-}
   rule282 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule283 #-}
   rule283 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule284 #-}
   rule284 = \  (_ :: ()) ->
     []
   {-# INLINE rule285 #-}
   rule285 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule286 #-}
   rule286 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule287 #-}
   rule287 = \ ((_lhsIattrDecls) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIattrDecls
   {-# INLINE rule288 #-}
   rule288 = \ ((_lhsIattrs) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIattrs
   {-# INLINE rule289 #-}
   rule289 = \ ((_lhsIdefSets) :: Map Identifier (Set NontermIdent,Set Identifier)) ->
     _lhsIdefSets
{-# NOINLINE sem_Elem_Set #-}
sem_Elem_Set :: (Pos) -> (NontermIdent) -> (Bool) -> T_NontSet  -> T_Elem 
sem_Elem_Set _ arg_name_ arg_merge_ arg_set_ = T_Elem (return st17) where
   {-# NOINLINE st17 #-}
   st17 = let
      v16 :: T_Elem_v16 
      v16 = \ (T_Elem_vIn16 _lhsIallAttrDecls _lhsIallAttrs _lhsIallConstructors _lhsIallFields _lhsIallNonterminals _lhsIattrDecls _lhsIattrs _lhsIdefSets _lhsIdefinedSets _lhsIoptions) -> ( let
         _setX29 = Control.Monad.Identity.runIdentity (attach_T_NontSet (arg_set_))
         (T_NontSet_vOut28 _setIcollectedNames _setIerrors _setInontSet) = inv_NontSet_s29 _setX29 (T_NontSet_vIn28 _setOallFields _setOallNonterminals _setOdefinedSets)
         _lhsOcollectedSetNames :: Set Identifier
         _lhsOcollectedSetNames = rule290 arg_name_
         (_defSets2,_errs) = rule291 _lhsIallNonterminals _lhsIdefSets _setIcollectedNames _setInontSet arg_merge_ arg_name_
         _lhsOdefSets :: Map Identifier (Set NontermIdent,Set Identifier)
         _lhsOdefSets = rule292 _defSets2
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule293 _errs _setIerrors
         _lhsOattrOrderCollect :: AttrOrderMap
         _lhsOattrOrderCollect = rule294  ()
         _lhsOblocks :: Blocks
         _lhsOblocks = rule295  ()
         _lhsOcollectedArounds :: [ (NontermIdent, ConstructorIdent, [AroundInfo])  ]
         _lhsOcollectedArounds = rule296  ()
         _lhsOcollectedAugments :: [ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]
         _lhsOcollectedAugments = rule297  ()
         _lhsOcollectedConParams :: [(NontermIdent, ConstructorIdent, Set Identifier)]
         _lhsOcollectedConParams = rule298  ()
         _lhsOcollectedConstraints :: [(NontermIdent, ConstructorIdent, [Type])]
         _lhsOcollectedConstraints = rule299  ()
         _lhsOcollectedConstructorsMap :: Map NontermIdent (Set ConstructorIdent)
         _lhsOcollectedConstructorsMap = rule300  ()
         _lhsOcollectedFields :: [(NontermIdent, ConstructorIdent, FieldMap)]
         _lhsOcollectedFields = rule301  ()
         _lhsOcollectedInsts :: [ (NontermIdent, ConstructorIdent, [Identifier]) ]
         _lhsOcollectedInsts = rule302  ()
         _lhsOcollectedMacros :: [(NontermIdent, ConstructorIdent, MaybeMacro)]
         _lhsOcollectedMacros = rule303  ()
         _lhsOcollectedMerges :: [ (NontermIdent, ConstructorIdent, [MergeInfo])   ]
         _lhsOcollectedMerges = rule304  ()
         _lhsOcollectedNames :: Set Identifier
         _lhsOcollectedNames = rule305 _setIcollectedNames
         _lhsOcollectedRules :: [ (NontermIdent, ConstructorIdent, RuleInfo)]
         _lhsOcollectedRules = rule306  ()
         _lhsOcollectedSigs :: [ (NontermIdent, ConstructorIdent, SigInfo) ]
         _lhsOcollectedSigs = rule307  ()
         _lhsOcollectedUniques :: [ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]
         _lhsOcollectedUniques = rule308  ()
         _lhsOctxCollect :: ContextMap
         _lhsOctxCollect = rule309  ()
         _lhsOderivings :: Derivings
         _lhsOderivings = rule310  ()
         _lhsOmoduleDecl :: Maybe (String,String,String)
         _lhsOmoduleDecl = rule311  ()
         _lhsOparamsCollect :: ParamMap
         _lhsOparamsCollect = rule312  ()
         _lhsOpragmas :: Options -> Options
         _lhsOpragmas = rule313  ()
         _lhsOquantCollect :: QuantMap
         _lhsOquantCollect = rule314  ()
         _lhsOsemPragmasCollect :: PragmaMap
         _lhsOsemPragmasCollect = rule315  ()
         _lhsOtypeSyns :: TypeSyns
         _lhsOtypeSyns = rule316  ()
         _lhsOuseMap :: Map NontermIdent (Map Identifier (String,String,String))
         _lhsOuseMap = rule317  ()
         _lhsOwrappers :: Set NontermIdent
         _lhsOwrappers = rule318  ()
         _lhsOattrDecls :: Map NontermIdent (Attributes, Attributes)
         _lhsOattrDecls = rule319 _lhsIattrDecls
         _lhsOattrs :: Map NontermIdent (Attributes, Attributes)
         _lhsOattrs = rule320 _lhsIattrs
         _setOallFields = rule321 _lhsIallFields
         _setOallNonterminals = rule322 _lhsIallNonterminals
         _setOdefinedSets = rule323 _lhsIdefinedSets
         __result_ = T_Elem_vOut16 _lhsOattrDecls _lhsOattrOrderCollect _lhsOattrs _lhsOblocks _lhsOcollectedArounds _lhsOcollectedAugments _lhsOcollectedConParams _lhsOcollectedConstraints _lhsOcollectedConstructorsMap _lhsOcollectedFields _lhsOcollectedInsts _lhsOcollectedMacros _lhsOcollectedMerges _lhsOcollectedNames _lhsOcollectedRules _lhsOcollectedSetNames _lhsOcollectedSigs _lhsOcollectedUniques _lhsOctxCollect _lhsOdefSets _lhsOderivings _lhsOerrors _lhsOmoduleDecl _lhsOparamsCollect _lhsOpragmas _lhsOquantCollect _lhsOsemPragmasCollect _lhsOtypeSyns _lhsOuseMap _lhsOwrappers
         in __result_ )
     in C_Elem_s17 v16
   {-# INLINE rule290 #-}
   {-# LINE 597 "./src-ag/Transform.ag" #-}
   rule290 = \ name_ ->
                                   {-# LINE 597 "./src-ag/Transform.ag" #-}
                                   Set.singleton name_
                                   {-# LINE 2739 "dist/build/Transform.hs"#-}
   {-# INLINE rule291 #-}
   {-# LINE 714 "./src-ag/Transform.ag" #-}
   rule291 = \ ((_lhsIallNonterminals) :: Set NontermIdent) ((_lhsIdefSets) :: Map Identifier (Set NontermIdent,Set Identifier)) ((_setIcollectedNames) :: Set Identifier) ((_setInontSet) :: Set NontermIdent) merge_ name_ ->
                                {-# LINE 714 "./src-ag/Transform.ag" #-}
                                let allUsedNames = Set.unions [ maybe (Set.singleton n)
                                                                      snd
                                                                      (Map.lookup n _lhsIdefSets)
                                                              | n <- Set.toList _setIcollectedNames
                                                              ]
                                    (nontSet,e1) | Set.member name_ allUsedNames
                                                             = (Set.empty, Seq.singleton(CyclicSet name_))
                                                 | otherwise = (_setInontSet, Seq.empty)
                                    (res, e2) = let toAdd = (nontSet,Set.insert name_ allUsedNames)
                                                    un (a,b) (c,d) = (a `Set.union` c, b `Set.union` d)
                                                in if Set.member name_ _lhsIallNonterminals || not merge_
                                                   then checkDuplicate DupSet name_ toAdd _lhsIdefSets
                                                   else (Map.insertWith un name_ toAdd _lhsIdefSets, Seq.empty)
                                in (res, e1 Seq.>< e2)
                                {-# LINE 2758 "dist/build/Transform.hs"#-}
   {-# INLINE rule292 #-}
   {-# LINE 728 "./src-ag/Transform.ag" #-}
   rule292 = \ _defSets2 ->
                                {-# LINE 728 "./src-ag/Transform.ag" #-}
                                _defSets2
                                {-# LINE 2764 "dist/build/Transform.hs"#-}
   {-# INLINE rule293 #-}
   {-# LINE 729 "./src-ag/Transform.ag" #-}
   rule293 = \ _errs ((_setIerrors) :: Seq Error) ->
                                {-# LINE 729 "./src-ag/Transform.ag" #-}
                                _errs >< _setIerrors
                                {-# LINE 2770 "dist/build/Transform.hs"#-}
   {-# INLINE rule294 #-}
   rule294 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule295 #-}
   rule295 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule296 #-}
   rule296 = \  (_ :: ()) ->
     []
   {-# INLINE rule297 #-}
   rule297 = \  (_ :: ()) ->
     []
   {-# INLINE rule298 #-}
   rule298 = \  (_ :: ()) ->
     []
   {-# INLINE rule299 #-}
   rule299 = \  (_ :: ()) ->
     []
   {-# INLINE rule300 #-}
   rule300 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule301 #-}
   rule301 = \  (_ :: ()) ->
     []
   {-# INLINE rule302 #-}
   rule302 = \  (_ :: ()) ->
     []
   {-# INLINE rule303 #-}
   rule303 = \  (_ :: ()) ->
     []
   {-# INLINE rule304 #-}
   rule304 = \  (_ :: ()) ->
     []
   {-# INLINE rule305 #-}
   rule305 = \ ((_setIcollectedNames) :: Set Identifier) ->
     _setIcollectedNames
   {-# INLINE rule306 #-}
   rule306 = \  (_ :: ()) ->
     []
   {-# INLINE rule307 #-}
   rule307 = \  (_ :: ()) ->
     []
   {-# INLINE rule308 #-}
   rule308 = \  (_ :: ()) ->
     []
   {-# INLINE rule309 #-}
   rule309 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule310 #-}
   rule310 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule311 #-}
   rule311 = \  (_ :: ()) ->
     mzero
   {-# INLINE rule312 #-}
   rule312 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule313 #-}
   rule313 = \  (_ :: ()) ->
     id
   {-# INLINE rule314 #-}
   rule314 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule315 #-}
   rule315 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule316 #-}
   rule316 = \  (_ :: ()) ->
     []
   {-# INLINE rule317 #-}
   rule317 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule318 #-}
   rule318 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule319 #-}
   rule319 = \ ((_lhsIattrDecls) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIattrDecls
   {-# INLINE rule320 #-}
   rule320 = \ ((_lhsIattrs) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIattrs
   {-# INLINE rule321 #-}
   rule321 = \ ((_lhsIallFields) :: DataTypes) ->
     _lhsIallFields
   {-# INLINE rule322 #-}
   rule322 = \ ((_lhsIallNonterminals) :: Set NontermIdent) ->
     _lhsIallNonterminals
   {-# INLINE rule323 #-}
   rule323 = \ ((_lhsIdefinedSets) :: DefinedSets) ->
     _lhsIdefinedSets
{-# NOINLINE sem_Elem_Deriving #-}
sem_Elem_Deriving :: (Pos) -> T_NontSet  -> ([NontermIdent]) -> T_Elem 
sem_Elem_Deriving _ arg_set_ arg_classes_ = T_Elem (return st17) where
   {-# NOINLINE st17 #-}
   st17 = let
      v16 :: T_Elem_v16 
      v16 = \ (T_Elem_vIn16 _lhsIallAttrDecls _lhsIallAttrs _lhsIallConstructors _lhsIallFields _lhsIallNonterminals _lhsIattrDecls _lhsIattrs _lhsIdefSets _lhsIdefinedSets _lhsIoptions) -> ( let
         _setX29 = Control.Monad.Identity.runIdentity (attach_T_NontSet (arg_set_))
         (T_NontSet_vOut28 _setIcollectedNames _setIerrors _setInontSet) = inv_NontSet_s29 _setX29 (T_NontSet_vIn28 _setOallFields _setOallNonterminals _setOdefinedSets)
         _lhsOderivings :: Derivings
         _lhsOderivings = rule324 _setInontSet arg_classes_
         _lhsOattrOrderCollect :: AttrOrderMap
         _lhsOattrOrderCollect = rule325  ()
         _lhsOblocks :: Blocks
         _lhsOblocks = rule326  ()
         _lhsOcollectedArounds :: [ (NontermIdent, ConstructorIdent, [AroundInfo])  ]
         _lhsOcollectedArounds = rule327  ()
         _lhsOcollectedAugments :: [ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]
         _lhsOcollectedAugments = rule328  ()
         _lhsOcollectedConParams :: [(NontermIdent, ConstructorIdent, Set Identifier)]
         _lhsOcollectedConParams = rule329  ()
         _lhsOcollectedConstraints :: [(NontermIdent, ConstructorIdent, [Type])]
         _lhsOcollectedConstraints = rule330  ()
         _lhsOcollectedConstructorsMap :: Map NontermIdent (Set ConstructorIdent)
         _lhsOcollectedConstructorsMap = rule331  ()
         _lhsOcollectedFields :: [(NontermIdent, ConstructorIdent, FieldMap)]
         _lhsOcollectedFields = rule332  ()
         _lhsOcollectedInsts :: [ (NontermIdent, ConstructorIdent, [Identifier]) ]
         _lhsOcollectedInsts = rule333  ()
         _lhsOcollectedMacros :: [(NontermIdent, ConstructorIdent, MaybeMacro)]
         _lhsOcollectedMacros = rule334  ()
         _lhsOcollectedMerges :: [ (NontermIdent, ConstructorIdent, [MergeInfo])   ]
         _lhsOcollectedMerges = rule335  ()
         _lhsOcollectedNames :: Set Identifier
         _lhsOcollectedNames = rule336 _setIcollectedNames
         _lhsOcollectedRules :: [ (NontermIdent, ConstructorIdent, RuleInfo)]
         _lhsOcollectedRules = rule337  ()
         _lhsOcollectedSetNames :: Set Identifier
         _lhsOcollectedSetNames = rule338  ()
         _lhsOcollectedSigs :: [ (NontermIdent, ConstructorIdent, SigInfo) ]
         _lhsOcollectedSigs = rule339  ()
         _lhsOcollectedUniques :: [ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]
         _lhsOcollectedUniques = rule340  ()
         _lhsOctxCollect :: ContextMap
         _lhsOctxCollect = rule341  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule342 _setIerrors
         _lhsOmoduleDecl :: Maybe (String,String,String)
         _lhsOmoduleDecl = rule343  ()
         _lhsOparamsCollect :: ParamMap
         _lhsOparamsCollect = rule344  ()
         _lhsOpragmas :: Options -> Options
         _lhsOpragmas = rule345  ()
         _lhsOquantCollect :: QuantMap
         _lhsOquantCollect = rule346  ()
         _lhsOsemPragmasCollect :: PragmaMap
         _lhsOsemPragmasCollect = rule347  ()
         _lhsOtypeSyns :: TypeSyns
         _lhsOtypeSyns = rule348  ()
         _lhsOuseMap :: Map NontermIdent (Map Identifier (String,String,String))
         _lhsOuseMap = rule349  ()
         _lhsOwrappers :: Set NontermIdent
         _lhsOwrappers = rule350  ()
         _lhsOattrDecls :: Map NontermIdent (Attributes, Attributes)
         _lhsOattrDecls = rule351 _lhsIattrDecls
         _lhsOattrs :: Map NontermIdent (Attributes, Attributes)
         _lhsOattrs = rule352 _lhsIattrs
         _lhsOdefSets :: Map Identifier (Set NontermIdent,Set Identifier)
         _lhsOdefSets = rule353 _lhsIdefSets
         _setOallFields = rule354 _lhsIallFields
         _setOallNonterminals = rule355 _lhsIallNonterminals
         _setOdefinedSets = rule356 _lhsIdefinedSets
         __result_ = T_Elem_vOut16 _lhsOattrDecls _lhsOattrOrderCollect _lhsOattrs _lhsOblocks _lhsOcollectedArounds _lhsOcollectedAugments _lhsOcollectedConParams _lhsOcollectedConstraints _lhsOcollectedConstructorsMap _lhsOcollectedFields _lhsOcollectedInsts _lhsOcollectedMacros _lhsOcollectedMerges _lhsOcollectedNames _lhsOcollectedRules _lhsOcollectedSetNames _lhsOcollectedSigs _lhsOcollectedUniques _lhsOctxCollect _lhsOdefSets _lhsOderivings _lhsOerrors _lhsOmoduleDecl _lhsOparamsCollect _lhsOpragmas _lhsOquantCollect _lhsOsemPragmasCollect _lhsOtypeSyns _lhsOuseMap _lhsOwrappers
         in __result_ )
     in C_Elem_s17 v16
   {-# INLINE rule324 #-}
   {-# LINE 1016 "./src-ag/Transform.ag" #-}
   rule324 = \ ((_setInontSet) :: Set NontermIdent) classes_ ->
                               {-# LINE 1016 "./src-ag/Transform.ag" #-}
                               Map.fromList [(nt,Set.fromList classes_) | nt <- Set.toList _setInontSet]
                               {-# LINE 2941 "dist/build/Transform.hs"#-}
   {-# INLINE rule325 #-}
   rule325 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule326 #-}
   rule326 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule327 #-}
   rule327 = \  (_ :: ()) ->
     []
   {-# INLINE rule328 #-}
   rule328 = \  (_ :: ()) ->
     []
   {-# INLINE rule329 #-}
   rule329 = \  (_ :: ()) ->
     []
   {-# INLINE rule330 #-}
   rule330 = \  (_ :: ()) ->
     []
   {-# INLINE rule331 #-}
   rule331 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule332 #-}
   rule332 = \  (_ :: ()) ->
     []
   {-# INLINE rule333 #-}
   rule333 = \  (_ :: ()) ->
     []
   {-# INLINE rule334 #-}
   rule334 = \  (_ :: ()) ->
     []
   {-# INLINE rule335 #-}
   rule335 = \  (_ :: ()) ->
     []
   {-# INLINE rule336 #-}
   rule336 = \ ((_setIcollectedNames) :: Set Identifier) ->
     _setIcollectedNames
   {-# INLINE rule337 #-}
   rule337 = \  (_ :: ()) ->
     []
   {-# INLINE rule338 #-}
   rule338 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule339 #-}
   rule339 = \  (_ :: ()) ->
     []
   {-# INLINE rule340 #-}
   rule340 = \  (_ :: ()) ->
     []
   {-# INLINE rule341 #-}
   rule341 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule342 #-}
   rule342 = \ ((_setIerrors) :: Seq Error) ->
     _setIerrors
   {-# INLINE rule343 #-}
   rule343 = \  (_ :: ()) ->
     mzero
   {-# INLINE rule344 #-}
   rule344 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule345 #-}
   rule345 = \  (_ :: ()) ->
     id
   {-# INLINE rule346 #-}
   rule346 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule347 #-}
   rule347 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule348 #-}
   rule348 = \  (_ :: ()) ->
     []
   {-# INLINE rule349 #-}
   rule349 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule350 #-}
   rule350 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule351 #-}
   rule351 = \ ((_lhsIattrDecls) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIattrDecls
   {-# INLINE rule352 #-}
   rule352 = \ ((_lhsIattrs) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIattrs
   {-# INLINE rule353 #-}
   rule353 = \ ((_lhsIdefSets) :: Map Identifier (Set NontermIdent,Set Identifier)) ->
     _lhsIdefSets
   {-# INLINE rule354 #-}
   rule354 = \ ((_lhsIallFields) :: DataTypes) ->
     _lhsIallFields
   {-# INLINE rule355 #-}
   rule355 = \ ((_lhsIallNonterminals) :: Set NontermIdent) ->
     _lhsIallNonterminals
   {-# INLINE rule356 #-}
   rule356 = \ ((_lhsIdefinedSets) :: DefinedSets) ->
     _lhsIdefinedSets
{-# NOINLINE sem_Elem_Wrapper #-}
sem_Elem_Wrapper :: (Pos) -> T_NontSet  -> T_Elem 
sem_Elem_Wrapper _ arg_set_ = T_Elem (return st17) where
   {-# NOINLINE st17 #-}
   st17 = let
      v16 :: T_Elem_v16 
      v16 = \ (T_Elem_vIn16 _lhsIallAttrDecls _lhsIallAttrs _lhsIallConstructors _lhsIallFields _lhsIallNonterminals _lhsIattrDecls _lhsIattrs _lhsIdefSets _lhsIdefinedSets _lhsIoptions) -> ( let
         _setX29 = Control.Monad.Identity.runIdentity (attach_T_NontSet (arg_set_))
         (T_NontSet_vOut28 _setIcollectedNames _setIerrors _setInontSet) = inv_NontSet_s29 _setX29 (T_NontSet_vIn28 _setOallFields _setOallNonterminals _setOdefinedSets)
         _lhsOwrappers :: Set NontermIdent
         _lhsOwrappers = rule357 _setInontSet
         _lhsOattrOrderCollect :: AttrOrderMap
         _lhsOattrOrderCollect = rule358  ()
         _lhsOblocks :: Blocks
         _lhsOblocks = rule359  ()
         _lhsOcollectedArounds :: [ (NontermIdent, ConstructorIdent, [AroundInfo])  ]
         _lhsOcollectedArounds = rule360  ()
         _lhsOcollectedAugments :: [ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]
         _lhsOcollectedAugments = rule361  ()
         _lhsOcollectedConParams :: [(NontermIdent, ConstructorIdent, Set Identifier)]
         _lhsOcollectedConParams = rule362  ()
         _lhsOcollectedConstraints :: [(NontermIdent, ConstructorIdent, [Type])]
         _lhsOcollectedConstraints = rule363  ()
         _lhsOcollectedConstructorsMap :: Map NontermIdent (Set ConstructorIdent)
         _lhsOcollectedConstructorsMap = rule364  ()
         _lhsOcollectedFields :: [(NontermIdent, ConstructorIdent, FieldMap)]
         _lhsOcollectedFields = rule365  ()
         _lhsOcollectedInsts :: [ (NontermIdent, ConstructorIdent, [Identifier]) ]
         _lhsOcollectedInsts = rule366  ()
         _lhsOcollectedMacros :: [(NontermIdent, ConstructorIdent, MaybeMacro)]
         _lhsOcollectedMacros = rule367  ()
         _lhsOcollectedMerges :: [ (NontermIdent, ConstructorIdent, [MergeInfo])   ]
         _lhsOcollectedMerges = rule368  ()
         _lhsOcollectedNames :: Set Identifier
         _lhsOcollectedNames = rule369 _setIcollectedNames
         _lhsOcollectedRules :: [ (NontermIdent, ConstructorIdent, RuleInfo)]
         _lhsOcollectedRules = rule370  ()
         _lhsOcollectedSetNames :: Set Identifier
         _lhsOcollectedSetNames = rule371  ()
         _lhsOcollectedSigs :: [ (NontermIdent, ConstructorIdent, SigInfo) ]
         _lhsOcollectedSigs = rule372  ()
         _lhsOcollectedUniques :: [ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]
         _lhsOcollectedUniques = rule373  ()
         _lhsOctxCollect :: ContextMap
         _lhsOctxCollect = rule374  ()
         _lhsOderivings :: Derivings
         _lhsOderivings = rule375  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule376 _setIerrors
         _lhsOmoduleDecl :: Maybe (String,String,String)
         _lhsOmoduleDecl = rule377  ()
         _lhsOparamsCollect :: ParamMap
         _lhsOparamsCollect = rule378  ()
         _lhsOpragmas :: Options -> Options
         _lhsOpragmas = rule379  ()
         _lhsOquantCollect :: QuantMap
         _lhsOquantCollect = rule380  ()
         _lhsOsemPragmasCollect :: PragmaMap
         _lhsOsemPragmasCollect = rule381  ()
         _lhsOtypeSyns :: TypeSyns
         _lhsOtypeSyns = rule382  ()
         _lhsOuseMap :: Map NontermIdent (Map Identifier (String,String,String))
         _lhsOuseMap = rule383  ()
         _lhsOattrDecls :: Map NontermIdent (Attributes, Attributes)
         _lhsOattrDecls = rule384 _lhsIattrDecls
         _lhsOattrs :: Map NontermIdent (Attributes, Attributes)
         _lhsOattrs = rule385 _lhsIattrs
         _lhsOdefSets :: Map Identifier (Set NontermIdent,Set Identifier)
         _lhsOdefSets = rule386 _lhsIdefSets
         _setOallFields = rule387 _lhsIallFields
         _setOallNonterminals = rule388 _lhsIallNonterminals
         _setOdefinedSets = rule389 _lhsIdefinedSets
         __result_ = T_Elem_vOut16 _lhsOattrDecls _lhsOattrOrderCollect _lhsOattrs _lhsOblocks _lhsOcollectedArounds _lhsOcollectedAugments _lhsOcollectedConParams _lhsOcollectedConstraints _lhsOcollectedConstructorsMap _lhsOcollectedFields _lhsOcollectedInsts _lhsOcollectedMacros _lhsOcollectedMerges _lhsOcollectedNames _lhsOcollectedRules _lhsOcollectedSetNames _lhsOcollectedSigs _lhsOcollectedUniques _lhsOctxCollect _lhsOdefSets _lhsOderivings _lhsOerrors _lhsOmoduleDecl _lhsOparamsCollect _lhsOpragmas _lhsOquantCollect _lhsOsemPragmasCollect _lhsOtypeSyns _lhsOuseMap _lhsOwrappers
         in __result_ )
     in C_Elem_s17 v16
   {-# INLINE rule357 #-}
   {-# LINE 789 "./src-ag/Transform.ag" #-}
   rule357 = \ ((_setInontSet) :: Set NontermIdent) ->
                             {-# LINE 789 "./src-ag/Transform.ag" #-}
                             _setInontSet
                             {-# LINE 3118 "dist/build/Transform.hs"#-}
   {-# INLINE rule358 #-}
   rule358 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule359 #-}
   rule359 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule360 #-}
   rule360 = \  (_ :: ()) ->
     []
   {-# INLINE rule361 #-}
   rule361 = \  (_ :: ()) ->
     []
   {-# INLINE rule362 #-}
   rule362 = \  (_ :: ()) ->
     []
   {-# INLINE rule363 #-}
   rule363 = \  (_ :: ()) ->
     []
   {-# INLINE rule364 #-}
   rule364 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule365 #-}
   rule365 = \  (_ :: ()) ->
     []
   {-# INLINE rule366 #-}
   rule366 = \  (_ :: ()) ->
     []
   {-# INLINE rule367 #-}
   rule367 = \  (_ :: ()) ->
     []
   {-# INLINE rule368 #-}
   rule368 = \  (_ :: ()) ->
     []
   {-# INLINE rule369 #-}
   rule369 = \ ((_setIcollectedNames) :: Set Identifier) ->
     _setIcollectedNames
   {-# INLINE rule370 #-}
   rule370 = \  (_ :: ()) ->
     []
   {-# INLINE rule371 #-}
   rule371 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule372 #-}
   rule372 = \  (_ :: ()) ->
     []
   {-# INLINE rule373 #-}
   rule373 = \  (_ :: ()) ->
     []
   {-# INLINE rule374 #-}
   rule374 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule375 #-}
   rule375 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule376 #-}
   rule376 = \ ((_setIerrors) :: Seq Error) ->
     _setIerrors
   {-# INLINE rule377 #-}
   rule377 = \  (_ :: ()) ->
     mzero
   {-# INLINE rule378 #-}
   rule378 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule379 #-}
   rule379 = \  (_ :: ()) ->
     id
   {-# INLINE rule380 #-}
   rule380 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule381 #-}
   rule381 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule382 #-}
   rule382 = \  (_ :: ()) ->
     []
   {-# INLINE rule383 #-}
   rule383 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule384 #-}
   rule384 = \ ((_lhsIattrDecls) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIattrDecls
   {-# INLINE rule385 #-}
   rule385 = \ ((_lhsIattrs) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIattrs
   {-# INLINE rule386 #-}
   rule386 = \ ((_lhsIdefSets) :: Map Identifier (Set NontermIdent,Set Identifier)) ->
     _lhsIdefSets
   {-# INLINE rule387 #-}
   rule387 = \ ((_lhsIallFields) :: DataTypes) ->
     _lhsIallFields
   {-# INLINE rule388 #-}
   rule388 = \ ((_lhsIallNonterminals) :: Set NontermIdent) ->
     _lhsIallNonterminals
   {-# INLINE rule389 #-}
   rule389 = \ ((_lhsIdefinedSets) :: DefinedSets) ->
     _lhsIdefinedSets
{-# NOINLINE sem_Elem_Nocatas #-}
sem_Elem_Nocatas :: (Pos) -> T_NontSet  -> T_Elem 
sem_Elem_Nocatas _ arg_set_ = T_Elem (return st17) where
   {-# NOINLINE st17 #-}
   st17 = let
      v16 :: T_Elem_v16 
      v16 = \ (T_Elem_vIn16 _lhsIallAttrDecls _lhsIallAttrs _lhsIallConstructors _lhsIallFields _lhsIallNonterminals _lhsIattrDecls _lhsIattrs _lhsIdefSets _lhsIdefinedSets _lhsIoptions) -> ( let
         _setX29 = Control.Monad.Identity.runIdentity (attach_T_NontSet (arg_set_))
         (T_NontSet_vOut28 _setIcollectedNames _setIerrors _setInontSet) = inv_NontSet_s29 _setX29 (T_NontSet_vIn28 _setOallFields _setOallNonterminals _setOdefinedSets)
         _lhsOpragmas :: Options -> Options
         _lhsOpragmas = rule390 _setInontSet
         _lhsOattrOrderCollect :: AttrOrderMap
         _lhsOattrOrderCollect = rule391  ()
         _lhsOblocks :: Blocks
         _lhsOblocks = rule392  ()
         _lhsOcollectedArounds :: [ (NontermIdent, ConstructorIdent, [AroundInfo])  ]
         _lhsOcollectedArounds = rule393  ()
         _lhsOcollectedAugments :: [ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]
         _lhsOcollectedAugments = rule394  ()
         _lhsOcollectedConParams :: [(NontermIdent, ConstructorIdent, Set Identifier)]
         _lhsOcollectedConParams = rule395  ()
         _lhsOcollectedConstraints :: [(NontermIdent, ConstructorIdent, [Type])]
         _lhsOcollectedConstraints = rule396  ()
         _lhsOcollectedConstructorsMap :: Map NontermIdent (Set ConstructorIdent)
         _lhsOcollectedConstructorsMap = rule397  ()
         _lhsOcollectedFields :: [(NontermIdent, ConstructorIdent, FieldMap)]
         _lhsOcollectedFields = rule398  ()
         _lhsOcollectedInsts :: [ (NontermIdent, ConstructorIdent, [Identifier]) ]
         _lhsOcollectedInsts = rule399  ()
         _lhsOcollectedMacros :: [(NontermIdent, ConstructorIdent, MaybeMacro)]
         _lhsOcollectedMacros = rule400  ()
         _lhsOcollectedMerges :: [ (NontermIdent, ConstructorIdent, [MergeInfo])   ]
         _lhsOcollectedMerges = rule401  ()
         _lhsOcollectedNames :: Set Identifier
         _lhsOcollectedNames = rule402 _setIcollectedNames
         _lhsOcollectedRules :: [ (NontermIdent, ConstructorIdent, RuleInfo)]
         _lhsOcollectedRules = rule403  ()
         _lhsOcollectedSetNames :: Set Identifier
         _lhsOcollectedSetNames = rule404  ()
         _lhsOcollectedSigs :: [ (NontermIdent, ConstructorIdent, SigInfo) ]
         _lhsOcollectedSigs = rule405  ()
         _lhsOcollectedUniques :: [ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]
         _lhsOcollectedUniques = rule406  ()
         _lhsOctxCollect :: ContextMap
         _lhsOctxCollect = rule407  ()
         _lhsOderivings :: Derivings
         _lhsOderivings = rule408  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule409 _setIerrors
         _lhsOmoduleDecl :: Maybe (String,String,String)
         _lhsOmoduleDecl = rule410  ()
         _lhsOparamsCollect :: ParamMap
         _lhsOparamsCollect = rule411  ()
         _lhsOquantCollect :: QuantMap
         _lhsOquantCollect = rule412  ()
         _lhsOsemPragmasCollect :: PragmaMap
         _lhsOsemPragmasCollect = rule413  ()
         _lhsOtypeSyns :: TypeSyns
         _lhsOtypeSyns = rule414  ()
         _lhsOuseMap :: Map NontermIdent (Map Identifier (String,String,String))
         _lhsOuseMap = rule415  ()
         _lhsOwrappers :: Set NontermIdent
         _lhsOwrappers = rule416  ()
         _lhsOattrDecls :: Map NontermIdent (Attributes, Attributes)
         _lhsOattrDecls = rule417 _lhsIattrDecls
         _lhsOattrs :: Map NontermIdent (Attributes, Attributes)
         _lhsOattrs = rule418 _lhsIattrs
         _lhsOdefSets :: Map Identifier (Set NontermIdent,Set Identifier)
         _lhsOdefSets = rule419 _lhsIdefSets
         _setOallFields = rule420 _lhsIallFields
         _setOallNonterminals = rule421 _lhsIallNonterminals
         _setOdefinedSets = rule422 _lhsIdefinedSets
         __result_ = T_Elem_vOut16 _lhsOattrDecls _lhsOattrOrderCollect _lhsOattrs _lhsOblocks _lhsOcollectedArounds _lhsOcollectedAugments _lhsOcollectedConParams _lhsOcollectedConstraints _lhsOcollectedConstructorsMap _lhsOcollectedFields _lhsOcollectedInsts _lhsOcollectedMacros _lhsOcollectedMerges _lhsOcollectedNames _lhsOcollectedRules _lhsOcollectedSetNames _lhsOcollectedSigs _lhsOcollectedUniques _lhsOctxCollect _lhsOdefSets _lhsOderivings _lhsOerrors _lhsOmoduleDecl _lhsOparamsCollect _lhsOpragmas _lhsOquantCollect _lhsOsemPragmasCollect _lhsOtypeSyns _lhsOuseMap _lhsOwrappers
         in __result_ )
     in C_Elem_s17 v16
   {-# INLINE rule390 #-}
   {-# LINE 796 "./src-ag/Transform.ag" #-}
   rule390 = \ ((_setInontSet) :: Set NontermIdent) ->
                             {-# LINE 796 "./src-ag/Transform.ag" #-}
                             \o -> o { nocatas = _setInontSet `Set.union` nocatas o }
                             {-# LINE 3295 "dist/build/Transform.hs"#-}
   {-# INLINE rule391 #-}
   rule391 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule392 #-}
   rule392 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule393 #-}
   rule393 = \  (_ :: ()) ->
     []
   {-# INLINE rule394 #-}
   rule394 = \  (_ :: ()) ->
     []
   {-# INLINE rule395 #-}
   rule395 = \  (_ :: ()) ->
     []
   {-# INLINE rule396 #-}
   rule396 = \  (_ :: ()) ->
     []
   {-# INLINE rule397 #-}
   rule397 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule398 #-}
   rule398 = \  (_ :: ()) ->
     []
   {-# INLINE rule399 #-}
   rule399 = \  (_ :: ()) ->
     []
   {-# INLINE rule400 #-}
   rule400 = \  (_ :: ()) ->
     []
   {-# INLINE rule401 #-}
   rule401 = \  (_ :: ()) ->
     []
   {-# INLINE rule402 #-}
   rule402 = \ ((_setIcollectedNames) :: Set Identifier) ->
     _setIcollectedNames
   {-# INLINE rule403 #-}
   rule403 = \  (_ :: ()) ->
     []
   {-# INLINE rule404 #-}
   rule404 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule405 #-}
   rule405 = \  (_ :: ()) ->
     []
   {-# INLINE rule406 #-}
   rule406 = \  (_ :: ()) ->
     []
   {-# INLINE rule407 #-}
   rule407 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule408 #-}
   rule408 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule409 #-}
   rule409 = \ ((_setIerrors) :: Seq Error) ->
     _setIerrors
   {-# INLINE rule410 #-}
   rule410 = \  (_ :: ()) ->
     mzero
   {-# INLINE rule411 #-}
   rule411 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule412 #-}
   rule412 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule413 #-}
   rule413 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule414 #-}
   rule414 = \  (_ :: ()) ->
     []
   {-# INLINE rule415 #-}
   rule415 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule416 #-}
   rule416 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule417 #-}
   rule417 = \ ((_lhsIattrDecls) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIattrDecls
   {-# INLINE rule418 #-}
   rule418 = \ ((_lhsIattrs) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIattrs
   {-# INLINE rule419 #-}
   rule419 = \ ((_lhsIdefSets) :: Map Identifier (Set NontermIdent,Set Identifier)) ->
     _lhsIdefSets
   {-# INLINE rule420 #-}
   rule420 = \ ((_lhsIallFields) :: DataTypes) ->
     _lhsIallFields
   {-# INLINE rule421 #-}
   rule421 = \ ((_lhsIallNonterminals) :: Set NontermIdent) ->
     _lhsIallNonterminals
   {-# INLINE rule422 #-}
   rule422 = \ ((_lhsIdefinedSets) :: DefinedSets) ->
     _lhsIdefinedSets
{-# NOINLINE sem_Elem_Pragma #-}
sem_Elem_Pragma :: (Pos) -> ([NontermIdent]) -> T_Elem 
sem_Elem_Pragma _ arg_names_ = T_Elem (return st17) where
   {-# NOINLINE st17 #-}
   st17 = let
      v16 :: T_Elem_v16 
      v16 = \ (T_Elem_vIn16 _lhsIallAttrDecls _lhsIallAttrs _lhsIallConstructors _lhsIallFields _lhsIallNonterminals _lhsIattrDecls _lhsIattrs _lhsIdefSets _lhsIdefinedSets _lhsIoptions) -> ( let
         _lhsOpragmas :: Options -> Options
         _lhsOpragmas = rule423 arg_names_
         _lhsOattrOrderCollect :: AttrOrderMap
         _lhsOattrOrderCollect = rule424  ()
         _lhsOblocks :: Blocks
         _lhsOblocks = rule425  ()
         _lhsOcollectedArounds :: [ (NontermIdent, ConstructorIdent, [AroundInfo])  ]
         _lhsOcollectedArounds = rule426  ()
         _lhsOcollectedAugments :: [ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]
         _lhsOcollectedAugments = rule427  ()
         _lhsOcollectedConParams :: [(NontermIdent, ConstructorIdent, Set Identifier)]
         _lhsOcollectedConParams = rule428  ()
         _lhsOcollectedConstraints :: [(NontermIdent, ConstructorIdent, [Type])]
         _lhsOcollectedConstraints = rule429  ()
         _lhsOcollectedConstructorsMap :: Map NontermIdent (Set ConstructorIdent)
         _lhsOcollectedConstructorsMap = rule430  ()
         _lhsOcollectedFields :: [(NontermIdent, ConstructorIdent, FieldMap)]
         _lhsOcollectedFields = rule431  ()
         _lhsOcollectedInsts :: [ (NontermIdent, ConstructorIdent, [Identifier]) ]
         _lhsOcollectedInsts = rule432  ()
         _lhsOcollectedMacros :: [(NontermIdent, ConstructorIdent, MaybeMacro)]
         _lhsOcollectedMacros = rule433  ()
         _lhsOcollectedMerges :: [ (NontermIdent, ConstructorIdent, [MergeInfo])   ]
         _lhsOcollectedMerges = rule434  ()
         _lhsOcollectedNames :: Set Identifier
         _lhsOcollectedNames = rule435  ()
         _lhsOcollectedRules :: [ (NontermIdent, ConstructorIdent, RuleInfo)]
         _lhsOcollectedRules = rule436  ()
         _lhsOcollectedSetNames :: Set Identifier
         _lhsOcollectedSetNames = rule437  ()
         _lhsOcollectedSigs :: [ (NontermIdent, ConstructorIdent, SigInfo) ]
         _lhsOcollectedSigs = rule438  ()
         _lhsOcollectedUniques :: [ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]
         _lhsOcollectedUniques = rule439  ()
         _lhsOctxCollect :: ContextMap
         _lhsOctxCollect = rule440  ()
         _lhsOderivings :: Derivings
         _lhsOderivings = rule441  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule442  ()
         _lhsOmoduleDecl :: Maybe (String,String,String)
         _lhsOmoduleDecl = rule443  ()
         _lhsOparamsCollect :: ParamMap
         _lhsOparamsCollect = rule444  ()
         _lhsOquantCollect :: QuantMap
         _lhsOquantCollect = rule445  ()
         _lhsOsemPragmasCollect :: PragmaMap
         _lhsOsemPragmasCollect = rule446  ()
         _lhsOtypeSyns :: TypeSyns
         _lhsOtypeSyns = rule447  ()
         _lhsOuseMap :: Map NontermIdent (Map Identifier (String,String,String))
         _lhsOuseMap = rule448  ()
         _lhsOwrappers :: Set NontermIdent
         _lhsOwrappers = rule449  ()
         _lhsOattrDecls :: Map NontermIdent (Attributes, Attributes)
         _lhsOattrDecls = rule450 _lhsIattrDecls
         _lhsOattrs :: Map NontermIdent (Attributes, Attributes)
         _lhsOattrs = rule451 _lhsIattrs
         _lhsOdefSets :: Map Identifier (Set NontermIdent,Set Identifier)
         _lhsOdefSets = rule452 _lhsIdefSets
         __result_ = T_Elem_vOut16 _lhsOattrDecls _lhsOattrOrderCollect _lhsOattrs _lhsOblocks _lhsOcollectedArounds _lhsOcollectedAugments _lhsOcollectedConParams _lhsOcollectedConstraints _lhsOcollectedConstructorsMap _lhsOcollectedFields _lhsOcollectedInsts _lhsOcollectedMacros _lhsOcollectedMerges _lhsOcollectedNames _lhsOcollectedRules _lhsOcollectedSetNames _lhsOcollectedSigs _lhsOcollectedUniques _lhsOctxCollect _lhsOdefSets _lhsOderivings _lhsOerrors _lhsOmoduleDecl _lhsOparamsCollect _lhsOpragmas _lhsOquantCollect _lhsOsemPragmasCollect _lhsOtypeSyns _lhsOuseMap _lhsOwrappers
         in __result_ )
     in C_Elem_s17 v16
   {-# INLINE rule423 #-}
   {-# LINE 805 "./src-ag/Transform.ag" #-}
   rule423 = \ names_ ->
                            {-# LINE 805 "./src-ag/Transform.ag" #-}
                            let mk n o = case getName n of
                                           "gencatas"     -> o { folds       = True  }
                                           "nogencatas"   -> o { folds       = False }
                                           "gendatas"     -> o { dataTypes   = True  }
                                           "datarecords"  -> o { dataRecords = True  }
                                           "nogendatas"   -> o { dataTypes   = False }
                                           "gensems"      -> o { semfuns     = True  }
                                           "nogensems"    -> o { semfuns     = False }
                                           "gentypesigs"  -> o { typeSigs    = True  }
                                           "nogentypesigs"-> o { typeSigs    = False }
                                           "nocycle"      -> o { withCycle   = False }
                                           "cycle"        -> o { withCycle   = True  }
                                           "nostrictdata" -> o { strictData  = False }
                                           "strictdata"   -> o { strictData  = True  }
                                           "nostrictcase" -> o { strictCases = False }
                                           "strictcase"   -> o { strictCases = True  }
                                           "strictercase" -> o { strictCases = True, stricterCases = True }
                                           "nostrictwrap" -> o { strictWrap  = False }
                                           "strictwrap"   -> o { strictWrap  = True  }
                                           "novisit"      -> o { visit       = False }
                                           "visit"        -> o { visit       = True  }
                                           "nocase"       -> o { cases       = False }
                                           "case"         -> o { cases       = True  }
                                           "noseq"        -> o { withSeq     = False }
                                           "seq"          -> o { withSeq     = True  }
                                           "nounbox"      -> o { unbox       = False }
                                           "unbox"        -> o { unbox       = True  }
                                           "bangpats"     -> o { bangpats    = True  }
                                           "breadthfirst" -> o { breadthFirst = True }
                                           "breadthfirstStrict" -> o { breadthFirstStrict = True }
                                           "nooptimize"   -> o { cases = False , visit = False }
                                           "optimize"     -> o { cases = True  , visit = True  }
                                           "strictsem"    -> o { strictSems = True }
                                           "gentraces"    -> o { genTraces = True }
                                           "genusetraces" -> o { genUseTraces = True }
                                           "splitsems"    -> o { splitSems = True }
                                           "gencostcentres" -> o { genCostCentres = True }
                                           "sepsemmods"   -> sepSemModsOpt o
                                           "genlinepragmas" -> o { genLinePragmas = True }
                                           "newtypes"       -> o { newtypes = True }
                                           "nonewtypes"     -> o { newtypes = False }
                                           "nooptimizations" -> o { noOptimizations = True }
                                           "kennedywarren"   -> o { kennedyWarren = True }
                                           "aspectag"        -> o { genAspectAG = True }
                                           'n':'o':'g':'r':'o':'u':'p':'_':atts
                                                             -> o { noGroup =  extract atts  ++ noGroup o }
                                           "rename"          -> o { rename = True }
                                           "parallel"        -> o { parallelInvoke = True }
                                           "monadicwrappers" -> o { monadicWrappers = True }
                                           "dummytokenvisit" -> o { dummyTokenVisit = True }
                                           "tupleasdummytoken" -> o { tupleAsDummyToken = True }
                                           "stateasdummytoken" -> o { tupleAsDummyToken = False }
                                           "strictdummytoken" -> o { strictDummyToken = True }
                                           "noperruletypesigs" -> o { noPerRuleTypeSigs = True }
                                           "noperstatetypesigs" -> o { noPerStateTypeSigs = True }
                                           "noeagerblackholing" -> o { noEagerBlackholing = True }
                                           "noperrulecostcentres" -> o { noPerRuleCostCentres = True }
                                           "nopervisitcostcentres" -> o { noPerVisitCostCentres = True }
                                           "helpinlining" -> o { helpInlining = True }
                                           "noinlinepragmas" -> o { noInlinePragmas = True }
                                           "aggressiveinlinepragmas" -> o { aggressiveInlinePragmas = True }
                                           "latehigherorderbindings" -> o { lateHigherOrderBinding = True }
                                           "ocaml"                   -> ocamlOpt o
                                           s              -> trace ("uuagc: ignoring unknown pragma: " ++ s) o
                            in \o -> foldr mk o names_
                            {-# LINE 3531 "dist/build/Transform.hs"#-}
   {-# INLINE rule424 #-}
   rule424 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule425 #-}
   rule425 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule426 #-}
   rule426 = \  (_ :: ()) ->
     []
   {-# INLINE rule427 #-}
   rule427 = \  (_ :: ()) ->
     []
   {-# INLINE rule428 #-}
   rule428 = \  (_ :: ()) ->
     []
   {-# INLINE rule429 #-}
   rule429 = \  (_ :: ()) ->
     []
   {-# INLINE rule430 #-}
   rule430 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule431 #-}
   rule431 = \  (_ :: ()) ->
     []
   {-# INLINE rule432 #-}
   rule432 = \  (_ :: ()) ->
     []
   {-# INLINE rule433 #-}
   rule433 = \  (_ :: ()) ->
     []
   {-# INLINE rule434 #-}
   rule434 = \  (_ :: ()) ->
     []
   {-# INLINE rule435 #-}
   rule435 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule436 #-}
   rule436 = \  (_ :: ()) ->
     []
   {-# INLINE rule437 #-}
   rule437 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule438 #-}
   rule438 = \  (_ :: ()) ->
     []
   {-# INLINE rule439 #-}
   rule439 = \  (_ :: ()) ->
     []
   {-# INLINE rule440 #-}
   rule440 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule441 #-}
   rule441 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule442 #-}
   rule442 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule443 #-}
   rule443 = \  (_ :: ()) ->
     mzero
   {-# INLINE rule444 #-}
   rule444 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule445 #-}
   rule445 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule446 #-}
   rule446 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule447 #-}
   rule447 = \  (_ :: ()) ->
     []
   {-# INLINE rule448 #-}
   rule448 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule449 #-}
   rule449 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule450 #-}
   rule450 = \ ((_lhsIattrDecls) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIattrDecls
   {-# INLINE rule451 #-}
   rule451 = \ ((_lhsIattrs) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIattrs
   {-# INLINE rule452 #-}
   rule452 = \ ((_lhsIdefSets) :: Map Identifier (Set NontermIdent,Set Identifier)) ->
     _lhsIdefSets
{-# NOINLINE sem_Elem_Module #-}
sem_Elem_Module :: (Pos) -> (String) -> (String) -> (String) -> T_Elem 
sem_Elem_Module _ arg_name_ arg_exports_ arg_imports_ = T_Elem (return st17) where
   {-# NOINLINE st17 #-}
   st17 = let
      v16 :: T_Elem_v16 
      v16 = \ (T_Elem_vIn16 _lhsIallAttrDecls _lhsIallAttrs _lhsIallConstructors _lhsIallFields _lhsIallNonterminals _lhsIattrDecls _lhsIattrs _lhsIdefSets _lhsIdefinedSets _lhsIoptions) -> ( let
         _lhsOmoduleDecl :: Maybe (String,String,String)
         _lhsOmoduleDecl = rule453 arg_exports_ arg_imports_ arg_name_
         _lhsOattrOrderCollect :: AttrOrderMap
         _lhsOattrOrderCollect = rule454  ()
         _lhsOblocks :: Blocks
         _lhsOblocks = rule455  ()
         _lhsOcollectedArounds :: [ (NontermIdent, ConstructorIdent, [AroundInfo])  ]
         _lhsOcollectedArounds = rule456  ()
         _lhsOcollectedAugments :: [ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]
         _lhsOcollectedAugments = rule457  ()
         _lhsOcollectedConParams :: [(NontermIdent, ConstructorIdent, Set Identifier)]
         _lhsOcollectedConParams = rule458  ()
         _lhsOcollectedConstraints :: [(NontermIdent, ConstructorIdent, [Type])]
         _lhsOcollectedConstraints = rule459  ()
         _lhsOcollectedConstructorsMap :: Map NontermIdent (Set ConstructorIdent)
         _lhsOcollectedConstructorsMap = rule460  ()
         _lhsOcollectedFields :: [(NontermIdent, ConstructorIdent, FieldMap)]
         _lhsOcollectedFields = rule461  ()
         _lhsOcollectedInsts :: [ (NontermIdent, ConstructorIdent, [Identifier]) ]
         _lhsOcollectedInsts = rule462  ()
         _lhsOcollectedMacros :: [(NontermIdent, ConstructorIdent, MaybeMacro)]
         _lhsOcollectedMacros = rule463  ()
         _lhsOcollectedMerges :: [ (NontermIdent, ConstructorIdent, [MergeInfo])   ]
         _lhsOcollectedMerges = rule464  ()
         _lhsOcollectedNames :: Set Identifier
         _lhsOcollectedNames = rule465  ()
         _lhsOcollectedRules :: [ (NontermIdent, ConstructorIdent, RuleInfo)]
         _lhsOcollectedRules = rule466  ()
         _lhsOcollectedSetNames :: Set Identifier
         _lhsOcollectedSetNames = rule467  ()
         _lhsOcollectedSigs :: [ (NontermIdent, ConstructorIdent, SigInfo) ]
         _lhsOcollectedSigs = rule468  ()
         _lhsOcollectedUniques :: [ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]
         _lhsOcollectedUniques = rule469  ()
         _lhsOctxCollect :: ContextMap
         _lhsOctxCollect = rule470  ()
         _lhsOderivings :: Derivings
         _lhsOderivings = rule471  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule472  ()
         _lhsOparamsCollect :: ParamMap
         _lhsOparamsCollect = rule473  ()
         _lhsOpragmas :: Options -> Options
         _lhsOpragmas = rule474  ()
         _lhsOquantCollect :: QuantMap
         _lhsOquantCollect = rule475  ()
         _lhsOsemPragmasCollect :: PragmaMap
         _lhsOsemPragmasCollect = rule476  ()
         _lhsOtypeSyns :: TypeSyns
         _lhsOtypeSyns = rule477  ()
         _lhsOuseMap :: Map NontermIdent (Map Identifier (String,String,String))
         _lhsOuseMap = rule478  ()
         _lhsOwrappers :: Set NontermIdent
         _lhsOwrappers = rule479  ()
         _lhsOattrDecls :: Map NontermIdent (Attributes, Attributes)
         _lhsOattrDecls = rule480 _lhsIattrDecls
         _lhsOattrs :: Map NontermIdent (Attributes, Attributes)
         _lhsOattrs = rule481 _lhsIattrs
         _lhsOdefSets :: Map Identifier (Set NontermIdent,Set Identifier)
         _lhsOdefSets = rule482 _lhsIdefSets
         __result_ = T_Elem_vOut16 _lhsOattrDecls _lhsOattrOrderCollect _lhsOattrs _lhsOblocks _lhsOcollectedArounds _lhsOcollectedAugments _lhsOcollectedConParams _lhsOcollectedConstraints _lhsOcollectedConstructorsMap _lhsOcollectedFields _lhsOcollectedInsts _lhsOcollectedMacros _lhsOcollectedMerges _lhsOcollectedNames _lhsOcollectedRules _lhsOcollectedSetNames _lhsOcollectedSigs _lhsOcollectedUniques _lhsOctxCollect _lhsOdefSets _lhsOderivings _lhsOerrors _lhsOmoduleDecl _lhsOparamsCollect _lhsOpragmas _lhsOquantCollect _lhsOsemPragmasCollect _lhsOtypeSyns _lhsOuseMap _lhsOwrappers
         in __result_ )
     in C_Elem_s17 v16
   {-# INLINE rule453 #-}
   {-# LINE 1213 "./src-ag/Transform.ag" #-}
   rule453 = \ exports_ imports_ name_ ->
                         {-# LINE 1213 "./src-ag/Transform.ag" #-}
                         Just (name_, exports_, imports_)
                         {-# LINE 3694 "dist/build/Transform.hs"#-}
   {-# INLINE rule454 #-}
   rule454 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule455 #-}
   rule455 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule456 #-}
   rule456 = \  (_ :: ()) ->
     []
   {-# INLINE rule457 #-}
   rule457 = \  (_ :: ()) ->
     []
   {-# INLINE rule458 #-}
   rule458 = \  (_ :: ()) ->
     []
   {-# INLINE rule459 #-}
   rule459 = \  (_ :: ()) ->
     []
   {-# INLINE rule460 #-}
   rule460 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule461 #-}
   rule461 = \  (_ :: ()) ->
     []
   {-# INLINE rule462 #-}
   rule462 = \  (_ :: ()) ->
     []
   {-# INLINE rule463 #-}
   rule463 = \  (_ :: ()) ->
     []
   {-# INLINE rule464 #-}
   rule464 = \  (_ :: ()) ->
     []
   {-# INLINE rule465 #-}
   rule465 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule466 #-}
   rule466 = \  (_ :: ()) ->
     []
   {-# INLINE rule467 #-}
   rule467 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule468 #-}
   rule468 = \  (_ :: ()) ->
     []
   {-# INLINE rule469 #-}
   rule469 = \  (_ :: ()) ->
     []
   {-# INLINE rule470 #-}
   rule470 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule471 #-}
   rule471 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule472 #-}
   rule472 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule473 #-}
   rule473 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule474 #-}
   rule474 = \  (_ :: ()) ->
     id
   {-# INLINE rule475 #-}
   rule475 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule476 #-}
   rule476 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule477 #-}
   rule477 = \  (_ :: ()) ->
     []
   {-# INLINE rule478 #-}
   rule478 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule479 #-}
   rule479 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule480 #-}
   rule480 = \ ((_lhsIattrDecls) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIattrDecls
   {-# INLINE rule481 #-}
   rule481 = \ ((_lhsIattrs) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIattrs
   {-# INLINE rule482 #-}
   rule482 = \ ((_lhsIdefSets) :: Map Identifier (Set NontermIdent,Set Identifier)) ->
     _lhsIdefSets

-- Elems -------------------------------------------------------
-- wrapper
data Inh_Elems  = Inh_Elems { allAttrDecls_Inh_Elems :: (Map NontermIdent (Attributes, Attributes)), allAttrs_Inh_Elems :: (Map NontermIdent (Attributes, Attributes)), allConstructors_Inh_Elems :: (Map NontermIdent (Set ConstructorIdent)), allFields_Inh_Elems :: (DataTypes), allNonterminals_Inh_Elems :: (Set NontermIdent), attrDecls_Inh_Elems :: (Map NontermIdent (Attributes, Attributes)), attrs_Inh_Elems :: (Map NontermIdent (Attributes, Attributes)), defSets_Inh_Elems :: (Map Identifier (Set NontermIdent,Set Identifier)), definedSets_Inh_Elems :: (DefinedSets), options_Inh_Elems :: (Options) }
data Syn_Elems  = Syn_Elems { attrDecls_Syn_Elems :: (Map NontermIdent (Attributes, Attributes)), attrOrderCollect_Syn_Elems :: (AttrOrderMap), attrs_Syn_Elems :: (Map NontermIdent (Attributes, Attributes)), blocks_Syn_Elems :: (Blocks), collectedArounds_Syn_Elems :: ([ (NontermIdent, ConstructorIdent, [AroundInfo])  ]), collectedAugments_Syn_Elems :: ([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]), collectedConParams_Syn_Elems :: ([(NontermIdent, ConstructorIdent, Set Identifier)]), collectedConstraints_Syn_Elems :: ([(NontermIdent, ConstructorIdent, [Type])]), collectedConstructorsMap_Syn_Elems :: (Map NontermIdent (Set ConstructorIdent)), collectedFields_Syn_Elems :: ([(NontermIdent, ConstructorIdent, FieldMap)]), collectedInsts_Syn_Elems :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ]), collectedMacros_Syn_Elems :: ([(NontermIdent, ConstructorIdent, MaybeMacro)]), collectedMerges_Syn_Elems :: ([ (NontermIdent, ConstructorIdent, [MergeInfo])   ]), collectedNames_Syn_Elems :: (Set Identifier), collectedRules_Syn_Elems :: ([ (NontermIdent, ConstructorIdent, RuleInfo)]), collectedSetNames_Syn_Elems :: (Set Identifier), collectedSigs_Syn_Elems :: ([ (NontermIdent, ConstructorIdent, SigInfo) ]), collectedUniques_Syn_Elems :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]), ctxCollect_Syn_Elems :: (ContextMap), defSets_Syn_Elems :: (Map Identifier (Set NontermIdent,Set Identifier)), derivings_Syn_Elems :: (Derivings), errors_Syn_Elems :: (Seq Error), moduleDecl_Syn_Elems :: (Maybe (String,String,String)), paramsCollect_Syn_Elems :: (ParamMap), pragmas_Syn_Elems :: (Options -> Options), quantCollect_Syn_Elems :: (QuantMap), semPragmasCollect_Syn_Elems :: (PragmaMap), typeSyns_Syn_Elems :: (TypeSyns), useMap_Syn_Elems :: (Map NontermIdent (Map Identifier (String,String,String))), wrappers_Syn_Elems :: (Set NontermIdent) }
{-# INLINABLE wrap_Elems #-}
wrap_Elems :: T_Elems  -> Inh_Elems  -> (Syn_Elems )
wrap_Elems (T_Elems act) (Inh_Elems _lhsIallAttrDecls _lhsIallAttrs _lhsIallConstructors _lhsIallFields _lhsIallNonterminals _lhsIattrDecls _lhsIattrs _lhsIdefSets _lhsIdefinedSets _lhsIoptions) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Elems_vIn19 _lhsIallAttrDecls _lhsIallAttrs _lhsIallConstructors _lhsIallFields _lhsIallNonterminals _lhsIattrDecls _lhsIattrs _lhsIdefSets _lhsIdefinedSets _lhsIoptions
        (T_Elems_vOut19 _lhsOattrDecls _lhsOattrOrderCollect _lhsOattrs _lhsOblocks _lhsOcollectedArounds _lhsOcollectedAugments _lhsOcollectedConParams _lhsOcollectedConstraints _lhsOcollectedConstructorsMap _lhsOcollectedFields _lhsOcollectedInsts _lhsOcollectedMacros _lhsOcollectedMerges _lhsOcollectedNames _lhsOcollectedRules _lhsOcollectedSetNames _lhsOcollectedSigs _lhsOcollectedUniques _lhsOctxCollect _lhsOdefSets _lhsOderivings _lhsOerrors _lhsOmoduleDecl _lhsOparamsCollect _lhsOpragmas _lhsOquantCollect _lhsOsemPragmasCollect _lhsOtypeSyns _lhsOuseMap _lhsOwrappers) <- return (inv_Elems_s20 sem arg)
        return (Syn_Elems _lhsOattrDecls _lhsOattrOrderCollect _lhsOattrs _lhsOblocks _lhsOcollectedArounds _lhsOcollectedAugments _lhsOcollectedConParams _lhsOcollectedConstraints _lhsOcollectedConstructorsMap _lhsOcollectedFields _lhsOcollectedInsts _lhsOcollectedMacros _lhsOcollectedMerges _lhsOcollectedNames _lhsOcollectedRules _lhsOcollectedSetNames _lhsOcollectedSigs _lhsOcollectedUniques _lhsOctxCollect _lhsOdefSets _lhsOderivings _lhsOerrors _lhsOmoduleDecl _lhsOparamsCollect _lhsOpragmas _lhsOquantCollect _lhsOsemPragmasCollect _lhsOtypeSyns _lhsOuseMap _lhsOwrappers)
   )

-- cata
{-# NOINLINE sem_Elems #-}
sem_Elems :: Elems  -> T_Elems 
sem_Elems list = Prelude.foldr sem_Elems_Cons sem_Elems_Nil (Prelude.map sem_Elem list)

-- semantic domain
newtype T_Elems  = T_Elems {
                           attach_T_Elems :: Identity (T_Elems_s20 )
                           }
newtype T_Elems_s20  = C_Elems_s20 {
                                   inv_Elems_s20 :: (T_Elems_v19 )
                                   }
data T_Elems_s21  = C_Elems_s21
type T_Elems_v19  = (T_Elems_vIn19 ) -> (T_Elems_vOut19 )
data T_Elems_vIn19  = T_Elems_vIn19 (Map NontermIdent (Attributes, Attributes)) (Map NontermIdent (Attributes, Attributes)) (Map NontermIdent (Set ConstructorIdent)) (DataTypes) (Set NontermIdent) (Map NontermIdent (Attributes, Attributes)) (Map NontermIdent (Attributes, Attributes)) (Map Identifier (Set NontermIdent,Set Identifier)) (DefinedSets) (Options)
data T_Elems_vOut19  = T_Elems_vOut19 (Map NontermIdent (Attributes, Attributes)) (AttrOrderMap) (Map NontermIdent (Attributes, Attributes)) (Blocks) ([ (NontermIdent, ConstructorIdent, [AroundInfo])  ]) ([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]) ([(NontermIdent, ConstructorIdent, Set Identifier)]) ([(NontermIdent, ConstructorIdent, [Type])]) (Map NontermIdent (Set ConstructorIdent)) ([(NontermIdent, ConstructorIdent, FieldMap)]) ([ (NontermIdent, ConstructorIdent, [Identifier]) ]) ([(NontermIdent, ConstructorIdent, MaybeMacro)]) ([ (NontermIdent, ConstructorIdent, [MergeInfo])   ]) (Set Identifier) ([ (NontermIdent, ConstructorIdent, RuleInfo)]) (Set Identifier) ([ (NontermIdent, ConstructorIdent, SigInfo) ]) ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]) (ContextMap) (Map Identifier (Set NontermIdent,Set Identifier)) (Derivings) (Seq Error) (Maybe (String,String,String)) (ParamMap) (Options -> Options) (QuantMap) (PragmaMap) (TypeSyns) (Map NontermIdent (Map Identifier (String,String,String))) (Set NontermIdent)
{-# NOINLINE sem_Elems_Cons #-}
sem_Elems_Cons :: T_Elem  -> T_Elems  -> T_Elems 
sem_Elems_Cons arg_hd_ arg_tl_ = T_Elems (return st20) where
   {-# NOINLINE st20 #-}
   st20 = let
      v19 :: T_Elems_v19 
      v19 = \ (T_Elems_vIn19 _lhsIallAttrDecls _lhsIallAttrs _lhsIallConstructors _lhsIallFields _lhsIallNonterminals _lhsIattrDecls _lhsIattrs _lhsIdefSets _lhsIdefinedSets _lhsIoptions) -> ( let
         _hdX17 = Control.Monad.Identity.runIdentity (attach_T_Elem (arg_hd_))
         _tlX20 = Control.Monad.Identity.runIdentity (attach_T_Elems (arg_tl_))
         (T_Elem_vOut16 _hdIattrDecls _hdIattrOrderCollect _hdIattrs _hdIblocks _hdIcollectedArounds _hdIcollectedAugments _hdIcollectedConParams _hdIcollectedConstraints _hdIcollectedConstructorsMap _hdIcollectedFields _hdIcollectedInsts _hdIcollectedMacros _hdIcollectedMerges _hdIcollectedNames _hdIcollectedRules _hdIcollectedSetNames _hdIcollectedSigs _hdIcollectedUniques _hdIctxCollect _hdIdefSets _hdIderivings _hdIerrors _hdImoduleDecl _hdIparamsCollect _hdIpragmas _hdIquantCollect _hdIsemPragmasCollect _hdItypeSyns _hdIuseMap _hdIwrappers) = inv_Elem_s17 _hdX17 (T_Elem_vIn16 _hdOallAttrDecls _hdOallAttrs _hdOallConstructors _hdOallFields _hdOallNonterminals _hdOattrDecls _hdOattrs _hdOdefSets _hdOdefinedSets _hdOoptions)
         (T_Elems_vOut19 _tlIattrDecls _tlIattrOrderCollect _tlIattrs _tlIblocks _tlIcollectedArounds _tlIcollectedAugments _tlIcollectedConParams _tlIcollectedConstraints _tlIcollectedConstructorsMap _tlIcollectedFields _tlIcollectedInsts _tlIcollectedMacros _tlIcollectedMerges _tlIcollectedNames _tlIcollectedRules _tlIcollectedSetNames _tlIcollectedSigs _tlIcollectedUniques _tlIctxCollect _tlIdefSets _tlIderivings _tlIerrors _tlImoduleDecl _tlIparamsCollect _tlIpragmas _tlIquantCollect _tlIsemPragmasCollect _tlItypeSyns _tlIuseMap _tlIwrappers) = inv_Elems_s20 _tlX20 (T_Elems_vIn19 _tlOallAttrDecls _tlOallAttrs _tlOallConstructors _tlOallFields _tlOallNonterminals _tlOattrDecls _tlOattrs _tlOdefSets _tlOdefinedSets _tlOoptions)
         _lhsOattrOrderCollect :: AttrOrderMap
         _lhsOattrOrderCollect = rule483 _hdIattrOrderCollect _tlIattrOrderCollect
         _lhsOblocks :: Blocks
         _lhsOblocks = rule484 _hdIblocks _tlIblocks
         _lhsOcollectedArounds :: [ (NontermIdent, ConstructorIdent, [AroundInfo])  ]
         _lhsOcollectedArounds = rule485 _hdIcollectedArounds _tlIcollectedArounds
         _lhsOcollectedAugments :: [ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]
         _lhsOcollectedAugments = rule486 _hdIcollectedAugments _tlIcollectedAugments
         _lhsOcollectedConParams :: [(NontermIdent, ConstructorIdent, Set Identifier)]
         _lhsOcollectedConParams = rule487 _hdIcollectedConParams _tlIcollectedConParams
         _lhsOcollectedConstraints :: [(NontermIdent, ConstructorIdent, [Type])]
         _lhsOcollectedConstraints = rule488 _hdIcollectedConstraints _tlIcollectedConstraints
         _lhsOcollectedConstructorsMap :: Map NontermIdent (Set ConstructorIdent)
         _lhsOcollectedConstructorsMap = rule489 _hdIcollectedConstructorsMap _tlIcollectedConstructorsMap
         _lhsOcollectedFields :: [(NontermIdent, ConstructorIdent, FieldMap)]
         _lhsOcollectedFields = rule490 _hdIcollectedFields _tlIcollectedFields
         _lhsOcollectedInsts :: [ (NontermIdent, ConstructorIdent, [Identifier]) ]
         _lhsOcollectedInsts = rule491 _hdIcollectedInsts _tlIcollectedInsts
         _lhsOcollectedMacros :: [(NontermIdent, ConstructorIdent, MaybeMacro)]
         _lhsOcollectedMacros = rule492 _hdIcollectedMacros _tlIcollectedMacros
         _lhsOcollectedMerges :: [ (NontermIdent, ConstructorIdent, [MergeInfo])   ]
         _lhsOcollectedMerges = rule493 _hdIcollectedMerges _tlIcollectedMerges
         _lhsOcollectedNames :: Set Identifier
         _lhsOcollectedNames = rule494 _hdIcollectedNames _tlIcollectedNames
         _lhsOcollectedRules :: [ (NontermIdent, ConstructorIdent, RuleInfo)]
         _lhsOcollectedRules = rule495 _hdIcollectedRules _tlIcollectedRules
         _lhsOcollectedSetNames :: Set Identifier
         _lhsOcollectedSetNames = rule496 _hdIcollectedSetNames _tlIcollectedSetNames
         _lhsOcollectedSigs :: [ (NontermIdent, ConstructorIdent, SigInfo) ]
         _lhsOcollectedSigs = rule497 _hdIcollectedSigs _tlIcollectedSigs
         _lhsOcollectedUniques :: [ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]
         _lhsOcollectedUniques = rule498 _hdIcollectedUniques _tlIcollectedUniques
         _lhsOctxCollect :: ContextMap
         _lhsOctxCollect = rule499 _hdIctxCollect _tlIctxCollect
         _lhsOderivings :: Derivings
         _lhsOderivings = rule500 _hdIderivings _tlIderivings
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule501 _hdIerrors _tlIerrors
         _lhsOmoduleDecl :: Maybe (String,String,String)
         _lhsOmoduleDecl = rule502 _hdImoduleDecl _tlImoduleDecl
         _lhsOparamsCollect :: ParamMap
         _lhsOparamsCollect = rule503 _hdIparamsCollect _tlIparamsCollect
         _lhsOpragmas :: Options -> Options
         _lhsOpragmas = rule504 _hdIpragmas _tlIpragmas
         _lhsOquantCollect :: QuantMap
         _lhsOquantCollect = rule505 _hdIquantCollect _tlIquantCollect
         _lhsOsemPragmasCollect :: PragmaMap
         _lhsOsemPragmasCollect = rule506 _hdIsemPragmasCollect _tlIsemPragmasCollect
         _lhsOtypeSyns :: TypeSyns
         _lhsOtypeSyns = rule507 _hdItypeSyns _tlItypeSyns
         _lhsOuseMap :: Map NontermIdent (Map Identifier (String,String,String))
         _lhsOuseMap = rule508 _hdIuseMap _tlIuseMap
         _lhsOwrappers :: Set NontermIdent
         _lhsOwrappers = rule509 _hdIwrappers _tlIwrappers
         _lhsOattrDecls :: Map NontermIdent (Attributes, Attributes)
         _lhsOattrDecls = rule510 _tlIattrDecls
         _lhsOattrs :: Map NontermIdent (Attributes, Attributes)
         _lhsOattrs = rule511 _tlIattrs
         _lhsOdefSets :: Map Identifier (Set NontermIdent,Set Identifier)
         _lhsOdefSets = rule512 _tlIdefSets
         _hdOallAttrDecls = rule513 _lhsIallAttrDecls
         _hdOallAttrs = rule514 _lhsIallAttrs
         _hdOallConstructors = rule515 _lhsIallConstructors
         _hdOallFields = rule516 _lhsIallFields
         _hdOallNonterminals = rule517 _lhsIallNonterminals
         _hdOattrDecls = rule518 _lhsIattrDecls
         _hdOattrs = rule519 _lhsIattrs
         _hdOdefSets = rule520 _lhsIdefSets
         _hdOdefinedSets = rule521 _lhsIdefinedSets
         _hdOoptions = rule522 _lhsIoptions
         _tlOallAttrDecls = rule523 _lhsIallAttrDecls
         _tlOallAttrs = rule524 _lhsIallAttrs
         _tlOallConstructors = rule525 _lhsIallConstructors
         _tlOallFields = rule526 _lhsIallFields
         _tlOallNonterminals = rule527 _lhsIallNonterminals
         _tlOattrDecls = rule528 _hdIattrDecls
         _tlOattrs = rule529 _hdIattrs
         _tlOdefSets = rule530 _hdIdefSets
         _tlOdefinedSets = rule531 _lhsIdefinedSets
         _tlOoptions = rule532 _lhsIoptions
         __result_ = T_Elems_vOut19 _lhsOattrDecls _lhsOattrOrderCollect _lhsOattrs _lhsOblocks _lhsOcollectedArounds _lhsOcollectedAugments _lhsOcollectedConParams _lhsOcollectedConstraints _lhsOcollectedConstructorsMap _lhsOcollectedFields _lhsOcollectedInsts _lhsOcollectedMacros _lhsOcollectedMerges _lhsOcollectedNames _lhsOcollectedRules _lhsOcollectedSetNames _lhsOcollectedSigs _lhsOcollectedUniques _lhsOctxCollect _lhsOdefSets _lhsOderivings _lhsOerrors _lhsOmoduleDecl _lhsOparamsCollect _lhsOpragmas _lhsOquantCollect _lhsOsemPragmasCollect _lhsOtypeSyns _lhsOuseMap _lhsOwrappers
         in __result_ )
     in C_Elems_s20 v19
   {-# INLINE rule483 #-}
   rule483 = \ ((_hdIattrOrderCollect) :: AttrOrderMap) ((_tlIattrOrderCollect) :: AttrOrderMap) ->
     _hdIattrOrderCollect `orderMapUnion` _tlIattrOrderCollect
   {-# INLINE rule484 #-}
   rule484 = \ ((_hdIblocks) :: Blocks) ((_tlIblocks) :: Blocks) ->
     _hdIblocks `mapUnionWithPlusPlus` _tlIblocks
   {-# INLINE rule485 #-}
   rule485 = \ ((_hdIcollectedArounds) :: [ (NontermIdent, ConstructorIdent, [AroundInfo])  ]) ((_tlIcollectedArounds) :: [ (NontermIdent, ConstructorIdent, [AroundInfo])  ]) ->
     _hdIcollectedArounds ++ _tlIcollectedArounds
   {-# INLINE rule486 #-}
   rule486 = \ ((_hdIcollectedAugments) :: [ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]) ((_tlIcollectedAugments) :: [ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]) ->
     _hdIcollectedAugments ++ _tlIcollectedAugments
   {-# INLINE rule487 #-}
   rule487 = \ ((_hdIcollectedConParams) :: [(NontermIdent, ConstructorIdent, Set Identifier)]) ((_tlIcollectedConParams) :: [(NontermIdent, ConstructorIdent, Set Identifier)]) ->
     _hdIcollectedConParams ++ _tlIcollectedConParams
   {-# INLINE rule488 #-}
   rule488 = \ ((_hdIcollectedConstraints) :: [(NontermIdent, ConstructorIdent, [Type])]) ((_tlIcollectedConstraints) :: [(NontermIdent, ConstructorIdent, [Type])]) ->
     _hdIcollectedConstraints ++ _tlIcollectedConstraints
   {-# INLINE rule489 #-}
   rule489 = \ ((_hdIcollectedConstructorsMap) :: Map NontermIdent (Set ConstructorIdent)) ((_tlIcollectedConstructorsMap) :: Map NontermIdent (Set ConstructorIdent)) ->
     _hdIcollectedConstructorsMap `mapUnionWithSetUnion` _tlIcollectedConstructorsMap
   {-# INLINE rule490 #-}
   rule490 = \ ((_hdIcollectedFields) :: [(NontermIdent, ConstructorIdent, FieldMap)]) ((_tlIcollectedFields) :: [(NontermIdent, ConstructorIdent, FieldMap)]) ->
     _hdIcollectedFields ++ _tlIcollectedFields
   {-# INLINE rule491 #-}
   rule491 = \ ((_hdIcollectedInsts) :: [ (NontermIdent, ConstructorIdent, [Identifier]) ]) ((_tlIcollectedInsts) :: [ (NontermIdent, ConstructorIdent, [Identifier]) ]) ->
     _hdIcollectedInsts ++ _tlIcollectedInsts
   {-# INLINE rule492 #-}
   rule492 = \ ((_hdIcollectedMacros) :: [(NontermIdent, ConstructorIdent, MaybeMacro)]) ((_tlIcollectedMacros) :: [(NontermIdent, ConstructorIdent, MaybeMacro)]) ->
     _hdIcollectedMacros ++ _tlIcollectedMacros
   {-# INLINE rule493 #-}
   rule493 = \ ((_hdIcollectedMerges) :: [ (NontermIdent, ConstructorIdent, [MergeInfo])   ]) ((_tlIcollectedMerges) :: [ (NontermIdent, ConstructorIdent, [MergeInfo])   ]) ->
     _hdIcollectedMerges ++ _tlIcollectedMerges
   {-# INLINE rule494 #-}
   rule494 = \ ((_hdIcollectedNames) :: Set Identifier) ((_tlIcollectedNames) :: Set Identifier) ->
     _hdIcollectedNames `Set.union` _tlIcollectedNames
   {-# INLINE rule495 #-}
   rule495 = \ ((_hdIcollectedRules) :: [ (NontermIdent, ConstructorIdent, RuleInfo)]) ((_tlIcollectedRules) :: [ (NontermIdent, ConstructorIdent, RuleInfo)]) ->
     _hdIcollectedRules ++ _tlIcollectedRules
   {-# INLINE rule496 #-}
   rule496 = \ ((_hdIcollectedSetNames) :: Set Identifier) ((_tlIcollectedSetNames) :: Set Identifier) ->
     _hdIcollectedSetNames `Set.union` _tlIcollectedSetNames
   {-# INLINE rule497 #-}
   rule497 = \ ((_hdIcollectedSigs) :: [ (NontermIdent, ConstructorIdent, SigInfo) ]) ((_tlIcollectedSigs) :: [ (NontermIdent, ConstructorIdent, SigInfo) ]) ->
     _hdIcollectedSigs ++ _tlIcollectedSigs
   {-# INLINE rule498 #-}
   rule498 = \ ((_hdIcollectedUniques) :: [ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]) ((_tlIcollectedUniques) :: [ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]) ->
     _hdIcollectedUniques ++ _tlIcollectedUniques
   {-# INLINE rule499 #-}
   rule499 = \ ((_hdIctxCollect) :: ContextMap) ((_tlIctxCollect) :: ContextMap) ->
     _hdIctxCollect `mergeCtx` _tlIctxCollect
   {-# INLINE rule500 #-}
   rule500 = \ ((_hdIderivings) :: Derivings) ((_tlIderivings) :: Derivings) ->
     _hdIderivings `mergeDerivings` _tlIderivings
   {-# INLINE rule501 #-}
   rule501 = \ ((_hdIerrors) :: Seq Error) ((_tlIerrors) :: Seq Error) ->
     _hdIerrors Seq.>< _tlIerrors
   {-# INLINE rule502 #-}
   rule502 = \ ((_hdImoduleDecl) :: Maybe (String,String,String)) ((_tlImoduleDecl) :: Maybe (String,String,String)) ->
     _hdImoduleDecl `mplus` _tlImoduleDecl
   {-# INLINE rule503 #-}
   rule503 = \ ((_hdIparamsCollect) :: ParamMap) ((_tlIparamsCollect) :: ParamMap) ->
     _hdIparamsCollect `mergeParams` _tlIparamsCollect
   {-# INLINE rule504 #-}
   rule504 = \ ((_hdIpragmas) :: Options -> Options) ((_tlIpragmas) :: Options -> Options) ->
     _hdIpragmas . _tlIpragmas
   {-# INLINE rule505 #-}
   rule505 = \ ((_hdIquantCollect) :: QuantMap) ((_tlIquantCollect) :: QuantMap) ->
     _hdIquantCollect `mergeQuant` _tlIquantCollect
   {-# INLINE rule506 #-}
   rule506 = \ ((_hdIsemPragmasCollect) :: PragmaMap) ((_tlIsemPragmasCollect) :: PragmaMap) ->
     _hdIsemPragmasCollect `pragmaMapUnion` _tlIsemPragmasCollect
   {-# INLINE rule507 #-}
   rule507 = \ ((_hdItypeSyns) :: TypeSyns) ((_tlItypeSyns) :: TypeSyns) ->
     _hdItypeSyns ++ _tlItypeSyns
   {-# INLINE rule508 #-}
   rule508 = \ ((_hdIuseMap) :: Map NontermIdent (Map Identifier (String,String,String))) ((_tlIuseMap) :: Map NontermIdent (Map Identifier (String,String,String))) ->
     _hdIuseMap `merge` _tlIuseMap
   {-# INLINE rule509 #-}
   rule509 = \ ((_hdIwrappers) :: Set NontermIdent) ((_tlIwrappers) :: Set NontermIdent) ->
     _hdIwrappers `Set.union` _tlIwrappers
   {-# INLINE rule510 #-}
   rule510 = \ ((_tlIattrDecls) :: Map NontermIdent (Attributes, Attributes)) ->
     _tlIattrDecls
   {-# INLINE rule511 #-}
   rule511 = \ ((_tlIattrs) :: Map NontermIdent (Attributes, Attributes)) ->
     _tlIattrs
   {-# INLINE rule512 #-}
   rule512 = \ ((_tlIdefSets) :: Map Identifier (Set NontermIdent,Set Identifier)) ->
     _tlIdefSets
   {-# INLINE rule513 #-}
   rule513 = \ ((_lhsIallAttrDecls) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIallAttrDecls
   {-# INLINE rule514 #-}
   rule514 = \ ((_lhsIallAttrs) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIallAttrs
   {-# INLINE rule515 #-}
   rule515 = \ ((_lhsIallConstructors) :: Map NontermIdent (Set ConstructorIdent)) ->
     _lhsIallConstructors
   {-# INLINE rule516 #-}
   rule516 = \ ((_lhsIallFields) :: DataTypes) ->
     _lhsIallFields
   {-# INLINE rule517 #-}
   rule517 = \ ((_lhsIallNonterminals) :: Set NontermIdent) ->
     _lhsIallNonterminals
   {-# INLINE rule518 #-}
   rule518 = \ ((_lhsIattrDecls) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIattrDecls
   {-# INLINE rule519 #-}
   rule519 = \ ((_lhsIattrs) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIattrs
   {-# INLINE rule520 #-}
   rule520 = \ ((_lhsIdefSets) :: Map Identifier (Set NontermIdent,Set Identifier)) ->
     _lhsIdefSets
   {-# INLINE rule521 #-}
   rule521 = \ ((_lhsIdefinedSets) :: DefinedSets) ->
     _lhsIdefinedSets
   {-# INLINE rule522 #-}
   rule522 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule523 #-}
   rule523 = \ ((_lhsIallAttrDecls) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIallAttrDecls
   {-# INLINE rule524 #-}
   rule524 = \ ((_lhsIallAttrs) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIallAttrs
   {-# INLINE rule525 #-}
   rule525 = \ ((_lhsIallConstructors) :: Map NontermIdent (Set ConstructorIdent)) ->
     _lhsIallConstructors
   {-# INLINE rule526 #-}
   rule526 = \ ((_lhsIallFields) :: DataTypes) ->
     _lhsIallFields
   {-# INLINE rule527 #-}
   rule527 = \ ((_lhsIallNonterminals) :: Set NontermIdent) ->
     _lhsIallNonterminals
   {-# INLINE rule528 #-}
   rule528 = \ ((_hdIattrDecls) :: Map NontermIdent (Attributes, Attributes)) ->
     _hdIattrDecls
   {-# INLINE rule529 #-}
   rule529 = \ ((_hdIattrs) :: Map NontermIdent (Attributes, Attributes)) ->
     _hdIattrs
   {-# INLINE rule530 #-}
   rule530 = \ ((_hdIdefSets) :: Map Identifier (Set NontermIdent,Set Identifier)) ->
     _hdIdefSets
   {-# INLINE rule531 #-}
   rule531 = \ ((_lhsIdefinedSets) :: DefinedSets) ->
     _lhsIdefinedSets
   {-# INLINE rule532 #-}
   rule532 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Elems_Nil #-}
sem_Elems_Nil ::  T_Elems 
sem_Elems_Nil  = T_Elems (return st20) where
   {-# NOINLINE st20 #-}
   st20 = let
      v19 :: T_Elems_v19 
      v19 = \ (T_Elems_vIn19 _lhsIallAttrDecls _lhsIallAttrs _lhsIallConstructors _lhsIallFields _lhsIallNonterminals _lhsIattrDecls _lhsIattrs _lhsIdefSets _lhsIdefinedSets _lhsIoptions) -> ( let
         _lhsOattrOrderCollect :: AttrOrderMap
         _lhsOattrOrderCollect = rule533  ()
         _lhsOblocks :: Blocks
         _lhsOblocks = rule534  ()
         _lhsOcollectedArounds :: [ (NontermIdent, ConstructorIdent, [AroundInfo])  ]
         _lhsOcollectedArounds = rule535  ()
         _lhsOcollectedAugments :: [ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]
         _lhsOcollectedAugments = rule536  ()
         _lhsOcollectedConParams :: [(NontermIdent, ConstructorIdent, Set Identifier)]
         _lhsOcollectedConParams = rule537  ()
         _lhsOcollectedConstraints :: [(NontermIdent, ConstructorIdent, [Type])]
         _lhsOcollectedConstraints = rule538  ()
         _lhsOcollectedConstructorsMap :: Map NontermIdent (Set ConstructorIdent)
         _lhsOcollectedConstructorsMap = rule539  ()
         _lhsOcollectedFields :: [(NontermIdent, ConstructorIdent, FieldMap)]
         _lhsOcollectedFields = rule540  ()
         _lhsOcollectedInsts :: [ (NontermIdent, ConstructorIdent, [Identifier]) ]
         _lhsOcollectedInsts = rule541  ()
         _lhsOcollectedMacros :: [(NontermIdent, ConstructorIdent, MaybeMacro)]
         _lhsOcollectedMacros = rule542  ()
         _lhsOcollectedMerges :: [ (NontermIdent, ConstructorIdent, [MergeInfo])   ]
         _lhsOcollectedMerges = rule543  ()
         _lhsOcollectedNames :: Set Identifier
         _lhsOcollectedNames = rule544  ()
         _lhsOcollectedRules :: [ (NontermIdent, ConstructorIdent, RuleInfo)]
         _lhsOcollectedRules = rule545  ()
         _lhsOcollectedSetNames :: Set Identifier
         _lhsOcollectedSetNames = rule546  ()
         _lhsOcollectedSigs :: [ (NontermIdent, ConstructorIdent, SigInfo) ]
         _lhsOcollectedSigs = rule547  ()
         _lhsOcollectedUniques :: [ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]
         _lhsOcollectedUniques = rule548  ()
         _lhsOctxCollect :: ContextMap
         _lhsOctxCollect = rule549  ()
         _lhsOderivings :: Derivings
         _lhsOderivings = rule550  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule551  ()
         _lhsOmoduleDecl :: Maybe (String,String,String)
         _lhsOmoduleDecl = rule552  ()
         _lhsOparamsCollect :: ParamMap
         _lhsOparamsCollect = rule553  ()
         _lhsOpragmas :: Options -> Options
         _lhsOpragmas = rule554  ()
         _lhsOquantCollect :: QuantMap
         _lhsOquantCollect = rule555  ()
         _lhsOsemPragmasCollect :: PragmaMap
         _lhsOsemPragmasCollect = rule556  ()
         _lhsOtypeSyns :: TypeSyns
         _lhsOtypeSyns = rule557  ()
         _lhsOuseMap :: Map NontermIdent (Map Identifier (String,String,String))
         _lhsOuseMap = rule558  ()
         _lhsOwrappers :: Set NontermIdent
         _lhsOwrappers = rule559  ()
         _lhsOattrDecls :: Map NontermIdent (Attributes, Attributes)
         _lhsOattrDecls = rule560 _lhsIattrDecls
         _lhsOattrs :: Map NontermIdent (Attributes, Attributes)
         _lhsOattrs = rule561 _lhsIattrs
         _lhsOdefSets :: Map Identifier (Set NontermIdent,Set Identifier)
         _lhsOdefSets = rule562 _lhsIdefSets
         __result_ = T_Elems_vOut19 _lhsOattrDecls _lhsOattrOrderCollect _lhsOattrs _lhsOblocks _lhsOcollectedArounds _lhsOcollectedAugments _lhsOcollectedConParams _lhsOcollectedConstraints _lhsOcollectedConstructorsMap _lhsOcollectedFields _lhsOcollectedInsts _lhsOcollectedMacros _lhsOcollectedMerges _lhsOcollectedNames _lhsOcollectedRules _lhsOcollectedSetNames _lhsOcollectedSigs _lhsOcollectedUniques _lhsOctxCollect _lhsOdefSets _lhsOderivings _lhsOerrors _lhsOmoduleDecl _lhsOparamsCollect _lhsOpragmas _lhsOquantCollect _lhsOsemPragmasCollect _lhsOtypeSyns _lhsOuseMap _lhsOwrappers
         in __result_ )
     in C_Elems_s20 v19
   {-# INLINE rule533 #-}
   rule533 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule534 #-}
   rule534 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule535 #-}
   rule535 = \  (_ :: ()) ->
     []
   {-# INLINE rule536 #-}
   rule536 = \  (_ :: ()) ->
     []
   {-# INLINE rule537 #-}
   rule537 = \  (_ :: ()) ->
     []
   {-# INLINE rule538 #-}
   rule538 = \  (_ :: ()) ->
     []
   {-# INLINE rule539 #-}
   rule539 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule540 #-}
   rule540 = \  (_ :: ()) ->
     []
   {-# INLINE rule541 #-}
   rule541 = \  (_ :: ()) ->
     []
   {-# INLINE rule542 #-}
   rule542 = \  (_ :: ()) ->
     []
   {-# INLINE rule543 #-}
   rule543 = \  (_ :: ()) ->
     []
   {-# INLINE rule544 #-}
   rule544 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule545 #-}
   rule545 = \  (_ :: ()) ->
     []
   {-# INLINE rule546 #-}
   rule546 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule547 #-}
   rule547 = \  (_ :: ()) ->
     []
   {-# INLINE rule548 #-}
   rule548 = \  (_ :: ()) ->
     []
   {-# INLINE rule549 #-}
   rule549 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule550 #-}
   rule550 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule551 #-}
   rule551 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule552 #-}
   rule552 = \  (_ :: ()) ->
     mzero
   {-# INLINE rule553 #-}
   rule553 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule554 #-}
   rule554 = \  (_ :: ()) ->
     id
   {-# INLINE rule555 #-}
   rule555 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule556 #-}
   rule556 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule557 #-}
   rule557 = \  (_ :: ()) ->
     []
   {-# INLINE rule558 #-}
   rule558 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule559 #-}
   rule559 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule560 #-}
   rule560 = \ ((_lhsIattrDecls) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIattrDecls
   {-# INLINE rule561 #-}
   rule561 = \ ((_lhsIattrs) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIattrs
   {-# INLINE rule562 #-}
   rule562 = \ ((_lhsIdefSets) :: Map Identifier (Set NontermIdent,Set Identifier)) ->
     _lhsIdefSets

-- Field -------------------------------------------------------
-- wrapper
data Inh_Field  = Inh_Field { allNonterminals_Inh_Field :: (Set NontermIdent) }
data Syn_Field  = Syn_Field { collectedConstraints_Syn_Field :: ([Type]), collectedFields_Syn_Field :: ([(Identifier, Type)]) }
{-# INLINABLE wrap_Field #-}
wrap_Field :: T_Field  -> Inh_Field  -> (Syn_Field )
wrap_Field (T_Field act) (Inh_Field _lhsIallNonterminals) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Field_vIn22 _lhsIallNonterminals
        (T_Field_vOut22 _lhsOcollectedConstraints _lhsOcollectedFields) <- return (inv_Field_s23 sem arg)
        return (Syn_Field _lhsOcollectedConstraints _lhsOcollectedFields)
   )

-- cata
{-# NOINLINE sem_Field #-}
sem_Field :: Field  -> T_Field 
sem_Field ( FChild name_ tp_ ) = sem_Field_FChild name_ tp_
sem_Field ( FCtx tps_ ) = sem_Field_FCtx tps_

-- semantic domain
newtype T_Field  = T_Field {
                           attach_T_Field :: Identity (T_Field_s23 )
                           }
newtype T_Field_s23  = C_Field_s23 {
                                   inv_Field_s23 :: (T_Field_v22 )
                                   }
data T_Field_s24  = C_Field_s24
type T_Field_v22  = (T_Field_vIn22 ) -> (T_Field_vOut22 )
data T_Field_vIn22  = T_Field_vIn22 (Set NontermIdent)
data T_Field_vOut22  = T_Field_vOut22 ([Type]) ([(Identifier, Type)])
{-# NOINLINE sem_Field_FChild #-}
sem_Field_FChild :: (Identifier) -> (Type) -> T_Field 
sem_Field_FChild arg_name_ arg_tp_ = T_Field (return st23) where
   {-# NOINLINE st23 #-}
   st23 = let
      v22 :: T_Field_v22 
      v22 = \ (T_Field_vIn22 _lhsIallNonterminals) -> ( let
         _lhsOcollectedFields :: [(Identifier, Type)]
         _lhsOcollectedFields = rule563 _lhsIallNonterminals arg_name_ arg_tp_
         _lhsOcollectedConstraints :: [Type]
         _lhsOcollectedConstraints = rule564  ()
         __result_ = T_Field_vOut22 _lhsOcollectedConstraints _lhsOcollectedFields
         in __result_ )
     in C_Field_s23 v22
   {-# INLINE rule563 #-}
   {-# LINE 579 "./src-ag/Transform.ag" #-}
   rule563 = \ ((_lhsIallNonterminals) :: Set NontermIdent) name_ tp_ ->
                          {-# LINE 579 "./src-ag/Transform.ag" #-}
                          [(name_, makeType _lhsIallNonterminals tp_)]
                          {-# LINE 4268 "dist/build/Transform.hs"#-}
   {-# INLINE rule564 #-}
   rule564 = \  (_ :: ()) ->
     []
{-# NOINLINE sem_Field_FCtx #-}
sem_Field_FCtx :: ([Type]) -> T_Field 
sem_Field_FCtx arg_tps_ = T_Field (return st23) where
   {-# NOINLINE st23 #-}
   st23 = let
      v22 :: T_Field_v22 
      v22 = \ (T_Field_vIn22 _lhsIallNonterminals) -> ( let
         _lhsOcollectedConstraints :: [Type]
         _lhsOcollectedConstraints = rule565 arg_tps_
         _lhsOcollectedFields :: [(Identifier, Type)]
         _lhsOcollectedFields = rule566  ()
         __result_ = T_Field_vOut22 _lhsOcollectedConstraints _lhsOcollectedFields
         in __result_ )
     in C_Field_s23 v22
   {-# INLINE rule565 #-}
   {-# LINE 588 "./src-ag/Transform.ag" #-}
   rule565 = \ tps_ ->
                               {-# LINE 588 "./src-ag/Transform.ag" #-}
                               tps_
                               {-# LINE 4291 "dist/build/Transform.hs"#-}
   {-# INLINE rule566 #-}
   rule566 = \  (_ :: ()) ->
     []

-- Fields ------------------------------------------------------
-- wrapper
data Inh_Fields  = Inh_Fields { allNonterminals_Inh_Fields :: (Set NontermIdent) }
data Syn_Fields  = Syn_Fields { collectedConstraints_Syn_Fields :: ([Type]), collectedFields_Syn_Fields :: ([(Identifier, Type)]) }
{-# INLINABLE wrap_Fields #-}
wrap_Fields :: T_Fields  -> Inh_Fields  -> (Syn_Fields )
wrap_Fields (T_Fields act) (Inh_Fields _lhsIallNonterminals) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Fields_vIn25 _lhsIallNonterminals
        (T_Fields_vOut25 _lhsOcollectedConstraints _lhsOcollectedFields) <- return (inv_Fields_s26 sem arg)
        return (Syn_Fields _lhsOcollectedConstraints _lhsOcollectedFields)
   )

-- cata
{-# NOINLINE sem_Fields #-}
sem_Fields :: Fields  -> T_Fields 
sem_Fields list = Prelude.foldr sem_Fields_Cons sem_Fields_Nil (Prelude.map sem_Field list)

-- semantic domain
newtype T_Fields  = T_Fields {
                             attach_T_Fields :: Identity (T_Fields_s26 )
                             }
newtype T_Fields_s26  = C_Fields_s26 {
                                     inv_Fields_s26 :: (T_Fields_v25 )
                                     }
data T_Fields_s27  = C_Fields_s27
type T_Fields_v25  = (T_Fields_vIn25 ) -> (T_Fields_vOut25 )
data T_Fields_vIn25  = T_Fields_vIn25 (Set NontermIdent)
data T_Fields_vOut25  = T_Fields_vOut25 ([Type]) ([(Identifier, Type)])
{-# NOINLINE sem_Fields_Cons #-}
sem_Fields_Cons :: T_Field  -> T_Fields  -> T_Fields 
sem_Fields_Cons arg_hd_ arg_tl_ = T_Fields (return st26) where
   {-# NOINLINE st26 #-}
   st26 = let
      v25 :: T_Fields_v25 
      v25 = \ (T_Fields_vIn25 _lhsIallNonterminals) -> ( let
         _hdX23 = Control.Monad.Identity.runIdentity (attach_T_Field (arg_hd_))
         _tlX26 = Control.Monad.Identity.runIdentity (attach_T_Fields (arg_tl_))
         (T_Field_vOut22 _hdIcollectedConstraints _hdIcollectedFields) = inv_Field_s23 _hdX23 (T_Field_vIn22 _hdOallNonterminals)
         (T_Fields_vOut25 _tlIcollectedConstraints _tlIcollectedFields) = inv_Fields_s26 _tlX26 (T_Fields_vIn25 _tlOallNonterminals)
         _lhsOcollectedConstraints :: [Type]
         _lhsOcollectedConstraints = rule567 _hdIcollectedConstraints _tlIcollectedConstraints
         _lhsOcollectedFields :: [(Identifier, Type)]
         _lhsOcollectedFields = rule568 _hdIcollectedFields _tlIcollectedFields
         _hdOallNonterminals = rule569 _lhsIallNonterminals
         _tlOallNonterminals = rule570 _lhsIallNonterminals
         __result_ = T_Fields_vOut25 _lhsOcollectedConstraints _lhsOcollectedFields
         in __result_ )
     in C_Fields_s26 v25
   {-# INLINE rule567 #-}
   rule567 = \ ((_hdIcollectedConstraints) :: [Type]) ((_tlIcollectedConstraints) :: [Type]) ->
     _hdIcollectedConstraints ++ _tlIcollectedConstraints
   {-# INLINE rule568 #-}
   rule568 = \ ((_hdIcollectedFields) :: [(Identifier, Type)]) ((_tlIcollectedFields) :: [(Identifier, Type)]) ->
     _hdIcollectedFields ++ _tlIcollectedFields
   {-# INLINE rule569 #-}
   rule569 = \ ((_lhsIallNonterminals) :: Set NontermIdent) ->
     _lhsIallNonterminals
   {-# INLINE rule570 #-}
   rule570 = \ ((_lhsIallNonterminals) :: Set NontermIdent) ->
     _lhsIallNonterminals
{-# NOINLINE sem_Fields_Nil #-}
sem_Fields_Nil ::  T_Fields 
sem_Fields_Nil  = T_Fields (return st26) where
   {-# NOINLINE st26 #-}
   st26 = let
      v25 :: T_Fields_v25 
      v25 = \ (T_Fields_vIn25 _lhsIallNonterminals) -> ( let
         _lhsOcollectedConstraints :: [Type]
         _lhsOcollectedConstraints = rule571  ()
         _lhsOcollectedFields :: [(Identifier, Type)]
         _lhsOcollectedFields = rule572  ()
         __result_ = T_Fields_vOut25 _lhsOcollectedConstraints _lhsOcollectedFields
         in __result_ )
     in C_Fields_s26 v25
   {-# INLINE rule571 #-}
   rule571 = \  (_ :: ()) ->
     []
   {-# INLINE rule572 #-}
   rule572 = \  (_ :: ()) ->
     []

-- NontSet -----------------------------------------------------
-- wrapper
data Inh_NontSet  = Inh_NontSet { allFields_Inh_NontSet :: (DataTypes), allNonterminals_Inh_NontSet :: (Set NontermIdent), definedSets_Inh_NontSet :: (DefinedSets) }
data Syn_NontSet  = Syn_NontSet { collectedNames_Syn_NontSet :: (Set Identifier), errors_Syn_NontSet :: (Seq Error), nontSet_Syn_NontSet :: (Set NontermIdent) }
{-# INLINABLE wrap_NontSet #-}
wrap_NontSet :: T_NontSet  -> Inh_NontSet  -> (Syn_NontSet )
wrap_NontSet (T_NontSet act) (Inh_NontSet _lhsIallFields _lhsIallNonterminals _lhsIdefinedSets) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_NontSet_vIn28 _lhsIallFields _lhsIallNonterminals _lhsIdefinedSets
        (T_NontSet_vOut28 _lhsOcollectedNames _lhsOerrors _lhsOnontSet) <- return (inv_NontSet_s29 sem arg)
        return (Syn_NontSet _lhsOcollectedNames _lhsOerrors _lhsOnontSet)
   )

-- cata
{-# NOINLINE sem_NontSet #-}
sem_NontSet :: NontSet  -> T_NontSet 
sem_NontSet ( NamedSet name_ ) = sem_NontSet_NamedSet name_
sem_NontSet ( All  ) = sem_NontSet_All 
sem_NontSet ( Union set1_ set2_ ) = sem_NontSet_Union ( sem_NontSet set1_ ) ( sem_NontSet set2_ )
sem_NontSet ( Intersect set1_ set2_ ) = sem_NontSet_Intersect ( sem_NontSet set1_ ) ( sem_NontSet set2_ )
sem_NontSet ( Difference set1_ set2_ ) = sem_NontSet_Difference ( sem_NontSet set1_ ) ( sem_NontSet set2_ )
sem_NontSet ( Path from_ to_ ) = sem_NontSet_Path from_ to_

-- semantic domain
newtype T_NontSet  = T_NontSet {
                               attach_T_NontSet :: Identity (T_NontSet_s29 )
                               }
newtype T_NontSet_s29  = C_NontSet_s29 {
                                       inv_NontSet_s29 :: (T_NontSet_v28 )
                                       }
data T_NontSet_s30  = C_NontSet_s30
type T_NontSet_v28  = (T_NontSet_vIn28 ) -> (T_NontSet_vOut28 )
data T_NontSet_vIn28  = T_NontSet_vIn28 (DataTypes) (Set NontermIdent) (DefinedSets)
data T_NontSet_vOut28  = T_NontSet_vOut28 (Set Identifier) (Seq Error) (Set NontermIdent)
{-# NOINLINE sem_NontSet_NamedSet #-}
sem_NontSet_NamedSet :: (NontermIdent) -> T_NontSet 
sem_NontSet_NamedSet arg_name_ = T_NontSet (return st29) where
   {-# NOINLINE st29 #-}
   st29 = let
      v28 :: T_NontSet_v28 
      v28 = \ (T_NontSet_vIn28 _lhsIallFields _lhsIallNonterminals _lhsIdefinedSets) -> ( let
         _lhsOcollectedNames :: Set Identifier
         _lhsOcollectedNames = rule573 arg_name_
         (_nontSet,_errors) = rule574 _lhsIdefinedSets arg_name_
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule575 _errors
         _lhsOnontSet :: Set NontermIdent
         _lhsOnontSet = rule576 _nontSet
         __result_ = T_NontSet_vOut28 _lhsOcollectedNames _lhsOerrors _lhsOnontSet
         in __result_ )
     in C_NontSet_s29 v28
   {-# INLINE rule573 #-}
   {-# LINE 603 "./src-ag/Transform.ag" #-}
   rule573 = \ name_ ->
                                    {-# LINE 603 "./src-ag/Transform.ag" #-}
                                    Set.singleton name_
                                    {-# LINE 4436 "dist/build/Transform.hs"#-}
   {-# INLINE rule574 #-}
   {-# LINE 733 "./src-ag/Transform.ag" #-}
   rule574 = \ ((_lhsIdefinedSets) :: DefinedSets) name_ ->
                                        {-# LINE 733 "./src-ag/Transform.ag" #-}
                                        case Map.lookup name_ _lhsIdefinedSets of
                                                     Nothing  -> (Set.empty, Seq.singleton (UndefNont name_))
                                                     Just set -> (set, Seq.empty)
                                        {-# LINE 4444 "dist/build/Transform.hs"#-}
   {-# INLINE rule575 #-}
   rule575 = \ _errors ->
     _errors
   {-# INLINE rule576 #-}
   rule576 = \ _nontSet ->
     _nontSet
{-# NOINLINE sem_NontSet_All #-}
sem_NontSet_All ::  T_NontSet 
sem_NontSet_All  = T_NontSet (return st29) where
   {-# NOINLINE st29 #-}
   st29 = let
      v28 :: T_NontSet_v28 
      v28 = \ (T_NontSet_vIn28 _lhsIallFields _lhsIallNonterminals _lhsIdefinedSets) -> ( let
         _lhsOnontSet :: Set NontermIdent
         _lhsOnontSet = rule577 _lhsIallNonterminals
         _lhsOcollectedNames :: Set Identifier
         _lhsOcollectedNames = rule578  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule579  ()
         __result_ = T_NontSet_vOut28 _lhsOcollectedNames _lhsOerrors _lhsOnontSet
         in __result_ )
     in C_NontSet_s29 v28
   {-# INLINE rule577 #-}
   {-# LINE 732 "./src-ag/Transform.ag" #-}
   rule577 = \ ((_lhsIallNonterminals) :: Set NontermIdent) ->
                               {-# LINE 732 "./src-ag/Transform.ag" #-}
                               _lhsIallNonterminals
                               {-# LINE 4472 "dist/build/Transform.hs"#-}
   {-# INLINE rule578 #-}
   rule578 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule579 #-}
   rule579 = \  (_ :: ()) ->
     Seq.empty
{-# NOINLINE sem_NontSet_Union #-}
sem_NontSet_Union :: T_NontSet  -> T_NontSet  -> T_NontSet 
sem_NontSet_Union arg_set1_ arg_set2_ = T_NontSet (return st29) where
   {-# NOINLINE st29 #-}
   st29 = let
      v28 :: T_NontSet_v28 
      v28 = \ (T_NontSet_vIn28 _lhsIallFields _lhsIallNonterminals _lhsIdefinedSets) -> ( let
         _set1X29 = Control.Monad.Identity.runIdentity (attach_T_NontSet (arg_set1_))
         _set2X29 = Control.Monad.Identity.runIdentity (attach_T_NontSet (arg_set2_))
         (T_NontSet_vOut28 _set1IcollectedNames _set1Ierrors _set1InontSet) = inv_NontSet_s29 _set1X29 (T_NontSet_vIn28 _set1OallFields _set1OallNonterminals _set1OdefinedSets)
         (T_NontSet_vOut28 _set2IcollectedNames _set2Ierrors _set2InontSet) = inv_NontSet_s29 _set2X29 (T_NontSet_vIn28 _set2OallFields _set2OallNonterminals _set2OdefinedSets)
         _lhsOnontSet :: Set NontermIdent
         _lhsOnontSet = rule580 _set1InontSet _set2InontSet
         _lhsOcollectedNames :: Set Identifier
         _lhsOcollectedNames = rule581 _set1IcollectedNames _set2IcollectedNames
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule582 _set1Ierrors _set2Ierrors
         _set1OallFields = rule583 _lhsIallFields
         _set1OallNonterminals = rule584 _lhsIallNonterminals
         _set1OdefinedSets = rule585 _lhsIdefinedSets
         _set2OallFields = rule586 _lhsIallFields
         _set2OallNonterminals = rule587 _lhsIallNonterminals
         _set2OdefinedSets = rule588 _lhsIdefinedSets
         __result_ = T_NontSet_vOut28 _lhsOcollectedNames _lhsOerrors _lhsOnontSet
         in __result_ )
     in C_NontSet_s29 v28
   {-# INLINE rule580 #-}
   {-# LINE 736 "./src-ag/Transform.ag" #-}
   rule580 = \ ((_set1InontSet) :: Set NontermIdent) ((_set2InontSet) :: Set NontermIdent) ->
                               {-# LINE 736 "./src-ag/Transform.ag" #-}
                               Set.union         _set1InontSet _set2InontSet
                               {-# LINE 4510 "dist/build/Transform.hs"#-}
   {-# INLINE rule581 #-}
   rule581 = \ ((_set1IcollectedNames) :: Set Identifier) ((_set2IcollectedNames) :: Set Identifier) ->
     _set1IcollectedNames `Set.union` _set2IcollectedNames
   {-# INLINE rule582 #-}
   rule582 = \ ((_set1Ierrors) :: Seq Error) ((_set2Ierrors) :: Seq Error) ->
     _set1Ierrors Seq.>< _set2Ierrors
   {-# INLINE rule583 #-}
   rule583 = \ ((_lhsIallFields) :: DataTypes) ->
     _lhsIallFields
   {-# INLINE rule584 #-}
   rule584 = \ ((_lhsIallNonterminals) :: Set NontermIdent) ->
     _lhsIallNonterminals
   {-# INLINE rule585 #-}
   rule585 = \ ((_lhsIdefinedSets) :: DefinedSets) ->
     _lhsIdefinedSets
   {-# INLINE rule586 #-}
   rule586 = \ ((_lhsIallFields) :: DataTypes) ->
     _lhsIallFields
   {-# INLINE rule587 #-}
   rule587 = \ ((_lhsIallNonterminals) :: Set NontermIdent) ->
     _lhsIallNonterminals
   {-# INLINE rule588 #-}
   rule588 = \ ((_lhsIdefinedSets) :: DefinedSets) ->
     _lhsIdefinedSets
{-# NOINLINE sem_NontSet_Intersect #-}
sem_NontSet_Intersect :: T_NontSet  -> T_NontSet  -> T_NontSet 
sem_NontSet_Intersect arg_set1_ arg_set2_ = T_NontSet (return st29) where
   {-# NOINLINE st29 #-}
   st29 = let
      v28 :: T_NontSet_v28 
      v28 = \ (T_NontSet_vIn28 _lhsIallFields _lhsIallNonterminals _lhsIdefinedSets) -> ( let
         _set1X29 = Control.Monad.Identity.runIdentity (attach_T_NontSet (arg_set1_))
         _set2X29 = Control.Monad.Identity.runIdentity (attach_T_NontSet (arg_set2_))
         (T_NontSet_vOut28 _set1IcollectedNames _set1Ierrors _set1InontSet) = inv_NontSet_s29 _set1X29 (T_NontSet_vIn28 _set1OallFields _set1OallNonterminals _set1OdefinedSets)
         (T_NontSet_vOut28 _set2IcollectedNames _set2Ierrors _set2InontSet) = inv_NontSet_s29 _set2X29 (T_NontSet_vIn28 _set2OallFields _set2OallNonterminals _set2OdefinedSets)
         _lhsOnontSet :: Set NontermIdent
         _lhsOnontSet = rule589 _set1InontSet _set2InontSet
         _lhsOcollectedNames :: Set Identifier
         _lhsOcollectedNames = rule590 _set1IcollectedNames _set2IcollectedNames
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule591 _set1Ierrors _set2Ierrors
         _set1OallFields = rule592 _lhsIallFields
         _set1OallNonterminals = rule593 _lhsIallNonterminals
         _set1OdefinedSets = rule594 _lhsIdefinedSets
         _set2OallFields = rule595 _lhsIallFields
         _set2OallNonterminals = rule596 _lhsIallNonterminals
         _set2OdefinedSets = rule597 _lhsIdefinedSets
         __result_ = T_NontSet_vOut28 _lhsOcollectedNames _lhsOerrors _lhsOnontSet
         in __result_ )
     in C_NontSet_s29 v28
   {-# INLINE rule589 #-}
   {-# LINE 737 "./src-ag/Transform.ag" #-}
   rule589 = \ ((_set1InontSet) :: Set NontermIdent) ((_set2InontSet) :: Set NontermIdent) ->
                               {-# LINE 737 "./src-ag/Transform.ag" #-}
                               Set.intersection  _set1InontSet _set2InontSet
                               {-# LINE 4566 "dist/build/Transform.hs"#-}
   {-# INLINE rule590 #-}
   rule590 = \ ((_set1IcollectedNames) :: Set Identifier) ((_set2IcollectedNames) :: Set Identifier) ->
     _set1IcollectedNames `Set.union` _set2IcollectedNames
   {-# INLINE rule591 #-}
   rule591 = \ ((_set1Ierrors) :: Seq Error) ((_set2Ierrors) :: Seq Error) ->
     _set1Ierrors Seq.>< _set2Ierrors
   {-# INLINE rule592 #-}
   rule592 = \ ((_lhsIallFields) :: DataTypes) ->
     _lhsIallFields
   {-# INLINE rule593 #-}
   rule593 = \ ((_lhsIallNonterminals) :: Set NontermIdent) ->
     _lhsIallNonterminals
   {-# INLINE rule594 #-}
   rule594 = \ ((_lhsIdefinedSets) :: DefinedSets) ->
     _lhsIdefinedSets
   {-# INLINE rule595 #-}
   rule595 = \ ((_lhsIallFields) :: DataTypes) ->
     _lhsIallFields
   {-# INLINE rule596 #-}
   rule596 = \ ((_lhsIallNonterminals) :: Set NontermIdent) ->
     _lhsIallNonterminals
   {-# INLINE rule597 #-}
   rule597 = \ ((_lhsIdefinedSets) :: DefinedSets) ->
     _lhsIdefinedSets
{-# NOINLINE sem_NontSet_Difference #-}
sem_NontSet_Difference :: T_NontSet  -> T_NontSet  -> T_NontSet 
sem_NontSet_Difference arg_set1_ arg_set2_ = T_NontSet (return st29) where
   {-# NOINLINE st29 #-}
   st29 = let
      v28 :: T_NontSet_v28 
      v28 = \ (T_NontSet_vIn28 _lhsIallFields _lhsIallNonterminals _lhsIdefinedSets) -> ( let
         _set1X29 = Control.Monad.Identity.runIdentity (attach_T_NontSet (arg_set1_))
         _set2X29 = Control.Monad.Identity.runIdentity (attach_T_NontSet (arg_set2_))
         (T_NontSet_vOut28 _set1IcollectedNames _set1Ierrors _set1InontSet) = inv_NontSet_s29 _set1X29 (T_NontSet_vIn28 _set1OallFields _set1OallNonterminals _set1OdefinedSets)
         (T_NontSet_vOut28 _set2IcollectedNames _set2Ierrors _set2InontSet) = inv_NontSet_s29 _set2X29 (T_NontSet_vIn28 _set2OallFields _set2OallNonterminals _set2OdefinedSets)
         _lhsOnontSet :: Set NontermIdent
         _lhsOnontSet = rule598 _set1InontSet _set2InontSet
         _lhsOcollectedNames :: Set Identifier
         _lhsOcollectedNames = rule599 _set1IcollectedNames _set2IcollectedNames
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule600 _set1Ierrors _set2Ierrors
         _set1OallFields = rule601 _lhsIallFields
         _set1OallNonterminals = rule602 _lhsIallNonterminals
         _set1OdefinedSets = rule603 _lhsIdefinedSets
         _set2OallFields = rule604 _lhsIallFields
         _set2OallNonterminals = rule605 _lhsIallNonterminals
         _set2OdefinedSets = rule606 _lhsIdefinedSets
         __result_ = T_NontSet_vOut28 _lhsOcollectedNames _lhsOerrors _lhsOnontSet
         in __result_ )
     in C_NontSet_s29 v28
   {-# INLINE rule598 #-}
   {-# LINE 738 "./src-ag/Transform.ag" #-}
   rule598 = \ ((_set1InontSet) :: Set NontermIdent) ((_set2InontSet) :: Set NontermIdent) ->
                               {-# LINE 738 "./src-ag/Transform.ag" #-}
                               Set.difference    _set1InontSet _set2InontSet
                               {-# LINE 4622 "dist/build/Transform.hs"#-}
   {-# INLINE rule599 #-}
   rule599 = \ ((_set1IcollectedNames) :: Set Identifier) ((_set2IcollectedNames) :: Set Identifier) ->
     _set1IcollectedNames `Set.union` _set2IcollectedNames
   {-# INLINE rule600 #-}
   rule600 = \ ((_set1Ierrors) :: Seq Error) ((_set2Ierrors) :: Seq Error) ->
     _set1Ierrors Seq.>< _set2Ierrors
   {-# INLINE rule601 #-}
   rule601 = \ ((_lhsIallFields) :: DataTypes) ->
     _lhsIallFields
   {-# INLINE rule602 #-}
   rule602 = \ ((_lhsIallNonterminals) :: Set NontermIdent) ->
     _lhsIallNonterminals
   {-# INLINE rule603 #-}
   rule603 = \ ((_lhsIdefinedSets) :: DefinedSets) ->
     _lhsIdefinedSets
   {-# INLINE rule604 #-}
   rule604 = \ ((_lhsIallFields) :: DataTypes) ->
     _lhsIallFields
   {-# INLINE rule605 #-}
   rule605 = \ ((_lhsIallNonterminals) :: Set NontermIdent) ->
     _lhsIallNonterminals
   {-# INLINE rule606 #-}
   rule606 = \ ((_lhsIdefinedSets) :: DefinedSets) ->
     _lhsIdefinedSets
{-# NOINLINE sem_NontSet_Path #-}
sem_NontSet_Path :: (NontermIdent) -> (NontermIdent) -> T_NontSet 
sem_NontSet_Path arg_from_ arg_to_ = T_NontSet (return st29) where
   {-# NOINLINE st29 #-}
   st29 = let
      v28 :: T_NontSet_v28 
      v28 = \ (T_NontSet_vIn28 _lhsIallFields _lhsIallNonterminals _lhsIdefinedSets) -> ( let
         _lhsOnontSet :: Set NontermIdent
         _lhsOnontSet = rule607 _lhsIallFields arg_from_ arg_to_
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule608 _lhsIallNonterminals arg_from_ arg_to_
         _lhsOcollectedNames :: Set Identifier
         _lhsOcollectedNames = rule609  ()
         __result_ = T_NontSet_vOut28 _lhsOcollectedNames _lhsOerrors _lhsOnontSet
         in __result_ )
     in C_NontSet_s29 v28
   {-# INLINE rule607 #-}
   {-# LINE 739 "./src-ag/Transform.ag" #-}
   rule607 = \ ((_lhsIallFields) :: DataTypes) from_ to_ ->
                               {-# LINE 739 "./src-ag/Transform.ag" #-}
                               let table = flattenDatas _lhsIallFields
                               in path table from_ to_
                               {-# LINE 4669 "dist/build/Transform.hs"#-}
   {-# INLINE rule608 #-}
   {-# LINE 741 "./src-ag/Transform.ag" #-}
   rule608 = \ ((_lhsIallNonterminals) :: Set NontermIdent) from_ to_ ->
                              {-# LINE 741 "./src-ag/Transform.ag" #-}
                              let check name | Set.member name _lhsIallNonterminals
                                                         = Seq.empty
                                             | otherwise = Seq.singleton (UndefNont name)
                              in check from_ >< check to_
                              {-# LINE 4678 "dist/build/Transform.hs"#-}
   {-# INLINE rule609 #-}
   rule609 = \  (_ :: ()) ->
     Set.empty

-- Pattern -----------------------------------------------------
-- wrapper
data Inh_Pattern  = Inh_Pattern {  }
data Syn_Pattern  = Syn_Pattern { copy_Syn_Pattern :: (Pattern), definedAttrs_Syn_Pattern :: ([AttrName]), definedInsts_Syn_Pattern :: ([Identifier]), patunder_Syn_Pattern :: ([AttrName]->Pattern), stpos_Syn_Pattern :: (Pos) }
{-# INLINABLE wrap_Pattern #-}
wrap_Pattern :: T_Pattern  -> Inh_Pattern  -> (Syn_Pattern )
wrap_Pattern (T_Pattern act) (Inh_Pattern ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Pattern_vIn31 
        (T_Pattern_vOut31 _lhsOcopy _lhsOdefinedAttrs _lhsOdefinedInsts _lhsOpatunder _lhsOstpos) <- return (inv_Pattern_s32 sem arg)
        return (Syn_Pattern _lhsOcopy _lhsOdefinedAttrs _lhsOdefinedInsts _lhsOpatunder _lhsOstpos)
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
                               attach_T_Pattern :: Identity (T_Pattern_s32 )
                               }
newtype T_Pattern_s32  = C_Pattern_s32 {
                                       inv_Pattern_s32 :: (T_Pattern_v31 )
                                       }
data T_Pattern_s33  = C_Pattern_s33
type T_Pattern_v31  = (T_Pattern_vIn31 ) -> (T_Pattern_vOut31 )
data T_Pattern_vIn31  = T_Pattern_vIn31 
data T_Pattern_vOut31  = T_Pattern_vOut31 (Pattern) ([AttrName]) ([Identifier]) ([AttrName]->Pattern) (Pos)
{-# NOINLINE sem_Pattern_Constr #-}
sem_Pattern_Constr :: (ConstructorIdent) -> T_Patterns  -> T_Pattern 
sem_Pattern_Constr arg_name_ arg_pats_ = T_Pattern (return st32) where
   {-# NOINLINE st32 #-}
   st32 = let
      v31 :: T_Pattern_v31 
      v31 = \ (T_Pattern_vIn31 ) -> ( let
         _patsX35 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_pats_))
         (T_Patterns_vOut34 _patsIcopy _patsIdefinedAttrs _patsIdefinedInsts _patsIpatunder) = inv_Patterns_s35 _patsX35 (T_Patterns_vIn34 )
         _lhsOpatunder :: [AttrName]->Pattern
         _lhsOpatunder = rule610 _patsIpatunder arg_name_
         _lhsOstpos :: Pos
         _lhsOstpos = rule611 arg_name_
         _lhsOdefinedAttrs :: [AttrName]
         _lhsOdefinedAttrs = rule612 _patsIdefinedAttrs
         _lhsOdefinedInsts :: [Identifier]
         _lhsOdefinedInsts = rule613 _patsIdefinedInsts
         _copy = rule614 _patsIcopy arg_name_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule615 _copy
         __result_ = T_Pattern_vOut31 _lhsOcopy _lhsOdefinedAttrs _lhsOdefinedInsts _lhsOpatunder _lhsOstpos
         in __result_ )
     in C_Pattern_s32 v31
   {-# INLINE rule610 #-}
   {-# LINE 1189 "./src-ag/Transform.ag" #-}
   rule610 = \ ((_patsIpatunder) :: [AttrName]->Patterns) name_ ->
                               {-# LINE 1189 "./src-ag/Transform.ag" #-}
                               \us -> Constr name_ (_patsIpatunder us)
                               {-# LINE 4745 "dist/build/Transform.hs"#-}
   {-# INLINE rule611 #-}
   {-# LINE 1200 "./src-ag/Transform.ag" #-}
   rule611 = \ name_ ->
                             {-# LINE 1200 "./src-ag/Transform.ag" #-}
                             getPos name_
                             {-# LINE 4751 "dist/build/Transform.hs"#-}
   {-# INLINE rule612 #-}
   rule612 = \ ((_patsIdefinedAttrs) :: [AttrName]) ->
     _patsIdefinedAttrs
   {-# INLINE rule613 #-}
   rule613 = \ ((_patsIdefinedInsts) :: [Identifier]) ->
     _patsIdefinedInsts
   {-# INLINE rule614 #-}
   rule614 = \ ((_patsIcopy) :: Patterns) name_ ->
     Constr name_ _patsIcopy
   {-# INLINE rule615 #-}
   rule615 = \ _copy ->
     _copy
{-# NOINLINE sem_Pattern_Product #-}
sem_Pattern_Product :: (Pos) -> T_Patterns  -> T_Pattern 
sem_Pattern_Product arg_pos_ arg_pats_ = T_Pattern (return st32) where
   {-# NOINLINE st32 #-}
   st32 = let
      v31 :: T_Pattern_v31 
      v31 = \ (T_Pattern_vIn31 ) -> ( let
         _patsX35 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_pats_))
         (T_Patterns_vOut34 _patsIcopy _patsIdefinedAttrs _patsIdefinedInsts _patsIpatunder) = inv_Patterns_s35 _patsX35 (T_Patterns_vIn34 )
         _lhsOpatunder :: [AttrName]->Pattern
         _lhsOpatunder = rule616 _patsIpatunder arg_pos_
         _lhsOstpos :: Pos
         _lhsOstpos = rule617 arg_pos_
         _lhsOdefinedAttrs :: [AttrName]
         _lhsOdefinedAttrs = rule618 _patsIdefinedAttrs
         _lhsOdefinedInsts :: [Identifier]
         _lhsOdefinedInsts = rule619 _patsIdefinedInsts
         _copy = rule620 _patsIcopy arg_pos_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule621 _copy
         __result_ = T_Pattern_vOut31 _lhsOcopy _lhsOdefinedAttrs _lhsOdefinedInsts _lhsOpatunder _lhsOstpos
         in __result_ )
     in C_Pattern_s32 v31
   {-# INLINE rule616 #-}
   {-# LINE 1190 "./src-ag/Transform.ag" #-}
   rule616 = \ ((_patsIpatunder) :: [AttrName]->Patterns) pos_ ->
                                {-# LINE 1190 "./src-ag/Transform.ag" #-}
                                \us -> Product pos_ (_patsIpatunder us)
                                {-# LINE 4792 "dist/build/Transform.hs"#-}
   {-# INLINE rule617 #-}
   {-# LINE 1201 "./src-ag/Transform.ag" #-}
   rule617 = \ pos_ ->
                             {-# LINE 1201 "./src-ag/Transform.ag" #-}
                             pos_
                             {-# LINE 4798 "dist/build/Transform.hs"#-}
   {-# INLINE rule618 #-}
   rule618 = \ ((_patsIdefinedAttrs) :: [AttrName]) ->
     _patsIdefinedAttrs
   {-# INLINE rule619 #-}
   rule619 = \ ((_patsIdefinedInsts) :: [Identifier]) ->
     _patsIdefinedInsts
   {-# INLINE rule620 #-}
   rule620 = \ ((_patsIcopy) :: Patterns) pos_ ->
     Product pos_ _patsIcopy
   {-# INLINE rule621 #-}
   rule621 = \ _copy ->
     _copy
{-# NOINLINE sem_Pattern_Alias #-}
sem_Pattern_Alias :: (Identifier) -> (Identifier) -> T_Pattern  -> T_Pattern 
sem_Pattern_Alias arg_field_ arg_attr_ arg_pat_ = T_Pattern (return st32) where
   {-# NOINLINE st32 #-}
   st32 = let
      v31 :: T_Pattern_v31 
      v31 = \ (T_Pattern_vIn31 ) -> ( let
         _patX32 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pat_))
         (T_Pattern_vOut31 _patIcopy _patIdefinedAttrs _patIdefinedInsts _patIpatunder _patIstpos) = inv_Pattern_s32 _patX32 (T_Pattern_vIn31 )
         _lhsOdefinedAttrs :: [AttrName]
         _lhsOdefinedAttrs = rule622 _patIdefinedAttrs arg_attr_ arg_field_
         _lhsOpatunder :: [AttrName]->Pattern
         _lhsOpatunder = rule623 _copy arg_attr_ arg_field_
         _lhsOdefinedInsts :: [Identifier]
         _lhsOdefinedInsts = rule624 _patIdefinedInsts arg_attr_ arg_field_
         _lhsOstpos :: Pos
         _lhsOstpos = rule625 arg_field_
         _copy = rule626 _patIcopy arg_attr_ arg_field_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule627 _copy
         __result_ = T_Pattern_vOut31 _lhsOcopy _lhsOdefinedAttrs _lhsOdefinedInsts _lhsOpatunder _lhsOstpos
         in __result_ )
     in C_Pattern_s32 v31
   {-# INLINE rule622 #-}
   {-# LINE 1185 "./src-ag/Transform.ag" #-}
   rule622 = \ ((_patIdefinedAttrs) :: [AttrName]) attr_ field_ ->
                               {-# LINE 1185 "./src-ag/Transform.ag" #-}
                               (field_, attr_) : _patIdefinedAttrs
                               {-# LINE 4839 "dist/build/Transform.hs"#-}
   {-# INLINE rule623 #-}
   {-# LINE 1186 "./src-ag/Transform.ag" #-}
   rule623 = \ _copy attr_ field_ ->
                               {-# LINE 1186 "./src-ag/Transform.ag" #-}
                               \us -> if ((field_,attr_) `elem` us) then Underscore noPos else _copy
                               {-# LINE 4845 "dist/build/Transform.hs"#-}
   {-# INLINE rule624 #-}
   {-# LINE 1187 "./src-ag/Transform.ag" #-}
   rule624 = \ ((_patIdefinedInsts) :: [Identifier]) attr_ field_ ->
                               {-# LINE 1187 "./src-ag/Transform.ag" #-}
                               (if field_ == _INST then [attr_] else []) ++ _patIdefinedInsts
                               {-# LINE 4851 "dist/build/Transform.hs"#-}
   {-# INLINE rule625 #-}
   {-# LINE 1202 "./src-ag/Transform.ag" #-}
   rule625 = \ field_ ->
                             {-# LINE 1202 "./src-ag/Transform.ag" #-}
                             getPos field_
                             {-# LINE 4857 "dist/build/Transform.hs"#-}
   {-# INLINE rule626 #-}
   rule626 = \ ((_patIcopy) :: Pattern) attr_ field_ ->
     Alias field_ attr_ _patIcopy
   {-# INLINE rule627 #-}
   rule627 = \ _copy ->
     _copy
{-# NOINLINE sem_Pattern_Irrefutable #-}
sem_Pattern_Irrefutable :: T_Pattern  -> T_Pattern 
sem_Pattern_Irrefutable arg_pat_ = T_Pattern (return st32) where
   {-# NOINLINE st32 #-}
   st32 = let
      v31 :: T_Pattern_v31 
      v31 = \ (T_Pattern_vIn31 ) -> ( let
         _patX32 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pat_))
         (T_Pattern_vOut31 _patIcopy _patIdefinedAttrs _patIdefinedInsts _patIpatunder _patIstpos) = inv_Pattern_s32 _patX32 (T_Pattern_vIn31 )
         _lhsOpatunder :: [AttrName]->Pattern
         _lhsOpatunder = rule628 _patIpatunder
         _lhsOdefinedAttrs :: [AttrName]
         _lhsOdefinedAttrs = rule629 _patIdefinedAttrs
         _lhsOdefinedInsts :: [Identifier]
         _lhsOdefinedInsts = rule630 _patIdefinedInsts
         _copy = rule631 _patIcopy
         _lhsOcopy :: Pattern
         _lhsOcopy = rule632 _copy
         _lhsOstpos :: Pos
         _lhsOstpos = rule633 _patIstpos
         __result_ = T_Pattern_vOut31 _lhsOcopy _lhsOdefinedAttrs _lhsOdefinedInsts _lhsOpatunder _lhsOstpos
         in __result_ )
     in C_Pattern_s32 v31
   {-# INLINE rule628 #-}
   {-# LINE 1191 "./src-ag/Transform.ag" #-}
   rule628 = \ ((_patIpatunder) :: [AttrName]->Pattern) ->
                                 {-# LINE 1191 "./src-ag/Transform.ag" #-}
                                 \us -> Irrefutable (_patIpatunder us)
                                 {-# LINE 4892 "dist/build/Transform.hs"#-}
   {-# INLINE rule629 #-}
   rule629 = \ ((_patIdefinedAttrs) :: [AttrName]) ->
     _patIdefinedAttrs
   {-# INLINE rule630 #-}
   rule630 = \ ((_patIdefinedInsts) :: [Identifier]) ->
     _patIdefinedInsts
   {-# INLINE rule631 #-}
   rule631 = \ ((_patIcopy) :: Pattern) ->
     Irrefutable _patIcopy
   {-# INLINE rule632 #-}
   rule632 = \ _copy ->
     _copy
   {-# INLINE rule633 #-}
   rule633 = \ ((_patIstpos) :: Pos) ->
     _patIstpos
{-# NOINLINE sem_Pattern_Underscore #-}
sem_Pattern_Underscore :: (Pos) -> T_Pattern 
sem_Pattern_Underscore arg_pos_ = T_Pattern (return st32) where
   {-# NOINLINE st32 #-}
   st32 = let
      v31 :: T_Pattern_v31 
      v31 = \ (T_Pattern_vIn31 ) -> ( let
         _lhsOpatunder :: [AttrName]->Pattern
         _lhsOpatunder = rule634 _copy
         _lhsOstpos :: Pos
         _lhsOstpos = rule635 arg_pos_
         _lhsOdefinedAttrs :: [AttrName]
         _lhsOdefinedAttrs = rule636  ()
         _lhsOdefinedInsts :: [Identifier]
         _lhsOdefinedInsts = rule637  ()
         _copy = rule638 arg_pos_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule639 _copy
         __result_ = T_Pattern_vOut31 _lhsOcopy _lhsOdefinedAttrs _lhsOdefinedInsts _lhsOpatunder _lhsOstpos
         in __result_ )
     in C_Pattern_s32 v31
   {-# INLINE rule634 #-}
   {-# LINE 1188 "./src-ag/Transform.ag" #-}
   rule634 = \ _copy ->
                                {-# LINE 1188 "./src-ag/Transform.ag" #-}
                                \_ -> _copy
                                {-# LINE 4934 "dist/build/Transform.hs"#-}
   {-# INLINE rule635 #-}
   {-# LINE 1203 "./src-ag/Transform.ag" #-}
   rule635 = \ pos_ ->
                             {-# LINE 1203 "./src-ag/Transform.ag" #-}
                             pos_
                             {-# LINE 4940 "dist/build/Transform.hs"#-}
   {-# INLINE rule636 #-}
   rule636 = \  (_ :: ()) ->
     []
   {-# INLINE rule637 #-}
   rule637 = \  (_ :: ()) ->
     []
   {-# INLINE rule638 #-}
   rule638 = \ pos_ ->
     Underscore pos_
   {-# INLINE rule639 #-}
   rule639 = \ _copy ->
     _copy

-- Patterns ----------------------------------------------------
-- wrapper
data Inh_Patterns  = Inh_Patterns {  }
data Syn_Patterns  = Syn_Patterns { copy_Syn_Patterns :: (Patterns), definedAttrs_Syn_Patterns :: ([AttrName]), definedInsts_Syn_Patterns :: ([Identifier]), patunder_Syn_Patterns :: ([AttrName]->Patterns) }
{-# INLINABLE wrap_Patterns #-}
wrap_Patterns :: T_Patterns  -> Inh_Patterns  -> (Syn_Patterns )
wrap_Patterns (T_Patterns act) (Inh_Patterns ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Patterns_vIn34 
        (T_Patterns_vOut34 _lhsOcopy _lhsOdefinedAttrs _lhsOdefinedInsts _lhsOpatunder) <- return (inv_Patterns_s35 sem arg)
        return (Syn_Patterns _lhsOcopy _lhsOdefinedAttrs _lhsOdefinedInsts _lhsOpatunder)
   )

-- cata
{-# NOINLINE sem_Patterns #-}
sem_Patterns :: Patterns  -> T_Patterns 
sem_Patterns list = Prelude.foldr sem_Patterns_Cons sem_Patterns_Nil (Prelude.map sem_Pattern list)

-- semantic domain
newtype T_Patterns  = T_Patterns {
                                 attach_T_Patterns :: Identity (T_Patterns_s35 )
                                 }
newtype T_Patterns_s35  = C_Patterns_s35 {
                                         inv_Patterns_s35 :: (T_Patterns_v34 )
                                         }
data T_Patterns_s36  = C_Patterns_s36
type T_Patterns_v34  = (T_Patterns_vIn34 ) -> (T_Patterns_vOut34 )
data T_Patterns_vIn34  = T_Patterns_vIn34 
data T_Patterns_vOut34  = T_Patterns_vOut34 (Patterns) ([AttrName]) ([Identifier]) ([AttrName]->Patterns)
{-# NOINLINE sem_Patterns_Cons #-}
sem_Patterns_Cons :: T_Pattern  -> T_Patterns  -> T_Patterns 
sem_Patterns_Cons arg_hd_ arg_tl_ = T_Patterns (return st35) where
   {-# NOINLINE st35 #-}
   st35 = let
      v34 :: T_Patterns_v34 
      v34 = \ (T_Patterns_vIn34 ) -> ( let
         _hdX32 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_hd_))
         _tlX35 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_tl_))
         (T_Pattern_vOut31 _hdIcopy _hdIdefinedAttrs _hdIdefinedInsts _hdIpatunder _hdIstpos) = inv_Pattern_s32 _hdX32 (T_Pattern_vIn31 )
         (T_Patterns_vOut34 _tlIcopy _tlIdefinedAttrs _tlIdefinedInsts _tlIpatunder) = inv_Patterns_s35 _tlX35 (T_Patterns_vIn34 )
         _lhsOpatunder :: [AttrName]->Patterns
         _lhsOpatunder = rule640 _hdIpatunder _tlIpatunder
         _lhsOdefinedAttrs :: [AttrName]
         _lhsOdefinedAttrs = rule641 _hdIdefinedAttrs _tlIdefinedAttrs
         _lhsOdefinedInsts :: [Identifier]
         _lhsOdefinedInsts = rule642 _hdIdefinedInsts _tlIdefinedInsts
         _copy = rule643 _hdIcopy _tlIcopy
         _lhsOcopy :: Patterns
         _lhsOcopy = rule644 _copy
         __result_ = T_Patterns_vOut34 _lhsOcopy _lhsOdefinedAttrs _lhsOdefinedInsts _lhsOpatunder
         in __result_ )
     in C_Patterns_s35 v34
   {-# INLINE rule640 #-}
   {-# LINE 1195 "./src-ag/Transform.ag" #-}
   rule640 = \ ((_hdIpatunder) :: [AttrName]->Pattern) ((_tlIpatunder) :: [AttrName]->Patterns) ->
                          {-# LINE 1195 "./src-ag/Transform.ag" #-}
                          \us -> (_hdIpatunder us) : (_tlIpatunder us)
                          {-# LINE 5012 "dist/build/Transform.hs"#-}
   {-# INLINE rule641 #-}
   rule641 = \ ((_hdIdefinedAttrs) :: [AttrName]) ((_tlIdefinedAttrs) :: [AttrName]) ->
     _hdIdefinedAttrs ++ _tlIdefinedAttrs
   {-# INLINE rule642 #-}
   rule642 = \ ((_hdIdefinedInsts) :: [Identifier]) ((_tlIdefinedInsts) :: [Identifier]) ->
     _hdIdefinedInsts ++ _tlIdefinedInsts
   {-# INLINE rule643 #-}
   rule643 = \ ((_hdIcopy) :: Pattern) ((_tlIcopy) :: Patterns) ->
     (:) _hdIcopy _tlIcopy
   {-# INLINE rule644 #-}
   rule644 = \ _copy ->
     _copy
{-# NOINLINE sem_Patterns_Nil #-}
sem_Patterns_Nil ::  T_Patterns 
sem_Patterns_Nil  = T_Patterns (return st35) where
   {-# NOINLINE st35 #-}
   st35 = let
      v34 :: T_Patterns_v34 
      v34 = \ (T_Patterns_vIn34 ) -> ( let
         _lhsOpatunder :: [AttrName]->Patterns
         _lhsOpatunder = rule645  ()
         _lhsOdefinedAttrs :: [AttrName]
         _lhsOdefinedAttrs = rule646  ()
         _lhsOdefinedInsts :: [Identifier]
         _lhsOdefinedInsts = rule647  ()
         _copy = rule648  ()
         _lhsOcopy :: Patterns
         _lhsOcopy = rule649 _copy
         __result_ = T_Patterns_vOut34 _lhsOcopy _lhsOdefinedAttrs _lhsOdefinedInsts _lhsOpatunder
         in __result_ )
     in C_Patterns_s35 v34
   {-# INLINE rule645 #-}
   {-# LINE 1194 "./src-ag/Transform.ag" #-}
   rule645 = \  (_ :: ()) ->
                         {-# LINE 1194 "./src-ag/Transform.ag" #-}
                         \_ ->  []
                         {-# LINE 5049 "dist/build/Transform.hs"#-}
   {-# INLINE rule646 #-}
   rule646 = \  (_ :: ()) ->
     []
   {-# INLINE rule647 #-}
   rule647 = \  (_ :: ()) ->
     []
   {-# INLINE rule648 #-}
   rule648 = \  (_ :: ()) ->
     []
   {-# INLINE rule649 #-}
   rule649 = \ _copy ->
     _copy

-- SemAlt ------------------------------------------------------
-- wrapper
data Inh_SemAlt  = Inh_SemAlt { allAttrDecls_Inh_SemAlt :: (Map NontermIdent (Attributes, Attributes)), allAttrs_Inh_SemAlt :: (Map NontermIdent (Attributes, Attributes)), allFields_Inh_SemAlt :: (DataTypes), nts_Inh_SemAlt :: (Set NontermIdent), options_Inh_SemAlt :: (Options) }
data Syn_SemAlt  = Syn_SemAlt { attrOrderCollect_Syn_SemAlt :: (AttrOrderMap), collectedArounds_Syn_SemAlt :: ([ (NontermIdent, ConstructorIdent, [AroundInfo])  ]), collectedAugments_Syn_SemAlt :: ([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]), collectedInsts_Syn_SemAlt :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ]), collectedMerges_Syn_SemAlt :: ([ (NontermIdent, ConstructorIdent, [MergeInfo])   ]), collectedRules_Syn_SemAlt :: ([ (NontermIdent, ConstructorIdent, RuleInfo)]), collectedSigs_Syn_SemAlt :: ([ (NontermIdent, ConstructorIdent, SigInfo) ]), collectedUniques_Syn_SemAlt :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]), errors_Syn_SemAlt :: (Seq Error), semPragmasCollect_Syn_SemAlt :: (PragmaMap) }
{-# INLINABLE wrap_SemAlt #-}
wrap_SemAlt :: T_SemAlt  -> Inh_SemAlt  -> (Syn_SemAlt )
wrap_SemAlt (T_SemAlt act) (Inh_SemAlt _lhsIallAttrDecls _lhsIallAttrs _lhsIallFields _lhsInts _lhsIoptions) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_SemAlt_vIn37 _lhsIallAttrDecls _lhsIallAttrs _lhsIallFields _lhsInts _lhsIoptions
        (T_SemAlt_vOut37 _lhsOattrOrderCollect _lhsOcollectedArounds _lhsOcollectedAugments _lhsOcollectedInsts _lhsOcollectedMerges _lhsOcollectedRules _lhsOcollectedSigs _lhsOcollectedUniques _lhsOerrors _lhsOsemPragmasCollect) <- return (inv_SemAlt_s38 sem arg)
        return (Syn_SemAlt _lhsOattrOrderCollect _lhsOcollectedArounds _lhsOcollectedAugments _lhsOcollectedInsts _lhsOcollectedMerges _lhsOcollectedRules _lhsOcollectedSigs _lhsOcollectedUniques _lhsOerrors _lhsOsemPragmasCollect)
   )

-- cata
{-# INLINE sem_SemAlt #-}
sem_SemAlt :: SemAlt  -> T_SemAlt 
sem_SemAlt ( SemAlt pos_ constructorSet_ rules_ ) = sem_SemAlt_SemAlt pos_ ( sem_ConstructorSet constructorSet_ ) ( sem_SemDefs rules_ )

-- semantic domain
newtype T_SemAlt  = T_SemAlt {
                             attach_T_SemAlt :: Identity (T_SemAlt_s38 )
                             }
newtype T_SemAlt_s38  = C_SemAlt_s38 {
                                     inv_SemAlt_s38 :: (T_SemAlt_v37 )
                                     }
data T_SemAlt_s39  = C_SemAlt_s39
type T_SemAlt_v37  = (T_SemAlt_vIn37 ) -> (T_SemAlt_vOut37 )
data T_SemAlt_vIn37  = T_SemAlt_vIn37 (Map NontermIdent (Attributes, Attributes)) (Map NontermIdent (Attributes, Attributes)) (DataTypes) (Set NontermIdent) (Options)
data T_SemAlt_vOut37  = T_SemAlt_vOut37 (AttrOrderMap) ([ (NontermIdent, ConstructorIdent, [AroundInfo])  ]) ([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]) ([ (NontermIdent, ConstructorIdent, [Identifier]) ]) ([ (NontermIdent, ConstructorIdent, [MergeInfo])   ]) ([ (NontermIdent, ConstructorIdent, RuleInfo)]) ([ (NontermIdent, ConstructorIdent, SigInfo) ]) ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]) (Seq Error) (PragmaMap)
{-# NOINLINE sem_SemAlt_SemAlt #-}
sem_SemAlt_SemAlt :: (Pos) -> T_ConstructorSet  -> T_SemDefs  -> T_SemAlt 
sem_SemAlt_SemAlt _ arg_constructorSet_ arg_rules_ = T_SemAlt (return st38) where
   {-# NOINLINE st38 #-}
   st38 = let
      v37 :: T_SemAlt_v37 
      v37 = \ (T_SemAlt_vIn37 _lhsIallAttrDecls _lhsIallAttrs _lhsIallFields _lhsInts _lhsIoptions) -> ( let
         _constructorSetX14 = Control.Monad.Identity.runIdentity (attach_T_ConstructorSet (arg_constructorSet_))
         _rulesX47 = Control.Monad.Identity.runIdentity (attach_T_SemDefs (arg_rules_))
         (T_ConstructorSet_vOut13 _constructorSetIcollectedConstructorNames _constructorSetIconstructors _constructorSetIerrors) = inv_ConstructorSet_s14 _constructorSetX14 (T_ConstructorSet_vIn13 )
         (T_SemDefs_vOut46 _rulesIaroundInfos _rulesIaugmentInfos _rulesIdefinedInsts _rulesIerrors _rulesImergeInfos _rulesIorderDepsCollect _rulesIpragmaNamesCollect _rulesIruleInfos _rulesIsigInfos _rulesIuniqueInfos) = inv_SemDefs_s47 _rulesX47 (T_SemDefs_vIn46 _rulesOoptions)
         _pragmaNames = rule650 _rulesIpragmaNamesCollect
         _lhsOsemPragmasCollect :: PragmaMap
         _lhsOsemPragmasCollect = rule651 _coninfo _pragmaNames
         _attrOrders = rule652 _coninfo _rulesIorderDepsCollect
         _lhsOattrOrderCollect :: AttrOrderMap
         _lhsOattrOrderCollect = rule653 _attrOrders
         _coninfo = rule654 _constructorSetIconstructors _lhsIallFields _lhsInts
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule655 _coninfo _rulesIerrors
         _lhsOcollectedRules :: [ (NontermIdent, ConstructorIdent, RuleInfo)]
         _lhsOcollectedRules = rule656 _coninfo _rulesIruleInfos
         _lhsOcollectedSigs :: [ (NontermIdent, ConstructorIdent, SigInfo) ]
         _lhsOcollectedSigs = rule657 _coninfo _rulesIsigInfos
         _lhsOcollectedInsts :: [ (NontermIdent, ConstructorIdent, [Identifier]) ]
         _lhsOcollectedInsts = rule658 _coninfo _rulesIdefinedInsts
         _lhsOcollectedUniques :: [ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]
         _lhsOcollectedUniques = rule659 _coninfo _rulesIuniqueInfos
         _lhsOcollectedAugments :: [ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]
         _lhsOcollectedAugments = rule660 _coninfo _rulesIaugmentInfos
         _lhsOcollectedArounds :: [ (NontermIdent, ConstructorIdent, [AroundInfo])  ]
         _lhsOcollectedArounds = rule661 _coninfo _rulesIaroundInfos
         _lhsOcollectedMerges :: [ (NontermIdent, ConstructorIdent, [MergeInfo])   ]
         _lhsOcollectedMerges = rule662 _coninfo _rulesImergeInfos
         _rulesOoptions = rule663 _lhsIoptions
         __result_ = T_SemAlt_vOut37 _lhsOattrOrderCollect _lhsOcollectedArounds _lhsOcollectedAugments _lhsOcollectedInsts _lhsOcollectedMerges _lhsOcollectedRules _lhsOcollectedSigs _lhsOcollectedUniques _lhsOerrors _lhsOsemPragmasCollect
         in __result_ )
     in C_SemAlt_s38 v37
   {-# INLINE rule650 #-}
   {-# LINE 887 "./src-ag/Transform.ag" #-}
   rule650 = \ ((_rulesIpragmaNamesCollect) :: [Identifier]) ->
                                {-# LINE 887 "./src-ag/Transform.ag" #-}
                                Set.fromList _rulesIpragmaNamesCollect
                                {-# LINE 5136 "dist/build/Transform.hs"#-}
   {-# INLINE rule651 #-}
   {-# LINE 888 "./src-ag/Transform.ag" #-}
   rule651 = \ _coninfo _pragmaNames ->
                                {-# LINE 888 "./src-ag/Transform.ag" #-}
                                foldr pragmaMapUnion Map.empty [ pragmaMapSingle nt con _pragmaNames
                                                               | (nt, conset, _) <- _coninfo
                                                               , con <- Set.toList conset
                                                               ]
                                {-# LINE 5145 "dist/build/Transform.hs"#-}
   {-# INLINE rule652 #-}
   {-# LINE 917 "./src-ag/Transform.ag" #-}
   rule652 = \ _coninfo ((_rulesIorderDepsCollect) :: Set Dependency) ->
            {-# LINE 917 "./src-ag/Transform.ag" #-}
            [ orderMapSingle nt con _rulesIorderDepsCollect
            | (nt, conset, _) <- _coninfo
            , con <- Set.toList conset
            ]
            {-# LINE 5154 "dist/build/Transform.hs"#-}
   {-# INLINE rule653 #-}
   {-# LINE 922 "./src-ag/Transform.ag" #-}
   rule653 = \ _attrOrders ->
                               {-# LINE 922 "./src-ag/Transform.ag" #-}
                               foldr orderMapUnion Map.empty _attrOrders
                               {-# LINE 5160 "dist/build/Transform.hs"#-}
   {-# INLINE rule654 #-}
   {-# LINE 1104 "./src-ag/Transform.ag" #-}
   rule654 = \ ((_constructorSetIconstructors) :: (Set ConstructorIdent->Set ConstructorIdent)) ((_lhsIallFields) :: DataTypes) ((_lhsInts) :: Set NontermIdent) ->
                           {-# LINE 1104 "./src-ag/Transform.ag" #-}
                           [ (nt, conset, conkeys)
                           | nt  <- Set.toList _lhsInts
                           , let conmap = Map.findWithDefault Map.empty nt _lhsIallFields
                           , let conkeys = Set.fromList (Map.keys conmap)
                           , let conset  = _constructorSetIconstructors conkeys
                           ]
                           {-# LINE 5171 "dist/build/Transform.hs"#-}
   {-# INLINE rule655 #-}
   {-# LINE 1111 "./src-ag/Transform.ag" #-}
   rule655 = \ _coninfo ((_rulesIerrors) :: Seq Error) ->
                          {-# LINE 1111 "./src-ag/Transform.ag" #-}
                          Seq.fromList
                             [ UndefAlt nt con
                             | (nt, conset, conkeys) <- _coninfo
                             , con <- Set.toList (Set.difference conset conkeys)
                             ]
                          Seq.>< _rulesIerrors
                          {-# LINE 5182 "dist/build/Transform.hs"#-}
   {-# INLINE rule656 #-}
   {-# LINE 1118 "./src-ag/Transform.ag" #-}
   rule656 = \ _coninfo ((_rulesIruleInfos) :: [RuleInfo]) ->
                         {-# LINE 1118 "./src-ag/Transform.ag" #-}
                         [ (nt,con,r)
                         | (nt, conset, _) <- _coninfo
                         , con <- Set.toList conset
                         , r <- _rulesIruleInfos
                         ]
                         {-# LINE 5192 "dist/build/Transform.hs"#-}
   {-# INLINE rule657 #-}
   {-# LINE 1124 "./src-ag/Transform.ag" #-}
   rule657 = \ _coninfo ((_rulesIsigInfos) :: [SigInfo]) ->
                         {-# LINE 1124 "./src-ag/Transform.ag" #-}
                         [ (nt,con,ts)
                         | (nt, conset, _) <- _coninfo
                         , con <- Set.toList conset
                         , ts <- _rulesIsigInfos
                         ]
                         {-# LINE 5202 "dist/build/Transform.hs"#-}
   {-# INLINE rule658 #-}
   {-# LINE 1131 "./src-ag/Transform.ag" #-}
   rule658 = \ _coninfo ((_rulesIdefinedInsts) :: [Identifier]) ->
                         {-# LINE 1131 "./src-ag/Transform.ag" #-}
                         [ (nt,con,_rulesIdefinedInsts)
                         | (nt, conset, _) <- _coninfo
                         , con <- Set.toList conset
                         ]
                         {-# LINE 5211 "dist/build/Transform.hs"#-}
   {-# INLINE rule659 #-}
   {-# LINE 1137 "./src-ag/Transform.ag" #-}
   rule659 = \ _coninfo ((_rulesIuniqueInfos) :: [UniqueInfo]) ->
                         {-# LINE 1137 "./src-ag/Transform.ag" #-}
                         [ (nt,con,_rulesIuniqueInfos)
                         | (nt, conset, _) <- _coninfo
                         , con <- Set.toList conset
                         ]
                         {-# LINE 5220 "dist/build/Transform.hs"#-}
   {-# INLINE rule660 #-}
   {-# LINE 1143 "./src-ag/Transform.ag" #-}
   rule660 = \ _coninfo ((_rulesIaugmentInfos) :: [AugmentInfo]) ->
                         {-# LINE 1143 "./src-ag/Transform.ag" #-}
                         [ (nt, con, _rulesIaugmentInfos)
                         | (nt, conset, _) <- _coninfo
                         , con <- Set.toList conset
                         ]
                         {-# LINE 5229 "dist/build/Transform.hs"#-}
   {-# INLINE rule661 #-}
   {-# LINE 1149 "./src-ag/Transform.ag" #-}
   rule661 = \ _coninfo ((_rulesIaroundInfos) :: [AroundInfo]) ->
                         {-# LINE 1149 "./src-ag/Transform.ag" #-}
                         [ (nt, con, _rulesIaroundInfos)
                         | (nt, conset, _) <- _coninfo
                         , con <- Set.toList conset
                         ]
                         {-# LINE 5238 "dist/build/Transform.hs"#-}
   {-# INLINE rule662 #-}
   {-# LINE 1155 "./src-ag/Transform.ag" #-}
   rule662 = \ _coninfo ((_rulesImergeInfos) :: [MergeInfo]) ->
                         {-# LINE 1155 "./src-ag/Transform.ag" #-}
                         [ (nt, con, _rulesImergeInfos)
                         | (nt, conset, _) <- _coninfo
                         , con <- Set.toList conset
                         ]
                         {-# LINE 5247 "dist/build/Transform.hs"#-}
   {-# INLINE rule663 #-}
   rule663 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions

-- SemAlts -----------------------------------------------------
-- wrapper
data Inh_SemAlts  = Inh_SemAlts { allAttrDecls_Inh_SemAlts :: (Map NontermIdent (Attributes, Attributes)), allAttrs_Inh_SemAlts :: (Map NontermIdent (Attributes, Attributes)), allFields_Inh_SemAlts :: (DataTypes), nts_Inh_SemAlts :: (Set NontermIdent), options_Inh_SemAlts :: (Options) }
data Syn_SemAlts  = Syn_SemAlts { attrOrderCollect_Syn_SemAlts :: (AttrOrderMap), collectedArounds_Syn_SemAlts :: ([ (NontermIdent, ConstructorIdent, [AroundInfo])  ]), collectedAugments_Syn_SemAlts :: ([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]), collectedInsts_Syn_SemAlts :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ]), collectedMerges_Syn_SemAlts :: ([ (NontermIdent, ConstructorIdent, [MergeInfo])   ]), collectedRules_Syn_SemAlts :: ([ (NontermIdent, ConstructorIdent, RuleInfo)]), collectedSigs_Syn_SemAlts :: ([ (NontermIdent, ConstructorIdent, SigInfo) ]), collectedUniques_Syn_SemAlts :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]), errors_Syn_SemAlts :: (Seq Error), semPragmasCollect_Syn_SemAlts :: (PragmaMap) }
{-# INLINABLE wrap_SemAlts #-}
wrap_SemAlts :: T_SemAlts  -> Inh_SemAlts  -> (Syn_SemAlts )
wrap_SemAlts (T_SemAlts act) (Inh_SemAlts _lhsIallAttrDecls _lhsIallAttrs _lhsIallFields _lhsInts _lhsIoptions) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_SemAlts_vIn40 _lhsIallAttrDecls _lhsIallAttrs _lhsIallFields _lhsInts _lhsIoptions
        (T_SemAlts_vOut40 _lhsOattrOrderCollect _lhsOcollectedArounds _lhsOcollectedAugments _lhsOcollectedInsts _lhsOcollectedMerges _lhsOcollectedRules _lhsOcollectedSigs _lhsOcollectedUniques _lhsOerrors _lhsOsemPragmasCollect) <- return (inv_SemAlts_s41 sem arg)
        return (Syn_SemAlts _lhsOattrOrderCollect _lhsOcollectedArounds _lhsOcollectedAugments _lhsOcollectedInsts _lhsOcollectedMerges _lhsOcollectedRules _lhsOcollectedSigs _lhsOcollectedUniques _lhsOerrors _lhsOsemPragmasCollect)
   )

-- cata
{-# NOINLINE sem_SemAlts #-}
sem_SemAlts :: SemAlts  -> T_SemAlts 
sem_SemAlts list = Prelude.foldr sem_SemAlts_Cons sem_SemAlts_Nil (Prelude.map sem_SemAlt list)

-- semantic domain
newtype T_SemAlts  = T_SemAlts {
                               attach_T_SemAlts :: Identity (T_SemAlts_s41 )
                               }
newtype T_SemAlts_s41  = C_SemAlts_s41 {
                                       inv_SemAlts_s41 :: (T_SemAlts_v40 )
                                       }
data T_SemAlts_s42  = C_SemAlts_s42
type T_SemAlts_v40  = (T_SemAlts_vIn40 ) -> (T_SemAlts_vOut40 )
data T_SemAlts_vIn40  = T_SemAlts_vIn40 (Map NontermIdent (Attributes, Attributes)) (Map NontermIdent (Attributes, Attributes)) (DataTypes) (Set NontermIdent) (Options)
data T_SemAlts_vOut40  = T_SemAlts_vOut40 (AttrOrderMap) ([ (NontermIdent, ConstructorIdent, [AroundInfo])  ]) ([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]) ([ (NontermIdent, ConstructorIdent, [Identifier]) ]) ([ (NontermIdent, ConstructorIdent, [MergeInfo])   ]) ([ (NontermIdent, ConstructorIdent, RuleInfo)]) ([ (NontermIdent, ConstructorIdent, SigInfo) ]) ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]) (Seq Error) (PragmaMap)
{-# NOINLINE sem_SemAlts_Cons #-}
sem_SemAlts_Cons :: T_SemAlt  -> T_SemAlts  -> T_SemAlts 
sem_SemAlts_Cons arg_hd_ arg_tl_ = T_SemAlts (return st41) where
   {-# NOINLINE st41 #-}
   st41 = let
      v40 :: T_SemAlts_v40 
      v40 = \ (T_SemAlts_vIn40 _lhsIallAttrDecls _lhsIallAttrs _lhsIallFields _lhsInts _lhsIoptions) -> ( let
         _hdX38 = Control.Monad.Identity.runIdentity (attach_T_SemAlt (arg_hd_))
         _tlX41 = Control.Monad.Identity.runIdentity (attach_T_SemAlts (arg_tl_))
         (T_SemAlt_vOut37 _hdIattrOrderCollect _hdIcollectedArounds _hdIcollectedAugments _hdIcollectedInsts _hdIcollectedMerges _hdIcollectedRules _hdIcollectedSigs _hdIcollectedUniques _hdIerrors _hdIsemPragmasCollect) = inv_SemAlt_s38 _hdX38 (T_SemAlt_vIn37 _hdOallAttrDecls _hdOallAttrs _hdOallFields _hdOnts _hdOoptions)
         (T_SemAlts_vOut40 _tlIattrOrderCollect _tlIcollectedArounds _tlIcollectedAugments _tlIcollectedInsts _tlIcollectedMerges _tlIcollectedRules _tlIcollectedSigs _tlIcollectedUniques _tlIerrors _tlIsemPragmasCollect) = inv_SemAlts_s41 _tlX41 (T_SemAlts_vIn40 _tlOallAttrDecls _tlOallAttrs _tlOallFields _tlOnts _tlOoptions)
         _lhsOattrOrderCollect :: AttrOrderMap
         _lhsOattrOrderCollect = rule664 _hdIattrOrderCollect _tlIattrOrderCollect
         _lhsOcollectedArounds :: [ (NontermIdent, ConstructorIdent, [AroundInfo])  ]
         _lhsOcollectedArounds = rule665 _hdIcollectedArounds _tlIcollectedArounds
         _lhsOcollectedAugments :: [ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]
         _lhsOcollectedAugments = rule666 _hdIcollectedAugments _tlIcollectedAugments
         _lhsOcollectedInsts :: [ (NontermIdent, ConstructorIdent, [Identifier]) ]
         _lhsOcollectedInsts = rule667 _hdIcollectedInsts _tlIcollectedInsts
         _lhsOcollectedMerges :: [ (NontermIdent, ConstructorIdent, [MergeInfo])   ]
         _lhsOcollectedMerges = rule668 _hdIcollectedMerges _tlIcollectedMerges
         _lhsOcollectedRules :: [ (NontermIdent, ConstructorIdent, RuleInfo)]
         _lhsOcollectedRules = rule669 _hdIcollectedRules _tlIcollectedRules
         _lhsOcollectedSigs :: [ (NontermIdent, ConstructorIdent, SigInfo) ]
         _lhsOcollectedSigs = rule670 _hdIcollectedSigs _tlIcollectedSigs
         _lhsOcollectedUniques :: [ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]
         _lhsOcollectedUniques = rule671 _hdIcollectedUniques _tlIcollectedUniques
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule672 _hdIerrors _tlIerrors
         _lhsOsemPragmasCollect :: PragmaMap
         _lhsOsemPragmasCollect = rule673 _hdIsemPragmasCollect _tlIsemPragmasCollect
         _hdOallAttrDecls = rule674 _lhsIallAttrDecls
         _hdOallAttrs = rule675 _lhsIallAttrs
         _hdOallFields = rule676 _lhsIallFields
         _hdOnts = rule677 _lhsInts
         _hdOoptions = rule678 _lhsIoptions
         _tlOallAttrDecls = rule679 _lhsIallAttrDecls
         _tlOallAttrs = rule680 _lhsIallAttrs
         _tlOallFields = rule681 _lhsIallFields
         _tlOnts = rule682 _lhsInts
         _tlOoptions = rule683 _lhsIoptions
         __result_ = T_SemAlts_vOut40 _lhsOattrOrderCollect _lhsOcollectedArounds _lhsOcollectedAugments _lhsOcollectedInsts _lhsOcollectedMerges _lhsOcollectedRules _lhsOcollectedSigs _lhsOcollectedUniques _lhsOerrors _lhsOsemPragmasCollect
         in __result_ )
     in C_SemAlts_s41 v40
   {-# INLINE rule664 #-}
   rule664 = \ ((_hdIattrOrderCollect) :: AttrOrderMap) ((_tlIattrOrderCollect) :: AttrOrderMap) ->
     _hdIattrOrderCollect `orderMapUnion` _tlIattrOrderCollect
   {-# INLINE rule665 #-}
   rule665 = \ ((_hdIcollectedArounds) :: [ (NontermIdent, ConstructorIdent, [AroundInfo])  ]) ((_tlIcollectedArounds) :: [ (NontermIdent, ConstructorIdent, [AroundInfo])  ]) ->
     _hdIcollectedArounds ++ _tlIcollectedArounds
   {-# INLINE rule666 #-}
   rule666 = \ ((_hdIcollectedAugments) :: [ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]) ((_tlIcollectedAugments) :: [ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]) ->
     _hdIcollectedAugments ++ _tlIcollectedAugments
   {-# INLINE rule667 #-}
   rule667 = \ ((_hdIcollectedInsts) :: [ (NontermIdent, ConstructorIdent, [Identifier]) ]) ((_tlIcollectedInsts) :: [ (NontermIdent, ConstructorIdent, [Identifier]) ]) ->
     _hdIcollectedInsts ++ _tlIcollectedInsts
   {-# INLINE rule668 #-}
   rule668 = \ ((_hdIcollectedMerges) :: [ (NontermIdent, ConstructorIdent, [MergeInfo])   ]) ((_tlIcollectedMerges) :: [ (NontermIdent, ConstructorIdent, [MergeInfo])   ]) ->
     _hdIcollectedMerges ++ _tlIcollectedMerges
   {-# INLINE rule669 #-}
   rule669 = \ ((_hdIcollectedRules) :: [ (NontermIdent, ConstructorIdent, RuleInfo)]) ((_tlIcollectedRules) :: [ (NontermIdent, ConstructorIdent, RuleInfo)]) ->
     _hdIcollectedRules ++ _tlIcollectedRules
   {-# INLINE rule670 #-}
   rule670 = \ ((_hdIcollectedSigs) :: [ (NontermIdent, ConstructorIdent, SigInfo) ]) ((_tlIcollectedSigs) :: [ (NontermIdent, ConstructorIdent, SigInfo) ]) ->
     _hdIcollectedSigs ++ _tlIcollectedSigs
   {-# INLINE rule671 #-}
   rule671 = \ ((_hdIcollectedUniques) :: [ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]) ((_tlIcollectedUniques) :: [ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]) ->
     _hdIcollectedUniques ++ _tlIcollectedUniques
   {-# INLINE rule672 #-}
   rule672 = \ ((_hdIerrors) :: Seq Error) ((_tlIerrors) :: Seq Error) ->
     _hdIerrors Seq.>< _tlIerrors
   {-# INLINE rule673 #-}
   rule673 = \ ((_hdIsemPragmasCollect) :: PragmaMap) ((_tlIsemPragmasCollect) :: PragmaMap) ->
     _hdIsemPragmasCollect `pragmaMapUnion` _tlIsemPragmasCollect
   {-# INLINE rule674 #-}
   rule674 = \ ((_lhsIallAttrDecls) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIallAttrDecls
   {-# INLINE rule675 #-}
   rule675 = \ ((_lhsIallAttrs) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIallAttrs
   {-# INLINE rule676 #-}
   rule676 = \ ((_lhsIallFields) :: DataTypes) ->
     _lhsIallFields
   {-# INLINE rule677 #-}
   rule677 = \ ((_lhsInts) :: Set NontermIdent) ->
     _lhsInts
   {-# INLINE rule678 #-}
   rule678 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule679 #-}
   rule679 = \ ((_lhsIallAttrDecls) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIallAttrDecls
   {-# INLINE rule680 #-}
   rule680 = \ ((_lhsIallAttrs) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIallAttrs
   {-# INLINE rule681 #-}
   rule681 = \ ((_lhsIallFields) :: DataTypes) ->
     _lhsIallFields
   {-# INLINE rule682 #-}
   rule682 = \ ((_lhsInts) :: Set NontermIdent) ->
     _lhsInts
   {-# INLINE rule683 #-}
   rule683 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_SemAlts_Nil #-}
sem_SemAlts_Nil ::  T_SemAlts 
sem_SemAlts_Nil  = T_SemAlts (return st41) where
   {-# NOINLINE st41 #-}
   st41 = let
      v40 :: T_SemAlts_v40 
      v40 = \ (T_SemAlts_vIn40 _lhsIallAttrDecls _lhsIallAttrs _lhsIallFields _lhsInts _lhsIoptions) -> ( let
         _lhsOattrOrderCollect :: AttrOrderMap
         _lhsOattrOrderCollect = rule684  ()
         _lhsOcollectedArounds :: [ (NontermIdent, ConstructorIdent, [AroundInfo])  ]
         _lhsOcollectedArounds = rule685  ()
         _lhsOcollectedAugments :: [ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]
         _lhsOcollectedAugments = rule686  ()
         _lhsOcollectedInsts :: [ (NontermIdent, ConstructorIdent, [Identifier]) ]
         _lhsOcollectedInsts = rule687  ()
         _lhsOcollectedMerges :: [ (NontermIdent, ConstructorIdent, [MergeInfo])   ]
         _lhsOcollectedMerges = rule688  ()
         _lhsOcollectedRules :: [ (NontermIdent, ConstructorIdent, RuleInfo)]
         _lhsOcollectedRules = rule689  ()
         _lhsOcollectedSigs :: [ (NontermIdent, ConstructorIdent, SigInfo) ]
         _lhsOcollectedSigs = rule690  ()
         _lhsOcollectedUniques :: [ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]
         _lhsOcollectedUniques = rule691  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule692  ()
         _lhsOsemPragmasCollect :: PragmaMap
         _lhsOsemPragmasCollect = rule693  ()
         __result_ = T_SemAlts_vOut40 _lhsOattrOrderCollect _lhsOcollectedArounds _lhsOcollectedAugments _lhsOcollectedInsts _lhsOcollectedMerges _lhsOcollectedRules _lhsOcollectedSigs _lhsOcollectedUniques _lhsOerrors _lhsOsemPragmasCollect
         in __result_ )
     in C_SemAlts_s41 v40
   {-# INLINE rule684 #-}
   rule684 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule685 #-}
   rule685 = \  (_ :: ()) ->
     []
   {-# INLINE rule686 #-}
   rule686 = \  (_ :: ()) ->
     []
   {-# INLINE rule687 #-}
   rule687 = \  (_ :: ()) ->
     []
   {-# INLINE rule688 #-}
   rule688 = \  (_ :: ()) ->
     []
   {-# INLINE rule689 #-}
   rule689 = \  (_ :: ()) ->
     []
   {-# INLINE rule690 #-}
   rule690 = \  (_ :: ()) ->
     []
   {-# INLINE rule691 #-}
   rule691 = \  (_ :: ()) ->
     []
   {-# INLINE rule692 #-}
   rule692 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule693 #-}
   rule693 = \  (_ :: ()) ->
     Map.empty

-- SemDef ------------------------------------------------------
-- wrapper
data Inh_SemDef  = Inh_SemDef { options_Inh_SemDef :: (Options) }
data Syn_SemDef  = Syn_SemDef { aroundInfos_Syn_SemDef :: ([AroundInfo]), augmentInfos_Syn_SemDef :: ([AugmentInfo]), definedInsts_Syn_SemDef :: ([Identifier]), errors_Syn_SemDef :: (Seq Error), mergeInfos_Syn_SemDef :: ([MergeInfo]), orderDepsCollect_Syn_SemDef :: (Set Dependency), pragmaNamesCollect_Syn_SemDef :: ([Identifier]), ruleInfos_Syn_SemDef :: ([RuleInfo]), sigInfos_Syn_SemDef :: ([SigInfo]), uniqueInfos_Syn_SemDef :: ([UniqueInfo]) }
{-# INLINABLE wrap_SemDef #-}
wrap_SemDef :: T_SemDef  -> Inh_SemDef  -> (Syn_SemDef )
wrap_SemDef (T_SemDef act) (Inh_SemDef _lhsIoptions) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_SemDef_vIn43 _lhsIoptions
        (T_SemDef_vOut43 _lhsOaroundInfos _lhsOaugmentInfos _lhsOdefinedInsts _lhsOerrors _lhsOmergeInfos _lhsOorderDepsCollect _lhsOpragmaNamesCollect _lhsOruleInfos _lhsOsigInfos _lhsOuniqueInfos) <- return (inv_SemDef_s44 sem arg)
        return (Syn_SemDef _lhsOaroundInfos _lhsOaugmentInfos _lhsOdefinedInsts _lhsOerrors _lhsOmergeInfos _lhsOorderDepsCollect _lhsOpragmaNamesCollect _lhsOruleInfos _lhsOsigInfos _lhsOuniqueInfos)
   )

-- cata
{-# NOINLINE sem_SemDef #-}
sem_SemDef :: SemDef  -> T_SemDef 
sem_SemDef ( Def pos_ mbName_ pattern_ rhs_ owrt_ pure_ eager_ ) = sem_SemDef_Def pos_ mbName_ ( sem_Pattern pattern_ ) rhs_ owrt_ pure_ eager_
sem_SemDef ( TypeDef pos_ ident_ tp_ ) = sem_SemDef_TypeDef pos_ ident_ tp_
sem_SemDef ( UniqueDef ident_ ref_ ) = sem_SemDef_UniqueDef ident_ ref_
sem_SemDef ( AugmentDef ident_ rhs_ ) = sem_SemDef_AugmentDef ident_ rhs_
sem_SemDef ( AroundDef ident_ rhs_ ) = sem_SemDef_AroundDef ident_ rhs_
sem_SemDef ( MergeDef target_ nt_ sources_ rhs_ ) = sem_SemDef_MergeDef target_ nt_ sources_ rhs_
sem_SemDef ( SemPragma names_ ) = sem_SemDef_SemPragma names_
sem_SemDef ( AttrOrderBefore before_ after_ ) = sem_SemDef_AttrOrderBefore before_ after_

-- semantic domain
newtype T_SemDef  = T_SemDef {
                             attach_T_SemDef :: Identity (T_SemDef_s44 )
                             }
newtype T_SemDef_s44  = C_SemDef_s44 {
                                     inv_SemDef_s44 :: (T_SemDef_v43 )
                                     }
data T_SemDef_s45  = C_SemDef_s45
type T_SemDef_v43  = (T_SemDef_vIn43 ) -> (T_SemDef_vOut43 )
data T_SemDef_vIn43  = T_SemDef_vIn43 (Options)
data T_SemDef_vOut43  = T_SemDef_vOut43 ([AroundInfo]) ([AugmentInfo]) ([Identifier]) (Seq Error) ([MergeInfo]) (Set Dependency) ([Identifier]) ([RuleInfo]) ([SigInfo]) ([UniqueInfo])
{-# NOINLINE sem_SemDef_Def #-}
sem_SemDef_Def :: (Pos) -> (Maybe Identifier) -> T_Pattern  -> (Expression) -> (Bool) -> (Bool) -> (Bool) -> T_SemDef 
sem_SemDef_Def _ arg_mbName_ arg_pattern_ arg_rhs_ arg_owrt_ arg_pure_ arg_eager_ = T_SemDef (return st44) where
   {-# NOINLINE st44 #-}
   st44 = let
      v43 :: T_SemDef_v43 
      v43 = \ (T_SemDef_vIn43 _lhsIoptions) -> ( let
         _patternX32 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pattern_))
         (T_Pattern_vOut31 _patternIcopy _patternIdefinedAttrs _patternIdefinedInsts _patternIpatunder _patternIstpos) = inv_Pattern_s32 _patternX32 (T_Pattern_vIn31 )
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule694 _lhsIoptions arg_rhs_
         _lhsOruleInfos :: [RuleInfo]
         _lhsOruleInfos = rule695 _patternIdefinedAttrs _patternIpatunder _patternIstpos arg_eager_ arg_mbName_ arg_owrt_ arg_pure_ arg_rhs_
         _lhsOaroundInfos :: [AroundInfo]
         _lhsOaroundInfos = rule696  ()
         _lhsOaugmentInfos :: [AugmentInfo]
         _lhsOaugmentInfos = rule697  ()
         _lhsOdefinedInsts :: [Identifier]
         _lhsOdefinedInsts = rule698 _patternIdefinedInsts
         _lhsOmergeInfos :: [MergeInfo]
         _lhsOmergeInfos = rule699  ()
         _lhsOorderDepsCollect :: Set Dependency
         _lhsOorderDepsCollect = rule700  ()
         _lhsOpragmaNamesCollect :: [Identifier]
         _lhsOpragmaNamesCollect = rule701  ()
         _lhsOsigInfos :: [SigInfo]
         _lhsOsigInfos = rule702  ()
         _lhsOuniqueInfos :: [UniqueInfo]
         _lhsOuniqueInfos = rule703  ()
         __result_ = T_SemDef_vOut43 _lhsOaroundInfos _lhsOaugmentInfos _lhsOdefinedInsts _lhsOerrors _lhsOmergeInfos _lhsOorderDepsCollect _lhsOpragmaNamesCollect _lhsOruleInfos _lhsOsigInfos _lhsOuniqueInfos
         in __result_ )
     in C_SemDef_s44 v43
   {-# INLINE rule694 #-}
   {-# LINE 556 "./src-ag/Transform.ag" #-}
   rule694 = \ ((_lhsIoptions) :: Options) rhs_ ->
                 {-# LINE 556 "./src-ag/Transform.ag" #-}
                 if checkParseRhs _lhsIoptions
                 then Seq.fromList $ checkRhs rhs_
                 else Seq.empty
                 {-# LINE 5523 "dist/build/Transform.hs"#-}
   {-# INLINE rule695 #-}
   {-# LINE 1161 "./src-ag/Transform.ag" #-}
   rule695 = \ ((_patternIdefinedAttrs) :: [AttrName]) ((_patternIpatunder) :: [AttrName]->Pattern) ((_patternIstpos) :: Pos) eager_ mbName_ owrt_ pure_ rhs_ ->
                           {-# LINE 1161 "./src-ag/Transform.ag" #-}
                           [ (mbName_, _patternIpatunder, rhs_, _patternIdefinedAttrs, owrt_, show _patternIstpos, pure_, eager_) ]
                           {-# LINE 5529 "dist/build/Transform.hs"#-}
   {-# INLINE rule696 #-}
   rule696 = \  (_ :: ()) ->
     []
   {-# INLINE rule697 #-}
   rule697 = \  (_ :: ()) ->
     []
   {-# INLINE rule698 #-}
   rule698 = \ ((_patternIdefinedInsts) :: [Identifier]) ->
     _patternIdefinedInsts
   {-# INLINE rule699 #-}
   rule699 = \  (_ :: ()) ->
     []
   {-# INLINE rule700 #-}
   rule700 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule701 #-}
   rule701 = \  (_ :: ()) ->
     []
   {-# INLINE rule702 #-}
   rule702 = \  (_ :: ()) ->
     []
   {-# INLINE rule703 #-}
   rule703 = \  (_ :: ()) ->
     []
{-# NOINLINE sem_SemDef_TypeDef #-}
sem_SemDef_TypeDef :: (Pos) -> (Identifier) -> (Type) -> T_SemDef 
sem_SemDef_TypeDef arg_pos_ arg_ident_ arg_tp_ = T_SemDef (return st44) where
   {-# NOINLINE st44 #-}
   st44 = let
      v43 :: T_SemDef_v43 
      v43 = \ (T_SemDef_vIn43 _lhsIoptions) -> ( let
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule704 _lhsIoptions arg_pos_ arg_tp_
         _lhsOsigInfos :: [SigInfo]
         _lhsOsigInfos = rule705 arg_ident_ arg_tp_
         _lhsOaroundInfos :: [AroundInfo]
         _lhsOaroundInfos = rule706  ()
         _lhsOaugmentInfos :: [AugmentInfo]
         _lhsOaugmentInfos = rule707  ()
         _lhsOdefinedInsts :: [Identifier]
         _lhsOdefinedInsts = rule708  ()
         _lhsOmergeInfos :: [MergeInfo]
         _lhsOmergeInfos = rule709  ()
         _lhsOorderDepsCollect :: Set Dependency
         _lhsOorderDepsCollect = rule710  ()
         _lhsOpragmaNamesCollect :: [Identifier]
         _lhsOpragmaNamesCollect = rule711  ()
         _lhsOruleInfos :: [RuleInfo]
         _lhsOruleInfos = rule712  ()
         _lhsOuniqueInfos :: [UniqueInfo]
         _lhsOuniqueInfos = rule713  ()
         __result_ = T_SemDef_vOut43 _lhsOaroundInfos _lhsOaugmentInfos _lhsOdefinedInsts _lhsOerrors _lhsOmergeInfos _lhsOorderDepsCollect _lhsOpragmaNamesCollect _lhsOruleInfos _lhsOsigInfos _lhsOuniqueInfos
         in __result_ )
     in C_SemDef_s44 v43
   {-# INLINE rule704 #-}
   {-# LINE 563 "./src-ag/Transform.ag" #-}
   rule704 = \ ((_lhsIoptions) :: Options) pos_ tp_ ->
                 {-# LINE 563 "./src-ag/Transform.ag" #-}
                 if checkParseTy _lhsIoptions
                 then case tp_ of
                        Haskell s -> let ex  = Expression pos_ tks
                                         tks = [tk]
                                         tk  = HsToken s pos_
                                     in Seq.fromList $ checkTy ex
                        _ -> Seq.empty
                 else Seq.empty
                 {-# LINE 5596 "dist/build/Transform.hs"#-}
   {-# INLINE rule705 #-}
   {-# LINE 1164 "./src-ag/Transform.ag" #-}
   rule705 = \ ident_ tp_ ->
                              {-# LINE 1164 "./src-ag/Transform.ag" #-}
                              [ (ident_, tp_) ]
                              {-# LINE 5602 "dist/build/Transform.hs"#-}
   {-# INLINE rule706 #-}
   rule706 = \  (_ :: ()) ->
     []
   {-# INLINE rule707 #-}
   rule707 = \  (_ :: ()) ->
     []
   {-# INLINE rule708 #-}
   rule708 = \  (_ :: ()) ->
     []
   {-# INLINE rule709 #-}
   rule709 = \  (_ :: ()) ->
     []
   {-# INLINE rule710 #-}
   rule710 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule711 #-}
   rule711 = \  (_ :: ()) ->
     []
   {-# INLINE rule712 #-}
   rule712 = \  (_ :: ()) ->
     []
   {-# INLINE rule713 #-}
   rule713 = \  (_ :: ()) ->
     []
{-# NOINLINE sem_SemDef_UniqueDef #-}
sem_SemDef_UniqueDef :: (Identifier) -> (Identifier) -> T_SemDef 
sem_SemDef_UniqueDef arg_ident_ arg_ref_ = T_SemDef (return st44) where
   {-# NOINLINE st44 #-}
   st44 = let
      v43 :: T_SemDef_v43 
      v43 = \ (T_SemDef_vIn43 _lhsIoptions) -> ( let
         _lhsOuniqueInfos :: [UniqueInfo]
         _lhsOuniqueInfos = rule714 arg_ident_ arg_ref_
         _lhsOaroundInfos :: [AroundInfo]
         _lhsOaroundInfos = rule715  ()
         _lhsOaugmentInfos :: [AugmentInfo]
         _lhsOaugmentInfos = rule716  ()
         _lhsOdefinedInsts :: [Identifier]
         _lhsOdefinedInsts = rule717  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule718  ()
         _lhsOmergeInfos :: [MergeInfo]
         _lhsOmergeInfos = rule719  ()
         _lhsOorderDepsCollect :: Set Dependency
         _lhsOorderDepsCollect = rule720  ()
         _lhsOpragmaNamesCollect :: [Identifier]
         _lhsOpragmaNamesCollect = rule721  ()
         _lhsOruleInfos :: [RuleInfo]
         _lhsOruleInfos = rule722  ()
         _lhsOsigInfos :: [SigInfo]
         _lhsOsigInfos = rule723  ()
         __result_ = T_SemDef_vOut43 _lhsOaroundInfos _lhsOaugmentInfos _lhsOdefinedInsts _lhsOerrors _lhsOmergeInfos _lhsOorderDepsCollect _lhsOpragmaNamesCollect _lhsOruleInfos _lhsOsigInfos _lhsOuniqueInfos
         in __result_ )
     in C_SemDef_s44 v43
   {-# INLINE rule714 #-}
   {-# LINE 1167 "./src-ag/Transform.ag" #-}
   rule714 = \ ident_ ref_ ->
                                   {-# LINE 1167 "./src-ag/Transform.ag" #-}
                                   [ (ident_, ref_) ]
                                   {-# LINE 5662 "dist/build/Transform.hs"#-}
   {-# INLINE rule715 #-}
   rule715 = \  (_ :: ()) ->
     []
   {-# INLINE rule716 #-}
   rule716 = \  (_ :: ()) ->
     []
   {-# INLINE rule717 #-}
   rule717 = \  (_ :: ()) ->
     []
   {-# INLINE rule718 #-}
   rule718 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule719 #-}
   rule719 = \  (_ :: ()) ->
     []
   {-# INLINE rule720 #-}
   rule720 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule721 #-}
   rule721 = \  (_ :: ()) ->
     []
   {-# INLINE rule722 #-}
   rule722 = \  (_ :: ()) ->
     []
   {-# INLINE rule723 #-}
   rule723 = \  (_ :: ()) ->
     []
{-# NOINLINE sem_SemDef_AugmentDef #-}
sem_SemDef_AugmentDef :: (Identifier) -> (Expression) -> T_SemDef 
sem_SemDef_AugmentDef arg_ident_ arg_rhs_ = T_SemDef (return st44) where
   {-# NOINLINE st44 #-}
   st44 = let
      v43 :: T_SemDef_v43 
      v43 = \ (T_SemDef_vIn43 _lhsIoptions) -> ( let
         _lhsOaugmentInfos :: [AugmentInfo]
         _lhsOaugmentInfos = rule724 arg_ident_ arg_rhs_
         _lhsOaroundInfos :: [AroundInfo]
         _lhsOaroundInfos = rule725  ()
         _lhsOdefinedInsts :: [Identifier]
         _lhsOdefinedInsts = rule726  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule727  ()
         _lhsOmergeInfos :: [MergeInfo]
         _lhsOmergeInfos = rule728  ()
         _lhsOorderDepsCollect :: Set Dependency
         _lhsOorderDepsCollect = rule729  ()
         _lhsOpragmaNamesCollect :: [Identifier]
         _lhsOpragmaNamesCollect = rule730  ()
         _lhsOruleInfos :: [RuleInfo]
         _lhsOruleInfos = rule731  ()
         _lhsOsigInfos :: [SigInfo]
         _lhsOsigInfos = rule732  ()
         _lhsOuniqueInfos :: [UniqueInfo]
         _lhsOuniqueInfos = rule733  ()
         __result_ = T_SemDef_vOut43 _lhsOaroundInfos _lhsOaugmentInfos _lhsOdefinedInsts _lhsOerrors _lhsOmergeInfos _lhsOorderDepsCollect _lhsOpragmaNamesCollect _lhsOruleInfos _lhsOsigInfos _lhsOuniqueInfos
         in __result_ )
     in C_SemDef_s44 v43
   {-# INLINE rule724 #-}
   {-# LINE 1170 "./src-ag/Transform.ag" #-}
   rule724 = \ ident_ rhs_ ->
                                     {-# LINE 1170 "./src-ag/Transform.ag" #-}
                                     [ (ident_, rhs_) ]
                                     {-# LINE 5725 "dist/build/Transform.hs"#-}
   {-# INLINE rule725 #-}
   rule725 = \  (_ :: ()) ->
     []
   {-# INLINE rule726 #-}
   rule726 = \  (_ :: ()) ->
     []
   {-# INLINE rule727 #-}
   rule727 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule728 #-}
   rule728 = \  (_ :: ()) ->
     []
   {-# INLINE rule729 #-}
   rule729 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule730 #-}
   rule730 = \  (_ :: ()) ->
     []
   {-# INLINE rule731 #-}
   rule731 = \  (_ :: ()) ->
     []
   {-# INLINE rule732 #-}
   rule732 = \  (_ :: ()) ->
     []
   {-# INLINE rule733 #-}
   rule733 = \  (_ :: ()) ->
     []
{-# NOINLINE sem_SemDef_AroundDef #-}
sem_SemDef_AroundDef :: (Identifier) -> (Expression) -> T_SemDef 
sem_SemDef_AroundDef arg_ident_ arg_rhs_ = T_SemDef (return st44) where
   {-# NOINLINE st44 #-}
   st44 = let
      v43 :: T_SemDef_v43 
      v43 = \ (T_SemDef_vIn43 _lhsIoptions) -> ( let
         _lhsOaroundInfos :: [AroundInfo]
         _lhsOaroundInfos = rule734 arg_ident_ arg_rhs_
         _lhsOaugmentInfos :: [AugmentInfo]
         _lhsOaugmentInfos = rule735  ()
         _lhsOdefinedInsts :: [Identifier]
         _lhsOdefinedInsts = rule736  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule737  ()
         _lhsOmergeInfos :: [MergeInfo]
         _lhsOmergeInfos = rule738  ()
         _lhsOorderDepsCollect :: Set Dependency
         _lhsOorderDepsCollect = rule739  ()
         _lhsOpragmaNamesCollect :: [Identifier]
         _lhsOpragmaNamesCollect = rule740  ()
         _lhsOruleInfos :: [RuleInfo]
         _lhsOruleInfos = rule741  ()
         _lhsOsigInfos :: [SigInfo]
         _lhsOsigInfos = rule742  ()
         _lhsOuniqueInfos :: [UniqueInfo]
         _lhsOuniqueInfos = rule743  ()
         __result_ = T_SemDef_vOut43 _lhsOaroundInfos _lhsOaugmentInfos _lhsOdefinedInsts _lhsOerrors _lhsOmergeInfos _lhsOorderDepsCollect _lhsOpragmaNamesCollect _lhsOruleInfos _lhsOsigInfos _lhsOuniqueInfos
         in __result_ )
     in C_SemDef_s44 v43
   {-# INLINE rule734 #-}
   {-# LINE 1173 "./src-ag/Transform.ag" #-}
   rule734 = \ ident_ rhs_ ->
                                    {-# LINE 1173 "./src-ag/Transform.ag" #-}
                                    [ (ident_, rhs_) ]
                                    {-# LINE 5788 "dist/build/Transform.hs"#-}
   {-# INLINE rule735 #-}
   rule735 = \  (_ :: ()) ->
     []
   {-# INLINE rule736 #-}
   rule736 = \  (_ :: ()) ->
     []
   {-# INLINE rule737 #-}
   rule737 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule738 #-}
   rule738 = \  (_ :: ()) ->
     []
   {-# INLINE rule739 #-}
   rule739 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule740 #-}
   rule740 = \  (_ :: ()) ->
     []
   {-# INLINE rule741 #-}
   rule741 = \  (_ :: ()) ->
     []
   {-# INLINE rule742 #-}
   rule742 = \  (_ :: ()) ->
     []
   {-# INLINE rule743 #-}
   rule743 = \  (_ :: ()) ->
     []
{-# NOINLINE sem_SemDef_MergeDef #-}
sem_SemDef_MergeDef :: (Identifier) -> (Identifier) -> ([Identifier]) -> (Expression) -> T_SemDef 
sem_SemDef_MergeDef arg_target_ arg_nt_ arg_sources_ arg_rhs_ = T_SemDef (return st44) where
   {-# NOINLINE st44 #-}
   st44 = let
      v43 :: T_SemDef_v43 
      v43 = \ (T_SemDef_vIn43 _lhsIoptions) -> ( let
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule744 _lhsIoptions arg_rhs_
         _lhsOmergeInfos :: [MergeInfo]
         _lhsOmergeInfos = rule745 arg_nt_ arg_rhs_ arg_sources_ arg_target_
         _lhsOaroundInfos :: [AroundInfo]
         _lhsOaroundInfos = rule746  ()
         _lhsOaugmentInfos :: [AugmentInfo]
         _lhsOaugmentInfos = rule747  ()
         _lhsOdefinedInsts :: [Identifier]
         _lhsOdefinedInsts = rule748  ()
         _lhsOorderDepsCollect :: Set Dependency
         _lhsOorderDepsCollect = rule749  ()
         _lhsOpragmaNamesCollect :: [Identifier]
         _lhsOpragmaNamesCollect = rule750  ()
         _lhsOruleInfos :: [RuleInfo]
         _lhsOruleInfos = rule751  ()
         _lhsOsigInfos :: [SigInfo]
         _lhsOsigInfos = rule752  ()
         _lhsOuniqueInfos :: [UniqueInfo]
         _lhsOuniqueInfos = rule753  ()
         __result_ = T_SemDef_vOut43 _lhsOaroundInfos _lhsOaugmentInfos _lhsOdefinedInsts _lhsOerrors _lhsOmergeInfos _lhsOorderDepsCollect _lhsOpragmaNamesCollect _lhsOruleInfos _lhsOsigInfos _lhsOuniqueInfos
         in __result_ )
     in C_SemDef_s44 v43
   {-# INLINE rule744 #-}
   {-# LINE 556 "./src-ag/Transform.ag" #-}
   rule744 = \ ((_lhsIoptions) :: Options) rhs_ ->
                 {-# LINE 556 "./src-ag/Transform.ag" #-}
                 if checkParseRhs _lhsIoptions
                 then Seq.fromList $ checkRhs rhs_
                 else Seq.empty
                 {-# LINE 5853 "dist/build/Transform.hs"#-}
   {-# INLINE rule745 #-}
   {-# LINE 1176 "./src-ag/Transform.ag" #-}
   rule745 = \ nt_ rhs_ sources_ target_ ->
                                   {-# LINE 1176 "./src-ag/Transform.ag" #-}
                                   [ (target_, nt_, sources_, rhs_) ]
                                   {-# LINE 5859 "dist/build/Transform.hs"#-}
   {-# INLINE rule746 #-}
   rule746 = \  (_ :: ()) ->
     []
   {-# INLINE rule747 #-}
   rule747 = \  (_ :: ()) ->
     []
   {-# INLINE rule748 #-}
   rule748 = \  (_ :: ()) ->
     []
   {-# INLINE rule749 #-}
   rule749 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule750 #-}
   rule750 = \  (_ :: ()) ->
     []
   {-# INLINE rule751 #-}
   rule751 = \  (_ :: ()) ->
     []
   {-# INLINE rule752 #-}
   rule752 = \  (_ :: ()) ->
     []
   {-# INLINE rule753 #-}
   rule753 = \  (_ :: ()) ->
     []
{-# NOINLINE sem_SemDef_SemPragma #-}
sem_SemDef_SemPragma :: ([NontermIdent]) -> T_SemDef 
sem_SemDef_SemPragma arg_names_ = T_SemDef (return st44) where
   {-# NOINLINE st44 #-}
   st44 = let
      v43 :: T_SemDef_v43 
      v43 = \ (T_SemDef_vIn43 _lhsIoptions) -> ( let
         _lhsOpragmaNamesCollect :: [Identifier]
         _lhsOpragmaNamesCollect = rule754 arg_names_
         _lhsOaroundInfos :: [AroundInfo]
         _lhsOaroundInfos = rule755  ()
         _lhsOaugmentInfos :: [AugmentInfo]
         _lhsOaugmentInfos = rule756  ()
         _lhsOdefinedInsts :: [Identifier]
         _lhsOdefinedInsts = rule757  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule758  ()
         _lhsOmergeInfos :: [MergeInfo]
         _lhsOmergeInfos = rule759  ()
         _lhsOorderDepsCollect :: Set Dependency
         _lhsOorderDepsCollect = rule760  ()
         _lhsOruleInfos :: [RuleInfo]
         _lhsOruleInfos = rule761  ()
         _lhsOsigInfos :: [SigInfo]
         _lhsOsigInfos = rule762  ()
         _lhsOuniqueInfos :: [UniqueInfo]
         _lhsOuniqueInfos = rule763  ()
         __result_ = T_SemDef_vOut43 _lhsOaroundInfos _lhsOaugmentInfos _lhsOdefinedInsts _lhsOerrors _lhsOmergeInfos _lhsOorderDepsCollect _lhsOpragmaNamesCollect _lhsOruleInfos _lhsOsigInfos _lhsOuniqueInfos
         in __result_ )
     in C_SemDef_s44 v43
   {-# INLINE rule754 #-}
   {-# LINE 897 "./src-ag/Transform.ag" #-}
   rule754 = \ names_ ->
                                 {-# LINE 897 "./src-ag/Transform.ag" #-}
                                 names_
                                 {-# LINE 5919 "dist/build/Transform.hs"#-}
   {-# INLINE rule755 #-}
   rule755 = \  (_ :: ()) ->
     []
   {-# INLINE rule756 #-}
   rule756 = \  (_ :: ()) ->
     []
   {-# INLINE rule757 #-}
   rule757 = \  (_ :: ()) ->
     []
   {-# INLINE rule758 #-}
   rule758 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule759 #-}
   rule759 = \  (_ :: ()) ->
     []
   {-# INLINE rule760 #-}
   rule760 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule761 #-}
   rule761 = \  (_ :: ()) ->
     []
   {-# INLINE rule762 #-}
   rule762 = \  (_ :: ()) ->
     []
   {-# INLINE rule763 #-}
   rule763 = \  (_ :: ()) ->
     []
{-# NOINLINE sem_SemDef_AttrOrderBefore #-}
sem_SemDef_AttrOrderBefore :: ([Occurrence]) -> ([Occurrence]) -> T_SemDef 
sem_SemDef_AttrOrderBefore arg_before_ arg_after_ = T_SemDef (return st44) where
   {-# NOINLINE st44 #-}
   st44 = let
      v43 :: T_SemDef_v43 
      v43 = \ (T_SemDef_vIn43 _lhsIoptions) -> ( let
         _dependency = rule764 arg_after_ arg_before_
         _lhsOorderDepsCollect :: Set Dependency
         _lhsOorderDepsCollect = rule765 _dependency
         _lhsOaroundInfos :: [AroundInfo]
         _lhsOaroundInfos = rule766  ()
         _lhsOaugmentInfos :: [AugmentInfo]
         _lhsOaugmentInfos = rule767  ()
         _lhsOdefinedInsts :: [Identifier]
         _lhsOdefinedInsts = rule768  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule769  ()
         _lhsOmergeInfos :: [MergeInfo]
         _lhsOmergeInfos = rule770  ()
         _lhsOpragmaNamesCollect :: [Identifier]
         _lhsOpragmaNamesCollect = rule771  ()
         _lhsOruleInfos :: [RuleInfo]
         _lhsOruleInfos = rule772  ()
         _lhsOsigInfos :: [SigInfo]
         _lhsOsigInfos = rule773  ()
         _lhsOuniqueInfos :: [UniqueInfo]
         _lhsOuniqueInfos = rule774  ()
         __result_ = T_SemDef_vOut43 _lhsOaroundInfos _lhsOaugmentInfos _lhsOdefinedInsts _lhsOerrors _lhsOmergeInfos _lhsOorderDepsCollect _lhsOpragmaNamesCollect _lhsOruleInfos _lhsOsigInfos _lhsOuniqueInfos
         in __result_ )
     in C_SemDef_s44 v43
   {-# INLINE rule764 #-}
   {-# LINE 928 "./src-ag/Transform.ag" #-}
   rule764 = \ after_ before_ ->
                               {-# LINE 928 "./src-ag/Transform.ag" #-}
                               [ Dependency b a | b <- before_, a <- after_ ]
                               {-# LINE 5983 "dist/build/Transform.hs"#-}
   {-# INLINE rule765 #-}
   {-# LINE 929 "./src-ag/Transform.ag" #-}
   rule765 = \ _dependency ->
                               {-# LINE 929 "./src-ag/Transform.ag" #-}
                               Set.fromList _dependency
                               {-# LINE 5989 "dist/build/Transform.hs"#-}
   {-# INLINE rule766 #-}
   rule766 = \  (_ :: ()) ->
     []
   {-# INLINE rule767 #-}
   rule767 = \  (_ :: ()) ->
     []
   {-# INLINE rule768 #-}
   rule768 = \  (_ :: ()) ->
     []
   {-# INLINE rule769 #-}
   rule769 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule770 #-}
   rule770 = \  (_ :: ()) ->
     []
   {-# INLINE rule771 #-}
   rule771 = \  (_ :: ()) ->
     []
   {-# INLINE rule772 #-}
   rule772 = \  (_ :: ()) ->
     []
   {-# INLINE rule773 #-}
   rule773 = \  (_ :: ()) ->
     []
   {-# INLINE rule774 #-}
   rule774 = \  (_ :: ()) ->
     []

-- SemDefs -----------------------------------------------------
-- wrapper
data Inh_SemDefs  = Inh_SemDefs { options_Inh_SemDefs :: (Options) }
data Syn_SemDefs  = Syn_SemDefs { aroundInfos_Syn_SemDefs :: ([AroundInfo]), augmentInfos_Syn_SemDefs :: ([AugmentInfo]), definedInsts_Syn_SemDefs :: ([Identifier]), errors_Syn_SemDefs :: (Seq Error), mergeInfos_Syn_SemDefs :: ([MergeInfo]), orderDepsCollect_Syn_SemDefs :: (Set Dependency), pragmaNamesCollect_Syn_SemDefs :: ([Identifier]), ruleInfos_Syn_SemDefs :: ([RuleInfo]), sigInfos_Syn_SemDefs :: ([SigInfo]), uniqueInfos_Syn_SemDefs :: ([UniqueInfo]) }
{-# INLINABLE wrap_SemDefs #-}
wrap_SemDefs :: T_SemDefs  -> Inh_SemDefs  -> (Syn_SemDefs )
wrap_SemDefs (T_SemDefs act) (Inh_SemDefs _lhsIoptions) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_SemDefs_vIn46 _lhsIoptions
        (T_SemDefs_vOut46 _lhsOaroundInfos _lhsOaugmentInfos _lhsOdefinedInsts _lhsOerrors _lhsOmergeInfos _lhsOorderDepsCollect _lhsOpragmaNamesCollect _lhsOruleInfos _lhsOsigInfos _lhsOuniqueInfos) <- return (inv_SemDefs_s47 sem arg)
        return (Syn_SemDefs _lhsOaroundInfos _lhsOaugmentInfos _lhsOdefinedInsts _lhsOerrors _lhsOmergeInfos _lhsOorderDepsCollect _lhsOpragmaNamesCollect _lhsOruleInfos _lhsOsigInfos _lhsOuniqueInfos)
   )

-- cata
{-# NOINLINE sem_SemDefs #-}
sem_SemDefs :: SemDefs  -> T_SemDefs 
sem_SemDefs list = Prelude.foldr sem_SemDefs_Cons sem_SemDefs_Nil (Prelude.map sem_SemDef list)

-- semantic domain
newtype T_SemDefs  = T_SemDefs {
                               attach_T_SemDefs :: Identity (T_SemDefs_s47 )
                               }
newtype T_SemDefs_s47  = C_SemDefs_s47 {
                                       inv_SemDefs_s47 :: (T_SemDefs_v46 )
                                       }
data T_SemDefs_s48  = C_SemDefs_s48
type T_SemDefs_v46  = (T_SemDefs_vIn46 ) -> (T_SemDefs_vOut46 )
data T_SemDefs_vIn46  = T_SemDefs_vIn46 (Options)
data T_SemDefs_vOut46  = T_SemDefs_vOut46 ([AroundInfo]) ([AugmentInfo]) ([Identifier]) (Seq Error) ([MergeInfo]) (Set Dependency) ([Identifier]) ([RuleInfo]) ([SigInfo]) ([UniqueInfo])
{-# NOINLINE sem_SemDefs_Cons #-}
sem_SemDefs_Cons :: T_SemDef  -> T_SemDefs  -> T_SemDefs 
sem_SemDefs_Cons arg_hd_ arg_tl_ = T_SemDefs (return st47) where
   {-# NOINLINE st47 #-}
   st47 = let
      v46 :: T_SemDefs_v46 
      v46 = \ (T_SemDefs_vIn46 _lhsIoptions) -> ( let
         _hdX44 = Control.Monad.Identity.runIdentity (attach_T_SemDef (arg_hd_))
         _tlX47 = Control.Monad.Identity.runIdentity (attach_T_SemDefs (arg_tl_))
         (T_SemDef_vOut43 _hdIaroundInfos _hdIaugmentInfos _hdIdefinedInsts _hdIerrors _hdImergeInfos _hdIorderDepsCollect _hdIpragmaNamesCollect _hdIruleInfos _hdIsigInfos _hdIuniqueInfos) = inv_SemDef_s44 _hdX44 (T_SemDef_vIn43 _hdOoptions)
         (T_SemDefs_vOut46 _tlIaroundInfos _tlIaugmentInfos _tlIdefinedInsts _tlIerrors _tlImergeInfos _tlIorderDepsCollect _tlIpragmaNamesCollect _tlIruleInfos _tlIsigInfos _tlIuniqueInfos) = inv_SemDefs_s47 _tlX47 (T_SemDefs_vIn46 _tlOoptions)
         _lhsOaroundInfos :: [AroundInfo]
         _lhsOaroundInfos = rule775 _hdIaroundInfos _tlIaroundInfos
         _lhsOaugmentInfos :: [AugmentInfo]
         _lhsOaugmentInfos = rule776 _hdIaugmentInfos _tlIaugmentInfos
         _lhsOdefinedInsts :: [Identifier]
         _lhsOdefinedInsts = rule777 _hdIdefinedInsts _tlIdefinedInsts
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule778 _hdIerrors _tlIerrors
         _lhsOmergeInfos :: [MergeInfo]
         _lhsOmergeInfos = rule779 _hdImergeInfos _tlImergeInfos
         _lhsOorderDepsCollect :: Set Dependency
         _lhsOorderDepsCollect = rule780 _hdIorderDepsCollect _tlIorderDepsCollect
         _lhsOpragmaNamesCollect :: [Identifier]
         _lhsOpragmaNamesCollect = rule781 _hdIpragmaNamesCollect _tlIpragmaNamesCollect
         _lhsOruleInfos :: [RuleInfo]
         _lhsOruleInfos = rule782 _hdIruleInfos _tlIruleInfos
         _lhsOsigInfos :: [SigInfo]
         _lhsOsigInfos = rule783 _hdIsigInfos _tlIsigInfos
         _lhsOuniqueInfos :: [UniqueInfo]
         _lhsOuniqueInfos = rule784 _hdIuniqueInfos _tlIuniqueInfos
         _hdOoptions = rule785 _lhsIoptions
         _tlOoptions = rule786 _lhsIoptions
         __result_ = T_SemDefs_vOut46 _lhsOaroundInfos _lhsOaugmentInfos _lhsOdefinedInsts _lhsOerrors _lhsOmergeInfos _lhsOorderDepsCollect _lhsOpragmaNamesCollect _lhsOruleInfos _lhsOsigInfos _lhsOuniqueInfos
         in __result_ )
     in C_SemDefs_s47 v46
   {-# INLINE rule775 #-}
   rule775 = \ ((_hdIaroundInfos) :: [AroundInfo]) ((_tlIaroundInfos) :: [AroundInfo]) ->
     _hdIaroundInfos ++ _tlIaroundInfos
   {-# INLINE rule776 #-}
   rule776 = \ ((_hdIaugmentInfos) :: [AugmentInfo]) ((_tlIaugmentInfos) :: [AugmentInfo]) ->
     _hdIaugmentInfos ++ _tlIaugmentInfos
   {-# INLINE rule777 #-}
   rule777 = \ ((_hdIdefinedInsts) :: [Identifier]) ((_tlIdefinedInsts) :: [Identifier]) ->
     _hdIdefinedInsts ++ _tlIdefinedInsts
   {-# INLINE rule778 #-}
   rule778 = \ ((_hdIerrors) :: Seq Error) ((_tlIerrors) :: Seq Error) ->
     _hdIerrors Seq.>< _tlIerrors
   {-# INLINE rule779 #-}
   rule779 = \ ((_hdImergeInfos) :: [MergeInfo]) ((_tlImergeInfos) :: [MergeInfo]) ->
     _hdImergeInfos ++ _tlImergeInfos
   {-# INLINE rule780 #-}
   rule780 = \ ((_hdIorderDepsCollect) :: Set Dependency) ((_tlIorderDepsCollect) :: Set Dependency) ->
     _hdIorderDepsCollect `Set.union` _tlIorderDepsCollect
   {-# INLINE rule781 #-}
   rule781 = \ ((_hdIpragmaNamesCollect) :: [Identifier]) ((_tlIpragmaNamesCollect) :: [Identifier]) ->
     _hdIpragmaNamesCollect ++ _tlIpragmaNamesCollect
   {-# INLINE rule782 #-}
   rule782 = \ ((_hdIruleInfos) :: [RuleInfo]) ((_tlIruleInfos) :: [RuleInfo]) ->
     _hdIruleInfos ++ _tlIruleInfos
   {-# INLINE rule783 #-}
   rule783 = \ ((_hdIsigInfos) :: [SigInfo]) ((_tlIsigInfos) :: [SigInfo]) ->
     _hdIsigInfos ++ _tlIsigInfos
   {-# INLINE rule784 #-}
   rule784 = \ ((_hdIuniqueInfos) :: [UniqueInfo]) ((_tlIuniqueInfos) :: [UniqueInfo]) ->
     _hdIuniqueInfos ++ _tlIuniqueInfos
   {-# INLINE rule785 #-}
   rule785 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule786 #-}
   rule786 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_SemDefs_Nil #-}
sem_SemDefs_Nil ::  T_SemDefs 
sem_SemDefs_Nil  = T_SemDefs (return st47) where
   {-# NOINLINE st47 #-}
   st47 = let
      v46 :: T_SemDefs_v46 
      v46 = \ (T_SemDefs_vIn46 _lhsIoptions) -> ( let
         _lhsOaroundInfos :: [AroundInfo]
         _lhsOaroundInfos = rule787  ()
         _lhsOaugmentInfos :: [AugmentInfo]
         _lhsOaugmentInfos = rule788  ()
         _lhsOdefinedInsts :: [Identifier]
         _lhsOdefinedInsts = rule789  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule790  ()
         _lhsOmergeInfos :: [MergeInfo]
         _lhsOmergeInfos = rule791  ()
         _lhsOorderDepsCollect :: Set Dependency
         _lhsOorderDepsCollect = rule792  ()
         _lhsOpragmaNamesCollect :: [Identifier]
         _lhsOpragmaNamesCollect = rule793  ()
         _lhsOruleInfos :: [RuleInfo]
         _lhsOruleInfos = rule794  ()
         _lhsOsigInfos :: [SigInfo]
         _lhsOsigInfos = rule795  ()
         _lhsOuniqueInfos :: [UniqueInfo]
         _lhsOuniqueInfos = rule796  ()
         __result_ = T_SemDefs_vOut46 _lhsOaroundInfos _lhsOaugmentInfos _lhsOdefinedInsts _lhsOerrors _lhsOmergeInfos _lhsOorderDepsCollect _lhsOpragmaNamesCollect _lhsOruleInfos _lhsOsigInfos _lhsOuniqueInfos
         in __result_ )
     in C_SemDefs_s47 v46
   {-# INLINE rule787 #-}
   rule787 = \  (_ :: ()) ->
     []
   {-# INLINE rule788 #-}
   rule788 = \  (_ :: ()) ->
     []
   {-# INLINE rule789 #-}
   rule789 = \  (_ :: ()) ->
     []
   {-# INLINE rule790 #-}
   rule790 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule791 #-}
   rule791 = \  (_ :: ()) ->
     []
   {-# INLINE rule792 #-}
   rule792 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule793 #-}
   rule793 = \  (_ :: ()) ->
     []
   {-# INLINE rule794 #-}
   rule794 = \  (_ :: ()) ->
     []
   {-# INLINE rule795 #-}
   rule795 = \  (_ :: ()) ->
     []
   {-# INLINE rule796 #-}
   rule796 = \  (_ :: ()) ->
     []
