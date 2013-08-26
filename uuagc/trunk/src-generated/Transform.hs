{-# LANGUAGE Rank2Types, GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Transform where
{-# LINE 2 "./src-ag/Patterns.ag" #-}

-- Patterns.ag imports
import UU.Scanner.Position(Pos)
import CommonTypes (ConstructorIdent,Identifier)
{-# LINE 11 "dist/build/Transform.hs" #-}

{-# LINE 2 "./src-ag/ConcreteSyntax.ag" #-}

import UU.Scanner.Position (Pos)
import Patterns   (Pattern)
import Expression (Expression)
import CommonTypes
import Macro --marcos
{-# LINE 20 "dist/build/Transform.hs" #-}

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

{-# LINE 874 "./src-ag/Transform.ag" #-}

extract :: String -> [String]
extract s = case dropWhile isSeparator s of
                                "" -> []
                                s' -> w : extract s''
                                      where (w, s'') = break isSeparator  s'
isSeparator :: Char -> Bool
isSeparator x = x == '_'
{-# LINE 349 "dist/build/Transform.hs" #-}

{-# LINE 900 "./src-ag/Transform.ag" #-}

pragmaMapUnion :: PragmaMap -> PragmaMap -> PragmaMap
pragmaMapUnion = Map.unionWith (Map.unionWith Set.union)

pragmaMapSingle :: NontermIdent -> ConstructorIdent -> Set Identifier -> PragmaMap
pragmaMapSingle nt con nms = Map.singleton nt (Map.singleton con nms)
{-# LINE 358 "dist/build/Transform.hs" #-}

{-# LINE 932 "./src-ag/Transform.ag" #-}

orderMapUnion :: AttrOrderMap -> AttrOrderMap -> AttrOrderMap
orderMapUnion = Map.unionWith (Map.unionWith Set.union)

orderMapSingle :: NontermIdent -> ConstructorIdent -> Set Dependency -> AttrOrderMap
orderMapSingle nt con deps = Map.singleton nt (Map.singleton con deps)
{-# LINE 367 "dist/build/Transform.hs" #-}

{-# LINE 958 "./src-ag/Transform.ag" #-}

mergeParams :: ParamMap -> ParamMap -> ParamMap
mergeParams = Map.unionWith (++)
{-# LINE 373 "dist/build/Transform.hs" #-}

{-# LINE 981 "./src-ag/Transform.ag" #-}

mergeCtx :: ContextMap -> ContextMap -> ContextMap
mergeCtx
  = Map.unionWith nubconcat
  where nubconcat a b = nub (a ++ b)
{-# LINE 381 "dist/build/Transform.hs" #-}

{-# LINE 1000 "./src-ag/Transform.ag" #-}

mergeQuant :: QuantMap -> QuantMap -> QuantMap
mergeQuant = Map.unionWith (++)
{-# LINE 387 "dist/build/Transform.hs" #-}

{-# LINE 1011 "./src-ag/Transform.ag" #-}

mergeDerivings :: Derivings -> Derivings -> Derivings
mergeDerivings m1 m2 = foldr (\(n,cs) m -> Map.insertWith Set.union n cs m) m2 (Map.toList m1)
{-# LINE 393 "dist/build/Transform.hs" #-}

{-# LINE 1023 "./src-ag/Transform.ag" #-}

merge ::(Ord k, Ord k1) => Map k (Map k1 a) -> Map k (Map k1 a) -> Map k (Map k1 a)
merge x y = foldr f y (Map.toList x)
 where f ~(k,v) m = Map.insertWith (Map.union) k v m
{-# LINE 400 "dist/build/Transform.hs" #-}

{-# LINE 1066 "./src-ag/Transform.ag" #-}

checkAttrs :: DataTypes -> [NontermIdent] -> [(Identifier, a)] -> [(Identifier, b)] -> Map NontermIdent (Map Identifier a, Map Identifier b) -> (Map NontermIdent (Map Identifier a, Map Identifier b), Seq Error)
checkAttrs allFields nts inherited synthesized decls' = foldErrors check decls' nts where
  check nt decls | not (nt `Map.member` allFields) = (decls,Seq.singleton(UndefNont nt))
                 | otherwise = let (inh,syn) = Map.findWithDefault (Map.empty,Map.empty) nt decls
                                   (inh',einh) = checkDuplicates (DupInhAttr nt) inherited   inh
                                   (syn',esyn) = checkDuplicates (DupSynAttr nt) synthesized syn
                               in (Map.insert nt (inh',syn') decls,einh >< esyn)
{-# LINE 411 "dist/build/Transform.hs" #-}

{-# LINE 1078 "./src-ag/Transform.ag" #-}

addSelf :: Ord k1 => k1 -> Map k1 (Map k a, Attributes) -> Map k1 (Map k a, Attributes)
addSelf name atMap = let (eInh,eSyn) = Map.findWithDefault(Map.empty,Map.empty) name atMap
                     in  Map.insert name (eInh, Map.insert (Ident "self" noPos) Self eSyn)atMap
{-# LINE 418 "dist/build/Transform.hs" #-}

{-# LINE 1216 "./src-ag/Transform.ag" #-}

-- We want the last Just in the list
flipmplus = flip mplus
{-# LINE 424 "dist/build/Transform.hs" #-}

{-# LINE 1224 "./src-ag/Transform.ag" #-}

makeType :: Set NontermIdent -> Type -> Type
makeType nts tp@(NT x _ _)   | Set.member x nts = tp
                             | otherwise        = Haskell (typeToHaskellString Nothing [] tp)
makeType _   tp                                 = tp
{-# LINE 432 "dist/build/Transform.hs" #-}

{-# LINE 1230 "./src-ag/Transform.ag" #-}

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
{-# LINE 504 "dist/build/Transform.hs" #-}

{-# LINE 1301 "./src-ag/Transform.ag" #-}

mapUnionWithSetUnion :: Map NontermIdent (Set ConstructorIdent) -> Map NontermIdent (Set ConstructorIdent) -> Map NontermIdent (Set ConstructorIdent)
mapUnionWithSetUnion = Map.unionWith Set.union
mapUnionWithPlusPlus :: Map BlockInfo [a] -> Map BlockInfo [a] -> Map BlockInfo [a]
mapUnionWithPlusPlus = Map.unionWith (++)
{-# LINE 512 "dist/build/Transform.hs" #-}
-- AG ----------------------------------------------------------
-- wrapper
data Inh_AG  = Inh_AG { options_Inh_AG :: (Options) }
data Syn_AG  = Syn_AG { agi_Syn_AG :: ((Set NontermIdent, DataTypes, Map NontermIdent (Attributes, Attributes))), blocks_Syn_AG :: (Blocks), constructorTypeMap_Syn_AG :: (Map NontermIdent ConstructorType), errors_Syn_AG :: (Seq Error), moduleDecl_Syn_AG :: (Maybe (String,String,String)), output_Syn_AG :: (Grammar), pragmas_Syn_AG :: (Options -> Options) }
{-# INLINABLE wrap_AG #-}
wrap_AG :: T_AG  -> Inh_AG  -> (Syn_AG )
wrap_AG (T_AG act) (Inh_AG _lhsIoptions) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_AG_vIn1 _lhsIoptions
        (T_AG_vOut1 _lhsOagi _lhsOblocks _lhsOconstructorTypeMap _lhsOerrors _lhsOmoduleDecl _lhsOoutput _lhsOpragmas) <- return (inv_AG_s2 sem arg)
        return (Syn_AG _lhsOagi _lhsOblocks _lhsOconstructorTypeMap _lhsOerrors _lhsOmoduleDecl _lhsOoutput _lhsOpragmas)
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
data T_AG_vOut1  = T_AG_vOut1 ((Set NontermIdent, DataTypes, Map NontermIdent (Attributes, Attributes))) (Blocks) (Map NontermIdent ConstructorType) (Seq Error) (Maybe (String,String,String)) (Grammar) (Options -> Options)
{-# NOINLINE sem_AG_AG #-}
sem_AG_AG :: T_Elems  -> T_AG 
sem_AG_AG arg_elems_ = T_AG (return st2) where
   {-# NOINLINE st2 #-}
   st2 = let
      v1 :: T_AG_v1 
      v1 = \ (T_AG_vIn1 _lhsIoptions) -> ( let
         _elemsX20 = Control.Monad.Identity.runIdentity (attach_T_Elems (arg_elems_))
         (T_Elems_vOut19 _elemsIattrDecls _elemsIattrOrderCollect _elemsIattrs _elemsIblocks _elemsIcollectedArounds _elemsIcollectedAugments _elemsIcollectedConParams _elemsIcollectedConstraints _elemsIcollectedConstructorsMap _elemsIcollectedFields _elemsIcollectedInsts _elemsIcollectedMacros _elemsIcollectedMerges _elemsIcollectedNames _elemsIcollectedRules _elemsIcollectedSetNames _elemsIcollectedSigs _elemsIcollectedUniques _elemsIconstructorTypeMap _elemsIctxCollect _elemsIdefSets _elemsIderivings _elemsIerrors _elemsImoduleDecl _elemsIparamsCollect _elemsIpragmas _elemsIquantCollect _elemsIsemPragmasCollect _elemsItypeSyns _elemsIuseMap _elemsIwrappers) = inv_Elems_s20 _elemsX20 (T_Elems_vIn19 _elemsOallAttrDecls _elemsOallAttrs _elemsOallConstructors _elemsOallFields _elemsOallNonterminals _elemsOattrDecls _elemsOattrs _elemsOdefSets _elemsOdefinedSets _elemsOoptions)
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
         _lhsOconstructorTypeMap :: Map NontermIdent ConstructorType
         _lhsOconstructorTypeMap = rule53 _elemsIconstructorTypeMap
         _lhsOmoduleDecl :: Maybe (String,String,String)
         _lhsOmoduleDecl = rule54 _elemsImoduleDecl
         _lhsOpragmas :: Options -> Options
         _lhsOpragmas = rule55 _elemsIpragmas
         _elemsOallAttrDecls = rule56 _allAttrDecls
         _elemsOallAttrs = rule57 _allAttrs
         _elemsOallFields = rule58 _allFields
         _elemsOallNonterminals = rule59 _allNonterminals
         _elemsOoptions = rule60 _lhsIoptions
         __result_ = T_AG_vOut1 _lhsOagi _lhsOblocks _lhsOconstructorTypeMap _lhsOerrors _lhsOmoduleDecl _lhsOoutput _lhsOpragmas
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
                      {-# LINE 650 "dist/build/Transform.hs"#-}
   {-# INLINE rule1 #-}
   {-# LINE 258 "./src-ag/Transform.ag" #-}
   rule1 = \ ((_elemsIcollectedFields) :: [(NontermIdent, ConstructorIdent, FieldMap)]) ->
                             {-# LINE 258 "./src-ag/Transform.ag" #-}
                             let f (nt,con,_) = Map.insertWith g nt [con]
                                 g [con] lst | con `elem` lst = lst
                                             | otherwise      = con : lst
                                 g _ _ = error "This is not possible"
                             in  foldr f Map.empty _elemsIcollectedFields
                             {-# LINE 660 "dist/build/Transform.hs"#-}
   {-# INLINE rule2 #-}
   {-# LINE 263 "./src-ag/Transform.ag" #-}
   rule2 = \ ((_elemsIcollectedFields) :: [(NontermIdent, ConstructorIdent, FieldMap)]) ->
                             {-# LINE 263 "./src-ag/Transform.ag" #-}
                             let f (nt,con,fm) = Map.insertWith (Map.unionWith (++)) nt (Map.singleton con fm)
                             in  foldr f (Map.empty) _elemsIcollectedFields
                             {-# LINE 667 "dist/build/Transform.hs"#-}
   {-# INLINE rule3 #-}
   {-# LINE 266 "./src-ag/Transform.ag" #-}
   rule3 = \ ((_elemsIcollectedConstraints) :: [(NontermIdent, ConstructorIdent, [Type])]) ->
                                {-# LINE 266 "./src-ag/Transform.ag" #-}
                                let f (nt,con,fm) = Map.insertWith (Map.unionWith (++)) nt (Map.singleton con fm)
                                in  foldr f (Map.empty) _elemsIcollectedConstraints
                                {-# LINE 674 "dist/build/Transform.hs"#-}
   {-# INLINE rule4 #-}
   {-# LINE 269 "./src-ag/Transform.ag" #-}
   rule4 = \ ((_elemsIcollectedConParams) :: [(NontermIdent, ConstructorIdent, Set Identifier)]) ->
                                {-# LINE 269 "./src-ag/Transform.ag" #-}
                                let f (nt,con,fm) = Map.insertWith (Map.unionWith Set.union) nt (Map.singleton con fm)
                                in  foldr f (Map.empty) _elemsIcollectedConParams
                                {-# LINE 681 "dist/build/Transform.hs"#-}
   {-# INLINE rule5 #-}
   {-# LINE 272 "./src-ag/Transform.ag" #-}
   rule5 = \ ((_elemsIcollectedFields) :: [(NontermIdent, ConstructorIdent, FieldMap)]) ->
                             {-# LINE 272 "./src-ag/Transform.ag" #-}
                             let f (nt,con,_) = Map.insertWith (++) nt [con]
                             in  foldr f (Map.empty) _elemsIcollectedFields
                             {-# LINE 688 "dist/build/Transform.hs"#-}
   {-# INLINE rule6 #-}
   {-# LINE 275 "./src-ag/Transform.ag" #-}
   rule6 = \ ((_elemsIcollectedRules) :: [ (NontermIdent, ConstructorIdent, RuleInfo)]) ->
                             {-# LINE 275 "./src-ag/Transform.ag" #-}
                             let f (nt,con,r) = Map.insertWith (Map.unionWith (++)) nt (Map.singleton con [r])
                             in  foldr f (Map.empty) _elemsIcollectedRules
                             {-# LINE 695 "dist/build/Transform.hs"#-}
   {-# INLINE rule7 #-}
   {-# LINE 278 "./src-ag/Transform.ag" #-}
   rule7 = \ _allAttrDecls ((_elemsIcollectedSigs) :: [ (NontermIdent, ConstructorIdent, SigInfo) ]) ((_elemsIcollectedUniques) :: [ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]) ->
                             {-# LINE 278 "./src-ag/Transform.ag" #-}
                             let f (nt,con,t) = Map.insertWith (Map.unionWith (++)) nt (Map.singleton con [t])
                                 typeof nt r = Map.findWithDefault (Haskell "<unknown>") r $ fst $ Map.findWithDefault (Map.empty,Map.empty) nt _allAttrDecls
                             in  foldr f (Map.empty) ( _elemsIcollectedSigs
                                                     ++ [ (nt, con, (ident,typeof nt ref))  | (nt, con, us) <- _elemsIcollectedUniques, (ident,ref) <- us ]
                                                     )
                             {-# LINE 705 "dist/build/Transform.hs"#-}
   {-# INLINE rule8 #-}
   {-# LINE 284 "./src-ag/Transform.ag" #-}
   rule8 = \ ((_elemsIcollectedInsts) :: [ (NontermIdent, ConstructorIdent, [Identifier]) ]) ->
                             {-# LINE 284 "./src-ag/Transform.ag" #-}
                             let f (nt,con,is) = Map.insertWith (Map.unionWith (++)) nt (Map.singleton con is)
                             in  foldr f (Map.empty) _elemsIcollectedInsts
                             {-# LINE 712 "dist/build/Transform.hs"#-}
   {-# INLINE rule9 #-}
   {-# LINE 287 "./src-ag/Transform.ag" #-}
   rule9 = \ ((_elemsIcollectedUniques) :: [ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]) ->
                             {-# LINE 287 "./src-ag/Transform.ag" #-}
                             let f (nt,con,us) = Map.insertWith (Map.unionWith (++)) nt (Map.singleton con us)
                             in foldr f (Map.empty) _elemsIcollectedUniques
                             {-# LINE 719 "dist/build/Transform.hs"#-}
   {-# INLINE rule10 #-}
   {-# LINE 289 "./src-ag/Transform.ag" #-}
   rule10 = \ ((_elemsIcollectedAugments) :: [ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]) ->
                             {-# LINE 289 "./src-ag/Transform.ag" #-}
                             let f (nt,con,as) = Map.insertWith (Map.unionWith (++)) nt (Map.singleton con as)
                             in foldr f Map.empty _elemsIcollectedAugments
                             {-# LINE 726 "dist/build/Transform.hs"#-}
   {-# INLINE rule11 #-}
   {-# LINE 291 "./src-ag/Transform.ag" #-}
   rule11 = \ ((_elemsIcollectedArounds) :: [ (NontermIdent, ConstructorIdent, [AroundInfo])  ]) ->
                              {-# LINE 291 "./src-ag/Transform.ag" #-}
                              let f (nt,con,as) = Map.insertWith (Map.unionWith (++)) nt (Map.singleton con as)
                              in foldr f Map.empty _elemsIcollectedArounds
                              {-# LINE 733 "dist/build/Transform.hs"#-}
   {-# INLINE rule12 #-}
   {-# LINE 293 "./src-ag/Transform.ag" #-}
   rule12 = \ ((_elemsIcollectedMerges) :: [ (NontermIdent, ConstructorIdent, [MergeInfo])   ]) ->
                              {-# LINE 293 "./src-ag/Transform.ag" #-}
                              let f (nt,con,as) = Map.insertWith (Map.unionWith (++)) nt (Map.singleton con as)
                               in foldr f Map.empty _elemsIcollectedMerges
                              {-# LINE 740 "dist/build/Transform.hs"#-}
   {-# INLINE rule13 #-}
   {-# LINE 296 "./src-ag/Transform.ag" #-}
   rule13 = \ _allAugments ->
                                {-# LINE 296 "./src-ag/Transform.ag" #-}
                                let gen _ = []
                                in Map.map (Map.map gen) _allAugments
                                {-# LINE 747 "dist/build/Transform.hs"#-}
   {-# INLINE rule14 #-}
   {-# LINE 299 "./src-ag/Transform.ag" #-}
   rule14 = \ _allAttrDecls _allFields _allInsts _allMerges _allRules _allSigs ->
                                {-# LINE 299 "./src-ag/Transform.ag" #-}
                                Map.mapWithKey (Map.mapWithKey . (checkRules _allAttrDecls _allFields _allInsts _allSigs     _allMerges    )) _allRules
                                {-# LINE 753 "dist/build/Transform.hs"#-}
   {-# INLINE rule15 #-}
   {-# LINE 300 "./src-ag/Transform.ag" #-}
   rule15 = \ _allRules ->
                                {-# LINE 300 "./src-ag/Transform.ag" #-}
                                Map.mapWithKey (Map.mapWithKey . checkRuleNames) _allRules
                                {-# LINE 759 "dist/build/Transform.hs"#-}
   {-# INLINE rule16 #-}
   {-# LINE 301 "./src-ag/Transform.ag" #-}
   rule16 = \ _allSigs ->
                                {-# LINE 301 "./src-ag/Transform.ag" #-}
                                Map.mapWithKey (Map.mapWithKey . (checkSigs                                                 )) _allSigs
                                {-# LINE 765 "dist/build/Transform.hs"#-}
   {-# INLINE rule17 #-}
   {-# LINE 302 "./src-ag/Transform.ag" #-}
   rule17 = \ _allFields _allInsts _allNonterminals _allSigs ->
                                {-# LINE 302 "./src-ag/Transform.ag" #-}
                                Map.mapWithKey (Map.mapWithKey . (checkInsts _allNonterminals     _allSigs     _allFields   )) _allInsts
                                {-# LINE 771 "dist/build/Transform.hs"#-}
   {-# INLINE rule18 #-}
   {-# LINE 303 "./src-ag/Transform.ag" #-}
   rule18 = \ _allAttrDecls _allUniques ->
                                {-# LINE 303 "./src-ag/Transform.ag" #-}
                                Map.mapWithKey (Map.mapWithKey . (checkUniques _allAttrDecls                                )) _allUniques
                                {-# LINE 777 "dist/build/Transform.hs"#-}
   {-# INLINE rule19 #-}
   {-# LINE 304 "./src-ag/Transform.ag" #-}
   rule19 = \ _allAttrDecls _allAugments ->
                                {-# LINE 304 "./src-ag/Transform.ag" #-}
                                Map.mapWithKey (Map.mapWithKey . (checkAugments _allAttrDecls                               )) _allAugments
                                {-# LINE 783 "dist/build/Transform.hs"#-}
   {-# INLINE rule20 #-}
   {-# LINE 305 "./src-ag/Transform.ag" #-}
   rule20 = \ _allArounds _allFields ->
                                {-# LINE 305 "./src-ag/Transform.ag" #-}
                                Map.mapWithKey (Map.mapWithKey . (checkArounds _allFields    )) _allArounds
                                {-# LINE 789 "dist/build/Transform.hs"#-}
   {-# INLINE rule21 #-}
   {-# LINE 306 "./src-ag/Transform.ag" #-}
   rule21 = \ _allFields _allInsts _allMerges _allNonterminals ->
                                {-# LINE 306 "./src-ag/Transform.ag" #-}
                                Map.mapWithKey (Map.mapWithKey . (checkMerges _allNonterminals     _allInsts     _allFields    )) _allMerges
                                {-# LINE 795 "dist/build/Transform.hs"#-}
   {-# INLINE rule22 #-}
   {-# LINE 308 "./src-ag/Transform.ag" #-}
   rule22 = \ _allRulesErrs ->
                                 {-# LINE 308 "./src-ag/Transform.ag" #-}
                                 Map.map (Map.map fst) _allRulesErrs
                                 {-# LINE 801 "dist/build/Transform.hs"#-}
   {-# INLINE rule23 #-}
   {-# LINE 309 "./src-ag/Transform.ag" #-}
   rule23 = \ _allSigsErrs _augmentSigs ->
                                 {-# LINE 309 "./src-ag/Transform.ag" #-}
                                 Map.map (Map.map fst) _allSigsErrs     `unionunionplusplus` _augmentSigs
                                 {-# LINE 807 "dist/build/Transform.hs"#-}
   {-# INLINE rule24 #-}
   {-# LINE 310 "./src-ag/Transform.ag" #-}
   rule24 = \ _allInstsErrs ->
                                 {-# LINE 310 "./src-ag/Transform.ag" #-}
                                 Map.map (Map.map fst) _allInstsErrs
                                 {-# LINE 813 "dist/build/Transform.hs"#-}
   {-# INLINE rule25 #-}
   {-# LINE 311 "./src-ag/Transform.ag" #-}
   rule25 = \ _allUniquesErrs ->
                                 {-# LINE 311 "./src-ag/Transform.ag" #-}
                                 Map.map (Map.map fst) _allUniquesErrs
                                 {-# LINE 819 "dist/build/Transform.hs"#-}
   {-# INLINE rule26 #-}
   {-# LINE 312 "./src-ag/Transform.ag" #-}
   rule26 = \ _allAugmentErrs ->
                                 {-# LINE 312 "./src-ag/Transform.ag" #-}
                                 Map.map (Map.map fst) _allAugmentErrs
                                 {-# LINE 825 "dist/build/Transform.hs"#-}
   {-# INLINE rule27 #-}
   {-# LINE 313 "./src-ag/Transform.ag" #-}
   rule27 = \ _allAroundsErrs ->
                                 {-# LINE 313 "./src-ag/Transform.ag" #-}
                                 Map.map (Map.map fst) _allAroundsErrs
                                 {-# LINE 831 "dist/build/Transform.hs"#-}
   {-# INLINE rule28 #-}
   {-# LINE 314 "./src-ag/Transform.ag" #-}
   rule28 = \ _allAttrDecls _allFields _allRules _checkedInsts _checkedRulesPre _checkedUniques ((_lhsIoptions) :: Options) ->
                                 {-# LINE 314 "./src-ag/Transform.ag" #-}
                                 Map.unionWith (Map.unionWith (++)) _checkedRulesPre     (Map.mapWithKey (Map.mapWithKey . (mkUniqueRules _lhsIoptions _allRules     _allFields     _checkedInsts     _allAttrDecls    )) _checkedUniques    )
                                 {-# LINE 837 "dist/build/Transform.hs"#-}
   {-# INLINE rule29 #-}
   {-# LINE 315 "./src-ag/Transform.ag" #-}
   rule29 = \ _allMergesErrs ->
                                 {-# LINE 315 "./src-ag/Transform.ag" #-}
                                 Map.map (Map.map fst) _allMergesErrs
                                 {-# LINE 843 "dist/build/Transform.hs"#-}
   {-# INLINE rule30 #-}
   {-# LINE 317 "./src-ag/Transform.ag" #-}
   rule30 = \ ((_elemsItypeSyns) :: TypeSyns) ->
                             {-# LINE 317 "./src-ag/Transform.ag" #-}
                             let f = checkForDuplicates (DupSynonym)
                             in  Seq.fromList . f . map fst $ _elemsItypeSyns
                             {-# LINE 850 "dist/build/Transform.hs"#-}
   {-# INLINE rule31 #-}
   {-# LINE 320 "./src-ag/Transform.ag" #-}
   rule31 = \ _allFields ->
                             {-# LINE 320 "./src-ag/Transform.ag" #-}
                             let g nt (con,fm) = checkForDuplicates (DupChild nt con) (map fst fm)
                                 f (nt,cfm)    = concat . map (g nt) . Map.toList $ cfm
                             in  Seq.fromList . concat . map f . Map.toList $ _allFields
                             {-# LINE 858 "dist/build/Transform.hs"#-}
   {-# INLINE rule32 #-}
   {-# LINE 324 "./src-ag/Transform.ag" #-}
   rule32 = \  (_ :: ()) ->
                             {-# LINE 324 "./src-ag/Transform.ag" #-}
                             let
                             in   Seq.empty
                             {-# LINE 865 "dist/build/Transform.hs"#-}
   {-# INLINE rule33 #-}
   {-# LINE 328 "./src-ag/Transform.ag" #-}
   rule33 = \ _allRulesErrs ->
                             {-# LINE 328 "./src-ag/Transform.ag" #-}
                             let  f m s = Map.fold ((><) . snd) s m
                             in Map.fold f Seq.empty _allRulesErrs
                             {-# LINE 872 "dist/build/Transform.hs"#-}
   {-# INLINE rule34 #-}
   {-# LINE 331 "./src-ag/Transform.ag" #-}
   rule34 = \ _allSigsErrs ->
                             {-# LINE 331 "./src-ag/Transform.ag" #-}
                             let  f m s = Map.fold ((><) . snd) s m
                             in Map.fold f Seq.empty _allSigsErrs
                             {-# LINE 879 "dist/build/Transform.hs"#-}
   {-# INLINE rule35 #-}
   {-# LINE 334 "./src-ag/Transform.ag" #-}
   rule35 = \ _allInstsErrs ->
                             {-# LINE 334 "./src-ag/Transform.ag" #-}
                             let  f m s = Map.fold ((><) . snd) s m
                             in Map.fold f Seq.empty _allInstsErrs
                             {-# LINE 886 "dist/build/Transform.hs"#-}
   {-# INLINE rule36 #-}
   {-# LINE 337 "./src-ag/Transform.ag" #-}
   rule36 = \ _allUniquesErrs ->
                             {-# LINE 337 "./src-ag/Transform.ag" #-}
                             let  f m s = Map.fold ((><) . snd) s m
                             in Map.fold f Seq.empty _allUniquesErrs
                             {-# LINE 893 "dist/build/Transform.hs"#-}
   {-# INLINE rule37 #-}
   {-# LINE 340 "./src-ag/Transform.ag" #-}
   rule37 = \ _allAugmentErrs ->
                             {-# LINE 340 "./src-ag/Transform.ag" #-}
                             let  f m s = Map.fold ((><) . snd) s m
                             in Map.fold f Seq.empty _allAugmentErrs
                             {-# LINE 900 "dist/build/Transform.hs"#-}
   {-# INLINE rule38 #-}
   {-# LINE 343 "./src-ag/Transform.ag" #-}
   rule38 = \ _allAroundsErrs ->
                             {-# LINE 343 "./src-ag/Transform.ag" #-}
                             let  f m s = Map.fold ((><) . snd) s m
                             in Map.fold f Seq.empty _allAroundsErrs
                             {-# LINE 907 "dist/build/Transform.hs"#-}
   {-# INLINE rule39 #-}
   {-# LINE 346 "./src-ag/Transform.ag" #-}
   rule39 = \ _allNamesErrs ->
                              {-# LINE 346 "./src-ag/Transform.ag" #-}
                              let  f m s = Map.fold ((><)) s m
                              in Map.fold f Seq.empty _allNamesErrs
                              {-# LINE 914 "dist/build/Transform.hs"#-}
   {-# INLINE rule40 #-}
   {-# LINE 349 "./src-ag/Transform.ag" #-}
   rule40 = \ _allMergesErrs ->
                              {-# LINE 349 "./src-ag/Transform.ag" #-}
                              let f m s = Map.fold ((><) . snd) s m
                              in Map.fold f Seq.empty _allMergesErrs
                              {-# LINE 921 "dist/build/Transform.hs"#-}
   {-# INLINE rule41 #-}
   {-# LINE 352 "./src-ag/Transform.ag" #-}
   rule41 = \ ((_elemsIerrors) :: Seq Error) _errs1 _errs10 _errs11 _errs2 _errs3 _errs4 _errs5 _errs6 _errs7 _errs8 _errs9 ->
                             {-# LINE 352 "./src-ag/Transform.ag" #-}
                             _elemsIerrors >< _errs1 >< _errs2 >< _errs3 >< _errs4 >< _errs5 >< _errs6 >< _errs7 >< _errs8 >< _errs9 >< _errs10 >< _errs11
                             {-# LINE 927 "dist/build/Transform.hs"#-}
   {-# INLINE rule42 #-}
   {-# LINE 606 "./src-ag/Transform.ag" #-}
   rule42 = \ ((_elemsIcollectedNames) :: Set Identifier) ((_elemsIcollectedSetNames) :: Set Identifier) ->
                                 {-# LINE 606 "./src-ag/Transform.ag" #-}
                                 _elemsIcollectedNames `Set.difference` _elemsIcollectedSetNames
                                 {-# LINE 933 "dist/build/Transform.hs"#-}
   {-# INLINE rule43 #-}
   {-# LINE 626 "./src-ag/Transform.ag" #-}
   rule43 = \ ((_elemsIcollectedConstructorsMap) :: Map NontermIdent (Set ConstructorIdent)) ->
                                 {-# LINE 626 "./src-ag/Transform.ag" #-}
                                 _elemsIcollectedConstructorsMap
                                 {-# LINE 939 "dist/build/Transform.hs"#-}
   {-# INLINE rule44 #-}
   {-# LINE 709 "./src-ag/Transform.ag" #-}
   rule44 = \ _allNonterminals ->
                             {-# LINE 709 "./src-ag/Transform.ag" #-}
                             Map.fromList (map (\x->(x,(Set.singleton x, Set.empty))) (Set.toList _allNonterminals    ))
                             {-# LINE 945 "dist/build/Transform.hs"#-}
   {-# INLINE rule45 #-}
   {-# LINE 710 "./src-ag/Transform.ag" #-}
   rule45 = \ ((_elemsIdefSets) :: Map Identifier (Set NontermIdent,Set Identifier)) ->
                             {-# LINE 710 "./src-ag/Transform.ag" #-}
                             Map.map fst _elemsIdefSets
                             {-# LINE 951 "dist/build/Transform.hs"#-}
   {-# INLINE rule46 #-}
   {-# LINE 1030 "./src-ag/Transform.ag" #-}
   rule46 = \  (_ :: ()) ->
                           {-# LINE 1030 "./src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 957 "dist/build/Transform.hs"#-}
   {-# INLINE rule47 #-}
   {-# LINE 1086 "./src-ag/Transform.ag" #-}
   rule47 = \ _allNonterminals ((_elemsIattrDecls) :: Map NontermIdent (Attributes, Attributes)) ((_lhsIoptions) :: Options) ->
                             {-# LINE 1086 "./src-ag/Transform.ag" #-}
                             if withSelf _lhsIoptions
                              then foldr addSelf _elemsIattrDecls (Set.toList _allNonterminals    )
                              else               _elemsIattrDecls
                             {-# LINE 965 "dist/build/Transform.hs"#-}
   {-# INLINE rule48 #-}
   {-# LINE 1328 "./src-ag/Transform.ag" #-}
   rule48 = \ ((_elemsIcollectedMacros) :: [(NontermIdent, ConstructorIdent, MaybeMacro)]) ->
                             {-# LINE 1328 "./src-ag/Transform.ag" #-}
                             let f (nt,con,m) = Map.insertWith (Map.union) nt (Map.singleton con m)
                             in  foldr f (Map.empty) _elemsIcollectedMacros
                             {-# LINE 972 "dist/build/Transform.hs"#-}
   {-# INLINE rule49 #-}
   {-# LINE 1341 "./src-ag/Transform.ag" #-}
   rule49 = \ _allAttrs _allFields _allNonterminals ->
                        {-# LINE 1341 "./src-ag/Transform.ag" #-}
                        (_allNonterminals    ,_allFields    ,_allAttrs    )
                        {-# LINE 978 "dist/build/Transform.hs"#-}
   {-# INLINE rule50 #-}
   {-# LINE 1343 "./src-ag/Transform.ag" #-}
   rule50 = \ _allNonterminals ((_elemsIattrs) :: Map NontermIdent (Attributes, Attributes)) ((_lhsIoptions) :: Options) ->
                         {-# LINE 1343 "./src-ag/Transform.ag" #-}
                         if withSelf _lhsIoptions
                              then foldr addSelf _elemsIattrs (Set.toList _allNonterminals    )
                              else               _elemsIattrs
                         {-# LINE 986 "dist/build/Transform.hs"#-}
   {-# INLINE rule51 #-}
   {-# LINE 1351 "./src-ag/Transform.ag" #-}
   rule51 = \  (_ :: ()) ->
                        {-# LINE 1351 "./src-ag/Transform.ag" #-}
                        Map.empty
                        {-# LINE 992 "dist/build/Transform.hs"#-}
   {-# INLINE rule52 #-}
   rule52 = \ ((_elemsIblocks) :: Blocks) ->
     _elemsIblocks
   {-# INLINE rule53 #-}
   rule53 = \ ((_elemsIconstructorTypeMap) :: Map NontermIdent ConstructorType) ->
     _elemsIconstructorTypeMap
   {-# INLINE rule54 #-}
   rule54 = \ ((_elemsImoduleDecl) :: Maybe (String,String,String)) ->
     _elemsImoduleDecl
   {-# INLINE rule55 #-}
   rule55 = \ ((_elemsIpragmas) :: Options -> Options) ->
     _elemsIpragmas
   {-# INLINE rule56 #-}
   rule56 = \ _allAttrDecls ->
     _allAttrDecls
   {-# INLINE rule57 #-}
   rule57 = \ _allAttrs ->
     _allAttrs
   {-# INLINE rule58 #-}
   rule58 = \ _allFields ->
     _allFields
   {-# INLINE rule59 #-}
   rule59 = \ _allNonterminals ->
     _allNonterminals
   {-# INLINE rule60 #-}
   rule60 = \ ((_lhsIoptions) :: Options) ->
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
         _lhsOcollectedFields = rule61 _fieldsIcollectedFields _lhsIallConstructors _lhsInts _namesIconstructors
         _lhsOcollectedConstraints :: [(NontermIdent, ConstructorIdent, [Type])]
         _lhsOcollectedConstraints = rule62 _fieldsIcollectedConstraints _lhsIallConstructors _lhsInts _namesIconstructors
         _lhsOcollectedConParams :: [(NontermIdent, ConstructorIdent, Set Identifier)]
         _lhsOcollectedConParams = rule63 _lhsIallConstructors _lhsInts _namesIconstructors arg_tyvars_
         _lhsOcollectedMacros :: [(NontermIdent, ConstructorIdent, MaybeMacro)]
         _lhsOcollectedMacros = rule64 _lhsIallConstructors _lhsInts _namesIconstructors arg_macro_
         _lhsOcollectedConstructorNames :: Set ConstructorIdent
         _lhsOcollectedConstructorNames = rule65 _namesIcollectedConstructorNames
         _fieldsOallNonterminals = rule66 _lhsIallNonterminals
         __result_ = T_Alt_vOut4 _lhsOcollectedConParams _lhsOcollectedConstraints _lhsOcollectedConstructorNames _lhsOcollectedFields _lhsOcollectedMacros
         in __result_ )
     in C_Alt_s5 v4
   {-# INLINE rule61 #-}
   {-# LINE 240 "./src-ag/Transform.ag" #-}
   rule61 = \ ((_fieldsIcollectedFields) :: [(Identifier, Type)]) ((_lhsIallConstructors) :: Map NontermIdent (Set ConstructorIdent)) ((_lhsInts) :: Set NontermIdent) ((_namesIconstructors) :: (Set ConstructorIdent->Set ConstructorIdent)) ->
                                        {-# LINE 240 "./src-ag/Transform.ag" #-}
                                        [ (nt, con, _fieldsIcollectedFields)
                                        | nt  <- Set.toList _lhsInts
                                        , con <- Set.toList (_namesIconstructors (Map.findWithDefault Set.empty nt _lhsIallConstructors))
                                        ]
                                        {-# LINE 1084 "dist/build/Transform.hs"#-}
   {-# INLINE rule62 #-}
   {-# LINE 244 "./src-ag/Transform.ag" #-}
   rule62 = \ ((_fieldsIcollectedConstraints) :: [Type]) ((_lhsIallConstructors) :: Map NontermIdent (Set ConstructorIdent)) ((_lhsInts) :: Set NontermIdent) ((_namesIconstructors) :: (Set ConstructorIdent->Set ConstructorIdent)) ->
                                        {-# LINE 244 "./src-ag/Transform.ag" #-}
                                        [ (nt, con, _fieldsIcollectedConstraints)
                                        | nt  <- Set.toList _lhsInts
                                        , con <- Set.toList (_namesIconstructors (Map.findWithDefault Set.empty nt _lhsIallConstructors))
                                        ]
                                        {-# LINE 1093 "dist/build/Transform.hs"#-}
   {-# INLINE rule63 #-}
   {-# LINE 248 "./src-ag/Transform.ag" #-}
   rule63 = \ ((_lhsIallConstructors) :: Map NontermIdent (Set ConstructorIdent)) ((_lhsInts) :: Set NontermIdent) ((_namesIconstructors) :: (Set ConstructorIdent->Set ConstructorIdent)) tyvars_ ->
                                        {-# LINE 248 "./src-ag/Transform.ag" #-}
                                        [ (nt, con, Set.fromList tyvars_)
                                        | nt  <- Set.toList _lhsInts
                                        , con <- Set.toList (_namesIconstructors (Map.findWithDefault Set.empty nt _lhsIallConstructors))
                                        ]
                                        {-# LINE 1102 "dist/build/Transform.hs"#-}
   {-# INLINE rule64 #-}
   {-# LINE 1319 "./src-ag/Transform.ag" #-}
   rule64 = \ ((_lhsIallConstructors) :: Map NontermIdent (Set ConstructorIdent)) ((_lhsInts) :: Set NontermIdent) ((_namesIconstructors) :: (Set ConstructorIdent->Set ConstructorIdent)) macro_ ->
                                        {-# LINE 1319 "./src-ag/Transform.ag" #-}
                                        [ (nt, con, macro_)
                                        | nt  <- Set.toList _lhsInts
                                        , con <- Set.toList (_namesIconstructors (Map.findWithDefault Set.empty nt _lhsIallConstructors))
                                        ]
                                        {-# LINE 1111 "dist/build/Transform.hs"#-}
   {-# INLINE rule65 #-}
   rule65 = \ ((_namesIcollectedConstructorNames) :: Set ConstructorIdent) ->
     _namesIcollectedConstructorNames
   {-# INLINE rule66 #-}
   rule66 = \ ((_lhsIallNonterminals) :: Set NontermIdent) ->
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
         _lhsOcollectedConParams = rule67 _hdIcollectedConParams _tlIcollectedConParams
         _lhsOcollectedConstraints :: [(NontermIdent, ConstructorIdent, [Type])]
         _lhsOcollectedConstraints = rule68 _hdIcollectedConstraints _tlIcollectedConstraints
         _lhsOcollectedConstructorNames :: Set ConstructorIdent
         _lhsOcollectedConstructorNames = rule69 _hdIcollectedConstructorNames _tlIcollectedConstructorNames
         _lhsOcollectedFields :: [(NontermIdent, ConstructorIdent, FieldMap)]
         _lhsOcollectedFields = rule70 _hdIcollectedFields _tlIcollectedFields
         _lhsOcollectedMacros :: [(NontermIdent, ConstructorIdent, MaybeMacro)]
         _lhsOcollectedMacros = rule71 _hdIcollectedMacros _tlIcollectedMacros
         _hdOallConstructors = rule72 _lhsIallConstructors
         _hdOallNonterminals = rule73 _lhsIallNonterminals
         _hdOnts = rule74 _lhsInts
         _tlOallConstructors = rule75 _lhsIallConstructors
         _tlOallNonterminals = rule76 _lhsIallNonterminals
         _tlOnts = rule77 _lhsInts
         __result_ = T_Alts_vOut7 _lhsOcollectedConParams _lhsOcollectedConstraints _lhsOcollectedConstructorNames _lhsOcollectedFields _lhsOcollectedMacros
         in __result_ )
     in C_Alts_s8 v7
   {-# INLINE rule67 #-}
   rule67 = \ ((_hdIcollectedConParams) :: [(NontermIdent, ConstructorIdent, Set Identifier)]) ((_tlIcollectedConParams) :: [(NontermIdent, ConstructorIdent, Set Identifier)]) ->
     _hdIcollectedConParams ++ _tlIcollectedConParams
   {-# INLINE rule68 #-}
   rule68 = \ ((_hdIcollectedConstraints) :: [(NontermIdent, ConstructorIdent, [Type])]) ((_tlIcollectedConstraints) :: [(NontermIdent, ConstructorIdent, [Type])]) ->
     _hdIcollectedConstraints ++ _tlIcollectedConstraints
   {-# INLINE rule69 #-}
   rule69 = \ ((_hdIcollectedConstructorNames) :: Set ConstructorIdent) ((_tlIcollectedConstructorNames) :: Set ConstructorIdent) ->
     _hdIcollectedConstructorNames `Set.union` _tlIcollectedConstructorNames
   {-# INLINE rule70 #-}
   rule70 = \ ((_hdIcollectedFields) :: [(NontermIdent, ConstructorIdent, FieldMap)]) ((_tlIcollectedFields) :: [(NontermIdent, ConstructorIdent, FieldMap)]) ->
     _hdIcollectedFields ++ _tlIcollectedFields
   {-# INLINE rule71 #-}
   rule71 = \ ((_hdIcollectedMacros) :: [(NontermIdent, ConstructorIdent, MaybeMacro)]) ((_tlIcollectedMacros) :: [(NontermIdent, ConstructorIdent, MaybeMacro)]) ->
     _hdIcollectedMacros ++ _tlIcollectedMacros
   {-# INLINE rule72 #-}
   rule72 = \ ((_lhsIallConstructors) :: Map NontermIdent (Set ConstructorIdent)) ->
     _lhsIallConstructors
   {-# INLINE rule73 #-}
   rule73 = \ ((_lhsIallNonterminals) :: Set NontermIdent) ->
     _lhsIallNonterminals
   {-# INLINE rule74 #-}
   rule74 = \ ((_lhsInts) :: Set NontermIdent) ->
     _lhsInts
   {-# INLINE rule75 #-}
   rule75 = \ ((_lhsIallConstructors) :: Map NontermIdent (Set ConstructorIdent)) ->
     _lhsIallConstructors
   {-# INLINE rule76 #-}
   rule76 = \ ((_lhsIallNonterminals) :: Set NontermIdent) ->
     _lhsIallNonterminals
   {-# INLINE rule77 #-}
   rule77 = \ ((_lhsInts) :: Set NontermIdent) ->
     _lhsInts
{-# NOINLINE sem_Alts_Nil #-}
sem_Alts_Nil ::  T_Alts 
sem_Alts_Nil  = T_Alts (return st8) where
   {-# NOINLINE st8 #-}
   st8 = let
      v7 :: T_Alts_v7 
      v7 = \ (T_Alts_vIn7 _lhsIallConstructors _lhsIallNonterminals _lhsInts) -> ( let
         _lhsOcollectedConParams :: [(NontermIdent, ConstructorIdent, Set Identifier)]
         _lhsOcollectedConParams = rule78  ()
         _lhsOcollectedConstraints :: [(NontermIdent, ConstructorIdent, [Type])]
         _lhsOcollectedConstraints = rule79  ()
         _lhsOcollectedConstructorNames :: Set ConstructorIdent
         _lhsOcollectedConstructorNames = rule80  ()
         _lhsOcollectedFields :: [(NontermIdent, ConstructorIdent, FieldMap)]
         _lhsOcollectedFields = rule81  ()
         _lhsOcollectedMacros :: [(NontermIdent, ConstructorIdent, MaybeMacro)]
         _lhsOcollectedMacros = rule82  ()
         __result_ = T_Alts_vOut7 _lhsOcollectedConParams _lhsOcollectedConstraints _lhsOcollectedConstructorNames _lhsOcollectedFields _lhsOcollectedMacros
         in __result_ )
     in C_Alts_s8 v7
   {-# INLINE rule78 #-}
   rule78 = \  (_ :: ()) ->
     []
   {-# INLINE rule79 #-}
   rule79 = \  (_ :: ()) ->
     []
   {-# INLINE rule80 #-}
   rule80 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule81 #-}
   rule81 = \  (_ :: ()) ->
     []
   {-# INLINE rule82 #-}
   rule82 = \  (_ :: ()) ->
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
         (_attrDecls,_errors) = rule83 _inherited _lhsIallFields _lhsIattrDecls _lhsInts _synthesized
         (_inherited,_synthesized,_useMap) = rule84 _lhsIallNonterminals arg_chn_ arg_inh_ arg_syn_
         _lhsOuseMap :: Map NontermIdent (Map Identifier (String,String,String))
         _lhsOuseMap = rule85 _lhsInts _useMap
         _errors1 = rule86 _lhsIoptions arg_chn_ arg_inh_ arg_syn_
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule87 _errors _errors1
         _lhsOattrs :: Map NontermIdent (Attributes, Attributes)
         _lhsOattrs = rule88 _inherited _lhsIattrs _lhsInts _synthesized
         _lhsOattrDecls :: Map NontermIdent (Attributes, Attributes)
         _lhsOattrDecls = rule89 _attrDecls
         __result_ = T_Attrs_vOut10 _lhsOattrDecls _lhsOattrs _lhsOerrors _lhsOuseMap
         in __result_ )
     in C_Attrs_s11 v10
   {-# INLINE rule83 #-}
   {-# LINE 1039 "./src-ag/Transform.ag" #-}
   rule83 = \ _inherited ((_lhsIallFields) :: DataTypes) ((_lhsIattrDecls) :: Map NontermIdent (Attributes, Attributes)) ((_lhsInts) :: Set NontermIdent) _synthesized ->
                                     {-# LINE 1039 "./src-ag/Transform.ag" #-}
                                     checkAttrs _lhsIallFields (Set.toList _lhsInts) _inherited _synthesized _lhsIattrDecls
                                     {-# LINE 1304 "dist/build/Transform.hs"#-}
   {-# INLINE rule84 #-}
   {-# LINE 1041 "./src-ag/Transform.ag" #-}
   rule84 = \ ((_lhsIallNonterminals) :: Set NontermIdent) chn_ inh_ syn_ ->
                                                 {-# LINE 1041 "./src-ag/Transform.ag" #-}
                                                 let splitAttrs xs = unzip [ ((n,makeType _lhsIallNonterminals t),(n,ud))
                                                                           | (n,t,ud) <- xs
                                                                           ]
                                                     (inh,_)     = splitAttrs inh_
                                                     (chn,uses1) = splitAttrs chn_
                                                     (syn,uses2) = splitAttrs syn_
                                                     isUse (_,(e1,e2,_)) = not (null e1 || null e2)
                                                 in (inh++chn,chn++syn, Map.fromList (Prelude.filter isUse (uses1++uses2)))
                                                 {-# LINE 1317 "dist/build/Transform.hs"#-}
   {-# INLINE rule85 #-}
   {-# LINE 1049 "./src-ag/Transform.ag" #-}
   rule85 = \ ((_lhsInts) :: Set NontermIdent) _useMap ->
                         {-# LINE 1049 "./src-ag/Transform.ag" #-}
                         Map.fromList (zip (Set.toList _lhsInts) (repeat _useMap))
                         {-# LINE 1323 "dist/build/Transform.hs"#-}
   {-# INLINE rule86 #-}
   {-# LINE 1051 "./src-ag/Transform.ag" #-}
   rule86 = \ ((_lhsIoptions) :: Options) chn_ inh_ syn_ ->
                          {-# LINE 1051 "./src-ag/Transform.ag" #-}
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
                          {-# LINE 1340 "dist/build/Transform.hs"#-}
   {-# INLINE rule87 #-}
   {-# LINE 1063 "./src-ag/Transform.ag" #-}
   rule87 = \ _errors _errors1 ->
                         {-# LINE 1063 "./src-ag/Transform.ag" #-}
                         _errors     Seq.>< _errors1
                         {-# LINE 1346 "dist/build/Transform.hs"#-}
   {-# INLINE rule88 #-}
   {-# LINE 1355 "./src-ag/Transform.ag" #-}
   rule88 = \ _inherited ((_lhsIattrs) :: Map NontermIdent (Attributes, Attributes)) ((_lhsInts) :: Set NontermIdent) _synthesized ->
                          {-# LINE 1355 "./src-ag/Transform.ag" #-}
                          let ins decls nt = if Map.member nt decls
                                             then  Map.update (\(inh,syn) -> Just ( Map.union inh $ Map.fromList _inherited
                                                                                       , Map.union syn $ Map.fromList _synthesized)) nt decls
                                             else  Map.insert nt (Map.fromList _inherited, Map.fromList _synthesized) decls
                          in  foldl ins _lhsIattrs (Set.toList _lhsInts)
                          {-# LINE 1356 "dist/build/Transform.hs"#-}
   {-# INLINE rule89 #-}
   rule89 = \ _attrDecls ->
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
         _lhsOcollectedConstructorNames = rule90 arg_name_
         _lhsOconstructors :: (Set ConstructorIdent->Set ConstructorIdent)
         _lhsOconstructors = rule91 arg_name_
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule92  ()
         __result_ = T_ConstructorSet_vOut13 _lhsOcollectedConstructorNames _lhsOconstructors _lhsOerrors
         in __result_ )
     in C_ConstructorSet_s14 v13
   {-# INLINE rule90 #-}
   {-# LINE 614 "./src-ag/Transform.ag" #-}
   rule90 = \ name_ ->
                                            {-# LINE 614 "./src-ag/Transform.ag" #-}
                                            Set.singleton name_
                                            {-# LINE 1415 "dist/build/Transform.hs"#-}
   {-# INLINE rule91 #-}
   {-# LINE 777 "./src-ag/Transform.ag" #-}
   rule91 = \ name_ ->
                                     {-# LINE 777 "./src-ag/Transform.ag" #-}
                                     \_  -> Set.singleton name_
                                     {-# LINE 1421 "dist/build/Transform.hs"#-}
   {-# INLINE rule92 #-}
   rule92 = \  (_ :: ()) ->
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
         _lhsOconstructors = rule93 _set1Iconstructors _set2Iconstructors
         _lhsOcollectedConstructorNames :: Set ConstructorIdent
         _lhsOcollectedConstructorNames = rule94 _set1IcollectedConstructorNames _set2IcollectedConstructorNames
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule95 _set1Ierrors _set2Ierrors
         __result_ = T_ConstructorSet_vOut13 _lhsOcollectedConstructorNames _lhsOconstructors _lhsOerrors
         in __result_ )
     in C_ConstructorSet_s14 v13
   {-# INLINE rule93 #-}
   {-# LINE 778 "./src-ag/Transform.ag" #-}
   rule93 = \ ((_set1Iconstructors) :: (Set ConstructorIdent->Set ConstructorIdent)) ((_set2Iconstructors) :: (Set ConstructorIdent->Set ConstructorIdent)) ->
                                     {-# LINE 778 "./src-ag/Transform.ag" #-}
                                     \ds -> _set1Iconstructors ds `Set.union`      _set2Iconstructors ds
                                     {-# LINE 1450 "dist/build/Transform.hs"#-}
   {-# INLINE rule94 #-}
   rule94 = \ ((_set1IcollectedConstructorNames) :: Set ConstructorIdent) ((_set2IcollectedConstructorNames) :: Set ConstructorIdent) ->
     _set1IcollectedConstructorNames `Set.union` _set2IcollectedConstructorNames
   {-# INLINE rule95 #-}
   rule95 = \ ((_set1Ierrors) :: Seq Error) ((_set2Ierrors) :: Seq Error) ->
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
         _lhsOconstructors = rule96 _set1Iconstructors _set2Iconstructors
         _lhsOcollectedConstructorNames :: Set ConstructorIdent
         _lhsOcollectedConstructorNames = rule97 _set1IcollectedConstructorNames _set2IcollectedConstructorNames
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule98 _set1Ierrors _set2Ierrors
         __result_ = T_ConstructorSet_vOut13 _lhsOcollectedConstructorNames _lhsOconstructors _lhsOerrors
         in __result_ )
     in C_ConstructorSet_s14 v13
   {-# INLINE rule96 #-}
   {-# LINE 779 "./src-ag/Transform.ag" #-}
   rule96 = \ ((_set1Iconstructors) :: (Set ConstructorIdent->Set ConstructorIdent)) ((_set2Iconstructors) :: (Set ConstructorIdent->Set ConstructorIdent)) ->
                                     {-# LINE 779 "./src-ag/Transform.ag" #-}
                                     \ds -> _set1Iconstructors ds `Set.difference` _set2Iconstructors ds
                                     {-# LINE 1482 "dist/build/Transform.hs"#-}
   {-# INLINE rule97 #-}
   rule97 = \ ((_set1IcollectedConstructorNames) :: Set ConstructorIdent) ((_set2IcollectedConstructorNames) :: Set ConstructorIdent) ->
     _set1IcollectedConstructorNames `Set.union` _set2IcollectedConstructorNames
   {-# INLINE rule98 #-}
   rule98 = \ ((_set1Ierrors) :: Seq Error) ((_set2Ierrors) :: Seq Error) ->
     _set1Ierrors Seq.>< _set2Ierrors
{-# NOINLINE sem_ConstructorSet_CAll #-}
sem_ConstructorSet_CAll ::  T_ConstructorSet 
sem_ConstructorSet_CAll  = T_ConstructorSet (return st14) where
   {-# NOINLINE st14 #-}
   st14 = let
      v13 :: T_ConstructorSet_v13 
      v13 = \ (T_ConstructorSet_vIn13 ) -> ( let
         _lhsOconstructors :: (Set ConstructorIdent->Set ConstructorIdent)
         _lhsOconstructors = rule99  ()
         _lhsOcollectedConstructorNames :: Set ConstructorIdent
         _lhsOcollectedConstructorNames = rule100  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule101  ()
         __result_ = T_ConstructorSet_vOut13 _lhsOcollectedConstructorNames _lhsOconstructors _lhsOerrors
         in __result_ )
     in C_ConstructorSet_s14 v13
   {-# INLINE rule99 #-}
   {-# LINE 780 "./src-ag/Transform.ag" #-}
   rule99 = \  (_ :: ()) ->
                                     {-# LINE 780 "./src-ag/Transform.ag" #-}
                                     \ds -> ds
                                     {-# LINE 1510 "dist/build/Transform.hs"#-}
   {-# INLINE rule100 #-}
   rule100 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule101 #-}
   rule101 = \  (_ :: ()) ->
     Seq.empty

-- Elem --------------------------------------------------------
-- wrapper
data Inh_Elem  = Inh_Elem { allAttrDecls_Inh_Elem :: (Map NontermIdent (Attributes, Attributes)), allAttrs_Inh_Elem :: (Map NontermIdent (Attributes, Attributes)), allConstructors_Inh_Elem :: (Map NontermIdent (Set ConstructorIdent)), allFields_Inh_Elem :: (DataTypes), allNonterminals_Inh_Elem :: (Set NontermIdent), attrDecls_Inh_Elem :: (Map NontermIdent (Attributes, Attributes)), attrs_Inh_Elem :: (Map NontermIdent (Attributes, Attributes)), defSets_Inh_Elem :: (Map Identifier (Set NontermIdent,Set Identifier)), definedSets_Inh_Elem :: (DefinedSets), options_Inh_Elem :: (Options) }
data Syn_Elem  = Syn_Elem { attrDecls_Syn_Elem :: (Map NontermIdent (Attributes, Attributes)), attrOrderCollect_Syn_Elem :: (AttrOrderMap), attrs_Syn_Elem :: (Map NontermIdent (Attributes, Attributes)), blocks_Syn_Elem :: (Blocks), collectedArounds_Syn_Elem :: ([ (NontermIdent, ConstructorIdent, [AroundInfo])  ]), collectedAugments_Syn_Elem :: ([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]), collectedConParams_Syn_Elem :: ([(NontermIdent, ConstructorIdent, Set Identifier)]), collectedConstraints_Syn_Elem :: ([(NontermIdent, ConstructorIdent, [Type])]), collectedConstructorsMap_Syn_Elem :: (Map NontermIdent (Set ConstructorIdent)), collectedFields_Syn_Elem :: ([(NontermIdent, ConstructorIdent, FieldMap)]), collectedInsts_Syn_Elem :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ]), collectedMacros_Syn_Elem :: ([(NontermIdent, ConstructorIdent, MaybeMacro)]), collectedMerges_Syn_Elem :: ([ (NontermIdent, ConstructorIdent, [MergeInfo])   ]), collectedNames_Syn_Elem :: (Set Identifier), collectedRules_Syn_Elem :: ([ (NontermIdent, ConstructorIdent, RuleInfo)]), collectedSetNames_Syn_Elem :: (Set Identifier), collectedSigs_Syn_Elem :: ([ (NontermIdent, ConstructorIdent, SigInfo) ]), collectedUniques_Syn_Elem :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]), constructorTypeMap_Syn_Elem :: (Map NontermIdent ConstructorType), ctxCollect_Syn_Elem :: (ContextMap), defSets_Syn_Elem :: (Map Identifier (Set NontermIdent,Set Identifier)), derivings_Syn_Elem :: (Derivings), errors_Syn_Elem :: (Seq Error), moduleDecl_Syn_Elem :: (Maybe (String,String,String)), paramsCollect_Syn_Elem :: (ParamMap), pragmas_Syn_Elem :: (Options -> Options), quantCollect_Syn_Elem :: (QuantMap), semPragmasCollect_Syn_Elem :: (PragmaMap), typeSyns_Syn_Elem :: (TypeSyns), useMap_Syn_Elem :: (Map NontermIdent (Map Identifier (String,String,String))), wrappers_Syn_Elem :: (Set NontermIdent) }
{-# INLINABLE wrap_Elem #-}
wrap_Elem :: T_Elem  -> Inh_Elem  -> (Syn_Elem )
wrap_Elem (T_Elem act) (Inh_Elem _lhsIallAttrDecls _lhsIallAttrs _lhsIallConstructors _lhsIallFields _lhsIallNonterminals _lhsIattrDecls _lhsIattrs _lhsIdefSets _lhsIdefinedSets _lhsIoptions) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Elem_vIn16 _lhsIallAttrDecls _lhsIallAttrs _lhsIallConstructors _lhsIallFields _lhsIallNonterminals _lhsIattrDecls _lhsIattrs _lhsIdefSets _lhsIdefinedSets _lhsIoptions
        (T_Elem_vOut16 _lhsOattrDecls _lhsOattrOrderCollect _lhsOattrs _lhsOblocks _lhsOcollectedArounds _lhsOcollectedAugments _lhsOcollectedConParams _lhsOcollectedConstraints _lhsOcollectedConstructorsMap _lhsOcollectedFields _lhsOcollectedInsts _lhsOcollectedMacros _lhsOcollectedMerges _lhsOcollectedNames _lhsOcollectedRules _lhsOcollectedSetNames _lhsOcollectedSigs _lhsOcollectedUniques _lhsOconstructorTypeMap _lhsOctxCollect _lhsOdefSets _lhsOderivings _lhsOerrors _lhsOmoduleDecl _lhsOparamsCollect _lhsOpragmas _lhsOquantCollect _lhsOsemPragmasCollect _lhsOtypeSyns _lhsOuseMap _lhsOwrappers) <- return (inv_Elem_s17 sem arg)
        return (Syn_Elem _lhsOattrDecls _lhsOattrOrderCollect _lhsOattrs _lhsOblocks _lhsOcollectedArounds _lhsOcollectedAugments _lhsOcollectedConParams _lhsOcollectedConstraints _lhsOcollectedConstructorsMap _lhsOcollectedFields _lhsOcollectedInsts _lhsOcollectedMacros _lhsOcollectedMerges _lhsOcollectedNames _lhsOcollectedRules _lhsOcollectedSetNames _lhsOcollectedSigs _lhsOcollectedUniques _lhsOconstructorTypeMap _lhsOctxCollect _lhsOdefSets _lhsOderivings _lhsOerrors _lhsOmoduleDecl _lhsOparamsCollect _lhsOpragmas _lhsOquantCollect _lhsOsemPragmasCollect _lhsOtypeSyns _lhsOuseMap _lhsOwrappers)
   )

-- cata
{-# NOINLINE sem_Elem #-}
sem_Elem :: Elem  -> T_Elem 
sem_Elem ( Data pos_ contype_ ctx_ names_ params_ attrs_ alts_ ext_ ) = sem_Elem_Data pos_ contype_ ctx_ ( sem_NontSet names_ ) params_ ( sem_Attrs attrs_ ) ( sem_Alts alts_ ) ext_
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
data T_Elem_vOut16  = T_Elem_vOut16 (Map NontermIdent (Attributes, Attributes)) (AttrOrderMap) (Map NontermIdent (Attributes, Attributes)) (Blocks) ([ (NontermIdent, ConstructorIdent, [AroundInfo])  ]) ([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]) ([(NontermIdent, ConstructorIdent, Set Identifier)]) ([(NontermIdent, ConstructorIdent, [Type])]) (Map NontermIdent (Set ConstructorIdent)) ([(NontermIdent, ConstructorIdent, FieldMap)]) ([ (NontermIdent, ConstructorIdent, [Identifier]) ]) ([(NontermIdent, ConstructorIdent, MaybeMacro)]) ([ (NontermIdent, ConstructorIdent, [MergeInfo])   ]) (Set Identifier) ([ (NontermIdent, ConstructorIdent, RuleInfo)]) (Set Identifier) ([ (NontermIdent, ConstructorIdent, SigInfo) ]) ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]) (Map NontermIdent ConstructorType) (ContextMap) (Map Identifier (Set NontermIdent,Set Identifier)) (Derivings) (Seq Error) (Maybe (String,String,String)) (ParamMap) (Options -> Options) (QuantMap) (PragmaMap) (TypeSyns) (Map NontermIdent (Map Identifier (String,String,String))) (Set NontermIdent)
{-# NOINLINE sem_Elem_Data #-}
sem_Elem_Data :: (Pos) -> (ConstructorType) -> (ClassContext) -> T_NontSet  -> ([Identifier]) -> T_Attrs  -> T_Alts  -> (Bool) -> T_Elem 
sem_Elem_Data _ arg_contype_ arg_ctx_ arg_names_ arg_params_ arg_attrs_ arg_alts_ _ = T_Elem (return st17) where
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
         _altsOnts = rule102 _namesInontSet
         _lhsOcollectedConstructorsMap :: Map NontermIdent (Set ConstructorIdent)
         _lhsOcollectedConstructorsMap = rule103 _altsIcollectedConstructorNames _namesInontSet
         _lhsOparamsCollect :: ParamMap
         _lhsOparamsCollect = rule104 _namesInontSet arg_params_
         _lhsOctxCollect :: ContextMap
         _lhsOctxCollect = rule105 _namesInontSet arg_ctx_
         _attrsOnts = rule106 _namesInontSet
         _lhsOconstructorTypeMap :: Map NontermIdent ConstructorType
         _lhsOconstructorTypeMap = rule107 _namesIcollectedNames arg_contype_
         _lhsOattrOrderCollect :: AttrOrderMap
         _lhsOattrOrderCollect = rule108  ()
         _lhsOblocks :: Blocks
         _lhsOblocks = rule109  ()
         _lhsOcollectedArounds :: [ (NontermIdent, ConstructorIdent, [AroundInfo])  ]
         _lhsOcollectedArounds = rule110  ()
         _lhsOcollectedAugments :: [ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]
         _lhsOcollectedAugments = rule111  ()
         _lhsOcollectedConParams :: [(NontermIdent, ConstructorIdent, Set Identifier)]
         _lhsOcollectedConParams = rule112 _altsIcollectedConParams
         _lhsOcollectedConstraints :: [(NontermIdent, ConstructorIdent, [Type])]
         _lhsOcollectedConstraints = rule113 _altsIcollectedConstraints
         _lhsOcollectedFields :: [(NontermIdent, ConstructorIdent, FieldMap)]
         _lhsOcollectedFields = rule114 _altsIcollectedFields
         _lhsOcollectedInsts :: [ (NontermIdent, ConstructorIdent, [Identifier]) ]
         _lhsOcollectedInsts = rule115  ()
         _lhsOcollectedMacros :: [(NontermIdent, ConstructorIdent, MaybeMacro)]
         _lhsOcollectedMacros = rule116 _altsIcollectedMacros
         _lhsOcollectedMerges :: [ (NontermIdent, ConstructorIdent, [MergeInfo])   ]
         _lhsOcollectedMerges = rule117  ()
         _lhsOcollectedNames :: Set Identifier
         _lhsOcollectedNames = rule118 _namesIcollectedNames
         _lhsOcollectedRules :: [ (NontermIdent, ConstructorIdent, RuleInfo)]
         _lhsOcollectedRules = rule119  ()
         _lhsOcollectedSetNames :: Set Identifier
         _lhsOcollectedSetNames = rule120  ()
         _lhsOcollectedSigs :: [ (NontermIdent, ConstructorIdent, SigInfo) ]
         _lhsOcollectedSigs = rule121  ()
         _lhsOcollectedUniques :: [ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]
         _lhsOcollectedUniques = rule122  ()
         _lhsOderivings :: Derivings
         _lhsOderivings = rule123  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule124 _attrsIerrors _namesIerrors
         _lhsOmoduleDecl :: Maybe (String,String,String)
         _lhsOmoduleDecl = rule125  ()
         _lhsOpragmas :: Options -> Options
         _lhsOpragmas = rule126  ()
         _lhsOquantCollect :: QuantMap
         _lhsOquantCollect = rule127  ()
         _lhsOsemPragmasCollect :: PragmaMap
         _lhsOsemPragmasCollect = rule128  ()
         _lhsOtypeSyns :: TypeSyns
         _lhsOtypeSyns = rule129  ()
         _lhsOuseMap :: Map NontermIdent (Map Identifier (String,String,String))
         _lhsOuseMap = rule130 _attrsIuseMap
         _lhsOwrappers :: Set NontermIdent
         _lhsOwrappers = rule131  ()
         _lhsOattrDecls :: Map NontermIdent (Attributes, Attributes)
         _lhsOattrDecls = rule132 _attrsIattrDecls
         _lhsOattrs :: Map NontermIdent (Attributes, Attributes)
         _lhsOattrs = rule133 _attrsIattrs
         _lhsOdefSets :: Map Identifier (Set NontermIdent,Set Identifier)
         _lhsOdefSets = rule134 _lhsIdefSets
         _namesOallFields = rule135 _lhsIallFields
         _namesOallNonterminals = rule136 _lhsIallNonterminals
         _namesOdefinedSets = rule137 _lhsIdefinedSets
         _attrsOallFields = rule138 _lhsIallFields
         _attrsOallNonterminals = rule139 _lhsIallNonterminals
         _attrsOattrDecls = rule140 _lhsIattrDecls
         _attrsOattrs = rule141 _lhsIattrs
         _attrsOoptions = rule142 _lhsIoptions
         _altsOallConstructors = rule143 _lhsIallConstructors
         _altsOallNonterminals = rule144 _lhsIallNonterminals
         __result_ = T_Elem_vOut16 _lhsOattrDecls _lhsOattrOrderCollect _lhsOattrs _lhsOblocks _lhsOcollectedArounds _lhsOcollectedAugments _lhsOcollectedConParams _lhsOcollectedConstraints _lhsOcollectedConstructorsMap _lhsOcollectedFields _lhsOcollectedInsts _lhsOcollectedMacros _lhsOcollectedMerges _lhsOcollectedNames _lhsOcollectedRules _lhsOcollectedSetNames _lhsOcollectedSigs _lhsOcollectedUniques _lhsOconstructorTypeMap _lhsOctxCollect _lhsOdefSets _lhsOderivings _lhsOerrors _lhsOmoduleDecl _lhsOparamsCollect _lhsOpragmas _lhsOquantCollect _lhsOsemPragmasCollect _lhsOtypeSyns _lhsOuseMap _lhsOwrappers
         in __result_ )
     in C_Elem_s17 v16
   {-# INLINE rule102 #-}
   {-# LINE 176 "./src-ag/Transform.ag" #-}
   rule102 = \ ((_namesInontSet) :: Set NontermIdent) ->
                      {-# LINE 176 "./src-ag/Transform.ag" #-}
                      _namesInontSet
                      {-# LINE 1653 "dist/build/Transform.hs"#-}
   {-# INLINE rule103 #-}
   {-# LINE 620 "./src-ag/Transform.ag" #-}
   rule103 = \ ((_altsIcollectedConstructorNames) :: Set ConstructorIdent) ((_namesInontSet) :: Set NontermIdent) ->
                                           {-# LINE 620 "./src-ag/Transform.ag" #-}
                                           Map.fromList
                                           [ (n, _altsIcollectedConstructorNames)
                                           | n <- Set.toList _namesInontSet
                                           ]
                                           {-# LINE 1662 "dist/build/Transform.hs"#-}
   {-# INLINE rule104 #-}
   {-# LINE 948 "./src-ag/Transform.ag" #-}
   rule104 = \ ((_namesInontSet) :: Set NontermIdent) params_ ->
                            {-# LINE 948 "./src-ag/Transform.ag" #-}
                            if null params_
                            then Map.empty
                            else Map.fromList [(nt, params_) | nt <- Set.toList _namesInontSet]
                            {-# LINE 1670 "dist/build/Transform.hs"#-}
   {-# INLINE rule105 #-}
   {-# LINE 971 "./src-ag/Transform.ag" #-}
   rule105 = \ ((_namesInontSet) :: Set NontermIdent) ctx_ ->
                         {-# LINE 971 "./src-ag/Transform.ag" #-}
                         if null ctx_
                         then Map.empty
                         else Map.fromList [(nt, ctx_) | nt <- Set.toList _namesInontSet]
                         {-# LINE 1678 "dist/build/Transform.hs"#-}
   {-# INLINE rule106 #-}
   {-# LINE 1033 "./src-ag/Transform.ag" #-}
   rule106 = \ ((_namesInontSet) :: Set NontermIdent) ->
                       {-# LINE 1033 "./src-ag/Transform.ag" #-}
                       _namesInontSet
                       {-# LINE 1684 "dist/build/Transform.hs"#-}
   {-# INLINE rule107 #-}
   {-# LINE 1371 "./src-ag/Transform.ag" #-}
   rule107 = \ ((_namesIcollectedNames) :: Set Identifier) contype_ ->
                                    {-# LINE 1371 "./src-ag/Transform.ag" #-}
                                    Set.fold (\nm mp -> Map.insert nm contype_ mp) Map.empty _namesIcollectedNames
                                    {-# LINE 1690 "dist/build/Transform.hs"#-}
   {-# INLINE rule108 #-}
   rule108 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule109 #-}
   rule109 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule110 #-}
   rule110 = \  (_ :: ()) ->
     []
   {-# INLINE rule111 #-}
   rule111 = \  (_ :: ()) ->
     []
   {-# INLINE rule112 #-}
   rule112 = \ ((_altsIcollectedConParams) :: [(NontermIdent, ConstructorIdent, Set Identifier)]) ->
     _altsIcollectedConParams
   {-# INLINE rule113 #-}
   rule113 = \ ((_altsIcollectedConstraints) :: [(NontermIdent, ConstructorIdent, [Type])]) ->
     _altsIcollectedConstraints
   {-# INLINE rule114 #-}
   rule114 = \ ((_altsIcollectedFields) :: [(NontermIdent, ConstructorIdent, FieldMap)]) ->
     _altsIcollectedFields
   {-# INLINE rule115 #-}
   rule115 = \  (_ :: ()) ->
     []
   {-# INLINE rule116 #-}
   rule116 = \ ((_altsIcollectedMacros) :: [(NontermIdent, ConstructorIdent, MaybeMacro)]) ->
     _altsIcollectedMacros
   {-# INLINE rule117 #-}
   rule117 = \  (_ :: ()) ->
     []
   {-# INLINE rule118 #-}
   rule118 = \ ((_namesIcollectedNames) :: Set Identifier) ->
     _namesIcollectedNames
   {-# INLINE rule119 #-}
   rule119 = \  (_ :: ()) ->
     []
   {-# INLINE rule120 #-}
   rule120 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule121 #-}
   rule121 = \  (_ :: ()) ->
     []
   {-# INLINE rule122 #-}
   rule122 = \  (_ :: ()) ->
     []
   {-# INLINE rule123 #-}
   rule123 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule124 #-}
   rule124 = \ ((_attrsIerrors) :: Seq Error) ((_namesIerrors) :: Seq Error) ->
     _namesIerrors Seq.>< _attrsIerrors
   {-# INLINE rule125 #-}
   rule125 = \  (_ :: ()) ->
     mzero
   {-# INLINE rule126 #-}
   rule126 = \  (_ :: ()) ->
     id
   {-# INLINE rule127 #-}
   rule127 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule128 #-}
   rule128 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule129 #-}
   rule129 = \  (_ :: ()) ->
     []
   {-# INLINE rule130 #-}
   rule130 = \ ((_attrsIuseMap) :: Map NontermIdent (Map Identifier (String,String,String))) ->
     _attrsIuseMap
   {-# INLINE rule131 #-}
   rule131 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule132 #-}
   rule132 = \ ((_attrsIattrDecls) :: Map NontermIdent (Attributes, Attributes)) ->
     _attrsIattrDecls
   {-# INLINE rule133 #-}
   rule133 = \ ((_attrsIattrs) :: Map NontermIdent (Attributes, Attributes)) ->
     _attrsIattrs
   {-# INLINE rule134 #-}
   rule134 = \ ((_lhsIdefSets) :: Map Identifier (Set NontermIdent,Set Identifier)) ->
     _lhsIdefSets
   {-# INLINE rule135 #-}
   rule135 = \ ((_lhsIallFields) :: DataTypes) ->
     _lhsIallFields
   {-# INLINE rule136 #-}
   rule136 = \ ((_lhsIallNonterminals) :: Set NontermIdent) ->
     _lhsIallNonterminals
   {-# INLINE rule137 #-}
   rule137 = \ ((_lhsIdefinedSets) :: DefinedSets) ->
     _lhsIdefinedSets
   {-# INLINE rule138 #-}
   rule138 = \ ((_lhsIallFields) :: DataTypes) ->
     _lhsIallFields
   {-# INLINE rule139 #-}
   rule139 = \ ((_lhsIallNonterminals) :: Set NontermIdent) ->
     _lhsIallNonterminals
   {-# INLINE rule140 #-}
   rule140 = \ ((_lhsIattrDecls) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIattrDecls
   {-# INLINE rule141 #-}
   rule141 = \ ((_lhsIattrs) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIattrs
   {-# INLINE rule142 #-}
   rule142 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule143 #-}
   rule143 = \ ((_lhsIallConstructors) :: Map NontermIdent (Set ConstructorIdent)) ->
     _lhsIallConstructors
   {-# INLINE rule144 #-}
   rule144 = \ ((_lhsIallNonterminals) :: Set NontermIdent) ->
     _lhsIallNonterminals
{-# NOINLINE sem_Elem_Type #-}
sem_Elem_Type :: (Pos) -> (ClassContext) -> (NontermIdent) -> ([Identifier]) -> (ComplexType) -> T_Elem 
sem_Elem_Type arg_pos_ arg_ctx_ arg_name_ arg_params_ arg_type_ = T_Elem (return st17) where
   {-# NOINLINE st17 #-}
   st17 = let
      v16 :: T_Elem_v16 
      v16 = \ (T_Elem_vIn16 _lhsIallAttrDecls _lhsIallAttrs _lhsIallConstructors _lhsIallFields _lhsIallNonterminals _lhsIattrDecls _lhsIattrs _lhsIdefSets _lhsIdefinedSets _lhsIoptions) -> ( let
         _lhsOcollectedFields :: [(NontermIdent, ConstructorIdent, FieldMap)]
         _lhsOcollectedFields = rule145 _expanded arg_name_
         _lhsOcollectedNames :: Set Identifier
         _lhsOcollectedNames = rule146 arg_name_
         _expanded = rule147 _argType arg_name_ arg_params_ arg_pos_
         _argType = rule148 _lhsIallNonterminals arg_type_
         _lhsOtypeSyns :: TypeSyns
         _lhsOtypeSyns = rule149 _argType arg_name_
         _lhsOparamsCollect :: ParamMap
         _lhsOparamsCollect = rule150 arg_name_ arg_params_
         _lhsOctxCollect :: ContextMap
         _lhsOctxCollect = rule151 arg_ctx_ arg_name_
         _lhsOattrOrderCollect :: AttrOrderMap
         _lhsOattrOrderCollect = rule152  ()
         _lhsOblocks :: Blocks
         _lhsOblocks = rule153  ()
         _lhsOcollectedArounds :: [ (NontermIdent, ConstructorIdent, [AroundInfo])  ]
         _lhsOcollectedArounds = rule154  ()
         _lhsOcollectedAugments :: [ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]
         _lhsOcollectedAugments = rule155  ()
         _lhsOcollectedConParams :: [(NontermIdent, ConstructorIdent, Set Identifier)]
         _lhsOcollectedConParams = rule156  ()
         _lhsOcollectedConstraints :: [(NontermIdent, ConstructorIdent, [Type])]
         _lhsOcollectedConstraints = rule157  ()
         _lhsOcollectedConstructorsMap :: Map NontermIdent (Set ConstructorIdent)
         _lhsOcollectedConstructorsMap = rule158  ()
         _lhsOcollectedInsts :: [ (NontermIdent, ConstructorIdent, [Identifier]) ]
         _lhsOcollectedInsts = rule159  ()
         _lhsOcollectedMacros :: [(NontermIdent, ConstructorIdent, MaybeMacro)]
         _lhsOcollectedMacros = rule160  ()
         _lhsOcollectedMerges :: [ (NontermIdent, ConstructorIdent, [MergeInfo])   ]
         _lhsOcollectedMerges = rule161  ()
         _lhsOcollectedRules :: [ (NontermIdent, ConstructorIdent, RuleInfo)]
         _lhsOcollectedRules = rule162  ()
         _lhsOcollectedSetNames :: Set Identifier
         _lhsOcollectedSetNames = rule163  ()
         _lhsOcollectedSigs :: [ (NontermIdent, ConstructorIdent, SigInfo) ]
         _lhsOcollectedSigs = rule164  ()
         _lhsOcollectedUniques :: [ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]
         _lhsOcollectedUniques = rule165  ()
         _lhsOconstructorTypeMap :: Map NontermIdent ConstructorType
         _lhsOconstructorTypeMap = rule166  ()
         _lhsOderivings :: Derivings
         _lhsOderivings = rule167  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule168  ()
         _lhsOmoduleDecl :: Maybe (String,String,String)
         _lhsOmoduleDecl = rule169  ()
         _lhsOpragmas :: Options -> Options
         _lhsOpragmas = rule170  ()
         _lhsOquantCollect :: QuantMap
         _lhsOquantCollect = rule171  ()
         _lhsOsemPragmasCollect :: PragmaMap
         _lhsOsemPragmasCollect = rule172  ()
         _lhsOuseMap :: Map NontermIdent (Map Identifier (String,String,String))
         _lhsOuseMap = rule173  ()
         _lhsOwrappers :: Set NontermIdent
         _lhsOwrappers = rule174  ()
         _lhsOattrDecls :: Map NontermIdent (Attributes, Attributes)
         _lhsOattrDecls = rule175 _lhsIattrDecls
         _lhsOattrs :: Map NontermIdent (Attributes, Attributes)
         _lhsOattrs = rule176 _lhsIattrs
         _lhsOdefSets :: Map Identifier (Set NontermIdent,Set Identifier)
         _lhsOdefSets = rule177 _lhsIdefSets
         __result_ = T_Elem_vOut16 _lhsOattrDecls _lhsOattrOrderCollect _lhsOattrs _lhsOblocks _lhsOcollectedArounds _lhsOcollectedAugments _lhsOcollectedConParams _lhsOcollectedConstraints _lhsOcollectedConstructorsMap _lhsOcollectedFields _lhsOcollectedInsts _lhsOcollectedMacros _lhsOcollectedMerges _lhsOcollectedNames _lhsOcollectedRules _lhsOcollectedSetNames _lhsOcollectedSigs _lhsOcollectedUniques _lhsOconstructorTypeMap _lhsOctxCollect _lhsOdefSets _lhsOderivings _lhsOerrors _lhsOmoduleDecl _lhsOparamsCollect _lhsOpragmas _lhsOquantCollect _lhsOsemPragmasCollect _lhsOtypeSyns _lhsOuseMap _lhsOwrappers
         in __result_ )
     in C_Elem_s17 v16
   {-# INLINE rule145 #-}
   {-# LINE 254 "./src-ag/Transform.ag" #-}
   rule145 = \ _expanded name_ ->
                                 {-# LINE 254 "./src-ag/Transform.ag" #-}
                                 map (\(x,y)->(name_, x, y)) _expanded
                                 {-# LINE 1881 "dist/build/Transform.hs"#-}
   {-# INLINE rule146 #-}
   {-# LINE 600 "./src-ag/Transform.ag" #-}
   rule146 = \ name_ ->
                                 {-# LINE 600 "./src-ag/Transform.ag" #-}
                                 Set.singleton name_
                                 {-# LINE 1887 "dist/build/Transform.hs"#-}
   {-# INLINE rule147 #-}
   {-# LINE 654 "./src-ag/Transform.ag" #-}
   rule147 = \ _argType name_ params_ pos_ ->
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
                                 {-# LINE 1929 "dist/build/Transform.hs"#-}
   {-# INLINE rule148 #-}
   {-# LINE 691 "./src-ag/Transform.ag" #-}
   rule148 = \ ((_lhsIallNonterminals) :: Set NontermIdent) type_ ->
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
                                 {-# LINE 1943 "dist/build/Transform.hs"#-}
   {-# INLINE rule149 #-}
   {-# LINE 700 "./src-ag/Transform.ag" #-}
   rule149 = \ _argType name_ ->
                                 {-# LINE 700 "./src-ag/Transform.ag" #-}
                                 [(name_,_argType)]
                                 {-# LINE 1949 "dist/build/Transform.hs"#-}
   {-# INLINE rule150 #-}
   {-# LINE 954 "./src-ag/Transform.ag" #-}
   rule150 = \ name_ params_ ->
                            {-# LINE 954 "./src-ag/Transform.ag" #-}
                            if null params_
                            then Map.empty
                            else Map.singleton name_ params_
                            {-# LINE 1957 "dist/build/Transform.hs"#-}
   {-# INLINE rule151 #-}
   {-# LINE 977 "./src-ag/Transform.ag" #-}
   rule151 = \ ctx_ name_ ->
                         {-# LINE 977 "./src-ag/Transform.ag" #-}
                         if null ctx_
                         then Map.empty
                         else Map.singleton name_ ctx_
                         {-# LINE 1965 "dist/build/Transform.hs"#-}
   {-# INLINE rule152 #-}
   rule152 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule153 #-}
   rule153 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule154 #-}
   rule154 = \  (_ :: ()) ->
     []
   {-# INLINE rule155 #-}
   rule155 = \  (_ :: ()) ->
     []
   {-# INLINE rule156 #-}
   rule156 = \  (_ :: ()) ->
     []
   {-# INLINE rule157 #-}
   rule157 = \  (_ :: ()) ->
     []
   {-# INLINE rule158 #-}
   rule158 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule159 #-}
   rule159 = \  (_ :: ()) ->
     []
   {-# INLINE rule160 #-}
   rule160 = \  (_ :: ()) ->
     []
   {-# INLINE rule161 #-}
   rule161 = \  (_ :: ()) ->
     []
   {-# INLINE rule162 #-}
   rule162 = \  (_ :: ()) ->
     []
   {-# INLINE rule163 #-}
   rule163 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule164 #-}
   rule164 = \  (_ :: ()) ->
     []
   {-# INLINE rule165 #-}
   rule165 = \  (_ :: ()) ->
     []
   {-# INLINE rule166 #-}
   rule166 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule167 #-}
   rule167 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule168 #-}
   rule168 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule169 #-}
   rule169 = \  (_ :: ()) ->
     mzero
   {-# INLINE rule170 #-}
   rule170 = \  (_ :: ()) ->
     id
   {-# INLINE rule171 #-}
   rule171 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule172 #-}
   rule172 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule173 #-}
   rule173 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule174 #-}
   rule174 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule175 #-}
   rule175 = \ ((_lhsIattrDecls) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIattrDecls
   {-# INLINE rule176 #-}
   rule176 = \ ((_lhsIattrs) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIattrs
   {-# INLINE rule177 #-}
   rule177 = \ ((_lhsIdefSets) :: Map Identifier (Set NontermIdent,Set Identifier)) ->
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
         _lhsOctxCollect = rule178 _namesInontSet arg_ctx_
         _lhsOquantCollect :: QuantMap
         _lhsOquantCollect = rule179 _namesInontSet arg_quants_
         _attrsOnts = rule180 _namesInontSet
         _lhsOattrOrderCollect :: AttrOrderMap
         _lhsOattrOrderCollect = rule181  ()
         _lhsOblocks :: Blocks
         _lhsOblocks = rule182  ()
         _lhsOcollectedArounds :: [ (NontermIdent, ConstructorIdent, [AroundInfo])  ]
         _lhsOcollectedArounds = rule183  ()
         _lhsOcollectedAugments :: [ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]
         _lhsOcollectedAugments = rule184  ()
         _lhsOcollectedConParams :: [(NontermIdent, ConstructorIdent, Set Identifier)]
         _lhsOcollectedConParams = rule185  ()
         _lhsOcollectedConstraints :: [(NontermIdent, ConstructorIdent, [Type])]
         _lhsOcollectedConstraints = rule186  ()
         _lhsOcollectedConstructorsMap :: Map NontermIdent (Set ConstructorIdent)
         _lhsOcollectedConstructorsMap = rule187  ()
         _lhsOcollectedFields :: [(NontermIdent, ConstructorIdent, FieldMap)]
         _lhsOcollectedFields = rule188  ()
         _lhsOcollectedInsts :: [ (NontermIdent, ConstructorIdent, [Identifier]) ]
         _lhsOcollectedInsts = rule189  ()
         _lhsOcollectedMacros :: [(NontermIdent, ConstructorIdent, MaybeMacro)]
         _lhsOcollectedMacros = rule190  ()
         _lhsOcollectedMerges :: [ (NontermIdent, ConstructorIdent, [MergeInfo])   ]
         _lhsOcollectedMerges = rule191  ()
         _lhsOcollectedNames :: Set Identifier
         _lhsOcollectedNames = rule192 _namesIcollectedNames
         _lhsOcollectedRules :: [ (NontermIdent, ConstructorIdent, RuleInfo)]
         _lhsOcollectedRules = rule193  ()
         _lhsOcollectedSetNames :: Set Identifier
         _lhsOcollectedSetNames = rule194  ()
         _lhsOcollectedSigs :: [ (NontermIdent, ConstructorIdent, SigInfo) ]
         _lhsOcollectedSigs = rule195  ()
         _lhsOcollectedUniques :: [ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]
         _lhsOcollectedUniques = rule196  ()
         _lhsOconstructorTypeMap :: Map NontermIdent ConstructorType
         _lhsOconstructorTypeMap = rule197  ()
         _lhsOderivings :: Derivings
         _lhsOderivings = rule198  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule199 _attrsIerrors _namesIerrors
         _lhsOmoduleDecl :: Maybe (String,String,String)
         _lhsOmoduleDecl = rule200  ()
         _lhsOparamsCollect :: ParamMap
         _lhsOparamsCollect = rule201  ()
         _lhsOpragmas :: Options -> Options
         _lhsOpragmas = rule202  ()
         _lhsOsemPragmasCollect :: PragmaMap
         _lhsOsemPragmasCollect = rule203  ()
         _lhsOtypeSyns :: TypeSyns
         _lhsOtypeSyns = rule204  ()
         _lhsOuseMap :: Map NontermIdent (Map Identifier (String,String,String))
         _lhsOuseMap = rule205 _attrsIuseMap
         _lhsOwrappers :: Set NontermIdent
         _lhsOwrappers = rule206  ()
         _lhsOattrDecls :: Map NontermIdent (Attributes, Attributes)
         _lhsOattrDecls = rule207 _attrsIattrDecls
         _lhsOattrs :: Map NontermIdent (Attributes, Attributes)
         _lhsOattrs = rule208 _attrsIattrs
         _lhsOdefSets :: Map Identifier (Set NontermIdent,Set Identifier)
         _lhsOdefSets = rule209 _lhsIdefSets
         _namesOallFields = rule210 _lhsIallFields
         _namesOallNonterminals = rule211 _lhsIallNonterminals
         _namesOdefinedSets = rule212 _lhsIdefinedSets
         _attrsOallFields = rule213 _lhsIallFields
         _attrsOallNonterminals = rule214 _lhsIallNonterminals
         _attrsOattrDecls = rule215 _lhsIattrDecls
         _attrsOattrs = rule216 _lhsIattrs
         _attrsOoptions = rule217 _lhsIoptions
         __result_ = T_Elem_vOut16 _lhsOattrDecls _lhsOattrOrderCollect _lhsOattrs _lhsOblocks _lhsOcollectedArounds _lhsOcollectedAugments _lhsOcollectedConParams _lhsOcollectedConstraints _lhsOcollectedConstructorsMap _lhsOcollectedFields _lhsOcollectedInsts _lhsOcollectedMacros _lhsOcollectedMerges _lhsOcollectedNames _lhsOcollectedRules _lhsOcollectedSetNames _lhsOcollectedSigs _lhsOcollectedUniques _lhsOconstructorTypeMap _lhsOctxCollect _lhsOdefSets _lhsOderivings _lhsOerrors _lhsOmoduleDecl _lhsOparamsCollect _lhsOpragmas _lhsOquantCollect _lhsOsemPragmasCollect _lhsOtypeSyns _lhsOuseMap _lhsOwrappers
         in __result_ )
     in C_Elem_s17 v16
   {-# INLINE rule178 #-}
   {-# LINE 971 "./src-ag/Transform.ag" #-}
   rule178 = \ ((_namesInontSet) :: Set NontermIdent) ctx_ ->
                         {-# LINE 971 "./src-ag/Transform.ag" #-}
                         if null ctx_
                         then Map.empty
                         else Map.fromList [(nt, ctx_) | nt <- Set.toList _namesInontSet]
                         {-# LINE 2136 "dist/build/Transform.hs"#-}
   {-# INLINE rule179 #-}
   {-# LINE 996 "./src-ag/Transform.ag" #-}
   rule179 = \ ((_namesInontSet) :: Set NontermIdent) quants_ ->
                           {-# LINE 996 "./src-ag/Transform.ag" #-}
                           if null quants_
                           then Map.empty
                           else Map.fromList [(nt, quants_) | nt <- Set.toList _namesInontSet]
                           {-# LINE 2144 "dist/build/Transform.hs"#-}
   {-# INLINE rule180 #-}
   {-# LINE 1034 "./src-ag/Transform.ag" #-}
   rule180 = \ ((_namesInontSet) :: Set NontermIdent) ->
                       {-# LINE 1034 "./src-ag/Transform.ag" #-}
                       _namesInontSet
                       {-# LINE 2150 "dist/build/Transform.hs"#-}
   {-# INLINE rule181 #-}
   rule181 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule182 #-}
   rule182 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule183 #-}
   rule183 = \  (_ :: ()) ->
     []
   {-# INLINE rule184 #-}
   rule184 = \  (_ :: ()) ->
     []
   {-# INLINE rule185 #-}
   rule185 = \  (_ :: ()) ->
     []
   {-# INLINE rule186 #-}
   rule186 = \  (_ :: ()) ->
     []
   {-# INLINE rule187 #-}
   rule187 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule188 #-}
   rule188 = \  (_ :: ()) ->
     []
   {-# INLINE rule189 #-}
   rule189 = \  (_ :: ()) ->
     []
   {-# INLINE rule190 #-}
   rule190 = \  (_ :: ()) ->
     []
   {-# INLINE rule191 #-}
   rule191 = \  (_ :: ()) ->
     []
   {-# INLINE rule192 #-}
   rule192 = \ ((_namesIcollectedNames) :: Set Identifier) ->
     _namesIcollectedNames
   {-# INLINE rule193 #-}
   rule193 = \  (_ :: ()) ->
     []
   {-# INLINE rule194 #-}
   rule194 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule195 #-}
   rule195 = \  (_ :: ()) ->
     []
   {-# INLINE rule196 #-}
   rule196 = \  (_ :: ()) ->
     []
   {-# INLINE rule197 #-}
   rule197 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule198 #-}
   rule198 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule199 #-}
   rule199 = \ ((_attrsIerrors) :: Seq Error) ((_namesIerrors) :: Seq Error) ->
     _namesIerrors Seq.>< _attrsIerrors
   {-# INLINE rule200 #-}
   rule200 = \  (_ :: ()) ->
     mzero
   {-# INLINE rule201 #-}
   rule201 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule202 #-}
   rule202 = \  (_ :: ()) ->
     id
   {-# INLINE rule203 #-}
   rule203 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule204 #-}
   rule204 = \  (_ :: ()) ->
     []
   {-# INLINE rule205 #-}
   rule205 = \ ((_attrsIuseMap) :: Map NontermIdent (Map Identifier (String,String,String))) ->
     _attrsIuseMap
   {-# INLINE rule206 #-}
   rule206 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule207 #-}
   rule207 = \ ((_attrsIattrDecls) :: Map NontermIdent (Attributes, Attributes)) ->
     _attrsIattrDecls
   {-# INLINE rule208 #-}
   rule208 = \ ((_attrsIattrs) :: Map NontermIdent (Attributes, Attributes)) ->
     _attrsIattrs
   {-# INLINE rule209 #-}
   rule209 = \ ((_lhsIdefSets) :: Map Identifier (Set NontermIdent,Set Identifier)) ->
     _lhsIdefSets
   {-# INLINE rule210 #-}
   rule210 = \ ((_lhsIallFields) :: DataTypes) ->
     _lhsIallFields
   {-# INLINE rule211 #-}
   rule211 = \ ((_lhsIallNonterminals) :: Set NontermIdent) ->
     _lhsIallNonterminals
   {-# INLINE rule212 #-}
   rule212 = \ ((_lhsIdefinedSets) :: DefinedSets) ->
     _lhsIdefinedSets
   {-# INLINE rule213 #-}
   rule213 = \ ((_lhsIallFields) :: DataTypes) ->
     _lhsIallFields
   {-# INLINE rule214 #-}
   rule214 = \ ((_lhsIallNonterminals) :: Set NontermIdent) ->
     _lhsIallNonterminals
   {-# INLINE rule215 #-}
   rule215 = \ ((_lhsIattrDecls) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIattrDecls
   {-# INLINE rule216 #-}
   rule216 = \ ((_lhsIattrs) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIattrs
   {-# INLINE rule217 #-}
   rule217 = \ ((_lhsIoptions) :: Options) ->
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
         _altsOnts = rule218 _namesInontSet
         _lhsOctxCollect :: ContextMap
         _lhsOctxCollect = rule219 _namesInontSet arg_ctx_
         _lhsOquantCollect :: QuantMap
         _lhsOquantCollect = rule220 _namesInontSet arg_quants_
         _attrsOnts = rule221 _namesInontSet
         _lhsOattrOrderCollect :: AttrOrderMap
         _lhsOattrOrderCollect = rule222 _altsIattrOrderCollect
         _lhsOblocks :: Blocks
         _lhsOblocks = rule223  ()
         _lhsOcollectedArounds :: [ (NontermIdent, ConstructorIdent, [AroundInfo])  ]
         _lhsOcollectedArounds = rule224 _altsIcollectedArounds
         _lhsOcollectedAugments :: [ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]
         _lhsOcollectedAugments = rule225 _altsIcollectedAugments
         _lhsOcollectedConParams :: [(NontermIdent, ConstructorIdent, Set Identifier)]
         _lhsOcollectedConParams = rule226  ()
         _lhsOcollectedConstraints :: [(NontermIdent, ConstructorIdent, [Type])]
         _lhsOcollectedConstraints = rule227  ()
         _lhsOcollectedConstructorsMap :: Map NontermIdent (Set ConstructorIdent)
         _lhsOcollectedConstructorsMap = rule228  ()
         _lhsOcollectedFields :: [(NontermIdent, ConstructorIdent, FieldMap)]
         _lhsOcollectedFields = rule229  ()
         _lhsOcollectedInsts :: [ (NontermIdent, ConstructorIdent, [Identifier]) ]
         _lhsOcollectedInsts = rule230 _altsIcollectedInsts
         _lhsOcollectedMacros :: [(NontermIdent, ConstructorIdent, MaybeMacro)]
         _lhsOcollectedMacros = rule231  ()
         _lhsOcollectedMerges :: [ (NontermIdent, ConstructorIdent, [MergeInfo])   ]
         _lhsOcollectedMerges = rule232 _altsIcollectedMerges
         _lhsOcollectedNames :: Set Identifier
         _lhsOcollectedNames = rule233 _namesIcollectedNames
         _lhsOcollectedRules :: [ (NontermIdent, ConstructorIdent, RuleInfo)]
         _lhsOcollectedRules = rule234 _altsIcollectedRules
         _lhsOcollectedSetNames :: Set Identifier
         _lhsOcollectedSetNames = rule235  ()
         _lhsOcollectedSigs :: [ (NontermIdent, ConstructorIdent, SigInfo) ]
         _lhsOcollectedSigs = rule236 _altsIcollectedSigs
         _lhsOcollectedUniques :: [ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]
         _lhsOcollectedUniques = rule237 _altsIcollectedUniques
         _lhsOconstructorTypeMap :: Map NontermIdent ConstructorType
         _lhsOconstructorTypeMap = rule238  ()
         _lhsOderivings :: Derivings
         _lhsOderivings = rule239  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule240 _altsIerrors _attrsIerrors _namesIerrors
         _lhsOmoduleDecl :: Maybe (String,String,String)
         _lhsOmoduleDecl = rule241  ()
         _lhsOparamsCollect :: ParamMap
         _lhsOparamsCollect = rule242  ()
         _lhsOpragmas :: Options -> Options
         _lhsOpragmas = rule243  ()
         _lhsOsemPragmasCollect :: PragmaMap
         _lhsOsemPragmasCollect = rule244 _altsIsemPragmasCollect
         _lhsOtypeSyns :: TypeSyns
         _lhsOtypeSyns = rule245  ()
         _lhsOuseMap :: Map NontermIdent (Map Identifier (String,String,String))
         _lhsOuseMap = rule246 _attrsIuseMap
         _lhsOwrappers :: Set NontermIdent
         _lhsOwrappers = rule247  ()
         _lhsOattrDecls :: Map NontermIdent (Attributes, Attributes)
         _lhsOattrDecls = rule248 _attrsIattrDecls
         _lhsOattrs :: Map NontermIdent (Attributes, Attributes)
         _lhsOattrs = rule249 _attrsIattrs
         _lhsOdefSets :: Map Identifier (Set NontermIdent,Set Identifier)
         _lhsOdefSets = rule250 _lhsIdefSets
         _namesOallFields = rule251 _lhsIallFields
         _namesOallNonterminals = rule252 _lhsIallNonterminals
         _namesOdefinedSets = rule253 _lhsIdefinedSets
         _attrsOallFields = rule254 _lhsIallFields
         _attrsOallNonterminals = rule255 _lhsIallNonterminals
         _attrsOattrDecls = rule256 _lhsIattrDecls
         _attrsOattrs = rule257 _lhsIattrs
         _attrsOoptions = rule258 _lhsIoptions
         _altsOallAttrDecls = rule259 _lhsIallAttrDecls
         _altsOallAttrs = rule260 _lhsIallAttrs
         _altsOallFields = rule261 _lhsIallFields
         _altsOoptions = rule262 _lhsIoptions
         __result_ = T_Elem_vOut16 _lhsOattrDecls _lhsOattrOrderCollect _lhsOattrs _lhsOblocks _lhsOcollectedArounds _lhsOcollectedAugments _lhsOcollectedConParams _lhsOcollectedConstraints _lhsOcollectedConstructorsMap _lhsOcollectedFields _lhsOcollectedInsts _lhsOcollectedMacros _lhsOcollectedMerges _lhsOcollectedNames _lhsOcollectedRules _lhsOcollectedSetNames _lhsOcollectedSigs _lhsOcollectedUniques _lhsOconstructorTypeMap _lhsOctxCollect _lhsOdefSets _lhsOderivings _lhsOerrors _lhsOmoduleDecl _lhsOparamsCollect _lhsOpragmas _lhsOquantCollect _lhsOsemPragmasCollect _lhsOtypeSyns _lhsOuseMap _lhsOwrappers
         in __result_ )
     in C_Elem_s17 v16
   {-# INLINE rule218 #-}
   {-# LINE 177 "./src-ag/Transform.ag" #-}
   rule218 = \ ((_namesInontSet) :: Set NontermIdent) ->
                      {-# LINE 177 "./src-ag/Transform.ag" #-}
                      _namesInontSet
                      {-# LINE 2359 "dist/build/Transform.hs"#-}
   {-# INLINE rule219 #-}
   {-# LINE 971 "./src-ag/Transform.ag" #-}
   rule219 = \ ((_namesInontSet) :: Set NontermIdent) ctx_ ->
                         {-# LINE 971 "./src-ag/Transform.ag" #-}
                         if null ctx_
                         then Map.empty
                         else Map.fromList [(nt, ctx_) | nt <- Set.toList _namesInontSet]
                         {-# LINE 2367 "dist/build/Transform.hs"#-}
   {-# INLINE rule220 #-}
   {-# LINE 996 "./src-ag/Transform.ag" #-}
   rule220 = \ ((_namesInontSet) :: Set NontermIdent) quants_ ->
                           {-# LINE 996 "./src-ag/Transform.ag" #-}
                           if null quants_
                           then Map.empty
                           else Map.fromList [(nt, quants_) | nt <- Set.toList _namesInontSet]
                           {-# LINE 2375 "dist/build/Transform.hs"#-}
   {-# INLINE rule221 #-}
   {-# LINE 1035 "./src-ag/Transform.ag" #-}
   rule221 = \ ((_namesInontSet) :: Set NontermIdent) ->
                       {-# LINE 1035 "./src-ag/Transform.ag" #-}
                       _namesInontSet
                       {-# LINE 2381 "dist/build/Transform.hs"#-}
   {-# INLINE rule222 #-}
   rule222 = \ ((_altsIattrOrderCollect) :: AttrOrderMap) ->
     _altsIattrOrderCollect
   {-# INLINE rule223 #-}
   rule223 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule224 #-}
   rule224 = \ ((_altsIcollectedArounds) :: [ (NontermIdent, ConstructorIdent, [AroundInfo])  ]) ->
     _altsIcollectedArounds
   {-# INLINE rule225 #-}
   rule225 = \ ((_altsIcollectedAugments) :: [ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]) ->
     _altsIcollectedAugments
   {-# INLINE rule226 #-}
   rule226 = \  (_ :: ()) ->
     []
   {-# INLINE rule227 #-}
   rule227 = \  (_ :: ()) ->
     []
   {-# INLINE rule228 #-}
   rule228 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule229 #-}
   rule229 = \  (_ :: ()) ->
     []
   {-# INLINE rule230 #-}
   rule230 = \ ((_altsIcollectedInsts) :: [ (NontermIdent, ConstructorIdent, [Identifier]) ]) ->
     _altsIcollectedInsts
   {-# INLINE rule231 #-}
   rule231 = \  (_ :: ()) ->
     []
   {-# INLINE rule232 #-}
   rule232 = \ ((_altsIcollectedMerges) :: [ (NontermIdent, ConstructorIdent, [MergeInfo])   ]) ->
     _altsIcollectedMerges
   {-# INLINE rule233 #-}
   rule233 = \ ((_namesIcollectedNames) :: Set Identifier) ->
     _namesIcollectedNames
   {-# INLINE rule234 #-}
   rule234 = \ ((_altsIcollectedRules) :: [ (NontermIdent, ConstructorIdent, RuleInfo)]) ->
     _altsIcollectedRules
   {-# INLINE rule235 #-}
   rule235 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule236 #-}
   rule236 = \ ((_altsIcollectedSigs) :: [ (NontermIdent, ConstructorIdent, SigInfo) ]) ->
     _altsIcollectedSigs
   {-# INLINE rule237 #-}
   rule237 = \ ((_altsIcollectedUniques) :: [ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]) ->
     _altsIcollectedUniques
   {-# INLINE rule238 #-}
   rule238 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule239 #-}
   rule239 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule240 #-}
   rule240 = \ ((_altsIerrors) :: Seq Error) ((_attrsIerrors) :: Seq Error) ((_namesIerrors) :: Seq Error) ->
     _namesIerrors Seq.>< _attrsIerrors Seq.>< _altsIerrors
   {-# INLINE rule241 #-}
   rule241 = \  (_ :: ()) ->
     mzero
   {-# INLINE rule242 #-}
   rule242 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule243 #-}
   rule243 = \  (_ :: ()) ->
     id
   {-# INLINE rule244 #-}
   rule244 = \ ((_altsIsemPragmasCollect) :: PragmaMap) ->
     _altsIsemPragmasCollect
   {-# INLINE rule245 #-}
   rule245 = \  (_ :: ()) ->
     []
   {-# INLINE rule246 #-}
   rule246 = \ ((_attrsIuseMap) :: Map NontermIdent (Map Identifier (String,String,String))) ->
     _attrsIuseMap
   {-# INLINE rule247 #-}
   rule247 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule248 #-}
   rule248 = \ ((_attrsIattrDecls) :: Map NontermIdent (Attributes, Attributes)) ->
     _attrsIattrDecls
   {-# INLINE rule249 #-}
   rule249 = \ ((_attrsIattrs) :: Map NontermIdent (Attributes, Attributes)) ->
     _attrsIattrs
   {-# INLINE rule250 #-}
   rule250 = \ ((_lhsIdefSets) :: Map Identifier (Set NontermIdent,Set Identifier)) ->
     _lhsIdefSets
   {-# INLINE rule251 #-}
   rule251 = \ ((_lhsIallFields) :: DataTypes) ->
     _lhsIallFields
   {-# INLINE rule252 #-}
   rule252 = \ ((_lhsIallNonterminals) :: Set NontermIdent) ->
     _lhsIallNonterminals
   {-# INLINE rule253 #-}
   rule253 = \ ((_lhsIdefinedSets) :: DefinedSets) ->
     _lhsIdefinedSets
   {-# INLINE rule254 #-}
   rule254 = \ ((_lhsIallFields) :: DataTypes) ->
     _lhsIallFields
   {-# INLINE rule255 #-}
   rule255 = \ ((_lhsIallNonterminals) :: Set NontermIdent) ->
     _lhsIallNonterminals
   {-# INLINE rule256 #-}
   rule256 = \ ((_lhsIattrDecls) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIattrDecls
   {-# INLINE rule257 #-}
   rule257 = \ ((_lhsIattrs) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIattrs
   {-# INLINE rule258 #-}
   rule258 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule259 #-}
   rule259 = \ ((_lhsIallAttrDecls) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIallAttrDecls
   {-# INLINE rule260 #-}
   rule260 = \ ((_lhsIallAttrs) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIallAttrs
   {-# INLINE rule261 #-}
   rule261 = \ ((_lhsIallFields) :: DataTypes) ->
     _lhsIallFields
   {-# INLINE rule262 #-}
   rule262 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Elem_Txt #-}
sem_Elem_Txt :: (Pos) -> (BlockKind) -> (Maybe NontermIdent) -> ([String]) -> T_Elem 
sem_Elem_Txt arg_pos_ arg_kind_ arg_mbNt_ arg_lines_ = T_Elem (return st17) where
   {-# NOINLINE st17 #-}
   st17 = let
      v16 :: T_Elem_v16 
      v16 = \ (T_Elem_vIn16 _lhsIallAttrDecls _lhsIallAttrs _lhsIallConstructors _lhsIallFields _lhsIallNonterminals _lhsIattrDecls _lhsIattrs _lhsIdefSets _lhsIdefinedSets _lhsIoptions) -> ( let
         _blockInfo = rule263 arg_kind_ arg_mbNt_
         _blockValue = rule264 arg_lines_ arg_pos_
         _lhsOblocks :: Blocks
         _lhsOblocks = rule265 _blockInfo _blockValue
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule266 _lhsIoptions arg_lines_ arg_pos_
         _lhsOattrOrderCollect :: AttrOrderMap
         _lhsOattrOrderCollect = rule267  ()
         _lhsOcollectedArounds :: [ (NontermIdent, ConstructorIdent, [AroundInfo])  ]
         _lhsOcollectedArounds = rule268  ()
         _lhsOcollectedAugments :: [ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]
         _lhsOcollectedAugments = rule269  ()
         _lhsOcollectedConParams :: [(NontermIdent, ConstructorIdent, Set Identifier)]
         _lhsOcollectedConParams = rule270  ()
         _lhsOcollectedConstraints :: [(NontermIdent, ConstructorIdent, [Type])]
         _lhsOcollectedConstraints = rule271  ()
         _lhsOcollectedConstructorsMap :: Map NontermIdent (Set ConstructorIdent)
         _lhsOcollectedConstructorsMap = rule272  ()
         _lhsOcollectedFields :: [(NontermIdent, ConstructorIdent, FieldMap)]
         _lhsOcollectedFields = rule273  ()
         _lhsOcollectedInsts :: [ (NontermIdent, ConstructorIdent, [Identifier]) ]
         _lhsOcollectedInsts = rule274  ()
         _lhsOcollectedMacros :: [(NontermIdent, ConstructorIdent, MaybeMacro)]
         _lhsOcollectedMacros = rule275  ()
         _lhsOcollectedMerges :: [ (NontermIdent, ConstructorIdent, [MergeInfo])   ]
         _lhsOcollectedMerges = rule276  ()
         _lhsOcollectedNames :: Set Identifier
         _lhsOcollectedNames = rule277  ()
         _lhsOcollectedRules :: [ (NontermIdent, ConstructorIdent, RuleInfo)]
         _lhsOcollectedRules = rule278  ()
         _lhsOcollectedSetNames :: Set Identifier
         _lhsOcollectedSetNames = rule279  ()
         _lhsOcollectedSigs :: [ (NontermIdent, ConstructorIdent, SigInfo) ]
         _lhsOcollectedSigs = rule280  ()
         _lhsOcollectedUniques :: [ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]
         _lhsOcollectedUniques = rule281  ()
         _lhsOconstructorTypeMap :: Map NontermIdent ConstructorType
         _lhsOconstructorTypeMap = rule282  ()
         _lhsOctxCollect :: ContextMap
         _lhsOctxCollect = rule283  ()
         _lhsOderivings :: Derivings
         _lhsOderivings = rule284  ()
         _lhsOmoduleDecl :: Maybe (String,String,String)
         _lhsOmoduleDecl = rule285  ()
         _lhsOparamsCollect :: ParamMap
         _lhsOparamsCollect = rule286  ()
         _lhsOpragmas :: Options -> Options
         _lhsOpragmas = rule287  ()
         _lhsOquantCollect :: QuantMap
         _lhsOquantCollect = rule288  ()
         _lhsOsemPragmasCollect :: PragmaMap
         _lhsOsemPragmasCollect = rule289  ()
         _lhsOtypeSyns :: TypeSyns
         _lhsOtypeSyns = rule290  ()
         _lhsOuseMap :: Map NontermIdent (Map Identifier (String,String,String))
         _lhsOuseMap = rule291  ()
         _lhsOwrappers :: Set NontermIdent
         _lhsOwrappers = rule292  ()
         _lhsOattrDecls :: Map NontermIdent (Attributes, Attributes)
         _lhsOattrDecls = rule293 _lhsIattrDecls
         _lhsOattrs :: Map NontermIdent (Attributes, Attributes)
         _lhsOattrs = rule294 _lhsIattrs
         _lhsOdefSets :: Map Identifier (Set NontermIdent,Set Identifier)
         _lhsOdefSets = rule295 _lhsIdefSets
         __result_ = T_Elem_vOut16 _lhsOattrDecls _lhsOattrOrderCollect _lhsOattrs _lhsOblocks _lhsOcollectedArounds _lhsOcollectedAugments _lhsOcollectedConParams _lhsOcollectedConstraints _lhsOcollectedConstructorsMap _lhsOcollectedFields _lhsOcollectedInsts _lhsOcollectedMacros _lhsOcollectedMerges _lhsOcollectedNames _lhsOcollectedRules _lhsOcollectedSetNames _lhsOcollectedSigs _lhsOcollectedUniques _lhsOconstructorTypeMap _lhsOctxCollect _lhsOdefSets _lhsOderivings _lhsOerrors _lhsOmoduleDecl _lhsOparamsCollect _lhsOpragmas _lhsOquantCollect _lhsOsemPragmasCollect _lhsOtypeSyns _lhsOuseMap _lhsOwrappers
         in __result_ )
     in C_Elem_s17 v16
   {-# INLINE rule263 #-}
   {-# LINE 186 "./src-ag/Transform.ag" #-}
   rule263 = \ kind_ mbNt_ ->
                            {-# LINE 186 "./src-ag/Transform.ag" #-}
                            ( kind_
                            , mbNt_
                            )
                            {-# LINE 2586 "dist/build/Transform.hs"#-}
   {-# INLINE rule264 #-}
   {-# LINE 189 "./src-ag/Transform.ag" #-}
   rule264 = \ lines_ pos_ ->
                            {-# LINE 189 "./src-ag/Transform.ag" #-}
                            [(lines_, pos_)]
                            {-# LINE 2592 "dist/build/Transform.hs"#-}
   {-# INLINE rule265 #-}
   {-# LINE 190 "./src-ag/Transform.ag" #-}
   rule265 = \ _blockInfo _blockValue ->
                            {-# LINE 190 "./src-ag/Transform.ag" #-}
                            Map.singleton _blockInfo     _blockValue
                            {-# LINE 2598 "dist/build/Transform.hs"#-}
   {-# INLINE rule266 #-}
   {-# LINE 191 "./src-ag/Transform.ag" #-}
   rule266 = \ ((_lhsIoptions) :: Options) lines_ pos_ ->
                            {-# LINE 191 "./src-ag/Transform.ag" #-}
                            if checkParseBlock _lhsIoptions
                            then let ex  = Expression pos_ tks
                                     tks = [tk]
                                     tk  = HsToken (unlines lines_) pos_
                                 in Seq.fromList $ checkBlock $ ex
                            else Seq.empty
                            {-# LINE 2609 "dist/build/Transform.hs"#-}
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
     Map.empty
   {-# INLINE rule273 #-}
   rule273 = \  (_ :: ()) ->
     []
   {-# INLINE rule274 #-}
   rule274 = \  (_ :: ()) ->
     []
   {-# INLINE rule275 #-}
   rule275 = \  (_ :: ()) ->
     []
   {-# INLINE rule276 #-}
   rule276 = \  (_ :: ()) ->
     []
   {-# INLINE rule277 #-}
   rule277 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule278 #-}
   rule278 = \  (_ :: ()) ->
     []
   {-# INLINE rule279 #-}
   rule279 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule280 #-}
   rule280 = \  (_ :: ()) ->
     []
   {-# INLINE rule281 #-}
   rule281 = \  (_ :: ()) ->
     []
   {-# INLINE rule282 #-}
   rule282 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule283 #-}
   rule283 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule284 #-}
   rule284 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule285 #-}
   rule285 = \  (_ :: ()) ->
     mzero
   {-# INLINE rule286 #-}
   rule286 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule287 #-}
   rule287 = \  (_ :: ()) ->
     id
   {-# INLINE rule288 #-}
   rule288 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule289 #-}
   rule289 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule290 #-}
   rule290 = \  (_ :: ()) ->
     []
   {-# INLINE rule291 #-}
   rule291 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule292 #-}
   rule292 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule293 #-}
   rule293 = \ ((_lhsIattrDecls) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIattrDecls
   {-# INLINE rule294 #-}
   rule294 = \ ((_lhsIattrs) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIattrs
   {-# INLINE rule295 #-}
   rule295 = \ ((_lhsIdefSets) :: Map Identifier (Set NontermIdent,Set Identifier)) ->
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
         _lhsOcollectedSetNames = rule296 arg_name_
         (_defSets2,_errs) = rule297 _lhsIallNonterminals _lhsIdefSets _setIcollectedNames _setInontSet arg_merge_ arg_name_
         _lhsOdefSets :: Map Identifier (Set NontermIdent,Set Identifier)
         _lhsOdefSets = rule298 _defSets2
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule299 _errs _setIerrors
         _lhsOattrOrderCollect :: AttrOrderMap
         _lhsOattrOrderCollect = rule300  ()
         _lhsOblocks :: Blocks
         _lhsOblocks = rule301  ()
         _lhsOcollectedArounds :: [ (NontermIdent, ConstructorIdent, [AroundInfo])  ]
         _lhsOcollectedArounds = rule302  ()
         _lhsOcollectedAugments :: [ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]
         _lhsOcollectedAugments = rule303  ()
         _lhsOcollectedConParams :: [(NontermIdent, ConstructorIdent, Set Identifier)]
         _lhsOcollectedConParams = rule304  ()
         _lhsOcollectedConstraints :: [(NontermIdent, ConstructorIdent, [Type])]
         _lhsOcollectedConstraints = rule305  ()
         _lhsOcollectedConstructorsMap :: Map NontermIdent (Set ConstructorIdent)
         _lhsOcollectedConstructorsMap = rule306  ()
         _lhsOcollectedFields :: [(NontermIdent, ConstructorIdent, FieldMap)]
         _lhsOcollectedFields = rule307  ()
         _lhsOcollectedInsts :: [ (NontermIdent, ConstructorIdent, [Identifier]) ]
         _lhsOcollectedInsts = rule308  ()
         _lhsOcollectedMacros :: [(NontermIdent, ConstructorIdent, MaybeMacro)]
         _lhsOcollectedMacros = rule309  ()
         _lhsOcollectedMerges :: [ (NontermIdent, ConstructorIdent, [MergeInfo])   ]
         _lhsOcollectedMerges = rule310  ()
         _lhsOcollectedNames :: Set Identifier
         _lhsOcollectedNames = rule311 _setIcollectedNames
         _lhsOcollectedRules :: [ (NontermIdent, ConstructorIdent, RuleInfo)]
         _lhsOcollectedRules = rule312  ()
         _lhsOcollectedSigs :: [ (NontermIdent, ConstructorIdent, SigInfo) ]
         _lhsOcollectedSigs = rule313  ()
         _lhsOcollectedUniques :: [ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]
         _lhsOcollectedUniques = rule314  ()
         _lhsOconstructorTypeMap :: Map NontermIdent ConstructorType
         _lhsOconstructorTypeMap = rule315  ()
         _lhsOctxCollect :: ContextMap
         _lhsOctxCollect = rule316  ()
         _lhsOderivings :: Derivings
         _lhsOderivings = rule317  ()
         _lhsOmoduleDecl :: Maybe (String,String,String)
         _lhsOmoduleDecl = rule318  ()
         _lhsOparamsCollect :: ParamMap
         _lhsOparamsCollect = rule319  ()
         _lhsOpragmas :: Options -> Options
         _lhsOpragmas = rule320  ()
         _lhsOquantCollect :: QuantMap
         _lhsOquantCollect = rule321  ()
         _lhsOsemPragmasCollect :: PragmaMap
         _lhsOsemPragmasCollect = rule322  ()
         _lhsOtypeSyns :: TypeSyns
         _lhsOtypeSyns = rule323  ()
         _lhsOuseMap :: Map NontermIdent (Map Identifier (String,String,String))
         _lhsOuseMap = rule324  ()
         _lhsOwrappers :: Set NontermIdent
         _lhsOwrappers = rule325  ()
         _lhsOattrDecls :: Map NontermIdent (Attributes, Attributes)
         _lhsOattrDecls = rule326 _lhsIattrDecls
         _lhsOattrs :: Map NontermIdent (Attributes, Attributes)
         _lhsOattrs = rule327 _lhsIattrs
         _setOallFields = rule328 _lhsIallFields
         _setOallNonterminals = rule329 _lhsIallNonterminals
         _setOdefinedSets = rule330 _lhsIdefinedSets
         __result_ = T_Elem_vOut16 _lhsOattrDecls _lhsOattrOrderCollect _lhsOattrs _lhsOblocks _lhsOcollectedArounds _lhsOcollectedAugments _lhsOcollectedConParams _lhsOcollectedConstraints _lhsOcollectedConstructorsMap _lhsOcollectedFields _lhsOcollectedInsts _lhsOcollectedMacros _lhsOcollectedMerges _lhsOcollectedNames _lhsOcollectedRules _lhsOcollectedSetNames _lhsOcollectedSigs _lhsOcollectedUniques _lhsOconstructorTypeMap _lhsOctxCollect _lhsOdefSets _lhsOderivings _lhsOerrors _lhsOmoduleDecl _lhsOparamsCollect _lhsOpragmas _lhsOquantCollect _lhsOsemPragmasCollect _lhsOtypeSyns _lhsOuseMap _lhsOwrappers
         in __result_ )
     in C_Elem_s17 v16
   {-# INLINE rule296 #-}
   {-# LINE 597 "./src-ag/Transform.ag" #-}
   rule296 = \ name_ ->
                                   {-# LINE 597 "./src-ag/Transform.ag" #-}
                                   Set.singleton name_
                                   {-# LINE 2780 "dist/build/Transform.hs"#-}
   {-# INLINE rule297 #-}
   {-# LINE 714 "./src-ag/Transform.ag" #-}
   rule297 = \ ((_lhsIallNonterminals) :: Set NontermIdent) ((_lhsIdefSets) :: Map Identifier (Set NontermIdent,Set Identifier)) ((_setIcollectedNames) :: Set Identifier) ((_setInontSet) :: Set NontermIdent) merge_ name_ ->
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
                                {-# LINE 2799 "dist/build/Transform.hs"#-}
   {-# INLINE rule298 #-}
   {-# LINE 728 "./src-ag/Transform.ag" #-}
   rule298 = \ _defSets2 ->
                                {-# LINE 728 "./src-ag/Transform.ag" #-}
                                _defSets2
                                {-# LINE 2805 "dist/build/Transform.hs"#-}
   {-# INLINE rule299 #-}
   {-# LINE 729 "./src-ag/Transform.ag" #-}
   rule299 = \ _errs ((_setIerrors) :: Seq Error) ->
                                {-# LINE 729 "./src-ag/Transform.ag" #-}
                                _errs >< _setIerrors
                                {-# LINE 2811 "dist/build/Transform.hs"#-}
   {-# INLINE rule300 #-}
   rule300 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule301 #-}
   rule301 = \  (_ :: ()) ->
     Map.empty
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
   rule305 = \  (_ :: ()) ->
     []
   {-# INLINE rule306 #-}
   rule306 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule307 #-}
   rule307 = \  (_ :: ()) ->
     []
   {-# INLINE rule308 #-}
   rule308 = \  (_ :: ()) ->
     []
   {-# INLINE rule309 #-}
   rule309 = \  (_ :: ()) ->
     []
   {-# INLINE rule310 #-}
   rule310 = \  (_ :: ()) ->
     []
   {-# INLINE rule311 #-}
   rule311 = \ ((_setIcollectedNames) :: Set Identifier) ->
     _setIcollectedNames
   {-# INLINE rule312 #-}
   rule312 = \  (_ :: ()) ->
     []
   {-# INLINE rule313 #-}
   rule313 = \  (_ :: ()) ->
     []
   {-# INLINE rule314 #-}
   rule314 = \  (_ :: ()) ->
     []
   {-# INLINE rule315 #-}
   rule315 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule316 #-}
   rule316 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule317 #-}
   rule317 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule318 #-}
   rule318 = \  (_ :: ()) ->
     mzero
   {-# INLINE rule319 #-}
   rule319 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule320 #-}
   rule320 = \  (_ :: ()) ->
     id
   {-# INLINE rule321 #-}
   rule321 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule322 #-}
   rule322 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule323 #-}
   rule323 = \  (_ :: ()) ->
     []
   {-# INLINE rule324 #-}
   rule324 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule325 #-}
   rule325 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule326 #-}
   rule326 = \ ((_lhsIattrDecls) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIattrDecls
   {-# INLINE rule327 #-}
   rule327 = \ ((_lhsIattrs) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIattrs
   {-# INLINE rule328 #-}
   rule328 = \ ((_lhsIallFields) :: DataTypes) ->
     _lhsIallFields
   {-# INLINE rule329 #-}
   rule329 = \ ((_lhsIallNonterminals) :: Set NontermIdent) ->
     _lhsIallNonterminals
   {-# INLINE rule330 #-}
   rule330 = \ ((_lhsIdefinedSets) :: DefinedSets) ->
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
         _lhsOderivings = rule331 _setInontSet arg_classes_
         _lhsOattrOrderCollect :: AttrOrderMap
         _lhsOattrOrderCollect = rule332  ()
         _lhsOblocks :: Blocks
         _lhsOblocks = rule333  ()
         _lhsOcollectedArounds :: [ (NontermIdent, ConstructorIdent, [AroundInfo])  ]
         _lhsOcollectedArounds = rule334  ()
         _lhsOcollectedAugments :: [ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]
         _lhsOcollectedAugments = rule335  ()
         _lhsOcollectedConParams :: [(NontermIdent, ConstructorIdent, Set Identifier)]
         _lhsOcollectedConParams = rule336  ()
         _lhsOcollectedConstraints :: [(NontermIdent, ConstructorIdent, [Type])]
         _lhsOcollectedConstraints = rule337  ()
         _lhsOcollectedConstructorsMap :: Map NontermIdent (Set ConstructorIdent)
         _lhsOcollectedConstructorsMap = rule338  ()
         _lhsOcollectedFields :: [(NontermIdent, ConstructorIdent, FieldMap)]
         _lhsOcollectedFields = rule339  ()
         _lhsOcollectedInsts :: [ (NontermIdent, ConstructorIdent, [Identifier]) ]
         _lhsOcollectedInsts = rule340  ()
         _lhsOcollectedMacros :: [(NontermIdent, ConstructorIdent, MaybeMacro)]
         _lhsOcollectedMacros = rule341  ()
         _lhsOcollectedMerges :: [ (NontermIdent, ConstructorIdent, [MergeInfo])   ]
         _lhsOcollectedMerges = rule342  ()
         _lhsOcollectedNames :: Set Identifier
         _lhsOcollectedNames = rule343 _setIcollectedNames
         _lhsOcollectedRules :: [ (NontermIdent, ConstructorIdent, RuleInfo)]
         _lhsOcollectedRules = rule344  ()
         _lhsOcollectedSetNames :: Set Identifier
         _lhsOcollectedSetNames = rule345  ()
         _lhsOcollectedSigs :: [ (NontermIdent, ConstructorIdent, SigInfo) ]
         _lhsOcollectedSigs = rule346  ()
         _lhsOcollectedUniques :: [ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]
         _lhsOcollectedUniques = rule347  ()
         _lhsOconstructorTypeMap :: Map NontermIdent ConstructorType
         _lhsOconstructorTypeMap = rule348  ()
         _lhsOctxCollect :: ContextMap
         _lhsOctxCollect = rule349  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule350 _setIerrors
         _lhsOmoduleDecl :: Maybe (String,String,String)
         _lhsOmoduleDecl = rule351  ()
         _lhsOparamsCollect :: ParamMap
         _lhsOparamsCollect = rule352  ()
         _lhsOpragmas :: Options -> Options
         _lhsOpragmas = rule353  ()
         _lhsOquantCollect :: QuantMap
         _lhsOquantCollect = rule354  ()
         _lhsOsemPragmasCollect :: PragmaMap
         _lhsOsemPragmasCollect = rule355  ()
         _lhsOtypeSyns :: TypeSyns
         _lhsOtypeSyns = rule356  ()
         _lhsOuseMap :: Map NontermIdent (Map Identifier (String,String,String))
         _lhsOuseMap = rule357  ()
         _lhsOwrappers :: Set NontermIdent
         _lhsOwrappers = rule358  ()
         _lhsOattrDecls :: Map NontermIdent (Attributes, Attributes)
         _lhsOattrDecls = rule359 _lhsIattrDecls
         _lhsOattrs :: Map NontermIdent (Attributes, Attributes)
         _lhsOattrs = rule360 _lhsIattrs
         _lhsOdefSets :: Map Identifier (Set NontermIdent,Set Identifier)
         _lhsOdefSets = rule361 _lhsIdefSets
         _setOallFields = rule362 _lhsIallFields
         _setOallNonterminals = rule363 _lhsIallNonterminals
         _setOdefinedSets = rule364 _lhsIdefinedSets
         __result_ = T_Elem_vOut16 _lhsOattrDecls _lhsOattrOrderCollect _lhsOattrs _lhsOblocks _lhsOcollectedArounds _lhsOcollectedAugments _lhsOcollectedConParams _lhsOcollectedConstraints _lhsOcollectedConstructorsMap _lhsOcollectedFields _lhsOcollectedInsts _lhsOcollectedMacros _lhsOcollectedMerges _lhsOcollectedNames _lhsOcollectedRules _lhsOcollectedSetNames _lhsOcollectedSigs _lhsOcollectedUniques _lhsOconstructorTypeMap _lhsOctxCollect _lhsOdefSets _lhsOderivings _lhsOerrors _lhsOmoduleDecl _lhsOparamsCollect _lhsOpragmas _lhsOquantCollect _lhsOsemPragmasCollect _lhsOtypeSyns _lhsOuseMap _lhsOwrappers
         in __result_ )
     in C_Elem_s17 v16
   {-# INLINE rule331 #-}
   {-# LINE 1017 "./src-ag/Transform.ag" #-}
   rule331 = \ ((_setInontSet) :: Set NontermIdent) classes_ ->
                               {-# LINE 1017 "./src-ag/Transform.ag" #-}
                               Map.fromList [(nt,Set.fromList classes_) | nt <- Set.toList _setInontSet]
                               {-# LINE 2987 "dist/build/Transform.hs"#-}
   {-# INLINE rule332 #-}
   rule332 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule333 #-}
   rule333 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule334 #-}
   rule334 = \  (_ :: ()) ->
     []
   {-# INLINE rule335 #-}
   rule335 = \  (_ :: ()) ->
     []
   {-# INLINE rule336 #-}
   rule336 = \  (_ :: ()) ->
     []
   {-# INLINE rule337 #-}
   rule337 = \  (_ :: ()) ->
     []
   {-# INLINE rule338 #-}
   rule338 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule339 #-}
   rule339 = \  (_ :: ()) ->
     []
   {-# INLINE rule340 #-}
   rule340 = \  (_ :: ()) ->
     []
   {-# INLINE rule341 #-}
   rule341 = \  (_ :: ()) ->
     []
   {-# INLINE rule342 #-}
   rule342 = \  (_ :: ()) ->
     []
   {-# INLINE rule343 #-}
   rule343 = \ ((_setIcollectedNames) :: Set Identifier) ->
     _setIcollectedNames
   {-# INLINE rule344 #-}
   rule344 = \  (_ :: ()) ->
     []
   {-# INLINE rule345 #-}
   rule345 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule346 #-}
   rule346 = \  (_ :: ()) ->
     []
   {-# INLINE rule347 #-}
   rule347 = \  (_ :: ()) ->
     []
   {-# INLINE rule348 #-}
   rule348 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule349 #-}
   rule349 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule350 #-}
   rule350 = \ ((_setIerrors) :: Seq Error) ->
     _setIerrors
   {-# INLINE rule351 #-}
   rule351 = \  (_ :: ()) ->
     mzero
   {-# INLINE rule352 #-}
   rule352 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule353 #-}
   rule353 = \  (_ :: ()) ->
     id
   {-# INLINE rule354 #-}
   rule354 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule355 #-}
   rule355 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule356 #-}
   rule356 = \  (_ :: ()) ->
     []
   {-# INLINE rule357 #-}
   rule357 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule358 #-}
   rule358 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule359 #-}
   rule359 = \ ((_lhsIattrDecls) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIattrDecls
   {-# INLINE rule360 #-}
   rule360 = \ ((_lhsIattrs) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIattrs
   {-# INLINE rule361 #-}
   rule361 = \ ((_lhsIdefSets) :: Map Identifier (Set NontermIdent,Set Identifier)) ->
     _lhsIdefSets
   {-# INLINE rule362 #-}
   rule362 = \ ((_lhsIallFields) :: DataTypes) ->
     _lhsIallFields
   {-# INLINE rule363 #-}
   rule363 = \ ((_lhsIallNonterminals) :: Set NontermIdent) ->
     _lhsIallNonterminals
   {-# INLINE rule364 #-}
   rule364 = \ ((_lhsIdefinedSets) :: DefinedSets) ->
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
         _lhsOwrappers = rule365 _setInontSet
         _lhsOattrOrderCollect :: AttrOrderMap
         _lhsOattrOrderCollect = rule366  ()
         _lhsOblocks :: Blocks
         _lhsOblocks = rule367  ()
         _lhsOcollectedArounds :: [ (NontermIdent, ConstructorIdent, [AroundInfo])  ]
         _lhsOcollectedArounds = rule368  ()
         _lhsOcollectedAugments :: [ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]
         _lhsOcollectedAugments = rule369  ()
         _lhsOcollectedConParams :: [(NontermIdent, ConstructorIdent, Set Identifier)]
         _lhsOcollectedConParams = rule370  ()
         _lhsOcollectedConstraints :: [(NontermIdent, ConstructorIdent, [Type])]
         _lhsOcollectedConstraints = rule371  ()
         _lhsOcollectedConstructorsMap :: Map NontermIdent (Set ConstructorIdent)
         _lhsOcollectedConstructorsMap = rule372  ()
         _lhsOcollectedFields :: [(NontermIdent, ConstructorIdent, FieldMap)]
         _lhsOcollectedFields = rule373  ()
         _lhsOcollectedInsts :: [ (NontermIdent, ConstructorIdent, [Identifier]) ]
         _lhsOcollectedInsts = rule374  ()
         _lhsOcollectedMacros :: [(NontermIdent, ConstructorIdent, MaybeMacro)]
         _lhsOcollectedMacros = rule375  ()
         _lhsOcollectedMerges :: [ (NontermIdent, ConstructorIdent, [MergeInfo])   ]
         _lhsOcollectedMerges = rule376  ()
         _lhsOcollectedNames :: Set Identifier
         _lhsOcollectedNames = rule377 _setIcollectedNames
         _lhsOcollectedRules :: [ (NontermIdent, ConstructorIdent, RuleInfo)]
         _lhsOcollectedRules = rule378  ()
         _lhsOcollectedSetNames :: Set Identifier
         _lhsOcollectedSetNames = rule379  ()
         _lhsOcollectedSigs :: [ (NontermIdent, ConstructorIdent, SigInfo) ]
         _lhsOcollectedSigs = rule380  ()
         _lhsOcollectedUniques :: [ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]
         _lhsOcollectedUniques = rule381  ()
         _lhsOconstructorTypeMap :: Map NontermIdent ConstructorType
         _lhsOconstructorTypeMap = rule382  ()
         _lhsOctxCollect :: ContextMap
         _lhsOctxCollect = rule383  ()
         _lhsOderivings :: Derivings
         _lhsOderivings = rule384  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule385 _setIerrors
         _lhsOmoduleDecl :: Maybe (String,String,String)
         _lhsOmoduleDecl = rule386  ()
         _lhsOparamsCollect :: ParamMap
         _lhsOparamsCollect = rule387  ()
         _lhsOpragmas :: Options -> Options
         _lhsOpragmas = rule388  ()
         _lhsOquantCollect :: QuantMap
         _lhsOquantCollect = rule389  ()
         _lhsOsemPragmasCollect :: PragmaMap
         _lhsOsemPragmasCollect = rule390  ()
         _lhsOtypeSyns :: TypeSyns
         _lhsOtypeSyns = rule391  ()
         _lhsOuseMap :: Map NontermIdent (Map Identifier (String,String,String))
         _lhsOuseMap = rule392  ()
         _lhsOattrDecls :: Map NontermIdent (Attributes, Attributes)
         _lhsOattrDecls = rule393 _lhsIattrDecls
         _lhsOattrs :: Map NontermIdent (Attributes, Attributes)
         _lhsOattrs = rule394 _lhsIattrs
         _lhsOdefSets :: Map Identifier (Set NontermIdent,Set Identifier)
         _lhsOdefSets = rule395 _lhsIdefSets
         _setOallFields = rule396 _lhsIallFields
         _setOallNonterminals = rule397 _lhsIallNonterminals
         _setOdefinedSets = rule398 _lhsIdefinedSets
         __result_ = T_Elem_vOut16 _lhsOattrDecls _lhsOattrOrderCollect _lhsOattrs _lhsOblocks _lhsOcollectedArounds _lhsOcollectedAugments _lhsOcollectedConParams _lhsOcollectedConstraints _lhsOcollectedConstructorsMap _lhsOcollectedFields _lhsOcollectedInsts _lhsOcollectedMacros _lhsOcollectedMerges _lhsOcollectedNames _lhsOcollectedRules _lhsOcollectedSetNames _lhsOcollectedSigs _lhsOcollectedUniques _lhsOconstructorTypeMap _lhsOctxCollect _lhsOdefSets _lhsOderivings _lhsOerrors _lhsOmoduleDecl _lhsOparamsCollect _lhsOpragmas _lhsOquantCollect _lhsOsemPragmasCollect _lhsOtypeSyns _lhsOuseMap _lhsOwrappers
         in __result_ )
     in C_Elem_s17 v16
   {-# INLINE rule365 #-}
   {-# LINE 789 "./src-ag/Transform.ag" #-}
   rule365 = \ ((_setInontSet) :: Set NontermIdent) ->
                             {-# LINE 789 "./src-ag/Transform.ag" #-}
                             _setInontSet
                             {-# LINE 3169 "dist/build/Transform.hs"#-}
   {-# INLINE rule366 #-}
   rule366 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule367 #-}
   rule367 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule368 #-}
   rule368 = \  (_ :: ()) ->
     []
   {-# INLINE rule369 #-}
   rule369 = \  (_ :: ()) ->
     []
   {-# INLINE rule370 #-}
   rule370 = \  (_ :: ()) ->
     []
   {-# INLINE rule371 #-}
   rule371 = \  (_ :: ()) ->
     []
   {-# INLINE rule372 #-}
   rule372 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule373 #-}
   rule373 = \  (_ :: ()) ->
     []
   {-# INLINE rule374 #-}
   rule374 = \  (_ :: ()) ->
     []
   {-# INLINE rule375 #-}
   rule375 = \  (_ :: ()) ->
     []
   {-# INLINE rule376 #-}
   rule376 = \  (_ :: ()) ->
     []
   {-# INLINE rule377 #-}
   rule377 = \ ((_setIcollectedNames) :: Set Identifier) ->
     _setIcollectedNames
   {-# INLINE rule378 #-}
   rule378 = \  (_ :: ()) ->
     []
   {-# INLINE rule379 #-}
   rule379 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule380 #-}
   rule380 = \  (_ :: ()) ->
     []
   {-# INLINE rule381 #-}
   rule381 = \  (_ :: ()) ->
     []
   {-# INLINE rule382 #-}
   rule382 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule383 #-}
   rule383 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule384 #-}
   rule384 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule385 #-}
   rule385 = \ ((_setIerrors) :: Seq Error) ->
     _setIerrors
   {-# INLINE rule386 #-}
   rule386 = \  (_ :: ()) ->
     mzero
   {-# INLINE rule387 #-}
   rule387 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule388 #-}
   rule388 = \  (_ :: ()) ->
     id
   {-# INLINE rule389 #-}
   rule389 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule390 #-}
   rule390 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule391 #-}
   rule391 = \  (_ :: ()) ->
     []
   {-# INLINE rule392 #-}
   rule392 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule393 #-}
   rule393 = \ ((_lhsIattrDecls) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIattrDecls
   {-# INLINE rule394 #-}
   rule394 = \ ((_lhsIattrs) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIattrs
   {-# INLINE rule395 #-}
   rule395 = \ ((_lhsIdefSets) :: Map Identifier (Set NontermIdent,Set Identifier)) ->
     _lhsIdefSets
   {-# INLINE rule396 #-}
   rule396 = \ ((_lhsIallFields) :: DataTypes) ->
     _lhsIallFields
   {-# INLINE rule397 #-}
   rule397 = \ ((_lhsIallNonterminals) :: Set NontermIdent) ->
     _lhsIallNonterminals
   {-# INLINE rule398 #-}
   rule398 = \ ((_lhsIdefinedSets) :: DefinedSets) ->
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
         _lhsOpragmas = rule399 _setInontSet
         _lhsOattrOrderCollect :: AttrOrderMap
         _lhsOattrOrderCollect = rule400  ()
         _lhsOblocks :: Blocks
         _lhsOblocks = rule401  ()
         _lhsOcollectedArounds :: [ (NontermIdent, ConstructorIdent, [AroundInfo])  ]
         _lhsOcollectedArounds = rule402  ()
         _lhsOcollectedAugments :: [ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]
         _lhsOcollectedAugments = rule403  ()
         _lhsOcollectedConParams :: [(NontermIdent, ConstructorIdent, Set Identifier)]
         _lhsOcollectedConParams = rule404  ()
         _lhsOcollectedConstraints :: [(NontermIdent, ConstructorIdent, [Type])]
         _lhsOcollectedConstraints = rule405  ()
         _lhsOcollectedConstructorsMap :: Map NontermIdent (Set ConstructorIdent)
         _lhsOcollectedConstructorsMap = rule406  ()
         _lhsOcollectedFields :: [(NontermIdent, ConstructorIdent, FieldMap)]
         _lhsOcollectedFields = rule407  ()
         _lhsOcollectedInsts :: [ (NontermIdent, ConstructorIdent, [Identifier]) ]
         _lhsOcollectedInsts = rule408  ()
         _lhsOcollectedMacros :: [(NontermIdent, ConstructorIdent, MaybeMacro)]
         _lhsOcollectedMacros = rule409  ()
         _lhsOcollectedMerges :: [ (NontermIdent, ConstructorIdent, [MergeInfo])   ]
         _lhsOcollectedMerges = rule410  ()
         _lhsOcollectedNames :: Set Identifier
         _lhsOcollectedNames = rule411 _setIcollectedNames
         _lhsOcollectedRules :: [ (NontermIdent, ConstructorIdent, RuleInfo)]
         _lhsOcollectedRules = rule412  ()
         _lhsOcollectedSetNames :: Set Identifier
         _lhsOcollectedSetNames = rule413  ()
         _lhsOcollectedSigs :: [ (NontermIdent, ConstructorIdent, SigInfo) ]
         _lhsOcollectedSigs = rule414  ()
         _lhsOcollectedUniques :: [ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]
         _lhsOcollectedUniques = rule415  ()
         _lhsOconstructorTypeMap :: Map NontermIdent ConstructorType
         _lhsOconstructorTypeMap = rule416  ()
         _lhsOctxCollect :: ContextMap
         _lhsOctxCollect = rule417  ()
         _lhsOderivings :: Derivings
         _lhsOderivings = rule418  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule419 _setIerrors
         _lhsOmoduleDecl :: Maybe (String,String,String)
         _lhsOmoduleDecl = rule420  ()
         _lhsOparamsCollect :: ParamMap
         _lhsOparamsCollect = rule421  ()
         _lhsOquantCollect :: QuantMap
         _lhsOquantCollect = rule422  ()
         _lhsOsemPragmasCollect :: PragmaMap
         _lhsOsemPragmasCollect = rule423  ()
         _lhsOtypeSyns :: TypeSyns
         _lhsOtypeSyns = rule424  ()
         _lhsOuseMap :: Map NontermIdent (Map Identifier (String,String,String))
         _lhsOuseMap = rule425  ()
         _lhsOwrappers :: Set NontermIdent
         _lhsOwrappers = rule426  ()
         _lhsOattrDecls :: Map NontermIdent (Attributes, Attributes)
         _lhsOattrDecls = rule427 _lhsIattrDecls
         _lhsOattrs :: Map NontermIdent (Attributes, Attributes)
         _lhsOattrs = rule428 _lhsIattrs
         _lhsOdefSets :: Map Identifier (Set NontermIdent,Set Identifier)
         _lhsOdefSets = rule429 _lhsIdefSets
         _setOallFields = rule430 _lhsIallFields
         _setOallNonterminals = rule431 _lhsIallNonterminals
         _setOdefinedSets = rule432 _lhsIdefinedSets
         __result_ = T_Elem_vOut16 _lhsOattrDecls _lhsOattrOrderCollect _lhsOattrs _lhsOblocks _lhsOcollectedArounds _lhsOcollectedAugments _lhsOcollectedConParams _lhsOcollectedConstraints _lhsOcollectedConstructorsMap _lhsOcollectedFields _lhsOcollectedInsts _lhsOcollectedMacros _lhsOcollectedMerges _lhsOcollectedNames _lhsOcollectedRules _lhsOcollectedSetNames _lhsOcollectedSigs _lhsOcollectedUniques _lhsOconstructorTypeMap _lhsOctxCollect _lhsOdefSets _lhsOderivings _lhsOerrors _lhsOmoduleDecl _lhsOparamsCollect _lhsOpragmas _lhsOquantCollect _lhsOsemPragmasCollect _lhsOtypeSyns _lhsOuseMap _lhsOwrappers
         in __result_ )
     in C_Elem_s17 v16
   {-# INLINE rule399 #-}
   {-# LINE 796 "./src-ag/Transform.ag" #-}
   rule399 = \ ((_setInontSet) :: Set NontermIdent) ->
                             {-# LINE 796 "./src-ag/Transform.ag" #-}
                             \o -> o { nocatas = _setInontSet `Set.union` nocatas o }
                             {-# LINE 3351 "dist/build/Transform.hs"#-}
   {-# INLINE rule400 #-}
   rule400 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule401 #-}
   rule401 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule402 #-}
   rule402 = \  (_ :: ()) ->
     []
   {-# INLINE rule403 #-}
   rule403 = \  (_ :: ()) ->
     []
   {-# INLINE rule404 #-}
   rule404 = \  (_ :: ()) ->
     []
   {-# INLINE rule405 #-}
   rule405 = \  (_ :: ()) ->
     []
   {-# INLINE rule406 #-}
   rule406 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule407 #-}
   rule407 = \  (_ :: ()) ->
     []
   {-# INLINE rule408 #-}
   rule408 = \  (_ :: ()) ->
     []
   {-# INLINE rule409 #-}
   rule409 = \  (_ :: ()) ->
     []
   {-# INLINE rule410 #-}
   rule410 = \  (_ :: ()) ->
     []
   {-# INLINE rule411 #-}
   rule411 = \ ((_setIcollectedNames) :: Set Identifier) ->
     _setIcollectedNames
   {-# INLINE rule412 #-}
   rule412 = \  (_ :: ()) ->
     []
   {-# INLINE rule413 #-}
   rule413 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule414 #-}
   rule414 = \  (_ :: ()) ->
     []
   {-# INLINE rule415 #-}
   rule415 = \  (_ :: ()) ->
     []
   {-# INLINE rule416 #-}
   rule416 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule417 #-}
   rule417 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule418 #-}
   rule418 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule419 #-}
   rule419 = \ ((_setIerrors) :: Seq Error) ->
     _setIerrors
   {-# INLINE rule420 #-}
   rule420 = \  (_ :: ()) ->
     mzero
   {-# INLINE rule421 #-}
   rule421 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule422 #-}
   rule422 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule423 #-}
   rule423 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule424 #-}
   rule424 = \  (_ :: ()) ->
     []
   {-# INLINE rule425 #-}
   rule425 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule426 #-}
   rule426 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule427 #-}
   rule427 = \ ((_lhsIattrDecls) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIattrDecls
   {-# INLINE rule428 #-}
   rule428 = \ ((_lhsIattrs) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIattrs
   {-# INLINE rule429 #-}
   rule429 = \ ((_lhsIdefSets) :: Map Identifier (Set NontermIdent,Set Identifier)) ->
     _lhsIdefSets
   {-# INLINE rule430 #-}
   rule430 = \ ((_lhsIallFields) :: DataTypes) ->
     _lhsIallFields
   {-# INLINE rule431 #-}
   rule431 = \ ((_lhsIallNonterminals) :: Set NontermIdent) ->
     _lhsIallNonterminals
   {-# INLINE rule432 #-}
   rule432 = \ ((_lhsIdefinedSets) :: DefinedSets) ->
     _lhsIdefinedSets
{-# NOINLINE sem_Elem_Pragma #-}
sem_Elem_Pragma :: (Pos) -> ([NontermIdent]) -> T_Elem 
sem_Elem_Pragma _ arg_names_ = T_Elem (return st17) where
   {-# NOINLINE st17 #-}
   st17 = let
      v16 :: T_Elem_v16 
      v16 = \ (T_Elem_vIn16 _lhsIallAttrDecls _lhsIallAttrs _lhsIallConstructors _lhsIallFields _lhsIallNonterminals _lhsIattrDecls _lhsIattrs _lhsIdefSets _lhsIdefinedSets _lhsIoptions) -> ( let
         _lhsOpragmas :: Options -> Options
         _lhsOpragmas = rule433 arg_names_
         _lhsOattrOrderCollect :: AttrOrderMap
         _lhsOattrOrderCollect = rule434  ()
         _lhsOblocks :: Blocks
         _lhsOblocks = rule435  ()
         _lhsOcollectedArounds :: [ (NontermIdent, ConstructorIdent, [AroundInfo])  ]
         _lhsOcollectedArounds = rule436  ()
         _lhsOcollectedAugments :: [ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]
         _lhsOcollectedAugments = rule437  ()
         _lhsOcollectedConParams :: [(NontermIdent, ConstructorIdent, Set Identifier)]
         _lhsOcollectedConParams = rule438  ()
         _lhsOcollectedConstraints :: [(NontermIdent, ConstructorIdent, [Type])]
         _lhsOcollectedConstraints = rule439  ()
         _lhsOcollectedConstructorsMap :: Map NontermIdent (Set ConstructorIdent)
         _lhsOcollectedConstructorsMap = rule440  ()
         _lhsOcollectedFields :: [(NontermIdent, ConstructorIdent, FieldMap)]
         _lhsOcollectedFields = rule441  ()
         _lhsOcollectedInsts :: [ (NontermIdent, ConstructorIdent, [Identifier]) ]
         _lhsOcollectedInsts = rule442  ()
         _lhsOcollectedMacros :: [(NontermIdent, ConstructorIdent, MaybeMacro)]
         _lhsOcollectedMacros = rule443  ()
         _lhsOcollectedMerges :: [ (NontermIdent, ConstructorIdent, [MergeInfo])   ]
         _lhsOcollectedMerges = rule444  ()
         _lhsOcollectedNames :: Set Identifier
         _lhsOcollectedNames = rule445  ()
         _lhsOcollectedRules :: [ (NontermIdent, ConstructorIdent, RuleInfo)]
         _lhsOcollectedRules = rule446  ()
         _lhsOcollectedSetNames :: Set Identifier
         _lhsOcollectedSetNames = rule447  ()
         _lhsOcollectedSigs :: [ (NontermIdent, ConstructorIdent, SigInfo) ]
         _lhsOcollectedSigs = rule448  ()
         _lhsOcollectedUniques :: [ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]
         _lhsOcollectedUniques = rule449  ()
         _lhsOconstructorTypeMap :: Map NontermIdent ConstructorType
         _lhsOconstructorTypeMap = rule450  ()
         _lhsOctxCollect :: ContextMap
         _lhsOctxCollect = rule451  ()
         _lhsOderivings :: Derivings
         _lhsOderivings = rule452  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule453  ()
         _lhsOmoduleDecl :: Maybe (String,String,String)
         _lhsOmoduleDecl = rule454  ()
         _lhsOparamsCollect :: ParamMap
         _lhsOparamsCollect = rule455  ()
         _lhsOquantCollect :: QuantMap
         _lhsOquantCollect = rule456  ()
         _lhsOsemPragmasCollect :: PragmaMap
         _lhsOsemPragmasCollect = rule457  ()
         _lhsOtypeSyns :: TypeSyns
         _lhsOtypeSyns = rule458  ()
         _lhsOuseMap :: Map NontermIdent (Map Identifier (String,String,String))
         _lhsOuseMap = rule459  ()
         _lhsOwrappers :: Set NontermIdent
         _lhsOwrappers = rule460  ()
         _lhsOattrDecls :: Map NontermIdent (Attributes, Attributes)
         _lhsOattrDecls = rule461 _lhsIattrDecls
         _lhsOattrs :: Map NontermIdent (Attributes, Attributes)
         _lhsOattrs = rule462 _lhsIattrs
         _lhsOdefSets :: Map Identifier (Set NontermIdent,Set Identifier)
         _lhsOdefSets = rule463 _lhsIdefSets
         __result_ = T_Elem_vOut16 _lhsOattrDecls _lhsOattrOrderCollect _lhsOattrs _lhsOblocks _lhsOcollectedArounds _lhsOcollectedAugments _lhsOcollectedConParams _lhsOcollectedConstraints _lhsOcollectedConstructorsMap _lhsOcollectedFields _lhsOcollectedInsts _lhsOcollectedMacros _lhsOcollectedMerges _lhsOcollectedNames _lhsOcollectedRules _lhsOcollectedSetNames _lhsOcollectedSigs _lhsOcollectedUniques _lhsOconstructorTypeMap _lhsOctxCollect _lhsOdefSets _lhsOderivings _lhsOerrors _lhsOmoduleDecl _lhsOparamsCollect _lhsOpragmas _lhsOquantCollect _lhsOsemPragmasCollect _lhsOtypeSyns _lhsOuseMap _lhsOwrappers
         in __result_ )
     in C_Elem_s17 v16
   {-# INLINE rule433 #-}
   {-# LINE 805 "./src-ag/Transform.ag" #-}
   rule433 = \ names_ ->
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
                                           "cleanlang"               -> cleanOpt o
                                           s              -> trace ("uuagc: ignoring unknown pragma: " ++ s) o
                            in \o -> foldr mk o names_
                            {-# LINE 3593 "dist/build/Transform.hs"#-}
   {-# INLINE rule434 #-}
   rule434 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule435 #-}
   rule435 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule436 #-}
   rule436 = \  (_ :: ()) ->
     []
   {-# INLINE rule437 #-}
   rule437 = \  (_ :: ()) ->
     []
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
     []
   {-# INLINE rule442 #-}
   rule442 = \  (_ :: ()) ->
     []
   {-# INLINE rule443 #-}
   rule443 = \  (_ :: ()) ->
     []
   {-# INLINE rule444 #-}
   rule444 = \  (_ :: ()) ->
     []
   {-# INLINE rule445 #-}
   rule445 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule446 #-}
   rule446 = \  (_ :: ()) ->
     []
   {-# INLINE rule447 #-}
   rule447 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule448 #-}
   rule448 = \  (_ :: ()) ->
     []
   {-# INLINE rule449 #-}
   rule449 = \  (_ :: ()) ->
     []
   {-# INLINE rule450 #-}
   rule450 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule451 #-}
   rule451 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule452 #-}
   rule452 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule453 #-}
   rule453 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule454 #-}
   rule454 = \  (_ :: ()) ->
     mzero
   {-# INLINE rule455 #-}
   rule455 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule456 #-}
   rule456 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule457 #-}
   rule457 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule458 #-}
   rule458 = \  (_ :: ()) ->
     []
   {-# INLINE rule459 #-}
   rule459 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule460 #-}
   rule460 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule461 #-}
   rule461 = \ ((_lhsIattrDecls) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIattrDecls
   {-# INLINE rule462 #-}
   rule462 = \ ((_lhsIattrs) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIattrs
   {-# INLINE rule463 #-}
   rule463 = \ ((_lhsIdefSets) :: Map Identifier (Set NontermIdent,Set Identifier)) ->
     _lhsIdefSets
{-# NOINLINE sem_Elem_Module #-}
sem_Elem_Module :: (Pos) -> (String) -> (String) -> (String) -> T_Elem 
sem_Elem_Module _ arg_name_ arg_exports_ arg_imports_ = T_Elem (return st17) where
   {-# NOINLINE st17 #-}
   st17 = let
      v16 :: T_Elem_v16 
      v16 = \ (T_Elem_vIn16 _lhsIallAttrDecls _lhsIallAttrs _lhsIallConstructors _lhsIallFields _lhsIallNonterminals _lhsIattrDecls _lhsIattrs _lhsIdefSets _lhsIdefinedSets _lhsIoptions) -> ( let
         _lhsOmoduleDecl :: Maybe (String,String,String)
         _lhsOmoduleDecl = rule464 arg_exports_ arg_imports_ arg_name_
         _lhsOattrOrderCollect :: AttrOrderMap
         _lhsOattrOrderCollect = rule465  ()
         _lhsOblocks :: Blocks
         _lhsOblocks = rule466  ()
         _lhsOcollectedArounds :: [ (NontermIdent, ConstructorIdent, [AroundInfo])  ]
         _lhsOcollectedArounds = rule467  ()
         _lhsOcollectedAugments :: [ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]
         _lhsOcollectedAugments = rule468  ()
         _lhsOcollectedConParams :: [(NontermIdent, ConstructorIdent, Set Identifier)]
         _lhsOcollectedConParams = rule469  ()
         _lhsOcollectedConstraints :: [(NontermIdent, ConstructorIdent, [Type])]
         _lhsOcollectedConstraints = rule470  ()
         _lhsOcollectedConstructorsMap :: Map NontermIdent (Set ConstructorIdent)
         _lhsOcollectedConstructorsMap = rule471  ()
         _lhsOcollectedFields :: [(NontermIdent, ConstructorIdent, FieldMap)]
         _lhsOcollectedFields = rule472  ()
         _lhsOcollectedInsts :: [ (NontermIdent, ConstructorIdent, [Identifier]) ]
         _lhsOcollectedInsts = rule473  ()
         _lhsOcollectedMacros :: [(NontermIdent, ConstructorIdent, MaybeMacro)]
         _lhsOcollectedMacros = rule474  ()
         _lhsOcollectedMerges :: [ (NontermIdent, ConstructorIdent, [MergeInfo])   ]
         _lhsOcollectedMerges = rule475  ()
         _lhsOcollectedNames :: Set Identifier
         _lhsOcollectedNames = rule476  ()
         _lhsOcollectedRules :: [ (NontermIdent, ConstructorIdent, RuleInfo)]
         _lhsOcollectedRules = rule477  ()
         _lhsOcollectedSetNames :: Set Identifier
         _lhsOcollectedSetNames = rule478  ()
         _lhsOcollectedSigs :: [ (NontermIdent, ConstructorIdent, SigInfo) ]
         _lhsOcollectedSigs = rule479  ()
         _lhsOcollectedUniques :: [ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]
         _lhsOcollectedUniques = rule480  ()
         _lhsOconstructorTypeMap :: Map NontermIdent ConstructorType
         _lhsOconstructorTypeMap = rule481  ()
         _lhsOctxCollect :: ContextMap
         _lhsOctxCollect = rule482  ()
         _lhsOderivings :: Derivings
         _lhsOderivings = rule483  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule484  ()
         _lhsOparamsCollect :: ParamMap
         _lhsOparamsCollect = rule485  ()
         _lhsOpragmas :: Options -> Options
         _lhsOpragmas = rule486  ()
         _lhsOquantCollect :: QuantMap
         _lhsOquantCollect = rule487  ()
         _lhsOsemPragmasCollect :: PragmaMap
         _lhsOsemPragmasCollect = rule488  ()
         _lhsOtypeSyns :: TypeSyns
         _lhsOtypeSyns = rule489  ()
         _lhsOuseMap :: Map NontermIdent (Map Identifier (String,String,String))
         _lhsOuseMap = rule490  ()
         _lhsOwrappers :: Set NontermIdent
         _lhsOwrappers = rule491  ()
         _lhsOattrDecls :: Map NontermIdent (Attributes, Attributes)
         _lhsOattrDecls = rule492 _lhsIattrDecls
         _lhsOattrs :: Map NontermIdent (Attributes, Attributes)
         _lhsOattrs = rule493 _lhsIattrs
         _lhsOdefSets :: Map Identifier (Set NontermIdent,Set Identifier)
         _lhsOdefSets = rule494 _lhsIdefSets
         __result_ = T_Elem_vOut16 _lhsOattrDecls _lhsOattrOrderCollect _lhsOattrs _lhsOblocks _lhsOcollectedArounds _lhsOcollectedAugments _lhsOcollectedConParams _lhsOcollectedConstraints _lhsOcollectedConstructorsMap _lhsOcollectedFields _lhsOcollectedInsts _lhsOcollectedMacros _lhsOcollectedMerges _lhsOcollectedNames _lhsOcollectedRules _lhsOcollectedSetNames _lhsOcollectedSigs _lhsOcollectedUniques _lhsOconstructorTypeMap _lhsOctxCollect _lhsOdefSets _lhsOderivings _lhsOerrors _lhsOmoduleDecl _lhsOparamsCollect _lhsOpragmas _lhsOquantCollect _lhsOsemPragmasCollect _lhsOtypeSyns _lhsOuseMap _lhsOwrappers
         in __result_ )
     in C_Elem_s17 v16
   {-# INLINE rule464 #-}
   {-# LINE 1214 "./src-ag/Transform.ag" #-}
   rule464 = \ exports_ imports_ name_ ->
                         {-# LINE 1214 "./src-ag/Transform.ag" #-}
                         Just (name_, exports_, imports_)
                         {-# LINE 3761 "dist/build/Transform.hs"#-}
   {-# INLINE rule465 #-}
   rule465 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule466 #-}
   rule466 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule467 #-}
   rule467 = \  (_ :: ()) ->
     []
   {-# INLINE rule468 #-}
   rule468 = \  (_ :: ()) ->
     []
   {-# INLINE rule469 #-}
   rule469 = \  (_ :: ()) ->
     []
   {-# INLINE rule470 #-}
   rule470 = \  (_ :: ()) ->
     []
   {-# INLINE rule471 #-}
   rule471 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule472 #-}
   rule472 = \  (_ :: ()) ->
     []
   {-# INLINE rule473 #-}
   rule473 = \  (_ :: ()) ->
     []
   {-# INLINE rule474 #-}
   rule474 = \  (_ :: ()) ->
     []
   {-# INLINE rule475 #-}
   rule475 = \  (_ :: ()) ->
     []
   {-# INLINE rule476 #-}
   rule476 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule477 #-}
   rule477 = \  (_ :: ()) ->
     []
   {-# INLINE rule478 #-}
   rule478 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule479 #-}
   rule479 = \  (_ :: ()) ->
     []
   {-# INLINE rule480 #-}
   rule480 = \  (_ :: ()) ->
     []
   {-# INLINE rule481 #-}
   rule481 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule482 #-}
   rule482 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule483 #-}
   rule483 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule484 #-}
   rule484 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule485 #-}
   rule485 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule486 #-}
   rule486 = \  (_ :: ()) ->
     id
   {-# INLINE rule487 #-}
   rule487 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule488 #-}
   rule488 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule489 #-}
   rule489 = \  (_ :: ()) ->
     []
   {-# INLINE rule490 #-}
   rule490 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule491 #-}
   rule491 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule492 #-}
   rule492 = \ ((_lhsIattrDecls) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIattrDecls
   {-# INLINE rule493 #-}
   rule493 = \ ((_lhsIattrs) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIattrs
   {-# INLINE rule494 #-}
   rule494 = \ ((_lhsIdefSets) :: Map Identifier (Set NontermIdent,Set Identifier)) ->
     _lhsIdefSets

-- Elems -------------------------------------------------------
-- wrapper
data Inh_Elems  = Inh_Elems { allAttrDecls_Inh_Elems :: (Map NontermIdent (Attributes, Attributes)), allAttrs_Inh_Elems :: (Map NontermIdent (Attributes, Attributes)), allConstructors_Inh_Elems :: (Map NontermIdent (Set ConstructorIdent)), allFields_Inh_Elems :: (DataTypes), allNonterminals_Inh_Elems :: (Set NontermIdent), attrDecls_Inh_Elems :: (Map NontermIdent (Attributes, Attributes)), attrs_Inh_Elems :: (Map NontermIdent (Attributes, Attributes)), defSets_Inh_Elems :: (Map Identifier (Set NontermIdent,Set Identifier)), definedSets_Inh_Elems :: (DefinedSets), options_Inh_Elems :: (Options) }
data Syn_Elems  = Syn_Elems { attrDecls_Syn_Elems :: (Map NontermIdent (Attributes, Attributes)), attrOrderCollect_Syn_Elems :: (AttrOrderMap), attrs_Syn_Elems :: (Map NontermIdent (Attributes, Attributes)), blocks_Syn_Elems :: (Blocks), collectedArounds_Syn_Elems :: ([ (NontermIdent, ConstructorIdent, [AroundInfo])  ]), collectedAugments_Syn_Elems :: ([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]), collectedConParams_Syn_Elems :: ([(NontermIdent, ConstructorIdent, Set Identifier)]), collectedConstraints_Syn_Elems :: ([(NontermIdent, ConstructorIdent, [Type])]), collectedConstructorsMap_Syn_Elems :: (Map NontermIdent (Set ConstructorIdent)), collectedFields_Syn_Elems :: ([(NontermIdent, ConstructorIdent, FieldMap)]), collectedInsts_Syn_Elems :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ]), collectedMacros_Syn_Elems :: ([(NontermIdent, ConstructorIdent, MaybeMacro)]), collectedMerges_Syn_Elems :: ([ (NontermIdent, ConstructorIdent, [MergeInfo])   ]), collectedNames_Syn_Elems :: (Set Identifier), collectedRules_Syn_Elems :: ([ (NontermIdent, ConstructorIdent, RuleInfo)]), collectedSetNames_Syn_Elems :: (Set Identifier), collectedSigs_Syn_Elems :: ([ (NontermIdent, ConstructorIdent, SigInfo) ]), collectedUniques_Syn_Elems :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]), constructorTypeMap_Syn_Elems :: (Map NontermIdent ConstructorType), ctxCollect_Syn_Elems :: (ContextMap), defSets_Syn_Elems :: (Map Identifier (Set NontermIdent,Set Identifier)), derivings_Syn_Elems :: (Derivings), errors_Syn_Elems :: (Seq Error), moduleDecl_Syn_Elems :: (Maybe (String,String,String)), paramsCollect_Syn_Elems :: (ParamMap), pragmas_Syn_Elems :: (Options -> Options), quantCollect_Syn_Elems :: (QuantMap), semPragmasCollect_Syn_Elems :: (PragmaMap), typeSyns_Syn_Elems :: (TypeSyns), useMap_Syn_Elems :: (Map NontermIdent (Map Identifier (String,String,String))), wrappers_Syn_Elems :: (Set NontermIdent) }
{-# INLINABLE wrap_Elems #-}
wrap_Elems :: T_Elems  -> Inh_Elems  -> (Syn_Elems )
wrap_Elems (T_Elems act) (Inh_Elems _lhsIallAttrDecls _lhsIallAttrs _lhsIallConstructors _lhsIallFields _lhsIallNonterminals _lhsIattrDecls _lhsIattrs _lhsIdefSets _lhsIdefinedSets _lhsIoptions) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Elems_vIn19 _lhsIallAttrDecls _lhsIallAttrs _lhsIallConstructors _lhsIallFields _lhsIallNonterminals _lhsIattrDecls _lhsIattrs _lhsIdefSets _lhsIdefinedSets _lhsIoptions
        (T_Elems_vOut19 _lhsOattrDecls _lhsOattrOrderCollect _lhsOattrs _lhsOblocks _lhsOcollectedArounds _lhsOcollectedAugments _lhsOcollectedConParams _lhsOcollectedConstraints _lhsOcollectedConstructorsMap _lhsOcollectedFields _lhsOcollectedInsts _lhsOcollectedMacros _lhsOcollectedMerges _lhsOcollectedNames _lhsOcollectedRules _lhsOcollectedSetNames _lhsOcollectedSigs _lhsOcollectedUniques _lhsOconstructorTypeMap _lhsOctxCollect _lhsOdefSets _lhsOderivings _lhsOerrors _lhsOmoduleDecl _lhsOparamsCollect _lhsOpragmas _lhsOquantCollect _lhsOsemPragmasCollect _lhsOtypeSyns _lhsOuseMap _lhsOwrappers) <- return (inv_Elems_s20 sem arg)
        return (Syn_Elems _lhsOattrDecls _lhsOattrOrderCollect _lhsOattrs _lhsOblocks _lhsOcollectedArounds _lhsOcollectedAugments _lhsOcollectedConParams _lhsOcollectedConstraints _lhsOcollectedConstructorsMap _lhsOcollectedFields _lhsOcollectedInsts _lhsOcollectedMacros _lhsOcollectedMerges _lhsOcollectedNames _lhsOcollectedRules _lhsOcollectedSetNames _lhsOcollectedSigs _lhsOcollectedUniques _lhsOconstructorTypeMap _lhsOctxCollect _lhsOdefSets _lhsOderivings _lhsOerrors _lhsOmoduleDecl _lhsOparamsCollect _lhsOpragmas _lhsOquantCollect _lhsOsemPragmasCollect _lhsOtypeSyns _lhsOuseMap _lhsOwrappers)
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
data T_Elems_vOut19  = T_Elems_vOut19 (Map NontermIdent (Attributes, Attributes)) (AttrOrderMap) (Map NontermIdent (Attributes, Attributes)) (Blocks) ([ (NontermIdent, ConstructorIdent, [AroundInfo])  ]) ([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]) ([(NontermIdent, ConstructorIdent, Set Identifier)]) ([(NontermIdent, ConstructorIdent, [Type])]) (Map NontermIdent (Set ConstructorIdent)) ([(NontermIdent, ConstructorIdent, FieldMap)]) ([ (NontermIdent, ConstructorIdent, [Identifier]) ]) ([(NontermIdent, ConstructorIdent, MaybeMacro)]) ([ (NontermIdent, ConstructorIdent, [MergeInfo])   ]) (Set Identifier) ([ (NontermIdent, ConstructorIdent, RuleInfo)]) (Set Identifier) ([ (NontermIdent, ConstructorIdent, SigInfo) ]) ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]) (Map NontermIdent ConstructorType) (ContextMap) (Map Identifier (Set NontermIdent,Set Identifier)) (Derivings) (Seq Error) (Maybe (String,String,String)) (ParamMap) (Options -> Options) (QuantMap) (PragmaMap) (TypeSyns) (Map NontermIdent (Map Identifier (String,String,String))) (Set NontermIdent)
{-# NOINLINE sem_Elems_Cons #-}
sem_Elems_Cons :: T_Elem  -> T_Elems  -> T_Elems 
sem_Elems_Cons arg_hd_ arg_tl_ = T_Elems (return st20) where
   {-# NOINLINE st20 #-}
   st20 = let
      v19 :: T_Elems_v19 
      v19 = \ (T_Elems_vIn19 _lhsIallAttrDecls _lhsIallAttrs _lhsIallConstructors _lhsIallFields _lhsIallNonterminals _lhsIattrDecls _lhsIattrs _lhsIdefSets _lhsIdefinedSets _lhsIoptions) -> ( let
         _hdX17 = Control.Monad.Identity.runIdentity (attach_T_Elem (arg_hd_))
         _tlX20 = Control.Monad.Identity.runIdentity (attach_T_Elems (arg_tl_))
         (T_Elem_vOut16 _hdIattrDecls _hdIattrOrderCollect _hdIattrs _hdIblocks _hdIcollectedArounds _hdIcollectedAugments _hdIcollectedConParams _hdIcollectedConstraints _hdIcollectedConstructorsMap _hdIcollectedFields _hdIcollectedInsts _hdIcollectedMacros _hdIcollectedMerges _hdIcollectedNames _hdIcollectedRules _hdIcollectedSetNames _hdIcollectedSigs _hdIcollectedUniques _hdIconstructorTypeMap _hdIctxCollect _hdIdefSets _hdIderivings _hdIerrors _hdImoduleDecl _hdIparamsCollect _hdIpragmas _hdIquantCollect _hdIsemPragmasCollect _hdItypeSyns _hdIuseMap _hdIwrappers) = inv_Elem_s17 _hdX17 (T_Elem_vIn16 _hdOallAttrDecls _hdOallAttrs _hdOallConstructors _hdOallFields _hdOallNonterminals _hdOattrDecls _hdOattrs _hdOdefSets _hdOdefinedSets _hdOoptions)
         (T_Elems_vOut19 _tlIattrDecls _tlIattrOrderCollect _tlIattrs _tlIblocks _tlIcollectedArounds _tlIcollectedAugments _tlIcollectedConParams _tlIcollectedConstraints _tlIcollectedConstructorsMap _tlIcollectedFields _tlIcollectedInsts _tlIcollectedMacros _tlIcollectedMerges _tlIcollectedNames _tlIcollectedRules _tlIcollectedSetNames _tlIcollectedSigs _tlIcollectedUniques _tlIconstructorTypeMap _tlIctxCollect _tlIdefSets _tlIderivings _tlIerrors _tlImoduleDecl _tlIparamsCollect _tlIpragmas _tlIquantCollect _tlIsemPragmasCollect _tlItypeSyns _tlIuseMap _tlIwrappers) = inv_Elems_s20 _tlX20 (T_Elems_vIn19 _tlOallAttrDecls _tlOallAttrs _tlOallConstructors _tlOallFields _tlOallNonterminals _tlOattrDecls _tlOattrs _tlOdefSets _tlOdefinedSets _tlOoptions)
         _lhsOattrOrderCollect :: AttrOrderMap
         _lhsOattrOrderCollect = rule495 _hdIattrOrderCollect _tlIattrOrderCollect
         _lhsOblocks :: Blocks
         _lhsOblocks = rule496 _hdIblocks _tlIblocks
         _lhsOcollectedArounds :: [ (NontermIdent, ConstructorIdent, [AroundInfo])  ]
         _lhsOcollectedArounds = rule497 _hdIcollectedArounds _tlIcollectedArounds
         _lhsOcollectedAugments :: [ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]
         _lhsOcollectedAugments = rule498 _hdIcollectedAugments _tlIcollectedAugments
         _lhsOcollectedConParams :: [(NontermIdent, ConstructorIdent, Set Identifier)]
         _lhsOcollectedConParams = rule499 _hdIcollectedConParams _tlIcollectedConParams
         _lhsOcollectedConstraints :: [(NontermIdent, ConstructorIdent, [Type])]
         _lhsOcollectedConstraints = rule500 _hdIcollectedConstraints _tlIcollectedConstraints
         _lhsOcollectedConstructorsMap :: Map NontermIdent (Set ConstructorIdent)
         _lhsOcollectedConstructorsMap = rule501 _hdIcollectedConstructorsMap _tlIcollectedConstructorsMap
         _lhsOcollectedFields :: [(NontermIdent, ConstructorIdent, FieldMap)]
         _lhsOcollectedFields = rule502 _hdIcollectedFields _tlIcollectedFields
         _lhsOcollectedInsts :: [ (NontermIdent, ConstructorIdent, [Identifier]) ]
         _lhsOcollectedInsts = rule503 _hdIcollectedInsts _tlIcollectedInsts
         _lhsOcollectedMacros :: [(NontermIdent, ConstructorIdent, MaybeMacro)]
         _lhsOcollectedMacros = rule504 _hdIcollectedMacros _tlIcollectedMacros
         _lhsOcollectedMerges :: [ (NontermIdent, ConstructorIdent, [MergeInfo])   ]
         _lhsOcollectedMerges = rule505 _hdIcollectedMerges _tlIcollectedMerges
         _lhsOcollectedNames :: Set Identifier
         _lhsOcollectedNames = rule506 _hdIcollectedNames _tlIcollectedNames
         _lhsOcollectedRules :: [ (NontermIdent, ConstructorIdent, RuleInfo)]
         _lhsOcollectedRules = rule507 _hdIcollectedRules _tlIcollectedRules
         _lhsOcollectedSetNames :: Set Identifier
         _lhsOcollectedSetNames = rule508 _hdIcollectedSetNames _tlIcollectedSetNames
         _lhsOcollectedSigs :: [ (NontermIdent, ConstructorIdent, SigInfo) ]
         _lhsOcollectedSigs = rule509 _hdIcollectedSigs _tlIcollectedSigs
         _lhsOcollectedUniques :: [ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]
         _lhsOcollectedUniques = rule510 _hdIcollectedUniques _tlIcollectedUniques
         _lhsOconstructorTypeMap :: Map NontermIdent ConstructorType
         _lhsOconstructorTypeMap = rule511 _hdIconstructorTypeMap _tlIconstructorTypeMap
         _lhsOctxCollect :: ContextMap
         _lhsOctxCollect = rule512 _hdIctxCollect _tlIctxCollect
         _lhsOderivings :: Derivings
         _lhsOderivings = rule513 _hdIderivings _tlIderivings
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule514 _hdIerrors _tlIerrors
         _lhsOmoduleDecl :: Maybe (String,String,String)
         _lhsOmoduleDecl = rule515 _hdImoduleDecl _tlImoduleDecl
         _lhsOparamsCollect :: ParamMap
         _lhsOparamsCollect = rule516 _hdIparamsCollect _tlIparamsCollect
         _lhsOpragmas :: Options -> Options
         _lhsOpragmas = rule517 _hdIpragmas _tlIpragmas
         _lhsOquantCollect :: QuantMap
         _lhsOquantCollect = rule518 _hdIquantCollect _tlIquantCollect
         _lhsOsemPragmasCollect :: PragmaMap
         _lhsOsemPragmasCollect = rule519 _hdIsemPragmasCollect _tlIsemPragmasCollect
         _lhsOtypeSyns :: TypeSyns
         _lhsOtypeSyns = rule520 _hdItypeSyns _tlItypeSyns
         _lhsOuseMap :: Map NontermIdent (Map Identifier (String,String,String))
         _lhsOuseMap = rule521 _hdIuseMap _tlIuseMap
         _lhsOwrappers :: Set NontermIdent
         _lhsOwrappers = rule522 _hdIwrappers _tlIwrappers
         _lhsOattrDecls :: Map NontermIdent (Attributes, Attributes)
         _lhsOattrDecls = rule523 _tlIattrDecls
         _lhsOattrs :: Map NontermIdent (Attributes, Attributes)
         _lhsOattrs = rule524 _tlIattrs
         _lhsOdefSets :: Map Identifier (Set NontermIdent,Set Identifier)
         _lhsOdefSets = rule525 _tlIdefSets
         _hdOallAttrDecls = rule526 _lhsIallAttrDecls
         _hdOallAttrs = rule527 _lhsIallAttrs
         _hdOallConstructors = rule528 _lhsIallConstructors
         _hdOallFields = rule529 _lhsIallFields
         _hdOallNonterminals = rule530 _lhsIallNonterminals
         _hdOattrDecls = rule531 _lhsIattrDecls
         _hdOattrs = rule532 _lhsIattrs
         _hdOdefSets = rule533 _lhsIdefSets
         _hdOdefinedSets = rule534 _lhsIdefinedSets
         _hdOoptions = rule535 _lhsIoptions
         _tlOallAttrDecls = rule536 _lhsIallAttrDecls
         _tlOallAttrs = rule537 _lhsIallAttrs
         _tlOallConstructors = rule538 _lhsIallConstructors
         _tlOallFields = rule539 _lhsIallFields
         _tlOallNonterminals = rule540 _lhsIallNonterminals
         _tlOattrDecls = rule541 _hdIattrDecls
         _tlOattrs = rule542 _hdIattrs
         _tlOdefSets = rule543 _hdIdefSets
         _tlOdefinedSets = rule544 _lhsIdefinedSets
         _tlOoptions = rule545 _lhsIoptions
         __result_ = T_Elems_vOut19 _lhsOattrDecls _lhsOattrOrderCollect _lhsOattrs _lhsOblocks _lhsOcollectedArounds _lhsOcollectedAugments _lhsOcollectedConParams _lhsOcollectedConstraints _lhsOcollectedConstructorsMap _lhsOcollectedFields _lhsOcollectedInsts _lhsOcollectedMacros _lhsOcollectedMerges _lhsOcollectedNames _lhsOcollectedRules _lhsOcollectedSetNames _lhsOcollectedSigs _lhsOcollectedUniques _lhsOconstructorTypeMap _lhsOctxCollect _lhsOdefSets _lhsOderivings _lhsOerrors _lhsOmoduleDecl _lhsOparamsCollect _lhsOpragmas _lhsOquantCollect _lhsOsemPragmasCollect _lhsOtypeSyns _lhsOuseMap _lhsOwrappers
         in __result_ )
     in C_Elems_s20 v19
   {-# INLINE rule495 #-}
   rule495 = \ ((_hdIattrOrderCollect) :: AttrOrderMap) ((_tlIattrOrderCollect) :: AttrOrderMap) ->
     _hdIattrOrderCollect `orderMapUnion` _tlIattrOrderCollect
   {-# INLINE rule496 #-}
   rule496 = \ ((_hdIblocks) :: Blocks) ((_tlIblocks) :: Blocks) ->
     _hdIblocks `mapUnionWithPlusPlus` _tlIblocks
   {-# INLINE rule497 #-}
   rule497 = \ ((_hdIcollectedArounds) :: [ (NontermIdent, ConstructorIdent, [AroundInfo])  ]) ((_tlIcollectedArounds) :: [ (NontermIdent, ConstructorIdent, [AroundInfo])  ]) ->
     _hdIcollectedArounds ++ _tlIcollectedArounds
   {-# INLINE rule498 #-}
   rule498 = \ ((_hdIcollectedAugments) :: [ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]) ((_tlIcollectedAugments) :: [ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]) ->
     _hdIcollectedAugments ++ _tlIcollectedAugments
   {-# INLINE rule499 #-}
   rule499 = \ ((_hdIcollectedConParams) :: [(NontermIdent, ConstructorIdent, Set Identifier)]) ((_tlIcollectedConParams) :: [(NontermIdent, ConstructorIdent, Set Identifier)]) ->
     _hdIcollectedConParams ++ _tlIcollectedConParams
   {-# INLINE rule500 #-}
   rule500 = \ ((_hdIcollectedConstraints) :: [(NontermIdent, ConstructorIdent, [Type])]) ((_tlIcollectedConstraints) :: [(NontermIdent, ConstructorIdent, [Type])]) ->
     _hdIcollectedConstraints ++ _tlIcollectedConstraints
   {-# INLINE rule501 #-}
   rule501 = \ ((_hdIcollectedConstructorsMap) :: Map NontermIdent (Set ConstructorIdent)) ((_tlIcollectedConstructorsMap) :: Map NontermIdent (Set ConstructorIdent)) ->
     _hdIcollectedConstructorsMap `mapUnionWithSetUnion` _tlIcollectedConstructorsMap
   {-# INLINE rule502 #-}
   rule502 = \ ((_hdIcollectedFields) :: [(NontermIdent, ConstructorIdent, FieldMap)]) ((_tlIcollectedFields) :: [(NontermIdent, ConstructorIdent, FieldMap)]) ->
     _hdIcollectedFields ++ _tlIcollectedFields
   {-# INLINE rule503 #-}
   rule503 = \ ((_hdIcollectedInsts) :: [ (NontermIdent, ConstructorIdent, [Identifier]) ]) ((_tlIcollectedInsts) :: [ (NontermIdent, ConstructorIdent, [Identifier]) ]) ->
     _hdIcollectedInsts ++ _tlIcollectedInsts
   {-# INLINE rule504 #-}
   rule504 = \ ((_hdIcollectedMacros) :: [(NontermIdent, ConstructorIdent, MaybeMacro)]) ((_tlIcollectedMacros) :: [(NontermIdent, ConstructorIdent, MaybeMacro)]) ->
     _hdIcollectedMacros ++ _tlIcollectedMacros
   {-# INLINE rule505 #-}
   rule505 = \ ((_hdIcollectedMerges) :: [ (NontermIdent, ConstructorIdent, [MergeInfo])   ]) ((_tlIcollectedMerges) :: [ (NontermIdent, ConstructorIdent, [MergeInfo])   ]) ->
     _hdIcollectedMerges ++ _tlIcollectedMerges
   {-# INLINE rule506 #-}
   rule506 = \ ((_hdIcollectedNames) :: Set Identifier) ((_tlIcollectedNames) :: Set Identifier) ->
     _hdIcollectedNames `Set.union` _tlIcollectedNames
   {-# INLINE rule507 #-}
   rule507 = \ ((_hdIcollectedRules) :: [ (NontermIdent, ConstructorIdent, RuleInfo)]) ((_tlIcollectedRules) :: [ (NontermIdent, ConstructorIdent, RuleInfo)]) ->
     _hdIcollectedRules ++ _tlIcollectedRules
   {-# INLINE rule508 #-}
   rule508 = \ ((_hdIcollectedSetNames) :: Set Identifier) ((_tlIcollectedSetNames) :: Set Identifier) ->
     _hdIcollectedSetNames `Set.union` _tlIcollectedSetNames
   {-# INLINE rule509 #-}
   rule509 = \ ((_hdIcollectedSigs) :: [ (NontermIdent, ConstructorIdent, SigInfo) ]) ((_tlIcollectedSigs) :: [ (NontermIdent, ConstructorIdent, SigInfo) ]) ->
     _hdIcollectedSigs ++ _tlIcollectedSigs
   {-# INLINE rule510 #-}
   rule510 = \ ((_hdIcollectedUniques) :: [ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]) ((_tlIcollectedUniques) :: [ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]) ->
     _hdIcollectedUniques ++ _tlIcollectedUniques
   {-# INLINE rule511 #-}
   rule511 = \ ((_hdIconstructorTypeMap) :: Map NontermIdent ConstructorType) ((_tlIconstructorTypeMap) :: Map NontermIdent ConstructorType) ->
     _hdIconstructorTypeMap `Map.union` _tlIconstructorTypeMap
   {-# INLINE rule512 #-}
   rule512 = \ ((_hdIctxCollect) :: ContextMap) ((_tlIctxCollect) :: ContextMap) ->
     _hdIctxCollect `mergeCtx` _tlIctxCollect
   {-# INLINE rule513 #-}
   rule513 = \ ((_hdIderivings) :: Derivings) ((_tlIderivings) :: Derivings) ->
     _hdIderivings `mergeDerivings` _tlIderivings
   {-# INLINE rule514 #-}
   rule514 = \ ((_hdIerrors) :: Seq Error) ((_tlIerrors) :: Seq Error) ->
     _hdIerrors Seq.>< _tlIerrors
   {-# INLINE rule515 #-}
   rule515 = \ ((_hdImoduleDecl) :: Maybe (String,String,String)) ((_tlImoduleDecl) :: Maybe (String,String,String)) ->
     _hdImoduleDecl `flipmplus` _tlImoduleDecl
   {-# INLINE rule516 #-}
   rule516 = \ ((_hdIparamsCollect) :: ParamMap) ((_tlIparamsCollect) :: ParamMap) ->
     _hdIparamsCollect `mergeParams` _tlIparamsCollect
   {-# INLINE rule517 #-}
   rule517 = \ ((_hdIpragmas) :: Options -> Options) ((_tlIpragmas) :: Options -> Options) ->
     _hdIpragmas . _tlIpragmas
   {-# INLINE rule518 #-}
   rule518 = \ ((_hdIquantCollect) :: QuantMap) ((_tlIquantCollect) :: QuantMap) ->
     _hdIquantCollect `mergeQuant` _tlIquantCollect
   {-# INLINE rule519 #-}
   rule519 = \ ((_hdIsemPragmasCollect) :: PragmaMap) ((_tlIsemPragmasCollect) :: PragmaMap) ->
     _hdIsemPragmasCollect `pragmaMapUnion` _tlIsemPragmasCollect
   {-# INLINE rule520 #-}
   rule520 = \ ((_hdItypeSyns) :: TypeSyns) ((_tlItypeSyns) :: TypeSyns) ->
     _hdItypeSyns ++ _tlItypeSyns
   {-# INLINE rule521 #-}
   rule521 = \ ((_hdIuseMap) :: Map NontermIdent (Map Identifier (String,String,String))) ((_tlIuseMap) :: Map NontermIdent (Map Identifier (String,String,String))) ->
     _hdIuseMap `merge` _tlIuseMap
   {-# INLINE rule522 #-}
   rule522 = \ ((_hdIwrappers) :: Set NontermIdent) ((_tlIwrappers) :: Set NontermIdent) ->
     _hdIwrappers `Set.union` _tlIwrappers
   {-# INLINE rule523 #-}
   rule523 = \ ((_tlIattrDecls) :: Map NontermIdent (Attributes, Attributes)) ->
     _tlIattrDecls
   {-# INLINE rule524 #-}
   rule524 = \ ((_tlIattrs) :: Map NontermIdent (Attributes, Attributes)) ->
     _tlIattrs
   {-# INLINE rule525 #-}
   rule525 = \ ((_tlIdefSets) :: Map Identifier (Set NontermIdent,Set Identifier)) ->
     _tlIdefSets
   {-# INLINE rule526 #-}
   rule526 = \ ((_lhsIallAttrDecls) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIallAttrDecls
   {-# INLINE rule527 #-}
   rule527 = \ ((_lhsIallAttrs) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIallAttrs
   {-# INLINE rule528 #-}
   rule528 = \ ((_lhsIallConstructors) :: Map NontermIdent (Set ConstructorIdent)) ->
     _lhsIallConstructors
   {-# INLINE rule529 #-}
   rule529 = \ ((_lhsIallFields) :: DataTypes) ->
     _lhsIallFields
   {-# INLINE rule530 #-}
   rule530 = \ ((_lhsIallNonterminals) :: Set NontermIdent) ->
     _lhsIallNonterminals
   {-# INLINE rule531 #-}
   rule531 = \ ((_lhsIattrDecls) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIattrDecls
   {-# INLINE rule532 #-}
   rule532 = \ ((_lhsIattrs) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIattrs
   {-# INLINE rule533 #-}
   rule533 = \ ((_lhsIdefSets) :: Map Identifier (Set NontermIdent,Set Identifier)) ->
     _lhsIdefSets
   {-# INLINE rule534 #-}
   rule534 = \ ((_lhsIdefinedSets) :: DefinedSets) ->
     _lhsIdefinedSets
   {-# INLINE rule535 #-}
   rule535 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule536 #-}
   rule536 = \ ((_lhsIallAttrDecls) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIallAttrDecls
   {-# INLINE rule537 #-}
   rule537 = \ ((_lhsIallAttrs) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIallAttrs
   {-# INLINE rule538 #-}
   rule538 = \ ((_lhsIallConstructors) :: Map NontermIdent (Set ConstructorIdent)) ->
     _lhsIallConstructors
   {-# INLINE rule539 #-}
   rule539 = \ ((_lhsIallFields) :: DataTypes) ->
     _lhsIallFields
   {-# INLINE rule540 #-}
   rule540 = \ ((_lhsIallNonterminals) :: Set NontermIdent) ->
     _lhsIallNonterminals
   {-# INLINE rule541 #-}
   rule541 = \ ((_hdIattrDecls) :: Map NontermIdent (Attributes, Attributes)) ->
     _hdIattrDecls
   {-# INLINE rule542 #-}
   rule542 = \ ((_hdIattrs) :: Map NontermIdent (Attributes, Attributes)) ->
     _hdIattrs
   {-# INLINE rule543 #-}
   rule543 = \ ((_hdIdefSets) :: Map Identifier (Set NontermIdent,Set Identifier)) ->
     _hdIdefSets
   {-# INLINE rule544 #-}
   rule544 = \ ((_lhsIdefinedSets) :: DefinedSets) ->
     _lhsIdefinedSets
   {-# INLINE rule545 #-}
   rule545 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Elems_Nil #-}
sem_Elems_Nil ::  T_Elems 
sem_Elems_Nil  = T_Elems (return st20) where
   {-# NOINLINE st20 #-}
   st20 = let
      v19 :: T_Elems_v19 
      v19 = \ (T_Elems_vIn19 _lhsIallAttrDecls _lhsIallAttrs _lhsIallConstructors _lhsIallFields _lhsIallNonterminals _lhsIattrDecls _lhsIattrs _lhsIdefSets _lhsIdefinedSets _lhsIoptions) -> ( let
         _lhsOattrOrderCollect :: AttrOrderMap
         _lhsOattrOrderCollect = rule546  ()
         _lhsOblocks :: Blocks
         _lhsOblocks = rule547  ()
         _lhsOcollectedArounds :: [ (NontermIdent, ConstructorIdent, [AroundInfo])  ]
         _lhsOcollectedArounds = rule548  ()
         _lhsOcollectedAugments :: [ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]
         _lhsOcollectedAugments = rule549  ()
         _lhsOcollectedConParams :: [(NontermIdent, ConstructorIdent, Set Identifier)]
         _lhsOcollectedConParams = rule550  ()
         _lhsOcollectedConstraints :: [(NontermIdent, ConstructorIdent, [Type])]
         _lhsOcollectedConstraints = rule551  ()
         _lhsOcollectedConstructorsMap :: Map NontermIdent (Set ConstructorIdent)
         _lhsOcollectedConstructorsMap = rule552  ()
         _lhsOcollectedFields :: [(NontermIdent, ConstructorIdent, FieldMap)]
         _lhsOcollectedFields = rule553  ()
         _lhsOcollectedInsts :: [ (NontermIdent, ConstructorIdent, [Identifier]) ]
         _lhsOcollectedInsts = rule554  ()
         _lhsOcollectedMacros :: [(NontermIdent, ConstructorIdent, MaybeMacro)]
         _lhsOcollectedMacros = rule555  ()
         _lhsOcollectedMerges :: [ (NontermIdent, ConstructorIdent, [MergeInfo])   ]
         _lhsOcollectedMerges = rule556  ()
         _lhsOcollectedNames :: Set Identifier
         _lhsOcollectedNames = rule557  ()
         _lhsOcollectedRules :: [ (NontermIdent, ConstructorIdent, RuleInfo)]
         _lhsOcollectedRules = rule558  ()
         _lhsOcollectedSetNames :: Set Identifier
         _lhsOcollectedSetNames = rule559  ()
         _lhsOcollectedSigs :: [ (NontermIdent, ConstructorIdent, SigInfo) ]
         _lhsOcollectedSigs = rule560  ()
         _lhsOcollectedUniques :: [ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]
         _lhsOcollectedUniques = rule561  ()
         _lhsOconstructorTypeMap :: Map NontermIdent ConstructorType
         _lhsOconstructorTypeMap = rule562  ()
         _lhsOctxCollect :: ContextMap
         _lhsOctxCollect = rule563  ()
         _lhsOderivings :: Derivings
         _lhsOderivings = rule564  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule565  ()
         _lhsOmoduleDecl :: Maybe (String,String,String)
         _lhsOmoduleDecl = rule566  ()
         _lhsOparamsCollect :: ParamMap
         _lhsOparamsCollect = rule567  ()
         _lhsOpragmas :: Options -> Options
         _lhsOpragmas = rule568  ()
         _lhsOquantCollect :: QuantMap
         _lhsOquantCollect = rule569  ()
         _lhsOsemPragmasCollect :: PragmaMap
         _lhsOsemPragmasCollect = rule570  ()
         _lhsOtypeSyns :: TypeSyns
         _lhsOtypeSyns = rule571  ()
         _lhsOuseMap :: Map NontermIdent (Map Identifier (String,String,String))
         _lhsOuseMap = rule572  ()
         _lhsOwrappers :: Set NontermIdent
         _lhsOwrappers = rule573  ()
         _lhsOattrDecls :: Map NontermIdent (Attributes, Attributes)
         _lhsOattrDecls = rule574 _lhsIattrDecls
         _lhsOattrs :: Map NontermIdent (Attributes, Attributes)
         _lhsOattrs = rule575 _lhsIattrs
         _lhsOdefSets :: Map Identifier (Set NontermIdent,Set Identifier)
         _lhsOdefSets = rule576 _lhsIdefSets
         __result_ = T_Elems_vOut19 _lhsOattrDecls _lhsOattrOrderCollect _lhsOattrs _lhsOblocks _lhsOcollectedArounds _lhsOcollectedAugments _lhsOcollectedConParams _lhsOcollectedConstraints _lhsOcollectedConstructorsMap _lhsOcollectedFields _lhsOcollectedInsts _lhsOcollectedMacros _lhsOcollectedMerges _lhsOcollectedNames _lhsOcollectedRules _lhsOcollectedSetNames _lhsOcollectedSigs _lhsOcollectedUniques _lhsOconstructorTypeMap _lhsOctxCollect _lhsOdefSets _lhsOderivings _lhsOerrors _lhsOmoduleDecl _lhsOparamsCollect _lhsOpragmas _lhsOquantCollect _lhsOsemPragmasCollect _lhsOtypeSyns _lhsOuseMap _lhsOwrappers
         in __result_ )
     in C_Elems_s20 v19
   {-# INLINE rule546 #-}
   rule546 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule547 #-}
   rule547 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule548 #-}
   rule548 = \  (_ :: ()) ->
     []
   {-# INLINE rule549 #-}
   rule549 = \  (_ :: ()) ->
     []
   {-# INLINE rule550 #-}
   rule550 = \  (_ :: ()) ->
     []
   {-# INLINE rule551 #-}
   rule551 = \  (_ :: ()) ->
     []
   {-# INLINE rule552 #-}
   rule552 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule553 #-}
   rule553 = \  (_ :: ()) ->
     []
   {-# INLINE rule554 #-}
   rule554 = \  (_ :: ()) ->
     []
   {-# INLINE rule555 #-}
   rule555 = \  (_ :: ()) ->
     []
   {-# INLINE rule556 #-}
   rule556 = \  (_ :: ()) ->
     []
   {-# INLINE rule557 #-}
   rule557 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule558 #-}
   rule558 = \  (_ :: ()) ->
     []
   {-# INLINE rule559 #-}
   rule559 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule560 #-}
   rule560 = \  (_ :: ()) ->
     []
   {-# INLINE rule561 #-}
   rule561 = \  (_ :: ()) ->
     []
   {-# INLINE rule562 #-}
   rule562 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule563 #-}
   rule563 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule564 #-}
   rule564 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule565 #-}
   rule565 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule566 #-}
   rule566 = \  (_ :: ()) ->
     mzero
   {-# INLINE rule567 #-}
   rule567 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule568 #-}
   rule568 = \  (_ :: ()) ->
     id
   {-# INLINE rule569 #-}
   rule569 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule570 #-}
   rule570 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule571 #-}
   rule571 = \  (_ :: ()) ->
     []
   {-# INLINE rule572 #-}
   rule572 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule573 #-}
   rule573 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule574 #-}
   rule574 = \ ((_lhsIattrDecls) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIattrDecls
   {-# INLINE rule575 #-}
   rule575 = \ ((_lhsIattrs) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIattrs
   {-# INLINE rule576 #-}
   rule576 = \ ((_lhsIdefSets) :: Map Identifier (Set NontermIdent,Set Identifier)) ->
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
         _lhsOcollectedFields = rule577 _lhsIallNonterminals arg_name_ arg_tp_
         _lhsOcollectedConstraints :: [Type]
         _lhsOcollectedConstraints = rule578  ()
         __result_ = T_Field_vOut22 _lhsOcollectedConstraints _lhsOcollectedFields
         in __result_ )
     in C_Field_s23 v22
   {-# INLINE rule577 #-}
   {-# LINE 579 "./src-ag/Transform.ag" #-}
   rule577 = \ ((_lhsIallNonterminals) :: Set NontermIdent) name_ tp_ ->
                          {-# LINE 579 "./src-ag/Transform.ag" #-}
                          [(name_, makeType _lhsIallNonterminals tp_)]
                          {-# LINE 4348 "dist/build/Transform.hs"#-}
   {-# INLINE rule578 #-}
   rule578 = \  (_ :: ()) ->
     []
{-# NOINLINE sem_Field_FCtx #-}
sem_Field_FCtx :: ([Type]) -> T_Field 
sem_Field_FCtx arg_tps_ = T_Field (return st23) where
   {-# NOINLINE st23 #-}
   st23 = let
      v22 :: T_Field_v22 
      v22 = \ (T_Field_vIn22 _lhsIallNonterminals) -> ( let
         _lhsOcollectedConstraints :: [Type]
         _lhsOcollectedConstraints = rule579 arg_tps_
         _lhsOcollectedFields :: [(Identifier, Type)]
         _lhsOcollectedFields = rule580  ()
         __result_ = T_Field_vOut22 _lhsOcollectedConstraints _lhsOcollectedFields
         in __result_ )
     in C_Field_s23 v22
   {-# INLINE rule579 #-}
   {-# LINE 588 "./src-ag/Transform.ag" #-}
   rule579 = \ tps_ ->
                               {-# LINE 588 "./src-ag/Transform.ag" #-}
                               tps_
                               {-# LINE 4371 "dist/build/Transform.hs"#-}
   {-# INLINE rule580 #-}
   rule580 = \  (_ :: ()) ->
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
         _lhsOcollectedConstraints = rule581 _hdIcollectedConstraints _tlIcollectedConstraints
         _lhsOcollectedFields :: [(Identifier, Type)]
         _lhsOcollectedFields = rule582 _hdIcollectedFields _tlIcollectedFields
         _hdOallNonterminals = rule583 _lhsIallNonterminals
         _tlOallNonterminals = rule584 _lhsIallNonterminals
         __result_ = T_Fields_vOut25 _lhsOcollectedConstraints _lhsOcollectedFields
         in __result_ )
     in C_Fields_s26 v25
   {-# INLINE rule581 #-}
   rule581 = \ ((_hdIcollectedConstraints) :: [Type]) ((_tlIcollectedConstraints) :: [Type]) ->
     _hdIcollectedConstraints ++ _tlIcollectedConstraints
   {-# INLINE rule582 #-}
   rule582 = \ ((_hdIcollectedFields) :: [(Identifier, Type)]) ((_tlIcollectedFields) :: [(Identifier, Type)]) ->
     _hdIcollectedFields ++ _tlIcollectedFields
   {-# INLINE rule583 #-}
   rule583 = \ ((_lhsIallNonterminals) :: Set NontermIdent) ->
     _lhsIallNonterminals
   {-# INLINE rule584 #-}
   rule584 = \ ((_lhsIallNonterminals) :: Set NontermIdent) ->
     _lhsIallNonterminals
{-# NOINLINE sem_Fields_Nil #-}
sem_Fields_Nil ::  T_Fields 
sem_Fields_Nil  = T_Fields (return st26) where
   {-# NOINLINE st26 #-}
   st26 = let
      v25 :: T_Fields_v25 
      v25 = \ (T_Fields_vIn25 _lhsIallNonterminals) -> ( let
         _lhsOcollectedConstraints :: [Type]
         _lhsOcollectedConstraints = rule585  ()
         _lhsOcollectedFields :: [(Identifier, Type)]
         _lhsOcollectedFields = rule586  ()
         __result_ = T_Fields_vOut25 _lhsOcollectedConstraints _lhsOcollectedFields
         in __result_ )
     in C_Fields_s26 v25
   {-# INLINE rule585 #-}
   rule585 = \  (_ :: ()) ->
     []
   {-# INLINE rule586 #-}
   rule586 = \  (_ :: ()) ->
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
         _lhsOcollectedNames = rule587 arg_name_
         (_nontSet,_errors) = rule588 _lhsIdefinedSets arg_name_
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule589 _errors
         _lhsOnontSet :: Set NontermIdent
         _lhsOnontSet = rule590 _nontSet
         __result_ = T_NontSet_vOut28 _lhsOcollectedNames _lhsOerrors _lhsOnontSet
         in __result_ )
     in C_NontSet_s29 v28
   {-# INLINE rule587 #-}
   {-# LINE 603 "./src-ag/Transform.ag" #-}
   rule587 = \ name_ ->
                                    {-# LINE 603 "./src-ag/Transform.ag" #-}
                                    Set.singleton name_
                                    {-# LINE 4516 "dist/build/Transform.hs"#-}
   {-# INLINE rule588 #-}
   {-# LINE 733 "./src-ag/Transform.ag" #-}
   rule588 = \ ((_lhsIdefinedSets) :: DefinedSets) name_ ->
                                        {-# LINE 733 "./src-ag/Transform.ag" #-}
                                        case Map.lookup name_ _lhsIdefinedSets of
                                                     Nothing  -> (Set.empty, Seq.singleton (UndefNont name_))
                                                     Just set -> (set, Seq.empty)
                                        {-# LINE 4524 "dist/build/Transform.hs"#-}
   {-# INLINE rule589 #-}
   rule589 = \ _errors ->
     _errors
   {-# INLINE rule590 #-}
   rule590 = \ _nontSet ->
     _nontSet
{-# NOINLINE sem_NontSet_All #-}
sem_NontSet_All ::  T_NontSet 
sem_NontSet_All  = T_NontSet (return st29) where
   {-# NOINLINE st29 #-}
   st29 = let
      v28 :: T_NontSet_v28 
      v28 = \ (T_NontSet_vIn28 _lhsIallFields _lhsIallNonterminals _lhsIdefinedSets) -> ( let
         _lhsOnontSet :: Set NontermIdent
         _lhsOnontSet = rule591 _lhsIallNonterminals
         _lhsOcollectedNames :: Set Identifier
         _lhsOcollectedNames = rule592  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule593  ()
         __result_ = T_NontSet_vOut28 _lhsOcollectedNames _lhsOerrors _lhsOnontSet
         in __result_ )
     in C_NontSet_s29 v28
   {-# INLINE rule591 #-}
   {-# LINE 732 "./src-ag/Transform.ag" #-}
   rule591 = \ ((_lhsIallNonterminals) :: Set NontermIdent) ->
                               {-# LINE 732 "./src-ag/Transform.ag" #-}
                               _lhsIallNonterminals
                               {-# LINE 4552 "dist/build/Transform.hs"#-}
   {-# INLINE rule592 #-}
   rule592 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule593 #-}
   rule593 = \  (_ :: ()) ->
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
         _lhsOnontSet = rule594 _set1InontSet _set2InontSet
         _lhsOcollectedNames :: Set Identifier
         _lhsOcollectedNames = rule595 _set1IcollectedNames _set2IcollectedNames
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule596 _set1Ierrors _set2Ierrors
         _set1OallFields = rule597 _lhsIallFields
         _set1OallNonterminals = rule598 _lhsIallNonterminals
         _set1OdefinedSets = rule599 _lhsIdefinedSets
         _set2OallFields = rule600 _lhsIallFields
         _set2OallNonterminals = rule601 _lhsIallNonterminals
         _set2OdefinedSets = rule602 _lhsIdefinedSets
         __result_ = T_NontSet_vOut28 _lhsOcollectedNames _lhsOerrors _lhsOnontSet
         in __result_ )
     in C_NontSet_s29 v28
   {-# INLINE rule594 #-}
   {-# LINE 736 "./src-ag/Transform.ag" #-}
   rule594 = \ ((_set1InontSet) :: Set NontermIdent) ((_set2InontSet) :: Set NontermIdent) ->
                               {-# LINE 736 "./src-ag/Transform.ag" #-}
                               Set.union         _set1InontSet _set2InontSet
                               {-# LINE 4590 "dist/build/Transform.hs"#-}
   {-# INLINE rule595 #-}
   rule595 = \ ((_set1IcollectedNames) :: Set Identifier) ((_set2IcollectedNames) :: Set Identifier) ->
     _set1IcollectedNames `Set.union` _set2IcollectedNames
   {-# INLINE rule596 #-}
   rule596 = \ ((_set1Ierrors) :: Seq Error) ((_set2Ierrors) :: Seq Error) ->
     _set1Ierrors Seq.>< _set2Ierrors
   {-# INLINE rule597 #-}
   rule597 = \ ((_lhsIallFields) :: DataTypes) ->
     _lhsIallFields
   {-# INLINE rule598 #-}
   rule598 = \ ((_lhsIallNonterminals) :: Set NontermIdent) ->
     _lhsIallNonterminals
   {-# INLINE rule599 #-}
   rule599 = \ ((_lhsIdefinedSets) :: DefinedSets) ->
     _lhsIdefinedSets
   {-# INLINE rule600 #-}
   rule600 = \ ((_lhsIallFields) :: DataTypes) ->
     _lhsIallFields
   {-# INLINE rule601 #-}
   rule601 = \ ((_lhsIallNonterminals) :: Set NontermIdent) ->
     _lhsIallNonterminals
   {-# INLINE rule602 #-}
   rule602 = \ ((_lhsIdefinedSets) :: DefinedSets) ->
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
         _lhsOnontSet = rule603 _set1InontSet _set2InontSet
         _lhsOcollectedNames :: Set Identifier
         _lhsOcollectedNames = rule604 _set1IcollectedNames _set2IcollectedNames
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule605 _set1Ierrors _set2Ierrors
         _set1OallFields = rule606 _lhsIallFields
         _set1OallNonterminals = rule607 _lhsIallNonterminals
         _set1OdefinedSets = rule608 _lhsIdefinedSets
         _set2OallFields = rule609 _lhsIallFields
         _set2OallNonterminals = rule610 _lhsIallNonterminals
         _set2OdefinedSets = rule611 _lhsIdefinedSets
         __result_ = T_NontSet_vOut28 _lhsOcollectedNames _lhsOerrors _lhsOnontSet
         in __result_ )
     in C_NontSet_s29 v28
   {-# INLINE rule603 #-}
   {-# LINE 737 "./src-ag/Transform.ag" #-}
   rule603 = \ ((_set1InontSet) :: Set NontermIdent) ((_set2InontSet) :: Set NontermIdent) ->
                               {-# LINE 737 "./src-ag/Transform.ag" #-}
                               Set.intersection  _set1InontSet _set2InontSet
                               {-# LINE 4646 "dist/build/Transform.hs"#-}
   {-# INLINE rule604 #-}
   rule604 = \ ((_set1IcollectedNames) :: Set Identifier) ((_set2IcollectedNames) :: Set Identifier) ->
     _set1IcollectedNames `Set.union` _set2IcollectedNames
   {-# INLINE rule605 #-}
   rule605 = \ ((_set1Ierrors) :: Seq Error) ((_set2Ierrors) :: Seq Error) ->
     _set1Ierrors Seq.>< _set2Ierrors
   {-# INLINE rule606 #-}
   rule606 = \ ((_lhsIallFields) :: DataTypes) ->
     _lhsIallFields
   {-# INLINE rule607 #-}
   rule607 = \ ((_lhsIallNonterminals) :: Set NontermIdent) ->
     _lhsIallNonterminals
   {-# INLINE rule608 #-}
   rule608 = \ ((_lhsIdefinedSets) :: DefinedSets) ->
     _lhsIdefinedSets
   {-# INLINE rule609 #-}
   rule609 = \ ((_lhsIallFields) :: DataTypes) ->
     _lhsIallFields
   {-# INLINE rule610 #-}
   rule610 = \ ((_lhsIallNonterminals) :: Set NontermIdent) ->
     _lhsIallNonterminals
   {-# INLINE rule611 #-}
   rule611 = \ ((_lhsIdefinedSets) :: DefinedSets) ->
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
         _lhsOnontSet = rule612 _set1InontSet _set2InontSet
         _lhsOcollectedNames :: Set Identifier
         _lhsOcollectedNames = rule613 _set1IcollectedNames _set2IcollectedNames
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule614 _set1Ierrors _set2Ierrors
         _set1OallFields = rule615 _lhsIallFields
         _set1OallNonterminals = rule616 _lhsIallNonterminals
         _set1OdefinedSets = rule617 _lhsIdefinedSets
         _set2OallFields = rule618 _lhsIallFields
         _set2OallNonterminals = rule619 _lhsIallNonterminals
         _set2OdefinedSets = rule620 _lhsIdefinedSets
         __result_ = T_NontSet_vOut28 _lhsOcollectedNames _lhsOerrors _lhsOnontSet
         in __result_ )
     in C_NontSet_s29 v28
   {-# INLINE rule612 #-}
   {-# LINE 738 "./src-ag/Transform.ag" #-}
   rule612 = \ ((_set1InontSet) :: Set NontermIdent) ((_set2InontSet) :: Set NontermIdent) ->
                               {-# LINE 738 "./src-ag/Transform.ag" #-}
                               Set.difference    _set1InontSet _set2InontSet
                               {-# LINE 4702 "dist/build/Transform.hs"#-}
   {-# INLINE rule613 #-}
   rule613 = \ ((_set1IcollectedNames) :: Set Identifier) ((_set2IcollectedNames) :: Set Identifier) ->
     _set1IcollectedNames `Set.union` _set2IcollectedNames
   {-# INLINE rule614 #-}
   rule614 = \ ((_set1Ierrors) :: Seq Error) ((_set2Ierrors) :: Seq Error) ->
     _set1Ierrors Seq.>< _set2Ierrors
   {-# INLINE rule615 #-}
   rule615 = \ ((_lhsIallFields) :: DataTypes) ->
     _lhsIallFields
   {-# INLINE rule616 #-}
   rule616 = \ ((_lhsIallNonterminals) :: Set NontermIdent) ->
     _lhsIallNonterminals
   {-# INLINE rule617 #-}
   rule617 = \ ((_lhsIdefinedSets) :: DefinedSets) ->
     _lhsIdefinedSets
   {-# INLINE rule618 #-}
   rule618 = \ ((_lhsIallFields) :: DataTypes) ->
     _lhsIallFields
   {-# INLINE rule619 #-}
   rule619 = \ ((_lhsIallNonterminals) :: Set NontermIdent) ->
     _lhsIallNonterminals
   {-# INLINE rule620 #-}
   rule620 = \ ((_lhsIdefinedSets) :: DefinedSets) ->
     _lhsIdefinedSets
{-# NOINLINE sem_NontSet_Path #-}
sem_NontSet_Path :: (NontermIdent) -> (NontermIdent) -> T_NontSet 
sem_NontSet_Path arg_from_ arg_to_ = T_NontSet (return st29) where
   {-# NOINLINE st29 #-}
   st29 = let
      v28 :: T_NontSet_v28 
      v28 = \ (T_NontSet_vIn28 _lhsIallFields _lhsIallNonterminals _lhsIdefinedSets) -> ( let
         _lhsOnontSet :: Set NontermIdent
         _lhsOnontSet = rule621 _lhsIallFields arg_from_ arg_to_
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule622 _lhsIallNonterminals arg_from_ arg_to_
         _lhsOcollectedNames :: Set Identifier
         _lhsOcollectedNames = rule623  ()
         __result_ = T_NontSet_vOut28 _lhsOcollectedNames _lhsOerrors _lhsOnontSet
         in __result_ )
     in C_NontSet_s29 v28
   {-# INLINE rule621 #-}
   {-# LINE 739 "./src-ag/Transform.ag" #-}
   rule621 = \ ((_lhsIallFields) :: DataTypes) from_ to_ ->
                               {-# LINE 739 "./src-ag/Transform.ag" #-}
                               let table = flattenDatas _lhsIallFields
                               in path table from_ to_
                               {-# LINE 4749 "dist/build/Transform.hs"#-}
   {-# INLINE rule622 #-}
   {-# LINE 741 "./src-ag/Transform.ag" #-}
   rule622 = \ ((_lhsIallNonterminals) :: Set NontermIdent) from_ to_ ->
                              {-# LINE 741 "./src-ag/Transform.ag" #-}
                              let check name | Set.member name _lhsIallNonterminals
                                                         = Seq.empty
                                             | otherwise = Seq.singleton (UndefNont name)
                              in check from_ >< check to_
                              {-# LINE 4758 "dist/build/Transform.hs"#-}
   {-# INLINE rule623 #-}
   rule623 = \  (_ :: ()) ->
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
         _lhsOpatunder = rule624 _patsIpatunder arg_name_
         _lhsOstpos :: Pos
         _lhsOstpos = rule625 arg_name_
         _lhsOdefinedAttrs :: [AttrName]
         _lhsOdefinedAttrs = rule626 _patsIdefinedAttrs
         _lhsOdefinedInsts :: [Identifier]
         _lhsOdefinedInsts = rule627 _patsIdefinedInsts
         _copy = rule628 _patsIcopy arg_name_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule629 _copy
         __result_ = T_Pattern_vOut31 _lhsOcopy _lhsOdefinedAttrs _lhsOdefinedInsts _lhsOpatunder _lhsOstpos
         in __result_ )
     in C_Pattern_s32 v31
   {-# INLINE rule624 #-}
   {-# LINE 1190 "./src-ag/Transform.ag" #-}
   rule624 = \ ((_patsIpatunder) :: [AttrName]->Patterns) name_ ->
                               {-# LINE 1190 "./src-ag/Transform.ag" #-}
                               \us -> Constr name_ (_patsIpatunder us)
                               {-# LINE 4825 "dist/build/Transform.hs"#-}
   {-# INLINE rule625 #-}
   {-# LINE 1201 "./src-ag/Transform.ag" #-}
   rule625 = \ name_ ->
                             {-# LINE 1201 "./src-ag/Transform.ag" #-}
                             getPos name_
                             {-# LINE 4831 "dist/build/Transform.hs"#-}
   {-# INLINE rule626 #-}
   rule626 = \ ((_patsIdefinedAttrs) :: [AttrName]) ->
     _patsIdefinedAttrs
   {-# INLINE rule627 #-}
   rule627 = \ ((_patsIdefinedInsts) :: [Identifier]) ->
     _patsIdefinedInsts
   {-# INLINE rule628 #-}
   rule628 = \ ((_patsIcopy) :: Patterns) name_ ->
     Constr name_ _patsIcopy
   {-# INLINE rule629 #-}
   rule629 = \ _copy ->
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
         _lhsOpatunder = rule630 _patsIpatunder arg_pos_
         _lhsOstpos :: Pos
         _lhsOstpos = rule631 arg_pos_
         _lhsOdefinedAttrs :: [AttrName]
         _lhsOdefinedAttrs = rule632 _patsIdefinedAttrs
         _lhsOdefinedInsts :: [Identifier]
         _lhsOdefinedInsts = rule633 _patsIdefinedInsts
         _copy = rule634 _patsIcopy arg_pos_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule635 _copy
         __result_ = T_Pattern_vOut31 _lhsOcopy _lhsOdefinedAttrs _lhsOdefinedInsts _lhsOpatunder _lhsOstpos
         in __result_ )
     in C_Pattern_s32 v31
   {-# INLINE rule630 #-}
   {-# LINE 1191 "./src-ag/Transform.ag" #-}
   rule630 = \ ((_patsIpatunder) :: [AttrName]->Patterns) pos_ ->
                                {-# LINE 1191 "./src-ag/Transform.ag" #-}
                                \us -> Product pos_ (_patsIpatunder us)
                                {-# LINE 4872 "dist/build/Transform.hs"#-}
   {-# INLINE rule631 #-}
   {-# LINE 1202 "./src-ag/Transform.ag" #-}
   rule631 = \ pos_ ->
                             {-# LINE 1202 "./src-ag/Transform.ag" #-}
                             pos_
                             {-# LINE 4878 "dist/build/Transform.hs"#-}
   {-# INLINE rule632 #-}
   rule632 = \ ((_patsIdefinedAttrs) :: [AttrName]) ->
     _patsIdefinedAttrs
   {-# INLINE rule633 #-}
   rule633 = \ ((_patsIdefinedInsts) :: [Identifier]) ->
     _patsIdefinedInsts
   {-# INLINE rule634 #-}
   rule634 = \ ((_patsIcopy) :: Patterns) pos_ ->
     Product pos_ _patsIcopy
   {-# INLINE rule635 #-}
   rule635 = \ _copy ->
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
         _lhsOdefinedAttrs = rule636 _patIdefinedAttrs arg_attr_ arg_field_
         _lhsOpatunder :: [AttrName]->Pattern
         _lhsOpatunder = rule637 _copy arg_attr_ arg_field_
         _lhsOdefinedInsts :: [Identifier]
         _lhsOdefinedInsts = rule638 _patIdefinedInsts arg_attr_ arg_field_
         _lhsOstpos :: Pos
         _lhsOstpos = rule639 arg_field_
         _copy = rule640 _patIcopy arg_attr_ arg_field_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule641 _copy
         __result_ = T_Pattern_vOut31 _lhsOcopy _lhsOdefinedAttrs _lhsOdefinedInsts _lhsOpatunder _lhsOstpos
         in __result_ )
     in C_Pattern_s32 v31
   {-# INLINE rule636 #-}
   {-# LINE 1186 "./src-ag/Transform.ag" #-}
   rule636 = \ ((_patIdefinedAttrs) :: [AttrName]) attr_ field_ ->
                               {-# LINE 1186 "./src-ag/Transform.ag" #-}
                               (field_, attr_) : _patIdefinedAttrs
                               {-# LINE 4919 "dist/build/Transform.hs"#-}
   {-# INLINE rule637 #-}
   {-# LINE 1187 "./src-ag/Transform.ag" #-}
   rule637 = \ _copy attr_ field_ ->
                               {-# LINE 1187 "./src-ag/Transform.ag" #-}
                               \us -> if ((field_,attr_) `elem` us) then Underscore noPos else _copy
                               {-# LINE 4925 "dist/build/Transform.hs"#-}
   {-# INLINE rule638 #-}
   {-# LINE 1188 "./src-ag/Transform.ag" #-}
   rule638 = \ ((_patIdefinedInsts) :: [Identifier]) attr_ field_ ->
                               {-# LINE 1188 "./src-ag/Transform.ag" #-}
                               (if field_ == _INST then [attr_] else []) ++ _patIdefinedInsts
                               {-# LINE 4931 "dist/build/Transform.hs"#-}
   {-# INLINE rule639 #-}
   {-# LINE 1203 "./src-ag/Transform.ag" #-}
   rule639 = \ field_ ->
                             {-# LINE 1203 "./src-ag/Transform.ag" #-}
                             getPos field_
                             {-# LINE 4937 "dist/build/Transform.hs"#-}
   {-# INLINE rule640 #-}
   rule640 = \ ((_patIcopy) :: Pattern) attr_ field_ ->
     Alias field_ attr_ _patIcopy
   {-# INLINE rule641 #-}
   rule641 = \ _copy ->
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
         _lhsOpatunder = rule642 _patIpatunder
         _lhsOdefinedAttrs :: [AttrName]
         _lhsOdefinedAttrs = rule643 _patIdefinedAttrs
         _lhsOdefinedInsts :: [Identifier]
         _lhsOdefinedInsts = rule644 _patIdefinedInsts
         _copy = rule645 _patIcopy
         _lhsOcopy :: Pattern
         _lhsOcopy = rule646 _copy
         _lhsOstpos :: Pos
         _lhsOstpos = rule647 _patIstpos
         __result_ = T_Pattern_vOut31 _lhsOcopy _lhsOdefinedAttrs _lhsOdefinedInsts _lhsOpatunder _lhsOstpos
         in __result_ )
     in C_Pattern_s32 v31
   {-# INLINE rule642 #-}
   {-# LINE 1192 "./src-ag/Transform.ag" #-}
   rule642 = \ ((_patIpatunder) :: [AttrName]->Pattern) ->
                                 {-# LINE 1192 "./src-ag/Transform.ag" #-}
                                 \us -> Irrefutable (_patIpatunder us)
                                 {-# LINE 4972 "dist/build/Transform.hs"#-}
   {-# INLINE rule643 #-}
   rule643 = \ ((_patIdefinedAttrs) :: [AttrName]) ->
     _patIdefinedAttrs
   {-# INLINE rule644 #-}
   rule644 = \ ((_patIdefinedInsts) :: [Identifier]) ->
     _patIdefinedInsts
   {-# INLINE rule645 #-}
   rule645 = \ ((_patIcopy) :: Pattern) ->
     Irrefutable _patIcopy
   {-# INLINE rule646 #-}
   rule646 = \ _copy ->
     _copy
   {-# INLINE rule647 #-}
   rule647 = \ ((_patIstpos) :: Pos) ->
     _patIstpos
{-# NOINLINE sem_Pattern_Underscore #-}
sem_Pattern_Underscore :: (Pos) -> T_Pattern 
sem_Pattern_Underscore arg_pos_ = T_Pattern (return st32) where
   {-# NOINLINE st32 #-}
   st32 = let
      v31 :: T_Pattern_v31 
      v31 = \ (T_Pattern_vIn31 ) -> ( let
         _lhsOpatunder :: [AttrName]->Pattern
         _lhsOpatunder = rule648 _copy
         _lhsOstpos :: Pos
         _lhsOstpos = rule649 arg_pos_
         _lhsOdefinedAttrs :: [AttrName]
         _lhsOdefinedAttrs = rule650  ()
         _lhsOdefinedInsts :: [Identifier]
         _lhsOdefinedInsts = rule651  ()
         _copy = rule652 arg_pos_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule653 _copy
         __result_ = T_Pattern_vOut31 _lhsOcopy _lhsOdefinedAttrs _lhsOdefinedInsts _lhsOpatunder _lhsOstpos
         in __result_ )
     in C_Pattern_s32 v31
   {-# INLINE rule648 #-}
   {-# LINE 1189 "./src-ag/Transform.ag" #-}
   rule648 = \ _copy ->
                                {-# LINE 1189 "./src-ag/Transform.ag" #-}
                                \_ -> _copy
                                {-# LINE 5014 "dist/build/Transform.hs"#-}
   {-# INLINE rule649 #-}
   {-# LINE 1204 "./src-ag/Transform.ag" #-}
   rule649 = \ pos_ ->
                             {-# LINE 1204 "./src-ag/Transform.ag" #-}
                             pos_
                             {-# LINE 5020 "dist/build/Transform.hs"#-}
   {-# INLINE rule650 #-}
   rule650 = \  (_ :: ()) ->
     []
   {-# INLINE rule651 #-}
   rule651 = \  (_ :: ()) ->
     []
   {-# INLINE rule652 #-}
   rule652 = \ pos_ ->
     Underscore pos_
   {-# INLINE rule653 #-}
   rule653 = \ _copy ->
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
         _lhsOpatunder = rule654 _hdIpatunder _tlIpatunder
         _lhsOdefinedAttrs :: [AttrName]
         _lhsOdefinedAttrs = rule655 _hdIdefinedAttrs _tlIdefinedAttrs
         _lhsOdefinedInsts :: [Identifier]
         _lhsOdefinedInsts = rule656 _hdIdefinedInsts _tlIdefinedInsts
         _copy = rule657 _hdIcopy _tlIcopy
         _lhsOcopy :: Patterns
         _lhsOcopy = rule658 _copy
         __result_ = T_Patterns_vOut34 _lhsOcopy _lhsOdefinedAttrs _lhsOdefinedInsts _lhsOpatunder
         in __result_ )
     in C_Patterns_s35 v34
   {-# INLINE rule654 #-}
   {-# LINE 1196 "./src-ag/Transform.ag" #-}
   rule654 = \ ((_hdIpatunder) :: [AttrName]->Pattern) ((_tlIpatunder) :: [AttrName]->Patterns) ->
                          {-# LINE 1196 "./src-ag/Transform.ag" #-}
                          \us -> (_hdIpatunder us) : (_tlIpatunder us)
                          {-# LINE 5092 "dist/build/Transform.hs"#-}
   {-# INLINE rule655 #-}
   rule655 = \ ((_hdIdefinedAttrs) :: [AttrName]) ((_tlIdefinedAttrs) :: [AttrName]) ->
     _hdIdefinedAttrs ++ _tlIdefinedAttrs
   {-# INLINE rule656 #-}
   rule656 = \ ((_hdIdefinedInsts) :: [Identifier]) ((_tlIdefinedInsts) :: [Identifier]) ->
     _hdIdefinedInsts ++ _tlIdefinedInsts
   {-# INLINE rule657 #-}
   rule657 = \ ((_hdIcopy) :: Pattern) ((_tlIcopy) :: Patterns) ->
     (:) _hdIcopy _tlIcopy
   {-# INLINE rule658 #-}
   rule658 = \ _copy ->
     _copy
{-# NOINLINE sem_Patterns_Nil #-}
sem_Patterns_Nil ::  T_Patterns 
sem_Patterns_Nil  = T_Patterns (return st35) where
   {-# NOINLINE st35 #-}
   st35 = let
      v34 :: T_Patterns_v34 
      v34 = \ (T_Patterns_vIn34 ) -> ( let
         _lhsOpatunder :: [AttrName]->Patterns
         _lhsOpatunder = rule659  ()
         _lhsOdefinedAttrs :: [AttrName]
         _lhsOdefinedAttrs = rule660  ()
         _lhsOdefinedInsts :: [Identifier]
         _lhsOdefinedInsts = rule661  ()
         _copy = rule662  ()
         _lhsOcopy :: Patterns
         _lhsOcopy = rule663 _copy
         __result_ = T_Patterns_vOut34 _lhsOcopy _lhsOdefinedAttrs _lhsOdefinedInsts _lhsOpatunder
         in __result_ )
     in C_Patterns_s35 v34
   {-# INLINE rule659 #-}
   {-# LINE 1195 "./src-ag/Transform.ag" #-}
   rule659 = \  (_ :: ()) ->
                         {-# LINE 1195 "./src-ag/Transform.ag" #-}
                         \_ ->  []
                         {-# LINE 5129 "dist/build/Transform.hs"#-}
   {-# INLINE rule660 #-}
   rule660 = \  (_ :: ()) ->
     []
   {-# INLINE rule661 #-}
   rule661 = \  (_ :: ()) ->
     []
   {-# INLINE rule662 #-}
   rule662 = \  (_ :: ()) ->
     []
   {-# INLINE rule663 #-}
   rule663 = \ _copy ->
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
         _pragmaNames = rule664 _rulesIpragmaNamesCollect
         _lhsOsemPragmasCollect :: PragmaMap
         _lhsOsemPragmasCollect = rule665 _coninfo _pragmaNames
         _attrOrders = rule666 _coninfo _rulesIorderDepsCollect
         _lhsOattrOrderCollect :: AttrOrderMap
         _lhsOattrOrderCollect = rule667 _attrOrders
         _coninfo = rule668 _constructorSetIconstructors _lhsIallFields _lhsInts
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule669 _coninfo _rulesIerrors
         _lhsOcollectedRules :: [ (NontermIdent, ConstructorIdent, RuleInfo)]
         _lhsOcollectedRules = rule670 _coninfo _rulesIruleInfos
         _lhsOcollectedSigs :: [ (NontermIdent, ConstructorIdent, SigInfo) ]
         _lhsOcollectedSigs = rule671 _coninfo _rulesIsigInfos
         _lhsOcollectedInsts :: [ (NontermIdent, ConstructorIdent, [Identifier]) ]
         _lhsOcollectedInsts = rule672 _coninfo _rulesIdefinedInsts
         _lhsOcollectedUniques :: [ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]
         _lhsOcollectedUniques = rule673 _coninfo _rulesIuniqueInfos
         _lhsOcollectedAugments :: [ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]
         _lhsOcollectedAugments = rule674 _coninfo _rulesIaugmentInfos
         _lhsOcollectedArounds :: [ (NontermIdent, ConstructorIdent, [AroundInfo])  ]
         _lhsOcollectedArounds = rule675 _coninfo _rulesIaroundInfos
         _lhsOcollectedMerges :: [ (NontermIdent, ConstructorIdent, [MergeInfo])   ]
         _lhsOcollectedMerges = rule676 _coninfo _rulesImergeInfos
         _rulesOoptions = rule677 _lhsIoptions
         __result_ = T_SemAlt_vOut37 _lhsOattrOrderCollect _lhsOcollectedArounds _lhsOcollectedAugments _lhsOcollectedInsts _lhsOcollectedMerges _lhsOcollectedRules _lhsOcollectedSigs _lhsOcollectedUniques _lhsOerrors _lhsOsemPragmasCollect
         in __result_ )
     in C_SemAlt_s38 v37
   {-# INLINE rule664 #-}
   {-# LINE 888 "./src-ag/Transform.ag" #-}
   rule664 = \ ((_rulesIpragmaNamesCollect) :: [Identifier]) ->
                                {-# LINE 888 "./src-ag/Transform.ag" #-}
                                Set.fromList _rulesIpragmaNamesCollect
                                {-# LINE 5216 "dist/build/Transform.hs"#-}
   {-# INLINE rule665 #-}
   {-# LINE 889 "./src-ag/Transform.ag" #-}
   rule665 = \ _coninfo _pragmaNames ->
                                {-# LINE 889 "./src-ag/Transform.ag" #-}
                                foldr pragmaMapUnion Map.empty [ pragmaMapSingle nt con _pragmaNames
                                                               | (nt, conset, _) <- _coninfo
                                                               , con <- Set.toList conset
                                                               ]
                                {-# LINE 5225 "dist/build/Transform.hs"#-}
   {-# INLINE rule666 #-}
   {-# LINE 918 "./src-ag/Transform.ag" #-}
   rule666 = \ _coninfo ((_rulesIorderDepsCollect) :: Set Dependency) ->
            {-# LINE 918 "./src-ag/Transform.ag" #-}
            [ orderMapSingle nt con _rulesIorderDepsCollect
            | (nt, conset, _) <- _coninfo
            , con <- Set.toList conset
            ]
            {-# LINE 5234 "dist/build/Transform.hs"#-}
   {-# INLINE rule667 #-}
   {-# LINE 923 "./src-ag/Transform.ag" #-}
   rule667 = \ _attrOrders ->
                               {-# LINE 923 "./src-ag/Transform.ag" #-}
                               foldr orderMapUnion Map.empty _attrOrders
                               {-# LINE 5240 "dist/build/Transform.hs"#-}
   {-# INLINE rule668 #-}
   {-# LINE 1105 "./src-ag/Transform.ag" #-}
   rule668 = \ ((_constructorSetIconstructors) :: (Set ConstructorIdent->Set ConstructorIdent)) ((_lhsIallFields) :: DataTypes) ((_lhsInts) :: Set NontermIdent) ->
                           {-# LINE 1105 "./src-ag/Transform.ag" #-}
                           [ (nt, conset, conkeys)
                           | nt  <- Set.toList _lhsInts
                           , let conmap = Map.findWithDefault Map.empty nt _lhsIallFields
                           , let conkeys = Set.fromList (Map.keys conmap)
                           , let conset  = _constructorSetIconstructors conkeys
                           ]
                           {-# LINE 5251 "dist/build/Transform.hs"#-}
   {-# INLINE rule669 #-}
   {-# LINE 1112 "./src-ag/Transform.ag" #-}
   rule669 = \ _coninfo ((_rulesIerrors) :: Seq Error) ->
                          {-# LINE 1112 "./src-ag/Transform.ag" #-}
                          Seq.fromList
                             [ UndefAlt nt con
                             | (nt, conset, conkeys) <- _coninfo
                             , con <- Set.toList (Set.difference conset conkeys)
                             ]
                          Seq.>< _rulesIerrors
                          {-# LINE 5262 "dist/build/Transform.hs"#-}
   {-# INLINE rule670 #-}
   {-# LINE 1119 "./src-ag/Transform.ag" #-}
   rule670 = \ _coninfo ((_rulesIruleInfos) :: [RuleInfo]) ->
                         {-# LINE 1119 "./src-ag/Transform.ag" #-}
                         [ (nt,con,r)
                         | (nt, conset, _) <- _coninfo
                         , con <- Set.toList conset
                         , r <- _rulesIruleInfos
                         ]
                         {-# LINE 5272 "dist/build/Transform.hs"#-}
   {-# INLINE rule671 #-}
   {-# LINE 1125 "./src-ag/Transform.ag" #-}
   rule671 = \ _coninfo ((_rulesIsigInfos) :: [SigInfo]) ->
                         {-# LINE 1125 "./src-ag/Transform.ag" #-}
                         [ (nt,con,ts)
                         | (nt, conset, _) <- _coninfo
                         , con <- Set.toList conset
                         , ts <- _rulesIsigInfos
                         ]
                         {-# LINE 5282 "dist/build/Transform.hs"#-}
   {-# INLINE rule672 #-}
   {-# LINE 1132 "./src-ag/Transform.ag" #-}
   rule672 = \ _coninfo ((_rulesIdefinedInsts) :: [Identifier]) ->
                         {-# LINE 1132 "./src-ag/Transform.ag" #-}
                         [ (nt,con,_rulesIdefinedInsts)
                         | (nt, conset, _) <- _coninfo
                         , con <- Set.toList conset
                         ]
                         {-# LINE 5291 "dist/build/Transform.hs"#-}
   {-# INLINE rule673 #-}
   {-# LINE 1138 "./src-ag/Transform.ag" #-}
   rule673 = \ _coninfo ((_rulesIuniqueInfos) :: [UniqueInfo]) ->
                         {-# LINE 1138 "./src-ag/Transform.ag" #-}
                         [ (nt,con,_rulesIuniqueInfos)
                         | (nt, conset, _) <- _coninfo
                         , con <- Set.toList conset
                         ]
                         {-# LINE 5300 "dist/build/Transform.hs"#-}
   {-# INLINE rule674 #-}
   {-# LINE 1144 "./src-ag/Transform.ag" #-}
   rule674 = \ _coninfo ((_rulesIaugmentInfos) :: [AugmentInfo]) ->
                         {-# LINE 1144 "./src-ag/Transform.ag" #-}
                         [ (nt, con, _rulesIaugmentInfos)
                         | (nt, conset, _) <- _coninfo
                         , con <- Set.toList conset
                         ]
                         {-# LINE 5309 "dist/build/Transform.hs"#-}
   {-# INLINE rule675 #-}
   {-# LINE 1150 "./src-ag/Transform.ag" #-}
   rule675 = \ _coninfo ((_rulesIaroundInfos) :: [AroundInfo]) ->
                         {-# LINE 1150 "./src-ag/Transform.ag" #-}
                         [ (nt, con, _rulesIaroundInfos)
                         | (nt, conset, _) <- _coninfo
                         , con <- Set.toList conset
                         ]
                         {-# LINE 5318 "dist/build/Transform.hs"#-}
   {-# INLINE rule676 #-}
   {-# LINE 1156 "./src-ag/Transform.ag" #-}
   rule676 = \ _coninfo ((_rulesImergeInfos) :: [MergeInfo]) ->
                         {-# LINE 1156 "./src-ag/Transform.ag" #-}
                         [ (nt, con, _rulesImergeInfos)
                         | (nt, conset, _) <- _coninfo
                         , con <- Set.toList conset
                         ]
                         {-# LINE 5327 "dist/build/Transform.hs"#-}
   {-# INLINE rule677 #-}
   rule677 = \ ((_lhsIoptions) :: Options) ->
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
         _lhsOattrOrderCollect = rule678 _hdIattrOrderCollect _tlIattrOrderCollect
         _lhsOcollectedArounds :: [ (NontermIdent, ConstructorIdent, [AroundInfo])  ]
         _lhsOcollectedArounds = rule679 _hdIcollectedArounds _tlIcollectedArounds
         _lhsOcollectedAugments :: [ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]
         _lhsOcollectedAugments = rule680 _hdIcollectedAugments _tlIcollectedAugments
         _lhsOcollectedInsts :: [ (NontermIdent, ConstructorIdent, [Identifier]) ]
         _lhsOcollectedInsts = rule681 _hdIcollectedInsts _tlIcollectedInsts
         _lhsOcollectedMerges :: [ (NontermIdent, ConstructorIdent, [MergeInfo])   ]
         _lhsOcollectedMerges = rule682 _hdIcollectedMerges _tlIcollectedMerges
         _lhsOcollectedRules :: [ (NontermIdent, ConstructorIdent, RuleInfo)]
         _lhsOcollectedRules = rule683 _hdIcollectedRules _tlIcollectedRules
         _lhsOcollectedSigs :: [ (NontermIdent, ConstructorIdent, SigInfo) ]
         _lhsOcollectedSigs = rule684 _hdIcollectedSigs _tlIcollectedSigs
         _lhsOcollectedUniques :: [ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]
         _lhsOcollectedUniques = rule685 _hdIcollectedUniques _tlIcollectedUniques
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule686 _hdIerrors _tlIerrors
         _lhsOsemPragmasCollect :: PragmaMap
         _lhsOsemPragmasCollect = rule687 _hdIsemPragmasCollect _tlIsemPragmasCollect
         _hdOallAttrDecls = rule688 _lhsIallAttrDecls
         _hdOallAttrs = rule689 _lhsIallAttrs
         _hdOallFields = rule690 _lhsIallFields
         _hdOnts = rule691 _lhsInts
         _hdOoptions = rule692 _lhsIoptions
         _tlOallAttrDecls = rule693 _lhsIallAttrDecls
         _tlOallAttrs = rule694 _lhsIallAttrs
         _tlOallFields = rule695 _lhsIallFields
         _tlOnts = rule696 _lhsInts
         _tlOoptions = rule697 _lhsIoptions
         __result_ = T_SemAlts_vOut40 _lhsOattrOrderCollect _lhsOcollectedArounds _lhsOcollectedAugments _lhsOcollectedInsts _lhsOcollectedMerges _lhsOcollectedRules _lhsOcollectedSigs _lhsOcollectedUniques _lhsOerrors _lhsOsemPragmasCollect
         in __result_ )
     in C_SemAlts_s41 v40
   {-# INLINE rule678 #-}
   rule678 = \ ((_hdIattrOrderCollect) :: AttrOrderMap) ((_tlIattrOrderCollect) :: AttrOrderMap) ->
     _hdIattrOrderCollect `orderMapUnion` _tlIattrOrderCollect
   {-# INLINE rule679 #-}
   rule679 = \ ((_hdIcollectedArounds) :: [ (NontermIdent, ConstructorIdent, [AroundInfo])  ]) ((_tlIcollectedArounds) :: [ (NontermIdent, ConstructorIdent, [AroundInfo])  ]) ->
     _hdIcollectedArounds ++ _tlIcollectedArounds
   {-# INLINE rule680 #-}
   rule680 = \ ((_hdIcollectedAugments) :: [ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]) ((_tlIcollectedAugments) :: [ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]) ->
     _hdIcollectedAugments ++ _tlIcollectedAugments
   {-# INLINE rule681 #-}
   rule681 = \ ((_hdIcollectedInsts) :: [ (NontermIdent, ConstructorIdent, [Identifier]) ]) ((_tlIcollectedInsts) :: [ (NontermIdent, ConstructorIdent, [Identifier]) ]) ->
     _hdIcollectedInsts ++ _tlIcollectedInsts
   {-# INLINE rule682 #-}
   rule682 = \ ((_hdIcollectedMerges) :: [ (NontermIdent, ConstructorIdent, [MergeInfo])   ]) ((_tlIcollectedMerges) :: [ (NontermIdent, ConstructorIdent, [MergeInfo])   ]) ->
     _hdIcollectedMerges ++ _tlIcollectedMerges
   {-# INLINE rule683 #-}
   rule683 = \ ((_hdIcollectedRules) :: [ (NontermIdent, ConstructorIdent, RuleInfo)]) ((_tlIcollectedRules) :: [ (NontermIdent, ConstructorIdent, RuleInfo)]) ->
     _hdIcollectedRules ++ _tlIcollectedRules
   {-# INLINE rule684 #-}
   rule684 = \ ((_hdIcollectedSigs) :: [ (NontermIdent, ConstructorIdent, SigInfo) ]) ((_tlIcollectedSigs) :: [ (NontermIdent, ConstructorIdent, SigInfo) ]) ->
     _hdIcollectedSigs ++ _tlIcollectedSigs
   {-# INLINE rule685 #-}
   rule685 = \ ((_hdIcollectedUniques) :: [ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]) ((_tlIcollectedUniques) :: [ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]) ->
     _hdIcollectedUniques ++ _tlIcollectedUniques
   {-# INLINE rule686 #-}
   rule686 = \ ((_hdIerrors) :: Seq Error) ((_tlIerrors) :: Seq Error) ->
     _hdIerrors Seq.>< _tlIerrors
   {-# INLINE rule687 #-}
   rule687 = \ ((_hdIsemPragmasCollect) :: PragmaMap) ((_tlIsemPragmasCollect) :: PragmaMap) ->
     _hdIsemPragmasCollect `pragmaMapUnion` _tlIsemPragmasCollect
   {-# INLINE rule688 #-}
   rule688 = \ ((_lhsIallAttrDecls) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIallAttrDecls
   {-# INLINE rule689 #-}
   rule689 = \ ((_lhsIallAttrs) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIallAttrs
   {-# INLINE rule690 #-}
   rule690 = \ ((_lhsIallFields) :: DataTypes) ->
     _lhsIallFields
   {-# INLINE rule691 #-}
   rule691 = \ ((_lhsInts) :: Set NontermIdent) ->
     _lhsInts
   {-# INLINE rule692 #-}
   rule692 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule693 #-}
   rule693 = \ ((_lhsIallAttrDecls) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIallAttrDecls
   {-# INLINE rule694 #-}
   rule694 = \ ((_lhsIallAttrs) :: Map NontermIdent (Attributes, Attributes)) ->
     _lhsIallAttrs
   {-# INLINE rule695 #-}
   rule695 = \ ((_lhsIallFields) :: DataTypes) ->
     _lhsIallFields
   {-# INLINE rule696 #-}
   rule696 = \ ((_lhsInts) :: Set NontermIdent) ->
     _lhsInts
   {-# INLINE rule697 #-}
   rule697 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_SemAlts_Nil #-}
sem_SemAlts_Nil ::  T_SemAlts 
sem_SemAlts_Nil  = T_SemAlts (return st41) where
   {-# NOINLINE st41 #-}
   st41 = let
      v40 :: T_SemAlts_v40 
      v40 = \ (T_SemAlts_vIn40 _lhsIallAttrDecls _lhsIallAttrs _lhsIallFields _lhsInts _lhsIoptions) -> ( let
         _lhsOattrOrderCollect :: AttrOrderMap
         _lhsOattrOrderCollect = rule698  ()
         _lhsOcollectedArounds :: [ (NontermIdent, ConstructorIdent, [AroundInfo])  ]
         _lhsOcollectedArounds = rule699  ()
         _lhsOcollectedAugments :: [ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]
         _lhsOcollectedAugments = rule700  ()
         _lhsOcollectedInsts :: [ (NontermIdent, ConstructorIdent, [Identifier]) ]
         _lhsOcollectedInsts = rule701  ()
         _lhsOcollectedMerges :: [ (NontermIdent, ConstructorIdent, [MergeInfo])   ]
         _lhsOcollectedMerges = rule702  ()
         _lhsOcollectedRules :: [ (NontermIdent, ConstructorIdent, RuleInfo)]
         _lhsOcollectedRules = rule703  ()
         _lhsOcollectedSigs :: [ (NontermIdent, ConstructorIdent, SigInfo) ]
         _lhsOcollectedSigs = rule704  ()
         _lhsOcollectedUniques :: [ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]
         _lhsOcollectedUniques = rule705  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule706  ()
         _lhsOsemPragmasCollect :: PragmaMap
         _lhsOsemPragmasCollect = rule707  ()
         __result_ = T_SemAlts_vOut40 _lhsOattrOrderCollect _lhsOcollectedArounds _lhsOcollectedAugments _lhsOcollectedInsts _lhsOcollectedMerges _lhsOcollectedRules _lhsOcollectedSigs _lhsOcollectedUniques _lhsOerrors _lhsOsemPragmasCollect
         in __result_ )
     in C_SemAlts_s41 v40
   {-# INLINE rule698 #-}
   rule698 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule699 #-}
   rule699 = \  (_ :: ()) ->
     []
   {-# INLINE rule700 #-}
   rule700 = \  (_ :: ()) ->
     []
   {-# INLINE rule701 #-}
   rule701 = \  (_ :: ()) ->
     []
   {-# INLINE rule702 #-}
   rule702 = \  (_ :: ()) ->
     []
   {-# INLINE rule703 #-}
   rule703 = \  (_ :: ()) ->
     []
   {-# INLINE rule704 #-}
   rule704 = \  (_ :: ()) ->
     []
   {-# INLINE rule705 #-}
   rule705 = \  (_ :: ()) ->
     []
   {-# INLINE rule706 #-}
   rule706 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule707 #-}
   rule707 = \  (_ :: ()) ->
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
         _lhsOerrors = rule708 _lhsIoptions arg_rhs_
         _lhsOruleInfos :: [RuleInfo]
         _lhsOruleInfos = rule709 _patternIdefinedAttrs _patternIpatunder _patternIstpos arg_eager_ arg_mbName_ arg_owrt_ arg_pure_ arg_rhs_
         _lhsOaroundInfos :: [AroundInfo]
         _lhsOaroundInfos = rule710  ()
         _lhsOaugmentInfos :: [AugmentInfo]
         _lhsOaugmentInfos = rule711  ()
         _lhsOdefinedInsts :: [Identifier]
         _lhsOdefinedInsts = rule712 _patternIdefinedInsts
         _lhsOmergeInfos :: [MergeInfo]
         _lhsOmergeInfos = rule713  ()
         _lhsOorderDepsCollect :: Set Dependency
         _lhsOorderDepsCollect = rule714  ()
         _lhsOpragmaNamesCollect :: [Identifier]
         _lhsOpragmaNamesCollect = rule715  ()
         _lhsOsigInfos :: [SigInfo]
         _lhsOsigInfos = rule716  ()
         _lhsOuniqueInfos :: [UniqueInfo]
         _lhsOuniqueInfos = rule717  ()
         __result_ = T_SemDef_vOut43 _lhsOaroundInfos _lhsOaugmentInfos _lhsOdefinedInsts _lhsOerrors _lhsOmergeInfos _lhsOorderDepsCollect _lhsOpragmaNamesCollect _lhsOruleInfos _lhsOsigInfos _lhsOuniqueInfos
         in __result_ )
     in C_SemDef_s44 v43
   {-# INLINE rule708 #-}
   {-# LINE 556 "./src-ag/Transform.ag" #-}
   rule708 = \ ((_lhsIoptions) :: Options) rhs_ ->
                 {-# LINE 556 "./src-ag/Transform.ag" #-}
                 if checkParseRhs _lhsIoptions
                 then Seq.fromList $ checkRhs rhs_
                 else Seq.empty
                 {-# LINE 5603 "dist/build/Transform.hs"#-}
   {-# INLINE rule709 #-}
   {-# LINE 1162 "./src-ag/Transform.ag" #-}
   rule709 = \ ((_patternIdefinedAttrs) :: [AttrName]) ((_patternIpatunder) :: [AttrName]->Pattern) ((_patternIstpos) :: Pos) eager_ mbName_ owrt_ pure_ rhs_ ->
                           {-# LINE 1162 "./src-ag/Transform.ag" #-}
                           [ (mbName_, _patternIpatunder, rhs_, _patternIdefinedAttrs, owrt_, show _patternIstpos, pure_, eager_) ]
                           {-# LINE 5609 "dist/build/Transform.hs"#-}
   {-# INLINE rule710 #-}
   rule710 = \  (_ :: ()) ->
     []
   {-# INLINE rule711 #-}
   rule711 = \  (_ :: ()) ->
     []
   {-# INLINE rule712 #-}
   rule712 = \ ((_patternIdefinedInsts) :: [Identifier]) ->
     _patternIdefinedInsts
   {-# INLINE rule713 #-}
   rule713 = \  (_ :: ()) ->
     []
   {-# INLINE rule714 #-}
   rule714 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule715 #-}
   rule715 = \  (_ :: ()) ->
     []
   {-# INLINE rule716 #-}
   rule716 = \  (_ :: ()) ->
     []
   {-# INLINE rule717 #-}
   rule717 = \  (_ :: ()) ->
     []
{-# NOINLINE sem_SemDef_TypeDef #-}
sem_SemDef_TypeDef :: (Pos) -> (Identifier) -> (Type) -> T_SemDef 
sem_SemDef_TypeDef arg_pos_ arg_ident_ arg_tp_ = T_SemDef (return st44) where
   {-# NOINLINE st44 #-}
   st44 = let
      v43 :: T_SemDef_v43 
      v43 = \ (T_SemDef_vIn43 _lhsIoptions) -> ( let
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule718 _lhsIoptions arg_pos_ arg_tp_
         _lhsOsigInfos :: [SigInfo]
         _lhsOsigInfos = rule719 arg_ident_ arg_tp_
         _lhsOaroundInfos :: [AroundInfo]
         _lhsOaroundInfos = rule720  ()
         _lhsOaugmentInfos :: [AugmentInfo]
         _lhsOaugmentInfos = rule721  ()
         _lhsOdefinedInsts :: [Identifier]
         _lhsOdefinedInsts = rule722  ()
         _lhsOmergeInfos :: [MergeInfo]
         _lhsOmergeInfos = rule723  ()
         _lhsOorderDepsCollect :: Set Dependency
         _lhsOorderDepsCollect = rule724  ()
         _lhsOpragmaNamesCollect :: [Identifier]
         _lhsOpragmaNamesCollect = rule725  ()
         _lhsOruleInfos :: [RuleInfo]
         _lhsOruleInfos = rule726  ()
         _lhsOuniqueInfos :: [UniqueInfo]
         _lhsOuniqueInfos = rule727  ()
         __result_ = T_SemDef_vOut43 _lhsOaroundInfos _lhsOaugmentInfos _lhsOdefinedInsts _lhsOerrors _lhsOmergeInfos _lhsOorderDepsCollect _lhsOpragmaNamesCollect _lhsOruleInfos _lhsOsigInfos _lhsOuniqueInfos
         in __result_ )
     in C_SemDef_s44 v43
   {-# INLINE rule718 #-}
   {-# LINE 563 "./src-ag/Transform.ag" #-}
   rule718 = \ ((_lhsIoptions) :: Options) pos_ tp_ ->
                 {-# LINE 563 "./src-ag/Transform.ag" #-}
                 if checkParseTy _lhsIoptions
                 then case tp_ of
                        Haskell s -> let ex  = Expression pos_ tks
                                         tks = [tk]
                                         tk  = HsToken s pos_
                                     in Seq.fromList $ checkTy ex
                        _ -> Seq.empty
                 else Seq.empty
                 {-# LINE 5676 "dist/build/Transform.hs"#-}
   {-# INLINE rule719 #-}
   {-# LINE 1165 "./src-ag/Transform.ag" #-}
   rule719 = \ ident_ tp_ ->
                              {-# LINE 1165 "./src-ag/Transform.ag" #-}
                              [ (ident_, tp_) ]
                              {-# LINE 5682 "dist/build/Transform.hs"#-}
   {-# INLINE rule720 #-}
   rule720 = \  (_ :: ()) ->
     []
   {-# INLINE rule721 #-}
   rule721 = \  (_ :: ()) ->
     []
   {-# INLINE rule722 #-}
   rule722 = \  (_ :: ()) ->
     []
   {-# INLINE rule723 #-}
   rule723 = \  (_ :: ()) ->
     []
   {-# INLINE rule724 #-}
   rule724 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule725 #-}
   rule725 = \  (_ :: ()) ->
     []
   {-# INLINE rule726 #-}
   rule726 = \  (_ :: ()) ->
     []
   {-# INLINE rule727 #-}
   rule727 = \  (_ :: ()) ->
     []
{-# NOINLINE sem_SemDef_UniqueDef #-}
sem_SemDef_UniqueDef :: (Identifier) -> (Identifier) -> T_SemDef 
sem_SemDef_UniqueDef arg_ident_ arg_ref_ = T_SemDef (return st44) where
   {-# NOINLINE st44 #-}
   st44 = let
      v43 :: T_SemDef_v43 
      v43 = \ (T_SemDef_vIn43 _lhsIoptions) -> ( let
         _lhsOuniqueInfos :: [UniqueInfo]
         _lhsOuniqueInfos = rule728 arg_ident_ arg_ref_
         _lhsOaroundInfos :: [AroundInfo]
         _lhsOaroundInfos = rule729  ()
         _lhsOaugmentInfos :: [AugmentInfo]
         _lhsOaugmentInfos = rule730  ()
         _lhsOdefinedInsts :: [Identifier]
         _lhsOdefinedInsts = rule731  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule732  ()
         _lhsOmergeInfos :: [MergeInfo]
         _lhsOmergeInfos = rule733  ()
         _lhsOorderDepsCollect :: Set Dependency
         _lhsOorderDepsCollect = rule734  ()
         _lhsOpragmaNamesCollect :: [Identifier]
         _lhsOpragmaNamesCollect = rule735  ()
         _lhsOruleInfos :: [RuleInfo]
         _lhsOruleInfos = rule736  ()
         _lhsOsigInfos :: [SigInfo]
         _lhsOsigInfos = rule737  ()
         __result_ = T_SemDef_vOut43 _lhsOaroundInfos _lhsOaugmentInfos _lhsOdefinedInsts _lhsOerrors _lhsOmergeInfos _lhsOorderDepsCollect _lhsOpragmaNamesCollect _lhsOruleInfos _lhsOsigInfos _lhsOuniqueInfos
         in __result_ )
     in C_SemDef_s44 v43
   {-# INLINE rule728 #-}
   {-# LINE 1168 "./src-ag/Transform.ag" #-}
   rule728 = \ ident_ ref_ ->
                                   {-# LINE 1168 "./src-ag/Transform.ag" #-}
                                   [ (ident_, ref_) ]
                                   {-# LINE 5742 "dist/build/Transform.hs"#-}
   {-# INLINE rule729 #-}
   rule729 = \  (_ :: ()) ->
     []
   {-# INLINE rule730 #-}
   rule730 = \  (_ :: ()) ->
     []
   {-# INLINE rule731 #-}
   rule731 = \  (_ :: ()) ->
     []
   {-# INLINE rule732 #-}
   rule732 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule733 #-}
   rule733 = \  (_ :: ()) ->
     []
   {-# INLINE rule734 #-}
   rule734 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule735 #-}
   rule735 = \  (_ :: ()) ->
     []
   {-# INLINE rule736 #-}
   rule736 = \  (_ :: ()) ->
     []
   {-# INLINE rule737 #-}
   rule737 = \  (_ :: ()) ->
     []
{-# NOINLINE sem_SemDef_AugmentDef #-}
sem_SemDef_AugmentDef :: (Identifier) -> (Expression) -> T_SemDef 
sem_SemDef_AugmentDef arg_ident_ arg_rhs_ = T_SemDef (return st44) where
   {-# NOINLINE st44 #-}
   st44 = let
      v43 :: T_SemDef_v43 
      v43 = \ (T_SemDef_vIn43 _lhsIoptions) -> ( let
         _lhsOaugmentInfos :: [AugmentInfo]
         _lhsOaugmentInfos = rule738 arg_ident_ arg_rhs_
         _lhsOaroundInfos :: [AroundInfo]
         _lhsOaroundInfos = rule739  ()
         _lhsOdefinedInsts :: [Identifier]
         _lhsOdefinedInsts = rule740  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule741  ()
         _lhsOmergeInfos :: [MergeInfo]
         _lhsOmergeInfos = rule742  ()
         _lhsOorderDepsCollect :: Set Dependency
         _lhsOorderDepsCollect = rule743  ()
         _lhsOpragmaNamesCollect :: [Identifier]
         _lhsOpragmaNamesCollect = rule744  ()
         _lhsOruleInfos :: [RuleInfo]
         _lhsOruleInfos = rule745  ()
         _lhsOsigInfos :: [SigInfo]
         _lhsOsigInfos = rule746  ()
         _lhsOuniqueInfos :: [UniqueInfo]
         _lhsOuniqueInfos = rule747  ()
         __result_ = T_SemDef_vOut43 _lhsOaroundInfos _lhsOaugmentInfos _lhsOdefinedInsts _lhsOerrors _lhsOmergeInfos _lhsOorderDepsCollect _lhsOpragmaNamesCollect _lhsOruleInfos _lhsOsigInfos _lhsOuniqueInfos
         in __result_ )
     in C_SemDef_s44 v43
   {-# INLINE rule738 #-}
   {-# LINE 1171 "./src-ag/Transform.ag" #-}
   rule738 = \ ident_ rhs_ ->
                                     {-# LINE 1171 "./src-ag/Transform.ag" #-}
                                     [ (ident_, rhs_) ]
                                     {-# LINE 5805 "dist/build/Transform.hs"#-}
   {-# INLINE rule739 #-}
   rule739 = \  (_ :: ()) ->
     []
   {-# INLINE rule740 #-}
   rule740 = \  (_ :: ()) ->
     []
   {-# INLINE rule741 #-}
   rule741 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule742 #-}
   rule742 = \  (_ :: ()) ->
     []
   {-# INLINE rule743 #-}
   rule743 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule744 #-}
   rule744 = \  (_ :: ()) ->
     []
   {-# INLINE rule745 #-}
   rule745 = \  (_ :: ()) ->
     []
   {-# INLINE rule746 #-}
   rule746 = \  (_ :: ()) ->
     []
   {-# INLINE rule747 #-}
   rule747 = \  (_ :: ()) ->
     []
{-# NOINLINE sem_SemDef_AroundDef #-}
sem_SemDef_AroundDef :: (Identifier) -> (Expression) -> T_SemDef 
sem_SemDef_AroundDef arg_ident_ arg_rhs_ = T_SemDef (return st44) where
   {-# NOINLINE st44 #-}
   st44 = let
      v43 :: T_SemDef_v43 
      v43 = \ (T_SemDef_vIn43 _lhsIoptions) -> ( let
         _lhsOaroundInfos :: [AroundInfo]
         _lhsOaroundInfos = rule748 arg_ident_ arg_rhs_
         _lhsOaugmentInfos :: [AugmentInfo]
         _lhsOaugmentInfos = rule749  ()
         _lhsOdefinedInsts :: [Identifier]
         _lhsOdefinedInsts = rule750  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule751  ()
         _lhsOmergeInfos :: [MergeInfo]
         _lhsOmergeInfos = rule752  ()
         _lhsOorderDepsCollect :: Set Dependency
         _lhsOorderDepsCollect = rule753  ()
         _lhsOpragmaNamesCollect :: [Identifier]
         _lhsOpragmaNamesCollect = rule754  ()
         _lhsOruleInfos :: [RuleInfo]
         _lhsOruleInfos = rule755  ()
         _lhsOsigInfos :: [SigInfo]
         _lhsOsigInfos = rule756  ()
         _lhsOuniqueInfos :: [UniqueInfo]
         _lhsOuniqueInfos = rule757  ()
         __result_ = T_SemDef_vOut43 _lhsOaroundInfos _lhsOaugmentInfos _lhsOdefinedInsts _lhsOerrors _lhsOmergeInfos _lhsOorderDepsCollect _lhsOpragmaNamesCollect _lhsOruleInfos _lhsOsigInfos _lhsOuniqueInfos
         in __result_ )
     in C_SemDef_s44 v43
   {-# INLINE rule748 #-}
   {-# LINE 1174 "./src-ag/Transform.ag" #-}
   rule748 = \ ident_ rhs_ ->
                                    {-# LINE 1174 "./src-ag/Transform.ag" #-}
                                    [ (ident_, rhs_) ]
                                    {-# LINE 5868 "dist/build/Transform.hs"#-}
   {-# INLINE rule749 #-}
   rule749 = \  (_ :: ()) ->
     []
   {-# INLINE rule750 #-}
   rule750 = \  (_ :: ()) ->
     []
   {-# INLINE rule751 #-}
   rule751 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule752 #-}
   rule752 = \  (_ :: ()) ->
     []
   {-# INLINE rule753 #-}
   rule753 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule754 #-}
   rule754 = \  (_ :: ()) ->
     []
   {-# INLINE rule755 #-}
   rule755 = \  (_ :: ()) ->
     []
   {-# INLINE rule756 #-}
   rule756 = \  (_ :: ()) ->
     []
   {-# INLINE rule757 #-}
   rule757 = \  (_ :: ()) ->
     []
{-# NOINLINE sem_SemDef_MergeDef #-}
sem_SemDef_MergeDef :: (Identifier) -> (Identifier) -> ([Identifier]) -> (Expression) -> T_SemDef 
sem_SemDef_MergeDef arg_target_ arg_nt_ arg_sources_ arg_rhs_ = T_SemDef (return st44) where
   {-# NOINLINE st44 #-}
   st44 = let
      v43 :: T_SemDef_v43 
      v43 = \ (T_SemDef_vIn43 _lhsIoptions) -> ( let
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule758 _lhsIoptions arg_rhs_
         _lhsOmergeInfos :: [MergeInfo]
         _lhsOmergeInfos = rule759 arg_nt_ arg_rhs_ arg_sources_ arg_target_
         _lhsOaroundInfos :: [AroundInfo]
         _lhsOaroundInfos = rule760  ()
         _lhsOaugmentInfos :: [AugmentInfo]
         _lhsOaugmentInfos = rule761  ()
         _lhsOdefinedInsts :: [Identifier]
         _lhsOdefinedInsts = rule762  ()
         _lhsOorderDepsCollect :: Set Dependency
         _lhsOorderDepsCollect = rule763  ()
         _lhsOpragmaNamesCollect :: [Identifier]
         _lhsOpragmaNamesCollect = rule764  ()
         _lhsOruleInfos :: [RuleInfo]
         _lhsOruleInfos = rule765  ()
         _lhsOsigInfos :: [SigInfo]
         _lhsOsigInfos = rule766  ()
         _lhsOuniqueInfos :: [UniqueInfo]
         _lhsOuniqueInfos = rule767  ()
         __result_ = T_SemDef_vOut43 _lhsOaroundInfos _lhsOaugmentInfos _lhsOdefinedInsts _lhsOerrors _lhsOmergeInfos _lhsOorderDepsCollect _lhsOpragmaNamesCollect _lhsOruleInfos _lhsOsigInfos _lhsOuniqueInfos
         in __result_ )
     in C_SemDef_s44 v43
   {-# INLINE rule758 #-}
   {-# LINE 556 "./src-ag/Transform.ag" #-}
   rule758 = \ ((_lhsIoptions) :: Options) rhs_ ->
                 {-# LINE 556 "./src-ag/Transform.ag" #-}
                 if checkParseRhs _lhsIoptions
                 then Seq.fromList $ checkRhs rhs_
                 else Seq.empty
                 {-# LINE 5933 "dist/build/Transform.hs"#-}
   {-# INLINE rule759 #-}
   {-# LINE 1177 "./src-ag/Transform.ag" #-}
   rule759 = \ nt_ rhs_ sources_ target_ ->
                                   {-# LINE 1177 "./src-ag/Transform.ag" #-}
                                   [ (target_, nt_, sources_, rhs_) ]
                                   {-# LINE 5939 "dist/build/Transform.hs"#-}
   {-# INLINE rule760 #-}
   rule760 = \  (_ :: ()) ->
     []
   {-# INLINE rule761 #-}
   rule761 = \  (_ :: ()) ->
     []
   {-# INLINE rule762 #-}
   rule762 = \  (_ :: ()) ->
     []
   {-# INLINE rule763 #-}
   rule763 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule764 #-}
   rule764 = \  (_ :: ()) ->
     []
   {-# INLINE rule765 #-}
   rule765 = \  (_ :: ()) ->
     []
   {-# INLINE rule766 #-}
   rule766 = \  (_ :: ()) ->
     []
   {-# INLINE rule767 #-}
   rule767 = \  (_ :: ()) ->
     []
{-# NOINLINE sem_SemDef_SemPragma #-}
sem_SemDef_SemPragma :: ([NontermIdent]) -> T_SemDef 
sem_SemDef_SemPragma arg_names_ = T_SemDef (return st44) where
   {-# NOINLINE st44 #-}
   st44 = let
      v43 :: T_SemDef_v43 
      v43 = \ (T_SemDef_vIn43 _lhsIoptions) -> ( let
         _lhsOpragmaNamesCollect :: [Identifier]
         _lhsOpragmaNamesCollect = rule768 arg_names_
         _lhsOaroundInfos :: [AroundInfo]
         _lhsOaroundInfos = rule769  ()
         _lhsOaugmentInfos :: [AugmentInfo]
         _lhsOaugmentInfos = rule770  ()
         _lhsOdefinedInsts :: [Identifier]
         _lhsOdefinedInsts = rule771  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule772  ()
         _lhsOmergeInfos :: [MergeInfo]
         _lhsOmergeInfos = rule773  ()
         _lhsOorderDepsCollect :: Set Dependency
         _lhsOorderDepsCollect = rule774  ()
         _lhsOruleInfos :: [RuleInfo]
         _lhsOruleInfos = rule775  ()
         _lhsOsigInfos :: [SigInfo]
         _lhsOsigInfos = rule776  ()
         _lhsOuniqueInfos :: [UniqueInfo]
         _lhsOuniqueInfos = rule777  ()
         __result_ = T_SemDef_vOut43 _lhsOaroundInfos _lhsOaugmentInfos _lhsOdefinedInsts _lhsOerrors _lhsOmergeInfos _lhsOorderDepsCollect _lhsOpragmaNamesCollect _lhsOruleInfos _lhsOsigInfos _lhsOuniqueInfos
         in __result_ )
     in C_SemDef_s44 v43
   {-# INLINE rule768 #-}
   {-# LINE 898 "./src-ag/Transform.ag" #-}
   rule768 = \ names_ ->
                                 {-# LINE 898 "./src-ag/Transform.ag" #-}
                                 names_
                                 {-# LINE 5999 "dist/build/Transform.hs"#-}
   {-# INLINE rule769 #-}
   rule769 = \  (_ :: ()) ->
     []
   {-# INLINE rule770 #-}
   rule770 = \  (_ :: ()) ->
     []
   {-# INLINE rule771 #-}
   rule771 = \  (_ :: ()) ->
     []
   {-# INLINE rule772 #-}
   rule772 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule773 #-}
   rule773 = \  (_ :: ()) ->
     []
   {-# INLINE rule774 #-}
   rule774 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule775 #-}
   rule775 = \  (_ :: ()) ->
     []
   {-# INLINE rule776 #-}
   rule776 = \  (_ :: ()) ->
     []
   {-# INLINE rule777 #-}
   rule777 = \  (_ :: ()) ->
     []
{-# NOINLINE sem_SemDef_AttrOrderBefore #-}
sem_SemDef_AttrOrderBefore :: ([Occurrence]) -> ([Occurrence]) -> T_SemDef 
sem_SemDef_AttrOrderBefore arg_before_ arg_after_ = T_SemDef (return st44) where
   {-# NOINLINE st44 #-}
   st44 = let
      v43 :: T_SemDef_v43 
      v43 = \ (T_SemDef_vIn43 _lhsIoptions) -> ( let
         _dependency = rule778 arg_after_ arg_before_
         _lhsOorderDepsCollect :: Set Dependency
         _lhsOorderDepsCollect = rule779 _dependency
         _lhsOaroundInfos :: [AroundInfo]
         _lhsOaroundInfos = rule780  ()
         _lhsOaugmentInfos :: [AugmentInfo]
         _lhsOaugmentInfos = rule781  ()
         _lhsOdefinedInsts :: [Identifier]
         _lhsOdefinedInsts = rule782  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule783  ()
         _lhsOmergeInfos :: [MergeInfo]
         _lhsOmergeInfos = rule784  ()
         _lhsOpragmaNamesCollect :: [Identifier]
         _lhsOpragmaNamesCollect = rule785  ()
         _lhsOruleInfos :: [RuleInfo]
         _lhsOruleInfos = rule786  ()
         _lhsOsigInfos :: [SigInfo]
         _lhsOsigInfos = rule787  ()
         _lhsOuniqueInfos :: [UniqueInfo]
         _lhsOuniqueInfos = rule788  ()
         __result_ = T_SemDef_vOut43 _lhsOaroundInfos _lhsOaugmentInfos _lhsOdefinedInsts _lhsOerrors _lhsOmergeInfos _lhsOorderDepsCollect _lhsOpragmaNamesCollect _lhsOruleInfos _lhsOsigInfos _lhsOuniqueInfos
         in __result_ )
     in C_SemDef_s44 v43
   {-# INLINE rule778 #-}
   {-# LINE 929 "./src-ag/Transform.ag" #-}
   rule778 = \ after_ before_ ->
                               {-# LINE 929 "./src-ag/Transform.ag" #-}
                               [ Dependency b a | b <- before_, a <- after_ ]
                               {-# LINE 6063 "dist/build/Transform.hs"#-}
   {-# INLINE rule779 #-}
   {-# LINE 930 "./src-ag/Transform.ag" #-}
   rule779 = \ _dependency ->
                               {-# LINE 930 "./src-ag/Transform.ag" #-}
                               Set.fromList _dependency
                               {-# LINE 6069 "dist/build/Transform.hs"#-}
   {-# INLINE rule780 #-}
   rule780 = \  (_ :: ()) ->
     []
   {-# INLINE rule781 #-}
   rule781 = \  (_ :: ()) ->
     []
   {-# INLINE rule782 #-}
   rule782 = \  (_ :: ()) ->
     []
   {-# INLINE rule783 #-}
   rule783 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule784 #-}
   rule784 = \  (_ :: ()) ->
     []
   {-# INLINE rule785 #-}
   rule785 = \  (_ :: ()) ->
     []
   {-# INLINE rule786 #-}
   rule786 = \  (_ :: ()) ->
     []
   {-# INLINE rule787 #-}
   rule787 = \  (_ :: ()) ->
     []
   {-# INLINE rule788 #-}
   rule788 = \  (_ :: ()) ->
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
         _lhsOaroundInfos = rule789 _hdIaroundInfos _tlIaroundInfos
         _lhsOaugmentInfos :: [AugmentInfo]
         _lhsOaugmentInfos = rule790 _hdIaugmentInfos _tlIaugmentInfos
         _lhsOdefinedInsts :: [Identifier]
         _lhsOdefinedInsts = rule791 _hdIdefinedInsts _tlIdefinedInsts
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule792 _hdIerrors _tlIerrors
         _lhsOmergeInfos :: [MergeInfo]
         _lhsOmergeInfos = rule793 _hdImergeInfos _tlImergeInfos
         _lhsOorderDepsCollect :: Set Dependency
         _lhsOorderDepsCollect = rule794 _hdIorderDepsCollect _tlIorderDepsCollect
         _lhsOpragmaNamesCollect :: [Identifier]
         _lhsOpragmaNamesCollect = rule795 _hdIpragmaNamesCollect _tlIpragmaNamesCollect
         _lhsOruleInfos :: [RuleInfo]
         _lhsOruleInfos = rule796 _hdIruleInfos _tlIruleInfos
         _lhsOsigInfos :: [SigInfo]
         _lhsOsigInfos = rule797 _hdIsigInfos _tlIsigInfos
         _lhsOuniqueInfos :: [UniqueInfo]
         _lhsOuniqueInfos = rule798 _hdIuniqueInfos _tlIuniqueInfos
         _hdOoptions = rule799 _lhsIoptions
         _tlOoptions = rule800 _lhsIoptions
         __result_ = T_SemDefs_vOut46 _lhsOaroundInfos _lhsOaugmentInfos _lhsOdefinedInsts _lhsOerrors _lhsOmergeInfos _lhsOorderDepsCollect _lhsOpragmaNamesCollect _lhsOruleInfos _lhsOsigInfos _lhsOuniqueInfos
         in __result_ )
     in C_SemDefs_s47 v46
   {-# INLINE rule789 #-}
   rule789 = \ ((_hdIaroundInfos) :: [AroundInfo]) ((_tlIaroundInfos) :: [AroundInfo]) ->
     _hdIaroundInfos ++ _tlIaroundInfos
   {-# INLINE rule790 #-}
   rule790 = \ ((_hdIaugmentInfos) :: [AugmentInfo]) ((_tlIaugmentInfos) :: [AugmentInfo]) ->
     _hdIaugmentInfos ++ _tlIaugmentInfos
   {-# INLINE rule791 #-}
   rule791 = \ ((_hdIdefinedInsts) :: [Identifier]) ((_tlIdefinedInsts) :: [Identifier]) ->
     _hdIdefinedInsts ++ _tlIdefinedInsts
   {-# INLINE rule792 #-}
   rule792 = \ ((_hdIerrors) :: Seq Error) ((_tlIerrors) :: Seq Error) ->
     _hdIerrors Seq.>< _tlIerrors
   {-# INLINE rule793 #-}
   rule793 = \ ((_hdImergeInfos) :: [MergeInfo]) ((_tlImergeInfos) :: [MergeInfo]) ->
     _hdImergeInfos ++ _tlImergeInfos
   {-# INLINE rule794 #-}
   rule794 = \ ((_hdIorderDepsCollect) :: Set Dependency) ((_tlIorderDepsCollect) :: Set Dependency) ->
     _hdIorderDepsCollect `Set.union` _tlIorderDepsCollect
   {-# INLINE rule795 #-}
   rule795 = \ ((_hdIpragmaNamesCollect) :: [Identifier]) ((_tlIpragmaNamesCollect) :: [Identifier]) ->
     _hdIpragmaNamesCollect ++ _tlIpragmaNamesCollect
   {-# INLINE rule796 #-}
   rule796 = \ ((_hdIruleInfos) :: [RuleInfo]) ((_tlIruleInfos) :: [RuleInfo]) ->
     _hdIruleInfos ++ _tlIruleInfos
   {-# INLINE rule797 #-}
   rule797 = \ ((_hdIsigInfos) :: [SigInfo]) ((_tlIsigInfos) :: [SigInfo]) ->
     _hdIsigInfos ++ _tlIsigInfos
   {-# INLINE rule798 #-}
   rule798 = \ ((_hdIuniqueInfos) :: [UniqueInfo]) ((_tlIuniqueInfos) :: [UniqueInfo]) ->
     _hdIuniqueInfos ++ _tlIuniqueInfos
   {-# INLINE rule799 #-}
   rule799 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule800 #-}
   rule800 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_SemDefs_Nil #-}
sem_SemDefs_Nil ::  T_SemDefs 
sem_SemDefs_Nil  = T_SemDefs (return st47) where
   {-# NOINLINE st47 #-}
   st47 = let
      v46 :: T_SemDefs_v46 
      v46 = \ (T_SemDefs_vIn46 _lhsIoptions) -> ( let
         _lhsOaroundInfos :: [AroundInfo]
         _lhsOaroundInfos = rule801  ()
         _lhsOaugmentInfos :: [AugmentInfo]
         _lhsOaugmentInfos = rule802  ()
         _lhsOdefinedInsts :: [Identifier]
         _lhsOdefinedInsts = rule803  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule804  ()
         _lhsOmergeInfos :: [MergeInfo]
         _lhsOmergeInfos = rule805  ()
         _lhsOorderDepsCollect :: Set Dependency
         _lhsOorderDepsCollect = rule806  ()
         _lhsOpragmaNamesCollect :: [Identifier]
         _lhsOpragmaNamesCollect = rule807  ()
         _lhsOruleInfos :: [RuleInfo]
         _lhsOruleInfos = rule808  ()
         _lhsOsigInfos :: [SigInfo]
         _lhsOsigInfos = rule809  ()
         _lhsOuniqueInfos :: [UniqueInfo]
         _lhsOuniqueInfos = rule810  ()
         __result_ = T_SemDefs_vOut46 _lhsOaroundInfos _lhsOaugmentInfos _lhsOdefinedInsts _lhsOerrors _lhsOmergeInfos _lhsOorderDepsCollect _lhsOpragmaNamesCollect _lhsOruleInfos _lhsOsigInfos _lhsOuniqueInfos
         in __result_ )
     in C_SemDefs_s47 v46
   {-# INLINE rule801 #-}
   rule801 = \  (_ :: ()) ->
     []
   {-# INLINE rule802 #-}
   rule802 = \  (_ :: ()) ->
     []
   {-# INLINE rule803 #-}
   rule803 = \  (_ :: ()) ->
     []
   {-# INLINE rule804 #-}
   rule804 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule805 #-}
   rule805 = \  (_ :: ()) ->
     []
   {-# INLINE rule806 #-}
   rule806 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule807 #-}
   rule807 = \  (_ :: ()) ->
     []
   {-# INLINE rule808 #-}
   rule808 = \  (_ :: ()) ->
     []
   {-# INLINE rule809 #-}
   rule809 = \  (_ :: ()) ->
     []
   {-# INLINE rule810 #-}
   rule810 = \  (_ :: ()) ->
     []
